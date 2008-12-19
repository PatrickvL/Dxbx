(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2007 Shadow_tj and other members of the development team.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
unit uEmuFS;

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  Windows, // SwitchToThread
  SysUtils, // IntToHex
  // JEDI
  JwaWinType,
  // OpenXDK
  XboxKrnl, // NT_TIB
  // Dxbx
  uTypes,
  uLog,
  uDxbxUtils,
  uXbe,
  uEmuAlloc,
  uEmuLDT;

type
  TSwapFS = (fsSwap, fsWindows, fsXbox);

procedure EmuGenerateFS(pTLS: PXBE_TLS; pTLSData: PVOID);
procedure EmuInitFS;
function EmuIsXboxFS: Boolean; {$IFDEF SUPPORTS_INLINE_ASM}inline; {$ENDIF}
procedure EmuSwapFS(); {$IFDEF SUPPORTS_INLINE_ASM}inline; {$ENDIF} overload;
procedure EmuSwapFS(const aSwapTo: TSwapFS); {$IFDEF SUPPORTS_INLINE_ASM}inline; {$ENDIF} overload;
procedure EmuCleanupFS;

const
  // TIB layout, reachable via FS :
  FS_StructuredExceptionHandlerPointer = $00;
  FS_StackBasePointer = $04;
  FS_StackLimit = $08; // Int32
  FS_SubSystemTib = $0C; // Pointer
  FS_FiberData = $10; // for TIB : Pointer
  FS_Version = $10; // for TEB (?) : Pointer
  FS_ArbitraryUserPointer = $14;
  FS_ThreadInformationBlockPointer = $18; // IntPtr
  FS_ProcessEnvironmentBlock = $30; // FS points to TEB/TIB which has a pointer to the PEB

  // Aliases :
  FS_ExceptionList = FS_StructuredExceptionHandlerPointer; // $00
  FS_Self = FS_ThreadInformationBlockPointer; // $18

  // Dxbx values inside FS_ArbitraryUserPointer :
  DxbxFS_SwapFS = FS_ArbitraryUserPointer; // = $14 : UInt16
  DxbxFS_IsXboxFS = FS_ArbitraryUserPointer + SizeOf(UInt16); // = $16 : ByteBool

var
  // Xbox is a single process system, and because of this fact, demos
  // and games are likely to suffer from Xbox-Never-Sleeps syndrome.
  //
  // Basically, there are situations where the Xbe will have no
  // reason to bother yielding to other threads. One solution to this
  // problem is to keep track of the number of function intercepts,
  // and every so often, force a sleep. This is the rate at which
  // those forced sleeps occur.
  // Automatically insert after this many EmuFS() swaps :
  EmuAutoSleepRate: Integer = -1;

implementation

// is the current fs register the xbox emulation variety?
function EmuIsXboxFS: Boolean;//LongBool;
(*asm
  xor eax, eax
  mov ah, fs:[DxbxFS_IsXboxFS]
end;
(**)
var
  chk: Byte;
begin
  asm
    mov ah, fs:[DxbxFS_IsXboxFS]
    mov chk, ah
  end;

  Result := (chk = 1);
end;
(**)

// This function is used to swap between the native Win2k/XP FS:
// structure, and the Emu FS: structure. Before running Windows
// code, you *must* swap over to Win2k/XP FS. Similarly, before
// running Xbox code, you *must* swap back over to Emu FS.
procedure EmuSwapFS;
{$J+}
const
  // Note that this is only the *approximate* interception count,
  // because not all interceptions swap the FS register, and some
  // non-interception code uses it
  dwInterceptionCount: Integer = 0;
{$J-}
begin
  asm
    mov ax, fs:[DxbxFS_SwapFS]
    mov fs, ax
  end;

  // Every "N" interceptions, perform various periodic services
  Inc(dwInterceptionCount);
  if dwInterceptionCount >= EmuAutoSleepRate then
  begin
    // If we're in the Xbox FS, wait until the next swap
    if EmuIsXboxFS then
    begin
      Dec(dwInterceptionCount);
      Exit;
    end;

    // Yield!
    SwitchToThread;

    // Back to Zero!
    dwInterceptionCount := 0;
  end;
end; // EmuSwapFS

procedure EmuSwapFS(const aSwapTo: TSwapFS);
begin
  if (aSwapTo = fsSwap)
  or ((aSwapTo = fsWindows) = EmuIsXboxFS) then
    EmuSwapFS();
end;

// initialize fs segment selector emulation
procedure EmuInitFS();
begin
  EmuInitLDT();
end;

(**)
// generate fs segment selector
procedure EmuGenerateFS(pTLS: PXBE_TLS; pTLSData: PVOID);
var
  OrgNtTib: PNT_TIB;
  pNewTLS: PXBE_TLS;
  NewFS, OrgFS: UInt16;
  dwCopySize, dwZeroSize: UInt32;
{$IFDEF _DEBUG_TRACE}
  stop: UInt32;
  v: UInt32;
  bByte: PUInt8;
  Line: string;
{$ENDIF}
  dwSize: uint32;
  NewPcr: PKPCR;
  EThread: PETHREAD;
begin
  pNewTLS := nil;
  NewFS := UInt16(-1);
  OrgFS := UInt16(-1);

  // copy global TLS to the current thread
  begin
    dwCopySize := pTLS.dwDataEndAddr - pTLS.dwDataStartAddr;
    dwZeroSize := pTLS.dwSizeofZeroFill;

    pNewTLS := CxbxMalloc(dwCopySize + dwZeroSize + $100 { + HACK: extra safety padding 0x100});

    memset(pNewTLS, 0, dwCopySize + dwZeroSize + $100);
    memcpy(pNewTLS, pTLSData, dwCopySize);
  end;

  // dump raw TLS data
  begin
{$IFDEF _DEBUG_TRACE}
    if pNewTLS = nil then
      DbgPrintf('EmuFS : TLS Non-Existant (OK)')
    else
    begin
      Line := 'EmuFS : TLS Data Dump...';

      stop := (pTLS.dwDataEndAddr - pTLS.dwDataStartAddr) + pTLS.dwSizeofZeroFill;

      for v := 0 to stop - 1 do
      begin
        if (v mod $10) = 0 then
        begin
          DbgPrintf(Line);
          Line := 'EmuFS : 0x' + PointerToString(@(PUint8(pNewTLS)[v])) + ': ';
        end;

        bByte := PUInt8(UIntPtr(pNewTLS) + v);

        Line := Line + IntToHex(Integer(bByte^), 2);

      end;

      DbgPrintf(Line);
    end;
{$ENDIF}
  end;

  asm
    // Obtain "OrgFS"
    mov ax, fs
    mov OrgFS, ax

    // Obtain "OrgNtTib"
    mov eax, fs:[FS_ThreadInformationBlockPointer]
    mov OrgNtTib, eax
  end;

  // allocate LDT entry
  begin
    dwSize := SizeOf(XboxKrnl.KPCR);
    NewPcr := CxbxMalloc(dwSize);
//    memset(NewPcr, 0, dwSize);
    memset(NewPcr, 0, SizeOf(NewPcr^));
    NewFS := EmuAllocateLDT(UInt32(NewPcr), UInt32(UIntPtr(NewPcr) + dwSize));
  end;

  // update "OrgFS" with NewFS and (bIsXboxFS = False)
  asm
    mov ax, NewFS
    mov bh, 0

    mov fs:[DxbxFS_SwapFS], ax
    mov fs:[DxbxFS_IsXboxFS], bh
  end;

  // generate TIB
  begin
    EThread := CxbxMalloc(SizeOf(xboxkrnl.ETHREAD));

    EThread.Tcb.TlsData := pNewTLS;
    EThread.UniqueThread := GetCurrentThreadId();

    memcpy(@(NewPcr.NtTib), OrgNtTib, SizeOf(NT_TIB));

    NewPcr.NtTib.Self := @(NewPcr.NtTib);

    NewPcr.PrcbData.CurrentThread := xboxkrnl.PKTHREAD(EThread);

    NewPcr.Prcb := @(NewPcr.PrcbData);
  end;

  // prepare TLS
  begin
    // TLS Index Address := 0
    pTLS.dwTLSIndexAddr := 0;

    // dword @ pTLSData := pTLSData
    if Assigned(pNewTLS) then
      PPointer(pNewTLS)^ := pNewTLS;
  end;

	// swap into "NewFS"
	EmuSwapFS();

  // update "NewFS" with OrgFS and (bIsXboxFS = True)
  asm
    mov ax, OrgFS
    mov bh, 1

    mov fs:[DxbxFS_SwapFS], ax
    mov fs:[DxbxFS_IsXboxFS], bh
  end;

  // save "TLSPtr" inside NewFS.StackBase
  asm
    mov eax, pNewTLS
    mov fs:[FS_StackBasePointer], eax
  end;

  // swap back into the "OrgFS"
	EmuSwapFS();

  DbgPrintf('EmuFS : OrgFS=' + IntToStr(OrgFS) + ' NewFS=' + IntToStr(NewFS) + ' pTLS=$' + PointerToString(pTLS));
end;
(*
CHANGING THE LAYOUT OF THE ABOVE CODE COULD GIVE YOU THIS
(which lets Dxbx run Turok one tiny step further) :
* )
// generate fs segment selector
procedure EmuGenerateFS(pTLS: PXBE_TLS; pTLSData: PVOID);
var
  OrgNtTib: PNT_TIB;
  pNewTLS: PUInt08;
  NewFS, OrgFS: UInt16;
  dwCopySize, dwZeroSize: UInt32;
{$IFDEF _DEBUG_TRACE}
  stop: UInt32;
  v: UInt32;
  bByte: PUInt8;
  Line: string;
{$ENDIF}
  dwSize: uint32;
  NewPcr: PKPCR;
  EThread: PETHREAD;
begin
  pNewTLS := nil;
  NewFS := UInt16(-1);
  OrgFS := UInt16(-1);

  // copy global TLS to the current thread
  begin
    dwCopySize := pTLS.dwDataEndAddr - pTLS.dwDataStartAddr;
    dwZeroSize := pTLS.dwSizeofZeroFill;

    pNewTLS := CxbxMalloc(dwCopySize + dwZeroSize + $100 { + HACK: extra safety padding 0x100});

    ZeroMemory(pNewTLS, dwCopySize + dwZeroSize + $100);
    CopyMemory(pNewTLS, pTLSData, dwCopySize);
  end;

  // dump raw TLS data
  begin
{$IFDEF _DEBUG_TRACE}
    if pNewTLS = nil then
      DbgPrintf('EmuFS : TLS Non-Existant (OK)')
    else
    begin
      Line := 'EmuFS : TLS Data Dump...';

      stop := (pTLS.dwDataEndAddr - pTLS.dwDataStartAddr) + pTLS.dwSizeofZeroFill;

      for v := 0 to stop - 1 do
      begin
        if (v mod $10) = 0 then
        begin
          DbgPrintf(Line);
          Line := 'EmuFS : 0x' + PointerToString(@pNewTLS[v]) + ': ';
        end;

        bByte := PUInt8(UIntPtr(pNewTLS) + v);

        Line := Line + IntToHex(Integer(bByte^), 2);

      end;

      DbgPrintf(Line);
    end;
{$ENDIF}
  end;

  // allocate LDT entry
  begin
    dwSize := SizeOf(XboxKrnl.KPCR);
    NewPcr := xboxkrnl.PKPCR(CxbxMalloc(dwSize));
    ZeroMemory(NewPcr, dwSize); // was : SizeOf(NewPcr));
    NewFS := EmuAllocateLDT(UInt32(NewPcr), UInt32(UIntPtr(NewPcr) + dwSize));
  end;

  // prepare TLS
  begin
    // TLS Index Address := 0
    pTLS.dwTLSIndexAddr := 0;

    // dword @ pTLSData := pTLSData
    if Assigned(pNewTLS) then
      PPointer(pNewTLS)^ := pNewTLS;
  end;

  asm
    // Obtain "OrgNtTib"
    mov eax, fs:[FS_ThreadInformationBlockPointer]
    mov OrgNtTib, eax
  end;

  // generate TIB
  begin
    EThread := CxbxMalloc(SizeOf(xboxkrnl.ETHREAD));

    EThread.Tcb.TlsData := pNewTLS;
    EThread.UniqueThread := GetCurrentThreadId();

    CopyMemory(@(NewPcr.NtTib), OrgNtTib, SizeOf(NT_TIB));

    NewPcr.NtTib.Self := @(NewPcr.NtTib);

    NewPcr.PrcbData.CurrentThread := xboxkrnl.PKTHREAD(EThread);

    NewPcr.Prcb := @(NewPcr.PrcbData);
  end;

  asm
    // Obtain "OrgFS"
    mov cx, fs
    mov OrgFS, cx

    // Retreive "NewFS"
    mov ax, NewFS

    // update "OrgFS" with NewFS and (bIsXboxFS = False)
    mov bh, 0
    mov fs:[DxbxFS_SwapFS], ax
    mov fs:[DxbxFS_IsXboxFS], bh

    // swap into "NewFS"
    mov fs, ax

    // update "NewFS" with OrgFS and (bIsXboxFS = True)
    mov bh, 1
    mov fs:[DxbxFS_SwapFS], cx
    mov fs:[DxbxFS_IsXboxFS], bh

    // save "TLSPtr" inside NewFS.StackBase
    mov eax, pNewTLS
    mov fs:[FS_StackBasePointer], eax

    // swap back into the "OrgFS"
    mov fs, cx
  end;

  DbgPrintf('EmuFS : OrgFS=' + IntToStr(OrgFS) + ' NewFS=' + IntToStr(NewFS) + ' pTLS=$' + PointerToString(pTLS));
end;
(**)

// cleanup fs segment selector emulation
procedure EmuCleanupFS();
var
  wSwapFS: Word;
  pTLSData: PByte;
begin
  wSwapFS := 0;

  asm
    mov ax, fs:[DxbxFS_SwapFS]
    mov wSwapFS, ax;
  end;

  if wSwapFS = 0 then
    Exit;

  EmuSwapFS(fsXbox);

  //pTLSData := NULL;

  asm
    mov eax, fs:[FS_StackBasePointer]
    mov pTLSData, eax
  end;

  EmuSwapFS(fsWindows);

  if Assigned(pTLSData) then
    CxbxFree(pTLSData);

  EmuDeallocateLDT(wSwapFS);
end;

end.
