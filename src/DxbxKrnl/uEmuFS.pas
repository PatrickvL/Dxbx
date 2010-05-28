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

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, // SwitchToThread
  SysUtils, // IntToHex
{$IFDEF DXBX_EXTENSIVE_CALLSTACK_LOGGING}
  Classes, // TStringList
  // Jcl
  JclDebug,
{$ENDIF}
  JclWin32,
  // Jedi Win32API
  JwaWinType,
  // OpenXDK
  XboxKrnl, // NT_TIB
  // Dxbx
  uTypes,
  uDxbxUtils,
  uLog,
  uXbe,
  uEmuAlloc,
  uEmuLDT;

procedure EmuInitFS();

procedure EmuGenerateFS(pTLS: PXBE_TLS; pTLSData: PVOID);

procedure EmuCleanupFS();

function EmuIsXboxFS(): _bool; {$IFDEF SUPPORTS_INLINE_ASM}inline; {$ENDIF}

// Dxbx addition :
type
  TSwapFS = (fsSwap, fsWindows, fsXbox);

procedure EmuSwapFS(); {$IFDEF SUPPORTS_INLINE_ASM}inline; {$ENDIF} overload;
procedure EmuSwapFS(const aSwapTo: TSwapFS); {$IFDEF SUPPORTS_INLINE_ASM}inline; {$ENDIF} overload;

function DumpCurrentFS(): string;

const
  // TIB layout, reachable via FS :
  FS_StructuredExceptionHandlerPointer = $00;
  FS_StackBasePointer = $04;
  FS_StackLimit = $08; // Int32
  FS_SubSystemTib = $0C; // Pointer
  FS_FiberData = $10; // for TIB : Pointer
  FS_Version = $10; // for TEB (?) : Int32
  FS_ArbitraryUserPointer = $14;
  FS_ThreadInformationBlockPointer = $18;
  FS_SelfPcr = $1C; // Pointer to self
  FS_Prcb = $20; // In NT this is called _Client_ID
  FS_Irql = $24;
  FS_PrcbData = $28;
  FS_ProcessEnvironmentBlock = $30; // FS points to TEB/TIB which has a pointer to the PEB

  // Aliases :
  FS_ExceptionList = FS_StructuredExceptionHandlerPointer; // $00
  FS_Self = FS_ThreadInformationBlockPointer; // $18

  // Dxbx values inside FS_ArbitraryUserPointer :
  DxbxFS_SwapFS = FS_ArbitraryUserPointer; // = $14 : UInt16
  DxbxFS_IsXboxFS = FS_ArbitraryUserPointer + SizeOf(UInt16); // = $16 : ByteBool

implementation

// is the current fs register the xbox emulation variety?
function EmuIsXboxFS(): _bool;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := (GetTIBEntryWord(DxbxFS_IsXboxFS) <> 0);
end;

//
// Xbox is a single process system, and because of this fact, demos
// and games are likely to suffer from Xbox-Never-Sleeps syndrome.
//
// Basically, there are situations where the Xbe will have no
// reason to bother yielding to other threads. One solution to this
// problem is to keep track of the number of function intercepts,
// and every so often, force a sleep. This is the rate at which
// those forced sleeps occur.
// Automatically insert after this many EmuFS() swaps :
var EmuAutoSleepRate: uint32 = uint32(-1);

//
// This function is used to swap between the native Win2k/XP FS:
// structure, and the Emu FS: structure. Before running Windows
// code, you *must* swap over to Win2k/XP FS. Similarly, before
// running Xbox code, you *must* swap back over to Emu FS.
//
// Dxbx NOTE : DO NOT USE WriteLn (or seemingly other I/O) while
// inside the Xbox FS state, or the Win32 kernel will merrily
// restore the FS register - which is NOT what we want here!
procedure EmuSwapFS();
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
{$WRITEABLECONST ON}
const
  // Note that this is only the *approximate* interception count,
  // because not all interceptions swap the FS register, and some
  // non-interception code uses it
  dwInterceptionCount: uint32 = 0;
{$WRITEABLECONST OFF}
var
  CurrentFS: Word;
begin
  asm
    mov ax, fs:[DxbxFS_SwapFS]
    mov fs, ax
  end;

  // every "N" interceptions, perform various periodic services
  Inc(dwInterceptionCount);
  if dwInterceptionCount >= EmuAutoSleepRate then
  begin
    // If we're in the Xbox FS, wait until the next swap
    if EmuIsXboxFS then
    begin
      Dec(dwInterceptionCount);
      Exit;
    end;

    // Somehow, SwitchToThread also alters FS, so store it first :
    CurrentFS := GetFS();

    // Yield!
    Sleep(1); // Dxbx used SwitchToThread, but that keeps the application burning cycles

    // Restore the FS register again :
    SetFS(CurrentFS);

    // Back to Zero!
    dwInterceptionCount := 0;
  end;
end; // EmuSwapFS

{$IFDEF DXBX_EXTENSIVE_CALLSTACK_LOGGING}
var
  // CallStack[True] is latest callstack when leaving Xbox FS
  // CallStack[False] is latest callstack when leaving Windows FS
  LastCallStackInfo: array [Boolean] of TJclStackInfoList;
  LastCallStackLst: array [Boolean] of TStringList;
  LastCallStackStr: array [Boolean] of string;
{$ENDIF}

procedure EmuSwapFS(const aSwapTo: TSwapFS);
var
  InXboxFS: Boolean;
begin
  InXboxFS := EmuIsXboxFS;
  if (aSwapTo = fsSwap)
  or ((aSwapTo = fsWindows) = InXboxFS) then
  begin
{$IFDEF DXBX_EXTENSIVE_CALLSTACK_LOGGING}
    if not InXboxFS then
    begin
      LastCallStackLst[InXboxFS].Clear;
      LastCallStackInfo[InXboxFS].AddToStrings(LastCallStackLst[InXboxFS]);
      LastCallStackStr[InXboxFS] := LastCallStackLst[InXboxFS].Text;
    end;
{$ENDIF}
    EmuSwapFS();
  end;
end;

// initialize fs segment selector emulation
procedure EmuInitFS();
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuInitLDT();
end;

function GetCurrentKPCR(): PKPCR; // inline;
begin
  Result := GetTIBEntry(FS_SelfPcr);
end;

function DumpXboxFS(const aFS: PKPCR): string;
begin
  with aFS.NtTib do
    Result := DxbxFormat(
      '$%.02x KPCR.NT_TIB.ExceptionList: PEXCEPTION_REGISTRATION_RECORD = $%.08x'#13#10 +
      '$%.02x KPCR.NT_TIB.StackBase: PVOID = $%.08x'#13#10 +
      '$%.02x KPCR.NT_TIB.StackLimit: PVOID = $%.08x'#13#10 +
      '$%.02x KPCR.NT_TIB.SubSystemTib: PVOID = $%.08x'#13#10 +
      '$%.02x KPCR.NT_TIB.FiberData: PVOID = $%.08x'#13#10 +
      '$%.02x KPCR.NT_TIB.Version: ULONG = $%.08x'#13#10 +
      '$%.02x KPCR.NT_TIB.Dxbx_SwapFS: WORD = $%.04x'#13#10 +
      '$%.02x KPCR.NT_TIB.Dxbx_IsXboxFS: WORDBOOL = $%.04x'#13#10 +
      '$%.02x KPCR.NT_TIB.Self: PNT_TIB = $%.08x',
      [FIELD_OFFSET(PKPCR(nil).NtTib.ExceptionList), ExceptionList,
      FIELD_OFFSET(PKPCR(nil).NtTib.StackBase), StackBase,
      FIELD_OFFSET(PKPCR(nil).NtTib.StackLimit), StackLimit,
      FIELD_OFFSET(PKPCR(nil).NtTib.SubSystemTib), SubSystemTib,
      FIELD_OFFSET(PKPCR(nil).NtTib.union_a.FiberData), union_a.FiberData,
      FIELD_OFFSET(PKPCR(nil).NtTib.union_a.Version), union_a.Version,
      FIELD_OFFSET(PKPCR(nil).NtTib.union_b.Dxbx_SwapFS), union_b.Dxbx_SwapFS,
      FIELD_OFFSET(PKPCR(nil).NtTib.union_b.Dxbx_IsXboxFS), union_b.Dxbx_IsXboxFS,
      FIELD_OFFSET(PKPCR(nil).NtTib.Self), aFS.NtTib.Self]);

  with aFS^ do
    Result := Result + DxbxFormat(#13#10 +
      '$%.02x KPCR.SelfPcr: PKPCR = $%.08x'#13#10 +
      '$%.02x KPCR.Prcb: PKPRCB = $%.08x'#13#10 +
      '$%.02x KPCR.Irql: UCHAR = %3d',
      [FIELD_OFFSET(PKPCR(nil).SelfPcr), SelfPcr,
      FIELD_OFFSET(PKPCR(nil).Prcb), Prcb,
      FIELD_OFFSET(PKPCR(nil).Irql), Irql]);

  with aFS.PrcbData do
    Result := Result + DxbxFormat(#13#10 +
      '$%.02x KPCR.PrcbData.CurrentThread: PKTHREAD = $%.08x'#13#10 +
      '$%.02x KPCR.PrcbData.NextThread: PKTHREAD = $%.08x'#13#10 +
      '$%.02x KPCR.PrcbData.IdleThread: PKTHREAD = $%.08x',
      [FIELD_OFFSET(PKPCR(nil).PrcbData.CurrentThread), CurrentThread,
      FIELD_OFFSET(PKPCR(nil).PrcbData.NextThread), NextThread,
      FIELD_OFFSET(PKPCR(nil).PrcbData.IdleThread), IdleThread]);
end;

function DumpWin32FS(const aFS: JclWin32.PNT_TIB32): string;
begin
  with aFS^ do
    Result := DxbxFormat(
      '$%.02x NT_TIB32.ExceptionList: DWORD = $%.08x'#13#10 +
      '$%.02x NT_TIB32.StackBase: DWORD = $%.08x'#13#10 +
      '$%.02x NT_TIB32.StackLimit: DWORD = $%.08x'#13#10 +
      '$%.02x NT_TIB32.SubSystemTib: DWORD = $%.08x'#13#10 +
      '$%.02x NT_TIB32.FiberData: DWORD = $%.08x'#13#10 +
      '$%.02x NT_TIB32.Version: DWORD = $%.08x'#13#10 +
      '$%.02x NT_TIB32.ArbitraryUserPointer: DWORD = $%.08x'#13#10 +
      '$%.02x NT_TIB32.Self: DWORD = $%.08x',
      [FIELD_OFFSET(PNT_TIB32(nil).ExceptionList), ExceptionList,
      FIELD_OFFSET(PNT_TIB32(nil).StackBase), StackBase,
      FIELD_OFFSET(PNT_TIB32(nil).StackLimit), StackLimit,
      FIELD_OFFSET(PNT_TIB32(nil).SubSystemTib), SubSystemTib,
      FIELD_OFFSET(PNT_TIB32(nil).FiberData), FiberData,
      FIELD_OFFSET(PNT_TIB32(nil).Version), Version,
      FIELD_OFFSET(PNT_TIB32(nil).ArbitraryUserPointer), ArbitraryUserPointer,
      FIELD_OFFSET(PNT_TIB32(nil).Self), aFS.Self]);
end;

function DumpCurrentFS(): string;
begin
  Result := DxbxFormat('FS = $%.04x ($%.08x)', [GetFS(), GetTIB()]);
  if EmuIsXboxFS then
    Result := Result + ' (Xbox FS)'#13#10 + DumpXboxFS(GetTIB())
  else
    Result := Result + ' (Win32 FS)'#13#10 + DumpWin32FS(GetTIB());
end;

// generate fs segment selector
procedure EmuGenerateFS(pTLS: PXBE_TLS; pTLSData: PVOID);
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pNewTLS: PXBE_TLS;
  NewFS, OrgFS: UInt16;
  OrgNtTib: PNT_TIB;
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
{$IFDEF DXBX_EXTREME_LOGGING}
  DbgPrintf('Entering EmuGenerateFS() : '#13#10 + DumpCurrentFS());
{$ENDIF}

  // copy global TLS to the current thread
  if Assigned(pTLS) then
  begin
    dwCopySize := pTLS.dwDataEndAddr - pTLS.dwDataStartAddr;
    dwZeroSize := pTLS.dwSizeofZeroFill;

    pNewTLS := CxbxMalloc(dwCopySize + dwZeroSize + $100 { + HACK: extra safety padding 0x100});

    memset(pNewTLS, 0, dwCopySize + dwZeroSize + $100);
    memcpy(pNewTLS, pTLSData, dwCopySize);
  end
  else
  begin
    pNewTLS := nil;
    dwCopySize := 0;
    dwZeroSize := 0;
  end;

{$IFDEF _DEBUG_TRACE}
  // dump raw TLS data
  begin
    if (pNewTLS = nil)
    or (dwCopySize + dwZeroSize = 0) then
      DbgPrintf('EmuFS : TLS Non-Existant (OK)')
    else
    begin
      Line := 'EmuFS : TLS Data Dump...';

      stop := (pTLS.dwDataEndAddr - pTLS.dwDataStartAddr) + pTLS.dwSizeofZeroFill;

      if stop > 0 then // Dxbx addition, to prevent underflow
      for v := 0 to stop - 1 do
      begin
        if (v mod $10) = 0 then
        begin
          DbgPrintf(Line);
          Line := 'EmuFS : 0x' + PointerToString(@(PUInt8(pNewTLS)[v])) + ': ';
        end;

        bByte := PUInt8(UIntPtr(pNewTLS) + v);

        Line := Line + IntToHex(Integer(bByte[0]), 2);

      end;

      DbgPrintf(Line);
    end;
  end;
{$ENDIF}

  OrgFS := GetFS();
  OrgNtTib := GetTIB();

  // allocate LDT entry
  begin
    dwSize := sizeof(XboxKrnl.KPCR);

    NewPcr := CxbxMalloc(dwSize);
    memset(NewPcr, 0, dwSize);
    NewFS := EmuAllocateLDT(uint32(NewPcr), uint32(UIntPtr(NewPcr) + dwSize));
  end;

  // update "OrgFS" with NewFS and (bIsXboxFS = False)
  OrgNtTib.union_b.Dxbx_SwapFS := NewFS;
  OrgNtTib.union_b.Dxbx_IsXboxFS := False;

{$IFDEF DXBX_EXTREME_LOGGING}
  DbgPrintf('update "OrgFS" ($%.04x) with NewFS ($%.04x) and (bIsXboxFS = False) : '#13#10 + DumpCurrentFS(), [OrgFS, NewFS]);
{$ENDIF}

  // generate TIB
  begin
    EThread := CxbxMalloc(sizeof(xboxkrnl.ETHREAD));

    EThread.Tcb.TlsData := pNewTLS;
    EThread.UniqueThread := GetCurrentThreadId();

    memcpy(@(NewPcr.NtTib), OrgNtTib, sizeof(NT_TIB));

    NewPcr.NtTib.Self := @(NewPcr.NtTib);

    NewPcr.PrcbData.CurrentThread := xboxkrnl.PKTHREAD(EThread);

    NewPcr.Prcb := @(NewPcr.PrcbData);

//    NewPcr.SelfPcr := NewPcr; // TODO -oDXBX: Is this correct?
  end;

  // prepare TLS
  if Assigned(pTLS) then
  begin
    // TLS Index Address := 0
    if Assigned(PUInt32(pTLS.dwTLSIndexAddr)) then
      Puint32(pTLS.dwTLSIndexAddr)^ := 0;

    // dword @ pTLSData := pTLSData
    if Assigned(pNewTLS) then
      PPvoid(pNewTLS)^ := pNewTLS;
  end;

  // update "NewFS" with OrgFS and (bIsXboxFS = True)
  NewPcr.NtTib.union_b.Dxbx_SwapFS := OrgFS;
  NewPcr.NtTib.union_b.Dxbx_IsXboxFS := True;

  // save "TLSPtr" inside NewFS.StackBase
  NewPcr.NtTib.StackBase := pNewTLS;

{$IFDEF DXBX_EXTREME_LOGGING}
  DbgPrintf('Xbox FS'#13#10 + DumpXboxFS(NewPcr));

  DbgPrintf('swap back into the "OrgFS" : '#13#10 + DumpCurrentFS());
{$ENDIF}

{$IFDEF DEBUG}
  DbgPrintf('EmuFS : CurrentFS=%.04x  OrgFS=%d  NewFS=%d  pTLS=0x%.08x', [GetFS(), OrgFS, NewFS, pTLS]);
{$ENDIF}
end;

// cleanup fs segment selector emulation
procedure EmuCleanupFS();
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  wSwapFS: uint16;
  pTLSData: Puint08;
begin
  wSwapFS := GetTIBEntryWord(DxbxFS_SwapFS);
  if wSwapFS = 0 then
    Exit;

  EmuSwapFS(fsXbox);

  pTLSData := GetTIBEntry(FS_StackBasePointer);

  EmuSwapFS(fsWindows);

  if (pTLSData <> nil) then
    CxbxFree(pTLSData);

  EmuDeallocateLDT(wSwapFS);
end;

initialization

  Assert(FIELD_OFFSET(PKPCR(nil).NtTib.union_b.Dxbx_SwapFS) = DxbxFS_SwapFS);
  Assert(FIELD_OFFSET(PKPCR(nil).NtTib.union_b.Dxbx_IsXboxFS) = DxbxFS_IsXboxFS);
  Assert(FIELD_OFFSET(PKPCR(nil).SelfPcr) = FS_SelfPcr);
  Assert(FIELD_OFFSET(PKPCR(nil).Prcb) = FS_Prcb);
  Assert(FIELD_OFFSET(PKPCR(nil).Irql) = FS_Irql);
  Assert(FIELD_OFFSET(PKPCR(nil).PrcbData) = FS_PrcbData);

{$IFDEF DXBX_EXTENSIVE_CALLSTACK_LOGGING}
  LastCallStackInfo[True] := JclCreateStackList(False, 0, nil, True);
  LastCallStackInfo[False] := JclCreateStackList(False, 0, nil, True);

  LastCallStackLst[True] := TStringList.Create;
  LastCallStackLst[False] := TStringList.Create;
{$ENDIF}

end.
