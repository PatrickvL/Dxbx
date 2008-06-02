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
  Windows, // for SwitchToThread
  // Dxbx
  uLog,
  uXbe,
  uEmuLDT;


procedure EmuSwapFS; {$IFDEF SUPPORTS_INLINE_ASM} inline; {$ENDIF}
procedure EmuInitFS;
procedure EmuGenerateFS(pTLS: P_XBE_TLS; pTLSData: Pointer);
procedure EmuCleanupFS;
function EmuIsXboxFS: Boolean; {$IFDEF SUPPORTS_INLINE_ASM} inline; {$ENDIF}

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
function EmuIsXboxFS: Boolean;
var
  chk: Byte;
begin
  asm
    mov ah, fs:16h
    mov chk, ah
  end;

  Result := (chk = 1);
end;

// This function is used to swap between the native Win2k/XP FS:
// structure, and the Emu FS: structure. Before running Windows
// code, you *must* swap over to Win2k/XP FS. Similarly, before
// running Xbox code, you *must* swap back over to Emu FS.
procedure EmuSwapFS();
const
{$J+}
  dwInterceptionCount: Integer = 0;
{$J-}
begin
  // Note that this is only the *approximate* interception count,
  // because not all interceptions swap the FS register, and some
  // non-interception code uses it

  asm
    mov ax, fs:14h
    mov fs, ax
  end;

  // Every "N" interceptions, perform various periodic services
  Inc(dwInterceptionCount);
  if (dwInterceptionCount - 1) >= EmuAutoSleepRate then
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

// initialize fs segment selector emulation
procedure EmuInitFS();
begin
  EmuInitLDT();
end;

// generate fs segment selector
procedure EmuGenerateFS(pTLS: P_XBE_TLS; pTLSData: Pointer);
begin
(*
  NT_TIB         *OrgNtTib;
  xboxkrnl.KPCR *NewPcr;

  uint08 *pNewTLS := 0;

  uint16 NewFS := -1, OrgFS = -1;

  // copy global TLS to the current thread
  begin
    uint32 dwCopySize := pTLS^.dwDataEndAddr - pTLS^.dwDataStartAddr;
    uint32 dwZeroSize := pTLS^.dwSizeofZeroFill;

    pNewTLS := (uint08)CxbxMalloc(dwCopySize + dwZeroSize + $100 (* + HACK: extra safety padding 0x100* ));

    FillChar(pNewTLS, 0, dwCopySize + dwZeroSize + $100);
    memcpy(pNewTLS, pTLSData, dwCopySize);
  end;

  // dump raw TLS data
  begin
{$IFDEF _DEBUG_TRACE}
		if pNewTLS = 0 then
			DbgPrintf('EmuFS ($ mod X): TLS Non-Existant (OK)', GetCurrentThreadId())
		else
		begin
			DbgPrintf('EmuFS ($ mod X): TLS Data DumpArgs: array of const', GetCurrentThreadId());
            DbgPrintf('EmuFS ($ mod X): $ mod .08X: ', GetCurrentThreadId(), pNewTLS);

			uint32 stop := pTLS^.dwDataEndAddr - pTLS^.dwDataStartAddr + pTLS^.dwSizeofZeroFill;

			for(uint32 v:=0;v<stop;v++)
			begin
				uint08 *bByte := (uint08)pNewTLS + v;

				DbgPrintf(' mod .01X', (uint32)*bByte);

				if((v+1) mod $10 = 0 and v+1<stop) then
					DbgPrintf('EmuFS ($ mod X): $ mod .08X: ', GetCurrentThreadId(), ((uint32)pNewTLS + v));
			 end;

			DbgPrintf('');
		 end;
{$ENDIF}
  end;

  asm
    // Obtain "OrgFS"
    mov ax, fs
    mov OrgFS, ax

    // Obtain "OrgNtTib"
    mov eax, fs:[$18]
    mov OrgNtTib, eax
  end;

  // allocate LDT entry
  begin
    uint32 dwSize := SizeOf(xboxkrnl.KPCR);

    NewPcr := (xboxkrnl.KPCR)CxbxMalloc(dwSize);

    FillChar(NewPcr, 0, SizeOf(NewPcr));

    NewFS := EmuAllocateLDT((uint32)NewPcr, (uint32)NewPcr + dwSize);
  end;

  // update "OrgFS" with NewFS and (bIsXboxFS = false)
  asm
    mov ax, NewFS
    mov bh, 0

    mov fs:[$14], ax
    mov fs:[$16], bh
  end;

  // generate TIB
  begin
    xboxkrnl.ETHREAD *EThread := (xboxkrnl.ETHREAD)CxbxMalloc(SizeOf(xboxkrnl.ETHREAD));

    EThread^.Tcb.TlsData  := pNewTLS;
    EThread^.UniqueThread := GetCurrentThreadId();

    memcpy(@NewPcr^.NtTib, OrgNtTib, SizeOf(NT_TIB));

    NewPcr^.NtTib.Self := @NewPcr^.NtTib;

    NewPcr^.PrcbData.CurrentThread := (xboxkrnl.KTHREAD)EThread;

    NewPcr^.Prcb := @NewPcr^.PrcbData;
  end;

  // prepare TLS
  begin
    // TLS Index Address := 0
    *(uint32)pTLS^.dwTLSIndexAddr := 0;

    // dword @ pTLSData := pTLSData
    if(pNewTLS <> 0) then
        *pNewTLS := pNewTLS;
  end;

  // swap into "NewFS"
  EmuSwapFS();

  // update "NewFS" with OrgFS and (bIsXboxFS = true)
  asm
    mov ax, OrgFS
    mov bh, 1

    mov fs:[$14], ax
    mov fs:[$16], bh
  end;

  // save "TLSPtr" inside NewFS.StackBase
  asm
    mov eax, pNewTLS
    mov fs:[$04], eax
  end;

  // swap back into the "OrgFS"
  EmuSwapFS();

  DbgPrintf('EmuFS ($ mod X): OrgFS:= mod d NewFS= mod d pTLS=$ mod .08X', GetCurrentThreadId(), OrgFS, NewFS, pTLS);
*)
end;

// cleanup fs segment selector emulation
procedure EmuCleanupFS();
var
  wSwapFS: Word;
  pTLSData: PByte;
begin
  wSwapFS := 0;

  asm
    mov ax, fs:14h  // FS.ArbitraryUserPointer
    mov wSwapFS, ax;
  end;

  if wSwapFS = 0 then
    Exit;

  if not EmuIsXboxFS() then
    EmuSwapFS();    // Xbox FS

  asm
    mov eax, fs:04h
    mov pTLSData, eax
  end;

  EmuSwapFS(); // Win2k/XP FS

  if pTLSData <> nil then
    FreeMem(pTLSData);

// TODO :  EmuDeallocateLDT(wSwapFS);
end;

end.
