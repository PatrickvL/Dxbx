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
unit uEmuLDT;

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // JEDI
  JwaWinType,
  // Dxbx
  uConsts,
  uDxbxKrnlUtils;
  
procedure EmuInitLDT;
function EmuAllocateLDT(dwBaseAddr: uint32; dwLimit: uint32): uint16;
procedure EmuDeallocateLDT(wSelector: uint16);

implementation

var
  // Table of free LDT entries
  FreeLDTEntries : array [00..MAXIMUM_XBOX_THREADS-1] of DWord;

  // Critical section lock
  EmuLDTLock: _RTL_CRITICAL_SECTION;

procedure EmuInitLDT;
var
  v: Integer;
begin
  InitializeCriticalSection(EmuLDTLock);

  for v := 0 to MAXIMUM_XBOX_THREADS - 1 do
    FreeLDTEntries[v] := DWord((v * 8) + 7 + 8);
end;

function EmuAllocateLDT(dwBaseAddr: uint32; dwLimit: uint32): uint16;
var
//  LDTEntry: NtDll.LDT_ENTRY;
  x: Integer;
begin
  x := 0;

  EnterCriticalSection(EmuLDTLock);

  // Locate a free LDT entry
  begin
    while x < MAXIMUM_XBOX_THREADS do
      if FreeLDTEntries[x] > 0 then
        Inc(x)
      else
        Break;

    if x = MAXIMUM_XBOX_THREADS then
    begin
      LeaveCriticalSection(EmuLDTLock);

			CxbxKrnlCleanup('Could not locate free LDT entry (too many threads?)');

      Result := 0;
      Exit;
    end;
  end;
(*
  // Set up selector information
  begin
    LDTEntry.BaseLow                    := WORD(dwBaseAddr and $FFFF);
    LDTEntry.HighWord.Bits.BaseMid      := (dwBaseAddr shr 16) and $FF;
    LDTEntry.HighWord.Bits.BaseHi       := (dwBaseAddr shr 24) and $FF;
    LDTEntry.HighWord.Bits.cType        := $13; // RW data segment
    LDTEntry.HighWord.Bits.Dpl          := 3;    // user segment
    LDTEntry.HighWord.Bits.Pres         := 1;    // present
    LDTEntry.HighWord.Bits.Sys          := 0;
    LDTEntry.HighWord.Bits.Reserved_0   := 0;
    LDTEntry.HighWord.Bits.Default_Big  := 1;    // 386 segment
    LDTEntry.HighWord.Bits.Granularity  := iif(dwLimit >= $00100000, 1, 0);

    if LDTEntry.HighWord.Bits.Granularity > 0 then
      dwLimit := dwLimit shr 12;

    LDTEntry.LimitLow                   := WORD(dwLimit and $FFFF);
    LDTEntry.HighWord.Bits.LimitHi      := (dwLimit shr 16) and $F;
  end;

  // Allocate selector
  begin
//        using namespace NtDll;

    if not NT_SUCCESS(NtDll.NtSetLdtEntries((x*8)+7+8, LDTEntry, 0, LDTEntry)) then
    begin
      LeaveCriticalSection(EmuLDTLock);

			CxbxKrnlCleanup('Could not set LDT entries');

      Result := 0;
      Exit;
    end;
  end;
*)
  LeaveCriticalSection(EmuLDTLock);

  FreeLDTEntries[x] := 0;

  Result := (x*8)+7+8;
end;

procedure EmuDeallocateLDT(wSelector: uint16);
//var
//  LDTEntry: NtDll.LDT_ENTRY;
begin
  EnterCriticalSection(EmuLDTLock);
(*
  ZeroMemory(LDTEntry, SizeOf(LDTEntry));

  NtDll.NtSetLdtEntries(wSelector, LDTEntry, 0, LDTEntry);
*)
  FreeLDTEntries[(wSelector shr 3)-1] := wSelector;

  LeaveCriticalSection(EmuLDTLock);
end;

end.

