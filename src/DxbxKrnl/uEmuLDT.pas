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

(*
This unit contains 'Local Descriptor Table'-related emulation methods.
See http://en.wikipedia.org/wiki/Local_Descriptor_Table for more details.
*)

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // JEDI
  JwaWinType,
  JwaWinNT,
  JwaNative, // NtSetLdtEntries
  // Dxbx
  uLog,
  uConsts,
  uDxbxKrnlUtils;

procedure EmuInitLDT;
function EmuAllocateLDT(dwBaseAddr: uint32; dwLimit: uint32): uint16;
procedure EmuDeallocateLDT(wSelector: uint16);

implementation

var
  // Table of free LDT entries
  FreeLDTEntries: array[0..MAXIMUM_XBOX_THREADS - 1] of UInt16;

  // Critical section lock
  EmuLDTLock: Windows._RTL_CRITICAL_SECTION;

procedure EmuInitLDT;
var
  v: UInt32;
begin
  InitializeCriticalSection({var}EmuLDTLock);

  for v := 0 to MAXIMUM_XBOX_THREADS - 1 do
    FreeLDTEntries[v] := UInt16((v * 8) + 7 + 8);
end;

function EmuAllocateLDT(dwBaseAddr: uint32; dwLimit: uint32): uint16;
var
  LDTEntry: LDT_ENTRY;
  x: Integer;
begin
  x := 0;

  EnterCriticalSection(EmuLDTLock);

  // Locate a free LDT entry
  begin
    while x < MAXIMUM_XBOX_THREADS do
      if FreeLDTEntries[x] = 0 then
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

  // Set up selector information
  begin
    LDTEntry.BaseLow := WORD(dwBaseAddr and $FFFF);
    LDTEntry.BaseMid := (dwBaseAddr shr 16) and $FF;
    LDTEntry.BaseHi := (dwBaseAddr shr 24) and $FF;
    LDTEntry.Flags1 := 0;
    LDTEntry.Flags1 := LDTEntry.Flags1 or ($13 and LDTENTRY_FLAGS1_TYPE); // RW data segment
    LDTEntry.Flags1 := LDTEntry.Flags1 or LDTENTRY_FLAGS1_DPL; // user segment
    LDTEntry.Flags1 := LDTEntry.Flags1 or LDTENTRY_FLAGS1_PRES; // present
    LDTEntry.Flags2 := 0;
    LDTEntry.Flags2 := LDTEntry.Flags2 or ($0 and LDTENTRY_FLAGS2_SYS);
    LDTEntry.Flags2 := LDTEntry.Flags2 or ($0 and LDTENTRY_FLAGS2_RESERVED_0);
    LDTEntry.Flags2 := LDTEntry.Flags2 or (LDTENTRY_FLAGS2_DEFAULT_BIG); // 386 segment
    if {LDTEntry.HighWord.Bits.Granularity=} dwLimit >= $00100000 then
    begin
      LDTEntry.Flags2 := LDTEntry.Flags2 or LDTENTRY_FLAGS2_GRANULARITY;
      dwLimit := dwLimit shr 12;
    end;

    LDTEntry.LimitLow := WORD(dwLimit and $FFFF);
    LDTEntry.Flags2 := LDTEntry.Flags2 or ((dwLimit shr 16) and LDTENTRY_FLAGS2_LIMITHI);
  end;

  // Allocate selector
  begin
    if not NT_SUCCESS(NtSetLdtEntries((x * 8) + 7 + 8, LDTEntry, 0, LDTEntry)) then
    begin
      WriteLog(GetLastErrorString);
      LeaveCriticalSection(EmuLDTLock);

      CxbxKrnlCleanup('Could not set LDT entries');

      Result := 0;
      Exit;
    end;
  end;

  LeaveCriticalSection(EmuLDTLock);

  FreeLDTEntries[x] := 0;

  Result := (x * 8) + 7 + 8;
end;

procedure EmuDeallocateLDT(wSelector: uint16);
var
  LDTEntry: LDT_ENTRY;
begin
  EnterCriticalSection(EmuLDTLock);

  ZeroMemory(@LDTEntry, SizeOf(LDTEntry));

  NtSetLdtEntries(wSelector, LDTEntry, 0, LDTEntry);

  FreeLDTEntries[(wSelector shr 3) - 1] := wSelector;

  LeaveCriticalSection(EmuLDTLock);
end;

end.
