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

type
  {$A1}
  RLDT_ENTRY_Bits = record
  public
    BaseMid: BYTE;
  private
    Flags: WORD;
    function GetBits(const aIndex: Integer): Integer;
    procedure SetBits(const aIndex: Integer; const aValue: Integer);
  public
    BaseHi: BYTE;

    property _Type: Integer index $05 read GetBits write SetBits; // 5 bits at offset 0
    property Dpl: Integer index $52 read GetBits write SetBits; // 2 bits at offset 5
    property Pres: Integer index $71 read GetBits write SetBits; // 1 bit at offset 7
    property LimitHi: Integer index $84 read GetBits write SetBits; // 4 bits at offset 8
    property Sys: Integer index $C1 read GetBits write SetBits; // 1 bit at offset 12
    property Reserved_0: Integer index $D1 read GetBits write SetBits; // 1 bit at offset 13
    property Default_Big: Integer index $E1 read GetBits write SetBits; // 1 bit at offset 14
    property Granularity: Integer index $F1 read GetBits write SetBits; // 1 bit at offset 15
  end;

  RLDT_ENTRY_Bytes = record
    BaseMid: BYTE;
    Flags1: BYTE; // Declare as bytes to avoid alignment
    Flags2: BYTE; // Problems.
    BaseHi: BYTE;
  end;

  RLDT_ENTRY_HighWord = record
    case Integer of
    0: (Bytes: RLDT_ENTRY_Bytes);
    1: (Bits: RLDT_ENTRY_Bits); // Bit-fields are handled seperatly
  end;

  DXBX_LDT_ENTRY = record
    LimitLow: WORD;
    BaseLow: WORD;
    HighWord: RLDT_ENTRY_HighWord;
  end;

{$OPTIMIZATION ON}
{$OVERFLOWCHECKS OFF}
function RLDT_ENTRY_Bits.GetBits(const aIndex: Integer): Integer;
var
  Offset: Integer;
  NrBits: Integer;
  Mask: Integer;
begin
  NrBits := aIndex and $F;
  Offset := aIndex shr 4;

  Mask := ((1 shl NrBits) - 1);

  Result := Flags;
  Result := Result shr Offset;
  Result := Result and Mask;
end;

procedure RLDT_ENTRY_Bits.SetBits(const aIndex: Integer; const aValue: Integer);
var
  Offset: Integer;
  NrBits: Integer;
  Mask: Integer;
  Result: Integer;
begin
  NrBits := aIndex and $F;
  Offset := aIndex shr 4;

  Mask := ((1 shl NrBits) - 1);

  Result := Flags;
    Result := Result and (not (Mask shl Offset));
    Result := Result or (aValue shl Offset);
  Flags := WORD(Result);
end;

//

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
  _LDTENTRY: DXBX_LDT_ENTRY absolute LDTEntry;
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
    ZeroMemory(@LDTENTRY, SizeOf(LDTENTRY));

    _LDTEntry.BaseLow                    := WORD(dwBaseAddr and $FFFF);
    _LDTEntry.HighWord.Bits.BaseMid      := (dwBaseAddr shr 16) and $FF;
    _LDTEntry.HighWord.Bits.BaseHi       := (dwBaseAddr shr 24) and $FF;
    _LDTEntry.HighWord.Bits._Type        := $13; // RW data segment
    _LDTEntry.HighWord.Bits.Dpl          := 3;    // user segment
    _LDTEntry.HighWord.Bits.Pres         := 1;    // present
    _LDTEntry.HighWord.Bits.Sys          := 0;
    _LDTEntry.HighWord.Bits.Reserved_0   := 0;
    _LDTEntry.HighWord.Bits.Default_Big  := 1;    // 386 segment
    _LDTEntry.HighWord.Bits.Granularity  := iif(dwLimit >= $00100000, 1, 0);

    if _LDTEntry.HighWord.Bits.Granularity > 0 then
      dwLimit := dwLimit shr 12;

    _LDTEntry.LimitLow                   := WORD(dwLimit and $FFFF);
    _LDTEntry.HighWord.Bits.LimitHi      := (dwLimit shr 16) and $F;
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
