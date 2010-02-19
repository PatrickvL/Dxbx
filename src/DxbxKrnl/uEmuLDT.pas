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

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // Jedi Win32API
  JwaWinType,
  JwaWinNT,
  JwaNative, // NtSetLdtEntries
  // Dxbx
  uConsts,
  uDxbxUtils,
  uLog,
  uDxbxKrnlUtils;

procedure EmuInitLDT;
function EmuAllocateLDT(dwBaseAddr: uint32; dwLimit: uint32): uint16;
procedure EmuDeallocateLDT(wSelector: uint16);

implementation

type
  {$A1}
  RLDT_ENTRY_Bits = packed record
  private
    Flags: DWord;
    function GetBits(const aIndex: Integer): Integer;
    procedure SetBits(const aIndex: Integer; const aValue: Integer);
  public
    property BaseMid: Integer index $0008 read GetBits write SetBits; // 8 bits at offset 0
    property Type_: Integer index $0805 read GetBits write SetBits; // 5 bits at offset 8
    property Dpl: Integer index $0D02 read GetBits write SetBits; // 2 bits at offset 13
    property Pres: Integer index $0F01 read GetBits write SetBits; // 1 bit at offset 15
    property LimitHi: Integer index $1004 read GetBits write SetBits; // 4 bits at offset 16
    property Sys: Integer index $1401 read GetBits write SetBits; // 1 bit at offset 20
    property Reserved_0: Integer index $1501 read GetBits write SetBits; // 1 bit at offset 21
    property Default_Big: Integer index $1601 read GetBits write SetBits; // 1 bit at offset 22
    property Granularity: Integer index $1701 read GetBits write SetBits; // 1 bit at offset 23
    property BaseHi: Integer index $1808 read GetBits write SetBits; // 8 bits at offset 24
  end;

  RLDT_ENTRY_Bytes = packed record
    BaseMid: Byte;
    Flags1: Byte; // Declare as bytes to avoid alignment
    Flags2: Byte; // Problems.
    BaseHi: Byte;
  end;

  RLDT_ENTRY_HighWord = packed record
    case Integer of
    0: (Bytes: RLDT_ENTRY_Bytes);
    1: (Bits: RLDT_ENTRY_Bits); // Bit-fields are handled seperatly
  end;

  DXBX_LDT_ENTRY = packed record
    LimitLow: Word;
    BaseLow: Word;
    HighWord: RLDT_ENTRY_HighWord;
  end;

{$OPTIMIZATION ON}
{$OVERFLOWCHECKS OFF}
function RLDT_ENTRY_Bits.GetBits(const aIndex: Integer): Integer;
begin
  Result := GetDWordBits(Flags, aIndex);
end;

procedure RLDT_ENTRY_Bits.SetBits(const aIndex: Integer; const aValue: Integer);
begin
  SetDWordBits({var}Flags, aIndex, aValue);
end;

//

var
  // Table of free LDT entries
  FreeLDTEntries: array [0..MAXIMUM_XBOX_THREADS - 1] of UInt16;

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
  EnterCriticalSection({var}EmuLDTLock);

  // Locate a free LDT entry
  begin
    x := -1;
    repeat
      Inc(x);
      Result := FreeLDTEntries[x];
    until (Result <> 0) or (x = (MAXIMUM_XBOX_THREADS - 1));

    if Result = 0 then
    begin
      LeaveCriticalSection({var}EmuLDTLock);

      CxbxKrnlCleanup('Could not locate free LDT entry (too many threads?)');
      Exit;
    end;
  end;

  // Set up selector information
  begin
//    ZeroMemory(@LDTENTRY, SizeOf(LDTENTRY));

    _LDTEntry.BaseLow := Word(dwBaseAddr and $FFFF);
(*
    DWORD   BaseMid : 8;

    DWORD   Type : 5;
    DWORD   Dpl : 2;
    DWORD   Pres : 1;

    DWORD   LimitHi : 4;
    DWORD   Sys : 1;
    DWORD   Reserved_0 : 1;
    DWORD   Default_Big : 1;
    DWORD   Granularity : 1;

    DWORD   BaseHi : 8;
*)

    _LDTEntry.HighWord.Bytes.BaseMid := (dwBaseAddr shr 16) and $FF;
    _LDTEntry.HighWord.Bytes.BaseHi := (dwBaseAddr shr 24) and $FF;
    
    Assert(_LDTEntry.HighWord.Bytes.BaseHi = _LDTEntry.HighWord.Bits.BaseHi);
    Assert(_LDTEntry.HighWord.Bytes.BaseMid = _LDTEntry.HighWord.Bits.BaseMid);

    _LDTEntry.HighWord.Bits.Type_ := $13; // RW data segment
    _LDTEntry.HighWord.Bits.Dpl := 3;    // user segment
    _LDTEntry.HighWord.Bits.Pres := 1;    // present
    _LDTEntry.HighWord.Bits.Sys := 0;
    _LDTEntry.HighWord.Bits.Reserved_0 := 0;
    _LDTEntry.HighWord.Bits.Default_Big := 1;    // 386 segment
    _LDTEntry.HighWord.Bits.Granularity := iif(dwLimit >= $00100000, 1, 0);

    if _LDTEntry.HighWord.Bits.Granularity <> 0 then
      dwLimit := dwLimit shr 12;

    _LDTEntry.HighWord.Bits.LimitHi := (dwLimit shr 16) and $F;

    _LDTEntry.LimitLow := Word(dwLimit and $FFFF);
  end;

  // Allocate selector
  begin
    if not NT_SUCCESS(NtSetLdtEntries(Result, LDTEntry, 0, LDTEntry)) then
    begin
      WriteLog(GetLastErrorString);
      LeaveCriticalSection({var}EmuLDTLock);

      CxbxKrnlCleanup('Could not set LDT entries');

      Result := 0;
      Exit;
    end;
  end;

  FreeLDTEntries[x] := 0;

  LeaveCriticalSection({var}EmuLDTLock);
end;

procedure EmuDeallocateLDT(wSelector: uint16);
var
  LDTEntry: LDT_ENTRY;
begin
  EnterCriticalSection({var}EmuLDTLock);

  ZeroMemory(@LDTEntry, SizeOf(LDTEntry));

  NtSetLdtEntries(wSelector, LDTEntry, 0, LDTEntry);

  FreeLDTEntries[(wSelector shr 3) - 1] := wSelector;

  LeaveCriticalSection({var}EmuLDTLock);
end;

end.
