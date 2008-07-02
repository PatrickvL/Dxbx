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
unit uPatternScanner;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Classes,
  Windows,
  // Dxbx
  uBitUtils;

const
  PATTERNSIZE = 32; // A pattern is 32 bytes long

type
  UInt32 = Cardinal;

  PPattern = ^RPattern;
  RPattern = record
    PatternBytesToUseMask: UInt32; // These 32 bits indicate which corresponding 32 bytes are used in the pattern
    PatternBytes: array [0..PATTERNSIZE - 1] of Byte;
    Name: string;
    // TODO : Add CRC and other stuff here too!
  end;

function GetSortedPatternList(const aPatternName: string): TList;

function ScanAndAddHexDigit(var Value: Integer; const aHexDigit: Char): Boolean;

implementation

function ScanAndAddHexDigit(var Value: Integer; const aHexDigit: Char): Boolean;
begin
  Result := True;
  case aHexDigit of
    '0'..'9':
      Value := (Value * 16) + (Ord(aHexDigit) - Ord('0'));
    'A'..'F':
      Value := (Value * 16) + (Ord(aHexDigit) - Ord('A') + 10);
    'a'..'f':
      Value := (Value * 16) + (Ord(aHexDigit) - Ord('a') + 10);
  else
    Result := False;
  end;
end;

function GetSortedPatternList(const aPatternName: string): TList;

  procedure _ScanPatternLine(const aLine: PChar; const aLength: Integer);
  const
    PATTERN_START_OF_NAME = 85;
    PATTERN_VALID_NAME_CHARACTERS = ['_', 'a'..'z', 'A'..'Z', '0'..'9', '@'];
  var
    Pattern: PPattern;
    c, Value: Integer;
    C1, C2: Char;
  begin
    // Lines must at least be 86 characters long :
    if aLength < PATTERN_START_OF_NAME + 1 then
      Exit;

    Pattern := AllocMem(SizeOf(RPattern));
    ZeroMemory(Pattern, SizeOf(RPattern));

    for c := 0 to PATTERNSIZE - 1 do
    begin
      C1 := aLine[(2*c) + 0];
      C2 := aLine[(2*c) + 1];

      Value := 0;
      if  ScanAndAddHexDigit({var}Value, C1)
      and ScanAndAddHexDigit({var}Value, C2) then
      begin
        Pattern.PatternBytes[c] := Byte(Value);
        SetBit(Pattern.PatternBytesToUseMask, c);
      end;
    end;

    c := PATTERN_START_OF_NAME;
    while aLine[c] in PATTERN_VALID_NAME_CHARACTERS do
      Inc(c);

    SetString(Pattern.Name, aLine + PATTERN_START_OF_NAME, c - PATTERN_START_OF_NAME);

    Result.Add(Pattern);
  end;

var
  p1, p2: PChar;
begin
  Result := TList.Create;

  // Get Pattern data from resource:
  with TResourceStream.Create(LibModuleList.ResInstance, aPatternName, RT_RCDATA) do
  try
    // Scan Patterns:
    p1 := Memory;
    while p1^ <> #0 do
    begin
      // Stop at a '-' marker :
      if p1^ = '-' then
        Break;

      // Scan this line until end of line (#0..#13) :
      p2 := p1;
      while p2^ > #13 do
        Inc(p2);

      // Handle this line :
      _ScanPatternLine(p1, p2-p1);

      // Step over to the start of the next line :
      p1 := p2;
      while p1^ in [#10, #13] do
        Inc(p1);
    end;

    // TODO : Sort the patterns

  finally
    // Unlock the resources :
    Free;
  end;
end;

end.

