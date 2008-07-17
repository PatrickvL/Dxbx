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
  Windows,
  SysUtils,
  Classes,
  // Dxbx
  uTypes,
  uBitUtils,
  uXboxLibraryUtils;

function ConvertPatternsToSortedList(const aPatterns: PChar): TList;

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

function PatternListCompare(Pattern1, Pattern2: PXboxLibraryFunction): Integer;
var
  i: Integer;
begin
  for i := 0 to PATTERNSIZE - 1 do
  begin
    Result := Integer(Pattern1.Pattern.Bytes[i]) - Integer(Pattern2.Pattern.Bytes[i]);
    if Result <> 0 then
      Exit;
  end;

  if Result = 0 then
    Result := Pattern1.CRCValue - Pattern2.CRCValue;
end;

function ConvertPatternsToSortedList(const aPatterns: PChar): TList;

  function _ScanHexDigits(var aLine: PChar; var Value: Integer; Digits: Integer): Boolean;
  begin
    Value := 0;
    while Digits > 0 do
    begin
      Result := ScanAndAddHexDigit(Value, aLine^);
      if not Result then
        Exit;

      Inc(aLine);
      Dec(Digits);
    end;
    
    Result := True;
  end;

  function _ScanHexByte(var aLine: PChar; var Value: Integer): Boolean;
  begin
    Result := _ScanHexDigits(aLine, Value, 2);
  end;

  function _ScanHexWord(var aLine: PChar; var Value: Integer): Boolean;
  begin
    Result := _ScanHexDigits(aLine, Value, 4);
  end;

  procedure _ScanPatternLine(aLine: PChar; const aLength: Integer);
  const
    PATTERN_START_OF_NAME = 85;
    PATTERN_VALID_NAME_CHARACTERS = ['_', 'a'..'z', 'A'..'Z', '0'..'9', '@'];
  var
    XboxLibraryFunction: PXboxLibraryFunction;
    c, Value: Integer;
    aStart: PChar;
  begin
    // Lines must at least be 86 characters long :
    if aLength < PATTERN_START_OF_NAME + 1 then
      Exit;

    XboxLibraryFunction := AllocMem(SizeOf(RXboxLibraryFunction));
    ZeroMemory(XboxLibraryFunction, SizeOf(RXboxLibraryFunction));

    for c := 0 to PATTERNSIZE - 1 do
    begin
      if _ScanHexByte({var}aLine, {var}Value) then
      begin
        XboxLibraryFunction.Pattern.Bytes[c] := Byte(Value);
        SetBit(XboxLibraryFunction.Pattern.BytesToUseMask, c);
      end
      else
        Inc(aLine, 2);
    end;

    Assert(aLine^ = ' ');
    Inc(aLine);

    if _ScanHexByte(aLine, Value) then
      XboxLibraryFunction.CRCLength := Byte(Value);
    Assert(aLine^ = ' ');
    Inc(aLine);

    if _ScanHexWord(aLine, Value) then
      XboxLibraryFunction.CRCValue := Word(Value);
    Assert(aLine^ = ' ');
    Inc(aLine);

    if _ScanHexWord(aLine, Value) then
      XboxLibraryFunction.TotalLength := Word(Value);
    Assert(aLine^ = ' ');
    Inc(aLine);

    Assert(aLine^ = ':');
    Inc(aLine);
    if _ScanHexWord(aLine, Value) then
      ; // Ignore offset
    if aLine^ = '@' then
      Inc(aLine);
    Assert(aLine^ = ' ');
    Inc(aLine);

    aStart := aLine;
    repeat
      Inc(aLine);
    until not (aLine^ in PATTERN_VALID_NAME_CHARACTERS);

    SetString(XboxLibraryFunction.Name, aStart, aLine - aStart);

    Result.Add(XboxLibraryFunction);
  end;

var
  p1, p2: PChar;
begin
  Result := TList.Create;

  // Scan Patterns:
  p1 := aPatterns;
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

  Result.Sort(@PatternListCompare);
end;

end.

