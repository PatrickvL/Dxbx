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
    CRCLength: Byte;
    CRCValue: Word;
    TotalLength: Word;
    // TODO : Add referenced API's and trailing bytes here too!
HitCount: Integer;
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

function PatternListCompare(Pattern1, Pattern2: PPattern): Integer;
var
  i: Integer;
begin
  for i := 0 to PATTERNSIZE - 1 do
  begin
    Result := Integer(Pattern1.PatternBytes[i]) - Integer(Pattern2.PatternBytes[i]);
    if Result <> 0 then
      Exit;
  end;

  if Result = 0 then
    Result := Pattern1.CRCValue - Pattern2.CRCValue;
end;

function GetSortedPatternList(const aPatternName: string): TList;

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
    Pattern: PPattern;
    c, Value: Integer;
    aStart: PChar;
  begin
    // Lines must at least be 86 characters long :
    if aLength < PATTERN_START_OF_NAME + 1 then
      Exit;

    Pattern := AllocMem(SizeOf(RPattern));
    ZeroMemory(Pattern, SizeOf(RPattern));

    for c := 0 to PATTERNSIZE - 1 do
    begin
      if _ScanHexByte({var}aLine, {var}Value) then
      begin
        Pattern.PatternBytes[c] := Byte(Value);
        SetBit(Pattern.PatternBytesToUseMask, c);
      end
      else
        Inc(aLine, 2);
    end;

    Assert(aLine^ = ' ');
    Inc(aLine);

    if _ScanHexByte(aLine, Value) then
      Pattern.CRCLength := Byte(Value);
    Assert(aLine^ = ' ');
    Inc(aLine);

    if _ScanHexWord(aLine, Value) then
      Pattern.CRCValue := Word(Value);
    Assert(aLine^ = ' ');
    Inc(aLine);

    if _ScanHexWord(aLine, Value) then
      Pattern.TotalLength := Word(Value);
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

    SetString(Pattern.Name, aStart, aLine - aStart);

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

    Result.Sort(@PatternListCompare);

  finally
    // Unlock the resources :
    Free;
  end;
end;

end.

