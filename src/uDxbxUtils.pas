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
unit uDxbxUtils;

interface

uses
  // Delphi
  Windows,
  SysUtils,
  Classes,
  // Dxbx
  uTypes;

const
  NUMBER_OF_THUNKS = 379;

type
  EMU_STATE = (esNone, esFileOpen, esRunning);

  TDebugInfoType = (ditConsole, ditFile);
  EnumAutoConvert = (CONVERT_TO_MANUAL, CONVERT_TO_XBEPATH, CONVERT_TO_WINDOWSTEMP);
  DebugMode = (DM_NONE, DM_CONSOLE, DM_FILE);

  TLogType = (ltKernel, ltGui);

  TEntryProc = procedure();
  PEntryProc = ^TEntryProc;

  TSetXbePath = procedure(const Path: PChar); cdecl;

  TKernelThunkTable = packed array[0..NUMBER_OF_THUNKS - 1] of IntPtr;
  PKernelThunkTable = ^TKernelThunkTable;

  TGetKernelThunkTable = function: PKernelThunkTable; cdecl;

  TLineCallback = function (aLinePtr: PAnsiChar; aLength: Integer; aData: Pointer): Boolean;

procedure ScanPCharLines(const aPChar: PAnsiChar; const aLineCallback: TLineCallback; const aCallbackData: Pointer);

function ScanHexByte(var aLine: PAnsiChar; var Value: Integer): Boolean;
function ScanHexWord(var aLine: PAnsiChar; var Value: Integer): Boolean;

function FindFiles(const aFolder, aFileMask: TFileName; aFileNames: TStrings): Integer;

function StartsWithText(const aString, aPrefix: string): Boolean;

procedure Swap(var aElement1, aElement2); overload;
function RoundUp(dwValue, dwMult: DWord): DWord;

function FixInvalidFilePath(const aFilePath: string; const aReplacement: string = '_'): string;

function DebugModeToString(const aDebugMode: DebugMode): string;

function LogTypeToString(const aLogType: TLogType): string;

function PointerToString(const aPointer: Pointer): string;

implementation

function FixInvalidFilePath(const aFilePath: string; const aReplacement: string = '_'): string;
const
  InvalidNTFSFilePathChars: set of Char = [#0..#31] + ['/', '\', ':', '*', '?', '"', '<', '>', '|', #127];
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(aFilePath) do
  begin
    if aFilePath[i] in InvalidNTFSFilePathChars then
      Result := Result + aReplacement
    else
      Result := Result + aFilePath[i];
  end;
end;

procedure Swap(var aElement1, aElement2);
var
  Tmp: Pointer;
begin
  Tmp := Pointer(aElement1);
  Pointer(aElement1) := Pointer(aElement2);
  Pointer(aElement2) := Tmp;
end;

function RoundUp(dwValue, dwMult: DWord): DWord;
begin
  if dwMult = 0 then
    Result := dwValue
  else
    Result := dwValue - ((dwValue - 1) mod dwMult) + (dwMult - 1);
end;

function StartsWithText(const aString, aPrefix: string): Boolean;
begin
  Result := AnsiStrLIComp(PChar(aString), PChar(aPrefix), Length(aPrefix)) = 0;
end;

function FindFiles(const aFolder, aFileMask: TFileName; aFileNames: TStrings): Integer;
var
  Status: Integer;
  SearchRec: TSearchRec;
begin
  with aFileNames do
  begin
    BeginUpdate;
    try
      Clear;
      Status := FindFirst(IncludeTrailingPathDelimiter(aFolder) + aFileMask, faAnyFile, SearchRec);
      while Status = 0 do
      begin
        if (SearchRec.Attr and faDirectory) = 0 then
          Add(IncludeTrailingPathDelimiter(aFolder) + SearchRec.Name);

        Status := FindNext(SearchRec);
      end;

      FindClose(SearchRec);
    finally
      EndUpdate;
    end;

    Result := Count;
  end;
end;

function _ScanAndAddHexDigit(var Value: Integer; const aHexDigit: AnsiChar): Boolean;
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

function _ScanHexDigits(var aLine: PAnsiChar; var Value: Integer; Digits: Integer): Boolean;
begin
  Value := 0;
  while Digits > 0 do
  begin
    Result := _ScanAndAddHexDigit(Value, aLine^);
    if not Result then
      Exit;

    Inc(aLine);
    Dec(Digits);
  end;

  Result := True;
end;

function ScanHexByte(var aLine: PAnsiChar; var Value: Integer): Boolean;
begin
  Result := _ScanHexDigits(aLine, Value, 2);
end;

function ScanHexWord(var aLine: PAnsiChar; var Value: Integer): Boolean;
begin
  Result := _ScanHexDigits(aLine, Value, 4);
end;

procedure ScanPCharLines(const aPChar: PAnsiChar; const aLineCallback: TLineCallback; const aCallbackData: Pointer);
var
  p1, p2: PAnsiChar;
begin
  // Scan Lines:
  p1 := aPChar;
  while p1^ > #0 do
  begin
    // Scan this line until end of line (#0..#13) :
    p2 := p1;
    while p2^ > #13 do
      Inc(p2);

    // Handle this line :
    if not aLineCallback(p1, {Length=}p2-p1, aCallbackData) then
      Exit;

    // Step over to the start of the next line :
    p1 := p2;
    while p1^ in [#10, #13] do
      Inc(p1);
  end;
end;

function PointerToString(const aPointer: Pointer): string;
begin
  Result := IntToHex(Integer(aPointer), 8);
end;

function DebugModeToString(const aDebugMode: DebugMode): string;
begin
  case aDebugMode of
    DM_NONE: Result := 'DM_NONE';
    DM_CONSOLE: Result := 'DM_CONSOLE';
    DM_FILE: Result := 'DM_FILE';
  else
    Result := '?Unknown?';
  end;
end;

function LogTypeToString(const aLogType: TLogType): string;
begin
  case aLogType of
    ltKernel: Result := 'ltKernel';
    ltGui: Result := 'ltGui';
  else
    Result := '?Unknown?';
  end;
end;

end.

