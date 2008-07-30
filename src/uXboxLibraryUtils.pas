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
unit uXboxLibraryUtils;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
  // Dxbx
  uTypes;

const
  PATTERNSIZE = 32; // A pattern is 32 bytes long

type
  // This enumerate type contains all Xbox library functions that we patch.
  TXboxLibraryPatch = (
    xlp_Unknown,
    xlp_XapiInitProcess,
    xlp_RtlCreateHeap
    );

  PPattern32 = ^RPattern32;
  // This record contains one span of 32 function bytes,
  // including a bitmask indicating which bytes are static.
  // (Non-static bytes must be ignored in detection routines).
  RPattern32 = record
    BytesToUseMask: UInt32; // These 32 bits indicate which corresponding 32 bytes are used in the pattern
    Bytes: array [0..PATTERNSIZE - 1] of Byte;
  end;

  PXboxLibraryFunction = ^RXboxLibraryFunction;
  // This record contains everything we need to detect an Xbox library function.
  RXboxLibraryFunction = record
    Name: string;
    Pattern: RPattern32;
    CRCLength: Byte;
    CRCValue: Word;
    TotalLength: Word;
    // TODO : Add referenced API's and trailing bytes here too!
  end;

  TCodePointer = type Pointer;

  PDetectedXboxLibraryFunction = ^RDetectedXboxLibraryFunction;
  RDetectedXboxLibraryFunction = record
    Info: PXboxLibraryFunction;
    HitCount: Integer;
    CodeStart: TCodePointer;
    CodeEnd: TCodePointer;
  end;

  TPatternArray = array of RXboxLibraryFunction;
  TSortedPatterns = array of PXboxLibraryFunction;

  PXboxLibraryInfo = ^RXboxLibraryInfo;
  RXboxLibraryInfo = record
    LibVersion: Integer;
    LibName: string;
    PatternArray: TPatternArray;
    SortedPatterns: TSortedPatterns
  end;

// This method creates a somewhat readable string for each patched method.
function XboxLibraryPatchToString(const aValue: TXboxLibraryPatch): string;

// This method returns the function-name for a (to be) patched method (indicated by aValue).
function XboxLibraryPatchToFunctionName(const aValue: TXboxLibraryPatch): string;
// This method determines which patch corresponds with the supplied aFunctionName.
function XboxFunctionNameToLibraryPatch(const aFunctionName: string): TXboxLibraryPatch;

function IsXboxLibraryPatch(const aFunctionName: string): Boolean;

function PatternList_NameCompare(Pattern1, Pattern2: PXboxLibraryFunction): Integer;
function PatternList_PatternCompare(Pattern1, Pattern2: PXboxLibraryFunction): Integer;

implementation

function PatternList_PatternCompare(Pattern1, Pattern2: PXboxLibraryFunction): Integer;
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

function PatternList_NameCompare(Pattern1, Pattern2: PXboxLibraryFunction): Integer;
begin
  if not Assigned(Pattern2) then
  begin
    Result := 1;
    Exit;
  end;

  Result := CompareStr(Pattern1.Name, Pattern2.Name);
  if Result = 0 then
    Result := PatternList_PatternCompare(Pattern1, Pattern2);
end;

function XboxLibraryPatchToString(const aValue: TXboxLibraryPatch): string;
var
  i: Integer;
begin
  // Start out with the official pattern-name :
  Result := XboxLibraryPatchToFunctionName(aValue);
  // Now remove all prefix non-letters :
  while (Result <> '') and (not (Result[1] in ['a'..'z','A'..'Z'])) do
    Delete(Result, 1, 1);
  // And remove everything from '@' onward :
  i := Pos('@', Result);
  if i > 0 then
    Delete(Result, i, MaxInt);
end;

function XboxLibraryPatchToFunctionName(const aValue: TXboxLibraryPatch): string;
begin
  case aValue of
    xlp_Unknown:
      Result := 'UNKNOWN';
    xlp_XapiInitProcess:
      Result := '_XapiInitProcess@0';
    xlp_RtlCreateHeap:
      Result := '_RtlCreateHeap@24';
  else
    Result := '';
    Assert(False);
  end;
end;

function XboxFunctionNameToLibraryPatch(const aFunctionName: string): TXboxLibraryPatch;
begin
  Result := High(TXboxLibraryPatch);
  repeat
    if XboxLibraryPatchToFunctionName(Result) = aFunctionName then
      Exit;

    Dec(Result);
  until (Result = xlp_Unknown);
end;

function IsXboxLibraryPatch(const aFunctionName: string): Boolean;
begin
  Result := XboxFunctionNameToLibraryPatch(aFunctionName) <> xlp_Unknown;
end;

end.

