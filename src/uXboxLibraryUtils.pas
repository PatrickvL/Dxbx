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
  // Dxbx
  uTypes;

const
  PATTERNSIZE = 32; // A pattern is 32 bytes long

type
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
    HitCount: Integer; // TODO : Include this field only when compiling in DxbxKrnl
  end;

  TPatternArray = array of RXboxLibraryFunction;

  RXboxLibraryInfo = record
    Name: string;
    Version: string;
    NrPatterns: Integer;
    PatternArray: Pointer; // TPatternArray?
  end;

  // This enumerate type contains all Xbox library functions that we patch.
  TXboxLibraryPatch = (
    xlp_XapiInitProcess
    );

// This method creates a somewhat readable string for each patched method.
function XboxLibraryPatchToString(const aValue: TXboxLibraryPatch): string;

// This method returns the pattern-name for each patched method.
function XboxLibraryPatchToPatternName(const aValue: TXboxLibraryPatch): string;

implementation

function XboxLibraryPatchToString(const aValue: TXboxLibraryPatch): string;
var
  i: Integer;
begin
  // Start out with the official pattern-name :
  Result := XboxLibraryPatchToPatternName(aValue);
  // Now remove all prefix non-letters :
  while (Result <> '') and (not (Result[1] in ['a'..'z','A'..'Z'])) do
    Delete(Result, 1, 1);
  // And remove everything from '@' onward :
  i := Pos('@', Result);
  if i > 0 then
    Delete(Result, i, MaxInt);
end;

function XboxLibraryPatchToPatternName(const aValue: TXboxLibraryPatch): string;
begin
  case aValue of
    xlp_XapiInitProcess:
       Result := '_XapiInitProcess@0';
  else
    Result := '';
    Assert(False);
  end;
end;

end.

