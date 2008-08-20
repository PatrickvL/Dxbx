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
  Classes; // MaxListSize

const
  PATTERNSIZE = 32; // A pattern is 32 bytes long

  PatternDontCareValue = Word($FFFF);
  
type
  // This enumerate type contains all Xbox library functions that we patch.
  TXboxLibraryPatch = (
    xlp_Unknown,
    xlp_XapiInitProcess,
    xlp_RtlCreateHeap,
    xlp_XapiApplyKernelPatches,
    xlp_XapiThreadStartup
    );

  PVersionedXboxLibrary = ^RVersionedXboxLibrary;
  RVersionedXboxLibrary = record
    LibVersion: Integer;
    LibName: string;
  end;

  PVersionedXboxLibraryFunction = ^RVersionedXboxLibraryFunction;
  RVersionedXboxLibraryFunction = record
    VersionedXboxLibrary: PVersionedXboxLibrary;
    Name: string;
    CRCLength: Byte;
    CRCValue: Word;
    TotalLength: Word;
  end;
  TCodePointer = type Pointer;

  PDetectedVersionedXboxLibraryFunction = ^RDetectedVersionedXboxLibraryFunction;
  RDetectedVersionedXboxLibraryFunction = record
    VersionedXboxLibrary: PVersionedXboxLibrary;
    FunctionName: string;
    HitCount: Integer;
    CodeStart: TCodePointer;
    CodeEnd: TCodePointer;
  end;

  TLibVersion = Word; // The 4-digit version number of an XDK library

  BaseIndexType = Word; // A Word will suffice while we store less than 65536 strings & functions.

  PByteOffset = ^TByteOffset;
  TByteOffset = type Cardinal; /// Use this everywhere a location in the Trie's persistent storage is needed.

  TStringTableIndex = type BaseIndexType; /// Use this everywhere a string is uniquely identified.

  {$A1} // Make sure all the following records are byte-aligned for best space-usage :

  RStoredStringTable = record
    NrStrings: BaseIndexType; /// The number of strings in this table (no duplicates, sorted for faster searching)
    StringOffsets: TByteOffset; /// The start of the string table (each string is one TByteOffset into the AnsiCharData)
    AnsiCharData: TByteOffset; /// The start of the actual string data (AnsiChars, no separators)
  end;

  PStringOffsetList = ^TStringOffsetList;
  TStringOffsetList = array[0..MaxListSize-1] of TByteOffset;

  TLibraryIndex = type Byte; /// Use this everywhere a library is uniquely identified.

  RStoredLibrary = record
    LibVersion: TLibVersion; /// The version number of this library
    LibNameIndex: TStringTableIndex; // The name of this library, in the form of an index into the StringTable.
  end;

  RStoredLibraryTable = record
    NrOfLibraries: Word; /// The number of libraries in this table
    LibrariesOffset: TByteOffset; /// The location of the first stored library
  end;

{$IFDEF _DEBUG_TRACE}
  // A function occurs in two locations - per library and global.
  /// This record contains the global function information.
  PStoredGlobalFunction = ^RStoredGlobalFunction;
  RStoredGlobalFunction = record
    FunctionNameIndex: TStringTableIndex; /// This record only has the index to the name of this function.
  end;

  PGlobalFunctionList = ^TGlobalFunctionList;
  TGlobalFunctionList = array[0..MaxListSize-1] of RStoredGlobalFunction;
{$ENDIF}

  // Global functions can be indicated using a number in the range [0..NrOfFunctions-1].
  // These unique global function numbers can be put in a to-be generated code unit,
  // so we can refer to them by number, instead of name. This saves quite some space
  // in release-builds, because with this method we won't even need to store the
  // function-names in the file anymore. (All this is yet to-be-done/TODO for now.)
  RStoredGlobalFunctionTable = record
    NrOfFunctions: BaseIndexType; /// The number of global functions in this table
{$IFDEF _DEBUG_TRACE}
    GlobalFunctionsOffset: TByteOffset; /// The location of the first stored function
{$ENDIF}
  end;

  TFunctionIndex = type BaseIndexType; /// Use this everywhere a function is uniquely identified.

  // A function occurs in two locations - per library and global.
  // This record contains the per-library function information.
  PStoredLibraryFunction = ^RStoredLibraryFunction;
  RStoredLibraryFunction = record
    GlobalFunctionIndex: TFunctionIndex; /// The unique index of this function in the RStoredGlobalFunctionTable
    LibraryIndex: TLibraryIndex; /// The unique index of the libray containing this function
    CRCLength: Byte;
    CRCValue: Word;
    FunctionLength: Word;
  end;

  PStoredTrieNode = ^RStoredTrieNode;
  RStoredTrieNode = record
    NextSiblingOffset: TByteOffset;
    NrChildrenByte1: Byte;
    // The next byte is optional, only used when the actual number of
    // children is larger than 128 (see TPatternTrie.Save._WriteTrieNodes):
    NrChildrenByte2: Byte;
    // The rest of this record cannot be defined as a static type,
    // but has this layout :
    //
    // For every stretch of fixed bytes, a header byte is given first.
    //
    // The bits of this header byte are defined as, "nnnnnctt", where:
    //
    // nnnnn : 5 bits indicating the number [0..31] of fixed bytes that follow
    // c : 1 continue bit - if set more stretches follow after this one
    // tt : 2 bits, indicating the Node Type Flags (see below)
    //
    // The specified number of fixed bytes follow directly after this.
    //
    // The "don't care" bytes are not stored in the output,
    // but the amount of them is indicated by the Node Type Flags.
  end;

  PStoredSignatureTrieHeader = ^RStoredSignatureTrieHeader;
  RStoredSignatureTrieHeader = record
    Header: array[0..5] of AnsiChar; // Chosen so this record becomes a nice 32 bytes large
    StringTable: RStoredStringTable;
    LibraryTable: RStoredLibraryTable;
    GlobalFunctionTable: RStoredGlobalFunctionTable;
    TrieRootNode: TByteOffset; /// The location of the root of the Trie, this location contains a RStoredTrieNode
  end;

const
  // Node Type Flags :
  NODE_5BITFIXED_0WILDCARDS = 0;
  NODE_5BITFIXED_4WILDCARDS = 1;
  NODE_5BITFIXED_8WILDCARDS = 2;
  NODE_5BITFIXED_ALLWILDCARDS = 3; // No followup-bit normally, which enables:

  // The "ALLFIXED" flag is used to indicate a full stretch of 32 fixed values,
  // which couldn't otherwise be specified with the 5 bits reserved for that :
  NODE_ALLFIXED = 7;

// This method creates a somewhat readable string for each patched method.
function XboxLibraryPatchToString(const aValue: TXboxLibraryPatch): string;

// This method returns the function-name for a (to be) patched method (indicated by aValue).
function XboxLibraryPatchToFunctionName(const aValue: TXboxLibraryPatch): string;
// This method determines which patch corresponds with the supplied aFunctionName.
function XboxFunctionNameToLibraryPatch(const aFunctionName: string): TXboxLibraryPatch;

function IsXboxLibraryPatch(const aFunctionName: string): Boolean;

implementation

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

const
  PatchNames: array [TXboxLibraryPatch] of string = (
    {xlp_Unknown=}'UNKNOWN',
    {xlp_XapiInitProcess=}'_XapiInitProcess@0',
    {xlp_RtlCreateHeap=}'_RtlCreateHeap@24',
    {xlp_XapiApplyKernelPatches=}'_XapiApplyKernelPatches@0',
    {xlp_XapiThreadStartup=}'_XapiThreadStartup@8'
  );

function XboxLibraryPatchToFunctionName(const aValue: TXboxLibraryPatch): string;
begin
  Result := PatchNames[aValue];
  Assert(Result <> '');
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

