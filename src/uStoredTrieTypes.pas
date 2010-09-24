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
unit uStoredTrieTypes;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
  Classes, // MaxListSize
  // Dxbx
  uTypes,
  uDxbxUtils;

const
  PATTERNSIZE = 32; // A pattern is 32 bytes long

  PatternDontCareValue = Word($FFFF);

type
  {$A1} // Make sure all the following records are byte-aligned for best space-usage :

{$IFDEF DXBX_RECTYPE}
  TRecType = (
    rtUnknown
    , rtStoredLibrary
    , rtStoredStringHeader
    , rtStoredLibraryFunction
    , rtStoredSymbolReference
    , rtStoredTrieNode
//    , rtStoredGlobal
//    , rtStoredLeafNode
    );
{$ENDIF}

  TLibVersion = Word; // The 4-digit version number of an XDK library

  // Here follow all 32 lib versions we know of, usable as a flag :
  // Note : In the following enumeration, 5849.16 is left out to stay within 32 bits
  // but also because that version cannot be identified seperatly from 5849.
  TLibraryVersionFlag = (
    lib3424, lib3911, lib3925, lib3936, lib3937, lib3941, lib3944, lib3948,
    lib3950, lib4034, lib4039, lib4134, lib4242, lib4361, lib4400, lib4432,
    lib4531, lib4627, lib4721, lib4831, lib4920, lib4928, lib5028, lib5120,
    lib5233, lib5344, lib5455, lib5558, lib5659, lib5788, lib5849, lib5933);

  TLibraryVersionFlags = set of TLibraryVersionFlag;

  BaseIndexType = Cardinal; // A Word suffices for less than 65536 strings & functions, use Cardinal for larger sets

  PByteOffset = ^TByteOffset;
  TByteOffset = type Cardinal; /// Use this everywhere a location in the Trie's persistent storage is needed.

  TStringTableIndex = type BaseIndexType; /// Use this everywhere a string is uniquely identified.

  TLeafID = type BaseIndexType; /// Use this everywhere a leaf is uniquely identified.

  TFunctionIndex = type BaseIndexType; /// Use this everywhere a function is uniquely identified.
  PFunctionIndex = ^TFunctionIndex;

  // This header is put right before a string and indicates which (if any) functions are associated with it.
  PStoredStringHeader = ^RStoredStringHeader;
  RStoredStringHeader = packed record
{$IFDEF DXBX_RECTYPE}
    RecType: TRecType;
{$ENDIF}
    NrOfFunctions: Word; // Note : Even if a string has zero functions, the FirstFunctionIndex will be set (to Prev+Count)
    FirstFunctionIndex: TFunctionIndex; // Using a binary search over the strings, the accompanying functions can be found,
    // but also vice-versa: Given a function index, the string that 'associates' this function can be binary-searched too!
  end;

  PIndexedStringOffsets = ^TIndexedStringOffsets;
  TIndexedStringOffsets = array [0..MaxListSize-1] of TByteOffset;

  TLibraryIndex = type Byte; /// Use this everywhere a library is uniquely identified.

  PStoredLibrary = ^RStoredLibrary;
  RStoredLibrary = packed record
{$IFDEF DXBX_RECTYPE}
    RecType: TRecType;
{$ENDIF}
    LibVersion: TLibVersion; /// The version number of this library
    LibNameIndex: TStringTableIndex; // The name of this library, in the form of an index into the StringTable.
  end;

  PStoredLibrariesList = ^TStoredLibrariesList;
  TStoredLibrariesList = array [0..(MaxInt div SizeOf(RStoredLibrary))-1] of RStoredLibrary;
  TSymbolReferenceIndex = type BaseIndexType;

  // All functions that reference other symbols, use this record.
  PStoredSymbolReference = ^RStoredSymbolReference;
  RStoredSymbolReference = packed record
{$IFDEF DXBX_RECTYPE}
    RecType: TRecType;
{$ENDIF}
    Offset: Word; /// Offset from start of parent function
    BaseOffset: SmallInt; /// Offset to apply to the referenced address to get to the base of the referenced symbol (only applicable to data symbols)
    NameIndex: TStringTableIndex; /// Name of the reference (by index)
    ReferenceFlags: Word; /// Indications about the type & usage of this reference
    // TODO : Split up the Offsets, SymbolNames and BaseOffset, to increase re-use
  end;

const
  // ReferenceFlags
  rfIsAbsolute       = $0001; // If set, reference is an absolute address
  rfIsRelative       = $0002; // If set, is a relative address
  rfIsSectionRel     = $0004; // If set, is a 'section-relative' address (Which means what!?)
  rfIsDuplicate      = $0008; // If set, symbol is referenced multiple times
  rfIsDataReference  = $0010; // If set, symmbol is data - TODO : Realize this

type
  PStoredSymbolReferencesList = ^TStoredSymbolReferencesList;
  TStoredSymbolReferencesList = array [0..(MaxInt div SizeOf(RStoredSymbolReference))-1] of RStoredSymbolReference;

  // This record contains the function information (shared by at most 32 libraries).
  PStoredLibraryFunction = ^RStoredLibraryFunction;
  RStoredLibraryFunction = packed record
{$IFDEF DXBX_RECTYPE}
    RecType: TRecType;
{$ENDIF}
    PatternLeafNodeOffset: TByteOffset; /// Use this to backtrace the pattern associated with this function
    // ParentLeafNodeID: TLeafID;
    FunctionLength: Word;
    CRCValue: Word;
    CRCLength: Byte; /// TODO : This could be removed once the CRC covers the whole function (except its references)
    NrOfSymbolReferences: Byte; // A byte is enough for 255 symbol references, no function will use that much!
    FirstSymbolReference: TSymbolReferenceIndex; /// The index of the first reference (the others are right next to it)
    AvailableInVersions: TLibraryVersionFlags; /// Indicates which XDK versions this exact function occurs in
    LibraryNameIndex: TStringTableIndex; /// Just the name of the library this function originates from
  end;

  PStoredLibraryFunctionsList = ^TStoredLibraryFunctionsList;
  TStoredLibraryFunctionsList = array [0..(MaxInt div SizeOf(RStoredLibraryFunction))-1] of RStoredLibraryFunction;

//  RStoredGlobal = packed record
//{$IFDEF DXBX_RECTYPE}
//    RecType: TRecType;
//{$ENDIF}
//    DataLength: Word;
//    GlobalNameIndex: TStringTableIndex; /// Name of the global (by index)
//  end;

//  PStoredLeafNode = ^RStoredLeafNode;
//  RStoredLeafNode = packed record
//{$IFDEF DXBX_RECTYPE}
//    RecType: TRecType;
//{$ENDIF}
//  end;

  PStoredTrieNode = ^RStoredTrieNode;
  RStoredTrieNode = packed record
{$IFDEF DXBX_RECTYPE}
    RecType: TRecType;
{$ENDIF}
    //ParentNodeOffset: TByteOffset; // so we can backtrace the pattern for one single function
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
    // c : 1 continue bit - if set, more stretches follow after this one
    // tt : 2 bits, indicating the Node Type Flags (see below)
    //
    // The specified number of fixed bytes follow directly after this.
    //
    // The "don't care" bytes are not stored in the output,
    // but the amount of them is indicated by the Node Type Flags.
    //
    // Directly following a leaf node, we have 'NrChildren' occurrances
    // of FunctionIndexes into the stored FunctionTable array (which
    // are ordered alphabetically, so we can find (and scan) specific
    // functions by name).
  end;

  TStretchHeaderByte = type Byte;

  PStoredSignatureTrieHeader = ^RStoredSignatureTrieHeader;
  RStoredSignatureTrieHeader = packed record
    Header: array [0..7] of AnsiChar; // 'DxbxTrie'

    StringTable: packed record
      NrOfStrings: BaseIndexType; /// The number of strings in this table (no duplicates, sorted for faster searching)
      IndexedStringOffsets: TByteOffset; /// The start of the string table (each string is one TByteOffset into the StringsOffset)
      StringsOffset: TByteOffset; /// The start of the string data (header + AnsiChars, no separators)
    end;

    LibraryTable: packed record
      NrOfLibraries: Word; /// The number of libraries in this table
      LibrariesOffset: TByteOffset; /// The location of the first library
    end;

//    LeafNodeTable: packed record // TODO : Populate & use this!
//      NrOfLeafs: BaseIndexType;
//      LeadNodesOffset: TByteOffset;
//    end;

    // Global functions can be indicated using a number in the range [0..NrOfFunctions-1].
    // These unique global function numbers can be put in a to-be generated code unit,
    // so we can refer to them by number, instead of name. This saves quite some space
    // in release-builds, because with this method we won't even need to store the
    // function-names in the file anymore. (All this is yet to-be-done/TODO for now.)

    FunctionTable: packed record
      NrOfFunctions: BaseIndexType; /// The number of functions in this table (sorted on name)
      FunctionsOffset: TByteOffset; /// The location of the first function (RStoredLibraryFunction)
    end;

    ReferencesTable: packed record
      NrOfReferences: BaseIndexType; /// The number of references in this table
      ReferencesOffset: TByteOffset; /// The location of the first reference (RStoredSymbolReference)
    end;

//    GlobalsTable: packed record
//      NrOfGlobals: BaseIndexType; /// The number of globals in this table
//      GlobalsOffset: TByteOffset; /// The location of the first global (RStoredGlobal)
//    end;

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

  NODE_TYPE_MASK = 3; // bit 0 and 1 (see above for meaning of possible values 0, 1, 2 and 3)
  NODE_TYPE_MASK_EXTENDED = 7; //

  NODE_FLAG_MORE = (1 shl 2); // bit 2 (indicates another stretch, unless combined with NODE_5BITFIXED_ALLWILDCARDS)
  NODE_NR_FIXED_SHIFT = 3; // Shift bit 0,1 and 2 away to get to the remaining 5 bits for NrFixed

type
  TPatternTrieReader = class(TObject)
  public
    StoredSignatureTrieHeader: PStoredSignatureTrieHeader;
    IndexedStringOffsets: PIndexedStringOffsets;
    StringTableStartPtr: Pointer;
    StoredLibrariesList: PStoredLibrariesList;
    StoredLibraryFunctionsList: PStoredLibraryFunctionsList;
    StoredSymbolReferencesList: PStoredSymbolReferencesList;
    RootNode: PStoredTrieNode;
    function GetByteOffset(const aOffset: TByteOffset): PByteOffset;
  public
    procedure LoadFromStream(const aStream: TStream);

    function NrOfStrings: Cardinal;
    function NrOfFunctions: Cardinal;
    function NrOfReferences: Cardinal;
    function GetStringHeader(const aStringIndex: TStringTableIndex): PStoredStringHeader;
    function GetString(const aStringIndex: TStringTableIndex): string;
    function GetStoredLibrary(const aStoredLibraryIndex: TLibraryIndex): PStoredLibrary;
    function GetStoredLibraryFunction(const aStoredFunctionIndex: TFunctionIndex): PStoredLibraryFunction;
    function GetSymbolReference(const aStoredSymbolReferencesIndex: TSymbolReferenceIndex): PStoredSymbolReference;
    //function GetGlobalSymbol(const aStoredGlobalIndex: TSymbolReferenceIndex): PStoredSymbolReference;
    function GetFunctionName(const aStoredLibraryFunctionIndex: TFunctionIndex): string;
    //function GetLibraryName(const aLibraryIndex: TLibraryIndex): string;
    function GetNode(const aNodeOffset: TByteOffset): PStoredTrieNode;
  end;

function LibraryVersionNumberToFlag(const aLibraryVersionNumber: Integer): TLibraryVersionFlag;
function GetNodeNrChildren(const aStoredTrieNode: PStoredTrieNode; out StretchPtr: PByte): Integer;

var
  LibraryVersionFlagToInt: array [TLibraryVersionFlag] of Integer = (
    3424, 3911, 3925, 3936, 3937, 3941, 3944, 3948,
    3950, 4034, 4039, 4134, 4242, 4361, 4400, 4432,
    4531, 4627, 4721, 4831, 4920, 4928, 5028, 5120,
    5233, 5344, 5455, 5558, 5659, 5788, 5849, 5933
  );

implementation

function LibraryVersionNumberToFlag(const aLibraryVersionNumber: Integer): TLibraryVersionFlag;
begin
  Result := Low(TLibraryVersionFlag);
  while aLibraryVersionNumber > LibraryVersionFlagToInt[Result] do
    Inc(Result);
end;

{ TPatternTrieReader }

procedure TPatternTrieReader.LoadFromStream(const aStream: TStream);
begin
  if aStream is TCustomMemoryStream then
    StoredSignatureTrieHeader := TCustomMemoryStream(aStream).Memory
  else
    Assert(False, 'Stream class not handled yet!'); // TODO

  IndexedStringOffsets := PIndexedStringOffsets(GetByteOffset(StoredSignatureTrieHeader.StringTable.IndexedStringOffsets));
  StringTableStartPtr := GetByteOffset(StoredSignatureTrieHeader.StringTable.StringsOffset);
  StoredLibrariesList := PStoredLibrariesList(GetByteOffset(StoredSignatureTrieHeader.LibraryTable.LibrariesOffset));
  StoredLibraryFunctionsList := PStoredLibraryFunctionsList(GetByteOffset(StoredSignatureTrieHeader.FunctionTable.FunctionsOffset));
  StoredSymbolReferencesList := PStoredSymbolReferencesList(GetByteOffset(StoredSignatureTrieHeader.ReferencesTable.ReferencesOffset));
  RootNode := GetNode(StoredSignatureTrieHeader.TrieRootNode);
end;

function TPatternTrieReader.NrOfStrings: Cardinal;
begin
  Result := StoredSignatureTrieHeader.StringTable.NrOfStrings;
end;

function TPatternTrieReader.NrOfFunctions: Cardinal;
begin
  Result := StoredSignatureTrieHeader.FunctionTable.NrOfFunctions;
end;

function TPatternTrieReader.NrOfReferences: Cardinal;
begin
  Result := StoredSignatureTrieHeader.ReferencesTable.NrOfReferences;
end;

function TPatternTrieReader.GetByteOffset(const aOffset: TByteOffset): PByteOffset;
begin
  UIntPtr(Result) := UIntPtr(StoredSignatureTrieHeader) + aOffset;
end;

function TPatternTrieReader.GetStringHeader(const aStringIndex: TStringTableIndex): PStoredStringHeader;
begin
{$IFDEF DEBUG}
  if {(aStringIndex < 0) or} (aStringIndex >= NrOfStrings) then
    raise EListError.CreateFmt('String index out of bounds (%d)', [aStringIndex]);
{$ENDIF}

  if aStringIndex = 0 then
    Result := StringTableStartPtr
  else
    Result := PStoredStringHeader(GetByteOffset(IndexedStringOffsets[aStringIndex - 1]));
{$IFDEF DXBX_RECTYPE}
  Assert(Result.RecType = rtStoredStringHeader, 'StoredStringHeader type mismatch!');
{$ENDIF}
end;

function TPatternTrieReader.GetString(const aStringIndex: TStringTableIndex): string;
var
  StrBase, StrEnd: PAnsiChar;
  Len: Integer;
  Value: AnsiString;
begin
{$IFDEF DEBUG}
  if {(aStringIndex < 0) or} (aStringIndex >= NrOfStrings) then
    raise EListError.CreateFmt('String index out of bounds (%d)', [aStringIndex]);
{$ENDIF}

  StrBase := PAnsiChar(GetStringHeader(aStringIndex)) + SizeOf(RStoredStringHeader);
  // Don't call GetStringHeader for the end-pointer, as that would go out of bounds :
  StrEnd := PAnsiChar(GetByteOffset(IndexedStringOffsets[aStringIndex]));
  Len := StrEnd - StrBase;
  SetLength(Value, Len);
  Move(StrBase^, Value[1], Len);
{$IFDEF DXBX_TRIE_COMPRESS_STRINGS}
  // Decompress simple string-prefix reduction (first 1 uncompressed, then 3 compressed) :
  if (aStringIndex and 3) > 0 then
  begin
    // Read & remove repeat count byte :
    Len := Ord(Value[1]);
    Delete(Value, 1, 1);
  end
  else
{$ENDIF}
    Len := 0;

  Result := string(Value); // In Unicode Delphi's, this compiles into an explicit AnsiString>UnicodeString upcast

  // When asked for, prepend prefix of previous string  :
  if Len > 0 then
    Result := Copy(GetString(aStringIndex and (not 3)), 1, Len) + Result;
end;

function TPatternTrieReader.GetStoredLibrary(const aStoredLibraryIndex: TLibraryIndex): PStoredLibrary;
begin
  Result := @(StoredLibrariesList[aStoredLibraryIndex]);
{$IFDEF DXBX_RECTYPE}
  Assert(Result.RecType = rtStoredLibrary, 'StoredLibrary type mismatch!');
{$ENDIF}
end;

function TPatternTrieReader.GetStoredLibraryFunction(const aStoredFunctionIndex: TFunctionIndex): PStoredLibraryFunction;
begin
{$IFDEF DEBUG}
  if {(aGlobalFunctionIndex < 0) or} (aStoredFunctionIndex >= NrOfFunctions) then
    raise EListError.CreateFmt('Function index out of bounds (%d)', [aStoredFunctionIndex]);
{$ENDIF}

  Result := @(StoredLibraryFunctionsList[aStoredFunctionIndex]);
{$IFDEF DXBX_RECTYPE}
  Assert(Result.RecType = rtStoredLibraryFunction, 'StoredFunctionIndex type mismatch!');
{$ENDIF}
end;

function TPatternTrieReader.GetSymbolReference(const aStoredSymbolReferencesIndex: TSymbolReferenceIndex): PStoredSymbolReference;
begin
{$IFDEF DEBUG}
  if {(aStoredSymbolReferencesIndex < 0) or} (aStoredSymbolReferencesIndex >= NrOfReferences) then
    raise EListError.CreateFmt('SymbolReference index out of bounds (%d)', [aStoredSymbolReferencesIndex]);
{$ENDIF}

  Result := @(StoredSymbolReferencesList[aStoredSymbolReferencesIndex]);
{$IFDEF DXBX_RECTYPE}
  Assert(Result.RecType = rtStoredSymbolReference, 'SymbolReference type mismatch!');
{$ENDIF}
end;

function CompareFunctionIndexToStringIndex(const Self: TPatternTrieReader; const StringIndex: TStringTableIndex;
  const aStoredLibraryFunctionIndex: TFunctionIndex): Integer;
var
  StringHeader: PStoredStringHeader;
begin
  // Compare the supplied FunctionIndex to to the FirstFunctionIndex associated with this string :
  StringHeader := Self.GetStringHeader(StringIndex);
  if aStoredLibraryFunctionIndex < StringHeader.FirstFunctionIndex then
    Result := 1
  else
    if (aStoredLibraryFunctionIndex >= (StringHeader.FirstFunctionIndex + StringHeader.NrOfFunctions))
    or (StringHeader.NrOfFunctions = 0) then
      Result := -1
    else
      Result := 0;
end;

function TPatternTrieReader.GetFunctionName(const aStoredLibraryFunctionIndex: TFunctionIndex): string;
var
  StringTableIndex: TStringTableIndex;
begin
  // Do a binary search over the string headers, and stop at the string-table index
  // where this function is associated with the string. This way, we know what string to return :
  GenericBinarySearch({List=}Self, {Count=}NrOfStrings,
    {SearchData=}Pointer(aStoredLibraryFunctionIndex), @CompareFunctionIndexToStringIndex,
    {out}Integer(StringTableIndex));
  Assert(StringTableIndex < NrOfStrings);

  Result := GetString(StringTableIndex);
end;

//function TPatternTrieReader.GetLibraryName(const aLibraryIndex: TLibraryIndex): string;
//var
//  StoredLibrary: PStoredLibrary;
//begin
//  StoredLibrary := GetStoredLibrary(aLibraryIndex);
//  Result := Format('%8s %4d', [GetString(StoredLibrary.LibNameIndex), StoredLibrary.LibVersion]);
//end;

function TPatternTrieReader.GetNode(const aNodeOffset: TByteOffset): PStoredTrieNode;
begin
  UIntPtr(Result) := UIntPtr(StoredSignatureTrieHeader) + aNodeOffset;
{$IFDEF DXBX_RECTYPE}
  Assert(Result.RecType = rtStoredTrieNode, 'StoredTrieNode type mismatch!');
{$ENDIF}
end;

function GetNodeNrChildren(const aStoredTrieNode: PStoredTrieNode; out StretchPtr: PByte): Integer;
begin
  Result := aStoredTrieNode.NrChildrenByte1;
  UIntPtr(StretchPtr) := UIntPtr(aStoredTrieNode) + SizeOf(RStoredTrieNode);
  if Result >= 128 then
    // Reconstruct the NrChildren value :
    Result := (Integer(Result and 127) shl 8) or aStoredTrieNode.NrChildrenByte2
  else
    // If one byte was sufficient, then the next stretch starts 1 byte earlier :
    Dec(UIntPtr(StretchPtr), SizeOf({RStoredTrieNode.NrChildrenByte2:}Byte));
end;

end.

