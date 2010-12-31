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
unit uPatternsToTrie;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, // ERROR_SUCCESS
  SysUtils, // CompareStr, FreeAndNil
  Classes, // TList
  Forms, // Application
  Contnrs, // TObjectList
  Math, // Min
  // Dxbx
  uTypes,
  uDxbxUtils,
  uStoredTrieTypes,
  uCRC16;

type
  TPattern = array of Word;

  TPatternTrieLeaf = class; // forward

  RSymbolReference = packed record
    Offset: Word;
    BaseOffset: Word;
    ReferenceFlag: AnsiChar;
    Name: AnsiString;
  end;

  PVersionedXboxLibrary = ^RVersionedXboxLibrary;
  RVersionedXboxLibrary = packed record
    LibVersion: Integer;
    LibVersionFlag: TLibraryVersionFlag;
    LibNameIndex: Integer;
    LibFlags: TLibFlags;
    LibName: string;
  end;

  PVersionedXboxLibraryFunction = ^RVersionedXboxLibraryFunction;
  RVersionedXboxLibraryFunction = packed record
    VersionedXboxLibrary: PVersionedXboxLibrary;
    ScanParts: array of PAnsiChar; // Used when scanning the .pat lines
    Name: string; // should be AnsiString, but then we'd need TAnsiStringList which Delphi doesn't have.
    SymbolReferences: array of RSymbolReference;
    GlobalFunctionIndex: TFunctionIndex; // Used in _WriteLeaf
    Stored: RStoredLibraryFunction;
  end;

  TPatternTrieNode = class(TObject)
  protected
    MyChildren: TObjectList;
    function GetChild(const Index: Integer): TPatternTrieNode;
    function GetNrChildren: Integer;
    function FindLongestMatchingChild(const aFunctionPattern: TPattern; const aPatternOffset: Integer; out MatchLen: Integer): TPatternTrieNode;
    function SplitBeforeOffset(const aSplitOffset: Integer): TPatternTrieNode;
    function AddChild(const aFunctionPattern: TPattern; const aPatternOffset: Integer): TPatternTrieNode;
    function AddLeaf(const aFunction: PVersionedXboxLibraryFunction): Boolean;
    function NodeAsString: string; virtual;
  public
    Pattern: TPattern;

    property NrChildren: Integer read GetNrChildren;
    property Children[const Index: Integer]: TPatternTrieNode read GetChild;

    constructor Create;
    destructor Destroy; override;

    function AddPattern(const aFunction: PVersionedXboxLibraryFunction; const aFunctionPattern: TPattern; const aPatternOffset: Integer): TPatternTrieNode;
  end;

  TPatternTrieLeaf = class(TPatternTrieNode)
  public
    LeafNr: Integer;
    VersionedXboxLibraryFunction: PVersionedXboxLibraryFunction;
    function NodeAsString: string; override;
  end;

  TPatternTrie = class(TPatternTrieNode)
  public
    VersionedFunctions: TList;
    VersionedLibraries: TList;

    function NodeAsString: string; override;
    procedure Sort;
    procedure Dump(const aOutput: TStrings);
    procedure Save(const aFileName: string);
  end;

procedure PatternToTrie_Main;

implementation

function IsXboxLibraryPatch(const aFunctionName: string): Boolean;
begin
  Result := True; // TODO : Retrieve this info from DxbxKrnl.DLL
end;

const
  // TODO : Make these a command-line option :
  DumpFileName = string('TrieDump.txt');
  StoredTrieFileName = string('StoredTrie.dpt'); // Dxbx Pattern Trie
  StoredTrieTestFileName = string(StoredTrieFileName + '.txt');

function PatternTrie_NodeCompare(Node1, Node2: TPatternTrieNode): Integer;
var
  i, Len: Integer;
begin
  // Determine the length of the shortest of the two patterns :
  Len := Min(Length(Node1.Pattern), Length(Node2.Pattern));
  // Loop over the first part of the pattern, which is present in both :
  for i := 0 to Len - 1 do
  begin
    // Compare every byte (well, actually word - because of the DontCareValue) :
    Result := Node1.Pattern[i] - Node2.Pattern[i];
    // Stop on a mismatch :
    if Result <> 0 then
      Exit;
  end;

  // This case can't happen in a radix trie, but implement it anyway :
  Result := Length(Node1.Pattern) - Length(Node2.Pattern);
end;

function PatternTrie_LeafCompare(Leaf1, Leaf2: TPatternTrieLeaf): Integer;
begin
  // Order leafs on their function name first :
  Result := CompareStr(Leaf1.VersionedXboxLibraryFunction.Name,
                       Leaf2.VersionedXboxLibraryFunction.Name);
  if Result <> 0 then
    Exit;

  // Next, order them on the name of the library they're in :
  Result := CompareStr(Leaf1.VersionedXboxLibraryFunction.VersionedXboxLibrary.LibName,
                       Leaf2.VersionedXboxLibraryFunction.VersionedXboxLibrary.LibName);
  if Result <> 0 then
    Exit;

  // And last, order these by the version of the library :
  Result := Leaf1.VersionedXboxLibraryFunction.VersionedXboxLibrary.LibVersion -
            Leaf2.VersionedXboxLibraryFunction.VersionedXboxLibrary.LibVersion;
end;

{ TPatternTrieNode }

constructor TPatternTrieNode.Create;
begin
  inherited Create;

  MyChildren := TObjectList.Create({OwnsObjects=}True);
end;

destructor TPatternTrieNode.Destroy;
begin
  FreeAndNil(MyChildren);

  inherited Destroy;
end;

function TPatternTrieNode.GetNrChildren: Integer;
begin
  Result := MyChildren.Count;
end;

function TPatternTrieNode.GetChild(const Index: Integer): TPatternTrieNode;
begin
  Result := TPatternTrieNode(MyChildren[Index]);
end;

function TPatternTrieNode.FindLongestMatchingChild(
  const aFunctionPattern: TPattern;
  const aPatternOffset: Integer;
  out MatchLen: Integer): TPatternTrieNode;
var
  i, j: Integer;
  Child: TPatternTrieNode;
begin
  // Search in all children :
  for i := 0 to NrChildren - 1 do
  begin
    Child := Children[i];

    // Count how many elements match :
    j := 0;
    while (j < Length(Child.Pattern))
      and (Child.Pattern[j] = aFunctionPattern[aPatternOffset + j]) do
        Inc(j);

    // As soon as we have a match, we return that (the structure of the
    // trie prevents getting more matches, so we can stop searching) : 
    if j > 0 then
    begin
      Result := Child;
      MatchLen := j;
      Exit;
    end;
  end;

  // Return nothing found :
  Result := nil;
  MatchLen := 0;
end;

function TPatternTrieNode.SplitBeforeOffset(const aSplitOffset: Integer): TPatternTrieNode;
begin
  // Create a new node :
  Result := TPatternTrieNode.Create;
  // Split the Pattern in two, and put the right part in the new node :
  Result.Pattern := Copy(Self.Pattern, aSplitOffset, MaxInt);
  // Shorten the original Pattern with the same length :
  Self.Pattern := Copy(Self.Pattern, 0, aSplitOffset);
  // Move all children over to this new node :
  Swap(MyChildren, Result.MyChildren);
  // Make the new node a child of this node :
  MyChildren.Add(Result);
  // No sorting needed
  Assert(MyChildren.Count = 1);
end;

function TPatternTrieNode.AddChild(const aFunctionPattern: TPattern; const aPatternOffset: Integer): TPatternTrieNode;
begin
  // Create a new child for this function :
  Result := TPatternTrieNode.Create;
  // Determine the Pattern :
  Result.Pattern := Copy(aFunctionPattern, aPatternOffset, MaxInt);
  // Add this child to the list :
  MyChildren.Add(Result);
end;

function TPatternTrieNode.AddLeaf(const aFunction: PVersionedXboxLibraryFunction): Boolean;

  function _CanBeMerged(const aFunction1, aFunction2: PVersionedXboxLibraryFunction): Boolean;
  var
    i: Integer;
  begin
    Result := (aFunction1.Stored.FunctionLength = aFunction2.Stored.FunctionLength)
          and (aFunction1.Stored.CRCValue = aFunction2.Stored.CRCValue)
          and (aFunction1.Stored.CRCLength = aFunction2.Stored.CRCLength)
          and (Length(aFunction1.SymbolReferences) = Length(aFunction2.SymbolReferences)); // Same as NrOfSymbolReferences

    for i := 0 to Length(aFunction1.SymbolReferences) - 1 do
    begin
      if not Result then
        Exit;

      Result := (aFunction1.SymbolReferences[i].Offset        = aFunction2.SymbolReferences[i].Offset)
            and (aFunction1.SymbolReferences[i].BaseOffset    = aFunction2.SymbolReferences[i].BaseOffset)
            and (aFunction1.SymbolReferences[i].ReferenceFlag = aFunction2.SymbolReferences[i].ReferenceFlag)
            and (aFunction1.SymbolReferences[i].Name          = aFunction2.SymbolReferences[i].Name);
    end;
  end;

var
  i: Integer;
  Leaf: TPatternTrieLeaf;
begin
  // Search in all leafs, to see if this function is already present :
  for i := 0 to NrChildren - 1 do
  begin
    Leaf := TPatternTrieLeaf(Children[i]);

    // Check if there's a leaf function already with the same name :
    if  (Leaf.VersionedXboxLibraryFunction.Name = aFunction.Name) then
    begin
      // Check if it's from the same versioned library :
      if Leaf.VersionedXboxLibraryFunction.VersionedXboxLibrary = aFunction.VersionedXboxLibrary then
      begin
        // If so, don't add this duplicate (these do occur somehow).
        Result := False;
        Exit;
      end;

      // Check if the other identifying variables could be merged :
      if _CanBeMerged(aFunction, Leaf.VersionedXboxLibraryFunction) then
      begin
        // Merge identical functions from multiple library version (and fill AvailableInVersions)
        Leaf.VersionedXboxLibraryFunction.Stored.AvailableInVersions :=
          Leaf.VersionedXboxLibraryFunction.Stored.AvailableInVersions
            + [aFunction.VersionedXboxLibrary.LibVersionFlag];

        // Now that the function is merged, don't add another function for it :
        Result := False;
        Exit;
      end;
    end;
  end;

  // Create a new leaf for this function :
  Leaf := TPatternTrieLeaf.Create;
  // Register the Function :
  Leaf.VersionedXboxLibraryFunction := aFunction;
  // Add this leaf to the list :
  MyChildren.Add(Leaf);

  // Indicate that a leaf was added :
  Result := True;
end;

function TPatternTrieNode.AddPattern(const aFunction: PVersionedXboxLibraryFunction;
  const aFunctionPattern: TPattern; const aPatternOffset: Integer): TPatternTrieNode;
var
  MatchLen: Integer;
begin
  // Search for a match in the existing children first :
  Result := FindLongestMatchingChild(aFunctionPattern, aPatternOffset, {out}MatchLen);

  // Check if there should be a new child for this part :
  if Result = nil then
  begin
    // Not a single byte matches the trie, so create a new child node for this :
    Result := AddChild(aFunctionPattern, aPatternOffset);
    // Continue the remainder of this method with the indication that
    // the whole pattern is matched now :
    MatchLen := PATTERNSIZE;
  end;

  // If the pattern wasn't matched completely yet :
  if (aPatternOffset + MatchLen) < PATTERNSIZE then
  begin
    // Check if the matched pattern was only partially covered :
    if MatchLen < Length(Result.Pattern) then
      // If so, split this child in two :
      Result.SplitBeforeOffset(MatchLen);

    // And add the rest of this function to a new child :
    Result := Result.AddPattern(aFunction, aFunctionPattern, aPatternOffset + MatchLen);
  end
  else
    // Now that the complete pattern is present, add the leaf to this last node :
    if not Result.AddLeaf(aFunction) then
      // If that failed (because this is a duplicate), return a nil :
      Result := nil;
end;

// Based on Classes.BinToHex
function TPatternTrieNode.NodeAsString: string;
const
  NibbleHexChars: array [0..15] of Char = '0123456789ABCDEF';
var
  P: PChar;
  i: Integer;
begin
  SetLength(Result, (Length(Pattern) * 2) + 1);
  P := PChar(Result);
  for i := 0 to Length(Pattern) - 1 do
  begin
    if Pattern[i] < 256 then
    begin
      P[0] := NibbleHexChars[Byte(Pattern[i]) shr 4];
      P[1] := NibbleHexChars[Byte(Pattern[i]) and $F];
    end
    else
    begin
      P[0] := '.';
      P[1] := '.';
    end;

    Inc(P, 2);
  end;

  P[0] := ':';
end;

{ TPatternTrieLeaf }

function TPatternTrieLeaf.NodeAsString: string;
var
  i: Integer;
begin
  Result := Format('%d. %.2x %.4x %.4x [%8s %4d] %s', [
    LeafNr,
    VersionedXboxLibraryFunction.Stored.CRCLength,
    VersionedXboxLibraryFunction.Stored.CRCValue,
    VersionedXboxLibraryFunction.Stored.FunctionLength,
    VersionedXboxLibraryFunction.VersionedXboxLibrary.LibName,
    VersionedXboxLibraryFunction.VersionedXboxLibrary.LibVersion,
    VersionedXboxLibraryFunction.Name
    ]);

  for i := 0 to Length(VersionedXboxLibraryFunction.SymbolReferences) - 1 do
  begin
    Result := Result + Format(' ^%.4x %s', [
      VersionedXboxLibraryFunction.SymbolReferences[i].Offset,
      VersionedXboxLibraryFunction.SymbolReferences[i].Name
      ]);

    if VersionedXboxLibraryFunction.SymbolReferences[i].BaseOffset > 0 then
      Result := Result + Format('+%.4x', [
        VersionedXboxLibraryFunction.SymbolReferences[i].BaseOffset
        ]);
  end;
end;

{ TPatternTrie }

function TPatternTrie.NodeAsString: string;
begin
  Result := 'Trie dump:';
end;

procedure TPatternTrie.Sort;

  procedure _SortNode(aPatternNode: TPatternTrieNode);
  var
    i: Integer;
    ChildrenAreLeafNodes: Boolean;
  begin
    // Skip nodes that have no children :
    if aPatternNode.NrChildren = 0 then
      Exit;

    // Determine if the children are leafs or nodes :
    ChildrenAreLeafNodes := (aPatternNode.Children[0].ClassType = TPatternTrieLeaf);

    // Leafs need other sorting than nodes :
    if ChildrenAreLeafNodes then
      aPatternNode.MyChildren.Sort(@PatternTrie_LeafCompare)
    else
      aPatternNode.MyChildren.Sort(@PatternTrie_NodeCompare);

    // Now loop over these (sorted) children :
    for i := 0 to aPatternNode.NrChildren - 1 do
    begin
      if ChildrenAreLeafNodes then
        // Leafs need an incremental number :
        TPatternTrieLeaf(aPatternNode.Children[i]).LeafNr := i
      else
        // And nodes need to be sorted too (this works recursively) :
        _SortNode(aPatternNode.Children[i]);
    end;
  end; // _SortNode

begin
  // Sort the whole trie :
  _SortNode(Self);
end;

procedure TPatternTrie.Dump(const aOutput: TStrings);
var
  IndentStr: TStringList;

  procedure _DumpNode(aPatternNode: TPatternTrieNode; const aIndentLevel: Integer);
  var
    i: Integer;
  begin
    // Output an indented string (produced by the node/leaf itself) :
    aOutput.Add(IndentStr[aIndentLevel] + aPatternNode.NodeAsString);

    // Also dump the children (if there are any), but then one indent-level further :
    for i := 0 to aPatternNode.NrChildren - 1 do
      _DumpNode(aPatternNode.Children[i], aIndentLevel + 1);
  end; // _DumpNode

begin
  IndentStr := TStringList.Create;
  try
    // Prepare the indent strings, so we don't have to call StringOfChar thousands of times :
    while IndentStr.Count < PATTERNSIZE do
      IndentStr.Add(StringOfChar(' ', (IndentStr.Count * 2)));
    IndentStr.Insert(0, '');

    // Now dump the whole trie :
    _DumpNode(Self, 0);
  finally
    FreeAndNil(IndentStr);
  end;
end; // Dump

procedure TPatternTrie.Save(const aFileName: string);
var
  UniqueStrings: TStringList;
  AllFunctions: TStringList;
  OutputFile: TFileStream;

  procedure _WriteLeaf(const aLeaf: TPatternTrieLeaf; const aParentNodeOffset: TByteOffset);
  var
    FunctionIndex: TFunctionIndex;
  begin
    aLeaf.VersionedXboxLibraryFunction.Stored.PatternLeafNodeOffset := aParentNodeOffset;
    FunctionIndex := aLeaf.VersionedXboxLibraryFunction.GlobalFunctionIndex;
    OutputFile.WriteBuffer(FunctionIndex, SizeOf(FunctionIndex));
  end; // _WriteLeaf

  procedure _WriteTrieNodes(const aPatternNode: TPatternTrieNode; const aParentNodeOffset: TByteOffset; Depth: Integer);
  var
    i: Integer;
    ChildrenAreLeafNodes: Boolean;
    CurrentNodeOffset: TByteOffset;
    Len, NrFixed, NrWildcards, PrevIndex: Integer;
    StoredTrieNode: RStoredTrieNode;
    PreviousSiblingOffset: TByteOffset;

    procedure _SetPreviousSibling_NextSiblingOffset(var aPreviousSiblingPosition: TByteOffset);
    var
      CurrentPosition: TByteOffset;
    begin
      // Remember this position (this is where the next sibling will be written) :
      CurrentPosition := OutputFile.Position;
      // Go to the position of the 'NextSiblingOffset' field in the previous sibling :
      OutputFile.Position := aPreviousSiblingPosition + {UIntPtr}(FIELD_OFFSET(PStoredTrieNode(nil).NextSiblingOffset));
      OutputFile.Write(CurrentPosition, SizeOf(TByteOffset));
      // Return to the previous position, and remember that for the next call to this method :
      OutputFile.Position := CurrentPosition;
      {var}aPreviousSiblingPosition := CurrentPosition;
    end;

    procedure _FlushStretch(Index: Integer);
    var
      b: TStretchHeaderByte;
      i: Integer;
    begin
      b := (NrFixed shl NODE_NR_FIXED_SHIFT);
      case NrWildcards of
        0: if NrFixed = PATTERNSIZE then
             b := NODE_ALLFIXED; // 32 can't be measured, but use a special flag for that
        4: b := b or NODE_5BITFIXED_4WILDCARDS;
        8: b := b or NODE_5BITFIXED_8WILDCARDS;
      else
        if Depth = PATTERNSIZE then
          b := b or NODE_5BITFIXED_ALLWILDCARDS
        else
          raise Exception.Create('unhandled case!');
      end;

      // Determine it there's followup :
      if Index < Len then
        b := b or NODE_FLAG_MORE;

      // Write out the heading-byte of this stretch:
      OutputFile.Write(b, SizeOf(b));

      // Write out the fixed bytes for this stretch :
      for i := PrevIndex to Index - 1 do
        if aPatternNode.Pattern[i] <> PatternDontCareValue then
        begin
          b := Byte(aPatternNode.Pattern[i]);
          OutputFile.Write(b, SizeOf(b));
        end;

      // Now that we've flushed, initialize the next stretch :
      PrevIndex := Index;
      NrFixed := 0;
      NrWildcards := 0;
    end; // _FlushStretch

  begin // _WriteTrieNodes
    // All other nodes have at least 1 byte of pattern data, but the root has no data.
    // We could prevent storage for root, but that would complicate the reading code.
    // So root is handled here like any other node - without any special cases.

    // Write the TrieNode :
{$IFDEF DXBX_RECTYPE}
    StoredTrieNode.RecType := rtStoredTrieNode;
{$ENDIF}
    StoredTrieNode.NextSiblingOffset := 0;
    Len := SizeOf(RStoredTrieNode);
    if aPatternNode.NrChildren < 128 then
    begin
      // With a small number of children, store the node with 1 byte less :
      StoredTrieNode.NrChildrenByte1 := Byte(aPatternNode.NrChildren);
      Dec(Len, SizeOf(StoredTrieNode.NrChildrenByte2));
    end
    else
    begin
      // With many children, use both bytes :
      StoredTrieNode.NrChildrenByte1 := Byte(aPatternNode.NrChildren shr 8) or 128;
      StoredTrieNode.NrChildrenByte2 := Byte(aPatternNode.NrChildren);
    end;

    // Write the node record to file already (it will be updated later on) :
    //StoredTrieNode.ParentNodeOffset := aParentNodeOffset;
    CurrentNodeOffset := OutputFile.Position;
    OutputFile.WriteBuffer(StoredTrieNode, Len);

    // Determine how many pattern bytes need to be handled :
    Len := Length(aPatternNode.Pattern);

    // Correct the Depth already :
    Inc(Depth, Len);

    // Loop over the pattern, split it up in stretches and flush it along the way :
    NrFixed := 0;
    NrWildcards := 0;
    PrevIndex := 0;
    for i := 0 to Len - 1 do
    begin
      if aPatternNode.Pattern[i] = PatternDontCareValue then
        Inc(NrWildcards)
      else
      begin
        // Once we start a new stretch of fixed bytes, flush the previous stretch :
        if (NrWildcards > 0) then
          _FlushStretch(i);

        Inc(NrFixed);
      end;
    end;

    // Flush the last stretch :
    _FlushStretch(Len);

    // Remember that the first sibling of all children starts here :
    PreviousSiblingOffset := OutputFile.Position;

    // Determine if the children are leafs or nodes :
    ChildrenAreLeafNodes := (aPatternNode.Children[0].ClassType = TPatternTrieLeaf);
    for i := 0 to aPatternNode.NrChildren - 1 do
    begin
      // Write the children (either leafs or recursive nodes) :
      if ChildrenAreLeafNodes then
        _WriteLeaf(TPatternTrieLeaf(aPatternNode.Children[i]), CurrentNodeOffset)
      else
      begin
        // Update the previous sibling's NextSiblingOffset member :
        if {HasPrevious=}i > 0 then
          _SetPreviousSibling_NextSiblingOffset({var}PreviousSiblingOffset);

        _WriteTrieNodes(aPatternNode.Children[i], {aParentNodeOffset=}CurrentNodeOffset, Depth);
      end;
    end;
  end; // _WriteTrieNodes

var
  i, j, k: Integer;
  CurrentFunction: PVersionedXboxLibraryFunction;
  FunctionIndex: TFunctionIndex;
  TmpString: string;
  StringOffsets: array of TByteOffset;
  PrevOutputString: AnsiString;
  OutputString: AnsiString;
  StoredStringHeader: RStoredStringHeader;
  StoredSignatureTrieHeader: RStoredSignatureTrieHeader;
  StoredLibrary: RStoredLibrary;
  StoredLibraryFunction: RStoredLibraryFunction;
  StoredSymbolReference: RStoredSymbolReference;
begin // Save
{$IFDEF DXBX_RECTYPE}
  StoredStringHeader.RecType := rtStoredStringHeader;
  StoredLibrary.RecType := rtStoredLibrary;
  StoredSymbolReference.RecType := rtStoredSymbolReference;
{$ENDIF}

  OutputFile := TFileStream.Create(aFileName, fmCreate);
  UniqueStrings := TStringList.Create;
  AllFunctions := TStringList.Create;
  try
    // Collection for all strings (one occurrence per unique string) :
    UniqueStrings.Sorted := True;
    UniqueStrings.Duplicates := dupIgnore;
    UniqueStrings.CaseSensitive := True;

    // Collect all functions in a list :
    AllFunctions.Duplicates := dupAccept;
    AllFunctions.CaseSensitive := True;
    for i := 0 to VersionedFunctions.Count - 1 do
    begin
      // Add each function to the functionlist :
      CurrentFunction := PVersionedXboxLibraryFunction(VersionedFunctions[i]);
      TmpString := CurrentFunction.Name;
      AllFunctions.AddObject(TmpString, TObject(CurrentFunction));

      // Function names are one origin of strings :
      UniqueStrings.Add(TmpString);

      // Add all symbol-reference names to the unique string list too :
      for j := 0 to Length(CurrentFunction.SymbolReferences) - 1 do
      begin
        TmpString := string(CurrentFunction.SymbolReferences[j].Name);
        UniqueStrings.Add(TmpString);
      end
    end;

    // Sort the functions and register their new index for later use in _WriteLeaf :
    AllFunctions.Sorted := True;
    for i := 0 to AllFunctions.Count - 1 do
    begin
      CurrentFunction := PVersionedXboxLibraryFunction(AllFunctions.Objects[i]);
      CurrentFunction.GlobalFunctionIndex := i;
    end;

    // Also, add all library names to the unique string list :
    for i := 0 to VersionedLibraries.Count - 1 do
    begin
      TmpString := PVersionedXboxLibrary(VersionedLibraries[i]).LibName;
      UniqueStrings.Add(TmpString);
    end;

    // Initialize the header record :
    ZeroMemory(@StoredSignatureTrieHeader, SizeOf(StoredSignatureTrieHeader));
    StoredSignatureTrieHeader.Header := 'DxbxTrie';

    // Initialize the string table information, the first section of data after the header :
    StoredSignatureTrieHeader.StringTable.NrOfStrings := UniqueStrings.Count;
    StoredSignatureTrieHeader.StringTable.IndexedStringOffsets := SizeOf(StoredSignatureTrieHeader);
    StoredSignatureTrieHeader.StringTable.StringsOffset := StoredSignatureTrieHeader.StringTable.IndexedStringOffsets + TByteOffset(SizeOf(TByteOffset) * UniqueStrings.Count);

    // Start by writing the string data, we know exactly where it will start, the size will be known afterwards :
    OutputFile.Position := StoredSignatureTrieHeader.StringTable.StringsOffset;
    SetLength(StringOffsets, UniqueStrings.Count);
    FunctionIndex := 0;
    PrevOutputString := '';
    for i := 0 to UniqueStrings.Count - 1 do
    begin
      // Populate the string header with the index of the first function that bares this name,
      // and the number of functions that follow (zero means there's no function for this string) :
      begin
        // We start at the current function index (even when the delta will become zero,
        // because we need to be able to do a binary search - see TPatternTrieReader.GetFunctionName) :
        StoredStringHeader.FirstFunctionIndex := FunctionIndex;

        // Read the current string, and step over all functions that match that name (could be zero).
        // Do note, that both UniqueStrings and AllFunctions are sorted, allowing a stepping lookup :
        TmpString := UniqueStrings[i];
        while (DWord(FunctionIndex) < DWord(AllFunctions.Count))
          and (TmpString = AllFunctions[FunctionIndex]) do
            Inc(FunctionIndex);

        // Calculate the delta, and store that in the header :
        StoredStringHeader.NrOfFunctions := FunctionIndex - StoredStringHeader.FirstFunctionIndex;
      end;

      // First, write the string header (which indicates the functions associated with this string) :
      OutputFile.WriteBuffer(StoredStringHeader, SizeOf(RStoredStringHeader));

      // Make sure we write the strings in Ansi format (else, Unicode Delphi's would garble the output) :
      OutputString := AnsiString(TmpString);
{$IFDEF DXBX_TRIE_COMPRESS_STRINGS}
      // Compress simple string-prefix reduction (first 1 uncompressed, then 3 compressed) :
      if (i and 3) > 0 then
      begin
        j := 1;
        while (j <= Length(OutputString)) and (j <= Length(PrevOutputString)) and (OutputString[j] = PrevOutputString[j]) and (j < 256) do
          Inc(j);
        Dec(j);

        if j = 0 then
          OutputString := #0 + OutputString
        else
        begin
          // Mark with 1 byte how many lead-bytes of the preceding string are identical (we don't store them again) :
          Delete({var}OutputString, 1, j - 1);
          OutputString[1] := AnsiChar(j);
        end;
      end
      else
        PrevOutputString := OutputString;
{$ENDIF}

      // Right after the header, write the actual string (no zero terminator character needed)
      // and remember the resulting file position (the first byte AFTER the complete string) :
      OutputFile.WriteString(OutputString);
      StringOffsets[i] := OutputFile.Position;
    end; // for UniqueStrings

    // Remember where we ended, so we know where to put the LibraryTable :
    StoredSignatureTrieHeader.LibraryTable.NrOfLibraries := VersionedLibraries.Count;
    StoredSignatureTrieHeader.LibraryTable.LibrariesOffset := OutputFile.Position;

    // Now that the string data is written, put the IndexedStringOffsets in place :
    OutputFile.Position := StoredSignatureTrieHeader.StringTable.IndexedStringOffsets;
    OutputFile.WriteBuffer(StringOffsets[0], Length(StringOffsets) * SizeOf(StringOffsets[0]));
    SetLength(StringOffsets, 0); // and clear them already

    // Write the libraries to the position we just remembered :
    OutputFile.Position := StoredSignatureTrieHeader.LibraryTable.LibrariesOffset;
    for i := 0 to VersionedLibraries.Count - 1 do
    begin
      StoredLibrary.LibVersion := PVersionedXboxLibrary(VersionedLibraries[i]).LibVersion;
      PVersionedXboxLibrary(VersionedLibraries[i]).LibNameIndex := UniqueStrings.IndexOf(PVersionedXboxLibrary(VersionedLibraries[i]).LibName);
      StoredLibrary.LibNameIndex := PVersionedXboxLibrary(VersionedLibraries[i]).LibNameIndex;
      StoredLibrary.LibFlags := PVersionedXboxLibrary(VersionedLibraries[i]).LibFlags;
      OutputFile.WriteBuffer(StoredLibrary, SizeOf(StoredLibrary));
    end;

    // TODO : Split the symbol references up (for better re-use) over these parts :
    // 1) offsets
    // 2) symbol indexes
    // 3) BaseOffset corrections

    // Write symbol-references :
    StoredSignatureTrieHeader.ReferencesTable.NrOfReferences := 0;
    StoredSignatureTrieHeader.ReferencesTable.ReferencesOffset := OutputFile.Position;
    for i := 0 to AllFunctions.Count - 1 do
    begin
      CurrentFunction := PVersionedXboxLibraryFunction(AllFunctions.Objects[i]);
      CurrentFunction.Stored.FirstSymbolReference := StoredSignatureTrieHeader.ReferencesTable.NrOfReferences;
      CurrentFunction.Stored.LibraryNameIndex := CurrentFunction.VersionedXboxLibrary.LibNameIndex;
      for j := 0 to Length(CurrentFunction.SymbolReferences) - 1 do
      begin
        StoredSymbolReference.Offset := CurrentFunction.SymbolReferences[j].Offset;
        StoredSymbolReference.BaseOffset := CurrentFunction.SymbolReferences[j].BaseOffset;
        StoredSymbolReference.NameIndex := UniqueStrings.IndexOf(string(CurrentFunction.SymbolReferences[j].Name));
        case CurrentFunction.SymbolReferences[j].ReferenceFlag of
          'D':
            StoredSymbolReference.ReferenceFlags := rfIsAbsolute;
          'R':
            StoredSymbolReference.ReferenceFlags := rfIsRelative;
          'S':
            StoredSymbolReference.ReferenceFlags := rfIsSectionRel;
        else
          StoredSymbolReference.ReferenceFlags := 0;
        end;

        // Detect & mark duplicate references :
        for k := 0 to j - 1 do
          if CurrentFunction.SymbolReferences[k].Name = CurrentFunction.SymbolReferences[j].Name then
          begin
            StoredSymbolReference.ReferenceFlags := StoredSymbolReference.ReferenceFlags or rfIsDuplicate;
            Break;
          end;

        OutputFile.WriteBuffer(StoredSymbolReference, SizeOf(RStoredSymbolReference));
      end;

      Inc(StoredSignatureTrieHeader.ReferencesTable.NrOfReferences, Length(CurrentFunction.SymbolReferences));
    end;

    // Reserve space for functions but write them after processing leaf nodes,
    // so that each function can point to the leaf in which it ended up in :
    // (This will be necessary when scanning for one specific function.)
    StoredSignatureTrieHeader.FunctionTable.NrOfFunctions := AllFunctions.Count;
    StoredSignatureTrieHeader.FunctionTable.FunctionsOffset := OutputFile.Position;
    OutputFile.Position := OutputFile.Position + (AllFunctions.Count * SizeOf(StoredLibraryFunction));

    // Write the trie - a compact format for all nodes and leafs.
    // This whole file will be included in our Xbox Krnl DLL as a resource,
    // so it's going to be a readonly structure, that must be fast to parse :
    StoredSignatureTrieHeader.TrieRootNode := OutputFile.Position;
    _WriteTrieNodes(Self, {aParentNodeOffset=}0, {Depth=}0);

    // Write functions :
    OutputFile.Position := StoredSignatureTrieHeader.FunctionTable.FunctionsOffset;
    for i := 0 to AllFunctions.Count - 1 do
    begin
      CurrentFunction := PVersionedXboxLibraryFunction(AllFunctions.Objects[i]);

      StoredLibraryFunction := CurrentFunction.Stored;
{$IFDEF DXBX_RECTYPE}
      StoredLibraryFunction.RecType := rtStoredLibraryFunction;
{$ENDIF}
      StoredLibraryFunction.NrOfSymbolReferences := Length(CurrentFunction.SymbolReferences);

      OutputFile.WriteBuffer(StoredLibraryFunction, SizeOf(StoredLibraryFunction));
    end;

    // Last, go back to the start of the file, and write the completed header there :
    OutputFile.Position := 0;
    OutputFile.WriteBuffer(StoredSignatureTrieHeader, SizeOf(StoredSignatureTrieHeader));
  finally
    FreeAndNil(AllFunctions);
    FreeAndNil(UniqueStrings);
    FreeAndNil(OutputFile);
  end;
end; // Save

//

function LocateVisualStudioFilePath(const aFileName: string): string;
var
  VisualStudioCommonToolsFolder: string;
begin
  // Search for the specified file in the search-path :
  Result := LocateExecutablePath(aFileName);
  if IsFile(Result) then
    Exit;

  // Determine Visual Studio's Common\Tools folder :
  VisualStudioCommonToolsFolder := GetEnvVarValue('VS71COMNTOOLS', {Dequote=}True);
  if not IsFolder(VisualStudioCommonToolsFolder) then
  begin
    VisualStudioCommonToolsFolder := GetEnvVarValue('VS90COMNTOOLS', {Dequote=}True);
    if not IsFolder(VisualStudioCommonToolsFolder) then
    begin
      // Visual Studio folder not found :
      Result := '';
      Exit;
    end;
  end;

  // Search for the specified file in Visual Studio's Common\IDE folder :
  Result := ExpandFileName(VisualStudioCommonToolsFolder + '\..\IDE\' + aFileName);
  if IsFile(Result) then
    Exit;

  // Search for the specified file in Visual Studio's VC\bin folder :
  Result := ExpandFileName(VisualStudioCommonToolsFolder + '\..\..\VC\bin\' + aFileName);
  if not IsFile(Result) then
    // File not found :
    Result := '';
end;

var
  LinkPath: string;
  mspdb80Path: string;

function LocateLink: Boolean;
begin
  LinkPath := LocateVisualStudioFilePath('link.exe');
  mspdb80Path := LocateVisualStudioFilePath('mspdb80.dll');

  Result := (LinkPath <> '') and (mspdb80Path <> '');
end;

// Based on http://www.martinstoeckli.ch/delphi/delphi.html#AppRedirectOutput and
// http://stackoverflow.com/questions/1212176/delphi-6-read-console-apps-output-while-running
procedure RunLibDump(const aLibFileName: string; const aOutput: TStream);
const
  BLOCK_SIZE = 4300;
var
  CommandLine: string;
  Security: TSecurityAttributes;
  StdInPipe_Read, StdInPipe_Write: THandle;
  Start: TStartUpInfo;
  ProcessInfo: TProcessInformation;
  KeepRunning: Boolean;
  Buffer: array[0..BLOCK_SIZE-1] of Byte;
  BytesAvail, BytesRead: DWord;
  LogLine: string;
begin
  CommandLine := '"' + LinkPath + '" /dump /headers /rawdata /relocations "' +  aLibFileName + '"';
  LogLine := #13'Dumping ' + ExtractFileName(aLibFileName) + ' : ';

  Security.nLength := SizeOf(TSecurityAttributes) ;
  Security.bInheritHandle := True;
  Security.lpSecurityDescriptor := nil;
  if CreatePipe({var}StdInPipe_Read, {var}StdInPipe_Write, @Security, 0) then
  try
    ZeroMemory(@Start, Sizeof(Start));
    Start.cb := SizeOf(Start) ;
    Start.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
    Start.wShowWindow := SW_HIDE;
    Start.hStdInput := StdInPipe_Read;
    Start.hStdOutput := StdInPipe_Write;

    // Note : mspdb80.dll is needed, but is not installed in System32 when
    // Visual Studio is installed, but rather in Common7\IDE. By passing
    // it's dir as CurrentDirectory, Link.exe can find it :
    if CreateProcess(nil,
                     PChar(CommandLine),
                     @Security,
                     @Security,
                     True,
                     NORMAL_PRIORITY_CLASS,
                     nil,
                     PChar(ExtractFilePath(mspdb80Path)),
                     Start,
                     {var}ProcessInfo) then
    try
      aOutput.Size := 16 * 1024 * 1024; // Reserve 16 MB upfront
      aOutput.Position := 0;
      repeat
        KeepRunning := WaitForSingleObject(ProcessInfo.hProcess, 0) = WAIT_TIMEOUT;
        Application.ProcessMessages;
        while True do
        begin
          if not PeekNamedPipe(StdInPipe_Read, nil, 0, nil, @BytesAvail, nil) then
            RaiseLastOSError;

          if BytesAvail = 0 then
            Break; // from while

          if not ReadFile(StdInPipe_Read, {var}Buffer[0], BLOCK_SIZE, {var}BytesRead, nil) then
            RaiseLastOSError;

          while (BytesRead > 0) and (Buffer[BytesRead-1] = 0) do
            Dec(BytesRead);

          if BytesRead > 0 then
          begin
            // Write the output to the stream, and show the byte-count as a progress indicator :
            aOutput.Write(Buffer, BytesRead);
            Write(LogLine, aOutput.Position);
          end;
        end; // while True

      until not KeepRunning;
    finally
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  finally
    CloseHandle(StdInPipe_Read);
    CloseHandle(StdInPipe_Write);
  end;

  aOutput.Size := aOutput.Position;

  // Overwrite progress line with a finish-message :
  WriteLn(#13'Dumped ', ExtractFileName(aLibFileName), '.               ');
end; // RunLibDump

procedure ConvertDumpToPattern(const aDump: TCustomMemoryStream);
const
  SymbolStartMarker = 'COMDAT; sym= ';
var
  StringList: TStringList;
  i, p: Integer;
  Line: string;
  ScanMode: (smUnknown, smReadSectionHeader, smReadRawData, smReadReallocations);

  SymbolSizeHexStr: string;
  SymbolIsCode: Boolean;
  SymbolName: string;
  PatternString: string;
  SymbolReferences: string;

  CRCBuffer: array [0..255] of Byte;

  NrOfPatternsFound: Integer;

  procedure _WriteString(const aString: string);
  begin
    aDump.WriteString(AnsiString(aString));
  end;

  procedure _FlushPattern;
  var
    i: Integer;
    CRCLength: Word;
    CRCValue: Word;
  begin
    if SymbolName = '' then
      Exit;

    if not SymbolIsCode then
    begin
      // TODO : Handle data symbols (SymbolIsCode=False) somehow.
      // It would be nice if we stored their name and size for later usage.
      Exit;
    end;

    CRCValue := 0;
    CRCLength := 0;
    if Length(PatternString) > 64 then
    begin
      // Determine length of non-wildcard string after the first 64 hex chars :
      i := 65;
      while (CRCLength < $FF)
        and (i < Length(PatternString))
        and (PatternString[i] <> '.') do
      begin
        Inc(CRCLength);
        Inc(i, 2);
      end;
      // Convert that part of the pattern to a byte-buffer and calculate the CRC for that data :
      HexToBin(PChar(@PatternString[65]), CRCBuffer, CRCLength);
      CRCValue := CalcCRC16(@CRCBuffer[0], CRCLength);
    end
    else
    begin
      // Pad the pattern with wildcards until it's at least 64 characters long :
      while Length(PatternString) < 64 do
        PatternString := PatternString + '..';
    end;

    // write a .pat file back to Input, so it can be scanned by ParseAndAppendPatternsToTrie :
    _WriteString(Copy(PatternString, 1, 64));
    _WriteString(Format(' %.2x %.4x ', [CRCLength, CRCValue]));
    _WriteString(SymbolSizeHexStr);
    _WriteString(' ');
    _WriteString(SymbolName);
    if SymbolReferences <> '' then
      _WriteString(SymbolReferences);
    if Length(PatternString) > 64 then
    begin
      _WriteString(' ');
      _WriteString(Copy(PatternString, 65 + (CRCLength * 2), MaxInt));
    end;
    _WriteString(#13#10);

    // Show some progress :
    SetLength(SymbolName, 78);
    Write(SymbolName, #13);
    Inc(NrOfPatternsFound);
  end; // _FlushPattern

  procedure _ReinitVariables;
  begin
    SymbolSizeHexStr := '';
    SymbolIsCode := True;
    SymbolName := '';
    PatternString := '';
    SymbolReferences := '';
  end; // _ReinitVariables

begin
  StringList := TStringList.Create;
  try
    // Move the dump over to a stringlist :
    StringList.Text := string(aDump.DataString);

    aDump.Position := 0;
    NrOfPatternsFound := 0;

    _ReinitVariables;

    // Scan the dump line by line :
    ScanMode := smUnknown;
    for i := 0 to StringList.Count - 1 do
    begin
      Line := StringList[i];
      if Line = '' then
        Continue;

      // Check block starts only when possible (for speed) :
      if (Line[1] > 'F') and (Pos('#', Line) > 0) then
      begin
        // Check if we enter a new output block here :
        if StartsWithText(Line, 'SECTION HEADER #') then
        begin
          _FlushPattern;
          _ReinitVariables;
          ScanMode := smReadSectionHeader;
          Continue;
        end;

        // Only enter other blocks if we're scanning a named Code symbol :
        if (SymbolName <> '') and SymbolIsCode then
        begin
          if StartsWithText(Line, 'RAW DATA #') then
          begin
            ScanMode := smReadRawData;
            Continue;
          end;

          if StartsWithText(Line, 'RELOCATIONS #') then
          begin
            ScanMode := smReadReallocations;
            Continue;
          end;
        end;

        ScanMode := smUnknown;
      end;

      case ScanMode of
        smReadSectionHeader:
        begin
          // Search for symbol size :
          p := Pos('size of raw data', Line);
          if p > 0 then
          begin
            SymbolSizeHexStr := Trim(Copy(Line, 1, p - 1));
            while Length(SymbolSizeHexStr) < 4 do
              SymbolSizeHexStr := '0' + SymbolSizeHexStr;

            Continue;
          end;

          // Search for symbol data-marker :
          if Pos({Uni/I}'nitialized Data', Line) > 0 then
          begin
            // Mark symbol as data :
            SymbolIsCode := False;
            Continue;
          end;

          // Search for symbol name :
          p := Pos(SymbolStartMarker, Line);
          if p > 0 then
          begin
            // Handle cases like :
            // COMDAT; sym= "unsigned long const * const D3D::g_TextureFormatMask" (?g_TextureFormatMask@D3D@@3QBKB)
            // COMDAT; sym= "void __stdcall D3D::InitializeVertexShaderFromFvf(struct D3D::VertexShader *,unsigned long)" (?InitializeVertexShaderFromFvf@D3D@@YGXPAUVertexShader@1@K@Z)
            // COMDAT; sym= _D3DPRIMITIVETOVERTEXCOUNT
            SymbolName := Copy(Line, p + Length(SymbolStartMarker), MaxInt);

            p := Pos('"', SymbolName);
            if p > 0 then
            begin
              Delete(SymbolName, 1, p);
              p := Pos('"', SymbolName);
              if p > 0 then
                Delete(SymbolName, 1, p);
            end;

            p := Pos('(', SymbolName);
            if p > 0 then
              Delete(SymbolName, 1, p);
            if SymbolName[Length(SymbolName)] = ')' then
              SetLength(SymbolName, Length(SymbolName) - 1);

            // Last header value read, postpone scanning until next block :
            ScanMode := smUnknown;
            Continue;
          end;
        end;

        smReadRawData:
        begin
          // Handle cases like :
          //   00000000: 53 8A 5C 24 18 F6 C3 10 56 75 24 8B 35 00 00 00  S.\$.öÃ.Vu$.5...
          //   000000A0: 05 46 6C 61 67 73 02 00 06 00                    .Flags....
          p := Pos(':', Line);
          if p > 8 then
            PatternString := PatternString + StringReplace(Copy(Line, p + 1, 16 *3), ' ', '', [rfReplaceAll]);
        end;

        smReadReallocations:
        begin
          // Handle cases like :
          //0        1         2         3         4         5         6         7         8
          //12345678901234567890123456789012345678901234567890123456789012345678901234567890
          // 00000000  DIR32NB                    00000000        94  ?GetColorMaterial@D3D@@YGKXZ (unsigned long __stdcall D3D::GetColorMaterial(void))
          // 00000014  DIR32                      00000174       2EA  _D3D__RenderState
          // 00000049  REL32                      00000000       223  _D3DDevice_BeginStateBig@4
          // 000000E4  REL32                      00000000        5A  ?ParseProgram@D3D@@YGPCKPCKPBKK@Z (unsigned long volatile * __stdcall D3D::ParseProgram(unsigned long volatile *,unsigned long const *,unsigned long))
          if IsDigit(Line[2]) then
          begin
            // See if there's a unmangled symbol name too (we dont want those)
            p := Pos('(', Line);
            if p = 0 then
              p := MaxInt;

            // Parse and add symbol-reference (Offset + R/D + Referenced SymbolName) :
            SymbolReferences := SymbolReferences
                              + ' ^' + {Offset=}Copy(Line, 6, 4)
                              + {'R'el32/'D'ir32/'S'ecrel=}Line[12]
                              + {SymbolName=}Copy(Line, 58, p - 59);

            // Add the base-offset (when it's not zero) :
            if Copy(Line, 43, 4) <> '0000' then
              SymbolReferences := SymbolReferences + '+' + {BaseOffset=}Copy(Line, 43, 4);

            // Mask out all symbol-reference locations in the pattern
            if ScanHexWord(PAnsiChar(AnsiString({Offset=}Copy(Line, 6, 4))), {var}p) then
            begin
              p := 1 + (p * 2);
              PatternString[p+0] := '.';
              PatternString[p+1] := '.';
              PatternString[p+2] := '.';
              PatternString[p+3] := '.';
              PatternString[p+4] := '.';
              PatternString[p+5] := '.';
              PatternString[p+6] := '.';
              PatternString[p+7] := '.';
            end;
          end;
        end;
      end; // case
    end; // for

    _FlushPattern;
    _WriteString('---'#13#10);
    aDump.Size := aDump.Position;
    WriteLn('Converted dump to ', NrOfPatternsFound, ' pattern format lines.                                  ');

  finally
    FreeAndNil(StringList);
  end;
end; // ConvertDumpToPattern

const
  // Value Indexes :
  viPattern = 0;
  viCRCLength = 1;
  viCRCValue = 2;
  viFunctionLength = 3;
  viFunctionName = 4;

const
  MAX_NR_OF_SYMBOL_REFERENCES = High(Byte);


type
  TScanPart = (spPatterns, spInformation);

  PPatternScanningContext = ^RPatternScanningContext;
  RPatternScanningContext = packed record
    VersionedXboxLibrary: PVersionedXboxLibrary;
    OnlyPatches: Boolean;
    FunctionList: TStringList;
    PatternTrie: TPatternTrie;
    ScanPart: TScanPart;
  end;

function ParseAndAppendPatternLineToTrie_Callback(aLine: PAnsiChar; aLength: Integer; aContext: PPatternScanningContext): Boolean;
const
  PATTERN_START_OF_NAME = 85;
  Marker_IsIncomplete = 'IsIncomplete';
  MarkerLen_IsIncomplete = Length(Marker_IsIncomplete);
var
  VersionedXboxLibraryFunction: PVersionedXboxLibraryFunction;
  i: Integer;
  Prev: PAnsiChar;
  NrValues: Integer;

  procedure _Add(const aPtr: PAnsiChar);
  begin
    aPtr^ := #0;
    Inc(NrValues);
    if NrValues > Length(VersionedXboxLibraryFunction.ScanParts) then
      SetLength(VersionedXboxLibraryFunction.ScanParts, NrValues * 2);

    VersionedXboxLibraryFunction.ScanParts[NrValues - 1] := Prev;
    Prev := aPtr + 1;
  end;

begin
  // Stop scanning at #0 :
  if (aLine^ = #0) then
  begin
    Result := False;
    Exit;
  end;

  // Continue scanning following lines :
  Result := True;

  // Switch scanning mode at the '-' marker :
  if (aLine^ = '-') then
  begin
    aContext.ScanPart := spInformation;
    Exit;
  end;

  if aContext.ScanPart = spInformation then
  begin
    // Detect if this is a manually created (and thus incomplete) library.
    // This fact is used in the symbol scanning code - incomplete patterns
    // are not used as a 'definite' when scanning for symbols (especially
    // when we have no version-exact patterns for a title) :
    if strncmp(aLine, Marker_IsIncomplete, MarkerLen_IsIncomplete) = 0 then
      Include(aContext.VersionedXboxLibrary.LibFlags, lfIsIncomplete);

    Exit;
  end;

  // Skip lines that don't start with a hexadecimal character (so this skips ';' comment lines too) :
  if not (aLine^ in ['0'..'9','a'..'z','A'..'Z']) then
    Exit;

  // Lines must at least be 86 characters long :
  if aLength < PATTERN_START_OF_NAME + 1 then
    Exit;

  // Prepare the record which will contain this pattern's data :
  New(VersionedXboxLibraryFunction);
  SetLength(VersionedXboxLibraryFunction.ScanParts, 16);
  VersionedXboxLibraryFunction.VersionedXboxLibrary := aContext.VersionedXboxLibrary;
  VersionedXboxLibraryFunction.Stored.AvailableInVersions := [aContext.VersionedXboxLibrary.LibVersionFlag];

  Prev := aLine;
  NrValues := 0;
  for i := 0 to aLength - 1 do
    if aLine[i] = ' ' then
      _Add(@(aLine[i]));

  _Add(@(aLine[aLength]));

  // Only store functions that have enough, but not too many parts :
  if (NrValues > viFunctionName) and (NrValues <= viFunctionName + (2 * MAX_NR_OF_SYMBOL_REFERENCES)) then
  begin
    SetLength(VersionedXboxLibraryFunction.ScanParts, NrValues);
    aContext.FunctionList.AddObject(string(AnsiString(VersionedXboxLibraryFunction.ScanParts[viFunctionName])), Pointer(VersionedXboxLibraryFunction))
  end
  else
    Dispose(VersionedXboxLibraryFunction);
end; // ParseAndAppendPatternLineToTrie_Callback

function ParseVersionedXboxLibraryFunction(const aVersionedXboxLibraryFunction: PVersionedXboxLibraryFunction; aContext: PPatternScanningContext): Boolean;
const
  PATTERN_VALID_NAME_CHARACTERS = ['_', 'a'..'z', 'A'..'Z', '0'..'9', '@', ':', '?', '$'];
var
  VersionedXboxLibraryFunctionPattern: TPattern;
  i, Value: Integer;
  NrSymbolReferences: Integer;
  SymbolReferencedFunctionName: AnsiString;
  aLine: PAnsiChar;

  procedure _SkipChar(const aChar: AnsiChar);
  begin
    Assert(aLine^ = aChar);
    Inc(aLine);
  end;

  function _MustRegisterReference: Boolean;
  begin
    // Test for symbols that shouldn't be present; Data and function references are fine,
    // but 'special' things, like internal labels, section-relative variables and such
    // shouldn't even be mentioned in the list of references :

    // No __tls stuff :
    if StartsWithString(SymbolReferencedFunctionName,'__tls')
    // No internal labels :
    or StartsWithString(SymbolReferencedFunctionName, '$L')
    // No ___ stuff :
    or StartsWithString(SymbolReferencedFunctionName, '___') then
    begin
      Result := False;
      Exit;
    end;

    // Only DIR32 or REL32 references :
    case aLine[4] of
      'D', 'R':
        Result := True
    else // SECTION and SECREL references are not easy to resolve, so skip those too :
      Result := False
    end;

    // TODO -oDxbx: Apart from the actual availability, also determine if this
    // reference must be checked during our symbol-detection. This is currently
    // done in _CheckAndRememberFunctionSymbolReferences, but it's quicker to
    // do that check here, and register the result via a flag.
  end;

begin
  // Remember the function name :
  aVersionedXboxLibraryFunction.Name := string(AnsiString(aVersionedXboxLibraryFunction.ScanParts[viFunctionName]));

  // Now, see if we need to filter
  if aContext.OnlyPatches then
  begin
    // In this mode, skip all functions that aren't patched :
    if not IsXboxLibraryPatch(aVersionedXboxLibraryFunction.Name) then
    begin
      // No leaf added, free the function :
      Result := False;
      Exit;
    end;
  end;

  // Convert the first 32 bytes into a pattern :
  aLine := aVersionedXboxLibraryFunction.ScanParts[viPattern];
  SetLength(VersionedXboxLibraryFunctionPattern, PATTERNSIZE);
  for i := 0 to PATTERNSIZE - 1 do
  begin
    if ScanHexByte(aLine, {var}Value) then
    begin
      // Every succesfully scanned hexadecimal byte, is added as-is to the pattern-array :
      VersionedXboxLibraryFunctionPattern[i] := Value;
    end
    else
    begin
      // Everything else is probably a "don't care" indicator, so put that in here :
      VersionedXboxLibraryFunctionPattern[i] := PatternDontCareValue;
    end;
    Inc(aLine, 2);
  end;

  // Scan the CRC length :
  if ScanHexByte(aVersionedXboxLibraryFunction.ScanParts[viCRCLength], {var}Value) then
    aVersionedXboxLibraryFunction.Stored.CRCLength := Byte(Value);

  // Scan the CRC value :
  if ScanHexWord(aVersionedXboxLibraryFunction.ScanParts[viCRCValue], {var}Value) then
    aVersionedXboxLibraryFunction.Stored.CRCValue := Word(Value);

  // Scan the function length :
  if ScanHexWord(aVersionedXboxLibraryFunction.ScanParts[viFunctionLength], {var}Value) then
    aVersionedXboxLibraryFunction.Stored.FunctionLength := Word(Value);

  Result := True;

  // See if there's something after the function name (like a symbol-reference) :
  i := viFunctionName + 1;
  NrSymbolReferences := 0;
  // Pre-allocate enough space, to prevent reallocations :
  SetLength(aVersionedXboxLibraryFunction.SymbolReferences, Length(aVersionedXboxLibraryFunction.ScanParts) div 2);
//  SetLength(aVersionedXboxLibraryFunction.SymbolReferences, MAX_NR_OF_SYMBOL_REFERENCES); // Ridiculously large, but save a lot of reallocation
  // Scan all symbol-references, and add the valid ones to the function :
  while (i + 1) < Length(aVersionedXboxLibraryFunction.ScanParts) do
  begin
    aLine := aVersionedXboxLibraryFunction.ScanParts[i];

    // Check if there's a symbol-reference starting here (begins with an offset) :
    if aLine^ <> '^' then
      Break;

    // There's a symbol-reference here, scan it :
    Inc(aLine);
    if not ScanHexWord(aLine, {var}Value) then
      Break;

    // Get the function name :
    Inc(i);
    SymbolReferencedFunctionName := AnsiString(aVersionedXboxLibraryFunction.ScanParts[i]);

    // Check if the symbol reference must be known :
    if _MustRegisterReference then
    begin
      // Remember the cross-reference function name :
      Inc(NrSymbolReferences);
      Assert(NrSymbolReferences < High(Byte), aVersionedXboxLibraryFunction.Name);
      aVersionedXboxLibraryFunction.SymbolReferences[NrSymbolReferences-1].Offset := Word(Value);
      aVersionedXboxLibraryFunction.SymbolReferences[NrSymbolReferences-1].BaseOffset := 0;
      aVersionedXboxLibraryFunction.SymbolReferences[NrSymbolReferences-1].ReferenceFlag := aLine[4];

      Value := Pos(AnsiString('+'), SymbolReferencedFunctionName);
      if Value > 0 then
      begin
        aVersionedXboxLibraryFunction.SymbolReferences[NrSymbolReferences-1].Name := Copy(SymbolReferencedFunctionName, 1, Value - 1);
        if ScanHexWord(PAnsiChar(SymbolReferencedFunctionName) + Value, {var}Value) then
          aVersionedXboxLibraryFunction.SymbolReferences[NrSymbolReferences-1].BaseOffset := Value;
      end
      else
        aVersionedXboxLibraryFunction.SymbolReferences[NrSymbolReferences-1].Name := SymbolReferencedFunctionName;
    end;

    // Step to the next possible symbol-reference :
    Inc(i);
  end; // while

  // Resize-to-fit the array of symbol-references :
  SetLength(aVersionedXboxLibraryFunction.SymbolReferences, NrSymbolReferences);

  // Now that the pattern line is scanned, add it to the trie :
  if aContext.PatternTrie.AddPattern(aVersionedXboxLibraryFunction, VersionedXboxLibraryFunctionPattern, 0) = nil then
    // No leaf added, free the function :
    Dispose(aVersionedXboxLibraryFunction)
  else
    // The function was added to the trie, remember it :
    aContext.PatternTrie.VersionedFunctions.Add(aVersionedXboxLibraryFunction);
end; // ParseVersionedXboxLibraryFunction

procedure ParseAndAppendPatternsToTrie(const aPatterns: PAnsiChar;
  const aPatternTrie: TPatternTrie;
  const aOnlyPatches: Boolean;
  const aLibrary: PVersionedXboxLibrary);
var
  PatternScanningContext: RPatternScanningContext;
  i: Integer;
  VersionedXboxLibraryFunction: PVersionedXboxLibraryFunction;
begin
  // Set up the context for the line-parsing routine :
  PatternScanningContext.PatternTrie := aPatternTrie;
  PatternScanningContext.OnlyPatches := aOnlyPatches;
  PatternScanningContext.VersionedXboxLibrary := aLibrary;
  PatternScanningContext.FunctionList := TStringList.Create;
  PatternScanningContext.ScanPart := spPatterns;
  try
    // Quickly scan all pattern lines and add them to the FunctionList :
    ScanPCharLines(aPatterns, @ParseAndAppendPatternLineToTrie_Callback, @PatternScanningContext);

    // Handle each function in this list :
    PatternScanningContext.FunctionList.Sorted := True;
    for i := PatternScanningContext.FunctionList.Count - 1 downto 0 do
    begin
      VersionedXboxLibraryFunction := Pointer(PatternScanningContext.FunctionList.Objects[i]);
      if ParseVersionedXboxLibraryFunction(VersionedXboxLibraryFunction, @PatternScanningContext) then
        SetLength(VersionedXboxLibraryFunction.ScanParts, 0)
      else
      begin
        PatternScanningContext.FunctionList.Delete(i);
        Dispose(VersionedXboxLibraryFunction);
      end;
    end;

  finally
    FreeAndNil(PatternScanningContext.FunctionList);
  end;
end; // ParseAndAppendPatternsToTrie

function GeneratePatternTrie(const aFileList: TStrings; const aOnlyPatches: Boolean): Integer;

  procedure _ProcessPatternFiles(aPatternTrie: TPatternTrie);
  var
    Input: TMemoryStream;
    i: Integer;
    FilePath, LibName, VersionStr: string;
    LibVersion: Integer;
    VersionedXboxLibrary: PVersionedXboxLibrary;
  begin
    Input := TMemoryStream.Create;
    try
      for i := 0 to aFileList.Count - 1 do
      begin
        // Get the Pattern FilePath, and figure out for which library and LibVersion it is :
        FilePath := aFileList[i];
        LibName := ChangeFileExt(ExtractFileName(FilePath), '');

        LibVersion := 0;
        while (LibName <> '') and IsDigit(LibName[1]) do
        begin
          LibVersion := (LibVersion * 10) + Ord(LibName[1]) - Ord('0');
          Delete(LibName, 1, 1);
        end;

        if LibVersion = 0 then
        begin
          // Get LibVersion string from the last few digits of the parent folder name :
          VersionStr := ExtractFileDir(FilePath);
          LibVersion := Length(VersionStr);
          while (LibVersion > 0) and IsDigit(VersionStr[LibVersion]) do
            Dec(LibVersion);

          Delete(VersionStr, 1, LibVersion);
          LibVersion := StrToIntDef(VersionStr, 0);
        end;

        Assert(LibName <> '');
        Assert(LibVersion > 0);

        WriteLn('Processing "', LibName, '" library, version ', LibVersion, '...');

        // Create and initialize this new library :
        VersionedXboxLibrary := AllocMem(SizeOf(RVersionedXboxLibrary));
        VersionedXboxLibrary.LibVersion := LibVersion;
        VersionedXboxLibrary.LibVersionFlag := LibraryVersionNumberToFlag(LibVersion);
        VersionedXboxLibrary.LibName := LibName;
        VersionedXboxLibrary.LibFlags := [];

        aPatternTrie.VersionedLibraries.Add(VersionedXboxLibrary);

        // If the file is a .lib :
        if SameText(ExtractFileExt(FilePath), '.lib') then
        begin
          // Dump it with link.exe and convert that to pattern-format :
          RunLibDump(FilePath, Input);
          ConvertDumpToPattern(Input);
          Input.SaveToFile(ChangeFileExt(FilePath, '.pat'));
        end
        else
          // Load pattern file into memory (assuming the file ext is .pat) :
          Input.LoadFromFile(FilePath);

        // Parse patterns in this file and append them to the trie :
        ParseAndAppendPatternsToTrie(Input.Memory, aPatternTrie, aOnlyPatches, VersionedXboxLibrary);
      end;
    finally
      FreeAndNil(Input);
    end;
  end; // _ProcessPatternFiles

var
  PatternTrie: TPatternTrie;
  Output: TStringList;
  i: Integer;
begin
//  Result := -1;

  PatternTrie := TPatternTrie.Create;
  PatternTrie.VersionedLibraries := TList.Create;
  PatternTrie.VersionedFunctions := TList.Create;
  try
    WriteLn('Adding ', aFileList.Count, ' files to trie...');
    _ProcessPatternFiles(PatternTrie);

    WriteLn('Sorting the trie...');
    PatternTrie.Sort;

    WriteLn('Trie contains ', PatternTrie.VersionedFunctions.Count, ' unique patterns.');

    if DumpFileName <> '' then
    begin
      WriteLn('Dumping the trie to "', ExpandFileName(DumpFileName), '"...');
      Output := TStringList.Create;
      try
        PatternTrie.Dump(Output);
        Output.SaveToFile(ExpandFileName(DumpFileName));
      finally
        FreeAndNil(Output);
      end;
    end;

    WriteLn('Saving the trie to "', ExpandFileName(StoredTrieFileName), '"...');
    PatternTrie.Save(ExpandFileName(StoredTrieFileName));

    Result := ERROR_SUCCESS;
  finally
    for i := 0 to PatternTrie.VersionedFunctions.Count - 1 do
      FreeMem(PatternTrie.VersionedFunctions[i]);
    FreeAndNil(PatternTrie.VersionedFunctions);

    for i := 0 to PatternTrie.VersionedLibraries.Count - 1 do
      FreeMem(PatternTrie.VersionedLibraries[i]);
    FreeAndNil(PatternTrie.VersionedLibraries);
    FreeAndNil(PatternTrie);
  end;
end; // GeneratePatternTrie

procedure PatternToTrie_Main;
var
  LibPath: string;
  LibFiles: TStringList;
  i: Integer;
begin
  WriteLn(ChangeFileExt(ExtractFileName(ParamStr(0)), ''), ' - Creates a trie of Xbox1 library symbols.');
  WriteLn('');

  if ParamStr(1) = '/?' then
  begin
    WriteLn('This tool is part of the Dxbx Xbox emulator project,');
    WriteLn('and compiles a binary file containing all data needed');
    WriteLn('for Dxbx to do automated symbol detection.');
    WriteLn('');
    WriteLn('When reading from .lib files, we parse the output');
    WriteLn('of Link.exe [(c) Microsoft], creating pattern files');
    WriteLn('that resemble the pat files generated by the IDA Pro');
    WriteLn('Flirt tools pcf.exe [(c) Hex-Rays].');
    WriteLn('When lacking .lib files, the .pat files themselves are used.');
    WriteLn('');
    WriteLn('Usage:');
    WriteLn('> PatternTrieBuilder.exe [Folder|.lib|.pat file]+');
    Exit;
  end;

  if ParamCount < 1 then
  begin
    WriteLn('ERROR! Missing argument. Supply a folder with Xbox1 .lib or .pat files,');
    WriteLn('and/or the .lib and .pat files themselves.');
    Exit;
  end;

  LibFiles := TStringList.Create;
  try
    // Collect all .lib files specified on the command-line :
    for i := 1 to ParamCount do
    begin
      LibPath := ExpandFileName(ParamStr(i));
      if IsFolder(LibPath) then
      begin
        // Pick up all .lib files from the specified folder :
        FindFiles(LibPath, {aFileMask=}'*.lib', LibFiles);
        // Pick up all .pat files from the specified folder :
        FindFiles(LibPath, {aFileMask=}'*.pat', LibFiles);
      end
      else
      begin
        // Add explicitly specified .lib and .pat files :
        if SameText(ExtractFileExt(LibPath), '.lib')
        or SameText(ExtractFileExt(LibPath), '.pat') then
          LibFiles.Add(LibPath)
        else
          // Warn about unsupported files :
          WriteLn('WARNING! File type not recognized : "', ParamStr(i), '"!');
      end;
    end; // for

    // Check that we have files to work with :
    if LibFiles.Count = 0 then
    begin
      WriteLn('ERROR! No ".lib" or ".pat" files found!');
      Exit;
    end;

    // See if we need to handle a .lib file :
    for i := 0 to LibFiles.Count - 1 do
    begin
      if SameText(ExtractFileExt(LibFiles[i]), '.lib') then
      begin
        // In that case, we need to have access to link.exe :
        if LocateLink then
        begin
          WriteLn('Using "', LinkPath, '"');
          WriteLn('and "', mspdb80Path, '".');
          WriteLn('');
          // No need to search any further :
          Break;
        end;

        // No link.exe (or mspdb80.dll) found - show error and stop :
        if LinkPath = '' then
          WriteLn('ERROR! Cannot find Link.exe!');
        if mspdb80Path = '' then
          WriteLn('ERROR! Cannot find mspdb80.dll!');
        WriteLn('');
        WriteLn('Please install Microsoft Visual Studio 2003 or later (or copy');
        WriteLn('Link.exe and the accompanying mspdb80.dll in your searchpath).');
        Exit;
      end;
    end;

    // Parse all files and generate a pattern-trie from it :
    ExitCode := GeneratePatternTrie(LibFiles, {aOnlyPatches=}False);
    if ExitCode = ERROR_SUCCESS then
    begin
      WriteLn('Done. Press enter to quit.');
      ReadLn;
    end
    else
      WriteLn('ERROR! No trie file generated!');

  finally
    FreeAndNil(LibFiles);
  end;
end; // PatternToTrie_Main

end.

