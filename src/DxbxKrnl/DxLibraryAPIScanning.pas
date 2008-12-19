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
unit DxLibraryAPIScanning;

{$DEFINE _OPTIMIZE_UNIT}

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  Classes,
  SysUtils,
  Contnrs,
  // 3rd Party
  JclWin32, // UNDNAME_COMPLETE
  JclPeImage, // UndecorateSymbolName
  // Dxbx
  uTypes,
  uXBE,
  uLog,
  uStoredTrieTypes,
  uXboxLibraryUtils,
  uCRC16,
  uEmuXapi; // XTL_EmuXapiProcessHeap

type
  PDetectedVersionedXboxLibraryFunction = ^RDetectedVersionedXboxLibraryFunction;
  RDetectedVersionedXboxLibraryFunction = record
    XboxLibraryPatch: TXboxLibraryPatch;
    FunctionName: string;
    StoredLibraryFunction: PStoredLibraryFunction;
//    PotentialLocations: array of RDetectedLocation = record CodeStart, CodeEnd: TCodePointer; end;
    HitCount: Integer;
    CodeStart: TCodePointer;
    CodeEnd: TCodePointer;
  end;

  TDetectedFunctions = class(TObject)
  protected
    FList: TList;
    function GetItem(Index: Integer): PDetectedVersionedXboxLibraryFunction;
  public
    ActualXboxLibraries: TList;

    function Count: Integer;
    property Items[Index: Integer]: PDetectedVersionedXboxLibraryFunction read GetItem; default;
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function New(const aFunctionName: string): PDetectedVersionedXboxLibraryFunction;
    function FindByName(const aName: string): PDetectedVersionedXboxLibraryFunction;
    function FindByAddress(const aAddress: TCodePointer): PDetectedVersionedXboxLibraryFunction;
  end;

procedure DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);

var
  DetectedFunctions: TDetectedFunctions;

implementation

function TestAddressUsingPatternTrie(const aPatternTrieReader: TPatternTrieReader; const aAddress: PByte): PStoredLibraryFunction;

  function _TryMatchingLeaf(const aStoredLibraryFunction: PStoredLibraryFunction; aAddress: PByte): PStoredLibraryFunction;
  var
    StoredLibrary: PStoredLibrary;
    i: Integer;
    VersionedXboxLibrary: PVersionedXboxLibrary;
  begin
    Result := aStoredLibraryFunction;
{$IFDEF DXBX_RECTYPE}
    Assert(aStoredLibraryFunction.RecType = rtStoredLibraryFunction, 'StoredLibraryFunction type mismatch!');
{$ENDIF}
    if aStoredLibraryFunction.CRCLength > 0 then
    begin
      if aStoredLibraryFunction.CRCValue <> CalcCRC16(aAddress, aStoredLibraryFunction.CRCLength) then
      begin
        Result := nil;
        Exit;
      end;
    end;

    // TODO : Include data & test-code for : Cross-referenced APIs
    // TODO : Include data & test-code for : Trailing bytes

    // Get the Stored Library associated with this pattern :
    StoredLibrary := aPatternTrieReader.GetStoredLibrary(aStoredLibraryFunction.LibraryIndex);

    // Determine if this matches one of the actual libraries :
    for i := 0 to DetectedFunctions.ActualXboxLibraries.Count - 1 do
    begin
      VersionedXboxLibrary := DetectedFunctions.ActualXboxLibraries[i];
      if SameText(VersionedXboxLibrary.LibName, aPatternTrieReader.GetString(StoredLibrary.LibNameIndex)) then
        Exit;
    end;

    // No matching library, skip this :
    Result := nil;
  end; // _TryMatchingLeaf

  function _TryMatchingNode(aStoredTrieNode: PStoredTrieNode; aAddress: PByte; Depth: Integer): PStoredLibraryFunction;
  var
    NrChildren: Integer;
    StretchPtr: PByte;
    StretchHeaderByte: TStretchHeaderByte;
    More: Boolean;
    NrFixed, NrWildcards, i: Integer;
    NextOffset: TByteOffset;
    StoredLibraryFunction: PStoredLibraryFunction;
  begin
    Result := nil;
{$IFDEF DXBX_RECTYPE}
    Assert(aStoredTrieNode.RecType = rtStoredTrieNode, 'StoredTrieNode type mismatch!');
{$ENDIF}
    // Calculate the position of the data after this TreeNode (StretchPtr) :
    NrChildren := aStoredTrieNode.NrChildrenByte1;
    IntPtr(StretchPtr) := IntPtr(aStoredTrieNode) + SizeOf(RStoredTrieNode);
    if NrChildren >= 128 then
      // Reconstruct the NrChildren value :
      NrChildren := (Integer(aStoredTrieNode.NrChildrenByte1 and 127) shl 8) or aStoredTrieNode.NrChildrenByte2
    else
      // If one byte was sufficient, then the next stretch starts 1 byte earlier :
      Dec(IntPtr(StretchPtr), SizeOf({RStoredTrieNode.NrChildrenByte2:}Byte));

    // Scan all stretches after this node :
    repeat
      StretchHeaderByte := StretchPtr^;
      Inc(StretchPtr);

      // Determine if there are more stretches after this one :
      More := (StretchHeaderByte and NODE_FLAG_MORE) > 0;
      // Determine how many wildcard bytes need to be skipped :
      NrFixed := StretchHeaderByte shr NODE_NR_FIXED_SHIFT;
      case StretchHeaderByte and NODE_TYPE_MASK of
        NODE_5BITFIXED_4WILDCARDS: NrWildcards := 4;
        NODE_5BITFIXED_8WILDCARDS: NrWildcards := 8;
        NODE_5BITFIXED_ALLWILDCARDS:
          if (StretchHeaderByte and NODE_TYPE_MASK_EXTENDED) = NODE_ALLFIXED then
          begin
            NrFixed := PATTERNSIZE;
            NrWildcards := 0;
            More := False;
          end
          else
            NrWildcards := PATTERNSIZE - (Depth + NrFixed);
      else // NODE_5BITFIXED_0WILDCARDS:
        NrWildcards := 0;
      end;

      // Check if all fixed bytes match :
      for i := 0 to NrFixed - 1 do
      begin
        if aAddress^ <> StretchPtr^ then
          Exit;

        Inc(aAddress);
        Inc(StretchPtr);
      end;

      // If stretch was hit, update depth and search-address for the next stretch :
      Inc(Depth, NrFixed);
      Inc(Depth, NrWildcards);
      Inc(aAddress, NrWildcards);

    until not More;

    // When we're at the end of the pattern :
    if Depth >= PATTERNSIZE then
    begin
      // Handle all children leafs here, searching for the best-fit lib-version :
      StoredLibraryFunction := Pointer(StretchPtr);
      while NrChildren > 0 do
      begin
        Result := _TryMatchingLeaf(StoredLibraryFunction, aAddress);
        if Assigned(Result) then
          Exit;

        Inc(IntPtr(StoredLibraryFunction), SizeOf(RStoredLibraryFunction));
        Dec(NrChildren);
      end;

      Exit;
    end; // if Depth

    aStoredTrieNode := Pointer(StretchPtr);
    while NrChildren > 0 do
    begin
      // Try to match pattern on this node
      Result := _TryMatchingNode(aStoredTrieNode, aAddress, Depth);
      if Assigned(Result) then
        Exit;

      // Try next child, maybe that helps:
      NextOffset := aStoredTrieNode.NextSiblingOffset;
      // Sanity-check on next-offset :
      if (NextOffset <= 0) or (NextOffset > 100*1024*1024) then
        Break;

      // Jump to next sibling :
      aStoredTrieNode := aPatternTrieReader.GetNode(NextOffset);
      Dec(NrChildren);
    end;

    Result := nil;
  end; // _TryMatchingNode

var
  Node: PStoredTrieNode;
begin
  // Search if this address matches a pattern :
  Node := aPatternTrieReader.GetNode(aPatternTrieReader.StoredSignatureTrieHeader.TrieRootNode);

  Result := _TryMatchingNode(Node, aAddress, 0);
end; // TestAddressUsingPatternTrie


procedure DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);
var
  ByteScanLower, ByteScanUpper: PByte;

  procedure _DetectVersionedXboxLibraries(const aPatternTrieReader: TPatternTrieReader);
  var
    CurrentXbeLibraryVersion: PXBE_LIBRARYVERSION;
    CurrentLibName: string;
    StoredLibrary, BestFit: PStoredLibrary;
    BestDist, ThisDist: Integer;
    StoredLibraryName: string;
    i, j: Integer;
    VersionedXboxLibrary: PVersionedXboxLibrary;
  begin
    // Loop over all libraries :
    CurrentXbeLibraryVersion := pLibraryVersion;
    if not Assigned(CurrentXbeLibraryVersion) then
    begin
      DbgPrintf('DxbxHLE : No XBE library versions to scan!');
      Exit;
    end;

    // Loop over all library versions in the executable:
    for i := 0 to pXbeHeader.dwLibraryVersions - 1 do
    begin
      CurrentLibName := Copy(CurrentXbeLibraryVersion.szName, 1, 8);
      DbgPrintf('DxbxHLE : Library "%s" is version %d', [CurrentLibName, CurrentXbeLibraryVersion.wBuildVersion]);

      // Find the library version in our pattern trie that best matches this :
      BestFit := nil;
      BestDist := Low(BestDist);
      for j := 0 to aPatternTrieReader.StoredSignatureTrieHeader.LibraryTable.NrOfLibraries - 1 do
      begin
        StoredLibrary := aPatternTrieReader.GetStoredLibrary(j);
        StoredLibraryName := aPatternTrieReader.GetString(StoredLibrary.LibNameIndex);

        // Only consider libraries with exactly the same name :
        if SameText(StoredLibraryName, CurrentLibName) then
        begin
          // Distance : 0 on exact hit, positive when more recent, negative when older;
          // More recent is better than older version; The closer to 0, the better.
          ThisDist := Integer(StoredLibrary.LibVersion) - Integer(CurrentXbeLibraryVersion.wBuildVersion);
          // Use this library when no other are found yet :
          if (BestFit = nil)
          // Or when the this version comes closer to the actual version :
          or (ThisDist = 0)
          or ((BestDist < 0) and (ThisDist > 0))
          or ((BestDist < 0) and (ThisDist < 0) and (ThisDist > BestDist))
          or ((BestDist > 0) and (ThisDist > 0) and (ThisDist < BestDist)) then
          begin
            BestFit := StoredLibrary;
            BestDist := ThisDist;
          end;
        end;
      end;

      if Assigned(BestFit) then
      begin
        if BestFit.LibVersion = CurrentXbeLibraryVersion.wBuildVersion then
          DbgPrintf('... Got patterns for exactly this version!')
        else
          DbgPrintf('... Approximating this with patterns from library %d.', [BestFit.LibVersion]);

        // Add this library to a list we'll use in the detection-code :
        VersionedXboxLibrary := AllocMem(SizeOf(RVersionedXboxLibrary));
{$IFDEF DXBX_RECTYPE}
        VersionedXboxLibrary.RecType := rtVersionedXboxLibrary;
{$ENDIF}
        VersionedXboxLibrary.LibVersion := BestFit.LibVersion;
        VersionedXboxLibrary.LibName := aPatternTrieReader.GetString(BestFit.LibNameIndex);
        DetectedFunctions.ActualXboxLibraries.Add(VersionedXboxLibrary);
      end
      else
        DbgPrintf('... No patterns registered for this library!');

      // Skip to the next library :
      Inc(CurrentXbeLibraryVersion);
    end;
  end; // _DetectVersionedXboxLibraries

  function _FindAndRememberPattern(const aPatternTrieReader: TPatternTrieReader; const aAddress: PByte): PStoredLibraryFunction;
  var
    FunctionName: string;
    Detected: PDetectedVersionedXboxLibraryFunction;
    Unmangled: string;

    function DxbxUnmangleSymbolName(const aStr: string): string;
    var
      UnmangleFlags: DWord;
      i: Integer;
    begin
      if aStr = '' then
        Exit;

      Result := aStr;

      // Check if the symbol starts with an underscore ('_') or '@':
      case Result[1] of
        '?':
          begin
            UnmangleFlags := 0
                          // UNDNAME_COMPLETE               // Enable full undecoration
                          or UNDNAME_NO_LEADING_UNDERSCORES // Remove leading underscores from MS extended keywords
                          or UNDNAME_NO_MS_KEYWORDS         // Disable expansion of MS extended keywords
                          or UNDNAME_NO_FUNCTION_RETURNS    // Disable expansion of return type for primary declaration
                          or UNDNAME_NO_ALLOCATION_MODEL    // Disable expansion of the declaration model
                          or UNDNAME_NO_ALLOCATION_LANGUAGE // Disable expansion of the declaration language specifier
                          or UNDNAME_NO_MS_THISTYPE         // NYI Disable expansion of MS keywords on the 'this' type for primary declaration
                          or UNDNAME_NO_CV_THISTYPE         // NYI Disable expansion of CV modifiers on the 'this' type for primary declaration
                          or UNDNAME_NO_THISTYPE            // Disable all modifiers on the 'this' type
                          or UNDNAME_NO_ACCESS_SPECIFIERS   // Disable expansion of access specifiers for members
                          or UNDNAME_NO_THROW_SIGNATURES    // Disable expansion of 'throw-signatures' for functions and pointers to functions
                          or UNDNAME_NO_MEMBER_TYPE         // Disable expansion of 'static' or 'virtual'ness of members
                          or UNDNAME_NO_RETURN_UDT_MODEL    // Disable expansion of MS model for UDT returns
                          or UNDNAME_32_BIT_DECODE          // Undecorate 32-bit decorated names
                          or UNDNAME_NAME_ONLY              // Crack only the name for primary declaration;
                          or UNDNAME_NO_ARGUMENTS           // Don't undecorate arguments to function
                          or UNDNAME_NO_SPECIAL_SYMS        // Don't undecorate special names (v-table, vcall, vector xxx, metatype, etc)
                          ;

            // Do Microsoft symbol demangling :
            if not UndecorateSymbolName(aStr, {var}Result, UnmangleFlags) then
              Result := aStr;
          end;
        '_', '@':
        begin
          // Remove this leading character :
          Delete(Result, 1, 1);
          // Replace all following underscores with a dot ('.') :
          Unmangled := StringReplace(Unmangled, '_', '.', [rfReplaceAll]);
        end;
      end;

      // Remove everything from '@' onward :
      i := Pos('@', Result);
      if i > 1 then
        Delete(Result, i, MaxInt);

      // Replace '::' with '.' :
      Result := StringReplace(Result, '::', '.', [rfReplaceAll]);
    end; // DxbxUnmangleSymbolName

    function _DetermineJmpAddress(const aStartingAddress: PByte; const aOffset: Word): PByte;
    begin
      IntPtr(Result) := IntPtr(aStartingAddress) + IntPtr(aOffset);
      Result := PByte(IntPtr(Result) + PInteger(Result)^ + 4);
    end;

    function _HandleCrossReference(const aStartingAddress: PByte;
      const aCrossReferenceOffset: Word; const aCrossReferenceNameIndex: TStringTableIndex): Boolean;
    var
      CrossReferenceAddress: PByte;
      CrossReferenced: PStoredLibraryFunction;
    begin
      // First check : Do we have a cross-reference?
      Result := (aCrossReferenceNameIndex = NO_STRING_INDEX);
      if Result then
        Exit;

      // Use aCrossReferenceOffset to determine
      // the call-location that should be checked :
      CrossReferenceAddress := _DetermineJmpAddress(aStartingAddress, aCrossReferenceOffset);

      // First check : does this address reside in the executable segment?
      if (IntPtr(CrossReferenceAddress) < IntPtr(ByteScanLower)) or (IntPtr(CrossReferenceAddress) > IntPtr(ByteScanUpper)) then
        Exit;

      // See if we can find a function on this address :
      CrossReferenced := TestAddressUsingPatternTrie(aPatternTrieReader, CrossReferenceAddress);
      if CrossReferenced = nil then
        Exit;

      // Check if this function is indeed the one mentioned, by comparing
      // the string-indexes of both functions (they should match) :
      Result := aCrossReferenceNameIndex =
                aPatternTrieReader.GetGlobalFunction(CrossReferenced.GlobalFunctionIndex).FunctionNameIndex;
    end;

  begin
    // Search if this address matches a pattern :
    Result := TestAddressUsingPatternTrie(aPatternTrieReader, aAddress);
    if Result = nil then
      Exit;

    // Now that it's found, see if it was already registered :
    FunctionName := aPatternTrieReader.GetFunctionName(Result.GlobalFunctionIndex);
    Detected := DetectedFunctions.FindByName(FunctionName);
    if Assigned(Detected) then
    begin
      // Count the number of times it was found (should stay at 1) :
      Inc(Detected.HitCount);
      Exit;
    end;

    // Handle a possible cross-reference check :
    if not _HandleCrossReference(aAddress, Result.CrossReference1Offset, Result.CrossReference1NameIndex) then
      Exit;

    // Do our own demangling :
    Unmangled := DxbxUnmangleSymbolName(FunctionName);

    // Newly detected functions are registered here (including their range,
    // which will come in handy when debugging) :
    Detected := DetectedFunctions.New(Unmangled);
    Detected.StoredLibraryFunction := Result;
    Detected.CodeStart := TCodePointer(aAddress);
    Detected.CodeEnd := TCodePointer(IntPtr(aAddress) + Result.FunctionLength);
    Detected.HitCount := 1;

{$IFDEF DXBX_DEBUG}
    DbgPrintf('DxbxHLE : Detected at $%.8x : ''%s'' (was "%s")', [aAddress, Unmangled, FunctionName]);
    // string(XboxLibraryPatchToString(Detected.XboxLibraryPatch))
{$ENDIF}
  end; // _FindAndRememberPattern

  procedure _ScanMemoryRangeForLibraryPatterns(const ByteScanLower, ByteScanUpper: PByte;
    const aPatternTrieReader: TPatternTrieReader);
  var
    p: PByte;
  begin
{$IFDEF DXBX_DEBUG}
    DbgPrintf('DxbxHLE : Detecting functions from $%.8x to $%.8x', [ByteScanLower, ByteScanUpper]);
{$ENDIF}
    p := ByteScanLower;
    while p <> ByteScanUpper do
    begin
      try
        _FindAndRememberPattern(aPatternTrieReader, p);
      except
{$IFDEF DXBX_DEBUG}
        DbgPrintf('DxbxHLE : Exception while scanning on address $%.8x', [p]);
{$ENDIF}
      end;

      Inc(p);
    end;
  end; // _ScanMemoryRangeForLibraryPatterns

  procedure _ResolveXapiProcessHeapAddress(const aPatternTrieReader: TPatternTrieReader);
  var
    DetectedXapiInitProcess: PDetectedVersionedXboxLibraryFunction;
    StoredLibrary: PStoredLibrary;
    ProcessHeapOffs: Integer;
  begin
    DetectedXapiInitProcess := DetectedFunctions.FindByName('XapiInitProcess');
    if Assigned(DetectedXapiInitProcess) then
    begin
      StoredLibrary := aPatternTrieReader.GetStoredLibrary(DetectedXapiInitProcess.StoredLibraryFunction.LibraryIndex);
      if Assigned(StoredLibrary) then
      begin
        // Source for these offsets is Cxbx code (HLEIntercept.cpp) and our
        // Xapi library patterns - search in the definition of _XapiInitProcess@0
        // for the offset to the '_XapiProcessHeap' cross-reference.
        
        if (StoredLibrary.LibVersion >= 5849) then
          ProcessHeapOffs := $51
        else if (StoredLibrary.LibVersion >= 5558) then
          ProcessHeapOffs := $51
        else if (StoredLibrary.LibVersion >= 4928) then
          ProcessHeapOffs := $44
        else if (StoredLibrary.LibVersion >= 4361) then
          ProcessHeapOffs := $3E
        else // 3911, 4034, 4134
          ProcessHeapOffs := $3E;

        XTL_EmuXapiProcessHeap := PPointer(IntPtr(DetectedXapiInitProcess.CodeStart) + ProcessHeapOffs)^;
{$IFDEF DXBX_DEBUG}
        DbgPrintf('DxbxHLE : Resolved XapiProcessHeap at $%.8x', [XTL_EmuXapiProcessHeap]);
{$ENDIF}
      end;
    end;
  end; // _ResolveXapiProcessHeapAddress

var
  ResourceStream: TResourceStream;
  PatternTrieReader: TPatternTrieReader;
{$IFDEF DXBX_DEBUG}
  i: Integer;
{$ENDIF}
begin
  ByteScanLower := PByte(pXbeHeader.dwBaseAddr);
  ByteScanUpper := PByte(IntPtr(ByteScanLower) + Integer(pXbeHeader.dwSizeofImage));

  DetectedFunctions.Clear;

  // Get StoredPatternTrie from resource :
  ResourceStream := TResourceStream.Create(LibModuleList.ResInstance, 'StoredPatternTrie', RT_RCDATA);
  try
    PatternTrieReader := TPatternTrieReader.Create;
    try
      PatternTrieReader.LoadFromStream(ResourceStream);

      _DetectVersionedXboxLibraries(PatternTrieReader);

      // Scan Patterns using this trie :
      _ScanMemoryRangeForLibraryPatterns(ByteScanLower, ByteScanUpper, PatternTrieReader);

      // Resolve the address of _XapiProcessHeap :
      _ResolveXapiProcessHeapAddress(PatternTrieReader);

    finally
      FreeAndNil(PatternTrieReader);
    end;

  finally
    // Unlock the resource :
    FreeAndNil(ResourceStream);
  end;

{$IFDEF DXBX_DEBUG}
  // Show a list of detected functions with a HitCount > 1 :
  for i := 0 to DetectedFunctions.Count - 1 do
    if DetectedFunctions[i].HitCount > 1 then
      DbgPrintf('DxbxHLE : Duplicate %.3d hits on ''%s'' ', [DetectedFunctions[i].HitCount, DetectedFunctions[i].FunctionName]);
    // string(XboxLibraryPatchToString(Detected.XboxLibraryPatch))
{$ENDIF}
  DbgPrintf('DxbxHLE : Detected functions : %d.', [DetectedFunctions.Count]);
end; // DxbxScanForLibraryAPIs

{ TDetectedFunctions }

constructor TDetectedFunctions.Create;
begin
  inherited Create;

  FList := TList.Create;
  ActualXboxLibraries := TList.Create;
end;

destructor TDetectedFunctions.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  FreeAndNil(ActualXboxLibraries);

  inherited Destroy;
end;

function TDetectedFunctions.Count: Integer;
begin
  Result := FList.Count;
end;

function TDetectedFunctions.GetItem(Index: Integer): PDetectedVersionedXboxLibraryFunction;
begin
  Result := PDetectedVersionedXboxLibraryFunction(FList[Index]);
end;

procedure TDetectedFunctions.Clear;
var
  i: Integer;
  DetectedFunction: PDetectedVersionedXboxLibraryFunction;
begin
  for i := 0 to FList.Count - 1 do
  begin
    DetectedFunction := Items[i];
    Dispose(DetectedFunction);
  end;

  FList.Clear;

  ActualXboxLibraries.Clear;
end;

function TDetectedFunctions.New(const aFunctionName: string): PDetectedVersionedXboxLibraryFunction;
begin
  Result := AllocMem(SizeOf(RDetectedVersionedXboxLibraryFunction));
  Result.FunctionName := aFunctionName;
  Result.XboxLibraryPatch := XboxFunctionNameToLibraryPatch(aFunctionName);
  FList.Add(Result);
end;

function TDetectedFunctions.FindByName(const aName: string): PDetectedVersionedXboxLibraryFunction;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Result := Items[i];
    if SameText(Result.FunctionName, aName) then
      Exit;
  end;

  Result := nil;
end;

function TDetectedFunctions.FindByAddress(const aAddress: TCodePointer): PDetectedVersionedXboxLibraryFunction;
var
  i: Integer;
begin
  // TODO : For speed, usse a binary search here (which needs an address-ordered collection)
  for i := 0 to FList.Count - 1 do
  begin
    Result := Items[i];
    if (IntPtr(Result.CodeStart) <= IntPtr(aAddress)) and (IntPtr(aAddress) <= IntPtr(Result.CodeEnd)) then
      Exit;
  end;

  Result := nil;
end;

initialization

  DetectedFunctions := TDetectedFunctions.Create;

finalization

  FreeAndNil(DetectedFunctions);

end.
