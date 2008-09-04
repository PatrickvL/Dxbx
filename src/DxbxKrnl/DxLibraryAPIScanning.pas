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
  // Dxbx
  uTypes,
  uXBE,
  uLog,
  uStoredTrieTypes,
  uXboxLibraryUtils,
  uCRC16;

type
  PDetectedVersionedXboxLibraryFunction = ^RDetectedVersionedXboxLibraryFunction;
  RDetectedVersionedXboxLibraryFunction = record
    XboxLibraryPatch: TXboxLibraryPatch;
    FunctionName: string;
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
  end;

  function _TryMatchingNode(aStoredTrieNode: PStoredTrieNode; aAddress: PByte; Depth: Integer): PStoredLibraryFunction;
  var
    NrChildren: Integer;
    StretchPtr: PByte;
    StretchHeader: TStretchHeaderByte;
    More: Boolean;
    NrFixed, NrWildcards, i: Integer;
    NextOffset: TByteOffset;
    StoredLibraryFunction: PStoredLibraryFunction;
  begin
    Result := nil;
    NrChildren := aStoredTrieNode.NrChildrenByte1;
    IntPtr(StretchPtr) := IntPtr(aStoredTrieNode) + SizeOf(RStoredTrieNode);
    if NrChildren >= 128 then
      // Reconstruct the NrChildren value :
      NrChildren := (Integer(aStoredTrieNode.NrChildrenByte1 and 127) shl 8) or aStoredTrieNode.NrChildrenByte2
    else
      // If one byte was sufficient, then the stretch starts 1 byte earlier too :
      Dec(IntPtr(StretchPtr));

    repeat
      StretchHeader := StretchPtr^;
      Inc(StretchPtr);

      More := (StretchHeader and 4) > 0;
      NrFixed := StretchHeader shr 3;
      case StretchHeader and 3 of
        NODE_5BITFIXED_4WILDCARDS: NrWildcards := 4;
        NODE_5BITFIXED_8WILDCARDS: NrWildcards := 8;
        NODE_5BITFIXED_ALLWILDCARDS:
          if (StretchHeader and 7) = NODE_ALLFIXED then
          begin
            NrFixed := 32;
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

    if Depth = PATTERNSIZE then
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
    end;

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
  end;

var
  Node: PStoredTrieNode;
begin
  // Search if this address matches a pattern :
  Node := aPatternTrieReader.GetNode(aPatternTrieReader.StoredSignatureTrieHeader.TrieRootNode);

  Result := _TryMatchingNode(Node, aAddress, 0);
end; // TestAddressUsingPatternTrie


procedure DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);

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
        VersionedXboxLibrary.LibVersion := BestFit.LibVersion;
        VersionedXboxLibrary.LibName := aPatternTrieReader.GetString(BestFit.LibNameIndex);;
        DetectedFunctions.ActualXboxLibraries.Add(VersionedXboxLibrary);
      end
      else
        DbgPrintf('... No patterns registered for this library!');

      // Skip to the next library :
      Inc(CurrentXbeLibraryVersion);
    end;
  end;

  function _FindAndRememberPattern(const aPatternTrieReader: TPatternTrieReader; const aAddress: PByte): PStoredLibraryFunction;
  var
    FunctionName: string;
    Detected: PDetectedVersionedXboxLibraryFunction;
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

    // Newly detected functions are registered here (including their range,
    // which will come in handy when debugging) :
    Detected := DetectedFunctions.New(FunctionName);
    Detected.CodeStart := TCodePointer(aAddress);
    Detected.CodeEnd := TCodePointer(IntPtr(aAddress) + Result.FunctionLength);
    Detected.HitCount := 1;

{$IFDEF DXBX_DEBUG}
    DbgPrintf('DxbxHLE : 0x%.8x -> %s [%s]', [aAddress, Detected.FunctionName, string(XboxLibraryPatchToString(Detected.XboxLibraryPatch))]);
{$ENDIF}
  end;

  procedure _ScanMemoryRangeForLibraryPatterns(const ByteScanLower, ByteScanUpper: PByte;
    const aPatternTrieReader: TPatternTrieReader);
  var
    p: PByte;
  begin
    p := ByteScanLower;
    while p <> ByteScanUpper do
    begin
      _FindAndRememberPattern(aPatternTrieReader, p);
      Inc(p);
    end;
  end;

var
  ByteScanLower, ByteScanUpper: PByte;
  ResourceStream: TResourceStream;
  PatternTrieReader: TPatternTrieReader;
//  i, j, PrevCount: Integer;
//  CurrentXbeLibraryVersion: PXBE_LIBRARYVERSION;
//  CurrentLibName: string;
//  CurrentVersionedXboxLibrary: PVersionedXboxLibrary;
begin
  ByteScanLower := PByte(pXbeHeader.dwBaseAddr);
  ByteScanUpper := PByte(IntPtr(ByteScanLower) + pXbeHeader.dwSizeofImage);

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

    finally
      FreeAndNil(PatternTrieReader);
    end;

  finally
    // Unlock the resource :
    FreeAndNil(ResourceStream);
  end;

(*
  // Loop over all libraries :
  CurrentXbeLibraryVersion := pLibraryVersion;
  if not Assigned(CurrentXbeLibraryVersion) then
  begin
    DbgPrintf('DxbxHLE : No XBE library versions to scan!');
    Exit;
  end;

  // Loop over all library versions :
  PrevCount := 0;
  for i := 0 to pXbeHeader.dwLibraryVersions - 1 do
  begin
    CurrentLibName := Copy(CurrentXbeLibraryVersion.szName, 1, 8);
    DbgPrintf('DxbxHLE : Library "%s" is version %d', [CurrentLibName, CurrentXbeLibraryVersion.wBuildVersion]);

    // Find the patterns the fit this library exactly (version and name) :
    for j := 0 to Length(AllXboxLibraries) - 1 do
    begin
      CurrentVersionedXboxLibraryInfo := @(AllXboxLibraries[j]);
      // Take LibVersion and LibName into account (don't scan outside active lib+version) :
      if  (CurrentVersionedXboxLibraryInfo.LibVersion = CurrentXbeLibraryVersion.wBuildVersion)
      and (StrLIComp(PAnsiChar(CurrentVersionedXboxLibraryInfo.LibName), PAnsiChar(CurrentLibName), 8) = 0) then
      begin
        // Once found, scan the memory for functions from this library :
        _ScanMemoryRangeForLibraryPatterns(ByteScanLower, ByteScanUpper, CurrentVersionedXboxLibraryInfo);
        Break;
      end;
    end;

    DbgPrintf('DxbxHLE : Detected %d APIs from "%s"', [DetectedFunctions.Count - PrevCount, CurrentLibName]);
    PrevCount := DetectedFunctions.Count;

    // Skip to the next library :
    Inc(CurrentXbeLibraryVersion);
  end;
*)

  DbgPrintf('DxbxHLE : Detected %d APIs', [DetectedFunctions.Count]);
end;

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
