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
  uBitUtils,
  uXboxLibraryUtils,
  uCRC16;

type
  TDetectedFunctions = class(TObject)
  protected
    FList: TList;
    function GetItem(Index: Integer): PDetectedVersionedXboxLibraryFunction;
  public
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

function TestAddressUsingPatternTrie(const aStoredSignatureTrieHeader: PStoredSignatureTrieHeader; const aAddress: PByte): PStoredLibraryFunction;

  function _GetNode(const aNodeOffset: TByteOffset): PStoredTrieNode;
  begin
    IntPtr(Result) := IntPtr(aStoredSignatureTrieHeader) + aNodeOffset;
  end;

  function _TryMatchingLeaf(const aStoredLibraryFunction: PStoredLibraryFunction; aAddress: PByte): PStoredLibraryFunction;
  begin
    Result := aStoredLibraryFunction;
    if aStoredLibraryFunction.CRCLength > 0 then
    begin
      if aStoredLibraryFunction.CRCValue <> CalcCRC16(aAddress, aStoredLibraryFunction.CRCLength) then
        Result := nil;
    end;

    // TODO : Include data & test-code for : Cross-referenced APIs
    // TODO : Include data & test-code for : Trailing bytes
    
    // TODO : Also consider different XDK versions here!
  end;

  function _TryMatchingNode(aStoredTrieNode: PStoredTrieNode; aAddress: PByte; Depth: Integer): PStoredLibraryFunction;
  var
    NrChildren: Integer;
    StretchPtr: PByte;
    StretchHeader: Byte;
    More: Boolean;
    NrFixed, NrWildcards, i: Integer;
    NextOffset: TByteOffset;
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

      // If stretch was hit, update depth and search-addres for the next stretch :
      Inc(Depth, NrFixed);
      Inc(Depth, NrWildcards);
      Inc(aAddress, NrWildcards);
    until not More;

    if Depth = PATTERNSIZE then
    begin
      Result := _TryMatchingLeaf(Pointer(StretchPtr), aAddress);
      // TODO : Handle all children leafs here, searching for the best-fit lib-version
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
      aStoredTrieNode := _GetNode(NextOffset);
      Dec(NrChildren);
    end;

    Result := nil;
  end;

var
  Node: PStoredTrieNode;
begin
  // Search if this address matches a pattern :
  Node := _GetNode(aStoredSignatureTrieHeader.TrieRootNode);

  Result := _TryMatchingNode(Node, aAddress, 0);
end; // TestAddressUsingPatternTrie

procedure DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);
var
  StoredSignatureTrieHeader: PStoredSignatureTrieHeader;
  StringOffsetList: PStringOffsetList;
{$IFDEF _DEBUG_TRACE}
  GlobalFunctionList: PGlobalFunctionList;
{$ENDIF}

  function _GetByteOffset(const aOffset: TByteOffset): PByteOffset;
  begin
    IntPtr(Result) := IntPtr(StoredSignatureTrieHeader) + aOffset;
  end;

  function _GetStringPointerByIndex(const aStringIndex: TStringTableIndex): PAnsiChar;
  var
    Offset: TByteOffset;
  begin
    Offset := StringOffsetList[aStringIndex];
    Result := PAnsiChar(_GetByteOffset(Offset));
  end;

  function _GetString(const aStringIndex: TStringTableIndex): AnsiString;
  var
    StrBase, StrEnd: PAnsiChar;
    Len: Integer;
  begin
    if aStringIndex = 0 then
      StrBase := PAnsiChar(_GetByteOffset(StoredSignatureTrieHeader.StringTable.AnsiCharData))
    else
      StrBase := _GetStringPointerByIndex(aStringIndex - 1);

    StrEnd := _GetStringPointerByIndex(aStringIndex);

    Len := StrEnd - StrBase;
    SetLength(Result, Len);
    Move(StrBase^, Result[1], Len);
  end;

{$IFDEF _DEBUG_TRACE}
  function _GetGlobalFunction(const aGlobalFunctionIndex: TFunctionIndex): PStoredGlobalFunction;
  begin
    Result := @(GlobalFunctionList[aGlobalFunctionIndex]);
  end;
{$ENDIF}

  function _GetFunctionName(const aGlobalFunctionIndex: TFunctionIndex): string;
{$IFDEF _DEBUG_TRACE}
  var
    StoredGlobalFunction: PStoredGlobalFunction;
{$ENDIF}
  begin
{$IFDEF _DEBUG_TRACE}
    StoredGlobalFunction := _GetGlobalFunction(aGlobalFunctionIndex);
    Result := _GetString(StoredGlobalFunction.FunctionNameIndex);
{$ELSE}
    Result := Format('%.4x', [aGlobalFunctionIndex]);
{$ENDIF}
  end;

  function _FindAndRememberPattern(const aStoredPatternTrie: Pointer; const aAddress: PByte): PStoredLibraryFunction;
  var
    FunctionName: string;
    Detected: PDetectedVersionedXboxLibraryFunction;
  begin
    // Search if this address matches a pattern :
    Result := TestAddressUsingPatternTrie(aStoredPatternTrie, aAddress);
    if Result = nil then
      Exit;

    // Now that it's found, see if it was already registered :
    FunctionName := _GetFunctionName(Result.GlobalFunctionIndex);
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

    DbgPrintf('DxbxHLE : 0x%.8x -> %s', [aAddress, FunctionName]);
  end;

  procedure _ScanMemoryRangeForLibraryPatterns(const ByteScanLower, ByteScanUpper: PByte;
    const aStoredPatternTrie: Pointer);
  var
    p: PByte;
  begin
    p := ByteScanLower;
    while p <> ByteScanUpper do
    begin
      _FindAndRememberPattern(aStoredPatternTrie, p);
      Inc(p);
    end;
  end;

var
  ByteScanLower, ByteScanUpper: PByte;
//  i, j, PrevCount: Integer;
//  CurrentXbeLibraryVersion: PXBE_LIBRARYVERSION;
//  CurrentLibName: string;
//  CurrentVersionedXboxLibrary: PVersionedXboxLibrary;
begin
  ByteScanLower := PByte(pXbeHeader.dwBaseAddr);
  ByteScanUpper := PByte(IntPtr(ByteScanLower) + pXbeHeader.dwSizeofImage);

  DetectedFunctions.Clear;

  // Get StoredPatternTrie from resource :
  with TResourceStream.Create(LibModuleList.ResInstance, 'StoredPatternTrie', RT_RCDATA) do
  try
    StoredSignatureTrieHeader := Memory;
    StringOffsetList := PStringOffsetList(_GetByteOffset(StoredSignatureTrieHeader.StringTable.StringOffsets));
{$IFDEF _DEBUG_TRACE}
    GlobalFunctionList := PGlobalFunctionList(_GetByteOffset(StoredSignatureTrieHeader.GlobalFunctionTable.GlobalFunctionsOffset));
{$ENDIF}

    // Scan Patterns using this trie :
    _ScanMemoryRangeForLibraryPatterns(ByteScanLower, ByteScanUpper, StoredSignatureTrieHeader);
  finally
    // Unlock the resource :
    Free;
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

  DbgPrintf('DxbxHLE : Detected a total of %d APIs', [DetectedFunctions.Count]);
end;

{ TDetectedFunctions }

constructor TDetectedFunctions.Create;
begin
  inherited Create;

  FList := TList.Create;
end;

destructor TDetectedFunctions.Destroy;
begin
  Clear;
  FreeAndNil(FList);

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
end;

function TDetectedFunctions.New(const aFunctionName: string): PDetectedVersionedXboxLibraryFunction;
begin
  Result := AllocMem(SizeOf(RDetectedVersionedXboxLibraryFunction));
  Result.FunctionName := aFunctionName;
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
