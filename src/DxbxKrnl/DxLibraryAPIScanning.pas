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

{.$DEFINE _OPTIMIZE_UNIT}

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  Classes,
  SysUtils,
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
  TPotentialSymbolLocation = class; // forward

  TDetectedVersionedXboxLibrarySymbol = class(TObject)
  protected
    MyLocations: TList;
    function GetLocationCount: Integer;
    function GetLocation(const aIndex: Integer): TPotentialSymbolLocation;
  public
    SymbolName: string;
    XboxLibraryPatch: TXboxLibraryPatch;

    CrossReferences: array of PStoredCrossReference;

    StoredLibraryFunction: PStoredLibraryFunction;

    property LocationCount: Integer read GetLocationCount;
    property Locations[const aIndex: Integer]: TPotentialSymbolLocation read GetLocation;

    constructor Create;
    destructor Destroy; override;

    function AddPotentialSymbolLocation(const aAddress: PByte;
      const FoundFunction: PStoredLibraryFunction): TPotentialSymbolLocation;
    procedure RemoveLocation(Location: TPotentialSymbolLocation);
    function FindLocation(const aAddress: TCodePointer): TPotentialSymbolLocation;
  end;

  TPotentialSymbolLocation = class(TObject)
  protected
    FSymbol: TDetectedVersionedXboxLibrarySymbol;
  public
    StoredLibraryFunction: PStoredLibraryFunction;
    SymbolLocation, CodeEnd: TCodePointer;

    PatternHitCount: Integer;
    CrossReferencedHitCount: Integer;

    property Symbol: TDetectedVersionedXboxLibrarySymbol read FSymbol;

    constructor Create(const aSymbol: TDetectedVersionedXboxLibrarySymbol);
    destructor Destroy; override;
  end;

  PAddressRange = ^RAddressRange;
  RAddressRange = record
    RangeStart, RangeEnd: PByte;
  end;
  
  TDetectedSymbols = class(TObject)
  protected
    MySymbolList: TStringList;
    MyFunctionLocations: TList;
    function GetCount: Integer;
    function GetSymbol(Index: Integer): TDetectedVersionedXboxLibrarySymbol;
  protected
    ByteScanLower, ByteScanUpper: PByte;
    PatternTrieReader: TPatternTrieReader;
    procedure DetectVersionedXboxLibraries(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);
    procedure DetectProcessMemoryRanges();
    function IsAddressInValidProcessMemory(const aAddress: Pointer): Boolean;
    procedure RegisterPotentialFunctionLocation(const aAddress: PByte; const FoundFunction: PStoredLibraryFunction);
    procedure TestAddressUsingPatternTrie(const aAddress: PByte);
    procedure ScanMemoryRangeForLibraryFunctions();
    procedure ValidateFunctionCrossReferences();
    procedure DetermineFinalFunctionAndSymbolLocations();
  public
    LibraryIndexesToConsider: TBits;
    ProcessMemoryRanges: TList; // Containing RAddressRange;

    property Count: Integer read GetCount;
    property Symbols[Index: Integer]: TDetectedVersionedXboxLibrarySymbol read GetSymbol; default;

    constructor Create;
    destructor Destroy; override;

    procedure DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);

    procedure Clear;
    function New(const aSymbolName: string): TDetectedVersionedXboxLibrarySymbol;
    function FindByName(const aSymbolName: string): TDetectedVersionedXboxLibrarySymbol;
    function FindOrCreateByName(const aSymbolName: string): TDetectedVersionedXboxLibrarySymbol;
    function FindByAddress(const aAddress: TCodePointer): TDetectedVersionedXboxLibrarySymbol;
    procedure DeleteSymbol(const Index: Integer);
  end;

var
  DetectedSymbols: TDetectedSymbols;

implementation

const
  // These two are based on our most current XDKTracker database :
  MinXdkVersion = 3911;
  MaxXdkVersion = 5933;
  
  TotalXdkVersionDelta = MaxXdkVersion - MinXdkVersion;
  
// Do our own demangling
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
      Result := StringReplace(Result, '_', '.', [rfReplaceAll]);
    end;
  end;

  // Remove everything from '@' onward :
  i := Pos('@', Result);
  if i > 1 then
    Delete(Result, i, MaxInt);

  // Replace '::' with '.' :
  Result := StringReplace(Result, '::', '.', [rfReplaceAll]);
end; // DxbxUnmangleSymbolName

{$OVERFLOWCHECKS OFF}
function DetermineRelativeAddress(const aStartingAddress: PByte; const aOffset: Word): PByte;
begin
  IntPtr(Result) := IntPtr(aStartingAddress) + IntPtr(aOffset);
  Result := PByte(IntPtr(Result) + PInteger(Result)^ + 4);
end;
{$IFDEF OVERFLOWCHECKS_ON}
  {$OVERFLOWCHECKS ON}
{$ENDIF}

{ TListHelper }

type
  TListHelper = class helper for TList
    function BinarySearch(const Data: Pointer; var Index: Integer; const Compare: TListSortCompare = nil): Boolean;
  end;

// Note : This is a copy of TStringList.Find(), which is
// the only binary search method in the entire Delphi RTL+VCL.
function TListHelper.BinarySearch(const Data: Pointer; var Index: Integer; const Compare: TListSortCompare = nil): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    if @Compare = nil then
      C := IntPtr(Self[I]) - IntPtr(Data)
    else
      C := Compare(Self[I], Data);

    if C < 0 then
      L := I + 1
    else
    begin
      if C = 0 then
      begin
        Result := True;
        L := I;
        Break;
      end;

      H := I - 1;
    end;
  end;

  {var}Index := L;
end;

{ TPotentialSymbolLocation }

constructor TPotentialSymbolLocation.Create(const aSymbol: TDetectedVersionedXboxLibrarySymbol);
begin
  inherited Create;

  FSymbol := aSymbol;
end;

destructor TPotentialSymbolLocation.Destroy;
begin
  if Assigned(Symbol) then
    Symbol.RemoveLocation(Self);

  inherited Destroy;
end;

{ TDetectedVersionedXboxLibrarySymbol }

constructor TDetectedVersionedXboxLibrarySymbol.Create;
begin
  inherited Create;

  MyLocations := TList.Create;
end;

destructor TDetectedVersionedXboxLibrarySymbol.Destroy;
var
  i: Integer;
begin
  for i := 0 to LocationCount - 1 do
  begin
    Locations[i].FSymbol := nil;
    Locations[i].Free;
  end;

  FreeAndNil(MyLocations);

  inherited Destroy;
end;

function TDetectedVersionedXboxLibrarySymbol.GetLocationCount: Integer;
begin
  Result := MyLocations.Count;
end;

function TDetectedVersionedXboxLibrarySymbol.GetLocation(const aIndex: Integer): TPotentialSymbolLocation;
begin
  Result := TPotentialSymbolLocation(MyLocations[aIndex]);
end;

function TDetectedVersionedXboxLibrarySymbol.AddPotentialSymbolLocation(
  const aAddress: PByte; const FoundFunction: PStoredLibraryFunction): TPotentialSymbolLocation;
var
  FoundExisting: Boolean;
  i: Integer;
begin
  // Search for duplicates :
  Result := nil;
  FoundExisting := False;
  for i := 0 to LocationCount - 1 do
  begin
    Result := Locations[i];
    if Result.SymbolLocation = aAddress then
    begin
      // Check weither this address points to the same function or not :
      if (FoundFunction = nil)
      or (Result.StoredLibraryFunction = FoundFunction) then
        FoundExisting := True;

      Break;
    end;
  end;

  if not FoundExisting then
  begin
    // If none found, create and link a new potential location record :
    Result := TPotentialSymbolLocation.Create(Self);
    Result.SymbolLocation := TCodePointer(aAddress);
    // Initially assume it's a global variable of type DWORD :
    Result.CodeEnd := TCodePointer(IntPtr(aAddress) + SizeOf(DWORD));
    MyLocations.Add(Result);
  end;

  if Assigned(FoundFunction) then
  begin
    if Assigned(Result.StoredLibraryFunction) then
//      Assert(Result.StoredLibraryFunction = FoundFunction, 'Collision detected!')
    else
    begin
      Result.StoredLibraryFunction := FoundFunction;
      Result.CodeEnd := TCodePointer(IntPtr(aAddress) + FoundFunction.FunctionLength);
    end;

    Inc(Result.PatternHitCount);
  end
  else
    Inc(Result.CrossReferencedHitCount);

  // When this entry was already present, prevent it from being registered twice :
  if FoundExisting then
    Result := nil;
end; // AddPotentialSymbolLocation

procedure TDetectedVersionedXboxLibrarySymbol.RemoveLocation(Location: TPotentialSymbolLocation);
var
  i: Integer;
begin
  for i := 0 to LocationCount - 1 do
  begin
    if Locations[i] = Location then
    begin
      MyLocations.Delete(i);
      Location.Free;
      Exit;
    end;
  end;
end;

function TDetectedVersionedXboxLibrarySymbol.FindLocation(
  const aAddress: TCodePointer): TPotentialSymbolLocation;
var
  i: Integer;
begin
  for i := 0 to LocationCount - 1 do
  begin
    Result := Locations[i];
    if  (IntPtr(Result.SymbolLocation) >= IntPtr(aAddress))
    and (IntPtr(aAddress) <= IntPtr(Result.CodeEnd)) then
      Exit;
  end;

  Result := nil;
end;

{ TDetectedSymbols }

constructor TDetectedSymbols.Create;
begin
  inherited Create;

  MySymbolList := TStringList.Create;
  MySymbolList.Sorted := True;
  MySymbolList.Duplicates := dupIgnore;
  MySymbolList.CaseSensitive := False;
  MyFunctionLocations := TList.Create;
  ProcessMemoryRanges := TList.Create;
  LibraryIndexesToConsider := TBits.Create;
end;

destructor TDetectedSymbols.Destroy;
begin
  Clear;
  FreeAndNil(MyFunctionLocations);
  FreeAndNil(MySymbolList);
  FreeAndNil(ProcessMemoryRanges);
  FreeAndNil(LibraryIndexesToConsider);

  inherited Destroy;
end;

function TDetectedSymbols.GetCount: Integer;
begin
  Result := MySymbolList.Count;
end;

function TDetectedSymbols.GetSymbol(Index: Integer): TDetectedVersionedXboxLibrarySymbol;
begin
  Result := TDetectedVersionedXboxLibrarySymbol(MySymbolList.Objects[Index]);
end;

procedure TDetectedSymbols.Clear;
var
  i: Integer;
  DetectedSymbol: TDetectedVersionedXboxLibrarySymbol;
begin
  if Assigned(MySymbolList) then
  begin
    for i := 0 to Count - 1 do
    begin
      DetectedSymbol := Symbols[i];
      FreeAndNil(DetectedSymbol); // handles nil gracefully too
    end;

    MySymbolList.Clear;
  end;

  if Assigned(MyFunctionLocations) then
    MyFunctionLocations.Clear;

  if Assigned(ProcessMemoryRanges) then
    ProcessMemoryRanges.Clear; // TODO : Free RAddressRange's

  if Assigned(LibraryIndexesToConsider) then
    LibraryIndexesToConsider.Size := 0;
end;

function TDetectedSymbols.New(const aSymbolName: string): TDetectedVersionedXboxLibrarySymbol;
var
  i: Integer;
begin
  i := MySymbolList.Add(aSymbolName);
  Result := TDetectedVersionedXboxLibrarySymbol(MySymbolList.Objects[i]);
  if Assigned(Result) then
    Exit;

  Result := TDetectedVersionedXboxLibrarySymbol.Create;
  Result.SymbolName := aSymbolName;

  MySymbolList.Objects[i] := Result;
end;

function TDetectedSymbols.FindByName(const aSymbolName: string): TDetectedVersionedXboxLibrarySymbol;
var
  i: Integer;
begin
  i := MySymbolList.IndexOf(aSymbolName);
  if i >= 0 then
    Result := Symbols[i]
  else
    Result := nil;
end;

function TDetectedSymbols.FindOrCreateByName(const aSymbolName: string): TDetectedVersionedXboxLibrarySymbol;
begin
  Result := FindByName(aSymbolName);
  if not Assigned(Result) then
    Result := New(aSymbolName);
end;

function TDetectedSymbols.FindByAddress(const aAddress: TCodePointer): TDetectedVersionedXboxLibrarySymbol;

  function Compare(Item1: TPotentialSymbolLocation; Item2: TCodePointer): Integer;
  begin
    Result := IntPtr(Item1.SymbolLocation) - IntPtr(Item2);
    if Result <= 0 then
      Exit;

    if IntPtr(Item2) >= IntPtr(Item1.CodeEnd) then
      Result := MaxInt;
  end;

var
  i: Integer;
begin
  // For speed, use a binary search in an address-ordered collection here :
  if MyFunctionLocations.BinarySearch(aAddress, i, @Compare) then
    Result := TPotentialSymbolLocation(MyFunctionLocations[i]).Symbol
  else
    Result := nil;
end;

procedure TDetectedSymbols.DeleteSymbol(const Index: Integer);
var
  DetectedSymbol: TDetectedVersionedXboxLibrarySymbol;
begin
  DetectedSymbol := Symbols[Index];
  FreeAndNil(DetectedSymbol); // handles nil gracefully too
  MySymbolList.Delete(Index);
end;

function TDetectedSymbols.IsAddressInValidProcessMemory(const aAddress: Pointer): Boolean;

  function Compare(Item1: PAddressRange; aAddress: UIntPtr): Integer;
  begin
    if aAddress < UIntPtr(Item1.RangeStart) then
      Result := 1
    else
      if aAddress < UIntPtr(Item1.RangeEnd) then
        Result := 0
      else
        Result := -1;
  end;

var
  i: Integer;
begin
  Result := ProcessMemoryRanges.BinarySearch(aAddress, i, @Compare);
end;

procedure TDetectedSymbols.DetectProcessMemoryRanges();
var
  Base: PByte;
  MemInfo: TMemoryBasicInformation;
  Allocated: Boolean;
  AddressRange: PAddressRange;
begin
  ProcessMemoryRanges.Clear;
  Base := nil;
  FillChar(MemInfo, SizeOf(MemInfo), #0);
  while VirtualQuery(Base, MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo) do
  begin
    Allocated := (MemInfo.Type_9 and (MEM_IMAGE or MEM_MAPPED or MEM_PRIVATE)) > 0;
    if Allocated then
    begin
      System.New(AddressRange);
      AddressRange.RangeStart := MemInfo.BaseAddress;
      AddressRange.RangeEnd := PByte(UIntPtr(MemInfo.BaseAddress) + UIntPtr(MemInfo.RegionSize - 1));
      ProcessMemoryRanges.Add(AddressRange);
    end;

    Inc(Base, MemInfo.RegionSize);
  end; // while

  // Note : The resulting ProcessMemoryRanges list is inherently sorted already.
  // So, no sorting needed; Binary-searching in this list is good to go now!
end; // DetectProcessMemoryRanges

procedure TDetectedSymbols.DetectVersionedXboxLibraries(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);
var
  LinkedLibrary: PXBE_LIBRARYVERSION;
  LinkedLibraryName: string;
  LinkedLibraryVersion: Integer;

  IndexOfAboveVersion: Integer; AboveVersionNumber: Integer;
  IndexOfBelowVersion: Integer; BelowVersionNumber: Integer;

  i, j: Integer;

  KnownStoredLibrary: PStoredLibrary;
  KnownStoredLibraryName: string;
  KnownStoredLibraryVersion: Integer;
begin
  // Loop over all libraries :
  LinkedLibrary := pLibraryVersion;
  if not Assigned(LinkedLibrary) then
  begin
    DbgPrintf('DxbxHLE : No XBE library versions to scan!');
    Exit;
  end;

  // Loop over all library versions in the executable:
  LibraryIndexesToConsider.Size := 0;
  LibraryIndexesToConsider.Size := PatternTrieReader.StoredSignatureTrieHeader.LibraryTable.NrOfLibraries;
  for i := 0 to pXbeHeader.dwLibraryVersions - 1 do
  begin
    // Retreive the name & version of each linked library :
    LinkedLibraryName := string(Copy(LinkedLibrary.szName, 1, 8)); // silence D2009+ warning
    LinkedLibraryVersion := Integer(LinkedLibrary.wBuildVersion);
    DbgPrintf('DxbxHLE : Library "%s" is version %d', [LinkedLibraryName, LinkedLibraryVersion]);
    // Jump to the next library already :
    Inc(LinkedLibrary);

    // Find the library versions in our pattern trie that best match the linked version :
    AboveVersionNumber := MaxInt; IndexOfAboveVersion := -1;
    BelowVersionNumber := 0; IndexOfBelowVersion := -1;
    for j := 0 to PatternTrieReader.StoredSignatureTrieHeader.LibraryTable.NrOfLibraries - 1 do
    begin
      KnownStoredLibrary := PatternTrieReader.GetStoredLibrary(j);
      KnownStoredLibraryName := PatternTrieReader.GetString(KnownStoredLibrary.LibNameIndex);
      KnownStoredLibraryVersion := Integer(KnownStoredLibrary.LibVersion);

      // Only consider libraries with exactly the same name :
      if not SameText(KnownStoredLibraryName, LinkedLibraryName) then
        Continue;

      // Exact match above
      if (KnownStoredLibraryVersion = LinkedLibraryVersion)
      // Better-than-previous match above
      or (KnownStoredLibraryVersion > LinkedLibraryVersion) and (KnownStoredLibraryVersion < AboveVersionNumber) then
      begin
        AboveVersionNumber := KnownStoredLibraryVersion;
        IndexOfAboveVersion := j;
      end;

      // Exact match below
      if (KnownStoredLibraryVersion = LinkedLibraryVersion)
      // Better-than-previous match below
      or (KnownStoredLibraryVersion < LinkedLibraryVersion) and (KnownStoredLibraryVersion > BelowVersionNumber) then
      begin
        BelowVersionNumber := KnownStoredLibraryVersion;
        IndexOfBelowVersion := j;
      end;
    end; // for

    if (IndexOfAboveVersion < 0) and (IndexOfBelowVersion < 0) then
    begin
      DbgPrintf('... No patterns registered for this library!');
      Continue;
    end;

    // Always try a lower version instead of quit the process
    if (AboveVersionNumber - BelowVersionNumber) > TotalXdkVersionDelta then
    begin
      // The distinction here is, that patterns for OpenXDK libraries can't
      // be used to detect retail XDK libraries with :
      DbgPrintf('... No usable patterns available for this library!');
      Continue;
    end;

    if AboveVersionNumber = BelowVersionNumber then
      DbgPrintf('... Got patterns for exactly this version!')
    else
      DbgPrintf('... Approximating this with patterns from XDK %d and %d.', [BelowVersionNumber, AboveVersionNumber]);

    // Add the found libraries (actually, their indexes)
    // to a set we'll be checking in the detection-code :
    if IndexOfAboveVersion >= 0 then
      LibraryIndexesToConsider.Bits[IndexOfAboveVersion] := True;
      
    if IndexOfBelowVersion >= 0 then
      LibraryIndexesToConsider.Bits[IndexOfBelowVersion] := True;
  end;
end; // DetectVersionedXboxLibraries

procedure TDetectedSymbols.RegisterPotentialFunctionLocation(const aAddress: PByte; const FoundFunction: PStoredLibraryFunction);
var
  FunctionName: string;
  DetectedSymbol: TDetectedVersionedXboxLibrarySymbol;
  CrossReference: PStoredCrossReference;
  i: Integer;
begin
  Assert(Assigned(FoundFunction));

  FunctionName := PatternTrieReader.GetFunctionName(FoundFunction.GlobalFunctionIndex);

  // Now that it's found, see if it was already registered :
  DetectedSymbol := FindOrCreateByName(FunctionName);
  DetectedSymbol.StoredLibraryFunction := FoundFunction;
  DetectedSymbol.AddPotentialSymbolLocation(aAddress, FoundFunction);

  // TODO : Support having more than 1 incarnation of a function
  // (which is possible when 2 libraries are considered)

  if Length(DetectedSymbol.CrossReferences) < FoundFunction.NrCrossReferences then
  begin
    // Last, register all cross-referenced symbols from this function too :
    SetLength(DetectedSymbol.CrossReferences, FoundFunction.NrCrossReferences);
    IntPtr(CrossReference) := IntPtr(FoundFunction) + SizeOf(RStoredLibraryFunction);
    for i := 0 to FoundFunction.NrCrossReferences - 1 do
    begin
      DetectedSymbol.CrossReferences[i] := CrossReference;
      Inc(CrossReference);
    end;
  end;
end; // RegisterPotentialFunctionLocation

procedure TDetectedSymbols.TestAddressUsingPatternTrie(const aAddress: PByte);

  function _CheckCrossReference(const aStartingAddress: PByte;
    const aCrossReferenceOffset: Word; const aCrossReferenceNameIndex: TStringTableIndex): Boolean;
  var
    CrossReferenceAddress: PByte;
  begin
    // Use aCrossReferenceOffset to determine
    // the call-location that should be checked :
    CrossReferenceAddress := DetermineRelativeAddress(aStartingAddress, aCrossReferenceOffset);

    // Now check if this address resides in memory owned by this process?
    Result := IsAddressInValidProcessMemory(CrossReferenceAddress);
  end; // _CheckCrossReference

  function _TryMatchingLeaf(var aStoredLibraryFunction: PStoredLibraryFunction; aAddress: PByte): Boolean;
  var
    i: Integer;
    FoundFunction: PStoredLibraryFunction;
    CrossReference: PStoredCrossReference;
  begin
    Result := False;
    FoundFunction := aStoredLibraryFunction;
{$IFDEF DXBX_RECTYPE}
    Assert(FoundFunction.RecType = rtStoredLibraryFunction, 'StoredLibraryFunction type mismatch!');
{$ENDIF}

    // Start off, by skipping to the next stored library function already :
    Inc({var}aStoredLibraryFunction);
    // Including a step over all cross-references :
    Inc(IntPtr({var}aStoredLibraryFunction), FoundFunction.NrCrossReferences * SizeOf(RStoredCrossReference));

    // Determine if the stored library version associated with this pattern
    // matches the library versions we've decided to consider :
    if not LibraryIndexesToConsider.Bits[FoundFunction.LibraryIndex] then
      // No matching library, skip this :
      Exit;

    // Check if the (optional) CRC over this address matches :
    if FoundFunction.CRCLength > 0 then
    begin
      if FoundFunction.CRCValue <> CalcCRC16(aAddress, FoundFunction.CRCLength) then
        Exit;
    end;

    // TODO : Include data & test-code for 'trailing bytes' patterns.

    // Correct aAddress back to starting location :
    Dec(UIntPtr(aAddress), PATTERNSIZE);

    // Handle possible cross-reference checks :
    IntPtr(CrossReference) := IntPtr(FoundFunction) + SizeOf(RStoredLibraryFunction);
    for i := 0 to FoundFunction.NrCrossReferences - 1 do
    begin
      if not _CheckCrossReference(aAddress, CrossReference.Offset, CrossReference.NameIndex) then
        // Cross-reference out-of-bounds, skip this :
        Exit;

      Inc(CrossReference);
    end;

    // Potential hit :
    RegisterPotentialFunctionLocation(aAddress, FoundFunction);
    Result := True;
  end; // _TryMatchingLeaf

  function _TryMatchingNode(aStoredTrieNode: PStoredTrieNode; aAddress: PByte; Depth: Integer): Boolean;
  var
    NrChildren: Integer;
    StretchPtr: PByte;
    StretchHeaderByte: TStretchHeaderByte;
    More: Boolean;
    NrFixed, NrWildcards, i: Integer;
    NextOffset: TByteOffset;
    StoredLibraryFunction: PStoredLibraryFunction;
  begin
    Result := False;
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
      // Note, we're handling _all_ children here :
      StoredLibraryFunction := Pointer(StretchPtr);
      while NrChildren > 0 do
      begin
        if _TryMatchingLeaf({var}StoredLibraryFunction, aAddress) then
        begin
          // TODO : Instead of calling RegisterPotentialFunctionLocation right away,
          // we should collect all valid's and register only the longest functions
          // (per versioned-library).
          Result := True;
        end;

        Dec(NrChildren);
      end;

      Exit;
    end; // if Depth

    aStoredTrieNode := Pointer(StretchPtr);
    while NrChildren > 0 do
    begin
      // Try to match pattern on this node
      Result := _TryMatchingNode(aStoredTrieNode, aAddress, Depth);
      if Result then
        Exit;

      // Try next child, maybe that helps:
      NextOffset := aStoredTrieNode.NextSiblingOffset;
      // Sanity-check on next-offset :
      if (NextOffset <= 0) or (NextOffset > 100*1024*1024) then
        Break;

      // Jump to next sibling :
      aStoredTrieNode := PatternTrieReader.GetNode(NextOffset);
      Dec(NrChildren);
    end;
  end; // _TryMatchingNode

var
  Node: PStoredTrieNode;
begin
  // Search if this address matches a pattern :
  Node := PatternTrieReader.GetNode(PatternTrieReader.StoredSignatureTrieHeader.TrieRootNode);

  _TryMatchingNode(Node, aAddress, 0);
end; // TestAddressUsingPatternTrie

procedure TDetectedSymbols.ScanMemoryRangeForLibraryFunctions();
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
      // Search if this address matches a pattern :
      TestAddressUsingPatternTrie(p);
    except
{$IFDEF DXBX_DEBUG}
      DbgPrintf('DxbxHLE : Exception while scanning on address $%.8x', [p],
      {MayRenderArguments=}False);
{$ENDIF}
    end;

    Inc(p);
  end;
end; // ScanMemoryRangeForLibraryFunctions

procedure TDetectedSymbols.ValidateFunctionCrossReferences();
var
  i: Integer;
  Symbol: TDetectedVersionedXboxLibrarySymbol;
  j: Integer;
  PotentialSymbolLocation: TPotentialSymbolLocation;
  k: Integer;
  CrossReferencedSymbolName: string;
  CrossReferencedSymbol: TDetectedVersionedXboxLibrarySymbol;//PStoredCrossReference;
  CrossReferenceAddress: PByte;
  CrossReferencedSymbolLocation: TPotentialSymbolLocation;
begin
  // Loop over all potential-locations of all registered functions :
  for i := Count - 1 downto 0 do
  begin
    Symbol := Symbols[i];
    j := Symbol.LocationCount;
    while j > 0 do
    begin
      Dec(j);
      PotentialSymbolLocation := Symbol.Locations[j];

      // Check for all functions they cross-reference
      // if the referenced-function could indeed reside at the given address.
      for k := 0 to Length(Symbol.CrossReferences) - 1 do
      begin
        CrossReferencedSymbolName := PatternTrieReader.GetString(Symbol.CrossReferences[k].NameIndex);
        CrossReferencedSymbol := FindByName(CrossReferencedSymbolName);
        if not Assigned(CrossReferencedSymbol) then
          Continue; // for

        CrossReferenceAddress := DetermineRelativeAddress(PotentialSymbolLocation.SymbolLocation, Symbol.CrossReferences[k].Offset);
        CrossReferencedSymbolLocation := CrossReferencedSymbol.FindLocation(CrossReferenceAddress);
        if Assigned(CrossReferencedSymbolLocation) then
          // If so, the referenced-function scores a hit on his 'potential location' corresponding to this address.
          Inc(CrossReferencedSymbolLocation.PatternHitCount)
        else
        begin
          // If not, this function itself is probably wrong - de-register this location
          Symbol.RemoveLocation(PotentialSymbolLocation);
          Break; // for
        end;
      end; // for cross-references

    end; // while symbol locations

    // When the above leaves no potential locations, the complete symbol has to go too.
    if Symbol.LocationCount = 0 then
      DeleteSymbol(i);
  end; // for symbols

end; // ValidateFunctionCrossReferences

procedure TDetectedSymbols.DetermineFinalFunctionAndSymbolLocations();
var
  i: Integer;
  Symbol: TDetectedVersionedXboxLibrarySymbol;
  j: Integer;
  PotentialSymbolLocation: TPotentialSymbolLocation;
  BestPotentialSymbolLocation: TPotentialSymbolLocation;
  k: Integer;
  CrossReferencedSymbolName: string;
  CrossReferencedSymbol: TDetectedVersionedXboxLibrarySymbol;//PStoredCrossReference;
  CrossReferenceAddress: PByte;
{$IFDEF DXBX_DEBUG}
  Unmangled: string;
{$ENDIF}
begin
  // Loop over all registered functions :
  for i := 0 to Count - 1 do
  begin
    Symbol := Symbols[i];
    Assert(Symbol.LocationCount > 0);

    // Find the potential location with the highest PatternHitCount:
    BestPotentialSymbolLocation := Symbol.Locations[0];
    for j := 1 to Symbol.LocationCount - 1 do
    begin
      PotentialSymbolLocation := Symbol.Locations[j];
      if BestPotentialSymbolLocation.PatternHitCount < PotentialSymbolLocation.PatternHitCount then
        BestPotentialSymbolLocation := PotentialSymbolLocation;
    end;

    // Register this location as the actual location of this symbol
    // by removing all other locations :
    j := Symbol.LocationCount;
    while j > 0 do
    begin
      Dec(j);
      PotentialSymbolLocation := Symbol.Locations[j];
      if PotentialSymbolLocation <> BestPotentialSymbolLocation then
        Symbol.RemoveLocation(PotentialSymbolLocation);
    end;

    // Register all cross-referenced symbols :
    for k := 0 to Length(Symbol.CrossReferences) - 1 do
    begin
      CrossReferencedSymbolName := PatternTrieReader.GetString(Symbol.CrossReferences[k].NameIndex);
      CrossReferencedSymbol := FindOrCreateByName(CrossReferencedSymbolName);
      if CrossReferencedSymbol.LocationCount > 0 then
        Continue;

      CrossReferenceAddress := DetermineRelativeAddress(BestPotentialSymbolLocation.SymbolLocation, Symbol.CrossReferences[k].Offset);
      CrossReferencedSymbol.AddPotentialSymbolLocation(CrossReferenceAddress, nil);
{$IFDEF DXBX_DEBUG}
      // Do our own demangling :
      Unmangled := DxbxUnmangleSymbolName(CrossReferencedSymbol.SymbolName);
      DbgPrintf('DxbxHLE : Detected cross-referenced symbol at $%.8x : ''%s'' ("%s")',
        [CrossReferenceAddress, CrossReferencedSymbol.SymbolName, Unmangled],
        {MayRenderArguments=}False);
{$ENDIF}
    end;

{$IFDEF DXBX_DEBUG}
    // Do our own demangling :
    Unmangled := DxbxUnmangleSymbolName(Symbol.SymbolName);
    DbgPrintf('DxbxHLE : Detected at $%.8x : ''%s'' ("%s")',
      [BestPotentialSymbolLocation.SymbolLocation, Symbol.SymbolName, Unmangled],
      // string(XboxLibraryPatchToString(Detected.XboxLibraryPatch)
      {MayRenderArguments=}False);
{$ENDIF}
    // TODO : Add to MyFunctionLocations, so functions can be found with FindByAddress (a pure debugging aid)

///  DetectedSymbol.StoredLibraryFunction := FoundFunction;
  end; // for symbols
end; // DetermineFinalFunctionAndSymbolLocations

procedure TDetectedSymbols.DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);

  procedure _ResolveXapiProcessHeapAddress();
  var
    DetectedXapiProcessHeap,
    DetectedXapiInitProcess: TDetectedVersionedXboxLibrarySymbol;
    StoredLibrary: PStoredLibrary;
    ProcessHeapOffs: Integer;
  begin
    DetectedXapiProcessHeap := FindByName('_XapiProcessHeap');
    if Assigned(DetectedXapiProcessHeap) then
    begin
      XTL_EmuXapiProcessHeap := DetectedXapiProcessHeap.Locations[0].SymbolLocation;
      if Assigned(XTL_EmuXapiProcessHeap) then
        Exit;
    end;

    DetectedXapiInitProcess := FindByName('_XapiInitProcess@0');
    if Assigned(DetectedXapiInitProcess) then
    begin
      StoredLibrary := PatternTrieReader.GetStoredLibrary(DetectedXapiInitProcess.StoredLibraryFunction.LibraryIndex);
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

        XTL_EmuXapiProcessHeap := PPointer(IntPtr(DetectedXapiInitProcess.Locations[0].SymbolLocation) + ProcessHeapOffs)^;
        
{$IFDEF DXBX_DEBUG}
        DbgPrintf('DxbxHLE : Resolved XapiProcessHeap at $%.8x', [XTL_EmuXapiProcessHeap],
          {MayRenderArguments=}False);
// Dxbx Output = DxbxHLE : Resolved XapiProcessHeap at $00258654
// Cxbx Output = HLE: 0x00258654 -> EmuXapiProcessHeap
{$ENDIF}
      end;
    end;
  end; // _ResolveXapiProcessHeapAddress

var
  ResourceStream: TResourceStream;
begin
  ByteScanLower := PByte(pXbeHeader.dwBaseAddr);
  ByteScanUpper := PByte(IntPtr(ByteScanLower) + Integer(pXbeHeader.dwSizeofImage) + Integer(pXbeHeader.dwPeStackCommit));
  // Dxbx Note : Extending the scan beyond dwSizeofImage (by adding dwPeStackCommit)
  // might cause Access Violations (in which case we should probably determine
  // a more accurate boundary). The increment of the range was done to be able
  // to still capture far-away global symbols (like 'XapiProcessHeap').
  //
  // In the case of Turok, this global is located at $00269DB3, whilst code
  // ends before that at $00262420. By adding dwPeStackCommit ($00010000),
  // we stretch the window to $00272420. This seems about right, as the
  // Access Violations only start a little after that, at address $00274000.

  Clear;

  DetectProcessMemoryRanges();

  // Get StoredPatternTrie from resource :
  ResourceStream := TResourceStream.Create(LibModuleList.ResInstance, 'StoredPatternTrie', RT_RCDATA);
  try
    PatternTrieReader := TPatternTrieReader.Create;
    try
      PatternTrieReader.LoadFromStream(ResourceStream);

      DetectVersionedXboxLibraries(pLibraryVersion, pXbeHeader);

      ScanMemoryRangeForLibraryFunctions();

      ValidateFunctionCrossReferences();

      DetermineFinalFunctionAndSymbolLocations();

      // Resolve the address of _XapiProcessHeap :
      _ResolveXapiProcessHeapAddress();

    finally
      FreeAndNil(PatternTrieReader);
    end;

  finally
    // Unlock the resource :
    FreeAndNil(ResourceStream);
  end;

  DbgPrintf('DxbxHLE : Detected functions : %d.', [DetectedSymbols.Count]);
end; // DxbxScanForLibraryAPIs

initialization

  DetectedSymbols := TDetectedSymbols.Create;

finalization

  FreeAndNil(DetectedSymbols);

end.

