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
  SysUtils,
  Classes,
  FileCtrl, // ForceDirectories
  // Dxbx
  uConsts,
  uTypes,
  uDxbxUtils,
  uLog,
  uXBE,
  uDxbxKrnlUtils,
  uCRC16,
  uStoredTrieTypes,
  uXboxLibraryUtils,
  uEmuD3D8Types,// X_D3DRS_UNK
  uState, // XTL_EmuD3DDeferredRenderState and XTL_EmuD3DDeferredTextureState
  uEmu, // EmuWarning
  uEmuXapi, // XTL_EmuXapiProcessHeap
  uEmuExe; // ReinitXbeImageHeader, ReinitExeImageHeader

type
  TSymbolInformation = class; // forward

  PPotentialFunctionLocation = ^RPotentialFunctionLocation;
  RPotentialFunctionLocation = record
  private
    function GetLength: Cardinal;
    function GetCodeEnd: TCodePointer;
  public
    Address: TCodePointer;
    Symbol: TSymbolInformation;
    SymbolReferencesIndex: Integer;
    NrOfReferencesTo: Integer;
    NextPotentialFunctionLocationIndex: Integer;

    property Length: Cardinal read GetLength;
    property CodeEnd: TCodePointer read GetCodeEnd;
  end; // SizeOf() = 20

  TSymbolInformation = class(TObject)
  private
    function GetHasPotentialLocations: Boolean;
    function GetIsSymbolNameUsedInFunctions: Boolean;
    function GetHasFunctionInformation: Boolean;
    function GetHasSymbolReferences: Boolean;
    function GetLength: Cardinal;
    function GetSymbolReferenceCount: Integer;
    function GetSymbolReference(const aIndex: Integer): PStoredSymbolReference;
  public
    NameIndex: TStringTableIndex;
    Name: string;
    Address: TCodePointer;
    FirstPotentialFunctionLocationIndex: Integer;
    StoredLibraryFunction: PStoredLibraryFunction; // can be nil for non-pattern symbols

    function FindPotentialLocationIndex(const aAddress: TCodePointer): Integer;
    function FindPotentialLocation(const aAddress: TCodePointer): PPotentialFunctionLocation;
    function AddPotentialLocation(const aAddress: TCodePointer): PPotentialFunctionLocation;

    property IsSymbolNameUsedInFunctions: Boolean read GetIsSymbolNameUsedInFunctions;
    property HasPotentialLocations: Boolean read GetHasPotentialLocations;
    property HasFunctionInformation: Boolean read GetHasFunctionInformation;
    property HasSymbolReferences: Boolean read GetHasSymbolReferences;

    property Length: Cardinal read GetLength;
    property SymbolReferenceCount: Integer read GetSymbolReferenceCount;
    property SymbolReferences[const aIndex: Integer]: PStoredSymbolReference read GetSymbolReference;
  end;

  TSymbolManager = class(TObject)
  protected // Symbols :
    MySymbolsHashedOnName: array [Word] of TSymbolInformation; // Hash table for all detected symbols, hashed on Name
    MySymbolCount: Integer; // Number of detected symbols (including symbol-references)
    function FindSymbol(const aNameIndex: TStringTableIndex): TSymbolInformation; overload;
    function FindOrAddSymbol(const aNameIndex: TStringTableIndex; const aFoundFunction: PStoredLibraryFunction = nil): TSymbolInformation; overload;
    function FindOrAddSymbol(const aName: string; const aFoundFunction: PStoredLibraryFunction = nil): TSymbolInformation; overload;
  public
    function FindSymbol(const aName: string): TSymbolInformation; overload;
  protected // Potential function locations :
    MyPotentialFunctionLocations: array of RPotentialFunctionLocation; // Lineair array for all potential function locations, 0 is invalid
    MyPotentialFunctionLocations_Count: Integer; // Number of potential function locations
    function AddPotentialSymbolLocation(const aLocation: TCodePointer; const aSymbol: TSymbolInformation): PPotentialFunctionLocation;
  public
    function FindPotentialFunctionLocation(const aLocation: TCodePointer): PPotentialFunctionLocation;
  protected // Final function locations :
    MyFinalLocations: TList;
    function GetCount: Integer;
    function GetLocation(const aIndex: Integer): TSymbolInformation;
  public
    property Count: Integer read GetCount;
    property Locations[const aIndex: Integer]: TSymbolInformation read GetLocation; default;
  protected
    MyAddressesPotentiallyContainingCode: TBits;
    MyAddressesScanned: TBits;
    ScanUpper: UIntPtr;
    PatternTrieReader: TPatternTrieReader;
    AllSymbolReferences: array of record Symbol: TSymbolInformation; Address: TCodePointer; MustSkip: Boolean; end;
    AllSymbolReferencesInUse: Integer;
    FCurrentTestAddress: PByte;
    procedure _Debug(const aSymbol: TSymbolInformation);
    function IsAddressWithinCodeRange(const aAddress: TCodePointer): Boolean;
    function IsAddressWithinScanRange(const aAddress: TCodePointer): Boolean;
    function GetReferencedSymbolAddress(const aStartingAddress: PByte; const aSymbolReference: PStoredSymbolReference): TCodePointer;
    procedure DetectVersionedXboxLibraries(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBEIMAGE_HEADER);
    procedure TestAddressUsingPatternTrie(var aTestAddress: PByte; const DoForwardScan: Boolean = True);
    procedure DetermineFinalLocations;
    procedure DetermineSpecialSymbols;
    procedure ScanMemoryRangeForLibraryPatterns(const pXbeHeader: PXBEIMAGE_HEADER);
  public
    LibraryVersionsToScan: TLibraryVersionFlags;
    constructor Create;
    destructor Destroy; override;

    procedure DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBEIMAGE_HEADER);

    procedure Clear;

    function RegisterSpecificFunctionLocation(const aFunctionName: string; const aAddress: PByte): TSymbolInformation;

    class function CacheFileName(const pXbeHeader: PXBEIMAGE_HEADER): string;
    function LoadSymbolsFromCache(const aCacheFile: string): Boolean;
    procedure SaveSymbolsToCache(const aCacheFile: string);
  end;

var
  SymbolManager: TSymbolManager;
  LibD3D8: PStoredLibrary;

const
  OPCODE_NOP = $90;
  OPCODE_INT3 = $CC;
  OPCODE_JMP = $E9;

implementation

uses
  uVertexBuffer; // CRC32Init, Crc32


function SameLibName(StoredLibraryName, CurrentLibName: string): Boolean;
// StoredLibraryName comes from our pattern files (like '4627xgraphics.pat' gives 'xgraphics')
// CurrentLibName comes from the XBE header (like 'XGRAPHC')
begin
  // Map the pattern-based names to the linked names :
  if SameText(StoredLibraryName, 'xgraphics') then
    StoredLibraryName := 'XGRAPHC';

  Result := SameText(StoredLibraryName, CurrentLibName);
end;

{$OVERFLOWCHECKS OFF}
function DetermineImmediateAddress(const aStartingAddress: PByte; const aOffset: Word): PByte;
begin
  UIntPtr(Result) := UIntPtr(aStartingAddress) + UIntPtr(aOffset);
  Result := PByte(PInteger(Result)^);
end;

function DetermineRelativeAddress(const aStartingAddress: PByte; const aOffset: Word): PByte;
begin
  UIntPtr(Result) := UIntPtr(aStartingAddress) + UIntPtr(aOffset);
  Result := PByte(IntPtr(Result) + PInteger(Result)^ + 4);
end;

{$IFDEF OVERFLOWCHECKS_ON}
  {$OVERFLOWCHECKS ON}
{$ENDIF}

{ RPotentialFunctionLocation }

function RPotentialFunctionLocation.GetLength: Cardinal;
begin
  Assert(Assigned(Symbol));

  if Assigned(Symbol.StoredLibraryFunction) then
    Result := Symbol.Length
  else
    // Assume it's a global variable of type DWORD :
    Result := SizeOf(DWORD);
end;

function RPotentialFunctionLocation.GetCodeEnd: TCodePointer;
begin
  Assert(Assigned(Symbol));

  UIntPtr(Result) := UIntPtr(Address) + Length - 1;
end;

{ TSymbolInformation }

function TSymbolInformation.GetHasPotentialLocations: Boolean;
begin
  Result := (FirstPotentialFunctionLocationIndex > 0);
end;

function TSymbolInformation.GetIsSymbolNameUsedInFunctions: Boolean;
begin
  Result := SymbolManager.PatternTrieReader.GetStringHeader(NameIndex).NrOfFunctions > 0;
end;

function TSymbolInformation.GetHasFunctionInformation: Boolean;
begin
  Result := Assigned(StoredLibraryFunction);
end;

function TSymbolInformation.GetHasSymbolReferences: Boolean;
begin
  Result := (SymbolReferenceCount > 0);
end;

function TSymbolInformation.GetLength: Cardinal;
begin
  if Assigned(StoredLibraryFunction) then
    Result := StoredLibraryFunction.FunctionLength
  else
    // Assume it's a global variable of type DWORD :
    Result := SizeOf(DWORD);
end;

function TSymbolInformation.GetSymbolReferenceCount: Integer;
begin
  if Assigned(StoredLibraryFunction) then
    Result := StoredLibraryFunction.NrOfSymbolReferences
  else
    Result := 0;
end;

function TSymbolInformation.GetSymbolReference(const aIndex: Integer): PStoredSymbolReference;
begin
  Result := SymbolManager.PatternTrieReader.GetSymbolReference(StoredLibraryFunction.FirstSymbolReference + aIndex);
{$IFDEF DXBX_RECTYPE}
  Assert(Result.RecType = rtStoredSymbolReference, 'StoredSymbolReference type mismatch!');
{$ENDIF}
end;

function TSymbolInformation.FindPotentialLocationIndex(const aAddress: TCodePointer): Integer;
begin
  Result := FirstPotentialFunctionLocationIndex;
  while Result > 0 do
  begin
    if SymbolManager.MyPotentialFunctionLocations[Result].Address = aAddress then
      Exit;

    Result := SymbolManager.MyPotentialFunctionLocations[Result].NextPotentialFunctionLocationIndex;
  end;
end;

function TSymbolInformation.FindPotentialLocation(const aAddress: TCodePointer): PPotentialFunctionLocation;
var
  Index: Integer;
begin
  Index := FindPotentialLocationIndex(aAddress);
  if Index > 0 then
    Result := @(SymbolManager.MyPotentialFunctionLocations[Index])
  else
    Result := nil;
end;

function TSymbolInformation.AddPotentialLocation(const aAddress: TCodePointer): PPotentialFunctionLocation;
begin
  Result := FindPotentialLocation(aAddress);
  if Result = nil then
    Result := SymbolManager.AddPotentialSymbolLocation(aAddress, Self);
end;

{ TSymbolManager }

constructor TSymbolManager.Create;
begin
  inherited Create;

  MyFinalLocations := TList.Create;
  MyAddressesScanned := TBits.Create;
  MyAddressesPotentiallyContainingCode := TBits.Create;
end;

destructor TSymbolManager.Destroy;
begin
  Clear;
  FreeAndNil(MyFinalLocations);
  FreeAndNil(MyAddressesScanned);
  FreeAndNil(MyAddressesPotentiallyContainingCode);

  inherited Destroy;
end;

function TSymbolManager.GetCount: Integer;
begin
  Result := MyFinalLocations.Count;
end;

function TSymbolManager.GetLocation(const aIndex: Integer): TSymbolInformation;
begin
  Result := MyFinalLocations[aIndex];
end;

procedure TSymbolManager.Clear;
var
  w: Word;
begin
  MyAddressesScanned.Size := 0;
  LibraryVersionsToScan := [];

  // Clear MySymbolsHashedOnName :
  for w := Low(w) to High(w) do
    FreeAndNil(MySymbolsHashedOnName[w]);
  MySymbolCount := 0;

  SetLength(MyPotentialFunctionLocations, 0);
  MyFinalLocations.Clear;
  MyPotentialFunctionLocations_Count := 0;
end; // Clear

function TSymbolManager.FindSymbol(const aNameIndex: TStringTableIndex): TSymbolInformation;
var
  SymbolName: string;
begin
  SymbolName := PatternTrieReader.GetString(aNameIndex);
  Result := FindSymbol(SymbolName);
  if Assigned(Result) then
  begin
    if Result.NameIndex = 0 then
      Result.NameIndex := aNameIndex
    else
      Assert(Result.NameIndex = aNameIndex);
  end;
end; // FindSymbol

function TSymbolManager.FindSymbol(const aName: string): TSymbolInformation;
var
  HashIndex: Word;
begin
  HashIndex := CalcCRC16(PByte(aName), Length(aName) * SizeOf(Char));
  Result := MySymbolsHashedOnName[HashIndex];
  while Assigned(Result) do
  begin
    if (Result.Name = aName) then
      Exit;

    Inc(HashIndex, 1);
    Result := MySymbolsHashedOnName[HashIndex];
  end;
end; // FindSymbol

function TSymbolManager.FindOrAddSymbol(const aNameIndex: TStringTableIndex;
  const aFoundFunction: PStoredLibraryFunction = nil): TSymbolInformation;
var
  SymbolName: string;
begin
  SymbolName := PatternTrieReader.GetString(aNameIndex);
  Result := FindOrAddSymbol(SymbolName, aFoundFunction);
  if Result.NameIndex = 0 then
    Result.NameIndex := aNameIndex
  else
    Assert(Result.NameIndex = aNameIndex);
end; // FindOrAddSymbol

function TSymbolManager.FindOrAddSymbol(const aName: string;
  const aFoundFunction: PStoredLibraryFunction = nil): TSymbolInformation;
var
  HashIndex: Word;
begin
  HashIndex := CalcCRC16(PByte(aName), Length(aName) * SizeOf(Char));
  Result := MySymbolsHashedOnName[HashIndex];
  while Assigned(Result) do
  begin
    if (Result.Name = aName) then
    begin
      if Result.StoredLibraryFunction = nil then
        Result.StoredLibraryFunction := aFoundFunction
      else
      begin
        if Assigned(aFoundFunction) then
        begin
          // Multiple occurrances are possible (from multiple XDK's)
          // and are allowable as long as they describe the same function :
//          Assert(Result.StoredLibraryFunction.GlobalFunctionIndex = aFoundFunction.GlobalFunctionIndex, 'Error! Same name, other function!');

          // Keep track of the longest version :
          if Result.StoredLibraryFunction.FunctionLength < aFoundFunction.FunctionLength then
            Result.StoredLibraryFunction := aFoundFunction;
        end;
      end;

      Exit;
    end;

    Inc(HashIndex, 1);
    Result := MySymbolsHashedOnName[HashIndex];
  end;

  Result := TSymbolInformation.Create;
  Result.StoredLibraryFunction := aFoundFunction;
  Result.Name := aName;

  MySymbolsHashedOnName[HashIndex] := Result;
  Inc(MySymbolCount);
end; // FindOrAddSymbol

function TSymbolManager.AddPotentialSymbolLocation(const aLocation: TCodePointer; const aSymbol: TSymbolInformation): PPotentialFunctionLocation;
var
  CurrentCapacity: Integer;
begin
  Assert(Assigned(aLocation));
  Assert(Assigned(aSymbol));

  Inc(MyPotentialFunctionLocations_Count);

  // Grow using the so-called 'Doubling method' for O(1) average performance :
  CurrentCapacity := Length(MyPotentialFunctionLocations);
  if CurrentCapacity <= MyPotentialFunctionLocations_Count then
  begin
    if CurrentCapacity = 0 then
      CurrentCapacity := 1024
    else
      CurrentCapacity := CurrentCapacity * 2;

    SetLength(MyPotentialFunctionLocations, CurrentCapacity);
  end;

  Result := @(MyPotentialFunctionLocations[MyPotentialFunctionLocations_Count]);
  with Result^ do
  begin
    Address := aLocation;
    Symbol := aSymbol;
    NextPotentialFunctionLocationIndex := aSymbol.FirstPotentialFunctionLocationIndex;
    NrOfReferencesTo := 0;
  end;

  aSymbol.FirstPotentialFunctionLocationIndex := MyPotentialFunctionLocations_Count;
end; // AddPotentialSymbolLocation

// Does a binary search over MyPotentialFunctionLocations for the given address.
// When found, the lowest index where this address appears is returned, -1 otherwise.
function TSymbolManager.FindPotentialFunctionLocation(const aLocation: TCodePointer): PPotentialFunctionLocation;
var
  L, H, I, C: Integer;
begin
  // Assert(Sorted);
  Result := nil;
  L := 1;
  H := MyPotentialFunctionLocations_Count;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Result := @(MyPotentialFunctionLocations[I]);
    C := IntPtr(Result.Address) - IntPtr(aLocation);
    if C < 0 then
      L := I + 1
    else
    begin
      if C = 0 then
      begin
        L := I;
//        Result := L;
        // We need to find the left-most index, so no break here!
      end;

      H := I - 1;
    end;
  end;
end; // FindPotentialFunctionLocation

function TSymbolManager.IsAddressWithinCodeRange(const aAddress: TCodePointer): Boolean;
begin
  Result := (UIntPtr(aAddress) < UIntPtr(MyAddressesPotentiallyContainingCode.Size))
        and MyAddressesPotentiallyContainingCode[Integer(aAddress)];
end;

function TSymbolManager.IsAddressWithinScanRange(const aAddress: TCodePointer): Boolean;
begin
  Result := (UIntPtr(aAddress) >= XBE_IMAGE_BASE) and (UIntPtr(aAddress) <= ScanUpper);
end;

function TSymbolManager.GetReferencedSymbolAddress(const aStartingAddress: PByte;
  const aSymbolReference: PStoredSymbolReference): TCodePointer;
begin
  // Use aSymbolReference to determine the symbol-address that should be checked :
  if (aSymbolReference.ReferenceFlags and rfIsRelative) > 0 then
    Result := DetermineRelativeAddress(aStartingAddress, aSymbolReference.Offset)
  else
  if (aSymbolReference.ReferenceFlags and rfIsAbsolute) > 0 then
    Result := DetermineImmediateAddress(aStartingAddress, aSymbolReference.Offset)
  else
  begin
    // TODO : Handle rfIsSectionRel
    Result := nil;
    Exit;
  end;

  Dec(IntPtr(Result), aSymbolReference.BaseOffset); // Must be signed ptr, for negative offsets!
  if not IsAddressWithinScanRange(Result) then
    Result := nil;
end; // GetReferencedSymbolAddress

function TSymbolManager.RegisterSpecificFunctionLocation(const aFunctionName: string; const aAddress: PByte): TSymbolInformation;
begin
  Result := FindOrAddSymbol(aFunctionName, {FoundFunction=}nil);
  AddPotentialSymbolLocation(aAddress, Result);
end;

procedure TSymbolManager._Debug(const aSymbol: TSymbolInformation);
const
  DuplicateString: array [Boolean] of string = ('', ' [Duplicate]');
var
  i: Integer;
  f: Integer;
  c: Integer;
  CurrentLocation: PPotentialFunctionLocation;
  SymbolReference: PStoredSymbolReference;
begin
  if aSymbol = nil then
    Exit;

  DbgPrintf('SYMBOL : "%s"', [aSymbol.Name]);
  DbgPrintf('Length : %d', [aSymbol.Length]);
  DbgPrintf('Address : $%.08x', [aSymbol.Address]);
  DbgPrintf('SymbolReferenceCount : %d', [aSymbol.SymbolReferenceCount]);
  DbgPrintf('PotentialLocations :');
  c := 10;
  f := aSymbol.FirstPotentialFunctionLocationIndex;
  while f > 0 do
  begin
    CurrentLocation := @(MyPotentialFunctionLocations[f]);
    Assert(Assigned(CurrentLocation));

    DbgPrintf('#%d : $%0.8x (%2d references)', [
      f,
      CurrentLocation.Address,
      CurrentLocation.NrOfReferencesTo]);

    if CurrentLocation.SymbolReferencesIndex > 0 then
    begin
      for i := 0 to aSymbol.SymbolReferenceCount - 1 do
      begin
        SymbolReference := aSymbol.SymbolReferences[i];
        Assert(Assigned(SymbolReference));

        DbgPrintf('  SymbolReference %2d : $%0.4x -> $%.08x = "%s"+$%.04x%s', [
          i+1,
          SymbolReference.Offset,
          AllSymbolReferences[CurrentLocation.SymbolReferencesIndex+i].Address,
          AllSymbolReferences[CurrentLocation.SymbolReferencesIndex+i].Symbol.Name,
          SymbolReference.BaseOffset,
          DuplicateString[AllSymbolReferences[CurrentLocation.SymbolReferencesIndex+i].MustSkip]
          ]);
      end;
    end;

    f := CurrentLocation.NextPotentialFunctionLocationIndex;

    // Limit the number of locations we show :
    Dec(c);
    if c = 0 then
    begin
      if f > 0 then
        DbgPrintf('  ...and more');

      Break;
    end;

  end;
end;

procedure TSymbolManager.TestAddressUsingPatternTrie(var aTestAddress: PByte; const DoForwardScan: Boolean = True);

  function _MayCheckFunction(const aStoredLibraryFunction: PStoredLibraryFunction): Boolean;
  var
    LengthWithoutPadding: Integer;
  begin
    // Don't count padding bytes (NOP and INT3 for now) at the end of the function (if any) :
    LengthWithoutPadding := aStoredLibraryFunction.FunctionLength;
    while (LengthWithoutPadding > 0) and (PBytes(aTestAddress)[LengthWithoutPadding-1] in [OPCODE_NOP, OPCODE_INT3]) do
      Dec(LengthWithoutPadding);

    // Skip small functions with only one symbol-reference, because those are
    // very common. Instead, we hope they will be discovered via other functions :
    case aStoredLibraryFunction.NrOfSymbolReferences of
      0: Result := (LengthWithoutPadding >= 7);
      1: Result := (LengthWithoutPadding > 5)
               and (aTestAddress^ <> OPCODE_JMP);
      2: Result := (LengthWithoutPadding >= 10);
    else
      Result := True;
    end;
  end;

  function _CountFunctionCRC(const aStoredLibraryFunction: PStoredLibraryFunction; const aAddress: PByte): Boolean;
  begin
    if aStoredLibraryFunction.CRCLength > 0 then
      Result := (aStoredLibraryFunction.CRCValue = CalcCRC16(aAddress, aStoredLibraryFunction.CRCLength))
    else
      Result := True;
  end;

  function _CheckAndRememberFunctionSymbolReferences(const aSymbol: TSymbolInformation): Boolean;
  var
    SymbolReference: PStoredSymbolReference;
    c, x, i: Integer;
  begin
    Result := (aSymbol.SymbolReferenceCount = 0);
    if Result then
      Exit;

    // Make sure there's space for the symbol-references :
    c := AllSymbolReferencesInUse + aSymbol.SymbolReferenceCount;
    while Length(AllSymbolReferences) < c do
      SetLength(AllSymbolReferences, Length(AllSymbolReferences) * 2);
    c := AllSymbolReferencesInUse;

    // Read all symbol-references and check if they point to a valid address :
    for x := 0 to aSymbol.SymbolReferenceCount - 1 do
    begin
      SymbolReference := aSymbol.SymbolReferences[x];

      AllSymbolReferences[c+x].Symbol := FindOrAddSymbol(SymbolReference.NameIndex, {Function=}nil);

      // Determine which references shouldn't be checked further :
      AllSymbolReferences[c+x].MustSkip := False
        // Skip Structured Exception Handling labels (would generate lots of alternatives) :
        or (strncmp(PChar(AllSymbolReferences[c+x].Symbol.Name), '__SEH', Length('__SEH')) = 0)
        // Skip kernel call references (for now, but can be resolved later) :
        or (strncmp(PChar(AllSymbolReferences[c+x].Symbol.Name), '__imp__', Length('__imp__')) = 0);

      // Check that any duplicate symbol-references are all pointing to the same address :
      if AllSymbolReferences[c+x].MustSkip then
        Continue;

      // If a symbol-reference falls outside the valid range, we consider this a false hit :
      AllSymbolReferences[c+x].Address := GetReferencedSymbolAddress(aTestAddress, SymbolReference);
      if (AllSymbolReferences[c+x].Address = nil) then
        Exit;

      // Self-references should always point to the test-address :
      if AllSymbolReferences[c+x].Symbol = aSymbol then
        if AllSymbolReferences[c+x].Address <> aTestAddress then
          Exit;

      begin
        for i := 0 to x - 1 do
        begin
          if (AllSymbolReferences[c+i].Symbol = AllSymbolReferences[c+x].Symbol) then
          begin
            AllSymbolReferences[c+i].MustSkip := True;
            // Same symbol, but different address-reference means a false hit, which must be skipped :
            if (AllSymbolReferences[c+i].Address <> AllSymbolReferences[c+x].Address) then
              Exit;
          end;
        end; // for prior SymbolReferences
      end;

    end; // for SymbolReferences

    Result := True;
  end; // _CheckAndRememberFunctionSymbolReferences

  function _CheckForwardFunctionSymbolReferences(const aStoredLibraryFunction: PStoredLibraryFunction;
    const aSymbolReferencesIndex: Integer): Boolean;
  var
    x: Integer;
    ReferencedAddress: PByte;
  begin
    for x := 0 to aStoredLibraryFunction.NrOfSymbolReferences - 1 do
    begin
      if AllSymbolReferences[aSymbolReferencesIndex+x].MustSkip then
        Continue;

      // Determine which referenced address is to be scanned too :
      ReferencedAddress := AllSymbolReferences[aSymbolReferencesIndex+x].Address;

      // Do not test symbols on the originating address, as we're probably
      // still busy scanning for all possible symbols that could reside there :
      if ReferencedAddress = FCurrentTestAddress then
        Continue;

      // Only scan at addresses where there might be code (as we're about to check
      // if this address does indeed validate against the symbol's function-pattern) :
      if IsAddressWithinCodeRange(ReferencedAddress) then
      begin
        TestAddressUsingPatternTrie({var}ReferencedAddress, {DoForwardScan=}False);
        // Referenced address was changed, re-apply the actual address :
        ReferencedAddress := AllSymbolReferences[aSymbolReferencesIndex+x].Address;

        // Now see if the referenced address does indeed matches the symbol being referenced:
        if AllSymbolReferences[aSymbolReferencesIndex+x].Symbol.HasFunctionInformation then
          // If that symbol doesn't occur at the referenced address, we quit here :
          if AllSymbolReferences[aSymbolReferencesIndex+x].Symbol.FindPotentialLocationIndex(ReferencedAddress) = 0 then
          begin
            Result := False;
            Exit;
          end;
      end;
    end;

    Result := True;
  end; // _CheckForwardFunctionSymbolReferences

  procedure _CheckLeafFunctions(aLeafFunctionIndexes: PFunctionIndex; NrChildren: Integer; const aAddress: PByte);
  var
    i: Integer;
    aStoredLibraryFunction: PStoredLibraryFunction;
    FunctionName: string;
    Symbol: TSymbolInformation;
    Location: PPotentialFunctionLocation;
  begin
    // Check the possible functions further, and find the most number of seemingly valid symbol-references :
    for i := 0 to NrChildren - 1 do
    begin
      aStoredLibraryFunction := PatternTrieReader.GetStoredLibraryFunction(aLeafFunctionIndexes^);
{$IFDEF DXBX_RECTYPE}
      Assert(aStoredLibraryFunction.RecType = rtStoredLibraryFunction, 'StoredLibraryFunction type mismatch!');
{$ENDIF}

      FunctionName := PatternTrieReader.GetFunctionName(aLeafFunctionIndexes^);

      // Check if this functions' library matches an active one :
      if (aStoredLibraryFunction.AvailableInVersions * LibraryVersionsToScan) <> [] then
      if _MayCheckFunction(aStoredLibraryFunction) then
      // Check if the CRC holds :
      if _CountFunctionCRC(aStoredLibraryFunction, aAddress) then
      // TODO : Include data & test-code for 'trailing bytes' patterns (or extend the CRC-range).
      begin
        Symbol := FindOrAddSymbol(FunctionName, aStoredLibraryFunction);

        // Check if all symbol-references are within the expected memory-range :
        if _CheckAndRememberFunctionSymbolReferences(Symbol) then
        begin
          // Only now add the current location as a candidate for this function :
          Location := SymbolManager.AddPotentialSymbolLocation(aTestAddress, Symbol);

          // Persist the symbol-references determined for this location :
          if aStoredLibraryFunction.NrOfSymbolReferences > 0 then
          begin
            Location.SymbolReferencesIndex := AllSymbolReferencesInUse;
            Inc(AllSymbolReferencesInUse, aStoredLibraryFunction.NrOfSymbolReferences);

            if DoForwardScan then
              if not _CheckForwardFunctionSymbolReferences(aStoredLibraryFunction, Location.SymbolReferencesIndex) then
                // If the forward function scan fails, invalidate this location (but keep checking other children!) :
                Location.Address := nil;
          end;
        end;
      end;

      // Skip to the next stored library function (including a step over all symbol-references) :
      Inc(UIntPtr({var}aStoredLibraryFunction),
        SizeOf(RStoredLibraryFunction) +
        (SizeOf(RStoredSymbolReference) * aStoredLibraryFunction.NrOfSymbolReferences)
        );

      // Step to the next function in the leaf :
      Inc(aLeafFunctionIndexes);
    end; // for NrChildren
  end; // _CheckLeafFunctions

  function _TryMatchingNode(aStoredTrieNode: PStoredTrieNode; aAddress: PByte; Depth: Integer): Boolean;
  var
    NrChildren: Integer;
    StretchPtr: PByte;
    StretchHeaderByte: TStretchHeaderByte;
    More: Boolean;
    NrFixed, NrWildcards, i: Integer;
    NextOffset: TByteOffset;
  begin
    Result := False;
{$IFDEF DXBX_RECTYPE}
    Assert(aStoredTrieNode.RecType = rtStoredTrieNode, 'StoredTrieNode type mismatch!');
{$ENDIF}
    // Calculate the position of the data after this TrieNode (StretchPtr) :
    NrChildren := aStoredTrieNode.NrChildrenByte1;
    UIntPtr(StretchPtr) := UIntPtr(aStoredTrieNode) + SizeOf(RStoredTrieNode);
    if NrChildren >= 128 then
      // Reconstruct the NrChildren value :
      NrChildren := (Integer(NrChildren and 127) shl 8) or aStoredTrieNode.NrChildrenByte2
    else
      // If one byte was sufficient, then the next stretch starts 1 byte earlier :
      Dec(UIntPtr(StretchPtr), SizeOf({RStoredTrieNode.NrChildrenByte2:}Byte));

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
      _CheckLeafFunctions(PFunctionIndex(StretchPtr), NrChildren, aAddress);
      // Stop recursion, by returning True :
      Result := True;
      Exit; // leave _TryMatchingNode
    end; // if Depth

    // When we're somewhere in the node-trie, try to match one of the child nodes :
    aStoredTrieNode := Pointer(StretchPtr);
    while NrChildren > 0 do
    begin
      // Try to match pattern on this node, stop if we can't go any deeper :
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
  if not MyAddressesScanned[Integer(aTestAddress)] then
  begin
    MyAddressesScanned[Integer(aTestAddress)] := True;

    // Search if this address matches a pattern :
    Node := PatternTrieReader.GetNode(PatternTrieReader.StoredSignatureTrieHeader.TrieRootNode);

    _TryMatchingNode(Node, aTestAddress, 0);
  end;

  Inc({var}aTestAddress);
end; // TestAddressUsingPatternTrie

procedure TSymbolManager.ScanMemoryRangeForLibraryPatterns(const pXbeHeader: PXBEIMAGE_HEADER);
var
  i: DWord;
  Section: PXBE_SECTIONHEADER;
  ScanEnd: UIntPtr;
  p: UIntPtr;

  procedure _CheckStringToFunctionMappings;
  var
    s, f, i: Integer;
    Str: AnsiString;
    sh: PStoredStringHeader;
    FuncName: AnsiString;
  begin
    for s := 0 to PatternTrieReader.StoredSignatureTrieHeader.StringTable.NrOfStrings - 1 do
    begin
      sh := PatternTrieReader.GetStringHeader(s);
      str := PatternTrieReader.GetString(s);
      for f := 0 to Integer(sh.NrOfFunctions) - 1 do
      begin
        FuncName := PatternTrieReader.GetFunctionName(sh.FirstFunctionIndex + f);
        Assert(FuncName = Str);
      end;
    end;
  end;

  procedure _ScanTest;

    procedure _Test(const aAddress: UIntPtr; const aSymbolName: string);
    begin
      DbgPrintf('Testing at 0x%.08x for "%s"', [aAddress, aSymbolName]);
      p := aAddress;
      TestAddressUsingPatternTrie(PByte(p));
      _Debug(FindSymbol(aSymbolName));
    end;

  begin
(*
    // Turok - Fixed wrong locations and duplicates :
    _Test($0001171D, 'XapiInitProcess'); // TODO : Use correct pattern-name!
    _Test($00011A35, '_RaiseException@16');
    _Test($00011A9D, '_XRegisterThreadNotifyRoutine@8');
    _Test($001939B0, 'EmuIDirect3DDevice8_SetRenderState_StencilEnable'); // TODO : Use correct pattern-name!
    _Test($00193A40, 'EmuIDirect3DDevice8_SetRenderState_StencilFail'); // TODO : Use correct pattern-name!
    _Test($00193AB0, 'EmuIDirect3DDevice8_SetRenderState_YuvEnable'); // TODO : Use correct pattern-name!
    _Test($00193AE0, 'EmuIDirect3DDevice8_SetRenderState_OcclusionCullEnable'); // TODO : Use correct pattern-name!
    _Test($00193B50, 'EmuIDirect3DDevice8_SetRenderState_StencilCullEnable'); // TODO : Use correct pattern-name!
    _Test($00193BC0, 'EmuIDirect3DDevice8_SetRenderState_RopZCmpAlwaysRead'); // TODO : Use correct pattern-name!
    _Test($00193BE0, 'EmuIDirect3DDevice8_SetRenderState_RopZRead'); // TODO : Use correct pattern-name!
    _Test($00194EA0, '_D3DDevice_SetLight@8');
    _Test($00195080, '_D3DDevice_LightEnable@8');
    _Test($00195170, '?SetTexture@D3DDevice@@SGJKPAUD3DBaseTexture@@@Z');
    _Test($001960E0, 'EmuIDirect3DBaseTexture8_GetLevelCount'); // TODO : Use correct pattern-name!
    _Test($001997F0, '?Get2DSurfaceDesc@PixelJar@D3D@@YGXPAUD3DPixelContainer@@IPAU_D3DSURFACE_DESC@@@Z'); //HLE: 0x001997F0 -> EmuGet2DSurfaceDesc
    _Test($0019D360, '_D3DDevice_Reset@4'); //HLE: 0x0019D360 -> EmuIDirect3DDevice8_Reset
    _Test($001B018A, 'CDirectSound_SetRolloffFactor'); // TODO : Use correct pattern-name!
    _Test($001F35E3, '_USBD_Init@8'); // Cxbx incorrectly calls this XInitDevices!

    // Cxbx meshes :
    _Test($0001DE60, '_D3DDevice_SetTile@8'); // EmuIDirect3DDevice8_SetTile
    _Test($00020200, '?SetFence@D3D@@YGKK@Z'); // D3D::SetFence (XREF)
    _Test($0002CC34, '_XapiProcessHeap');

    // Compiled Xdk samples\Tutorials\Tut01_CreateDevice\Release :
    _Test($0001F2C0, '?GetSurfaceFormat@PixelJar@D3D@@YGKPAUD3DPixelContainer@@0@Z'); // from EmuIDirect3DDevice8_Clear
    _Test($00019610, '_D3DDevice_Clear@24'); // EmuIDirect3DDevice8_Clear
    // Psx :
    _Test($00114A09, '_IDirectSoundBuffer_Play@16');
    _Test($00115AC2, '_DirectSoundCreate@12');
    // Gamepad
    _Test($00016FBF, '_XapiInitProcess@0');
    _Test($00016966, '_SetLastError@4');
    _Test($000340B3, '_XInputOpen@16');
*)
  end; // _ScanTest

  function _SectionCanContainCode(const aSection: PXBE_SECTIONHEADER): Boolean;
  var
    SectionName: string;
  begin
    SectionName := string(PAnsiChar(aSection.dwSectionNameAddr));
    // TODO : Improve the check if a section can contain code,
    // for now skip resource sections (beginning with an '$') :
    Result := (SectionName[1] <> '$')
          and (aSection.dwSizeofRaw > 0); // only when there's something in it!
  end;

var
  SectionLower, SectionUpper: UIntPtr;
begin
  Assert(Assigned(pXbeHeader));
  Assert(pXbeHeader.dwSections > 0);

  // Determine upper bound for scanning, based on the XBE sections :
  ScanUpper := Low(ScanUpper);
  UIntPtr(Section) := UIntPtr(pXbeHeader) + pXbeHeader.dwSectionHeadersAddr - pXbeHeader.dwBaseAddr;
  for i := 0 to pXbeHeader.dwSections - 1 do
  begin
    if ScanUpper < UIntPtr(Section.dwVirtualAddr) + Section.dwVirtualSize then
      ScanUpper := UIntPtr(Section.dwVirtualAddr) + Section.dwVirtualSize;

    Inc(Section);
  end;

  // Reserve a bit per address to see which addresses are already scanned :
  MyAddressesScanned.Size := 0;
  MyAddressesScanned.Size := ScanUpper;

  // Reserve a bit per address to see which addresses might contain code :
  MyAddressesPotentiallyContainingCode.Size := 0;
  MyAddressesPotentiallyContainingCode.Size := ScanUpper;

  // Determine where all code can reside :
  UIntPtr(Section) := UIntPtr(pXbeHeader) + pXbeHeader.dwSectionHeadersAddr - pXbeHeader.dwBaseAddr;
  for i := 0 to pXbeHeader.dwSections - 1 do
  begin
    if _SectionCanContainCode(Section) then
    begin
      // Calculate the range in which code can fall :
      SectionLower := UIntPtr(Section.dwVirtualAddr);
      SectionUpper := UIntPtr(Section.dwVirtualAddr) + Section.dwSizeofRaw;

      // Mark all bytes that might contain code :
      while SectionLower <= SectionUpper do
      begin
        MyAddressesPotentiallyContainingCode[Integer(SectionLower)] := True;
        Inc(SectionLower);
      end;
    end;

    Inc(Section);
  end;

  // Allocate the symbol-references pool :
  SetLength(AllSymbolReferences, 1024);
  ZeroMemory(@(AllSymbolReferences[0]), Length(AllSymbolReferences) * SizeOf(AllSymbolReferences[0]));
  AllSymbolReferencesInUse := 1; // Start at index 1, so index 0 means 'no value'

  // _CheckStringToFunctionMappings;

  _ScanTest;

  // Do the actual scanning per section :
  UIntPtr(Section) := UIntPtr(pXbeHeader) + pXbeHeader.dwSectionHeadersAddr - pXbeHeader.dwBaseAddr;
  for i := 0 to pXbeHeader.dwSections - 1 do
  begin
    // Only for the sections that can contain code :
    if _SectionCanContainCode(Section) then
    begin
      p := UIntPtr(Section.dwVirtualAddr);
      ScanEnd := UIntPtr(Section.dwVirtualAddr) + Section.dwSizeofRaw; // Don't scan outside of raw size!

{$IFDEF DXBX_DEBUG}
      DbgPrintf('DxbxHLE : Detecting functions in section $%0.4x (%s) from $%.8x to $%.8x', [
        i, string(PAnsiChar(Section.dwSectionNameAddr)), p, ScanEnd]);
{$ENDIF}

      while p < ScanEnd do
      try
        // Store the addres we're about to scan, so it can
        // be used in _CheckForwardFunctionSymbolReferences :
        FCurrentTestAddress := PByte(p);
        // Now scan this address, collecting all symbols :
        TestAddressUsingPatternTrie({var}PByte(p));
      except
{$IFDEF DXBX_DEBUG}
        DbgPrintf('DxbxHLE : Exception while scanning on address $%.8x', [p]);
{$ENDIF}
        Inc(p);
      end;
    end;

    Inc(Section);
  end;

  // Set of 'potentially code'-addresses is no longer needed :
  MyAddressesPotentiallyContainingCode.Size := 0;
end; // ScanMemoryRangeForLibraryPatterns

procedure TSymbolManager.DetectVersionedXboxLibraries(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBEIMAGE_HEADER);
var
  CurrentXbeLibraryVersion: PXBE_LIBRARYVERSION;
  CurrentLibName: string;
  StoredLibrary: PStoredLibrary;
  StoredLibraryName: string;
  i, j: Integer;
  StoredLibraryVersions: TStringList;
  BestStoredLibraryIndex: Integer;
  PrevBestStoredLibraryIndex: Integer;
begin
  // Loop over all libraries :
  CurrentXbeLibraryVersion := pLibraryVersion;
  if not Assigned(CurrentXbeLibraryVersion) then
  begin
    DbgPrintf('DxbxHLE : No XBE library versions to scan!');
    Exit;
  end;

  StoredLibraryVersions := TStringList.Create;

  // Loop over all library versions in the executable:
  for i := 0 to pXbeHeader.dwLibraryVersions - 1 do
  begin
    CurrentLibName := string(PCharToString(@(CurrentXbeLibraryVersion.szName[0]), XBE_LIBRARYNAME_MAXLENGTH));
    DbgPrintf('DxbxHLE : Library "%s" is version %d', [CurrentLibName, CurrentXbeLibraryVersion.wBuildVersion]);

    // Collect all known library-numbers with the same name in a stringlist :
    StoredLibraryVersions.Clear;
    for j := 0 to PatternTrieReader.StoredSignatureTrieHeader.LibraryTable.NrOfLibraries - 1 do
    begin
      StoredLibrary := PatternTrieReader.GetStoredLibrary(j);
      StoredLibraryName := PatternTrieReader.GetString(StoredLibrary.LibNameIndex);
      if SameLibName(StoredLibraryName, CurrentLibName) then
        StoredLibraryVersions.AddObject(IntToStr(StoredLibrary.LibVersion), TObject(j));
    end;

    PrevBestStoredLibraryIndex := -1;
    BestStoredLibraryIndex := -1;

    // Sort the list and search (nearest) index of the needed version :
    if StoredLibraryVersions.Count > 0 then
    begin
      StoredLibraryVersions.Sort;
      if StoredLibraryVersions.Find(IntToStr(CurrentXbeLibraryVersion.wBuildVersion), {var}j) then
        // Exact hit :
        BestStoredLibraryIndex := Integer(StoredLibraryVersions.Objects[j])
      else
      begin
        // No exact hit, try a range of the surrounding two versions if possible :
        if j <= 0 then
          j := 0
        else
          PrevBestStoredLibraryIndex := Integer(StoredLibraryVersions.Objects[j-1]);

        BestStoredLibraryIndex := Integer(StoredLibraryVersions.Objects[j]);
      end;
    end;

    if BestStoredLibraryIndex >= 0 then
    begin
      StoredLibrary := PatternTrieReader.GetStoredLibrary(BestStoredLibraryIndex);
      StoredLibraryName := PatternTrieReader.GetString(StoredLibrary.LibNameIndex);
      if SameLibName(StoredLibraryName, 'D3D8') then
      begin
        LibD3D8 := StoredLibrary;
        g_BuildVersion := LibD3D8.LibVersion;
      end;

      // Add this library to a set we'll use in the detection-code :
      LibraryVersionsToScan := LibraryVersionsToScan + [LibraryVersionNumberToFlag(StoredLibrary.LibVersion)];
      if StoredLibrary.LibVersion = CurrentXbeLibraryVersion.wBuildVersion then
        DbgPrintf('... Got patterns for exactly this version!')
      else
      begin
        DbgPrintf('... Approximating this with patterns from version %d', [StoredLibrary.LibVersion]);
        if PrevBestStoredLibraryIndex > -1 then
        begin
          StoredLibrary := PatternTrieReader.GetStoredLibrary(PrevBestStoredLibraryIndex);
          LibraryVersionsToScan := LibraryVersionsToScan + [LibraryVersionNumberToFlag(StoredLibrary.LibVersion)];
          DbgPrintf('    and version %d.', [StoredLibrary.LibVersion]);
        end;
      end;
    end
    else
      DbgPrintf('... No patterns registered for this library!');

    // Skip to the next library :
    Inc(CurrentXbeLibraryVersion);
  end; // for all library versions

  FreeAndNil(StoredLibraryVersions);
end; // DetectVersionedXboxLibraries

procedure TSymbolManager.DetermineFinalLocations;

  procedure _FinalTest;

    procedure _Test(const aAddress: UIntPtr; const aSymbolName: string);
    begin
      DbgPrintf('Testing at 0x%.08x for "%s"', [aAddress, aSymbolName]);
      _Debug(FindSymbol(aSymbolName));
    end;

  begin
(*
    // Turok - Fixed wrong locations and duplicates :
    _Test(?, '_D3D__RenderState'));
    _Test(?, '?CommonSetDebugRegisters@D3D@@YIXXZ'));
    _Test(?, '_XapiProcessHeap'));
    _Test($00193BC0, '_D3DDevice_SetRenderState_RopZCmpAlwaysRead@4'));
    _Test($00193BE0, '_D3DDevice_SetRenderState_RopZRead@4'));
    _Test($00193C00, '_D3DDevice_SetRenderState_DoNotCullUncompressed@4'));
    _Test($00194EA0, '_D3DDevice_SetLight@8');
    _Test($00195080, '_D3DDevice_LightEnable@8');
    _Test($001997F0, '?Get2DSurfaceDesc@PixelJar@D3D@@YGXPAUD3DPixelContainer@@IPAU_D3DSURFACE_DESC@@@Z'); //HLE: 0x001997F0 -> EmuGet2DSurfaceDesc
    _Test($0019D360, '_D3DDevice_Reset@4'); //HLE: 0x0019D360 -> EmuIDirect3DDevice8_Reset
    // Compiled Xdk samples\Tutorials\Tut01_CreateDevice\Release :
    _Test($00019610, '_D3DDevice_Clear@24'); // EmuIDirect3DDevice8_Clear
    // Psx :
    _Test($00115AC2, '_DirectSoundCreate@12');
    // Gamepad
    _Test($00016FBF, '_XapiInitProcess@0');
    _Test($00016966, '_SetLastError@4');
    _Test($000340B3, '_XInputOpen@16');
*)
  end; // _FinalTest

  procedure _AddMissingSymbols;
  var
    w: Word;
    CurrentSymbol: TSymbolInformation;
    i: Integer;
    CurrentLocation: PPotentialFunctionLocation;
    x: Integer;
    ReferencedSymbolInformation: TSymbolInformation;
    ReferencedSymbolAddress: TCodePointer;
    ReferencedSymbolLocation: PPotentialFunctionLocation;
  begin
    DbgPrintf('DxbxHLE : Adding missing symbols');
    // Loop over all symbols :
    for w := Low(w) to High(w) do
    begin
      CurrentSymbol := MySymbolsHashedOnName[w];
      if (CurrentSymbol = nil) then
        Continue;
      if (not CurrentSymbol.HasSymbolReferences) then
        Continue;

      // Iterate over all potential locations of this symbol :
      i := CurrentSymbol.FirstPotentialFunctionLocationIndex;
      while i > 0 do
      begin
        CurrentLocation := @(MyPotentialFunctionLocations[i]);

        // For now, skip locations that where added later and thus have no symbol-references cached :
        if CurrentLocation.SymbolReferencesIndex > 0 then
        begin
          // Loop over each symbol-reference reachable from this symbol-address :
          for x := 0 to CurrentSymbol.SymbolReferenceCount - 1 do
          begin
            if AllSymbolReferences[CurrentLocation.SymbolReferencesIndex + x].MustSkip then
              Continue;

            ReferencedSymbolAddress := AllSymbolReferences[CurrentLocation.SymbolReferencesIndex + x].Address;
            ReferencedSymbolInformation := AllSymbolReferences[CurrentLocation.SymbolReferencesIndex + x].Symbol;

            if ReferencedSymbolInformation.HasFunctionInformation then
            begin
              // A cross referenced symbol with function-information should
              // have been detected at the referenced addres :
              ReferencedSymbolLocation := ReferencedSymbolInformation.FindPotentialLocation(ReferencedSymbolAddress);
              // If there's no potential location at that address for that symbol :
              if ReferencedSymbolLocation = nil then
              begin
                // Then we invalidate the originating location :
                CurrentLocation.Address := nil;
                Break;
              end;
            end
            else
              // Add all symbol-references that haven't been detected as function yet (ONLY if it must be data!) :
              if ReferencedSymbolInformation.IsSymbolNameUsedInFunctions then
                ReferencedSymbolLocation := nil
              else
              begin
                ReferencedSymbolLocation := ReferencedSymbolInformation.AddPotentialLocation(ReferencedSymbolAddress);

                // Re-determine the CurrentLocation, as the above add might have enlarged the
                // MyPotentialFunctionLocations, invalidating the pointer we're working with :
                CurrentLocation := @(MyPotentialFunctionLocations[i]);
              end;

            // Count all references :
            if Assigned(ReferencedSymbolLocation) then
              Inc(ReferencedSymbolLocation.NrOfReferencesTo);
          end; // for SymbolReferences
        end;

        i := CurrentLocation.NextPotentialFunctionLocationIndex;
      end; // while Locations
    end; // for Symbols
  end; // _AddMissingSymbols

  procedure _SelectBestLocation;
  var
    w: Word;
    CurrentSymbol: TSymbolInformation;
    BestLocationIndex: Integer;
    NrOfReferencesToBestLocation: Integer;
    i: Integer;
  begin
    DbgPrintf('DxbxHLE : Selecting best locations');
    // Loop over all symbols and select their best address :
    for w := Low(w) to High(w) do
    begin
      CurrentSymbol := MySymbolsHashedOnName[w];
      if CurrentSymbol = nil then
        Continue;

      BestLocationIndex := 0;
      NrOfReferencesToBestLocation := -1;

      // Iterate over all locations for this symbol :
      i := CurrentSymbol.FirstPotentialFunctionLocationIndex;
      while i > 0 do
      begin
        if NrOfReferencesToBestLocation < MyPotentialFunctionLocations[i].NrOfReferencesTo then
        begin
          NrOfReferencesToBestLocation := MyPotentialFunctionLocations[i].NrOfReferencesTo;
          BestLocationIndex := i;
        end;

        i := MyPotentialFunctionLocations[i].NextPotentialFunctionLocationIndex;
      end; // while locations

      if BestLocationIndex > 0 then
        CurrentSymbol.Address := MyPotentialFunctionLocations[BestLocationIndex].Address;
    end; // for Symbols
  end; // _SelectBestLocation

  function _RemoveIncorrectAlternatives: Boolean;
  var
    w: Word;
    CurrentSymbol: TSymbolInformation;
    i: Integer;
    PrevLocation: PPotentialFunctionLocation;
    CurrentLocation: PPotentialFunctionLocation;
    Remove: Boolean;
    x: Integer;
    ReferencedSymbolInformation: TSymbolInformation;
    ReferencedSymbolAddress: TCodePointer;
  begin
    DbgPrintf('DxbxHLE : Removing incorrect alternatives');
    Result := False;
    // Loop over all symbols :
    for w := Low(w) to High(w) do
    begin
      CurrentSymbol := MySymbolsHashedOnName[w];
      if (CurrentSymbol = nil) then
        Continue;
      if (not CurrentSymbol.HasSymbolReferences) then
        Continue;

      // Iterate over all potential locations of this symbol :
      i := CurrentSymbol.FirstPotentialFunctionLocationIndex;
      PrevLocation := nil;
      while i > 0 do
      begin
        CurrentLocation := @(MyPotentialFunctionLocations[i]);
        i := CurrentLocation.NextPotentialFunctionLocationIndex;

        // Loop over each symbol-reference reachable from this symbol-address :
        Remove := False;
        if  (CurrentLocation.SymbolReferencesIndex > 0)
        and (CurrentLocation.Address = CurrentSymbol.Address) then
        begin
          for x := 0 to CurrentSymbol.SymbolReferenceCount - 1 do
          begin
            if AllSymbolReferences[CurrentLocation.SymbolReferencesIndex + x].MustSkip then
              Continue;

            // Check if one of the potential locations of the referenced symbol is detected at the referenced address :
            ReferencedSymbolAddress := AllSymbolReferences[CurrentLocation.SymbolReferencesIndex + x].Address;
            ReferencedSymbolInformation := AllSymbolReferences[CurrentLocation.SymbolReferencesIndex + x].Symbol;
            Assert(Assigned(ReferencedSymbolInformation));

//            if ReferencedSymbolInformation.HasFunctionInformation then
//            if ReferencedSymbolInformation.HasPotentialLocations then
            if ReferencedSymbolInformation.Address <> ReferencedSymbolAddress then
            begin
              // Discard this alternative if the above check failed :
              Remove := True;
              Break;
            end;
          end; // for SymbolReferences
        end;

        if Remove then
        begin
          // Invalidate this potential location :
          CurrentLocation.Address := nil;
          CurrentLocation.Symbol := nil;
          CurrentLocation.NrOfReferencesTo := 0;
          CurrentLocation.SymbolReferencesIndex := 0;
          CurrentLocation.NextPotentialFunctionLocationIndex := 0;

          // Decouple current alternative :
          if Assigned(PrevLocation) then
            PrevLocation.NextPotentialFunctionLocationIndex := i
          else
            CurrentSymbol.FirstPotentialFunctionLocationIndex := i;

          // Signal a change :
          Result := True;
        end
        else
          PrevLocation := CurrentLocation;
      end; // while Locations
    end; // for Symbols
  end; // _RemoveIncorrectAlternatives

  procedure _PutSymbolsInList;

    function _Compare_Symbol_Address(Item1, Item2: Pointer): Integer;
    begin
      Result := Integer(TSymbolInformation(Item1).Address) - Integer(TSymbolInformation(Item2).Address);
    end;

  var
    w: Word;
    CurrentSymbol: TSymbolInformation;
  begin
    DbgPrintf('DxbxHLE : Collecting final symbols');
    MyFinalLocations.Count := MySymbolCount;
    MySymbolCount := 0;

    // Finally, loop over all symbols and put them in one big list :
    for w := Low(w) to High(w) do
    begin
      CurrentSymbol := MySymbolsHashedOnName[w];
      if (CurrentSymbol = nil)
      or (CurrentSymbol.Address = nil) then
        Continue;

      MyFinalLocations[MySymbolCount] := CurrentSymbol;
      Inc(MySymbolCount);
    end;

    MyFinalLocations.Count := MySymbolCount;

    // Sort that list
    MyFinalLocations.Sort(@_Compare_Symbol_Address);
  end; // _PutSymbolsInList

  procedure _PrintLocationList;
  var
    i: Integer;
    CurrentSymbol: TSymbolInformation;
    Name: string;
  begin
    for i := 0 to Count - 1 do
    begin
      CurrentSymbol := Locations[i];

      Name := DxbxUnmangleSymbolName(CurrentSymbol.Name);
      if (CurrentSymbol.StoredLibraryFunction = nil)
      or (CurrentSymbol.FirstPotentialFunctionLocationIndex = 0) then
        Name := Name + ' (XRef)';

      DbgPrintf('Found at 0x%.8x-0x%.8x : %s  [%s]',
        [CurrentSymbol.Address,
         UIntPtr(CurrentSymbol.Address) + UInt(CurrentSymbol.Length) - 1,
         Name,
         CurrentSymbol.Name]);
    end; // for Symbols
  end; // _PrintLocationList

var
  i: Integer;
begin // DetermineFinalLocations

  _AddMissingSymbols;

  _FinalTest;

  repeat
    _SelectBestLocation();
  until _RemoveIncorrectAlternatives() = False;

  _FinalTest;

  _PutSymbolsInList;

  _PrintLocationList;

  // TODO :
  // - fix cache-loading
end; // DetermineFinalLocations

procedure TSymbolManager.DetermineSpecialSymbols;
var
  Symbol: TSymbolInformation;
  s, v: int;
begin
  DbgPrintf('DxbxHLE : Determining special symbols');
  // locate D3DDeferredTextureState
  begin
    XTL_EmuD3DDeferredTextureState := nil;
    Symbol := FindSymbol('_D3D__TextureState');
    if Assigned(Symbol) then
      XTL_EmuD3DDeferredTextureState := Symbol.Address;

    if Assigned(XTL_EmuD3DDeferredTextureState) then
    begin
      for s := 0 to 4-1 do
      begin
        for v := 0 to 32-1 do
          XTL_EmuD3DDeferredTextureState[v+s*32] := X_D3DTSS_UNK;
      end;

      DbgPrintf('HLE: $%.08X -> EmuD3DDeferredTextureState',
        [XTL_EmuD3DDeferredTextureState]);
    end
    else
      EmuWarning('EmuD3DDeferredTextureState was not found!');
  end;

  // locate D3DDeferredRenderState
  begin
    XTL_EmuD3DDeferredRenderState := nil;
    // Just search for _D3D__RenderState itself !
    Symbol := FindSymbol('_D3D__RenderState');
    if Assigned(Symbol) then
      XTL_EmuD3DDeferredRenderState := Symbol.Address;

    if Assigned(XTL_EmuD3DDeferredRenderState) then
    begin
      // Calculate the location of D3DDeferredRenderState via an XDK-dependent offset to _D3D__RenderState :
      // Dxbx note : XTL_EmuD3DDeferredRenderState:PDWORDs cast to UIntPtr to avoid incrementing with that many array-sizes!
      if LibD3D8.LibVersion <= 3925 then
        Inc(UIntPtr(XTL_EmuD3DDeferredRenderState), X_D3DRS_DEFERRED_START_3925 * 4)
      else if LibD3D8.LibVersion <= 4361 then
        Inc(UIntPtr(XTL_EmuD3DDeferredRenderState), X_D3DRS_DEFERRED_START_4361 * 4)
      else if LibD3D8.LibVersion <= 4432 then
        Inc(UIntPtr(XTL_EmuD3DDeferredRenderState), X_D3DRS_DEFERRED_START_4432 * 4)
      else if LibD3D8.LibVersion <= 5933 then
        Inc(UIntPtr(XTL_EmuD3DDeferredRenderState), X_D3DRS_DEFERRED_START_5933 * 4)
      else
        ; // Dxbx : What now?!

      for v := 0 to X_D3DRS_DEFERRED_SIZE_5933 - 1 do // TODO : Use the actual number per SDK version
        XTL_EmuD3DDeferredRenderState[v] := X_D3DRS_UNK;

{$IFDEF DEBUG}
      DbgPrintf('HLE: $%.08X -> EmuD3DDeferredRenderState', [XTL_EmuD3DDeferredRenderState]);
{$ENDIF}
    end
    else
      EmuWarning('EmuD3DDeferredRenderState was not found!');
  end;

  // locate XapiProcessHeap
  begin
    // Resolve the address of the _XapiProcessHeap symbol (at least referenced once, from XapiInitProcess) :
    Symbol := FindSymbol('_XapiProcessHeap');
    if Assigned(Symbol) then
      // and remember that in a global :
      XTL_EmuXapiProcessHeap := Symbol.Address;

{$IFDEF DXBX_DEBUG}
    if Assigned(XTL_EmuXapiProcessHeap) then
      DbgPrintf('HLE: $%.08X -> XapiProcessHeap',
        [XTL_EmuXapiProcessHeap])
    else
      DbgPrintf('HLE : Can''t find XapiProcessHeap!');
{$ENDIF}
  end;
end; // DetermineSpecialSymbols

procedure TSymbolManager.DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBEIMAGE_HEADER);
var
  ResourceStream: TResourceStream;
  CacheFileNameStr: string;
begin
  Clear;

  // Get StoredPatternTrie from resource :
  ResourceStream := TResourceStream.Create(LibModuleList.ResInstance, 'StoredPatternTrie', RT_RCDATA);
  try
    PatternTrieReader := TPatternTrieReader.Create;
    try
      PatternTrieReader.LoadFromStream(ResourceStream);

      DetectVersionedXboxLibraries(pLibraryVersion, pXbeHeader);

      // Note : Somehow, the output of CacheFileName() changes because of the
      // following code, that's why we put the initial filename in a variable :
      CacheFileNameStr := CacheFileName(pXbeHeader);

      // Try to load the symbols from the cache :
      if LoadSymbolsFromCache(CacheFileNameStr) then
        // If that succeeded, we don't have to scan and save anymore :
        CacheFileNameStr := ''
      else
      begin
        // Scan Patterns using this trie :
        ScanMemoryRangeForLibraryPatterns(pXbeHeader);

        DetermineFinalLocations();
      end;

      DetermineSpecialSymbols();

    finally
      FreeAndNil(PatternTrieReader);
    end;

  finally
    // Unlock the resource :
    FreeAndNil(ResourceStream);
  end;

{$IFDEF DXBX_DEBUG}
  DbgPrintf('DxbxHLE : Detected %d symbols, chosen from %d potential locations.', [Count, MyPotentialFunctionLocations_Count]);
{$ENDIF}

  // After detection of all symbols, see if we need to save that to cache :
  if CacheFileNameStr <> '' then
    SaveSymbolsToCache(CacheFileNameStr);

end; // DxbxScanForLibraryAPIs

class function TSymbolManager.CacheFileName(const pXbeHeader: PXBEIMAGE_HEADER): string;
begin
  CRC32Init();
  ReinitXbeImageHeader;
  Result := SymbolCacheFolder
          // TitleID
          + IntToHex(PXBE_CERTIFICATE(pXbeHeader.dwCertificateAddr).dwTitleId, 8)
          // + CRC32 over XbeHeader :
          + '_' + IntToHex(CRC32(PByte(pXbeHeader), {Len=}pXbeHeader.dwSizeofHeaders), 8)
          + SymbolCacheFileExt;
  ReinitExeImageHeader;
end;

function TSymbolManager.LoadSymbolsFromCache(const aCacheFile: string): Boolean;
var
  i: Integer;
  FuncStr: string;
  Addr: Pointer;
  CurrentSymbol: TSymbolInformation;
  Symbols: TStringList;
begin
  Result := False;
  if not FileExists(aCacheFile) then
    Exit;

  Symbols := TStringList.Create;
  try
    uXbe.LoadSymbolsFromCache(Symbols, aCacheFile);

    // Add each entry to the list :
    MyFinalLocations.Count := Symbols.Count;
    for i := 0 to Symbols.Count - 1 do
    begin
      FuncStr := Symbols[i];
      Addr := Pointer(Symbols.Objects[i]);
      if Assigned(Addr) then
      begin
        CurrentSymbol := RegisterSpecificFunctionLocation(FuncStr, Addr);
        CurrentSymbol.Address := Addr;
        MyFinalLocations[i] := CurrentSymbol;
        Result := True;
      end;
    end;
  finally
    FreeAndNil(Symbols);
  end;

{$IFDEF DXBX_DEBUG}
  if Result then
    DbgPrintf('DxbxHLE : Loaded symbols : %d.', [MyPotentialFunctionLocations_Count]);
{$ENDIF}
end; // LoadSymbolsFromCache

procedure TSymbolManager.SaveSymbolsToCache(const aCacheFile: string);
var
  i: Integer;
  Symbol: TSymbolInformation;
begin
  if not ForceDirectories(ExtractFilePath(aCacheFile)) then
    Exit;

  with TStringList.Create do
  try
    for i := 0 to Self.Count - 1 do
    begin
      Symbol := Locations[i];
      AddObject(Symbol.Name + ':$' + IntToHex(Integer(Symbol.Address), 8), TObject(Symbol.Address));
    end;

    CustomSort(@SortObjects);

    SaveToFile(aCacheFile);
  finally
    Free;
  end;
end; // SaveSymbolsToCache

initialization

  SymbolManager := TSymbolManager.Create;

finalization

  FreeAndNil(SymbolManager);

end.

