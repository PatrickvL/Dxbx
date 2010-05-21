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
  // 3rd Party
  JclWin32, // UNDNAME_COMPLETE
  JclPeImage, // UndecorateSymbolName
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
  uEmu, // EmuWarning
  uState, // XTL_EmuD3DDeferredRenderState and XTL_EmuD3DDeferredTextureState
  uEmuXapi, // XTL_EmuXapiProcessHeap
  uEmuD3D8Types;// X_D3DRS_UNK

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
    CrossReferencesIndex: Integer;
    NrOfReferencesTo: Integer;
    NextPotentialFunctionLocationIndex: Integer;

    property Length: Cardinal read GetLength;
    property CodeEnd: TCodePointer read GetCodeEnd;
  end; // SizeOf() = 16

  TSymbolInformation = class(TObject)
  private
    function GetLength: Cardinal;
    function GetCrossReferenceCount: Integer;
    function GetCrossReference(const aIndex: Integer): PStoredCrossReference;
  public
    NameIndex: TStringTableIndex;
    Name: string;
    Address: TCodePointer;
    FirstPotentialFunctionLocationIndex: Integer;
    StoredLibraryFunction: PStoredLibraryFunction; // can be nil for non-pattern symbols
    XRefCause: string;

    function FindPotentialLocationIndex(const aAddress: TCodePointer): Integer;
    function FindPotentialLocation(const aAddress: TCodePointer): PPotentialFunctionLocation;
    function AddPotentialLocation(const aAddress: TCodePointer): PPotentialFunctionLocation;

    property Length: Cardinal read GetLength;
    property CrossReferenceCount: Integer read GetCrossReferenceCount;
    property CrossReferences[const aIndex: Integer]: PStoredCrossReference read GetCrossReference;
  end;

  TSymbolManager = class(TObject)
  protected // Symbols :
    MySymbolHashTable: array [Word] of TSymbolInformation; // Hash table for all detected symbols, hashed on Name
    MySymbolCount: Integer; // Number of detected symbols (including cross-references)
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
    // TODO : Add array for other symbol locations (for which we have no patterns, but do know their symbol)
  protected // Final function locations :
    MyFinalLocations: TList;
    function GetCount: Integer;
    function GetLocation(const aIndex: Integer): TSymbolInformation;
  public
    property Count: Integer read GetCount;
    property Locations[const aIndex: Integer]: TSymbolInformation read GetLocation; default;
  protected
    ScanUpper: UIntPtr;
    PatternTrieReader: TPatternTrieReader;
    AllCrossReferences: array of record Symbol: TSymbolInformation; Address: TCodePointer; end;
    AllCrossReferencesInUse: Integer;
    procedure _Debug(const aSymbol: TSymbolInformation);
    function IsAddressWithinScanRange(const aAddress: TCodePointer): Boolean;
    function GetCrossReferencedAddress(const aStartingAddress: PByte; const aCrossReference: PStoredCrossReference): TCodePointer;
    procedure DetectVersionedXboxLibraries(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);
    procedure TestAddressUsingPatternTrie(var aTestAddress: PByte);
    procedure DetermineFinalLocations;
    procedure DetermineSpecialSymbols;
    procedure ScanMemoryRangeForLibraryPatterns(const pXbeHeader: PXBE_HEADER);
  public
    StoredLibraryIndexedToScan: TBits;
    constructor Create;
    destructor Destroy; override;

    procedure DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);

    procedure Clear;

    function RegisterSpecificFunctionLocation(const aFunctionName: string; const aAddress: PByte): TSymbolInformation;

    class function CacheFileName(const pXbeHeader: PXBE_HEADER): string;
    function LoadSymbolsFromCache(const aCacheFile: string): Boolean;
    procedure SaveSymbolsToCache(const aCacheFile: string);
  end;

var
  SymbolManager: TSymbolManager;
  LibD3D8: PStoredLibrary;

implementation

uses
  uVertexBuffer; // CRC32Init, Crc32


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
end; // BinarySearch

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

function TSymbolInformation.GetLength: Cardinal;
begin
  if Assigned(StoredLibraryFunction) then
    Result := StoredLibraryFunction.FunctionLength
  else
    // Assume it's a global variable of type DWORD :
    Result := SizeOf(DWORD);
end;

function TSymbolInformation.GetCrossReferenceCount: Integer;
begin
  if Assigned(StoredLibraryFunction) then
    Result := StoredLibraryFunction.NrCrossReferences
  else
    Result := 0;
end;

function TSymbolInformation.GetCrossReference(const aIndex: Integer): PStoredCrossReference;
begin
  Result := GetCrossReferenceByIndex(StoredLibraryFunction, aIndex);
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

  StoredLibraryIndexedToScan := TBits.Create;
  MyFinalLocations := TList.Create;
end;

destructor TSymbolManager.Destroy;
begin
  Clear;
  FreeAndNil(StoredLibraryIndexedToScan);
  FreeAndNil(MyFinalLocations);

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
  if Assigned(StoredLibraryIndexedToScan) then
    StoredLibraryIndexedToScan.Size := 0;

  // Clear MySymbolHashTable :
  for w := Low(w) to High(w) do
    FreeAndNil(MySymbolHashTable[w]);
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
  Result := MySymbolHashTable[HashIndex];
  while Assigned(Result) do
  begin
    if (Result.Name = aName) then
      Exit;

    Inc(HashIndex, 1);
    Result := MySymbolHashTable[HashIndex];
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
  Result := MySymbolHashTable[HashIndex];
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
          Assert(Result.StoredLibraryFunction.GlobalFunctionIndex = aFoundFunction.GlobalFunctionIndex, 'Error! Same name, other function!');

          // Keep track of the longest version :
          if Result.StoredLibraryFunction.FunctionLength < aFoundFunction.FunctionLength then
            Result.StoredLibraryFunction := aFoundFunction;
        end;
      end;

      Exit;
    end;

    Inc(HashIndex, 1);
    Result := MySymbolHashTable[HashIndex];
  end;

  Result := TSymbolInformation.Create;
  Result.StoredLibraryFunction := aFoundFunction;
  Result.Name := aName;

  MySymbolHashTable[HashIndex] := Result;
  Inc(MySymbolCount);
end; // FindOrAddSymbol

function TSymbolManager.AddPotentialSymbolLocation(const aLocation: TCodePointer; const aSymbol: TSymbolInformation): PPotentialFunctionLocation;
var
  CurrentCapacity: Integer;
begin
  Assert(Assigned(aLocation));
  Assert(Assigned(aSymbol));

//  if MyPotentialFunctionLocations_Count > 0 then
//  begin
//    Assert(IntPtr(MyPotentialFunctionLocations[MyPotentialFunctionLocations_Count - 1].Address) <= IntPtr(aLocation),
//      'AddPotentialSymbolLocation should be called with increasing locations!');
//
//    if (IntPtr(aLocation) + aSymbol.Length - 1) <= IntPtr(MyPotentialFunctionLocations[MyPotentialFunctionLocations_Count - 1].CodeEnd) then
//      Exit;
//  end;

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
end; // PotentialFunctionLocationIndex

function TSymbolManager.IsAddressWithinScanRange(const aAddress: TCodePointer): Boolean;
begin
  Result := (UIntPtr(aAddress) >= XBE_IMAGE_BASE) and (UIntPtr(aAddress) <= ScanUpper);
end;

function TSymbolManager.GetCrossReferencedAddress(const aStartingAddress: PByte;
  const aCrossReference: PStoredCrossReference): TCodePointer;
begin
{$IFDEF DXBX_RECTYPE}
  Assert(aCrossReference.RecType = rtStoredCrossReference, 'StoredCrossReference type mismatch!');
{$ENDIF}
  // Use aCrossReference.Offset to determine the symbol-address that should be checked :

  Result := DetermineRelativeAddress(aStartingAddress, aCrossReference.Offset);
  Dec(UIntPtr(Result), aCrossReference.BaseOffset);
  if IsAddressWithinScanRange(Result) then
    Exit;

  Result := DetermineImmediateAddress(aStartingAddress, aCrossReference.Offset);
  Dec(UIntPtr(Result), aCrossReference.BaseOffset);
  if IsAddressWithinScanRange(Result) then
    Exit;

  Result := nil;
end; // GetCrossReferencedAddress

function TSymbolManager.RegisterSpecificFunctionLocation(const aFunctionName: string; const aAddress: PByte): TSymbolInformation;
begin
  Result := FindOrAddSymbol(aFunctionName, {FoundFunction=}nil);
  AddPotentialSymbolLocation(aAddress, Result);
end;

procedure TSymbolManager._Debug(const aSymbol: TSymbolInformation);
var
  i: Integer;
  f: Integer;
  CurrentLocation: PPotentialFunctionLocation;
begin
  if aSymbol = nil then
    Exit;

  DbgPrintf('SYMBOL : "%s"', [aSymbol.Name]);
  DbgPrintf('Length : %d', [aSymbol.Length]);
  DbgPrintf('Address : $%.08x', [aSymbol.Address]);
  DbgPrintf('XRefCause : "%s"', [aSymbol.XRefCause]);
  DbgPrintf('CrossReferenceCount : %d', [aSymbol.CrossReferenceCount]);
  DbgPrintf('PotentialLocations :');
  f := aSymbol.FirstPotentialFunctionLocationIndex;
  while f > 0 do
  begin
    CurrentLocation := @(MyPotentialFunctionLocations[f]);
    DbgPrintf('#%d : $%0.8x (%2d references)', [
      f,
      CurrentLocation.Address,
      CurrentLocation.NrOfReferencesTo]);
    for i := 0 to aSymbol.CrossReferenceCount - 1 do
      DbgPrintf('  CrossReference %2d : $%0.4x -> $%.08x = "%s"+$%.04x', [
        i+1,
        aSymbol.GetCrossReference(i).Offset,
        GetCrossReferencedAddress(CurrentLocation.Address, aSymbol.GetCrossReference(i)),
        PatternTrieReader.GetString(aSymbol.GetCrossReference(i).NameIndex),
        aSymbol.GetCrossReference(i).BaseOffset
        ]);
    f := CurrentLocation.NextPotentialFunctionLocationIndex;
  end;
end;

procedure TSymbolManager.TestAddressUsingPatternTrie(var aTestAddress: PByte);

  function _MayCheckFunction(const aStoredLibraryFunction: PStoredLibraryFunction): Boolean;
  begin
    // Skip small functions with only one cross-reference, because those are
    // very common. Instead, we hope they will be discovered via other functions :
    if (aStoredLibraryFunction.FunctionLength < 6)
    and (aStoredLibraryFunction.NrCrossReferences = 1) then
      Result := False
    else
      Result := True;
  end;

  function _CountFunctionCRC(const aStoredLibraryFunction: PStoredLibraryFunction; const aAddress: PByte): Boolean;
  begin
    if aStoredLibraryFunction.CRCLength > 0 then
      Result := (aStoredLibraryFunction.CRCValue = CalcCRC16(aAddress, aStoredLibraryFunction.CRCLength))
    else
      Result := True;
  end;

  function _CheckFunctionEnd(const aStoredLibraryFunction: PStoredLibraryFunction; const aAddress: PByte): Boolean;
  const
    // Source: http://ref.x86asm.net/coder32.html
    OPCODE_NOP = $90;
    OPCODE_RETNi16 = $C2;
    OPCODE_RETN = $C3;
    OPCODE_RETFi16 = $CA;
    OPCODE_RETF = $CB;
    OPCODE_INT3 = $CC;
    OPCODE_JMPrel32 = $E9;
    OPCODE_JMPrel8 = $EB;
  var
    RetPos: PBytes;
  begin
    Result := True;
    // Does the function extend beyond what we've tested already?
    if aStoredLibraryFunction.FunctionLength > (PATTERNSIZE + aStoredLibraryFunction.CRCLength) then
    begin
      // Start at what's presumably the last function address :
      RetPos := PBytes(aAddress + aStoredLibraryFunction.FunctionLength - 1);

      // Some functions end with NOP's, trace back to the last non-NOP byte :
      while RetPos[0] = OPCODE_NOP do
        Dec(UIntPtr(RetPos));

      // Skip back 4 more bytes, and now check if there's a return-opcode at the end :
      // (We need to step back 4 bytes, as some of the opcodes have 4 argument-bytes)
      Dec(UIntPtr(RetPos), 4);
      if (RetPos[0] = OPCODE_JMPrel32)
      or (RetPos[2] = OPCODE_RETNi16)
      or (RetPos[2] = OPCODE_RETFi16)
      or (RetPos[3] = OPCODE_JMPrel8)
      or (RetPos[4] = OPCODE_RETN)
      or (RetPos[4] = OPCODE_RETF)
      or (RetPos[4] = OPCODE_INT3)
      // TODO : Check if the above suffices for all function-endings (what about other JMP's for example?)
      then
        // If this check holds, this address still seems to be a valid function - fall through.
      else
        // No ret opcode found, so no match :
        Result := False;
    end;
  end; // _CheckFunctionEnd

  function _CheckAndRememberFunctionCrossReferences(
    const aStoredLibraryFunction: PStoredLibraryFunction;
    const aAddress: PByte): Boolean;
  var
    CrossReference: PStoredCrossReference;
    x, c, j: Integer;
  begin
    Result := (aStoredLibraryFunction.NrCrossReferences = 0);
    if Result then
      Exit;

    // Make sure there's space for the cross-references :
    c := AllCrossReferencesInUse + aStoredLibraryFunction.NrCrossReferences;
    while Length(AllCrossReferences) < c do
      SetLength(AllCrossReferences, Length(AllCrossReferences) * 2);
    c := AllCrossReferencesInUse;

    // Read all cross-references and check if they point to a valid address :
    UIntPtr(CrossReference) := UIntPtr(aStoredLibraryFunction) + SizeOf(RStoredLibraryFunction);
    for x := 0 to aStoredLibraryFunction.NrCrossReferences - 1 do
    begin
      // If a cross-reference falls outside the valid range, we consider this a false hit :
      AllCrossReferences[c+x].Address := GetCrossReferencedAddress(aAddress, CrossReference);
      if AllCrossReferences[c+x].Address = nil then
        Exit;

      // Check that any duplicate symbol-references are all pointing to the same address :
      AllCrossReferences[c+x].Symbol := FindOrAddSymbol(CrossReference.NameIndex, {Function=}nil);
      for j := 0 to x - 1 do
      begin
        // Same symbol, but different address-reference means a false hit, which must be skipped :
        if (AllCrossReferences[c+j].Symbol  =  AllCrossReferences[c+x].Symbol) then
        // [Delphi has a compiler bug regarding Exit - DO NOT 'and' these 2 conditions into one!]
        if (AllCrossReferences[c+j].Address <> AllCrossReferences[c+x].Address) then
          Exit;
      end;

      Inc(CrossReference);
    end;

    Result := True;
  end; // _CheckAndRememberFunctionCrossReferences

  procedure _CheckLeafFunctions(aStoredLibraryFunction: PStoredLibraryFunction; NrChildren: Integer; const aAddress: PByte);
  var
    ShortestFunctionLength: Integer;
    i: Integer;
    FunctionName: string;
    Symbol: TSymbolInformation;
    Location: PPotentialFunctionLocation;
  begin
    ShortestFunctionLength := MaxInt;
    // Check the possible functions further, and find the most number of seemingly valid cross-references :
    for i := 0 to NrChildren - 1 do
    begin
{$IFDEF DXBX_RECTYPE}
      Assert(aStoredLibraryFunction.RecType = rtStoredLibraryFunction, 'StoredLibraryFunction type mismatch!');
{$ENDIF}
      // TODO : Once detection produces good results, improve speed by postponing this a bit :
      FunctionName := PatternTrieReader.GetFunctionName(aStoredLibraryFunction.GlobalFunctionIndex);

      // Check if this functions' library matches an active one :
      if  StoredLibraryIndexedToScan[aStoredLibraryFunction.LibraryIndex] then
      if _MayCheckFunction(aStoredLibraryFunction) then
      // Check if the CRC holds :
      if _CountFunctionCRC(aStoredLibraryFunction, aAddress) then
//      if _CheckFunctionEnd(aStoredLibraryFunction, aTestAddress) then
      // TODO : Include data & test-code for 'trailing bytes' patterns (or extend the CRC-range).
      // Check if all cross-references are within the expected memory-range :
      if _CheckAndRememberFunctionCrossReferences(aStoredLibraryFunction, aTestAddress) then
      begin
        // Only now add the current location as a candidate for this function :
        Symbol := FindOrAddSymbol(FunctionName, aStoredLibraryFunction);
        Location := Symbol.AddPotentialLocation(aTestAddress);

        // Persist the cross-references determined for this location :
        if aStoredLibraryFunction.NrCrossReferences > 0 then
        begin
          Location.CrossReferencesIndex := AllCrossReferencesInUse;
          Inc(AllCrossReferencesInUse, aStoredLibraryFunction.NrCrossReferences);
        end;

        // Remember the shortest function (to skip bytes with after all leaves are processed) :
        if ShortestFunctionLength > aStoredLibraryFunction.FunctionLength then
          ShortestFunctionLength := aStoredLibraryFunction.FunctionLength;
      end;

      // Skip to the next stored library function (including a step over all cross-references) :
      Inc(UIntPtr({var}aStoredLibraryFunction),
        SizeOf(RStoredLibraryFunction) +
        (SizeOf(RStoredCrossReference) * aStoredLibraryFunction.NrCrossReferences)
        );
    end;
    // Skip the least amount of covered bytes :
    if ShortestFunctionLength < MaxInt then
//      if ShortestFunctionLength > 16 then
        Inc({var}aTestAddress, ShortestFunctionLength - 1);
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
    // Calculate the position of the data after this TreeNode (StretchPtr) :
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
      _CheckLeafFunctions(PStoredLibraryFunction(StretchPtr), NrChildren, aAddress);
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
  // Search if this address matches a pattern :
  Node := PatternTrieReader.GetNode(PatternTrieReader.StoredSignatureTrieHeader.TrieRootNode);

  _TryMatchingNode(Node, aTestAddress, 0);

  Inc({var}aTestAddress);
end; // TestAddressUsingPatternTrie

procedure TSymbolManager.ScanMemoryRangeForLibraryPatterns(const pXbeHeader: PXBE_HEADER);
var
  i: DWord;
  Section: PXBE_SECTIONHEADER;
  ScanEnd: UIntPtr;
  p: UIntPtr;
begin
  // Determine upper bound for scanning, based on the XBE sections :
  ScanUpper := Low(ScanUpper);
  i := pXbeHeader.dwSections;
  UIntPtr(Section) := UIntPtr(pXbeHeader) + pXbeHeader.dwSectionHeadersAddr - pXbeHeader.dwBaseAddr;
  while i > 0 do
  begin
    if ScanUpper < UIntPtr(Section.dwVirtualAddr) + Section.dwVirtualSize then
      ScanUpper := UIntPtr(Section.dwVirtualAddr) + Section.dwVirtualSize;

    Inc(Section);
    Dec(i);
  end;

  // Allocate the cross-references pool :
  SetLength(AllCrossReferences, 1024);
  ZeroMemory(@(AllCrossReferences[0]), Length(AllCrossReferences) * SizeOf(AllCrossReferences[0]));
  AllCrossReferencesInUse := 1; // Start at index 1, so index 0 means 'no value'

(*
// TODO : Analyze and fix all missing and mismatching functions from Turok, compared to Cxbx :
////{test} p := $0001171D; TestAddressUsingPatternTrie(PByte(p)); // XapiInitProcess
{test} p := $001939B0; TestAddressUsingPatternTrie(PByte(p)); // EmuIDirect3DDevice8_SetRenderState_StencilEnable
{test} p := $00193A40; TestAddressUsingPatternTrie(PByte(p)); // EmuIDirect3DDevice8_SetRenderState_StencilFail
//{test} p := $00193AB0; TestAddressUsingPatternTrie(PByte(p)); // EmuIDirect3DDevice8_SetRenderState_YuvEnable
{test} p := $00193AE0; TestAddressUsingPatternTrie(PByte(p)); // EmuIDirect3DDevice8_SetRenderState_OcclusionCullEnable
{test} p := $00193B50; TestAddressUsingPatternTrie(PByte(p)); // EmuIDirect3DDevice8_SetRenderState_StencilCullEnable
{test} p := $00193BC0; TestAddressUsingPatternTrie(PByte(p)); // EmuIDirect3DDevice8_SetRenderState_RopZCmpAlwaysRead
{test} p := $00193BE0; TestAddressUsingPatternTrie(PByte(p)); // EmuIDirect3DDevice8_SetRenderState_RopZRead
////{test} p := $001960E0; TestAddressUsingPatternTrie(PByte(p)); // EmuIDirect3DBaseTexture8_GetLevelCount
////{test} p := $001B018A; TestAddressUsingPatternTrie(PByte(p)); // CDirectSound_SetRolloffFactor
{test} p := $001F35E3; TestAddressUsingPatternTrie(PByte(p)); // EmuXInitDevices
// Cxbx meshes :
{test} p := $0001DE60; TestAddressUsingPatternTrie(PByte(p)); // EmuIDirect3DDevice8_SetTile
{test} p := $00020200; TestAddressUsingPatternTrie(PByte(p)); // D3D::SetFence (XREF)
{test} p := $0002CC34; TestAddressUsingPatternTrie(PByte(p)); // EmuXapiProcessHeap
*)
//{test} p := $00258654; TestAddressUsingPatternTrie(PByte(p)); // _XapiProcessHeap

  i := 0;
  UIntPtr(Section) := UIntPtr(pXbeHeader) + pXbeHeader.dwSectionHeadersAddr - pXbeHeader.dwBaseAddr;
  while i < pXbeHeader.dwSections do
  begin
    p := UIntPtr(Section.dwVirtualAddr);
    ScanEnd := UIntPtr(Section.dwVirtualAddr) + Section.dwVirtualSize;

{$IFDEF DXBX_DEBUG}
    DbgPrintf('DxbxHLE : Detecting functions in section $%0.4x '{(%s)}+' from $%.8x to $%.8x', [
      i, {string(PAnsiChar(GetAddr(Section.dwSectionNameAddr))), }p, ScanEnd],
      {MayRenderArguments=}False);
{$ENDIF}

    while p <> ScanEnd do
    try
      TestAddressUsingPatternTrie({var}PByte(p));
    except
{$IFDEF DXBX_DEBUG}
      DbgPrintf('DxbxHLE : Exception while scanning on address $%.8x', [p]);
{$ENDIF}
      Inc(p);
    end;

    Inc(Section);
    Inc(i);
  end;
end; // ScanMemoryRangeForLibraryPatterns

procedure TSymbolManager.DetectVersionedXboxLibraries(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);
var
  CurrentXbeLibraryVersion: PXBE_LIBRARYVERSION;
  CurrentLibName: string;
  StoredLibrary: PStoredLibrary;
  BestDist, ThisDist: Integer;
  StoredLibraryName: string;
  i, j: Integer;
  BestStoredLibraryIndex: Integer;
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
    CurrentLibName := string(PCharToString(@(CurrentXbeLibraryVersion.szName[0]), XBE_LIBRARYNAME_MAXLENGTH));
    DbgPrintf('DxbxHLE : Library "%s" is version %d', [CurrentLibName, CurrentXbeLibraryVersion.wBuildVersion]);

    // Find the library version in our pattern trie that best matches this :
    BestStoredLibraryIndex := -1;
    BestDist := Low(BestDist);
    for j := 0 to PatternTrieReader.StoredSignatureTrieHeader.LibraryTable.NrOfLibraries - 1 do
    begin
      StoredLibrary := PatternTrieReader.GetStoredLibrary(j);
      StoredLibraryName := PatternTrieReader.GetString(StoredLibrary.LibNameIndex);

      if SameLibName(StoredLibraryName, 'D3D8') then
        LibD3D8 := StoredLibrary;

      // Only consider libraries with the same name :
      if SameLibName(StoredLibraryName, CurrentLibName) then
      begin
        // Distance : 0 on exact hit, positive when more recent, negative when older;
        // More recent is better than older version; The closer to 0, the better.
        ThisDist := Integer(StoredLibrary.LibVersion) - Integer(CurrentXbeLibraryVersion.wBuildVersion);
        // Use this library when no other are found yet :
        if (BestStoredLibraryIndex < 0)
        // Or when the this version comes closer to the actual version :
        or (ThisDist = 0)
        or ((BestDist < 0) and (ThisDist > 0))
        or ((BestDist < 0) and (ThisDist < 0) and (ThisDist > BestDist))
        or ((BestDist > 0) and (ThisDist > 0) and (ThisDist < BestDist)) then
        begin
          // Remember best fit for now :
          BestStoredLibraryIndex := j;
          BestDist := ThisDist;

          // Can't do better than an exact match :
          if BestDist = 0 then
            Break;
        end;
      end;
    end; // for NrOfLibraries

    if BestStoredLibraryIndex >= 0 then
    begin
      StoredLibrary := PatternTrieReader.GetStoredLibrary(BestStoredLibraryIndex);
      if StoredLibrary.LibVersion = CurrentXbeLibraryVersion.wBuildVersion then
        DbgPrintf('... Got patterns for exactly this version!')
      else
        DbgPrintf('... Approximating this with patterns from library %d.', [StoredLibrary.LibVersion]);

      // Add this library to a set we'll use in the detection-code :
      StoredLibraryIndexedToScan[BestStoredLibraryIndex] := True;
    end
    else
      DbgPrintf('... No patterns registered for this library!');

    // Skip to the next library :
    Inc(CurrentXbeLibraryVersion);
  end; // for all all library versions

  // Prevent out-of-bounds errors when using this:
  StoredLibraryIndexedToScan.Size := PatternTrieReader.StoredSignatureTrieHeader.LibraryTable.NrOfLibraries;
end; // DetectVersionedXboxLibraries

procedure TSymbolManager.DetermineFinalLocations;

  function _RemoveIncorrectAlternatives: Boolean;
  var
    w: Word;
    CurrentSymbol: TSymbolInformation;
    i, NextIndex: Integer;
    PrevLocation: PPotentialFunctionLocation;
    CurrentLocation: PPotentialFunctionLocation;
    Remove: Boolean;
    x: Integer;
    CrossReferencedSymbol: TSymbolInformation;
    CrossReferencedAddress: TCodePointer;
  begin
    Result := False;
    // Loop over all symbols :
    for w := Low(w) to High(w) do
    begin
      CurrentSymbol := MySymbolHashTable[w];
      if (CurrentSymbol = nil)
      or (CurrentSymbol.CrossReferenceCount = 0) then
        Continue;

      // Iterate over all potential locations of this symbol :
      i := CurrentSymbol.FirstPotentialFunctionLocationIndex;
      PrevLocation := nil;
      while i > 0 do
      begin
        CurrentLocation := @(MyPotentialFunctionLocations[i]);
        NextIndex := CurrentLocation.NextPotentialFunctionLocationIndex;

        // Loop over each cross-reference reachable from this symbol-address :
        Remove := False;
        for x := 0 to CurrentSymbol.CrossReferenceCount - 1 do
        begin
          // Check if one of the potential locations of the referenced symbol is detected at the referenced address :
          CrossReferencedAddress := AllCrossReferences[CurrentLocation.CrossReferencesIndex + x].Address;
          CrossReferencedSymbol := AllCrossReferences[CurrentLocation.CrossReferencesIndex + x].Symbol;
          Assert(Assigned(CrossReferencedSymbol));

          if CrossReferencedSymbol.FirstPotentialFunctionLocationIndex > 0 then
          if CrossReferencedSymbol.FindPotentialLocation(CrossReferencedAddress) = nil then
          begin
            // Discard this alternative if the above check failed :
            Remove := True;
            Break;
          end;
        end; // for CrossReferences

        if Remove then
        begin
          // Invalidate this potential location :
          CurrentLocation.Address := nil;
          CurrentLocation.Symbol := nil;
          CurrentLocation.NrOfReferencesTo := 0;
          CurrentLocation.CrossReferencesIndex := 0;
          CurrentLocation.NextPotentialFunctionLocationIndex := 0;

          // Decouple current alternative :
          if PrevLocation = nil then
            CurrentSymbol.FirstPotentialFunctionLocationIndex := NextIndex
          else
            PrevLocation.NextPotentialFunctionLocationIndex := NextIndex;

          // Signal a re-selection :
          Result := True;
        end
        else
          PrevLocation := CurrentLocation;

        i := NextIndex;
      end; // while Locations
    end; // for Symbols
  end; // _RemoveIncorrectAlternatives

  procedure _ExpandAndCountCrossReferences;
  var
    i: Integer;
    CurrentLocation: PPotentialFunctionLocation;
    CurrentSymbol: TSymbolInformation;
    x: Integer;
    CrossReferencedSymbol: TSymbolInformation;
    CrossReferencedAddress: TCodePointer;
    CrossReferencedLocation: PPotentialFunctionLocation;
  begin
    // Build some statistics on the cross-references :
    for i := 1 to MyPotentialFunctionLocations_Count do
    begin
      CurrentLocation := @(MyPotentialFunctionLocations[i]);
      CurrentSymbol := CurrentLocation.Symbol;
      if CurrentSymbol = nil then
        Continue;

      // Loop over each cross-reference reachable from this symbol-address :
      for x := 0 to CurrentSymbol.CrossReferenceCount - 1 do
      begin
        CrossReferencedSymbol := AllCrossReferences[CurrentLocation.CrossReferencesIndex + x].Symbol;
        Assert(Assigned(CrossReferencedSymbol));

        CrossReferencedAddress := AllCrossReferences[CurrentLocation.CrossReferencesIndex + x].Address;
        Assert(Assigned(CrossReferencedAddress));

        // Look up the referenced potential location (creating it if necessary),
        // and count the number of references to it :
        CrossReferencedLocation := CrossReferencedSymbol.AddPotentialLocation(CrossReferencedAddress);
        Inc(CrossReferencedLocation.NrOfReferencesTo);
      end; // for CrossReferences
    end; // for PotentialLocations
  end; // _ExpandAndCountCrossReferences

  procedure _SelectSymbolMostReferencedLocation(const CurrentSymbol: TSymbolInformation);
  var
    BestIndex: Integer;
    BestNrOfReferences: Integer;
    i: Integer;
    CurrentLocation: PPotentialFunctionLocation;
  begin
    BestIndex := 0;
    BestNrOfReferences := 0;

    // Iterate over all potential locations of this symbol :
    i := CurrentSymbol.FirstPotentialFunctionLocationIndex;
    while i > 0 do
    begin
      CurrentLocation := @(MyPotentialFunctionLocations[i]);
      // Select the version which is referenced the most :
      if BestNrOfReferences < CurrentLocation.NrOfReferencesTo then
      begin
        BestNrOfReferences := CurrentLocation.NrOfReferencesTo;
        BestIndex := i;
      end;

      i := CurrentLocation.NextPotentialFunctionLocationIndex;
    end;

    // Assign the chosen address :
    if BestIndex > 0 then
      CurrentSymbol.Address := MyPotentialFunctionLocations[BestIndex].Address;
  end; // _SelectSymbolMostReferencedLocation

  procedure _SelectBestLocations;
  var
    w: Word;
    CurrentSymbol: TSymbolInformation;
  begin
    // Loop over all symbols and select their best address :
    for w := Low(w) to High(w) do
    begin
      CurrentSymbol := MySymbolHashTable[w];
      if Assigned(CurrentSymbol) then
        _SelectSymbolMostReferencedLocation(CurrentSymbol);
    end; // for Symbols
  end; // _SelectBestLocations

  procedure _PutSymbolsInList;

    function _Compare_Symbol_Address(Item1, Item2: Pointer): Integer;
    begin
      Result := Integer(TSymbolInformation(Item1).Address) - Integer(TSymbolInformation(Item2).Address);
    end;

  var
    w: Word;
    CurrentSymbol: TSymbolInformation;
  begin
    MyFinalLocations.Count := MySymbolCount;
    MySymbolCount := 0;

    // Finally, loop over all symbols and put them in one big list :
    for w := Low(w) to High(w) do
    begin
      CurrentSymbol := MySymbolHashTable[w];
      if CurrentSymbol = nil then
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
        Name := Name + ' (XRef ' + CurrentSymbol.XRefCause + ')';

      DbgPrintf('Found at 0x%.8x-0x%.8x : %s  [%s]',
        [CurrentSymbol.Address,
         UIntPtr(CurrentSymbol.Address) + UInt(CurrentSymbol.Length) - 1,
         Name,
         CurrentSymbol.Name],
        {MayRenderArguments=}False);
    end; // for Symbols
  end; // _PrintLocationList

begin // DetermineFinalLocations

  // First, for each potential address, count the number of cross references that
  // we can find symbol-information for, and which seem to hold up address-wise :
  _RemoveIncorrectAlternatives;
  _ExpandAndCountCrossReferences;
  _SelectBestLocations;

  _Debug(FindOrAddSymbol('_D3DDevice_SetRenderState_StencilEnable@4'));
  _Debug(FindOrAddSymbol('_D3DDevice_SetRenderState_StencilFail@4'));
  _Debug(FindOrAddSymbol('_D3DDevice_SetRenderState_DoNotCullUncompressed@4'));
  _Debug(FindOrAddSymbol('_D3DDevice_SetRenderState_RopZRead@4'));
  _Debug(FindOrAddSymbol('_D3DDevice_SetRenderState_RopZCmpAlwaysRead@4'));
  _Debug(FindOrAddSymbol('_D3D__RenderState'));
//  _Debug(FindOrAddSymbol('_XInitDevices@8'));

//  // Repeat this while there are still locations that have wrong references :
//  while _RemoveIncorrectAlternatives do
//    // Then select the address that's referenced most as the default location :
//    _SelectBestLocations;
//
//  _Debug(FindOrAddSymbol('_D3DDevice_SetRenderState_StencilEnable@4'));
//  _Debug(FindOrAddSymbol('_D3DDevice_SetRenderState_StencilFail@4'));
//  _Debug(FindOrAddSymbol('_D3DDevice_SetRenderState_DoNotCullUncompressed@4'));
//  _Debug(FindOrAddSymbol('_D3DDevice_SetRenderState_RopZRead@4'));
//  _Debug(FindOrAddSymbol('_D3DDevice_SetRenderState_RopZCmpAlwaysRead@4'));
//  _Debug(FindOrAddSymbol('_D3D__RenderState'));
////  _Debug(FindOrAddSymbol('_XInitDevices@8'));

  _PutSymbolsInList;

  _PrintLocationList;

  // TODO :
  // - fix doubles addresses (conflicts)
  // - compare output with Cxbx, determine who's right, fix our mis-matches ;-)
  // - fix cache-loading
end; // DetermineFinalLocations

procedure TSymbolManager.DetermineSpecialSymbols;
var
  Symbol: TSymbolInformation;
  s, v: int;
begin
  // locate XapiProcessHeap
  begin
    // Resolve the address of the _XapiProcessHeap symbol (at least cross-referenced once, from XapiInitProcess) :
    Symbol := FindSymbol('_XapiProcessHeap');
    if Assigned(Symbol) then
      // and remember that in a global :
      XTL_EmuXapiProcessHeap := Symbol.Address;

{$IFDEF DXBX_DEBUG}
    if Assigned(XTL_EmuXapiProcessHeap) then
      DbgPrintf('HLE: $%.08X . XapiProcessHeap',
        [XTL_EmuXapiProcessHeap],
        {MayRenderArguments=}False)
    else
      DbgPrintf('HLE : Can''t find XapiProcessHeap!',
        [],
        {MayRenderArguments=}False);
{$ENDIF}
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
      DbgPrintf('HLE: $%.08X . EmuD3DDeferredRenderState', [XTL_EmuD3DDeferredRenderState]);
{$ENDIF}
    end
    else
      EmuWarning('EmuD3DDeferredRenderState was not found!');
  end;

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

      DbgPrintf('HLE: $%.08X . EmuD3DDeferredTextureState',
        [XTL_EmuD3DDeferredTextureState],
        {MayRenderArguments=}False);
    end
    else
      EmuWarning('EmuD3DDeferredTextureState was not found!');
  end;
end; // DetermineSpecialSymbols

procedure TSymbolManager.DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);
var
  ResourceStream: TResourceStream;
begin
  // Get StoredPatternTrie from resource :
  ResourceStream := TResourceStream.Create(LibModuleList.ResInstance, 'StoredPatternTrie', RT_RCDATA);
  try
    PatternTrieReader := TPatternTrieReader.Create;
    try
      PatternTrieReader.LoadFromStream(ResourceStream);

      DetectVersionedXboxLibraries(pLibraryVersion, pXbeHeader);

      // Scan Patterns using this trie :
      ScanMemoryRangeForLibraryPatterns(pXbeHeader);

      DetermineFinalLocations();

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
end; // DxbxScanForLibraryAPIs

class function TSymbolManager.CacheFileName(const pXbeHeader: PXBE_HEADER): string;
begin
  CRC32Init();
  Result := SymbolCacheFolder
          // TitleID
          + IntToHex(PXBE_CERTIFICATE(pXbeHeader.dwCertificateAddr).dwTitleId, 8)
          // + CRC32 over XbeHeader :
          + '_' + IntToHex(CRC32(PByte(pXbeHeader), {Len=}pXbeHeader.dwSizeofHeaders), 8)
          + SymbolCacheFileExt;
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
    MyFinalLocations.Count := Count;
    for i := 0 to Count - 1 do
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

  DetermineSpecialSymbols();

{$IFDEF DXBX_DEBUG}
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

