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
  uXboxLibraryUtils;

type
  TSymbolInformation = class; // forward

  PPotentialFunctionLocation = ^RPotentialFunctionLocation;
  RPotentialFunctionLocation = record
  private
    function GetLength: Integer;
    function GetCodeEnd: TCodePointer;
  public
    Address: TCodePointer;
    Symbol: TSymbolInformation;
    NextPotentialFunctionLocationIndex: Integer;
    NrOfAvailableCrossReferences: Integer;
    NrOfSeeminglyCorrectCrossReferences: Integer;

    property Length: Integer read GetLength;
    property CodeEnd: TCodePointer read GetCodeEnd;
  end;

  TSymbolInformation = class(TObject)
  private
    FAddress: TCodePointer;
    function GetLength: Integer;
    function GetLocation: TCodePointer;
    function GetCrossReferenceCount: Integer;
    function GetCrossReference(const aIndex: Integer): PStoredCrossReference;
  public
    NameIndex: TStringTableIndex;
    Name: string;
    FirstPotentialFunctionLocationIndex: Integer;
    StoredLibraryFunction: PStoredLibraryFunction; // can be nil for non-pattern symbols
    XRefCause: string;

    function FindLocationIndex(const aAddress: TCodePointer): Integer;
    function FindPotentialLocation(const aAddress: TCodePointer): PPotentialFunctionLocation;

    property Length: Integer read GetLength;
    property Address: TCodePointer read GetLocation write FAddress;
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
    procedure AddPotentialSymbolLocation(const aLocation: TCodePointer; const aSymbol: TSymbolInformation);
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
    ScanLower, ScanUpper: UIntPtr;
    PatternTrieReader: TPatternTrieReader;
    function IsAddressWithinScanRange(const aAddress: TCodePointer): Boolean;
    function GetCrossReferencedAddress(const aStartingAddress: PByte; const aCrossReference: PStoredCrossReference): TCodePointer;
    procedure FindAndRememberPattern(const aAddress: PByte; const FoundFunction: PStoredLibraryFunction);
    procedure DetectVersionedXboxLibraries(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);
    procedure TestAddressUsingPatternTrie(const aTestAddress: PByte);
    procedure DetermineFinalLocations;
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
begin
  // HACK: These libraries are considered the same :
  if SameText(StoredLibraryName, 'D3DX8') then
    StoredLibraryName := 'D3D8';

  if SameText(CurrentLibName, 'D3DX8') then
    CurrentLibName := 'D3D8';

  Result := SameText(StoredLibraryName, CurrentLibName);
end;

{$OVERFLOWCHECKS OFF}
function DetermineImmediateAddress(const aStartingAddress: PByte; const aOffset: Word): PByte;
begin
  IntPtr(Result) := IntPtr(aStartingAddress) + IntPtr(aOffset);
  Result := PByte(PInteger(Result)^);
end;

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
end; // BinarySearch

{ RPotentialFunctionLocation }

function RPotentialFunctionLocation.GetLength: Integer;
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

  IntPtr(Result) := IntPtr(Address) + Length - 1;
end;

{ TSymbolInformation }

function TSymbolInformation.GetLength: Integer;
begin
  if Assigned(StoredLibraryFunction) then
    Result := StoredLibraryFunction.FunctionLength
  else
    // Assume it's a global variable of type DWORD :
    Result := SizeOf(DWORD);
end;

function TSymbolInformation.GetLocation: TCodePointer;
begin
  Result := FAddress;
  if Assigned(Result) then
    Exit;

  if FirstPotentialFunctionLocationIndex > 0 then
    Result := SymbolManager.MyPotentialFunctionLocations[FirstPotentialFunctionLocationIndex].Address
  else
    Result := nil;
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

function TSymbolInformation.FindLocationIndex(const aAddress: TCodePointer): Integer;
begin
  Result := FirstPotentialFunctionLocationIndex;
  repeat
    if SymbolManager.MyPotentialFunctionLocations[Result].Address = aAddress then
      Exit;

    Result := SymbolManager.MyPotentialFunctionLocations[Result].NextPotentialFunctionLocationIndex;
  until Result = 0;
end;

function TSymbolInformation.FindPotentialLocation(const aAddress: TCodePointer): PPotentialFunctionLocation;
var
  Index: Integer;
begin
  Index := FirstPotentialFunctionLocationIndex;
  while Index > 0 do
  begin
    Result := @(SymbolManager.MyPotentialFunctionLocations[Index]);
    if Result.Address = aAddress then
      Exit;

    Index := Result.NextPotentialFunctionLocationIndex;
  end;

  Result := nil;
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

procedure TSymbolManager.AddPotentialSymbolLocation(const aLocation: TCodePointer; const aSymbol: TSymbolInformation);
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

  with MyPotentialFunctionLocations[MyPotentialFunctionLocations_Count] do
  begin
    Address := aLocation;
    Symbol := aSymbol;
    NextPotentialFunctionLocationIndex := aSymbol.FirstPotentialFunctionLocationIndex;
    NrOfAvailableCrossReferences := 0;
    NrOfSeeminglyCorrectCrossReferences := 0;
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
  Result := (UIntPtr(aAddress) >= ScanLower) and (UIntPtr(aAddress) <= ScanUpper);
end;

function TSymbolManager.GetCrossReferencedAddress(const aStartingAddress: PByte;
  const aCrossReference: PStoredCrossReference): TCodePointer;
begin
{$IFDEF DXBX_RECTYPE}
  Assert(aCrossReference.RecType = rtStoredCrossReference, 'StoredCrossReference type mismatch!');
{$ENDIF}
  // Use aCrossReference.Offset to determine the symbol-address that should be checked :

  Result := DetermineRelativeAddress(aStartingAddress, aCrossReference.Offset);
  if IsAddressWithinScanRange(Result) then
    Exit;

  Result := DetermineImmediateAddress(aStartingAddress, aCrossReference.Offset);
  if IsAddressWithinScanRange(Result) then
    Exit;

  Result := nil;
end; // GetCrossReferencedAddress

function TSymbolManager.RegisterSpecificFunctionLocation(const aFunctionName: string; const aAddress: PByte): TSymbolInformation;
begin
  Result := FindOrAddSymbol(aFunctionName, {FoundFunction=}nil);
  AddPotentialSymbolLocation(aAddress, Result);
end;

procedure TSymbolManager.FindAndRememberPattern(const aAddress: PByte; const FoundFunction: PStoredLibraryFunction);
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
  CrossReference: PStoredCrossReference;
  i: Integer;
  FunctionName: string;
  Symbol: TSymbolInformation;
begin
  Assert(Assigned(FoundFunction));
//  Assert(FoundFunction.FunctionLength > 0);
//  Assert(FoundFunction.FunctionLength < High(Word)); // TODO : Use actual max length

  // Does the function extend beyond what we've tested already?
  if FoundFunction.FunctionLength > (PATTERNSIZE + FoundFunction.CRCLength) then
  begin
    // Start at what's presumably the last function address :
    RetPos := PBytes(aAddress + FoundFunction.FunctionLength - 1);

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
      Exit;
  end;

  // Check all cross-references to have a valid address :
  IntPtr(CrossReference) := IntPtr(FoundFunction) + SizeOf(RStoredLibraryFunction);
  for i := 0 to FoundFunction.NrCrossReferences - 1 do
    if Assigned(GetCrossReferencedAddress(aAddress, CrossReference)) then
      Inc(CrossReference)
    else
      Exit;

  // Now that the function seems valid on first looks, make sure it is registered :
  FunctionName := PatternTrieReader.GetFunctionName(FoundFunction.GlobalFunctionIndex);

  Symbol := FindOrAddSymbol(FunctionName, FoundFunction);
  AddPotentialSymbolLocation(aAddress, Symbol);
  // TODO : Support having more than 1 incarnation of a function
end; // FindAndRememberPattern

procedure TSymbolManager.TestAddressUsingPatternTrie(const aTestAddress: PByte);

  function _TryMatchingLeaf(var aStoredLibraryFunction: PStoredLibraryFunction; const aAddress: PByte): PStoredLibraryFunction;
  begin
    Result := aStoredLibraryFunction;
{$IFDEF DXBX_RECTYPE}
    Assert(Result.RecType = rtStoredLibraryFunction, 'StoredLibraryFunction type mismatch!');
{$ENDIF}

    // Skip to the next stored library function (including a step over all cross-references) :
    Inc({var}aStoredLibraryFunction);
    Inc(IntPtr({var}aStoredLibraryFunction), Result.NrCrossReferences * SizeOf(RStoredCrossReference));

    if Result.CRCLength > 0 then
    begin
      if Result.CRCValue <> CalcCRC16(aAddress, Result.CRCLength) then
      begin
        Result := nil;
        Exit;
      end;
    end;

    // TODO : Include data & test-code for 'trailing bytes' patterns.

    // Check if this functions' library matches an active one :
    if StoredLibraryIndexedToScan[Result.LibraryIndex] then
      Exit;

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
      NrChildren := (Integer(NrChildren and 127) shl 8) or aStoredTrieNode.NrChildrenByte2
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
        Result := _TryMatchingLeaf({var}StoredLibraryFunction, aAddress);
        if Assigned(Result) then
          // TODO : Instead of calling RegisterPotentialFunctionLocation right away,
          // Note, we're handling all children here, so no early exit after this :
          FindAndRememberPattern(aTestAddress, Result);

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
      aStoredTrieNode := PatternTrieReader.GetNode(NextOffset);
      Dec(NrChildren);
    end;

    Result := nil;
  end; // _TryMatchingNode

var
  Node: PStoredTrieNode;
begin
  // Search if this address matches a pattern :
  Node := PatternTrieReader.GetNode(PatternTrieReader.StoredSignatureTrieHeader.TrieRootNode);

  _TryMatchingNode(Node, aTestAddress, 0);
end; // TestAddressUsingPatternTrie

procedure TSymbolManager.ScanMemoryRangeForLibraryPatterns(const pXbeHeader: PXBE_HEADER);
var
  i: DWord;
  Section: PXBE_SECTIONHEADER;
  ScanEnd: UIntPtr;
  p: UIntPtr;
begin
  // Determine lower and upper bound for scanning, based on the XBE sections :
  ScanLower := High(ScanLower);
  ScanUpper := Low(ScanUpper);
  i := pXbeHeader.dwSections;
  UIntPtr(Section) := UIntPtr(pXbeHeader) + pXbeHeader.dwSectionHeadersAddr - pXbeHeader.dwBaseAddr;
  while i > 0 do
  begin
    if ScanLower > UIntPtr(Section.dwVirtualAddr) then
      ScanLower := UIntPtr(Section.dwVirtualAddr);
    if ScanUpper < UIntPtr(Section.dwVirtualAddr) + Section.dwVirtualSize then
      ScanUpper := UIntPtr(Section.dwVirtualAddr) + Section.dwVirtualSize;

    Inc(Section);
    Dec(i);
  end;

  //{test} TestAddressUsingPatternTrie(PByte($001960E0)); // This should find EmuIDirect3DBaseTexture8_GetLevelCount in Turok
  //{test} TestAddressUsingPatternTrie(PByte($001B018A)); // This should find CDirectSound_SetRolloffFactor in Turok

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
    begin
      try
        TestAddressUsingPatternTrie(PByte(p));
      except
  {$IFDEF DXBX_DEBUG}
        DbgPrintf('DxbxHLE : Exception while scanning on address $%.8x', [p]);
  {$ENDIF}
      end;

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

  procedure _CountMatchingCrossReferences;
  var
    i: Integer;
    CurrentLocation: PPotentialFunctionLocation;
    CurrentSymbol: TSymbolInformation;
    x: Integer;
    CrossReference: PStoredCrossReference;
    CrossReferencedSymbol: TSymbolInformation;
    CrossReferencedAddress: TCodePointer;
    CrossReferencedLocation: PPotentialFunctionLocation;
  begin
    for i := 1 to MyPotentialFunctionLocations_Count do
    begin
      CurrentLocation := @MyPotentialFunctionLocations[i];
      CurrentSymbol := CurrentLocation.Symbol;
      // Loop over each cross-reference reachable from this symbol-address :
      for x := 0 to CurrentSymbol.GetCrossReferenceCount - 1 do
      begin
        CrossReference := CurrentSymbol.CrossReferences[x];
        // See if the referenced symbol was detected already :
        CrossReferencedSymbol := FindSymbol(CrossReference.NameIndex);
        if Assigned(CrossReferencedSymbol) then
        begin
          Inc(CurrentLocation.NrOfAvailableCrossReferences);
          // See if we can find a match for this cross-referenced address :
          CrossReferencedAddress := GetCrossReferencedAddress(CurrentLocation.Address, CrossReference);
          CrossReferencedLocation := CrossReferencedSymbol.FindPotentialLocation(CrossReferencedAddress);
          if Assigned(CrossReferencedLocation) then
            Inc(CurrentLocation.NrOfSeeminglyCorrectCrossReferences);
        end;
      end; // for CrossReferences
    end; // for PotentialLocations
  end; // _CountMatchingCrossReferences

  procedure _SelectBestLocation;
  var
    w: Word;
    CurrentSymbol: TSymbolInformation;
    BestIndex: Integer;
    BestNrOfAvailableCrossReferences: Integer;
    BestNrOfSeeminglyCorrectCrossReferences: Integer;
    i: Integer;
    x: Integer;
    CrossReference: PStoredCrossReference;
    CrossReferencedSymbol: TSymbolInformation;
    CrossReferencedAddress: TCodePointer;
  begin
    // Loop over all symbols and select their best address :
    for w := Low(w) to High(w) do
    begin
      CurrentSymbol := MySymbolHashTable[w];
      if CurrentSymbol = nil then
        Continue;

      BestIndex := 0;
      BestNrOfAvailableCrossReferences := 0;
      BestNrOfSeeminglyCorrectCrossReferences := 0;

      // Iterate over all locations for this symbol :
      i := CurrentSymbol.FirstPotentialFunctionLocationIndex;
      while i > 0 do
      begin
        if  (BestNrOfAvailableCrossReferences < MyPotentialFunctionLocations[i].NrOfAvailableCrossReferences)
        or ((BestNrOfAvailableCrossReferences = MyPotentialFunctionLocations[i].NrOfAvailableCrossReferences) and
            (BestNrOfSeeminglyCorrectCrossReferences < MyPotentialFunctionLocations[i].NrOfSeeminglyCorrectCrossReferences)) then
        begin
          BestIndex := i;
          BestNrOfAvailableCrossReferences := MyPotentialFunctionLocations[i].NrOfAvailableCrossReferences;
          BestNrOfSeeminglyCorrectCrossReferences := MyPotentialFunctionLocations[i].NrOfSeeminglyCorrectCrossReferences;
        end;

        i := MyPotentialFunctionLocations[i].NextPotentialFunctionLocationIndex;
      end;

      if BestIndex > 0 then
        CurrentSymbol.Address := MyPotentialFunctionLocations[BestIndex].Address;

      // Now add all cross-references that don't exist yet;
      // Loop over each cross-reference reachable from this symbol-address :
      for x := 0 to CurrentSymbol.GetCrossReferenceCount - 1 do
      begin
        CrossReference := CurrentSymbol.CrossReferences[x];
        CrossReferencedSymbol := FindOrAddSymbol(CrossReference.NameIndex, nil);
        if CrossReferencedSymbol.Address = nil then
        begin
          CrossReferencedAddress := GetCrossReferencedAddress(CurrentSymbol.Address, CrossReference);
          CrossReferencedSymbol.Address := CrossReferencedAddress;
          CrossReferencedSymbol.XRefCause := CurrentSymbol.Name;
        end;
      end; // for CrossReferences
    end; // for Symbols
  end; // _SelectBestLocation

  procedure _PutSymbolsInList;
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
  end; // _PutSymbolsInList

  function _Compare_Symbol_Address(Item1, Item2: Pointer): Integer;
  begin
    Result := Integer(TSymbolInformation(Item1).Address) - Integer(TSymbolInformation(Item2).Address);
  end;

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
         UIntPtr(CurrentSymbol.Address) + CurrentSymbol.Length - 1,
         Name,
         CurrentSymbol.Name],
        {MayRenderArguments=}False);
    end; // for Symbols
  end; // _PrintLocationList

begin // DetermineFinalLocations

  // First, for each potential address, count the number of cross references that
  // we can find symbol-information for, and which seem to hold up address-wise :
  _CountMatchingCrossReferences;

  _SelectBestLocation;

  _PutSymbolsInList;

  // Sort that list
  MyFinalLocations.Sort(@_Compare_Symbol_Address);

  _PrintLocationList;

  // TODO :
  // - fix doubles addresses (conflicts)
  // - compare output with Cxbx, determine who's right, fix our mis-matches ;-)
  // - fix cache-loading
end; // DetermineFinalLocations

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

