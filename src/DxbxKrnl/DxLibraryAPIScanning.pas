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
  IniFiles,
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
  uEmuD3D8Types,
  uEmuShared,
  uState, // XTL_EmuD3DDeferredTextureState
  uEmu, // EmuWarning
  uEmuXapi, // XTL_EmuXapiProcessHeap
  uEmuExe; // ReinitXbeImageHeader, ReinitExeImageHeader

// Scanning steps:
// - scan memory once, byte for byte, noteing only hits on leafs
// - leafs can be hit more than once so we chain them together
// - the chain starts with a LeafDetectionInfo record,
//   and links PatternHitResult records for each hit.
//
// There's no ID for leafs, except their offset in the resource, which is inconvenient.
// But we can take the first function in the leaf (functions appear only once, so they're
// associated with one leaf only), and use the function offset as a (sparse) leaf number.
//
// Once that's done, we loop over MySymbol (which is perpared as sorted big to small)
// and use it's PatternLeafNodeOffset to get to the leaf (and thus via it's first function
// to the LeafDetectionInfo record).
// For all hits chained to this leaf, we test if the address matches the function;
// We check the CRC, and all it's references - they should stay inside the executable memory,
// and if they are functions too, they should either be validated at the referenced address
// already, or they need to be validated.

type
  PPatternHitResult = ^RPatternHitResult;
  RPatternHitResult = record
    Address: PByte; // The address which matched to the pattern ending up in the owning leaf
    NextHit: PPatternHitResult; // The next hit (if there are any) on the same leaf
  end;

  // This record contains all info needed to remember which addresses are a potential symbol location,
  // using the leaf as a starting point.
  RLeafDetectionInfo = record
    FirstFunctionIndex: PFunctionIndex; // A leaf in the resource is nothing more than a list of function ID's
    NrChildren: Integer; // The number of functions (0 means undetermined, as there are no leafs with zero functions)
    FirstHit: PPatternHitResult; // A singly-linked list of addresses that match the PatternTrie up to this leaf
  end;

  TSymbolInformation = class(TObject)
  protected
    function GetLength: Cardinal;
    function GetSymbolReferenceCount: Integer;
    function GetSymbolReference(const aIndex: Integer): PStoredSymbolReference;
  public
    OtherVersion: TSymbolInformation;
    AliasSymbol: TSymbolInformation;
    Name: string;
    UnmangledName: string;
    Address: TCodePointer;
    RangeClaimed: Boolean;
    StoredLibraryFunction: PStoredLibraryFunction; // can be nil for non-pattern symbols
    OrderingImportance: Integer;
    OrderingVersion: Integer; // Only used when (mangled)name matches
    OrderingScore: Integer;
    Discovery: string;
    FailReason: string;
    IsApproximation: Boolean;
    PatchedBy: string;
    function IsOperator: Boolean;
    property Length: Cardinal read GetLength;
    property SymbolReferenceCount: Integer read GetSymbolReferenceCount;
    property SymbolReferences[const aIndex: Integer]: PStoredSymbolReference read GetSymbolReference;
  end;

  TSymbolManager = class(TObject)
  protected // Symbols :
    MySymbolsHashedOnName: array [Word] of TSymbolInformation; // Hash table for all detected symbols, hashed on Name
    FSymbolCount: Integer; // Number of detected symbols (including symbol-references)
    function AddSymbol(const aName: string; const aFoundFunction: PStoredLibraryFunction = nil): TSymbolInformation;
    function FindOrAddVersionedSymbol(const aAvailableInVersions: TLibraryVersionFlags;
      const aNameIndex: TStringTableIndex; const aFoundFunction: PStoredLibraryFunction = nil): TSymbolInformation;
  public
    function FindSymbolWithAddress(const aName: string): TSymbolInformation; overload;
  protected // Address & reference tooling :
    EntryPoint: UIntPtr;
    ScanUpper: UIntPtr;
    PatternTrieReader: TPatternTrieReader;
    function IsAddressWithinScanRange(const aAddress: TCodePointer): Boolean;
    function MayCheckFunction(const aStoredLibraryFunction: PStoredLibraryFunction): Boolean;
    function GetReferencedSymbolAddress(const aStartingAddress: PByte; const aSymbolReference: PStoredSymbolReference): TCodePointer;
  protected // Leaf detection :
    MyDetectedLeafs: array of RLeafDetectionInfo; // All detected leafs, to be indexed with LeafID (it's first FunctionID)
    FNrDetectedLeafs: Integer; // The number of detected leafs (not counting contained functions)
    NrLeafHits: Integer; // The number of addresses that hit on a leaf
    function GetLeafID(const LeafNode: PStoredTrieNode): TLeafID;
    procedure AddLeafHit(const aAddress: PByte; const aFirstFunctionIndex: PFunctionIndex; const NrChildren: Integer);
    procedure CheckSectionAddressForPatternHit(const aTestAddress: PByte);
    procedure ScanAddressRangeForPatternHits(const pXbeHeader: PXBEIMAGE_HEADER);
    procedure ConvertLeafsIntoSymbols();
  protected // Intermediate symbol registration :
    FIntermediateSymbolRegistrationsCount: Integer;
    MyIntermediateSymbolRegistrations: array of TSymbolInformation;
    procedure AddIntermediateSymbolRegistration(const CurrentSymbol: TSymbolInformation;
      const aAddress: PByte; const IsFailureRegistration: Boolean = False);
    procedure ConfirmIntermediateSymbolRegistrations(const CurrentSymbol: TSymbolInformation);
    procedure RevertIntermediateSymbolRegistrations(const CurrentSymbol: TSymbolInformation; NrToRevert: Integer = -1);
  protected // Actual scanning :
    FLibraryVersionsToScan: TLibraryVersionFlags;
    MyAddressesInKnownSections: TDxbxBits; // Marks addresses that are covered with our library patterns
    MyAddressesIdentified: TDxbxBits; // Marks memory identification status with 1 bit per address
    IsTestCase: Boolean;
    ScanningInKnownAddresses: Boolean;
    function CheckFunctionAtAddressAndRegister(const CurrentSymbol: TSymbolInformation; const aAddress: PByte; const Prefix: string = ''): Integer;
    procedure PrioritizeSymbolScan();
    procedure LookupFunctionsInHits();
    procedure PutSymbolsInFinalList;
    procedure PrintSymbolList;
    procedure CleanupTemporaryScanningData;
  protected // Library and specific symbol handling :
    UsingLibraryApproximations: Boolean;
    SectionInfo: array of record
      SectionHeader: PXBE_SECTIONHEADER;
      SectionEndAddr: UIntPtr;
      SectionName: string;
      LibraryNameIndex: TStringTableIndex;
      LibraryName: string;
    end;
    procedure DetectVersionedXboxLibraries(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBEIMAGE_HEADER);
    procedure LookupGlobalEmulationSymbols;
  protected // Caching :
    class function CacheFileName(const pXbeHeader: PXBEIMAGE_HEADER): string;
    function LoadSymbolsFromCache(const aCacheFile: string): Boolean;
    procedure SaveSymbolsToCache(const pXbeHeader: PXBEIMAGE_HEADER; const aCacheFile: string);
  protected // Final symbols :
    MySymbols: TList;
    function GetCount: Integer;
    function GetSymbol(const aIndex: Integer): TSymbolInformation;
  public
    property Count: Integer read GetCount;
    property Symbols[const aIndex: Integer]: TSymbolInformation read GetSymbol; default;
  public // Main interface :
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBEIMAGE_HEADER);
  end;

var
  SymbolManager: TSymbolManager;
  LibD3D8: PStoredLibrary = nil;
//  TextSection: PXBE_SECTIONHEADER;

const
  OPCODE_NOP = $90;
  OPCODE_INT3 = $CC;
  OPCODE_JMP = $E9;

implementation

uses
  uHLEIntercept, // EmuInstallWrappers
  uVertexBuffer; // CRC32Init, Crc32

const lfUnit = lfDxbx or lfSymbolScan;

(* Old test code :

procedure CheckStringToFunctionMappings;
// Checks if the stored function names are correct
var
  s, f: Integer;
  Str: AnsiString;
  sh: PStoredStringHeader;
  FuncName: AnsiString;
begin
  for s := 0 to PatternTrieReader.StoredSignatureTrieHeader.StringTable.NrOfStrings - 1 do
  begin
    sh := PatternTrieReader.GetStringHeader(s);
    str := AnsiString(PatternTrieReader.GetString(s));
    for f := 0 to Integer(sh.NrOfFunctions) - 1 do
    begin
      FuncName := AnsiString(PatternTrieReader.GetFunctionName(sh.FirstFunctionIndex + Cardinal(f)));
      Assert(FuncName = Str);
    end;
  end;
end;

*)

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

function DecodeStretchHeader(var StretchHeader: PByte; out NrFixed, NrWildcards: Integer): Boolean;
var
  StretchHeaderByte: TStretchHeaderByte;
begin
  StretchHeaderByte := StretchHeader^;
  Inc(StretchHeader);

  // Determine if there are more stretches after this one :
  Result := (StretchHeaderByte and NODE_FLAG_MORE) > 0;
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
        Result := False;
      end
      else
        NrWildcards := -1; // call will see this and calculate "PATTERNSIZE - (Depth + NrFixed)" instead
  else // NODE_5BITFIXED_0WILDCARDS:
    NrWildcards := 0;
  end;
end;

{ TSymbolInformation }

function TSymbolInformation.GetLength: Cardinal;
begin
  if Assigned(StoredLibraryFunction) then
  begin
    Result := StoredLibraryFunction.FunctionLength;

    // Approximations cover only 75 % of their length, to prevent overlap on functions that became shorter:
//    if IsApproximation then
    if SymbolManager.UsingLibraryApproximations then
      Result := (Result * 3) div 4;
  end
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
  Result := SymbolManager.PatternTrieReader.GetSymbolReference(StoredLibraryFunction.FirstSymbolReference + Cardinal(aIndex));
{$IFDEF DXBX_RECTYPE}
  Assert(Result.RecType = rtStoredSymbolReference, 'StoredSymbolReference type mismatch!');
{$ENDIF}
end;

function TSymbolInformation.IsOperator: Boolean;
begin
  Result := (System.Length(Name) >= 2)
        and (Name[1] = '?')
        and (Name[2] = '?');
end;

{ TSymbolManager }

constructor TSymbolManager.Create;
begin
  inherited Create;

  MySymbols := TList.Create;
  MyAddressesInKnownSections := TDxbxBits.Create;
  MyAddressesIdentified := TDxbxBits.Create;
end;

destructor TSymbolManager.Destroy;
begin
  Clear;
  FreeAndNil(MyAddressesIdentified);
  FreeAndNil(MyAddressesInKnownSections);
  FreeAndNil(MySymbols);

  inherited Destroy;
end;

function TSymbolManager.GetCount: Integer;
begin
  Result := MySymbols.Count;
end;

function TSymbolManager.GetSymbol(const aIndex: Integer): TSymbolInformation;
begin
  Result := MySymbols[aIndex];
end;

procedure TSymbolManager.Clear;
var
  w: Word;
begin
  CleanupTemporaryScanningData;

  FLibraryVersionsToScan := [];

  // Clear MySymbolsHashedOnName :
  for w := Low(w) to High(w) do
    FreeAndNil(MySymbolsHashedOnName[w]);
  FSymbolCount := 0;

  MySymbols.Clear;
end; // Clear

///
/// Symbol registration :
///

function TSymbolManager.AddSymbol(const aName: string; const aFoundFunction: PStoredLibraryFunction = nil): TSymbolInformation;
var
  PrevVersion: TSymbolInformation;
  HashIndex: Word;
begin
  PrevVersion := nil;
  HashIndex := CalcCRC16(PByte(aName), Length(aName) * SizeOf(Char));
  Result := MySymbolsHashedOnName[HashIndex];
  while Assigned(Result) do
  begin
    if (Result.Name = aName) then
      PrevVersion := Result;

    Inc(HashIndex, 1);
    Result := MySymbolsHashedOnName[HashIndex];
  end;

  Result := TSymbolInformation.Create;
  Result.AliasSymbol := Result; // Initially, a symbol 'aliases' to itself
  Result.StoredLibraryFunction := aFoundFunction;
  Result.Name := aName;
  Result.UnmangledName := DxbxUnmangleSymbolName(aName);

  if Assigned(PrevVersion) then
  begin
    Result.OtherVersion := PrevVersion;
    PrevVersion.OtherVersion := Result;
  end;

  MySymbolsHashedOnName[HashIndex] := Result;
  Inc(FSymbolCount);
end;

function TSymbolManager.FindOrAddVersionedSymbol(const aAvailableInVersions: TLibraryVersionFlags;
  const aNameIndex: TStringTableIndex; const aFoundFunction: PStoredLibraryFunction = nil): TSymbolInformation;
var
  SymbolName: string;
  HashIndex: Word;
begin
  SymbolName := PatternTrieReader.GetString(aNameIndex);

  HashIndex := CalcCRC16(PByte(SymbolName), Length(SymbolName) * SizeOf(Char));
  Result := MySymbolsHashedOnName[HashIndex];
  while Assigned(Result) do
  begin
    if (Result.Name = SymbolName) then
    begin
      if Assigned(Result.StoredLibraryFunction) then
        if (Result.StoredLibraryFunction.AvailableInVersions * aAvailableInVersions) <> [] then
        begin
          Result := Result.AliasSymbol;
          Exit;
        end;

      // If no function - take over given :
      if Result.StoredLibraryFunction = nil then
        Result.StoredLibraryFunction := aFoundFunction;

      // If name and function match, we're done :
      if Result.StoredLibraryFunction = aFoundFunction then
      begin
        Result := Result.AliasSymbol;
        Exit;
      end;

      // function mismatch, continue search
    end;

    Inc(HashIndex, 1);
    Result := MySymbolsHashedOnName[HashIndex];
  end;

  Result := AddSymbol(SymbolName, aFoundFunction);
end; // FindOrAddVersionedSymbol

function TSymbolManager.FindSymbolWithAddress(const aName: string): TSymbolInformation;
var
  HashIndex: Word;
begin
  HashIndex := CalcCRC16(PByte(aName), Length(aName) * SizeOf(Char));
  Result := MySymbolsHashedOnName[HashIndex];
  while Assigned(Result) do
  begin
    if (Result.Name = aName) then
    begin
      Result := Result.AliasSymbol;
      if Assigned(Result.Address) then
        Exit;
    end;

    Inc(HashIndex, 1);
    Result := MySymbolsHashedOnName[HashIndex];
  end;
end; // FindSymbolWithAddress

///
/// Leaf scanning code :
///

// 'Convert' the 1st function index in the leaf to a LeafID
function TSymbolManager.GetLeafID(const LeafNode: PStoredTrieNode): TLeafID;
var
  StretchPtr: PByte;
  More: Boolean;
  NrFixed, NrWildcards: Integer;
begin
  // Step to the stretch (yeah, a leaf node also contains stretches) :
  GetNodeNrChildren(LeafNode, {out}StretchPtr);

  // Skip over all stretches :
  repeat
    More := DecodeStretchHeader({var}StretchPtr, {out}NrFixed, {out}NrWildcards);
    Inc(StretchPtr, NrFixed);
  until not More;

  // Right after that, is the first function index, return that as a LeafID:
  Result := TLeafID(PFunctionIndex(StretchPtr)^);
end;

procedure TSymbolManager.AddLeafHit(const aAddress: PByte; const aFirstFunctionIndex: PFunctionIndex; const NrChildren: Integer);
var
  LeafID: TLeafID;
  NewLength: Integer;
  Hit: PPatternHitResult;
begin
  // Read the first function index, which is what we use as LeafID :
  LeafID := TLeafID(aFirstFunctionIndex^);

  // Make sure we have room for this leaf :
  begin
    NewLength := Length(MyDetectedLeafs);
    if NewLength = 0 then
      NewLength := 16 * 1024;

    while NewLength <= Integer(LeafID) do
      Inc(NewLength, NewLength);

    if NewLength > Length(MyDetectedLeafs) then
      SetLength(MyDetectedLeafs, NewLength);
  end;

  // Append this address to the chain :
  begin
    Inc(FNrDetectedLeafs);

    New(Hit);
//    Hit.Section := Section;
    Hit.Address := aAddress;

    if MyDetectedLeafs[LeafID].FirstFunctionIndex = nil then
    begin
      Inc(NrLeafHits);
      MyDetectedLeafs[LeafID].FirstFunctionIndex := aFirstFunctionIndex;
      MyDetectedLeafs[LeafID].NrChildren := NrChildren;
      Hit.NextHit := nil;
    end
    else
    begin
      Assert(MyDetectedLeafs[LeafID].FirstFunctionIndex = aFirstFunctionIndex);
      Assert(MyDetectedLeafs[LeafID].NrChildren = NrChildren);
      Hit.NextHit := MyDetectedLeafs[LeafID].FirstHit;
    end;

    MyDetectedLeafs[LeafID].FirstHit := Hit;
  end;
end; // AddLeafHit

procedure TSymbolManager.CheckSectionAddressForPatternHit(const aTestAddress: PByte);

  function _TryMatchingNode(aStoredTrieNode: PStoredTrieNode; aAddress: PByte; Depth: Integer): Boolean;
  var
    NrChildren: Integer;
    StretchPtr: PByte;
    More: Boolean;
    NrFixed, NrWildcards, i: Integer;
    NextOffset: TByteOffset;
  begin
    Result := False;
{$IFDEF DXBX_RECTYPE}
    Assert(aStoredTrieNode.RecType = rtStoredTrieNode, 'StoredTrieNode type mismatch!');
{$ENDIF}
    // Calculate the position of the data after this TrieNode (StretchPtr) :
    NrChildren := GetNodeNrChildren(aStoredTrieNode, {out}StretchPtr);

    // Scan all stretches after this node :
    repeat
      More := DecodeStretchHeader({var}StretchPtr, {out}NrFixed, {out}NrWildcards);
      if NrWildcards < 0 then
        NrWildcards := PATTERNSIZE - (Depth + NrFixed);

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
      // Remember the original address, and the leaf which gave a hit :
      AddLeafHit(aTestAddress, PFunctionIndex(StretchPtr), NrChildren);
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
        Break; // TODO : Remove this when the Trie is proven to contain no errors

      // Jump to next sibling :
      aStoredTrieNode := PatternTrieReader.GetNode(NextOffset);
      Dec(NrChildren);
    end;
  end; // _TryMatchingNode

begin
  _TryMatchingNode(PatternTrieReader.RootNode, aTestAddress, {aDepth=}0);
end; // CheckSectionAddressForPatternHit

procedure TSymbolManager.ScanAddressRangeForPatternHits(const pXbeHeader: PXBEIMAGE_HEADER);
var
  i: DWord;
  Section: PXBE_SECTIONHEADER;
  j: Integer;
  StoredLibrary: PStoredLibrary;
  StoredLibraryName: string;
  ScanEnd: UIntPtr;
  ScanPtr: UIntPtr;

  function _SectionIsKnown(const aSectionName: string): Boolean;
  begin
    Result := (aSectionName = 'D3D')
           or (aSectionName = 'D3DX')
//           or (aSectionName = 'DOLBY') // Contains 'Initialized Data' from "dsound.lib"
           or (aSectionName = 'DMUSIC')
           or (aSectionName = 'DSOUND')
           or (aSectionName = 'WMADEC')
           or (aSectionName = 'WMADECXM')
           or (aSectionName = 'XACTENG')
           or (aSectionName = 'XMV')
           or (aSectionName = 'XPP') // Contains code from "xapilib.lib"
           or (aSectionName = 'XGRPH') // Contains code from "xgraphics.lib"
           or (aSectionName = 'XNET')
           or (aSectionName = 'XONLINE');
  end;

begin
  Assert(Assigned(pXbeHeader));
  Assert(pXbeHeader.dwSections > 0);

  // Retrieve the entry point, for registration & checking later on :
  EntryPoint := GetEntryPoint(pXbeHeader);

  // Determine upper bound for scanning, using highest virtual address :
  ScanUpper := Low(ScanUpper);
  UIntPtr(Section) := UIntPtr(pXbeHeader) + pXbeHeader.dwSectionHeadersAddr - pXbeHeader.dwBaseAddr;
  for i := 0 to pXbeHeader.dwSections - 1 do
  begin
    ScanPtr := UIntPtr(Section.dwVirtualAddr);
    ScanEnd := ScanPtr + Section.dwVirtualSize;
    if ScanUpper < ScanEnd then
      ScanUpper := ScanEnd;

    Inc(Section);
  end;

  // Init work variables :
  MyAddressesInKnownSections.Size := 0;
  MyAddressesInKnownSections.Size := ScanUpper;
  UIntPtr(Section) := UIntPtr(pXbeHeader) + pXbeHeader.dwSectionHeadersAddr - pXbeHeader.dwBaseAddr;
  SetLength(SectionInfo, pXbeHeader.dwSections);

  // Loop over all XBE sections, checking & registering various aspects :
  for i := 0 to pXbeHeader.dwSections - 1 do
  begin
    // Remember section & (raw) scan-size :
    ScanPtr := UIntPtr(Section.dwVirtualAddr);
    ScanEnd := ScanPtr + Section.dwSizeofRaw;
    SectionInfo[i].SectionHeader := Section;
    SectionInfo[i].SectionEndAddr := ScanEnd;
    SectionInfo[i].SectionName := string(PCharToString(PAnsiChar(Section.dwSectionNameAddr), XBE_SECTIONNAME_MAXLENGTH));

//    // .text is a special case; Compiler&Linker-effects cause pattern-deviations in it!
//    if (SectionName = '.text') then
//      TextSection := Section; // TODO : Research if all detections before _main (if in .text at all) should always be avoided?
//    if (aSectionName = '.rdata') then
//      ; // "Fable The Lost Chapters" has it's _main() here
//    if (aSectionName = '.data') then
//      ; // Contains kernel thunk table in some XBEs

    // Mark only those sections that we know about :
    if _SectionIsKnown(SectionInfo[i].SectionName) then
    begin
      // Determine section's corresponding library name :
      if SectionInfo[i].SectionName = 'XPP' then
        SectionInfo[i].LibraryName := 'xapilib'
      else
      if SectionInfo[i].SectionName = 'XGRPH' then
        SectionInfo[i].LibraryName := 'xgraphics'
      else
      if SectionInfo[i].SectionName = 'D3D' then
        SectionInfo[i].LibraryName := 'd3d8'
      else
      if SectionInfo[i].SectionName = 'D3DX' then
        SectionInfo[i].LibraryName := 'd3dx8'
      else
        SectionInfo[i].LibraryName := SectionInfo[i].SectionName;

      // Look up library name index :
      for j := 0 to PatternTrieReader.StoredSignatureTrieHeader.LibraryTable.NrOfLibraries - 1 do
      begin
        StoredLibrary := PatternTrieReader.GetStoredLibrary(j);
        StoredLibraryName := PatternTrieReader.GetString(StoredLibrary.LibNameIndex);
        if SameLibName(StoredLibraryName, SectionInfo[i].LibraryName) then
        begin
          SectionInfo[i].LibraryNameIndex := StoredLibrary.LibNameIndex;
          Break;
        end;
      end;

      // Reserve a bit per address that's contained in a section that's covered by our library patterns :
      while ScanPtr < ScanEnd do
      begin
        MyAddressesInKnownSections[ScanPtr] := True;
        Inc(ScanPtr);
      end;
    end;

    Inc(Section);
  end;

  // Reserve a bit per address to see which addresses are already identified :
  MyAddressesIdentified.Size := 0;
  MyAddressesIdentified.Size := ScanUpper;

  // Do the actual scanning per section :
  for i := 0 to Length(SectionInfo) - 1 do
  begin
    // Only scan sections ...
    // .. when there's something in it :
    if  (SectionInfo[i].SectionHeader.dwSizeofRaw > 0)
    // .. names not starting with a '$' :
    and (SectionInfo[i].SectionName <> '')
    and (SectionInfo[i].SectionName[1] <> '$')
    // .. and not in 'BINK*' sections :
    and (not SameText('BINK', Copy(SectionInfo[i].SectionName, 1, 4))) then
    begin
      ScanPtr := UIntPtr(SectionInfo[i].SectionHeader.dwVirtualAddr);
      ScanEnd := ScanPtr + SectionInfo[i].SectionHeader.dwSizeofRaw; // Don't scan outside of raw size!

      if MayLog(lfUnit or lfDebug) then
      begin
        DbgPrintf('DxbxHLE : Detecting functions in section $%0.4x (%s) from $%.8x to $%.8x', [
          i, SectionInfo[i].SectionName, ScanPtr, ScanEnd]);
      end;

      while ScanPtr < ScanEnd do
      try
        // Now check this address, adding a LeafHit when apropriate :
        CheckSectionAddressForPatternHit(PByte(ScanPtr));
        Inc(PByte(ScanPtr));

      except
{$IFDEF DXBX_DEBUG}
        DbgPrintf('DxbxHLE : Exception while scanning on address $%.8x', [ScanPtr]);
{$ENDIF}
        Inc(ScanPtr);
      end;
    end;
  end;
end; // ScanAddressRangeForPatternHits

procedure TSymbolManager.ConvertLeafsIntoSymbols();

  function _IsAliasFunction(aStoredLibraryFunction1, aStoredLibraryFunction2: PStoredLibraryFunction): Boolean;
  var
    i: Integer;
    Reference1, Reference2: PStoredSymbolReference;
  begin
    Result := (aStoredLibraryFunction1.FunctionLength = aStoredLibraryFunction2.FunctionLength)
          and (aStoredLibraryFunction1.CRCLength = aStoredLibraryFunction2.CRCLength)
          and (aStoredLibraryFunction1.CRCValue = aStoredLibraryFunction2.CRCValue)
          and (aStoredLibraryFunction1.NrOfSymbolReferences = aStoredLibraryFunction2.NrOfSymbolReferences)
          and (  (aStoredLibraryFunction1.AvailableInVersions * FLibraryVersionsToScan)
               = (aStoredLibraryFunction2.AvailableInVersions * FLibraryVersionsToScan));
    if not Result then
      Exit;

    // Check all references are identical too :
    for i := 0 to aStoredLibraryFunction1.NrOfSymbolReferences - 1 do
    begin
      Reference1 := PatternTrieReader.GetSymbolReference(aStoredLibraryFunction1.FirstSymbolReference + Cardinal(i));
      Reference2 := PatternTrieReader.GetSymbolReference(aStoredLibraryFunction2.FirstSymbolReference + Cardinal(i));

      if (Reference1.Offset         <> Reference2.Offset)
      or (Reference1.BaseOffset     <> Reference2.BaseOffset)
      or (Reference1.NameIndex      <> Reference2.NameIndex) // Hm... these could be aliasses too....
      or (Reference1.ReferenceFlags <> Reference2.ReferenceFlags) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

  function _IsAliasSymbol(const aSymbol1, aSymbol2: TSymbolInformation): Boolean;
  begin
    Result := (aSymbol1.Name <> aSymbol2.Name)
          and _IsAliasFunction(aSymbol1.StoredLibraryFunction, aSymbol2.StoredLibraryFunction);
    if Result then
      Result := (aSymbol1.SymbolReferenceCount > 0)
            or SameStr(aSymbol1.UnmangledName, aSymbol2.UnmangledName);
  end;

var
  i, j, k: Integer;
  FunctionIndex: PFunctionIndex;
  FunctionName: string;
  NrValidSymbols: Integer;
  ValidSymbols: array of TSymbolInformation;
  StoredFunction: PStoredLibraryFunction;
begin
  Assert(FSymbolCount = 0);
  SetLength(ValidSymbols, 16);

  // Loop over all leafs :
  for i := 0 to Length(MyDetectedLeafs) - 1 do
  begin
    // Check if the leaf was reached in the scan :
    FunctionIndex := MyDetectedLeafs[i].FirstFunctionIndex;
    if FunctionIndex = nil then
      Continue;

    // Loop over all functions in the leaf :
    NrValidSymbols := 0;
    for j := 0 to MyDetectedLeafs[i].NrChildren - 1 do
    begin
      // Check if the function is available in the current SDK version :
      StoredFunction := PatternTrieReader.GetStoredLibraryFunction(FunctionIndex^);
      if (StoredFunction.AvailableInVersions * FLibraryVersionsToScan) <> [] then
      begin
        // Create a symbol & remember it :
        FunctionName := PatternTrieReader.GetFunctionName(FunctionIndex^);
        // Note : If we have two different versions of the same symbol, both will be created :
        ValidSymbols[NrValidSymbols] := AddSymbol(FunctionName, StoredFunction);
        Inc(NrValidSymbols);

        // Make room when needed :
        if Length(ValidSymbols) = NrValidSymbols then
          SetLength(ValidSymbols, 2 * NrValidSymbols);
      end;

      Inc(FunctionIndex);
    end; // for valid leaf children

    // Find & Register aliases (duplicate declarations) between these symbols :
    for j := 0 to NrValidSymbols - 1 do
    begin
      // Loop over all following symbols :
      for k := j + 1 to NrValidSymbols - 1 do
        // Check if these are equal to the initial symbol :
        if  (ValidSymbols[k].AliasSymbol = ValidSymbols[k]) // Only alias when it still points to itself
        and (ValidSymbols[k].OtherVersion = nil) // Don't alias other versions
        and _IsAliasSymbol(ValidSymbols[k], ValidSymbols[j]) then
        begin
          // Alias one to the other :
          ValidSymbols[k].AliasSymbol := ValidSymbols[j];
          DbgPrintf('Aliased %s (%s) to %s (%s)', [
            ValidSymbols[k].Name,
            ValidSymbols[k].UnmangledName,
            ValidSymbols[j].Name,
            ValidSymbols[j].UnmangledName
            ]);
        end;
    end; // for aliases
  end; // for leafs
end; // ConvertLeafsIntoSymbols


function _IsTestCase(const aAddress: UIntPtr; const aMangledName: string): Boolean;

  function _Test(const aCheckAddress: UIntPtr; const aCheckName: string): Boolean;
  begin
    Result := (aCheckAddress = aAddress) or SameStr(aCheckName, aMangledName);
  end;

begin
  Result := False
  // PetitCopter
//  or _Test($0004779E, '_XapiInitProcess@0')
//  or _Test($000476C0, '_XapiVerifyMediaInDrive@0')
//  // Myst III - New symbol detection :
//  or _Test(nil, '_DirectSoundCreate@12')
//  or _Text(nil, '?BltBox2D@CD3DXBlt@@IAEJXZ')
//
//  // Turok - Fixed wrong locations and duplicates :
//  or _Test(?, '_D3D__RenderState'))
//  or _Test(?, '?CommonSetDebugRegisters@D3D@@YIXXZ'))
//  or _Test(?, '_XapiProcessHeap'))
//  or _Test($0001171D, '_XapiInitProcess@0')
//  or _Test($00011A35, '_RaiseException@16')
//  or _Test($00011A9D, '_XRegisterThreadNotifyRoutine@8')
//  or _Test($001939B0, 'EmuIDirect3DDevice8_SetRenderState_StencilEnable') // TODO : Use correct pattern-name!
//  or _Test($00193A40, 'EmuIDirect3DDevice8_SetRenderState_StencilFail') // TODO : Use correct pattern-name!
//  or _Test($00193AB0, 'EmuIDirect3DDevice8_SetRenderState_YuvEnable') // TODO : Use correct pattern-name!
//  or _Test($00193AE0, 'EmuIDirect3DDevice8_SetRenderState_OcclusionCullEnable') // TODO : Use correct pattern-name!
//  or _Test($00193B50, 'EmuIDirect3DDevice8_SetRenderState_StencilCullEnable') // TODO : Use correct pattern-name!
//  or _Test($00193BC0, '_D3DDevice_SetRenderState_RopZCmpAlwaysRead@4'))
//  or _Test($00193BE0, '_D3DDevice_SetRenderState_RopZRead@4'))
//  or _Test($00193C00, '_D3DDevice_SetRenderState_DoNotCullUncompressed@4'))
//
//  or _Test($00194EA0, '_D3DDevice_SetLight@8')
//  or _Test($00195080, '_D3DDevice_LightEnable@8')
//  or _Test($00195170, '?SetTexture@D3DDevice@@SGJKPAUD3DBaseTexture@@@Z')
//  or _Test($001960E0, 'EmuIDirect3DBaseTexture8_GetLevelCount') // TODO : Use correct pattern-name!
//  or _Test($001997F0, '?Get2DSurfaceDesc@PixelJar@D3D@@YGXPAUD3DPixelContainer@@IPAU_D3DSURFACE_DESC@@@Z') //HLE: 0x001997F0 -> EmuGet2DSurfaceDesc
//  or _Test($0019D360, '_D3DDevice_Reset@4') //HLE: 0x0019D360 -> EmuIDirect3DDevice8_Reset
//  or _Test($001B018A, 'CDirectSound_SetRolloffFactor') // TODO : Use correct pattern-name!
//  or _Test($001F35E3, '_USBD_Init@8') // Cxbx incorrectly calls this XInitDevices!
//
//  // Cxbx meshes :
//  or _Test($0001DE60, '_D3DDevice_SetTile@8') // EmuIDirect3DDevice8_SetTile
//  or _Test($00020200, '?SetFence@D3D@@YGKK@Z') // D3D::SetFence (XREF)
//  or _Test($0002CC34, '_XapiProcessHeap')
//
//  // Compiled Xdk samples\Tutorials\Tut01_CreateDevice\Release :
//  or _Test($0001F2C0, '?GetSurfaceFormat@PixelJar@D3D@@YGKPAUD3DPixelContainer@@0@Z') // from EmuIDirect3DDevice8_Clear
//  or _Test($00019610, '_D3DDevice_Clear@24') // EmuIDirect3DDevice8_Clear
//
//  // Psx :
//  or _Test($00114A09, '_IDirectSoundBuffer_Play@16')
//  or _Test($00115AC2, '_DirectSoundCreate@12')
//
//  // Gamepad
//  or _Test($00016FBF, '_XapiInitProcess@0')
//  or _Test($00016966, '_SetLastError@4')
//  or _Test($000340B3, '_XInputOpen@16')
//
//  or _Test($00022020, '_D3DDevice_SetRenderState_StencilEnable@4')
//  or _Test($00022150, '_D3DDevice_SetRenderState_OcclusionCullEnable@4')
//  or _Test($00022230, '_D3DDevice_SetRenderState_RopZCmpAlwaysRead@4')
//  or _Test($00022250, '_D3DDevice_SetRenderState_RopZRead@4')
//  or _Test($00022290, '_D3DDevice_SetRenderState_MultiSampleAntiAlias@4')
//  or _Test($00022B70, '_D3DDevice_GetDepthStencilSurface@4')
//  or _Test($00030E38, '?D3DXVec3LengthSq@@YAMPBUD3DXVECTOR3@@@Z')
//  or _Test($00030E80, '_D3DXVec3Normalize@8')

  // Smashing Drive
//  or _Test($000A5620, '?Init@CDevice@D3D@@QAEJPAU_D3DPRESENT_PARAMETERS_@@@Z'); // "address range taken" ? Fixed.
//  or _Test($000DA7ED, '@OHCD_fIsochCloseEndpoint@8') // 3911 has ref to @OHCD_ScheduleAddEndpointPeriodic@8 at +$21 (4631 refs @OHCD_fPauseEndpoint@8) Fixed.
//  or _Test($000A0320, '_D3DDevice_BlockUntilIdle@0') // Address appeared 2 times (but overlap not allowed!) Fixed.
//  or _Test($000A0320, '_D3DVolumeTexture_GetLevelDesc@12') // 2nd appearance
//  or _Test($000D4CA9, '_XInputGetCapabilities@8') // TODO : Fix missing
//  or _Test($000D9DE6, '@OHCD_fPauseEndpoint@8') // Busy.: 2 version found at conflicting addresses
//  or _Test($000D8A30, '@OHCD_fPauseEndpoint@8') // Busy.: 2 version found at conflicting addresses
//  or _Test($000D8A30, '@OHCD_ScheduleRemoveEndpointPeriodic@8') // Also conflicting
//  or _Test($000DA7ED, '@OHCD_fIsochCloseEndpoint@8') // Cause - 2 versions have different references

end; // _IsTestCase

///
/// Function scanning code :
///

function TSymbolManager.IsAddressWithinScanRange(const aAddress: TCodePointer): Boolean;
begin
  Result := (UIntPtr(aAddress) >= XBE_IMAGE_BASE) and (UIntPtr(aAddress) <= ScanUpper);
end;

function TSymbolManager.MayCheckFunction(const aStoredLibraryFunction: PStoredLibraryFunction): Boolean;
// Determines if a function should be scanned for
begin
  // Skip small functions with only one symbol-reference, because those are
  // very common. Instead, we hope they will be discovered via other functions :
  case aStoredLibraryFunction.NrOfSymbolReferences of
    0: Result := (aStoredLibraryFunction.FunctionLength > 7);
    1: Result := (aStoredLibraryFunction.FunctionLength > 5);
    2: Result := (aStoredLibraryFunction.FunctionLength > 9);
  else
    Result := True;
  end;
end; // MayCheckFunction

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
end; // GetReferencedSymbolAddress

procedure TSymbolManager.AddIntermediateSymbolRegistration(const CurrentSymbol: TSymbolInformation;
  const aAddress: PByte; const IsFailureRegistration: Boolean = False);
begin
  // Only register intermediate results when the address is not set already :
  if CurrentSymbol.Address = aAddress then
  begin
    if IsTestCase then
    begin
      DbgPrintf('AddIntermediateSymbolRegistration(%s) %d - Address already determined :', [
        CurrentSymbol.Name,
        FIntermediateSymbolRegistrationsCount
        ]);

      DbgPrintf('$%.08x;%s [-$%.08x] %s', [
        CurrentSymbol.Address,
        CurrentSymbol.UnmangledName,
        UIntPtr(CurrentSymbol.Address) + CurrentSymbol.Length - 1,
        CurrentSymbol.Discovery
        ]);
    end;

    Exit;
  end;

  // To register, mark the covered addresses as being 'identified', and set the address in this symbol :
  if not IsFailureRegistration then
  begin
    MyAddressesIdentified.SetRange(UIntPtr(aAddress), CurrentSymbol.Length);
    CurrentSymbol.RangeClaimed := True;
  end;
  CurrentSymbol.Address := aAddress;

  // Make room for an new intermediate registration :
  Inc(FIntermediateSymbolRegistrationsCount);
  if Length(MyIntermediateSymbolRegistrations) < FIntermediateSymbolRegistrationsCount then
    SetLength(MyIntermediateSymbolRegistrations, FIntermediateSymbolRegistrationsCount * 2);

  // The only thing we need to remember, is the symbol itself :
  MyIntermediateSymbolRegistrations[FIntermediateSymbolRegistrationsCount-1] := CurrentSymbol;
end; // AddIntermediateSymbolRegistration

procedure TSymbolManager.ConfirmIntermediateSymbolRegistrations(const CurrentSymbol: TSymbolInformation);
var
  i: Integer;
  Symbol: TSymbolInformation;
begin
  if FIntermediateSymbolRegistrationsCount = 0 then
    Exit;

  if MayLog(lfUnit) or IsTestCase then // TODO : Add lfExtreme flag once the output is reliable
  begin
    DbgPrintf('ConfirmIntermediateSymbolRegistrations(%s) %d', [
      CurrentSymbol.Name,
      FIntermediateSymbolRegistrationsCount
      ]);

    for i := 0 to FIntermediateSymbolRegistrationsCount - 1 do
    begin
      Symbol := MyIntermediateSymbolRegistrations[i];
      DbgPrintf('$%.08x;%s [-$%.08x] %s %s', [
        Symbol.Address,
        Symbol.{Unmangled}Name,
        UIntPtr(Symbol.Address) + Symbol.Length - 1,
        Symbol.Discovery,
        Symbol.FailReason
        ]);
    end;
  end;

  // There's not much to it, really - confirming the registrations is just
  // a matter of not reverting them (as they've already been registered) :
  FIntermediateSymbolRegistrationsCount := 0;
end; // ConfirmIntermediateSymbolRegistrations

procedure TSymbolManager.RevertIntermediateSymbolRegistrations(const CurrentSymbol: TSymbolInformation; NrToRevert: Integer = -1);

  procedure _Revert(Symbol: TSymbolInformation);
  begin
    if MayLog(lfUnit) or IsTestCase then // TODO : Add lfExtreme flag once the output is reliable
      DbgPrintf('NOT $%.08x;%s [-$%.08x] %s %s', [
        Symbol.Address,
        Symbol.{Unmangled}Name,
        UIntPtr(Symbol.Address) + Symbol.Length - 1,
        Symbol.Discovery,
        Symbol.FailReason
        ]);

    if Symbol.RangeClaimed then
      MyAddressesIdentified.ClearRange(UIntPtr(Symbol.Address), Symbol.Length);
    Symbol.RangeClaimed := False;
    Symbol.Address := nil;
    Symbol.Discovery := '';
    Symbol.FailReason := '';
    Symbol.IsApproximation := False;
  end;

var
  i: Integer;
begin
  if (FIntermediateSymbolRegistrationsCount = 0)
  or (NrToRevert = 0) then
    Exit;

  if MayLog(lfUnit) or IsTestCase then // TODO : Add lfExtreme flag once the output is reliable
    DbgPrintf('RevertIntermediateSymbolRegistrations(%s) %d', [
      CurrentSymbol.Name,
      FIntermediateSymbolRegistrationsCount
      ]);

  if NrToRevert > 0 then
  begin
    if NrToRevert > FIntermediateSymbolRegistrationsCount then
      NrToRevert := FIntermediateSymbolRegistrationsCount;

    Dec(FIntermediateSymbolRegistrationsCount, NrToRevert);
    for i := 0 to NrToRevert - 1 do
      _Revert(MyIntermediateSymbolRegistrations[FIntermediateSymbolRegistrationsCount + i]);
  end
  else
  begin
    // Loop over all intermediate registrations, and remove their trails :
    for i := 0 to FIntermediateSymbolRegistrationsCount - 1 do
      _Revert(MyIntermediateSymbolRegistrations[i]);

    // Reset the index, so the array can be filled again :
    FIntermediateSymbolRegistrationsCount := 0;
  end;
end; // RevertIntermediateSymbolRegistrations

const
  // Check results (positive values are acceptable, negative ones are not) :
  CR_APPROXIMATION = 0;

  CR_ADDRESS_MATCH = 1;
  CR_OPERATOR_IGNORED = 2;
  CR_DATA_OKAY = 3;
  CR_ALL_CHECKS_OKAY = 4;

  CR_ADDRESS_ALREADY_SET = -1;
  CR_ADDRESS_RANGE_TAKEN = -2;
  CR_CRC_FAILED = -3;
  CR_REFERENCE_OUT_OF_RANGE = -4;
  CR_REFERENCE_INCONSISTENCY = -5;
  CR_WRONG_SECTION = -6;

function TSymbolManager.CheckFunctionAtAddressAndRegister(const CurrentSymbol: TSymbolInformation; const aAddress: PByte; const Prefix: string = ''): Integer;

  function _CheckFunctionCRC(const aStoredLibraryFunction: PStoredLibraryFunction; const aAddress: PByte): Boolean;
  begin
    if aStoredLibraryFunction.CRCLength > 0 then
      Result := (aStoredLibraryFunction.CRCValue = CalcCRC16(aAddress, aStoredLibraryFunction.CRCLength))
    else
      Result := True;
  end;

  // This is to prevent scanning functions in other sections than their own (and maybe .text?)
  function _IsAddressInsideLibrary(const aAddress: UIntPtr; const aLibraryNameIndex: TStringTableIndex): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to Length(SectionInfo) - 1 do
    begin
      if  (aAddress >= SectionInfo[i].SectionHeader.dwVirtualAddr)
      and (aAddress < SectionInfo[i].SectionEndAddr)
      and (SectionInfo[i].LibraryNameIndex > 0) then
      begin
        Result := (SectionInfo[i].LibraryNameIndex = aLibraryNameIndex);
        Exit;
      end;
    end;

    Result := True;
  end;

  function _GetVersionedStoredFunction(const aAvailableInVersions: TLibraryVersionFlags;
    const aNameIndex: TStringTableIndex): PStoredLibraryFunction;
  var
    i: Integer;
    StoredStringHeader: PStoredStringHeader;
  begin
    StoredStringHeader := PatternTrieReader.GetStringHeader(aNameIndex);
    // TODO : Data/function distinction could be put in StoredSymbolReference.ReferenceFlags...
    for i := 0 to Integer(StoredStringHeader.NrOfFunctions) - 1 do
    begin
      // Check if the function is from the correct SDK version (skip the others) :
      Result := PatternTrieReader.GetStoredLibraryFunction(StoredStringHeader.FirstFunctionIndex + TFunctionIndex(i));
      if (Result.AvailableInVersions * aAvailableInVersions) <> [] then
        Exit;
    end;

    // Clear the function pointer, in case the reference is a data symbol :
    Result := nil;
  end; // _GetVersionedStoredFunction

  function _IsAddressInKnownSection(const aAddress: UIntPtr): Boolean;
  begin
    if (aAddress < UIntPtr(MyAddressesInKnownSections.Size)) then
      Result := MyAddressesInKnownSections[aAddress]
    else
      Result := False;
  end;

var
  Referenced: array of record
    StoredSymbolReference: PStoredSymbolReference;
    ReferencedAddress: TCodePointer;
    Symbol: TSymbolInformation;
  end;
  Version1: TSymbolInformation;
  Version2: TSymbolInformation;
  Function_: PStoredLibraryFunction;
  i, j: Integer;
begin
  if _IsTestCase(UIntPtr(aAddress), CurrentSymbol.Name) then
    IsTestCase := True;

  if IsTestCase then
    DbgPrintf('Test: CheckFunctionAtAddressAndRegister(%s, $%.08x, %s)', [
      CurrentSymbol.Name, UIntPtr(aAddress), Prefix]);

  if CurrentSymbol.IsOperator then
  begin
    Result := CR_OPERATOR_IGNORED;
    Exit;
  end;

  if CurrentSymbol.Discovery = '' then
    CurrentSymbol.Discovery := Prefix;

  // If the symbol was already detected here, return that fact :
  if (CurrentSymbol.Address = aAddress) then
  begin
    Result := CR_ADDRESS_MATCH;
    Exit;
  end;

  // If the symbol was already detected somewhere else, return that fact :
  if Assigned(CurrentSymbol.Address) then
  begin
    CurrentSymbol.FailReason := Format('address already set at $%.08x', [UIntPtr(CurrentSymbol.Address)]);
    Result := CR_ADDRESS_ALREADY_SET;
    Exit;
  end;

  // Check that none of the covered bytes are already identified :
  if not MyAddressesIdentified.IsRangeClear(UIntPtr(aAddress), CurrentSymbol.Length) then
  begin
    CurrentSymbol.FailReason := 'address range taken';
    Result := CR_ADDRESS_RANGE_TAKEN;
    Exit;
  end;

  // See if the symbol has function information (if not, it's probably data, which we can't check any further) :
  if (CurrentSymbol.StoredLibraryFunction = nil) then
  begin
    // TODO : For data-symbols, we should somehow determine the length...

    // To facilitate recursive references checks (on a higher level), pretend the data-symbol is detected at this address :
    AddIntermediateSymbolRegistration(CurrentSymbol, aAddress);
    Result := CR_DATA_OKAY;
    Exit;
  end;

  if not _IsAddressInsideLibrary(UIntPtr(aAddress), CurrentSymbol.StoredLibraryFunction.LibraryNameIndex) then
  begin
    CurrentSymbol.FailReason := 'symbol cannot occur at another library address';
    Result := CR_WRONG_SECTION;
    Exit;
  end;

  // Check the CRC for this function on the given address :
  if not _CheckFunctionCRC(CurrentSymbol.StoredLibraryFunction, aAddress + PATTERNSIZE) then
  begin
    CurrentSymbol.FailReason := 'crc check failed';
    Result := CR_CRC_FAILED;
    Exit;
  end;

  // Collect all references and check they're all within bounds :
  SetLength(Referenced, CurrentSymbol.SymbolReferenceCount);
  for i := 0 to Length(Referenced) - 1 do
  begin
    Referenced[i].StoredSymbolReference := CurrentSymbol.SymbolReferences[i];

    // Lookup if the reference is actually a function reference (using the reference's name) :
    Function_ := _GetVersionedStoredFunction(CurrentSymbol.StoredLibraryFunction.AvailableInVersions, Referenced[i].StoredSymbolReference.NameIndex);

    // Retrieve an object for the symbol being referenced :
    Referenced[i].Symbol := FindOrAddVersionedSymbol(CurrentSymbol.StoredLibraryFunction.AvailableInVersions, Referenced[i].StoredSymbolReference.NameIndex, Function_);

    // Skip operators (they're often compiled-in on multiple places, like 'operator new' and 'operator new[]') :
    if Referenced[i].Symbol.IsOperator then
      Continue;

    Referenced[i].ReferencedAddress := GetReferencedSymbolAddress(aAddress, Referenced[i].StoredSymbolReference);

    // Check all duplicate symbol references point to the same location :
    if (Referenced[i].StoredSymbolReference.ReferenceFlags and rfIsDuplicate) > 0 then
    begin
      for j := 0 to i - 1 do
        if  (Referenced[j].StoredSymbolReference.NameIndex = Referenced[i].StoredSymbolReference.NameIndex)
        and (Referenced[j].ReferencedAddress <> Referenced[i].ReferencedAddress) then
        begin
          CurrentSymbol.FailReason := Format('reference #%d ($%.08x) should match reference #%d $%.08x', [
            j, UIntPtr(Referenced[j].ReferencedAddress),
            i, UIntPtr(Referenced[i].ReferencedAddress)]);
          Result := CR_REFERENCE_INCONSISTENCY;
          Exit;
        end;

      // No further checking needed on duplicate references :
      Continue;
    end;

    // Unique references should point to a valid address :
    if not IsAddressWithinScanRange(Referenced[i].ReferencedAddress) then
    begin
      CurrentSymbol.FailReason := Format('reference #%d out of range : $%.08x', [
        i, UIntPtr(Referenced[i].ReferencedAddress)]);
      Result := CR_REFERENCE_OUT_OF_RANGE;
      Exit;
    end;
  end;

  // To facilitate recursive references checks, pretend the function is detected at this address :
  AddIntermediateSymbolRegistration(CurrentSymbol, aAddress);

  // Recursively check all referenced symbols :
  for i := 0 to Length(Referenced) - 1 do
  begin
    // Don't check duplicates (the first one is not a duplicate by the way) :
    if (Referenced[i].StoredSymbolReference.ReferenceFlags and rfIsDuplicate) > 0 then
      Continue;

    // Do the recursive check :
    begin
      // Make sure we test the shortest of the two versions first,
      // to assure we're not claiming to much address range :
      Version1 := Referenced[i].Symbol;
      Version2 := Version1.OtherVersion;
      if Assigned(Version2) and (Version1.Length > Version2.Length) then
      begin
        Version1 := Version2; // 1 is shortest of the two now
        Version2 := Referenced[i].Symbol;
      end;

      // Remember the current number of registrations (so we can rollback when needed) :
      j := FIntermediateSymbolRegistrationsCount;
      Result := CheckFunctionAtAddressAndRegister(Version1, Referenced[i].ReferencedAddress, Prefix + '.' + IntToStr(i));

      // When there's a problem, and we're scanning using approximating patterns, try the other version (if any) :
      if  UsingLibraryApproximations
      and (Result <= CR_ADDRESS_ALREADY_SET)
      and Assigned(Version2) then
      begin
        // Remove any registrations done by the previous call :
        RevertIntermediateSymbolRegistrations(CurrentSymbol, FIntermediateSymbolRegistrationsCount - j);
        // And try again with the other version :
        Result := CheckFunctionAtAddressAndRegister(Version2, Referenced[i].ReferencedAddress, Prefix + '.' + IntToStr(i));
        Version1 := Version2;
      end
    end;

    // See if there was a failure of some kind :
    if Result <= CR_ADDRESS_ALREADY_SET then
    begin
      // If the failure wasn't address- or range-related :
      if Result < CR_ADDRESS_RANGE_TAKEN then
      begin
        // Check if this symbol couldn't be detected 100% reliably :
        if UsingLibraryApproximations
        or (not _IsAddressInKnownSection(UIntPtr(Referenced[i].ReferencedAddress))) then
        // Only if we started from a 'known' (ie: linked library) address :
        if ScanningInKnownAddresses then
        begin
          // Don't call any of the following exits a 'failure' anymore when they're big enough   :
          if MayCheckFunction(Version1.StoredLibraryFunction) then
            Result := CR_APPROXIMATION;
        end;
      end;

      if Result = CR_APPROXIMATION then
      begin
        // Approximations where failures, so they're not yet registered;
        // Mark them as an approximation (trunking their length a bit) and register anyway :
        Version1.IsApproximation := True;
        AddIntermediateSymbolRegistration(Version1, Referenced[i].ReferencedAddress);
        Version1.FailReason := 'Allowed approximation';
      end
      else
      begin
        // Here, the reference was really unacceptable, so we're gonna stop this check;

        // Add the failing symbol, for debugging purposes (but not for 'Address already set' failures,
        // to prevent removal of previously registered symbols!) :
        if Result = CR_ADDRESS_ALREADY_SET then
          // skip these
        else
          AddIntermediateSymbolRegistration(Version1, Referenced[i].ReferencedAddress, {IsFailureRegistration=}True);

        // When the recursive check fails, all temporary registrations will be reverted (see RevertIntermediateSymbolRegistrations)
        Exit;
      end;
    end;
  end;

  // All checks are done, we got the right symbol address (for now) !
  Result := CR_ALL_CHECKS_OKAY;
end; // CheckFunctionAtAddressAndRegister

procedure TSymbolManager.PrioritizeSymbolScan();

  function _HighestVersion(const aVersions: TLibraryVersionFlags): Integer;
  var
    i: TLibraryVersionFlag;
  begin
    for i := High(i) downto Low(i) do
    begin
      if i in aVersions then
      begin
        Result := LibraryVersionFlagToInt[i];
        Exit;
      end;
    end;

    Result := 0;
  end;

  function _Compare(Item1, Item2: Pointer): Integer;
  var
    Symbol1: TSymbolInformation absolute Item1;
    Symbol2: TSymbolInformation absolute Item2;
  begin
    // Hard-coded priority ordering (higher first) :
    Result := Integer(Symbol2.OrderingImportance) - Integer(Symbol1.OrderingImportance);
    if Result <> 0 then
      Exit;

    if Symbol2.Name = Symbol1.Name then
    begin
      // Version-based ordering (higher first) :
      Result := Integer(Symbol2.OrderingVersion) - Integer(Symbol1.OrderingVersion);
      if Result <> 0 then
        Exit;
    end;

    // Score-based ordering (higher first) :
    Result := Integer(Symbol2.OrderingScore) - Integer(Symbol1.OrderingScore);
    if Result <> 0 then
      Exit;

    // Make the ordering unique :
    Result := Integer(Item2) - Integer(Item1);
  end; // _Compare

const // Make these configurable :
  ScoreHasNoFunction = 0;
  ScoreHasFunction = 1000;

  ScoreNrOfSymbolReferences = 20;
  ScoreCRCLength = 10;
  ScoreFunctionLength = 1;
var
  w: Word;
  CurrentSymbol: TSymbolInformation;
begin
  // Note: FSymbolCount is grown in ConvertLeafsIntoSymbols using calls to AddSymbol
  MySymbols.Count := FSymbolCount;
  FSymbolCount := 0;
  for w := Low(w) to High(w) do
  begin
    CurrentSymbol := MySymbolsHashedOnName[w];
    if Assigned(CurrentSymbol) then
    begin
      MySymbols[FSymbolCount] := CurrentSymbol;
      Inc(FSymbolCount);

      // Note : Cxbx first scans for 'XapiInitProcess' and then for '_D3DDevice_SetRenderState_CullMode@4',
      // so we immitate that, and add some more important symbols while we're at it :
      if Pos('_mainCRTStartup', CurrentSymbol.Name) > 0 then begin CurrentSymbol.OrderingImportance := 100;
        // Register the entry point, which is the only symbol ("_mainCRTStartup") we know the address of for sure :
        CurrentSymbol.Address := TCodePointer(EntryPoint); end
      else if Pos('XapiInitProcess', CurrentSymbol.Name) > 0 then CurrentSymbol.OrderingImportance := 90
      else if Pos('_D3DDevice_SetRenderState_', CurrentSymbol.Name) > 0 then CurrentSymbol.OrderingImportance := 80
      else if Pos('_D3DDevice_', CurrentSymbol.Name) > 0 then CurrentSymbol.OrderingImportance := 75
      else if Pos('D3D', CurrentSymbol.Name) > 0 then CurrentSymbol.OrderingImportance := 70
//      else if Pos('Sound', CurrentSymbol.Name) > 0 then CurrentSymbol.OrderingImportance := 60
//      else if Pos('XInput', CurrentSymbol.Name) > 0 then CurrentSymbol.OrderingImportance := 50
      else CurrentSymbol.OrderingImportance := 0;

      if CurrentSymbol.StoredLibraryFunction = nil then
      begin
        CurrentSymbol.OrderingVersion := 0;
        CurrentSymbol.OrderingScore := ScoreHasNoFunction;
      end
      else
      begin
        CurrentSymbol.OrderingVersion := _HighestVersion(CurrentSymbol.StoredLibraryFunction.AvailableInVersions);
        CurrentSymbol.OrderingScore := ScoreHasFunction;

        Inc(CurrentSymbol.OrderingScore, (CurrentSymbol.StoredLibraryFunction.NrOfSymbolReferences * ScoreNrOfSymbolReferences));
        Inc(CurrentSymbol.OrderingScore, (CurrentSymbol.StoredLibraryFunction.CRCLength * ScoreCRCLength));
        Inc(CurrentSymbol.OrderingScore, (CurrentSymbol.StoredLibraryFunction.FunctionLength * ScoreFunctionLength));
      end;


    end;
  end;

  Assert(FSymbolCount = MySymbols.Count);

  // Sort all functions descending from most references and longest code length :
  MySymbols.Sort(@_Compare);
end; // PrioritizeSymbolScan

procedure TSymbolManager.LookupFunctionsInHits();
var
  i: Integer;
  CurrentSymbol: TSymbolInformation;
  StoredFunction: PStoredLibraryFunction;
  LeafNode: PStoredTrieNode;
  LeafID: TLeafID;
  CurrentHit: PPatternHitResult;
begin
  // Loop over the sorted list of possible functions :
  for i := 0 to MySymbols.Count - 1 do
  begin
    CurrentSymbol := TSymbolInformation(MySymbols[i]);
    StoredFunction := CurrentSymbol.StoredLibraryFunction;

    // Skip small functions to prevent too many hits (I guess we'll find most of them via references anyhow) :
    if not MayCheckFunction(StoredFunction) then
      Continue;

    // Retrieve the LeafNode in which this function ended up :
    LeafNode := PatternTrieReader.GetNode(StoredFunction.PatternLeafNodeOffset);
    // Get the ID from this leaf node :
    LeafID := GetLeafID(LeafNode);

    IsTestCase := False;

    // Do the following twice, first for 'known' addresses, then for all others :
    ScanningInKnownAddresses := True;
    repeat
      // Loop over all locations matching up to this leaf (and thus potentially hitting this function) :
      CurrentHit := MyDetectedLeafs[LeafID].FirstHit;
      while Assigned(CurrentHit) do
      begin
        // Check if we may check this address in this iteration (we'll be running this loop twice) :
        if MyAddressesInKnownSections[UIntPtr(CurrentHit.Address)] = ScanningInKnownAddresses then
        begin
          // Check the references recursively & register it (including it's implicated symbols):
          if CheckFunctionAtAddressAndRegister(CurrentSymbol, {CurrentHit.Section,} CurrentHit.Address, IntToStr(i)) >= CR_APPROXIMATION then
          begin
            // If the function is pinpointed successfully, confirm the resulting registrations and step to the next function :
            ConfirmIntermediateSymbolRegistrations(CurrentSymbol);
            ScanningInKnownAddresses := False; // Prevent a second scan
            Break;
          end;

          // The check failed, remove it's intermediate symbol registrations :
          RevertIntermediateSymbolRegistrations(CurrentSymbol);
        end;

        // And try an alternative hit on the same leaf :
        CurrentHit := CurrentHit.NextHit;
      end; // while hits

      // Flip sections to scan in :
      ScanningInKnownAddresses := not ScanningInKnownAddresses;
    // Stop search if both types are scanned :
    until ScanningInKnownAddresses = True;
  end; // for functions

end; // LookupFunctionsInHits

procedure TSymbolManager.CleanupTemporaryScanningData;

  procedure _RecursiveDisposeHits(const aHit: PPatternHitResult);
  begin
    if Assigned(aHit) then
    begin
      _RecursiveDisposeHits(aHit.NextHit);
      Dispose(aHit);
    end;
  end;

var
  i: Integer;
begin
  SetLength(MyIntermediateSymbolRegistrations, 0);

  for i := 0 to FNrDetectedLeafs - 1 do
    _RecursiveDisposeHits(MyDetectedLeafs[i].FirstHit);

  SetLength(MyDetectedLeafs, 0);
  FNrDetectedLeafs := 0;

  MyAddressesIdentified.Size := 0;
  MyAddressesInKnownSections.Size := 0;
end; // CleanupTemporaryScanningData

procedure TSymbolManager.PutSymbolsInFinalList;

  function _Compare_Symbol_Address(Item1, Item2: Pointer): Integer;
  begin
    Result := Integer(TSymbolInformation(Item1).Address) - Integer(TSymbolInformation(Item2).Address);
  end;

var
  w: Word;
  CurrentSymbol: TSymbolInformation;
begin
  if MayLog(lfUnit) then
    DbgPrintf('DxbxHLE : Collecting final symbols');

  MySymbols.Count := FSymbolCount;
  FSymbolCount := 0;

  // Loop over all symbols and put them in one big list :
  for w := Low(w) to High(w) do
  begin
    CurrentSymbol := MySymbolsHashedOnName[w];

    if (CurrentSymbol = nil) then
      Continue;

    if (CurrentSymbol.Address = nil) then
      Continue;

    MySymbols[FSymbolCount] := CurrentSymbol;
    Inc(FSymbolCount);
  end;

  MySymbols.Count := FSymbolCount;

  // Sort that list
  MySymbols.Sort(@_Compare_Symbol_Address);
end; // PutSymbolsInFinalList

procedure TSymbolManager.PrintSymbolList;
var
  i: Integer;
  CurrentSymbol: TSymbolInformation;
  Name: string;
begin
  if not MayLog(lfUnit or lfDebug) then
    Exit;

  for i := 0 to Count - 1 do
  begin
    CurrentSymbol := Symbols[i];

    Name := CurrentSymbol.UnmangledName;
    if (CurrentSymbol.StoredLibraryFunction = nil) then
      Name := Name + ' (XRef)';

    DbgPrintf('Found at 0x%.8x-0x%.8x : %s  [%s %s]',
      [CurrentSymbol.Address,
       UIntPtr(CurrentSymbol.Address) + UInt(CurrentSymbol.Length) - 1,
       Name,
       CurrentSymbol.Discovery,
       CurrentSymbol.Name]);
  end; // for Symbols
end; // PrintSymbolList

///
/// Support code :
///

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
    if MayLog(lfUnit) then
      DbgPrintf('DxbxHLE : No XBE library versions to scan!');

    Exit;
  end;

  StoredLibraryVersions := TStringList.Create;

  // Loop over all library versions in the executable:
  for i := 0 to pXbeHeader.dwLibraryVersions - 1 do
  begin
    CurrentLibName := string(PCharToString(@(CurrentXbeLibraryVersion.szName[0]), XBE_LIBRARYNAME_MAXLENGTH));
    if MayLog(lfUnit) then
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
        LibD3D8 := StoredLibrary;

      // Add this library to a set we'll use in the detection-code :
      FLibraryVersionsToScan := FLibraryVersionsToScan + [LibraryVersionNumberToFlag(StoredLibrary.LibVersion)];
      if StoredLibrary.LibVersion = CurrentXbeLibraryVersion.wBuildVersion then
        DbgPrintf('... Got patterns for exactly this version!')
      else
      begin
        UsingLibraryApproximations := True;
        DbgPrintf('... Approximating this with patterns from version %d', [StoredLibrary.LibVersion]);
        if PrevBestStoredLibraryIndex > -1 then
        begin
          StoredLibrary := PatternTrieReader.GetStoredLibrary(PrevBestStoredLibraryIndex);
          FLibraryVersionsToScan := FLibraryVersionsToScan + [LibraryVersionNumberToFlag(StoredLibrary.LibVersion)];
          DbgPrintf('    and version %d.', [StoredLibrary.LibVersion]);
        end;
      end;
    end
    else
      DbgPrintf('... No patterns registered for this library!');

    // Skip to the next library :
    Inc(CurrentXbeLibraryVersion);
  end; // for all library versions

  if Assigned(LibD3D8) then
    g_BuildVersion := LibD3D8.LibVersion
  else
    // For OpenSDK / SDLx linked XBEs, assume 4627 libs where used :
    g_BuildVersion := DEFAULT_XDK_VERSION; // TODO -oDxbx: Make this configurable!

  if IsRunning(TITLEID_PetitCopter) then
  begin
    // TODO : Make this configurable ?
    DbgPrintf('!Game-hack! PetitCopter also needs 4627 patterns (at least to verify ''_XapiVerifyMediaInDrive@0'' at $000476C0)');
    FLibraryVersionsToScan := FLibraryVersionsToScan + [lib4627];
    UsingLibraryApproximations := True;
  end;

  FreeAndNil(StoredLibraryVersions);
end; // DetectVersionedXboxLibraries

procedure TSymbolManager.LookupGlobalEmulationSymbols;
var
  Symbol: TSymbolInformation;
  s, v: int;
begin
  if MayLog(lfUnit) then
    DbgPrintf('DxbxHLE : Determining special symbols');

  // locate D3DDeferredTextureState
  begin
    XTL_EmuD3DDeferredTextureState := nil;
    Symbol := FindSymbolWithAddress('_D3D__TextureState');
    if Assigned(Symbol) then
      XTL_EmuD3DDeferredTextureState := Symbol.Address;

    if Assigned(XTL_EmuD3DDeferredTextureState) then
    begin
      for s := 0 to X_D3DTS_STAGECOUNT-1 do
      begin
        for v := 0 to X_D3DTS_STAGESIZE-1 do
          XTL_EmuD3DDeferredTextureState[s, v] := X_D3DTSS_UNK;
      end;

      if MayLog(lfUnit) then
        DbgPrintf('HLE: $%.08X -> EmuD3DDeferredTextureState',
          [XTL_EmuD3DDeferredTextureState]);
    end
    else
      EmuWarning('EmuD3DDeferredTextureState was not found!');
  end;

  // Locate the RenderState structure, and map all it's variables XDK version-independently :
  begin
    // Just search for _D3D__RenderState itself !
    Symbol := FindSymbolWithAddress('_D3D__RenderState');
    if Assigned(Symbol) then
      // Build up a mapping table based on this :
      DxbxBuildRenderStateMappingTable(Symbol.Address)
    else
      EmuWarning('EmuD3DDeferredRenderState was not found!');
  end;

  // locate XapiProcessHeap
  begin
    // Resolve the address of the _XapiProcessHeap symbol (at least referenced once, from XapiInitProcess) :
    Symbol := FindSymbolWithAddress('_XapiProcessHeap');
    if Assigned(Symbol) then
      // and remember that in a global :
      XTL_EmuXapiProcessHeap := Symbol.Address;

    if MayLog(lfUnit) then
    begin
      if Assigned(XTL_EmuXapiProcessHeap) then
        DbgPrintf('HLE: $%.08X -> XapiProcessHeap',
          [XTL_EmuXapiProcessHeap])
      else
        DbgPrintf('HLE : Can''t find XapiProcessHeap!');
    end;
  end;
end; // LookupGlobalEmulationSymbols

///
/// Cache related code :
///

class function TSymbolManager.CacheFileName(const pXbeHeader: PXBEIMAGE_HEADER): string;
var
  m_Certificate: PXBE_CERTIFICATE;
begin
  CRC32Init();
  ReinitXbeImageHeader;
  m_Certificate := PXBE_CERTIFICATE(pXbeHeader.dwCertificateAddr);
  Result := SymbolCacheFolder
          // TitleID
          + IntToHex(m_Certificate.dwTitleId, 8)
          // + CRC32 over XbeHeader :
          + '_' + IntToHex(CRC32(PByte(pXbeHeader), {Len=}pXbeHeader.dwSizeofHeaders), 8)
          // TitleName
          + '_' + GetReadableTitle(m_Certificate)
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
  if not FileExists(aCacheFile)
  and not g_EmuShared.m_EnableSymbolCache then
    Exit;

  Symbols := TStringList.Create;
  try
    uXbe.LoadSymbolsFromCache(Symbols, aCacheFile);

    // Add each entry to the list :
    MySymbols.Count := Symbols.Count;
    for i := 0 to Symbols.Count - 1 do
    begin
      FuncStr := Symbols[i];
      Addr := Pointer(Symbols.Objects[i]);
      if Assigned(Addr) then
      begin
        CurrentSymbol := AddSymbol(FuncStr, {FoundFunction=}nil);
        CurrentSymbol.Address := Addr;
        MySymbols[i] := CurrentSymbol;
        Result := True;
      end;
    end;
  finally
    FreeAndNil(Symbols);
  end;

  if Result then
    if MayLog(lfUnit) then
      DbgPrintf('DxbxHLE : Loaded symbols : %d.', [MySymbols.Count]);
end; // LoadSymbolsFromCache

procedure TSymbolManager.SaveSymbolsToCache(const pXbeHeader: PXBEIMAGE_HEADER; const aCacheFile: string);
var
  i: Integer;
  Symbol: TSymbolInformation;
  m_Certificate: PXBE_CERTIFICATE;
  TmpStr: string;
begin
  if Count <= 0 then
    Exit;

  if not ForceDirectories(ExtractFilePath(aCacheFile)) then
    Exit;

  with TStringList.Create do
  try
    for i := 0 to Self.Count - 1 do
    begin
      Symbol := Symbols[i];
      AddObject(Symbol.Name, Symbol);
    end;

    CustomSort(@SortObjects);

    with TIniFile.Create(aCacheFile) do
    try
      // Remove the entire [Symbols] section :
      EraseSection('Symbols');

      // Add some Xbe info into a separate section, can be expanded upon later :
      m_Certificate := PXBE_CERTIFICATE(pXbeHeader.dwCertificateAddr);
      WriteString('XbeInfo', 'TitleID', IntToHex(m_Certificate.dwTitleId, 8));
      WriteString('XbeInfo', 'TitleName', GetReadableTitle(m_Certificate));

      // (Re)generate the complete [Symbols] section :
      // In the [Symbols] section, each line looks like this :
      // ?Pause@CDirectSoundStream@DirectSound@@QAGJK@Z=$000CCDB1;DirectSound.CDirectSoundStream.Pause // PatchedBy
      // ^-------mangled function name----------------^ ^address^ ^----unmangled function name-------^
      for i := 0 to Count - 1 do
      begin
        Symbol := TSymbolInformation(Objects[i]);
        TmpStr := Format('$%.8x;%s', [UIntPtr(Symbol.Address), DxbxUnmangleSymbolName(Strings[i])]);
        if Symbol.PatchedBy <> '' then
          TmpStr := TmpStr + ' // PatchedBy:' + Symbol.PatchedBy
        else
          TmpStr := TmpStr + ' // ## UNPATCHED ##';

        WriteString('Symbols', Strings[i], TmpStr);
      end;
    finally
      Free;
    end;
  finally
    Free;
  end;
end; // SaveSymbolsToCache

///
/// Main detection method :
///

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
        // Scan the xbe memory using the trie, resulting in a bunch of LeafHits :
        ScanAddressRangeForPatternHits(pXbeHeader);

        ConvertLeafsIntoSymbols();

        PrioritizeSymbolScan();

        // Detect functions from those leafs, expanding recursively to all reachable symbols :
        LookupFunctionsInHits();

        CleanupTemporaryScanningData();

        // Populate the final symbol list :
        PutSymbolsInFinalList();

        PrintSymbolList();
      end;

      // Lookup a few symbols for global access during emulation :
      LookupGlobalEmulationSymbols();

    finally
      FreeAndNil(PatternTrieReader);
    end;

  finally
    // Unlock the resource :
    FreeAndNil(ResourceStream);
  end;

  if MayLog(lfUnit) then
    DbgPrintf('DxbxHLE : Detected %d symbols, reachable from %d leaf hits.', [Count, NrLeafHits]);

  // Now that the symbols are known, patch them up where needed :
  EmuInstallWrappers(pXbeHeader);

  // After detection of all symbols, see if we need to save that to cache :
  if CacheFileNameStr <> '' then
    SaveSymbolsToCache(pXbeHeader, CacheFileNameStr);

end; // DxbxScanForLibraryAPIs

initialization

  SymbolManager := TSymbolManager.Create;

finalization

  FreeAndNil(SymbolManager);

end.

