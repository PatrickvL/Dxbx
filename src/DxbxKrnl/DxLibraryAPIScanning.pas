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

{$DEFINE DO_BINARY_PATTERN_SEARCH}

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
  uPatterns, // auto-generated unit, containing AllPatternArrays
  uXboxLibraryUtils,
  uCRC16;

type
  TDetectedFunctions = class(TObject)
  protected
    FList: TList;
    function GetItem(Index: Integer): PDetectedXboxLibraryFunction;
  public
    function Count: Integer;
    property Items[Index: Integer]: PDetectedXboxLibraryFunction read GetItem; default;
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function New(const aInfo: PXboxLibraryFunction): PDetectedXboxLibraryFunction;
    function FindByName(const aName: string): PDetectedXboxLibraryFunction;
    function FindByAddress(const aAddress: TCodePointer): PDetectedXboxLibraryFunction;
  end;
  
procedure DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);

var
  DetectedFunctions: TDetectedFunctions;

implementation

function TestLibraryOnAddress(const aXboxLibraryInfo: PXboxLibraryInfo; const aAddress: PByte): PXboxLibraryFunction;

  function _ComparePattern(const aXboxLibraryFunction: PXboxLibraryFunction; aAddress: PByte): Integer;
  var
    b: Integer;
  begin
    // Loop over the first 32 pattern-bytes :
    Result := 0;
    for b := 0 to PATTERNSIZE - 1 do
    begin
      // Test if this byte offset must be checked :
      if TestBit(aXboxLibraryFunction.Pattern.BytesToUseMask, b) then
      begin
        Result := Integer(aXboxLibraryFunction.Pattern.Bytes[b]) - Integer(aAddress^);
        if Result <> 0 then
          Exit;
      end;

      Inc(aAddress);
    end;

    if aXboxLibraryFunction.CRCLength > 0 then
      Result := Integer(aXboxLibraryFunction.CRCValue) - Integer(CalcCRC16(aAddress, aXboxLibraryFunction.CRCLength));

    // TODO : Include data & test-code for : Cross-referenced APIs
    // TODO : Include data & test-code for : Trailing bytes
  end;

{$IFDEF DO_BINARY_PATTERN_SEARCH}

  // Modified copy of the binary search code in TList.Find() :
  function _FindPattern(const aPatternLibrary: TSortedPatterns; const aCount: Integer; const aAddress: PByte; var Index: Integer): Boolean;
  var
    L, H, I, C: Integer;
  begin
    Result := False;
    L := 0;
    H := aCount - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := _ComparePattern(aPatternLibrary[I], aAddress);
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
//          if Duplicates <> dupAccept then
//            L := I;
        end;
      end;
    end;

    Index := L;
  end;

{$ELSE} // not DO_BINARY_PATTERN_SEARCH

  function _FindPattern(const aPatternLibrary: TSortedPatterns; const aCount: Integer; const aAddress: PByte; var Index: Integer): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to aCount - 1 do
    begin
      Result := _ComparePattern(aPatternLibrary[i], aAddress) = 0;
      if Result then
      begin
        {var}Index := i;
        Exit;
      end;
    end;

    Result := False;
  end;
  
{$ENDIF ~DO_BINARY_PATTERN_SEARCH}

var
  Index: Integer;
begin
  // Search if this address matches a pattern :
  if _FindPattern(aXboxLibraryInfo.SortedPatterns, Length(aXboxLibraryInfo.SortedPatterns), aAddress, {var}Index) then
    Result := aXboxLibraryInfo.SortedPatterns[Index]
  else
    Result := nil;
end; // TestLibraryOnAddress

procedure DxbxScanForLibraryAPIs(const pLibraryVersion: PXBE_LIBRARYVERSION; const pXbeHeader: PXBE_HEADER);

  function _SortPatternArray(const aPatternArray: TPatternArray; const Len: Integer): TSortedPatterns;
  var
    List: TList;
    i: Integer;
  begin
    // We use a TList here, because it has an easy Sort method that array's lack...
    List := TList.Create;
    try
      // Build up a list of pointers to the patterns :
      List.Count := Len;
      for i := 0 to Len - 1 do
        List[i] := @(aPatternArray[i]);

      // Sort them in such a way that finding them can be done via bin-search :
      List.Sort(@PatternList_PatternCompare);

      // Transport the result over into a dynamic array :
      SetLength(Result, Len);
      for i := 0 to Len - 1 do
        Result[i] := List[i];
    finally
      FreeAndNil(List);
    end;
  end;

  function _FindAndRememberPattern(const aXboxLibraryInfo: PXboxLibraryInfo; const aAddress: PByte): PXboxLibraryFunction;
  var
    Detected: PDetectedXboxLibraryFunction;
  begin
    // Search if this address matches a pattern :
    Result := TestLibraryOnAddress(aXboxLibraryInfo, aAddress);
    if Result = nil then
      Exit;

    // Now that it's found, see if it was already registered :
    Detected := DetectedFunctions.FindByName(Result.Name);
    if Assigned(Detected) then
    begin
      // Count the number of times it was found (should stay at 1) :
      Inc(Detected.HitCount);
      Exit;
    end;

    // Newly detected functions are registered here (including their range,
    // which will come in handy when debugging) :
    Detected := DetectedFunctions.New(Result);
    Detected.CodeStart := TCodePointer(aAddress);
    Detected.CodeEnd := TCodePointer(IntPtr(aAddress) + Result.TotalLength);
    Detected.HitCount := 1;

    DbgPrintf('DxbxHLE : 0x%.8x -> %s', [aAddress, Result.Name]);
  end;

  procedure _ScanMemoryRangeForLibraryPatterns(const ByteScanLower, ByteScanUpper: PByte;
    const aXboxLibraryInfo: PXboxLibraryInfo);
  var
    p: PByte;
  begin
    p := ByteScanLower;
    while p <> ByteScanUpper do
    begin
      _FindAndRememberPattern(aXboxLibraryInfo, p);
      Inc(p);
    end;
  end;

var
  ByteScanLower, ByteScanUpper: PByte;
  i, j, PrevCount: Integer;
  CurrentXbeLibraryVersion: PXBE_LIBRARYVERSION;
  CurrentLibName: string;
  CurrentXboxLibraryInfo: PXboxLibraryInfo;
begin
  ByteScanLower := PByte(pXbeHeader.dwBaseAddr);
  ByteScanUpper := PByte(IntPtr(ByteScanLower) + pXbeHeader.dwSizeofImage);

  DetectedFunctions.Clear;

  // Sort all pattern arrays, to enable binary searching :
  for i := 0 to Length(AllXboxLibraries) - 1 do
    AllXboxLibraries[i].SortedPatterns := _SortPatternArray(AllXboxLibraries[i].PatternArray, AllXboxLibraries[i].PatternLength, );

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
      CurrentXboxLibraryInfo := @(AllXboxLibraries[j]);
      // Take LibVersion and LibName into account (don't scan outside active lib+version) :
      if  (CurrentXboxLibraryInfo.LibVersion = CurrentXbeLibraryVersion.wBuildVersion)
      and (StrLIComp(PAnsiChar(CurrentXboxLibraryInfo.LibName), PAnsiChar(CurrentLibName), 8) = 0) then
      begin
        // Once found, scan the memory for functions from this library :
        _ScanMemoryRangeForLibraryPatterns(ByteScanLower, ByteScanUpper, CurrentXboxLibraryInfo);
        Break;
      end;
    end;

    DbgPrintf('DxbxHLE : Detected %d APIs from "%s"', [DetectedFunctions.Count - PrevCount, CurrentLibName]);
    PrevCount := DetectedFunctions.Count;

    // Skip to the next library :
    Inc(CurrentXbeLibraryVersion);
  end;

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

function TDetectedFunctions.GetItem(Index: Integer): PDetectedXboxLibraryFunction;
begin
  Result := PDetectedXboxLibraryFunction(FList[Index]);
end;

procedure TDetectedFunctions.Clear;
var
  i: Integer;
  DetectedFunction: PDetectedXboxLibraryFunction;
begin
  for i := 0 to FList.Count - 1 do
  begin
    DetectedFunction := Items[i];
    Dispose(DetectedFunction);
  end;

  FList.Clear;
end;

function TDetectedFunctions.New(const aInfo: PXboxLibraryFunction): PDetectedXboxLibraryFunction;
begin
  Result := AllocMem(SizeOf(RDetectedXboxLibraryFunction));
  ZeroMemory(Result, SizeOf(RDetectedXboxLibraryFunction));
  Result.Info := aInfo;
  FList.Add(Result);
end;

function TDetectedFunctions.FindByName(const aName: string): PDetectedXboxLibraryFunction;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Result := Items[i];
    if SameText(Result.Info.Name, aName) then
      Exit;
  end;

  Result := nil;
end;

function TDetectedFunctions.FindByAddress(const aAddress: TCodePointer): PDetectedXboxLibraryFunction;
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
