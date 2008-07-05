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

{$INCLUDE ..\Dxbx.inc}

interface

{$R 'DxbxKrnl\PatternData.res' 'DxbxKrnl\PatternData.rc'}

uses
  // Delphi
  Classes,
  // Dxbx
  uXBE,
  uLog,
  uBitUtils,
  uPatternScanner,
  uCRC16;

procedure DxbxScanForLibraryAPIs(const pXbeHeader: PXBE_HEADER);

var
  NrOfAPIsDetected: Integer;

implementation

procedure DxbxScanForLibraryAPIs(const pXbeHeader: PXBE_HEADER);

  function _ComparePattern(const Pattern: PPattern; const aAddress: PByte): Integer;
  var
    b: Integer;
  begin
    // Loop over the first 32 pattern-bytes :
    Result := 0;
    for b := 0 to PATTERNSIZE - 1 do
    begin
      // Test if this byte offset must be checked :
      if TestBit(Pattern.PatternBytesToUseMask, b) then
      begin
        Result := Pattern.PatternBytes[b] - PByte(Integer(aAddress) + b)^;
        if Result <> 0 then
          Exit;
      end;
    end;

    if Pattern.CRCLength > 0 then
      Result := (Pattern.CRCValue - CalcCRC16(Pointer(Integer(aAddress) + PATTERNSIZE), Pattern.CRCLength));

    // TODO : Test all referenced APIs

    // TODO : Test trailing bytes too
  end;

  // Modified copy of the binary search code in TStringList.Find() :
  function _FindPattern(const aPatternLibrary: TList; const aAddress: PByte; var Index: Integer): Boolean;
  var
    L, H, I, C: Integer;
  begin
    Result := False;
    L := 0;
    H := aPatternLibrary.Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := _ComparePattern(PPattern(aPatternLibrary.List^[I]), aAddress);
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
//          if Duplicates <> dupAccept then
            L := I;
        end;
      end;
    end;

    Index := L;
  end;

  procedure _FindAndRememberPattern(const aPatternLibrary: TList; const aAddress: PByte);
  var
    Index: Integer;
    Pattern: PPattern;
  begin
    // Search if this address matches a pattern :
    if not _FindPattern(aPatternLibrary, aAddress, {var}Index) then
      Exit;

    Pattern := PPattern(aPatternLibrary.List^[Index]);

if Pattern.CRCLength > 0 then // DEBUG : Only print when CRC was used
    // TODO : We found a library-function! Administrate that somewhere...
    DbgPrintf('DxbxHLE: 0x%.8x -> %s', [aAddress, Pattern.Name]);
    Inc(NrOfAPIsDetected);
  end;

var
  PatternsXAPI5788: TList;
  PatternsKrnl0000: TList;
  ByteScanLower, ByteScanUpper, p: PByte;
  Pattern: PPattern;
begin
  PatternsXAPI5788 := GetSortedPatternList('5788xapilib');
  PatternsKrnl0000 := GetSortedPatternList('0000xbox');
  // TODO : What about other libraries and different versions ?

  ByteScanLower := PByte(pXbeHeader.dwBaseAddr);
  ByteScanUpper := PByte(pXbeHeader.dwBaseAddr + pXbeHeader.dwSizeofImage);

  NrOfAPIsDetected := 0;
  p := ByteScanLower;
  while p <> ByteScanUpper do
  begin
    _FindAndRememberPattern(PatternsXAPI5788, p);
    _FindAndRememberPattern(PatternsKrnl0000, p);
    Inc(p);
  end;
end;

end.

