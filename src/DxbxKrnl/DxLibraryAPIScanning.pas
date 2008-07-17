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
  Classes,
  SysUtils,
  Contnrs,
  // Dxbx
  uTypes,
  uXBE,
  uLog,
  uBitUtils,
  uPatterns,
  uXboxLibraryUtils,
  uCRC16;

procedure DxbxScanForLibraryAPIs(const pXbeHeader: PXBE_HEADER);

var
  NrOfAPIsDetected: Integer;

implementation

{.$DEFINE BRUTE_FORCE}

procedure DxbxScanForLibraryAPIs(const pXbeHeader: PXBE_HEADER);

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

    // TODO : Test all referenced APIs

    // TODO : Test trailing bytes too
  end;

{$IFDEF BRUTE_FORCE}
  function _FindPattern(const aPatternLibrary: TPatternArray; const aCount: Integer; const aAddress: PByte; var Index: Integer): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to aCount - 1 do
    begin
      Result := _ComparePattern(RXboxLibraryFunction(@(aPatternLibrary[i])), aAddress) = 0;
      if Result then
      begin
        {var}Index := i;
        Exit;
      end;
    end;

    Result := False;
  end;
{$ELSE}
  // Modified copy of the binary search code in TStringList.Find() :
  function _FindPattern(const aPatternLibrary: TPatternArray; const aCount: Integer; const aAddress: PByte; var Index: Integer): Boolean;
  var
    L, H, I, C: Integer;
  begin
    Result := False;
    L := 0;
    H := aCount - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := _ComparePattern(PXboxLibraryFunction(@(aPatternLibrary[I])), aAddress);
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
{$ENDIF}

  procedure _FindAndRememberPattern(const aXboxLibraryInfo: RXboxLibraryInfo; const aAddress: PByte);
  var
    Index: Integer;
    Pattern: PXboxLibraryFunction;
  begin
    // Search if this address matches a pattern :
    if not _FindPattern(TPatternArray(aXboxLibraryInfo.PatternArray), aXboxLibraryInfo.NrPatterns, aAddress, {var}Index) then
      Exit;

    Pattern := PXboxLibraryFunction(@(TPatternArray(aXboxLibraryInfo.PatternArray)[Index]));

Inc(Pattern.HitCount); if Pattern.HitCount > 1 then Exit;

    // TODO : We found a library-function! Administrate that somewhere...
    DbgPrintf('DxbxHLE: 0x%.8x -> %s', [aAddress, Pattern.Name]);
    Inc(NrOfAPIsDetected);
  end;

var
  ByteScanLower, ByteScanUpper, p: PByte;
  i: Integer;
begin
  ByteScanLower := PByte(pXbeHeader.dwBaseAddr);
  ByteScanUpper := PByte(IntPtr(ByteScanLower) + pXbeHeader.dwSizeofImage);

  NrOfAPIsDetected := 0;
  p := ByteScanLower;
  while p <> ByteScanUpper do
  begin
    for i := 0 to Length(AllPatternArrays) - 1 do
      _FindAndRememberPattern(AllPatternArrays[i], p);

    Inc(p);
  end;

  DbgPrintf('DxbxHLE: %d APIs detected', [NrOfAPIsDetected]);
end;

end.
