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
  uPatternScanner;

procedure DxbxScanForLibraryAPIs(const pXbeHeader: PXBE_HEADER);

var
  NrOfAPIsDetected: Integer;

implementation

procedure DxbxScanForLibraryAPIs(const pXbeHeader: PXBE_HEADER);

  procedure _FindPattern(const aPatternLibrary: TList; const aAddress: PByte);
  var
    i: Integer;
    Pattern: PPattern;
    b: Integer;
    Found: Boolean;
  begin
    // TODO : Once the patterns are sorted (or otherwise ordened),
    // change this detection-code too!

    // For now, NrOfAPIsDetected each pattern in the library :
    for i := 0 to aPatternLibrary.Count - 1 do
    begin
      Pattern := aPatternLibrary[i];

      // Loop over the first 32 pattern-bytes :
      Found := True;
      for b := 0 to PATTERNSIZE - 1 do
      begin
        // NrOfAPIsDetected if this byte offset must be checked :
        if TestBit(Pattern.PatternBytesToUseMask, b) then
        begin
          if Pattern.PatternBytes[b] <> PByte(Integer(aAddress) + b)^ then
          begin
            Found := False;
            Break;
          end;
        end;
      end;

      if Found then
      begin
        // TODO : We found a library-function! Administrate that somewhere...
        DbgPrintf('DxbxHLE: 0x%.8x -> %s', [aAddress, Pattern.Name]);
        Inc(NrOfAPIsDetected);
        Exit;
      end;

    end;
  end;

var
  PatternsXAPI5788: TList;
  ByteScanLower, ByteScanUpper, p: PByte;
  Pattern: PPattern;
begin
  PatternsXAPI5788 := GetSortedPatternList('5788xapilib');

  ByteScanLower := PByte(pXbeHeader.dwBaseAddr);
  ByteScanUpper := PByte(pXbeHeader.dwBaseAddr + pXbeHeader.dwSizeofImage);
  NrOfAPIsDetected := 0;
  p := ByteScanLower;
  while p <> ByteScanUpper do
  begin
    _FindPattern(PatternsXAPI5788, p);
    Inc(p);
  end;
end;

end.

