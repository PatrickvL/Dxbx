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
unit uCRC16;

{$DEFINE _OPTIMIZE_UNIT}

{$INCLUDE Dxbx.inc}

interface

function CalcCRC16(aBuffer: PByte; aLength: Integer): Word;

implementation

const
  P_KERMIT = Word($8408);

// This code is fully classified as 'CRC-16/X-KERMIT' - a deprecated CRC...
// See http://homepages.tesco.net/~rainstorm/crc-catalogue.htm#crc.cat.kermit
function CalcCRC16(aBuffer: PByte; aLength: Integer): Word;
var
  j: Integer;
  crc, c: Word;
begin
  Result := 0;
  if aLength <= 0 then
    Exit;

  crc := $FFFF;
  repeat
    c := aBuffer^;
    Inc(aBuffer);

    for j := 0 to 7 do
    begin
      if ((crc xor c) and 1) > 0 then
        crc := ( crc shr 1 ) xor P_KERMIT
      else
        crc :=   crc shr 1;

      c := c shr 1;
    end;

    Dec(aLength);
  until aLength = 0;

  Result := Swap(not crc);
end;

end.

