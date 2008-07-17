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

var
  CRC_CCITT_TABLE: array [Byte] of Word;

procedure UpdateCRC16(var CRC: WORD; const aByte: Byte); inline;
begin
  CRC := (CRC shr 8) xor CRC_CCITT_TABLE[Lo(CRC xor aByte)];
end;

// Function:     crc16
// Description:
//   crc16 is ripped straight out the c file that comes with the
//   FLAIR package
//                                        16   12   5
//   this is the CCITT CRC 16 polynomial X  + X  + X  + 1.
//   This works out to be 0x1021, but the way the algorithm works
//   lets us use 0x8408 (the reverse of the bit pattern).  The high
//   bit is always assumed to be set, thus we only use 16 bits to
//   represent the 17 bit value.
// Parameters: uchar *pdata - pointer to data
//             uint16 len - data length
// Returns:    CRC
// Optimize by TQN, run faster about 12 times
function CalcCRC16(aBuffer: PByte; aLength: Integer): Word;
var
  crc: Word;
begin
  Result := 0;
  if aLength <= 0 then
    Exit;

  crc := $FFFF;
  repeat
    UpdateCRC16({var}crc, aBuffer^);
    Inc(aBuffer);
    Dec(aLength);
  until aLength = 0;

  Result := Swap(not crc);
end;

// Init the table lookup for CRC_CCITT 16 calculation
procedure init_crc16_tab;
var
  i, j: Integer;
  crc: Word;
begin
  for i := 0 to 255 do
  begin
    crc := Word(i);

    for j := 0 to 7 do
    begin
      if (crc and 1) > 0 then
        crc := ( crc shr 1 ) xor P_KERMIT
      else
        crc :=   crc shr 1;
    end;

    CRC_CCITT_TABLE[i] := crc;
  end;
end;

initialization

  init_crc16_tab;

end.
