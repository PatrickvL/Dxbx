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

{$INCLUDE Dxbx.inc}

interface

function CalcCRC16(aBuffer: PByte; aLength: Integer): Word;

implementation

var
  crc_tab16: array [0..255] of Word;

procedure UpdateCRC16(var CRC: WORD; const aByte: Byte);
begin
  CRC := (CRC shr 8) xor crc_tab16[Lo(CRC) xor aByte];
end;

function CalcCRC16(aBuffer: PByte; aLength: Integer): Word;
begin
  Result := 0;
  while aLength > 0 do
  begin
    UpdateCRC16({var}Result, aBuffer^);
    Inc(aBuffer);
    Dec(aLength);
  end;
end;

// The function init_crc16_tab() is used  to  fill  the  array
// for calculation of the CRC-16 with values.
procedure init_crc16_tab;
const
  P_16 = Word($A001); // x^16, x^14, x^1
var
  i, j: Integer;
  crc, c: Word;
begin
  for i := 0 to 255 do
  begin
    crc := 0;
    c   := Word(i);

    for j := 0 to 7 do
    begin
      if ((crc xor c) and 1) > 0 then
        crc := ( crc shr 1 ) xor P_16
      else
        crc :=   crc shr 1;

      c := c shr 1;
    end;

    crc_tab16[i] := crc;
  end;
end;

{$IFDEF DUNIT}
const
  TestCRC = $4521;
  TestInput = 'ABC';
{$ENDIF}

initialization

  init_crc16_tab;

{$IFDEF DUNIT}
  Assert(TestCRC = CalcCRC16(Pointer(PAnsiChar(TestInput)), 3));
{$ENDIF}

end.

