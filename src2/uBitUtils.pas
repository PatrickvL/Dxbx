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
unit uBitUtils;

{$INCLUDE Dxbx.inc}

interface

procedure ClearBit(var aValue; const aIndex: Integer);
procedure SetBit(var aValue; const aIndex: Integer);
function TestBit(const aValue; const aIndex: Integer): Boolean;
function TestAndSetBit(const aValue; const aIndex: Integer): Boolean;

implementation

procedure ClearBit(var aValue; const aIndex: Integer);
asm
  BTR     [EAX], aIndex
end;

procedure SetBit(var aValue; const aIndex: Integer);
asm
  BTS     [EAX], aIndex
end;

// Test bit 'aIndex' within 'aValue', returns True if bit is set, False otherwise. (Based on TBitSet.GetBit).
function TestBit(const aValue; const aIndex: Integer): Boolean;
asm
  BT      [EAX], aIndex
  SBB     EAX, EAX
  AND     EAX, 1
end;

function TestAndSetBit(const aValue; const aIndex: Integer): Boolean;
asm
  BTS     [EAX], aIndex
  SBB     EAX, EAX
  AND     EAX, 1
end;

end.

