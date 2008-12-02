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
unit uTypes;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  SysUtils;

type
  TVarByteArray = array of Byte;

  TDWordArray = array[0..10000] of DWord;
  PDWordArray = ^TDWordArray;

  PDWord = ^DWord;

  UInt32 = Cardinal;
  UInt16 = Word;
  UInt8 = Byte;

  PUInt32 = ^UInt32;
  PUInt16 = ^UInt16;
  PUInt8 = PAnsiChar; //^UInt8;

  UInt08 = UInt8;
  PUInt08 = PUInt8;

{$IF DECLARED(UINT_PTR)}
  UIntPtr = UINT_PTR;
{$ELSE}
  UIntPtr = UInt32;
{$IFEND}

{$IF DECLARED(INT_PTR)}
  IntPtr = INT_PTR;
{$ELSE}
  IntPtr = Int32;
{$IFEND}

  TCodePointer = type Pointer;

implementation

end.
