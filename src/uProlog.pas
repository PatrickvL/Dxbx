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
unit uProlog;

{$INCLUDE Dxbx.inc}

interface

var

  // ******************************************************************
  // *
  // * WARNING:
  // *
  // * This is hard-coded as an array to stress the point that
  // * it can not be modified (unless you really know what you're
  // * doing). EmuExe will modify the contents of this function
  // * directly, so this precise structure is necessary.
  // *
  // ******************************************************************
  Prolog: array[0..255] of Byte = (
    $BE, $C3, $C3, $C3, $C3, // mov esi, 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $FF, $D6,                // call esi
    $C3, $CC, $CC, $CC, $CC, // ret
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC);


  EndFilling: array[0..524] of Byte = (
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD);

implementation

end.
