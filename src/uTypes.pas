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

type
  TDebugInfoType = (ditConsole, ditFile);
  EnumAutoConvert = (CONVERT_TO_MANUAL, CONVERT_TO_XBEPATH, CONVERT_TO_WINDOWSTEMP);
  DebugMode = (DM_NONE, DM_CONSOLE, DM_FILE);

  TLogType = (ltKernel, ltGui);

  TVarByteArray = array of Byte;

implementation

end.
