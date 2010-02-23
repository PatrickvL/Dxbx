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
unit uEmuKrnlXe;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  JwaWinBase,
  JwaWinNT,
  JwaNative,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uLog,
  uXbe,
  uEmuFS,
  uEmuFile,
  uEmuXapi,
  uEmuKrnl,
  uDxbxKrnl;

var {355}xboxkrnl_XePublicKeyData: DWord;
// Source:?  Branch:Dxbx  Translator:PatrickvL  Done:100

var {326}xboxkrnl_XeImageFileName: PANSI_STRING;
// Source:Xbox-Linux  Branch:Dxbx  Translator:PatrickvL  Done:100
//
// XeImageFileName.Buffer points to path of XBE
//
// Format is like this: \Device\Harddisk0\Partition1\bla.xbe
// Size of XeImageFileName.Buffer is stored in XeImageFileName.Length

function {327} xboxkrnl_XeLoadSection(
  Section: PXBE_SECTIONHEADER // In, out
  ): NTSTATUS; stdcall;
function {328} xboxkrnl_XeUnloadSection(
  Section: PXBE_SECTIONHEADER // In, out
  ): NTSTATUS; stdcall;

implementation
  
// XeLoadSection:
// Adds one to the reference count of the specified section and loads if the
// count is now above zero.
//
// New to the XBOX.
function {327} xboxkrnl_XeLoadSection(
  Section: PXBE_SECTIONHEADER // In, out
  ): NTSTATUS; stdcall;
// Source:XBMC  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XeLoadSection');
  EmuSwapFS(fsXbox);
end;

// XeUnloadSection:
// Subtracts one from the reference count of the specified section and loads
// if the count is now below zero.
//
// New to the XBOX.
function {328} xboxkrnl_XeUnloadSection(
  Section: PXBE_SECTIONHEADER // In, out
  ): NTSTATUS; stdcall;
// Source:XBMC  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XeUnloadSection');
  EmuSwapFS(fsXbox);
end;

end.
