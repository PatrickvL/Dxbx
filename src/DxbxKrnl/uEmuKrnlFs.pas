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
unit uEmuKrnlFs;

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
  // Jedi
  JwaWinType,
  JwaWinBase,
  JwaWinNT,
  JwaNative,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uLog,
  uEmuFS,
  uEmuFile,
  uEmuXapi,
  uEmuKrnl,
  uDxbxKrnl;

function xboxkrnl_FscGetCacheSize(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_FscInvalidateIdleBlocks(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_FscSetCacheSize(uCachePages: ULONG): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

function xboxkrnl_FscGetCacheSize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('FscGetCacheSize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_FscInvalidateIdleBlocks(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('FscInvalidateIdleBlocks');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_FscSetCacheSize(uCachePages: ULONG): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('FscSetCacheSize');
  EmuSwapFS(); // Xbox FS
end;

end.

