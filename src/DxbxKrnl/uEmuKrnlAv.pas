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
unit uEmuKrnlAv;

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

function xboxkrnl_AvGetSavedDataAddress(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_AvSendTVEncoderOption(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_AvSetDisplayMode(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_AvSetSavedDataAddress(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

function xboxkrnl_AvGetSavedDataAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('AvGetSavedDataAddress');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_AvSendTVEncoderOption(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('AvSendTVEncoderOption');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_AvSetDisplayMode(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('AvSetDisplayMode');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_AvSetSavedDataAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('AvSetSavedDataAddress');
  EmuSwapFS(); // Xbox FS
end;

end.

