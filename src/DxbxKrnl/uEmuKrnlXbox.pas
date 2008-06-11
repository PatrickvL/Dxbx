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
unit uEmuKrnlXbox;

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

function xboxkrnl_XboxEEPROMKey(): NTSTATUS; stdcall;
function xboxkrnl_XboxHardwareInfo(): NTSTATUS; stdcall;
function xboxkrnl_XboxHDKey(): NTSTATUS; stdcall;
function xboxkrnl_XboxKrnlVersion(): NTSTATUS; stdcall;
function xboxkrnl_XboxSignatureKey(): NTSTATUS; stdcall;
function xboxkrnl_XboxLANKey(): NTSTATUS; stdcall;
function xboxkrnl_XboxAlternateSignatureKeys(): NTSTATUS; stdcall;

implementation

function xboxkrnl_XboxEEPROMKey(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxEEPROMKey');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxHardwareInfo(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxHardwareInfo');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxHDKey(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxHDKey');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxKrnlVersion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxKrnlVersion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxSignatureKey(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxSignatureKey');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxLANKey(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxLANKey');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxAlternateSignatureKeys(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxAlternateSignatureKeys');
  EmuSwapFS(); // Xbox FS
end;

end.

