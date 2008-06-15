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
unit uEmuKrnlXc;

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

procedure xboxkrnl_XcSHAInit(
  pbSHAContext: PUCHAR
  ); stdcall;
procedure xboxkrnl_XcSHAUpdate(
  pbSHAContext: PUCHAR;
  pbInput: PUCHAR;
  dwInputLength: ULONG
  ); stdcall;
procedure xboxkrnl_XcSHAFinal(
  pbSHAContext: PUCHAR;
  pbDigest: PUCHAR
  ); stdcall;
function xboxkrnl_XcRC4Key(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcRC4Crypt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcHMAC(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcPKEncPublic(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcPKDecPrivate(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcPKGetKeyLen(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcVerifyPKCS1Signature(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcModExp(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcDESKeyParity(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcKeyTable(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcBlockCrypt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcBlockCryptCBC(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcCryptService(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_XcUpdateCrypto(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

procedure xboxkrnl_XcSHAInit(
  pbSHAContext: PUCHAR
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('XcSHAInit');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_XcSHAUpdate(
  pbSHAContext: PUCHAR;
  pbInput: PUCHAR;
  dwInputLength: ULONG
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('XcSHAUpdate');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_XcSHAFinal(
  pbSHAContext: PUCHAR;
  pbDigest: PUCHAR
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('XcSHAFinal');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcRC4Key(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcRC4Key');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcRC4Crypt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcRC4Crypt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcHMAC(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcHMAC');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcPKEncPublic(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcPKEncPublic');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcPKDecPrivate(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcPKDecPrivate');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcPKGetKeyLen(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcPKGetKeyLen');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcVerifyPKCS1Signature(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcVerifyPKCS1Signature');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcModExp(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcModExp');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcDESKeyParity(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcDESKeyParity');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcKeyTable(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcKeyTable');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcBlockCrypt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcBlockCrypt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcBlockCryptCBC(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcBlockCryptCBC');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcCryptService(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcCryptService');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcUpdateCrypto(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcUpdateCrypto');
  EmuSwapFS(); // Xbox FS
end;

end.

