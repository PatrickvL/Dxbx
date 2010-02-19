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
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('XcSHAInit');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcSHAUpdate(
  pbSHAContext: PUCHAR;
  pbInput: PUCHAR;
  dwInputLength: ULONG
  ); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('XcSHAUpdate');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcSHAFinal(
  pbSHAContext: PUCHAR;
  pbDigest: PUCHAR
  ); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('XcSHAFinal');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcRC4Key(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcRC4Key');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcRC4Crypt(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcRC4Crypt');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcHMAC(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcHMAC');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcPKEncPublic(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcPKEncPublic');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcPKDecPrivate(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcPKDecPrivate');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcPKGetKeyLen(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcPKGetKeyLen');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcVerifyPKCS1Signature(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcVerifyPKCS1Signature');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcModExp(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcModExp');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcDESKeyParity(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcDESKeyParity');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcKeyTable(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcKeyTable');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcBlockCrypt(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcBlockCrypt');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcBlockCryptCBC(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcBlockCryptCBC');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcCryptService(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcCryptService');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcUpdateCrypto(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('XcUpdateCrypto');
  EmuSwapFS(fsXbox);
end;

end.
