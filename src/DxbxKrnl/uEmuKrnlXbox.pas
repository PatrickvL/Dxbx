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

var
  xXboxKrnlVersion: ULONGLONG; // Xbox Kernel version (pointed to by XboxKrnlVersion)

var
  {321}xboxkrnl_XboxEEPROMKey: array [0..16 - 1] of UCHAR; // Source OpenXDK
  {322}xboxkrnl_XboxHardwareInfo: XBOX_HARDWARE_INFO; // Source OpenXDK
  {323}xboxkrnl_XboxHDKey: array [0..16 - 1] of UCHAR; // Source OpenXDK
  {324}xboxkrnl_XboxKrnlVersion: PULONGLONG = @xXboxKrnlVersion; // Kernel version to simulate
  // Source:nkpatcher  Branch:Dxbx  Translator:PatrickvL  Done:100

  {325}xboxkrnl_XboxSignatureKey: array [0..16 - 1] of Byte; // Source OpenXDK
  {353}xboxkrnl_XboxLANKey: DWord;
  {354}xboxkrnl_XboxAlternateSignatureKeys: DWord;

function MAKEDLLVERULL(
    wMajorVersion: WORD;
    wMinorVersion: WORD;
    wBuild: WORD;
    wQFE: WORD
): ULONGLONG;

procedure SetKernelVersion(const aBuild: Word);
function GetKernelVersion: Word;

implementation

function MAKEDLLVERULL(
    wMajorVersion: WORD;
    wMinorVersion: WORD;
    wBuild: WORD;
    wQFE: WORD
): ULONGLONG;
// Branch:Dxbx  Translator:PatrickvL  Done:100
// See http://msdn.microsoft.com/en-us/library/bb774312(VS.85).aspx
begin
  Result := (ULONGLONG(wMajorVersion) shl 48) or
            (ULONGLONG(wMinorVersion) shl 32) or
            (ULONGLONG(       wBuild) shl 16) or
            (ULONGLONG(         wQFE) shl  0);
end;

procedure SetKernelVersion(const aBuild: Word);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  // TODO : Are Major & Minor correct, or must they be swapped ?
  xXboxKrnlVersion := MAKEDLLVERULL(
    {wMajorVersion=}1,
    {wMinorVersion=}0,
    {wBuild=}aBuild,
    {wQFE=}1
    );
end;

function GetKernelVersion: Word;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := Word(xXboxKrnlVersion shr 16);
end;

initialization

  // Default Dxbx to emulating kernel version 5838 :
  SetKernelVersion(5838);
  
end.
