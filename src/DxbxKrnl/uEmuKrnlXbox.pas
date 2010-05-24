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

type
  X_DllVersionInfo = packed record
    MajorVersion: Word;
    MinorVersion: Word;
    BuildNumber: Word;
    QFE: Word; // This might well be a flags member, just like in _XBE_LIBRARYVERSION
  end; // packed size = 8
  PX_DllVersionInfo = ^X_DllVersionInfo;

var
  {321}xboxkrnl_XboxEEPROMKey: array [0..16 - 1] of UCHAR; // Source OpenXDK
  {322}xboxkrnl_XboxHardwareInfo: XBOX_HARDWARE_INFO; // Source OpenXDK
  {323}xboxkrnl_XboxHDKey: array [0..16 - 1] of UCHAR; // Source OpenXDK
  {324}xboxkrnl_XboxKrnlVersion: X_DllVersionInfo; // Xbox Kernel version (part of kernel thunk table)
  // Source:nkpatcher  Branch:Dxbx  Translator:PatrickvL  Done:100

  {325}xboxkrnl_XboxSignatureKey: array [0..16 - 1] of Byte; // Source OpenXDK
  {353}xboxkrnl_XboxLANKey: DWord;
  {354}xboxkrnl_XboxAlternateSignatureKeys: DWord;

procedure SetKernelVersion(const aBuild: Word);
function GetKernelVersion: Word;

implementation

procedure SetKernelVersion(const aBuild: Word);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  xboxkrnl_XboxKrnlVersion.MajorVersion := 1;
  xboxkrnl_XboxKrnlVersion.MinorVersion := 0;
  xboxkrnl_XboxKrnlVersion.BuildNumber := aBuild;
  xboxkrnl_XboxKrnlVersion.QFE := 1;
end;

function GetKernelVersion: Word;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := xboxkrnl_XboxKrnlVersion.BuildNumber;
end;

initialization

  // Default Dxbx to emulating kernel version 5838 :
  SetKernelVersion(5838);
  
end.
