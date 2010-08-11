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
  uEmuKrnl,
  uDxbxKrnl;

function {001} xboxkrnl_AvGetSavedDataAddress(
  ): PVOID; stdcall;
procedure {002} xboxkrnl_AvSendTVEncoderOption(
  RegisterBase: PVOID;
  Option: ULONG;
  Param: ULONG;
  Result: PULONG // OUT
  ); stdcall;
function {003} xboxkrnl_AvSetDisplayMode(
  RegisterBase: PVOID;
  Step: ULONG;
  Mode: ULONG;
  Format: ULONG;
  Pitch: ULONG;
  FrameBuffer: ULONG
  ): ULONG; stdcall;
function {004} xboxkrnl_AvSetSavedDataAddress(
  Address: PVOID
  ): PVOID; stdcall;

implementation

const lfUnit = lfCxbx or lfKernel;


var xboxkrnl_AvSavedDataAddress: PVOID = PVOID($F0040000);
// TODO -oDXBX: Take shogun's NULL ?

function {001} xboxkrnl_AvGetSavedDataAddress()
  : PVOID; stdcall;
// Source:OpenXDK  Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : AvGetSavedDataAddress();');
    EmuSwapFS(fsXbox);
  end;

  Result := xboxkrnl_AvSavedDataAddress; // Dxbx addition
end;

procedure {002} xboxkrnl_AvSendTVEncoderOption(
  RegisterBase: PVOID;
  Option: ULONG;
  Param: ULONG;
  Result: PULONG // OUT
  ); stdcall;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('AvSendTVEncoderOption');
  // "Run Like Hell" (5233) calls this from a routine at 0x11FCD0 - See XTL_EmuIDirect3DDevice8_Unknown1
  EmuSwapFS(fsXbox);
end;

function {003} xboxkrnl_AvSetDisplayMode(
  RegisterBase: PVOID;
  Step: ULONG;
  Mode: ULONG;
  Format: ULONG;
  Pitch: ULONG;
  FrameBuffer: ULONG
  ): ULONG; stdcall;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('AvSetDisplayMode');
  EmuSwapFS(fsXbox);
end;

function {004} xboxkrnl_AvSetSavedDataAddress(
  Address: PVOID
  ): PVOID; stdcall;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:50
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : AvSetSavedDataAddress' +
      #13#10'(' +
      #13#10'  Address          : 0x%.8x' +
      #13#10');', [
      Address
      ]);
    EmuSwapFS(fsXbox);
  end;

  Result := Address;
  xboxkrnl_AvSavedDataAddress := Result;
end;

end.
