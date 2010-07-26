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
  uEmu,
  uEmuKrnl,
  uDxbxKrnl;

function {035} xboxkrnl_FscGetCacheSize(
  ): SIZE_T; stdcall;
procedure {036} xboxkrnl_FscInvalidateIdleBlocks(); stdcall;
function {037} xboxkrnl_FscSetCacheSize(
  uCachePages: ULONG
  ): NTSTATUS; stdcall;

var
  FscNumberOfCachePages: SIZE_T = 1; // Default, 1 page of cache

implementation

function {035} xboxkrnl_FscGetCacheSize(
  ): SIZE_T; stdcall;
// Source:Dxbx  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuKrnl : FscGetCacheSize();');
  Result := FscNumberOfCachePages;
  EmuSwapFS(fsXbox);
end;

procedure {036} xboxkrnl_FscInvalidateIdleBlocks(); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('FscInvalidateIdleBlocks');
  EmuSwapFS(fsXbox);
end;

function {037} xboxkrnl_FscSetCacheSize(
  uCachePages: ULONG
  ): NTSTATUS; stdcall;
// Source:Cxbx  Branch:shogun  Revision:0.8.2-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : FscSetCacheSize' +
      #13#10'(' +
      #13#10'   uCachePages         : 0x%.08X' +
      #13#10');',
      [uCachePages]);
{$ENDIF}

  // TODO -oDxbx : Actually make this have some effect somehow?

  FscNumberOfCachePages := uCachePages;

  EmuSwapFS(fsXbox);
  
  Result := STATUS_SUCCESS;
end;

end.
