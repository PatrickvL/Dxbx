(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2007 PatrickvL and other members of the development team.

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

unit uEmuXG;

{$INCLUDE Dxbx.inc}

interface

uses
  // Jedi Win32API
  JwaWinType,
  // DirectX
{$IFDEF DXBX_USE_D3D9}
  Direct3D9,
{$ELSE}
  Direct3D8, // D3DFORMAT
{$ENDIF}
  // Dxbx
  uTypes,
  uLog,
  uDxbxKrnlUtils,
  uEmuD3D8Types,
  uEmuFS,
  uEmu,
  uXboxLibraryUtils, // PatchPrefix
  uConvert;

// From EmuXG.h :

// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
type _XGPOINT3D = record
    u: DWORD;
    v: DWORD;
    w: DWORD;
end; // size = 12
XGPOINT3D = _XGPOINT3D;
PXGPOINT3D = ^XGPOINT3D;

implementation

const lfUnit = lfCxbx or lfXapi;

// From EmuXG.cpp :

(*
function XTL_EmuXGIsSwizzledFormat
(
    Format: X_D3DFORMAT
): _bool; stdcall; // Dxbx addition : This check should return a boolean
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
*)

(*
procedure XTL_EmuXGSwizzleRect
(
    pSource: LPCVOID;
    Pitch: DWORD;
    pRect: LPCRECT;
    pDest: LPVOID;
    Width: DWORD;
    Height: DWORD;
    {CONST} pPoint: LPPOINT;
    BytesPerPixel: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
*)

(*
procedure XTL_EmuXGSwizzleBox
(
    pSource: LPCVOID;
    RowPitch: DWORD;
    SlicePitch: DWORD;
    {CONST} pBox: PD3DBOX;
    pDest: LPVOID;
    Width: DWORD;
    Height: DWORD;
    Depth: DWORD;
    {CONST} pPoint: PXGPOINT3D;
    BytesPerPixel: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
*)

(*
procedure XTL_EmuXGUnswizzleRect
(
    pSrcBuff: PVOID;
    dwWidth: DWORD;
    dwHeight: DWORD;
    dwDepth: DWORD;
    pDstBuff: PVOID;
    dwPitch: DWORD;
    rSrc: TRECT;
    poDst: TPOINT;
    dwBPP: DWORD
); stdcall;
*)

function XTL_EmuXGWriteSurfaceOrTextureToXPR
(
  pResource: LPVOID;
  {const} cPath: P_char;
  bWriteSurfaceAsTexture: BOOL
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXapi : EmuXGWriteSurfaceOrTextureToXPR').
      _(pResource, 'pResource').
      _(cPath, 'cPath').
      _(bWriteSurfaceAsTexture, 'bWriteSurfaceAsTexture').
    LogEnd();

  // TODO -oCXBX: If necessary, either reverse the .xbx and .xpr file formats
  // and write the surface/texture to a file, or output a generic .xbx
  // file and be done with it.

  EmuWarning('(Temporarily) ignoring EmuXGWriteSurfaceOrTextureToXPR. Need file specs.');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

exports
//  XTL_EmuXGIsSwizzledFormat, // Among others, used by Rayman
//  XTL_EmuXGSwizzleBox, // Among others, used by Turok
//  XTL_EmuXGSwizzleRect, // Among others, used by Rayman
//  XTL_EmuXGUnswizzleRect, // Among others, used by Rayman
  XTL_EmuXGWriteSurfaceOrTextureToXPR; // Among others, used by Gamepad

end.
