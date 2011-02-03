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

function XTL_EmuXGIsSwizzledFormat
(
    Format: X_D3DFORMAT
): _bool; stdcall; // Dxbx addition : This check should return a boolean
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXapi : EmuXGIsSwizzledFormat').
      _(Format, 'Format').
    LogEnd();

  Result := False;

  EmuSwapFS(fsXbox);
end;

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
var
  dwMaxY: DWORD;
  dwChunkSize: DWORD;
  pSrc: Puint08;
  pDst: Puint08;
  y: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXapi : EmuXGSwizzleRect').
      _(pSource, 'pSource').
      _(Pitch, 'Pitch').
      _(pRect, 'pRect').
      _(pDest, 'pDest').
      _(Width, 'Width').
      _(Height, 'Height').
      _(pPoint, 'pPoint').
      _(BytesPerPixel, 'BytesPerPixel').
    LogEnd();

  if (pRect = NULL) and (pPoint = NULL) and (Pitch = 0) then
  begin
    memcpy(pDest, pSource, Width * Height * BytesPerPixel);
  end
  else
  begin
    if Assigned(pPoint) and ((pPoint.x <> 0) or (pPoint.y <> 0)) then
      DxbxKrnlCleanup('Temporarily unsupported swizzle (very easy fix)');

    dwMaxY := Height;
    dwChunkSize := Width;

    pSrc := Puint08(pSource);
    pDst := Puint08(pDest);

    if Assigned(pRect) then
    begin
      Inc(pSrc, DWORD(pRect.top)*Pitch);
      Inc(pSrc, pRect.left);

      dwMaxY := pRect.bottom - pRect.top;
      dwChunkSize := pRect.right - pRect.left;
    end;

    if dwMaxY > 0 then // Dxbx addition, to prevent underflow
    for y := 0 to dwMaxY - 1 do
    begin
      memcpy(pSrc, pDst, dwChunkSize);
      Inc(pSrc, Pitch);
      Inc(pDst, Pitch);
    end;
  end;

  EmuSwapFS(fsXbox);
end;

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
var
  dwMaxY: DWORD;
  // dwMaxZ: DWORD;
  dwChunkSize: DWORD;
  pSrc: Puint08;
  pDst: Puint08;
  y: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXapi : EmuXGSwizzleBox').
      _(pSource, 'pSource').
      _(RowPitch, 'RowPitch').
      _(SlicePitch, 'SlicePitch').
      _(pBox, 'pBox').
      _(pDest, 'pDest').
      _(Width, 'Width').
      _(Height, 'Height').
      _(Depth, 'Depth').
      _(pPoint, 'pPoint').
      _(BytesPerPixel, 'BytesPerPixel').
    LogEnd();

  if (DWORD(pDest) < $80000000) then
  begin
    if (pBox = NULL) and (pPoint = NULL) and (RowPitch = 0) and (SlicePitch = 0) then
    begin
      memcpy(pDest, pSource, Width * Height * Depth * BytesPerPixel);
    end
    else
    begin
      if (pPoint <> NULL) and ((pPoint.u <> 0) or (pPoint.u <> 0) or (pPoint.w <> 0)) then
        DxbxKrnlCleanup('Temporarily unsupported swizzle (easy fix)');

      dwMaxY := Height;
      // dwMaxZ := Depth;
      dwChunkSize := Width;

      pSrc := Puint08(pSource);
      pDst := Puint08(pDest);

      if (pBox <> nil) then
      begin
        Inc(pSrc, pBox.Top*RowPitch);
        Inc(pSrc, pBox.Left);

        dwMaxY := pBox.Bottom - pBox.Top;
        dwChunkSize := pBox.Right - pBox.Left;
      end;

      if dwMaxY > 0 then // Dxbx addition, to prevent underflow
      for y := 0 to dwMaxY - 1 do
      begin
        memcpy(pSrc, pDst, dwChunkSize);
        Inc(pSrc, RowPitch);
        Inc(pDst, RowPitch);
      end;
    end;
  end;

  EmuSwapFS(fsXbox);
end;

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
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXapi : EmuXGUnswizzleRect').
      _(pSrcBuff, 'pSrcBuff').
      _(dwWidth, 'dwWidth').
      _(dwHeight, 'dwHeight').
      _(dwDepth, 'dwDepth').
      _(pDstBuff, 'pDstBuff').
      _(dwPitch, 'dwPitch').
      _(Pointer(@rSrc), 'rSrc').
      _(Pointer(@poDst), 'poDst').
      _(dwBPP, 'dwBPP').
    LogEnd();

  EmuUnswizzleRect(
    pSrcBuff,
    dwWidth,
    dwHeight,
    dwDepth,
    pDstBuff,
    dwPitch,
    rSrc,
    poDst,
    dwBPP);

  EmuSwapFS(fsXbox);
end;

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
  XTL_EmuXGIsSwizzledFormat, // Among others, used by Rayman
  XTL_EmuXGSwizzleBox, // Among others, used by Turok
  XTL_EmuXGSwizzleRect, // Among others, used by Rayman
  XTL_EmuXGUnswizzleRect, // Among others, used by Rayman
  XTL_EmuXGWriteSurfaceOrTextureToXPR; // Among others, used by Gamepad

end.
