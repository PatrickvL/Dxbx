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

unit uEmuXG;

{$INCLUDE Dxbx.inc}

interface

uses
  uTypes,
  JwaWinType,
  Direct3D8,
  uLog,
  uEmuFS;

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
) ; stdcall;
function XTL_EmuXGIsSwizzledFormat(Format : D3DFORMAT) : PVOID; stdcall;


implementation

function XTL_EmuXGIsSwizzledFormat(Format : D3DFORMAT) : PVOID; stdcall;
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS();   // Win2k/XP FS
  DbgPrintf('EmuXapi: EmuXGIsSwizzledFormat' +
         #13#10+'(' +
         #13#10+'   Format              : 0x%.08X' +
         #13#10+')',
         [Ord(Format)]);
  EmuSwapFS();   // Xbox FS
{$ENDIF}

  Result := nil;
end;

(*procedure XTL_EmuXGSwizzleRect
(
    pSource: LPCVOID;
    Pitch: DWORD;
    pRect: LPCRECT;
    pDest: LPVOID;
    Width: DWORD;
    Height: DWORD;
    CONST pPoint: LPPOINT;
    BytesPerPixel: DWORD;
)
begin
{
    EmuSwapFS();   // Win2k/XP FS

{$IFDEF DEBUG}
    DbgPrintf('EmuXapi : EmuXGSwizzleRect' +
        #13#10'(' +
        #13#10'   pSource             : 0x%.08X' +
        #13#10'   Pitch               : 0x%.08X' +
        #13#10'   pRect               : 0x%.08X' +
        #13#10'   pDest               : 0x%.08X' +
        #13#10'   Width               : 0x%.08X' +
        #13#10'   Height              : 0x%.08X' +
        #13#10'   pPoint              : 0x%.08X' +
        #13#10'   BytesPerPixel       : 0x%.08X' +
        #13#10');',
        [pSource, Pitch, pRect, pDest, Width, Height,
         pPoint, BytesPerPixel]);
{$ENDIF}

    if (pRect = NULL) and (pPoint = NULL) and (Pitch = 0) then
    {
        memcpy(pDest, pSource, Width*Height*BytesPerPixel);
    }
    else
    {
        if (Assigned(pPoint) and (pPoint.x <> 0 or pPoint.y <> 0)) then
            CxbxKrnlCleanup('Temporarily unsupported swizzle (very easy fix)');

        DWORD dwMaxY = Height;
        DWORD dwChunkSize = Width;

        uint08 *pSrc = (uint08*)(*pSource;
        uint08 *pDst = (uint08*)(*pDest;

        if Assigned(pRect) then
        {
            pSrc += pRect.top*Pitch;
            pSrc += pRect.left;

            dwMaxY = pRect.bottom - pRect.top;
            dwChunkSize = pRect.right - pRect.left;
        }

        for(DWORD y=0;y<dwMaxY;y++)
        {
            memcpy(pSrc, pDst, dwChunkSize);
            pSrc += Pitch;
            pDst += Pitch;
        }
    }

    EmuSwapFS();   // Xbox FS

end;
*)


(*VOID WINAPI XTL::EmuXGSwizzleBox
(
    LPCVOID          pSource,
    DWORD            RowPitch,
    DWORD            SlicePitch,
    CONST D3DBOX    *pBox,
    LPVOID           pDest,
    DWORD            Width,
    DWORD            Height,
    DWORD            Depth,
    CONST XGPOINT3D *pPoint,
    DWORD            BytesPerPixel
)
{
    EmuSwapFS();   // Win2k/XP FS

{$IFDEF DEBUG}
    DbgPrintf('EmuXapi : EmuXGSwizzleBox' +
        #13#10'(' +
        #13#10'   pSource             : 0x%.08X' +
        #13#10'   RowPitch            : 0x%.08X' +
        #13#10'   SlicePitch          : 0x%.08X' +
        #13#10'   pBox                : 0x%.08X' +
        #13#10'   pDest               : 0x%.08X' +
        #13#10'   Width               : 0x%.08X' +
        #13#10'   Height              : 0x%.08X' +
        #13#10'   Depth               : 0x%.08X' +
        #13#10'   pPoint              : 0x%.08X' +
        #13#10'   BytesPerPixel       : 0x%.08X' +
        #13#10');',
        [pSource, RowPitch, SlicePitch, pBox, pDest, Width, Height,
         Depth, pPoint, BytesPerPixel]);
{$ENDIF}

    if (pBox = NULL) and (pPoint = NULL) and (RowPitch = 0) and (SlicePitch = 0) then
    {
        memcpy(pDest, pSource, Width*Height*Depth*BytesPerPixel);
    }
    else
    {
        CxbxKrnlCleanup('Temporarily unsupported swizzle (easy fix)');
    }

    EmuSwapFS();   // Xbox FS

}
*)

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
) ; stdcall;
(*var
  dwOffsetU : DWORD;
  dwMaskU : DWORD;
  dwOffsetV : DWORD;
  dwMaskV : DWORD;
  dwOffsetW : DWORD;
  dwMaskW : DWORD;
  i : DWORD;
  j : DWORD; *)
begin
(*
  dwOffsetU := 0;
  dwMaskU := 0;
  dwOffsetV := 0;
  dwMaskV := 0;
  dwOffsetW := 0;
  dwMaskW := 0;

  i := 1;
  j := 1;*)

//  MARKED OUT CXBX: while ((i >= dwWidth) or (i >= dwHeight) or (i >= dwDepth))

{ TODO -oDXBX : needs to be translated }
(*  while ((i <= dwWidth) or (i <= dwHeight) or (i <= dwDepth)) do
  begin

    if (i < dwWidth) then
    begin
      dwMaskU |= j;
      j := j shl 1;
    end;

    if (i < dwHeight) then
    begin
      dwMaskV |= j;
      j := j shl 1;
    end;

    if (i < dwDepth) then
    begin
      dwMaskW |= j;
      j := j shl 1;
    end;

    i := i shl 1;
  end;

  DWORD dwSU = 0;
  DWORD dwSV = 0;
  DWORD dwSW = 0;
  DWORD dwMaskMax=0;

  // get the biggest mask
  if(dwMaskU > dwMaskV)
    dwMaskMax=dwMaskU;
  else
    dwMaskMax=dwMaskV;
  if(dwMaskW > dwMaskMax)
    dwMaskMax=dwMaskW;

  for(i = 1; i <= dwMaskMax; i<<=1)
    {
    if(i<=dwMaskU)
        {
      if(dwMaskU and i) > 0 then dwSU |= (dwOffsetU and i);
      else            dwOffsetU<<=1;
    }

        if (i<=dwMaskV) then
        {
      if(dwMaskV and i) > 0 then dwSV |= (dwOffsetV and i);
      else            dwOffsetV<<=1;
    }

        if (i<=dwMaskW) then
        {
      if(dwMaskW and i) > 0 then dwSW |= (dwOffsetW and i);
      else            dwOffsetW<<=1;
    }
  }

  DWORD dwW = dwSW;
  DWORD dwV = dwSV;
  DWORD dwU = dwSU;

  for(DWORD z=0; z<dwDepth; z++)
  {
    dwV = dwSV;

    for(DWORD y=0; y<dwHeight; y++)
    {
      dwU = dwSU;

      for (DWORD x=0; x<dwWidth; x++)
      {
        memcpy(pDstBuff, @((BYTE*)(*pSrcBuff)[(dwU|dwV|dwW)*dwBPP], dwBPP);
        pDstBuff=(PVOID)(((DWORD)pDstBuff)+dwBPP);

        dwU = (dwU - dwMaskU) and dwMaskU;
      }
      pDstBuff=(PVOID)(((DWORD)pDstBuff)+(dwPitch-dwWidth*dwBPP));
      dwV = (dwV - dwMaskV) and dwMaskV;
    }
    dwW = (dwW - dwMaskW) and dwMaskW;
  }
}   *)
end;


exports
  XTL_EmuXGUnswizzleRect,
  XTL_EmuXGIsSwizzledFormat;

end.
