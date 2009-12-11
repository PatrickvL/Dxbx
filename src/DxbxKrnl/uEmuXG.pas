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
  DbgPrintf('EmuXapi: EmuXGIsSwizzledFormat\n' +
         #13#10+'(' +
         #13#10+'   Format              : 0x%.08X\n' +
         #13#10+')',
         [Ord(Format)]);
  EmuSwapFS();   // Xbox FS
{$ENDIF}
(*	Result := FALSE; *)
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
    DbgPrintf("EmuXapi (0x%X): EmuXGSwizzleRect\n"
           "(\n"
           "   pSource             : 0x%.08X\n"
           "   Pitch               : 0x%.08X\n"
           "   pRect               : 0x%.08X\n"
           "   pDest               : 0x%.08X\n"
           "   Width               : 0x%.08X\n"
           "   Height              : 0x%.08X\n"
           "   pPoint              : 0x%.08X\n"
           "   BytesPerPixel       : 0x%.08X\n"
           ");\n",
           GetCurrentThreadId(), pSource, Pitch, pRect, pDest, Width, Height,
           pPoint, BytesPerPixel);
{$ENDIF}

    if (pRect == NULL && pPoint == NULL && Pitch == 0) then
    {
        memcpy(pDest, pSource, Width*Height*BytesPerPixel);
    }
    else
    {
        if (pPoint != NULL && (pPoint->x != 0 || pPoint->y != 0)) then
            CxbxKrnlCleanup("Temporarily unsupported swizzle (very easy fix)");

        DWORD dwMaxY = Height;
        DWORD dwChunkSize = Width;

        uint08 *pSrc = (uint08*)(*pSource;
        uint08 *pDst = (uint08*)(*pDest;

        if (pRect != 0) then
        {
            pSrc += pRect->top*Pitch;
            pSrc += pRect->left;

            dwMaxY = pRect->bottom - pRect->top;
            dwChunkSize = pRect->right - pRect->left;
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
    DbgPrintf("EmuXapi (0x%X): EmuXGSwizzleBox\n"
           "(\n"
           "   pSource             : 0x%.08X\n"
           "   RowPitch            : 0x%.08X\n"
           "   SlicePitch          : 0x%.08X\n"
           "   pBox                : 0x%.08X\n"
           "   pDest               : 0x%.08X\n"
           "   Width               : 0x%.08X\n"
           "   Height              : 0x%.08X\n"
           "   Depth               : 0x%.08X\n"
           "   pPoint              : 0x%.08X\n"
           "   BytesPerPixel       : 0x%.08X\n"
           ");\n",
           GetCurrentThreadId(), pSource, RowPitch, SlicePitch, pBox, pDest, Width, Height,
           Depth, pPoint, BytesPerPixel);
{$ENDIF}

    if (pBox == NULL && pPoint == NULL && RowPitch == 0 && SlicePitch == 0) then
    {
        memcpy(pDest, pSource, Width*Height*Depth*BytesPerPixel);
    }
    else
    {
        CxbxKrnlCleanup("Temporarily unsupported swizzle (easy fix)");
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

//	MARKED OUT CXBX: while ((i >= dwWidth) or (i >= dwHeight) or (i >= dwDepth))

{ TODO -oDXBX : needs to be translated }
(*  while ((i <= dwWidth) or (i <= dwHeight) or (i <= dwDepth)) do
  begin

    if (i < dwWidth) then
    begin
			dwMaskU |= j;
			j<<=1;
		end;

    if (i < dwHeight) then
    begin
			dwMaskV |= j;
			j<<=1;
		end;

    if (i < dwDepth) then
    begin
			dwMaskW |= j;
      j<<=1;
    end;

    i<<=1;
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
			if(dwMaskU & i) dwSU |= (dwOffsetU & i);
			else            dwOffsetU<<=1;
		}

        if (i<=dwMaskV) then
        {
			if(dwMaskV & i) dwSV |= (dwOffsetV & i);
			else            dwOffsetV<<=1;
		}

        if (i<=dwMaskW) then
        {
			if(dwMaskW & i) dwSW |= (dwOffsetW & i);
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
				memcpy(pDstBuff, &((BYTE*)(*pSrcBuff)[(dwU|dwV|dwW)*dwBPP], dwBPP);
				pDstBuff=(PVOID)(((DWORD)pDstBuff)+dwBPP);

				dwU = (dwU - dwMaskU) & dwMaskU;
			}
			pDstBuff=(PVOID)(((DWORD)pDstBuff)+(dwPitch-dwWidth*dwBPP));
			dwV = (dwV - dwMaskV) & dwMaskV;
		}
		dwW = (dwW - dwMaskW) & dwMaskW;
	}
}   *)
end;


exports
  XTL_EmuXGUnswizzleRect,
  XTL_EmuXGIsSwizzledFormat;

end.
