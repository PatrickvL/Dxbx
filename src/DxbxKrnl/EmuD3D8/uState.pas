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

unit uState;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // DirectX
  Direct3D8, // IDirect3DBaseTexture8
  // Dxbx
  uTypes,
  uDxbxUtils, // iif
  uEmuXTL,
  uDxbxKrnlUtils;

procedure XTL_EmuUpdateDeferredStates; stdcall;

// deferred state lookup tables
var XTL_EmuD3DDeferredRenderState: PDWORDs;
var XTL_EmuD3DDeferredTextureState: PDWORDs;

var g_BuildVersion: uint32;
var g_OrigBuildVersion: uint32;

implementation

uses
  uEmuD3D8, // g_BuildVersion
  uEmuD3D8Types;

procedure XTL_EmuUpdateDeferredStates; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwConv: DWORD;
  v: int;
  pCur: PDWORDs;
  pTexture: XTL_PIDirect3DBaseTexture8;
  dwValue: DWORD;
  bHack3925: bool;
  Adjust1: int;
  Adjust2: int;
begin
  // Certain D3DRS values need to be checked on each Draw[Indexed]Vertices
  if (XTL_EmuD3DDeferredRenderState <> nil) then
  begin
    if (XTL_EmuD3DDeferredRenderState[0] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_FOGENABLE, XTL_EmuD3DDeferredRenderState[0]);

    if (XTL_EmuD3DDeferredRenderState[1] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_FOGTABLEMODE, XTL_EmuD3DDeferredRenderState[1]);

    if (XTL_EmuD3DDeferredRenderState[2] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_FOGSTART, XTL_EmuD3DDeferredRenderState[2]);

    if (XTL_EmuD3DDeferredRenderState[3] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_FOGEND, XTL_EmuD3DDeferredRenderState[3]);

    if (XTL_EmuD3DDeferredRenderState[4] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_FOGDENSITY, XTL_EmuD3DDeferredRenderState[4]);

    if (XTL_EmuD3DDeferredRenderState[5] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_RANGEFOGENABLE, XTL_EmuD3DDeferredRenderState[5]);

    if (XTL_EmuD3DDeferredRenderState[6] <> X_D3DRS_UNK) then
    begin
      dwConv := 0;

      if (XTL_EmuD3DDeferredRenderState[6] and $00000010) > 0 then dwConv := dwConv or D3DWRAP_U;
      if (XTL_EmuD3DDeferredRenderState[6] and $00001000) > 0 then dwConv := dwConv or D3DWRAP_V;
      if (XTL_EmuD3DDeferredRenderState[6] and $00100000) > 0 then dwConv := dwConv or D3DWRAP_W;

      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_WRAP0, dwConv);
    end;

    if (XTL_EmuD3DDeferredRenderState[7] <> X_D3DRS_UNK) then
    begin
      dwConv := 0;

      if (XTL_EmuD3DDeferredRenderState[7] and $00000010) > 0 then dwConv := dwConv or D3DWRAP_U;
      if (XTL_EmuD3DDeferredRenderState[7] and $00001000) > 0 then dwConv := dwConv or D3DWRAP_V;
      if (XTL_EmuD3DDeferredRenderState[7] and $00100000) > 0 then dwConv := dwConv or D3DWRAP_W;

      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_WRAP1, dwConv);
    end;

    if (XTL_EmuD3DDeferredRenderState[10] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_LIGHTING, XTL_EmuD3DDeferredRenderState[10]);

    if (XTL_EmuD3DDeferredRenderState[11] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_SPECULARENABLE, XTL_EmuD3DDeferredRenderState[11]);

    if (XTL_EmuD3DDeferredRenderState[13] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_COLORVERTEX, XTL_EmuD3DDeferredRenderState[13]);

    if (XTL_EmuD3DDeferredRenderState[19] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_DIFFUSEMATERIALSOURCE, XTL_EmuD3DDeferredRenderState[19]);

    if (XTL_EmuD3DDeferredRenderState[20] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_AMBIENTMATERIALSOURCE, XTL_EmuD3DDeferredRenderState[20]);

    if (XTL_EmuD3DDeferredRenderState[21] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_EMISSIVEMATERIALSOURCE, XTL_EmuD3DDeferredRenderState[21]);

    if (XTL_EmuD3DDeferredRenderState[23] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_AMBIENT, XTL_EmuD3DDeferredRenderState[23]);

    if (XTL_EmuD3DDeferredRenderState[24] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_POINTSIZE, XTL_EmuD3DDeferredRenderState[24]);

    if (XTL_EmuD3DDeferredRenderState[25] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_POINTSIZE_MIN, XTL_EmuD3DDeferredRenderState[25]);

    if (XTL_EmuD3DDeferredRenderState[26] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_POINTSPRITEENABLE, XTL_EmuD3DDeferredRenderState[26]);

    if (XTL_EmuD3DDeferredRenderState[27] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_POINTSCALEENABLE, XTL_EmuD3DDeferredRenderState[27]);

    if (XTL_EmuD3DDeferredRenderState[28] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_POINTSCALE_A, XTL_EmuD3DDeferredRenderState[28]);

    if (XTL_EmuD3DDeferredRenderState[29] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_POINTSCALE_B, XTL_EmuD3DDeferredRenderState[29]);

    if (XTL_EmuD3DDeferredRenderState[30] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_POINTSCALE_C, XTL_EmuD3DDeferredRenderState[30]);

    if (XTL_EmuD3DDeferredRenderState[31] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_POINTSIZE_MAX, XTL_EmuD3DDeferredRenderState[31]);

    if (XTL_EmuD3DDeferredRenderState[33] <> X_D3DRS_UNK) then
      IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_PATCHSEGMENTS, XTL_EmuD3DDeferredRenderState[33]);

    (* Cxbx has this disabled :
    // To check for unhandled RenderStates
    for(int v=0;v<117-82;v++)
    begin
      if (XTL_EmuD3DDeferredRenderState[v] <> X_D3DRS_UNK) then
      begin
        if  (v <>  0) and (v <>  1) and (v <>  2) and (v <>  3) and (v <>  4) and (v <>  5) and (v <>  6) and (v <>  7)
        and (v <> 10) and (v <> 11) and (v <> 13) and (v <> 19) and (v <> 20) and (v <> 21) and (v <> 23) and (v <> 24)
        and (v <> 25) and (v <> 26) and (v <> 27) and (v <> 28) and (v <> 29) and (v <> 30) and (v <> 31) and (v <> 33) then
          EmuWarning('Unhandled RenderState Change @ %d (%d)', [v, v + 82]);
      end;
    end;
    *)
  end;

  // For 3925, the actual D3DTSS flags have different values.
  bHack3925 := (g_BuildVersion = 3925);
  Adjust1 := iif(bHack3925, 12, 0);
  Adjust2 := iif(bHack3925, 10, 0);

  // Certain D3DTS values need to be checked on each Draw[Indexed]Vertices
  if (XTL_EmuD3DDeferredTextureState <> nil) then
  begin
    for v := 0 to 4-1 do
    begin
      pCur := @(XTL_EmuD3DDeferredTextureState[v*32]);

      if (pCur[0+Adjust2] <> X_D3DTSS_UNK) then
      begin
        if (pCur[0+Adjust2] = 5) then
          CxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ADDRESSU, pCur[0]);
      end;

      if (pCur[1+Adjust2] <> X_D3DTSS_UNK) then
      begin
        if (pCur[1+Adjust2] = 5) then
          CxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ADDRESSV, pCur[1+Adjust2]); // Dxbx fix
      end;

      if (pCur[2+Adjust2] <> X_D3DTSS_UNK) then
      begin
        if (pCur[2+Adjust2] = 5) then
          CxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ADDRESSW, pCur[2+Adjust2]); // Dxbx fix
      end;

      if (pCur[3+Adjust2] <> X_D3DTSS_UNK) then
      begin
        if (pCur[3+Adjust2] = 4) then
          CxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_MAGFILTER, pCur[3+Adjust2]); // Dxbx fix
      end;

      if (pCur[4+Adjust2] <> X_D3DTSS_UNK) then
      begin
        if (pCur[4+Adjust2] = 4) then
          CxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_MINFILTER, pCur[4+Adjust2]);
      end;

      if (pCur[5+Adjust2] <> X_D3DTSS_UNK) then
      begin
        if (pCur[5+Adjust2] = 4) then
          CxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_MIPFILTER, pCur[5+Adjust2]);
      end;

      if (pCur[6+Adjust2] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_MIPMAPLODBIAS, pCur[6+Adjust2]);

      if (pCur[7+Adjust2] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_MAXMIPLEVEL, pCur[7+Adjust2]);

      if (pCur[8+Adjust2] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_MAXANISOTROPY, pCur[8+Adjust2]);

      // Cxbx TODO: Use a lookup table, this is not always a 1:1 map
      if (pCur[12] <> X_D3DTSS_UNK) then
      begin
        // Dxbx fix : Use Adjust-ment consistently :
        if (pCur[12-Adjust1] > 12) and (not (pCur[12-Adjust1] >= 17) and (pCur[12] <= 21)) and 
           (pCur[12-Adjust1] <> 22) and (pCur[12-Adjust1] <> 14) and 
           (pCur[12-Adjust1] <> 15) and (pCur[12-Adjust1] <> 13) then
          CxbxKrnlCleanup('(Temporarily) Unsupported D3DTSS_COLOROP Value (%d)', [pCur[12-Adjust1]]);

        // Dirty Hack: 22 = D3DTOP_DOTPRODUCT3
        if ( pCur[12-Adjust1] = 22 ) then
          IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLOROP, D3DTOP_DOTPRODUCT3)
        else if ( pCur[12-Adjust1] = 14 ) then
          IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLOROP, D3DTOP_BLENDTEXTUREALPHA)
        else if ( pCur[12-Adjust1] = 15 ) then
          IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLOROP, D3DTOP_BLENDFACTORALPHA)
        else if ( pCur[12-Adjust1] = 13 ) then
          IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLOROP, D3DTOP_BLENDCURRENTALPHA)
        else
          IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLOROP, pCur[12-Adjust1]);
      end;

      if (pCur[13-Adjust1] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLORARG0, pCur[13-Adjust1]);

      if (pCur[14-Adjust1] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLORARG1, pCur[14-Adjust1]);

      if (pCur[15-Adjust1] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLORARG2, pCur[15-Adjust1]);

      // Cxbx TODO: Use a lookup table, this is not always a 1:1 map (same as D3DTSS_COLOROP)
      if (pCur[16-Adjust1] <> X_D3DTSS_UNK) then
      begin
        if (pCur[16-Adjust1] > 12) and (pCur[16-Adjust1] <> 14) and (pCur[16-Adjust1] <> 13) then
          CxbxKrnlCleanup('(Temporarily) Unsupported D3DTSS_ALPHAOP Value (%d)', [pCur[16-Adjust1]]);

        if ( pCur[16-Adjust1] = 14 ) then
          IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ALPHAOP, D3DTOP_BLENDTEXTUREALPHA)
        else if ( pCur[16-Adjust1] = 15 ) then
          IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ALPHAOP, D3DTOP_BLENDFACTORALPHA)
        else if ( pCur[16-Adjust1] = 13 ) then
          IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ALPHAOP, D3DTOP_BLENDCURRENTALPHA)
        else
          IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ALPHAOP, pCur[16-Adjust1]);
      end;

      if (pCur[17-Adjust1] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ALPHAARG0, pCur[17-Adjust1]);

      if (pCur[18-Adjust1] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ALPHAARG1, pCur[18-Adjust1]);

      if (pCur[19-Adjust1] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ALPHAARG2, pCur[19-Adjust1]);

      if (pCur[20-Adjust1] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_RESULTARG, pCur[20-Adjust1]);

      if (pCur[21-Adjust1] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_TEXTURETRANSFORMFLAGS, pCur[21-Adjust1]);

      if (pCur[29] <> X_D3DTSS_UNK) then
        IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_BORDERCOLOR, pCur[29]);


      (* Cxbx has this disabled :
      // To check for unhandled texture stage state changes
      for(int r=0;r<32;r++)
      begin
        static const int unchecked[] =
        begin
          0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 29, 30, 31
        end;;

        if (pCur[r] <> X_D3DTSS_UNK) then
        begin
          bool pass := true;

          for(int q=0;q<sizeof(unchecked)/sizeof(int);q++)
          begin
            if (r = unchecked[q]) then
            begin
              pass := false;
              break;
            end;
          end;

          if (pass) then
            EmuWarning('Unhandled TextureState Change @ %d.%d', [v, r]);
        end;
      end;
      *)
    end;

    // if point sprites are enabled, copy stage 3 over to 0
    if (XTL_EmuD3DDeferredRenderState[26] > 0) then
    begin
      // pCur := Texture Stage 3 States
      pCur := @(XTL_EmuD3DDeferredTextureState[2*32]);

      // set the point sprites texture
      IDirect3DDevice8(g_pD3DDevice8).GetTexture(3, @pTexture);
      IDirect3DDevice8(g_pD3DDevice8).SetTexture(0, IDirect3DBaseTexture8(pTexture));
      // Dxbx TODO : Should we clear the pTexture interface (and how)?

      // disable all other stages
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(1, D3DTSS_COLOROP, D3DTOP_DISABLE);
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);

      // in that case we have to copy over the stage by hand
      for v := 0 to 30-1 do
      begin
        if (pCur[v] <> X_D3DTSS_UNK) then
        begin
          IDirect3DDevice8(g_pD3DDevice8).GetTextureStageState(3, D3DTEXTURESTAGESTATETYPE(v), {Out}dwValue);
          IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(0, D3DTEXTURESTAGESTATETYPE(v), dwValue);
        end;
      end;
    end;
  end;

  if (g_bFakePixelShaderLoaded) then
  begin
    IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_FOGENABLE, Ord(False));

    // programmable pipeline
    //*
    for v := 0 to 4-1 do
    begin
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLOROP, D3DTOP_DISABLE);
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
    end;
    //*/

    // fixed pipeline
    (* Cxbx has this disabled :
    for(int v=0;v<4;v++)
    begin
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLOROP,   D3DTOP_MODULATE);
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLORARG1, D3DTA_TEXTURE);
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_COLORARG2, D3DTA_CURRENT);

      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ALPHAOP,   D3DTOP_MODULATE);
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(v, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
    end;

    IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_NORMALIZENORMALS, TRUE);
    IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_LIGHTING,TRUE);
    IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_AMBIENT, $FFFFFFFF);
    *)
  end;
end;

exports
  XTL_EmuUpdateDeferredStates;

end.
