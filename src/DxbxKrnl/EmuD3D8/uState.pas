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
  Windows;

procedure XTL_EmuUpdateDeferredStates; stdcall;

var
  // deferred state lookup tables
  XTL_EmuD3DDeferredRenderState: PDWORD;
  XTL_EmuD3DDeferredTextureState: PDWORD;


implementation

uses
  uEmuD3D8Types;

procedure XTL_EmuUpdateDeferredStates; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:70
begin
  //using namespace XTL;
  // Certain D3DRS values need to be checked on each Draw[Indexed]Vertices
(*
  if (XTL_EmuD3DDeferredRenderState <> nil) then
  begin
    if (XTL_EmuD3DDeferredRenderState[0] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_FOGENABLE, EmuD3DDeferredRenderState[0]);

    if (XTL_EmuD3DDeferredRenderState[1] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_FOGTABLEMODE, XTL_EmuD3DDeferredRenderState[1]);

    if (XTL_EmuD3DDeferredRenderState[2] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_FOGSTART, XTL_EmuD3DDeferredRenderState[2]);

    if (XTL_EmuD3DDeferredRenderState[3] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_FOGEND, XTL_EmuD3DDeferredRenderState[3]);

    if (XTL_EmuD3DDeferredRenderState[4] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_FOGDENSITY, XTL_EmuD3DDeferredRenderState[4]);

    if (XTL_EmuD3DDeferredRenderState[5] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_RANGEFOGENABLE, XTL_EmuD3DDeferredRenderState[5]);

    if (XTL_EmuD3DDeferredRenderState[6] <> X_D3DRS_UNK) then
    begin
      ::DWORD dwConv := 0;

      dwConv |= (XTL_EmuD3DDeferredRenderState[6] and $00000010) ? D3DWRAP_U : 0;
      dwConv |= (XTL_EmuD3DDeferredRenderState[6] and $00001000) ? D3DWRAP_V : 0;
      dwConv |= (XTL_EmuD3DDeferredRenderState[6] and $00100000) ? D3DWRAP_W : 0;

      g_pD3DDevice8.SetRenderState(D3DRS_WRAP0, dwConv);
    end;

    if (XTL_EmuD3DDeferredRenderState[7] <> X_D3DRS_UNK) then
    begin
      ::DWORD dwConv := 0;

      dwConv |= (XTL_EmuD3DDeferredRenderState[7] and $00000010) ? D3DWRAP_U : 0;
      dwConv |= (XTL_EmuD3DDeferredRenderState[7] and $00001000) ? D3DWRAP_V : 0;
      dwConv |= (XTL_EmuD3DDeferredRenderState[7] and $00100000) ? D3DWRAP_W : 0;

      g_pD3DDevice8.SetRenderState(D3DRS_WRAP1, dwConv);
    end;

    if (XTL_EmuD3DDeferredRenderState[10] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_LIGHTING, XTL_EmuD3DDeferredRenderState[10]);

    if (XTL_EmuD3DDeferredRenderState[11] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_SPECULARENABLE, XTL_EmuD3DDeferredRenderState[11]);

    if (XTL_EmuD3DDeferredRenderState[13] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_COLORVERTEX, XTL_EmuD3DDeferredRenderState[13]);

    if (XTL_EmuD3DDeferredRenderState[19] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_DIFFUSEMATERIALSOURCE, XTL_EmuD3DDeferredRenderState[19]);

    if (XTL_EmuD3DDeferredRenderState[20] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_AMBIENTMATERIALSOURCE, XTL_EmuD3DDeferredRenderState[20]);

    if (XTL_EmuD3DDeferredRenderState[21] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_EMISSIVEMATERIALSOURCE, XTL_EmuD3DDeferredRenderState[21]);

    if (XTL_EmuD3DDeferredRenderState[23] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_AMBIENT, XTL_EmuD3DDeferredRenderState[23]);

    if (XTL_EmuD3DDeferredRenderState[24] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_POINTSIZE, XTL_EmuD3DDeferredRenderState[24]);

    if (XTL_EmuD3DDeferredRenderState[25] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_POINTSIZE_MIN, XTL_EmuD3DDeferredRenderState[25]);

    if (XTL_EmuD3DDeferredRenderState[26] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_POINTSPRITEENABLE, XTL_EmuD3DDeferredRenderState[26]);

    if (XTL_EmuD3DDeferredRenderState[27] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_POINTSCALEENABLE, XTL_EmuD3DDeferredRenderState[27]);

    if (XTL_EmuD3DDeferredRenderState[28] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_POINTSCALE_A, XTL_EmuD3DDeferredRenderState[28]);

    if (XTL_EmuD3DDeferredRenderState[29] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_POINTSCALE_B, XTL_EmuD3DDeferredRenderState[29]);

    if (XTL_EmuD3DDeferredRenderState[30] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_POINTSCALE_C, XTL_EmuD3DDeferredRenderState[30]);

    if (XTL_EmuD3DDeferredRenderState[31] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_POINTSIZE_MAX, XTL_EmuD3DDeferredRenderState[31]);

    if (XTL_EmuD3DDeferredRenderState[33] <> X_D3DRS_UNK) then
      g_pD3DDevice8.SetRenderState(D3DRS_PATCHSEGMENTS, XTL_EmuD3DDeferredRenderState[33]);

    /** To check for unhandled RenderStates
    for(int v=0;v<117-82;v++)
    begin
      if (XTL_EmuD3DDeferredRenderState[v] <> X_D3DRS_UNK) then
      begin
        if (v <>  0) and (v <>  1) and (v <>  2) and (v <>  3) and (v <>  4) and (v <>  5) and (v <>  6) and (v <>  7)
        and (v <> 10) and (v <> 11) and (v <> 13) and (v <> 19) and (v <> 20) and (v <> 21) and (v <> 23) and (v <> 24)
        and (v <> 25) and (v <> 26) and (v <> 27) and (v <> 28) and (v <> 29) and (v <> 30) and (v <> 31) and (v <> 33) then
          EmuWarning('Unhandled RenderState Change @ %d (%d)', [v, v + 82]);
      end;
    end;
    //**/
  end;
*)

(*
  // Certain D3DTS values need to be checked on each Draw[Indexed]Vertices
  if (EmuD3DDeferredTextureState <> 0) then
  begin
    for(int v=0;v<4;v++)
    begin
        ::DWORD *pCur := @(EmuD3DDeferredTextureState[v*32];

        if (pCur[0] <> X_D3DTSS_UNK) then
        begin
            if (pCur[0] = 5) then
                CxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ADDRESSU, pCur[0]);
        end;

        if (pCur[1] <> X_D3DTSS_UNK) then
        begin
            if (pCur[1] = 5) then
                CxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ADDRESSV, pCur[1]);
        end;

        if (pCur[2] <> X_D3DTSS_UNK) then
        begin
            if (pCur[2] = 5) then
                CxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ADDRESSW, pCur[2]);
        end;

        if (pCur[3] <> X_D3DTSS_UNK) then
        begin
            if (pCur[3] = 4) then
                CxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_MAGFILTER, pCur[3]);
        end;

        if (pCur[4] <> X_D3DTSS_UNK) then
        begin
            if (pCur[4] = 4) then
                CxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_MINFILTER, pCur[4]);
        end;

        if (pCur[5] <> X_D3DTSS_UNK) then
        begin
            if (pCur[5] = 4) then
                CxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_MIPFILTER, pCur[5]);
        end;

        if (pCur[6] <> X_D3DTSS_UNK) then
            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_MIPMAPLODBIAS, pCur[6]);

        if (pCur[7] <> X_D3DTSS_UNK) then
            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_MAXMIPLEVEL, pCur[7]);

        if (pCur[8] <> X_D3DTSS_UNK) then
            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_MAXANISOTROPY, pCur[8]);

        // TODO: Use a lookup table, this is not always a 1:1 map
        if (pCur[12] <> X_D3DTSS_UNK) then
        begin
          if (pCur[12] > 12) and (not (pCur[12] >= 17) and (pCur[12] <= 21)) and ((pCur[12] <> 22)) and ((pCur[12] <> 14) and (pCur[12] <> 15)) then
            CxbxKrnlCleanup('(Temporarily) Unsupported D3DTSS_COLOROP Value (%d)', [pCur[12]]);

          // Dirty Hack: 22 = D3DTOP_DOTPRODUCT3
          if ( pCur[12] = 22 ) then
            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_COLOROP, D3DTOP_DOTPRODUCT3);
          else if ( pCur[12] = 14 ) then
            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_COLOROP, D3DTOP_BLENDTEXTUREALPHA);
          else if ( pCur[12] = 15 ) then
            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_COLOROP, D3DTOP_BLENDFACTORALPHA);
          else
            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_COLOROP, pCur[12]);
        end;

        if (pCur[13] <> X_D3DTSS_UNK) then
          g_pD3DDevice8.SetTextureStageState(v, D3DTSS_COLORARG0, pCur[13]);

        if (pCur[14] <> X_D3DTSS_UNK) then
          g_pD3DDevice8.SetTextureStageState(v, D3DTSS_COLORARG1, pCur[14]);

        if (pCur[15] <> X_D3DTSS_UNK) then
          g_pD3DDevice8.SetTextureStageState(v, D3DTSS_COLORARG2, pCur[15]);

        // TODO: Use a lookup table, this is not always a 1:1 map (same as D3DTSS_COLOROP)
        if (pCur[16] <> X_D3DTSS_UNK) then
        begin
          if (pCur[16] > 12) and (pCur[16] <> 14) then
            CxbxKrnlCleanup('(Temporarily) Unsupported D3DTSS_ALPHAOP Value (%d)', [pCur[16]]);

          if ( pCur[16] = 14 ) then
            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ALPHAOP, D3DTOP_BLENDTEXTUREALPHA);
          if ( pCur[16] = 15 ) then
            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ALPHAOP, D3DTOP_BLENDFACTORALPHA);
          else
            g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ALPHAOP, pCur[16]);
        end;

        if (pCur[17] <> X_D3DTSS_UNK) then
          g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ALPHAARG0, pCur[17]);

        if (pCur[18] <> X_D3DTSS_UNK) then
          g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ALPHAARG1, pCur[18]);

        if (pCur[19] <> X_D3DTSS_UNK) then
          g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ALPHAARG2, pCur[19]);

        if (pCur[20] <> X_D3DTSS_UNK) then
          g_pD3DDevice8.SetTextureStageState(v, D3DTSS_RESULTARG, pCur[20]);

        if (pCur[21] <> X_D3DTSS_UNK) then
          g_pD3DDevice8.SetTextureStageState(v, D3DTSS_TEXTURETRANSFORMFLAGS, pCur[21]);

        if (pCur[29] <> X_D3DTSS_UNK) then
          g_pD3DDevice8.SetTextureStageState(v, D3DTSS_BORDERCOLOR, pCur[29]);

        /** To check for unhandled texture stage state changes
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
        //**/
    end;

    // if point sprites are enabled, copy stage 3 over to 0
    if (EmuD3DDeferredRenderState[26] = TRUE) then
    begin
      // pCur := Texture Stage 3 States
      ::DWORD *pCur := @(EmuD3DDeferredTextureState[2*32];

      IDirect3DBaseTexture8 *pTexture;

      // set the point sprites texture
      g_pD3DDevice8.GetTexture(3, @pTexture);
      g_pD3DDevice8.SetTexture(0, pTexture);

      // disable all other stages
      g_pD3DDevice8.SetTextureStageState(1, D3DTSS_COLOROP, D3DTOP_DISABLE);
      g_pD3DDevice8.SetTextureStageState(1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);

      // in that case we have to copy over the stage by hand
      for(int v=0;v<30;v++)
      begin
        if (pCur[v] <> X_D3DTSS_UNK) then
        begin
          ::DWORD dwValue;

          g_pD3DDevice8.GetTextureStageState(3, (D3DTEXTURESTAGESTATETYPE)v, @dwValue);
          g_pD3DDevice8.SetTextureStageState(0, (D3DTEXTURESTAGESTATETYPE)v, dwValue);
        end;
      end;
    end;
  end;
*)

(*
  if (g_bFakePixelShaderLoaded) then
  begin
    g_pD3DDevice8.SetRenderState(D3DRS_FOGENABLE, FALSE);

    // programmable pipeline
    //*
    for(int v=0;v<4;v++)
    begin
      g_pD3DDevice8.SetTextureStageState(v, D3DTSS_COLOROP, D3DTOP_DISABLE);
      g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
    end;
    //*/

    // fixed pipeline
    /*
    for(int v=0;v<4;v++)
    begin
      g_pD3DDevice8.SetTextureStageState(v, D3DTSS_COLOROP,   D3DTOP_MODULATE);
      g_pD3DDevice8.SetTextureStageState(v, D3DTSS_COLORARG1, D3DTA_TEXTURE);
      g_pD3DDevice8.SetTextureStageState(v, D3DTSS_COLORARG2, D3DTA_CURRENT);

      g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ALPHAOP,   D3DTOP_MODULATE);
      g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
      g_pD3DDevice8.SetTextureStageState(v, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
    end;

    g_pD3DDevice8.SetRenderState(D3DRS_NORMALIZENORMALS, TRUE);
    g_pD3DDevice8.SetRenderState(D3DRS_LIGHTING,TRUE);
    g_pD3DDevice8.SetRenderState(D3DRS_AMBIENT, $FFFFFFFF);
    //*/
  end;
*)
end;

exports
  XTL_EmuUpdateDeferredStates;

end.
