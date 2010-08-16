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
{$IFDEF DXBX_USE_D3D9}
  Direct3D9,
{$ELSE}
  Direct3D8, // IDirect3DBaseTexture8
{$ENDIF}
  // Dxbx
  uTypes,
  uConvert,
  uDxbxUtils, // iif
  uDxbxKrnlUtils,
  uEmuD3D8Utils;

procedure XTL_EmuUpdateDeferredStates(); {NOPATCH}
function VersionAdjust_D3DTSS(const NewValue: DWORD): DWORD; {NOPATCH}

// deferred state lookup tables
var XTL_EmuD3DDeferredRenderState: PDWORDs;
var XTL_EmuD3DDeferredTextureState: PDWORDs;

var XTL_EmuD3DDeferredRenderState_Start: DWord; // Dxbx addition, to allow for SDK version dependant shifting
var XTL_EmuD3DDeferredRenderState_Size: DWord; // Dxbx addition
var XTL_EmuD3DRenderState_ComplexCorrection: Integer; // Dxbx addition, to allow for SDK version dependant shifting

const DEFAULT_XDK_VERSION = 4627; // TODO -oDxbx : Make this configurable
var g_BuildVersion: uint32 = DEFAULT_XDK_VERSION;
// var g_OrigBuildVersion: uint32; // Dxbx note : Unused


implementation

uses
  uEmuD3D8, // g_BuildVersion
  uEmuD3D8Types;

// For 3925, the actual D3DTSS flags have different values.
// This function maps new indexes to old ones, so that we
// can read a specific member from the emulated XBE's
// XTL_EmuD3DDeferredTextureState buffer.
function VersionAdjust_D3DTSS(const NewValue: DWORD): DWORD;
const
  OLD_X_D3DTSS_COLOROP = 0;
  OLD_X_D3DTSS_TEXTURETRANSFORMFLAGS = 9;
  OLD_X_D3DTSS_ADDRESSU = 10;
  OLD_X_D3DTSS_ALPHAKILL = 21;
begin
  Result := NewValue;
  if g_BuildVersion <= 3925 then
  begin
    // In SDK 3925 (or at somewhere else between 3911 and 4361), the deferred texture states where switched;
    // D3DTSS_COLOROP ..D3DTSS_TEXTURETRANSFORMFLAGS ranged  0.. 9 which has become 12..21
    // D3DTSS_ADDRESSU..D3DTSS_ALPHAKILL             ranged 10..21 which has become  0..11
    if (NewValue <= X_D3DTSS_TEXTURETRANSFORMFLAGS) then
      if (NewValue <= X_D3DTSS_ALPHAKILL) then
        Inc(Result, OLD_X_D3DTSS_ADDRESSU)
      else
        Dec(Result, {NEW}X_D3DTSS_COLOROP);
  end;
end;

procedure XTL_EmuUpdateDeferredStates(); {NOPATCH}
// Branch:shogun  Revision:163  Translator:PatrickvL  Done:100
var
  dwConv: DWORD;
  v: int;
  pCur: PDWORDs;
  X_D3DTSS: DWORD;
  pTexture: XTL_PIDirect3DBaseTexture8;
  dwValue: DWORD;
begin
  // Certain D3DRS values need to be checked on each Draw[Indexed]Vertices
  if (XTL_EmuD3DDeferredRenderState <> nil) then
  begin
    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_FOGENABLE] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_FOGENABLE, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_FOGENABLE]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_FOGTABLEMODE] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_FOGTABLEMODE, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_FOGTABLEMODE]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_FOGSTART] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_FOGSTART, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_FOGSTART]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_FOGEND] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_FOGEND, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_FOGEND]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_FOGDENSITY] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_FOGDENSITY, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_FOGDENSITY]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_RANGEFOGENABLE] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_RANGEFOGENABLE, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_RANGEFOGENABLE]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_WRAP0] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_WRAP0, EmuXB2PC_D3DWRAP(XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_WRAP0]));

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_WRAP1] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_WRAP1, EmuXB2PC_D3DWRAP(XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_WRAP1]));

    // Dxbx addition :
    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_WRAP2] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_WRAP2, EmuXB2PC_D3DWRAP(XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_WRAP2]));

    // Dxbx addition :
    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_WRAP3] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_WRAP3, EmuXB2PC_D3DWRAP(XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_WRAP3]));

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_LIGHTING] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_LIGHTING, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_LIGHTING]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_SPECULARENABLE] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_SPECULARENABLE, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_SPECULARENABLE]);

    // Dxbx addition :
    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_LOCALVIEWER] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_LOCALVIEWER, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_LOCALVIEWER]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_COLORVERTEX] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_COLORVERTEX, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_COLORVERTEX]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_DIFFUSEMATERIALSOURCE] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_DIFFUSEMATERIALSOURCE, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_DIFFUSEMATERIALSOURCE]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_AMBIENTMATERIALSOURCE] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_AMBIENTMATERIALSOURCE, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_AMBIENTMATERIALSOURCE]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_EMISSIVEMATERIALSOURCE] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_EMISSIVEMATERIALSOURCE, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_EMISSIVEMATERIALSOURCE]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_AMBIENT] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_AMBIENT, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_AMBIENT]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSIZE] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_POINTSIZE, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSIZE]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSIZE_MIN] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_POINTSIZE_MIN, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSIZE_MIN]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSPRITEENABLE] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_POINTSPRITEENABLE, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSPRITEENABLE]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSCALEENABLE] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_POINTSCALEENABLE, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSCALEENABLE]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSCALE_A] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_POINTSCALE_A, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSCALE_A]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSCALE_B] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_POINTSCALE_B, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSCALE_B]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSCALE_C] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_POINTSCALE_C, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSCALE_C]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSIZE_MAX] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_POINTSIZE_MAX, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSIZE_MAX]);

    // Dxbx addition :
    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_PATCHEDGESTYLE] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_PATCHEDGESTYLE, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_PATCHEDGESTYLE]);

    if (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_PATCHSEGMENTS] <> X_D3DRS_UNK) then
      g_pD3DDevice.SetRenderState(D3DRS_PATCHSEGMENTS, XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_PATCHSEGMENTS]);

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

  // Certain D3DTS values need to be checked on each Draw[Indexed]Vertices
  if (XTL_EmuD3DDeferredTextureState <> nil) then
  begin
    for v := 0 to X_D3DTS_STAGECOUNT-1 do
    begin
      pCur := @(XTL_EmuD3DDeferredTextureState[v*X_D3DTS_STAGESIZE]);

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_ADDRESSU)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 5) then
          DxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_ADDRESSU, X_D3DTSS);
      end;

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_ADDRESSV)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 5) then
          DxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_ADDRESSV, X_D3DTSS);
      end;

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_ADDRESSW)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 5) then
          DxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_ADDRESSW, X_D3DTSS);
      end;

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_MAGFILTER)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 4) then
          DxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MAGFILTER, X_D3DTSS);
      end;

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_MINFILTER)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 4) then
          DxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MINFILTER, X_D3DTSS);
      end;

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_MIPFILTER)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 4) then
          DxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MIPFILTER, X_D3DTSS);
      end;

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_MIPMAPLODBIAS)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MIPMAPLODBIAS, X_D3DTSS);

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_MAXMIPLEVEL)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MAXMIPLEVEL, X_D3DTSS);

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_MAXANISOTROPY)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MAXANISOTROPY, X_D3DTSS);

      // TODO -oDxbx : Emulate X_D3DTSS_COLORKEYOP (Xbox ext.)
      // TODO -oDxbx : Emulate X_D3DTSS_COLORSIGN (Xbox ext.)
      // TODO -oDxbx : Emulate X_D3DTSS_ALPHAKILL (Xbox ext.)

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_COLOROP)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLOROP, EmuXB2PC_D3DTEXTUREOP(X_D3DTSS));

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_COLORARG0)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLORARG0, X_D3DTSS);

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_COLORARG1)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLORARG1, X_D3DTSS);

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_COLORARG2)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLORARG2, X_D3DTSS);

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_ALPHAOP)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAOP, EmuXB2PC_D3DTEXTUREOP(X_D3DTSS));

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_ALPHAARG0)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAARG0, X_D3DTSS);

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_ALPHAARG1)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAARG1, X_D3DTSS);

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_ALPHAARG2)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAARG2, X_D3DTSS);

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_RESULTARG)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_RESULTARG, X_D3DTSS);

      X_D3DTSS := pCur[VersionAdjust_D3DTSS(X_D3DTSS_TEXTURETRANSFORMFLAGS)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_TEXTURETRANSFORMFLAGS, X_D3DTSS);

      // Cxbx note : D3DTSS_BORDERCOLOR is NOT a deferred texture state!

      (* Cxbx has this disabled :
      // To check for unhandled texture stage state changes
      for(int r=0;r<X_D3DTS_STAGESIZE;r++)
      begin
        static const int unchecked[] =
        begin
          0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 29, 30, 31
        end;;

        if (pCur[r] <> X_D3DTSS_UNK) then
        begin
          _bool pass := true;

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
    if  Assigned(XTL_EmuD3DDeferredRenderState) // Dxbx addition
    and (XTL_EmuD3DDeferredRenderState[X_D3DRS_DEFERRED_POINTSPRITEENABLE] = DWord(BOOL_TRUE)) then // Dxbx note : DWord cast to prevent warning
    begin
      // pCur := Texture Stage 3 States
      pCur := @(XTL_EmuD3DDeferredTextureState[3*X_D3DTS_STAGESIZE]); // StrikerX3: why was this 2*32? PatrickvL: Probably a bug.

      // set the point sprites texture
      g_pD3DDevice.GetTexture(3, PIDirect3DBaseTexture(@pTexture));
      g_pD3DDevice.SetTexture(0, IDirect3DBaseTexture(pTexture));
      // TODO -oDXBX: Should we clear the pTexture interface (and how)?

      // disable all other stages
      g_pD3DDevice.SetTextureStageState(1, D3DTSS_COLOROP, D3DTOP_DISABLE);
      g_pD3DDevice.SetTextureStageState(1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);

      // in that case we have to copy over the stage by hand
      for v := 0 to XTL_EmuD3DDeferredRenderState_Size-1 do
      begin
        if (pCur[v] <> X_D3DTSS_UNK) then
        begin
          g_pD3DDevice.GetTextureStageState(3, D3DTEXTURESTAGESTATETYPE(v), {Out}dwValue);
          g_pD3DDevice.SetTextureStageState(0, D3DTEXTURESTAGESTATETYPE(v), dwValue);
        end;
      end;
    end;
  end;

  if (g_bFakePixelShaderLoaded) then
  begin
    g_pD3DDevice.SetRenderState(D3DRS_FOGENABLE, BOOL_FALSE);

    // programmable pipeline
    (* Dxbx note : If the following is enabled, Sokoban loses it's textures, so disable it for now :
    for v:=0 to X_D3DTS_STAGECOUNT-1 do
    begin
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLOROP, D3DTOP_DISABLE);
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
    end;
    *)

    // fixed pipeline
    (* Cxbx has this disabled :
    for v:=0 to 4-1 do
    begin
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLOROP,   D3DTOP_MODULATE);
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLORARG1, D3DTA_TEXTURE);
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLORARG2, D3DTA_CURRENT);

      g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAOP,   D3DTOP_MODULATE);
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
    end;

    g_pD3DDevice.SetRenderState(D3DRS_NORMALIZENORMALS, BOOL_TRUE);
    g_pD3DDevice.SetRenderState(D3DRS_LIGHTING,BOOL_TRUE);
    g_pD3DDevice.SetRenderState(D3DRS_AMBIENT, $FFFFFFFF);
    *)
  end;
end;

{.$MESSAGE 'PatrickvL reviewed up to here'}
end.
