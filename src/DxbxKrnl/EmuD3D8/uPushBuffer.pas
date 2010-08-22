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

unit uPushBuffer;

{$INCLUDE Dxbx.inc}

{.$define _DEBUG_TRACK_PB}

interface

uses
  // Delphi
  Windows
  , SysUtils
  , Classes
  // Jedi Win32API
  , JwaWinType
  // DirectX
{$IFDEF DXBX_USE_D3D9}
  , Direct3D9
{$ELSE}
  , Direct3D8
{$ENDIF}
  // Dxbx
  , uTypes
  , uDxbxUtils // iif
  , uResourceTracker
  , uEmuD3D8Types
  , uEmuD3D8Utils
  , uVertexBuffer
  , uEmu
  , uEmuXG;

// From PushBuffer.h :

procedure XTL_EmuExecutePushBuffer
(
  pPushBuffer: PX_D3DPushBuffer;
  pFixup: PX_D3DFixup
); {NOPATCH}

procedure XTL_EmuExecutePushBufferRaw
(
  pdwPushData: PDWORD
); {NOPATCH}

// primary push buffer
var g_dwPrimaryPBCount: uint32 = 0;
var g_pPrimaryPB: PDWORD = nil; // Dxbx note : Cxbx uses Puint32 for this

// push buffer debugging
var XTL_g_bStepPush: _bool = false;
var XTL_g_bSkipPush: _bool = false;
var XTL_g_bBrkPush: _bool = false;

var g_bPBSkipPusher: _bool = false;

{$IFDEF _DEBUG_TRACK_PB}
procedure DbgDumpMesh(pIndexData: PWORD; dwCount: DWORD); {NOPATCH}
{$ENDIF}

procedure EmuUnswizzleActiveTexture(); {NOPATCH}

implementation

uses
  // Dxbx
    uDxbxKrnlUtils
  , uLog
  , uConvert
  , uEmuD3D8 // DxbxPresent
  , uState
  , uVertexShader;

const D3DPUSH_METHOD_MASK = $3FFFF;
const D3DPUSH_COUNT_SHIFT = 18;
const D3DPUSH_NOINCREMENT_FLAG = $40000000;
const D3DPUSH_MAX_COUNT = 2047;

const D3DPUSH_NO_OPERATION                = $00000100; // Parameter must be zero
const D3DPUSH_SET_BEGIN_END               = $000017fc; // Parameter is D3DPRIMITIVETYPE or 0 to end
const D3DPUSH_INLINE_ARRAY                = $00001818; // Use NOINCREMENT_FLAG
const D3DPUSH_SET_TRANSFORM_CONSTANT_LOAD = $00001ea4; // Add 96 to constant index parameter
const D3DPUSH_SET_TRANSFORM_CONSTANT      = $00000b80; // Can't use NOINCREMENT_FLAG, maximum of 32 writes

const NVPB_InlineIndexArray = $1800;
const NVPB_FixLoop = $1808;

const lfUnit = lfCxbx or lfPushBuffer;

// From PushBuffer.cpp :

procedure XTL_EmuExecutePushBuffer
(
    pPushBuffer: PX_D3DPushBuffer; 
    pFixup: PX_D3DFixup
); {NOPATCH}
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  if (pFixup <> NULL) then
    DxbxKrnlCleanup('PushBuffer has fixups');

  XTL_EmuExecutePushBufferRaw(PDWORD(pPushBuffer.Data));
end;

procedure EmuUnswizzleActiveTexture(); {NOPATCH}
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pPixelContainer: PX_D3DPixelContainer;
  XBFormat: DWord;
  dwBPP: DWord;
  pTexture: XTL_PIDirect3DTexture8;
  dwLevelCount: DWord;
  v: uint32;
  SurfaceDesc: D3DSURFACE_DESC;
  hRet: HRESULT;
  LockedRect: D3DLOCKED_RECT;

  dwWidth: DWord;
  dwHeight: DWord;
  dwDepth: DWord;
  dwPitch: DWord;
  iRect: TRect;
  iPoint: TPoint;

  pTemp: Pointer;

  Stage: int;
begin
  for Stage := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    // for current usages, we're always on stage 0
    pPixelContainer := PX_D3DPixelContainer(g_EmuD3DActiveTexture[Stage]);

    if (pPixelContainer = NULL) or (0 = (pPixelContainer.Common and X_D3DCOMMON_ISLOCKED)) then
      Exit;

    XBFormat := (pPixelContainer.Format and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT;
    dwBPP := 0;

    if (not EmuXBFormatIsSwizzled(XBFormat, @dwBPP)) then
      Exit;

    // TODO -oCXBX: potentially CRC to see if this surface was actually modified..

    //
    // unswizzle texture
    //

    begin
      pTexture := pPixelContainer.Emu.Texture;

      dwLevelCount := IDirect3DTexture(pTexture).GetLevelCount();

      if dwLevelCount > 0 then // Dxbx addition, to prevent underflow
      for v := 0 to dwLevelCount - 1 do
      begin
        // Dxbx addition : Remove lock for each level separately :
        IDirect3DTexture(pTexture).UnlockRect(v);

        hRet := IDirect3DTexture(pTexture).GetLevelDesc(v, {out}SurfaceDesc);

        if (FAILED(hRet)) then
          continue;

        //
        // perform unswizzle
        //

        begin
          // Cxbx has this commented out :
          //if (SurfaceDesc.Format <> XTL_D3DFMT_A8R8G8B8) then
          //  break;
          //DxbxKrnlCleanup('Temporarily unsupported format for active texture unswizzle (0x%.08X)', [SurfaceDesc.Format]);

          hRet := IDirect3DTexture(pTexture).LockRect(v, {out}LockedRect, NULL, 0);

          if (FAILED(hRet)) then
            continue;

          dwWidth := SurfaceDesc.Width;
          dwHeight := SurfaceDesc.Height;
          dwDepth := 1;
          dwPitch := LockedRect.Pitch;
          iRect := Classes.Rect(0,0,0,0);
          iPoint := Classes.Point(0,0);

          pTemp := malloc(dwHeight * dwPitch);

          EmuXGUnswizzleRect
          (
              LockedRect.pBits, dwWidth, dwHeight, dwDepth,
              pTemp, dwPitch, iRect, iPoint, dwBPP
          );

          memcpy(LockedRect.pBits, pTemp, dwPitch * dwHeight);

          IDirect3DTexture(pTexture).UnlockRect(v); // Dxbx fix : Cxbx unlocks level 0 each time!

          free(pTemp);
        end;
      end;

      if MayLog(lfUnit) then
        DbgPrintf('Active texture was unswizzled');
    end;

    // Dxbx note : Only set this _after_ processing all levels :
    pPixelContainer.Common := pPixelContainer.Common and (not X_D3DCOMMON_ISLOCKED);
  end;
end;

{static} var pIndexBuffer: XTL_LPDIRECT3DINDEXBUFFER8 = nil; // = XTL_PIDirect3DIndexBuffer8
//{static} var pVertexBuffer: XTL_LPDIRECT3DVERTEXBUFFER8 = nil; // = XTL_PIDirect3DVertexBuffer8
{static} var maxIBSize: uint = 0;
procedure XTL_EmuExecutePushBufferRaw
(
    pdwPushData: PDWord
); {NOPATCH}
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pIndexData: PVOID;
  pVertexData: PVOID;
  dwVertexShader: DWord;
  dwStride: DWord;
  pIBMem: array [0..4-1] of WORD;
  PCPrimitiveType: D3DPRIMITIVETYPE;
  XBPrimitiveType: X_D3DPRIMITIVETYPE;
  dwCount: DWord;
  dwMethod: DWord;
  bInc: BOOL_;
  hRet: HRESULT;
  pData: PWORDArray;
  VertexCount: UINT;
  PrimitiveCount: UINT;
  VPDesc: VertexPatchDesc;
  VertPatch: VertexPatcher;
  mi: uint;
  pwVal: PWORDs;

{$ifdef _DEBUG_TRACK_PB}
  pdwOrigPushData: PDWORD;
  bShowPB: _bool;
  s: uint;
  pActiveVB: XTL_PIDirect3DVertexBuffer8;
  VBDesc: D3DVERTEXBUFFER_DESC;
  pVBData: PBYTE;
  uiOffsetInBytes: UINT;
  uiStride: UINT;
{$endif}

begin
  if XTL_g_bSkipPush then
    Exit;

{$ifdef _DEBUG_TRACK_PB}
  pdwOrigPushData := pdwPushData;
{$endif}

  //pIndexData := nil;
  //pVertexData := nil;

  dwVertexShader := DWORD(-1);
  //dwStride := DWORD(-1);

  // cache of last 4 indices
  pIBMem[0] := $FFFF; pIBMem[1] := $FFFF; pIBMem[2] := $FFFF; pIBMem[3] := $FFFF;

  PCPrimitiveType := D3DPRIMITIVETYPE(-1);
  XBPrimitiveType := X_D3DPT_INVALID;

  // TODO -oCXBX: This technically should be enabled
  XTL_EmuUpdateDeferredStates();

{$ifdef _DEBUG_TRACK_PB}
  bShowPB := false;

  g_PBTrackTotal.insert(pdwPushData);

  if (not g_PBTrackShowOnce.exists(pdwPushData)) then
  begin
    g_PBTrackShowOnce.insert(pdwPushData);

    if MayLog(lfUnit) then
    begin
      DbgPrintf('');
      DbgPrintf('');
      DbgPrintf('  PushBuffer@0x%.08X...', [pdwPushData]);
      DbgPrintf('');
    end;

    bShowPB := true;
  end;
{$endif}

  (* Dxbx note : Do not initialize these 'static' var's :
  pIndexBuffer := nil;
  pVertexBuffer := nil;

  maxIBSize := 0;
  *)

  while(true) do
  begin
    bInc := (pdwPushData^ and D3DPUSH_NOINCREMENT_FLAG) > 0;
    dwMethod := (pdwPushData^ and D3DPUSH_METHOD_MASK);
    dwCount := ((pdwPushData^ and (not D3DPUSH_NOINCREMENT_FLAG)) shr D3DPUSH_COUNT_SHIFT);
    Inc(pdwPushData);

    if MayLog(lfUnit) then
      DbgPrintf('  Method: 0x%.08X      Count: 0x%.08X', [dwMethod, dwCount]);

    // Interpret GPU Instruction
    if (dwMethod = D3DPUSH_SET_BEGIN_END) then
    begin
{$ifdef _DEBUG_TRACK_PB}
      if (bShowPB) then
      begin
        DbgPrintf('  NVPB_SetBeginEnd(');
      end;
{$endif}

      if (pdwPushData^ = 0) then
      begin
{$ifdef _DEBUG_TRACK_PB}
        if (bShowPB) then
        begin
          DbgPrintf('DONE)');
        end;
{$endif}
        break;  // done?
      end
      else
      begin
{$IFDEF _DEBUG_TRACK_PB}
        if (bShowPB) then
        begin
          DbgPrintf('PrimitiveType := %d)', [pdwPushData^]);
        end;
{$endif}

        XBPrimitiveType := X_D3DPRIMITIVETYPE(pdwPushData^);
        PCPrimitiveType := EmuPrimitiveType(XBPrimitiveType);
      end;

      Inc(pdwPushData);
    end
    else if (dwMethod = D3DPUSH_INLINE_ARRAY) then
    begin
      pVertexData := pdwPushData;

      Inc(pdwPushData, dwCount);

      // retrieve vertex shader
{$IFDEF DXBX_USE_D3D9}
      // For Direct3D9, try to retrieve the vertex shader interface :
      dwVertexShader := 0;
      g_pD3DDevice.GetVertexShader({out}PIDirect3DVertexShader9(@dwVertexShader));
      // If that didn't work, get the active FVF :
      if dwVertexShader = 0 then
        g_pD3DDevice.GetFVF({out}dwVertexShader);
{$ELSE}
      g_pD3DDevice.GetVertexShader({out}dwVertexShader);
{$ENDIF}

      if (dwVertexShader > $FFFF) then
      begin
        DxbxKrnlCleanup('Non-FVF Vertex Shaders not yet supported for PushBuffer emulation!');
        dwVertexShader := 0;
      end
      else if (dwVertexShader = 0) then
      begin
        EmuWarning('FVF Vertex Shader is null');
        dwVertexShader := DWORD(-1);
      end;

      //
      // calculate stride
      //

      dwStride := 0;
      if (VshHandleIsFVF(dwVertexShader)) then
      begin
        dwStride := DxbxFVFToVertexSizeInBytes(dwVertexShader, {IncludeTextures=}True);
      end;

      (* MARKED OUT BY CXBX
      // create cached vertex buffer only once, with maxed out size
      if (pVertexBuffer = nil) then
      begin
        hRet := g_pD3DDevice.CreateVertexBuffer(2047*SizeOf(DWORD), D3DUSAGE_WRITEONLY, dwVertexShader, D3DPOOL_MANAGED, @pVertexBuffer);

        if (FAILED(hRet)) then
          DxbxKrnlCleanup('Unable to create vertex buffer cache for PushBuffer emulation ($1818, dwCount : %d)', [dwCount]);

      end;

      // copy vertex data
      begin
        pData: Puint8 := nil;

        hRet := pVertexBuffer.Lock(0, dwCount*4, @pData, 0);

        if (FAILED(hRet)) then
          DxbxKrnlCleanup('Unable to lock vertex buffer cache for PushBuffer emulation ($1818, dwCount : %d)', [dwCount]);

        memcpy(pData, pVertexData, dwCount*4);

        pVertexBuffer.Unlock();
      end;
      *)

{$ifdef _DEBUG_TRACK_PB}
      if (bShowPB) then
      begin
        DbgPrintf('NVPB_InlineVertexArray(...)');
        DbgPrintf('  dwCount : %d', [dwCount]);
        DbgPrintf('  dwVertexShader : 0x%08X', [dwVertexShader]);
      end;
{$endif}

      EmuUnswizzleActiveTexture();

      // render vertices
      if (dwVertexShader <> DWord(-1)) then
      begin
        VertexCount := (dwCount*sizeof(DWORD)) div dwStride;
        PrimitiveCount := EmuD3DVertex2PrimitiveCount(XBPrimitiveType, VertexCount);

        VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer

        VPDesc.dwVertexCount := VertexCount;
        VPDesc.PrimitiveType := XBPrimitiveType;
        VPDesc.dwPrimitiveCount := PrimitiveCount;
        VPDesc.dwOffset := 0;
        VPDesc.pVertexStreamZeroData := pVertexData;
        VPDesc.uiVertexStreamZeroStride := dwStride;
        VPDesc.hVertexShader := dwVertexShader;

        VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

        {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

        g_pD3DDevice.DrawPrimitiveUP
        (
            PCPrimitiveType, // Dxbx : Why not this : EmuPrimitiveType(VPDesc.PrimitiveType),
            VPDesc.dwPrimitiveCount,
            VPDesc.pVertexStreamZeroData,
            VPDesc.uiVertexStreamZeroStride
        );

        VertPatch.Restore();
      end;
    end
    else if (dwMethod = NVPB_FixLoop) then
    begin
{$ifdef _DEBUG_TRACK_PB}
      if (bShowPB) then
      begin
        DbgPrintf('  NVPB_FixLoop(%d)', [dwCount]);
        DbgPrintf('');
        DbgPrintf('  Index Array Data...');

        pwVal := PWORDs(pdwPushData); // TODO -oDXBX: Do older Delphi's add 4 bytes too?

        if dwCount > 0 then // Dxbx addition, to prevent underflow
        for s := 0 to dwCount - 1 do
        begin
          if (s mod 8 = 0) then printf(#13#10'  ');

          printf('  %.04X', [pwVal[s]]);
        end;

        printf(#13#10);
        DbgPrintf('');
      end;
{$endif}

      pwVal := PWORDs(pdwPushData); // TODO -oDXBX: Do older Delphi's add 4 bytes too?
      if dwCount > 0 then // Dxbx addition, to prevent underflow
      for mi := 0 to dwCount - 1 do
      begin
        pIBMem[mi+2] := pwVal[mi];
      end;

      // perform rendering
      if (pIBMem[0] <> $FFFF) then
      begin
        // TODO -oCXBX: depreciate maxIBSize after N milliseconds..then N milliseconds later drop down to new highest
        if ((dwCount*2 + 2*2) > maxIBSize) then
        begin
          if (pIndexBuffer <> nil) then
          begin
            IDirect3DIndexBuffer(pIndexBuffer)._Release();
            pIndexBuffer := nil; // Dxbx addition - nil out after decreasing reference count
          end;

          hRet := IDirect3DDevice_CreateIndexBuffer(g_pD3DDevice, dwCount*2 + 2*2, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, PIDirect3DIndexBuffer(@pIndexBuffer));

          maxIBSize := dwCount*2 + 2*2;
        end
        else
        begin
          hRet := D3D_OK;
        end;

        if (FAILED(hRet)) then
          DxbxKrnlCleanup('Unable to create index buffer for PushBuffer emulation ($1808, dwCount : %d)', [dwCount]);

        // copy index data
        begin
          pData := nil;

          IDirect3DIndexBuffer(pIndexBuffer).Lock(0, dwCount*2 + 2*2, {out}TLockData(pData), 0);

          memcpy(pData, @pIBMem[0], dwCount*2 + 2*2);

          IDirect3DIndexBuffer(pIndexBuffer).Unlock();
        end;

        // render indexed vertices
        begin
          PrimitiveCount := EmuD3DVertex2PrimitiveCount(XBPrimitiveType, dwCount + 2);
          VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer

          VPDesc.dwVertexCount := dwCount;
          VPDesc.PrimitiveType := XBPrimitiveType;
          VPDesc.dwPrimitiveCount := PrimitiveCount;
          VPDesc.dwOffset := 0;
          VPDesc.pVertexStreamZeroData := nil;
          VPDesc.uiVertexStreamZeroStride := 0;
          // TODO -oCXBX: Set the current shader and let the patcher handle it..
          VPDesc.hVertexShader := g_CurrentVertexShader;

          VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

          {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

          g_pD3DDevice.SetIndices(IDirect3DIndexBuffer(pIndexBuffer){$IFDEF DXBX_USE_D3D9}{$MESSAGE 'fixme'}{$ELSE}, 0{$ENDIF});

{$ifdef _DEBUG_TRACK_PB}
          if ( not g_PBTrackDisable.exists(pdwOrigPushData)) then
{$endif}
          begin
            if (not g_bPBSkipPusher) then
            begin
              if (IsValidCurrentShader()) then
              begin
                g_pD3DDevice.DrawIndexedPrimitive
                (
                    PCPrimitiveType,
  {$IFDEF DXBX_USE_D3D9}
                    {BaseVertexIndex=}0,
  {$ENDIF}
                    {MinVertexIndex=}0,
                    8*1024*1024,
                    0,
                    PrimitiveCount
                    // Dxbx : Why not this : EmuPrimitiveType(VPDesc.PrimitiveType), 0, VPDesc.dwVertexCount, 0, VPDesc.dwPrimitiveCount
                );
              end;
            end;
          end;

          VertPatch.Restore();

          g_pD3DDevice.SetIndices(nil{$IFDEF DXBX_USE_D3D9}{$MESSAGE 'fixme'}{$ELSE}, 0{$ENDIF});
        end;
      end;

      Inc(pdwPushData, dwCount);
    end
    else if (dwMethod = NVPB_InlineIndexArray) then
    begin
      if bInc then
        dwCount := dwCount * 2 + 2;

      pIndexData := pdwPushData;

{$ifdef _DEBUG_TRACK_PB}
      if (bShowPB) then
      begin
        DbgPrintf('  NVPB_InlineIndexArray(0x%.08X, %d)...', [pIndexData, dwCount]);
        DbgPrintf('');
        DbgPrintf('  Index Array Data...');

        pwVal := PWORDs(pIndexData);

        if dwCount > 0 then // Dxbx addition, to prevent underflow
        for s := 0 to dwCount - 1 do
        begin
          if (s mod 8) = 0 then printf(#13#10'  ');

          printf('  %.04X', [pwVal[s]]);
        end;

        printf(#13#10);

        pActiveVB := nil;

        pVBData := nil;

        // retrieve stream data
        g_pD3DDevice.GetStreamSource(
          0,
          @pActiveVB,
{$IFDEF DXBX_USE_D3D9}
          {out}uiOffsetInBytes,
{$ENDIF}
          {out}uiStride);

        // retrieve stream desc
        IDirect3DVertexBuffer(pActiveVB).GetDesc({out}VBDesc);

        // unlock just in case
        IDirect3DVertexBuffer(pActiveVB).Unlock();

        // grab ptr
        IDirect3DVertexBuffer(pActiveVB).Lock(0, 0, {out}TLockData(pVBData), D3DLOCK_READONLY);

        // print out stream data
        begin
          if MayLog(lfUnit) then
          begin
            DbgPrintf('');
            DbgPrintf('  Vertex Stream Data (0x%.08X)...', [pActiveVB]);
            DbgPrintf('');
            DbgPrintf('  Format : %d', [Ord(VBDesc.Format)]);
            DbgPrintf('  Size   : %d bytes', [VBDesc.Size]);
            DbgPrintf('  FVF    : 0x%.08X', [VBDesc.FVF]);
            DbgPrintf('');
          end;
        end;

        // release ptr
        IDirect3DVertexBuffer(pActiveVB).Unlock();

        DbgDumpMesh(PWORD(pIndexData), dwCount);
      end;
{$endif}

      Inc(pdwPushData, (dwCount div 2) - DWord(iif(bInc, 0, 2)));

      // perform rendering
      begin
        // TODO -oCXBX: depreciate maxIBSize after N milliseconds..then N milliseconds later drop down to new highest
        if (dwCount*2 > maxIBSize) then
        begin
          if (pIndexBuffer <> nil) then
          begin
            IDirect3DIndexBuffer(pIndexBuffer)._Release();
            pIndexBuffer := nil; // Dxbx addition - nil out after decreasing reference count
          end;

          hRet := IDirect3DDevice_CreateIndexBuffer(g_pD3DDevice, dwCount*2, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, PIDirect3DIndexBuffer(@pIndexBuffer));

          maxIBSize := dwCount*2;
        end
        else
        begin
          hRet := D3D_OK;
        end;

        if (FAILED(hRet)) then
          DxbxKrnlCleanup('Unable to create index buffer for PushBuffer emulation ($1800, dwCount : %d)', [dwCount]);

        // copy index data
        begin
          pData := nil;

          IDirect3DIndexBuffer(pIndexBuffer).Lock(0, dwCount*2, {out}TLockData(pData), 0);

          memcpy(pData, pIndexData, dwCount*2);

          // remember last 2 indices
          if (dwCount >= 2) then
          begin
            pIBMem[0] := pData[dwCount - 2];
            pIBMem[1] := pData[dwCount - 1];
          end
          else
          begin
            pIBMem[0] := $FFFF;
          end;

          IDirect3DIndexBuffer(pIndexBuffer).Unlock();
        end;

        // render indexed vertices
        begin
          PrimitiveCount := EmuD3DVertex2PrimitiveCount(XBPrimitiveType, dwCount);
          VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer

          VPDesc.dwVertexCount := dwCount;
          VPDesc.PrimitiveType := XBPrimitiveType;
          VPDesc.dwPrimitiveCount := PrimitiveCount;
          VPDesc.dwOffset := 0;
          VPDesc.pVertexStreamZeroData := nil;
          VPDesc.uiVertexStreamZeroStride := 0;
          // TODO -oCXBX: Set the current shader and let the patcher handle it..
          VPDesc.hVertexShader := g_CurrentVertexShader;

          VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

          {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

          g_pD3DDevice.SetIndices(IDirect3DIndexBuffer(pIndexBuffer){$IFDEF DXBX_USE_D3D9}{$MESSAGE 'fixme'}{$ELSE}, 0{$ENDIF});

{$ifdef _DEBUG_TRACK_PB}
          if (not g_PBTrackDisable.exists(pdwOrigPushData)) then
{$endif}
          begin
            if (not g_bPBSkipPusher) and IsValidCurrentShader() then
            begin
              g_pD3DDevice.DrawIndexedPrimitive
              (
                  PCPrimitiveType,
{$IFDEF DXBX_USE_D3D9}
                  {BaseVertexIndex=}0,
{$ENDIF}
                  {MinVertexIndex=}0,
                  (*dwCount*2*)8*1024*1024,
                  0,
                  PrimitiveCount
                  // Dxbx : Why not this : EmuPrimitiveType(VPDesc.PrimitiveType), 0, VPDesc.dwVertexCount, 0, VPDesc.dwPrimitiveCount
              );
            end;
          end;

          VertPatch.Restore();

          g_pD3DDevice.SetIndices(nil{$IFDEF DXBX_USE_D3D9}{$MESSAGE 'fixme'}{$ELSE}, 0{$ENDIF});
        end;
      end;
    end
    else
    begin
      EmuWarning('Unknown PushBuffer Operation (0x%.04X, %d)', [dwMethod, dwCount]);
      Exit;
    end;
  end;

{$ifdef _DEBUG_TRACK_PB}
  if (bShowPB) then
  begin
    DbgPrintf('');
    DbgPrintf('DxbxDbg> ');
    fflush(stdout);
  end;
{$endif}

  if (XTL_g_bStepPush) then
  begin
    DxbxPresent(nil, nil, 0, nil);
    Sleep(500);
  end;
end;


{$IFDEF _DEBUG_TRACK_PB}

procedure DbgDumpMesh(pIndexData: PWORD; dwCount: DWORD); {NOPATCH}
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pActiveVB: XTL_PIDirect3DVertexBuffer8;
  VBDesc: D3DVERTEXBUFFER_DESC;
  pVBData: PBYTE;
{$IFDEF DXBX_USE_D3D9}
  uiOffsetInBytes: UINT;
{$ENDIF}
  uiStride: UINT;
  szFileName: array [0..128 - 1] of AnsiChar;
  pwVal: PWORD;
  maxIndex: uint32;
  pwChk: PWORD;
  chk: uint;
  x: DWORD;
  dbgVertices: PFILE;
  max: uint;
  v: uint;
  a: DWORD;
  b: DWORD;
  c: DWORD;
//  la, lb, lc: DWORD;
  i: uint;
begin
  if (not IsValidCurrentShader() or (dwCount = 0)) then
    Exit;

  pActiveVB := NULL;

  pVBData := nil;
  
  // retrieve stream data
  g_pD3DDevice.GetStreamSource(
    0,
    @pActiveVB,
{$IFDEF DXBX_USE_D3D9}
    {out}uiOffsetInBytes,
{$ENDIF}
    {out}uiStride);

  sprintf(@szFileName[0], AnsiString(DxbxDebugFolder +'\DxbxMesh-0x%.08X.x'), [pIndexData]);
  dbgVertices := fopen(szFileName, 'wt');

  // retrieve stream desc
  IDirect3DVertexBuffer(pActiveVB).GetDesc({out}VBDesc);

  // unlock just in case
  IDirect3DVertexBuffer(pActiveVB).Unlock();

  // grab ptr
  IDirect3DVertexBuffer(pActiveVB).Lock(0, 0, {out}TLockData(pVBData), D3DLOCK_READONLY);

  // print out stream data
  if Assigned(dbgVertices) then // Dxbx addition
  begin
    maxIndex := 0;

    pwChk := PWORD(pIndexData);

    if dwCount > 0 then // Dxbx addition, to prevent underflow
    for chk := 0 to dwCount - 1 do
    begin
      x := pwChk^; Inc(pwChk);

      if (x > maxIndex) then
        maxIndex := x;
    end;

    if (maxIndex > ((VBDesc.Size div uiStride) - 1)) then
      maxIndex := (VBDesc.Size div uiStride) - 1;

    fprintf(dbgVertices, 'xof 0303txt 0032'#13#10);
    fprintf(dbgVertices, ''#13#10);
    fprintf(dbgVertices, '//'#13#10);
    fprintf(dbgVertices, '//  Vertex Stream Data (0x%.08X)...'#13#10, [pActiveVB]);
    fprintf(dbgVertices, '//'#13#10);
    fprintf(dbgVertices, '//  Format : %d'#13#10, [Ord(VBDesc.Format)]);
    fprintf(dbgVertices, '//  Size   : %d bytes'#13#10, [VBDesc.Size]);
    fprintf(dbgVertices, '//  FVF    : 0x%.08X'#13#10, [VBDesc.FVF]);
    fprintf(dbgVertices, '//  iCount : %d'#13#10, [dwCount div 2]);
    fprintf(dbgVertices, '//'#13#10);
    fprintf(dbgVertices, ''#13#10);
    fprintf(dbgVertices, 'Frame SCENE_ROOT {#13#10');
    fprintf(dbgVertices, ''#13#10);
    fprintf(dbgVertices, '  FrameTransformMatrix {#13#10');
    fprintf(dbgVertices, '    1.000000,0.000000,0.000000,0.000000,'#13#10);
    fprintf(dbgVertices, '    0.000000,1.000000,0.000000,0.000000,'#13#10);
    fprintf(dbgVertices, '    0.000000,0.000000,1.000000,0.000000,'#13#10);
    fprintf(dbgVertices, '    0.000000,0.000000,0.000000,1.000000;'#13#10);
    fprintf(dbgVertices, '  }'#13#10);
    fprintf(dbgVertices, ''#13#10);
    fprintf(dbgVertices, '  Frame Turok1 {'#13#10);
    fprintf(dbgVertices, ''#13#10);
    fprintf(dbgVertices, '    FrameTransformMatrix {'#13#10);
    fprintf(dbgVertices, '      1.000000,0.000000,0.000000,0.000000,'#13#10);
    fprintf(dbgVertices, '      0.000000,1.000000,0.000000,0.000000,'#13#10);
    fprintf(dbgVertices, '      0.000000,0.000000,1.000000,0.000000,'#13#10);
    fprintf(dbgVertices, '      0.000000,0.000000,0.000000,1.000000;'#13#10);
    fprintf(dbgVertices, '    }#13#10');
    fprintf(dbgVertices, ''#13#10);
    fprintf(dbgVertices, '    Mesh {'#13#10);
    fprintf(dbgVertices, '      %d;#13#10', [maxIndex + 1]);

    max := maxIndex + 1;
    for v := 0 to max -1 do
    begin
      fprintf(dbgVertices, '      %f;%f;%f;%s'#13#10, [
        PFLOAT(@pVBData[v * uiStride + 0])^,
        PFLOAT(@pVBData[v * uiStride + 4])^,
        PFLOAT(@pVBData[v * uiStride + 8])^,
        iif(v < (max - 1), ',', ';')]);
    end;

    fprintf(dbgVertices, '      %d;'#13#10, [dwCount - 2]);

    pwVal := PWORD(pIndexData);

    max := dwCount;

    a := pwVal^; Inc(pwVal);
    b := pwVal^; Inc(pwVal);
    c := pwVal^; Inc(pwVal);

//    la := a; lb := b; lc := c;

    if max > 0 then // Dxbx addition, to prevent underflow
    for i := 2 to max - 1 do
    begin
      fprintf(dbgVertices, '      3;%d,%d,%d;%s'#13#10,
        [a, b, c, iif(i < (max - 1), ',', ';')]);

      a := b;
      b := c;
      c := pwVal^; Inc(pwVal);

//      la := a;
//      lb := b;
//      lc := c;
    end;

    fprintf(dbgVertices, '    }'#13#10);
    fprintf(dbgVertices, '  }'#13#10);
    fprintf(dbgVertices, '}'#13#10);

    fclose(dbgVertices);
  end;

  // release ptr
  IDirect3DVertexBuffer(pActiveVB).Unlock();
end;
{$ENDIF}

end.

