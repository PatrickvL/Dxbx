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

{$INCLUDE ..\..\Dxbx.inc}

interface

uses
  // Delphi
  Windows
  , Direct3D8
  // Dxbx
  , uEmuD3D8Types;

var
  g_dwPrimaryPBCount: LongInt = 0;
  g_pPrimaryPB: LongInt = 0;
  XTL_g_bStepPush: Boolean = False;
  XTL_g_bSkipPush: Boolean = False;
  XTL_g_bBrkPush: Boolean = False;
  g_bPBSkipPusher: Boolean = False;

procedure XTL_EmuExecutePushBufferRaw(pdwPushData: DWord); stdcall; // forward
procedure XTL_EmuExecutePushBuffer(pPushBuffer: PX_D3DPushBuffer; pFixup: PX_D3DFixup); stdcall;

implementation

uses
  // Dxbx
  uDxbxKrnlUtils
  , uLog
  , uState
  , uEmuXTL
  , uEmuD3D8
  , JwaWinType
  , uVertexShader
  , uConvert;


procedure XTL_EmuExecutePushBuffer(pPushBuffer: PX_D3DPushBuffer; pFixup: PX_D3DFixup); stdcall;
// Branch:martin  Revision:100  Translator:Shadow_Tj
begin
  if Assigned(pFixup) then
    CxbxKrnlCleanup('PushBuffer has fixups');
    
  XTL_EmuExecutePushBufferRaw(pPushBuffer.Data); 
end;

procedure EmuUnswizzleActiveTexture();
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0

var
  pPixelContainer: X_D3DPixelContainer;
  XBFormat: DWord;
  dwBPP: DWord;

begin
    // for current usages, we're always on stage 0
    (*pPixelContainer := EmuD3DActiveTexture[0];

    if(pPixelContainer = 0 or  not (pPixelContainer.Common and X_D3DCOMMON_ISLOCKED)) then
        Exit;

    XBFormat := (pPixelContainer.Format and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT;
    dwBPP := 0;

    if( not XTL_EmuXBFormatIsSwizzled(XBFormat, @dwBPP)) then
        Exit;

    // remove lock
    pPixelContainer.EmuTexture8.UnlockRect(0);
    pPixelContainer.Common := pPixelContainer.Common and ~X_D3DCOMMON_ISLOCKED;

    // Cxbx TODO: potentially CRC to see if this surface was actually modified..

    //
    // unswizzle texture
    //

    begin
        XTL.IDirect3DTexture8 *pTexture := pPixelContainer.EmuTexture8;

        DWord dwLevelCount := pTexture.GetLevelCount();

        for(uint32 v:=0;v<dwLevelCount;v++)
        begin
            XTL.D3DSURFACE_DESC SurfaceDesc;

            HRESULT hRet := pTexture.GetLevelDesc(v, @SurfaceDesc);

            if(FAILED(hRet)) then
                Continue;

            //
            // perform unswizzle
            //

            begin
                XTL.D3DLOCKED_RECT LockedRect;

                //if(SurfaceDesc.Format != XTL::D3DFMT_A8R8G8B8)
                //    Break;
                //CxbxKrnlCleanup('Temporarily unsupported format for active texture unswizzle (0x%.08X)', SurfaceDesc.Format);

                hRet := pTexture.LockRect(v, @LockedRect, 0, 0);

                if(FAILED(hRet)) then
                    Continue;

                DWord dwWidth := SurfaceDesc.Width;
                DWord dwHeight := SurfaceDesc.Height;
                DWord dwDepth := 1;
                DWord dwPitch := LockedRect.Pitch;
                TRect  iRect := (0,0,0,0);
                TPoint iPoint := (0,0);

                Pointer pTemp := malloc(dwHeight*dwPitch);

                XTL.EmuXGUnswizzleRect
                (
                    LockedRect.pBits, dwWidth, dwHeight, dwDepth,
                    pTemp, dwPitch, iRect, iPoint, dwBPP
                );

                memcpy(LockedRect.pBits, pTemp, dwPitch*dwHeight);

                pTexture.UnlockRect(0);

                free(pTemp);
             end;
         end;

        DbgPrintf('Active texture was unswizzled');
     end;
     *)
end;

procedure XTL_EmuExecutePushBufferRaw(pdwPushData: DWord); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
(*var
  pdwOrigPushData: DWord;
  pIndexData : PVOID;
  pVertexData : PVOID;

  dwVertexShader : DWord;
  dwStride : DWord;
  pIBMem : Array [0..3] of WORD;
  bShowPB : bool;

  PCPrimitiveType : D3DPRIMITIVETYPE;
  XBPrimitiveType : X_D3DPRIMITIVETYPE;

  pIndexBuffer : IDIRECT3DINDEXBUFFER8;
  pVertexBuffer : IDIRECT3DVERTEXBUFFER8;
  maxIBSize : uint;

  dwCount : DWord;
  dwMethod : DWord;

  bInc : BOOL; *)

begin
  (*if XTL_g_bSkipPush then
    Exit;

    pdwOrigPushData := pdwPushData;

    pIndexData := Nil;
    pVertexData := Nil;

    dwVertexShader := -1;
    dwStride := -1;

    // cache of last 4 indices
    pIBMem[3] := [$FFFF, $FFFF, $FFFF, $FFFF];

    PCPrimitiveType := D3DPRIMITIVETYPE(-1);
    XBPrimitiveType := X_D3DPT_INVALID; 

    // Cxbx TODO: This technically should be enabled
    XTL_EmuUpdateDeferredStates();

    {$ifdef _DEBUG_TRACK_PB}
      bShowPB := False;
      g_PBTrackTotal.insert(pdwPushData);

      if(g_PBTrackShowOnce.exists(pdwPushData)) then
      begin
          g_PBTrackShowOnce.remove(pdwPushData);

          DbgPrintf('');
          DbgPrintf('');
          DbgPrintf('  PushBuffer@$%.08X...', [pdwPushData]);
          DbgPrintf('');

          bShowPB := True;
       end;
    {$endif}

     pIndexBuffer  := Nil;
     pVertexBuffer := Nil;
     maxIBSize     := 0;

    while True do
    begin
        dwCount := (pdwPushData shr 18);
        dwMethod := (pdwPushData and $3FFFF);

        // Interpret GPU Instruction
        if(dwMethod = $000017FC) then  // NVPB_SetBeginEnd
        begin
            pdwPushData:= pdwPushData + 1;

            {$ifdef _DEBUG_TRACK_PB}
            if(bShowPB) then
            begin
                DbgPrintf('  NVPB_SetBeginEnd(');
             end;
            {$endif}

            if(pdwPushData = 0) then
            begin
                {$ifdef _DEBUG_TRACK_PB}
                if(bShowPB) then
                begin
                    DbgPrintf('DONE)');
                 end;
                {$endif}
                Break;  // done?
             end
            else
            begin
                {$ifdef _DEBUG_TRACK_PB}
                if(bShowPB) then
                begin
                    DbgPrintf('PrimitiveType := %d)', *pdwPushData);
                 end;
                {$endif}

                XBPrimitiveType := (X_D3DPRIMITIVETYPE)*pdwPushData;
                PCPrimitiveType := EmuPrimitiveType(XBPrimitiveType); 
             end;
         end
        else if(dwMethod = $1818) then  // NVPB_InlineVertexArray
        begin
            bInc := *pdwPushData and $40000000;

            if(bInc) then
            begin
                dwCount := (pdwPushData - ($40000000 or $00001818)) shr 18;
             end;

            pVertexData := ++pdwPushData;

            pdwPushData:= pdwPushData + dwCount;

            // retrieve vertex shader
            g_pD3DDevice8.GetVertexShader(@dwVertexShader);

            if(dwVertexShader > $FFFF) then
            begin
                CxbxKrnlCleanup('Non-FVF Vertex Shaders not yet supported for PushBuffer emulation!');
                dwVertexShader := 0;
             end
            else if(dwVertexShader = 0) then
            begin
                EmuWarning('FVF Vertex Shader is null');
                dwVertexShader := -1;
             end;

            //
            // calculate stride
            //

            dwStride := 0;

            if( not VshHandleIsVertexShader(dwVertexShader)) then
            begin
                if(dwVertexShader and D3DFVF_XYZRHW) begin  dwStride:= dwStride + SizeOf(FLOAT) then *4;  end;
                if(dwVertexShader and D3DFVF_DIFFUSE) begin  dwStride:= dwStride + SizeOf(DWord) then ;  end;
                if(dwVertexShader and D3DFVF_SPECULAR) begin  dwStride:= dwStride + SizeOf(DWord) then ;  end;

                dwStride:= dwStride + ((dwVertexShader and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT)*SizeOf(FLOAT)*2;
             end;

            {
            // create cached vertex buffer only once, with maxed out size
            if(pVertexBuffer = 0) then
            begin
                HRESULT hRet := g_pD3DDevice8.CreateVertexBuffer(2047*SizeOf(DWord), D3DUSAGE_WRITEONLY, dwVertexShader, D3DPOOL_MANAGED, @pVertexBuffer);

                if(FAILED(hRet)) then
                    CxbxKrnlCleanup('Unable to create vertex buffer cache for PushBuffer emulation ($1818, dwCount : %d)', dwCount);

             end;

            // copy vertex data
            begin
                uint08 *pData := 0;

                HRESULT hRet := pVertexBuffer.Lock(0, dwCount*4, @pData, 0);

                if(FAILED(hRet)) then
                    CxbxKrnlCleanup('Unable to lock vertex buffer cache for PushBuffer emulation ($1818, dwCount : %d)', dwCount);

                memcpy(pData, pVertexData, dwCount*4);

                pVertexBuffer.Unlock();
             end;
            }

            #ifdef _DEBUG_TRACK_PB
            if(bShowPB) then
            begin
                printf('NVPB_InlineVertexArray(...)');
                printf('  dwCount : %d', dwCount);
                printf('  dwVertexShader : $%08X', dwVertexShader);
             end;
            //endif

            EmuUnswizzleActiveTexture();

            // render vertices
            if(dwVertexShader <> -1) then
            begin
                UINT VertexCount := (dwCount*SizeOf(DWord)) / dwStride;
                UINT PrimitiveCount := EmuD3DVertex2PrimitiveCount(XBPrimitiveType, VertexCount);

                VertexPatchDesc VPDesc;

                VPDesc.dwVertexCount := VertexCount;
                VPDesc.PrimitiveType := XBPrimitiveType;
                VPDesc.dwPrimitiveCount := PrimitiveCount;
                VPDesc.dwOffset := 0;
                VPDesc.pVertexStreamZeroData := pVertexData;
                VPDesc.uiVertexStreamZeroStride := dwStride;
                VPDesc.hVertexShader := dwVertexShader;

                VertexPatcher VertPatch;

                bool bPatched := VertPatch.Apply(@VPDesc);

                g_pD3DDevice8.DrawPrimitiveUP
                (
                    PCPrimitiveType,
                    VPDesc.dwPrimitiveCount,
                    VPDesc.pVertexStreamZeroData,
                    VPDesc.uiVertexStreamZeroStride
                );

                VertPatch.Restore();
             end;

            pdwPushData:= pdwPushData - 1;
         end;
        else if(dwMethod = $1808) then  // NVPB_FixLoop
        begin
            #ifdef _DEBUG_TRACK_PB
            if(bShowPB) then
            begin
                printf('  NVPB_FixLoop(%d)', dwCount);
                printf('');
                printf('  Index Array Data...');

                WORD *pwVal := (WORD)(pdwPushData + 1);

                for(uint s:=0;s<dwCount;s++)
                begin
                    if(s%8 := 0) printf('  ') then ;

                    printf('  %.04X', *pwVal++);
                 end;

                printf('');
                printf('');
             end;
            //endif

            WORD *pwVal := (WORD)(pdwPushData + 1);
            for(uint mi:=0;mi<dwCount;mi++)
            begin
                pIBMem[mi+2] := pwVal[mi];
             end;

            // perform rendering
            if(pIBMem[0] <> $FFFF) then
            begin
                HRESULT hRet;

                // Cxbx TODO: depreciate maxIBSize after N milliseconds..then N milliseconds later drop down to new highest
                if((dwCount*2 + 2*2) > maxIBSize) then
                begin
                    if(pIndexBuffer <> 0) then
                    begin
                        pIndexBuffer.Release();
                     end;

                    hRet := g_pD3DDevice8.CreateIndexBuffer(dwCount*2 + 2*2, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, @pIndexBuffer);

                    maxIBSize := dwCount*2 + 2*2;
                 end;
                else
                begin
                    hRet := D3D_OK;
                 end;

                if(FAILED(hRet)) then
                    CxbxKrnlCleanup('Unable to create index buffer for PushBuffer emulation ($1808, dwCount : %d)', dwCount);

                // copy index data
                begin
                    WORD *pData:=0;

                    pIndexBuffer.Lock(0, dwCount*2 + 2*2, (UCHAR)@pData, 0);

                    memcpy(pData, pIBMem, dwCount*2 + 2*2);

                    pIndexBuffer.Unlock();
                 end;

                // render indexed vertices
                begin
                    UINT PrimitiveCount := EmuD3DVertex2PrimitiveCount(XBPrimitiveType, dwCount + 2);
                    VertexPatchDesc VPDesc;

                    VPDesc.dwVertexCount := dwCount;
                    VPDesc.PrimitiveType := XBPrimitiveType;
                    VPDesc.dwPrimitiveCount := PrimitiveCount;
                    VPDesc.dwOffset := 0;
                    VPDesc.pVertexStreamZeroData := 0;
                    VPDesc.uiVertexStreamZeroStride := 0;
                    // Cxbx TODO: Set the current shader and let the patcher handle it..
                    VPDesc.hVertexShader := g_CurrentVertexShader;

                    VertexPatcher VertPatch;

                    bool bPatched := VertPatch.Apply(@VPDesc);

                    g_pD3DDevice8.SetIndices(pIndexBuffer, 0);

                    #ifdef _DEBUG_TRACK_PB
                    if( not g_PBTrackDisable.exists(pdwOrigPushData)) then
                    begin
                    //endif

                    if( not g_bPBSkipPusher) then
                    begin
                        if(IsValidCurrentShader()) then
                        begin
                            g_pD3DDevice8.DrawIndexedPrimitive
                            (
                                PCPrimitiveType, 0, 8*1024*1024, 0, PrimitiveCount
//                                PCPrimitiveType, 0, dwCount*2, 0, PrimitiveCount
                            );
                         end;
                     end;

                    #ifdef _DEBUG_TRACK_PB
                     end;
                    //endif

                    VertPatch.Restore();

                    g_pD3DDevice8.SetIndices(0, 0);
                 end;
             end;

            pdwPushData:= pdwPushData + dwCount;
         end;
        else if(dwMethod = $1800) then  // NVPB_InlineIndexArray
        begin
            BOOL bInc := *pdwPushData and $40000000;

            if(bInc) then
            begin
                dwCount := ((pdwPushData - ($40000000 or $00001818)) shr 18)*2 + 2;
             end;

            pIndexData := ++pdwPushData;

            #ifdef _DEBUG_TRACK_PB
            if(bShowPB) then
            begin
                printf('  NVPB_InlineIndexArray($%.08X, %d)...', pIndexData, dwCount);
                printf('');
                printf('  Index Array Data...');

                WORD *pwVal := (WORD)pIndexData;

                for(uint s:=0;s<dwCount;s++)
                begin
                    if(s%8 := 0) printf('  ') then ;

                    printf('  %.04X', *pwVal++);
                 end;

                printf('');

                XTL.IDirect3DVertexBuffer8 *pActiveVB := 0;

                D3DVERTEXBUFFER_DESC VBDesc;

                BYTE *pVBData := 0;
                UINT  uiStride;

                // retrieve stream data
                g_pD3DDevice8.GetStreamSource(0, @pActiveVB, @uiStride);

                // retrieve stream desc
                pActiveVB.GetDesc(@VBDesc);

                // unlock just in case
                pActiveVB.Unlock();

                // grab ptr
                pActiveVB.Lock(0, 0, @pVBData, D3DLOCK_READONLY);

                // print out stream data
                begin
                    printf('');
                    printf('  Vertex Stream Data ($%.08X)...', pActiveVB);
                    printf('');
                    printf('  Format : %d', VBDesc.Format);
                    printf('  Size   : %d bytes', VBDesc.Size);
                    printf('  FVF    : $%.08X', VBDesc.FVF);
                    printf('');
                 end;

                // release ptr
                pActiveVB.Unlock();

                DbgDumpMesh((WORD)pIndexData, dwCount);
             end;
            //endif

            pdwPushData:= pdwPushData + (dwCount/2) - (bInc ? 0 : 2);

            // perform rendering
            begin
                HRESULT hRet;

                // Cxbx TODO: depreciate maxIBSize after N milliseconds..then N milliseconds later drop down to new highest
                if(dwCount*2 > maxIBSize) then
                begin
                    if(pIndexBuffer <> 0) then
                    begin
                        pIndexBuffer.Release();
                     end;

                    hRet := g_pD3DDevice8.CreateIndexBuffer(dwCount*2, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, @pIndexBuffer);

                    maxIBSize := dwCount*2;
                 end;
                else
                begin
                    hRet := D3D_OK;
                 end;

                if(FAILED(hRet)) then
                    CxbxKrnlCleanup('Unable to create index buffer for PushBuffer emulation ($1800, dwCount : %d)', dwCount);

                // copy index data
                begin
                    WORD *pData:=0;

                    pIndexBuffer.Lock(0, dwCount*2, (UCHAR)@pData, 0);

                    memcpy(pData, pIndexData, dwCount*2);

                    // remember last 2 indices
                    if(dwCount >= 2) then
                    begin
                        pIBMem[0] := pData[dwCount - 2];
                        pIBMem[1] := pData[dwCount - 1];
                     end;
                    else
                    begin
                        pIBMem[0] := $FFFF;
                     end;

                    pIndexBuffer.Unlock();
                 end;

                // render indexed vertices
                begin
                    UINT PrimitiveCount := EmuD3DVertex2PrimitiveCount(XBPrimitiveType, dwCount);
                    VertexPatchDesc VPDesc;

                    VPDesc.dwVertexCount := dwCount;
                    VPDesc.PrimitiveType := XBPrimitiveType;
                    VPDesc.dwPrimitiveCount := PrimitiveCount;
                    VPDesc.dwOffset := 0;
                    VPDesc.pVertexStreamZeroData := 0;
                    VPDesc.uiVertexStreamZeroStride := 0;
                    // Cxbx TODO: Set the current shader and let the patcher handle it..
                    VPDesc.hVertexShader := g_CurrentVertexShader;

                    VertexPatcher VertPatch;

                    bool bPatched := VertPatch.Apply(@VPDesc);

                    g_pD3DDevice8.SetIndices(pIndexBuffer, 0);

                    #ifdef _DEBUG_TRACK_PB
                    if( not g_PBTrackDisable.exists(pdwOrigPushData)) then
                    begin
                    //endif

                    if( not g_bPBSkipPusher and IsValidCurrentShader()) then
                    begin
                        g_pD3DDevice8.DrawIndexedPrimitive
                        (
                            PCPrimitiveType, 0, (*dwCount*2*)(*8*1024*1024, 0, PrimitiveCount     *)(*
                        );
                     end;

                    #ifdef _DEBUG_TRACK_PB
                     end;
                    //endif

                    VertPatch.Restore();

                    g_pD3DDevice8.SetIndices(0, 0);
                 end;
             end;

            pdwPushData:= pdwPushData - 1;
         end;
        else
        begin
            EmuWarning('Unknown PushBuffer Operation ($%.04X, %d)', dwMethod, dwCount);
            Exit;
         end;

        pdwPushData:= pdwPushData + 1;
     end;

    #ifdef _DEBUG_TRACK_PB
    if(bShowPB) then
    begin
        printf('');
        printf('CxbxDbg> ');
        fflush(stdout);
    end;
    //endif

    if(g_bStepPush) then
    begin
        g_pD3DDevice8.Present(0,0,0,0);
        Sleep(500);
    end;
    *)
end;


{$IFDEF _DEBUG_TRACK_PB}

procedure DbgDumpMesh(var pIndexData: Word; dwCount: DWord);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:1
var
  pActiveVB : IDirect3DVertexBuffer8;
  VBDesc : D3DVERTEXBUFFER_DESC;
  pVBData : PBYTE;
  uiStride : UINT;
  szFileName: array[0..128 - 1] of Char;
begin
  if (not XTL_IsValidCurrentShader() or (dwCount = 0)) then
    Exit;

  pActiveVB := Nil;

  // retrieve stream data
  g_pD3DDevice8.GetStreamSource(0, pActiveVB, uiStride);
 (* StrFmt(szFileName, 'C:\CxbxMesh-$%.08X.x', pIndexData);
  file * dbgVertices := FileOpen(szFileName, 'wt');

    // retrieve stream desc
  pActiveVB.GetDesc(@VBDesc);

    // unlock just in case
  pActiveVB.Unlock();

    // grab ptr
  pActiveVB.Lock(0, 0, @pVBData, D3DLOCK_READONLY);

    // print out stream data
  begin
    uint32 maxIndex := 0;

    WORD * pwChk := (WORD)pIndexData;

    for (uint chk := 0; chk < dwCount; chk++)
    begin
      DWord x = * pwChk := * pwChk + 1;

      if (x > maxIndex) then
        maxIndex := x;
    end;

    if (maxIndex > ((VBDesc.Size / uiStride) - 1)) then
      maxIndex := (VBDesc.Size / uiStride) - 1;

    fprintf(dbgVertices, 'xof 0303txt 0032');
    fprintf(dbgVertices, '');
    fprintf(dbgVertices, '//\n');
    fprintf(dbgVertices, '//  Vertex Stream Data (0x%.08X)...\n', pActiveVB);
    fprintf(dbgVertices, '//\n');
    fprintf(dbgVertices, '//  Format : %d\n', VBDesc.Format);
    fprintf(dbgVertices, '//  Size   : %d bytes\n', VBDesc.Size);
    fprintf(dbgVertices, '//  FVF    : 0x%.08X\n', VBDesc.FVF);
    fprintf(dbgVertices, '//  iCount : %d\n', dwCount / 2);
    fprintf(dbgVertices, '//\n');
    fprintf(dbgVertices, '');
    fprintf(dbgVertices, 'Frame SCENE_ROOT begin ');
    fprintf(dbgVertices, '');
    fprintf(dbgVertices, '  FrameTransformMatrix begin ');
    fprintf(dbgVertices, '    1.000000,0.000000,0.000000,0.000000,');
    fprintf(dbgVertices, '    0.000000,1.000000,0.000000,0.000000,');
    fprintf(dbgVertices, '    0.000000,0.000000,1.000000,0.000000,');
    fprintf(dbgVertices, '    0.000000,0.000000,0.000000,1.000000;;');
    fprintf(dbgVertices, '   end;');
    fprintf(dbgVertices, '');
    fprintf(dbgVertices, '  Frame Turok1 begin ');
    fprintf(dbgVertices, '');
    fprintf(dbgVertices, '    FrameTransformMatrix begin ');
    fprintf(dbgVertices, '      1.000000,0.000000,0.000000,0.000000,');
    fprintf(dbgVertices, '      0.000000,1.000000,0.000000,0.000000,');
    fprintf(dbgVertices, '      0.000000,0.000000,1.000000,0.000000,');
    fprintf(dbgVertices, '      0.000000,0.000000,0.000000,1.000000;;');
    fprintf(dbgVertices, '     end;');
    fprintf(dbgVertices, '');
    fprintf(dbgVertices, '    Mesh begin ');
    fprintf(dbgVertices, '      %d;', maxIndex + 1);

    uint max := maxIndex + 1;
    for (uint v := 0; v < max; v++)
    begin
      fprintf(dbgVertices, '      %f;%f;%f;%s',
        * (FLOAT)@pVBData[v * uiStride + 0],
        * (FLOAT)@pVBData[v * uiStride + 4],
        * (FLOAT)@pVBData[v * uiStride + 8],
        (v < (max - 1))? ',': ';');
    end;

    fprintf(dbgVertices, '      %d;', dwCount - 2);

    WORD * pwVal := (WORD)pIndexData;

    max := dwCount;

    DWord a = * pwVal := * pwVal + 1;
    DWord b = * pwVal := * pwVal + 1;
    DWord c = * pwVal := * pwVal + 1;

    DWord la := a, lb = b, lc = c;

    for (uint i := 2; i < max; i++)
    begin
      fprintf(dbgVertices, '      3;%d,%d,%d;%s',
        a, b, c, (i < (max - 1))? ',': ';');

      a := b;
      b := c;
      c = * pwVal := * pwVal + 1;

      la := a;
      lb := b;
      lc := c;
    end;

    fprintf(dbgVertices, '     end;');
    fprintf(dbgVertices, '   end;');
    fprintf(dbgVertices, ' end;');

    FileClose(dbgVertices);
  end;

    // release ptr
  pActiveVB.Unlock();    *)
end;
{$ENDIF}

exports
  XTL_EmuExecutePushBuffer,
  XTL_EmuExecutePushBufferRaw;

end.

