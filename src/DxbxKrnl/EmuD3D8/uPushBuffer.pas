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

implementation

procedure XTL_EmuExecutePushBuffer;
(*(
    X_D3DPushBuffer       *pPushBuffer,
    X_D3DFixup            *pFixup
) *)
begin
(*    if(pFixup <> 0) then
        CxbxKrnlCleanup('PushBuffer has fixups');

    EmuExecutePushBufferRaw((DWORD)pPushBuffer^.Data);

    Exit;
 end;

  procedure EmuUnswizzleActiveTexture();
    // for current usages, we're always on stage 0
    XTL.X_D3DPixelContainer *pPixelContainer := (XTL.X_D3DPixelContainer)XTL.EmuD3DActiveTexture[0];

    if(pPixelContainer = 0 or  not (pPixelContainer^.Common and X_D3DCOMMON_ISLOCKED)) then
        Exit;

    DWORD XBFormat := (pPixelContainer^.Format and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT;
    DWORD dwBPP := 0;

    if( not XTL.EmuXBFormatIsSwizzled(XBFormat, @dwBPP)) then
        Exit;

    // remove lock
    pPixelContainer^.EmuTexture8^.UnlockRect(0);
    pPixelContainer^.Common:= pPixelContainer^.Common and ~X_D3DCOMMON_ISLOCKED;

    // TODO: potentially CRC to see if this surface was actually modified..

    //
    // unswizzle texture
    //

    begin
        XTL.IDirect3DTexture8 *pTexture := pPixelContainer^.EmuTexture8;

        DWORD dwLevelCount := pTexture^.GetLevelCount();

        for(uint32 v:=0;v<dwLevelCount;v++)
        begin
            XTL.D3DSURFACE_DESC SurfaceDesc;

            HRESULT hRet := pTexture^.GetLevelDesc(v, @SurfaceDesc);

            if(FAILED(hRet)) then
                continue;

            //
            // perform unswizzle
            //

            begin
                XTL.D3DLOCKED_RECT LockedRect;

                //if(SurfaceDesc.Format != XTL::D3DFMT_A8R8G8B8)
                //    break;
                //CxbxKrnlCleanup("Temporarily unsupported format for active texture unswizzle (0x%.08X)", SurfaceDesc.Format);

                hRet := pTexture^.LockRect(v, @LockedRect, 0, 0);

                if(FAILED(hRet)) then
                    continue;

                DWORD dwWidth := SurfaceDesc.Width;
                DWORD dwHeight := SurfaceDesc.Height;
                DWORD dwDepth := 1;
                DWORD dwPitch := LockedRect.Pitch;
                TRect  iRect := (0,0,0,0);
                TPoint iPoint := (0,0);

                Pointer pTemp := malloc(dwHeight*dwPitch);

                XTL.EmuXGUnswizzleRect
                (
                    LockedRect.pBits, dwWidth, dwHeight, dwDepth,
                    pTemp, dwPitch, iRect, iPoint, dwBPP
                );

                memcpy(LockedRect.pBits, pTemp, dwPitch*dwHeight);

                pTexture^.UnlockRect(0);

                free(pTemp);
             end;
         end;

        DbgPrintf('Active texture was unswizzled');
     end; *)
 end;

(*procedure XTL_EmuExecutePushBufferRaw; DWORD                 *pdwPushData
)
begin
    if(g_bSkipPush) then
        Exit;

    DWORD *pdwOrigPushData := pdwPushData;

    PVOID pIndexData := 0;
    PVOID pVertexData := 0;

    DWORD dwVertexShader := -1;
    DWORD dwStride := -1;

    // cache of last 4 indices
    WORD pIBMem[4] := ($FFFF, $FFFF, $FFFF, $FFFF);

    D3DPRIMITIVETYPE    PCPrimitiveType := (D3DPRIMITIVETYPE)-1;
    X_D3DPRIMITIVETYPE  XBPrimitiveType := X_D3DPT_INVALID;

    // TODO: This technically should be enabled
    XTL.EmuUpdateDeferredStates();

    #ifdef _DEBUG_TRACK_PB
    bool bShowPB := false;

    g_PBTrackTotal.insert(pdwPushData);

    if(g_PBTrackShowOnce.exists(pdwPushData)) then 
    begin 
        g_PBTrackShowOnce.remove(pdwPushData);

        printf('');
        printf('');
        printf('  PushBuffer@$ mod .08XArgs: array of const', pdwPushData);
        printf('');

        bShowPB := true;
     end;
    //endif

     LPDIRECT3DINDEXBUFFER8  pIndexBuffer:=0;
     LPDIRECT3DVERTEXBUFFER8 pVertexBuffer:=0;

     uint maxIBSize := 0;

    while(true)
    begin 
        DWORD dwCount := (pdwPushData shr 18);
        DWORD dwMethod := (pdwPushData and $3FFFF);

        // Interpret GPU Instruction
        if(dwMethod = $000017FC) then  // NVPB_SetBeginEnd
        begin 
            pdwPushData:= pdwPushData + 1;

            #ifdef _DEBUG_TRACK_PB
            if(bShowPB) then 
            begin 
                printf('  NVPB_SetBeginEnd(');
             end;
            //endif

            if(pdwPushData = 0) then 
            begin 
                #ifdef _DEBUG_TRACK_PB
                if(bShowPB) then 
                begin 
                    printf('DONE)');
                 end;
                //endif
                break;  // done?
             end;
            else
            begin 
                #ifdef _DEBUG_TRACK_PB
                if(bShowPB) then 
                begin 
                    printf('PrimitiveType :=  mod d)', *pdwPushData);
                 end;
                //endif

                XBPrimitiveType := (X_D3DPRIMITIVETYPE)*pdwPushData;
                PCPrimitiveType := EmuPrimitiveType(XBPrimitiveType);
             end;
         end;
        else if(dwMethod = $1818) then  // NVPB_InlineVertexArray
        begin 
            BOOL bInc := *pdwPushData and $40000000;

            if(bInc) then 
            begin 
                dwCount := (pdwPushData - ($40000000 or $00001818)) shr 18;
             end;

            pVertexData := ++pdwPushData;

            pdwPushData:= pdwPushData + dwCount;

            // retrieve vertex shader
            g_pD3DDevice8^.GetVertexShader(@dwVertexShader);

            if(dwVertexShader > $FFFF) then 
            begin 
                CxbxKrnlCleanup('Non-FVF Vertex Shaders not yet supported for PushBuffer emulation not ');
                dwVertexShader := 0;
             end;
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
                if(dwVertexShader and D3DFVF_DIFFUSE) begin  dwStride:= dwStride + SizeOf(DWORD) then ;  end;
                if(dwVertexShader and D3DFVF_SPECULAR) begin  dwStride:= dwStride + SizeOf(DWORD) then ;  end;

                dwStride:= dwStride + ((dwVertexShader and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT)*SizeOf(FLOAT)*2;
             end;

            (*
            // create cached vertex buffer only once, with maxed out size
            if(pVertexBuffer = 0) then 
            begin 
                HRESULT hRet := g_pD3DDevice8^.CreateVertexBuffer(2047*SizeOf(DWORD), D3DUSAGE_WRITEONLY, dwVertexShader, D3DPOOL_MANAGED, @pVertexBuffer);

                if(FAILED(hRet)) then 
                    CxbxKrnlCleanup('Unable to create vertex buffer cache for PushBuffer emulation ($1818, dwCount :  mod d)', dwCount);

             end;

            // copy vertex data
            begin 
                uint08 *pData := 0;

                HRESULT hRet := pVertexBuffer^.Lock(0, dwCount*4, @pData, 0);

                if(FAILED(hRet)) then 
                    CxbxKrnlCleanup('Unable to lock vertex buffer cache for PushBuffer emulation ($1818, dwCount :  mod d)', dwCount);

                memcpy(pData, pVertexData, dwCount*4);

                pVertexBuffer^.Unlock();
             end;
            *)

(*            #ifdef _DEBUG_TRACK_PB
            if(bShowPB) then
            begin 
                printf('NVPB_InlineVertexArray(Args: array of const)');
                printf('  dwCount :  mod d', dwCount);
                printf('  dwVertexShader : $ mod 08X', dwVertexShader);
             end;
            //endif

            EmuUnswizzleActiveTexture();

            // render vertices
            if(dwVertexShader <> -1) then 
            begin 
                UINT VertexCount := (dwCount*SizeOf(DWORD)) / dwStride;
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

                g_pD3DDevice8^.DrawPrimitiveUP
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
                printf('  NVPB_FixLoop( mod d)", dwCount);
                printf("");
                printf("  Index Array DataArgs: array of const");

                WORD *pwVal := (WORD)(pdwPushData + 1);

                for(uint s:=0;s<dwCount;s++)
                begin 
                    if(s mod 8 := 0) printf("  ") then ;

                    printf("   mod .04X", *pwVal++);
                 end;

                printf("");
                printf("");
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

                // TODO: depreciate maxIBSize after N milliseconds..then N milliseconds later drop down to new highest
                if((dwCount*2 + 2*2) > maxIBSize) then 
                begin 
                    if(pIndexBuffer <> 0) then 
                    begin 
                        pIndexBuffer^.Release();
                     end;

                    hRet := g_pD3DDevice8^.CreateIndexBuffer(dwCount*2 + 2*2, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, @pIndexBuffer);

                    maxIBSize := dwCount*2 + 2*2;
                 end;
                else
                begin 
                    hRet := D3D_OK;
                 end;

                if(FAILED(hRet)) then 
                    CxbxKrnlCleanup("Unable to create index buffer for PushBuffer emulation ($1808, dwCount :  mod d)", dwCount);

                // copy index data
                begin 
                    WORD *pData:=0;

                    pIndexBuffer^.Lock(0, dwCount*2 + 2*2, (UCHAR)@pData, 0);

                    memcpy(pData, pIBMem, dwCount*2 + 2*2);

                    pIndexBuffer^.Unlock();
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
                    // TODO: Set the current shader and let the patcher handle it..
                    VPDesc.hVertexShader := g_CurrentVertexShader;

                    VertexPatcher VertPatch;

                    bool bPatched := VertPatch.Apply(@VPDesc);

                    g_pD3DDevice8^.SetIndices(pIndexBuffer, 0);

                    #ifdef _DEBUG_TRACK_PB
                    if( not g_PBTrackDisable.exists(pdwOrigPushData)) then 
                    begin 
                    //endif

                    if( not g_bPBSkipPusher) then 
                    begin 
                        if(IsValidCurrentShader()) then 
                        begin 
                            g_pD3DDevice8^.DrawIndexedPrimitive
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

                    g_pD3DDevice8^.SetIndices(0, 0);
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
                printf("  NVPB_InlineIndexArray($ mod .08X,  mod d)Args: array of const", pIndexData, dwCount);
                printf("");
                printf("  Index Array DataArgs: array of const");

                WORD *pwVal := (WORD)pIndexData;

                for(uint s:=0;s<dwCount;s++)
                begin 
                    if(s mod 8 := 0) printf("  ") then ;

                    printf("   mod .04X", *pwVal++);
                 end;

                printf("");

                XTL.IDirect3DVertexBuffer8 *pActiveVB := 0;

                D3DVERTEXBUFFER_DESC VBDesc;

                BYTE *pVBData := 0;
                UINT  uiStride;

                // retrieve stream data
                g_pD3DDevice8^.GetStreamSource(0, @pActiveVB, @uiStride);

                // retrieve stream desc
                pActiveVB^.GetDesc(@VBDesc);

                // unlock just in case
                pActiveVB^.Unlock();

                // grab ptr
                pActiveVB^.Lock(0, 0, @pVBData, D3DLOCK_READONLY);

                // print out stream data
                begin 
                    printf("");
                    printf("  Vertex Stream Data ($ mod .08X)Args: array of const", pActiveVB);
                    printf("");
                    printf("  Format :  mod d", VBDesc.Format);
                    printf("  Size   :  mod d bytes", VBDesc.Size);
                    printf("  FVF    : $ mod .08X", VBDesc.FVF);
                    printf("");
                 end;

                // release ptr
                pActiveVB^.Unlock();

                DbgDumpMesh((WORD)pIndexData, dwCount);
             end;
            //endif
          
            pdwPushData:= pdwPushData + (dwCount/2) - (bInc ? 0 : 2);
            
            // perform rendering
            begin 
                HRESULT hRet;

                // TODO: depreciate maxIBSize after N milliseconds..then N milliseconds later drop down to new highest
                if(dwCount*2 > maxIBSize) then 
                begin 
                    if(pIndexBuffer <> 0) then 
                    begin 
                        pIndexBuffer^.Release();
                     end;

                    hRet := g_pD3DDevice8^.CreateIndexBuffer(dwCount*2, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, @pIndexBuffer);

                    maxIBSize := dwCount*2;
                 end;
                else
                begin 
                    hRet := D3D_OK;
                 end;

                if(FAILED(hRet)) then 
                    CxbxKrnlCleanup("Unable to create index buffer for PushBuffer emulation ($1800, dwCount :  mod d)", dwCount);

                // copy index data
                begin 
                    WORD *pData:=0;

                    pIndexBuffer^.Lock(0, dwCount*2, (UCHAR)@pData, 0);

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

                    pIndexBuffer^.Unlock();
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
                    // TODO: Set the current shader and let the patcher handle it..
                    VPDesc.hVertexShader := g_CurrentVertexShader;

                    VertexPatcher VertPatch;

                    bool bPatched := VertPatch.Apply(@VPDesc);

                    g_pD3DDevice8^.SetIndices(pIndexBuffer, 0);

                    #ifdef _DEBUG_TRACK_PB
                    if( not g_PBTrackDisable.exists(pdwOrigPushData)) then 
                    begin 
                    //endif

                    if( not g_bPBSkipPusher and IsValidCurrentShader()) then 
                    begin 
                        g_pD3DDevice8^.DrawIndexedPrimitive
                        (
                            PCPrimitiveType, 0, (*dwCount*2*)(*8*1024*1024, 0, PrimitiveCount
                        );
                     end;

                    #ifdef _DEBUG_TRACK_PB
                     end;
                    //endif

                    VertPatch.Restore();

                    g_pD3DDevice8^.SetIndices(0, 0);
                 end;
             end;

            pdwPushData:= pdwPushData - 1;
         end;
        else
        begin 
            EmuWarning("Unknown PushBuffer Operation ($ mod .04X,  mod d)", dwMethod, dwCount);
            Exit;
         end;

        pdwPushData:= pdwPushData + 1;
     end;

    #ifdef _DEBUG_TRACK_PB
    if(bShowPB) then 
    begin 
        printf("");
        printf("CxbxDbg> ");
        fflush(stdout);
     end;
    //endif

    if(g_bStepPush) then 
    begin 
        g_pD3DDevice8^.Present(0,0,0,0);
        Sleep(500);
     end;
 end;       *)

(*#ifdef _DEBUG_TRACK_PB
procedure DbgDumpMesh(var pIndexData: WORD; dwCount: DWORD);
begin 
    if( not XTL.IsValidCurrentShader() or (dwCount = 0)) then 
        Exit;

    XTL.IDirect3DVertexBuffer8 *pActiveVB := 0;

    XTL.D3DVERTEXBUFFER_DESC VBDesc;

    BYTE *pVBData := 0;
    UINT  uiStride;

    // retrieve stream data
    g_pD3DDevice8^.GetStreamSource(0, @pActiveVB, @uiStride);

     szFileName: array[0..128-1] of Char;
    StrFmt(szFileName, "C:\CxbxMesh-$ mod .08X.x", pIndexData);
    FILE *dbgVertices := FileOpen(szFileName, "wt");

    // retrieve stream desc
    pActiveVB^.GetDesc(@VBDesc);

    // unlock just in case
    pActiveVB^.Unlock();

    // grab ptr
    pActiveVB^.Lock(0, 0, @pVBData, D3DLOCK_READONLY);

    // print out stream data
    begin 
        uint32 maxIndex := 0;

        WORD *pwChk := (WORD)pIndexData;

        for(uint chk:=0;chk<dwCount;chk++)
        begin 
            DWORD x = *pwChk:= *pwChk + 1;

            if(x > maxIndex) then 
                maxIndex := x;
         end;

        if(maxIndex > ((VBDesc.Size/uiStride) - 1)) then 
            maxIndex := (VBDesc.Size / uiStride) - 1;

        fprintf(dbgVertices, "xof 0303txt 0032");
        fprintf(dbgVertices, "");
        fprintf(dbgVertices, "//\n");
        fprintf(dbgVertices, "//  Vertex Stream Data (0x%.08X)...\n", pActiveVB);
        fprintf(dbgVertices, "//\n");
        fprintf(dbgVertices, "//  Format : %d\n", VBDesc.Format);
        fprintf(dbgVertices, "//  Size   : %d bytes\n", VBDesc.Size);
        fprintf(dbgVertices, "//  FVF    : 0x%.08X\n", VBDesc.FVF);
        fprintf(dbgVertices, "//  iCount : %d\n", dwCount/2);
        fprintf(dbgVertices, "//\n");
        fprintf(dbgVertices, "");
        fprintf(dbgVertices, "Frame SCENE_ROOT begin ");
        fprintf(dbgVertices, "");
        fprintf(dbgVertices, "  FrameTransformMatrix begin ");
        fprintf(dbgVertices, "    1.000000,0.000000,0.000000,0.000000,");
        fprintf(dbgVertices, "    0.000000,1.000000,0.000000,0.000000,");
        fprintf(dbgVertices, "    0.000000,0.000000,1.000000,0.000000,");
        fprintf(dbgVertices, "    0.000000,0.000000,0.000000,1.000000;;");
        fprintf(dbgVertices, "   end;");
        fprintf(dbgVertices, "");
        fprintf(dbgVertices, "  Frame Turok1 begin ");
        fprintf(dbgVertices, "");
        fprintf(dbgVertices, "    FrameTransformMatrix begin ");
        fprintf(dbgVertices, "      1.000000,0.000000,0.000000,0.000000,");
        fprintf(dbgVertices, "      0.000000,1.000000,0.000000,0.000000,");
        fprintf(dbgVertices, "      0.000000,0.000000,1.000000,0.000000,");
        fprintf(dbgVertices, "      0.000000,0.000000,0.000000,1.000000;;");
        fprintf(dbgVertices, "     end;");
        fprintf(dbgVertices, "");
        fprintf(dbgVertices, "    Mesh begin ");
        fprintf(dbgVertices, "       mod d;", maxIndex+1);

        uint max := maxIndex+1;
        for(uint v:=0;v<max;v++)
        begin 
            fprintf(dbgVertices, "       mod f; mod f; mod f; mod s", 
                *(FLOAT)@pVBData[v*uiStride+0],
                *(FLOAT)@pVBData[v*uiStride+4],
                *(FLOAT)@pVBData[v*uiStride+8],
                (v < (max - 1)) ? "," : ";");
         end;

        fprintf(dbgVertices, "       mod d;", dwCount - 2);

        WORD *pwVal := (WORD)pIndexData;

        max := dwCount;

        DWORD a = *pwVal:= *pwVal + 1;
        DWORD b = *pwVal:= *pwVal + 1;
        DWORD c = *pwVal:= *pwVal + 1;

        DWORD la := a,lb = b,lc = c;

        for(uint i:=2;i<max;i++)
        begin 
            fprintf(dbgVertices, "      3; mod d, mod d, mod d; mod s",
                a,b,c, (i < (max - 1)) ? "," : ";");

            a := b;
            b := c;
            c = *pwVal:= *pwVal + 1;

            la := a;
            lb := b;
            lc := c;
         end;

        fprintf(dbgVertices, "     end;");
        fprintf(dbgVertices, "   end;");
        fprintf(dbgVertices, " end;");

        FileClose(dbgVertices);
     end;

    // release ptr
    pActiveVB^.Unlock();
 end;
//endif           *)


end.
