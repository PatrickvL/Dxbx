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

unit uVertexBuffer;

{$INCLUDE ..\..\Dxbx.inc}

interface

implementation

const VERTEX_BUFFER_CACHE_SIZE = 64;
const MAX_STREAM_NOT_USED_TIME = (2 * CLOCKS_PER_SEC); // TODO: Trim the not used time

// inline vertex buffer emulation
XTL.DWORD                  *XTL.g_pIVBVertexBuffer := 0;
XTL.X_D3DPRIMITIVETYPE      XTL.g_IVBPrimitiveType := XTL.X_D3DPT_INVALID;
UINT                         XTL.g_IVBTblOffs := 0;
struct XTL::_D3DIVB         *XTL.g_IVBTable := 0;
 DWORD                 XTL.g_IVBFVF := 0;

  crctab: array[0..256-1] of Word;

  procedure CRC32Init;
     boolean bFirstTime := true;
    integer i, j;

    Word crc;

    if( not bFirstTime) then 
    begin 
        Exit;
     end;
    for(i := 0; i < 256; i++)
    begin 
        crc := i shl 24;
        for(j := 0; j < 8; j++)
        begin 
            if(crc and $80000000) then 
                crc := (crc shl 1) ^ $04c11db7;
            else
                crc := crc shl 1;
         end;
        crctab[i] := crc;
     end;
    bFirstTime := false;
 end;

 function CRC32(var data: Byte; len: integer): Word;
begin 
    Word        cresult;
    integer                 i;;
    
    if(len < 4) abort() then ;

    cresult := *data++ shl 24;
    cresult:= cresult or *data++ shl 16;
    cresult:= cresult or *data++ shl 8;
    cresult:= cresult or *data:= *data + 1;
    cresult := ~ cresult;
    len -:=4;
    
    for(i:=0; i<len; i++)
    begin 
        cresult := (cresult shl 8 or *data++) ^ crctab[cresult shr 24];
     end;
    
    result:= ~cresult;
 end;

XTL.VertexPatcher.VertexPatcher()
begin 
    this^.m_uiNbrStreams := 0;
    ZeroMemory(this^.m_pStreams, SizeOf(PATCHEDSTREAM) * MAX_NBR_STREAMS);
    this^.m_bPatched := false;
    this^.m_bAllocatedStreamZeroData := false;
    this^.m_pNewVertexStreamZeroData := 0;
    this^.m_pDynamicPatch := 0;
    CRC32Init();
 end;

XTL.VertexPatcher.~VertexPatcher()
begin 
 end;

procedure XTL.VertexPatcher.DumpCache;
begin 
    printf('--- Dumping streams cache ---');
    RTNode *pNode := g_PatchedStreamsCache.getHead();
    while(pNode)
    begin 
        CACHEDSTREAM *pCachedStream := (CACHEDSTREAM )pNode^.pResource;
        if(pCachedStream) then 
        begin 
            // TODO: Write nicer dump presentation
            printf('Key: $ mod .08X Cache Hits:  mod d IsUP:  mod s OrigStride:  mod d NewStride:  mod d CRCCount:  mod d CRCFreq:  mod d Lengh:  mod d CRC32: $ mod .08X',
                   pNode^.uiKey, pCachedStream^.uiCacheHit, pCachedStream^.bIsUP ? 'YES' : 'NO',
                   pCachedStream^.Stream.uiOrigStride, pCachedStream^.Stream.uiNewStride,
                   pCachedStream^.uiCount, pCachedStream^.uiCheckFrequency,
                   pCachedStream^.uiLength, pCachedStream^.uiCRC32);
         end;

        pNode := pNode^.pNext;
     end;
 end;

procedure XTL.VertexPatcher.CacheStream(var pPatchDesc: VertexPatchDesc;
                                     UINT             uiStream)
begin 
    UINT                       uiStride;
    IDirect3DVertexBuffer8    *pOrigVertexBuffer;
    XTL.D3DVERTEXBUFFER_DESC  Desc;
    procedure                      *pCalculateData;
    uint32                     uiKey;
    UINT                       uiLength;
    CACHEDSTREAM              *pCachedStream := (CACHEDSTREAM )CxbxMalloc(SizeOf(CACHEDSTREAM));

    ZeroMemory(pCachedStream, SizeOf(CACHEDSTREAM));

    // Check if the cache is full, if so, throw away the least used stream
    if(g_PatchedStreamsCache.get_count() > VERTEX_BUFFER_CACHE_SIZE) then 
    begin 
        uint32 uiKey := 0;
        uint32 uiMinHit := $FFFFFFFF;

        RTNode *pNode := g_PatchedStreamsCache.getHead();
        while(pNode)
        begin 
            if(pNode^.pResource) then 
            begin 
                // First, check if there is an "expired" stream in the cache (not recently used)
                if(((CACHEDSTREAM )pNode^.pResource)^.lLastUsed < (clock() + MAX_STREAM_NOT_USED_TIME)) then 
                begin 
                    printf(' not  not  not Found an old stream,  mod 2.2f', ((FLOAT)((clock() + MAX_STREAM_NOT_USED_TIME) - ((CACHEDSTREAM )pNode^.pResource)^.lLastUsed)) / (FLOAT)CLOCKS_PER_SEC);
                    uiKey := pNode^.uiKey;
                    break;
                 end;
                // Find the least used cached stream
                if((uint32)((CACHEDSTREAM )pNode^.pResource)^.uiCacheHit < uiMinHit) then 
                begin 
                    uiMinHit := ((CACHEDSTREAM )pNode^.pResource)^.uiCacheHit;
                    uiKey := pNode^.uiKey;
                 end;
             end;
            pNode := pNode^.pNext;
         end;
        if(uiKey <> 0) then 
        begin 
            printf(' not  not  not Removing stream');
            FreeCachedStream(uiKey);
         end;
     end;

    // Start the actual stream caching
    if( not pPatchDesc^.pVertexStreamZeroData) then 
    begin 
        pOrigVertexBuffer := m_pStreams[uiStream].pOriginalStream;
        pOrigVertexBuffer^.AddRef();
        m_pStreams[uiStream].pPatchedStream^.AddRef();
        if(FAILED(pOrigVertexBuffer^.GetDesc(@Desc))) then 
        begin 
            CxbxKrnlCleanup('Could not retrieve original buffer size');
         end;
        if(FAILED(pOrigVertexBuffer^.Lock(0, 0, (uint08)@pCalculateData, 0))) then 
        begin 
            CxbxKrnlCleanup('Couldn't lock the original buffer');
         end;

        uiLength := Desc.Size;
        pCachedStream^.bIsUP := false;
        uiKey := (uint32)pOrigVertexBuffer;
     end;
    else
    begin 
        // There should only be one stream (stream zero) in this case
        if(uiStream <> 0) then 
        begin 
            CxbxKrnlCleanup('Trying to patch a Draw..UP with more than stream zero not ');
         end;
        uiStride  := pPatchDesc^.uiVertexStreamZeroStride;
        pCalculateData := (uint08 )pPatchDesc^.pVertexStreamZeroData;
        // TODO: This is sometimes the number of indices, which isn't too good
        uiLength := pPatchDesc^.dwVertexCount * pPatchDesc^.uiVertexStreamZeroStride;
        pCachedStream^.bIsUP := true;
        pCachedStream^.pStreamUP := pCalculateData;
        uiKey := (uint32)pCalculateData;
     end;

    UINT uiChecksum := CRC32((Byte )pCalculateData, uiLength);
    if( not pPatchDesc^.pVertexStreamZeroData) then 
    begin 
        pOrigVertexBuffer^.Unlock();
     end;

    pCachedStream^.uiCRC32 := uiChecksum;
    pCachedStream^.Stream := m_pStreams[uiStream];
    pCachedStream^.uiCheckFrequency := 1; // Start with checking every 1th Draw..
    pCachedStream^.uiCount := 0;
    pCachedStream^.uiLength := uiLength;
    pCachedStream^.uiCacheHit := 0;
    pCachedStream^.dwPrimitiveCount := pPatchDesc^.dwPrimitiveCount;
    pCachedStream^.lLastUsed := clock();
    g_PatchedStreamsCache.insert(uiKey, pCachedStream);
 end;

procedure XTL.VertexPatcher.FreeCachedStream(pStream: Pointer);
begin 
    g_PatchedStreamsCache.Lock();
    CACHEDSTREAM *pCachedStream := (CACHEDSTREAM )g_PatchedStreamsCache.get(pStream);
    if(pCachedStream) then 
    begin 
        if(pCachedStream^.bIsUP and pCachedStream^.pStreamUP) then 
        begin 
            CxbxFree(pCachedStream^.pStreamUP);
         end;
        if(pCachedStream^.Stream.pOriginalStream) then 
        begin 
            pCachedStream^.Stream.pOriginalStream^.Release();
         end;
        if(pCachedStream^.Stream.pPatchedStream) then 
        begin 
            pCachedStream^.Stream.pPatchedStream^.Release();
         end;
        CxbxFree(pCachedStream);
     end;
    g_PatchedStreamsCache.Unlock();
    g_PatchedStreamsCache.remove(pStream);
 end;

function XTL.VertexPatcher.ApplyCachedStream(var pPatchDesc: VertexPatchDesc; uiStream: UINT): bool;
begin 
    UINT                       uiStride;
    IDirect3DVertexBuffer8    *pOrigVertexBuffer;
    XTL.D3DVERTEXBUFFER_DESC  Desc;
    procedure                      *pCalculateData;
    UINT                       uiLength;
    bool                       bApplied := false;
    uint32                     uiKey;
    //CACHEDSTREAM              *pCachedStream = (CACHEDSTREAM *)CxbxMalloc(sizeof(CACHEDSTREAM));

    if( not pPatchDesc^.pVertexStreamZeroData) then 
    begin 
        g_pD3DDevice8^.GetStreamSource(uiStream, @pOrigVertexBuffer, @uiStride);
        if(FAILED(pOrigVertexBuffer^.GetDesc(@Desc))) then 
        begin 
            CxbxKrnlCleanup('Could not retrieve original buffer size');
         end;
        uiLength := Desc.Size;
        uiKey := (uint32)pOrigVertexBuffer;
        //pCachedStream->bIsUP = false;
     end;
    else
    begin 
        // There should only be one stream (stream zero) in this case
        if(uiStream <> 0) then 
        begin 
            CxbxKrnlCleanup('Trying to find a cached Draw..UP with more than stream zero not ');
         end;
        uiStride  := pPatchDesc^.uiVertexStreamZeroStride;
        pCalculateData := (uint08 )pPatchDesc^.pVertexStreamZeroData;
        // TODO: This is sometimes the number of indices, which isn't too good
        uiLength := pPatchDesc^.dwVertexCount * pPatchDesc^.uiVertexStreamZeroStride;
        uiKey := (uint32)pCalculateData;
        //pCachedStream->bIsUP = true;
        //pCachedStream->pStreamUP = pCalculateData;
     end;
    g_PatchedStreamsCache.Lock();

    CACHEDSTREAM *pCachedStream := (CACHEDSTREAM )g_PatchedStreamsCache.get(uiKey);
    if(pCachedStream) then 
    begin 
        pCachedStream^.lLastUsed := clock();
        pCachedStream^.uiCacheHit:= pCachedStream^.uiCacheHit + 1;
        bool bMismatch := false;
        if(pCachedStream^.uiCount = (pCachedStream^.uiCheckFrequency - 1)) then 
        begin 
            if( not pPatchDesc^.pVertexStreamZeroData) then 
            begin 
                if(FAILED(pOrigVertexBuffer^.Lock(0, 0, (uint08)@pCalculateData, 0))) then 
                begin 
                    CxbxKrnlCleanup('Couldn't lock the original buffer');
                 end;
             end;
            // Use the cached stream length (which is a must for the UP stream)
            uint32 Checksum := CRC32((uint08)pCalculateData, pCachedStream^.uiLength);
            if(Checksum = pCachedStream^.uiCRC32) then 
            begin 
                // Take a while longer to check
                if(pCachedStream^.uiCheckFrequency < 32*1024) then 
                begin 
                    pCachedStream^.uiCheckFrequency:= pCachedStream^.uiCheckFrequency * 2;
                 end;
                pCachedStream^.uiCount := 0;
             end;
            else
            begin 
                // TODO: Do something about this
                if(pCachedStream^.bIsUP) then 
                begin 
                    FreeCachedStream(pCachedStream^.pStreamUP);
                 end;
                else
                begin 
                    FreeCachedStream(pCachedStream^.Stream.pOriginalStream);
                 end;
                pCachedStream := 0;
                bMismatch := true;
             end;
            if( not pPatchDesc^.pVertexStreamZeroData) then 
            begin 
                pOrigVertexBuffer^.Unlock();
             end;
         end;
        else
        begin 
            pCachedStream^.uiCount:= pCachedStream^.uiCount + 1;
         end;
        if( not bMismatch) then 
        begin 
            if( not pCachedStream^.bIsUP) then 
            begin 
                m_pStreams[uiStream].pOriginalStream := pOrigVertexBuffer;
                m_pStreams[uiStream].uiOrigStride := uiStride;
                g_pD3DDevice8^.SetStreamSource(uiStream, pCachedStream^.Stream.pPatchedStream, pCachedStream^.Stream.uiNewStride);
                pCachedStream^.Stream.pPatchedStream^.AddRef();
                pCachedStream^.Stream.pOriginalStream^.AddRef();
                m_pStreams[uiStream].pPatchedStream := pCachedStream^.Stream.pPatchedStream;
                m_pStreams[uiStream].uiNewStride := pCachedStream^.Stream.uiNewStride;
             end;
            else
            begin 
                pPatchDesc^.pVertexStreamZeroData := pCachedStream^.pStreamUP;
                pPatchDesc^.uiVertexStreamZeroStride := pCachedStream^.Stream.uiNewStride;
             end;
            if(pCachedStream^.dwPrimitiveCount) then 
            begin 
                // The primitives were patched, draw with the correct number of primimtives from the cache
                pPatchDesc^.dwPrimitiveCount := pCachedStream^.dwPrimitiveCount;
             end;
            bApplied := true;
            m_bPatched := true;
         end;
     end;
    g_PatchedStreamsCache.Unlock();

    if( not pPatchDesc^.pVertexStreamZeroData) then 
    begin 
        pOrigVertexBuffer^.Release();
     end;

    result:= bApplied;
 end;

function XTL.VertexPatcher.GetNbrStreams(var pPatchDesc: VertexPatchDesc): UINT;
begin 
    if(VshHandleIsVertexShader(g_CurrentVertexShader)) then 
    begin 
        VERTEX_DYNAMIC_PATCH *pDynamicPatch := VshGetVertexDynamicPatch(g_CurrentVertexShader);
        if(pDynamicPatch) then 
        begin 
            result:= pDynamicPatch^.NbrStreams;
         end;
        else
        begin 
            result:= 1; // Could be more, but it doesn't matter as we're not going to patch the types
         end;
     end;
    else if(g_CurrentVertexShader) then 
    begin 
        result:= 1;
     end;
    result:= 0;
 end;

function XTL.VertexPatcher.PatchStream(var pPatchDesc: VertexPatchDesc; uiStream: UINT): bool;
begin 
    PATCHEDSTREAM *pStream := @m_pStreams[uiStream];
    if( not m_pDynamicPatch) then 
    begin 
        result:= false;
     end;

    if( not VshHandleIsVertexShader(pPatchDesc^.hVertexShader)) then 
    begin 
        // No need to patch FVF types, there are no xbox extensions
        result:= false;
     end;

    if( not m_pDynamicPatch^.pStreamPatches[uiStream].NeedPatch) then 
    begin 
        result:= false;
     end;
    // Do some groovey patchin'
    IDirect3DVertexBuffer8    *pOrigVertexBuffer;
    IDirect3DVertexBuffer8    *pNewVertexBuffer;
    uint08                    *pOrigData;
    uint08                    *pNewData;
    UINT                       uiStride;
    XTL.D3DVERTEXBUFFER_DESC  Desc;
    STREAM_DYNAMIC_PATCH      *pStreamPatch := @m_pDynamicPatch^.pStreamPatches[uiStream];
    DWORD dwNewSize;

    if( not pPatchDesc^.pVertexStreamZeroData) then 
    begin 
        g_pD3DDevice8^.GetStreamSource(uiStream, @pOrigVertexBuffer, @uiStride);
        if(FAILED(pOrigVertexBuffer^.GetDesc(@Desc))) then 
        begin 
            CxbxKrnlCleanup('Could not retrieve original buffer size');
         end;
        // Set a new (exact) vertex count
        pPatchDesc^.dwVertexCount := Desc.Size / uiStride;
        dwNewSize := pPatchDesc^.dwVertexCount * pStreamPatch^.ConvertedStride;

        if(FAILED(pOrigVertexBuffer^.Lock(0, 0, @pOrigData, 0))) then 
        begin 
            CxbxKrnlCleanup('Couldn't lock the original buffer');
         end;
        g_pD3DDevice8^.CreateVertexBuffer(dwNewSize, 0, 0, XTL.D3DPOOL_MANAGED, @pNewVertexBuffer);
        if(FAILED(pNewVertexBuffer^.Lock(0, 0, @pNewData, 0))) then 
        begin 
            CxbxKrnlCleanup('Couldn't lock the new buffer');
         end;
        if( not pStream^.pOriginalStream) then 
        begin 
            // The stream was not previously patched, we'll need this when restoring
            pStream^.pOriginalStream := pOrigVertexBuffer;
         end;
     end;
    else
    begin 
        // There should only be one stream (stream zero) in this case
        if(uiStream <> 0) then 
        begin 
            CxbxKrnlCleanup('Trying to patch a Draw..UP with more than stream zero not ');
         end;
        uiStride  := pPatchDesc^.uiVertexStreamZeroStride;
        pOrigData := (uint08 )pPatchDesc^.pVertexStreamZeroData;
        // TODO: This is sometimes the number of indices, which isn't too good
        dwNewSize := pPatchDesc^.dwVertexCount * pStreamPatch^.ConvertedStride;
        pNewVertexBuffer := 0;
        pNewData := (uint08)CxbxMalloc(dwNewSize);
        if( not pNewData) then 
        begin 
            CxbxKrnlCleanup('Couldn't allocate the new stream zero buffer');
         end;
     end;

    for(uint32 uiVertex := 0; uiVertex < pPatchDesc^.dwVertexCount; uiVertex++)
    begin 
        DWORD dwPosOrig := 0;
        DWORD dwPosNew := 0;
        for(UINT uiType := 0; uiType < pStreamPatch^.NbrTypes; uiType++)
        begin 
            case(pStreamPatch^.pTypes[uiType]) of
            begin 
                 $12: // FLOAT1
                    memcpy(@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           SizeOf(FLOAT));
                    dwPosOrig:= dwPosOrig + SizeOf(FLOAT);
                    dwPosNew := dwPosNew + SizeOf(FLOAT);
                    break;
                 $22: // FLOAT2
                    memcpy(@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           2 * SizeOf(FLOAT));
                    dwPosOrig:= dwPosOrig + 2 * SizeOf(FLOAT);
                    dwPosNew := dwPosNew + 2 * SizeOf(FLOAT);
                    break;
                 $32: // FLOAT3
                    memcpy(@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           3 * SizeOf(FLOAT));
                    dwPosOrig:= dwPosOrig + 3 * SizeOf(FLOAT);
                    dwPosNew := dwPosNew + 3 * SizeOf(FLOAT);
                    break;
                 $42: // FLOAT4
                    memcpy(@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           4 * SizeOf(FLOAT));
                    dwPosOrig:= dwPosOrig + 4 * SizeOf(FLOAT);
                    dwPosNew := dwPosNew + 4 * SizeOf(FLOAT);
                    break;
                 $40: // D3DCOLOR
                    memcpy(@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           SizeOf(XTL.D3DCOLOR));
                    dwPosOrig:= dwPosOrig + SizeOf(XTL.D3DCOLOR);
                    dwPosNew := dwPosNew + SizeOf(XTL.D3DCOLOR);
                    break;
                 $16: //NORMPACKED3
                    begin 
                        DWORD dwPacked := ((DWORD )@pOrigData[uiVertex * uiStride + dwPosOrig])[0];

                        ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[0] := ((FLOAT)(dwPacked and $7ff)) / 1023.0f;
                        ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[1] := ((FLOAT)((dwPacked shr 11) and $7ff)) / 1023.0f;
                        ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[2] := ((FLOAT)((dwPacked shr 22) and $3ff)) / 511.0f;

                        dwPosOrig:= dwPosOrig + SizeOf(DWORD);
                        dwPosNew := dwPosNew + 3 * SizeOf(FLOAT);
                     end;
                    break;
                 $15: // SHORT1
                    // Make it a SHORT2
                    (((SmallInt )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew + 0 * SizeOf(SmallInt)])) := *(SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig];
                    (((SmallInt )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew + 1 * SizeOf(SmallInt)])) := $00;

                    dwPosOrig:= dwPosOrig + 1 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 2 * SizeOf(SmallInt);

                    break;
                 $25: // SHORT2
                    memcpy(@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride+dwPosOrig],
                           2 * SizeOf(SmallInt));
                    dwPosOrig:= dwPosOrig + 2 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 2 * SizeOf(SmallInt);
                    break;
                 $35: // SHORT3
                    memcpy(@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           3 * SizeOf(SmallInt));
                    // Make it a SHORT4 and set the last short to 1
                    (((SmallInt )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew + 3 * SizeOf(SmallInt)])) := $01;

                    dwPosOrig:= dwPosOrig + 3 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 4 * SizeOf(SmallInt);

                    break;
                 $45: // SHORT4
                    memcpy(@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           4 * SizeOf(SmallInt));
                    dwPosOrig:= dwPosOrig + 4 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 4 * SizeOf(SmallInt);
                    break;
                 $14: // PBYTE1
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[0] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 255.0f;

                    dwPosOrig:= dwPosOrig + 1 * SizeOf(BYTE);
                    dwPosNew := dwPosNew + 1 * SizeOf(FLOAT);

                    break;
                 $24: // PBYTE2
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[0] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[1] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 255.0f;

                    dwPosOrig:= dwPosOrig + 2 * SizeOf(BYTE);
                    dwPosNew := dwPosNew + 2 * SizeOf(FLOAT);

                    break;
                 $34: // PBYTE3
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[0] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[1] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[2] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[2]) / 255.0f;

                    dwPosOrig:= dwPosOrig + 3 * SizeOf(BYTE);
                    dwPosNew := dwPosNew + 3 * SizeOf(FLOAT);

                    break;
                 $44: // PBYTE4
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[0] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[1] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[2] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[2]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[3] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[3]) / 255.0f;

                    dwPosOrig:= dwPosOrig + 4 * SizeOf(BYTE);
                    dwPosNew := dwPosNew + 4 * SizeOf(FLOAT);

                    break;
                 $11: // NORMSHORT1
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[0] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 32767.0f;

                    dwPosOrig:= dwPosOrig + 1 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 1 * SizeOf(FLOAT);
                    break;
                 $21: // NORMSHORT2
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[0] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[1] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 32767.0f;

                    dwPosOrig:= dwPosOrig + 2 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 2 * SizeOf(FLOAT);
                    break;
                 $31: // NORMSHORT3
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[0] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[1] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[2] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[2]) / 32767.0f;

                    dwPosOrig:= dwPosOrig + 3 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 3 * SizeOf(FLOAT);
                    break;
                 $41: // NORMSHORT4
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[0] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[1] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[2] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[2]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[3] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[3]) / 32767.0f;

                    dwPosOrig:= dwPosOrig + 4 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 4 * SizeOf(FLOAT);
                    break;
                 $72: // FLOAT2H
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[0] := ((FLOAT)@pOrigData[uiVertex * uiStride + dwPosOrig])[0];
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[1] := ((FLOAT)@pOrigData[uiVertex * uiStride + dwPosOrig])[1];
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[2] := 0.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch^.ConvertedStride + dwPosNew])[3] := ((FLOAT)@pOrigData[uiVertex * uiStride + dwPosOrig])[2];

                (*TODO
                 $02:
                    printf('D3DVSDT_NONE / xbox ext. nsp /');
                    dwNewDataType := $FF;
                    break;
                *)
                default:
                    CxbxKrnlCleanup('Unhandled stream ctype: $ mod .02X', pStreamPatch^.pTypes[uiType]);
                    break;
             end;
         end;
     end;
    if( not pPatchDesc^.pVertexStreamZeroData) then 
    begin 
        pNewVertexBuffer^.Unlock();
        pOrigVertexBuffer^.Unlock();

        if(FAILED(g_pD3DDevice8^.SetStreamSource(uiStream, pNewVertexBuffer, pStreamPatch^.ConvertedStride))) then 
        begin 
            CxbxKrnlCleanup('Failed to set the ctype patched buffer as the new stream source not ');
         end;
        if(pStream^.pPatchedStream) then 
        begin 
            // The stream was already primitive patched, release the previous vertex buffer to avoid memory leaks
            pStream^.pPatchedStream^.Release();
         end;
        pStream^.pPatchedStream := pNewVertexBuffer;
     end;
    else
    begin 
        pPatchDesc^.pVertexStreamZeroData := pNewData;
        pPatchDesc^.uiVertexStreamZeroStride := pStreamPatch^.ConvertedStride;
        if( not m_bAllocatedStreamZeroData) then 
        begin 
            // The stream was not previously patched. We'll need this when restoring
            m_bAllocatedStreamZeroData := true;
            m_pNewVertexStreamZeroData := pNewData;
         end;
     end;
    pStream^.uiOrigStride := uiStride;
    pStream^.uiNewStride := pStreamPatch^.ConvertedStride;
    m_bPatched := true;

    result:= true;
 end;

function XTL.VertexPatcher.PatchPrimitive(var pPatchDesc: VertexPatchDesc; uiStream: UINT): bool;
begin 
    PATCHEDSTREAM *pStream := @m_pStreams[uiStream];
    // only quad and listloop are currently supported
    if((pPatchDesc^.PrimitiveType <> X_D3DPT_QUADLIST) and (pPatchDesc^.PrimitiveType <> X_D3DPT_LINELOOP)) then 
        result:= false;

    if(pPatchDesc^.pVertexStreamZeroData and uiStream > 0) then 
    begin 
        CxbxKrnlCleanup('Draw..UP call with more than one stream not ');
     end;

    pStream^.uiOrigStride := 0;

    // sizes of our part in the vertex buffer
    DWORD dwOriginalSize    := 0;
    DWORD dwNewSize         := 0;

    // sizes with the rest of the buffer
    DWORD dwOriginalSizeWR  := 0;
    DWORD dwNewSizeWR       := 0;

    // vertex data arrays
    BYTE *pOrigVertexData := 0;
    BYTE *pPatchedVertexData := 0;

    if(pPatchDesc^.pVertexStreamZeroData = 0) then 
    begin 
        g_pD3DDevice8^.GetStreamSource(0, @pStream^.pOriginalStream, @pStream^.uiOrigStride);
        pStream^.uiNewStride := pStream^.uiOrigStride; // The stride is still the same

        if(pPatchDesc^.PrimitiveType = X_D3DPT_QUADLIST) then 
        begin 
            pPatchDesc^.dwPrimitiveCount:= pPatchDesc^.dwPrimitiveCount * 2;

            // This is a list of sqares/rectangles, so we convert it to a list of triangles
            dwOriginalSize  := pPatchDesc^.dwPrimitiveCount * pStream^.uiOrigStride * 2;
            dwNewSize       := pPatchDesc^.dwPrimitiveCount * pStream^.uiOrigStride * 3;
         end;
        // LineLoop
        else if(pPatchDesc^.PrimitiveType = X_D3DPT_LINELOOP) then 
        begin 
            pPatchDesc^.dwPrimitiveCount:= pPatchDesc^.dwPrimitiveCount + 1;

            // We will add exactly one more line
            dwOriginalSize  := pPatchDesc^.dwPrimitiveCount * pStream^.uiOrigStride;
            dwNewSize       := pPatchDesc^.dwPrimitiveCount * pStream^.uiOrigStride + pStream^.uiOrigStride;
         end;

        // Retrieve the original buffer size
        begin 
            XTL.D3DVERTEXBUFFER_DESC Desc;

            if(FAILED(pStream^.pOriginalStream^.GetDesc(@Desc))) then 
            begin 
                CxbxKrnlCleanup('Could not retrieve buffer size');
             end;

            // Here we save the full buffer size
            dwOriginalSizeWR := Desc.Size;

            // So we can now calculate the size of the rest (dwOriginalSizeWR - dwOriginalSize) and
            // add it to our new calculated size of the patched buffer
            dwNewSizeWR := dwNewSize + dwOriginalSizeWR - dwOriginalSize;
         end;

        g_pD3DDevice8->CreateVertexBuffer(dwNewSizeWR, 0, 0, XTL.D3DPOOL_MANAGED, @pStream->pPatchedStream);

        if(pStream->pOriginalStream <> 0) then 
        begin 
            pStream->pOriginalStream->Lock(0, 0, @pOrigVertexData, 0);
         end;

        if(pStream->pPatchedStream <> 0) then 
        begin 
            pStream->pPatchedStream->Lock(0, 0, @pPatchedVertexData, 0);
         end;
     end;
    else
    begin 
        pStream->uiOrigStride := pPatchDesc->uiVertexStreamZeroStride;

        if(pPatchDesc->PrimitiveType = X_D3DPT_QUADLIST) then 
        begin 
            pPatchDesc->dwPrimitiveCount:= pPatchDesc->dwPrimitiveCount * 2;

            // This is a list of sqares/rectangles, so we convert it to a list of triangles
            dwOriginalSize  := pPatchDesc->dwPrimitiveCount * pStream->uiOrigStride * 2;
            dwNewSize       := pPatchDesc->dwPrimitiveCount * pStream->uiOrigStride * 3;
         end;
        else if(pPatchDesc->PrimitiveType = X_D3DPT_LINELOOP) then  // LineLoop
        begin 
            pPatchDesc->dwPrimitiveCount:= pPatchDesc->dwPrimitiveCount + 1;

            // We will add exactly one more line
            dwOriginalSize  := pPatchDesc->dwPrimitiveCount * pStream->uiOrigStride;
            dwNewSize       := pPatchDesc->dwPrimitiveCount * pStream->uiOrigStride + pStream->uiOrigStride;
         end;

        dwOriginalSizeWR := dwOriginalSize;
        dwNewSizeWR := dwNewSize;

        m_pNewVertexStreamZeroData := (uint08)CxbxMalloc(dwNewSizeWR);
        m_bAllocatedStreamZeroData := true;

        pPatchedVertexData := (uint08)m_pNewVertexStreamZeroData;
        pOrigVertexData := (uint08)pPatchDesc->pVertexStreamZeroData;

        pPatchDesc->pVertexStreamZeroData := pPatchedVertexData;
     end;

    // Copy the nonmodified data
    memcpy(pPatchedVertexData, pOrigVertexData, pPatchDesc->dwOffset);
    memcpy(@pPatchedVertexData[pPatchDesc->dwOffset+dwNewSize],
           @pOrigVertexData[pPatchDesc->dwOffset+dwOriginalSize],
           dwOriginalSizeWR - pPatchDesc->dwOffset - dwOriginalSize);

    // Quad
    if(pPatchDesc->PrimitiveType = X_D3DPT_QUADLIST) then 
    begin 
        uint08 *pPatch1 := @pPatchedVertexData[pPatchDesc->dwOffset     * pStream->uiOrigStride];
        uint08 *pPatch2 := @pPatchedVertexData[pPatchDesc->dwOffset + 3 * pStream->uiOrigStride];
        uint08 *pPatch3 := @pPatchedVertexData[pPatchDesc->dwOffset + 4 * pStream->uiOrigStride];
        uint08 *pPatch4 := @pPatchedVertexData[pPatchDesc->dwOffset + 5 * pStream->uiOrigStride];

        uint08 *pOrig1 := @pOrigVertexData[pPatchDesc->dwOffset     * pStream->uiOrigStride];
        uint08 *pOrig2 := @pOrigVertexData[pPatchDesc->dwOffset + 2 * pStream->uiOrigStride];
        uint08 *pOrig3 := @pOrigVertexData[pPatchDesc->dwOffset + 3 * pStream->uiOrigStride];

        for(uint32 i := 0;i < pPatchDesc->dwPrimitiveCount/2;i++)
        begin 
            memcpy(pPatch1, pOrig1, pStream->uiOrigStride * 3); // Vertex 0,1,2 := Vertex 0,1,2
            memcpy(pPatch2, pOrig2, pStream->uiOrigStride);     // Vertex 3     := Vertex 2
            memcpy(pPatch3, pOrig3, pStream->uiOrigStride);     // Vertex 4     := Vertex 3
            memcpy(pPatch4, pOrig1, pStream->uiOrigStride);     // Vertex 5     := Vertex 0

            pPatch1:= pPatch1 + pStream->uiOrigStride * 6;
            pPatch2:= pPatch2 + pStream->uiOrigStride * 6;
            pPatch3:= pPatch3 + pStream->uiOrigStride * 6;
            pPatch4:= pPatch4 + pStream->uiOrigStride * 6;

            pOrig1:= pOrig1 + pStream->uiOrigStride * 4;
            pOrig2:= pOrig2 + pStream->uiOrigStride * 4;
            pOrig3:= pOrig3 + pStream->uiOrigStride * 4;

            if(pPatchDesc->hVertexShader and D3DFVF_XYZRHW) then 
            begin 
                for(integer z := 0; z < 6; z++)
                begin 
                    if(((FLOAT)@pPatchedVertexData[pPatchDesc->dwOffset + i * pStream->uiOrigStride * 6 + z * pStream->uiOrigStride])[2] = 0.0f) then 
                        ((FLOAT)@pPatchedVertexData[pPatchDesc->dwOffset + i * pStream->uiOrigStride * 6 + z * pStream->uiOrigStride])[2] := 1.0f;
                    if(((FLOAT)@pPatchedVertexData[pPatchDesc->dwOffset + i * pStream->uiOrigStride * 6 + z * pStream->uiOrigStride])[3] = 0.0f) then 
                        ((FLOAT)@pPatchedVertexData[pPatchDesc->dwOffset + i * pStream->uiOrigStride * 6 + z * pStream->uiOrigStride])[3] := 1.0f;
                 end;
             end;
         end;
     end;
    // LineLoop
    else if(pPatchDesc->PrimitiveType = X_D3DPT_LINELOOP) then 
    begin 
        memcpy(@pPatchedVertexData[pPatchDesc->dwOffset], @pOrigVertexData[pPatchDesc->dwOffset], dwOriginalSize);
        memcpy(@pPatchedVertexData[pPatchDesc->dwOffset + dwOriginalSize], @pOrigVertexData[pPatchDesc->dwOffset], pStream->uiOrigStride);
     end;

    if(pPatchDesc->pVertexStreamZeroData = 0) then 
    begin 
        pStream->pOriginalStream->Unlock();
        pStream->pPatchedStream->Unlock();

        g_pD3DDevice8->SetStreamSource(0, pStream->pPatchedStream, pStream->uiOrigStride);
     end;

    m_bPatched := true;

    result:= true;
 end;

function XTL.VertexPatcher.Apply(var pPatchDesc: VertexPatchDesc): bool;
begin 
    bool Patched := false;
    // Get the number of streams
    m_uiNbrStreams := GetNbrStreams(pPatchDesc);
    if(VshHandleIsVertexShader(pPatchDesc->hVertexShader)) then 
    begin 
        m_pDynamicPatch :=  and ((VERTEX_SHADER )VshHandleGetVertexShader(pPatchDesc->hVertexShader)->Handle)->VertexDynamicPatch;
     end;
    for(UINT uiStream := 0; uiStream < m_uiNbrStreams; uiStream++)
    begin 
        bool LocalPatched := false;

        if(ApplyCachedStream(pPatchDesc, uiStream)) then 
        begin 
            m_pStreams[uiStream].bUsedCached := true;
            continue;
         end;

        LocalPatched:= LocalPatched or PatchPrimitive(pPatchDesc, uiStream);
        LocalPatched:= LocalPatched or PatchStream(pPatchDesc, uiStream);
        if(LocalPatched and  not pPatchDesc->pVertexStreamZeroData) then 
        begin 
            // Insert the patched stream in the cache
            CacheStream(pPatchDesc, uiStream);
            m_pStreams[uiStream].bUsedCached := true;
         end;
        Patched:= Patched or LocalPatched;
     end;

    result:= Patched;
 end;

function XTL.VertexPatcher.Restore(): bool;
begin 
    if( not this->m_bPatched) then 
        result:= false;

    for(UINT uiStream := 0; uiStream < m_uiNbrStreams; uiStream++)
    begin 
        if(m_pStreams[uiStream].pOriginalStream <> 0 and m_pStreams[uiStream].pPatchedStream <> 0) then 
        begin 
            g_pD3DDevice8->SetStreamSource(0, m_pStreams[uiStream].pOriginalStream, m_pStreams[uiStream].uiOrigStride);
         end;

        if(m_pStreams[uiStream].pOriginalStream <> 0) then 
        begin 
            UINT a := m_pStreams[uiStream].pOriginalStream->Release();
         end;

        if(m_pStreams[uiStream].pPatchedStream <> 0) then 
        begin 
            UINT b := m_pStreams[uiStream].pPatchedStream->Release();
         end;

        if( not m_pStreams[uiStream].bUsedCached) then 
        begin 

            if(this->m_bAllocatedStreamZeroData) then 
            begin 
                CxbxFree(m_pNewVertexStreamZeroData);
             end;
         end;
        else
        begin 
            m_pStreams[uiStream].bUsedCached := false;
         end;

     end;

    result:= true;
 end;

VOID XTL.EmuFlushIVB()
begin 
    if(g_IVBPrimitiveType = X_D3DPT_TRIANGLEFAN) then 
    begin 
        XTL.EmuUpdateDeferredStates();

        DWORD *pdwVB := (DWORD)g_IVBTable;

        UINT uiStride := 0;

        DbgPrintf('g_IVBTblOffs :=  mod d', g_IVBTblOffs);

        // TEMP DEBUGGING
        //*
        g_IVBTable[0].TexCoord1.x := 0.0f;
        g_IVBTable[0].TexCoord1.y := 0.0f;
        g_IVBTable[1].TexCoord1.x := 1.0f;
        g_IVBTable[1].TexCoord1.y := 0.0f;
        g_IVBTable[2].TexCoord1.x := 1.0f;
        g_IVBTable[2].TexCoord1.y := 1.0f;
        g_IVBTable[3].TexCoord1.x := 0.0f;
        g_IVBTable[3].TexCoord1.y := 1.0f;
        g_IVBTable[0].TexCoord2.x := 0.0f;
        g_IVBTable[0].TexCoord2.y := 0.0f;
        g_IVBTable[1].TexCoord2.x := 1.0f;
        g_IVBTable[1].TexCoord2.y := 0.0f;
        g_IVBTable[2].TexCoord2.x := 1.0f;
        g_IVBTable[2].TexCoord2.y := 1.0f;
        g_IVBTable[3].TexCoord2.x := 0.0f;
        g_IVBTable[3].TexCoord2.y := 1.0f;
        g_IVBTable[0].TexCoord3.x := 0.0f;
        g_IVBTable[0].TexCoord3.y := 0.0f;
        g_IVBTable[1].TexCoord3.x := 1.0f;
        g_IVBTable[1].TexCoord3.y := 0.0f;
        g_IVBTable[2].TexCoord3.x := 1.0f;
        g_IVBTable[2].TexCoord3.y := 1.0f;
        g_IVBTable[3].TexCoord3.x := 0.0f;
        g_IVBTable[3].TexCoord3.y := 1.0f;
        g_IVBTable[0].TexCoord4.x := 0.0f;
        g_IVBTable[0].TexCoord4.y := 0.0f;
        g_IVBTable[1].TexCoord4.x := 1.0f;
        g_IVBTable[1].TexCoord4.y := 0.0f;
        g_IVBTable[2].TexCoord4.x := 1.0f;
        g_IVBTable[2].TexCoord4.y := 1.0f;
        g_IVBTable[3].TexCoord4.x := 0.0f;
        g_IVBTable[3].TexCoord4.y := 1.0f;
        //*/
        (*
         IDirect3DTexture8 *pDummyTexture[4] := (0, 0, 0, 0);

        for(integer Stage:=0;Stage<4;Stage++)
        begin 
            if(pDummyTexture[Stage] = 0) then 
            begin 
                if(Stage = 0) then 
                begin 
                    if(D3DXCreateTextureFromFile(g_pD3DDevice8, 'C:\dummy1.bmp', @pDummyTexture[Stage]) <> D3D_OK) then 
                        CxbxKrnlCleanup('Could not create dummy texture not ');
                 end;
                else if(Stage = 1) then 
                begin 
                    if(D3DXCreateTextureFromFile(g_pD3DDevice8, 'C:\dummy2.bmp', @pDummyTexture[Stage]) <> D3D_OK) then 
                        CxbxKrnlCleanup('Could not create dummy texture not ');
                 end;
             end;

            g_pD3DDevice8->SetTexture(Stage, pDummyTexture[Stage]);
         end;
        //*/
        g_pD3DDevice8->SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_BLENDDIFFUSEALPHA);
        g_pD3DDevice8->SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
        g_pD3DDevice8->SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_SPECULAR);
        g_pD3DDevice8->SetTextureStageState(0, D3DTSS_ALPHAOP,   D3DTOP_SELECTARG1);
        g_pD3DDevice8->SetTextureStageState(0, D3DTSS_ALPHAARG1,   D3DTA_TEXTURE);

        (*
        g_pD3DDevice8->SetTextureStageState(0, D3DTSS_ALPHAOP,   D3DTOP_SELECTARG1);
        g_pD3DDevice8->SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
        g_pD3DDevice8->SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_SELECTARG1);
        g_pD3DDevice8->SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
        g_pD3DDevice8->SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);

        g_pD3DDevice8->SetTextureStageState(1, D3DTSS_ALPHAOP,   D3DTOP_DISABLE);
        g_pD3DDevice8->SetTextureStageState(1, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
        g_pD3DDevice8->SetTextureStageState(1, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
        g_pD3DDevice8->SetTextureStageState(1, D3DTSS_COLOROP,   D3DTOP_DISABLE);
        g_pD3DDevice8->SetTextureStageState(1, D3DTSS_COLORARG1, D3DTA_TEXTURE);
        g_pD3DDevice8->SetTextureStageState(1, D3DTSS_COLORARG2, D3DTA_CURRENT);

        g_pD3DDevice8->SetTextureStageState(2, D3DTSS_ALPHAOP,   D3DTOP_DISABLE);
        g_pD3DDevice8->SetTextureStageState(2, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
        g_pD3DDevice8->SetTextureStageState(2, D3DTSS_COLOROP,   D3DTOP_DISABLE);
        g_pD3DDevice8->SetTextureStageState(2, D3DTSS_COLORARG1, D3DTA_TEXTURE);
        g_pD3DDevice8->SetTextureStageState(2, D3DTSS_COLORARG2, D3DTA_CURRENT);

        g_pD3DDevice8->SetTextureStageState(3, D3DTSS_ALPHAOP,   D3DTOP_DISABLE);
        g_pD3DDevice8->SetTextureStageState(3, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
        g_pD3DDevice8->SetTextureStageState(3, D3DTSS_COLOROP,   D3DTOP_DISABLE);
        g_pD3DDevice8->SetTextureStageState(3, D3DTSS_COLORARG1, D3DTA_TEXTURE);
        g_pD3DDevice8->SetTextureStageState(3, D3DTSS_COLORARG2, D3DTA_CURRENT);

        g_pD3DDevice8->SetRenderState(D3DRS_SRCBLEND, D3DBLEND_ONE);
        g_pD3DDevice8->SetRenderState(D3DRS_DESTBLEND, D3DBLEND_SRCCOLOR);
        g_pD3DDevice8->SetRenderState(D3DRS_AMBIENT, RGB(255,255,255));
        g_pD3DDevice8->SetRenderState(D3DRS_LIGHTING, FALSE);
        g_pD3DDevice8->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
        g_pD3DDevice8->SetRenderState(D3DRS_ZENABLE, D3DZB_TRUE);
        //*/

        for(uint v:=0;v<g_IVBTblOffs;v++)
        begin 
            DWORD dwPos := g_IVBFVF and D3DFVF_POSITION_MASK;

            if(dwPos = D3DFVF_XYZRHW) then 
            begin 
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.y;
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.z;
                *(FLOAT)pdwVB++ := g_IVBTable[v].Rhw;

                if(v = 0) then 
                begin 
                    uiStride:= uiStride + (SizeOf(FLOAT)*4);
                 end;

                DbgPrintf('IVB Position := ( mod f,  mod f,  mod f end;', g_IVBTable[v].Position.x, g_IVBTable[v].Position.y, g_IVBTable[v].Position.z);
             end;
            else
            begin 
                CxbxKrnlCleanup('Unsupported Position Mask (FVF := $ mod .08X)', g_IVBFVF);
             end;

            if(g_IVBFVF and D3DFVF_DIFFUSE) then 
            begin 
                *(DWORD)pdwVB++ := g_IVBTable[v].dwDiffuse;

                if(v = 0) then 
                begin 
                    uiStride:= uiStride + SizeOf(DWORD);
                 end;

                DbgPrintf('IVB Diffuse := $ mod .08X', g_IVBTable[v].dwDiffuse);
             end;

            if(g_IVBFVF and D3DFVF_SPECULAR) then 
            begin 
                *(DWORD)pdwVB++ := g_IVBTable[v].dwDiffuse;

                if(v = 0) then 
                begin 
                    uiStride:= uiStride + SizeOf(DWORD);
                 end;

                DbgPrintf('IVB Specular := $ mod .08X', g_IVBTable[v].dwSpecular);
             end;

            DWORD dwTexN := (g_IVBFVF and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT;

            if(dwTexN >= 1) then 
            begin 
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord1.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord1.y;

                if(v = 0) then 
                begin 
                    uiStride:= uiStride + SizeOf(FLOAT)*2;
                 end;

                DbgPrintf('IVB TexCoord1 := ( mod f,  mod f end;', g_IVBTable[v].TexCoord1.x, g_IVBTable[v].TexCoord1.y);
             end;

            if(dwTexN >= 2) then 
            begin 
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord2.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord2.y;

                if(v = 0) then 
                begin 
                    uiStride:= uiStride + SizeOf(FLOAT)*2;
                 end;

                DbgPrintf('IVB TexCoord2 := ( mod f,  mod f end;', g_IVBTable[v].TexCoord2.x, g_IVBTable[v].TexCoord2.y);
             end;

            if(dwTexN >= 3) then 
            begin 
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord3.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord3.y;

                if(v = 0) then 
                begin 
                    uiStride:= uiStride + SizeOf(FLOAT)*2;
                 end;

                DbgPrintf('IVB TexCoord3 := ( mod f,  mod f end;", g_IVBTable[v].TexCoord3.x, g_IVBTable[v].TexCoord3.y);
             end;

            if(dwTexN >= 4) then 
            begin 
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord4.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord4.y;

                if(v = 0) then 
                begin 
                    uiStride:= uiStride + SizeOf(FLOAT)*2;
                 end;

                DbgPrintf("IVB TexCoord4 := ( mod f,  mod f end;", g_IVBTable[v].TexCoord4.x, g_IVBTable[v].TexCoord4.y);
             end;
         end;

        g_pD3DDevice8->SetVertexShader(g_IVBFVF);
        g_pD3DDevice8->SetPixelShader(0);

        // patch buffer
        UINT PrimitiveCount := EmuD3DVertex2PrimitiveCount(g_IVBPrimitiveType, g_IVBTblOffs);

        VertexPatchDesc VPDesc;

        VPDesc.dwVertexCount := g_IVBTblOffs;
        VPDesc.PrimitiveType := g_IVBPrimitiveType;
        VPDesc.dwPrimitiveCount := PrimitiveCount;
        VPDesc.dwOffset := 0;
        VPDesc.pVertexStreamZeroData := g_IVBTable;
        VPDesc.uiVertexStreamZeroStride := uiStride;
        // TODO: Set the current shader and let the patcher handle it..
        VPDesc.hVertexShader := g_IVBFVF;

        VertexPatcher VertPatch;

        bool bPatched := VertPatch.Apply(@VPDesc);

        (*
        IDirect3DBaseTexture8 *pTexture := 0;

        g_pD3DDevice8->GetTexture(0, @pTexture);
    
        if(pTexture <> 0) then 
        begin 
             integer dwDumpTexture := 0;

             szBuffer: array[0..255-1] of Char;

            StrFmt(szBuffer, "C:\Aaron\Textures\Texture-Active mod .03d ($ mod .08X).bmp", dwDumpTexture++, pTexture);

            D3DXSaveTextureToFile(szBuffer, D3DXIFF_BMP, pTexture, 0);
         end;
        //*/
        EmuUpdateActiveTexture();

        g_pD3DDevice8->DrawPrimitiveUP(D3DPT_TRIANGLEFAN, VPDesc.dwPrimitiveCount, VPDesc.pVertexStreamZeroData, VPDesc.uiVertexStreamZeroStride);

        VertPatch.Restore();

        g_IVBTblOffs := 0;
     end;
    else if((g_IVBPrimitiveType = X_D3DPT_QUADLIST) and (g_IVBTblOffs = 4)) then 
    begin 
        XTL.EmuUpdateDeferredStates();

        DWORD *pdwVB := (DWORD)g_IVBTable;

        UINT uiStride := 0;

        for(integer v:=0;v<4;v++)
        begin 
            DWORD dwPos := g_IVBFVF and D3DFVF_POSITION_MASK;

            if(dwPos = D3DFVF_XYZ) then 
            begin 
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.y;
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.z;

                if(v = 0) then 
                begin 
                    uiStride:= uiStride + (SizeOf(FLOAT)*3);
                 end;

                DbgPrintf("IVB Position := ( mod f,  mod f,  mod f end;", g_IVBTable[v].Position.x, g_IVBTable[v].Position.y, g_IVBTable[v].Position.z);
             end;
            else
            begin 
                CxbxKrnlCleanup("Unsupported Position Mask (FVF := $ mod .08X)", g_IVBFVF);
             end;

            if(g_IVBFVF and D3DFVF_DIFFUSE) then 
            begin 
                *(DWORD)pdwVB++ := g_IVBTable[v].dwDiffuse;

                if(v = 0) then 
                begin 
                    uiStride:= uiStride + SizeOf(DWORD);
                 end;

                DbgPrintf("IVB Diffuse := $ mod .08X", g_IVBTable[v].dwDiffuse);
             end;

            DWORD dwTexN := (g_IVBFVF and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT;

            if(dwTexN >= 1) then 
            begin 
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord1.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord1.y;

                if(v = 0) then 
                begin 
                    uiStride:= uiStride + SizeOf(FLOAT)*2;
                 end;

                DbgPrintf("IVB TexCoord1 := ( mod f,  mod f end;", g_IVBTable[v].TexCoord1.x, g_IVBTable[v].TexCoord1.y);
             end;
         end;

        g_pD3DDevice8->SetVertexShader(g_IVBFVF);
        g_pD3DDevice8->SetPixelShader(0);

        // patch buffer
        UINT PrimitiveCount := EmuD3DVertex2PrimitiveCount(g_IVBPrimitiveType, 4);

        VertexPatchDesc VPDesc;

        VPDesc.dwVertexCount := 4;
        VPDesc.PrimitiveType := g_IVBPrimitiveType;
        VPDesc.dwPrimitiveCount := PrimitiveCount;
        VPDesc.dwOffset := 0;
        VPDesc.pVertexStreamZeroData := g_IVBTable;
        VPDesc.uiVertexStreamZeroStride := uiStride;
        // TODO: Set the current shader and let the patcher handle it..
        VPDesc.hVertexShader := g_IVBFVF;

        VertexPatcher VertPatch;

        bool bPatched := VertPatch.Apply(@VPDesc);

        g_pD3DDevice8->DrawPrimitiveUP(D3DPT_TRIANGLELIST, VPDesc.dwPrimitiveCount, VPDesc.pVertexStreamZeroData, VPDesc.uiVertexStreamZeroStride);

        VertPatch.Restore();

        // ignore
        g_IVBTblOffs := 0;
     end;

    Exit;
 end;

VOID XTL.EmuUpdateActiveTexture()
begin 
    //
    // DEBUGGING
    //
    for(integer Stage:=0;Stage<4;Stage++)
    begin 
        X_D3DResource *pTexture := EmuD3DActiveTexture[Stage];

        if(pTexture = 0) then 
            continue;

        //*
        X_D3DResource       *pResource := (X_D3DResource)pTexture;
        X_D3DPixelContainer *pPixelContainer := (X_D3DPixelContainer)pTexture;

        X_D3DFORMAT X_Format := (X_D3DFORMAT)((pPixelContainer->Format and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT);

        if(X_Format <> $CD and (pTexture->EmuResource8->GetType() = D3DRTYPE_TEXTURE)) then 
        begin 
            DWORD dwWidth, dwHeight, dwBPP, dwDepth := 1, dwPitch = 0, dwMipMapLevels = 1;
            BOOL  bSwizzled := FALSE, bCompressed = FALSE, dwCompressedSize = 0;
            BOOL  bCubemap := pPixelContainer->Format and X_D3DFORMAT_CUBEMAP;

            // Interpret Width/Height/BPP
            if(X_Format = $07 (* X_D3DFMT_X8R8G8B8 *) || X_Format == 0x06 /* X_D3DFMT_A8R8G8B8 */) then 
            begin 
                bSwizzled := TRUE;

                // Swizzled 32 Bit
                dwWidth  := 1 shl ((pPixelContainer->Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
                dwHeight := 1 shl ((pPixelContainer->Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
                dwMipMapLevels := (pPixelContainer->Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
                dwDepth  := 1;// HACK? 1 << ((pPixelContainer->Format & X_D3DFORMAT_PSIZE_MASK) >> X_D3DFORMAT_PSIZE_SHIFT);
                dwPitch  := dwWidth*4;
                dwBPP := 4;
             end;
            else if(X_Format = $05 (* X_D3DFMT_R5G6B5 *) then  || X_Format == 0x04 /* X_D3DFMT_A4R4G4B4 */
                 or X_Format = $1D (* X_D3DFMT_LIN_A4R4G4B4 *) || X_Format == 0x02 /* X_D3DFMT_A1R5G5B5 */
                 or X_Format = $28 (* X_D3DFMT_G8B8 *))
            begin 
                bSwizzled := TRUE;

                // Swizzled 16 Bit
                dwWidth  := 1 shl ((pPixelContainer->Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
                dwHeight := 1 shl ((pPixelContainer->Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
                dwMipMapLevels := (pPixelContainer->Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
                dwDepth  := 1;// HACK? 1 << ((pPixelContainer->Format & X_D3DFORMAT_PSIZE_MASK) >> X_D3DFORMAT_PSIZE_SHIFT);
                dwPitch  := dwWidth*2;
                dwBPP := 2;
             end;
            else if(X_Format = $00 (* X_D3DFMT_L8 *) || X_Format == 0x0B /* X_D3DFMT_P8 */ || X_Format == 0x01 /* X_D3DFMT_AL8 */ || X_Format == 0x1A /* X_D3DFMT_A8L8 */) then 
            begin 
                bSwizzled := TRUE;

                // Swizzled 8 Bit
                dwWidth  := 1 shl ((pPixelContainer->Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
                dwHeight := 1 shl ((pPixelContainer->Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
                dwMipMapLevels := (pPixelContainer->Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
                dwDepth  := 1;// HACK? 1 << ((pPixelContainer->Format & X_D3DFORMAT_PSIZE_MASK) >> X_D3DFORMAT_PSIZE_SHIFT);
                dwPitch  := dwWidth;
                dwBPP := 1;
             end;
            else if(X_Format = $1E (* X_D3DFMT_LIN_X8R8G8B8 *) || X_Format == 0x12 /* X_D3DFORMAT_A8R8G8B8 */ || X_Format == 0x2E /* D3DFMT_LIN_D24S8 */) then 
            begin 
                // Linear 32 Bit
                dwWidth  := (pPixelContainer->Size and X_D3DSIZE_WIDTH_MASK) + 1;
                dwHeight := ((pPixelContainer->Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
                dwPitch  := (((pPixelContainer->Size and X_D3DSIZE_PITCH_MASK) shr X_D3DSIZE_PITCH_SHIFT)+1)*64;
                dwBPP := 4;
             end;
            else if(X_Format = $11 (* D3DFMT_LIN_R5G6B5 *)) then 
            begin 
                // Linear 16 Bit
                dwWidth  := (pPixelContainer->Size and X_D3DSIZE_WIDTH_MASK) + 1;
                dwHeight := ((pPixelContainer->Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
                dwPitch  := (((pPixelContainer->Size and X_D3DSIZE_PITCH_MASK) shr X_D3DSIZE_PITCH_SHIFT)+1)*64;
                dwBPP := 2;
             end;
            else if(X_Format = $0C (* D3DFMT_DXT1 *) || X_Format == 0x0E /* D3DFMT_DXT2 */ || X_Format == 0x0F /* D3DFMT_DXT3 */) then 
            begin 
                bCompressed := TRUE;

                // Compressed
                dwWidth  := 1 shl ((pPixelContainer->Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
                dwHeight := 1 shl ((pPixelContainer->Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
                dwDepth  := 1 shl ((pPixelContainer->Format and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
                dwMipMapLevels := (pPixelContainer->Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;

                // D3DFMT_DXT2...D3DFMT_DXT5 : 128bits per block/per 16 texels
                dwCompressedSize := dwWidth*dwHeight;

                if(X_Format = $0C) then     // D3DFMT_DXT1 : 64bits per block/per 16 texels
                    dwCompressedSize:= dwCompressedSize div 2;

                dwBPP := 1;
             end;
            else if(X_Format = $24 (* D3DFMT_YUY2 *)) then 
            begin 
                // Linear 32 Bit
                dwWidth  := (pPixelContainer->Size and X_D3DSIZE_WIDTH_MASK) + 1;
                dwHeight := ((pPixelContainer->Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
                dwPitch  := (((pPixelContainer->Size and X_D3DSIZE_PITCH_MASK) shr X_D3DSIZE_PITCH_SHIFT)+1)*64;
             end;
            else
            begin 
                CxbxKrnlCleanup("$ mod .08X is not a supported format not ", X_Format);
             end;

            // as we iterate through mipmap levels, we'll adjust the source resource offset
            DWORD dwCompressedOffset := 0;

            DWORD dwMipOffs := 0;
            DWORD dwMipWidth := dwWidth;
            DWORD dwMipHeight := dwHeight;
            DWORD dwMipPitch := dwPitch;

            if(dwMipMapLevels > 6) then 
                dwMipMapLevels := 6;

            // iterate through the number of mipmap levels
            for(uint level:=0;level<dwMipMapLevels;level++)
            begin 
                D3DLOCKED_RECT LockedRect;

                HRESULT hRet := pResource->EmuTexture8->LockRect(level, @LockedRect, 0, 0);

                TRect  iRect  := (0,0,0,0);
                TPoint iPoint := (0,0);

                BYTE *pSrc := (BYTE)pTexture->Data;

                if( IsSpecialResource(pResource->Data) and (pResource->Data and X_D3DRESOURCE_DATA_FLAG_SURFACE)) then 
                begin 

                 end;
                else
                begin 
                    if(bSwizzled) then 
                    begin 
                        if((DWORD)pSrc = $80000000) then 
                        begin 
                            // TODO: Fix or handle this situation..?
                         end;
                        else
                        begin 
                            XTL.EmuXGUnswizzleRect
                            (
                                pSrc + dwMipOffs, dwMipWidth, dwMipHeight, dwDepth, LockedRect.pBits, 
                                LockedRect.Pitch, iRect, iPoint, dwBPP
                            );
                         end;
                     end;
                    else if(bCompressed) then 
                    begin 
                        // NOTE: compressed size is (dwWidth/2)*(dwHeight/2)/2, so each level divides by 4

                        memcpy(LockedRect.pBits, pSrc + dwCompressedOffset, dwCompressedSize shr (level*2));

                        dwCompressedOffset:= dwCompressedOffset + (dwCompressedSize shr (level*2));
                     end;
                    else
                    begin 
                        BYTE *pDest := (BYTE)LockedRect.pBits;

                        if((DWORD)LockedRect.Pitch = dwMipPitch and dwMipPitch = dwMipWidth*dwBPP) then 
                        begin 
                            memcpy(pDest, pSrc + dwMipOffs, dwMipWidth*dwMipHeight*dwBPP);
                         end;
                        else
                        begin 
                            for(DWORD v:=0;v<dwMipHeight;v++)
                            begin 
                                memcpy(pDest, pSrc + dwMipOffs, dwMipWidth*dwBPP);

                                pDest:= pDest + LockedRect.Pitch;
                                pSrc := pSrc + dwMipPitch;
                             end;
                         end;
                     end;
                 end;

                pResource->EmuTexture8->UnlockRect(level);

                dwMipOffs:= dwMipOffs + dwMipWidth*dwMipHeight*dwBPP;

                dwMipWidth:= dwMipWidth div 2;
                dwMipHeight:= dwMipHeight div 2;
                dwMipPitch:= dwMipPitch div 2;
             end;
         end;

        g_pD3DDevice8->SetTexture(Stage, pTexture->EmuTexture8);
        //*/
     end;
 end;


end.
