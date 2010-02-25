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

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows
  , SysUtils // Abort
  , StrUtils
  , Classes
  // Jedi Win32API
  , JwaWinType
  // DirectX
  , Direct3D
  , Direct3D8
  , D3DX8
  // Dxbx
  , uTypes // CLOCKS_PER_SEC, clock()
  , uLog
  , uEmuXG
  , uState
  , uDxbxKrnlUtils
  , uResourceTracker
  , uEmuAlloc
  , uConvert
  , uVertexShader
  , uEmuD3D8Types
  , uEmuD3D8Utils;

const
  MAX_NBR_STREAMS = 16;

type _VertexPatchDesc = packed record
    PrimitiveType: X_D3DPRIMITIVETYPE;
    dwVertexCount: DWORD;
    dwPrimitiveCount: DWORD;
    dwOffset: DWORD;
    // Data if Draw...UP call
    pVertexStreamZeroData: PVOID;
    uiVertexStreamZeroStride: UINT;
    // The current vertex shader, used to identify the streams
    hVertexShader: DWORD;
  end;
  VertexPatchDesc = _VertexPatchDesc;
  PVertexPatchDesc = ^VertexPatchDesc;

type _PATCHEDSTREAM = packed record
    pOriginalStream: IDirect3DVertexBuffer8; // P?
    pPatchedStream: IDirect3DVertexBuffer8; // P?
    uiOrigStride: UINT;
    uiNewStride: UINT;
    bUsedCached: bool;
  end;
  PATCHEDSTREAM = _PATCHEDSTREAM;
  PPATCHEDSTREAM = ^PATCHEDSTREAM;

type _CACHEDSTREAM = packed record
    uiCRC32: uint32;
    uiCheckFrequency: uint32;
    uiCacheHit: uint32;
    bIsUP: bool;
    Stream: PATCHEDSTREAM;
    pStreamUP: Pvoid;            // Draw..UP (instead of pOriginalStream)
    uiLength: uint32;            // The length of the stream
    uiCount: uint32;             // CRC32 check count
    dwPrimitiveCount: uint32;
    lLastUsed: long;             // For cache removal purposes
  end;
  CACHEDSTREAM = _CACHEDSTREAM;
  PCACHEDSTREAM = ^CACHEDSTREAM;

type XTL_VertexPatcher = object
  public
    procedure Create;
    procedure Destroy;

    function Apply(pPatchDesc: PVertexPatchDesc): bool;
    function Restore(): bool;
    // Dumps the cache to the console
    procedure DumpCache();
  private
    m_uiNbrStreams: UINT;
    m_pStreams: array [0..MAX_NBR_STREAMS-1] of PATCHEDSTREAM;

    m_pNewVertexStreamZeroData: PVOID;

    m_bPatched: bool;
    m_bAllocatedStreamZeroData: bool;

    m_pDynamicPatch: PVERTEX_DYNAMIC_PATCH;
    // Returns the number of streams of a patch
    function GetNbrStreams(pPatchDesc: PVertexPatchDesc): UINT;
    // Caches a patched stream
    procedure CacheStream(pPatchDesc: PVertexPatchDesc; uiStream: UINT);
    // Frees a cached, patched stream
    procedure FreeCachedStream(pStream: Pvoid);
    // Tries to apply a previously patched stream from the cache
    function ApplyCachedStream(pPatchDesc: PVertexPatchDesc; uiStream: UINT): bool;
    // Patches the types of the stream
    function PatchStream(pPatchDesc: PVertexPatchDesc; uiStream: UINT): bool;
    // Normalize texture coordinates in FVF stream if needed
    function NormalizeTexCoords(pPatchDesc: PVertexPatchDesc; uiStream: UINT): bool;
    // Patches the primitive of the stream
    function PatchPrimitive(pPatchDesc: PVertexPatchDesc; uiStream: UINT): bool;
  end;

// inline vertex buffer emulation
var g_pIVBVertexBuffer: PDWORD = nil;
var g_IVBPrimitiveType: X_D3DPRIMITIVETYPE = X_D3DPT_INVALID;
var g_IVBFVF: DWORD = 0;
var g_CurrentVertexShader: DWord = 0;

type _D3DIVB = packed record
    Position: TD3DXVECTOR3; // Position
    Rhw: FLOAT; // Rhw
    Blend1: FLOAT; // Blend1
    dwSpecular: DWORD; // Specular
    dwDiffuse: DWORD; // Diffuse
    Normal: TD3DXVECTOR3; // Normal
    TexCoord1: TD3DXVECTOR2; // TexCoord1
    TexCoord2: TD3DXVECTOR2; // TexCoord2
    TexCoord3: TD3DXVECTOR2; // TexCoord3
    TexCoord4: TD3DXVECTOR2; // TexCoord4
  end;
  D3DIVB = _D3DIVB;

  PD3DIVB = ^D3DIVB;
  TD3DIVBArray = array [0..(MaxInt div SizeOf(D3DIVB)) - 1] of D3DIVB;
  PD3DIVBs = ^TD3DIVBArray;

procedure XTL_EmuFlushIVB; stdcall;
procedure XTL_EmuUpdateActiveTexture; stdcall;

const VERTEX_BUFFER_CACHE_SIZE = 64;
const MAX_STREAM_NOT_USED_TIME = (2 * CLOCKS_PER_SEC); // Cxbx TODO: Trim the not used time

// inline vertex buffer emulation
var g_IVBTblOffs: UINT = 0;
var g_IVBTable: PD3DIVBs = nil;

implementation

uses
  uEmuD3D8;

var crctab: array [0..256-1] of uint;

procedure CRC32Init;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
{$WRITEABLECONST ON}
const
  bFirstTime: boolean = True;
{$WRITEABLECONST OFF}
var
  i, j: int;
  crc: uint;
begin
  if not bFirstTime then
    Exit;

  for i := 0 to 256 - 1 do
  begin
    crc := i shl 24;
    for j := 0 to 8 - 1 do
    begin
      if (crc and $80000000) > 0 then
        crc := (crc shl 1) xor $04C11DB7
      else
        crc := crc shl 1;
    end;

    crctab[i] := crc;
  end;

  bFirstTime := false;
end;

function CRC32(data: PByte; len: int): uint;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  i: int;
begin
  if len < 4 then
    Abort;

  Result :=           (data^ shl 24); Inc(data);
  Result := Result or (data^ shl 16); Inc(data);
  Result := Result or (data^ shl  8); Inc(data);
  Result := Result or  data^        ; Inc(data);
  Result := not Result;
  Dec(len, 4);

  for i := 0 to len - 1 do
  begin
    Result := ((Result shl 8) or data^) xor crctab[Result shr 24];
    Inc(Data);
  end;

  Result := not Result;
end;


procedure XTL_VertexPatcher.Create;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  m_uiNbrStreams := 0;
  ZeroMemory(@(m_pStreams[0]), SizeOf(PATCHEDSTREAM) * MAX_NBR_STREAMS);
  m_bPatched := false;
  m_bAllocatedStreamZeroData := false;
  m_pNewVertexStreamZeroData := NULL;
  m_pDynamicPatch := NULL;
  CRC32Init();
end;

procedure XTL_VertexPatcher.Destroy;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
end;

procedure XTL_VertexPatcher.DumpCache();
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pNode: PRTNode;
  pCachedStream_: PCACHEDSTREAM;
begin
  DbgPrintf('--- Dumping streams cache ---');

  pNode := g_PatchedStreamsCache.getHead();
  while Assigned(pNode) do
  begin
    pCachedStream_ := PCACHEDSTREAM(pNode.pResource);
    if Assigned(pCachedStream_) then
    begin
      // Cxbx TODO: Write nicer dump presentation
      DbgPrintf('Key: 0x%.08X Cache Hits: %d IsUP: %s OrigStride: %d NewStride: %d CRCCount: %d CRCFreq: %d Lengh: %d CRC32: 0x%.08X',
             [pNode.uiKey, pCachedStream_.uiCacheHit, ifThen(pCachedStream_.bIsUP, 'YES', 'NO'),
             pCachedStream_.Stream.uiOrigStride, pCachedStream_.Stream.uiNewStride,
             pCachedStream_.uiCount, pCachedStream_.uiCheckFrequency,
             pCachedStream_.uiLength, pCachedStream_.uiCRC32]);
    end;

    pNode := pNode.pNext;
  end;
end;

procedure XTL_VertexPatcher.CacheStream(pPatchDesc: PVertexPatchDesc;
                                        uiStream: UINT);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pCachedStream_: PCACHEDSTREAM;
  uiKey: uint32;
  uiMinHit: uint32;
  pNode: PRTNode;
  pOrigVertexBuffer: IDirect3DVertexBuffer8;
  Desc: D3DVERTEXBUFFER_DESC;
  pCalculateData: Pointer;
  uiLength: UINT;
{  uiStride: UINT;} // DXBX, uiStride never used
  uiChecksum: UINT;
begin
  pCachedStream_ := PCACHEDSTREAM(CxbxMalloc(SizeOf(CACHEDSTREAM)));

  ZeroMemory(pCachedStream_, SizeOf(CACHEDSTREAM));

  // Check if the cache is full, if so, throw away the least used stream
  if (g_PatchedStreamsCache.get_count() > VERTEX_BUFFER_CACHE_SIZE) then
  begin
    uiKey := 0;
    uiMinHit := $FFFFFFFF;

    pNode := g_PatchedStreamsCache.getHead();
    while Assigned(pNode) do
    begin
      if Assigned(pNode.pResource) then
      begin
        // First, check if there is an 'expired' stream in the cache (not recently used)
        if (DWord(PCACHEDSTREAM(pNode.pResource).lLastUsed) < (clock() + MAX_STREAM_NOT_USED_TIME)) then
        begin
{$IFDEF DEBUG}
          printf('!!!Found an old stream, %2.2f', [{FLOAT}((clock() + MAX_STREAM_NOT_USED_TIME) - DWord(PCACHEDSTREAM(pNode.pResource).lLastUsed)) / {FLOAT}(CLOCKS_PER_SEC)]);
{$ENDIF}
          uiKey := pNode.uiKey;
          Break;
        end;
        // Find the least used cached stream
        if (uint32(PCACHEDSTREAM(pNode.pResource).uiCacheHit) < uiMinHit) then
        begin
          uiMinHit := PCACHEDSTREAM(pNode.pResource).uiCacheHit;
          uiKey := pNode.uiKey;
        end;
      end;
      pNode := pNode.pNext;
    end;
    if (uiKey <> 0) then
    begin
{$IFDEF DEBUG}
      printf('!!!Removing stream');
{$ENDIF}
      FreeCachedStream(Pvoid(uiKey));
    end;
  end;

  // Start the actual stream caching
  if not Assigned(pPatchDesc.pVertexStreamZeroData) then
  begin
    pOrigVertexBuffer := m_pStreams[uiStream].pOriginalStream;
    pOrigVertexBuffer._AddRef();
    m_pStreams[uiStream].pPatchedStream._AddRef();
    if (FAILED(pOrigVertexBuffer.GetDesc({out}Desc))) then
    begin
      CxbxKrnlCleanup('Could not retrieve original buffer size');
    end;
    if (FAILED(pOrigVertexBuffer.Lock(0, 0, {out}PByte(pCalculateData), 0))) then
    begin
      CxbxKrnlCleanup('Couldn''t lock the original buffer');
    end;

    uiLength := Desc.Size;
    pCachedStream_.bIsUP := false;
    uiKey := uint32(pOrigVertexBuffer);
  end
  else
  begin
    // There should only be one stream (stream zero) in this case
    if (uiStream <> 0) then
    begin
      CxbxKrnlCleanup('Trying to patch a Draw..UP with more than stream zero!');
    end;
    {uiStride := pPatchDesc.uiVertexStreamZeroStride;} // DXBX, uiStride never used
    pCalculateData := Puint08(pPatchDesc.pVertexStreamZeroData);
    // Cxbx TODO: This is sometimes the number of indices, which isn't too good
    uiLength := pPatchDesc.dwVertexCount * pPatchDesc.uiVertexStreamZeroStride;
    pCachedStream_.bIsUP := true;
    pCachedStream_.pStreamUP := pCalculateData;
    uiKey := uint32(pCalculateData);
  end;

  uiChecksum := CRC32(PByte(pCalculateData), uiLength);
  if (pPatchDesc.pVertexStreamZeroData = nil) then
  begin
    pOrigVertexBuffer.Unlock();
  end;

  pCachedStream_.uiCRC32 := uiChecksum;
  pCachedStream_.Stream := m_pStreams[uiStream];
  pCachedStream_.uiCheckFrequency := 1; // Start with checking every 1th Draw..
  pCachedStream_.uiCount := 0;
  pCachedStream_.uiLength := uiLength;
  pCachedStream_.uiCacheHit := 0;
  pCachedStream_.dwPrimitiveCount := pPatchDesc.dwPrimitiveCount;
  pCachedStream_.lLastUsed := clock();
  g_PatchedStreamsCache.insert(uiKey, pCachedStream_);
end;


procedure XTL_VertexPatcher.FreeCachedStream(pStream: PVoid);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pCachedStream_: PCACHEDSTREAM;
begin
  g_PatchedStreamsCache.Lock();
  pCachedStream_ := PCACHEDSTREAM(g_PatchedStreamsCache.get(pStream));
  if Assigned(pCachedStream_) then
  begin
    if pCachedStream_.bIsUP and Assigned(pCachedStream_.pStreamUP) then
    begin
      CxbxFree(pCachedStream_.pStreamUP);
    end;
    if Assigned(pCachedStream_.Stream.pOriginalStream) then
    begin
      pCachedStream_.Stream.pOriginalStream._Release();
    end;
    if Assigned(pCachedStream_.Stream.pPatchedStream) then
    begin
      pCachedStream_.Stream.pPatchedStream._Release();
    end;
    CxbxFree(pCachedStream_);
  end;
  g_PatchedStreamsCache.Unlock();
  g_PatchedStreamsCache.remove(pStream);
end;

function XTL_VertexPatcher.ApplyCachedStream(pPatchDesc: PVertexPatchDesc; 
                                             uiStream: UINT): bool;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:10
var
  bApplied: bool;
(*  uiStride: UINT;
  pOrigVertexBuffer: IDirect3DVertexBuffer8;
  Desc: D3DVERTEXBUFFER_DESC; *)
begin
(*    procedure                      *pCalculateData;
    UINT                       uiLength;  *)
    bApplied := False;
    (*
    uint32                     uiKey;
    //CACHEDSTREAM              *pCachedStream = (CACHEDSTREAM *)(*CxbxMalloc(sizeof(CACHEDSTREAM));

    if (not pPatchDesc.pVertexStreamZeroData) then
    begin
        g_pD3DDevice8.GetStreamSource(uiStream, {out}IDirect3DVertexBuffer8(pOrigVertexBuffer), @uiStride);
        if (FAILED(pOrigVertexBuffer.GetDesc(@Desc))) then
        begin
            CxbxKrnlCleanup('Could not retrieve original buffer size');
         end;
        uiLength := Desc.Size;
        uiKey := uint32(pOrigVertexBuffer);
        //pCachedStream.bIsUP = False;
    end
    else
    begin
        // There should only be one stream (stream zero) in this case
        if (uiStream <> 0) then
        begin
            CxbxKrnlCleanup('Trying to find a cached Draw..UP with more than stream zero!');
        end;
        uiStride := pPatchDesc.uiVertexStreamZeroStride;
        pCalculateData := Puint08(pPatchDesc.pVertexStreamZeroData);
        // Cxbx TODO: This is sometimes the number of indices, which isn't too good
        uiLength := pPatchDesc.dwVertexCount * pPatchDesc.uiVertexStreamZeroStride;
        uiKey := uint32(pCalculateData);
        //pCachedStream.bIsUP = True;
        //pCachedStream.pStreamUP = pCalculateData;
    end;
    g_PatchedStreamsCache.Lock();

    CACHEDSTREAM *pCachedStream := PCACHEDSTREAM(g_PatchedStreamsCache.get(uiKey));
    if (pCachedStream) then
    begin
        pCachedStream.lLastUsed := clock();
        Inc(pCachedStream.uiCacheHit);
        bool bMismatch := False;
        if (pCachedStream.uiCount = (pCachedStream.uiCheckFrequency - 1)) then
        begin
            if (not pPatchDesc.pVertexStreamZeroData) then
            begin
                if (FAILED(pOrigVertexBuffer.Lock(0, 0, PPuint08(@pCalculateData), 0))) then
                begin
                    CxbxKrnlCleanup('Couldn't lock the original buffer');
                end;
            end;
            // Use the cached stream length (which is a must for the UP stream)
            uint32 Checksum := CRC32(Puint08(pCalculateData), pCachedStream.uiLength);
            if (Checksum = pCachedStream.uiCRC32) then
            begin
                // Take a while longer to check
                if (pCachedStream.uiCheckFrequency < 32*1024) then
                begin
                  pCachedStream.uiCheckFrequency:= pCachedStream.uiCheckFrequency * 2;
                end;
                pCachedStream.uiCount := 0;
            end
            else
            begin
                // Cxbx TODO: Do something about this
                if (pCachedStream.bIsUP) then
                begin
                    FreeCachedStream(pCachedStream.pStreamUP);
                end
                else
                begin
                    FreeCachedStream(pCachedStream.Stream.pOriginalStream);
                end;
                pCachedStream := 0;
                bMismatch := True;
            end;
            if (not pPatchDesc.pVertexStreamZeroData) then
            begin
                pOrigVertexBuffer.Unlock();
            end;
        end
        else
        begin
            pCachedStream.uiCount:= pCachedStream.uiCount + 1;
        end;
        if (not bMismatch) then
        begin
            if (not pCachedStream.bIsUP) then
            begin
                m_pStreams[uiStream].pOriginalStream := pOrigVertexBuffer;
                m_pStreams[uiStream].uiOrigStride := uiStride;
                g_pD3DDevice8.SetStreamSource(uiStream, pCachedStream.Stream.pPatchedStream, pCachedStream.Stream.uiNewStride);
                pCachedStream.Stream.pPatchedStream._AddRef();
                pCachedStream.Stream.pOriginalStream._AddRef();
                m_pStreams[uiStream].pPatchedStream := pCachedStream.Stream.pPatchedStream;
                m_pStreams[uiStream].uiNewStride := pCachedStream.Stream.uiNewStride;
            end
            else
            begin
                pPatchDesc.pVertexStreamZeroData := pCachedStream.pStreamUP;
                pPatchDesc.uiVertexStreamZeroStride := pCachedStream.Stream.uiNewStride;
             end;
            if (pCachedStream.dwPrimitiveCount) then
            begin
                // The primitives were patched, draw with the correct number of primimtives from the cache
                pPatchDesc.dwPrimitiveCount := pCachedStream.dwPrimitiveCount;
            end;
            bApplied := True;
            m_bPatched := True;
         end;
     end;
    g_PatchedStreamsCache.Unlock();

  if (not pPatchDesc.pVertexStreamZeroData) then
  begin
    pOrigVertexBuffer._Release();
  end;     *)

  Result := bApplied;
end;


function XTL_VertexPatcher.GetNbrStreams(pPatchDesc: PVertexPatchDesc): UINT;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pDynamicPatch: PVERTEX_DYNAMIC_PATCH;
begin
  if (VshHandleIsVertexShader(g_CurrentVertexShader)) then
  begin
    pDynamicPatch := XTL_VshGetVertexDynamicPatch(g_CurrentVertexShader);
    if Assigned(pDynamicPatch) then
    begin
      Result := pDynamicPatch.NbrStreams;
      Exit;
    end
    else
    begin
      Result := 1; // Could be more, but it doesn't matter as we're not going to patch the types
      Exit;
    end;
  end
  else if g_CurrentVertexShader > 0 then
  begin
    Result := 1;
    Exit;
  end;
  Result := 0;
end;

function XTL_VertexPatcher.PatchStream(pPatchDesc: PVertexPatchDesc;
                                       uiStream: UINT): bool;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:80
var
  pStream: PPATCHEDSTREAM;

  pOrigVertexBuffer: IDirect3DVertexBuffer8;
  pNewVertexBuffer: IDirect3DVertexBuffer8;
  pOrigData: Puint08;
  pNewData: Puint08;
  uiStride: UINT;
  Desc: D3DVERTEXBUFFER_DESC;
  pStreamPatch: PSTREAM_DYNAMIC_PATCH;
  dwNewSize: DWORD;

  uiVertex: uint32;
  dwPosOrig: DWORD;
  dwPosNew: DWORD;
  uiType: UINT;

begin
    pStream := nil;

    // FVF buffers doesn't have Xbox extensions, but texture coordinates may
    // need normalization if used with linear textures.
    if (not VshHandleIsVertexShader(pPatchDesc.hVertexShader)) then
    begin
      if (pPatchDesc.hVertexShader and D3DFVF_TEXCOUNT_MASK) > 0 then
      begin
        Result := NormalizeTexCoords(pPatchDesc, uiStream);
        Exit;
      end
      else
      begin
        Result := False;
        Exit;
      end;
    end;

    if not (Assigned(m_pDynamicPatch)) or (not m_pDynamicPatch.pStreamPatches[uiStream].NeedPatch) then
    begin
      Result := False;
      Exit;
    end;

    // Do some groovey patchin'
    
    pStreamPatch := @m_pDynamicPatch.pStreamPatches[uiStream];

    if not Assigned(pPatchDesc.pVertexStreamZeroData) then
    begin
        g_pD3DDevice8.GetStreamSource(uiStream, {out}IDirect3DVertexBuffer8(pOrigVertexBuffer), uiStride);
        if (FAILED(pOrigVertexBuffer.GetDesc(Desc))) then
        begin
          CxbxKrnlCleanup('Could not retrieve original buffer size');
        end;
        // Set a new (exact) vertex count
        pPatchDesc.dwVertexCount := Desc.Size div uiStride;
        dwNewSize := pPatchDesc.dwVertexCount * pStreamPatch.ConvertedStride;

        if (FAILED(pOrigVertexBuffer.Lock(0, 0, PByte(pOrigData), 0))) then
        begin
          CxbxKrnlCleanup('Couldn`t lock the original buffer');
        end;
        IDirect3DDevice8_CreateVertexBuffer(g_pD3DDevice8, dwNewSize, 0, 0, D3DPOOL_MANAGED, @pNewVertexBuffer);
        if (FAILED(pNewVertexBuffer.Lock(0, 0, PByte(pNewData), 0))) then
        begin
          CxbxKrnlCleanup('Couldn`t lock the new buffer');
        end;
        if not Assigned(pStream.pOriginalStream) then
        begin
            // The stream was not previously patched, we'll need this when restoring
            pStream.pOriginalStream := pOrigVertexBuffer;
        end;
    end
    else
    begin
        // There should only be one stream (stream zero) in this case
        if (uiStream <> 0) then
        begin
            CxbxKrnlCleanup('Trying to patch a Draw..UP with more than stream zero!');
        end;
        uiStride  := pPatchDesc.uiVertexStreamZeroStride;
        pOrigData := pPatchDesc.pVertexStreamZeroData;
        // Cxbx TODO: This is sometimes the number of indices, which isn't too good
        dwNewSize := pPatchDesc.dwVertexCount * pStreamPatch.ConvertedStride;
        pNewVertexBuffer := nil;
        pNewData := CxbxMalloc(dwNewSize);
        if not Assigned(pNewData) then
        begin
           CxbxKrnlCleanup('Couldn`t allocate the new stream zero buffer');
        end;
    end;

    for uiVertex := 0 to pPatchDesc.dwVertexCount - 1 do
    begin
        (*dwPosOrig := 0;
        dwPosNew := 0;
        for uiType := 0 to pStreamPatch.NbrTypes - 1 do
        begin
            case(pStreamPatch.pTypes[uiType]) of
                 $12: begin // FLOAT1
                    memcpy(@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           SizeOf(FLOAT));
                    dwPosOrig:= dwPosOrig + SizeOf(FLOAT);
                    dwPosNew := dwPosNew + SizeOf(FLOAT);
                    end;
                 $22: begin // FLOAT2
                    memcpy(@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           2 * SizeOf(FLOAT));
                    dwPosOrig:= dwPosOrig + 2 * SizeOf(FLOAT);
                    dwPosNew := dwPosNew + 2 * SizeOf(FLOAT);
                    end;
                 $32: begin // FLOAT3
                    memcpy(@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           3 * SizeOf(FLOAT));
                    dwPosOrig:= dwPosOrig + 3 * SizeOf(FLOAT);
                    dwPosNew := dwPosNew + 3 * SizeOf(FLOAT);
                    end;
                 $42: begin // FLOAT4
                    memcpy(@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           4 * SizeOf(FLOAT));
                    dwPosOrig:= dwPosOrig + 4 * SizeOf(FLOAT);
                    dwPosNew := dwPosNew + 4 * SizeOf(FLOAT);
                    end;
                 $40: begin // D3DCOLOR
                    memcpy(@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           SizeOf(XTL.D3DCOLOR));
                    dwPosOrig:= dwPosOrig + SizeOf(XTL.D3DCOLOR);
                    dwPosNew := dwPosNew + SizeOf(XTL.D3DCOLOR);
                    end;
                 $16: begin //NORMPACKED3
                        DWORD dwPacked := ((DWORD )@pOrigData[uiVertex * uiStride + dwPosOrig])[0];

                        ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[0] := ((FLOAT)(dwPacked and $7ff)) / 1023.0f;
                        ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[1] := ((FLOAT)((dwPacked shr 11) and $7ff)) / 1023.0f;
                        ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[2] := ((FLOAT)((dwPacked shr 22) and $3ff)) / 511.0f;

                        dwPosOrig:= dwPosOrig + SizeOf(DWORD);
                        dwPosNew := dwPosNew + 3 * SizeOf(FLOAT);
                   end;
                 $15: begin// SHORT1
                    // Make it a SHORT2
                    (((SmallInt )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew + 0 * SizeOf(SmallInt)])) := *(SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig];
                    (((SmallInt )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew + 1 * SizeOf(SmallInt)])) := $00;

                    dwPosOrig:= dwPosOrig + 1 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 2 * SizeOf(SmallInt);

                    end;
                 $25: begin // SHORT2
                    memcpy(@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride+dwPosOrig],
                           2 * SizeOf(SmallInt));
                    dwPosOrig:= dwPosOrig + 2 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 2 * SizeOf(SmallInt);
                    end;
                 $35: begin // SHORT3
                    memcpy(@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           3 * SizeOf(SmallInt));
                    // Make it a SHORT4 and set the last short to 1
                    (((SmallInt )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew + 3 * SizeOf(SmallInt)])) := $01;

                    dwPosOrig:= dwPosOrig + 3 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 4 * SizeOf(SmallInt);

                    end;
                 $45: begin // SHORT4
                    memcpy(@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew],
                           @pOrigData[uiVertex * uiStride + dwPosOrig],
                           4 * SizeOf(SmallInt));
                    dwPosOrig:= dwPosOrig + 4 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 4 * SizeOf(SmallInt);
                    end;
                 $14: begin // PBYTE1
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[0] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 255.0f;

                    dwPosOrig:= dwPosOrig + 1 * SizeOf(BYTE);
                    dwPosNew := dwPosNew + 1 * SizeOf(FLOAT);

                    end;
                 $24: begin // PBYTE2
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[0] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[1] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 255.0f;

                    dwPosOrig:= dwPosOrig + 2 * SizeOf(BYTE);
                    dwPosNew := dwPosNew + 2 * SizeOf(FLOAT);

                    end;
                 $34: begin // PBYTE3
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[0] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[1] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[2] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[2]) / 255.0f;

                    dwPosOrig:= dwPosOrig + 3 * SizeOf(BYTE);
                    dwPosNew := dwPosNew + 3 * SizeOf(FLOAT);

                    end;
                 $44: begin // PBYTE4
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[0] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[1] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[2] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[2]) / 255.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[3] := ((FLOAT)((BYTE)@pOrigData[uiVertex * uiStride + dwPosOrig])[3]) / 255.0f;

                    dwPosOrig:= dwPosOrig + 4 * SizeOf(BYTE);
                    dwPosNew := dwPosNew + 4 * SizeOf(FLOAT);

                    end;
                 $11: begin // NORMSHORT1
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[0] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 32767.0f;

                    dwPosOrig:= dwPosOrig + 1 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 1 * SizeOf(FLOAT);
                    end;
                 $21: begin // NORMSHORT2
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[0] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[1] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 32767.0f;

                    dwPosOrig:= dwPosOrig + 2 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 2 * SizeOf(FLOAT);
                    end;
                 $31: begin // NORMSHORT3
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[0] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[1] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[2] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[2]) / 32767.0f;

                    dwPosOrig:= dwPosOrig + 3 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 3 * SizeOf(FLOAT);
                    end;
                 $41: begin// NORMSHORT4
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[0] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[0]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[1] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[1]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[2] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[2]) / 32767.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[3] := ((FLOAT)((SmallInt)@pOrigData[uiVertex * uiStride + dwPosOrig])[3]) / 32767.0f;

                    dwPosOrig:= dwPosOrig + 4 * SizeOf(SmallInt);
                    dwPosNew := dwPosNew + 4 * SizeOf(FLOAT);
                    end;
                 $72: begin// FLOAT2H
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[0] := ((FLOAT)@pOrigData[uiVertex * uiStride + dwPosOrig])[0];
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[1] := ((FLOAT)@pOrigData[uiVertex * uiStride + dwPosOrig])[1];
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[2] := 0.0f;
                    ((FLOAT )@pNewData[uiVertex * pStreamPatch.ConvertedStride + dwPosNew])[3] := ((FLOAT)@pOrigData[uiVertex * uiStride + dwPosOrig])[2];

                (*Cxbx TODO
                 $02:
{$IFDEF DEBUG}
                    printf('D3DVSDT_NONE / xbox ext. nsp /');
{$ENDIF}
                    dwNewDataType := $FF;
                    end;
                default:
                    CxbxKrnlCleanup('Unhandled stream type: 0x%.02X', pStreamPatch.pTypes[uiType]);
                    end;
             end;
         end; *)
    end;
    if not Assigned(pPatchDesc.pVertexStreamZeroData) then
    begin
        pNewVertexBuffer.Unlock();
        pOrigVertexBuffer.Unlock();

        if (FAILED(g_pD3DDevice8.SetStreamSource(uiStream, pNewVertexBuffer, pStreamPatch.ConvertedStride))) then
        begin
            CxbxKrnlCleanup('Failed to set the type patched buffer as the new stream source!');
        end;
        if Assigned(pStream.pPatchedStream) then
        begin
            // The stream was already primitive patched, release the previous vertex buffer to avoid memory leaks
            pStream.pPatchedStream._Release();
        end;
        pStream.pPatchedStream := pNewVertexBuffer;
    end
    else
    begin
        pPatchDesc.pVertexStreamZeroData := pNewData;
        pPatchDesc.uiVertexStreamZeroStride := pStreamPatch.ConvertedStride;
        if ( not m_bAllocatedStreamZeroData) then
        begin
            // The stream was not previously patched. We'll need this when restoring
            m_bAllocatedStreamZeroData := True;
            m_pNewVertexStreamZeroData := pNewData;
        end;
    end;
    pStream.uiOrigStride := uiStride;
    pStream.uiNewStride := pStreamPatch.ConvertedStride;
    m_bPatched := True;

    Result := True;
end;

function XTL_VertexPatcher.NormalizeTexCoords(pPatchDesc: PVertexPatchDesc; uiStream: UINT): bool;
// Branch:shogun  Revision:  Translator:PatrickvL  Done:100
var
  bHasLinearTex: bool;
  bTexIsLinear: array [0..4-1] of bool;
  pLinearPixelContainer: array [0..4-1] of PX_D3DPixelContainer;
  i: uint08;
  pPixelContainer: PX_D3DPixelContainer;

  pOrigVertexBuffer: IDirect3DVertexBuffer8;
  pNewVertexBuffer: IDirect3DVertexBuffer8;
  pStream: PPATCHEDSTREAM;
  pData: PByte;
  pUVData: Puint08;
  uiStride: uint;
  uiVertexCount: uint;

  Desc: D3DVERTEXBUFFER_DESC;
  pOrigData: PByte;
  uiOffset: uint;
  dwTexN: DWORD;
  uiVertex: uint32;
begin
    // Check for active linear textures.
    bHasLinearTex := false;
    pStream := nil; // DXBX - pstream might not have been initialized

    for i := 0 to 4 - 1 do
    begin
        pPixelContainer := PX_D3DPixelContainer(EmuD3DActiveTexture[i]);
        if (Assigned(pPixelContainer) and EmuXBFormatIsLinear((X_D3DFORMAT(pPixelContainer.Format) and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT)) then
        begin
            bHasLinearTex := true; bTexIsLinear[i] := true;
            pLinearPixelContainer[i] := pPixelContainer;
        end
        else
        begin
            bTexIsLinear[i] := false;
        end
    end;

    if (not bHasLinearTex) then
    begin
      Result := False;
      Exit;
    end;

    if Assigned(pPatchDesc.pVertexStreamZeroData) then
    begin
        // In-place patching of inline buffer.
        pNewVertexBuffer := nil;
        pData := (*Puint08*)(pPatchDesc.pVertexStreamZeroData);
        uiStride := pPatchDesc.uiVertexStreamZeroStride;
        uiVertexCount := pPatchDesc.dwVertexCount;
    end
    else
    begin
        // Copy stream for patching and caching.
        g_pD3DDevice8.GetStreamSource(uiStream, pOrigVertexBuffer, uiStride);

        if(FAILED(pOrigVertexBuffer.GetDesc(Desc))) then
        begin
            CxbxKrnlCleanup('Could not retrieve original FVF buffer size.');
        end;
        uiVertexCount := Desc.Size div uiStride;

        if(FAILED(pOrigVertexBuffer.Lock(0, 0, pOrigData, 0))) then
        begin
            CxbxKrnlCleanup('Couldn''t lock original FVF buffer.');
        end;
        g_pD3DDevice8.CreateVertexBuffer(Desc.Size, 0, 0, D3DPOOL_MANAGED, pNewVertexBuffer);
        if(FAILED(pNewVertexBuffer.Lock(0, 0, pData, 0))) then
        begin
            CxbxKrnlCleanup('Couldn''t lock new FVF buffer.');
        end;
        memcpy(pData, pOrigData, Desc.Size);
        pOrigVertexBuffer.Unlock();

        pStream := @m_pStreams[uiStream];
        if not assigned(pStream.pOriginalStream) then
        begin
            pStream.pOriginalStream := pOrigVertexBuffer;
        end;
    end;

    // Locate texture coordinate offset in vertex structure.
    uiOffset := 0;
    if (pPatchDesc.hVertexShader and D3DFVF_XYZRHW) > 0  then
      Inc(uiOffset, (sizeof(FLOAT) * 4))
    else
    begin
        if (pPatchDesc.hVertexShader and D3DFVF_XYZ) > 0 then
            Inc(uiOffset, (sizeof(FLOAT) * 3 ))
        else if (pPatchDesc.hVertexShader and D3DFVF_XYZB1) > 0 then
            Inc(uiOffset, (sizeof(FLOAT) *4 ))
        else if (pPatchDesc.hVertexShader and D3DFVF_XYZB2) > 0 then
            Inc(uiOffset, (sizeof(FLOAT) * 5))
        else if (pPatchDesc.hVertexShader and D3DFVF_XYZB3) > 0 then
            Inc(uiOffset, (sizeof(FLOAT) * 6))
        else if (pPatchDesc.hVertexShader and D3DFVF_XYZB4) > 0 then
            Inc (uiOffset, (sizeof(FLOAT) * 7));

        if (pPatchDesc.hVertexShader and D3DFVF_NORMAL) > 0 then
            Inc(uiOffset, (sizeof(FLOAT) * 3));
    end;

    if(pPatchDesc.hVertexShader and D3DFVF_DIFFUSE) > 0 then
        Inc(uiOffset,sizeof(DWORD));
    if (pPatchDesc.hVertexShader and D3DFVF_SPECULAR) > 0 then
        Inc(uiOffset, sizeof(DWORD));

    dwTexN := (pPatchDesc.hVertexShader and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT;

    // Normalize texture coordinates.
    for uiVertex := 0 to uiVertexCount - 1 do
    begin
        pUVData := Puint08(pData + (uiVertex * uiStride) + uiOffset);

        if (dwTexN >= 1) then
        begin
            if (bTexIsLinear[0]) then
            begin
                pUVData[0] := pUVData[0] div (pLinearPixelContainer[0].Size and X_D3DSIZE_WIDTH_MASK) + 1;
                pUVData[1] := pUVData[1] div ((pLinearPixelContainer[0].Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
            end;
            Inc(PByte(pUVData), sizeof(FLOAT) * 2);
        end;

        if (dwTexN >= 2) then
        begin
            if (bTexIsLinear[1]) then
            begin
              pUVData[0] := pUVData[0] div ( pLinearPixelContainer[1].Size and X_D3DSIZE_WIDTH_MASK) + 1;
              pUVData[1] := pUVData[1] div ((pLinearPixelContainer[1].Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
            end;
            Inc(PByte(pUVData), sizeof(FLOAT) * 2);
        end;

        if (dwTexN >= 3) then
        begin
            if (bTexIsLinear[2]) then
            begin
                pUVData[0] := pUVData[0] div ( pLinearPixelContainer[2].Size and X_D3DSIZE_WIDTH_MASK) + 1;
                pUVData[1] := pUVData[1] div ((pLinearPixelContainer[2].Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
            end;
            Inc(PByte(pUVData), sizeof(FLOAT) * 2);
        end;

        if((dwTexN >= 4) and bTexIsLinear[3]) then
        begin
            pUVData[0] := pUVData[0] div ( pLinearPixelContainer[3].Size and X_D3DSIZE_WIDTH_MASK) + 1;
            pUVData[1] := pUVData[1] div ((pLinearPixelContainer[3].Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
        end;
    end;

    if Assigned(pNewVertexBuffer) then
    begin
        pNewVertexBuffer.Unlock();

        if (FAILED(g_pD3DDevice8.SetStreamSource(uiStream, pNewVertexBuffer, uiStride))) then
        begin
            CxbxKrnlCleanup('Failed to set the texcoord patched FVF buffer as the new stream source.');
        end;
        if Assigned(pStream.pPatchedStream) then
        begin
            pStream.pPatchedStream._Release();
        end;

        pStream.pPatchedStream := pNewVertexBuffer;
        pStream.uiOrigStride := uiStride;
        pStream.uiNewStride := uiStride;
        m_bPatched := true;
    end;

    Result := m_bPatched;

end;

function XTL_VertexPatcher.PatchPrimitive(pPatchDesc: PVertexPatchDesc; uiStream: UINT): bool;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:55
var
  pStream: PPATCHEDSTREAM;
  dwOriginalSize: DWORD;
  dwNewSize: DWORD;
  dwOriginalSizeWR: DWORD;
  dwNewSizeWR: DWORD;
  pOrigVertexData: PBYTE;
  pPatchedVertexData: PBYTE;
  Desc: D3DVERTEXBUFFER_DESC;

  pPatch1: Puint08;
  pPatch2: Puint08;
  pPatch3: Puint08;
  pPatch4: Puint08;

  pOrig1: Puint08;
  pOrig2: Puint08;
  pOrig3: Puint08;
  i: uint32;
  z: Integer;
begin
    pStream := @m_pStreams[uiStream];
    // only quad and listloop are currently supported
    if ((pPatchDesc.PrimitiveType <> X_D3DPT_QUADLIST) and (pPatchDesc.PrimitiveType <> X_D3DPT_LINELOOP)) then
        Result := False;

    if Assigned(pPatchDesc.pVertexStreamZeroData) and (uiStream > 0) then
    begin
        CxbxKrnlCleanup('Draw..UP call with more than one stream!');
    end;

    pStream.uiOrigStride := 0;

    // sizes of our part in the vertex buffer
    dwOriginalSize    := 0;
    dwNewSize         := 0;

    // vertex data arrays
    pOrigVertexData := nil;
    pPatchedVertexData := nil;

    if not Assigned(pPatchDesc.pVertexStreamZeroData) then
    begin
        g_pD3DDevice8.GetStreamSource(0, {out}IDirect3DVertexBuffer8(pStream.pOriginalStream), pStream.uiOrigStride);
        pStream.uiNewStride := pStream.uiOrigStride; // The stride is still the same

        if (pPatchDesc.PrimitiveType = X_D3DPT_QUADLIST) then
        begin
            pPatchDesc.dwPrimitiveCount:= pPatchDesc.dwPrimitiveCount * 2;

            // This is a list of sqares/rectangles, so we convert it to a list of triangles
            dwOriginalSize  := pPatchDesc.dwPrimitiveCount * pStream.uiOrigStride * 2;
            dwNewSize       := pPatchDesc.dwPrimitiveCount * pStream.uiOrigStride * 3;
        end
        // LineLoop
        else if (pPatchDesc.PrimitiveType = X_D3DPT_LINELOOP) then
        begin
            pPatchDesc.dwPrimitiveCount:= pPatchDesc.dwPrimitiveCount + 1;

            // We will add exactly one more line
            dwOriginalSize  := pPatchDesc.dwPrimitiveCount * pStream.uiOrigStride;
            dwNewSize       := pPatchDesc.dwPrimitiveCount * pStream.uiOrigStride + pStream.uiOrigStride;
         end;

        // Retrieve the original buffer size
        begin
            if (FAILED(pStream.pOriginalStream.GetDesc(Desc))) then
            begin
                CxbxKrnlCleanup('Could not retrieve buffer size');
             end;

            // Here we save the full buffer size
            dwOriginalSizeWR := Desc.Size;

            // So we can now calculate the size of the rest (dwOriginalSizeWR - dwOriginalSize) and
            // add it to our new calculated size of the patched buffer
            dwNewSizeWR := dwNewSize + dwOriginalSizeWR - dwOriginalSize;
        end;

        IDirect3DDevice8_CreateVertexBuffer(g_pD3DDevice8, dwNewSizeWR, 0, 0, D3DPOOL_MANAGED, @(pStream.pPatchedStream));

        if Assigned(pStream.pOriginalStream) then
        begin
            pStream.pOriginalStream.Lock(0, 0, pOrigVertexData, 0);
         end;

        if Assigned(pStream.pPatchedStream) then
        begin
            pStream.pPatchedStream.Lock(0, 0, pPatchedVertexData, 0);
        end;
    end
    else
    begin
        pStream.uiOrigStride := pPatchDesc.uiVertexStreamZeroStride;

        if (pPatchDesc.PrimitiveType = X_D3DPT_QUADLIST) then
        begin
            pPatchDesc.dwPrimitiveCount:= pPatchDesc.dwPrimitiveCount * 2;

            // This is a list of sqares/rectangles, so we convert it to a list of triangles
            dwOriginalSize  := pPatchDesc.dwPrimitiveCount * pStream.uiOrigStride * 2;
            dwNewSize       := pPatchDesc.dwPrimitiveCount * pStream.uiOrigStride * 3;
        end
        else if (pPatchDesc.PrimitiveType = X_D3DPT_LINELOOP) then  // LineLoop
        begin
            pPatchDesc.dwPrimitiveCount:= pPatchDesc.dwPrimitiveCount + 1;

            // We will add exactly one more line
            dwOriginalSize  := pPatchDesc.dwPrimitiveCount * pStream.uiOrigStride;
            dwNewSize       := pPatchDesc.dwPrimitiveCount * pStream.uiOrigStride + pStream.uiOrigStride;
        end;

        dwOriginalSizeWR := dwOriginalSize;
        dwNewSizeWR := dwNewSize;

        m_pNewVertexStreamZeroData := CxbxMalloc(dwNewSizeWR);
        m_bAllocatedStreamZeroData := True;

        pPatchedVertexData := m_pNewVertexStreamZeroData;
        pOrigVertexData := pPatchDesc.pVertexStreamZeroData;

        pPatchDesc.pVertexStreamZeroData := pPatchedVertexData;
     end;

    // Copy the nonmodified data
    memcpy(pPatchedVertexData, pOrigVertexData, pPatchDesc.dwOffset);
    memcpy(@pPatchedVertexData[pPatchDesc.dwOffset+dwNewSize],
           @pOrigVertexData[pPatchDesc.dwOffset+dwOriginalSize],
           dwOriginalSizeWR - pPatchDesc.dwOffset - dwOriginalSize);

    // Quad
    if (pPatchDesc.PrimitiveType = X_D3DPT_QUADLIST) then
    begin
        pPatch1 := @pPatchedVertexData[pPatchDesc.dwOffset     * pStream.uiOrigStride];
        pPatch2 := @pPatchedVertexData[pPatchDesc.dwOffset + 3 * pStream.uiOrigStride];
        pPatch3 := @pPatchedVertexData[pPatchDesc.dwOffset + 4 * pStream.uiOrigStride];
        pPatch4 := @pPatchedVertexData[pPatchDesc.dwOffset + 5 * pStream.uiOrigStride];

        pOrig1 := @pOrigVertexData[pPatchDesc.dwOffset     * pStream.uiOrigStride];
        pOrig2 := @pOrigVertexData[pPatchDesc.dwOffset + 2 * pStream.uiOrigStride];
        pOrig3 := @pOrigVertexData[pPatchDesc.dwOffset + 3 * pStream.uiOrigStride];

        for i := 0 to (pPatchDesc.dwPrimitiveCount div 2) - 1 do
        begin
            memcpy(pPatch1, pOrig1, pStream.uiOrigStride * 3); // Vertex 0,1,2 := Vertex 0,1,2
            memcpy(pPatch2, pOrig2, pStream.uiOrigStride);     // Vertex 3     := Vertex 2
            memcpy(pPatch3, pOrig3, pStream.uiOrigStride);     // Vertex 4     := Vertex 3
            memcpy(pPatch4, pOrig1, pStream.uiOrigStride);     // Vertex 5     := Vertex 0

            (*pPatch1:= pPatch1 + pStream.uiOrigStride * 6;
            pPatch2:= pPatch2 + pStream.uiOrigStride * 6;
            pPatch3:= pPatch3 + pStream.uiOrigStride * 6;
            pPatch4:= pPatch4 + pStream.uiOrigStride * 6;

            pOrig1:= pOrig1 + pStream.uiOrigStride * 4;
            pOrig2:= pOrig2 + pStream.uiOrigStride * 4;
            pOrig3:= pOrig3 + pStream.uiOrigStride * 4; *)

            if (pPatchDesc.hVertexShader and D3DFVF_XYZRHW) > 0 then
            begin
                for z := 0 to 6 - 1 do
                begin
                    (*if ((pPatchedVertexData[pPatchDesc.dwOffset + i * pStream.uiOrigStride * 6 + z * pStream.uiOrigStride])[2] = 0.0) then
                        (@pPatchedVertexData[pPatchDesc.dwOffset + i * pStream.uiOrigStride * 6 + z * pStream.uiOrigStride])[2] := 1.0;
                    if ((@pPatchedVertexData[pPatchDesc.dwOffset + i * pStream.uiOrigStride * 6 + z * pStream.uiOrigStride])[3] = 0.0) then
                        (@pPatchedVertexData[pPatchDesc.dwOffset + i * pStream.uiOrigStride * 6 + z * pStream.uiOrigStride])[3] := 1.0; *)
                 end;
             end;
         end;
    end
    // LineLoop
    else if (pPatchDesc.PrimitiveType = X_D3DPT_LINELOOP) then
    begin
        memcpy(@pPatchedVertexData[pPatchDesc.dwOffset], @pOrigVertexData[pPatchDesc.dwOffset], dwOriginalSize);
        memcpy(@pPatchedVertexData[pPatchDesc.dwOffset + dwOriginalSize], @pOrigVertexData[pPatchDesc.dwOffset], pStream.uiOrigStride);
    end;

    if not Assigned(pPatchDesc.pVertexStreamZeroData) then
    begin
        pStream.pOriginalStream.Unlock();
        pStream.pPatchedStream.Unlock();

        g_pD3DDevice8.SetStreamSource(0, pStream.pPatchedStream, pStream.uiOrigStride);
     end;

    m_bPatched := True;

    Result := True;
end;


function XTL_VertexPatcher.Apply(pPatchDesc: PVertexPatchDesc): bool;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  Patched: bool;
  uiStream: UINT;
  LocalPatched: bool;
begin
  Patched := false;
  // Get the number of streams
  m_uiNbrStreams := GetNbrStreams(pPatchDesc);
  if (VshHandleIsVertexShader(pPatchDesc.hVertexShader)) then
  begin
    m_pDynamicPatch := @(PVERTEX_SHADER(VshHandleGetVertexShader(pPatchDesc.hVertexShader).Handle).VertexDynamicPatch);
  end;
  for uiStream := 0 to m_uiNbrStreams -1 do
  begin
    LocalPatched := false;

    if (ApplyCachedStream(pPatchDesc, uiStream)) then
    begin
      m_pStreams[uiStream].bUsedCached := true;
      Continue;
    end;

    LocalPatched := LocalPatched or PatchPrimitive(pPatchDesc, uiStream);
    LocalPatched := LocalPatched or PatchStream(pPatchDesc, uiStream);
    if LocalPatched and (not Assigned(pPatchDesc.pVertexStreamZeroData)) then
    begin
      // Insert the patched stream in the cache
      CacheStream(pPatchDesc, uiStream);
      m_pStreams[uiStream].bUsedCached := true;
    end;
    Patched := Patched or LocalPatched;
  end;

  Result := Patched;
end;

function XTL_VertexPatcher.Restore: bool;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  uiStream: UINT;
begin
  if (not m_bPatched) then
  begin
    Result := False;
    Exit;
  end;

  for uiStream := 0 to m_uiNbrStreams - 1 do
  begin
    if (m_pStreams[uiStream].pOriginalStream <> NULL) and (m_pStreams[uiStream].pPatchedStream <> NULL) then
    begin
      g_pD3DDevice8.SetStreamSource(0, m_pStreams[uiStream].pOriginalStream, m_pStreams[uiStream].uiOrigStride);
    end;

    if (m_pStreams[uiStream].pOriginalStream <> NULL) then
      m_pStreams[uiStream].pOriginalStream._Release();

    if (m_pStreams[uiStream].pPatchedStream <> NULL) then
      m_pStreams[uiStream].pPatchedStream._Release();

    if (not m_pStreams[uiStream].bUsedCached) then
    begin
      if (Self.m_bAllocatedStreamZeroData) then
      begin
        CxbxFree(m_pNewVertexStreamZeroData);
      end;
    end
    else
    begin
      m_pStreams[uiStream].bUsedCached := false;
    end;
  end;

  Result := true;
end;

procedure XTL_EmuFlushIVB(); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
(*var
  pdwVB: PDWORD;
  uiStride: UINT;
  pDummyTexture : array [0..4 - 1] of IDirect3DTexture8; *)
begin
(*    if (g_IVBPrimitiveType = X_D3DPT_TRIANGLEFAN) then
    begin
        XTL_EmuUpdateDeferredStates();
        pdwVB := PDWORD(g_IVBTable);
        uiStride := 0;

{$IFDEF DEBUG}
        DbgPrintf('g_IVBTblOffs := %d', g_IVBTblOffs);
{$ENDIF}

        // TEMP DEBUGGING
        (*//*  MARKED OUT CXBX
        g_IVBTable[0].TexCoord1.x := 0.0;
        g_IVBTable[0].TexCoord1.y := 0.0;
        g_IVBTable[1].TexCoord1.x := 1.0;
        g_IVBTable[1].TexCoord1.y := 0.0;
        g_IVBTable[2].TexCoord1.x := 1.0;
        g_IVBTable[2].TexCoord1.y := 1.0;
        g_IVBTable[3].TexCoord1.x := 0.0;
        g_IVBTable[3].TexCoord1.y := 1.0;
        g_IVBTable[0].TexCoord2.x := 0.0;
        g_IVBTable[0].TexCoord2.y := 0.0;
        g_IVBTable[1].TexCoord2.x := 1.0;
        g_IVBTable[1].TexCoord2.y := 0.0;
        g_IVBTable[2].TexCoord2.x := 1.0;
        g_IVBTable[2].TexCoord2.y := 1.0;
        g_IVBTable[3].TexCoord2.x := 0.0;
        g_IVBTable[3].TexCoord2.y := 1.0;
        g_IVBTable[0].TexCoord3.x := 0.0;
        g_IVBTable[0].TexCoord3.y := 0.0;
        g_IVBTable[1].TexCoord3.x := 1.0;
        g_IVBTable[1].TexCoord3.y := 0.0;
        g_IVBTable[2].TexCoord3.x := 1.0;
        g_IVBTable[2].TexCoord3.y := 1.0;
        g_IVBTable[3].TexCoord3.x := 0.0;
        g_IVBTable[3].TexCoord3.y := 1.0;
        g_IVBTable[0].TexCoord4.x := 0.0;
        g_IVBTable[0].TexCoord4.y := 0.0;
        g_IVBTable[1].TexCoord4.x := 1.0;
        g_IVBTable[1].TexCoord4.y := 0.0;
        g_IVBTable[2].TexCoord4.x := 1.0;
        g_IVBTable[2].TexCoord4.y := 1.0;
        g_IVBTable[3].TexCoord4.x := 0.0;
        g_IVBTable[3].TexCoord4.y := 1.0;
        //*/  *)

(*        pDummyTexture[0] := nil;
        pDummyTexture[1] := nil;
        pDummyTexture[2] := nil;
        pDummyTexture[3] := nil;

        for(Integer Stage:=0;Stage<4;Stage++)
        begin
            if (pDummyTexture[Stage] = 0) then
            begin
                if (Stage = 0) then
                begin
                    if (D3DXCreateTextureFromFile(g_pD3DDevice8, 'C:\dummy1.bmp', @pDummyTexture[Stage]) <> D3D_OK) then
                        CxbxKrnlCleanup('Could not create dummy texture!');
                end
                else if (Stage = 1) then
                begin
                    if (D3DXCreateTextureFromFile(g_pD3DDevice8, 'C:\dummy2.bmp', @pDummyTexture[Stage]) <> D3D_OK) then
                        CxbxKrnlCleanup('Could not create dummy texture!');
                 end;
             end;

            g_pD3DDevice8.SetTexture(Stage, pDummyTexture[Stage]);
         end;

        (*//*/  MARKED OUT BY CXBX
        g_pD3DDevice8.SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_BLENDDIFFUSEALPHA);
        g_pD3DDevice8.SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
        g_pD3DDevice8.SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_SPECULAR);
        g_pD3DDevice8.SetTextureStageState(0, D3DTSS_ALPHAOP,   D3DTOP_SELECTARG1);
        g_pD3DDevice8.SetTextureStageState(0, D3DTSS_ALPHAARG1,   D3DTA_TEXTURE);


        g_pD3DDevice8.SetTextureStageState(0, D3DTSS_ALPHAOP,   D3DTOP_SELECTARG1);
        g_pD3DDevice8.SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
        g_pD3DDevice8.SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_SELECTARG1);
        g_pD3DDevice8.SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
        g_pD3DDevice8.SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);

        g_pD3DDevice8.SetTextureStageState(1, D3DTSS_ALPHAOP,   D3DTOP_DISABLE);
        g_pD3DDevice8.SetTextureStageState(1, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
        g_pD3DDevice8.SetTextureStageState(1, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
        g_pD3DDevice8.SetTextureStageState(1, D3DTSS_COLOROP,   D3DTOP_DISABLE);
        g_pD3DDevice8.SetTextureStageState(1, D3DTSS_COLORARG1, D3DTA_TEXTURE);
        g_pD3DDevice8.SetTextureStageState(1, D3DTSS_COLORARG2, D3DTA_CURRENT);

        g_pD3DDevice8.SetTextureStageState(2, D3DTSS_ALPHAOP,   D3DTOP_DISABLE);
        g_pD3DDevice8.SetTextureStageState(2, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
        g_pD3DDevice8.SetTextureStageState(2, D3DTSS_COLOROP,   D3DTOP_DISABLE);
        g_pD3DDevice8.SetTextureStageState(2, D3DTSS_COLORARG1, D3DTA_TEXTURE);
        g_pD3DDevice8.SetTextureStageState(2, D3DTSS_COLORARG2, D3DTA_CURRENT);

        g_pD3DDevice8.SetTextureStageState(3, D3DTSS_ALPHAOP,   D3DTOP_DISABLE);
        g_pD3DDevice8.SetTextureStageState(3, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
        g_pD3DDevice8.SetTextureStageState(3, D3DTSS_COLOROP,   D3DTOP_DISABLE);
        g_pD3DDevice8.SetTextureStageState(3, D3DTSS_COLORARG1, D3DTA_TEXTURE);
        g_pD3DDevice8.SetTextureStageState(3, D3DTSS_COLORARG2, D3DTA_CURRENT);

        g_pD3DDevice8.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_ONE);
        g_pD3DDevice8.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_SRCCOLOR);
        g_pD3DDevice8.SetRenderState(D3DRS_AMBIENT, RGB(255,255,255));
        g_pD3DDevice8.SetRenderState(D3DRS_LIGHTING, FALSE);
        g_pD3DDevice8.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
        g_pD3DDevice8.SetRenderState(D3DRS_ZENABLE, D3DZB_TRUE);
        //*/  *)

(*        for(uint v:=0;v<g_IVBTblOffs;v++)
        begin
            DWORD dwPos := g_IVBFVF and D3DFVF_POSITION_MASK;

            if (dwPos = D3DFVF_XYZRHW) then
            begin
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.y;
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.z;
                *(FLOAT)pdwVB++ := g_IVBTable[v].Rhw;

                if (v = 0) then
                begin
                    uiStride:= uiStride + (SizeOf(FLOAT)*4);
                 end;

{$IFDEF DEBUG}
                DbgPrintf('IVB Position := (%f, %f, %f end;', g_IVBTable[v].Position.x, g_IVBTable[v].Position.y, g_IVBTable[v].Position.z);
{$ENDIF}
            end
            else
            begin
                CxbxKrnlCleanup('Unsupported Position Mask (FVF := $%.08X)', g_IVBFVF);
            end;

            if (g_IVBFVF and D3DFVF_DIFFUSE) then
            begin
                *(DWORD)pdwVB++ := g_IVBTable[v].dwDiffuse;

                if (v = 0) then
                begin
                    uiStride:= uiStride + SizeOf(DWORD);
                 end;

{$IFDEF DEBUG}
                DbgPrintf('IVB Diffuse := $%.08X', g_IVBTable[v].dwDiffuse);
{$ENDIF}
             end;

            if (g_IVBFVF and D3DFVF_SPECULAR) then
            begin
                *(DWORD)pdwVB++ := g_IVBTable[v].dwDiffuse;

                if (v = 0) then
                begin
                    uiStride:= uiStride + SizeOf(DWORD);
                 end;

{$IFDEF DEBUG}
                DbgPrintf('IVB Specular := $%.08X', g_IVBTable[v].dwSpecular);
{$ENDIF}
             end;

            DWORD dwTexN := (g_IVBFVF and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT;

            if (dwTexN >= 1) then
            begin
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord1.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord1.y;

                if (v = 0) then
                begin
                    uiStride:= uiStride + SizeOf(FLOAT)*2;
                 end;

{$IFDEF DEBUG}
                DbgPrintf('IVB TexCoord1 := (%f, %f end;', g_IVBTable[v].TexCoord1.x, g_IVBTable[v].TexCoord1.y);
{$ENDIF}
             end;

            if (dwTexN >= 2) then
            begin
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord2.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord2.y;

                if (v = 0) then
                begin
                    uiStride:= uiStride + SizeOf(FLOAT)*2;
                 end;

{$IFDEF DEBUG}
                DbgPrintf('IVB TexCoord2 := (%f, %f end;', g_IVBTable[v].TexCoord2.x, g_IVBTable[v].TexCoord2.y);
{$ENDIF}
             end;

            if (dwTexN >= 3) then
            begin
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord3.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord3.y;

                if (v = 0) then
                begin
                    uiStride:= uiStride + SizeOf(FLOAT)*2;
                 end;

{$IFDEF DEBUG}
                DbgPrintf('IVB TexCoord3 := (%f, %f end;', g_IVBTable[v].TexCoord3.x, g_IVBTable[v].TexCoord3.y);
{$ENDIF}
             end;

            if (dwTexN >= 4) then
            begin
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord4.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord4.y;

                if (v = 0) then
                begin
                    uiStride:= uiStride + SizeOf(FLOAT)*2;
                 end;

{$IFDEF DEBUG}
                DbgPrintf('IVB TexCoord4 := (%f, %f end;', g_IVBTable[v].TexCoord4.x, g_IVBTable[v].TexCoord4.y);
{$ENDIF}
             end;
         end;

        g_pD3DDevice8.SetVertexShader(g_IVBFVF);
        g_pD3DDevice8.SetPixelShader(0);

        // patch buffer
        UINT PrimitiveCount := EmuD3DVertex2PrimitiveCount(g_IVBPrimitiveType, g_IVBTblOffs);

        VertexPatchDesc VPDesc;

        VPDesc.dwVertexCount := g_IVBTblOffs;
        VPDesc.PrimitiveType := g_IVBPrimitiveType;
        VPDesc.dwPrimitiveCount := PrimitiveCount;
        VPDesc.dwOffset := 0;
        VPDesc.pVertexStreamZeroData := g_IVBTable;
        VPDesc.uiVertexStreamZeroStride := uiStride;
        // Cxbx TODO: Set the current shader and let the patcher handle it..
        VPDesc.hVertexShader := g_IVBFVF;

        XTL_VertexPatcher VertPatch;

        bool bPatched := VertPatch.Apply(@VPDesc);

        (*
        IDirect3DBaseTexture8 *pTexture := 0;

        g_pD3DDevice8.GetTexture(0, @pTexture);

        if (pTexture <> 0) then
        begin
             Integer dwDumpTexture := 0;

             szBuffer: array [0..255-1] of Char;

            StrFmt(szBuffer, 'C:\Aaron\Textures\Texture-Active%.03d ($%.08X).bmp', dwDumpTexture++, pTexture);

            D3DXSaveTextureToFile(szBuffer, D3DXIFF_BMP, pTexture, 0);
         end;
        //*/
        EmuUpdateActiveTexture();

        g_pD3DDevice8.DrawPrimitiveUP(D3DPT_TRIANGLEFAN, VPDesc.dwPrimitiveCount, VPDesc.pVertexStreamZeroData, VPDesc.uiVertexStreamZeroStride);

        VertPatch.Restore();

        g_IVBTblOffs := 0;
    end
    else if ((g_IVBPrimitiveType = X_D3DPT_QUADLIST) and (g_IVBTblOffs = 4)) then
    begin
        XTL_EmuUpdateDeferredStates();

        DWORD *pdwVB := (DWORD)g_IVBTable;

        UINT uiStride := 0;

        for(Integer v:=0;v<4;v++)
        begin
            DWORD dwPos := g_IVBFVF and D3DFVF_POSITION_MASK;

            if (dwPos = D3DFVF_XYZ) then
            begin
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.y;
                *(FLOAT)pdwVB++ := g_IVBTable[v].Position.z;

                if (v = 0) then
                begin
                    uiStride:= uiStride + (SizeOf(FLOAT)*3);
                 end;

{$IFDEF DEBUG}
                DbgPrintf('IVB Position := (%f, %f, %f end;', g_IVBTable[v].Position.x, g_IVBTable[v].Position.y, g_IVBTable[v].Position.z);
{$ENDIF}
            end
            else
            begin
                CxbxKrnlCleanup('Unsupported Position Mask (FVF := $%.08X)', g_IVBFVF);
            end;

            if (g_IVBFVF and D3DFVF_DIFFUSE) then
            begin
                *(DWORD)pdwVB++ := g_IVBTable[v].dwDiffuse;

                if (v = 0) then
                begin
                    uiStride:= uiStride + SizeOf(DWORD);
                 end;

{$IFDEF DEBUG}
                DbgPrintf('IVB Diffuse := $%.08X', g_IVBTable[v].dwDiffuse);
{$ENDIF}
             end;

            DWORD dwTexN := (g_IVBFVF and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT;

            if (dwTexN >= 1) then
            begin
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord1.x;
                *(FLOAT)pdwVB++ := g_IVBTable[v].TexCoord1.y;

                if (v = 0) then
                begin
                    uiStride:= uiStride + SizeOf(FLOAT)*2;
                 end;

{$IFDEF DEBUG}
                DbgPrintf('IVB TexCoord1 := (%f, %f end;', g_IVBTable[v].TexCoord1.x, g_IVBTable[v].TexCoord1.y);
{$ENDIF}
             end;
         end;

        g_pD3DDevice8.SetVertexShader(g_IVBFVF);
        g_pD3DDevice8.SetPixelShader(0);

        // patch buffer
        UINT PrimitiveCount := EmuD3DVertex2PrimitiveCount(g_IVBPrimitiveType, 4);

        VertexPatchDesc VPDesc;

        VPDesc.dwVertexCount := 4;
        VPDesc.PrimitiveType := g_IVBPrimitiveType;
        VPDesc.dwPrimitiveCount := PrimitiveCount;
        VPDesc.dwOffset := 0;
        VPDesc.pVertexStreamZeroData := g_IVBTable;
        VPDesc.uiVertexStreamZeroStride := uiStride;
        // Cxbx TODO: Set the current shader and let the patcher handle it..
        VPDesc.hVertexShader := g_IVBFVF;

        XTL_VertexPatcher VertPatch;

        bool bPatched := VertPatch.Apply(@VPDesc);

        g_pD3DDevice8.DrawPrimitiveUP(D3DPT_TRIANGLELIST, VPDesc.dwPrimitiveCount, VPDesc.pVertexStreamZeroData, VPDesc.uiVertexStreamZeroStride);

        VertPatch.Restore();

        // ignore
        g_IVBTblOffs := 0;
     end;

    *)
end;

procedure XTL_EmuUpdateActiveTexture(); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  Stage: integer;
  pTexture: PX_D3DResource;
  pResource: PX_D3DResource;
  pPixelContainer: PX_D3DPixelContainer;
  X_Format: X_D3DFORMAT;
  dwWidth: DWORD;
  dwHeight: DWORD;
  dwBPP: DWORD;
  dwDepth: DWORD;
  dwPitch: DWORD;
  dwMipMapLevels: DWORD;
  bSwizzled: BOOL;
  bCompressed: BOOL;
  dwCompressedSize: DWORD;
  bCubemap: BOOL;

  dwCompressedOffset: DWORD;
  dwMipOffs: DWORD;
  dwMipWidth: DWORD;
  dwMipHeight: DWORD;
  dwMipPitch: DWORD;
  level: uint;

  LockedRect: D3DLOCKED_RECT;

  hRet: HRESULT;

  iRect: TRect;
  iPoint: TPoint;
  pSrc: PBYTE;
  pDest: PBYTE;
  v: DWORD;
begin
    //
    // DEBUGGING
    //
  dwWidth := 0;
  dwHeight := 0;
  dwBPP := 0;

  for Stage := 0 to 3 do
  begin
    pTexture := EmuD3DActiveTexture[Stage];

    if (pTexture = nil) then
        continue;

    pResource := pTexture;
    pPixelContainer := PX_D3DPixelContainer(pTexture);

    X_Format := X_D3DFORMAT(((pPixelContainer.Format and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT));

    if (X_Format <> $CD) and (pTexture.EmuResource8.GetType() = D3DRTYPE_TEXTURE) then
    begin
      dwDepth := 1; dwPitch := 0; dwMipMapLevels := 1;
      bSwizzled := FALSE; bCompressed := FALSE; dwCompressedSize := 0;
      bCubemap := (pPixelContainer.Format and X_D3DFORMAT_CUBEMAP) > 0;

      // Interpret Width/Height/BPP
      if (X_Format = X_D3DFMT_X8R8G8B8) or (X_Format = X_D3DFMT_A8R8G8B8) then
      begin
        bSwizzled := TRUE;

        // Swizzled 32 Bit
        dwWidth  := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
        dwHeight := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
        dwMipMapLevels := (pPixelContainer.Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
        dwDepth  := 1;// HACK? 1 << ((pPixelContainer.Format and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
        dwPitch  := dwWidth*4;
        dwBPP := 4;
      end
      else if (X_Format = X_D3DFMT_R5G6B5) or (X_Format = X_D3DFMT_A4R4G4B4)
           or (X_Format = X_D3DFMT_LIN_A4R4G4B4) or (X_Format = X_D3DFMT_A1R5G5B5)
           or (X_Format = $28) (* X_D3DFMT_G8B8 *) then
      begin
        bSwizzled := True;

                  // Swizzled 16 Bit
        dwWidth := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
        dwHeight := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
        dwMipMapLevels := (pPixelContainer.Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
        dwDepth := 1; // HACK? 1 << ((pPixelContainer.Format and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
        dwPitch := dwWidth * 2;
        dwBPP := 2;
      end
      else if (X_Format = X_D3DFMT_L8) or (X_Format = X_D3DFMT_P8) or (X_Format = X_D3DFMT_AL8) or (X_Format = X_D3DFMT_A8L8) then
      begin
        bSwizzled := TRUE;

        // Swizzled 8 Bit
        dwWidth  := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
        dwHeight := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
        dwMipMapLevels := (pPixelContainer.Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
        dwDepth  := 1;// HACK? 1 << ((pPixelContainer.Format and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
        dwPitch  := dwWidth;
        dwBPP := 1;
      end
      else if (X_Format = X_D3DFMT_LIN_X8R8G8B8) or (X_Format = X_D3DFMT_LIN_A8R8G8B8) or (X_Format = X_D3DFMT_LIN_D24S8) then
      begin
        // Linear 32 Bit
        dwWidth  := (pPixelContainer.Size and X_D3DSIZE_WIDTH_MASK) + 1;
        dwHeight := ((pPixelContainer.Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
        dwPitch  := (((pPixelContainer.Size and X_D3DSIZE_PITCH_MASK) shr X_D3DSIZE_PITCH_SHIFT)+1)*64;
        dwBPP := 4;
      end
      else if (X_Format = X_D3DFMT_LIN_R5G6B5) then
      begin
        // Linear 16 Bit
        dwWidth := (pPixelContainer.Size and X_D3DSIZE_WIDTH_MASK) + 1;
        dwHeight := ((pPixelContainer.Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
        dwPitch := (((pPixelContainer.Size and X_D3DSIZE_PITCH_MASK) shr X_D3DSIZE_PITCH_SHIFT) + 1) * 64;
        dwBPP := 2;
      end
      else if (X_Format = X_D3DFMT_DXT1) or (X_Format = X_D3DFMT_DXT3) or (X_Format = X_D3DFMT_DXT5) then
      begin
        bCompressed := TRUE;

        // Compressed
        dwWidth  := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
        dwHeight := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
        dwDepth  := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
        dwMipMapLevels := (pPixelContainer.Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;

        // D3DFMT_DXT2...D3DFMT_DXT5 : 128bits per block/per 16 texels
        dwCompressedSize := dwWidth*dwHeight;

        if (X_Format = X_D3DFMT_DXT1) then     // 64bits per block/per 16 texels
          dwCompressedSize := dwCompressedSize div 2;

        dwBPP := 1;
      end
      else if (X_Format = X_D3DFMT_YUY2) then
      begin
        // Linear 32 Bit
        dwWidth := (pPixelContainer.Size and X_D3DSIZE_WIDTH_MASK) + 1;
        dwHeight := ((pPixelContainer.Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
        dwPitch := (((pPixelContainer.Size and X_D3DSIZE_PITCH_MASK) shr X_D3DSIZE_PITCH_SHIFT) + 1) * 64;
      end
      else
      begin
        CxbxKrnlCleanup('$%.08X is not a supported format!', [X_Format]);
      end;

      // as we iterate through mipmap levels, we'll adjust the source resource offset
      dwCompressedOffset := 0;

      dwMipOffs := 0;
      dwMipWidth := dwWidth;
      dwMipHeight := dwHeight;
      dwMipPitch := dwPitch;

      if (dwMipMapLevels > 6) then
        dwMipMapLevels := 6;

      // iterate through the number of mipmap levels
      for level := 0 to dwMipMapLevels - 1 do
      begin
        {hRet := }pResource.EmuTexture8.LockRect(level, LockedRect, NULL, 0);

        iRect := classes.Rect(0, 0, 0, 0);
        iPoint := classes.Point(0, 0);

        pSrc := PBYTE(pTexture.Data);

        if IsSpecialResource(pResource.Data) and ((pResource.Data and X_D3DRESOURCE_DATA_FLAG_SURFACE) > 0) then
        begin

        end
        else
        begin
          if (bSwizzled) then
          begin
            if (DWORD(pSrc) = $80000000) then
            begin
              // Cxbx TODO: Fix or handle this situation..?
            end
            else
            begin
              XTL_EmuXGUnswizzleRect
                (
                pSrc + dwMipOffs, dwMipWidth, dwMipHeight, dwDepth, LockedRect.pBits,
                LockedRect.Pitch, iRect, iPoint, dwBPP
                );
            end;
          end
          else if (bCompressed) then
          begin
            // NOTE: compressed size is (dwWidth/2)*(dwHeight/2)/2, so each level divides by 4

            memcpy(LockedRect.pBits, pSrc + dwCompressedOffset, dwCompressedSize shr (level * 2));

            Inc(dwCompressedOffset, (dwCompressedSize shr (level * 2)));
          end
          else
          begin
            pDest := PBYTE(LockedRect.pBits);

            if (DWORD(LockedRect.Pitch) = dwMipPitch) and (dwMipPitch = dwMipWidth * dwBPP) then
            begin
              memcpy(pDest, pSrc + dwMipOffs, dwMipWidth * dwMipHeight * dwBPP);
            end
            else
            begin
              for v := 0 to dwMipHeight - 1 do
              begin
                memcpy(pDest, pSrc + dwMipOffs, dwMipWidth * dwBPP);

                Inc(pDest, LockedRect.Pitch);
                Inc(pSrc, dwMipPitch);
              end;
            end;
          end;
        end;

        pResource.EmuTexture8.UnlockRect(level);

        Inc(dwMipOffs, dwMipWidth * dwMipHeight * dwBPP);

        dwMipWidth := dwMipWidth div 2;
        dwMipHeight := dwMipHeight div 2;
        dwMipPitch := dwMipPitch div 2;
      end;
    end;

    g_pD3DDevice8.SetTexture(Stage, pTexture.EmuTexture8);

  end;
end;

//exports
//  XTL_EmuFlushIVB,
//  XTL_EmuUpdateActiveTexture;

end.

