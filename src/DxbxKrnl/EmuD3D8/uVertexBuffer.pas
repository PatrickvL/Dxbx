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
  , Math // IfThen
  // Jedi Win32API
  , JwaWinType
  // DirectX
  , Direct3D
{$IFDEF DXBX_USE_D3D9}
  , Direct3D9
  , D3DX9
{$ELSE}
  , Direct3D8
  , D3DX8
{$ENDIF}
  // Dxbx
  , uConsts
  , uTypes // CLOCKS_PER_SEC, clock()
  , uLog
  , uDxbxUtils // Log2:int
  , uDxbxKrnlUtils
  , uState
  , uResourceTracker
  , uEmuAlloc
  , uEmu
  , uEmuXG
  , uEmuD3D8Types
  , uEmuD3D8Utils
  , uConvert
  , uVertexShader;

type _VertexPatchDesc = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    PrimitiveType: X_D3DPRIMITIVETYPE; // input, can be updated by PatchPrimitive
    dwVertexCount: DWORD; // input
    dwPrimitiveCount: DWORD; // output
    // Data if Draw...UP call
    pVertexStreamZeroData: PVOID;
    uiVertexStreamZeroStride: UINT;
    // The current vertex shader, used to identify the streams
    hVertexShader: DWORD;
{$IFDEF DXBX_USE_D3D9}
    uiOffsetInBytes: UINT;
{$ENDIF}
    procedure VertexPatchDesc();
  end; // size = 28 (as in Cxbx)
  VertexPatchDesc = _VertexPatchDesc;
  PVertexPatchDesc = ^VertexPatchDesc;

type _PATCHEDSTREAM = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    pOriginalStream: XTL_PIDirect3DVertexBuffer8;
    pPatchedStream: XTL_PIDirect3DVertexBuffer8;
    uiOrigStride: UINT;
    uiNewStride: UINT;
    bUsedCached: _bool;
{$IFDEF DXBX_USE_D3D9}
    uiOffsetInBytes: UINT;
{$ENDIF}
  end; // size = 20 (as in Cxbx)
  PATCHEDSTREAM = _PATCHEDSTREAM;
  PPATCHEDSTREAM = ^PATCHEDSTREAM;

type _CACHEDSTREAM = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    uiCRC32: uint32;
    uiCheckFrequency: uint32;
    uiCacheHit: uint32;
    bIsUP: _bool;
    Stream: PATCHEDSTREAM;
    pStreamUP: Pvoid;            // Draw..UP (instead of pOriginalStream)
    uiLength: uint32;            // The length of the stream
    uiCount: uint32;             // CRC32 check count
    dwPrimitiveCount: uint32;
    lLastUsed: long;             // For cache removal purposes
  end; // size = 56 (as in Cxbx)
  CACHEDSTREAM = _CACHEDSTREAM;
  PCACHEDSTREAM = ^CACHEDSTREAM;

type VertexPatcher = object
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  public
    procedure VertexPatcher();

    function Apply(pPatchDesc: PVertexPatchDesc; pbFatalError: P_bool): _bool;
    function Restore(): _bool;
    // Dumps the cache to the console
    procedure DumpCache();
  private
    m_uiNbrStreams: UINT;
    m_pStreams: array [0..MAX_NBR_STREAMS-1] of PATCHEDSTREAM;

    m_pNewVertexStreamZeroData: PVOID;

    m_bPatched: _bool;
    m_bAllocatedStreamZeroData: _bool;

    m_pDynamicPatch: PVERTEX_DYNAMIC_PATCH;
    // Returns the number of streams of a patch
    function GetNbrStreams(pPatchDesc: PVertexPatchDesc): UINT;
    // Caches a patched stream
(* NOT USED
   procedure CacheStream(pPatchDesc: PVertexPatchDesc;
                          uiStream: UINT);
    // Frees a cached, patched stream
    procedure FreeCachedStream(pStream: Pvoid);
    // Tries to apply a previously patched stream from the cache
    function ApplyCachedStream(pPatchDesc: PVertexPatchDesc;
                               uiStream: UINT;
                               pbFatalError: P_bool): _bool;
*)
    // Patches the types of the stream
    function PatchStream(pPatchDesc: PVertexPatchDesc; uiStream: UINT): _bool;
    // Normalize texture coordinates in FVF stream if needed
    function NormalizeTexCoords(pPatchDesc: PVertexPatchDesc; uiStream: UINT): _bool;
    // Patches the primitive type
    procedure PatchPrimitive(pPatchDesc: PVertexPatchDesc);
  end; // size = 336 (as in Cxbx)

// inline vertex buffer emulation
var g_pIVBVertexBuffer: array of DWORD = nil;
var g_IVBPrimitiveType: X_D3DPRIMITIVETYPE = X_D3DPT_INVALID;
var g_IVBFVF: DWORD = 0;

type _D3DIVB = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    Position: TD3DXVECTOR3; // Position
    Rhw: FLOAT; // Rhw
    Blend1: FLOAT; // Blend1
    Blend2: FLOAT; // Dxbx addition : for D3DFVF_XYZB2 TODO : Where should we set these?
    Blend3: FLOAT; // Dxbx addition : for D3DFVF_XYZB3
    Blend4: FLOAT; // Dxbx addition : for D3DFVF_XYZB4
    dwSpecular: DWORD; // Specular
    dwDiffuse: DWORD; // Diffuse
    Normal: TD3DXVECTOR3; // Normal
    TexCoord1: TD3DXVECTOR2; // TexCoord1
    TexCoord2: TD3DXVECTOR2; // TexCoord2
    TexCoord3: TD3DXVECTOR2; // TexCoord3
    TexCoord4: TD3DXVECTOR2; // TexCoord4
  end; // size = 72 (as in Cxbx)
  D3DIVB = _D3DIVB;
  PD3DIVB = ^D3DIVB;

procedure XTL_EmuFlushIVB(); {NOPATCH}

procedure CRC32Init;
function CRC32(data: PByte; len: int): uint;

const
  VERTICES_PER_QUAD = 4;
  VERTICES_PER_TRIANGLE = 3;
  TRIANGLES_PER_QUAD = 2;

const VERTEX_BUFFER_CACHE_SIZE = 64;
const MAX_STREAM_NOT_USED_TIME = (2 * CLOCKS_PER_SEC); // TODO -oCXBX: Trim the not used time

// inline vertex buffer emulation
var g_IVBTblOffs: UINT = 0;
var g_IVBTable: array of D3DIVB = nil;

implementation

uses
  uEmuD3D8;

const lfUnit = lfCxbx or lfVertexBuffer;

var crctab: array [0..256-1] of uint;

{static}var bFirstTime: boolean = true;
procedure CRC32Init;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  i, j: int;
  crc: uint;
begin
  if not bFirstTime then
    Exit;

  for i := 0 to 256-1 do
  begin
    crc := i shl 24;
    for j := 0 to 8-1 do
    begin
      if (crc and $80000000) > 0 then
        crc := (crc shl 1) xor $04c11db7
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
  if len < 4 then abort;

  result :=           (data^ shl 24); Inc(data);
  result := result or (data^ shl 16); Inc(data);
  result := result or (data^ shl  8); Inc(data);
  result := result or  data^        ; Inc(data);
  result := not result;
  Dec(len, 4);

  for i := 0 to len - 1 do
  begin
    result := ((result shl 8) or data^) xor crctab[result shr 24];
    Inc(data);
  end;

  result := not result;
end;

{ _VertexPatchDesc }

procedure _VertexPatchDesc.VertexPatchDesc();
begin
  ZeroMemory(@Self, SizeOf(Self));
end;

{ VertexPatcher }

procedure VertexPatcher.VertexPatcher();
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  m_uiNbrStreams := 0;
  ZeroMemory(@(m_pStreams[0]), sizeof(PATCHEDSTREAM) * MAX_NBR_STREAMS);
  m_bPatched := false;
  m_bAllocatedStreamZeroData := false;
  m_pNewVertexStreamZeroData := NULL;
  m_pDynamicPatch := NULL;
  CRC32Init();
end;

procedure VertexPatcher.DumpCache();
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
      // TODO -oCXBX: Write nicer dump presentation
      DbgPrintf('Key: 0x%.08X Cache Hits: %d IsUP: %s OrigStride: %d NewStride: %d CRCCount: %d CRCFreq: %d Lengh: %d CRC32: 0x%.08X',
             [pNode.uiKey, pCachedStream_.uiCacheHit, ifThen(pCachedStream_.bIsUP, 'YES', 'NO'),
             pCachedStream_.Stream.uiOrigStride, pCachedStream_.Stream.uiNewStride,
             pCachedStream_.uiCount, pCachedStream_.uiCheckFrequency,
             pCachedStream_.uiLength, pCachedStream_.uiCRC32]);
    end;

    pNode := pNode.pNext;
  end;
end;

(* NOT USED

procedure VertexPatcher.CacheStream(pPatchDesc: PVertexPatchDesc;
                                    uiStream: UINT);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
//  uiStride: UINT; // DXBX, uiStride never used
  pOrigVertexBuffer: XTL_PIDirect3DVertexBuffer8;
  Desc: D3DVERTEXBUFFER_DESC;
  pCalculateData: Pvoid;
  uiKey: uint32;
  uiMinHit: uint32;
  uiLength: UINT;
  pCachedStream_: PCACHEDSTREAM;
  pNode: PRTNode;
  uiChecksum: UINT;
begin
  pCalculateData := NULL;
  pCachedStream_ := PCACHEDSTREAM(DxbxMalloc(sizeof(CACHEDSTREAM)));

  ZeroMemory(pCachedStream_, sizeof(CACHEDSTREAM));

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
          if MayLog(lfUnit or lfDebug) then
            DbgPrintf('!!!Found an old stream, %2.2f', [{FLOAT}((clock() + MAX_STREAM_NOT_USED_TIME) - DWord(PCACHEDSTREAM(pNode.pResource).lLastUsed)) / {FLOAT}(CLOCKS_PER_SEC)]);

          uiKey := pNode.uiKey;
          break;
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
      if MayLog(lfUnit or lfDebug) then
        DbgPrintf('!!!Removing stream');

      FreeCachedStream(Pvoid(uiKey));
    end;
  end;

  // Start the actual stream caching
  if (nil=pPatchDesc.pVertexStreamZeroData) then
  begin
    pOrigVertexBuffer := m_pStreams[uiStream].pOriginalStream;
    IDirect3DVertexBuffer(pOrigVertexBuffer)._AddRef();
    IDirect3DVertexBuffer(m_pStreams[uiStream].pPatchedStream)._AddRef();
    if (FAILED(IDirect3DVertexBuffer(pOrigVertexBuffer).GetDesc({out}Desc))) then
    begin
      DxbxKrnlCleanup('Could not retrieve original buffer size');
    end;
    if (FAILED(IDirect3DVertexBuffer(pOrigVertexBuffer).Lock(0, 0, {out}TLockData(pCalculateData), 0))) then
    begin
      DxbxKrnlCleanup('Couldn''t lock the original buffer');
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
      DxbxKrnlCleanup('Trying to patch a Draw..UP with more than stream zero!');
    end;
    // uiStride := pPatchDesc.uiVertexStreamZeroStride; // DXBX, uiStride never used
    pCalculateData := Puint08(pPatchDesc.pVertexStreamZeroData);
    // TODO -oCXBX: This is sometimes the number of indices, which isn't too good
    uiLength := pPatchDesc.dwVertexCount * pPatchDesc.uiVertexStreamZeroStride;
    pCachedStream_.bIsUP := true;
    pCachedStream_.pStreamUP := pCalculateData;
    uiKey := uint32(pCalculateData);
  end;

  uiChecksum := CRC32(PByte(pCalculateData), uiLength);
  if (nil=pPatchDesc.pVertexStreamZeroData) then
  begin
    IDirect3DVertexBuffer(pOrigVertexBuffer).Unlock();
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
end; // VertexPatcher.CacheStream

procedure VertexPatcher.FreeCachedStream(pStream: Pvoid);
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
      DxbxFree(pCachedStream_.pStreamUP);
      pCachedStream_.pStreamUP := nil; // Dxbx addition - nil out after freeing
    end;
    if Assigned(pCachedStream_.Stream.pOriginalStream) then
    begin
      if IDirect3DVertexBuffer(pCachedStream_.Stream.pOriginalStream)._Release() = 0 then
        pCachedStream_.Stream.pOriginalStream := nil; // Dxbx addition - nil out after decreasing reference count
    end;
    if Assigned(pCachedStream_.Stream.pPatchedStream) then
    begin
{.$MESSAGE 'FreeCachedStream hits an int 3 because of this call to pPatchedStream._Release() :'}
      if IDirect3DVertexBuffer(pCachedStream_.Stream.pPatchedStream)._Release() = 0 then
        pCachedStream_.Stream.pPatchedStream := nil; // Dxbx addition - nil out after decreasing reference count
    end;
    DxbxFree(pCachedStream_);
  end;
  g_PatchedStreamsCache.remove(pStream);
  g_PatchedStreamsCache.Unlock(); // Dxbx addition - Unlock _after_ update?
end;

function VertexPatcher.ApplyCachedStream(pPatchDesc: PVertexPatchDesc;
                                         uiStream: UINT;
                                         pbFatalError: P_bool): _bool;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
{$IFDEF DXBX_USE_D3D9}
  uiOffsetInBytes: UINT;
{$ENDIF}
  uiStride: UINT;
  pOrigVertexBuffer: XTL_PIDirect3DVertexBuffer8;
  Desc: D3DVERTEXBUFFER_DESC;
  pCalculateData: Pvoid;
//UNUSED  uiLength: UINT;
  bApplied: _bool;
  uiKey: uint32;
  pCachedStream_: PCACHEDSTREAM;
  bMismatch: _bool;
  Checksum: uint32;
begin
  pCalculateData := NULL;
  bApplied := false;
  //pCachedStream_ := PCACHEDSTREAM(DxbxMalloc(sizeof(CACHEDSTREAM)));

  if (nil=pPatchDesc.pVertexStreamZeroData) then
  begin
    g_pD3DDevice.GetStreamSource(
      uiStream,
      PIDirect3DVertexBuffer(@pOrigVertexBuffer),
{$IFDEF DXBX_USE_D3D9}
      {out}uiOffsetInBytes,
{$ENDIF}
      {out}uiStride);

    if (nil=pOrigVertexBuffer) then
    begin
      {if(nil=g_pVertexBuffer) or (nil=g_pVertexBuffer.Emu.VertexBuffer) then
        DxbxKrnlCleanup('Unable to retrieve original buffer (Stream := %d)', [uiStream]);
      else
        pOrigVertexBuffer := g_pVertexBuffer.Emu.VertexBuffer;}

      if Assigned(pbFatalError) then
        pbFatalError^ := true;

      Result := false;
      Exit;
    end;

    if (FAILED(IDirect3DVertexBuffer(pOrigVertexBuffer).GetDesc({out}Desc))) then
    begin
      DxbxKrnlCleanup('Could not retrieve original buffer size');
    end;
    //UNUSED uiLength := Desc.Size;
    uiKey := uint32(pOrigVertexBuffer);
    //pCachedStream_.bIsUP := false;
  end
  else
  begin
    // There should only be one stream (stream zero) in this case
    if (uiStream <> 0) then
    begin
      DxbxKrnlCleanup('Trying to find a cached Draw..UP with more than stream zero!');
    end;
{$IFDEF DXBX_USE_D3D9}
    uiOffsetInBytes := pPatchDesc.uiOffsetInBytes;
{$ENDIF}
    uiStride := pPatchDesc.uiVertexStreamZeroStride;
    pCalculateData := Puint08(pPatchDesc.pVertexStreamZeroData);
    // TODO -oCXBX: This is sometimes the number of indices, which isn't too good
    //UNUSED uiLength := pPatchDesc.dwVertexCount * pPatchDesc.uiVertexStreamZeroStride;
    uiKey := uint32(pCalculateData);
    //pCachedStream_.bIsUP := true;
    //pCachedStream_.pStreamUP := pCalculateData;
  end;

  g_PatchedStreamsCache.Lock();
  pCachedStream_ := PCACHEDSTREAM(g_PatchedStreamsCache.get(uiKey));
  if Assigned(pCachedStream_) then
  begin
    pCachedStream_.lLastUsed := clock();
    Inc(pCachedStream_.uiCacheHit);
    bMismatch := false;
    if (pCachedStream_.uiCount = (pCachedStream_.uiCheckFrequency - 1)) then
    begin
      if (nil=pPatchDesc.pVertexStreamZeroData) then
      begin
        if (FAILED(IDirect3DVertexBuffer(pOrigVertexBuffer).Lock(0, 0, {out}TLockData(pCalculateData), 0))) then
        begin
          DxbxKrnlCleanup('Couldn''t lock the original buffer');
        end;
      end;
      // Use the cached stream length (which is a must for the UP stream)
      Checksum := CRC32(PByte(pCalculateData), pCachedStream_.uiLength);
      if (Checksum = pCachedStream_.uiCRC32) then
      begin
        // Take a while longer to check
        if (pCachedStream_.uiCheckFrequency < 32*1024) then
        begin
          pCachedStream_.uiCheckFrequency:= pCachedStream_.uiCheckFrequency * 2;
        end;
        pCachedStream_.uiCount := 0;
      end
      else
      begin
        // TODO -oCXBX: Do something about this
        if (pCachedStream_.bIsUP) then
        begin
          FreeCachedStream(pCachedStream_.pStreamUP);
        end
        else
        begin
          FreeCachedStream(pCachedStream_.Stream.pOriginalStream);
        end;
        pCachedStream_ := NULL;
        bMismatch := true;
      end;
      if (nil=pPatchDesc.pVertexStreamZeroData) then
      begin
        IDirect3DVertexBuffer(pOrigVertexBuffer).Unlock();
      end;
    end
    else
    begin
      Inc(pCachedStream_.uiCount);
    end;
    if (not bMismatch) then
    begin
      if (not pCachedStream_.bIsUP) then
      begin
        m_pStreams[uiStream].pOriginalStream := pOrigVertexBuffer;
        m_pStreams[uiStream].uiOrigStride := uiStride;
        //??DxbxUnlockD3DResource(pCachedStream_);
        g_pD3DDevice.SetStreamSource(uiStream, IDirect3DVertexBuffer(pCachedStream_.Stream.pPatchedStream), {$IFDEF DXBX_USE_D3D9}{OffsetInBytes=}0, {$ENDIF} pCachedStream_.Stream.uiNewStride);
        IDirect3DVertexBuffer(pCachedStream_.Stream.pPatchedStream)._AddRef();
        IDirect3DVertexBuffer(pCachedStream_.Stream.pOriginalStream)._AddRef();
        m_pStreams[uiStream].pPatchedStream := pCachedStream_.Stream.pPatchedStream;
        m_pStreams[uiStream].uiNewStride := pCachedStream_.Stream.uiNewStride;
      end
      else
      begin
        pPatchDesc.pVertexStreamZeroData := pCachedStream_.pStreamUP;
        pPatchDesc.uiVertexStreamZeroStride := pCachedStream_.Stream.uiNewStride;
      end;
      if (pCachedStream_.dwPrimitiveCount > 0) then
      begin
        // The primitives were patched, draw with the correct number of primitives from the cache
        pPatchDesc.dwPrimitiveCount := pCachedStream_.dwPrimitiveCount;
      end;
      bApplied := true;
      m_bPatched := true;
    end;
  end;
  g_PatchedStreamsCache.Unlock();

  if (nil=pPatchDesc.pVertexStreamZeroData) then
  begin
    IDirect3DVertexBuffer(pOrigVertexBuffer)._Release(); // Because we used GetStreamSource
    pOrigVertexBuffer := nil; // Dxbx addition - nil out after decreasing reference count
  end;

  Result := bApplied;
end; // VertexPatcher.ApplyCachedStream
*)

function VertexPatcher.GetNbrStreams(pPatchDesc: PVertexPatchDesc): UINT;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pDynamicPatch: PVERTEX_DYNAMIC_PATCH;
begin
  if (VshHandleIsVertexShader(g_CurrentVertexShader)) then
  begin
    pDynamicPatch := VshGetVertexDynamicPatch(g_CurrentVertexShader);
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

function VertexPatcher.PatchStream(pPatchDesc: PVertexPatchDesc;
                                   uiStream: UINT): _bool;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pStream: PPATCHEDSTREAM;

  pOrigVertexBuffer: XTL_PIDirect3DVertexBuffer8;
  pNewVertexBuffer: XTL_PIDirect3DVertexBuffer8;
  pOrigData: Puint08;
  pNewData: Puint08;
  pOrigVertex: Puint08;
  pNewDataPos: Puint08;
  uiVertexCount: uint;
{$IFDEF DXBX_USE_D3D9}
  uiOffsetInBytes: UINT;
{$ENDIF}
  uiStride: UINT;
  Desc: D3DVERTEXBUFFER_DESC;
  pStreamPatch: PSTREAM_DYNAMIC_PATCH;
  dwNewSize: DWORD;

  uiVertex: uint32;
  dwPosOrig: DWORD;
  uiType: UINT;
  dwPacked: int; // needs to be signed
begin
  DxbxUpdateActiveVertexBufferStreams(); // Do this here, so that GetStreamSource succeeds

  // FVF buffers doesn't have Xbox extensions, but texture coordinates may
  // need normalization if used with linear textures.
  if (VshHandleIsFVF(pPatchDesc.hVertexShader)) then
  begin
    if (pPatchDesc.hVertexShader and D3DFVF_TEXCOUNT_MASK) <> 0 then
    begin
      Result := NormalizeTexCoords(pPatchDesc, uiStream);
      Exit;
    end
    else
    begin
      Result := false;
      Exit;
    end;
  end;

  if (nil=m_pDynamicPatch) or (not m_pDynamicPatch.pStreamPatches[uiStream].NeedPatch) then
  begin
    Result := false;
    Exit;
  end;

  // Do some groovey patchin'

  pStream := @(m_pStreams[uiStream]);
  pStreamPatch := @(m_pDynamicPatch.pStreamPatches[uiStream]);

  if (nil=pPatchDesc.pVertexStreamZeroData) then
  begin
    g_pD3DDevice.GetStreamSource(
      uiStream,
      PIDirect3DVertexBuffer(@pOrigVertexBuffer),
{$IFDEF DXBX_USE_D3D9}
      {out}uiOffsetInBytes,
{$ENDIF}
      {out}uiStride);
    if (FAILED(IDirect3DVertexBuffer(pOrigVertexBuffer).GetDesc({out}Desc))) then
    begin
      DxbxKrnlCleanup('Could not retrieve original buffer size');
    end;
    // Set a new (exact) vertex count
    uiVertexCount := Desc.Size div uiStride;
    // Dxbx addition : Don't update pPatchDesc.dwVertexCount because an indexed draw
    // can (and will) use less vertices than the supplied nr of indexes. Thix fixes
    // the missing parts in the CompressedVertices sample (in Vertex shader mode).
    pStreamPatch.ConvertedStride := max(pStreamPatch.ConvertedStride, uiStride); // ??
    dwNewSize := uiVertexCount * pStreamPatch.ConvertedStride;

    if (FAILED(IDirect3DVertexBuffer(pOrigVertexBuffer).Lock(0, 0, {out}TLockData(pOrigData), 0))) then
    begin
      DxbxKrnlCleanup('Couldn''t lock the original buffer');
    end;
    g_pD3DDevice.CreateVertexBuffer(dwNewSize, {Usage=}0, {FVF=}0, D3DPOOL_MANAGED, PIDirect3DVertexBuffer(@pNewVertexBuffer){$IFDEF DXBX_USE_D3D9}, {pSharedHandle=}NULL{$ENDIF});
    if (FAILED(IDirect3DVertexBuffer(pNewVertexBuffer).Lock(0, 0, {out}TLockData(pNewData), 0))) then
    begin
      DxbxKrnlCleanup('Couldn''t lock the new buffer');
    end;
    if (nil=pStream.pOriginalStream) then
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
      DxbxKrnlCleanup('Trying to patch a Draw..UP with more than stream zero!');
    end;
    uiStride := pPatchDesc.uiVertexStreamZeroStride;
    pStreamPatch.ConvertedStride := max(pStreamPatch.ConvertedStride, uiStride); // ??
    pOrigData := Puint08(pPatchDesc.pVertexStreamZeroData);
    // TODO -oCXBX: This is sometimes the number of indices, which isn't too good
    uiVertexCount := pPatchDesc.dwVertexCount;
    dwNewSize := uiVertexCount * pStreamPatch.ConvertedStride;
    pNewVertexBuffer := NULL;
    pNewData := DxbxCalloc(1, dwNewSize);
    if (nil=pNewData) then
    begin
      DxbxKrnlCleanup('Couldn''t allocate the new stream zero buffer');
    end;
  end;

  if uiVertexCount > 0 then // Dxbx addition, to prevent underflow
  if pStreamPatch.NbrTypes > 0 then // Dxbx addition, to prevent underflow
  for uiVertex := 0 to uiVertexCount - 1 do
  begin
    dwPosOrig := 0;
    pOrigVertex := @pOrigData[uiVertex * uiStride];
    pNewDataPos := @pNewData[uiVertex * pStreamPatch.ConvertedStride];
    for uiType := 0 to pStreamPatch.NbrTypes - 1 do
    begin
      // Dxbx note : The following code handles only the D3DVSDT enums that need conversion;
      // All other cases are catched by the memcpy in the else-block.
      case (pStreamPatch.pTypes[uiType]) of
        X_D3DVSDT_NORMPACKED3: begin // Make it FLOAT3
          dwPacked := int(PDWORDs(@pOrigVertex[dwPosOrig])[0]);

          // Dxbx note : Be aware that shr doesn't do sign-extension in Delphi, so we need to div here to handle the sign correctly :
          // See http://galfar.vevb.net/wp/2009/11/shift-right-delphi-vs-c/
          PFLOATs(pNewDataPos)[0] := (ToFLOAT((dwPacked shl 21) div (1 shl 21))) / 1023.0;
          PFLOATs(pNewDataPos)[1] := (ToFLOAT((dwPacked shl 10) div (1 shl 21))) / 1023.0;
          PFLOATs(pNewDataPos)[2] := (ToFLOAT((dwPacked       ) div (1 shl 22))) /  511.0;

          Inc(dwPosOrig, sizeof(DWORD));
        end;

        X_D3DVSDT_SHORT1: begin // Make it SHORT2 and set the second short to 0
          PSHORTs(pNewDataPos)[0] := PSHORTs(@pOrigVertex[dwPosOrig])[0];
          PSHORTs(pNewDataPos)[1] := $00;

          Inc(dwPosOrig, 1 * sizeof(SHORT));
        end;

        X_D3DVSDT_SHORT3: begin // Make it SHORT4 and set the last short to 1
          PSHORTs(pNewDataPos)[0] := PSHORTs(@pOrigVertex[dwPosOrig])[0];
          PSHORTs(pNewDataPos)[1] := PSHORTs(@pOrigVertex[dwPosOrig])[1];
          PSHORTs(pNewDataPos)[2] := PSHORTs(@pOrigVertex[dwPosOrig])[2];
//          memcpy(pNewDataPos,
//                 @pOrigVertex[dwPosOrig],
//                 3 * sizeof(SHORT));
          PSHORTs(pNewDataPos)[3] := 1; // Dxbx note : NOT 32767 !

          Inc(dwPosOrig, 3 * sizeof(SHORT));
        end;

        X_D3DVSDT_PBYTE1: begin // Make it FLOAT1
          PFLOATs(pNewDataPos)[0] := ToFLOAT(PBYTEs(@pOrigVertex[dwPosOrig])[0]) / 255.0;

          Inc(dwPosOrig, 1 * sizeof(BYTE));
        end;

        X_D3DVSDT_PBYTE2: begin // Make it FLOAT2
          PFLOATs(pNewDataPos)[0] := ToFLOAT(PBYTEs(@pOrigVertex[dwPosOrig])[0]) / 255.0;
          PFLOATs(pNewDataPos)[1] := ToFLOAT(PBYTEs(@pOrigVertex[dwPosOrig])[1]) / 255.0;

          Inc(dwPosOrig, 2 * sizeof(BYTE));
        end;

        X_D3DVSDT_PBYTE3: begin // Make it FLOAT3
          PFLOATs(pNewDataPos)[0] := ToFLOAT(PBYTEs(@pOrigVertex[dwPosOrig])[0]) / 255.0;
          PFLOATs(pNewDataPos)[1] := ToFLOAT(PBYTEs(@pOrigVertex[dwPosOrig])[1]) / 255.0;
          PFLOATs(pNewDataPos)[2] := ToFLOAT(PBYTEs(@pOrigVertex[dwPosOrig])[2]) / 255.0;

          Inc(dwPosOrig, 3 * sizeof(BYTE));
        end;

        X_D3DVSDT_PBYTE4: begin // Make it FLOAT4
          PFLOATs(pNewDataPos)[0] := ToFLOAT(PBYTEs(@pOrigVertex[dwPosOrig])[0]) / 255.0;
          PFLOATs(pNewDataPos)[1] := ToFLOAT(PBYTEs(@pOrigVertex[dwPosOrig])[1]) / 255.0;
          PFLOATs(pNewDataPos)[2] := ToFLOAT(PBYTEs(@pOrigVertex[dwPosOrig])[2]) / 255.0;
          PFLOATs(pNewDataPos)[3] := ToFLOAT(PBYTEs(@pOrigVertex[dwPosOrig])[3]) / 255.0;

          Inc(dwPosOrig, 4 * sizeof(BYTE));
        end;

        X_D3DVSDT_NORMSHORT1: begin // Make it FLOAT2 and set the second float to 0
          PFLOATs(pNewDataPos)[0] := ToFLOAT(PSHORTs(@pOrigVertex[dwPosOrig])[0]) / 32767.0;
          PFLOATs(pNewDataPos)[1] := 0.0;

          Inc(dwPosOrig, 1 * sizeof(SHORT));
        end;

{$IFNDEF DXBX_USE_D3D9} // No need for patching in D3D9
        X_D3DVSDT_NORMSHORT2: begin // Make it FLOAT2
          PFLOATs(pNewDataPos)[0] := ToFLOAT(PSHORTs(@pOrigVertex[dwPosOrig])[0]) / 32767.0;
          PFLOATs(pNewDataPos)[1] := ToFLOAT(PSHORTs(@pOrigVertex[dwPosOrig])[1]) / 32767.0;

          Inc(dwPosOrig, 2 * sizeof(SHORT));
        end;
{$ENDIF}

        X_D3DVSDT_NORMSHORT3: begin // Make it FLOAT3
          PFLOATs(pNewDataPos)[0] := ToFLOAT(PSHORTs(@pOrigVertex[dwPosOrig])[0]) / 32767.0;
          PFLOATs(pNewDataPos)[1] := ToFLOAT(PSHORTs(@pOrigVertex[dwPosOrig])[1]) / 32767.0;
          PFLOATs(pNewDataPos)[2] := ToFLOAT(PSHORTs(@pOrigVertex[dwPosOrig])[2]) / 32767.0;

          Inc(dwPosOrig, 3 * sizeof(SHORT));
        end;

{$IFNDEF DXBX_USE_D3D9} // No need for patching in D3D9
        X_D3DVSDT_NORMSHORT4: begin // Make it FLOAT4
          PFLOATs(pNewDataPos)[0] := ToFLOAT(PSHORTs(@pOrigVertex[dwPosOrig])[0]) / 32767.0;
          PFLOATs(pNewDataPos)[1] := ToFLOAT(PSHORTs(@pOrigVertex[dwPosOrig])[1]) / 32767.0;
          PFLOATs(pNewDataPos)[2] := ToFLOAT(PSHORTs(@pOrigVertex[dwPosOrig])[2]) / 32767.0;
          PFLOATs(pNewDataPos)[3] := ToFLOAT(PSHORTs(@pOrigVertex[dwPosOrig])[3]) / 32767.0;

          Inc(dwPosOrig, 4 * sizeof(SHORT));
        end;
{$ENDIF}

        X_D3DVSDT_FLOAT2H: begin // Make it FLOAT4 and set the third float to 0
          PFLOATs(pNewDataPos)[0] := PFLOATs(@pOrigVertex[dwPosOrig])[0];
          PFLOATs(pNewDataPos)[1] := PFLOATs(@pOrigVertex[dwPosOrig])[1];
          PFLOATs(pNewDataPos)[2] := 0.0;
          PFLOATs(pNewDataPos)[3] := PFLOATs(@pOrigVertex[dwPosOrig])[2];

          Inc(dwPosOrig, 3 * sizeof(FLOAT));
        end;
      (*TODO -oCXBX:
        X_D3DVSDT_NONE: begin
          printf('D3DVSDT_NONE / xbox ext. nsp /');
          dwNewDataType := $FF;
        end;*)
      else
        // Generic 'conversion' - just make a copy :
        memcpy(pNewDataPos, @pOrigVertex[dwPosOrig], pStreamPatch.pSizes[uiType]);
        Inc(dwPosOrig, pStreamPatch.pSizes[uiType]);
      end; // case

      // Increment the new pointer :
      Inc(UIntPtr(pNewDataPos), pStreamPatch.pSizes[uiType]);
    end;
  end;

  if (nil = pPatchDesc.pVertexStreamZeroData) then
  begin
    if Assigned(pNewVertexBuffer) then // Dxbx addition
      IDirect3DVertexBuffer(pNewVertexBuffer).Unlock();
    if Assigned(pOrigVertexBuffer) then // Dxbx addition
      IDirect3DVertexBuffer(pOrigVertexBuffer).Unlock();

    if (FAILED(g_pD3DDevice.SetStreamSource(uiStream, IDirect3DVertexBuffer(pNewVertexBuffer), {$IFDEF DXBX_USE_D3D9}{OffsetInBytes=}0, {$ENDIF} pStreamPatch.ConvertedStride))) then
    begin
      DxbxKrnlCleanup('Failed to set the type patched buffer as the new stream source!');
    end;
    if Assigned(pStream.pPatchedStream) then
    begin
      // The stream was already primitive patched, release the previous vertex buffer to avoid memory leaks
      if IDirect3DVertexBuffer(pStream.pPatchedStream)._Release() = 0 then
        pStream.pPatchedStream := nil; // Dxbx addition - nil out after decreasing reference count
    end;
    pStream.pPatchedStream := pNewVertexBuffer;
  end
  else
  begin
    pPatchDesc.pVertexStreamZeroData := pNewData;
    pPatchDesc.uiVertexStreamZeroStride := pStreamPatch.ConvertedStride;
    if (not m_bAllocatedStreamZeroData) then
    begin
      // The stream was not previously patched. We'll need this when restoring
      m_bAllocatedStreamZeroData := true;
      m_pNewVertexStreamZeroData := pNewData;
    end;
  end;
  pStream.uiOrigStride := uiStride;
  pStream.uiNewStride := pStreamPatch.ConvertedStride;
  m_bPatched := true;

  Result := true;
end; // VertexPatcher.PatchStream

function VertexPatcher.NormalizeTexCoords(pPatchDesc: PVertexPatchDesc; uiStream: UINT): _bool;
// Branch:shogun  Revision:163  Translator:PatrickvL  Done:100
var
  bHasLinearTex: _bool;
  pActivePixelContainer: array [0..X_D3DTS_STAGECOUNT-1] of record bTexIsLinear: _bool; Width, Height: int; end;
  i: uint08;
  pPixelContainer: PX_D3DPixelContainer;
  X_Format: X_D3DFORMAT;
  pOrigVertexBuffer: XTL_PIDirect3DVertexBuffer8;
  pNewVertexBuffer: XTL_PIDirect3DVertexBuffer8;
  pStream: PPATCHEDSTREAM;
  pData: Puint08;
{$IFDEF DXBX_USE_D3D9}
  uiOffsetInBytes: uint;
{$ENDIF}
  uiStride: uint;
  uiVertexCount: uint;
  Desc: D3DVERTEXBUFFER_DESC;

  dwTexN: DWORD;
  uiOffset: uint;
  pUVData: PFLOATs;
  uiVertex: uint32;

  pOrigData: PByte;
begin
  // Dxbx addition :
  // Assert(VshHandleIsFVF(pPatchDesc.hVertexShader), 'This function is only meant for FVF cases!');

  // Check for active linear textures.
  bHasLinearTex := false;
  pStream := nil; // DXBX - pstream might not have been initialized

  for i := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    pPixelContainer := PX_D3DPixelContainer(g_EmuD3DActiveTexture[i]);
    pActivePixelContainer[i].bTexIsLinear := false;
    if Assigned(pPixelContainer) then
    begin
      X_Format := GetD3DFormat(pPixelContainer);
      if EmuXBFormatIsLinear(X_Format) then
      begin
        // This is often hit by the help screen in XDK samples.
        bHasLinearTex := true;
        // Remember linearity, width and height :
        pActivePixelContainer[i].bTexIsLinear := true;
        DxbxDecodeSizeIntoDimensions(pPixelContainer.Size,
          @(pActivePixelContainer[i].Width),
          @(pActivePixelContainer[i].Height),
          {pPitch=}nil);
      end;
    end
  end;

  if (not bHasLinearTex) then
  begin
    Result := false;
    Exit;
  end;

  if Assigned(pPatchDesc.pVertexStreamZeroData) then
  begin
    // In-place patching of inline buffer.
    pNewVertexBuffer := nil;
    pData := Puint08(pPatchDesc.pVertexStreamZeroData);
{$IFDEF DXBX_USE_D3D9}
    uiOffsetInBytes := pPatchDesc.uiOffsetInBytes; // Ignored?
{$ENDIF}
    uiStride := pPatchDesc.uiVertexStreamZeroStride;
    uiVertexCount := pPatchDesc.dwVertexCount;
  end
  else
  begin
    // Copy stream for patching and caching.

    g_pD3DDevice.GetStreamSource(
      uiStream,
      PIDirect3DVertexBuffer(@pOrigVertexBuffer),
{$IFDEF DXBX_USE_D3D9}
      {out}uiOffsetInBytes, // Ignored?
{$ENDIF}
      {out}uiStride);

    if (nil=pOrigVertexBuffer) or (FAILED(IDirect3DVertexBuffer(pOrigVertexBuffer).GetDesc({out}Desc))) then
    begin
      DxbxKrnlCleanup('Could not retrieve original FVF buffer size.');
    end;
    uiVertexCount := Desc.Size div uiStride;

    if(FAILED(IDirect3DVertexBuffer(pOrigVertexBuffer).Lock(0, 0, {out}TLockData(pOrigData), 0))) then
    begin
      DxbxKrnlCleanup('Couldn''t lock original FVF buffer.');
    end;
    g_pD3DDevice.CreateVertexBuffer(Desc.Size, {Usage=}0, {FVF=}0, D3DPOOL_MANAGED, PIDirect3DVertexBuffer(@pNewVertexBuffer){$IFDEF DXBX_USE_D3D9}, {pSharedHandle=}NULL{$ENDIF});
    if(FAILED(IDirect3DVertexBuffer(pNewVertexBuffer).Lock(0, 0, {out}TLockData(pData), 0))) then
    begin
      DxbxKrnlCleanup('Couldn''t lock new FVF buffer.');
    end;
    memcpy(pData, pOrigData, Desc.Size);
    IDirect3DVertexBuffer(pOrigVertexBuffer).Unlock();

    pStream := @m_pStreams[uiStream];
    if (nil=pStream.pOriginalStream) then
    begin
      pStream.pOriginalStream := pOrigVertexBuffer;
    end;
  end;

  // Normalize texture coordinates.
  dwTexN := (pPatchDesc.hVertexShader and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT;
  if (dwTexN >= 1) then // Dxbx addition, no need to test this every loop
  begin
    // Don't normalize coordinates not used by the shader :
    while dwTexN < X_D3DTS_STAGECOUNT do
    begin
      pActivePixelContainer[dwTexN].bTexIsLinear := False;
      Inc(dwTexN);
    end;

    // Locate texture coordinate offset in vertex structure.
    uiOffset := DxbxFVFToVertexSizeInBytes(pPatchDesc.hVertexShader, {aIncludeTextures}False);
    pUVData := PFLOATs(pData + uiOffset);
    if uiVertexCount > 0 then // Dxbx addition, to prevent underflow
    for uiVertex := 0 to uiVertexCount - 1 do
    begin
      if pActivePixelContainer[0].bTexIsLinear then
      begin
        pUVData[0] := pUVData[0] / pActivePixelContainer[0].Width;
        pUVData[1] := pUVData[1] / pActivePixelContainer[0].Height;
      end;

      if pActivePixelContainer[3].bTexIsLinear then
      begin
        pUVData[2] := pUVData[2] / pActivePixelContainer[1].Width;
        pUVData[3] := pUVData[3] / pActivePixelContainer[1].Height;
      end;

      if pActivePixelContainer[2].bTexIsLinear then
      begin
        pUVData[4] := pUVData[4] / pActivePixelContainer[2].Width;
        pUVData[5] := pUVData[5] / pActivePixelContainer[2].Height;
      end;

      if pActivePixelContainer[3].bTexIsLinear then
      begin
        pUVData[6] := pUVData[6] / pActivePixelContainer[3].Width;
        pUVData[7] := pUVData[7] / pActivePixelContainer[3].Height;
      end;

      Inc(Puint08(pUVData), uiStride);
    end;
  end;

  if Assigned(pNewVertexBuffer) then
  begin
    IDirect3DVertexBuffer(pNewVertexBuffer).Unlock();

    if (FAILED(g_pD3DDevice.SetStreamSource(uiStream, IDirect3DVertexBuffer(pNewVertexBuffer), {$IFDEF DXBX_USE_D3D9}{OffsetInBytes=}0, {$ENDIF} uiStride))) then
    begin
      DxbxKrnlCleanup('Failed to set the texcoord patched FVF buffer as the new stream source.');
    end;
    if Assigned(pStream.pPatchedStream) then
    begin
      IDirect3DVertexBuffer(pStream.pPatchedStream)._Release();
    end;

    pStream.pPatchedStream := pNewVertexBuffer;
    pStream.uiOrigStride := uiStride;
    pStream.uiNewStride := uiStride;
    m_bPatched := true;
  end;

  Result := m_bPatched;
end; // VertexPatcher.NormalizeTexCoords

procedure VertexPatcher.PatchPrimitive(pPatchDesc: PVertexPatchDesc);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  if(pPatchDesc.PrimitiveType < X_D3DPT_POINTLIST) or (pPatchDesc.PrimitiveType >= X_D3DPT_MAX) then
  begin
    DxbxKrnlCleanup('Unknown primitive type: 0x%.02X', [Ord(pPatchDesc.PrimitiveType)]);
  end;

  case (pPatchDesc.PrimitiveType) of
    X_D3DPT_QUADLIST: begin
      // Dxbx speedup for Billboard sample - draw 1 quad as 2 triangles :
      if (pPatchDesc.dwVertexCount = 4) then
        // Draw 1 quad as a 2 triangles in a fan (which both have the same winding order) :
        pPatchDesc.PrimitiveType := X_D3DPT_TRIANGLEFAN;
        // Note : We render 2 and more quads using a fixed index list through DrawIndexedPrimitive()
      end;

    X_D3DPT_QUADSTRIP: begin
(* Given trianglestrips work from 1 quad and up, this is not really necessary :
      if (pPatchDesc.dwVertexCount = 4) then
        // Draw 1 quad as a 2 triangles in a fan (which both have the same winding order) :
        pPatchDesc.PrimitiveType := X_D3DPT_TRIANGLEFAN
      else*)
        // A quadstrip starts with 4 vertices and adds 2 vertices per additional quad.
        // This is much like a trianglstrip, which starts with 3 vertices and adds
        // 1 vertex per additional triangle, so we use that instead. The planar nature
        // of the quads 'survives' through this change. There's a catch though :
        // In a trianglestrip, every 2nd triangle has an opposing winding order,
        // which would cause backface culling - but this seems to be intelligently
        // handled by d3d :
        pPatchDesc.PrimitiveType := X_D3DPT_TRIANGLESTRIP;
      end;

    // Convex polygon is the same as a triangle fan.
    X_D3DPT_POLYGON: begin
      pPatchDesc.PrimitiveType := X_D3DPT_TRIANGLEFAN;
      end;
  end;

  pPatchDesc.dwPrimitiveCount := EmuD3DVertex2PrimitiveCount(pPatchDesc.PrimitiveType, pPatchDesc.dwVertexCount);
end; // VertexPatcher.PatchPrimitive

function VertexPatcher.Apply(pPatchDesc: PVertexPatchDesc; pbFatalError: P_bool): _bool;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  Patched: _bool;
  uiStream: UINT;
  LocalPatched: _bool;
begin
  Patched := false;
  // Get the number of streams
  m_uiNbrStreams := GetNbrStreams(pPatchDesc);
  if (VshHandleIsVertexShader(pPatchDesc.hVertexShader)) then
  begin
    m_pDynamicPatch := @(PVERTEX_SHADER(VshHandleGetVertexShader(pPatchDesc.hVertexShader).Handle).VertexDynamicPatch);
  end;
  if m_uiNbrStreams > 0 then // Dxbx addition, to prevent underflow
  for uiStream := 0 to m_uiNbrStreams - 1 do
  begin
    LocalPatched := false;

//    if (ApplyCachedStream(pPatchDesc, uiStream, pbFatalError)) then
//    begin
//      m_pStreams[uiStream].bUsedCached := true;
//      continue;
//    end;

    if PatchStream(pPatchDesc, uiStream) then
      LocalPatched := True;

// TODO : Fix the following caching code, which is leaking hundreds of MBs in the PointSprites and XMarbles XDK samples!
//    if LocalPatched and (nil=pPatchDesc.pVertexStreamZeroData) then
//    begin
//      // Insert the patched stream in the cache
//      CacheStream(pPatchDesc, uiStream);
//      m_pStreams[uiStream].bUsedCached := true;
//    end;

    Patched := Patched or LocalPatched;
  end;

  PatchPrimitive(pPatchDesc);

  Result := Patched;
end; // VertexPatcher.Apply

function VertexPatcher.Restore(): _bool;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  uiStream: UINT;
begin
  if (not m_bPatched) then
  begin
    Result := false;
    Exit;
  end;

  if m_uiNbrStreams > 0 then // Dxbx addition, to prevent underflow
  for uiStream := 0 to m_uiNbrStreams - 1 do
  begin
    if (m_pStreams[uiStream].pOriginalStream <> NULL) and (m_pStreams[uiStream].pPatchedStream <> NULL) then
    begin
      g_pD3DDevice.SetStreamSource(0, IDirect3DVertexBuffer(m_pStreams[uiStream].pOriginalStream), {$IFDEF DXBX_USE_D3D9}{OffsetInBytes=}0, {$ENDIF} m_pStreams[uiStream].uiOrigStride);
    end;

    if (m_pStreams[uiStream].pOriginalStream <> NULL) then
    begin
      // Release the reference to original stream we got via GetStreamSource() :
      if IDirect3DVertexBuffer(m_pStreams[uiStream].pOriginalStream)._Release() = 0 then
        m_pStreams[uiStream].pOriginalStream := nil; // Dxbx addition - nil out after decreasing reference count
    end;

    if (m_pStreams[uiStream].pPatchedStream <> NULL) then
    begin
      if IDirect3DVertexBuffer(m_pStreams[uiStream].pPatchedStream)._Release() = 0 then
        m_pStreams[uiStream].pPatchedStream := nil; // Dxbx addition - nil out after decreasing reference count
    end;

    if (not m_pStreams[uiStream].bUsedCached) then
    begin
      if (Self.m_bAllocatedStreamZeroData) then
      begin
        DxbxFree(m_pNewVertexStreamZeroData);
        m_pNewVertexStreamZeroData := nil; // Dxbx addition
        Self.m_bAllocatedStreamZeroData := False; // Dxbx addition
      end;
    end
    else
    begin
      m_pStreams[uiStream].bUsedCached := false;
    end;

  end;

  Result := true;
end; // VertexPatcher.Restore

procedure XTL_EmuFlushIVB(); {NOPATCH}
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pdwVB: PDWORD;
  uiStride: UINT;
  bFVF: _bool;
  dwCurFVF: DWORD;
  v: uint;
  g_IVBTable_v: PD3DIVB;
  dwPos: DWORD;
  dwTexN: DWORD;
  VPDesc: VertexPatchDesc;
  VertPatch: VertexPatcher;
  //bPatched: _bool;
begin
  // Make sure g_pIVBVertexBuffer has enough space :
  if Length(g_pIVBVertexBuffer) < Integer(sizeof(_D3DIVB)*g_IVBTblOffs) then
    SetLength(g_pIVBVertexBuffer, (sizeof(_D3DIVB)*g_IVBTblOffs));

  // Dxbx note : Cxbx re-uses g_IVBTable, but has a risk over overwriting data,
  // so we populate g_pIVBVertexBuffer instead :
  pdwVB := PDWORD(g_pIVBVertexBuffer);

  // Parse IVB table with current FVF shader if possible.
  bFVF := VshHandleIsFVF(g_CurrentVertexShader);

  if(bFVF and ((g_CurrentVertexShader and D3DFVF_POSITION_MASK) <> D3DFVF_XYZRHW)) then
  begin
    dwCurFVF := g_CurrentVertexShader;

    if IsRunning(TITLEID_Halo) then
    begin
      // HACK: Halo...
      if(dwCurFVF = 0) then
      begin
        EmuWarning('EmuFlushIVB(): using g_IVBFVF instead of current FVF!');
        dwCurFVF := g_IVBFVF;
      end;
    end;
  end
  else
  begin
    dwCurFVF := g_IVBFVF;
  end;

  if MayLog(lfUnit or lfTrace) then
    DbgPrintf('g_IVBTblOffs := %d', [g_IVBTblOffs]);

  // Dxbx note : Do this once, not inside the for-loop :
  dwPos := dwCurFVF and D3DFVF_POSITION_MASK;
  dwTexN := (dwCurFVF and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT;

  if g_IVBTblOffs > 0 then // Dxbx addition, to prevent underflow
  for v := 0 to g_IVBTblOffs - 1 do
  begin
    // Direct pointer, to optimize the code a little :
    g_IVBTable_v := @g_IVBTable[v];
    // TODO : Move all logging to an inline function, to prevent finalization of the array-arguments to DbgPrintf

    if(dwPos = D3DFVF_XYZRHW) then
    begin
      PFLOATs(pdwVB)[0] := g_IVBTable_v.Position.x;
      PFLOATs(pdwVB)[1] := g_IVBTable_v.Position.y;
      PFLOATs(pdwVB)[2] := g_IVBTable_v.Position.z;
      PFLOATs(pdwVB)[3] := g_IVBTable_v.Rhw;
      Inc(PFLOAT(pdwVB), 4);

      if MayLog(lfUnit or lfTrace) then
        DbgPrintf('IVB Position := {%f, %f, %f, %f}', [g_IVBTable_v.Position.x, g_IVBTable_v.Position.y, g_IVBTable_v.Position.z, g_IVBTable_v.Position.z, g_IVBTable_v.Rhw]);
    end
    else // XYZRHW cannot be combined with NORMAL, but the other XYZ formats can :
    begin
      if(dwPos = D3DFVF_XYZ) then
      begin
        PFLOATs(pdwVB)[0] := g_IVBTable_v.Position.x;
        PFLOATs(pdwVB)[1] := g_IVBTable_v.Position.y;
        PFLOATs(pdwVB)[2] := g_IVBTable_v.Position.z;
        Inc(PFLOAT(pdwVB), 3);

        if MayLog(lfUnit or lfTrace) then
          DbgPrintf('IVB Position := {%f, %f, %f}', [g_IVBTable_v.Position.x, g_IVBTable_v.Position.y, g_IVBTable_v.Position.z]);
      end
      else if(dwPos = D3DFVF_XYZB1) then
      begin
        PFLOATs(pdwVB)[0] := g_IVBTable_v.Position.x;
        PFLOATs(pdwVB)[1] := g_IVBTable_v.Position.y;
        PFLOATs(pdwVB)[2] := g_IVBTable_v.Position.z;
        PFLOATs(pdwVB)[3] := g_IVBTable_v.Blend1;
        Inc(PFLOAT(pdwVB), 4);

        if MayLog(lfUnit or lfTrace) then
          DbgPrintf('IVB Position := {%f, %f, %f, %f}', [g_IVBTable_v.Position.x, g_IVBTable_v.Position.y, g_IVBTable_v.Position.z, g_IVBTable_v.Blend1]);
      end
      else if(dwPos = D3DFVF_XYZB2) then
      begin
        PFLOATs(pdwVB)[0] := g_IVBTable_v.Position.x;
        PFLOATs(pdwVB)[1] := g_IVBTable_v.Position.y;
        PFLOATs(pdwVB)[2] := g_IVBTable_v.Position.z;
        PFLOATs(pdwVB)[3] := g_IVBTable_v.Blend1;
        PFLOATs(pdwVB)[4] := g_IVBTable_v.Blend2;
        Inc(PFLOAT(pdwVB), 5);

        if MayLog(lfUnit or lfTrace) then
          DbgPrintf('IVB Position := {%f, %f, %f, %f, %f}', [g_IVBTable_v.Position.x, g_IVBTable_v.Position.y, g_IVBTable_v.Position.z, g_IVBTable_v.Blend1, g_IVBTable_v.Blend2]);
      end
      else if(dwPos = D3DFVF_XYZB3) then
      begin
        PFLOATs(pdwVB)[0] := g_IVBTable_v.Position.x;
        PFLOATs(pdwVB)[1] := g_IVBTable_v.Position.y;
        PFLOATs(pdwVB)[2] := g_IVBTable_v.Position.z;
        PFLOATs(pdwVB)[3] := g_IVBTable_v.Blend1;
        PFLOATs(pdwVB)[4] := g_IVBTable_v.Blend2;
        PFLOATs(pdwVB)[5] := g_IVBTable_v.Blend3;
        Inc(PFLOAT(pdwVB), 6);

        if MayLog(lfUnit or lfTrace) then
          DbgPrintf('IVB Position := {%f, %f, %f, %f, %f, %f}', [g_IVBTable_v.Position.x, g_IVBTable_v.Position.y, g_IVBTable_v.Position.z, g_IVBTable_v.Blend1, g_IVBTable_v.Blend2, g_IVBTable_v.Blend3]);
      end
      else if(dwPos = D3DFVF_XYZB4) then
      begin
        PFLOATs(pdwVB)[0] := g_IVBTable_v.Position.x;
        PFLOATs(pdwVB)[1] := g_IVBTable_v.Position.y;
        PFLOATs(pdwVB)[2] := g_IVBTable_v.Position.z;
        PFLOATs(pdwVB)[3] := g_IVBTable_v.Blend1;
        PFLOATs(pdwVB)[4] := g_IVBTable_v.Blend2;
        PFLOATs(pdwVB)[5] := g_IVBTable_v.Blend3;
        PFLOATs(pdwVB)[6] := g_IVBTable_v.Blend4;
        Inc(PFLOAT(pdwVB), 7);

        if MayLog(lfUnit or lfTrace) then
          DbgPrintf('IVB Position := {%f, %f, %f, %f, %f, %f, %f}', [g_IVBTable_v.Position.x, g_IVBTable_v.Position.y, g_IVBTable_v.Position.z, g_IVBTable_v.Blend1, g_IVBTable_v.Blend2, g_IVBTable_v.Blend3, g_IVBTable_v.Blend4]);
      end
      else
      begin
        DxbxKrnlCleanup('Unsupported Position Mask (FVF := 0x%.08X dwPos := 0x%.08X)', [g_IVBFVF, dwPos]);
      end;

      if(dwCurFVF and D3DFVF_NORMAL) > 0 then
      begin
        PFLOATs(pdwVB)[0] := g_IVBTable_v.Normal.x;
        PFLOATs(pdwVB)[1] := g_IVBTable_v.Normal.y;
        PFLOATs(pdwVB)[2] := g_IVBTable_v.Normal.z;
        Inc(PFLOAT(pdwVB), 3);

        if MayLog(lfUnit or lfTrace) then
          DbgPrintf('IVB Normal := {%f, %f, %f}', [g_IVBTable_v.Normal.x, g_IVBTable_v.Normal.y, g_IVBTable_v.Normal.z]);
      end;
    end;

    if(dwCurFVF and D3DFVF_DIFFUSE) > 0 then
    begin
      PDWORDs(pdwVB)[0] := g_IVBTable_v.dwDiffuse; Inc(PDWORD(pdwVB));

      if MayLog(lfUnit or lfTrace) then
        DbgPrintf('IVB Diffuse := 0x%.08X', [g_IVBTable_v.dwDiffuse]);
    end;

    if(dwCurFVF and D3DFVF_SPECULAR) > 0 then
    begin
      PDWORDs(pdwVB)[0] := g_IVBTable_v.dwSpecular; Inc(PDWORD(pdwVB));

      if MayLog(lfUnit or lfTrace) then
        DbgPrintf('IVB Specular := 0x%.08X', [g_IVBTable_v.dwSpecular]);
    end;

    // TODO -oDxbx : Handle other sizes than D3DFVF_TEXCOORDSIZE2 too!
    if(dwTexN >= 1) then
    begin
      PFLOATs(pdwVB)[0] := g_IVBTable_v.TexCoord1.x;
      PFLOATs(pdwVB)[1] := g_IVBTable_v.TexCoord1.y;
      Inc(PFLOAT(pdwVB), 2);

      if MayLog(lfUnit or lfTrace) then
        DbgPrintf('IVB TexCoord1 := {%f, %f}', [g_IVBTable_v.TexCoord1.x, g_IVBTable_v.TexCoord1.y]);
//    end;

    if(dwTexN >= 2) then
    begin
      PFLOATs(pdwVB)[0] := g_IVBTable_v.TexCoord2.x;
      PFLOATs(pdwVB)[1] := g_IVBTable_v.TexCoord2.y;
      Inc(PFLOAT(pdwVB), 2);

      if MayLog(lfUnit or lfTrace) then
        DbgPrintf('IVB TexCoord2 := {%f, %f}', [g_IVBTable_v.TexCoord2.x, g_IVBTable_v.TexCoord2.y]);
//    end;

    if(dwTexN >= 3) then
    begin
      PFLOATs(pdwVB)[0] := g_IVBTable_v.TexCoord3.x;
      PFLOATs(pdwVB)[1] := g_IVBTable_v.TexCoord3.y;
      Inc(PFLOAT(pdwVB), 2);

      if MayLog(lfUnit or lfTrace) then
        DbgPrintf('IVB TexCoord3 := {%f, %f}', [g_IVBTable_v.TexCoord3.x, g_IVBTable_v.TexCoord3.y]);
//    end;

    if(dwTexN >= 4) then
    begin
      PFLOATs(pdwVB)[0] := g_IVBTable_v.TexCoord4.x;
      PFLOATs(pdwVB)[1] := g_IVBTable_v.TexCoord4.y;
      Inc(PFLOAT(pdwVB), 2);

      if MayLog(lfUnit or lfTrace) then
        DbgPrintf('IVB TexCoord4 := {%f, %f}', [g_IVBTable_v.TexCoord4.x, g_IVBTable_v.TexCoord4.y]);
    end;
    end;
    end;
    end;
  end;

  DxbxUpdateNativeD3DResources();

  VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer

  // Dxbx note : Instead of calculating this above (when v=0),
  // we use a tooling function to determine the vertex stride :
  uiStride := DxbxFVFToVertexSizeInBytes(dwCurFVF);

  VPDesc.PrimitiveType := g_IVBPrimitiveType;
  VPDesc.dwVertexCount := g_IVBTblOffs;
  VPDesc.pVertexStreamZeroData := g_pIVBVertexBuffer;
  VPDesc.uiVertexStreamZeroStride := uiStride;
  VPDesc.hVertexShader := dwCurFVF; // TODO -oDxbx : Why does Cxbx use g_CurrentVertexShader ?

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

  {bPatched := }VertPatch.Apply(@VPDesc, NULL);

  // Disable this 'fix', as it doesn't really help; On ATI, it isn't needed (and causes missing
  // textures if enabled). On Nvidia, it stops the jumping (but also removes the font from view).
  // So I think it's better to keep this bug visible, as a motivation for a real fix, and better
  // rendering on ATI chipsets...

//  bFVF := True; // This fixes jumping triangles on Nvidia chipsets, as suggested by Defiance
  // As a result however, this change also seems to remove the texture of the fonts in XSokoban!?!

  if(bFVF) then
  begin
{$IFDEF DXBX_USE_D3D9}
    g_pD3DDevice.SetVertexShader(NULL);
    g_pD3DDevice.SetFVF(dwCurFVF);
{$ELSE}
    g_pD3DDevice.SetVertexShader(dwCurFVF);
{$ENDIF}
  end;

  DxbxDrawPrimitiveUP(VPDesc);

  if(bFVF) then
  begin
{$IFDEF DXBX_USE_D3D9}
    g_pD3DDevice.SetVertexShader(NULL);
    g_pD3DDevice.SetFVF(g_CurrentVertexShader);
{$ELSE}
    g_pD3DDevice.SetVertexShader(g_CurrentVertexShader);
{$ENDIF}
  end;

  VertPatch.Restore();

  // Clear the portion that was in use previously (as only that part was written to) :
  if g_IVBTblOffs > 0 then
    ZeroMemory(@g_IVBTable[0], sizeof(g_IVBTable[0])*(g_IVBTblOffs+1));

  g_IVBTblOffs := 0;
end; // XTL_EmuFlushIVB

end.
