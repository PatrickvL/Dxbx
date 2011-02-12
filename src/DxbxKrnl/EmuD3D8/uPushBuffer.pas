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
  , uEmuAlloc
  , uResourceTracker
  , uEmuD3D8Types
  , uEmuD3D8Utils
  , uVertexBuffer
  , uEmu
  , uEmuXG;

type
  Pusher = record
    m_pPut: PDWord; // This is the address to where the CPU will write it's next GPU instruction
    m_pThreshold: PDWord; // This is the upper limit for m_pPut (when it's reached, MakeSpace() is called,
    // which just forwards the call to MakeRequestedSpace, passing it m_PushSegmentSize/2 as 'minimum space',
    // and m_PushSegmentSize (without division) as 'requested space')
  end;
  PPusher = ^Pusher;

// From PushBuffer.h :

(*
procedure XTL_EmuExecutePushBuffer
(
  pPushBuffer: PX_D3DPushBuffer;
  pFixup: PX_D3DFixup
); {NOPATCH}

procedure XTL_EmuApplyPushBufferFixup
(
  pdwPushData: PDWORD;
  pdwFixupData: PDWORD
); {NOPATCH}
*)

procedure XTL_EmuExecutePushBufferRaw
(
  pdwPushData: PDWORD;
  pdwPushDataEnd: PDWORD
); {NOPATCH}

// primary push buffer
var g_dwPrimaryPBCount: uint32 = 0;
var g_pPrimaryPB: PDWORD = nil; // Dxbx note : Cxbx uses Puint32 for this

// push buffer debugging
var g_bPBSkipPusher: _bool = false;

{$IFDEF _DEBUG_TRACK_PB}
procedure DbgDumpMesh(pIndexData: PWORD; dwCount: DWORD); {NOPATCH}
{$ENDIF}

implementation

uses
  // Dxbx
    uDxbxKrnlUtils
  , uLog
  , uConvert
  , uNV2A
  , uMiniport
  , uEmuD3D8 // DxbxPresent
  , uState
  , uVertexShader;


const NV2A_JMP_FLAG         = $00000001;
const NV2A_CALL_FLAG        = $00000002; // TODO : Should JMP & CALL be switched?
const NV2A_ADDR_MASK        = $FFFFFFFC;
const NV2A_METHOD_MASK      = $00001FFC;
const NV2A_SUBCH_MASK       = $0000E000;
const NV2A_COUNT_MASK       = $0FFF0000; // 12 bits
const NV2A_NOINCREMENT_FLAG = $40000000;
// Dxbx note : What do the other bits mean (mask $B0000000) ?

const NV2A_METHOD_SHIFT = 0; // Dxbx note : Not 2, because methods are actually DWORD offsets (and thus defined with increments of 4)
const NV2A_SUBCH_SHIFT = 12;
const NV2A_COUNT_SHIFT = 18;

const NV2A_METHOD_MAX = (NV2A_METHOD_MASK or 3) shr NV2A_METHOD_SHIFT; // = 8191
const NV2A_COUNT_MAX = NV2A_COUNT_MASK shr NV2A_COUNT_SHIFT; // = 2047

const lfUnit = lfCxbx or lfPushBuffer;

procedure D3DPUSH_DECODE(const dwPushCommand: DWORD; out dwMethod, dwSubCh, dwCount: DWORD; out bNoInc: BOOL_);
begin
  {out}dwMethod := (dwPushCommand and NV2A_METHOD_MASK) {shr NV2A_METHOD_SHIFT};
  {out}dwSubCh  := (dwPushCommand and NV2A_SUBCH_MASK) shr NV2A_SUBCH_SHIFT;
  {out}dwCount  := (dwPushCommand and NV2A_COUNT_MASK) shr NV2A_COUNT_SHIFT;
  {out}bNoInc   := (dwPushCommand and NV2A_NOINCREMENT_FLAG) > 0;
end;

// From PushBuffer.cpp :

(*
procedure XTL_EmuExecutePushBuffer
(
    pPushBuffer: PX_D3DPushBuffer;
    pFixup: PX_D3DFixup
); {NOPATCH}
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  if (pFixup <> NULL) then
  begin
    XTL_EmuApplyPushBufferFixup(PDWORD(pPushBuffer.Data), PDWORD(pFixup.Data + pFixup.Run));
    // TODO : Should we change this in a while Assigned(pFixup := pFixup.Next) ?
  end;

  XTL_EmuExecutePushBufferRaw(PDWORD(pPushBuffer.Data));
end;

procedure XTL_EmuApplyPushBufferFixup
(
  pdwPushData: PDWORD;
  pdwFixupData: PDWORD
); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  SizeInBytes: UInt;
  OffsetInBytes: UInt;
begin
  while True do
  begin
    SizeInBytes := pdwFixupData^;
    if SizeInBytes = $FFFFFFFF then
      Exit;

    Inc(pdwFixupData);
    OffsetInBytes := pdwFixupData^;
    Inc(pdwFixupData);

    memcpy({dest=}Pointer(UIntPtr(pdwPushData) + OffsetInBytes), {src=}pdwFixupData, SizeInBytes);
    Inc(UIntPtr(pdwFixupData), SizeInBytes);
  end;

{
  When IDirect3DDevice8::RunPushBuffer is called with a fix-up object specified,
  it will parse the fix-up data pointed to by Data and with a byte offset of Run.

  The fix-up data is encoded as follows. The first DWORD is the size, in bytes,
  of the push-buffer fix-up to be modified. The second DWORD is the offset, in bytes,
  from the start of the push-buffer where the fix-up is to be modified.

  The subsequent DWORDS are the data to be copied. This encoding repeats for every fix-up to be done,
  until it terminates with a size value of 0xffffffff.

  The offsets must be in an increasing order.
}
end;
*)

type
  TPostponedDrawType = (pdUndetermined, pdDrawVertices, pdDrawIndexedVertices, pdDrawVerticesUP, pdDrawIndexedVerticesUP);

  PDxbxPushBufferState = ^TDxbxPushBufferState;
  TDxbxPushBufferState = record
  public
    NV2AInstance: RNV2AInstance;

  // Draw[Indexed]Vertices[UP] related :
  public
    VertexFormat: array [0..15] of record Stride, Size, Type_: int; end;
    procedure RegisterVertexFormat(Slot: UINT; pdwPushArguments: PDWORD);
  public
    VertexAddress: array [0..3] of Pointer;
    procedure RegisterVertexAddress(Stage: UINT; pdwPushArguments: PDWORD);
  public
    VertexIndex: INT;
    VertexCount: UINT;
    procedure RegisterVertexBatch(pdwPushArguments: PDWORD);
  public
    XBPrimitiveType: X_D3DPRIMITIVETYPE;
    PostponedDrawType: TPostponedDrawType;
    procedure PostponedDrawVertices;
    procedure PostponedDrawIndexedVertices;
    procedure PostponedDrawVerticesUP;
    procedure PostponedDrawIndexedVerticesUP;

  // SetVertesShader :
  public
    VertexShaderSlots: array [0..D3DVS_XBOX_NR_ADDRESS_SLOTS-1] of DWORD;
    function HandleSetVertexShaderBatch(pdwPushArguments: PDWORD; dwCount: DWORD): Integer;

  // SetRenderState :
  public
    function TrySetRenderState(dwMethod: DWORD; pdwPushArguments: PDWORD; dwCount: DWORD): Boolean;

  // Triggers :
  public
    Viewport: D3DVIEWPORT;
    procedure TriggerClearBuffers();
    procedure TriggerSetRenderTarget();
    procedure TriggerSetViewport();
    function TriggerModelViewMatrix(pdwPushArguments: PDWORD; dwCount: DWORD): DWORD;
    function TriggerCompositeMatrix(pdwPushArguments: PDWORD; dwCount: DWORD): DWORD;
    function TriggerSetVertexShaderConstant(pdwPushArguments: PDWORD; dwCount: DWORD): DWORD;
    procedure TriggerDrawBeginEnd();

  // Globals and controller :
  public
    OldRegisterValue: DWORD;
    PrevMethod: array [0..2-1] of DWORD;
    ZScale: FLOAT;
    function SeenRecentMethod(Method: DWORD): Boolean;
    procedure ExecutePushBufferRaw(pdwPushData, pdwPushDataEnd: PDWord);

  // TODO : All below must be re-implemented :
  public
    pVertexData: PVOID;
    dwVertexShader: DWord;
    dwStride: DWord;
    NrCachedIndices: int;
    StartIndex: UINT;
    pIBMem: array [0..2-1] of WORD;
    pIndexBuffer: XTL_LPDIRECT3DINDEXBUFFER8; // = XTL_PIDirect3DIndexBuffer8
    // pVertexBuffer: XTL_LPDIRECT3DVERTEXBUFFER8; // = XTL_PIDirect3DVertexBuffer8
    maxIBSize: uint;
    procedure _RenderIndexedVertices(dwCount: DWORD);
    function HandleVertexData(pdwPushArguments: PDWORD; dwCount: DWORD): Integer;
    function HandleIndex32(pdwPushArguments: PDWORD; dwCount: DWORD): Integer;
    function HandleIndex16_16(pdwPushArguments: PDWORD; dwCount: DWORD): Integer;
  end;

procedure DWORDSplit2(const aValue: DWORD; w1: Integer; out v1: Integer; w2: Integer; out v2: Integer);
begin
  {out}v1 := (aValue       ) and ((1 shl w1) - 1);
  {out}v2 := (aValue shr w1) and ((1 shl w2) - 1);
end;

function SwapRgb(color: D3DCOLOR): DWORD;
begin
  Result :=  (color and $ff00ff00)
         or ((color and $00ff0000) shr 16)
         or ((color and $000000ff) shl 16);
end;

{ TDxbxPushBufferState }

procedure TDxbxPushBufferState.RegisterVertexFormat(Slot: UINT; pdwPushArguments: PDWORD);
// TODO : Do this in PostponedDraw
begin
  // Register vertex format (bits:31-8=Stride,7-4=Size,3-0=Type) per slot :
  VertexFormat[Slot].Stride := pdwPushArguments^ shr 8;
  VertexFormat[Slot].Size := (pdwPushArguments^ shr 4) and $f; // Size:1..4=1..4,7=3w?
  VertexFormat[Slot].Type_ := pdwPushArguments^ and $f; // Type:1=S1,2=F,4=UB_OGL,5=S32K,6=CMP?
end;

procedure TDxbxPushBufferState.RegisterVertexAddress(Stage: UINT; pdwPushArguments: PDWORD);
begin
  // Register vertex buffer address (this address is a combination of all levels of offsets & indexes) per stage :
  VertexAddress[Stage] := Pointer(pdwPushArguments^);
end;

procedure TDxbxPushBufferState.RegisterVertexBatch(pdwPushArguments: PDWORD);
// This command just registers the number of vertices are to be used in D3DDevice_DrawVertices.
var
  BatchCount: Integer;
  BatchIndex: Integer;
begin
  // Decode the arguments (index is an incrementing number, count is cumulative per batch) :
  DWORDSplit2(pdwPushArguments^, 24, {out}BatchIndex, 8, {out}BatchCount);
  Inc(BatchCount);

  // Accumulate the vertices for the draw that will follow :
  Inc(VertexCount, BatchCount);
  // Register the index only once :
  if VertexIndex >= 0 then
    Exit;

  // Note : Additional commands will mention an index right next to the previous batch
  VertexIndex := BatchIndex;

  // Register that the postponed draw will be a DrawVertices() :
  PostponedDrawType := pdDrawVertices;
end;

procedure TDxbxPushBufferState.PostponedDrawVertices;
//var
//  VPDesc: VertexPatchDesc;
//  VertPatch: VertexPatcher;
begin
  // TODO : Parse all NV2A_VTXBUF_ADDRESS and NV2A_VTXFMT data here, so that we know how where the vertex data is, and what format it has.
  // Effectively, we do RegisterVertexFormat and RegisterVertexAddress here.

  DbgPrintf('  DrawPrimitive VertexIndex=%d, VertexCount=%d', [VertexIndex, VertexCount]);
(*
  VPDesc.VertexPatchDesc();

  VPDesc.PrimitiveType := XBPrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
//   VPDesc.pVertexStreamZeroData := pVertexData;
//   VPDesc.uiVertexStreamZeroStride := dwStride;
//   VPDesc.hVertexShader := dwVertexShader;

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

//   DxbxDrawPrimitiveUP(VPDesc);

  VertPatch.Restore();
*)
end;

procedure TDxbxPushBufferState.PostponedDrawIndexedVertices;
begin
end;

procedure TDxbxPushBufferState.PostponedDrawVerticesUP;
begin
end;

procedure TDxbxPushBufferState.PostponedDrawIndexedVerticesUP;
begin
end;

function TDxbxPushBufferState.TrySetRenderState(dwMethod: DWORD; pdwPushArguments: PDWORD; dwCount: DWORD): Boolean;
var
  XRenderState: X_D3DRenderStateType;
begin
  // See if this method is actually a render state :
  XRenderState := DxbxXboxMethodToRenderState(dwMethod);
  Result := XRenderState in [X_D3DRS_FIRST..X_D3DRS_LAST];
  if Result then
    DxbxSetRenderStateInternal('  NV2A SetRenderState', XRenderState, pdwPushArguments^);
end;

//
// Clear
//

procedure TDxbxPushBufferState.TriggerClearBuffers();
var
  ClearRect: RECT;
  ClearDepthValue: DWORD;
  PCFlags: DWORD;
  ClearZ: Single;
  ClearStencil: DWORD;
begin
  // make adjustments to parameters to make sense with windows d3d
  begin
    DWORDSplit2(NV2AInstance.CLEAR_RECT_HORIZONTAL, 16, {out}ClearRect.Left, 16, {out}ClearRect.Right);
    DWORDSplit2(NV2AInstance.CLEAR_RECT_VERTICAL, 16, {out}ClearRect.Top, 16, {out}ClearRect.Bottom);

    ClearDepthValue := NV2AInstance.CLEAR_DEPTH_VALUE;

    // First, convert from Xbox to PC, after which we'll remove the invalid flags :
    PCFlags := EmuXB2PC_D3DCLEAR_FLAGS({XClearFlags=}NV2AInstance.CLEAR_BUFFERS);
(*
    // Only clear ZBuffer if we actually have one :
    if Assigned(g_EmuD3DActiveDepthStencil)
    and EmuXBFormatIsDepthBuffer(GetD3DFormat(g_EmuD3DActiveDepthStencil)) then
      // Allow depth to be cleared (if requested)
    else
      PCFlags := PCFlags and (not D3DCLEAR_ZBUFFER);

    // Only clear Stencil buffer if there actually is one :
    // if not g_EmuCDPD.NativePresentationParameters.EnableAutoDepthStencil then
    // TODO -oDxbx: The above check should work (but doesn't!) so for now look at the Xbox PresParam instead :
    if Assigned(g_EmuD3DActiveDepthStencil)
    and EmuXBFormatHasChannel(GetD3DFormat(g_EmuD3DActiveDepthStencil), S) then
      // Allow stencil to be cleared (if requested)
    else
      PCFlags := PCFlags and (not D3DCLEAR_STENCIL);
*)
  end;

  // Since we filter the flags, make sure there are some left (else, clear isn't necessary) :
  if PCFlags > 0 then
  begin
    // Before clearing, make sure we have the correct render target :
//    DxbxUpdateActiveRenderTarget(); // TODO : Or should we have to call DxbxUpdateNativeD3DResources ?

    ClearZ := ClearDepthValue shr 8; // TODO : Convert this to the right value for all depth-buffer formats
    ClearStencil := ClearDepthValue and $FF;

    g_pD3DDevice.Clear(1, @ClearRect, PCFlags, NV2AInstance.CLEAR_VALUE, ClearZ, ClearStencil);
  end;
end;

procedure TDxbxPushBufferState.TriggerSetRenderTarget();
var
  SurfaceFormat: X_D3DFORMAT;
//  ActiveRenderTargetData: UIntPtr;
//  ActiveRenderTargetWidth: DWORD;
//  ActiveRenderTargetHeight: DWORD;
//  ActiveDepthStencilData: UIntPtr;
begin
  // Always calculate ZScale and other factors like SuperSampleScaleX,
  // based on NV2AInstance.RT_FORMAT (our trigger) :

  ZScale := (2 shl 24) - 1.0; // TODO : Calculate real Z scale based on active depth buffer format (D24S8, D16, etc)
//  SurfaceFormat := X_D3DFORMAT((NV2AInstance.RT_FORMAT and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT);
//  if EmuXBFormatIsDepthBuffer(SurfaceFormat) then
//    ZScale := 1.0;

  // Check if we just seen a StencilEnable method, which means we're really handling SetRenderTarget here.
  if not SeenRecentMethod(NV2A_STENCIL_ENABLE) then
    // If not, we're dealing with a temporary switch of NV2A_RT_FORMAT,
    // which seems to be done by D3DDevice_Clear to support swizzled surface clears.
    Exit;

//  ActiveRenderTargetData := NV2AInstance.COLOR_OFFSET;
//  ActiveDepthStencilData := NV2AInstance.ZETA_OFFSET;
//  ActiveRenderTargetWidth := NV2AInstance.RT_HORIZ shr 16;
//  ActiveRenderTargetHeight := NV2AInstance.RT_VERT shr 16;

  // NV2AInstance.CONTROL0 could tell us more, like if the render target is a YUV format,
  // if the depth buffer uses 'w' perspective, and if the depth-buffer is a floating point format.

  // TODO : Emulate SetRenderTarget
end;

//
// SetViewport
//

procedure TDxbxPushBufferState.TriggerSetViewport();
//var
//  ViewportTranslateX: FLOAT;
//  ViewportTranslateY: FLOAT;
//  ViewportTranslateZ: FLOAT;
//  ViewportScaleX: FLOAT;
//  ViewportScaleY: FLOAT;
//  ViewportScaleZ: FLOAT;
begin
//  // Interpret all related NV2A registers :
//  begin
//    ViewportTranslateX := NV2AInstance.VIEWPORT_TRANSLATE_X; // = Viewport.X * SuperSampleScaleX + ScreenSpaceOffsetX
//    ViewportTranslateY := NV2AInstance.VIEWPORT_TRANSLATE_Y; // = Viewport.Y * SuperSampleScaleY + ScreenSpaceOffsetY
//
//    // The following variables are 0.0 when fixed-function pipeline is active, otherwise they are calculated as :
//    begin
//      ViewportTranslateZ := NV2AInstance.VIEWPORT_TRANSLATE_Z; // = ZScale * Viewport.MinZ
//      // NV2AInstance.VIEWPORT_TRANSLATE_W ignored. Should always be 0.
//      ViewportScaleX := NV2AInstance.VIEWPORT_SCALE_X; // = 0.5 * Viewport.Width * SuperSampleScaleX
//      ViewportScaleY := NV2AInstance.VIEWPORT_SCALE_Y; // = -0.5 * Viewport.Height * SuperSampleScaleY
//      ViewportScaleZ := NV2AInstance.VIEWPORT_SCALE_Z; // = ZScale * (Viewport.MaxZ - Viewport.MinZ)
//      // NV2AInstance.VIEWPORT_SCALE_W ignored. Should always be 0.
//    end;
//  end;

  // TODO : Calculate the correct ViewPort values using the above determined variables :
  ViewPort.X := 0;
  ViewPort.Y := 0;
  ViewPort.Width := 640;
  ViewPort.Height := 480;
  // TODO : The following should behave differently under fixed-function when D3DRS_ZENABLE=D3DZB_USEW :
  ViewPort.MinZ := NV2AInstance.DEPTH_RANGE_NEAR / ZScale;
  ViewPort.MaxZ := NV2AInstance.DEPTH_RANGE_FAR / ZScale;

  // Place the native call :
  g_pD3DDevice.SetViewport(ViewPort);
end;

//
// SetTransform
//

function TDxbxPushBufferState.TriggerModelViewMatrix(pdwPushArguments: PDWORD; dwCount: DWORD): DWORD;
begin
  Assert(dwCount >= 16);
  Result := 16; // We handle only one matrix

  // The ModelView = D3DTS_WORLD * D3DTS_VIEW.
  // We cannot decompose these two matrixes, but if we keep the World view as a static Identity view,
  // we should be able to use the ModelView as native View matrix. [If we did it the other way around
  // (set View to identity and apply ModelView to World), many fog, lighting and other aspects wouldn't
  // be influenced on our native D3D device.]
  // TODO : Is this reasoning sound?
  g_pD3DDevice.SetTransform(D3DTS_VIEW, PD3DMatrix(pdwPushArguments));
end;

function TDxbxPushBufferState.TriggerCompositeMatrix(pdwPushArguments: PDWORD; dwCount: DWORD): DWORD;
begin
  Assert(dwCount >= 16);
  Result := 16; // We handle only one matrix

  // The ultimate goal is to recover D3DTS_PROJECTION here.
  //
  // if NV2AInstance.SKIN_MODE > 0 then
  //   CompositeView = ModelViewMatrix * ViewPortAdjustedProjectionViewMatrix
  //   TODO : Can we really infer the AdjustedProjectionViewMatrix by calculating CompositeView / ModelViewMatrix ?
  // else
  //   CompositeView = ViewPortAdjustedProjectionViewMatrix;
  //
  // Where
  //   ViewportAdjustedProjectionViewMatrix = D3DTS_PROJECTION * ViewportTransformMatrix
  //
  // Where (see http://msdn.microsoft.com/en-us/library/ee418867(v=VS.85).aspx)
  //   ViewportTransformMatrix =
  //     _11 0.0 0.0 0.0
  //     0.0 _22 0.0 0.0
  //     0.0 0.0 _33 0.0
  //     _41 _42 _43 _44
  //   (TODO : Is the '4' column a row instead?
  //
  //   _11 =    0.5 * SuperSampleScaleX * Viewport.Width
  //   _22 =   -0.5 * SuperSampleScaleY * Viewport.Height
  //   _33 = ZScale * (Viewport.MaxZ - Viewport.MinZ)
  //   _41 =    _11
  //   _42 =  - _22
  //   _43 = ZScale * Viewport.MinZ
  //   _44 =    1.0
  //
  // TODO : How on earth are we going to recover all this?!?!
  // g_pD3DDevice.SetTransform(D3DTS_PROJECTION, PD3DMatrix(pdwPushArguments));
end;

//
// SetVertexShaderConstant
//

function TDxbxPushBufferState.TriggerSetVertexShaderConstant(pdwPushArguments: PDWORD; dwCount: DWORD): DWORD;
begin
  // Since we always start at NV2A_VP_UPLOAD_CONST__0, never handle more than allowed :
  Assert(dwCount <= NV2A_VP_UPLOAD_CONST__SIZE);

  Result := dwCount;

  // Just set the constants right from the pushbuffer, as they come in batches and won't exceed the native bounds :
  g_pD3DDevice.SetVertexShaderConstant
  (
      // The VP_UPLOAD_CONST_ID GPU register is always pushed before the actual values, and contains the base Register for this batch :
      {Register=}NV2AInstance.VP_UPLOAD_CONST_ID,
      {pConstantData=}pdwPushArguments,
      {ConstantCount=}Result
  );
end;

//
// Draw[Indexed]Vertices[UP]
//

procedure TDxbxPushBufferState.TriggerDrawBeginEnd();
var
  NewPrimitiveType: X_D3DPRIMITIVETYPE;
begin
  NewPrimitiveType := X_D3DPRIMITIVETYPE(NV2AInstance.VERTEX_BEGIN_END);
  if (NewPrimitiveType = X_D3DPT_NONE) then
  begin
    DbgPrintf('  NV2A_VERTEX_BEGIN_END(DONE) -> Drawing');

    // D3DDevice_SetRenderTarget        : NV2A_RT_PITCH, NV2A_COLOR_OFFSET and NV2A_ZETA_OFFSET
    // D3DDevice_SetStreamSource (lazy) : NV2A_VTXFMT and NV2A_VTXBUF_ADDRESS
    // D3DDevice_DrawVertices           : NV2A_VB_VERTEX_BATCH (the batch is indicated by an Index and a Count in the active vertex buffer - as set by SetStreamSource)
    // D3DDevice_DrawIndexedVertices    : NV2A_VB_ELEMENT_U16 (and an optional closing NV2A_VB_ELEMENT_U32)  (the index is added into SetStreamSource)
    // D3DDevice_DrawVerticesUP         : NV2A_VERTEX_DATA
    // D3DDevice_DrawIndexedVerticesUP  : NV2A_VERTEX_DATA

    // Trigger the draw here (instead of in HandleVertexData, HandleIndex32 and/or HandleIndex16_16) :
    case PostponedDrawType of
      pdDrawVertices:
        PostponedDrawVertices;
      pdDrawIndexedVertices:
        PostponedDrawIndexedVertices;
      pdDrawVerticesUP:
        PostponedDrawVerticesUP;
      pdDrawIndexedVerticesUP:
        PostponedDrawIndexedVerticesUP;
    else
      DxbxKrnlCleanup('TriggerDrawBeginEnd encountered unknown draw mode!');
    end;
  end
  else
    DbgPrintf('  NV2A_VERTEX_BEGIN_END(PrimitiveType = 0x%.03x %s)', [Ord(NewPrimitiveType), X_D3DPRIMITIVETYPE2String(NewPrimitiveType)]);

  // Reset variables related to a single draw (others like VertexFormat and VertexAddress are persistent) :
  VertexCount := 0;
  VertexIndex := -1;
  PostponedDrawType := pdUndetermined;
  XBPrimitiveType := NewPrimitiveType;
end;

function TDxbxPushBufferState.HandleSetVertexShaderBatch(pdwPushArguments: PDWORD; dwCount: DWORD): Integer;
begin
  Result := dwCount;
  // Collect all slots in a separate array (as only part of it is present in the GPU registers) :
  memcpy(@VertexShaderSlots[NV2AInstance.VP_UPLOAD_FROM_ID], pdwPushArguments, dwCount * SizeOf(DWORD));
  // Batches are max 32 DWORDs, so just increase VP_UPLOAD_FROM_ID (the current slot index, max 136) :
  Inc(NV2AInstance.VP_UPLOAD_FROM_ID, Result);

  // TODO : When do we compile the shader?
end;

function TDxbxPushBufferState.HandleVertexData(pdwPushArguments: PDWORD; dwCount: DWORD): Integer;
// TODO : Postpone the draw in here to TriggerDrawBeginEnd, instead collect all vertices first.
var
  VPDesc: VertexPatchDesc;
  VertPatch: VertexPatcher;
  VertexCount: UINT;
begin
  Result := dwCount;
  PostponedDrawType := pdDrawVerticesUP;
  pVertexData := pdwPushArguments;

//  if Assigned(g_pD3DDevice) then
//    DxbxUpdateNativeD3DResources();

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

    memcpy({dest}pData, {src=}pVertexData, dwCount*4);

    pVertexBuffer.Unlock();
  end;
  *)

{$ifdef _DEBUG_TRACK_PB}
  if (bShowPB) then
  begin
    DbgPrintf('  NV2A_InlineVertexArray(...)');
    DbgPrintf('  dwVertexShader : 0x%08X', [dwVertexShader]);
  end;
{$endif}
  // render vertices
  if (dwVertexShader <> DWord(-1)) then
  begin
    VertexCount := (dwCount * sizeof(DWORD)) div dwStride;

    VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer

    VPDesc.PrimitiveType := XBPrimitiveType;
    VPDesc.dwVertexCount := VertexCount;
    VPDesc.pVertexStreamZeroData := pVertexData;
    VPDesc.uiVertexStreamZeroStride := dwStride;
    VPDesc.hVertexShader := dwVertexShader;

    VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

    {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

    DxbxDrawPrimitiveUP(VPDesc);

    VertPatch.Restore();
  end;
end;

procedure TDxbxPushBufferState._RenderIndexedVertices(dwCount: DWORD);
var
  VPDesc: VertexPatchDesc;
  VertPatch: VertexPatcher;
begin
  VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer

  VPDesc.PrimitiveType := XBPrimitiveType;
  VPDesc.dwVertexCount := dwCount;
  VPDesc.pVertexStreamZeroData := nil;
  VPDesc.uiVertexStreamZeroStride := 0;
  // TODO -oCXBX: Set the current shader and let the patcher handle it..
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

  g_pD3DDevice.SetIndices(IDirect3DIndexBuffer(pIndexBuffer){$IFNDEF DXBX_USE_D3D9}, 0{$ENDIF});

{$ifdef _DEBUG_TRACK_PB}
  if (not g_PBTrackDisable.exists(pdwOrigPushData)) then
{$endif}
  begin
    if (IsValidCurrentShader()) then
    begin
      g_pD3DDevice.DrawIndexedPrimitive
      (
        EmuXB2PC_D3DPrimitiveType(VPDesc.PrimitiveType),
        {$IFDEF DXBX_USE_D3D9}{BaseVertexIndex=}0,{$ENDIF}
        {MinVertexIndex=}0,
        {NumVertices=}g_EmuD3DActiveStreamSizes[0], // Note : ATI drivers are especially picky about this -
        // NumVertices should be the span of covered vertices in the active vertex buffer (TODO : Is stream 0 correct?)
        StartIndex,
        VPDesc.dwPrimitiveCount
      );
      StartIndex := 0;
    end;
  end;

  VertPatch.Restore();

  g_pD3DDevice.SetIndices(nil{$IFNDEF DXBX_USE_D3D9}, 0{$ENDIF});
end;

function TDxbxPushBufferState.HandleIndex32(pdwPushArguments: PDWORD; dwCount: DWORD): Integer;
// This command is (normally) used to end an index buffer with one last index.
// TODO : Collect this index together with the previous batch(es) and wait for the postponed draw.

  procedure _AssureIndexBuffer;
  var
    hRet: HRESULT;
  begin
    // TODO -oCXBX: depreciate maxIBSize after N milliseconds..then N milliseconds later drop down to new highest
    if (maxIBSize < (dwCount*SizeOf(WORD))) then
    begin
      maxIBSize := dwCount*SizeOf(WORD);

      if (pIndexBuffer <> nil) then
      begin
        IDirect3DIndexBuffer(pIndexBuffer)._Release();
        pIndexBuffer := nil; // Dxbx addition - nil out after decreasing reference count
      end;

      hRet := IDirect3DDevice_CreateIndexBuffer(g_pD3DDevice, maxIBSize, {Usage=}0, D3DFMT_INDEX16, D3DPOOL_MANAGED, PIDirect3DIndexBuffer(@pIndexBuffer));
    end
    else
    begin
      hRet := D3D_OK;
    end;

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('Unable to create index buffer for PushBuffer emulation');
  end;

var
  pwVal: PWORDs;
  pData: PWORDArray;
begin
  Result := dwCount;
  pwVal := PWORDs(pdwPushArguments);

{$ifdef _DEBUG_TRACK_PB}
  if (bShowPB) then
  begin
    DbgPrintf('  NV2A_FixLoop');
    DbgPrintf('');
    DbgPrintf('  Index Array Data...');

    if dwCount > 0 then // Dxbx addition, to prevent underflow
    for s := 0 to dwCount - 1 do
    begin
      if ((s mod 8) = 0) then printf(#13#10'  ');

      printf('  %.04X', [pwVal[s]]);
    end;

    printf(#13#10);
    DbgPrintf('');
  end;
{$endif}

  Inc(dwCount, NrCachedIndices);

  // perform rendering
  if (dwCount > 2) then
  begin
    if Assigned(g_pD3DDevice) then
      DxbxUpdateNativeD3DResources();

    _AssureIndexBuffer;

    // TODO : Instead of complicating things, we should probably render 1 primitive using
    // a small intermediate index buffer, and render the rest of the primitives with a normal
    // index buffer, that emulate in all other places via DxbxUpdateActiveIndexBuffer.

    // copy index data
    begin
      pData := nil;

      IDirect3DIndexBuffer(pIndexBuffer).Lock(0, dwCount*SizeOf(WORD), {out}TLockData(pData), 0);

      if NrCachedIndices > 0 then
      begin
        // If present, first insert previous two indices :
        memcpy({dest}pData, {src=}@pIBMem[0], NrCachedIndices*SizeOf(WORD));
        Inc(UIntPtr(pData), NrCachedIndices*SizeOf(WORD));
      end;

      memcpy({dest}pData, {src=}pwVal, dwCount*SizeOf(WORD));

      IDirect3DIndexBuffer(pIndexBuffer).Unlock();
    end;

    _RenderIndexedVertices(dwCount);
  end;
end;

function TDxbxPushBufferState.HandleIndex16_16(pdwPushArguments: PDWORD; dwCount: DWORD): Integer;
// TODO : Change this into collecting indexes and wait for the postponed Draw.
var
  pIndexData: PWORD;
begin
  Result := dwCount;

  pIndexData := PWORD(pdwPushArguments);
  dwCount := dwCount * 2; // Convert DWORD count to WORD count

{$ifdef _DEBUG_TRACK_PB}
  if (bShowPB) then
  begin
    DbgPrintf('  NV2A_VB_ELEMENT_U16(0x%.08X, %d)...', [pIndexData, dwCount]);
    DbgPrintf('');
    DbgPrintf('  Index Array Data...');

    pwVal := PWORDs(pIndexData);

    if dwCount > 0 then // Dxbx addition, to prevent underflow
    for s := 0 to dwCount - 1 do
    begin
      if ((s mod 8) = 0) then printf(#13#10'  ');

      printf('  %.04X', [pwVal[s]]);
    end;

    printf(#13#10);

    if Assigned(g_pD3DDevice) then
      DxbxUpdateNativeD3DResources();

    pActiveVB := nil;

    pVBData := nil;

    DxbxUpdateActiveVertexBufferStreams();

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

    DbgDumpMesh(pIndexData, dwCount);
  end;
{$endif}

  // perform rendering
  begin
    DxbxUpdateActiveIndexBuffer(pIndexData, dwCount, {out}StartIndex);

    // remember last 2 indices (will be used in HandleIndex32) :
    if (dwCount >= 2) then // TODO : Is 2 indices enough for all primitive types?
    begin
      pIBMem[0] := pIndexData[dwCount - 2];
      pIBMem[1] := pIndexData[dwCount - 1];
      NrCachedIndices := 2;
    end
    else
      NrCachedIndices := 0;

    _RenderIndexedVertices(dwCount);
  end;
end;

function TDxbxPushBufferState.SeenRecentMethod(Method: DWORD): Boolean;
// Checks if a method was recently handled.
begin
  Result := (PrevMethod[0] = Method) or (PrevMethod[1] = Method);
end;

procedure TDxbxPushBufferState.ExecutePushBufferRaw(pdwPushData, pdwPushDataEnd: PDWord);
var
  dwPushCommand: DWord;
  dwMethod: DWord;
  dwSubCh: DWord;
  dwCount: DWord;
  bNoInc: BOOL_;
  pdwPushArguments: PDWord;
  HandledCount: DWord;
  HandledBy: string;
  StepNr: Integer;
  LogStr: string;
begin
  if MayLog(lfUnit) then
    if UIntPtr(pdwPushData) <> UIntPtr(pdwPushDataEnd) then
      DbgPrintf('  NV2A run from 0x%.08X to 0x%.08X', [UIntPtr(pdwPushData), UIntPtr(pdwPushDataEnd)]);

  while pdwPushData <> pdwPushDataEnd do
  try
    LogStr := Format('  NV2A Get=$%.08X', [UIntPtr(pdwPushData)]);

    dwPushCommand := pdwPushData^;

    // Handle jumps and/or calls :
    if ((dwPushCommand and NV2A_JMP_FLAG) > 0)
    or ((dwPushCommand and NV2A_CALL_FLAG) > 0) then
    begin
      // Both 'jump' and 'call' just direct execution to the indicated address :
      pdwPushData := PDWORD(dwPushCommand and NV2A_ADDR_MASK);
      if MayLog(lfUnit) then
        DbgPrintf('%s Jump:0x%.08X', [LogStr, UIntPtr(pdwPushData)]);

      Continue;
    end;

    // Decode push buffer contents (inverse of D3DPUSH_ENCODE) :
    D3DPUSH_DECODE(dwPushCommand, {out}dwMethod, {out}dwSubCh, {out}dwCount, {out}bNoInc);

    // Append a counter (variable part via %d, count already formatted) :
    LogStr := LogStr + ' %2d/' + Format('%2d',[dwCount]) + ':';
    if dwSubCh > 0 then
      LogStr := LogStr + ' [SubCh:' + IntToStr(dwSubCh) + ']';

    if bNoInc then
      LogStr := LogStr + ' [NoInc]';

    // Skip method DWORD, remember the address of the arguments and skip over the arguments already :
    Inc(pdwPushData);
    pdwPushArguments := pdwPushData;
    Inc(pdwPushData, dwCount);

    // Initialize handled count & name to their default :
    HandledCount := 1;
    HandledBy := '';

    // Skip all commands not intended for channel 0 :
    if dwSubCh > 0 then
    begin
      HandledCount := dwCount;
      HandledBy := '*CHANNEL IGNORED*';
      dwMethod := dwMethod or $80000000; // Add a 'Channel-Ignore-bit' so that the following code doesn't do anything when we're not handling channel 0
    end;

    // Interpret GPU Instruction(s) :
    StepNr := 0;
    while dwCount > 0 do
    try
      Inc(StepNr);

      // Simulate writes to the NV2A instance registers :
      if dwMethod < SizeOf(NV2AInstance) then
      begin
        OldRegisterValue := NV2AInstance.Registers[dwMethod div 4]; // Remember previous value
        NV2AInstance.Registers[dwMethod div 4] := pdwPushArguments^; // Write new value
      end;

      // Note : The above statement covers all non-triggering data transfers (yeah!)
      // and makes them available as named variables too, since NV2AInstance is declared
      // with an overlay definition - which uses correct types where possible to reduce
      // the number of type-casts we need to do in here.

      if (g_pD3DDevice = nil) then
        HandledBy := '*NO DEVICE*' // Don't do anything if we have no device yet (should not occur anymore, but this helps spotting errors)
        // Note : A Delphi bug prevents us from using 'Continue' here, which costs us an indent level...
      else
      begin
        case dwMethod of
          0:
            HandledCount := dwCount;

          NV2A_NOP:
            HandledCount := dwCount; // Note : This case prevents a hit in DxbxXboxMethodToRenderState.

          // These can safely be ignored :
          NV2A_VTX_CACHE_INVALIDATE: HandledBy := 'D3DVertexBuffer_Lock';
          // TODO : Place more to-be-ignored methods here.

          NV2A_WRITE_SEMAPHORE_RELEASE:
          begin
            // TODO : What should we do with this data? Most probably it should go into a semaphore somewhere;
            // Perhaps this semaphore is in the 96 bytes that was first allocated with MmAllocateContiguousMemoryEx
            // (as we've seen the m_pGPUTime variable resides there too). Also telling : this command is pushed by SetFence!
          end;

          // Clear :
          NV2A_CLEAR_BUFFERS: // Gives clear flags, should trigger the clear
          begin
            TriggerClearBuffers();
            HandledBy := 'Clear';
          end;

          // Swap :
          NV2A_FLIP_STALL: // TODO : Should we trigger at NV2A_FLIP_INCREMENT_WRITE instead?
          begin
            DxbxPresent(nil, nil, 0, nil);
            HandledBy := 'Swap';
          end;

          // SetRenderTarget :
          NV2A_RT_FORMAT: // Set surface format
          begin
            TriggerSetRenderTarget();
            HandledBy := 'SetRenderTarget';
          end;

          // SetViewport :
          NV2A_DEPTH_RANGE_FAR:
          begin
            // Note : This is always the last method for SetViewport, so we use it as a trigger :
            TriggerSetViewport();
            HandledBy := 'SetViewport';
          end;

          // SetTransform :
          NV2A_MODELVIEW0_MATRIX__0:
          begin
            HandledCount := // Note : Disable this assignment-line to get more pushbuffer debug output
              TriggerModelViewMatrix(pdwPushArguments, dwCount);
            HandledBy := 'SetTransform';
          end;

          NV2A_COMPOSITE_MATRIX__0:
          begin
            HandledCount := // Note : Disable this assignment-line to get more pushbuffer debug output
              TriggerCompositeMatrix(pdwPushArguments, dwCount);
            HandledBy := 'SetTransform';
          end;


          // SetTexture / SwitchTexture :
          NV2A_TX_OFFSET__0, NV2A_TX_OFFSET__1, NV2A_TX_OFFSET__2, NV2A_TX_OFFSET__3:
            ; // TODO : Handle this

          NV2A_TX_FORMAT__0, NV2A_TX_FORMAT__1, NV2A_TX_FORMAT__2, NV2A_TX_FORMAT__3:
            ; // TODO : Handle this

          NV2A_TX_ENABLE__0, NV2A_TX_ENABLE__1, NV2A_TX_ENABLE__2, NV2A_TX_ENABLE__3:
            ; // TODO : Handle this

          // SetVertexData :
          NV2A_VERTEX_DATA2F__0..NV2A_VERTEX_DATA2F__15:
            ;
          NV2A_VERTEX_DATA2S__0..NV2A_VERTEX_DATA2S__15:
            ;
          NV2A_VERTEX_DATA4UB__0..NV2A_VERTEX_DATA4UB__15:
          begin
            DxbxSetVertexData({Register=}(dwMethod - NV2A_VERTEX_DATA4UB__0) div 4,
              ( pdwPushArguments^         and $ff) / High(Byte),
              ((pdwPushArguments^ shr  8) and $ff) / High(Byte),
              ((pdwPushArguments^ shr 16) and $ff) / High(Byte),
              ((pdwPushArguments^ shr 24) and $ff) / High(Byte));
            // TODO : When should we call XTL_EmuFlushIVB()?
            HandledBy := 'SetVertexData';
          end;

          NV2A_VERTEX_DATA4S__0..NV2A_VERTEX_DATA4S__15:
            ;
          NV2A_VERTEX_DATA4F__0..NV2A_VERTEX_DATA4F__15:
            ;
          NV2A_VERTEX_POS_4F_X: // (if Register = D3DVSDE_VERTEX)
            ;

          NV2A_VP_UPLOAD_CONST__0:
          begin
            HandledCount := TriggerSetVertexShaderConstant(pdwPushArguments, dwCount);
            HandledBy := 'SetVertexShaderConstant';
          end;

          // Draw[Indexed]Vertices[UP] :
          NV2A_VERTEX_BEGIN_END:
          begin
            TriggerDrawBeginEnd();
            HandledBy := 'Draw';
          end;

          // Data ultimately originating from SetVertexShader / SetStreamSource :
          NV2A_VTXFMT__0..NV2A_VTXFMT__15:
          begin
            RegisterVertexFormat({Slot=}(dwMethod - NV2A_VTXFMT__0) div 4, pdwPushArguments);
            HandledBy := 'VertexFormat';
          end;

          NV2A_VTXBUF_ADDRESS__0..NV2A_VTXBUF_ADDRESS__3:
          begin
            RegisterVertexAddress({Stage=}(dwMethod - NV2A_VTXBUF_ADDRESS__0) div 4, pdwPushArguments);
            HandledBy := 'VertexAddress';
          end;

          NV2A_VP_UPLOAD_INST__0:
          begin
            HandledCount := HandleSetVertexShaderBatch(pdwPushArguments, dwCount);
            HandledBy := 'SetVertexShader';
          end;

          NV2A_VB_VERTEX_BATCH:
          begin
            RegisterVertexBatch(pdwPushArguments);
            HandledBy := 'DrawVertices';
          end;

          NV2A_VERTEX_DATA:
          begin
            HandledCount := HandleVertexData(pdwPushArguments, dwCount);
            HandledBy := 'DrawVertices';
          end;

          NV2A_VB_ELEMENT_U16:
          begin
            HandledCount := HandleIndex16_16(pdwPushArguments, dwCount);
            HandledBy := 'DrawIndexedVertices';
          end;

          NV2A_VB_ELEMENT_U32:
          begin
            HandledCount := HandleIndex32(pdwPushArguments, dwCount);
            HandledBy := 'DrawIndexedVertices';
          end;

          // SetRenderState special cases (value conversions) :
          NV2A_FOG_COLOR:
          begin
            g_pD3DDevice.SetRenderState(D3DRS_FOGCOLOR, SwapRGB(pdwPushArguments^));
            HandledBy := 'SetRenderState';
          end;

          NV2A_CULL_FACE_ENABLE:
          begin
            if pdwPushArguments^ = DWORD(BOOL_FALSE) then
              g_pD3DDevice.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);

            HandledBy := 'SetRenderState';
          end;

          NV2A_CULL_FACE:
          begin
            g_pD3DDevice.SetRenderState(D3DRS_CULLMODE, D3DCULL_CW); // TODO : Use D3DCULL_CCW if D3DRS_FRONTFACE=D3DCULL_CW

            HandledBy := 'SetRenderState';
          end;

        else
          // SetRenderState, normal cases (direct value copy) :
          if TrySetRenderState(dwMethod, pdwPushArguments, dwCount) then
            HandledBy := 'SetRenderState'
          else
            // TODO : Add other generic states here, like texture stage states.
        end; // case
      end;

    finally
      if MayLog(lfUnit) then
      begin
        if HandledBy <> '' then HandledBy := '> ' + HandledBy;
        dwMethod := dwMethod and NV2A_METHOD_MASK; // Remove 'Channel-Ignore-bit'
        DbgPrintf(LogStr + ' Method=%.04X Data=%.08X %s %s', [StepNr, dwMethod, pdwPushArguments^, DxbxXboxMethodToString(dwMethod), HandledBy]);
      end;

      // Since some instructions use less arguments, we repeat this loop
      // for the next instruction so any leftover values are handled there :
      Inc(pdwPushArguments, HandledCount);
      Dec(dwCount, HandledCount);

      // Re-initialize handled count & name to their default, for the next command :
      HandledCount := 1;
      HandledBy := '';

      // The no-increment flag applies to method only :
      if not bNoInc then
      begin
        Inc(dwMethod, 4); // 1 method further

        // Remember the last two methods, in case we need to differentiate contexts (using SeenRecentMethod):
        PrevMethod[1] := PrevMethod[0];
        PrevMethod[0] := dwMethod;
      end;

      // Fake a read by the Nv2A, by moving the DMA 'Get' location
      // up to where the pushbuffer is executed, so that the BusyLoop
      // in CDevice.Init finishes cleanly :
      g_NV2ADMAChannel.Get := pdwPushData;
      // TODO : We should probably set g_NV2ADMAChannel.Put to the same value first?

      // We trigger the DMA semaphore by setting GPU time to CPU time - 2 :
      {D3DDevice.}m_pGpuTime^ := {D3DDevice.}m_pCpuTime^ - 2;

      // TODO : We should register vblank counts somewhere?
    end; // while dwCount > 0 try

  except
    DbgPrintf('PUSHBUFFER EXCEPTION!');
  end; // while pdwPushData <> pdwPushDataEnd do try

  // This line is to reset the GPU 'Get' pointer, so that busyloops will terminate :
  g_NV2ADMAChannel.Get := pdwPushData;
end;

var
  DxbxPushBufferState: PDxbxPushBufferState = nil;

procedure XTL_EmuExecutePushBufferRaw
(
    pdwPushData: PDWord;
    pdwPushDataEnd: PDWord
); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if DxbxPushBufferState = nil then
  begin
    // Initialize pushbuffer state only once :
    New(DxbxPushBufferState);
    ZeroMemory(DxbxPushBufferState, SizeOf(DxbxPushBufferState));

    DxbxPushBufferState.dwVertexShader := DWORD(-1);
    DxbxPushBufferState.dwStride := DWORD(-1);
    DxbxPushBufferState.XBPrimitiveType := X_D3DPT_INVALID;
  end;

  DxbxPushBufferState.ExecutePushBufferRaw(pdwPushData, pdwPushDataEnd)
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

  DxbxUpdateActiveVertexBufferStreams();

  // retrieve stream data
  g_pD3DDevice.GetStreamSource(
    0,
    @pActiveVB,
{$IFDEF DXBX_USE_D3D9}
    {out}uiOffsetInBytes,
{$ENDIF}
    {out}uiStride);

  sprintf(@szFileName[0], AnsiString(DxbxDebugFolder +'\DxbxMesh-0x%.08X.x'), [UIntPtr(pIndexData)]);
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

      if (maxIndex < x) then
        maxIndex := x;
    end;

    if (maxIndex > ((VBDesc.Size div uiStride) - 1)) then
      maxIndex := (VBDesc.Size div uiStride) - 1;

    fprintf(dbgVertices, 'xof 0303txt 0032'#13#10);
    fprintf(dbgVertices, ''#13#10);
    fprintf(dbgVertices, '//'#13#10);
    fprintf(dbgVertices, '//  Vertex Stream Data (0x%.08X)...'#13#10, [UIntPtr(pActiveVB)]);
    fprintf(dbgVertices, '//'#13#10);
    fprintf(dbgVertices, '//  Format : %d'#13#10, [Ord(VBDesc.Format)]);
    fprintf(dbgVertices, '//  Size   : %d bytes'#13#10, [VBDesc.Size]);
    fprintf(dbgVertices, '//  FVF    : 0x%.08X'#13#10, [VBDesc.FVF]);
    fprintf(dbgVertices, '//  iCount : %d'#13#10, [dwCount div 2]);
    fprintf(dbgVertices, '//'#13#10);
    fprintf(dbgVertices, ''#13#10);
    fprintf(dbgVertices, 'Frame SCENE_ROOT {'#13#10);
    fprintf(dbgVertices, ''#13#10);
    fprintf(dbgVertices, '  FrameTransformMatrix {'#13#10);
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
    fprintf(dbgVertices, '    }'#13#10);
    fprintf(dbgVertices, ''#13#10);
    fprintf(dbgVertices, '    Mesh {'#13#10);
    fprintf(dbgVertices, '      %d;'#13#10, [maxIndex + 1]);

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

