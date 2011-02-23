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

{$IFDEF DXBX_USE_OPENGL}
  {$UNDEF DXBX_USE_D3D}
  {$UNDEF DXBX_USE_D3D8}
  {$UNDEF DXBX_USE_D3D9}
{$ENDIF}

{.$define _DEBUG_TRACK_PB}

interface

uses
  // Delphi
  Windows
  , SysUtils
  , Classes
{$IFDEF DXBX_USE_OPENGL}
  , OpenGL1x
  , OpenGLTokens
{$ENDIF}
  // Jedi Win32API
  , JwaWinType
  // DirectX
{$IFDEF DXBX_USE_D3D8}
  , Direct3D8
{$ENDIF}
{$IFDEF DXBX_USE_D3D9}
  , Direct3D9
{$ENDIF}
  , D3DX8 // TD3DXColor
  // Dxbx
  , uTypes
  , uTime // DxbxTimer
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

var GPURegisterBase: PByte;

{$IFDEF _DEBUG_TRACK_PB}
procedure DbgDumpMesh(pIndexData: PWORD; dwCount: DWORD); {NOPATCH}
{$ENDIF}

function EmuThreadHandleNV2ADMA(lpVoid: LPVOID): DWORD; stdcall;

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
const NV2A_COUNT_MAX = (NV2A_COUNT_MASK shr NV2A_COUNT_SHIFT) - 1; // = 2047

const lfUnit = lfCxbx or lfPushBuffer;

const
  // Vertex shader header, mapping Xbox1 registers to the ARB syntax (original version by KingOfC) :
  DxbxVertexShaderHeader: AnsiString =
    '!!ARBvp1.0'#13#10 +
    'TEMP R0,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12;'#13#10 +
    'ADDRESS A0;'#13#10 +
    'ATTRIB v0 = vertex.attrib[0];'#13#10 +
    'ATTRIB v1 = vertex.attrib[1];'#13#10 +
    'ATTRIB v2 = vertex.attrib[2];'#13#10 +
    'ATTRIB v3 = vertex.attrib[3];'#13#10 +
    'ATTRIB v4 = vertex.attrib[4];'#13#10 +
    'ATTRIB v5 = vertex.attrib[5];'#13#10 +
    'ATTRIB v6 = vertex.attrib[6];'#13#10 +
    'ATTRIB v7 = vertex.attrib[7];'#13#10 +
    'ATTRIB v8 = vertex.attrib[8];'#13#10 +
    'ATTRIB v9 = vertex.attrib[9];'#13#10 +
    'ATTRIB v10 = vertex.attrib[10];'#13#10 +
    'ATTRIB v11 = vertex.attrib[11];'#13#10 +
    'ATTRIB v12 = vertex.attrib[12];'#13#10 +
    'ATTRIB v13 = vertex.attrib[13];'#13#10 +
    'ATTRIB v14 = vertex.attrib[14];'#13#10 +
    'ATTRIB v15 = vertex.attrib[15];'#13#10 +
    'OUTPUT oPos = result.position;'#13#10 +
    'OUTPUT oD0 = result.color.front.primary;'#13#10 +
    'OUTPUT oD1 = result.color.front.secondary;'#13#10 +
    'OUTPUT oB0 = result.color.back.primary;'#13#10 +
    'OUTPUT oB1 = result.color.back.secondary;'#13#10 +
    'OUTPUT oPts = result.pointsize;'#13#10 +
    'OUTPUT oFog = result.fogcoord;'#13#10 +
    'OUTPUT oT0 = result.texcoord[0];'#13#10 +
    'OUTPUT oT1 = result.texcoord[1];'#13#10 +
    'OUTPUT oT2 = result.texcoord[2];'#13#10 +
    'OUTPUT oT3 = result.texcoord[3];'#13#10 +
    // PatrickvL addition :
//    'PARAM c[] = { program.env[0..191] };'#13#10 // All constants in 1 array declaration, requires NV_gpu_program4
    'PARAM c0 = program.env[0];'#13#10 +
    'PARAM c1 = program.env[1];'#13#10 +
//    // TODO : Add PARAM declarations for all c[0-191]
    'PARAM c58 = program.env[58];'#13#10 + // X_D3DSCM_RESERVED_CONSTANT1 + X_D3DSCM_CORRECTION
    'PARAM c59 = program.env[59];'#13#10 + // X_D3DSCM_RESERVED_CONSTANT2 + X_D3DSCM_CORRECTION
    'PARAM c133 = program.env[133];'#13#10 +
    'PARAM c134 = program.env[134];'#13#10 +
    'PARAM c191 = program.env[191];'#13#10;

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
  TPostponedDrawType = (pdUndetermined, pdAlreadyDone, pdDrawVertices, pdDrawIndexedVertices);

var
  // Global(s)
  NV2AInstance: RNV2AInstance;
  dwMethod: DWord = 0;
  dwCount: DWord = 0;
  pdwPushArguments: PDWord = nil;
  HandledCount: DWord = 0;
  HandledBy: string = '';
  VertexIndex: INT = 0;
  VertexCount: UINT = 0;
  DxbxCurrentVertexStride: UINT = 0;
  XBPrimitiveType: X_D3DPRIMITIVETYPE;
  PostponedDrawType: TPostponedDrawType = pdUndetermined;
  VertexShaderSlots: array [0..D3DVS_XBOX_NR_ADDRESS_SLOTS-1] of DWORD;

  // Globals and controller :
  OldRegisterValue: DWORD = 0;
  PrevMethod: array [0..2-1] of DWORD = (0, 0);
  ZScale: FLOAT = 0.0;
//  function SeenRecentMethod(Method: DWORD): Boolean;
//  procedure ExecutePushBufferRaw(pdwPushData, pdwPushDataEnd: PDWord);

  // TODO : All below must be re-implemented :
  pVertexData: PVOID = nil;
  dwVertexShader: DWord = DWORD(-1);
  dwStride: DWord = DWORD(-1);
  NrCachedIndices: int = 0;
  StartIndex: UINT = 0;
  pIBMem: array [0..2-1] of WORD = (0, 0);
  maxIBSize: uint = 0;
  VertexOffset: uint = 0;
{$IFDEF DXBX_USE_D3D}
  Viewport: D3DVIEWPORT;
  pIndexBuffer: XTL_LPDIRECT3DINDEXBUFFER8; // = XTL_PIDirect3DIndexBuffer8
  pVertexBuffer: XTL_LPDIRECT3DVERTEXBUFFER8; // = XTL_PIDirect3DVertexBuffer8
{$ENDIF}
{$IFDEF DXBX_USE_OPENGL}
  g_EmuWindowsDC: HDC = 0;
  VertexProgramIDs: array [0..4-1] of GLuint = (0, 0, 0, 0);
  TextureIDs: array [0..X_D3DTS_STAGECOUNT-1] of GLuint = (0, 0, 0, 0);
{$ENDIF}

procedure DWORDSplit2(const aValue: DWORD; w1: Integer; out v1: Integer; w2: Integer; out v2: Integer);
begin
  {out}v1 := (aValue       ) and ((1 shl w1) - 1);
  {out}v2 := (aValue shr w1) and ((1 shl w2) - 1);
end;

{$IFDEF DXBX_USE_D3D}
function SwapRgb(color: D3DCOLOR): DWORD;
begin
  Result :=  (color and $ff00ff00)
         or ((color and $00ff0000) shr 16)
         or ((color and $000000ff) shl 16);
end;
{$ENDIF}

function SeenRecentMethod(Method: DWORD): Boolean;
// Checks if a method was recently handled.
begin
  Result := (PrevMethod[0] = Method) or (PrevMethod[1] = Method);
end;

{ TDxbxPushBufferState }

procedure ClearVariables;
begin
  ZeroMemory(@NV2AInstance, SizeOf(NV2AInstance));
  ZeroMemory(@VertexShaderSlots, SizeOf(VertexShaderSlots));
  dwVertexShader := DWORD(-1);
  dwStride := DWORD(-1);
  XBPrimitiveType := X_D3DPT_INVALID;
end;

function DxbxGetNV2AVertexFormatStride(Slot: Integer): uint;
begin
  Result := (NV2AInstance.VTXFMT[Slot] and NV2A_VTXFMT_STRIDE_MASK) shr NV2A_VTXFMT_STRIDE_SHIFT;
end;

function DxbxGetNV2AVertexFormatSize(Slot: Integer): uint;
begin
  Result := (NV2AInstance.VTXFMT[Slot] and NV2A_VTXFMT_SIZE_MASK) shr NV2A_VTXFMT_SIZE_SHIFT; // Size:1..4=1..4,7=3w?
end;

function DxbxGetNV2AVertexFormatType(Slot: Integer): uint;
begin
  Result := (NV2AInstance.VTXFMT[Slot] and NV2A_VTXFMT_TYPE_MASK); // Type:1=S1,2=F,4=UB_OGL,5=S32K,6=CMP?
end;

{$IFDEF DXBX_USE_OPENGL}

function NV2APrimitiveTypeToGL(Value: X_D3DPRIMITIVETYPE): DWORD;
begin
  Result := Ord(Value) - 1;
  (* The above is a faster version of this mapping :
  case Value of
     {1}X_D3DPT_POINTLIST     : Result := {0}GL_POINTS;
     {2}X_D3DPT_LINELIST      : Result := {1}GL_LINES;
     {3}X_D3DPT_LINELOOP      : Result := {2}GL_LINE_LOOP;
     {4}X_D3DPT_LINESTRIP     : Result := {3}GL_LINE_STRIP;
     {5}X_D3DPT_TRIANGLELIST  : Result := {4}GL_TRIANGLES;
     {6}X_D3DPT_TRIANGLESTRIP : Result := {5}GL_TRIANGLE_STRIP;
     {7}X_D3DPT_TRIANGLEFAN   : Result := {6}GL_TRIANGLE_FAN;
     {8}X_D3DPT_QUADLIST      : Result := {7}GL_QUADS;
     {9}X_D3DPT_QUADSTRIP     : Result := {8}GL_QUAD_STRIP;
    {10}X_D3DPT_POLYGON       : Result := {9}GL_POLYGON;
  end; *)
end;

function NV2AVertexFormatTypeToString(Value: DWORD): string;
begin
  case VALUE of
    {0}NV2A_VTXFMT_TYPE_COLORBYTE: Result := 'ColorByte';
    {1}NV2A_VTXFMT_TYPE_SHORT: Result := 'Short';
    {2}NV2A_VTXFMT_TYPE_FLOAT: Result := 'Float';
    {4}NV2A_VTXFMT_TYPE_UBYTE: Result := 'UByte';
    {5}NV2A_VTXFMT_TYPE_USHORT: Result := 'UShort';
//    6: ??
//  GL_BYTE = $1400;
//  GL_INT = $1404;
//  GL_UNSIGNED_INT = $1405;
//  GL_DOUBLE = $140A;
  else
    Result := '';
  end;
end;

function NV2AVertexFormatTypeToGL(Value: DWORD): DWORD;
begin
  case VALUE of
    {0}NV2A_VTXFMT_TYPE_COLORBYTE: Result := GL_UNSIGNED_BYTE; // Used for D3DCOLOR
    {1}NV2A_VTXFMT_TYPE_SHORT: Result := GL_SHORT;
    {2}NV2A_VTXFMT_TYPE_FLOAT: Result := GL_FLOAT;
    {4}NV2A_VTXFMT_TYPE_UBYTE: Result := GL_UNSIGNED_BYTE;
    {5}NV2A_VTXFMT_TYPE_USHORT: Result := GL_UNSIGNED_SHORT;
//    6: ??
//  GL_BYTE = $1400;
//  GL_INT = $1404;
//  GL_UNSIGNED_INT = $1405;
//  GL_DOUBLE = $140A;
  else
    DxbxKrnlCleanup('Unsupported Vertex format!');
    Result := GL_FALSE;
  end;
end;

{$ENDIF}

function BooleanToString(Value: Boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

function ColorBytesToString(Ptr: PBYTE; Count: uint = 1; NrPerGroup: uint = 4; Stride: uint = 0): string;
const
  ChannelPrefix: array [0..3] of string = ('B:', 'G:', 'R:', 'A:');
var
  i: uint;
begin
  Assert(NrPerGroup <= 4);
  Result := '{';
  i := 0;
  if Stride > 0 then Dec(Stride, NrPerGroup * SizeOf(Ptr^));
  while i < Count do
  begin
    Result := Result + ChannelPrefix[i mod NrPerGroup] + Format('%d', [Ptr^]);
    Inc(Ptr);
    Inc(i);
    if i < Count then
      if (i mod NrPerGroup) > 0 then
        Result := Result + ', '
      else
      begin
        Result := Result + '}{';
        Inc(UIntPtr(Ptr), Stride);
      end;
  end;

  Result := Result + '}';
end;

function ShortsToString(Ptr: PSHORT; Count: uint = 1; NrPerGroup: uint = 4; Stride: uint = 0): string;
var
  i: uint;
begin
  Result := '{';
  i := 0;
  if Stride > 0 then Dec(Stride, NrPerGroup * SizeOf(Ptr^));
  while i < Count do
  begin
    Result := Result + Format('%d', [Ptr^]);
    Inc(Ptr);
    Inc(i);
    if i < Count then
      if (i mod NrPerGroup) > 0 then
        Result := Result + ', '
      else
      begin
        Result := Result + '}{';
        Inc(UIntPtr(Ptr), Stride);
      end;
  end;

  Result := Result + '}';
end;

function FloatsToString(Ptr: PFLOAT; Count: uint = 1; NrPerGroup: uint = 4; Stride: uint = 0): string; overload;
var
  i: uint;
begin
  Result := '{';
  i := 0;
  if Stride > 0 then Dec(Stride, NrPerGroup * SizeOf(Ptr^));
  while i < Count do
  begin
    Result := Result + FloatToStr(Ptr^);
    Inc(Ptr);
    Inc(i);
    if i < Count then
      if (i mod NrPerGroup) > 0 then
        Result := Result + ', '
      else
      begin
        Result := Result + '}{';
        Inc(UIntPtr(Ptr), Stride);
      end;
  end;

  Result := Result + '}';
end;

function FloatsToString(Ptr: Pointer; Count: uint = 1; NrPerGroup: uint = 4; Stride: uint = 0): string; overload;
begin
  Result := FloatsToString(PFLOAT(Ptr), Count, NrPerGroup, Stride);
end;

type
  UBYTE = ShortInt;
  PUBYTE = ^UBYTE;

function UBytesToString(Ptr: PUBYTE; Count: uint = 1; NrPerGroup: uint = 4; Stride: uint = 0): string;
var
  i: uint;
begin
  Result := '{';
  i := 0;
  if Stride > 0 then Dec(Stride, NrPerGroup * SizeOf(Ptr^));
  while i < Count do
  begin
    Result := Result + Format('%d', [Ptr^]);
    Inc(Ptr);
    Inc(i);
    if i < Count then
      if (i mod NrPerGroup) > 0 then
        Result := Result + ', '
      else
      begin
        Result := Result + '}{';
        Inc(UIntPtr(Ptr), Stride);
      end;
  end;

  Result := Result + '}';
end;

function UShortsToString(Ptr: PUSHORT; Count: uint = 1; NrPerGroup: uint = 4; Stride: uint = 0): string;
var
  i: uint;
begin
  Result := '{';
  i := 0;
  if Stride > 0 then Dec(Stride, NrPerGroup * SizeOf(Ptr^));
  while i < Count do
  begin
    Result := Result + Format('%d', [Ptr^]);
    Inc(Ptr);
    Inc(i);
    if i < Count then
      if (i mod NrPerGroup) > 0 then
        Result := Result + ', '
      else
      begin
        Result := Result + '}{';
        Inc(UIntPtr(Ptr), Stride);
      end;
  end;

  Result := Result + '}';
end;

{$IFDEF DXBX_USE_OPENGL}
var
  // Since we'll never recieve just one index, our "doubling" method of index-memory
  // allocation won't need a special case for the initial (nil) state if we start at 1 :
  DxbxIndicesMaxWordCount: uint = 1;
  DxbxIndices: PWORD = nil;

procedure DxbxMakeIndexSpace(const NewWordCount: uint);
begin
  if NewWordCount < DxbxIndicesMaxWordCount then
    Exit;

  repeat
    DxbxIndicesMaxWordCount := DxbxIndicesMaxWordCount * 2;
  until DxbxIndicesMaxWordCount >= NewWordCount;

  // Reserve that space (keeping the old data intact) :
  ReallocMem(Pointer(DxbxIndices), DxbxIndicesMaxWordCount);
end;

var
  // Since we'll never recieve just one index, our "doubling" method of index-memory
  // allocation won't need a special case for the initial (nil) state if we start at 1 :
  DxbxVerticesSize: uint = 1;
  DxbxVertices: PBYTE = nil;
  DxbxCurrentVertices: PBYTE = nil; // Either nil, DxbxVertices or pdwPushArguments

procedure DxbxMakeVertexSpace(const NewSize: uint);
begin
  if NewSize < DxbxVerticesSize then
    Exit;

  repeat
    DxbxVerticesSize := DxbxVerticesSize * 2;
  until DxbxVerticesSize >= NewSize;

  // Reserve that space (keeping the old data intact) :
  ReallocMem(Pointer(DxbxIndices), NewSize);
end;

var
  VertexAttribSize: array[0..15] of uint;

function DxbxCalculateVertexStride(): uint;
var
  i: uint;
  NrElements: uint;
  VType: DWORD;
begin
  // Calculate the stride :
  Result := 0;
  for i := X_D3DVSDE_POSITION to X_D3DVSDE_TEXCOORD3 do
  begin
    NrElements := DxbxGetNV2AVertexFormatSize(i);
    if NrElements > 0 then
    begin
      VType := DxbxGetNV2AVertexFormatType(i);
      case VType of
        NV2A_VTXFMT_TYPE_COLORBYTE: VertexAttribSize[i] := NrElements * SizeOf(BYTE);
        NV2A_VTXFMT_TYPE_SHORT:     VertexAttribSize[i] := NrElements * SizeOf(SHORT);
        NV2A_VTXFMT_TYPE_FLOAT:     VertexAttribSize[i] := NrElements * SizeOf(FLOAT);
        NV2A_VTXFMT_TYPE_UBYTE:     VertexAttribSize[i] := NrElements * SizeOf(UBYTE);
        NV2A_VTXFMT_TYPE_USHORT:    VertexAttribSize[i] := NrElements * SizeOf(USHORT);
      else
        // ?
      end;

      Inc(Result, VertexAttribSize[i]);
    end;
  end;
end;

procedure DxbxSetupVertexPointers(InlinePointer: PBYTE = nil);
// Parse all NV2A_VTXBUF_ADDRESS and NV2A_VTXFMT data here, so that we know how where the vertex data is, and what format it has.
// (If InlinePointer is given, NV2A_VTXBUF_ADDRESS is bypassed, since the vertex data is either tightly packed in the pushbuffer
// itself, or copied to a buffer of ourselves.)
var
  i: uint;
  NrElements: uint;
  Stride: uint;
  VType: DWORD;
  VertexAttribPointer: PBYTE;
begin
  // Since DrawBeginEnd has no choice but to start a glBegin() block,
  // we must first leave that here, as we're not going to draw immediate :
  glEnd();

  // Make sure we have no VBO active :
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glEnable(GL_VERTEX_PROGRAM_ARB);

  // TODO : Implement an alternative route for the following code,
  // if the current OpenGL context doesn't support this :
  glBindProgramARB(GL_VERTEX_PROGRAM_ARB, VertexProgramIDs[1]);

  // If we point directly into a buffer buffer, we already calculated
  // the address of the first attribute and the accompanying stride :
  VertexAttribPointer := InlinePointer;
  Stride := DxbxCurrentVertexStride;

  // glEnableClientState(GL_VERTEX_ARRAY); // This would be needed for the older glVertexPointer API
  for i := X_D3DVSDE_POSITION to X_D3DVSDE_TEXCOORD3 do
  begin
    NrElements := DxbxGetNV2AVertexFormatSize(i);
    if NrElements > 0 then
    begin
      glEnableVertexAttribArray(i);
      VType := DxbxGetNV2AVertexFormatType(i);

      // If we don't point directly into the pushbuffer
      if InlinePointer = nil then
      begin
        // read the address of this attribute's data from the corresponding NV2A register :
        VertexAttribPointer := NV2AInstance.VTXBUF_ADDRESS[i];
        // Because of reading from somewhere else, the stride has to come from a register too :
        Stride := DxbxGetNV2AVertexFormatStride(i);
      end;

      glVertexAttribPointer(
        {Index=}i,
        NrElements,
        {Type=}NV2AVertexFormatTypeToGL(VType),
        {Normalized=}(VType <> NV2A_VTXFMT_TYPE_FLOAT) or (i >= X_D3DVSDE_TEXCOORD0), // Note : Texture coordinates are not normalized, but what about others?
        Stride,
        VertexAttribPointer);

      // Only log coordinates when there are just a few vertices to draw :
      if VertexCount <= 4 then
      begin
        // TODO : Prepend slot-string here too
        HandledBy := HandledBy + ' ' + NV2AVertexFormatTypeToString(DxbxGetNV2AVertexFormatType(i)) + ':';
        case DxbxGetNV2AVertexFormatType(i) of
          NV2A_VTXFMT_TYPE_COLORBYTE:
            HandledBy := HandledBy + ColorBytesToString(VertexAttribPointer, NrElements*VertexCount, NrElements, Stride);
          NV2A_VTXFMT_TYPE_SHORT:
            HandledBy := HandledBy + ShortsToString(PSHORT(VertexAttribPointer), NrElements*VertexCount, NrElements, Stride);
          NV2A_VTXFMT_TYPE_FLOAT:
            HandledBy := HandledBy + FloatsToString(PFLOAT(VertexAttribPointer), NrElements*VertexCount, NrElements, Stride);
          NV2A_VTXFMT_TYPE_UBYTE:
            HandledBy := HandledBy + UBytesToString(PUBYTE(VertexAttribPointer), NrElements*VertexCount, NrElements, Stride);
          NV2A_VTXFMT_TYPE_USHORT:
            HandledBy := HandledBy + UShortsToString(PUSHORT(VertexAttribPointer), NrElements*VertexCount, NrElements, Stride);
        end;
      end;

      // If we point directly into the pushbuffer, step to the next address (so the following attribute will be there) :
      if InlinePointer <> nil then
        Inc(VertexAttribPointer, VertexAttribSize[i]);
    end
    else
    begin
      glDisableVertexAttribArray(i);
      // Some shaders use vertex attributes without an input stream
      // (like the "Compressed Vertices" tutorial which uses v9);
      // So arrange for default input here :
      //glVertexAttrib4fv(i, @NV2AInstance.VERTEX_DATA4F[0]);
    end;
  end;
end;


procedure DxbxFinishVertexPointers();
begin
//  glDisableClientState(GL_VERTEX_ARRAY);
//  glDisableClientState(GL_COLOR_ARRAY);
  glDisable(GL_VERTEX_PROGRAM_ARB);
end;

const
  NV2AMagnificationFilter: array [1..2] of GLenum =
  (
    {NV2A_TX_FILTER_MAGNIFY_NEAREST:} GL_NEAREST,
    {NV2A_TX_FILTER_MAGNIFY_LINEAR:}  GL_LINEAR
  );

  NV2AMinificationFilter: array [1..6] of GLenum =
  (
    {NV2A_TX_FILTER_MINIFY_NEAREST:}                GL_NEAREST,
    {NV2A_TX_FILTER_MINIFY_LINEAR:}                 GL_LINEAR,
    {NV2A_TX_FILTER_MINIFY_NEAREST_MIPMAP_NEAREST:} GL_NEAREST_MIPMAP_NEAREST,
    {NV2A_TX_FILTER_MINIFY_LINEAR_MIPMAP_NEAREST:}  GL_LINEAR_MIPMAP_NEAREST,
    {NV2A_TX_FILTER_MINIFY_NEAREST_MIPMAP_LINEAR:}  GL_NEAREST_MIPMAP_LINEAR,
    {NV2A_TX_FILTER_MINIFY_LINEAR_MIPMAP_LINEAR:}   GL_LINEAR_MIPMAP_LINEAR
  );

  NV2ATexCoordWrapToGL: array [NV2A_TX_WRAP_S_REPEAT..NV2A_TX_WRAP_S_CLAMP] of GLenum =
  (
    {NV2A_TX_WRAP_S_REPEAT:}          GL_REPEAT,          // = X_D3DTADDRESS_WRAP
    {NV2A_TX_WRAP_S_MIRRORED_REPEAT:} GL_MIRRORED_REPEAT, // = X_D3DTADDRESS_MIRROR
    {NV2A_TX_WRAP_S_CLAMP_TO_EDGE:}   GL_CLAMP_TO_EDGE,   // = X_D3DTADDRESS_CLAMPTOEDGE
    {NV2A_TX_WRAP_S_CLAMP_TO_BORDER:} GL_CLAMP_TO_BORDER, // = X_D3DTADDRESS_BORDER
    {NV2A_TX_WRAP_S_CLAMP:}           GL_CLAMP            // = X_D3DTADDRESS_CLAMP
  );


procedure DxbxUpdateTextures();
var
  Stage: int;
  DxbxPixelJar: RDxbxDecodedPixelContainer;
  GLTextureTarget: GLenum;
  GLInternalFormat, GLFormat, GLType: DWORD;
  Pixels: PBYTE;
  Tmp: DWORD;
  NrFaces, Face, Level, NrSlices, Slice: DWORD;
  dwMipWidth, dwMipHeight, dwMipPitch: DWORD;
begin
  glDisable(GL_TEXTURE_1D);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_TEXTURE_3D);
  glDisable(GL_TEXTURE_CUBE_MAP);

  for Stage := 0 to X_D3DTS_STAGECOUNT - 1 do
  begin
    // Skip textures that aren't enabled :
    if (NV2AInstance.TX_OFFSET[Stage].TX_ENABLE and NV2A_TX_ENABLE_ENABLE) = 0 then
      Continue;

    glShadeModel(GL_SMOOTH); // TODO : Where to put this?
    glActiveTexture(GL_TEXTURE0 + Stage);

    DxbxGetFormatRelatedVariables(NV2AInstance.TX_OFFSET[Stage].TX_FORMAT, NV2AInstance.TX_OFFSET[Stage].TX_NPOT_SIZE,
      {out}DxbxPixelJar);

    if DxbxPixelJar.bIsCubeMap then GLTextureTarget := GL_TEXTURE_CUBE_MAP
    else if DxbxPixelJar.bIs3D then GLTextureTarget := GL_TEXTURE_3D
    else                            GLTextureTarget := GL_TEXTURE_2D;

    // TODO : The following generation of a TextureID should only be done for
    // new textures (indicated by changes in attributes and/or contents).
    // (Older textures should also be evited...)
    glGenTextures(1, @TextureIDs[Stage]);
    glBindTexture(GLTextureTarget, TextureIDs[Stage]);

    // Set magnification filter :
    Tmp := ((NV2AInstance.TX_OFFSET[Stage].TX_FILTER and NV2A_TX_FILTER_MAGNIFY_MASK) shr NV2A_TX_FILTER_MAGNIFY_SHIFT);
    if Tmp in [1..2] then
      glTexParameteri(GLTextureTarget, GL_TEXTURE_MAG_FILTER, NV2AMagnificationFilter[Tmp]);

    // Set minification filter :
    Tmp := ((NV2AInstance.TX_OFFSET[Stage].TX_FILTER and NV2A_TX_FILTER_MINIFY_MASK) shr NV2A_TX_FILTER_MINIFY_SHIFT);
    if Tmp in [1..6] then
      // TODO : Do any of the _MIPMAP_ values indicate mipmapping? What if there's only one level?
      glTexParameteri(GLTextureTarget, GL_TEXTURE_MIN_FILTER, NV2AMinificationFilter[Tmp]);

    // Set texture coordinate wrapping :
    Tmp := (NV2AInstance.TX_OFFSET[Stage].TX_WRAP and NV2A_TX_WRAP_S_MASK) shr NV2A_TX_WRAP_S_SHIFT;
    if Tmp in [NV2A_TX_WRAP_S_REPEAT..NV2A_TX_WRAP_S_CLAMP] then
      glTexParameteri(GLTextureTarget, GL_TEXTURE_WRAP_S, NV2ATexCoordWrapToGL[Tmp]);

    Tmp := (NV2AInstance.TX_OFFSET[Stage].TX_WRAP and NV2A_TX_WRAP_T_MASK) shr NV2A_TX_WRAP_T_SHIFT;
    if Tmp in [NV2A_TX_WRAP_S_REPEAT..NV2A_TX_WRAP_S_CLAMP] then
      glTexParameteri(GLTextureTarget, GL_TEXTURE_WRAP_T, NV2ATexCoordWrapToGL[Tmp]);

    Tmp := (NV2AInstance.TX_OFFSET[Stage].TX_WRAP and NV2A_TX_WRAP_R_MASK) shr NV2A_TX_WRAP_R_SHIFT;
    if Tmp in [NV2A_TX_WRAP_S_REPEAT..NV2A_TX_WRAP_S_CLAMP] then
      glTexParameteri(GLTextureTarget, GL_TEXTURE_WRAP_R, NV2ATexCoordWrapToGL[Tmp]);

    // TODO : Determine the following variables, based on DxbxPixelJar.X_Format (probably use a lookup table like WineD3D) :
    GLInternalFormat := 0; { Specifies the number of color components in the texture. Must be 1, 2, 3, or 4,
      or one of the following symbolic constants: GL_ALPHA, GL_ALPHA4, GL_ALPHA8, GL_ALPHA12, GL_ALPHA16,
      GL_COMPRESSED_ALPHA, GL_COMPRESSED_LUMINANCE, GL_COMPRESSED_LUMINANCE_ALPHA, GL_COMPRESSED_INTENSITY,
      GL_COMPRESSED_RGB, GL_COMPRESSED_RGBA, GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT16, GL_DEPTH_COMPONENT24,
      GL_DEPTH_COMPONENT32, GL_LUMINANCE, GL_LUMINANCE4, GL_LUMINANCE8, GL_LUMINANCE12, GL_LUMINANCE16,
      GL_LUMINANCE_ALPHA, GL_LUMINANCE4_ALPHA4, GL_LUMINANCE6_ALPHA2, GL_LUMINANCE8_ALPHA8, GL_LUMINANCE12_ALPHA4,
      GL_LUMINANCE12_ALPHA12, GL_LUMINANCE16_ALPHA16, GL_INTENSITY, GL_INTENSITY4, GL_INTENSITY8, GL_INTENSITY12,
      GL_INTENSITY16, GL_R3_G3_B2, GL_RGB, GL_RGB4, GL_RGB5, GL_RGB8, GL_RGB10, GL_RGB12, GL_RGB16, GL_RGBA, GL_RGBA2,
      GL_RGBA4, GL_RGB5_A1, GL_RGBA8, GL_RGB10_A2, GL_RGBA12, GL_RGBA16, GL_SLUMINANCE, GL_SLUMINANCE8, GL_SLUMINANCE_ALPHA,
      GL_SLUMINANCE8_ALPHA8, GL_SRGB, GL_SRGB8, GL_SRGB_ALPHA, or GL_SRGB8_ALPHA8. }

    // TODO : Convert unsupported texture formats

    GLFormat := 0; { Specifies the format of the pixel data. The following symbolic values are accepted:
      GL_COLOR_INDEX, GL_RED, GL_GREEN, GL_BLUE, GL_ALPHA, GL_RGB, GL_BGR, GL_RGBA, GL_BGRA, GL_LUMINANCE, and GL_LUMINANCE_ALPHA. }

    GLType := 0; { Specifies the data type of the pixel data. The following symbolic values are accepted:
      GL_UNSIGNED_BYTE, GL_BYTE, GL_BITMAP, GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT,
      GL_UNSIGNED_BYTE_3_3_2, GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5, GL_UNSIGNED_SHORT_5_6_5_REV,
      GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV, GL_UNSIGNED_SHORT_5_5_5_1, GL_UNSIGNED_SHORT_1_5_5_5_REV,
      GL_UNSIGNED_INT_8_8_8_8, GL_UNSIGNED_INT_8_8_8_8_REV, GL_UNSIGNED_INT_10_10_10_2, and GL_UNSIGNED_INT_2_10_10_10_REV }

    glEnable(GLTextureTarget); // TODO : Where to put this?

    if DxbxPixelJar.bIsCubeMap then
    begin
      NrFaces := 6;
      // For cubemaps, we have to call glTexImage2D with GL_TEXTURE_CUBE_MAP_POSITIVE_X and up :
      GLTextureTarget := GL_TEXTURE_CUBE_MAP_POSITIVE_X;
    end
    else
      NrFaces := 1;

    // This outer loop walks over all faces (6 for CubeMaps, just 1 for anything else) :
    for Face := 0 to NrFaces - 1 do
    begin
      dwMipWidth := DxbxPixelJar.dwWidth;
      dwMipHeight := DxbxPixelJar.dwHeight;
      dwMipPitch := DxbxPixelJar.dwRowPitch;

      // This 2nd loop iterates through all mipmap levels :
      for Level := 0 to DxbxPixelJar.dwMipMapLevels - 1 do
      begin

        // This inner loop walks over all slices (1 for anything but 3D textures - those
        // use the correct amount for the current mipmap level, but never go below 1) :
        NrSlices := DxbxPixelJar.MipMapSlices[Level];
        for Slice := 0 to NrSlices - 1 do
        begin
          // Determine the Xbox data pointer and step it to the correct mipmap, face and/or slice :
          Pixels := NV2AInstance.TX_OFFSET[Stage].TX_OFFSET; // TODO : Remove write-combined memory-mask where needed
          Inc(Pixels, DxbxPixelJar.MipMapOffsets[Level]);
          if DxbxPixelJar.bIsCubeMap then
            Inc(Pixels, DxbxPixelJar.dwFacePitch * Face)
          else
            if DxbxPixelJar.bIs3D then
              // Use the Xbox slice pitch (from the current level) to step to each slice in src :
              Inc(Pixels, DxbxPixelJar.SlicePitches[Level] * Slice);

          // TODO : Deswizzle, just like in DxbxUpdateNativePixelContainer (from where most of this code was taken)
          // Maybe even pallete>rgb conversion. In both cases, dwMipPitch will be needed.

          // Set the texture in OpenGL :
          if DxbxPixelJar.bIs3D then
            glTexImage3D(GLTextureTarget, {MipMap=}Level, GLInternalFormat,
              dwMipWidth, dwMipHeight, {Depth=}Slice, Ord(DxbxPixelJar.bIsBorderSource),
              GLFormat, GLType, Pixels)
          else
            glTexImage2D(GLTextureTarget + Face, {MipMap=}Level, GLInternalFormat,
              dwMipWidth, dwMipHeight, Ord(DxbxPixelJar.bIsBorderSource),
              GLFormat, GLType, Pixels);

        end; // for slices

        // Step to next mipmap level (but never go below minimum) :
        if dwMipWidth > DxbxPixelJar.dwMinXYValue then
        begin
          dwMipWidth := dwMipWidth div 2;
          dwMipPitch := dwMipPitch div 2;
        end;

        if dwMipHeight > DxbxPixelJar.dwMinXYValue then
          dwMipHeight := dwMipHeight div 2;
      end; // for mipmap levels
    end; // for faces

//    glClientActiveTexture(GL_TEXTURE0 + Stage);
  end;
end;
{$ENDIF}

procedure PostponedDrawVertices;
//  VPDesc: VertexPatchDesc;
//  VertPatch: VertexPatcher;
begin
  HandledBy := HandledBy + Format(' DrawPrimitive(VertexIndex=%d, VertexCount=%d)', [VertexIndex, VertexCount]);

{$IFDEF DXBX_USE_OPENGL}
  DxbxSetupVertexPointers(DxbxCurrentVertices);
  try
    glDrawArrays(NV2APrimitiveTypeToGL(XBPrimitiveType), VertexIndex, VertexCount);
  finally
    DxbxCurrentVertices := nil;
    DxbxCurrentVertexStride := 0;
    DxbxFinishVertexPointers();
  end;
{$ENDIF}

// test code :
//  // DxbxDrawPrimitiveUP ?
//  g_pD3DDevice.DrawPrimitive
//  (
//      EmuXB2PC_D3DPrimitiveType(XBPrimitiveType),
//      {StartVertex=}0, // VertexIndex ?
//      EmuD3DVertex2PrimitiveCount(XBPrimitiveType, VertexCount)
//  );

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

procedure PostponedDrawIndexedVertices;
begin
{$IFDEF DXBX_USE_OPENGL}
  DxbxSetupVertexPointers();
  try
    glDrawElements(NV2APrimitiveTypeToGL(XBPrimitiveType), VertexCount, GL_UNSIGNED_SHORT, DxbxIndices);
  finally
    DxbxFinishVertexPointers();
  end;
{$ENDIF}
end;

function TrySetRenderState: Boolean;
var
  XRenderState: X_D3DRenderStateType;
begin
  // See if this method is actually a render state :
  XRenderState := DxbxXboxMethodToRenderState(dwMethod);
  Result := XRenderState in [X_D3DRS_FIRST..X_D3DRS_LAST];
  if Result then
    DxbxSetRenderStateInternal('  NV2A SetRenderState', XRenderState, pdwPushArguments^);
end;

function IsEngineFixedFunctionPipeline(): Boolean;
// True=FVF, False=custom vertex shader
begin
  // Fixed function happens when there's no user-supplied vertex-program active :
  Result := (NV2AInstance.ENGINE and NV2A_ENGINE_VP) = 0;
end;

function IsEngineVertexProgram(): Boolean;
// True=User defined vertex shader program is active
begin
  Result := (NV2AInstance.ENGINE and NV2A_ENGINE_VP) > 0;
end;

function NextGPUCommand(): DWORD;
begin
  // Read the DWORD after the current argument data, and mask out the GPU method,
  // so that when necessary we can peek at the next command (hopefully always present) :
  Result := ((pdwPushArguments + dwCount)^ and NV2A_METHOD_MASK) {shr NV2A_METHOD_SHIFT};
end;

procedure EmuNV2A_NOP(); begin {HandledBy := 'nop'; }HandledCount := dwCount; end;
procedure EmuNV2A_CDevice_Init(); begin HandledBy := '_CDevice_Init'; end;
procedure EmuNV2A_VertexCacheInvalidate(); begin HandledBy := 'D3DVertexBuffer_Lock'; end;

//
// Clear
//

procedure EmuNV2A_ClearBuffers();
var
  ClearRect: RECT;
  ClearDepthValue: DWORD;
  PCFlags: DWORD;
  ClearZ: FLOAT;
  ClearStencil: DWORD;
{$IFDEF DXBX_USE_OPENGL}
  XColor: TD3DXColor;
{$ENDIF}
begin
  // Parse the GPU registers that are associated with this clear :
  begin
    // Reconstruct the clear rectangle :
    DWORDSplit2(NV2AInstance.CLEAR_RECT_HORIZONTAL, 16, {out}ClearRect.Left, 16, {out}ClearRect.Right);
    DWORDSplit2(NV2AInstance.CLEAR_RECT_VERTICAL, 16, {out}ClearRect.Top, 16, {out}ClearRect.Bottom);
    // Decode the Z and Stencil values :
    ClearDepthValue := NV2AInstance.CLEAR_DEPTH_VALUE;
    ClearZ := (ClearDepthValue shr 8) / ZScale;
    ClearStencil := ClearDepthValue and $FF;
  end;

  HandledBy := Format('ClearRect(%d, %d, %d, %d) ClearColor=%s ClearZ=%f ClearStencil=%d',
    [ClearRect.Left, ClearRect.Top, ClearRect.Right, ClearRect.Bottom,
    ColorBytesToString(@NV2AInstance.CLEAR_VALUE, 4),
    ClearZ, ClearStencil]);


{$IFDEF DXBX_USE_D3D}
  // Convert the clear-flags from Xbox to PC, after which we'll remove the invalid flags :
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

  // Since we filter the flags, make sure there are some left (else, clear isn't necessary) :
  if PCFlags > 0 then
  begin
    // Before clearing, make sure we have the correct render target :
//    DxbxUpdateActiveRenderTarget(); // TODO : Or should we have to call DxbxUpdateNativeD3DResources ?

    g_pD3DDevice.Clear(1, @ClearRect, PCFlags, NV2AInstance.CLEAR_VALUE, {Single}(ClearZ), ClearStencil);
  end;
{$ENDIF}

{$IFDEF DXBX_USE_OPENGL}
  glPushAttrib(GL_SCISSOR_BIT or GL_COLOR_BUFFER_BIT); // TODO : Do we need to push and GL_DEPTH_BUFFER_BIT and GL_STENCIL_BUFFER_BIT too?
    // Tell OpenGL the rectangle that must be cleared :
    glEnable(GL_SCISSOR_TEST);
    glScissor(ClearRect.Left, ClearRect.Top, ClearRect.Right - ClearRect.Left + 1, ClearRect.Bottom - ClearRect.Top + 1);
    // Make sure only the selected color component are cleared :
    glColorMask(
      (NV2AInstance.CLEAR_BUFFERS and NV2A_CLEAR_BUFFERS_COLOR_R) > 0,
      (NV2AInstance.CLEAR_BUFFERS and NV2A_CLEAR_BUFFERS_COLOR_G) > 0,
      (NV2AInstance.CLEAR_BUFFERS and NV2A_CLEAR_BUFFERS_COLOR_B) > 0,
      (NV2AInstance.CLEAR_BUFFERS and NV2A_CLEAR_BUFFERS_COLOR_A) > 0);
    // Set the Color, Depth and Stencil values to be used by the clear :
    XColor := D3DXColorFromDWord(NV2AInstance.CLEAR_VALUE);
    glClearColor(XColor.r, XColor.g, XColor.b, XColor.a);
    glClearDepth(ClearZ);
    glClearStencil(ClearStencil);
    // Determine the OpenGL clear flags :
    PCFlags := 0;
    if (NV2AInstance.CLEAR_BUFFERS and NV2A_CLEAR_BUFFERS_COLOR_ALL) > 0 then PCFlags := PCFlags or GL_COLOR_BUFFER_BIT;
    if (NV2AInstance.CLEAR_BUFFERS and NV2A_CLEAR_BUFFERS_STENCIL) > 0 then PCFlags := PCFlags or GL_STENCIL_BUFFER_BIT;
    if (NV2AInstance.CLEAR_BUFFERS and NV2A_CLEAR_BUFFERS_DEPTH) > 0 then PCFlags := PCFlags or GL_DEPTH_BUFFER_BIT;
    // Enque the clear :
    glClear(PCFlags);
    // TODO : What happens if a component (like stencil) is not available ?
  glPopAttrib();
{$ENDIF}
end;

//
// Swap
//

procedure EmuNV2A_FlipStall();
begin
  HandledBy := 'Swap';
{$IFDEF DXBX_USE_D3D}
  DxbxPresent(nil, nil, 0, nil);
{$ENDIF}
{$IFDEF DXBX_USE_OPENGL}
  SwapBuffers(g_EmuWindowsDC); // TODO : Use glFlush() when single-buffered?
{$ENDIF}
end;

procedure EmuNV2A_SetRenderTarget();
//var
//  SurfaceFormat: X_D3DFORMAT;
//  ActiveRenderTargetData: UIntPtr;
//  ActiveRenderTargetWidth: DWORD;
//  ActiveRenderTargetHeight: DWORD;
//  ActiveDepthStencilData: UIntPtr;
begin
  HandledBy := 'SetRenderTarget';

  // Always calculate ZScale and other factors like SuperSampleScaleX,
  // based on NV2AInstance.RT_FORMAT (our trigger) :

  ZScale := (1 shl 24) - 1; // TODO : Calculate real Z scale based on active depth buffer format (D24S8, D16, etc)
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

procedure EmuNV2A_SetRenderTargetSurface();
begin
  HandledBy := 'SetRenderTarget(Surface)';
end;

//
// ViewportOffset
//

procedure EmuNV2A_ViewportOffset();
begin
  Assert(dwCount = 4);
  HandledCount := 4;
  HandledBy := 'ViewportOffset(' + FloatsToString(pdwPushArguments, HandledCount) + ')';
{$IFDEF DXBX_USE_OPENGL}
  glProgramEnvParameter4fvARB(
    {target=}GL_VERTEX_PROGRAM_ARB,
    {index=}X_D3DSCM_RESERVED_CONSTANT2 + X_D3DSCM_CORRECTION, // C[-37] > c59
    {params=}PGLfloat(pdwPushArguments)
  );
{$ENDIF}
end;

//
// ViewportScale
//

procedure EmuNV2A_ViewportScale();
begin
  Assert(dwCount = 4);
  HandledCount := 4;
  HandledBy := 'ViewportScale(' + FloatsToString(pdwPushArguments, HandledCount) + ')';
{$IFDEF DXBX_USE_OPENGL}
  glProgramEnvParameter4fvARB(
    {target=}GL_VERTEX_PROGRAM_ARB,
    {index=}X_D3DSCM_RESERVED_CONSTANT1 + X_D3DSCM_CORRECTION, // C[-38] > c58
    {params=}PGLfloat(pdwPushArguments)
  );
{$ENDIF}
end;

procedure EmuNV2A_SetRenderState();
// Temporary function for generic logging. Each render-state needs specific emulation.
var
  XboxRenderState: X_D3DRenderStateType;
begin
  XboxRenderState := DxbxXboxMethodToRenderState(dwMethod);
  HandledBy := Format('SetRenderState(%-33s, 0x%.08X {=%s})', [
    DxbxRenderStateInfo[XboxRenderState].S,
    pdwPushArguments^,
    DxbxTypedValueToString(DxbxRenderStateInfo[XboxRenderState].T, pdwPushArguments^)]);
end;

procedure EmuNV2A_DepthTest();
begin
  HandledBy := 'DepthTest := ' + BooleanToString(pdwPushArguments^ <> 0);
{$IFDEF DXBX_USE_OPENGL}
  if pdwPushArguments^ <> 0 then
    glEnable(GL_DEPTH_TEST)
  else
    glDisable(GL_DEPTH_TEST);
{$ENDIF}
end;

procedure EmuNV2A_Dither();
begin
  HandledBy := 'Dither := ' + BooleanToString(pdwPushArguments^ <> 0);
{$IFDEF DXBX_USE_OPENGL}
  if pdwPushArguments^ <> 0 then
    glEnable(GL_DITHER)
  else
    glDisable(GL_DITHER);
{$ENDIF}
end;

procedure EmuNV2A_StencilTest();
begin
  HandledBy := 'StencilTest := ' + BooleanToString(pdwPushArguments^ <> 0);
{$IFDEF DXBX_USE_OPENGL}
  if pdwPushArguments^ <> 0 then
    glEnable(GL_STENCIL_TEST)
  else
    glDisable(GL_STENCIL_TEST);
{$ENDIF}
end;

//
// SetViewport
//

procedure EmuNV2A_DepthRange();
var
  ZNear: FLOAT;
  ZFar: FLOAT;
//  ViewportTranslateX: FLOAT;
//  ViewportTranslateY: FLOAT;
//  ViewportTranslateZ: FLOAT;
//  ViewportScaleX: FLOAT;
//  ViewportScaleY: FLOAT;
//  ViewportScaleZ: FLOAT;
begin
  Assert(dwCount = 2);
  HandledCount := 2;
  HandledBy := 'DepthRange(' + FloatsToString(pdwPushArguments, HandledCount) + ')';

//  // Interpret all related NV2A registers :
//  begin
//    ViewportTranslateX := NV2AInstance.VIEWPORT_TRANSLATE_X; // = Viewport.X * SuperSampleScaleX + ScreenSpaceOffsetX
//    ViewportTranslateY := NV2AInstance.VIEWPORT_TRANSLATE_Y; // = Viewport.Y * SuperSampleScaleY + ScreenSpaceOffsetY
//
//    // The following variables are pushed when a shader is active :
//    begin
//      ViewportTranslateZ := NV2AInstance.VIEWPORT_TRANSLATE_Z; // = ZScale * Viewport.MinZ
//      // NV2AInstance.VIEWPORT_TRANSLATE_W ignored. Should always be 0.
//      ViewportScaleX := NV2AInstance.VIEWPORT_SCALE_X; // = 0.5 * Viewport.Width * SuperSampleScaleX
//      ViewportScaleY := NV2AInstance.VIEWPORT_SCALE_Y; // = -0.5 * Viewport.Height * SuperSampleScaleY
//      ViewportScaleZ := NV2AInstance.VIEWPORT_SCALE_Z; // = ZScale * (Viewport.MaxZ - Viewport.MinZ)
//      // NV2AInstance.VIEWPORT_SCALE_W ignored. Should always be 0.
//    end;
//  end;

  ZNear := NV2AInstance.DEPTH_RANGE_NEAR / ZScale;
  ZFar := NV2AInstance.DEPTH_RANGE_FAR / ZScale;

{$IFDEF DXBX_USE_D3D}
  // TODO : Calculate the correct ViewPort values using the above determined variables :
  ViewPort.X := 0;
  ViewPort.Y := 0;
  ViewPort.Width := 640;
  ViewPort.Height := 480;
  // TODO : The following should behave differently under fixed-function when D3DRS_ZENABLE=D3DZB_USEW :
  ViewPort.MinZ := ZNear;
  ViewPort.MaxZ := ZFar;
  // Place the native call :
  g_pD3DDevice.SetViewport(ViewPort);
{$ENDIF}
{$IFDEF DXBX_USE_OPENGL}
  // Output from Vertices sample :
  //
  //  NV2A_VIEWPORT_TRANSLATE_X({320.53125, 240.53125, 0, 0}) > goes into c[133] (see EmuNV2A_ViewportOffset)
  //  NV2A_VIEWPORT_SCALE_X({320, -240, 16777215, 0}) > goes into c[134] (see EmuNV2A_ViewportScale)
  //  NV2A_DEPTH_RANGE_NEAR({0, 16777215}) > handled here
  //
  // (The presense of NV2A_VIEWPORT_SCALE_X indicates a shader is active.)
  //
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glFrustum({Left=}-1.0, {Right=}1.0, {Bottom=}-1.0, {Top=}1.0, ZNear, ZFar);

//  glFrustum(-320, 320, -240, 240, NV2AInstance.DEPTH_RANGE_NEAR, NV2AInstance.DEPTH_RANGE_FAR);
{$ENDIF}
end;

//
// SetTransform
//

procedure EmuNV2A_ModelViewMatrix();
begin
  Assert(dwCount >= 16);
  // Note : Disable this assignment-line to get more pushbuffer debug output :
  HandledCount := 16; // We handle only one matrix
  // TODO : Support multiple ModelView matrices (used for blending)

  HandledBy := 'SetTransform(ModelView, ' + FloatsToString(pdwPushArguments, HandledCount) + ')';
{$IFDEF DXBX_USE_D3D}
  // The ModelView = D3DTS_WORLD * D3DTS_VIEW.
  // We cannot decompose these two matrixes, but if we keep the World view as a static Identity view,
  // we should be able to use the ModelView as native View matrix. [If we did it the other way around
  // (set View to identity and apply ModelView to World), many fog, lighting and other aspects wouldn't
  // be influenced on our native D3D device.]
  // TODO : Is this reasoning sound?
  g_pD3DDevice.SetTransform(D3DTS_VIEW, PD3DMatrix(pdwPushArguments));
{$ENDIF}

{$IFDEF DXBX_USE_OPENGL}
  glMatrixMode(GL_MODELVIEW);

  // glLoadIdentity(); // Note : We assume this is not necessary here, as we set a complete matrix;
  // Note : OpenGL needs column-major format - it seems the pushbuffer matrix layout is indeed in that format.
  glLoadMatrixf(PGLfloat(pdwPushArguments));

  // D3D uses a left-handed coordinate space (z positive into screen), while OpenGL uses the right-handed system.
  // So, switch to left-handed coordinate space (as per http://www.opengl.org/resources/faq/technical/transformations.htm) :
  // TODO : Is this indeed necessary?
  glScalef(1.0, 1.0, -1.0);
{$ENDIF}
end;

procedure EmuNV2A_CompositeMatrix();
begin
  Assert(dwCount >= 16);

  // Note : Disable this assignment-line to get more pushbuffer debug output :
  HandledCount := 16; // We handle only one matrix

  HandledBy := 'SetTransform(Projection, ' + FloatsToString(pdwPushArguments, HandledCount) + ')';

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

{$IFDEF DXBX_USE_OPENGL}
//  glMatrixMode(GL_PROJECTION);
//  glLoadMatrixf(PGLfloat(pdwPushArguments));
{$ENDIF}
end;

//
// SetVertexShaderConstant
//

procedure EmuNV2A_SetVertexShaderConstant();
var
  Slot: uint;
begin
  HandledBy := 'SetVertexShaderConstant';

  // Make sure we use the correct index if we enter at an offset other than 0 :
  Slot := (dwMethod - NV2A_VP_UPLOAD_CONST__0) div 4;
  // Since we always start at NV2A_VP_UPLOAD_CONST__0, never handle more than allowed :
  Assert(Slot + dwCount <= NV2A_VP_UPLOAD_CONST__SIZE);

{$IFDEF DXBX_USE_D3D}
  HandledCount := dwCount;
  // Just set the constants right from the pushbuffer, as they come in batches and won't exceed the native bounds :
  g_pD3DDevice.SetVertexShaderConstant
  (
      // The VP_UPLOAD_CONST_ID GPU register is always pushed before the actual values, and contains the base Register for this batch :
      {Register=}NV2AInstance.VP_UPLOAD_CONST_ID + Slot,
      {pConstantData=}pdwPushArguments,
      {ConstantCount=}dwCount
  );
{$ENDIF}
{$IFDEF DXBX_USE_OPENGL}
  Assert(dwCount and 3 = 0); // Input must be a multiple of 4

  // Just set the constant right from the pushbuffer, as they come in batches and won't exceed the native bounds;
  // Can we set them in 1 go?
  if GL_EXT_gpu_program_parameters then
  begin
    // Handle all constants in 1 call :
    HandledCount := dwCount;
    glProgramEnvParameters4fvEXT
    (
      {target=}GL_VERTEX_PROGRAM_ARB,
      {index=}NV2AInstance.VP_UPLOAD_CONST_ID + Slot,
      {count=}HandledCount div 4,
      {params=}PGLfloat(pdwPushArguments)
    );
  end
  else
  begin
    // Handle 1 constant (other constants will be handled in the next GPU method callback, arriving here again) :
    HandledCount := 4;
    glProgramEnvParameter4fvARB
    (
      {target=}GL_VERTEX_PROGRAM_ARB,
      {index=}NV2AInstance.VP_UPLOAD_CONST_ID + Slot,
      {params=}PGLfloat(pdwPushArguments)
    );
    // Note : We cannot read from NV2AInstance.VP_UPLOAD_CONST[Slot], as only 1 DWORD is written there.
  end;

  HandledBy := HandledBy + Format('(%d, %s)', [NV2AInstance.VP_UPLOAD_CONST_ID + Slot, FloatsToString(pdwPushArguments, HandledCount)]);
{$ENDIF}
end;

//
// Draw[Indexed]Vertices[UP]
//

procedure EmuNV2A_DrawBeginEnd;
var
  NewPrimitiveType: X_D3DPRIMITIVETYPE;
begin
  NewPrimitiveType := X_D3DPRIMITIVETYPE(NV2AInstance.VERTEX_BEGIN_END);
  if (NewPrimitiveType = X_D3DPT_NONE) then
  begin
    HandledBy := 'DrawEnd()';

    // Trigger the draw :
    case PostponedDrawType of
      pdAlreadyDone: ; // No error, drawing is already done
      pdDrawVertices:
        PostponedDrawVertices;
      pdDrawIndexedVertices:
        PostponedDrawIndexedVertices;
    else
//      DxbxKrnlCleanup('TriggerDrawBeginEnd encountered unknown draw mode!');
    end;
{$IFDEF DXBX_USE_OPENGL}
    glEnd();
{$ENDIF}
  end
  else
  begin
    HandledBy := Format('DrawBegin(PrimitiveType=%d{=%s})', [Ord(NewPrimitiveType), X_D3DPRIMITIVETYPE2String(NewPrimitiveType)]);
{$IFDEF DXBX_USE_OPENGL}
    DxbxUpdateTextures();
    glBegin(NV2APrimitiveTypeToGL(NewPrimitiveType));
{$ENDIF}
  end;

  // Reset variables related to a single draw (others like VertexFormat and VertexAddress are persistent) :
  VertexCount := 0;
  VertexIndex := 0;
  PostponedDrawType := pdUndetermined;
  XBPrimitiveType := NewPrimitiveType;
  VertexOffset := 0;
end;

procedure EmuNV2A_SetVertexShaderBatch();
var
  Slot: uint;
begin
  HandledCount := dwCount;
  HandledBy := 'SetVertexShader';

  // Make sure we use the correct index if we enter at an offset other than 0 :
  Slot := (dwMethod - NV2A_VP_UPLOAD_INST__0) div 4;
  Assert(Slot + dwCount <= NV2A_VP_UPLOAD_INST__SIZE);

  Inc(NV2AInstance.VP_UPLOAD_FROM_ID, Slot);

  // Collect all slots in a separate array (as only part of it is present in the GPU registers) :
  memcpy(@VertexShaderSlots[NV2AInstance.VP_UPLOAD_FROM_ID], pdwPushArguments, dwCount * SizeOf(DWORD));
  // Batches are max 32 DWORDs, so just increase VP_UPLOAD_FROM_ID (the current slot index, max 136) :
  Inc(NV2AInstance.VP_UPLOAD_FROM_ID, dwCount);

  // TODO : When do we compile the shader?
end;

procedure EmuNV2A_SetVertexPos3f();
begin
  Assert(dwCount = 3);
  HandledCount := 3;
  HandledBy := 'SetVertexPos3f(' + FloatsToString(pdwPushArguments, HandledCount) + ')';
{$IFDEF DXBX_USE_OPENGL}
  glVertex3fv(PGLfloat(pdwPushArguments));
{$ENDIF}
end;

procedure EmuNV2A_SetVertexPos4f();
begin
  Assert(dwCount = 4);
  HandledCount := 4;
  HandledBy := 'SetVertexPos4f(' + FloatsToString(pdwPushArguments, HandledCount) + ')';
{$IFDEF DXBX_USE_OPENGL}
  glVertex4fv(PGLfloat(pdwPushArguments));
{$ENDIF}
end;

procedure EmuNV2A_SetVertexPos4s();
begin
  Assert(dwCount = 2);
  HandledCount := 2;
  HandledBy := 'SetVertexPos4s(' + ShortsToString(PSHORT(pdwPushArguments), 4) + ')';
{$IFDEF DXBX_USE_OPENGL}
  glVertex4sv(PGLshort(pdwPushArguments));
{$ENDIF}
end;

procedure EmuNV2A_SetVertexData2F();
var
  Slot: uint;
begin
  Slot := (dwMethod - NV2A_VERTEX_DATA2F__0) div 8;
  HandledCount := 2;
  HandledBy := 'SetVertexData2F(' + X_D3DVSDE2String(Slot) + ', ' + FloatsToString(pdwPushArguments, HandledCount) + ')';
{$IFDEF DXBX_USE_OPENGL}
  glVertexAttrib2fv(Slot, PGLfloat(pdwPushArguments));
{$ENDIF}
end;

procedure EmuNV2A_SetVertexData2S();
var
  Slot: uint;
begin
  Slot := (dwMethod - NV2A_VERTEX_DATA2S__0) div 4;
  // Default HandledCount := 1;
  HandledBy := 'SetVertexData2S(' + X_D3DVSDE2String(Slot) + ', ' + ShortsToString(PSHORT(pdwPushArguments), 2) + ')';
{$IFDEF DXBX_USE_OPENGL}
  glVertexAttrib2svARB(Slot, PGLshort(pdwPushArguments));
{$ENDIF}
end;

procedure EmuNV2A_SetVertexData4UB();
var
  Slot: uint;
begin
  Slot := (dwMethod - NV2A_VERTEX_DATA4UB__0) div 4;
  // Default HandledCount := 1;
  HandledBy := 'SetVertexData4UB(' + X_D3DVSDE2String(Slot) + ', ' + ColorBytesToString(PBYTE(pdwPushArguments), 4) + ')';
{$IFDEF DXBX_USE_OPENGL}
  glVertexAttrib4ubv(Slot, PGLubyte(pdwPushArguments));
{$ENDIF}
end;

procedure EmuNV2A_SetVertexData4S();
var
  Slot: uint;
begin
  Slot := (dwMethod - NV2A_VERTEX_DATA4S__0) div 8;
  HandledCount := 2;
  HandledBy := 'SetVertexData4S(' + X_D3DVSDE2String(Slot) + ', ' + ShortsToString(PSHORT(pdwPushArguments), 4) + ')';
{$IFDEF DXBX_USE_OPENGL}
  glVertexAttrib4sv(Slot, PGLshort(pdwPushArguments));
{$ENDIF}
end;

procedure EmuNV2A_SetVertexData4F();
var
  Slot: uint;
begin
  Slot := (dwMethod - NV2A_VERTEX_DATA4F__0) div 16;
  HandledCount := 4;
  HandledBy := 'SetVertexData4F(' + X_D3DVSDE2String(Slot) + ', ' + FloatsToString(pdwPushArguments, HandledCount) + ')';
{$IFDEF DXBX_USE_OPENGL}
  glVertexAttrib4fv(Slot, PGLfloat(pdwPushArguments));
{$ENDIF}
end;

procedure EmuNV2A_VertexData();
// The data for this command constists of all vertex attributes packed together.
// So in order to emulate it correctly, we have to find out which vertex attributes
// are active and how they are laid out in the pushbuffer (at max. 2047 DWORD per batch).
// (In OpenGL, we do this by calling DxbxSetupVertexPointers with the push-buffer address).
// Here, we draw each batch separately, to avoid intermediary copies at the cost
// of more drawing calls (time will tell if this is fast enough it in practise.)
//
// Alternatively, we could copy all bactches to a buffer and trigger drawing that
// when the vertex data is closed off in DrawBeginEnd, but that's for later.
{$IFDEF DXBX_USE_D3D}
var
  VPDesc: VertexPatchDesc;
  VertPatch: VertexPatcher;
  VertexCount: UINT;
  hRet: HRESULT;
  pData: PByte;
{$ENDIF}
{$IFDEF DXBX_USE_OPENGL}
var
  BatchSize: uint;
{$ENDIF}
begin
{$IFDEF DXBX_USE_D3D}
  HandledCount := dwCount;
  HandledBy := 'DrawVertices';
  // TODO : Postpone the draw in here to TriggerDrawBeginEnd, instead collect all vertices first.
  PostponedDrawType := pdDrawVertices;
  pVertexData := pdwPushArguments;

  dwVertexShader := 0;
  if IsEngineFixedFunctionPipeline() then
  begin
    // Determine the FVF flags :
    case (NV2AInstance.VTXFMT[X_D3DVSDE_POSITION] and NV2A_VTXFMT_SIZE_MASK) shr NV2A_VTXFMT_SIZE_SHIFT of
      3:
        dwVertexShader := D3DFVF_XYZ;
      4:
        dwVertexShader := D3DFVF_XYZRHW;
    else
      DxbxKrnlCleanup('Cannot determine FVF type!');
    end;

    if ((NV2AInstance.VTXFMT[X_D3DVSDE_DIFFUSE] and NV2A_VTXFMT_SIZE_MASK) shr NV2A_VTXFMT_SIZE_SHIFT) > 0 then
      dwVertexShader := dwVertexShader or D3DFVF_DIFFUSE;

    // TODO : Support all flags here
  end
  else
  begin
    // At some point we have to create the vertex shader locally - for this we need at least :
    // - the original declaration, ex. D3DVSD_STREAM(0), D3DVSD_REG({Position}0,D3DVSDT_FLOAT3),D3DVSD_REG({Diffuse}1,D3DVSDT_D3DCOLOR),D3DVSD_END()
    // - the original shader (this one we've collected in the VertexShaderSlots array)
    // Also, it would be nice if we didn't create the shader every time it appears in the pushbuffer,
    // so a CRC based lookup would be good to have too.
    //
    // The following line doesn't work, as the declaration is in the wrong format (we should decode the VTXFMT array for that) :
    // g_pD3DDevice.CreateVertexShader(@NV2AInstance.VTXFMT[0], @VertexShaderSlots[0], {out}dwVertexShader, 0);

    // Make sure we have a large enough vertex buffer :
    if (pVertexBuffer = nil) then
    begin
      hRet := g_pD3DDevice.CreateVertexBuffer(2047*SizeOf(DWORD), D3DUSAGE_WRITEONLY, {FVF=}0, D3DPOOL_MANAGED, @pVertexBuffer);
      if (FAILED(hRet)) then
        DxbxKrnlCleanup('Unable to create vertex buffer cache for PushBuffer emulation ($1818, dwCount : %d)', [dwCount]);
    end;

    // Copy vertex data into it :
    begin
      hRet := IDirect3DVertexBuffer(pVertexBuffer).Lock(VertexOffset, dwCount*4, {out}pData, 0);
      if (FAILED(hRet)) then
      begin
        memcpy({dest}pData, {src=}pdwPushArguments, dwCount*4);
        IDirect3DVertexBuffer(pVertexBuffer).Unlock();
        Inc(VertexOffset, dwCount*4);
      end;
    end;

    Exit;
  end;
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
{$ENDIF}
{$IFDEF DXBX_USE_D3D8}
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
{$ENDIF}
{$IFDEF DXBX_USE_OPENGL}
  HandledCount := dwCount;
  HandledBy := 'VertexData';

  // For the first vertex batch, do some preprocessing :
  BatchSize := dwCount * SizeOf(DWORD);
  if (VertexCount = 0) then
  begin
    PostponedDrawType := pdDrawVertices;

    // Determine stride :
    DxbxCurrentVertexStride := DxbxCalculateVertexStride();

    // See if we're about to handle just one batch :
    if (NextGPUCommand() <> NV2A_VERTEX_DATA) then
    begin
      // We're handling just a single batch, this can be emulated in-place.
      // Just use the pushbuffer argument data location as vertex pointer :
      DxbxCurrentVertices := PBYTE(pdwPushArguments);

      // Also calculate the number of vertices we're about to draw :
      VertexCount := BatchSize div DxbxCurrentVertexStride;

      Exit;
    end;
  end;

  // Here we have to handle multiple vertex batches;
  // Collect all vertices in a buffer of ourselves :
  DxbxMakeVertexSpace((VertexCount * DxbxCurrentVertexStride) + BatchSize);
  memcpy(@DxbxVertices[VertexCount * DxbxCurrentVertexStride], pdwPushArguments, BatchSize);
  Inc(VertexCount, BatchSize div DxbxCurrentVertexStride);

  // Make sure we're going to render from our own copy :
  DxbxCurrentVertices := DxbxVertices;

{$ENDIF}
end;

procedure _RenderIndexedVertices(dwCount: DWORD);
{$IFDEF DXBX_USE_D3D}
var
  VPDesc: VertexPatchDesc;
  VertPatch: VertexPatcher;
{$ENDIF}
begin
{$IFDEF DXBX_USE_D3D}
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
{$ENDIF}
end;

procedure EmuNV2A_RegisterBatchOfWordIndices();
// Collects indexes for the postponed Draw.
{$IFDEF DXBX_USE_OPENGL}
var
  NewCount: uint;
{$ENDIF}
{$IFDEF DXBX_USE_D3D}
var
  pIndexData: PWORD;
{$ENDIF}
begin
  HandledBy := 'DrawIndexedVertices';
  HandledCount := dwCount;

{$IFDEF DXBX_USE_OPENGL}
  // The following code always creates a copy of the indices in the pushbuffer.
  // We could improve upon that by preventing a copy just like is done in EmuNV2A_VertexData.
  // Indices do have a special case though : When the vertices to be drawn are an uneven number,
  // the pushbuffer contains a separate command for the last index. Instead of falling back to
  // copying all indices to a buffer of ourselves, we could alternatively hack the pushbuffer
  // so that the last index is put 1 DWORD to the left (and then skip the 2 DWORDs of this
  // 32-bit index command). If we do it like that, we can render all small index-batches
  // right from the pushbuffer itself, avoiding a slow copy. (This is still TODO.)

  NewCount := VertexCount + (dwCount * 2); // Each DWORD data in the pushbuffer carries 2 words
  DxbxMakeIndexSpace(NewCount);
  // Collect all indexes; we'll call glDrawElements() when the indices are finished in DrawBeginEnd :
  memcpy(@DxbxIndices[VertexCount], pdwPushArguments, dwCount * SizeOf(DWORD));
  VertexCount := NewCount;

  PostponedDrawType := pdDrawIndexedVertices;
{$ENDIF}

{$IFDEF DXBX_USE_D3D}
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
{$ENDIF}
end;

procedure EmuNV2A_RegisterBatchOfDWordIndices();
// This command is (normally) used to end an index buffer with one last index.
// TODO : Collect this index together with the previous batch(es) and wait for the postponed draw.

{$IFDEF DXBX_USE_D3D}
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
{$ENDIF}
begin
  HandledBy := 'DrawIndexedVertices';
  HandledCount := dwCount;

{$IFDEF DXBX_USE_OPENGL}
  Assert(dwCount = 1); // In practice this command recieves only one index, if there is an off nr of indices to draw.
  Assert(PostponedDrawType = pdDrawIndexedVertices); // The previous indices where already handled

  DxbxMakeIndexSpace(VertexCount + 1); // Make sure we have space for this index
  DxbxIndices[VertexCount] := Word(pdwPushArguments^); // Put it in place
  Inc(VertexCount); // Count it.
{$ENDIF}

{$IFDEF DXBX_USE_D3D}
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
{$ENDIF}
end;

procedure EmuNV2A_RegisterBatchOfVertices();
// This command just registers the number of vertices are to be used in D3DDevice_DrawVertices.
var
  BatchCount: Integer;
  BatchIndex: Integer;
begin
  // Decode the arguments (index is an incrementing number, count is cumulative per batch) :
  DWORDSplit2(pdwPushArguments^, 24, {out}BatchIndex, 8, {out}BatchCount);
  Inc(BatchCount);

  // Register the index only once :
  if VertexCount = 0 then
  begin
    // Note : Additional commands will mention an index right next to the previous batch
    VertexIndex := BatchIndex;

    // Register that the postponed draw will be a DrawVertices() :
    PostponedDrawType := pdDrawVertices;
  end;

  // Accumulate the vertices for the draw that will follow :
  Inc(VertexCount, BatchCount);

  HandledBy := Format('VertexBatch(BatchIndex=%d, BatchCount=%d) > VertexIndex=%d VertexCount=%d',
    [BatchIndex, BatchCount, VertexIndex, VertexCount]);
end;

procedure EmuNV2A_TextureFormat();
begin
  HandledBy := ResourceFormatToStr(pdwPushArguments^);
end;

const
  NV2ACallbacks: array [0..($2000 div Sizeof(DWORD)) -1] of procedure = (
  {0000}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0040}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0080}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {00C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0100 NV2A_NOP}EmuNV2A_NOP,
             nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0130 NV2A_FLIP_STALL}EmuNV2A_FlipStall, // TODO : Should we trigger at NV2A_FLIP_INCREMENT_WRITE instead?
                                                                         nil, nil, nil,
  {0140}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0180}
  // These can safely be ignored :
  {0180 NV2A_DMA_NOTIFY}EmuNV2A_CDevice_Init,
  {0184 NV2A_DMA_TEXTURE0}EmuNV2A_CDevice_Init,
  {0188 NV2A_DMA_TEXTURE1}EmuNV2A_CDevice_Init,
  {018C}nil,
  {0190 NV2A_DMA_STATE}EmuNV2A_CDevice_Init,
  {0194 NV2A_DMA_COLOR}EmuNV2A_CDevice_Init,
  {0198 NV2A_DMA_ZETA}EmuNV2A_CDevice_Init,
  {019C NV2A_DMA_VTXBUF0}EmuNV2A_CDevice_Init,
  {01A0 NV2A_DMA_VTXBUF1}EmuNV2A_CDevice_Init,
  {01A4 NV2A_DMA_FENCE}EmuNV2A_CDevice_Init,
  {01A8 NV2A_DMA_QUERY}EmuNV2A_CDevice_Init,
  {01AC}                                                       nil, nil, nil, nil, nil,
  {01C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0200}nil, nil,
  {0208 NV2A_RT_FORMAT}EmuNV2A_SetRenderTarget, // Set surface format
  {020C}nil,
  {0210 NV2A_COLOR_OFFSET}EmuNV2A_SetRenderTargetSurface,
  {0214}                         nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0240}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0280}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {02C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0300 NV2A_ALPHA_FUNC_ENABLE}EmuNV2A_SetRenderState, // = X_D3DRS_ALPHATESTENABLE
  {0304 NV2A_BLEND_FUNC_ENABLE}EmuNV2A_SetRenderState, // = X_D3DRS_ALPHABLENDENABLE
  {0308}nil,
  {030C NV2A_DEPTH_TEST_ENABLE}EmuNV2A_DepthTest, // X_D3DRS_ZENABLE
  {0310 NV2A_DITHER_ENABLE}EmuNV2A_Dither, // X_D3DRS_DITHERENABLE
  {0314}nil,
  {0318 NV2A_POINT_PARAMETERS_ENABLE}EmuNV2A_SetRenderState, // = X_D3DRS_POINTSCALEENABLE
  {031C NV2A_POINT_SMOOTH_ENABLE}EmuNV2A_SetRenderState, // = X_D3DRS_POINTSPRITEENABLE
  {0320}nil,
  {0324}nil,
  {0328 NV2A_SKIN_MODE}EmuNV2A_SetRenderState, // = X_D3DRS_VERTEXBLEND
  {032C NV2A_STENCIL_ENABLE}EmuNV2A_StencilTest, // = X_D3DRS_STENCILENABLE
  {0330 NV2A_POLYGON_OFFSET_POINT_ENABLE}EmuNV2A_SetRenderState, // = X_D3DRS_POINTOFFSETENABLE
  {0334 NV2A_POLYGON_OFFSET_LINE_ENABLE}EmuNV2A_SetRenderState, // = X_D3DRS_WIREFRAMEOFFSETENABLE
  {0338 NV2A_POLYGON_OFFSET_FILL_ENABLE}EmuNV2A_SetRenderState, // = X_D3DRS_SOLIDOFFSETENABLE
  {033C NV2A_ALPHA_FUNC_FUNC}EmuNV2A_SetRenderState, // = X_D3DRS_ALPHAFUNC
  {0340 NV2A_ALPHA_FUNC_REF}EmuNV2A_SetRenderState, // = X_D3DRS_ALPHAREF
  {0344 NV2A_BLEND_FUNC_SRC}EmuNV2A_SetRenderState, // = X_D3DRS_SRCBLEND
  {0348 NV2A_BLEND_FUNC_DST}EmuNV2A_SetRenderState, // = X_D3DRS_DESTBLEND
  {034C NV2A_BLEND_COLOR}EmuNV2A_SetRenderState, // = X_D3DRS_BLENDCOLOR
  {0350 NV2A_BLEND_EQUATION}EmuNV2A_SetRenderState, // = X_D3DRS_BLENDOP
  {0354 NV2A_DEPTH_FUNC}EmuNV2A_SetRenderState, // = X_D3DRS_ZFUNC
  {0358 NV2A_COLOR_MASK}EmuNV2A_SetRenderState, // = X_D3DRS_COLORWRITEENABLE
  {035C NV2A_DEPTH_WRITE_ENABLE}EmuNV2A_SetRenderState, // = X_D3DRS_ZWRITEENABLE
  {0360 NV2A_STENCIL_MASK}EmuNV2A_SetRenderState, // = X_D3DRS_STENCILWRITEMASK
  {0364 NV2A_STENCIL_FUNC_FUNC}EmuNV2A_SetRenderState, // = X_D3DRS_STENCILFUNC
  {0368 NV2A_STENCIL_FUNC_REF}EmuNV2A_SetRenderState, // = X_D3DRS_STENCILREF
  {036C NV2A_STENCIL_FUNC_MASK}EmuNV2A_SetRenderState, // = X_D3DRS_STENCILMASK
  {0370}nil,
  {0374 NV2A_STENCIL_OP_ZFAIL}EmuNV2A_SetRenderState, // = X_D3DRS_STENCILZFAIL
  {0378 NV2A_STENCIL_OP_ZPASS}EmuNV2A_SetRenderState, // = X_D3DRS_STENCILPASS
  {037c NV2A_SHADE_MODEL}EmuNV2A_SetRenderState, // = X_D3DRS_SHADEMODE
  {0380 NV2A_LINE_WIDTH}EmuNV2A_SetRenderState, // = X_D3DRS_LINEWIDTH
  {0384 NV2A_POLYGON_OFFSET_FACTOR}EmuNV2A_SetRenderState, // = X_D3DRS_POLYGONOFFSETZSLOPESCALE
  {0388 NV2A_POLYGON_OFFSET_UNITS}EmuNV2A_SetRenderState, // = X_D3DRS_POLYGONOFFSETZOFFSET
  {038c NV2A_POLYGON_MODE_FRONT}EmuNV2A_SetRenderState, // = X_D3DRS_FILLMODE
  {0390}nil,
  {0394 NV2A_DEPTH_RANGE_FAR}EmuNV2A_DepthRange, // Always the last method for SetViewport, so we use it as a trigger
  {0398}nil,
  {039C}nil,
  {03A0 NV2A_FRONT_FACE}EmuNV2A_SetRenderState, // = X_D3DRS_FRONTFACE
  {03A4 NV2A_NORMALIZE_ENABLE}EmuNV2A_SetRenderState, // = X_D3DRS_NORMALIZENORMALS
  {03A8}                                                  nil, nil, nil, nil, nil, nil,
  {03C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0400}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0440}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0480 NV2A_MODELVIEW0_MATRIX__0}EmuNV2A_ModelViewMatrix,
             nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {04C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0500}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0540}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0580}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {05C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0600}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0640}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0680 NV2A_COMPOSITE_MATRIX__0}EmuNV2A_CompositeMatrix, // SetTransform
             nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {06C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0700}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0740}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0780}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {07C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0800}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0840}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0880}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {08C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0900}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0940}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0980}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {09C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {09F8 NV2A_SWATH_WIDTH}EmuNV2A_SetRenderState, // = X_D3DRS_SWATHWIDTH
  {09FC NV2A_FLAT_SHADE_OP}EmuNV2A_CDevice_Init,
  {0A00}nil, nil, nil, nil, nil, nil, nil, nil,
  {0A20}EmuNV2A_ViewportOffset, EmuNV2A_ViewportOffset, EmuNV2A_ViewportOffset, EmuNV2A_ViewportOffset,
                                                                    nil, nil, nil, nil,
  {0A40}nil, nil, nil, nil,
  {0A50 NV2A_EYE_POSITION__0}EmuNV2A_CDevice_Init,
  {0A54 NV2A_EYE_POSITION__1}EmuNV2A_CDevice_Init,
  {0A58 NV2A_EYE_POSITION__2}EmuNV2A_CDevice_Init,
  {0A5C NV2A_EYE_POSITION__3}EmuNV2A_CDevice_Init,
                                                nil, nil, nil, nil, nil, nil, nil, nil,
  {0A80}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0AC0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0AF0}EmuNV2A_ViewportScale, EmuNV2A_ViewportScale, EmuNV2A_ViewportScale, EmuNV2A_ViewportScale,
  {0B00 NV2A_VP_UPLOAD_INST__0}EmuNV2A_SetVertexShaderBatch,
  {0B04 NV2A_VP_UPLOAD_INST__1}EmuNV2A_SetVertexShaderBatch,
  {0B08 NV2A_VP_UPLOAD_INST__2}EmuNV2A_SetVertexShaderBatch,
  {0B0C NV2A_VP_UPLOAD_INST__3}EmuNV2A_SetVertexShaderBatch,
  {0B10 NV2A_VP_UPLOAD_INST__4}EmuNV2A_SetVertexShaderBatch,
  {0B14 NV2A_VP_UPLOAD_INST__5}EmuNV2A_SetVertexShaderBatch,
  {0B18 NV2A_VP_UPLOAD_INST__6}EmuNV2A_SetVertexShaderBatch,
  {0B1C NV2A_VP_UPLOAD_INST__7}EmuNV2A_SetVertexShaderBatch,
  {0B20 NV2A_VP_UPLOAD_INST__8}EmuNV2A_SetVertexShaderBatch,
  {0B24 NV2A_VP_UPLOAD_INST__9}EmuNV2A_SetVertexShaderBatch,
  {0B28 NV2A_VP_UPLOAD_INST__10}EmuNV2A_SetVertexShaderBatch,
  {0B2C NV2A_VP_UPLOAD_INST__11}EmuNV2A_SetVertexShaderBatch,
  {0B30 NV2A_VP_UPLOAD_INST__12}EmuNV2A_SetVertexShaderBatch,
  {0B34 NV2A_VP_UPLOAD_INST__13}EmuNV2A_SetVertexShaderBatch,
  {0B38 NV2A_VP_UPLOAD_INST__14}EmuNV2A_SetVertexShaderBatch,
  {0B3C NV2A_VP_UPLOAD_INST__15}EmuNV2A_SetVertexShaderBatch,
  {0B40 NV2A_VP_UPLOAD_INST__16}EmuNV2A_SetVertexShaderBatch,
  {0B44 NV2A_VP_UPLOAD_INST__17}EmuNV2A_SetVertexShaderBatch,
  {0B48 NV2A_VP_UPLOAD_INST__18}EmuNV2A_SetVertexShaderBatch,
  {0B4C NV2A_VP_UPLOAD_INST__19}EmuNV2A_SetVertexShaderBatch,
  {0B50 NV2A_VP_UPLOAD_INST__20}EmuNV2A_SetVertexShaderBatch,
  {0B54 NV2A_VP_UPLOAD_INST__21}EmuNV2A_SetVertexShaderBatch,
  {0B58 NV2A_VP_UPLOAD_INST__22}EmuNV2A_SetVertexShaderBatch,
  {0B5C NV2A_VP_UPLOAD_INST__23}EmuNV2A_SetVertexShaderBatch,
  {0B60 NV2A_VP_UPLOAD_INST__24}EmuNV2A_SetVertexShaderBatch,
  {0B64 NV2A_VP_UPLOAD_INST__25}EmuNV2A_SetVertexShaderBatch,
  {0B68 NV2A_VP_UPLOAD_INST__26}EmuNV2A_SetVertexShaderBatch,
  {0B6C NV2A_VP_UPLOAD_INST__27}EmuNV2A_SetVertexShaderBatch,
  {0B70 NV2A_VP_UPLOAD_INST__28}EmuNV2A_SetVertexShaderBatch,
  {0B74 NV2A_VP_UPLOAD_INST__29}EmuNV2A_SetVertexShaderBatch,
  {0B78 NV2A_VP_UPLOAD_INST__30}EmuNV2A_SetVertexShaderBatch,
  {0B7C NV2A_VP_UPLOAD_INST__31}EmuNV2A_SetVertexShaderBatch,
  {0B80 NV2A_VP_UPLOAD_CONST__0}EmuNV2A_SetVertexShaderConstant,
  {0B84 NV2A_VP_UPLOAD_CONST__1}EmuNV2A_SetVertexShaderConstant,
  {0B88 NV2A_VP_UPLOAD_CONST__2}EmuNV2A_SetVertexShaderConstant,
  {0B8C NV2A_VP_UPLOAD_CONST__3}EmuNV2A_SetVertexShaderConstant,
  {0B90 NV2A_VP_UPLOAD_CONST__4}EmuNV2A_SetVertexShaderConstant,
  {0B94 NV2A_VP_UPLOAD_CONST__5}EmuNV2A_SetVertexShaderConstant,
  {0B98 NV2A_VP_UPLOAD_CONST__6}EmuNV2A_SetVertexShaderConstant,
  {0B9C NV2A_VP_UPLOAD_CONST__7}EmuNV2A_SetVertexShaderConstant,
  {0BA0 NV2A_VP_UPLOAD_CONST__8}EmuNV2A_SetVertexShaderConstant,
  {0BA4 NV2A_VP_UPLOAD_CONST__9}EmuNV2A_SetVertexShaderConstant,
  {0BA8 NV2A_VP_UPLOAD_CONST__10}EmuNV2A_SetVertexShaderConstant,
  {0BAC NV2A_VP_UPLOAD_CONST__11}EmuNV2A_SetVertexShaderConstant,
  {0BB0 NV2A_VP_UPLOAD_CONST__12}EmuNV2A_SetVertexShaderConstant,
  {0BB4 NV2A_VP_UPLOAD_CONST__13}EmuNV2A_SetVertexShaderConstant,
  {0BB8 NV2A_VP_UPLOAD_CONST__14}EmuNV2A_SetVertexShaderConstant,
  {0BBC NV2A_VP_UPLOAD_CONST__15}EmuNV2A_SetVertexShaderConstant,
  {0BC0 NV2A_VP_UPLOAD_CONST__16}EmuNV2A_SetVertexShaderConstant,
  {0BC4 NV2A_VP_UPLOAD_CONST__17}EmuNV2A_SetVertexShaderConstant,
  {0BC8 NV2A_VP_UPLOAD_CONST__18}EmuNV2A_SetVertexShaderConstant,
  {0BCC NV2A_VP_UPLOAD_CONST__19}EmuNV2A_SetVertexShaderConstant,
  {0BD0 NV2A_VP_UPLOAD_CONST__20}EmuNV2A_SetVertexShaderConstant,
  {0BD4 NV2A_VP_UPLOAD_CONST__21}EmuNV2A_SetVertexShaderConstant,
  {0BD8 NV2A_VP_UPLOAD_CONST__22}EmuNV2A_SetVertexShaderConstant,
  {0BDC NV2A_VP_UPLOAD_CONST__23}EmuNV2A_SetVertexShaderConstant,
  {0BE0 NV2A_VP_UPLOAD_CONST__24}EmuNV2A_SetVertexShaderConstant,
  {0BE4 NV2A_VP_UPLOAD_CONST__25}EmuNV2A_SetVertexShaderConstant,
  {0BE8 NV2A_VP_UPLOAD_CONST__26}EmuNV2A_SetVertexShaderConstant,
  {0BEC NV2A_VP_UPLOAD_CONST__27}EmuNV2A_SetVertexShaderConstant,
  {0BF0 NV2A_VP_UPLOAD_CONST__28}EmuNV2A_SetVertexShaderConstant,
  {0BF4 NV2A_VP_UPLOAD_CONST__29}EmuNV2A_SetVertexShaderConstant,
  {0BF8 NV2A_VP_UPLOAD_CONST__30}EmuNV2A_SetVertexShaderConstant,
  {0BFC NV2A_VP_UPLOAD_CONST__31}EmuNV2A_SetVertexShaderConstant,
  {0C00}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0C40}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0C80}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0CC0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0D00}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0D40}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0D80}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0DC0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0E00}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0E40}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0E80}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0EC0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0F00}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0F40}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0F80}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {0FC0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1000}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1040}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1080}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {10C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1100}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1140}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1180}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {11C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1200}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1240}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1280}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {12C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1300}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1340}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1380}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {13C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1400}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1440}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {147C NV2A_POLYGON_STIPPLE_ENABLE}EmuNV2A_SetRenderState, // = X_D3DRS_STIPPLEENABLE
  {1480}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {14C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1500 NV2A_VERTEX_POS_3F_X}EmuNV2A_SetVertexPos3f,
             nil, nil, nil, nil, nil,
  {1518 NV2A_VERTEX_POS_4F_X}EmuNV2A_SetVertexPos4f,
                                           nil, nil, nil,
  {1528 NV2A_VERTEX_POS_3I_XY}EmuNV2A_SetVertexPos4s,
                                                               nil, nil, nil, nil, nil,
  {1540}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1580}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {15C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1600}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1640}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1680}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {16BC NV2A_EDGEFLAG_ENABLE}EmuNV2A_CDevice_Init,
  {16C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1700}nil, nil, nil, nil,
  {1710 NV2A_VTX_CACHE_INVALIDATE}EmuNV2A_VertexCacheInvalidate,
                                 nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1740}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1780}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {17C0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {17FC NV2A_VERTEX_BEGIN_END}EmuNV2A_DrawBeginEnd,
  {1800 NV2A_VB_ELEMENT_U16}EmuNV2A_RegisterBatchOfWordIndices,
             nil,
  {1808 NV2A_VB_ELEMENT_U32}EmuNV2A_RegisterBatchOfDWordIndices,
                       nil,
  {1810 NV2A_VB_VERTEX_BATCH}EmuNV2A_RegisterBatchOfVertices,
                                 nil,
  {1818 NV2A_VERTEX_DATA}EmuNV2A_VertexData,
                                           nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1840}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1880}EmuNV2A_SetVertexData2F, nil, EmuNV2A_SetVertexData2F, nil,
  {1890}EmuNV2A_SetVertexData2F, nil, EmuNV2A_SetVertexData2F, nil,
  {18A0}EmuNV2A_SetVertexData2F, nil, EmuNV2A_SetVertexData2F, nil,
  {18B0}EmuNV2A_SetVertexData2F, nil, EmuNV2A_SetVertexData2F, nil,
  {18C0}EmuNV2A_SetVertexData2F, nil, EmuNV2A_SetVertexData2F, nil,
  {18D0}EmuNV2A_SetVertexData2F, nil, EmuNV2A_SetVertexData2F, nil,
  {18E0}EmuNV2A_SetVertexData2F, nil, EmuNV2A_SetVertexData2F, nil,
  {18F0}EmuNV2A_SetVertexData2F, nil, EmuNV2A_SetVertexData2F, nil,
  {1900}EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S,
  {1910}EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S,
  {1920}EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S,
  {1930}EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S, EmuNV2A_SetVertexData2S,
  {1940}EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB,
  {1950}EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB,
  {1960}EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB,
  {1970}EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB, EmuNV2A_SetVertexData4UB,
  {1980}EmuNV2A_SetVertexData4S, nil, EmuNV2A_SetVertexData4S, nil,
  {1990}EmuNV2A_SetVertexData4S, nil, EmuNV2A_SetVertexData4S, nil,
  {19A0}EmuNV2A_SetVertexData4S, nil, EmuNV2A_SetVertexData4S, nil,
  {19B0}EmuNV2A_SetVertexData4S, nil, EmuNV2A_SetVertexData4S, nil,
  {19C0}EmuNV2A_SetVertexData4S, nil, EmuNV2A_SetVertexData4S, nil,
  {19D0}EmuNV2A_SetVertexData4S, nil, EmuNV2A_SetVertexData4S, nil,
  {19E0}EmuNV2A_SetVertexData4S, nil, EmuNV2A_SetVertexData4S, nil,
  {19F0}EmuNV2A_SetVertexData4S, nil, EmuNV2A_SetVertexData4S, nil,
  {1A00}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1A10}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1A20}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1A30}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1A40}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1A50}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1A60}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1A70}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1A80}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1A90}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1AA0}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1AB0}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1AC0}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1AD0}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1AE0}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1AF0}EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F, EmuNV2A_SetVertexData4F,
  {1B00}nil,
  {1B04 NV2A_TX_FORMAT(0)}EmuNV2A_TextureFormat,
                  nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1B40}nil,
  {1B44 NV2A_TX_FORMAT(1)}EmuNV2A_TextureFormat,
                  nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1B80}nil,
  {1B84 NV2A_TX_FORMAT(2)}EmuNV2A_TextureFormat,
                  nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1BC0}nil,
  {1BC4 NV2A_TX_FORMAT(3)}EmuNV2A_TextureFormat,
                  nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1C00}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1C40}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1C80}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1CC0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1D00}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1D40}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1D6C NV2A_SEMAPHORE_OFFSET}EmuNV2A_CDevice_Init,
  {1D70 NV2A_WRITE_SEMAPHORE_RELEASE}nil,
  {1D74}nil,
  {1D78 NV2A_DEPTHCLIPCONTROL}EmuNV2A_SetRenderState, // = X_D3DRS_DEPTHCLIPCONTROL
  {1D7C NV2A_MULTISAMPLE_CONTROL}EmuNV2A_SetRenderState, // = X_D3DRS_MULTISAMPLEANTIALIAS // Also send by D3DRS_MULTISAMPLEMASK (both values in 1 command)
  {1D80 NV2A_COMPRESS_ZBUFFER_EN}EmuNV2A_CDevice_Init,
  {1D84 NV2A_OCCLUDE_ZSTENCIL_EN}EmuNV2A_SetRenderState, // = X_D3DRS_OCCLUSIONCULLENABLE
  {1D88}nil,
  {1D8C NV2A_CLEAR_DEPTH_VALUE}nil,
  {1D90 NV2A_CLEAR_VALUE}nil,
  {1D94 NV2A_CLEAR_BUFFERS}EmuNV2A_ClearBuffers, // Gives clear flags, should trigger the clear
                                      nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1DC0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1E00}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1E40}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1E68 NV2A_SHADOW_ZSLOPE_THRESHOLD}EmuNV2A_CDevice_Init,
  {1E6C NV2A_TX_RCOMP}EmuNV2A_SetRenderState, // = X_D3DRS_SHADOWFUNC
                                                                   nil, nil, nil, nil,
  {1E80}nil, nil, nil, nil, nil,
  {1E94 NV2A_ENGINE}nil,
                                      nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1EC0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1F00}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1F40}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1F80}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
  {1FC0}nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil
  );

(*
        case dwMethod of
          0:
            HandledCount := dwCount;

          NV2A_WRITE_SEMAPHORE_RELEASE:
          begin
            // TODO : What should we do with this data? Most probably it should go into a semaphore somewhere;
            // Perhaps this semaphore is in the 96 bytes that was first allocated with MmAllocateContiguousMemoryEx
            // (as we've seen the m_pGPUTime variable resides there too). Also telling : this command is pushed by SetFence!
          end;

          // SetTexture / SwitchTexture :
          NV2A_TX_OFFSET__0, NV2A_TX_OFFSET__1, NV2A_TX_OFFSET__2, NV2A_TX_OFFSET__3:
            ; // TODO : Handle this

          NV2A_TX_FORMAT__0, NV2A_TX_FORMAT__1, NV2A_TX_FORMAT__2, NV2A_TX_FORMAT__3:
            ; // TODO : Handle this

          NV2A_TX_ENABLE__0, NV2A_TX_ENABLE__1, NV2A_TX_ENABLE__2, NV2A_TX_ENABLE__3:
            ; // TODO : Handle this

          // Draw[Indexed]Vertices[UP] :

          // SetRenderState special cases (value conversions) :
          NV2A_FOG_COLOR:
          begin
{$IFDEF DXBX_USE_D3D}
            g_pD3DDevice.SetRenderState(D3DRS_FOGCOLOR, SwapRGB(pdwPushArguments^));
{$ENDIF}
            HandledBy := 'SetRenderState';
          end;

          NV2A_CULL_FACE_ENABLE:
          begin
{$IFDEF DXBX_USE_D3D}
            if pdwPushArguments^ = DWORD(BOOL_FALSE) then
              g_pD3DDevice.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
{$ENDIF}
            HandledBy := 'SetRenderState';
          end;

          NV2A_CULL_FACE:
          begin
{$IFDEF DXBX_USE_D3D}
            g_pD3DDevice.SetRenderState(D3DRS_CULLMODE, D3DCULL_CW); // TODO : Use D3DCULL_CCW if D3DRS_FRONTFACE=D3DCULL_CW
{$ENDIF}
            HandledBy := 'SetRenderState';
          end;

        else
          // SetRenderState, normal cases (direct value copy) :
          if TrySetRenderState(dwMethod, pdwPushArguments, dwCount) then
            HandledBy := 'SetRenderState'
          else
            // TODO : Add other generic states here, like texture stage states.
        end; // case
*)
  // D3DDevice_SetRenderTarget        : NV2A_RT_PITCH, NV2A_COLOR_OFFSET and NV2A_ZETA_OFFSET
  // D3DDevice_SetStreamSource (lazy) : NV2A_VTXFMT and NV2A_VTXBUF_ADDRESS
  // D3DDevice_DrawVertices           : NV2A_VB_VERTEX_BATCH (the batch is indicated by an Index and a Count in the active vertex buffer - as set by SetStreamSource)
  // D3DDevice_DrawIndexedVertices    : NV2A_VB_ELEMENT_U16 (and an optional closing NV2A_VB_ELEMENT_U32)  (the index is added into SetStreamSource)
  // D3DDevice_DrawVerticesUP         : NV2A_VERTEX_DATA
  // D3DDevice_DrawIndexedVerticesUP  : NV2A_VERTEX_DATA

procedure XTL_EmuExecutePushBufferRaw
(
    pdwPushData: PDWord;
    pdwPushDataEnd: PDWord
); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  dwPushCommand: DWord;
  dwSubCh: DWord;
  bNoInc: BOOL_;
  StepNr: Integer;
  LogPrefixStr: string;
  NV2ACallback: procedure;
begin
  if MayLog(lfUnit) then
    if UIntPtr(pdwPushData) <> UIntPtr(pdwPushDataEnd) then
      DbgPrintf('  NV2A run from 0x%.08X to 0x%.08X', [UIntPtr(pdwPushData), UIntPtr(pdwPushDataEnd)]);

  LogPrefixStr := '';
  while pdwPushData <> pdwPushDataEnd do
  try
    if MayLog(lfUnit) then
      LogPrefixStr := Format('  NV2A Get=$%.08X', [UIntPtr(pdwPushData)]);

    dwPushCommand := pdwPushData^;

    // Handle jumps and/or calls :
    if ((dwPushCommand and NV2A_JMP_FLAG) > 0)
    or ((dwPushCommand and NV2A_CALL_FLAG) > 0) then
    begin
      // Both 'jump' and 'call' just direct execution to the indicated address :
      pdwPushData := PDWORD(dwPushCommand and NV2A_ADDR_MASK);
      if MayLog(lfUnit) then
        DbgPrintf('%s Jump:0x%.08X', [LogPrefixStr, UIntPtr(pdwPushData)]);

      Continue;
    end;

    // Decode push buffer contents (inverse of D3DPUSH_ENCODE) :
    D3DPUSH_DECODE(dwPushCommand, {out}dwMethod, {out}dwSubCh, {out}dwCount, {out}bNoInc);

    // Append a counter (variable part via %d, count already formatted) :
    if MayLog(lfUnit) then
    begin
      LogPrefixStr := LogPrefixStr + ' %2d/' + Format('%2d',[dwCount]) + ':';
      if dwSubCh > 0 then
        LogPrefixStr := LogPrefixStr + ' [SubCh:' + IntToStr(dwSubCh) + ']';

      if bNoInc then
        LogPrefixStr := LogPrefixStr + ' [NoInc]';
    end;

    // Skip method DWORD, remember the address of the arguments and skip over the arguments already :
    Inc(pdwPushData);
    pdwPushArguments := pdwPushData;
    Inc(pdwPushData, dwCount);

    // Initialize handled count & name to their default :
    HandledCount := 1;
    HandledBy := '';

    // Interpret GPU Instruction(s) :
    StepNr := 1;
    while dwCount > 0 do
    try
      NV2ACallback := nil;

      // Skip all commands not intended for channel 0 :
      if dwSubCh > 0 then
      begin
        HandledCount := dwCount;
        HandledBy := '*CHANNEL IGNORED*';
      end
      else
      begin
        // Simulate writes to the NV2A instance registers :
        if dwMethod < SizeOf(NV2AInstance) then
        begin
          // TODO : Perhaps we should write all DWORDs before executing them? (See EmuNV2A_SetVertexShaderConstant)
          OldRegisterValue := NV2AInstance.Registers[dwMethod div 4]; // Remember previous value
          NV2AInstance.Registers[dwMethod div 4] := pdwPushArguments^; // Write new value
          NV2ACallback := NV2ACallbacks[dwMethod div 4];
        end;
      end;

      // Note : The above statement covers all non-triggering data transfers (yeah!)
      // and makes them available as named variables too, since NV2AInstance is declared
      // with an overlay definition - which uses correct types where possible to reduce
      // the number of type-casts we need to do in here.

{$IFDEF DXBX_USE_D3D}
      if (g_pD3DDevice = nil) then
      begin
        HandledBy := '*NO DEVICE*'; // Don't do anything if we have no device yet (should not occur anymore, but this helps spotting errors)
        // Note : A Delphi bug prevents us from using 'Continue' here, which costs us an indent level...
        NV2ACallback := nil;
      end;
{$ENDIF}
{$IFDEF DXBX_USE_OPENGL}
      if (g_EmuWindowsDC = 0) then
      begin
        HandledBy := '*NO OGL DC*'; // Don't do anything if we have no device yet (should not occur anymore, but this helps spotting errors)
        // Note : A Delphi bug prevents us from using 'Continue' here, which costs us an indent level...
        NV2ACallback := nil;
      end;
{$ENDIF}

      if MayLog(lfUnit) then
      begin
        // Before handling the method, display it's details :
        DbgPrintf(LogPrefixStr + ' Method=%.04X Data=%.08X %s %s', [StepNr, dwMethod, pdwPushArguments^, DxbxXboxMethodToString(dwMethod), HandledBy]);
        HandledBy := '';
      end;

      if Assigned(NV2ACallback) then
        NV2ACallback();

    finally
      if MayLog(lfUnit) then
      begin
        // If there are more details, print them now :
        if HandledBy <> '' then
          DbgPrintf('  NV2A > ' + HandledBy);
      end;

      // Since some instructions use less arguments, we repeat this loop
      // for the next instruction so any leftover values are handled there :
      Inc(pdwPushArguments, HandledCount);
      Dec(dwCount, HandledCount);
      Inc(StepNr, HandledCount);

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

{$IFDEF DXBX_USE_OPENGL}
procedure SetupPixelFormat(DC: HDC);
const
   pfd: PIXELFORMATDESCRIPTOR = (
    nSize: SizeOf(PIXELFORMATDESCRIPTOR); // size
    nVersion: 1;   // version
    dwFlags: PFD_SUPPORT_OPENGL or PFD_DRAW_TO_WINDOW or PFD_DOUBLEBUFFER; // support double-buffering
    iPixelType: PFD_TYPE_RGBA; // color type
    cColorBits: 32;   // preferred color depth
    cRedBits: 0; cRedShift: 0; // color bits (ignored)
    cGreenBits: 0;  cGreenShift: 0;
    cBlueBits: 0; cBlueShift: 0;
    cAlphaBits: 0;  cAlphaShift: 0;   // no alpha buffer
    cAccumBits: 0;
    cAccumRedBits: 0;    // no accumulation buffer,
    cAccumGreenBits: 0;      // accum bits (ignored)
    cAccumBlueBits: 0;
    cAccumAlphaBits: 0;
    cDepthBits: 16;   // depth buffer
    cStencilBits: 0;   // no stencil buffer
    cAuxBuffers: 0;   // no auxiliary buffers
    iLayerType: PFD_MAIN_PLANE;   // main layer
    bReserved: 0;
    dwLayerMask: 0;
    dwVisibleMask: 0;
    dwDamageMask: 0;                    // no layer, visible, damage masks
    );
var
  PixelFormat: Integer;
begin
   pixelFormat := ChoosePixelFormat(DC, @pfd);
   if (PixelFormat = 0) then
     Exit;

   if (SetPixelFormat(DC, PixelFormat, @pfd) <> True) then
     Exit;
end;

procedure InitOpenGLContext();
var
  RC: HGLRC;
  szCode: AnsiString;
  GLErrorPos: int;
begin
  g_EmuWindowsDC := GetDC(g_hEmuWindow); // Actually, you can use any windowed control here
  SetupPixelFormat(g_EmuWindowsDC);

  RC := wglCreateContext(g_EmuWindowsDC); // makes OpenGL window out of DC
  wglMakeCurrent(g_EmuWindowsDC, RC);   // makes OpenGL window active
  ReadImplementationProperties; // Determine a set of booleans indicating which OpenGL extensions are available
  ReadExtensions; // Assign all OpenGL extension API's (DON'T call them if the extension is not available!)

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
//  glOrtho({Left=}0, {Right=}640, {Bottom=}480, {Top}0, {ZNear=}-1.0, {ZFar=}500);
//  //  glFrustum(-0.1, 0.1, -0.1, 0.1, 0.3, 25.0); ?
  glFrustum({Left=}-1.0, {Right=}1.0, {Bottom=}-1.0, {Top=}1.0, {ZNear=}0.0, {ZFar=}1.0);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  // Switch to left-handed coordinate space (as per http://www.opengl.org/resources/faq/technical/transformations.htm) :
  glScalef(1.0, 1.0, -1.0);

  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL); // Nearer Z coordinates cover further Z

  glViewport(0, 0,
    g_EmuCDPD.pPresentationParameters.BackBufferWidth,
    g_EmuCDPD.pPresentationParameters.BackBufferHeight);

  // Enable vertex shading
  glEnable(GL_VERTEX_PROGRAM_ARB);

  // TODO : The following code only works on cards that support the
  // vertex program extensions (NVidia cards mainly); So for ATI we
  // have to come up with another solution !!!
  glGenProgramsARB(4, @VertexProgramIDs[0]);

  // Precompiled shader for the fixed function pipeline :
  szCode := {AnsiString}DxbxVertexShaderHeader +
    // This part adjusts the vertex position by the super-sampling scale & offset :
    'MOV R0, v0;'#13#10 +
    'RCP R0.w, R0.w;'#13#10 +
    'MUL R0, R0, c0;'#13#10 + // c[-96] in D3D speak - applies SuperSampleScale
    // Note : Use R12 instead of oPos because this is not yet the final assignment :
    'ADD R12, R0, c1;'#13#10 + // c[-95] in D3D speak - applies SuperSampleOffset
    // This part just reads all other components and passes them to the output :
    'MOV oD0, v3;'#13#10 +
    'MOV oD1, v4;'#13#10 +
    'RCP oFog, v4.w;'#13#10 + // specular fog
//    'RCP oFog, v0.z;'#13#10 + // z fog
//    'RCP oFog, v0.w;'#13#10 + // w fog
    'MOV oPts, v1.x;'#13#10 +
    'MOV oB0, v7;'#13#10 +
    'MOV oB1, v8;'#13#10 +
    'MOV oT0, v9;'#13#10 +
    'MOV oT1, v10;'#13#10 +
    'MOV oT2, v11;'#13#10 +
    'MOV oT3, v12;'#13#10 +
    // This part applies the screen-space transform :
    'MUL R12.xyz, R12, c58;'#13#10 + // c[-38] in D3D speak - see EmuNV2A_ViewportScale,
    'RCP R1.x, R12.w;'#13#10 + // Originally RCC, but that's not supported in ARBvp1.0
    // Note : Here's the final assignment to oPos :
    'MAD oPos.xyz, R12, R1.x, c59;'#13#10 + // c[-37] in D3D speak - see EmuNV2A_ViewportOffset

    'END';

  glBindProgramARB(GL_VERTEX_PROGRAM_ARB, VertexProgramIDs[1]);

  glProgramStringARB(GL_VERTEX_PROGRAM_ARB, GL_PROGRAM_FORMAT_ASCII_ARB, Length(szCode), Pointer(szCode));

  // errors are catched
  glGetIntegerv(GL_PROGRAM_ERROR_POSITION_ARB, @GLErrorPos);

  if(GLErrorPos > 0) then
  begin
    Insert('{ERROR}', {var}szCode, GLErrorPos);
    EmuWarning('Program error at position %d:', [GLErrorPos]);
    EmuWarning(string(glGetString(GL_PROGRAM_ERROR_STRING_ARB)));
    EmuWarning(string(szCode));
  end;

  glDisable(GL_VERTEX_PROGRAM_ARB);
end;
{$ENDIF}

// timing thread procedure
function EmuThreadHandleNV2ADMA(lpVoid: LPVOID): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  UpdateTimer: DxbxTimer;
  NV2ADMAChannel: PNv2AControlDma;
  pNV2AWorkTrigger: PDWORD;
  Pusher: PPusher;
  GPUStart, GPUEnd: PDWord;
begin
  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : NV2A DMA thread is running.');

  DxbxLogPushBufferPointers('NV2AThread');

{$IFDEF DXBX_USE_OPENGL}
  // The OpenGL context must be created in the same thread that's going to do all drawing
  // (in this case it's the NV2A push buffer emulator thread doing all the drawing) :
  InitOpenGLContext();
{$ENDIF}

  UpdateTimer.InitFPS(100); // 100 updates per second should be enough

  Pusher := PPusher(PPointer(XTL_D3D__Device)^);
  NV2ADMAChannel := g_NV2ADMAChannel;
  pNV2AWorkTrigger := PDWORD(GPURegisterBase + NV2A_PFB_WC_CACHE);

  // Emulate the GPU engine here, by running the pushbuffer on the correct addresses :
  while true do // TODO -oDxbx: When do we break out of this while loop ?
  begin
    UpdateTimer.Wait;

    // Check that KickOff() signaled a work flush :
    begin
      if (pNV2AWorkTrigger^ and NV2A_PFB_WC_CACHE_FLUSH_TRIGGER) > 0 then
      begin
        // Reset the flush trigger, so that KickOff() continues :
        pNV2AWorkTrigger^ := pNV2AWorkTrigger^ and (not NV2A_PFB_WC_CACHE_FLUSH_TRIGGER);
        DxbxLogPushBufferPointers('NV2AThread work trigger');
      end;
    end;

    // Start at the DMA's 'Put' address, and assume we'll run
    // up to the end of the pushbuffer (as filled by software) :
    GPUStart := NV2ADMAChannel.Get;
    if GPUStart = nil then
      GPUStart := NV2ADMAChannel.Put;

    GPUEnd := Pusher.m_pPut;

    // See if there's a valid work pointer :
    if Assigned(GPUStart) and Assigned(GPUEnd) then
      // Execute the instructions, this returns the address where execution stopped :
      XTL_EmuExecutePushBufferRaw(GPUStart, GPUEnd);
  end; // while
end; // EmuThreadHandleNV2ADMA

initialization
  ClearVariables; // TODO : Delay this

end.

