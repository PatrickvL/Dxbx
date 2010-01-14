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

unit uVertexShader;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows
  // Jedi
  , JwaWinType
  // DirectX
  , D3DX8
  , Direct3D8
  // Dxbx
  , uEmu
  , uTypes
  , uEmuD3D8Types
  , uEmuAlloc;


const
  VSH_INSTRUCTION_SIZE = 4;
  VSH_INSTRUCTION_SIZE_BYTES = VSH_INSTRUCTION_SIZE * SizeOf(DWORD);
  VSH_MAX_INTERMEDIATE_COUNT = 1024; // The maximum number of intermediate format slots

  VERSION_VS =                      $F0; // vs.1.1, not an official value
  VERSION_XVS =                     $20; // Xbox vertex shader
  VERSION_XVSS =                    $73; // Xbox vertex state shader
  VERSION_XVSW =                    $77; // Xbox vertex read/write shader
  VSH_XBOX_MAX_INSTRUCTION_COUNT =  136;  // The maximum Xbox shader instruction count


type
  LPD3DXBUFFER = ID3DXBuffer; // Dxbx TODO : Move to better location.
  PLPD3DXBUFFER = ^LPD3DXBUFFER;

  _VSH_SHADER_HEADER = packed record
    aType: uint08;
    Version: uint08;
    NumInst: uint08;
    Unknown0: uint08;
  end;
  VSH_SHADER_HEADER = _VSH_SHADER_HEADER;
  PVSH_SHADER_HEADER = ^VSH_SHADER_HEADER;

  _VSH_TYPE_PATCH_DATA = packed record
    NbrTypes: DWORD;
    Types: array [0..255] of UINT;
  end;
  VSH_TYPE_PATCH_DATA = _VSH_TYPE_PATCH_DATA;

  _VSH_STREAM_PATCH_DATA = packed record
    NbrStreams: DWORD;
    pStreamPatches: array [0..255] of STREAM_DYNAMIC_PATCH;
  end;
  VSH_STREAM_PATCH_DATA = _VSH_STREAM_PATCH_DATA;

  _VSH_PATCH_DATA = packed record
    NeedPatching: boolean;
    ConvertedStride: DWORD;
    TypePatchData: VSH_TYPE_PATCH_DATA;
    StreamPatchData: VSH_STREAM_PATCH_DATA;
  end;
  VSH_PATCH_DATA = _VSH_PATCH_DATA;
  PVSH_PATCH_DATA = ^VSH_PATCH_DATA;

  _VSH_IMD_INSTRUCTION_TYPE = (IMD_MAC,IMD_ILU);
  VSH_IMD_INSTRUCTION_TYPE = _VSH_IMD_INSTRUCTION_TYPE;

  // Local types
  _VSH_FIELD_NAME = (
    FLD_ILU = 0,
    FLD_MAC,
    FLD_CONST,
    FLD_V,
    // Input A
    FLD_A_NEG,
    FLD_A_SWZ_X,
    FLD_A_SWZ_Y,
    FLD_A_SWZ_Z,
    FLD_A_SWZ_W,
    FLD_A_R,
    FLD_A_MUX,
    // Input B
    FLD_B_NEG,
    FLD_B_SWZ_X,
    FLD_B_SWZ_Y,
    FLD_B_SWZ_Z,
    FLD_B_SWZ_W,
    FLD_B_R,
    FLD_B_MUX,
    // Input C
    FLD_C_NEG,
    FLD_C_SWZ_X,
    FLD_C_SWZ_Y,
    FLD_C_SWZ_Z,
    FLD_C_SWZ_W,
    FLD_C_R_HIGH,
    FLD_C_R_LOW,
    FLD_C_MUX,
    // Output
    FLD_OUT_MAC_MASK_X,
    FLD_OUT_MAC_MASK_Y,
    FLD_OUT_MAC_MASK_Z,
    FLD_OUT_MAC_MASK_W,
    FLD_OUT_R,
    FLD_OUT_ILU_MASK_X,
    FLD_OUT_ILU_MASK_Y,
    FLD_OUT_ILU_MASK_Z,
    FLD_OUT_ILU_MASK_W,
    FLD_OUT_O_MASK_X,
    FLD_OUT_O_MASK_Y,
    FLD_OUT_O_MASK_Z,
    FLD_OUT_O_MASK_W,
    FLD_OUT_ORB,
    FLD_OUT_ADDRESS,
    FLD_OUT_MUX,
    // Relative addressing
    FLD_A0X,
    // Final instruction
    FLD_FINAL
  );
  VSH_FIELD_NAME = _VSH_FIELD_NAME;

  _VSH_OREG_NAME = (
    OREG_OPOS,
    OREG_UNUSED1,
    OREG_UNUSED2,
    OREG_OD0,
    OREG_OD1,
    OREG_OFOG,
    OREG_OPTS,
    OREG_OB0,
    OREG_OB1,
    OREG_OT0,
    OREG_OT1,
    OREG_OT2,
    OREG_OT3,
    OREG_UNUSED3,
    OREG_UNUSED4,
    OREG_A0X
  );
  VSH_OREG_NAME = _VSH_OREG_NAME;

  _VSH_PARAMETER_TYPE = (PARAM_UNKNOWN = 0,
                         PARAM_R,
                         PARAM_V,
                         PARAM_C);
  VSH_PARAMETER_TYPE = _VSH_PARAMETER_TYPE;

  _VSH_IMD_OUTPUT_TYPE = (IMD_OUTPUT_C,
                          IMD_OUTPUT_R,
                          IMD_OUTPUT_O,
                          IMD_OUTPUT_A0X);
  VSH_IMD_OUTPUT_TYPE = _VSH_IMD_OUTPUT_TYPE;

  _VSH_IMD_OUTPUT = packed record
    aType: VSH_IMD_OUTPUT_TYPE;
    Mask: array [0..3] of boolean;
    Address: UInt16;
  end;
  VSH_IMD_OUTPUT = _VSH_IMD_OUTPUT;

  _VSH_OUTPUT_TYPE = (
    OUTPUT_C = 0,
    OUTPUT_O
  );
  VSH_OUTPUT_TYPE = _VSH_OUTPUT_TYPE;

  _VSH_OUTPUT_MUX = (
    OMUX_MAC = 0,
    OMUX_ILU
  );
  VSH_OUTPUT_MUX = _VSH_OUTPUT_MUX;

  _VSH_ILU = (ILU_NOP = 0,
              ILU_MOV,
              ILU_RCP,
              ILU_RCC,
              ILU_RSQ,
              ILU_EXP,
              ILU_LOG,
              ILU_LIT);
  VSH_ILU = _VSH_ILU;

  _VSH_MAC = (MAC_NOP,
              MAC_MOV,
              MAC_MUL,
              AC_ADD,
              MAC_MAD,
              MAC_DP3,
              MAC_DPH,
              MAC_DP4,
              MAC_DST,
              MAC_MIN,
              MAC_MAX,
              MAC_SLT,
              MAC_SGE,
              MAC_ARL);
  VSH_MAC = _VSH_MAC;

  _VSH_OPCODE_PARAMS = packed record
    ILU: VSH_ILU;
    MAC: VSH_MAC;
    A: boolean;
    B: boolean;
    C: boolean;
  end;
  VSH_OPCODE_PARAMS = _VSH_OPCODE_PARAMS;

  _VSH_SWIZZLE = (SWIZZLE_X = 0,
                  SWIZZLE_Y,
                  SWIZZLE_Z,
                  SWIZZLE_W);
  VSH_SWIZZLE = _VSH_SWIZZLE;

  _VSH_PARAMETER = packed record
    ParameterType: VSH_PARAMETER_TYPE;   // Parameter type, R, V or C
    Neg: boolean;             // TRUE if negated, FALSE if not
    Swizzle: array [0..4-1] of VSH_SWIZZLE;      // The four swizzles
    Address: int16;         // Register address
  end;
  VSH_PARAMETER = _VSH_PARAMETER;

  _VSH_OUTPUT = packed record
    // Output register
    OutputMux: VSH_OUTPUT_MUX;       // MAC or ILU used as output
    OutputType: VSH_OUTPUT_TYPE;      // C or O
    OutputMask: array [0..4-1] of boolean;
    OutputAddress: int16;
    // MAC output R register
    MACRMask: array [0..4-1] of boolean;
    MACRAddress: boolean;
    // ILU output R register
    ILURMask: array [0..4-1] of boolean;
    ILURAddress: boolean;
  end;
  VSH_OUTPUT = _VSH_OUTPUT;

  // The raw, parsed shader instruction (can be many combined [paired] instructions)
  _VSH_SHADER_INSTRUCTION = packed record
    ILU: VSH_ILU;
    MAC: VSH_MAC;
    Output: VSH_OUTPUT;
    A: VSH_PARAMETER;
    B: VSH_PARAMETER;
    C: VSH_PARAMETER;
    a0x: boolean;
  end;
  VSH_SHADER_INSTRUCTION = _VSH_SHADER_INSTRUCTION;

  _VSH_IMD_PARAMETER = packed record
    Active: boolean;
    Parameter: VSH_PARAMETER;
    IsA0X: boolean;
  end;
  VSH_IMD_PARAMETER = _VSH_IMD_PARAMETER;

  _VSH_INTERMEDIATE_FORMAT = packed record
    IsCombined: boolean;
    InstructionType: VSH_IMD_INSTRUCTION_TYPE;
    MAC: VSH_MAC;
    ILU: VSH_ILU;
    Output: VSH_IMD_OUTPUT;
    Parameters: array [0..2] of VSH_IMD_PARAMETER;
  end;
  VSH_INTERMEDIATE_FORMAT = _VSH_INTERMEDIATE_FORMAT;

  _VSH_XBOX_SHADER = packed record
    ShaderHeader: VSH_SHADER_HEADER;
    IntermediateCount: uint16;
    Intermediate: array [0..VSH_MAX_INTERMEDIATE_COUNT -1] of VSH_INTERMEDIATE_FORMAT ;
  end;
  VSH_XBOX_SHADER = _VSH_XBOX_SHADER;
  PVSH_XBOX_SHADER = ^VSH_XBOX_SHADER;

function XTL_IsValidCurrentShader: Boolean; stdcall; // forward
procedure XTL_FreeVertexDynamicPatch(pVertexShader: PVERTEX_SHADER) stdcall;

function VshHandleIsVertexShader(aHandle: DWORD): Boolean;
function VshHandleGetVertexShader(aHandle: DWORD): PX_D3DVertexShader;
function VshGetVertexDynamicPatch(Handle: DWORD): PVERTEX_DYNAMIC_PATCH;

function XTL_EmuRecompileVshDeclaration
(
  pDeclaration: PDWORD;
  ppRecompiledDeclaration: PPDWORD;
  pDeclarationSize: PDWORD;
  IsFixedFunction: Boolean;
  pVertexDynamicPatch: PVERTEX_DYNAMIC_PATCH
): DWORD;

function XTL_EmuRecompileVshFunction
(
    pFunction: PDWORD;
    ppRecompiled: PLPD3DXBUFFER;
    pOriginalSize: PDWORD;
    bNoReservedConstants: boolean
) : HRESULT; stdcall;

function VshGetDeclarationSize(pDeclaration: PDWord): DWORD;
function VshRecompileToken(pToken: PDWord; IsFixedFunction: boolean; pPatchData: PVSH_PATCH_DATA): DWORD;


const
  DEF_VSH_END = $FFFFFFFF;
  DEF_VSH_NOP = $00000000;

implementation

uses
  // Dxbx
  uEmuFS
  , uLog
  , uEmuD3D8;

procedure DbgVshPrintf(aStr: string); overload;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
{$ifdef _DEBUG_TRACK_VS}
  if (g_bPrintfOn) then
{$endif}
    DbgPrintf(aStr);
end;

procedure DbgVshPrintf(aStr: string; Args: array of const); overload;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
{$ifdef _DEBUG_TRACK_VS}
  if (g_bPrintfOn) then
    DbgVshPrintf(DxbxFormat(aStr, Args)));
{$endif}
end;

function VshGetDeclarationSize(pDeclaration: PDWord): DWORD;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  Pos: DWORD;
begin
  Pos := 0;
  while PDWord(UIntPtr(pDeclaration) + Pos)^ <> DEF_VSH_END do
    Inc(Pos);

  Result := (Pos + 1) * SizeOf(DWORD);
end;

function Xb2PCRegisterType(VertexRegister: DWORD): DWORD;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
var
  PCRegisterType: Integer;
begin
  case Integer(VertexRegister) of
  -1: begin
      DbgVshPrintf('D3DVSDE_VERTEX /* xbox ext. */');
      PCRegisterType := -1;
    end;
  0: begin
      DbgVshPrintf('D3DVSDE_POSITION');
      PCRegisterType := D3DVSDE_POSITION;
    end;
  1: begin
      DbgVshPrintf('D3DVSDE_BLENDWEIGHT');
      PCRegisterType := D3DVSDE_BLENDWEIGHT;
    end;
  2: begin
      DbgVshPrintf('D3DVSDE_NORMAL');
      PCRegisterType := D3DVSDE_NORMAL;
    end;
  3: begin
      DbgVshPrintf('D3DVSDE_DIFFUSE');
      PCRegisterType := D3DVSDE_DIFFUSE;
    end;
  4: begin
      DbgVshPrintf('D3DVSDE_SPECULAR');
      PCRegisterType := D3DVSDE_SPECULAR;
    end;
  5: begin
      DbgVshPrintf('D3DVSDE_FOG /* xbox ext. */');
      PCRegisterType := -1;
    end;
  7: begin
      DbgVshPrintf('D3DVSDE_BACKDIFFUSE /* xbox ext. */');
      PCRegisterType := -1;
    end;
  8: begin
      DbgVshPrintf('D3DVSDE_BACKSPECULAR /* xbox ext. */');
      PCRegisterType := -1;
    end;
  9: begin
      DbgVshPrintf('D3DVSDE_TEXCOORD0');
      PCRegisterType := D3DVSDE_TEXCOORD0;
    end;
  10: begin
      DbgVshPrintf('D3DVSDE_TEXCOORD1');
      PCRegisterType := D3DVSDE_TEXCOORD1;
    end;
  11: begin
      DbgVshPrintf('D3DVSDE_TEXCOORD2');
      PCRegisterType := D3DVSDE_TEXCOORD2;
    end;
  12: begin
      DbgVshPrintf('D3DVSDE_TEXCOORD3');
      PCRegisterType := D3DVSDE_TEXCOORD3;
    end;
  else
    DbgVshPrintf('%d /* unknown register */', [VertexRegister]);
    PCRegisterType := -1;
  end;

  Result := DWord(PCRegisterType);
end; // Xb2PCRegisterType

function VshGetTokenType(Token: DWORD): DWORD;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  Result := (Token and D3DVSD_TOKENTYPEMASK) shr D3DVSD_TOKENTYPESHIFT;
end;

function VshGetVertexRegister(Token: DWORD): DWORD; inline;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  Result := (Token and D3DVSD_VERTEXREGMASK) shr D3DVSD_VERTEXREGSHIFT;
end;

function VshGetVertexRegisterIn(Token: DWORD): DWORD; inline;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  Result := (Token and D3DVSD_VERTEXREGINMASK) shr D3DVSD_VERTEXREGINSHIFT;
end;

function VshGetVertexStream(Token: DWORD): DWORD; inline;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  Result := (Token and D3DVSD_STREAMNUMBERMASK) shr D3DVSD_STREAMNUMBERSHIFT;
end;

procedure VshConvertToken_NOP(pToken: PDWORD);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // D3DVSD_NOP
  if (pToken^ <> DEF_VSH_NOP) then
  begin
    EmuWarning('Token NOP found, but extra parameters are given!');
  end;
  DbgVshPrintf(#9'D3DVSD_NOP(),');
end;

function VshConvertToken_CONSTMEM(pToken: PDWORD): DWORD;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
var
  ConstantAddress: DWORD;
  Count: DWORD;
  i: int;
begin
  // D3DVSD_CONST
  ConstantAddress := ((pToken^ shr D3DVSD_CONSTADDRESSSHIFT) and $FF);
  Count           := (pToken^ and D3DVSD_CONSTCOUNTMASK) shr D3DVSD_CONSTCOUNTSHIFT;

  DbgVshPrintf(#9'D3DVSD_CONST(%d, %d),', [ConstantAddress, Count]);

  //pToken = D3DVSD_CONST(ConstantAddress, Count);

  for i := 0 to Count - 1 do
    DbgVshPrintf(#9'0x%08X,', [pToken]);

  Result := Count;
end; // VshConvertToken_CONSTMEM

procedure VshConverToken_TESSELATOR(pToken: PDWORD; IsFixedFunction: boolean);
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  VertexRegister: DWORD;
  NewVertexRegister: DWORD;
  VertexRegisterIn: DWORD;
  VertexRegisterOut: DWORD;
  NewVertexRegisterIn: DWORD;
  NewVertexRegisterOut: DWORD;
begin
  // Cxbx TODO: Investigate why Xb2PCRegisterType is only used for fixed function vertex shaders
  // D3DVSD_TESSUV
  if (pToken^ and $10000000) > 0 then
  begin
    VertexRegister    := VshGetVertexRegister(pToken^);
    NewVertexRegister := VertexRegister;

    DbgVshPrintf(#9'D3DVSD_TESSUV(');

    if (IsFixedFunction) then
    begin
      NewVertexRegister := Xb2PCRegisterType(VertexRegister);
    end
    else
    begin
      DbgVshPrintf('%d', [NewVertexRegister]);
    end;

    DbgVshPrintf('),');

    pToken^ := D3DVSD_TESSUV(NewVertexRegister);
  end
  // D3DVSD_TESSNORMAL
  else
  begin
    VertexRegisterIn  := VshGetVertexRegisterIn(pToken^);
    VertexRegisterOut := VshGetVertexRegister(pToken^);

    NewVertexRegisterIn  := VertexRegisterIn;
    NewVertexRegisterOut := VertexRegisterOut;

    DbgVshPrintf(#9'D3DVSD_TESSNORMAL(');

    if (IsFixedFunction) then
    begin
      NewVertexRegisterIn := Xb2PCRegisterType(VertexRegisterIn);
    end
    else
    begin
      DbgVshPrintf('%d', [NewVertexRegisterIn]);
    end;

    DbgVshPrintf(', ');

    if (IsFixedFunction) then
    begin
      NewVertexRegisterOut := Xb2PCRegisterType(VertexRegisterOut);
    end
    else
    begin
      DbgVshPrintf('%d', [NewVertexRegisterOut]);
    end;

    DbgVshPrintf('),');
    pToken^ := D3DVSD_TESSNORMAL(NewVertexRegisterIn, NewVertexRegisterOut);
  end;
end; // VshConverToken_TESSELATOR

function VshAddStreamPatch(pPatchData: PVSH_PATCH_DATA): boolean;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  CurrentStream: int;
  pStreamPatch: STREAM_DYNAMIC_PATCH;
begin
  CurrentStream := pPatchData.StreamPatchData.NbrStreams - 1;

  if (CurrentStream >= 0) then
  begin
    DbgVshPrintf('NeedPatching: %s', [pPatchData.NeedPatching]);

    pStreamPatch := pPatchData.StreamPatchData.pStreamPatches[CurrentStream];

    pStreamPatch.ConvertedStride := pPatchData.ConvertedStride;
    pStreamPatch.NbrTypes := pPatchData.TypePatchData.NbrTypes;
    pStreamPatch.NeedPatch := pPatchData.NeedPatching;
    pStreamPatch.pTypes := CxbxMalloc(pPatchData.TypePatchData.NbrTypes * SizeOf(VSH_TYPE_PATCH_DATA));
    move(pPatchData.TypePatchData.Types, pStreamPatch.pTypes, pPatchData.TypePatchData.NbrTypes * SizeOf(VSH_TYPE_PATCH_DATA));

    Result := TRUE;
    Exit;
  end;

  Result := FALSE;
end; // VshAddStreamPatch

procedure VshConvertToken_STREAM(pToken: PDWORD; pPatchData: PVSH_PATCH_DATA);
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  StreamNumber: DWORD;
begin
  // D3DVSD_STREAM_TESS
  if (pToken^ and D3DVSD_STREAMTESSMASK) > 0 then
  begin
    DbgVshPrintf(#9'D3DVSD_STREAM_TESS(),');
  end
  // D3DVSD_STREAM
  else
  begin
    StreamNumber := VshGetVertexStream(pToken^);
    DbgVshPrintf(#9'D3DVSD_STREAM(%d),', [StreamNumber]);

    // new stream
    // copy current data to structure
    if (VshAddStreamPatch(pPatchData)) then
    begin
      pPatchData.ConvertedStride := 0;
      pPatchData.TypePatchData.NbrTypes := 0;
      pPatchData.NeedPatching := False;
    end;

    Inc(pPatchData.StreamPatchData.NbrStreams);
  end;
end; // VshConvertToken_STREAM

procedure VshConvertToken_STREAMDATA_SKIP(pToken: PDWORD);
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
var
  SkipCount: DWORD;
begin
  SkipCount := (pToken^ and D3DVSD_SKIPCOUNTMASK) shr D3DVSD_SKIPCOUNTSHIFT;
  DbgVshPrintf(#9'D3DVSD_SKIP(%d),\n', [SkipCount]);
end;

procedure VshConvertToken_STREAMDATA_SKIPBYTES(pToken: PDWORD);
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
var
  SkipBytesCount: DWORD;
begin
  SkipBytesCount := (pToken^ and D3DVSD_SKIPCOUNTMASK) shr D3DVSD_SKIPCOUNTSHIFT;
  DbgVshPrintf(#9'D3DVSD_SKIPBYTES(%d), /* xbox ext. */', [SkipBytesCount]);
  if (SkipBytesCount mod SizeOf(DWORD)) > 0 then
  begin
    EmuWarning('D3DVSD_SKIPBYTES can''t be converted to D3DVSD_SKIP, not divisble by 4.');
  end;
  pToken^ := D3DVSD_SKIP(SkipBytesCount div SizeOf(DWORD));
end;

procedure VshConvertToken_STREAMDATA_REG(
  pToken: PDWORD;
  IsFixedFunction: Boolean;
  pPatchData: PVSH_PATCH_DATA);
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
var
  VertexRegister: DWORD ;
  NewVertexRegister: DWORD;
  DataType: DWORD;
  NewDataType: DWORD;
begin
  DbgVshPrintf(#9'D3DVSD_REG(');

  VertexRegister := VshGetVertexRegister(pToken^);

  if (IsFixedFunction) then
  begin
    NewVertexRegister := Xb2PCRegisterType(VertexRegister);
  end
  else
  begin
    NewVertexRegister := VertexRegister;
    DbgVshPrintf('%d', [NewVertexRegister]);
  end;

  DbgVshPrintf(', ');

  DataType := (pToken^ shr D3DVSD_DATATYPESHIFT) and $FF;
  NewDataType := 0;

  // save patching information
  pPatchData.TypePatchData.Types[pPatchData.TypePatchData.NbrTypes] := DataType;
  Inc(pPatchData.TypePatchData.NbrTypes);

  case (DataType) of
    $12: begin
      DbgVshPrintf('D3DVSDT_FLOAT1');
      NewDataType := D3DVSDT_FLOAT1;
      Inc(pPatchData.ConvertedStride, SizeOf(FLOAT));
    end;
    $22: begin
      DbgVshPrintf('D3DVSDT_FLOAT2');
      NewDataType := D3DVSDT_FLOAT2;
      Inc(pPatchData.ConvertedStride, 2*SizeOf(FLOAT));
    end;
    $32: begin
      DbgVshPrintf('D3DVSDT_FLOAT3');
      NewDataType := D3DVSDT_FLOAT3;
      Inc(pPatchData.ConvertedStride, 3*SizeOf(FLOAT));
    end;
    $42: begin
      DbgVshPrintf('D3DVSDT_FLOAT4');
      NewDataType := D3DVSDT_FLOAT4;
      Inc(pPatchData.ConvertedStride, 4*SizeOf(FLOAT));
    end;
    $40: begin
      DbgVshPrintf('D3DVSDT_D3DCOLOR');
      NewDataType := D3DVSDT_D3DCOLOR;
      Inc(pPatchData.ConvertedStride, SizeOf(D3DCOLOR));
    end;
    $25: begin
      DbgVshPrintf('D3DVSDT_SHORT2');
      NewDataType := D3DVSDT_SHORT2;
      Inc(pPatchData.ConvertedStride, 2*SizeOf(SHORT));
    end;
    $45: begin
      DbgVshPrintf('D3DVSDT_SHORT4');
      NewDataType := D3DVSDT_SHORT4;
      Inc(pPatchData.ConvertedStride, 4*SizeOf(SHORT));
    end;
    $11: begin
      DbgVshPrintf('D3DVSDT_NORMSHORT1 /* xbox ext. */');
      NewDataType := D3DVSDT_SHORT2; // hmm, emulation?
      Inc(pPatchData.ConvertedStride, 2*SizeOf(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    $21: begin
      DbgVshPrintf('D3DVSDT_NORMSHORT2 /* xbox ext. */');
      NewDataType := D3DVSDT_SHORT2;
      Inc(pPatchData.ConvertedStride, 2*SizeOf(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    $31: begin
      DbgVshPrintf('D3DVSDT_NORMSHORT3 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_SHORT4;
      Inc(pPatchData.ConvertedStride, 4*SizeOf(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    $41: begin
      DbgVshPrintf('D3DVSDT_NORMSHORT4 /* xbox ext. */');
      NewDataType := D3DVSDT_SHORT4;
      Inc(pPatchData.ConvertedStride, 4*SizeOf(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    $16: begin
      DbgVshPrintf('D3DVSDT_NORMPACKED3 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_FLOAT3;//$FF; //32bit
      Inc(pPatchData.ConvertedStride, 3*SizeOf(FLOAT));
      pPatchData.NeedPatching := TRUE;
    end;
    $15: begin
      DbgVshPrintf('D3DVSDT_SHORT1 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_SHORT2;
      Inc(pPatchData.ConvertedStride, 2*SizeOf(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    $35: begin
      DbgVshPrintf('D3DVSDT_SHORT3 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_SHORT4;
      Inc(pPatchData.ConvertedStride, 4*SizeOf(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    $14: begin
      DbgVshPrintf('D3DVSDT_PBYTE1 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_FLOAT1;
      Inc(pPatchData.ConvertedStride, 1*SizeOf(FLOAT));
      pPatchData.NeedPatching := TRUE;
    end;
    $24: begin
      DbgVshPrintf('D3DVSDT_PBYTE2 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_FLOAT2;
      Inc(pPatchData.ConvertedStride, 2*SizeOf(FLOAT));
      pPatchData.NeedPatching := TRUE;
    end;
    $34: begin
      DbgVshPrintf('D3DVSDT_PBYTE3 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_FLOAT3;
      Inc(pPatchData.ConvertedStride, 3*SizeOf(FLOAT));
      pPatchData.NeedPatching := TRUE;
    end;
    $44: begin
      DbgVshPrintf('D3DVSDT_PBYTE4 /* xbox ext. */');
      NewDataType := D3DVSDT_FLOAT4;
      Inc(pPatchData.ConvertedStride, 4*SizeOf(FLOAT));
    end;
    $72: begin
      DbgVshPrintf('D3DVSDT_FLOAT2H /* xbox ext. */');
      NewDataType := D3DVSDT_FLOAT3;
      Inc(pPatchData.ConvertedStride, 3*SizeOf(FLOAT));
      pPatchData.NeedPatching := TRUE;
    end;
    $02: begin
      DbgVshPrintf('D3DVSDT_NONE /* xbox ext. nsp */');
      NewDataType := $FF;
    end;
  else
    DbgVshPrintf('Unknown data type for D3DVSD_REG: $%02X', [DataType]);
  end;
  pToken^ := D3DVSD_REG(NewVertexRegister, NewDataType);

  DbgVshPrintf('),');

  if (NewDataType = $FF) then
  begin
    EmuWarning('/* WARNING: Fatal type mismatch, no fitting type! */');
  end;
end; // VshConvertToken_STREAMDATA_REG

procedure VshConvertToken_STREAMDATA(
  pToken: PDWORD;
  IsFixedFunction: boolean;
  pPatchData: PVSH_PATCH_DATA);
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  // D3DVSD_SKIP
  if (pToken^ and $10000000) > 0 then
  begin
    VshConvertToken_STREAMDATA_SKIP(pToken);
  end
  // D3DVSD_SKIPBYTES
  else if (pToken^ and $18000000) > 0 then
  begin
    VshConvertToken_STREAMDATA_SKIPBYTES(pToken);
  end
  // D3DVSD_REG
  else
  begin
    VshConvertToken_STREAMDATA_REG(pToken, IsFixedFunction, pPatchData);
  end;
end; // VshConvertToken_STREAMDATA

function VshRecompileToken(pToken: PDWord; IsFixedFunction: boolean; pPatchData: PVSH_PATCH_DATA): DWORD;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  Step: DWORD;
begin
  Step := 1;

  case TD3DVSDTokenType(VshGetTokenType(pToken^)) of
    D3DVSD_TOKEN_NOP:
      VshConvertToken_NOP(pToken);
    D3DVSD_TOKEN_STREAM:
      VshConvertToken_STREAM(pToken, pPatchData);
    D3DVSD_TOKEN_STREAMDATA:
      VshConvertToken_STREAMDATA(pToken, IsFixedFunction, pPatchData);
    D3DVSD_TOKEN_TESSELLATOR:
      VshConverToken_TESSELATOR(pToken, IsFixedFunction);
    D3DVSD_TOKEN_CONSTMEM:
      Step := VshConvertToken_CONSTMEM(pToken);
  else
    DbgVshPrintf('Unknown token type: %d', [VshGetTokenType(pToken^)]);
  end;

  Result := Step;
end; // VshRecompileToken

function XTL_EmuRecompileVshDeclaration
(
  pDeclaration: PDWORD;
  ppRecompiledDeclaration: PPDWORD;
  pDeclarationSize: PDWORD;
  IsFixedFunction: Boolean;
  pVertexDynamicPatch: PVERTEX_DYNAMIC_PATCH
) : DWORD;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  DeclarationSize: DWORD;
  pRecompiled: PDWord;
  PatchData: VSH_PATCH_DATA;
  Step: DWORD;
  StreamsSize: DWORD;
begin
  // First of all some info:
  // We have to figure out which flags are set and then
  // we have to patch their params

  // some token values
  // 0xFFFFFFFF - end of the declaration
  // 0x00000000 - nop (means that this value is ignored)

  // Calculate size of declaration
  DeclarationSize := VshGetDeclarationSize(pDeclaration);
  ppRecompiledDeclaration^ := PDWORD(CxbxMalloc(DeclarationSize));
  pRecompiled := ppRecompiledDeclaration^;

  memcpy(pRecompiled, pDeclaration, DeclarationSize);
  pDeclarationSize^ := DeclarationSize;

  // Cxbx TODO: Put these in one struct
  ZeroMemory(@PatchData, SizeOf(PatchData));

  DbgVshPrintf('DWORD dwVSHDecl[] = '#13#10'{');

  while pRecompiled^ <> DEF_VSH_END do
  begin
    Step := VshRecompileToken(pRecompiled, IsFixedFunction, @PatchData);
    Inc(UIntPtr(pRecompiled), Step);
  end;
  DbgVshPrintf(#9'D3DVSD_END()'#13#10'};');

  VshAddStreamPatch(@PatchData);

  DbgVshPrintf('NbrStreams: %d', [PatchData.StreamPatchData.NbrStreams]);

  // Copy the patches to the vertex shader struct
  StreamsSize := PatchData.StreamPatchData.NbrStreams * SizeOf(STREAM_DYNAMIC_PATCH);
  pVertexDynamicPatch.NbrStreams := PatchData.StreamPatchData.NbrStreams;
  pVertexDynamicPatch.pStreamPatches := PSTREAM_DYNAMIC_PATCH(CxbxMalloc(StreamsSize));
  memcpy(pVertexDynamicPatch.pStreamPatches,
         @(PatchData.StreamPatchData.pStreamPatches[0]),
         StreamsSize);

  Result := D3D_OK;
end; // XTL_EmuRecompileVshDeclaration

function XTL_EmuRecompileVshFunction
(
    pFunction: PDWORD;
    ppRecompiled: PLPD3DXBUFFER;
    pOriginalSize: PDWORD;
    bNoReservedConstants: boolean
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:50
var
  pShaderHeader: PVSH_SHADER_HEADER;
  pToken: PDWord;
  EOI: boolean;
  Inst: VSH_SHADER_INSTRUCTION;
  pShader: PVSH_XBOX_SHADER;
  hRet: HRESULT;
  pShaderDisassembly: PAnsiChar;
begin
  pShaderHeader := PVSH_SHADER_HEADER(pFunction);
  EOI := false;
  pShader := PVSH_XBOX_SHADER(CxbxMalloc(SizeOf(VSH_XBOX_SHADER)));
  hRet := 0;

  // Cxbx TODO: support this situation..
  if not Assigned(pFunction) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  ppRecompiled := NULL;
  pOriginalSize := nil;
  if (not Assigned(pShader)) then
  begin
    EmuWarning('Couldn''t allocate memory for vertex shader conversion buffer');
    hRet := E_OUTOFMEMORY;
  end;
  memset(pShader, 0, SizeOf(VSH_XBOX_SHADER));
  pShader.ShaderHeader := pShaderHeader^;
  case (pShaderHeader.Version) of
    VERSION_XVS: ;
    VERSION_XVSS:
      begin
        EmuWarning('Might not support vertex state shaders?');
        hRet := E_FAIL;
      end;
    VERSION_XVSW:
      begin
        EmuWarning('Might not support vertex read/write shaders?');
        hRet := E_FAIL;
      end;
    else
      begin
        EmuWarning('Unknown vertex shader version 0x%02X', [pShaderHeader.Version]);
        hRet := E_FAIL;
      end;
  end;

  if (SUCCEEDED(hRet)) then
  begin
    pToken := PDWORD(UIntPtr(pFunction) + SizeOf(VSH_SHADER_HEADER));
    while not EOI do
    begin
(*
      VshParseInstruction(pToken, @Inst);
      VshConvertToIntermediate(@Inst, pShader);
      EOI := boolean(VshGetField(pToken, FLD_FINAL));
*)
      Inc(pToken, VSH_INSTRUCTION_SIZE);
    end;

     // The size of the shader is
     pOriginalSize^ := DWORD(pToken) - DWORD(pFunction);

     pShaderDisassembly := PAnsiChar(CxbxMalloc(pShader.IntermediateCount * 50)); // Should be plenty
     DbgVshPrintf('-- Before conversion --');
(*     VshWriteShader(pShader, pShaderDisassembly, FALSE);*)
     DbgVshPrintf('%s', [pShaderDisassembly]);
     DbgVshPrintf('-----------------------');

(*     VshConvertShader(pShader, bNoReservedConstants);
     VshWriteShader(pShader, pShaderDisassembly, TRUE);*)

     DbgVshPrintf('-- After conversion ---');
     DbgVshPrintf('%s', [pShaderDisassembly]);
     DbgVshPrintf('-----------------------');

(*
     hRet := D3DXAssembleShader(pShaderDisassembly,
                               strlen(pShaderDisassembly),
                               D3DXASM_SKIPVALIDATION,
                               NULL,
                               ppRecompiled,
                               NULL);
*)

    if (FAILED(hRet)) then
       EmuWarning('Couldn''t assemble recompiled vertex shader');

    CxbxFree(pShaderDisassembly);
  end;

  CxbxFree(pShader);
  Result := hRet;
end; // XTL_EmuRecompileVshFunction

procedure XTL_FreeVertexDynamicPatch(pVertexShader: PVERTEX_SHADER) stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  i: DWord;
begin
  for i := 0 to pVertexShader.VertexDynamicPatch.NbrStreams - 1 do
    CxbxFree(pVertexShader.VertexDynamicPatch.pStreamPatches[i].pTypes);

  CxbxFree(pVertexShader.VertexDynamicPatch.pStreamPatches);
  pVertexShader.VertexDynamicPatch.pStreamPatches := nil;
  pVertexShader.VertexDynamicPatch.NbrStreams := 0;
end;

// Checks for failed vertex shaders, and shaders that would need patching
function XTL_IsValidCurrentShader: Boolean; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  aHandle: DWORD;
  pVertexShader: PVERTEX_SHADER;
  pD3DVertexShader: PX_D3DVertexShader;
begin
  EmuSwapFS(fsWindows);
  XTL_EmuIDirect3DDevice8_GetVertexShader(@aHandle);
  EmuSwapFS(fsXbox);
  if (VshHandleIsVertexShader(aHandle)) then
  begin
    pD3DVertexShader := PX_D3DVertexShader(aHandle and $7FFFFFFF);
    pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);
    if (pVertexShader.Status <> 0) then
    begin
      Result := FALSE;
      Exit;
    end;
    { Cxbx has this disabled :
    for i := 0 to pVertexShader.VertexDynamicPatch.NbrStreams - 1 do
    begin
      if (pVertexShader.VertexDynamicPatch.pStreamPatches[i].NeedPatch) then
      begin
       // Just for caching purposes
        pVertexShader.Status := $80000001;
        Result := FALSE;
        Exit;
      end;
    end;
    }
  end;

  Result := True;
end; // XTL_IsValidCurrentShader

function VshHandleIsVertexShader(aHandle: DWORD): Boolean;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  Result := (aHandle and $8000000) <> 0;
end;

function VshHandleGetVertexShader(aHandle: DWORD): PX_D3DVertexShader;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := PX_D3DVertexShader(aHandle and $7FFFFFFF);
end;

function VshGetVertexDynamicPatch(Handle: DWORD): PVERTEX_DYNAMIC_PATCH;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pD3DVertexShader: PX_D3DVertexShader;
  pVertexShader: PVERTEX_SHADER;
  i: uint32;
begin
  pD3DVertexShader := VshHandleGetVertexShader(Handle);
  pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);

  for i := 0 to pVertexShader.VertexDynamicPatch.NbrStreams -1 do
  begin
    if (pVertexShader.VertexDynamicPatch.pStreamPatches[i].NeedPatch) then
    begin
      Result := @pVertexShader.VertexDynamicPatch;
      Exit;
    end;
  end;

  Result := Null;
end; // VshGetVertexDynamicPatch

exports
  XTL_IsValidCurrentShader,
  XTL_EmuRecompileVshFunction,
  XTL_FreeVertexDynamicPatch;

end.

