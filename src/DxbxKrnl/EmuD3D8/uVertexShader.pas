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

{$MINENUMSIZE 4} // Enums in this unit need to be 4 bytes !

interface

uses
  // Delphi
  Windows
  , SysUtils // strlen
  // Jedi Win32API
  , JwaWinType
  // DirectX
  , Direct3D8
  , D3DX8
  // Dxbx
  , uTypes
  , uDxbxKrnlUtils
  , uEmuD3D8Types
  , uEmuD3D8Utils
  , uEmuAlloc
  , uEmu;


type Dxbx4Booleans = array [0..4-1] of boolean;
  PDxbx4Booleans = ^Dxbx4Booleans;

// Types from VertexShader.h :

// nv2a microcode header
type _VSH_SHADER_HEADER = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    Type_: uint08;
    Version: uint08;
    NumInst: uint08;
    Unknown0: uint08;
  end; // size = 4 (as in Cxbx)
  VSH_SHADER_HEADER = _VSH_SHADER_HEADER;
  PVSH_SHADER_HEADER = ^VSH_SHADER_HEADER;

const VSH_INSTRUCTION_SIZE = 4;
const VSH_INSTRUCTION_SIZE_BYTES = VSH_INSTRUCTION_SIZE * sizeof(DWORD);

// Types from VertexShader.cpp :

// ****************************************************************************
// * Vertex shader function recompiler
// ****************************************************************************

// Local macros
const VERSION_VS =                      $F0;  // vs.1.1, not an official value
const VERSION_XVS =                     $20;  // Xbox vertex shader
const VERSION_XVSS =                    $73;  // Xbox vertex state shader
const VERSION_XVSW =                    $77;  // Xbox vertex read/write shader
const VSH_XBOX_MAX_INSTRUCTION_COUNT =  136;  // The maximum Xbox shader instruction count
const VSH_MAX_INTERMEDIATE_COUNT =      1024; // The maximum number of intermediate format slots

// Local types
type _VSH_FIELD_NAME = 
(
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

type _VSH_OREG_NAME = 
(
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
    OREG_A0X // = 15 - all values of the 4 bits are used
);
VSH_OREG_NAME = _VSH_OREG_NAME;

type _VSH_PARAMETER_TYPE = 
(
    PARAM_UNKNOWN = 0,
    PARAM_R,
    PARAM_V,
    PARAM_C
);
VSH_PARAMETER_TYPE = _VSH_PARAMETER_TYPE;

type _VSH_OUTPUT_TYPE = 
(
    OUTPUT_C = 0,
    OUTPUT_O
);
VSH_OUTPUT_TYPE = _VSH_OUTPUT_TYPE;

type _VSH_OUTPUT_MUX =
(
    OMUX_MAC = 0,
    OMUX_ILU
);
VSH_OUTPUT_MUX = _VSH_OUTPUT_MUX;

// Dxbx note : ILU stands for 'Inverse Logic Unit' opcodes
type _VSH_ILU =
(
    ILU_NOP = 0,
    ILU_MOV,
    ILU_RCP,
    ILU_RCC,
    ILU_RSQ,
    ILU_EXP,
    ILU_LOG,
    ILU_LIT // = 7 - all values of the 3 bits are used
);
VSH_ILU = _VSH_ILU;

// Dxbx note : MAC stands for 'Multiply And Accumulate' opcodes
type _VSH_MAC =
(
    MAC_NOP,
    MAC_MOV,
    MAC_MUL,
    MAC_ADD,
    MAC_MAD,
    MAC_DP3,
    MAC_DPH,
    MAC_DP4,
    MAC_DST,
    MAC_MIN,
    MAC_MAX,
    MAC_SLT,
    MAC_SGE,
    MAC_ARL
    // ??? 14
    // ??? 15 - 2 values of the 4 bits are undefined
);
VSH_MAC = _VSH_MAC;

type _VSH_OPCODE_PARAMS = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
// Dxbx Note : Since we split up g_OpCodeParams into g_OpCodeParams_ILU and g_OpCodeParams_MAC
// the following two members aren't needed anymore :
//    ILU: VSH_ILU;
//    MAC: VSH_MAC;
    A: boolean;
    B: boolean;
    C: boolean;
  end; // size = 12 (as in Cxbx)
  VSH_OPCODE_PARAMS = _VSH_OPCODE_PARAMS;
  PVSH_OPCODE_PARAMS = ^VSH_OPCODE_PARAMS;

type _VSH_SWIZZLE =
(
    SWIZZLE_X = 0,
    SWIZZLE_Y,
    SWIZZLE_Z,
    SWIZZLE_W
);
VSH_SWIZZLE = _VSH_SWIZZLE;

type _VSH_PARAMETER = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    ParameterType: VSH_PARAMETER_TYPE;      // Parameter type, R, V or C
    Neg: boolean;                           // TRUE if negated, FALSE if not
    Swizzle: array [0..4-1] of VSH_SWIZZLE; // The four swizzles
    Address: int16;                         // Register address
  end; // size = 28 (as in Cxbx)
  VSH_PARAMETER = _VSH_PARAMETER;
  PVSH_PARAMETER = ^VSH_PARAMETER;

type _VSH_OUTPUT = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    // Output register
    OutputMux: VSH_OUTPUT_MUX;       // MAC or ILU used as output
    OutputType: VSH_OUTPUT_TYPE;     // C or O
    OutputMask: Dxbx4Booleans;
    OutputAddress: int16;
    // MAC output R register
    MACRMask: Dxbx4Booleans;
    MACRAddress: int16;//boolean;
    // ILU output R register
    ILURMask: Dxbx4Booleans;
    ILURAddress: int16;//boolean;
  end; // size = 24 (as in Cxbx)
  VSH_OUTPUT = _VSH_OUTPUT;

// The raw, parsed shader instruction (can be many combined [paired] instructions)
type _VSH_SHADER_INSTRUCTION = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    ILU: VSH_ILU;
    MAC: VSH_MAC;
    Output: VSH_OUTPUT;
    A: VSH_PARAMETER;
    B: VSH_PARAMETER;
    C: VSH_PARAMETER;
    a0x: boolean;
  end; // size = 120 (as in Cxbx)
  VSH_SHADER_INSTRUCTION = _VSH_SHADER_INSTRUCTION;
  PVSH_SHADER_INSTRUCTION = ^VSH_SHADER_INSTRUCTION;

type _VSH_IMD_OUTPUT_TYPE =
(
    IMD_OUTPUT_C,
    IMD_OUTPUT_R,
    IMD_OUTPUT_O,
    IMD_OUTPUT_A0X
);
VSH_IMD_OUTPUT_TYPE = _VSH_IMD_OUTPUT_TYPE;

type _VSH_IMD_INSTRUCTION_TYPE =
(
    IMD_MAC,
    IMD_ILU
);
VSH_IMD_INSTRUCTION_TYPE = _VSH_IMD_INSTRUCTION_TYPE;

type _VSH_IMD_OUTPUT = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    Type_: VSH_IMD_OUTPUT_TYPE;
    Mask: Dxbx4Booleans;
    Address: UInt16;
  end; // size = 12 (as in Cxbx)
  VSH_IMD_OUTPUT = _VSH_IMD_OUTPUT;
  PVSH_IMD_OUTPUT = ^VSH_IMD_OUTPUT;


type _VSH_IMD_PARAMETER = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    Active: boolean;
    Parameter: VSH_PARAMETER;
    IsA0X: boolean;
  end; // size = 36 (as in Cxbx)
  VSH_IMD_PARAMETER = _VSH_IMD_PARAMETER;
  PVSH_IMD_PARAMETER = ^VSH_IMD_PARAMETER;

  TVSH_IMD_PARAMETERArray = array [0..(MaxInt div SizeOf(VSH_IMD_PARAMETER)) - 1] of VSH_IMD_PARAMETER;
  PVSH_IMD_PARAMETERs = ^TVSH_IMD_PARAMETERArray;

type _VSH_INTERMEDIATE_FORMAT = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    IsCombined: boolean;
    InstructionType: VSH_IMD_INSTRUCTION_TYPE;
    MAC: VSH_MAC;
    ILU: VSH_ILU;
    Output: VSH_IMD_OUTPUT;
    Parameters: array [0..3-1] of VSH_IMD_PARAMETER;
  end; // size = 136 (as in Cxbx)
  VSH_INTERMEDIATE_FORMAT = _VSH_INTERMEDIATE_FORMAT;
  PVSH_INTERMEDIATE_FORMAT = ^VSH_INTERMEDIATE_FORMAT;

// Used for xvu spec definition
type _VSH_FIELDMAPPING = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    FieldName: VSH_FIELD_NAME;
    SubToken: uint08;
    StartBit: uint08;
    BitLength: uint08;
  end; // size = 8 (as in Cxbx)
  VSH_FIELDMAPPING = _VSH_FIELDMAPPING;
  PVSH_FIELDMAPPING = ^VSH_FIELDMAPPING;

type _VSH_XBOX_SHADER = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    ShaderHeader: VSH_SHADER_HEADER;
    IntermediateCount: uint16;
    Intermediate: array [0..VSH_MAX_INTERMEDIATE_COUNT -1] of VSH_INTERMEDIATE_FORMAT;
  end; // size = 139272 (as in Cxbx)
  VSH_XBOX_SHADER = _VSH_XBOX_SHADER;
  PVSH_XBOX_SHADER = ^VSH_XBOX_SHADER;

// Local constants
const g_FieldMapping: array [VSH_FIELD_NAME] of VSH_FIELDMAPPING = 
(
    //          Field Name            DWORD         BitPos           BitSize
    ( FieldName:FLD_ILU;              SubToken:1;   StartBit:25;     BitLength:3 ), // VSH_ILU
    ( FieldName:FLD_MAC;              SubToken:1;   StartBit:21;     BitLength:4 ), // VSH_MAC
    ( FieldName:FLD_CONST;            SubToken:1;   StartBit:13;     BitLength:8 ),
    ( FieldName:FLD_V;                SubToken:1;   StartBit: 9;     BitLength:4 ),
    // INPUT A
    ( FieldName:FLD_A_NEG;            SubToken:1;   StartBit: 8;     BitLength:1 ), // Boolean
    ( FieldName:FLD_A_SWZ_X;          SubToken:1;   StartBit: 6;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_A_SWZ_Y;          SubToken:1;   StartBit: 4;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_A_SWZ_Z;          SubToken:1;   StartBit: 2;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_A_SWZ_W;          SubToken:1;   StartBit: 0;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_A_R;              SubToken:2;   StartBit:28;     BitLength:4 ),
    ( FieldName:FLD_A_MUX;            SubToken:2;   StartBit:26;     BitLength:2 ), // VSH_PARAMETER_TYPE
    // INPUT B
    ( FieldName:FLD_B_NEG;            SubToken:2;   StartBit:25;     BitLength:1 ),
    ( FieldName:FLD_B_SWZ_X;          SubToken:2;   StartBit:23;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_B_SWZ_Y;          SubToken:2;   StartBit:21;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_B_SWZ_Z;          SubToken:2;   StartBit:19;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_B_SWZ_W;          SubToken:2;   StartBit:17;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_B_R;              SubToken:2;   StartBit:13;     BitLength:4 ),
    ( FieldName:FLD_B_MUX;            SubToken:2;   StartBit:11;     BitLength:2 ), // VSH_PARAMETER_TYPE
    // INPUT C
    ( FieldName:FLD_C_NEG;            SubToken:2;   StartBit:10;     BitLength:1 ),
    ( FieldName:FLD_C_SWZ_X;          SubToken:2;   StartBit: 8;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_C_SWZ_Y;          SubToken:2;   StartBit: 6;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_C_SWZ_Z;          SubToken:2;   StartBit: 4;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_C_SWZ_W;          SubToken:2;   StartBit: 2;     BitLength:2 ), // VSH_SWIZZLE
    ( FieldName:FLD_C_R_HIGH;         SubToken:2;   StartBit: 0;     BitLength:2 ), // Forms FLD_C_R together with
    ( FieldName:FLD_C_R_LOW;          SubToken:3;   StartBit:30;     BitLength:2 ), // this (to bridge a DWord).
    ( FieldName:FLD_C_MUX;            SubToken:3;   StartBit:28;     BitLength:2 ), // VSH_PARAMETER_TYPE
    // Output
    ( FieldName:FLD_OUT_MAC_MASK_X;   SubToken:3;   StartBit:27;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_MAC_MASK_Y;   SubToken:3;   StartBit:26;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_MAC_MASK_Z;   SubToken:3;   StartBit:25;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_MAC_MASK_W;   SubToken:3;   StartBit:24;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_R;            SubToken:3;   StartBit:20;     BitLength:4 ), // Dxbx note : Why 4 bits?
    ( FieldName:FLD_OUT_ILU_MASK_X;   SubToken:3;   StartBit:19;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_ILU_MASK_Y;   SubToken:3;   StartBit:18;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_ILU_MASK_Z;   SubToken:3;   StartBit:17;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_ILU_MASK_W;   SubToken:3;   StartBit:16;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_O_MASK_X;     SubToken:3;   StartBit:15;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_O_MASK_Y;     SubToken:3;   StartBit:14;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_O_MASK_Z;     SubToken:3;   StartBit:13;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_O_MASK_W;     SubToken:3;   StartBit:12;     BitLength:1 ), // Boolean
    ( FieldName:FLD_OUT_ORB;          SubToken:3;   StartBit:11;     BitLength:1 ), // VSH_OUTPUT_TYPE
    ( FieldName:FLD_OUT_ADDRESS;      SubToken:3;   StartBit: 3;     BitLength:8 ),
    ( FieldName:FLD_OUT_MUX;          SubToken:3;   StartBit: 2;     BitLength:1 ), // VSH_OUTPUT_MUX
    // Other
    ( FieldName:FLD_A0X;              SubToken:3;   StartBit: 1;     BitLength:1 ), // Boolean
    ( FieldName:FLD_FINAL;            SubToken:3;   StartBit: 0;     BitLength:1 )  // Boolean
);

const g_OpCodeParams_ILU: array [VSH_ILU] of VSH_OPCODE_PARAMS =
(
    //     ILU OP       MAC OP      ParamA   ParamB   ParamC
    ( {ILU:ILU_NOP; MAC:MAC_NOP;} a:FALSE; b:FALSE; c:FALSE ), // Dxbx note : Unused
    ( {ILU:ILU_MOV; MAC:MAC_NOP;} a:FALSE; b:FALSE; c:TRUE  ),
    ( {ILU:ILU_RCP; MAC:MAC_NOP;} a:FALSE; b:FALSE; c:TRUE  ),
    ( {ILU:ILU_RCC; MAC:MAC_NOP;} a:FALSE; b:FALSE; c:TRUE  ),
    ( {ILU:ILU_RSQ; MAC:MAC_NOP;} a:FALSE; b:FALSE; c:TRUE  ),
    ( {ILU:ILU_EXP; MAC:MAC_NOP;} a:FALSE; b:FALSE; c:TRUE  ),
    ( {ILU:ILU_LOG; MAC:MAC_NOP;} a:FALSE; b:FALSE; c:TRUE  ),
    ( {ILU:ILU_LIT; MAC:MAC_NOP;} a:FALSE; b:FALSE; c:TRUE  )
);

const g_OpCodeParams_MAC: array [VSH_MAC] of VSH_OPCODE_PARAMS =
(
    //     ILU OP       MAC OP      ParamA   ParamB   ParamC
    ( {ILU:ILU_NOP; MAC:MAC_NOP;} a:FALSE; b:FALSE; c:FALSE ), // Dxbx note : Unused
    ( {ILU:ILU_NOP; MAC:MAC_MOV;} a:TRUE;  b:FALSE; c:FALSE ),
    ( {ILU:ILU_NOP; MAC:MAC_MUL;} a:TRUE;  b:TRUE;  c:FALSE ),
    ( {ILU:ILU_NOP; MAC:MAC_ADD;} a:TRUE;  b:FALSE; c:TRUE  ),
    ( {ILU:ILU_NOP; MAC:MAC_MAD;} a:TRUE;  b:TRUE;  c:TRUE  ),
    ( {ILU:ILU_NOP; MAC:MAC_DP3;} a:TRUE;  b:TRUE;  c:FALSE ),
    ( {ILU:ILU_NOP; MAC:MAC_DPH;} a:TRUE;  b:TRUE;  c:FALSE ),
    ( {ILU:ILU_NOP; MAC:MAC_DP4;} a:TRUE;  b:TRUE;  c:FALSE ),
    ( {ILU:ILU_NOP; MAC:MAC_DST;} a:TRUE;  b:TRUE;  c:FALSE ),
    ( {ILU:ILU_NOP; MAC:MAC_MIN;} a:TRUE;  b:TRUE;  c:FALSE ),
    ( {ILU:ILU_NOP; MAC:MAC_MAX;} a:TRUE;  b:TRUE;  c:FALSE ),
    ( {ILU:ILU_NOP; MAC:MAC_SLT;} a:TRUE;  b:TRUE;  c:FALSE ),
    ( {ILU:ILU_NOP; MAC:MAC_SGE;} a:TRUE;  b:TRUE;  c:FALSE ),
    ( {ILU:ILU_NOP; MAC:MAC_ARL;} a:TRUE;  b:FALSE; c:FALSE )
);

const MAC_OpCode: array [VSH_MAC] of P_char =
(
    'nop',
    'mov',
    'mul',
    'add',
    'mad',
    'dp3',
    'dph',
    'dp4',
    'dst',
    'min',
    'max',
    'slt',
    'sge',
    'mov' // Cxbx says : really 'arl' - Dxbx asks : Why can't we use 'arl' then?
);

const ILU_OpCode: array [VSH_ILU] of P_char =
(
    'nop',
    'mov',
    'rcp',
    'rcc',
    'rsq',
    'exp',
    'log',
    'lit'
);

const OReg_Name: array [VSH_OREG_NAME] of P_char =
(
    'oPos',
    '???',
    '???',
    'oD0',
    'oD1',
    'oFog',
    'oPts',
    'oB0',
    'oB1',
    'oT0',
    'oT1',
    'oT2',
    'oT3',
    '???',
    '???',
    'a0.x'
);

// Dxbx forward declarations :

function VshHandleIsFVF(aHandle: DWORD): boolean; // inline
function VshHandleIsVertexShader(aHandle: DWORD): boolean; inline; // forward
function VshHandleGetVertexShader(aHandle: DWORD): PX_D3DVertexShader; inline; // forward
function VshHandleGetRealHandle(aHandle: DWORD): DWORD; // forward

function XTL_EmuRecompileVshDeclaration(
  pDeclaration: PDWORD;
  ppRecompiledDeclaration: PPDWORD;
  pDeclarationSize: PDWORD;
  IsFixedFunction: boolean;
  pVertexDynamicPatch: PVERTEX_DYNAMIC_PATCH
): DWORD; // forward
function XTL_EmuRecompileVshFunction(
    pFunction: PDWORD;
    ppRecompiled: XTL_PLPD3DXBUFFER;
    pOriginalSize: PDWORD;
    bNoReservedConstants: boolean
) : HRESULT; // forward
procedure XTL_FreeVertexDynamicPatch(pVertexShader: PVERTEX_SHADER); // forward
function IsValidCurrentShader(): boolean; // forward
function VshHandleIsValidShader(aHandle: DWORD): boolean; // forward
function VshGetVertexDynamicPatch(Handle: DWORD): PVERTEX_DYNAMIC_PATCH; // forward

implementation

uses
  // Dxbx
    uLog
  , uEmuFS
  , uEmuD3D8;

{$DEFINE _DEBUG_TRACK_VS}

// VertexShader.h

procedure DbgVshPrintf(aStr: string); overload;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
{$ifdef _DEBUG_TRACK_VS}
  if (g_bPrintfOn) then
    printf(aStr);
{$endif}
end;

procedure DbgVshPrintf(aStr: string; Args: array of const); overload;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
{$ifdef _DEBUG_TRACK_VS}
  if (g_bPrintfOn) then
    printf(aStr, Args);
{$endif}
end;

function VshHandleIsFVF(aHandle: DWORD): boolean; // inline
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := (aHandle and $80000000) = 0;
end;

function VshHandleIsVertexShader(aHandle: DWORD): boolean; // inline
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := (aHandle and $80000000) <> 0;
end;

function VshHandleGetVertexShader(aHandle: DWORD): PX_D3DVertexShader; // inline
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := PX_D3DVertexShader(aHandle and $7FFFFFFF);
end;

function VshHandleGetRealHandle(aHandle: DWORD): DWORD;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  pD3DVertexShader: PX_D3DVertexShader;
  pVertexShader: PVERTEX_SHADER;
begin
  if VshHandleIsVertexShader(aHandle) then
  begin
    pD3DVertexShader := VshHandleGetVertexShader(aHandle);
    Assert(Assigned(pD3DVertexShader));

    pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);
    Assert(Assigned(pVertexShader));

    Result := pVertexShader.Handle;
  end
  else
    Result := aHandle;
end;

// VertexShader.cpp

function IsInUse(const pMask: Dxbx4Booleans): Boolean; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := pMask[0] or pMask[1] or pMask[2] or pMask[3];
end;

function HasMACR(pInstruction: PVSH_SHADER_INSTRUCTION): boolean; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := IsInUse(pInstruction.Output.MACRMask) and (pInstruction.MAC <> MAC_NOP);
end;

function HasMACO(pInstruction: PVSH_SHADER_INSTRUCTION): boolean; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := IsInUse(pInstruction.Output.OutputMask) and
            (pInstruction.Output.OutputMux = OMUX_MAC) and
            (pInstruction.MAC <> MAC_NOP);
end;

function HasMACARL(pInstruction: PVSH_SHADER_INSTRUCTION): boolean; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := (* Cxbx : (not IsInUse(pInstruction.Output.OutputMask)) and
            (pInstruction.Output.OutputMux = OMUX_MAC) and*)
            (pInstruction.MAC = MAC_ARL);
end;

function HasILUR(pInstruction: PVSH_SHADER_INSTRUCTION): boolean; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := IsInUse(pInstruction.Output.ILURMask) and (pInstruction.ILU <> ILU_NOP);
end;

function HasILUO(pInstruction: PVSH_SHADER_INSTRUCTION): boolean; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := IsInUse(pInstruction.Output.OutputMask) and
            (pInstruction.Output.OutputMux = OMUX_ILU) and
            (pInstruction.ILU <> ILU_NOP);
end;

// Retrieves a number of bits in the instruction token
function VshGetFromToken(pShaderToken: Puint32;
                         SubToken: uint08;
                         StartBit: uint08;
                         BitLength: uint08): int; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := (PDWORDs(pShaderToken)[SubToken] shr StartBit) and (not ($FFFFFFFF shl BitLength));
end;

// Converts the C register address to disassembly format
function ConvertCRegister(const CReg: int16): int16; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := ((((CReg shr 5) and 7) - 3) * 32) + (CReg and 31);
end;

function VshGetField(pShaderToken: Puint32;
                     FieldName: VSH_FIELD_NAME): uint08;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := uint08(VshGetFromToken(pShaderToken,
                                   g_FieldMapping[FieldName].SubToken,
                                   g_FieldMapping[FieldName].StartBit,
                                   g_FieldMapping[FieldName].BitLength));
end;

function VshGetOpCodeParams(ILU: VSH_ILU;
                            MAC: VSH_MAC): PVSH_OPCODE_PARAMS;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if ILU in [ILU_MOV..ILU_LIT] then
    Result := PVSH_OPCODE_PARAMS(@g_OpCodeParams_ILU[ILU])
  else
    if MAC in [MAC_MOV..MAC_ARL] then
      Result := PVSH_OPCODE_PARAMS(@g_OpCodeParams_MAC[MAC])
    else
      Result := nil;
end;

procedure VshParseInstruction(pShaderToken: Puint32;
                              pInstruction: PVSH_SHADER_INSTRUCTION);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  // First get the instruction(s).
  pInstruction.ILU := VSH_ILU(VshGetField(pShaderToken, FLD_ILU));
  pInstruction.MAC := VSH_MAC(VshGetField(pShaderToken, FLD_MAC));
  // Get parameter A
  pInstruction.A.ParameterType := VSH_PARAMETER_TYPE(VshGetField(pShaderToken, FLD_A_MUX));

  case pInstruction.A.ParameterType of
    PARAM_R:
      pInstruction.A.Address := VshGetField(pShaderToken, FLD_A_R);

    PARAM_V:
      pInstruction.A.Address := VshGetField(pShaderToken, FLD_V);

    PARAM_C:
      pInstruction.A.Address := ConvertCRegister(VshGetField(pShaderToken, FLD_CONST));

  else
    EmuWarning('Invalid instruction, parameter A type unknown %d', [Ord(pInstruction.A.ParameterType)]);
    Exit;
  end;

  pInstruction.A.Neg := Boolean(VshGetField(pShaderToken, FLD_A_NEG) > 0);
  pInstruction.A.Swizzle[0] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_A_SWZ_X));
  pInstruction.A.Swizzle[1] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_A_SWZ_Y));
  pInstruction.A.Swizzle[2] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_A_SWZ_Z));
  pInstruction.A.Swizzle[3] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_A_SWZ_W));
  // Get parameter B
  pInstruction.B.ParameterType := VSH_PARAMETER_TYPE(VshGetField(pShaderToken, FLD_B_MUX));

  case pInstruction.B.ParameterType of
    PARAM_R:
      pInstruction.B.Address := VshGetField(pShaderToken, FLD_B_R);

    PARAM_V:
      pInstruction.B.Address := VshGetField(pShaderToken, FLD_V);

    PARAM_C:
      pInstruction.B.Address := ConvertCRegister(VshGetField(pShaderToken, FLD_CONST));

  else
    DbgVshPrintf('Invalid instruction, parameter B type unknown %d'#13#10, [Ord(pInstruction.B.ParameterType)]);
    Exit;
  end;

  pInstruction.B.Neg := Boolean(VshGetField(pShaderToken, FLD_B_NEG) > 0);
  pInstruction.B.Swizzle[0] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_B_SWZ_X));
  pInstruction.B.Swizzle[1] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_B_SWZ_Y));
  pInstruction.B.Swizzle[2] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_B_SWZ_Z));
  pInstruction.B.Swizzle[3] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_B_SWZ_W));
  // Get parameter C
  pInstruction.C.ParameterType := VSH_PARAMETER_TYPE(VshGetField(pShaderToken, FLD_C_MUX));

  case pInstruction.C.ParameterType of
    PARAM_R: begin
        pInstruction.C.Address := (VshGetField(pShaderToken, FLD_C_R_HIGH) shl 2) or
                                   VshGetField(pShaderToken, FLD_C_R_LOW);
      end;
    PARAM_V: begin
        pInstruction.C.Address := VshGetField(pShaderToken, FLD_V);
      end;
    PARAM_C: begin
        pInstruction.C.Address := ConvertCRegister(VshGetField(pShaderToken, FLD_CONST));
      end;

    else begin
        DbgVshPrintf('Invalid instruction, parameter C type unknown %d'#13#10, [Ord(pInstruction.C.ParameterType)]);
        Exit;
    end;
  end;

  pInstruction.C.Neg := Boolean(VshGetField(pShaderToken, FLD_C_NEG) > 0);
  pInstruction.C.Swizzle[0] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_C_SWZ_X));
  pInstruction.C.Swizzle[1] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_C_SWZ_Y));
  pInstruction.C.Swizzle[2] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_C_SWZ_Z));
  pInstruction.C.Swizzle[3] := VSH_SWIZZLE(VshGetField(pShaderToken, FLD_C_SWZ_W));
  // Get output
  // Output register
  pInstruction.Output.OutputType := VSH_OUTPUT_TYPE(VshGetField(pShaderToken, FLD_OUT_ORB));

  case pInstruction.Output.OutputType of
    OUTPUT_C:
     pInstruction.Output.OutputAddress := ConvertCRegister(VshGetField(pShaderToken, FLD_OUT_ADDRESS));

    OUTPUT_O:
     pInstruction.Output.OutputAddress := VshGetField(pShaderToken, FLD_OUT_ADDRESS) and $F;
  end;

  pInstruction.Output.OutputMux := VSH_OUTPUT_MUX(VshGetField(pShaderToken, FLD_OUT_MUX));
  pInstruction.Output.OutputMask[0] := Boolean(VshGetField(pShaderToken, FLD_OUT_O_MASK_X) > 0);
  pInstruction.Output.OutputMask[1] := Boolean(VshGetField(pShaderToken, FLD_OUT_O_MASK_Y) > 0);
  pInstruction.Output.OutputMask[2] := Boolean(VshGetField(pShaderToken, FLD_OUT_O_MASK_Z) > 0);
  pInstruction.Output.OutputMask[3] := Boolean(VshGetField(pShaderToken, FLD_OUT_O_MASK_W) > 0);
  // MAC output
  pInstruction.Output.MACRMask[0] := Boolean(VshGetField(pShaderToken, FLD_OUT_MAC_MASK_X) > 0);
  pInstruction.Output.MACRMask[1] := Boolean(VshGetField(pShaderToken, FLD_OUT_MAC_MASK_Y) > 0);
  pInstruction.Output.MACRMask[2] := Boolean(VshGetField(pShaderToken, FLD_OUT_MAC_MASK_Z) > 0);
  pInstruction.Output.MACRMask[3] := Boolean(VshGetField(pShaderToken, FLD_OUT_MAC_MASK_W) > 0);
  pInstruction.Output.MACRAddress := VshGetField(pShaderToken, FLD_OUT_R);
  // ILU output
  pInstruction.Output.ILURMask[0] := Boolean(VshGetField(pShaderToken, FLD_OUT_ILU_MASK_X) > 0);
  pInstruction.Output.ILURMask[1] := Boolean(VshGetField(pShaderToken, FLD_OUT_ILU_MASK_Y) > 0);
  pInstruction.Output.ILURMask[2] := Boolean(VshGetField(pShaderToken, FLD_OUT_ILU_MASK_Z) > 0);
  pInstruction.Output.ILURMask[3] := Boolean(VshGetField(pShaderToken, FLD_OUT_ILU_MASK_W) > 0);
  pInstruction.Output.ILURAddress := VshGetField(pShaderToken, FLD_OUT_R);
  // Finally, get a0.x indirect constant addressing
  pInstruction.a0x := Boolean(VshGetField(pShaderToken, FLD_A0X) > 0);
end;

// Print functions
function VshGetRegisterName(ParameterType: VSH_PARAMETER_TYPE): _char;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  case (ParameterType) of
    PARAM_R:
      Result := 'r';
    PARAM_V:
      Result := 'v';
    PARAM_C:
      Result := 'c';
  else
    Result := '?';
  end;
end;

procedure VshWriteOutputMask(const OutputMask: Dxbx4Booleans;
                             pDisassembly: P_char;
                             pDisassemblyPos: Puint32);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
const
  _x: array [Boolean] of AnsiString = ('', 'x');
  _y: array [Boolean] of AnsiString = ('', 'y');
  _z: array [Boolean] of AnsiString = ('', 'z');
  _w: array [Boolean] of AnsiString = ('', 'w');
begin
  if (OutputMask[0]) and (OutputMask[1]) and (OutputMask[2]) and (OutputMask[3]) then
  begin
    // All components are there, no need to print the mask
    Exit;
  end;

  Inc(pDisassemblyPos^, sprintf(pDisassembly + pDisassemblyPos^, '.%s%s%s%s', [
    _x[OutputMask[0]],
    _y[OutputMask[1]],
    _z[OutputMask[2]],
    _w[OutputMask[3]]]));
end;

procedure VshWriteParameter(pParameter: PVSH_IMD_PARAMETER;
                            pDisassembly: P_char;
                            pDisassemblyPos: Puint32);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
const
  _neg: array [Boolean] of AnsiString = ('', '-');
var
  i: int;
  j: int;
  Swizzle: _char;
begin
  Inc(pDisassemblyPos^, sprintf(pDisassembly + pDisassemblyPos^, ', %s%s', [
                _neg[pParameter.Parameter.Neg],
                VshGetRegisterName(pParameter.Parameter.ParameterType)]));
  if (pParameter.Parameter.ParameterType = PARAM_C) and (pParameter.IsA0X) then
  begin
    // Only display the offset if it's not 0.
    if (pParameter.Parameter.Address) > 0 then
    begin
      Inc(pDisassemblyPos^, sprintf(pDisassembly + pDisassemblyPos^, '[a0.x+%d]', [pParameter.Parameter.Address]));
    end
    else
    begin
      Inc(pDisassemblyPos^, sprintf(pDisassembly + pDisassemblyPos^, '[a0.x]'));
    end;
  end
  else
  begin
    Inc(pDisassemblyPos^, sprintf(pDisassembly + pDisassemblyPos^, '%d', [pParameter.Parameter.Address]));
  end;
  // Only bother printing the swizzle if it is not .xyzw
  if not ((pParameter.Parameter.Swizzle[0] = SWIZZLE_X) and
          (pParameter.Parameter.Swizzle[1] = SWIZZLE_Y) and
          (pParameter.Parameter.Swizzle[2] = SWIZZLE_Z) and
          (pParameter.Parameter.Swizzle[3] = SWIZZLE_W)) then
  begin
    Inc(pDisassemblyPos^, sprintf(pDisassembly + pDisassemblyPos^, '.'));
    for i := 0 to 4-1 do
    begin
      Swizzle := '?';
      case (pParameter.Parameter.Swizzle[i]) of
        SWIZZLE_X:
          Swizzle := 'x';
        SWIZZLE_Y:
          Swizzle := 'y';
        SWIZZLE_Z:
          Swizzle := 'z';
        SWIZZLE_W:
          Swizzle := 'w';
      end;
      Inc(pDisassemblyPos^, sprintf(pDisassembly + pDisassemblyPos^, '%s', [Swizzle]));
      j := i;
      while j < 4 do
      begin
        if (pParameter.Parameter.Swizzle[i] <> pParameter.Parameter.Swizzle[j]) then
        begin
          break;
        end;
        Inc(j);
      end; // while
      if (j = 4) then
      begin
        break;
      end;
    end;
  end;
end; // VshWriteParameter

procedure VshWriteShader(pShader: PVSH_XBOX_SHADER;
                         pDisassembly: P_char;
                         Truncate: boolean);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  DisassemblyPos: uint32;
  i, j: int;
  pIntermediate: PVSH_INTERMEDIATE_FORMAT;
  pParameter: PVSH_IMD_PARAMETER;
begin
  DisassemblyPos := 0;
  case pShader.ShaderHeader.Version of
    VERSION_VS:
      Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, 'vs.1.1'#13#10));
    VERSION_XVS:
      Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, 'xvs.1.1'#13#10));
    VERSION_XVSS:
      Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, 'xvss.1.1'#13#10));
    VERSION_XVSW:
      Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, 'xvsw.1.1'#13#10));
  end;

  // Dxbx note : Translated 'for' to 'while', because loop condition is a complex expression :
  i := 0; while (i < pShader.IntermediateCount) and ((i < 128) or (not Truncate)) do
  begin
    pIntermediate := @(pShader.Intermediate[i]);

    if(i = 128) then
    begin
      Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, '; -- Passing the truncation limit --'#13#10));
    end;
    // Writing combining sign if neccessary
    if(pIntermediate.IsCombined) then
    begin
      Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, '+'));
    end;
    // Print the op code
    if(pIntermediate.InstructionType = IMD_MAC) then
    begin
      // Dxbx addition : Safeguard against incorrect MAC opcodes :
      if (Ord(pIntermediate.MAC) > Ord(HIGH(VSH_MAC))) then
        Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, '??? '))
      else
        Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, '%s ', [MAC_OpCode[pIntermediate.MAC]]))
    end
    else
    begin
      // Dxbx addition : Safeguard against incorrect ILU opcodes :
      if (Ord(pIntermediate.ILU) > Ord(HIGH(VSH_ILU))) then
        Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, '??? '))
      else
        Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, '%s ', [ILU_OpCode[pIntermediate.ILU]]));
    end;

    // Print the output parameter
    if(pIntermediate.Output.Type_ = IMD_OUTPUT_A0X) then
    begin
      Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, 'a0.x'))
    end
    else
    begin
      case(pIntermediate.Output.Type_) of
        IMD_OUTPUT_C:
          Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, 'c%d', [pIntermediate.Output.Address]));
        IMD_OUTPUT_R:
          Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, 'r%d', [pIntermediate.Output.Address]));
        IMD_OUTPUT_O:
          // Dxbx addition : Safeguard against incorrect VSH_OREG_NAME values :
          if (Integer(pIntermediate.Output.Address) > Ord(HIGH(VSH_OREG_NAME))) then
            // don't add anything
          else
            Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, '%s', [OReg_Name[VSH_OREG_NAME(pIntermediate.Output.Address)]]));
      else
        DxbxKrnlCleanup('Invalid output register in vertex shader!');
      end;
      VshWriteOutputMask(pIntermediate.Output.Mask, pDisassembly, @DisassemblyPos);
    end;

    // Print the parameters
    for j := 0 to 3-1 do
    begin
      pParameter := @(pIntermediate.Parameters[j]);
      if(pParameter.Active) then
      begin
        VshWriteParameter(pParameter, pDisassembly, @DisassemblyPos);
      end;
    end;
    Inc(DisassemblyPos, sprintf(pDisassembly + DisassemblyPos, #13#10));
    Inc(i);
  end;
  pDisassembly[DisassemblyPos] := #0;
end; // VshWriteShader

procedure VshAddParameter(pParameter: PVSH_PARAMETER;
                          a0x: boolean;
                          pIntermediateParameter: PVSH_IMD_PARAMETER);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  pIntermediateParameter.Parameter := pParameter^;
  pIntermediateParameter.Active := TRUE;
  pIntermediateParameter.IsA0X := a0x;
end;

procedure VshAddParameters(pInstruction: PVSH_SHADER_INSTRUCTION;
                           ILU: VSH_ILU;
                           MAC: VSH_MAC;
                           pParameters: PVSH_IMD_PARAMETERs);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  ParamCount: uint08;
  pParams: PVSH_OPCODE_PARAMS;
begin
  ParamCount := 0;
  pParams := VshGetOpCodeParams(ILU, MAC);

  // param A
  if (pParams.A) then
  begin
    VshAddParameter(@pInstruction.A, pInstruction.a0x, @pParameters[ParamCount]);
    Inc(ParamCount);
  end;

  // param B
  if (pParams.B) then
  begin
    VshAddParameter(@pInstruction.B, pInstruction.a0x, @pParameters[ParamCount]);
    Inc(ParamCount);
  end;

  // param C
  if (pParams.C) then
  begin
    VshAddParameter(@pInstruction.C, pInstruction.a0x, @pParameters[ParamCount]);
    // Inc(ParamCount);
  end;
end; // VshAddParameters

procedure VshVerifyBufferBounds(pShader: PVSH_XBOX_SHADER);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  if (pShader.IntermediateCount = VSH_MAX_INTERMEDIATE_COUNT) then
  begin
    DxbxKrnlCleanup('Shader exceeds conversion buffer!');
  end;
end;

function VshNewIntermediate(pShader: PVSH_XBOX_SHADER): PVSH_INTERMEDIATE_FORMAT;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  VshVerifyBufferBounds(pShader);

  ZeroMemory(@pShader.Intermediate[pShader.IntermediateCount], sizeof(VSH_INTERMEDIATE_FORMAT));

  Result := @pShader.Intermediate[pShader.IntermediateCount];
  Inc(pShader.IntermediateCount);
end;

procedure VshInsertIntermediate(pShader: PVSH_XBOX_SHADER;
                                pIntermediate: PVSH_INTERMEDIATE_FORMAT; 
                                Pos: uint16);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  i: int;
begin
  VshVerifyBufferBounds(pShader);

  for i := pShader.IntermediateCount downto pos do
  begin
    pShader.Intermediate[i + 1] := pShader.Intermediate[i];
  end;
  pShader.Intermediate[Pos] := pIntermediate^;
  Inc(pShader.IntermediateCount);
end;

procedure VshDeleteIntermediate(pShader: PVSH_XBOX_SHADER; 
                                Pos: uint16);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  i: int;
begin
  if pShader.IntermediateCount > 1 then // Dxbx addition, to prevent underflow
  for i := Pos to (pShader.IntermediateCount - 1) - 1 do
  begin
    pShader.Intermediate[i] := pShader.Intermediate[i + 1];
  end;
  Dec(pShader.IntermediateCount);
end;

function VshAddInstructionMAC_R(pInstruction: PVSH_SHADER_INSTRUCTION; 
                                pShader: PVSH_XBOX_SHADER; 
                                IsCombined: boolean): boolean;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  pIntermediate: PVSH_INTERMEDIATE_FORMAT;
begin
  if (not HasMACR(pInstruction)) then
  begin
    Result := FALSE;
    Exit;
  end;

  pIntermediate := VshNewIntermediate(pShader);
  pIntermediate.IsCombined := IsCombined;

  // Opcode
  pIntermediate.InstructionType := IMD_MAC;
  pIntermediate.MAC := pInstruction.MAC;

  // Output param
  pIntermediate.Output.Type_ := IMD_OUTPUT_R;
  pIntermediate.Output.Address := Word(pInstruction.Output.MACRAddress);
  memcpy(@(pIntermediate.Output.Mask[0]), @(pInstruction.Output.MACRMask[0]), sizeof(pIntermediate.Output.Mask));

  // Other parameters
  VshAddParameters(pInstruction, ILU_NOP, pInstruction.MAC, @pIntermediate.Parameters[0]);

  Result := TRUE;
end; // VshAddInstructionMAC_R

function VshAddInstructionMAC_O(pInstruction: PVSH_SHADER_INSTRUCTION; 
                                pShader: PVSH_XBOX_SHADER; 
                                IsCombined: boolean): boolean;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  pIntermediate: PVSH_INTERMEDIATE_FORMAT;
begin
  if (not HasMACO(pInstruction)) then
  begin
    Result := FALSE;
    Exit;
  end;

  pIntermediate := VshNewIntermediate(pShader);
  pIntermediate.IsCombined := IsCombined;

  // Opcode
  pIntermediate.InstructionType := IMD_MAC;
  pIntermediate.MAC := pInstruction.MAC;

  // Output param
  if pInstruction.Output.OutputType = OUTPUT_C then
    pIntermediate.Output.Type_ := IMD_OUTPUT_C
  else
    pIntermediate.Output.Type_ := IMD_OUTPUT_O;
  pIntermediate.Output.Address := pInstruction.Output.OutputAddress;
  memcpy(@(pIntermediate.Output.Mask[0]), @(pInstruction.Output.OutputMask[0]), sizeof(pIntermediate.Output.Mask));

  // Other parameters
  VshAddParameters(pInstruction, ILU_NOP, pInstruction.MAC, @pIntermediate.Parameters[0]);

  Result := TRUE;
end; // VshAddInstructionMAC_O

function VshAddInstructionMAC_ARL(pInstruction: PVSH_SHADER_INSTRUCTION; 
                                  pShader: PVSH_XBOX_SHADER; 
                                  IsCombined: boolean): boolean;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  pIntermediate: PVSH_INTERMEDIATE_FORMAT;
begin
  if (not HasMACARL(pInstruction)) then
  begin
    Result := FALSE;
    Exit;
  end;

  pIntermediate := VshNewIntermediate(pShader);
  pIntermediate.IsCombined := IsCombined;

  // Opcode
  pIntermediate.InstructionType := IMD_MAC;
  pIntermediate.MAC := pInstruction.MAC;

  // Output param
  pIntermediate.Output.Type_ := IMD_OUTPUT_A0X;
  pIntermediate.Output.Address := pInstruction.Output.OutputAddress;

  // Other parameters
  VshAddParameters(pInstruction, ILU_NOP, pInstruction.MAC, @pIntermediate.Parameters[0]);

  Result := TRUE;
end; // VshAddInstructionMAC_ARL

function VshAddInstructionILU_R(pInstruction: PVSH_SHADER_INSTRUCTION; 
                                pShader: PVSH_XBOX_SHADER; 
                                IsCombined: boolean): boolean;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  pIntermediate: PVSH_INTERMEDIATE_FORMAT;
begin
  if (not HasILUR(pInstruction)) then
  begin
    Result := FALSE;
    Exit;
  end;

  pIntermediate := VshNewIntermediate(pShader);
  pIntermediate.IsCombined := IsCombined;

  // Opcode
  pIntermediate.InstructionType := IMD_ILU;
  pIntermediate.ILU := pInstruction.ILU;

  // Output param
  pIntermediate.Output.Type_ := IMD_OUTPUT_R;
  // If this is a combined instruction, only r1 is allowed (R address should not be used)
  if IsCombined then
    pIntermediate.Output.Address := 1
  else
    pIntermediate.Output.Address := Word(pInstruction.Output.ILURAddress);
  memcpy(@(pIntermediate.Output.Mask[0]), @(pInstruction.Output.ILURMask[0]), sizeof(pIntermediate.Output.Mask));

  // Other parameters
  VshAddParameters(pInstruction, pInstruction.ILU, MAC_NOP, @pIntermediate.Parameters[0]);

  Result := TRUE;
end; // VshAddInstructionILU_R

function VshAddInstructionILU_O(pInstruction: PVSH_SHADER_INSTRUCTION; 
                                pShader: PVSH_XBOX_SHADER; 
                                IsCombined: boolean): boolean;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  pIntermediate: PVSH_INTERMEDIATE_FORMAT;
begin
  if (not HasILUO(pInstruction)) then
  begin
    Result := FALSE;
    Exit;
  end;

  pIntermediate := VshNewIntermediate(pShader);
  pIntermediate.IsCombined := IsCombined;

  // Opcode
  pIntermediate.InstructionType := IMD_ILU;
  pIntermediate.ILU := pInstruction.ILU;

  // Output param
  if pInstruction.Output.OutputType = OUTPUT_C then
    pIntermediate.Output.Type_ := IMD_OUTPUT_C
  else
    pIntermediate.Output.Type_ := IMD_OUTPUT_O;

  pIntermediate.Output.Address := pInstruction.Output.OutputAddress;
  memcpy(@(pIntermediate.Output.Mask[0]), @(pInstruction.Output.OutputMask[0]), sizeof(pIntermediate.Output.Mask));

  // Other parameters
  VshAddParameters(pInstruction, pInstruction.ILU, MAC_NOP, @pIntermediate.Parameters[0]);
  Result := TRUE;
end; // VshAddInstructionILU_O

procedure VshConvertToIntermediate(pInstruction: PVSH_SHADER_INSTRUCTION; 
                                   pShader: PVSH_XBOX_SHADER);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  IsCombined: boolean;
begin
  // Five types of instructions:
  //   MAC
  //
  //   ILU
  //
  //   MAC
  //   +ILU
  //
  //   MAC
  //   +MAC
  //   +ILU
  //
  //   MAC
  //   +ILU
  //   +ILU
  IsCombined := FALSE;

  if (VshAddInstructionMAC_R(pInstruction, pShader, IsCombined)) then
  begin
    if (HasMACO(pInstruction) or
        HasILUR(pInstruction) or
        HasILUO(pInstruction)) then
    begin
      IsCombined := TRUE;
    end;
  end;

  if (VshAddInstructionMAC_O(pInstruction, pShader, IsCombined)) then
  begin
    if (HasILUR(pInstruction) or
        HasILUO(pInstruction)) then
    begin
      IsCombined := TRUE;
    end;
  end;

  // Special case, arl (mov a0.x, ...)
  if (VshAddInstructionMAC_ARL(pInstruction, pShader, IsCombined)) then
  begin
    if (HasILUR(pInstruction) or
        HasILUO(pInstruction)) then
    begin
      IsCombined := TRUE;
    end;
  end;

  if (VshAddInstructionILU_R(pInstruction, pShader, IsCombined)) then
  begin
    if (HasILUO(pInstruction)) then
    begin
      IsCombined := TRUE;
    end;
  end;

  {ignore}VshAddInstructionILU_O(pInstruction, pShader, IsCombined);
end; // VshConvertToIntermediate

procedure VshSetSwizzle(pParameter: PVSH_IMD_PARAMETER; 
                        x: VSH_SWIZZLE; 
                        y: VSH_SWIZZLE;
                        z: VSH_SWIZZLE;
                        w: VSH_SWIZZLE); inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  pParameter.Parameter.Swizzle[0] := x;
  pParameter.Parameter.Swizzle[1] := y;
  pParameter.Parameter.Swizzle[2] := z;
  pParameter.Parameter.Swizzle[3] := w;
end;

procedure VshSetOutputMask(pOutput: PVSH_IMD_OUTPUT; 
                           MaskX: boolean; 
                           MaskY: boolean;
                           MaskZ: boolean;
                           MaskW: boolean); inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  pOutput.Mask[0] := MaskX;
  pOutput.Mask[1] := MaskY;
  pOutput.Mask[2] := MaskZ;
  pOutput.Mask[3] := MaskW;
end;

(*
    mul oPos.xyz, r12, c-38
    +rcc r1.x, r12.w

    mad oPos.xyz, r12, r1.x, c-37
*)
procedure VshRemoveScreenSpaceInstructions(pShader: PVSH_XBOX_SHADER);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  PosC38: int16;
  deleted: int;
  i: int;
  k: int;
  j: int;
  pIntermediate: PVSH_INTERMEDIATE_FORMAT;
  pIntermediate1W: PVSH_INTERMEDIATE_FORMAT;
  MulIntermediate: VSH_INTERMEDIATE_FORMAT;
  AddIntermediate: VSH_INTERMEDIATE_FORMAT;
begin
  PosC38 := -1;
  deleted := 0;
  // Dxbx note : Translated 'for' to 'while', because counter is incremented twice :
  i := 0; while i < pShader.IntermediateCount do
  begin
    pIntermediate := @pShader.Intermediate[i];

    for k := 0 to 3-1 do
    begin
      if (pIntermediate.Parameters[k].Active) then
      begin
        if (pIntermediate.Parameters[k].Parameter.ParameterType = PARAM_C) and
           (not pIntermediate.Parameters[k].IsA0X) then
        begin
          if (pIntermediate.Parameters[k].Parameter.Address = X_VSCM_RESERVED_CONSTANT2{=-37}) then
          begin
            // Found c-37, remove the instruction
            if (k = 2) and
               pIntermediate.Parameters[1].Active and
               (pIntermediate.Parameters[1].Parameter.ParameterType = PARAM_R) then
            begin
              DbgVshPrintf('PosC38 = %d i = %d'#13#10, [PosC38, i]);
              for j := (i-1) downto 0 do
              begin
                pIntermediate1W := @pShader.Intermediate[j];
                // Time to start searching for +rcc r#.x, r12.w
                if (pIntermediate1W.InstructionType = IMD_ILU) and
                    (pIntermediate1W.ILU = ILU_RCC) and
                    (pIntermediate1W.Output.Type_ = IMD_OUTPUT_R) and
                    (pIntermediate1W.Output.Address = 
                     Word(pIntermediate.Parameters[1].Parameter.Address)) then
                begin
                  DbgVshPrintf('Deleted +rcc r1.x, r12.w'#13#10);
                  VshDeleteIntermediate(pShader, j);
                  Inc(deleted);
                  Dec(i);
                  //Dec(j);
                  break;
                end;
              end;
            end;
            VshDeleteIntermediate(pShader, i);
            Inc(deleted);
            Dec(i);
            DbgVshPrintf('Deleted mad oPos.xyz, r12, r1.x, c-37'#13#10);
            break;
          end
          else if (pIntermediate.Parameters[k].Parameter.Address = X_VSCM_RESERVED_CONSTANT1{=-38}) then
          begin
            VshDeleteIntermediate(pShader, i);
            PosC38 := i;
            Inc(deleted);
            Dec(i);
            DbgVshPrintf('Deleted mul oPos.xyz, r12, c-38'#13#10);
          end;
        end;
      end;
    end;

    Inc(i);
  end;

  // If we couldn't find the generic screen space transformation we're
  // assuming that the shader writes direct screen coordinates that must be
  // normalized. This hack will fail if (a) the shader uses custom screen
  // space transformation, (b) reads r10 or r11 after we have written to
  // them, or (c) doesn't reserve c-38 and c-37 for scale and offset.
  if (deleted <> 3) then
  begin
    EmuWarning('Applying screen space vertex shader patching hack!');
    // Dxbx note : Translated 'for' to 'while', because counter is incremented twice :
    i := 0; while i < pShader.IntermediateCount do
    begin
      pIntermediate := @pShader.Intermediate[i];

      // Find instructions outputting to oPos.
      if (pIntermediate.Output.Type_ = IMD_OUTPUT_O) and
         (pIntermediate.Output.Address = Word(Ord(OREG_OPOS))) then
      begin
        // Redirect output to r11.
        pIntermediate.Output.Type_    := IMD_OUTPUT_R;
        pIntermediate.Output.Address  := 11;

        // Scale r11 to r10. (mul r10.[mask], r11, c58)
        MulIntermediate.IsCombined        := FALSE;
        MulIntermediate.InstructionType   := IMD_MAC;
        MulIntermediate.MAC               := MAC_MUL;
        MulIntermediate.Output.Type_      := IMD_OUTPUT_R;
        MulIntermediate.Output.Address    := 10;
        MulIntermediate.Output.Mask[0]    := pIntermediate.Output.Mask[0];
        MulIntermediate.Output.Mask[1]    := pIntermediate.Output.Mask[1];
        MulIntermediate.Output.Mask[2]    := pIntermediate.Output.Mask[2];
        MulIntermediate.Output.Mask[3]    := pIntermediate.Output.Mask[3];
        MulIntermediate.Parameters[0].Active                  := TRUE;
        MulIntermediate.Parameters[0].IsA0X                   := FALSE;
        MulIntermediate.Parameters[0].Parameter.ParameterType := PARAM_R;
        MulIntermediate.Parameters[0].Parameter.Address       := 11;
        MulIntermediate.Parameters[0].Parameter.Neg           := FALSE;
        VshSetSwizzle(@MulIntermediate.Parameters[0], SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_W);
        MulIntermediate.Parameters[1].Active                  := TRUE;
        MulIntermediate.Parameters[1].IsA0X                   := FALSE;
        MulIntermediate.Parameters[1].Parameter.ParameterType := PARAM_C;
        // Dxbx note : Cxbx calls ConvertCRegister(58) here, but doing a conversion seems incorrect.
        // That, and the constant address is also corrected afterwards, so use the original :
        MulIntermediate.Parameters[1].Parameter.Address       := X_VSCM_RESERVED_CONSTANT1{=-38};
        MulIntermediate.Parameters[1].Parameter.Neg           := FALSE;
        VshSetSwizzle(@MulIntermediate.Parameters[1], SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_W);
        MulIntermediate.Parameters[2].Active                  := FALSE;
        Inc(i); VshInsertIntermediate(pShader, @MulIntermediate, i);

        // Add offset with r10 to oPos (add oPos.[mask], r10, c59)
        AddIntermediate := MulIntermediate;
        AddIntermediate.MAC               := MAC_ADD;
        AddIntermediate.Output.Type_      := IMD_OUTPUT_O;
        AddIntermediate.Output.Address    := Ord(OREG_OPOS);
        AddIntermediate.Parameters[0].Parameter.ParameterType := PARAM_R;
        AddIntermediate.Parameters[0].Parameter.Address       := 10;
        // Dxbx note : Cxbx calls ConvertCRegister(59) here, but doing a conversion seems incorrect.
        // That, and the constant address is also corrected afterwards, so use the original :
        AddIntermediate.Parameters[1].Parameter.Address       := X_VSCM_RESERVED_CONSTANT2{=-37};
        Inc(i); VshInsertIntermediate(pShader, @AddIntermediate, i);
      end;

      Inc(i);
    end; // while
  end;
end; // VshRemoveScreenSpaceInstructions

// Converts the intermediate format vertex shader to DirectX 8 format
function VshConvertShader(pShader: PVSH_XBOX_SHADER;
                          bNoReservedConstants: boolean): boolean;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  RUsage: array [0..13-1] of boolean;
  i: int;
  j: int;
  k: int;
  pIntermediate: PVSH_INTERMEDIATE_FORMAT;
  TmpIntermediate: VSH_INTERMEDIATE_FORMAT;
  R12Replacement: int16;
  pOPosWriteBack: PVSH_INTERMEDIATE_FORMAT;
  outRegister: int;
  swizzle: int;
begin
  RUsage[0] := FALSE;
  RUsage[1] := FALSE;
  RUsage[2] := FALSE;
  RUsage[3] := FALSE;
  RUsage[4] := FALSE;
  RUsage[5] := FALSE;
  RUsage[6] := FALSE;
  RUsage[7] := FALSE;
  RUsage[8] := FALSE;
  RUsage[9] := FALSE;
  RUsage[10] := FALSE;
  RUsage[11] := FALSE;
  RUsage[12] := FALSE;

  // TODO -oCXBX: What about state shaders and such?
  pShader.ShaderHeader.Version := VERSION_VS;

  // Search for the screen space instructions, and remove them
  if (not bNoReservedConstants) then
  begin
    VshRemoveScreenSpaceInstructions(pShader);
  end;

  // TODO -oCXBX: Add routine for compacting r register usage so that at least one is freed (two if dph and r12)
  // Dxbx note : Translated 'for' to 'while', because counter is incremented twice :
  i := 0; while i < pShader.IntermediateCount do
  begin
    pIntermediate := @pShader.Intermediate[i];
    // Combining not supported in vs.1.1
    pIntermediate.IsCombined := FALSE;

    (* MARKED OUT CXBX
    if (pIntermediate.Output.Type = IMD_OUTPUT_O) and (pIntermediate.Output.Address = OREG_OFOG) then
    begin
        // The PC shader assembler doesn't like masks on scalar registers
        VshSetOutputMask(@pIntermediate.Output, TRUE, TRUE, TRUE, TRUE);
    end;
    *)

    if (pIntermediate.InstructionType = IMD_ILU) and (pIntermediate.ILU = ILU_RCC) then
    begin
      // Convert rcc to rcp
      pIntermediate.ILU := ILU_RCP;
    end;

    if (pIntermediate.Output.Type_ = IMD_OUTPUT_R) then
    begin
      RUsage[pIntermediate.Output.Address] := TRUE;
    end;

    // Make constant registers range from 0 to 191 instead of -96 to 95
    if (pIntermediate.Output.Type_ = IMD_OUTPUT_C) then
    begin
      Inc(pIntermediate.Output.Address, X_VSCM_CORRECTION{=96});
    end;

    for j := 0 to 3-1 do
    begin
      // Dxbx fix : PARAM_C correction shouldn't depend on Active :
//      if (pIntermediate.Parameters[j].Active) then
      begin
        if (pIntermediate.Parameters[j].Parameter.ParameterType = PARAM_R) then
        begin
          // Dxbx fix : Here, Active does seem to apply :
          if (pIntermediate.Parameters[j].Active) then
            RUsage[pIntermediate.Parameters[j].Parameter.Address] := TRUE;
        end;
        // Make constant registers range from 0 to 191 instead of -96 to 95
        if (pIntermediate.Parameters[j].Parameter.ParameterType = PARAM_C) then
        begin
          Inc(pIntermediate.Parameters[j].Parameter.Address, X_VSCM_CORRECTION{=96});
        end;
      end;
    end;

    if (pIntermediate.InstructionType = IMD_MAC) and (pIntermediate.MAC = MAC_DPH) then
    begin
      // 2010/01/12 - revel8n - attempt to alleviate conversion issues relate to the dph instruction

      // Replace dph with dp3 and add
      if (pIntermediate.Output.Type_ <> IMD_OUTPUT_R) then
      begin
        // TODO -oCXBX: Complete dph support
        EmuWarning('Can''t simulate dph for other than output r registers (yet)');

        // attempt to find unused register...
        outRegister := -1;
        for j := 11 downto 0 do
        begin
          if (not RUsage[j]) then
          begin
            outRegister := j;
            break;
          end;
        end;

        // return failure if there are no available registers
        if (outRegister = -1) then
        begin
          Result := FALSE;
          Exit;
        end;
        
        TmpIntermediate := pIntermediate^;

        // modify the instructions
        // the register value is not needed beyond these instructions so setting the usage flag should not be necessary (??)
        pIntermediate.MAC := MAC_DP3;
        pIntermediate.Output.Type_ := IMD_OUTPUT_R;
        pIntermediate.Output.Address := outRegister;
        VshSetOutputMask(@pIntermediate.Output, TRUE, TRUE, TRUE, TRUE);

        TmpIntermediate.MAC := MAC_ADD;
        TmpIntermediate.Parameters[0].IsA0X := FALSE;
        TmpIntermediate.Parameters[0].Parameter.ParameterType := PARAM_R;
        TmpIntermediate.Parameters[0].Parameter.Address := outRegister;
        TmpIntermediate.Parameters[0].Parameter.Neg := FALSE;
        // VshSetSwizzle(@TmpIntermediate.Parameters[0], SWIZZLE_W, SWIZZLE_W, SWIZZLE_W, SWIZZLE_W);
        VshSetSwizzle(@TmpIntermediate.Parameters[1], SWIZZLE_W, SWIZZLE_W, SWIZZLE_W, SWIZZLE_W);
        //VshSetOutputMask(@TmpIntermediate.Output, FALSE, FALSE, FALSE, TRUE);
        VshInsertIntermediate(pShader, @TmpIntermediate, i + 1);
      end
      else
      begin
        TmpIntermediate := pIntermediate^;
        pIntermediate.MAC := MAC_DP3;
        TmpIntermediate.MAC := MAC_ADD;
        TmpIntermediate.Parameters[0].IsA0X := FALSE;
        TmpIntermediate.Parameters[0].Parameter.ParameterType := PARAM_R;
        TmpIntermediate.Parameters[0].Parameter.Address := TmpIntermediate.Output.Address;
        TmpIntermediate.Parameters[0].Parameter.Neg := FALSE;

        swizzle := Ord(TmpIntermediate.Output.Mask[0]) or (Ord(TmpIntermediate.Output.Mask[1]) shl 1) or (Ord(TmpIntermediate.Output.Mask[2]) shl 2) or (Ord(TmpIntermediate.Output.Mask[3]) shl 3);
        case (swizzle) of
          1: begin
            VshSetSwizzle(@TmpIntermediate.Parameters[0], SWIZZLE_X, SWIZZLE_X, SWIZZLE_X, SWIZZLE_X);
          end;
          2: begin
            VshSetSwizzle(@TmpIntermediate.Parameters[0], SWIZZLE_Y, SWIZZLE_Y, SWIZZLE_Y, SWIZZLE_Y);
          end;
          4: begin
            VshSetSwizzle(@TmpIntermediate.Parameters[0], SWIZZLE_Z, SWIZZLE_Z, SWIZZLE_Z, SWIZZLE_Z);
          end;
          8: begin
            VshSetSwizzle(@TmpIntermediate.Parameters[0], SWIZZLE_W, SWIZZLE_W, SWIZZLE_W, SWIZZLE_W);
          end;
        // 15: begin
        else // default:
          VshSetSwizzle(@TmpIntermediate.Parameters[0], SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_W);
          break;
        end;
        //VshSetSwizzle(@TmpIntermediate.Parameters[0], SWIZZLE_W, SWIZZLE_W, SWIZZLE_W, SWIZZLE_W);
        VshSetSwizzle(@TmpIntermediate.Parameters[1], SWIZZLE_W, SWIZZLE_W, SWIZZLE_W, SWIZZLE_W);
        //VshSetOutputMask(@TmpIntermediate.Output, FALSE, FALSE, FALSE, TRUE);
        VshInsertIntermediate(pShader, @TmpIntermediate, i + 1);
      end;
      Inc(i);
    end; // if

    Inc(i);
  end; // while

  R12Replacement := -1;
  if (RUsage[12]) then
  begin
    // Sigh, they absolutely had to use r12, didn't they?
    for i := 11 downto 0 do
    begin
      if (not RUsage[i]) then
      begin
        R12Replacement := i;
        break;
      end;
    end;

    if (R12Replacement = -1) then
    begin
      EmuWarning('Vertex shader uses all r registers, including r12; impossible to convert!');
      Result := FALSE;
      Exit;
    end;

    if pShader.IntermediateCount > 0 then // Dxbx addition, to prevent underflow
    for j := 0 to pShader.IntermediateCount - 1 do
    begin
      pIntermediate := @pShader.Intermediate[j];
      if (pIntermediate.Output.Type_ = IMD_OUTPUT_O) and
         (pIntermediate.Output.Address = Word(Ord(OREG_OPOS))) then
      begin
        // Found instruction writing to oPos
        pIntermediate.Output.Type_ := IMD_OUTPUT_R;
        pIntermediate.Output.Address := R12Replacement;
      end;

      for k := 0 to 3-1 do
      begin
        if (pIntermediate.Parameters[k].Active) then
        begin
          if (pIntermediate.Parameters[k].Parameter.ParameterType = PARAM_R) and
             (pIntermediate.Parameters[k].Parameter.Address = 12) then
          begin
            // Found a r12 used as a parameter; replace
            pIntermediate.Parameters[k].Parameter.Address := R12Replacement;
          end
          else if (pIntermediate.Parameters[k].Parameter.ParameterType = PARAM_C) and
                  (pIntermediate.Parameters[k].Parameter.Address = ({58=}X_VSCM_RESERVED_CONSTANT1{=-38}+X_VSCM_CORRECTION{=96})) and
                  (not pIntermediate.Parameters[k].IsA0X) then
          begin
            // Found c-38, replace it with r12.w
            pIntermediate.Parameters[k].Parameter.ParameterType := PARAM_R;
            pIntermediate.Parameters[k].Parameter.Address := R12Replacement;
            VshSetSwizzle(@pIntermediate.Parameters[k], SWIZZLE_W, SWIZZLE_W, SWIZZLE_W, SWIZZLE_W);
          end;
        end;
      end;
    end;

    // Insert mov oPos, r## in the end
    pOPosWriteBack := VshNewIntermediate(pShader);
    pOPosWriteBack.InstructionType := IMD_ILU;
    pOPosWriteBack.ILU := ILU_MOV;
    pOPosWriteBack.MAC := MAC_NOP;
    pOPosWriteBack.Output.Type_ := IMD_OUTPUT_O;
    pOPosWriteBack.Output.Address := Ord(OREG_OPOS);
    VshSetOutputMask(@pOPosWriteBack.Output, TRUE, TRUE, TRUE, TRUE);
    pOPosWriteBack.Parameters[0].Active := TRUE;
    pOPosWriteBack.Parameters[0].Parameter.ParameterType := PARAM_R;
    pOPosWriteBack.Parameters[0].Parameter.Address := R12Replacement;
    VshSetSwizzle(@pOPosWriteBack.Parameters[0], SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_W);
  end;
  Result := TRUE;
end;

// ****************************************************************************
// * Vertex shader declaration recompiler
// ****************************************************************************

type _VSH_TYPE_PATCH_DATA = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    NbrTypes: DWORD;
    Types: array [0..256-1] of UINT;
  end; // size = 1028 (as in Cxbx)
  VSH_TYPE_PATCH_DATA = _VSH_TYPE_PATCH_DATA;

type _VSH_STREAM_PATCH_DATA = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    NbrStreams: DWORD;
    pStreamPatches: array [0..256-1] of STREAM_DYNAMIC_PATCH;
  end; // size = 4100 (as in Cxbx)
  VSH_STREAM_PATCH_DATA = _VSH_STREAM_PATCH_DATA;

type _VSH_PATCH_DATA = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    NeedPatching: boolean;
    ConvertedStride: DWORD;
    TypePatchData: VSH_TYPE_PATCH_DATA;
    StreamPatchData: VSH_STREAM_PATCH_DATA;
  end; // size = 5136 (as in Cxbx)
  VSH_PATCH_DATA = _VSH_PATCH_DATA;
  PVSH_PATCH_DATA = ^VSH_PATCH_DATA;

// VERTEX SHADER
const DEF_VSH_END = $FFFFFFFF;
const DEF_VSH_NOP = $00000000;

function VshGetDeclarationSize(pDeclaration: PDWORD): DWORD;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  Pos: DWORD;
begin
  Pos := 0;
  while PDWord(UIntPtr(pDeclaration) + (Pos * sizeof(DWORD)))^ <> DEF_VSH_END do
  begin
    Inc(Pos);
  end;
  Result := (Pos + 1) * sizeof(DWORD);
end;

function Xb2PCRegisterType(VertexRegister: DWORD): DWORD;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  PCRegisterType: DWORD;
begin
  case Integer(VertexRegister) of
  -1: begin
      DbgVshPrintf('D3DVSDE_VERTEX /* xbox ext. */');
      PCRegisterType := DWORD(-1);
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
      PCRegisterType := DWORD(-1);
    end;
  7: begin
      DbgVshPrintf('D3DVSDE_BACKDIFFUSE /* xbox ext. */');
      PCRegisterType := DWORD(-1);
    end;
  8: begin
      DbgVshPrintf('D3DVSDE_BACKSPECULAR /* xbox ext. */');
      PCRegisterType := DWORD(-1);
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
    PCRegisterType := DWORD(-1);
  end;

  Result := PCRegisterType;
end; // Xb2PCRegisterType

function VshGetTokenType(Token: DWORD): DWORD; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := (Token and D3DVSD_TOKENTYPEMASK) shr D3DVSD_TOKENTYPESHIFT;
end;

function VshGetVertexRegister(Token: DWORD): DWORD; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := (Token and D3DVSD_VERTEXREGMASK) shr D3DVSD_VERTEXREGSHIFT;
end;

function VshGetVertexRegisterIn(Token: DWORD): DWORD; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := (Token and D3DVSD_VERTEXREGINMASK) shr D3DVSD_VERTEXREGINSHIFT;
end;

function VshGetVertexStream(Token: DWORD): DWORD; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := (Token and D3DVSD_STREAMNUMBERMASK) shr D3DVSD_STREAMNUMBERSHIFT;
end;

procedure VshConvertToken_NOP(pToken: PDWORD);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  // D3DVSD_NOP
  if (pToken^ <> DEF_VSH_NOP) then
  begin
    EmuWarning('Token NOP found, but extra parameters are given!');
  end;
  DbgVshPrintf(#9'D3DVSD_NOP(),'#13#10);
end;

function VshConvertToken_CONSTMEM(pToken: PDWORD): DWORD;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  ConstantAddress: DWORD;
  Count: DWORD;
  i: int;
begin
  // D3DVSD_CONST
  DbgVshPrintf(#9'D3DVSD_CONST(');
  ConstantAddress := ((pToken^ shr D3DVSD_CONSTADDRESSSHIFT) and $FF);
  Count           := (pToken^ and D3DVSD_CONSTCOUNTMASK) shr D3DVSD_CONSTCOUNTSHIFT;

  DbgVshPrintf('%d, %d),'#13#10, [ConstantAddress, Count]);

  //pToken = D3DVSD_CONST(ConstantAddress, Count);

  if Count > 0 then // Dxbx addition, to prevent underflow
  for i := 0 to Count - 1 do
  begin
    DbgVshPrintf(#9'0x%08X,'#13#10, [pToken]);
  end;
  Result := Count;
end; // VshConvertToken_CONSTMEM

procedure VshConverToken_TESSELATOR(pToken: PDWORD; 
                                    IsFixedFunction: boolean);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  VertexRegister: DWORD;
  NewVertexRegister: DWORD;
  VertexRegisterIn: DWORD;
  VertexRegisterOut: DWORD;
  NewVertexRegisterIn: DWORD;
  NewVertexRegisterOut: DWORD;
begin
  // TODO -oCXBX: Investigate why Xb2PCRegisterType is only used for fixed function vertex shaders
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

    DbgVshPrintf('),'#13#10);

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

    DbgVshPrintf('),'#13#10);
    pToken^ := D3DVSD_TESSNORMAL(NewVertexRegisterIn, NewVertexRegisterOut);
  end;
end; // VshConverToken_TESSELATOR

function VshAddStreamPatch(pPatchData: PVSH_PATCH_DATA): boolean;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  CurrentStream: int;
  pStreamPatch: STREAM_DYNAMIC_PATCH;
begin
  CurrentStream := int(pPatchData.StreamPatchData.NbrStreams) - 1;

  if (CurrentStream >= 0) then
  begin
    DbgVshPrintf('NeedPatching: %d'#13#10, [Ord(pPatchData.NeedPatching)]);

    pStreamPatch := pPatchData.StreamPatchData.pStreamPatches[CurrentStream];

    pStreamPatch.ConvertedStride := pPatchData.ConvertedStride;
    pStreamPatch.NbrTypes := pPatchData.TypePatchData.NbrTypes;
    pStreamPatch.NeedPatch := pPatchData.NeedPatching;
    // 2010/01/12 - revel8n - fixed allocated data size and type
    pStreamPatch.pTypes := PUINTs(DxbxMalloc(pPatchData.TypePatchData.NbrTypes * sizeof(UINT)));
    memcpy(pStreamPatch.pTypes, @(pPatchData.TypePatchData.Types[0]), pPatchData.TypePatchData.NbrTypes * sizeof(UINT));

    Result := TRUE;
    Exit;
  end;

  Result := FALSE;
end; // VshAddStreamPatch

procedure VshConvertToken_STREAM(pToken: PDWORD; 
                                 pPatchData: PVSH_PATCH_DATA);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  StreamNumber: DWORD;
begin
  // D3DVSD_STREAM_TESS
  if (pToken^ and D3DVSD_STREAMTESSMASK) > 0 then
  begin
    DbgVshPrintf(#9'D3DVSD_STREAM_TESS(),'#13#10);
  end
  // D3DVSD_STREAM
  else
  begin
    StreamNumber := VshGetVertexStream(pToken^);
    DbgVshPrintf(#9'D3DVSD_STREAM(%d),'#13#10, [StreamNumber]);

    // new stream
    // copy current data to structure
    if (VshAddStreamPatch(pPatchData)) then
    begin
      pPatchData.ConvertedStride := 0;
      pPatchData.TypePatchData.NbrTypes := 0;
      pPatchData.NeedPatching := FALSE;
    end;

    Inc(pPatchData.StreamPatchData.NbrStreams);
  end;
end; // VshConvertToken_STREAM

procedure VshConvertToken_STREAMDATA_SKIP(pToken: PDWORD);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  SkipCount: DWORD;
begin
  SkipCount := (pToken^ and D3DVSD_SKIPCOUNTMASK) shr D3DVSD_SKIPCOUNTSHIFT;
  DbgVshPrintf(#9'D3DVSD_SKIP(%d),'#13#10, [SkipCount]);
end;

procedure VshConvertToken_STREAMDATA_SKIPBYTES(pToken: PDWORD);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  SkipBytesCount: DWORD;
begin
  SkipBytesCount := (pToken^ and D3DVSD_SKIPCOUNTMASK) shr D3DVSD_SKIPCOUNTSHIFT;
  DbgVshPrintf(#9'D3DVSD_SKIPBYTES(%d), /* xbox ext. */'#13#10, [SkipBytesCount]);
  if (SkipBytesCount mod sizeof(DWORD)) > 0 then
  begin
    EmuWarning('D3DVSD_SKIPBYTES can''t be converted to D3DVSD_SKIP, not divisble by 4.');
  end;
  pToken^ := D3DVSD_SKIP(SkipBytesCount div sizeof(DWORD));
end;

procedure VshConvertToken_STREAMDATA_REG(pToken: PDWORD;
                                         IsFixedFunction: boolean;
                                         pPatchData: PVSH_PATCH_DATA);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  VertexRegister: DWORD;
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

  case(DataType) of
    { $12=}X_D3DVSDT_FLOAT1: begin
      DbgVshPrintf('D3DVSDT_FLOAT1');
      NewDataType := D3DVSDT_FLOAT1;
      Inc(pPatchData.ConvertedStride, sizeof(FLOAT));
    end;
    { $22=}X_D3DVSDT_FLOAT2: begin
      DbgVshPrintf('D3DVSDT_FLOAT2');
      NewDataType := D3DVSDT_FLOAT2;
      Inc(pPatchData.ConvertedStride, 2*sizeof(FLOAT));
    end;
    { $32=}X_D3DVSDT_FLOAT3: begin
      DbgVshPrintf('D3DVSDT_FLOAT3');
      NewDataType := D3DVSDT_FLOAT3;
      Inc(pPatchData.ConvertedStride, 3*sizeof(FLOAT));
    end;
    { $42=}X_D3DVSDT_FLOAT4: begin
      DbgVshPrintf('D3DVSDT_FLOAT4');
      NewDataType := D3DVSDT_FLOAT4;
      Inc(pPatchData.ConvertedStride, 4*sizeof(FLOAT));
    end;
    { $40=}X_D3DVSDT_D3DCOLOR: begin
      DbgVshPrintf('D3DVSDT_D3DCOLOR');
      NewDataType := D3DVSDT_D3DCOLOR;
      Inc(pPatchData.ConvertedStride, sizeof(D3DCOLOR));
    end;
    { $25=}X_D3DVSDT_SHORT2: begin
      DbgVshPrintf('D3DVSDT_SHORT2');
      NewDataType := D3DVSDT_SHORT2;
      Inc(pPatchData.ConvertedStride, 2*sizeof(SHORT));
    end;
    { $45=}X_D3DVSDT_SHORT4: begin
      DbgVshPrintf('D3DVSDT_SHORT4');
      NewDataType := D3DVSDT_SHORT4;
      Inc(pPatchData.ConvertedStride, 4*sizeof(SHORT));
    end;
    { $11=}X_D3DVSDT_NORMSHORT1: begin
      DbgVshPrintf('D3DVSDT_NORMSHORT1 /* xbox ext. */');
      NewDataType := D3DVSDT_SHORT2; // hmm, emulation?
      Inc(pPatchData.ConvertedStride, 2*sizeof(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    { $21=}X_D3DVSDT_NORMSHORT2: begin
      DbgVshPrintf('D3DVSDT_NORMSHORT2 /* xbox ext. */');
      NewDataType := D3DVSDT_SHORT2;
      Inc(pPatchData.ConvertedStride, 2*sizeof(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    { $31=}X_D3DVSDT_NORMSHORT3: begin
      DbgVshPrintf('D3DVSDT_NORMSHORT3 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_SHORT4;
      Inc(pPatchData.ConvertedStride, 4*sizeof(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    { $41=}X_D3DVSDT_NORMSHORT4: begin
      DbgVshPrintf('D3DVSDT_NORMSHORT4 /* xbox ext. */');
      NewDataType := D3DVSDT_SHORT4;
      Inc(pPatchData.ConvertedStride, 4*sizeof(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    { $16=}X_D3DVSDT_NORMPACKED3: begin
      DbgVshPrintf('D3DVSDT_NORMPACKED3 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_FLOAT3;//$FF; //32bit
      Inc(pPatchData.ConvertedStride, 3*sizeof(FLOAT));
      pPatchData.NeedPatching := TRUE;
    end;
    { $15=}X_D3DVSDT_SHORT1: begin
      DbgVshPrintf('D3DVSDT_SHORT1 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_SHORT2;
      Inc(pPatchData.ConvertedStride, 2*sizeof(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    { $35=}X_D3DVSDT_SHORT3: begin
      DbgVshPrintf('D3DVSDT_SHORT3 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_SHORT4;
      Inc(pPatchData.ConvertedStride, 4*sizeof(SHORT));
      pPatchData.NeedPatching := TRUE;
    end;
    { $14=}X_D3DVSDT_PBYTE1: begin
      DbgVshPrintf('D3DVSDT_PBYTE1 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_FLOAT1;
      Inc(pPatchData.ConvertedStride, 1*sizeof(FLOAT));
      pPatchData.NeedPatching := TRUE;
    end;
    { $24=}X_D3DVSDT_PBYTE2: begin
      DbgVshPrintf('D3DVSDT_PBYTE2 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_FLOAT2;
      Inc(pPatchData.ConvertedStride, 2*sizeof(FLOAT));
      pPatchData.NeedPatching := TRUE;
    end;
    { $34=}X_D3DVSDT_PBYTE3: begin
      DbgVshPrintf('D3DVSDT_PBYTE3 /* xbox ext. nsp */');
      NewDataType := D3DVSDT_FLOAT3;
      Inc(pPatchData.ConvertedStride, 3*sizeof(FLOAT));
      pPatchData.NeedPatching := TRUE;
    end;
    { $44=}X_D3DVSDT_PBYTE4: begin
      DbgVshPrintf('D3DVSDT_PBYTE4 /* xbox ext. */');
      NewDataType := D3DVSDT_FLOAT4;
      Inc(pPatchData.ConvertedStride, 4*sizeof(FLOAT));
    end;
    { $72=}X_D3DVSDT_FLOAT2H: begin
      DbgVshPrintf('D3DVSDT_FLOAT2H /* xbox ext. */');
      NewDataType := D3DVSDT_FLOAT3;
      Inc(pPatchData.ConvertedStride, 3*sizeof(FLOAT));
      pPatchData.NeedPatching := TRUE;
    end;
    { $02=}X_D3DVSDT_NONE: begin
      DbgVshPrintf('D3DVSDT_NONE /* xbox ext. nsp */');
      // TODO -oDxbx: Use D3DVSD_NOP ?
      NewDataType := $FF;
    end;
  else // default:
    DbgVshPrintf('Unknown data type for D3DVSD_REG: 0x%02X'#13#10, [DataType]);
  end;
  pToken^ := D3DVSD_REG(NewVertexRegister, NewDataType);

  DbgVshPrintf('),'#13#10);

  if (NewDataType = $FF) then
  begin
    EmuWarning('/* WARNING: Fatal type mismatch, no fitting type! */');
  end;
end; // VshConvertToken_STREAMDATA_REG

procedure VshConvertToken_STREAMDATA(pToken: PDWORD;
                                     IsFixedFunction: boolean;
                                     pPatchData: PVSH_PATCH_DATA);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
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

function VshRecompileToken(pToken: PDWORD; 
                           IsFixedFunction: boolean; 
                           pPatchData: PVSH_PATCH_DATA): DWORD;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
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
    DbgVshPrintf('Unknown token type: %d'#13#10, [VshGetTokenType(pToken^)]);
  end;

  Result := Step;
end; // VshRecompileToken

// recompile xbox vertex shader declaration
function XTL_EmuRecompileVshDeclaration
(
  pDeclaration: PDWORD;
  ppRecompiledDeclaration: PPDWORD;
  pDeclarationSize: PDWORD;
  IsFixedFunction: boolean;
  pVertexDynamicPatch: PVERTEX_DYNAMIC_PATCH
): DWORD;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  DeclarationSize: DWORD;
  pRecompiled: PDWORD;
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
  ppRecompiledDeclaration^ := PDWORD(DxbxMalloc(DeclarationSize));
  pRecompiled := ppRecompiledDeclaration^;
  memcpy(pRecompiled, pDeclaration, DeclarationSize);
  pDeclarationSize^ := DeclarationSize;

  // TODO -oCXBX: Put these in one struct
  ZeroMemory(@PatchData, SizeOf(PatchData));

  DbgVshPrintf('DWORD dwVSHDecl[] ='#13#10'{'#13#10);

  while pRecompiled^ <> DEF_VSH_END do
  begin
    Step := VshRecompileToken(pRecompiled, IsFixedFunction, @PatchData);
    Inc(pRecompiled, Step); // Older Delphi's might need : Inc(UIntPtr(pRecompiled), Step * SizeOf(DWord));
  end;
  DbgVshPrintf(#9'D3DVSD_END()'#13#10'};'#13#10);

  VshAddStreamPatch(@PatchData);

  DbgVshPrintf('NbrStreams: %d'#13#10, [PatchData.StreamPatchData.NbrStreams]);

  // Copy the patches to the vertex shader struct
  StreamsSize := PatchData.StreamPatchData.NbrStreams * sizeof(STREAM_DYNAMIC_PATCH);
  pVertexDynamicPatch.NbrStreams := PatchData.StreamPatchData.NbrStreams;
  pVertexDynamicPatch.pStreamPatches := PSTREAM_DYNAMIC_PATCHs(DxbxMalloc(StreamsSize));
  memcpy(pVertexDynamicPatch.pStreamPatches,
         @(PatchData.StreamPatchData.pStreamPatches[0]),
         StreamsSize);

  Result := D3D_OK;
end; // XTL_EmuRecompileVshDeclaration

// recompile xbox vertex shader function
function XTL_EmuRecompileVshFunction
(
    pFunction: PDWORD;
    ppRecompiled: XTL_PLPD3DXBUFFER;
    pOriginalSize: PDWORD;
    bNoReservedConstants: boolean
): HRESULT;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  pShaderHeader: PVSH_SHADER_HEADER;
  pToken: Puint32;//PDWORD;
  EOI: boolean;
  pShader: PVSH_XBOX_SHADER;
  hRet: HRESULT;
  Inst: VSH_SHADER_INSTRUCTION;
  pShaderDisassembly: P_char;
  pErrors: XTL_LPD3DXBUFFER;
begin
  pShaderHeader := PVSH_SHADER_HEADER(pFunction);
  EOI := false;
  pShader := PVSH_XBOX_SHADER(DxbxMalloc(sizeof(VSH_XBOX_SHADER)));
  hRet := 0;
  pErrors := nil;

  // TODO -oCXBX: support this situation..
  if(pFunction = NULL) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  ppRecompiled^ := NULL;
  pOriginalSize^ := 0;
  if(nil=pShader) then
  begin
    EmuWarning('Couldn''t allocate memory for vertex shader conversion buffer');
    hRet := E_OUTOFMEMORY;
  end;
  memset(pShader, 0, sizeof(VSH_XBOX_SHADER));
  pShader.ShaderHeader := pShaderHeader^;
  case(pShaderHeader.Version) of
    VERSION_XVS: 
      ;
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
    pToken := Puint32(UIntPtr(pFunction) + sizeof(VSH_SHADER_HEADER));
    while not EOI do
    begin
      VshParseInstruction(pToken, @Inst);
      VshConvertToIntermediate(@Inst, pShader);
      EOI := boolean(VshGetField(pToken, FLD_FINAL) > 0);
      Inc(pToken, VSH_INSTRUCTION_SIZE);
    end;

    // The size of the shader is
    pOriginalSize^ := DWORD(pToken) - DWORD(pFunction);

    pShaderDisassembly := P_char(DxbxMalloc(pShader.IntermediateCount * 50)); // Should be plenty
    DbgVshPrintf('-- Before conversion --'#13#10);
    VshWriteShader(pShader, pShaderDisassembly, FALSE);
    DbgVshPrintf('%s', [pShaderDisassembly]);
    DbgVshPrintf('-----------------------'#13#10);

    VshConvertShader(pShader, bNoReservedConstants);
    VshWriteShader(pShader, pShaderDisassembly, TRUE);

    DbgVshPrintf('-- After conversion ---'#13#10);
    DbgVshPrintf('%s', [pShaderDisassembly]);
    DbgVshPrintf('-----------------------'#13#10);


//{$IFDEF GAME_HACKS_ENABLED}??
    // HACK: Azurik. Prevent Direct3D from trying to assemble this.
    if(0=strcmp(pShaderDisassembly, 'vs.1.1'#13#10)) then
    begin
      EmuWarning('Cannot assemble empty vertex shader!');
      hRet := D3DXERR_INVALIDDATA;
    end
    else
      hRet := D3DXAssembleShader(pShaderDisassembly,
                                strlen(pShaderDisassembly),
                                {Flags=}D3DXASM_SKIPVALIDATION,
                                {ppConstants=}NULL,
                                {ppCompiledShader=}PID3DXBuffer(ppRecompiled),
                                {ppCompilationErrors=}@pErrors); // Dxbx addition

    if (FAILED(hRet)) then
    begin
      EmuWarning('Couldn''t assemble recompiled vertex shader');
      //EmuWarning(string(AnsiString(PAnsiChar(ID3DXBuffer(pErrors).GetBufferPointer)))); // Dxbx addition
    end;

    // Dxbx addition : Release interface reference manually :
    if Assigned(pErrors) then
    begin
      ID3DXBuffer(pErrors)._Release;
      pErrors := nil;
    end;

    DxbxFree(pShaderDisassembly);
  end;

  DxbxFree(pShader);
  Result := hRet;
end; // XTL_EmuRecompileVshFunction

procedure XTL_FreeVertexDynamicPatch(pVertexShader: PVERTEX_SHADER);
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  i: DWORD;
begin
  if pVertexShader.VertexDynamicPatch.NbrStreams > 0 then // Dxbx addition, to prevent underflow
  for i := 0 to pVertexShader.VertexDynamicPatch.NbrStreams - 1 do
  begin
    DxbxFree(pVertexShader.VertexDynamicPatch.pStreamPatches[i].pTypes);
    pVertexShader.VertexDynamicPatch.pStreamPatches[i].pTypes := nil;
  end;
  DxbxFree(pVertexShader.VertexDynamicPatch.pStreamPatches);
  pVertexShader.VertexDynamicPatch.pStreamPatches := NULL;
  pVertexShader.VertexDynamicPatch.NbrStreams := 0;
end;

function IsValidCurrentShader(): boolean;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  // Dxbx addition : There's no need to go to XboxFS and call
  // XTL_EmuIDirect3DDevice8_GetVertexShader, just check g_CurrentVertexShader :
  Result := VshHandleIsValidShader(g_CurrentVertexShader);
end; // IsValidCurrentShader

// Checks for failed vertex shaders, and shaders that would need patching
function VshHandleIsValidShader(aHandle: DWORD): boolean;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  pD3DVertexShader: PX_D3DVertexShader;
  pVertexShader: PVERTEX_SHADER;
begin
  if (VshHandleIsVertexShader(aHandle)) then
  begin
    pD3DVertexShader := VshHandleGetVertexShader(aHandle);
    pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);
    if (pVertexShader.Status <> 0) then
    begin
      Result := FALSE;
      Exit;
    end;
    (* Cxbx has this disabled :
    if pVertexShader.VertexDynamicPatch.NbrStreams > 0 then // Dxbx addition, to prevent underflow
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
    *)
  end;

  Result := TRUE;
end; // IsValidShaderHandle

function VshGetVertexDynamicPatch(Handle: DWORD): PVERTEX_DYNAMIC_PATCH;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  pD3DVertexShader: PX_D3DVertexShader;
  pVertexShader: PVERTEX_SHADER;
  i: uint32;
begin
  pD3DVertexShader := VshHandleGetVertexShader(Handle);
  pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);

  if pVertexShader.VertexDynamicPatch.NbrStreams > 0 then // Dxbx addition, to prevent underflow
  for i := 0 to pVertexShader.VertexDynamicPatch.NbrStreams - 1 do
  begin
    if (pVertexShader.VertexDynamicPatch.pStreamPatches[i].NeedPatch) then
    begin
      Result := @pVertexShader.VertexDynamicPatch;
      Exit;
    end;
  end;
  Result := NULL;
end; // VshGetVertexDynamicPatch

{.$MESSAGE 'PatrickvL reviewed up to here'}
end.
