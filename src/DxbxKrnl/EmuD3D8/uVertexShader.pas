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

{$INCLUDE ..\..\Dxbx.inc}

interface

implementation

// ****************************************************************************
// * Vertex shader function recompiler
// ****************************************************************************

// Local macros
const VERSION_VS =                     $F0; // vs.1.1, not an official value
const VERSION_XVS =                    $20; // Xbox vertex shader
const VERSION_XVSS =                   $73; // Xbox vertex state shader
const VERSION_XVSW =                   $77; // Xbox vertex read/write shader
const VSH_XBOX_MAX_INSTRUCTION_COUNT = 136;  // The maximum Xbox shader instruction count
const VSH_MAX_INTERMEDIATE_COUNT =     1024; // The maximum number of intermediate format slots

// Local types
type	(**** Convert following enum types to constants. ****	 **** e.g. v1 = n, where v1 is constant and n is the value ****	 **** if a constant has a value, do not assign a new value ****) _VSH_FIELD_NAME
begin 
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
 end;
VSH_FIELD_NAME;

type	(**** Convert following enum types to constants. ****	 **** e.g. v1 = n, where v1 is constant and n is the value ****	 **** if a constant has a value, do not assign a new value ****) _VSH_OREG_NAME
begin 
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
 end;
VSH_OREG_NAME;

type	(**** Convert following enum types to constants. ****	 **** e.g. v1 = n, where v1 is constant and n is the value ****	 **** if a constant has a value, do not assign a new value ****) _VSH_PARAMETER_TYPE
begin 
    PARAM_UNKNOWN = 0,
    PARAM_R,
    PARAM_V,
    PARAM_C
 end;
VSH_PARAMETER_TYPE;

type	(**** Convert following enum types to constants. ****	 **** e.g. v1 = n, where v1 is constant and n is the value ****	 **** if a constant has a value, do not assign a new value ****) _VSH_OUTPUT_TYPE
begin 
    OUTPUT_C = 0,
    OUTPUT_O
 end;
VSH_OUTPUT_TYPE;

type	(**** Convert following enum types to constants. ****	 **** e.g. v1 = n, where v1 is constant and n is the value ****	 **** if a constant has a value, do not assign a new value ****) _VSH_OUTPUT_MUX
begin 
    OMUX_MAC = 0,
    OMUX_ILU
 end;
VSH_OUTPUT_MUX;

type	(**** Convert following enum types to constants. ****	 **** e.g. v1 = n, where v1 is constant and n is the value ****	 **** if a constant has a value, do not assign a new value ****) _VSH_ILU
begin 
    ILU_NOP = 0,
    ILU_MOV,
    ILU_RCP,
    ILU_RCC,
    ILU_RSQ,
    ILU_EXP,
    ILU_LOG,
    ILU_LIT
 end;
VSH_ILU;

type	(**** Convert following enum types to constants. ****	 **** e.g. v1 = n, where v1 is constant and n is the value ****	 **** if a constant has a value, do not assign a new value ****) _VSH_MAC
begin 
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
 end;
VSH_MAC;

type	
VSH_OPCODE_PARAMS	= record  
    VSH_ILU   ILU;
    VSH_MAC   MAC;
    boolean   A;
    boolean   B;
    boolean   C;
 end;

type	(**** Convert following enum types to constants. ****	 **** e.g. v1 = n, where v1 is constant and n is the value ****	 **** if a constant has a value, do not assign a new value ****) _VSH_SWIZZLE
begin 
    SWIZZLE_X = 0,
    SWIZZLE_Y,
    SWIZZLE_Z,
    SWIZZLE_W
 end;
VSH_SWIZZLE;

type	
VSH_PARAMETER	= record  
    VSH_PARAMETER_TYPE  ParameterType;   // Parameter type, R, V or C
    boolean             Neg;             // TRUE if negated, FALSE if not
             Swizzle: array[0..4-1] of VSH_SWIZZLE;      // The four swizzles
    int16               Address;         // Register address
 end;

type	
VSH_OUTPUT	= record  
    // Output register
    VSH_OUTPUT_MUX      OutputMux;       // MAC or ILU used as output
    VSH_OUTPUT_TYPE     OutputType;      // C or O
                 OutputMask: array[0..4-1] of boolean;
    int16               OutputAddress;
    // MAC output R register
                 MACRMask: array[0..4-1] of boolean;
    boolean             MACRAddress;
    // ILU output R register
                 ILURMask: array[0..4-1] of boolean;
    boolean             ILURAddress;
 end;

// The raw, parsed shader instruction (can be many combined [paired] instructions)
type	
VSH_SHADER_INSTRUCTION	= record  
    VSH_ILU       ILU;
    VSH_MAC       MAC;
    VSH_OUTPUT    Output;
    VSH_PARAMETER A;
    VSH_PARAMETER B;
    VSH_PARAMETER C;
    boolean       a$;
 end;

type	(**** Convert following enum types to constants. ****	 **** e.g. v1 = n, where v1 is constant and n is the value ****	 **** if a constant has a value, do not assign a new value ****) _VSH_IMD_OUTPUT_TYPE
begin 
    IMD_OUTPUT_C,
    IMD_OUTPUT_R,
    IMD_OUTPUT_O,
    IMD_OUTPUT_A0X
 end;
VSH_IMD_OUTPUT_TYPE;

type	(**** Convert following enum types to constants. ****	 **** e.g. v1 = n, where v1 is constant and n is the value ****	 **** if a constant has a value, do not assign a new value ****) _VSH_IMD_INSTRUCTION_TYPE
begin 
    IMD_MAC,
    IMD_ILU
 end;
VSH_IMD_INSTRUCTION_TYPE;

type	
VSH_IMD_OUTPUT	= record  
    VSH_IMD_OUTPUT_TYPE cType;
                 Mask: array[0..4-1] of boolean;
    int16               Address;
 end;

type	
VSH_IMD_PARAMETER	= record  
    boolean         Active;
    VSH_PARAMETER   Parameter;
    boolean         IsA0X;
 end;

type	
VSH_INTERMEDIATE_FORMAT	= record  
    
    boolean                  IsCombined;
    VSH_IMD_INSTRUCTION_TYPE InstructionType;
    VSH_MAC                  MAC;
    VSH_ILU                  ILU;
    VSH_IMD_OUTPUT           Output;
            Parameters: array[0..3-1] of VSH_IMD_PARAMETER;
 end;

// Used for xvu spec definition
type	
VSH_FIELDMAPPING	= record  
    VSH_FIELD_NAME  FieldName;
    uint08          SubToken;
    uint08          StartBit;
    uint08          BitLength;
 end;

type	
VSH_XBOX_SHADER	= record  
    XTL.VSH_SHADER_HEADER       ShaderHeader;
    uint16                  IntermediateCount;
     Intermediate: array[0..VSH_MAX_INTERMEDIATE_COUNT-1] of VSH_INTERMEDIATE_FORMAT;
 end;

// Local constants
  VSH_FIELDMAPPING g_FieldMapping[] =
begin 
    // Field Name         DWORD BitPos BitSize
    begin   FLD_ILU,              1,   25,     3 ),
    begin   FLD_MAC,              1,   21,     4 ),
    begin   FLD_CONST,            1,   13,     8 ),
    begin   FLD_V,                1,    9,     4 ),
    // INPUT A
    begin   FLD_A_NEG,            1,    8,     1 ),
    begin   FLD_A_SWZ_X,          1,    6,     2 ),
    begin   FLD_A_SWZ_Y,          1,    4,     2 ),
    begin   FLD_A_SWZ_Z,          1,    2,     2 ),
    begin   FLD_A_SWZ_W,          1,    0,     2 ),
    begin   FLD_A_R,              2,   28,     4 ),
    begin   FLD_A_MUX,            2,   26,     2 ),
    // INPUT B
    begin   FLD_B_NEG,            2,   25,     1 ),
    begin   FLD_B_SWZ_X,          2,   23,     2 ),
    begin   FLD_B_SWZ_Y,          2,   21,     2 ),
    begin   FLD_B_SWZ_Z,          2,   19,     2 ),
    begin   FLD_B_SWZ_W,          2,   17,     2 ),
    begin   FLD_B_R,              2,   13,     4 ),
    begin   FLD_B_MUX,            2,   11,     2 ),
    // INPUT C
    begin   FLD_C_NEG,            2,   10,     1 ),
    begin   FLD_C_SWZ_X,          2,    8,     2 ),
    begin   FLD_C_SWZ_Y,          2,    6,     2 ),
    begin   FLD_C_SWZ_Z,          2,    4,     2 ),
    begin   FLD_C_SWZ_W,          2,    2,     2 ),
    begin   FLD_C_R_HIGH,         2,    0,     2 ),
    begin   FLD_C_R_LOW,          3,   30,     2 ),
    begin   FLD_C_MUX,            3,   28,     2 ),
    // Output
    begin   FLD_OUT_MAC_MASK_X,   3,   27,     1 ),
    begin   FLD_OUT_MAC_MASK_Y,   3,   26,     1 ),
    begin   FLD_OUT_MAC_MASK_Z,   3,   25,     1 ),
    begin   FLD_OUT_MAC_MASK_W,   3,   24,     1 ),
    begin   FLD_OUT_R,            3,   20,     4 ),
    begin   FLD_OUT_ILU_MASK_X,   3,   19,     1 ),
    begin   FLD_OUT_ILU_MASK_Y,   3,   18,     1 ),
    begin   FLD_OUT_ILU_MASK_Z,   3,   17,     1 ),
    begin   FLD_OUT_ILU_MASK_W,   3,   16,     1 ),
    begin   FLD_OUT_O_MASK_X,     3,   15,     1 ),
    begin   FLD_OUT_O_MASK_Y,     3,   14,     1 ),
    begin   FLD_OUT_O_MASK_Z,     3,   13,     1 ),
    begin   FLD_OUT_O_MASK_W,     3,   12,     1 ),
    begin   FLD_OUT_ORB,          3,   11,     1 ),
    begin   FLD_OUT_ADDRESS,      3,    3,     8 ),
    begin   FLD_OUT_MUX,          3,    2,     1 ),
    // Other
    begin   FLD_A0X,              3,    1,     1 ),
    begin   FLD_FINAL,            3,    0,     1  end;
);

  VSH_OPCODE_PARAMS g_OpCodeParams[] =
begin 
    // ILU OP   MAC OP  ParamA ParamB ParamC
    begin  ILU_MOV, MAC_NOP, FALSE, FALSE, TRUE  ),
    begin  ILU_RCP, MAC_NOP, FALSE, FALSE, TRUE  ),
    begin  ILU_RCC, MAC_NOP, FALSE, FALSE, TRUE  ),
    begin  ILU_RSQ, MAC_NOP, FALSE, FALSE, TRUE  ),
    begin  ILU_EXP, MAC_NOP, FALSE, FALSE, TRUE  ),
    begin  ILU_LOG, MAC_NOP, FALSE, FALSE, TRUE  ),
    begin  ILU_LIT, MAC_NOP, FALSE, FALSE, TRUE  ),
    begin  ILU_NOP, MAC_MOV, TRUE,  FALSE, FALSE ),
    begin  ILU_NOP, MAC_MUL, TRUE,  TRUE,  FALSE ),
    begin  ILU_NOP, MAC_ADD, TRUE,  FALSE, TRUE  ),
    begin  ILU_NOP, MAC_MAD, TRUE,  TRUE,  TRUE  ),
    begin  ILU_NOP, MAC_DP3, TRUE,  TRUE,  FALSE ),
    begin  ILU_NOP, MAC_DPH, TRUE,  TRUE,  FALSE ),
    begin  ILU_NOP, MAC_DP4, TRUE,  TRUE,  FALSE ),
    begin  ILU_NOP, MAC_DST, TRUE,  TRUE,  FALSE ),
    begin  ILU_NOP, MAC_MIN, TRUE,  TRUE,  FALSE ),
    begin  ILU_NOP, MAC_MAX, TRUE,  TRUE,  FALSE ),
    begin  ILU_NOP, MAC_SLT, TRUE,  TRUE,  FALSE ),
    begin  ILU_NOP, MAC_SGE, TRUE,  TRUE,  FALSE ),
    begin  ILU_NOP, MAC_ARL, TRUE,  FALSE, FALSE  end;
);

  PChar  MAC_OpCode[] = 
begin 
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
    'mov', // really "arl"
    '???',
    '???'
);

  PChar  ILU_OpCode[] = 
begin 
    'nop',
    'mov',
    'rcp',
    'rcc',
    'rsq',
    'exp',
    'log',
    'lit'
);

  PChar  OReg_Name[] =
begin 
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

  function IsInUse(var pMask: boolean): integer;
begin 
    result:= (pMask[0] or pMask[1] or pMask[2] or pMask[3]);
 end;

  boolean HasMACR(VSH_SHADER_INSTRUCTION *pInstruction)
begin 
    result:= IsInUse(pInstruction^.Output.MACRMask) and pInstruction^.MAC <> MAC_NOP;
 end;

  boolean HasMACO(VSH_SHADER_INSTRUCTION *pInstruction)
begin 
    result:= IsInUse(pInstruction^.Output.OutputMask) and 
            pInstruction^.Output.OutputMux = OMUX_MAC  and 
            pInstruction^.MAC <> MAC_NOP;
 end;

  boolean HasMACARL(VSH_SHADER_INSTRUCTION *pInstruction)
begin 
    result:= (*!IsInUse(pInstruction->Output.OutputMask) && 
            pInstruction^.Output.OutputMux = OMUX_MAC  and *)
            pInstruction^.MAC := MAC_ARL;
 end;

  boolean HasILUR(VSH_SHADER_INSTRUCTION *pInstruction)
begin 
    result:= IsInUse(pInstruction^.Output.ILURMask) and pInstruction^.ILU <> ILU_NOP;
 end;

  boolean HasILUO(VSH_SHADER_INSTRUCTION *pInstruction)
begin 
    result:= IsInUse(pInstruction^.Output.OutputMask) and 
            pInstruction^.Output.OutputMux = OMUX_ILU  and 
            pInstruction^.ILU <> ILU_NOP;
 end;

// Retrieves a number of bits in the instruction token
  function VshGetFromToken(var pShaderToken: uint32; SubToken: uint08; StartBit: uint08; BitLength: uint08): integer;
begin 
    result:= (pShaderToken[SubToken] shr StartBit) and ~($FFFFFFFF shl BitLength);
 end;

// Converts the C register address to disassembly format
  int16 ConvertCRegister( int16 CReg)
begin 
    result:= ((((CReg shr 5) and 7) - 3) * 32) + (CReg and 31);
 end;

uint08 VshGetField(uint32         *pShaderToken,
                   VSH_FIELD_NAME FieldName)
begin 
    result:= (uint08)(VshGetFromToken(pShaderToken,
                                   g_FieldMapping[FieldName].SubToken,
                                   g_FieldMapping[FieldName].StartBit,
                                   g_FieldMapping[FieldName].BitLength));
 end;

 VSH_OPCODE_PARAMS* VshGetOpCodeParams(VSH_ILU ILU,
                                             VSH_MAC MAC)
begin 
    integer i;

    for (i := 0; i < (SizeOf(g_OpCodeParams) / SizeOf(VSH_OPCODE_PARAMS)); i++)
    begin 
        if(ILU <> ILU_NOP and ILU = g_OpCodeParams[i].ILU  or then  
            MAC <> MAC_NOP and MAC = g_OpCodeParams[i].MAC)
        begin 
            result:= (VSH_OPCODE_PARAMS)@g_OpCodeParams[i];
         end;
     end;
    result:= 0;
 end;

  procedure VshParseInstruction(var pShaderToken: uint32;                          VSH_SHADER_INSTRUCTION *pInstruction)
begin 
    // First get the instruction(s).
    pInstruction^.ILU := (VSH_ILU)VshGetField(pShaderToken, FLD_ILU);
    pInstruction^.MAC := (VSH_MAC)VshGetField(pShaderToken, FLD_MAC);
    // Get parameter A
    pInstruction^.A.ParameterType := (VSH_PARAMETER_TYPE)VshGetField(pShaderToken, FLD_A_MUX);
    case(pInstruction^.A.ParameterType) of
    begin 
     PARAM_R:
        pInstruction^.A.Address := VshGetField(pShaderToken, FLD_A_R);
        break;
     PARAM_V:
        pInstruction^.A.Address := VshGetField(pShaderToken, FLD_V);
        break;
     PARAM_C:
        pInstruction^.A.Address := ConvertCRegister(VshGetField(pShaderToken, FLD_CONST));
        break;
    default:
        EmuWarning('Invalid instruction, parameter A ctype unknown  mod d', pInstruction^.A.ParameterType);
        Exit;
     end;
    pInstruction^.A.Neg := VshGetField(pShaderToken, FLD_A_NEG);
    pInstruction^.A.Swizzle[0] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_A_SWZ_X);
    pInstruction^.A.Swizzle[1] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_A_SWZ_Y);
    pInstruction^.A.Swizzle[2] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_A_SWZ_Z);
    pInstruction^.A.Swizzle[3] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_A_SWZ_W);
    // Get parameter B
    pInstruction^.B.ParameterType := (VSH_PARAMETER_TYPE)VshGetField(pShaderToken, FLD_B_MUX);
    case(pInstruction^.B.ParameterType) of
    begin 
     PARAM_R:
        pInstruction^.B.Address := VshGetField(pShaderToken, FLD_B_R);
        break;
     PARAM_V:
        pInstruction^.B.Address := VshGetField(pShaderToken, FLD_V);
        break;
     PARAM_C:
        pInstruction^.B.Address := ConvertCRegister(VshGetField(pShaderToken, FLD_CONST));
        break;
    default:
        DbgVshPrintf('Invalid instruction, parameter B ctype unknown  mod d', pInstruction^.B.ParameterType);
        Exit;
     end;
    pInstruction^.B.Neg := VshGetField(pShaderToken, FLD_B_NEG);
    pInstruction^.B.Swizzle[0] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_B_SWZ_X);
    pInstruction^.B.Swizzle[1] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_B_SWZ_Y);
    pInstruction^.B.Swizzle[2] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_B_SWZ_Z);
    pInstruction^.B.Swizzle[3] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_B_SWZ_W);
    // Get parameter C
    pInstruction^.C.ParameterType := (VSH_PARAMETER_TYPE)VshGetField(pShaderToken, FLD_C_MUX);
    case(pInstruction^.C.ParameterType) of
    begin 
     PARAM_R:
        pInstruction^.C.Address = VshGetField(pShaderToken, FLD_C_R_HIGH) shl 2  or 
                                  VshGetField(pShaderToken, FLD_C_R_LOW);
        break;
     PARAM_V:
        pInstruction^.C.Address := VshGetField(pShaderToken, FLD_V);
        break;
     PARAM_C:
        pInstruction^.C.Address := ConvertCRegister(VshGetField(pShaderToken, FLD_CONST));
        break;
    default:
        DbgVshPrintf('Invalid instruction, parameter C ctype unknown  mod d', pInstruction^.C.ParameterType);
        Exit;
     end;
    pInstruction^.C.Neg := VshGetField(pShaderToken, FLD_C_NEG);
    pInstruction^.C.Swizzle[0] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_C_SWZ_X);
    pInstruction^.C.Swizzle[1] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_C_SWZ_Y);
    pInstruction^.C.Swizzle[2] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_C_SWZ_Z);
    pInstruction^.C.Swizzle[3] := (VSH_SWIZZLE)VshGetField(pShaderToken, FLD_C_SWZ_W);
    // Get output
    // Output register
    pInstruction^.Output.OutputType := (VSH_OUTPUT_TYPE)VshGetField(pShaderToken, FLD_OUT_ORB);
    case(pInstruction^.Output.OutputType) of
    begin 
     OUTPUT_C:
        pInstruction^.Output.OutputAddress := ConvertCRegister(VshGetField(pShaderToken, FLD_OUT_ADDRESS));
        break;
     OUTPUT_O:
        pInstruction^.Output.OutputAddress := VshGetField(pShaderToken, FLD_OUT_ADDRESS) and $F;
        break;
     end;
    pInstruction^.Output.OutputMux := (VSH_OUTPUT_MUX)VshGetField(pShaderToken, FLD_OUT_MUX);
    pInstruction^.Output.OutputMask[0] := VshGetField(pShaderToken, FLD_OUT_O_MASK_X);
    pInstruction^.Output.OutputMask[1] := VshGetField(pShaderToken, FLD_OUT_O_MASK_Y);
    pInstruction^.Output.OutputMask[2] := VshGetField(pShaderToken, FLD_OUT_O_MASK_Z);
    pInstruction^.Output.OutputMask[3] := VshGetField(pShaderToken, FLD_OUT_O_MASK_W);
    // MAC output
    pInstruction^.Output.MACRMask[0] := VshGetField(pShaderToken, FLD_OUT_MAC_MASK_X);
    pInstruction^.Output.MACRMask[1] := VshGetField(pShaderToken, FLD_OUT_MAC_MASK_Y);
    pInstruction^.Output.MACRMask[2] := VshGetField(pShaderToken, FLD_OUT_MAC_MASK_Z);
    pInstruction^.Output.MACRMask[3] := VshGetField(pShaderToken, FLD_OUT_MAC_MASK_W);
    pInstruction^.Output.MACRAddress := VshGetField(pShaderToken, FLD_OUT_R);
    // ILU output
    pInstruction^.Output.ILURMask[0] := VshGetField(pShaderToken, FLD_OUT_ILU_MASK_X);
    pInstruction^.Output.ILURMask[1] := VshGetField(pShaderToken, FLD_OUT_ILU_MASK_Y);
    pInstruction^.Output.ILURMask[2] := VshGetField(pShaderToken, FLD_OUT_ILU_MASK_Z);
    pInstruction^.Output.ILURMask[3] := VshGetField(pShaderToken, FLD_OUT_ILU_MASK_W);
    pInstruction^.Output.ILURAddress := VshGetField(pShaderToken, FLD_OUT_R);
    // Finally, get a0.x indirect constant addressing
    pInstruction^.a$ := VshGetField(pShaderToken, FLD_A0X);
 end;

// Print functions
 function VshGetRegisterName(ParameterType: VSH_PARAMETER_TYPE): Char;
begin 
    case(ParameterType) of
    begin 
     PARAM_R:
        result:= 'r';
     PARAM_V:
        result:= 'v';
     PARAM_C:
        result:= 'c';
    default:
        result:= '?';
     end;
 end;

  procedure VshWriteOutputMask(var OutputMask: boolean;                         Char    *pDisassembly,
                               uint32  *pDisassemblyPos)
begin 
    if(OutputMask[0] and OutputMask[1] and OutputMask[2] and OutputMask[3]) then 
    begin 
        // All compoenents are there, no need to print the mask
        Exit;
     end;
    *pDisassemblyPos:= *pDisassemblyPos + StrFmt(pDisassembly + *pDisassemblyPos, '. mod s mod s mod s mod s',
                                OutputMask[0] ? 'x' : '',
                                OutputMask[1] ? 'y' : "",
                                OutputMask[2] ? "z" : "",
                                OutputMask[3] ? "w" : "");
 end;

  procedure VshWriteParameter(var pParameter: VSH_IMD_PARAMETER;                        Char              *pDisassembly,
                              uint32            *pDisassemblyPos)
begin 
    *pDisassemblyPos:= *pDisassemblyPos + StrFmt(pDisassembly + *pDisassemblyPos, ",  mod s mod c",
                                pParameter^.Parameter.Neg ? "-" : "",
                                VshGetRegisterName(pParameter^.Parameter.ParameterType));
    if(pParameter^.Parameter.ParameterType = PARAM_C and pParameter^.IsA0X) then 
    begin 
        // Only display the offset if it's not 0.
        if(pParameter^.Parameter.Address) then 
        begin 
            *pDisassemblyPos:= *pDisassemblyPos + StrFmt(pDisassembly + *pDisassemblyPos, "[a0.x+ mod d]", pParameter^.Parameter.Address);
         end;
        else
        begin 
            *pDisassemblyPos:= *pDisassemblyPos + StrFmt(pDisassembly + *pDisassemblyPos, "[a0.x]");
         end;
     end;
    else
    begin 
        *pDisassemblyPos:= *pDisassemblyPos + StrFmt(pDisassembly + *pDisassemblyPos, " mod d", pParameter^.Parameter.Address);
     end;
    // Only bother printing the swizzle if it is not .xyzw
    if( not (pParameter^.Parameter.Swizzle[0] = 0  and then  
          pParameter^.Parameter.Swizzle[1] = 1  and 
          pParameter^.Parameter.Swizzle[2] = 2  and 
          pParameter^.Parameter.Swizzle[3] = 3))
    begin 
        integer i;

        *pDisassemblyPos:= *pDisassemblyPos + StrFmt(pDisassembly + *pDisassemblyPos, ".");
        for (i := 0; i < 4; i++)
        begin 
            integer j;
            Char Swizzle := '?';
            case(pParameter^.Parameter.Swizzle[i]) of
            begin 
             SWIZZLE_X:
                Swizzle := 'x';
                break;
             SWIZZLE_Y:
                Swizzle := 'y';
                break;
             SWIZZLE_Z:
                Swizzle := 'z';
                break;
             SWIZZLE_W:
                Swizzle := 'w';
                break;
             end;
            *pDisassemblyPos:= *pDisassemblyPos + StrFmt(pDisassembly + *pDisassemblyPos, " mod c", Swizzle);
            for (j := i; j < 4; j++)
            begin 
                if(pParameter^.Parameter.Swizzle[i] <> pParameter^.Parameter.Swizzle[j]) then 
                begin 
                    break;
                 end;
             end;
            if(j = 4) then 
            begin 
                break;
             end;
         end;
     end;
 end;

  procedure VshWriteShader(var pShader: VSH_XBOX_SHADER;                     PChar  pDisassembly,
                           boolean Truncate)
begin 
    uint32 DisassemblyPos := 0;
    case(pShader^.ShaderHeader.Version) of
    begin 
         VERSION_VS:
            DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, "vs.1.1");
            break;
         VERSION_XVS:
            DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, "xvs.1.1");
            break;
         VERSION_XVSS:
            DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, "xvss.1.1");
            break;
         VERSION_XVSW:
            DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, "xvsw.1.1");
            break;
        default:
            break;
     end;
    for (integer i := 0; i < pShader^.IntermediateCount and (i < 128 or  not Truncate); i++)
    begin 
        VSH_INTERMEDIATE_FORMAT *pIntermediate := @pShader^.Intermediate[i];

        if(i = 128) then 
        begin 
            DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, "; -- Passing the truncation limit --");
         end;
        // Writing combining sign if neccessary
        if(pIntermediate^.IsCombined) then 
        begin 
            DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, "+");
         end;
        // Print the op code
        if(pIntermediate^.InstructionType = IMD_MAC) then 
        begin 
            DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, " mod s ", MAC_OpCode[pIntermediate^.MAC]);
         end;
        else
        begin 
            DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, " mod s ", ILU_OpCode[pIntermediate^.ILU]);
         end;

        // Print the output parameter
        if(pIntermediate^.Output.cType = IMD_OUTPUT_A0X) then 
        begin 
            DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, "a0.x");
         end;
        else
        begin 
            case(pIntermediate^.Output.cType) of
            begin 
             IMD_OUTPUT_C:
                DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, "c mod d", pIntermediate^.Output.Address);
                break;
             IMD_OUTPUT_R:
                DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, "r mod d", pIntermediate^.Output.Address);
                break;
             IMD_OUTPUT_O:
                DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, " mod s", OReg_Name[pIntermediate^.Output.Address]);
                break;
            default:
                CxbxKrnlCleanup("Invalid output register in vertex shader not ");
                break;
             end;
            VshWriteOutputMask(pIntermediate^.Output.Mask, pDisassembly, @DisassemblyPos);
         end;
        // Print the parameters
        for (integer i := 0; i < 3; i++)
        begin 
            VSH_IMD_PARAMETER *pParameter := @pIntermediate^.Parameters[i];
            if(pParameter^.Active) then 
            begin 
                VshWriteParameter(pParameter, pDisassembly, @DisassemblyPos);
             end;
         end;
        DisassemblyPos:= DisassemblyPos + StrFmt(pDisassembly + DisassemblyPos, "");
     end;
    *(pDisassembly + DisassemblyPos) := 0;
 end;

  procedure VshAddParameter(var pParameter: VSH_PARAMETER;                      boolean           a$,
                            VSH_IMD_PARAMETER *pIntermediateParameter)
begin 
    pIntermediateParameter^.Parameter := *pParameter;
    pIntermediateParameter^.Active    := TRUE;
    pIntermediateParameter^.IsA0X     := a$;
 end;

  procedure VshAddParameters(var pInstruction: VSH_SHADER_INSTRUCTION;                       VSH_ILU                 ILU,
                             VSH_MAC                 MAC,
                             VSH_IMD_PARAMETER       *pParameters)
begin 
    uint08 ParamCount := 0;
    VSH_OPCODE_PARAMS* pParams := VshGetOpCodeParams(ILU, MAC);

    // param A
    if(pParams^.A) then 
    begin 
        VshAddParameter(@pInstruction^.A, pInstruction^.a$, @pParameters[ParamCount]);
        ParamCount:= ParamCount + 1;
     end;

    // param B
    if(pParams^.B) then 
    begin 
        VshAddParameter(@pInstruction^.B, pInstruction^.a$, @pParameters[ParamCount]);
        ParamCount:= ParamCount + 1;
     end;

    // param C
    if(pParams^.C) then 
    begin 
        VshAddParameter(@pInstruction^.C, pInstruction^.a$, @pParameters[ParamCount]);
        ParamCount:= ParamCount + 1;
     end;
 end;

  procedure VshVerifyBufferBounds(var pShader: VSH_XBOX_SHADER);
    if(pShader^.IntermediateCount = VSH_MAX_INTERMEDIATE_COUNT) then 
    begin 
        CxbxKrnlCleanup("Shader exceeds conversion buffer not ");
     end;
 end;

 VSH_INTERMEDIATE_FORMAT *VshNewIntermediate(VSH_XBOX_SHADER *pShader)
begin 
    VshVerifyBufferBounds(pShader);

    ZeroMemory(@pShader^.Intermediate[pShader^.IntermediateCount], SizeOf(VSH_INTERMEDIATE_FORMAT));

    result:= @pShader^.Intermediate[pShader^.IntermediateCount++];
 end;

  procedure VshInsertIntermediate(var pShader: VSH_XBOX_SHADER;                            VSH_INTERMEDIATE_FORMAT *pIntermediate,
                                  uint16                  Pos)
begin 
    VshVerifyBufferBounds(pShader);
    
    for (integer i := pShader^.IntermediateCount; i >= Pos; i--)
    begin 
        pShader^.Intermediate[i + 1] := pShader^.Intermediate[i];
     end;
    pShader^.Intermediate[Pos] := *pIntermediate;
    pShader^.IntermediateCount:= pShader^.IntermediateCount + 1;
 end;

  procedure VshDeleteIntermediate(var pShader: VSH_XBOX_SHADER;                            uint16          Pos)
begin 
    for (integer i := Pos; i < (pShader^.IntermediateCount - 1); i++)
    begin 
        pShader^.Intermediate[i] := pShader^.Intermediate[i + 1];
     end;
    pShader^.IntermediateCount:= pShader^.IntermediateCount - 1;
 end;

 boolean VshAddInstructionMAC_R(VSH_SHADER_INSTRUCTION *pInstruction,
                                      VSH_XBOX_SHADER        *pShader,
                                      boolean                IsCombined)
begin 
    VSH_INTERMEDIATE_FORMAT *pIntermediate;
    if( not HasMACR(pInstruction)) then 
    begin 
        result:= FALSE;
     end;

    pIntermediate := VshNewIntermediate(pShader);
    pIntermediate^.IsCombined := IsCombined;

    // Opcode
    pIntermediate^.InstructionType := IMD_MAC;
    pIntermediate^.MAC := pInstruction^.MAC;

    // Output param
    pIntermediate^.Output.cType := IMD_OUTPUT_R;
    pIntermediate^.Output.Address := pInstruction^.Output.MACRAddress;
    memcpy(pIntermediate^.Output.Mask, pInstruction^.Output.MACRMask, SizeOf(boolean) * 4);

    // Other parameters
    VshAddParameters(pInstruction, ILU_NOP, pInstruction^.MAC, pIntermediate^.Parameters);

    result:= TRUE;
 end;

 boolean VshAddInstructionMAC_O(VSH_SHADER_INSTRUCTION* pInstruction,
                                      VSH_XBOX_SHADER        *pShader,
                                      boolean                IsCombined)
begin 
    VSH_INTERMEDIATE_FORMAT *pIntermediate;
    if( not HasMACO(pInstruction)) then 
    begin 
        result:= FALSE;
     end;

    pIntermediate := VshNewIntermediate(pShader);
    pIntermediate^.IsCombined := IsCombined;

    // Opcode
    pIntermediate^.InstructionType := IMD_MAC;
    pIntermediate^.MAC := pInstruction^.MAC;

    // Output param
    pIntermediate^.Output.cType := pInstruction^.Output.OutputType = OUTPUT_C ? IMD_OUTPUT_C : IMD_OUTPUT_O;
    pIntermediate^.Output.Address := pInstruction^.Output.OutputAddress;
    memcpy(pIntermediate^.Output.Mask, pInstruction^.Output.OutputMask, SizeOf(boolean) * 4);

    // Other parameters
    VshAddParameters(pInstruction, ILU_NOP, pInstruction^.MAC, pIntermediate^.Parameters);

    result:= TRUE;
 end;

 boolean VshAddInstructionMAC_ARL(VSH_SHADER_INSTRUCTION *pInstruction,
                                        VSH_XBOX_SHADER        *pShader,
                                        boolean                IsCombined)
begin 
    VSH_INTERMEDIATE_FORMAT *pIntermediate;
    if( not HasMACARL(pInstruction)) then 
    begin 
        result:= FALSE;
     end;

    pIntermediate := VshNewIntermediate(pShader);
    pIntermediate^.IsCombined := IsCombined;

    // Opcode
    pIntermediate^.InstructionType := IMD_MAC;
    pIntermediate^.MAC := pInstruction^.MAC;

    // Output param
    pIntermediate^.Output.cType := IMD_OUTPUT_A0X;
    pIntermediate^.Output.Address := pInstruction^.Output.OutputAddress;

    // Other parameters
    VshAddParameters(pInstruction, ILU_NOP, pInstruction^.MAC, pIntermediate^.Parameters);

    result:= TRUE;
 end;

 boolean VshAddInstructionILU_R(VSH_SHADER_INSTRUCTION *pInstruction,
                                      VSH_XBOX_SHADER        *pShader,
                                      boolean                IsCombined)
begin 
    VSH_INTERMEDIATE_FORMAT *pIntermediate;
    if( not HasILUR(pInstruction)) then 
    begin 
        result:= FALSE;
     end;

    pIntermediate := VshNewIntermediate(pShader);
    pIntermediate^.IsCombined := IsCombined;

    // Opcode
    pIntermediate^.InstructionType := IMD_ILU;
    pIntermediate^.ILU := pInstruction^.ILU;

    // Output param
    pIntermediate^.Output.cType := IMD_OUTPUT_R;
    // If this is a combined instruction, only r1 is allowed (R address should not be used)
    pIntermediate^.Output.Address := IsCombined ? 1 : pInstruction^.Output.ILURAddress;
    memcpy(pIntermediate^.Output.Mask, pInstruction^.Output.ILURMask, SizeOf(boolean) * 4);

    // Other parameters
    VshAddParameters(pInstruction, pInstruction^.ILU, MAC_NOP, pIntermediate^.Parameters);

    result:= TRUE;
 end;

 boolean VshAddInstructionILU_O(VSH_SHADER_INSTRUCTION *pInstruction,
                                      VSH_XBOX_SHADER        *pShader,
                                      boolean                IsCombined)
begin 
    VSH_INTERMEDIATE_FORMAT *pIntermediate;
    if( not HasILUO(pInstruction)) then 
    begin 
        result:= FALSE;
     end;

    pIntermediate := VshNewIntermediate(pShader);
    pIntermediate^.IsCombined := IsCombined;

    // Opcode
    pIntermediate^.InstructionType := IMD_ILU;
    pIntermediate^.ILU := pInstruction^.ILU;

    // Output param
    pIntermediate^.Output.cType := pInstruction^.Output.OutputType = OUTPUT_C ? IMD_OUTPUT_C : IMD_OUTPUT_O;
    pIntermediate^.Output.Address := pInstruction^.Output.OutputAddress;
    memcpy(pIntermediate^.Output.Mask, pInstruction^.Output.OutputMask, SizeOf(boolean) * 4);

    // Other parameters
    VshAddParameters(pInstruction, pInstruction^.ILU, MAC_NOP, pIntermediate^.Parameters);

    result:= TRUE;
 end;

  procedure VshConvertToIntermediate(var pInstruction: VSH_SHADER_INSTRUCTION;                               VSH_XBOX_SHADER        *pShader)
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
    boolean IsCombined := FALSE;

    if(VshAddInstructionMAC_R(pInstruction, pShader, IsCombined)) then 
    begin 
        if(HasMACO(pInstruction) then   or 
            HasILUR(pInstruction)  or 
            HasILUO(pInstruction))
        begin 
            IsCombined := TRUE;
         end;
     end;
    if(VshAddInstructionMAC_O(pInstruction, pShader, IsCombined)) then 
    begin 
        if(HasILUR(pInstruction) then   or 
            HasILUO(pInstruction))
        begin 
            IsCombined := TRUE;
         end;
     end;
    // Special case, arl (mov a0.x, ...)
    if(VshAddInstructionMAC_ARL(pInstruction, pShader, IsCombined)) then 
    begin 
        if(HasILUR(pInstruction) then   or 
            HasILUO(pInstruction))
        begin 
            IsCombined := TRUE;
         end;
     end;
    if(VshAddInstructionILU_R(pInstruction, pShader, IsCombined)) then 
    begin 
        if(HasILUO(pInstruction)) then 
        begin 
            IsCombined := TRUE;
         end;
     end;
    VshAddInstructionILU_O(pInstruction, pShader, IsCombined);
 end;

    procedure VshSetSwizzle(var pParameter: VSH_IMD_PARAMETER;                          VSH_SWIZZLE       x,
                                 VSH_SWIZZLE       y,
                                 VSH_SWIZZLE       z,
                                 VSH_SWIZZLE       w)
begin 
    pParameter^.Parameter.Swizzle[0] := x;
    pParameter^.Parameter.Swizzle[1] := y;
    pParameter^.Parameter.Swizzle[2] := z;
    pParameter^.Parameter.Swizzle[3] := w;
 end;

    procedure VshSetOutputMask(pOutput: VSH_IMD_OUTPUT*;                             boolean MaskX,
                                    boolean MaskY,
                                    boolean MaskZ,
                                    boolean MaskW)
begin 
    pOutput^.Mask[0] := MaskX;
    pOutput^.Mask[1] := MaskY;
    pOutput^.Mask[2] := MaskZ;
    pOutput^.Mask[3] := MaskW;
 end;
(*
    mul oPos.xyz, r12, c-38
    +rcc r1.x, r12.w

    mad oPos.xyz, r12, r1.x, c-37
*)
  procedure VshRemoveScreenSpaceInstructions(var pShader: VSH_XBOX_SHADER);
    int16 PosC38    := -1;

    for (integer i := 0; i < pShader^.IntermediateCount; i++)
    begin 
        VSH_INTERMEDIATE_FORMAT* pIntermediate := @pShader^.Intermediate[i];

        for (integer k := 0; k < 3; k++)
        begin 
            if(pIntermediate^.Parameters[k].Active) then 
            begin 
                if(pIntermediate^.Parameters[k].Parameter.ParameterType = PARAM_C  and then  
                    not pIntermediate^.Parameters[k].IsA0X)
                begin 
                    if(pIntermediate^.Parameters[k].Parameter.Address = -37) then 
                    begin 
                        // Found c-37, remove the instruction
                        if(k = 2  and then  
                           pIntermediate^.Parameters[1].Active  and 
                           pIntermediate^.Parameters[1].Parameter.ParameterType = PARAM_R)
                        begin 
                            DbgVshPrintf("PosC38 :=  mod d i =  mod d", PosC38, i);
                            for (integer j := (i-1); j >= 0; j--)
                            begin 
                                VSH_INTERMEDIATE_FORMAT* pIntermediate1W := @pShader^.Intermediate[j];
                                // Time to start searching for +rcc r#.x, r12.w
                                if(pIntermediate1W^.InstructionType = IMD_ILU  and then  
                                    pIntermediate1W^.ILU = ILU_RCC  and 
                                    pIntermediate1W^.Output.cType = IMD_OUTPUT_R  and 
                                    pIntermediate1W^.Output.Address = 
                                    pIntermediate^.Parameters[1].Parameter.Address)
                                begin 
                                    DbgVshPrintf("Deleted +rcc r1.x, r12.w");
                                    VshDeleteIntermediate(pShader, j);
                                    i:= i - 1;
                                    //j--;
                                    break;
                                 end;
                             end;
                         end;
                        VshDeleteIntermediate(pShader, i);
                        i:= i - 1;
                        DbgVshPrintf("Deleted mad oPos.xyz, r12, r1.x, c-37");
                        break;
                     end;
                    else if(pIntermediate^.Parameters[k].Parameter.Address = -38) then 
                    begin 
                        VshDeleteIntermediate(pShader, i);
                        PosC38 := i;
                        i:= i - 1;
                        DbgVshPrintf("Deleted mul oPos.xyz, r12, c-38");
                     end;
                 end;
             end;
         end;
     end;
 end;

// Converts the intermediate format vertex shader to DirectX 8 format
 boolean VshConvertShader(VSH_XBOX_SHADER *pShader,
                                boolean         bNoReservedConstants)
begin 
    boolean RUsage[13] := ( FALSE );
    // TODO: What about state shaders and such?
    pShader^.ShaderHeader.Version := VERSION_VS;

    // Search for the screen space instructions, and remove them
    if( not bNoReservedConstants) then 
    begin 
        VshRemoveScreenSpaceInstructions(pShader);
     end;

    // TODO: Add routine for compacting r register usage so that at least one is freed (two if dph and r12)

    for (integer i := 0; i < pShader^.IntermediateCount; i++)
    begin 
        VSH_INTERMEDIATE_FORMAT* pIntermediate := @pShader^.Intermediate[i];
        // Combining not supported in vs.1.1
        pIntermediate^.IsCombined := FALSE;

        (*
        if(pIntermediate^.Output.cType = IMD_OUTPUT_O and pIntermediate^.Output.Address = OREG_OFOG) then 
        begin 
            // The PC shader assembler doesn't like masks on scalar registers
            VshSetOutputMask(@pIntermediate^.Output, TRUE, TRUE, TRUE, TRUE);
         end;*)

        if(pIntermediate^.InstructionType = IMD_ILU and pIntermediate^.ILU = ILU_RCC) then 
        begin 
            // Convert rcc to rcp
            pIntermediate^.ILU := ILU_RCP;
         end;

        if(pIntermediate^.Output.cType = IMD_OUTPUT_R) then 
        begin 
            RUsage[pIntermediate^.Output.Address] := TRUE;
         end;
        // Make constant registers range from 0 to 192 instead of -96 to 96
        if(pIntermediate^.Output.cType = IMD_OUTPUT_C) then 
        begin 
            pIntermediate^.Output.Address:= pIntermediate^.Output.Address + 96;
         end;

        for (integer j := 0; j < 3; j++)
        begin 
            if(pIntermediate^.Parameters[j].Active) then 
            begin 
                if(pIntermediate^.Parameters[j].Parameter.ParameterType = PARAM_R) then 
                begin 
                    RUsage[pIntermediate^.Parameters[j].Parameter.Address] := TRUE;
                 end;
                // Make constant registers range from 0 to 192 instead of -96 to 96
                if(pIntermediate^.Parameters[j].Parameter.ParameterType = PARAM_C) then 
                begin 
                    pIntermediate^.Parameters[j].Parameter.Address:= pIntermediate^.Parameters[j].Parameter.Address + 96;
                 end;
             end;
         end;

        if(pIntermediate^.InstructionType = IMD_MAC and pIntermediate^.MAC = MAC_DPH) then 
        begin 
            // Replace dph with dp3 and add
            if(pIntermediate^.Output.cType <> IMD_OUTPUT_R) then 
            begin 
                // TODO: Complete dph support
                EmuWarning("Can't simulate dph for other than output r registers (yet)");
                result:= FALSE;
             end;
            VSH_INTERMEDIATE_FORMAT TmpIntermediate := *pIntermediate;
            pIntermediate^.MAC := MAC_DP3;
            TmpIntermediate.MAC := MAC_ADD;
            TmpIntermediate.Parameters[0].IsA0X := FALSE;
            TmpIntermediate.Parameters[0].Parameter.ParameterType := PARAM_R;
            TmpIntermediate.Parameters[0].Parameter.Address := TmpIntermediate.Output.Address;
            TmpIntermediate.Parameters[0].Parameter.Neg := FALSE;
            VshSetSwizzle(@TmpIntermediate.Parameters[0], SWIZZLE_W, SWIZZLE_W, SWIZZLE_W, SWIZZLE_W);
            VshSetSwizzle(@TmpIntermediate.Parameters[1], SWIZZLE_W, SWIZZLE_W, SWIZZLE_W, SWIZZLE_W);
            VshSetOutputMask(@TmpIntermediate.Output, FALSE, FALSE, FALSE, TRUE);
            VshInsertIntermediate(pShader, @TmpIntermediate, i + 1);
            i:= i + 1;
         end;
     end;
    int16 R12Replacement := -1;
    if(RUsage[12]) then 
    begin 
        // Sigh, they absolutely had to use r12, didn't they?
        for (integer i := 11; i >= 0; i--)
        begin 
            if( not RUsage[i]) then 
            begin 
                R12Replacement := i;
                break;
             end;
         end;
        if(R12Replacement = -1) then 
        begin 
            EmuWarning("Vertex shader uses all r registers, including r12; impossible to convert not ");
            result:= FALSE;
         end;
        for (integer j := 0; j < pShader^.IntermediateCount; j++)
        begin 
            VSH_INTERMEDIATE_FORMAT* pIntermediate := @pShader^.Intermediate[j];
            if(pIntermediate^.Output.cType = IMD_OUTPUT_O  and then  
                pIntermediate^.Output.Address = OREG_OPOS)
            begin 
                // Found instruction writing to oPos
                pIntermediate^.Output.cType := IMD_OUTPUT_R;
                pIntermediate^.Output.Address := R12Replacement;
             end;

            for (integer k := 0; k < 3; k++)
            begin 
                if(pIntermediate^.Parameters[k].Active) then 
                begin 
                    if(pIntermediate^.Parameters[k].Parameter.ParameterType = PARAM_R  and then  
                        pIntermediate^.Parameters[k].Parameter.Address = 12)
                    begin 
                        // Found a r12 used as a parameter; replace
                        pIntermediate^.Parameters[k].Parameter.Address := R12Replacement;
                     end;
                    else if(pIntermediate^.Parameters[k].Parameter.ParameterType = PARAM_C  and then  
                             pIntermediate^.Parameters[k].Parameter.Address = 58  and 
                              not pIntermediate^.Parameters[k].IsA0X)
                    begin 
                        // Found c-38, replace it with r12.w
                        pIntermediate^.Parameters[k].Parameter.ParameterType := PARAM_R;
                        pIntermediate^.Parameters[k].Parameter.Address := R12Replacement;
                        VshSetSwizzle(@pIntermediate^.Parameters[k], SWIZZLE_W, SWIZZLE_W, SWIZZLE_W, SWIZZLE_W);
                     end;
                 end;
             end;
         end;
        // Insert mov oPos, r## in the end
        VSH_INTERMEDIATE_FORMAT *pOPosWriteBack := VshNewIntermediate(pShader);
        pOPosWriteBack^.InstructionType := IMD_ILU;
        pOPosWriteBack^.ILU := ILU_MOV;
        pOPosWriteBack^.MAC := MAC_NOP;
        pOPosWriteBack^.Output.cType := IMD_OUTPUT_O;
        pOPosWriteBack^.Output.Address := OREG_OPOS;
        VshSetOutputMask(@pOPosWriteBack^.Output, TRUE, TRUE, TRUE, TRUE);
        pOPosWriteBack^.Parameters[0].Active := TRUE;
        pOPosWriteBack^.Parameters[0].Parameter.ParameterType := PARAM_R;
        pOPosWriteBack^.Parameters[0].Parameter.Address := R12Replacement;
        VshSetSwizzle(@pOPosWriteBack^.Parameters[0], SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_W);
     end;
    result:= TRUE;
 end;

// ****************************************************************************
// * Vertex shader declaration recompiler
// ****************************************************************************

type	
VSH_TYPE_PATCH_DATA	= record  
    DWORD NbrTypes;
      Types: array[0..256-1] of UINT;
 end;

type	
VSH_STREAM_PATCH_DATA	= record  
    DWORD                     NbrStreams;
     pStreamPatches: array[0..256-1] of XTL.STREAM_DYNAMIC_PATCH;
 end;

type	
VSH_PATCH_DATA	= record  
    boolean              NeedPatching;
    DWORD                ConvertedStride;
    VSH_TYPE_PATCH_DATA  TypePatchData;
    VSH_STREAM_PATCH_DATA StreamPatchData;
 end;

// VERTEX SHADER
const DEF_VSH_END = $FFFFFFFF;
const DEF_VSH_NOP = $00000000;

 DWORD VshGetDeclarationSize(DWORD *pDeclaration)
begin 
    DWORD Pos := 0;
    while ((pDeclaration + Pos) <> DEF_VSH_END)
    begin 
        Pos:= Pos + 1;
     end;
    result:= (Pos + 1) * SizeOf(DWORD);
 end;

DWORD Xb2PCRegisterType(DWORD VertexRegister)
begin 
    DWORD PCRegisterType;
    case(VertexRegister) of
    begin 
     -1:
        DbgVshPrintf("D3DVSDE_VERTEX (* xbox ext. *)");
        PCRegisterType := -1;
        break;
     0:
        DbgVshPrintf("D3DVSDE_POSITION");
        PCRegisterType := D3DVSDE_POSITION;
        break;
     1:
        DbgVshPrintf("D3DVSDE_BLENDWEIGHT");
        PCRegisterType := D3DVSDE_BLENDWEIGHT;
        break;
     2:
        DbgVshPrintf("D3DVSDE_NORMAL");
        PCRegisterType := D3DVSDE_NORMAL;
        break;
     3:
        DbgVshPrintf("D3DVSDE_DIFFUSE");
        PCRegisterType := D3DVSDE_DIFFUSE;
        break;
     4:
        DbgVshPrintf("D3DVSDE_SPECULAR");
        PCRegisterType := D3DVSDE_SPECULAR;
        break;
     5:
        DbgVshPrintf("D3DVSDE_FOG (* xbox ext. *)");
        PCRegisterType := -1;
        break;
     7:
        DbgVshPrintf("D3DVSDE_BACKDIFFUSE (* xbox ext. *)");
        PCRegisterType := -1;
        break;
     8:
        DbgVshPrintf("D3DVSDE_BACKSPECULAR (* xbox ext. *)");
        PCRegisterType := -1;
        break;
     9:
        DbgVshPrintf("D3DVSDE_TEXCOORD0");
        PCRegisterType := D3DVSDE_TEXCOORD0;
        break;
     10:
        DbgVshPrintf("D3DVSDE_TEXCOORD1");
        PCRegisterType := D3DVSDE_TEXCOORD1;
        break;
     11:
        DbgVshPrintf("D3DVSDE_TEXCOORD2");
        PCRegisterType := D3DVSDE_TEXCOORD2;
        break;
     12:
        DbgVshPrintf("D3DVSDE_TEXCOORD3");
        PCRegisterType := D3DVSDE_TEXCOORD3;
        break;
    default:
        DbgVshPrintf(" mod d (* unknown register *)", VertexRegister);
        PCRegisterType := -1;
        break;
     end;
    result:= PCRegisterType;
 end;

  DWORD VshGetTokenType(DWORD Token)
begin 
    result:= (Token and D3DVSD_TOKENTYPEMASK) shr D3DVSD_TOKENTYPESHIFT;
 end;

  DWORD VshGetVertexRegister(DWORD Token)
begin 
    result:= (Token and D3DVSD_VERTEXREGMASK) shr D3DVSD_VERTEXREGSHIFT;
 end;

  DWORD VshGetVertexRegisterIn(DWORD Token)
begin 
    result:= (Token and D3DVSD_VERTEXREGINMASK) shr D3DVSD_VERTEXREGINSHIFT;
 end;

  DWORD VshGetVertexStream(DWORD Token)
begin 
    result:= (Token and D3DVSD_STREAMNUMBERMASK) shr D3DVSD_STREAMNUMBERSHIFT;
 end;

  procedure VshConvertToken_NOP(var pToken: DWORD);
    // D3DVSD_NOP
    if(pToken <> DEF_VSH_NOP) then 
    begin 
        EmuWarning("Token NOP found, but extra parameters are given not ");
     end;
    DbgVshPrintf("D3DVSD_NOP(),");
 end;

 DWORD VshConvertToken_CONSTMEM(DWORD *pToken)
begin 
    // D3DVSD_CONST
    DbgVshPrintf("D3DVSD_CONST(");

    DWORD ConstantAddress := ((pToken shr D3DVSD_CONSTADDRESSSHIFT) and $FF);
    DWORD Count           := (pToken and D3DVSD_CONSTCOUNTMASK) shr D3DVSD_CONSTCOUNTSHIFT;

    DbgVshPrintf(" mod d,  mod d),", ConstantAddress, Count);

    //pToken = D3DVSD_CONST(ConstantAddress, Count);

    for (uint i := 0; i < Count; i++)
    begin 
        DbgVshPrintf("$ mod 08X,", pToken);
     end;
    result:= Count;
 end;

  procedure VshConverToken_TESSELATOR(var pToken: DWORD;                                boolean IsFixedFunction)
begin 
    using namespace XTL;

    // TODO: Investigate why Xb2PCRegisterType is only used for fixed function vertex shaders
    // D3DVSD_TESSUV
    if(pToken and $10000000) then 
    begin 
        XTL.DWORD VertexRegister    := VshGetVertexRegister(pToken);
        XTL.DWORD NewVertexRegister := VertexRegister;

        DbgVshPrintf("D3DVSD_TESSUV(");

        if(IsFixedFunction) then 
        begin 
            NewVertexRegister := Xb2PCRegisterType(VertexRegister);
         end;
        else
        begin 
            DbgVshPrintf(" mod d", NewVertexRegister);
         end;

        DbgVshPrintf("),");

        *pToken := D3DVSD_TESSUV(NewVertexRegister);
     end;
    // D3DVSD_TESSNORMAL
    else
    begin 
        XTL.DWORD VertexRegisterIn  := VshGetVertexRegisterIn(pToken);
        XTL.DWORD VertexRegisterOut := VshGetVertexRegister(pToken);

        XTL.DWORD NewVertexRegisterIn  := VertexRegisterIn;
        XTL.DWORD NewVertexRegisterOut := VertexRegisterOut;

        DbgVshPrintf("D3DVSD_TESSNORMAL(");

        if(IsFixedFunction) then 
        begin 
            NewVertexRegisterIn := Xb2PCRegisterType(VertexRegisterIn);
         end;
        else
        begin 
            DbgVshPrintf(" mod d", NewVertexRegisterIn);
         end;

        DbgVshPrintf(", ");

        if(IsFixedFunction) then 
        begin 
            NewVertexRegisterOut := Xb2PCRegisterType(VertexRegisterOut);
         end;
        else
        begin 
            DbgVshPrintf(" mod d", NewVertexRegisterOut);
         end;

        DbgVshPrintf("),");
        *pToken := D3DVSD_TESSNORMAL(NewVertexRegisterIn, NewVertexRegisterOut);
     end;
 end;

 boolean VshAddStreamPatch(VSH_PATCH_DATA *pPatchData)
begin 
    integer CurrentStream := pPatchData^.StreamPatchData.NbrStreams - 1;

    if(CurrentStream >= 0) then 
    begin 
        DbgVshPrintf("NeedPatching:  mod d", pPatchData^.NeedPatching);

        XTL.STREAM_DYNAMIC_PATCH* pStreamPatch := @pPatchData^.StreamPatchData.pStreamPatches[CurrentStream];

        pStreamPatch^.ConvertedStride := pPatchData^.ConvertedStride;
        pStreamPatch^.NbrTypes := pPatchData^.TypePatchData.NbrTypes;
        pStreamPatch^.NeedPatch := pPatchData^.NeedPatching;
        pStreamPatch^.pTypes := (UINT )CxbxMalloc(pPatchData^.TypePatchData.NbrTypes * SizeOf(VSH_TYPE_PATCH_DATA));
        memcpy(pStreamPatch^.pTypes, pPatchData^.TypePatchData.Types, pPatchData^.TypePatchData.NbrTypes * SizeOf(VSH_TYPE_PATCH_DATA));

        result:= TRUE;
     end;
    result:= FALSE;
 end;

  procedure VshConvertToken_STREAM(var pToken: DWORD;                             VSH_PATCH_DATA *pPatchData)
begin 
    // D3DVSD_STREAM_TESS
    if(pToken and D3DVSD_STREAMTESSMASK) then 
    begin 
        DbgVshPrintf("D3DVSD_STREAM_TESS(),");
     end;
    // D3DVSD_STREAM
    else
    begin 
        XTL.DWORD StreamNumber := VshGetVertexStream(pToken);
        DbgVshPrintf("D3DVSD_STREAM( mod d),", StreamNumber);

        // new stream
        // copy current data to structure
        if(VshAddStreamPatch(pPatchData)) then 
        begin 
            pPatchData^.ConvertedStride := 0;
            pPatchData^.TypePatchData.NbrTypes := 0;
            pPatchData^.NeedPatching := FALSE;
         end;

        pPatchData^.StreamPatchData.NbrStreams:= pPatchData^.StreamPatchData.NbrStreams + 1;
     end;
 end;

  procedure VshConvertToken_STREAMDATA_SKIP(var pToken: DWORD);
    using namespace XTL;

    XTL.DWORD SkipCount := (pToken and D3DVSD_SKIPCOUNTMASK) shr D3DVSD_SKIPCOUNTSHIFT;
    DbgVshPrintf("D3DVSD_SKIP( mod d),", SkipCount);
 end;

  procedure VshConvertToken_STREAMDATA_SKIPBYTES(var pToken: DWORD);
    using namespace XTL;

    XTL.DWORD SkipBytesCount := (pToken and D3DVSD_SKIPCOUNTMASK) shr D3DVSD_SKIPCOUNTSHIFT;
    DbgVshPrintf("D3DVSD_SKIPBYTES( mod d), (* xbox ext. *)\n", SkipBytesCount);
    if(SkipBytesCount mod SizeOf(XTL.DWORD)) then 
    begin 
        EmuWarning("D3DVSD_SKIPBYTES can't be converted to D3DVSD_SKIP, not divisble by 4.");
     end;
    *pToken := D3DVSD_SKIP(SkipBytesCount / SizeOf(XTL.DWORD));
 end;

  procedure VshConvertToken_STREAMDATA_REG(var pToken: DWORD;                                     boolean         IsFixedFunction,
                                           VSH_PATCH_DATA *pPatchData)
begin 
    using namespace XTL;

    DbgVshPrintf("D3DVSD_REG(");

    XTL.DWORD VertexRegister := VshGetVertexRegister(pToken);
    XTL.DWORD NewVertexRegister;

    if(IsFixedFunction) then 
    begin 
        NewVertexRegister := Xb2PCRegisterType(VertexRegister);
     end;
    else
    begin 
        NewVertexRegister := VertexRegister;
        DbgVshPrintf(" mod d", NewVertexRegister);
     end;

    DbgVshPrintf(", ");

    XTL.DWORD DataType := (pToken shr D3DVSD_DATATYPESHIFT) and $FF;
    XTL.DWORD NewDataType := 0;

    // save patching information
    pPatchData^.TypePatchData.Types[pPatchData^.TypePatchData.NbrTypes] := DataType;
    pPatchData^.TypePatchData.NbrTypes:= pPatchData^.TypePatchData.NbrTypes + 1;

    case(DataType) of
    begin 
     $12:
        DbgVshPrintf("D3DVSDT_FLOAT1");
        NewDataType := D3DVSDT_FLOAT1;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + SizeOf(FLOAT);
        break;
     $22:
        DbgVshPrintf("D3DVSDT_FLOAT2");
        NewDataType := D3DVSDT_FLOAT2; 
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 2*SizeOf(FLOAT);
        break;
     $32:
        DbgVshPrintf("D3DVSDT_FLOAT3");
        NewDataType := D3DVSDT_FLOAT3;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 3*SizeOf(FLOAT);
        break;
     $42:
        DbgVshPrintf("D3DVSDT_FLOAT4");
        NewDataType := D3DVSDT_FLOAT4;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 4*SizeOf(FLOAT);
        break;
     $40:
        DbgVshPrintf("D3DVSDT_D3DCOLOR");
        NewDataType := D3DVSDT_D3DCOLOR; 
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + SizeOf(D3DCOLOR);
        break;
     $25:
        DbgVshPrintf("D3DVSDT_SHORT2");
        NewDataType := D3DVSDT_SHORT2;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 2*SizeOf(XTL.SmallInt);
        break;
     $45:
        DbgVshPrintf("D3DVSDT_SHORT4");
        NewDataType := D3DVSDT_SHORT4;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 4*SizeOf(XTL.SmallInt);
        break;
     $11:
        DbgVshPrintf("D3DVSDT_NORMSHORT1 (* xbox ext. *)");
        NewDataType := D3DVSDT_SHORT2; // hmm, emulation?
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 2*SizeOf(XTL.SmallInt);
        pPatchData^.NeedPatching := TRUE;
        break;
     $21:
        DbgVshPrintf("D3DVSDT_NORMSHORT2 (* xbox ext. *)");
        NewDataType := D3DVSDT_SHORT2;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 2*SizeOf(XTL.SmallInt);
        pPatchData^.NeedPatching := TRUE;
        break;
     $31:
        DbgVshPrintf("D3DVSDT_NORMSHORT3 (* xbox ext. nsp *)");
        NewDataType := D3DVSDT_SHORT4;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 4*SizeOf(XTL.SmallInt);
        pPatchData^.NeedPatching := TRUE;
        break;
     $41:
        DbgVshPrintf("D3DVSDT_NORMSHORT4 (* xbox ext. *)");
        NewDataType := D3DVSDT_SHORT4;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 4*SizeOf(XTL.SmallInt);
        pPatchData^.NeedPatching := TRUE;
        break;
     $16:
        DbgVshPrintf("D3DVSDT_NORMPACKED3 (* xbox ext. nsp *)");
        NewDataType := D3DVSDT_FLOAT3;//0xFF; //32bit
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 3*SizeOf(FLOAT);
        pPatchData^.NeedPatching := TRUE;
        break;
     $15:
        DbgVshPrintf("D3DVSDT_SHORT1 (* xbox ext. nsp *)");
        NewDataType := D3DVSDT_SHORT2;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 2*SizeOf(XTL.SmallInt);
        pPatchData^.NeedPatching := TRUE;
        break;
     $35:
        DbgVshPrintf("D3DVSDT_SHORT3 (* xbox ext. nsp *)");
        NewDataType := D3DVSDT_SHORT4;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 4*SizeOf(XTL.SmallInt);
        pPatchData^.NeedPatching := TRUE;
        break;
     $14:
        DbgVshPrintf("D3DVSDT_PBYTE1 (* xbox ext. nsp *)");
        NewDataType := D3DVSDT_FLOAT1;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 1*SizeOf(FLOAT);
        pPatchData^.NeedPatching := TRUE;
        break;
     $24:
        DbgVshPrintf("D3DVSDT_PBYTE2 (* xbox ext. nsp *)");
        NewDataType := D3DVSDT_FLOAT2;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 2*SizeOf(FLOAT);
        pPatchData^.NeedPatching := TRUE;
        break;
     $34:
        DbgVshPrintf("D3DVSDT_PBYTE3 (* xbox ext. nsp *)");
        NewDataType := D3DVSDT_FLOAT3;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 3*SizeOf(FLOAT);
        pPatchData^.NeedPatching := TRUE;
        break;
     $44:
        DbgVshPrintf("D3DVSDT_PBYTE4 (* xbox ext. *)");
        NewDataType := D3DVSDT_FLOAT4;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 4*SizeOf(FLOAT);
        break;
     $72:
        DbgVshPrintf("D3DVSDT_FLOAT2H (* xbox ext. *)");
        NewDataType := D3DVSDT_FLOAT3;
        pPatchData^.ConvertedStride:= pPatchData^.ConvertedStride + 3*SizeOf(FLOAT);
        pPatchData^.NeedPatching := TRUE;
        break;
     $02:
        DbgVshPrintf("D3DVSDT_NONE (* xbox ext. nsp *)");
        NewDataType := $FF;
        break;
    default:
        DbgVshPrintf("Unknown data ctype for D3DVSD_REG: $ mod 02X", DataType);
        break;
     end;
    *pToken := D3DVSD_REG(NewVertexRegister, NewDataType);

    DbgVshPrintf("),");

    if(NewDataType = $FF) then 
    begin 
        EmuWarning("(* WARNING: Fatal type mismatch, no fitting type! *)\n");
     end;
 end;

  procedure VshConvertToken_STREAMDATA(var pToken: DWORD;                                 boolean         IsFixedFunction,
                                       VSH_PATCH_DATA *pPatchData)
begin 
    using namespace XTL;

    // D3DVSD_SKIP
    if(pToken and $10000000) then 
    begin 
        VshConvertToken_STREAMDATA_SKIP(pToken);
     end;
    // D3DVSD_SKIPBYTES
    else if(pToken and $18000000) then 
    begin 
        VshConvertToken_STREAMDATA_SKIPBYTES(pToken);
     end;
    // D3DVSD_REG
    else
    begin 
        VshConvertToken_STREAMDATA_REG(pToken, IsFixedFunction, pPatchData);
     end;
 end;

 DWORD VshRecompileToken(DWORD          *pToken,
                               boolean         IsFixedFunction,
                               VSH_PATCH_DATA *pPatchData)
begin 
    using namespace XTL;

    XTL.DWORD Step := 1;

    case(VshGetTokenType(pToken)) of
    begin 
     D3DVSD_TOKEN_NOP:
        VshConvertToken_NOP(pToken);
        break;
     D3DVSD_TOKEN_STREAM:
    begin 
        VshConvertToken_STREAM(pToken, pPatchData);
        break;
     end;
     D3DVSD_TOKEN_STREAMDATA:
    begin 
        VshConvertToken_STREAMDATA(pToken, IsFixedFunction, pPatchData);
        break;
     end;
     D3DVSD_TOKEN_TESSELLATOR:
    begin 
        VshConverToken_TESSELATOR(pToken, IsFixedFunction);
        break;
     end;
     D3DVSD_TOKEN_CONSTMEM:
    begin 
        Step := VshConvertToken_CONSTMEM(pToken);
        break;
     end;
    default:
        DbgVshPrintf("Unknown token ctype:  mod d", VshGetTokenType(pToken));
        break;
     end;

    result:= Step;
 end;

DWORD XTL.EmuRecompileVshDeclaration
(
    DWORD                *pDeclaration, 
    DWORD               **ppRecompiledDeclaration,
    DWORD                *pDeclarationSize,
    boolean               IsFixedFunction,
    VERTEX_DYNAMIC_PATCH *pVertexDynamicPatch
)
begin 
    // First of all some info:
    // We have to figure out which flags are set and then
    // we have to patch their params

    // some token values
    // 0xFFFFFFFF - end of the declaration
    // 0x00000000 - nop (means that this value is ignored)

    // Calculate size of declaration
    DWORD DeclarationSize := VshGetDeclarationSize(pDeclaration);
    *ppRecompiledDeclaration := (DWORD )CxbxMalloc(DeclarationSize);
    DWORD *pRecompiled := *ppRecompiledDeclaration;
    memcpy(pRecompiled, pDeclaration, DeclarationSize);
    *pDeclarationSize := DeclarationSize;

    // TODO: Put these in one struct
    VSH_PATCH_DATA       PatchData := ( 0 );

    DbgVshPrintf("DWORD dwVSHDecl[] :=begin ");

    while (pRecompiled <> DEF_VSH_END)
    begin 
        DWORD Step := VshRecompileToken(pRecompiled, IsFixedFunction, @PatchData);
        pRecompiled:= pRecompiled + Step;
     end;
    DbgVshPrintf("D3DVSD_END());");

    VshAddStreamPatch(@PatchData);

    DbgVshPrintf("NbrStreams:  mod d", PatchData.StreamPatchData.NbrStreams);

    // Copy the patches to the vertex shader struct
    DWORD StreamsSize := PatchData.StreamPatchData.NbrStreams * SizeOf(STREAM_DYNAMIC_PATCH);
    pVertexDynamicPatch^.NbrStreams := PatchData.StreamPatchData.NbrStreams;
    pVertexDynamicPatch^.pStreamPatches := (STREAM_DYNAMIC_PATCH )CxbxMalloc(StreamsSize);
    memcpy(pVertexDynamicPatch^.pStreamPatches,
           PatchData.StreamPatchData.pStreamPatches,
           StreamsSize);

    result:= D3D_OK;
 end;

// recompile xbox vertex shader function
 HRESULT XTL.EmuRecompileVshFunction
(
    DWORD        *pFunction,
    LPD3DXBUFFER *ppRecompiled,
    DWORD        *pOriginalSize,
    boolean      bNoReservedConstants
)
begin 
    VSH_SHADER_HEADER   *pShaderHeader := (VSH_SHADER_HEADER)pFunction;
    DWORD               *pToken;
    boolean             EOI := false;
    VSH_XBOX_SHADER     *pShader := (VSH_XBOX_SHADER)CxbxMalloc(SizeOf(VSH_XBOX_SHADER));
    HRESULT             hRet := 0;

    // TODO: support this situation..
    if(pFunction = 0) then 
        result:= E_FAIL;

    *ppRecompiled := 0;
    *pOriginalSize := 0;
    if( not pShader) then 
    begin 
        EmuWarning("Couldn't allocate memory for vertex shader conversion buffer");
        hRet := E_OUTOFMEMORY;
     end;
    FillChar(pShader, 0, SizeOf(VSH_XBOX_SHADER));
    pShader^.ShaderHeader := *pShaderHeader;
    case(pShaderHeader^.Version) of
    begin 
         VERSION_XVS:
            break;
         VERSION_XVSS:
            EmuWarning("Might not support vertex state shaders?");
            hRet := E_FAIL;
            break;
         VERSION_XVSW:
            EmuWarning("Might not support vertex read/write shaders?");
            hRet := E_FAIL;
            break;
        default:
            EmuWarning("Unknown vertex shader version $ mod 02X", pShaderHeader^.Version);
            hRet := E_FAIL;
            break;
     end;

    if(SUCCEEDED(hRet)) then 
    begin 

        for (pToken = (DWORD)((uint08)pFunction + SizeOf(VSH_SHADER_HEADER));  not EOI; pToken:= pToken + VSH_INSTRUCTION_SIZE)
        begin 
            VSH_SHADER_INSTRUCTION Inst;

            VshParseInstruction(pToken, @Inst);
            VshConvertToIntermediate(@Inst, pShader);
            EOI := (boolean)VshGetField(pToken, FLD_FINAL);
         end;

        // The size of the shader is
        *pOriginalSize := (DWORD)pToken - (DWORD)pFunction;

        PChar  pShaderDisassembly := (Char)CxbxMalloc(pShader^.IntermediateCount * 50); // Should be plenty
        DbgVshPrintf("-- Before conversion --");
        VshWriteShader(pShader, pShaderDisassembly, FALSE);
        DbgVshPrintf(" mod s", pShaderDisassembly);
        DbgVshPrintf("-----------------------");

        VshConvertShader(pShader, bNoReservedConstants);
        VshWriteShader(pShader, pShaderDisassembly, TRUE);

        DbgVshPrintf("-- After conversion ---");
        DbgVshPrintf(" mod s", pShaderDisassembly);
        DbgVshPrintf("-----------------------");

        hRet = D3DXAssembleShader(pShaderDisassembly,
                                  strlen(pShaderDisassembly),
                                  D3DXASM_SKIPVALIDATION,
                                  0,
                                  ppRecompiled,
                                  0);

        if (FAILED(hRet)) then 
        begin 
            EmuWarning("Couldn't assemble recompiled vertex shader");
         end;

        CxbxFree(pShaderDisassembly);
     end;
    CxbxFree(pShader);

    result:= hRet;
 end;

  procedure XTL.FreeVertexDynamicPatch(var pVertexShader: VERTEX_SHADER);
    for (DWORD i := 0; i < pVertexShader^.VertexDynamicPatch.NbrStreams; i++)
    begin 
        CxbxFree(pVertexShader^.VertexDynamicPatch.pStreamPatches[i].pTypes);
     end;
    CxbxFree(pVertexShader^.VertexDynamicPatch.pStreamPatches);
    pVertexShader^.VertexDynamicPatch.pStreamPatches := 0;
    pVertexShader^.VertexDynamicPatch.NbrStreams := 0;
 end;

 boolean XTL.IsValidCurrentShader
begin 
    DWORD Handle;

    EmuSwapFS();
    EmuIDirect3DDevice8_GetVertexShader(@Handle);
    EmuSwapFS();
    if (VshHandleIsVertexShader(Handle)) then 
    begin 
        X_D3DVertexShader *pD3DVertexShader := (X_D3DVertexShader )(Handle and $7FFFFFFF);
        VERTEX_SHADER *pVertexShader := (VERTEX_SHADER )pD3DVertexShader^.Handle;
        if (pVertexShader^.Status <> 0) then 
        begin 
            result:= FALSE;
         end;
        (*
        for (uint32 i := 0; i < pVertexShader^.VertexDynamicPatch.NbrStreams; i++)
        begin 
            if (pVertexShader^.VertexDynamicPatch.pStreamPatches[i].NeedPatch) then 
            begin 
                // Just for caching purposes
                pVertexShader^.Status := $80000001;
                result:= FALSE;
             end;
         end;
        *)
     end;
    result:= TRUE;
 end;

 XTL.VERTEX_DYNAMIC_PATCH *XTL.VshGetVertexDynamicPatch(DWORD Handle)
begin 
    X_D3DVertexShader *pD3DVertexShader := VshHandleGetVertexShader(Handle);
    VERTEX_SHADER *pVertexShader := (VERTEX_SHADER )pD3DVertexShader^.Handle;

    for (uint32 i := 0; i < pVertexShader^.VertexDynamicPatch.NbrStreams; i++)
    begin 
        if (pVertexShader->VertexDynamicPatch.pStreamPatches[i].NeedPatch) then 
        begin 
            result:= @pVertexShader->VertexDynamicPatch;
         end;
     end;
    result:= 0;
 end;


end.
