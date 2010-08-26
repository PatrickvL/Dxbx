(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2007 Shadow_tj and other members of the development team.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY, without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

unit uPixelShader;

interface

{$INCLUDE Dxbx.inc}

uses
  // Delphi
  Windows,
  SysUtils, // Format
  // Dxbx
  uConsts, // TITLEID_AZURIK
  uTypes,
  uDxbxUtils, // iif
  uDxbxKrnlUtils, // IsRunning
  uLog,
  uEmuD3D8Types,
  uEmuD3D8Utils,
  uEmu;

// From PixelShader.h :

(*---------------------------------------------------------------------------*)
(*  Texture configuration - The following members of the D3DPixelShaderDef   *)
(*  structure define the addressing modes of each of the four texture stages:*)
(*      PSTextureModes                                                       *)
(*      PSDotMapping                                                         *)
(*      PSInputTexture                                                       *)
(*      PSCompareMode                                                        *)
(*---------------------------------------------------------------------------*)

// =========================================================================================================
// PSTextureModes
// --------.--------.--------.---xxxxx stage0
// --------.--------.------xx.xxx----- stage1
// --------.--------.-xxxxx--.-------- stage2
// --------.----xxxx.x-------.-------- stage3

// PS_TEXTUREMODES(t0, t1, t2, t3) = ((t3 shl 15) or (t2 shl 10) or (t1 shl 5) or t0),


(*
Texture modes:
NONE           :stage inactive
PROJECT2D      :argb = texture(s/q, t/q)
PROJECT3D      :argb = texture(s/q, t/q, r/q)
CUBEMAP        :argb = cubemap(s,t,r)
PASSTHRU       :argb = s,t,r,q
CLIPPLANE      :pixel not drawn if s,t,r, or q < 0.  PSCompareMode affects comparison
BUMPENVMAP     :argb=texture(s+mat00*src.r+mat01*src.g,
                             t+mat10*src.r+mat11*src.g)
                mat00 set via D3DTSS_BUMPENVMAT00, etc.
BUMPENVMAP_LUM :argb=texture(s+mat00*src.r+mat01*src.g,
                             t+mat10*src.r+mat11*src.g),
                rgb *= (lum_scale*src.b + lum_bias), (a is not affected)
                lum_scale set by D3DTSS_BUMPENVLSCALE
                lum_bias set by D3DTSS_BUMPENVLOFFSET
                mat00 set via D3DTSS_BUMPENVMAT00, etc.
BRDF           :argb = texture(eyeSigma, lightSigma, dPhi)
                       eyeSigma = Sigma of eye vector in spherical coordinates
                       lightSigma = Sigma of light vector in spherical coordinates
                       dPhi = Phi of eye - Phi of light
DOT_ST         :argb = texture(<DotResult of stage-1>, (s,t,r).(src.r,src.g,src.b))
DOT_ZW         :frag depth = (<DotResult of stage-1>/((s,t,r).(src.r,src.g,src.b))
DOT_RFLCT_DIFF :n = (<DotResult of stage-1>,(s,t,r).(src.r,src.g,src.b),<DotResult of stage+1>)
                argb = cubemap(n)
DOT_RFLCT_SPEC :n = (<DotResult of stage-2>,<DotResult of stage-1>,(s,t,r).(src.r,src.g,src.b))
                r = 2*n*(n.e)/(n.n) - e where e is eye vector built from q coord of each stage
                argb = cubemap(r)
DOT_STR_3D     :argb=texture((<DotResult of stage-2>,<DotResult of stage-1>,(s,t,r).(src.r,src.g,src.b)))
DOT_STR_CUBE   :argb=cubemap((<DotResult of stage-2>,<DotResult of stage-1>,(s,t,r).(src.r,src.g,src.b)))
DEPENDENT_AR   :argb = texture(src.a, src.r)
DEPENDENT_GB   :argb = texture(src.g, src.b)
DOTPRODUCT     :argb = (s,t,r).(src.r,src.g,src.b)
DOT_RFLCT_SPEC_CONST :n = (<DotResult of stage-2>,<DotResult of stage-1>,(s,t,r).(src.r,src.g,src.b))
                r = 2*n*(n.e)/(n.n) - e where e is eye vector set via SetEyeVector()
                argb = cubemap(r)
*)

type PS_TEXTUREMODES =
(                               // valid in stage 0 1 2 3
    PS_TEXTUREMODES_NONE=                 $00, // * * * *
    PS_TEXTUREMODES_PROJECT2D=            $01, // * * * *
    PS_TEXTUREMODES_PROJECT3D=            $02, // * * * *
    PS_TEXTUREMODES_CUBEMAP=              $03, // * * * *
    PS_TEXTUREMODES_PASSTHRU=             $04, // * * * *
    PS_TEXTUREMODES_CLIPPLANE=            $05, // * * * *
    PS_TEXTUREMODES_BUMPENVMAP=           $06, // - * * *
    PS_TEXTUREMODES_BUMPENVMAP_LUM=       $07, // - * * *
    PS_TEXTUREMODES_BRDF=                 $08, // - - * *
    PS_TEXTUREMODES_DOT_ST=               $09, // - - * *
    PS_TEXTUREMODES_DOT_ZW=               $0a, // - - * *
    PS_TEXTUREMODES_DOT_RFLCT_DIFF=       $0b, // - - * -
    PS_TEXTUREMODES_DOT_RFLCT_SPEC=       $0c, // - - - *
    PS_TEXTUREMODES_DOT_STR_3D=           $0d, // - - - *
    PS_TEXTUREMODES_DOT_STR_CUBE=         $0e, // - - - *
    PS_TEXTUREMODES_DPNDNT_AR=            $0f, // - * * *
    PS_TEXTUREMODES_DPNDNT_GB=            $10, // - * * *
    PS_TEXTUREMODES_DOTPRODUCT=           $11, // - * * -
    PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST= $12  // - - - *
    // $13-$1f reserved
);


// =========================================================================================================
// PSDotMapping
// --------.--------.--------.-----xxx // stage1
// --------.--------.--------.-xxx---- // stage2
// --------.--------.-----xxx.-------- // stage3

// PS_DOTMAPPING(t0, t1, t2, t3) = ((t3 shl 8) or (t2 shl 4) or t1),


// Mappings:
// ZERO_TO_ONE         :rgb->(r,g,b): 0x0=>0.0, 0xff=>1.0
// MINUS1_TO_1_D3D     :rgb->(r,g,b): 0x0=>-128/127, 0x01=>-1.0, 0x80=>0.0, 0xff=>1.0
// MINUS1_TO_1_GL      :rgb->(r,g,b): 0x80=>-1.0, 0x0=>0.0, 0x7f=>1.0
// MINUS1_TO_1         :rgb->(r,g,b): 0x80=>-128/127, 0x81=>-1.0, 0x0=>0.0, 0x7f=>1.0
// HILO_1              :HL->(H,L,1.0): 0x0000=>0.0, 0xffff=>1.0
// HILO_HEMISPHERE     :HL->(H,L,sqrt(1-H*H-L*L)): 0x8001=>-1.0, 0x0=>0.0, 0x7fff=>1.0, 0x8000=>-32768/32767

type PS_DOTMAPPING =
(                            // valid in stage 0 1 2 3
    PS_DOTMAPPING_ZERO_TO_ONE=         $00, // - * * *
    PS_DOTMAPPING_MINUS1_TO_1_D3D=     $01, // - * * *
    PS_DOTMAPPING_MINUS1_TO_1_GL=      $02, // - * * *
    PS_DOTMAPPING_MINUS1_TO_1=         $03, // - * * *
    PS_DOTMAPPING_HILO_1=              $04, // - * * *
    PS_DOTMAPPING_HILO_HEMISPHERE=     $07  // - * * *
);

// =========================================================================================================
// PSCompareMode
// --------.--------.--------.----xxxx // stage0
// --------.--------.--------.xxxx---- // stage1
// --------.--------.----xxxx.-------- // stage2
// --------.--------.xxxx----.-------- // stage3

// PS_COMPAREMODE(t0, t1, t2, t3) = ((t3 shl 12) or (t2 shl 8) or (t1 shl 4) or t0),

type PS_COMPAREMODE =
(
    PS_COMPAREMODE_S_LT= $00,
    PS_COMPAREMODE_S_GE= $01,

    PS_COMPAREMODE_T_LT= $00,
    PS_COMPAREMODE_T_GE= $02,

    PS_COMPAREMODE_R_LT= $00,
    PS_COMPAREMODE_R_GE= $04,

    PS_COMPAREMODE_Q_LT= $00,
    PS_COMPAREMODE_Q_GE= $08
);


// =========================================================================================================
// PSInputTexture
// --------.-------x.--------.-------- // stage2
// --------.--xx----.--------.-------- // stage3
//
// Selects the other texture to use as an input in the following texture modes:
// DOT_ST, DOT_STR_3D, DOT_STR_CUBE, DOT_ZW, DOT_RFLCT_SPEC,
// DOT_RFLCT_DIFF, DPNDNT_AR, DPNDNT_GB, BUMPENVMAP,
// BUMPENVMAP_LUM, DOT_PRODUCT

// PS_INPUTTEXTURE(t0, t1, t2, t3) = ((t3 shl 20) or (t2 shl 16)),


(*---------------------------------------------------------------------------------*)
(*  Color combiners - The following members of the D3DPixelShaderDef structure     *)
(*  define the state for the eight stages of color combiners:                      *)
(*      PSCombinerCount - Number of stages                                         *)
(*      PSAlphaInputs[8] - Inputs for alpha portion of each stage                  *)
(*      PSRGBInputs[8] - Inputs for RGB portion of each stage                      *)
(*      PSConstant0[8] - Constant 0 for each stage                                 *)
(*      PSConstant1[8] - Constant 1 for each stage                                 *)
(*      PSFinalCombinerConstant0 - Constant 0 for final combiner                   *)
(*      PSFinalCombinerConstant1 - Constant 1 for final combiner                   *)
(*      PSAlphaOutputs[8] - Outputs for alpha portion of each stage                *)
(*      PSRGBOutputs[8] - Outputs for RGB portion of each stage                    *)
(*---------------------------------------------------------------------------------*)


// =========================================================================================================
// PSCombinerCount
// --------.--------.--------.----xxxx // number of combiners (1-8)
// --------.--------.-------x.-------- // mux bit (0= LSB, 1= MSB)
// --------.--------.---x----.-------- // separate C0
// --------.-------x.--------.-------- // separate C1

// PS_COMBINERCOUNT(count, flags) = ((flags shl 8) or count),
// count is 1-8, flags contains one or more values from PS_COMBINERCOUNTFLAGS

const // PS_COMBINERCOUNTFLAGS =
    PS_COMBINERCOUNT_MUX_LSB=     $0000; // mux on r0.a lsb
    PS_COMBINERCOUNT_MUX_MSB=     $0001; // mux on r0.a msb

    PS_COMBINERCOUNT_SAME_C0=     $0000; // c0 same in each stage
    PS_COMBINERCOUNT_UNIQUE_C0=   $0010; // c0 unique in each stage

    PS_COMBINERCOUNT_SAME_C1=     $0000; // c1 same in each stage
    PS_COMBINERCOUNT_UNIQUE_C1=   $0100; // c1 unique in each stage


// =========================================================================================================
// PSRGBInputs[0-7]
// PSAlphaInputs[0-7]
// PSFinalCombinerInputsABCD
// PSFinalCombinerInputsEFG
// --------.--------.--------.----xxxx // D register
// --------.--------.--------.---x---- // D channel (0= RGB/BLUE, 1= ALPHA)
// --------.--------.--------.xxx----- // D input mapping
// --------.--------.----xxxx.-------- // C register
// --------.--------.---x----.-------- // C channel (0= RGB/BLUE, 1= ALPHA)
// --------.--------.xxx-----.-------- // C input mapping
// --------.----xxxx.--------.-------- // B register
// --------.---x----.--------.-------- // B channel (0= RGB/BLUE, 1= ALPHA)
// --------.xxx-----.--------.-------- // B input mapping
// ----xxxx.--------.--------.-------- // A register
// ---x----.--------.--------.-------- // A channel (0= RGB/BLUE, 1= ALPHA)
// xxx-----.--------.--------.-------- // A input mapping

// examples:
//
// shader.PSRGBInputs[3]= PS_COMBINERINPUTS(
//     PS_REGISTER_T0 | PS_INPUTMAPPING_EXPAND_NORMAL     | PS_CHANNEL_RGB,
//     PS_REGISTER_C0 | PS_INPUTMAPPING_UNSIGNED_IDENTITY | PS_CHANNEL_ALPHA,
//     PS_REGISTER_ZERO,
//     PS_REGISTER_ZERO),
//
// shader.PSFinalCombinerInputsABCD= PS_COMBINERINPUTS(
//     PS_REGISTER_T0     | PS_INPUTMAPPING_UNSIGNED_IDENTITY | PS_CHANNEL_ALPHA,
//     PS_REGISTER_ZERO   | PS_INPUTMAPPING_EXPAND_NORMAL     | PS_CHANNEL_RGB,
//     PS_REGISTER_EFPROD | PS_INPUTMAPPING_UNSIGNED_INVERT   | PS_CHANNEL_RGB,
//     PS_REGISTER_ZERO),
//
// PS_FINALCOMBINERSETTING is set in 4th field of PSFinalCombinerInputsEFG with PS_COMBINERINPUTS
// example:
//
// shader.PSFinalCombinerInputsEFG= PS_COMBINERINPUTS(
//     PS_REGISTER_R0 | PS_INPUTMAPPING_UNSIGNED_IDENTITY | PS_CHANNEL_RGB,
//     PS_REGISTER_R1 | PS_INPUTMAPPING_UNSIGNED_IDENTITY | PS_CHANNEL_RGB,
//     PS_REGISTER_R1 | PS_INPUTMAPPING_UNSIGNED_IDENTITY | PS_CHANNEL_BLUE,
//    PS_FINALCOMBINERSETTING_CLAMP_SUM | PS_FINALCOMBINERSETTING_COMPLEMENT_R0),

// PS_COMBINERINPUTS(a,b,c,d) = ((a shl 24) or (b shl 16) or (c shl 8) or d),

// For PSFinalCombinerInputsEFG,
//     a,b,c contain a value from PS_REGISTER, PS_CHANNEL, and PS_INPUTMAPPING for input E,F, and G
//     d contains values from PS_FINALCOMBINERSETTING
// For all other inputs,
//     a,b,c,d each contain a value from PS_REGISTER, PS_CHANNEL, and PS_INPUTMAPPING

type PS_INPUTMAPPING =
(
    PS_INPUTMAPPING_UNSIGNED_IDENTITY= $00, // max(0,x)         OK for final combiner: y = abs(x)
    PS_INPUTMAPPING_UNSIGNED_INVERT=   $20, // 1 - max(0,x)     OK for final combiner: y = 1 - x
    PS_INPUTMAPPING_EXPAND_NORMAL=     $40, // 2*max(0,x) - 1   invalid for final combiner
    PS_INPUTMAPPING_EXPAND_NEGATE=     $60, // 1 - 2*max(0,x)   invalid for final combiner
    PS_INPUTMAPPING_HALFBIAS_NORMAL=   $80, // max(0,x) - 1/2   invalid for final combiner
    PS_INPUTMAPPING_HALFBIAS_NEGATE=   $a0, // 1/2 - max(0,x)   invalid for final combiner
    PS_INPUTMAPPING_SIGNED_IDENTITY=   $c0, // x                invalid for final combiner
    PS_INPUTMAPPING_SIGNED_NEGATE=     $e0  // -x               invalid for final combiner
);

type PS_REGISTER =
(
    PS_REGISTER_ZERO=              $00, // r
    PS_REGISTER_DISCARD=           $00, // w
    PS_REGISTER_C0=                $01, // r
    PS_REGISTER_C1=                $02, // r
    PS_REGISTER_FOG=               $03, // r
    PS_REGISTER_V0=                $04, // r/w
    PS_REGISTER_V1=                $05, // r/w
    PS_REGISTER_T0=                $08, // r/w
    PS_REGISTER_T1=                $09, // r/w
    PS_REGISTER_T2=                $0a, // r/w
    PS_REGISTER_T3=                $0b, // r/w
    PS_REGISTER_R0=                $0c, // r/w
    PS_REGISTER_R1=                $0d, // r/w
    PS_REGISTER_V1R0_SUM=          $0e, // r    = r0 + v1
    PS_REGISTER_EF_PROD=           $0f, // r    = E * F

    PS_REGISTER_ONE=               Ord({PS_REGISTER_ZERO or} PS_INPUTMAPPING_UNSIGNED_INVERT), // OK for final combiner
    PS_REGISTER_NEGATIVE_ONE=      Ord({PS_REGISTER_ZERO or} PS_INPUTMAPPING_EXPAND_NORMAL),   // invalid for final combiner
    PS_REGISTER_ONE_HALF=          Ord({PS_REGISTER_ZERO or} PS_INPUTMAPPING_HALFBIAS_NEGATE), // invalid for final combiner
    PS_REGISTER_NEGATIVE_ONE_HALF= Ord({PS_REGISTER_ZERO or} PS_INPUTMAPPING_HALFBIAS_NORMAL)  // invalid for final combiner
);

// FOG ALPHA is only available in final combiner
// V1R0_SUM and EF_PROD are only available in final combiner (A,B,C,D inputs only)
// V1R0_SUM_ALPHA and EF_PROD_ALPHA are not available
// R0_ALPHA is initialized to T0_ALPHA in stage0

type PS_CHANNEL =
(
    PS_CHANNEL_RGB=   $00, // used as RGB source
    PS_CHANNEL_BLUE=  $00, // used as ALPHA source
    PS_CHANNEL_ALPHA= $10  // used as RGB or ALPHA source
);

const
  PS_ChannelMask = DWORD(Ord(PS_CHANNEL_ALPHA));
  PS_NoChannelMask = DWORD(not PS_ChannelMask);
  PS_AlphaChannelsMask = DWORD(PS_ChannelMask or (PS_ChannelMask shl 8) or (PS_ChannelMask shl 16) or (PS_ChannelMask shl 24));
  PS_NoChannelsMask = DWORD(not PS_AlphaChannelsMask);

type PS_FINALCOMBINERSETTING =
(
    PS_FINALCOMBINERSETTING_CLAMP_SUM=     $80, // V1+R0 sum clamped to [0,1]
    PS_FINALCOMBINERSETTING_COMPLEMENT_V1= $40, // unsigned invert mapping  (1 - v1) is used as an input to the sum rather than v1
    PS_FINALCOMBINERSETTING_COMPLEMENT_R0= $20  // unsigned invert mapping  (1 - r0) is used as an input to the sum rather than r0
);

// =========================================================================================================
// PSRGBOutputs[0-7]
// PSAlphaOutputs[0-7]
// --------.--------.--------.----xxxx // CD register
// --------.--------.--------.xxxx---- // AB register
// --------.--------.----xxxx.-------- // SUM register
// --------.--------.---x----.-------- // CD output (0= multiply, 1= dot product)
// --------.--------.--x-----.-------- // AB output (0= multiply, 1= dot product)
// --------.--------.-x------.-------- // AB_CD mux/sum select (0= sum, 1= mux)
// --------.------xx.x-------.-------- // Output mapping
// --------.-----x--.--------.-------- // CD blue to alpha
// --------.----x---.--------.-------- // AB blue to alpha

// PS_COMBINEROUTPUTS(ab,cd,mux_sum,flags) = ((flags shl 12) or (mux_sum shl 8) or (ab shl 4) or cd),

// ab,cd,mux_sum contain a value from PS_REGISTER
// flags contains values from PS_COMBINEROUTPUT

type PS_COMBINEROUTPUT = DWORD;
const
    PS_COMBINEROUTPUT_IDENTITY=            $00; // y = x
    PS_COMBINEROUTPUT_BIAS=                $08; // y = x - 0.5
    PS_COMBINEROUTPUT_SHIFTLEFT_1=         $10; // y = x*2
    PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS=    $18; // y = (x - 0.5)*2
    PS_COMBINEROUTPUT_SHIFTLEFT_2=         $20; // y = x*4
    PS_COMBINEROUTPUT_SHIFTRIGHT_1=        $30; // y = x/2

    PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA=    $80; // RGB only

    PS_COMBINEROUTPUT_CD_BLUE_TO_ALPHA=    $40; // RGB only

    PS_COMBINEROUTPUT_AB_MULTIPLY=         $00;
    PS_COMBINEROUTPUT_AB_DOT_PRODUCT=      $02; // RGB only

    PS_COMBINEROUTPUT_CD_MULTIPLY=         $00;
    PS_COMBINEROUTPUT_CD_DOT_PRODUCT=      $01; // RGB only

    PS_COMBINEROUTPUT_AB_CD_SUM=           $00; // 3rd output is AB+CD
    PS_COMBINEROUTPUT_AB_CD_MUX=           $04; // 3rd output is MUX(AB,CD) based on R0.a

// AB_CD register output must be DISCARD if either AB_DOT_PRODUCT or CD_DOT_PRODUCT are set

// =========================================================================================================
// PSC0Mapping
// PSC1Mapping
// --------.--------.--------.----xxxx // offset of D3D constant for stage 0
// --------.--------.--------.xxxx---- // offset of D3D constant for stage 1
// --------.--------.----xxxx.-------- // offset of D3D constant for stage 2
// --------.--------.xxxx----.-------- // offset of D3D constant for stage 3
// --------.----xxxx.--------.-------- // offset of D3D constant for stage 4
// --------.xxxx----.--------.-------- // offset of D3D constant for stage 5
// ----xxxx.--------.--------.-------- // offset of D3D constant for stage 6
// xxxx----.--------.--------.-------- // offset of D3D constant for stage 7

//function PS_CONSTANTMAPPING(s0,s1,s2,s3,s4,s5,s6,s7:): ,
//begin
//  Result := ((DWORD(s0) and $f) shl  0) or ((DWORD(s1) and $f) shl 4) or
//            ((DWORD(s2) and $f) shl  8) or ((DWORD(s3) and $f) shl 12) or
//            ((DWORD(s4) and $f) shl 16) or ((DWORD(s5) and $f) shl 20) or
//            ((DWORD(s6) and $f) shl 24) or ((DWORD(s7) and $f) shl 28),
//end,
// s0-s7 contain the offset of the D3D constant that corresponds to the
// c0 or c1 constant in stages 0 through 7.  These mappings are only used in
// SetPixelShaderConstant().

// =========================================================================================================
// PSFinalCombinerConstants
// --------.--------.--------.----xxxx // offset of D3D constant for C0
// --------.--------.--------.xxxx---- // offset of D3D constant for C1
// --------.--------.-------x.-------- // Adjust texture flag

// PS_FINALCOMBINERCONSTANTS(c0,c1,flags) = ((DWORD(flags) shl 8) or (DWORD(c0) and $f) shl 0) or ((DWORD(c1) and $f) shl 4),

// c0 and c1 contain the offset of the D3D constant that corresponds to the
// constants in the final combiner.  These mappings are only used in
// SetPixelShaderConstant().  Flags contains values from PS_GLOBALFLAGS

type PS_GLOBALFLAGS =
(
    // if this flag is set, the texture mode for each texture stage is adjusted as follows:
    //     if set texture is a cubemap,
    //         change PS_TEXTUREMODES_PROJECT2D to PS_TEXTUREMODES_CUBEMAP
    //         change PS_TEXTUREMODES_PROJECT3D to PS_TEXTUREMODES_CUBEMAP
    //         change PS_TEXTUREMODES_DOT_STR_3D to PS_TEXTUREMODES_DOT_STR_CUBE
    //     if set texture is a volume texture,
    //         change PS_TEXTUREMODES_PROJECT2D to PS_TEXTUREMODES_PROJECT3D
    //         change PS_TEXTUREMODES_CUBEMAP to PS_TEXTUREMODES_PROJECT3D
    //         change PS_TEXTUREMODES_DOT_STR_CUBE to PS_TEXTUREMODES_DOT_STR_3D
    //     if set texture is neither cubemap or volume texture,
    //         change PS_TEXTUREMODES_PROJECT3D to PS_TEXTUREMODES_PROJECT2D
    //         change PS_TEXTUREMODES_CUBEMAP to PS_TEXTUREMODES_PROJECT2D

    PS_GLOBALFLAGS_NO_TEXMODE_ADJUST=     $0000, // don't adjust texture modes
    PS_GLOBALFLAGS_TEXMODE_ADJUST=        $0001  // adjust texture modes according to set texture
);

type
  // Work variables for the Disassemble* methods
  RPSDisassembleScope = record
    InstructionOutputCombiner: string;
    SourceRegisterModifier: string;
    Stage: int;
    OutputWriteMask: string;
    // Mapped rendering for C0 and C1 :
    C0RegStr: string;
    C1RegStr: string;
    // Final combiner registers :
    FogReg: PS_REGISTER;
    V1R0Reg: PS_REGISTER;
    EFReg: PS_REGISTER;
  end;
  PPSDisassembleScope = ^RPSDisassembleScope;

  PPSOutputRegister = ^RPSOutputRegister;
  RPSOutputRegister = object
    IsAlpha: Boolean;
    Reg: PS_REGISTER;
    procedure Decode(Value: Byte; aIsAlpha: Boolean);
    function IsSameAsAlpha(const Alpha: PPSOutputRegister): Boolean;
    function IntermediateToString(): string;
    function Disassemble(const aScope: PPSDisassembleScope): string;
  end;

  PPSInputRegister = ^RPSInputRegister;
  RPSInputRegister = object(RPSOutputRegister)
    Channel: PS_CHANNEL;
    InputMapping: PS_INPUTMAPPING;
    procedure Decode(Value: Byte; aIsAlpha: Boolean);
    function IsSameAsAlpha(const Alpha: PPSInputRegister): Boolean;
    function IntermediateToString(): string;
    function Disassemble(const aScope: PPSDisassembleScope): string;
  end;

  PPSCombinerOutput = ^RPSCombinerOutput;
  RPSCombinerOutput = object(RPSOutputRegister)
    DotProduct: Boolean; // False=Multiply, True=DotProduct
    BlueToAlpha: Boolean; // False=Alpha-to-Alpha, True=Blue-to-Alpha
    function IsSameAsAlpha(const Alpha: PPSCombinerOutput): Boolean;
    function CombineStageInputDot(const aScope: PPSDisassembleScope; const InputA, InputB: RPSInputRegister): string;
    function CombineStageInputMul(const aScope: PPSDisassembleScope; const InputA, InputB: RPSInputRegister): string;
  end;

  PPSCombinerStageChannel = ^RPSCombinerStageChannel;
  RPSCombinerStageChannel = record
    InputA: RPSInputRegister;
    InputB: RPSInputRegister;
    InputC: RPSInputRegister;
    InputD: RPSInputRegister;
    OutputAB: RPSCombinerOutput;
    OutputCD: RPSCombinerOutput;
    OutputSUM: RPSOutputRegister;
    CombinerOutputFlags: PS_COMBINEROUTPUT;
    AB_CD_MUX: Boolean; // False=AB+CD, True=MUX(AB;CD) based on R0.a
    procedure Decode(PSInputs, PSOutputs: DWORD; IsAlpha: Boolean = False);
    function IsSameAsAlpha(const Alpha: PPSCombinerStageChannel): Boolean;
    function DisassembleCombinerStage(const aScope: PPSDisassembleScope): string;
  end;

  RPSCombinerStage = record
    RGB: RPSCombinerStageChannel;
    Alpha: RPSCombinerStageChannel;

    MappedC0: DWORD; // C0 for each stage
    MappedC1: DWORD; // C1 for each stage

    function Disassemble(const aScope: PPSDisassembleScope): string;
  end;

  RPSFinalCombiner = record
    InputA: RPSInputRegister;
    InputB: RPSInputRegister;
    InputC: RPSInputRegister;
    InputD: RPSInputRegister;
    InputE: RPSInputRegister;
    InputF: RPSInputRegister;
    InputG: RPSInputRegister;

    FinalCombinerFlags: PS_FINALCOMBINERSETTING;

    FinalCombinerC0: DWORD;
    FinalCombinerC1: DWORD;

    dwPS_GLOBALFLAGS: DWORD;
    procedure Decode(const PSFinalCombinerInputsABCD, PSFinalCombinerInputsEFG: DWORD);
    function Disassemble(const aScope: PPSDisassembleScope): string;
  end;

  RPSIntermediate = record
    Original: X_D3DPIXELSHADERDEF;

    PSTextureModes: array[0..X_D3DTS_STAGECOUNT-1] of PS_TEXTUREMODES;
    PSDotMapping: array[0..X_D3DTS_STAGECOUNT-1] of PS_DOTMAPPING;
    PSCompareMode: array[0..X_D3DTS_STAGECOUNT-1] of DWORD;
    PSInputTexture: array[0..X_D3DTS_STAGECOUNT-1] of int;

    NumberOfCombiners: DWORD;
    CombinerCountFlags: DWORD; // For PS_COMBINERCOUNTFLAGS

    dwPSCCMux: DWORD; // Read from CombinerCountFlags
    dwPSCCC0: DWORD;
    dwPSCCC1: DWORD;

    Combiners: array [0..8-1] of RPSCombinerStage;

    FinalCombiner: RPSFinalCombiner;

    procedure Init(pPSDef: PX_D3DPIXELSHADERDEF);
  private
    procedure Decode(pPSDef: PX_D3DPIXELSHADERDEF);
    function DisassembleTextureMode(const aScope: PPSDisassembleScope; const NextIs2D: Boolean): string;
    function DisassembleTextureModes(const aScope: PPSDisassembleScope): string;

    function OriginalToString(): string;
    function IntermediateToString(): string;
    function Disassemble(): string;
  end;

// dump pixel shader definition to file
procedure XTL_DumpPixelShaderToFile(pPSDef: PX_D3DPIXELSHADERDEF);
// dump pixel shader definition to string
function XTL_DumpPixelShaderDefToString(pPSDef: PX_D3DPIXELSHADERDEF): string;
// print relevant contents to the debug console
procedure XTL_PrintPixelShaderDefContents(pPSDef: PX_D3DPIXELSHADERDEF);
// Recompile Xbox PixelShader def
function XTL_EmuRecompilePshDef(pPSDef: PX_D3DPIXELSHADERDEF): string;

implementation

const lfUnit = lfCxbx or lfPixelShader;

procedure DbgPshPrintf(aStr: string); overload;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  if (g_bPrintfOn) then // TODO -oDxbx: Remove this once our logging relies on MayLog completely
  Log(lfUnit, aStr);
end;

procedure DbgPshPrintf(aStr: string; Args: array of const); overload;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  if (g_bPrintfOn) then // TODO -oDxbx: Remove this once our logging relies on MayLog completely
  Log(lfUnit, aStr, Args);
end;

// From PixelShader.cpp -----------------------------------------------------------

type P_char = string; // Strings are easier for Delphi

// PS Texture Modes
const PS_TextureModesStr: array [PS_TEXTUREMODES] of P_char =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_TEXTUREMODES_NONE',                 // 0x00
    'PS_TEXTUREMODES_PROJECT2D',            // 0x01
    'PS_TEXTUREMODES_PROJECT3D',            // 0x02
    'PS_TEXTUREMODES_CUBEMAP',              // 0x03
    'PS_TEXTUREMODES_PASSTHRU',             // 0x04
    'PS_TEXTUREMODES_CLIPPLANE',            // 0x05
    'PS_TEXTUREMODES_BUMPENVMAP',           // 0x06
    'PS_TEXTUREMODES_BUMPENVMAP_LUM',       // 0x07
    'PS_TEXTUREMODES_BRDF',                 // 0x08
    'PS_TEXTUREMODES_DOT_ST',               // 0x09
    'PS_TEXTUREMODES_DOT_ZW',               // 0x0A
    'PS_TEXTUREMODES_DOT_RFLCT_DIFF',       // 0x0B
    'PS_TEXTUREMODES_DOT_RFLCT_SPEC',       // 0x0C
    'PS_TEXTUREMODES_DOT_STR_3D',           // 0x0D
    'PS_TEXTUREMODES_DOT_STR_CUBE',         // 0x0E
    'PS_TEXTUREMODES_DPNDNT_AR',            // 0x0F
    'PS_TEXTUREMODES_DPNDNT_GB',            // 0x10
    'PS_TEXTUREMODES_DOTPRODUCT',           // 0x11
    'PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST'  // 0x12
);

// PS DotMapping
const PS_DotMappingStr: array [PS_DOTMAPPING] of P_char =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_DOTMAPPING_ZERO_TO_ONE',      // 0x00
    'PS_DOTMAPPING_MINUS1_TO_1_D3D',  // 0x01
    'PS_DOTMAPPING_MINUS1_TO_1_GL',   // 0x02
    'PS_DOTMAPPING_MINUS1_TO_1',      // 0x03
    'PS_DOTMAPPING_HILO_1',           // 0x04
    '???',
    '???',
    'PS_DOTMAPPING_HILO_HEMISPHERE'   // 0x07
);

// PS CompareMode
const PS_CompareModeStr: array [{PS_COMPAREMODE=}0..8-1] of P_char =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_COMPAREMODE_S_LT', // 0x00L
    'PS_COMPAREMODE_S_GE', // 0x01L

    'PS_COMPAREMODE_T_LT', // 0x00L
    'PS_COMPAREMODE_T_GE', // 0x02L

    'PS_COMPAREMODE_R_LT', // 0x00L
    'PS_COMPAREMODE_R_GE', // 0x04L

    'PS_COMPAREMODE_Q_LT', // 0x00L
    'PS_COMPAREMODE_Q_GE'  // 0x08L
);

// PS CombinerCountFlags
const PS_CombinerCountFlagsStr: array [{PS_COMBINERCOUNT=}0..6-1] of P_char =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_COMBINERCOUNT_MUX_LSB',    // 0x0000L, // mux on r0.a lsb
    'PS_COMBINERCOUNT_MUX_MSB',    // 0x0001L, // mux on r0.a msb

    'PS_COMBINERCOUNT_SAME_C0',    // 0x0000L, // c0 same in each stage
    'PS_COMBINERCOUNT_UNIQUE_C0',  // 0x0010L, // c0 unique in each stage

    'PS_COMBINERCOUNT_SAME_C1',    // 0x0000L, // c1 same in each stage
    'PS_COMBINERCOUNT_UNIQUE_C1'   // 0x0100L  // c1 unique in each stage
);

// PS InputMapping
const PS_InputMappingStr: array [{PS_INPUTMAPPING=}0..8-1] of string =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_INPUTMAPPING_UNSIGNED_IDENTITY',  // 0x00L, // max(0,x)         OK for final combiner: y = abs(x)
    'PS_INPUTMAPPING_UNSIGNED_INVERT',    // 0x20L, // 1 - max(0,x)     OK for final combiner: y = 1 - x
    'PS_INPUTMAPPING_EXPAND_NORMAL',      // 0x40L, // 2*max(0,x) - 1   invalid for final combiner
    'PS_INPUTMAPPING_EXPAND_NEGATE',      // 0x60L, // 1 - 2*max(0,x)   invalid for final combiner
    'PS_INPUTMAPPING_HALFBIAS_NORMAL',    // 0x80L, // max(0,x) - 1/2   invalid for final combiner
    'PS_INPUTMAPPING_HALFBIAS_NEGATE',    // 0xa0L, // 1/2 - max(0,x)   invalid for final combiner
    'PS_INPUTMAPPING_SIGNED_IDENTITY',    // 0xc0L, // x                invalid for final combiner
    'PS_INPUTMAPPING_SIGNED_NEGATE'       // 0xe0L, // -x               invalid for final combiner
);

// PS Register (note, a few have one space, to line up the output a little)
const PS_RegisterStr: array [{PS_REGISTER=}0..21-1] of P_char =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_REGISTER_ZERO',      // 0x00L, // r
    'PS_REGISTER_DISCARD',   // 0x00L, // w
    'PS_REGISTER_C0 ',       // 0x01L, // r
    'PS_REGISTER_C1 ',       // 0x02L, // r
    'PS_REGISTER_FOG',       // 0x03L, // r
    'PS_REGISTER_V0 ',       // 0x04L, // r/w
    'PS_REGISTER_V1 ',       // 0x05L, // r/w
    '??', // 0x06
    '??', // 0x07
    'PS_REGISTER_T0 ',       // 0x08L, // r/w
    'PS_REGISTER_T1 ',       // 0x09L, // r/w
    'PS_REGISTER_T2 ',       // 0x0aL, // r/w
    'PS_REGISTER_T3 ',       // 0x0bL, // r/w
    'PS_REGISTER_R0 ',       // 0x0cL, // r/w
    'PS_REGISTER_R1 ',       // 0x0dL, // r/w
    'PS_REGISTER_V1R0_SUM',  // 0x0eL, // r
    'PS_REGISTER_EF_PROD',   // 0x0fL, // r

    'PS_REGISTER_ONE',               // PS_REGISTER_ZERO | PS_INPUTMAPPING_UNSIGNED_INVERT, // OK for final combiner
    'PS_REGISTER_NEGATIVE_ONE',      // PS_REGISTER_ZERO | PS_INPUTMAPPING_EXPAND_NORMAL,   // invalid for final combiner
    'PS_REGISTER_ONE_HALF',          // PS_REGISTER_ZERO | PS_INPUTMAPPING_HALFBIAS_NEGATE, // invalid for final combiner
    'PS_REGISTER_NEGATIVE_ONE_HALF'  // PS_REGISTER_ZERO | PS_INPUTMAPPING_HALFBIAS_NORMAL, // invalid for final combiner
);

// PS Channel
const PS_ChannelStr: array [{PS_CHANNEL=}0..3-1] of P_char =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_CHANNEL_RGB  ', // 0x00, // used as RGB source
    'PS_CHANNEL_BLUE ', // 0x00, // used as ALPHA source
    'PS_CHANNEL_ALPHA'  // 0x10, // used as RGB or ALPHA source
);

// PS FinalCombinerSetting
const PS_FinalCombinerSettingStr: array [{PS_FINALCOMBINERSETTING=}0..3-1] of P_char =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_FINALCOMBINERSETTING_CLAMP_SUM'  ,    // 0x80, // V1+R0 sum clamped to [0,1]
    'PS_FINALCOMBINERSETTING_COMPLEMENT_V1',  // 0x40, // unsigned invert mapping
    'PS_FINALCOMBINERSETTING_COMPLEMENT_R0'   // 0x20, // unsigned invert mapping
);

// PS CombineOutput
const PS_CombineOutputStr: array [{PS_COMBINEROUTPUT=}0..14-1] of P_char =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_COMBINEROUTPUT_IDENTITY',          // 0x00L, // y := x
    'PS_COMBINEROUTPUT_BIAS',              // 0x08L, // y := x - 0.5
    'PS_COMBINEROUTPUT_SHIFTLEFT_1',       // 0x10L, // y := x*2
    'PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS',  // 0x18L, // y := (x - 0.5)*2
    'PS_COMBINEROUTPUT_SHIFTLEFT_2',       // 0x20L, // y := x*4
    'PS_COMBINEROUTPUT_SHIFTRIGHT_1',      // 0x30L, // y := x/2

    'PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA',  // 0x80L, // RGB only

    'PS_COMBINEROUTPUT_CD_BLUE_TO_ALPHA',  // 0x40L, // RGB only

    'PS_COMBINEROUTPUT_AB_MULTIPLY',       // 0x00L,
    'PS_COMBINEROUTPUT_AB_DOT_PRODUCT',    // 0x02L, // RGB only

    'PS_COMBINEROUTPUT_CD_MULTIPLY',       // 0x00L,
    'PS_COMBINEROUTPUT_CD_DOT_PRODUCT',    // 0x01L, // RGB only

    'PS_COMBINEROUTPUT_AB_CD_SUM',         // 0x00L, // 3rd output is AB+CD
    'PS_COMBINEROUTPUT_AB_CD_MUX'          // 0x04L, // 3rd output is MUX(AB,CD) based on R0.a
);

// PS GlobalFlags
const PS_GlobalFlagsStr: array [PS_GLOBALFLAGS] of P_char =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_GLOBALFLAGS_NO_TEXMODE_ADJUST',     // 0x0000L, // don't adjust texture modes
    'PS_GLOBALFLAGS_TEXMODE_ADJUST'         // 0x0001L, // adjust texture modes according to set texture
);

function PSCombinerOutputFlagsToStr(const dwFlags: DWORD; IsAlpha: Boolean = False): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result :={Result + ' | ' +}PS_CombineOutputStr[ 0 + ((dwFlags and $38)                                   shr 3)];
  Result := Result + ' | ' + PS_CombineOutputStr[ 8 + ((dwFlags and Ord(PS_COMBINEROUTPUT_AB_DOT_PRODUCT)) shr 1)];
  Result := Result + ' | ' + PS_CombineOutputStr[10 + ((dwFlags and Ord(PS_COMBINEROUTPUT_CD_DOT_PRODUCT)) shr 0)];
  Result := Result + ' | ' + PS_CombineOutputStr[12 + ((dwFlags and Ord(PS_COMBINEROUTPUT_AB_CD_MUX))      shr 2)];

  if IsAlpha then
  begin
    if (dwFlags and Ord(PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA)) > 0 then
      Result := Result + ' | ' + PS_CombineOutputStr[6];

    if (dwFlags and Ord(PS_COMBINEROUTPUT_CD_BLUE_TO_ALPHA)) > 0 then
      Result := Result + ' | ' + PS_CombineOutputStr[7];
  end;
end;

function PSFinalCombinerSettingToStr(const dwPS_FINALCOMBINERSETTING: DWORD): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := '';
  if (dwPS_FINALCOMBINERSETTING and Ord(PS_FINALCOMBINERSETTING_CLAMP_SUM)) > 0 then
    Result := Result + ' | ' + PS_FinalCombinerSettingStr[0];

  if (dwPS_FINALCOMBINERSETTING and Ord(PS_FINALCOMBINERSETTING_COMPLEMENT_V1)) > 0 then
    Result := Result + ' | ' + PS_FinalCombinerSettingStr[1];

  if (dwPS_FINALCOMBINERSETTING and Ord(PS_FINALCOMBINERSETTING_COMPLEMENT_R0)) > 0 then
    Result := Result + ' | ' + PS_FinalCombinerSettingStr[2];

  if Result <> '' then
    Delete(Result, 1, 3);
end;

function PSRegToStr(const aScope: PPSDisassembleScope; const aReg: PS_REGISTER): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Assert((PS_REGISTER_C0 <= aReg) and (aReg <= PS_REGISTER_EF_PROD));

  case aReg of
    PS_REGISTER_C0: Result := aScope.C0RegStr;
    PS_REGISTER_C1: Result := aScope.C1RegStr;
    PS_REGISTER_FOG: Result := PSRegToStr(aScope, aScope.FogReg); // Emulated using a constant
    PS_REGISTER_V0: Result := 'v0';
    PS_REGISTER_V1: Result := 'v1';
    PS_REGISTER_T0: Result := 't0';
    PS_REGISTER_T1: Result := 't1';
    PS_REGISTER_T2: Result := 't2';
    PS_REGISTER_T3: Result := 't3';
    PS_REGISTER_R0: Result := 'r0';
    PS_REGISTER_R1: Result := 'r1';
    PS_REGISTER_V1R0_SUM: Result := PSRegToStr(aScope, aScope.V1R0Reg); // Uses a temporary register
    PS_REGISTER_EF_PROD: Result := PSRegToStr(aScope, aScope.EFReg); // Uses a temporary register
  else
    Result := '';
  end;
end;

{ RPSOutputRegister }

procedure RPSOutputRegister.Decode(Value: Byte; aIsAlpha: Boolean);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  IsAlpha := aIsAlpha;
  Reg := PS_REGISTER(Value);
end;

function RPSOutputRegister.IsSameAsAlpha(const Alpha: PPSOutputRegister): Boolean;
begin
  Result := (Reg = Alpha.Reg);
end;

function RPSOutputRegister.IntermediateToString(): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Assert((PS_REGISTER_DISCARD <= Reg) and (Reg <= PS_REGISTER_EF_PROD));

  Result := PS_RegisterStr[Ord(Reg) + 1];
end;

function RPSOutputRegister.Disassemble(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := PSRegToStr(aScope, Reg);
end;

{ RPSInputRegister }

procedure RPSInputRegister.Decode(Value: Byte; aIsAlpha: Boolean);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  inherited Decode(Value and PS_NoChannelMask, aIsAlpha);

  Channel := PS_CHANNEL(Value and Ord(PS_CHANNEL_ALPHA));
  InputMapping := PS_INPUTMAPPING(Value and $e0);

  // If the input Register isn't ZERO, remove the InputMapping flags :
  if (PS_REGISTER(Ord(Reg) and $f) <> PS_REGISTER_ZERO) then
    Reg := PS_REGISTER(Ord(Reg) and $f);
end;

function RPSInputRegister.IsSameAsAlpha(const Alpha: PPSInputRegister): Boolean;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := (Reg = Alpha.Reg)
        and (InputMapping = Alpha.InputMapping);
end;

(*
function PSCombinerInputToStr(const dwPSFCI: DWORD, IsAlpha: Boolean = False): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if dwPSFCI = Ord(PS_REGISTER_ZERO) then
    Result := PS_RegisterStr[0]
  else
  begin
    // First, render the channel as a string :
    Result := PS_ChannelStr[iif((dwPSFCI and Ord(PS_CHANNEL_ALPHA)) > 0, {Alpha}2, iif(IsAlpha, {Blue}1, {RGB}0))];

    // See if there's a special combination of flags (disregarding the channel bit) :
    case dwPSFCI and PS_NoChannelMask of
      Ord(PS_REGISTER_ONE):
        Result := PS_RegisterStr[$11] + ' | ' + Result;
      Ord(PS_REGISTER_NEGATIVE_ONE):
        Result := PS_RegisterStr[$12] + ' | ' + Result;
      Ord(PS_REGISTER_ONE_HALF):
        Result := PS_RegisterStr[$13] + ' | ' + Result;
      Ord(PS_REGISTER_NEGATIVE_ONE_HALF):
        Result := PS_RegisterStr[$14] + ' | ' + Result;
    else
      // Else, just render each part (including the previously determined channel) :
      Result := Format('%s | %s | %s', [
        PS_RegisterStr[(dwPSFCI and $F) + 1];
        Result;
        PS_InputMappingStr[(dwPSFCI shr 5) and 7]
        ]);
    end;
  end;
end;
*)
function RPSInputRegister.IntermediateToString(): string; // Was PSCombinerInputToStr
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  // First, render the channel as a string :
  Result := PS_ChannelStr[iif(Ord(Channel) > 0, {Alpha}2, iif(IsAlpha, {Blue}1, {RGB}0))];

  // Check if we're handling the special case register (zero) :
  if (PS_REGISTER(Ord(Reg) and $f) = PS_REGISTER_ZERO) then
  begin
    // In this case, we'll use the Input mapping as a selector for a few special registers :
    case PS_REGISTER(InputMapping) of
      PS_REGISTER_ZERO:
        // If there's no input mapping, the only thing we'll print is zero itself :
        Result := PS_RegisterStr[0];
      PS_REGISTER_ONE:
        Result := PS_RegisterStr[$11] + ' | ' + Result;
      PS_REGISTER_NEGATIVE_ONE:
        Result := PS_RegisterStr[$12] + ' | ' + Result;
      PS_REGISTER_ONE_HALF:
        Result := PS_RegisterStr[$13] + ' | ' + Result;
      PS_REGISTER_NEGATIVE_ONE_HALF:
        Result := PS_RegisterStr[$14] + ' | ' + Result;
    else
      Result := '??'; // PS_REGISTER_DISCARD ?
    end;

    Exit;
  end;

  Result := inherited IntermediateToString()
          + ' | ' + Result
          + ' | ' + PS_InputMappingStr[(Ord(InputMapping) shr 5) and 7];
end;

function RPSInputRegister.Disassemble(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := inherited Disassemble(aScope);
  if Result = '' then
    Exit;

  case InputMapping of
//    PS_INPUTMAPPING_UNSIGNED_IDENTITY:
//      Result := Result + '_sat';
    PS_INPUTMAPPING_UNSIGNED_INVERT: // 1-x
      Result := '1-' + Result;
    PS_INPUTMAPPING_EXPAND_NORMAL: // (2*x) - 1
      Result := Result + '_bx2';
    PS_INPUTMAPPING_EXPAND_NEGATE: // 1 - (2*x)
      Result := '1-' + Result + '_x2';
//    PS_INPUTMAPPING_HALFBIAS_NORMAL=   $80; // max(0,x) - 1/2   invalid for final combiner
//    PS_INPUTMAPPING_HALFBIAS_NEGATE=   $a0; // 1/2 - max(0,x)   invalid for final combiner
    PS_INPUTMAPPING_SIGNED_IDENTITY: ; // x
    PS_INPUTMAPPING_SIGNED_NEGATE: // -x
      Result := '-' + Result;
  end;
end;

{ RPSCombinerOutput }

function RPSCombinerOutput.IsSameAsAlpha(const Alpha: PPSCombinerOutput): Boolean;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := (Reg = Alpha.Reg)
        and (DotProduct = Alpha.DotProduct)
        and (BlueToAlpha = Alpha.BlueToAlpha);
end;

function RPSCombinerOutput.CombineStageInputDot(const aScope: PPSDisassembleScope; const InputA, InputB: RPSInputRegister): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := 'dp3' + aScope.InstructionOutputCombiner + ' ' +
    Self.Disassemble(aScope) + aScope.OutputWriteMask + ', ' +
    InputA.Disassemble(aScope) + ', ' +
    InputB.Disassemble(aScope);
end;

function RPSCombinerOutput.CombineStageInputMul(const aScope: PPSDisassembleScope; const InputA, InputB: RPSInputRegister): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  OutputStr: string;
  InputAReadMask: string;
  InputBReadMask: string;
begin
  if IsAlpha then
  begin
    if (InputA.Channel = PS_CHANNEL_BLUE) then
      InputAReadMask := '.b'
    else
      InputAReadMask := '.a';

    if (InputB.Channel = PS_CHANNEL_BLUE) then
      InputBReadMask := '.b'
    else
      InputBReadMask := '.a';
  end
  else
  begin
    InputAReadMask := '';
    InputBReadMask := '';
  end;

  Result := '';
  OutputStr := aScope.InstructionOutputCombiner + ' ' + Self.Disassemble(aScope) + aScope.OutputWriteMask + ', ';

  // Check for cases where InputA doesn't matter :
  case InputA.Reg of
    PS_REGISTER_ONE: // = input * 1.0 = input
    begin
      if InputB.Reg = PS_REGISTER_ONE then // When InputB = PS_REGISTER_ONE, skip this
        Exit;

      Result := Result + 'mov' + OutputStr + InputB.Disassemble(aScope) + InputBReadMask;
    end;
    PS_REGISTER_NEGATIVE_ONE: // = input * -1.0 = - input
      Result := Result + 'mov' + OutputStr + '-' + InputB.Disassemble(aScope) + InputBReadMask;
//    PS_REGISTER_ONE_HALF: // = input * 0.5
//    PS_REGISTER_NEGATIVE_ONE_HALF: // = input * -0.5
  end;

  if Result <> '' then
    Exit;

  // Check for cases where InputB doesn't matter :
  case InputB.Reg of
    PS_REGISTER_ONE: // = input * 1.0 = input
      Result := Result + 'mov' + OutputStr + InputA.Disassemble(aScope) + InputAReadMask;
    PS_REGISTER_NEGATIVE_ONE: // = input * -1.0 = - input
      Result := Result + 'mov' + OutputStr + '-' + InputA.Disassemble(aScope) + InputAReadMask;
//    Ord(PS_REGISTER_ONE_HALF): // = input * 0.5
//    Ord(PS_REGISTER_NEGATIVE_ONE_HALF): // = -0.5
  end;

  if Result <> '' then
    Exit;

  if (InputA.Reg = PS_REGISTER_ZERO) or (InputB.Reg = PS_REGISTER_ZERO) then
    // Ouput := ZERO * something = zero, which can be simulated by subtracting a (guaranteed) register from itself :
    Result := Result + 'sub' + OutputStr + 'v0, v0'
  else
    Result := Result + 'mul' + OutputStr + InputA.Disassemble(aScope) + InputAReadMask + ', ' + InputB.Disassemble(aScope) + InputBReadMask;
end;

{ RPSCombinerStageChannel }

procedure RPSCombinerStageChannel.Decode(PSInputs, PSOutputs: DWORD; IsAlpha: Boolean = False);
begin
  // Decode PSAlphaInputs / PSRGBInputs :
  InputA.Decode((PSInputs shr 24) and $FF, IsAlpha);
  InputB.Decode((PSInputs shr 16) and $FF, IsAlpha);
  InputC.Decode((PSInputs shr  8) and $FF, IsAlpha);
  InputD.Decode((PSInputs shr  0) and $FF, IsAlpha);

  // Decode PSAlphaOutputs / PSRGBOutputs :
  OutputAB.Decode((PSOutputs shr 4) and $F, IsAlpha);
  OutputCD.Decode((PSOutputs shr 0) and $F, IsAlpha);
  OutputSUM.Decode((PSOutputs shr 8) and $F, IsAlpha);

  // Get the combiner output flags :
  CombinerOutputFlags := PS_COMBINEROUTPUT(PSOutputs shr 12);

  // Decompose the combiner output flags :
  OutputAB.DotProduct := (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_DOT_PRODUCT) > 0; // False=Multiply, True=DotProduct
  OutputCD.DotProduct := (CombinerOutputFlags and PS_COMBINEROUTPUT_CD_DOT_PRODUCT) > 0; // False=Multiply, True=DotProduct

  OutputAB.BlueToAlpha := (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA) > 0; // False=Alpha-to-Alpha, True=Blue-to-Alpha
  OutputCD.BlueToAlpha := (CombinerOutputFlags and PS_COMBINEROUTPUT_CD_BLUE_TO_ALPHA) > 0; // False=Alpha-to-Alpha, True=Blue-to-Alpha

  AB_CD_MUX := (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_CD_MUX) > 0; // False=AB+CD, True=MUX(AB,CD) based on R0.a
end;

// Checks if this (RGB) stage is identical to the Alhpa stage.
function RPSCombinerStageChannel.IsSameAsAlpha(const Alpha: PPSCombinerStageChannel): Boolean;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := OutputAB.IsSameAsAlpha(@(Alpha.OutputAB))
        and OutputCD.IsSameAsAlpha(@(Alpha.OutputCD))
        and OutputSUM.IsSameAsAlpha(@(Alpha.OutputSUM))
        and InputA.IsSameAsAlpha(@(Alpha.InputA))
        and InputB.IsSameAsAlpha(@(Alpha.InputB))
        and InputC.IsSameAsAlpha(@(Alpha.InputC))
        and InputD.IsSameAsAlpha(@(Alpha.InputD));
end;

function RPSCombinerStageChannel.DisassembleCombinerStage(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  SumOutputString: string;
begin
  Result := '';

  // Convert the CombinerOutput flag to a InstructionOutputCombiner
  // or an SourceRegisterModifier (so far as that's possible):
  aScope.InstructionOutputCombiner := '';
  aScope.SourceRegisterModifier := '';
  case PS_COMBINEROUTPUT(Ord(CombinerOutputFlags) and $38) of
    PS_COMBINEROUTPUT_IDENTITY:         aScope.InstructionOutputCombiner := '';      // y = x
    PS_COMBINEROUTPUT_BIAS:             aScope.SourceRegisterModifier    := '_bias'; // y = x - 0.5
    PS_COMBINEROUTPUT_SHIFTLEFT_1:      aScope.InstructionOutputCombiner := '_x2';   // y = x*2
    PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS: aScope.SourceRegisterModifier    := '_bias_x2'; // y = (x - 0.5)*2
    PS_COMBINEROUTPUT_SHIFTLEFT_2:      aScope.InstructionOutputCombiner := '_x4';   // y = x*4
    PS_COMBINEROUTPUT_SHIFTRIGHT_1:     aScope.InstructionOutputCombiner := '_d2';   // y = x/2
  end;

  // Do we need to calculate AB ?
  if OutputAB.Reg > PS_REGISTER_DISCARD then
  begin
    // Handle combining of A and B (doing either a dot-product, or a multiplication) :
    if OutputAB.DotProduct then
      Result := Result + OutputAB.CombineStageInputDot(aScope, InputA, InputB)
    else
      Result := Result + OutputAB.CombineStageInputMul(aScope, InputA, InputB);

    if Result <> '' then
      Result := Result + #13#10;

    // The blue-to-alpha flag is only valid for RGB, so the '+' extend syntax if free to use :
    if OutputAB.BlueToAlpha then
      Result := Result + '+ mov ' + OutputAB.Disassemble(aScope) + '.a, ' + OutputAB.Disassemble(aScope) + '.b'#13#10;
  end;

  // Do we need to calculate CD ?
  if OutputCD.Reg > PS_REGISTER_DISCARD then
  begin
    // Handle combining of C and D (doing either a dot-product, or a multiplication) :
    if OutputCD.DotProduct then
      Result := Result + OutputCD.CombineStageInputDot(aScope, InputC, InputD)
    else
      Result := Result + OutputCD.CombineStageInputMul(aScope, InputC, InputD);

    if Result <> '' then
      Result := Result + #13#10;

    // The blue-to-alpha flag is only valid for RGB, so the '+' extend syntax if free to use :
    if OutputCD.BlueToAlpha then
      Result := Result + '+ mov ' + OutputCD.Disassemble(aScope) + '.a, ' + OutputCD.Disassemble(aScope) + '.b'#13#10;
  end;

  // Do we need to calculate SUM ?
  if OutputSUM.Reg > PS_REGISTER_DISCARD then
  begin
    SumOutputString := aScope.InstructionOutputCombiner + ' ' + OutputSUM.Disassemble(aScope) + aScope.OutputWriteMask + ', ';
    if AB_CD_MUX then
    begin
      // Handle PS_COMBINEROUTPUT_AB_CD_MUX, output is MUX(AB,CD) based on R0.a :

      if (OutputAB.Reg = PS_REGISTER_DISCARD) then
      begin
        if (OutputCD.Reg = PS_REGISTER_DISCARD) then
        begin
          if  (InputB.Reg = PS_REGISTER_ONE)
          and (InputD.Reg = PS_REGISTER_ONE) then
            Result := Result + 'cnd' + SumOutputString + 'r0.a, ' +
              InputA.Disassemble(aScope) + ', ' +
              InputC.Disassemble(aScope) + #13#10
          else
          if  (InputA.Reg = PS_REGISTER_ONE)
          and (InputD.Reg = PS_REGISTER_ONE) then
            Result := Result + 'cnd' + SumOutputString + 'r0.a, ' +
              InputB.Disassemble(aScope) + ', ' +
              InputC.Disassemble(aScope) + #13#10
          else
          if  (InputB.Reg = PS_REGISTER_ONE)
          and (InputC.Reg = PS_REGISTER_ONE) then
            Result := Result + 'cnd' + SumOutputString + 'r0.a, ' +
              InputA.Disassemble(aScope) + ', ' +
              InputD.Disassemble(aScope) + #13#10
          else
          if  (InputA.Reg = PS_REGISTER_ONE)
          and (InputC.Reg = PS_REGISTER_ONE) then
            Result := Result + 'cnd' + SumOutputString + 'r0.a, ' +
              InputB.Disassemble(aScope) + ', ' +
              InputD.Disassemble(aScope) + #13#10
          else
          begin
            // TODO : We use Sum register as a temp, which could pose a problem if one or more of the inputs use the same register!
            Result := Result + 'mul' + SumOutputString +
              InputA.Disassemble(aScope) + ', ' +
              InputB.Disassemble(aScope) + #13#10;
            Result := Result + 'mad' + SumOutputString +
              InputC.Disassemble(aScope) + ', ' +
              InputD.Disassemble(aScope) + ', ' +
              OutputSUM.Disassemble(aScope) + #13#10;
          end;
        end
        else
          // TODO :
          Result := Result + '; Can''t mux when AB is discarded!'#13#10;
      end
      else
      begin
        if (OutputCD.Reg = PS_REGISTER_DISCARD) then
        begin
          // TODO :
          Result := Result + '; Can''t mux when CD is discarded!'#13#10;
        end
        else
        begin
          Result := Result + 'cnd' + SumOutputString + 'r0.a, ' +
            OutputAB.Disassemble(aScope) + ', ' +
            OutputCD.Disassemble(aScope) + #13#10;
        end;
      end;
    end
    else
    begin
      // Handle PS_COMBINEROUTPUT_AB_CD_SUM, output is AB+CD :

      // TODO : Handle PS_INPUTMAPPING here too !
      if (OutputAB.Reg = PS_REGISTER_DISCARD) then
      begin
        if (OutputCD.Reg = PS_REGISTER_DISCARD) then
        begin
          // AB and CD are discarded, but we still have to calculate "sum = (A * B) + (C * D)"
          // First, check if there are effectively 2 input (when both are multiplied by one) :
          if  (InputB.Reg = PS_REGISTER_ONE)
          and (InputD.Reg = PS_REGISTER_ONE) then
            Result := Result + 'add' + SumOutputString +
              InputA.Disassemble(aScope) + ', ' +
              InputC.Disassemble(aScope) + #13#10
          else
          if  (InputA.Reg = PS_REGISTER_ONE)
          and (InputD.Reg = PS_REGISTER_ONE) then
            Result := Result + 'add' + SumOutputString +
              InputB.Disassemble(aScope) + ', ' +
              InputC.Disassemble(aScope) + #13#10
          else
          if  (InputB.Reg = PS_REGISTER_ONE)
          and (InputC.Reg = PS_REGISTER_ONE) then
            Result := Result + 'add' + SumOutputString +
              InputA.Disassemble(aScope) + ', ' +
              InputD.Disassemble(aScope) + #13#10
          else
          if  (InputA.Reg = PS_REGISTER_ONE)
          and (InputC.Reg = PS_REGISTER_ONE) then
            Result := Result + 'add' + SumOutputString +
              InputB.Disassemble(aScope) + ', ' +
              InputD.Disassemble(aScope) + #13#10
          else
          // The problem is, there's no instruction for that. Luckily, we do have 'mad' to our disposal;
          // which can do "output = (input1 * input2) + input3", but if we want to use that, we must check
          // if one of the inputs is 1, so that it can be ignored (as "A * 1" equals "A") :
          if (InputA.Reg = PS_REGISTER_ONE) then
            Result := Result + 'mad' + SumOutputString +
              InputC.Disassemble(aScope) + ', ' +
              InputD.Disassemble(aScope) + ', ' +
              InputB.Disassemble(aScope) + #13#10
          else
          if (InputB.Reg = PS_REGISTER_ONE) then
            Result := Result + 'mad' + SumOutputString +
              InputC.Disassemble(aScope) + ', ' +
              InputD.Disassemble(aScope) + ', ' +
              InputA.Disassemble(aScope) + #13#10
          else
          if (InputC.Reg = PS_REGISTER_ONE) then
            Result := Result + 'mad' + SumOutputString +
              InputA.Disassemble(aScope) + ', ' +
              InputB.Disassemble(aScope) + ', ' +
              InputD.Disassemble(aScope) + #13#10
          else
          if (InputD.Reg = PS_REGISTER_ONE) then
            Result := Result + 'mad' + SumOutputString +
              InputA.Disassemble(aScope) + ', ' +
              InputB.Disassemble(aScope) + ', ' +
              InputC.Disassemble(aScope) + #13#10
          else
          begin
            if  (InputA.Reg = InputD.Reg)
            and (InputA.Channel = InputD.Channel)
            and (InputA.InputMapping = InputD.InputMapping)
            and True{InputA is inverse of InputD PS_INPUTMAPPING_UNSIGNED_INVERT } then
            begin
              Result := Result + 'lrp' + SumOutputString +
                InputA.Disassemble(aScope) + ', ' +
                InputB.Disassemble(aScope) + ', ' +
                InputC.Disassemble(aScope) + #13#10
            end
            else
              // TODO : We use Sum register as a temp, which could pose a problem if one or more of the inputs use the same register!
              Result := Result + 'mul' + SumOutputString +
                InputA.Disassemble(aScope) + ', ' +
                InputB.Disassemble(aScope) + #13#10;
              Result := Result + 'mad' + SumOutputString +
                InputC.Disassemble(aScope) + ', ' +
                InputD.Disassemble(aScope) + ', ' +
                OutputSUM.Disassemble(aScope) + #13#10;
          end;
        end
        else
        begin
          // Only AB is discarded, but we still have to calculate "sum = (A * B) + (C * D)"
          Result := Result + 'mad' + SumOutputString +
            InputA.Disassemble(aScope) + ', ' +
            InputB.Disassemble(aScope) + ', ' +
            OutputCD.Disassemble(aScope) + #13#10
        end;
      end
      else
      begin
        if (OutputCD.Reg = PS_REGISTER_DISCARD) then
        begin
          // Only CD is discarded, but we still have to calculate "sum = (A * B) + (C * D)"
          Result := Result + 'mad' + SumOutputString +
            InputC.Disassemble(aScope) + ', ' +
            InputD.Disassemble(aScope) + ', ' +
            OutputAB.Disassemble(aScope) + #13#10;
        end
        else
          Result := Result + 'add' + SumOutputString +
            OutputAB.Disassemble(aScope) + ', ' +
            OutputCD.Disassemble(aScope) + #13#10;
      end;
    end;
  end;
end;

{ RPSCombinerStage }

// Convert each stage's output-from-input mappings, back to PC-compatible pixel shader instructions
function RPSCombinerStage.Disassemble(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  StageOutputStrRGB: string;
  StageOutputStrAlpha: string;
begin
  aScope.C0RegStr := 'c' + IntToStr(MappedC0);
  aScope.C1RegStr := 'c' + IntToStr(MappedC1);

  Result := '; combine stage ' + IntToStr(aScope.Stage) + #13#10;

  // Check if RGB and Alpha are handled identical :
  if  RGB.IsSameAsAlpha(@Alpha) then
  begin
    // In that case, we combine both channels in one go without Output Write Masks (which defaults to '.rgba') :
    aScope.OutputWriteMask := '';
    Result := Result + RGB.DisassembleCombinerStage(aScope);
  end
  else
  begin
    // Else, handle rgb separately from alpha :
    aScope.OutputWriteMask := '.rgb';
    StageOutputStrRGB := RGB.DisassembleCombinerStage(aScope);

    aScope.OutputWriteMask := '.a';
    StageOutputStrAlpha := Alpha.DisassembleCombinerStage(aScope);

    Result := Result + StageOutputStrRGB;
    if StageOutputStrAlpha <> '' then
    begin
      if StageOutputStrRGB <> '' then
        Result := Result + '+ ';

      Result := Result + StageOutputStrAlpha;
    end;
  end;
end;

{ RPSFinalCombiner }

procedure RPSFinalCombiner.Decode(const PSFinalCombinerInputsABCD, PSFinalCombinerInputsEFG: DWORD);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  InputA.Decode((PSFinalCombinerInputsABCD shr 24) and $FF, {IsAlpha=}False);
  InputB.Decode((PSFinalCombinerInputsABCD shr 16) and $FF, {IsAlpha=}False);
  InputC.Decode((PSFinalCombinerInputsABCD shr  8) and $FF, {IsAlpha=}False);
  InputD.Decode((PSFinalCombinerInputsABCD shr  0) and $FF, {IsAlpha=}False);
  InputE.Decode((PSFinalCombinerInputsEFG  shr 24) and $FF, {IsAlpha=}False);
  InputF.Decode((PSFinalCombinerInputsEFG  shr 16) and $FF, {IsAlpha=}False);
  InputG.Decode((PSFinalCombinerInputsEFG  shr  8) and $FF, {IsAlpha=}False);
  // TODO : Give the following fields a better name:
  FinalCombinerFlags := PS_FINALCOMBINERSETTING((PSFinalCombinerInputsEFG shr 0) and $FF);
end;

function RPSFinalCombiner.Disassemble(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  TmpReg: PS_REGISTER;
  AlreadyHandled_D: Boolean;
begin
  Result := '';
  AlreadyHandled_D := False;

  aScope.C0RegStr := 'c' + IntToStr(FinalCombinerC0);
  aScope.C1RegStr := 'c' + IntToStr(FinalCombinerC1);

//  The final combiner performs the following operations :
//
//    prod register = E*F                // PS_REGISTER_EF_PROD, useable in A,B,C,D,G
//
//    rgbout        = A*B + (1-A)*C + D  // lrp tmp.rgb, A, B, C       // Note : tmp can be r0 if [A,B,C,D] * r0 = []
//                                       // add r0.rgb, tmp.rgb, D.rgb // Otherwise use a writable register from A;B or C
//
//    alphaout      = G.a                // mov r0.a, G.a              // Not necessary if G = r0
//
//    (also the final combiner can read PS_REGISTER_V1R0_SUM, which is equal to v1 + r0)
//  Normal optimizations apply, like when A = PS_REGISTER_ZERO, all we have left is C + D (add r0.rgb, C.rgb, D.rgb)
//  Also, if D = PS_REGISTER_ZERO, the add can be changed into a mov (if the result isn't already in r0.rgb)

  // Handle PS_REGISTER_FOG :
  if (InputA.Reg = PS_REGISTER_FOG)
  or (InputB.Reg = PS_REGISTER_FOG)
  or (InputC.Reg = PS_REGISTER_FOG)
  or (InputD.Reg = PS_REGISTER_FOG)
  or (InputE.Reg = PS_REGISTER_FOG)
  or (InputF.Reg = PS_REGISTER_FOG)
  or (InputG.Reg = PS_REGISTER_FOG) then
  begin
    Result := Result + '; final combiner - FOG detected - NOT EMULATED!'#13#10;
    aScope.FogReg := PS_REGISTER_C0; // TODO : Detect this beforehand and use a constant register (if one is available!)
  end;

  // Handle PS_REGISTER_EF_PROD :
  if (InputE.Reg > PS_REGISTER_ZERO) or (InputF.Reg > PS_REGISTER_ZERO) then
  begin
    Result := Result + '; final combiner - E*F'#13#10;
    if (InputE.Reg = PS_REGISTER_R0)
    or (InputF.Reg = PS_REGISTER_R0) then
      aScope.EFReg := PS_REGISTER_R0
    else
      aScope.EFReg := PS_REGISTER_R1; // TODO : Find another temp than R0 for E*F

    Result := Result + 'mul ' + PSRegToStr(aScope, aScope.EFReg) + ', ' + InputE.Disassemble(aScope) + ', ' + InputF.Disassemble(aScope) + #13#10;
  end;

  // Handle PS_REGISTER_V1R0_SUM :
  if (InputA.Reg = PS_REGISTER_V1R0_SUM)
  or (InputB.Reg = PS_REGISTER_V1R0_SUM)
  or (InputC.Reg = PS_REGISTER_V1R0_SUM)
  or (InputD.Reg = PS_REGISTER_V1R0_SUM)
  or (InputG.Reg = PS_REGISTER_V1R0_SUM) then
  begin
    Result := Result + '; final combiner - V1+R0'#13#10;
    aScope.V1R0Reg := PS_REGISTER_R0; // TODO : Find a temporary register

    // TODO : Handle the settings :
    // PS_FINALCOMBINERSETTING_CLAMP_SUM : V1+R0 sum clamped to [0,1]
    // PS_FINALCOMBINERSETTING_COMPLEMENT_V1 : unsigned invert mapping  (1 - v1) is used as an input to the sum rather than v1
    // PS_FINALCOMBINERSETTING_COMPLEMENT_R0 : unsigned invert mapping  (1 - r0) is used as an input to the sum rather than r0

    Result := Result + 'add ' + PSRegToStr(aScope, aScope.V1R0Reg) + ', ' + PSRegToStr(aScope, PS_REGISTER_V1) + ', ' + PSRegToStr(aScope, PS_REGISTER_R0) + #13#10;
  end;

  // Handle the final combiner's linear interpolation :
  if  ((InputA.Reg = PS_REGISTER_ZERO) or (InputA.Reg = PS_REGISTER_R0))
  and ((InputB.Reg = PS_REGISTER_ZERO) or (InputB.Reg = PS_REGISTER_R0))
  and ((InputC.Reg = PS_REGISTER_ZERO) or (InputC.Reg = PS_REGISTER_R0)) then
    // do nothing
  else
  begin
    Result := Result + '; final combiner - r0 = A*B + (1-A)*C + D'#13#10;
    TmpReg := PS_REGISTER_R0; // TODO : Check if r0 is usable

    if InputC.Reg = PS_REGISTER_ZERO then
    begin
      if (InputA.Reg = PS_REGISTER_ONE) then
      begin
        // TODO
      end
      else if (InputB.Reg = PS_REGISTER_ONE) then
      begin
        // TODO
      end
      else
      begin
        // r0 = A*B + D
        Result := Result + 'mad ' + PSRegToStr(aScope, PS_REGISTER_R0) + ', ' + InputA.Disassemble(aScope) + ', ' + InputB.Disassemble(aScope) + ', ' + InputD.Disassemble(aScope) + #13#10;
        // Reset D - already handled :
        AlreadyHandled_D := True;
      end;
    end
    else
      // r0 = A*B + (1-A)*C + D
      Result := Result + 'lrp ' + PSRegToStr(aScope, TmpReg) + ', ' + InputA.Disassemble(aScope) + ', ' + InputB.Disassemble(aScope) + ', ' + InputC.Disassemble(aScope) + #13#10;
  end;

  if (TmpReg = PS_REGISTER_ZERO) or (TmpReg = PS_REGISTER_R0) then
  begin
    if (InputD.Reg = PS_REGISTER_ZERO) or (InputD.Reg = PS_REGISTER_R0) then
      // do nothing
    else
      Result := Result + 'mov r0.rgb, ' + InputD.Disassemble(aScope) + #13#10;
  end
  else
  begin
    if AlreadyHandled_D then
      Result := Result + 'mov r0.rgb, ' + PSRegToStr(aScope, TmpReg) + #13#10
    else
      Result := Result + 'add r0.rgb, ' + PSRegToStr(aScope, TmpReg) + ', ' + InputD.Disassemble(aScope) + #13#10;
  end;

  // Handle alphaout :
  if (InputG.Reg = PS_REGISTER_ONE) or (InputG.Reg = PS_REGISTER_R0) then
    // Do nothing if G = 1 or r0
  else
  begin
    Result := Result + '; final combiner - alphaout'#13#10;
    if InputG.Reg = PS_REGISTER_ZERO then
      // R0.A := 0, can be simulated by subtracting a (guaranteed) register from itself :
      Result := Result + 'sub r0.a, v0, v0'#13#10
    else
      Result := Result + 'mov r0.a, ' + InputG.Disassemble(aScope) + #13#10;
  end;
end;

{ RPSIntermediate }

procedure RPSIntermediate.Init(pPSDef: PX_D3DPIXELSHADERDEF);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Assert(Assigned(pPSDef));
  Original := pPSDef^;

  Decode(@Original);
end;

function RPSIntermediate.OriginalToString(): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result :=Format('PSAphaInputs[8]              = 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X'#13#10 +
                  'PSFinalCombinerInputsABCD    = 0x%.08X'#13#10 +
                  'PSFinalCombinerInputsEFG     = 0x%.08X'#13#10 +
                  'PSConstant0[8]               = 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X'#13#10 +
                  'PSConstant1[8]               = 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X'#13#10 +
                  'PSAlphaOutputs[8]            = 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X'#13#10 +
                  'PSRGBInputs[8]               = 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X'#13#10 +
                  'PSCompareMode                = 0x%.08X'#13#10 +
                  'PSFinalCombinerConstant0     = 0x%.08X'#13#10 +
                  'PSFinalCombinerConstant1     = 0x%.08X'#13#10 +
                  'PSRGBOutputs[8]              = 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X'#13#10 +
                  'PSCombinerCount              = 0x%.08X'#13#10 +
                  'PSTextureModes               = 0x%.08X'#13#10 +
                  'PSDotMapping                 = 0x%.08X'#13#10 +
                  'PSInputTexture               = 0x%.08X'#13#10 +
                  'PSC0Mapping                  = 0x%.08X'#13#10 +
                  'PSC1Mapping                  = 0x%.08X'#13#10 +
                  'PSFinalCombinerConstants     = 0x%.08X'#13#10,
                  [Original.PSAlphaInputs[0], Original.PSAlphaInputs[1], Original.PSAlphaInputs[2], Original.PSAlphaInputs[3],
                  Original.PSAlphaInputs[4], Original.PSAlphaInputs[5], Original.PSAlphaInputs[6], Original.PSAlphaInputs[7],
                  Original.PSFinalCombinerInputsABCD,
                  Original.PSFinalCombinerInputsEFG,
                  Original.PSConstant0[0], Original.PSConstant0[1], Original.PSConstant0[2], Original.PSConstant0[3],
                  Original.PSConstant0[4], Original.PSConstant0[5], Original.PSConstant0[6], Original.PSConstant0[7],
                  Original.PSConstant1[0], Original.PSConstant1[1], Original.PSConstant1[2], Original.PSConstant1[3],
                  Original.PSConstant1[4], Original.PSConstant1[5], Original.PSConstant1[6], Original.PSConstant1[7],
                  Original.PSAlphaOutputs[0], Original.PSAlphaOutputs[1], Original.PSAlphaOutputs[2], Original.PSAlphaOutputs[3],
                  Original.PSAlphaOutputs[4], Original.PSAlphaOutputs[5], Original.PSAlphaOutputs[6], Original.PSAlphaOutputs[7],
                  Original.PSRGBInputs[0], Original.PSRGBInputs[1], Original.PSRGBInputs[2], Original.PSRGBInputs[3],
                  Original.PSRGBInputs[4], Original.PSRGBInputs[5], Original.PSRGBInputs[6], Original.PSRGBInputs[7],
                  Original.PSCompareMode,
                  Original.PSFinalCombinerConstant0,
                  Original.PSFinalCombinerConstant1,
                  Original.PSRGBOutputs[0], Original.PSRGBOutputs[1], Original.PSRGBOutputs[2], Original.PSRGBOutputs[3],
                  Original.PSRGBOutputs[4], Original.PSRGBOutputs[5], Original.PSRGBOutputs[6], Original.PSRGBOutputs[7],
                  Original.PSCombinerCount,
                  Original.PSTextureModes,
                  Original.PSDotMapping,
                  Original.PSInputTexture,
                  Original.PSC0Mapping,
                  Original.PSC1Mapping,
                  Original.PSFinalCombinerConstants]);
end;

procedure RPSIntermediate.Decode(pPSDef: PX_D3DPIXELSHADERDEF);
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  i: int;
begin
  for i := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    PSTextureModes[i] := PS_TEXTUREMODES((pPSDef.PSTextureModes shr (i*5)) and $1F);
    PSCompareMode[i] := (pPSDef.PSCompareMode shr (i*4)) and $F;
  end;

  PSDotMapping[0] := PS_DOTMAPPING(0);
  PSDotMapping[1] := PS_DOTMAPPING((pPSDef.PSDotMapping shr 0) and $7);
  PSDotMapping[2] := PS_DOTMAPPING((pPSDef.PSDotMapping shr 4) and $7);
  PSDotMapping[3] := PS_DOTMAPPING((pPSDef.PSDotMapping shr 8) and $7);

  PSInputTexture[0] := -1; // Stage 0 has no predecessors
  PSInputTexture[1] := 0; // Stage 1 can only use stage 0
  PSInputTexture[2] := (pPSDef.PSInputTexture shr 16) and $1; // Stage 2 can use stage 0 or 1
  PSInputTexture[3] := (pPSDef.PSInputTexture shr 20) and $3; // Stage 3 can only use stage 0, 1 or 2

  NumberOfCombiners := (pPSDef.PSCombinerCount shr 0) and $F;
  CombinerCountFlags := (pPSDef.PSCombinerCount shr 8);

  for i := 0 to 8 - 1 do
  begin
    Combiners[i].RGB.Decode(pPSDef.PSRGBInputs[i], pPSDef.PSRGBOutputs[i]);
    Combiners[i].Alpha.Decode(pPSDef.PSAlphaInputs[i], pPSDef.PSAlphaOutputs[i], {IsAlpha=}True);

    // Decode & map the C0 and C1 registers :
    if (CombinerCountFlags and PS_COMBINERCOUNT_UNIQUE_C0) > 0 then
      Combiners[i].MappedC0 := (pPSDef.PSC0Mapping shr (i * 4)) and $f
    else
      Combiners[i].MappedC0 := 0;

    if (CombinerCountFlags and PS_COMBINERCOUNT_UNIQUE_C1) > 0 then
      Combiners[i].MappedC1 := (pPSDef.PSC1Mapping shr (i * 4)) and $f
    else
      Combiners[i].MappedC1 := 1;
  end;

  FinalCombiner.Decode(pPSDef.PSFinalCombinerInputsABCD, pPSDef.PSFinalCombinerInputsEFG);

  FinalCombiner.FinalCombinerC0 := (pPSDef.PSFinalCombinerConstants shr 0) and $F;
  FinalCombiner.FinalCombinerC1 := (pPSDef.PSFinalCombinerConstants shr 4) and $F;
  FinalCombiner.dwPS_GLOBALFLAGS := (pPSDef.PSFinalCombinerConstants shr 8) and $1;
end;

// print relevant contents to the debug console
function RPSIntermediate.IntermediateToString(): string;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100

  procedure _Add(const aStr: string); overload;
  begin
    Result := Result + aStr + #13#10;
  end;

  procedure _Add(const aStr: string; Args: array of const); overload;
  begin
    _Add(DxbxFormat(aStr, Args));
  end;

var
  i: int;
begin
  // Show the contents to the user
  Result := #13#10'-----PixelShader Def Contents-----';
  _Add(OriginalToString());

  if (Original.PSTextureModes > 0) then
  begin
    _Add(#13#10'PSTextureModes ->'); // Texture addressing modes
    _Add('Stage 0: %s', [PS_TextureModesStr[PSTextureModes[0]]]);
    _Add('Stage 1: %s', [PS_TextureModesStr[PSTextureModes[1]]]);
    _Add('Stage 2: %s', [PS_TextureModesStr[PSTextureModes[2]]]);
    _Add('Stage 3: %s', [PS_TextureModesStr[PSTextureModes[3]]]);
  end;

  if (Original.PSDotMapping > 0) then // Input mapping for dot product modes
  begin
    _Add(#13#10'PSDotMapping ->');
    _Add('Stage 1: %s', [PS_DotMappingStr[PSDotMapping[1]]]);
    _Add('Stage 2: %s', [PS_DotMappingStr[PSDotMapping[2]]]);
    _Add('Stage 3: %s', [PS_DotMappingStr[PSDotMapping[3]]]);
  end;

  if (Original.PSCompareMode > 0) then // Compare modes for clipplane texture mode
  begin
    _Add(#13#10'PSCompareMode ->');
    _Add('Stage 0: %s', [PS_CompareModeStr[iif(PSCompareMode[0] = 0, 0, 1)]]);
    _Add('Stage 1: %s', [PS_CompareModeStr[iif(PSCompareMode[1] = 0, 2, 3)]]);
    _Add('Stage 2: %s', [PS_CompareModeStr[iif(PSCompareMode[2] = 0, 4, 5)]]);
    _Add('Stage 3: %s', [PS_CompareModeStr[iif(PSCompareMode[3] = 0, 6, 7)]]);
  end;

  if (Original.PSInputTexture > 0) then // Texture source for some texture modes
  begin
    _Add(#13#10'PSInputTexture ->');
    _Add('Stage 1: %d', [PSInputTexture[1]]);
    _Add('Stage 2: %d', [PSInputTexture[2]]);
    _Add('Stage 3: %d', [PSInputTexture[3]]);
  end;

  if (Original.PSCombinerCount > 0) then // Active combiner count (Stages 0-7)
  begin
    _Add(#13#10'PSCombinerCount ->');
    _Add('Combiners: %d', [NumberOfCombiners]);
    _Add('Mux:       %s', [PS_CombinerCountFlagsStr[iif(CombinerCountFlags and PS_COMBINERCOUNT_MUX_MSB = 0, 0, 1)]]);
    _Add('C0:        %s', [PS_CombinerCountFlagsStr[iif(CombinerCountFlags and PS_COMBINERCOUNT_UNIQUE_C0 = 0, 2, 3)]]);
    _Add('C1:        %s', [PS_CombinerCountFlagsStr[iif(CombinerCountFlags and PS_COMBINERCOUNT_UNIQUE_C1 = 0, 4, 5)]]);
  end;

  // Dxbx additions from here onwards :

  if NumberOfCombiners > 0 then
  for i := 0 to NumberOfCombiners-1 do // Loop over all combiner stages
  begin
    _Add(#13#10);

    _Add('PSRGBOutputs[%d] AB: %s', [i, Combiners[i].RGB.OutputAB.IntermediateToString()]);
    _Add('PSRGBOutputs[%d] CD: %s', [i, Combiners[i].RGB.OutputCD.IntermediateToString()]);
    _Add('PSRGBOutputs[%d] SUM: %s', [i, Combiners[i].RGB.OutputSUM.IntermediateToString()]);
    _Add('PSRGBOutputs[%d] flags: %s', [i, PSCombinerOutputFlagsToStr(Combiners[i].RGB.CombinerOutputFlags, {IsAlpha=}False)]);

    _Add(#13#10);
    _Add('PSRGBInputs[%d] A: %s', [i, Combiners[i].RGB.InputA.IntermediateToString()]);
    _Add('PSRGBInputs[%d] B: %s', [i, Combiners[i].RGB.InputB.IntermediateToString()]);
    _Add('PSRGBInputs[%d] C: %s', [i, Combiners[i].RGB.InputC.IntermediateToString()]);
    _Add('PSRGBInputs[%d] D: %s', [i, Combiners[i].RGB.InputD.IntermediateToString()]);

    _Add(#13#10);
    _Add('PSAlphaOutputs[%d] AB: %s', [i, Combiners[i].Alpha.OutputAB.IntermediateToString()]);
    _Add('PSAlphaOutputs[%d] CD: %s', [i, Combiners[i].Alpha.OutputCD.IntermediateToString()]);
    _Add('PSAlphaOutputs[%d] SUM: %s', [i, Combiners[i].Alpha.OutputSUM.IntermediateToString()]);
    _Add('PSAlphaOutputs[%d] flags: %s', [i, PSCombinerOutputFlagsToStr(Combiners[i].Alpha.CombinerOutputFlags, {IsAlpha=}True)]);

    _Add(#13#10);
    _Add('PSAlphaInputs[%d] A: %s', [i, Combiners[i].Alpha.InputA.IntermediateToString()]);
    _Add('PSAlphaInputs[%d] B: %s', [i, Combiners[i].Alpha.InputB.IntermediateToString()]);
    _Add('PSAlphaInputs[%d] C: %s', [i, Combiners[i].Alpha.InputC.IntermediateToString()]);
    _Add('PSAlphaInputs[%d] D: %s', [i, Combiners[i].Alpha.InputD.IntermediateToString()]);

    _Add(#13#10);
    _Add('PSConstant0[%d] : %x', [i, Original.PSConstant0[i]]); // C0 for each stage
    _Add('PSConstant1[%d] : %x', [i, Original.PSConstant1[i]]); // C1 for each stage
  end;

  if (Original.PSFinalCombinerInputsABCD > 0)
  or (Original.PSFinalCombinerInputsEFG  > 0) then // Final combiner inputs
  begin
    _Add(#13#10'PSFinalCombinerInputsABCD ->');
    _Add('Input A: %s', [FinalCombiner.InputA.IntermediateToString()]);
    _Add('Input B: %s', [FinalCombiner.InputB.IntermediateToString()]);
    _Add('Input C: %s', [FinalCombiner.InputC.IntermediateToString()]);
    _Add('Input D: %s', [FinalCombiner.InputD.IntermediateToString()]);

    _Add(#13#10'PSFinalCombinerInputsEFG ->');
    _Add('Input E: %s', [FinalCombiner.InputE.IntermediateToString()]);
    _Add('Input F: %s', [FinalCombiner.InputF.IntermediateToString()]);
    _Add('Input G: %s', [FinalCombiner.InputG.IntermediateToString()]);
    _Add('Final combiner setting: %s', [PSFinalCombinerSettingToStr(Ord(FinalCombiner.FinalCombinerFlags))]);
  end;

(* TODO :
  PSFinalCombinerConstant0: DWORD,         // C0 in final combiner
  PSFinalCombinerConstant1: DWORD,         // C1 in final combiner
  // These last three DWORDs are used to define how Direct3D8 pixel shader constants map to the constant
  // registers in each combiner stage. They are used by the Direct3D run-time software but not by the hardware.
  PSC0Mapping: DWORD,                      // Mapping of c0 regs to D3D constants
  PSC1Mapping: DWORD,                      // Mapping of c1 regs to D3D constants
*)
  _Add(#13#10'PSFinalCombinerConstants ->'); // // Final combiner constant mapping
  _Add('Offset of D3D constant for C0: %d', [FinalCombiner.FinalCombinerC0]);
  _Add('Offset of D3D constant for C1: %d', [FinalCombiner.FinalCombinerC1]);

  _Add('Adjust texture flag: %s', [PS_GlobalFlagsStr[PS_GLOBALFLAGS(FinalCombiner.dwPS_GLOBALFLAGS)]]);

  _Add(#13#10);
end;

function RPSIntermediate.DisassembleTextureMode(const aScope: PPSDisassembleScope; const NextIs2D: Boolean): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100

  function TStr(const t: int): string;
  begin
    Result := 't' + IntToStr(t);
  end;

begin
  Result := '';
  // TODO : Apply conversions when PS_GLOBALFLAGS_TEXMODE_ADJUST is set (but ... how to check the texture type? read D3DRS_PSTEXTUREMODES?)

  // Convert the texture mode to a texture addressing instruction :
  case PSTextureModes[aScope.Stage] of
    PS_TEXTUREMODES_PROJECT2D: Result := 'tex';
    PS_TEXTUREMODES_PROJECT3D: Result := 'tex'; // Note : 3d textures are sampled using PS_TEXTUREMODES_CUBEMAP
    PS_TEXTUREMODES_CUBEMAP: Result := 'tex'; // Note : If we use 'texreg2rgb', that requires ps.1.2 (we're still using ps.1.1)
    PS_TEXTUREMODES_PASSTHRU: Result := 'textcoord';
    PS_TEXTUREMODES_CLIPPLANE: Result := 'texkill';
    PS_TEXTUREMODES_BUMPENVMAP: Result := 'texbem';
    PS_TEXTUREMODES_BUMPENVMAP_LUM: Result := 'texbemi';
//    PS_TEXTUREMODES_BRDF: Result := 'texbrdf'; // Note : Not supported by Direct3D8 ?
    PS_TEXTUREMODES_DOT_ST: Result := 'texm3x2tex';
    PS_TEXTUREMODES_DOT_ZW: Result := 'texm3x2depth'; // Note : requires ps.1.3 and a preceding texm3x2pad
//    PS_TEXTUREMODES_DOT_RFLCT_DIFF: Result := 'texm3x3diff'; // Note : Not supported by Direct3D8 ?
    PS_TEXTUREMODES_DOT_RFLCT_SPEC: Result := 'texm3x3vspec';
    PS_TEXTUREMODES_DOT_STR_3D: Result := 'texm3x3tex'; // Note : Uses a 3d texture
    PS_TEXTUREMODES_DOT_STR_CUBE: Result := 'texm3x3tex'; // Note : Uses a cube texture
    PS_TEXTUREMODES_DPNDNT_AR: Result := 'texreg2ar';
    PS_TEXTUREMODES_DPNDNT_GB: Result := 'texreg2gb';
    PS_TEXTUREMODES_DOTPRODUCT:
      if NextIs2D then
        Result := 'texm3x2pad'
      else
        Result := 'texm3x3pad';
    PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST: Result := 'texm3x3spec'; // Note : Needs 3 arguments!
  end;

  if Result = '' then
    Exit;

  Result := Result + ' ' + TStr(aScope.Stage);

  // For those texture modes that need it, add the source stage as argument :
  if PSTextureModes[aScope.Stage] in [
    PS_TEXTUREMODES_BUMPENVMAP,
    PS_TEXTUREMODES_BUMPENVMAP_LUM,
    PS_TEXTUREMODES_DOT_ST,
    PS_TEXTUREMODES_DOT_ZW,
    PS_TEXTUREMODES_DOT_RFLCT_DIFF,
    PS_TEXTUREMODES_DOT_RFLCT_SPEC,
    PS_TEXTUREMODES_DOT_STR_3D,
    PS_TEXTUREMODES_DOT_STR_CUBE,
    PS_TEXTUREMODES_DPNDNT_AR,
    PS_TEXTUREMODES_DPNDNT_GB,
    PS_TEXTUREMODES_DOTPRODUCT,
    PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST] then
  begin
    Result := Result + ', ' + TStr(PSInputTexture[aScope.Stage]);

    case PSDotMapping[aScope.Stage] of
      PS_DOTMAPPING_MINUS1_TO_1_D3D:
        Result := Result + '_bx2';
    end;
  end;

  // Add the third argument :
  if PSTextureModes[aScope.Stage] in [
    PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST] then
    Result := Result + ', ' + aScope.C0RegStr + ' ; Dxbx guess'; // TODO : Where do we get the 3rd argument to this?

  Result := Result + #13#10;
end;

function RPSIntermediate.DisassembleTextureModes(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100

  function _NextIs2D(Stage: int): Boolean;
  begin
    if Stage < X_D3DTS_STAGECOUNT-1 then
      Result := PSTextureModes[Stage + 1] = PS_TEXTUREMODES_DOT_ZW
    else
      Result := False;
  end;

var
  Stage: int;
begin
  Result := '';
  for Stage := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    aScope.Stage := Stage;
    Result := Result + DisassembleTextureMode(aScope, _NextIs2D(Stage));
  end;
end;

function RPSIntermediate.Disassemble(): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  LogFlags: TLogFlags;
  i: Integer;
  Scope: RPSDisassembleScope;
begin
  // Azurik likes to create and destroy the same shader every frame! O_o
  LogFlags := lfUnit;
  if IsRunning(TITLEID_AZURIK) then
    LogFlags := LogFlags or lfExtreme;

  // Dump the contents of the PixelShader def
  if MayLog(LogFlags) then
    XTL_DumpPixelShaderToFile(@Original);
//    PrintPixelShaderDefContents

  if MayLog(LogFlags) then
    XTL_PrintPixelShaderDefContents(@Original);

  // First things first, set the pixel shader version
  // 1.1 allows reading from 2 textures (which we use in 'cnd') and reading from the .b (blue) channel
  // 1.3 allows the use of texm3x2depth (which can occur sometimes)
  Result := 'ps.1.3'#13#10;

  for i := 0 to 8-1 do
  begin
    // Define constants directly after the version instruction and before any other instruction :
    if Original.PSConstant0[i] > 0 then
      Result := Result + Format('def c%d, %ff, %ff, %ff, %ff'#13#10, [i,
        {R}((Original.PSConstant0[i] shr 16) and $FF) / 255.0,
        {G}((Original.PSConstant0[i] shr  8) and $FF) / 255.0,
        {B}((Original.PSConstant0[i] shr  0) and $FF) / 255.0,
        {A}((Original.PSConstant0[i] shr 24) and $FF) / 255.0
        ]);
    // TODO : What indexes are for Original.PSConstant1 ?
  end;

  // Handle Texture declarations :
  Result := Result + DisassembleTextureModes(@Scope);

  // On the Xbox, the alpha portion of the R0 register is initialized to
  // the alpha component of texture 0 if texturing is enabled for texture 0 :
  if (PSTextureModes[0] > PS_TEXTUREMODES_NONE)
  and not (PSTextureModes[0] in [PS_TEXTUREMODES_CLIPPLANE, PS_TEXTUREMODES_DOT_ZW, PS_TEXTUREMODES_DOTPRODUCT]) then
    // TODO : Move this over to where r0.a is first read (if ever)
    Result := Result + 'mov r0.a, t0.a'#13#10;

  // Loop over all combiner stages :
  if NumberOfCombiners > 0 then
  for i := 0 to NumberOfCombiners-1 do // Loop over all  combiner stages
  begin
    Scope.Stage := i;
    Result := Result + Combiners[i].Disassemble(@Scope);
  end;

  // Check if there's a final combiner
  if (Original.PSFinalCombinerInputsABCD > 0)
  or (Original.PSFinalCombinerInputsEFG > 0) then
    Result := Result + FinalCombiner.Disassemble(@Scope);

  // Note : The end result (rgba) should be in r0 (output register) now!

  if MayLog(LogFlags) then
    DbgPrintf(string(Result));
end;

{static}var PshNumber: int = 0; // Keep track of how many pixel shaders we've attempted to convert.
procedure XTL_DumpPixelShaderToFile(pPSDef: PX_D3DPIXELSHADERDEF);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  PSIntermediate: RPSIntermediate;
  szPSDef: array [0..32-1] of AnsiChar;
  out_: PFILE;
begin
  sprintf(@szPSDef[0], 'PSDef%.03d.txt', [PshNumber]); Inc(PshNumber);
  out_ := fopen(szPSDeF, 'w');
  if Assigned(out_) then
  begin
    PSIntermediate.Init(pPSDef);
    fprintf(out_, PAnsiChar(AnsiString(PSIntermediate.OriginalToString())));
    fclose(out_);
  end;
end;

function XTL_DumpPixelShaderDefToString(pPSDef: PX_D3DPIXELSHADERDEF): string;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  PSIntermediate: RPSIntermediate;
begin
  PSIntermediate.Init(pPSDef);
  Result :=  PSIntermediate.IntermediateToString();
end;

procedure XTL_PrintPixelShaderDefContents(pPSDef: PX_D3DPIXELSHADERDEF);
begin
  DbgPshPrintf(XTL_DumpPixelShaderDefToString(pPSDef));
end;

function XTL_EmuRecompilePshDef(pPSDef: PX_D3DPIXELSHADERDEF): string;
var
  PSIntermediate: RPSIntermediate;
begin
  PSIntermediate.Init(pPSDef);
  Result := PSIntermediate.Disassemble();
end;

end.

