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

{$DEFINE PS_REWRITE}

uses
  // Delphi
  Windows,
  SysUtils, // Format
  // DirectX
{$IFDEF DXBX_USE_D3D9}
  Direct3D9,
{$ELSE}
  Direct3D8, // D3DCOLOR_ARGB
{$ENDIF}
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

// PS_TEXTUREMODES(t0, t1, t2, t3) = ((t3 shl 15 or (t2 shl 10) or (t1 shl 5) or t0),


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
    PS_REGISTER_NEGATIVE_ONE_HALF= Ord({PS_REGISTER_ZERO or} PS_INPUTMAPPING_HALFBIAS_NORMAL), // invalid for final combiner

    PS_REGISTER_DXBX_PROD=         Ord({PS_REGISTER_ZERO or} PS_INPUTMAPPING_SIGNED_IDENTITY)  // Dxbx internal use
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

type PS_FINALCOMBINERSETTING = DWORD;
const
    PS_FINALCOMBINERSETTING_CLAMP_SUM=     $80; // V1+R0 sum clamped to [0,1]
    PS_FINALCOMBINERSETTING_COMPLEMENT_V1= $40; // unsigned invert mapping  (1 - v1) is used as an input to the sum rather than v1
    PS_FINALCOMBINERSETTING_COMPLEMENT_R0= $20; // unsigned invert mapping  (1 - r0) is used as an input to the sum rather than r0

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
    // $28 ?
    PS_COMBINEROUTPUT_SHIFTRIGHT_1=        $30; // y = x/2
    // $38 ?

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

{$IFDEF PS_REWRITE}

type PSH_OPCODE =
(
    PO_COMMENT,
    PO_PS,
    PO_DEF,
    PO_TEX,
    // Direct3D8 arithmetic instructions :
    PO_ADD,
    PO_CMP,
    PO_CND,
    PO_DP3,  // dp3 d, s1,s2                : d=s0 dot s1 (replicated to all channels, .rgb=color only, .a=color+alpha)
    PO_DP4,  // dp3 d, s1,s2                : d.r=d.g=d.b=d.a=(s1.r*s2.r)+(s1.g*s2.g)+(s1.b*s2.b)+(s1.a*s2.a)
    PO_LRP,
    PO_MAD,
    PO_MOV,
    PO_MUL,
    PO_NOP,
    PO_SUB,
    // Xbox1 opcodes :
    PO_XMMA,
    PO_XMMC,
    PO_XDM,
    PO_XDD,
    PO_XFC
);

var PSH_OPCODE_DEFS: array [PSH_OPCODE] of record mn: string; _Out, _In: int; note: string end = (
  // Pixel shader header opcodes (must be specified in this order) :
  ({PO_COMMENT}mn:';'; _Out:0; _In:0; note:''), //
  ({PO_PS}  mn:'ps';   _Out:0; _In:0; note:''), // Must occur once; Xbox needs an x prefix (xps), Native needs a 1.3 suffix (ps.1.3)
  ({PO_DEF} mn:'def';  _Out:1; _In:4; note:''), // Output must be a PARAM_C, arguments must be 4 floats [0.00f .. 1.00f]
  ({PO_TEX} mn:'tex';  _Out:1; _In:0; note:''), // TODO : Add all other texture opcodes here (most also have 1 input)
  // Arithmetic opcodes :
  ({PO_ADD} mn:'add';  _Out:1; _In:2; note:'d0=s0+s1'),
  ({PO_CMP} mn:'cmp';  _Out:1; _In:3; note:'d0=(s0>=0?s1:s2)'),
  ({PO_CND} mn:'cnd';  _Out:1; _In:3; note:'d1=(s0.a>0.5?s1:s2)'), // 1st input must be 'r0.a'
  ({PO_DP3} mn:'dp3';  _Out:1; _In:2; note:'d0=s0 dot3 s1'),
  ({PO_DP4} mn:'dp4';  _Out:1; _In:2; note:'d0=s0 dot4 s1'),
  ({PO_LRP} mn:'lrp';  _Out:1; _In:3; note:'d0=s0*s1+(1-s0)*s2=s0*(s1-s2)+s2'),
  ({PO_MAD} mn:'mad';  _Out:1; _In:3; note:'d0=s0*s1+s2'),
  ({PO_MOV} mn:'mov';  _Out:1; _In:1; note:'d0=s0'),
  ({PO_MUL} mn:'mul';  _Out:1; _In:2; note:'d0=s0*s1'),
  ({PO_NOP} mn:'nop';  _Out:0; _In:0; note:''),
  ({PO_SUB} mn:'sub';  _Out:1; _In:2; note:'d0=s0-s1'),
  // Xbox-only (NV2A) opcodes :
  ({PO_XMMA}mn:'xmma'; _Out:3; _In:4; note:'d0=s0*s1, d1=s2*s3, d2=(s0*s1)+(s2*s3)'),
  ({PO_XMMC}mn:'xmmc'; _Out:3; _In:4; note:'d0=s0*s1, d1=s2*s3, d2=(r0.a>0.5)?(s0*s1):(s2*s3)'),
  ({PO_XDM} mn:'xdm';  _Out:2; _In:4; note:'d0=s0 dot s1, d1=s2*s3'),
  ({PO_XDD} mn:'xdd';  _Out:2; _In:4; note:'d0=s0 dot s1, d1=s2 dot s3'),
  ({PO_XFC} mn:'xfc';  _Out:0; _In:7; note:'r0.rgb=s0*s1+(1-s0)*s2+s3, r0.a=s6.a, prod=s4*s5, sum=r0+v1')
  );

type PSH_ARGUMENT_TYPE =
(
    PARAM_VALUE,      // Xbox only; Numberic constants used in Xbox-only opcodes
    PARAM_DISCARD,    // Xbox only;
    PARAM_FOG,        // Final combiner only; Read-only register fog register
    PARAM_V1R0_SUM,   // Final combiner only; Read-only register that contains the result of V1+R0
    PARAM_EF_PROD,    // Final combiner only; Read-only register that contains the result of final combiner parameters E * F
    PARAM_R,          // Temporary registers (unassigned except r0.a, which on NV2A is initially set to t0.a)
    PARAM_T,          // Textures
    PARAM_V,          // Vertex colors
    PARAM_C           // Constant registers, set by def opcodes or SetPixelShaderConstant
);

var PSH_ARGUMENT_TYPE_Str: array [PSH_ARGUMENT_TYPE] of string = (
//  Prefix        #  r/w   Input?  Output?  Note
    '',        // *  r     No      No       Used for numeric constants like -1, 0, 1
    'discard', // *  w     No      Yes      Only for xbox opcodes (native opcodes have single output - discards must be removed)
    'fog',     // 1  r     Yes     No       Only for final combiner parameter
    'sum',     // 1  r     Yes     No       Only for final combiner parameter
    'prod',    // 1  r     Yes     No       Only for final combiner parameter
    'r',       // 2  r/w   Yes     Yes      We fake a few extra registers and resolve them in FixupPixelShader
    't',       // 4  r/w   Yes     Yes      D3D9 cannot write to these!
    'v',       // 2  r     Yes     Yes
    'c'        // 16 r     Yes     No       Xbox has 8*c0,c1=16, while PC D3D8 has only 8, we try to reduce that in FixupPixelShader
);

const
  FakeRegNr_Sum = 2;
  FakeRegNr_Prod = 3;
  FakeRegNr_Xmm1 = 4;
  FakeRegNr_Xmm2 = 5;

type PSH_INST_MODIFIER = (
  INSMOD_NONE,
  INSMOD_BIAS, // Xbox only : TODO : Fixup occurrances!
  INSMOD_X2,
  INSMOD_BX2,  // Xbox only : TODO : Fixup occurrances!
  INSMOD_X4,
  INSMOD_D2,
  INSMOD_SAT   // Xbox doesn't support this, but has ARGMOD_SATURATE instead
);

var PSH_INST_MODIFIER_Str: array [PSH_INST_MODIFIER] of string = (
  '',
  '_bias',
  '_x2',
  '_bx2',
  '_x4',
  '_d2',
  '_sat'
);

// Four argument modifiers (applied in this order) :
// 1: Inversion (invert or negate : '1-' or '-')
// 2: Apply bias ('_bias')
// 3: Apply scale ('_x2', '_bx2', '_x4', or '_d2')
// 4: Apply clamp ('_sat')
type PSH_ARG_MODIFIER = (
  ARGMOD_IDENTITY,        // y = x

  ARGMOD_INVERT,          // y = 1-x     -> 0..1 >    1..0
  ARGMOD_NEGATE,          // y = -x      -> 0..1 >    0..-1

  ARGMOD_BIAS,            // y =  x-0.5  -> 0..1 > -0.5..0.5

  ARGMOD_SCALE_X2,        // y =  x*2    -> 0..1 >    0..2
  ARGMOD_SCALE_BX2,       // y = (x*2)-1 -> 0..1 >   -1..1
  ARGMOD_SCALE_X4,        // y =  x*4    -> 0..1 >    0..4
  ARGMOD_SCALE_D2,        // y =  x/2    -> 0..1 >    0..0.5

  ARGMOD_SATURATE,        // Xbox - not available in PS1.3 (can be done on output instead)

  ARGMOD_ALPHA_REPLICATE,
  ARGMOD_BLUE_REPLICATE   // PS1.1-PS1.3 only allow this if destination writemask = .a
);
  PSH_ARG_MODIFIERs = set of PSH_ARG_MODIFIER;

var PSH_ARG_MODIFIER_Str: array [PSH_ARG_MODIFIER] of string = (
  '%s',

  '1-%s',
  '-%s',

  '%s_bias',

  '%s_x2',
  '%s_bx2',
  '%s_x4',
  '%s_d2',

  '%s_sat',

  '%s', // .a is added via Mask
  '%s'  // .b idem
);

const
  MASK_R = $001;
  MASK_G = $002;
  MASK_B = $004;
  MASK_A = $008;
  MASK_RGB = MASK_R or MASK_G or MASK_B;
  MASK_RGBA = MASK_R or MASK_G or MASK_B or MASK_A;

type
  TArgumentType = (atInput, atOutput, atFinalCombiner);

type PSH_IMD_ARGUMENT = object
    Type_: PSH_ARGUMENT_TYPE; // For parameters: R, T, V or C  For output : Discard, R, T or V
    Address: Int16;           // Register address
    Mask: DWORD;
    Modifiers: PSH_ARG_MODIFIERs;
    Multiplier: Float;
    procedure SetConstValue(Value: Float);
    function GetConstValue: Float;
    function ToString: string;
    function Decode(const Value: DWORD; aMask: DWORD; ArgumentType: TArgumentType): Boolean;
    procedure Invert;
  end;
  PPSH_IMD_ARGUMENT = ^PSH_IMD_ARGUMENT;

  TPSH_IMD_ARGUMENTArray = array [0..(MaxInt div SizeOf(PSH_IMD_ARGUMENT)) - 1] of PSH_IMD_ARGUMENT;
  PPSH_IMD_ARGUMENTs = ^TPSH_IMD_ARGUMENTArray;

type PSH_INTERMEDIATE_FORMAT = record
    CombinerStageNr: int;
    IsCombined: boolean;
    Opcode: PSH_OPCODE;
    CommentString: string;
    Modifier: PSH_INST_MODIFIER;
    Output: array [0..3-1] of PSH_IMD_ARGUMENT; // 3 = xmm* output count
    Parameters: array [0..7-1] of PSH_IMD_ARGUMENT; // 7 = xfc parameter count
    procedure Initialize(const aOpcode: PSH_OPCODE);
    function ToString: string;
    procedure SwapParameter(const Index1, Index2: int);
    procedure XSwapOutput();
    function MoveRemovableParametersRight(const Index1, Index2: int): Boolean;
    function XMoveDiscardOutputsRight(): Boolean;
    procedure XCopySecondOpcodeToFirst(const aOpcode: PSH_OPCODE);
    function Decode(CombinerStageNr, PSInputs, PSOutputs: DWORD; aMask: DWORD): Boolean;
    function DecodeFinalCombiner(aPSFinalCombinerInputsABCD, aPSFinalCombinerInputsEFG: DWORD): Boolean;
  end;
  PPSH_INTERMEDIATE_FORMAT = ^PSH_INTERMEDIATE_FORMAT;

type PSH_XBOX_SHADER = record
    IntermediateCount: int;
    Intermediate: array[0..X_PSH_COMBINECOUNT * 3] of PSH_INTERMEDIATE_FORMAT; // ...should be enough slots to hold all possible shaders

    FinalCombinerFlags: PS_FINALCOMBINERSETTING;
    CombinerCountFlags: DWORD; // For PS_COMBINERCOUNTFLAGS
    // Read from CombinerCountFlags :
    CombinerMuxesOnMsb: Boolean;
    CombinerHasUniqueC0: Boolean;
    CombinerHasUniqueC1: Boolean;
    function ToString: string;
    procedure Log(const PhaseStr: string);
    function NewIntermediate(): PPSH_INTERMEDIATE_FORMAT;
    procedure InsertIntermediate(pIntermediate: PPSH_INTERMEDIATE_FORMAT; Index: int);
    procedure DeleteIntermediate(Index: int);
    procedure DeleteLastIntermediate;
    procedure Decode(pPSDef: PX_D3DPIXELSHADERDEF);
    function MoveRemovableParametersRight: Boolean;
    procedure ConvertXboxShaderToNative(pPSDef: PX_D3DPIXELSHADERDEF);
    function ConvertXMMToNative_Except3RdOutput(i: int): Boolean;
    procedure ConvertXMMAToNative(i: int);
    procedure ConvertXMMCToNative(i: int);
    procedure ConvertXDMToNative(i: int);
    procedure ConvertXDDToNative(i: int);
    procedure ConvertXFCToNative(i: int);
    function CombineInstructions(): Boolean;
    function RemoveNops(): Boolean;
    function SimplifyMOV(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
    function SimplifyADD(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
    function SimplifySUB(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
    function SimplifyMUL(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
    function SimplifyLRP(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
    function FixupPixelShader(): Boolean;
    function FixInvalidSrcSwizzle(): Boolean;
    function FixMissingR0a(): Boolean;
    function FixCoIssuedOpcodes(): Boolean;
  end;
{$ENDIF PS_REWRITE}

type
  PPSDisassembleScope = ^RPSDisassembleScope;

  PPSRegisterObject = ^RPSRegisterObject;
  RPSRegisterObject = object
    IsAlpha: Boolean;
    Reg: PS_REGISTER;
    function IsNativeRegWriteable: Boolean;
    procedure Decode(Value: Byte; aIsAlpha: Boolean);
    function IsSameAsAlpha(const Alpha: PPSRegisterObject): Boolean;
    function IntermediateToString(): string;
    function DisassembleRegister(const aScope: PPSDisassembleScope): string;
  end;

  PPSInputRegister = ^RPSInputRegister;
  RPSInputRegister = object(RPSRegisterObject)
    Channel: PS_CHANNEL;
    InputMapping: PS_INPUTMAPPING;
    // Added to ease emulation :
    MulResult: int;
    Multiplier, Correction: Float;
    procedure Decode(Value: Byte; aIsAlpha: Boolean);
    function IsSameAsAlpha(const Alpha: PPSInputRegister): Boolean;
    function IntermediateToString(): string;
    function DisassembleInputRegister(const aScope: PPSDisassembleScope): string;
  end;

  PPSCombinerOutput = ^RPSCombinerOutput;
  RPSCombinerOutput = object(RPSRegisterObject)
    Input1: RPSInputRegister; // Called InputA or InputC (depending if it's inside the AB or CD combiner)
    Input2: RPSInputRegister; // Called InputC or InputD (depending if it's inside the AB or CD combiner)
    DotProduct: Boolean; // False=Multiply, True=DotProduct
    BlueToAlpha: Boolean; // False=Alpha-to-Alpha, True=Blue-to-Alpha
    // Added to ease emulation :
    MulResult: int;
    function IsSameAsAlpha(const Alpha: PPSCombinerOutput): Boolean;
    procedure Decode(Value: Byte; PSInputs: DWORD; aIsAlpha: Boolean);
    function CombineStageInputMul(const aScope: PPSDisassembleScope): string;
    function CombineStageInputDot(const aScope: PPSDisassembleScope): string;
    function DisassembleCombinerOutput(const aScope: PPSDisassembleScope): string;
  end;

  PPSCombinerOutputMuxSum = ^RPSCombinerOutputMuxSum;
  RPSCombinerOutputMuxSum = object(RPSRegisterObject)
    OutputAB: RPSCombinerOutput; // Contains InputA and InputB (as Input1 and Input2)
    OutputCD: RPSCombinerOutput; // Contains InputC and InputD (as Input1 and Input2)
    function IsSameAsAlpha(const Alpha: PPSCombinerOutputMuxSum): Boolean;

    function CombineStageInputSum(const aScope: PPSDisassembleScope): string;

    function SumTryLerp(const aScope: PPSDisassembleScope; SumOutputString: string): string;
    function SumTry4Regs(const aScope: PPSDisassembleScope; SumOutputString: string): string;
    function SumTry2Regs_1Reg1Fixed(const aScope: PPSDisassembleScope; const SumOutputString: string;
      const Output2Reg, Output1Reg: PPSCombinerOutput): string;
    function CombineStageInputMux(const aScope: PPSDisassembleScope): string;
  end;

  PPSCombinerStageChannel = ^RPSCombinerStageChannel;
  RPSCombinerStageChannel = record
    OutputSUM: RPSCombinerOutputMuxSum; // Contains OutputAB, OutputCD
    CombinerOutputFlags: PS_COMBINEROUTPUT;
    AB_CD_SUM: Boolean; // True=AB+CD, False=MUX(AB;CD) based on R0.a
    procedure Decode(PSInputs, PSOutputs: DWORD; IsAlpha: Boolean = False);
    function IsSameAsAlpha(const Alpha: PPSCombinerStageChannel): Boolean;
    function DisassembleCombinerStageChannel(const aScope: PPSDisassembleScope): string;
  end;

  RPSCombinerStage = record
    RGB: RPSCombinerStageChannel;
    Alpha: RPSCombinerStageChannel;

    C0Mapping: DWORD; // C0 for each stage
    C1Mapping: DWORD; // C1 for each stage

    function DisassembleCombinerStage(const aScope: PPSDisassembleScope): string;
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

    FinalCombinerC0Mapping: Byte;
    FinalCombinerC1Mapping: Byte;

    dwPS_GLOBALFLAGS: DWORD;
    procedure Decode(const PSFinalCombinerInputsABCD, PSFinalCombinerInputsEFG, PSFinalCombinerConstants: DWORD);
    function DisassembleFinalCombiner(const aScope: PPSDisassembleScope): string;
  end;

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
    function EmitConstant(const OutputStr: string; const MulResult: int): string;
    function EmitMov(const OutputStr: string; const Input: PPSRegisterObject): string;
    function EmitAddOutputs(const OutputStr: string; const Input1, Input2: PPSRegisterObject): string;
    function EmitAdd(const OutputStr: string; const Input1, Input2: PPSInputRegister): string;
    function EmitSub(const OutputStr: string; const Input1, Input2: PPSInputRegister): string;
    function EmitMul(const DestRegister: PPSRegisterObject; const Input1, Input2: PPSInputRegister): string;
    function EmitMad(const OutputStr: string; const Input1, Input2: PPSInputRegister; const Input3: PPSRegisterObject): string;
    function EmitLrp(const OutputStr: string; const Input1, Input2, Input3: PPSInputRegister): string;
  end;

  RPSIntermediate = record
    Original: X_D3DPIXELSHADERDEF;

    PSTextureModes: array[0..X_D3DTS_STAGECOUNT-1] of PS_TEXTUREMODES;
    PSDotMapping: array[0..X_D3DTS_STAGECOUNT-1] of PS_DOTMAPPING;
    PSCompareMode: array[0..X_D3DTS_STAGECOUNT-1] of DWORD;
    PSInputTexture: array[0..X_D3DTS_STAGECOUNT-1] of int;

    NumberOfCombiners: DWORD;
    CombinerCountFlags: DWORD; // For PS_COMBINERCOUNTFLAGS

    CombinerMuxesOnMsb: Boolean; // Read from CombinerCountFlags
    CombinerHasUniqueC0: Boolean;
    CombinerHasUniqueC1: Boolean;

    Combiners: array [0..X_PSH_COMBINECOUNT-1] of RPSCombinerStage;

    FinalCombiner: RPSFinalCombiner;

    procedure Init(pPSDef: PX_D3DPIXELSHADERDEF);
  private
    procedure Decode(pPSDef: PX_D3DPIXELSHADERDEF);
    function DisassembleTextureMode(const aScope: PPSDisassembleScope; const NextIs2D: Boolean): string;
    function DisassembleTextureModes(const aScope: PPSDisassembleScope): string;

    function OriginalToString(): string;
    function IntermediateToString(): string;
    function DisassembleIntermediate(): string;
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

{$IFDEF PS_REWRITE}

const
  XFC_COMBINERSTAGENR = X_PSH_COMBINECOUNT; // Always call XFC 'stage 9', 1 after

  PSH_XBOX_MAX_C_REGISTER_COUNT = 16;
  PSH_XBOX_MAX_R_REGISTER_COUNT = 2;
  PSH_XBOX_MAX_T_REGISTER_COUNT = 4;
  PSH_XBOX_MAX_V_REGISTER_COUNT = 2;

  // Two extra constants are possible in the final combiner - give them fake numbers :
  X_PSH_CONSTANT_FC0 = PSH_XBOX_MAX_C_REGISTER_COUNT + 1;
  X_PSH_CONSTANT_FC1 = X_PSH_CONSTANT_FC0 + 1;

const
  CONST_NEG_TWO = 4;
  CONST_NEG_ONE = -2;
  CONST_NEG_HALF = -1;
  CONST_ZERO = 0;
  CONST_POS_HALF = 1;
  CONST_POS_ONE = 2;
  CONST_POS_TWO = 4;

{ PSH_IMD_ARGUMENT }

procedure PSH_IMD_ARGUMENT.SetConstValue(Value: Float);
begin
  Type_ := PARAM_VALUE;
  Address := CONST_ZERO;
  Multiplier := Value;
  Modifiers := [];
end;

function PSH_IMD_ARGUMENT.GetConstValue: Float;
begin
  if Type_ <> PARAM_VALUE then
  begin
    // Anything other than a const returns a value never checked for :
    Result := 100000;
    Exit;
  end;

  Result := Multiplier;

  // y = 1-x     -> 0..1 >    1..0
  if (Modifiers * [ARGMOD_INVERT]) <> [] then Result := 1.0-Result;

  // y = -x      -> 0..1 >    0..-1
  if (Modifiers * [ARGMOD_NEGATE]) <> [] then Result := -Result;

  // y =  x-0.5  -> 0..1 > -0.5..0.5
  if (Modifiers * [ARGMOD_BIAS]) <> [] then Result := Result-0.5;

  // y =  x*2    -> 0..1 >    0..2
  if (Modifiers * [ARGMOD_SCALE_X2]) <> [] then Result := Result*2.0;

  // y = (x*2)-1 -> 0..1 >   -1..1
  if (Modifiers * [ARGMOD_SCALE_BX2]) <> [] then Result := (Result*2.0)-1.0;

  // y =  x*4    -> 0..1 >    0..4
  if (Modifiers * [ARGMOD_SCALE_X4]) <> [] then Result := Result*4.0;

  // y =  x/2    -> 0..1 >    0..0.5
  if (Modifiers * [ARGMOD_SCALE_D2]) <> [] then Result := Result/2.0;
end; // GetConstValue

function PSH_IMD_ARGUMENT.ToString: string;
var
  Modifier: PSH_ARG_MODIFIER;
begin
  if Type_ = PARAM_VALUE then
  begin
    Result := Format('%.0f', [GetConstValue]);
    if Pos(DecimalSeparator, Result) > 0 then
      Result := Result + 'f';

    Exit;
  end;

  Result := PSH_ARGUMENT_TYPE_Str[Type_];

  if Type_ >= PARAM_R then
    Result := Result + IntToStr(Address);

  if Type_ > PARAM_DISCARD then
  begin
    for Modifier := Low(PSH_ARG_MODIFIER) to High(PSH_ARG_MODIFIER) do
      if Modifier in Modifiers then
        Result := Format(PSH_ARG_MODIFIER_Str[Modifier], [Result]);

    if (Mask > 0) and (Mask <> MASK_RGBA) then
    begin
      Result := Result + '.';
      if (Mask and MASK_R) > 0 then Result := Result + 'r';
      if (Mask and MASK_G) > 0 then Result := Result + 'g';
      if (Mask and MASK_B) > 0 then Result := Result + 'b';
      if (Mask and MASK_A) > 0 then Result := Result + 'a';
    end;
  end;
end; // ToString

function PSH_IMD_ARGUMENT.Decode(const Value: DWORD; aMask: DWORD; ArgumentType: TArgumentType): Boolean;
var
  Reg: PS_REGISTER;
  InputMapping: PS_INPUTMAPPING;
  Channel: PS_CHANNEL;
begin
  Result := True;
  Address := 0;
  Mask := aMask; // TODO : UNBELIEVABLE! If this line is enabled, Rayman crashes on it's 24th pixelshader!?!
  Modifiers := [ARGMOD_IDENTITY];
  Multiplier := 1.0;

  // Determine PS_REGISTER for this argument type :
  begin
    Reg := PS_REGISTER(Value and $F);
    if ArgumentType = atOutput then
    begin
      // Output arguments may not write to C0 or C1, prevent that :
      if (Reg = PS_REGISTER_C0) or (Reg = PS_REGISTER_C1) then
        Reg := PS_REGISTER_DXBX_PROD; // unhandled case - will reach "invalid" else-block
    end
    else
    begin
      // Input arguments (normal or final combiners) can use the extended PS_REGISTER values :
      if Reg = PS_REGISTER_ZERO then
        Reg := PS_REGISTER(Value and $E0);

      // 'Signed Identity' flag on PS_REGISTER_ZERO has no meaning, treat as zero :
      if Reg = PS_REGISTER_DXBX_PROD then
        Reg := PS_REGISTER_ZERO;

      // Prevent decoding final combiner registers outside that mode :
      if (ArgumentType <> atFinalCombiner) then
        if (Reg = PS_REGISTER_FOG) or (Reg = PS_REGISTER_V1R0_SUM) or (Reg = PS_REGISTER_EF_PROD) then
          Reg := PS_REGISTER_DXBX_PROD; // unhandled case - will reach "invalid" else-block
    end;
  end;

  case Reg of
    PS_REGISTER_ZERO:
    begin
      if ArgumentType = atOutput then
      begin
        // Mark output arguments as 'discard' and return that fact :
        Type_ := PARAM_DISCARD;
        Result := False;
      end
      else
        Type_ := PARAM_VALUE;

      Address := CONST_ZERO;
      Multiplier := 0.0;
    end;
    PS_REGISTER_C0:
      Type_ := PARAM_C;
    PS_REGISTER_C1:
    begin
      Type_ := PARAM_C;
      Address := 1;
    end;
    PS_REGISTER_V0:
      Type_ := PARAM_V;
    PS_REGISTER_V1:
    begin
      Type_ := PARAM_V;
      Address := 1;
    end;
    PS_REGISTER_T0:
      Type_ := PARAM_T;
    PS_REGISTER_T1:
    begin
      Type_ := PARAM_T;
      Address := 1;
    end;
    PS_REGISTER_T2:
    begin
      Type_ := PARAM_T;
      Address := 2;
    end;
    PS_REGISTER_T3:
    begin
      Type_ := PARAM_T;
      Address := 3;
    end;
    PS_REGISTER_R0:
      Type_ := PARAM_R;
    PS_REGISTER_R1:
    begin
      Type_ := PARAM_R;
      Address := 1;
    end;
    // Registers only available when ArgumentType <> atOutput (Reg is capped otherwise) :
    PS_REGISTER_ONE:
    begin
      Type_ := PARAM_VALUE;
      Address := CONST_POS_ONE;
      Multiplier := 1.0;
    end;
    PS_REGISTER_NEGATIVE_ONE:
    begin
      Type_ := PARAM_VALUE;
      Address := CONST_NEG_ONE;
      Multiplier := -1.0;
    end;
    PS_REGISTER_ONE_HALF:
    begin
      Type_ := PARAM_VALUE;
      Address := CONST_POS_HALF;
      Multiplier := 0.5;
    end;
    PS_REGISTER_NEGATIVE_ONE_HALF:
    begin
      Type_ := PARAM_VALUE;
      Address := CONST_NEG_HALF;
      Multiplier := -0.5;
    end;
    // Registers only available when ArgumentType = atFinalCombiner (Reg is capped otherwise) :
    PS_REGISTER_FOG:
      Type_ := PARAM_FOG;
    PS_REGISTER_V1R0_SUM:
      Type_ := PARAM_V1R0_SUM;
    PS_REGISTER_EF_PROD:
      Type_ := PARAM_EF_PROD;
  else
    DbgPrintf('INVALID ARGUMENT!');

    Result := False;
  end;

  if ArgumentType = atOutput then
  begin
//    Mask := aMask;
    Exit;
  end;

  if Type_ > PARAM_VALUE then
  begin
    Channel := PS_CHANNEL(Value and Ord(PS_CHANNEL_ALPHA));
    if Channel = PS_CHANNEL_ALPHA then
      Mask := MASK_A
    else // PS_CHANNEL_BLUE (only valid for Alpha step) :
      // ARGMOD_BLUE_REPLICATE ?
      if aMask = MASK_A then
        Mask := MASK_B;

    InputMapping := PS_INPUTMAPPING(Value and $e0);

  //    ARGMOD_BIAS,
  //
  //    ARGMOD_SCALE_X2, ARGMOD_SCALE_BX2, ARGMOD_SCALE_X4, ARGMOD_SCALE_D2,
  //
  //    ARGMOD_SATURATE,
  //
  //    ARGMOD_ALPHA_REPLICATE, ARGMOD_BLUE_REPLICATE];

    case InputMapping of
      PS_INPUTMAPPING_UNSIGNED_IDENTITY:
        Modifiers := [ARGMOD_IDENTITY];
      PS_INPUTMAPPING_UNSIGNED_INVERT:
        Modifiers := [ARGMOD_INVERT];
      PS_INPUTMAPPING_EXPAND_NORMAL:
      begin
        Modifiers := [ARGMOD_SCALE_BX2];
        Multiplier := 2.0 * Multiplier;
      end;
      PS_INPUTMAPPING_EXPAND_NEGATE:
      begin
        Modifiers := [ARGMOD_NEGATE];
        Multiplier := -Multiplier;
      end;
      PS_INPUTMAPPING_HALFBIAS_NORMAL:
        Modifiers := [ARGMOD_BIAS];
  //    PS_INPUTMAPPING_HALFBIAS_NEGATE:
  //      Modifiers := [ARGMOD_IDENTITY]; ???
      PS_INPUTMAPPING_SIGNED_IDENTITY:
        Modifiers := [ARGMOD_IDENTITY];
      PS_INPUTMAPPING_SIGNED_NEGATE:
      begin
        Modifiers := [ARGMOD_NEGATE];
        Multiplier := -Multiplier;
      end;
    end;
  end;
end; // Decode

procedure PSH_IMD_ARGUMENT.Invert;
begin
  if (Modifiers * [ARGMOD_INVERT]) = [] then
    Modifiers := Modifiers + [ARGMOD_INVERT]
  else
    Modifiers := Modifiers - [ARGMOD_INVERT];
end;

{ PSH_INTERMEDIATE_FORMAT }

procedure PSH_INTERMEDIATE_FORMAT.Initialize(const aOpcode: PSH_OPCODE);
var
  i: int;
begin
  ZeroMemory(@Self, sizeof(Self));
  Opcode := aOpcode;
  for i := Low(Output) to High(Output) do
    Output[i].Multiplier := 1.0;
  for i := Low(Parameters) to High(Parameters) do
    Parameters[i].Multiplier := 1.0;
end;

function PSH_INTERMEDIATE_FORMAT.ToString: string;
var
  i: int;
  SeparatorChar: Char;
begin
  if Opcode = PO_COMMENT then
  begin
    Result := '; ' + CommentString;
    Exit;
  end;

  if IsCombined then
    Result := '+'
  else
    Result := '';

  Result := Result + PSH_OPCODE_DEFS[Opcode].mn + PSH_INST_MODIFIER_Str[Modifier];

  // Output a comma-separated list of output registers :
  SeparatorChar := ' ';
  for i := 0 to PSH_OPCODE_DEFS[Opcode]._Out - 1 do
  begin
    Result := Result + SeparatorChar + Self.Output[i].ToString;
    SeparatorChar := ',';
  end;

  // If this opcode has both output and input, put a space between them :
  if (PSH_OPCODE_DEFS[Opcode]._Out > 0) and (PSH_OPCODE_DEFS[Opcode]._In > 0) then
  begin
    Result := Result + ',';
    SeparatorChar := ' ';
  end;

  // Output a comma-separated list of parameters :
  for i := 0 to PSH_OPCODE_DEFS[Opcode]._In - 1 do
  begin
    Result := Result + SeparatorChar + Parameters[i].ToString;
    SeparatorChar := ',';
  end;

  if (CommentString <> '')
  or (PSH_OPCODE_DEFS[Opcode].note <> '') then
    Result := Result + ' ; ' + PSH_OPCODE_DEFS[Opcode].note + ' ' + CommentString;
end; // ToString

procedure PSH_INTERMEDIATE_FORMAT.SwapParameter(const Index1, Index2: int);
// Swaps two parameters.
var
  TmpParameters: PSH_IMD_ARGUMENT;
begin
  TmpParameters := Parameters[Index1];
  Parameters[Index1] := Parameters[Index2];
  Parameters[Index2] := TmpParameters;
end;

procedure PSH_INTERMEDIATE_FORMAT.XSwapOutput();
// Swaps the two outputs, along with their arguments. Applies only to Xbox opcodes.
var
  TmpOutput: PSH_IMD_ARGUMENT;
begin
  // Swap output 0 with 1 :
  TmpOutput := Output[0];
  Output[0] := Output[1];
  Output[1] := TmpOutput;

  // Swap parameters 0 with 2 and and 1 with 3 :
  SwapParameter(0, 2);
  SwapParameter(1, 3);
end;

function PSH_INTERMEDIATE_FORMAT.MoveRemovableParametersRight(const Index1, Index2: int): Boolean;
// Swaps discarded (and const) parameters to the right position, to ease later conversions.
begin
  Result := False;

  if  (Parameters[Index1].Type_ <= PARAM_DISCARD)
  and (Parameters[Index2].Type_ > PARAM_DISCARD) then
  begin
    SwapParameter(Index1, Index2);
    Result := True;
  end;
end;

function PSH_INTERMEDIATE_FORMAT.XMoveDiscardOutputsRight(): Boolean;
// Swap discards and constants to the right position, to ease later conversions. Applies only to Xbox opcodes.
begin
  Result := False;

  // First, check if the left output is discarded, while the second isn't :
  if  (Output[0].Type_ <= PARAM_DISCARD)
  and (Output[1].Type_ > PARAM_DISCARD) then
  begin
    // Swap the outputs, so the discarded version is positioned rightmost :
    XSwapOutput();
    Result := True;
  end;

  // Also try to swap the parameters to the first operation :
  if MoveRemovableParametersRight(0, 1) then
    Result := True;

  // Idem for the parameters to second operation :
  if MoveRemovableParametersRight(2, 3) then
    Result := True;
end;

procedure PSH_INTERMEDIATE_FORMAT.XCopySecondOpcodeToFirst(const aOpcode: PSH_OPCODE);
// Copies second opcode to first position, changing the opcode type on the fly.
begin
  Opcode := aOpcode;
  Output[0] := Output[1];
  Parameters[0] := Parameters[2];
  Parameters[1] := Parameters[3];
end;

function PSH_INTERMEDIATE_FORMAT.Decode(CombinerStageNr, PSInputs, PSOutputs: DWORD; aMask: DWORD): Boolean;
var
  CombinerOutputFlags: DWORD;
  i: int;
begin
  Result := False;
  Self.CombinerStageNr := CombinerStageNr;
  Self.IsCombined := aMask = MASK_A;

  // Decode first two outputs :
  if Output[0].Decode((PSOutputs shr 4) and $F, aMask, atOutput) then
    Result := True;
  if Output[1].Decode((PSOutputs shr 0) and $F, aMask, atOutput) then
    Result := True;

  // Get the combiner output flags :
  CombinerOutputFlags := PS_COMBINEROUTPUT(PSOutputs shr 12);

  // Use that to choose between the four possible operations :
  // - xdd (dot/dot/discard) > calculating AB=A.B and CD=C.D
  // - xdm (dot/mul/discard) > calculating AB=A.B and CD=C*D
  // - xmmc (mul/mul/mux)    > calculating AB=A*B and CD=C*D and Mux=AB?CD
  // - xmma (mul/mul/sum)    > calculating AB=A*B and CD=C*D and Sum=AB+CD
  if (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_DOT_PRODUCT) > 0 then // False=Multiply, True=DotProduct
  begin
    if (CombinerOutputFlags and PS_COMBINEROUTPUT_CD_DOT_PRODUCT) > 0 then // False=Multiply, True=DotProduct
      Self.Opcode := PO_XDD
    else
      Self.Opcode := PO_XDM;

    // Note : All arguments are already in-place for these two opcodes.

    // No 3rd output; Assert that (PSOutputs shr 8) and $F = PS_REGISTER_DISCARD ?
  end
  else
  if (CombinerOutputFlags and PS_COMBINEROUTPUT_CD_DOT_PRODUCT) > 0 then // False=Multiply, True=DotProduct
  begin
    // The first operation is a multiply, but the second is a dot-product;
    // There's no opcode for that, but we can reverse the two and still use XDM :
    Self.Opcode := PO_XDM;
    XSwapOutput();

    // No 3rd output; Assert that (PSOutputs shr 8) and $F = PS_REGISTER_DISCARD ?
  end
  else
  begin
    if {AB_CD_SUM=}(CombinerOutputFlags and PS_COMBINEROUTPUT_AB_CD_MUX) = 0 then // True=AB+CD, False=MUX(AB,CD) based on R0.a
      Self.Opcode := PO_XMMA
    else
      Self.Opcode := PO_XMMC;

    // This has a 3rd output, set that already :
    if Output[2].Decode((PSOutputs shr 8) and $F, aMask, atOutput) then
      Result := True;
  end;

  if Result then
  begin
    case PS_COMBINEROUTPUT(Ord(CombinerOutputFlags) and $38) of
      PS_COMBINEROUTPUT_BIAS:             Self.Modifier := INSMOD_BIAS;
      PS_COMBINEROUTPUT_SHIFTLEFT_1:      Self.Modifier := INSMOD_X2;
      PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS: Self.Modifier := INSMOD_BX2;
      PS_COMBINEROUTPUT_SHIFTLEFT_2:      Self.Modifier := INSMOD_X4;
      PS_COMBINEROUTPUT_SHIFTRIGHT_1:     Self.Modifier := INSMOD_D2;
    else {PS_COMBINEROUTPUT_IDENTITY:}    Self.Modifier := INSMOD_NONE;
    end;

    if (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA) > 0 then // False=Alpha-to-Alpha, True=Blue-to-Alpha
    begin
      Output[0].Modifiers := Output[0].Modifiers + [ARGMOD_BLUE_REPLICATE];
      Output[0].Mask := MASK_B;
    end;

    if (CombinerOutputFlags and PS_COMBINEROUTPUT_CD_BLUE_TO_ALPHA) > 0 then // False=Alpha-to-Alpha, True=Blue-to-Alpha
    begin
      Output[1].Modifiers := Output[1].Modifiers + [ARGMOD_BLUE_REPLICATE];
      Output[1].Mask := MASK_B;
    end;

    // Decode all four inputs :
    for i := 0 to PSH_OPCODE_DEFS[Self.Opcode]._In - 1 do
      Self.Parameters[i].Decode((PSInputs shr ((3-i) * 8)) and $FF, aMask, atInput);
  end;
end; // Decode

function PSH_INTERMEDIATE_FORMAT.DecodeFinalCombiner(aPSFinalCombinerInputsABCD, aPSFinalCombinerInputsEFG: DWORD): Boolean;
var
  i: int;
begin
  Self.Opcode := PO_XFC;
  Self.CombinerStageNr := XFC_COMBINERSTAGENR;

  // Decode A,B,C and D :
  for i := 0 to 4 - 1 do
    Self.Parameters[i].Decode((aPSFinalCombinerInputsABCD shr ((3-i) * 8)) and $FF, MASK_RGB{?}, atFinalCombiner);

  // Decode E,F and G :
  for i := 0 to 3 - 1 do
    Self.Parameters[4+i].Decode((aPSFinalCombinerInputsEFG shr ((3-i) * 8)) and $FF, MASK_RGB{?}, atFinalCombiner);

  Result := True;
end;

{ PSH_XBOX_SHADER }

function PSH_XBOX_SHADER.ToString: string;
var
  i: int;
begin
  Result := 'ps.1.3'#13#10;
//
Result := Result + 'tex t0'#13#10;
Result := Result + 'tex t1'#13#10;
Result := Result + 'tex t2'#13#10;
//
  for i := 0 to IntermediateCount-1 do
    Result := Result + Intermediate[i].ToString + #13#10;
end;

procedure PSH_XBOX_SHADER.Log(const PhaseStr: string);
begin
  if MayLog(lfUnit) then
  begin
    DbgPrintf('New decoding - %s :', [PhaseStr]);
    DbgPrintf(ToString);
  end;
end;

function PSH_XBOX_SHADER.NewIntermediate(): PPSH_INTERMEDIATE_FORMAT;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := @Intermediate[IntermediateCount];
  Result.Initialize(PO_COMMENT);
  Inc(IntermediateCount);
end;

procedure PSH_XBOX_SHADER.InsertIntermediate(pIntermediate: PPSH_INTERMEDIATE_FORMAT; Index: int);
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  i: int;
begin
  i := IntermediateCount - 1;
  while i >= Index do
  begin
    Intermediate[i + 1] := Intermediate[i];
    Dec(i);
  end;

  Intermediate[Index] := pIntermediate^;
  Inc(IntermediateCount);
end;

procedure PSH_XBOX_SHADER.DeleteIntermediate(Index: int);
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  i: int;
begin
  for i := Index to IntermediateCount - 2 do
    Intermediate[i] := Intermediate[i + 1];

  Dec(IntermediateCount);
end;

procedure PSH_XBOX_SHADER.DeleteLastIntermediate;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if IntermediateCount > 0 then
    DeleteIntermediate(IntermediateCount - 1);
end;

procedure PSH_XBOX_SHADER.Decode(pPSDef: PX_D3DPIXELSHADERDEF);
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  i: int;
  NumberOfCombiners: int;
begin
  ZeroMemory(@Self, SizeOf(Self));

//  for i := 0 to X_D3DTS_STAGECOUNT-1 do
//  begin
//    PSTextureModes[i] := PS_TEXTUREMODES((pPSDef.PSTextureModes shr (i*5)) and $1F);
//    PSCompareMode[i] := (pPSDef.PSCompareMode shr (i*4)) and $F;
//  end;
//
//  PSDotMapping[0] := PS_DOTMAPPING(0);
//  PSDotMapping[1] := PS_DOTMAPPING((pPSDef.PSDotMapping shr 0) and $7);
//  PSDotMapping[2] := PS_DOTMAPPING((pPSDef.PSDotMapping shr 4) and $7);
//  PSDotMapping[3] := PS_DOTMAPPING((pPSDef.PSDotMapping shr 8) and $7);
//
//  PSInputTexture[0] := -1; // Stage 0 has no predecessors
//  PSInputTexture[1] := 0; // Stage 1 can only use stage 0
//  PSInputTexture[2] := (pPSDef.PSInputTexture shr 16) and $1; // Stage 2 can use stage 0 or 1
//  PSInputTexture[3] := (pPSDef.PSInputTexture shr 20) and $3; // Stage 3 can only use stage 0, 1 or 2
//  TODO : Insert DisassembleTextureModes / DisassembleTextureMode

  NumberOfCombiners := (pPSDef.PSCombinerCount shr 0) and $F;
  CombinerCountFlags := (pPSDef.PSCombinerCount shr 8);

  CombinerMuxesOnMsb := (CombinerCountFlags and PS_COMBINERCOUNT_MUX_MSB) > 0;
  CombinerHasUniqueC0 := (CombinerCountFlags and PS_COMBINERCOUNT_UNIQUE_C0) > 0;
  CombinerHasUniqueC1 := (CombinerCountFlags and PS_COMBINERCOUNT_UNIQUE_C1) > 0;

  for i := 0 to NumberOfCombiners - 1 do
  begin
    if  ((pPSDef.PSRGBInputs[i] and PS_NoChannelsMask) = (pPSDef.PSAlphaInputs[i] and PS_NoChannelsMask))
    and (pPSDef.PSRGBOutputs[i] = pPSDef.PSAlphaOutputs[i]) then
    begin
      if not NewIntermediate.Decode(i, pPSDef.PSRGBInputs[i], pPSDef.PSRGBOutputs[i], MASK_RGBA) then
        DeleteLastIntermediate;
    end
    else
    begin
      if not NewIntermediate.Decode(i, pPSDef.PSRGBInputs[i], pPSDef.PSRGBOutputs[i], MASK_RGB) then
        DeleteLastIntermediate;

      if not NewIntermediate.Decode(i, pPSDef.PSAlphaInputs[i], pPSDef.PSAlphaOutputs[i], MASK_A) then
        DeleteLastIntermediate;
    end;
  end;

  if (pPSDef.PSFinalCombinerInputsABCD > 0)
  or (pPSDef.PSFinalCombinerInputsEFG > 0) then
    if NewIntermediate.DecodeFinalCombiner(pPSDef.PSFinalCombinerInputsABCD, pPSDef.PSFinalCombinerInputsEFG) then
    begin
      FinalCombinerFlags := PS_FINALCOMBINERSETTING((pPSDef.PSFinalCombinerInputsEFG shr 0) and $FF);
//    FinalCombinerC0Mapping := (pPSDef.PSFinalCombinerConstants shr 0) and $F;
//    FinalCombinerC1Mapping := (pPSDef.PSFinalCombinerConstants shr 4) and $F;
//    dwPS_GLOBALFLAGS := (pPSDef.PSFinalCombinerConstants shr 8) and $1;
    end
    else
      DeleteLastIntermediate;

  // TODO:
  // - Insert tex* and def instructions

  Log('Parse result');

  if MoveRemovableParametersRight then
    Log('MoveRemovableParametersRight');

  if RemoveNops() then
    Log('RemoveNops');

  ConvertXboxShaderToNative(pPSDef);
  Log('ConvertXboxShaderToNative');

  // Resolve all differences :
  if FixupPixelShader then
    Log('FixupPixelShader');

  if FixInvalidSrcSwizzle then
    Log('FixInvalidSrcSwizzle');

  if FixMissingR0a then
    Log('FixMissingR0a');

  if FixCoIssuedOpcodes then
    Log('FixCoIssuedOpcodes');

  Log('End result');
end;

function PSH_XBOX_SHADER.MoveRemovableParametersRight: Boolean;
var
  i: int;
begin
  Result := False;

  // For all opcodes, try to put constant and discarded arguments in the rightmost slot, to ease following analysis :
  i := IntermediateCount;
  while i > 0 do
  begin
    Dec(i);

    case Intermediate[i].Opcode of
//      PO_SUB // 1-x is not the same as x-1, but can still be reduced - see SimplifySUB
      PO_ADD,
      PO_DP3,
      PO_DP4,
      PO_MUL: // All these opcodes have two swappable parameters, so try that :
        if Intermediate[i].MoveRemovableParametersRight(0, 1) then
          Result := True;

      PO_XMMA,
      PO_XMMC,
      PO_XDD:
        if Intermediate[i].XMoveDiscardOutputsRight() then
          Result := True;

      PO_XDM:
      begin
        // Parameters may be swapped for both dot and mul,
        // but the opcodes themselves may not, as we handle
        // both XDM operations separately below :
        if Intermediate[i].MoveRemovableParametersRight(0, 1) then
          Result := True;

        if Intermediate[i].MoveRemovableParametersRight(2, 3) then
          Result := True;
      end;
    end;
  end;
end;

procedure PSH_XBOX_SHADER.ConvertXboxShaderToNative(pPSDef: PX_D3DPIXELSHADERDEF);

  function ByteToFloat(const aByte: Byte): Float;
  begin
  //  Result := (aByte - 127.0) / 128.0
    Result := aByte / 255.0
  end;

  procedure _SetColor(var NewIns: PSH_INTERMEDIATE_FORMAT; ConstColor: D3DCOLOR);
  begin
    // Colors are defined in RGBA format, and range 0.0 - 1.0 (negative values
    // can be obtained by supplying PS_INPUTMAPPING_SIGNED_NEGATE to the combiner
    // that reads from these constants).
    NewIns.Parameters[0].SetConstValue({R}ByteToFloat((ConstColor shr 16) and $FF));
    NewIns.Parameters[1].SetConstValue({G}ByteToFloat((ConstColor shr  8) and $FF));
    NewIns.Parameters[2].SetConstValue({B}ByteToFloat((ConstColor shr  0) and $FF));
    NewIns.Parameters[3].SetConstValue({A}ByteToFloat((ConstColor shr 24) and $FF));
  end;

const
  MAX_ADDRESS = 8;
var
  i, j: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
  NewIns: PSH_INTERMEDIATE_FORMAT;
  CurArg: PPSH_IMD_ARGUMENT;
  ConstColor: D3DCOLOR;
  RegUsage: array [PSH_ARGUMENT_TYPE, 0..MAX_ADDRESS] of DWORD;
  IsConstDeclared: array [0..X_PSH_CONSTANT_FC1-1] of Boolean;
begin
  // Mark only R0 (and discard) as initially 'read', as these may not result in a removal :
  ZeroMemory(@RegUsage, SizeOf(RegUsage));
  RegUsage[PARAM_R, 0] := MASK_RGBA;
  for i := 0 to MAX_ADDRESS do
    RegUsage[PARAM_DISCARD, i] := MASK_RGBA;

  // Keep track of which constants are already 'DEF'ined :
  ZeroMemory(@IsConstDeclared, SizeOf(IsConstDeclared));

  // Do a bottom-to-top pass, converting all xbox opcodes into a native set of opcodes :
  i := IntermediateCount;
  while i > 0 do
  begin
    Dec(i);
    Cur := @(Intermediate[i]);

    // Prepare constant DEFinitions :
    NewIns.Initialize(PO_DEF);

    // Loop over the input arguments :
    for j := 0 to PSH_OPCODE_DEFS[Cur.Opcode]._In - 1 do
    begin
      CurArg := @(Cur.Parameters[j]);

      // Remove useless flag, to ease up later comparisions :
      CurArg.Modifiers := CurArg.Modifiers - [ARGMOD_IDENTITY];

      // Meanwhile, fix "Invalid src swizzle" :
      if CurArg.Mask = MASK_RGB then
        CurArg.Mask := MASK_RGBA;

      // Keep track of all register reads, so that we can discard useless writes :
      if CurArg.Address < MAX_ADDRESS then
        RegUsage[CurArg.Type_, CurArg.Address] := RegUsage[CurArg.Type_, CurArg.Address] or CurArg.Mask;

      // Handle C0 and C1 (as we need to map these two to actual constants) :
      if CurArg.Type_ = PARAM_C then
      begin
        if (CurArg.Address = 0) then
        begin
          if Cur.CombinerStageNr = XFC_COMBINERSTAGENR then
          begin
            // TODO : Is the final combiner C0 als influenced by CombinerHasUniqueC0 ?
            CurArg.Address := X_PSH_CONSTANT_FC0;
            ConstColor := pPSDef.PSFinalCombinerConstant0;
          end
          else
          begin
            // See if C0 has a unique index per combiner stage :
            if CombinerHasUniqueC0 then
              // C0 actually ranges from c0 to c7, one for each possible combiner stage :
              // X_D3DRS_PSCONSTANT0_0..X_D3DRS_PSCONSTANT0_7
              Inc(CurArg.Address, Cur.CombinerStageNr);

            ConstColor := pPSDef.PSConstant0[CurArg.Address];
          end;
        end
        else
        begin
          // See if C1 has a unique index per combiner stage :
          Assert(CurArg.Address = 1);

          if Cur.CombinerStageNr = XFC_COMBINERSTAGENR then
          begin
            // TODO : Is the final combiner C1 als influenced by CombinerHasUniqueC1 ?
            CurArg.Address := X_PSH_CONSTANT_FC1;
            ConstColor := pPSDef.PSFinalCombinerConstant1;
          end
          else
          begin
            if CombinerHasUniqueC1 then
            begin
              // C1 actually ranges from c8 to c15, one for each possible combiner stage :
              // X_D3DRS_PSCONSTANT1_0..X_D3DRS_PSCONSTANT1_7
              Inc(CurArg.Address, 8 + Cur.CombinerStageNr - 1);
              ConstColor := pPSDef.PSConstant1[CurArg.Address - 8];
            end
            else
              ConstColor := pPSDef.PSConstant1[1];
          end;
        end;

        // If this is a new constant, output a 'def' opcode for it at the begin of the shader :
        if not IsConstDeclared[CurArg.Address] then
        begin
          IsConstDeclared[CurArg.Address] := True;

          NewIns.Output[0] := CurArg^;
          NewIns.Output[0].Mask := MASK_RGBA;
          _SetColor(NewIns, ConstColor);
          InsertIntermediate(@NewIns, 0);
          // Note : We'll try to fixup constants above c7 later.
        end;
      end;
    end;

    // Loop over the output arguments :
    for j := 0 to PSH_OPCODE_DEFS[Cur.Opcode]._Out - 1 do
    begin
      CurArg := @(Cur.Output[j]);

      // Remove useless flag, to ease up later comparisions :
      CurArg.Modifiers := CurArg.Modifiers - [ARGMOD_IDENTITY];

      // Discard useless writes :
      if  (CurArg.Address < MAX_ADDRESS)
      and ((RegUsage[CurArg.Type_, CurArg.Address] and CurArg.Mask) = 0) then
      begin
        DbgPrintf('; Removed useless assignment to register ' + CurArg.ToString);
        CurArg.Type_ := PARAM_DISCARD;
      end;
    end;

    // Convert all Xbox opcodes into native opcodes :
    NewIns.Initialize(PO_COMMENT);
    NewIns.CommentString := Cur.ToString;
    case Cur.Opcode of
      PO_XMMA: ConvertXMMAToNative(i);
      PO_XMMC: ConvertXMMCToNative(i);
      PO_XDM: ConvertXDMToNative(i);
      PO_XDD: ConvertXDDToNative(i);
      PO_XFC: ConvertXFCToNative(i); // Can only occur once, as the last instruction
    else
      NewIns.CommentString := '';
    end;

    if NewIns.CommentString <> '' then
      InsertIntermediate(@NewIns, i);
  end;
end; // ConvertXboxShaderToNative

function PSH_XBOX_SHADER.ConvertXMMToNative_Except3RdOutput(i: int): Boolean;
var
  Cur: PPSH_INTERMEDIATE_FORMAT;
  InsertPos: int;
  Ins: PSH_INTERMEDIATE_FORMAT;
begin
  Result := False;
  Cur := @(Intermediate[i]);
  InsertPos := i;

  // This block is meant for cases where XMMA/XMMC discards the 3rd output :
  if Cur.Output[2].Type_ = PARAM_DISCARD then
  begin
    // Mark that this XMMA/XMMC opcode is already handled here :
    Result := True;

    // The opcode must unconditionally change into a MUL (or two) :
    Cur.Opcode := PO_MUL;

    // Is the second output ignored?
    if Cur.Output[1].Type_ = PARAM_DISCARD then
    begin
      // If the first output is also ignored :
      if Cur.Output[0].Type_ = PARAM_DISCARD then
        // The complete opcode can already be removed early on :
        DeleteIntermediate(i)
      else
        ;// The first output is just a MUL, it's output (and first two parameters) are already in-place, so we're done

      Exit;
    end;
    Inc(InsertPos);

    // Create a second MUL opcode for the second result :
    Ins := Cur^;
    Ins.XCopySecondOpcodeToFirst(PO_MUL);
    InsertIntermediate(@Ins, InsertPos);
    Exit;
  end;

  // The third output is needed, but what about the first and second output ?

  if (Cur.Output[0].Type_ = PARAM_DISCARD) then
  begin
    Cur.Output[0].Type_ := PARAM_R;
    Cur.Output[0].Address := FakeRegNr_Xmm1;
  end;

  if (Cur.Output[1].Type_ = PARAM_DISCARD) then
  begin
    Cur.Output[1].Type_ := PARAM_R;
    Cur.Output[1].Address := FakeRegNr_Xmm2;
  end;

  // Generate a MUL for the 1st output :
  Ins := Cur^;
  Ins.Opcode := PO_MUL;
  InsertIntermediate(@Ins, InsertPos);
  Inc(InsertPos);

  // Generate a MUL for the 2nd output :
  Ins := Cur^;
  Ins.XCopySecondOpcodeToFirst(PO_MUL);
  InsertIntermediate(@Ins, InsertPos);

  // Note : If XMMA or XMMC writes to the third argument, we now have
  // the first and second stored already (if they where not ignored).
  // IF one (or both) are ignored, the intermediate result might be
  // needed, but let XMMA/XMMC figure that out first - the resulting
  // opcode(s) will probably require the initial opcode's removal!
end; // ConvertXMMToNative_Except3RdOutput

procedure PSH_XBOX_SHADER.ConvertXMMAToNative(i: int);
var
  Cur: PPSH_INTERMEDIATE_FORMAT;
begin
  // Handle the generic case of XMM, and check if the 3rd (Add) argument is ignored :
  if not ConvertXMMToNative_Except3RdOutput(i) then
  begin
    // Add needs to be stored, we already have 2 MULs, so change the XMMA into an ADD :
    Cur := @(Intermediate[i+2]);
    Cur.Opcode := PO_ADD;
    Cur.Modifier := INSMOD_NONE;
    Cur.Parameters[0] := Cur.Output[0];
    Cur.Parameters[1] := Cur.Output[1];
    Cur.Output[0] := Cur.Output[2];
  end;
end;

procedure PSH_XBOX_SHADER.ConvertXMMCToNative(i: int);
var
  Cur: PPSH_INTERMEDIATE_FORMAT;
begin
  // Handle the generic case of XMM, and check if the 3rd (Compare) argument is ignored :
  if not ConvertXMMToNative_Except3RdOutput(i) then
  begin
    // Add needs to be stored, we already have 2 MULs, so change the XMMC into an CND :
    Cur := @(Intermediate[i+2]);
    Cur.Opcode := PO_CND;
    Cur.Modifier := INSMOD_NONE;
    // Begin the input of CND with the required r0.a parameter :
    Cur.Parameters[0].Type_ := PARAM_R;
    Cur.Parameters[0].Address := 0;
    Cur.Parameters[0].Modifiers := [ARGMOD_IDENTITY];
    Cur.Parameters[0].Multiplier := 1.0;
    Cur.Parameters[0].Mask := MASK_A;
    // Follow that with the 2 selection registers :
    Cur.Parameters[1] := Cur.Output[0];
    Cur.Parameters[2] := Cur.Output[1];
    // And put the result it in the final register :
    Cur.Output[0] := Cur.Output[2];
  end;
end;

procedure PSH_XBOX_SHADER.ConvertXDMToNative(i: int);
var
  Cur: PPSH_INTERMEDIATE_FORMAT;
  Ins: PSH_INTERMEDIATE_FORMAT;
begin
  Cur := @(Intermediate[i]);

  // XDM does two operations :

  // a multiply :
  if Cur.Output[1].Type_ <> PARAM_DISCARD then
  begin
    Ins := Cur^;
    Ins.XCopySecondOpcodeToFirst(PO_MUL);
    InsertIntermediate(@Ins, i+1);
  end;

  // and a dot product :
  if Cur.Output[0].Type_ = PARAM_DISCARD then
    DeleteIntermediate(i)
  else
    Cur.Opcode := PO_DP3;
end;

procedure PSH_XBOX_SHADER.ConvertXDDToNative(i: int);
var
  Cur: PPSH_INTERMEDIATE_FORMAT;
  Ins: PSH_INTERMEDIATE_FORMAT;
begin
  Cur := @(Intermediate[i]);

  // XDD does two operations :

  // ...a dot product :
  Cur.Opcode := PO_DP3;

  // and another dot product :
  if Cur.Output[1].Type_ <> PARAM_DISCARD then
  begin
    Ins := Cur^;
    Ins.XCopySecondOpcodeToFirst(PO_DP3);
    InsertIntermediate(@Ins, i+1);
  end;
end;

procedure PSH_XBOX_SHADER.ConvertXFCToNative(i: int);
var
  Cur: PSH_INTERMEDIATE_FORMAT;
  InsertPos: int;
  NeedsProd: Boolean;
  NeedsSum: Boolean;
  CurArg: PPSH_IMD_ARGUMENT;
  Ins: PSH_INTERMEDIATE_FORMAT;
begin
  // Get a copy of XFC and remove it already, new instructions will replace it :
  Cur := Intermediate[i];
  DeleteIntermediate(i);
  InsertPos := i;

  // See if the final combiner uses the prod or sum input parameters :
  NeedsProd := False;
  NeedsSum := False;
  for i := 0 to PSH_OPCODE_DEFS[Cur.Opcode]._In - 1 do
  begin
    CurArg := @(Cur.Parameters[i]);

    // Check for the three final-combiner-specific argument types :
    case CurArg.Type_ of
      PARAM_V1R0_SUM:
      begin
        // Change SUM into a fake register, which will be resolved later :
        CurArg.Type_ := PARAM_R;
        CurArg.Address := FakeRegNr_Sum;
        NeedsSum := True;
        Cur.CommentString := 'final combiner - V1R0_sum register';
      end;

      PARAM_EF_PROD:
      begin
        // Change PROD into a fake register, which will be resolved later :
        CurArg.Type_ := PARAM_R;
        CurArg.Address := FakeRegNr_Prod;
        NeedsProd := True;
        Cur.CommentString := 'final combiner - EF_prod register';
      end;

      PARAM_FOG:
      begin
        // Change FOG into a constant of 1.0, as we can't simulate it otherwise :
        CurArg.SetConstValue(1.0);
        Cur.CommentString := 'final combiner - FOG not emulated, using 1.';
      end;
    end;
  end; // for input

  if NeedsSum then
  begin
    // Add a new opcode that calculates r0*v1 :
    Ins.Initialize(PO_MUL);
    Ins.Output[0].Type_ := PARAM_R;
    Ins.Output[0].Address := FakeRegNr_Sum;
    Ins.Output[0].Mask := MASK_RGBA;

    Ins.Parameters[0].Type_ := PARAM_R;
    Ins.Parameters[0].Address := 0;
    Ins.Parameters[1].Type_ := PARAM_V;
    Ins.Parameters[1].Address := 1;

    // Take the FinalCombinerFlags that influence this result into account :
    if ((FinalCombinerFlags and PS_FINALCOMBINERSETTING_COMPLEMENT_R0) > 0) then
      Ins.Parameters[0].Modifiers := [ARGMOD_INVERT]; // (1-r0) is used as an input to the sum rather than r0
    if ((FinalCombinerFlags and PS_FINALCOMBINERSETTING_COMPLEMENT_V1) > 0) then
      Ins.Parameters[1].Modifiers := [ARGMOD_INVERT]; // (1-v1) is used as an input to the sum rather than v1
    if ((FinalCombinerFlags and PS_FINALCOMBINERSETTING_CLAMP_SUM) > 0) then
      Ins.Modifier := INSMOD_SAT; // V1+R0 sum clamped to [0,1]

    InsertIntermediate(@Ins, InsertPos);
    Inc(InsertPos);
  end;

  if NeedsProd then
  begin
    // Add a new opcode that calculates E*F :
    Ins.Initialize(PO_MUL);
    Ins.Output[0].Type_ := PARAM_R;
    Ins.Output[0].Address := FakeRegNr_Prod;
    Ins.Output[0].Mask := MASK_RGBA;
    Ins.Parameters[0] := Cur.Parameters[4]; // E
    Ins.Parameters[1] := Cur.Parameters[5]; // F
    InsertIntermediate(@Ins, InsertPos);
    Inc(InsertPos);
  end;

  // The final combiner calculates : r0.rgb=s0*s1 + (1-s0)*s2 + s3
  // Change that into a LRP + ADD, and let the optimizer reduce it;

  // Add a new opcode that calculates r0.rgb=s0*s1 + (1-s0)*s2 via a LRP :
  // Set the output to r0.rgb (as r0.a is determined via s6.a) :
  Cur.Output[0].Type_ := PARAM_R;
  Cur.Output[0].Address := 0;
  Cur.Output[0].Mask := MASK_RGB;

  Ins := Cur;
  Ins.Opcode := PO_LRP;
  Ins.Modifier := INSMOD_NONE;
  InsertIntermediate(@Ins, InsertPos);
  Inc(InsertPos);

  // Add a new opcode that calculates r0.rgb=r0.rgb+s3 :
  Ins.Opcode := PO_ADD;
  Ins.Modifier := Cur.Modifier;
  Ins.Output[0] := Cur.Output[0]; // = r0.rgb
  Ins.Parameters[0] := Cur.Output[0]; // = r0.rgb
  Ins.Parameters[1] := Cur.Parameters[3]; // =s3 from XFC
  InsertIntermediate(@Ins, InsertPos);
  Inc(InsertPos);

  // See if s6 is something else than 'r0.a' :
  if Cur.Parameters[6].ToString <> 'r0.a' then
  begin
    // Add a new opcode that moves s6 over to r0.a :
    Ins.Initialize(PO_MOV);
    Ins.Output[0].Type_ := PARAM_R;
    Ins.Output[0].Address := 0;
    Ins.Output[0].Mask := MASK_A;
    Ins.Parameters[0] := Cur.Parameters[6];
    InsertIntermediate(@Ins, InsertPos);
    Inc(InsertPos);
  end;
end;

function PSH_XBOX_SHADER.RemoveNops(): Boolean;
var
  i, j: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
  HasOutput: Boolean;
begin
  Result := False;
  i := IntermediateCount;
  while i > 0 do
  begin
    Dec(i);
    Cur := @(Intermediate[i]);

    // Skip opcodes that have no output, but should stay anyway :
    if Cur.Opcode in [PO_COMMENT, PO_XFC] then
      Continue;

    // See if this opcode writes to any of it's outputs :
    begin
      HasOutput := False;
      for j := 0 to PSH_OPCODE_DEFS[Cur.Opcode]._Out - 1 do
        if  (Cur.Output[j].Type_ <> PARAM_DISCARD) then
        begin
          HasOutput := True;
          Break;
        end;

      if not HasOutput then
      begin
        // Remove the opcode (as it doesn't change anything) :
        // This applies to PO_NOP and opcodes that discard all their results :
        DeleteIntermediate(i);
        Result := True;
        Continue;
      end;
    end;
  end;
end;

function PSH_XBOX_SHADER.CombineInstructions(): Boolean;

  function _CanLerp(Mul1, Mul2, AddOpcode: PPSH_INTERMEDIATE_FORMAT; Left, Right: int): Boolean;
  var
    ParamLeft, ParamRight: PPSH_IMD_ARGUMENT;
  begin
    // Check if Left and Right are the same register :
    Result := False;
    ParamLeft := @Mul1.Parameters[Left];
    ParamRight := @Mul2.Parameters[Right];
    if (ParamLeft.Type_ <> ParamRight.Type_) or (ParamLeft.Address <> ParamRight.Address) then
      Exit;

    // Is the left argument inverted and the right not (or the other way around) ?
    if (ParamLeft.Modifiers * [ARGMOD_INVERT]) <> (ParamRight.Modifiers * [ARGMOD_INVERT]) then
    begin
      // In that case, already move the arguments over to AddOpcode so we create a LRP :
      AddOpcode.Parameters[0] := ParamLeft^;
      AddOpcode.Parameters[1] := Mul1.Parameters[1-Left];
      AddOpcode.Parameters[2] := Mul2.Parameters[3-Right];
      Result := True;
    end;
  end;

  function _CanMad(ConstOne: int; Mul1, Mul2, AddOpcode: PPSH_INTERMEDIATE_FORMAT): Boolean;
  begin
    // Check if the given parameter is 1 :
    Result := Mul1.Parameters[ConstOne].GetConstValue = 1.0;
    if Result then
    begin
      // Put the other 3 parameters int the resulting opcode, so we can make it a MAD :
      AddOpcode.Parameters[0] := Mul2.Parameters[0];
      AddOpcode.Parameters[1] := Mul2.Parameters[1];
      AddOpcode.Parameters[2] := Mul1.Parameters[1-ConstOne];
    end;
  end;

var
  i: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
begin
  Result := False;

  i := IntermediateCount;
  while i > 0 do
  begin
    Dec(i);
    Cur := @(Intermediate[i]);

(*
    // Combinations that can be made if their intermediate result is not read again or overwritten later:

    MOV+ADD > ADD (if MOV.Output[0] was only read by ADD.Parameter[0] or ADD.Parameter[1])
    MOV+SUB > SUB (if MOV.Output[0] was only read by SUB.Parameter[0] or SUB.Parameter[1])
    MOV+MUL > MUL (if MOV.Output[0] was only read by MOV.Parameter[0] or MOV.Parameter[1])

    MUL+MOV > MUL (if MUL.Output[0] was only read by MOV.Parameter[0])
    MUL+ADD > MAD (if MUL.Output[0] was only read by ADD.Parameter[0] or ADD.Parameter[1])
    MUL+SUB > MAD (if MUL.Output[0] was only read by SUB.Parameter[0] - Do invert MAD.Parameter[2])

    // TODO : We can remove a MOV entirely if the input is not changed while
    // the output is read, up until the output is re-written; We can change all
    // these occurances into a read from the input of this MOV instead.
    if  (Cur.Opcode = PO_MOV) then
    begin
      CanOptimize := False;
      j := i + 1;
      while j < IntermediateCount do
      begin
        CanOptimize := True;
        if Intermediate[j].OutputsTo(Cur.Output[0]) then
          Break;

        CanOptimize := False;
        Inc(j);
      end;

      if CanOptimize then
      begin
        while j > i do
        begin
          // TODO : For Intermediate[j].Parameters, change all occurrances of Cur.Parameters[0] into Cur.Output[0]
          Dec(j);
        end;

        DeleteIntermediate(i);
      end;
    end;
*)
    // Check if there are two consecutive opcodes reading from a fake R register;
    // We outputted these ourselves, in order to ease the conversion and profit
    // from having generic optimizations in one place :
    if  (Cur.Output[0].Type_ = PARAM_R)
    and (Cur.Output[0].Address >= PSH_XBOX_MAX_R_REGISTER_COUNT)
    and (Intermediate[i+1].Output[0].Type_ = PARAM_R)
    and (Intermediate[i+1].Output[0].Address >= PSH_XBOX_MAX_R_REGISTER_COUNT) then
    begin
      // Did we output those from a CND opcode (originally XMMC) ?
      if (Intermediate[i+2].Opcode = PO_CND) then
      begin
        if  (Cur.Opcode = PO_MOV)
        and (Intermediate[i+1].Opcode = Cur.Opcode)
        and (Intermediate[i+1].Modifier = Cur.Modifier) then
        begin
          Intermediate[i+2].Modifier := Cur.Modifier;
          Intermediate[i+2].Parameters[1] := Cur.Parameters[0];
          Intermediate[i+2].Parameters[2] := Intermediate[i+1].Parameters[0];
          DeleteIntermediate(i);
          DeleteIntermediate(i);
          DbgPrintf('; Changed temporary MUL,MUL,CND via MOV,MOV,CND into a single CND');
          Result := True;
          Continue;
        end;
      end;

      // Did we output those from a ADD opcode (originally XMMA) ?
      if (Intermediate[i+2].Opcode = PO_ADD) then
      begin
        if  (Cur.Opcode = PO_MUL)
        and (Intermediate[i+1].Opcode = Cur.Opcode)
        and (Intermediate[i+1].Modifier = Cur.Modifier) then
        begin
          // Check if we can lerp - we just need the same register on both sides that's inverted on the other :
          if _CanLerp(Cur, @Intermediate[i+1], @Intermediate[i+2], 0, 2)
          or _CanLerp(Cur, @Intermediate[i+1], @Intermediate[i+2], 1, 2)
          or _CanLerp(Cur, @Intermediate[i+1], @Intermediate[i+2], 0, 3)
          or _CanLerp(Cur, @Intermediate[i+1], @Intermediate[i+2], 1, 3) then
          begin
            // The lerp can be done, and the correct parameters are already set to Intermediate[i+2],
            // so all we need to do now, it fixup the rest and remove the two MOV's :
            Intermediate[i+2].Opcode := PO_LRP;
            Intermediate[i+2].Modifier := Cur.Modifier;
            DeleteIntermediate(i);
            DeleteIntermediate(i);
            DbgPrintf('; Changed temporary MUL,MUL,ADD into a single LRP');
            Result := True;
            Continue;
          end;

          // Check if we can mad - we just need a constant 1 in one argument :
          if _CanMad(0, Cur, @Intermediate[i+1], @Intermediate[i+2])
          or _CanMad(1, Cur, @Intermediate[i+1], @Intermediate[i+2])
          or _CanMad(0, @Intermediate[i+1], Cur, @Intermediate[i+2])
          or _CanMad(1, @Intermediate[i+1], Cur, @Intermediate[i+2]) then
          begin
            // The lerp can be done, and the correct parameters are already set to Intermediate[i+2],
            // so all we need to do now, it fixup the rest and remove the two MOV's :
            Intermediate[i+2].Opcode := PO_MAD;
            Intermediate[i+2].Modifier := Cur.Modifier;
            DeleteIntermediate(i);
            DeleteIntermediate(i);
            DbgPrintf('; Changed temporary MUL,MUL,ADD into a single MAD');
            Result := True;
            Continue;
          end;

          // No single opcode possible, so change it into a MUL + MAD :
          // The first mul may write to the last output register (without a modifier) :
          Cur.Modifier := INSMOD_NONE;
          Cur.Output[0] := Intermediate[i+2].Output[0];
          // Change the second MUL into a MAD :
          Intermediate[i+1].Opcode := PO_MAD;
          Intermediate[i+1].Output[0] := Intermediate[i+2].Output[0];
          Intermediate[i+1].Parameters[2] := Cur.Output[0];
          // Remove the trailing ADD :
          DeleteIntermediate(i+2);
          DbgPrintf('; Changed temporary MUL,MUL,ADD into a MUL,MAD');
          Result := True;
          Continue;
        end;
      end;
    end;
  end; // while
end; // CombineInstructions

function PSH_XBOX_SHADER.SimplifyMOV(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
var
  CanSimplify: Boolean;
begin
  Result := False;

  // NOP-out MOV's that read & write to the same register :
  if  (Cur.Output[0].Type_ = Cur.Parameters[0].Type_)
  and (Cur.Output[0].Address = Cur.Parameters[0].Address)
  and (Cur.Output[0].Mask = Cur.Parameters[0].Mask) then
  begin
    if Cur.Output[0].Type_ = PARAM_VALUE then
      CanSimplify := Cur.Output[0].GetConstValue = Cur.Parameters[0].GetConstValue
    else
      CanSimplify := (Cur.Output[0].Modifiers = Cur.Parameters[0].Modifiers)
                 and (Cur.Output[0].Multiplier = Cur.Parameters[0].Multiplier);

    if CanSimplify then
    begin
      Cur.Opcode := PO_NOP; // This nop will be removed in a recursive fixup
      Result := True;
      Exit;
    end;
  end;

  // Does this MOV put a 0 (zero) in the output?
  if Cur.Parameters[0].GetConstValue = 0.0 then
  begin
    // TODO : Find a constant with the value 0, and use that if present.
    // Fixup via "sub d0=v0,v0" :
    Cur.Opcode := PO_SUB;
    Cur.Parameters[0].Type_ := PARAM_V;
    Cur.Parameters[0].Address := 0;
    Cur.Parameters[0].Modifiers := [];
    Cur.Parameters[1] := Cur.Parameters[0];
    Exit;
  end;

  // Does this MOV put a 1 (one) in the output?
  if Cur.Parameters[0].GetConstValue = 1.0 then
  begin
    // TODO : Find a constant with the value 1, and use that if present.
    // Fixup via "sub d0=1-v0,-v0" :
    Cur.Opcode := PO_SUB;
    Cur.Parameters[0].Type_ := PARAM_V;
    Cur.Parameters[0].Address := 0;
    Cur.Parameters[0].Modifiers := [ARGMOD_INVERT];
    Cur.Parameters[1] := Cur.Parameters[0];
    Cur.Parameters[1].Modifiers := [ARGMOD_NEGATE];
    Exit;
  end;

  // TODO : Simulate other factors (like -2.0, -1.0, -0.5, 0.5 and 2.0)?
end;

function PSH_XBOX_SHADER.SimplifyADD(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
begin
  Result := False;

  // Is this an addition of s0+0 ?
  if Cur.Parameters[1].GetConstValue = 0.0 then
  begin
    // Change it into a MOV (the first argument is already in-place)
    Cur.Opcode := PO_MOV;
    Result := True;
    Exit;
  end;
end;

function PSH_XBOX_SHADER.SimplifySUB(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
begin
  Result := False;

  // Is this an subtraction of s0-0 ?
  if Cur.Parameters[1].GetConstValue = 0.0 then
  begin
    // Change it into a MOV (the first argument is already in-place)
    Cur.Opcode := PO_MOV;
    Result := True;
    Exit;
  end;
end;

function PSH_XBOX_SHADER.SimplifyMUL(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
begin
  Result := False;

  // Is the result of this multiplication zero ?
  if (Cur.Parameters[1].GetConstValue = 0.0) then
  begin
    // Change it into a MOV (the 0 argument will be resolve in a recursive MOV fixup) :
    Cur.Opcode := PO_MOV;
    Cur.Parameters[0].SetConstValue(0.0);
    Result := True;
    Exit;
  end;

  // Is this a multiply-by-const 1 ?
  if (Cur.Parameters[1].GetConstValue = 1.0) then
  begin
    // Change it into a simple MOV :
    Cur.Opcode := PO_MOV;
    Result := True;
    Exit;
  end;

  // TODO : What about multiplications by other factors (like -2.0, -1.0, -0.5, 0.5 and 2.0)?
end; // SimplifyMUL

function PSH_XBOX_SHADER.SimplifyLRP(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
begin
  Result := False;

  // LRP calculates : d0=s0*s1+(1-s0)*s2 which can also be read as : d0=s0*(s1-s2)+s2

  // Is the right part ((1-s0)*s2) zero?
  if (Cur.Parameters[0].GetConstValue = 1.0) or (Cur.Parameters[2].GetConstValue = 0.0) then
  begin
    // Change it into a MUL (calculating the left part : s0*s1 :
    Cur.Opcode := PO_MUL;
    Result := True;
    Exit;
  end;

  // Is the left part (s0*s1) zero?
  if (Cur.Parameters[0].GetConstValue = 0.0) or (Cur.Parameters[1].GetConstValue = 0.0) then
  begin
    // Change it into a MUL (calculating the right part : (1-s0)*s2) :
    Cur.Opcode := PO_MUL;
    Cur.Parameters[0].Invert;
    Cur.Parameters[1] := Cur.Parameters[2];
    Result := True;
    Exit;
  end;
end; // SimplifyLRP

function PSH_XBOX_SHADER.FixupPixelShader(): Boolean;
var
  i: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
begin
  Result := RemoveNops();

  // TODO : Fixup writes to read-only registers (V0, V1) via another free register (if possible)
  // TODO : Fixup the usage of non-existent register numbers (like FakeRegNr_Sum and FakeRegNr_Prod)
  // TODO : Fixup the usage of the unsupported INSMOD_BIAS and INSMOD_BX2 instruction modifiers
  // TODO : Use the INSMOD_SAT instruction modifier instead of the ARGMOD_SATURATE argument modifier
  // TODO : Shift independent instructions up or down so the alpha write combiner can be used more often
  // TODO : Condense constants registers, to avoid the non-existant C8-C15 (requires a mapping in SetPixelShaderConstant too...)

  // TODO : Conversion steps :
  // - Convert numeric arguments (-2, -1, 0, 1, 2) into modifiers on the other argument
  // - Merge idential .rgb .a instructions
  // - Move independent .a channel instructions upwards to enable channel-combining
  // - Mark .a instructions as 'Combined' if they follow an .rgb instruction

  // TODO : Fixup C8..C15 + FC0+FC1 constants - ps.1.3 goes only up to C7 (PS.3.0 has 224, but needs a complete Direct3D9 port)
  // With a bit of luck, there are gaps in the constant-usage (which we could remove via a mapping table),
  // or some of the additional constants could be identical to one of C0..C7, in which case we could
  // switch them over (hopeing that no SetPixelShaderConstant call updates these).

  if MoveRemovableParametersRight then
    Result := True;

  if CombineInstructions() then
    Result := True;

  // Simplify instructions, which can help to compress the result :
  i := IntermediateCount;
  while i > 0 do
  begin
    Dec(i);
    Cur := @(Intermediate[i]);

    case Cur.Opcode of
      PO_MOV:
        if SimplifyMOV(Cur) then
          Result := True;

      PO_ADD:
        if SimplifyADD(Cur) then
          Result := True;

      PO_SUB:
        if SimplifySUB(Cur) then
          Result := True;

      PO_MUL:
        if SimplifyMUL(Cur) then
          Result := True;

      PO_LRP:
        if SimplifyLRP(Cur) then
          Result := True;
    end; // case
  end; // for

  // If the above code made any alteration, repeat it as some changes require a followup (like MUL>MOV>NOP) :
  if Result then
  begin
    Log('Fixup intermediate result');
    FixupPixelShader();
  end;
end; // FixupPixelShader

function PSH_XBOX_SHADER.FixInvalidSrcSwizzle(): Boolean;
var
  i, j: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
  CurArg: PPSH_IMD_ARGUMENT;
begin
  Result := False;
  for i := 0 to IntermediateCount - 1 do
  begin
    Cur := @(Intermediate[i]);
    // Is this an arithmetic opcode?
    if Cur.Opcode > PO_TEX then
    begin
      // Loop over the input arguments :
      for j := 0 to PSH_OPCODE_DEFS[Cur.Opcode]._In - 1 do
      begin
        CurArg := @(Cur.Parameters[j]);

        // Fix "Invalid src swizzle" :
        if CurArg.Mask = MASK_RGB then
        begin
          CurArg.Mask := MASK_RGBA;
          Result := True;
        end;
      end;
    end;
  end;
end;

function PSH_XBOX_SHADER.FixMissingR0a(): Boolean;
var
  NeedsR0aDefault: Boolean;
  i, j: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
  CurArg: PPSH_IMD_ARGUMENT;
  NewIns: PSH_INTERMEDIATE_FORMAT;
begin
  Result := False;
  // Detect a read of r0.a without a write, as we need to insert a "MOV r0.a, t0.a" as default (like the xbox has) :
  NeedsR0aDefault := True;
  i := IntermediateCount;
  while (i > 0) and NeedsR0aDefault do
  begin
    Dec(i);
    Cur := @(Intermediate[i]);
    if Cur.Opcode = PO_DEF then
      Break;

    // Check if r0.a is written to by this opcode :
    for j := 0 to PSH_OPCODE_DEFS[Cur.Opcode]._Out - 1 do
    begin
      CurArg := @(Cur.Output[j]);
      if  (CurArg.Type_ = PARAM_R)
      and (CurArg.Address = 0)
      and ((CurArg.Mask and MASK_A) > 0) then
      begin
        // Then we don't have to insert a default :
        NeedsR0aDefault := False;
        Break;
      end;
    end;
  end;

  if NeedsR0aDefault then
  begin
    // Insert a new opcode : MOV r0.a, t0.a
    NewIns.Initialize(PO_MOV);
    NewIns.Output[0].Type_ := PARAM_R;
    NewIns.Output[0].Address := 0;
    NewIns.Output[0].Mask := MASK_A;
    NewIns.Parameters[0] := NewIns.Output[0];
    NewIns.Parameters[0].Type_ := PARAM_T;
    NewIns.CommentString := 'Inserted r0.a default';
    InsertIntermediate(@NewIns, i);
    Result := True;
  end;
end; // FixMissingR0a

function PSH_XBOX_SHADER.FixCoIssuedOpcodes(): Boolean;
var
  PrevMask: DWORD;
  i: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
begin
  Result := False;

  // Start with Alpha, so the first opcode doesn't become a write-combined opcode (which isn't possible) :
  PrevMask := MASK_A;
  // TODO : We could try to bubble some opcodes up and/or down if possible, to combine better.
  for i := 0 to IntermediateCount - 1 do
  begin
    Cur := @(Intermediate[i]);
    // Is this an arithmetic opcode?
    if Cur.Opcode > PO_TEX then
    begin
      // Set IsCombined only when previous opcode doesn't write to Alpha, while this opcode writes only to Alpha :
      if Cur.IsCombined <> ((PrevMask and MASK_A) = 0) and (Cur.Output[0].Mask = MASK_A) then
      begin
        Cur.IsCombined := not Cur.IsCombined;
        Result := True;
      end;

      PrevMask := Cur.Output[0].Mask;
    end;
  end;
end;

{$ENDIF PS_REWRITE}

///

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
//    Assert((PS_REGISTER_C0 <= aReg) and (aReg <= PS_REGISTER_EF_PROD));
    Result := 'c7'; // TODO : FocusBlur sample needs a zero in 'cnd' opcode
  end;
end;

const
  // Multiplication results in a constant :
  MULRESULT_ZERO     =  0;
  MULRESULT_ONE      =  1;
  MULRESULT_NEG_ONE  = -1;
  MULRESULT_HALF     =  2; // Note : Instead of 0.5 we use 2 (so we can keep using integers)
  MULRESULT_NEG_HALF = -2; // Note : Not the value, but the matching to these consts is what matters!
  // Multiplication results in one register (albeit optionally factored) :
  MULRESULT_VARIABLE = MULRESULT_ONE * 3;
  MULRESULT_NEG_VARIABLE = MULRESULT_NEG_ONE * MULRESULT_VARIABLE;
  MULRESULT_HALF_VARIABLE = MULRESULT_HALF * MULRESULT_VARIABLE;
  MULRESULT_NEG_HALF_VARIABLE = MULRESULT_NEG_HALF * MULRESULT_VARIABLE;
  // Multiplication is affected by both input registers :
  MULRESULT_MULTIPLY = MULRESULT_VARIABLE * MULRESULT_VARIABLE;

// Determine a 'Multiplication indicator' for the given register,
// which is used to determine the type of output comming from a multiplication.
function DetermineRegisterMultiplicationIndicator(const aReg: PS_REGISTER): int;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  case aReg of
    PS_REGISTER_ZERO:
      Result := MULRESULT_ZERO;
    PS_REGISTER_ONE:
      Result := MULRESULT_ONE;
    PS_REGISTER_NEGATIVE_ONE:
      Result := MULRESULT_NEG_ONE;
    PS_REGISTER_ONE_HALF:
      Result := MULRESULT_HALF;
    PS_REGISTER_NEGATIVE_ONE_HALF:
      Result := MULRESULT_NEG_HALF;
  else
    Result := MULRESULT_VARIABLE;
  end;
end;

{ RPSRegisterObject }

procedure RPSRegisterObject.Decode(Value: Byte; aIsAlpha: Boolean);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  IsAlpha := aIsAlpha;
  Reg := PS_REGISTER(Value);
end;

function RPSRegisterObject.IsNativeRegWriteable: Boolean;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := Reg in [PS_REGISTER_T0..PS_REGISTER_T3,
                    PS_REGISTER_R0..PS_REGISTER_R1];

  // Note : Xbox allows writing to V0 (diffuse color) and V1 (specular color), but native ps.1.3 doesn't!
  // Some examples of this behaviour can be seen when running RayMan Arena.
  // TODO : How are we ever going to support this?!?
end;

function RPSRegisterObject.IsSameAsAlpha(const Alpha: PPSRegisterObject): Boolean;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := (Reg = Alpha.Reg);
end;

function RPSRegisterObject.IntermediateToString(): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Assert((PS_REGISTER_DISCARD <= Reg) and (Reg <= PS_REGISTER_EF_PROD));

  Result := PS_RegisterStr[Ord(Reg) + 1];
end;

function RPSRegisterObject.DisassembleRegister(const aScope: PPSDisassembleScope): string;
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

  // Remove the above flags from the register :
  Reg := PS_REGISTER(Ord(Reg) and $f);

  // Check if the input Register is ZERO, in which case we want to allow the extended registers :
  if (Reg = PS_REGISTER_ZERO) then
  begin
    case PS_REGISTER(InputMapping) of
      PS_REGISTER_ONE, PS_REGISTER_NEGATIVE_ONE, PS_REGISTER_ONE_HALF, PS_REGISTER_NEGATIVE_ONE_HALF:
        // These input mapping have their own register - keep these in 'Reg', so we can check for them :
        Reg := PS_REGISTER(InputMapping);

      PS_REGISTER(PS_INPUTMAPPING_EXPAND_NEGATE):
        // This case has no separate PS_REGISTER define, but when applied to zero, also results in one :
        Reg := PS_REGISTER_ONE;
    end;
  end;

  // Ease the decoding stage, by determining the 'Multiplication indicator' for this input :
  MulResult := DetermineRegisterMultiplicationIndicator(Reg);

  // The input can have the following mappings applied :
  //
  // PS_INPUTMAPPING_UNSIGNED_IDENTITY : y = max(0,x)       =  1*max(0,x) + 0.0
  // PS_INPUTMAPPING_UNSIGNED_INVERT   : y = 1 - max(0,x)   = -1*max(0,x) + 1.0
  // PS_INPUTMAPPING_EXPAND_NORMAL     : y = 2*max(0,x) - 1 =  2*max(0,x) - 1.0
  // PS_INPUTMAPPING_EXPAND_NEGATE     : y = 1 - 2*max(0,x) = -2*max(0,x) + 1.0
  // PS_INPUTMAPPING_HALFBIAS_NORMAL   : y = max(0,x) - 1/2 =  1*max(0,x) - 0.5
  // PS_INPUTMAPPING_HALFBIAS_NEGATE   : y = 1/2 - max(0,x) = -1*max(0,x) + 0.5
  // PS_INPUTMAPPING_SIGNED_IDENTITY   : y = x              =  1*      x  + 0.0
  // PS_INPUTMAPPING_SIGNED_NEGATE     : y = -x             = -1*      x  + 0.0
  //
  // (Note : I don't know for sure if the max() operation mentioned above is indeed what happens,
  // as there's no further documentation available on this. Native Direct3D can clamp with the
  // '_sat' instruction modifier, but that's not really the same as these Xbox1 input mappings.)
  //
  // When the input register is PS_ZERO, the above mappings result in the following constants:
  //
  // PS_REGISTER_NEGATIVE_ONE      (PS_INPUTMAPPING_EXPAND_NORMAL on zero)   : y = -1.0
  // PS_REGISTER_NEGATIVE_ONE_HALF (PS_INPUTMAPPING_HALFBIAS_NORMAL on zero) : y = -0.5
  // PS_REGISTER_ZERO itself                                                 : y =  0.0
  // PS_REGISTER_ONE_HALF          (PS_INPUTMAPPING_HALFBIAS_NEGATE on zero) : y =  0.5
  // PS_REGISTER_ONE               (PS_INPUTMAPPING_UNSIGNED_INVERT on zero) : y =  1.0
  // (Note : It has no define, but PS_INPUTMAPPING_EXPAND_NEGATE on zero results in ONE too!)

  case InputMapping of
    PS_INPUTMAPPING_UNSIGNED_IDENTITY: begin Multiplier :=  1; Correction := +0.0; end;
    PS_INPUTMAPPING_UNSIGNED_INVERT  : begin Multiplier := -1; Correction := +1.0; end;
    PS_INPUTMAPPING_EXPAND_NORMAL    : begin Multiplier :=  2; Correction := -1.0; end;
    PS_INPUTMAPPING_EXPAND_NEGATE    : begin Multiplier := -2; Correction := +1.0; end;
    PS_INPUTMAPPING_HALFBIAS_NORMAL  : begin Multiplier :=  1; Correction := -0.5; end;
    PS_INPUTMAPPING_HALFBIAS_NEGATE  : begin Multiplier := -1; Correction := +0.5; end;
    PS_INPUTMAPPING_SIGNED_IDENTITY  : begin Multiplier :=  1; Correction := +0.0; end;
    PS_INPUTMAPPING_SIGNED_NEGATE    : begin Multiplier := -1; Correction := +0.0; end;
  end;
end;

function RPSInputRegister.IsSameAsAlpha(const Alpha: PPSInputRegister): Boolean;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := (Reg = Alpha.Reg)
        and (InputMapping = Alpha.InputMapping);
end;

function RPSInputRegister.IntermediateToString(): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  InputMappingStr: string;
begin
  InputMappingStr := '';
  case Reg of
    PS_REGISTER_ZERO:
    begin
      Result := PS_RegisterStr[0];
      Exit;
    end;
    PS_REGISTER_ONE:
      Result := PS_RegisterStr[$11];
    PS_REGISTER_NEGATIVE_ONE:
      Result := PS_RegisterStr[$12];
    PS_REGISTER_ONE_HALF:
      Result := PS_RegisterStr[$13];
    PS_REGISTER_NEGATIVE_ONE_HALF:
      Result := PS_RegisterStr[$14];
  else
    Result := inherited IntermediateToString();
    InputMappingStr := ' | ' + PS_InputMappingStr[(Ord(InputMapping) shr 5) and 7];
  end;

  // Render the channel as a string :
  Result := Result + ' | ' + PS_ChannelStr[iif(Ord(Channel) > 0, {Alpha}2, iif(IsAlpha, {Blue}1, {RGB}0))] + InputMappingStr;
end;

function RPSInputRegister.DisassembleInputRegister(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := inherited DisassembleRegister(aScope);
  if Result = '' then
    Exit;

  // Check if only the alpha channel is read :
  if (Channel = PS_CHANNEL_ALPHA) then
    // Because if we're in an RGB channel :
    if not IsAlpha then
      // we need to add the alpha channel selector :
      Result := Result + '.a';

  case InputMapping of
//    PS_INPUTMAPPING_UNSIGNED_IDENTITY: // abs(x)
//      Result := Result + '_sat';
    PS_INPUTMAPPING_UNSIGNED_INVERT: // 1-x
      Result := '1-' + Result;
    PS_INPUTMAPPING_EXPAND_NORMAL: // (2*x) - 1
      Result := Result + '_bx2';
    PS_INPUTMAPPING_EXPAND_NEGATE: // 1 - (2*x)
      Result := '1-' + Result + '_x2';
//    PS_INPUTMAPPING_HALFBIAS_NORMAL: // max(0,x) - 1/2   invalid for final combiner
//    PS_INPUTMAPPING_HALFBIAS_NEGATE: // 1/2 - max(0,x)   invalid for final combiner
    PS_INPUTMAPPING_SIGNED_IDENTITY: ; // x
    PS_INPUTMAPPING_SIGNED_NEGATE: // -x
      Result := '-' + Result;
  end;
end;

{ RPSDisassembleScope }

// TODO : With all these emits, check if r0.a is read before it's written,
// in which case we have to emit the "mov r0.a, t0.a" instruction (favorably
// in a free alpha channel slot).

function RPSDisassembleScope.EmitConstant(const OutputStr: string; const MulResult: int): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  case MulResult of
    MULRESULT_ZERO:
      // Simulate 0 by subtracting a (guaranteed) register from itself :
      Result := 'sub' + OutputStr + 'v0, v0 ; Const 0.0';
    MULRESULT_ONE:
      // Simulate 1 by calculating it via a (guaranteed) register :
      // We follow this for now : (1-v0) - (-v0) = (1-v0) + v0 = 1
      Result := 'sub' + OutputStr + '1-v0, -v0 ; Const 1.0'; // TODO : Should we use Input2ReadMask?
    MULRESULT_NEG_ONE:
      // Simulate -1 by calculating it via a (guaranteed) register :
      // We follow this for now : (-v0) - (1-v0) = -v0 - 1 + v0 = -1
      Result := 'sub' + OutputStr + '-v0, 1-v0 ; Const -1.0'; // TODO : Should we use Input2ReadMask?
    MULRESULT_HALF:
      Result := '; TODO : MULRESULT_HALF';
    MULRESULT_NEG_HALF:
      Result := '; TODO : MULRESULT_NEG_HALF';
  else
    Result := '';
  end;
end;

function RPSDisassembleScope.EmitMov(const OutputStr: string; const Input: PPSRegisterObject): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := Input.DisassembleRegister(@Self);
  if Pos(Result, OutputStr) > 0 then
    Result := ''
  else
    Result := 'mov ' + OutputStr + ', ' + Result + #13#10;
end;

function RPSDisassembleScope.EmitAddOutputs(const OutputStr: string; const Input1, Input2: PPSRegisterObject): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if Input2.Reg = PS_REGISTER_ZERO then
    Result := EmitMov(OutputStr, Input1)
  else
    Result := 'add' + OutputStr + Input1.DisassembleRegister(@Self) + ', ' + Input2.DisassembleRegister(@Self) + #13#10;
end;

function RPSDisassembleScope.EmitAdd(const OutputStr: string; const Input1, Input2: PPSInputRegister): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if Input2.Reg = PS_REGISTER_ZERO then
    Result := EmitMov(OutputStr, Input1)
  else
    Result := 'add' + OutputStr + Input1.DisassembleInputRegister(@Self) + ', ' + Input2.DisassembleInputRegister(@Self) + #13#10;
end;

function RPSDisassembleScope.EmitSub(const OutputStr: string; const Input1, Input2: PPSInputRegister): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if Input2.Reg = PS_REGISTER_ZERO then
    Result := EmitMov(OutputStr, Input1)
  else
    Result := 'sub' + OutputStr + Input1.DisassembleInputRegister(@Self) + ', ' + Input2.DisassembleInputRegister(@Self) + #13#10;
end;

function RPSDisassembleScope.EmitMul(const DestRegister: PPSRegisterObject;
  const Input1, Input2: PPSInputRegister): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  Input1ReadMask: string;
  Input2ReadMask: string;
  OutputStr: string;
  MulResult: int;
begin
  if DestRegister.IsAlpha then
  begin
    if (Input1.Channel = PS_CHANNEL_BLUE) then
      Input1ReadMask := '.b'
    else
      Input1ReadMask := '.a';

    if (Input2.Channel = PS_CHANNEL_BLUE) then
      Input2ReadMask := '.b'
    else
      Input2ReadMask := '.a';
  end
  else
  begin
    Input1ReadMask := '';
    Input2ReadMask := '';
  end;

  // Here a half-implemented way to handle PS_COMBINEROUTPUT :
  OutputStr := InstructionOutputCombiner + ' ' + DestRegister.DisassembleRegister(@Self) + OutputWriteMask + ', ';

  Result := '';
  MulResult := Input1.MulResult * Input2.MulResult;
  case MulResult of
    MULRESULT_ZERO,
    MULRESULT_ONE,
    MULRESULT_NEG_ONE,
    MULRESULT_HALF,
    MULRESULT_NEG_HALF:
//      Result := EmitConstant(' ' + DestRegister.DisassembleRegister(@Self) + ', ', MulResult); // Not full OutputStr!
      Result := EmitConstant(OutputStr, MulResult);

    MULRESULT_VARIABLE: // 1 * A or B
      if Input1.Reg = PS_REGISTER_ONE then
        Result := Result + 'mov' + OutputStr + Input2.DisassembleInputRegister(@Self) + Input2ReadMask
      else
        Result := Result + 'mov' + OutputStr + Input1.DisassembleInputRegister(@Self) + Input1ReadMask;
    MULRESULT_NEG_VARIABLE: // -1 * A or B
      if Input1.Reg = PS_REGISTER_NEGATIVE_ONE then
        Result := Result + 'mov' + OutputStr + '-' + Input2.DisassembleInputRegister(@Self) + Input2ReadMask
      else
        Result := Result + 'mov' + OutputStr + '-' + Input1.DisassembleInputRegister(@Self) + Input1ReadMask;
    MULRESULT_HALF_VARIABLE: // 0.5 * A or B
      // TODO : Handle the halving-factor, but for that we need to change the way we apply PS_COMBINEROUTPUT too
      if Input1.Reg = PS_REGISTER_ONE_HALF then
        Result := Result + 'mov' + OutputStr + Input2.DisassembleInputRegister(@Self) + Input2ReadMask
      else
        Result := Result + 'mov' + OutputStr + Input1.DisassembleInputRegister(@Self) + Input1ReadMask;
    MULRESULT_NEG_HALF_VARIABLE: // -0.5 * A or B
      // TODO : Handle the halving-factor, but for that we need to change the way we apply PS_COMBINEROUTPUT too
      if Input1.Reg = PS_REGISTER_NEGATIVE_ONE then
        Result := Result + 'mov' + OutputStr + '-' + Input2.DisassembleInputRegister(@Self) + Input2ReadMask
      else
        Result := Result + 'mov' + OutputStr + '-' + Input1.DisassembleInputRegister(@Self) + Input1ReadMask;
    MULRESULT_MULTIPLY: // A * B
      Result := Result + 'mul' + OutputStr + Input1.DisassembleInputRegister(@Self) + Input1ReadMask + ', ' + Input2.DisassembleInputRegister(@Self) + Input2ReadMask;
  end;

  if Result <> '' then
    Result := Result + #13#10;
end; // EmitMul

function RPSDisassembleScope.EmitMad(const OutputStr: string;
  const Input1, Input2: PPSInputRegister; const Input3: PPSRegisterObject): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if Input3.Reg = PS_REGISTER_ZERO then
    // EmitMul :
    Result := 'mul' + OutputStr
            + Input1.DisassembleInputRegister(@Self) + ', '
            + Input2.DisassembleInputRegister(@Self) + #13#10
  else
    Result := 'mad' + OutputStr
            + Input1.DisassembleInputRegister(@Self) + ', '
            + Input2.DisassembleInputRegister(@Self) + ', '
            + Input3.DisassembleRegister(@Self) + #13#10;
end;

function RPSDisassembleScope.EmitLrp(const OutputStr: string;
  const Input1, Input2, Input3: PPSInputRegister): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := 'lrp' + OutputStr
          + Input1.DisassembleInputRegister(@Self) + ', '
          + Input2.DisassembleInputRegister(@Self) + ', '
          + Input3.DisassembleInputRegister(@Self) + #13#10;
end;

{ RPSCombinerOutput }

function RPSCombinerOutput.IsSameAsAlpha(const Alpha: PPSCombinerOutput): Boolean;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := (Reg = Alpha.Reg)
        and (DotProduct = Alpha.DotProduct)
        and (BlueToAlpha = Alpha.BlueToAlpha)
        and Input1.IsSameAsAlpha(@(Alpha.Input1))
        and Input2.IsSameAsAlpha(@(Alpha.Input2));
end;

procedure RPSCombinerOutput.Decode(Value: Byte; PSInputs: DWORD; aIsAlpha: Boolean);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  inherited Decode(Value, aIsAlpha);

  // Decode PSAlphaInputs / PSRGBInputs :
  Input1.Decode((PSInputs shr  8) and $FF, IsAlpha);
  Input2.Decode((PSInputs shr  0) and $FF, IsAlpha);

  // Ease the decoding stage, by determining the effects of a multiplication between these two inputs :
  MulResult := Input1.MulResult * Input2.MulResult;
end;

function RPSCombinerOutput.CombineStageInputMul(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := aScope.EmitMul(@Self, @Input1, @Input2);
end;

var
  Emitted_dp3: Boolean = False;

function RPSCombinerOutput.CombineStageInputDot(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := 'dp3' + aScope.InstructionOutputCombiner + ' ' +
    Self.DisassembleRegister(aScope) + aScope.OutputWriteMask + ', ' +
    Input1.DisassembleInputRegister(aScope) + ', ' +
    Input2.DisassembleInputRegister(aScope) + #13#10;

  // Because "dp3" needs the color/vector pipeline, no color component outputing opcode can be co-issued with it.
  // So, we need to skip adding '+' after this, by signalling this via the OutputWriteMask :
  Emitted_dp3 := True;
end;

function RPSCombinerOutput.DisassembleCombinerOutput(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if not IsNativeRegWriteable then
  begin
    // Cannot assign to a read-only (probably vertex) register!
    Result := '; Ignored assignment to read-only register (' + Self.DisassembleRegister(aScope) + ')!'#13#10;
    // TODO : We could try to find a free spare register for this case and use that instead,
    // or even mark this output as DISCARD, when there's no further use of this register!
    // (If we do that, we should do it before determining MayPostponeMul, to profit from that optimization.)
    aScope.OutputWriteMask := ''; // Also prevent co-issueing (instead, output a normal instruction)
    Exit;
  end;

  // Handle combining of A and B (doing either a dot-product, or a multiplication) :
  if DotProduct then
    Result := Result + CombineStageInputDot(aScope)
  else
    Result := Result + CombineStageInputMul(aScope);

  if Result <> '' then
  begin
    // Handle blue-to-alpha flag (only valid for RGB) :
    if BlueToAlpha then
      // Note : We can't use the '+ ' prefix, as the blue channel is not determined yet!
      // Note 2: Pixel shader 1.1-1.3 'blue replicate' on source, uses an alpha destination write mask.
      Result := Result + 'mov ' + DisassembleRegister(aScope) + '.a, ' + DisassembleRegister(aScope) + '.b'#13#10;
  end;
end; // DisassembleCombinerOutput

{ RPSCombinerOutputMuxSum }

function RPSCombinerOutputMuxSum.IsSameAsAlpha(const Alpha: PPSCombinerOutputMuxSum): Boolean;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := inherited IsSameAsAlpha(Alpha)
        and OutputAB.IsSameAsAlpha(@(Alpha.OutputAB)) // Also compares InputA and InputB
        and OutputCD.IsSameAsAlpha(@(Alpha.OutputCD)) // Also compares InputC and InputD
end;

function RPSCombinerOutputMuxSum.CombineStageInputMux(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
// Handle PS_COMBINEROUTPUT_AB_CD_MUX, output is MUX(AB,CD) based on R0.a :
var
  SumOutputString: string;
begin
  SumOutputString := aScope.InstructionOutputCombiner + ' ' + Self.DisassembleRegister(aScope) + aScope.OutputWriteMask + ', ';
  // TODO : For all cnd comparisons, handle CombinerMuxesOnMsb=False too!
  // (Maybe the "cmp" opcode works identical to Lsb compares?)

  if (OutputAB.Reg = PS_REGISTER_DISCARD) then
  begin
    if (OutputCD.Reg = PS_REGISTER_DISCARD) then
    begin
      // TODO : Handle ONE*ZERO too!
      if  (OutputAB.Input2.Reg = PS_REGISTER_ONE)
      and (OutputCD.Input2.Reg = PS_REGISTER_ONE) then
        Result := Result + 'cnd' + SumOutputString + 'r0.a, ' +
          // Note : AB and CD reversed like in 'depth.psh' from FocusBlur sample :
          OutputCD.Input1.DisassembleInputRegister(aScope) + ', ' +
          OutputAB.Input1.DisassembleInputRegister(aScope) + #13#10
      else
      if  (OutputAB.Input1.Reg = PS_REGISTER_ONE)
      and (OutputCD.Input2.Reg = PS_REGISTER_ONE) then
        Result := Result + 'cnd' + SumOutputString + 'r0.a, ' +
          OutputAB.Input2.DisassembleInputRegister(aScope) + ', ' +
          OutputCD.Input1.DisassembleInputRegister(aScope) + #13#10
      else
      if  (OutputAB.Input2.Reg = PS_REGISTER_ONE)
      and (OutputCD.Input1.Reg = PS_REGISTER_ONE) then
        Result := Result + 'cnd' + SumOutputString + 'r0.a, ' +
          OutputAB.Input1.DisassembleInputRegister(aScope) + ', ' +
          OutputCD.Input2.DisassembleInputRegister(aScope) + #13#10
      else
      if  (OutputAB.Input1.Reg = PS_REGISTER_ONE)
      and (OutputCD.Input1.Reg = PS_REGISTER_ONE) then
        Result := Result + 'cnd' + SumOutputString + 'r0.a, ' +
          OutputAB.Input2.DisassembleInputRegister(aScope) + ', ' +
          OutputCD.Input2.DisassembleInputRegister(aScope) + #13#10
      else
      begin
        // TODO : We use Sum register as a temp, which could pose a problem if one or more of the inputs use the same register!
//        Result := Result + 'mul' + SumOutputString +
//          OutputAB.Input1.DisassembleInputRegister(aScope) + ', ' +
//          OutputAB.Input2.DisassembleInputRegister(aScope) + #13#10;
        Result := Result + aScope.EmitMul(@Self, @OutputAB.Input1, @OutputAB.Input2);
        Result := Result + aScope.EmitMad(SumOutputString, @OutputCD.Input1, @OutputCD.Input2, @Self);
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
        OutputAB.DisassembleRegister(aScope) + ', ' +
        OutputCD.DisassembleRegister(aScope) + #13#10;
    end;
  end;
end;

function RPSCombinerOutputMuxSum.SumTryLerp(const aScope: PPSDisassembleScope; SumOutputString: string): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  Src0, Src1, Src2: PPSInputRegister;

  function _CanLerp(const Input1, Input2, Input3, Input4: PPSInputRegister): Boolean;
  begin
    Result := (Input1.Reg = Input2.Reg) and (Input1.Multiplier * Input2.Multiplier = -1.0);
    if Result then
    begin
      Src0 := Input1;
      Src1 := Input3;
      Src2 := Input4;
    end;
  end;

begin
  Src0 := nil; Src1 := nil; Src2 := nil;

  if _CanLerp(@OutputAB.Input1, @OutputCD.Input1, @OutputAB.Input2, @OutputCD.Input2)
  or _CanLerp(@OutputAB.Input1, @OutputCD.Input2, @OutputAB.Input2, @OutputCD.Input1)
  or _CanLerp(@OutputAB.Input2, @OutputCD.Input1, @OutputAB.Input1, @OutputCD.Input2)
  or _CanLerp(@OutputAB.Input2, @OutputCD.Input2, @OutputAB.Input1, @OutputCD.Input1) then
    Result := aScope.EmitLrp(SumOutputString, Src0, Src1, Src2)
  else
    Result := '';
end;

function RPSCombinerOutputMuxSum.SumTry4Regs(const aScope: PPSDisassembleScope; SumOutputString: string): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  ABCheck: Boolean;
  CDCheck: Boolean;
  TmpInstructionOutputCombiner: string;
begin
  Result := '';
  // All four inputs need to be read, so find a temporary register and use a two-step simulation.
  // TODO : Scan over the four inputs, and check if the output register (either R0 or R1, the only
  // available temporary registers) is used on only one side (at most). Handle that side first,
  // put the result in the chosen register, and "mad" it to the other side.

  ABCheck := (OutputAB.Input1.Reg = Self.Reg) or (OutputAB.Input2.Reg = Self.Reg);
  CDCheck := (OutputCD.Input1.Reg = Self.Reg) or (OutputCD.Input2.Reg = Self.Reg);
  if ABCheck and CDCheck then
    // We can't use the sum output register, as it's read by both AB and CD.
    // TODO : We could try to find & use a free temporary register or one of
    // the other (writeable) inputs, as long as these aren't used any further.
    Exit;

  // We're going to write two statements to get one result, so make sure that
  // the output modifier is only applied to the second of those two statements :
  TmpInstructionOutputCombiner := aScope.InstructionOutputCombiner;
  aScope.InstructionOutputCombiner := '';

  // We have at least one side NOT reading from the SUM output register;
  if ABCheck then
  begin
    // AB does read from the SUM output register (so CD doesn't),
    // so calculate AB first via the temp register and "mad" it to C*D :
    Result := aScope.EmitMul(@Self, @OutputAB.Input1, @OutputAB.Input2);
    aScope.InstructionOutputCombiner := TmpInstructionOutputCombiner;
    Result := Result + aScope.EmitMad(SumOutputString, @OutputCD.Input1, @OutputCD.Input2, @Self);
  end
  else
  begin
    // Here, we know AB doesn't read from the SUM output register, so we calculate C*D first
    // (no matter if they read from the SUM output register or not) and "mad" it to A*B :
    Result := aScope.EmitMul(@Self, @OutputCD.Input1, @OutputCD.Input2);
    aScope.InstructionOutputCombiner := TmpInstructionOutputCombiner;
    Result := Result + aScope.EmitMad(SumOutputString, @OutputAB.Input1, @OutputAB.Input2, @Self);
  end;
end;

function RPSCombinerOutputMuxSum.SumTry2Regs_1Reg1Fixed(const aScope: PPSDisassembleScope; const SumOutputString: string;
  const Output2Reg, Output1Reg: PPSCombinerOutput): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  // Is A (Output1.Input1) a register?
  if Abs(Output1Reg.Input1.MulResult) >= MULRESULT_VARIABLE then
    // Multiply CD and add A using the "mad" opcode :
    // TODO : B is a fixed value - handle the multiplication factor that implies!
    Result := aScope.EmitMad(SumOutputString, @Output2Reg.Input1, @Output2Reg.Input2, @Output1Reg.Input1)
  else
    // Is B (Output2.Input2) a register ?
    if Abs(Output1Reg.Input2.MulResult) >= MULRESULT_VARIABLE then
      // Multiply CD and add B using the "mad" opcode :
      // TODO : A is a fixed value - handle the multiplication factor that implies!
      Result := aScope.EmitMad(SumOutputString, @Output2Reg.Input1, @Output2Reg.Input2, @Output1Reg.Input2)
    else
    begin
      // AB is a fixed value, so just multiply CD using the "mul" opcode :
      Result := aScope.EmitMul(@Self, @Output2Reg.Input1, @Output2Reg.Input2);
      if Abs(Output1Reg.MulResult) > MULRESULT_ZERO then
        Result := Result + '; Ignored fixed value <> ZERO!'#13#10; // TODO : Somehow, incorporate the fixed value here
    end;
end;

function RPSCombinerOutputMuxSum.CombineStageInputSum(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
// Handle PS_COMBINEROUTPUT_AB_CD_SUM, output is AB+CD (AB and CD can only be multiplied, dotproduct is not possible)
var
  SumOutputString: string;
  ABCheck: Boolean;
  CDCheck: Boolean;
begin
  SumOutputString := aScope.InstructionOutputCombiner + ' ' + Self.DisassembleRegister(aScope) + aScope.OutputWriteMask + ', ';

  // First, test if the outputs where already calculated into a register :
  ABCheck := (OutputAB.Reg <> PS_REGISTER_DISCARD);
  CDCheck := (OutputCD.Reg <> PS_REGISTER_DISCARD);
  if ABCheck and CDCheck then
  begin
    // AB and CD where both calculated, so just "add" these :
    Result := aScope.EmitAddOutputs(SumOutputString, @OutputAB, @OutputCD);
    Exit;
  end;

  if ABCheck or CDCheck then
  begin
    // Either AB or CD was calculated, check which one it was :
    if ABCheck then
      // If AB is already available, "mad" it to C*D :
      Result := aScope.EmitMad(SumOutputString, @OutputCD.Input1, @OutputCD.Input2, @OutputAB)
    else
      // CD is already available, "mad" it to A*B :
      Result := aScope.EmitMad(SumOutputString, @OutputAB.Input1, @OutputAB.Input2, @OutputCD);

    Exit;
  end;

  // Here, AB and CD are both not calculated yet, so we have to do : "sum = (A * B) + (C * D)".
  //
  // Native Direct3D (pixel shader 1.3) has no instruction for this, so we have to simulate
  // the same behaviour, favorably using the least amount of instructions. We can use the
  // 'mad' opcode, which can calculate "sum = (A * B) + C", which is /almost/ enough, except
  // that the D factor is not applied yet with this...
  //
  // There is one case that can be done by one instruction : the "lerp" - linear interpolation.
  // For this, one input appears on both sides, but with an inverted sign. This can be simulated
  // using the "lrp" opcode.
  //
  // But when four separate inputs are read, we have no choice but to use a temporary register.
  // As long as the Sum ouputs to a register that's never read, or only read on one side,
  // we can use that one using the sequence "sum=C*D", "sum=(A*B)+sum". This takes two opcodes,
  // but at least it's a solution. If there's no such register, we could look for another free
  // register but the odds are slim - we probably won't find one (there's only two available!)
  // so we can't solve that situation reliably.
  //
  // In some cases however, exactly one of the inputs is a fixed value (often, it's ONE),
  // so we only have to read from 3 inputs (which fits nicely in the "mad" opcode).
  //
  // In other cases, both sides use a fixed value, so we only have to "add" these together.
  //
  // There are even simpler cases, when one side ("A*B" or "C*D") is effectively ZERO,
  // we could ignore that side and just calculate the other side, using the "mul" opcode.
  //
  // The most simple case would be when one side is ZERO, and the other reads just one
  // register. We can solve that using a "mov" opcode. (Unless the register is moved into
  // itself, which would become a no-op!)
  //
  // What complicates all this, is that both the input & output can be decorated with
  // modifiers, which effectively apply a multiplication and a correction to the input
  // registers and/or final resulting value. In all 4 previously described cases, the
  // input-modifiers should be correctly applied.

  // Test if the outputs are a result of two registers being multiplied :
  ABCheck := (OutputAB.MulResult = MULRESULT_MULTIPLY);
  CDCheck := (OutputCD.MulResult = MULRESULT_MULTIPLY);
  if ABCheck and CDCheck then
  begin
    // AB and CD are using registers for both inputs

    // Check for "lerp = (src0 * src1) + ((1-src0) * src2)" :
    Result := SumTryLerp(aScope, SumOutputString);
    if Result <> '' then
      Exit;

    Result := SumTry4Regs(aScope, SumOutputString);

    if Result = '' then
      // Note : After these tries, there's no other solution, so output that fact and exit :
      Result := '; Can''t find a register for the intermediate result of (A*B)+(C*D)'#13#10;

    Exit;
  end;

  // Here at least one input is a fixed value, which leaves us up to 3 registers to handle.
  // It would be best to use the least amount of opcodes, so we handle each case separately.

  if ABCheck or CDCheck then
  begin
    // AB or CD are using registers for both inputs
    if ABCheck then
      // If AB is the side that uses 2 registers, handle it that way :
      Result := SumTry2Regs_1Reg1Fixed(aScope, SumOutputString, @OutputAB, @OutputCD)
    else
      // Otherwise CD is the side that uses 2 registers, handle it that way :
      Result := SumTry2Regs_1Reg1Fixed(aScope, SumOutputString, @OutputCD, @OutputAB);

    Exit;
  end;

  // Reg*Reg multiplications are handled above, so check the remaining cases :

  // Test if the outputs read one variable (not two, that was handled above):
  ABCheck := (Abs(OutputAB.MulResult) >= MULRESULT_VARIABLE);
  CDCheck := (Abs(OutputCD.MulResult) >= MULRESULT_VARIABLE);
  if ABCheck and CDCheck then
  begin
    // Handle cases like this example : sum R0 = (T0*ONE) + (-C0*ONE)
    // or sum R0 = (R0*ONE) + (R1*NEG_ONE)
    // TODO : Check where the registers are, and also apply all possible input mappings!
    // For now, we assume the fixed values are in B and D (so we add registers A and C) :
    if (OutputCD.MulResult < 0) then
      Result := aScope.EmitSub(SumOutputString, @OutputAB.Input1, @OutputCD.Input1)
    else
      Result := aScope.EmitAdd(SumOutputString, @OutputAB.Input1, @OutputCD.Input1);

    Exit;
  end;

  if ABCheck or CDCheck then
  begin
//    // AB or CD are using registers for one inputs
//    if ABCheck then
//      // If AB is the side that uses 2 registers, handle it that way :
//      Result := Try1Reg1Fixed(aScope, SumOutputString, @OutputAB, @OutputCD)
//    else
//      // Otherwise CD is the side that uses 2 registers, handle it that way :
//      Result := Try1Reg1Fixed(aScope, SumOutputString, @OutputCD, @OutputAB);
    Result := '; TODO : Unhandled case : Try1Reg1Fixed'#13#10;
    Exit;
  end;

  // Test if the outputs result in a fixed value :
  ABCheck := (Abs(OutputAB.MulResult) < MULRESULT_VARIABLE);
  CDCheck := (Abs(OutputCD.MulResult) < MULRESULT_VARIABLE);
  if ABCheck and CDCheck then
  begin
    // Both outputs are a fixed value - calculate what that amounts to, and generate code
    // that puts that fixed value in the output.
    Result := aScope.EmitConstant(SumOutputString, OutputAB.MulResult + OutputCD.MulResult) + #13#10;
    // TODO : The constant can range from -0.5*0.5 = -0.25 up to 1.0*1.0 = 2.0 (which is
    // not even counting the input register modifier!) So it could mean we have to do more
    // that just the above! (Like "add"-ing the result to itself, taking an extra opcode.)
    Exit;
  end;

  if ABCheck or CDCheck then
  begin
    // AB or CD is a fixed value - calculate what that amounts to, and generate code
    if ABCheck then
    begin
      // AB is a fixed value, calculate the impact on C*D and emit that
      Result := '; TODO : Calc fixed AB value and add that to CD'#13#10;
      // TODO : Emit "mul SUM, C, D"??
    end
    else
    begin
      // CD is a fixed value, calculate the impact on A*B and emit that
      Result := '; TODO : Calc fixed CD value and add that to AB'#13#10;
      // TODO : Emit "mul SUM, A, B"
    end;

    Exit;
  end;

  Result := '; TODO : Reached end of CombineStageInputSum without hitting any handling code!'#13#10;
end; // CombineStageInputSum

{ RPSCombinerStageChannel }

procedure RPSCombinerStageChannel.Decode(PSInputs, PSOutputs: DWORD; IsAlpha: Boolean = False);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  // Get the combiner output flags :
  CombinerOutputFlags := PS_COMBINEROUTPUT(PSOutputs shr 12);

  // Decompose the combiner output flags :
  OutputSUM.OutputAB.DotProduct := (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_DOT_PRODUCT) > 0; // False=Multiply, True=DotProduct
  OutputSUM.OutputCD.DotProduct := (CombinerOutputFlags and PS_COMBINEROUTPUT_CD_DOT_PRODUCT) > 0; // False=Multiply, True=DotProduct

  OutputSUM.OutputAB.BlueToAlpha := (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA) > 0; // False=Alpha-to-Alpha, True=Blue-to-Alpha
  OutputSUM.OutputCD.BlueToAlpha := (CombinerOutputFlags and PS_COMBINEROUTPUT_CD_BLUE_TO_ALPHA) > 0; // False=Alpha-to-Alpha, True=Blue-to-Alpha

  // Decode PSAlphaOutputs / PSRGBOutputs and PSAlphaInputs / PSRGBInputs :
  OutputSUM.OutputAB.Decode((PSOutputs shr 4) and $F, (PSInputs shr 16) and $FFFF, IsAlpha);
  OutputSUM.OutputCD.Decode((PSOutputs shr 0) and $F, (PSInputs shr  0) and $FFFF, IsAlpha);
  OutputSUM.Decode((PSOutputs shr 8) and $F, IsAlpha);

  AB_CD_SUM := (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_CD_MUX) = 0; // True=AB+CD, False=MUX(AB,CD) based on R0.a
(*
  // Ease the emulation by convering the combiner output flags to a multiplier & correction
  case PS_COMBINEROUTPUT(Ord(CombinerOutputFlags) and $38) of
    PS_COMBINEROUTPUT_IDENTITY:         begin Multiplier := 1.0; Correction := +0.0; end; // y = x
    PS_COMBINEROUTPUT_BIAS:             begin Multiplier := 1.0; Correction := -0.5; end; // y = x - 0.5
    PS_COMBINEROUTPUT_SHIFTLEFT_1:      begin Multiplier := 2.0; Correction := +0.0; end; // y = x*2
    PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS: begin Multiplier := 2.0; Correction := -1.0; end; // y = (x - 0.5)*2 = x*2 - 1.0
    PS_COMBINEROUTPUT_SHIFTLEFT_2:      begin Multiplier := 4.0; Correction := +0.0; end; // y = x*4
    PS_COMBINEROUTPUT_SHIFTRIGHT_1:     begin Multiplier := 0.5; Correction := +0.0; end; // y = x/2 = x*0.5
  end;
*)
end;

// Checks if this (RGB) stage is identical to the Alhpa stage.
function RPSCombinerStageChannel.IsSameAsAlpha(const Alpha: PPSCombinerStageChannel): Boolean;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := OutputSUM.IsSameAsAlpha(@(Alpha.OutputSUM)); // Also compares OutputAB and OutputCD
end;

function RPSCombinerStageChannel.DisassembleCombinerStageChannel(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  MayPostponeMul: Boolean;
begin
  Result := '';

  // Note : On a hardware level, there are only 4 pixel shaders instructions present in the Nvidia NV2A GPU :
  // - xdd (dot/dot/discard) > calculating AB=A.B and CD=C.D
  // - xdm (dot/mul/discard) > calculating AB=A.B and CD=C*D
  // - xmmc (mul/mul/mux)    > calculating AB=A*B and CD=C*D and Mux=AB?CD
  // - xmma (mul/mul/sum)    > calculating AB=A*B and CD=C*D and Sum=AB+CD
  // (One of the implications is, that once a dot-product is issued, no Sum or Mux operation is possible.)
  // All other instructions (mov, add, sub, mul, lrp, dp3) are compiled into one of these 4 using varying arguments.
  // All 4 instruction specify up to three output registers, all of which must be unique (or be discarded).
  //
  // Apart from the r0,r1 and t0-t3 registers, the NV2A allows writing to the v0,v1 (this conflicts with PS.1.3!)
  //
  // The precision of registers is also different; On the Xbox, all 4 color components (RGBA) for constant registers
  // range from 0.0 to 1.0 (with 8 bits of precision), while all other registers (r, t and v) range from -1.0 to 1.0.
  //
  // This is different from native PS.1.3 in which constant registers suddenly have a range -1.0 to 1.0, but vertex
  // registers (v0 and v1) range from 0.0 to 1.0 instead, and the temporary and texture registers have a range
  // from negative 'MaxPixelShaderValue' to positive 'MaxPixelShaderValue', which value must at least be 1.0
  // (but depending on hardware capabilities can be higher).
  //
  // TODO : Correct emulation should correct these differences; The range of constant-registers must be converted
  // from 0.0-1.0 to -1.0-1.0, and vertex-registers must be converted from -1.0..1.0 to 0.0..1.0 (if anything like
  // that is at all possible!)
  //
  // register | Xbox range | Native range | Xbox      | Native    |
  //  C0..C8  |  0.0 - 1.0 |   -1.0 - 1.0 | readonly  | readonly  |
  //  R0..R1  | -1.0 - 1.0 |   -1.0 - 1.0 | writeable | writeable |
  //  T0..T3  | -1.0 - 1.0 |   -1.0 - 1.0 | writeable | writeable |
  //  V0..V1  | -1.0 - 1.0 |    0.0 - 1.0 | writeable | readonly  |
  //
  // "-C0_bias_x2" shifts range from [ 0..1] to [-1..1]
  // "-V0_bias_d2" shifts range from [-1..1] to [ 0..1]

  // Convert the CombinerOutput flag to a InstructionOutputCombiner
  // or an SourceRegisterModifier (so far as that's possible):
  aScope.InstructionOutputCombiner := '';
  aScope.SourceRegisterModifier := '';
  case PS_COMBINEROUTPUT(Ord(CombinerOutputFlags) and $38) of
    PS_COMBINEROUTPUT_IDENTITY:         aScope.InstructionOutputCombiner := '';         // y = x
    PS_COMBINEROUTPUT_BIAS:             aScope.SourceRegisterModifier    := '_bias';    // y = x - 0.5
    PS_COMBINEROUTPUT_SHIFTLEFT_1:      aScope.InstructionOutputCombiner := '_x2';      // y = x*2
    PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS: aScope.SourceRegisterModifier    := '_bias_x2'; // y = (x - 0.5)*2 = x*2 - 1.0
    PS_COMBINEROUTPUT_SHIFTLEFT_2:      aScope.InstructionOutputCombiner := '_x4';      // y = x*4
    PS_COMBINEROUTPUT_SHIFTRIGHT_1:     aScope.InstructionOutputCombiner := '_d2';      // y = x/2 = x*0.5
  end;

  // In Sum-mode, we might be able to skip (one) multiplication, if we can postpone it to the Sum step :
  MayPostponeMul := AB_CD_SUM and (OutputSUM.Reg > PS_REGISTER_DISCARD);

  // Do we need to calculate AB ?
  if OutputSUM.OutputAB.Reg > PS_REGISTER_DISCARD then
  begin
    // If the result is a multiplication and goes to the same register as the Sum target register,
    // we don't output A*B just yet, as it could be done more efficient later :
    if MayPostponeMul and (OutputSUM.OutputAB.Reg = OutputSUM.Reg) then
    begin
      // This one occurance was encountered, ignore AB for now (and don't postpone CD calculation!) :
      OutputSUM.OutputAB.Reg := PS_REGISTER_DISCARD;
      MayPostponeMul := False;
    end
    else
      Result := Result + OutputSUM.OutputAB.DisassembleCombinerOutput(aScope);
  end;

  // Do we need to calculate CD ?
  if OutputSUM.OutputCD.Reg > PS_REGISTER_DISCARD then
  begin
    // If the result is a multiplication and goes to the same register as the Sum target register,
    // we don't output C*D just yet, as it could be done more efficient later :
    if MayPostponeMul and (OutputSUM.OutputCD.Reg = OutputSUM.Reg) then
    begin
      // unused MaySkip := False;
      OutputSUM.OutputCD.Reg := PS_REGISTER_DISCARD;
    end
    else
      Result := Result + OutputSUM.OutputCD.DisassembleCombinerOutput(aScope);
  end;

  if Emitted_dp3 then
  begin
    aScope.OutputWriteMask := '';
    Emitted_dp3 := False;
  end;

  // Do we need to calculate SUM ?
  if OutputSUM.Reg > PS_REGISTER_DISCARD then
  begin
    if AB_CD_SUM then
      Result := Result + OutputSUM.CombineStageInputSum(aScope)
    else
      Result := Result + OutputSUM.CombineStageInputMux(aScope);
  end;
end; // DisassembleCombinerStageChannel

{ RPSCombinerStage }

// Convert each stage's output-from-input mappings, back to PC-compatible pixel shader instructions
function RPSCombinerStage.DisassembleCombinerStage(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  StageOutputStrRGB: string;
  StageOutputStrAlpha: string;
begin
  aScope.C0RegStr := 'c' + IntToStr(C0Mapping);
  aScope.C1RegStr := 'c' + IntToStr(C1Mapping);

  Result := '; combine stage ' + IntToStr(aScope.Stage) + #13#10;

  // Check if RGB and Alpha are handled identical :
  if RGB.IsSameAsAlpha(@Alpha) then
  begin
    // In that case, we combine both channels in one go without Output Write Masks (which defaults to '.rgba') :
    aScope.OutputWriteMask := '';
    Result := Result + RGB.DisassembleCombinerStageChannel(aScope);
    Exit;
  end;

  // Else, handle RGB separately from Alpha :
  aScope.OutputWriteMask := '.rgb';
  StageOutputStrRGB := RGB.DisassembleCombinerStageChannel(aScope);
  Result := Result + StageOutputStrRGB;

  // If someone (like CombineStageInputDot) cleared the output-writemask (disabling co-issueing)
  if aScope.OutputWriteMask = '' then
    // Disable the following co-issueing with the Alpha channel :
    StageOutputStrRGB := '';

  // Now, generate the Alpha channel :
  aScope.OutputWriteMask := '.a';
  StageOutputStrAlpha := Alpha.DisassembleCombinerStageChannel(aScope);

  // And output it (only if Alpha output is not identical to RGB) :
  // TODO : This test needs improvement!
  if StageOutputStrAlpha <> '' then
  begin
    // Co-issue the Alpha channel together with the RGB channel (if that's still possible) :
    if (StageOutputStrRGB <> '') and (StageOutputStrAlpha[1] <> ';') then
      Result := Result + '+';

    Result := Result + StageOutputStrAlpha;
  end;
end;

{ RPSFinalCombiner }

procedure RPSFinalCombiner.Decode(const PSFinalCombinerInputsABCD, PSFinalCombinerInputsEFG, PSFinalCombinerConstants: DWORD);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  InputA.Decode((PSFinalCombinerInputsABCD shr 24) and $FF, {IsAlpha=}False);
  InputB.Decode((PSFinalCombinerInputsABCD shr 16) and $FF, {IsAlpha=}False);
  InputC.Decode((PSFinalCombinerInputsABCD shr  8) and $FF, {IsAlpha=}False);
  InputD.Decode((PSFinalCombinerInputsABCD shr  0) and $FF, {IsAlpha=}False);

  InputE.Decode((PSFinalCombinerInputsEFG  shr 24) and $FF, {IsAlpha=}False);
  InputF.Decode((PSFinalCombinerInputsEFG  shr 16) and $FF, {IsAlpha=}False);
  InputG.Decode((PSFinalCombinerInputsEFG  shr  8) and $FF, {IsAlpha=}False);
  FinalCombinerFlags := PS_FINALCOMBINERSETTING((PSFinalCombinerInputsEFG shr 0) and $FF);

  FinalCombinerC0Mapping := (PSFinalCombinerConstants shr 0) and $F;
  FinalCombinerC1Mapping := (PSFinalCombinerConstants shr 4) and $F;
  dwPS_GLOBALFLAGS := (PSFinalCombinerConstants shr 8) and $1;
end;

function RPSFinalCombiner.DisassembleFinalCombiner(const aScope: PPSDisassembleScope): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  TmpReg: PS_REGISTER;
  AlreadyHandled_D: Boolean;
  SumOutputString: string;
begin
  Result := '';

  aScope.C0RegStr := 'c' + IntToStr(FinalCombinerC0Mapping);
  aScope.C1RegStr := 'c' + IntToStr(FinalCombinerC1Mapping);

// Note : The sign bit is lost upon input to the final combiner!

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
    Result := Result + '; final combiner - FOG not emulated, using 1.'#13#10;
//    aScope.FogReg := PS_REGISTER_C0; // TODO : Detect this beforehand and use a constant register (if one is available!)
    if (InputA.Reg = PS_REGISTER_FOG) then
      InputA.Reg := PS_REGISTER_ONE;
    if (InputB.Reg = PS_REGISTER_FOG) then
      InputB.Reg := PS_REGISTER_ONE;
    if (InputC.Reg = PS_REGISTER_FOG) then
      InputC.Reg := PS_REGISTER_ONE;
    if (InputD.Reg = PS_REGISTER_FOG) then
      InputD.Reg := PS_REGISTER_ONE;
    if (InputE.Reg = PS_REGISTER_FOG) then
      InputE.Reg := PS_REGISTER_ONE;
    if (InputF.Reg = PS_REGISTER_FOG) then
      InputF.Reg := PS_REGISTER_ONE;
    if (InputG.Reg = PS_REGISTER_FOG) then
      InputG.Reg := PS_REGISTER_ONE;
  end;

  // Handle PS_REGISTER_EF_PROD :
  aScope.EFReg := PS_REGISTER_ZERO;
  if (InputE.Reg > PS_REGISTER_ZERO) and (InputF.Reg > PS_REGISTER_ZERO) then
  begin
    Result := Result + '; final combiner - E*F'#13#10;
    if  (InputE.Reg = PS_REGISTER_ONE)
    and (InputF.Reg = PS_REGISTER_ONE) then
      aScope.EFReg := PS_REGISTER_ONE
    else
      if (InputE.Reg = PS_REGISTER_R0)
      or (InputF.Reg = PS_REGISTER_R0) then
        aScope.EFReg := PS_REGISTER_R0
      else
        if (InputE.Reg = PS_REGISTER_R1)
        or (InputF.Reg = PS_REGISTER_R1) then
          aScope.EFReg := PS_REGISTER_R1
        else
          ; // TODO : See if R0 or R1 is available - use it for E*F or stop

    if aScope.EFReg <= PS_REGISTER_R1 then
      Result := Result + 'mul ' + PSRegToStr(aScope, aScope.EFReg) + ', ' + InputE.DisassembleInputRegister(aScope) + ', ' + InputF.DisassembleInputRegister(aScope) + #13#10
//    Result := Result + aScope.EmitMul(PSRegToStr(aScope, aScope.EFReg), @InputE, @InputF)
    else
      ; // TODO : How should we handle EFReg PS_REGISTER_ONE ?
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

    // Handle the V1R0 flags :
    if (FinalCombinerFlags and PS_FINALCOMBINERSETTING_CLAMP_SUM) > 0 then
      // V1+R0 sum clamped to [0,1]
      Result := Result + 'add_sat '
    else
      Result := Result + 'add ';

    Result := Result + PSRegToStr(aScope, aScope.V1R0Reg) + ', ';

    if (FinalCombinerFlags and PS_FINALCOMBINERSETTING_COMPLEMENT_V1) > 0 then
      // unsigned invert mapping  (1 - v1) is used as an input to the sum rather than v1
      Result := Result + '1-';
    Result := Result + PSRegToStr(aScope, PS_REGISTER_V1) + ', ';

    if (FinalCombinerFlags and PS_FINALCOMBINERSETTING_COMPLEMENT_R0) > 0 then
      // unsigned invert mapping  (1 - r0) is used as an input to the sum rather than r0
      Result := Result + '1-';
    Result := Result + PSRegToStr(aScope, PS_REGISTER_R0) + #13#10;
  end;

  // Handle the final combiner's linear interpolation :
  Result := Result + '; final combiner - r0 = A*B + (1-A)*C + D'#13#10;

  TmpReg := PS_REGISTER_R0; // TODO : Check if r0 is usable
  AlreadyHandled_D := True; // Assume D is handled

//  SumOutputString := aScope.InstructionOutputCombiner + ' ' + Self.DisassembleRegister(aScope) + aScope.OutputWriteMask + ', ';
  SumOutputString := ' ' + PSRegToStr(aScope, TmpReg) + ', ';

  if  ((InputA.Reg = PS_REGISTER_ZERO) or (InputA.Reg = PS_REGISTER_R0))
  and ((InputB.Reg = PS_REGISTER_ZERO) or (InputB.Reg = PS_REGISTER_R0))
  and ((InputC.Reg = PS_REGISTER_ZERO) or (InputC.Reg = PS_REGISTER_R0)) then
    // do nothing TODO : Is this correct???
  else
  begin
    if InputC.Reg = PS_REGISTER_ZERO then
    begin
      if (InputA.Reg = PS_REGISTER_ONE) then
      begin
        // r0 = B + D
        Result := Result + aScope.EmitAdd(SumOutputString, @InputB, @InputD);
      end
      else if (InputB.Reg = PS_REGISTER_ONE) then
      begin
        // r0 = A + D
        Result := Result + aScope.EmitAdd(SumOutputString, @InputA, @InputD);
      end
      else
      begin
        // r0 = A*B + D
//        Result := Result + 'mad ' + PSRegToStr(aScope, TmpReg) + ', ' + InputA.DisassembleInputRegister(aScope) + ', ' + InputB.DisassembleInputRegister(aScope) + ', ' + InputD.DisassembleInputRegister(aScope) + #13#10;
        Result := Result + aScope.EmitMad(SumOutputString, @InputA, @InputB, @InputD);
      end;
    end
    else
    if InputB.Reg = PS_REGISTER_ZERO then
    begin
      // r0 = (1-A)*C + D
      Result := Result + 'mad ' + PSRegToStr(aScope, TmpReg) + ', 1-' + InputA.DisassembleInputRegister(aScope) + ', ' + InputC.DisassembleInputRegister(aScope) + ', ' + InputD.DisassembleInputRegister(aScope) + #13#10;
    end
    else
    begin
      if (InputA.Reg = PS_REGISTER_ONE) then
      begin
        if InputD.Reg = PS_REGISTER_ZERO then
          // r0 = B
          Result := Result + 'mov r0, ' + InputB.DisassembleInputRegister(aScope) + #13#10
        else
          // r0 = B + D
          Result := Result + 'add r0, ' + InputB.DisassembleInputRegister(aScope) + ', ' + InputD.DisassembleInputRegister(aScope) + #13#10;

        // Reset D - already handled :
        AlreadyHandled_D := True;
      end
      else
      begin
        // r0 = A*B + (1-A)*C + D
        Result := Result + 'lrp ' + PSRegToStr(aScope, TmpReg) + ', ' + InputA.DisassembleInputRegister(aScope) + ', ' + InputB.DisassembleInputRegister(aScope) + ', ' + InputC.DisassembleInputRegister(aScope) + #13#10;

        // Reset D - already handled :
        AlreadyHandled_D := False;
      end;
    end;
  end;

  if (TmpReg = PS_REGISTER_ZERO) or (TmpReg = PS_REGISTER_R0) then
  begin
    if (InputD.Reg = PS_REGISTER_ZERO) or (InputD.Reg = PS_REGISTER_R0) or AlreadyHandled_D then
      // do nothing
    else
      Result := Result + 'mov r0.rgb, ' + InputD.DisassembleInputRegister(aScope) + #13#10;
  end
  else
  begin
    if AlreadyHandled_D then
      Result := Result + 'mov r0.rgb, ' + PSRegToStr(aScope, TmpReg) + #13#10
    else
      Result := Result + 'add r0.rgb, ' + PSRegToStr(aScope, TmpReg) + ', ' + InputD.DisassembleInputRegister(aScope) + #13#10;
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
      Result := Result + 'mov r0.a, ' + InputG.DisassembleInputRegister(aScope) + #13#10;
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

  CombinerMuxesOnMsb := (CombinerCountFlags and PS_COMBINERCOUNT_MUX_MSB) > 0;
  CombinerHasUniqueC0 := (CombinerCountFlags and PS_COMBINERCOUNT_UNIQUE_C0) > 0;
  CombinerHasUniqueC1 := (CombinerCountFlags and PS_COMBINERCOUNT_UNIQUE_C1) > 0;

  for i := 0 to X_PSH_COMBINECOUNT - 1 do
  begin
    Combiners[i].RGB.Decode(pPSDef.PSRGBInputs[i], pPSDef.PSRGBOutputs[i]);
    Combiners[i].Alpha.Decode(pPSDef.PSAlphaInputs[i], pPSDef.PSAlphaOutputs[i], {IsAlpha=}True);

    // Decode & map the C0 and C1 registers :
    if CombinerHasUniqueC0 then
      Combiners[i].C0Mapping := i // X_D3DRS_PSCONSTANT0_0..X_D3DRS_PSCONSTANT0_7
    else
      Combiners[i].C0Mapping := 0;

    if CombinerHasUniqueC1 then
      Combiners[i].C1Mapping := i + 8 // X_D3DRS_PSCONSTANT1_0..X_D3DRS_PSCONSTANT1_7
    else
      Combiners[i].C1Mapping := 8;
  end;

  FinalCombiner.Decode(pPSDef.PSFinalCombinerInputsABCD, pPSDef.PSFinalCombinerInputsEFG, pPSDef.PSFinalCombinerConstants);
end;

function RPSIntermediate.IntermediateToString(): string;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
// print relevant contents to the debug console

  procedure _AddStr(const aStr: string); overload;
  begin
    Result := Result + aStr + #13#10;
  end;

  procedure _AddStr(const aStr: string; Args: array of const); overload;
  begin
    _AddStr(DxbxFormat(aStr, Args));
  end;

var
  i: int;
begin
  Result := '';
  // Show the contents to the user
  _AddStr(#13#10'-----PixelShader Def Contents-----');
  _AddStr(OriginalToString());

  if (Original.PSTextureModes > 0) then
  begin
    _AddStr(#13#10'PSTextureModes ->'); // Texture addressing modes
    _AddStr('Stage 0: %s', [PS_TextureModesStr[PSTextureModes[0]]]);
    _AddStr('Stage 1: %s', [PS_TextureModesStr[PSTextureModes[1]]]);
    _AddStr('Stage 2: %s', [PS_TextureModesStr[PSTextureModes[2]]]);
    _AddStr('Stage 3: %s', [PS_TextureModesStr[PSTextureModes[3]]]);
  end;

  if (Original.PSDotMapping > 0) then // Input mapping for dot product modes
  begin
    _AddStr(#13#10'PSDotMapping ->');
    _AddStr('Stage 1: %s', [PS_DotMappingStr[PSDotMapping[1]]]);
    _AddStr('Stage 2: %s', [PS_DotMappingStr[PSDotMapping[2]]]);
    _AddStr('Stage 3: %s', [PS_DotMappingStr[PSDotMapping[3]]]);
  end;

  if (Original.PSCompareMode > 0) then // Compare modes for clipplane texture mode
  begin
    _AddStr(#13#10'PSCompareMode ->');
    _AddStr('Stage 0: %s', [PS_CompareModeStr[iif(PSCompareMode[0] = 0, 0, 1)]]);
    _AddStr('Stage 1: %s', [PS_CompareModeStr[iif(PSCompareMode[1] = 0, 2, 3)]]);
    _AddStr('Stage 2: %s', [PS_CompareModeStr[iif(PSCompareMode[2] = 0, 4, 5)]]);
    _AddStr('Stage 3: %s', [PS_CompareModeStr[iif(PSCompareMode[3] = 0, 6, 7)]]);
  end;

  if (Original.PSInputTexture > 0) then // Texture source for some texture modes
  begin
    _AddStr(#13#10'PSInputTexture ->');
    _AddStr('Stage 1: %d', [PSInputTexture[1]]);
    _AddStr('Stage 2: %d', [PSInputTexture[2]]);
    _AddStr('Stage 3: %d', [PSInputTexture[3]]);
  end;

  if (Original.PSCombinerCount > 0) then // Active combiner count (Stages 0-7)
  begin
    _AddStr(#13#10'PSCombinerCount ->');
    _AddStr('Combiners: %d', [NumberOfCombiners]);
    _AddStr('Mux:       %s', [PS_CombinerCountFlagsStr[iif(CombinerCountFlags and PS_COMBINERCOUNT_MUX_MSB = 0, 0, 1)]]);
    _AddStr('C0:        %s', [PS_CombinerCountFlagsStr[iif(CombinerCountFlags and PS_COMBINERCOUNT_UNIQUE_C0 = 0, 2, 3)]]);
    _AddStr('C1:        %s', [PS_CombinerCountFlagsStr[iif(CombinerCountFlags and PS_COMBINERCOUNT_UNIQUE_C1 = 0, 4, 5)]]);
  end;

  // Dxbx additions from here onwards :

  if NumberOfCombiners > 0 then
  for i := 0 to NumberOfCombiners-1 do // Loop over all combiner stages
  begin
    _AddStr(#13#10);

    _AddStr('PSRGBOutputs[%d] AB: %s', [i, Combiners[i].RGB.OutputSUM.OutputAB.IntermediateToString()]);
    _AddStr('PSRGBOutputs[%d] CD: %s', [i, Combiners[i].RGB.OutputSUM.OutputCD.IntermediateToString()]);
    _AddStr('PSRGBOutputs[%d] SUM: %s', [i, Combiners[i].RGB.OutputSUM.IntermediateToString()]);
    _AddStr('PSRGBOutputs[%d] flags: %s', [i, PSCombinerOutputFlagsToStr(Combiners[i].RGB.CombinerOutputFlags, {IsAlpha=}False)]);

    _AddStr(#13#10);
    _AddStr('PSRGBInputs[%d] A: %s', [i, Combiners[i].RGB.OutputSUM.OutputAB.Input1.IntermediateToString()]);
    _AddStr('PSRGBInputs[%d] B: %s', [i, Combiners[i].RGB.OutputSUM.OutputAB.Input2.IntermediateToString()]);
    _AddStr('PSRGBInputs[%d] C: %s', [i, Combiners[i].RGB.OutputSUM.OutputCD.Input1.IntermediateToString()]);
    _AddStr('PSRGBInputs[%d] D: %s', [i, Combiners[i].RGB.OutputSUM.OutputCD.Input2.IntermediateToString()]);

    _AddStr(#13#10);
    _AddStr('PSAlphaOutputs[%d] AB: %s', [i, Combiners[i].Alpha.OutputSUM.OutputAB.IntermediateToString()]);
    _AddStr('PSAlphaOutputs[%d] CD: %s', [i, Combiners[i].Alpha.OutputSUM.OutputCD.IntermediateToString()]);
    _AddStr('PSAlphaOutputs[%d] SUM: %s', [i, Combiners[i].Alpha.OutputSUM.IntermediateToString()]);
    _AddStr('PSAlphaOutputs[%d] flags: %s', [i, PSCombinerOutputFlagsToStr(Combiners[i].Alpha.CombinerOutputFlags, {IsAlpha=}True)]);

    _AddStr(#13#10);
    _AddStr('PSAlphaInputs[%d] A: %s', [i, Combiners[i].Alpha.OutputSUM.OutputAB.Input1.IntermediateToString()]);
    _AddStr('PSAlphaInputs[%d] B: %s', [i, Combiners[i].Alpha.OutputSUM.OutputAB.Input2.IntermediateToString()]);
    _AddStr('PSAlphaInputs[%d] C: %s', [i, Combiners[i].Alpha.OutputSUM.OutputCD.Input1.IntermediateToString()]);
    _AddStr('PSAlphaInputs[%d] D: %s', [i, Combiners[i].Alpha.OutputSUM.OutputCD.Input2.IntermediateToString()]);

    _AddStr(#13#10);
    _AddStr('PSConstant0[%d] : %x', [i, Original.PSConstant0[i]]); // C0 for each stage
    _AddStr('PSConstant1[%d] : %x', [i, Original.PSConstant1[i]]); // C1 for each stage
  end;

  if (Original.PSFinalCombinerInputsABCD > 0)
  or (Original.PSFinalCombinerInputsEFG  > 0) then // Final combiner inputs
  begin
    _AddStr(#13#10'PSFinalCombinerConstant0 : %x', [Original.PSFinalCombinerConstant0]); // C0 in final combiner
    _AddStr('PSFinalCombinerConstant1 : %x', [Original.PSFinalCombinerConstant1]); // C1 in final combiner

    _AddStr(#13#10'PSFinalCombinerInputsABCD ->');
    _AddStr('Input A: %s', [FinalCombiner.InputA.IntermediateToString()]);
    _AddStr('Input B: %s', [FinalCombiner.InputB.IntermediateToString()]);
    _AddStr('Input C: %s', [FinalCombiner.InputC.IntermediateToString()]);
    _AddStr('Input D: %s', [FinalCombiner.InputD.IntermediateToString()]);

    _AddStr(#13#10'PSFinalCombinerInputsEFG ->');
    _AddStr('Input E: %s', [FinalCombiner.InputE.IntermediateToString()]);
    _AddStr('Input F: %s', [FinalCombiner.InputF.IntermediateToString()]);
    _AddStr('Input G: %s', [FinalCombiner.InputG.IntermediateToString()]);
    _AddStr('Final combiner setting: %s', [PSFinalCombinerSettingToStr(Ord(FinalCombiner.FinalCombinerFlags))]);

    _AddStr(#13#10'PSFinalCombinerConstants ->'); // Final combiner constant mapping
    _AddStr('Offset of D3D constant for C0: %d', [FinalCombiner.FinalCombinerC0Mapping]);
    _AddStr('Offset of D3D constant for C1: %d', [FinalCombiner.FinalCombinerC1Mapping]);
    _AddStr('Adjust texture flag: %s', [PS_GlobalFlagsStr[PS_GLOBALFLAGS(FinalCombiner.dwPS_GLOBALFLAGS)]]);
  end;

(* TODO :
  // These last DWORDs are used to define how Direct3D8 pixel shader constants map to the constant
  // registers in each combiner stage. They are used by the Direct3D run-time software but not by the hardware.
  PSC0Mapping: DWORD,                      // Mapping of c0 regs to D3D constants
  PSC1Mapping: DWORD,                      // Mapping of c1 regs to D3D constants
*)

  _AddStr(#13#10);
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
{$IFNDEF DXBX_USE_D3D9}
    PS_TEXTUREMODES_PROJECT2D: Result := 'tex';
    PS_TEXTUREMODES_PROJECT3D: Result := 'tex'; // Note : 3d textures are sampled using PS_TEXTUREMODES_CUBEMAP
    PS_TEXTUREMODES_CUBEMAP: Result := 'tex'; // Note : If we use 'texreg2rgb', that requires ps.1.2 (we're still using ps.1.1)
{$ENDIF}
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

  // Warn about unprocessed flag :
  if (FinalCombiner.dwPS_GLOBALFLAGS and Ord(PS_GLOBALFLAGS_TEXMODE_ADJUST)) > 0 then
    Result := Result + '; PS_GLOBALFLAGS_TEXMODE_ADJUST unhandled!';

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

{$IFDEF DXBX_USE_D3D9}
  for Stage := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    aScope.Stage := Stage;
    if PSTextureModes[aScope.Stage] <> PS_TEXTUREMODES_NONE then
      Result := Result + Format('dcl t%d.xyzw'#13#10, [Stage]);
  end;
{$ENDIF}

  for Stage := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    aScope.Stage := Stage;
    Result := Result + DisassembleTextureMode(aScope, _NextIs2D(Stage));
  end;
end;

function ByteToFloat(const aByte: Byte): Float;
begin
//  Result := (aByte - 127.0) / 128.0
  Result := aByte / 255.0
end;

function _EmitConstDef(i: DWORD; Constant: D3DCOLOR): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  // Colors are defined in RGBA format, and range 0.0 - 1.0 (negative values
  // can be obtained by supplying PS_INPUTMAPPING_SIGNED_NEGATE to the combiner
  // that reads from these constants).
  Result := Format('def c%d, %ff, %ff, %ff, %ff'#13#10, [i,
    {R}ByteToFloat((Constant shr 16) and $FF),
    {G}ByteToFloat((Constant shr  8) and $FF),
    {B}ByteToFloat((Constant shr  0) and $FF),
    {A}ByteToFloat((Constant shr 24) and $FF)
    ]);
end;

function RPSIntermediate.DisassembleIntermediate(): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  LogFlags: TLogFlags;
  i, j: int;
  Scope: RPSDisassembleScope;
  ConstUsed: array [0..X_PSH_CONSTANTCOUNT-1] of Boolean;
  ConstEmitted: array [0..X_PSH_CONSTANTCOUNT-1] of Boolean;
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
  // 2.0 allows up to r12, c32 and t8
{$IFDEF DXBX_USE_D3D9}
  Result := 'ps_2_0'#13#10;
{$ELSE}
  Result := 'ps.1.3'#13#10;
{$ENDIF}

  for j := 0 to X_PSH_CONSTANTCOUNT - 1 do
  begin
    ConstEmitted[j] := False;
    ConstUsed[j] := True;
    // TODO -oDxbx: Determine ConstUsed during decoding (probably more like the vertex shader decoding)
    // As long as we don't do this, Turok shows much more black as there are too many
    // constants 'def'ined...
  end;

  for i := 0 to NumberOfCombiners-1 do
  begin
    // Define constants directly after the version instruction and before any other instruction :
    j := Combiners[i].C0Mapping;
    if (j < X_PSH_CONSTANTCOUNT) and ConstUsed[j] and (not ConstEmitted[j]) then
    begin
      Result := Result + _EmitConstDef(j, Original.PSConstant0[i]);
      ConstEmitted[j] := True;
    end;

    j := Combiners[i].C1Mapping;
    if (j < X_PSH_CONSTANTCOUNT) and ConstUsed[j] and (not ConstEmitted[j]) then
    begin
      Result := Result + _EmitConstDef(j, Original.PSConstant1[i]);
      ConstEmitted[j] := True;
    end;
  end;

// Emitting these causes black cogs in BumpDemo :
//  if (Original.PSFinalCombinerInputsABCD > 0)
//  or (Original.PSFinalCombinerInputsEFG > 0) then
//  begin
//    if FinalCombiner.FinalCombinerC0Mapping < 8 then
//      Result := Result + _EmitConstDef(FinalCombiner.FinalCombinerC0Mapping, Original.PSFinalCombinerConstant0);
//    if FinalCombiner.FinalCombinerC1Mapping < 8 then
//      Result := Result + _EmitConstDef(FinalCombiner.FinalCombinerC1Mapping, Original.PSFinalCombinerConstant1);
//  end;

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
    Result := Result + Combiners[i].DisassembleCombinerStage(@Scope);
  end;

  // Check if there's a final combiner
  if (Original.PSFinalCombinerInputsABCD > 0)
  or (Original.PSFinalCombinerInputsEFG > 0) then
    // TODO : XSokoban looses it's font rendering when the final combiner is emitted,
    // when disabled, the font reappears (in various colors). This could be because
    // the constants are not properly set locally...
    Result := Result + FinalCombiner.DisassembleFinalCombiner(@Scope);


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
  // Don't dump more than 100 shaders, to prevent cluttering the filesystem :
  if PshNumber >= 100 then
    Exit;

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
  Result := PSIntermediate.IntermediateToString();
end;

procedure XTL_PrintPixelShaderDefContents(pPSDef: PX_D3DPIXELSHADERDEF);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if MayLog(lfUnit) then
    DbgPrintf(XTL_DumpPixelShaderDefToString(pPSDef));
end;

function XTL_EmuRecompilePshDef(pPSDef: PX_D3DPIXELSHADERDEF): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  PSIntermediate: RPSIntermediate;
{$IFDEF PS_REWRITE}
  New: PSH_XBOX_SHADER;
{$ENDIF}
begin
  PSIntermediate.Init(pPSDef);
  Result := PSIntermediate.DisassembleIntermediate();
{$IFDEF PS_REWRITE}
  New.Decode(pPSDef);
//  Result := New.ToString;
{$ENDIF}
end;

end.

