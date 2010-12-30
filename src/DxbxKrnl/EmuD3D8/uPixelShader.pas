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
  // DirectX
  Math,
{$IFDEF DXBX_USE_D3D9}
  Direct3D9,
  D3DX9,
{$ELSE}
  Direct3D8, // D3DCOLOR_ARGB
  D3DX8,
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

//{$IFDEF DXBX_USE_D3D9}
//  {$IFDEF EXPIRIMENTAL}
//    {$DEFINE DXBX_USE_PS_3_0}
//  {$ELSE}
//    {$DEFINE DXBX_USE_PS_2_0}
//  {$ENDIF}
//{$ELSE}
  {$DEFINE DXBX_USE_PS_1_3}
//{$ENDIF}

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

type PSH_OPCODE =
(
    PO_COMMENT,
    PO_PS,
    PO_DEF,
{$IFDEF DXBX_USE_PS_2_0}
    PO_DCL,
{$ENDIF}
    PO_TEX,
    PO_TEXBEM,
    PO_TEXBEML,
    PO_TEXBRDF, // Xbox ext.
    PO_TEXCOORD,
    PO_TEXKILL,
    PO_TEXDP3, // Note : ps.1.3 only
    PO_TEXDP3TEX, // Note : ps.1.3 only
    PO_TEXM3X2TEX,
    PO_TEXM3X2DEPTH, // Note : requires ps.1.3 and a preceding texm3x2pad
    PO_TEXM3X3DIFF, // Xbox ext.
    PO_TEXM3X3VSPEC,
    PO_TEXM3X3TEX, // Note : Uses a cube texture
    PO_TEXREG2AR,
    PO_TEXREG2GB,
    PO_TEXM3X2PAD, // Note : Must be combined with texm3x2tex or texm3x2depth
    PO_TEXM3X3PAD,
    PO_TEXM3X3SPEC, // NOTE : NEEDS 3 ARGUMENTS!
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
{$IFDEF DXBX_USE_PS_2_0}
  ({PO_DCL} mn:'dcl';  _Out:1; _In:0; note:''),
{$ENDIF}
  ({PO_TEX} mn:'tex';  _Out:1; _In:0; note:''),
  ({PO_TEXBEM} mn:'texbem';  _Out:1; _In:1; note:''),
  ({PO_TEXBEML} mn:'texbeml';  _Out:1; _In:1; note:''),
  ({PO_TEXBRDF} mn:'texbrdf';  _Out:1; _In:1; note:''), // Note : Not supported by Direct3D8 ?
  ({PO_TEXCOORD} mn:'texcoord';  _Out:1; _In:0; note:''),
  ({PO_TEXKILL} mn:'texkill';  _Out:1; _In:0; note:''),
  ({PO_TEXDP3} mn:'texdp3';  _Out:1; _In:1; note:''),
  ({PO_TEXDP3TEX} mn:'texdp3tex';  _Out:1; _In:1; note:''),
  ({PO_TEXM3X2TEX} mn:'texm3x2tex';  _Out:1; _In:1; note:''),
  ({PO_TEXM3X2DEPTH} mn:'texm3x2depth';  _Out:1; _In:1; note:''), // Note : requires ps.1.3 and a preceding texm3x2pad
  ({PO_TEXM3X3DIFF} mn:'texm3x3diff';  _Out:1; _In:1; note:''), // Note : Not supported by Direct3D8 ?
  ({PO_TEXM3X3VSPEC} mn:'texm3x3vspec';  _Out:1; _In:1; note:''),
  ({PO_TEXM3X3TEX} mn:'texm3x3tex';  _Out:1; _In:1; note:''), // Note : Uses a cube texture
  ({PO_TEXREG2AR} mn:'texreg2ar';  _Out:1; _In:1; note:''),
  ({PO_TEXREG2GB} mn:'texreg2gb';  _Out:1; _In:1; note:''),
  ({PO_TEXM3X2PAD} mn:'texm3x2pad';  _Out:1; _In:1; note:''),
  ({PO_TEXM3X3PAD} mn:'texm3x3pad';  _Out:1; _In:1; note:''),
  ({PO_TEXM3X3SPEC} mn:'texm3x3spec';  _Out:1; _In:2; note:''),
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

const
  XFC_COMBINERSTAGENR = X_PSH_COMBINECOUNT; // Always call XFC 'stage 9', 1 after the 8th combiner

  PSH_XBOX_MAX_C_REGISTER_COUNT = 16;
  PSH_XBOX_MAX_R_REGISTER_COUNT = 2;
  PSH_XBOX_MAX_T_REGISTER_COUNT = 4;
  PSH_XBOX_MAX_V_REGISTER_COUNT = 2;
  // Extra constants to support features not present in Native D3D :
  PSH_XBOX_CONSTANT_FOG = PSH_XBOX_MAX_C_REGISTER_COUNT; // = 16
  PSH_XBOX_CONSTANT_FC0 = PSH_XBOX_CONSTANT_FOG + 1; // = 17
  PSH_XBOX_CONSTANT_FC1 = PSH_XBOX_CONSTANT_FC0 + 1; // = 18
  PSH_XBOX_CONSTANT_MAX = PSH_XBOX_CONSTANT_FC1 + 1; // = 19

{$IFDEF DXBX_USE_PS_3_0}
  PSH_PC_MAX_C_REGISTER_COUNT = 224; // ps 3.0
  PSH_PC_MAX_R_REGISTER_COUNT = 32; // ps 3.0
  PSH_PC_MAX_S_REGISTER_COUNT = 16; // ps 3.0
  PSH_PC_MAX_V_REGISTER_COUNT = 10; // ps 3.0
  PSH_PC_MAX_REGISTER_COUNT = 224;
{$ELSE}
 {$IFDEF DXBX_USE_PS_2_0}
  PSH_PC_MAX_C_REGISTER_COUNT = 32; // ps 2.0
  PSH_PC_MAX_R_REGISTER_COUNT = 12; // ps 2.0
  PSH_PC_MAX_S_REGISTER_COUNT = 16; // ps 2.0
  PSH_PC_MAX_T_REGISTER_COUNT = 8; // ps 2.0
  PSH_PC_MAX_V_REGISTER_COUNT = 2; // ps 2.0
  PSH_PC_MAX_REGISTER_COUNT = 32;
 {$ELSE} // DXBX_USE_PS_1_3
  PSH_PC_MAX_C_REGISTER_COUNT = 8; // ps 1.3
  PSH_PC_MAX_R_REGISTER_COUNT = 2; // ps 1.3
  PSH_PC_MAX_T_REGISTER_COUNT = 4; // ps 1.3
  PSH_PC_MAX_V_REGISTER_COUNT = 2; // ps 1.3
  PSH_PC_MAX_REGISTER_COUNT = 8;
 {$ENDIF}
{$ENDIF}

type PSH_INST_MODIFIER = (
  INSMOD_NONE, // y =  x
  INSMOD_BIAS, // y =  x - 0.5         // Xbox only : TODO : Fixup occurrances!
  INSMOD_X2,   // y =  x        *   2
  INSMOD_BX2,  // y = (x - 0.5) *   2  // Xbox only : TODO : Fixup occurrances!
  INSMOD_X4,   // y =  x        *   4
  INSMOD_D2,   // y =  x        * 0.5
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

type
  RPSRegisterObject = object
    IsAlpha: Boolean;
    Reg: PS_REGISTER;
    procedure Decode(Value: Byte; aIsAlpha: Boolean);
    function DecodedToString(): string;
  end;

  RPSInputRegister = object(RPSRegisterObject)
    Channel: PS_CHANNEL;
    InputMapping: PS_INPUTMAPPING;
    procedure Decode(Value: Byte; aIsAlpha: Boolean);
    function DecodedToString(): string;
  end;

  RPSCombinerOutput = object(RPSRegisterObject)
    Input1: RPSInputRegister; // Called InputA or InputC (depending if it's inside the AB or CD combiner)
    Input2: RPSInputRegister; // Called InputC or InputD (depending if it's inside the AB or CD combiner)
    DotProduct: Boolean; // False=Multiply, True=DotProduct
    BlueToAlpha: Boolean; // False=Alpha-to-Alpha, True=Blue-to-Alpha
    procedure Decode(Value: Byte; PSInputs: DWORD; aIsAlpha: Boolean);
  end;

  RPSCombinerOutputMuxSum = object(RPSRegisterObject)
    OutputAB: RPSCombinerOutput; // Contains InputA and InputB (as Input1 and Input2)
    OutputCD: RPSCombinerOutput; // Contains InputC and InputD (as Input1 and Input2)
  end;

  RPSCombinerStageChannel = record
    OutputSUM: RPSCombinerOutputMuxSum; // Contains OutputAB, OutputCD
    CombinerOutputFlags: PS_COMBINEROUTPUT;
    AB_CD_SUM: Boolean; // True=AB+CD, False=MUX(AB;CD) based on R0.a
    procedure Decode(PSInputs, PSOutputs: DWORD; IsAlpha: Boolean = False);
  end;

  RPSCombinerStage = record
    RGB: RPSCombinerStageChannel;
    Alpha: RPSCombinerStageChannel;
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
  end;

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
    function UsesRegister: Boolean;
    function IsRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16): Boolean; overload;
    function IsRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16; aMask: DWORD): Boolean; overload;
    procedure SetRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16; aMask: DWORD);
    function ToString: string;
    function Decode(const Value: DWORD; aMask: DWORD; ArgumentType: TArgumentType): Boolean;
    procedure Invert;
    procedure Negate;
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
    function IsArithmetic: Boolean;
    procedure ScaleOutput(aFactor: Float);
    function ReadsFromRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16): Boolean; overload;
    function ReadsFromRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16; aMask: DWORD): Boolean; overload;
    function WritesToRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16): Boolean; overload;
    function WritesToRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16; aMask: DWORD): Boolean; overload;
    procedure SwapParameter(const Index1, Index2: int);
    procedure XSwapOutput();
    function MoveRemovableParametersRight(const Index1, Index2: int): Boolean;
    function XMoveNonRegisterOutputsRight(): Boolean;
    procedure XCopySecondOpcodeToFirst(const aOpcode: PSH_OPCODE);
    function Decode(CombinerStageNr, PSInputs, PSOutputs: DWORD; aMask: DWORD): Boolean;
    function DecodeFinalCombiner(aPSFinalCombinerInputsABCD, aPSFinalCombinerInputsEFG: DWORD): Boolean;
  end;
  PPSH_INTERMEDIATE_FORMAT = ^PSH_INTERMEDIATE_FORMAT;

type
  PPSH_RECOMPILED_SHADER = ^PSH_RECOMPILED_SHADER;
  PSH_RECOMPILED_SHADER = record
    Next: PPSH_RECOMPILED_SHADER;
    PSDef: X_D3DPIXELSHADERDEF;
    NewShaderStr: string;
    ConvertedHandle: DWORD;
    ConstInUse: array [0..PSH_XBOX_CONSTANT_MAX-1] of boolean;
    ConstMapping: array [0..PSH_XBOX_CONSTANT_MAX-1] of int;
  end;

type PSH_XBOX_SHADER = record
    // Reserve enough slots for all shaders, so we need space for 2 constants, 4 texture addressing codes and 5 lines per opcode : :
    Intermediate: array[0..(2+X_D3DTS_STAGECOUNT+X_PSH_COMBINECOUNT * 5)] of PSH_INTERMEDIATE_FORMAT;
    IntermediateCount: int;

    PSTextureModes: array[0..X_D3DTS_STAGECOUNT-1] of PS_TEXTUREMODES;
    PSDotMapping: array[0..X_D3DTS_STAGECOUNT-1] of PS_DOTMAPPING;
    PSCompareMode: array[0..X_D3DTS_STAGECOUNT-1] of DWORD;
    PSInputTexture: array[0..X_D3DTS_STAGECOUNT-1] of int;

    FinalCombinerFlags: PS_FINALCOMBINERSETTING;
    // Note : The following constants are only needed for PSH_XBOX_SHADER.DecodedToString,
    // they are not involved in the actual pixel shader recompilation anymore :
    FinalCombiner: RPSFinalCombiner;
    Combiners: array [0..X_PSH_COMBINECOUNT-1] of RPSCombinerStage;
    NumberOfCombiners: int;
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
    function OriginalToString(pPSDef: PX_D3DPIXELSHADERDEF): string;
    function Decode(pPSDef: PX_D3DPIXELSHADERDEF): PSH_RECOMPILED_SHADER;
    function DecodedToString(pPSDef: PX_D3DPIXELSHADERDEF): string;
    function DecodeTextureModes(pPSDef: PX_D3DPIXELSHADERDEF): Boolean;
    function MoveRemovableParametersRight: Boolean;
    procedure ConvertXboxOpcodesToNative(pPSDef: PX_D3DPIXELSHADERDEF);
    function ConvertConstantsToNative(pPSDef: PX_D3DPIXELSHADERDEF; var Recompiled: PSH_RECOMPILED_SHADER): Boolean;
    function RemoveUselessWrites: Boolean;
    function IsRegisterFreeFromIndexOnwards(aIndex: int; aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16): Boolean;
    procedure ReplaceRegisterFromIndexOnwards(aIndex: int;
      aSrcRegType: PSH_ARGUMENT_TYPE; aSrcAddress: Int16;
      aDstRegType: PSH_ARGUMENT_TYPE; aDstAddress: Int16);
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
    function SimplifyMAD(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
    function SimplifySUB(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
    function SimplifyMUL(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
    function SimplifyLRP(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
    function FixupPixelShader(): Boolean;
    function FixInvalidSrcSwizzle(): Boolean;
    function FixMissingR0a(): Boolean;
    function FixCoIssuedOpcodes(): Boolean;
  end;

// dump pixel shader definition to file
procedure XTL_DumpPixelShaderToFile(pPSDef: PX_D3DPIXELSHADERDEF);
// Recompile Xbox PixelShader def
function XTL_EmuRecompilePshDef(pPSDef: PX_D3DPIXELSHADERDEF): PSH_RECOMPILED_SHADER;

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
    'PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS',  // 0x18L, // y := (x - 0.5)*2 = x*2 - 1.0
    'PS_COMBINEROUTPUT_SHIFTLEFT_2',       // 0x20L, // y := x*4
    'PS_COMBINEROUTPUT_SHIFTRIGHT_1',      // 0x30L, // y := x/2 = x*0.5

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

const
  CONST_NEG_ONE = -2;
  CONST_NEG_HALF = -1;
  CONST_ZERO = 0;
  CONST_POS_HALF = 1; // Note : Instead of 0.5 we use 1 (so we can keep using integers)
  CONST_POS_ONE = 2;

var
  LogFlags: TLogFlags;

///

function PSCombinerOutputFlagsToStr(const dwFlags: DWORD; IsAlpha: Boolean = False): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result :={Result + ' | ' +}PS_CombineOutputStr[ 0 + ((dwFlags and $38)                                   shr 3)];
  Result := Result + ' | ' + PS_CombineOutputStr[ 8 + ((dwFlags and Ord(PS_COMBINEROUTPUT_AB_DOT_PRODUCT)) shr 1)];
  Result := Result + ' | ' + PS_CombineOutputStr[10 + ((dwFlags and Ord(PS_COMBINEROUTPUT_CD_DOT_PRODUCT)) shr 0)];
  Result := Result + ' | ' + PS_CombineOutputStr[12 + ((dwFlags and Ord(PS_COMBINEROUTPUT_AB_CD_MUX))      shr 2)];

  if not IsAlpha then
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
    // Anything other than a value-parameter returns a value never checked for :
    Result := Infinity;
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

function PSH_IMD_ARGUMENT.UsesRegister: Boolean;
begin
  Result := (Type_ > PARAM_DISCARD);
end;

function PSH_IMD_ARGUMENT.IsRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16): Boolean;
begin
  Result := (Type_ = aRegType)
        and (Address = aAddress);
end;

function PSH_IMD_ARGUMENT.IsRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16; aMask: DWORD): Boolean;
begin
  Result := IsRegister(aRegType, aAddress)
        // Check the mask itself, but also 'mask-less' :
        and (((Mask and aMask) = aMask) or (Mask = 0));
end;

procedure PSH_IMD_ARGUMENT.SetRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16; aMask: DWORD);
begin
  Type_ := aRegType;
  Address := aAddress;
  Mask := aMask;
end;

function PSH_IMD_ARGUMENT.ToString: string;
var
  Modifier: PSH_ARG_MODIFIER;
begin
  if Type_ = PARAM_VALUE then
  begin
    Result := Format('%f', [GetConstValue]);
    if Pos(DecimalSeparator, Result) > 0 then
      Result := Result + 'f';

    Exit;
  end;

  Result := PSH_ARGUMENT_TYPE_Str[Type_];

  if Type_ >= PARAM_R then
    Result := Result + IntToStr(Address);

  if UsesRegister then
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
  Mask := aMask;
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

  // We're done if this decoding is meant for output parameters,
  // or when the input is a value-parameter (already read above) :
  if (ArgumentType = atOutput)
  or (Type_ = PARAM_VALUE) then
    Exit;

  // Handle the Channel Designator :
  begin
    Channel := PS_CHANNEL(Value and Ord(PS_CHANNEL_ALPHA));
    if Channel = PS_CHANNEL_ALPHA then
      // Input comes from alpha portion of input register (valid for both RGB and alpha portions) :
      Mask := MASK_A
    else // = PS_CHANNEL_BLUE (for Alpha step) = PS_CHANNEL_BLUE (for RGB step) :
      if aMask = MASK_A then
        // Input comes from b portion of input register (valid for alpha portion only) :
        Mask := MASK_B // Note : This is not the same as ARGMOD_BLUE_REPLICATE!
      else
        // Input comes from the RGB portion of the input register (valid for RGB portion only) :
        Mask := aMask; // Note : Is already put here, but makes this code clearer
  end;

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
end; // Decode

procedure PSH_IMD_ARGUMENT.Invert;
begin
  if (Modifiers * [ARGMOD_INVERT]) = [] then
    Modifiers := Modifiers + [ARGMOD_INVERT]
  else
    Modifiers := Modifiers - [ARGMOD_INVERT];
end;

procedure PSH_IMD_ARGUMENT.Negate;
begin
  if (Modifiers * [ARGMOD_NEGATE]) = [] then
    Modifiers := Modifiers + [ARGMOD_NEGATE]
  else
    Modifiers := Modifiers - [ARGMOD_NEGATE];
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

function PSH_INTERMEDIATE_FORMAT.IsArithmetic: Boolean;
begin
  Result := (Opcode >= PO_ADD);
end;

procedure PSH_INTERMEDIATE_FORMAT.ScaleOutput(aFactor: Float);
begin
  Assert(aFactor > 0.0);

  if aFactor = 1.0 then
    Exit;

  if aFactor = 0.5 then
  begin
    // Half the output modifier :
    case Modifier of
      INSMOD_X4:
        Modifier := INSMOD_X2;
      INSMOD_X2:
        Modifier := INSMOD_NONE;
      INSMOD_NONE:
        Modifier := INSMOD_D2;
    end;

    Exit;
  end;

  if aFactor = 2.0 then
  begin
    // Double the output modifier :
    case Modifier of
      INSMOD_D2:
        Modifier := INSMOD_NONE;
      INSMOD_NONE:
        Modifier := INSMOD_X2;
      INSMOD_X2:
        Modifier := INSMOD_X4;
    end;

    Exit;
  end;
end;

function PSH_INTERMEDIATE_FORMAT.ReadsFromRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16): Boolean;
var
  i: int;
begin
  // Check all parameters :
  for i := 0 to PSH_OPCODE_DEFS[Opcode]._In - 1 do
  begin
    // Check if one of them reads from the given register :
    Result := Parameters[i].IsRegister(aRegType, aAddress);
    if Result then
      Exit;
  end;

  Result := False;
end;

function PSH_INTERMEDIATE_FORMAT.ReadsFromRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16; aMask: DWORD): Boolean;
var
  i: int;
begin
  // Check all parameters :
  for i := 0 to PSH_OPCODE_DEFS[Opcode]._In - 1 do
  begin
    // Check if one of them reads from the given register :
    Result := Parameters[i].IsRegister(aRegType, aAddress, aMask);
    if Result then
      Exit;
  end;

  Result := False;
end;

function PSH_INTERMEDIATE_FORMAT.WritesToRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16): Boolean;
var
  i: int;
begin
  // Check the output :
  for i := 0 to PSH_OPCODE_DEFS[Opcode]._Out - 1 do
  begin
    // Check if one of them writes to the given register :
    Result := Output[i].IsRegister(aRegType, aAddress);
    if Result then
      Exit;
  end;

  Result := False;
end;

function PSH_INTERMEDIATE_FORMAT.WritesToRegister(aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16; aMask: DWORD): Boolean;
var
  i: int;
begin
  // Check the output :
  for i := 0 to PSH_OPCODE_DEFS[Opcode]._Out - 1 do
  begin
    // Check if one of them writes to the given register :
    Result := Output[i].IsRegister(aRegType, aAddress, aMask);
    if Result then
      Exit;
  end;

  Result := False;
end;

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

  if  (not Parameters[Index1].UsesRegister)
  and (Parameters[Index2].UsesRegister) then
  begin
    SwapParameter(Index1, Index2);
    Result := True;
  end;
end;

function PSH_INTERMEDIATE_FORMAT.XMoveNonRegisterOutputsRight(): Boolean;
// Swap discards and constants to the right position, to ease later conversions. Applies only to Xbox opcodes.
begin
  Result := False;

  // First, check if the left output is discarded, while the second isn't :
  if  (not Output[0].UsesRegister)
  and (Output[1].UsesRegister) then
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
    // Handle the Output Mapping :
    case PS_COMBINEROUTPUT(Ord(CombinerOutputFlags) and $38) of
      PS_COMBINEROUTPUT_BIAS:             Self.Modifier := INSMOD_BIAS; // TODO : Fixup occurrances!
      PS_COMBINEROUTPUT_SHIFTLEFT_1:      Self.Modifier := INSMOD_X2;
      PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS: Self.Modifier := INSMOD_BX2;  // TODO : Fixup occurrances!
      PS_COMBINEROUTPUT_SHIFTLEFT_2:      Self.Modifier := INSMOD_X4;
      PS_COMBINEROUTPUT_SHIFTRIGHT_1:     Self.Modifier := INSMOD_D2;
    else {PS_COMBINEROUTPUT_IDENTITY:}    Self.Modifier := INSMOD_NONE;
    end;

    if (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA) > 0 then // False=Alpha-to-Alpha, True=Blue-to-Alpha
    begin
      // Note : The effect of this flag is not entirely clear - blue to alpha itself is an easy to understand operation,
      // but on what output does it operate? AB? or the mux_sum destination register (which doesn't occur when a dot
      // operation is executed)? What if AB is discarded, but AB+CD is registered? Also, what happens to the other
      // color channels (R,G and A) in that register? The docs seem to imply that AB itself is not changed (as they
      // state that the alpha portion is not necessarily discarded), which would mean that only the mux_sum output
      // is influenced, but that would imply that this flag has no effect for dot-products (XDD or XDM)...
      // And if this is true, how do the blue-to-alpha flags behave if present on both AB and CD?

      // TODO : Rayman does this in some shaders, requires a fixup (as output.b is incorrect and not allowed)
      Output[0].Modifiers := Output[0].Modifiers + [ARGMOD_BLUE_REPLICATE];
      Output[0].Mask := MASK_B;
      // TODO Handle blue-to-alpha flag (only valid for RGB)
      // Note : We can't use the '+ ' prefix, as the blue channel is not determined yet!
      // Note 2: Pixel shader 1.1-1.3 'blue replicate' on source, uses an alpha destination write mask.
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

  // TODO : XSokoban looses it's font rendering when the final combiner is emitted,
  // when disabled, the font reappears (in various colors). This could be because
  // the constants are not properly set locally...

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
  // First things first, set the pixel shader version
  // 1.1 allows reading from 2 textures (which we use in 'cnd') and reading from the .b (blue) channel
  // 1.3 allows the use of texm3x2depth (which can occur sometimes)
  // 2.0 allows up to r12, c32, t8 and s16 (requires Direct3D9)
  // 3.0 allows up to r32, c224, v10 (instead of t via dcl), s16 and vFace (which can do two-sided lighting)
{$IFDEF DXBX_USE_PS_3_0}
  Result := 'ps_3_0'#13#10;
{$ELSE}
 {$IFDEF DXBX_USE_PS_2_0}
  Result := 'ps_2_0'#13#10;
 {$ELSE}
  Result := 'ps.1.3'#13#10;
 {$ENDIF}
{$ENDIF}
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

function PSH_XBOX_SHADER.OriginalToString(pPSDef: PX_D3DPIXELSHADERDEF): string;
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
                  [pPSDef.PSAlphaInputs[0], pPSDef.PSAlphaInputs[1], pPSDef.PSAlphaInputs[2], pPSDef.PSAlphaInputs[3],
                  pPSDef.PSAlphaInputs[4], pPSDef.PSAlphaInputs[5], pPSDef.PSAlphaInputs[6], pPSDef.PSAlphaInputs[7],
                  pPSDef.PSFinalCombinerInputsABCD,
                  pPSDef.PSFinalCombinerInputsEFG,
                  pPSDef.PSConstant0[0], pPSDef.PSConstant0[1], pPSDef.PSConstant0[2], pPSDef.PSConstant0[3],
                  pPSDef.PSConstant0[4], pPSDef.PSConstant0[5], pPSDef.PSConstant0[6], pPSDef.PSConstant0[7],
                  pPSDef.PSConstant1[0], pPSDef.PSConstant1[1], pPSDef.PSConstant1[2], pPSDef.PSConstant1[3],
                  pPSDef.PSConstant1[4], pPSDef.PSConstant1[5], pPSDef.PSConstant1[6], pPSDef.PSConstant1[7],
                  pPSDef.PSAlphaOutputs[0], pPSDef.PSAlphaOutputs[1], pPSDef.PSAlphaOutputs[2], pPSDef.PSAlphaOutputs[3],
                  pPSDef.PSAlphaOutputs[4], pPSDef.PSAlphaOutputs[5], pPSDef.PSAlphaOutputs[6], pPSDef.PSAlphaOutputs[7],
                  pPSDef.PSRGBInputs[0], pPSDef.PSRGBInputs[1], pPSDef.PSRGBInputs[2], pPSDef.PSRGBInputs[3],
                  pPSDef.PSRGBInputs[4], pPSDef.PSRGBInputs[5], pPSDef.PSRGBInputs[6], pPSDef.PSRGBInputs[7],
                  pPSDef.PSCompareMode,
                  pPSDef.PSFinalCombinerConstant0,
                  pPSDef.PSFinalCombinerConstant1,
                  pPSDef.PSRGBOutputs[0], pPSDef.PSRGBOutputs[1], pPSDef.PSRGBOutputs[2], pPSDef.PSRGBOutputs[3],
                  pPSDef.PSRGBOutputs[4], pPSDef.PSRGBOutputs[5], pPSDef.PSRGBOutputs[6], pPSDef.PSRGBOutputs[7],
                  pPSDef.PSCombinerCount,
                  pPSDef.PSTextureModes,
                  pPSDef.PSDotMapping,
                  pPSDef.PSInputTexture,
                  pPSDef.PSC0Mapping,
                  pPSDef.PSC1Mapping,
                  pPSDef.PSFinalCombinerConstants]);
end;

function PSH_XBOX_SHADER.Decode(pPSDef: PX_D3DPIXELSHADERDEF): PSH_RECOMPILED_SHADER;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  i: int;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.PSDef := pPSDef^;

  // Azurik likes to create and destroy the same shader every frame! O_o
  LogFlags := lfUnit;
  if IsRunning(TITLEID_AZURIK) then
    LogFlags := LogFlags or lfExtreme;

  ZeroMemory(@Self, SizeOf(Self));

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

  // Backwards compatible decoding (purely for logging) :
  begin
    for i := 0 to X_PSH_COMBINECOUNT - 1 do
    begin
      Combiners[i].RGB.Decode(pPSDef.PSRGBInputs[i], pPSDef.PSRGBOutputs[i]);
      Combiners[i].Alpha.Decode(pPSDef.PSAlphaInputs[i], pPSDef.PSAlphaOutputs[i], {IsAlpha=}True);
    end;

    FinalCombiner.Decode(pPSDef.PSFinalCombinerInputsABCD, pPSDef.PSFinalCombinerInputsEFG, pPSDef.PSFinalCombinerConstants);
  end;

  for i := 0 to NumberOfCombiners - 1 do
  begin
    // Check that the RGB and Alpha inputs do the same operation :
    if  ((pPSDef.PSRGBInputs[i] and PS_NoChannelsMask) = (pPSDef.PSAlphaInputs[i] and PS_NoChannelsMask))
    // Check if all RGB channels are set to read from PS_CHANNEL_RGB :
    and ((pPSDef.PSRGBInputs[i] and PS_AlphaChannelsMask) = 0)
    // Check if all Alpha channels are set to read from PS_CHANNEL_ALPHA :
    and ((pPSDef.PSAlphaInputs[i] and PS_AlphaChannelsMask) = PS_AlphaChannelsMask)
    // Check that RGB and Alpha output to the same register(s) :
    and (pPSDef.PSRGBOutputs[i] = pPSDef.PSAlphaOutputs[i]) then
    begin
      // In this case, we can convert RGB and Alpha together :
      if not NewIntermediate.Decode(i, pPSDef.PSRGBInputs[i], pPSDef.PSRGBOutputs[i], MASK_RGBA) then
        DeleteLastIntermediate;
    end
    else
    begin
      // Otherwise, we need to convert RGB and Alpha separately :
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
//    dwPS_GLOBALFLAGS := (pPSDef.PSFinalCombinerConstants shr 8) and $1;
    end
    else
      DeleteLastIntermediate;

  // Dump the contents of the PixelShader def
  if MayLog(LogFlags) then
    // dump pixel shader definition to string
    XTL_DumpPixelShaderToFile(pPSDef);

  if MayLog(LogFlags) then
  begin
    // print relevant contents to the debug console
    DbgPrintf(DecodedToString(pPSDef));
  end;

  // TODO:
  // - Insert tex* and def instructions

  Log('Parse result');

  if MoveRemovableParametersRight then
    Log('MoveRemovableParametersRight');

  if RemoveNops() then
    Log('RemoveNops');

  if RemoveUselessWrites then
    Log('RemoveUselessWrites');

  if ConvertConstantsToNative(pPSDef, {Recompiled=}Result) then
    Log('ConvertConstantsToNative');

  ConvertXboxOpcodesToNative(pPSDef);
  Log('ConvertXboxOpcodesToNative');

  if RemoveUselessWrites then // twice!
    Log('RemoveUselessWrites');

  // Resolve all differences :
  if FixupPixelShader then
    Log('FixupPixelShader');

  // Handle Texture declarations :
  if DecodeTextureModes(pPSDef) then
    Log('DecodeTextureModes');

  if FixInvalidSrcSwizzle then
    Log('FixInvalidSrcSwizzle');

  if FixMissingR0a then
    Log('FixMissingR0a');

  if FixCoIssuedOpcodes then
    Log('FixCoIssuedOpcodes');

  Log('End result');

  Result.NewShaderStr := ToString;
end;

function PSH_XBOX_SHADER.DecodedToString(pPSDef: PX_D3DPIXELSHADERDEF): string;
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
  _AddStr(#13#10'-----PixelShader Definition Contents-----');
  _AddStr(OriginalToString(pPSDef));

  if (pPSDef.PSTextureModes > 0) then
  begin
    _AddStr(#13#10'PSTextureModes ->'); // Texture addressing modes
    _AddStr('Stage 0: %s', [PS_TextureModesStr[PSTextureModes[0]]]);
    _AddStr('Stage 1: %s', [PS_TextureModesStr[PSTextureModes[1]]]);
    _AddStr('Stage 2: %s', [PS_TextureModesStr[PSTextureModes[2]]]);
    _AddStr('Stage 3: %s', [PS_TextureModesStr[PSTextureModes[3]]]);
  end;

  if (pPSDef.PSDotMapping > 0) then // Input mapping for dot product modes
  begin
    _AddStr(#13#10'PSDotMapping ->');
    _AddStr('Stage 1: %s', [PS_DotMappingStr[PSDotMapping[1]]]);
    _AddStr('Stage 2: %s', [PS_DotMappingStr[PSDotMapping[2]]]);
    _AddStr('Stage 3: %s', [PS_DotMappingStr[PSDotMapping[3]]]);
  end;

  if (pPSDef.PSCompareMode > 0) then // Compare modes for clipplane texture mode
  begin
    _AddStr(#13#10'PSCompareMode ->');
    _AddStr('Stage 0: %s', [PS_CompareModeStr[iif(PSCompareMode[0] = 0, 0, 1)]]);
    _AddStr('Stage 1: %s', [PS_CompareModeStr[iif(PSCompareMode[1] = 0, 2, 3)]]);
    _AddStr('Stage 2: %s', [PS_CompareModeStr[iif(PSCompareMode[2] = 0, 4, 5)]]);
    _AddStr('Stage 3: %s', [PS_CompareModeStr[iif(PSCompareMode[3] = 0, 6, 7)]]);
  end;

  if (pPSDef.PSInputTexture > 0) then // Texture source for some texture modes
  begin
    _AddStr(#13#10'PSInputTexture ->');
    _AddStr('Stage 1: %d', [PSInputTexture[1]]);
    _AddStr('Stage 2: %d', [PSInputTexture[2]]);
    _AddStr('Stage 3: %d', [PSInputTexture[3]]);
  end;

  if (pPSDef.PSCombinerCount > 0) then // Active combiner count (Stages 0-7)
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

    _AddStr('PSRGBOutputs[%d] AB: %s', [i, Combiners[i].RGB.OutputSUM.OutputAB.DecodedToString()]);
    _AddStr('PSRGBOutputs[%d] CD: %s', [i, Combiners[i].RGB.OutputSUM.OutputCD.DecodedToString()]);
    _AddStr('PSRGBOutputs[%d] SUM: %s', [i, Combiners[i].RGB.OutputSUM.DecodedToString()]);
    _AddStr('PSRGBOutputs[%d] flags: %s', [i, PSCombinerOutputFlagsToStr(Combiners[i].RGB.CombinerOutputFlags, {IsAlpha=}False)]);

    _AddStr(#13#10);
    _AddStr('PSRGBInputs[%d] A: %s', [i, Combiners[i].RGB.OutputSUM.OutputAB.Input1.DecodedToString()]);
    _AddStr('PSRGBInputs[%d] B: %s', [i, Combiners[i].RGB.OutputSUM.OutputAB.Input2.DecodedToString()]);
    _AddStr('PSRGBInputs[%d] C: %s', [i, Combiners[i].RGB.OutputSUM.OutputCD.Input1.DecodedToString()]);
    _AddStr('PSRGBInputs[%d] D: %s', [i, Combiners[i].RGB.OutputSUM.OutputCD.Input2.DecodedToString()]);

    _AddStr(#13#10);
    _AddStr('PSAlphaOutputs[%d] AB: %s', [i, Combiners[i].Alpha.OutputSUM.OutputAB.DecodedToString()]);
    _AddStr('PSAlphaOutputs[%d] CD: %s', [i, Combiners[i].Alpha.OutputSUM.OutputCD.DecodedToString()]);
    _AddStr('PSAlphaOutputs[%d] SUM: %s', [i, Combiners[i].Alpha.OutputSUM.DecodedToString()]);
    _AddStr('PSAlphaOutputs[%d] flags: %s', [i, PSCombinerOutputFlagsToStr(Combiners[i].Alpha.CombinerOutputFlags, {IsAlpha=}True)]);

    _AddStr(#13#10);
    _AddStr('PSAlphaInputs[%d] A: %s', [i, Combiners[i].Alpha.OutputSUM.OutputAB.Input1.DecodedToString()]);
    _AddStr('PSAlphaInputs[%d] B: %s', [i, Combiners[i].Alpha.OutputSUM.OutputAB.Input2.DecodedToString()]);
    _AddStr('PSAlphaInputs[%d] C: %s', [i, Combiners[i].Alpha.OutputSUM.OutputCD.Input1.DecodedToString()]);
    _AddStr('PSAlphaInputs[%d] D: %s', [i, Combiners[i].Alpha.OutputSUM.OutputCD.Input2.DecodedToString()]);

    _AddStr(#13#10);
    _AddStr('PSConstant0[%d] : %x', [i, pPSDef.PSConstant0[i]]); // C0 for each stage
    _AddStr('PSConstant1[%d] : %x', [i, pPSDef.PSConstant1[i]]); // C1 for each stage
  end;

  if (pPSDef.PSFinalCombinerInputsABCD > 0)
  or (pPSDef.PSFinalCombinerInputsEFG  > 0) then // Final combiner inputs
  begin
    _AddStr(#13#10'PSFinalCombinerConstant0 : %x', [pPSDef.PSFinalCombinerConstant0]); // C0 in final combiner
    _AddStr('PSFinalCombinerConstant1 : %x', [pPSDef.PSFinalCombinerConstant1]); // C1 in final combiner

    _AddStr(#13#10'PSFinalCombinerInputsABCD ->');
    _AddStr('Input A: %s', [FinalCombiner.InputA.DecodedToString()]);
    _AddStr('Input B: %s', [FinalCombiner.InputB.DecodedToString()]);
    _AddStr('Input C: %s', [FinalCombiner.InputC.DecodedToString()]);
    _AddStr('Input D: %s', [FinalCombiner.InputD.DecodedToString()]);

    _AddStr(#13#10'PSFinalCombinerInputsEFG ->');
    _AddStr('Input E: %s', [FinalCombiner.InputE.DecodedToString()]);
    _AddStr('Input F: %s', [FinalCombiner.InputF.DecodedToString()]);
    _AddStr('Input G: %s', [FinalCombiner.InputG.DecodedToString()]);
    _AddStr('Final combiner setting: %s', [PSFinalCombinerSettingToStr(Ord(FinalCombiner.FinalCombinerFlags))]);

    _AddStr(#13#10'PSFinalCombinerConstants ->'); // Final combiner constant mapping
    _AddStr('Offset of D3D constant for C0: %d', [FinalCombiner.FinalCombinerC0Mapping]);
    _AddStr('Offset of D3D constant for C1: %d', [FinalCombiner.FinalCombinerC1Mapping]);
    _AddStr('Adjust texture flag: %s', [PS_GlobalFlagsStr[PS_GLOBALFLAGS(FinalCombiner.dwPS_GLOBALFLAGS)]]);
  end;

  _AddStr(#13#10);
end;

function PSH_XBOX_SHADER.DecodeTextureModes(pPSDef: PX_D3DPIXELSHADERDEF): Boolean;
// Branch:Dxbx  Translator:PatrickvL  Done:100

  function _NextIs2D(Stage: int): Boolean;
  begin
    if Stage < X_D3DTS_STAGECOUNT-1 then
      Result := PSTextureModes[Stage + 1] in [PS_TEXTUREMODES_DOT_ST, PS_TEXTUREMODES_DOT_ZW]
    else
      Result := False;
  end;

var
  InsertPos: int;
  Ins: PSH_INTERMEDIATE_FORMAT;
  Stage: int;
begin
  Result := False;

  InsertPos := -1;
  repeat
    Inc(InsertPos);
  until Intermediate[InsertPos].Opcode <> PO_DEF;

{$IFDEF DXBX_USE_PS_2_0}
  Ins.Initialize(PO_DCL);
  for Stage := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    if PSTextureModes[Stage] <> PS_TEXTUREMODES_NONE then
    begin
      Ins.Output[0].SetRegister(PARAM_T, Stage, MASK_RGBA);
      InsertIntermediate(@Ins, InsertPos);
      Inc(InsertPos);
      Result := True;
    end;
  end;
{$ENDIF}

  Ins.Initialize(PO_TEX);
  for Stage := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    // TODO : Apply conversions when PS_GLOBALFLAGS_TEXMODE_ADJUST is set (but ... how to check the texture type? read D3DRS_PSTEXTUREMODES?)

    // Convert the texture mode to a texture addressing instruction :
    case PSTextureModes[Stage] of // input = q,s,t,r (same layout as a,r,g,b, also known as w,x,y,z)
{$IFNDEF DXBX_USE_PS_2_0}
      PS_TEXTUREMODES_PROJECT2D: Ins.Opcode := PO_TEX; // argb = texture(r/q, s/q)      TODO : Apply the division via D3DTOP_BUMPENVMAP ?
      PS_TEXTUREMODES_PROJECT3D: Ins.Opcode := PO_TEX; // argb = texture(r/q, s/q, t/q) Note : 3d textures are sampled using PS_TEXTUREMODES_CUBEMAP
      PS_TEXTUREMODES_CUBEMAP: Ins.Opcode := PO_TEX; // argb = cubemap(r/q, s/q, t/q)
{$ENDIF}
      PS_TEXTUREMODES_PASSTHRU: Ins.Opcode := PO_TEXCOORD;
      PS_TEXTUREMODES_CLIPPLANE: Ins.Opcode := PO_TEXKILL;
      PS_TEXTUREMODES_BUMPENVMAP: Ins.Opcode := PO_TEXBEM;
      PS_TEXTUREMODES_BUMPENVMAP_LUM: Ins.Opcode := PO_TEXBEML;
//    PS_TEXTUREMODES_BRDF: Ins.Opcode := PO_TEXBRDF; // Note : Not supported by Direct3D8 ?
      PS_TEXTUREMODES_DOT_ST: Ins.Opcode := PO_TEXM3X2TEX;
      PS_TEXTUREMODES_DOT_ZW: Ins.Opcode := PO_TEXM3X2DEPTH; // Note : requires ps.1.3 and a preceding texm3x2pad
//    PS_TEXTUREMODES_DOT_RFLCT_DIFF: Ins.Opcode := PO_TEXM3X3DIFF; // Note : Not supported by Direct3D8 ?
      PS_TEXTUREMODES_DOT_RFLCT_SPEC: Ins.Opcode := PO_TEXM3X3VSPEC;
      PS_TEXTUREMODES_DOT_STR_3D: Ins.Opcode := PO_TEXM3X3TEX; // Note : Uses a 3d texture
      PS_TEXTUREMODES_DOT_STR_CUBE: Ins.Opcode := PO_TEXM3X3TEX; // Note : Uses a cube texture
      PS_TEXTUREMODES_DPNDNT_AR: Ins.Opcode := PO_TEXREG2AR;
      PS_TEXTUREMODES_DPNDNT_GB: Ins.Opcode := PO_TEXREG2GB;
      PS_TEXTUREMODES_DOTPRODUCT:
        if _NextIs2D(Stage) then
          Ins.Opcode := PO_TEXM3X2PAD
        else
          Ins.Opcode := PO_TEXM3X3PAD;
      PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST: Ins.Opcode := PO_TEXM3X3SPEC; // Note : Needs 3 arguments!
    else
      Continue;
    end;

    Ins.Output[0].SetRegister(PARAM_T, Stage, 0);

    // For those texture modes that need it, add the source stage as argument :
    if PSH_OPCODE_DEFS[Ins.Opcode]._In >= 1 then
    begin
      Ins.Parameters[0].SetRegister(PARAM_T, PSInputTexture[Stage], 0);

      case PSDotMapping[Stage] of
        PS_DOTMAPPING_MINUS1_TO_1_D3D:
          Ins.Parameters[0].Modifiers := [ARGMOD_SCALE_BX2];
      end;
    end;

    if PSH_OPCODE_DEFS[Ins.Opcode]._In >= 2 then
    begin
      // Add the third argument :
      case PSTextureModes[Stage] of
        PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST:
        begin
          Ins.Parameters[0].SetRegister(PARAM_C, 0, 0);
          Ins.CommentString := 'Dxbx guess'; // TODO : Where do we get the 3rd argument to this?
        end;
      end;
    end;

//    // Warn about unprocessed flag :
//    if (dwPS_GLOBALFLAGS and Ord(PS_GLOBALFLAGS_TEXMODE_ADJUST)) > 0 then
//      Ins.CommentString := Ins.CommentString + ' PS_GLOBALFLAGS_TEXMODE_ADJUST unhandled!';

    InsertIntermediate(@Ins, InsertPos);
    Inc(InsertPos);
    Result := True;
  end;
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
        if Intermediate[i].XMoveNonRegisterOutputsRight() then
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
end; // MoveRemovableParametersRight

function PSH_XBOX_SHADER.ConvertConstantsToNative(pPSDef: PX_D3DPIXELSHADERDEF; var Recompiled: PSH_RECOMPILED_SHADER): Boolean;
var
  i, j: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
  CurArg: PPSH_IMD_ARGUMENT;
  NativeConstInUse: array [0..PSH_PC_MAX_C_REGISTER_COUNT-1] of boolean;
  OriginalConstantNr: Int16;
  EmittedNewConstant: Boolean;
  NewIns: PSH_INTERMEDIATE_FORMAT;

  procedure _SetColor(var NewIns: PSH_INTERMEDIATE_FORMAT; ConstColor: D3DCOLOR);
  var
    XColor: D3DCOLORVALUE;
  begin
    // Colors are defined in RGBA format, and range 0.0 - 1.0 (negative values
    // can be obtained by supplying PS_INPUTMAPPING_SIGNED_NEGATE to the combiner
    // that reads from these constants).
    XColor := D3DXColorFromDWord(ConstColor);
    NewIns.Parameters[0].SetConstValue(XColor.r);
    NewIns.Parameters[1].SetConstValue(XColor.g);
    NewIns.Parameters[2].SetConstValue(XColor.b);
    NewIns.Parameters[3].SetConstValue(XColor.a);
  end;

  // Try to fixup constants above the limit (c7 for PS.1.3) :
  function _MapConstant(ConstNr: int): int;
  begin
    // 1-to-1 mapping for constants that can be supported native (if not used already) :
    if (ConstNr < PSH_PC_MAX_C_REGISTER_COUNT) and (not NativeConstInUse[ConstNr]) then
    begin
      Result := ConstNr;
      Exit;
    end;

    // Assign not-yet-defined constants bottom-to-up :
    Result := 0;
    while Result < PSH_PC_MAX_C_REGISTER_COUNT do
    begin
      if not NativeConstInUse[Result] then
        Exit;

      Inc(Result);
    end;

    // Unresolved - fallback to 1st constant :
    if Result >= PSH_PC_MAX_C_REGISTER_COUNT then
      Result := 0;

    EmuWarning('; Too many constants to emulate, this pixel shader will give unexpected output!');
  end;

  function _HandleConst(XboxConst: int): int;
  var
    NativeConst: int;
  begin
    if not Recompiled.ConstInUse[XboxConst] then
    begin
      // Determine and remember a new mapping to native :
      NativeConst := _MapConstant(XboxConst);
      NativeConstInUse[NativeConst] := True;
      Recompiled.ConstMapping[XboxConst] := NativeConst;
      Recompiled.ConstInUse[XboxConst] := True;
      // Make sure we can check this is a new constant (so we can emit a constant declaration
      // for any final combiner constants - because those cannot be set via SetPixelShaderConstant) :
      EmittedNewConstant := True;
    end;

    // Return the (previously) determined mapping :
    Result := Recompiled.ConstMapping[XboxConst];
  end;

begin
  Result := False;

  // Note : Recompiled.ConstMapping and Recompiled.ConstInUse[i] are still empty here.
  for i := 0 to PSH_PC_MAX_C_REGISTER_COUNT - 1 do
    NativeConstInUse[i] := False;

  // Loop over all opcodes to update the constant-indexes (Xbox uses C0 and C1 in each combiner) :
  for i := 0 to IntermediateCount - 1 do
  begin
    // Loop over this opcodes' input arguments :
    Cur := @(Intermediate[i]);
    for j := 0 to PSH_OPCODE_DEFS[Cur.Opcode]._In - 1 do
    begin
      // Only handle arguments that address a constant register :
      CurArg := @(Cur.Parameters[j]);

      // The Fog register is not supported on PC so we convert it to a constant too :
      // (But only if the MASK is not solely accessing the alpha-channel - we don't support that)
      if (CurArg.Type_ = PARAM_FOG) and (CurArg.Mask <> MASK_A) then
      begin
        CurArg.Type_ := PARAM_C;
        CurArg.Address := _HandleConst(PSH_XBOX_CONSTANT_FOG);
        CurArg.Mask := CurArg.Mask and (not MASK_A);
        Continue;
      end;

      if CurArg.Type_ <> PARAM_C then
        Continue;

      // Make sure we can detect new constants (and if it was C0 or C1),
      // as we need this for fixing up final combiner constants :
      EmittedNewConstant := False;
      OriginalConstantNr := CurArg.Address;

      // For each constant being addressed, we find out which Xbox constant it is,
      // and map it to a native constant (as far as we have space for them) :
      case CurArg.Address of
        0: // Handle C0 (if present) :
        begin
          // The final combiner has a separate C0 constant :
          if Cur.CombinerStageNr = XFC_COMBINERSTAGENR then
            CurArg.Address := _HandleConst(PSH_XBOX_CONSTANT_FC0)
          else
          begin
            // See if C0 has a unique index per combiner stage :
            if CombinerHasUniqueC0 then
              // C0 actually ranges from c0 to c7, one for each possible combiner stage (X_D3DRS_PSCONSTANT0_0..X_D3DRS_PSCONSTANT0_7) :
              CurArg.Address := _HandleConst(Cur.CombinerStageNr)
            else
              // Non-unique just reads the same C0 in every stage :
              CurArg.Address := _HandleConst(0);
          end;
        end;

        1: // Handle C1 (if present) :
        begin
          // The final combiner has a separate C1 constant :
          if Cur.CombinerStageNr = XFC_COMBINERSTAGENR then
            CurArg.Address := _HandleConst(PSH_XBOX_CONSTANT_FC1)
          else
          begin
            // See if C1 has a unique index per combiner stage :
            if CombinerHasUniqueC1 then
              // C1 actually ranges from c8 to c15, one for each possible combiner stage (X_D3DRS_PSCONSTANT1_0..X_D3DRS_PSCONSTANT1_7) :
              CurArg.Address := _HandleConst(Cur.CombinerStageNr + 8)
            else
              // Non-unique just reads the same C1 in every stage :
              CurArg.Address := _HandleConst(1);
          end;
        end;
      end; // case

      // New constants solely used for the final combiner must be DEFined separately,
      // as there's no other way to set these (SetPixelShaderConstant can only write
      // to the 16 slots X_D3DRS_PSCONSTANT1_0..X_D3DRS_PSCONSTANT1_7) :
      if (Cur.CombinerStageNr = XFC_COMBINERSTAGENR) and EmittedNewConstant then
      begin
        // Output a new opcode to define this constant :
        NewIns.Initialize(PO_DEF);
        NewIns.Output[0].SetRegister(PARAM_C, CurArg.Address, MASK_RGBA);
        if OriginalConstantNr = 0 then
          _SetColor(NewIns, pPSDef.PSFinalCombinerConstant0)
        else
          _SetColor(NewIns, pPSDef.PSFinalCombinerConstant1);

        InsertIntermediate(@NewIns, 0);
      end;
    end; // for arguments
  end; // for opcodes
end; // ConvertConstantsToNative

function PSH_XBOX_SHADER.RemoveUselessWrites: Boolean;
// Note : Xbox allows writing to V0 (diffuse color) and V1 (specular color), but native ps.1.3 doesn't!
// Some examples of this behaviour can be seen when running RayMan Arena.
var
  i, j: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
  CurArg: PPSH_IMD_ARGUMENT;
  RegUsage: array [PSH_ARGUMENT_TYPE, 0..PSH_PC_MAX_REGISTER_COUNT] of DWORD;
begin
  // TODO : In Polynomial Texture Maps, one extra opcode could be deleted (sub r1.rgb, v0,v0), why doesn't it?
  Result := False;

  // Mark only R0 (and discard) as initially 'read', as these may not result in a removal :
  ZeroMemory(@RegUsage, SizeOf(RegUsage));
  RegUsage[PARAM_R, 0] := MASK_RGBA;
  for i := 0 to High(RegUsage[PARAM_DISCARD]) do
    RegUsage[PARAM_DISCARD, i] := MASK_RGBA;

  i := IntermediateCount;
  while i > 0 do
  begin
    Dec(i);
    Cur := @(Intermediate[i]);
    if not Cur.IsArithmetic then
      Continue;

    // Loop over the output arguments :
    for j := 0 to PSH_OPCODE_DEFS[Cur.Opcode]._Out - 1 do
    begin
      CurArg := @(Cur.Output[j]);

      // Remove useless flag, to ease up later comparisions :
      CurArg.Modifiers := CurArg.Modifiers - [ARGMOD_IDENTITY];

      // Discard useless writes :
      if  (CurArg.Address < PSH_PC_MAX_R_REGISTER_COUNT)
      and ((RegUsage[CurArg.Type_, CurArg.Address] and CurArg.Mask) = 0) then
      begin
        DbgPrintf('; Removed useless assignment to register ' + CurArg.ToString);
        CurArg.Type_ := PARAM_DISCARD;
      end;
    end;

    // Loop over the input arguments :
    for j := 0 to PSH_OPCODE_DEFS[Cur.Opcode]._In - 1 do
    begin
      CurArg := @(Cur.Parameters[j]);
      // Skip non-register parameters :
      if not CurArg.UsesRegister then
        Continue;

      // Remove useless flag, to ease up later comparisions :
      CurArg.Modifiers := CurArg.Modifiers - [ARGMOD_IDENTITY];

      // Keep track of all register reads, so that we can discard useless writes :
      if CurArg.Address < PSH_PC_MAX_R_REGISTER_COUNT then
        RegUsage[CurArg.Type_, CurArg.Address] := RegUsage[CurArg.Type_, CurArg.Address] or CurArg.Mask;
    end;
  end;
end; // RemoveUselessWrites

procedure PSH_XBOX_SHADER.ConvertXboxOpcodesToNative(pPSDef: PX_D3DPIXELSHADERDEF);
var
  i: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
  NewIns: PSH_INTERMEDIATE_FORMAT;
begin
  // Do a bottom-to-top pass, converting all xbox opcodes into a native set of opcodes :
  i := IntermediateCount;
  while i > 0 do
  begin
    Dec(i);
    Cur := @(Intermediate[i]);

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
end; // ConvertXboxOpcodesToNative

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
    // TODO : If CombinerMuxesOnMsb is False, we should compare to the LeastSignificantBit of r0.a - but how?
    Cur.Opcode := PO_CND;
    Cur.Modifier := INSMOD_NONE;
    // Begin the input of CND with the required r0.a parameter :
    Cur.Parameters[0].SetRegister(PARAM_R, 0, MASK_A);
    Cur.Parameters[0].Modifiers := [ARGMOD_IDENTITY];
    Cur.Parameters[0].Multiplier := 1.0;
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
  // 'final combiner - r0 = A*B + (1-A)*C + D';

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
      end;

      PARAM_EF_PROD:
      begin
        // Change PROD into a fake register, which will be resolved later :
        CurArg.Type_ := PARAM_R;
        CurArg.Address := FakeRegNr_Prod;
        NeedsProd := True;
      end;

      PARAM_FOG:
      begin
        // Change FOG into a constant of 1.0, as we can't simulate it otherwise :
//        CurArg.SetConstValue(1.0);
//        Cur.CommentString := 'final combiner - FOG not emulated, using 1.';
      end;
    end;
  end; // for input

  if NeedsSum then
  begin
    // Add a new opcode that calculates r0*v1 :
    Ins.Initialize(PO_MUL);
    Ins.Output[0].SetRegister(PARAM_R, FakeRegNr_Sum, MASK_RGBA);

    Ins.Parameters[0].SetRegister(PARAM_R, 0, MASK_RGB);
    Ins.Parameters[1].SetRegister(PARAM_V, 1, MASK_RGB);

    // Take the FinalCombinerFlags that influence this result into account :
    if ((FinalCombinerFlags and PS_FINALCOMBINERSETTING_COMPLEMENT_R0) > 0) then
      Ins.Parameters[0].Modifiers := [ARGMOD_INVERT]; // (1-r0) is used as an input to the sum rather than r0
    if ((FinalCombinerFlags and PS_FINALCOMBINERSETTING_COMPLEMENT_V1) > 0) then
      Ins.Parameters[1].Modifiers := [ARGMOD_INVERT]; // (1-v1) is used as an input to the sum rather than v1
    if ((FinalCombinerFlags and PS_FINALCOMBINERSETTING_CLAMP_SUM) > 0) then
      Ins.Modifier := INSMOD_SAT; // V1+R0 sum clamped to [0,1]

    InsertIntermediate(@Ins, InsertPos);
    Inc(InsertPos);
    DbgPrintf('; Inserted final combiner calculation of V1R0_sum register');
  end;

  if NeedsProd then
  begin
    // Add a new opcode that calculates E*F :
    Ins.Initialize(PO_MUL);
    Ins.Output[0].SetRegister(PARAM_R, FakeRegNr_Prod, MASK_RGBA);
    Ins.Parameters[0] := Cur.Parameters[4]; // E
    Ins.Parameters[1] := Cur.Parameters[5]; // F
    InsertIntermediate(@Ins, InsertPos);
    Inc(InsertPos);
    DbgPrintf('; Inserted final combiner calculation of EF_prod register');
  end;

  // The final combiner calculates : r0.rgb=s0*s1 + (1-s0)*s2 + s3
  // Change that into a LRP + ADD, and let the optimizer reduce it;

  // Add a new opcode that calculates r0.rgb=s0*s1 + (1-s0)*s2 via a LRP :
  // Set the output to r0.rgb (as r0.a is determined via s6.a) :

  // Watch out! If s3=r0.rgb, then the LRP cannot use r0, but must use r1 as temp!
  if Cur.Parameters[3].IsRegister(PARAM_R, 0, 0) then
    Cur.Output[0].SetRegister(PARAM_R, 1, MASK_RGB)
  else
    Cur.Output[0].SetRegister(PARAM_R, 0, MASK_RGB);

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
    Ins.Output[0].SetRegister(PARAM_R, 0, MASK_A);
    Ins.Parameters[0] := Cur.Parameters[6];
    InsertIntermediate(@Ins, InsertPos);
//    Inc(InsertPos);
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

function PSH_XBOX_SHADER.IsRegisterFreeFromIndexOnwards(aIndex: int; aRegType: PSH_ARGUMENT_TYPE; aAddress: Int16): Boolean;
var
  i: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
begin
  for i := aIndex to IntermediateCount - 1 do
  begin
    Cur := @(Intermediate[i]);
    // Detect a write or read :
    if Cur.WritesToRegister(aRegType, aAddress)
    or Cur.ReadsFromRegister(aRegType, aAddress) then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

procedure PSH_XBOX_SHADER.ReplaceRegisterFromIndexOnwards(aIndex: int;
  aSrcRegType: PSH_ARGUMENT_TYPE; aSrcAddress: Int16;
  aDstRegType: PSH_ARGUMENT_TYPE; aDstAddress: Int16);
var
  i: int;
  j: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
begin
  for i := aIndex to IntermediateCount - 1 do
  begin
    Cur := @(Intermediate[i]);

    for j := 0 to PSH_OPCODE_DEFS[Cur.Opcode]._Out - 1 do
      if Cur.Output[j].IsRegister(aSrcRegType, aSrcAddress) then
        Cur.Output[j].SetRegister(aDstRegType, aDstAddress, Cur.Output[j].Mask);

    for j := 0 to PSH_OPCODE_DEFS[Cur.Opcode]._In - 1 do
      if Cur.Parameters[j].IsRegister(aSrcRegType, aSrcAddress) then
        Cur.Parameters[j].SetRegister(aDstRegType, aDstAddress, Cur.Parameters[j].Mask);
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
    if (ParamLeft.Type_ <> ParamRight.Type_)
    or (ParamLeft.Address <> ParamRight.Address)
    or (ParamLeft.Mask <> ParamRight.Mask) then
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
  Op0: PPSH_INTERMEDIATE_FORMAT;
  Op1: PPSH_INTERMEDIATE_FORMAT;
  Op2: PPSH_INTERMEDIATE_FORMAT;
  CanOptimize: boolean;
  j: int;
  k: int;
begin
  Result := False;

  i := IntermediateCount - 1;
  while i > 0 do
  begin
    Dec(i);
    Op0 := @(Intermediate[i+0]);
    Op1 := @(Intermediate[i+1]);
    Op2 := @(Intermediate[i+2]);

    // Check if there are two consecutive opcodes reading from a fake R register;
    // We outputted these ourselves, in order to ease the conversion and profit
    // from having generic optimizations in one place :
    if  (Op0.Output[0].Type_ = PARAM_R)
    and (Op0.Output[0].Address >= PSH_XBOX_MAX_R_REGISTER_COUNT)
    and (Op1.Output[0].Type_ = PARAM_R)
    and (Op1.Output[0].Address >= PSH_XBOX_MAX_R_REGISTER_COUNT) then
    begin
      // Did we output those from a CND opcode (originally XMMC) ?
      if (Op2.Opcode = PO_CND) then
      begin
        if  (Op0.Opcode = PO_MOV)
        and (Op1.Opcode = Op0.Opcode)
        and (Op1.Modifier = Op0.Modifier) then
        begin
          Op2.Modifier := Op0.Modifier;
          Op2.Parameters[1] := Op0.Parameters[0];
          Op2.Parameters[2] := Op1.Parameters[0];
          DeleteIntermediate(i);
          DeleteIntermediate(i);
          DbgPrintf('; Changed temporary MUL,MUL,CND via MOV,MOV,CND into a single CND');
          Result := True;
          Continue;
        end;
      end;

      // Did we output those from a ADD opcode (originally XMMA) ?
      if (Op2.Opcode = PO_ADD) then
      begin
        if  (Op0.Opcode = PO_MUL)
        and (Op1.Opcode = Op0.Opcode)
        and (Op1.Modifier = Op0.Modifier) then
        begin
          // Check if we can lerp - we just need the same register on both sides that's inverted on the other :
          if _CanLerp(Op0, Op1, Op2, 0, 2)
          or _CanLerp(Op0, Op1, Op2, 1, 2)
          or _CanLerp(Op0, Op1, Op2, 0, 3)
          or _CanLerp(Op0, Op1, Op2, 1, 3) then
          begin
            // The lerp can be done, and the correct parameters are already set to Op2,
            // so all we need to do now, it fixup the rest and remove the two MOV's :
            Op2.Opcode := PO_LRP;
            Op2.Modifier := Op0.Modifier;
            DeleteIntermediate(i);
            DeleteIntermediate(i);
            DbgPrintf('; Changed temporary MUL,MUL,ADD into a single LRP');
            Result := True;
            Continue;
          end;

          // Check if we can mad - we just need a constant 1 in one argument :
          if _CanMad(0, Op0, Op1, Op2)
          or _CanMad(1, Op0, Op1, Op2)
          or _CanMad(0, Op1, Op0, Op2)
          or _CanMad(1, Op1, Op0, Op2) then
          begin
            // The mad can be done, and the correct parameters are already set to Op2,
            // so all we need to do now, it fixup the rest and remove the two MOV's :
            Op2.Opcode := PO_MAD;
            Op2.Modifier := Op0.Modifier;
            DeleteIntermediate(i);
            DeleteIntermediate(i);
            DbgPrintf('; Changed temporary MUL,MUL,ADD into a single MAD');
            Result := True;
            Continue;
          end;

          // No single opcode possible, so change it into a MUL + MAD :
          // The first mul may write to the last output register (without a modifier) :
          Op0.Modifier := INSMOD_NONE;
          Op0.Output[0] := Op2.Output[0];
          // Change the second MUL into a MAD :
          Op1.Opcode := PO_MAD;
          Op1.Output[0] := Op2.Output[0];
          Op1.Parameters[2] := Op0.Output[0];
          // Remove the trailing ADD :
          DeleteIntermediate(i+2);
          DbgPrintf('; Changed temporary MUL,MUL,ADD into a MUL,MAD');
          Result := True;
          Continue;
        end;

        // Was it a MUL,MUL,ADD?
        if  (Op0.Opcode = PO_MUL)
        and (Op1.Opcode = PO_MUL)
        and (Op0.Parameters[1].GetConstValue = 1.0)
        and (Op1.Parameters[1].GetConstValue = 1.0) then
        begin
          // Remove the two MOV's and fold their arguments into a MUL :
          Op2.Opcode := PO_MUL;
          Op2.Parameters[0] := Op0.Parameters[0];
          Op2.Parameters[1] := Op1.Parameters[0];
          DeleteIntermediate(i);
          DeleteIntermediate(i);
          DbgPrintf('; Changed temporary MUL,MUL,ADD into a MUL');
          Result := True;
          Continue;
        end;
      end;
    end;

    // Do two neighbouring opcodes output to the same register (without a modifier) ?
    if  (Op0.Output[0].ToString = Op1.Output[0].ToString)
    and (Op0.Modifier = INSMOD_NONE)
    and (Op1.Modifier = INSMOD_NONE) then
    begin
      // Is it MUL,ADD ?
      if  (Op0.Opcode = PO_MUL)
      and (Op1.Opcode = PO_ADD) then
      begin
        // Is the output of the MUL input to the ADD ?
        if  (Op0.Output[0].Type_ = Op1.Parameters[0].Type_)
        and (Op0.Output[0].Address = Op1.Parameters[0].Address)
        and (Op0.Output[0].Modifiers = Op1.Parameters[0].Modifiers) then
        // Mask and Multiplier are not important here
        begin
          Op0.Opcode := PO_MAD;
          Op0.Parameters[2] := Op1.Parameters[1];
          DeleteIntermediate(i+1);
          DbgPrintf('; Changed MUL,ADD into a single MAD');
          Result := True;
          Continue;
        end;
      end;
    end;

(*
    // Combinations that can be made if their intermediate result is not read again or overwritten later:

    MOV+ADD > ADD (if MOV.Output[0] was only read by ADD.Parameter[0] or ADD.Parameter[1])
    MOV+SUB > SUB (if MOV.Output[0] was only read by SUB.Parameter[0] or SUB.Parameter[1])
    MOV+MUL > MUL (if MOV.Output[0] was only read by MOV.Parameter[0] or MOV.Parameter[1])

    MUL+MOV > MUL (if MUL.Output[0] was only read by MOV.Parameter[0])
    MUL+ADD > MAD (if MUL.Output[0] was only read by ADD.Parameter[0] or ADD.Parameter[1])
    MUL+SUB > MAD (if MUL.Output[0] was only read by SUB.Parameter[0] - Do invert MAD.Parameter[2])
*)

    // We can remove a MOV entirely if the input is not changed while
    // the output is read, up until the output is re-written; We can change all
    // these occurances into a read from the input of this MOV instead :
    // This fixes some shaders in Turok, that are reduced to 8 instead of 9 opcodes.
    if  (Op0.Opcode = PO_MOV)
    and (Op0.Modifier = INSMOD_NONE)
    and (Op0.Output[0].Mask = MASK_RGBA) then
    begin
      CanOptimize := False;
      j := i + 1;
      while j < IntermediateCount do
      begin
        // Don't optimize if the output is needed for CND or CMP (which must read from r0) :
        // This fixes : "(Validation Error) First source for cnd instruction must be 'r0.a'" in Modify Pixel Shader XDK sample.
        if  (Intermediate[j].Opcode in [PO_CND, PO_CMP])
        and (Op0.Output[0].Type_ = PARAM_R)
        and (Op0.Output[0].Address = 0) then
          Break;

        // TODO : Add other prevention rules here (like too many texture-reads, and other scases)

        // We can optimize if the MOV-output is written to again before the end of the shader :
        CanOptimize := True;
        if Intermediate[j].WritesToRegister(Op0.Output[0].Type_, Op0.Output[0].Address, MASK_RGBA) then
          Break;

        CanOptimize := False;
        Inc(j);
      end;

      if CanOptimize then
      begin
        // Loop over all instructions in between, and try to replace reads :
        CanOptimize := False;
        while j > i do
        begin
          // For Intermediate[j].Parameters, change all occurrances of Op0.Output[0] into Op0.Parameters[0] :
          for k := 0 to PSH_OPCODE_DEFS[Intermediate[j].Opcode]._In - 1 do
            if  (Intermediate[j].Parameters[k].Type_ = Op0.Output[0].Type_)
            and (Intermediate[j].Parameters[k].Address = Op0.Output[0].Address) then
            begin
              Intermediate[j].Parameters[k].Type_ := Op0.Parameters[0].Type_;
              Intermediate[j].Parameters[k].Address := Op0.Parameters[0].Address;
              // Signal that a replacement is actually done :
              CanOptimize := True;
            end;

          Dec(j);
        end;

        if CanOptimize then
        begin
          DeleteIntermediate(i);
          DbgPrintf('; Moved MOV input into following instructions');
          Result := True;
        end;
      end;
    end;

    // Fix Dolphin :
    //  mul r3, r0,t0 ; d0=s0*s1
    //  mov r0.rgb, r3 ; d0=s0 final combiner - FOG not emulated, using 1.
    if  (Op0.Output[0].Type_ = PARAM_R)
    and (Op0.Output[0].Address >= PSH_XBOX_MAX_R_REGISTER_COUNT)
    and (Op1.Parameters[0].Type_ = PARAM_R)
    and (Op1.Parameters[0].Address >= PSH_XBOX_MAX_R_REGISTER_COUNT) then
    begin
      if  (Op0.Opcode = PO_MUL)
      and (Op1.Opcode = PO_MOV) then
      begin
        // > mul r0.rgb, r0,t0
        Op0.Output[0] := Op1.Output[0];
        DeleteIntermediate(i+1);
        DbgPrintf('; Changed temporary MUL,MOV into a MUL');
        Result := True;
        Continue;
      end;
    end;

    // Fix Crash bandicoot xfc leftover r3 :
    if  (Op0.Output[0].Type_ = PARAM_R)
    and (Op0.Output[0].Address = FakeRegNr_Prod) then
    begin
      // The final combiner uses r3, try to use r1 instead :
      if IsRegisterFreeFromIndexOnwards(i, PARAM_R, 1) then
      begin
        ReplaceRegisterFromIndexOnwards(i, Op0.Output[0].Type_, Op0.Output[0].Address, PARAM_R, 1);
        DbgPrintf('; Changed fake register by r1');
        Result := True;
        Continue;
      end;
    end;
  end; // while
end; // CombineInstructions

function PSH_XBOX_SHADER.SimplifyMOV(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
var
  CanSimplify: Boolean;
  Factor: Float;
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
      DbgPrintf('; Changed MOV into a NOP');
      Result := True;
      Exit;
    end;
  end;

  // Does this MOV put a 0 (zero) in the output?
  if Cur.Parameters[0].GetConstValue = 0.0 then
  begin
    // TODO : Find a constant with the value 0, and use that if present.
    // Simulate 0 by subtracting a (guaranteed) register from itself :
    // Fixup via "sub d0=v0,v0" :
    Cur.Opcode := PO_SUB;
    Cur.Parameters[0].Type_ := PARAM_V;
    Cur.Parameters[0].Address := 0;
    Cur.Parameters[0].Modifiers := [];
    Cur.Parameters[1] := Cur.Parameters[0];
    DbgPrintf('; Changed MOV 0 into a SUB v0,v0');
    Exit;
  end;

  // Does this MOV put a constant in the output?
  if Cur.Parameters[0].Type_ = PARAM_VALUE then
  begin
    // TODO : If there's a constant equal to GetConstValue(), use that.
    Factor := Cur.Parameters[0].GetConstValue();

    // Fixup via a SUB (which can calculate a constant value) :
    Cur.Opcode := PO_SUB;
    Cur.Parameters[0].Type_ := PARAM_V;
    Cur.Parameters[0].Address := 0;

    if Factor < 0.0 then
    begin
      // Simulate -1 by calculating it via a (guaranteed) register :
      // We follow this : (-v0) - (1-v0) = -v0 - 1 + v0 = -1
      Cur.Parameters[0].Modifiers := [ARGMOD_NEGATE];
      Cur.Parameters[1] := Cur.Parameters[0];
      Cur.Parameters[1].Modifiers := [ARGMOD_INVERT];
      // Go on with a positive factor, to ease the scaling :
      Factor := -Factor;
    end
    else
    begin
      // Simulate 1 by calculating it via a (guaranteed) register :
      // We follow this : (1-v0) - (-v0) = (1-v0) + v0 = 1
      Cur.Parameters[0].Modifiers := [ARGMOD_INVERT];
      Cur.Parameters[1] := Cur.Parameters[0];
      Cur.Parameters[1].Modifiers := [ARGMOD_NEGATE];
    end;

    // Try to simulate all factors (0.5, 1.0 and 2.0) using an output modifier :
    Cur.ScaleOutput(Factor);

    DbgPrintf('; Changed MOV {const} into a SUB_factor 1-v0,-v0');
    Exit;
  end;
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
    DbgPrintf('; Changed ADD s0,0 into a MOV s0');
    Exit;
  end;
end;

function PSH_XBOX_SHADER.SimplifyMAD(Cur: PPSH_INTERMEDIATE_FORMAT): Boolean;
begin
  Result := False;

  // Is this s0*0+s2 ?
  if Cur.Parameters[1].GetConstValue = 0.0 then
  begin
    // Change it into s2 :
    Cur.Opcode := PO_MOV;
    Cur.Parameters[0] := Cur.Parameters[2];
    Result := True;
    DbgPrintf('; Changed MAD s0,0 into a MOV s0');
    Exit;
  end;

  // Is this s0*1+s2 ?
  if Cur.Parameters[1].GetConstValue = 1.0 then
  begin
    // Change it into s0+s2 :
    Cur.Opcode := PO_ADD;
    Cur.Parameters[1] := Cur.Parameters[2];
    Result := True;
    DbgPrintf('; Changed MAD s0,1,s2 into a ADD s0,s2');
    Exit;
  end;

  // Is this s0*-1+s2 ?
  if Cur.Parameters[1].GetConstValue = -1.0 then
  begin
    // Change it into s2-s0 :
    Cur.Opcode := PO_SUB;
    Cur.Parameters[1] := Cur.Parameters[0];
    Cur.Parameters[0] := Cur.Parameters[2];
    Result := True;
    DbgPrintf('; Changed MAD s0,-1,s2 into a SUB s2,s0');
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
    DbgPrintf('; Changed SUB x, 0 into a MOV x');
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
    DbgPrintf('; Changed MUL s0,0 into a MOV 0');
    Exit;
  end;

  // Is this a multiply-by-const ?
  if (Cur.Parameters[1].Type_ = PARAM_VALUE) then
  begin
    // Change it into a simple MOV and scale the output instead :
    Cur.Opcode := PO_MOV;
    Cur.ScaleOutput(Cur.Parameters[1].GetConstValue());
    Result := True;
    DbgPrintf('; Changed MUL s0,{const} into a MOV_factor s0');
    Exit;
  end;
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
    DbgPrintf('; Changed LRP s0,s1,s2 (where (1-s0)*s2=0) into a MUL s0,s1');
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
    DbgPrintf('; Changed LRP s0,s1,s2 (where s0*s1=0) into a MUL (1-s0),s2');
    Exit;
  end;

  // Is it d0=s0*s1+(1-s0)*1 ?
  if (Cur.Parameters[2].GetConstValue = 1.0) then
  begin
    // Change it into a d0=s0*s1+(1-s0)
    Cur.Opcode := PO_MAD;
    Cur.Parameters[2] := Cur.Parameters[0];
    Cur.Parameters[2].Invert;
    Result := True;
    DbgPrintf('; Changed LRP s0,s1,1 into a MAD s0,s1,1-s0');
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
  // TODO : Condense constants registers, to avoid the non-existant C8-C15 (requires a mapping in SetPixelShaderConstant too...)
  // TODO : Convert numeric arguments (-2, -1, 0, 1, 2) into modifiers on the other argument
  // TODO : Complete to port to D3D9 to support all 18 constants (including C8..C15 + FC0+FC1)

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

      PO_MAD:
        if SimplifyMAD(Cur) then
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
// On the Xbox, the alpha portion of the R0 register is initialized to
// the alpha component of texture 0 if texturing is enabled for texture 0 :
var
  R0aDefaultInsertPos: int;
  i: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
  NewIns: PSH_INTERMEDIATE_FORMAT;
begin
  Result := False;

  // Detect a read of r0.a without a write, as we need to insert a "MOV r0.a, t0.a" as default (like the xbox has) :
  R0aDefaultInsertPos := -1;
  for i := 0 to IntermediateCount - 1 do
  begin
    Cur := @(Intermediate[i]);
    if not Cur.IsArithmetic then
      Continue;

    // Make sure if we insert at all, it'll be after the DEF's :
    if R0aDefaultInsertPos < 0 then
      R0aDefaultInsertPos := i;

    // First, check if r0.a is read by this opcode :
    if Cur.ReadsFromRegister(PARAM_R, 0, MASK_A) then
    begin
      R0aDefaultInsertPos := i;
      Break;
    end;

    // If this opcode writes to r0.a, we're done :
    if Cur.WritesToRegister(PARAM_R, 0, MASK_A) then
      Exit;
  end;

  if R0aDefaultInsertPos >= 0 then
  begin
    // Insert a new opcode : MOV r0.a, t0.a
    NewIns.Initialize(PO_MOV);
    NewIns.Output[0].SetRegister(PARAM_R, 0, MASK_A);
    NewIns.Parameters[0] := NewIns.Output[0];
    NewIns.Parameters[0].Type_ := PARAM_T;
    NewIns.CommentString := 'Inserted r0.a default';
    InsertIntermediate(@NewIns, R0aDefaultInsertPos);
    Result := True;
  end;
end; // FixMissingR0a

function PSH_XBOX_SHADER.FixCoIssuedOpcodes(): Boolean;
var
  PrevMask: DWORD;
  PrevOpcode: PSH_OPCODE;
  i: int;
  Cur: PPSH_INTERMEDIATE_FORMAT;
//  j: int;
  NewIsCombined: boolean;
begin
  Result := False;

(*
  // TODO : Shift independent .a instructions up or down so the alpha write combiner can be used more often :
  for i := 0 to IntermediateCount - 1 do
  begin
    Cur := @(Intermediate[i]);
    // Is this an arithmetic opcode?
    if Cur.Opcode > PO_TEX then
    begin
      // Does this instruction write solely to Alpha?
      if (Cur.Output[0].Mask = MASK_A) then
      begin
        // Look at prior instructions :
        for j := i - 1 downto 0 do
        begin
          // Because "dp3" needs the color/vector pipeline, no color component outputing opcode can be co-issued with it :
          if (Intermediate[j].Opcode = PO_DP3) then
            Break;

          // TODO : Test that none of the inputs of 'Cur' are written to (break otherwise)

          // Does a prior instruction skip alpha?
          if (Intermediate[j].Output[0].Mask and MASK_A) = 0 then
          begin
            // TODO : Move instruction up to right below j
            // Result := True;
            // Break;
          end;
        end;
      end;
    end;
  end;
*)

  // Update IsCombined flags :
  // Start with Alpha, so the first opcode doesn't become a write-combined opcode (which isn't possible) :
  PrevMask := MASK_A;
  PrevOpcode := PO_COMMENT;
  for i := 0 to IntermediateCount - 1 do
  begin
    Cur := @(Intermediate[i]);
    // Is this an arithmetic opcode?
    if Cur.Opcode > PO_TEX then
    begin
      // Set IsCombined only when previous opcode doesn't write to Alpha, while this opcode writes only to Alpha :
      NewIsCombined := (PrevOpcode <> PO_DP3)
                   and ((PrevMask and MASK_A) = 0)
                   and (Cur.Output[0].Mask = MASK_A);

      if Cur.IsCombined <> NewIsCombined then
      begin
        Cur.IsCombined := NewIsCombined;
        Result := True;
      end;

      PrevMask := Cur.Output[0].Mask;
      PrevOpcode := Cur.Opcode;
    end;
  end;
end;

// TODO : FocusBlur sample needs a zero in 'cnd' opcode

{ RPSRegisterObject }

procedure RPSRegisterObject.Decode(Value: Byte; aIsAlpha: Boolean);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  IsAlpha := aIsAlpha;
  Reg := PS_REGISTER(Value);
end;

function RPSRegisterObject.DecodedToString(): string;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Assert((PS_REGISTER_DISCARD <= Reg) and (Reg <= PS_REGISTER_EF_PROD));

  Result := PS_RegisterStr[Ord(Reg) + 1];
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
end;

function RPSInputRegister.DecodedToString(): string;
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
    Result := inherited DecodedToString();
    InputMappingStr := ' | ' + PS_InputMappingStr[(Ord(InputMapping) shr 5) and 7];
  end;

  // Render the channel as a string :
  Result := Result + ' | ' + PS_ChannelStr[iif(Ord(Channel) > 0, {Alpha}2, iif(IsAlpha, {Blue}1, {RGB}0))] + InputMappingStr;
end;

{ RPSCombinerOutput }

procedure RPSCombinerOutput.Decode(Value: Byte; PSInputs: DWORD; aIsAlpha: Boolean);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  inherited Decode(Value, aIsAlpha);

  // Decode PSAlphaInputs / PSRGBInputs :
  Input1.Decode((PSInputs shr  8) and $FF, IsAlpha);
  Input2.Decode((PSInputs shr  0) and $FF, IsAlpha);
end;

{ RPSCombinerStageChannel }

procedure RPSCombinerStageChannel.Decode(PSInputs, PSOutputs: DWORD; IsAlpha: Boolean = False);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  // Get the combiner output flags :
  CombinerOutputFlags := PS_COMBINEROUTPUT(PSOutputs shr 12);

  // Decompose the combiner output flags :
  OutputSUM.OutputAB.DotProduct := (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_DOT_PRODUCT) > 0; // False=Multiply, True=DotProduct
  OutputSUM.OutputCD.DotProduct := (CombinerOutputFlags and PS_COMBINEROUTPUT_CD_DOT_PRODUCT) > 0; // False=Multiply, True=DotProduct

  if not IsAlpha then
  begin
    OutputSUM.OutputAB.BlueToAlpha := (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA) > 0; // False=Alpha-to-Alpha, True=Blue-to-Alpha
    OutputSUM.OutputCD.BlueToAlpha := (CombinerOutputFlags and PS_COMBINEROUTPUT_CD_BLUE_TO_ALPHA) > 0; // False=Alpha-to-Alpha, True=Blue-to-Alpha
  end;

  // Decode PSAlphaOutputs / PSRGBOutputs and PSAlphaInputs / PSRGBInputs :
  OutputSUM.OutputAB.Decode((PSOutputs shr 4) and $F, (PSInputs shr 16) and $FFFF, IsAlpha);
  OutputSUM.OutputCD.Decode((PSOutputs shr 0) and $F, (PSInputs shr  0) and $FFFF, IsAlpha);
  OutputSUM.Decode((PSOutputs shr 8) and $F, IsAlpha);

  AB_CD_SUM := (CombinerOutputFlags and PS_COMBINEROUTPUT_AB_CD_MUX) = 0; // True=AB+CD, False=MUX(AB,CD) based on R0.a
end;

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

{static}var PshNumber: int = 0; // Keep track of how many pixel shaders we've attempted to convert.
procedure XTL_DumpPixelShaderToFile(pPSDef: PX_D3DPIXELSHADERDEF);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  PSH: PSH_XBOX_SHADER;
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
    fprintf(out_, PAnsiChar(AnsiString(PSH.OriginalToString(pPSDef))));
    fclose(out_);
  end;
end;

function XTL_EmuRecompilePshDef(pPSDef: PX_D3DPIXELSHADERDEF): PSH_RECOMPILED_SHADER;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  PSH: PSH_XBOX_SHADER;
begin
  Result := PSH.Decode(pPSDef);
end;

end.

