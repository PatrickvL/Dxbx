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

// PS_TEXTUREMODES(t0, t1, t2, t3) = ((t3 shl 15) or (t2 shl 10) or (t1 shl 5) or t0);


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
                             t+mat10*src.r+mat11*src.g);
                rgb *= (lum_scale*src.b + lum_bias); (a is not affected)
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

// PS_DOTMAPPING(t0, t1, t2, t3) = ((t3 shl 8) or (t2 shl 4) or t1);


// Mappings:
// ZERO_TO_ONE         :rgb->(r,g,b): 0x0=>0.0, 0xff=>1.0
// MINUS1_TO_1_D3D     :rgb->(r,g,b): 0x0=>-128/127, 0x01=>-1.0, 0x80=>0.0, 0xff=>1.0
// MINUS1_TO_1_GL      :rgb->(r,g,b): 0x80=>-1.0, 0x7f=>1.0
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

// PS_COMPAREMODE(t0, t1, t2, t3) = ((t3 shl 12) or (t2 shl 8) or (t1 shl 4) or t0);

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

// PS_INPUTTEXTURE(t0, t1, t2, t3) = ((t3 shl 20) or (t2 shl 16));


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

// PS_COMBINERCOUNT(count, flags) = ((flags shl 8) or count);
// count is 1-8, flags contains one or more values from PS_COMBINERCOUNTFLAGS

type PS_COMBINERCOUNTFLAGS =
(
    PS_COMBINERCOUNT_MUX_LSB=     $0000, // mux on r0.a lsb
    PS_COMBINERCOUNT_MUX_MSB=     $0001, // mux on r0.a msb

    PS_COMBINERCOUNT_SAME_C0=     $0000, // c0 same in each stage
    PS_COMBINERCOUNT_UNIQUE_C0=   $0010, // c0 unique in each stage

    PS_COMBINERCOUNT_SAME_C1=     $0000, // c1 same in each stage
    PS_COMBINERCOUNT_UNIQUE_C1=   $0100  // c1 unique in each stage
);


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
//     PS_REGISTER_ZERO);
//
// shader.PSFinalCombinerInputsABCD= PS_COMBINERINPUTS(
//     PS_REGISTER_T0     | PS_INPUTMAPPING_UNSIGNED_IDENTITY | PS_CHANNEL_ALPHA,
//     PS_REGISTER_ZERO   | PS_INPUTMAPPING_EXPAND_NORMAL     | PS_CHANNEL_RGB,
//     PS_REGISTER_EFPROD | PS_INPUTMAPPING_UNSIGNED_INVERT   | PS_CHANNEL_RGB,
//     PS_REGISTER_ZERO);
//
// PS_FINALCOMBINERSETTING is set in 4th field of PSFinalCombinerInputsEFG with PS_COMBINERINPUTS
// example:
//
// shader.PSFinalCombinerInputsEFG= PS_COMBINERINPUTS(
//     PS_REGISTER_R0 | PS_INPUTMAPPING_UNSIGNED_IDENTITY | PS_CHANNEL_RGB,
//     PS_REGISTER_R1 | PS_INPUTMAPPING_UNSIGNED_IDENTITY | PS_CHANNEL_RGB,
//     PS_REGISTER_R1 | PS_INPUTMAPPING_UNSIGNED_IDENTITY | PS_CHANNEL_BLUE,
//    PS_FINALCOMBINERSETTING_CLAMP_SUM | PS_FINALCOMBINERSETTING_COMPLEMENT_R0);

// PS_COMBINERINPUTS(a,b,c,d) = ((a shl 24) or (b shl 16) or (c shl 8) or d);

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

    PS_FINALCOMBINERSETTING_COMPLEMENT_R0= $20  // unsigned invert mapping  (1 - r1) is used as an input to the sum rather than r1
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

// PS_COMBINEROUTPUTS(ab,cd,mux_sum,flags) = ((flags shl 12) or (mux_sum shl 8) or (ab shl 4) or cd);

// ab,cd,mux_sum contain a value from PS_REGISTER
// flags contains values from PS_COMBINEROUTPUT

type PS_COMBINEROUTPUT =
(
    PS_COMBINEROUTPUT_IDENTITY=            $00, // y = x
    PS_COMBINEROUTPUT_BIAS=                $08, // y = x - 0.5
    PS_COMBINEROUTPUT_SHIFTLEFT_1=         $10, // y = x*2
    PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS=    $18, // y = (x - 0.5)*2
    PS_COMBINEROUTPUT_SHIFTLEFT_2=         $20, // y = x*4
    PS_COMBINEROUTPUT_SHIFTRIGHT_1=        $30, // y = x/2

    PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA=    $80, // RGB only

    PS_COMBINEROUTPUT_CD_BLUE_TO_ALPHA=    $40, // RGB only

    PS_COMBINEROUTPUT_AB_MULTIPLY=         $00,
    PS_COMBINEROUTPUT_AB_DOT_PRODUCT=      $02, // RGB only

    PS_COMBINEROUTPUT_CD_MULTIPLY=         $00,
    PS_COMBINEROUTPUT_CD_DOT_PRODUCT=      $01, // RGB only

    PS_COMBINEROUTPUT_AB_CD_SUM=           $00, // 3rd output is AB+CD
    PS_COMBINEROUTPUT_AB_CD_MUX=           $04  // 3rd output is MUX(AB,CD) based on R0.a
);

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

//function PS_CONSTANTMAPPING(s0,s1,s2,s3,s4,s5,s6,s7:): ;
//begin
//  Result := ((DWORD(s0) and $f) shl  0) or ((DWORD(s1) and $f) shl 4) or
//            ((DWORD(s2) and $f) shl  8) or ((DWORD(s3) and $f) shl 12) or
//            ((DWORD(s4) and $f) shl 16) or ((DWORD(s5) and $f) shl 20) or
//            ((DWORD(s6) and $f) shl 24) or ((DWORD(s7) and $f) shl 28);
//end;
// s0-s7 contain the offset of the D3D constant that corresponds to the
// c0 or c1 constant in stages 0 through 7.  These mappings are only used in
// SetPixelShaderConstant().

// =========================================================================================================
// PSFinalCombinerConstants
// --------.--------.--------.----xxxx // offset of D3D constant for C0
// --------.--------.--------.xxxx---- // offset of D3D constant for C1
// --------.--------.-------x.-------- // Adjust texture flag

// PS_FINALCOMBINERCONSTANTS(c0,c1,flags) = ((DWORD(flags) shl 8) or (DWORD(c0) and $f) shl 0) or ((DWORD(c1) and $f) shl 4);

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

// dump pixel shader definition to file
procedure XTL_DumpPixelShaderDefToFile(pPSDef: PX_D3DPIXELSHADERDEF);
// dump pixel shader definition to string
function XTL_DumpPixelShaderDefToString(pPSDef: PX_D3DPIXELSHADERDEF): string;
// print relevant contents to the debug console
procedure XTL_PrintPixelShaderDefContents(pPSDef: PX_D3DPIXELSHADERDEF);
// Recompile Xbox PixelShader def
function XTL_EmuRecompilePshDef(pPSDef: PX_D3DPIXELSHADERDEF): AnsiString;

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
const PS_TextureModesStr: array [{PS_TEXTUREMODES=}0..32-1] of P_char =
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
    'PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST', // 0x12
    '???',                                  // 0x13
    '???',                                  // 0x14
    '???',                                  // 0x15
    '???',                                  // 0x16
    '???',                                  // 0x17
    '???',                                  // 0x18
    '???',                                  // 0x19
    '???',                                  // 0x1A
    '???',                                  // 0x1B
    '???',                                  // 0x1C
    '???',                                  // 0x1D
    '???',                                  // 0x1E
    '???'                                   // 0x1F
);

// PS DotMapping
const PS_DotMappingStr: array [{PS_DOTMAPPING=}0..8-1] of P_char =
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
const PS_InputMappingStr: array [{PS_INPUTMAPPING=}0..8-1] of P_char =
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
    'PS_REGISTER_ZERO',     // 0x00L, // r
    'PS_REGISTER_DISCARD',  // 0x00L, // w
    'PS_REGISTER_C0 ',       // 0x01L, // r
    'PS_REGISTER_C1 ',       // 0x02L, // r
    'PS_REGISTER_FOG',      // 0x03L, // r
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
    'PS_REGISTER_V1R0_SUM', // 0x0eL, // r
    'PS_REGISTER_EF_PROD',  // 0x0fL, // r

    'PS_REGISTER_ONE',               // PS_REGISTER_ZERO | PS_INPUTMAPPING_UNSIGNED_INVERT, // OK for final combiner
    'PS_REGISTER_NEGATIVE_ONE',      // PS_REGISTER_ZERO | PS_INPUTMAPPING_EXPAND_NORMAL,   // invalid for final combiner
    'PS_REGISTER_ONE_HALF',          // PS_REGISTER_ZERO | PS_INPUTMAPPING_HALFBIAS_NEGATE, // invalid for final combiner
    'PS_REGISTER_NEGATIVE_ONE_HALF'  // PS_REGISTER_ZERO | PS_INPUTMAPPING_HALFBIAS_NORMAL, // invalid for final combiner
);

// PS Channel
const PS_ChannelStr: array [{PS_CHANNEL=}0..3-1] of P_char =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_CHANNEL_RGB  ',  // 0x00, // used as RGB source
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


{static}var PshNumber: int = 0;  // Keep track of how many pixel shaders we've attempted to convert.
procedure XTL_DumpPixelShaderDefToFile(pPSDef: PX_D3DPIXELSHADERDEF);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  szPSDef: array [0..512-1] of AnsiChar;
  out_: PFILE;
begin
  sprintf(@szPSDef[0], 'PSDef%.03d.txt', [PshNumber]); Inc(PshNumber);

  out_ := fopen(szPSDef, 'w');
  if Assigned(out_) then
  begin
    fprintf(out_, PAnsiChar(AnsiString(XTL_DumpPixelShaderDefToString(pPSDef))));
    fclose(out_);
  end;
end;

function XTL_DumpPixelShaderDefToString(pPSDef: PX_D3DPIXELSHADERDEF): string;
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

function PSCombinerInputToStr(const dwPSFCI: DWORD; IsAlpha: Boolean = False): string;
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
        PS_RegisterStr[(dwPSFCI and $F) + 1],
        Result,
        PS_InputMappingStr[(dwPSFCI shr 5) and 7]
        ]);
    end;
  end;
end;

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

type
  RPSCombinerInput = record
    D: Byte;
    C: Byte;
    B: Byte;
    A: Byte;
  end;

// print relevant contents to the debug console
procedure XTL_PrintPixelShaderDefContents(pPSDef: PX_D3DPIXELSHADERDEF);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwPSTexMode0: DWORD;
  dwPSTexMode1: DWORD;
  dwPSTexMode2: DWORD;
  dwPSTexMode3: DWORD;

  dwPSDMStage1: DWORD;
  dwPSDMStage2: DWORD;
  dwPSDMStage3: DWORD;

  dwPSCMStage0: DWORD;
  dwPSCMStage1: DWORD;
  dwPSCMStage2: DWORD;
  dwPSCMStage3: DWORD;

  dwPSITStage2: DWORD;
  dwPSITStage3: DWORD;

  dwPSCCNumCombiners: DWORD;
  dwPSCCMux: DWORD;
  dwPSCCC0: DWORD;
  dwPSCCC1: DWORD;

  i: int;

  dwPSFCIA: DWORD;
  dwPSFCIB: DWORD;
  dwPSFCIC: DWORD;
  dwPSFCID: DWORD;

  dwPSFCIE: DWORD;
  dwPSFCIF: DWORD;
  dwPSFCIG: DWORD;
  dwPS_FINALCOMBINERSETTING: DWORD;

  dwPS_GLOBALFLAGS: DWORD;
begin
  // Show the contents to the user
  if Assigned(pPSDef) then
  begin
    DbgPshPrintf(#13#10'-----PixelShader Def Contents-----');
    DbgPshPrintf(XTL_DumpPixelShaderDefToString(pPSDef));

    if (pPSDef.PSTextureModes > 0) then
    begin
      dwPSTexMode0 := (pPSDef.PSTextureModes shr 0) and $1F;
      dwPSTexMode1 := (pPSDef.PSTextureModes shr 5) and $1F;
      dwPSTexMode2 := (pPSDef.PSTextureModes shr 10) and $1F;
      dwPSTexMode3 := (pPSDef.PSTextureModes shr 15) and $1F;

      DbgPshPrintf(#13#10'PSTextureModes ->'); // Texture addressing modes
      DbgPshPrintf('Stage 0: %s', [PS_TextureModesStr[dwPSTexMode0]]);
      DbgPshPrintf('Stage 1: %s', [PS_TextureModesStr[dwPSTexMode1]]);
      DbgPshPrintf('Stage 2: %s', [PS_TextureModesStr[dwPSTexMode2]]);
      DbgPshPrintf('Stage 3: %s', [PS_TextureModesStr[dwPSTexMode3]]);
    end;

    if (pPSDef.PSDotMapping > 0) then // Input mapping for dot product modes
    begin
      dwPSDMStage1 := (pPSDef.PSDotMapping shr 0) and $7;
      dwPSDMStage2 := (pPSDef.PSDotMapping shr 4) and $7;
      dwPSDMStage3 := (pPSDef.PSDotMapping shr 8) and $7;

      DbgPshPrintf(#13#10'PSDotMapping ->');
      DbgPshPrintf('Stage 1: %s', [PS_DotMappingStr[dwPSDMStage1]]);
      DbgPshPrintf('Stage 2: %s', [PS_DotMappingStr[dwPSDMStage2]]);
      DbgPshPrintf('Stage 3: %s', [PS_DotMappingStr[dwPSDMStage3]]);
    end;

    if (pPSDef.PSCompareMode > 0) then // Compare modes for clipplane texture mode
    begin
      dwPSCMStage0 := (pPSDef.PSCompareMode shr 0) and $F;
      dwPSCMStage1 := (pPSDef.PSCompareMode shr 4) and $F;
      dwPSCMStage2 := (pPSDef.PSCompareMode shr 8) and $F;
      dwPSCMStage3 := (pPSDef.PSCompareMode shr 12) and $F;

      DbgPshPrintf(#13#10'PSCompareMode ->');
      DbgPshPrintf('Stage 0: %s', [PS_CompareModeStr[iif(dwPSCMStage0 = 0, 0, 1)]]);
      DbgPshPrintf('Stage 1: %s', [PS_CompareModeStr[iif(dwPSCMStage1 = 0, 2, 3)]]);
      DbgPshPrintf('Stage 2: %s', [PS_CompareModeStr[iif(dwPSCMStage2 = 0, 4, 5)]]);
      DbgPshPrintf('Stage 3: %s', [PS_CompareModeStr[iif(dwPSCMStage3 = 0, 6, 7)]]);
    end;

    if (pPSDef.PSInputTexture > 0) then // Texture source for some texture modes
    begin
      dwPSITStage2 := (pPSDef.PSInputTexture shr 16) and $1;
      dwPSITStage3 := (pPSDef.PSInputTexture shr 20) and $3;

      DbgPshPrintf(#13#10'PSInputTexture ->');
      DbgPshPrintf('Stage 2: %s', [PS_TextureModesStr[dwPSITStage2]]);
      DbgPshPrintf('Stage 3: %s', [PS_TextureModesStr[dwPSITStage3]]);
    end;

    dwPSCCNumCombiners := (pPSDef.PSCombinerCount shr 0) and $F;
    if (pPSDef.PSCombinerCount > 0) then // Active combiner count (Stages 0-7)
    begin
      dwPSCCMux := (pPSDef.PSCombinerCount shr 8) and $1;
      dwPSCCC0 := (pPSDef.PSCombinerCount shr 12) and $1;
      dwPSCCC1 := (pPSDef.PSCombinerCount shr 16) and $1;

      DbgPshPrintf(#13#10'PSCombinerCount ->');
      DbgPshPrintf('Combiners: %d', [dwPSCCNumCombiners]);
      DbgPshPrintf('Mux:       %s', [PS_CombinerCountFlagsStr[dwPSCCMux]]);
      DbgPshPrintf('C0:        %s', [PS_CombinerCountFlagsStr[iif(dwPSCCC0 = 0, 2, 3)]]);
      DbgPshPrintf('C1:        %s', [PS_CombinerCountFlagsStr[iif(dwPSCCC1 = 0, 4, 5)]]);
    end;

    // Dxbx additions from here onwards :

    if dwPSCCNumCombiners > 0 then
    for i := 0 to dwPSCCNumCombiners-1 do // Loop over all combiner stages
    begin
      DbgPshPrintf(#13#10);
      DbgPshPrintf('pPSDef.PSRGBOutputs[%d] AB: %s', [i, PS_RegisterStr[((pPSDef.PSRGBOutputs[i] shr 4) and $F) + 1]]);
      DbgPshPrintf('pPSDef.PSRGBOutputs[%d] CD: %s', [i, PS_RegisterStr[((pPSDef.PSRGBOutputs[i] shr 0) and $F) + 1]]);
      DbgPshPrintf('pPSDef.PSRGBOutputs[%d] SUM: %s', [i, PS_RegisterStr[((pPSDef.PSRGBOutputs[i] shr 8) and $F) + 1]]);
      DbgPshPrintf('pPSDef.PSRGBOutputs[%d] flags: %s', [i, PSCombinerOutputFlagsToStr(pPSDef.PSRGBOutputs[i] shr 12)]);

      DbgPshPrintf(#13#10);
      DbgPshPrintf('pPSDef.PSRGBInputs[%d] A: %s', [i, PSCombinerInputToStr((pPSDef.PSRGBInputs[i] shr 24) and $FF)]);
      DbgPshPrintf('pPSDef.PSRGBInputs[%d] B: %s', [i, PSCombinerInputToStr((pPSDef.PSRGBInputs[i] shr 16) and $FF)]);
      DbgPshPrintf('pPSDef.PSRGBInputs[%d] C: %s', [i, PSCombinerInputToStr((pPSDef.PSRGBInputs[i] shr  8) and $FF)]);
      DbgPshPrintf('pPSDef.PSRGBInputs[%d] D: %s', [i, PSCombinerInputToStr((pPSDef.PSRGBInputs[i] shr  0) and $FF)]);

      DbgPshPrintf(#13#10);
      DbgPshPrintf('pPSDef.PSAlphaOutputs[%d] AB: %s', [i, PS_RegisterStr[((pPSDef.PSAlphaOutputs[i] shr 4) and $F) + 1]]);
      DbgPshPrintf('pPSDef.PSAlphaOutputs[%d] CD: %s', [i, PS_RegisterStr[((pPSDef.PSAlphaOutputs[i] shr 0) and $F) + 1]]);
      DbgPshPrintf('pPSDef.PSAlphaOutputs[%d] SUM: %s', [i, PS_RegisterStr[((pPSDef.PSAlphaOutputs[i] shr 8) and $F) + 1]]);
      DbgPshPrintf('pPSDef.PSAlphaOutputs[%d] flags: %s', [i, PSCombinerOutputFlagsToStr(pPSDef.PSAlphaOutputs[i] shr 12, {IsAlpha=}True)]);

      DbgPshPrintf(#13#10);
      DbgPshPrintf('pPSDef.PSAlphaInputs[%d] A: %s', [i, PSCombinerInputToStr((pPSDef.PSAlphaInputs[i] shr 24) and $FF, {IsAlpha=}True)]);
      DbgPshPrintf('pPSDef.PSAlphaInputs[%d] B: %s', [i, PSCombinerInputToStr((pPSDef.PSAlphaInputs[i] shr 16) and $FF, {IsAlpha=}True)]);
      DbgPshPrintf('pPSDef.PSAlphaInputs[%d] C: %s', [i, PSCombinerInputToStr((pPSDef.PSAlphaInputs[i] shr  8) and $FF, {IsAlpha=}True)]);
      DbgPshPrintf('pPSDef.PSAlphaInputs[%d] D: %s', [i, PSCombinerInputToStr((pPSDef.PSAlphaInputs[i] shr  0) and $FF, {IsAlpha=}True)]);

      DbgPshPrintf(#13#10);
      DbgPshPrintf('pPSDef.PSConstant0[%d] : %x', [i, pPSDef.PSConstant0[i]]); // C0 for each stage
      DbgPshPrintf('pPSDef.PSConstant1[%d] : %x', [i, pPSDef.PSConstant1[i]]); // C1 for each stage
    end;

    if (pPSDef.PSFinalCombinerInputsABCD > 0)
    or (pPSDef.PSFinalCombinerInputsEFG  > 0) then // Final combiner inputs
    begin
      dwPSFCIA := (pPSDef.PSFinalCombinerInputsABCD shr 24) and $FF;
      dwPSFCIB := (pPSDef.PSFinalCombinerInputsABCD shr 16) and $FF;
      dwPSFCIC := (pPSDef.PSFinalCombinerInputsABCD shr  8) and $FF;
      dwPSFCID := (pPSDef.PSFinalCombinerInputsABCD shr  0) and $FF;
      dwPSFCIE := (pPSDef.PSFinalCombinerInputsEFG  shr 24) and $FF;
      dwPSFCIF := (pPSDef.PSFinalCombinerInputsEFG  shr 16) and $FF;
      dwPSFCIG := (pPSDef.PSFinalCombinerInputsEFG  shr  8) and $FF;
      dwPS_FINALCOMBINERSETTING := (pPSDef.PSFinalCombinerInputsEFG shr 0) and $FF;

      DbgPshPrintf(#13#10'PSFinalCombinerInputsABCD ->');
      DbgPshPrintf('Input A: %s', [PSCombinerInputToStr(dwPSFCIA)]);
      DbgPshPrintf('Input B: %s', [PSCombinerInputToStr(dwPSFCIB)]);
      DbgPshPrintf('Input C: %s', [PSCombinerInputToStr(dwPSFCIC)]);
      DbgPshPrintf('Input D: %s', [PSCombinerInputToStr(dwPSFCID)]);

      DbgPshPrintf(#13#10'PSFinalCombinerInputsEFG ->');
      DbgPshPrintf('Input E: %s', [PSCombinerInputToStr(dwPSFCIE)]);
      DbgPshPrintf('Input F: %s', [PSCombinerInputToStr(dwPSFCIF)]);
      DbgPshPrintf('Input G: %s', [PSCombinerInputToStr(dwPSFCIG)]);
      DbgPshPrintf('Final combiner setting: %s', [PSFinalCombinerSettingToStr(dwPS_FINALCOMBINERSETTING)]);
    end;

(* TODO :
    PSFinalCombinerConstant0: DWORD;         // C0 in final combiner
    PSFinalCombinerConstant1: DWORD;         // C1 in final combiner
    // These last three DWORDs are used to define how Direct3D8 pixel shader constants map to the constant
    // registers in each combiner stage. They are used by the Direct3D run-time software but not by the hardware.
    PSC0Mapping: DWORD;                      // Mapping of c0 regs to D3D constants
    PSC1Mapping: DWORD;                      // Mapping of c1 regs to D3D constants
*)
    DbgPshPrintf(#13#10'PSFinalCombinerConstants ->'); // // Final combiner constant mapping
    DbgPshPrintf('Offset of D3D constant for C0: %d', [(pPSDef.PSFinalCombinerConstants shr 0) and $F]);
    DbgPshPrintf('Offset of D3D constant for C1: %d', [(pPSDef.PSFinalCombinerConstants shr 4) and $F]);
    dwPS_GLOBALFLAGS := (pPSDef.PSFinalCombinerConstants shr 8) and $1;
    DbgPshPrintf('Adjust texture flag: %s', [PS_GlobalFlagsStr[PS_GLOBALFLAGS(dwPS_GLOBALFLAGS)]]);

    DbgPshPrintf(#13#10);
  end;
end;

var // TODO : Make these thread-safe :
  InstructionOutputCombiner: AnsiString;
  SourceRegisterModifier: AnsiString ;

function PSRegToStr(const aPSReg: PS_REGISTER): AnsiString;
begin
  case PS_REGISTER(Ord(aPSReg) and $0f) of
    PS_REGISTER_C0: Result := 'c0';
    PS_REGISTER_C1: Result := 'c1';
    PS_REGISTER_FOG: Result := 'fog'; // Not supported, but makes pixel shader assembly more readable
    PS_REGISTER_V0: Result := 'v0';
    PS_REGISTER_V1: Result := 'v1';
    PS_REGISTER_T0: Result := 't0';
    PS_REGISTER_T1: Result := 't1';
    PS_REGISTER_T2: Result := 't2';
    PS_REGISTER_T3: Result := 't3';
    PS_REGISTER_R0: Result := 'r0';
    PS_REGISTER_R1: Result := 'r1';
//  PS_REGISTER_V1R0_SUM and PS_REGISTER_EF_PROD are not handled here (only valid in final combiner)
  else
    Result := '';
    Exit;
  end;

  case PS_INPUTMAPPING(Ord(aPSReg) and $e0) of
//    PS_INPUTMAPPING_UNSIGNED_IDENTITY:
//      Result := Result + '_sat';
    PS_INPUTMAPPING_UNSIGNED_INVERT: // 1-x
      Result := '1-' + Result;
    PS_INPUTMAPPING_EXPAND_NORMAL: // (2*x) - 1
      Result := Result + '_bx2';
    PS_INPUTMAPPING_EXPAND_NEGATE: // 1 - (2*x)
      Result := '1-' + Result + '_x2';
//    PS_INPUTMAPPING_HALFBIAS_NORMAL=   $80, // max(0,x) - 1/2   invalid for final combiner
//    PS_INPUTMAPPING_HALFBIAS_NEGATE=   $a0, // 1/2 - max(0,x)   invalid for final combiner
    PS_INPUTMAPPING_SIGNED_IDENTITY: ; // x
    PS_INPUTMAPPING_SIGNED_NEGATE: // -x
      Result := '-' + Result;
  end;
end;

function CombineStageInputDot(const InputA, InputB: PS_REGISTER; const OutputReg: PS_REGISTER; const OutputWriteMask: AnsiString): AnsiString;
begin
  Result := Result + 'dp3' + InstructionOutputCombiner + ' ' +
    PSRegToStr(PS_REGISTER(Ord(OutputReg) and $f)) + OutputWriteMask + ', ' +
    PSRegToStr(InputA) + ', ' +
    PSRegToStr(InputB);
end;

function CombineStageInputMul(const InputA, InputB: PS_REGISTER; const OutputReg: PS_REGISTER; const OutputWriteMask: AnsiString): AnsiString;
var
  OutputStr: AnsiString;
  InputAReadMask: AnsiString;
  InputBReadMask: AnsiString;
begin
  InputAReadMask := '';
  InputBReadMask := '';
  if OutputWriteMask = '.a' then
  begin
    if (Ord(InputA) and PS_ChannelMask) = DWORD(PS_CHANNEL_BLUE) then
      InputAReadMask := '.b'
    else
      InputAReadMask := '.a';

    if (Ord(InputB) and PS_ChannelMask) = DWORD(PS_CHANNEL_BLUE) then
      InputBReadMask := '.b'
    else
      InputBReadMask := '.a';
  end;

  Result := '';
  OutputStr := InstructionOutputCombiner + ' ' + PSRegToStr(OutputReg) + OutputWriteMask + ', ';

  // Check for cases where InputA doesn't matter :
  case PS_REGISTER(Ord(InputA) and PS_NoChannelMask) of
    PS_REGISTER_ONE: // = input * 1.0 = input
    begin
      if PS_REGISTER(Ord(InputB) and PS_NoChannelMask) = PS_REGISTER_ONE then // When InputB = PS_REGISTER_ONE, skip this
        Exit;

      Result := Result + 'mov' + OutputStr + PSRegToStr(InputB) + InputBReadMask;
    end;
    PS_REGISTER_NEGATIVE_ONE: // = input * -1.0 = - input
      Result := Result + 'mov' + OutputStr + '-' + PSRegToStr(InputB) + InputBReadMask;
//    PS_REGISTER_ONE_HALF: // = input * 0.5
//    PS_REGISTER_NEGATIVE_ONE_HALF: // = input * -0.5
  end;

  if Result <> '' then
    Exit;

  // Check for cases where InputB doesn't matter :
  case PS_REGISTER(Ord(InputB) and PS_NoChannelMask) of
    PS_REGISTER_ONE: // = input * 1.0 = input
      Result := Result + 'mov' + OutputStr + PSRegToStr(InputA) + InputAReadMask;
    PS_REGISTER_NEGATIVE_ONE: // = input * -1.0 = - input
      Result := Result + 'mov' + OutputStr + '-' + PSRegToStr(InputA) + InputAReadMask;
//    Ord(PS_REGISTER_ONE_HALF): // = input * 0.5
//    Ord(PS_REGISTER_NEGATIVE_ONE_HALF): // = -0.5
  end;

  if Result <> '' then
    Exit;

  Result := Result + 'mul' + OutputStr + PSRegToStr(InputA) + InputAReadMask + ', ' + PSRegToStr(InputB) + InputBReadMask;
end;

function CombineStage(const dwInput, dwOutput: DWORD; const OutputWriteMask: AnsiString): AnsiString;
var
  OutputAB, OutputCD, OutputSUM: PS_REGISTER;
  OutputFlags: DWORD; // Actually PS_COMBINEROUTPUT, but easier to handle as DWORD
  InputA, InputB, InputC, InputD: PS_REGISTER;
  InputAReg, InputBReg, InputCReg, InputDReg: PS_REGISTER;
begin
  Result := '';

  InputA := PS_REGISTER((dwInput shr 24) and $FF);
  InputB := PS_REGISTER((dwInput shr 16) and $FF);
  InputC := PS_REGISTER((dwInput shr  8) and $FF);
  InputD := PS_REGISTER((dwInput shr  0) and $FF);

  InputAReg := PS_REGISTER(Ord(InputA) and PS_NoChannelMask);
  InputBReg := PS_REGISTER(Ord(InputB) and PS_NoChannelMask);
  InputCReg := PS_REGISTER(Ord(InputC) and PS_NoChannelMask);
  InputDReg := PS_REGISTER(Ord(InputD) and PS_NoChannelMask);

  OutputAB := PS_REGISTER((dwOutput shr 4) and $F);
  OutputCD := PS_REGISTER((dwOutput shr 0) and $F);
  OutputSUM := PS_REGISTER((dwOutput shr 8) and $F);
  OutputFlags := (dwOutput shr 12);

  // Convert the CombinerOutput flag to a InstructionOutputCombiner
  // or an SourceRegisterModifier (so far as that's possible):
  InstructionOutputCombiner := '';
  SourceRegisterModifier := '';
  case (OutputFlags and $38) of
    Ord(PS_COMBINEROUTPUT_IDENTITY):         InstructionOutputCombiner := '';      // y = x
    Ord(PS_COMBINEROUTPUT_BIAS):             SourceRegisterModifier    := '_bias'; // y = x - 0.5
    Ord(PS_COMBINEROUTPUT_SHIFTLEFT_1):      InstructionOutputCombiner := '_x2';   // y = x*2
    Ord(PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS): SourceRegisterModifier    := '_bias_x2'; // y = (x - 0.5)*2
    Ord(PS_COMBINEROUTPUT_SHIFTLEFT_2):      InstructionOutputCombiner := '_x4';   // y = x*4
    Ord(PS_COMBINEROUTPUT_SHIFTRIGHT_1):     InstructionOutputCombiner := '_d2';   // y = x/2
  end;   //

  // Do we need to calculate AB ?
  if OutputAB > PS_REGISTER_DISCARD then
  begin
    // Handle combining of A and B (doing either a dot-product, or a multiplication) :
    if OutputFlags and Ord(PS_COMBINEROUTPUT_AB_DOT_PRODUCT) > 0 then
      Result := Result + CombineStageInputDot(InputA, InputB, OutputAB, OutputWriteMask)
    else
      Result := Result + CombineStageInputMul(InputA, InputB, OutputAB, OutputWriteMask);

    if Result <> '' then
      Result := Result + #13#10;

    // The blue-to-alpha flag is only valid for RGB, so the '+' extend syntax if free to use :
    if (OutputFlags and Ord(PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA)) > 0 then
      Result := Result + '+ mov ' + PSRegToStr(OutputAB) + '.a, ' + PSRegToStr(OutputAB) + '.b'#13#10;
  end;

  // Do we need to calculate CD ?
  if OutputCD > PS_REGISTER_DISCARD then
  begin
    // Handle combining of C and D (doing either a dot-product, or a multiplication) :
    if OutputFlags and Ord(PS_COMBINEROUTPUT_CD_DOT_PRODUCT) > 0 then
      Result := Result + CombineStageInputDot(InputC, InputD, OutputCD, OutputWriteMask)
    else
      Result := Result + CombineStageInputMul(InputC, InputD, OutputCD, OutputWriteMask);

    if Result <> '' then
      Result := Result + #13#10;

    // The blue-to-alpha flag is only valid for RGB, so the '+' extend syntax if free to use :
    if (OutputFlags and Ord(PS_COMBINEROUTPUT_CD_BLUE_TO_ALPHA)) > 0 then
      Result := Result + '+ mov ' + PSRegToStr(OutputCD) + '.a, ' + PSRegToStr(OutputAB) + '.b'#13#10;
  end;

  // Do we need to calculate SUM ?
  if OutputSUM > PS_REGISTER_DISCARD then
  begin
    if OutputFlags and Ord(PS_COMBINEROUTPUT_AB_CD_MUX) > 0 then
    begin
      // Handle PS_COMBINEROUTPUT_AB_CD_MUX, output is MUX(AB,CD) based on R0.a :

      if (OutputAB = PS_REGISTER_DISCARD) then
      begin
        if (OutputCD = PS_REGISTER_DISCARD) then
        begin
          if  (InputBReg = PS_REGISTER_ONE)
          and (InputDReg = PS_REGISTER_ONE) then
            Result := Result + 'cnd' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', r0.a, ' +
              PSRegToStr(InputA) + ', ' +
              PSRegToStr(InputC) + #13#10
          else
          if  (InputAReg = PS_REGISTER_ONE)
          and (InputDReg = PS_REGISTER_ONE) then
            Result := Result + 'cnd' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', r0.a, ' +
              PSRegToStr(InputB) + ', ' +
              PSRegToStr(InputC) + #13#10
          else
          if  (InputBReg = PS_REGISTER_ONE)
          and (InputCReg = PS_REGISTER_ONE) then
            Result := Result + 'cnd' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', r0.a, ' +
              PSRegToStr(InputA) + ', ' +
              PSRegToStr(InputD) + #13#10
          else
          if  (InputAReg = PS_REGISTER_ONE)
          and (InputCReg = PS_REGISTER_ONE) then
            Result := Result + 'cnd' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', r0.a, ' +
              PSRegToStr(InputB) + ', ' +
              PSRegToStr(InputD) + #13#10
          else
            // TODO :
            Result := Result + '; Can''t mux when both AB and CD discarded (no PS_REGISTER_ONE''s present)!'#13#10;
        end
        else
          // TODO :
          Result := Result + '; Can''t mux when AB is discarded!'#13#10;
      end
      else
      begin
        if (OutputCD = PS_REGISTER_DISCARD) then
        begin
          // TODO :
          Result := Result + '; Can''t mux when CD is discarded!'#13#10;
        end
        else
        begin
          Result := Result + 'cnd' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', r0.a, ' +
            PSRegToStr(OutputAB) + ', ' +
            PSRegToStr(OutputCD) + #13#10;
        end;
      end;
    end
    else
    begin
      // Handle PS_COMBINEROUTPUT_AB_CD_SUM, output is AB+CD :

      // TODO : Handle PS_INPUTMAPPING here too !
      if (OutputAB = PS_REGISTER_DISCARD) then
      begin
        if (OutputCD = PS_REGISTER_DISCARD) then
        begin
          // AB and CD are discarded, but we still have to calculate "sum = (A * B) + (C * D)"
          // First, check if there are effectively 2 input (when both are multiplied by one) :
          if  (InputBReg = PS_REGISTER_ONE)
          and (InputDReg = PS_REGISTER_ONE) then
            Result := Result + 'add' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
              PSRegToStr(InputA) + ', ' +
              PSRegToStr(InputC) + #13#10
          else
          if  (InputAReg = PS_REGISTER_ONE)
          and (InputDReg = PS_REGISTER_ONE) then
            Result := Result + 'add' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
              PSRegToStr(InputB) + ', ' +
              PSRegToStr(InputC) + #13#10
          else
          if  (InputBReg = PS_REGISTER_ONE)
          and (InputCReg = PS_REGISTER_ONE) then
            Result := Result + 'add' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
              PSRegToStr(InputA) + ', ' +
              PSRegToStr(InputD) + #13#10
          else
          if  (InputAReg = PS_REGISTER_ONE)
          and (InputCReg = PS_REGISTER_ONE) then
            Result := Result + 'add' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
              PSRegToStr(InputB) + ', ' +
              PSRegToStr(InputD) + #13#10
          else
          // The problem is, there's no instruction for that. Luckily, we do have 'mad' to our disposal,
          // which can do "output = (input1 * input2) + input3", but if we want to use that, we must check
          // if one of the inputs is 1, so that it can be ignored (as "A * 1" equals "A") :
          if (InputAReg = PS_REGISTER_ONE) then
            Result := Result + 'mad' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
              PSRegToStr(InputC) + ', ' +
              PSRegToStr(InputD) + ', ' +
              PSRegToStr(InputB) + #13#10
          else
          if (InputBReg = PS_REGISTER_ONE) then
            Result := Result + 'mad' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
              PSRegToStr(InputC) + ', ' +
              PSRegToStr(InputD) + ', ' +
              PSRegToStr(InputA) + #13#10
          else
          if (InputCReg = PS_REGISTER_ONE) then
            Result := Result + 'mad' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
              PSRegToStr(InputA) + ', ' +
              PSRegToStr(InputB) + ', ' +
              PSRegToStr(InputD) + #13#10
          else
          if (InputDReg = PS_REGISTER_ONE) then
            Result := Result + 'mad' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
              PSRegToStr(InputA) + ', ' +
              PSRegToStr(InputB) + ', ' +
              PSRegToStr(InputC) + #13#10
          else
          begin
            if  (InputA = InputD)
            and True{InputA is inverse of InputD PS_INPUTMAPPING_UNSIGNED_INVERT } then
            begin
              Result := Result + 'lrp' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
                PSRegToStr(InputA) + ', ' +
                PSRegToStr(InputB) + ', ' +
                PSRegToStr(InputC) + #13#10
            end
            else
              // TODO : Unsupported for now, maybe try to use a temp register?
              Result := Result + '; Can''t sum when both AB and CD discarded (no PS_REGISTER_ONE present)!'#13#10;
          end;
        end
        else
        begin
          // Only AB is discarded, but we still have to calculate "sum = (A * B) + (C * D)"
          Result := Result + 'mad' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
            PSRegToStr(InputA) + ', ' +
            PSRegToStr(InputB) + ', ' +
            PSRegToStr(OutputCD) + #13#10
        end;
      end
      else
      begin
        if (OutputCD = PS_REGISTER_DISCARD) then
        begin
          // Only CD is discarded, but we still have to calculate "sum = (A * B) + (C * D)"
          Result := Result + 'mad' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
            PSRegToStr(InputC) + ', ' +
            PSRegToStr(InputD) + ', ' +
            PSRegToStr(OutputAB) + #13#10;
        end
        else
          Result := Result + 'add' + InstructionOutputCombiner + ' ' + PSRegToStr(OutputSUM) + OutputWriteMask + ', ' +
            PSRegToStr(OutputAB) + ', ' + PSRegToStr(OutputCD) + #13#10;
      end;
    end;
  end;
end;

function ProcessTextureMode(const aStage: int; const aDotMapping: PS_DOTMAPPING;
  const aTextureMode: PS_TEXTUREMODES; const aInputStage: int): AnsiString;

  function TStr(const t: int): AnsiString;
  begin
    Result := 't' + AnsiString(IntToStr(t));
  end;

begin
  Result := '';
  // TODO : Handle the parameters to all other texture-addressing methods (than 'tex') here too

  // Convert the texture mode to a texture addressing instruction :
  case aTextureMode of
    PS_TEXTUREMODES_PROJECT2D: Result := 'tex';
    PS_TEXTUREMODES_PROJECT3D: Result := 'tex'; // Note : 3d textures are sampled using PS_TEXTUREMODES_CUBEMAP
    PS_TEXTUREMODES_CUBEMAP: Result := 'tex'; // Note : If we use 'texreg2rgb', that requires ps.1.2 (we're still using ps.1.1)
    PS_TEXTUREMODES_PASSTHRU: Result := 'textcoord';
    PS_TEXTUREMODES_CLIPPLANE: Result := 'texkill';
    PS_TEXTUREMODES_BUMPENVMAP: Result := 'texbem';
    PS_TEXTUREMODES_BUMPENVMAP_LUM: Result := 'texbemi';
//    PS_TEXTUREMODES_BRDF: Result := 'texbrdf'; // Note : Not supported by Direct3D8 ?
    PS_TEXTUREMODES_DOT_ST: Result := 'texm3x2tex';
    PS_TEXTUREMODES_DOT_ZW: Result := 'texm3x2depth';
//    PS_TEXTUREMODES_DOT_RFLCT_DIFF: Result := 'texm3x3diff'; // Note : Not supported by Direct3D8 ?
    PS_TEXTUREMODES_DOT_RFLCT_SPEC: Result := 'texm3x3vspec';
    PS_TEXTUREMODES_DOT_STR_3D: Result := 'texm3x3tex'; // Note : Uses a 3d texture
    PS_TEXTUREMODES_DOT_STR_CUBE: Result := 'texm3x3tex'; // Note : Uses a cube texture
    PS_TEXTUREMODES_DPNDNT_AR: Result := 'texreg2ar';
    PS_TEXTUREMODES_DPNDNT_GB: Result := 'texreg2gb';
    PS_TEXTUREMODES_DOTPRODUCT: Result := 'texm3x3pad';
    PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST: Result := 'texm3x3spec'; // Note : Needs 3 arguments!
  end;

  if Result = '' then
    Exit;

  Result := Result + ' ' + TStr(aStage);

  // For those texture modes that need it, add the source stage as argument :
  if aTextureMode in [
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
    Result := Result + ', ' + TStr(aInputStage);

    case aDotMapping of
      PS_DOTMAPPING_MINUS1_TO_1_D3D:
        Result := Result + '_bx2';
    end;
  end;

  // Add the third argument :
  if aTextureMode in [
    PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST] then
    Result := Result + ', c0 ; Dxbx guess'; // TODO : Where do we get the 3rd argument to

  Result := Result + #13#10;
end;

function ProcessFinalCombiner(pPSDef: PX_D3DPIXELSHADERDEF): AnsiString;
var
  A, B, C, D, E, F, G: PS_REGISTER;
  flags: PS_FINALCOMBINERSETTING;
  EFReg: PS_REGISTER;
  TmpReg: PS_REGISTER;
begin
  Result := '';

  A := PS_REGISTER((pPSDef.PSFinalCombinerInputsABCD shr 24) and $FF);
  B := PS_REGISTER((pPSDef.PSFinalCombinerInputsABCD shr 16) and $FF);
  C := PS_REGISTER((pPSDef.PSFinalCombinerInputsABCD shr  8) and $FF);
  D := PS_REGISTER((pPSDef.PSFinalCombinerInputsABCD shr  0) and $FF);
  E := PS_REGISTER((pPSDef.PSFinalCombinerInputsEFG shr 24) and $FF);
  F := PS_REGISTER((pPSDef.PSFinalCombinerInputsEFG shr 16) and $FF);
  G := PS_REGISTER((pPSDef.PSFinalCombinerInputsEFG shr  8) and $FF);
  flags := PS_FINALCOMBINERSETTING((pPSDef.PSFinalCombinerInputsEFG shr 0) and $FF);

//  The final combiner performs the following operations :
//
//    prod register = E*F                // PS_REGISTER_EF_PROD, useable in A,B,C,D,G
//
//    rgbout        = A*B + (1-A)*C + D  // lrp tmp.rgb, A, B, C       // Note : tmp can be r0 if [A,B,C,D] * r0 = []
//                                       // add r0.rgb, tmp.rgb, D.rgb // Otherwise use a writable register from A,B or C
//
//    alphaout      = G.a                // mov r0.a, G.a              // Not necessary if G = r0
//
//    (also the final combiner can read PS_REGISTER_V1R0_SUM, which is equal to v1 + r0)
//  Normal optimizations apply, like when A = PS_REGISTER_ZERO, all we have left is C + D (add r0.rgb, C.rgb, D.rgb)
//  Also, if D = PS_REGISTER_ZERO, the add can be changed into a mov (if the result isn't already in r0.rgb)

  // Handle PS_REGISTER_EF_PROD :
  if (E > PS_REGISTER_ZERO) or (F > PS_REGISTER_ZERO) then
  begin
    if (PS_REGISTER(Ord(E) and $f) = PS_REGISTER_R0)
    or (PS_REGISTER(Ord(F) and $f) = PS_REGISTER_R0) then
      EFReg := PS_REGISTER_R0
    else
      EFReg := PS_REGISTER_R1; // TODO : Find another temp than R0 for E*F

    Result := Result + 'mul ' + PSRegToStr(EFReg) + ', '+ PSRegToStr(E) + ', ' + PSRegToStr(F) + #13#10;

    // All registers that use E*F, must use the EF result register instead :
    if (PS_REGISTER(Ord(A) and $f) = PS_REGISTER_EF_PROD) then A := EFReg;
    if (PS_REGISTER(Ord(B) and $f) = PS_REGISTER_EF_PROD) then B := EFReg;
    if (PS_REGISTER(Ord(C) and $f) = PS_REGISTER_EF_PROD) then C := EFReg;
    if (PS_REGISTER(Ord(D) and $f) = PS_REGISTER_EF_PROD) then D := EFReg;
    if (PS_REGISTER(Ord(G) and $f) = PS_REGISTER_EF_PROD) then G := EFReg;
  end;

  // Handle PS_REGISTER_V1R0_SUM :
  if (PS_REGISTER(Ord(A) and $f) = PS_REGISTER_V1R0_SUM)
  or (PS_REGISTER(Ord(B) and $f) = PS_REGISTER_V1R0_SUM)
  or (PS_REGISTER(Ord(C) and $f) = PS_REGISTER_V1R0_SUM)
  or (PS_REGISTER(Ord(D) and $f) = PS_REGISTER_V1R0_SUM)
  or (PS_REGISTER(Ord(G) and $f) = PS_REGISTER_V1R0_SUM) then
  begin
    TmpReg := PS_REGISTER_R0; // TODO : Find a temporary register

    Result := Result + 'add ' + PSRegToStr(TmpReg) + ', V1, R0'#13#10;

    if (PS_REGISTER(Ord(A) and $f) = PS_REGISTER_EF_PROD) then A := TmpReg;
    if (PS_REGISTER(Ord(B) and $f) = PS_REGISTER_EF_PROD) then B := TmpReg;
    if (PS_REGISTER(Ord(C) and $f) = PS_REGISTER_EF_PROD) then C := TmpReg;
    if (PS_REGISTER(Ord(D) and $f) = PS_REGISTER_EF_PROD) then D := TmpReg;
    if (PS_REGISTER(Ord(G) and $f) = PS_REGISTER_EF_PROD) then G := TmpReg;
  end;

  // Handle the lerp
  if  ((A = PS_REGISTER_ZERO) or (A = PS_REGISTER_R0))
  and ((B = PS_REGISTER_ZERO) or (B = PS_REGISTER_R0))
  and ((C = PS_REGISTER_ZERO) or (C = PS_REGISTER_R0)) then
    // do nothing
  else
  begin
    TmpReg := PS_REGISTER_R0; // TODO : Check if r0 is usable
    Result := Result + 'lrp ' + PSRegToStr(TmpReg) + ', ' + PSRegToStr(A) + ', ' + PSRegToStr(B) + ', ' + PSRegToStr(C) + #13#10;
  end;

  if (TmpReg = PS_REGISTER_ZERO) or (TmpReg = PS_REGISTER_R0) then
  begin
    if (D = PS_REGISTER_ZERO) or (D = PS_REGISTER_R0) then
      // do nothing
    else
      Result := Result + 'mov r0.rgb, ' + PSRegToStr(D) + #13#10;
  end
  else
  begin
    if D = PS_REGISTER_ZERO then
      Result := Result + 'mov r0.rgb, ' + PSRegToStr(TmpReg) + #13#10
    else
      Result := Result + 'add r0.rgb, ' + PSRegToStr(TmpReg) + ', ' + PSRegToStr(D) + #13#10;
  end;

  // Handle alphaout :
  if (g = PS_REGISTER_ONE) or (G = PS_REGISTER_R0) then
    // Do nothing if G = 1 or r0
  else
    Result := Result + 'mov r0.a, ' + PSRegToStr(G) + #13#10;
end;

function XTL_EmuRecompilePshDef(pPSDef: PX_D3DPIXELSHADERDEF): AnsiString;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  LogFlags: TLogFlags;
  i: Integer;
  dwPSCCNumCombiners: DWORD;
  RGBInput: DWORD;
  RGBOutput: DWORD;
  AlphaInput: DWORD;
  AlphaOutput: DWORD;
  AlphaAdd: AnsiString;
begin
  // Azurik likes to create and destroy the same shader every frame! O_o
  LogFlags := lfUnit;
  if IsRunning(TITLEID_AZURIK) then
    LogFlags := LogFlags or lfExtreme;

  // Dump the contents of the PixelShader def
  if MayLog(LogFlags) then
    XTL_DumpPixelShaderDefToFile(pPSDef);

  if MayLog(LogFlags) then
    XTL_PrintPixelShaderDefContents(pPSDef);

  // First things first, set the pixel shader version
  // TODO -oCXBX: ps.1.1 might be a better idea...
  //Result := 'ps.1.0'#13#10;
  Result := 'ps.1.1'#13#10; // 1.1 allows reading from 2 textures (which we use in 'cnd')

  for i := 0 to 8-1 do
  begin
    // Define constants directly after the version instruction and before any other instruction :
    if pPSDef.PSConstant0[i] > 0 then
      Result := Result + AnsiString(Format('def c%d, %ff, %ff, %ff, %ff'#13#10, [i,
        {R}((pPSDef.PSConstant0[i] shr 16) and $FF) / 255.0,
        {G}((pPSDef.PSConstant0[i] shr  8) and $FF) / 255.0,
        {B}((pPSDef.PSConstant0[i] shr  0) and $FF) / 255.0,
        {A}((pPSDef.PSConstant0[i] shr 24) and $FF) / 255.0
        ]));
  end;

  // Handle Texture declarations :
  Result := Result + ProcessTextureMode(0, PS_DOTMAPPING(0),
    PS_TEXTUREMODES((pPSDef.PSTextureModes shr  0) and $1F), -1); // Stage 0 has no predecessors
  Result := Result + ProcessTextureMode(1, PS_DOTMAPPING((pPSDef.PSDotMapping shr 0) and 7),
    PS_TEXTUREMODES((pPSDef.PSTextureModes shr  5) and $1F), 0); // Stage 1 can only use stage 0
  Result := Result + ProcessTextureMode(2, PS_DOTMAPPING((pPSDef.PSDotMapping shr 4) and 7),
    PS_TEXTUREMODES((pPSDef.PSTextureModes shr 10) and $1F), (pPSDef.PSInputTexture shr 16) and $1);
  Result := Result + ProcessTextureMode(3, PS_DOTMAPPING((pPSDef.PSDotMapping shr 8) and 7),
    PS_TEXTUREMODES((pPSDef.PSTextureModes shr 15) and $1F), (pPSDef.PSInputTexture shr 20) and $3);

  // On the Xbox, the alpha portion of the R0 register is initialized to
  // the alpha component of texture 0 if texturing is enabled for texture 0 :
  if ((pPSDef.PSTextureModes shr  0) and $1F) > 0 then
    // TODO : Move this over to where r0.a is first read (if ever)
    Result := Result + 'mov r0.a, t0.a'#13#10;

  // Loop over all stages :
  dwPSCCNumCombiners := (pPSDef.PSCombinerCount shr 0) and $F;
  if dwPSCCNumCombiners > 0 then
  for i := 0 to dwPSCCNumCombiners-1 do // Loop over all  combiner stages
  begin
    // Convert each stage's output-from-input mappings, back to PC-compatible pixel shader instructions :

    RGBInput := pPSDef.PSRGBInputs[i];
    AlphaInput := pPSDef.PSAlphaInputs[i];

    RGBOutput := pPSDef.PSRGBOutputs[i];
    AlphaOutput := pPSDef.PSAlphaOutputs[i];

    // Check if RGB and Alpha are handled identical :
    if  (RGBOutput = AlphaOutput)
    and ((RGBInput and PS_NoChannelsMask) = (AlphaInput and PS_NoChannelsMask))
    and ((RGBInput and PS_AlphaChannelsMask) = 0)
    and ((AlphaInput and PS_AlphaChannelsMask) = PS_AlphaChannelsMask) then
      // In that case, we combine both channels in one go without Output Write Masks (which defaults to '.rgba') :
      Result := Result + CombineStage(RGBInput, RGBOutput, '')
    else
    begin
      // Else, handle rgb separately from alpha :
      Result := Result + CombineStage(RGBInput, RGBOutput, '.rgb');
      AlphaAdd := CombineStage(AlphaInput, AlphaOutput, '.a');
      if AlphaAdd <> '' then
        Result := Result + '+ ' + AlphaAdd;
    end;
  end;

  // Check if there's a final combiner
  if (pPSDef.PSFinalCombinerInputsABCD > 0)
  or (pPSDef.PSFinalCombinerInputsEFG > 0) then
    Result := Result + ProcessFinalCombiner(pPSDef);

  // Note : The end result (rgba) should be in r0 (output register) now!

  if MayLog(LogFlags) then
    DbgPrintf(string(Result));
end;

end.

