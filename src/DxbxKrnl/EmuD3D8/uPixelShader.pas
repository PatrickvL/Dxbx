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

uses
  // Delphi
  Windows,
  // Dxbx
  uTypes,
  uLog,
  uDxbxUtils, // iif
  uEmuD3D8Types,
  uEmuD3D8Utils;

{$INCLUDE Dxbx.inc}

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
    PS_INPUTMAPPING_UNSIGNED_IDENTITY= $00, // max(0,x)         OK for final combiner
    PS_INPUTMAPPING_UNSIGNED_INVERT=   $20, // 1 - max(0,x)     OK for final combiner
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
    PS_REGISTER_V1R0_SUM=          $0e, // r
    PS_REGISTER_EF_PROD=           $0f, // r

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


type PS_FINALCOMBINERSETTING =
(
    PS_FINALCOMBINERSETTING_CLAMP_SUM=     $80, // V1+R0 sum clamped to [0,1]

    PS_FINALCOMBINERSETTING_COMPLEMENT_V1= $40, // unsigned invert mapping

    PS_FINALCOMBINERSETTING_COMPLEMENT_R0= $20  // unsigned invert mapping
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
// print relevant contents to the debug console
procedure XTL_PrintPixelShaderDefContents(pPSDef: PX_D3DPIXELSHADERDEF);
// Recompile Xbox PixelShader def
function XTL_EmuRecompilePshDef(pPSDef: PX_D3DPIXELSHADERDEF; ppRecompiled: PLPD3DXBUFFER): HRESULT;

implementation

procedure DbgPshPrintf(aStr: string); overload;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
{$ifdef _DEBUG_TRACK_PS}
  if (g_bPrintfOn) then
    printf(aStr);
{$endif}
end;

procedure DbgPshPrintf(aStr: string; Args: array of const); overload;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
{$ifdef _DEBUG_TRACK_PS}
  if (g_bPrintfOn) then
    printf(aStr, Args);
{$endif}
end;

// From PixelShader.cpp -----------------------------------------------------------

// PS Texture Modes
const PS_TextureModesStr: array [0..$1f] of PAnsiChar =
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
const PS_DotMappingStr: array [0..7] of PAnsiChar =
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

(*
// PS CompareMode
const PS_CompareModeStr: array [PS_COMPAREMODE] of PAnsiChar =
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
*)

// PS CombinerCountFlags
const PS_CombinerCountFlagsStr: array [0..6-1] of PAnsiChar =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_COMBINERCOUNT_MUX_LSB',    // 0x0000L, // mux on r0.a lsb
    'PS_COMBINERCOUNT_MUX_MSB',    // 0x0001L, // mux on r0.a msb

    'PS_COMBINERCOUNT_SAME_C0',    // 0x0000L, // c0 same in each stage
    'PS_COMBINERCOUNT_UNIQUE_C0',  // 0x0010L, // c0 unique in each stage

    'PS_COMBINERCOUNT_SAME_C1',    // 0x0000L, // c1 same in each stage
    'PS_COMBINERCOUNT_UNIQUE_C1'   // 0x0100L  // c1 unique in each stage
);

(*
// PS InputMapping
const PS_InputMappingStr: array [PS_INPUTMAPPING] of PAnsiChar =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_INPUTMAPPING_UNSIGNED_IDENTITY',  // 0x00L, // max(0,x)         OK for final combiner
    'PS_INPUTMAPPING_UNSIGNED_INVERT',    // 0x20L, // 1 - max(0,x)     OK for final combiner
    'PS_INPUTMAPPING_EXPAND_NORMAL',      // 0x40L, // 2*max(0,x) - 1   invalid for final combiner
    'PS_INPUTMAPPING_EXPAND_NEGATE',      // 0x60L, // 1 - 2*max(0,x)   invalid for final combiner
    'PS_INPUTMAPPING_HALFBIAS_NORMAL',    // 0x80L, // max(0,x) - 1/2   invalid for final combiner
    'PS_INPUTMAPPING_HALFBIAS_NEGATE',    // 0xa0L, // 1/2 - max(0,x)   invalid for final combiner
    'PS_INPUTMAPPING_SIGNED_IDENTITY',    // 0xc0L, // x                invalid for final combiner
    'PS_INPUTMAPPING_SIGNED_NEGATE'       // 0xe0L, // -x               invalid for final combiner
);

// PS Register
const PS_RegisterStr: array [PS_REGISTER] of PAnsiChar =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_REGISTER_ZERO',     // 0x00L, // r
    'PS_REGISTER_DISCARD',  // 0x00L, // w
    'PS_REGISTER_C0',       // 0x01L, // r
    'PS_REGISTER_C1',       // 0x02L, // r
    'PS_REGISTER_FOG',      // 0x03L, // r
    'PS_REGISTER_V0',       // 0x04L, // r/w
    'PS_REGISTER_V1',       // 0x05L, // r/w
    'PS_REGISTER_T0',       // 0x08L, // r/w
    'PS_REGISTER_T1',       // 0x09L, // r/w
    'PS_REGISTER_T2',       // 0x0aL, // r/w
    'PS_REGISTER_T3',       // 0x0bL, // r/w
    'PS_REGISTER_R0',       // 0x0cL, // r/w
    'PS_REGISTER_R1',       // 0x0dL, // r/w
    'PS_REGISTER_V1R0_SUM', // 0x0eL, // r
    'PS_REGISTER_EF_PROD',  // 0x0fL, // r

    'PS_REGISTER_ONE',               // PS_REGISTER_ZERO | PS_INPUTMAPPING_UNSIGNED_INVERT, // OK for final combiner
    'PS_REGISTER_NEGATIVE_ONE',      // PS_REGISTER_ZERO | PS_INPUTMAPPING_EXPAND_NORMAL,   // invalid for final combiner
    'PS_REGISTER_ONE_HALF',          // PS_REGISTER_ZERO | PS_INPUTMAPPING_HALFBIAS_NEGATE, // invalid for final combiner
    'PS_REGISTER_NEGATIVE_ONE_HALF'  // PS_REGISTER_ZERO | PS_INPUTMAPPING_HALFBIAS_NORMAL, // invalid for final combiner
);

// PS Channel
const PS_ChannelStr: array [PS_CHANNEL] of PAnsiChar =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_CHANNEL_RGB',   // 0x00, // used as RGB source
    'PS_CHANNEL_BLUE',  // 0x00, // used as ALPHA source
    'PS_CHANNEL_ALPHA'  // 0x10, // used as RGB or ALPHA source
);

// PS FinalCombinerSetting
const PS_FinalCombinerSettingStr: array [PS_FINALCOMBINERSETTING] of PAnsiChar =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_FINALCOMBINERSETTING_CLAMP_SUM'  ,    // 0x80, // V1+R0 sum clamped to [0,1]

    'PS_FINALCOMBINERSETTING_COMPLEMENT_V1',  // 0x40, // unsigned invert mapping

    'PS_FINALCOMBINERSETTING_COMPLEMENT_R0'   // 0x20, // unsigned invert mapping
);

// PS CombineOutput
const PS_CombineOutputStr: array [PS_COMBINEROUTPUT] of PAnsiChar =
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
*)

// PS GlobalFlags
const PS_GlobalFlagsStr: array [PS_GLOBALFLAGS] of PAnsiChar =
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
(
    'PS_GLOBALFLAGS_NO_TEXMODE_ADJUST',     // 0x0000L, // don't adjust texture modes
    'PS_GLOBALFLAGS_TEXMODE_ADJUST'         // 0x0001L, // adjust texture modes according to set texture
);


{static}var PshNumber: int = 0;  // Keep track of how many pixel shaders we've attemted to convert.
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
    fprintf(out_, 'PSAphaInputs[8]              = 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X 0x%.08X'#13#10 +
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
    fclose(out_);
  end;
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
begin
  // Show the contents to the user
  if Assigned(pPSDef) then
  begin
    DbgPshPrintf(#13#10'-----PixelShader Def Contents-----'#13#10);

    if (pPSDef.PSTextureModes > 0) then
    begin
      dwPSTexMode0 := (pPSDef.PSTextureModes shr 0) and $1F;
      dwPSTexMode1 := (pPSDef.PSTextureModes shr 5) and $1F;
      dwPSTexMode2 := (pPSDef.PSTextureModes shr 10) and $1F;
      dwPSTexMode3 := (pPSDef.PSTextureModes shr 15) and $1F;

      DbgPshPrintf('PSTextureModes ->'#13#10);
      DbgPshPrintf('Stage 0: %s'#13#10, [PS_TextureModesStr[dwPSTexMode0]]);
      DbgPshPrintf('Stage 1: %s'#13#10, [PS_TextureModesStr[dwPSTexMode1]]);
      DbgPshPrintf('Stage 2: %s'#13#10, [PS_TextureModesStr[dwPSTexMode2]]);
      DbgPshPrintf('Stage 3: %s'#13#10, [PS_TextureModesStr[dwPSTexMode3]]);
    end;

    if (pPSDef.PSDotMapping > 0) then
    begin
      dwPSDMStage1 := (pPSDef.PSDotMapping shr 0) and $7;
      dwPSDMStage2 := (pPSDef.PSDotMapping shr 4) and $7;
      dwPSDMStage3 := (pPSDef.PSDotMapping shr 8) and $7;

      DbgPshPrintf('PSDotMapping ->'#13#10);
      DbgPshPrintf('Stage 1: %s'#13#10, [PS_DotMappingStr[dwPSDMStage1]]);
      DbgPshPrintf('Stage 2: %s'#13#10, [PS_DotMappingStr[dwPSDMStage2]]);
      DbgPshPrintf('Stage 3: %s'#13#10, [PS_DotMappingStr[dwPSDMStage3]]);
    end;

    if (pPSDef.PSCompareMode > 0) then
    begin
      dwPSCMStage0 := (pPSDef.PSCompareMode shr 0) and $F;
      dwPSCMStage1 := (pPSDef.PSCompareMode shr 4) and $F;
      dwPSCMStage2 := (pPSDef.PSCompareMode shr 8) and $F;
      dwPSCMStage3 := (pPSDef.PSCompareMode shr 12) and $F;

      DbgPshPrintf('PSCompareMode ->'#13#10);
      DbgPshPrintf('Stage 0: %s'#13#10, [PS_TextureModesStr[iif(dwPSCMStage0 = 0, 0, 1)]]);
      DbgPshPrintf('Stage 1: %s'#13#10, [PS_TextureModesStr[iif(dwPSCMStage1 = 0, 2, 3)]]);
      DbgPshPrintf('Stage 2: %s'#13#10, [PS_TextureModesStr[iif(dwPSCMStage2 = 0, 4, 5)]]);
      DbgPshPrintf('Stage 3: %s'#13#10, [PS_TextureModesStr[iif(dwPSCMStage3 = 0, 6, 7)]]);
    end;

    if (pPSDef.PSInputTexture > 0) then
    begin
      dwPSITStage2 := (pPSDef.PSInputTexture shr 16) and $1;
      dwPSITStage3 := (pPSDef.PSInputTexture shr 20) and $3;

      DbgPshPrintf('PSInputTexture ->'#13#10);
      DbgPshPrintf('Stage 2: %s'#13#10, [PS_TextureModesStr[dwPSITStage2]]);
      DbgPshPrintf('Stage 3: %s'#13#10, [PS_TextureModesStr[dwPSITStage3]]);
    end;

    if (pPSDef.PSCombinerCount > 0) then
    begin
      dwPSCCNumCombiners := (pPSDef.PSCombinerCount shr 0) and $F;
      dwPSCCMux := (pPSDef.PSCombinerCount shr 8) and $1;
      dwPSCCC0 := (pPSDef.PSCombinerCount shr 12) and $1;
      dwPSCCC1 := (pPSDef.PSCombinerCount shr 16) and $1;

      DbgPshPrintf('PSCombinerCount ->'#13#10);
      DbgPshPrintf('Combiners: %d'#13#10, [dwPSCCNumCombiners]);
      DbgPshPrintf('Mux:       %s'#13#10, [PS_CombinerCountFlagsStr[dwPSCCMux]]);
      DbgPshPrintf('C0:        %s'#13#10, [PS_CombinerCountFlagsStr[iif(dwPSCCC0 = 0, 2, 3)]]);
      DbgPshPrintf('C1:        %s'#13#10, [PS_CombinerCountFlagsStr[iif(dwPSCCC1 = 0, 4, 5)]]);
    end;

    (*for(int i := 0; i > 7; i++)
    begin
      if Assigned(pPSDef.PSRGBInputs[i]) then
      begin*)
  end;
end;

function XTL_EmuRecompilePshDef(pPSDef: PX_D3DPIXELSHADERDEF; ppRecompiled: PLPD3DXBUFFER): HRESULT;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  szPshString: array [0..2048] of AnsiChar;    // I'm sure that's big enough...
begin

  // Dump the contents of the PixelShader def
{$ifdef _DEBUG_TRACK_PS}
  XTL_DumpPixelShaderDefToFile(pPSDef);
  XTL_PrintPixelShaderDefContents(pPSDef);
{$endif}

  // First things first, set the pixel shader version
  // TODO -oCXBX: ps.1.1 might be a better idea...
  sprintf(@szPshString[0], '%s', ['ps.1.0'#13#10]);

  // Handle Texture declarations
  if (pPSDef.PSTextureModes <> 0) then
  begin
  end;

  Result := S_OK;
end;

end.

