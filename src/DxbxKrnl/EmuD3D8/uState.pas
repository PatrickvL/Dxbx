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

unit uState;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // DirectX
{$IFDEF DXBX_USE_D3D9}
  Direct3D9,
{$ELSE}
  Direct3D8, // IDirect3DBaseTexture8
{$ENDIF}
  // Dxbx
  uTypes,
  uLog,
  uConvert,
  uDxbxUtils, // iif
  uDxbxKrnlUtils,
  uEmuD3D8Types,
  uEmuD3D8Utils;

function DxbxRenderStateIntroducedAtVersion(const aRenderState: X_D3DRENDERSTATETYPE): uint32; {NOPATCH}
function DxbxRenderStateIsXboxExtension(const Value: X_D3DRENDERSTATETYPE): Boolean; {NOPATCH}
function DxbxXboxMethodToRenderState(const aMethod: DWORD): X_D3DRenderStateType; {NOPATCH}

function EmuXB2PC_D3DRS(Value: X_D3DRENDERSTATETYPE): D3DRENDERSTATETYPE;

procedure DxbxBuildRenderStateMappingTable(const aD3DRenderState: PDWORDs); {NOPATCH}
procedure DxbxInitializeDefaultRenderStates(const aParameters: PX_D3DPRESENT_PARAMETERS); {NOPATCH}

function DxbxVersionAdjust_D3DRS(const Value: X_D3DRENDERSTATETYPE): X_D3DRENDERSTATETYPE; {NOPATCH}
function Dxbx_SetRenderState(const XboxRenderState: X_D3DRenderStateType; const XboxValue: DWORD): DWORD; {NOPATCH}
function DxbxTransferRenderState(const XboxRenderState: X_D3DRENDERSTATETYPE): HResult;

function DxbxTextureStageStateIsXboxExtension(const Value: X_D3DTEXTURESTAGESTATETYPE): Boolean; {NOPATCH}
function DxbxFromOldVersion_D3DTSS(const OldValue: X_D3DTEXTURESTAGESTATETYPE): X_D3DTEXTURESTAGESTATETYPE; {NOPATCH}
function DxbxFromNewVersion_D3DTSS(const NewValue: X_D3DTEXTURESTAGESTATETYPE): X_D3DTEXTURESTAGESTATETYPE; {NOPATCH}

procedure XTL_EmuUpdateDeferredStates(); {NOPATCH}

// XDK version independent renderstate table, containing pointers to the original locations.
var XTL_EmuMappedD3DRenderState: array [X_D3DRS_FIRST..X_D3DRS_LAST+1] of PDWORD; // +1 for the unsupported value

// Dxbx addition : Dummy value (and pointer to that) to transparently ignore unsupported render states :
const X_D3DRS_UNSUPPORTED = X_D3DRS_LAST+1;
var DummyRenderState: PDWORD = @(XTL_EmuMappedD3DRenderState[X_D3DRS_UNSUPPORTED]); // Unsupported states share this pointer value

// Dxbx indicator of an unsupported render state (returned when processing an Xbox extension) :
const D3DRS_UNSUPPORTED = D3DRS_FORCE_DWORD;

// Texture state lookup table (same size in all XDK versions, so defined as a fixed size array) :
type TD3DDeferredTextureState = array [0..X_D3DTS_STAGECOUNT-1, 0..X_D3DTS_STAGESIZE-1] of DWORD;
     PD3DDeferredTextureState = ^TD3DDeferredTextureState;
var XTL_EmuD3DDeferredTextureState: PD3DDeferredTextureState;

const DEFAULT_XDK_VERSION = 4627; // TODO -oDxbx : Make this configurable
var g_BuildVersion: uint32 = DEFAULT_XDK_VERSION;
// var g_OrigBuildVersion: uint32; // Dxbx note : Unused

type
  TXB2PCFunc = function(Value: DWORD): DWORD;

var
  DxbxRenderStateXB2PCCallback: array [X_D3DRS_FIRST..X_D3DRS_LAST] of TXB2PCFunc;
  DxbxMapActiveVersionToMostRecent: array [X_D3DRS_FIRST..X_D3DRS_LAST] of X_D3DRENDERSTATETYPE;
  DxbxMapMostRecentToActiveVersion: array [X_D3DRS_FIRST..X_D3DRS_LAST] of X_D3DRENDERSTATETYPE;
  DxbxRenderStateXB2String: array [X_D3DRS_FIRST..X_D3DRS_LAST] of AnsiString = (
    'D3DRS_PSALPHAINPUTS0',
    'D3DRS_PSALPHAINPUTS1',
    'D3DRS_PSALPHAINPUTS2',
    'D3DRS_PSALPHAINPUTS3',
    'D3DRS_PSALPHAINPUTS4',
    'D3DRS_PSALPHAINPUTS5',
    'D3DRS_PSALPHAINPUTS6',
    'D3DRS_PSALPHAINPUTS7',
    'D3DRS_PSFINALCOMBINERINPUTSABCD',
    'D3DRS_PSFINALCOMBINERINPUTSEFG',
    'D3DRS_PSCONSTANT0_0',
    'D3DRS_PSCONSTANT0_1',
    'D3DRS_PSCONSTANT0_2',
    'D3DRS_PSCONSTANT0_3',
    'D3DRS_PSCONSTANT0_4',
    'D3DRS_PSCONSTANT0_5',
    'D3DRS_PSCONSTANT0_6',
    'D3DRS_PSCONSTANT0_7',
    'D3DRS_PSCONSTANT1_0',
    'D3DRS_PSCONSTANT1_1',
    'D3DRS_PSCONSTANT1_2',
    'D3DRS_PSCONSTANT1_3',
    'D3DRS_PSCONSTANT1_4',
    'D3DRS_PSCONSTANT1_5',
    'D3DRS_PSCONSTANT1_6',
    'D3DRS_PSCONSTANT1_7',
    'D3DRS_PSALPHAOUTPUTS0',
    'D3DRS_PSALPHAOUTPUTS1',
    'D3DRS_PSALPHAOUTPUTS2',
    'D3DRS_PSALPHAOUTPUTS3',
    'D3DRS_PSALPHAOUTPUTS4',
    'D3DRS_PSALPHAOUTPUTS5',
    'D3DRS_PSALPHAOUTPUTS6',
    'D3DRS_PSALPHAOUTPUTS7',
    'D3DRS_PSRGBINPUTS0',
    'D3DRS_PSRGBINPUTS1',
    'D3DRS_PSRGBINPUTS2',
    'D3DRS_PSRGBINPUTS3',
    'D3DRS_PSRGBINPUTS4',
    'D3DRS_PSRGBINPUTS5',
    'D3DRS_PSRGBINPUTS6',
    'D3DRS_PSRGBINPUTS7',
    'D3DRS_PSCOMPAREMODE',
    'D3DRS_PSFINALCOMBINERCONSTANT0',
    'D3DRS_PSFINALCOMBINERCONSTANT1',
    'D3DRS_PSRGBOUTPUTS0',
    'D3DRS_PSRGBOUTPUTS1',
    'D3DRS_PSRGBOUTPUTS2',
    'D3DRS_PSRGBOUTPUTS3',
    'D3DRS_PSRGBOUTPUTS4',
    'D3DRS_PSRGBOUTPUTS5',
    'D3DRS_PSRGBOUTPUTS6',
    'D3DRS_PSRGBOUTPUTS7',
    'D3DRS_PSCOMBINERCOUNT',
    'D3DRS_PS_DEPRECATED', // ??
    'D3DRS_PSDOTMAPPING',
    'D3DRS_PSINPUTTEXTURE',
    'D3DRS_ZFUNC',
    'D3DRS_ALPHAFUNC',
    'D3DRS_ALPHABLENDENABLE',
    'D3DRS_ALPHATESTENABLE',
    'D3DRS_ALPHAREF',
    'D3DRS_SRCBLEND',
    'D3DRS_DESTBLEND',
    'D3DRS_ZWRITEENABLE',
    'D3DRS_DITHERENABLE',
    'D3DRS_SHADEMODE',
    'D3DRS_COLORWRITEENABLE',
    'D3DRS_STENCILZFAIL',
    'D3DRS_STENCILPASS',
    'D3DRS_STENCILFUNC',
    'D3DRS_STENCILREF',
    'D3DRS_STENCILMASK',
    'D3DRS_STENCILWRITEMASK',
    'D3DRS_BLENDOP',
    'D3DRS_BLENDCOLOR',
    'D3DRS_SWATHWIDTH',
    'D3DRS_POLYGONOFFSETZSLOPESCALE',
    'D3DRS_POLYGONOFFSETZOFFSET',
    'D3DRS_POINTOFFSETENABLE',
    'D3DRS_WIREFRAMEOFFSETENABLE',
    'D3DRS_SOLIDOFFSETENABLE',
    'D3DRS_DEPTHCLIPCONTROL',
    'D3DRS_STIPPLEENABLE',
    'D3DRS_SIMPLE_UNUSED8',
    'D3DRS_SIMPLE_UNUSED7',
    'D3DRS_SIMPLE_UNUSED6',
    'D3DRS_SIMPLE_UNUSED5',
    'D3DRS_SIMPLE_UNUSED4',
    'D3DRS_SIMPLE_UNUSED3',
    'D3DRS_SIMPLE_UNUSED2',
    'D3DRS_SIMPLE_UNUSED1',
    'D3DRS_FOGENABLE',
    'D3DRS_FOGTABLEMODE',
    'D3DRS_FOGSTART',
    'D3DRS_FOGEND',
    'D3DRS_FOGDENSITY',
    'D3DRS_RANGEFOGENABLE',
    'D3DRS_WRAP0',
    'D3DRS_WRAP1',
    'D3DRS_WRAP2',
    'D3DRS_WRAP3',
    'D3DRS_LIGHTING',
    'D3DRS_SPECULARENABLE',
    'D3DRS_LOCALVIEWER',
    'D3DRS_COLORVERTEX',
    'D3DRS_BACKSPECULARMATERIALSOURCE',
    'D3DRS_BACKDIFFUSEMATERIALSOURCE',
    'D3DRS_BACKAMBIENTMATERIALSOURCE',
    'D3DRS_BACKEMISSIVEMATERIALSOURCE',
    'D3DRS_SPECULARMATERIALSOURCE',
    'D3DRS_DIFFUSEMATERIALSOURCE',
    'D3DRS_AMBIENTMATERIALSOURCE',
    'D3DRS_EMISSIVEMATERIALSOURCE',
    'D3DRS_BACKAMBIENT',
    'D3DRS_AMBIENT',
    'D3DRS_POINTSIZE',
    'D3DRS_POINTSIZE_MIN',
    'D3DRS_POINTSPRITEENABLE',
    'D3DRS_POINTSCALEENABLE',
    'D3DRS_POINTSCALE_A',
    'D3DRS_POINTSCALE_B',
    'D3DRS_POINTSCALE_C',
    'D3DRS_POINTSIZE_MAX',
    'D3DRS_PATCHEDGESTYLE',
    'D3DRS_PATCHSEGMENTS',
    'D3DRS_SWAPFILTER',
    'D3DRS_PRESENTATIONINTERVAL',
    'D3DRS_DEFERRED_UNUSED8',
    'D3DRS_DEFERRED_UNUSED7',
    'D3DRS_DEFERRED_UNUSED6',
    'D3DRS_DEFERRED_UNUSED5',
    'D3DRS_DEFERRED_UNUSED4',
    'D3DRS_DEFERRED_UNUSED3',
    'D3DRS_DEFERRED_UNUSED2',
    'D3DRS_DEFERRED_UNUSED1',
    'D3DRS_PSTEXTUREMODES',
    'D3DRS_VERTEXBLEND',
    'D3DRS_FOGCOLOR',
    'D3DRS_FILLMODE',
    'D3DRS_BACKFILLMODE',
    'D3DRS_TWOSIDEDLIGHTING',
    'D3DRS_NORMALIZENORMALS',
    'D3DRS_ZENABLE',
    'D3DRS_STENCILENABLE',
    'D3DRS_STENCILFAIL',
    'D3DRS_FRONTFACE',
    'D3DRS_CULLMODE',
    'D3DRS_TEXTUREFACTOR',
    'D3DRS_ZBIAS',
    'D3DRS_LOGICOP',
    'D3DRS_EDGEANTIALIAS',
    'D3DRS_MULTISAMPLEANTIALIAS',
    'D3DRS_MULTISAMPLEMASK',
    'D3DRS_MULTISAMPLEMODE',
    'D3DRS_MULTISAMPLERENDERTARGETMODE',
    'D3DRS_SHADOWFUNC',
    'D3DRS_LINEWIDTH',
    'D3DRS_SAMPLEALPHA',
    'D3DRS_DXT1NOISEENABLE',
    'D3DRS_YUVENABLE',
    'D3DRS_OCCLUSIONCULLENABLE',
    'D3DRS_STENCILCULLENABLE',
    'D3DRS_ROPZCMPALWAYSREAD',
    'D3DRS_ROPZREAD',
    'D3DRS_DONOTCULLUNCOMPRESSED'
  );

implementation

uses
  uEmuD3D8; // g_BuildVersion

// Returns the XDK version since which a render state was introduced (using the 5911 declarations as a base).
function DxbxRenderStateIntroducedAtVersion(const aRenderState: X_D3DRENDERSTATETYPE): uint32;
begin
  case aRenderState of
    X_D3DRS_DEPTHCLIPCONTROL..X_D3DRS_SIMPLE_UNUSED1:
      Result := 4627;

    X_D3DRS_SWAPFILTER:
      Result := 4361;

    X_D3DRS_PRESENTATIONINTERVAL..X_D3DRS_DEFERRED_UNUSED1:
      Result := 4627;

    X_D3DRS_MULTISAMPLERENDERTARGETMODE:
      Result := 4361;

    X_D3DRS_YUVENABLE..X_D3DRS_DONOTCULLUNCOMPRESSED:
      Result := 3911;
  else
    Result := 3424;
  end;
end;

// Returns True when a render state is an xbox-extension (compared to native Direct3D8).
function DxbxRenderStateIsXboxExtension(const Value: X_D3DRENDERSTATETYPE): Boolean; {NOPATCH}
begin
  case Value of
    X_D3DRS_PSALPHAINPUTS0 .. X_D3DRS_PSINPUTTEXTURE,
    X_D3DRS_BLENDCOLOR,
    X_D3DRS_SWATHWIDTH,
    X_D3DRS_POLYGONOFFSETZSLOPESCALE,
    X_D3DRS_POLYGONOFFSETZOFFSET,
    X_D3DRS_POINTOFFSETENABLE,
    X_D3DRS_WIREFRAMEOFFSETENABLE,
    X_D3DRS_SOLIDOFFSETENABLE,
    X_D3DRS_DEPTHCLIPCONTROL,
    X_D3DRS_STIPPLEENABLE,
    X_D3DRS_BACKSPECULARMATERIALSOURCE,
    X_D3DRS_BACKDIFFUSEMATERIALSOURCE,
    X_D3DRS_BACKAMBIENTMATERIALSOURCE,
    X_D3DRS_BACKEMISSIVEMATERIALSOURCE,
    X_D3DRS_BACKAMBIENT,
    X_D3DRS_SWAPFILTER,
    X_D3DRS_PRESENTATIONINTERVAL,
    X_D3DRS_PSTEXTUREMODES,
    X_D3DRS_BACKFILLMODE,
    X_D3DRS_TWOSIDEDLIGHTING,
    X_D3DRS_FRONTFACE,
    X_D3DRS_LOGICOP,
    X_D3DRS_MULTISAMPLEMODE,
    X_D3DRS_MULTISAMPLERENDERTARGETMODE,
    X_D3DRS_SHADOWFUNC,
    X_D3DRS_LINEWIDTH,
    X_D3DRS_SAMPLEALPHA,
    X_D3DRS_DXT1NOISEENABLE,
    X_D3DRS_YUVENABLE,
    X_D3DRS_OCCLUSIONCULLENABLE,
    X_D3DRS_STENCILCULLENABLE,
    X_D3DRS_ROPZCMPALWAYSREAD,
    X_D3DRS_ROPZREAD,
    X_D3DRS_DONOTCULLUNCOMPRESSED:
      Result := True;
  else
    Result := False;
  end;
end;

// Convert a 'method' DWORD into it's associated 'pixel-shader' or 'simple' render state.
function DxbxXboxMethodToRenderState(const aMethod: DWORD): X_D3DRenderStateType; {NOPATCH}
begin
  // Dxbx note : Let the compiler sort this out, should be much quicker :
  case (aMethod and $00001fff) of
    $00000100: Result := X_D3DRS_PS_RESERVED; // XDK 3424 uses $00000100 (NOP), while 3911 onwards uses $00001d90 (SET_COLOR_CLEAR_VALUE)
    $00000260: Result := X_D3DRS_PSALPHAINPUTS0;
    $00000264: Result := X_D3DRS_PSALPHAINPUTS1;
    $00000268: Result := X_D3DRS_PSALPHAINPUTS2;
    $0000026c: Result := X_D3DRS_PSALPHAINPUTS3;
    $00000270: Result := X_D3DRS_PSALPHAINPUTS4;
    $00000274: Result := X_D3DRS_PSALPHAINPUTS5;
    $00000278: Result := X_D3DRS_PSALPHAINPUTS6;
    $0000027c: Result := X_D3DRS_PSALPHAINPUTS7;
    $00000288: Result := X_D3DRS_PSFINALCOMBINERINPUTSABCD;
    $0000028c: Result := X_D3DRS_PSFINALCOMBINERINPUTSEFG;
    $00000300: Result := X_D3DRS_ALPHATESTENABLE;
    $00000304: Result := X_D3DRS_ALPHABLENDENABLE;
    $00000310: Result := X_D3DRS_DITHERENABLE;
    $00000330: Result := X_D3DRS_POINTOFFSETENABLE;
    $00000334: Result := X_D3DRS_WIREFRAMEOFFSETENABLE;
    $00000338: Result := X_D3DRS_SOLIDOFFSETENABLE;
    $0000033c: Result := X_D3DRS_ALPHAFUNC;
    $00000340: Result := X_D3DRS_ALPHAREF;
    $00000344: Result := X_D3DRS_SRCBLEND;
    $00000348: Result := X_D3DRS_DESTBLEND;
    $0000034c: Result := X_D3DRS_BLENDCOLOR;
    $00000350: Result := X_D3DRS_BLENDOP;
    $00000354: Result := X_D3DRS_ZFUNC;
    $00000358: Result := X_D3DRS_COLORWRITEENABLE;
    $0000035c: Result := X_D3DRS_ZWRITEENABLE;
    $00000360: Result := X_D3DRS_STENCILWRITEMASK;
    $00000364: Result := X_D3DRS_STENCILFUNC;
    $00000368: Result := X_D3DRS_STENCILREF;
    $0000036c: Result := X_D3DRS_STENCILMASK;
    $00000374: Result := X_D3DRS_STENCILZFAIL;
    $00000378: Result := X_D3DRS_STENCILPASS;
    $0000037c: Result := X_D3DRS_SHADEMODE;
    $00000384: Result := X_D3DRS_POLYGONOFFSETZSLOPESCALE;
    $00000388: Result := X_D3DRS_POLYGONOFFSETZOFFSET;
    $000009f8: Result := X_D3DRS_SWATHWIDTH;
    $00000a60: Result := X_D3DRS_PSCONSTANT0_0;
    $00000a64: Result := X_D3DRS_PSCONSTANT0_1;
    $00000a68: Result := X_D3DRS_PSCONSTANT0_2;
    $00000a6c: Result := X_D3DRS_PSCONSTANT0_3;
    $00000a70: Result := X_D3DRS_PSCONSTANT0_4;
    $00000a74: Result := X_D3DRS_PSCONSTANT0_5;
    $00000a78: Result := X_D3DRS_PSCONSTANT0_6;
    $00000a7c: Result := X_D3DRS_PSCONSTANT0_7;
    $00000a80: Result := X_D3DRS_PSCONSTANT1_0;
    $00000a84: Result := X_D3DRS_PSCONSTANT1_1;
    $00000a88: Result := X_D3DRS_PSCONSTANT1_2;
    $00000a8c: Result := X_D3DRS_PSCONSTANT1_3;
    $00000a90: Result := X_D3DRS_PSCONSTANT1_4;
    $00000a94: Result := X_D3DRS_PSCONSTANT1_5;
    $00000a98: Result := X_D3DRS_PSCONSTANT1_6;
    $00000a9c: Result := X_D3DRS_PSCONSTANT1_7;
    $00000aa0: Result := X_D3DRS_PSALPHAOUTPUTS0;
    $00000aa4: Result := X_D3DRS_PSALPHAOUTPUTS1;
    $00000aa8: Result := X_D3DRS_PSALPHAOUTPUTS2;
    $00000aac: Result := X_D3DRS_PSALPHAOUTPUTS3;
    $00000ab0: Result := X_D3DRS_PSALPHAOUTPUTS4;
    $00000ab4: Result := X_D3DRS_PSALPHAOUTPUTS5;
    $00000ab8: Result := X_D3DRS_PSALPHAOUTPUTS6;
    $00000abc: Result := X_D3DRS_PSALPHAOUTPUTS7;
    $00000ac0: Result := X_D3DRS_PSRGBINPUTS0;
    $00000ac4: Result := X_D3DRS_PSRGBINPUTS1;
    $00000ac8: Result := X_D3DRS_PSRGBINPUTS2;
    $00000acc: Result := X_D3DRS_PSRGBINPUTS3;
    $00000ad0: Result := X_D3DRS_PSRGBINPUTS4;
    $00000ad4: Result := X_D3DRS_PSRGBINPUTS5;
    $00000ad8: Result := X_D3DRS_PSRGBINPUTS6;
    $00000adc: Result := X_D3DRS_PSRGBINPUTS7;
    $0000147c: Result := X_D3DRS_STIPPLEENABLE;
    $000017f8: Result := X_D3DRS_PSCOMPAREMODE;
    $00001d78: Result := X_D3DRS_DEPTHCLIPCONTROL;
    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED1;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED2;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED3;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED4;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED5;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED6;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED7;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED8;
    $00001e20: Result := X_D3DRS_PSFINALCOMBINERCONSTANT0;
    $00001e24: Result := X_D3DRS_PSFINALCOMBINERCONSTANT1;
    $00001e40: Result := X_D3DRS_PSRGBOUTPUTS0;
    $00001e44: Result := X_D3DRS_PSRGBOUTPUTS1;
    $00001e48: Result := X_D3DRS_PSRGBOUTPUTS2;
    $00001e4c: Result := X_D3DRS_PSRGBOUTPUTS3;
    $00001e50: Result := X_D3DRS_PSRGBOUTPUTS4;
    $00001e54: Result := X_D3DRS_PSRGBOUTPUTS5;
    $00001e58: Result := X_D3DRS_PSRGBOUTPUTS6;
    $00001e5c: Result := X_D3DRS_PSRGBOUTPUTS7;
    $00001e60: Result := X_D3DRS_PSCOMBINERCOUNT;
    $00001e74: Result := X_D3DRS_PSDOTMAPPING;
    $00001e78: Result := X_D3DRS_PSINPUTTEXTURE;
  else
    int(Result) := -1;
  end;
end;

// convert from xbox to pc render state
function EmuXB2PC_D3DRS(Value: X_D3DRENDERSTATETYPE): D3DRENDERSTATETYPE;
begin
  case (Value) of
    X_D3DRS_ZFUNC                       : Result := D3DRS_ZFUNC;
    X_D3DRS_ALPHAFUNC                   : Result := D3DRS_ALPHAFUNC;
    X_D3DRS_ALPHABLENDENABLE            : Result := D3DRS_ALPHABLENDENABLE;
    X_D3DRS_ALPHATESTENABLE             : Result := D3DRS_ALPHATESTENABLE;
    X_D3DRS_ALPHAREF                    : Result := D3DRS_ALPHAREF;
    X_D3DRS_SRCBLEND                    : Result := D3DRS_SRCBLEND;
    X_D3DRS_DESTBLEND                   : Result := D3DRS_DESTBLEND;
    X_D3DRS_ZWRITEENABLE                : Result := D3DRS_ZWRITEENABLE;
    X_D3DRS_DITHERENABLE                : Result := D3DRS_DITHERENABLE;
    X_D3DRS_SHADEMODE                   : Result := D3DRS_SHADEMODE;
    X_D3DRS_COLORWRITEENABLE            : Result := D3DRS_COLORWRITEENABLE;
    X_D3DRS_STENCILZFAIL                : Result := D3DRS_STENCILZFAIL;
    X_D3DRS_STENCILPASS                 : Result := D3DRS_STENCILPASS;
    X_D3DRS_STENCILFUNC                 : Result := D3DRS_STENCILFUNC;
    X_D3DRS_STENCILREF                  : Result := D3DRS_STENCILREF;
    X_D3DRS_STENCILMASK                 : Result := D3DRS_STENCILMASK;
    X_D3DRS_STENCILWRITEMASK            : Result := D3DRS_STENCILWRITEMASK;
    X_D3DRS_BLENDOP                     : Result := D3DRS_BLENDOP;

    X_D3DRS_FOGENABLE                   : Result := D3DRS_FOGENABLE;
    X_D3DRS_FOGTABLEMODE                : Result := D3DRS_FOGTABLEMODE;
    X_D3DRS_FOGSTART                    : Result := D3DRS_FOGSTART;
    X_D3DRS_FOGEND                      : Result := D3DRS_FOGEND;
    X_D3DRS_FOGDENSITY                  : Result := D3DRS_FOGDENSITY;
    X_D3DRS_RANGEFOGENABLE              : Result := D3DRS_RANGEFOGENABLE;
    X_D3DRS_WRAP0                       : Result := D3DRS_WRAP0;
    X_D3DRS_WRAP1                       : Result := D3DRS_WRAP1;
    X_D3DRS_WRAP2                       : Result := D3DRS_WRAP2;
    X_D3DRS_WRAP3                       : Result := D3DRS_WRAP3;
    X_D3DRS_LIGHTING                    : Result := D3DRS_LIGHTING;
    X_D3DRS_SPECULARENABLE              : Result := D3DRS_SPECULARENABLE;
    X_D3DRS_LOCALVIEWER                 : Result := D3DRS_LOCALVIEWER;
    X_D3DRS_COLORVERTEX                 : Result := D3DRS_COLORVERTEX;
    X_D3DRS_SPECULARMATERIALSOURCE      : Result := D3DRS_SPECULARMATERIALSOURCE;
    X_D3DRS_DIFFUSEMATERIALSOURCE       : Result := D3DRS_DIFFUSEMATERIALSOURCE;
    X_D3DRS_AMBIENTMATERIALSOURCE       : Result := D3DRS_AMBIENTMATERIALSOURCE;
    X_D3DRS_EMISSIVEMATERIALSOURCE      : Result := D3DRS_EMISSIVEMATERIALSOURCE;
    X_D3DRS_AMBIENT                     : Result := D3DRS_AMBIENT;
    X_D3DRS_POINTSIZE                   : Result := D3DRS_POINTSIZE;
    X_D3DRS_POINTSIZE_MIN               : Result := D3DRS_POINTSIZE_MIN;
    X_D3DRS_POINTSPRITEENABLE           : Result := D3DRS_POINTSPRITEENABLE;
    X_D3DRS_POINTSCALEENABLE            : Result := D3DRS_POINTSCALEENABLE;
    X_D3DRS_POINTSCALE_A                : Result := D3DRS_POINTSCALE_A;
    X_D3DRS_POINTSCALE_B                : Result := D3DRS_POINTSCALE_B;
    X_D3DRS_POINTSCALE_C                : Result := D3DRS_POINTSCALE_C;
    X_D3DRS_POINTSIZE_MAX               : Result := D3DRS_POINTSIZE_MAX;
    X_D3DRS_PATCHEDGESTYLE              : Result := D3DRS_PATCHEDGESTYLE;
    X_D3DRS_PATCHSEGMENTS               : Result := D3DRS_PATCHSEGMENTS;
    X_D3DRS_VERTEXBLEND                 : Result := D3DRS_VERTEXBLEND;
    X_D3DRS_FOGCOLOR                    : Result := D3DRS_FOGCOLOR;
    X_D3DRS_FILLMODE                    : Result := D3DRS_FILLMODE;
    X_D3DRS_NORMALIZENORMALS            : Result := D3DRS_NORMALIZENORMALS;
    X_D3DRS_ZENABLE                     : Result := D3DRS_ZENABLE;
    X_D3DRS_STENCILENABLE               : Result := D3DRS_STENCILENABLE;
    X_D3DRS_STENCILFAIL                 : Result := D3DRS_STENCILFAIL;
    X_D3DRS_CULLMODE                    : Result := D3DRS_CULLMODE;
    X_D3DRS_TEXTUREFACTOR               : Result := D3DRS_TEXTUREFACTOR;
    X_D3DRS_ZBIAS                       : Result := {$IFDEF DXBX_USE_D3D9}D3DRS_DEPTHBIAS{$ELSE}D3DRS_ZBIAS{$ENDIF};
    X_D3DRS_EDGEANTIALIAS               : Result := {$IFDEF DXBX_USE_D3D9}D3DRS_ANTIALIASEDLINEENABLE{$ELSE}D3DRS_EDGEANTIALIAS{$ENDIF};
    X_D3DRS_MULTISAMPLEANTIALIAS        : Result := D3DRS_MULTISAMPLEANTIALIAS;
    X_D3DRS_MULTISAMPLEMASK             : Result := D3DRS_MULTISAMPLEMASK;

(* Direct3D8 states unused :
    D3DRS_LINEPATTERN
    D3DRS_LASTPIXEL
    D3DRS_ZVISIBLE
    D3DRS_WRAP4
    D3DRS_WRAP5
    D3DRS_WRAP6
    D3DRS_WRAP7
    D3DRS_CLIPPING
    D3DRS_FOGVERTEXMODE
    D3DRS_CLIPPLANEENABLE
    D3DRS_SOFTWAREVERTEXPROCESSING
    D3DRS_DEBUGMONITORTOKEN
    D3DRS_INDEXEDVERTEXBLENDENABLE
    D3DRS_TWEENFACTOR
    D3DRS_POSITIONORDER
    D3DRS_NORMALORDER

 Direct3D9 states unused :
    D3DRS_SCISSORTESTENABLE         = 174,
    D3DRS_SLOPESCALEDEPTHBIAS       = 175,
    D3DRS_ANTIALIASEDLINEENABLE     = 176,
    D3DRS_MINTESSELLATIONLEVEL      = 178,
    D3DRS_MAXTESSELLATIONLEVEL      = 179,
    D3DRS_ADAPTIVETESS_X            = 180,
    D3DRS_ADAPTIVETESS_Y            = 181,
    D3DRS_ADAPTIVETESS_Z            = 182,
    D3DRS_ADAPTIVETESS_W            = 183,
    D3DRS_ENABLEADAPTIVETESSELLATION = 184,
    D3DRS_TWOSIDEDSTENCILMODE       = 185,   // BOOL enable/disable 2 sided stenciling
    D3DRS_CCW_STENCILFAIL           = 186,   // D3DSTENCILOP to do if ccw stencil test fails
    D3DRS_CCW_STENCILZFAIL          = 187,   // D3DSTENCILOP to do if ccw stencil test passes and Z test fails
    D3DRS_CCW_STENCILPASS           = 188,   // D3DSTENCILOP to do if both ccw stencil and Z tests pass
    D3DRS_CCW_STENCILFUNC           = 189,   // D3DCMPFUNC fn.  ccw Stencil Test passes if ((ref & mask) stencilfn (stencil & mask)) is true
    D3DRS_COLORWRITEENABLE1         = 190,   // Additional ColorWriteEnables for the devices that support D3DPMISCCAPS_INDEPENDENTWRITEMASKS
    D3DRS_COLORWRITEENABLE2         = 191,   // Additional ColorWriteEnables for the devices that support D3DPMISCCAPS_INDEPENDENTWRITEMASKS
    D3DRS_COLORWRITEENABLE3         = 192,   // Additional ColorWriteEnables for the devices that support D3DPMISCCAPS_INDEPENDENTWRITEMASKS
    D3DRS_BLENDFACTOR               = 193,   // D3DCOLOR used for a constant blend factor during alpha blending for devices that support D3DPBLENDCAPS_BLENDFACTOR
    D3DRS_SRGBWRITEENABLE           = 194,   // Enable rendertarget writes to be DE-linearized to SRGB (for formats that expose D3DUSAGE_QUERY_SRGBWRITE)
    D3DRS_DEPTHBIAS                 = 195,
    D3DRS_WRAP8                     = 198,   // Additional wrap states for vs_3_0+ attributes with D3DDECLUSAGE_TEXCOORD
    D3DRS_WRAP9                     = 199,
    D3DRS_WRAP10                    = 200,
    D3DRS_WRAP11                    = 201,
    D3DRS_WRAP12                    = 202,
    D3DRS_WRAP13                    = 203,
    D3DRS_WRAP14                    = 204,
    D3DRS_WRAP15                    = 205,
    D3DRS_SEPARATEALPHABLENDENABLE  = 206,  // TRUE to enable a separate blending function for the alpha channel
    D3DRS_SRCBLENDALPHA             = 207,  // SRC blend factor for the alpha channel when D3DRS_SEPARATEDESTALPHAENABLE is TRUE
    D3DRS_DESTBLENDALPHA            = 208,  // DST blend factor for the alpha channel when D3DRS_SEPARATEDESTALPHAENABLE is TRUE
    D3DRS_BLENDOPALPHA              = 209   // Blending operation for the alpha channel when D3DRS_SEPARATEDESTALPHAENABLE is TRUE
*)
  else
    Result := D3DRENDERSTATETYPE(D3DRS_UNSUPPORTED);
  end;
end;

function DxbxXB2PC_NOP(Value: DWORD): DWORD;
begin
  Result := Value;
end;

procedure DxbxBuildRenderStateMappingTable(const aD3DRenderState: PDWORDs); {NOPATCH}
var
  State: X_D3DRENDERSTATETYPE;
  State_VersionDependent: X_D3DRENDERSTATETYPE;
  XTL_EmuD3DDeferredRenderState: PDWORDs;
  i: Integer;
begin
  if not Assigned(aD3DRenderState) then
    Exit;

  // Loop over all latest (5911) states :
  State_VersionDependent := 0;
  for State := X_D3DRS_FIRST to X_D3DRS_LAST do
  begin
    // Check if this state is available in the active SDK version :
    if g_BuildVersion >= DxbxRenderStateIntroducedAtVersion(State) then
    begin
      // If it is available, register this offset in the various mapping tables we use :
      DxbxMapActiveVersionToMostRecent[State_VersionDependent] := State;
      DxbxMapMostRecentToActiveVersion[State] := State_VersionDependent;
      XTL_EmuMappedD3DRenderState[State] := @(aD3DRenderState[State_VersionDependent]);
      // Step to the next offset :
      Inc(State_VersionDependent);
    end
    else
    begin
      // When unavailable, apply a dummy pointer, and *don't* increment the version dependent state,
      // so the mapping table will correspond to the actual (version dependent) layout :
      // DxbxMapActiveVersionToMostRecent shouldn't be set here, as there's no element for this state!
      DxbxMapMostRecentToActiveVersion[State] := X_D3DRS_UNSUPPORTED;
      XTL_EmuMappedD3DRenderState[State] := DummyRenderState;
    end;
  end;

  // Log the start address of the "deferred" render states (not needed anymore, just to keep logging the same) :
  begin
    // Calculate the location of D3DDeferredRenderState via an XDK-dependent offset to _D3D__RenderState :
    XTL_EmuD3DDeferredRenderState := aD3DRenderState;
    // Dxbx note : XTL_EmuD3DDeferredRenderState:PDWORDs cast to UIntPtr to avoid incrementing with that many array-sizes!
    Inc(UIntPtr(XTL_EmuD3DDeferredRenderState), DxbxMapMostRecentToActiveVersion[X_D3DRS_DEFERRED_FIRST] * 4);
    DbgPrintf('HLE: $%.08X -> EmuD3DDeferredRenderState', [XTL_EmuD3DDeferredRenderState]);
  end;

  // Build a table with converter functions for all renderstates :
  for i := X_D3DRS_FIRST to X_D3DRS_LAST do
    DxbxRenderStateXB2PCCallback[i] := @DxbxXB2PC_NOP;

  // For all render states that need a specific converter, assign the one handling the underlying type :
  DxbxRenderStateXB2PCCallback[X_D3DRS_WRAP0] := @EmuXB2PC_D3DWRAP;
  DxbxRenderStateXB2PCCallback[X_D3DRS_WRAP1] := @EmuXB2PC_D3DWRAP;
  DxbxRenderStateXB2PCCallback[X_D3DRS_WRAP2] := @EmuXB2PC_D3DWRAP;
  DxbxRenderStateXB2PCCallback[X_D3DRS_WRAP3] := @EmuXB2PC_D3DWRAP;
  DxbxRenderStateXB2PCCallback[X_D3DRS_FILLMODE] := @EmuXB2PC_D3DFILLMODE;
  DxbxRenderStateXB2PCCallback[X_D3DRS_BACKFILLMODE] := @EmuXB2PC_D3DFILLMODE;
  DxbxRenderStateXB2PCCallback[X_D3DRS_SHADEMODE] := @EmuXB2PC_D3DSHADEMODE;
  DxbxRenderStateXB2PCCallback[X_D3DRS_SRCBLEND] := @EmuXB2PC_D3DBLEND;
  DxbxRenderStateXB2PCCallback[X_D3DRS_DESTBLEND] := @EmuXB2PC_D3DBLEND;
  DxbxRenderStateXB2PCCallback[X_D3DRS_ZFUNC] := @EmuXB2PC_D3DCMPFUNC;
  DxbxRenderStateXB2PCCallback[X_D3DRS_ALPHAFUNC] := @EmuXB2PC_D3DCMPFUNC;
//  DxbxRenderStateXB2PCCallback[X_D3DRS_FOGTABLEMODE] := @EmuXB2PC_D3DFOGMODE; // No conversion needed; Xbox = PC
  DxbxRenderStateXB2PCCallback[X_D3DRS_STENCILFAIL] := @EmuXB2PC_D3DSTENCILOP;
  DxbxRenderStateXB2PCCallback[X_D3DRS_STENCILZFAIL] := @EmuXB2PC_D3DSTENCILOP;
  DxbxRenderStateXB2PCCallback[X_D3DRS_STENCILPASS] := @EmuXB2PC_D3DSTENCILOP;
  DxbxRenderStateXB2PCCallback[X_D3DRS_STENCILFUNC] := @EmuXB2PC_D3DCMPFUNC;
//  DxbxRenderStateXB2PCCallback[X_D3DRS_DIFFUSEMATERIALSOURCE] := @EmuXB2PC_D3DMCS; // No conversion needed; Xbox = PC
//  DxbxRenderStateXB2PCCallback[X_D3DRS_BACKDIFFUSEMATERIALSOURCE] := @EmuXB2PC_D3DMCS; // Never used; Xbox ext.
//  DxbxRenderStateXB2PCCallback[X_D3DRS_SPECULARMATERIALSOURCE] := @EmuXB2PC_D3DMCS; // No conversion needed; Xbox = PC
//  DxbxRenderStateXB2PCCallback[X_D3DRS_BACKSPECULARMATERIALSOURCE] := @EmuXB2PC_D3DMCS; // Never used; Xbox ext.
//  DxbxRenderStateXB2PCCallback[X_D3DRS_AMBIENTMATERIALSOURCE] := @EmuXB2PC_D3DMCS; // No conversion needed; Xbox = PC
//  DxbxRenderStateXB2PCCallback[X_D3DRS_BACKAMBIENTMATERIALSOURCE] := @EmuXB2PC_D3DMCS; // Never used; Xbox ext.
//  DxbxRenderStateXB2PCCallback[X_D3DRS_EMISSIVEMATERIALSOURCE] := @EmuXB2PC_D3DMCS; // No conversion needed; Xbox = PC
//  DxbxRenderStateXB2PCCallback[X_D3DRS_BACKEMISSIVEMATERIALSOURCE] := @EmuXB2PC_D3DMCS; // Never used; Xbox ext.
  DxbxRenderStateXB2PCCallback[X_D3DRS_VERTEXBLEND] := @EmuXB2PC_D3DVERTEXBLENDFLAGS;
//  DxbxRenderStateXB2PCCallback[X_D3DRS_SWAPFILTER] := @EmuXB2PC_D3DMULTISAMPLE_TYPE; // Never used; Xbox ext.
  DxbxRenderStateXB2PCCallback[X_D3DRS_COLORWRITEENABLE] := @EmuXB2PC_D3DCOLORWRITEENABLE; // No conversion needed; Xbox = PC
  DxbxRenderStateXB2PCCallback[X_D3DRS_BLENDOP] := @EmuXB2PC_D3DBLENDOP;
//  DxbxRenderStateXB2PCCallback[X_D3DRS_SWATHWIDTH] := @EmuXB2PC_D3DSWATH; // Never used; Xbox ext.
//  DxbxRenderStateXB2PCCallback[X_D3DRS_SHADOWFUNC] := @EmuXB2PC_D3DCMPFUNC; // Never used; Xbox ext.

end;

var
  DummyRenderStateValue: X_D3DRENDERSTATETYPE;

procedure DxbxInitializeDefaultRenderStates(const aParameters: PX_D3DPRESENT_PARAMETERS); {NOPATCH}
var
  i: Integer;
begin
  // Initialize the dummy render state :
  DummyRenderStateValue := 0;
  DummyRenderState := @DummyRenderStateValue;

  // First, clear out all render states :
  for i := X_D3DRS_FIRST to X_D3DRS_LAST do
    XTL_EmuMappedD3DRenderState[i]^ := 0;

  // Now set the "deferred" states to 'unknown' :
  for i := X_D3DRS_DEFERRED_FIRST to X_D3DRS_DEFERRED_LAST do
    XTL_EmuMappedD3DRenderState[i]^ := X_D3DRS_UNK; // TODO : Explain why the default 0 causes dots in PointSprites sample!

  // Assign all Xbox default render states values :
  begin
    // Make up a few pixel shader colors here (we really should find&use the actual defaults!) :
    for i := X_D3DRS_PSCONSTANT0_0 to X_D3DRS_PSCONSTANT1_7 do
      XTL_EmuMappedD3DRenderState[i]^ := (X_D3DRS_PSCONSTANT1_7 - i) * $01010101;

    if DxbxFix_HasZBuffer then
      XTL_EmuMappedD3DRenderState[X_D3DRS_ZENABLE]^ := D3DZB_TRUE
    else
      XTL_EmuMappedD3DRenderState[X_D3DRS_ZENABLE]^ := D3DZB_FALSE;

    XTL_EmuMappedD3DRenderState[X_D3DRS_FILLMODE]^ := DWORD(X_D3DFILL_SOLID);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKFILLMODE]^ := DWORD(X_D3DFILL_SOLID);
    XTL_EmuMappedD3DRenderState[X_D3DRS_TWOSIDEDLIGHTING]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_SHADEMODE]^ := DWORD(X_D3DSHADE_GOURAUD);
    XTL_EmuMappedD3DRenderState[X_D3DRS_ZWRITEENABLE]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_ALPHATESTENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_SRCBLEND]^ := DWORD(X_D3DBLEND_ONE);
    XTL_EmuMappedD3DRenderState[X_D3DRS_DESTBLEND]^ := DWORD(X_D3DBLEND_ZERO);
    XTL_EmuMappedD3DRenderState[X_D3DRS_FRONTFACE]^ := DWORD(X_D3DFRONT_CW);
    XTL_EmuMappedD3DRenderState[X_D3DRS_CULLMODE]^ := DWORD(X_D3DCULL_CCW);
    XTL_EmuMappedD3DRenderState[X_D3DRS_ZFUNC]^ := DWORD(X_D3DCMP_LESSEQUAL);
//  XTL_EmuMappedD3DRenderState[X_D3DRS_ALPHAREF]^ := 0; // Unknown default
    XTL_EmuMappedD3DRenderState[X_D3DRS_ALPHAFUNC]^ := DWORD(X_D3DCMP_ALWAYS);
    XTL_EmuMappedD3DRenderState[X_D3DRS_DITHERENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_ALPHABLENDENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_SPECULARENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGCOLOR]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGTABLEMODE]^ := DWORD(X_D3DFOG_NONE);
    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGSTART]^ := F2DW(0.0); // Unknown default
    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGEND]^ := F2DW(1.0); // Unknown default
    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGDENSITY]^ := F2DW(1.0);
    XTL_EmuMappedD3DRenderState[X_D3DRS_EDGEANTIALIAS]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_ZBIAS]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_RANGEFOGENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILFAIL]^ := DWORD(X_D3DSTENCILOP_KEEP);
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILZFAIL]^ := DWORD(X_D3DSTENCILOP_KEEP);
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILPASS]^ := DWORD(X_D3DSTENCILOP_KEEP);
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILFUNC]^ := DWORD(X_D3DCMP_ALWAYS);
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILREF]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILMASK]^ := $FFFFFFFF;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILWRITEMASK]^ := $FFFFFFFF;
//    XTL_EmuMappedD3DRenderState[X_D3DRS_TEXTUREFACTOR]^ := 0; // Unknown default
    XTL_EmuMappedD3DRenderState[X_D3DRS_WRAP0]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_WRAP1]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_WRAP2]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_WRAP3]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_LIGHTING]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_AMBIENT]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKAMBIENT]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_COLORVERTEX]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_LOCALVIEWER]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_NORMALIZENORMALS]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_DIFFUSEMATERIALSOURCE]^ := DWORD(X_D3DMCS_COLOR1);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKDIFFUSEMATERIALSOURCE]^ := DWORD(X_D3DMCS_COLOR1);
    XTL_EmuMappedD3DRenderState[X_D3DRS_SPECULARMATERIALSOURCE]^ := DWORD(X_D3DMCS_COLOR2);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKSPECULARMATERIALSOURCE]^ := DWORD(X_D3DMCS_COLOR2);
    XTL_EmuMappedD3DRenderState[X_D3DRS_AMBIENTMATERIALSOURCE]^ := DWORD(X_D3DMCS_COLOR2);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKAMBIENTMATERIALSOURCE]^ := DWORD(X_D3DMCS_COLOR2);
    XTL_EmuMappedD3DRenderState[X_D3DRS_EMISSIVEMATERIALSOURCE]^ := DWORD(X_D3DMCS_MATERIAL);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKEMISSIVEMATERIALSOURCE]^ := DWORD(X_D3DMCS_MATERIAL);
    XTL_EmuMappedD3DRenderState[X_D3DRS_VERTEXBLEND]^ := DWORD(X_D3DVBF_DISABLE);
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSIZE]^ := F2DW(1.0);
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSIZE_MIN]^ := F2DW(1.0);
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSPRITEENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSCALEENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSCALE_A]^ := F2DW(1.0);
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSCALE_B]^ := F2DW(0.0);
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSCALE_C]^ := F2DW(0.0);
    XTL_EmuMappedD3DRenderState[X_D3DRS_MULTISAMPLEANTIALIAS]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_MULTISAMPLEMASK]^ := $FFFFFFFF;
    XTL_EmuMappedD3DRenderState[X_D3DRS_MULTISAMPLEMODE]^ := aParameters.MultiSampleType;
    XTL_EmuMappedD3DRenderState[X_D3DRS_MULTISAMPLERENDERTARGETMODE]^ := DWORD(X_D3DMULTISAMPLEMODE_1X);
    XTL_EmuMappedD3DRenderState[X_D3DRS_SWAPFILTER]^ := aParameters.MultiSampleType;
    XTL_EmuMappedD3DRenderState[X_D3DRS_PRESENTATIONINTERVAL]^ := aParameters.FullScreen_PresentationInterval;
//    XTL_EmuMappedD3DRenderState[X_D3DRS_PATCHEDGESTYLE]^ := 0; // Unknown default
//    XTL_EmuMappedD3DRenderState[X_D3DRS_PATCHSEGMENTS]^ := 0; // Unknown default
//    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSIZE_MAX]^ := between D3DCAPS8.MaxPointSize and D3DRS_POINTSIZE_MIN (including)
    XTL_EmuMappedD3DRenderState[X_D3DRS_COLORWRITEENABLE]^ := X_D3DCOLORWRITEENABLE_ALL;
    XTL_EmuMappedD3DRenderState[X_D3DRS_BLENDOP]^ := DWORD(X_D3DBLENDOP_ADD);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BLENDCOLOR]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_SWATHWIDTH]^ := DWORD(X_D3DSWATH_128);
    XTL_EmuMappedD3DRenderState[X_D3DRS_DXT1NOISEENABLE]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_YUVENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_OCCLUSIONCULLENABLE]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILCULLENABLE]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_ROPZCMPALWAYSREAD]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_ROPZREAD]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_DONOTCULLUNCOMPRESSED]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POLYGONOFFSETZSLOPESCALE]^ := F2DW(0.0);
    XTL_EmuMappedD3DRenderState[X_D3DRS_POLYGONOFFSETZOFFSET]^ := F2DW(0.0);
    XTL_EmuMappedD3DRenderState[X_D3DRS_SOLIDOFFSETENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_DEPTHCLIPCONTROL]^ := X_D3DDCC_CULLPRIMITIVE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STIPPLEENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTOFFSETENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_WIREFRAMEOFFSETENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_SHADOWFUNC]^ := DWORD(X_D3DCMP_NEVER);
    XTL_EmuMappedD3DRenderState[X_D3DRS_LINEWIDTH]^ := F2DW(1.0); // Unknown default
    XTL_EmuMappedD3DRenderState[X_D3DRS_SAMPLEALPHA]^ := 0; // Unknown default
//    XTL_EmuMappedD3DRenderState[X_D3DSAMPLEALPHA_TOONE]^ := 0; // Unknown default
    XTL_EmuMappedD3DRenderState[X_D3DRS_LOGICOP]^ := DWORD(X_D3DLOGICOP_NONE);
  end;
end;

// Converts the input render state from a version-dependent into a version-neutral value.
function DxbxVersionAdjust_D3DRS(const Value: X_D3DRENDERSTATETYPE): X_D3DRENDERSTATETYPE; {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := DxbxMapActiveVersionToMostRecent[Value];
end;

const
  OLD_X_D3DTSS_COLOROP = 0;
  OLD_X_D3DTSS_TEXTURETRANSFORMFLAGS = 9;
  OLD_X_D3DTSS_ADDRESSU = 10;
  OLD_X_D3DTSS_ALPHAKILL = 21;
// For 3925, the actual D3DTSS flags have different values.
// This function maps new indexes to old ones, so that we
// can read a specific member from the emulated XBE's
// XTL_EmuD3DDeferredTextureState buffer.
function DxbxFromNewVersion_D3DTSS(const NewValue: X_D3DTEXTURESTAGESTATETYPE): X_D3DTEXTURESTAGESTATETYPE; {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := NewValue;
  if g_BuildVersion <= 3925 then
  begin
    // In SDK 3925 (or at somewhere else between 3911 and 4361), the deferred texture states where switched;
    // D3DTSS_COLOROP ..D3DTSS_TEXTURETRANSFORMFLAGS ranged  0.. 9 which has become 12..21
    // D3DTSS_ADDRESSU..D3DTSS_ALPHAKILL             ranged 10..21 which has become  0..11
    if (NewValue <= X_D3DTSS_TEXTURETRANSFORMFLAGS) then
      if (NewValue <= X_D3DTSS_ALPHAKILL) then
        Inc(Result, OLD_X_D3DTSS_ADDRESSU)
      else
        Dec(Result, {NEW}X_D3DTSS_COLOROP);
  end;
end;

function DxbxTextureStageStateIsXboxExtension(const Value: X_D3DTEXTURESTAGESTATETYPE): Boolean; {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  case Value of
    X_D3DTSS_COLORKEYOP,
    X_D3DTSS_COLORSIGN,
    X_D3DTSS_ALPHAKILL,
    X_D3DTSS_COLORKEYCOLOR:
      Result := True;
  else
    Result := False;
  end;
end;

function DxbxFromOldVersion_D3DTSS(const OldValue: X_D3DTEXTURESTAGESTATETYPE): X_D3DTEXTURESTAGESTATETYPE; {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := OldValue;
  if g_BuildVersion <= 3925 then
  begin
    // In SDK 3925 (or at somewhere else between 3911 and 4361), the deferred texture states where switched;
    // D3DTSS_COLOROP ..D3DTSS_TEXTURETRANSFORMFLAGS ranged  0.. 9 which has become 12..21
    // D3DTSS_ADDRESSU..D3DTSS_ALPHAKILL             ranged 10..21 which has become  0..11
    if (OldValue <= OLD_X_D3DTSS_ALPHAKILL) then
      if (OldValue <= OLD_X_D3DTSS_TEXTURETRANSFORMFLAGS) then
        Inc(Result, 12)
      else
        Dec(Result, 10);
  end;
end;

function Dxbx_SetRenderState(const XboxRenderState: X_D3DRenderStateType; const XboxValue: DWORD): DWORD; {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  RenderStateValue: TD3DColorValue;
  PCRenderState: D3DRenderStateType;
  PCValue: DWORD;
begin
  Result := XboxValue;

  // Handle the pixel shader constants first :
  if (X_D3DRS_PSCONSTANT0_0 <= XboxRenderState) and (XboxRenderState <= X_D3DRS_PSCONSTANT1_7) then
  begin
    // Convert color DWORD to a D3DColor :
    RenderStateValue.r := ((XboxValue shr 16) and $FF) / 255.0;
    RenderStateValue.g := ((XboxValue shr  8) and $FF) / 255.0;
    RenderStateValue.b := ((XboxValue shr  0) and $FF) / 255.0;
    RenderStateValue.a := ((XboxValue shr 24) and $FF) / 255.0;

    // Convert Xbox render state to Pixel shader constant number :
    PCValue := EmuXB2PC_PSConstant(XboxRenderState);

    // Safeguard against overflowing native pixel shader constant count :
    if PCValue >= 8 then
      Exit;

    // Set the constant locally :
{$IFDEF DXBX_USE_D3D9}
    g_pD3DDevice.SetPixelShaderConstantF(PCValue, PSingle(@RenderStateValue), 1);
{$ELSE}
    g_pD3DDevice.SetPixelShaderConstant(PCValue, {untyped const}RenderStateValue, 1);
{$ENDIF}

    Exit;
  end;

  // Check if the render state is mapped :
  if XTL_EmuMappedD3DRenderState[XboxRenderState] <> DummyRenderState then
  begin
    // Map the Xbox state to a PC state, and check if it's supported :
    PCRenderState := EmuXB2PC_D3DRS(XboxRenderState); // TODO : Speed this up using a lookup table
    if Ord(PCRenderState) <> Ord(D3DRS_UNSUPPORTED) then
    begin
      // Convert the value from Xbox format into PC format, and set it locally :
      Result := DxbxRenderStateXB2PCCallback[XboxRenderState](XboxValue);

      g_pD3DDevice.SetRenderState(PCRenderState, {PCValue=}Result);
    end
    else
      ; // TODO : What do we return when the render state is not supported?
  end
  else
    DxbxKrnlCleanup('Unsupported RenderState (0x%.08X)', [int(XboxRenderState)]);
end;

function DxbxTransferRenderState(const XboxRenderState: X_D3DRENDERSTATETYPE): HResult;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  XboxValue: DWORD;
begin
  // Read the current Xbox value, and set it locally :
  XboxValue := XTL_EmuMappedD3DRenderState[XboxRenderState]^;
  Dxbx_SetRenderState(XboxRenderState, XboxValue);
  Result := D3D_OK;
end;

procedure XTL_EmuUpdateDeferredStates(); {NOPATCH}
// Branch:shogun  Revision:163  Translator:PatrickvL  Done:100
var
  State: int;
  Stage: int;
  XboxValue: DWORD;
  PCValue: DWORD;
  pTexture: XTL_PIDirect3DBaseTexture8;
begin
  // Generic transfer of all Xbox deferred render states to PC :
  for State := X_D3DRS_DEFERRED_FIRST to X_D3DRS_DEFERRED_LAST do
    DxbxTransferRenderState(X_D3DRENDERSTATETYPE(State));

  // Certain D3DTS values need to be checked on each Draw[Indexed]^Vertices
  if (XTL_EmuD3DDeferredTextureState <> nil) then
  begin
    for Stage := 0 to X_D3DTS_STAGECOUNT-1 do
    begin
      for State := X_D3DTSS_DEFERRED_FIRST to X_D3DTSS_DEFERRED_LAST do
      begin
        XboxValue := XTL_EmuD3DDeferredTextureState[Stage, Ord(DxbxFromNewVersion_D3DTSS(State))];
        if (XboxValue = X_D3DTSS_UNK) then
          Continue;

        // Convert Xbox value to PC value for a few differing texture stage state :
        PCValue := XboxValue;
        case State of
          X_D3DTSS_ADDRESSU,
          X_D3DTSS_ADDRESSV,
          X_D3DTSS_ADDRESSW:
            if (XboxValue = 5) then
              DxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

          X_D3DTSS_MAGFILTER,
          X_D3DTSS_MINFILTER,
          X_D3DTSS_MIPFILTER:
            if (XboxValue = 4) then
              DxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

          X_D3DTSS_COLOROP,
          X_D3DTSS_ALPHAOP:
            PCValue := EmuXB2PC_D3DTEXTUREOP(XboxValue);

          // Xbox extensions :
          X_D3DTSS_COLORKEYOP,
          X_D3DTSS_COLORSIGN,
          X_D3DTSS_ALPHAKILL,
          X_D3DTSS_COLORKEYCOLOR:
            // TODO -oDxbx : Emulate these Xbox extensions somehow
            Continue;
        end;

        IDirect3DDevice_SetTextureStageState(g_pD3DDevice, Stage, State, PCValue);
      end;
    end;

    // if point sprites are enabled, copy stage 3 over to 0
    if (XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSPRITEENABLE]^ = DWord(BOOL_TRUE)) then // Dxbx note : DWord cast to prevent warning
    begin
      // set the point sprites texture
      g_pD3DDevice.GetTexture(3, PIDirect3DBaseTexture(@pTexture));
      g_pD3DDevice.SetTexture(0, IDirect3DBaseTexture(pTexture));
      // TODO -oDXBX: Should we clear the pTexture interface (and how)?

      // disable all other stages
      IDirect3DDevice_SetTextureStageState(g_pD3DDevice, 1, X_D3DTSS_COLOROP, D3DTOP_DISABLE);
      IDirect3DDevice_SetTextureStageState(g_pD3DDevice, 1, X_D3DTSS_ALPHAOP, D3DTOP_DISABLE);

      // copy over the stage
      for State := 0 to X_D3DTS_STAGESIZE-1 do
      begin
        XboxValue := XTL_EmuD3DDeferredTextureState[3, Ord(DxbxFromNewVersion_D3DTSS(State))];
        if (XboxValue <> X_D3DTSS_UNK) then
        begin
          PCValue := XboxValue; // TODO : Use registry of callbacks to do this conversion.
          IDirect3DDevice_GetTextureStageState(g_pD3DDevice, 3, State, {out}PCValue);
          IDirect3DDevice_SetTextureStageState(g_pD3DDevice, 0, State, PCValue);
        end;
      end;
    end;
  end;

  if (g_bFakePixelShaderLoaded) then
  begin
    g_pD3DDevice.SetRenderState(D3DRS_FOGENABLE, BOOL_FALSE);

    // programmable pipeline
    (* Dxbx note : If the following is enabled, Sokoban loses it's textures, so disable it for now :
    for v:=0 to X_D3DTS_STAGECOUNT-1 do
    begin
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLOROP, D3DTOP_DISABLE);
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
    end;
    *)

    // fixed pipeline
    (* Cxbx has this disabled :
    for v:=0 to 4-1 do
    begin
      IDirect3DDevice_SetTextureStageState(g_pD3DDevice, v, X_D3DTSS_COLOROP,   D3DTOP_MODULATE);
      IDirect3DDevice_SetTextureStageState(g_pD3DDevice, v, X_D3DTSS_COLORARG1, D3DTA_TEXTURE);
      IDirect3DDevice_SetTextureStageState(g_pD3DDevice, v, X_D3DTSS_COLORARG2, D3DTA_CURRENT);

      IDirect3DDevice_SetTextureStageState(g_pD3DDevice, v, X_D3DTSS_ALPHAOP,   D3DTOP_MODULATE);
      IDirect3DDevice_SetTextureStageState(g_pD3DDevice, v, X_D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
      IDirect3DDevice_SetTextureStageState(g_pD3DDevice, v, X_D3DTSS_ALPHAARG2, D3DTA_CURRENT);
    end;

    g_pD3DDevice.SetRenderState(D3DRS_NORMALIZENORMALS, BOOL_TRUE);
    g_pD3DDevice.SetRenderState(D3DRS_LIGHTING,BOOL_TRUE);
    g_pD3DDevice.SetRenderState(D3DRS_AMBIENT, $FFFFFFFF);
    *)
  end;
end;

{.$MESSAGE 'PatrickvL reviewed up to here'}
end.
