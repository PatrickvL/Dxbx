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
  SysUtils, // strlen
  Classes,
  Math, // IfThen
  // DirectX
{$IFDEF DXBX_USE_D3D9}
  Direct3D9,
  D3DX9,
{$ELSE}
  Direct3D8, // IDirect3DBaseTexture8
  D3DX8,
{$ENDIF}
  // Dxbx
  uConsts,
  uTypes,
  uLog,
  uConvert,
  uDxbxUtils, // iif
  uDxbxKrnlUtils,
  uEmu,
  uEmuAlloc,
  uEmuD3D8Types,
  uEmuD3D8Utils;

function DxbxXboxMethodToRenderState(const aMethod: X_NV2AMETHOD): X_D3DRenderStateType; {NOPATCH}

procedure DxbxBuildRenderStateMappingTable(const aD3DRenderState: PDWORDs); {NOPATCH}
procedure DxbxInitializeDefaultRenderStates(const aParameters: PX_D3DPRESENT_PARAMETERS); {NOPATCH}

function DxbxVersionAdjust_D3DRS(const XboxRenderState_VersionDependent: X_D3DRENDERSTATETYPE): X_D3DRENDERSTATETYPE; {NOPATCH}
function Dxbx_SetRenderState(const XboxRenderState: X_D3DRenderStateType; XboxValue: DWORD): DWORD; {NOPATCH}
function DxbxTransferRenderState(const XboxRenderState: X_D3DRENDERSTATETYPE): HResult;

function DxbxFromOldVersion_D3DTSS(const OldValue: X_D3DTEXTURESTAGESTATETYPE): X_D3DTEXTURESTAGESTATETYPE; {NOPATCH}
function DxbxFromNewVersion_D3DTSS(const NewValue: X_D3DTEXTURESTAGESTATETYPE): X_D3DTEXTURESTAGESTATETYPE; {NOPATCH}

procedure XTL_EmuUpdateDeferredStates(); {NOPATCH}
procedure XTL_EmuUpdateActiveTexture(); {NOPATCH}

procedure DxbxGetFormatRelatedVariables(
  const pPixelContainer: PX_D3DPixelContainer;
  const X_Format: X_D3DFORMAT;
  var dwWidth: DWORD;
  var dwHeight: DWORD;
  var dwBPP: DWORD;
  var dwDepth: DWORD;
  var dwPitch: DWORD;
  var dwMipMapLevels: DWORD;
  var bSwizzled: BOOL_;
  var bCompressed: BOOL_;
  var dwCompressedSize: DWORD;
  var bCubeMap: BOOL_); {NOPATCH}

procedure DxbxUpdatePixelContainer(
  pPixelContainer: PX_D3DPixelContainer;
  dwCommonType: DWORD;

  dwWidth: DWORD;
  dwHeight: DWORD;
  dwBPP: DWORD;
  dwDepth: DWORD;
  dwPitch: DWORD;
  dwMipMapLevels: DWORD;
  bSwizzled: BOOL_;
  bCompressed: BOOL_;
  dwCompressedSize: DWORD;
  bCubeMap: BOOL_;
  CacheFormat: X_D3DFORMAT
  ); {NOPATCH}

procedure DxbxUpdateNativeD3DResources(); {NOPATCH}

const X_D3DRS_UNSUPPORTED = X_D3DRS_LAST+1;

// XDK version independent renderstate table, containing pointers to the original locations.
var XTL_EmuMappedD3DRenderState: array [X_D3DRS_FIRST..X_D3DRS_LAST+1] of PDWORD; // 1 extra for the unsupported value

// Dxbx addition : Dummy value (and pointer to that) to transparently ignore unsupported render states :
var DummyRenderStateValue: X_D3DRENDERSTATETYPE = 0;
const DummyRenderState: PDWORD = @DummyRenderStateValue; // Unsupported states share this pointer value

// Dxbx indicator of an unsupported render state (returned when processing an Xbox extension) :
const D3DRS_UNSUPPORTED = D3DRENDERSTATETYPE(0); // Defined as zero, to coincide with default value of DxbxRenderStateInfo.PC

// Texture state lookup table (same size in all XDK versions, so defined as a fixed size array) :
type TD3DDeferredTextureState = array [0..X_D3DTS_STAGECOUNT-1, 0..X_D3DTS_STAGESIZE-1] of DWORD;
     PD3DDeferredTextureState = ^TD3DDeferredTextureState;
var XTL_EmuD3DDeferredTextureState: PD3DDeferredTextureState;

// The Xbox1 D3D__Device :
var XTL_D3D__Device: PDWORDs = nil;

var XTL_D3D__RenderState: PDWORDs = nil;

type D3D_InitializeD3dState = function(): int; cdecl;
var XTL_D3D_InitializeD3dState: D3D_InitializeD3dState;

type  Direct3D_CreateDevice = function(
    Adapter: UINT;
    DeviceType: D3DDEVTYPE;
    hFocusWindow: HWND;
    BehaviorFlags: DWORD;
    pPresentationParameters: PX_D3DPRESENT_PARAMETERS;
    ppReturnedDeviceInterface: XTL_PPIDirect3DDevice8
): HRESULT; stdcall;
var XTL_Direct3D_CreateDevice: Direct3D_CreateDevice;


const DEFAULT_XDK_VERSION = 4627; // TODO -oDxbx : Make this configurable
var g_BuildVersion: WORD = DEFAULT_XDK_VERSION;
// var g_OrigBuildVersion: uint32; // Dxbx note : Unused

var X_D3DSCM_CORRECTION_VersionDependent: int = 0;

var
  DxbxTextureStageStateXB2PCCallback: array [X_D3DTSS_FIRST..X_D3DTSS_LAST] of TXB2PCFunc;
  DxbxRenderStateXB2PCCallback: array [X_D3DRS_FIRST..X_D3DRS_LAST] of TXB2PCFunc;
  DxbxMapActiveVersionToMostRecent: array [X_D3DRS_FIRST..X_D3DRS_LAST] of X_D3DRENDERSTATETYPE;
  DxbxMapMostRecentToActiveVersion: array [X_D3DRS_FIRST..X_D3DRS_LAST] of X_D3DRENDERSTATETYPE;

implementation

uses
  uEmuD3D8, // g_BuildVersion
  uPixelShader;

// Convert a 'method' DWORD into it's associated 'pixel-shader' or 'simple' render state.
function DxbxXboxMethodToRenderState(const aMethod: X_NV2AMETHOD): X_D3DRenderStateType; {NOPATCH}
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
    // Missing : $0000????: Result := X_D3DRS_PSTEXTUREMODES;
  else
    int(Result) := -1;
  end;
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
    if g_BuildVersion >= DxbxRenderStateInfo[State].V then
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

  // Initialize the dummy render state :
  DummyRenderStateValue := 0;
  XTL_EmuMappedD3DRenderState[X_D3DRS_UNSUPPORTED] := DummyRenderState;

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
    DxbxRenderStateXB2PCCallback[i] := TXB2PCFunc(DxbxXBTypeInfo[DxbxRenderStateInfo[i].T].F);

  // Build a table with converter functions for all texture stage states :
  for i := X_D3DTSS_FIRST to X_D3DTSS_LAST do
    DxbxTextureStageStateXB2PCCallback[i] := TXB2PCFunc(DxbxXBTypeInfo[DxbxTextureStageStateInfo[i].T].F);
end;

const
  fZero: Float = 0.0;
  fOne: Float = 1.0;

procedure DxbxInitializeDefaultRenderStates(const aParameters: PX_D3DPRESENT_PARAMETERS); {NOPATCH}
var
  f2dwZero: DWORD absolute fZero; // = F2DW(0.0)
  f2dwOne: DWORD absolute fOne; // = $3F800000 = F2DW(1.0)
var
  i: Integer;
begin
  // First, clear out all render states :
  for i := X_D3DRS_FIRST to X_D3DRS_LAST do
    XTL_EmuMappedD3DRenderState[i]^ := 0;

  // Now set the "deferred" states to 'unknown' :
  for i := X_D3DRS_DEFERRED_FIRST to X_D3DRS_DEFERRED_LAST do
    XTL_EmuMappedD3DRenderState[i]^ := X_D3DRS_UNK; // TODO : Explain why the default 0 causes dots in PointSprites sample!

  // Assign all Xbox default render states values :
  begin
    // Make up a few pixel shader colors here (TODO : we really should find&use the actual defaults!) :
    for i := X_D3DRS_PSCONSTANT0_0 to X_D3DRS_PSCONSTANT1_7 do
      XTL_EmuMappedD3DRenderState[i]^ := (X_D3DRS_PSCONSTANT1_7 - i) * $01010101;

    // The following default value assigments are derived from 5933 g_InitialRenderStates filling :
    XTL_EmuMappedD3DRenderState[X_D3DRS_ZFUNC]^ := DWORD(X_D3DCMP_LESSEQUAL);
    XTL_EmuMappedD3DRenderState[X_D3DRS_ALPHAFUNC]^ := DWORD(X_D3DCMP_ALWAYS);
    XTL_EmuMappedD3DRenderState[X_D3DRS_ALPHABLENDENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_ALPHATESTENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_ALPHAREF]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_SRCBLEND]^ := DWORD(X_D3DBLEND_ONE);
    XTL_EmuMappedD3DRenderState[X_D3DRS_DESTBLEND]^ := DWORD(X_D3DBLEND_ZERO);
    XTL_EmuMappedD3DRenderState[X_D3DRS_ZWRITEENABLE]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_DITHERENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_SHADEMODE]^ := DWORD(X_D3DSHADE_GOURAUD);
    XTL_EmuMappedD3DRenderState[X_D3DRS_COLORWRITEENABLE]^ := X_D3DCOLORWRITEENABLE_ALL;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILZFAIL]^ := DWORD(X_D3DSTENCILOP_KEEP);
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILPASS]^ := DWORD(X_D3DSTENCILOP_KEEP);
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILFUNC]^ := DWORD(X_D3DCMP_ALWAYS);
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILREF]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILMASK]^ := $FFFFFFFF;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILWRITEMASK]^ := $FFFFFFFF;
    XTL_EmuMappedD3DRenderState[X_D3DRS_BLENDOP]^ := DWORD(X_D3DBLENDOP_ADD);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BLENDCOLOR]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_SWATHWIDTH]^ := DWORD(X_D3DSWATH_128);
    XTL_EmuMappedD3DRenderState[X_D3DRS_POLYGONOFFSETZSLOPESCALE]^ := f2dwZero;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POLYGONOFFSETZOFFSET]^ := f2dwZero;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTOFFSETENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_WIREFRAMEOFFSETENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_SOLIDOFFSETENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_DEPTHCLIPCONTROL]^ := X_D3DDCC_CULLPRIMITIVE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STIPPLEENABLE]^ := BOOL_FALSE;

    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGTABLEMODE]^ := DWORD(X_D3DFOG_NONE);
    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGSTART]^ := f2dwZero;
    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGEND]^ := f2dwOne;
    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGDENSITY]^ := f2dwOne;
    XTL_EmuMappedD3DRenderState[X_D3DRS_RANGEFOGENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_WRAP0]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_WRAP1]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_WRAP2]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_WRAP3]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_LIGHTING]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_SPECULARENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_LOCALVIEWER]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_COLORVERTEX]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKSPECULARMATERIALSOURCE]^ := DWORD(X_D3DMCS_COLOR2);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKDIFFUSEMATERIALSOURCE]^ := DWORD(X_D3DMCS_COLOR1);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKAMBIENTMATERIALSOURCE]^ := DWORD(X_D3DMCS_MATERIAL);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKEMISSIVEMATERIALSOURCE]^ := DWORD(X_D3DMCS_MATERIAL);
    XTL_EmuMappedD3DRenderState[X_D3DRS_SPECULARMATERIALSOURCE]^ := DWORD(X_D3DMCS_COLOR2);
    XTL_EmuMappedD3DRenderState[X_D3DRS_DIFFUSEMATERIALSOURCE]^ := DWORD(X_D3DMCS_COLOR1);
    XTL_EmuMappedD3DRenderState[X_D3DRS_AMBIENTMATERIALSOURCE]^ := DWORD(X_D3DMCS_MATERIAL);
    XTL_EmuMappedD3DRenderState[X_D3DRS_EMISSIVEMATERIALSOURCE]^ := DWORD(X_D3DMCS_MATERIAL);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKAMBIENT]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_AMBIENT]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSIZE]^ := f2dwOne;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSIZE_MIN]^ := f2dwZero;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSPRITEENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSCALEENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSCALE_A]^ := f2dwOne;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSCALE_B]^ := f2dwZero;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSCALE_C]^ := f2dwZero;
    XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSIZE_MAX]^ := $42800000; // = F2DW(64.0) between D3DCAPS8.MaxPointSize and D3DRS_POINTSIZE_MIN (including)
    XTL_EmuMappedD3DRenderState[X_D3DRS_PATCHEDGESTYLE]^ := 0; // = D3DPATCHEDGE_DISCRETE
    XTL_EmuMappedD3DRenderState[X_D3DRS_PATCHSEGMENTS]^ := f2dwOne;
//    XTL_EmuMappedD3DRenderState[X_D3DRS_SWAPFILTER]^ := aParameters.MultiSampleType; // DEADBEEF
//    XTL_EmuMappedD3DRenderState[X_D3DRS_PRESENTATIONINTERVAL]^ := aParameters.FullScreen_PresentationInterval; // DEADBEEF

    // X_D3DRS_DEFERRED_UNUSED8 .. X_D3DRS_DEFERRED_UNUSED1 ?
    XTL_EmuMappedD3DRenderState[X_D3DRS_PSTEXTUREMODES]^ := 0;

    XTL_EmuMappedD3DRenderState[X_D3DRS_VERTEXBLEND]^ := DWORD(X_D3DVBF_DISABLE);
    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGCOLOR]^ := 0;

    XTL_EmuMappedD3DRenderState[X_D3DRS_FILLMODE]^ := DWORD(X_D3DFILL_SOLID);
    XTL_EmuMappedD3DRenderState[X_D3DRS_BACKFILLMODE]^ := DWORD(X_D3DFILL_SOLID);
    XTL_EmuMappedD3DRenderState[X_D3DRS_TWOSIDEDLIGHTING]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_NORMALIZENORMALS]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_ZENABLE]^ := DWORD(D3DZB_USEW);
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILFAIL]^ := DWORD(X_D3DSTENCILOP_KEEP);
    XTL_EmuMappedD3DRenderState[X_D3DRS_FRONTFACE]^ := DWORD(X_D3DFRONT_CW);
    XTL_EmuMappedD3DRenderState[X_D3DRS_CULLMODE]^ := DWORD(X_D3DCULL_CCW);
    XTL_EmuMappedD3DRenderState[X_D3DRS_TEXTUREFACTOR]^ := $FFFFFFFF;
    XTL_EmuMappedD3DRenderState[X_D3DRS_ZBIAS]^ := 0;
    XTL_EmuMappedD3DRenderState[X_D3DRS_LOGICOP]^ := DWORD(X_D3DLOGICOP_NONE);
    XTL_EmuMappedD3DRenderState[X_D3DRS_EDGEANTIALIAS]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_MULTISAMPLEANTIALIAS]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_MULTISAMPLEMASK]^ := $FFFFFFFF;
//    XTL_EmuMappedD3DRenderState[X_D3DRS_MULTISAMPLEMODE]^ := aParameters.MultiSampleType; // DEADBEEF
    XTL_EmuMappedD3DRenderState[X_D3DRS_MULTISAMPLERENDERTARGETMODE]^ := DWORD(X_D3DMULTISAMPLEMODE_1X);
    XTL_EmuMappedD3DRenderState[X_D3DRS_SHADOWFUNC]^ := DWORD(X_D3DCMP_NEVER);
    XTL_EmuMappedD3DRenderState[X_D3DRS_LINEWIDTH]^ := f2dwOne;
    XTL_EmuMappedD3DRenderState[X_D3DRS_SAMPLEALPHA]^ := 0; // Unknown default
    XTL_EmuMappedD3DRenderState[X_D3DRS_DXT1NOISEENABLE]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_YUVENABLE]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_OCCLUSIONCULLENABLE]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_STENCILCULLENABLE]^ := BOOL_TRUE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_ROPZCMPALWAYSREAD]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_ROPZREAD]^ := BOOL_FALSE;
    XTL_EmuMappedD3DRenderState[X_D3DRS_DONOTCULLUNCOMPRESSED]^ := BOOL_FALSE;

    if DxbxFix_HasZBuffer then
      XTL_EmuMappedD3DRenderState[X_D3DRS_ZENABLE]^ := D3DZB_TRUE
    else
      XTL_EmuMappedD3DRenderState[X_D3DRS_ZENABLE]^ := D3DZB_FALSE;
  end;
end;

// Converts the input render state from a version-dependent into a version-neutral value.
function DxbxVersionAdjust_D3DRS(const XboxRenderState_VersionDependent: X_D3DRENDERSTATETYPE): X_D3DRENDERSTATETYPE; {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := DxbxMapActiveVersionToMostRecent[XboxRenderState_VersionDependent];
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

function Dxbx_SetRenderState(const XboxRenderState: X_D3DRenderStateType; XboxValue: DWORD): DWORD; {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  PCRenderState: TD3DRenderStateType;
begin
  Result := XboxValue;

  // Check if the render state is mapped :
  if XTL_EmuMappedD3DRenderState[XboxRenderState] = DummyRenderState then
  begin
    DxbxKrnlCleanup('Unsupported RenderState : %s (0x%.08X)', [DxbxRenderStateInfo[XboxRenderState].S, int(XboxRenderState)]);
    Exit;
  end;

  // Set this value into the RenderState structure too (so other code will read the new current value) :
  XTL_EmuMappedD3DRenderState[XboxRenderState]^ := XboxValue;

// Disabled, as it messes up Nvidia rendering too much :
//  // Dxbx addition : Hack for Smashing drive (on ATI X1300), don't transfer fog (or everything becomes opaque) :
//  if  IsRunning(TITLEID_SmashingDrive)
//  and (XboxRenderState  in [X_D3DRS_FOGSTART, X_D3DRS_FOGEND, X_D3DRS_FOGDENSITY]) then
//    Exit;

  case XboxRenderState of
    // Pixel shader constants are handled in XTL_EmuUpdateActivePixelShader :
    X_D3DRS_PSCONSTANT0_0..X_D3DRS_PSCONSTANT1_7,
    X_D3DRS_PSFINALCOMBINERCONSTANT0, X_D3DRS_PSFINALCOMBINERCONSTANT1:
      Exit;

    X_D3DRS_DEFERRED_FIRST..X_D3DRS_DEFERRED_LAST:
    begin
      // Skip unspecified deferred render states :
      if (XboxValue = X_D3DTSS_UNK) then // TODO : These are no texture stage states, so X_D3DTSS_UNK is incorrect. Use D3DRS_UNSUPPORTED perhaps?
        Exit;
    end;

(*
    X_D3DRS_TEXTUREFACTOR:
    begin
      // TODO : If no pixel shader is set, initialize all 16 pixel shader constants
      //        (X_D3DRS_PSCONSTANT0_0..X_D3DRS_PSCONSTANT1_7) to this value too.
    end;

    X_D3DRS_CULLMODE:
    begin
      if XboxValue > DWORD(X_D3DCULL_NONE) then
        ; // TODO : Update X_D3DRS_FRONTFACE too

    end;
*)
    X_D3DRS_FILLMODE:
    begin
      // Configurable override on fillmode :
      case g_iWireframe of
        0: {Use fillmode specified by the XBE};
        1: XboxValue := DWORD(X_D3DFILL_WIREFRAME);
      else XboxValue := DWORD(X_D3DFILL_POINT);
      end;
    end;
  end;

  // Skip Xbox extensions :
  if DxbxRenderStateInfo[XboxRenderState].X then
    Exit;

  // Map the Xbox state to a PC state, and check if it's supported :
  PCRenderState := DxbxRenderStateInfo[XboxRenderState].PC;
  if PCRenderState = D3DRS_UNSUPPORTED then
  begin
    EmuWarning('%s is not supported!', [DxbxRenderStateInfo[XboxRenderState].S]);
    Exit;
  end;

  // Convert the value from Xbox format into PC format, and set it locally :
  {PCValue=}Result := DxbxRenderStateXB2PCCallback[XboxRenderState](XboxValue);

{$IFDEF DXBX_USE_D3D9}
  case XboxRenderState of
    X_D3DRS_EDGEANTIALIAS:
      ; // TODO -oDxbx : What can we do to support this?
    X_D3DRS_ZBIAS:
    begin
      // TODO -oDxbx : We need to calculate the sloped scale depth bias, here's what I know :
      // (see http://blog.csdn.net/qq283397319/archive/2009/02/14/3889014.aspx)
      //   bias = (max * D3DRS_SLOPESCALEDEPTHBIAS) + D3DRS_DEPTHBIAS (which is Value here)
      // > bias - Value = max * D3DRS_SLOPESCALEDEPTHBIAS
      // > D3DRS_SLOPESCALEDEPTHBIAS = (bias - Value) / max
      // TODO : So, what should we use as bias and max?
      g_pD3DDevice.SetRenderState(D3DRS_SLOPESCALEDEPTHBIAS, F2DW(1.0)); // For now.
      g_pD3DDevice.SetRenderState(D3DRS_DEPTHBIAS, Result);
    end;
  else
{$ELSE}
  begin
{$ENDIF}
    g_pD3DDevice.SetRenderState(PCRenderState, {PCValue=}Result);
  end;
end; // Dxbx_SetRenderState

function DxbxTransferRenderState(const XboxRenderState: X_D3DRENDERSTATETYPE): HResult;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  XboxValue: DWORD;
begin
  // Check if this render state is supported (so we don't trigger a warning) :
  if  (XTL_EmuMappedD3DRenderState[XboxRenderState] <> DummyRenderState)
  and (DxbxRenderStateInfo[XboxRenderState].PC <> D3DRS_UNSUPPORTED) then
  begin
    // Read the current Xbox value, and set it locally :
    XboxValue := XTL_EmuMappedD3DRenderState[XboxRenderState]^;
    // TODO : Prevent setting unchanged values
    Dxbx_SetRenderState(XboxRenderState, XboxValue);
  end;

  Result := D3D_OK;
end;

function DxbxTransferTextureStageState(Stage: int; State: X_D3DTEXTURESTAGESTATETYPE): HResult;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  XboxValue: DWORD;
  PCValue: DWORD;
begin
  Result := D3D_OK;

  // Skip Xbox extensions :
  if DxbxTextureStageStateInfo[State].X then
    // TODO -oDxbx : Emulate these Xbox extensions somehow
    Exit;

  XboxValue := XTL_EmuD3DDeferredTextureState[Stage, Ord(DxbxFromNewVersion_D3DTSS(State))];
  if (XboxValue = X_D3DTSS_UNK) then
    Exit;

  // Convert Xbox value to PC value for current texture stage state :
  PCValue := DxbxTextureStageStateXB2PCCallback[State](XboxValue);
  // TODO : Prevent setting unchanged values
  // Transfer over the deferred texture stage state to PC :
  IDirect3DDevice_SetTextureStageState(g_pD3DDevice, Stage, State, PCValue);
end;

procedure XTL_EmuUpdateDeferredStates(); {NOPATCH}
// Branch:shogun  Revision:163  Translator:PatrickvL  Done:100
var
  State: int;
  Stage: int;
  XboxValue: DWORD;
  PCValue: DWORD;
  pPCTexture: XTL_PIDirect3DBaseTexture8;
begin
  // Generic transfer of all Xbox deferred render states to PC :
  for State := X_D3DRS_DEFERRED_FIRST to X_D3DRS_DEFERRED_LAST do
    DxbxTransferRenderState(X_D3DRENDERSTATETYPE(State));

  // Certain D3DTS values need to be checked on each Draw[Indexed]^Vertices
  if (XTL_EmuD3DDeferredTextureState <> nil) then
  begin
    for Stage := 0 to X_D3DTS_STAGECOUNT-1 do
      for State := X_D3DTSS_DEFERRED_FIRST to X_D3DTSS_DEFERRED_LAST do
        DxbxTransferTextureStageState(Stage, State);

    // if point sprites are enabled, copy stage 3 over to 0
    if (XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSPRITEENABLE]^ = DWord(BOOL_TRUE)) then // Dxbx note : DWord cast to prevent warning
    begin
      // set the point sprites texture
      g_pD3DDevice.GetTexture(3, PIDirect3DBaseTexture(@pPCTexture));
      g_pD3DDevice.SetTexture(0, IDirect3DBaseTexture(pPCTexture));
      // TODO -oDXBX: Should we clear the pPCTexture interface (and how)?

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
end;

procedure DxbxGetFormatRelatedVariables(
  const pPixelContainer: PX_D3DPixelContainer;
  const X_Format: X_D3DFORMAT;
  var dwWidth: DWORD;
  var dwHeight: DWORD;
  var dwBPP: DWORD;
  var dwDepth: DWORD;
  var dwPitch: DWORD;
  var dwMipMapLevels: DWORD;
  var bSwizzled: BOOL_;
  var bCompressed: BOOL_;
  var dwCompressedSize: DWORD;
  var bCubeMap: BOOL_); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  v: uint32;
  MaxMipMapLevels: uint;
begin
  dwWidth := 1; dwHeight := 1; dwBPP := 1; dwDepth := 1; dwPitch := 0; dwMipMapLevels := 1;
  bSwizzled := FALSE; bCompressed := FALSE; dwCompressedSize := 0;
  bCubeMap := (pPixelContainer.Format and X_D3DFORMAT_CUBEMAP) > 0;

  // Interpret Width/Height/BPP
  bSwizzled := EmuXBFormatIsSwizzled(X_Format, @dwBPP);
  if bSwizzled then
  begin
    dwWidth := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
    dwHeight := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
    dwMipMapLevels := (pPixelContainer.Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
    dwDepth := 1;// HACK? 1 shl ((pPixelContainer.Format and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
    dwPitch := dwWidth * dwBPP;
  end
  else
  begin
    bCompressed := EmuXBFormatIsCompressed(X_Format);
    if bCompressed then
    begin
      dwWidth  := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
      dwHeight := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
      dwDepth  := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
      dwMipMapLevels := (pPixelContainer.Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;

      // D3DFMT_DXT2...D3DFMT_DXT5 : 128bits per block/per 16 texels
      dwCompressedSize := dwWidth * dwHeight;

      if (X_Format = X_D3DFMT_DXT1) then // 64bits per block/per 16 texels
        dwCompressedSize := dwCompressedSize div 2;

      dwBPP := 1;
    end
    else
    begin
      // The rest should be linear (this also includes the YUV formats) :
      if not EmuXBFormatIsLinear(X_Format, @dwBPP) then
        DxbxKrnlCleanup('0x%.08X is not a supported format!', [X_Format]);

      DxbxDecodeSizeIntoDimensions(pPixelContainer.Size, {out}dwWidth, {out}dwHeight, {out}dwPitch);
    end;
  end;

  // TODO -oDxbx : Cxbx doesn't do the following for X_D3DCOMMON_TYPE_SURFACE resources, so
  // we should determine if it harms us having this here, and make it conditional if needed:
  if (bSwizzled or bCompressed) then
  begin
    if dwMipMapLevels > 0 then // Dxbx addition, to prevent underflow
    for v := 0 to dwMipMapLevels - 1 do
    begin
      if (((1 shl v) >= dwWidth) or ((1 shl v) >= dwHeight)) then
      begin
        dwMipMapLevels := v + 1;
        break;
      end;
    end;
  end;

  // Dxbx addition : Limit Turok to 6 MipMap levels - anything more leads to crashes... but WHY?!?
  if IsRunning(TITLEID_TurokNTSC) then
    MaxMipMapLevels := 6
  else
    // On the other hand, Battlestar Galactica crashes if we limit to 6 (9 does the trick). Here also : but WHY?!?
    MaxMipMapLevels := 9;

  if (dwMipMapLevels > MaxMipMapLevels) then
  begin
    EmuWarning('Limited MipMapLevels to %d (instead of %d)', [MaxMipMapLevels, dwMipMapLevels]);
    dwMipMapLevels := MaxMipMapLevels;
  end;
end;

// Dxbx Note: This code is taken from XTL_EmuIDirect3DResource_Register and occured
// in XTL_EmuUpdateActiveTexture too, so it's generalize in this single implementation.
procedure DxbxUpdatePixelContainer(
  pPixelContainer: PX_D3DPixelContainer;
  dwCommonType: DWORD;

  dwWidth: DWORD;
  dwHeight: DWORD;
  dwBPP: DWORD;
  dwDepth: DWORD;
  dwPitch: DWORD;
  dwMipMapLevels: DWORD;
  bSwizzled: BOOL_;
  bCompressed: BOOL_;
  dwCompressedSize: DWORD;
  bCubeMap: BOOL_;
  CacheFormat: X_D3DFORMAT
  ); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  nrfaces: uint32;
  face: uint32;
  dwCompressedOffset: DWORD;
  dwMipOffs: DWORD;
  dwMipWidth: DWORD;
  dwMipHeight: DWORD;
  dwMipPitch: DWORD;
  level: uint;
  hRet: HRESULT;
  LockedRect: D3DLOCKED_RECT;
  iRect: TRect;
  iPoint: TPOINT;
  pSrc: PByte;
  pDest: PByte;

  v: uint32;

  // Palette conversion variables (expanding D3DFMT_P8 to D3DFMT_A8R8G8B8) :
  ConvertP8ToARGB: Boolean;
  NewTexture: XTL_PIDirect3DTexture8;
  OldTexture: XTL_PIDirect3DTexture8;
  dwDataSize: DWORD;
  pTextureCache: PBytes;
  src_yp: uint32;
  dst_yp: uint32;
  x: uint32;
  p: Byte;
begin
  pTextureCache := nil; // To prevent compiler warnings

  iRect := Classes.Rect(0, 0, 0, 0);
  iPoint := Classes.Point(0, 0);

  ConvertP8ToARGB := (CacheFormat = X_D3DFMT_P8);
  if ConvertP8ToARGB then
  begin
    if g_pCurrentPalette = nil then
    begin
      EmuWarning('Unsupported texture format D3DFMT_P8, no palette active to convert from!');
      Exit;
    end;

    EmuWarning('Unsupported texture format D3DFMT_P8, expanding to D3DFMT_A8R8G8B8');

    hRet := IDirect3DDevice_CreateTexture(g_pD3DDevice,
      dwWidth, dwHeight, dwMipMapLevels,
      {Usage=}0,//D3DUSAGE_RENDERTARGET,
      D3DFMT_A8R8G8B8,
      D3DPOOL_MANAGED, @NewTexture
    );
    if FAILED(hRet) then
      DxbxKrnlCleanup('Couldn''t create P8 replacement texture!');

    OldTexture := pPixelContainer.Emu.Texture;
    pPixelContainer.Emu.Texture := NewTexture;

    // Set new format in the resource header :
    pPixelContainer.Format := (pPixelContainer.Format and not X_D3DFORMAT_FORMAT_MASK)
                           or (X_D3DFMT_A8R8G8B8 shl X_D3DFORMAT_FORMAT_SHIFT);

    // Set new pitch in the resource header :
    pPixelContainer.Format := (pPixelContainer.Format and not X_D3DFORMAT_PSIZE_MASK)
                           or (Log2(dwPitch * SizeOf(D3DCOLOR)) shl X_D3DFORMAT_PSIZE_SHIFT);

    dwDataSize := dwPitch * dwHeight; // Number of P8 (byte sized) pixels
    pTextureCache := DxbxMalloc(dwDataSize);
  end;

  nrfaces := ifThen(bCubemap, 6, 1);
  for face := 0 to nrfaces - 1 do
  begin
    // as we iterate through mipmap levels, we'll adjust the source resource offset
    dwCompressedOffset := 0;

    dwMipOffs := 0;
    dwMipWidth := dwWidth;
    dwMipHeight := dwHeight;
    dwMipPitch := dwPitch;

    // iterate through the number of mipmap levels
    if dwMipMapLevels > 0 then // Dxbx addition, to prevent underflow
    for level := 0 to dwMipMapLevels - 1 do
    begin
      // Dxbx addition : Remove old lock(s) :
      DxbxUnlockD3DResource(pPixelContainer, level, face);

      // copy over data (deswizzle if necessary)
      if (dwCommonType = X_D3DCOMMON_TYPE_SURFACE) then
        hRet := IDirect3DSurface(pPixelContainer.Emu.Surface).LockRect(LockedRect, NULL, 0)
      else
      begin
        if (bCubemap) then
          hRet := IDirect3DCubeTexture(pPixelContainer.Emu.CubeTexture).LockRect(D3DCUBEMAP_FACES(face), 0, LockedRect, NULL, 0)
        else
          hRet := IDirect3DTexture(pPixelContainer.Emu.Texture).LockRect(level, {out}LockedRect, NULL, 0);
      end;

      // Dxbx addition : Mirror the behaviour in EmuUnswizzleActiveTexture :
      if hRet <> S_OK then
        Continue;

      pSrc := PBYTE(pPixelContainer.Data);
      pDest := LockedRect.pBits;
      if pSrc = pDest then
        Continue;

      if ConvertP8ToARGB then // Dxbx hack : Update Data member only when converting
        if (face = 0) and (level = 0) then
          pPixelContainer.Data := UIntPtr(pDest);

      if (IsSpecialResource(pPixelContainer.Data) and ((pPixelContainer.Data and X_D3DRESOURCE_DATA_FLAG_SURFACE) > 0)) then
      begin
        EmuWarning('Attempt to registered to another resource''s data (eww!)');

        // TODO -oCXBX: handle this horrible situation
        if dwMipHeight > 0 then // Dxbx addition, to prevent underflow
        for v := 0 to dwMipHeight - 1 do
        begin
          memset(pDest, 0, dwMipWidth * dwBPP);

          Inc(pDest, LockedRect.Pitch);
          // Inc(pSrc, dwMipPitch); // Dxbx note : Unused
        end;
      end
      else
      begin
        if (bSwizzled) or ConvertP8ToARGB then // Dxbx hack : Even convert when already unswizzled!
        begin
          if (DWORD(pSrc) = $80000000) then
          begin
            // TODO -oCXBX: Fix or handle this situation..?
          end
          else
          begin
            if ConvertP8ToARGB then
            begin
              // First we need to unswizzle the texture data to a temporary buffer :
              if bSwizzled then // Dxbx hack : Don't unswizzle twice!
                EmuUnswizzleRect(
                  pSrc + dwMipOffs, dwMipWidth, dwMipHeight, dwDepth, pTextureCache,
                  dwPitch, iRect, iPoint, dwBPP
                )
              else
                memcpy(pTextureCache, pSrc + dwMipOffs, dwMipWidth * dwMipHeight);

              // Lookup the colors of the paletted pixels in the current pallette
              // and write the expanded color back to the texture :
              src_yp := 0;
              dst_yp := 0;
              x := 0;
              dwDataSize := (dwMipWidth * dwMipHeight);
              while dwDataSize > 0 do
              begin
                Dec(dwDataSize);

                // Read P8 pixel :
                p := Byte(pTextureCache[src_yp + x]);
                // Read the corresponding ARGB from the palette and store it in the new texture :
                PDWORDs(pDest)[dst_yp + x] := PDWORDs(g_pCurrentPalette)[p];

                // Step to the next pixel, check if we've done one scanline :
                Inc(x);
                if (x = dwMipWidth) then
                begin
                  // Step to the start of the next scanline :
                  x := 0;
                  Inc(src_yp, dwMipPitch div SizeOf(Byte)); // src pitch is expressed in bytes
                  Inc(dst_yp, LockedRect.Pitch div SizeOf(D3DCOLOR)); // dst pitch is expressed in D3DCOLOR's
                end;
              end;
            end
            else
            begin
              EmuUnswizzleRect(
                pSrc + dwMipOffs, dwMipWidth, dwMipHeight, dwDepth, pDest,
                LockedRect.Pitch, iRect, iPoint, dwBPP
              );
            end;
          end;
        end
        else if (bCompressed) then
        begin
          // NOTE: compressed size is (dwWidth/2)*(dwHeight/2)/2, so each level divides by 4
          memcpy(pDest, pSrc + dwCompressedOffset, dwCompressedSize shr (level * 2));

          Inc(dwCompressedOffset, (dwCompressedSize shr (level * 2)));
        end
        else
        begin
          if (DWORD(LockedRect.Pitch) = dwMipPitch) and (dwMipPitch = dwMipWidth * dwBPP) then
          begin
            // TODO -oDxbx: This crashes on "minimario2ddemo", even though all arguments seem alright,
            // It seems that the unpatched function IDirect3DDevice_CreateSurface2 AND's the result
            // of MmAllocateContiguousMemoryEx with $0FFFFFFF which makes us loose the high nibble!!!
            memcpy(pDest, pSrc + dwMipOffs, dwMipPitch * dwMipHeight);
          end
          else
          begin
            if dwMipHeight > 0 then // Dxbx addition, to prevent underflow
            for v := 0 to dwMipHeight - 1 do
            begin
              memcpy(pDest, pSrc + dwMipOffs, dwMipWidth * dwBPP);

              Inc(pDest, LockedRect.Pitch);
              Inc(pSrc, dwMipPitch);
            end;
          end;
        end;


      end;

      DxbxUnlockD3DResource(pPixelContainer, level, face);

      Inc(dwMipOffs, dwMipPitch * dwMipHeight);

      dwMipWidth := dwMipWidth div 2;
      dwMipHeight := dwMipHeight div 2;
      dwMipPitch := dwMipPitch div 2;
    end;
  end;

  if ConvertP8ToARGB then
  begin
    // Flush unused data buffers
    DxbxFree(pTextureCache); // pTextureCache := nil;
    // Destroy old texture :
    for level := 0 to dwMipMapLevels - 1 do
      repeat until IDirect3DTexture(OldTexture).UnlockRect(level) <= D3D_OK;
    while IDirect3DTexture(OldTexture)._Release > 0 do ;
  end;
end;

//var g_EmuD3DConvertedTexture: array [0..X_D3DTS_STAGECOUNT-1] of PX_D3DBaseTexture; // = {0,0,0,0};

procedure XTL_EmuUpdateActiveTexture(); {NOPATCH}
// Branch:shogun  Revision:162  Translator:Shadow_Tj  Done:100
var
  Stage: int;
  pPixelContainer: PX_D3DPixelContainer;
  X_Format: X_D3DFORMAT;
  dwWidth: DWORD;
  dwHeight: DWORD;
  dwBPP: DWORD;
  dwDepth: DWORD;
  dwPitch: DWORD;
  dwMipMapLevels: DWORD;
  bSwizzled: BOOL_;
  bCompressed: BOOL_;
  dwCompressedSize: DWORD;
  bCubeMap: BOOL_;
begin
  for Stage := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    pPixelContainer := g_EmuD3DActiveTexture[Stage];
    if (pPixelContainer = NULL) then
      Continue;

    X_Format := X_D3DFORMAT((pPixelContainer.Format and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT);
    if X_Format = X_D3DFMT_P8 then // Dxbx note : For now, do only P8 conversions (later on others can be handled too)
    if (IDirect3DResource(pPixelContainer.Emu.Resource).GetType() = D3DRTYPE_TEXTURE) then
    begin
      DxbxGetFormatRelatedVariables(pPixelContainer, X_Format,
        {var}dwWidth, {var}dwHeight, {var}dwBPP, {var}dwDepth, {var}dwPitch, {var}dwMipMapLevels,
        {var}bSwizzled, {var}bCompressed, {var}dwCompressedSize, {var}bCubeMap);

      bSwizzled := False;

      DxbxUpdatePixelContainer(pPixelContainer, X_D3DCOMMON_TYPE_TEXTURE,
        dwWidth, dwHeight, dwBPP, dwDepth, dwPitch, dwMipMapLevels,
        bSwizzled, bCompressed, dwCompressedSize, bCubeMap, {CacheFormat=}X_Format);

      DxbxUnlockD3DResource(pPixelContainer); // Dxbx addition
      g_pD3DDevice.SetTexture(Stage, IDirect3DTexture(pPixelContainer.Emu.Texture));
    end;

  end;
end; // XTL_EmuUpdateActiveTexture

function RecompilePixelShader(pPSDef: PX_D3DPIXELSHADERDEF): PSH_RECOMPILED_SHADER;
const
  szDiffusePixelShader: P_char =
    'ps.1.0'#13#10 +
    'tex t0'#13#10 +
    'mov r0, t0'#13#10;
var
  ConvertedPixelShaderStr: AnsiString;
  hRet: DWORD;
  pShader: XTL_LPD3DXBUFFER;
  pErrors: XTL_LPD3DXBUFFER;
  pFunction: PDWORD;
begin
  // Attempt to recompile PixelShader
  Result := XTL_EmuRecompilePshDef(pPSDef);
  ConvertedPixelShaderStr := AnsiString(Result.NewShaderStr);

  // assemble the shader
  pShader := nil;
  pErrors := nil;
  hRet := D3DXAssembleShader(
    P_char(ConvertedPixelShaderStr),
    Length(ConvertedPixelShaderStr),
{$IFDEF DXBX_USE_D3D9}
    {pDefines=}nil,
    {pInclude=}nil,
    {Flags=}0,
{$ELSE}
    {Flags=}0,
    {ppConstants=}NULL,
{$ENDIF}
    {ppCompiledShader=}@pShader,
    {ppCompilationErrors}@pErrors);

  if pShader = nil then
  begin
    EmuWarning('Could not create pixel shader');
    EmuWarning(string(AnsiString(PAnsiChar(ID3DXBuffer(pErrors).GetBufferPointer)))); // Dxbx addition

    hRet := D3DXAssembleShader(
      szDiffusePixelShader,
      strlen(szDiffusePixelShader),
{$IFDEF DXBX_USE_D3D9}
      {pDefines=}nil,
      {pInclude=}nil,
      {Flags=}0,
{$ELSE}
      {Flags=}0,
      {ppConstants=}NULL,
{$ENDIF}
      {ppCompiledShader=}@pShader,
      {ppCompilationErrors}@pErrors);

    if pShader = nil then
      DxbxKrnlCleanup('Cannot fall back to the most simple pixel shader!');

    EmuWarning('We''re lying about the creation of a pixel shader!');
  end;

  if Assigned(pShader) then
  begin
    pFunction := PDWORD(ID3DXBuffer(pShader).GetBufferPointer());

    if (hRet = D3D_OK) then
      // redirect to windows d3d
      {hRet := }g_pD3DDevice.CreatePixelShader
      (
        pFunction,
{$IFDEF DXBX_USE_D3D9}
        PIDirect3DPixelShader9(@Result.ConvertedHandle) {$MESSAGE 'fixme'}
{$ELSE}
        {out}Result.ConvertedHandle
{$ENDIF}
      );

    // Dxbx note : We must release pShader here, else we would have a resource leak!
    ID3DXBuffer(pShader)._Release;
    pShader := nil;
  end;

  // Dxbx addition : We release pErrors here (or it would become a resource leak!)
  if Assigned(pErrors) then
  begin
    ID3DXBuffer(pErrors)._Release();
    pErrors := nil;
  end;
end;

var RecompiledShaders_Head: PPSH_RECOMPILED_SHADER = nil;

function XTL_EmuUpdateActivePixelShader(): HRESULT; {NOPATCH}
var
  pPSDef: PX_D3DPIXELSHADERDEF;
  RecompiledPixelShader: PPSH_RECOMPILED_SHADER;
  ConvertedPixelShaderHandle: DWORD;
  i: int;
  Register_: DWORD;
  dwColor: D3DCOLOR;
  fColor: D3DCOLORVALUE;
begin
  Result := D3D_OK;

  // Our SetPixelShader patch remembered the latest set pixel shader, see if it's assigned :
  if Assigned(g_EmuD3DActivePixelShader) then
  begin
    // We could read g_EmuD3DActivePixelShader.PshDef, but since this is copied into
    // D3D__RenderState (which contents might have been changed after the call to
    // SetPixelShader), we use the address of XTL_D3D__RenderState as the real pixel
    // shader definition :
    pPSDef := PX_D3DPIXELSHADERDEF(XTL_D3D__RenderState);
    if pPSDef = nil then
      // If we haven't found the symbol, then we can fall back to the given definition :
      pPSDef := g_EmuD3DActivePixelShader.PshDef;

    // Now, see if we already have a shader compiled for this declaration :
    RecompiledPixelShader := RecompiledShaders_Head;
    while Assigned(RecompiledPixelShader) do
    begin
      // Only compare parts that form a unique shader (ignore the constants and Direct3D8 run-time fields) :
      if  (memcmp(@(RecompiledPixelShader.PSDef.PSAlphaInputs[0]), @(pPSDef.PSAlphaInputs[0]), (8+2)*SizeOf(DWORD)) = 0)
      and (memcmp(@(RecompiledPixelShader.PSDef.PSAlphaOutputs[0]), @(pPSDef.PSAlphaOutputs[0]), (8+8+3+8+4)*SizeOf(DWORD)) = 0) then
        Break;

      RecompiledPixelShader := RecompiledPixelShader.Next;
    end;

    // If none was found, recompile this shader and remember it :
    if not Assigned(RecompiledPixelShader) then
    begin
      // Recompile this pixel shader :
      New(RecompiledPixelShader);
      RecompiledPixelShader^ := RecompilePixelShader(pPSDef);
      // Add this shader to the chain :
      RecompiledPixelShader.Next := RecompiledShaders_Head;
      RecompiledShaders_Head := RecompiledPixelShader;
    end;

    // Switch to the converted pixel shader :
    ConvertedPixelShaderHandle := RecompiledPixelShader.ConvertedHandle;

    // Set constants, not based on g_PixelShaderConstants, but based on
    // the render state slots containing the pixel shader constants,
    // as these could have been updated via SetRenderState or otherwise :
    for i := 0 to High(RecompiledPixelShader.ConstInUse) do
    begin
      if RecompiledPixelShader.ConstInUse[i] then
      begin
        // Read the register we can use on PC :
        Register_ := RecompiledPixelShader.ConstMapping[i];
        // Read the corresponding pixel shader slot :
        if i >= 16 then
          dwColor := XTL_EmuMappedD3DRenderState[X_D3DRS_PSFINALCOMBINERCONSTANT0 + i - 16]^
        else
          dwColor := XTL_EmuMappedD3DRenderState[X_D3DRS_PSCONSTANT0_0 + i]^;

        // Convert it back to 4 floats, so we can... :
        fColor := D3DXColorFromDWord(dwColor);
        // Set that value locally :
{$IFDEF DXBX_USE_D3D9}
        g_pD3DDevice.SetPixelShaderConstantF(Register_, PSingle(@fColor), 1);
{$ELSE}
        g_pD3DDevice.SetPixelShaderConstant(Register_, {untyped const}fColor, 1);
{$ENDIF}
      end;
    end;

  end
  else
    ConvertedPixelShaderHandle := 0;

  g_pD3DDevice.SetPixelShader({$IFDEF DXBX_USE_D3D9}IDirect3DPixelShader9{$ENDIF}(ConvertedPixelShaderHandle));

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

procedure XTL_EmuUpdateActiveVertexShader(); {NOPATCH}
begin
  // TODO : Move XTL_EmuD3DDevice_CreateVertexShader and related code over to here, so the patches can go
end;

procedure DxbxUpdateNativeD3DResources(); {NOPATCH}
begin
  XTL_EmuUpdateDeferredStates();
  XTL_EmuUpdateActivePixelShader();
  XTL_EmuUpdateActiveVertexShader();
  XTL_EmuUpdateActiveTexture();
end;

end.
