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
  Direct3D, // PD3DCOLOR
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
  uResourceTracker, // g_DataToTexture
  uEmu,
  uEmuAlloc,
  uEmuKrnlMM,
  uEmuD3D8Types,
  uEmuD3D8Utils,
  uNV2A;

function DxbxXboxMethodToString(const aMethod: X_NV2AMETHOD): string; {NOPATCH}
function DxbxXboxMethodToRenderState(const aMethod: X_NV2AMETHOD): X_D3DRenderStateType; {NOPATCH}

procedure DxbxBuildRenderStateMappingTable(const aD3DRenderState: PDWORDs); {NOPATCH}

function DxbxVersionAdjust_D3DRS(const XboxRenderState_VersionDependent: X_D3DRENDERSTATETYPE): X_D3DRENDERSTATETYPE; {NOPATCH}

function Dxbx_SetRenderState(const XboxRenderState: X_D3DRenderStateType; XboxValue: DWORD): DWORD; {NOPATCH}
procedure DxbxTransferRenderState(const XboxRenderState: X_D3DRENDERSTATETYPE);

function DxbxFromOldVersion_D3DTSS(const OldValue: X_D3DTEXTURESTAGESTATETYPE): X_D3DTEXTURESTAGESTATETYPE; {NOPATCH}
function DxbxFromNewVersion_D3DTSS(const NewValue: X_D3DTEXTURESTAGESTATETYPE): X_D3DTEXTURESTAGESTATETYPE; {NOPATCH}

function DxbxAssureNativeResource(pResource: PX_D3DResource; Usage: DWORD = 0): XTL_PIDirect3DResource8;

procedure DxbxPitchedCopy(pDest, pSrc: PByte; dwDestPitch, dwSrcPitch, dwWidthInBytes, dwHeight: int);

function DxbxXB2PC_D3DFormat(const X_Format: X_D3DFORMAT; const aResourceType: X_D3DRESOURCETYPE; var aUsage: DWORD): D3DFORMAT;

function DxbxUpdateNativePixelContainer(const pXboxBaseTexture: PX_D3DBaseTexture; Stage: int): XTL_PIDirect3DBaseTexture8;
procedure DxbxUpdateActiveVertexBufferStreams(); {NOPATCH}

procedure DxbxUpdateActiveRenderTarget(); {NOPATCH}
procedure DxbxRemoveIndexBuffer(Data: PWORD);
procedure DxbxUpdateActiveIndexBuffer(pwIndexData: PWORD; IndexCount: UINT; out StartIndex: UINT);

function DxbxUpdateNativeSurface(const pXboxSurface: PX_D3DSurface; D3DUsage: DWORD): XTL_PIDirect3DSurface8;

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

var XTL_D3D_CDevice_Init: PByte = nil;

var XTL_D3DDevice_SetRenderTarget: PByte = nil;

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

function DxbxXboxMethodToString(const aMethod: X_NV2AMETHOD): string; {NOPATCH}
begin
  Assert(aMethod and 3 = 0);

  if Integer(aMethod div 4) < Length(NV2AInfo) then
    Result := NV2AInfo[aMethod div 4].Name
  else
    Result := '';
end;

// Convert a 'method' DWORD into it's associated 'pixel-shader' or 'simple' render state.
function DxbxXboxMethodToRenderState(const aMethod: X_NV2AMETHOD): X_D3DRenderStateType; {NOPATCH}
begin
  // Dxbx note : Let the compiler sort this out, should be much quicker :
  case (aMethod and $00001ffc) of
    {00000100}NV2A_NOP: Result := X_D3DRS_PS_RESERVED; // XDK 3424 uses $00000100 (NOP), while 3911 onwards uses $00001d90 (SET_COLOR_CLEAR_VALUE)
    {00000260}NV2A_RC_IN_ALPHA__0: Result := X_D3DRS_PSALPHAINPUTS0;
    {00000264}NV2A_RC_IN_ALPHA__1: Result := X_D3DRS_PSALPHAINPUTS1;
    {00000268}NV2A_RC_IN_ALPHA__2: Result := X_D3DRS_PSALPHAINPUTS2;
    {0000026c}NV2A_RC_IN_ALPHA__3: Result := X_D3DRS_PSALPHAINPUTS3;
    {00000270}NV2A_RC_IN_ALPHA__4: Result := X_D3DRS_PSALPHAINPUTS4;
    {00000274}NV2A_RC_IN_ALPHA__5: Result := X_D3DRS_PSALPHAINPUTS5;
    {00000278}NV2A_RC_IN_ALPHA__6: Result := X_D3DRS_PSALPHAINPUTS6;
    {0000027c}NV2A_RC_IN_ALPHA__7: Result := X_D3DRS_PSALPHAINPUTS7;
    {00000288}NV2A_RC_FINAL0: Result := X_D3DRS_PSFINALCOMBINERINPUTSABCD;
    {0000028c}NV2A_RC_FINAL1: Result := X_D3DRS_PSFINALCOMBINERINPUTSEFG;
    {00000300}NV2A_ALPHA_FUNC_ENABLE: Result := X_D3DRS_ALPHATESTENABLE;
    {00000304}NV2A_BLEND_FUNC_ENABLE: Result := X_D3DRS_ALPHABLENDENABLE;
    {0000030C}NV2A_DEPTH_TEST_ENABLE: Result := X_D3DRS_ZENABLE;
    {00000310}NV2A_DITHER_ENABLE: Result := X_D3DRS_DITHERENABLE;
    {00000328}NV2A_SKIN_MODE: Result := X_D3DRS_VERTEXBLEND;
    {00000330}NV2A_POLYGON_OFFSET_POINT_ENABLE: Result := X_D3DRS_POINTOFFSETENABLE;
    {00000334}NV2A_POLYGON_OFFSET_LINE_ENABLE: Result := X_D3DRS_WIREFRAMEOFFSETENABLE;
    {00000338}NV2A_POLYGON_OFFSET_FILL_ENABLE: Result := X_D3DRS_SOLIDOFFSETENABLE;
    {0000033c}NV2A_ALPHA_FUNC_FUNC: Result := X_D3DRS_ALPHAFUNC;
    {00000340}NV2A_ALPHA_FUNC_REF: Result := X_D3DRS_ALPHAREF;
    {00000344}NV2A_BLEND_FUNC_SRC: Result := X_D3DRS_SRCBLEND;
    {00000348}NV2A_BLEND_FUNC_DST: Result := X_D3DRS_DESTBLEND;
    {0000034c}NV2A_BLEND_COLOR: Result := X_D3DRS_BLENDCOLOR;
    {00000350}NV2A_BLEND_EQUATION: Result := X_D3DRS_BLENDOP;
    {00000354}NV2A_DEPTH_FUNC: Result := X_D3DRS_ZFUNC;
    {00000358}NV2A_COLOR_MASK: Result := X_D3DRS_COLORWRITEENABLE;
    {0000035c}NV2A_DEPTH_WRITE_ENABLE: Result := X_D3DRS_ZWRITEENABLE;
    {00000360}NV2A_STENCIL_MASK: Result := X_D3DRS_STENCILWRITEMASK;
    {00000364}NV2A_STENCIL_FUNC_FUNC: Result := X_D3DRS_STENCILFUNC;
    {00000368}NV2A_STENCIL_FUNC_REF: Result := X_D3DRS_STENCILREF;
    {0000036c}NV2A_STENCIL_FUNC_MASK: Result := X_D3DRS_STENCILMASK;
    {00000374}NV2A_STENCIL_OP_ZFAIL: Result := X_D3DRS_STENCILZFAIL;
    {00000378}NV2A_STENCIL_OP_ZPASS: Result := X_D3DRS_STENCILPASS;
    {0000037c}NV2A_SHADE_MODEL: Result := X_D3DRS_SHADEMODE;
    {00000384}NV2A_POLYGON_OFFSET_FACTOR: Result := X_D3DRS_POLYGONOFFSETZSLOPESCALE;
    {00000388}NV2A_POLYGON_OFFSET_UNITS: Result := X_D3DRS_POLYGONOFFSETZOFFSET;
    {0000038c}NV2A_POLYGON_MODE_FRONT: Result := X_D3DRS_FILLMODE;
    {000003A0}NV2A_FRONT_FACE: Result := X_D3DRS_FRONTFACE;
    {000003A4}NV2A_NORMALIZE_ENABLE: Result := X_D3DRS_NORMALIZENORMALS;
    {000009f8}NV2A_SWATH_WIDTH: Result := X_D3DRS_SWATHWIDTH;
    {00000a60}NV2A_RC_CONSTANT_COLOR0__0: Result := X_D3DRS_PSCONSTANT0_0;
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
    {0000147c}NV2A_POLYGON_STIPPLE_ENABLE: Result := X_D3DRS_STIPPLEENABLE;
    {000017f8}NV2A_TX_SHADER_CULL_MODE: Result := X_D3DRS_PSCOMPAREMODE;
    {00001d78}NV2A_DEPTHCLIPCONTROL: Result := X_D3DRS_DEPTHCLIPCONTROL;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED1;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED2;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED3;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED4;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED5;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED6;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED7;
//    $00001d90: Result := X_D3DRS_SIMPLE_UNUSED8;
    {00001e20}NV2A_RC_COLOR0: Result := X_D3DRS_PSFINALCOMBINERCONSTANT0;
    {00001e24}NV2A_RC_COLOR1: Result := X_D3DRS_PSFINALCOMBINERCONSTANT1;
    $00001e40: Result := X_D3DRS_PSRGBOUTPUTS0;
    $00001e44: Result := X_D3DRS_PSRGBOUTPUTS1;
    $00001e48: Result := X_D3DRS_PSRGBOUTPUTS2;
    $00001e4c: Result := X_D3DRS_PSRGBOUTPUTS3;
    $00001e50: Result := X_D3DRS_PSRGBOUTPUTS4;
    $00001e54: Result := X_D3DRS_PSRGBOUTPUTS5;
    $00001e58: Result := X_D3DRS_PSRGBOUTPUTS6;
    $00001e5c: Result := X_D3DRS_PSRGBOUTPUTS7;
    {00001e60}NV2A_RC_ENABLE: Result := X_D3DRS_PSCOMBINERCOUNT;
    {00001e74}NV2A_TX_SHADER_DOTMAPPING: Result := X_D3DRS_PSDOTMAPPING;
    {00001e78}NV2A_TX_SHADER_PREVIOUS: Result := X_D3DRS_PSINPUTTEXTURE;
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

  if (Result > X_D3DTSS_LAST) then
    Result := X_D3DTSS_UNSUPPORTED;
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

  // Skip Xbox extensions :
  if DxbxRenderStateInfo[XboxRenderState].X then
    Exit;

// Disabled, as it messes up Nvidia rendering too much :
//  // Dxbx addition : Hack for Smashing drive (on ATI X1300), don't transfer fog (or everything becomes opaque) :
//  if  IsRunning(TITLEID_SmashingDrive)
//  and (XboxRenderState  in [X_D3DRS_FOGSTART, X_D3DRS_FOGEND, X_D3DRS_FOGDENSITY]) then
//    Exit;

  case XboxRenderState of
    // Pixel shader constants are handled in DxbxUpdateActivePixelShader :
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

var
  TransferAll: Boolean = True;
  TransferredValues: array [X_D3DRS_FIRST..X_D3DRS_LAST] of DWORD;

procedure DxbxTransferRenderState(const XboxRenderState: X_D3DRENDERSTATETYPE);
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
    // Prevent setting unchanged values :
    if TransferAll or (TransferredValues[XboxRenderState] <> XboxValue) then
    begin
      TransferredValues[XboxRenderState] := XboxValue;

      Dxbx_SetRenderState(XboxRenderState, XboxValue);
    end;
  end;
end;

function DxbxTransferTextureStageState(ReadStage, WriteStage: int; State: X_D3DTEXTURESTAGESTATETYPE): HResult;
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

  XboxValue := XTL_EmuD3DDeferredTextureState[ReadStage, Ord(DxbxFromNewVersion_D3DTSS(State))];
  if (XboxValue = X_D3DTSS_UNK) then
    Exit;

  // Convert Xbox value to PC value for current texture stage state :
  PCValue := DxbxTextureStageStateXB2PCCallback[State](XboxValue);
  // TODO : Prevent setting unchanged values
  // Transfer over the deferred texture stage state to PC :
  IDirect3DDevice_SetTextureStageState(g_pD3DDevice, WriteStage, State, PCValue);
end; // DxbxTransferTextureStageState

{.$define _DEBUG_DUMP_TEXTURE_SETTEXTURE}

procedure DxbxDumpResource(pPixelContainer: PX_D3DPixelContainer);
var
  FileName: AnsiString;
begin
(*
{$ifdef _DEBUG_DUMP_TEXTURE_SETTEXTURE}
  if (pTexture <> NULL) and (pTexture.Emu.Texture <> NULL) then
  begin
    // dwDumpTexture := 0; // Dxbx note : Do not reset 'static' var
    case (IDirect3DResource(pPixelContainer.Emu.Resource).GetType()) of
      D3DRTYPE_TEXTURE:
      begin
        sprintf(@szBuffer[0], 'SetTextureNorm - %.03d (0x%.08X).bmp', [dwDumpTexture, UIntPtr(pPixelContainer.Emu.Texture)]);
        Inc(dwDumpTexture);
        IDirect3DTexture(pPixelContainer.Emu.Texture).UnlockRect(0);

        D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, IDirect3DTexture(pPixelContainer.Emu.Texture), NULL);
      end;

      D3DRTYPE_CUBETEXTURE:
      begin
        face := 0;
        while face < 6 do
        begin
          Inc(dwDumpTexture);
          sprintf(@szBuffer[0], 'SetTextureCube%d - %.03d (0x%.08X).bmp', [face, dwDumpTexture, UIntPtr(pPixelContainer.Emu.Texture)]);
          IDirect3DTexture(pPixelContainer.Emu.CubeTexture).UnlockRect(face);
          D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, IDirect3DTexture(pPixelContainer.Emu.Texture), NULL);
          Inc(Face);
        end;
      end;
  end;
*)
  FileName := AnsiString(Format('D:\!tmp_texture_%0.8x.dds', [UIntPtr(pPixelContainer){, level, face}]));
  D3DXSaveTextureToFileA(PAnsiChar(FileName), D3DXIFF_DDS, IDirect3DTexture(pPixelContainer.Emu.Texture), NULL);
  // TODO : Dump all mipmap levels too (but how?!)
  // TODO : Depending type, switch to D3DXSaveSurfaceToFileA or D3DXSaveVolumeToFileA
end;

procedure DxbxPitchedCopy(pDest, pSrc: PByte; dwDestPitch, dwSrcPitch, dwWidthInBytes, dwHeight: int);
var
  v: uint32;
begin
  if (dwWidthInBytes = dwDestPitch) and (dwWidthInBytes = dwSrcPitch) then
  begin
    memcpy(pDest, pSrc, dwWidthInBytes * dwHeight);
  end
  else
  begin
    if dwHeight > 0 then // Dxbx addition, to prevent underflow
    for v := 0 to dwHeight - 1 do
    begin
      memcpy(pDest, pSrc, dwWidthInBytes);

      Inc(pDest, dwDestPitch);
      Inc(pSrc, dwSrcPitch);
    end;
  end;
end;

function DxbxLockD3DPixelContainer(
  const pPixelContainer: PX_D3DPixelContainer;
  out LockedBox: D3DLOCKED_BOX;
  level: int;
  face: int = Ord(D3DCUBEMAP_FACE_POSITIVE_X)): HRESULT;
var
  ResourceType: X_D3DRESOURCETYPE;
//  Box: D3DBOX;
  LockedRect: D3DLOCKED_RECT;
begin
  LockedBox.pBits := nil;
  ResourceType := DxbxGetResourceType(pPixelContainer);
//  Box.Left := 0;
//  Box.Top := 0;
//  Box.Right := 64;
//  Box.Bottom := 64;
//  Box.Front := face;
//  Box.Back := face;
  case ResourceType of
    X_D3DRTYPE_SURFACE:
      Result := IDirect3DSurface(pPixelContainer.Emu.Surface).LockRect(LockedRect, {pRect=}NULL, {Flags=}0);

    X_D3DRTYPE_VOLUME:
      // TODO -oDxbx : Totally untested! Volume's aren't used yet!
      Result := IDirect3DVolume(pPixelContainer.Emu.Volume).LockBox({out}LockedBox, {@Box=}NULL, {Flags=}0);

    X_D3DRTYPE_TEXTURE:
      Result := IDirect3DTexture(pPixelContainer.Emu.Texture).LockRect(level, {out}LockedRect, {pRect=}NULL, {Flags=}0);

    X_D3DRTYPE_VOLUMETEXTURE:
      // TODO -oDxbx : Totally untested! VolumeTexture's aren't used yet!?
      Result := IDirect3DVolumeTexture(pPixelContainer.Emu.VolumeTexture).LockBox(level, {out}LockedBox, {@Box=}NULL, {Flags=}0);

    X_D3DRTYPE_CUBETEXTURE:
      Result := IDirect3DCubeTexture(pPixelContainer.Emu.CubeTexture).LockRect(D3DCUBEMAP_FACES(face), level, LockedRect, {pRect=}NULL, {Flags=}0); // D3DLOCK_DISCARD ? (Needs dynamic texture)
  else
    Result := D3DERR_WRONGTEXTUREFORMAT;
  end;

  if FAILED(Result) then
    Exit;

  if LockedBox.pBits = nil then
  begin
    // Return pitch & bits for 2D pixel via a LockedBox, to allow easier usage by callers :
    {out}LockedBox.RowPitch := LockedRect.Pitch;
    {out}LockedBox.SlicePitch := 0; // 2D textures don't use slices
    {out}LockedBox.pBits := LockedRect.pBits;
  end;

  DbgPrintf('LOCKED %s Level=%d Face=%d pBits=%0.8x', [ResourceToString(pPixelContainer), level, face, UIntPtr(LockedBox.pBits)]);
end;

procedure DxbxDetermineSurFaceAndLevelByData(const DxbxPixelJar: RDxbxDecodedPixelContainer; out Level: int; out FaceType: D3DCUBEMAP_FACES);
var
  ParentData: UIntPtr;
  SurfaceData: UIntPtr;
begin
  ParentData := UIntPtr(DxbxGetDataFromXboxResource(PX_D3DSurface(DxbxPixelJar.pPixelContainer).Parent));
  SurfaceData := UIntPtr(DxbxGetDataFromXboxResource(DxbxPixelJar.pPixelContainer));

  // Step to the correct face :
  FaceType := D3DCUBEMAP_FACE_POSITIVE_X;
  while FaceType < D3DCUBEMAP_FACE_NEGATIVE_Z do
  begin
    if ParentData >= SurfaceData then
      Break;

    Inc(ParentData, DxbxPixelJar.dwFacePitch);
    Inc(FaceType);
  end;

  // Step to the correct mipmap level :
  Level := 0;
  while Level < X_MAX_MIPMAPS do
  begin
    if ParentData + DxbxPixelJar.MipMapOffsets[Level] >= SurfaceData then
      Break;

    Inc(Level);
  end;
end;

procedure DxbxAdjustTextureDimensions(ResourceType: D3DRESOURCETYPE; dwWidth: PUINT; dwHeight: PUINT);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  NeedsConversion: Boolean;
  NewWidth, NewHeight: UINT;
  v: int;
  mask: int;
begin
  NeedsConversion := False;
  case ResourceType of
    D3DRTYPE_SURFACE,
    D3DRTYPE_VOLUME,
    D3DRTYPE_TEXTURE:
      NeedsConversion := ((g_D3DCaps.TextureCaps and D3DPTEXTURECAPS_POW2) <> 0);
    D3DRTYPE_VOLUMETEXTURE:
//      NeedsConversion := ((g_D3DCaps.TextureCaps and D3DPTEXTURECAPS_VOLUMEMAP_POW2) <> 0);
      NeedsConversion := False; // No conversion, as Xbox1 never does anything else but power-of-two volumes!
    D3DRTYPE_CUBETEXTURE:
//      NeedsConversion := ((g_D3DCaps.TextureCaps and D3DPTEXTURECAPS_CUBEMAP_POW2) <> 0);
      NeedsConversion := False; // No conversion, as Xbox1 never does anything else but power-of-two cubemaps!
  end;

  if NeedsConversion then
  begin
    // Ensure a given width/height are powers of 2 :
    NewWidth := 0; NewHeight := 0;

    for v := 0 to 32-1 do
    begin
      mask := 1 shl v;

      if (dwWidth^ and mask) > 0 then
        NewWidth := mask;

      if (dwHeight^ and mask) > 0 then
        NewHeight := mask;
    end;

    if (dwWidth^ <> NewWidth) then
    begin
      NewWidth := NewWidth shl 1;
      EmuWarning('Needed to resize width (%d->%d)', [dwWidth^, NewWidth]);
    end;

    if (dwHeight^ <> NewHeight) then
    begin
      NewHeight := NewHeight shl 1;
      EmuWarning('Needed to resize height (%d->%d)', [dwHeight^, NewHeight]);
    end;

    dwWidth^ := NewWidth;
    dwHeight^ := NewHeight;
  end;

  // Dxbx addition : Ensure the square-requirement when needed :
  NeedsConversion := ((g_D3DCaps.TextureCaps and D3DPTEXTURECAPS_SQUAREONLY) <> 0);
  if NeedsConversion then
  begin
    // If squary textures are required, copy the longest dimension to the other :
    if dwWidth^ > dwHeight^ then
    begin
      EmuWarning('Needed to square height (%d->%d)', [dwHeight^, dwWidth^]);
      dwHeight^ := dwWidth^;
    end
    else
    begin
      EmuWarning('Needed to square width (%d->%d)', [dwWidth^, dwHeight^]);
      dwWidth^ := dwHeight^;
    end;
  end;
end; // DxbxAdjustTextureDimensions

function DxbxXB2PC_D3DFormat(const X_Format: X_D3DFORMAT; const aResourceType: X_D3DRESOURCETYPE; var aUsage: DWORD): D3DFORMAT;
var
  FirstResult: D3DFORMAT;
begin
  // Convert Format (Xbox->PC)
  Result := EmuXB2PC_D3DFormat(X_Format);
  FirstResult := Result;

  // Dxbx addition : : Check device caps :
  if g_pD3D.CheckDeviceFormat(
    g_EmuCDPD.CreationParameters.AdapterOrdinal,
    g_EmuCDPD.CreationParameters.DeviceType,
    g_EmuCDPD.NativePresentationParameters.BackBufferFormat,
    aUsage,
    _D3DRESOURCETYPE(aResourceType),
    Result) = D3D_OK then
      Exit;

  // TODO -oCXBX: HACK: Devices that don't support this should somehow emulate it!
  // TODO -oDxbx: Non-supported formats should be emulated in a generic way
  case Result of
    D3DFMT_P8:
      // Note : Allocate a BPP-identical format instead of P8 (which is nearly never supported natively),
      // so that we at least have a BPP-identical format. Conversion to RGB is done later,
      // in DxbxUpdateNativePixelContainer :
      Result := D3DFMT_L8;
    D3DFMT_D16:
      case aResourceType of
        X_D3DRTYPE_TEXTURE,
        X_D3DRTYPE_VOLUMETEXTURE,
        X_D3DRTYPE_CUBETEXTURE:
        begin
          if (aUsage and X_D3DUSAGE_DEPTHSTENCIL) > 0 then
          begin
            Result := D3DFMT_D16_LOCKABLE;
            // ATI Fix : Does this card support D16 DepthStencil textures ?
            if g_pD3D.CheckDeviceFormat(
              g_EmuCDPD.CreationParameters.AdapterOrdinal,
              g_EmuCDPD.CreationParameters.DeviceType,
              g_EmuCDPD.NativePresentationParameters.BackBufferFormat,
              aUsage,
              _D3DRESOURCETYPE(aResourceType),
              Result) <> D3D_OK then
            begin
              // If not, fall back to a format that's the same size and is most probably supported :
              Result := D3DFMT_R5G6B5; // Note : If used in shaders, this will give unpredictable channel mappings!

              // Since this cannot longer be created as a DepthStencil, reset the usage flag :
              {var}aUsage := D3DUSAGE_RENDERTARGET; // TODO : Why rendertarget instead? This asks for a testcase!
            end;
          end
          else
            Result := D3DFMT_R5G6B5; // also CheckDeviceMultiSampleType
        end;
        X_D3DRTYPE_SURFACE:
          Result := D3DFMT_D16_LOCKABLE;
      end;
    D3DFMT_D24S8:
      case aResourceType of
        X_D3DRTYPE_TEXTURE,
        X_D3DRTYPE_VOLUMETEXTURE,
        X_D3DRTYPE_CUBETEXTURE:
        begin
          Result := D3DFMT_A8R8G8B8;
          // Since this cannot longer be created as a DepthStencil, reset the usage flag :
          {var}aUsage := D3DUSAGE_RENDERTARGET;
        end;
      end;
    D3DFMT_V16U16:
      // This fixes NoSortAlphaBlend (after B button - z-replace) on nvidia:
      Result := D3DFMT_A8R8G8B8;

    D3DFMT_YUY2:
      Result := D3DFMT_V8U8; // Use another format that's also 16 bits wide
//      if aResourceType = X_D3DRTYPE_CUBETEXTURE then
//        DxbxKrnlCleanup('YUV not supported for cube textures');
  end;

  if FirstResult <> Result then
    EmuWarning('%s is an unsupported format for %s! Allocating %s', [
      X_D3DFORMAT2String(X_Format),
      X_D3DRESOURCETYPE2String(aResourceType),
      D3DFORMAT2String(Result)]);
end;

// Check if a native resource exists for the given xbox resource (if not, create one).
function DxbxAssureNativeResource(pResource: PX_D3DResource; Usage: DWORD = 0): XTL_PIDirect3DResource8;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  ResourceType: X_D3DRESOURCETYPE;
  dwSize: DWORD;
  Pool: D3DPOOL;
  DxbxPixelJar: RDxbxDecodedPixelContainer;
  PCParent: XTL_PIDirect3DBaseTexture8;
  PCFormat: D3DFORMAT;
  Level: int;
  FaceType: D3DCUBEMAP_FACES;
  DebugStr: string;
begin
  Result := nil;
  if (pResource = NULL) then
    Exit;

  Result := pResource.Emu.Resource;
  if Assigned(Result) then
    Exit;

  DbgPrintf('DxbxAssureNativeResource: %s D3DUSAGE=%s', [ResourceToString(pResource), DxbxD3DUsageToString(Usage)]);

  DebugStr := '';
  hRet := D3DERR_WRONGTEXTUREFORMAT;
  Pool := D3DPOOL_MANAGED;
  ResourceType := DxbxGetResourceType(pResource);
  case ResourceType of
    X_D3DRTYPE_VERTEXBUFFER:
    begin
      dwSize := EmuCheckAllocationSize(DxbxGetDataFromXboxResource(pResource), true);
      if (dwSize = DWORD(-1)) then
      begin
        EmuWarning('Vertex buffer allocation size unknown');
      end
      else
      begin
        DbgPrintf('DxbxAssureNativeResource: Vertex buffer bytes : %d', [dwSize]);
        hRet := g_pD3DDevice.CreateVertexBuffer(
          dwSize,
          {Usage=}0, // ignored according to Xbox docs
          {FVF=}0, // ignored according to Xbox docs
          Pool, // Note : If we don't supply D3DPOOL_MANAGED here, the PointSprites and Gamepad XDK samples crash!
          {ppVertexBuffer=}@(pResource.Emu.VertexBuffer) // IDirect3DVertexBuffer
          {$IFDEF DXBX_USE_D3D9}, {pSharedHandle=}NULL{$ENDIF}
        );
      end;
    end;

    X_D3DRTYPE_INDEXBUFFER:
    begin
      dwSize := EmuCheckAllocationSize(DxbxGetDataFromXboxResource(pResource), true);
      if (dwSize = DWORD(-1)) then
      begin
        // TODO -oCXBX: once this is known to be working, remove the warning
        EmuWarning('Index buffer allocation size unknown');

//          pResource.Emu.Lock := X_D3DRESOURCE_LOCK_FLAG_NOSIZE;

        // Cxbx has 'break'; Delphi can't do that, so we use an else-block
{$IFDEF GAME_HACKS_ENABLED}
        // Halo dwSize = 0x336;
{$ENDIF}
      end
      else
      begin
        DbgPrintf('DxbxAssureNativeResource: Index buffer bytes : %d (%d indices)', [dwSize, dwSize div SizeOf(Word)]);
        hRet := IDirect3DDevice_CreateIndexBuffer(g_pD3DDevice,
          dwSize, {Usage=}0, D3DFMT_INDEX16, D3DPOOL_MANAGED,
          @(pResource.Emu.IndexBuffer));
      end;
    end;

//    X_D3DRTYPE_VOLUME,
    X_D3DRTYPE_SURFACE,
    X_D3DRTYPE_CUBETEXTURE,
    X_D3DRTYPE_VOLUMETEXTURE,
    X_D3DRTYPE_TEXTURE:
    begin
      g_DataToTexture.insert(DWORD(pResource.Data), pResource);

      // Decode the texture dimensions and other attributes :
      DxbxGetFormatRelatedVariables(PX_D3DPixelContainer(pResource), {out}DxbxPixelJar);

      PCFormat := DxbxXB2PC_D3DFormat(DxbxPixelJar.X_Format, ResourceType, {var}Usage);

      // Adjust the pool if we're creating a render target or stencil buffer :
      if Usage <> 0 then
        Pool := D3DPOOL_DEFAULT; // Default D3DPOOL_MANAGED doesn't work, nor does D3DPOOL_SCRATCH or D3DPOOL_SYSTEMMEM

      // Depending it's attributes, create a CubeTexture, VolumeTexture or normal Texture :
      case ResourceType of
//        X_D3DRTYPE_VOLUME: // IDirect3DVolume
        X_D3DRTYPE_SURFACE:
        begin
          // Retrieve the PC resource that represents the parent of this surface,
          // including it's contents (updated if necessary) :
          // Samples like CubeMap show that render target formats must be applied to both surface and texture:
          PCParent := DxbxUpdateNativePixelContainer(PX_D3DSurface(pResource).Parent, {Stage=}-Usage); // Note: -1 Signals surface-usage
          // TODO : Do this when we don't need a content-copy (like when working inside Clear) :
          // PCParent := XTL_PIDirect3DBaseTexture8(DxbxAssureNativeResource(PX_D3DSurface(pResource).Parent, Usage));
          if Assigned(PCParent) then
          begin
            // Determine which face & mipmap level where used in the creation of this Xbox surface, using the Data pointer :
            DxbxDetermineSurFaceAndLevelByData(DxbxPixelJar, {out}Level, {out}FaceType);
            case DxbxGetResourceType(PX_D3DSurface(pResource).Parent) of
              X_D3DRTYPE_TEXTURE:
              begin
                hRet := IDirect3DTexture(PCParent).GetSurfaceLevel(
                  Level,
                  @(pResource.Emu.Surface) // IDirect3DSurface
                );
                int(FaceType) := -1;
              end;
              X_D3DRTYPE_CUBETEXTURE:
              begin
                hRet := IDirect3DCubeTexture(PCParent).GetCubeMapSurface(
                  FaceType,
                  Level,
                  @(pResource.Emu.Surface) // IDirect3DSurface
                );
                // PS: It's probably needed to patch up the destruction of this surface too!
                // PS2: We also need a mechanism to remove obsolete native resources,
                // like the native CubeMap texture that's used for this surfaces' parent.
              end;
            else
              DxbxD3DError('DxbxAssureNativeResource', 'Unhandled D3DSurface.Parent type!', pResource);
            end;

//            if not FAILED(hRet) then
//              DxbxUnlockD3DResource(PX_D3DSurface(pResource).Parent, Level, Ord(FaceType));
          end
          else
            // Differentiate between images & depth buffers :
            if EmuXBFormatIsRenderTarget(DxbxPixelJar.X_Format) then
            begin
              hRet := IDirect3DDevice_CreateImageSurface(g_pD3DDevice,
                DxbxPixelJar.dwWidth, DxbxPixelJar.dwHeight,
                PCFormat,
                @(pResource.Emu.Surface) // IDirect3DSurface
              );
            end
            else
              if EmuXBFormatIsDepthBuffer(DxbxPixelJar.X_Format) then
                hRet := IDirect3DDevice_CreateDepthStencilSurface(g_pD3DDevice,
                  DxbxPixelJar.dwWidth, DxbxPixelJar.dwHeight,
                  PCFormat,
                  {MultiSampleType=}D3DMULTISAMPLE_NONE, // TODO : Determine real MultiSampleType
                  @(pResource.Emu.Surface) // IDirect3DSurface
                )
              else
                DxbxD3DError('DxbxAssureNativeResource', 'Unhandled Surface format!', pResource);
        end;
        X_D3DRTYPE_CUBETEXTURE:
          hRet := IDirect3DDevice_CreateCubeTexture(g_pD3DDevice,
            {EdgeLength=}DxbxPixelJar.dwWidth,
            DxbxPixelJar.dwMipMapLevels,
            Usage,
            PCFormat,
            Pool, // Note : Cube textures created with D3DPOOL_DEFAULT are not lockable
            @(pResource.Emu.CubeTexture) // IDirect3DCubeTexture
          );
        X_D3DRTYPE_VOLUMETEXTURE:
          hRet := IDirect3DDevice_CreateVolumeTexture(g_pD3DDevice,
            DxbxPixelJar.dwWidth, DxbxPixelJar.dwHeight, DxbxPixelJar.dwDepth, DxbxPixelJar.dwMipMapLevels,
            Usage,  // TODO -oCXBX: Xbox Allows a border to be drawn (maybe hack this in software ;[)
            PCFormat,
            Pool,
            @(pResource.Emu.VolumeTexture) // IDirect3DVolumeTexture
          );
        X_D3DRTYPE_TEXTURE:
        begin
          hRet := IDirect3DDevice_CreateTexture(g_pD3DDevice,
            DxbxPixelJar.dwWidth, DxbxPixelJar.dwHeight, DxbxPixelJar.dwMipMapLevels,
            Usage,
            PCFormat,
            Pool,
            @(pResource.Emu.Texture) // IDirect3DTexture
          );
        end;
      end;
      DebugStr := ' PCFormat=' + D3DFormat2String(PCFormat);
    end;
//    X_D3DRTYPE_PUSHBUFFER: ; // ??
//    X_D3DRTYPE_FIXUP: ; // ??
//    X_D3DRTYPE_PALETTE: ; // ??
  end;

  DebugStr := Format('Usage=%s Pool=%s%s', [DxbxD3DUsageToString(Usage), DxbxD3DPoolToString(Pool), DebugStr]);
  if (FAILED(hRet)) then
    DxbxD3DError('DxbxAssureNativeResource', 'Could not create native resource! ' + DebugStr, pResource, hRet)
  else
    DbgPrintf('Created %s %s', [ResourceToString(pResource), DebugStr]);

  Result := pResource.Emu.Resource;
end; // DxbxAssureNativeResource

var
  pTmpBufferSize: uint = 0;
  pTmpBuffer: PBytes = nil;

function DxbxUpdateNativePixelContainer(const pXboxBaseTexture: PX_D3DBaseTexture; Stage: int): XTL_PIDirect3DBaseTexture8;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  MustCopyToNative: Boolean;
  D3DUsage: DWORD;
  nrfaces: uint32;
  face: uint32;
  nrslices: uint32;
  slice: uint32;
  dwMipWidth: DWORD;
  dwMipHeight: DWORD;
  dwMipPitch: DWORD;
  level: uint;
  hRet: HRESULT;
  LockedBox: D3DLOCKED_BOX;
  iRect: TRect;
  iPoint: TPOINT;
  pSrc: PByte;
  pDest: PByte;

  v: uint32;

  DxbxPixelJar: RDxbxDecodedPixelContainer;
  pPaletteColors: PD3DCOLOR;
  // Palette conversion variables (expanding D3DFMT_P8 to D3DFMT_A8R8G8B8) :
  ConvertP8ToARGB: Boolean;
  NewTexture: XTL_PIDirect3DTexture8;
  OldTexture: XTL_PIDirect3DTexture8;
  dwDataSize: DWORD;
  src_yp: uint32;
  dst_yp: uint32;
  x: uint32;
  p: Byte;
begin
  if pXboxBaseTexture = nil then
  begin
    Result := nil;
    Exit;
  end;

  // For now, only do an initial copy :
  MustCopyToNative := (pXboxBaseTexture.Emu.Resource = nil);
  // TODO : Later, we want to refresh the copy if needed (perhaps if a crc check fails)

  DxbxGetFormatRelatedVariables(pXboxBaseTexture, {out}DxbxPixelJar);

  // To support surfaces, check if we called this with a negative surface (and correct this) :
  if Stage < 0 then
  begin
    D3DUsage := -Stage;

    // Check if the usage indicates a rendertarget resource :
    if (D3DUsage and D3DUSAGE_RENDERTARGET) > 0 then
      // If however the format doesn't allow that sort of usage :
      if not EmuXBFormatIsRenderTarget(DxbxPixelJar.X_Format) then // TODO : Check this on PC format instead of Xbox!
        // Fur runs through Clear with this (no RENDERTARGET on surface.parent texture) :
        D3DUsage := D3DUsage and (not D3DUSAGE_RENDERTARGET);

    // Check if the usage indicates a depth stencil resource :
    if (D3DUsage and D3DUSAGE_DEPTHSTENCIL) > 0 then
      // If however the format doesn't allow that sort of usage :
      if not EmuXBFormatIsDepthBuffer(DxbxPixelJar.X_Format) then // TODO : Check this on PC format instead of Xbox!
      begin
        // Note : Samples like NoSortAlphaBlend use a depth stencil that's surface over a texture.
        // In that case, the surface must be marked as a depth stencil, but the underlying texture
        // must be created normally. This fixes NoSortAlphaBlend, ShadowBuffer and ZSprite samples
        // (no D3DUSAGE_DEPTHSTENCIL on surface.parent texture) :
        D3DUsage := D3DUsage and (not D3DUSAGE_DEPTHSTENCIL);
        // PS : XDK sample ZSprite fakes a D3DUSAGE_DEPTHSTENCIL by putting D3DFMT_LIN_D24S8 in the Resource.Format!
      end;

    Stage := 0; // Just a guess - what else could we use?
  end
  else
    D3DUsage := 0;

  // Make sure we have a native resource :
  Result := XTL_PIDirect3DBaseTexture8(DxbxAssureNativeResource(pXboxBaseTexture, D3DUsage));
  if not MustCopyToNative then
    Exit;

  // Make sure we do the following texture conversion with the palette from this stage :
  if Assigned(g_EmuD3DActivePalette[Stage]) then
    pPaletteColors := PD3DCOLOR(DxbxGetDataFromXboxResource(g_EmuD3DActivePalette[Stage]))
  else
    pPaletteColors := g_pCurrentPalette; // TODO : Remove this hack, in case we come here without an active palette (should normally be nil)

  iRect := Classes.Rect(0, 0, 0, 0);
  iPoint := Classes.Point(0, 0);

  ConvertP8ToARGB := (DxbxPixelJar.X_Format = X_D3DFMT_P8);
  if ConvertP8ToARGB then
  begin
    if pPaletteColors = nil then
    begin
      EmuWarning('Unsupported texture format D3DFMT_P8, no palette active to convert from!');
      Exit;
    end;

    EmuWarning('Unsupported texture format D3DFMT_P8, expanding to D3DFMT_A8R8G8B8');

    // Create a replacement RGB texture instead of the temporary L8 texture :
    hRet := IDirect3DDevice_CreateTexture(g_pD3DDevice,
      DxbxPixelJar.dwWidth, DxbxPixelJar.dwHeight, DxbxPixelJar.dwMipMapLevels,
      {Usage=}0, // Luckily, P8 cannot use the D3DUSAGE_RENDERTARGET flag!
      D3DFMT_A8R8G8B8,
      D3DPOOL_MANAGED, @NewTexture
    );
    if FAILED(hRet) then
      DxbxD3DError('DxbxUpdateNativePixelContainer', 'Couldn''t create P8 replacement texture!', pXboxBaseTexture, hRet);

    OldTexture := pXboxBaseTexture.Emu.Texture;
    pXboxBaseTexture.Emu.Texture := NewTexture;

    // Set new format in the resource header :
    pXboxBaseTexture.Format := (pXboxBaseTexture.Format and not X_D3DFORMAT_FORMAT_MASK)
                           or (X_D3DFMT_A8R8G8B8 shl X_D3DFORMAT_FORMAT_SHIFT);

    // Set new pitch in the resource header :
    pXboxBaseTexture.Format := (pXboxBaseTexture.Format and not X_D3DFORMAT_PSIZE_MASK)
                           or (Log2(DxbxPixelJar.dwRowPitch * SizeOf(D3DCOLOR)) shl X_D3DFORMAT_PSIZE_SHIFT);
  end;

  // Grow the temporary buffer when needed:
  dwDataSize := DxbxPixelJar.dwRowPitch * DxbxPixelJar.dwHeight * DxbxPixelJar.dwDepth; // Number bytes needed for input pixels
  if pTmpBufferSize < dwDataSize then
  begin
    pTmpBufferSize := dwDataSize;
    if pTmpBuffer <> nil then
      DxbxFree(pTmpBuffer);
    pTmpBuffer := DxbxMalloc(dwDataSize);
  end;

  // This outer loop walks over all faces (6 for CubeMaps, just 1 for anything else) :
  nrfaces := ifThen(DxbxPixelJar.bIsCubeMap, 6, 1);
  for face := 0 to nrfaces - 1 do
  begin
    dwMipWidth := DxbxPixelJar.dwWidth;
    dwMipHeight := DxbxPixelJar.dwHeight;
    dwMipPitch := DxbxPixelJar.dwRowPitch;

    // This 2nd loop iterates through all mipmap levels :
    if DxbxPixelJar.dwMipMapLevels > 0 then // Dxbx addition, to prevent underflow
    for level := 0 to DxbxPixelJar.dwMipMapLevels - 1 do
    begin
      // Copy over data (deswizzle if necessary)

      hRet := DxbxLockD3DPixelContainer(pXboxBaseTexture, {out}LockedBox, level, face);
      if (FAILED(hRet)) then
      begin
        DxbxD3DError('DxbxUpdateNativePixelContainer', 'Call to DxbxLockD3DPixelContainer failed!', pXboxBaseTexture, hRet, {bHalt=}False);
        Continue;
      end;

      // TODO -oCXBX: potentially CRC to see if this surface was actually modified..

      // This inner loop walks over all slices (1 for anything but 3D textures - those
      // use the correct amount for the current mipmap level, but never go below 1) :
      nrslices := DxbxPixelJar.MipMapSlices[level];
      for slice := 0 to nrslices - 1 do
      begin
        pDest := LockedBox.pBits;
        // Determine the Xbox data pointer and step it to the correct mipmap, face and/or slice :
        pSrc := DxbxGetDataFromXboxResource(pXboxBaseTexture);
        Inc(pSrc, DxbxPixelJar.MipMapOffsets[level]);
        if DxbxPixelJar.bIsCubeMap then
          Inc(pSrc, DxbxPixelJar.dwFacePitch * face)
        else
        begin
          if DxbxPixelJar.bIs3D then
          begin
            // Use the Xbox slice pitch (from the current level) to step to each slice in src :
            Inc(pSrc, DxbxPixelJar.SlicePitches[level] * slice);
            // Use the native SlicePitch (from the locked box) th step to eac slice in dest :
            Inc(pDest, uint(LockedBox.SlicePitch) * slice);
          end;
        end;

        if (DxbxPixelJar.bIsSwizzled) or ConvertP8ToARGB then // Dxbx hack : Even convert when already unswizzled!
        begin
          // TODO -oDxbx: This crashes on "minimario2ddemo", even though all arguments seem alright,
          // It seems that the unpatched function IDirect3DDevice_CreateSurface2 AND's the result
          // of MmAllocateContiguousMemoryEx with $0FFFFFFF which makes us loose the high nibble!!!

          if slice = 0 then
          begin
            // First we need to unswizzle the texture data to a temporary buffer :
            EmuUnswizzleRect(
              pSrc, dwMipWidth, dwMipHeight, DxbxPixelJar.dwDepth,
              pTmpBuffer, dwMipPitch,
              iRect, iPoint, DxbxPixelJar.dwBPP div 8
            );
          end;

          if ConvertP8ToARGB then
          begin
            Assert(DxbxPixelJar.bIsSwizzled, 'P8 not swizzled?');

            // Lookup the colors of the paletted pixels in the current pallette
            // and write the expanded color back to the texture :
            src_yp := 0;
            dst_yp := 0;
            x := 0;
//            dwDataSize := (dwMipWidth * dwMipHeight * DxbxPixelJar.dwBPP div 8);
            dwDataSize := (dwMipPitch * dwMipHeight);
            while dwDataSize > 0 do
            begin
              Dec(dwDataSize);

              // Read P8 pixel :
              p := Byte(pTmpBuffer[src_yp + x]);
              // Read the corresponding ARGB from the palette and store it in the new texture :
              PDWORDs(pDest)[dst_yp + x] := PDWORDs(pPaletteColors)[p];

              // Step to the next pixel, check if we've done one scanline :
              Inc(x);
              if (x = dwMipWidth) then
              begin
                // Step to the start of the next scanline :
                x := 0;
                Inc(src_yp, dwMipPitch div SizeOf(Byte)); // src pitch is expressed in bytes
                Inc(dst_yp, LockedBox.RowPitch div SizeOf(D3DCOLOR)); // dst pitch is expressed in D3DCOLOR's
              end;
            end;
          end
          else
          begin
            // Copy the unswizzled temporary data buffer back to the destination buffer :
            DxbxPitchedCopy(
              pDest,
              {pSrc=}PByte(@(pTmpBuffer[DxbxPixelJar.SlicePitches[level] * slice])),
              {DestPitch=}LockedBox.RowPitch, {SrcPitch=}dwMipPitch,
//              {WidthInBytes=}dwMipWidth * DxbxPixelJar.dwBPP div 8, dwMipHeight);
              {WidthInBytes=}dwMipPitch, dwMipHeight);
          end;
        end
        else if (DxbxPixelJar.bIsCompressed) then
        begin
          // NOTE: compressed size is (dwWidth/2)*(dwHeight/2)/2, so each level divides by 4
          memcpy(pDest, pSrc, DxbxPixelJar.MipMapOffsets[1] shr (level * 2));
        end
        else
          // Unmodified copy :
          DxbxPitchedCopy(
            pDest, pSrc,
            {DestPitch=}LockedBox.RowPitch, {SrcPitch=}dwMipPitch,
//            {WidthInBytes=}dwMipWidth * DxbxPixelJar.dwBPP div 8, dwMipHeight);
            {WidthInBytes=}dwMipPitch, dwMipHeight);

      end; // for slices

      DxbxUnlockD3DResource(pXboxBaseTexture, level, face);

      // Step to next mipmap level (but never go below minimum) :
      if dwMipWidth > DxbxPixelJar.dwMinXYValue then
      begin
        dwMipWidth := dwMipWidth div 2;
        dwMipPitch := dwMipPitch div 2;
      end;

      if dwMipHeight > DxbxPixelJar.dwMinXYValue then
        dwMipHeight := dwMipHeight div 2;
    end; // for mipmap levels
  end; // for faces

//DxbxDumpResource(pXboxBaseTexture);

  if ConvertP8ToARGB then
  begin
    // Destroy old texture :
    for level := 0 to DxbxPixelJar.dwMipMapLevels - 1 do
      repeat until IDirect3DTexture(OldTexture).UnlockRect(level) <= D3D_OK;
    while IDirect3DTexture(OldTexture)._Release > 0 do ;
  end;
end; // DxbxUpdateNativePixelContainer

function DxbxUpdateNativeVertexBuffer(const StreamNumber: Integer; var Stride: UINT): XTL_PIDirect3DVertexBuffer8;
var
  pXboxVertexBuffer: PX_D3DVertexBuffer;
  MustCopyToNative: Boolean;
  pData: PBYTE;
  pBase: PBYTE;
  dwSize: UINT;
  hRet: HRESULT;
  Info: RDxbxAllocationInfo;
begin
  pXboxVertexBuffer := g_EmuD3DActiveStreams[StreamNumber];
  if pXboxVertexBuffer = nil then
  begin
    Result := nil;
    Exit;
  end;

  // For now, always do a copy :
  MustCopyToNative := True;
//  MustCopyToNative := (pXboxVertexBuffer.Emu.Resource = nil);
  // TODO : Later, we want to refresh the copy only if needed (perhaps if a crc check fails)
  Result := XTL_PIDirect3DVertexBuffer8(DxbxAssureNativeResource(pXboxVertexBuffer));
  if Assigned(Result) and MustCopyToNative then
  begin
    pData := nil;
    hRet := IDirect3DVertexBuffer(Result).Lock(0, 0, {out}TLockData(pData), 0);
    if FAILED(hRet) then
      DxbxD3DError('DxbxUpdateNativeVertexBuffer', 'VertexBuffer Lock failed!', pXboxVertexBuffer, hRet);

    // Get the allocation information for this address :
    Info := DxbxGetAllocationInfo(pXboxVertexBuffer.Data);
    // Calculate the native address for this :
    pBase := Pointer(UIntPtr(Info.BaseAddress) + Info.Offset);
    // Calcalute how big this buffer can be maximally :
    dwSize := Info.AllocatedSize - Info.Offset;
    // Copy that :
    memcpy(pData, pBase, dwSize);
    // TODO : Instead creating a copy, we should probably do the vertex patching right here!

    IDirect3DVertexBuffer(Result).Unlock();

    g_EmuD3DActiveStreamSizes[StreamNumber] := dwSize div Stride;
  end;

end;

type
  PConvertedIndexBuffer = ^RConvertedIndexBuffer;
  RConvertedIndexBuffer = record
    Next: PConvertedIndexBuffer;
    IndexData: PWORD;
    IndexEnd: PWORD;
    pPCIndexBuffer: XTL_PIDirect3DIndexBuffer8;
  end;

var
  ConvertedIndexBuffers_Head: PConvertedIndexBuffer = nil;

// Remove all native index buffers that overlap the given memory address.
procedure DxbxRemoveIndexBuffer(Data: PWORD);

  function _Remove(Curr: PConvertedIndexBuffer): PConvertedIndexBuffer;
  begin
    Result := Curr.Next;
    DbgPrintf('DxbxRemoveIndexBuffer: Removing buffer for 0x%0.8x-0x%0.8x', [Curr.IndexData, Curr.IndexEnd]);
    IInterface(Curr.pPCIndexBuffer)._Release();
    DxbxFree(Curr);
  end;

var
  Prev: PConvertedIndexBuffer;
  Curr: PConvertedIndexBuffer;
begin
  // Loop over all cached index buffers :
  Prev := nil;
  Curr := ConvertedIndexBuffers_Head;
  while Assigned(Curr) do
  begin
    // See if it overlaps the given address :
    if (Curr.IndexData <= Data) and (Data < Curr.IndexEnd) then
    begin
      Curr := _Remove(Curr); // Curr is next or nil after this
      if Assigned(Prev) then
        Prev.Next := Curr // Skip removed entry
      else
        ConvertedIndexBuffers_Head := Curr; // Start after removed entry
    end
    else
    begin
      // Don't remove, step to next
      Prev := Curr;
      Curr := Curr.Next;
    end;
  end;
end;

procedure DxbxUpdateActiveIndexBuffer(pwIndexData: PWORD; IndexCount: UINT; out StartIndex: UINT);
var
  pwInitialStart: PWORD;
  pwIndexEnd: PWORD;
  MustCopy: Boolean;
  ConvertedIndexBuffer: PConvertedIndexBuffer;
  pData: PBYTE;
  hRet: HRESULT;
begin
  pwInitialStart := pwIndexData; // Remember this to calculate StartIndex later!
  pwIndexEnd := pwIndexData + IndexCount;
  MustCopy := False;

  // Note : We assume that all outdated index buffer(s) are already removed,
  // so that we'll never merge with buffers already destroyed on the Xbox side!

  // Now, see if we already have a (partial) index buffer for this range :
  ConvertedIndexBuffer := ConvertedIndexBuffers_Head;
  while Assigned(ConvertedIndexBuffer) do
  begin
    // Check if the given index range overlaps with this buffer :
    if (pwIndexData > ConvertedIndexBuffer.IndexEnd) then
      // new buffer starts beyond current (so no overlap)
    else if (pwIndexEnd < ConvertedIndexBuffer.IndexData) then
      // new buffer end before current (so no overlap)
    else
      // The new and current buffer overlap - we found a merge candidate!
      Break;

    ConvertedIndexBuffer := ConvertedIndexBuffer.Next;
  end;

  // Did we find a merge candidate?
  if ConvertedIndexBuffer = nil then
  begin
    DbgPrintf('DxbxUpdateActiveIndexBuffer: Creating new buffer for 0x%0.8x-0x%0.8x (%d indices)', [pwIndexData, pwIndexEnd, pwIndexEnd-pwIndexData]);

    // No merge, so add a new converted index buffer to the chain :
    ConvertedIndexBuffer := DxbxCalloc(SizeOf(RConvertedIndexBuffer), 1);
    ConvertedIndexBuffer.Next := ConvertedIndexBuffers_Head;
    ConvertedIndexBuffers_Head := ConvertedIndexBuffer;

    MustCopy := True;
  end
  else
  begin
    // We found an existing index buffer, see if we must extend it's bounds :

    if (pwIndexData < ConvertedIndexBuffer.IndexData) then
      MustCopy := True // The merge has a new start pointer
    else
      pwIndexData := ConvertedIndexBuffer.IndexData; // The merge keeps the old start pointer

    if (pwIndexEnd > ConvertedIndexBuffer.IndexEnd) then
      MustCopy := True // The merge has a new end pointer
    else
      pwIndexEnd := ConvertedIndexBuffer.IndexEnd; // The merge keeps the old end pointer

    // TODO : What if this grow causes two (or more) existing ranges to overlap - we should merge them all...
    // TOOD : What if the index buffer exceeds D3DCaps.MaxVertexIndex ?

    // TODO : If not MustCopy, Add a CRC check on the contents (forcing MustCopy) ?

    if MustCopy then
    begin
      DbgPrintf('DxbxUpdateActiveIndexBuffer: Enlarging buffer to 0x%0.8x-0x%0.8x (%d indices)', [pwIndexData, pwIndexEnd, pwIndexEnd-pwIndexData]);

      // Remove previous native buffer (this might leave one stale reference,
      // but this one will be released automatically in the next SetIndices call) :
      IDirect3DIndexBuffer(ConvertedIndexBuffer.pPCIndexBuffer)._Release;
      ConvertedIndexBuffer.pPCIndexBuffer := nil;
    end;
  end;

  if MustCopy then
  begin
    // Remember the (new) buffer bounds :
    ConvertedIndexBuffer.IndexData := pwIndexData;
    ConvertedIndexBuffer.IndexEnd := pwIndexEnd;
    IndexCount := pwIndexEnd - pwIndexData; // Calculate the number of WORD's between start & end

    // Create a new native index buffer of the above determined size :
    hRet := IDirect3DDevice_CreateIndexBuffer(g_pD3DDevice,
      IndexCount * SizeOf(Word),
      D3DUSAGE_WRITEONLY,
      D3DFMT_INDEX16,
      D3DPOOL_MANAGED,
      @ConvertedIndexBuffer.pPCIndexBuffer);
    if FAILED(hRet) then
      DxbxD3DError('DxbxUpdateActiveIndexBuffer', 'IndexBuffer Create Failed!', nil, hRet);

    // Copy the xbox indexes into this native buffer :
    pData := nil;
    IDirect3DIndexBuffer(ConvertedIndexBuffer.pPCIndexBuffer).Lock(0, 0, {out}TLockData(pData), 0);
    if (pData = nil) then
      DxbxD3DError('DxbxUpdateActiveIndexBuffer', 'Could not lock index buffer!');

    DbgPrintf('DxbxUpdateActiveIndexBuffer: Copying %d indices (D3DFMT_INDEX16)', [IndexCount]);
    memcpy({dest}pData, {src}pwIndexData, IndexCount * SizeOf(Word)); // TODO : Why does this crash at the 4th copy in Cartoon sample?

    IDirect3DIndexBuffer(ConvertedIndexBuffer.pPCIndexBuffer).Unlock();
  end;

  // Activate the new native index buffer :
  hRet := g_pD3DDevice.SetIndices(IDirect3DIndexBuffer(ConvertedIndexBuffer.pPCIndexBuffer){$IFNDEF DXBX_USE_D3D9}, {BaseVertexIndex=}0{$ENDIF});
  if (FAILED(hRet)) then
    DxbxKrnlCleanup('DxbxUpdateActiveIndexBuffer : SetIndices Failed!'#13#10 + DxbxD3DErrorString(hRet));

  // Make sure the caller knows what StartIndex it has to use to point to the indicated index start pointer :
  {out}StartIndex := pwInitialStart - ConvertedIndexBuffer.IndexData;
end; // DxbxUpdateActiveIndexBuffer

function DxbxUpdateNativeSurface(const pXboxSurface: PX_D3DSurface; D3DUsage: DWORD): XTL_PIDirect3DSurface8;
var
  MustCopyToNative: Boolean;
begin
  if pXboxSurface = nil then
  begin
    Result := nil;
    Exit;
  end;

  // For now, only do an initial copy :
  MustCopyToNative := (pXboxSurface.Emu.Resource = nil);
  // TODO : Later, we want to refresh the copy if needed (perhaps if a crc check fails)
  Result := XTL_PIDirect3DSurface8(DxbxAssureNativeResource(pXboxSurface, D3DUsage));
  if MustCopyToNative then
  begin
    // TODO : How to copy?
  end;
end;

function DxbxRecompilePixelShader(pPSDef: PX_D3DPIXELSHADERDEF): PSH_RECOMPILED_SHADER;
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
end; // DxbxRecompilePixelShader

var RecompiledShaders_Head: PPSH_RECOMPILED_SHADER = nil;

function DxbxUpdateActivePixelShader(): HRESULT; {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  pPSDef: PX_D3DPIXELSHADERDEF;
  RecompiledPixelShader: PPSH_RECOMPILED_SHADER;
  ConvertedPixelShaderHandle: DWORD;
  CurrentPixelShader: DWORD;
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
      RecompiledPixelShader^ := DxbxRecompilePixelShader(pPSDef);
      // Add this shader to the chain :
      RecompiledPixelShader.Next := RecompiledShaders_Head;
      RecompiledShaders_Head := RecompiledPixelShader;
    end;

    // Switch to the converted pixel shader (if it's any different from our currently active
    // pixel shader, to avoid many unnecessary state changes on the local side).
    ConvertedPixelShaderHandle := RecompiledPixelShader.ConvertedHandle;

{$IFDEF DXBX_USE_D3D9}
    g_pD3DDevice.GetPixelShader({out}PIDirect3DPixelShader9(@CurrentPixelShader));
{$ELSE}
    g_pD3DDevice.GetPixelShader({out}CurrentPixelShader);
{$ENDIF}
    if CurrentPixelShader <> ConvertedPixelShaderHandle then
      g_pD3DDevice.SetPixelShader({$IFDEF DXBX_USE_D3D9}IDirect3DPixelShader9{$ENDIF}(ConvertedPixelShaderHandle));

    // Note : We set the constants /after/ setting the shader, so that any
    // constants in the shader declaration can be overwritten (this will be
    // needed for the final combiner constants at least)!

(* This must be done once we somehow forward the vertex-shader oFog output to the pixel shader FOG input register :
   We could use the unused oT4.x to output fog from the vertex shader, and read it with 'texcoord t4' in pixel shader!
    // Disable native fog if pixel shader is said to handle it :
    if (RecompiledPixelShader.PSDef.PSFinalCombinerInputsABCD > 0)
    or (RecompiledPixelShader.PSDef.PSFinalCombinerInputsEFG > 0) then
    begin
      g_pD3DDevice.SetRenderState(D3DRS_FOGENABLE, BOOL_FALSE);
    end;
*)
    // Set constants, not based on g_PixelShaderConstants, but based on
    // the render state slots containing the pixel shader constants,
    // as these could have been updated via SetRenderState or otherwise :
    for i := 0 to High(RecompiledPixelShader.ConstInUse) do
    begin
      if RecompiledPixelShader.ConstInUse[i] then
      begin
        // Read the color from the corresponding render state slot :
        case i of
          PSH_XBOX_CONSTANT_FOG:
            dwColor := XTL_EmuMappedD3DRenderState[X_D3DRS_FOGCOLOR]^ or $FF000000;
            // Note : FOG.RGB is correct like this, but FOG.a should be coming
            // from the vertex shader (oFog) - however, D3D8 does not forward this...
          PSH_XBOX_CONSTANT_FC0:
            dwColor := XTL_EmuMappedD3DRenderState[X_D3DRS_PSFINALCOMBINERCONSTANT0]^;
          PSH_XBOX_CONSTANT_FC1:
            dwColor := XTL_EmuMappedD3DRenderState[X_D3DRS_PSFINALCOMBINERCONSTANT1]^;
        else
            dwColor := XTL_EmuMappedD3DRenderState[X_D3DRS_PSCONSTANT0_0 + i]^;
        end;

        // Convert it back to 4 floats  :
        fColor := D3DXColorFromDWord(dwColor);
        // Read the register we can use on PC :
        Register_ := RecompiledPixelShader.ConstMapping[i];
        // TODO : Avoid the following setter if it's no different from the previous update (this might speed things up)
        // Set the value locally in this register :
{$IFDEF DXBX_USE_D3D9}
        g_pD3DDevice.SetPixelShaderConstantF(Register_, PSingle(@fColor), 1);
{$ELSE}
        g_pD3DDevice.SetPixelShaderConstant(Register_, {untyped const}fColor, 1);
{$ENDIF}
      end;
    end;
  end
  else
  begin
    ConvertedPixelShaderHandle := 0;
    g_pD3DDevice.SetPixelShader({$IFDEF DXBX_USE_D3D9}IDirect3DPixelShader9{$ENDIF}(ConvertedPixelShaderHandle));
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
    for v:=0 to X_D3DTS_STAGECOUNT-1 do
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

procedure DxbxUpdateActiveVertexShader(); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  // TODO : Move XTL_EmuD3DDevice_CreateVertexShader and related code over to here, so the patches can go
end;

procedure DxbxUpdateActiveTextures(); {NOPATCH}
// Branch:Dxbx  Translator:Patrick  Done:100
var
  Stage: int;
  pPCTexture: XTL_PIDirect3DBaseTexture8;
  hRet: HRESULT;
begin
  for Stage := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    // Create textures dynamically :
    pPCTexture := DxbxUpdateNativePixelContainer(g_EmuD3DActiveTexture[Stage], Stage);
    hRet := g_pD3DDevice.SetTexture(Stage, IDirect3DTexture(pPCTexture));
    if FAILED(hRet) then
      DxbxD3DError('DxbxUpdateActiveTextures', 'SetTexture failed!', g_EmuD3DActiveTexture[Stage], hRet);
  end; // for Stage
end; // DxbxUpdateActiveTextures

procedure DxbxTransferTextureStage(ReadStage, WriteStage: int; StateFrom: int = X_D3DTSS_DEFERRED_FIRST; StateTo: int = X_D3DTSS_DEFERRED_LAST);
var
  State: int;
begin
  for State := StateFrom to StateTo do
    DxbxTransferTextureStageState(ReadStage, WriteStage, State);
end;

procedure DxbxUpdateDeferredStates(); {NOPATCH}
// Branch:shogun  Revision:163  Translator:PatrickvL  Done:100
var
  State: int;
  pPCTexture: XTL_PIDirect3DBaseTexture8;
begin
  // Generic transfer of all Xbox deferred render states to PC :
  for State := X_D3DRS_DEFERRED_FIRST to X_D3DRS_DEFERRED_LAST do
    DxbxTransferRenderState(X_D3DRENDERSTATETYPE(State));

  TransferAll := False; // TODO : When do we need to reset this to True?

  // Certain D3DTS values need to be checked on each Draw[Indexed]^Vertices
  if (XTL_EmuD3DDeferredTextureState = nil) then
    Exit;

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

    // Copy over stage 3 to 0 (ignoring stage 3):
    DxbxTransferTextureStage(3, 0, X_D3DTSS_FIRST, X_D3DTSS_LAST);
  end
  else
  begin
    // Copy stage 0 and 3 as-is :
    DxbxTransferTextureStage(0, 0);
    DxbxTransferTextureStage(3, 3);
  end;

  // Handle stage 1 and 2 too :
  DxbxTransferTextureStage(1, 1);
  DxbxTransferTextureStage(2, 2);

  // Dxbx note : If we don't transfer the stages in this order, many XDK samples
  // either don't show the controller image in their help screen, or (like in
  // the 'Tiling' and 'BeginPush' XDK samples) colors & textures are wrong!

end; // DxbxUpdateDeferredStates

procedure DxbxUpdateActiveVertexBufferStreams(); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  StreamNumber: Integer;
  Stride: UINT;
  pPCVertexBuffer: XTL_PIDirect3DVertexBuffer8;
  hRet: HRESULT;
begin
  for StreamNumber := 0 to MAX_NBR_STREAMS - 1 do
  begin
    Stride := g_EmuD3DActiveStreamStrides[StreamNumber];
    pPCVertexBuffer := DxbxUpdateNativeVertexBuffer(StreamNumber, {var}Stride);
    hRet := g_pD3DDevice.SetStreamSource(StreamNumber, IDirect3DVertexBuffer(pPCVertexBuffer), {$IFDEF DXBX_USE_D3D9}{OffsetInBytes=}0, {$ENDIF}Stride);
    if FAILED(hRet) then
      DxbxD3DError('DxbxUpdateActiveVertexBufferStreams', 'SetStreamSource failed!', g_EmuD3DActiveStreams[StreamNumber], hRet);
  end; // for Streams
end; // DxbxUpdateActiveVertexBufferStreams

procedure DxbxUpdateActiveRenderTarget(); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  pPCRenderTarget: XTL_PIDirect3DSurface8;
  pPCDepthStencil: XTL_PIDirect3DSurface8;
  hRet: HRESULT;
begin
  // TODO : Should we use the defaults when one of these is nil?
  pPCRenderTarget := DxbxUpdateNativeSurface(g_EmuD3DActiveRenderTarget, D3DUSAGE_RENDERTARGET);
  pPCDepthStencil := DxbxUpdateNativeSurface(g_EmuD3DActiveDepthStencil, D3DUSAGE_DEPTHSTENCIL);

{$IFDEF DXBX_USE_D3D9}
  hRet := g_pD3DDevice.SetRenderTarget({RenderTargetIndex=}0, IDirect3DSurface(pPCRenderTarget));
  g_pD3DDevice.SetDepthStencilSurface(IDirect3DSurface9(pPCDepthStencil));
{$ELSE}
  hRet := g_pD3DDevice.SetRenderTarget(IDirect3DSurface(pPCRenderTarget), IDirect3DSurface(pPCDepthStencil));
  // This tries to fix VolumeFog on ATI :
  if FAILED(hRet) then
  begin
    // TODO : Maybe some info : http://forums.create.msdn.com/forums/t/2124.aspx
    EmuWarning('SetRenderTarget failed! Trying ATI fix'#13#10 + DxbxD3DErrorString(hRet));
    hRet := g_pD3DDevice.SetRenderTarget(IDirect3DSurface(pPCRenderTarget), nil);
  end;
{$ENDIF}
  if FAILED(hRet) then
    EmuWarning('SetRenderTarget failed!'#13#10 + DxbxD3DErrorString(hRet));
end;

procedure DxbxUpdateNativeD3DResources(); {NOPATCH}
begin
  DxbxUpdateActiveVertexShader();
  DxbxUpdateActiveTextures();
  DxbxUpdateActivePixelShader();
  DxbxUpdateDeferredStates(); // BeginPush sample shows us that this must come *after* texture update!
  DxbxUpdateActiveVertexBufferStreams();
  DxbxUpdateActiveRenderTarget();
end;

end.

