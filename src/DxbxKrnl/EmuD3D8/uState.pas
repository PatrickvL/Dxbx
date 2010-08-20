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

function EmuXB2PC_D3DRS(Value: X_D3DRENDERSTATETYPE): D3DRENDERSTATETYPE;

procedure DxbxBuildRenderStateMappingTable(const aD3DRenderState: PDWORDs); {NOPATCH}
procedure DxbxInitializeDefaultRenderStates(const aParameters: PX_D3DPRESENT_PARAMETERS); {NOPATCH}

function DxbxVersionAdjust_D3DRS(const Value: X_D3DRENDERSTATETYPE): X_D3DRENDERSTATETYPE; {NOPATCH}
function DxbxVersionAdjust_D3DTSS(const NewValue: DWORD): DWORD; {NOPATCH}
function DxbxTransferRenderState(const aD3DRenderState: X_D3DRENDERSTATETYPE): HResult;

procedure XTL_EmuUpdateDeferredStates(); {NOPATCH}

// XDK version independent renderstate table, containing pointers to the original locations.
var XTL_EmuMappedD3DRenderState: array [X_D3DRS_FIRST..X_D3DRS_LAST+1] of PDWORD; // +1 for the unsupported value

// Dxbx addition : Dummy value (and pointer to that) to transparently ignore unsupported render states :
const X_D3DRS_UNSUPPORTED = X_D3DRS_LAST+1;
var DummyRenderState: PDWORD = @(XTL_EmuMappedD3DRenderState[X_D3DRS_UNSUPPORTED]); // Unsupported states share this pointer value

// Texture state lookup table
var XTL_EmuD3DDeferredTextureState: PDWORDs;

const DEFAULT_XDK_VERSION = 4627; // TODO -oDxbx : Make this configurable
var g_BuildVersion: uint32 = DEFAULT_XDK_VERSION;
// var g_OrigBuildVersion: uint32; // Dxbx note : Unused



implementation

uses
  uEmuD3D8; // g_BuildVersion
type
  TXB2PCFunc = function(Value: DWORD): DWORD;

var
  DxbxMapActiveVersionToMostRecent: array [X_D3DRS_FIRST..X_D3DRS_LAST] of X_D3DRENDERSTATETYPE;
  DxbxMapMostRecentToActiveVersion: array [X_D3DRS_FIRST..X_D3DRS_LAST] of X_D3DRENDERSTATETYPE;
  DxbxRenderStateXB2PCCallback: array [X_D3DRS_FIRST..X_D3DRS_LAST] of TXB2PCFunc;

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
    X_D3DRS_ZBIAS                       : Result := D3DRS_ZBIAS;
    X_D3DRS_EDGEANTIALIAS               : Result := D3DRS_EDGEANTIALIAS;
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
*)
  else
    Result := D3DRS_FORCE_DWORD; // Unsupported
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
      // When unavailable, apply a dummy pointer, and *don't* increate the version dependent state,
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
    XTL_EmuMappedD3DRenderState[i]^ := X_D3DRS_UNK;

  // Assign all Xbox default render states values :
  begin
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
//    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGSTART]^ := F2DW(0.0); // Unknown default
//    XTL_EmuMappedD3DRenderState[X_D3DRS_FOGEND]^ := F2DW(1.0); // Unknown default
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
begin
  Result := DxbxMapActiveVersionToMostRecent[Value];
end;

// For 3925, the actual D3DTSS flags have different values.
// This function maps new indexes to old ones, so that we
// can read a specific member from the emulated XBE's
// XTL_EmuD3DDeferredTextureState buffer.
function DxbxVersionAdjust_D3DTSS(const NewValue: DWORD): DWORD;
const
  OLD_X_D3DTSS_COLOROP = 0;
  OLD_X_D3DTSS_TEXTURETRANSFORMFLAGS = 9;
  OLD_X_D3DTSS_ADDRESSU = 10;
  OLD_X_D3DTSS_ALPHAKILL = 21;
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

function DxbxTransferRenderState(const aD3DRenderState: X_D3DRENDERSTATETYPE): HResult;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  PCRenderState: D3DRENDERSTATETYPE;
  XboxValue: DWORD;
  PCValue: DWORD;
begin
  // Check if the render state is mapped :
  if XTL_EmuMappedD3DRenderState[aD3DRenderState] <> DummyRenderState then
  begin
    // Read the current Xbox value, and check if it's assigned :
    XboxValue := XTL_EmuMappedD3DRenderState[aD3DRenderState]^;
    if XboxValue <> X_D3DRS_UNK then
    begin
      // Map the Xbox state to a PC state, and check if it's supported :
      PCRenderState := EmuXB2PC_D3DRS(aD3DRenderState); // TODO : Speed this up using a lookup table
      if PCRenderState <> D3DRS_FORCE_DWORD then
      begin
        // Convert the value from Xbox format into PC format, and set it locally :
        PCValue := DxbxRenderStateXB2PCCallback[aD3DRenderState](XboxValue);
        Result := g_pD3DDevice.SetRenderState(PCRenderState, PCValue);
        Exit;
      end;
    end;
  end;

  Result := 0; // TODO : What do we return on failure?
end;

procedure XTL_EmuUpdateDeferredStates(); {NOPATCH}
// Branch:shogun  Revision:163  Translator:PatrickvL  Done:100
var
  v: int;
  pCur: PDWORDs;
  X_D3DTSS: DWORD;
  pTexture: XTL_PIDirect3DBaseTexture8;
  dwValue: DWORD;
begin
  // Generic transfer of all Xbox deferred render states to PC :
  for v := X_D3DRS_DEFERRED_FIRST to X_D3DRS_DEFERRED_LAST - 1 do
    DxbxTransferRenderState(X_D3DRENDERSTATETYPE(v));

  // Certain D3DTS values need to be checked on each Draw[Indexed]^Vertices
  if (XTL_EmuD3DDeferredTextureState <> nil) then
  begin
    for v := 0 to X_D3DTS_STAGECOUNT-1 do
    begin
      pCur := @(XTL_EmuD3DDeferredTextureState[v*X_D3DTS_STAGESIZE]);

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_ADDRESSU)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 5) then
          DxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_ADDRESSU, X_D3DTSS);
      end;

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_ADDRESSV)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 5) then
          DxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_ADDRESSV, X_D3DTSS);
      end;

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_ADDRESSW)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 5) then
          DxbxKrnlCleanup('ClampToEdge is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_ADDRESSW, X_D3DTSS);
      end;

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_MAGFILTER)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 4) then
          DxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MAGFILTER, X_D3DTSS);
      end;

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_MINFILTER)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 4) then
          DxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MINFILTER, X_D3DTSS);
      end;

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_MIPFILTER)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
      begin
        if (X_D3DTSS = 4) then
          DxbxKrnlCleanup('QuinCunx is unsupported (temporarily)');

        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MIPFILTER, X_D3DTSS);
      end;

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_MIPMAPLODBIAS)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MIPMAPLODBIAS, X_D3DTSS);

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_MAXMIPLEVEL)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MAXMIPLEVEL, X_D3DTSS);

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_MAXANISOTROPY)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        IDirect3DDevice_SetSamplerState(g_pD3DDevice, v, D3DSAMP_MAXANISOTROPY, X_D3DTSS);

      // TODO -oDxbx : Emulate X_D3DTSS_COLORKEYOP (Xbox ext.)
      // TODO -oDxbx : Emulate X_D3DTSS_COLORSIGN (Xbox ext.)
      // TODO -oDxbx : Emulate X_D3DTSS_ALPHAKILL (Xbox ext.)

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_COLOROP)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLOROP, EmuXB2PC_D3DTEXTUREOP(X_D3DTSS));

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_COLORARG0)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLORARG0, X_D3DTSS);

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_COLORARG1)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLORARG1, X_D3DTSS);

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_COLORARG2)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLORARG2, X_D3DTSS);

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_ALPHAOP)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAOP, EmuXB2PC_D3DTEXTUREOP(X_D3DTSS));

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_ALPHAARG0)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAARG0, X_D3DTSS);

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_ALPHAARG1)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAARG1, X_D3DTSS);

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_ALPHAARG2)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAARG2, X_D3DTSS);

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_RESULTARG)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_RESULTARG, X_D3DTSS);

      X_D3DTSS := pCur[DxbxVersionAdjust_D3DTSS(X_D3DTSS_TEXTURETRANSFORMFLAGS)];
      if (X_D3DTSS <> X_D3DTSS_UNK) then
        g_pD3DDevice.SetTextureStageState(v, D3DTSS_TEXTURETRANSFORMFLAGS, X_D3DTSS);

      // Cxbx note : D3DTSS_BORDERCOLOR is NOT a deferred texture state!

      (* Cxbx has this disabled :
      // To check for unhandled texture stage state changes
      for(int r=0;r<X_D3DTS_STAGESIZE;r++)
      begin
        static const int unchecked[]^ =
        begin
          0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 29, 30, 31
        end;;

        if (pCur[r]^ <> X_D3DTSS_UNK) then
        begin
          _bool pass := true;

          for(int q=0;q<sizeof(unchecked)/sizeof(int);q++)
          begin
            if (r = unchecked[q]^) then
            begin
              pass := false;
              break;
            end;
          end;

          if (pass) then
            EmuWarning('Unhandled TextureState Change @ %d.%d', [v, r]^);
        end;
      end;
      *)
    end;

    // if point sprites are enabled, copy stage 3 over to 0
    if (XTL_EmuMappedD3DRenderState[X_D3DRS_POINTSPRITEENABLE]^ = DWord(BOOL_TRUE)) then // Dxbx note : DWord cast to prevent warning
    begin
      // pCur := Texture Stage 3 States
      pCur := @(XTL_EmuD3DDeferredTextureState[3*X_D3DTS_STAGESIZE]); // StrikerX3: why was this 2*32? PatrickvL: Probably a bug.

      // set the point sprites texture
      g_pD3DDevice.GetTexture(3, PIDirect3DBaseTexture(@pTexture));
      g_pD3DDevice.SetTexture(0, IDirect3DBaseTexture(pTexture));
      // TODO -oDXBX: Should we clear the pTexture interface (and how)?

      // disable all other stages
      g_pD3DDevice.SetTextureStageState(1, D3DTSS_COLOROP, D3DTOP_DISABLE);
      g_pD3DDevice.SetTextureStageState(1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);

      // in that case we have to copy over the stage by hand
      for v := 0 to X_D3DTS_STAGESIZE-1 do
      begin
        if (pCur[v] <> X_D3DTSS_UNK) then
        begin
          g_pD3DDevice.GetTextureStageState(3, D3DTEXTURESTAGESTATETYPE(v), {Out}dwValue);
          g_pD3DDevice.SetTextureStageState(0, D3DTEXTURESTAGESTATETYPE(v), dwValue);
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
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLOROP,   D3DTOP_MODULATE);
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLORARG1, D3DTA_TEXTURE);
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_COLORARG2, D3DTA_CURRENT);

      g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAOP,   D3DTOP_MODULATE);
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
      g_pD3DDevice.SetTextureStageState(v, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
    end;

    g_pD3DDevice.SetRenderState(D3DRS_NORMALIZENORMALS, BOOL_TRUE);
    g_pD3DDevice.SetRenderState(D3DRS_LIGHTING,BOOL_TRUE);
    g_pD3DDevice.SetRenderState(D3DRS_AMBIENT, $FFFFFFFF);
    *)
  end;
end;

{.$MESSAGE 'PatrickvL reviewed up to here'}
end.
