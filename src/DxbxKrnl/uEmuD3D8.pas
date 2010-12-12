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
unit uEmuD3D8;

{$DEFINE _OPTIMIZE_UNIT}

{$INCLUDE Dxbx.inc}

{.$define _DEBUG_DUMP_TEXTURE_SETTEXTURE}
{.$define _DEBUG_DUMP_TEXTURE_REGISTER}
{.$define _DEBUG_TRACK_VS}
{.$define _DEBUG_TRACE_VB}
{.$define _DEBUG_TRACK_VS_CONST}

{$DEFINE DXBX_PIXELSHADER_HOOKS} // Disable this to try dynamic pixel shader support
{.$DEFINE DXBX_TRY_DEEPER_DEVICE_INIT} // Try to run more of the original initialization code
{.$DEFINE DXBX_INDEXED_QUADLIST_TEST} // Render indexed QUADLIST as indexed TRIANGLFANS (XDK Ripple sample improves, although text&help disappears)

interface

uses
  // Delphi
  Windows,
  MMSystem, // timeBeginPeriod
  MultiMon,
  Messages,
  SysUtils, // strlen
  Classes, // TStringList
  Math,
  // Jedi Win32API
  JwaWinType,
  JwaNative,
  // DirectX
  DirectDraw, // IDIRECTDRAWSURFACE7
  Direct3D, // PD3DCOLOR
{$IFDEF DXBX_USE_D3D9}
  Direct3D9,
  D3DX9,
  DXErr9,
{$ELSE}
  Direct3D8, // D3DDEVTYPE
  D3DX8, // PD3DXVECTOR4
{$ENDIF}
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uConsts,
  uXbVideo,
  uEmuDSound,
  uTypes, // Use after uXBVideo, to declare BOOL correctly!
  uDxbxUtils,
  uLog,
  uXbe,
  uDxbxKrnlUtils,
  uState,
  uResourceTracker,
  uEmuShared,
  uEmuAlloc,
  uEmuFS,
  uEmu,
  uEmuDInput,
  uEmuKrnl,
  uEmuKrnlKe,
  uEmuD3D8Types,
  uEmuD3D8Utils,
  uConvert,
  uPixelShader,
  uPushBuffer,
  uVertexShader,
  uVertexBuffer,
  uEmuXapi, // PXINPUT_FEEDBACK
  uEmuXG;

function DxbxUnlockD3DResource(pResource: PX_D3DResource; uiLevel: int = 0; iFace: int = Ord(D3DCUBEMAP_FACE_POSITIVE_X) - 1): Boolean;
function DxbxFVF_GetTextureSize(const dwVertexShader: DWORD; const aTextureIndex: Integer): Integer;
function DxbxFVFToVertexSizeInBytes(const dwVertexShader: DWORD; bIncludeTextures: Boolean = True): uint;
function DxbxPresent(pSourceRect: PRECT; pDestRect: PRECT; pDummy1: HWND; pDummy2: PVOID): UINT;
function DxbxSetVertexData(const Register_: X_D3DVSDE; const a, b, c, d: FLOAT): HRESULT;

procedure XTL_EmuD3DInit(XbeHeader: PXBEIMAGE_HEADER; XbeHeaderSize: UInt32); {NOPATCH}
function XTL_EmuDirect3D_CreateDevice(Adapter: UINT; DeviceType: D3DDEVTYPE;
  hFocusWindow: HWND; BehaviorFlags: DWORD;
  pPresentationParameters: PX_D3DPRESENT_PARAMETERS;
  ppReturnedDeviceInterface: XTL_PPIDirect3DDevice8): HRESULT; stdcall; // forward

function XTL_EmuD3DDevice_GetVertexShader({CONST} pHandle: PDWORD): HRESULT; stdcall; // forward

procedure XTL_EmuD3DResource_Register(pThis: PX_D3DResource;
  pBase: PVOID); stdcall;
function XTL_EmuD3DDevice_CreateTexture(Width: UINT; Height: UINT;
    Levels: UINT; Usage: DWORD; Format: X_D3DFORMAT; Pool: X_D3DPOOL; ppTexture: PPX_D3DTexture): HRESULT; stdcall;
function XTL_EmuD3DDevice_CreateVolumeTexture(Width: UINT; Height: UINT;
    Depth: UINT; Levels: UINT; Usage: DWORD; Format: X_D3DFORMAT;
    Pool: X_D3DPOOL; ppVolumeTexture: PPX_D3DVolumeTexture): HRESULT; stdcall;
function XTL_EmuD3DDevice_CreateCubeTexture(
    EdgeLength: UINT;
    Levels: UINT;
    Usage: DWORD;
    Format: X_D3DFORMAT;
    Pool: X_D3DPOOL;
    ppCubeTexture: PPX_D3DCubeTexture): HRESULT; stdcall;
function XTL_EmuD3DDevice_CreateVertexBuffer(
    Length: UINT;
    Usage: DWORD;
    FVF: DWORD;
    Pool: X_D3DPOOL;
    ppVertexBuffer: PPX_D3DVertexBuffer): HRESULT; stdcall;
function XTL_EmuD3DDevice_CreateIndexBuffer(
    Length: UINT;
    Usage: DWORD;
    Format: X_D3DFORMAT;
    Pool: X_D3DPOOL;
    ppIndexBuffer: PPX_D3DIndexBuffer): HRESULT; stdcall;
function XTL_EmuD3DDevice_CreatePalette(
    Size: X_D3DPALETTESIZE;
    ppPalette: PPX_D3DPalette): HRESULT; stdcall;
function XTL_EmuD3DTexture_GetSurfaceLevel(pThis: PX_D3DTexture;
    Level: UINT;
    ppSurfaceLevel: PPX_D3DSurface): HRESULT; stdcall;

function XTL_EmuD3DPalette_Lock2(pThis: PX_D3DPalette; Flags: DWORD): PD3DCOLOR; stdcall;
function XTL_EmuD3DDevice_CreatePalette2(Size: X_D3DPALETTESIZE): PX_D3DPalette; stdcall;
function XTL_EmuD3DDevice_EnableOverlay(Enable: BOOL): HRESULT; stdcall;
function XTL_EmuD3DDevice_UpdateOverlay(pSurface: PX_D3DSurface;
  SrcRect: PRECT;
  DstRect: PRECT;
  EnableColorKey: BOOL;
  ColorKey: D3DCOLOR): HRESULT; stdcall;
function XTL_EmuD3DDevice_CreateVertexBuffer2(Length: UINT): PX_D3DVertexBuffer; stdcall;
procedure XTL_EmuD3DDevice_SetRenderState_Simple_Internal(
  XboxRenderState: X_D3DRenderStateType;
  XboxValue: DWORD
  ); {NOPATCH}

var
  // Global(s)
  g_pD3DDevice: IDirect3DDevice = NULL;
  g_pDDSPrimary7: XTL_LPDIRECTDRAWSURFACE7 = NULL;  // DirectDraw7 Primary Surface
  g_pDDSOverlay7: XTL_LPDIRECTDRAWSURFACE7 = NULL; // DirectDraw7 Overlay Surface
  g_pDDClipper7: XTL_LPDIRECTDRAWCLIPPER = NULL;    // DirectDraw7 Clipper
  g_CurrentVertexShader: DWORD = 0;
  g_bFakePixelShaderLoaded: BOOL_ = FALSE;
  g_bIsFauxFullscreen: BOOL_ = FALSE;
  g_bHackUpdateSoftwareOverlay: BOOL_ = FALSE;

function EmuThreadRenderWindow(lpVoid: LPVOID): DWORD; stdcall;
function EmuThreadCreateDeviceProxy(lpVoid: LPVOID): DWORD; stdcall;
function EmuMsgProc(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; // forward
function EmuThreadUpdateTickCount(lpVoid: LPVOID): DWORD; stdcall;
function EmuThreadPollInput(lpVoid: LPVOID): DWORD; stdcall;
function EmuThreadHandleVBlank(lpVoid: LPVOID): DWORD; stdcall;

const
  RESOURCE_CACHE_SIZE = 16;

var
  // Static Variable(s)
  g_ddguid: GUID;                // DirectDraw driver GUID
  g_hMonitor: HMONITOR = 0;      // Handle to DirectDraw monitor
  g_pD3D: IDirect3D = NULL;
  g_bYUY2OverlaysSupported: BOOL_ = FALSE; // Does device support YUY2 overlays?
  g_pDD7: XTL_LPDIRECTDRAW7 = NULL;   // DirectDraw7
  g_dwOverlayW: DWORD = 640;     // Cached Overlay Width
  g_dwOverlayH: DWORD = 480;     // Cached Overlay Height
  g_dwOverlayP: DWORD = 640;     // Cached Overlay Pitch

  g_XbeHeader: PXBEIMAGE_HEADER = NULL;         // XbeHeader
  g_XbeHeaderSize: uint32 = 0;             // XbeHeaderSize
  g_D3DCaps: D3DCAPS;                     // Direct3D8 Caps
  g_XboxCaps: X_D3DCAPS;
  g_hBgBrush: HBRUSH = 0;                  // Background Brush
  g_bRenderWindowActive: _bool = false;     // volatile?
  g_XBVideo: XBVideo;
  g_pVBCallback: D3DVBLANKCALLBACK = NULL; // Vertical-Blank callback routine
  g_pSwapCallback: D3DSWAPCALLBACK = NULL; // Swap/Present callback routine
  g_bIsBusy: BOOL;

  // wireframe toggle
  g_iWireframe: int = 0;

  // build version
  // g_BuildVersion: uint32; // Dxbx note : Already declared in uState.pas

  // resource caching for _Register
  g_EmuD3DResourceCache: array [0..RESOURCE_CACHE_SIZE - 1] of X_D3DResource; // Cxbx calls this 'pCache'

  // current active index buffer
  g_pIndexBuffer: PX_D3DIndexBuffer = NULL; // current active index buffer
  g_dwBaseVertexIndex: DWORD = 0; // current active index buffer base index

  // current active vertex stream
  g_pVertexBuffer: PX_D3DVertexBuffer = NULL; // current active vertex buffer
  g_pDummyBuffer: XTL_PIDirect3DVertexBuffer8 = NULL; // Dummy buffer, used to set unused stream sources with

  // current vertical blank information
  g_VBData: D3DVBLANKDATA; // = {0};
  g_VBLastSwap: DWORD = 0;

  // current swap information
  g_SwapData: D3DSWAPDATA; // = {0};
//  g_SwapLast: DWORD = 0; // Dxbx note : Unused

  // cached Direct3D state variable(s)
  g_pCachedRenderTarget: PX_D3DSurface = NULL;
  g_pCachedZStencilSurface: PX_D3DSurface = NULL;
  g_YuvSurface: PX_D3DSurface = NULL;
  g_fYuvEnabled: BOOL_ = FALSE;
  g_dwVertexShaderUsage: DWORD = 0;
  g_VertexShaderSlots: array [0..D3DVS_XBOX_NR_ADDRESS_SLOTS{=136} - 1] of DWORD;

  // cached palette pointer
  g_pCurrentPalette: PBytes;
  // cached palette size
  g_dwCurrentPaletteSize: DWORD = DWORD(-1);

  g_VertexShaderConstantMode: X_VERTEXSHADERCONSTANTMODE = X_D3DSCM_192CONSTANTS;

// cached Direct3D tiles
// g_EmuD3DTileCache (8 tiles maximum)
var g_EmuD3DTileCache: array [0..$08 - 1] of X_D3DTILE;

// cached active texture
var g_EmuD3DActiveTexture: array [0..X_D3DTS_STAGECOUNT-1] of PX_D3DBaseTexture; // = {0,0,0,0};

// information passed to the create device proxy thread
type EmuD3D8CreateDeviceProxyData = packed record
    CreationParameters: D3DDEVICE_CREATION_PARAMETERS;
    pPresentationParameters: PX_D3DPRESENT_PARAMETERS; // Original Xbox version
    NativePresentationParameters: D3DPRESENT_PARAMETERS; // Converted to Windows
    ppReturnedDeviceInterface: XTL_PPIDirect3DDevice8;
    {volatile} bReady: _bool;
    {union} case Integer of
      0: ({volatile} hRet: HRESULT);
      1: ({volatile} bCreate: _bool); // false: release
  end; // packed size = 29

var g_EmuCDPD: EmuD3D8CreateDeviceProxyData;

var XTL_Direct3D_GetDeviceCaps: function
(
//  Adapter: UINT;
//  DeviceType: D3DDEVTYPE;
  pCaps: PD3DCAPS
): HRESULT; stdcall;

var
  DxbxFix_HasZBuffer: Boolean = False;

implementation

uses
  // Dxbx
  uDxbxKrnl
  , uXboxLibraryUtils; // Should not be here, but needed for DxbxKrnlRegisterThread

const lfUnit = lfCxbx or lfGraphics;

var
  g_Title: string = '';
  g_SignalScreenShot: Boolean = False;

type
  RLogStackHelper = record helper for RLogStack
    function _(const aValue: X_D3DPOOL; const aName: string = ''): PLogStack; overload;
    function _(const aValue: X_D3DTRANSFORMSTATETYPE; const aName: string = ''): PLogStack; overload;
    function _(const aValue: X_D3DPRIMITIVETYPE; const aName: string = ''): PLogStack; overload;
    function _(const aValue: X_D3DFORMAT; const aName: string = ''): PLogStack; overload;
    function _(const aValue: D3DDEVTYPE; const aName: string = ''): PLogStack; overload;
    function _(const aValue: X_NV2AMETHOD; const aName: string = ''): PLogStack; overload;
    function _(const aValue: X_D3DTEXTURESTAGESTATETYPE; const aName: string = ''): PLogStack; overload;
    function _(const aValue: PD3DVIEWPORT; const aName: string = ''): PLogStack; overload;
    function _(const aValue: X_D3DVSDE; const aName: string = ''): PLogStack; overload;
    function _(const aValue: PX_D3DResource; const aName: string = ''): PLogStack; overload;
    function _(const aValue: X_D3DMULTISAMPLE_TYPE; const aName: string = ''): PLogStack; overload;
  end;

function RLogStackHelper._(const aValue: X_D3DPOOL; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'X_D3DPOOL');
  // D3DPoolToString :
  case aValue of
    D3DPOOL_DEFAULT: SetValue(UIntPtr(aValue), 'D3DPOOL_DEFAULT');
    D3DPOOL_MANAGED: SetValue(UIntPtr(aValue), 'D3DPOOL_MANAGED');
    D3DPOOL_SYSTEMMEM: SetValue(UIntPtr(aValue), 'D3DPOOL_SYSTEMMEM');
    D3DPOOL_SCRATCH: SetValue(UIntPtr(aValue), 'D3DPOOL_SCRATCH');
  else SetValue(UIntPtr(aValue));
  end;
end;

function RLogStackHelper._(const aValue: X_D3DTRANSFORMSTATETYPE; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'X_D3DTRANSFORMSTATETYPE');
  case aValue of
    X_D3DTS_VIEW: SetValue(UIntPtr(aValue), 'X_D3DTS_VIEW');
    X_D3DTS_PROJECTION: SetValue(UIntPtr(aValue), 'X_D3DTS_PROJECTION');
    X_D3DTS_TEXTURE0: SetValue(UIntPtr(aValue), 'X_D3DTS_TEXTURE0');
    X_D3DTS_TEXTURE1: SetValue(UIntPtr(aValue), 'X_D3DTS_TEXTURE1');
    X_D3DTS_TEXTURE2: SetValue(UIntPtr(aValue), 'X_D3DTS_TEXTURE2');
    X_D3DTS_TEXTURE3: SetValue(UIntPtr(aValue), 'X_D3DTS_TEXTURE3');
    X_D3DTS_WORLD: SetValue(UIntPtr(aValue), 'X_D3DTS_WORLD');
    X_D3DTS_WORLD1: SetValue(UIntPtr(aValue), 'X_D3DTS_WORLD1');
    X_D3DTS_WORLD2: SetValue(UIntPtr(aValue), 'X_D3DTS_WORLD2');
    X_D3DTS_WORLD3: SetValue(UIntPtr(aValue), 'X_D3DTS_WORLD3');
    X_D3DTS_MAX: SetValue(UIntPtr(aValue), 'X_D3DTS_MAX');
    X_D3DTS_FORCE_DWORD: SetValue(UIntPtr(aValue), 'X_D3DTS_FORCE_DWORD');
  else SetValue(UIntPtr(aValue));
  end;
end;

function RLogStackHelper._(const aValue: X_D3DPRIMITIVETYPE; const aName: string = ''): PLogStack;
var
  Str: string;
begin
  Result := SetName(aName, 'X_D3DPRIMITIVETYPE');
  Str := X_D3DPRIMITIVETYPE2String(aValue);
  if Str <> '' then
    SetValue(UIntPtr(aValue), Str)
  else
    SetValue(UIntPtr(aValue));
end;

function RLogStackHelper._(const aValue: X_D3DFORMAT; const aName: string = ''): PLogStack;
var
  Str: string;
begin
  Result := SetName(aName, 'X_D3DFORMAT');
  Str := X_D3DFORMAT2String(aValue);
  if Str <> '' then
    SetValue(UIntPtr(aValue), Str)
  else
    SetValue(UIntPtr(aValue));
end;

function RLogStackHelper._(const aValue: D3DDEVTYPE; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'D3DDEVTYPE');
  case aValue of
    D3DDEVTYPE_HAL: SetValue(UIntPtr(aValue), 'D3DDEVTYPE_HAL');
    D3DDEVTYPE_REF: SetValue(UIntPtr(aValue), 'D3DDEVTYPE_REF');
    D3DDEVTYPE_SW: SetValue(UIntPtr(aValue), 'D3DDEVTYPE_SW');
  else SetValue(UIntPtr(aValue));
  end;
end;

function RLogStackHelper._(const aValue: X_NV2AMETHOD; const aName: string = ''): PLogStack;
var
  rs: X_D3DRenderStateType;
begin
  Result := SetName(aName, 'X_NV2AMETHOD');

  rs := DxbxXboxMethodToRenderState(aValue);
  if rs in [X_D3DRS_FIRST..X_D3DRS_LAST] then
    SetValue(UIntPtr(aValue), 'NV2A_' + DxbxRenderStateXB2String[rs])
  else
    SetValue(UIntPtr(aValue));
end;

function RLogStackHelper._(const aValue: X_D3DTEXTURESTAGESTATETYPE; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'X_D3DTEXTURESTAGESTATETYPE');
  case aValue of
    X_D3DTSS_ADDRESSU: SetValue(UIntPtr(aValue), 'X_D3DTSS_ADDRESSU');
    X_D3DTSS_ADDRESSV: SetValue(UIntPtr(aValue), 'X_D3DTSS_ADDRESSV');
    X_D3DTSS_ADDRESSW: SetValue(UIntPtr(aValue), 'X_D3DTSS_ADDRESSW');
    X_D3DTSS_MAGFILTER: SetValue(UIntPtr(aValue), 'X_D3DTSS_MAGFILTER');
    X_D3DTSS_MINFILTER: SetValue(UIntPtr(aValue), 'X_D3DTSS_MINFILTER');
    X_D3DTSS_MIPFILTER: SetValue(UIntPtr(aValue), 'X_D3DTSS_MIPFILTER');
    X_D3DTSS_MIPMAPLODBIAS: SetValue(UIntPtr(aValue), 'X_D3DTSS_MIPMAPLODBIAS');
    X_D3DTSS_MAXMIPLEVEL: SetValue(UIntPtr(aValue), 'X_D3DTSS_MAXMIPLEVEL');
    X_D3DTSS_MAXANISOTROPY: SetValue(UIntPtr(aValue), 'X_D3DTSS_MAXANISOTROPY');
    X_D3DTSS_COLORKEYOP: SetValue(UIntPtr(aValue), 'X_D3DTSS_COLORKEYOP');
    X_D3DTSS_COLORSIGN: SetValue(UIntPtr(aValue), 'X_D3DTSS_COLORSIGN');
    X_D3DTSS_ALPHAKILL: SetValue(UIntPtr(aValue), 'X_D3DTSS_ALPHAKILL');
    X_D3DTSS_COLOROP: SetValue(UIntPtr(aValue), 'X_D3DTSS_COLOROP');
    X_D3DTSS_COLORARG0: SetValue(UIntPtr(aValue), 'X_D3DTSS_COLORARG0');
    X_D3DTSS_COLORARG1: SetValue(UIntPtr(aValue), 'X_D3DTSS_COLORARG1');
    X_D3DTSS_COLORARG2: SetValue(UIntPtr(aValue), 'X_D3DTSS_COLORARG2');
    X_D3DTSS_ALPHAOP: SetValue(UIntPtr(aValue), 'X_D3DTSS_ALPHAOP');
    X_D3DTSS_ALPHAARG0: SetValue(UIntPtr(aValue), 'X_D3DTSS_ALPHAARG0');
    X_D3DTSS_ALPHAARG1: SetValue(UIntPtr(aValue), 'X_D3DTSS_ALPHAARG1');
    X_D3DTSS_ALPHAARG2: SetValue(UIntPtr(aValue), 'X_D3DTSS_ALPHAARG2');
    X_D3DTSS_RESULTARG: SetValue(UIntPtr(aValue), 'X_D3DTSS_RESULTARG');
    X_D3DTSS_TEXTURETRANSFORMFLAGS: SetValue(UIntPtr(aValue), 'X_D3DTSS_TEXTURETRANSFORMFLAGS');
    X_D3DTSS_BUMPENVMAT00: SetValue(UIntPtr(aValue), 'X_D3DTSS_BUMPENVMAT00');
    X_D3DTSS_BUMPENVMAT01: SetValue(UIntPtr(aValue), 'X_D3DTSS_BUMPENVMAT01');
    X_D3DTSS_BUMPENVMAT11: SetValue(UIntPtr(aValue), 'X_D3DTSS_BUMPENVMAT11');
    X_D3DTSS_BUMPENVMAT10: SetValue(UIntPtr(aValue), 'X_D3DTSS_BUMPENVMAT10');
    X_D3DTSS_BUMPENVLSCALE: SetValue(UIntPtr(aValue), 'X_D3DTSS_BUMPENVLSCALE');
    X_D3DTSS_BUMPENVLOFFSET: SetValue(UIntPtr(aValue), 'X_D3DTSS_BUMPENVLOFFSET');
    X_D3DTSS_TEXCOORDINDEX: SetValue(UIntPtr(aValue), 'X_D3DTSS_TEXCOORDINDEX');
    X_D3DTSS_BORDERCOLOR: SetValue(UIntPtr(aValue), 'X_D3DTSS_BORDERCOLOR');
    X_D3DTSS_COLORKEYCOLOR: SetValue(UIntPtr(aValue), 'X_D3DTSS_COLORKEYCOLOR');
  else SetValue(UIntPtr(aValue));
  end;
end;

function RLogStackHelper._(const aValue: PD3DVIEWPORT; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'PD3DVIEWPORT');
  SetValue(DWORD(aValue), Format('%d, %d, %d, %d, %f, %f',
      [aValue.X, aValue.Y, aValue.Width, aValue.Height, aValue.MinZ, aValue.MaxZ]));
end;

function RLogStackHelper._(const aValue: X_D3DVSDE; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'X_D3DVSDE');
  case DWORD(aValue) of
    X_D3DVSDE_POSITION     : SetValue(UIntPtr(aValue), 'X_D3DVSDE_POSITION');
    X_D3DVSDE_BLENDWEIGHT  : SetValue(UIntPtr(aValue), 'X_D3DVSDE_BLENDWEIGHT');
    X_D3DVSDE_NORMAL       : SetValue(UIntPtr(aValue), 'X_D3DVSDE_NORMAL');
    X_D3DVSDE_DIFFUSE      : SetValue(UIntPtr(aValue), 'X_D3DVSDE_DIFFUSE');
    X_D3DVSDE_SPECULAR     : SetValue(UIntPtr(aValue), 'X_D3DVSDE_SPECULAR');
    X_D3DVSDE_FOG          : SetValue(UIntPtr(aValue), 'X_D3DVSDE_FOG');
    X_D3DVSDE_BACKDIFFUSE  : SetValue(UIntPtr(aValue), 'X_D3DVSDE_BACKDIFFUSE');
    X_D3DVSDE_BACKSPECULAR : SetValue(UIntPtr(aValue), 'X_D3DVSDE_BACKSPECULAR');
    X_D3DVSDE_TEXCOORD0    : SetValue(UIntPtr(aValue), 'X_D3DVSDE_TEXCOORD0');
    X_D3DVSDE_TEXCOORD1    : SetValue(UIntPtr(aValue), 'X_D3DVSDE_TEXCOORD1');
    X_D3DVSDE_TEXCOORD2    : SetValue(UIntPtr(aValue), 'X_D3DVSDE_TEXCOORD2');
    X_D3DVSDE_TEXCOORD3    : SetValue(UIntPtr(aValue), 'X_D3DVSDE_TEXCOORD3');
    X_D3DVSDE_VERTEX       : SetValue(UIntPtr(aValue), 'X_D3DVSDE_VERTEX');
  else SetValue(UIntPtr(aValue));
  end;
end;

function RLogStackHelper._(const aValue: PX_D3DResource; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'PX_D3DResource');
  if Assigned(aValue) then
    SetValue(ResourceToString(aValue))
  else
    SetValue(UIntPtr(aValue), 'Resource');
end;

function RLogStackHelper._(const aValue: X_D3DMULTISAMPLE_TYPE; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'X_D3DMULTISAMPLE_TYPE');
  case aValue of
    X_D3DMULTISAMPLE_NONE:
      SetValue(UIntPtr(aValue), 'X_D3DMULTISAMPLE_NONE');
    X_D3DMULTISAMPLE_2_SAMPLES_MULTISAMPLE_LINEAR:
      SetValue(UIntPtr(aValue), 'X_D3DMULTISAMPLE_2_SAMPLES_MULTISAMPLE_LINEAR');
    X_D3DMULTISAMPLE_2_SAMPLES_MULTISAMPLE_QUINCUNX:
      SetValue(UIntPtr(aValue), 'X_D3DMULTISAMPLE_2_SAMPLES_MULTISAMPLE_QUINCUNX');
    X_D3DMULTISAMPLE_2_SAMPLES_SUPERSAMPLE_HORIZONTAL_LINEAR:
      SetValue(UIntPtr(aValue), 'X_D3DMULTISAMPLE_2_SAMPLES_SUPERSAMPLE_HORIZONTAL_LINEAR');
    X_D3DMULTISAMPLE_2_SAMPLES_SUPERSAMPLE_VERTICAL_LINEAR:
      SetValue(UIntPtr(aValue), 'X_D3DMULTISAMPLE_2_SAMPLES_SUPERSAMPLE_VERTICAL_LINEAR');
    X_D3DMULTISAMPLE_4_SAMPLES_MULTISAMPLE_LINEAR:
      SetValue(UIntPtr(aValue), 'X_D3DMULTISAMPLE_4_SAMPLES_MULTISAMPLE_LINEAR');
    X_D3DMULTISAMPLE_4_SAMPLES_MULTISAMPLE_GAUSSIAN:
      SetValue(UIntPtr(aValue), 'X_D3DMULTISAMPLE_4_SAMPLES_MULTISAMPLE_GAUSSIAN');
    X_D3DMULTISAMPLE_4_SAMPLES_SUPERSAMPLE_LINEAR:
      SetValue(UIntPtr(aValue), 'X_D3DMULTISAMPLE_4_SAMPLES_SUPERSAMPLE_LINEAR');
    X_D3DMULTISAMPLE_4_SAMPLES_SUPERSAMPLE_GAUSSIAN:
      SetValue(UIntPtr(aValue), 'X_D3DMULTISAMPLE_4_SAMPLES_SUPERSAMPLE_GAUSSIAN');
    X_D3DMULTISAMPLE_9_SAMPLES_MULTISAMPLE_GAUSSIAN:
      SetValue(UIntPtr(aValue), 'X_D3DMULTISAMPLE_9_SAMPLES_MULTISAMPLE_GAUSSIAN');
    X_D3DMULTISAMPLE_9_SAMPLES_SUPERSAMPLE_GAUSSIAN:
      SetValue(UIntPtr(aValue), 'X_D3DMULTISAMPLE_9_SAMPLES_SUPERSAMPLE_GAUSSIAN');
  else SetValue(UIntPtr(aValue));
  end;
end;

//

function DxbxD3DErrorString(hResult: HRESULT): string;
begin
{$IFDEF DXBX_USE_D3D9}
  Result := DXGetErrorString9(hResult); // Source : http://www.fairyengine.com/articles/dxmultiviews.htm
  Result := Result + #13#10 + DXGetErrorDescription9(hResult); // Source : http://www.gamedev.net/community/forums/showfaq.asp?forum_id=10
{$ELSE}
  Result := D3DXGetErrorString(hResult); // Source : http://www.gamedev.net/community/forums/topic.asp?topic_id=16157
{$ENDIF}

  // If this gives a too cryptic output, try the old method :
  if (Length(Result) < 8) or (Result[1] < 'A') or (Result[1] > 'z') then
    Result := D3DErrorString(hResult);
end;

procedure DxbxResetGlobals;
begin
  g_pD3DDevice := NULL;
  g_pDDSPrimary7 := NULL;
  g_pDDSOverlay7 := NULL;
  g_pDDClipper7 := NULL;
  g_CurrentVertexShader := 0;
  g_bFakePixelShaderLoaded := FALSE;
  g_bIsFauxFullscreen := FALSE;
  g_bHackUpdateSoftwareOverlay := FALSE;

  ZeroMemory(@g_ddguid, SizeOf(GUID));
  g_hMonitor := 0;
  g_pD3D := NULL;
  g_bYUY2OverlaysSupported := FALSE;
  g_pDD7 := NULL;
  g_dwOverlayW := 640;
  g_dwOverlayH := 480;
  g_dwOverlayP := 640;

  g_XbeHeader := NULL;
  g_XbeHeaderSize := 0;
  ZeroMemory(@g_D3DCaps, SizeOf(g_D3DCaps));
  g_hBgBrush := 0;
  g_bRenderWindowActive := false;
  ZeroMemory(@g_XBVideo, SizeOf(g_XBVideo));
  g_pVBCallback := NULL;
  g_pSwapCallback := NULL;

  g_iWireframe := 0;

  g_BuildVersion := DEFAULT_XDK_VERSION;

  ZeroMemory(@g_EmuD3DResourceCache, SizeOf(g_EmuD3DResourceCache));

  g_pIndexBuffer := NULL;
  g_dwBaseVertexIndex := 0;

  g_pVertexBuffer := NULL;
  g_pDummyBuffer := NULL;

  ZeroMemory(@g_VBData, SizeOf(g_VBData));
  g_VBLastSwap := 0;

  ZeroMemory(@g_SwapData, SizeOf(g_SwapData));
//  g_SwapLast := 0;

  g_pCachedRenderTarget := NULL;
  g_pCachedZStencilSurface := NULL;
  g_YuvSurface := NULL;
  g_fYuvEnabled := FALSE;
  g_dwVertexShaderUsage := 0;
  ZeroMemory(@g_VertexShaderSlots, SizeOf(g_VertexShaderSlots));

  g_pCurrentPalette := nil;
  g_dwCurrentPaletteSize := DWORD(-1);

  g_VertexShaderConstantMode := X_D3DSCM_192CONSTANTS;

  ZeroMemory(@g_EmuD3DTileCache, SizeOf(g_EmuD3DTileCache));

  ZeroMemory(@g_EmuD3DActiveTexture, SizeOf(g_EmuD3DActiveTexture));
  ZeroMemory(@g_EmuCDPD, SizeOf(g_EmuCDPD));
end;

function DxbTextureFilterCapsToString(const TextureFilterCaps: DWORD): string;
begin
  Result := '';
  if (TextureFilterCaps and D3DPTFILTERCAPS_MINFPOINT) > 0 then Result := Result + 'D3DPTFILTERCAPS_MINFPOINT ';
  if (TextureFilterCaps and D3DPTFILTERCAPS_MINFLINEAR) > 0 then Result := Result + 'D3DPTFILTERCAPS_MINFLINEAR ';
  if (TextureFilterCaps and D3DPTFILTERCAPS_MINFANISOTROPIC) > 0 then Result := Result + 'D3DPTFILTERCAPS_MINFANISOTROPIC ';
  if (TextureFilterCaps and D3DPTFILTERCAPS_MIPFPOINT) > 0 then Result := Result + 'D3DPTFILTERCAPS_MIPFPOINT ';
  if (TextureFilterCaps and D3DPTFILTERCAPS_MIPFLINEAR) > 0 then Result := Result + 'D3DPTFILTERCAPS_MIPFLINEAR ';
  if (TextureFilterCaps and D3DPTFILTERCAPS_MAGFPOINT) > 0 then Result := Result + 'D3DPTFILTERCAPS_MAGFPOINT ';
  if (TextureFilterCaps and D3DPTFILTERCAPS_MAGFLINEAR) > 0 then Result := Result + 'D3DPTFILTERCAPS_MAGFLINEAR ';
  if (TextureFilterCaps and D3DPTFILTERCAPS_MAGFANISOTROPIC) > 0 then Result := Result + 'D3DPTFILTERCAPS_MAGFANISOTROPIC ';
  if (TextureFilterCaps and D3DPTFILTERCAPS_MAGFAFLATCUBIC) > 0 then Result := Result + 'D3DPTFILTERCAPS_MAGFAFLATCUBIC ';
  if (TextureFilterCaps and D3DPTFILTERCAPS_MAGFGAUSSIANCUBIC) > 0 then Result := Result + 'D3DPTFILTERCAPS_MAGFGAUSSIANCUBIC ';
end;

function DxbTextureAddressCapsToString(const TextureAddressCaps: DWORD): string;
begin
  Result := '';
  if (TextureAddressCaps and D3DPTADDRESSCAPS_WRAP) > 0 then Result := Result + 'D3DPTADDRESSCAPS_WRAP ';
  if (TextureAddressCaps and D3DPTADDRESSCAPS_MIRROR) > 0 then Result := Result + 'D3DPTADDRESSCAPS_MIRROR ';
  if (TextureAddressCaps and D3DPTADDRESSCAPS_CLAMP) > 0 then Result := Result + 'D3DPTADDRESSCAPS_CLAMP ';
  if (TextureAddressCaps and D3DPTADDRESSCAPS_BORDER) > 0 then Result := Result + 'D3DPTADDRESSCAPS_BORDER ';
  if (TextureAddressCaps and D3DPTADDRESSCAPS_INDEPENDENTUV) > 0 then Result := Result + 'D3DPTADDRESSCAPS_INDEPENDENTUV ';
  if (TextureAddressCaps and D3DPTADDRESSCAPS_MIRRORONCE) > 0 then Result := Result + 'D3DPTADDRESSCAPS_MIRRORONCE ';
end;

procedure DxbxDumpD3DCaps(const aD3DCaps: PD3DCAPS; const aFromXbox: Boolean);
const
  Source: array [Boolean] of string = ('Native', 'Xbox');
var
  LogStack: PLogStack;
begin
  LogStack := LogBegin(Source[aFromXbox] + ' D3DCaps').
    (* Device Info *)
    _(aD3DCaps.DeviceType, 'DeviceType').
    _(aD3DCaps.AdapterOrdinal, 'AdapterOrdinal').

    (* Caps from DX7 Draw *)
    _(aD3DCaps.Caps, 'Caps').
    _(aD3DCaps.Caps2, 'Caps2').
    _(aD3DCaps.Caps3, 'Caps3').
    _(aD3DCaps.PresentationIntervals, 'PresentationIntervals').

    (* Cursor Caps *)
    _(aD3DCaps.CursorCaps, 'CursorCaps').

    (* 3D Device Caps *)
    _(aD3DCaps.DevCaps, 'DevCaps').
    _(aD3DCaps.PrimitiveMiscCaps, 'PrimitiveMiscCaps').
    _(aD3DCaps.RasterCaps, 'RasterCaps').
    _(aD3DCaps.ZCmpCaps, 'ZCmpCaps').
    _(aD3DCaps.SrcBlendCaps, 'SrcBlendCaps').
    _(aD3DCaps.DestBlendCaps, 'DestBlendCaps').
    _(aD3DCaps.AlphaCmpCaps, 'AlphaCmpCaps').
    _(aD3DCaps.ShadeCaps, 'ShadeCaps').
    _(aD3DCaps.TextureCaps, 'TextureCaps').
    _(aD3DCaps.TextureFilterCaps, 'TextureFilterCaps', DxbTextureFilterCapsToString(aD3DCaps.TextureFilterCaps)). // D3DPTFILTERCAPS for IDirect3DTexture8's
    _(aD3DCaps.CubeTextureFilterCaps, 'CubeTextureFilterCaps', DxbTextureFilterCapsToString(aD3DCaps.CubeTextureFilterCaps)). // D3DPTFILTERCAPS for IDirect3DCubeTexture8's
    _(aD3DCaps.VolumeTextureFilterCaps, 'VolumeTextureFilterCaps', DxbTextureFilterCapsToString(aD3DCaps.VolumeTextureFilterCaps)). // D3DPTFILTERCAPS for IDirect3DVolumeTexture8's
    _(aD3DCaps.TextureAddressCaps, 'TextureAddressCaps', DxbTextureAddressCapsToString(aD3DCaps.TextureAddressCaps)). // D3DPTADDRESSCAPS for IDirect3DTexture8's
    _(aD3DCaps.VolumeTextureAddressCaps, 'VolumeTextureAddressCaps', DxbTextureAddressCapsToString(aD3DCaps.VolumeTextureAddressCaps)). // D3DPTADDRESSCAPS for IDirect3DVolumeTexture8's

    _(aD3DCaps.LineCaps, 'LineCaps'). // D3DLINECAPS

    _(aD3DCaps.MaxTextureWidth, 'MaxTextureWidth').
    _(aD3DCaps.MaxTextureHeight, 'MaxTextureHeight').
    _(aD3DCaps.MaxVolumeExtent, 'MaxVolumeExtent').

    _(aD3DCaps.MaxTextureRepeat, 'MaxTextureRepeat').
    _(aD3DCaps.MaxTextureAspectRatio, 'MaxTextureAspectRatio').
    _(aD3DCaps.MaxAnisotropy, 'MaxAnisotropy').
    _(aD3DCaps.MaxVertexW, 'MaxVertexW').

    _(aD3DCaps.GuardBandLeft, 'GuardBandLeft').
    _(aD3DCaps.GuardBandTop, 'GuardBandTop').
    _(aD3DCaps.GuardBandRight, 'GuardBandRight').
    _(aD3DCaps.GuardBandBottom, 'GuardBandBottom').

    _(aD3DCaps.ExtentsAdjust, 'ExtentsAdjust').
    _(aD3DCaps.StencilCaps, 'StencilCaps').

    _(aD3DCaps.FVFCaps, 'FVFCaps').
    _(aD3DCaps.TextureOpCaps, 'TextureOpCaps').
    _(aD3DCaps.MaxTextureBlendStages, 'MaxTextureBlendStages').
    _(aD3DCaps.MaxSimultaneousTextures, 'MaxSimultaneousTextures').

    _(aD3DCaps.VertexProcessingCaps, 'VertexProcessingCaps').
    _(aD3DCaps.MaxActiveLights, 'MaxActiveLights').
    _(aD3DCaps.MaxUserClipPlanes, 'MaxUserClipPlanes').
    _(aD3DCaps.MaxVertexBlendMatrices, 'MaxVertexBlendMatrices').
    _(aD3DCaps.MaxVertexBlendMatrixIndex, 'MaxVertexBlendMatrixIndex').

    _(aD3DCaps.MaxPointSize, 'MaxPointSize').

    _(aD3DCaps.MaxPrimitiveCount, 'MaxPrimitiveCount').         // max number of primitives per DrawPrimitive call
    _(aD3DCaps.MaxVertexIndex, 'MaxVertexIndex').
    _(aD3DCaps.MaxStreams, 'MaxStreams').
    _(aD3DCaps.MaxStreamStride, 'MaxStreamStride').             // max stride for SetStreamSource

    _(aD3DCaps.VertexShaderVersion, 'VertexShaderVersion').
    _(aD3DCaps.MaxVertexShaderConst, 'MaxVertexShaderConst').   // number of vertex shader constant registers

    _(aD3DCaps.PixelShaderVersion, 'PixelShaderVersion').
{$IFNDEF DXBX_USE_D3D9}
    _(aD3DCaps.MaxPixelShaderValue, 'MaxPixelShaderValue');     // max value of pixel shader arithmetic component
{$ELSE}
    _(aD3DCaps.PixelShader1xMaxValue, 'PixelShader1xMaxValue');      // max value storable in registers of ps.1.x shaders

  if aFromXbox = False then
  begin
    LogStack.
    // Here are the DX9 specific ones
    _(aD3DCaps.DevCaps2, 'DevCaps2').

    _(aD3DCaps.MaxNpatchTessellationLevel, 'MaxNpatchTessellationLevel').
    _(aD3DCaps.Reserved5, 'Reserved5').

    _(aD3DCaps.MasterAdapterOrdinal, 'MasterAdapterOrdinal').     // ordinal of master adaptor for adapter group
    _(aD3DCaps.AdapterOrdinalInGroup, 'AdapterOrdinalInGroup').    // ordinal inside the adapter group
    _(aD3DCaps.NumberOfAdaptersInGroup, 'NumberOfAdaptersInGroup').  // number of adapters in this adapter group (only if master)
    _(aD3DCaps.DeclTypes, 'DeclTypes').                   // Data types, supported in vertex declarations
    _(aD3DCaps.NumSimultaneousRTs, 'NumSimultaneousRTs').          // Will be at least 1
    _(aD3DCaps.StretchRectFilterCaps, 'StretchRectFilterCaps').       // Filter caps supported by StretchRect

    // VS20Caps: TD3DVShaderCaps2_0
    _(aD3DCaps.VS20Caps.Caps, 'VS20Caps.Caps').
    _(aD3DCaps.VS20Caps.DynamicFlowControlDepth, 'VS20Caps.DynamicFlowControlDepth').
    _(aD3DCaps.VS20Caps.NumTemps, 'VS20Caps.NumTemps').
    _(aD3DCaps.VS20Caps.StaticFlowControlDepth, 'VS20Caps.StaticFlowControlDepth').

    // PS20Caps: TD3DPShaderCaps2_0
    _(aD3DCaps.PS20Caps.Caps, 'PS20Caps.Caps').
    _(aD3DCaps.PS20Caps.DynamicFlowControlDepth, 'PS20Caps.DynamicFlowControlDepth').
    _(aD3DCaps.PS20Caps.NumTemps, 'PS20Caps.NumTemps').
    _(aD3DCaps.PS20Caps.StaticFlowControlDepth, 'PS20Caps.StaticFlowControlDepth').
    _(aD3DCaps.PS20Caps.NumInstructionSlots, 'PS20Caps.NumInstructionSlots').

    _('VertexTextureFilterCaps').SetValue(aD3DCaps.VertexTextureFilterCaps, DxbTextureFilterCapsToString(aD3DCaps.VertexTextureFilterCaps)). // D3DPTFILTERCAPS for IDirect3DTexture9's for texture, used in vertex shaders
    _(aD3DCaps.MaxVShaderInstructionsExecuted, 'MaxVShaderInstructionsExecuted'). // maximum number of vertex shader instructions that can be executed
    _(aD3DCaps.MaxPShaderInstructionsExecuted, 'MaxPShaderInstructionsExecuted'). // maximum number of pixel shader instructions that can be executed
    _(aD3DCaps.MaxVertexShader30InstructionSlots, 'MaxVertexShader30InstructionSlots').
    _(aD3DCaps.MaxPixelShader30InstructionSlots, 'MaxPixelShader30InstructionSlots');
  end;
{$ENDIF}

  LogStack.LogEnd();
end;

procedure DxbxDumpNativeRenderStates();
var
  XState: X_D3DRENDERSTATETYPE;
  PCState: D3DRENDERSTATETYPE;
  Value: DWORD;
begin
  DbgPrintf('DxbxDumpNativeRenderStates :');
  // Go via the Xbox states for now, as we don't have a 'ToString' function for all native render states :
  for XState := X_D3DRS_FIRST to X_D3DRS_LAST do
  begin
    // See if this renderstate is supported on Native D3D :
    PCState := EmuXB2PC_D3DRS(XState);
    if Ord(PCState) = Ord(D3DRS_UNSUPPORTED) then // Ord for D3D9 compatibility
      Continue;

    // Get the value and print it :
    g_pD3DDevice.GetRenderState(PCState, {out}Value);
    DbgPrintf('  %-28s = 0x%.08X;', [DxbxRenderStateXB2String[XState], Value]);
  end;
end;

function GetResourceType(pResource: PX_D3DResource): TD3DResourceType;
var
  Surface: IDirect3DSurface;
  Volume: IDirect3DVolume;
begin
  Result := TD3DResourceType(0);
  if (pResource = nil) or (pResource.Emu.Resource = nil) then
    Exit;

  if IUnknown(pResource.Emu.Resource).QueryInterface(IDirect3DSurface, Surface) = 0 then
  begin
    Result := D3DRTYPE_SURFACE;
    Surface := nil;
  end
  else
  if IUnknown(pResource.Emu.Resource).QueryInterface(IDirect3DVolume, Volume) = 0 then
  begin
    Result := D3DRTYPE_VOLUME;
    Volume := nil;
  end
  else
    Result := IDirect3DResource(pResource.Emu.Resource).GetType();
end;

function DxbxUpdateResourceFields(pResource: PX_D3DResource): Boolean;
var
  ResourceType: TD3DResourceType;
  CommonResourceType: DWORD;

  SurfaceDesc: D3DSURFACE_DESC;
  VolumeDesc: D3DVOLUME_DESC;
  VertDesc: D3DVERTEXBUFFER_DESC;
  IndexDesc: D3DINDEXBUFFER_DESC;

  RefCount: int;
  XFormat: X_D3DFORMAT;
  Width, Height, Pitch, Levels, Dimensions: uint;

  procedure _ReadSurfaceDesc();
  begin
    XFormat := EmuPC2XB_D3DFormat(SurfaceDesc.Format);
    Width := SurfaceDesc.Width;
    Height := SurfaceDesc.Height;
    Pitch := GetSurfaceSize(@SurfaceDesc) div Height;
    Levels := 1; // Surfaces have 1 mipmap level
    Dimensions := 2;
  end;

  procedure _ReadVolumeDesc();
  begin
    XFormat := EmuPC2XB_D3DFormat(VolumeDesc.Format);
    Width := VolumeDesc.Width;
    Height := VolumeDesc.Height;
    Pitch := GetVolumeSize(@VolumeDesc) div Height;
    Levels := VolumeDesc.Depth; // ?
    Dimensions := 3;
  end;

begin
  Result := Assigned(pResource) and Assigned(pResource.Emu.Resource);
  if not Result then
    Exit;

  if (pResource.Emu.Lock = X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
    Exit;

  // Get the current reference count of this resource :
  begin
    RefCount := IUnknown(pResource.Emu.Resource)._AddRef() - 1;
    IUnknown(pResource.Emu.Resource)._Release();
  end;

  ResourceType := GetResourceType(pResource);
  case ResourceType of
    D3DRTYPE_SURFACE                : CommonResourceType := X_D3DCOMMON_TYPE_SURFACE;
    D3DRTYPE_VOLUME                 : CommonResourceType := X_D3DCOMMON_TYPE_SURFACE; // Also covers Volume resources;
    D3DRTYPE_TEXTURE                : CommonResourceType := X_D3DCOMMON_TYPE_TEXTURE;
    D3DRTYPE_VOLUMETEXTURE          : CommonResourceType := X_D3DCOMMON_TYPE_TEXTURE;
    D3DRTYPE_CUBETEXTURE            : CommonResourceType := X_D3DCOMMON_TYPE_TEXTURE;
    D3DRTYPE_VERTEXBUFFER           : CommonResourceType := X_D3DCOMMON_TYPE_VERTEXBUFFER;
    D3DRTYPE_INDEXBUFFER            : CommonResourceType := X_D3DCOMMON_TYPE_INDEXBUFFER;
  end;

  case ResourceType of
    D3DRTYPE_SURFACE:
    begin
      IDirect3DSurface(pResource.Emu.Surface).GetDesc(SurfaceDesc);
      _ReadSurfaceDesc;
    end;

    D3DRTYPE_VOLUME:
    begin
      IDirect3DVolume(pResource.Emu.Volume).GetDesc(VolumeDesc);
      _ReadVolumeDesc;
    end;

    D3DRTYPE_TEXTURE:
    begin
      IDirect3DTexture(pResource.Emu.Texture).GetLevelDesc(0, SurfaceDesc);
      _ReadSurfaceDesc;
    end;

    D3DRTYPE_VOLUMETEXTURE:
    begin
      IDirect3DVolumeTexture(pResource.Emu.VolumeTexture).GetLevelDesc(0, VolumeDesc);
      _ReadVolumeDesc;
    end;

    D3DRTYPE_CUBETEXTURE:
    begin
      IDirect3DCubeTexture(pResource.Emu.CubeTexture).GetLevelDesc(0, SurfaceDesc);
      _ReadSurfaceDesc;
      // bCubeMap := (pPixelContainer.Format and X_D3DFORMAT_CUBEMAP) > 0;
    end;

    D3DRTYPE_VERTEXBUFFER:
    begin
      IDirect3DVertexBuffer(pResource.Emu.VertexBuffer).GetDesc(VertDesc);
      XFormat := X_D3DFMT_VERTEXDATA; // ??
      Width := 0;
      Height := 0;
      Pitch := 0;
      Levels  := 0;
      Dimensions := 0;
    end;

    D3DRTYPE_INDEXBUFFER:
    begin
      IDirect3DIndexBuffer(pResource.Emu.IndexBuffer).GetDesc(IndexDesc);
      XFormat := X_D3DFMT_INDEX16;
      Width := 0;
      Height := 0;
      Pitch := 0;
      Levels  := 0;
      Dimensions := 0;
    end;
  end;

  // Set all fields :
  PX_D3DPixelContainer(pResource).Format := (XFormat shl X_D3DFORMAT_FORMAT_SHIFT)
                                         or (Log2(Width) shl X_D3DFORMAT_USIZE_SHIFT)
                                         or (Log2(Height) shl X_D3DFORMAT_VSIZE_SHIFT)
                                         or (Log2(Pitch)  shl X_D3DFORMAT_PSIZE_SHIFT)
                                         or (Levels shl X_D3DFORMAT_MIPMAP_SHIFT)
                                         // or X_D3DFORMAT_CUBEMAP
                                         // or X_D3DFORMAT_BORDERSOURCE_COLOR
                                         or (Dimensions shl X_D3DFORMAT_DIMENSION_SHIFT)
                                         ;

  //  YUV formats have no USIZE, VSIZE and PSIZE in Format, but WIDTh, HEIGHT and PITCH in Size :
  PX_D3DPixelContainer(pResource).Size := DxbxEncodeDimensionsIntoSize(Width, Height, Pitch);
  // Initialize Common field properly :
  PX_D3DPixelContainer(pResource).Common := (RefCount and X_D3DCOMMON_REFCOUNT_MASK)
                                         or CommonResourceType
                                         // or X_D3DCOMMON_VIDEOMEMORY
                                         // or X_D3DCOMMON_D3DCREATED
                                         // or X_D3DCOMMON_ISLOCKED
                                         // or ((IntRefCount shl X_D3DCOMMON_INTREFCOUNT_SHIFT) and X_D3DCOMMON_INTREFCOUNT_MASK)
                                         ;

  // PX_D3DPixelContainer(pResource).Data := leave it (what about special resources - X_D3DRESOURCE_DATA_FLAG_SPECIAL)
  // PX_D3DPixelContainer(pResource).Emu is already set.

  // Note : ResourceToString() contains some code that converts the other way around
end;


// Generic unlocking of a D3DResource.
//
// Dxbx Note : As suggested by StrikerX3, we should call this for any resource
// that's locked while it shouldn't be anymore; So call this before Lock() itself,
// before SetTexture(), SetStreamSource(), SetIndices() and maybe other calls too.
function DxbxUnlockD3DResource(pResource: PX_D3DResource; uiLevel: int = 0; iFace: int = Ord(D3DCUBEMAP_FACE_POSITIVE_X) - 1): Boolean;
var
  ResourceType: TD3DResourceType;
begin
  Result := Assigned(pResource) and Assigned(pResource.Emu.Resource);
  if not Result then
    Exit;

  if IsSpecialResource(pResource.Data)
  or (pResource.Emu.Lock = X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then // Dxbx addition
    Exit;

  ResourceType := GetResourceType(pResource);
  case ResourceType of
    D3DRTYPE_SURFACE:
      repeat until IDirect3DSurface(pResource.Emu.Surface).UnlockRect() <= D3D_OK;

    D3DRTYPE_VOLUME:
      // TODO -oDxbx : Totally untested! Volume's aren't used yet!
      repeat until IDirect3DVolume(pResource.Emu.Resource).UnlockBox() <= D3D_OK;

    D3DRTYPE_TEXTURE:
      repeat until IDirect3DTexture(pResource.Emu.Texture).UnlockRect(uiLevel) <= D3D_OK;

    D3DRTYPE_VOLUMETEXTURE:
      // TODO -oDxbx : Totally untested! VolumeTexture's aren't used yet!?
      repeat until IDirect3DVolumeTexture(pResource.Emu.VolumeTexture).UnlockBox(uiLevel) <= D3D_OK;

    D3DRTYPE_CUBETEXTURE:
    begin
      if iFace >= Ord(D3DCUBEMAP_FACE_POSITIVE_X) then
      begin
        // Only one face given, so unlock only that one :
        repeat until IDirect3DCubeTexture(pResource.Emu.CubeTexture).UnlockRect(TD3DCubeMapFaces(iFace), uiLevel) <= D3D_OK;
      end
      else
      begin
        // No face given, so unlock all faces (just to be sure) :
        repeat until IDirect3DCubeTexture(pResource.Emu.CubeTexture).UnlockRect(D3DCUBEMAP_FACE_POSITIVE_X, uiLevel) <= D3D_OK;
        repeat until IDirect3DCubeTexture(pResource.Emu.CubeTexture).UnlockRect(D3DCUBEMAP_FACE_NEGATIVE_X, uiLevel) <= D3D_OK;
        repeat until IDirect3DCubeTexture(pResource.Emu.CubeTexture).UnlockRect(D3DCUBEMAP_FACE_POSITIVE_Y, uiLevel) <= D3D_OK;
        repeat until IDirect3DCubeTexture(pResource.Emu.CubeTexture).UnlockRect(D3DCUBEMAP_FACE_NEGATIVE_Y, uiLevel) <= D3D_OK;
        repeat until IDirect3DCubeTexture(pResource.Emu.CubeTexture).UnlockRect(D3DCUBEMAP_FACE_POSITIVE_Z, uiLevel) <= D3D_OK;
        repeat until IDirect3DCubeTexture(pResource.Emu.CubeTexture).UnlockRect(D3DCUBEMAP_FACE_NEGATIVE_Z, uiLevel) <= D3D_OK;
      end;
    end;

    D3DRTYPE_VERTEXBUFFER:
      repeat until IDirect3DVertexBuffer(pResource.Emu.VertexBuffer).Unlock() <= D3D_OK;

    D3DRTYPE_INDEXBUFFER:
      repeat until IDirect3DIndexBuffer(pResource.Emu.IndexBuffer).Unlock() <= D3D_OK;
  else
    Result := False;
  end;
end;

function DxbxFVF_GetTextureSize(const dwVertexShader: DWORD; const aTextureIndex: Integer): Integer;
// Determine the size (in bytes) of the texture format (indexed 0 .. 3).
// This is the reverse of the D3DFVF_TEXCOORDSIZE[0..3] macros.
begin
  case (dwVertexShader shr ((aTextureIndex * 2) + 16)) and 3 of
    D3DFVF_TEXTUREFORMAT1: Result := 1 * SizeOf(FLOAT);
    D3DFVF_TEXTUREFORMAT2: Result := 2 * SizeOf(FLOAT);
    D3DFVF_TEXTUREFORMAT3: Result := 3 * SizeOf(FLOAT);
    D3DFVF_TEXTUREFORMAT4: Result := 4 * SizeOf(FLOAT);
  else
    Assert(False);
    Result := 0;
  end;
end;

// Dxbx Note: This code is taken from EmuExecutePushBufferRaw and occured
// in EmuFlushIVB too, so it's generalize in this single implementation.
function DxbxFVFToVertexSizeInBytes(const dwVertexShader: DWORD; bIncludeTextures: Boolean = True): uint;
var
  NrTextures: Integer;
begin
(*
  X_D3DFVF_POSITION_MASK    = $00E; // Dec  /2  #fl

  X_D3DFVF_XYZ              = $002; //  2 > 1 > 3
  X_D3DFVF_XYZRHW           = $004; //  4 > 2 > 4
  X_D3DFVF_XYZB1            = $006; //  6 > 3 > 4
  X_D3DFVF_XYZB2            = $008; //  8 > 4 > 5
  X_D3DFVF_XYZB3            = $00a; // 10 > 5 > 6
  X_D3DFVF_XYZB4            = $00c; // 12 > 6 > 7
*)
  // Divide the D3DFVF by two, this gives almost the number of floats needed for the format :
  Result := (dwVertexShader and D3DFVF_POSITION_MASK) shr 1;
  if Result >= (D3DFVF_XYZB1 shr 1) then
    // Any format from D3DFVF_XYZB1 and above need 1 extra float :
    Inc(Result, 1)
  else
    // The other formats (XYZ and XYZRHW) need 2 extra floats :
    Inc(Result, 2);

  // Express the size in bytes, instead of floats :
  Result := Result * sizeof(FLOAT);

  // D3DFVF_NORMAL cannot be combined with D3DFVF_XYZRHW :
  if (dwVertexShader and D3DFVF_POSITION_MASK) <> D3DFVF_XYZRHW then
    if (dwVertexShader and D3DFVF_NORMAL) > 0 then begin Inc(Result, sizeof(FLOAT)*3); end;

  if (dwVertexShader and D3DFVF_DIFFUSE) > 0 then begin Inc(Result, sizeof(D3DCOLOR)); end;
  if (dwVertexShader and D3DFVF_SPECULAR) > 0 then begin Inc(Result, sizeof(D3DCOLOR)); end;

  if bIncludeTextures then
  begin
    NrTextures := ((dwVertexShader and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT);
    while NrTextures > 0 do
    begin
      Dec(NrTextures);
      Inc(Result, DxbxFVF_GetTextureSize(dwVertexShader, NrTextures));
    end;
  end;
end;

{static}var ScreenShotNr: Integer = 1;
{static}var ExtraXRGBSurface: IDirect3DSurface = nil; // this is our pointer to the memory location containing our copy of the front buffer

procedure AssureExtraXRGBSurface(const BackBufferSurface: IDirect3DSurface; const Caller: string);
var
  SurfaceDesc: D3DSURFACE_DESC;
  aResult: HRESULT;
begin
  // Assure we have a reusable surface (in the correct format) which the back buffer can be converted into :
  if (ExtraXRGBSurface = nil) then
  begin
    BackBufferSurface.GetDesc({out}SurfaceDesc);
    aResult := IDirect3DDevice_CreateImageSurface(
                  g_pD3DDevice,
                  SurfaceDesc.Width,
                  SurfaceDesc.Height,
                  D3DFMT_A8R8G8B8, // This format is supported by D3DXSaveSurfaceToFile (D3DFMT_X8R8G8B8 works too)
                  @ExtraXRGBSurface);
    if FAILED(aResult) then
    begin
      DbgPrintf('EmuD3D8 : %s could not create a extra buffer!' +#13#10+ DxbxD3DErrorString(aResult), [Caller]);
    end;
  end;
end;

procedure DxbxTakeScreenShot(hWnd: HWND);
// Branch:Dxbx  Translator:PatrickvL  Done:100
// Source : http://www.gamedev.net/reference/articles/article1844.asp

  function _GetScreenshotFileName(): string;
  begin
    Result := Format('Dxbx running %s (%0.3d).bmp', [TitleToNiceFilename(g_Title), ScreenShotNr]);
    Inc(ScreenShotNr);
  end;

const
  BackBuffer = 0;
var
  BackBufferSurface: IDirect3DSurface;
  hRet: HRESULT;
  FileName: AnsiString;
begin
  // Retrieve a pointer to the backbuffer :
  hRet := g_pD3DDevice.GetBackBuffer({$IFDEF DXBX_USE_D3D9}{iSwapChain=}0,{$ENDIF} BackBuffer, D3DBACKBUFFER_TYPE_MONO, @BackBufferSurface);
  if (FAILED(hRet)) then
  begin
    DbgPrintf('EmuD3D8 : DxbxTakeScreenShot could not get back buffer!' +#13#10+ DxbxD3DErrorString(hRet));
    Exit;
  end;

  AssureExtraXRGBSurface(BackBufferSurface, 'DxbxTakeScreenShot');

  // Convert the backbuffer to the intermediate buffer (which format is supported by D3DXSaveSurfaceToFile) :
  hRet := D3DXLoadSurfaceFromSurface(ExtraXRGBSurface, NULL, NULL, BackBufferSurface, NULL, NULL, D3DX_FILTER_LINEAR{D3DX_DEFAULT}, 0);
  if (FAILED(hRet)) then
  begin
    DbgPrintf('EmuD3D8 : DxbxTakeScreenShot could not convert back buffer!' +#13#10+ DxbxD3DErrorString(hRet));
    Exit;
  end;

  // Make sure we hold no reference to the backbuffer (we could wait and let Delphi's automatic reference counting takes care of this) :
  BackBufferSurface := nil;

  // Generate a filename that doesn't exist yet (so we don't overwrite previous screenshots) :
  repeat
    FileName := AnsiString(ExtractFilePath(ParamStr(0)) + _GetScreenshotFileName());
  until not FileExists(string(FileName));

  // Now save it to file (somehow, Direct3D8 doesn't export jpgs or pngs, so bitmap has to suffice for now) :
  hRet := D3DXSaveSurfaceToFileA(PAnsiChar(FileName), D3DXIFF_BMP, ExtraXRGBSurface, NULL, NULL);
  if (FAILED(hRet)) then
    DbgPrintf('EmuD3D8 : DxbxTakeScreenShot could not write screen buffer!' +#13#10+ DxbxD3DErrorString(hRet))
  else
    DbgPrintf('EmuD3D8 : Screenshot written to %s', [FileName]);
end;

// A wrapper for Present() with an extra safeguard to restore 'device lost' errors :
function DxbxPresent(pSourceRect: PRECT; pDestRect: PRECT; pDummy1: HWND; pDummy2: PVOID): UINT;
begin
{$IFDEF DXBX_USE_D3D9}
  // end scene
  g_pD3DDevice.EndScene();
{$ENDIF}

  g_bIsBusy := BOOL_TRUE;
  if g_SignalScreenShot then
  begin
    g_SignalScreenShot := False;

    DxbxTakeScreenShot(g_hEmuWindow);
  end;

  Result := g_pD3DDevice.Present(pSourceRect, pDestRect, pDummy1, pDummy2);
  g_bIsBusy := BOOL_FALSE;

{$IFDEF DXBX_USE_D3D9}
  // begin scene
  g_pD3DDevice.BeginScene();
{$ENDIF}

  if Result = D3D_OK then
    Exit;

  if Result = UINT(D3DERR_DEVICELOST) then
  repeat
    Result := g_pD3DDevice.TestCooperativeLevel;
    if(Result = UINT(D3DERR_DEVICELOST)) then //Device is lost and cannot be reset yet
      Sleep(500) //Wait a bit so we don't burn through cycles for no reason
    else
      if(Result = UINT(D3DERR_DEVICENOTRESET)) then //Lost but we can reset it now
        Result := g_pD3DDevice.Reset({const}g_EmuCDPD.NativePresentationParameters);
  until Result = UINT(D3D_OK);
end;

procedure DxbxInitializePixelContainerYUY2(const pPixelContainer: PX_D3DPixelContainer);
var
  dwSize: DWORD;
  pRefCount: PDWORD;
begin
  // Dxbx addition : Ease the RefCount, by moving this to the start of the buffer :
  dwSize := sizeof(DWORD) + (g_dwOverlayP * g_dwOverlayH);
  pRefCount := XboxAlloc(dwSize); // TODO : Honor D3DSURFACE_ALIGNMENT and the 'contiguous memory' requirement (use D3D_AllocContiguousMemory?)
  ZeroMemory(pRefCount, dwSize);

  // initialize ref count
  pRefCount^ := 1;

  // Dxbx addition : Initialize Common field properly :
  pPixelContainer.Common := (pRefCount^ and X_D3DCOMMON_REFCOUNT_MASK) or X_D3DCOMMON_TYPE_TEXTURE or X_D3DCOMMON_D3DCREATED;
  // Because YUY2 is not supported in hardware (in Direct3D8?), we'll actually mark this as a special fake texture (set highest bit)
  pPixelContainer.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_YUVSURF;
  pPixelContainer.Emu.Lock := UIntPtr(pRefCount); // The data follows directly after the refcount
  pPixelContainer.Format := (X_D3DFMT_YUY2 shl X_D3DFORMAT_FORMAT_SHIFT)
                         or (1 shl X_D3DFORMAT_MIPMAP_SHIFT); // Surfaces have 1 mipmap level
  //  YUV formats have no USIZE, VSIZE and PSIZE in Format, but WIDTh, HEIGHT and PITCH in Size :
  pPixelContainer.Size := DxbxEncodeDimensionsIntoSize(g_dwOverlayW, g_dwOverlayH, g_dwOverlayP);
end;

type
  PX_D3DFORMAT = ^X_D3DFORMAT;

function DxbxXB2PC_D3DFormat(const X_Format: X_D3DFORMAT; const aResourceType: TD3DResourceType; const CacheFormat: PX_D3DFORMAT = nil): D3DFORMAT;
begin
  if Assigned(CacheFormat) then
    CacheFormat^ := X_Format; // Save this for later; DxbxUpdatePixelContainer should convert when needed!

  // Convert Format (Xbox->PC)
  Result := EmuXB2PC_D3DFormat(X_Format);

  // TODO -oCXBX: HACK: Devices that don't support this should somehow emulate it!
  // TODO -oDxbx: Non-supported formats should be emulated in a generic way
  // TODO -oDxbx : Check device caps too!
  case Result of
    D3DFMT_P8:
    begin
      EmuWarning('D3DFMT_P8 is an unsupported texture format! Allocating D3DFMT_L8');
      Result := D3DFMT_L8;
    end;
    D3DFMT_D16:
    begin
      EmuWarning('D3DFMT_D16 is an unsupported texture format!');
      if aResourceType = D3DRTYPE_TEXTURE then
        Result := D3DFMT_R5G6B5
      else
        // D3DRTYPE_VOLUMETEXTURE, D3DRTYPE_CUBETEXTURE
        Result := D3DFMT_X8R8G8B8; // also CheckDeviceMultiSampleType
    end;
    D3DFMT_D24S8:
    begin
      EmuWarning('D3DFMT_D24S8 is an unsupported texture format! Allocating D3DFMT_X8R8G8B8');
      Result := D3DFMT_X8R8G8B8;
    end;
    D3DFMT_YUY2:
    begin
      if aResourceType = D3DRTYPE_CUBETEXTURE then
        DxbxKrnlCleanup('YUV not supported for cube textures');
    end;
  else
    if Assigned(CacheFormat) then
      CacheFormat^ := X_D3DFMT_UNKNOWN; // Means 'not important' as it's not used in DxbxUpdatePixelContainer
  end;
end;

// TODO : Move to appropriate unit :
procedure EmuXB2PC_D3DSURFACE_DESC(const SurfaceDesc: PX_D3DSURFACE_DESC; const pDesc: PD3DSURFACE_DESC; const CallerName: string);
begin
  // Convert Format (Xbox->PC)
  pDesc.Format := EmuXB2PC_D3DFormat(SurfaceDesc.Format);
  pDesc._Type := D3DRESOURCETYPE(SurfaceDesc.Type_);

//  if (Ord(pDesc.Type_) > 7) then
//    DxbxKrnlCleanup(CallerName + ': pDesc->Type > 7');

  pDesc.Usage := SurfaceDesc.Usage;
{$IFNDEF DXBX_USE_D3D9}
  pDesc.Size := GetSurfaceSize(@SurfaceDesc);
{$ENDIF}
  pDesc.MultiSampleType := EmuXB2PC_D3DMULTISAMPLE_TYPE(SurfaceDesc.MultiSampleType);
  pDesc.Width  := SurfaceDesc.Width;
  pDesc.Height := SurfaceDesc.Height;
end;

// TODO : Move to appropriate unit :
procedure EmuPC2XB_D3DSURFACE_DESC(const SurfaceDesc: D3DSURFACE_DESC; const pDesc: PX_D3DSURFACE_DESC; const CallerName: string);
begin
  // Convert Format (PC->Xbox)
  pDesc.Format := EmuPC2XB_D3DFormat(SurfaceDesc.Format);
  pDesc.Type_ := X_D3DRESOURCETYPE(SurfaceDesc._Type);

  if (Ord(pDesc.Type_) > 7) then
    DxbxKrnlCleanup(CallerName + ': pDesc->Type > 7');

  pDesc.Usage := SurfaceDesc.Usage;
  pDesc.Size := GetSurfaceSize(@SurfaceDesc);
  pDesc.MultiSampleType := EmuPC2XB_D3DMULTISAMPLE_TYPE(SurfaceDesc.MultiSampleType);
  pDesc.Width  := SurfaceDesc.Width;
  pDesc.Height := SurfaceDesc.Height;
end;

// TODO : Move to appropriate unit :
procedure EmuPC2XB_D3DVOLUME_DESC(const VolumeDesc: TD3DVolumeDesc; const pDesc: PX_D3DVOLUME_DESC; const CallerName: string);
begin
  // Convert Format (PC->Xbox)
  pDesc.Format := EmuPC2XB_D3DFormat(VolumeDesc.Format);
  pDesc.Type_ :=  X_D3DRESOURCETYPE(VolumeDesc._Type);

  if (Ord(pDesc.Type_) > 7) then
    DxbxKrnlCleanup(CallerName + ': pDesc->Type > 7');

  pDesc.Usage := VolumeDesc.Usage;
  pDesc.Size := GetVolumeSize(@VolumeDesc);
  pDesc.Width := VolumeDesc.Width;
  pDesc.Height := VolumeDesc.Height;
  pDesc.Depth := VolumeDesc.Depth;
end;

const
  MillisecondsPerSecond = 1000;

type
  DxbxTimer = record
    PerformanceCounterFrequency: LARGE_INTEGER;
    CurrentPerformanceCounter: LARGE_INTEGER;
    PreviousPerformanceCounter: LARGE_INTEGER;
    TicksBetweenTimeOuts: int;
    TicksPerSleep1: int;
    procedure InitTicks(const aTicksToWait: int);
    procedure InitMilliseconds(const aMillisecondsToWait: float);
    procedure InitFPS(const aFramesPerSecond: int);
    procedure Wait;
  end;

procedure DxbxTimer.InitTicks(const aTicksToWait: int);
begin
  QueryPerformanceFrequency({var}TLargeInteger(PerformanceCounterFrequency));
  QueryPerformanceCounter({var}TLargeInteger(CurrentPerformanceCounter));
  PreviousPerformanceCounter.QuadPart := 0;
  TicksBetweenTimeOuts := aTicksToWait;

  // Calculate how many ticks pass during Sleep(1), by using the Performance counter
  // frequency (which is measured in ticks per second) and dividing that by 1000
  // to get the number of ticks per millisecond (which is the unit of time Sleep
  // works with) :
  TicksPerSleep1 := int(PerformanceCounterFrequency.QuadPart div MillisecondsPerSecond);

  // However, Sleep(1) actually sleeps around 1-2 ms (*if* timeBeginPeriod(1) is called,
  // otherwise it would Sleep almost 10 ms!). If we worked with the lower bound, any deviation
  // would mean we would overshoot the waiting period. Using the average (1,5 ms) might sound
  // better, but still has the same behaviour. Only when we use the upper bound (2 ms), the risk
  // of overshooting our waiting period is reduced to near-never. So factor that in here :
  TicksPerSleep1 := TicksPerSleep1 * 2;

  // Raise the accuracy of Sleep to it's maximum :
  timeBeginPeriod(1);
  // Dxbx Note : We really shouldn't do this, as you can read about on
  // http://blogs.msdn.com/b/larryosterman/archive/2009/09/02/what-s-the-difference-between-gettickcount-and-timegettime.aspx
  // Instead, we could use NtDelayExecutionThread as an alternative (and hopefully accurate)
  // timing mechanism (see below).

  // TODO : Add a finalizer with timeEndPeriod(1);
end;

procedure DxbxTimer.InitMilliseconds(const aMillisecondsToWait: float);
begin
  // Determine the number of counts (ticks) per second :
  QueryPerformanceFrequency({var}TLargeInteger(PerformanceCounterFrequency));
  // Initialize the timer by calculating the number of ticks this takes :
  InitTicks(Trunc(aMillisecondsToWait * PerformanceCounterFrequency.QuadPart / MillisecondsPerSecond));
  // Say, there are 5,000,000 ticks per second, that would be 5,000 ticks per millisecond,
  // which would let 16.66666... milliseconds become 83,333 ticks.
end;

procedure DxbxTimer.InitFPS(const aFramesPerSecond: int);
begin
  // Initialize the FPS counter by calculating the (floating point) number of milliseconds that takes :
  InitMilliseconds(MillisecondsPerSecond / aFramesPerSecond); // 60 Hz becomes 16.66666... milliseconds
end;

procedure DxbxTimer.Wait;
var
  TicksPassed: int;
  TicksToWait: int;
//  i: int;
begin
  // Note : Even though we used timeBeginPeriod, Sleep is not really accurate.
  // So we do what VirtualDub does (http://www.virtualdub.org/blog/pivot/entry.php?id=272)
  // we make this thread run at THREAD_PRIORITY_HIGHEST, and take care of the frequency and
  // accuracy ourselves here.
  //
  // See http://www.geisswerks.com/ryan/FAQS/timing.html).

  if (PreviousPerformanceCounter.QuadPart <> 0) then
  begin
    while True do
    begin
      // Get current performance counter value :
      QueryPerformanceCounter({var}TLargeInteger(CurrentPerformanceCounter));

      // Calculate how many ticks have passed since last timer expiration :
      TicksPassed := int(CurrentPerformanceCounter.QuadPart - PreviousPerformanceCounter.QuadPart);

      // Safeguard against a time wrap (this can presumably happen sometimes with Performance counters) :
      if (TicksPassed <= 0) then
        Break;

      // Calculate how many ticks we still have to wait :
      TicksToWait := TicksBetweenTimeOuts - TicksPassed;
      // Stop this loop when the wait time's up :
      if (TicksToWait <= 0) then
        Break;

      // Check if the number of tick we still have to wait, is more than
      // the number of ticks that would pass when doing a Sleep(1) :
      if (TicksToWait > TicksPerSleep1) then
      begin
        // In this case, we can do the most efficient wait, by Sleep()ing
        // this thread, which costs the least amount of CPU overhead.
        // The number of milliseconds we should wait for can be calculated
        // by dividing the number of TicksToWait by the number of ticks
        // that passes during a Sleep(1). This way, we Sleep as long as we
        // possibly can, without risking 'overshooting' our waiting period.
        // To be safe, we *do* safeguard against EDivByZero, by testing
        // that the previously calculated TicksPerSleep1 is indeed positive :
        if TicksPerSleep1 > 0 then
          Sleep(TicksToWait div TicksPerSleep1)
        else
          // If somehow TicksPerSleep1 became 0 (or less!), just Sleep
          // 1 millisecond (or 2, depending on the system's deviation),
          // which results in a maximum frequency of about 500 updates
          // per second. The alternative Sleep(0), as done below, eats
          // up way to much CPU time, so that's not an option here.
          Sleep(1);
      end
      else
        // If the TicksToWait is less then the time for a Sleep(1),
        // then just give up this timeslice. If this happens a lot,
        // it will incur much CPU usage (with all the thread switching
        // going on), but it would at least spend a tiny amount of time:
        Sleep(0); // This causes thread to give up its timeslice
        // TODO : What about pausing the cpu a little (rep nop) instead?
    end; // while True
  end;

  PreviousPerformanceCounter := CurrentPerformanceCounter;
end; // Wait

// Direct3D initialization (called before emulation begins)
procedure XTL_EmuD3DInit(XbeHeader: PXBEIMAGE_HEADER; XbeHeaderSize: uint32); {NOPATCH}
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  dwThreadId: DWORD;
  hThread: HANDLE;
  hDupHandle: HANDLE;
  DevType: D3DDEVTYPE;
  Identifier: D3DADAPTER_IDENTIFIER;
  PresParam: X_D3DPRESENT_PARAMETERS;
begin
  g_EmuShared.GetXBVideo(@g_XBVideo);

  if (g_XBVideo.GetFullscreen()) then
    DxbxKrnl_hEmuParent := 0;

  // Dxbx addition : Disable DivByZero exceptions :
  Set8087CW(Get8087CW() or $0004);

  // cache XbeHeader and size of XbeHeader
  g_XbeHeader := XbeHeader;
  g_XbeHeaderSize := XbeHeaderSize;

  // create timing thread
  begin
    dwThreadId := 0;
    hThread := CreateThread(nil, 0, @EmuThreadUpdateTickCount, nil, 0, {var}dwThreadId);
    // If possible, assign this thread to another core than the one that runs Xbox1 code :
    SetThreadAffinityMask(dwThreadId, g_CPUOthers);
    // We set the priority of this thread a bit higher, to assure reliable timing :
    SetThreadPriority(hThread, THREAD_PRIORITY_ABOVE_NORMAL);

    // we must duplicate this handle in order to retain Suspend/Resume thread rights from a remote thread
    begin
      hDupHandle := 0;

      DuplicateHandle(GetCurrentProcess(), hThread, GetCurrentProcess(), @hDupHandle, 0, False, DUPLICATE_SAME_ACCESS);

      DxbxKrnlRegisterThread(hDupHandle);
    end;
  end;

  // create input handling thread
  begin
    dwThreadId := 0;
    {hThread :=} CreateThread(nil, 0, @EmuThreadPollInput, nil, 0, {var}dwThreadId);
    // If possible, assign this thread to another core than the one that runs Xbox1 code :
    SetThreadAffinityMask(dwThreadId, g_CPUOthers);
  end;

  // create vblank handling thread
  begin
    dwThreadId := 0;
    {hThread :=} CreateThread(nil, 0, @EmuThreadHandleVBlank, nil, 0, {var}dwThreadId);
    // Make sure VBlank callbacks run on the same core as the one that runs Xbox1 code :
    SetThreadAffinityMask(dwThreadId, g_CPUXbox);
  end;

  // create the create device proxy thread
  begin
    dwThreadId := 0;
    CreateThread(nil, 0, @EmuThreadCreateDeviceProxy, nil, 0, {var}dwThreadId);
    // If possible, assign this thread to another core than the one that runs Xbox1 code :
    SetThreadAffinityMask(dwThreadId, g_CPUOthers);
  end;

  // create window message processing thread
  begin
    g_bRenderWindowActive := false;

    dwThreadId := 0;
    {hThread :=} CreateThread(nil, 0, @EmuThreadRenderWindow, nil, 0, {var}dwThreadId);
    // If possible, assign this thread to another core than the one that runs Xbox1 code :
    SetThreadAffinityMask(dwThreadId, g_CPUOthers);

    while not g_bRenderWindowActive do
      Sleep(10); // Dxbx: Should we use SwitchToThread() or YieldProcessor() ?

    Sleep(50);
  end;

  // create Direct3D8 and retrieve caps
  begin
    //  using namespace XTL;

    // xbox Direct3DCreate8 returns '1' always, so we need our own ptr
{$IFDEF DXBX_USE_D3D9}
    g_pD3D := Direct3DCreate9(D3D_SDK_VERSION);
{$ELSE}
    g_pD3D := Direct3DCreate8(D3D_SDK_VERSION);
{$ENDIF}
    if (g_pD3D = NULL) then
      DxbxKrnlCleanup('Could not initialize Direct3D!');

    // Show adapter identifier :
    if FAILED(g_pD3D.GetAdapterIdentifier(g_XBVideo.GetDisplayAdapter(), {Flags=}0, {out}Identifier)) then
      DbgPrintf('Could not get adapter information')
    else
      LogBegin('Adapter identifier').
        _(AnsiString(Identifier.Driver), 'Driver').
        _(AnsiString(Identifier.Description), 'Description').
        _(Identifier.DriverVersionLowPart, 'DriverVersionLowPart').
        _(Identifier.DriverVersionHighPart, 'DriverVersionHighPart').
        _(Identifier.VendorId, 'VendorId').
        _(Identifier.DeviceId, 'DeviceId').
        _(Identifier.SubSysId, 'SubSysId').
        _(Identifier.Revision, 'Revision').
//      _(Identifier.DeviceIdentifier, 'DeviceIdentifier'). // TODO : Add GUID logging
        _(Identifier.WHQLLevel, 'WHQLLevel').
      LogEnd();

    // Show native device capabilities :
    begin
      DevType := iif(g_XBVideo.GetDirect3DDevice() = 0, D3DDEVTYPE_HAL, D3DDEVTYPE_REF);
      g_pD3D.GetDeviceCaps(g_XBVideo.GetDisplayAdapter(), DevType, {out}g_D3DCaps);
      DxbxDumpD3DCaps(@g_D3DCaps, {FromXbox=}False);
    end;

    // Show Xbox device capabilities :
    if Addr(XTL_Direct3D_GetDeviceCaps) <> nil then
    begin
      XTL_Direct3D_GetDeviceCaps(@g_XboxCaps);
      DxbxDumpD3DCaps(@g_XboxCaps, {FromXbox=}True);
    end
    else
      DbgPrintf('Could not get Xbox device caps!');

  end;

  SetFocus(g_hEmuWindow);

  // create default device
  begin
    ZeroMemory(@PresParam, sizeof(PresParam));

    PresParam.BackBufferWidth := 640;
    PresParam.BackBufferHeight := 480;
    PresParam.BackBufferFormat := X_D3DFMT_A8R8G8B8; //=6
    PresParam.BackBufferCount := 1;
    PresParam.MultiSampleType := X_D3DMULTISAMPLE_NONE; // =$0011;
    PresParam.SwapEffect := D3DSWAPEFFECT_DISCARD;
    PresParam.EnableAutoDepthStencil := BOOL_TRUE;
    PresParam.AutoDepthStencilFormat := X_D3DFMT_D24S8; //=$2A

    EmuSwapFS(fsXbox);

{$IFDEF DXBX_TRY_DEEPER_DEVICE_INIT}
    // Initialize Xbox device :
    if Assigned(Addr(XTL_Direct3D_CreateDevice)) then
    begin
      PPointer(XTL_D3D__Device)^ := AllocMem(9000); // Reserve enough memory for D3D_Device (TODO : How much exactly?)

      // Call Xbox version (or our patch)
      XTL_Direct3D_CreateDevice(
        0,
        D3DDEVTYPE_HAL,
        {ignored hFocusWindow=}0,
        {ignored BehaviorFlags=}D3DCREATE_HARDWARE_VERTEXPROCESSING, // = $00000040
        @PresParam,
        @g_pD3DDevice);
    end
    else
{$ENDIF DXBX_TRY_DEEPER_DEVICE_INIT}
      // Call our patched version
      XTL_EmuDirect3D_CreateDevice(
        0,
        D3DDEVTYPE_HAL,
        {ignored hFocusWindow=}0,
        {ignored BehaviorFlags=}D3DCREATE_HARDWARE_VERTEXPROCESSING, // = $00000040
        @PresParam,
        @g_pD3DDevice);

    EmuSwapFS(fsWindows);
  end;
end; // XTL_EmuD3DInit

//// cleanup Direct3D
//procedure XTL_EmuD3DCleanup(); {NOPATCH}
//// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
//begin
//  XTL_EmuDInputCleanup();
//end;

// enumeration procedure for locating display device GUIDs
{static}var dwEnumCount: DWORD = 0;
function EmuEnumDisplayDevices(lpGUID: PGUID; lpDriverDescription: LPSTR;
  lpDriverName: LPSTR; lpContext: LPVOID; hm: HMONITOR): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  Inc(dwEnumCount); // Cxbx uses post-increment and compares+1 :
  if (dwEnumCount = g_XBVideo.GetDisplayAdapter()) then
  begin
    g_hMonitor := hm;
    dwEnumCount := 0;
    if (lpGUID <> nil) then
    begin
      memcpy(@g_ddguid, lpGUID, sizeof(GUID))
    end
    else
    begin
      memset(@g_ddguid, 0, sizeof(GUID));
    end;

    Result := BOOL_FALSE;
    Exit;
  end;

  Result := BOOL_TRUE;
end; // EmuEnumDisplayDevices

// window message processing thread
function EmuThreadRenderWindow(lpVoid: LPVOID): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
const
  IDI_CXBX = 101;
  DXBX_RENDER_CLASS = 'DxbxRender';
var
  msg: TMsg;
  AsciiTitle: string;
  hDxbxDLL: HMODULE;
  logBrush: TLogBrush;
  wc: WNDCLASSEX;
  CertAddr: IntPtr; //uint32
  XbeCert: PXbe_Certificate;
  dwStyle: DWORD;
  nTitleHeight: Integer;
  nBorderWidth: Integer;
  nBorderHeight: Integer;
  x, y, nWidth, nHeight: Integer;
  hwndParent: HWND;
  UpdateTimer: DxbxTimer;
begin
  // register window class
  begin
    hDxbxDLL := HInstance; // = $10000000 (ImageBase of DxbxKrnl.DLL)

    logBrush.lbStyle := BS_SOLID;
    logBrush.lbColor := RGB(0, 0, 0);
    logBrush.lbHatch := 0;

    g_hBgBrush := CreateBrushIndirect(logBrush);

    ZeroMemory(@wc, SizeOf(wc));
    wc.cbSize := sizeof(WNDCLASSEX);
    wc.style := CS_CLASSDC;
    wc.lpfnWndProc := @EmuMsgProc;
    wc.cbClsExtra := 0;
    wc.cbWndExtra := 0;
    wc.hInstance := GetModuleHandle(NULL); // = $00010000 (ImageBase of Dxbx.exe)
    wc.hIcon := LoadIcon(hDxbxDll, MAKEINTRESOURCE(IDI_CXBX));
    wc.hCursor := LoadCursor(0, IDC_ARROW);
    wc.hbrBackground := g_hBgBrush;
    wc.lpszMenuName := NULL;
    wc.lpszClassName := DXBX_RENDER_CLASS;
    wc.hIconSm := 0;

    {Ignore ATOM:}RegisterClassEx(wc);
  end;

  // retrieve Xbe title (if possible)
  begin
    CertAddr := g_XbeHeader.dwCertificateAddr - g_XbeHeader.dwBaseAddr;
    IntPtr(XbeCert) := IntPtr(g_XbeHeader) + CertAddr;
    g_Title := GetReadableTitle(XbeCert);
    AsciiTitle := 'Dxbx: Emulating ' + g_Title;
  end;

  // create the window
  begin
    dwStyle := iif(g_XBVideo.GetFullscreen() or (DxbxKrnl_hEmuParent = 0), WS_OVERLAPPEDWINDOW, WS_CHILD);

    nTitleHeight := GetSystemMetrics(SM_CYCAPTION);
    nBorderWidth := GetSystemMetrics(SM_CXSIZEFRAME);
    nBorderHeight := GetSystemMetrics(SM_CYSIZEFRAME);

    x := 100; y := 100; nWidth := 640; nHeight := 480;

    Inc(nWidth, nBorderWidth * 2);
    Inc(nHeight, (nBorderHeight * 2) + nTitleHeight);

    sscanf(g_XBVideo.GetVideoResolution(), '%d x %d', [@nWidth, @nHeight]);

    if g_XBVideo.GetFullscreen() then
    begin
      x := 0; y := 0; nWidth := 0; nHeight := 0;
      dwStyle := WS_POPUP;
    end;

    hwndParent := GetDesktopWindow();
    if not g_XBVideo.GetFullscreen() then
    begin
      hwndParent := DxbxKrnl_hEmuParent;
    end;

    g_hEmuWindow := CreateWindow
    (
      DXBX_RENDER_CLASS, PChar(AsciiTitle),
      dwStyle, x, y, nWidth, nHeight,
      hwndParent, HMENU(0), GetModuleHandle(NULL), NULL
    );
  end;

  ShowWindow(g_hEmuWindow, iif((DxbxKrnl_hEmuParent = 0) or g_XBVideo.GetFullscreen(), SW_SHOWDEFAULT, SW_SHOWMAXIMIZED));
  UpdateWindow(g_hEmuWindow);

  // Dxbx addition, to notify the main Dxbx GUI we're here (I'm using a user message id here,
  // as WM_PARENTNOTIFY doesn't reach the GUI somehow?) :
  if DxbxKrnl_hEmuParent <> 0 then
    SendMessage(DxbxKrnl_hEmuParent, WM_USER_PARENTNOTIFY, WM_CREATE, 0);

  if (not g_XBVideo.GetFullscreen()) and (DxbxKrnl_hEmuParent <> 0) then
  begin
    SetFocus(DxbxKrnl_hEmuParent);
  end;

  // initialize direct input
  if not EmuDInputInit() then
    DxbxKrnlCleanup('Could not initialize DirectInput!');

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : Message-Pump thread is running.');

  SetFocus(g_hEmuWindow);

  // message processing loop
  begin
    ZeroMemory(@msg, sizeof(msg));

    // TODO : Should we temporarily disable logging ?

    g_bRenderWindowActive := true;

    UpdateTimer.InitFPS(10); // Poll messages at least 10 times per second

    while msg.message <> WM_QUIT do
    begin
      if PeekMessage({var}msg, 0, 0, 0, PM_REMOVE) then
      begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end
      else
      begin
        UpdateTimer.Wait;
        // TODO : Should we restore logging ?
      end;
    end;

    g_bRenderWindowActive := false;

    DxbxKrnlCleanup('');
  end;

  Result := D3D_OK;
end; // EmuThreadRenderWindow

// simple helper function
{static}var lRestore: LONG = 0; lRestoreEx: LONG = 0;
{static}var lRect: TRect = ();
procedure ToggleFauxFullscreen(hWnd: HWND);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if (g_XBVideo.GetFullscreen()) then
    Exit;

  if (not g_bIsFauxFullscreen) then
  begin
    if (DxbxKrnl_hEmuParent <> 0) then
    begin
      SetParent(hWnd, 0);
    end
    else
    begin
      lRestore := GetWindowLong(hWnd, GWL_STYLE);
      lRestoreEx := GetWindowLong(hWnd, GWL_EXSTYLE);

      GetWindowRect(hWnd, {var}lRect);
    end;

    SetWindowLong(hWnd, GWL_STYLE, LongInt(WS_POPUP));
    ShowWindow(hWnd, SW_MAXIMIZE);
    SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
  end
  else
  begin
    if DxbxKrnl_hEmuParent <> 0 then
    begin
      SetParent(hWnd, DxbxKrnl_hEmuParent);
      SetWindowLong(hWnd, GWL_STYLE, WS_CHILD);
      ShowWindow(hWnd, SW_MAXIMIZE);
      SetFocus(DxbxKrnl_hEmuParent);
    end
    else
    begin
      SetWindowLong(hWnd, GWL_STYLE, lRestore);
      SetWindowLong(hWnd, GWL_EXSTYLE, lRestoreEx);
      ShowWindow(hWnd, SW_RESTORE);
      SetWindowPos(hWnd, HWND_NOTOPMOST, lRect.left, lRect.top, lRect.right - lRect.left, lRect.bottom - lRect.top, 0);
      SetFocus(hWnd);
    end;
  end;

  g_bIsFauxFullscreen := not g_bIsFauxFullscreen;
end;

// rendering window message procedure
{static}var bAutoPaused: _bool = false;
function EmuMsgProc(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  case (msg) of
    WM_DESTROY:
      begin
        DeleteObject(g_hBgBrush);
        PostQuitMessage(0);
        Result := D3D_OK;
        Exit;
      end;

    WM_SYSKEYDOWN: // Alt pressed +
      begin
        if (wParam = VK_RETURN) then
        begin
          ToggleFauxFullscreen(hWnd);
        end
        else if (wParam = VK_F4) then
        begin
          PostMessage(hWnd, WM_CLOSE, 0, 0);
        end;
      end;

    WM_KEYDOWN: // Normal key press :
      case wParam of
        VK_ESCAPE:
        begin
          // disable fullscreen if we are set to faux mode, and faux fullscreen is active
          if (g_XBVideo.GetFullscreen()) then
          begin
            SendMessage(hWnd, WM_CLOSE, 0, 0);
          end
          else if (g_bIsFauxFullscreen) then
          begin
            ToggleFauxFullscreen(hWnd);
          end;
        end;
        VK_F3:
        begin
          g_XBSound.SetMute(not g_XBSound.GetMute());
        end;
        VK_F8:
        begin
          uLog.ToggleLogging;
        end;
        VK_F9:
        begin
          XTL_g_bBrkPush := TRUE;
        end;
        VK_F10:
        begin
          ToggleFauxFullscreen(hWnd);
        end;
        VK_F11:
        begin
          Inc(g_iWireframe); // Cxbx uses post-increment and compares+1 :
          if g_iWireframe = 3 then
            g_iWireframe := 0;
        end;
        VK_F12: // VK_SNAPSHOT
        begin
          // Debugging not used, and PrintScreen key can't be detected somehow, so use F12 for taking screenshotss :
          // XTL_g_bStepPush := not XTL_g_bStepPush;
          g_SignalScreenShot := True; // Will be seen at next Present()
        end;
      end; // case

    WM_SIZE:
      begin
        case (wParam) of
          SIZE_RESTORED,
          SIZE_MAXIMIZED:
            begin
              if (bAutoPaused) then
              begin
                bAutoPaused := false;
                DxbxKrnlResume();
              end;
            end;

          SIZE_MINIMIZED:
            begin
              if (g_XBVideo.GetFullscreen()) then
                DxbxKrnlCleanup('');

              if (not g_bEmuSuspended) then
              begin
                bAutoPaused := true;
                DxbxKrnlSuspend();
              end;
            end;
        end;
      end;

    WM_CLOSE:
      DestroyWindow(hWnd);

    WM_SETFOCUS:
      begin
        if (DxbxKrnl_hEmuParent <> 0) then
        begin
          SetFocus(DxbxKrnl_hEmuParent);
        end;
      end;

    WM_SETCURSOR:
      begin
        if (g_XBVideo.GetFullscreen() or g_bIsFauxFullscreen) then
        begin
          SetCursor(0);
          Result := D3D_OK;
          Exit;
        end;

        Result := DefWindowProc(hWnd, msg, wParam, lParam);
        Exit;
      end;
  else
    Result := DefWindowProc(hWnd, msg, wParam, lParam);
    Exit;
  end;

  Result := D3D_OK;
end; // EmuMsgProc

// timing thread procedure
function EmuThreadUpdateTickCount(lpVoid: LPVOID): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  UpdateTimer: DxbxTimer;
begin
  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : Update timer thread is running.');

  // Update the Xbox TickCount as often as reasonable, without swamping the CPU;
  UpdateTimer.InitFPS(100); // 100 updates per second should be enough

  while true do // TODO -oDxbx: When do we break out of this while loop ?
  begin
    UpdateTimer.Wait;

    // TODO -oDxbx : Perhaps we should put this update somewhere else,
    // like in EmuSwapFS(), so we can get rid of this entire timer thread ?
    xboxkrnl_KeTickCount := DxbxXboxGetTickCount();
  end; // while
end; // EmuThreadUpdateTickCount


function EmuThreadPollInput(lpVoid: LPVOID): DWORD; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  UpdateTimer: DxbxTimer;
  v: int;
  hDevice: HANDLE;
  dwLatency: DWORD;
  pFeedback: PXINPUT_FEEDBACK;
begin
  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : Input polling thread is running.');

  UpdateTimer.InitFPS(20); // Poll input 20 times per second

  while True do // TODO -oDxbx: When do we break out of this while loop ?
  begin
    UpdateTimer.Wait;

    // Poll input
    for v := 0 to XINPUT_SETSTATE_SLOTS-1 do
    begin
      hDevice := g_pXInputSetStateStatus[v].hDevice;

      if hDevice = 0 then
        continue;

      dwLatency := g_pXInputSetStateStatus[v].dwLatency;
      Inc(g_pXInputSetStateStatus[v].dwLatency);

      if (dwLatency < XINPUT_SETSTATE_LATENCY) then
        continue;

      g_pXInputSetStateStatus[v].dwLatency := 0;

      pFeedback := PXINPUT_FEEDBACK(g_pXInputSetStateStatus[v].pFeedback);
      if pFeedback = nil then
        continue;

      // Only update slot if it has not already been updated
      if pFeedback.Header.dwStatus <> ERROR_SUCCESS then
      begin
        if pFeedback.Header.hEvent <> 0 then
        begin
          SetEvent(pFeedback.Header.hEvent);
        end;

        pFeedback.Header.dwStatus := ERROR_SUCCESS;
      end;
    end; // for
  end; // while True
end; // EmuThreadPollInput


// thread dedicated to updating VBlank
function EmuThreadHandleVBlank(lpVoid: LPVOID): DWORD; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  UpdateTimer: DxbxTimer;
  pCertificate: PXBE_CERTIFICATE;
begin
  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : VBlank handling thread is running.');

  pCertificate := PXBE_CERTIFICATE(DxbxKrnl_XbeHeader.dwCertificateAddr);


  // since callbacks come from here
  EmuGenerateFS(DxbxKrnl_TLS, DxbxKrnl_TLSData);

  UpdateTimer.InitFPS(GameDisplayFrequency(pCertificate.dwGameRegion));

  while True do // TODO -oDxbx: When do we break out of this while loop ?
  begin
    UpdateTimer.Wait;

    // Handle VBlank functionality
    if g_bRenderWindowActive then
    begin
      Inc(g_VBData.VBlankCounter);

      // TODO -oCXBX: Fixme.  This may not be right...
      g_SwapData.SwapVBlank := 1;

      // Trigger VBlank callback
      if (Addr(g_pVBCallback) <> NULL) then
      begin
        EmuSwapFS(fsXbox);
        g_pVBCallback(@g_VBData);
        EmuSwapFS(fsWindows);
      end;

      // Reset Swap counter :
      g_VBData.SwapCounter := 0;

      // TODO -oCxbx: This can't be accurate...
      g_SwapData.TimeUntilSwapVBlank := 0;

      // TODO -oCxbx: we should check the D3DPRESENT_INTERVAL value for accurracy.
      g_SwapData.TimeBetweenSwapVBlanks := MillisecondsPerSecond div GameDisplayFrequency(pCertificate.dwGameRegion);
    end;
  end; // while True
end; // EmuThreadHandleVBlank


// thread dedicated to create devices
function EmuThreadCreateDeviceProxy(lpVoid: LPVOID): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  D3DDisplayMode: TD3DDisplayMode; // X_D3DDISPLAYMODE; // TODO -oDXBX: : What type should we use?
  szBackBufferFormat: array [0..16 - 1] of AnsiChar;
  PresentationInterval: LongWord;
  hRet: HRESULT;
  dwCodes: DWORD;
  lpCodes: PDWORD;
  v: DWORD;
  ddsd2: DDSURFACEDESC2;
  Streams: int;
begin
  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : CreateDevice proxy thread is running.');

  while (true) do
  begin
    // if we have been signalled, create the device with cached parameters
    if(g_EmuCDPD.bReady) then
    begin
      if MayLog(lfUnit) then
        DbgPrintf('EmuD3D8 : CreateDevice proxy thread received request.');

      if (g_EmuCDPD.bCreate) then
      begin
        // only one device should be created at once
        // TODO -oCXBX: ensure all surfaces are somehow cleaned up?
        if (g_pD3DDevice <> nil) then
        begin
          if MayLog(lfUnit) then
            DbgPrintf('EmuD3D8 : CreateDevice proxy thread releasing old Device.');

          g_pD3DDevice.EndScene();

          while (g_pD3DDevice._Release() > 0) do ;

          Pointer(g_pD3DDevice) := nil; // Cast to prevent automatic refcounting from clearing the object (we did that above)
          g_pDummyBuffer := nil;
        end;

        if (PX_D3DPRESENT_PARAMETERS(g_EmuCDPD.pPresentationParameters).BufferSurfaces[0] <> NULL) then
          EmuWarning('BufferSurfaces[0]: 0x%.08X', [PX_D3DPRESENT_PARAMETERS(g_EmuCDPD.pPresentationParameters).BufferSurfaces[0]]);

        if (PX_D3DPRESENT_PARAMETERS(g_EmuCDPD.pPresentationParameters).DepthStencilSurface <> NULL) then
          EmuWarning('DepthStencilSurface: 0x%.08X', [PX_D3DPRESENT_PARAMETERS(g_EmuCDPD.pPresentationParameters).DepthStencilSurface]);

        // make adjustments to parameters to make sense with windows Direct3D
        begin
          g_EmuCDPD.CreationParameters.AdapterOrdinal := g_XBVideo.GetDisplayAdapter();
          g_EmuCDPD.CreationParameters.DeviceType := iif(g_XBVideo.GetDirect3DDevice() = 0, D3DDEVTYPE_HAL, D3DDEVTYPE_REF);
          // Note: Instead of the hFocusWindow argument, we use the global g_hEmuWindow here:
          g_EmuCDPD.CreationParameters.hFocusWindow := g_hEmuWindow;
          // g_EmuCDPD.CreationParameters.BehaviorFlags := ?;

          // retrieve resolution from configuration
          g_EmuCDPD.NativePresentationParameters.BackBufferWidth := g_EmuCDPD.pPresentationParameters.BackBufferWidth;
          g_EmuCDPD.NativePresentationParameters.BackBufferHeight := g_EmuCDPD.pPresentationParameters.BackBufferHeight;

          g_EmuCDPD.NativePresentationParameters.Windowed := not g_XBVideo.GetFullscreen();
          if (g_EmuCDPD.NativePresentationParameters.Windowed) then
          begin
            sscanf(g_XBVideo.GetVideoResolution(), '%d x %d', [
              @(g_EmuCDPD.NativePresentationParameters.BackBufferWidth),
              @(g_EmuCDPD.NativePresentationParameters.BackBufferHeight)]);

            g_pD3D.GetAdapterDisplayMode(g_XBVideo.GetDisplayAdapter(), {out}D3DDisplayMode);

            g_EmuCDPD.NativePresentationParameters.BackBufferFormat := D3DDisplayMode.Format;
              // TODO -oDxbx : Why does BenchMark crash with this?
              //EmuXB2PC_D3DFormat(g_EmuCDPD.pPresentationParameters.BackBufferFormat);

            g_EmuCDPD.NativePresentationParameters.FullScreen_RefreshRateInHz := 0; // ??
          end
          else
          begin
            sscanf(g_XBVideo.GetVideoResolution(), '%d x %d %*dbit %s (%d hz)', [
              @(g_EmuCDPD.NativePresentationParameters.BackBufferWidth),
              @(g_EmuCDPD.NativePresentationParameters.BackBufferHeight),
              @(szBackBufferFormat[0]),
              @(g_EmuCDPD.NativePresentationParameters.FullScreen_RefreshRateInHz)]);

            if (strcmp(szBackBufferFormat, 'x1r5g5b5') = 0) then
              g_EmuCDPD.NativePresentationParameters.BackBufferFormat := D3DFMT_X1R5G5B5
            else if (strcmp(szBackBufferFormat, 'r5g6r5') = 0) then
              g_EmuCDPD.NativePresentationParameters.BackBufferFormat := D3DFMT_R5G6B5
            else if (strcmp(szBackBufferFormat, 'x8r8g8b8') = 0) then
              g_EmuCDPD.NativePresentationParameters.BackBufferFormat := D3DFMT_X8R8G8B8
            else //if (strcmp(szBackBufferFormat, 'a8r8g8b8') = 0) then
              g_EmuCDPD.NativePresentationParameters.BackBufferFormat := D3DFMT_A8R8G8B8
//            else
//              g_EmuCDPD.NativePresentationParameters.BackBufferFormat := EmuXB2PC_D3DFormat(g_EmuCDPD.pPresentationParameters.BackBufferFormat);
          end;

          if (g_XBVideo.GetVSync()) then
            g_EmuCDPD.NativePresentationParameters.SwapEffect := {$IFDEF DXBX_USE_D3D9}D3DSWAPEFFECT_COPY{$ELSE}D3DSWAPEFFECT_COPY_VSYNC{$ENDIF}
          else
            g_EmuCDPD.NativePresentationParameters.SwapEffect := g_EmuCDPD.pPresentationParameters.SwapEffect;

          // MultiSampleType may only be set if SwapEffect = D3DSWAPEFFECT_DISCARD :
          if g_EmuCDPD.NativePresentationParameters.SwapEffect = D3DSWAPEFFECT_DISCARD then
            g_EmuCDPD.NativePresentationParameters.MultiSampleType := EmuXB2PC_D3DMULTISAMPLE_TYPE(g_EmuCDPD.pPresentationParameters.MultiSampleType)
          else
            g_EmuCDPD.NativePresentationParameters.MultiSampleType := D3DMULTISAMPLE_NONE;

          // Set BackBufferCount (if this is too much, CreateDevice will change this into the allowed maximum,
          // so a second call should succeed) :
          g_EmuCDPD.NativePresentationParameters.BackBufferCount := g_EmuCDPD.pPresentationParameters.BackBufferCount;
          if g_EmuCDPD.NativePresentationParameters.SwapEffect = D3DSWAPEFFECT_COPY then
          begin
            EmuWarning('Limiting BackBufferCount to 1 because of D3DSWAPEFFECT_COPY...');
            g_EmuCDPD.NativePresentationParameters.BackBufferCount := 1;
          end;

          g_EmuCDPD.NativePresentationParameters.hDeviceWindow := g_EmuCDPD.pPresentationParameters.hDeviceWindow;
          g_EmuCDPD.NativePresentationParameters.EnableAutoDepthStencil := g_EmuCDPD.pPresentationParameters.EnableAutoDepthStencil <> BOOL_FALSE;
          g_EmuCDPD.NativePresentationParameters.AutoDepthStencilFormat := EmuXB2PC_D3DFormat(g_EmuCDPD.pPresentationParameters.AutoDepthStencilFormat);
          g_EmuCDPD.NativePresentationParameters.Flags := D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;

          if (not g_XBVideo.GetVSync() and ((g_D3DCaps.PresentationIntervals and D3DPRESENT_INTERVAL_IMMEDIATE) > 0) and g_XBVideo.GetFullscreen()) then
            PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE
          else
          begin
            if ((g_D3DCaps.PresentationIntervals and D3DPRESENT_INTERVAL_ONE) > 0) and g_XBVideo.GetFullscreen() then
              PresentationInterval := D3DPRESENT_INTERVAL_ONE
            else
              PresentationInterval := D3DPRESENT_INTERVAL_DEFAULT;
          end;

{$IFDEF DXBX_USE_D3D9}
          g_EmuCDPD.NativePresentationParameters.PresentationInterval := PresentationInterval;
{$ELSE}
          g_EmuCDPD.NativePresentationParameters.FullScreen_PresentationInterval := PresentationInterval;
{$ENDIF}
        end;

        // detect vertex processing capabilities
        if ((g_D3DCaps.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT) > 0) and (g_EmuCDPD.CreationParameters.DeviceType = D3DDEVTYPE_HAL) then
        begin
          if MayLog(lfUnit) then
            DbgPrintf('EmuD3D8 : Using hardware vertex processing');

          g_EmuCDPD.CreationParameters.BehaviorFlags := D3DCREATE_HARDWARE_VERTEXPROCESSING;
          g_dwVertexShaderUsage := 0;
        end
        else
        begin
          if MayLog(lfUnit) then
            DbgPrintf('EmuD3D8 : Using software vertex processing');

          g_EmuCDPD.CreationParameters.BehaviorFlags := D3DCREATE_SOFTWARE_VERTEXPROCESSING;
          g_dwVertexShaderUsage := D3DUSAGE_SOFTWAREPROCESSING;
        end;
// [PatrickvL] Reviewed up to here
        // Dxbx addition : Prevent Direct3D from changing the FPU Control word :
        g_EmuCDPD.CreationParameters.BehaviorFlags := g_EmuCDPD.CreationParameters.BehaviorFlags or D3DCREATE_FPU_PRESERVE;

        // redirect to windows Direct3D
        g_EmuCDPD.hRet := g_pD3D.CreateDevice
        (
          g_EmuCDPD.CreationParameters.AdapterOrdinal,
          g_EmuCDPD.CreationParameters.DeviceType,
          g_EmuCDPD.CreationParameters.hFocusWindow,
          g_EmuCDPD.CreationParameters.BehaviorFlags,
          @g_EmuCDPD.NativePresentationParameters,
          PIDirect3DDevice(g_EmuCDPD.ppReturnedDeviceInterface)
        );

        // See if the BackBufferCount was too large - retry if it was updated :
        if  (FAILED(g_EmuCDPD.hRet))
        and (g_EmuCDPD.NativePresentationParameters.SwapEffect <> D3DSWAPEFFECT_COPY)
        and (g_EmuCDPD.NativePresentationParameters.BackBufferCount <> g_EmuCDPD.pPresentationParameters.BackBufferCount) then
        begin
          EmuWarning('BackBufferCount too large! D3D changed it to %d, so retrying...', [g_EmuCDPD.NativePresentationParameters.BackBufferCount]);
          g_EmuCDPD.hRet := g_pD3D.CreateDevice
          (
            g_EmuCDPD.CreationParameters.AdapterOrdinal,
            g_EmuCDPD.CreationParameters.DeviceType,
            g_EmuCDPD.CreationParameters.hFocusWindow,
            g_EmuCDPD.CreationParameters.BehaviorFlags,
            @g_EmuCDPD.NativePresentationParameters,
            PIDirect3DDevice(g_EmuCDPD.ppReturnedDeviceInterface)
          );
        end;

        // report error
        if (FAILED(g_EmuCDPD.hRet)) then
        begin
          // TODO -oDXBX: Use DXGetErrorDescription(g_EmuCDPD.hRet); (requires another DLL though)
          if (g_EmuCDPD.hRet = D3DERR_INVALIDCALL) then
            DxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Invalid Call)' +#13#10+ DxbxD3DErrorString(g_EmuCDPD.hRet))
          else if (g_EmuCDPD.hRet = D3DERR_NOTAVAILABLE) then
            DxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Not Available)' +#13#10+ DxbxD3DErrorString(g_EmuCDPD.hRet))
          else if (g_EmuCDPD.hRet = D3DERR_OUTOFVIDEOMEMORY) then
            DxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Out of Video Memory)' +#13#10+ DxbxD3DErrorString(g_EmuCDPD.hRet));

          DxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Unknown)' +#13#10+ DxbxD3DErrorString(g_EmuCDPD.hRet));
        end;

        // Update Xbox PresentationParameters :
        g_EmuCDPD.pPresentationParameters.BackBufferWidth := g_EmuCDPD.NativePresentationParameters.BackBufferWidth;
        g_EmuCDPD.pPresentationParameters.BackBufferHeight := g_EmuCDPD.NativePresentationParameters.BackBufferHeight;
        g_EmuCDPD.pPresentationParameters.BackBufferFormat := EmuPC2XB_D3DFormat(g_EmuCDPD.NativePresentationParameters.BackBufferFormat);
        g_EmuCDPD.pPresentationParameters.BackBufferCount := g_EmuCDPD.NativePresentationParameters.BackBufferCount;

        // cache device pointer
        // TODO -oDxbx : g_pD3DDevice is already (indirectly) assigned here,
        // what happens with it's reference-count? This needs investigation.
        Pointer(g_pD3DDevice) := g_EmuCDPD.ppReturnedDeviceInterface^;

        DxbxDumpNativeRenderStates();

        // default NULL guid
        ZeroMemory(@g_ddguid, sizeof(GUID));

        // enumerate device guid for this monitor, for directdraw
        hRet := DirectDrawEnumerateExA(@EmuEnumDisplayDevices, {lpContext=}NULL, DDENUM_ATTACHEDSECONDARYDEVICES);

        // create DirectDraw7
        begin
          if (FAILED(hRet)) then
            hRet := DirectDrawCreateEx({lpGUID=}NULL, {out}IDirectDraw7(g_pDD7), {iid=}IID_IDirectDraw7, {pUnkOuter=}NULL)
          else
            hRet := DirectDrawCreateEx({lpGUID=}@g_ddguid, {out}IDirectDraw7(g_pDD7), {iid=}IID_IDirectDraw7, {pUnkOuter=}NULL);

          if (FAILED(hRet)) then
            DxbxKrnlCleanup('Could not initialize DirectDraw7' +#13#10+ DxbxD3DErrorString(hRet));

          hRet := IDirectDraw7(g_pDD7).SetCooperativeLevel(0, DDSCL_NORMAL);
          if (FAILED(hRet)) then
            DxbxKrnlCleanup('Could not set cooperative level' +#13#10+ DxbxD3DErrorString(hRet));
        end;

        // check for YUY2 overlay support
        // TODO -oCXBX: accept other overlay types
        begin
          dwCodes := 0;
          lpCodes := nil;
          IDirectDraw7(g_pDD7).GetFourCCCodes({var}dwCodes, lpCodes);
          lpCodes := DxbxMalloc(dwCodes*sizeof(DWORD));
          IDirectDraw7(g_pDD7).GetFourCCCodes({var}dwCodes, lpCodes);

          DbgPrintf('Supported FourCC codes:');
          g_bYUY2OverlaysSupported := false;
          if dwCodes > 0 then // Dxbx addition, to prevent underflow
          for v := 0 to dwCodes - 1 do
          begin
            DbgPrintf('%s%s%s%s', [PAnsiChar(lpCodes)[4*v+0], PAnsiChar(lpCodes)[4*v+1], PAnsiChar(lpCodes)[4*v+2], PAnsiChar(lpCodes)[4*v+3]]);

            if (PDWORDs(lpCodes)[v] = DWORD(D3DFMT_YUY2){=MAKEFOURCC('Y', 'U', 'Y', '2')}) then
              g_bYUY2OverlaysSupported := true;
          end;

          DxbxFree(lpCodes);

          if (not g_bYUY2OverlaysSupported) then
            EmuWarning('YUY2 overlays are not supported in hardware, could be slow!')
          else
          begin
            // Does the user want to use Hardware accelerated YUV surfaces?
            if g_XBVideo.GetHardwareYUV() then
            begin
              if MayLog(lfUnit) then
                DbgPrintf('EmuD3D8 : Hardware accelerated YUV surfaces Enabled...');
            end
            else
            begin
              g_bYUY2OverlaysSupported := false;
              if MayLog(lfUnit) then
                DbgPrintf('EmuD3D8 : Hardware accelerated YUV surfaces Disabled...');
            end
          end;
        end;

        // initialize primary surface
        if (g_bYUY2OverlaysSupported) then
        begin
          ZeroMemory(@ddsd2, sizeof(ddsd2));

          ddsd2.dwSize := sizeof(ddsd2);
          ddsd2.dwFlags := DDSD_CAPS;
          ddsd2.ddsCaps.dwCaps := DDSCAPS_PRIMARYSURFACE;

          hRet := IDirectDraw7(g_pDD7).CreateSurface(ddsd2, @g_pDDSPrimary7, nil);

          if (FAILED(hRet)) then
            DxbxKrnlCleanup('Could not create primary surface (0x%.08X)' +#13#10+ DxbxD3DErrorString(hRet), [hRet]);
        end;

        // update render target cache
        Dispose({var}g_pCachedRenderTarget); // Dxbx addition : Prevent memory leaks
        New({var X_D3DSurface:}g_pCachedRenderTarget);
        ZeroMemory(g_pCachedRenderTarget, SizeOf(g_pCachedRenderTarget^));
        g_pCachedRenderTarget.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_D3DREND;
        IDirect3DDevice_GetRenderTarget(g_pD3DDevice, @(g_pCachedRenderTarget.Emu.Surface));

        // update z-stencil surface cache
        Dispose({var}g_pCachedZStencilSurface); // Dxbx addition : Prevent memory leaks
        New({var}g_pCachedZStencilSurface);
        ZeroMemory(g_pCachedZStencilSurface, SizeOf(g_pCachedZStencilSurface^));
        g_pCachedZStencilSurface.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_D3DSTEN;
        if IDirect3DDevice_GetDepthStencilSurface(g_pD3DDevice, @(g_pCachedZStencilSurface.Emu.Surface)) = D3D_OK then
        begin
          // Dxbx addition : Test if a ZBuffer exists, to fix invalid arguments to XTL_EmuD3DDevice_Clear :
          DxbxFix_HasZBuffer := True;
          IDirect3DSurface(g_pCachedZStencilSurface.Emu.Surface)._Release;
        end
        else
          DxbxFix_HasZBuffer := False;

//        // Dxbx addition : Put the DepthStencilSurface in the PresentParameters structure too :
//        if Assigned(g_pCachedZStencilSurface.Emu.Surface) then
//          PX_D3DPRESENT_PARAMETERS(g_EmuCDPD.pPresentationParameters).DepthStencilSurface := g_pCachedZStencilSurface;

        g_pD3DDevice.CreateVertexBuffer
        (
          {Length=}1,
          {Usage=}0,
          {FVF=}0,
          {Pool=}D3DPOOL_MANAGED,
          {ppVertexBuffer=}@g_pDummyBuffer
          {$IFDEF DXBX_USE_D3D9}, {pSharedHandle=}NULL{$ENDIF}
        );
{$IFDEF _DEBUG_TUROK_CREATES}
        DbgPrintf('CreateVertexBuffer: g_pDummyBuffer = 0x%0.8x', [g_pDummyBuffer]);
{$ENDIF}

        for Streams := 0 to 8-1 do
        begin
          // Dxbx note : Why do we need a dummy stream at all?
          g_pD3DDevice.SetStreamSource(Streams, IDirect3DVertexBuffer(g_pDummyBuffer), {$IFDEF DXBX_USE_D3D9}{OffsetInBytes=}0, {$ENDIF} 1);
        end;

        // begin scene
        g_pD3DDevice.BeginScene();

        // initially, show a black screen
        g_pD3DDevice.Clear(0, nil, D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER, $FF000000, 1.0, 0);
        DxbxPresent(nil, nil, 0, nil);

        // signal completion
        g_EmuCDPD.bReady := false;
      end
      else // not bCreate
      begin
        // release direct3d
        if (g_pD3DDevice <> nil) then
        begin
          if MayLog(lfUnit) then
            DbgPrintf('EmuD3D8 : CreateDevice proxy thread releasing old Device.');

          g_pD3DDevice.EndScene();

          g_EmuCDPD.hRet := g_pD3DDevice._Release();

          if (g_EmuCDPD.hRet = 0) then
            g_pD3DDevice := nil;
        end;

        if (g_bYUY2OverlaysSupported) then
        begin
          // cleanup directdraw surface
          if (g_pDDSPrimary7 <> nil) then
          begin
            if IDirectDrawSurface7(g_pDDSPrimary7)._Release() = 0 then
              g_pDDSPrimary7 := nil;
          end;
        end;

        // cleanup directdraw
        if (g_pDD7 <> nil) then
        begin
          if IDirectDraw7(g_pDD7)._Release() = 0 then
            g_pDD7 := nil;
        end;

        // signal completion
        g_EmuCDPD.bReady := false;
      end;
    end; // if bReady

    Sleep(1); // Dxbx: Should we use SwitchToThread() or YieldProcessor() ?
  end; // while true

  Result := D3D_OK;
end; // EmuThreadCreateDeviceProxy

// check if a resource has been registered yet (if not, register it)
procedure EmuVerifyResourceIsRegistered(pResource: PX_D3DResource); //inline;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  v: int;
begin
{$IFDEF GAME_HACKS_ENABLED}
  // 0xEEEEEEEE and 0xFFFFFFFF are somehow set in Halo :(
  if (pResource.Emu.Lock <> $EEEEEEEE) and (pResource.Emu.Lock <> $FFFFFFFF) then
{$ENDIF}
  if (pResource.Emu.Lock <> 0) then
    Exit;

  // Already "Registered" implicitly
  if (    IsSpecialResource(pResource.Data)
      and (   ((pResource.Data and X_D3DRESOURCE_DATA_FLAG_D3DREND) > 0)
           or ((pResource.Data and X_D3DRESOURCE_DATA_FLAG_D3DSTEN) > 0)))
  or (pResource.Data = $B00BBABE) then
    Exit;

  if (pResource.Data <> 0) then // Dxbx addition : This won't change here, so check outside the loop
    for v := 0 to RESOURCE_CACHE_SIZE-1 do
    begin
      if (g_EmuD3DResourceCache[v].Data = pResource.Data) then
      begin
        pResource.Emu.Resource := g_EmuD3DResourceCache[v].Emu.Resource;
        Exit;
      end;
    end;

  EmuSwapFS(fsXbox);
  XTL_EmuD3DResource_Register(pResource, nil(*PVOID(pResource.Data)*));
  EmuSwapFS(fsWindows);

  if (pResource.Emu.Lock <> X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
  begin
    for v := 0 to RESOURCE_CACHE_SIZE-1 do
    begin
      if (g_EmuD3DResourceCache[v].Data = 0) then
      begin
        g_EmuD3DResourceCache[v].Data := pResource.Data;
        g_EmuD3DResourceCache[v].Emu.Resource := pResource.Emu.Resource;
        break;
      end;

      // DXBX MARKED OUT - CXBX WAS TESTING 16, but that was never hit... we do hit it.
      //if (v = RESOURCE_CACHE_SIZE-1) then // Dxbx note : Cxbx uses 16 here, but that's just plain wrong!
      //  DxbxKrnlCleanup('X_D3DResource cache is maxed out!');
    end;
  end;
end; // EmuVerifyResourceIsRegistered

procedure EmuAdjustTextureDimensions(ResourceType: D3DRESOURCETYPE; dwWidth: PUINT; dwHeight: PUINT);
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
      NeedsConversion := False; // Not influenced by D3DPTEXTURECAPS_POW2
    D3DRTYPE_CUBETEXTURE:
      NeedsConversion := ((g_D3DCaps.TextureCaps and D3DPTEXTURECAPS_CUBEMAP_POW2) <> 0);
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
end; // EmuAdjustTextureDimensions

procedure DumpPresentationParameters(pPresentationParameters: PX_D3DPRESENT_PARAMETERS);
begin
  // Print a few of the pPresentationParameters contents to the console
  if MayLog(lfUnit) then
    LogBegin('PresentationParameters:').
      _(pPresentationParameters.BackBufferWidth, 'BackBufferWidth').
      _(pPresentationParameters.BackBufferHeight,'BackBufferHeight').
      _(pPresentationParameters.BackBufferFormat, 'BackBufferFormat').
      _(pPresentationParameters.BackBufferCount,'BackBufferCount').
      _(pPresentationParameters.MultiSampleType, 'MultiSampleType').
      _(Ord(pPresentationParameters.SwapEffect), 'SwapEffect').
      _(pPresentationParameters.hDeviceWindow, 'hDeviceWindow').
      _(pPresentationParameters.Windowed, 'Windowed').
      _(pPresentationParameters.EnableAutoDepthStencil,'EnableAutoDepthStencil').
      _(pPresentationParameters.AutoDepthStencilFormat, 'AutoDepthStencilFormat').
      _(pPresentationParameters.Flags, 'Flags').
      _(pPresentationParameters.FullScreen_RefreshRateInHz, 'FullScreen_RefreshRateInHz').
      _(pPresentationParameters.FullScreen_PresentationInterval, 'FullScreen_PresentationInterval').
    LogEnd();
end;

function LogBegin(const aSymbolName: string; const aCategory: string = ''): PLogStack;
begin
  Result := uLog.LogBegin(aSymbolName, {Category=}'EmuD3D8');
end;

{$IFDEF DXBX_TRY_DEEPER_DEVICE_INIT}
function XTL_EmuD3D__CDevice__Init
(
  {0 EAX}THISCALL_FIX_ARGUMENT_TAKING_EAX: DWORD; // Ignore this
  {0 EDX}THISCALL_FIX_ARGUMENT_TAKING_EDX: DWORD; // Ignore this
  {1 ECX}This: Pointer;
  {2 stack}pPresentationParameters: PX_D3DPRESENT_PARAMETERS
): HRESULT; register; // thiscall simulation - See Translation guide
var
  v: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('XTL_EmuD3D__CDevice__Init').
      _(This, 'This').
      _(pPresentationParameters, 'pPresentationParameters').
    LogEnd();

    DumpPresentationParameters(pPresentationParameters);
  end;

  // Cache parameters
  g_EmuCDPD.CreationParameters.AdapterOrdinal := 0;
  g_EmuCDPD.CreationParameters.DeviceType := D3DDEVTYPE_HAL;
  g_EmuCDPD.CreationParameters.hFocusWindow := 0; // ignored
  g_EmuCDPD.CreationParameters.BehaviorFlags := D3DCREATE_HARDWARE_VERTEXPROCESSING; // ignored
  g_EmuCDPD.pPresentationParameters := pPresentationParameters;
  g_EmuCDPD.ppReturnedDeviceInterface := @g_pD3DDevice;

  // Wait until proxy is done with an existing call (i highly doubt this situation will come up)
  while (g_EmuCDPD.bReady) do
    Sleep(10); // Dxbx: Should we use SwitchToThread() or YieldProcessor() ?

  // Signal proxy thread, and wait for completion
  g_EmuCDPD.bCreate := true; // Dxbx: bCreate should be set before bReady!
  g_EmuCDPD.bReady := true;

  // Wait until proxy is completed
  while g_EmuCDPD.bReady do
    Sleep(10); // Dxbx: Should we use SwitchToThread() or YieldProcessor() ?

  // Initialize the Xbox RenderState structure with default values :
//  if Assigned(@XTL_D3D_InitializeD3dState) then
//  begin
//    EmuSwapFS(fsXbox);
//    XTL_D3D_InitializeD3dState(); // This crashes on the access to D3D__pDevice+6688 (probably the ZStencilSurface resource pointer)
//    EmuSwapFS(fsWindows);
//  end
//  else
    DxbxInitializeDefaultRenderStates(g_EmuCDPD.pPresentationParameters);

  // Transfer the default render states (set above) over to PC side :
  for v := X_D3DRS_FIRST to X_D3DRS_LAST do
    DxbxTransferRenderState(X_D3DRENDERSTATETYPE(v));

  EmuSwapFS(fsXbox);

  Result := g_EmuCDPD.hRet;
end; // XTL_EmuD3D__CDevice__Init
{$ENDIF DXBX_TRY_DEEPER_DEVICE_INIT}

function XTL_EmuDirect3D_CreateDevice
(
    Adapter: UINT;
    DeviceType: D3DDEVTYPE;
    hFocusWindow: HWND;
    BehaviorFlags: DWORD;
    pPresentationParameters: PX_D3DPRESENT_PARAMETERS;
    ppReturnedDeviceInterface: XTL_PPIDirect3DDevice8
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  v: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('EmuIDirect3D_CreateDevice').
      _(Adapter, 'Adapter').
      _(DeviceType, 'DeviceType').
      _(hFocusWindow, 'hFocusWindow').
      _(BehaviorFlags, 'BehaviorFlags').
      _(pPresentationParameters, 'pPresentationParameters').
      _(ppReturnedDeviceInterface, 'ppReturnedDeviceInterface').
    LogEnd();

    DumpPresentationParameters(pPresentationParameters);
  end;

  // Cache parameters
  g_EmuCDPD.CreationParameters.AdapterOrdinal := Adapter;
  g_EmuCDPD.CreationParameters.DeviceType := DeviceType;
  g_EmuCDPD.CreationParameters.hFocusWindow := hFocusWindow;
  g_EmuCDPD.CreationParameters.BehaviorFlags := BehaviorFlags; // Dxbx addition
  g_EmuCDPD.pPresentationParameters := pPresentationParameters;
  g_EmuCDPD.ppReturnedDeviceInterface := ppReturnedDeviceInterface;

  // Wait until proxy is done with an existing call (i highly doubt this situation will come up)
  while (g_EmuCDPD.bReady) do
    Sleep(10); // Dxbx: Should we use SwitchToThread() or YieldProcessor() ?

  // Signal proxy thread, and wait for completion
  g_EmuCDPD.bCreate := true; // Dxbx: bCreate should be set before bReady!
  g_EmuCDPD.bReady := true;

  // Wait until proxy is completed
  while g_EmuCDPD.bReady do
    Sleep(10); // Dxbx: Should we use SwitchToThread() or YieldProcessor() ?

  // Initialize the Xbox RenderState structure with default values :
//  if Assigned(@XTL_D3D_InitializeD3dState) then
//  begin
//    EmuSwapFS(fsXbox);
//    XTL_D3D_InitializeD3dState(); // This crashes on the access to D3D__pDevice+6688 (probably the ZStencilSurface resource pointer)
//    EmuSwapFS(fsWindows);
//  end
//  else
    DxbxInitializeDefaultRenderStates(g_EmuCDPD.pPresentationParameters);

  // Transfer the default render states (set above) over to PC side :
  for v := X_D3DRS_FIRST to X_D3DRS_LAST do
    DxbxTransferRenderState(X_D3DRENDERSTATETYPE(v));

  EmuSwapFS(fsXbox);

  Result := g_EmuCDPD.hRet;
end; // XTL_EmuDirect3D_CreateDevice

function XTL_EmuD3DDevice_IsBusy(): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_IsBusy();');

  EmuWarning('EmuD3DDevice_IsBusy ignored!');

  EmuSwapFS(fsXbox);

  Result := g_bIsBusy; // Dxbx note : We read a boolean that's only true while inside Present()
end; // XTL_EmuD3DDevice_IsBusy

procedure XTL_EmuD3DDevice_KickPushBuffer();
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_KickPushBuffer();');

  Unimplemented('EmuD3DDevice_KickPushBuffer');
  // TODO -oDxbx : Locate the current PushBuffer address, and supply that to RunPushBuffer (without a fixup)

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetCreationParameters(pParameters: PD3DDEVICE_CREATION_PARAMETERS): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetCreationParameters').
      _(pParameters, 'pParameters').
    LogEnd();

  pParameters.AdapterOrdinal := D3DADAPTER_DEFAULT;
  pParameters.DeviceType := D3DDEVTYPE_HAL;
  pParameters.hFocusWindow := 0;
  pParameters.BehaviorFlags := D3DCREATE_HARDWARE_VERTEXPROCESSING;

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end; // XTL_EmuD3DDevice_GetCreationParameters

function XTL_EmuD3DDevice_GetDirect3D
(
  ppD3D8: XTL_PLPDIRECT3D8
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetDirect3D').
      _(ppD3D8, 'ppD3D8').
    LogEnd();

{$IFDEF DXBX_USE_D3D9}
  g_pD3DDevice.GetDirect3D({out}PIDirect3D9(ppD3D8));
{$ELSE}
  g_pD3DDevice.GetDirect3D({out}PIDirect3D8(ppD3D8));
{$ENDIF}
  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_GetDepthClipPlanes
(
  pNear: Pfloat;
  pFar: Pfloat;
  Flags: DWORD
); stdcall;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetDepthClipPlanes').
      _(pNear, 'pNear').
      _(pFar, 'pFar').
      _(Flags, 'Flags').
    LogEnd();

//  g_pD3DDevice.GetDepthClipPlanes
  Unimplemented('XTL_EmuD3DDevice_GetDepthClipPlanes');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuDirect3D_CheckDeviceFormat
(
  Adapter: UINT;
  DeviceType: D3DDEVTYPE;
  AdapterFormat: X_D3DFORMAT;
  Usage: DWORD;
  RType: X_D3DRESOURCETYPE;
  CheckFormat: X_D3DFORMAT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3D_CheckDeviceFormat').
      _(Adapter, 'Adapter').
      _(DeviceType, 'DeviceType').
      _(AdapterFormat, 'AdapterFormat').
      _(Usage, 'Usage').
      _(Int(RType), 'RType').
      _(CheckFormat, 'CheckFormat').
    LogEnd;

  if (RType > X_D3DRTYPE_INDEXBUFFER) then
    DxbxKrnlCleanup('RType > 7 (X_D3DRTYPE_INDEXBUFFER)');

  Result := g_pD3D.CheckDeviceFormat
  (
    g_XBVideo.GetDisplayAdapter(),
    iif(g_XBVideo.GetDirect3DDevice() = 0, D3DDEVTYPE_HAL, D3DDEVTYPE_REF),
    EmuXB2PC_D3DFormat(AdapterFormat),
    Usage, _D3DRESOURCETYPE(RType), EmuXB2PC_D3DFormat(CheckFormat)
  );

  EmuSwapFS(fsXbox);
end; // XTL_EmuDirect3D_CheckDeviceFormat

function XTL_EmuD3DDevice_GetDisplayFieldStatus(pFieldStatus: PX_D3DFIELD_STATUS): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetDisplayFieldStatus').
      _(pFieldStatus, 'pFieldStatus').
    LogEnd;

//  pFieldStatus.Field := X_D3DFIELDTYPE(iif(g_VBData.VBlankCounter and 1 = 0, Ord(X_D3DFIELD_ODD), Ord(X_D3DFIELD_EVEN)));
//  pFieldStatus.VBlankCount := g_VBData.VBlankCounter;
  pFieldStatus.Field := X_D3DFIELD_PROGRESSIVE;
  pFieldStatus.VBlankCount := 0;

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end; // XTL_EmuD3DDevice_GetDisplayFieldStatus

function XTL_EmuD3DDevice_BeginPush(Count: DWORD): PDWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_BeginPush').
      _(Count, 'Count').
    LogEnd();

  // TODO -oDxbx : Speed this up by re-using a previously memory block

  Result := XboxAlloc(Count * sizeof(DWORD)); // Cxbx: new DWORD[Count] Dxbx: Put this in an Xbox heap
  ZeroMemory(Result, Count * sizeof(DWORD));
  g_dwPrimaryPBCount := Count;
  g_pPrimaryPB := Result;

  EmuSwapFS(fsXbox);
end; // XTL_EmuD3DDevice_BeginPush

function XTL_EmuD3DDevice_EndPush(pPush: PDWORD): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_EndPush').
      _(pPush, 'pPush').
    LogEnd();

  XTL_EmuExecutePushBufferRaw(g_pPrimaryPB);

  XboxFree(g_pPrimaryPB); // Cxbc: delete[]
  g_pPrimaryPB := nil;

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end; // XTL_EmuD3DDevice_EndPush

function XTL_EmuD3DDevice_BeginVisibilityTest(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('EmuD3DDevice_BeginVisibilityTest').LogEnd();
    EmuSwapFS(fsXbox);
  end;

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_EndVisibilityTest
(
    Index: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);

    if MayLog(lfUnit) then
      LogBegin('EmuD3DDevice_EndVisibilityTest').
        _(Index, 'Index').
      LogEnd();

    EmuSwapFS(fsXbox);
  end;

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_GetBackBufferScale
(
  px: PFLOAT;
  py: PFLOAT
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetBackBufferScale').
      _(px, 'px').
      _(py, 'py').
    LogEnd();

  EmuWarning('GetBackBufferScale ignored');

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetBackBufferScale
(
  x: FLOAT;
  y: FLOAT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetBackBufferScale').
      _(X, 'x').
      _(Y, 'y').
    LogEnd();

  EmuWarning('SetBackBufferScale ignored');

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetVisibilityTestResult
(
  Index: DWORD;
  {CONST} pResult: PUINT;
  {CONST} pTimeStamp: PULONGLONG
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetVisibilityTestResult').
      _(Index, 'Index').
      _(pResult, 'pResult').
      _(pTimeStamp, 'pTimeStamp').
    LogEnd();

  // TODO -oCXBX: actually emulate this!?

  if (pResult <> nil) then
    pResult^ := 640 * 480;

  if (pTimeStamp <> nil) then
    pTimeStamp^ := 0;

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

(* Too high level : No patch needed, just copies g_DeviceCaps :
procedure XTL_EmuD3DDevice_GetDeviceCaps
(
    pCaps: PD3DCAPS
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

function XTL_EmuD3DDevice_LoadVertexShader
(
  Handle: DWORD;
  Address: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pVertexShader: PVERTEX_SHADER;
  i: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_LoadVertexShader').
      _(Handle, 'Handle').
      _(Address, 'Address').
    LogEnd();

  if (Address < D3DVS_XBOX_NR_ADDRESS_SLOTS{=136}) and VshHandleIsVertexShader(Handle) then
  begin
    pVertexShader := PVERTEX_SHADER(VshHandleGetVertexShader(Handle).Handle);
    if pVertexShader.Size > 0 then // Dxbx addition, to prevent underflow
    for i := Address to pVertexShader.Size - 1 do
    begin
      // TODO -oCXBX: This seems very fishy
      g_VertexShaderSlots[i] := Handle;
    end;
  end;

  EmuSwapFS(fsXbox);
  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_SelectVertexShader
(
  Handle: DWORD;
  Address: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pVertexShader: PVERTEX_SHADER;
  pVertexShader2: PX_D3DVertexShader;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SelectVertexShader').
      _(Handle, 'Handle').
      _(Address, 'Address').
    LogEnd();

  if (VshHandleIsVertexShader(Handle)) then
  begin
    pVertexShader := PVERTEX_SHADER(VshHandleGetVertexShader(Handle).Handle);
{$IFDEF DXBX_USE_D3D9}
    g_pD3DDevice.SetVertexDeclaration(IDirect3DVertexDeclaration9(pVertexShader.pDeclaration));
    g_pD3DDevice.SetVertexShader(IDirect3DVertexShader(pVertexShader.Handle));
{$ELSE}
    g_pD3DDevice.SetVertexShader(pVertexShader.Handle);
{$ENDIF}
  end
  else if (Handle = HNULL) then
  begin
{$IFDEF DXBX_USE_D3D9}
    g_pD3DDevice.SetVertexShader(NULL);
    g_pD3DDevice.SetFVF(D3DFVF_XYZ or D3DFVF_TEX0);
{$ELSE}
    g_pD3DDevice.SetVertexShader(D3DFVF_XYZ or D3DFVF_TEX0);
{$ENDIF}
  end
  else if (Address < D3DVS_XBOX_NR_ADDRESS_SLOTS{=136}) then
  begin
    pVertexShader2 := PX_D3DVertexShader(g_VertexShaderSlots[Address]);

    if (pVertexShader2 <> NULL) then
    begin
{$IFDEF DXBX_USE_D3D9}
      g_pD3DDevice.SetVertexDeclaration(IDirect3DVertexDeclaration9(PVERTEX_SHADER(pVertexShader2.Handle).pDeclaration));
      g_pD3DDevice.SetVertexShader(IDirect3DVertexShader(PVERTEX_SHADER(pVertexShader2.Handle).Handle));
{$ELSE}
      g_pD3DDevice.SetVertexShader(PVERTEX_SHADER(pVertexShader2.Handle).Handle);
{$ENDIF}
    end
    else
    begin
      EmuWarning('g_VertexShaderSlots[%d] = 0', [Address]);
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

{$J+}
const
  // TODO -oDxbx : Officially, we should have an array like this for each adapter :v
  XboxResolutions: array [0..5] of record W, H, PCMode: LongWord; N: string; end = (
    (W:640; H:480; PCMode:0; N:'NTSC'),
    (W:640; H:576; PCMode:0; N:'PAL'),
    (W:720; H:480; PCMode:0; N:'480p'),
    (W:720; H:576; PCMode:0; N:'PAL2?'),
    (W:1280; H:720; PCMode:0; N:'720p'),
    (W:1920; H:1080; PCMode:0; N:'1080i')
  );
{$J-}

function IsValidXboxDisplayMode(const PCDisplayMode: D3DDISPLAYMODE; const PCModeNr: int): boolean;
var
  i: int;
begin
  for i := Low(XboxResolutions) to High(XboxResolutions) do
  begin
    if  (XboxResolutions[i].W = PCDisplayMode.Width)
    and (XboxResolutions[i].H = PCDisplayMode.Height) then
    begin
      XboxResolutions[i].PCMode := PCModeNr;
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

function XTL_EmuDirect3D_GetAdapterModeCount
(
  Adapter: UINT
): UINT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  ret: UINT;
  v: UINT32;
  PCDisplayMode: D3DDISPLAYMODE;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3D_GetAdapterModeCount').
      _(Adapter, 'Adapter').
    LogEnd();

  Result := 0;
  ret := g_pD3D.GetAdapterModeCount(g_XBVideo.GetDisplayAdapter(){$IFDEF DXBX_USE_D3D9}, D3DFMT_UNKNOWN{$ENDIF});
  if ret > 0 then // Dxbx addition, to prevent underflow
  for v := 0 to ret - 1 do
  begin
    if (g_pD3D.EnumAdapterModes(g_XBVideo.GetDisplayAdapter(), {$IFDEF DXBX_USE_D3D9}D3DFMT_UNKNOWN,{$ENDIF} v, {out}PCDisplayMode) <> D3D_OK) then
      break;

    // Dxbx addition : Only count valid Xbox resultions :
    if IsValidXboxDisplayMode(PCDisplayMode, v) then
      Inc(Result);
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuDirect3D_GetAdapterDisplayMode
(
  Adapter: UINT;
  pMode: PX_D3DDISPLAYMODE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  PCDisplayMode: D3DDISPLAYMODE;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3D_GetAdapterDisplayMode').
      _(Adapter, 'Adapter').
      _(pMode, 'pMode').
    LogEnd();

  // Cxbx NOTE: WARNING: We should cache the 'Emulated' display mode and return
  // This value. We can initialize the cache with the default Xbox mode data.
  Result := g_pD3D.GetAdapterDisplayMode
  (
    g_XBVideo.GetDisplayAdapter(),
    {out}PCDisplayMode
  );

  // make adjustments to the parameters to make sense with windows direct3d
  begin
    // TODO -oCXBX: Retrieve from current CreateDevice settings?
    pMode.Width := 640;
    pMode.Height := 480;
    pMode.RefreshRate := PCDisplayMode.RefreshRate;
    // Convert Format (PC->Xbox)
    pMode.Format := EmuPC2XB_D3DFormat(PCDisplayMode.Format);
    // TODO -oCXBX: Make this configurable in the future?
    pMode.Flags := X_D3DPRESENTFLAG_FIELD or X_D3DPRESENTFLAG_INTERLACED or X_D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuDirect3D_EnumAdapterModes
(
  Adapter: UINT;
  Mode: UINT;
  pMode: PX_D3DDISPLAYMODE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  PCDisplayMode: D3DDISPLAYMODE;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3D_EnumAdapterModes').
      _(Adapter, 'Adapter').
      _(Mode, 'Mode').
      _(pMode, 'pMode').
    LogEnd();

  if Mode < High(XboxResolutions) then
    Result := g_pD3D.EnumAdapterModes(g_XBVideo.GetDisplayAdapter(), {$IFDEF DXBX_USE_D3D9}D3DFMT_UNKNOWN,{$ENDIF} XboxResolutions[Mode].PCMode, PCDisplayMode)
  else
    Result := D3DERR_INVALIDCALL;

  // make adjustments to parameters to make sense with windows direct3d
  if (Result = D3D_OK) then
  begin
    // NOTE: WARNING: PC D3DDISPLAYMODE is different than Xbox D3DDISPLAYMODE!

    // Convert Format (PC->Xbox)
    pMode.Width := PCDisplayMode.Width;
    pMode.Height := PCDisplayMode.Height;
    pMode.RefreshRate := PCDisplayMode.RefreshRate;
    // TODO -oCXBX: Make this configurable in the future?
    pMode.Flags := X_D3DPRESENTFLAG_FIELD or X_D3DPRESENTFLAG_INTERLACED or X_D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;
    pMode.Format := EmuPC2XB_D3DFormat(PCDisplayMode.Format);
  end
  else
    Result := D3DERR_INVALIDCALL;

  EmuSwapFS(fsXbox);
end; // XTL_EmuDirect3D_EnumAdapterModes

procedure XTL_EmuD3D_KickOffAndWaitForIdle(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3D_KickOffAndWaitForIdle').LogEnd();

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetGammaRamp
(
  dwFlags: DWORD;
  {CONST} pRamp: PX_D3DGAMMARAMP
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwPCFlags: DWORD;
  PCRamp: D3DGAMMARAMP;
  v: int;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetGammaRamp').
      _(dwFlags, 'dwFlags').
      _(pRamp, 'pRamp').
    LogEnd();

  // remove D3DSGR_IMMEDIATE
  dwPCFlags := dwFlags and (not $00000002);

  for v := 0 to 255-1 do
  begin
    PCRamp.red[v] := pRamp.red[v];
    PCRamp.green[v] := pRamp.green[v];
    PCRamp.blue[v] := pRamp.blue[v];
   end;

  g_pD3DDevice.SetGammaRamp({$IFDEF DXBX_USE_D3D9}{iSwapChain=}0,{$ENDIF} dwPCFlags, PCRamp);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_AddRef(): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_AddRef').LogEnd();

  Result := ULONG(g_pD3DDevice._AddRef());

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_BeginStateBlock(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_BeginStateBlock').LogEnd();

  Result := g_pD3DDevice.BeginStateBlock();

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_BeginStateBig(
  Count: DWORD
  ): PDWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  // Dxbx note : BeginStateBig is called when BeginState (which is otherwise inlined)
  // receives a Count >= 128. Otherwise, BeginState just does something like this :
  //
  //   Result := D3D__Device.Put;
  //   if (Result >= D3D__Device.Threshold) then
  //     Result := D3DDevice_MakeSpace();
  //
  // EndState is always inlined and does :
  //
  //   D3D__Device.Put := pPush;

  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_BeginStateBig').
      _(Count, 'Count').
    LogEnd();

  DxbxKrnlCleanup('BeginStateBig is not implemented');
  Result := nil;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CaptureStateBlock(Token: DWORD): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CaptureStateBlock').
      _(Token, 'Token').
    LogEnd();

{$IFDEF DXBX_USE_D3D9}
  Result := IDirect3DStateBlock9(Token).Capture();
{$ELSE}
  Result := g_pD3DDevice.CaptureStateBlock(Token);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_ApplyStateBlock(Token: DWORD): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_ApplyStateBlock').
      _(Token, 'Token').
    LogEnd();

{$IFDEF DXBX_USE_D3D9}
  Result := IDirect3DStateBlock9(Token).Apply();
{$ELSE}
  Result := g_pD3DDevice.ApplyStateBlock(Token);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_EndStateBlock(pToken: PDWORD): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_EndStateBlock').
      _(pToken, 'pToken').
    LogEnd();

{$IFDEF DXBX_USE_D3D9}
  Result := g_pD3DDevice.EndStateBlock(PIDirect3DStateBlock(pToken));
{$ELSE}
  Result := g_pD3DDevice.EndStateBlock({out}pToken^);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CopyRects
(
  pSourceSurface: PX_D3DSurface;
  pSourceRectsArray: PRECT;
  cRects: UINT;
  pDestinationSurface: PX_D3DSurface;
  pDestPointsArray: PPOINT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
{var
  kthx: Integer;
  FileName: array [0..255 - 1] of _char;}
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CopyRects').
      _(pSourceSurface, 'pSourceSurface').
      _(pSourceRectsArray, 'pSourceRectsArray').
      _(cRects, 'cRects').
      _(pDestinationSurface, 'pDestinationSurface').
      _(pDestPointsArray, 'pDestPointsArray').
    LogEnd();

  // Dxbx addition : This is safer than a hardcoded call to UnlockRect like Cxbx does :
  DxbxUnlockD3DResource(pSourceSurface); // Dxbx addition
//  IDirect3DSurface(pSourceSurface.Emu.Surface).UnlockRect();

  { MARKED OUT BY CXBX
  kthx := 0;
  sprintf(FileName, DxbxDebugFolder +'\Textures\SourceSurface-%d.bmp', [kthx]); Inc(kthx);

  D3DXSaveSurfaceToFileA(FileName, D3DXIFF_BMP, pSourceSurface.EmuSurface8, nil, nil);
  }

{$IFDEF DXBX_USE_D3D9}
  Result := g_pD3DDevice.UpdateSurface
  (
    IDirect3DSurface9(pSourceSurface.Emu.Surface),
    pSourceRectsArray,
    //cRects, {$MESSAGE 'fixme'} // What should happen to cRects?
    IDirect3DSurface9(pDestinationSurface.Emu.Surface),
    pDestPointsArray
  );
{$ELSE}
  Result := g_pD3DDevice.CopyRects
  (
    IDirect3DSurface(pSourceSurface.Emu.Surface),
    pSourceRectsArray,
    cRects,
    IDirect3DSurface(pDestinationSurface.Emu.Surface),
    pDestPointsArray
  );
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CreateDepthStencilSurface
(
  Width: UINT;
  Height: UINT;
  Format: X_D3DFORMAT;
  MultiSample: X_D3DMULTISAMPLE_TYPE;
  ppSurface: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  PCFormat: D3DFORMAT;
  PCMultiSample: D3DMULTISAMPLE_TYPE;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CreateDepthStencilSurface').
      _(Width, 'Width').
      _(Height, 'Height').
      _(Format, 'Format').
      _(MultiSample, 'MultiSample').
      _(ppSurface, 'ppSurface').
    LogEnd();

  New({var PX_D3DSurface}ppSurface^);
  ZeroMemory(ppSurface^, SizeOf(ppSurface^^));

  PCFormat := EmuXB2PC_D3DFormat(Format);
  PCMultiSample := EmuXB2PC_D3DMULTISAMPLE_TYPE(MultiSample);

  EmuAdjustTextureDimensions(D3DRTYPE_SURFACE, @Width, @Height);

  Result := IDirect3DDevice_CreateDepthStencilSurface(g_pD3DDevice,
    Width,
    Height,
    PCFormat,
    PCMultiSample,
    PIDirect3DSurface(@(ppSurface^.Emu.Surface)));

  if FAILED(Result) then
    DxbxKrnlCleanup('CreateDepthStencilSurface failed! ' + #13#10 + 'Format = 0x%8.8X', [Format]);

  // Dxbx addition : Initialize Common field properly :
  ppSurface^.Common := ({RefCount=}1 and X_D3DCOMMON_REFCOUNT_MASK) or X_D3DCOMMON_TYPE_SURFACE or X_D3DCOMMON_D3DCREATED;

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuD3D8 : CreateDepthStencilSurface: Successfully Created Surface : ' + ResourceToString(ppSurface^));
    // TODO : Also print Result, Width, Height, PCFormat

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CreateImageSurface
(
  Width: UINT;
  Height: UINT;
  Format: X_D3DFORMAT;
  ppBackBuffer: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  PCFormat: D3DFORMAT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CreateImageSurface').
      _(Width, 'Width').
      _(Height, 'Height').
      _(Format, 'Format').
      _(ppBackBuffer, 'ppBackBuffer').
    LogEnd();

  New({var PX_D3DSurface}ppBackBuffer^);
  ZeroMemory(ppBackBuffer^, SizeOf(ppBackBuffer^^));

  PCFormat := EmuXB2PC_D3DFormat(Format);

  // Dxbx addition :
  EmuAdjustTextureDimensions(D3DRTYPE_SURFACE, @Width, @Height);

  Result := IDirect3DDevice_CreateImageSurface(g_pD3DDevice,
    Width,
    Height,
    PCFormat,
    PIDirect3DSurface(@(ppBackBuffer^.Emu.Surface)));

  if FAILED(Result) and (Format = X_D3DFMT_LIN_D24S8) then
  begin
    EmuWarning('CreateImageSurface: D3DFMT_LIN_D24S8 -> D3DFMT_A8R8G8B8');

    PCFormat := D3DFMT_A8R8G8B8;

    Result := IDirect3DDevice_CreateImageSurface(g_pD3DDevice,
      Width,
      Height,
      PCFormat,
      PIDirect3DSurface(@(ppBackBuffer^.Emu.Surface)));
  end;

  if FAILED(Result) and (Format = X_D3DFMT_LIN_D16) then
  begin
    EmuWarning('CreateImageSurface: D3DFMT_LIN_D16 -> D3DFMT_D16_LOCKABLE');

    PCFormat := D3DFMT_D16_LOCKABLE;

    Result := IDirect3DDevice_CreateImageSurface(g_pD3DDevice,
       Width,
       Height,
       PCFormat,
       PIDirect3DSurface(@(ppBackBuffer^.Emu.Surface)));
  end;

  if FAILED(Result) then
    DxbxKrnlCleanup('CreateImageSurface failed! ' + #13#10 + 'Format = 0x%8.8X', [Format]);

  // Dxbx addition : Initialize Common field properly :
  ppBackBuffer^.Common := ({RefCount=}1 and X_D3DCOMMON_REFCOUNT_MASK) or X_D3DCOMMON_TYPE_SURFACE or X_D3DCOMMON_D3DCREATED;
//    DxbxUpdateResourceFields(ppBackBuffer^);

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuD3D8 : CreateImageSurface: Successfully Created Surface : ' + ResourceToString(ppBackBuffer^));
    // TODO : Also print Result, Width, Height, PCFormat

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_GetGammaRamp
(
    pRamp: PX_D3DGAMMARAMP
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pGammaRamp: PD3DGAMMARAMP;
  v: int;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetGammaRamp').
      _(pRamp, 'pRamp').
    LogEnd();

  pGammaRamp := PD3DGAMMARAMP(DxbxMalloc(sizeof(D3DGAMMARAMP)));

  g_pD3DDevice.GetGammaRamp({$IFDEF DXBX_USE_D3D9}{iSwapChain=}0,{$ENDIF} {out}pGammaRamp^);

  for v := 0 to 256-1 do
  begin
    pRamp.red[v] := BYTE(pGammaRamp.red[v]);
    pRamp.green[v] := BYTE(pGammaRamp.green[v]);
    pRamp.blue[v] := BYTE(pGammaRamp.blue[v]);
  end;

  DxbxFree(pGammaRamp);

  EmuSwapFS(fsXbox);
end;

{static}var pBackBuffer: PX_D3DSurface = nil;
function XTL_EmuD3DDevice_GetBackBuffer2
(
    BackBuffer: INT
): PX_D3DSurface; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetBackBuffer2').
      _(BackBuffer, 'BackBuffer').
    LogEnd();

(* unsafe, somehow  -- MARKED OUT BY CXBX --
    HRESULT hRet := D3D_OK;

    X_D3DSurface *pBackBuffer := new X_D3DSurface();
    ZeroMemory(pBackBuffer, SizeOf(pBackBuffer^));

    if (BackBuffer = -1) then
    begin
         {static?}{pCachedPrimarySurface: XTL_PIDirect3DSurface8 := nil;

        if (pCachedPrimarySurface = nil) then
        begin
            // create a buffer to return
            // TODO -oCXBX: Verify the surface is always 640x480
            D3DDevice_CreateImageSurface(g_pD3DDevice, 640, 480, D3DFMT_A8R8G8B8, {out}{IDirect3DSurface(pCachedPrimarySurface));
         end;

        pBackBuffer.Emu.Surface := pCachedPrimarySurface;

        hRet := g_pD3DDevice.GetFrontBuffer(pBackBuffer.Emu.Surface);

        if (FAILED(hRet)) then
        begin
            EmuWarning('Could not retrieve primary surface, using backbuffer');
            pCachedPrimarySurface := nil;
            pBackBuffer.Emu.Surface._Release();
            pBackBuffer.Emu.Surface := nil;
            BackBuffer := 0;
         end;

        // Debug: Save this image temporarily
        //D3DXSaveSurfaceToFileA(DxbxDebugFolder +'\Textures\FrontBuffer.bmp', D3DXIFF_BMP, pBackBuffer.Emu.Surface, NULL, NULL);
     end;

    if (BackBuffer <> -1) then
        hRet := g_pD3DDevice.GetBackBuffer({$IFDEF DXBX_USE_D3D9}{iSwapChain=}0,{$ENDIF} BackBuffer, D3DBACKBUFFER_TYPE_MONO, @(pBackBuffer.Emu.Surface));
*)

  if pBackBuffer = nil then // Dxbx addition, to initialize this 'static' var only once
  begin
    New({var PX_D3DSurface}pBackBuffer);
    ZeroMemory(pBackBuffer, SizeOf(pBackBuffer^));
  end;

  if (BackBuffer = -1) then
    BackBuffer := 0; // TODO : Use actual FrontBuffer number here

  hRet := g_pD3DDevice.GetBackBuffer({$IFDEF DXBX_USE_D3D9}{iSwapChain=}0,{$ENDIF} BackBuffer, D3DBACKBUFFER_TYPE_MONO, @(pBackBuffer.Emu.Surface));

  if (hRet = D3D_OK) then // not FAILED
    //
  else
    DxbxKrnlCleanup('Unable to retrieve back buffer');

  // update data pointer
  pBackBuffer.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_SURFACE;

  EmuSwapFS(fsXbox);

  Result := pBackBuffer;
end;

function XTL_EmuD3DDevice_GetBackBuffer
(
  BackBuffer: INT;
  Type_: D3DBACKBUFFER_TYPE;
  ppBackBuffer: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);

    LogBegin('EmuD3DDevice_GetBackBuffer >>').
      _(BackBuffer, 'BackBuffer').
      _(Ord(Type_), 'Type').
      _(ppBackBuffer, 'ppBackBuffer').
    LogEnd();

    EmuSwapFS(fsXbox);
  end;

  // Allow backbuffer -1 and up to the allocated BackBufferCount, but nothing beyond this :
  if (BackBuffer < -1) or (BackBuffer >= int(g_EmuCDPD.NativePresentationParameters.BackBufferCount))
  or (ppBackBuffer = nil) then
    Result := D3DERR_INVALIDCALL
  else
  begin
    // Note : -1 returns the front buffer
    ppBackBuffer^ := XTL_EmuD3DDevice_GetBackBuffer2(BackBuffer);

    Result := D3D_OK;
  end;
end;

function XTL_EmuD3DDevice_GetBackMaterial
(
  pMaterial: PD3DMaterial
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetBackMaterial').
      _(pMaterial, 'pMaterial').
    LogEnd();

  // D3DDevice_GetBackMaterial(pMaterial);
  EmuWarning('GetBackMaterial not supported!');

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetViewport
(
  pViewport: PD3DVIEWPORT
): HRESULT; stdcall;
// Branch:shogun  Revision:162+StrickerX3_patch  Translator:Shadow_Tj  Done:100
(*
var
  dwX: DWORD;
  dwY: DWORD;
  dwWidth: DWORD;
  dwHeight: DWORD;
  currentViewport: D3DVIEWPORT;
*)
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetViewport').
      _(pViewport, 'pViewport').
    LogEnd();

(*
  dwX := pViewport.X;
  dwY := pViewport.Y;
  dwWidth := pViewport.Width;
  dwHeight := pViewport.Height;

  g_pD3DDevice.GetViewport({out}currentViewport);
  // resize to fit current viewport (otherwise crashes occur)
//  begin
//    if(dwX < currentViewport.X) then
//    begin
//      EmuWarning('Moving Viewport->X to %d', [currentViewport.X]);
//      pViewport.X := currentViewport.X;
//    end;
//    if(dwY < currentViewport.Y) then
//    begin
//      EmuWarning('Moving Viewport->Y to %d', [currentViewport.Y]);
//      pViewport.Y := currentViewport.Y;
//    end;
//    if (dwWidth > currentViewport.Width - dwX) then
//    begin
//      EmuWarning('Resizing Viewport.Width to %d', [currentViewport.Width - dwX]);
//      pViewport.Width := currentViewport.Width - dwX;
//    end;
//
//    if (dwHeight > currentViewport.Height - dwY) then
//    begin
//      EmuWarning('Resizing Viewport.Height to %d', [currentViewport.Height - dwY]);
//      pViewport.Height := currentViewport.Height - dwY;
//    end;
//  end;
*)
  Result := g_pD3DDevice.SetViewport(pViewport^);

  // restore originals
//  begin
//    if(dwX < currentViewport.X) then
//      pViewport.X := dwX;
//
//    if(dwY < currentViewport.Y) then
//      pViewport.Y := dwY;
//
//    if(dwWidth > currentViewport.Width) then
//      pViewport.Width := dwWidth;
//
//    if(dwHeight > currentViewport.Height) then
//      pViewport.Height := dwHeight;
//  end;

  if FAILED(Result) then
  begin
    if MayLog(lfUnit) then
      EmuWarning('Unable to set viewport!');
    Result := D3D_OK;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_Suspend(): HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_Suspend();');

  Unimplemented('XTL_EmuD3DDevice_Suspend');
//  D3DDevice_Suspend();
  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetViewport
(
  pViewport: PD3DVIEWPORT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetViewport').
      _(Pointer(pViewport), 'pViewport'). // Prevent rendering of PD3DVIEWPORT; TODO : Output arguments need a generic solution.
    LogEnd();

  Result := g_pD3DDevice.GetViewport({out}pViewport^);

  if FAILED(Result) then
  begin
    EmuWarning('Unable to get viewport!');
    Result := D3D_OK;
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_GetViewportOffsetAndScale
(
    pOffset: PD3DXVECTOR4;
    pScale: PD3DXVECTOR4
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
//var
{  fScaleX: Float;
  fScaleY: Float;
  fScaleZ: Float;
  fOffsetX: Float;
  fOffsetY: Float;}
//  Viewport: D3DVIEWPORT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetViewportOffsetAndScale').
      _(pOffset, 'pOffset').
      _(pScale, 'pScale').
    LogEnd();

{
  Never used because Cxbx marked them out below
  fScaleX := 1.0;
  fScaleY := 1.0;
  fScaleZ := 1.0;
  fOffsetX := 0.5 + 1.0 / 32;
  fOffsetY := 0.5 + 1.0 / 32;
}

//  EmuSwapFS(fsXbox);
//  XTL_EmuD3DDevice_GetViewport(@Viewport);
//  EmuSwapFS(fsWindows);

  pScale.x := 1.0;
  pScale.y := 1.0;
  pScale.z := 1.0;
  pScale.w := 1.0;

  pOffset.x := 0.0;
  pOffset.y := 0.0;
  pOffset.z := 0.0;
  pOffset.w := 0.0;

{ Marked out by Cxbx
  pScale.x := Viewport.Width * 0.5 * fScaleX;
  pScale.y := Viewport.Height * -0.5 * fScaleY;
  pScale.z := (Viewport.MaxZ - Viewport.MinZ) * fScaleZ;
  pScale.w := 0;

  pOffset.x := Viewport.Width * fScaleX * 0.5 + Viewport.X * fScaleX + fOffsetX;
  pOffset.y := Viewport.Height * fScaleY * 0.5 + Viewport.Y * fScaleY + fOffsetY;
  pOffset.z := Viewport.MinZ * fScaleZ;
  pOffset.w := 0;
}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetShaderConstantMode
(
  Mode: X_VERTEXSHADERCONSTANTMODE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetShaderConstantMode').
      _(Mode, 'Mode').
    LogEnd();

  g_VertexShaderConstantMode := Mode;
  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_Reset
(
  pPresentationParameters: PX_D3DPRESENT_PARAMETERS
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('EmuD3DDevice_Reset').
      _(pPresentationParameters, 'pPresentationParameters').
    LogEnd();

    DumpPresentationParameters(pPresentationParameters);
  end;

  EmuWarning('Device Reset is being utterly ignored');

  EmuSwapFS(fsXbox);
  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_Resume
(
  Reset: BOOL
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_Resume').
      _(Reset, 'Reset').
    LogEnd();

  if Reset = BOOL_TRUE then
    g_pD3DDevice.Reset(g_EmuCDPD.NativePresentationParameters);

  EmuSwapFS(fsXbox);
  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_GetRenderTarget
(
  ppRenderTarget: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pPCSurface: XTL_PIDirect3DSurface8;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetRenderTarget').
      _(ppRenderTarget, 'ppRenderTarget').
    LogEnd();

  if Assigned(g_pCachedRenderTarget) then
  begin
    pPCSurface := g_pCachedRenderTarget.Emu.Surface;
    if Assigned(pPCSurface) then
      IDirect3DSurface(pPCSurface)._AddRef();
  end;

  ppRenderTarget^ := g_pCachedRenderTarget;

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuD3D8 : RenderTarget : ' + ResourceToString(ppRenderTarget^));

  // TODO -oDXBX: Should we nil-out pSurface ?

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_GetRenderTarget2(): PX_D3DSurface; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_GetRenderTarget2(); >>');

  EmuSwapFS(fsXbox);

  XTL_EmuD3DDevice_GetRenderTarget(@Result);
end;

function XTL_EmuD3DDevice_GetTextureStageState
(
  Stage: DWORD;
  Type_: X_D3DTEXTURESTAGESTATETYPE;
  pValue: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
var
  Type_VersionIndependent: X_D3DTEXTURESTAGESTATETYPE;
begin
  EmuSwapFs(fsWindows);

  Type_VersionIndependent := DxbxFromOldVersion_D3DTSS(Type_);
  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetTextureStageState').
      _(Stage, 'Stage').
      _(DWORD(Type_), 'Type').
      _(Type_VersionIndependent, 'Type_VersionIndependent').
      _(pValue, 'pValue').
    LogEnd();

  if Assigned(PValue) then
  begin
    // Check if this is an Xbox extension  :
    if DxbxTextureStageStateIsXboxExtension(Type_VersionIndependent) then
      PValue^ := XTL_EmuD3DDeferredTextureState[Stage, Ord(Type_)]
    else
      IDirect3DDevice_GetTextureStageState(g_pD3DDevice, Stage, Type_VersionIndependent, {out}pValue^);
  end;

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetRenderState
(
  State: X_D3DRENDERSTATETYPE;
  pValue: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
var
  State_VersionIndependent: X_D3DRENDERSTATETYPE;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetRenderState').
      _(Int(State), 'State').
      _(pValue, 'pValue').
    LogEnd();

  if Assigned(PValue) then
  begin
    State_VersionIndependent := DxbxVersionAdjust_D3DRS(State);

    // Check if this is an Xbox extension  :
    if DxbxRenderStateIsXboxExtension(State_VersionIndependent) then
      PValue^ := XTL_EmuMappedD3DRenderState[State_VersionIndependent]^
    else
      g_pD3DDevice.GetRenderState(EmuXB2PC_D3DRS(State_VersionIndependent), {out}PValue^);
  end;

  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetDepthStencilSurface
(
  ppZStencilSurface: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pPCSurface: XTL_PIDirect3DSurface8;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetDepthStencilSurface').
      _(ppZStencilSurface, 'ppZStencilSurface').
    LogEnd();

  if Assigned(g_pCachedZStencilSurface) then
  begin
    pPCSurface := g_pCachedZStencilSurface.Emu.Surface;
    if (pPCSurface <> nil) then
      IDirect3DSurface(pPCSurface)._AddRef();
  end;

  ppZStencilSurface^ := g_pCachedZStencilSurface;

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuD3D8 : DepthStencilSurface : ' + ResourceToString(ppZStencilSurface^));

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_GetDepthStencilSurface2(): PX_D3DSurface; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_GetDepthStencilSurface2(); >>');

  EmuSwapFS(fsXbox);

  XTL_EmuD3DDevice_GetDepthStencilSurface(@Result);
end;

(* Too high level : No patch needed, just reads g_OverscanColor@D3D@@3KA :
function XTL_EmuD3DDevice_GetOverscanColor(): D3DCOLOR; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
// Note : When we had this patch active, it removed all tree-leafs in Turok Start screen...
*)

function XTL_EmuD3DDevice_GetTile
(
  Index: DWORD;
  {CONST} pTile: PX_D3DTILE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetTile').
      _(Index, 'Index').
      _(pTile, 'pTile').
    LogEnd();

  if (pTile <> NULL) then
    memcpy(pTile, @(g_EmuD3DTileCache[Index]), sizeof(X_D3DTILE));

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

procedure XTL_EmuD3DDevice_GetTileCompressionTagBits(
  Partition: DWORD;
  Address: DWORD;
  pData: PDWORD;
  Count: DWORD
);
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetTileCompressionTagBits').
      _(Partition, 'Partition').
      _(Address, 'Address').
      _(pData, 'pData').
      _(Count, 'Count').
    LogEnd();

//  D3DDevice_GetTileCompressionTagBits(Partition, Address, pData, Count);
  Unimplemented('XTL_EmuD3DDevice_GetTileCompressionTags');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetTileCompressionTags(
  ZStartTag: DWORD;
  ZEndTag: DWORD
): DWORD;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetTileCompressionTags').
      _(ZStartTag, 'ZStartTag').
      _(ZEndTag, 'ZEndTag').
    LogEnd();

//  return D3DDevice_GetTileCompressionTags(ZStartTag, ZEndTag);
  Result := Unimplemented('XTL_EmuD3DDevice_GetTileCompressionTags');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetTileNoWait
(
  Index: DWORD;
  {CONST} pTile: PX_D3DTILE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetTileNoWait').
      _(Index, 'Index').
      _(pTile, 'pTile').
    LogEnd();

  if (pTile <> NULL) then
    memcpy(@(g_EmuD3DTileCache[Index]), pTile, sizeof(X_D3DTILE));

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

procedure XTL_EmuD3DDevice_SetTileCompressionTagBits(
  Partition: DWORD;
  Address: DWORD;
  pData: PDWORD;
  Count: DWORD
);
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetTileCompressionTagBits').
      _(Partition, 'Partition').
      _(Address, 'Address').
      _(pData, 'pData').
      _(Count, 'Count').
    LogEnd();

  //D3DDevice_SetTileCompressionTagBits(Partition, Address, pData, Count);
  Unimplemented('XTL_EmuD3DDevice_SetTileCompressionTagBits');

  EmuSwapFS(fsXbox);
end;

{$ifdef _DEBUG_TRACK_VS}
{static}var FailedShaderCount: int = 0;
{$endif}
function XTL_EmuD3DDevice_CreateVertexShader
(
  pDeclaration: PDWORD;
  pFunction: PDWORD;
  pHandle: PDWORD;
  Usage: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  pD3DVertexShader: PX_D3DVertexShader;
  pVertexShader: PVERTEX_SHADER;

  pRecompiledBuffer: XTL_LPD3DXBUFFER;
  pRecompiledDeclaration: PDWORD;
  pRecompiledFunction: PDWORD;
  VertexShaderSize: DWORD;
  DeclarationSize: DWORD;
  Handle: DWORD;
{$IFDEF DXBX_USE_D3D9}
  pDecl: Pointer; // PIDirect3DVertexDeclaration9;
{$ENDIF}

{$ifdef _DEBUG_TRACK_VS}
  pFileName: array [0..30-1] of _char;
var
  pHeader: pVSH_SHADER_HEADER;
  f: PFILE;
{$endif}
const
  dummy: P_char =
    'vs.1.1'#13#10 +
    'mov oPos, v0'#13#10;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CreateVertexShader').
      _(pDeclaration, 'pDeclaration').
      _(pFunction, 'pFunction').
      _(pHandle, 'pHandle').
      _(Usage, 'Usage').
    LogEnd();

  // create emulated shader struct
  New(pD3DVertexShader);
  ZeroMemory(pD3DVertexShader, sizeof(pD3DVertexShader^));

  pVertexShader := PVERTEX_SHADER(XboxAlloc(sizeof(VERTEX_SHADER)));
  ZeroMemory(pVertexShader, sizeof(VERTEX_SHADER));
  // TODO -oCXBX: Intelligently fill out these fields as necessary

  // CXBX HACK:
  // TODO -oCXBX: support this situation
  if (pDeclaration = NULL) then
  begin
    pHandle^ := HNULL;
    EmuSwapFS(fsXbox);
    Result := D3D_OK;
    Exit;
  end;

  pRecompiledBuffer := NULL;
  pRecompiledFunction := NULL;
  VertexShaderSize := 0;
  Handle := 0;

  hRet := XTL_EmuRecompileVshDeclaration(PDWORD(pDeclaration),
                                         @pRecompiledDeclaration,
                                         @DeclarationSize,
                                         (pFunction = NULL),
                                         @pVertexShader.VertexDynamicPatch);
  if (SUCCEEDED(hRet) and Assigned(pFunction)) then
  begin
    hRet := XTL_EmuRecompileVshFunction(PDWORD(pFunction),
                                        @pRecompiledBuffer,
                                        @VertexShaderSize,
                                        (g_VertexShaderConstantMode and X_D3DSCM_NORESERVEDCONSTANTS) > 0); // Dxbx fix
    if (SUCCEEDED(hRet)) then
    begin
      pRecompiledFunction := PDWORD(ID3DXBuffer(pRecompiledBuffer).GetBufferPointer());
    end
    else
    begin
      pRecompiledFunction := NULL;
      EmuWarning('Couldn''t recompile vertex shader function.' );
      hRet := D3D_OK; // Try using a fixed function vertex shader instead
    end;
  end;

  if MayLog(lfUnit) then
    DbgPrintf('MaxVertexShaderConst = %d', [g_D3DCaps.MaxVertexShaderConst]);

  if (SUCCEEDED(hRet)) then
  begin
    // Dxbx addition : Only fallback is a shader is given (NULL is valid input)!
    // (Test this with the CompressedVertices and Patch XDK samples.)
    if (pRecompiledFunction = NULL) and (pFunction <> NULL) then
    begin
      // Dxbx Note : As suggested by StrikerX3
      hRet := D3DERR_INVALIDCALL; // Use fallback if recompilation failed
    end
    else
    begin
{$IFDEF DXBX_USE_D3D9}
// TODO -oDxbx : Enable this, once pRecompiledDeclaration is an array of D3DVertexElement9's!
//      hRet := g_pD3DDevice.CreateVertexDeclaration
//      (
//        {pVertexElements=}PD3DVertexElement9(pRecompiledDeclaration),
//        {ppDecl=}PIDirect3DVertexDeclaration9(@pDecl)
//      );
//      if SUCCEEDED(hRet) then
        hRet := g_pD3DDevice.CreateVertexShader
        (
          pRecompiledFunction,
          PIDirect3DVertexShader9(@Handle)
        );
{$ELSE}
      hRet := g_pD3DDevice.CreateVertexShader
      (
          pRecompiledDeclaration,
          pRecompiledFunction,
          {out}Handle,
          g_dwVertexShaderUsage   // TODO -oCXBX: HACK: Xbox has extensions!
      );
{$ENDIF}
    end;

    if Assigned(pRecompiledBuffer) then
    begin
      ID3DXBuffer(pRecompiledBuffer)._Release();
      pRecompiledBuffer := NULL;
    end;

    //* Fallback to dummy shader.
    if (FAILED(hRet)) then
    begin
      EmuWarning('CreateVertexShader failed : ' + DxbxD3DErrorString(hRet));
      EmuWarning('Trying fallback:'#13#10'%s', [dummy]);
      hRet := D3DXAssembleShader(
        dummy,
        strlen(dummy),
{$IFDEF DXBX_USE_D3D9}
        {pDefines=}nil,
        {pInclude=}nil,
        {Flags=}D3DXSHADER_SKIPVALIDATION,
{$ELSE}
        {Flags=}D3DXASM_SKIPVALIDATION,
        {ppConstants}NULL,
{$ENDIF}
        {ppCompiledShader}@pRecompiledBuffer,
        {ppCompilationErrors}NULL);
      if not (FAILED(hRet)) then // Dxbx addition
      begin
{$IFDEF DXBX_USE_D3D9}
// TODO -oDxbx : What declaration should we use in this case?
//        hRet := g_pD3DDevice.CreateVertexDeclaration
//        (
//          {pVertexElements=}PD3DVertexElement9(pRecompiledDeclaration),
//          {ppDecl=}PIDirect3DVertexDeclaration9(@pDecl)
//        );
//        if SUCCEEDED(hRet) then
          hRet := g_pD3DDevice.CreateVertexShader
          (
            PDWORD(ID3DXBuffer(pRecompiledBuffer).GetBufferPointer()),
            PIDirect3DVertexShader9(Handle)
          );
{$ELSE}
        hRet := g_pD3DDevice.CreateVertexShader
        (
            pRecompiledDeclaration,
            PDWORD(ID3DXBuffer(pRecompiledBuffer).GetBufferPointer()),
            {out}Handle,
            g_dwVertexShaderUsage
        );
{$ENDIF}
      end;
    end;
    //*/
  end;

  // Dxbx addition : Prevent memory leaks
  if Assigned(pRecompiledBuffer) then
  begin
    ID3DXBuffer(pRecompiledBuffer)._Release();
    pRecompiledBuffer := NULL;
  end;

  // Save the status, to remove things later
  pVertexShader.Status := hRet;

  DxbxFree(pRecompiledDeclaration);

  pVertexShader.pDeclaration := PDWORD(XboxAlloc(DeclarationSize));
  memcpy(pVertexShader.pDeclaration, pDeclaration, DeclarationSize);

  pVertexShader.FunctionSize := 0;
  pVertexShader.pFunction := NULL;
  pVertexShader.Type_ := X_VST_NORMAL;
  pVertexShader.Size := (VertexShaderSize - sizeof(VSH_SHADER_HEADER)) div VSH_INSTRUCTION_SIZE_BYTES;
  pVertexShader.DeclarationSize := DeclarationSize;

  if (SUCCEEDED(hRet)) then
  begin
    if (pFunction <> NULL) then
    begin
      pVertexShader.pFunction := PDWORD(XboxAlloc(VertexShaderSize));
      memcpy(pVertexShader.pFunction, pFunction, VertexShaderSize);
      pVertexShader.FunctionSize := VertexShaderSize;
    end
    else
    begin
      pVertexShader.pFunction := NULL;
      pVertexShader.FunctionSize := 0;
    end;
    pVertexShader.Handle := Handle;
  end
  else
  begin
    pVertexShader.Handle := D3DFVF_XYZ or D3DFVF_TEX0;
  end;

  pD3DVertexShader.Handle := DWORD(pVertexShader);

  pHandle^ := {PX_D3DVertexShader}DWORD(pD3DVertexShader) or $80000000;

  if (FAILED(hRet)) then
  begin
{$ifdef _DEBUG_TRACK_VS}
    if Assigned(pFunction) then
    begin
      FailedShaderCount := 0;
      pHeader := PVSH_SHADER_HEADER(pFunction);
      EmuWarning('Couldn''t create vertex shader!');
      sprintf(@pFileName[0], 'failed%05d.xvu', [FailedShaderCount]);
      f := fopen(@pFileName[0], 'wb');
      if Assigned(f) then
      begin
        fwrite(pFunction, sizeof(VSH_SHADER_HEADER) + pHeader.NumInst * 16, 1, f);
        fclose(f);
      end;
      Inc(FailedShaderCount);
    end;
{$endif}
    //hRet := D3D_OK;    // marked out cxbx
  end;

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuD3D8 : CreateVertexShader: Successfully Created shader : Handle=0x%.08X (VertexShader=0x%.08X)', [pHandle^, pVertexShader]);

  EmuSwapFS(fsXbox);

  Result := hRet;
end; // XTL_EmuD3DDevice_CreateVertexShader

function XTL_EmuD3DDevice_SetPixelShaderConstant
(
  Register_: DWORD;
  {CONST} pConstantData: PVOID;
  ConstantCount: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetPixelShaderConstant').
      _(Register_, 'Register').
      _(pConstantData, 'pConstantData').
      _(ConstantCount, 'ConstantCount').
    LogEnd();

  // TODO -oDxbx: This forwards the values to the native pixel shader, but we should also pack them
  // and place them into X_D3DRS_PSCONSTANT* render state registers (using the PS_CONSTANTMAPPING macro)!
  // It would probably be better to send these values to the native pixel shader later, at drawing time.
  //
  // for i := 0 to ConstantCount - 1 do
  // begin
  //   R := pConstantData[i * 4]; // also G B A
  //   PSValue := ((Trunc(R * 255.0) and $FF) shl 16) or G .. or B .. or A;
  //   XTL_EmuMappedD3DRenderState[EmuPC2XB_PSConstant(Register_ + i)]^ := PSValue;
  // end;

{$IFDEF DXBX_USE_D3D9}
  g_pD3DDevice.SetPixelShaderConstantF
  (
    Register_,
    PSingle(pConstantData),
    ConstantCount
  );
{$ELSE}
  g_pD3DDevice.SetPixelShaderConstant
  (
    Register_,
    {untyped const}pConstantData^,
    ConstantCount
  );
{$ENDIF}

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetVertexShaderConstant
(
  Register_: DWORD;
  {CONST} pConstantData: PVOID;
  ConstantCount: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  {$IFDEF _DEBUG_TRACK_VS_CONST}
  i: uint32;
  {$ENDIF}
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetVertexShaderConstant').
      _(Register_, 'Register').
      _(pConstantData, 'pConstantData').
      _(ConstantCount, 'ConstantCount').
    LogEnd();


  // TODO -oCXBX: HACK: Since Xbox vertex shader constants range from -96 to 95, during conversion
  // some shaders need to add 96 to use ranges 0 to 191.  This fixes 3911 - 4361 games and XDK
  // samples, but breaks Turok.
  // Dxbx note : 4627 samples show that the Register value arriving in this function is already
  // incremented with 96 (even though the code for these samples supplies 0, maybe there's a
  // macro responsible for that?)
  Inc(Register_, X_D3DSCM_CORRECTION_VersionDependent);
  if MayLog(lfUnit or lfTrace) then
    DbgPrintf('Corrected constant register : 0x%.08x', [Register_]);

{$IFDEF _DEBUG_TRACK_VS_CONST}
  if ConstantCount > 0 then // Dxbx addition, to prevent underflow
  for i := 0 to ConstantCount - 1 do
  begin
    if MayLog(lfUnit) then
        DbgPrintf('SetVertexShaderConstant, c%d = { %f, %f, %f, %f }',
               [Register_ + i,
               Pfloats(pConstantData)[4 * i],
               Pfloats(pConstantData)[4 * i + 1],
               Pfloats(pConstantData)[4 * i + 2],
               Pfloats(pConstantData)[4 * i + 3]]);
  end;
{$ENDIF}

// TODO -oDxbx : This looks correct, but removes the mist from Turok menu's, so disable it for now :
//  // Check we're not getting past the current upper bound :
//  hRet := X_D3DVS_CONSTREG_COUNT;
//  if (g_VertexShaderConstantMode and X_D3DSCM_192CONSTANTS) > 0 then
//    Dec(hRet, X_D3DSCM_CORRECTION);
//  if (Register_ < (hRet-X_D3DVS_CONSTREG_COUNT)) or (Register_ >= hRet) then
//    hRet := D3DERR_INVALIDCALL
//  else
  begin
{$IFDEF DXBX_USE_D3D9}
    hRet := g_pD3DDevice.SetVertexShaderConstantF
    (
      Register_,
      PSingle(pConstantData),
      ConstantCount
    );
{$ELSE}
    hRet := g_pD3DDevice.SetVertexShaderConstant
    (
      Register_,
      pConstantData,
      ConstantCount
    );
{$ENDIF}

    if (FAILED(hRet)) then
    begin
      EmuWarning('We''re lying about setting a vertex shader constant!');

      hRet := D3D_OK;
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

procedure XTL_EmuD3DDevice_SetVertexShaderConstant1(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}const pConstantData: PVOID;
  {1 ECX}Register_: DWORD
  ); register; // VALIDATED fastcall simulation - See Translation guide
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);

    LogBegin('EmuD3DDevice_SetVertexShaderConstant1 >>').
      _(Register_, 'Register').
      _(pConstantData, 'pConstantData').
    LogEnd();

    EmuSwapFS(fsXbox);
  end;

  // Dxbx note: Shouldn't we return the result of this call?
  XTL_EmuD3DDevice_SetVertexShaderConstant(Register_, pConstantData, 1);
end;

procedure XTL_EmuD3DDevice_SetVertexShaderConstant4(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}const pConstantData: PVOID;
  {1 ECX}Register_: DWORD
  ); register; // VALIDATED fastcall simulation - See Translation guide
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);

    LogBegin('EmuD3DDevice_SetVertexShaderConstant4 >>').
      _(Register_, 'Register').
      _(pConstantData, 'pConstantData').
    LogEnd();

    EmuSwapFS(fsXbox);
  end;

  // Dxbx note: Shouldn't we return the result of this call?
  XTL_EmuD3DDevice_SetVertexShaderConstant(Register_, pConstantData, 4);
end;

procedure XTL_EmuD3DDevice_SetVertexShaderConstantNotInline(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}const pConstantData: PVOID;
  {1 ECX}Register_: DWORD;
  {3 stack}ConstantCount: DWORD
  ); register; // fastcall simulation - See Translation guide
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);

    LogBegin('EmuD3DDevice_SetVertexShaderConstantNotInline >>').
      _(Register_, 'Register').
      _(pConstantData, 'pConstantData').
      _(ConstantCount, 'ConstantCount').
    LogEnd();

    EmuSwapFS(fsXbox);
  end;

  // Dxbx note: Shouldn't we return the result of this call?
  XTL_EmuD3DDevice_SetVertexShaderConstant(Register_, pConstantData, ConstantCount div 4);
end;

{$IFDEF DXBX_PIXELSHADER_HOOKS}
procedure XTL_EmuD3DDevice_DeletePixelShader
(
  Handle: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_DeletePixelShader').
      _(Handle, 'Handle').
    LogEnd();

  if (Handle = X_PIXELSHADER_FAKE_HANDLE) then
  begin
    // Cxbx: Do Nothing!
  end
  else
  begin
{$IFDEF DXBX_USE_D3D9}
    {$MESSAGE 'fixme'}
{$ELSE}
    g_pD3DDevice.DeletePixelShader(Handle);
{$ENDIF}
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CreatePixelShader
(
  pPSDef: PX_D3DPIXELSHADERDEF;
  pHandle: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  ConvertedPixelShader: AnsiString;
  pShader: XTL_LPD3DXBUFFER;
  pErrors: XTL_LPD3DXBUFFER;
  pFunction: PDWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CreatePixelShader').
      _(pPSDef, 'pPSDef').
      _(pHandle, 'pHandle').
    LogEnd();

  // TODO -oDxbx : Delay this until drawing time, so we can support ModifyPixelShader

  // Attempt to recompile PixelShader
  ConvertedPixelShader := AnsiString(XTL_EmuRecompilePshDef(pPSDef));

  // assemble the shader
  pShader := nil;
  pErrors := nil;
  Result := D3DXAssembleShader(
    P_char(ConvertedPixelShader),
    Length(ConvertedPixelShader),
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

  if Assigned(pShader) then
  begin
    pFunction := PDWORD(ID3DXBuffer(pShader).GetBufferPointer());

    if (Result = D3D_OK) then
      // redirect to windows d3d
      Result := g_pD3DDevice.CreatePixelShader
      (
        pFunction,
{$IFDEF DXBX_USE_D3D9}
        PIDirect3DPixelShader9(pHandle) {$MESSAGE 'fixme'} // @pHandle?
{$ELSE}
        {out}pHandle^
{$ENDIF}
      );

    // Dxbx note : We must release pShader here, else we would have a resource leak!
    ID3DXBuffer(pShader)._Release;
    pShader := nil;
  end;

  if FAILED(Result) then
  begin
    pHandle^ := X_PIXELSHADER_FAKE_HANDLE;
    EmuWarning(string(AnsiString(PAnsiChar(ID3DXBuffer(pErrors).GetBufferPointer)))); // Dxbx addition
    EmuWarning(string(ConvertedPixelShader));
    EmuWarning('We''re lying about the creation of a pixel shader!');
    Result := D3D_OK;
  end;

  // Dxbx addition : We release pErrors here (or it would become a resource leak!)
  if Assigned(pErrors) then
  begin
    ID3DXBuffer(pErrors)._Release();
    pErrors := nil;
  end;

  EmuSwapFS(fsXbox);
end;

{static}var dwHandle: DWORD = 0;
function XTL_EmuD3DDevice_SetPixelShader
(
  Handle: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
const
  szDiffusePixelShader: P_char =
    'ps.1.0'#13#10 +
    'tex t0'#13#10 +
    'mov r0, t0'#13#10;
var
  pShader: XTL_LPD3DXBUFFER;
  pErrors: XTL_LPD3DXBUFFER;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetPixelShader').
      _(Handle, 'Handle').
    LogEnd();

  // TODO -oDxbx : Delay this until drawing time, so we can support ModifyPixelShader

  // redirect to windows d3d
  Result := D3D_OK;

  g_bFakePixelShaderLoaded := FALSE; // Dxbx fix : Always reset this
  // Fake Programmable Pipeline
  if (Handle = X_PIXELSHADER_FAKE_HANDLE) then
  begin
    // programmable pipeline
    //*
    if (dwHandle = 0) then
    begin
      // simplest possible pixel shader, simply output the texture input
      pShader := nil;
      pErrors := nil;

      // assemble the shader
      Result := D3DXAssembleShader(
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

      if Assigned(pShader) then
      begin
        if (Result = D3D_OK) then
          // create the shader device handle
          Result := g_pD3DDevice.CreatePixelShader(
            ID3DXBuffer(pShader).GetBufferPointer(),
{$IFDEF DXBX_USE_D3D9}
            PIDirect3DPixelShader9(@dwHandle) {$MESSAGE 'fixme'} // @dwHandle?
{$ELSE}
            {out}dwHandle
{$ENDIF}
            );

        // Dxbx note : We must release pShader here, else we would have a resource leak!
        ID3DXBuffer(pShader)._Release;
        pShader := nil;
      end;

      if (FAILED(Result)) then
      begin
        EmuWarning('Could not create pixel shader');
        EmuWarning(string(AnsiString(PAnsiChar(ID3DXBuffer(pErrors).GetBufferPointer)))); // Dxbx addition
      end;

      // Dxbx addition : We release pErrors here (or it would become a resource leak!)
      if Assigned(pErrors) then
      begin
        ID3DXBuffer(pErrors)._Release();
        pErrors := nil;
      end;
    end;

    if (not FAILED(Result)) then
      Result := g_pD3DDevice.SetPixelShader({$IFDEF DXBX_USE_D3D9}IDirect3DPixelShader9{$MESSAGE 'fixme'}{$ENDIF}(dwHandle));

    if (FAILED(Result)) then
      EmuWarning('Could not set pixel shader!' +#13#10+ DxbxD3DErrorString(Result));
    //*/

    g_bFakePixelShaderLoaded := TRUE;
  end
  // Fixed Pipeline, or Recompiled Programmable Pipeline
  else
  begin
    Result := g_pD3DDevice.SetPixelShader({$IFDEF DXBX_USE_D3D9}nil{$ELSE}Handle{$ENDIF});
  end;

  if FAILED(Result) then
  begin
    EmuWarning('We''re lying about setting a pixel shader!');

    Result := D3D_OK;
  end;

  EmuSwapFS(fsXbox);
end;
{$ENDIF DXBX_PIXELSHADER_HOOKS}

function XTL_EmuD3DDevice_CreateTexture2
(
    Width: UINT;
    Height: UINT;
    Depth: UINT;
    Levels: UINT;
    Usage: DWORD;
    Format: X_D3DFORMAT;
    D3DResource: X_D3DRESOURCETYPE
): PX_D3DResource; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRes: HRESULT;
  pTexture: PX_D3DTexture;
begin
  // Dxbx note : No EmuSwapFS needed here

  // Dxbx addition : Prevent multiplications ending up zero (or less) :
  if Width <= 0 then Width := 1;
  if Height <= 0 then Height := 1;
  if Depth <= 0 then Depth := 1;
  if Levels <= 0 then Levels := 1;

  hRes := D3D_OK-1; // TODO -oDxbx : What error code should we return?
  case D3DResource of
    X_D3DRTYPE_SURFACE: // 1
      // XTL_EmuD3DDevice_CreateDepthStencilSurface ?
      hRes := XTL_EmuD3DDevice_CreateImageSurface(Width, Height, Format, @pTexture); // Dxbx addition

    X_D3DRTYPE_TEXTURE: // 3
      hRes := XTL_EmuD3DDevice_CreateTexture(Width, Height, Levels, Usage, Format, D3DPOOL_MANAGED, @pTexture);

    X_D3DRTYPE_VOLUMETEXTURE: // 4
      hRes := XTL_EmuD3DDevice_CreateVolumeTexture(Width, Height, Depth, Levels, Usage, Format, D3DPOOL_MANAGED, @pTexture);

    X_D3DRTYPE_CUBETEXTURE: // 5
      // TODO -oDxbx : Check what the actual EdgeLength value should be
      hRes := XTL_EmuD3DDevice_CreateCubeTexture({EdgeLength=?}Width, Levels, Usage, Format, D3DPOOL_MANAGED, @pTexture);

    X_D3DRTYPE_VERTEXBUFFER: // 6
      // TODO -oDxbx : Check what the actual Length and FVF values should be :
      hRes := XTL_EmuD3DDevice_CreateVertexBuffer({Length=}Width * Height * Depth * Levels, Usage, {FVF=?}Format, D3DPOOL_MANAGED, @pTexture); // Dxbx addition

    X_D3DRTYPE_INDEXBUFFER: // 7
      // TODO -oDxbx : Check what the actual Length value should be :
      hRes := XTL_EmuD3DDevice_CreateIndexBuffer({Length=}Width * Height * Depth * Levels, Usage, Format, D3DPOOL_MANAGED, @pTexture); // Dxbx addition
(*
    X_D3DRTYPE_PALETTE: // 9
      // TODO -oDxbx : Check what the actual Size value should be :
      // Choose D3DPALETTE_256 / D3DPALETTE_128 / D3DPALETTE_64 / D3DPALETTE_32 or D3DPALETTE_MAX,
      // based on Width, Height, Depth or Levels
      // XTL_EmuD3DDevice_CreatePalette({Size=}?, @pTexture); // Dxbx addition
*)
  else // X_D3DRTYPE_NONE{=0}, X_D3DRTYPE_VOLUME{=2}, X_D3DRTYPE_PUSHBUFFER{=8}, X_D3DRTYPE_FIXUP{=10} :
    EmuSwapFS(fsWindows);
    DxbxKrnlCleanup('D3DResource := %d is not supported!', [Ord(D3DResource)]);
    EmuSwapFS(fsXbox);
  end;

  if hRes = D3D_OK then
    Result := pTexture
  else
    Result := nil;
end;

function XTL_EmuD3DDevice_CreateTexture
(
  Width: UINT;
  Height: UINT;
  Levels: UINT;
  Usage: DWORD;
  Format: X_D3DFORMAT;
  Pool: X_D3DPOOL;
  ppTexture: PPX_D3DTexture
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  PCFormat: D3DFORMAT;
  LockedRect: D3DLOCKED_RECT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CreateTexture').
      _(Width, 'Width').
      _(Height, 'Height').
      _(Levels, 'Levels').
      _(Usage, 'Usage').
      _(Format, 'Format').
      _(Pool, 'Pool').
      _(ppTexture, 'ppTexture').
    LogEnd();

  // Convert Format (Xbox->PC)
  PCFormat := DxbxXB2PC_D3DFormat(Format, D3DRTYPE_TEXTURE);

  New({var PX_D3DTexture}ppTexture^);
  ZeroMemory(ppTexture^, SizeOf(ppTexture^^));

  if (PCFormat = D3DFMT_YUY2) then
  begin
    // cache the overlay size
    g_dwOverlayW := Width;
    g_dwOverlayH := Height;
    g_dwOverlayP := RoundUp(g_dwOverlayW, 64) * 2;

    DxbxInitializePixelContainerYUY2(ppTexture^);

    g_YuvSurface := PX_D3DSurface(ppTexture^);

    Result := D3D_OK;
  end
  else // PCFormat <> D3DFMT_YUY2
  begin
    // Dxbx addition : Limit the usages we accept :
    Usage := Usage and (D3DUSAGE_RENDERTARGET or D3DUSAGE_DEPTHSTENCIL);

    // Dxbx addition : If this texture is going to be a depth stencil, use the correct format for that :
    if (Usage and D3DUSAGE_DEPTHSTENCIL) > 0 then
    begin
      if g_pD3D.CheckDeviceFormat(
        g_EmuCDPD.CreationParameters.AdapterOrdinal,
        g_EmuCDPD.CreationParameters.DeviceType,
        g_EmuCDPD.NativePresentationParameters.BackBufferFormat,
        D3DUSAGE_DEPTHSTENCIL,
        D3DRTYPE_TEXTURE,
        D3DFMT_D24S8) = D3D_OK then
      begin
        // Note : XDK sample ZSprite fakes this anyhow by putting D3DFMT_LIN_D24S8 in the Resource.Format!
        PCFormat := D3DFMT_D24S8;
        EmuWarning('Overriding texture format into D3DFMT_D24S8 to support D3DUSAGE_DEPTHSTENCIL');
      end
      else
      begin
        Usage := Usage and (not D3DUSAGE_DEPTHSTENCIL);
        EmuWarning('Removing the D3DUSAGE_DEPTHSTENCIL flag, as you card doesn''t support this format!');
      end;
    end;

    Pool := D3DPOOL_MANAGED;

    EmuAdjustTextureDimensions(D3DRTYPE_TEXTURE, @Width, @Height);

//{$IFDEF GAME_HACKS_ENABLED}??
    // Cxbx HACK: Width and Height sometimes set to 0xFFFF and 0 in Crazy Taxi 3
    // When it fails, it returns a success anyway and D3DResource::Register
    // sees it as a VertexBuffer instead of a Texture.
    if (Width >= $FFFF) or (Height >= $FFFF) then
    begin
      Width := 256;
      Height := 256;
      PCFormat := D3DFMT_A8R8G8B8;

      EmuWarning('Overriding texture parameters:'#13#10 +
        'Width:  0xFFFF -> 256'#13#10 +
        'Height: 0xFFFF -> 256'#13#10 +
        'Format: D3DFMT_A8R8G8B8' );
    end;

    if (Width = 0) or (Height = 0) then
    begin
      Width := 256; Height := 256;
      EmuWarning('Overriding texture parameters:'#13#10 +
        'Width:  0 -> 256'#13#10 +
        'Height: 0 -> 256');
    end;

//    // Dxbx hack : Any lineair texture that's not screen-sized becomes a potential render target :
//    if (Width <> 640) or (Height <> 480) then
//      if EmuXBFormatIsLinear(Format) then
//        PCUsage := (D3DUSAGE_RENDERTARGET);

    if (Usage and (D3DUSAGE_RENDERTARGET or D3DUSAGE_DEPTHSTENCIL)) > 0 then
//    if (Usage and (D3DUSAGE_RENDERTARGET)) > 0 then
      Pool := D3DPOOL_DEFAULT;

    Result := IDirect3DDevice_CreateTexture
    (g_pD3DDevice,
      Width, Height, Levels,
      Usage, // TODO -oCXBX: Xbox Allows a border to be drawn (maybe hack this in software ;[)
      PCFormat, Pool, @(ppTexture^.Emu.Texture)
    );

    if (FAILED(Result)) then
    begin
      EmuWarning('CreateTexture Failed!' +#13#10+ DxbxD3DErrorString(Result));
      ppTexture^.Data := $BEADBEAD;
    end
    else
    begin
      // Dxbx addition : Initialize Common field properly :
      ppTexture^.Common := ({RefCount=}1 and X_D3DCOMMON_REFCOUNT_MASK) or X_D3DCOMMON_TYPE_TEXTURE or X_D3DCOMMON_D3DCREATED;

      // Dxbx addition : Request how many where created (as Levels could be 0) :
      Levels := IDirect3DTexture(ppTexture^.Emu.Texture).GetLevelCount();

      if MayLog(lfUnit or lfReturnValue) then
        DbgPrintf('Texture created @ 0x%.08X [hRet = 0x%.08X]: %dx%d, %d levels, usage = 0x%.08X, format = 0x%.08X', [
            ppTexture^.Emu.Texture, Result, Width, Height, Levels, Ord(Usage), Ord(PCFormat)]);

      ppTexture^.Format := (Ord(Format) shl X_D3DFORMAT_FORMAT_SHIFT)
                        or (Log2(Width) shl X_D3DFORMAT_USIZE_SHIFT)
                        or (Log2(Height) shl X_D3DFORMAT_VSIZE_SHIFT)
                        or ({Pitch=?}Log2(Width) shl X_D3DFORMAT_PSIZE_SHIFT)
                        or (Levels shl X_D3DFORMAT_MIPMAP_SHIFT);

      // Dxbx addition : Register all levels :
      while Levels > 0 do
      begin
        Dec(Levels);
        // Dxbx addition : Check if LockRect actually succeeds :
        if IDirect3DTexture(ppTexture^.Emu.Texture).LockRect(Levels, {out}LockedRect, NULL, 0) = D3D_OK then
        begin
          if Levels = 0 then
            ppTexture^.Data := DWORD(LockedRect.pBits);

          g_DataToTexture.insert(DWORD(LockedRect.pBits), ppTexture^);

          IDirect3DTexture(ppTexture^.Emu.Texture).UnlockRect(Levels);
        end;
      end;
    end;

    if MayLog(lfUnit or lfReturnValue) then
      DbgPrintf('EmuD3D8 : CreateTexture : Successfully Created Texture : ' + ResourceToString(ppTexture^));
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CreateVolumeTexture
(
    Width: UINT;
    Height: UINT;
    Depth: UINT;
    Levels: UINT;
    Usage: DWORD;
    Format: X_D3DFORMAT;
    Pool: X_D3DPOOL;
    ppVolumeTexture: PPX_D3DVolumeTexture
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  PCFormat: D3DFORMAT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CreateVolumeTexture').
      _(Width, 'Width').
      _(Height, 'Height').
      _(Depth, 'Depth').
      _(Levels, 'Levels').
      _(Usage, 'Usage').
      _(Format, 'Format').
      _(Pool, 'Pool').
      _(ppVolumeTexture, 'ppVolumeTexture').
    LogEnd();

  // Convert Format (Xbox->PC)
  PCFormat := DxbxXB2PC_D3DFormat(Format, D3DRTYPE_VOLUMETEXTURE);

  New({PX_D3DVolumeTexture}ppVolumeTexture^);
  ZeroMemory(ppVolumeTexture^, SizeOf(ppVolumeTexture^^));

  if (PCFormat = D3DFMT_YUY2) then
  begin
    // cache the overlay size
    g_dwOverlayW := Width;
    g_dwOverlayH := Height;
    g_dwOverlayP := RoundUp(g_dwOverlayW, 64) * 2; // Pitch should be in 64 byte increments; 2 bytes per pixel

    // create texture resource
    DxbxInitializePixelContainerYUY2(ppVolumeTexture^);

    hRet := D3D_OK;
  end
  else // PCFormat <> D3DFMT_YUY2
  begin
    // Dxbx addition :
    EmuAdjustTextureDimensions(D3DRTYPE_VOLUMETEXTURE, @Width, @Height);

    hRet := IDirect3DDevice_CreateVolumeTexture(g_pD3DDevice,
        Width, Height, Depth, Levels,
        0,  // TODO -oCXBX: Xbox Allows a border to be drawn (maybe hack this in software ;[)
        PCFormat, D3DPOOL_MANAGED, @(ppVolumeTexture^.Emu.VolumeTexture));

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('CreateVolumeTexture Failed! (0x%.08X)' +#13#10+ DxbxD3DErrorString(hRet), [hRet]);

    // Dxbx addition : Initialize Common field properly :
    ppVolumeTexture^.Common := ({RefCount=}1 and X_D3DCOMMON_REFCOUNT_MASK) or X_D3DCOMMON_TYPE_TEXTURE or X_D3DCOMMON_D3DCREATED;

    if MayLog(lfUnit or lfReturnValue) then
      DbgPrintf('EmuD3D8 : CreateVolumeTexture : Successfully Created Volume Texture : ' + ResourceToString(ppVolumeTexture^));
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuD3DDevice_CreateCubeTexture
(
    EdgeLength: UINT;
    Levels: UINT;
    Usage: DWORD;
    Format: X_D3DFORMAT;
    Pool: X_D3DPOOL;
    ppCubeTexture: PPX_D3DCubeTexture
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  PCFormat: D3DFORMAT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CreateCubeTexture').
      _(EdgeLength, 'EdgeLength').
      _(Levels, 'Levels').
      _(Usage, 'Usage').
      _(Format, 'Format').
      _(Pool, 'Pool').
      _(ppCubeTexture, 'ppCubeTexture').
    LogEnd();

  PCFormat := DxbxXB2PC_D3DFormat(Format, D3DRTYPE_CUBETEXTURE);

  New({var PX_D3DCubeTexture}ppCubeTexture^);
  ZeroMemory(ppCubeTexture^, SizeOf(ppCubeTexture^^));

  if (Usage and D3DUSAGE_RENDERTARGET) > 0 then
    Pool := D3DPOOL_DEFAULT
  else
    Pool := D3DPOOL_MANAGED;

  // Dxbx addition :
  EmuAdjustTextureDimensions(D3DRTYPE_CUBETEXTURE, @EdgeLength, @EdgeLength);

  Result := IDirect3DDevice_CreateCubeTexture(g_pD3DDevice,
      EdgeLength,
      Levels,
      Usage,  // TODO -oCXBX: Xbox Allows a border to be drawn (maybe hack this in software ;[)
      PCFormat,
      Pool, // Dxbx note : D3DPOOL_MANAGED makes CubeMap crash!
      @(ppCubeTexture^.Emu.CubeTexture)
  );

  if (FAILED(Result)) then
    DxbxKrnlCleanup('CreateCubeTexture Failed!' +#13#10+ DxbxD3DErrorString(Result));

  // Dxbx addition : Initialize Common field properly :
  ppCubeTexture^.Common := ({RefCount=}1 and X_D3DCOMMON_REFCOUNT_MASK) or X_D3DCOMMON_TYPE_TEXTURE or X_D3DCOMMON_D3DCREATED;
//  DxbxUpdatePixelContainer(ppCubeTexture^, X_D3DCOMMON_TYPE_TEXTURE, {Width=}EdgeLength, {Height=}EdgeLength, BPP, 3, 0, Levels, IsSwizzled, IsCompressed, 0, IsCubeMap, CacheFormat);

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuD3D8 : CreateCubeTexture : Successfully Created Cube Texture : ' + ResourceToString(ppCubeTexture^));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CreateIndexBuffer
(
  Length: UINT;
  Usage: DWORD;
  Format: X_D3DFORMAT;
  Pool: X_D3DPOOL;
  ppIndexBuffer: PPX_D3DIndexBuffer
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  pData: PBYTE;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('EmuD3DDevice_CreateIndexBuffer').
      _(Length, 'Length').
      _(Usage, 'Usage').
      _(Format, 'Format').
      _(Pool, 'Pool').
      _(ppIndexBuffer, 'ppIndexBuffer').
    LogEnd();

    if Format <> X_D3DFMT_INDEX16 then
      EmuWarning('XTL_EmuD3DDevice_CreateIndexBuffer: Weird index format - will be ignored anyway.');

    DbgPrintf('Number of indexes : %d', [Length div SizeOf(Word)]);
  end;

  New({var PX_D3DIndexBuffer}ppIndexBuffer^);
  ZeroMemory(ppIndexBuffer^, SizeOf(ppIndexBuffer^^));

  hRet := IDirect3DDevice_CreateIndexBuffer(g_pD3DDevice,
      Length, {Usage=}0, D3DFMT_INDEX16, D3DPOOL_MANAGED,
      @(ppIndexBuffer^.Emu.IndexBuffer));

  if (FAILED(hRet)) then
    DxbxKrnlCleanup('XTL_EmuD3DDevice_CreateIndexBuffer: IndexBuffer Create Failed!' +#13#10+ DxbxD3DErrorString(hRet));

  // Dxbx addition : Initialize Common field properly :
  ppIndexBuffer^.Common := ({RefCount=}1 and X_D3DCOMMON_REFCOUNT_MASK) or X_D3DCOMMON_TYPE_INDEXBUFFER or X_D3DCOMMON_D3DCREATED;

  // update data ptr
  pData := NULL;
  if (FAILED(IDirect3DIndexBuffer(ppIndexBuffer^.Emu.IndexBuffer).Lock(0, Length, {out}TLockData(pData), 0))) then
    DxbxKrnlCleanup('XTL_EmuD3DDevice_CreateIndexBuffer: IndexBuffer Lock Failed!' +#13#10+ DxbxD3DErrorString(hRet));

  ppIndexBuffer^.Data := DWORD(pData);

  // StrikerX3: experimenting...
  IDirect3DIndexBuffer(ppIndexBuffer^.Emu.IndexBuffer).Unlock();

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuD3D8 : CreateIndexBuffer: Successfully Created IndexBuffer : ' + ResourceToString(ppIndexBuffer^));

  EmuSwapFS(fsXbox);

  Result := hRet;
end;


function XTL_EmuD3DDevice_CreateIndexBuffer2(Length: UINT): PX_D3DIndexBuffer; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('XTL_EmuD3DDevice_CreateIndexBuffer2 >>').
      _(Length, 'Length').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  Result := NULL;

  XTL_EmuD3DDevice_CreateIndexBuffer
  (
      Length,
      {Usage=}0,
      X_D3DFMT_INDEX16,
      D3DPOOL_MANAGED,
      @Result
  );
end;


function XTL_EmuD3DDevice_SetIndices
(
  pIndexData: PX_D3DIndexBuffer;
  BaseVertexIndex: UINT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
label
  fail;
var
  pPCIndexBuffer: XTL_PIDirect3DIndexBuffer8;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetIndices').
      _(pIndexData, 'pIndexData').
      _(BaseVertexIndex, 'BaseVertexIndex').
    LogEnd();

  { // Commented out by CXBX
  fflush(stdout);
  if (pIndexData <> 0) then
  begin
    chk := 0; // Dxbx : this should become a writeable const
    if (chk++ = 0) then
    begin
     //  asm int 3 end;  NO ASM IN 3 THIS CRASH
    end;
  end;
  }

  Result := D3D_OK;

  g_dwBaseVertexIndex := BaseVertexIndex;

  if (pIndexData <> nil) then
  begin
    g_pIndexBuffer := pIndexData;

    if IsRunning(TITLEID_Halo) then // HACK: Halo Hack
      if (pIndexData.Emu.Lock = $00840863) then
        pIndexData.Emu.Lock := 0;

    EmuVerifyResourceIsRegistered(pIndexData);

    if IsRunning(TITLEID_UnrealChampionship) then // HACK: Unreal Championship
    begin
      if ((pIndexData.Emu.Lock and $FFFF0000) = $00490000) or ((pIndexData.Emu.Lock and $F0000000) <> $00000000) or
          (pIndexData.Emu.Lock = $10) then
      begin
        Result := E_FAIL;
        goto fail;
      end;
    end;

    pPCIndexBuffer := pIndexData.Emu.IndexBuffer;
    DxbxUnlockD3DResource(pIndexData); // Dxbx addition
    if (pIndexData.Emu.Lock <> X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
      Result := g_pD3DDevice.SetIndices(IDirect3DIndexBuffer(pPCIndexBuffer){$IFDEF DXBX_USE_D3D9}{$MESSAGE 'fixme'}{$ELSE}, BaseVertexIndex{$ENDIF});
  end
  else
  begin
    g_pIndexBuffer := nil;

    Result := g_pD3DDevice.SetIndices(nil{$IFDEF DXBX_USE_D3D9}{$MESSAGE 'fixme'}{$ELSE}, BaseVertexIndex{$ENDIF});
  end;

fail:
  EmuSwapFS(fsXbox);
end;

{$ifdef _DEBUG_DUMP_TEXTURE_SETTEXTURE}
{static}var dwDumpTexture: int = 0;
{$endif}
function XTL_EmuD3DDevice_SetTexture
(
    Stage: DWORD;
    pTexture: PX_D3DBaseTexture
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pPCBaseTexture: XTL_PIDirect3DBaseTexture8;
{$ifdef _DEBUG_DUMP_TEXTURE_SETTEXTURE}
  szBuffer: array [0..256-1] of _char;
  face: int;
{$endif}
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetTexture').
      _(Stage, 'Stage').
      _(pTexture, 'pTexture').
    LogEnd();

  pPCBaseTexture := NULL;

  g_EmuD3DActiveTexture[Stage] := pTexture;

  if (pTexture <> NULL) then
  begin
    EmuVerifyResourceIsRegistered(pTexture);

    if IsSpecialResource(pTexture.Data) and ((pTexture.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0) then
    begin
      //
      // NOTE: TODO -oCXBX: This is almost a hack!
      //

      EmuSwapFS(fsXbox);
      XTL_EmuD3DDevice_EnableOverlay(BOOL_TRUE);
      XTL_EmuD3DDevice_UpdateOverlay(PX_D3DSurface(pTexture), nil, nil, BOOL_FALSE, 0);
      EmuSwapFS(fsWindows);
    end
    else
    begin
      pPCBaseTexture := pTexture.Emu.BaseTexture;

      {$ifdef _DEBUG_DUMP_TEXTURE_SETTEXTURE}
        if (pTexture <> NULL) and (pTexture.Emu.Texture <> NULL) then
        begin
          // dwDumpTexture := 0; // Dxbx note : Do not reset 'static' var

          case (IDirect3DResource(pTexture.Emu.Resource).GetType()) of
            D3DRTYPE_TEXTURE:
            begin
              sprintf(@szBuffer[0], 'SetTextureNorm - %.03d (0x%.08X).bmp', [dwDumpTexture, UIntPtr(pTexture.Emu.Texture)]);
              Inc(dwDumpTexture);
              IDirect3DTexture(pTexture.Emu.Texture).UnlockRect(0);

              D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, IDirect3DTexture(pTexture.Emu.Texture), NULL);
            end;

            D3DRTYPE_CUBETEXTURE:
            begin
              face := 0;
              while face < 6 do
              begin
                Inc(dwDumpTexture);
                sprintf(@szBuffer[0], 'SetTextureCube%d - %.03d (0x%.08X).bmp', [face, dwDumpTexture, UIntPtr(pTexture.Emu.Texture)]);
                IDirect3DTexture(pTexture.Emu.CubeTexture).UnlockRect(face);
                D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, IDirect3DTexture(pTexture.Emu.Texture), NULL);
                Inc(Face);
              end;
            end;
          end;
        end;
      {$endif}
    end;
  end;

  (* --- MARKED OUT BY CXBX --
    IDirect3DTexture *pDummyTexture[X_D3DTS_STAGECOUNT] := (0, 0, 0, 0);

    if (pDummyTexture[Stage] = 0) then
    begin
        if (Stage = 0) then
        begin
            if (D3DXCreateTextureFromFile(g_pD3DDevice, DxbxDebugFolder +'\dummy1.bmp', @pDummyTexture[Stage]) <> D3D_OK) then
                DxbxKrnlCleanup('Could not create dummy texture!');
        end
        else if (Stage = 1) then
        begin
            if (D3DXCreateTextureFromFile(g_pD3DDevice, DxbxDebugFolder +'\dummy2.bmp', @pDummyTexture[Stage]) <> D3D_OK) then
                DxbxKrnlCleanup('Could not create dummy texture!');
        end;
     end;
  //*)

  (* -- MARKED OUT BY CXBX
  int dwDumpTexture := 0;
  szBuffer: array [0..256-1] of _char;
  sprintf(szBuffer, DxbxDebugFolder +'\Textures\DummyTexture - %.03d (0x%.08X).bmp', [dwDumpTexture, pDummyTexture]); Inc(dwDumpTexture);
  pDummyTexture.UnlockRect(0);
  D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, pDummyTexture, 0);
  //*)

  // Dxbx Note : As suggested by StrikerX3, as a fix for missing textures in Panzer :
  // Make sure the texture has no locks, otherwise we get blank polygons
  DxbxUnlockD3DResource(pTexture);

  // hRet = g_pD3DDevice.SetTexture(Stage, pDummyTexture[Stage]); // MARKED OUT BY CXBX
//  Result := g_pD3DDevice.SetTexture(Stage, IDirect3DBaseTexture(iif((g_iWireframe = 0), pPCBaseTexture, nil)));
  Result := g_pD3DDevice.SetTexture(Stage, IDirect3DBaseTexture(pPCBaseTexture));

  if FAILED(Result) then
  begin
    EmuWarning('SetTextureFailed!' +#13#10+ DxbxD3DErrorString(Result));
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SwitchTexture
(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Data: DWORD;
  {1 ECX}Method: X_NV2AMETHOD;
  {3 stack}Format: DWORD
); register; // fastcall simulation - See Translation guide
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
const
  StageLookup: array [0..X_D3DTS_STAGECOUNT-1] of DWORD = ( $00081b00, $00081b40, $00081b80, $00081bc0 );
var
  Stage: DWORD;
  v: int;
  pTexture: PX_D3DTexture;
{  hRet: HRESULT;}
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SwitchTexture').
      _(Method, 'Method').
      _(Data, 'Data').
      _(Format, 'Format').
    LogEnd();

  Stage := DWord(-1);

  for v := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    if (StageLookup[v] = Method) then
    begin
      Stage := v;
      Break;
    end;
  end;

  if (Stage = DWord(-1)) then
  begin
    EmuWarning('Unknown Method (0x%.08X)', [Method]);
  end
  else
  begin
    // WARNING: TODO -oCXBX: Correct reference counting has not been completely verified for this code

    pTexture := g_DataToTexture.get(Data);

    EmuWarning('Switching Texture %s @ Stage %d', [ResourceToString(pTexture), Stage]);

    DxbxUnlockD3DResource(pTexture); // Dxbx addition : Attempt to fix missing textures
    {hRet := }g_pD3DDevice.SetTexture(Stage, IDirect3DBaseTexture(pTexture.Emu.BaseTexture));

    // TODO -oDxbx : Shouldn't we do this too? :
    // g_EmuD3DActiveTexture[Stage] := pTexture;
    // TODO -oDxbx : Or even call XTL_EmuD3DDevice_SetTexture which already does this (and more)?


    { MARKED OUT BY CXBX
    if (pTexture.Emu.BaseTexture <> 0) then
    begin
         Integer dwDumpTexture := 0;

         szBuffer: array [0..255-1] of _char;

        sprintf(szBuffer, DxbxDebugFolder +'\Textures\0x%.08X-SwitchTexture%.03d.bmp', [pTexture, dwDumpTexture]); Inc(dwDumpTexture);

        pTexture.Emu.Texture.UnlockRect(0);

        D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, pTexture.Emu.BaseTexture, 0);
     end;
    }
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetDisplayMode
(
  pMode: PX_D3DDISPLAYMODE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  PCDisplayMode: D3DDISPLAYMODE;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetDisplayMode').
      _(pMode, 'pMode').
    LogEnd();

  // make adjustments to parameters to make sense with windows d3d
  begin
    Result := g_pD3DDevice.GetDisplayMode({$IFDEF DXBX_USE_D3D9}{iSwapChain=}0,{$ENDIF} {out}PCDisplayMode);

    // TODO -oCXBX: Retrieve from current CreateDevice settings?
    pMode.Width := 640;
    pMode.Height := 480;
    pMode.RefreshRate := PCDisplayMode.RefreshRate;
    // Convert Format (PC->Xbox)
    pMode.Format := EmuPC2XB_D3DFormat(PCDisplayMode.Format);
    // TODO -oCXBX: Make this configurable in the future?
    pMode.Flags := X_D3DPRESENTFLAG_FIELD or X_D3DPRESENTFLAG_INTERLACED or X_D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_Begin
(
    PrimitiveType: X_D3DPRIMITIVETYPE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_Begin').
      _(PrimitiveType, 'PrimitiveType').
    LogEnd();

  g_IVBPrimitiveType := PrimitiveType;
  g_IVBFVF := 0;
  g_IVBTblOffs := 0;

  // Note : The g_IVBTable array doesn't need to be cleared, as that's already being done
  // in DxbxSetVertexData (when growing) and in XTL_EmuFlushIVB (after conversion).

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_SetVertexData2f
(
    Register_: X_D3DVSDE;
    a: FLOAT;
    b: FLOAT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('EmuD3DDevice_SetVertexData2f').
      _(Register_, 'Register').
      _(a, 'a').
      _(b, 'b').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  // TODO -oDxbx : Handle Vertex Attributes that need a Color (in this case, r,g,b,a=0.0-1.0)
  Result := DxbxSetVertexData(Register_, a, b, 0.0, 1.0);
end;


function FtoDW(f: FLOAT): DWORD;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  Result := PDWORD(@f)^;
end;

function DWtoF(f: DWORD): FLOAT;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  Result := PFLOAT(@f)^;
end;


function XTL_EmuD3DDevice_SetVertexData2s
(
    Register_: X_D3DVSDE;
    a: SHORT;
    b: SHORT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  fA, fB: FLOAT;
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('EmuD3DDevice_SetVertexData2s').
      _(Register_, 'Register').
      _(a, 'a').
      _(b, 'b').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  fA := a; fB := b;

  if (Register_ = X_D3DVSDE_DIFFUSE) or (Register_ = X_D3DVSDE_SPECULAR) then
  begin
    fA := fA / High(a);
    fB := fB / High(b);
  end;

  // TODO -oDxbx : Handle Vertex Attributes that need a Color (in this case, r,g,b,a=0 or 1)
  Result := DxbxSetVertexData(Register_, fA, fB, 0.0, 1.0);
end;

function XTL_EmuD3DDevice_SetVertexData4f
(
    Register_: X_D3DVSDE;
    a: FLOAT;
    b: FLOAT;
    c: FLOAT;
    d: FLOAT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('EmuD3DDevice_SetVertexData4f').
      _(Register_, 'Register').
      _(a, 'a').
      _(b, 'b').
      _(c, 'c').
      _(d, 'd').
  //    _(g_IVBTblOffs, '## g_IVBTblOffs ##'). // test - show counter
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  Result := DxbxSetVertexData(Register_, a, b, c, d);
end;

// Note : We pass all SetVertexData* calls through this method, while the Xbox implements
// these by just pushing the supplied values into different pushbuffer commands without
// any conversion. Since we have no pushbuffer, our SetVertexData* functions convert
// their arguments into one common format : Float's.
// TODO : Research & document a few samples, so that we can establish the needed conversions,
// as our current SetVertexData* implementations contain just an educated guess.
function DxbxSetVertexData(const Register_: X_D3DVSDE; const a, b, c, d: FLOAT): HRESULT;

  function _abcdAsD3DCOLOR: D3DCOLOR;
  begin
    // TODO -oDxbx : Is this the correct way to check if Alpha should be set to 1?
    if not EmuXBFormatHasAlpha(g_EmuCDPD.pPresentationParameters.BackBufferFormat) then
      // Tests with XSokoban show that this is needed in order to make the text non-transparent :
      Result := D3DCOLOR_COLORVALUE({Red=}a, {Green=}b, {Blue=}c, {Alpha=}1.0)
    else
      Result := D3DCOLOR_COLORVALUE({Red=}a, {Green=}b, {Blue=}c, {Alpha=}d);
  end;

  procedure _Error;
  begin
    DxbxKrnlCleanup('Unknown IVB Register: %d', [Register_]);
  end;

const
  GROWSIZE = 128;
var
  g_IVBTable_o: PD3DIVB;
begin
  Result := D3D_OK;

  // Check if g_IVBTable has enough space for the current g_IVBTblOffs
  // (and one extra for the "Copy current color to next vertex" case) :
  if Length(g_IVBTable) <= (int(g_IVBTblOffs) + 1) then
  begin
    // Grow with 128 vertices at a time :
    SetLength(g_IVBTable, g_IVBTblOffs + GROWSIZE);

    // default values
    ZeroMemory(@g_IVBTable[g_IVBTblOffs], sizeof(g_IVBTable[0])*GROWSIZE);

    // Make sure we also copy the last vertex to the next, at resize time :
    if g_IVBTblOffs > 0 then
      g_IVBTable[g_IVBTblOffs] := g_IVBTable[g_IVBTblOffs - 1];
  end;

  g_IVBTable_o := @g_IVBTable[g_IVBTblOffs];

  case Cardinal(Register_) of
    // TODO -oCXBX: Blend weight.

    {0=}X_D3DVSDE_POSITION:
      begin
        g_IVBTable_o.Position.x := a;
        g_IVBTable_o.Position.y := b;
        g_IVBTable_o.Position.z := c;
        g_IVBTable_o.Rhw := d; //1.0; // Dxbx note : Why set Rhw to 1.0? And why ignore d?

        Inc(g_IVBTblOffs);

        g_IVBFVF := g_IVBFVF or D3DFVF_XYZRHW;
      end;

    {1=}X_D3DVSDE_BLENDWEIGHT:
      begin
        g_IVBTable_o.Position.x := a;
        g_IVBTable_o.Position.y := b;
        g_IVBTable_o.Position.z := c;
        g_IVBTable_o.Blend1 := d;

        Inc(g_IVBTblOffs);

        g_IVBFVF := g_IVBFVF or D3DFVF_XYZB1;
      end;

    {2=}X_D3DVSDE_NORMAL:
      begin
        g_IVBTable_o.Normal.x := a;
        g_IVBTable_o.Normal.y := b;
        g_IVBTable_o.Normal.z := c;

        Inc(g_IVBTblOffs);

        g_IVBFVF := g_IVBFVF or D3DFVF_NORMAL;
      end;

   {3=}X_D3DVSDE_DIFFUSE:
      begin
        g_IVBTable_o.dwDiffuse := _abcdAsD3DCOLOR;

        g_IVBFVF := g_IVBFVF or D3DFVF_DIFFUSE;
      end;

    {4=}X_D3DVSDE_SPECULAR:
      begin
        g_IVBTable_o.dwSpecular := _abcdAsD3DCOLOR;

        g_IVBFVF := g_IVBFVF or D3DFVF_SPECULAR;
      end;

    {9=}X_D3DVSDE_TEXCOORD0:
      begin
        g_IVBTable_o.TexCoord1.x := a;
        g_IVBTable_o.TexCoord1.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX1) then
        begin
          // Dxbx fix : Use mask, else the format might get expanded incorrectly :
          g_IVBFVF := (g_IVBFVF and (not D3DFVF_TEXCOUNT_MASK)) or D3DFVF_TEX1;
          // Dxbx note : Correct usage of D3DFVF_TEX1 (and the other cases below)
          // can be tested with "Daphne Xbox" (the Laserdisc Arcade Game Emulator).
        end;
      end;

    {10=}X_D3DVSDE_TEXCOORD1:
      begin
        g_IVBTable_o.TexCoord2.x := a;
        g_IVBTable_o.TexCoord2.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX2) then
        begin
          // Dxbx fix : Use mask, else the format might get expanded incorrectly :
          g_IVBFVF := (g_IVBFVF and (not D3DFVF_TEXCOUNT_MASK)) or D3DFVF_TEX2;
        end;
      end;

    {11=}X_D3DVSDE_TEXCOORD2:
      begin
        g_IVBTable_o.TexCoord3.x := a;
        g_IVBTable_o.TexCoord3.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX3) then
        begin
          // Dxbx fix : Use mask, else the format might get expanded incorrectly :
          g_IVBFVF := (g_IVBFVF and (not D3DFVF_TEXCOUNT_MASK)) or D3DFVF_TEX3;
        end;
      end;

    {12=}X_D3DVSDE_TEXCOORD3:
      begin
        g_IVBTable_o.TexCoord4.x := a;
        g_IVBTable_o.TexCoord4.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX4) then
        begin
          // Dxbx fix : Use mask, else the format might get expanded incorrectly :
          g_IVBFVF := (g_IVBFVF and (not D3DFVF_TEXCOUNT_MASK)) or D3DFVF_TEX4;
        end;
      end;

    { $FFFFFFFF=}X_D3DVSDE_VERTEX:
    begin
      g_IVBTable_o.Position.x := a;
      g_IVBTable_o.Position.y := b;
      g_IVBTable_o.Position.z := c;
      g_IVBTable_o.Rhw := d;

      Inc(g_IVBTblOffs);

      // Copy current color to next vertex
      g_IVBTable[g_IVBTblOffs].dwDiffuse := g_IVBTable_o.dwDiffuse;
      g_IVBTable[g_IVBTblOffs].dwSpecular := g_IVBTable_o.dwSpecular;
      // Dxbx note : Must we copy Blend1 (blendweight) too?

      g_IVBFVF := g_IVBFVF or D3DFVF_XYZRHW;
    end;
  else
    _Error; // Separate inline function, to prevent array finalization in normal flow
  end;
end;

function XTL_EmuD3DDevice_SetVertexData4ub
(
    Register_: X_D3DVSDE;
    a: BYTE;
    b: BYTE;
    c: BYTE;
    d: BYTE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Patrickvl  Done:100
var
  fA, fB, fC, fD: FLOAT;
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('EmuD3DDevice_SetVertexData4ub').
      _(Register_, 'Register').
      _(a, 'a').
      _(b, 'b').
      _(c, 'c').
      _(d, 'd').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  fA := a; fB := b; fC := c; fD := d;

//  if (Register_ = X_D3DVSDE_DIFFUSE) or (Register_ = X_D3DVSDE_SPECULAR) then
  begin
    fA := fA / High(a);
    fB := fB / High(b);
    fC := fC / High(c);
    fD := fD / High(d);
  end;

  // TODO -oDxbx : Handle Vertex Attributes that need a Color (in this case, r,g,b=0.0-255.0, a=0.0-1.0)
  Result := DxbxSetVertexData(Register_, fA, fB, fC, fD);
end;

function XTL_EmuD3DDevice_SetVertexData4s
(
    Register_: X_D3DVSDE;
    a: SHORT;
    b: SHORT;
    c: SHORT;
    d: SHORT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Patrickvl  Done:100
var
  fA, fB, fC, fD: FLOAT;
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('EmuD3DDevice_SetVertexData4s').
      _(Register_, 'Register').
      _(a, 'a').
      _(b, 'b').
      _(c, 'c').
      _(d, 'd').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  fA := a; fB := b; fC := c; fD := d;

  if (Register_ = X_D3DVSDE_DIFFUSE) or (Register_ = X_D3DVSDE_SPECULAR) then
  begin
    fA := fA / High(a);
    fB := fB / High(b);
    fC := fC / High(c);
    fD := fD / High(d);
  end;

  // TODO -oDxbx : Handle Vertex Attributes that need a Color
  Result := DxbxSetVertexData(Register_, fA, fB, fC, fD);
end;

function XTL_EmuD3DDevice_SetVertexDataColor
(
    Register_: X_D3DVSDE;
    Color: D3DCOLOR
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  XColor: D3DCOLORVALUE;
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('EmuD3DDevice_SetVertexDataColor').
      _(Register_, 'Register').
      _(Color, 'Color').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  // Convert the D3DColor into its 4 float components :
  XColor := D3DXColorFromDWord(Color);
  // TODO -oDxbx : What if register indicates something else than a X_D3DVSDE_DIFFUSE or X_D3DVSDE_SPECULAR color?

  // Append these to the vertex buffer (g_IVBTable) :
  Result := DxbxSetVertexData(Register_, XColor.r, XColor.g, XColor.b, XColor.a);
end;

function XTL_EmuD3DDevice_End(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_End();');

  if (g_IVBTblOffs > 0) then
    XTL_EmuFlushIVB();

    // MARKED OUT BY CXBX
    // TODO -oCXBX: Should technically clean this up at some point..but on XP doesnt matter much
//    SetLength(g_pIVBVertexBuffer, 0);
//    SetLength(g_IVBTable, 0);

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_RunPushBuffer
(
    pPushBuffer: PX_D3DPushBuffer;
    pFixup: PX_D3DFixup
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_RunPushBuffer').
      _(pPushBuffer, 'pPushBuffer').
      _(pFixup, 'pFixup').
    LogEnd();

  XTL_EmuExecutePushBuffer(pPushBuffer, pFixup);
  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetPushBufferOffset(
  pOffset: PDWORD
  ): HRESULT; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:1
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetPushBufferOffset').
      _(pOffset, 'pOffset').
    LogEnd();

//  D3DDevice_GetPushBufferOffset(pOffset);
  pOffset^ := 0; // TODO

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_Clear
(
    Count: DWORD;
    pRects: PD3DRECT;
    Flags: DWORD;
    Color: D3DCOLOR;
    Z: float;
    Stencil: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  PCFlags: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_Clear').
        _(Count, 'Count').
        _(pRects, 'pRects').
        _(Flags, 'Flags').
        _(Color, 'Color').
        _(Z, 'Z').
        _(Stencil, 'Stencil').
    LogEnd();

  // make adjustments to parameters to make sense with windows d3d
  begin
    // First, convert from Xbox to PC, after which we'll remove the invalid flags :
    PCFlags := EmuXB2PC_D3DCLEAR_FLAGS(Flags);

    // Only clear ZBuffer if we actually have one :
    if not DxbxFix_HasZBuffer then
      PCFlags := PCFlags and (not D3DCLEAR_ZBUFFER);

    // Only clear Stencil buffer if there actually is one :
    // if not g_EmuCDPD.NativePresentationParameters.EnableAutoDepthStencil then
    // TODO -oDxbx: The above check should work (but doesn't!) so for now look at the Xbox PresParam instead :
    if g_EmuCDPD.pPresentationParameters.EnableAutoDepthStencil = BOOL_FALSE then
      PCFlags := PCFlags and (not D3DCLEAR_STENCIL);
  end;

  // Since we filter the flags, make sure there are some left (else, clear isn't necessary) :
  if PCFlags > 0 then
    Result := g_pD3DDevice.Clear(Count, pRects, PCFlags, Color, Z, Stencil)
  else
    Result := D3D_OK;

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_Clear returns 0x%.08X', [Result]);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_Present
(
    {CONST} pSourceRect: PRECT;
    {CONST} pDestRect: PRECT;
    pDummy1: HWND;
    pDummy2: PVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  pPCBackBuffer: XTL_PIDirect3DSurface8;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_Present').
      _(pSourceRect, 'pSourceRect').
      _(pDestRect, 'pDestRect').
      _(pDummy1, 'pDummy1').
      _(pDummy2, 'pDummy2').
    LogEnd();

  // release back buffer lock
  begin
    if g_pD3DDevice.GetBackBuffer({$IFDEF DXBX_USE_D3D9}{iSwapChain=}0,{$ENDIF} 0, D3DBACKBUFFER_TYPE_MONO, @pPCBackBuffer) = D3D_OK then
    begin
      IDirect3DSurface(pPCBackBuffer).UnlockRect();
      IDirect3DSurface(pPCBackBuffer)._Release;
    end;
    // TODO : What about the other backbuffers?
  end;

  hRet := DxbxPresent(pSourceRect, pDestRect, HWND(pDummy1), pDummy2);

  // not really accurate because you definately dont always present on every vblank
  g_VBData.SwapCounter := g_VBData.VBlankCounter;

  if (g_VBData.VBlankCounter = g_VBLastSwap + 1) then
    g_VBData.Flags := D3DVBLANK_SWAPDONE
  else
  begin
    g_VBData.Flags := D3DVBLANK_SWAPMISSED;
    Inc(g_SwapData.MissedVBlanks);
  end;

  g_VBLastSwap := g_VBData.VBlankCounter; // TODO -oDxbx: Should we do this ?

  // Handle Swap Callback function
  begin
    g_SwapData.Swap := X_D3DSWAP_DEFAULT; // TODO -oDxbx : Should we do this ? Cxbx did Inc(g_SwapData.Swap);

    if Assigned(g_pSwapCallback{ is func <> NULL}) then
    begin
      EmuSwapFS(fsXbox);
      g_pSwapCallback(@g_SwapData);
      EmuSwapFS(fsWindows);
    end;
  end;

  g_bHackUpdateSoftwareOverlay := FALSE;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuD3DDevice_Swap
(
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_Swap >>').
      _(Flags, 'Flags').
    LogEnd();

  // TODO -oCXBX: Ensure this flag is always the same across library versions
  if (Flags <> X_D3DSWAP_DEFAULT) then
    EmuWarning('XTL.EmuD3DDevice_Swap: Flags <> X_D3DSWAP_DEFAULT');

  EmuSwapFS(fsXbox);

  // Forward to Present
  Result := XTL_EmuD3DDevice_Present(nil, nil, 0, nil);
end;

procedure XTL_EmuD3DResource_Register
(
    pThis: PX_D3DResource;
    pBase: PVOID
); stdcall;
// Branch:shogun  Revision:162  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  pResource: PX_D3DResource;
  dwCommonType: DWORD;
  pIndexBuffer: PX_D3DIndexBuffer;
  pVertexBuffer: PX_D3DVertexBuffer;
  pPushBuffer: PX_D3DPushBuffer;
  pPixelContainer: PX_D3DPixelContainer;
  pFixup: PX_D3DFixup;
  dwSize: DWORD;
  pData: PBYTE;
  X_Format: X_D3DFORMAT;
  PCFormat: D3DFORMAT;
  CacheFormat: X_D3DFORMAT;
  dwWidth: DWORD;
  dwHeight: DWORD;
  dwBPP: DWORD;
  dwDepth: DWORD;
  dwPitch: DWORD;
  dwMipMapLevels: DWORD;
  bSwizzled: BOOL_;
  bCompressed: BOOL_;
  dwCompressedSize: DWORD;
  bCubemap: BOOL_;

  pPalette: PX_D3DPalette;

  szString: string;
{$IFDEF _DEBUG_DUMP_TEXTURE_REGISTER}
  v: uint32;
  dwDumpSurface: int;
  szBuffer: array [0..255 - 1] of _char;
  dwDumpCube: Integer;
  pSurface: IDirect3DSurface;
  dwDumpTex: Integer;
{$ENDIF}

begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DResource_Register').
      _(pThis, 'pThis').
      _(pBase, 'pBase').
    LogEnd();

//  hRet := D3D_OK;

  // Dxbx note : Before a call to this Register function, an application should have called
  // one of XGSetCubeTextureHeader/XGSetSurfaceHeader/XGSetTextureHeader/XGSetVolumeTextureHeader
  // to initialize the resource header with format, dimensions and stuff. Next it should have
  // called D3D_AllocContiguousMemory (to allocate a correctly aligned buffer memory for the resource).
  // Then (on the Xbox) Register just sets the Data field (and modifies no other resource header fields).
  //
  // PS: For other resources, similar XGSet* functions exist:
  // XGSetFixupHeader, XGSetIndexBufferHeader, XGSetPaletteHeader, XGSetPushBufferHeader and XGSetVertexBufferHeader.

  pResource := pThis;

  dwCommonType := pResource.Common and X_D3DCOMMON_TYPE_MASK;

  // add the offset of the current texture to the base
  pBase := PVOID(DWORD(pBase) + pThis.Data);

  // Determine the resource type, and initialize
  case dwCommonType of

    X_D3DCOMMON_TYPE_VERTEXBUFFER:
    begin
      if MayLog(lfUnit) then
        DbgPrintf('EmuIDirect3DResource_Register: ->VertexBuffer...');

      pVertexBuffer := PX_D3DVertexBuffer(pResource);

      // create vertex buffer
      begin
        dwSize := EmuCheckAllocationSize(pBase, true);

        if (dwSize = DWORD(-1)) {Dxbx addition:}or (dwSize = 0) then
        begin
          // TODO -oCXBX: once this is known to be working, remove the warning
          EmuWarning('Vertex buffer allocation size unknown');
          dwSize := $2000; // temporarily assign a small buffer, which will be increased later
        end;

        hRet := g_pD3DDevice.CreateVertexBuffer
        (
          dwSize, {Usage=}0, {FVF=}0, D3DPOOL_MANAGED,
          {ppVertexBuffer=}@(pVertexBuffer.Emu.VertexBuffer)
          {$IFDEF DXBX_USE_D3D9}, {pSharedHandle=}NULL{$ENDIF}
        );

        if (FAILED(hRet)) then
        begin
          if IsRunning(TITLEID_CrazyTaxi) then
          begin
            // TODO -oCXBX: Hack for Crazy Taxi 3?
            szString := SysUtils.Format('CreateVertexBuffer Failed!'#13#10'   VB Size = 0x%X' +#13#10+ DxbxD3DErrorString(hRet), [dwSize]);

            if ( dwSize <> 0 ) then
              DxbxKrnlCleanup( szString )
            else
              EmuWarning( szString );
          end;

          EmuSwapFS(fsXbox);

//          Result := hRet;
          Exit;
        end;

  {$IFDEF _DEBUG_TRACK_VB}
        g_VBTrackTotal.insert(pVertexBuffer.Emu.VertexBuffer);
  {$ENDIF}

        pData := nil;
        hRet := IDirect3DVertexBuffer(pVertexBuffer.Emu.VertexBuffer).Lock(0, 0, {out}TLockData(pData), 0);
        if FAILED(hRet) then
          DxbxKrnlCleanup('EmuIDirect3DResource_Register: VertexBuffer Lock Failed!' +#13#10+ DxbxD3DErrorString(hRet));

        memcpy(pData, pBase, dwSize);

        pVertexBuffer.Data := ULONG(pData);

        IDirect3DVertexBuffer(pVertexBuffer.Emu.VertexBuffer).Unlock();
      end;

      if MayLog(lfUnit or lfReturnValue) then
        DbgPrintf('EmuIDirect3DResource_Register: Successfully Created VertexBuffer : dwSize=0x%.08x %s',
          [dwSize, ResourceToString(pVertexBuffer)]);
    end;


    X_D3DCOMMON_TYPE_INDEXBUFFER:
    begin
      if MayLog(lfUnit) then
        DbgPrintf('EmuIDirect3DResource_Register: ->IndexBuffer...');

      pIndexBuffer := pX_D3DIndexBuffer(pResource);

      // create index buffer
      begin
        dwSize := EmuCheckAllocationSize(pBase, true);

        if (dwSize = DWORD(-1)) then
        begin
          // TODO -oCXBX: once this is known to be working, remove the warning
          EmuWarning('Index buffer allocation size unknown');

          pIndexBuffer.Emu.Lock := X_D3DRESOURCE_LOCK_FLAG_NOSIZE;

          // Cxbx has 'break'; Delphi can't do that, so we use an else-block
{$IFDEF GAME_HACKS_ENABLED}
          // Halo dwSize = 0x336;
{$ENDIF}
        end
        else
        begin
          DbgPrintf('EmuIDirect3DResource_Register: Number of indexes : %d', [dwSize div SizeOf(Word)]);

          hRet := IDirect3DDevice_CreateIndexBuffer(g_pD3DDevice,
            dwSize, {Usage=}0, D3DFMT_INDEX16, D3DPOOL_MANAGED,
            @(pIndexBuffer.Emu.IndexBuffer));

          if (FAILED(hRet)) then
            DxbxKrnlCleanup('EmuIDirect3DResource_Register: IndexBuffer Create Failed!' +#13#10+ DxbxD3DErrorString(hRet));

          // update data ptr
          pData := nil;
          hRet := IDirect3DIndexBuffer(pIndexBuffer.Emu.IndexBuffer).Lock(0, dwSize, {out}TLockData(pData), 0);
          if (FAILED(hRet)) then
            DxbxKrnlCleanup('EmuIDirect3DResource_Register: IndexBuffer Lock Failed!' +#13#10 + DxbxD3DErrorString(hRet));

          memcpy({dest=}pData, {src=}pBase, dwSize);

          pIndexBuffer.Data := ULONG(pData);

          IDirect3DIndexBuffer(pIndexBuffer.Emu.IndexBuffer).Unlock();

          if MayLog(lfUnit or lfReturnValue) then
            DbgPrintf('EmuIDirect3DResource_Register: Successfully Created IndexBuffer : dwSize=0x%.08x %s',
              [dwSize, ResourceToString(pIndexBuffer)]);
        end;
      end;
    end;


    X_D3DCOMMON_TYPE_PUSHBUFFER:
    begin
      if MayLog(lfUnit) then
        DbgPrintf('EmuIDirect3DResource_Register: ->PushBuffer...');

      pPushBuffer := PX_D3DPushBuffer(pResource);

      // create push buffer
      begin
        dwSize := EmuCheckAllocationSize(pBase, true);

        if dwSize = DWORD(-1) then
        begin
          // TODO -oCXBX: once this is known to be working, remove the warning
          EmuWarning('Push buffer allocation size unknown');

          pPushBuffer.Emu.Lock := X_D3DRESOURCE_LOCK_FLAG_NOSIZE;
        end
        else
          pPushBuffer.Data := ULONG(pBase);
      end;

      if MayLog(lfUnit or lfReturnValue) then
        DbgPrintf('EmuIDirect3DResource_Register: Successfully Created PushBuffer : Size=0x%.08X, AllocationSize=0x%.08X %s',
          [pPushBuffer.Size, pPushBuffer.AllocationSize, ResourceToString(pPushBuffer)]);
    end;


    X_D3DCOMMON_TYPE_SURFACE,
    X_D3DCOMMON_TYPE_TEXTURE:
    begin
      if MayLog(lfUnit) then
        if (dwCommonType = X_D3DCOMMON_TYPE_SURFACE) then
          DbgPrintf('EmuIDirect3DResource_Register: ->Surface...')
        else
          DbgPrintf('EmuIDirect3DResource_Register: ->Texture...');

      pPixelContainer := PX_D3DPixelContainer(pResource);

      X_Format := X_D3DFORMAT((pPixelContainer.Format and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT);

      if (dwCommonType = X_D3DCOMMON_TYPE_SURFACE) then
        PCFormat := DxbxXB2PC_D3DFormat(X_Format, D3DRTYPE_SURFACE, @CacheFormat)
      else
        PCFormat := DxbxXB2PC_D3DFormat(X_Format, D3DRTYPE_TEXTURE, @CacheFormat);

      DxbxGetFormatRelatedVariables(pPixelContainer, X_Format,
        {var}dwWidth, {var}dwHeight, {var}dwBPP, {var}dwDepth, {var}dwPitch, {var}dwMipMapLevels,
        {var}bSwizzled, {var}bCompressed, {var}dwCompressedSize, {var}bCubeMap);

      if (X_Format = X_D3DFMT_YUY2) then
      begin
        // cache the overlay size
        g_dwOverlayW := dwWidth;
        g_dwOverlayH := dwHeight;
        g_dwOverlayP := RoundUp(g_dwOverlayW, 64) * 2; // Pitch should be in 64 byte increments; 2 bytes per pixel

        // create texture resource
        DxbxInitializePixelContainerYUY2(pPixelContainer);
      end
      else // X_Format <> X_D3DFMT_YUY2
      begin
        // create the happy little texture
        if (dwCommonType = X_D3DCOMMON_TYPE_SURFACE) then
        begin
          hRet := IDirect3DDevice_CreateImageSurface(g_pD3DDevice,
            dwWidth,
            dwHeight,
            PCFormat,
            @(pPixelContainer.Emu.Surface)
          );

          if (FAILED(hRet)) then
            DxbxKrnlCleanup('EmuIDirect3DResource_Register: CreateImageSurface Failed!' +#13#10+ DxbxD3DErrorString(hRet));

          if MayLog(lfUnit or lfReturnValue) then
          begin
            DbgPrintf('EmuIDirect3DResource_Register: Successfully Created ImageSurface : ' + ResourceToString(pPixelContainer));
            DbgPrintf('EmuIDirect3DResource_Register: Width:%d, Height:%d, Format:%d', [dwWidth, dwHeight, Ord(PCFormat)]);
          end;
        end
        else // dwCommonType = X_D3DCOMMON_TYPE_TEXTURE
        begin
          // TODO -oCXBX: HACK: Figure out why this is necessary!
          // TODO -oCXBX: This is necessary for DXT1 textures at least (4x4 blocks minimum)
          if (dwWidth < 4) then
          begin
            EmuWarning('Expanding texture width(%d.4)', [dwWidth]);
            dwWidth := 4;

            dwMipMapLevels := 3;
          end;

          if (dwHeight < 4) then
          begin
            EmuWarning('Expanding texture height(%d.4)', [dwHeight]);
            dwHeight := 4;

            dwMipMapLevels := 3;
          end;

          if (bCubemap) then
          begin
            if MayLog(lfUnit) then
              DbgPrintf('CreateCubeTexture(%d, %d, 0, %d, D3DPOOL_MANAGED, 0x%.08X)',
                [dwWidth, dwMipMapLevels, Ord(PCFormat), pPixelContainer.Emu.Texture]);

            hRet := IDirect3DDevice_CreateCubeTexture(g_pD3DDevice,
              dwWidth, dwMipMapLevels, {Usage=}0, PCFormat,
              D3DPOOL_MANAGED, @(pPixelContainer.Emu.CubeTexture));
            // dwHeight := dwWidth;
            // dwBPP? dwDepth?? dwPitch?

            if (FAILED(hRet)) then
              DxbxKrnlCleanup('EmuIDirect3DResource_Register: CreateCubeTexture Failed!' +#13#10+ DxbxD3DErrorString(hRet));

            if MayLog(lfUnit or lfReturnValue) then
              DbgPrintf('EmuIDirect3DResource_Register: Successfully Created CubeTexture : ' + ResourceToString(pPixelContainer));
          end
          else
          begin
            if MayLog(lfUnit) then
              DbgPrintf('CreateTexture(%d,%d,%d, 0,%d, D3DPOOL_MANAGED, 0x%.08X)',
                [dwWidth, dwHeight, dwMipMapLevels, Ord(PCFormat), @(pPixelContainer.Emu.Texture)]);

//            if CacheFormat = X_D3DFMT_P8 then
//              PCFormat := D3DFMT_A8R8G8B8;

            hRet := IDirect3DDevice_CreateTexture(g_pD3DDevice,
              dwWidth, dwHeight, dwMipMapLevels, {Usage=}0, PCFormat,
              D3DPOOL_MANAGED, @(pPixelContainer.Emu.Texture)
              );

            if (FAILED(hRet)) then
              DxbxKrnlCleanup('EmuIDirect3DResource_Register: CreateTexture Failed!' +#13#10+ DxbxD3DErrorString(hRet));

            if MayLog(lfUnit or lfReturnValue) then
              DbgPrintf('EmuIDirect3DResource_Register: Successfully Created Texture : ' + ResourceToString(pPixelContainer));
          end;
        end;

        pThis.Data := DWORD(pBase);

        DxbxUpdatePixelContainer(pPixelContainer, dwCommonType,
          dwWidth, dwHeight, dwBPP, dwDepth, dwPitch, dwMipMapLevels,
          bSwizzled, bCompressed, dwCompressedSize, bCubemap, CacheFormat);

        // Debug Texture Dumping
{$IFDEF _DEBUG_DUMP_TEXTURE_REGISTER}
        if (dwCommonType = X_D3DCOMMON_TYPE_SURFACE) then
        begin
          dwDumpSurface := 0;

          sprintf(@szBuffer[0], '_DEBUG_DUMP_TEXTURE_REGISTER %.03d - RegSurface%.03d.bmp', [X_Format, dwDumpSurface]); Inc(dwDumpSurface);
          D3DXSaveSurfaceToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, IDirect3DSurface(pPixelContainer.Emu.Surface), 0, 0);
        end
        else
        if (bCubemap) then
        begin
          dwDumpCube := 0;
          v := 0;
          while v < 6 do
          begin
            pSurface := nil;

            sprintf(szBuffer, '_DEBUG_DUMP_TEXTURE_REGISTER %.03d - RegCubeTex%.03d -%d.bmp', [X_Format, dwDumpCube, v]); Inc(dwDumpCube);

            IDirect3DCubeTexture(pPixelContainer.Emu.CubeTexture).GetCubeMapSurface(D3DCUBEMAP_FACES(v), 0, @pSurface);

            D3DXSaveSurfaceToFileA(szBuffer, D3DXIFF_BMP, pSurface, 0, 0);
            Inc(v);
          end;
        end
        else
        begin
          dwDumpTex := 0;
          sprintf(szBuffer,' _DEBUG_DUMP_TEXTURE_REGISTER %.03d - RegTexture%.03d.bmp', [X_Format, dwDumpTex]); Inc(dwDumpTex);
          D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, IDirect3DTexture(pPixelContainer.Emu.Texture), 0);
        end;
{$endif}
      end;
    end;


    X_D3DCOMMON_TYPE_PALETTE:
    begin
      if MayLog(lfUnit) then
        DbgPrintf('EmuIDirect3DResource_Register: ->Palette...');

      pPalette := PX_D3DPalette(pResource);

      // create palette
      begin
        dwSize := EmuCheckAllocationSize(pBase, true);

        if (dwSize = DWORD(-1)) then
        begin
          // TODO -oCXBX: once this is known to be working, remove the warning
          EmuWarning('Palette allocation size unknown');

          pPalette.Emu.Lock := X_D3DRESOURCE_LOCK_FLAG_NOSIZE;
        end;

        g_pCurrentPalette := pBase;
        g_dwCurrentPaletteSize := dwSize;

        pPalette.Data := ULONG(pBase);
      end;

      if MayLog(lfUnit or lfReturnValue) then
        DbgPrintf('EmuIDirect3DResource_Register: Successfully Created Palette : dwSize=0x%.08X %s',
         [dwSize, ResourceToString(pPalette)]);
    end;


    X_D3DCOMMON_TYPE_FIXUP:
    begin
      pFixup := PX_D3DFixup(pResource);

      DxbxKrnlCleanup('EmuIDirect3DResource_Register: X_D3DCOMMON_TYPE_FIXUP is not yet supported' +
                #13#10'0x%.08X (pFixup->Common)' +
                #13#10'0x%.08X (pFixup->Data)' +
                #13#10'0x%.08X (pFixup->Lock)' +
                #13#10'0x%.08X (pFixup->Run)' +
                #13#10'0x%.08X (pFixup->Next)' +
                #13#10'0x%.08X (pFixup->Size)',
                [pFixup.Common, pFixup.Data, pFixup.Emu.Lock, pFixup.Run, pFixup.Next, pFixup.Size]);
    end;

  else // case
    DxbxKrnlCleanup('IDirect3DResource.Register.Common Type 0x%.08X not yet supported', [dwCommonType]);
  end;

  EmuSwapFS(fsXbox);

//  Result := hRet;
end;

function XTL_EmuD3DResource_AddRef
(
  pThis: PX_D3DResource
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  uRet: ULONG;
  pRefCount: PDWORD;
  pPCResource: XTL_PIDirect3DResource8;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DResource_AddRef').
      _(pThis, 'pThis').
    LogEnd();

  uRet := 0;

  if (IsSpecialResource(pThis.Data)) and (pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF > 0) then
  begin
    pRefCount := PDWORD(pThis.Emu.Lock);

    Inc(pRefCount^);
    uRet := pRefCount^; // Dxbx addition : Return reference-count of special resources too!
  end
  else
  begin
    pPCResource := pThis.Emu.Resource;

    if(pThis.Emu.Lock = $8000BEEF) then
    begin
      // Inc(pThis.Emu.Lock); // TODO : This gives $8000BEF0, which is otherwise unhandled! What to do?
      uRet := pThis.Emu.Lock;
    end
    else if (pPCResource <> nil) then
      uRet := IDirect3DResource(pPCResource)._AddRef();

    // Update RefCount as present in the Common field :
    pThis.Common := (pThis.Common and (not X_D3DCOMMON_REFCOUNT_MASK))
                 or (uRet and X_D3DCOMMON_REFCOUNT_MASK);
  end;

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function XTL_EmuD3DResource_Release(
  pThis: PX_D3DResource
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  uRet: ULONG;
  dwPtr: DWORD;
  pRefCount: PDWORD;
  pPCResource: XTL_PIDirect3DResource8;
  v: int;
{$ifdef _DEBUG_TRACE_VB}
  Type_: D3DRESOURCETYPE;
{$endif}
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DResource_Release').
      _(pThis, 'pThis').
    LogEnd();

  uRet := 0;

  // HACK: In case the clone technique fails...
  if not Assigned(pThis) then
  begin
    EmuWarning('NULL texture!');

    EmuSwapFS(fsXbox);

    Result := 0;
    Exit;
  end;

  if(IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    dwPtr := DWORD(pThis.Emu.Lock);
    pRefCount := PDWORD(dwPtr);

    Dec(pRefCount^);
    uRet := pRefCount^; // Dxbx addition : Return reference-count of special resources too!
    if uRet = 0 then
    begin
      if (g_YuvSurface = pThis) then
        g_YuvSurface := NULL;

      // free memory associated with this special resource handle
      XboxFree(PVOID(dwPtr));
    end;

    // TODO -oDxbx : Is this the right moment? Shouldn't we do this only when RefCount=0?
    EmuSwapFS(fsXbox);
    XTL_EmuD3DDevice_EnableOverlay(BOOL_FALSE);
    EmuSwapFS(fsWindows);
  end
  else
  begin
    pPCResource := pThis.Emu.Resource;

    if (pThis.Emu.Lock = $8000BEEF) then
    begin
      XboxFree(PVOID(pThis.Data));
      PVOID(pThis.Data) := nil; // Make sure the Data pointer can't be accessed anymore
      // Dec(pThis.Emu.Lock); // TODO : This gives $8000BEEE, which is otherwise unhandled! What to do?
      uRet := pThis.Emu.Lock;
    end
    else
    begin
      if (pPCResource <> nil) then
      begin
        if (pThis.Data <> 0) then // Dxbx addition : This won't change here, so check outside the loop
          for v := 0 to RESOURCE_CACHE_SIZE-1 do
          begin
            if (g_EmuD3DResourceCache[v].Data = pThis.Data) then
            begin
              g_EmuD3DResourceCache[v].Data := 0;
              g_EmuD3DResourceCache[v].Emu.Resource := nil; // Dxbx addition
              break;
            end;
          end;

        // Dxbx addition : Because most Xbox resources don't need an
        // explicit Unlock, we need to do that at release-time ourselves here :
        IDirect3DResource(pPCResource)._AddRef();
        uRet := IDirect3DResource(pPCResource)._Release();
        if (uRet = 1) then
          DxbxUnlockD3DResource(pThis);

{$ifdef _DEBUG_TRACE_VB}
        Type_ := IDirect3DResource(pPCResource).GetType();
{$endif}

        // Now do the actual release :
        uRet := IDirect3DResource(pPCResource)._Release();
        if (uRet = 0) then
        begin
          if MayLog(lfUnit) then
            DbgPrintf('EmuIDirect3DResource_Release: Cleaned up a Resource!');

{$ifdef _DEBUG_TRACE_VB}
          if (Type_ = D3DRTYPE_VERTEXBUFFER) then
          begin
            g_VBTrackTotal.remove(pPCResource);
            g_VBTrackDisable.remove(pPCResource);
          end;
{$endif}
          pThis.Emu.Resource := nil; // Dxbx addition - nil out after decreasing reference count

          //delete pThis;
        end;

        pPCResource := nil; // Dxbx addition - nil out after decreasing reference count
      end;
    end;

  end;

  if uRet > 0 then
    // Update RefCount as present in the Common field :
    pThis.Common := (pThis.Common and (not X_D3DCOMMON_REFCOUNT_MASK))
                 or (uRet and X_D3DCOMMON_REFCOUNT_MASK);

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function XTL_EmuD3DResource_IsBusy
(
    pThis: PX_D3DResource
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
//var
//  pPCResource: XTL_PIDirect3DResource8;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DResource_IsBusy').
      _(pThis, 'pThis').
    LogEnd();

  // pPCResource := pThis.Emu.Resource;
  EmuSwapFS(fsXbox);

  // TODO -oDxbx : This is just an experiment, as Cxbx returns False, while shadow_tj got
  // 'Dead or alive volleyball' to work when returning True here; Let's see what this flag does :
  Result := g_bIsBusy;
end;

procedure XTL_EmuD3DResource_GetDevice
(
  pThis: PX_D3DResource;
  ppDevice: XTL_PPIDirect3DDevice8 // PPD3DDevice ?
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('IDirect3DResource_GetDevice').
      _(pThis, 'pThis').
      _(ppDevice, 'ppDevice').
    LogEnd();

  ppDevice^ := Pointer(g_pD3DDevice); // TODO -oDxbx : Is this correct?

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DResource_GetType
(
    pThis: PX_D3DResource
): X_D3DRESOURCETYPE; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DResource_GetType').
      _(pThis, 'pThis').
    LogEnd();

  // TODO -oCXBX: Handle situation where the resource type is >7
  Result := X_D3DRESOURCETYPE(IDirect3DResource(pThis.Emu.Resource).GetType());

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_PixelJar_Lock2DSurface
(
    pPixelContainer: PX_D3DPixelContainer;
    FaceType: D3DCUBEMAP_FACES;
    Level: UINT;
    pLockedRect: PD3DLOCKED_RECT;
    pRect: PRECT;
    Flags: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuLock2DSurface').
      _(pPixelContainer, 'pPixelContainer').
      _(Ord(FaceType), 'FaceType').
      _(Level, 'Level').
      _(pLockedRect, 'pLockedRect').
      _(pRect, 'pRect').
      _(Flags, 'Flags').
    LogEnd();

  EmuVerifyResourceIsRegistered(pPixelContainer);

  // Dxbx addition : Remove old lock(s) :
  IDirect3DCubeTexture(pPixelContainer.Emu.CubeTexture).UnlockRect(FaceType, Level);

  {ignore hRet:=}IDirect3DCubeTexture(pPixelContainer.Emu.CubeTexture).LockRect(FaceType, Level, {out}pLockedRect^, pRect, Flags);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_PixelJar_Get2DSurfaceDesc
(
    pPixelContainer: PX_D3DPixelContainer;
    dwLevel: DWORD;
    pDesc: PX_D3DSURFACE_DESC
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  SurfaceDesc: D3DSURFACE_DESC;
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuGet2DSurfaceDesc').
      _(pPixelContainer, 'pPixelContainer').
      _(dwLevel, 'dwLevel').
      _(pDesc, 'pDesc').
    LogEnd();

  EmuVerifyResourceIsRegistered(pPixelContainer);

  ZeroMemory(@SurfaceDesc, sizeof(SurfaceDesc));

  if (dwLevel = $FEFEFEFE) then
  begin
    hRet := IDirect3DSurface(pPixelContainer.Emu.Surface).GetDesc({out}SurfaceDesc);

    (* marked by cxbx
     Integer dwDumpSurface := 0;

     szBuffer: array [0..255-1] of _char;

     sprintf(szBuffer, DxbxDebugFolder +'\Textures\Surface%.03d.bmp', [dwDumpTexture]); Inc(dwDumpTexture);

     D3DXSaveSurfaceToFileA(szBuffer, D3DXIFF_BMP, pPixelContainer.Emu.Surface, 0, 0);
    *)
  end
  else
  begin
{$IFDEF GAME_HACKS_ENABLED}
    // TODO -oCXBX: Work on Namco Museum hack later...
    // if pPixelContainer.Emu.Texture = (IDirect3DTexture($078A0044)) then
{$ENDIF}

    hRet := IDirect3DTexture(pPixelContainer.Emu.Texture).GetLevelDesc(dwLevel, {out}SurfaceDesc);
    //hRet = IDirect3DSurface(pPixelContainer.Emu.Surface).GetDesc({out}SurfaceDesc);
    if FAILED(hRet) then
      EmuWarning('IDirect3DTexture::GetSurfaceDesc failed!' +#13#10+ DxbxD3DErrorString(hRet));

    if MayLog(lfUnit) then
      DbgPrintf('Okay');

    (* marked by cxbx
    int dwDumpTexture := 0;

    szBuffer: array [0..255-1] of _char;

    sprintf(szBuffer, DxbxDebugFolder +'\Textures\GetDescTexture%.03d.bmp', [dwDumpTexture]); Inc(dwDumpTexture);

    D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, pPixelContainer.Emu.Texture, 0);
    *)
  end;

  if SUCCEEDED(hRet) then
    // rearrange into xbox format (remove D3DPOOL)
    EmuPC2XB_D3DSURFACE_DESC(SurfaceDesc, pDesc, 'EmuGet2DSurfaceDesc');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuGet2DSurfaceDescD
(
  pPixelContainer: PX_D3DPixelContainer;
  pDesc: PX_D3DSURFACE_DESC
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('EmuGet2DSurfaceDescD').
      _(pPixelContainer, 'pPixelContainer').
      _(pDesc, 'pDesc').
    LogEnd();

    EmuSwapFS(fsXbox);
  end;

  XTL_EmuD3D_PixelJar_Get2DSurfaceDesc(pPixelContainer, $FEFEFEFE, pDesc);
end;

function XTL_EmuD3DSurface_GetDesc
(
  pThis: PX_D3DResource;
  pDesc: PX_D3DSURFACE_DESC
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pPCSurface: XTL_PIDirect3DSurface8;
  SurfaceDesc: D3DSURFACE_DESC;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DSurface_GetDesc').
      _(pThis, 'pThis').
      _(pDesc, 'pDesc').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);

  if IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0) then
  begin
    pDesc.Format := X_D3DFMT_YUY2; // = EmuPC2XB_D3DFormat(D3DFMT_YUY2);
    pDesc.Type_ := X_D3DRTYPE_SURFACE;
    pDesc.Usage := D3DUSAGE_RENDERTARGET; // Dxbx : Guesswork, but probably better than 0
    pDesc.Size := g_dwOverlayP * g_dwOverlayH;
    pDesc.MultiSampleType := X_D3DMULTISAMPLE_TYPE(0);
    pDesc.Height := g_dwOverlayH;
    pDesc.Width := g_dwOverlayW;

    Result := D3D_OK;
  end
  else
  begin
    pPCSurface := pThis.Emu.Surface;

    Result := IDirect3DSurface(pPCSurface).GetDesc({out}SurfaceDesc);

    if SUCCEEDED(Result) then
      // rearrange into xbox format (remove D3DPOOL)
      EmuPC2XB_D3DSURFACE_DESC(SurfaceDesc, pDesc, 'EmuIDirect3DSurface_GetDesc');
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DSurface_LockRect
(
  pThis: PX_D3DResource;
  pLockedRect: PD3DLOCKED_RECT;
  pRect: PRECT;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  pPCSurface: XTL_PIDirect3DSurface8;
  NewFlags: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DSurface_LockRect').
      _(pThis, 'pThis').
      _(pLockedRect, 'pLockedRect').
      _(pRect, 'pRect').
      _(Flags, 'Flags').
    LogEnd();

  hRet := 0; // Dxbx : Prevent 'not initialized' compiler warning

  EmuVerifyResourceIsRegistered(pThis);

  // check if we have an unregistered YUV2 resource
  if ((pThis <> nil) and IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    pLockedRect.Pitch := g_dwOverlayP;
    // Dxbx addition : Skip RefCount (since it's at the start of the buffer) :
    pLockedRect.pBits := PVOID(pThis.Emu.Lock + SizeOf(DWORD));

    hRet := D3D_OK;
  end
  else
  begin
    pPCSurface := pThis.Emu.Surface;

    NewFlags := EmuXB2PC_D3DLock(Flags);

    try
      // Remove old lock(s)
      IDirect3DSurface(pPCSurface).UnlockRect();

      hRet := IDirect3DSurface(pPCSurface).LockRect({out}pLockedRect^, pRect, NewFlags);

      if (FAILED(hRet)) then
        EmuWarning('LockRect Failed!' +#13#10+ DxbxD3DErrorString(hRet));
    except
      EmuWarning('Invalid Surface!');
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuD3DBaseTexture_GetLevelCount
(
  pThis: PX_D3DBaseTexture
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DBaseTexture_GetLevelCount').
      _(pThis, 'pThis').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);
  Result := IDirect3DBaseTexture(pThis.Emu.BaseTexture).GetLevelCount();

  if (FAILED(Result)) then
    EmuWarning('GetLevelCount Failed!' +#13#10+ DxbxD3DErrorString(Result));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DTexture_GetSurfaceLevel2
(
  pThis: PX_D3DTexture;
  Level: UINT
): PX_D3DResource; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
//var
//  pRefCount: PDWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DTexture_GetSurfaceLevel2 >>').
      _(pThis, 'pThis').
      _(Level, 'Level').
    LogEnd();

(* Dxbx note : This block is alredy present in XTL_EmuD3DTexture_GetSurfaceLevel too, so disable here :
  EmuVerifyResourceIsRegistered(pThis); // Dxbx addition

  // In a special situation, we are actually returning a memory ptr with high bit set
  if (IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    pRefCount := PDWORD(pThis.Emu.Lock);

    // increase ref count
    Inc(pRefCount^);

    Result := pThis;

    EmuSwapFS(fsXbox);
  end
  else
*)
  begin
    EmuSwapFS(fsXbox);

    XTL_EmuD3DTexture_GetSurfaceLevel(pThis, Level, PPX_D3DSurface(@Result));
  end;
end;


function XTL_EmuD3DTexture_LockRect
(
    pThis: PX_D3DTexture;
    Level: UINT;
    pLockedRect: PD3DLOCKED_RECT;
    pRect: PRECT;
    Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  pPCTexture: XTL_PIDirect3DTexture8;
  NewFlags: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DTexture_LockRect').
      _(pThis, 'pThis').
      _(Level, 'Level').
      _(pLockedRect, 'pLockedRect').
      _(pRect, 'pRect').
      _(Flags, 'Flags').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);

  // check if we have an unregistered YUV2 resource
  if ((pThis <> nil) and IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    pLockedRect.Pitch := g_dwOverlayP;
    // Dxbx addition : Skip RefCount (since it's at the start of the buffer) :
    pLockedRect.pBits := PVOID(pThis.Emu.Lock + SizeOf(DWORD));

    hRet := D3D_OK;
  end
  else
  begin
    pPCTexture := pThis.Emu.Texture;

    NewFlags := EmuXB2PC_D3DLock(Flags);

    if IsRunning(TITLEID_UnrealChampionship)
    and ((Level = 6) or (Level = 7) or (Level = 8) or (Level = 9)) then
    begin
      // HACK: Unreal Championship crashes when the texture level reaches 9...
      EmuWarning('Unreal Championship texture hack applied!');
      hRet := D3DERR_INVALIDCALL;
    end
    else
    begin
      // Remove old lock(s)
      IDirect3DTexture(pPCTexture).UnlockRect(Level);

      hRet := IDirect3DTexture(pPCTexture).LockRect(Level, {out}pLockedRect^, pRect, NewFlags);
      if (FAILED(hRet)) then
        EmuWarning('LockRect Failed!' +#13#10+ DxbxD3DErrorString(hRet));

      // Dxbx addition : Initialize Common field properly :
      pThis.Common := pThis.Common or X_D3DCOMMON_ISLOCKED or $10; // Dxbx : Set a RefCount too (was in X_D3DCOMMON_ISLOCKED before!)
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuD3DTexture_GetSurfaceLevel
(
  pThis: PX_D3DTexture;
  Level: UINT;
  ppSurfaceLevel: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  pRefCount: PDWORD;
  pPCTexture: XTL_PIDirect3DTexture8;
  pSurfaceLevel: PX_D3DSurface;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DTexture_GetSurfaceLevel').
      _(pThis, 'pThis').
      _(Level, 'Level').
      _(ppSurfaceLevel, 'ppSurfaceLevel').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);

  // if highest bit is set, this is actually a raw memory pointer (for YUY2 simulation)
  if (IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    pRefCount := PDWORD(pThis.Emu.Lock);

    // increase ref count
    Inc(pRefCount^);

    ppSurfaceLevel^ := PX_D3DSurface(pThis);

    hRet := D3D_OK;
  end
  else
  begin
    pPCTexture := pThis.Emu.Texture;

    New({var}pSurfaceLevel); // Cxbx : new X_D3DSurface();
    ZeroMemory(pSurfaceLevel, SizeOf(pSurfaceLevel^));
    // TODO -oDxbx : When should this be freeed? Isn't this a memory leak otherwise?

//    ppSurfaceLevel^.Data := $B00BBABE;
    hRet := IDirect3DTexture(pPCTexture).GetSurfaceLevel(Level, @(pSurfaceLevel.Emu.Surface));
    DxbxUpdateResourceFields(pSurfaceLevel);

    if (FAILED(hRet)) then
    begin
      Dispose({var}pSurfaceLevel);
      EmuWarning('GetSurfaceLevel Failed!' +#13#10+ DxbxD3DErrorString(hRet));
    end
    else
    begin
      // Dxbx addition : Initialize Common field properly :
      pSurfaceLevel.Common := ((IDirect3DSurface(pSurfaceLevel.Emu.Surface)._AddRef()-1) and X_D3DCOMMON_REFCOUNT_MASK) or X_D3DCOMMON_TYPE_SURFACE;
      if IDirect3DSurface(pSurfaceLevel.Emu.Surface)._Release() = 0 then
        pSurfaceLevel.Emu.Surface := nil;
//    DxbxUpdateResourceFields(pSurfaceLevel);

      ppSurfaceLevel^ := pSurfaceLevel;

      if MayLog(lfUnit or lfReturnValue) then
        DbgPrintf('EmuD3D8 : SurfaceLevel : ' + ResourceToString(pSurfaceLevel));
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuD3DVolumeTexture_LockBox
(
  pThis: PX_D3DVolumeTexture;
  Level: UINT;
  pLockedVolume: PD3DLOCKED_BOX;
  pBox: PD3DBOX;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DVolumeTexture_LockBox').
      _(pThis, 'pThis').
      _(Level, 'Level').
      _(pLockedVolume, 'pLockedVolume').
      _(pBox, 'pBox').
      _(Flags, 'Flags').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);
  Result := IDirect3DVolumeTexture(pThis.Emu.VolumeTexture).LockBox(Level, {out}pLockedVolume^, pBox, Flags);

  if (FAILED(Result)) then
    EmuWarning('LockBox Failed!' +#13#10+ DxbxD3DErrorString(Result));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DCubeTexture_LockRect
(
  pThis: PX_D3DCubeTexture;
  FaceType: D3DCUBEMAP_FACES;
  Level: UINT;
  pLockedBox: PD3DLOCKED_RECT;
  pRect: PRECT;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DCubeTexture_LockRect').
      _(pThis, 'pThis').
      _(Ord(FaceType), 'FaceType').
      _(Level, 'Level').
      _(pLockedBox, 'pLockedBox').
      _(pRect, 'pRect').
      _(Flags, 'Flags').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);

  // Dxbx addition : Remove old lock(s) :
  IDirect3DCubeTexture(pThis.Emu.CubeTexture).UnlockRect(FaceType, Level);
  Result := IDirect3DCubeTexture(pThis.Emu.CubeTexture).LockRect(FaceType, Level, {out}pLockedBox^, pRect, Flags);
  if (FAILED(Result)) then
    EmuWarning('LockRect Failed!' +#13#10+ DxbxD3DErrorString(Result));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_Release(): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_Release();');

  // Trick to get the current reference-count :
  g_pD3DDevice._AddRef();
  Result := g_pD3DDevice._Release();

  // Is this the last reference to the D3DDevice?
  if (Result = 1) then
  begin
    // Dxbx addition :
    // Wait until proxy is done with an existing call (i highly doubt this situation will come up)
    while (g_EmuCDPD.bReady) do
      Sleep(10); // Dxbx: Should we use SwitchToThread() or YieldProcessor() ?

    // Signal proxy thread, and wait for completion
    g_EmuCDPD.bCreate := false; // Dxbx: bCreate should be set before bReady!
    g_EmuCDPD.bReady := true;

  // Wait until proxy is completed
    while g_EmuCDPD.bReady do
      Sleep(10); // Dxbx: Should we use SwitchToThread() or YieldProcessor() ?

    Result := g_EmuCDPD.hRet;
  end
  else
    Result := g_pD3DDevice._Release();

  // Dxbx addition - TODO : is this better ?
  if Result = 0 then
    Pointer(g_pD3DDevice) := nil; // Cast to prevent automatic refcounting from clearing the object (we did that above)

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CreateVertexBuffer
(
  Length: UINT;
  Usage: DWORD;
  FVF: DWORD;
  Pool: X_D3DPOOL;
  ppVertexBuffer: PPX_D3DVertexBuffer
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pD3DVertexBuffer: PX_D3DVertexBuffer;
  NewLength: UINT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CreateVertexBuffer').
      _(Length, 'Length').
      _(Usage, 'Usage').
      _(FVF, 'FVF').
      _(Pool, 'Pool').
      _(ppVertexBuffer, 'ppVertexBuffer').
    LogEnd();

  NewLength := Length;

  // DXBX Addition, vertexbuffer length need to be at least a vertexbuffer length large.
  // At least 1 vertexbuffer needs to fit in.
  if (NewLength = DWORD(-1)) or (NewLength = 0) then
  begin
    // TODO -oCXBX: once this is known to be working, remove the warning
    EmuWarning('Vertex buffer allocation size unknown');
    NewLength := $2000; // temporarily assign a small buffer, which will be increased later
  end;

  New({PX_D3DVertexBuffer}pD3DVertexBuffer);
  ZeroMemory(pD3DVertexBuffer, SizeOf(pD3DVertexBuffer^));

  Result := g_pD3DDevice.CreateVertexBuffer(
    NewLength,
    {Usage=}0, // ignored according to Xbox docs
    {FVF=}0, // ignored according to Xbox docs
    D3DPOOL_MANAGED, // TODO : If we supply Pool here, the PointSprites and Gamepad XDK samples crash!
    {ppVertexBuffer=}@(pD3DVertexBuffer.Emu.VertexBuffer)
    {$IFDEF DXBX_USE_D3D9}, {pSharedHandle=}NULL{$ENDIF}
  );

  if (FAILED(Result)) then
  begin
    EmuWarning('CreateVertexBuffer Failed!' +#13#10+ DxbxD3DErrorString(Result));
    Dispose(pD3DVertexBuffer);
  end
  else
  begin
    // Dxbx addition : Initialize Common field properly :
    pD3DVertexBuffer.Common := ((IDirect3DVertexBuffer(pD3DVertexBuffer.Emu.VertexBuffer)._AddRef()-1) and X_D3DCOMMON_REFCOUNT_MASK) or X_D3DCOMMON_TYPE_VERTEXBUFFER or X_D3DCOMMON_D3DCREATED;
    if IDirect3DVertexBuffer(pD3DVertexBuffer.Emu.VertexBuffer)._Release() = 0 then
      pD3DVertexBuffer.Emu.VertexBuffer := nil;

{$ifdef _DEBUG_TRACK_VB}
    g_VBTrackTotal.insert(pD3DVertexBuffer.Emu.VertexBuffer);
{$endif}
    ppVertexBuffer^ := pD3DVertexBuffer;

    if MayLog(lfUnit or lfReturnValue) then
      DbgPrintf('EmuD3D8 : CreateVertexBuffer: Successfully Created VertexBuffer : ' + ResourceToString(ppVertexBuffer^));
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CreateVertexBuffer2
(
  Length: UINT
): PX_D3DVertexBuffer; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('EmuD3DDevice_CreateVertexBuffer2 >>').
      _(Length, 'Length').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  Result := NULL;

  XTL_EmuD3DDevice_CreateVertexBuffer(Length, {Usage=}0, {FVF=}0, D3DPOOL_MANAGED, @Result);
end;

function XTL_EmuD3DDevice_EnableOverlay
(
  Enable: BOOL
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  ddsd2: DDSURFACEDESC2;
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_EnableOverlay').
      _(Enable, 'Enable').
    LogEnd();

  if (Enable = BOOL_FALSE) and (g_pDDSOverlay7 <> NULL) then
  begin
    if (g_bYUY2OverlaysSupported) then
      IDirectDrawSurface7(g_pDDSOverlay7).UpdateOverlay(NULL, IDirectDrawSurface7(g_pDDSPrimary7), NULL, DDOVER_HIDE, nil);

    // cleanup overlay clipper
    if (g_pDDClipper7 <> nil) then
    begin
      if IDirectDrawClipper(g_pDDClipper7)._Release() = 0 then
        g_pDDClipper7 := nil;
    end;

    // cleanup overlay surface
    if Assigned(g_pDDSOverlay7) then
    begin
      if IDirectDrawSurface7(g_pDDSOverlay7)._Release() = 0 then
        g_pDDSOverlay7 := nil;
    end;
  end
  else
  begin
    if (Enable <> BOOL_FALSE) and (g_pDDSOverlay7 = nil) then
    begin
      // initialize overlay surface
      if (g_bYUY2OverlaysSupported) then
      begin
        ZeroMemory(@ddsd2, sizeof(ddsd2));

        ddsd2.dwSize := sizeof(ddsd2);
        ddsd2.dwFlags := DDSD_CAPS or DDSD_WIDTH or DDSD_HEIGHT or DDSD_PIXELFORMAT;
        ddsd2.ddsCaps.dwCaps := DDSCAPS_OVERLAY;
        ddsd2.dwWidth := g_dwOverlayW;
        ddsd2.dwHeight := g_dwOverlayH;

        ddsd2.ddpfPixelFormat.dwSize := sizeof(DDPIXELFORMAT);
        ddsd2.ddpfPixelFormat.dwFlags := DDPF_FOURCC;
        ddsd2.ddpfPixelFormat.dwFourCC := MAKEFOURCC('Y', 'U', 'Y', '2');

        hRet := IDirectDraw7(g_pDD7).CreateSurface(ddsd2, @g_pDDSOverlay7, NULL);

        if (FAILED(hRet)) then
          DxbxKrnlCleanup('EmuD3DDevice_EnableOverlay : Could not create overlay surface' +#13#10+ DxbxD3DErrorString(hRet));

        hRet := IDirectDraw7(g_pDD7).CreateClipper(0, {out}IDirectDrawClipper(g_pDDClipper7), NULL);

        if (FAILED(hRet)) then
          DxbxKrnlCleanup('EmuD3DDevice_EnableOverlay : Could not create overlay clipper' +#13#10+ DxbxD3DErrorString(hRet));

        {Dxbx unused hRet :=} IDirectDrawClipper(g_pDDClipper7).SetHWnd(0, g_hEmuWindow);
      end;
    end;
  end;

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function ClampIntToByte(const aValue: Integer): Byte;
begin
  if aValue < 0 then
    Result := 0
  else
    if aValue > 255 then
      Result := 255
    else
      Result := aValue;
end;

function XTL_EmuD3DDevice_UpdateOverlay
(
  pSurface: PX_D3DSurface;
  SrcRect: PRECT;
  DstRect: PRECT;
  EnableColorKey: BOOL;
  ColorKey: D3DCOLOR
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
const
  FIXED_POINT_FACTOR = (1 shl 16);
var
  ddsd2: DDSURFACEDESC2;
  pDest: P_char;
  pSour: P_char;
  w: DWORD;
  h: DWORD;
  v: UInt32;
  SourRect: TRect;
  DestRect: TRect;
  hRet: HRESULT;
  y: Integer;

  SurfaceDesc: D3DSURFACE_DESC;
  CanWriteToBackbuffer: Boolean;
  BackBufferSurface: IDirect3DSurface;
  OverlayBufferSurface: IDirect3DSurface;
  LockedRectDest: D3DLOCKED_RECT;

  pCurByte: Puint08;
  pDest2: Puint08;
  dx: uint32;
  dy: uint32;
  dwImageSize: uint32;
  stop: uint32;

  aMonitorInfo: MONITORINFO;

  nTitleHeight: Integer;
  nBorderWidth: Integer;
  nBorderHeight: Integer;
  ddofx: DDOVERLAYFX;

  x: Integer;
  Y3: uint08;

  Y0U0Y1V0: DWORD;
  Y0: Integer;
  U0: Integer;
  Y1: Integer;
  V0: Integer;

  R: Integer;
  G: Integer;
  B: Integer;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_UpdateOverlay').
      _(pSurface, 'pSurface').
      _(SrcRect, 'SrcRect').
      _(DstRect, 'DstRect').
      _(EnableColorKey, 'EnableColorKey').
      _(ColorKey, 'ColorKey').
   LogEnd();

//{$IFDEF GAME_HACKS_ENABLED}??
// Cxbx has #ifndef UnrealChampionshipHack
  if not Assigned(pSurface) then
  begin
    EmuWarning('pSurface == NULL!');
  end
  else
  begin
    // Calculate the source rectangle :
    SourRect := Classes.Rect(0, 0, g_dwOverlayW, g_dwOverlayH);

    // manually copy data over to overlay
    if (g_bYUY2OverlaysSupported) then
    begin
      ZeroMemory(@ddsd2, sizeof(ddsd2));
      ddsd2.dwSize := sizeof(ddsd2);

      hRet := IDirectDrawSurface7(g_pDDSOverlay7).Lock(NULL, {out}ddsd2, DDLOCK_SURFACEMEMORYPTR or DDLOCK_WAIT, 0);
      if FAILED(hRet) then
        EmuWarning('Unable to lock overlay surface!' +#13#10+ DxbxD3DErrorString(hRet));

      // copy data
      begin
        pDest := P_char(ddsd2.lpSurface);
        pSour := P_char(pSurface.Emu.Lock);

        w := g_dwOverlayW;
        h := g_dwOverlayH;

        // TODO -oCXBX: sucker the game into rendering directly to the overlay (speed boost)
        if ((DWORD(ddsd2.lPitch) = w * 2) and (DWORD(g_dwOverlayP) = w * 2)) then
          memcpy(pDest, pSour, h * w * 2)
        else
        begin
          if h > 0 then // Dxbx addition, to prevent underflow
          for y := 0 to h - 1 do
          begin
            memcpy(pDest, pSour, w * 2);
            Inc(pDest, ddsd2.lPitch);
            Inc(pSour, g_dwOverlayP);
          end;
        end;
      end;

      IDirectDrawSurface7(g_pDDSOverlay7).Unlock(NULL);

      // update overlay!

      // Calculate the destination rectangle :
      GetWindowRect(g_hEmuWindow, {var}DestRect);
      nTitleHeight  := 0;//GetSystemMetrics(SM_CYCAPTION);
      nBorderWidth  := 0;//GetSystemMetrics(SM_CXSIZEFRAME);
      nBorderHeight := 0;//GetSystemMetrics(SM_CYSIZEFRAME);
      Inc(DestRect.left,   nBorderWidth);
      Dec(DestRect.right,  nBorderWidth);
      Inc(DestRect.top,    nTitleHeight + nBorderHeight);
      Dec(DestRect.bottom, nBorderHeight);

      ZeroMemory(@aMonitorInfo, sizeof(aMonitorInfo));
      aMonitorInfo.cbSize := sizeof(MONITORINFO);
      GetMonitorInfo(g_hMonitor, @aMonitorInfo);

      Dec(DestRect.left,   aMonitorInfo.rcMonitor.left);
      Dec(DestRect.right,  aMonitorInfo.rcMonitor.left);
      Dec(DestRect.top,    aMonitorInfo.rcMonitor.top);
      Dec(DestRect.bottom, aMonitorInfo.rcMonitor.top);

      ZeroMemory(@ddofx, sizeof(ddofx));
      ddofx.dwSize := sizeof(DDOVERLAYFX);
      ddofx.dckDestColorkey.dwColorSpaceLowValue := 0;
      ddofx.dckDestColorkey.dwColorSpaceHighValue := 0;
      {Dxbx unused hRet :=} IDirectDrawSurface7(g_pDDSOverlay7).UpdateOverlay(@SourRect, IDirectDrawSurface7(g_pDDSPrimary7), @DestRect, {DDOVER_KEYDESTOVERRIDE or} DDOVER_SHOW, {&ddofx}nil);
    end
    else // not g_bYUY2OverlaysSupported
    begin
      // We need to write to the backbuffer ourselves, so get a handle on it :
      BackBufferSurface := nil;
      hRet := g_pD3DDevice.GetBackBuffer({$IFDEF DXBX_USE_D3D9}{iSwapChain=}0,{$ENDIF} 0, D3DBACKBUFFER_TYPE_MONO, @BackBufferSurface);
      if (hRet = D3D_OK) then
      begin
        // Determine if the overlay can be written directly to the backbuffer :
        BackBufferSurface.GetDesc({out}SurfaceDesc);
        CanWriteToBackbuffer := ((DestRect.Top - SourRect.Top) < 10)
                            and ((DestRect.Right - SourRect.Right) < 20)
                            and (SurfaceDesc.Format in [D3DFMT_A8R8G8B8, D3DFMT_X8R8G8B8{, D3DFMT_R8G8B8?}]);

        // If we can write to the back buffer, work with that, else use the screenshotbuffer as temporary surface :
        if CanWriteToBackbuffer then
          OverlayBufferSurface := BackBufferSurface
        else
        begin
          AssureExtraXRGBSurface(BackBufferSurface, 'EmuD3DDevice_UpdateOverlay');
          OverlayBufferSurface := ExtraXRGBSurface; // Note : This surface is always in RGB format
        end;

        // Manually translate the YUY2 formatted input surface into the RGB buffer of the pre-determined output surface :
        if (OverlayBufferSurface.LockRect({out}LockedRectDest, NULL, 0) = D3D_OK) then
        begin
          // Since this is a manual X_D3DRESOURCE_DATA_FLAG_YUVSURF,
          // skip the refcount we put at the begin, to get to the source data:
          pCurByte := Puint08(pSurface.Emu.Lock + SizeOf(DWORD));

          pDest2 := Puint08(LockedRectDest.pBits);
          dx:=0;
          dwImageSize := g_dwOverlayP*g_dwOverlayH;

(*
          // grayscale
          if (false) then
          begin
            if g_dwOverlayH > 0 then // Dxbx addition, to prevent underflow
            for y := 0 to g_dwOverlayH-1 do
            begin
              stop := g_dwOverlayW*4;
              x := 0; while Uint32(x) < stop do
              begin
                Y3 := pCurByte^;
                pDest2[x+0] := Y3;
                pDest2[x+1] := Y3;
                pDest2[x+2] := Y3;
                pDest2[x+3] := $FF;

                pCurByte := @(pCurByte[2]); // Inc(pCurByte, 2) doesn't work
                Inc(x, 4);
              end; // while

              pDest2:= @pDest2[LockedRectDest.Pitch];
           end;
          end
          else
*)
          // full color conversion (YUY2->XRGB)
          begin
            v := 0; while v < dwImageSize do
            begin
              // Convert YUY2 (= packed YUV, Y using all 256 levels) to RGB
              // See http://www.fourcc.org/yuv.php#YUY2
              // and http://en.wikipedia.org/wiki/YUV
              // and http://msdn.microsoft.com/en-us/library/ms893078.aspx
              // We do it integer-based for speed; The fractions and divisions
              // in the code below are compiled into (sign-extending!) shifts,
              // so it's quite fast, really. Note : If we would write out the
              // right shifts (using shr) ourselves, they would not sign-extend
              // negative values, causing color-wrapping in the output.

              Y0U0Y1V0 := PDWORD(pCurByte+v)^; Inc(v, 4);

              Y0 := Byte(Y0U0Y1V0       ) * FIXED_POINT_FACTOR;
              Y1 := Byte(Y0U0Y1V0 shr 16) * FIXED_POINT_FACTOR;
              V0 := Byte(Y0U0Y1V0 shr 24) - 128;
              U0 := Byte(Y0U0Y1V0 shr  8) - 128;

              R :=                                        (Round(1.402*FIXED_POINT_FACTOR)*V0);
              G := (Round(0.344*FIXED_POINT_FACTOR)*U0) + (Round(0.714*FIXED_POINT_FACTOR)*V0);
              B := (Round(1.772*FIXED_POINT_FACTOR)*U0);

              pDest2[dx+0] := ClampIntToByte((Y0 + B) div FIXED_POINT_FACTOR);
              pDest2[dx+1] := ClampIntToByte((Y0 - G) div FIXED_POINT_FACTOR);
              pDest2[dx+2] := ClampIntToByte((Y0 + R) div FIXED_POINT_FACTOR);
              pDest2[dx+3] := $FF;

              pDest2[dx+4] := ClampIntToByte((Y1 + B) div FIXED_POINT_FACTOR);
              pDest2[dx+5] := ClampIntToByte((Y1 - G) div FIXED_POINT_FACTOR);
              pDest2[dx+6] := ClampIntToByte((Y1 + R) div FIXED_POINT_FACTOR);
              pDest2[dx+7] := $FF;

              Inc(dx, 8);
              if (dx >= 4*g_dwOverlayW) then
              begin
                pDest2:= @pDest2[LockedRectDest.Pitch];
                dx := 0;
              end;
            end; // while
          end;

          OverlayBufferSurface.UnlockRect();

          if not CanWriteToBackbuffer then
            // When the overlay could not directly be converted into the back buffer,
            // we now have to stretch-copy there (this also does a format-conversion, if needed) :
            if D3DXLoadSurfaceFromSurface({DestSurface=}BackBufferSurface, NULL, {DestRect=}nil, {DestSurface=}OverlayBufferSurface, NULL, @SourRect, D3DX_FILTER_LINEAR{D3DX_DEFAULT}, 0) <> D3D_OK then
              DbgPrintf('EmuD3D8 : UpdateOverlay could not convert buffer!');
        end;
      end;

      BackBufferSurface := nil;
      OverlayBufferSurface := nil;

      // Update overlay if present was not called since the last call to
      // EmuD3DDevice_UpdateOverlay.
      if g_bHackUpdateSoftwareOverlay then
        DxbxPresent(nil, nil, 0, nil);
    end;

    g_bHackUpdateSoftwareOverlay := TRUE;
  end;

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetOverlayUpdateStatus(): LONGBOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuD3D8 : EmuD3DDevice_GetOverlayUpdateStatus();');
    EmuSwapFS(fsXbox);
  end;

  Result := (g_bHackUpdateSoftwareOverlay = False);
end;

procedure XTL_EmuD3DDevice_BlockUntilIdle(); stdcall;
// Branch:dxbx  Translator:Shadow_Tj  Done:100
{$IFDEF DXBX_USE_D3D9}
var
  pQuery: PIDirect3DQuery9;
  data: BOOL;
{$ENDIF}
begin
  EmuSwapFs(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_BlockUntilIdle();');

{$IFDEF DXBX_USE_D3D9}
  // DXBX: Own implementation of BlockUntilIdle
  // create an event and spin wait on it
  g_pD3DDevice.CreateQuery(D3DQUERYTYPE_EVENT, @pQuery);
  pQuery.Issue(D3DISSUE_END);
  while (pQuery.GetData(@data, sizeof(data), D3DGETDATA_FLUSH) = S_FALSE) do
  begin
    // busy wait
  end;
  pQuery._Release();
{$ELSE}
  Unimplemented('EmuD3DDevice_BlockUntilIdle');
{$ENDIF}

  EmuSwapFs(fsXbox);
end;

procedure XTL_EmuD3DDevice_BlockUntilVerticalBlank(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_BlockUntilVerticalBlank();');

  // - DXBX - DO NOT ENABLE GetVSync CHECK... ALMOST EVERY GAME CRASHES WHEN YOU DO NOT WAIT !!!
  // segaGT tends to freeze with this on
  //    if(g_XBVideo.GetVSync())
  IDirectDraw7(g_pDD7).WaitForVerticalBlank(DDWAITVB_BLOCKBEGIN, 0);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetVerticalBlankCallback
(
  pCallback: D3DVBLANKCALLBACK
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetVerticalBlankCallback').
      _(Addr(pCallback), 'pCallback').
    LogEnd();

  if Addr(pCallback) <> Null then
    g_pVBCallback := pCallback
  else
    // Dxbx note : Zapper passes the Handle of a previously created thread here... wierd!
    EmuWarning('SetVerticalBlankCallback ignored invalid Callback address');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetTextureState_TexCoordIndex
(
  Stage: DWORD;
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetTextureState_TexCoordIndex').
      _(Stage, 'Stage').
      _(Value, 'Value').
    LogEnd();

  // Native doesn't support D3DTSS_TCI_OBJECT, D3DTSS_TCI_SPHERE, D3DTSS_TCI_TEXGEN_MAX or higher:
  if (Value and $FFFF0000) > D3DTSS_TCI_CAMERASPACEREFLECTIONVECTOR then // Dxbx note : Cxbx uses 0x00030000, which is not enough for the Strip XDK sample!
    EmuWarning('EmuD3DDevice_SetTextureState_TexCoordIndex: Unknown TexCoordIndex Value (0x%.08X)', [Value]);

  // Dxbx addition : Set this value into the TextureState structure too (so other code will read the new current value)
  XTL_EmuD3DDeferredTextureState[Stage, Ord(DxbxFromNewVersion_D3DTSS(X_D3DTSS_TEXCOORDINDEX))] := Value;
  // TODO -oDxbx : Update the D3D DirtyFlags too?

  IDirect3DDevice_SetTextureStageState(g_pD3DDevice, Stage, X_D3DTSS_TEXCOORDINDEX, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_TwoSidedLighting
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetTextureState_TwoSidedLighting').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_TWOSIDEDLIGHTING, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_BackFillMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_BackFillMode').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_BACKFILLMODE, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetTextureState_BorderColor
(
  Stage: DWORD;
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetTextureState_BorderColor').
      _(Stage, 'Stage').
      _(Value, 'Value').
    LogEnd();

  // Dxbx addition : Set this value into the TextureState structure too (so other code will read the new current value)
  XTL_EmuD3DDeferredTextureState[Stage, Ord(DxbxFromNewVersion_D3DTSS(X_D3DTSS_BORDERCOLOR))] := Value;
  // TODO -oDxbx : Update the D3D DirtyFlags too?

  IDirect3DDevice_SetTextureStageState(g_pD3DDevice, Stage, X_D3DTSS_BORDERCOLOR, Value);

//  // TODO : Test if this stage's texture was created with the X_D3DFORMAT_BORDERCOLOR flag before doing this :
//  IDirect3DDevice_SetTextureStageState(g_pD3DDevice, Stage, X_D3DTSS_ADDRESSU, D3DTADDRESS_BORDER);
//  IDirect3DDevice_SetTextureStageState(g_pD3DDevice, Stage, X_D3DTSS_ADDRESSV, D3DTADDRESS_BORDER);
//  IDirect3DDevice_SetTextureStageState(g_pD3DDevice, Stage, X_D3DTSS_ADDRESSW, D3DTADDRESS_BORDER);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetTextureState_ColorKeyColor
(
  Stage: DWORD;
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetTextureState_ColorKeyColor').
      _(Stage, 'Stage').
      _(Value, 'Value').
    LogEnd();

  // Dxbx addition : Set this value into the TextureState structure too (so other code will read the new current value)
  XTL_EmuD3DDeferredTextureState[Stage, Ord(DxbxFromNewVersion_D3DTSS(X_D3DTSS_COLORKEYCOLOR))] := Value;
  // TODO -oDxbx : Update the D3D DirtyFlags too?

  EmuWarning('SetTextureState_ColorKeyColor is not supported!');

  EmuSwapFS(fsXbox);
end;

(*function XTL_EmuD3DDevice_SetTextureState_ParameterCheck
(
  State: D3DRENDERSTATETYPE;
  Value: DWORD
): HResult; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetTextureState_ParameterCheck').
      _(Integer(Ord(State)), 'State').
      _(Value, 'Value').
    LogEnd();

  EmuWarning('SetTextureState_ParameterCheck is not supported!');
  Result := 0;

  EmuSwapFS(fsXbox);
end; *)

procedure XTL_EmuD3DDevice_SetTextureState_BumpEnv
(
    Stage: DWORD;
    Type_: X_D3DTEXTURESTAGESTATETYPE;
    Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  Type_VersionIndependent: X_D3DTEXTURESTAGESTATETYPE;
begin
  EmuSwapFS(fsWindows);

  Type_VersionIndependent := DxbxFromOldVersion_D3DTSS(Type_);
  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetTextureState_BumpEnv').
      _(Stage, 'Stage').
      _(DWORD(Type_), 'Type').
      _(Type_VersionIndependent, 'Type_VersionIndependent').
      _(Value, 'Value').
      _(DWToF(Value), 'Value (as Float)').
    LogEnd();

  // Dxbx addition : Set this value into the TextureState structure too (so other code will read the new current value)
  XTL_EmuD3DDeferredTextureState[Stage, Ord(Type_)] := Value;
  // TODO -oDxbx : Update the D3D DirtyFlags too?

  // Dxbx Note : The BumpEnv values don't need a XB2PC conversion
  // Dxbx Note : The BumpEnv types all have a PC counterpart (so no -1 test needed)

  IDirect3DDevice_SetTextureStageState(g_pD3DDevice, Stage, Type_VersionIndependent, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_FrontFace
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_FrontFace').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_FRONTFACE, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_LogicOp
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_LogicOp').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_LOGICOP, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_NormalizeNormals
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_NormalizeNormals').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_NORMALIZENORMALS, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_TextureFactor
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_TextureFactor').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_TEXTUREFACTOR, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_ZBias
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
   LogBegin('EmuD3DDevice_SetRenderState_ZBias').
     _(Value, 'Value').
   LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_ZBIAS, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_EdgeAntiAlias
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_EdgeAntiAlias').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_EDGEANTIALIAS, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_FillMode
(
  Value: X_D3DFILLMODE
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_FillMode').
      _(DWORD(Value), 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_FILLMODE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_FogColor
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_FogColor').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_FOGCOLOR, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_Dxt1NoiseEnable
(
  Value: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_Dxt1NoiseEnable').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_DXT1NOISEENABLE, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_Simple(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Value: DWORD;
  {1 ECX}Method: X_NV2AMETHOD
  ); register; // VALIDATED fastcall simulation - See Translation guide
// Branch:shogun  Revision:20100412  Translator:Shadow_Tj  Done:100
var
  XboxRenderState: X_D3DRenderStateType;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_Simple').
      _(Method, 'Method').
      _(Value, 'Value').
    LogEnd();

  XboxRenderState := DxbxXboxMethodToRenderState(Method);
  // Dxbx note : Methods are already version-independant
  if (int(XboxRenderState) = -1) then
    EmuWarning('SetRenderState_Simple({Method=}0x%.08X, {Value=}0x%.08X) - unsupported method!', [Method, Value])
  else
    // Use a helper for the simple render states, as SetRenderStateNotInline
    // needs to be able to call it too :
    XTL_EmuD3DDevice_SetRenderState_Simple_Internal(XboxRenderState, {Xbox}Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_Simple_Internal(
  XboxRenderState: X_D3DRenderStateType;
  XboxValue: DWORD
  ); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  PCValue: DWORD;
begin
  PCValue := Dxbx_SetRenderState(XboxRenderState, XboxValue);

  if MayLog(lfUnit or lfReturnValue) then
  begin
    if PCValue <> XboxValue then
      DbgPrintf('%s := 0x%.08X (converted from Xbox value 0x%.08X)', [DxbxRenderStateXB2String[XboxRenderState], PCValue, XboxValue])
    else
      DbgPrintf('%s := 0x%.08X', [DxbxRenderStateXB2String[XboxRenderState], PCValue]);
  end;
end;

procedure XTL_EmuD3DDevice_SetRenderState_VertexBlend
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_VertexBlend').
      _(Ord(Value), 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_VERTEXBLEND, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_PSTextureModes
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_PSTextureModes').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_PSTEXTUREMODES, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_CullMode
(
  Value: X_D3DCULL
); stdcall;
// Branch:shogun  Revision:162  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_CullMode').
      _(DWORD(Value), 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_CULLMODE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_LineWidth
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_LineWidth').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_LINEWIDTH, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_StencilFail
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_StencilFail').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_STENCILFAIL, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_OcclusionCullEnable
(
  Value: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_OcclusionCullEnable').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_OCCLUSIONCULLENABLE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_StencilCullEnable
(
  Value: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_StencilCullEnable').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_STENCILCULLENABLE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_RopZCmpAlwaysRead
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_RopZCmpAlwaysRead').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_ROPZCMPALWAYSREAD, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_RopZRead
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_RopZRead').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_ROPZREAD, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_DoNotCullUncompressed
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_DoNotCullUncompressed').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_DONOTCULLUNCOMPRESSED, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_ZEnable
(
  Value: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DDevice_SetRenderState_ZEnable').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_ZENABLE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_StencilEnable
(
  Value: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_StencilEnable').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_STENCILENABLE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_MultiSampleAntiAlias
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_MultiSampleAntiAlias').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_MULTISAMPLEANTIALIAS, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_MultiSampleMask
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_MultiSampleMask').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_MULTISAMPLEMASK, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_MultiSampleMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_MultiSampleMode').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_MULTISAMPLEMODE, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_MultiSampleRenderTargetMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_MultiSampleRenderTargetMode').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_MULTISAMPLERENDERTARGETMODE, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_ShadowFunc
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_ShadowFunc').
      _(Value, 'Value').
    LogEnd();

  // Dxbx addition : Set this value into the RenderState structure too (so other code will read the new current value)
  XTL_EmuMappedD3DRenderState[X_D3DRS_SHADOWFUNC]^ := Value;

  if MayLog(lfUnit) then
    EmuWarning('ShadowFunc not implemented');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_YuvEnable
(
  Value: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_YuvEnable').
      _(Value, 'Value').
    LogEnd();

  // Dxbx addition : Set this value into the RenderState structure too (so other code will read the new current value)
  XTL_EmuMappedD3DRenderState[X_D3DRS_YUVENABLE]^ := Value;

  // HACK: Display YUV surface by using an overlay.
  if (Value <> BOOL_FALSE) <> g_fYuvEnabled then
  begin
    g_fYuvEnabled := (Value <> BOOL_FALSE);

    if MayLog(lfUnit) then
      EmuWarning('EmuD3DDevice_SetRenderState_YuvEnable using overlay!');

    EmuSwapFS(fsXbox);
    XTL_EmuD3DDevice_EnableOverlay(BOOL(g_fYuvEnabled));
    EmuSwapFS(fsWindows);
  end;

  if (g_fYuvEnabled) then
  begin
    EmuSwapFS(fsXbox);
    XTL_EmuD3DDevice_UpdateOverlay(g_YuvSurface, nil, nil, BOOL_FALSE, 0);
    EmuSwapFS(fsWindows);
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetTransform
(
  State: X_D3DTRANSFORMSTATETYPE;
  {CONST} pMatrix: PD3DMATRIX
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  PCState: D3DTRANSFORMSTATETYPE;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetTransform').
      _(State, 'State').
      _(pMatrix, 'pMatrix').
    LogEnd();

  (* Commented by CXBX
  DbgPrintf('pMatrix (%d)', [Ord(State)]);
  DbgPrintf('{');
  DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._11, pMatrix._12, pMatrix._13, pMatrix._14]);
  DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._21, pMatrix._22, pMatrix._23, pMatrix._24]);
  DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._31, pMatrix._32, pMatrix._33, pMatrix._34]);
  DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._41, pMatrix._42, pMatrix._43, pMatrix._44]);
  DbgPrintf('}');

  if (Ord(State) = 6) and (pMatrix._11 = 1.0) and (pMatrix._22 = 1.0) and (pMatrix._33 = 1.0) and (pMatrix._44 = 1.0) then
  begin
    Xtl_g_bSkipPush := TRUE;
    DbgPrintf('SkipPush ON');
  end
  else
  begin
    Xtl_g_bSkipPush := FALSE;
    DbgPrintf('SkipPush OFF');
  end;
  *)

  PCState := EmuXB2PC_D3DTS(State);

  Result := g_pD3DDevice.SetTransform(PCState, pMatrix{$IFDEF DXBX_USE_D3D9}^{$ENDIF});

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetTransform
(
  State: X_D3DTRANSFORMSTATETYPE;
  pMatrix: PD3DMATRIX
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetTransform').
      _(State, 'State').
      _(pMatrix, 'pMatrix').
    LogEnd();

  Result := g_pD3DDevice.GetTransform(EmuXB2PC_D3DTS(State), {out}pMatrix^);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DVertexBuffer_Lock
(
  pVertexBuffer: PX_D3DVertexBuffer;
  OffsetToLock: UINT;
  SizeToLock: UINT;
  ppbData: PPBYTE;
  Flags: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pPCVertexBuffer: XTL_PIDirect3DVertexBuffer8;
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DVertexBuffer_Lock').
      _(pVertexBuffer, 'pVertexBuffer').
      _(OffsetToLock, 'OffsetToLock').
      _(SizeToLock, 'SizeToLock').
      _(ppbData, 'ppbData').
      _(Flags, 'Flags').
    LogEnd();

  // TODO -oDxbx : Should we call EmuVerifyResourceIsRegistered here?
  EmuVerifyResourceIsRegistered(pVertexBuffer);

  pPCVertexBuffer := pVertexBuffer.Emu.VertexBuffer;

  hRet := IDirect3DVertexBuffer(pPCVertexBuffer).Lock(OffsetToLock, SizeToLock, {out}TLockData(pVertexBuffer.Data), EmuXB2PC_D3DLock(Flags)); // Fixed flags check, Battlestar Galactica now displays graphics correctly

  if (FAILED(hRet)) then
    EmuWarning('VertexBuffer Lock Failed!' +#13#10+ DxbxD3DErrorString(hRet))
  else
  begin
    if Assigned(ppbData) then
      ppbData^ := TLockData(pVertexBuffer.Data);

    if MayLog(lfUnit or lfReturnValue) then
      DbgPrintf('EmuD3D8 : VertexBuffer locked : ' + ResourceToString(pVertexBuffer));
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DVertexBuffer_Lock2
(
  pVertexBuffer: PX_D3DVertexBuffer;
  Flags: DWORD
): PByte; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pPCVertexBuffer: XTL_PIDirect3DVertexBuffer8;
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DVertexBuffer_Lock2 >> ').
      _(pVertexBuffer, 'pVertexBuffer').
      _(Flags, 'Flags').
    LogEnd();

  EmuSwapFS(fsXbox);

  XTL_EmuD3DVertexBuffer_Lock(pVertexBuffer, {OffsetToLock=}0, {SizeToLock=}0, @Result, Flags);
end;

function XTL_EmuD3DDevice_GetStreamSource2
(
  StreamNumber: UINT;
  pStride: PUINT
): PX_D3DVertexBuffer; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
{$IFDEF DXBX_USE_D3D9}
  OffsetInBytes: DWORD;
{$ENDIF}
  pVertexBuffer: PX_D3DVertexBuffer;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DDevice_GetStreamSource2').
      _(StreamNumber, 'StreamNumber').
      _(pStride, 'pStride').
    LogEnd();

  EmuWarning('Not correctly implemented yet!');
  {ignore HRESULT?}g_pD3DDevice.GetStreamSource(
    StreamNumber,
    {ppVertexBuffer=}@pVertexBuffer,
{$IFDEF DXBX_USE_D3D9}
    {out pOffsetInBytes=}OffsetInBytes, // Ignored?
{$ENDIF}
    {out}pStride^);

  EmuSwapFS(fsXbox);
  Result := pVertexBuffer;
end;

function XTL_EmuD3DDevice_SetStreamSource
(
  StreamNumber: UINT;
  pStreamData: PX_D3DVertexBuffer;
  Stride: UINT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pPCVertexBuffer: XTL_PIDirect3DVertexBuffer8;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('EmuD3DDevice_SetStreamSource').
      _(StreamNumber, 'StreamNumber').
      _(pStreamData, 'pStreamData').
      _(Stride, 'Stride').
    LogEnd();
  end;

  if (StreamNumber = 0) then
    g_pVertexBuffer := pStreamData;

  pPCVertexBuffer := NULL;

  if (pStreamData <> NULL) then
  begin
    EmuVerifyResourceIsRegistered(pStreamData);

    pPCVertexBuffer := pStreamData.Emu.VertexBuffer;
    //??DxbxUnlockD3DResource(pStreamData);
    if Assigned(pPCVertexBuffer) then
      IDirect3DVertexBuffer(pPCVertexBuffer).Unlock();

{$ifdef _DEBUG_TRACK_VB}
    g_bVBSkipStream := g_VBTrackDisable.exists(pPCVertexBuffer);
{$endif}
  end;

  Result := g_pD3DDevice.SetStreamSource(StreamNumber, IDirect3DVertexBuffer(pPCVertexBuffer), {$IFDEF DXBX_USE_D3D9}{OffsetInBytes=}0, {$ENDIF} Stride);
  if FAILED(RESULT) then
    DxbxKrnlCleanup('SetStreamSource Failed!' +#13#10+ DxbxD3DErrorString(Result));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetVertexShader
(
  Handle: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pVertexShader: PVERTEX_SHADER;
  RealHandle: DWORD;
const
  vScale: TD3DXVECTOR4 = (x: 2.0 / 640; y: -2.0 / 480; z: 0.0; w: 0.0);
  vOffset: TD3DXVECTOR4 = (x: -1.0; y: 1.0; z: 0.0; w: 1.0);
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetVertexShader').
      _(Handle, 'Handle').
    LogEnd();

  // Result := D3D_OK;

  g_CurrentVertexShader := Handle;

  // Store viewport offset and scale in constant registers 58 (c-38) and
  // 59 (c-37) used for screen space transformation.
  if (g_VertexShaderConstantMode and X_D3DSCM_NORESERVEDCONSTANTS) = 0 then // Dxbx fix
  begin
    // TODO -oCXBX: Proper solution.
//    with vScale do begin x := 2.0 / 640; y := -2.0 / 480; z := 0.0; w := 0.0; end;
//    with vOffset do begin x := -1.0; y := 1.0; z := 0.0; w := 1.0; end;

{$IFDEF DXBX_USE_D3D9}
    g_pD3DDevice.SetVertexShaderConstantF({58=}X_D3DSCM_RESERVED_CONSTANT1{=-38}+X_D3DSCM_CORRECTION{=96}, @vScale, 1);
    g_pD3DDevice.SetVertexShaderConstantF({59=}X_D3DSCM_RESERVED_CONSTANT2{=-37}+X_D3DSCM_CORRECTION{=96}, @vOffset, 1);
{$ELSE}
    g_pD3DDevice.SetVertexShaderConstant({58=}X_D3DSCM_RESERVED_CONSTANT1{=-38}+X_D3DSCM_CORRECTION{=96}, @vScale, 1);
    g_pD3DDevice.SetVertexShaderConstant({59=}X_D3DSCM_RESERVED_CONSTANT2{=-37}+X_D3DSCM_CORRECTION{=96}, @vOffset, 1);
{$ENDIF}
  end;

  if (VshHandleIsVertexShader(Handle)) then
  begin
    pVertexShader := PVERTEX_SHADER(VshHandleGetVertexShader(Handle).Handle);

{$IFDEF DXBX_USE_D3D9}
    g_pD3DDevice.SetVertexDeclaration(IDirect3DVertexDeclaration9(pVertexShader.pDeclaration));
    Result := g_pD3DDevice.SetVertexShader(IDirect3DVertexShader(pVertexShader.Handle));
{$ELSE}
    Result := g_pD3DDevice.SetVertexShader(pVertexShader.Handle);
{$ENDIF}
  end
  else
  begin
    // Dxbx addition : When setting D3DFVF_XYZRHW, g_VertexShaderSlots 0-D3DVS_XBOX_RESERVEDXYZRHWSLOTS-1 should be destroyed :
    if (Handle and D3DFVF_POSITION_MASK) = D3DFVF_XYZRHW then
      for RealHandle := 0 to D3DVS_XBOX_RESERVEDXYZRHWSLOTS-1 do
        g_VertexShaderSlots[RealHandle] := 0;

    RealHandle := Handle;

{$IFDEF DXBX_USE_D3D9}
    g_pD3DDevice.SetVertexShader(NULL);
    Result := g_pD3DDevice.SetFVF(RealHandle);
{$ELSE}
    Result := g_pD3DDevice.SetVertexShader(RealHandle);
{$ENDIF}
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_DrawVertices
(
  PrimitiveType: X_D3DPRIMITIVETYPE;
  StartVertex: UINT;
  VertexCount: UINT
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  VPDesc: VertexPatchDesc;
  VertPatch: VertexPatcher;
//  bPatched: _bool;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_DrawVertices').
      _(PrimitiveType, 'PrimitiveType').
      _(StartVertex, 'StartVertex').
      _(VertexCount, 'VertexCount').
    LogEnd();

  XTL_EmuUpdateDeferredStates();

//  EmuUnswizzleActiveTexture(); // This messed up textures in PSTest2_4627, but not anymore since rev 1245
{$IFDEF DXBX_ENABLE_P8_CONVERSION}
  XTL_EmuUpdateActiveTexture();
{$ENDIF}

  VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer

  VPDesc.PrimitiveType := PrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
  VPDesc.dwOffset := StartVertex;
  VPDesc.pVertexStreamZeroData := nil;
  VPDesc.uiVertexStreamZeroStride := 0;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

  if IsValidCurrentShader() then
  begin
    {$ifdef _DEBUG_TRACK_VB}
    if (not g_bVBSkipStream) then
    begin
    {$endif}

      g_pD3DDevice.DrawPrimitive
      (
          EmuPrimitiveType(VPDesc.PrimitiveType),
          VPDesc.dwOffset, // Dxbx note : Cxbx wrongly uses StartVertex here!
          VPDesc.dwPrimitiveCount
      );

    {$ifdef _DEBUG_TRACK_VB}
    end;
    {$endif}
  end;

  VertPatch.Restore();

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_DrawVerticesUP
(
  PrimitiveType: X_D3DPRIMITIVETYPE;
  VertexCount: UINT;
  pVertexStreamZeroData: PVOID;
  VertexStreamZeroStride: UINT
); stdcall;
// Branch:shogun  Revision:162  Translator:Shadow_Tj  Done:100
var
  VPDesc: VertexPatchDesc;
  VertPatch: VertexPatcher;
//  bPatched: _bool;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_DrawVerticesUP').
      _(PrimitiveType, 'PrimitiveType').
      _(VertexCount, 'VertexCount').
      _(pVertexStreamZeroData, 'pVertexStreamZeroData').
      _(VertexStreamZeroStride, 'VertexStreamZeroStride').
    LogEnd();

  XTL_EmuUpdateDeferredStates();

//  EmuUnswizzleActiveTexture(); // This messed up the letters in Chunktro, but not anymore since rev 1245
{$IFDEF DXBX_ENABLE_P8_CONVERSION}
  XTL_EmuUpdateActiveTexture();
{$ENDIF}

  VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer

  VPDesc.PrimitiveType := PrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
  VPDesc.dwOffset := 0;
  VPDesc.pVertexStreamZeroData := pVertexStreamZeroData;
  VPDesc.uiVertexStreamZeroStride := VertexStreamZeroStride;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

  if (IsValidCurrentShader()) then
  begin
    {$ifdef _DEBUG_TRACK_VB}
    if ( not g_bVBSkipStream) then
    begin
    {$endif}

      g_pD3DDevice.DrawPrimitiveUP
      (
          EmuPrimitiveType(VPDesc.PrimitiveType),
          VPDesc.dwPrimitiveCount,
          VPDesc.pVertexStreamZeroData,
          VPDesc.uiVertexStreamZeroStride
      );

    {$ifdef _DEBUG_TRACK_VB}
     end;
    {$endif}
  end;

  VertPatch.Restore();

  EmuSwapFS(fsXbox);
end;

//D3DMINLINE HRESULT WINAPI D3DDevice::DrawIndexedPrimitive(D3DPRIMITIVETYPE PrimitiveType, UINT MinIndex, UINT NumIndices, UINT StartIndex, UINT PrimitiveCount)
// { D3DDevice_DrawIndexedVertices(PrimitiveType, D3DVERTEXCOUNT(PrimitiveType, PrimitiveCount), D3D__IndexData + StartIndex); return S_OK; }
procedure XTL_EmuD3DDevice_DrawIndexedVertices
(
  PrimitiveType: X_D3DPRIMITIVETYPE;
  VertexCount: UINT;
  pIndexData: PWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  dwSize: DWORD;
  hRet: HRESULT;
  pData: PBYTE;
  VPDesc: VertexPatchDesc;
  VertPatch: VertexPatcher;
//  bPatched: _bool;
  pbData: PBYTE;
  bActiveIB: _bool;
  pPCIndexBuffer: XTL_PIDirect3DIndexBuffer8;
  BaseIndex: UINT;

  uiNumVertices: UINT;
  uiStartIndex: UINT;
  FatalError: _bool;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_DrawIndexedVertices').
      _(PrimitiveType, 'PrimitiveType').
      _(VertexCount, 'VertexCount').
      _(pIndexData, 'pIndexData').
    LogEnd();
// Test only the smallest of 3 X_D3DPT_TRIANGLESTRIP calls done by MatrixPaletteSkinning :
//if False then
//if (PrimitiveType <> X_D3DPT_TRIANGLESTRIP) or (VertexCount < 300) then
//if pIndexData <> nil then // This skips 1st subset of a mesh
//if pIndexData = nil then // Draws only 1st subset of a mesh; Shows glitch in MatrixPaletteSkinning:F11 (wireframe),A (circeling snake)
begin
  // update index buffer, if necessary
  if (g_pIndexBuffer <> nil) and (g_pIndexBuffer.Emu.Lock = X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
  begin
    dwSize := VertexCount * SizeOf(Word);   // 16-bit indices

    hRet := IDirect3DDevice_CreateIndexBuffer
    (g_pD3DDevice,
        dwSize, {Usage=}0, D3DFMT_INDEX16, D3DPOOL_MANAGED,
        @(g_pIndexBuffer.Emu.IndexBuffer)
    );

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('EmuD3DDevice_DrawIndexedVertices: IndexBuffer Create Failed!' +#13#10+ DxbxD3DErrorString(hRet));

    // Dxbx addition : Initialize Common field properly :
    g_pIndexBuffer.Common := g_pIndexBuffer.Common or X_D3DCOMMON_TYPE_INDEXBUFFER;

    pData := nil;
    hRet := IDirect3DIndexBuffer(g_pIndexBuffer.Emu.IndexBuffer).Lock(0, dwSize, {out}TLockData(pData), 0);
    if (FAILED(hRet)) then
      DxbxKrnlCleanup('DrawIndexedVertices : IndexBuffer Lock Failed!' +#13#10+ DxbxD3DErrorString(hRet));

    memcpy(pData, Pvoid(g_pIndexBuffer.Data), dwSize);

    g_pIndexBuffer.Data := ULONG(pData);

    IDirect3DIndexBuffer(g_pIndexBuffer.Emu.IndexBuffer).Unlock();

    hRet := g_pD3DDevice.SetIndices(IDirect3DIndexBuffer(g_pIndexBuffer.Emu.IndexBuffer){$IFDEF DXBX_USE_D3D9}{$MESSAGE 'fixme'}{$ELSE}, g_dwBaseVertexIndex{$ENDIF});

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('DrawIndexedVertices : SetIndices Failed!' +#13#10+ DxbxD3DErrorString(hRet));
  end;

  XTL_EmuUpdateDeferredStates();

//  if(PrimitiveType < X_D3DPT_POINTLIST) or (PrimitiveType >= X_D3DPT_MAX) then
//  begin
//    DxbxKrnlCleanup('DrawIndexedVertices : Unknown primitive type: 0x%.02X', [Ord(PrimitiveType)]);
//  end;
//  EmuUnswizzleActiveTexture(); // This messes up the PrimitiveType stackvalue (overwrite?!) in Cubemap sample
{$IFDEF DXBX_ENABLE_P8_CONVERSION}
  XTL_EmuUpdateActiveTexture();
{$ENDIF}
// If EmuUnswizzleActiveTexture was called, the above test would fail :
//  if(PrimitiveType < X_D3DPT_POINTLIST) or (PrimitiveType >= X_D3DPT_MAX) then
//  begin
//    DxbxKrnlCleanup('DrawIndexedVertices : Unknown primitive type: 0x%.02X', [Ord(PrimitiveType)]);
//  end;

  if (PrimitiveType = X_D3DPT_LINELOOP) or (PrimitiveType = X_D3DPT_QUADLIST) then
    EmuWarning('Unsupported PrimitiveType! (%d)', [DWORD(PrimitiveType)]);

  VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer

{$IFDEF DXBX_INDEXED_QUADLIST_TEST}
  if (PrimitiveType = X_D3DPT_QUADLIST) then
    // Fake another primitive type, so the vertex patcher doesn't do it's quad-splitting trick :
    VPDesc.PrimitiveType := X_D3DPT_TRIANGLEFAN
  else
{$ENDIF}
    VPDesc.PrimitiveType := PrimitiveType;

  VPDesc.dwVertexCount := VertexCount;
  VPDesc.dwOffset := 0;
  VPDesc.pVertexStreamZeroData := nil;
  VPDesc.uiVertexStreamZeroStride := 0;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer
  FatalError := false;

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, @FatalError);
  VertexCount := VPDesc.dwVertexCount; // Dxbx addition : Use the new VertexCount

  {$ifdef _DEBUG_TRACK_VB}
  if not g_bVBSkipStream then
  begin
  {$endif}

    bActiveIB := false;

    pPCIndexBuffer := nil;

    // check if there is an active index buffer
    begin
      BaseIndex := 0;

      g_pD3DDevice.GetIndices(@pPCIndexBuffer{$IFDEF DXBX_USE_D3D9}{$MESSAGE 'fixme'}{$ELSE}, {out}BaseIndex{$ENDIF});

      if (pPCIndexBuffer <> nil) then
      begin
        bActiveIB := true;
        IDirect3DIndexBuffer(pPCIndexBuffer)._Release();
        pPCIndexBuffer := nil; // Dxbx addition - nil out after decreasing reference count
       end;
    end;

    // uiNumVertices := 0;
    // uiStartIndex := 0;

    // TODO -oCXBX: caching (if it becomes noticably slow to recreate the buffer each time)
    if (not bActiveIB) then
    begin
      hRet := IDirect3DDevice_CreateIndexBuffer(g_pD3DDevice, VertexCount*SizeOf(Word), D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_MANAGED, @pPCIndexBuffer);
      if FAILED(hRet) then
        DxbxKrnlCleanup('DrawIndexedVertices: IndexBuffer Create Failed!' +#13#10+ DxbxD3DErrorString(hRet));

      pbData := nil;

      IDirect3DIndexBuffer(pPCIndexBuffer).Lock(0, 0, {out}TLockData(pbData), 0);

      if (pbData = nil) then
        DxbxKrnlCleanup('DrawIndexedVertices : Could not lock index buffer!');

      memcpy(pbData, pIndexData, VertexCount * SizeOf(Word));

      IDirect3DIndexBuffer(pPCIndexBuffer).Unlock();

      g_pD3DDevice.SetIndices(IDirect3DIndexBuffer(pPCIndexBuffer){$IFDEF DXBX_USE_D3D9}{$MESSAGE 'fixme'}{$ELSE}, g_dwBaseVertexIndex{$ENDIF});

      uiNumVertices := VertexCount;
      uiStartIndex := 0;
    end
    else
    begin
      // Dxbx note : Cxbx does this in reverse, but this is more efficient :
      // TODO -oDxbx: Should we use g_pIndexBuffer or pPCIndexBuffer, as set by SetIndices?
      uiStartIndex := DWORD(pIndexData) div SizeOf(Word);
      uiNumVertices := uiStartIndex + VertexCount; // Note Dxbx : uiStartIndex MUST be added (tested with Gamepad 4361)
    end;

    if (IsValidCurrentShader()) and not FatalError then
    begin
{$IFDEF DXBX_INDEXED_QUADLIST_TEST}
      if (PrimitiveType = X_D3DPT_QUADLIST) then
      begin
        // Indexed quadlist can be drawn using unpatched indexes via multiple draws of 2 'strip' triangles :
        // This is slower (because of the many calls) but doesn't require index buffer patching...
        while uiNumVertices >= 4 do
        begin
          g_pD3DDevice.DrawIndexedPrimitive
          (
            D3DPT_TRIANGLEFAN,
    {$IFDEF DXBX_USE_D3D9}
            {BaseVertexIndex=}0,
    {$ENDIF}
            {MinVertexIndex=}0,
            {NumVertices=}4,
            uiStartIndex,
            {primCount=}2
          );
          Inc(uiStartIndex, 4);
          Dec(uiNumVertices, 4);
        end;
      end
      else
{$ENDIF}
      begin
        g_pD3DDevice.DrawIndexedPrimitive
        (
          EmuPrimitiveType(VPDesc.PrimitiveType),
  {$IFDEF DXBX_USE_D3D9}
          {BaseVertexIndex=}0,
  {$ENDIF}
          {MinVertexIndex=}0,
          uiNumVertices,
          uiStartIndex,
          VPDesc.dwPrimitiveCount
        );
      end;
    end;

    if(not bActiveIB) then
    begin
      g_pD3DDevice.SetIndices(nil{$IFDEF DXBX_USE_D3D9}{$MESSAGE 'fixme'}{$ELSE}, 0{$ENDIF});
      IDirect3DIndexBuffer(pPCIndexBuffer)._Release();
      pPCIndexBuffer := nil; // Dxbx addition - nil out after decreasing reference count
    end;

  {$ifdef _DEBUG_TRACK_VB}
  end;
  {$endif}

  VertPatch.Restore();
end;
  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_DrawIndexedVerticesUP
(
  PrimitiveType: X_D3DPRIMITIVETYPE;
  VertexCount: UINT;
  pIndexData: PVOID;
  pVertexStreamZeroData: PVOID;
  VertexStreamZeroStride: UINT
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  VPDesc: VertexPatchDesc;
  VertPatch: VertexPatcher;
//  bPatched: _bool;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_DrawIndexedVerticesUP').
      _(PrimitiveType, 'PrimitiveType').
      _(VertexCount, 'VertexCount').
      _(pIndexData, 'pIndexData').
      _(pVertexStreamZeroData, 'pVertexStreamZeroData').
      _(VertexStreamZeroStride, 'VertexStreamZeroStride').
    LogEnd();

  // update index buffer, if necessary
  if (g_pIndexBuffer <> nil) and (g_pIndexBuffer.Emu.Lock = X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
    DxbxKrnlCleanup('DrawIndexedVerticesUP: g_pIndexBuffer != 0');

  XTL_EmuUpdateDeferredStates();

//  EmuUnswizzleActiveTexture(); // This messed up the loading screen background image in Rayman Arena, but not anymore since rev 1245
{$IFDEF DXBX_ENABLE_P8_CONVERSION}
  XTL_EmuUpdateActiveTexture();
{$ENDIF}

  if (PrimitiveType = X_D3DPT_LINELOOP) or (PrimitiveType = X_D3DPT_QUADLIST) then
    EmuWarning('Unsupported PrimitiveType! (%d)', [Ord(PrimitiveType)]);

  VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer

  VPDesc.PrimitiveType := PrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
  VPDesc.dwOffset := 0;
  VPDesc.pVertexStreamZeroData := pVertexStreamZeroData;
  VPDesc.uiVertexStreamZeroStride := VertexStreamZeroStride;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

  {$ifdef _DEBUG_TRACK_VB}
  if (not g_bVBSkipStream) then
  begin
  {$endif}

    if (IsValidCurrentShader()) then
    begin
      g_pD3DDevice.DrawIndexedPrimitiveUP
      (
          EmuPrimitiveType(VPDesc.PrimitiveType), 0, VPDesc.dwVertexCount, VPDesc.dwPrimitiveCount, pIndexData,
          D3DFMT_INDEX16, VPDesc.pVertexStreamZeroData, VPDesc.uiVertexStreamZeroStride
      );
    end;

  {$ifdef _DEBUG_TRACK_VB}
  end;
  {$endif}

  VertPatch.Restore();

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetLight
(
  Index: DWORD;
  pLight: PD3DLIGHT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetLight').
      _(Index, 'Index').
      _(pLight, 'pLight').
    LogEnd();

  Result := g_pD3DDevice.SetLight(Index, pLight^);
  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetMaterial
(
  {CONST} pMaterial: PD3DMaterial
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetMaterial').
      _(pMaterial, 'pMaterial').
    LogEnd();

  Result := g_pD3DDevice.SetMaterial({const}pMaterial^);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_LightEnable
(
  Index: DWORD;
  bEnable: BOOL
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_LightEnable').
      _(Index, 'Index').
      _(bEnable, 'bEnable').
    LogEnd();

  Result := g_pD3DDevice.LightEnable(Index, bEnable <> BOOL_FALSE);
  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetTextureStageStateNotInline
(
  Stage: DWORD;
  Type_: X_D3DTEXTURESTAGESTATETYPE;
  Value: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  Type_VersionIndependent: X_D3DTEXTURESTAGESTATETYPE;
begin
  EmuSwapFS(fsWindows);

  Type_VersionIndependent := DxbxFromOldVersion_D3DTSS(Type_);
  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetTextureStageStateNotInline >>').
      _(Stage, 'Stage').
      _(DWORD(Type_), 'Type').
      _(Type_VersionIndependent, 'Type_VersionIndependent').
      _(Value, 'Value').
    LogEnd();

  EmuSwapFS(fsXbox);

  case Type_VersionIndependent of
    X_D3DTSS_DEFERRED_FIRST..X_D3DTSS_DEFERRED_LAST:
      // TODO -oDxbx : Update the D3D DirtyFlags too
      XTL_EmuD3DDeferredTextureState[Stage, Ord(Type_)] := Value;
    X_D3DTSS_BUMPENVMAT00..X_D3DTSS_BUMPENVLOFFSET:
      XTL_EmuD3DDevice_SetTextureState_BumpEnv(Stage, Type_, Value);
    X_D3DTSS_TEXCOORDINDEX:
      XTL_EmuD3DDevice_SetTextureState_TexCoordIndex(Stage, Value);
    X_D3DTSS_BORDERCOLOR:
      XTL_EmuD3DDevice_SetTextureState_BorderColor(Stage, Value);
    X_D3DTSS_COLORKEYCOLOR:
      XTL_EmuD3DDevice_SetTextureState_ColorKeyColor(Stage, Value);
  end;

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_SetRenderTarget
(
  pRenderTarget: PX_D3DSurface;
  pNewZStencil: PX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pPCRenderTarget: XTL_PIDirect3DSurface8;
  pPCNewZStencil: XTL_PIDirect3DSurface8;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('EmuD3DDevice_SetRenderTarget').
       _(pRenderTarget, 'pRenderTarget').
       _(pNewZStencil, 'pNewZStencil').
    LogEnd();
  end;

  pPCRenderTarget := nil;
  pPCNewZStencil := nil;

  if (pRenderTarget <> nil) then
  begin
    if Assigned(pRenderTarget.Emu.Surface) then
    begin
      EmuVerifyResourceIsRegistered(pRenderTarget);
      pPCRenderTarget := pRenderTarget.Emu.Surface;
    end
    else
    begin
      pPCRenderTarget := g_pCachedRenderTarget.Emu.Surface;
    end;
  end;

  if (pNewZStencil <> nil) then
  begin
    if Assigned(pNewZStencil.Emu.Surface) then
    begin
      EmuVerifyResourceIsRegistered(pNewZStencil);
      pPCNewZStencil := pNewZStencil.Emu.Surface;
    end
    else
    begin
      pPCNewZStencil := g_pCachedZStencilSurface.Emu.Surface;
    end;
  end;

{$IFDEF DXBX_USE_D3D9}
  Result := g_pD3DDevice.SetRenderTarget({RenderTargetIndex=}0, IDirect3DSurface(pPCRenderTarget));
  g_pD3DDevice.SetDepthStencilSurface(IDirect3DSurface9(pPCNewZStencil));
{$ELSE}
  Result := g_pD3DDevice.SetRenderTarget(IDirect3DSurface(pPCRenderTarget), IDirect3DSurface(pPCNewZStencil));
  // This tries to fix VolumeFog on ATI :
  if FAILED(Result) then
  begin
    // TODO : Maybe some info : http://forums.create.msdn.com/forums/t/2124.aspx
    EmuWarning('SetRenderTarget Failed Trying ATI fix' +#13#10+ DxbxD3DErrorString(Result));
    Result := g_pD3DDevice.SetRenderTarget(IDirect3DSurface(pPCRenderTarget), nil);
  end;
{$ENDIF}

  if FAILED(Result) then
    if MayLog(lfUnit) then
      EmuWarning('SetRenderTarget failed!' +
                 ' pRenderTarget = 0x%.08X, pPCRenderTarget = 0x%.08X,' +
                 ' pNewZStencil = 0x%.08X, pPCNewZStencil = 0x%.08X,' +
                 ' hRet = 0x%.08X',
                 [pRenderTarget, pPCRenderTarget,
                  pNewZStencil, pPCNewZStencil,
                  Result]);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CreatePalette
(
  Size: X_D3DPALETTESIZE;
  ppPalette: PPX_D3DPalette
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  // Dxbx note : No EmuSwapFS needed here

  ppPalette^ := XTL_EmuD3DDevice_CreatePalette2(Size);
  Result := D3D_OK;
end;


function XTL_EmuD3DDevice_CreatePalette2
(
  Size: X_D3DPALETTESIZE
): PX_D3DPalette; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
const
  lk: array [D3DPALETTE_256..D3DPALETTE_32] of int = (
      256 * sizeof(D3DCOLOR),    // D3DPALETTE_256
      128 * sizeof(D3DCOLOR),    // D3DPALETTE_128
      64 * sizeof(D3DCOLOR),     // D3DPALETTE_64
      32 * sizeof(D3DCOLOR)      // D3DPALETTE_32
  );
var
  pPalette: PX_D3DPalette;
const
  RefCount = $8000BEEF;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CreatePalette2').
      _(Integer(Ord(Size)), 'Size').
    LogEnd();

  New({var PX_D3DPalette}pPalette);
  // ZeroMemory(pPalette,...) not nescessary, as we're filling each field already here :

  // Dxbx addition : Initialize Common field properly :
  pPalette.Common := (RefCount and X_D3DCOMMON_REFCOUNT_MASK) or X_D3DCOMMON_TYPE_PALETTE or X_D3DCOMMON_D3DCREATED;
  pPalette.Data := DWORD(XboxAlloc(lk[Size] * sizeof(uint08)));
  pPalette.Emu.Lock := RefCount; // emulated reference count for palettes

  g_pCurrentPalette := PBytes(pPalette.Data);

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuD3D8 : CreatePalette2: Successfully Created Palette : ' + ResourceToString(pPalette));

  EmuSwapFS(fsXbox);

  Result := pPalette;
end;

function XTL_EmuD3DDevice_SetPalette
(
  Stage: DWORD;
  pPalette: PX_D3DPalette
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetPalette').
      _(Stage, 'Stage').
      _(pPalette, 'pPalette').
    LogEnd();

  // Marked out by Cxbx
//  g_pD3DDevice.SetPaletteEntries(0, PPALETTEENTRY(@pPalette.Data));

  if Assigned(pPalette) then
    g_pCurrentPalette := PBytes(pPalette.Data);

  EmuWarning('Not setting palette');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_SetCopyRectsState
(
  pCopyRectState: PD3DCOPYRECTSTATE;
  pCopyRectRopState: PD3DCOPYRECTROPSTATE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetCopyRectsState').
      _(pCopyRectState, 'pCopyRectState').
      _(pCopyRectRopState, 'pCopyRectRopState').
    LogEnd();

//  D3DDevice_SetCopyRectsState(pCopyRectState, pCopyRectRopState);
  Unimplemented('EmuD3DDevice_SetCopyRectsState');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

procedure XTL_EmuD3DDevice_SetFlickerFilter
(
  Filter: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetFlickerFilter').
      _(Filter, 'Filter').
    LogEnd();

  EmuWarning('Not setting flicker filter');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetSoftDisplayFilter
(
  Enable: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetSoftDisplayFilter').
      _(Enable, 'Enable').
    LogEnd();

  EmuWarning('Not setting soft display filter');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DPalette_Lock
(
  pThis: PX_D3DPalette;
  ppColors: PPD3DCOLOR;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  // Dxbx note : No EmuSwapFS needed here

  ppColors^ := XTL_EmuD3DPalette_Lock2(pThis, Flags);
  (*DbgPrintf('Pallete 0x%.08X was locked: return = 0x%.08x', [
        pThis,
        ppColors^
        ]);*)

  Result := D3D_OK;
end;

function XTL_EmuD3DPalette_Lock2
(
  pThis: PX_D3DPalette;
  Flags: DWORD
): PD3DCOLOR; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DPalette_Lock').
      _(pThis, 'pThis').
      _(Flags, 'Flags').
    LogEnd();

  Result := PD3DCOLOR(pThis.Data);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DPalette_GetSize(
  pThis: PX_D3DPalette
): X_D3DPALETTESIZE;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DPalette_GetSize').
      _(pThis, 'pThis').
    LogEnd();

  Unimplemented('XTL_EmuD3DPalette_GetSize');

  //return D3DPalette_GetSize(pThis);
  Result := D3DPALETTE_32;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetVertexShaderSize
(
  Handle_: DWORD;
  pSize: PUINT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pD3DVertexShader: PX_D3DVertexShader;
  pVertexShader: PVERTEX_SHADER;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetVertexShaderSize').
      _(Handle_, 'Handle').
      _(pSize, 'pSize').
    LogEnd();

  if Assigned(pSize) then
  begin
    if VshHandleIsVertexShader(Handle_) then
    begin
      pD3DVertexShader := VshHandleGetVertexShader(Handle_);
      pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);
      pSize^ := pVertexShader.Size;
    end
    else
    begin
      pSize^ := 0;
    end;

    Result := D3D_OK;
  end
  else
    Result := D3DERR_INVALIDCALL;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_DeleteVertexShader
(
  Handle: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  RealHandle: DWORD;
  pD3DVertexShader: PX_D3DVertexShader;
  pVertexShader: PVERTEX_SHADER;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_DeleteVertexShader').
      _(Handle, 'Handle').
    LogEnd();

  Result := D3D_OK;

  if (VshHandleIsVertexShader(Handle)) then
  begin
    pD3DVertexShader := VshHandleGetVertexShader(Handle);
    pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);

    RealHandle := pVertexShader.Handle;
    XboxFree(pVertexShader.pDeclaration);

    if Assigned(pVertexShader.pFunction) then
    begin
      XboxFree(pVertexShader.pFunction);
    end;

    XTL_FreeVertexDynamicPatch(pVertexShader);

    XboxFree(pVertexShader);
    XboxFree(pD3DVertexShader);

{$IFDEF DXBX_USE_D3D9}
    IDirect3DVertexShader9(RealHandle)._Release; // TODO -oDxbx : Is this correctly done instead of DeleteVertexShader() ?
{$ELSE}
    Result := g_pD3DDevice.DeleteVertexShader(RealHandle);
{$ENDIF}
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SelectVertexShaderDirect
(
  pVAF: PX_VERTEXATTRIBUTEFORMAT;
  Address: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DDevice_SelectVertexShaderDirect').
      _(pVAF, 'pVAF').
      _(Address, 'Address').
    LogEnd();

  Unimplemented('EmuD3DDevice_SelectVertexShaderDirect');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_GetShaderConstantMode
(
  pMode: PDWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);

    LogBegin('EmuD3DDevice_GetShaderConstantMode').
      _(pMode, 'pMode').
    LogEnd();

    EmuSwapFS(fsXbox);
  end;

  if Assigned(pMode) then
    pMode^ := g_VertexShaderConstantMode;
end;

function XTL_EmuD3DDevice_GetVertexShader
(
  {CONST} pHandle: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:162  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetVertexShader').
      _(pHandle, 'pHandle').
    LogEnd();

  if  Assigned(pHandle)
  and VshHandleIsValidShader(g_CurrentVertexShader) then
  begin
    pHandle^ := g_CurrentVertexShader;
    Result := D3D_OK;
  end
  else
    Result := D3DERR_INVALIDCALL;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetVertexShaderConstant
(
  Register_: DWORD;
  pConstantData: Pvoid;
  ConstantCount: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DDevice_GetVertexShaderConstant').
      _(Register_, 'Register').
      _(pConstantData, 'pConstantData').
      _(ConstantCount, 'ConstantCount').
    LogEnd();

  // TODO -oDxbx: If we ever find a title that calls this, check if this correction
  // should indeed be done version-dependantly (like in SetVertexShaderConstant);
  // It seems logical that these two mirror eachother, but it could well be different:
  Inc(Register_, X_D3DSCM_CORRECTION_VersionDependent);
  if MayLog(lfUnit or lfTrace) then
    DbgPrintf('Corrected constant register : 0x%.08x', [Register_]);

{$IFDEF DXBX_USE_D3D9}
  Result := g_pD3DDevice.GetVertexShaderConstantF
{$ELSE}
  Result := g_pD3DDevice.GetVertexShaderConstant
{$ENDIF}
    (
    Register_,
{$IFDEF DXBX_USE_D3D9}
    PSingle(pConstantData),
{$ELSE}
    {out}pConstantData^,
{$ENDIF}
    ConstantCount
    );

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetVertexShaderInputDirect
(
  pVAF: PX_VERTEXATTRIBUTEFORMAT;
  StreamCount: UINT;
  pStreamInputs: PX_STREAMINPUT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DDevice_SelectVertexShaderDirect').
      _(pVAF, 'pVAF').
      _(StreamCount, 'StreamCount').
      _(pStreamInputs, 'pStreamInputs').
    LogEnd();

  Unimplemented('EmuD3DDevice_SetVertexShaderInputDirect');

  EmuSwapFS(fsXbox);
  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_GetVertexShaderInput
(
  pHandle: PDWORD;
  pStreamCount: PUINT;
  pStreamInputs: PX_STREAMINPUT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DDevice_GetVertexShaderInput').
      _(pHandle, 'pHandle').
      _(pStreamCount, 'pStreamCount').
      _(pStreamInputs, 'pStreamInputs').
    LogEnd();

  Unimplemented('EmuD3DDevice_GetVertexShaderInput');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_SetVertexShaderInput
(
  aHandle: DWORD;
  StreamCount: UINT;
  pStreamInputs: PX_STREAMINPUT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DDevice_SetVertexShaderInput').
      _(aHandle, 'aHandle').
      _(StreamCount, 'StreamCount').
      _(pStreamInputs, 'pStreamInputs').
    LogEnd();

  Unimplemented('EmuD3DDevice_SetVertexShaderInput');

  EmuSwapFS(fsXbox);
  Result := D3D_OK;
end;

procedure XTL_EmuD3DDevice_RunVertexStateShader
(
  Address: DWORD;
  {CONST} pData: PFLOAT
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DDevice_RunVertexStateShader').
      _(Address, 'Address').
      _(pData, 'pData').
    LogEnd();

  Unimplemented('EmuD3DDevice_RunVertexStateShader');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_LoadVertexShaderProgram
(
  {CONST} pFunction: PDWORD;
  Address: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DDevice_LoadVertexShaderProgram').
      _(pFunction, 'pFunction').
      _(Address, 'Address').
    LogEnd();

  Unimplemented('EmuD3DDevice_LoadVertexShaderProgram');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_Nop(): HRESULT;
// Branch:DXBX  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_Nop();');

  //D3DDevice_Nop();
  Unimplemented('EmuD3DDevice_Nop');

  EmuSwapFS(fsXbox);
  Result := D3D_OK;
end;

procedure XTL_EmuD3DDevice_GetVertexShaderType
(
  aHandle: DWORD;
  pType: PDWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DDevice_GetVertexShaderType').
      _(aHandle, 'aHandle').
      _(pType, 'pType').
    LogEnd();

  if Assigned(pType) and VshHandleIsVertexShader(aHandle) then
  begin
    pType^ := PVERTEX_SHADER(VshHandleGetVertexShader(aHandle).Handle).Type_;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetVertexShaderDeclaration
(
  Handle: DWORD;
  pData: PVOID;
  pSizeOfData: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pVertexShader: PVERTEX_SHADER;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DDevice_GetVertexShaderDeclaration').
      _(Handle, 'Handle').
      _(pData, 'pData').
      _(pSizeOfData, 'pSizeOfData').
    LogEnd();

  Result := D3DERR_INVALIDCALL;

  if Assigned(pSizeOfData) and VshHandleIsVertexShader(Handle) then
  begin
    pVertexShader := PVERTEX_SHADER(VshHandleGetVertexShader(Handle).Handle);
    if (pSizeOfData^ < pVertexShader.DeclarationSize) or (not Assigned(pData)) then
    begin
      pSizeOfData^ := pVertexShader.DeclarationSize;
      Result := ifThen(Assigned(pData), D3DERR_MOREDATA, D3D_OK);
    end
    else
    begin
      memcpy(pData, pVertexShader.pDeclaration, pVertexShader.DeclarationSize);
      Result := D3D_OK;
    end;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetVertexShaderFunction
(
  aHandle: DWORD;
  pData: PPVOID;
  pSizeOfData: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pVertexShader: PVERTEX_SHADER;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DDevice_GetVertexShaderFunction').
      _(aHandle, 'aHandle').
      _(pData, 'pData').
      _(pSizeOfData, 'pSizeOfData').
    LogEnd();

  Result := D3DERR_INVALIDCALL;

  if Assigned(pSizeOfData) and VshHandleIsVertexShader(aHandle) then
  begin
    pVertexShader := PVERTEX_SHADER(VshHandleGetVertexShader(aHandle).Handle);
    if (pSizeOfData^ < pVertexShader.FunctionSize) or (not Assigned(pData)) then
    begin
      pSizeOfData^ := pVertexShader.FunctionSize;
      Result := ifThen(Assigned(pData), D3DERR_MOREDATA, D3D_OK);
    end
    else
    begin
      memcpy(pData, pVertexShader.pFunction, pVertexShader.FunctionSize);
      Result := D3D_OK;
    end;
  end;

  EmuSwapFS(fsXbox);
end;

(* No need to patch, as our kernel implements MmAllocateContiguousMemory(Ex) already
function XTL_EmuDirect3D_AllocContiguousMemory
(
  dwSize: SIZE_T;
  dwAllocAttributes: DWORD
): PVOID; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Patrickvl  Done:100
*)

function XTL_EmuD3DTexture_GetLevelDesc
(
  pThis: PX_D3DTexture;
  Level: UINT;
  pDesc: PX_D3DSURFACE_DESC
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  SurfaceDesc: D3DSURFACE_DESC;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuD3DTexture_GetLevelDesc').
      _(pThis, 'pThis').
      _(Level, 'Level').
      _(pDesc, 'pDesc').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);
  Result := IDirect3DTexture(pThis.Emu.Texture).GetLevelDesc(Level, {out}SurfaceDesc);

  if (FAILED(Result)) then
    EmuWarning('EmuD3DTexture_GetLevelDesc Failed!' +#13#10+ DxbxD3DErrorString(Result))
  else
  begin
    // rearrange into xbox format (remove D3DPOOL)
    EmuPC2XB_D3DSURFACE_DESC(SurfaceDesc, pDesc, 'EmuIDirect3DSurface_GetDesc');
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuDirect3D_CheckDeviceMultiSampleType
(
  Adapter: UINT;
  DeviceType: D3DDEVTYPE;
  SurfaceFormat: X_D3DFORMAT;
  Windowed: BOOL;
  MultiSampleType: X_D3DMULTISAMPLE_TYPE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  PCSurfaceFormat: D3DFORMAT;
  PCMultiSampleType: D3DMULTISAMPLE_TYPE;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3D_CheckDeviceMultiSampleType').
      _(Adapter, 'Adapter').
      _(DeviceType, 'DeviceType').
      _(SurfaceFormat, 'SurfaceFormat').
      _(Windowed, 'Windowed').
      _(MultiSampleType, 'MultiSampleType').
    LogEnd();

  if (Adapter <> D3DADAPTER_DEFAULT) then
  begin
    EmuWarning('Adapter is not D3DADAPTER_DEFAULT, correcting!');
    Adapter := D3DADAPTER_DEFAULT;
  end;

  if Ord(DeviceType) > Ord(High(D3DDEVTYPE)) then
    EmuWarning('DeviceType := D3DDEVTYPE_FORCE_DWORD');

  // Convert SurfaceFormat (Xbox->PC)
  PCSurfaceFormat := DxbxXB2PC_D3DFormat(SurfaceFormat, D3DRTYPE_SURFACE);

  if (Windowed <> BOOL_FALSE) then
    Windowed := BOOL_FALSE;

  // Convert MultiSampleType from Xbox to PC :
  PCMultiSampleType := EmuXB2PC_D3DMULTISAMPLE_TYPE(MultiSampleType);

  // Now call the real CheckDeviceMultiSampleType with the corrected parameters.
  Result := g_pD3D.CheckDeviceMultiSampleType
    (
    Adapter,
    DeviceType,
    PCSurfaceFormat,
    Windowed <> BOOL_FALSE,
    PCMultiSampleType
{$IFDEF DXBX_USE_D3D9}
    , {pQualityLevels=}nil
{$ENDIF}
    );

  EmuSwapFS(fsXbox);
end; // XTL_EmuDirect3D_CheckDeviceMultiSampleType

(* Too high level : No patch needed, just copies g_DeviceCaps :
function XTL_EmuDirect3D_GetDeviceCaps
(
  Adapter: UINT;
  DeviceType: D3DDEVTYPE;
  pCaps: PD3DCAPS
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

function XTL_EmuDirect3D_SetPushBufferSize
(
  PushBufferSize: DWORD;
  KickOffSize: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3D_SetPushBufferSize').
      _(PushBufferSize, 'PushBufferSize').
      _(KickOffSize, 'KickOffSize').
    LogEnd();

  Result := D3D_OK; // This is a Xbox extension, meaning there is no pc counterpart.

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_InsertFence(): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_InsertFence();');

  // TODO -oCXBX: Actually implement this
  Result := $8000BEEF;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_IsFencePending
(
  Fence: DWORD
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_IsFencePending').
      _(Fence, 'Fence').
    LogEnd();

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);

  Result := BOOL_FALSE;
end;

procedure XTL_EmuD3DDevice_BlockOnFence
(
  Fence: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_BlockOnFence').
      _(Fence, 'Fence').
    LogEnd();

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DResource_BlockUntilNotBusy
(
  pThis: PX_D3DResource
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DResource_BlockUntilNotBusy').
      _(pThis, 'pThis').
    LogEnd();

  // TODO -oCXBX: Implement
//  while g_bIsBusy do Sleep(1); //??

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DResource_SetPrivateData
(
  pThis: PX_D3DResource;
  refguid: REFGUID;
  pData: Pvoid;
  SizeOfData: DWORD;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DResource_SetPrivateData').
      _(pThis, 'pThis').
      _(refguid, 'refguid').
      _(pData, 'pData').
      _(SizeOfData, 'SizeOfData').
      _(Flags, 'Flags').
    LogEnd();

  if IsSpecialResource(pThis.Data) then
    // TODO -oDxbx : How should we support this on special resources?
    EmuWarning('SetPrivateData not supported on special resources!')
  else
    IDirect3DResource(pThis.Emu.Resource).SetPrivateData(refGuid^, pData, SizeOfData, Flags);

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DResource_GetPrivateData
(
  pThis: PX_D3DResource;
  refguid: REFGUID;
  pData: Pvoid;
  PSizeOfData: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DResource_GetPrivateData').
      _(pThis, 'pThis').
      _(refguid, 'refguid').
      _(pData, 'pData').
      _(PSizeOfData, 'PSizeOfData').
    LogEnd();

  if IsSpecialResource(pThis.Data) then
    // TODO -oDxbx : How should we support this on special resources?
    EmuWarning('GetPrivateData not supported on special resources!')
  else
    IDirect3DResource(pThis.Emu.Resource).GetPrivateData(refguid^, pData, pSizeOfData^);

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DResource_FreePrivateData
(
  pThis: PX_D3DResource;
  refguid: REFGUID
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DResource_FreePrivateData').
      _(pThis, 'pThis').
      _(refguid, 'refguid').
    LogEnd();

  if IsSpecialResource(pThis.Data) then
    // TODO -oDxbx : How should we support this on special resources?
    EmuWarning('FreePrivateData not supported on special resources!')
  else
    IDirect3DResource(pThis.Emu.Resource).FreePrivateData(refguid^);

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DVertexBuffer_GetDesc
(
  pThis: PX_D3DVertexBuffer;
  pDesc: PD3DVERTEXBUFFER_DESC
): HResult; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DVertexBuffer_GetDesc').
      _(pThis, 'pThis').
      _(pDesc, 'pDesc').
    LogEnd();

  Result := IDirect3DVertexBuffer(pThis.Emu.VertexBuffer).GetDesc({out}pDesc^);

  // Dxbx addition : Convert Format (PC->Xbox, in-place)
  X_D3DFORMAT(pDesc.Format) := EmuPC2XB_D3DFormat(pDesc.Format);
//  pDesc.Type_ := X_D3DRESOURCETYPE(pDesc._Type);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetScissors
(
  Count: DWORD;
  Exclusive: BOOL;
  {CONST} pRects: PD3DRECT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetScissors').
      _(Count, 'Count').
      _(Exclusive, 'Exclusive').
      _(pRects, 'pRects').
    LogEnd();

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_SetScreenSpaceOffset
(
  x: FLOAT;
  y: FLOAT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetScreenSpaceOffset').
      _(x, 'x').
      _(y, 'y').
    LogEnd();

  EmuWarning('EmuD3DDevice_SetScreenSpaceOffset ignored');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;


{$IFDEF DXBX_PIXELSHADER_HOOKS}
function XTL_EmuD3DDevice_SetPixelShaderProgram
(
  {CONST} pPSDef: PX_D3DPIXELSHADERDEF
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwHandle: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetPixelShaderProgram >>').
      _(pPSDef, 'pPSDef').
    LogEnd();

  EmuSwapFS(fsXbox);

  // Redirect the creation and activation to the other patches we already have :
  dwHandle := 0;
  Result := XTL_EmuD3DDevice_CreatePixelShader(pPSDef, @dwHandle);
  if (FAILED(Result)) then
    dwHandle := X_PIXELSHADER_FAKE_HANDLE;

  Result := XTL_EmuD3DDevice_SetPixelShader(dwHandle);
end;
{$ENDIF DXBX_PIXELSHADER_HOOKS}

function XTL_EmuD3DDevice_CreateStateBlock
(
  Type_: D3DSTATEBLOCKTYPE;
  pToken: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_CreateStateBlock').
      _(Ord(Type_), 'Type').
      _(pToken, 'pToken').
    LogEnd();

  // blueshogun96 10/1/07
  // I'm assuming this is the same as the PC version...

{$IFDEF DXBX_USE_D3D9}
  Result := g_pD3DDevice.CreateStateBlock(Type_, PIDirect3DStateBlock9(pToken));
{$ELSE}
  Result := g_pD3DDevice.CreateStateBlock(Type_, {out}pToken^);
{$ENDIF}

  if (FAILED(Result)) then
    EmuWarning('CreateStateBlock failed!' +#13#10+ DxbxD3DErrorString(Result));

  EmuSwapFS(fsXbox);
end;


procedure XTL_EmuD3DDevice_InsertCallback
(
  Type_: X_D3DCALLBACKTYPE;
  pCallback: X_D3DCALLBACK;
  Context: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_InsertCallback').
      _(Ord(Type_), 'Type').
      _(pCallback, 'pCallback').
      _(Context, 'Context').
    LogEnd();

  EmuWarning('InsertCallback ignored!');

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);
end;


function XTL_EmuD3DDevice_DrawRectPatch
(
  Handle: UINT;
  {CONST} pNumSegs: PFLOAT;
  {CONST} pRectPatchInfo: PD3DRECTPATCH_INFO
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_DrawRectPatch').
      _(Handle, 'Handle').
      _(pNumSegs^, 'pNumSegs').
      _(pRectPatchInfo, 'pRectPatchInfo').
    LogEnd();

  XTL_EmuUpdateDeferredStates();

{$IFDEF DXBX_ENABLE_P8_CONVERSION}
  XTL_EmuUpdateActiveTexture();
{$ENDIF}

  Result := g_pD3DDevice.DrawRectPatch(Handle, PSingle(pNumSegs), pRectPatchInfo);

  if (FAILED(Result)) then
    EmuWarning('DrawRectPatch failed!' +#13#10+ DxbxD3DErrorString(Result));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_DrawTriPatch
(
  Handle: UINT;
  {CONST} pNumSegs: PFLOAT;
  {CONST} pTriPatchInfo: PD3DTRIPATCH_INFO
): HRESULT; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_DrawTriPatch').
      _(Handle, 'Handle').
      _(pNumSegs^, 'pNumSegs').
      _(pTriPatchInfo, 'pTriPatchInfo').
    LogEnd();

  XTL_EmuUpdateDeferredStates();

{$IFDEF DXBX_ENABLE_P8_CONVERSION}
  XTL_EmuUpdateActiveTexture();
{$ENDIF}

  Result := g_pD3DDevice.DrawTriPatch(Handle, PSingle(pNumSegs), pTriPatchInfo);
  if (FAILED(Result)) then
    EmuWarning('DrawTriPatch failed!' +#13#10+ DxbxD3DErrorString(Result));

  EmuSwapFS(fsXbox);
end;

//#pragma warning(disable:4244)
function XTL_EmuD3DDevice_GetProjectionViewportMatrix
(
  pProjectionViewport: PD3DXMATRIX
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  hRet: HRESULT;
  mtxProjection: D3DMATRIX;
  mtxViewport: D3DMATRIX;
  Viewport: D3DVIEWPORT;
  ClipWidth, ClipHeight, ClipX, ClipY, Width, Height: Float;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetProjectionViewportMatrix').
      _(pProjectionViewport, 'pProjectionViewport').
    LogEnd();

  // blueshogun96 1/25/10
  // It's been almost 3 years, but I think this is a better
  // implementation.  Still probably not right, but better
  // then before.

  if Assigned(pProjectionViewport) then
  begin
    // Get current viewport
    hRet := g_pD3DDevice.GetViewport({out}Viewport);
    if (FAILED(hRet)) then
      EmuWarning('Unable to get viewport!' +#13#10+ DxbxD3DErrorString(hRet));

    // Get current projection matrix
    hRet := g_pD3DDevice.GetTransform(D3DTS_PROJECTION, {out}mtxProjection);
    if (FAILED(hRet)) then
      EmuWarning('Unable to get projection matrix!' +#13#10+ DxbxD3DErrorString(hRet));

    // Create the Viewport matrix manually
    // Direct3D8 doesn't give me everything I need in a viewport structure
    // (one thing I REALLY HATE!) so some constants will have to be used
    // instead.

    ClipWidth := 2.0;
    ClipHeight := 2.0;
    ClipX := -1.0;
    ClipY := 1.0;
    Width := DWtoF(Viewport.Width);
    Height := DWtoF(Viewport.Height);

    D3DXMatrixIdentity({out}mtxViewport);
    mtxViewport._11 := Width / ClipWidth;
    mtxViewport._22 := -(Height / ClipHeight);
    mtxViewport._41 := -(ClipX * mtxViewport._11);
    mtxViewport._42 := -(ClipY * mtxViewport._22);

    // Multiply projection and viewport matrix together
    pProjectionViewport^ := D3DMATRIX_MULTIPLY(mtxProjection, mtxViewport);
  end;

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;
//#pragma warning(default:4244)

function XTL_EmuD3DDevice_GetPushDistance
(
  Handle_: DWORD
): Dword;
begin
  EmuSwapFs(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetPushDistance').
      _(Handle_, 'Handle').
    LogEnd();

  DxbxKrnlCleanup('XTL_EmuD3DDevice_GetPushDistance is not implemented');

  Result := D3D_OK;

  EmuSwapFs(fsXbox);
end;

function XTL_EmuD3DDevice_BackFillMode
(
  Value: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_BackFillMode').
      _(Value, 'Value').
    LogEnd();

  // blueshogun96 12/4/07
  // I haven't had access to Cxbx sources in a few months, great to be back :)
  //
  // Anyway, since standard Direct3D doesn't support the back fill mode
  // operation, this function will be ignored.  Things like this make me
  // think even more that an OpenGL port wouldn't hurt since OpenGL supports
  // nearly all of the missing features that Direct3D lacks.  The Xbox's version
  // of Direct3D was specifically created to take advantage of certain NVIDIA
  // GPU registers and provide more OpenGL-like features IHMO.

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;


// * func: EmuD3DDevice_KickOff (D3D::CDevice::KickOff)
procedure XTL_EmuD3D_CDevice_KickOff(); stdcall;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : D3D_CDevice_KickOff();');

  // TODO -oCXBX: Anything (kick off and NOT wait for idle)?
  // NOTE: We should actually emulate D3DDevice_KickPushBuffer()
  // instead of this function.  When needed, use the breakpoint (int 3)
  // to determine what is calling this function if it's something other
  // than D3DDevice_KickPushBuffer() itself.


  EmuSwapFS(fsXbox);
end;


function XTL_EmuD3DDevice_GetTexture
(
  dwStage: DWORD;
  ppTexture: PPX_D3DBaseTexture
): HRESULT; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetTexture').
      _(dwStage, 'dwStage').
      _(ppTexture, 'ppTexture').
    LogEnd();

  Result := D3D_OK;

  if Assigned(ppTexture) and (dwStage < X_D3DTS_STAGECOUNT) then
  begin
    ppTexture^ := g_EmuD3DActiveTexture[dwStage];
    // According to the XDK documentation, this function should also add
    // to the reference count.
    if Assigned(ppTexture^) then
    begin
      EmuSwapFS(fsXbox);
      XTL_EmuD3DResource_AddRef(ppTexture^);
      EmuSwapFS(fsWindows);
    end;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetTexture2
(
  dwStage: DWORD
): PX_D3DResource; stdcall;
// Branch:Dxbx  Translator:PatrickvL
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetTexture2 >> ').
      _(dwStage, 'Stage').
    LogEnd();

  EmuSwapFS(fsXbox);

  XTL_EmuD3DDevice_GetTexture(dwStage, @Result);
end;


// * func: EmuD3DDevice_SetStateVB (D3D::CDevice::SetStateVB)
procedure XTL_EmuD3DDevice_SetStateVB(Unknown1: ULONG); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetStateVB').
      _(Unknown1, 'Unknown1').
    LogEnd();

  // TODO -oCXBX: Anything?

  EmuSwapFS(fsXbox);
end;

// * func: EmuD3DDevice_SetStateUP (D3D::CDevice::SetStateUP)
procedure XTL_EmuD3DDevice_SetStateUP(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_SetStateUP();');

  // TODO -oCXBX: Anything?

  EmuSwapFS(fsXbox);
end;


procedure XTL_EmuD3DDevice_SetStipple(
  pPattern: PDWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetStipple').
      _(pPattern, 'pPattern').
    LogEnd();

  // We need an OpenGL port... badly

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetSwapCallback
(
  pCallback: D3DSWAPCALLBACK
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('EmuD3DDevice_SetSwapCallback').
      _(PPointer(@pCallback)^, 'pCallback').
    LogEnd();
  end;

  g_pSwapCallback := pCallback;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_PersistDisplay(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_PersistDisplay();');

  Result := D3D_OK;

{$IFDEF GAME_HACKS_ENABLED}
  // TODO -oCXBX: If this functionality is ever really needed, an idea for
  // implementation would be to save a copy of the backbuffer's contents
  // and free the memory after the next call to D3DDevice::Present().
  // This temporary data could also be made available to the Xbox game
  // through AvGetSavedDataAddress() since D3DDevice::GetPersistedDisplay2
  // just contains a call to that kernel function.  So far, Unreal Champ-
  // ionship is the only game that uses this functionality that I know of.
  // Other Unreal Engine 2.x games might as well.
{$ENDIF}

  EmuWarning('(Temporarily) Not persisting display. Blueshogun can fix this.');

  if not Assigned(g_pD3DDevice) then
  begin
    EmuWarning('Direct3D device not initialized!');
    Result := E_FAIL;
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_Unknown1(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_Unknown1();');

  // TODO -oCXBX: Find out what this actually is.
  // This function was only found in Run Like Hell (5233) @ 0x11FCD0.
  // So far, this function hasn't been found in any other XDKs.  Since
  // the only major thing going on inside of it is a call to the kernel
  // function AvSendTVEncoderOption, we can probably ignore it.

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_PrimeVertexCache
(
  VertexCount: UINT;
  pIndexData: PWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_PrimeVertexCache').
      _(VertexCount, 'VertexCount').
      _(pIndexData, 'pIndexData').
    LogEnd();

  // TODO -oCXBX: Implement
  EmuWarning('PrimeVertexCache is not supported!');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_SetRenderState_SampleAlpha
(
  Value: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_SampleAlpha').
      _(Value, 'Value').
    LogEnd();

  XTL_EmuD3DDevice_SetRenderState_Simple_Internal(X_D3DRS_SAMPLEALPHA, Value);

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

(* Dxbx note : Disabled, as we DO have EmuD3DDeferredRenderState pin-pointed correctly
procedure XTL_EmuD3DDevice_SetRenderState_Deferred(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Value: DWORD;
  {1 ECX}State: DWORD
  ); register; // fastcall simulation - See Translation guide
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
*)

function XTL_EmuD3DDevice_DeleteStateBlock
(
  Token: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_DeleteStateBlock').
      _(Token, 'Token').
    LogEnd();

{$IFDEF DXBX_USE_D3D9}
  Result := IDirect3DStateBlock9(Token)._Release;
{$ELSE}
  Result := g_pD3DDevice.DeleteStateBlock(Token);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetModelView
(
  {CONST} pModelView: PD3DMATRIX;
  {CONST} pInverseModelView: PD3DMATRIX;
  {CONST} pComposite: PD3DMATRIX
): HRESULT; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetModelView').
      _(pModelView, 'pModelView').
      _(pInverseModelView, 'pInverseModelView').
      _(pComposite, 'pComposite').
    LogEnd();

  // TODO -oCxbx: Implement
  EmuWarning('SetModelView not yet implemented (should be easy fix, tell blueshogun)');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

(* Too high level : No patch needed, just sets g_OverscanColor@D3D@@3KA :
procedure XTL_EmuD3DDevice_SetOverscanColor(
  Color: D3DCOLOR
); stdcall;
*)

function XTL_EmuD3DDevice_SetVertexBlendModelView
(
  Count: UINT;
  pModelViews: PD3DMATRIX;
  pInverseModelViews: PD3DMATRIX;
  pProjectionViewport: PD3DMATRIX
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XTL_EmuD3DDevice_SetVertexBlendModelView');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;


procedure XTL_EmuD3DDevice_FlushVertexCache(); stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_FlushVertexCache();');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_BeginPushBuffer
(
   pPushBuffer: PX_D3DPushBuffer
): HRESULT; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_BeginPushBuffer').
      _(pPushBuffer, 'pPushBuffer').
    LogEnd();

  Unimplemented('EmuD3DDevice_BeginPushBuffer');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_EndPushBuffer(): HRESULT; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_EndPushBuffer();');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

(*procedure XTL_EmuXMETAL_StartPush
(
  Unknown: Pvoid
); stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXMETAL_StartPush').
      _(Unknown, 'Unknown').
    LogEnd();

  EmuSwapFS(fsXbox);
end;*)

function XTL_EmuD3DDevice_GetLight
(
  Index: DWORD;
  pLight: PD3DLIGHT
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetLight').
      _(Index, 'Index').
      _(pLight, 'pLight').
    LogEnd();

  g_pD3DDevice.GetLight(Index, pLight^);
  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetLightEnable
(
  Index: DWORD;
  pEnable: PBOOL
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetLightEnable').
      _(Index, 'Index').
      _(pEnable, 'Enable').
    LogEnd();

  g_pD3DDevice.GetLightEnable(Index, pEnable^);
  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetMaterial(pMaterial : PD3DMaterial): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetMaterial').
      _(pMaterial, 'pMaterial').
    LogEnd();

  g_pD3DDevice.GetMaterial(pMaterial^);
  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetModelView
(
  pModelView: PD3DXMATRIX
): HRESULT; stdcall;
// Branch:shogun  Revision:161  Translator:PatrickvL  Done:100
var
  Out: D3DMATRIX;
  mtxWorld, mtxView: D3DMATRIX;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetModelView').
      _(pModelView, 'pModelView').
    LogEnd();

  // Dxbx note : Blueshogun remarked "I hope this is right". When Dxbx is running,
  // Rayman Arena it crashes after logging this (when going from the menu to ingame).
  // TODO -oDxbx : Find out if the crash occurs in here, or after this call (and fix it!)

  Result := g_pD3DDevice.GetTransform(D3DTS_WORLD, {out}mtxWorld);
  if (FAILED(Result)) then
    EmuWarning('Unable to get projection matrix!' +#13#10+ DxbxD3DErrorString(Result));

  Result := g_pD3DDevice.GetTransform(D3DTS_VIEW, {out}mtxView);
  if (FAILED(Result)) then
    EmuWarning('Unable to get projection matrix!' +#13#10+ DxbxD3DErrorString(Result));

  // Clear the destination matrix
  ZeroMemory(@Out, sizeof(D3DMATRIX));

(*
  D3DXMatrixIdentity({out}mtxViewport);
  mtxViewport._11 := Width / ClipWidth;
  mtxViewport._22 := -(Height / ClipHeight);
  mtxViewport._41 := -(ClipX * mtxViewport._11);
  mtxViewport._42 := -(ClipY * mtxViewport._22);
*)

  // Multiply world and view matrix together
  Out := D3DMATRIX_MULTIPLY(mtxWorld, mtxView);

  if Assigned(pModelView) then
    pModelView^ := Out;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetBackMaterial
(
  pMaterial: PD3DMaterial
): HRESULT; stdcall;
// Branch:shogun  Revision:161  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetBackMaterial').
      _(pMaterial, 'pMaterial').
    LogEnd();

  EmuWarning('SetBackMaterial is not supported!');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;


// -- NEW METHODS

function XTL_EmuD3DDevice_GetRasterStatus
(
  pRasterStatus: PD3DRASTER_STATUS
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetRasterStatus').
      _(pRasterStatus, 'pRasterStatus').
    LogEnd();

  Result := g_pD3DDevice.GetRasterStatus({$IFDEF DXBX_USE_D3D9}{iSwapChain=}0,{$ENDIF} {out}pRasterStatus^);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DCubeTexture_GetLevelDesc
(
  pThis: PX_D3DCubeTexture;
  Level: UINT;
  pDesc: PX_D3DSURFACE_DESC
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
var
  SurfaceDesc: D3DSURFACE_DESC;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('XTL_EmuD3DCubeTexture_GetLevelDesc').
      _(pThis, 'pThis').
      _(Level, 'Level').
      _(pDesc, 'pDesc').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);
  Result := IDirect3DCubeTexture(pThis.Emu.CubeTexture).GetLevelDesc(Level, {out}SurfaceDesc);

  if (FAILED(Result)) then
    EmuWarning('EmuD3DCubeTexture_GetLevelDesc Failed!' +#13#10+ DxbxD3DErrorString(Result))
  else
  begin
    // rearrange into xbox format (remove D3DPOOL)
    EmuPC2XB_D3DSURFACE_DESC(SurfaceDesc, pDesc, 'D3DCubeTexture_GetLevelDesc');
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DCubeTexture_GetCubeMapSurface2
(
  pThis: PX_D3DCubeTexture;
  FaceType: D3DCUBEMAP_FACES;
  Level: UINT
): PX_D3DSurface; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DCubeTexture_GetCubeMapSurface2').
      _(pThis, 'pThis').
      _(Ord(FaceType), 'FaceType').
      _(Level, 'Level').
    LogEnd();

  New(Result); // TODO -oDxbx : When should this be freeed? Isn't this a memory leak otherwise?
  ZeroMemory(Result, SizeOf(Result^));

  IDirect3DCubeTexture(pThis.Emu.CubeTexture).GetCubeMapSurface(FaceType, Level, PIDirect3DSurface(@Result.Emu.Surface));

  // Dxbx addition : Initialize Common field properly :
  Result.Common := ({RefCount=}1 and X_D3DCOMMON_REFCOUNT_MASK) or X_D3DCOMMON_TYPE_SURFACE;
  // TODO -oDxbx : Set other fields too, like Format

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DVolumeTexture_GetLevelDesc
(
  pThis: PX_D3DVolumeTexture;
  Level: UINT;
  pDesc: PX_D3DVOLUME_DESC
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:100
var
  VolumeDesc: TD3DVolumeDesc;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DVolumeTexture_GetLevelDesc').
      _(pThis, 'pThis').
      _(Level, 'Level').
      _(pDesc, 'pDesc').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);

  Result := IDirect3DVolumeTexture(pThis.Emu.VolumeTexture).GetLevelDesc(Level, {out}VolumeDesc);

  if (FAILED(Result)) then
    EmuWarning('EmuD3DVolumeTexture_GetLevelDesc Failed!' +#13#10+ DxbxD3DErrorString(Result))
  else
    // rearrange into xbox format (remove D3DPOOL)
    EmuPC2XB_D3DVOLUME_DESC(VolumeDesc, pDesc, 'D3DVolume_GetDesc');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DVolumeTexture_GetVolumeLevel2
(
  pThis: PX_D3DVolumeTexture;
  Level: UINT;
  ppVolumeLevel: PPX_D3DVolume
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DVolumeTexture_GetVolumeLevel2').
      _(pThis, 'pThis').
      _(Level, 'Level').
      _(ppVolumeLevel, 'ppVolumeLevel').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);
  Result := IDirect3DVolumeTexture(pThis.Emu.VolumeTexture).GetVolumeLevel(Level, PIDirect3DVolume(ppVolumeLevel));

  if (FAILED(Result)) then
    EmuWarning('GetVolumeLevel2 Failed!' +#13#10+ DxbxD3DErrorString(Result));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DVolume_GetDesc
(
  pThis: PX_D3DVolume;
  pDesc: PX_D3DVOLUME_DESC
): HRESULT; stdcall;
// Branch:DXBX  Translator:PatrickvL  Done:100
var
  pPCVolume: XTL_PIDirect3DVolume8;
  VolumeDesc: TD3DVolumeDesc;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DVolume_GetDesc').
      _(pThis, 'pThis').
      _(pDesc, 'pDesc').
    LogEnd();

//  EmuVerifyResourceIsRegistered(pThis);

  if IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0) then
  begin
    pDesc.Format := X_D3DFMT_YUY2; // = EmuPC2XB_D3DFormat(D3DFMT_YUY2);
    pDesc.Type_ := X_D3DRTYPE_VOLUME;
    pDesc.Usage := 0;
    pDesc.Size := g_dwOverlayP * g_dwOverlayH;
    pDesc.Height := g_dwOverlayH;
    pDesc.Width := g_dwOverlayW;
    pDesc.Depth := 4;

    Result := D3D_OK;
  end
  else
  begin
    pPCVolume := pThis.Emu.Volume;

    ZeroMemory(@VolumeDesc, sizeof(VolumeDesc));
    Result := IDirect3DVolume(pPCVolume).GetDesc({out}VolumeDesc);

    if SUCCEEDED(Result) then
      // rearrange into xbox format (remove D3DPOOL)
      EmuPC2XB_D3DVOLUME_DESC(VolumeDesc, pDesc, 'D3DVolume_GetDesc');
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DVolume_GetContainer2
(
  pThis: PX_D3DVolume;
  ppBaseTexture: PPX_D3DBaseTexture
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DVolume_GetContainer2').
      _(pThis, 'pThis').
      _(ppBaseTexture, 'ppBaseTexture').
    LogEnd();

  Result := IDirect3DVolume(pThis.Emu.VolumeTexture).GetContainer({IID_}IDirect3DTexture, {var}Pointer(ppBaseTexture));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DVolume_LockBox
(
  pThis: PX_D3DVolume;
  pLockedVolume: PD3DLOCKED_BOX;
  {CONST}pBox: PD3DBOX;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:DXBX  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIDirect3DVolume_LockBox').
      _(pThis, 'pThis').
      _(pLockedVolume, 'pLockedVolume').
      _(pBox, 'pBox').
      _(Flags, 'Flags').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);
  Result := IDirect3DVolume(pThis.Emu.Volume).LockBox({out}pLockedVolume^, pBox, Flags);
  if (FAILED(Result)) then
    EmuWarning('LockBox Failed!' +#13#10+ DxbxD3DErrorString(Result));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CreateSurface
(
  pDDSurfaceDesc: PX_D3DSURFACE_DESC;
  {out}lplpDDSurface: XTL_PIDirectDrawSurface7;
  pUnkOuter: IUnknown
): HRESULT; stdcall;
// Branch:DXBX  Translator:PatrickvL  Done:1
//var
//  SurfaceDesc: TDDSurfaceDesc2;
begin
  EmuSwapFS(fsWindows);

//  if MayLog(lfUnit) then
//    LogBegin('EmuD3DDevice_CreateSurface').
//      _(pDDSurfaceDesc, 'pDDSurfaceDesc').
//      _(lplpDDSurface, 'lplpDDSurface').
//      _(Pointer(pUnkOuter), 'pUnkOuter').
//    LogEnd();
//
//  EmuXB2PC_D3DSURFACE_DESC(pDDSurfaceDesc, @SurfaceDesc, 'EmuD3DDevice_CreateSurface');
//
//  Result := IDirectDraw7(g_pDD7).CreateSurface(SurfaceDesc, lplpDDSurface, pUnkOuter);

 Result := Unimplemented('XTL_EmuD3DDevice_CreateSurface');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_CreateSurface2(
  Width: DWORD;
  Height: DWORD;
  Usage: DWORD;
  Format: X_D3DFORMAT
): PX_D3DSurface; stdcall;
// Branch:DXBX  Translator:PatrickvL  Done:50
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('XTL_EmuD3DDevice_CreateSurface2 >>').
      _(Width, 'Width').
      _(Height, 'Height').
      _(Usage, 'Usage').
      _(Format, 'Format').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  Result := NULL;

  // TODO : What about Usage?
  {ignore}XTL_EmuD3DDevice_CreateImageSurface(Width, Height, Format, @Result); // Dxbx addition
end;

function XTL_EmuD3DSurface_GetContainer2
(
  pThis: PX_D3DSurface;
  ppBaseTexture: PPX_D3DBaseTexture
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('XTL_EmuD3DSurface_GetContainer2').
      _(pThis, 'pThis').
      _(ppBaseTexture, 'ppBaseTexture').
    LogEnd();

  EmuVerifyResourceIsRegistered(pThis);
  Result := IDirect3DVolume(pThis.Emu.Surface).GetContainer({IID_}IDirect3DTexture, {var}Pointer(ppBaseTexture));
  if (FAILED(Result)) then
    EmuWarning('GetVolumeLevel2 Failed!' +#13#10+ DxbxD3DErrorString(Result));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DPushBuffer_SetModelView
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset:  DWORD;
  {CONST}pModelView: PD3DMATRIX;
  {CONST}pInverseModelView: PD3DMATRIX;
  {CONST}pComposite: PD3DMATRIX
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

(*  D3DPushBuffer_SetModelView(pPushBuffer, Offset, pModelView, pInverseModelView, pComposite);
  return D3D_OK; *)

  Unimplemented('XTL_EmuD3DPushBuffer_SetModelView');

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DPushBuffer_SetVertexBlendModelView
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset: DWORD;
  Count: UINT;
  {CONST}pModelViews: PD3DMATRIX;
  {CONST}pInverseModelViews: PD3DMATRIX;
  {CONST}pProjectionViewport: PD3DMATRIX
): HRESULT; stdcall
// Branch:DXBX  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

(*  D3DPushBuffer_SetVertexBlendModelView(pPushBuffer, Offset, Count, pModelViews, pInverseModelViews, pProjectionViewport);
  return D3D_OK; *)

  Unimplemented('XTL_EmuD3DPushBuffer_SetVertexBlendModelView');

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DPushBuffer_SetVertexShaderInputDirect
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset: DWORD;
  Count: UINT;
  {CONST}pModelViews: PD3DMATRIX;
  {CONST}pInverseModelViews: PD3DMATRIX;
  {CONST}pProjectionViewport: PD3DMATRIX
): HRESULT; stdcall
// Branch:DXBX  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('XTL_EmuD3DPushBuffer_SetVertexShaderInputDirect');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DPushBuffer_SetPalette
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset: DWORD;
  Stage: DWORD;
  pPalette: PX_D3DPalette
): HRESULT; stdcall
// Branch:DXBX  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

(*  D3DPushBuffer_SetPalette(pPushBuffer, Offset, Stage, pPalette);
  return D3D_OK; *)

  Unimplemented('XTL_EmuD3DPushBuffer_SetPalette');

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DPushBuffer_SetVertexShaderConstant
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset:  DWORD;
  _Register: INT;
  {CONST}pConstantData: Pvoid;
  ConstantCount: DWORD
): HRESULT; stdcall
// Branch:DXBX  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

(*D3DPushBuffer_SetVertexShaderConstant(pPushBuffer, Offset, Register, pConstantData, ConstantCount);
  return D3D_OK; *)

  Unimplemented('XTL_EmuD3DPushBuffer_SetVertexShaderConstant');

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DPushBuffer_SetRenderState
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset: DWORD;
  State: X_D3DRENDERSTATETYPE;
  Value: DWORD
): HRESULT; stdcall
// Branch:DXBX  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('XTL_EmuD3DPushBuffer_SetRenderState');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DPushBuffer_CopyRects
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset: DWORD;
  pSourceSurface: PIDirect3DSurface;
  pDestinationSurface: PIDirect3DSurface
): HRESULT; stdcall
// Branch:DXBX  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('XTL_EmuD3DPushBuffer_CopyRects');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetRenderStateNotInline
(
  State: X_D3DRENDERSTATETYPE;
  Value: DWORD
): HRESULT; stdcall;
// Branch:DXBX  Translator:PatrickvL  Done:100
var
  XboxRenderState_VersionIndependent: X_D3DRENDERSTATETYPE;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_SetRenderState_SetRenderStateNotInline >>').
      _(State, 'State').
      _(Value, 'Value').
    LogEnd();

  XboxRenderState_VersionIndependent := DxbxVersionAdjust_D3DRS(State);

  Result := D3D_OK;
  if (XboxRenderState_VersionIndependent <= X_D3DRS_SIMPLE_LAST) then
  begin
    // Pixel & Simple render states - Just pass them on to our helper :
    XTL_EmuD3DDevice_SetRenderState_Simple_Internal(XboxRenderState_VersionIndependent, Value);
    EmuSwapFS(fsXbox);
    Exit;
  end;

  if  (XboxRenderState_VersionIndependent >= X_D3DRS_DEFERRED_FIRST)
  and (XboxRenderState_VersionIndependent <= X_D3DRS_DEFERRED_LAST) then
  begin
    // Deferred states - Since XTL_EmuD3DDevice_SetRenderState_Deferred
    // is not patched anymore, we'll set the DeferredRenderState here ourselves :
    XTL_EmuMappedD3DRenderState[XboxRenderState_VersionIndependent]^ := Value;
    EmuSwapFS(fsXbox);
    Exit;
  end;

  // What remains are the "complex" render states - each has a separate setter
  // (which are patches, so switch back to Xbox FS first) :
  EmuSwapFS(fsXbox);

  // Dxbx note : All complex render states (up to 5911) are handled.
  // Dxbx note2 : The value is converted from Xbox to PC format inside each patch.
  case XboxRenderState_VersionIndependent of
    X_D3DRS_PSTEXTUREMODES:
      XTL_EmuD3DDevice_SetRenderState_PSTextureModes(Value);
    X_D3DRS_VERTEXBLEND:
      XTL_EmuD3DDevice_SetRenderState_VertexBlend(Value);
    X_D3DRS_FOGCOLOR:
      XTL_EmuD3DDevice_SetRenderState_FogColor(Value);
    X_D3DRS_FILLMODE:
      XTL_EmuD3DDevice_SetRenderState_FillMode(X_D3DFILLMODE(Value));
    X_D3DRS_BACKFILLMODE:
      XTL_EmuD3DDevice_SetRenderState_BackFillMode(Value);
    X_D3DRS_TWOSIDEDLIGHTING:
      XTL_EmuD3DDevice_SetRenderState_TwoSidedLighting(Value);
    X_D3DRS_NORMALIZENORMALS:
      XTL_EmuD3DDevice_SetRenderState_NormalizeNormals(Value);
    X_D3DRS_ZENABLE:
      XTL_EmuD3DDevice_SetRenderState_ZEnable(Value);
    X_D3DRS_STENCILENABLE:
      XTL_EmuD3DDevice_SetRenderState_StencilEnable(Value);
    X_D3DRS_STENCILFAIL:
      XTL_EmuD3DDevice_SetRenderState_StencilFail(Value);
    X_D3DRS_FRONTFACE:
      XTL_EmuD3DDevice_SetRenderState_FrontFace(Value);
    X_D3DRS_CULLMODE:
      XTL_EmuD3DDevice_SetRenderState_CullMode(X_D3DCULL(Value));
    X_D3DRS_TEXTUREFACTOR:
      XTL_EmuD3DDevice_SetRenderState_TextureFactor(Value);
    X_D3DRS_ZBIAS:
      XTL_EmuD3DDevice_SetRenderState_ZBias(Value);
    X_D3DRS_LOGICOP:
      XTL_EmuD3DDevice_SetRenderState_LogicOp(Value);
    X_D3DRS_EDGEANTIALIAS:
      XTL_EmuD3DDevice_SetRenderState_EdgeAntiAlias(Value);
    X_D3DRS_MULTISAMPLEANTIALIAS:
      XTL_EmuD3DDevice_SetRenderState_MultiSampleAntiAlias(Value);
    X_D3DRS_MULTISAMPLEMASK:
      XTL_EmuD3DDevice_SetRenderState_MultiSampleMask(Value);
    X_D3DRS_MULTISAMPLEMODE:
      XTL_EmuD3DDevice_SetRenderState_MultiSampleMode(Value);
    X_D3DRS_MULTISAMPLERENDERTARGETMODE:
      XTL_EmuD3DDevice_SetRenderState_MultiSampleRenderTargetMode(Value);
    X_D3DRS_SHADOWFUNC:
      XTL_EmuD3DDevice_SetRenderState_ShadowFunc(Value);
    X_D3DRS_LINEWIDTH:
      XTL_EmuD3DDevice_SetRenderState_LineWidth(Value);
    X_D3DRS_SAMPLEALPHA:
      XTL_EmuD3DDevice_SetRenderState_SampleAlpha(Value);
    X_D3DRS_DXT1NOISEENABLE:
      XTL_EmuD3DDevice_SetRenderState_Dxt1NoiseEnable(Value);
    X_D3DRS_YUVENABLE:
      XTL_EmuD3DDevice_SetRenderState_YuvEnable(Value);
    X_D3DRS_OCCLUSIONCULLENABLE:
      XTL_EmuD3DDevice_SetRenderState_OcclusionCullEnable(Value);
    X_D3DRS_STENCILCULLENABLE:
      XTL_EmuD3DDevice_SetRenderState_StencilCullEnable(Value);
    X_D3DRS_ROPZCMPALWAYSREAD:
      XTL_EmuD3DDevice_SetRenderState_RopZCmpAlwaysRead(Value);
    X_D3DRS_ROPZREAD:
      XTL_EmuD3DDevice_SetRenderState_RopZRead(Value);
    X_D3DRS_DONOTCULLUNCOMPRESSED:
      XTL_EmuD3DDevice_SetRenderState_DoNotCullUncompressed(Value);
  else
    Result := E_FAIL;
  end;
end;


{$IFDEF DXBX_PIXELSHADER_HOOKS}
function XTL_EmuD3DDevice_GetPixelShader
(
  Value: PDWORD
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetPixelShader').
      _(Value, 'Value').
    LogEnd();

  g_pD3DDevice.GetPixelShader(
{$IFDEF DXBX_USE_D3D9}
    PIDirect3DPixelShader9(Value)
{$ELSE}
    {out}Value^
{$ENDIF}
    );

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;
{$ENDIF DXBX_PIXELSHADER_HOOKS}

function XTL_EmuD3DDevice_GetPixelShaderConstant
(
  Register_: DWORD;
  pConstantData: PVOID;
  ConstantCount: DWORD
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuD3DDevice_GetPixelShaderConstant').
      _(Register_, 'Register').
      _(pConstantData, 'pConstantData').
      _(ConstantCount, 'ConstantCount').
    LogEnd();

{$IFDEF DXBX_USE_D3D9}
  g_pD3DDevice.GetPixelShaderConstantF(Register_, pConstantData, ConstantCount);
{$ELSE}
  g_pD3DDevice.GetPixelShaderConstant(Register_, pConstantData, ConstantCount);
{$ENDIF}

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

{$IFDEF DXBX_PIXELSHADER_HOOKS}
function XTL_EmuD3DDevice_GetPixelShaderFunction
(
  Handle: DWORD;
  pData: PX_D3DPIXELSHADERDEF;
  pSizeOfData: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:75
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('XTL_EmuD3DDevice_GetPixelShaderFunction').
      _(Handle, 'Handle').
      _(pData, 'pData').
      _(pSizeOfData, 'pSizeOfData').
    LogEnd();

  Result := D3DERR_INVALIDCALL;
  if pSizeOfData <> NULL then
  begin
    if pData = NULL then
    begin
      pSizeOfData^ := SizeOf(X_D3DPIXELSHADERDEF);
      Result := D3D_OK;
    end
    else
    begin
      if pSizeOfData^ < SizeOf(X_D3DPIXELSHADERDEF) then
      begin
        pSizeOfData^ := SizeOf(X_D3DPIXELSHADERDEF);
        Result := D3DERR_MOREDATA;
      end
      else
      begin
        // TODO -oDxbx : memcpy(pData, CurrentPixelShader, SizeOf(X_D3DPIXELSHADERDEF));
        Unimplemented('GetPixelShaderFunction needs access to the Handle''s PixelShader!');
        Result := D3D_OK;
      end;
    end;
  end;

  EmuSwapFS(fsXbox);
end;
{$ENDIF DXBX_PIXELSHADER_HOOKS}

exports

(*  D3DIndexBuffer_GetDesc, *) // TODO -oDXBX: NOT YET IMPLEMENTED YET
  // XTL_EmuD3DDevice_FindFence // Not yet implemented
  // XTL_EmuD3DDevice_GetColorMaterial // Not yet implemented
  // XTL_EmuD3DDevice_GetPersistedSurface2, // Dxbx: not implemented yet
  // XTL_EmuD3DDevice_IsCompressedD3DFORMAT // Not yet implemented
  // XTL_EmuD3DDevice_IsResourceSetInDevice // Not yet implemented
  // XTL_EmuD3DDevice_JBInvSqrt // Not yet implemented
  // XTL_EmuD3DDevice_Log // Not yet implemented
  // XTL_EmuD3DDevice_Log2 // Not yet implemented
  // XTL_EmuD3DDevice_MakeRequestedSpace // Not yet implemented
  // XTL_EmuD3DDevice_MakeSpace, // Dxbx: Not yet implemented
  // XTL_EmuD3DDevice_MapToLinearD3DFORMAT // Not yet implemented
  // XTL_EmuD3DDevice_MatrixProduct4x4 // Not yet implemented
  // XTL_EmuD3DDevice_NormalizeVector3 // Not yet implemented
  // XTL_EmuD3DDevice_ParseDeclarationConstants // Not yet implemented
  // XTL_EmuD3DDevice_ParseDeclarationStream // Not yet implemented
  // XTL_EmuD3DDevice_ParseProgram // Not yet implemented
  // XTL_EmuD3DDevice_RecordStateBlock // Not yet implemented
  // XTL_EmuD3DDevice_RestoreVertexShaders // Not yet implemented
  // XTL_EmuD3DDevice_SetRenderState_Deferred, Dxbx note : Disabled, as we DO have EmuD3DDeferredRenderState pin-pointed correctly
  // XTL_EMUD3DDevice_SetSceneAmbientAndMaterialEmission, // Not yet implemented
  // XTL_EmuD3DDevice_SetSpecularParameters, // Not yet implemented
  // XTL_EmuD3DDevice_ShadowVertexShaderState, // Not yet implemented
  // XTL_EmuD3DDevice_SwapCopy, // Not yet implemented
  // XTL_EmuD3DDevice_SwapCopyBlt, // Not yet implemented
  // XTL_EmuD3DDevice_SwapFinish, // Not yet implemented
  // XTL_EmuD3DDevice_SwapFirstFlip, // Not yet implemented
  // XTL_EmuD3DDevice_SwapFlip, // Not yet implemented
  // XTL_EmuD3DDevice_SwapRestoreState // Not yet implemented
  // XTL_EmuD3DDevice_SwapRestoreSurfaces // Not yet implemented
  // XTL_EmuD3DDevice_SwapSaveState // Not yet implemented
  // XTL_EmuD3DDevice_SwapSaveSurfaces // Not yet implemented
  // XTL_EmuD3DDevice_SwapSetState // Not yet implemented
  // XTL_EmuD3DDevice_UpdateProjectionViewportTransform // Not yet implemented
  // XTL_EmuD3DDevice_VideoBitsPerPixelOfD3DFORMAT // Not yet implemented

  // XTL_EmuDevice3D_SetLightColors, // Not yet implemented
  // XTL_EmuDevice3D_FreeFrameBuffers name PatchPrefix + '?FreeFrameBuffers@CDevice@D3D@@QAEXXZ', .. Not yet implemented
  // XTL_EmuDevice3D_GpuGet name PatchPrefix + '?GpuGet@CDevice@D3D@@QAEPCKXZ', // Not yet implemented
  // XTL_EmuDevice3D_InitializePushBuffer name PatchPrefix + '?InitializePushBuffer@CDevice@D3D@@QAEJXZ', Not yet implemented
  // XTL_EmuDevice3D_LazySetStateUP name PatchPrefix + '?LazySetStateUP@CDevice@D3D@@QAEXXZ', // Not yet implemented
  // XTL_EmuDevice3D_LazySetStateVB name PatchPrefix + '?LazySetStateVB@CDevice@D3D@@QAEXK@Z', // Not yet implemented
  // XTL_EmuDevice3D_ReentrantKickOffAndWait name PatchPrefix + '?ReentrantKickOffAndWait@CDevice@D3D@@QAEXXZ', // Not yet implemented
  // XTL_EmuDevice3D_UnInit name PatchPrefix + '?UnInit@CDevice@D3D@@QAEXXZ', // Not yet implemented
  // XTL_EmuDevice3D_UnInit name PatchPrefix + '?UnInit@CDevice@D3D@@QAEXXZ', // Not yet implemented
  // XTL_EmuDevice3D_UninitializePushBuffer name PatchPrefix + '?UninitializePushBuffer@CDevice@D3D@@QAEXXZ', // Not yet implemented
  // XTL_EmuDevice3D_UninitializePushBuffer name PatchPrefix + '?UninitializePushBuffer@CDevice@D3D@@QAEXXZ', Not yet implemented
  // XTL_EmuDirect3D_AllocContiguousMemory name PatchPrefix + '_D3D_AllocContiguousMemory@8', // No need to patch, as our kernel implements MmAllocateContiguousMemory(Ex) already
  // XTL_EmuDirect3D_LazySetCombiners name PatchPrefix + 'LazySetCombiners', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetLights name PatchPrefix + 'LazySetLights', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetPointParams name PatchPrefix + 'LazySetPointParams', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetShaderStageProgram name PatchPrefix + LazySetShaderStageProgram, // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetSpecFogCombiner name PatchPrefix + 'LazySetSpecFogCombiner', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetState name PatchPrefix + 'LazySetState', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetTextureState name + PatchPrefix + 'LazySetTextureState', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetTextureTransform name + PatchPrefix 'LazySetTextureTransform', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetTransform name + PatchPrefix + 'LazySetTransform', // Dxbx: Not yet implemented

  // XTL_EmuFindSurfaceWithinTexture name PatchPrefix + 'FindSurfaceWithinTexture'; // Not yet implemented
  // XTL_EmuGetDepthBufferScale name PatchPrefix + 'GetDepthBufferScale', // Not yet implemented
  // XTL_EmuGetEncodeFormat name PatchPrefix + 'EncodeFormat', // Not yet implemented
  // XTL_EmuGetPitch name PatchPrefix + 'GetPitch', // Not yet implemented
  // XTL_EmuGetSize name PatchPrefix + 'GetSize', // Not yet implemented
  // XTL_EmuGetSlice name PathcPrefix + 'GetSlice', // Not yet implemented
  // XTL_EmuGetSurfaceFormat name PatchPrefix + 'GetSurfaceFormat', // Not yet implemented
  // XTL_EmuXMETAL_StartPush name PatchPrefix + 'XMETAL_StartPush';

  XTL_EmuD3D_KickOffAndWaitForIdle,

  XTL_EmuD3D_PixelJar_Get2DSurfaceDesc,
  XTL_EmuD3D_PixelJar_Lock2DSurface,

  XTL_EmuD3DBaseTexture_GetLevelCount,

  XTL_EmuD3DCubeTexture_GetCubeMapSurface2,
  XTL_EmuD3DCubeTexture_GetLevelDesc,
  XTL_EmuD3DCubeTexture_LockRect,

  XTL_EmuD3DDevice_AddRef,
  XTL_EmuD3DDevice_ApplyStateBlock,
  XTL_EmuD3DDevice_BackFillMode,
  XTL_EmuD3DDevice_Begin,
  XTL_EmuD3DDevice_BeginPush,
  XTL_EmuD3DDevice_BeginPushBuffer, // ??
  XTL_EmuD3DDevice_BeginStateBig,
  XTL_EmuD3DDevice_BeginStateBlock,
  XTL_EmuD3DDevice_BeginVisibilityTest,
  XTL_EmuD3DDevice_BlockOnFence,
  XTL_EmuD3DDevice_BlockUntilIdle,
  XTL_EmuD3DDevice_BlockUntilVerticalBlank,
  XTL_EmuD3DDevice_CaptureStateBlock,
  XTL_EmuD3DDevice_Clear,
  XTL_EmuD3DDevice_CopyRects,
  XTL_EmuD3DDevice_CreateCubeTexture,
  XTL_EmuD3DDevice_CreateDepthStencilSurface,
  XTL_EmuD3DDevice_CreateImageSurface,
  XTL_EmuD3DDevice_CreateIndexBuffer,
  XTL_EmuD3DDevice_CreateIndexBuffer2,
  XTL_EmuD3DDevice_CreatePalette,
  XTL_EmuD3DDevice_CreatePalette2,
{$IFDEF DXBX_PIXELSHADER_HOOKS}
  XTL_EmuD3DDevice_CreatePixelShader,
{$ENDIF}
  XTL_EmuD3DDevice_CreateStateBlock,
  XTL_EmuD3DDevice_CreateSurface,
  XTL_EmuD3DDevice_CreateSurface2,
  XTL_EmuD3DDevice_CreateTexture,
  XTL_EmuD3DDevice_CreateTexture2,
  XTL_EmuD3DDevice_CreateVertexBuffer,
  XTL_EmuD3DDevice_CreateVertexBuffer2,
  XTL_EmuD3DDevice_CreateVertexShader,
  XTL_EmuD3DDevice_CreateVolumeTexture,
{$IFDEF DXBX_PIXELSHADER_HOOKS}
  XTL_EmuD3DDevice_DeletePixelShader,
{$ENDIF}
  XTL_EmuD3DDevice_DeleteStateBlock,
  XTL_EmuD3DDevice_DeleteVertexShader,
  XTL_EmuD3DDevice_DrawIndexedVertices,
  XTL_EmuD3DDevice_DrawIndexedVerticesUP,
  XTL_EmuD3DDevice_DrawRectPatch,
  XTL_EmuD3DDevice_DrawTriPatch,
  XTL_EmuD3DDevice_DrawVertices,
  XTL_EmuD3DDevice_DrawVerticesUP,
  XTL_EmuD3DDevice_EnableOverlay,
  XTL_EmuD3DDevice_End,
  XTL_EmuD3DDevice_EndPush,
  XTL_EmuD3DDevice_EndPushBuffer, // ??
  XTL_EmuD3DDevice_EndStateBlock,
  XTL_EmuD3DDevice_EndVisibilityTest,
  XTL_EmuD3DDevice_FlushVertexCache, // ??
  XTL_EmuD3DDevice_GetBackBuffer,
  XTL_EmuD3DDevice_GetBackBuffer2,
  XTL_EmuD3DDevice_GetBackBufferScale,
  XTL_EmuD3DDevice_GetBackMaterial,
  XTL_EmuD3DDevice_GetCreationParameters,
  XTL_EmuD3DDevice_GetDepthClipPlanes,
  XTL_EmuD3DDevice_GetDepthStencilSurface,
  XTL_EmuD3DDevice_GetDepthStencilSurface2,
//  XTL_EmuD3DDevice_GetDeviceCaps, // Dxbx note : Disabled, too high level.
  XTL_EmuD3DDevice_GetDirect3D,
  XTL_EmuD3DDevice_GetDisplayFieldStatus,
  XTL_EmuD3DDevice_GetDisplayMode,
  XTL_EmuD3DDevice_GetGammaRamp,
  XTL_EmuD3DDevice_GetLight,
  XTL_EmuD3DDevice_GetLightEnable,
  XTL_EmuD3DDevice_GetMaterial,
  XTL_EmuD3DDevice_GetModelView, // ??
  XTL_EmuD3DDevice_GetOverlayUpdateStatus,
//  XTL_EmuD3DDevice_GetOverscanColor, // Dxbx note : Disabled, too high level.
{$IFDEF DXBX_PIXELSHADER_HOOKS}
  XTL_EmuD3DDevice_GetPixelShader,
{$ENDIF}
  XTL_EmuD3DDevice_GetPixelShaderConstant,
{$IFDEF DXBX_PIXELSHADER_HOOKS}
  XTL_EmuD3DDevice_GetPixelShaderFunction,
{$ENDIF}
  XTL_EmuD3DDevice_GetProjectionViewportMatrix,
  XTL_EmuD3DDevice_GetPushBufferOffset,
  XTL_EmuD3DDevice_GetPushDistance,
  XTL_EmuD3DDevice_GetRasterStatus,
  XTL_EmuD3DDevice_GetRenderState,
  XTL_EmuD3DDevice_GetRenderTarget,
  XTL_EmuD3DDevice_GetRenderTarget2,
  XTL_EmuD3DDevice_GetShaderConstantMode,
  XTL_EmuD3DDevice_GetStreamSource2,
  XTL_EmuD3DDevice_GetTexture,
  XTL_EmuD3DDevice_GetTexture2,
  XTL_EmuD3DDevice_GetTextureStageState,
  XTL_EmuD3DDevice_GetTile,
  XTL_EmuD3DDevice_GetTileCompressionTagBits,
  XTL_EmuD3DDevice_GetTileCompressionTags,
  XTL_EmuD3DDevice_GetTransform,
  XTL_EmuD3DDevice_GetVertexShader,
  XTL_EmuD3DDevice_GetVertexShaderConstant,
  XTL_EmuD3DDevice_GetVertexShaderDeclaration,
  XTL_EmuD3DDevice_GetVertexShaderFunction,
  XTL_EmuD3DDevice_GetVertexShaderInput,
  XTL_EmuD3DDevice_GetVertexShaderSize,
  XTL_EmuD3DDevice_GetVertexShaderType,
  XTL_EmuD3DDevice_GetViewport,
  XTL_EmuD3DDevice_GetViewportOffsetAndScale,
  XTL_EmuD3DDevice_GetVisibilityTestResult,
  XTL_EmuD3DDevice_InsertCallback,
  XTL_EmuD3DDevice_InsertFence,
  XTL_EmuD3DDevice_IsBusy,
  XTL_EmuD3DDevice_IsFencePending,
  XTL_EmuD3DDevice_KickPushBuffer,
  XTL_EmuD3DDevice_LightEnable,
  XTL_EmuD3DDevice_LoadVertexShader,
  XTL_EmuD3DDevice_LoadVertexShaderProgram,
  XTL_EmuD3DDevice_Nop,
  XTL_EmuD3DDevice_PersistDisplay,
  XTL_EmuD3DDevice_Present,
  XTL_EmuD3DDevice_PrimeVertexCache,
  XTL_EmuD3DDevice_Release,
  XTL_EmuD3DDevice_Reset,
  XTL_EmuD3DDevice_Resume,
  XTL_EmuD3DDevice_RunPushBuffer,
  XTL_EmuD3DDevice_RunVertexStateShader,
  XTL_EmuD3DDevice_SelectVertexShader,
  XTL_EmuD3DDevice_SelectVertexShaderDirect,
  XTL_EmuD3DDevice_SetBackBufferScale,
  XTL_EmuD3DDevice_SetBackMaterial, // ??
  XTL_EmuD3DDevice_SetCopyRectsState,
  XTL_EmuD3DDevice_SetFlickerFilter,
  XTL_EmuD3DDevice_SetGammaRamp,
  XTL_EmuD3DDevice_SetIndices,
  XTL_EmuD3DDevice_SetLight,
  XTL_EmuD3DDevice_SetMaterial,
  XTL_EmuD3DDevice_SetModelView, // ??
//  XTL_EmuD3DDevice_SetOverscanColor, // Dxbx note : Disabled, too high level.
  XTL_EmuD3DDevice_SetPalette,
{$IFDEF DXBX_PIXELSHADER_HOOKS}
  XTL_EmuD3DDevice_SetPixelShader,
{$ENDIF}
  XTL_EmuD3DDevice_SetPixelShaderConstant,
{$IFDEF DXBX_PIXELSHADER_HOOKS}
  XTL_EmuD3DDevice_SetPixelShaderProgram,
{$ENDIF}
  XTL_EmuD3DDevice_SetRenderState_BackFillMode,
  XTL_EmuD3DDevice_SetRenderState_CullMode,
  XTL_EmuD3DDevice_SetRenderState_DoNotCullUncompressed,
  XTL_EmuD3DDevice_SetRenderState_Dxt1NoiseEnable,
  XTL_EmuD3DDevice_SetRenderState_EdgeAntiAlias,
  XTL_EmuD3DDevice_SetRenderState_FillMode,
  XTL_EmuD3DDevice_SetRenderState_FogColor,
  XTL_EmuD3DDevice_SetRenderState_FrontFace,
  XTL_EmuD3DDevice_SetRenderState_LineWidth,
  XTL_EmuD3DDevice_SetRenderState_LogicOp,
  XTL_EmuD3DDevice_SetRenderState_MultiSampleAntiAlias,
  XTL_EmuD3DDevice_SetRenderState_MultiSampleMask,
  XTL_EmuD3DDevice_SetRenderState_MultiSampleMode,
  XTL_EmuD3DDevice_SetRenderState_MultiSampleRenderTargetMode,
  XTL_EmuD3DDevice_SetRenderState_NormalizeNormals,
  XTL_EmuD3DDevice_SetRenderState_OcclusionCullEnable,
  XTL_EmuD3DDevice_SetRenderState_PSTextureModes,
  XTL_EmuD3DDevice_SetRenderState_RopZCmpAlwaysRead,
  XTL_EmuD3DDevice_SetRenderState_RopZRead,
  XTL_EmuD3DDevice_SetRenderState_SampleAlpha,
  XTL_EmuD3DDevice_SetRenderState_ShadowFunc,
  XTL_EmuD3DDevice_SetRenderState_Simple, // Disabling this patch, makes Turok crash in MakeRequestedSpace; Why?
  XTL_EmuD3DDevice_SetRenderState_StencilCullEnable,
  XTL_EmuD3DDevice_SetRenderState_StencilEnable,
  XTL_EmuD3DDevice_SetRenderState_StencilFail,
  XTL_EmuD3DDevice_SetRenderState_TextureFactor,
  XTL_EmuD3DDevice_SetRenderState_TwoSidedLighting,
  XTL_EmuD3DDevice_SetRenderState_VertexBlend,
  XTL_EmuD3DDevice_SetRenderState_YuvEnable,
  XTL_EmuD3DDevice_SetRenderState_ZBias,
  XTL_EmuD3DDevice_SetRenderState_ZEnable,
  XTL_EmuD3DDevice_SetRenderStateNotInline,
  XTL_EmuD3DDevice_SetRenderTarget,
  XTL_EmuD3DDevice_SetScissors,
  XTL_EmuD3DDevice_SetScreenSpaceOffset,
  XTL_EmuD3DDevice_SetShaderConstantMode,
  XTL_EmuD3DDevice_SetSoftDisplayFilter,
  XTL_EmuD3DDevice_SetStateUP,
  XTL_EmuD3DDevice_SetStateVB,
  XTL_EmuD3DDevice_SetStipple,
  XTL_EmuD3DDevice_SetStreamSource,
  XTL_EmuD3DDevice_SetSwapCallback,
  XTL_EmuD3DDevice_SetTexture,
  XTL_EmuD3DDevice_SetTextureStageStateNotInline,
  XTL_EmuD3DDevice_SetTextureState_BorderColor,
  XTL_EmuD3DDevice_SetTextureState_BumpEnv,
  XTL_EmuD3DDevice_SetTextureState_ColorKeyColor,
//  XTL_EmuD3DDevice_SetTextureState_ParameterCheck, // Not yet implemented
  XTL_EmuD3DDevice_SetTextureState_TexCoordIndex,
  XTL_EmuD3DDevice_SetTileCompressionTagBits,
  XTL_EmuD3DDevice_SetTileNoWait name PatchPrefix + '?SetTileNoWait@D3D@@YGXKPBU_D3DTILE@@@Z',
  XTL_EmuD3DDevice_SetTileNoWait name PatchPrefix + 'D3DDevice_SetTile', // Dxbx note : SetTileNoWait is applied to SetTile in Cxbx 4361 OOPVA's!
  XTL_EmuD3DDevice_SetTransform,
  XTL_EmuD3DDevice_SetVertexBlendModelView, // ??
  XTL_EmuD3DDevice_SetVertexData2f,
  XTL_EmuD3DDevice_SetVertexData2s,
  XTL_EmuD3DDevice_SetVertexData4f,
  XTL_EmuD3DDevice_SetVertexData4s,
  XTL_EmuD3DDevice_SetVertexData4ub,
  XTL_EmuD3DDevice_SetVertexDataColor,
  XTL_EmuD3DDevice_SetVertexShader,
  XTL_EmuD3DDevice_SetVertexShaderConstant,
  XTL_EmuD3DDevice_SetVertexShaderConstant1,
  XTL_EmuD3DDevice_SetVertexShaderConstant4,
  XTL_EmuD3DDevice_SetVertexShaderConstantNotInline,
  XTL_EmuD3DDevice_SetVertexShaderInput,
  XTL_EmuD3DDevice_SetVertexShaderInputDirect,
  XTL_EmuD3DDevice_SetVerticalBlankCallback,
  XTL_EmuD3DDevice_SetViewport,
  XTL_EmuD3DDevice_Suspend,
  XTL_EmuD3DDevice_Swap,
  XTL_EmuD3DDevice_SwitchTexture,
  XTL_EmuD3DDevice_Unknown1 name PatchPrefix + 'D3DDevice_Unknown',
  XTL_EmuD3DDevice_UpdateOverlay,

  XTL_EmuD3DPalette_GetSize,
  XTL_EmuD3DPalette_Lock,
  XTL_EmuD3DPalette_Lock2,

// These pushbuffer functions are used to fill the fixup buffer;
// Since we now support fixups, there's no need to patch them anymore :
//  XTL_EmuD3DPushBuffer_CopyRects,
//  XTL_EmuD3DPushBuffer_SetModelView,
//  XTL_EmuD3DPushBuffer_SetPalette,
//  XTL_EmuD3DPushBuffer_SetRenderState,
//  XTL_EmuD3DPushBuffer_SetVertexBlendModelView,
//  XTL_EmuD3DPushBuffer_SetVertexShaderConstant,
//  XTL_EmuD3DPushBuffer_SetVertexShaderInputDirect,

  XTL_EmuD3DResource_AddRef,
  XTL_EmuD3DResource_BlockUntilNotBusy,
  XTL_EmuD3DResource_FreePrivateData,
  XTL_EmuD3DResource_GetDevice,
  XTL_EmuD3DResource_GetPrivateData,
  XTL_EmuD3DResource_GetType,
  XTL_EmuD3DResource_IsBusy,
  XTL_EmuD3DResource_Register,
  XTL_EmuD3DResource_Release,
  XTL_EmuD3DResource_SetPrivateData,

  XTL_EmuD3DSurface_GetContainer2,
  XTL_EmuD3DSurface_GetDesc,
  XTL_EmuD3DSurface_LockRect,
  XTL_EmuD3DTexture_GetLevelDesc,
  XTL_EmuD3DTexture_GetSurfaceLevel,
  XTL_EmuD3DTexture_GetSurfaceLevel2, // Disabling this patch, makes Turok crash early in CreateSurface or something :
  XTL_EmuD3DTexture_LockRect,

  XTL_EmuD3DVertexBuffer_GetDesc,
  XTL_EmuD3DVertexBuffer_Lock,
  XTL_EmuD3DVertexBuffer_Lock2,

  XTL_EmuD3DVolume_GetContainer2,
  XTL_EmuD3DVolume_GetDesc,
  XTL_EmuD3DVolume_LockBox,

  XTL_EmuD3DVolumeTexture_GetLevelDesc,
  XTL_EmuD3DVolumeTexture_GetVolumeLevel2,
  XTL_EmuD3DVolumeTexture_LockBox,

  XTL_EmuD3D_CDevice_KickOff,

  XTL_EmuDirect3D_CheckDeviceFormat,
  XTL_EmuDirect3D_CheckDeviceMultiSampleType,
{$IFDEF DXBX_TRY_DEEPER_DEVICE_INIT}
//  XTL_EmuD3D__CDevice__Init name PatchPrefix + '?Init@CDevice@D3D@@QAEJPAU_D3DPRESENT_PARAMETERS_@@@Z',
{$ELSE}
  XTL_EmuDirect3D_CreateDevice,
{$ENDIF}
  XTL_EmuDirect3D_EnumAdapterModes,
  XTL_EmuDirect3D_GetAdapterDisplayMode,
  XTL_EmuDirect3D_GetAdapterModeCount,
//  XTL_EmuDirect3D_GetDeviceCaps, // Dxbx note : Disabled, too high level.
  XTL_EmuDirect3D_SetPushBufferSize,

  XTL_EmuGet2DSurfaceDescD; // TODO -oDXBX: Fix wrong prefix!

initialization

  ZeroMemory(@g_VBData, SizeOf(g_VBData));
  ZeroMemory(@g_SwapData, SizeOf(g_SwapData));

finalization

  ExtraXRGBSurface := nil;

end.
