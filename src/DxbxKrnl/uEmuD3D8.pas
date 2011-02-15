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

interface

uses
  // Delphi
  Windows,
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
{$ELSE}
  Direct3D8, // D3DDEVTYPE
  D3DX8, // PD3DXVECTOR4
{$ENDIF}
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uConsts,
  uTime,
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

function DxbxGetDataFromXboxResource(const pThis: PX_D3DResource): Pointer;
function DxbxUnlockD3DResource(pResource: PX_D3DResource; uiLevel: int = 0; iFace: int = Ord(D3DCUBEMAP_FACE_POSITIVE_X) - 1): Boolean;
function DxbxFVF_GetTextureSize(const dwFVF: DWORD; const aTextureIndex: Integer): Integer;
function DxbxFVFToVertexSizeInBytes(const dwFVF: DWORD; bIncludeTextures: Boolean = True): uint;
function DxbxPresent(pSourceRect: PRECT; pDestRect: PRECT; pDummy1: HWND; pDummy2: PVOID): UINT;
procedure DxbxD3DInit(XbeHeader: PXBEIMAGE_HEADER; XbeHeaderSize: UInt32); {NOPATCH}
procedure DxbxDrawPrimitiveUP(const VPDesc: VertexPatchDesc);

procedure DxbxSetRenderStateInternal(
  Caller: string;
  XboxRenderState: X_D3DRenderStateType;
  XboxValue: DWORD
  ); {NOPATCH}

//function XTL_EmuD3DDevice_EnableOverlay(Enable: BOOL): HRESULT; stdcall;
//procedure XTL_EmuD3DDevice_UpdateOverlay(pSurface: PX_D3DSurface;
//  pSrcRect: PRECT;
//  pDstRect: PRECT;
//  EnableColorKey: BOOL;
//  ColorKey: D3DCOLOR); stdcall;

var
  // Global(s)
  g_pD3DDevice: IDirect3DDevice = NULL;
  g_pDDSPrimary7: XTL_LPDIRECTDRAWSURFACE7 = NULL;  // DirectDraw7 Primary Surface
  g_pDDSOverlay7: XTL_LPDIRECTDRAWSURFACE7 = NULL; // DirectDraw7 Overlay Surface
  g_pDDClipper7: XTL_LPDIRECTDRAWCLIPPER = NULL;    // DirectDraw7 Clipper
  g_CurrentVertexShader: DWORD = 0;
  g_PixelShaderConstants: array [0..15] of DWORD; // All 16 Xbox Pixel Shader constants (as DWORD : ARGB)
  g_bFakePixelShaderLoaded: BOOL_ = FALSE;
  g_bIsFauxFullscreen: BOOL_ = FALSE;
  g_bOverlayUpdated: BOOL_ = TRUE; // False if the overlay is still pending display at next vblank (see GetOverlayUpdateStatus)
  g_bHackUpdateSoftwareOverlay: BOOL_ = FALSE;
  g_OverlayHideDelay: int = 0; // The number of Present calls to wait before removing overlay

function EmuThreadRenderWindow(lpVoid: LPVOID): DWORD; stdcall;
function EmuMsgProc(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; // forward
function EmuThreadUpdateTickCount(lpVoid: LPVOID): DWORD; stdcall;
function EmuThreadPollInput(lpVoid: LPVOID): DWORD; stdcall;
function EmuThreadHandleVBlank(lpVoid: LPVOID): DWORD; stdcall;

procedure DxbxCreateDevice();
procedure DumpPresentationParameters(pPresentationParameters: PX_D3DPRESENT_PARAMETERS);

const
  RESOURCE_CACHE_SIZE = 16;

var
  // Static Variable(s)
  g_ddguid: GUID;                // DirectDraw driver GUID
  g_hMonitor: HMONITOR = 0;      // Handle to DirectDraw monitor
  g_pD3D: IDirect3D = NULL;
  g_bYUY2OverlaysSupported: BOOL_ = FALSE; // Does device support YUY2 overlays?
  g_pDD7: XTL_LPDIRECTDRAW7 = NULL;   // DirectDraw7
  g_OverlayRect: TRect;

  g_XbeHeader: PXBEIMAGE_HEADER = NULL;         // XbeHeader
  g_XbeHeaderSize: uint32 = 0;             // XbeHeaderSize
  g_D3DCaps: D3DCAPS;                     // Direct3D8 Caps
  g_XboxCaps: X_D3DCAPS;
  g_MonitorInfo: MONITORINFO;
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

  // current active index buffer
  g_pIndexBuffer: PX_D3DIndexBuffer = NULL; // current active index buffer

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
  g_EmuD3DFrameBufferCount: int = 0;
  g_EmuD3DFrameBuffers: array [0..3-1] of PX_D3DSurface; // [0]=Back buffer [1]=Front buffer [2]=Third buffer (optional)
  g_EmuD3DActiveRenderTarget: PX_D3DSurface = NULL;
  g_EmuD3DActiveDepthStencil: PX_D3DSurface = NULL;
  g_YuvSurface: PX_D3DSurface = NULL;
  g_fYuvEnabled: BOOL_ = FALSE;
  g_dwVertexShaderUsage: DWORD = 0;
  g_VertexShaderSlots: array [0..D3DVS_XBOX_NR_ADDRESS_SLOTS{=136} - 1] of DWORD;

  // cached active palette
  g_EmuD3DActivePalette: array [0..X_D3DTS_STAGECOUNT-1] of PX_D3DPalette; // = {nil, nil, nil, nil};
  // cached palette pointer
  g_pCurrentPalette: PD3DCOLOR;
  // cached palette size
  g_dwCurrentPaletteSize: DWORD = DWORD(-1);

  g_VertexShaderConstantMode: X_VERTEXSHADERCONSTANTMODE = X_D3DSCM_192CONSTANTS;

  // cached Direct3D tiles (max 8 tiles) :
  g_EmuD3DTileCache: array [0..$08 - 1] of X_D3DTILE;

  // cached active texture (max. 4 textures) :
  g_EmuD3DActiveTexture: array [0..X_D3DTS_STAGECOUNT-1] of PX_D3DBaseTexture; // = {nil, nil, nil, nil};

  // cached active streams (max. 16 streams) :
  g_EmuD3DActiveStreams: array [0..MAX_NBR_STREAMS-1] of PX_D3DVertexBuffer;
  g_EmuD3DActiveStreamStrides: array [0..MAX_NBR_STREAMS-1] of UINT;
  g_EmuD3DActiveStreamSizes: array [0..MAX_NBR_STREAMS-1] of UINT;

  g_EmuD3DActivePixelShader: PX_D3DPixelShader = nil;

// information passed to the create device proxy thread
type EmuD3D8CreateDeviceProxyData = packed record
    CreationParameters: D3DDEVICE_CREATION_PARAMETERS;
    pPresentationParameters: PX_D3DPRESENT_PARAMETERS; // Original Xbox version
    NativePresentationParameters: D3DPRESENT_PARAMETERS; // Converted to Windows
    {volatile} hRet: HRESULT;
  end; // packed size = 29

var g_EmuCDPD: EmuD3D8CreateDeviceProxyData;

var XTL_Direct3D_GetDeviceCaps: function
(
//  Adapter: UINT;
//  DeviceType: D3DDEVTYPE;
  pCaps: PD3DCAPS
): HRESULT; stdcall;

var
  DxbxOnSetRenderTarget: procedure (pRenderTarget: PX_D3DSurface; pNewZStencil: PX_D3DSurface) stdcall = nil;

implementation

uses
  // Dxbx
  uDxbxKrnl
  , uEmuKrnlMm // for xboxkrnl_MmAllocateContiguousMemory and DxbxGetNativeContiguousMemoryAddress
  , uXboxLibraryUtils; // Should not be here, but needed for DxbxKrnlRegisterThread

procedure DxbxDebugSwitchQuadIndexWinding; forward;


const lfUnit = lfCxbx or lfGraphics;

var
  g_Title: string = '';
  g_SignalStateDump: Boolean = False;
  g_SignalScreenShot: Boolean = False;

type
  RLogStackHelper = record helper for RLogStack
    function _(const aValue: X_D3DPOOL; const aName: string = ''): PLogStack; overload;
    function _(const aValue: D3DDEVTYPE; const aName: string = ''): PLogStack; overload;
    function _(const aValue: X_NV2AMETHOD; const aName: string = ''): PLogStack; overload;
    function _(const aValue: PD3DVIEWPORT; const aName: string = ''): PLogStack; overload;
    function _(const aValue: PD3DLOCKED_RECT; const aName: string = ''): PLogStack; overload;
    function _(const aValue: PD3DLOCKED_BOX; const aName: string = ''): PLogStack; overload;
    function _(const aValue: PX_D3DResource; const aName: string = ''): PLogStack; overload;
    function _(const aType: TXBType; const aValue: DWORD; const aName: string = ''): PLogStack; overload;
  end;

function RLogStackHelper._(const aValue: X_D3DPOOL; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'X_D3DPOOL');
  SetValue(UIntPtr(aValue), DxbxD3DPoolToString(aValue));
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
begin
  Result := SetName(aName, 'X_NV2AMETHOD');
  SetValue(UIntPtr(aValue), DxbxXboxMethodToString(aValue));
end;

function RLogStackHelper._(const aValue: PD3DVIEWPORT; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'PD3DVIEWPORT');
  if Assigned(aValue) then
    SetValue(DWORD(aValue), Format('%d, %d, %d, %d, %f, %f',
        [aValue.X, aValue.Y, aValue.Width, aValue.Height, aValue.MinZ, aValue.MaxZ]))
  else
    SetValue(UIntPtr(aValue), 'nil');
end;

function RLogStackHelper._(const aValue: PD3DLOCKED_RECT; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'PD3DLOCKED_RECT');
  if Assigned(aValue) then
    SetValue(DWORD(aValue), Format('%d, %0.8x',
        [aValue.Pitch, UIntPtr(aValue.pBits)]))
  else
    SetValue(UIntPtr(aValue), 'nil');
end;

function RLogStackHelper._(const aValue: PD3DLOCKED_BOX; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'PD3DLOCKED_BOX');
  if Assigned(aValue) then
    SetValue(DWORD(aValue), Format('%d, %d, %0.8x',
        [aValue.RowPitch, aValue.SlicePitch, UIntPtr(aValue.pBits)]))
  else
    SetValue(UIntPtr(aValue), 'nil');
end;

function RLogStackHelper._(const aValue: PX_D3DResource; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'PX_D3DResource');
  if Assigned(aValue) then
    SetValue(ResourceToString(aValue))
  else
    SetValue(UIntPtr(aValue), 'nil');
end;

function RLogStackHelper._(const aType: TXBType; const aValue: DWORD; const aName: string = ''): PLogStack;
// Generic display of name, value and value-to-string conversion (if available)
begin
  Result := SetName(aName, DxbxXBTypeInfo[aType].S);
  SetValue(UIntPtr(aValue), DxbxTypedValueToString(aType, aValue));
end;

function LogBegin(const aSymbolName: string; const aCategory: string = ''): PLogStack;
begin
  Result := uLog.LogBegin(aSymbolName, {Category=}'EmuD3D8');
end;

//

procedure DxbxUpdateOverlay(mode: uint; pSrcRect: PRECT = nil; ColorKey: D3DCOLOR = 0);
var
  WindowRect: TRect;
  ddofx: DDOVERLAYFX;
begin
  if (g_pDDClipper7 = nil) or (g_pDDSOverlay7 = nil) then
    Exit;

  // Calculate the destination rectangle :
  GetWindowRect(g_hEmuWindow, {var}WindowRect);

  // Correct rectangle for monitor (no effect on single monitor) :
  Dec(WindowRect.left,   g_MonitorInfo.rcMonitor.left);
  Dec(WindowRect.right,  g_MonitorInfo.rcMonitor.left);
  Dec(WindowRect.top,    g_MonitorInfo.rcMonitor.top);
  Dec(WindowRect.bottom, g_MonitorInfo.rcMonitor.top);

  // Set the color key effect :
  ZeroMemory(@ddofx, sizeof(ddofx));
  ddofx.dwSize := sizeof(DDOVERLAYFX);
  ddofx.dckDestColorkey.dwColorSpaceLowValue := ColorKey;
  ddofx.dckDestColorkey.dwColorSpaceHighValue := ColorKey;

  if (mode and DDOVER_SHOW) > 0 then
    {Dxbx unused hRet :=} IDirectDrawClipper(g_pDDClipper7).SetHWnd(0, g_hEmuWindow);

  {Dxbx unused hRet :=} IDirectDrawSurface7(g_pDDSOverlay7).UpdateOverlay(
    {lpSrcRect=}pSrcRect,
    {lpDDDestSurface=}IDirectDrawSurface7(g_pDDSPrimary7),
    {lpDestRect=}@WindowRect,
    {dwFlags=}{DDOVER_KEYDESTOVERRIDE or} mode,
    {lpDDOverlayFx=}@ddofx);
end;

procedure DxbxDelayedOverlayHide;
begin
  // Disable the overlay (with a delay of 1 swap/present) :
  if g_OverlayHideDelay > 0 then
  begin
    Dec(g_OverlayHideDelay);
    if g_OverlayHideDelay = 0 then
      DxbxUpdateOverlay(DDOVER_HIDE);
  end;
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
  g_OverlayHideDelay := 0;

  ZeroMemory(@g_ddguid, SizeOf(GUID));
  g_hMonitor := 0;
  g_pD3D := NULL;
  g_bYUY2OverlaysSupported := FALSE;
  g_pDD7 := NULL;

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

  g_pIndexBuffer := NULL;

  g_pVertexBuffer := NULL;
  g_pDummyBuffer := NULL;

  ZeroMemory(@g_VBData, SizeOf(g_VBData));
  g_VBLastSwap := 0;

  ZeroMemory(@g_SwapData, SizeOf(g_SwapData));
//  g_SwapLast := 0;

  g_EmuD3DActiveRenderTarget := NULL;
  g_EmuD3DActiveDepthStencil := NULL;
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

  ZeroMemory(@g_EmuD3DActiveStreams, SizeOf(g_EmuD3DActiveStreams));
  ZeroMemory(@g_EmuD3DActiveStreamStrides, SizeOf(g_EmuD3DActiveStreamStrides));
  ZeroMemory(@g_EmuD3DActiveStreamSizes, SizeOf(g_EmuD3DActiveStreamSizes));

  g_EmuD3DActivePixelShader := nil;
end;

procedure DxbxDumpD3DCaps(const aD3DCaps: PD3DCAPS; const aFromXbox: Boolean);
const
  Source: array [Boolean] of string = ('Native', 'Xbox');
var
  LogStack: PLogStack;
begin
  LogStack := uLog.LogBegin(Source[aFromXbox] + ' D3DCaps').
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
    _(aD3DCaps.RasterCaps, 'RasterCaps', DxbxRasterCapsToString(aD3DCaps.RasterCaps)).
    _(aD3DCaps.ZCmpCaps, 'ZCmpCaps', DxbxCmpCapsToString(aD3DCaps.ZCmpCaps)).
    _(aD3DCaps.SrcBlendCaps, 'SrcBlendCaps', DxbxBlendCapsToString(aD3DCaps.SrcBlendCaps)).
    _(aD3DCaps.DestBlendCaps, 'DestBlendCaps', DxbxBlendCapsToString(aD3DCaps.DestBlendCaps)).
    _(aD3DCaps.AlphaCmpCaps, 'AlphaCmpCaps', DxbxCmpCapsToString(aD3DCaps.AlphaCmpCaps)).
    _(aD3DCaps.ShadeCaps, 'ShadeCaps', DxbxShadeCapsToString(aD3DCaps.ShadeCaps)).
    _(aD3DCaps.TextureCaps, 'TextureCaps', DxbxTextureCapsToString(aD3DCaps.TextureCaps)). // D3DPTEXTURECAPS for IDirect3DTexture8's
    _(aD3DCaps.TextureFilterCaps, 'TextureFilterCaps', DxbxTextureFilterCapsToString(aD3DCaps.TextureFilterCaps)). // D3DPTFILTERCAPS for IDirect3DTexture8's
    _(aD3DCaps.CubeTextureFilterCaps, 'CubeTextureFilterCaps', DxbxTextureFilterCapsToString(aD3DCaps.CubeTextureFilterCaps)). // D3DPTFILTERCAPS for IDirect3DCubeTexture8's
    _(aD3DCaps.VolumeTextureFilterCaps, 'VolumeTextureFilterCaps', DxbxTextureFilterCapsToString(aD3DCaps.VolumeTextureFilterCaps)). // D3DPTFILTERCAPS for IDirect3DVolumeTexture8's
    _(aD3DCaps.TextureAddressCaps, 'TextureAddressCaps', DxbxTextureAddressCapsToString(aD3DCaps.TextureAddressCaps)). // D3DPTADDRESSCAPS for IDirect3DTexture8's
    _(aD3DCaps.VolumeTextureAddressCaps, 'VolumeTextureAddressCaps', DxbxTextureAddressCapsToString(aD3DCaps.VolumeTextureAddressCaps)). // D3DPTADDRESSCAPS for IDirect3DVolumeTexture8's

    _(aD3DCaps.LineCaps, 'LineCaps', DxbxLineCapsToString(aD3DCaps.LineCaps)).

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
    _(aD3DCaps.StencilCaps, 'StencilCaps', DxbxStencilCapsToString(aD3DCaps.StencilCaps)).

    _(aD3DCaps.FVFCaps, 'FVFCaps', DxbxFVFCapsToString(aD3DCaps.FVFCaps)).
    _(aD3DCaps.TextureOpCaps, 'TextureOpCaps', DxbxTextureOpCapsToString(aD3DCaps.TextureOpCaps)).
    _(aD3DCaps.MaxTextureBlendStages, 'MaxTextureBlendStages').
    _(aD3DCaps.MaxSimultaneousTextures, 'MaxSimultaneousTextures').

    _(aD3DCaps.VertexProcessingCaps, 'VertexProcessingCaps', DxbxVertexProcessingCapsToString(aD3DCaps.VertexProcessingCaps)).
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
    LogStack := LogStack.
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

    _('VertexTextureFilterCaps').SetValue(aD3DCaps.VertexTextureFilterCaps, DxbxTextureFilterCapsToString(aD3DCaps.VertexTextureFilterCaps)). // D3DPTFILTERCAPS for IDirect3DTexture9's for texture, used in vertex shaders
    _(aD3DCaps.MaxVShaderInstructionsExecuted, 'MaxVShaderInstructionsExecuted'). // maximum number of vertex shader instructions that can be executed
    _(aD3DCaps.MaxPShaderInstructionsExecuted, 'MaxPShaderInstructionsExecuted'). // maximum number of pixel shader instructions that can be executed
    _(aD3DCaps.MaxVertexShader30InstructionSlots, 'MaxVertexShader30InstructionSlots').
    _(aD3DCaps.MaxPixelShader30InstructionSlots, 'MaxPixelShader30InstructionSlots');
  end;
{$ENDIF}

  LogStack.LogEnd();
end;

procedure DxbxDumpXboxStates();
const
  XboxExtStr: array [Boolean] of string = ('', '(Xbox ext.)');
var
  Stage: DWORD;
  XboxTextureStageState: DWORD;
  XboxValue: DWORD;
  XboxRenderState: X_D3DRENDERSTATETYPE;
begin
  // Loop over all Xbox texture stage states :
  for Stage := 0 to X_D3DTS_STAGECOUNT-1 do
  begin
    DbgPrintf('DxbxDumpXboxStates - Texture stage %d states :', [Stage]);
    for XboxTextureStageState := X_D3DTSS_FIRST to X_D3DTSS_LAST do
    begin
      // Get the value and print it :
      XboxValue := XTL_EmuD3DDeferredTextureState[Stage, Ord(DxbxFromNewVersion_D3DTSS(XboxTextureStageState))];
      DbgPrintf('  %-33s[%d] = 0x%.08X; %s %s', [
        DxbxTextureStageStateInfo[XboxTextureStageState].S,
        Stage,
        XboxValue,
        DxbxTypedValueToString(DxbxTextureStageStateInfo[XboxTextureStageState].T, XboxValue),
        XboxExtStr[DxbxTextureStageStateInfo[XboxTextureStageState].X]
        ]);
    end;
  end;

  DbgPrintf('DxbxDumpXboxStates - Render states :');
  // Loop over all Xbox render states :
  for XboxRenderState := X_D3DRS_FIRST to X_D3DRS_LAST do
  begin
    if (XTL_EmuMappedD3DRenderState[XboxRenderState] <> DummyRenderState) then
    begin
      // Get the value and print it :
      XboxValue := XTL_EmuMappedD3DRenderState[XboxRenderState]^;
      DbgPrintf('  %-33s = 0x%.08X; %s %s', [
        DxbxRenderStateInfo[XboxRenderState].S,
        XboxValue,
        DxbxTypedValueToString(DxbxRenderStateInfo[XboxRenderState].T, XboxValue),
        XboxExtStr[DxbxRenderStateInfo[XboxRenderState].PC = D3DRS_UNSUPPORTED] // Xbox extensions have no PC state
        ]);
    end;
  end;
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
    PCState := DxbxRenderStateInfo[XState].PC;
    if PCState = D3DRS_UNSUPPORTED then
      Continue;

    // Get the value and print it :
    g_pD3DDevice.GetRenderState(PCState, {out}Value);
    DbgPrintf('  %-33s = 0x%.08X;', [DxbxRenderStateInfo[XState].S, Value]);
  end;
end;

function DxbxGetDataFromXboxResource(const pThis: PX_D3DResource): Pointer;
begin
  Result := uEmuKrnlMm.DxbxGetNativeContiguousMemoryAddress(pThis.Data);
end;

function DxbxXboxCreateSurface(dwWidth, dwHeight, dwFormat: DWORD; StartingInternalRefCount: uint = 1): PX_D3DSurface;
begin
  Result := XboxCalloc(SizeOf(X_D3DSurface));
  Result.Common := {RefCount=}1 or X_D3DCOMMON_TYPE_SURFACE or X_D3DCOMMON_D3DCREATED or (X_D3DCOMMON_INTREFCOUNT_1 * StartingInternalRefCount);
  Result.Format := DxbxEncodeResourceFormat({bCubeMap=}False, {bBorder=}False, {dwDimens=}2, dwFormat,
       {dwMipMap=}0, {dwUSize=}0, {dwVSize=}0, {dwPSize=}0);
  Result.Size := DxbxEncodeDimensionsIntoSize(dwWidth, dwHeight, {Pitch=}dwWidth * EmuXBFormatBPP(dwFormat) div 8);
end;

procedure DxbxXboxResource_AddRef(pResource: PX_D3DResource); //: int;
begin
  // TODO : It's safer to call the original Xbox version (if found) instead of our implementation.
  if (pResource.Common and X_D3DCOMMON_REFCOUNT_MASK) = X_D3DCOMMON_REFCOUNT_MASK then
    DxbxD3DError('DxbxXboxResource_AddRef', 'Max RefCount reached!', pResource)
  else
    Inc(pResource.Common);
  // Result := pResource.Common and X_D3DCOMMON_REFCOUNT_MASK;
end;

procedure DxbxXboxResource_Release(pResource: PX_D3DResource); //: int;
begin
  // TODO : It's safer to call the original Xbox version (if found) instead of our implementation.
  if (pResource.Common and X_D3DCOMMON_REFCOUNT_MASK) <= 1 then
    XboxFree(pResource) // TODO : Shouldn't we do something more here, like free the Data?
  // Exit(0);
  else
    Dec(pResource.Common); // Dxbx note : We ignore X_D3DCOMMON_REFCOUNT_MASK, as tested underflow already
  // Result := pResource.Common and X_D3DCOMMON_REFCOUNT_MASK;
end;

procedure DxbxXboxResource_AddRef_Internal(pResource: PX_D3DResource); //: int;
begin
  // TODO : It's safer to call the original Xbox version (if found) instead of our implementation.
  if (pResource.Common and X_D3DCOMMON_INTREFCOUNT_MASK) = X_D3DCOMMON_INTREFCOUNT_MASK then
    DxbxD3DError('DxbxXboxResource_AddRef_Internal', 'Max IntRefCount reached!', pResource)
  else
    pResource.Common := pResource.Common + X_D3DCOMMON_INTREFCOUNT_1;
end;

procedure DxbxXboxResource_Release_Internal(pResource: PX_D3DResource); //: int;
begin
  // TODO : It's safer to call the original Xbox version (if found) instead of our implementation.
  if (pResource.Common and X_D3DCOMMON_INTREFCOUNT_MASK) = 0 then
    DxbxD3DError('DxbxXboxResource_Release_Internal', 'IntRefCount not set!', pResource)
  else
  begin
    pResource.Common := pResource.Common - X_D3DCOMMON_INTREFCOUNT_1;
//    if (pResource.Common and X_D3DCOMMON_INTREFCOUNT_MASK) = 0 then
//      Release?
  end;
end;

// Generic unlocking of a D3DResource.
//
// Dxbx Note : As suggested by StrikerX3, we should call this for any resource
// that's locked while it shouldn't be anymore; So call this before Lock() itself,
// before SetTexture(), SetStreamSource(), SetIndices() and maybe other calls too.
function DxbxUnlockD3DResource(pResource: PX_D3DResource; uiLevel: int = 0; iFace: int = Ord(D3DCUBEMAP_FACE_POSITIVE_X) - 1): Boolean;
var
  ResourceType: X_D3DRESOURCETYPE;
begin
  Result := Assigned(pResource) and Assigned(pResource.Emu.Resource);
  if not Result then
    Exit;

  ResourceType := DxbxGetResourceType(pResource);
  case ResourceType of
    X_D3DRTYPE_SURFACE:
      repeat until IDirect3DSurface(pResource.Emu.Surface).UnlockRect() <= D3D_OK;

    X_D3DRTYPE_VOLUME:
      // TODO -oDxbx : Totally untested! Volume's aren't used yet!
      repeat until IDirect3DVolume(pResource.Emu.Resource).UnlockBox() <= D3D_OK;

    X_D3DRTYPE_TEXTURE:
      repeat until IDirect3DTexture(pResource.Emu.Texture).UnlockRect(uiLevel) <= D3D_OK;

    X_D3DRTYPE_VOLUMETEXTURE:
      // TODO -oDxbx : Totally untested! VolumeTexture's aren't used yet!?
      repeat until IDirect3DVolumeTexture(pResource.Emu.VolumeTexture).UnlockBox(uiLevel) <= D3D_OK;

    X_D3DRTYPE_CUBETEXTURE:
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

    X_D3DRTYPE_VERTEXBUFFER:
      repeat until IDirect3DVertexBuffer(pResource.Emu.VertexBuffer).Unlock() <= D3D_OK;

    X_D3DRTYPE_INDEXBUFFER:
      repeat until IDirect3DIndexBuffer(pResource.Emu.IndexBuffer).Unlock() <= D3D_OK;
  else
    Result := False;
  end;

  if Result then
    DbgPrintf('UNLOCKED ' + ResourceToString(pResource));
end;

function DxbxFVF_GetTextureSize(const dwFVF: DWORD; const aTextureIndex: Integer): Integer;
// Determine the size (in bytes) of the texture format (indexed 0 .. 3).
// This is the reverse of the D3DFVF_TEXCOORDSIZE[0..3] macros.
begin
  case (dwFVF shr ((aTextureIndex * 2) + 16)) and 3 of
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
function DxbxFVFToVertexSizeInBytes(const dwFVF: DWORD; bIncludeTextures: Boolean = True): uint;
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
  Result := (dwFVF and D3DFVF_POSITION_MASK) shr 1;
  if Result >= (D3DFVF_XYZB1 shr 1) then
    // Any format from D3DFVF_XYZB1 and above need 1 extra float :
    Inc(Result, 1)
  else
    // The other formats (XYZ and XYZRHW) need 2 extra floats :
    Inc(Result, 2);

  // Express the size in bytes, instead of floats :
  Result := Result * sizeof(FLOAT);

  // D3DFVF_NORMAL cannot be combined with D3DFVF_XYZRHW :
  if (dwFVF and D3DFVF_POSITION_MASK) <> D3DFVF_XYZRHW then
    if (dwFVF and D3DFVF_NORMAL) > 0 then begin Inc(Result, sizeof(FLOAT)*3); end;

  if (dwFVF and D3DFVF_DIFFUSE) > 0 then begin Inc(Result, sizeof(D3DCOLOR)); end;
  if (dwFVF and D3DFVF_SPECULAR) > 0 then begin Inc(Result, sizeof(D3DCOLOR)); end;

  if bIncludeTextures then
  begin
    NrTextures := ((dwFVF and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT);
    while NrTextures > 0 do
    begin
      Dec(NrTextures);
      Inc(Result, DxbxFVF_GetTextureSize(dwFVF, NrTextures));
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
      DbgPrintf('EmuD3D8 : %s could not create a extra buffer!'#13#10 + DxbxD3DErrorString(aResult), [Caller]);
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
    DbgPrintf('EmuD3D8 : DxbxTakeScreenShot could not get back buffer!'#13#10 + DxbxD3DErrorString(hRet));
    Exit;
  end;

  AssureExtraXRGBSurface(BackBufferSurface, 'DxbxTakeScreenShot');

  // Convert the backbuffer to the intermediate buffer (which format is supported by D3DXSaveSurfaceToFile) :
  hRet := D3DXLoadSurfaceFromSurface(ExtraXRGBSurface, NULL, NULL, BackBufferSurface, NULL, NULL, D3DX_FILTER_LINEAR{D3DX_DEFAULT}, 0);
  if (FAILED(hRet)) then
  begin
    DbgPrintf('EmuD3D8 : DxbxTakeScreenShot could not convert back buffer!'#13#10 + DxbxD3DErrorString(hRet));
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
    DbgPrintf('EmuD3D8 : DxbxTakeScreenShot could not write screen buffer!'#13#10 + DxbxD3DErrorString(hRet))
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
  if g_SignalStateDump then
  begin
    g_SignalStateDump := False;

    DxbxDumpXboxStates();
    DxbxDumpNativeRenderStates();
  end;

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
    if (Result = UINT(D3DERR_DEVICELOST)) then //Device is lost and cannot be reset yet
      Sleep(500) //Wait a bit so we don't burn through cycles for no reason
    else
      if (Result = UINT(D3DERR_DEVICENOTRESET)) then //Lost but we can reset it now
        Result := g_pD3DDevice.Reset({const}g_EmuCDPD.NativePresentationParameters);
  until Result = UINT(D3D_OK);
end;

type
  RPatch = record
    Symbol: PByte;
    Target: Pointer;
    OriginalHeader: array [0..4] of Byte;
    procedure Init(Symbol: PByte; Target: Pointer);
    procedure Unpatch;
    procedure Repatch;
  end;

procedure RPatch.Init(Symbol: PByte; Target: Pointer);
begin
  Self.Symbol := Symbol;
  Self.Target := Target;
  Repatch;
end;

procedure RPatch.Repatch;
begin
  if Symbol^ <> OPCODE_JMP then
  begin
    // Backup original 5 bytes of (currently unpatched) symbol :
    memcpy(@OriginalHeader[0], Symbol, 5);

    // Overwrite that with a jump to the interceptor :
    Symbol^ := OPCODE_JMP;
    PDWORD(Symbol+1)^ := DWORD(Target) - UIntPtr(Symbol) - 5;
  end;
end;

procedure RPatch.Unpatch;
begin
  // Restore the original code :
  memcpy(Symbol, @OriginalHeader[0], 5);
end;

var
  CDevice_Init_Patch: RPatch;

type
  TD3D_CDevice_Init = function
  (
    {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
    {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
    {1 ECX}This: Pvoid;
    {2 stack}pPresentationParameters: PX_D3DPRESENT_PARAMETERS
  ): HRESULT; register; // thiscall simulation - See Translation guide

function Dxbx_CDevice_Init
(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}pPresentationParameters: PX_D3DPRESENT_PARAMETERS
): HRESULT; register; // thiscall simulation - See Translation guide
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('Dxbx_CDevice_Init').
       _(This, 'This').
       _(pPresentationParameters, 'pPresentationParameters').
    LogEnd();

    DumpPresentationParameters(pPresentationParameters);
  end;

  g_EmuCDPD.pPresentationParameters := pPresentationParameters;
  DxbxCreateDevice();

  // Call the original version :
  begin
    CDevice_Init_Patch.Unpatch;

    EmuSwapFS(fsXbox);
    Result := TD3D_CDevice_Init(CDevice_Init_Patch.Symbol)(0, 0, This, pPresentationParameters);
    EmuSwapFS(fsWindows);

    CDevice_Init_Patch.Repatch;
  end;

  EmuSwapFS(fsXbox);
end;

procedure TemporarilyPatch_CDevice_Init;
begin
  CDevice_Init_Patch.Init(XTL_D3D_CDevice_Init, @Dxbx_CDevice_Init);
end;

// Direct3D initialization (called before emulation begins)
procedure DxbxD3DInit(XbeHeader: PXBEIMAGE_HEADER; XbeHeaderSize: uint32); {NOPATCH}
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  dwThreadId: DWORD;
  hThread: HANDLE;
  hDupHandle: HANDLE;
  DevType: D3DDEVTYPE;
  Identifier: D3DADAPTER_IDENTIFIER;
//  PresParam: X_D3DPRESENT_PARAMETERS;
begin
  DxbxResetGlobals();

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
      uLog.LogBegin('Adapter identifier').
        _(AnsiString(Identifier.Driver), 'Driver').
        _(AnsiString(Identifier.Description), 'Description').
        _(Format('%d.%d.%d.%d', [
          HiWord(Identifier.DriverVersionHighPart),
          LoWord(Identifier.DriverVersionHighPart),
          HiWord(Identifier.DriverVersionLowPart),
          LoWord(Identifier.DriverVersionLowPart)])
          , 'DriverVersion').
        _(Identifier.VendorId, 'VendorId').
        _(Identifier.DeviceId, 'DeviceId').
        _(Identifier.SubSysId, 'SubSysId').
        _(Identifier.Revision, 'Revision').
        _(GUIDToString(Identifier.DeviceIdentifier), 'DeviceIdentifier').
        _(Identifier.WHQLLevel, 'WHQLLevel').
      LogEnd();

    // Show native device capabilities :
    begin
      DevType := iif(g_XBVideo.GetDirect3DDevice() = 0, D3DDEVTYPE_HAL, D3DDEVTYPE_REF);
      g_pD3D.GetDeviceCaps(g_XBVideo.GetDisplayAdapter(), DevType, {out}g_D3DCaps);
      DxbxDumpD3DCaps(@g_D3DCaps, {FromXbox=}False);

      // TODO : Fail if the current device doesn't support D3DPTEXTURECAPS_CUBEMAP, D3DPTEXTURECAPS_VOLUMEMAP, or D3DPTEXTURECAPS_MIPMAP
    end;

    // Show Xbox device capabilities :
    if Addr(XTL_Direct3D_GetDeviceCaps) <> nil then
    begin
      XTL_Direct3D_GetDeviceCaps(@g_XboxCaps);
      DxbxDumpD3DCaps(@g_XboxCaps, {FromXbox=}True);
    end
    else
      DbgPrintf('Could not get Xbox device caps!');

    // TODO : Dump supported D3DFormats using GetAdapterModeCount, EnumAdapterModes, CheckDeviceFormat and CheckDeviceType!
  end;

  TemporarilyPatch_CDevice_Init;

  SetFocus(g_hEmuWindow);
end; // DxbxD3DInit

//// cleanup Direct3D
//procedure XTL_EmuD3DCleanup(); {NOPATCH}
//// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
//begin
//  XTL_EmuDInputCleanup();
//end;

procedure DxbxSetRenderStateInternal(
  Caller: string;
  XboxRenderState: X_D3DRenderStateType;
  XboxValue: DWORD
  ); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  PCValue: DWORD;
begin
  if (Caller <> '') and MayLog(lfUnit) then
    LogBegin(Caller).
      _(DxbxRenderStateInfo[XboxRenderState].T, XboxValue, 'Value').
    LogEnd();

  PCValue := Dxbx_SetRenderState(XboxRenderState, XboxValue);

  if MayLog(lfUnit or lfReturnValue) then
  begin
    // Dump the value that's being forwarded to PC :
    if PCValue <> XboxValue then
      DbgPrintf('  %s := 0x%.08X (converted from Xbox)', [DxbxRenderStateInfo[XboxRenderState].S, PCValue])
    else
      DbgPrintf('  %s := 0x%.08X', [DxbxRenderStateInfo[XboxRenderState].S, PCValue]);
  end;
end;

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

    // Print stop message separately...
    DbgPrintf('Message WM_QUIT received.');
    // So that we can cleanup without a messagebox :
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
        VK_F7:
        begin
          g_SignalStateDump := True;
        end;
        VK_F8:
        begin
          uLog.ToggleLogging;
        end;
        VK_F9:
        begin
          DxbxDebugSwitchQuadIndexWinding;
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
          // PrintScreen key can't be detected somehow, so use F12 for taking screenshotss :
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

var
  ddcaps: DDCAPS_DX6;

procedure DxbxCreateDevice();
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
    DbgPrintf('DxbxCreateDevice()');

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

      g_EmuCDPD.NativePresentationParameters.FullScreen_RefreshRateInHz := 0; // D3DDisplayMode.RefreshRate ??
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
//      else
//        g_EmuCDPD.NativePresentationParameters.BackBufferFormat := EmuXB2PC_D3DFormat(g_EmuCDPD.pPresentationParameters.BackBufferFormat);
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

  // Cleanup previous device (and all related state) if necessary :
  if Assigned(g_pD3DDevice) then
  begin
    while g_pD3DDevice._Release > 0 do
      ;
  end;

{$IFDEF DXBX_USE_OPENGL}
  Exit; // Test to see if CreateDevice tutorial works (don't create D3DDevice)
{$ENDIF}

  // redirect to windows Direct3D
  g_EmuCDPD.hRet := g_pD3D.CreateDevice
  (
    g_EmuCDPD.CreationParameters.AdapterOrdinal,
    g_EmuCDPD.CreationParameters.DeviceType,
    g_EmuCDPD.CreationParameters.hFocusWindow,
    g_EmuCDPD.CreationParameters.BehaviorFlags,
    @g_EmuCDPD.NativePresentationParameters,
    @g_pD3DDevice
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
      @g_pD3DDevice
    );
  end;

  // report error
  if (FAILED(g_EmuCDPD.hRet)) then
    DxbxD3DError('DxbxCreateDevice', 'IDirect3D8.CreateDevice failed', nil, g_EmuCDPD.hRet);

  // Update Xbox PresentationParameters :
  g_EmuCDPD.pPresentationParameters.BackBufferWidth := g_EmuCDPD.NativePresentationParameters.BackBufferWidth;
  g_EmuCDPD.pPresentationParameters.BackBufferHeight := g_EmuCDPD.NativePresentationParameters.BackBufferHeight;
  g_EmuCDPD.pPresentationParameters.BackBufferFormat := EmuPC2XB_D3DFormat(g_EmuCDPD.NativePresentationParameters.BackBufferFormat);
  g_EmuCDPD.pPresentationParameters.BackBufferCount := g_EmuCDPD.NativePresentationParameters.BackBufferCount;

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
      DxbxD3DError('DxbxCreateDevice', 'Could not initialize DirectDraw7', nil, hRet);

    hRet := IDirectDraw7(g_pDD7).SetCooperativeLevel(0, DDSCL_NORMAL);
    if (FAILED(hRet)) then
      DxbxD3DError('DxbxCreateDevice', 'Could not set cooperative level', nil, hRet);
  end;

  // check for YUY2 overlay support
  // TODO -oCXBX: accept other overlay types
  begin
    dwCodes := 0;
    lpCodes := nil;
    IDirectDraw7(g_pDD7).GetFourCCCodes({var}dwCodes, lpCodes);
    lpCodes := DxbxMalloc(dwCodes*sizeof(DWORD));
    IDirectDraw7(g_pDD7).GetFourCCCodes({var}dwCodes, lpCodes);

    ZeroMemory(@ddcaps, SizeOf(DDCAPS_DX6));
    ddcaps.dwSize := SizeOf(DDCAPS_DX6);
    IDirectDraw7(g_pDD7).GetCaps(@ddcaps, nil);

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

    // Retreive monitor info :
    ZeroMemory(@g_MonitorInfo, sizeof(MONITORINFO));
    g_MonitorInfo.cbSize := sizeof(MONITORINFO);
    GetMonitorInfo(g_hMonitor, @g_MonitorInfo); // g_hMonitor is set by EmuEnumDisplayDevices
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
      DxbxD3DError('DxbxCreateDevice', 'Could not create primary surface', nil, hRet);
  end;

  g_pD3DDevice.CreateVertexBuffer
  (
    {Length=}1,
    {Usage=}0,
    {FVF=}0,
    {Pool=}D3DPOOL_MANAGED,
    {ppVertexBuffer=}@g_pDummyBuffer
    {$IFDEF DXBX_USE_D3D9}, {pSharedHandle=}NULL{$ENDIF}
  );
  DbgPrintf('CreateVertexBuffer: g_pDummyBuffer = 0x%0.8x', [g_pDummyBuffer]);

  for Streams := 0 to MAX_NBR_STREAMS-1 do
  begin
    // Dxbx note : Why do we need a dummy stream at all?
    g_pD3DDevice.SetStreamSource(Streams, IDirect3DVertexBuffer(g_pDummyBuffer), {$IFDEF DXBX_USE_D3D9}{OffsetInBytes=}0, {$ENDIF} 1);
  end;

  // begin scene
  g_pD3DDevice.BeginScene();

  // initially, show a black screen
  g_pD3DDevice.Clear(0, nil, D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER, $FF000000, 1.0, 0);
  DxbxPresent(nil, nil, 0, nil);
end;

procedure DumpPresentationParameters(pPresentationParameters: PX_D3DPRESENT_PARAMETERS);
begin
  // Print a few of the pPresentationParameters contents to the console
  if MayLog(lfUnit) then
    uLog.LogBegin('PresentationParameters:').
      _(pPresentationParameters.BackBufferWidth, 'BackBufferWidth').
      _(pPresentationParameters.BackBufferHeight,'BackBufferHeight').
      _(xtD3DFORMAT, DWORD(pPresentationParameters.BackBufferFormat), 'BackBufferFormat').
      _(pPresentationParameters.BackBufferCount,'BackBufferCount').
      _(xtD3DMULTISAMPLE_TYPE, DWORD(pPresentationParameters.MultiSampleType), 'MultiSampleType').
      _(Ord(pPresentationParameters.SwapEffect), 'SwapEffect').
      _(pPresentationParameters.hDeviceWindow, 'hDeviceWindow').
      _(pPresentationParameters.Windowed, 'Windowed').
      _(pPresentationParameters.EnableAutoDepthStencil,'EnableAutoDepthStencil').
      _(xtD3DFORMAT, DWORD(pPresentationParameters.AutoDepthStencilFormat), 'AutoDepthStencilFormat').
      _(pPresentationParameters.Flags, 'Flags').
      _(pPresentationParameters.FullScreen_RefreshRateInHz, 'FullScreen_RefreshRateInHz').
      _(pPresentationParameters.FullScreen_PresentationInterval, 'FullScreen_PresentationInterval').
    LogEnd();
end;

{$J+}
const
  // TODO -oDxbx : Officially, we should have an array like this for each adapter :
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

{$IFNDEF PUSHBUFFER_ONLY}

(*procedure XTL_EmuXMETAL_StartPush
(
  Unknown: Pvoid
); stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('XMETAL_StartPush').
      _(Unknown, 'Unknown').
    LogEnd();

  EmuSwapFS(fsXbox);
end;*)

(*
function XTL_EmuD3DResource_AddRef
(
  pThis: PX_D3DResource
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DResource_Release(
  pThis: PX_D3DResource
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

function XTL_EmuD3DResource_IsBusy
(
    pThis: PX_D3DResource
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
//var
//  pPCResource: XTL_PIDirect3DResource8;
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('D3DResource_IsBusy').
      _(pThis, 'pThis').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  // pPCResource := pThis.Emu.Resource;

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
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('D3DResource_GetDevice').
      _(pThis, 'pThis').
      _(ppDevice, 'ppDevice').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  ppDevice^ := Pointer(g_pD3DDevice); // TODO -oDxbx : Is this correct?
end;

(*
function XTL_EmuD3DResource_GetType
(
    pThis: PX_D3DResource
): X_D3DRESOURCETYPE; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

procedure XTL_EmuD3DResource_BlockUntilNotBusy
(
  pThis: PX_D3DResource
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DResource_BlockUntilNotBusy').
      _(pThis, 'pThis').
    LogEnd();

  // TODO -oCXBX: Implement
//  while g_bIsBusy do Sleep(1); //??

  EmuSwapFS(fsXbox);
end;

(*
function XTL_EmuD3DResource_SetPrivateData
(
  pThis: PX_D3DResource;
  refguid: REFGUID;
  pData: Pvoid;
  SizeOfData: DWORD;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DResource_GetPrivateData
(
  pThis: PX_D3DResource;
  refguid: REFGUID;
  pData: Pvoid;
  PSizeOfData: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DResource_FreePrivateData
(
  pThis: PX_D3DResource;
  refguid: REFGUID
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
*)

(*
procedure XTL_EmuD3DResource_Register
(
    pThis: PX_D3DResource;
    pBase: PVOID
); stdcall;
// Branch:shogun  Revision:162  Translator:Shadow_Tj  Done:100
// Dxbx note : Before a call to this Register function, an application should have called
// one of XGSetCubeTextureHeader/XGSetSurfaceHeader/XGSetTextureHeader/XGSetVolumeTextureHeader
// to initialize the resource header with format, dimensions and stuff. Next it should have
// called D3D_AllocContiguousMemory (to allocate a correctly aligned buffer memory for the resource).
// Then (on the Xbox) Register just sets the Data field (and modifies no other resource header fields).
//
// PS: For other resources, similar XGSet* functions exist:
// XGSetFixupHeader, XGSetIndexBufferHeader, XGSetPaletteHeader, XGSetPushBufferHeader and XGSetVertexBufferHeader.
*)

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

procedure XTL_EmuD3D_BlockOnResource
(
  pResource: PX_D3DResource
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('D3D_BlockOnResource').
      _(pResource, 'pResource').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  // Do nothing
end;

procedure XTL_EmuD3D_BlockOnNonSurfaceResource
(
    pResource: PX_D3DResource
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('D3D_BlockOnNonSurfaceResource').
      _(pResource, 'pResource').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  // Do nothing
end;

procedure XTL_EmuD3D_CleanPrivateData
(
  pResource: PX_D3DResource
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  i: int;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3D_CleanPrivateData').
      _(pResource, 'pResource').
    LogEnd();

  // TODO : Replicate real CleanPrivateData here (or use a hook instead of a patch)

  // Check if we've been called from DestroyResource :
  i := ((pResource.Common and X_D3DCOMMON_REFCOUNT_MASK)                                     )
     + ((pResource.Common and X_D3DCOMMON_INTREFCOUNT_MASK) shr X_D3DCOMMON_INTREFCOUNT_Shift);
  if (i <= 1) then
  begin
    // If this was an index buffer, remove any native copies to prevent later merges with old data :
    // (This crashed the Benchmark sample after a few 'X' presses, which recreates index buffers.)
    DxbxRemoveIndexBuffer(DxbxGetDataFromXboxResource(pResource));

    // Release the native resource :
    if Assigned(pResource.Emu.Resource) then
    begin
      if (pResource.Common and X_D3DCOMMON_TYPE_SURFACE) = 0 then
      begin
        // Note : If the next line is repeated using a 'while _Release() > 0' test,
        // it would crash when releasing parents of surfaces, like the second call
        // in the WMCutScene sample (after playing the video) :
        IInterface(pResource.Emu.Resource)._Release()
      end
      else
      begin
        // Surfaces however must remove all references (otherwise, the
        // Lensflare sample looses it's flares) :
        while IInterface(pResource.Emu.Resource)._Release() > 0 do
          ;
      end;

      pResource.Emu.Resource := nil;
    end;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_GetDataFromResource
(
    pResource: PX_D3DResource
): PBYTE; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('D3D_GetDataFromResource').
      _(pResource, 'pResource').
    LogEnd();

  Result := DxbxGetDataFromXboxResource(pResource);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_KickOffAndWaitForIdle(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
{$IFDEF DXBX_USE_D3D9}
var
  pQuery: PIDirect3DQuery9;
  data: BOOL;
{$ENDIF}
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3D_KickOffAndWaitForIdle').LogEnd();

  // TODO : Actually do something - maybe call XTL_EmuD3DDevice_KickPushBuffer ?

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
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_PixelJar_FindSurfaceWithinTexture
(
  pPixelContainer: PX_D3DPixelContainer;
  FaceType: D3DCUBEMAP_FACES;
  Level: UINT;
  ppbData: PPBYTE;
  pRowPitch: PDWORD;
  pSlicePitch: PDWORD;
  pFormat: PDWORD;
  pSize: PDWORD
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:70
var
  Info: RDxbxDecodedPixelContainer;
  DataPtr: UIntPtr;
  RowPitch: DWORD;
  SlicePitch: DWORD;
  NewFormat: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3D_PixelJar_FindSurfaceWithinTexture').
      _(pPixelContainer, 'pPixelContainer').
      _(xtD3DCUBEMAP_FACES, Ord(FaceType), 'FaceType').
      _(Level, 'Level').
      _(ppbData, 'ppbData').
      _(pRowPitch, 'pRowPitch').
      _(pSlicePitch, 'pSlicePitch').
      _(pFormat, 'pFormat').
      _(pSize, 'pSize').
    LogEnd();

  // Retrieve characteristics of this pixel container :
  DxbxGetFormatRelatedVariables(pPixelContainer, {out}Info);
  // Read the base data address :
  DataPtr := UIntPtr(DxbxGetDataFromXboxResource(pPixelContainer));
  // Add the face offset (adds zero for non-cubemaps) :
  Inc(DataPtr, Info.dwFacePitch * uint(FaceType));
  // Add mipmap level offset (adds zero for mipmap level 0) :
  Inc(DataPtr, Info.MipMapOffsets[Level]);
  RowPitch := Info.dwRowPitch shr Level;
  // Volume slices don't need to be added, but their size does have to be returned :
  SlicePitch := Info.SlicePitches[Level];
  // Based on Level, return the recalculated D3DFORMAT_USIZE_MASK, D3DFORMAT_VSIZE_MASK and D3DFORMAT_PSIZE_MASK in pFormat^ :
  NewFormat := Info.MipMapFormats[Level];

  // Set the resulting outputs :
  ppbData^ := PByte(DataPtr);
  pRowPitch^ := RowPitch;
  if Assigned(pSlicePitch) then // TODO : Is this check really needed?
    pSlicePitch^ := SlicePitch;
  pFormat^ := NewFormat;
  pSize^ := pPixelContainer.Size; // The Size field never needs to be recalculated

  // Log the output for debugging purposes :
  if MayLog(lfUnit or lfReturnValue) then
    LogBegin('D3D_PixelJar_FindSurfaceWithinTexture result').
      _(Pointer(DataPtr), 'DataPtr').
      _(RowPitch, 'RowPitch').
      _(SlicePitch, 'SlicePitch').
      _(Pointer(NewFormat), 'NewFormat').
    LogEnd();

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_PixelJar_Get2DSurfaceDesc
(
    pPixelContainer: PX_D3DPixelContainer;
    dwLevel: DWORD;
    pDesc: PX_D3DSURFACE_DESC
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
// Needs a patch because it accesses _D3D__pDevice at some offset,
// probably comparing the data of this pixelcontainer to the framebuffer
// and setting the MultiSampleType as a result to either the device's
// MultiSampleType or D3DMULTISAMPLE_NONE.
var
  Info: RDxbxDecodedPixelContainer;
//  SurfaceDesc: D3DSURFACE_DESC;
//  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3D_PixelJar_Get2DSurfaceDesc').
      _(pPixelContainer, 'pPixelContainer').
      _(dwLevel, 'dwLevel').
      _(pDesc, 'pDesc').
    LogEnd();

  DxbxGetFormatRelatedVariables(pPixelContainer, {out}Info);
  pDesc.Format := Info.X_Format;
  pDesc.Type_ := DxbxGetResourceType(pPixelContainer);

  // Include X_D3DUSAGE_RENDERTARGET or X_D3DUSAGE_DEPTHSTENCIL where appropriate, which means :
  // only at dwLevel=0, when this is actually the active render target or depth stencil buffer.
  pDesc.Usage := 0;
  if dwLevel = 0 then
  begin
    if pPixelContainer = g_EmuD3DActiveRenderTarget then
      pDesc.Usage := X_D3DUSAGE_RENDERTARGET
    else
      if pPixelContainer = g_EmuD3DActiveDepthStencil then
        pDesc.Usage := X_D3DUSAGE_DEPTHSTENCIL;
  end;

  pDesc.Size := Info.MipMapOffsets[dwLevel+1]; // ??
  pDesc.MultiSampleType := X_D3DMULTISAMPLE_NONE; // TODO : If this is the active backbuffer, use the devices' multi sampling
  pDesc.Width := Info.dwWidth;
  pDesc.Height := Info.dwHeight;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_PixelJar_Get2DSurfaceDescD
(
  pPixelContainer: PX_D3DPixelContainer;
  pDesc: PX_D3DSURFACE_DESC
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('D3D_PixelJar_Get2DSurfaceDescD >>').
      _(pPixelContainer, 'pPixelContainer').
      _(pDesc, 'pDesc').
    LogEnd();

    EmuSwapFS(fsXbox);
  end;

  XTL_EmuD3D_PixelJar_Get2DSurfaceDesc(pPixelContainer, 0{?}, pDesc);
end;

procedure XTL_EmuD3D_PixelJar_LockSurface
(
    pPixelContainer: PX_D3DPixelContainer;
    FaceType: D3DCUBEMAP_FACES;
    Level: UINT;
    Flags: DWORD;
    ppbData: PPBYTE;
    pRowPitch: PDWORD;
    pSlicePitch: PDWORD
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
// This patch is needed to prevent the original code messing up the Data pointer.
// (This can happen if X_D3DLOCK_TILED is present in the Flags argument!)
var
  DummyFormat, DummySize: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3D_PixelJar_LockSurface >>').
      _(pPixelContainer, 'pPixelContainer').
      _(xtD3DCUBEMAP_FACES, Ord(FaceType), 'FaceType').
      _(Level, 'Level').
      _(Flags, 'Flags').
      _(ppbData, 'ppbData').
      _(pRowPitch, 'pRowPitch').
      _(pSlicePitch, 'pSlicePitch').
    LogEnd();

  // Get a pointer to the actual surface.
  EmuSwapFS(fsXbox);

  XTL_EmuD3D_PixelJar_FindSurfaceWithinTexture(pPixelContainer, FaceType, Level,
    ppbData, pRowPitch, pSlicePitch, @DummyFormat, @DummySize);
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
// This patch is needed to prevent the original code messing up the Data pointer.
// (This can happen if X_D3DLOCK_TILED is present in the Flags argument!)
var
  DummySlicePitch, DummyFormat, DummySize: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3D_PixelJar_Lock2DSurface >').
      _(pPixelContainer, 'pPixelContainer').
      _(xtD3DCUBEMAP_FACES, Ord(FaceType), 'FaceType').
      _(Level, 'Level').
      _(Pointer(pLockedRect), 'pLockedRect'). // Cast to prevent contents dump here
      _(pRect, 'pRect').
      _(Flags, 'Flags'). // Ignored
    LogEnd();

  EmuSwapFS(fsXbox);
  XTL_EmuD3D_PixelJar_FindSurfaceWithinTexture(pPixelContainer, FaceType, Level,
    @(pLockedRect.pBits), @(pLockedRect.Pitch), @DummySlicePitch, @DummyFormat, @DummySize);
  EmuSwapFS(fsWindows);

  if Assigned(pRect) then
  begin
    Inc(UIntPtr(pLockedRect.pBits), pRect.Top * pLockedRect.Pitch);
    Inc(UIntPtr(pLockedRect.pBits), UInt(pRect.Left) * EmuXBFormatBPP(GetD3DFormat(pPixelContainer)) div 8);
  end;

  // Log the output for debugging purposes :
  if MayLog(lfUnit or lfReturnValue) then
    LogBegin('D3D_PixelJar_Lock2DSurface result').
      _(pLockedRect, 'pLockedRect'). // NOW dump contents!
    LogEnd();

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_PixelJar_Lock3DSurface
(
    pPixelContainer: PX_D3DPixelContainer;
    Level: UINT;
    pLockedBox: PD3DLOCKED_BOX;
    pBox: PD3DBOX;
    Flags: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
// This patch is needed to prevent the original code messing up the Data pointer.
// (This can happen if X_D3DLOCK_TILED is present in the Flags argument!)
var
  DummyFormat, DummySize: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3D_PixelJar_Lock3DSurface >').
      _(pPixelContainer, 'pPixelContainer').
      _(Level, 'Level').
      _(Pointer(pLockedBox), 'pLockedBox'). // Cast to prevent contents dump here
      _(pBox, 'pBox').
      _(Flags, 'Flags'). // Ignored
    LogEnd();

  EmuSwapFS(fsXbox);
  XTL_EmuD3D_PixelJar_FindSurfaceWithinTexture(pPixelContainer, D3DCUBEMAP_FACE_POSITIVE_X{=0}, Level,
    @(pLockedBox.pBits), @(pLockedBox.RowPitch), @(pLockedBox.SlicePitch), @DummyFormat, @DummySize);
  EmuSwapFS(fsWindows);

  if Assigned(pBox) then
  begin
    Inc(UIntPtr(pLockedBox.pBits), UInt(pBox.Front) * UInt(pLockedBox.SlicePitch));
    Inc(UIntPtr(pLockedBox.pBits), UInt(pBox.Top) * UInt(pLockedBox.RowPitch));
    Inc(UIntPtr(pLockedBox.pBits), UInt(pBox.Left) * EmuXBFormatBPP(GetD3DFormat(pPixelContainer)) div 8);
  end;

  // Log the output for debugging purposes :
  if MayLog(lfUnit or lfReturnValue) then
    LogBegin('D3D_PixelJar_Lock3DSurface result').
      _(pLockedBox, 'pLockedBox'). // NOW dump contents!
    LogEnd();

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

(*
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
*)

(* Too high level : We should just let the Xbox do it's own format checking!
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
*)

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
    LogBegin('Direct3D_GetAdapterModeCount').
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
    LogBegin('Direct3D_GetAdapterDisplayMode').
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
    // TODO -oDxbx: Retrieve from front buffer and CreateDevice parameters :
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
    LogBegin('Direct3D_EnumAdapterModes').
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

(* Too high level : We should just let the Xbox do it's own MultiSampleType checking!
function XTL_EmuDirect3D_CheckDeviceMultiSampleType
(
  Adapter: UINT;
  DeviceType: D3DDEVTYPE;
  SurfaceFormat: X_D3DFORMAT;
  Windowed: BOOL;
  MultiSampleType: X_D3DMULTISAMPLE_TYPE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

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
    LogBegin('Direct3D_SetPushBufferSize').
      _(PushBufferSize, 'PushBufferSize').
      _(KickOffSize, 'KickOffSize').
    LogEnd();

  Result := D3D_OK; // This is a Xbox extension, meaning there is no pc counterpart.

  EmuSwapFS(fsXbox);
end;

(*
function XTL_EmuD3DDevice_AddRef(): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DDevice_Release(): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

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

procedure XTL_EmuD3DDevice_GetCreationParameters
(
  pParameters: PD3DDEVICE_CREATION_PARAMETERS
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetCreationParameters').
      _(pParameters, 'pParameters').
    LogEnd();

  pParameters.AdapterOrdinal := D3DADAPTER_DEFAULT;
  pParameters.DeviceType := D3DDEVTYPE_HAL;
  pParameters.hFocusWindow := 0;
  pParameters.BehaviorFlags := D3DCREATE_HARDWARE_VERTEXPROCESSING;

  EmuSwapFS(fsXbox);
end; // XTL_EmuD3DDevice_GetCreationParameters

procedure XTL_EmuD3DDevice_GetDirect3D
(
  ppD3D8: XTL_PLPDIRECT3D8
); stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetDirect3D').
      _(ppD3D8, 'ppD3D8').
    LogEnd();

{$IFDEF DXBX_USE_D3D9}
  g_pD3DDevice.GetDirect3D({out}PIDirect3D9(ppD3D8));
{$ELSE}
  g_pD3DDevice.GetDirect3D({out}PIDirect3D8(ppD3D8));
{$ENDIF}

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
    LogBegin('D3DDevice_GetDepthClipPlanes').
      _(pNear, 'pNear').
      _(pFar, 'pFar').
      _(Flags, 'Flags').
    LogEnd();

//  g_pD3DDevice.GetDepthClipPlanes
  Unimplemented('XTL_EmuD3DDevice_GetDepthClipPlanes');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_GetDisplayFieldStatus
(
  pFieldStatus: PX_D3DFIELD_STATUS
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfExtreme) then
    LogBegin('D3DDevice_GetDisplayFieldStatus').
      _(pFieldStatus, 'pFieldStatus').
    LogEnd;

  // TODO -oDxbx: This flag is probably checked on the wrong struct (we need a global DisplayMode) :
  if (PX_D3DPRESENT_PARAMETERS(g_EmuCDPD.pPresentationParameters).Flags and X_D3DPRESENTFLAG_INTERLACED) > 0 then
    pFieldStatus.Field := X_D3DFIELDTYPE(iif(g_VBData.VBlankCounter and 1 = 0, Ord(X_D3DFIELD_ODD), Ord(X_D3DFIELD_EVEN)))
  else
    pFieldStatus.Field := X_D3DFIELD_PROGRESSIVE;

  pFieldStatus.VBlankCount := g_VBData.VBlankCounter;

  EmuSwapFS(fsXbox);
end; // XTL_EmuD3DDevice_GetDisplayFieldStatus

(*
procedure XTL_EmuD3DDevice_BeginPushBuffer
(
   pPushBuffer: PX_D3DPushBuffer
); stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_BeginPushBuffer').
      _(pPushBuffer, 'pPushBuffer').
    LogEnd();

  Unimplemented('EmuD3DDevice_BeginPushBuffer');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_EndPushBuffer(): HRESULT; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_EndPushBuffer();');

  Unimplemented('EmuD3DDevice_EndPushBuffer');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

procedure XTL_EmuD3DDevice_GetPushBufferOffset
(
  pOffset: PDWORD
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:1
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetPushBufferOffset').
      _(pOffset, 'pOffset').
    LogEnd();

//  D3DDevice_GetPushBufferOffset(pOffset);
  pOffset^ := 0; // TODO

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetPushDistance // XDK 4627+
(
  Handle_: DWORD
): Dword;
begin
  EmuSwapFs(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetPushDistance').
      _(Handle_, 'Handle').
    LogEnd();

  DxbxKrnlCleanup('XTL_EmuD3DDevice_GetPushDistance is not implemented');

  Result := D3D_OK;

  EmuSwapFs(fsXbox);
end;

function XTL_EmuD3DDevice_BeginPush_8 // Dxbx Note : Appears in XDK 4361 as _D3DDevice_BeginPush@8 (2 arguments)
(
  Count: DWORD;
  ppPush: PPDWORD
): PDWORD; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_BeginPush').
      _(Count, 'Count').
      _(Count, 'Count').
    LogEnd();

  // TODO -oDxbx : Speed this up by re-using a previously allocated memory block?

  Result := XboxCalloc(Count * sizeof(DWORD)); // Cxbx: new DWORD[Count]
  g_dwPrimaryPBCount := Count;
  g_pPrimaryPB := Result;

  EmuSwapFS(fsXbox);
end; // XTL_EmuD3DDevice_BeginPush_8

// Note : XDK 4627 seems to miss BeginPush, and has StartPush instead.

function XTL_EmuD3DDevice_BeginPush_4 // Dxbx Note : Appears in XDK 5558 onwards as _D3DDevice_BeginPush@4 (1 argument)
(
  Count: DWORD
): PDWORD; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  Dummy: Pointer;
begin
  // Dxbx note : Forward this to the 2 argument version with a dummy argument :
  Result := XTL_EmuD3DDevice_BeginPush_8(Count, @Dummy);
end; // XTL_EmuD3DDevice_BeginPush_4

procedure XTL_EmuD3DDevice_EndPush
(
  pPush: PDWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_EndPush').
      _(pPush, 'pPush').
    LogEnd();

//  XTL_EmuExecutePushBufferRaw(g_pPrimaryPB);

  XboxFree(g_pPrimaryPB); // Cxbc: delete[]
  g_pPrimaryPB := nil;

  EmuSwapFS(fsXbox);
end; // XTL_EmuD3DDevice_EndPush
*)

function XTL_EmuD3DDevice_BeginVisibilityTest(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('D3DDevice_BeginVisibilityTest').LogEnd();
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
      LogBegin('D3DDevice_EndVisibilityTest').
        _(Index, 'Index').
      LogEnd();

    EmuSwapFS(fsXbox);
  end;

  Result := D3D_OK;
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
    LogBegin('D3DDevice_GetVisibilityTestResult').
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

function XTL_EmuD3DDevice_GetBackBufferScale
(
  px: PFLOAT;
  py: PFLOAT
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetBackBufferScale').
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
    LogBegin('D3DDevice_SetBackBufferScale').
      _(X, 'x').
      _(Y, 'y').
    LogEnd();

  EmuWarning('SetBackBufferScale ignored');

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

(* Too high level : No patch needed, just copies g_DeviceCaps :
procedure XTL_EmuD3DDevice_GetDeviceCaps
(
    pCaps: PD3DCAPS
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

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
    LogBegin('D3DDevice_GetGammaRamp').
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
    LogBegin('D3DDevice_SetGammaRamp').
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

function XTL_EmuD3DDevice_CreateStateBlock
(
  Type_: D3DSTATEBLOCKTYPE;
  pToken: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_CreateStateBlock').
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
    EmuWarning('CreateStateBlock failed!'#13#10 + DxbxD3DErrorString(Result));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_DeleteStateBlock
(
  Token: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_DeleteStateBlock').
      _(Token, 'Token').
    LogEnd();

{$IFDEF DXBX_USE_D3D9}
  Result := IDirect3DStateBlock9(Token)._Release;
{$ELSE}
  Result := g_pD3DDevice.DeleteStateBlock(Token);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_BeginStateBlock(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_BeginStateBlock').LogEnd();

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
    LogBegin('D3DDevice_BeginStateBig').
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
    LogBegin('D3DDevice_CaptureStateBlock').
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
    LogBegin('D3DDevice_ApplyStateBlock').
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
    LogBegin('D3DDevice_EndStateBlock').
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
    LogBegin('D3DDevice_CopyRects').
      _(pSourceSurface, 'pSourceSurface').
      _(pSourceRectsArray, 'pSourceRectsArray').
      _(cRects, 'cRects').
      _(pDestinationSurface, 'pDestinationSurface').
      _(pDestPointsArray, 'pDestPointsArray').
    LogEnd();

  // Dxbx addition : Make sure the given surfaces are represented locally :
  DxbxUpdateNativeSurface(pSourceSurface, 0);
  DxbxUpdateNativeSurface(pDestinationSurface, 0);

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

(*
function XTL_EmuD3DDevice_CreateDepthStencilSurface
(
  Width: UINT;
  Height: UINT;
  Format: X_D3DFORMAT;
  MultiSample: X_D3DMULTISAMPLE_TYPE;
  ppSurface: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
*)

(*
function XTL_EmuD3DDevice_CreateImageSurface
(
  Width: UINT;
  Height: UINT;
  Format: X_D3DFORMAT;
  ppBackBuffer: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
*)

(*
function XTL_EmuD3DDevice_GetBackBuffer2
(
    BackBuffer: INT
): PX_D3DSurface; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
// Back buffers are numbered from 0 to the total number of back buffers - 1.
// A value of 0 returns the first back buffer, not the front buffer.
// The front buffer can be accessed through this method by passing in a value of '-1'.
*)

(*
procedure XTL_EmuD3DDevice_GetBackBuffer
(
  BackBuffer: INT;
  Type_: D3DBACKBUFFER_TYPE;
  ppBackBuffer: PPX_D3DSurface
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

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
    LogBegin('D3DDevice_GetViewportOffsetAndScale').
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

function XTL_EmuD3DDevice_GetViewport
(
  pViewport: PD3DVIEWPORT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetViewport').
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
    LogBegin('D3DDevice_SetViewport').
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
//    if (dwX < currentViewport.X) then
//    begin
//      EmuWarning('Moving Viewport->X to %d', [currentViewport.X]);
//      pViewport.X := currentViewport.X;
//    end;
//    if (dwY < currentViewport.Y) then
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

  // Before setting, make sure we have the correct render target :
  DxbxUpdateActiveRenderTarget(); // TODO : Or should we have to call DxbxUpdateNativeD3DResources ?

  Result := g_pD3DDevice.SetViewport(pViewport^);

  // restore originals
//  begin
//    if (dwX < currentViewport.X) then
//      pViewport.X := dwX;
//
//    if (dwY < currentViewport.Y) then
//      pViewport.Y := dwY;
//
//    if (dwWidth > currentViewport.Width) then
//      pViewport.Width := dwWidth;
//
//    if (dwHeight > currentViewport.Height) then
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

function XTL_EmuD3DDevice_SetShaderConstantMode
(
  Mode: X_VERTEXSHADERCONSTANTMODE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetShaderConstantMode').
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
    LogBegin('D3DDevice_Reset').
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
    LogBegin('D3DDevice_Resume').
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
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetRenderTarget').
      _(ppRenderTarget, 'ppRenderTarget').
    LogEnd();

  ppRenderTarget^ := g_EmuD3DActiveRenderTarget;
  if Assigned(ppRenderTarget^) then
  begin
    DxbxXboxResource_AddRef(ppRenderTarget^);
    Result := D3D_OK;
  end
  else
    Result := D3DERR_NOTFOUND;

  EmuSwapFS(fsXbox);
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

procedure XTL_EmuD3DDevice_SetRenderTarget
(
  pRenderTarget: PX_D3DSurface;
  pNewZStencil: PX_D3DSurface
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3DDevice_SetRenderTarget').
       _(pRenderTarget, 'pRenderTarget'). // If NULL, the existing color buffer is retained
       _(pNewZStencil, 'pNewZStencil').
    LogEnd();
  end;

  if Assigned(DxbxOnSetRenderTarget) then
  begin
    DxbxOnSetRenderTarget(pRenderTarget, pNewZStencil);
    DxbxOnSetRenderTarget := nil;
  end;

  begin // Handle render target :
    // If no change, work with current :
    if pRenderTarget = nil then
      pRenderTarget := g_EmuD3DActiveRenderTarget;

    // Do internal reference counting :
    if Assigned(pRenderTarget) then
      DxbxXboxResource_AddRef_Internal(pRenderTarget);
    if Assigned(g_EmuD3DActiveRenderTarget) then
      DxbxXboxResource_Release_Internal(g_EmuD3DActiveRenderTarget);

    // Set new :
    g_EmuD3DActiveRenderTarget := pRenderTarget;
  end;

  begin // Handle depth buffer in (almost) the same way :
    // Note : For depth buffer, DON'T work with current!

    // Do internal reference counting :
    if Assigned(pNewZStencil) then
      DxbxXboxResource_AddRef_Internal(pNewZStencil);
    if Assigned(g_EmuD3DActiveDepthStencil) then
      DxbxXboxResource_Release_Internal(g_EmuD3DActiveDepthStencil);

    // Set new :
    g_EmuD3DActiveDepthStencil := pNewZStencil;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetDepthStencilSurface
(
  ppZStencilSurface: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetDepthStencilSurface').
      _(ppZStencilSurface, 'ppZStencilSurface').
    LogEnd();

  ppZStencilSurface^ := g_EmuD3DActiveDepthStencil;
  if Assigned(ppZStencilSurface^) then
  begin
    DxbxXboxResource_AddRef(ppZStencilSurface^);
    Result := D3D_OK;
  end
  else
    Result := D3DERR_NOTFOUND;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetDepthStencilSurface2(): PX_D3DSurface; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuD3D8 : EmuD3DDevice_GetDepthStencilSurface2(); >>');
    EmuSwapFS(fsXbox);
  end;

  XTL_EmuD3DDevice_GetDepthStencilSurface(@Result);
end;

function XTL_EmuD3DDevice_SetTile
(
  Index: DWORD;
  {CONST} pTile: PX_D3DTILE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetTile').
      _(Index, 'Index').
      _(pTile, 'pTile').
    LogEnd();

  if (pTile = NULL) or (pTile.pMemory = NULL) then
    ZeroMemory(@(g_EmuD3DTileCache[Index]), sizeof(X_D3DTILE))
  else
    memcpy(@(g_EmuD3DTileCache[Index]), pTile, sizeof(X_D3DTILE));

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_GetTile
(
  Index: DWORD;
  {CONST} pTile: PX_D3DTILE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetTile').
      _(Index, 'Index').
      _(pTile, 'pTile').
    LogEnd();

  if (pTile <> NULL) then
    memcpy(pTile, @(g_EmuD3DTileCache[Index]), sizeof(X_D3DTILE));

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
    LogBegin('D3DDevice_SetTileCompressionTagBits').
      _(Partition, 'Partition').
      _(Address, 'Address').
      _(pData, 'pData').
      _(Count, 'Count').
    LogEnd();

  //D3DDevice_SetTileCompressionTagBits(Partition, Address, pData, Count);
  Unimplemented('XTL_EmuD3DDevice_SetTileCompressionTagBits');

  EmuSwapFS(fsXbox);
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
    LogBegin('D3DDevice_GetTileCompressionTagBits').
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
    LogBegin('D3DDevice_GetTileCompressionTags').
      _(ZStartTag, 'ZStartTag').
      _(ZEndTag, 'ZEndTag').
    LogEnd();

//  return D3DDevice_GetTileCompressionTags(ZStartTag, ZEndTag);
  Unimplemented('XTL_EmuD3DDevice_GetTileCompressionTags');
  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

(* Too high level : Just allocates a X_D3DPixelShader and copies pPSDef into it.
function XTL_EmuD3DDevice_CreatePixelShader
(
  pPSDef: PX_D3DPIXELSHADERDEF;
  pHandle: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
*)

(* Too high level : Just decreases given pixel shaders' ReferenceCount and frees the memory if zero
function XTL_EmuD3DDevice_DeletePixelShader
(
  Handle: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
*)

function XTL_EmuD3DDevice_GetPixelShader
(
  Value: PDWORD
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfPixelShader) then
    LogBegin('D3DDevice_GetPixelShader').
      _(Value, 'Value').
    LogEnd();

  Value^ := DWORD(g_EmuD3DActivePixelShader);

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetPixelShader
(
  Handle: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pPSDef: PX_D3DPIXELSHADERDEF;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfPixelShader) then
    LogBegin('D3DDevice_SetPixelShader').
      _(Handle, 'Handle').
    LogEnd();

  g_bFakePixelShaderLoaded := FALSE; // Dxbx fix : Always reset this

  // Dxbx addition : Delay the actual pixel shader recompiling until drawing time,
  // so we can support ModifyPixelShader and other dynamic pixel shader samples & games :

  // TODO : Add (internal) reference counting ?

  g_EmuD3DActivePixelShader := PX_D3DPixelShader(Handle);
  if Assigned(g_EmuD3DActivePixelShader) then
  begin
    // When we have an assigned pixel shader, try to copy it into D3D__RenderState,
    // just like the Xbox does (but without the push-buffer fillings, as we don't
    // support that yet - when we do, this entire patch can be removed):
    pPSDef := PX_D3DPIXELSHADERDEF(XTL_D3D__RenderState);
    if Assigned(pPSDef) then
      memcpy(pPSDef, g_EmuD3DActivePixelShader.PshDef, SizeOf(X_D3DPIXELSHADERDEF));
  end;

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

(* Too high level : No patch needed, just copies the given pixel shader to the supplied buffer.
function XTL_EmuD3DDevice_GetPixelShaderFunction
(
  Handle: DWORD;
  pData: PX_D3DPIXELSHADERDEF;
  pSizeOfData: PDWORD
): HRESULT; stdcall;
*)

var InternalPixelShader: X_D3DPixelShader;
// Dxbx note : This patch is not really needed, just points an internal X_D3DPixelShader to pPSDef, and calls SetPixelShader with that.
// As soon as we have original D3D initialization working, this patch can be removed (until then,
// this patch is needed, because unpatched it would access the uninitialized 'g_pDevice').
function XTL_EmuD3DDevice_SetPixelShaderProgram
(
  {CONST} pPSDef: PX_D3DPIXELSHADERDEF
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfPixelShader) then
    LogBegin('D3DDevice_SetPixelShaderProgram >>').
      _(pPSDef, 'pPSDef').
    LogEnd();

  if Assigned(pPSDef) then
  begin
    InternalPixelShader.RefCount := 1;
    InternalPixelShader.Flags := 0;
    InternalPixelShader.PshDef := pPSDef;
  end;

  EmuSwapFS(fsXbox);

  // Redirect the creation and activation to the other patches we already have :
  if Assigned(pPSDef) then
    Result := XTL_EmuD3DDevice_SetPixelShader(DWORD(@InternalPixelShader))
  else
    Result := XTL_EmuD3DDevice_SetPixelShader(0);
end;

function XTL_EmuD3DDevice_GetPixelShaderConstant
(
  Register_: DWORD;
  pConstantData: PD3DXColor;
  ConstantCount: DWORD
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfPixelShader) then
    LogBegin('D3DDevice_GetPixelShaderConstant').
      _(Register_, 'Register').
      _(pConstantData, 'pConstantData').
      _(ConstantCount, 'ConstantCount').
    LogEnd();

  while ConstantCount > 0 do
  begin
    // Colors are defined in RGBA format, and range 0.0 - 1.0 (negative values
    // can be obtained by supplying PS_INPUTMAPPING_SIGNED_NEGATE to the combiner
    // that reads from these constants).
    // Note : Just like the original Xbox, we read the values previously set
    // by SetPixelShaderConstant, and not the values in the render state slots!
    pConstantData^ := D3DXColorFromDWord(g_PixelShaderConstants[Register_]);

    Dec(ConstantCount);
    Inc(pConstantData);
    Inc(Register_);
  end;

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

// TODO -oDxbx : Once we have unpatched D3D initialization working so that the original 'g_pDevice' and
// the pushbuffer are initialized, then this patch can be removed (that is, IF we have a method to handle
// or ignore the contents of the push buffer).
function XTL_EmuD3DDevice_SetPixelShaderConstant
(
  Register_: DWORD;
  {CONST} pConstantData: PD3DXColor;
  ConstantCount: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pPSDef: PX_D3DPIXELSHADERDEF;
  dwColor: DWORD;
  i: int;
  MappedRegister: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfPixelShader) then
    LogBegin('D3DDevice_SetPixelShaderConstant').
      _(Register_, 'Register').
      _(pConstantData, 'pConstantData').
      _(ConstantCount, 'ConstantCount').
    LogEnd();

  // Get the active pixel shader definition (normally located at the start of the render state variables block) :
  pPSDef := PX_D3DPIXELSHADERDEF(XTL_D3D__RenderState);
  if pPSDef = nil then
  begin
    Assert(Assigned(g_EmuD3DActivePixelShader));
    pPSDef := @(g_EmuD3DActivePixelShader.PshDef);
  end;

  // Handle all constants :
  while ConstantCount > 0 do
  begin
    dwColor := D3DXColorToDWord(pConstantData^);

    // Remember the given color (so we can return this value in GetPixelShaderConstant) :
    g_PixelShaderConstants[Register_] := dwColor;

    // The Xbox1 NV2A graphics chip has 18 constant registers;
    // Two for each pixel shader stage and 2 for the final combiner stage.
    // These 18 constants are coupled directly to their corresponding stage
    // via the PSConstant0 and PSConstant1 arrays and PSFinalCombinerConstant0
    // and PSFinalCombinerConstant1 values.
    //
    // Even though these mappings are hardwired, shader code is likely written
    // with a different order of constants. To facilitate this, the Xbox D3D8
    // implementation has a feature called 'constant mapping'. For this, there
    // are 3 mappings appended to a shader : PSC0Mapping, PSC1Mapping (mapping
    // the two constants for all 8 combiner stages) and PSFinalCombinerConstants
    // (mapping the two constants for the final combiner stage).

{
    Note : On the XBox, pPSDef.PSC0Mapping / PSC1Mapping are only used
    to map indexes in SetPixelShaderConstant; The pixel shader itself just uses
    c0..c7 (read from PSConstant0[]), c8-c15 (read from PSConstant1[]) and for
    the final combiner it's c0 and c1 are read from PSFinalCombinerConstant0/1.

    // Example from TechCertGame :

    // Declaration :
    psd.PSC0Mapping = PS_CONSTANTMAPPING(15,15,15,15,15,15,0,2);  // = $20FFFFFF
    psd.PSC1Mapping = PS_CONSTANTMAPPING(15,15,15,15,15,15,1,15); // = $F1FFFFFF

    // Constant colors declared in shader :
    psd.PSConstant0[1]=0xc0c0c0c0; // not mapped
    psd.PSConstant0[6]=0xdfdfdfdf; // mapped to c0
    psd.PSConstant1[6]=0xdfdfdfdf; // mapped to c1
    psd.PSConstant0[7]=0x20202020; // mapped to c2

    // Usage during rendering :
    float fDiffuse[4] = ( 0.9f, 0.9f, 0.9f, 0.9f );
    float fSpecular[4] = ( 0.0f, 0.0f, 0.0f, 0.0f );
    float fAmbient[4] = ( 0.2f, 0.2f, 0.2f, 0.2f );
    g_pd3dDevice->SetPixelShaderConstant( 0, fDiffuse, 1 );
    g_pd3dDevice->SetPixelShaderConstant( 1, fSpecular, 1 );
    g_pd3dDevice->SetPixelShaderConstant( 2, fAmbient, 1 );

    // Decoding sample:
    ConstMapping[0] :=  (pPSDef.PSC0Mapping shr  0) and $F);
    ConstMapping[1] := ((pPSDef.PSC0Mapping shr  4) and $F);
    ConstMapping[2] := ((pPSDef.PSC0Mapping shr  8) and $F);
    ConstMapping[3] := ((pPSDef.PSC0Mapping shr 12) and $F); // ... etc

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
}

    // Here, we make sure that these mappings are honoured :

    // Handle the C0 mappings for all 8 combiner stages :
    for i := 0 to 8 - 1 do
    begin
      // For each stage, determine what constant offset is used :
      MappedRegister := (pPSDef.PSC0Mapping shr (i * 4)) and $F;
      if MappedRegister = Register_ then
        // If that constant is now being set, put it in the correct render state slot :
        XTL_EmuMappedD3DRenderState[X_D3DRS_PSCONSTANT0_0 + i]^ := dwColor;

      // Handle the C1 mappings in the same way :
      MappedRegister := (pPSDef.PSC1Mapping shr (i * 4)) and $F;
      if MappedRegister = Register_ then
        XTL_EmuMappedD3DRenderState[X_D3DRS_PSCONSTANT1_0 + i]^ := dwColor;
    end;

    // Also the c0 and c1 mappings for the final combiner stage :
    for i := 0 to 2 - 1 do
    begin
      MappedRegister := (pPSDef.PSFinalCombinerConstants shr (i * 4)) and $F;
      if MappedRegister = Register_ then
        XTL_EmuMappedD3DRenderState[X_D3DRS_PSFINALCOMBINERCONSTANT0 + i]^ := dwColor;
    end;

    Dec(ConstantCount);
    Inc(pConstantData);
    Inc(Register_);
  end;

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

(*
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
*)

(*
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
*)

(*
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
*)

(*
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
*)

(*
function XTL_EmuD3DDevice_CreateIndexBuffer
(
  Length: UINT;
  Usage: DWORD;
  Format: X_D3DFORMAT;
  Pool: X_D3DPOOL;
  ppIndexBuffer: PPX_D3DIndexBuffer
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DDevice_CreateIndexBuffer2(Length: UINT): PX_D3DIndexBuffer; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
procedure XTL_EmuD3DDevice_SetIndices
(
  pIndexBuffer: PX_D3DIndexBuffer;
  BaseVertexIndex: UINT
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
*)

(*
procedure XTL_EmuD3DDevice_GetIndices
(
  ppIndexBuffer: PPX_D3DIndexBuffer;
  pBaseVertexIndex: PUINT
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
*)

procedure XTL_EmuD3DDevice_GetTexture
(
  dwStage: DWORD;
  ppTexture: PPX_D3DBaseTexture
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetTexture').
      _(dwStage, 'dwStage').
      _(ppTexture, 'ppTexture').
    LogEnd();

  Assert(Assigned(ppTexture));
  Assert(dwStage < X_D3DTS_STAGECOUNT);

  ppTexture^ := g_EmuD3DActiveTexture[dwStage];
  // According to the XDK documentation, this function should also add
  // to the reference count.
  if Assigned(ppTexture^) then
    DxbxXboxResource_AddRef(ppTexture^);

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
    LogBegin('D3DDevice_GetTexture2 >> ').
      _(dwStage, 'Stage').
    LogEnd();

  EmuSwapFS(fsXbox);

  XTL_EmuD3DDevice_GetTexture(dwStage, @Result);
end;

procedure XTL_EmuD3DDevice_SetTexture
(
    Stage: DWORD;
    pTexture: PX_D3DBaseTexture
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetTexture').
      _(Stage, 'Stage').
      _(pTexture, 'pTexture').
    LogEnd();

  // Do internal reference counting :
  if Assigned(pTexture) then
    DxbxXboxResource_AddRef_Internal(pTexture);
  if Assigned(g_EmuD3DActiveTexture[Stage]) then
    DxbxXboxResource_Release_Internal(g_EmuD3DActiveTexture[Stage]);

  g_EmuD3DActiveTexture[Stage] := pTexture;

(*
  if Assigned(pTexture) {and (pTexture = g_pActiveOverlaySurface)} then
  begin
    // NOTE: TODO -oCXBX: This is almost a hack!
    EmuSwapFS(fsXbox);
    XTL_EmuD3DDevice_EnableOverlay(BOOL_TRUE);
    XTL_EmuD3DDevice_UpdateOverlay(PX_D3DSurface(pTexture), nil, nil, BOOL_FALSE, 0);
    EmuSwapFS(fsWindows);
  end;
*)

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
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SwitchTexture >>').
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
    pTexture := nil;
    EmuWarning('Unknown Method (0x%.08X)', [Method]);
  end
  else
  begin
    // WARNING: TODO -oCXBX: Correct reference counting has not been completely verified for this code
    pTexture := g_DataToTexture.get(Data);
    EmuWarning('Switching Texture %s @ Stage %d', [ResourceToString(pTexture), Stage]);
  end;

  EmuSwapFS(fsXbox);

  if Assigned(pTexture) then
    XTL_EmuD3DDevice_SetTexture(Stage, pTexture);
end;

(* Too high level
function XTL_EmuD3DDevice_GetDisplayMode
(
  pMode: PX_D3DDISPLAYMODE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
*)

function XTL_EmuD3DDevice_Begin
(
    PrimitiveType: X_D3DPRIMITIVETYPE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_Begin').
      _(xtD3DPRIMITIVETYPE, DWORD(PrimitiveType), 'PrimitiveType').
    LogEnd();

  g_IVBPrimitiveType := PrimitiveType;
  g_IVBFVF := 0;
  g_IVBTblOffs := 0;

  if Length(g_IVBTable) > 0 then
    ZeroMemory(@g_IVBTable[0], Length(g_IVBTable) * SizeOf(g_IVBTable[0]));

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
    LogBegin('D3DDevice_SetVertexData2f').
      _(xtD3DVSDE, DWORD(Register_), 'Register').
      _(a, 'a').
      _(b, 'b').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  // TODO -oDxbx : Handle Vertex Attributes that need a Color (in this case, r,g,b,a=0.0-1.0)
  Result := DxbxSetVertexData(Register_, a, b, 0.0, 1.0);
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
    LogBegin('D3DDevice_SetVertexData2s').
      _(xtD3DVSDE, DWORD(Register_), 'Register').
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
    LogBegin('D3DDevice_SetVertexData4f').
      _(xtD3DVSDE, DWORD(Register_), 'Register').
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
    LogBegin('D3DDevice_SetVertexData4ub').
      _(xtD3DVSDE, DWORD(Register_), 'Register').
      _(a, 'a').
      _(b, 'b').
      _(c, 'c').
      _(d, 'd').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  fA := a; fB := b; fC := c; fD := d;

// TODO -oDxbx : When and where should we do this? No XDK sample calls this...
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
    LogBegin('D3DDevice_SetVertexData4s').
      _(xtD3DVSDE, DWORD(Register_), 'Register').
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
    LogBegin('D3DDevice_SetVertexDataColor').
      _(xtD3DVSDE, DWORD(Register_), 'Register').
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

(*
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

function XTL_EmuD3DDevice_RunPushBuffer
(
    pPushBuffer: PX_D3DPushBuffer;
    pFixup: PX_D3DFixup
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_RunPushBuffer').
      _(pPushBuffer, 'pPushBuffer').
      _(pFixup, 'pFixup').
    LogEnd();

//  XTL_EmuExecutePushBuffer(pPushBuffer, pFixup);
  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;
*)

procedure XTL_EmuD3DDevice_Clear
(
    Count: DWORD;
    pRects: PD3DRECT;
    Flags: DWORD;
    Color: D3DCOLOR;
    Z: float;
    Stencil: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  PCFlags: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_Clear').
        _(Count, 'Count').
        _(pRects, 'pRects').
        _(xtD3DCLEAR, Flags, 'Flags').
        _(Color, 'Color').
        _(Z, 'Z').
        _(Stencil, 'Stencil').
    LogEnd();

  // make adjustments to parameters to make sense with windows d3d
  begin
    // First, convert from Xbox to PC, after which we'll remove the invalid flags :
    PCFlags := EmuXB2PC_D3DCLEAR_FLAGS(Flags);

    // Only clear ZBuffer if we actually have one :
    if Assigned(g_EmuD3DActiveDepthStencil)
    and EmuXBFormatIsDepthBuffer(GetD3DFormat(g_EmuD3DActiveDepthStencil)) then
      // Allow depth to be cleared (if requested)
    else
      PCFlags := PCFlags and (not D3DCLEAR_ZBUFFER);

    // Only clear Stencil buffer if there actually is one :
    // if not g_EmuCDPD.NativePresentationParameters.EnableAutoDepthStencil then
    // TODO -oDxbx: The above check should work (but doesn't!) so for now look at the Xbox PresParam instead :
    if Assigned(g_EmuD3DActiveDepthStencil)
    and EmuXBFormatHasChannel(GetD3DFormat(g_EmuD3DActiveDepthStencil), S) then
      // Allow stencil to be cleared (if requested)
    else
      PCFlags := PCFlags and (not D3DCLEAR_STENCIL);
  end;

  // Since we filter the flags, make sure there are some left (else, clear isn't necessary) :
  if PCFlags > 0 then
  begin
    // Before clearing, make sure we have the correct render target :
    DxbxUpdateActiveRenderTarget(); // TODO : Or should we have to call DxbxUpdateNativeD3DResources ?

    g_pD3DDevice.Clear(Count, pRects, PCFlags, Color, Z, Stencil)
  end;

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
    LogBegin('D3DDevice_Present').
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

  g_bOverlayUpdated := True;
  g_bHackUpdateSoftwareOverlay := FALSE;

  DxbxDelayedOverlayHide;

  EmuSwapFS(fsXbox);

  Result := hRet;
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
    LogBegin('D3DDevice_IsFencePending').
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
    LogBegin('D3DDevice_BlockOnFence').
      _(Fence, 'Fence').
    LogEnd();

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);
end;

(* Too high level : No patch needed, just calls KickOffAndWaitForIdle :
procedure XTL_EmuD3DDevice_BlockUntilIdle(); stdcall;
// Branch:dxbx  Translator:Shadow_Tj  Done:100
*)

procedure XTL_EmuD3DDevice_BlockUntilVerticalBlank(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuD3D8 : EmuD3DDevice_BlockUntilVerticalBlank();');

  // - DXBX - DO NOT ENABLE GetVSync CHECK... ALMOST EVERY GAME CRASHES WHEN YOU DO NOT WAIT !!!
  // segaGT tends to freeze with this on
  //    if (g_XBVideo.GetVSync())
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
    LogBegin('D3DDevice_SetVerticalBlankCallback').
      _(Addr(pCallback), 'pCallback').
    LogEnd();

  if Addr(pCallback) <> Null then
    g_pVBCallback := pCallback
  else
    // Dxbx note : Zapper passes the Handle of a previously created thread here... wierd!
    EmuWarning('SetVerticalBlankCallback ignored invalid Callback address');

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
    LogBegin('D3DDevice_SetSwapCallback').
      _(PPointer(@pCallback)^, 'pCallback').
    LogEnd();
  end;

  g_pSwapCallback := pCallback;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_Swap
(
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_Swap >>').
      _(Flags, 'Flags').
    LogEnd();

  // TODO -oCXBX: Ensure this flag is always the same across library versions
  if (Flags <> X_D3DSWAP_DEFAULT) then
    EmuWarning('XTL.EmuD3DDevice_Swap: Flags <> X_D3DSWAP_DEFAULT');

  EmuSwapFS(fsXbox);

  // Forward to Present
  Result := XTL_EmuD3DDevice_Present(nil, nil, 0, nil);
end;

(*
function XTL_EmuD3DDevice_CreateSurface
(
  pDDSurfaceDesc: PX_D3DSURFACE_DESC;
  {out}lplpDDSurface: XTL_PIDirectDrawSurface7;
  pUnkOuter: IUnknown
): HRESULT; stdcall;
// Branch:DXBX  Translator:PatrickvL  Done:1
*)

(*
function XTL_EmuD3DDevice_CreateSurface2(
  Width: DWORD;
  Height: DWORD;
  Usage: DWORD;
  Format: X_D3DFORMAT
): PX_D3DSurface; stdcall;
// Branch:DXBX  Translator:PatrickvL  Done:50
*)

(*
function XTL_EmuD3DDevice_CreateVertexBuffer
(
  Length: UINT;
  Usage: DWORD;
  FVF: DWORD;
  Pool: X_D3DPOOL;
  ppVertexBuffer: PPX_D3DVertexBuffer
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DDevice_CreateVertexBuffer2
(
  Length: UINT
): PX_D3DVertexBuffer; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

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
    LogBegin('D3DDevice_EnableOverlay').
      _(Enable, 'Enable').
    LogEnd();

  // initialize overlay surface
  if (g_bYUY2OverlaysSupported) then
  begin
    if (g_pDDSOverlay7 = NULL) then
    begin
      ZeroMemory(@ddsd2, sizeof(ddsd2));

      ddsd2.dwSize := sizeof(ddsd2);
      ddsd2.dwFlags := DDSD_CAPS or DDSD_WIDTH or DDSD_HEIGHT or DDSD_PIXELFORMAT;
      ddsd2.ddsCaps.dwCaps := DDSCAPS_OVERLAY;
      ddsd2.dwWidth := 640; // TODO : Use actual frontbuffer width
      ddsd2.dwHeight := 480; // TODO : Use actual frontbuffer height

      ddsd2.ddpfPixelFormat.dwSize := sizeof(DDPIXELFORMAT);
      ddsd2.ddpfPixelFormat.dwFlags := DDPF_FOURCC;
      // Dxbx note : Xbox overlays are always using YUY2 format, so use that for our overlay surface too :
      ddsd2.ddpfPixelFormat.dwFourCC := DWORD(D3DFMT_YUY2); //=MAKEFOURCC('Y', 'U', 'Y', '2');

      hRet := IDirectDraw7(g_pDD7).CreateSurface(ddsd2, @g_pDDSOverlay7, NULL);
      if (FAILED(hRet)) then
        DxbxD3DError('EmuD3DDevice_EnableOverlay', 'Could not create overlay surface', nil, hRet);

      hRet := IDirectDraw7(g_pDD7).CreateClipper(0, {out}IDirectDrawClipper(g_pDDClipper7), NULL);
      if (FAILED(hRet)) then
        DxbxD3DError('EmuD3DDevice_EnableOverlay', 'Could not create overlay clipper', nil, hRet);
    end;

    if Enable = BOOL_FALSE then
      // Don't set g_OverlayHideDelay := 1 or hardware overlay will flicker again! (test with Virtual Pool)
    else
      g_OverlayHideDelay := 2;

(*
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
*)
  end;

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_UpdateOverlay
(
  pSurface: PX_D3DSurface;
  pSrcRect: PRECT;
  pDstRect: PRECT;
  EnableColorKey: BOOL;
  ColorKey: D3DCOLOR
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
const
  FIXED_POINT_FACTOR = (1 shl 16);
var
  hRet: HRESULT;

  // Hardware overlay variables :
  ddsd2: DDSURFACEDESC2;
  pDest: PByte;
  DxbxPixelJar: RDxbxDecodedPixelContainer;
  pSour: PByte;
  dwUpdateFlags: DWORD;

  // Software overlay variables :
  BackBufferSurface: IDirect3DSurface;
  SurfaceDesc: D3DSURFACE_DESC;
  CanWriteToBackbuffer: Boolean;
  OverlayBufferSurface: IDirect3DSurface;
  LockedRectDest: D3DLOCKED_RECT;
  pYUY2Input: Puint08;
  pRGBOutput: Puint08;
  dwImageSize: uint32;
  dx: int32;
  v: UInt32;

  // Software YUY2 > RGB conversion variables :
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
    LogBegin('D3DDevice_UpdateOverlay').
      _(pSurface, 'pSurface').
      _(pSrcRect, 'pSrcRect').
      _(pDstRect, 'pDstRect').
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
    g_bOverlayUpdated := False;

    // manually copy data over to overlay
    if (g_bYUY2OverlaysSupported) then
    begin
      ZeroMemory(@ddsd2, sizeof(ddsd2));
      ddsd2.dwSize := sizeof(ddsd2);
      hRet := IDirectDrawSurface7(g_pDDSOverlay7).Lock(NULL, {out}ddsd2, DDLOCK_SURFACEMEMORYPTR or DDLOCK_WAIT, 0);
      if FAILED(hRet) then
        EmuWarning('Unable to lock overlay surface!'#13#10 + DxbxD3DErrorString(hRet));

      // copy data
      begin
        pDest := PByte(ddsd2.lpSurface);

        DxbxGetFormatRelatedVariables(pSurface, {out}DxbxPixelJar);
        pSour := DxbxGetDataFromXboxResource(pSurface);

        DxbxPitchedCopy(
          pDest, pSour,
          {dwDestPitch=}ddsd2.lPitch, {dwSrcPitch=}DxbxPixelJar.dwRowPitch,
          {dwWidthInBytes=}DxbxPixelJar.dwWidth * 2, {dwHeight=}DxbxPixelJar.dwHeight);
      end;

      IDirectDrawSurface7(g_pDDSOverlay7).Unlock(NULL);

      // update overlay!
      g_OverlayHideDelay := 2;

      dwUpdateFlags := DDOVER_SHOW;

      if (EnableColorKey = BOOL_TRUE) and ((ddcaps.dwCKeyCaps and DDCKEYCAPS_SRCOVERLAY) > 0) then
        dwUpdateFlags := dwUpdateFlags or DDOVER_KEYSRCOVERRIDE;

      DxbxUpdateOverlay(dwUpdateFlags, pSrcRect, ColorKey);
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
        CanWriteToBackbuffer := (pDstRect.Left = pSrcRect.Left)
                            and (pDstRect.Right = pSrcRect.Right)
                            and (pDstRect.Top = pSrcRect.Top)
                            and (pDstRect.Bottom = pSrcRect.Bottom)
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
        if (OverlayBufferSurface.LockRect({out}LockedRectDest, pDstRect, 0) = D3D_OK) then
        begin
          // Determine the start of the Xbox overlay buffer and Native destination buffer :
          pYUY2Input := DxbxGetDataFromXboxResource(pSurface);
          pRGBOutput := Puint08(LockedRectDest.pBits);

          // We're going to convert YUY2 to xRGB, calculate how many bytes that is :
          dwImageSize := pSrcRect.Right * pSrcRect.Bottom * 2; // YUY2 uses 2 bytes per pixel
          dx := 0;
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

            Y0U0Y1V0 := PDWORD(pYUY2Input+v)^; Inc(v, 4);

            // full color conversion (YUY2->XRGB)
            Y0 := Byte(Y0U0Y1V0       ) * FIXED_POINT_FACTOR;
            Y1 := Byte(Y0U0Y1V0 shr 16) * FIXED_POINT_FACTOR;
            V0 := Byte(Y0U0Y1V0 shr 24) - 128;
            U0 := Byte(Y0U0Y1V0 shr  8) - 128;

            R :=                                        (Round(1.402*FIXED_POINT_FACTOR)*V0);
            G := (Round(0.344*FIXED_POINT_FACTOR)*U0) + (Round(0.714*FIXED_POINT_FACTOR)*V0);
            B := (Round(1.772*FIXED_POINT_FACTOR)*U0);

            pRGBOutput[dx+0] := ClampIntToByte((Y0 + B) div FIXED_POINT_FACTOR);
            pRGBOutput[dx+1] := ClampIntToByte((Y0 - G) div FIXED_POINT_FACTOR);
            pRGBOutput[dx+2] := ClampIntToByte((Y0 + R) div FIXED_POINT_FACTOR);
            pRGBOutput[dx+3] := $FF;

            pRGBOutput[dx+4] := ClampIntToByte((Y1 + B) div FIXED_POINT_FACTOR);
            pRGBOutput[dx+5] := ClampIntToByte((Y1 - G) div FIXED_POINT_FACTOR);
            pRGBOutput[dx+6] := ClampIntToByte((Y1 + R) div FIXED_POINT_FACTOR);
            pRGBOutput[dx+7] := $FF;

            // Since we wrote 2 destination pixels, 4 bytes each, step x 8 bytes :
            Inc(dx, 8);
            // Check if we reached the end of this scanline :
            if ((dx div 4) >= pSrcRect.Right) then
            begin
              // Step to the start of the next scanline :
              pRGBOutput:= @pRGBOutput[LockedRectDest.Pitch];
              dx := 0;
            end;
          end; // while

          OverlayBufferSurface.UnlockRect();

          if not CanWriteToBackbuffer then
          begin
            // When the overlay could not directly be converted into the back buffer,
            // we now have to stretch-copy there (this also does a format-conversion, if needed) :
            if D3DXLoadSurfaceFromSurface(
              {pDestSurface=}BackBufferSurface,
              {pDestPalette=}NULL, // Palette not needed for YUY2
              pDstRect,
              {pSrcSurface=}OverlayBufferSurface,
              {pSrcPalette=}NULL, // Palette not needed for YUY2
              pSrcRect,
              {Filter=}D3DX_FILTER_POINT, // Dxbx note : D3DX_FILTER_LINEAR gives a smoother image, but 'bleeds' across borders
              {ColorKey=}ColorKey) <> D3D_OK then
              DbgPrintf('EmuD3D8 : UpdateOverlay could not convert buffer!');
          end;
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

  Result := g_bOverlayUpdated;
end;

procedure XTL_EmuD3DDevice_EnableCC
(
  Enable: BOOL
); stdcall;
begin
end;

procedure XTL_EmuD3DDevice_SendCC
(
  Field: BOOL_;
  cc1: BYTE;
  cc2: BYTE
); stdcall;
begin
end;

procedure XTL_EmuD3DDevice_GetCCStatus
(
  pField1: PBOOL;
  pField2: PBOOL
); stdcall;
begin
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
    LogBegin('D3DDevice_GetTextureStageState').
      _(Stage, 'Stage').
      _(DWORD(Type_), 'Type').
      _(xtD3DTEXTURESTAGESTATETYPE, Type_VersionIndependent, 'Type_VersionIndependent').
      _(pValue, 'pValue').
    LogEnd();

  if Assigned(PValue) then
  begin
    // Check if this is an Xbox extension  :
    if DxbxTextureStageStateInfo[Type_VersionIndependent].X then
      PValue^ := XTL_EmuD3DDeferredTextureState[Stage, Ord(Type_)]
    else
      IDirect3DDevice_GetTextureStageState(g_pD3DDevice, Stage, Type_VersionIndependent, {out}pValue^);
  end;

  Result := D3D_OK;

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
    LogBegin('D3DDevice_SetTextureState_TexCoordIndex').
      _(Stage, 'Stage').
      _(DxbxTextureStageStateInfo[X_D3DTSS_TEXCOORDINDEX].T, Value, 'Value').
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

procedure XTL_EmuD3DDevice_SetTextureState_BorderColor
(
  Stage: DWORD;
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetTextureState_BorderColor').
      _(Stage, 'Stage').
      _(DxbxTextureStageStateInfo[X_D3DTSS_BORDERCOLOR].T, Value, 'Value').
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
    LogBegin('D3DDevice_SetTextureState_ColorKeyColor').
      _(Stage, 'Stage').
      _(DxbxTextureStageStateInfo[X_D3DTSS_COLORKEYCOLOR].T, Value, 'Value').
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
    LogBegin('D3DDevice_SetTextureState_ParameterCheck').
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
    LogBegin('D3DDevice_SetTextureState_BumpEnv').
      _(Stage, 'Stage').
      _(DWORD(Type_), 'Type').
      _(xtD3DTEXTURESTAGESTATETYPE, Type_VersionIndependent, 'Type_VersionIndependent').
      _(DxbxTextureStageStateInfo[Type_VersionIndependent].T, Value, 'Value').
    LogEnd();

  // Dxbx addition : Set this value into the TextureState structure too (so other code will read the new current value)
  XTL_EmuD3DDeferredTextureState[Stage, Ord(Type_)] := Value;
  // TODO -oDxbx : Update the D3D DirtyFlags too?

  // Dxbx Note : The BumpEnv values don't need a XB2PC conversion
  // Dxbx Note : The BumpEnv types all have a PC counterpart (so no -1 test needed)

  IDirect3DDevice_SetTextureStageState(g_pD3DDevice, Stage, Type_VersionIndependent, F2DW(DW2F(Value) / 3));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetTextureStageStateNotInline
(
  Stage: DWORD;
  Type_: X_D3DTEXTURESTAGESTATETYPE;
  Value: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
const
  NoForwardFunctionStr: array [Boolean] of string = ({False=}'D3DDevice_SetTextureStageStateNotInline >>', {True=}'D3DDevice_SetTextureStageStateNotInline');
var
  Type_VersionIndependent: X_D3DTEXTURESTAGESTATETYPE;
begin
  EmuSwapFS(fsWindows);

  Type_VersionIndependent := DxbxFromOldVersion_D3DTSS(Type_);
  if MayLog(lfUnit) then
    LogBegin(NoForwardFunctionStr[Type_VersionIndependent in [X_D3DTSS_DEFERRED_FIRST..X_D3DTSS_DEFERRED_LAST]]).
      _(Stage, 'Stage').
      _(DWORD(Type_), 'Type').
      _(xtD3DTEXTURESTAGESTATETYPE, Type_VersionIndependent, 'Type_VersionIndependent').
      _(DxbxTextureStageStateInfo[Type_VersionIndependent].T, Value, 'Value').
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
    LogBegin('D3DDevice_GetRenderState').
      _(Int(State), 'State').
      _(pValue, 'pValue').
    LogEnd();

  if Assigned(PValue) then
  begin
    State_VersionIndependent := DxbxVersionAdjust_D3DRS(State);

    // Check if this is an Xbox extension  :
    if DxbxRenderStateInfo[State_VersionIndependent].PC = D3DRS_UNSUPPORTED then // Xbox extensions have no PC state
      PValue^ := XTL_EmuMappedD3DRenderState[State_VersionIndependent]^
    else
      g_pD3DDevice.GetRenderState(DxbxRenderStateInfo[State_VersionIndependent].PC, {out}PValue^);
  end;

  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_TwoSidedLighting
(
  Value: DWORD
); stdcall;
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetTextureState_TwoSidedLighting',
    X_D3DRS_TWOSIDEDLIGHTING, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_BackFillMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // blueshogun96 12/4/07
  // I haven't had access to Cxbx sources in a few months, great to be back :)
  //
  // Anyway, since standard Direct3D doesn't support the back fill mode
  // operation, this function will be ignored.  Things like this make me
  // think even more that an OpenGL port wouldn't hurt since OpenGL supports
  // nearly all of the missing features that Direct3D lacks.  The Xbox's version
  // of Direct3D was specifically created to take advantage of certain NVIDIA
  // GPU registers and provide more OpenGL-like features IHMO.

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_BackFillMode',
    X_D3DRS_BACKFILLMODE, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_FrontFace
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_FrontFace',
    X_D3DRS_FRONTFACE, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_LogicOp
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_LogicOp',
    X_D3DRS_LOGICOP, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_NormalizeNormals
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_NormalizeNormals',
    X_D3DRS_NORMALIZENORMALS, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_TextureFactor
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_TextureFactor',
    X_D3DRS_TEXTUREFACTOR, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_ZBias
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
   'EmuD3DDevice_SetRenderState_ZBias',
   X_D3DRS_ZBIAS, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_EdgeAntiAlias
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_EdgeAntiAlias',
    X_D3DRS_EDGEANTIALIAS, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_FillMode
(
  Value: X_D3DFILLMODE
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_FillMode',
    X_D3DRS_FILLMODE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_FogColor
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_FogColor',
    X_D3DRS_FOGCOLOR, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_Dxt1NoiseEnable
(
  Value: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_Dxt1NoiseEnable',
    X_D3DRS_DXT1NOISEENABLE, Value);

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

  XboxRenderState := DxbxXboxMethodToRenderState(Method);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetRenderState_Simple').
      _(Method, 'Method').
      _(DxbxRenderStateInfo[XboxRenderState].T, DWORD(Value), 'Value').
    LogEnd();

  // Dxbx note : Methods are already version-independant
  if (int(XboxRenderState) = -1) then
    EmuWarning('SetRenderState_Simple({Method=}0x%.08X, {Value=}0x%.08X) - unsupported method!', [Method, Value])
  else
    // Use a helper for the simple render states, as SetRenderStateNotInline
    // needs to be able to call it too :
    DxbxSetRenderStateInternal('', XboxRenderState, {Xbox}Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_VertexBlend
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_VertexBlend',
    X_D3DRS_VERTEXBLEND, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_PSTextureModes
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_PSTextureModes',
    X_D3DRS_PSTEXTUREMODES, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_CullMode
(
  Value: X_D3DCULL
); stdcall;
// Branch:shogun  Revision:162  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_CullMode',
    X_D3DRS_CULLMODE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_LineWidth
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_LineWidth',
    X_D3DRS_LINEWIDTH, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_StencilFail
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_StencilFail',
    X_D3DRS_STENCILFAIL, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_OcclusionCullEnable
(
  Value: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_OcclusionCullEnable',
    X_D3DRS_OCCLUSIONCULLENABLE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_StencilCullEnable
(
  Value: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_StencilCullEnable',
    X_D3DRS_STENCILCULLENABLE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_RopZCmpAlwaysRead
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_RopZCmpAlwaysRead',
    X_D3DRS_ROPZCMPALWAYSREAD, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_RopZRead
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_RopZRead',
    X_D3DRS_ROPZREAD, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_DoNotCullUncompressed
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_DoNotCullUncompressed',
    X_D3DRS_DONOTCULLUNCOMPRESSED, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_ZEnable
(
  Value: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuIDirect3DDevice_SetRenderState_ZEnable',
    X_D3DRS_ZENABLE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_StencilEnable
(
  Value: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_StencilEnable',
    X_D3DRS_STENCILENABLE, DWORD(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_MultiSampleAntiAlias
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_MultiSampleAntiAlias',
    X_D3DRS_MULTISAMPLEANTIALIAS, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_MultiSampleMask
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_MultiSampleMask',
    X_D3DRS_MULTISAMPLEMASK, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_SetRenderState_MultiSampleMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pRenderTarget: PX_D3DSurface;
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_MultiSampleMode',
    X_D3DRS_MULTISAMPLEMODE, Value);

  EmuSwapFS(fsXbox);

  pRenderTarget := g_EmuD3DActiveRenderTarget;
  if (pRenderTarget = g_EmuD3DFrameBuffers[0]) then
    XTL_EmuD3DDevice_SetRenderTarget(pRenderTarget, g_EmuD3DActiveDepthStencil);
end;

procedure XTL_EmuD3DDevice_SetRenderState_MultiSampleRenderTargetMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pRenderTarget: PX_D3DSurface;
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_MultiSampleRenderTargetMode',
    X_D3DRS_MULTISAMPLERENDERTARGETMODE, Value);

  EmuSwapFS(fsXbox);

  pRenderTarget := g_EmuD3DActiveRenderTarget;
  if (pRenderTarget <> g_EmuD3DFrameBuffers[0]) then
    XTL_EmuD3DDevice_SetRenderTarget(pRenderTarget, g_EmuD3DActiveDepthStencil);
end;

procedure XTL_EmuD3DDevice_SetRenderState_ShadowFunc
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetRenderState_ShadowFunc').
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
    LogBegin('D3DDevice_SetRenderState_YuvEnable').
      _(Value, 'Value').
    LogEnd();

  // Dxbx addition : Set this value into the RenderState structure too (so other code will read the new current value)
  XTL_EmuMappedD3DRenderState[X_D3DRS_YUVENABLE]^ := Value;

(*
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
*)

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_SetRenderState_SampleAlpha
(
  Value: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  DxbxSetRenderStateInternal(
    'EmuD3DDevice_SetRenderState_SampleAlpha',
    X_D3DRS_SAMPLEALPHA, Value);

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

  XboxRenderState_VersionIndependent := DxbxVersionAdjust_D3DRS(State);
  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetRenderState_SetRenderStateNotInline >>').
      _(State, 'State', DxbxRenderStateInfo[XboxRenderState_VersionIndependent].S).
      _(DxbxRenderStateInfo[XboxRenderState_VersionIndependent].T, DWORD(Value), 'Value').
    LogEnd();

  Result := D3D_OK;
  if (XboxRenderState_VersionIndependent <= X_D3DRS_SIMPLE_LAST) then
  begin
    // Pixel & Simple render states - Just pass them on to our helper :
    DxbxSetRenderStateInternal('', XboxRenderState_VersionIndependent, Value);
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

function XTL_EmuD3DDevice_GetTransform
(
  State: X_D3DTRANSFORMSTATETYPE;
  pMatrix: PD3DMATRIX
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetTransform').
      _(xtD3DTRANSFORMSTATETYPE, DWORD(State), 'State').
      _(pMatrix, 'pMatrix').
    LogEnd();

  Result := g_pD3DDevice.GetTransform(EmuXB2PC_D3DTS(State), {out}pMatrix^);

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
//  PCMatrix: D3DMATRIX;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetTransform').
      _(xtD3DTRANSFORMSTATETYPE, DWORD(State), 'State').
      _(pMatrix, 'pMatrix').
    LogEnd();

(* Dxbx attempt to fix Smashing Drive fog problems on ATI (no effect) :
  if State = X_D3DTS_PROJECTION then
  begin
    // Dxbx addition : Make sure we work with "A W-Friendly Projection Matrix" :
    if  (pMatrix._34 <> 1.0)
    and (pMatrix._34 <> 0.0) then
    begin
      ZeroMemory(@PCMatrix, SizeOf(PCMatrix));
      PCMatrix._11 := pMatrix._11 / pMatrix._34;
      PCMatrix._22 := pMatrix._22 / pMatrix._34;
      PCMatrix._33 := pMatrix._33 / pMatrix._34;
      PCMatrix._43 := pMatrix._43 / pMatrix._34;
      PCMatrix._34 := 1.0;
      pMatrix := @PCMatrix;
      DbgPrintf('Guaranteed "A W-Friendly Projection Matrix"');
    end;
  end;
(*)

  (* Commented by DXBX
  if State = X_D3DTS_TEXTURE2 then // Dxbx test to see if chessboard texture actually moves in ModifyPixelShader (it does, although invisible)
  begin
    DbgPrintf('pMatrix (%d)', [Ord(State)]);
    DbgPrintf('{');
    DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._11, pMatrix._12, pMatrix._13, pMatrix._14]);
    DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._21, pMatrix._22, pMatrix._23, pMatrix._24]);
    DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._31, pMatrix._32, pMatrix._33, pMatrix._34]);
    DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._41, pMatrix._42, pMatrix._43, pMatrix._44]);
    DbgPrintf('}');
  end;
  *)

  PCState := EmuXB2PC_D3DTS(State);

  Result := g_pD3DDevice.SetTransform(PCState, pMatrix{$IFDEF DXBX_USE_D3D9}^{$ENDIF});

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_GetStreamSource
(
  StreamNumber: UINT;
  ppVertexBuffer: PPX_D3DVertexBuffer;
  pStride: PUINT
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  pVertexBuffer: PX_D3DVertexBuffer;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('D3DDevice_GetStreamSource').
      _(StreamNumber, 'StreamNumber').
      _(ppVertexBuffer, 'ppVertexBuffer').
      _(pStride, 'pStride').
    LogEnd();

  pVertexBuffer := g_EmuD3DActiveStreams[StreamNumber];
  if Assigned(pVertexBuffer) then
  begin
    ppVertexBuffer^ := pVertexBuffer;
    DxbxXboxResource_AddRef(pVertexBuffer);
    pStride^ := g_EmuD3DActiveStreamStrides[StreamNumber];
  end
  else
  begin
    ppVertexBuffer^ := nil;
    pStride^ := 0;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetStreamSource2
(
  StreamNumber: UINT;
  pStride: PUINT
): PX_D3DVertexBuffer; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('D3DDevice_GetStreamSource2 >> ').
      _(StreamNumber, 'StreamNumber').
      _(pStride, 'pStride').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  XTL_EmuD3DDevice_GetStreamSource(StreamNumber, @Result, pStride);
end;

procedure XTL_EmuD3DDevice_SetStreamSource
(
  StreamNumber: UINT;
  pStreamData: PX_D3DVertexBuffer;
  Stride: UINT
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3DDevice_SetStreamSource').
      _(StreamNumber, 'StreamNumber').
      _(pStreamData, 'pStreamData').
      _(Stride, 'Stride').
    LogEnd();
  end;

  Assert(StreamNumber < MAX_NBR_STREAMS);

  // Do internal reference counting :
  if Assigned(pStreamData) then
    DxbxXboxResource_AddRef_Internal(pStreamData);
  if Assigned(g_EmuD3DActiveStreams[StreamNumber]) then
    DxbxXboxResource_Release_Internal(g_EmuD3DActiveStreams[StreamNumber]);

  g_EmuD3DActiveStreams[StreamNumber] := pStreamData;
  g_EmuD3DActiveStreamStrides[StreamNumber] := Stride;

  EmuSwapFS(fsXbox);
end;

{$ENDIF !PUSHBUFFER_ONLY}

function QuadToTriangleVertexCount(NrOfQuadVertices: UINT): UINT;
begin
  Result := (NrOfQuadVertices * VERTICES_PER_TRIANGLE * TRIANGLES_PER_QUAD) div VERTICES_PER_QUAD;
end;

var
  WindingClockwise: Boolean = True;
  QuadToTriangleIndexBuffer_Size: UINT = 0; // = NrOfQuadVertices
  QuadToTriangleIndexBuffer: array of Word = nil;

var
  QuadToTriangleD3DIndexBuffer_Size: UINT = 0; // = NrOfQuadVertices
  QuadToTriangleD3DIndexBuffer: XTL_PIDirect3DIndexBuffer8 = nil;

procedure DxbxDebugSwitchQuadIndexWinding;
begin
  WindingClockwise := not WindingClockwise;

  SetLength(QuadToTriangleIndexBuffer, 0);
  QuadToTriangleIndexBuffer_Size := 0;
  QuadToTriangleD3DIndexBuffer_Size := 0;
  if Assigned(QuadToTriangleD3DIndexBuffer) then
    IDirect3DIndexBuffer(QuadToTriangleD3DIndexBuffer) := nil; // Note : This does a implicit _Release() call!
end;

function DxbxAssureQuadListIndexBuffer(NrOfQuadVertices: UINT): PWORD;
var
  NrOfTriangleVertices: UINT;
  i, j: UINT;
begin
  if QuadToTriangleIndexBuffer_Size < NrOfQuadVertices then
  begin
    QuadToTriangleIndexBuffer_Size := RoundUp(NrOfQuadVertices, 1000);

    NrOfTriangleVertices := QuadToTriangleVertexCount(QuadToTriangleIndexBuffer_Size);
    SetLength(QuadToTriangleIndexBuffer, NrOfTriangleVertices);

    i := 0;
    j := 0;
    while i < NrOfTriangleVertices do
    begin
      if WindingClockwise then
      begin
        // ABCD becomes ABC+CDA, so this is triangle 1 :
        QuadToTriangleIndexBuffer[i+0] := j+0;
        QuadToTriangleIndexBuffer[i+1] := j+1;
        QuadToTriangleIndexBuffer[i+2] := j+2;
        Inc(i, VERTICES_PER_TRIANGLE);

        // And this is triangle 2 :
        QuadToTriangleIndexBuffer[i+0] := j+2;
        QuadToTriangleIndexBuffer[i+1] := j+3;
        QuadToTriangleIndexBuffer[i+2] := j+0;
        Inc(i, VERTICES_PER_TRIANGLE);
      end
      else
      begin
        // ABCD becomes ADC+CBA, so this is triangle 1 :
        QuadToTriangleIndexBuffer[i+0] := j+0;
        QuadToTriangleIndexBuffer[i+1] := j+3;
        QuadToTriangleIndexBuffer[i+2] := j+2;
        Inc(i, VERTICES_PER_TRIANGLE);

        // And this is triangle 2 :
        QuadToTriangleIndexBuffer[i+0] := j+2;
        QuadToTriangleIndexBuffer[i+1] := j+1;
        QuadToTriangleIndexBuffer[i+2] := j+0;
        Inc(i, VERTICES_PER_TRIANGLE);
      end;

      // Next quad, please :
      Inc(j, VERTICES_PER_QUAD);
    end;
  end;

  Result := @(QuadToTriangleIndexBuffer[0]);
end;

procedure DxbxAssureQuadListD3DIndexBuffer(NrOfQuadVertices: UINT);
var
  hRet: HRESULT;
  NrOfTriangleVertices: UINT;
  pwData: PWord;
begin
  if QuadToTriangleD3DIndexBuffer_Size < NrOfQuadVertices then
  begin
    QuadToTriangleD3DIndexBuffer_Size := RoundUp(NrOfQuadVertices, 1000);
    if Assigned(QuadToTriangleD3DIndexBuffer) then
      IDirect3DIndexBuffer(QuadToTriangleD3DIndexBuffer)._Release();

    // Create a new native index buffer of the above determined size :
    NrOfTriangleVertices := QuadToTriangleVertexCount(QuadToTriangleD3DIndexBuffer_Size);
    hRet := IDirect3DDevice_CreateIndexBuffer(g_pD3DDevice,
      NrOfTriangleVertices * SizeOf(Word),
      D3DUSAGE_WRITEONLY,
      D3DFMT_INDEX16,
      D3DPOOL_MANAGED,
      @QuadToTriangleD3DIndexBuffer);
    if FAILED(hRet) then
      DxbxD3DError('DxbxAssureQuadListD3DIndexBuffer', 'IndexBuffer Create Failed!', nil, hRet);

    // Put quadlist-to-triangle-list index mappings into this buffer :
    pwData := nil;
    IDirect3DIndexBuffer(QuadToTriangleD3DIndexBuffer).Lock(0, 0, {out}TLockData(pwData), 0);
    if (pwData = nil) then
      DxbxD3DError('DxbxAssureQuadListD3DIndexBuffer', 'Could not lock index buffer!');

    memcpy({dest=}pwData, DxbxAssureQuadListIndexBuffer(NrOfQuadVertices), NrOfTriangleVertices * SizeOf(WORD));

    IDirect3DIndexBuffer(QuadToTriangleD3DIndexBuffer).Unlock();
  end;

  // Activate the new native index buffer :
  hRet := g_pD3DDevice.SetIndices(IDirect3DIndexBuffer(QuadToTriangleD3DIndexBuffer){$IFDEF DXBX_USE_D3D9}{$MESSAGE 'fixme'}{$ELSE}, {BaseVertexIndex=}0{$ENDIF});
  if (FAILED(hRet)) then
    DxbxKrnlCleanup('DxbxAssureQuadListD3DIndexBuffer : SetIndices Failed!'#13#10 + DxbxD3DErrorString(hRet));
end;

{$IFNDEF PUSHBUFFER_ONLY}

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
    LogBegin('D3DDevice_DrawVertices').
      _(xtD3DPRIMITIVETYPE, DWORD(PrimitiveType), 'PrimitiveType').
      _(StartVertex, 'StartVertex').
      _(VertexCount, 'VertexCount').
    LogEnd();

  // Note : In DrawVertices and DrawIndexedVertices, PrimitiveType may not be D3DPT_POLYGON

  DxbxUpdateNativeD3DResources();

  VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer
  VPDesc.PrimitiveType := PrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
  VPDesc.pVertexStreamZeroData := nil;
  VPDesc.uiVertexStreamZeroStride := 0;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

  if IsValidCurrentShader() then
  begin
    VertexCount := VPDesc.dwVertexCount; // Dxbx addition : Use the new VertexCount

    if (VPDesc.PrimitiveType = X_D3DPT_QUADLIST) then
    begin
      // Draw quadlists using a single 'quad-to-triangle mapping' index buffer :

      // Assure & activate that special index buffer :
      DxbxAssureQuadListD3DIndexBuffer({NrOfQuadVertices=}VertexCount);

      // Convert quad vertex-count & start to triangle vertex count & start :
      VertexCount := QuadToTriangleVertexCount(VertexCount);
      StartVertex := QuadToTriangleVertexCount(StartVertex);

      g_pD3DDevice.DrawIndexedPrimitive
      (
        D3DPT_TRIANGLELIST,
        {$IFDEF DXBX_USE_D3D9}{BaseVertexIndex=}0,{$ENDIF}
        {MinVertexIndex=}0,
        {NumVertices=}VertexCount,
        StartVertex,
        {primCount=}VPDesc.dwPrimitiveCount * TRIANGLES_PER_QUAD
      );
    end
    else
    begin
      // Other primitives than X_D3DPT_QUADLIST can be drawn normally :
      g_pD3DDevice.DrawPrimitive
      (
        EmuXB2PC_D3DPrimitiveType(VPDesc.PrimitiveType),
        StartVertex,
        VPDesc.dwPrimitiveCount
      );

      // Close line-loops using a separate DrawPrimitiveUP call :
      if (VPDesc.PrimitiveType = X_D3DPT_LINELOOP) then
      begin
        DxbxKrnlCleanup('XTL_EmuD3DDevice_DrawVertices : X_D3DPT_LINELOOP not unsupported yet!');
        // TODO : Close line-loops using a final single line, drawn from the end to the start vertex
        (*
        // TODO : Copy the last and first vertices into a separate buffer :
        memcpy({dest=}@(DxbxClosingLineVertices[0]),
               {src=}ActiveVertexBuffer.Data[StartVertex * ActiveVertexBuffer.Stride],
               ActiveVertexBuffer.Stride);
        // Append a second copy of the first vertex to the end, completing the strip to form a loop :
        memcpy({dest=}@(DxbxClosingLineVertices[0]),
               {src=}ActiveVertexBuffer.Data[(StartVertex + VertexCount) * ActiveVertexBuffer.Stride],
               ActiveVertexBuffer.Stride);
        g_pD3DDevice.DrawPrimitiveUP
        (
            D3DPT_LINELIST,
            {PrimitiveCount=}1,
            {pVertexStreamZeroData=}@DxbxClosingLineVertices[0],
            ActiveVertexBuffer.Stride
        );
        *)
      end;
    end;
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
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_DrawVerticesUP').
      _(xtD3DPRIMITIVETYPE, DWORD(PrimitiveType), 'PrimitiveType').
      _(VertexCount, 'VertexCount').
      _(pVertexStreamZeroData, 'pVertexStreamZeroData').
      _(VertexStreamZeroStride, 'VertexStreamZeroStride').
    LogEnd();

  DxbxUpdateNativeD3DResources();

  VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer
  VPDesc.PrimitiveType := PrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
  VPDesc.pVertexStreamZeroData := pVertexStreamZeroData;
  VPDesc.uiVertexStreamZeroStride := VertexStreamZeroStride;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

  if (IsValidCurrentShader()) then
    DxbxDrawPrimitiveUP(VPDesc);

  VertPatch.Restore();

  EmuSwapFS(fsXbox);
end;

{$ENDIF !PUSHBUFFER_ONLY}

var
  DxbxClosingLineVertices: array[0..256-1] of Byte;

procedure DxbxDrawPrimitiveUP(const VPDesc: VertexPatchDesc);
var
  VertexCount: UINT;
  pwIndexData: PWORD;
  pVertexStreamZeroData: Pointer;
begin
  VertexCount := VPDesc.dwVertexCount; // Dxbx addition : Use the new VertexCount

  if (VPDesc.PrimitiveType = X_D3DPT_QUADLIST) then
  begin
    // Draw quadlists using a single 'quad-to-triangle mapping' index buffer :

    // Assure & activate that special index buffer :
    pwIndexData := DxbxAssureQuadListIndexBuffer({NrOfQuadVertices=}VertexCount);

    // Convert quad vertex-count & start to triangle vertex count & start :
    VertexCount := QuadToTriangleVertexCount(VertexCount);

    g_pD3DDevice.DrawIndexedPrimitiveUP
    (
      D3DPT_TRIANGLELIST,
      {MinVertexIndex=}0,
      {NumVertexIndices=}VertexCount,
      {PrimitiveCount=}VPDesc.dwPrimitiveCount * TRIANGLES_PER_QUAD,
      pwIndexData,
      D3DFMT_INDEX16,
      VPDesc.pVertexStreamZeroData,
      VPDesc.uiVertexStreamZeroStride
    );
  end
  else
  begin
    // Other primitives than X_D3DPT_QUADLIST can be drawn normally :
    g_pD3DDevice.DrawPrimitiveUP
    (
      EmuXB2PC_D3DPrimitiveType(VPDesc.PrimitiveType),
      VPDesc.dwPrimitiveCount,
      VPDesc.pVertexStreamZeroData,
      VPDesc.uiVertexStreamZeroStride
    );

    if (VPDesc.PrimitiveType = X_D3DPT_LINELOOP) then
    begin
      // Note : XDK samples reaching this case : DebugKeyboard, Gamepad, Tiling, ShadowBuffer

      // Close line-loops using a final single line, drawn from the end to the start vertex :
      memcpy({dest=}@(DxbxClosingLineVertices[0]),
             {src=}VPDesc.pVertexStreamZeroData,
             VPDesc.uiVertexStreamZeroStride);
      memcpy({dest=}@(DxbxClosingLineVertices[VPDesc.uiVertexStreamZeroStride]),
             {src=}(PByte(VPDesc.pVertexStreamZeroData) + (VPDesc.uiVertexStreamZeroStride * (VertexCount-1))),
             VPDesc.uiVertexStreamZeroStride);

      pVertexStreamZeroData := @DxbxClosingLineVertices[0]; // Needed for D3D9
      g_pD3DDevice.DrawPrimitiveUP
      (
        D3DPT_LINELIST,
        {PrimitiveCount=}1,
        pVertexStreamZeroData,
        VPDesc.uiVertexStreamZeroStride
      );
    end;
  end;
end; // DxbxDrawPrimitiveUP

{$IFNDEF PUSHBUFFER_ONLY}

//D3DMINLINE HRESULT WINAPI::DrawIndexedPrimitive(D3DPRIMITIVETYPE PrimitiveType, UINT MinIndex, UINT NumIndices, UINT StartIndex, UINT PrimitiveCount)
// { D3DDevice_DrawIndexedVertices(PrimitiveType, D3DVERTEXCOUNT(PrimitiveType, PrimitiveCount), D3D__IndexData + StartIndex); return S_OK; }
procedure XTL_EmuD3DDevice_DrawIndexedVertices
(
  PrimitiveType: X_D3DPRIMITIVETYPE;
  VertexCount: UINT;
  pIndexData: PWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  uiStartIndex: UINT;
  VPDesc: VertexPatchDesc;
  VertPatch: VertexPatcher;
  FatalError: _bool;
  hRet: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_DrawIndexedVertices').
      _(xtD3DPRIMITIVETYPE, DWORD(PrimitiveType), 'PrimitiveType').
      _(VertexCount, 'VertexCount').
      _(pIndexData, 'pIndexData').
    LogEnd();

  // Note : In DrawVertices and DrawIndexedVertices, PrimitiveType may not be D3DPT_POLYGON

  DxbxUpdateNativeD3DResources();

  DxbxUpdateActiveIndexBuffer(pIndexData, VertexCount, {out}uiStartIndex);
  DbgPrintf('uiStartIndex = %d', [uiStartIndex]);

  VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer
  VPDesc.PrimitiveType := PrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
  VPDesc.pVertexStreamZeroData := nil;
  VPDesc.uiVertexStreamZeroStride := 0;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer
  FatalError := false;
  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, @FatalError);

  if (IsValidCurrentShader()) and (not FatalError) then
  begin
    VertexCount := VPDesc.dwVertexCount; // Dxbx addition : Use the new VertexCount

    if (VPDesc.PrimitiveType = X_D3DPT_QUADLIST) then
    begin
      // Indexed quadlist can be drawn using unpatched indexes via multiple draws of 2 'strip' triangles :
      // 4 vertices are just enough for two triangles (a fan starts with 3 vertices for 1 triangle,
      // and adds 1 triangle via 1 additional vertex)
      // This is slower (because of call-overhead) but doesn't require any index buffer patching at all!
      // Note : XDK samples reaching this case are : DisplacementMap, Ripple
      while Integer(VertexCount) >= VERTICES_PER_QUAD do
      begin
        g_pD3DDevice.DrawIndexedPrimitive
        (
          D3DPT_TRIANGLEFAN, // Draw a triangle-fan instead of a quad
          {$IFDEF DXBX_USE_D3D9}{BaseVertexIndex=}0,{$ENDIF}
          {MinVertexIndex=}0,
          {NumVertices=}VERTICES_PER_QUAD, // Use all 4 vertices of 1 quad
          uiStartIndex,
          {primCount=}TRIANGLES_PER_QUAD // Draw 2 triangles with that
        );
        Inc(uiStartIndex, VERTICES_PER_QUAD);

        Dec(VertexCount, VERTICES_PER_QUAD);
      end;
    end
    else
    begin
      // Other primitives than X_D3DPT_QUADLIST can be drawn normally :
      hRet := g_pD3DDevice.DrawIndexedPrimitive
      (
        EmuXB2PC_D3DPrimitiveType(VPDesc.PrimitiveType),
        {$IFDEF DXBX_USE_D3D9}{BaseVertexIndex=}0,{$ENDIF}
        {MinVertexIndex=}0,
        {NumVertices=}g_EmuD3DActiveStreamSizes[0], // Note : ATI drivers are especially picky about this -
        // NumVertices should be the span of covered vertices in the active vertex buffer (TODO : Is stream 0 correct?)
        uiStartIndex,
        VPDesc.dwPrimitiveCount
      );

      if FAILED(hRet) then
        EmuWarning('DrawIndexedPrimitive failed!'#13#10 + DxbxD3DErrorString(hRet));

      if (VPDesc.PrimitiveType = X_D3DPT_LINELOOP) then
      begin
        DxbxKrnlCleanup('XTL_EmuD3DDevice_DrawIndexedVertices : X_D3DPT_LINELOOP not unsupported yet!');
        // TODO : Close line-loops using a final single line, drawn from the end to the start vertex
      end;
    end;
  end;


  VertPatch.Restore();

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_DrawIndexedVerticesUP
(
  PrimitiveType: X_D3DPRIMITIVETYPE;
  VertexCount: UINT;
  pIndexData: PWORD;
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
    LogBegin('D3DDevice_DrawIndexedVerticesUP').
      _(xtD3DPRIMITIVETYPE, DWORD(PrimitiveType), 'PrimitiveType').
      _(VertexCount, 'VertexCount').
      _(pIndexData, 'pIndexData').
      _(pVertexStreamZeroData, 'pVertexStreamZeroData').
      _(VertexStreamZeroStride, 'VertexStreamZeroStride').
    LogEnd();

  DxbxUpdateNativeD3DResources();

  VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer
  VPDesc.PrimitiveType := PrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
  VPDesc.pVertexStreamZeroData := pVertexStreamZeroData;
  VPDesc.uiVertexStreamZeroStride := VertexStreamZeroStride;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, NULL);

  if (IsValidCurrentShader()) then
  begin
    VertexCount := VPDesc.dwVertexCount; // Dxbx addition : Use the new VertexCount

    if (VPDesc.PrimitiveType = X_D3DPT_QUADLIST) then
    begin
      // Indexed quadlist can be drawn using unpatched indexes via multiple draws of 2 'strip' triangles :
      // This is slower (because of call-overhead) but doesn't require any index buffer patching at all!
      // Draw quadlists using a single 'quad-to-triangle mapping' index buffer :
      while Integer(VertexCount) >= VERTICES_PER_QUAD do
      begin
        g_pD3DDevice.DrawIndexedPrimitiveUP
        (
          D3DPT_TRIANGLEFAN,
          {MinVertexIndex=}0,
          {NumVertexIndices=}VERTICES_PER_QUAD,
          {PrimitiveCount=}TRIANGLES_PER_QUAD,
          pIndexData,
          D3DFMT_INDEX16,
          VPDesc.pVertexStreamZeroData,
          VPDesc.uiVertexStreamZeroStride
        );
        Inc(pIndexData, VERTICES_PER_QUAD);

        DxbxKrnlCleanup('XTL_EmuD3DDevice_DrawIndexedVerticesUP : X_D3DPT_QUADLIST support untested - please tell PatrickvL which executable comes here!');
        // TODO : Is this shift necessary?: Inc(PBYTE(VPDesc.pVertexStreamZeroData), VERTICES_PER_QUAD * VPDesc.uiVertexStreamZeroStride);

        Dec(VertexCount, VERTICES_PER_QUAD);
      end;
    end
    else
    begin
      // Other primitives than X_D3DPT_QUADLIST can be drawn normally :
      g_pD3DDevice.DrawIndexedPrimitiveUP
      (
        EmuXB2PC_D3DPrimitiveType(VPDesc.PrimitiveType),
        {MinVertexIndex=}0,
        VertexCount,
        VPDesc.dwPrimitiveCount,
        pIndexData,
        D3DFMT_INDEX16,
        VPDesc.pVertexStreamZeroData,
        VPDesc.uiVertexStreamZeroStride
      );

      if (VPDesc.PrimitiveType = X_D3DPT_LINELOOP) then
      begin
        DxbxKrnlCleanup('XTL_EmuD3DDevice_DrawIndexedVerticesUP : X_D3DPT_LINELOOP support untested - please tell PatrickvL which executable comes here!');

        // Close line-loops using a final single line, drawn from the end to the start vertex :
        memcpy({dest=}@(DxbxClosingLineVertices[0]),
               {src=}(PByte(VPDesc.pVertexStreamZeroData) + (VPDesc.uiVertexStreamZeroStride * (pIndexData[VertexCount-1]))),
               VPDesc.uiVertexStreamZeroStride);
        memcpy({dest=}@(DxbxClosingLineVertices[VPDesc.uiVertexStreamZeroStride]),
               {src=}(PByte(VPDesc.pVertexStreamZeroData) + (VPDesc.uiVertexStreamZeroStride * (pIndexData[0]))),
               VPDesc.uiVertexStreamZeroStride);

        VPDesc.pVertexStreamZeroData := @DxbxClosingLineVertices[0]; // Needed for D3D9
        g_pD3DDevice.DrawPrimitiveUP
        (
          D3DPT_LINELIST,
          {PrimitiveCount=}1,
          VPDesc.pVertexStreamZeroData,
          VPDesc.uiVertexStreamZeroStride
        );
      end;
    end;
  end;

  VertPatch.Restore();

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
    LogBegin('D3DDevice_DrawRectPatch').
      _(Handle, 'Handle').
      _(pNumSegs^, 'pNumSegs').
      _(pRectPatchInfo, 'pRectPatchInfo').
    LogEnd();

  DxbxUpdateNativeD3DResources();

  Result := g_pD3DDevice.DrawRectPatch(Handle, PSingle(pNumSegs), pRectPatchInfo);

  if (FAILED(Result)) then
    EmuWarning('DrawRectPatch failed!'#13#10 + DxbxD3DErrorString(Result));

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
    LogBegin('D3DDevice_DrawTriPatch').
      _(Handle, 'Handle').
      _(pNumSegs^, 'pNumSegs').
      _(pTriPatchInfo, 'pTriPatchInfo').
    LogEnd();

  DxbxUpdateNativeD3DResources();

  Result := g_pD3DDevice.DrawTriPatch(Handle, PSingle(pNumSegs), pTriPatchInfo);

  if (FAILED(Result)) then
    EmuWarning('DrawTriPatch failed!'#13#10 + DxbxD3DErrorString(Result));

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
    LogBegin('D3DDevice_LightEnable').
      _(Index, 'Index').
      _(bEnable, 'bEnable').
    LogEnd();

  Result := g_pD3DDevice.LightEnable(Index, bEnable <> BOOL_FALSE);
  EmuSwapFS(fsXbox);
end;

(* Too high level : Just allocates a palette resource and the palette itself
 function XTL_EmuD3DDevice_CreatePalette
(
  Size: X_D3DPALETTESIZE;
  ppPalette: PPX_D3DPalette
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100

function XTL_EmuD3DDevice_CreatePalette2
(
  Size: X_D3DPALETTESIZE
): PX_D3DPalette; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

function XTL_EmuD3DDevice_SetPalette
(
  Stage: DWORD;
  pPalette: PX_D3DPalette
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetPalette').
      _(Stage, 'Stage').
      _(pPalette, 'pPalette').
    LogEnd();

  g_EmuD3DActivePalette[Stage] := pPalette;
  if Assigned(pPalette) then
    g_pCurrentPalette := PD3DCOLOR(DxbxGetDataFromXboxResource(pPalette))
  else
    g_pCurrentPalette := nil;

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
    LogBegin('D3DDevice_SetCopyRectsState').
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
    LogBegin('D3DDevice_SetFlickerFilter').
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
    LogBegin('D3DDevice_SetSoftDisplayFilter').
      _(Enable, 'Enable').
    LogEnd();

  EmuWarning('Not setting soft display filter');

  EmuSwapFS(fsXbox);
end;

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
  pRecompiledDeclaration: PVertexShaderDeclaration;
  pRecompiledFunction: PDWORD;
  VertexShaderSize: DWORD;
  DeclarationSize: DWORD;
  Handle: DWORD;

const
  dummy: P_char =
    'vs.1.1'#13#10 +
    'mov oPos, v0'#13#10;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_CreateVertexShader').
      _(pDeclaration, 'pDeclaration').
      _(pFunction, 'pFunction').
      _(pHandle, 'pHandle').
      _(Usage, 'Usage').
    LogEnd();

  // create emulated shader struct
  pD3DVertexShader := XboxCalloc(SizeOf(X_D3DVertexShader));

  pVertexShader := PVERTEX_SHADER(XboxCalloc(sizeof(VERTEX_SHADER)));
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

  // TODO : Merge below into RecompileVertexShader(pDeclaration, pFunction): VSH_RECOMPILED_SHADER = record{...};

  hRet := XTL_EmuRecompileVshDeclaration(PDWORD(pDeclaration),
                                         @pRecompiledDeclaration,
                                         @DeclarationSize,
                                         (pFunction = NULL),
                                         @pVertexShader.VertexDynamicPatch);
  if (SUCCEEDED(hRet) and Assigned(pFunction)) then
  begin
    hRet := XTL_EmuRecompileVshFunction(PDWORD(pFunction),
                                        pRecompiledDeclaration,
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
    // Dxbx addition : Only fallback if a shader is given (NULL is valid input)!
    // (Test this with the CompressedVertices and Patch XDK samples.)
    if (pRecompiledFunction = NULL) and (pFunction <> NULL) then
    begin
      // Dxbx Note : As suggested by StrikerX3
      hRet := D3DERR_INVALIDCALL; // Use fallback if recompilation failed
    end
    else
    begin
{$IFDEF DXBX_USE_D3D9}
      hRet := g_pD3DDevice.CreateVertexDeclaration
      (
        {pVertexElements=}PD3DVertexElement9(pRecompiledDeclaration),
        {ppDecl=}PIDirect3DVertexDeclaration9(@pVertexShader.hRecompiledDeclaration)
      );
      if SUCCEEDED(hRet) then
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
//          {ppDecl=}PIDirect3DVertexDeclaration9(@pVertexShader.hRecompiledDeclaration)
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
    //hRet := D3D_OK;    // marked out cxbx
  end;

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuD3D8 : CreateVertexShader: Successfully Created shader : Handle=0x%.08X (VertexShader=0x%.08X)', [pHandle^, pVertexShader]);

  EmuSwapFS(fsXbox);

  Result := hRet;
end; // XTL_EmuD3DDevice_CreateVertexShader

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
    LogBegin('D3DDevice_DeleteVertexShader').
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

{$IFDEF DXBX_USE_D3D9}
    IDirect3DVertexDeclaration9(pVertexShader.hRecompiledDeclaration)._Release();
{$ENDIF}
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
    LogBegin('D3DDevice_LoadVertexShader').
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

procedure XTL_EmuD3DDevice_LoadVertexShaderProgram
(
  {CONST} pFunction: PDWORD;
  Address: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('D3DDevice_LoadVertexShaderProgram').
      _(pFunction, 'pFunction').
      _(Address, 'Address').
    LogEnd();

  Unimplemented('EmuD3DDevice_LoadVertexShaderProgram');

  EmuSwapFS(fsXbox);
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
    LogBegin('D3DDevice_SelectVertexShader').
      _(Handle, 'Handle').
      _(Address, 'Address').
    LogEnd();

  if (VshHandleIsVertexShader(Handle)) then
  begin
    pVertexShader := PVERTEX_SHADER(VshHandleGetVertexShader(Handle).Handle);
{$IFDEF DXBX_USE_D3D9}
    g_pD3DDevice.SetVertexDeclaration(IDirect3DVertexDeclaration9(pVertexShader.hRecompiledDeclaration));
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
      g_pD3DDevice.SetVertexDeclaration(IDirect3DVertexDeclaration9(PVERTEX_SHADER(pVertexShader2.Handle).hRecompiledDeclaration));
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
    LogBegin('D3DDevice_GetVertexShaderSize').
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

procedure XTL_EmuD3DDevice_SelectVertexShaderDirect
(
  pVAF: PX_VERTEXATTRIBUTEFORMAT;
  Address: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('D3DDevice_SelectVertexShaderDirect').
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

    LogBegin('D3DDevice_GetShaderConstantMode').
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
    LogBegin('D3DDevice_GetVertexShader').
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

procedure XTL_EmuD3DDevice_SetVertexShader
(
  Handle: DWORD
); stdcall;
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
    LogBegin('D3DDevice_SetVertexShader').
      _(Handle, 'Handle').
    LogEnd();

  if not Assigned(DxbxOnSetRenderTarget) then // Skip this implementation when we've not yet done CreateDevice yet!
  begin
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
      g_pD3DDevice.SetVertexDeclaration(IDirect3DVertexDeclaration9(pVertexShader.hRecompiledDeclaration));
      g_pD3DDevice.SetVertexShader(IDirect3DVertexShader(pVertexShader.Handle));
  {$ELSE}
      g_pD3DDevice.SetVertexShader(pVertexShader.Handle);
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
      g_pD3DDevice.SetFVF(RealHandle);
  {$ELSE}
      g_pD3DDevice.SetVertexShader(RealHandle);
  {$ENDIF}
    end;
  end;

  EmuSwapFS(fsXbox);
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
    LogBegin('D3DDevice_GetVertexShaderType').
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
    LogBegin('D3DDevice_GetVertexShaderDeclaration').
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
    LogBegin('D3DDevice_GetVertexShaderFunction').
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
    LogBegin('D3DDevice_GetVertexShaderConstant').
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
    LogBegin('D3DDevice_SelectVertexShaderDirect').
      _(pVAF, 'pVAF').
      _(StreamCount, 'StreamCount').
      _(pStreamInputs, 'pStreamInputs').
    LogEnd();

  Unimplemented('EmuD3DDevice_SetVertexShaderInputDirect');

  EmuSwapFS(fsXbox);
  Result := D3D_OK;
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
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetVertexShaderConstant').
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
      {StartRegister=}Register_,
      {pConstantData=}PSingle(pConstantData),
      {Vector4fCount=}ConstantCount
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

    LogBegin('D3DDevice_SetVertexShaderConstant1 >>').
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

    LogBegin('D3DDevice_SetVertexShaderConstant4 >>').
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

    LogBegin('D3DDevice_SetVertexShaderConstantNotInline >>').
      _(Register_, 'Register').
      _(pConstantData, 'pConstantData').
      _(ConstantCount, 'ConstantCount').
    LogEnd();

    EmuSwapFS(fsXbox);
  end;

  // Dxbx note: Shouldn't we return the result of this call?
  XTL_EmuD3DDevice_SetVertexShaderConstant(Register_, pConstantData, ConstantCount div 4);
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
    LogBegin('D3DDevice_GetVertexShaderInput').
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
    LogBegin('D3DDevice_SetVertexShaderInput').
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
    LogBegin('D3DDevice_RunVertexStateShader').
      _(Address, 'Address').
      _(pData, 'pData').
    LogEnd();

  Unimplemented('EmuD3DDevice_RunVertexStateShader');

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
    LogBegin('D3DDevice_SetScissors').
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
    LogBegin('D3DDevice_SetScreenSpaceOffset').
      _(x, 'x').
      _(y, 'y').
    LogEnd();

  EmuWarning('EmuD3DDevice_SetScreenSpaceOffset ignored');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
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
    LogBegin('D3DDevice_InsertCallback').
      _(Ord(Type_), 'Type').
      _(pCallback, 'pCallback').
      _(Context, 'Context').
    LogEnd();

  EmuWarning('InsertCallback ignored!');

  // TODO -oCXBX: Implement

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
    LogBegin('D3DDevice_GetProjectionViewportMatrix').
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
      EmuWarning('Unable to get viewport!'#13#10 + DxbxD3DErrorString(hRet));

    // Get current projection matrix
    hRet := g_pD3DDevice.GetTransform(D3DTS_PROJECTION, {out}mtxProjection);
    if (FAILED(hRet)) then
      EmuWarning('Unable to get projection matrix!'#13#10 + DxbxD3DErrorString(hRet));

    // Create the Viewport matrix manually
    // Direct3D8 doesn't give me everything I need in a viewport structure
    // (one thing I REALLY HATE!) so some constants will have to be used
    // instead.

    ClipWidth := 2.0;
    ClipHeight := 2.0;
    ClipX := -1.0;
    ClipY := 1.0;
    Width := DW2F(Viewport.Width);
    Height := DW2F(Viewport.Height);

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


// * func: EmuD3DDevice_SetStateVB (D3D::CDevice::SetStateVB)
procedure XTL_EmuD3DDevice_SetStateVB(Unknown1: ULONG); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetStateVB').
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
    LogBegin('D3DDevice_SetStipple').
      _(pPattern, 'pPattern').
    LogEnd();

  // We need an OpenGL port... badly

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

(* Cxbx based code; Could be valuable info, so keep this around
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
*)

function XTL_EmuD3DDevice_PrimeVertexCache
(
  VertexCount: UINT;
  pIndexData: PWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_PrimeVertexCache').
      _(VertexCount, 'VertexCount').
      _(pIndexData, 'pIndexData').
    LogEnd();

  // TODO -oCXBX: Implement
  EmuWarning('PrimeVertexCache is not supported!');
  // Dxbx note : The given indices are pushed to the GPU in nearly the same way as when a small number
  // of indices are send to DrawIndexedVertices, so perhaps we should forward this call to DrawIndexedVertices
  // (but with what X_D3DPRIMITIVETYPE ?)

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
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
    LogBegin('D3DDevice_GetModelView').
      _(pModelView, 'pModelView').
    LogEnd();

  // Dxbx note : Blueshogun remarked "I hope this is right". When Dxbx is running,
  // Rayman Arena it crashes after logging this (when going from the menu to ingame).
  // TODO -oDxbx : Find out if the crash occurs in here, or after this call (and fix it!)

  Result := g_pD3DDevice.GetTransform(D3DTS_WORLD, {out}mtxWorld);
  if (FAILED(Result)) then
    EmuWarning('Unable to get projection matrix!'#13#10 + DxbxD3DErrorString(Result));

  Result := g_pD3DDevice.GetTransform(D3DTS_VIEW, {out}mtxView);
  if (FAILED(Result)) then
    EmuWarning('Unable to get projection matrix!'#13#10 + DxbxD3DErrorString(Result));

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

procedure XTL_EmuD3DDevice_SetModelView
(
  {CONST} pModelView: PD3DMATRIX;
  {CONST} pInverseModelView: PD3DMATRIX;
  {CONST} pComposite: PD3DMATRIX
); stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_SetModelView').
      _(pModelView, 'pModelView').
      _(pInverseModelView, 'pInverseModelView').
      _(pComposite, 'pComposite').
    LogEnd();

  // TODO -oCxbx: Implement
  EmuWarning('SetModelView not yet implemented (should be easy fix, tell blueshogun)');

  EmuSwapFS(fsXbox);
end;

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

procedure XTL_EmuD3DDevice_GetLightEnable
(
  Index: DWORD;
  pEnable: PBOOL
); stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetLightEnable').
      _(Index, 'Index').
      _(pEnable, 'Enable').
    LogEnd();

  g_pD3DDevice.GetLightEnable(Index, pEnable^);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3DDevice_GetLight
(
  Index: DWORD;
  pLight: PD3DLIGHT
); stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetLight').
      _(Index, 'Index').
      _(pLight, 'pLight').
    LogEnd();

  g_pD3DDevice.GetLight(Index, pLight^);

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
    LogBegin('D3DDevice_SetLight').
      _(Index, 'Index').
      _(pLight, 'pLight').
    LogEnd();

  Result := g_pD3DDevice.SetLight(Index, pLight^);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetMaterial(pMaterial : PD3DMaterial): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetMaterial').
      _(pMaterial, 'pMaterial').
    LogEnd();

  g_pD3DDevice.GetMaterial(pMaterial^);
  Result := D3D_OK;

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
    LogBegin('D3DDevice_SetMaterial').
      _(pMaterial, 'pMaterial').
    LogEnd();

  Result := g_pD3DDevice.SetMaterial({const}pMaterial^);

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
    LogBegin('D3DDevice_SetBackMaterial').
      _(pMaterial, 'pMaterial').
    LogEnd();

  EmuWarning('SetBackMaterial is not supported!');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuD3DDevice_GetBackMaterial
(
  pMaterial: PD3DMaterial
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetBackMaterial').
      _(pMaterial, 'pMaterial').
    LogEnd();

  // D3DDevice_GetBackMaterial(pMaterial);
  EmuWarning('GetBackMaterial not supported!');

  Result := D3D_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DDevice_GetRasterStatus
(
  pRasterStatus: PD3DRASTER_STATUS
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DDevice_GetRasterStatus').
      _(pRasterStatus, 'pRasterStatus').
    LogEnd();

  Result := g_pD3DDevice.GetRasterStatus({$IFDEF DXBX_USE_D3D9}{iSwapChain=}0,{$ENDIF} {out}pRasterStatus^);

  EmuSwapFS(fsXbox);
end;

var g_OverscanColor: D3DCOLOR;

procedure XTL_EmuD3DDevice_SetOverscanColor
(
  Color: D3DCOLOR
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('D3DDevice_SetOverscanColor').
      _(Color, 'D3DCOLOR').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  g_OverscanColor := Color;
end;

function XTL_EmuD3DDevice_GetOverscanColor(): D3DCOLOR; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('D3DDevice_GetOverscanColor').LogEnd();
    EmuSwapFS(fsXbox);
  end;

  Result := g_OverscanColor;
end;

procedure XTL_EmuD3DVertexBuffer_Lock
(
  pVertexBuffer: PX_D3DVertexBuffer;
  OffsetToLock: UINT;
  SizeToLock: UINT;
  ppbData: PPBYTE;
  Flags: DWORD
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DVertexBuffer_Lock').
      _(pVertexBuffer, 'pVertexBuffer').
      _(OffsetToLock, 'OffsetToLock').
      _(SizeToLock, 'SizeToLock').
      _(ppbData, 'ppbData').
      _(Flags, 'Flags').
    LogEnd();

  Assert(Assigned(ppbData));

  // TODO : Handle Flags D3DLOCK_READONLY and D3DLOCK_NOOVERWRITE somwhow
  // TODO : Flush, unless D3DLOCK_NOFLUSH flag is present. (How?)

  ppbData^ := TLockData(UIntPtr(DxbxGetDataFromXboxResource(pVertexBuffer)) + OffsetToLock);

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuD3D8 : VertexBuffer locked : 0x%0.8x', [ppbData^]);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3DVertexBuffer_Lock2
(
  pVertexBuffer: PX_D3DVertexBuffer;
  Flags: DWORD
): PByte; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('D3DVertexBuffer_Lock2 >> ').
      _(pVertexBuffer, 'pVertexBuffer').
      _(Flags, 'Flags').
    LogEnd();

  EmuSwapFS(fsXbox);

  XTL_EmuD3DVertexBuffer_Lock(pVertexBuffer, {OffsetToLock=}0, {SizeToLock=}0, @Result, Flags);
end;

(*
function XTL_EmuD3DVertexBuffer_GetDesc
(
  pThis: PX_D3DVertexBuffer;
  pDesc: PD3DVERTEXBUFFER_DESC
): HResult; stdcall;
*)

(*
function XTL_EmuD3DPalette_Lock2
(
  pThis: PX_D3DPalette;
  Flags: DWORD
): PD3DCOLOR; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
*)

(*
function XTL_EmuD3DPalette_Lock
(
  pThis: PX_D3DPalette;
  ppColors: PPD3DCOLOR;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
*)

(* Too high level
function XTL_EmuD3DPalette_GetSize(
  pThis: PX_D3DPalette
): X_D3DPALETTESIZE;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
*)

(*
function XTL_EmuD3DBaseTexture_GetLevelCount
(
  pThis: PX_D3DBaseTexture
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DTexture_GetLevelDesc
(
  pThis: PX_D3DTexture;
  Level: UINT;
  pDesc: PX_D3DSURFACE_DESC
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DTexture_GetSurfaceLevel
(
  pThis: PX_D3DTexture;
  Level: UINT;
  ppSurfaceLevel: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DTexture_GetSurfaceLevel2
(
  pThis: PX_D3DTexture;
  Level: UINT
): PX_D3DResource; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
*)

(*
function XTL_EmuD3DTexture_LockRect
(
    pThis: PX_D3DTexture;
    Level: UINT;
    pLockedRect: PD3DLOCKED_RECT;
    pRect: PRECT;
    Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
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
*)

(*
function XTL_EmuD3DCubeTexture_GetLevelDesc
(
  pThis: PX_D3DCubeTexture;
  Level: UINT;
  pDesc: PX_D3DSURFACE_DESC
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
*)

(*
function XTL_EmuD3DCubeTexture_GetCubeMapSurface2
(
  pThis: PX_D3DCubeTexture;
  FaceType: D3DCUBEMAP_FACES;
  Level: UINT
): PX_D3DSurface; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
*)

(*
function XTL_EmuD3DVolumeTexture_GetLevelDesc
(
  pThis: PX_D3DVolumeTexture;
  Level: UINT;
  pDesc: PX_D3DVOLUME_DESC
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DVolumeTexture_GetVolumeLevel2
(
  pThis: PX_D3DVolumeTexture;
  Level: UINT;
  ppVolumeLevel: PPX_D3DVolume
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:0
*)

(*
function XTL_EmuD3DVolumeTexture_LockBox
(
  pThis: PX_D3DVolumeTexture;
  Level: UINT;
  pLockedVolume: PD3DLOCKED_BOX;
  pBox: PD3DBOX;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DVolume_GetDesc
(
  pThis: PX_D3DVolume;
  pDesc: PX_D3DVOLUME_DESC
): HRESULT; stdcall;
// Branch:DXBX  Translator:PatrickvL  Done:100
*)

(*
function XTL_EmuD3DVolume_GetContainer2
(
  pThis: PX_D3DVolume;
  ppBaseTexture: PPX_D3DBaseTexture
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:0
*)

(*
function XTL_EmuD3DVolume_LockBox
(
  pThis: PX_D3DVolume;
  pLockedVolume: PD3DLOCKED_BOX;
  {CONST}pBox: PD3DBOX;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:DXBX  Translator:PatrickvL  Done:100
*)

(*
function XTL_EmuD3DSurface_GetDesc
(
  pThis: PX_D3DResource;
  pDesc: PX_D3DSURFACE_DESC
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DSurface_LockRect
(
  pThis: PX_D3DResource;
  pLockedRect: PD3DLOCKED_RECT;
  pRect: PRECT;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
*)

(*
function XTL_EmuD3DSurface_GetContainer2
(
  pThis: PX_D3DSurface;
  ppBaseTexture: PPX_D3DBaseTexture
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:0
*)

(* Too high level : These methods just fill a FixupBuffer, which we handle via XTL_EmuApplyPushBufferFixup.
function XTL_EmuD3DPushBuffer_SetModelView
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset:  DWORD;
  {CONST}pModelView: PD3DMATRIX;
  {CONST}pInverseModelView: PD3DMATRIX;
  {CONST}pComposite: PD3DMATRIX
): HRESULT; stdcall;
// Branch:DXBX  Translator:Shadow_Tj  Done:0

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

function XTL_EmuD3DPushBuffer_SetPalette
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset: DWORD;
  Stage: DWORD;
  pPalette: PX_D3DPalette
): HRESULT; stdcall
// Branch:DXBX  Translator:Shadow_Tj  Done:0

function XTL_EmuD3DPushBuffer_SetVertexShaderConstant
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset:  DWORD;
  _Register: INT;
  {CONST}pConstantData: Pvoid;
  ConstantCount: DWORD
): HRESULT; stdcall
// Branch:DXBX  Translator:Shadow_Tj  Done:0

function XTL_EmuD3DPushBuffer_SetRenderState
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset: DWORD;
  State: X_D3DRENDERSTATETYPE;
  Value: DWORD
): HRESULT; stdcall
// Branch:DXBX  Translator:Shadow_Tj  Done:0

function XTL_EmuD3DPushBuffer_CopyRects
(
  pPushBuffer: PX_D3DPushBuffer;
  Offset: DWORD;
  pSourceSurface: PIDirect3DSurface;
  pDestinationSurface: PIDirect3DSurface
): HRESULT; stdcall
// Branch:DXBX  Translator:Shadow_Tj  Done:0
*)

exports
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

  // XTL_EmuD3DIndexBuffer_GetDesc, // Not yet implemented

  // XTL_EmuDevice3D_SetLightColors, // Not yet implemented
  // XTL_EmuDevice3D_FreeFrameBuffers name PatchPrefix + '?FreeFrameBuffers@CDevice@D3D@@QAEXXZ', .. Not yet implemented
  // XTL_EmuDevice3D_GpuGet name PatchPrefix + '?GpuGet@CDevice@D3D@@QAEPCKXZ', // Not yet implemented
  // XTL_EmuDevice3D_InitializePushBuffer name PatchPrefix + '?InitializePushBuffer@CDevice@D3D@@QAEJXZ', Not yet implemented
  // XTL_EmuDevice3D_LazySetStateUP name PatchPrefix + '?LazySetStateUP@CDevice@D3D@@QAEXXZ', // Not yet implemented
  // XTL_EmuDevice3D_LazySetStateVB name PatchPrefix + '?LazySetStateVB@CDevice@D3D@@QAEXK@Z', // Not yet implemented
  // XTL_EmuDevice3D_ReentrantKickOffAndWait name PatchPrefix + '?ReentrantKickOffAndWait@CDevice@D3D@@QAEXXZ', // Not yet implemented
  // XTL_EmuDevice3D_UnInit name PatchPrefix + '?UnInit@CDevice@D3D@@QAEXXZ', // Not yet implemented
  // XTL_EmuDevice3D_UninitializePushBuffer name PatchPrefix + '?UninitializePushBuffer@CDevice@D3D@@QAEXXZ', // Not yet implemented

  // XTL_EmuDirect3D_LazySetCombiners name PatchPrefix + 'LazySetCombiners', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetLights name PatchPrefix + 'LazySetLights', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetPointParams name PatchPrefix + 'LazySetPointParams', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetShaderStageProgram name PatchPrefix + LazySetShaderStageProgram, // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetSpecFogCombiner name PatchPrefix + 'LazySetSpecFogCombiner', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetState name PatchPrefix + 'LazySetState', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetTextureState name + PatchPrefix + 'LazySetTextureState', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetTextureTransform name + PatchPrefix 'LazySetTextureTransform', // Dxbx: Not yet implemented
  // XTL_EmuDirect3D_LazySetTransform name + PatchPrefix + 'LazySetTransform', // Dxbx: Not yet implemented

  // XTL_EmuGetDepthBufferScale name PatchPrefix + 'GetDepthBufferScale', // Not yet implemented
  // XTL_EmuGetEncodeFormat name PatchPrefix + 'EncodeFormat', // Not yet implemented
  // XTL_EmuGetPitch name PatchPrefix + 'GetPitch', // Not yet implemented
  // XTL_EmuGetSize name PatchPrefix + 'GetSize', // Not yet implemented
  // XTL_EmuGetSlice name PathcPrefix + 'GetSlice', // Not yet implemented
  // XTL_EmuGetSurfaceFormat name PatchPrefix + 'GetSurfaceFormat', // Not yet implemented

  // XTL_EmuXMETAL_StartPush name PatchPrefix + 'XMETAL_StartPush', // TODO : Implement this once we're emulating at pushbuffer level!

  XTL_EmuD3D_BlockOnResource,
  XTL_EmuD3D_BlockOnNonSurfaceResource,
  XTL_EmuD3D_CleanPrivateData,
  XTL_EmuD3D_GetDataFromResource,
  XTL_EmuD3D_KickOffAndWaitForIdle, {nopatch:CreateDevice}

  XTL_EmuD3D_PixelJar_FindSurfaceWithinTexture,
  XTL_EmuD3D_PixelJar_Get2DSurfaceDesc,
  XTL_EmuD3D_PixelJar_Get2DSurfaceDescD,
  XTL_EmuD3D_PixelJar_LockSurface,
  XTL_EmuD3D_PixelJar_Lock2DSurface,
  XTL_EmuD3D_PixelJar_Lock3DSurface,

//  XTL_EmuD3DBaseTexture_GetLevelCount, // Crashes Turok after opening movie, if unpatched (rev.1532)

//  XTL_EmuD3DCubeTexture_GetCubeMapSurface2,
//  XTL_EmuD3DCubeTexture_GetLevelDesc,
//  XTL_EmuD3DCubeTexture_LockRect,

  XTL_EmuD3DDevice_EnableCC,
  XTL_EmuD3DDevice_SendCC,
  XTL_EmuD3DDevice_GetCCStatus,

//  XTL_EmuD3DDevice_AddRef,
  XTL_EmuD3DDevice_ApplyStateBlock,
  XTL_EmuD3DDevice_Begin,
//  XTL_EmuD3DDevice_BeginPush_8 name PatchPrefix + '_D3DDevice_BeginPush@8',
//  XTL_EmuD3DDevice_BeginPush_4 name PatchPrefix + '_D3DDevice_BeginPush@4',
//  XTL_EmuD3DDevice_BeginPushBuffer, // ??
  XTL_EmuD3DDevice_BeginStateBig,
  XTL_EmuD3DDevice_BeginStateBlock,
  XTL_EmuD3DDevice_BeginVisibilityTest,
  XTL_EmuD3DDevice_BlockOnFence,
//  XTL_EmuD3DDevice_BlockUntilIdle, // Dxbx note : Disabled, too high level.
  XTL_EmuD3DDevice_BlockUntilVerticalBlank,
  XTL_EmuD3DDevice_CaptureStateBlock,
  XTL_EmuD3DDevice_Clear, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_CopyRects,
//  XTL_EmuD3DDevice_CreateCubeTexture,
//  XTL_EmuD3DDevice_CreateDepthStencilSurface,
//  XTL_EmuD3DDevice_CreateImageSurface,
//  XTL_EmuD3DDevice_CreateIndexBuffer, // Dxbx note : Disabled, too high level.
//  XTL_EmuD3DDevice_CreateIndexBuffer2, // Dxbx note : Disabled, too high level.
//  XTL_EmuD3DDevice_CreatePalette, // Dxbx note : Disabled, too high level.
//  XTL_EmuD3DDevice_CreatePalette2, // Dxbx note : Disabled, too high level.
//  XTL_EmuD3DDevice_CreatePixelShader, // Dxbx note : Disabled, too high level.
  XTL_EmuD3DDevice_CreateStateBlock,
//  XTL_EmuD3DDevice_CreateSurface,
//  XTL_EmuD3DDevice_CreateSurface2,
//  XTL_EmuD3DDevice_CreateTexture,
//  XTL_EmuD3DDevice_CreateTexture2,
//  XTL_EmuD3DDevice_CreateVertexBuffer,
//  XTL_EmuD3DDevice_CreateVertexBuffer2,
  XTL_EmuD3DDevice_CreateVertexShader,
//  XTL_EmuD3DDevice_CreateVolumeTexture,
//  XTL_EmuD3DDevice_DeletePixelShader, // Dxbx note : Disabled, too high level.
  XTL_EmuD3DDevice_DeleteStateBlock,
  XTL_EmuD3DDevice_DeleteVertexShader,
  XTL_EmuD3DDevice_DrawIndexedVertices, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_DrawIndexedVerticesUP, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_DrawRectPatch, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_DrawTriPatch, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_DrawVertices, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_DrawVerticesUP, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_EnableOverlay,
  XTL_EmuD3DDevice_End,
//  XTL_EmuD3DDevice_EndPush,
//  XTL_EmuD3DDevice_EndPushBuffer, // ??
  XTL_EmuD3DDevice_EndStateBlock,
  XTL_EmuD3DDevice_EndVisibilityTest,
  XTL_EmuD3DDevice_FlushVertexCache, // ??
//  XTL_EmuD3DDevice_GetBackBuffer, // Dxbx note : Disabled, too high level.
//  XTL_EmuD3DDevice_GetBackBuffer2, // Dxbx note : Disabled, too high level.
  XTL_EmuD3DDevice_GetBackBufferScale,
  XTL_EmuD3DDevice_GetBackMaterial,
  XTL_EmuD3DDevice_GetCreationParameters, // Patch can go (once we've verified that)
  XTL_EmuD3DDevice_GetDepthClipPlanes,
  XTL_EmuD3DDevice_GetDepthStencilSurface,
  XTL_EmuD3DDevice_GetDepthStencilSurface2,
//  XTL_EmuD3DDevice_GetDeviceCaps, // Dxbx note : Disabled, too high level.
  XTL_EmuD3DDevice_GetDirect3D,
  XTL_EmuD3DDevice_GetDisplayFieldStatus,
//  XTL_EmuD3DDevice_GetDisplayMode,
  XTL_EmuD3DDevice_GetGammaRamp,
//  XTL_EmuD3DDevice_GetIndices,
  XTL_EmuD3DDevice_GetLight,
  XTL_EmuD3DDevice_GetLightEnable,
  XTL_EmuD3DDevice_GetMaterial,
  XTL_EmuD3DDevice_GetModelView, // ??
  XTL_EmuD3DDevice_GetOverlayUpdateStatus,
  XTL_EmuD3DDevice_GetOverscanColor,
  XTL_EmuD3DDevice_GetPixelShader,
  XTL_EmuD3DDevice_GetPixelShaderConstant,
//  XTL_EmuD3DDevice_GetPixelShaderFunction, // Dxbx note : Disabled, too high level.
  XTL_EmuD3DDevice_GetProjectionViewportMatrix,
//  XTL_EmuD3DDevice_GetPushBufferOffset,
//  XTL_EmuD3DDevice_GetPushDistance,
  XTL_EmuD3DDevice_GetRasterStatus,
//  XTL_EmuD3DDevice_GetRenderState,
  XTL_EmuD3DDevice_GetRenderTarget, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_GetRenderTarget2, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_GetShaderConstantMode,
  XTL_EmuD3DDevice_GetStreamSource,
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
//  XTL_EmuD3DDevice_KickPushBuffer,
  XTL_EmuD3DDevice_LightEnable,
  XTL_EmuD3DDevice_LoadVertexShader,
  XTL_EmuD3DDevice_LoadVertexShaderProgram,
  XTL_EmuD3DDevice_Nop,
  XTL_EmuD3DDevice_PersistDisplay,
  XTL_EmuD3DDevice_Present, // Dxbx note : Present only occurs in 3911, sometime before 4361 it was changed into Swap
  XTL_EmuD3DDevice_PrimeVertexCache,
//  XTL_EmuD3DDevice_Release,
  XTL_EmuD3DDevice_Reset, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_Resume, {nopatch:CreateDevice}
//  XTL_EmuD3DDevice_RunPushBuffer,
  XTL_EmuD3DDevice_RunVertexStateShader,
  XTL_EmuD3DDevice_SelectVertexShader,
  XTL_EmuD3DDevice_SelectVertexShaderDirect,
  XTL_EmuD3DDevice_SetBackBufferScale,
  XTL_EmuD3DDevice_SetBackMaterial, // ??
  XTL_EmuD3DDevice_SetCopyRectsState,
  XTL_EmuD3DDevice_SetFlickerFilter, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetGammaRamp,
//  XTL_EmuD3DDevice_SetIndices,
  XTL_EmuD3DDevice_SetLight,
  XTL_EmuD3DDevice_SetMaterial,
  XTL_EmuD3DDevice_SetModelView, // ??
  XTL_EmuD3DDevice_SetOverscanColor,
  XTL_EmuD3DDevice_SetPalette,
  XTL_EmuD3DDevice_SetPixelShader,
  XTL_EmuD3DDevice_SetPixelShaderConstant,
  XTL_EmuD3DDevice_SetPixelShaderProgram,
  XTL_EmuD3DDevice_SetRenderState_BackFillMode, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_CullMode, {nopatch:CreateDevice}
//  XTL_EmuD3DDevice_SetRenderState_Deferred, Dxbx note : Disabled, as we DO have EmuD3DDeferredRenderState pin-pointed correctly
  XTL_EmuD3DDevice_SetRenderState_DoNotCullUncompressed, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_Dxt1NoiseEnable, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_EdgeAntiAlias, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_FillMode,  {nopatch:CreateDevice}// Method: 0x0000038C
  XTL_EmuD3DDevice_SetRenderState_FogColor,  {nopatch:CreateDevice}// Method: 0x000002A8
  XTL_EmuD3DDevice_SetRenderState_FrontFace, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_LineWidth, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_LogicOp, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_MultiSampleAntiAlias, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_MultiSampleMask, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_MultiSampleMode, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_MultiSampleRenderTargetMode, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_NormalizeNormals, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_OcclusionCullEnable, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_PSTextureModes, {nopatch:CreateDevice} // Method: 0x0000033C
  XTL_EmuD3DDevice_SetRenderState_RopZCmpAlwaysRead, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_RopZRead, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_SampleAlpha, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_ShadowFunc, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_Simple, {nopatch:CreateDevice} // Disabling this patch, makes Turok crash in MakeRequestedSpace; Why?
  XTL_EmuD3DDevice_SetRenderState_StencilCullEnable, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_StencilEnable, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_StencilFail, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_TextureFactor, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_TwoSidedLighting, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_VertexBlend, {nopatch:CreateDevice} // Method: 0x00000304
  XTL_EmuD3DDevice_SetRenderState_YuvEnable, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_ZBias, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderState_ZEnable, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderStateNotInline, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetRenderTarget, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetScissors, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetScreenSpaceOffset, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetShaderConstantMode, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetSoftDisplayFilter, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetStateUP,
  XTL_EmuD3DDevice_SetStateVB,
  XTL_EmuD3DDevice_SetStipple,
  XTL_EmuD3DDevice_SetStreamSource,
  XTL_EmuD3DDevice_SetSwapCallback,
  XTL_EmuD3DDevice_SetTexture,
  XTL_EmuD3DDevice_SetTextureStageStateNotInline, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetTextureState_BorderColor, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetTextureState_BumpEnv, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetTextureState_ColorKeyColor, {nopatch:CreateDevice}
//  XTL_EmuD3DDevice_SetTextureState_ParameterCheck, // Not yet implemented
  XTL_EmuD3DDevice_SetTextureState_TexCoordIndex, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetTile, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetTile name PatchPrefix + '?SetTileNoWait@D3D@@YGXKPBU_D3DTILE@@@Z', {nopatch:CreateDevice} // Dxbx note : SetTile is applied to SetTileNoWait in Cxbx 4361 OOPVA's!
  XTL_EmuD3DDevice_SetTileCompressionTagBits,
  XTL_EmuD3DDevice_SetTransform, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetVertexBlendModelView, // ??
  XTL_EmuD3DDevice_SetVertexData2f,
  XTL_EmuD3DDevice_SetVertexData2s,
  XTL_EmuD3DDevice_SetVertexData4f,
  XTL_EmuD3DDevice_SetVertexData4s,
  XTL_EmuD3DDevice_SetVertexData4ub,
  XTL_EmuD3DDevice_SetVertexDataColor,
  XTL_EmuD3DDevice_SetVertexShader, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SetVertexShaderConstant,
  XTL_EmuD3DDevice_SetVertexShaderConstant1,
  XTL_EmuD3DDevice_SetVertexShaderConstant4,
  XTL_EmuD3DDevice_SetVertexShaderConstantNotInline,
  XTL_EmuD3DDevice_SetVertexShaderInput,
  XTL_EmuD3DDevice_SetVertexShaderInputDirect,
  XTL_EmuD3DDevice_SetVerticalBlankCallback,
  XTL_EmuD3DDevice_SetViewport, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_Suspend,
  XTL_EmuD3DDevice_Swap, {nopatch:CreateDevice}
  XTL_EmuD3DDevice_SwitchTexture,
  XTL_EmuD3DDevice_UpdateOverlay,

//  XTL_EmuD3DPalette_GetSize, // Dxbx note : Disabled, too high level.
//  XTL_EmuD3DPalette_Lock,
//  XTL_EmuD3DPalette_Lock2,

// These pushbuffer functions are used to fill the fixup buffer;
// Since we now support fixups, there's no need to patch them anymore :
//  XTL_EmuD3DPushBuffer_CopyRects,
//  XTL_EmuD3DPushBuffer_SetModelView,
//  XTL_EmuD3DPushBuffer_SetPalette,
//  XTL_EmuD3DPushBuffer_SetRenderState,
//  XTL_EmuD3DPushBuffer_SetVertexBlendModelView,
//  XTL_EmuD3DPushBuffer_SetVertexShaderConstant,
//  XTL_EmuD3DPushBuffer_SetVertexShaderInputDirect,

//  XTL_EmuD3DResource_AddRef,
  XTL_EmuD3DResource_BlockUntilNotBusy,
//  XTL_EmuD3DResource_FreePrivateData,
  XTL_EmuD3DResource_GetDevice,
//  XTL_EmuD3DResource_GetPrivateData,
//  XTL_EmuD3DResource_GetType,
  XTL_EmuD3DResource_IsBusy,
//  XTL_EmuD3DResource_Register,
//  XTL_EmuD3DResource_Release,
//  XTL_EmuD3DResource_SetPrivateData,

//  XTL_EmuD3DSurface_GetContainer2,
//  XTL_EmuD3DSurface_GetDesc,
//  XTL_EmuD3DSurface_LockRect,
//  XTL_EmuD3DTexture_GetLevelDesc,
//  XTL_EmuD3DTexture_GetSurfaceLevel,
//  XTL_EmuD3DTexture_GetSurfaceLevel2, // Disabling this patch, makes Turok crash early in CreateSurface or something :
//  XTL_EmuD3DTexture_LockRect,

//  XTL_EmuD3DVertexBuffer_GetDesc,
  XTL_EmuD3DVertexBuffer_Lock,
  XTL_EmuD3DVertexBuffer_Lock2,

//  XTL_EmuD3DVolume_GetContainer2,
//  XTL_EmuD3DVolume_GetDesc,
//  XTL_EmuD3DVolume_LockBox,

//  XTL_EmuD3DVolumeTexture_GetLevelDesc,
//  XTL_EmuD3DVolumeTexture_GetVolumeLevel2,
//  XTL_EmuD3DVolumeTexture_LockBox,

  XTL_EmuD3D_CDevice_KickOff, {nopatch:CreateDevice}

//  XTL_EmuDirect3D_AllocContiguousMemory name PatchPrefix + '_D3D_AllocContiguousMemory@8', // No need to patch, as our kernel implements MmAllocateContiguousMemory(Ex) already
//  XTL_EmuDirect3D_CheckDeviceFormat, // Dxbx note : Disabled, too high level.
//  XTL_EmuDirect3D_CheckDeviceMultiSampleType, // Dxbx note : Disabled, too high level.
//  XTL_EmuDirect3D_CreateDevice, // Dxbx note : Disabled, too high level. Initialization goes through miniport !!!
  XTL_EmuDirect3D_EnumAdapterModes,
  XTL_EmuDirect3D_GetAdapterDisplayMode,
  XTL_EmuDirect3D_GetAdapterModeCount;
//  XTL_EmuDirect3D_GetDeviceCaps, // Dxbx note : Disabled, too high level.
//  XTL_EmuDirect3D_SetPushBufferSize;
{$ENDIF !PUSHBUFFER_ONLY}

initialization

  ZeroMemory(@g_VBData, SizeOf(g_VBData));
  ZeroMemory(@g_SwapData, SizeOf(g_SwapData));

finalization

  ExtraXRGBSurface := nil;

end.


