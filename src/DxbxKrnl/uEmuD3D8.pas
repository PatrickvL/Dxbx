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

{$INCLUDE Dxbx.inc}

{.$define _DEBUG_DUMP_TEXTURE_SETTEXTURE}
{.$define _DEBUG_DUMP_TEXTURE_REGISTER}
{.$define _DEBUG_TRACK_VS}
{.$define _DEBUG_TRACE_VB}
{.$define _DEBUG_TRACK_VS_CONST}


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
  Direct3D, // PD3DCOLOR
  Direct3D8, // D3DDEVTYPE
  DirectDraw, // IDIRECTDRAWSURFACE7
  D3DX8, // PD3DXVECTOR4
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uConsts,
  uXbVideo,
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

function DxbxUnlockD3DResource(pResource: PX_D3DResource; uiLevel: int = 0): Boolean;
function DxbxFVFToVertexSizeInBytes(dwVertexShader: DWORD; bIncludeTextures: boolean): uint;
function DxbxPresent(pSourceRect: PRECT; pDestRect: PRECT; pDummy1: HWND; pDummy2: PVOID): UINT;

procedure XTL_EmuD3DInit(XbeHeader: PXBEIMAGE_HEADER; XbeHeaderSize: UInt32); {NOPATCH}
function XTL_EmuIDirect3D8_CreateDevice(Adapter: UINT; DeviceType: D3DDEVTYPE;
  hFocusWindow: HWND; BehaviorFlags: DWORD;
  pPresentationParameters: PX_D3DPRESENT_PARAMETERS;
  ppReturnedDeviceInterface: XTL_PPIDirect3DDevice8): HRESULT; stdcall// forward

function XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_: Integer;
  a, b, c, d: FLOAT): HRESULT; stdcall; // forward
function XTL_EmuIDirect3DDevice8_GetVertexShader({CONST} pHandle: PDWORD): HRESULT; stdcall; // forward

function XTL_EmuIDirect3DResource8_Register(pThis: PX_D3DResource;
  pBase: PVOID): HRESULT; stdcall;
function XTL_EmuIDirect3DDevice8_CreateTexture(Width: UINT; Height: UINT;
    Levels: UINT; Usage: DWORD; Format: X_D3DFORMAT; Pool: D3DPOOL; ppTexture: PPX_D3DTexture): HRESULT; stdcall;
function XTL_EmuIDirect3DDevice8_CreateVolumeTexture(Width: UINT; Height: UINT;
    Depth: UINT; Levels: UINT; Usage: DWORD; Format: X_D3DFORMAT;
    Pool: D3DPOOL; ppVolumeTexture: PPX_D3DVolumeTexture): HRESULT; stdcall;
function XTL_EmuIDirect3DDevice8_CreateCubeTexture(
    EdgeLength: UINT;
    Levels: UINT;
    Usage: DWORD;
    Format: X_D3DFORMAT;
    Pool: D3DPOOL;
    ppCubeTexture: PPX_D3DCubeTexture): HRESULT; stdcall;
function XTL_EmuIDirect3DTexture8_GetSurfaceLevel(pThis: PX_D3DTexture;
    Level: UINT;
    ppSurfaceLevel: PPX_D3DSurface): HRESULT; stdcall;

function XTL_EmuIDirect3DPalette8_Lock2(pThis: PX_D3DPalette; Flags: DWORD): PD3DCOLOR; stdcall;
function XTL_EmuIDirect3DDevice8_CreatePalette2(Size: X_D3DPALETTESIZE): PX_D3DPalette; stdcall;
procedure XTL_EmuIDirect3DDevice8_EnableOverlay(Enable: BOOL); stdcall;
procedure XTL_EmuIDirect3DDevice8_UpdateOverlay(pSurface: PX_D3DSurface;
  SrcRect: PRECT;
  DstRect: PRECT;
  EnableColorKey: BOOL;
  ColorKey: D3DCOLOR); stdcall;
function XTL_EmuIDirect3DDevice8_CreateVertexBuffer2(Length: UINT): PX_D3DVertexBuffer; stdcall;

var
  // Global(s)
  g_pD3DDevice8: XTL_LPDIRECT3DDEVICE8 = NULL;     // Direct3D8 Device
  g_pDDSPrimary: XTL_LPDIRECTDRAWSURFACE7 = NULL;  // DirectDraw7 Primary Surface
  g_pDDSOverlay7: XTL_LPDIRECTDRAWSURFACE7 = NULL; // DirectDraw7 Overlay Surface
  g_pDDClipper: XTL_LPDIRECTDRAWCLIPPER = NULL;    // DirectDraw7 Clipper
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
  g_pD3D8: XTL_LPDIRECT3D8 = NULL;    // Direct3D8
  g_bSupportsYUY2: BOOL_ = FALSE; // Does device support YUY2 overlays?
  g_pDD7: XTL_LPDIRECTDRAW7 = NULL;   // DirectDraw7
  g_dwOverlayW: DWORD = 640;     // Cached Overlay Width
  g_dwOverlayH: DWORD = 480;     // Cached Overlay Height
  g_dwOverlayP: DWORD = 640;     // Cached Overlay Pitch

  g_XbeHeader: PXBEIMAGE_HEADER = NULL;         // XbeHeader
  g_XbeHeaderSize: uint32 = 0;             // XbeHeaderSize
  g_D3DCaps: D3DCAPS8;                     // Direct3D8 Caps
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
  g_pCurrentPalette: PVOID;
  // cached palette size
  g_dwCurrentPaletteSize: DWORD = DWORD(-1);

  g_VertexShaderConstantMode: X_VERTEXSHADERCONSTANTMODE = X_VSCM_192;

// cached Direct3D tiles
// g_EmuD3DTileCache (8 tiles maximum)
var g_EmuD3DTileCache: array [0..$08 - 1] of X_D3DTILE;

// cached active texture
var g_EmuD3DActiveTexture: array [0..4 - 1] of PX_D3DResource; // = {0,0,0,0};

// information passed to the create device proxy thread
type EmuD3D8CreateDeviceProxyData = packed record
    Adapter: UINT;
    DeviceType: D3DDEVTYPE;
    hFocusWindow: HWND;
    BehaviorFlags: DWORD;
    pPresentationParameters: PX_D3DPRESENT_PARAMETERS;
    ppReturnedDeviceInterface: XTL_PPIDirect3DDevice8;
    {volatile} bReady: _bool;
    {union} case Integer of
      0: ({volatile} hRet: HRESULT);
      1: ({volatile} bCreate: _bool); // false: release
  end; // packed size = 29

var g_EmuCDPD: EmuD3D8CreateDeviceProxyData;

implementation

uses
  // Dxbx
  uDxbxKrnl
  , uXboxLibraryUtils; // Should not be here, but needed for DxbxKrnlRegisterThread

procedure DxbxResetGlobals;
begin
  g_pD3DDevice8 := NULL;
  g_pDDSPrimary := NULL;
  g_pDDSOverlay7 := NULL;
  g_pDDClipper := NULL;
  g_CurrentVertexShader := 0;
  g_bFakePixelShaderLoaded := FALSE;
  g_bIsFauxFullscreen := FALSE;
  g_bHackUpdateSoftwareOverlay := FALSE;

  ZeroMemory(@g_ddguid, SizeOf(GUID));
  g_hMonitor := 0;
  g_pD3D8 := NULL;
  g_bSupportsYUY2 := FALSE;
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

  g_BuildVersion := 0;

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

  g_VertexShaderConstantMode := X_VSCM_192;

  ZeroMemory(@g_EmuD3DTileCache, SizeOf(g_EmuD3DTileCache));

  ZeroMemory(@g_EmuD3DActiveTexture, SizeOf(g_EmuD3DActiveTexture));
  ZeroMemory(@g_EmuCDPD, SizeOf(g_EmuCDPD));
end;

// Generic unlocking of a D3DResource.
//
// Dxbx Note : As suggested by StrikerX3, we should call this for any resource
// that's locked while it shouldn't be anymore; So call this before Lock() itself,
// before SetTexture(), SetStreamSource(), SetIndices() and maybe other calls too.
function DxbxUnlockD3DResource(pResource: PX_D3DResource; uiLevel: int = 0): Boolean;
begin
  Result := Assigned(pResource) and Assigned(pResource.Emu.Resource8);
  if not Result then
    Exit;

  if IsSpecialResource(pResource.Data) and ((pResource.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0) then
    Exit;


  case (IDirect3DResource8(pResource.Emu.Resource8).GetType()) of
    D3DRTYPE_SURFACE:
      IDirect3DSurface8(pResource.Emu.Surface8).UnlockRect();

    D3DRTYPE_VOLUME:
      // TODO -oDxbx : Totally untested! Volume's aren't used yet!
      IDirect3DVolume8(pResource.Emu.Resource8).UnlockBox();

    D3DRTYPE_TEXTURE:
      IDirect3DTexture8(pResource.Emu.Texture8).UnlockRect(uiLevel);

    D3DRTYPE_VOLUMETEXTURE:
      // TODO -oDxbx : Totally untested! VolumeTexture's aren't used yet!?
      IDirect3DVolumeTexture8(pResource.Emu.VolumeTexture8).UnlockBox(uiLevel);

    D3DRTYPE_CUBETEXTURE:
    begin
      IDirect3DCubeTexture8(pResource.Emu.CubeTexture8).UnlockRect(D3DCUBEMAP_FACE_POSITIVE_X, uiLevel);
      IDirect3DCubeTexture8(pResource.Emu.CubeTexture8).UnlockRect(D3DCUBEMAP_FACE_NEGATIVE_X, uiLevel);
      IDirect3DCubeTexture8(pResource.Emu.CubeTexture8).UnlockRect(D3DCUBEMAP_FACE_POSITIVE_Y, uiLevel);
      IDirect3DCubeTexture8(pResource.Emu.CubeTexture8).UnlockRect(D3DCUBEMAP_FACE_NEGATIVE_Y, uiLevel);
      IDirect3DCubeTexture8(pResource.Emu.CubeTexture8).UnlockRect(D3DCUBEMAP_FACE_POSITIVE_Z, uiLevel);
      IDirect3DCubeTexture8(pResource.Emu.CubeTexture8).UnlockRect(D3DCUBEMAP_FACE_NEGATIVE_Z, uiLevel);
    end;

    D3DRTYPE_VERTEXBUFFER:
      IDirect3DVertexBuffer8(pResource.Emu.VertexBuffer8).Unlock();

    D3DRTYPE_INDEXBUFFER:
      IDirect3DIndexBuffer8(pResource.Emu.IndexBuffer8).Unlock();
  else
    Result := False;
  end;
end;

function DxbxFVFToVertexSizeInBytes(dwVertexShader: DWORD; bIncludeTextures: boolean): uint;
begin
(*
  D3DFVF_POSITION_MASK    = $00E; // Dec  /2  #fl

  D3DFVF_XYZ              = $002; //  2 > 1 > 3
  D3DFVF_XYZRHW           = $004; //  4 > 2 > 4
  D3DFVF_XYZB1            = $006; //  6 > 3 > 4
  D3DFVF_XYZB2            = $008; //  8 > 4 > 5
  D3DFVF_XYZB3            = $00a; // 10 > 5 > 6
  D3DFVF_XYZB4            = $00c; // 12 > 6 > 7
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

  if (dwVertexShader and D3DFVF_DIFFUSE) > 0 then begin Inc(Result, sizeof(DWORD)); end;
  if (dwVertexShader and D3DFVF_SPECULAR) > 0 then begin Inc(Result, sizeof(DWORD)); end;

  if bIncludeTextures then
    Inc(Result, ((dwVertexShader and D3DFVF_TEXCOUNT_MASK) shr D3DFVF_TEXCOUNT_SHIFT)*sizeof(FLOAT)*2);
end;

// A wrapper for Present() with an extra safeguard to restore 'device lost' errors :
function DxbxPresent(pSourceRect: PRECT; pDestRect: PRECT; pDummy1: HWND; pDummy2: PVOID): UINT;
begin
  g_bIsBusy := BOOL_TRUE;
  Result := IDirect3DDevice8(g_pD3DDevice8).Present(pSourceRect, pDestRect, pDummy1, pDummy2);
  g_bIsBusy := BOOL_FALSE;
  if Result = D3D_OK then
    Exit;

  if Result = UINT(D3DERR_DEVICELOST) then
  repeat
    Result := IDirect3DDevice8(g_pD3DDevice8).TestCooperativeLevel;
    if(Result = UINT(D3DERR_DEVICELOST)) then //Device is lost and cannot be reset yet
      Sleep(500) //Wait a bit so we don't burn through cycles for no reason
    else
      if(Result = UINT(D3DERR_DEVICENOTRESET)) then //Lost but we can reset it now
        Result := IDirect3DDevice8(g_pD3DDevice8).Reset(PD3DPresentParameters(g_EmuCDPD.pPresentationParameters)^);
  until Result = UINT(D3D_OK);
end;

const
  MillisecondsPerSecond = 1000;

type
  DxbxTimer = record
    PerformanceCounterFrequency: LARGE_INTEGER;
    CurrentPerformanceCounter: LARGE_INTEGER;
    PreviousPerformanceCounter: LARGE_INTEGER;
    TicksToWait: int;
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
  TicksToWait := aTicksToWait;

  // Calculate how many ticks pass during Sleep(1) :
  TicksPerSleep1 := int(PerformanceCounterFrequency.QuadPart * 2 div MillisecondsPerSecond);

  // Raise the accuracy of Sleep to it's maximum :
  timeBeginPeriod(1);

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
  TicksLeft: int;
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
      QueryPerformanceCounter({var}TLargeInteger(CurrentPerformanceCounter));

      TicksPassed := int(CurrentPerformanceCounter.QuadPart - PreviousPerformanceCounter.QuadPart);
      if (TicksPassed <= 0) then // time wrap
        Break;

      TicksLeft := TicksToWait - TicksPassed;
      if (TicksLeft <= 0) then // time's up
        Break;

      if (TicksLeft > TicksPerSleep1) then
        // if > 0.002s left, do Sleep(1), which will actually sleep some
        //  steady amount, probably 1-2 ms,
        //  and do so in a nice way (cpu meter drops; laptop battery spared).
        Sleep(TicksLeft div TicksPerSleep1)
      else
        // otherwise, do a few Sleep(0)'s, which just give up the timeslice,
        //  but don't really save cpu or battery, but do pass a tiny
        //  amount of time.
//        for i := 0 to 10-1 do
          Sleep(0); // causes thread to give up its timeslice
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
  PresParam: X_D3DPRESENT_PARAMETERS;
begin
  g_EmuShared.GetXBVideo(@g_XBVideo);

  if (g_XBVideo.GetFullscreen()) then
    DxbxKrnl_hEmuParent := 0;

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
    IDirect3D8(g_pD3D8) := Direct3DCreate8(D3D_SDK_VERSION);
    if (g_pD3D8 = NULL) then
      DxbxKrnlCleanup('Could not initialize Direct3D8!');

    DevType := iif(g_XBVideo.GetDirect3DDevice() = 0, D3DDEVTYPE_HAL, D3DDEVTYPE_REF);
    IDirect3D8(g_pD3D8).GetDeviceCaps(g_XBVideo.GetDisplayAdapter(), DevType, {out}g_D3DCaps);
  end;

  SetFocus(g_hEmuWindow);

  // create default device
  begin
    ZeroMemory(@PresParam, sizeof(PresParam));

    PresParam.BackBufferWidth := 640;
    PresParam.BackBufferHeight := 480;
    PresParam.BackBufferFormat := X_D3DFMT_A8R8G8B8; //=6
    PresParam.BackBufferCount := 1;
    PresParam.EnableAutoDepthStencil := BOOL_TRUE;
    PresParam.AutoDepthStencilFormat := X_D3DFMT_D24S8; //=$2A
    PresParam.SwapEffect := D3DSWAPEFFECT_DISCARD;

    EmuSwapFS(fsXbox);
    XTL_EmuIDirect3D8_CreateDevice(
      0,
      D3DDEVTYPE_HAL,
      {ignored hFocusWindow=}0,
      {ignored BehaviorFlags=}D3DCREATE_HARDWARE_VERTEXPROCESSING, // = $00000040
      @PresParam,
      @g_pD3DDevice8);
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
  lPrintfOn: _bool;
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
    AsciiTitle := 'Unknown';

    CertAddr := g_XbeHeader.dwCertificateAddr - g_XbeHeader.dwBaseAddr;

    // TODO -oDXBX: These $0C and 40 values should become a suiteably named constant :
    // (They represent the offset and length of the XBE_CERTIFICATE.wszTitleName field)
    if DWORD(CertAddr + $0C + 40) < g_XbeHeaderSize then
    begin
      IntPtr(XbeCert) := IntPtr(g_XbeHeader) + CertAddr;

      // SetLocaleInfo(LC_ALL, 'English'); // Not neccesary, Delphi has this by default

      AsciiTitle := XbeCert.wszTitleName; // No wcstombs needed, Delphi does this automatically
    end;

    AsciiTitle := 'Dxbx: Emulating ' + AsciiTitle;
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : Message-Pump thread is running.');
{$ENDIF}

  SetFocus(g_hEmuWindow);

  { TODO -oDXBX: Need to be translated to delphi }
  (*
  DbgConsole *dbgConsole := new DbgConsole();
  *)

  // message processing loop
  begin
    ZeroMemory(@msg, sizeof(msg));

    lPrintfOn := g_bPrintfOn;

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

        // if we've just switched back to display off, clear buffer & display prompt
        if not g_bPrintfOn and lPrintfOn then
          { TODO -oDXBX: Need to be translated to delphi }
          ; // dbgConsole.Reset();

        lPrintfOn := g_bPrintfOn;

        (*dbgConsole.Process();
        *)
      end;
    end;

    g_bRenderWindowActive := false;

//        delete dbgConsole;

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
        Result := S_OK;
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
      begin
        // disable fullscreen if we are set to faux mode, and faux fullscreen is active
        if (wParam = VK_ESCAPE) then
        begin
          if (g_XBVideo.GetFullscreen()) then
          begin
            SendMessage(hWnd, WM_CLOSE, 0, 0);
          end
          else if (g_bIsFauxFullscreen) then
          begin
            ToggleFauxFullscreen(hWnd);
          end;
        end
        else if (wParam = VK_F8) then
        begin
          g_bPrintfOn := not g_bPrintfOn;
        end
        else if (wParam = VK_F9) then
        begin
          XTL_g_bBrkPush := TRUE;
        end
        else if (wParam = VK_F10) then
        begin
          ToggleFauxFullscreen(hWnd);
        end
        else if (wParam = VK_F11) then
        begin
          Inc(g_iWireframe); // Cxbx uses post-increment and compares+1 :
          if g_iWireframe = 3 then
            g_iWireframe := 0;
        end
        else if (wParam = VK_F12) then
        begin
          XTL_g_bStepPush := not XTL_g_bStepPush;
        end;
      end;

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
          Result := S_OK;
          Exit;
        end;

        Result := DefWindowProc(hWnd, msg, wParam, lParam);
        Exit;
      end;
  else
    Result := DefWindowProc(hWnd, msg, wParam, lParam);
    Exit;
  end;

  Result := S_OK;
end; // EmuMsgProc


// timing thread procedure
function EmuThreadUpdateTickCount(lpVoid: LPVOID): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  UpdateTimer: DxbxTimer;
  NativeSystemTime: KSYSTEM_TIME;
  Tmp: LARGE_INTEGER;
begin
{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : Update timer thread is running.');
{$ENDIF}

  // Update the various Xbox kernel timers as often as reasonable, without swamping the CPU;
  UpdateTimer.InitFPS(100); // 100 updates per second should be enough

  while true do // TODO -oDxbx: When do we break out of this while loop ?
  begin
    UpdateTimer.Wait;

    // Dxbx note : Just like with xboxkrnl_KeQueryPerformanceCounter,
    // we apply a factor here, so that these counters don't run too fast :

    xboxkrnl_KeTickCount := Trunc(NativeToXboxSpeedFactor * timeGetTime());

    {ignore result}JwaNative.NtQuerySystemTime(@NativeSystemTime);
    Tmp.HighPart := NativeSystemTime.High1Time;
    Tmp.LowPart := NativeSystemTime.LowPart;
    Tmp.QuadPart := Trunc(NativeToXboxSpeedFactor * Tmp.QuadPart);
    xboxkrnl_KeSystemTime.High1Time := Tmp.HighPart;
    xboxkrnl_KeSystemTime.LowPart := Tmp.LowPart;

    // For now, set InterruptTime to native SystemTime, but according to
    // http://msdn.microsoft.com/en-us/library/ee662306(VS.85).aspx
    // that's not the best solution. TODO -oDxbx: Determine what's better.
    xboxkrnl_KeInterruptTime := xboxkrnl_KeSystemTime;

  end; // while
end; // EmuThreadUpdateTickCount


function EmuThreadPollInput(lpVoid: LPVOID): DWORD; stdcall;
// Branch:Dxbx Translator:PatrickvL  Done:100
var
  UpdateTimer: DxbxTimer;
  v: int;
  hDevice: HANDLE;
  dwLatency: DWORD;
  pFeedback: PXINPUT_FEEDBACK;
begin
{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : Input polling thread is running.');
{$ENDIF}

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
// Branch:Dxbx Translator:PatrickvL  Done:100
const
  VBlankHertz = 60; // TODO -oDxbx : Make VBlankHertz dependant on PAL (50 Hz) or NTSC (60 Hz) emulation
var
  UpdateTimer: DxbxTimer;
begin
{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : VBlank handling thread is running.');
{$ENDIF}

  // since callbacks come from here
  EmuGenerateFS(DxbxKrnl_TLS, DxbxKrnl_TLSData);

  UpdateTimer.InitFPS(VBlankHertz);

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

      // TODO -oCxbx: Recalculate this for PAL version if necessary.
      // Also, we should check the D3DPRESENT_INTERVAL value for accurracy.
      g_SwapData.TimeBetweenSwapVBlanks := 0; //MillisecondsPerSecond div VBlankHertz;
    end;
  end; // while True
end; // EmuThreadHandleVBlank


// thread dedicated to create devices
function EmuThreadCreateDeviceProxy(lpVoid: LPVOID): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  D3DDisplayMode: TD3DDisplayMode; // X_D3DDISPLAYMODE; // TODO -oDXBX: : What type should we use?
  szBackBufferFormat: array [0..16 - 1] of AnsiChar;
  hRet: HRESULT;
  dwCodes: DWORD;
  lpCodes: PDWORD;
  v: DWORD;
  ddsd2: DDSURFACEDESC2;
  Streams: int;
begin
{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : CreateDevice proxy thread is running.');
{$ENDIF}

  while (true) do
  begin
    // if we have been signalled, create the device with cached parameters
    if(g_EmuCDPD.bReady) then
    begin
{$IFDEF DEBUG}
      DbgPrintf('EmuD3D8 : CreateDevice proxy thread received request.');
{$ENDIF}

      if (g_EmuCDPD.bCreate) then
      begin
        // only one device should be created at once
        // TODO -oCXBX: ensure all surfaces are somehow cleaned up?
        if (g_pD3DDevice8 <> nil) then
        begin
{$IFDEF DEBUG}
          DbgPrintf('EmuD3D8 : CreateDevice proxy thread releasing old Device.');
{$ENDIF}

          IDirect3DDevice8(g_pD3DDevice8).EndScene();

          while (IDirect3DDevice8(g_pD3DDevice8)._Release() <> 0) do ;

          g_pD3DDevice8 := nil;
          g_pDummyBuffer := nil;
        end;

        if (PX_D3DPRESENT_PARAMETERS(g_EmuCDPD.pPresentationParameters).BufferSurfaces[0] <> NULL) then
          EmuWarning('BufferSurfaces[0]: 0x%.08X', [PX_D3DPRESENT_PARAMETERS(g_EmuCDPD.pPresentationParameters).BufferSurfaces[0]]);

        if (PX_D3DPRESENT_PARAMETERS(g_EmuCDPD.pPresentationParameters).DepthStencilSurface <> NULL) then
          EmuWarning('DepthStencilSurface: 0x%.08X', [PX_D3DPRESENT_PARAMETERS(g_EmuCDPD.pPresentationParameters).DepthStencilSurface]);

        // make adjustments to parameters to make sense with windows Direct3D
        begin
          g_EmuCDPD.DeviceType := iif(g_XBVideo.GetDirect3DDevice() = 0, D3DDEVTYPE_HAL, D3DDEVTYPE_REF);
          g_EmuCDPD.Adapter := g_XBVideo.GetDisplayAdapter();

          g_EmuCDPD.pPresentationParameters.Windowed := not g_XBVideo.GetFullscreen();

          if (g_XBVideo.GetVSync()) then
            g_EmuCDPD.pPresentationParameters.SwapEffect := D3DSWAPEFFECT_COPY_VSYNC;

          // Note: Instead of the hFocusWindow argument, we use the global g_hEmuWindow here:
          g_EmuCDPD.hFocusWindow := g_hEmuWindow;

          TD3DFormat(g_EmuCDPD.pPresentationParameters.BackBufferFormat) := EmuXB2PC_D3DFormat(g_EmuCDPD.pPresentationParameters.BackBufferFormat);
          TD3DFormat(g_EmuCDPD.pPresentationParameters.AutoDepthStencilFormat) := EmuXB2PC_D3DFormat(g_EmuCDPD.pPresentationParameters.AutoDepthStencilFormat);

          if (not g_XBVideo.GetVSync() and ((g_D3DCaps.PresentationIntervals and D3DPRESENT_INTERVAL_IMMEDIATE) > 0) and g_XBVideo.GetFullscreen()) then
            g_EmuCDPD.pPresentationParameters.FullScreen_PresentationInterval := D3DPRESENT_INTERVAL_IMMEDIATE
          else
          begin
            if ((g_D3DCaps.PresentationIntervals and D3DPRESENT_INTERVAL_ONE) > 0) and g_XBVideo.GetFullscreen() then
              g_EmuCDPD.pPresentationParameters.FullScreen_PresentationInterval := D3DPRESENT_INTERVAL_ONE
            else
              g_EmuCDPD.pPresentationParameters.FullScreen_PresentationInterval := D3DPRESENT_INTERVAL_DEFAULT;
          end;

          // Cxbx HACK: Disable Tripple Buffering for now...
          // TODO -oCXBX: Enumerate maximum BackBufferCount if possible.
          if g_EmuCDPD.pPresentationParameters.BackBufferCount > 1 then
          begin
            EmuWarning('Limiting BackBufferCount to 1...');
            g_EmuCDPD.pPresentationParameters.BackBufferCount := 1;
          end;

          // TODO -oCXBX: Support Xbox extensions if possible
          if (g_EmuCDPD.pPresentationParameters.MultiSampleType <> D3DMULTISAMPLE_NONE) then
          begin
            EmuWarning('MultiSampleType 0x%.08X is not supported!', [Ord(g_EmuCDPD.pPresentationParameters.MultiSampleType)]);

            g_EmuCDPD.pPresentationParameters.MultiSampleType := D3DMULTISAMPLE_NONE;

            // TODO -oCXBX: Check card for multisampling abilities // MARKDE OUT BY CXBX
            // if (g_EmuCDPD.pPresentationParameters.MultiSampleType = $00001121) then
            //   g_EmuCDPD.pPresentationParameters.MultiSampleType := D3DMULTISAMPLE_2_SAMPLES
            // else
            //   DxbxKrnlCleanup('Unknown MultiSampleType (0x%.08X)', [g_EmuCDPD.pPresentationParameters.MultiSampleType]);
          end;

          g_EmuCDPD.pPresentationParameters.Flags := D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;

          // retrieve resolution from configuration
          if (g_EmuCDPD.pPresentationParameters.Windowed) then
          begin
            sscanf(g_XBVideo.GetVideoResolution(), '%d x %d', [
              @(g_EmuCDPD.pPresentationParameters.BackBufferWidth),
              @(g_EmuCDPD.pPresentationParameters.BackBufferHeight)]);

            IDirect3D8(g_pD3D8).GetAdapterDisplayMode(g_XBVideo.GetDisplayAdapter(), {out}D3DDisplayMode);

            g_EmuCDPD.pPresentationParameters.BackBufferFormat := X_D3DFORMAT(D3DDisplayMode.Format);
            g_EmuCDPD.pPresentationParameters.FullScreen_RefreshRateInHz := 0;
          end
          else
          begin
            sscanf(g_XBVideo.GetVideoResolution(), '%d x %d %*dbit %s (%d hz)', [
              @(g_EmuCDPD.pPresentationParameters.BackBufferWidth),
              @(g_EmuCDPD.pPresentationParameters.BackBufferHeight),
              @(szBackBufferFormat[0]),
              @(g_EmuCDPD.pPresentationParameters.FullScreen_RefreshRateInHz)]);

            if (strcmp(szBackBufferFormat, 'x1r5g5b5') = 0) then
              g_EmuCDPD.pPresentationParameters.BackBufferFormat := X_D3DFORMAT(D3DFMT_X1R5G5B5)
            else if (strcmp(szBackBufferFormat, 'r5g6r5') = 0) then
              g_EmuCDPD.pPresentationParameters.BackBufferFormat := X_D3DFORMAT(D3DFMT_R5G6B5)
            else if (strcmp(szBackBufferFormat, 'x8r8g8b8') = 0) then
              g_EmuCDPD.pPresentationParameters.BackBufferFormat := X_D3DFORMAT(D3DFMT_X8R8G8B8)
            else if (strcmp(szBackBufferFormat, 'a8r8g8b8') = 0) then
              g_EmuCDPD.pPresentationParameters.BackBufferFormat := X_D3DFORMAT(D3DFMT_A8R8G8B8);
          end;
        end;

        // detect vertex processing capabilities
        if ((g_D3DCaps.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT) > 0) and (g_EmuCDPD.DeviceType = D3DDEVTYPE_HAL) then
        begin
{$IFDEF DEBUG}
          DbgPrintf('EmuD3D8 : Using hardware vertex processing');
{$ENDIF}

          g_EmuCDPD.BehaviorFlags := D3DCREATE_HARDWARE_VERTEXPROCESSING;
          g_dwVertexShaderUsage := 0;
        end
        else
        begin
{$IFDEF DEBUG}
          DbgPrintf('EmuD3D8 : Using software vertex processing');
{$ENDIF}

          g_EmuCDPD.BehaviorFlags := D3DCREATE_SOFTWARE_VERTEXPROCESSING;
          g_dwVertexShaderUsage := D3DUSAGE_SOFTWAREPROCESSING;
        end;
// [PatrickvL] Reviewed up to here
        // redirect to windows Direct3D
        g_EmuCDPD.hRet := IDirect3D8(g_pD3D8).CreateDevice
        (
          g_EmuCDPD.Adapter,
          g_EmuCDPD.DeviceType,
          g_EmuCDPD.hFocusWindow,
          g_EmuCDPD.BehaviorFlags,
          {var}PD3DPresentParameters(g_EmuCDPD.pPresentationParameters)^,
          PIDirect3DDevice8(g_EmuCDPD.ppReturnedDeviceInterface)
        );

        // report error
        if (FAILED(g_EmuCDPD.hRet)) then
        begin
          // TODO -oDXBX: Use DXGetErrorDescription(g_EmuCDPD.hRet); (requires another DLL though)
          if (g_EmuCDPD.hRet = D3DERR_INVALIDCALL) then
            DxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Invalid Call)')
          else if (g_EmuCDPD.hRet = D3DERR_NOTAVAILABLE) then
            DxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Not Available)')
          else if (g_EmuCDPD.hRet = D3DERR_OUTOFVIDEOMEMORY) then
            DxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Out of Video Memory)');

          DxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Unknown)');
        end;

        // cache device pointer
        g_pD3DDevice8 := g_EmuCDPD.ppReturnedDeviceInterface^;

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
            DxbxKrnlCleanup('Could not initialize DirectDraw7');

          hRet := IDirectDraw7(g_pDD7).SetCooperativeLevel(0, DDSCL_NORMAL);
          if (FAILED(hRet)) then
            DxbxKrnlCleanup('Could not set cooperative level');
        end;

        // check for YUY2 overlay support
        // TODO -oCXBX: accept other overlay types
        begin
          dwCodes := 0;
          lpCodes := nil;
          IDirectDraw7(g_pDD7).GetFourCCCodes({var}dwCodes, lpCodes);
          lpCodes := DxbxMalloc(dwCodes*sizeof(DWORD));
          IDirectDraw7(g_pDD7).GetFourCCCodes({var}dwCodes, lpCodes);

          g_bSupportsYUY2 := false;
          if dwCodes > 0 then // Dxbx addition, to prevent underflow
          for v := 0 to dwCodes - 1 do
          begin
            if (PDWORDs(lpCodes)[v] = MAKEFOURCC('Y', 'U', 'Y', '2')) then
            begin
              g_bSupportsYUY2 := true;
              break;
            end;
          end;

          DxbxFree(lpCodes);

          if (not g_bSupportsYUY2) then
            EmuWarning('YUY2 overlays are not supported in hardware, could be slow!');

          // Does the user want to use Hardware accelerated YUV surfaces?
          if g_bSupportsYUY2 and g_XBVideo.GetHardwareYUV() then
            DbgPrintf('EmuD3D8 : Hardware accelerated YUV surfaces Enabled...');

          if not g_XBVideo.GetHardwareYUV() then
          begin
            g_bSupportsYUY2 := false;
            DbgPrintf('EmuD3D8 : Hardware accelerated YUV surfaces Disabled...');
          end
        end;

        // initialize primary surface
        if (g_bSupportsYUY2) then
        begin
          ZeroMemory(@ddsd2, sizeof(ddsd2));

          ddsd2.dwSize := sizeof(ddsd2);
          ddsd2.dwFlags := DDSD_CAPS;
          ddsd2.ddsCaps.dwCaps := DDSCAPS_PRIMARYSURFACE;

          hRet := IDirectDraw7(g_pDD7).CreateSurface(ddsd2, @g_pDDSPrimary, nil);

          if (FAILED(hRet)) then
            DxbxKrnlCleanup('Could not create primary surface (0x%.08X)', [hRet]);
        end;

        // update render target cache
        New({var X_D3DSurface:}g_pCachedRenderTarget);
        g_pCachedRenderTarget.Common := 0;
        g_pCachedRenderTarget.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_D3DREND;
        IDirect3DDevice8_GetRenderTarget(g_pD3DDevice8, @(g_pCachedRenderTarget.Emu.Surface8));

        // update z-stencil surface cache
        New({var}g_pCachedZStencilSurface);
        g_pCachedZStencilSurface.Common := 0;
        g_pCachedZStencilSurface.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_D3DSTEN;
        IDirect3DDevice8_GetDepthStencilSurface(g_pD3DDevice8, @(g_pCachedZStencilSurface.Emu.Surface8));

        IDirect3DDevice8(g_pD3DDevice8).CreateVertexBuffer
        (
          {Length=}1,
          {Usage=}0,
          {FVF=}0,
          {Pool=}D3DPOOL_MANAGED,
          {ppVertexBuffer=}@g_pDummyBuffer
        );

        for Streams := 0 to 8-1 do
        begin
          // Dxbx note : Why do we need a dummy stream at all?
          IDirect3DDevice8(g_pD3DDevice8).SetStreamSource(Streams, IDirect3DVertexBuffer8(g_pDummyBuffer), 1);
        end;

        // begin scene
        IDirect3DDevice8(g_pD3DDevice8).BeginScene();

        // initially, show a black screen
        IDirect3DDevice8(g_pD3DDevice8).Clear(0, nil, D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER, $FF000000, 1.0, 0);
        IDirect3DDevice8(g_pD3DDevice8).Present(nil, nil, 0, nil);

        // signal completion
        g_EmuCDPD.bReady := false;
      end
      else // not bCreate
      begin
        // release direct3d
        if (g_pD3DDevice8 <> nil) then
        begin
{$IFDEF DEBUG}
          DbgPrintf('EmuD3D8 : CreateDevice proxy thread releasing old Device.');
{$ENDIF}

          IDirect3DDevice8(g_pD3DDevice8).EndScene();

          g_EmuCDPD.hRet := IDirect3DDevice8(g_pD3DDevice8)._Release();

          if (g_EmuCDPD.hRet = 0) then
            g_pD3DDevice8 := nil;
        end;

        if (g_bSupportsYUY2) then
        begin
          // cleanup directdraw surface
          if (g_pDDSPrimary <> nil) then
          begin
            IDirectDrawSurface7(g_pDDSPrimary)._Release();
            g_pDDSPrimary := nil;
          end;
        end;

        // cleanup directdraw
        if (g_pDD7 <> nil) then
        begin
          IDirectDraw7(g_pDD7)._Release();
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
  // 0xEEEEEEEE and 0xFFFFFFFF are somehow set in Halo :(
  if (pResource.Emu.Lock <> 0) and (pResource.Emu.Lock <> $EEEEEEEE) and (pResource.Emu.Lock <> $FFFFFFFF) then
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
        pResource.Emu.Resource8 := g_EmuD3DResourceCache[v].Emu.Resource8;
        Exit;
      end;
    end;

  EmuSwapFS(fsXbox);
  XTL_EmuIDirect3DResource8_Register(pResource, nil(*PVOID(pResource.Data)*));
  EmuSwapFS(fsWindows);

  if (pResource.Emu.Lock <> X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
  begin
    for v := 0 to RESOURCE_CACHE_SIZE-1 do
    begin
      if (g_EmuD3DResourceCache[v].Data = 0) then
      begin
        g_EmuD3DResourceCache[v].Data := pResource.Data;
        g_EmuD3DResourceCache[v].Emu.Resource8 := pResource.Emu.Resource8;
        break;
      end;

      // DXBX MARKED OUT - CXBX WAS TESTING 16, but that was never hit... we do hit it.
      //if (v = RESOURCE_CACHE_SIZE-1) then // Dxbx note : Cxbx uses 16 here, but that's just plain wrong!
      //  DxbxKrnlCleanup('X_D3DResource cache is maxed out!');
    end;
  end;
end; // EmuVerifyResourceIsRegistered

// ensure a given width/height are powers of 2
procedure EmuAdjustPower2(dwWidth: PUINT; dwHeight: PUINT);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  NewWidth, NewHeight: UINT;
  v: int;
  mask: int;
begin
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
end; // EmuAdjustPower2

function XTL_EmuIDirect3D8_CreateDevice
(
    Adapter: UINT;
    DeviceType: D3DDEVTYPE;
    hFocusWindow: HWND;
    BehaviorFlags: DWORD;
    pPresentationParameters: PX_D3DPRESENT_PARAMETERS;
    ppReturnedDeviceInterface: XTL_PPIDirect3DDevice8
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_CreateDevice' +
      #13#10'(' +
      #13#10'   Adapter                   : 0x%.08X' +
      #13#10'   DeviceType                : 0x%.08X' +
      #13#10'   hFocusWindow              : 0x%.08X' +
      #13#10'   BehaviorFlags             : 0x%.08X' +
      #13#10'   pPresentationParameters   : 0x%.08X' +
      #13#10'   ppReturnedDeviceInterface : 0x%.08X' +
      #13#10');',
      [ Adapter, Ord(DeviceType), hFocusWindow,
        BehaviorFlags, pPresentationParameters, ppReturnedDeviceInterface]);

  // Print a few of the pPresentationParameters contents to the console
  DbgPrintf('BackBufferWidth:        = %d' +
      #13#10'BackBufferHeight:       = %d' +
      #13#10'BackBufferFormat:       = 0x%.08X' +
      #13#10'BackBufferCount:        = 0x%.08X' +
      #13#10'SwapEffect:             = 0x%.08X' +
      #13#10'EnableAutoDepthStencil: = 0x%.08X' +
      #13#10'AutoDepthStencilFormat: = 0x%.08X' +
      #13#10'Flags:                  = 0x%.08X',
      [ pPresentationParameters.BackBufferWidth, pPresentationParameters.BackBufferHeight,
        pPresentationParameters.BackBufferFormat, pPresentationParameters.BackBufferCount,
        Ord(pPresentationParameters.SwapEffect), pPresentationParameters.EnableAutoDepthStencil,
        pPresentationParameters.AutoDepthStencilFormat, pPresentationParameters.Flags]);

{$ENDIF}

  // Cache parameters
  g_EmuCDPD.Adapter := Adapter;
  g_EmuCDPD.DeviceType := DeviceType;
  g_EmuCDPD.hFocusWindow := hFocusWindow;
  g_EmuCDPD.BehaviorFlags := BehaviorFlags; // Dxbx addition
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

  EmuSwapFS(fsXbox);

  Result := g_EmuCDPD.hRet;
end; // XTL_EmuIDirect3D8_CreateDevice

function XTL_EmuIDirect3DDevice8_IsBusy: BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_IsBusy();');
{$ENDIF}

  EmuWarning('EmuIDirect3DDevice8_IsBusy ignored!');

  EmuSwapFS(fsXbox);

  Result := g_bIsBusy; // Dxbx note : We read a boolean that's only true while inside Present()
end; // XTL_EmuIDirect3DDevice8_IsBusy

procedure XTL_EmuIDirect3DDevice8_GetCreationParameters(pParameters: PD3DDEVICE_CREATION_PARAMETERS); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetCreationParameters' +
    #13#10'(' +
    #13#10'   pParameters             : 0x%.08X' +
    #13#10');',
    [pParameters]);
{$ENDIF}

  pParameters.AdapterOrdinal := D3DADAPTER_DEFAULT;
  pParameters.DeviceType := D3DDEVTYPE_HAL;
  pParameters.hFocusWindow := 0;
  pParameters.BehaviorFlags := D3DCREATE_HARDWARE_VERTEXPROCESSING;

  EmuSwapFS(fsXbox);
end; // XTL_EmuIDirect3DDevice8_GetCreationParameters

function XTL_EmuIDirect3D8_CheckDeviceFormat
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_CheckDeviceFormat' +
    #13#10'(' +
    #13#10'   Adapter                 : 0x%.08X' +
    #13#10'   DeviceType              : 0x%.08X' +
    #13#10'   AdapterFormat           : 0x%.08X' +
    #13#10'   Usage                   : 0x%.08X' +
    #13#10'   RType                   : 0x%.08X' +
    #13#10'   CheckFormat             : 0x%.08X' +
    #13#10');',
    [Adapter, Ord(DeviceType), AdapterFormat,
    Usage, Ord(RType), CheckFormat]);
{$ENDIF}

  if (RType > X_D3DRTYPE_INDEXBUFFER) then
    DxbxKrnlCleanup('RType > 7 (X_D3DRTYPE_INDEXBUFFER)');

  Result := IDirect3D8(g_pD3D8).CheckDeviceFormat
  (
    g_XBVideo.GetDisplayAdapter(),
    iif(g_XBVideo.GetDirect3DDevice() = 0, D3DDEVTYPE_HAL, D3DDEVTYPE_REF),
    EmuXB2PC_D3DFormat(AdapterFormat),
    Usage, _D3DRESOURCETYPE(RType), EmuXB2PC_D3DFormat(CheckFormat)
  );

  EmuSwapFS(fsXbox);
end; // XTL_EmuIDirect3D8_CheckDeviceFormat

procedure XTL_EmuIDirect3DDevice8_GetDisplayFieldStatus(pFieldStatus: PX_D3DFIELD_STATUS); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetDisplayFieldStatus' +
    #13#10'(' +
    #13#10'   pFieldStatus            : 0x%.08X' +
    #13#10');',
    [pFieldStatus]);
{$ENDIF}

//  pFieldStatus.Field := X_D3DFIELDTYPE(iif(g_VBData.VBlankCounter and 1 = 0, Ord(X_D3DFIELD_ODD), Ord(X_D3DFIELD_EVEN)));
//  pFieldStatus.VBlankCount := g_VBData.VBlankCounter;
	pFieldStatus.Field := X_D3DFIELD_PROGRESSIVE;
	pFieldStatus.VBlankCount := 0;


  EmuSwapFS(fsXbox);
end; // XTL_EmuIDirect3DDevice8_GetDisplayFieldStatus

function XTL_EmuIDirect3DDevice8_BeginPush(Count: DWORD): PDWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_BeginPush(%d);', [Count]);
{$ENDIF}

  Result := calloc(Count, sizeof(DWORD)); // Cxbx: new DWORD[Count]
  g_dwPrimaryPBCount := Count;
  g_pPrimaryPB := Result;

  EmuSwapFS(fsXbox);
end; // XTL_EmuIDirect3DDevice8_BeginPush

procedure XTL_EmuIDirect3DDevice8_EndPush(pPush: PDWORD); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_EndPush(0x%.08X);', [pPush]);
{$ENDIF}

  XTL_EmuExecutePushBufferRaw(g_pPrimaryPB);

  Free(g_pPrimaryPB); // Cxbc: delete[]
  g_pPrimaryPB := nil;

  EmuSwapFS(fsXbox);
end; // XTL_EmuIDirect3DDevice8_EndPush

function XTL_EmuIDirect3DDevice8_BeginVisibilityTest(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF DEBUG}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_BeginVisibilityTest();');
  EmuSwapFS(fsXbox);
{$ENDIF}

  Result := D3D_OK;
end;

function XTL_EmuIDirect3DDevice8_EndVisibilityTest
(
    Index: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF DEBUG}
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_EndVisibilityTest' +
    #13#10'(' +
    #13#10'   Index                   : 0x%.08X' +
    #13#10');',
    [Index]);

  EmuSwapFS(fsXbox);
{$ENDIF}

  Result := D3D_OK;
end;

procedure XTL_EmuIDirect3DDevice8_SetBackBufferScale(x, y: FLOAT); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetBackBufferScale' +
    #13#10'(' +
    #13#10'   x                       :  %f' +
    #13#10'   y                       :  %f' +
    #13#10');',
    [x, y]);
{$ENDIF}

  EmuWarning('SetBackBufferScale ignored');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_GetVisibilityTestResult
(
  Index: DWORD;
  {CONST} pResult: PUINT;
  {CONST} pTimeStamp: PULONGLONG
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetVisibilityTestResult' +
    #13#10'(' +
    #13#10'   Index                   : 0x%.08X' +
    #13#10'   pResult                 : 0x%.08X' +
    #13#10'   pTimeStamp              : 0x%.08X' +
    #13#10');',
    [Index, pResult, pTimeStamp]);
{$ENDIF}

  // TODO -oCXBX: actually emulate this!?

  if (pResult <> nil) then
    pResult^ := 640 * 480;

  if (pTimeStamp <> nil) then
    pTimeStamp^ := 0;

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

procedure XTL_EmuIDirect3DDevice8_GetDeviceCaps
(
    pCaps: PD3DCAPS8
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetDeviceCaps' +
    #13#10'(' +
    #13#10'   pCaps                     : 0x%.08X' +
    #13#10');',
    [pCaps]);
{$ENDIF}

  IDirect3D8(g_pD3D8).GetDeviceCaps(g_XBVideo.GetDisplayAdapter(), iif(g_XBVideo.GetDirect3DDevice() = 0, D3DDEVTYPE_HAL, D3DDEVTYPE_REF), {out}pCaps^);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_LoadVertexShader
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

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_LoadVertexShader' +
      #13#10'(' +
      #13#10'   Handle            : 0x%.08X' +
      #13#10'   Address           : 0x%.08X' +
      #13#10');',
      [Handle, Address]);
{$ENDIF}

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

function XTL_EmuIDirect3DDevice8_SelectVertexShader
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SelectVertexShader' +
    #13#10'(' +
    #13#10'   Handle            : 0x%.08X' +
    #13#10'   Address           : 0x%.08X' +
    #13#10');',
    [Handle, Address]);
{$ENDIF}

  if (VshHandleIsVertexShader(Handle)) then
  begin
    pVertexShader := PVERTEX_SHADER(VshHandleGetVertexShader(Handle).Handle);
    IDirect3DDevice8(g_pD3DDevice8).SetVertexShader(pVertexShader.Handle);
  end
  else if (Handle = HNULL) then
  begin
    IDirect3DDevice8(g_pD3DDevice8).SetVertexShader(D3DFVF_XYZ or D3DFVF_TEX0);
  end
  else if (Address < D3DVS_XBOX_NR_ADDRESS_SLOTS{=136}) then
  begin
    pVertexShader2 := PX_D3DVertexShader(g_VertexShaderSlots[Address]);

    if (pVertexShader2 <> NULL) then
    begin
      IDirect3DDevice8(g_pD3DDevice8).SetVertexShader(PVERTEX_SHADER(pVertexShader2.Handle).Handle);
    end
    else
    begin
      EmuWarning('g_VertexShaderSlots[%d] = 0', [Address]);
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuIDirect3D8_GetAdapterModeCount
(
  Adapter: UINT
): UINT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  ret: UINT;
  v: UINT32;
  Mode: D3DDISPLAYMODE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_GetAdapterModeCount' +
    #13#10'(' +
    #13#10'   Adapter                 : 0x%.08X' +
    #13#10');',
    [Adapter]);
{$ENDIF}

  ret := IDirect3D8(g_pD3D8).GetAdapterModeCount(g_XBVideo.GetDisplayAdapter());
  if ret > 0 then // Dxbx addition, to prevent underflow
  for v := 0 to ret - 1 do
  begin
    if (IDirect3D8(g_pD3D8).EnumAdapterModes(g_XBVideo.GetDisplayAdapter(), v, {out}Mode) <> D3D_OK) then
      break;

    if (Mode.Width <> 640) or (Mode.Height <> 480) then
      Dec(ret);
  end;

  EmuSwapFS(fsXbox);
  
  Result := ret;
end;

function XTL_EmuIDirect3D8_GetAdapterDisplayMode
(
  Adapter: UINT;
  pMode: PX_D3DDISPLAYMODE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pPCMode: PD3DDISPLAYMODE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_GetAdapterDisplayMode' +
    #13#10'(' +
    #13#10'   Adapter                 : 0x%.08X' +
    #13#10'   pMode                   : 0x%.08X' +
    #13#10');',
    [Adapter, pMode]);
{$ENDIF}

  // Cxbx NOTE: WARNING: We should cache the 'Emulated' display mode and return
  // This value. We can initialize the cache with the default Xbox mode data.
  Result := IDirect3D8(g_pD3D8).GetAdapterDisplayMode
  (
    g_XBVideo.GetDisplayAdapter(), 
    {out}PD3DDISPLAYMODE(pMode)^
  );

  // make adjustments to the parameters to make sense with windows direct3d
  begin
    pPCMode := PD3DDISPLAYMODE(pMode);

    // Convert Format (PC->Xbox)
    pMode.Format := EmuPC2XB_D3DFormat(pPCMode.Format);

    // TODO -oCXBX: Make this configurable in the future?
    pMode.Flags := X_D3DPRESENTFLAG_FIELD or X_D3DPRESENTFLAG_INTERLACED or X_D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;

    // TODO -oCXBX: Retrieve from current CreateDevice settings?
    pMode.Width := 640;
    pMode.Height := 480;
  end;

  EmuSwapFS(fsXbox);
end;

{static}var ModeAdder: uint = 0; // Dxbx note : Changed Cxbx's int to uint to prevent warning
function XTL_EmuIDirect3D8_EnumAdapterModes
(
  Adapter: UINT;
  Mode: UINT;
  pMode: PX_D3DDISPLAYMODE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  PCMode: D3DDISPLAYMODE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_EnumAdapterModes' +
    #13#10'(' +
    #13#10'   Adapter                 : 0x%.08X' +
    #13#10'   Mode                    : 0x%.08X' +
    #13#10'   pMode                   : 0x%.08X' +
    #13#10');',
    [Adapter, Mode, pMode]);
{$ENDIF}

  Result := D3D_OK;

  if (Mode = 0) then
    ModeAdder := 0;

  while true do
  begin
    Result := IDirect3D8(g_pD3D8).EnumAdapterModes(g_XBVideo.GetDisplayAdapter(), Mode+ModeAdder, D3DDISPLAYMODE(PCMode));
    if (Result <> D3D_OK) or (PCMode.Width = 640) and (PCMode.Height = 480) then
      break;

    Inc(ModeAdder);
  end;

  // make adjustments to parameters to make sense with windows direct3d
  if (Result = D3D_OK) then
  begin
    //
    // NOTE: WARNING: PC D3DDISPLAYMODE is different than Xbox D3DDISPLAYMODE!
    //

    // Convert Format (PC->Xbox)
    pMode.Width := PCMode.Width;
    pMode.Height := PCMode.Height;
    pMode.RefreshRate := PCMode.RefreshRate;

    // TODO -oCXBX: Make this configurable in the future?
    pMode.Flags := X_D3DPRESENTFLAG_FIELD or X_D3DPRESENTFLAG_INTERLACED or X_D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;
    pMode.Format := EmuPC2XB_D3DFormat(PCMode.Format)
  end
  else
    Result := D3DERR_INVALIDCALL;

  EmuSwapFS(fsXbox);
end; // XTL_EmuIDirect3D8_EnumAdapterModes

procedure XTL_EmuIDirect3D8_KickOffAndWaitForIdle(); stdcall; // UNKNOWN_SIGNATURE
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_KickOffAndWaitForIdle();');
{$ENDIF}

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetGammaRamp
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetGammaRamp' +
    #13#10'(' +
    #13#10'   dwFlags           : 0x%.08X' +
    #13#10'   pRamp             : 0x%.08X' +
    #13#10');',
    [dwFlags, pRamp]);
{$ENDIF}

  // remove D3DSGR_IMMEDIATE
  dwPCFlags := dwFlags and (not $00000002);

  for v := 0 to 255-1 do
  begin
      PCRamp.red[v] := pRamp.red[v];
      PCRamp.green[v] := pRamp.green[v];
      PCRamp.blue[v] := pRamp.blue[v];
   end;

  IDirect3DDevice8(g_pD3DDevice8).SetGammaRamp(dwPCFlags, PCRamp);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_AddRef(): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_AddRef();');
{$ENDIF}

  Result := ULONG(IDirect3DDevice8(g_pD3DDevice8)._AddRef());

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_BeginStateBlock(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_BeginStateBlock();');
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).BeginStateBlock();

  EmuSwapFS(fsXbox);
end;

{ MARKED OUT BY CXBX
function XTL_EmuIDirect3DDevice8_BeginStateBig(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  ret: ULONG;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}{
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_BeginStateBig();');
{$ENDIF}{
  ret := IDirect3DDevice8(g_pD3DDevice8).BeginStateBlock();

  DxbxKrnlCleanup('BeginStateBig is not implemented');
  EmuSwapFS(fsXbox);
  Result := ret;
end; }

function XTL_EmuIDirect3DDevice8_CaptureStateBlock(Token: DWORD): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CaptureStateBlock' +
    #13#10'(' +
    #13#10'   Token             : 0x%.08X' +
    #13#10');',
    [Token]);
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).CaptureStateBlock(Token);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_ApplyStateBlock(Token: DWORD): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_ApplyStateBlock' +
    #13#10'(' +
    #13#10'   Token             : 0x%.08X' +
    #13#10');',
    [Token]);
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).ApplyStateBlock(Token);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_EndStateBlock(pToken: PDWORD): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_EndStateBlock' +
    #13#10'(' +
    #13#10'   pToken            : 0x%.08X' +
    #13#10');',
    [pToken]);
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).EndStateBlock({out}pToken^);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_CopyRects
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CopyRects' +
    #13#10'(' +
    #13#10'   pSourceSurface    : 0x%.08X' +
    #13#10'   pSourceRectsArray : 0x%.08X' +
    #13#10'   cRects            : 0x%.08X' +
    #13#10'   pDestinationSurface: 0x%.08X' +
    #13#10'   pDestPointsArray  : 0x%.08X' +
    #13#10');',
    [pSourceSurface, pSourceRectsArray, cRects,
    pDestinationSurface, pDestPointsArray]);
{$ENDIF}

  // Dxbx addition : This is safer than a hardcoded call to UnlockRect like Cxbx does :
  DxbxUnlockD3DResource(pSourceSurface); // Dxbx addition
//  IDirect3DSurface8(pSourceSurface.Emu.Surface8).UnlockRect();

  { MARKED OUT BY CXBX
  kthx := 0;
  sprintf(FileName, 'C:\Aaron\Textures\SourceSurface-%d.bmp', [kthx]); Inc(kthx);

  D3DXSaveSurfaceToFileA(FileName, D3DXIFF_BMP, pSourceSurface.EmuSurface8, nil, nil);
  }

  Result := IDirect3DDevice8(g_pD3DDevice8).CopyRects
  (
    IDirect3DSurface8(pSourceSurface.Emu.Surface8),
    pSourceRectsArray,
    cRects,
    IDirect3DSurface8(pDestinationSurface.Emu.Surface8),
    pDestPointsArray
  );

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_CreateImageSurface
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreateImageSurface' +
    #13#10'(' +
    #13#10'   Width             : 0x%.08X' +
    #13#10'   Height            : 0x%.08X' +
    #13#10'   Format            : 0x%.08X' +
    #13#10'   ppBackBuffer      : 0x%.08X' +
    #13#10');',
    [Width, Height, Ord(Format), ppBackBuffer]);
{$ENDIF}

  New({var PX_D3DSurface}ppBackBuffer^);

  PCFormat := EmuXB2PC_D3DFormat(Format);
  
  Result := IDirect3DDevice8_CreateImageSurface(g_pD3DDevice8,
    Width,
    Height,
    PCFormat,
    PIDirect3DSurface8(@(ppBackBuffer^.Emu.Surface8)));
  DbgPrintf('Created image surface @ 0x%08X [hRet = 0x%08X]: %dx%d, format = 0x%08X', [
        ppBackBuffer^.Emu.Surface8,
        Result,
        Width, Height, Ord(PCFormat)]);

  if FAILED(Result) and (Format = X_D3DFMT_LIN_D24S8) then
  begin
    EmuWarning('CreateImageSurface: D3DFMT_LIN_D24S8 -> D3DFMT_A8R8G8B8');

    Result := IDirect3DDevice8_CreateImageSurface(g_pD3DDevice8,
      Width,
      Height,
      D3DFMT_A8R8G8B8,
      PIDirect3DSurface8(@(ppBackBuffer^.Emu.Surface8)));
    DbgPrintf('Created image surface @ 0x%08X [hRet = 0x%08X]: %dx%d, format = 0x%08X', [
        ppBackBuffer^.Emu.Surface8,
        Result,
        Width, Height, Ord(D3DFMT_A8R8G8B8)]);
  end;

  if FAILED(Result) then
    {EmuWarning}DxbxKrnlCleanup('CreateImageSurface failed! ' + #13#10 + 'Format = 0x%8.8X', [Format]);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_GetGammaRamp
(
    pRamp: PX_D3DGAMMARAMP
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pGammaRamp: PD3DGAMMARAMP;
  v: int;        
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetGammaRamp' +
    #13#10'(' +
    #13#10'   pRamp             : 0x%.08X' +
    #13#10');',
    [pRamp]);
{$ENDIF}

  pGammaRamp := PD3DGAMMARAMP(malloc(sizeof(D3DGAMMARAMP)));

  IDirect3DDevice8(g_pD3DDevice8).GetGammaRamp({out}pGammaRamp^);

  for v := 0 to 256-1 do
  begin
    pRamp.red[v] := BYTE(pGammaRamp.red[v]);
    pRamp.green[v] := BYTE(pGammaRamp.green[v]);
    pRamp.blue[v] := BYTE(pGammaRamp.blue[v]);
  end;

  free(pGammaRamp);

  EmuSwapFS(fsXbox);
end;

{static}var pBackBuffer: PX_D3DSurface = nil;
function XTL_EmuIDirect3DDevice8_GetBackBuffer2
(
    BackBuffer: INT
): PX_D3DSurface; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetBackBuffer2' +
    #13#10'(' +
    #13#10'   BackBuffer            : 0x%.08X' +
    #13#10');',
    [BackBuffer]);
{$ENDIF}

{ unsafe, somehow  -- MARKED OUT BY CXBX --
    HRESULT hRet := S_OK;

    X_D3DSurface *pBackBuffer := new X_D3DSurface();

    if (BackBuffer = -1) then
    begin
         {static?}{pCachedPrimarySurface: XTL_PIDirect3DSurface8 := nil;

        if (pCachedPrimarySurface = nil) then
        begin
            // create a buffer to return
            // TODO -oCXBX: Verify the surface is always 640x480
            IDirect3DDevice8(g_pD3DDevice8).CreateImageSurface(640, 480, D3DFMT_A8R8G8B8, {out}{IDirect3DSurface8(pCachedPrimarySurface));
         end;

        pBackBuffer.Emu.Surface8 := pCachedPrimarySurface;

        hRet := IDirect3DDevice8(g_pD3DDevice8).GetFrontBuffer(pBackBuffer.Emu.Surface8);

        if (FAILED(hRet)) then
        begin
            EmuWarning('Could not retrieve primary surface, using backbuffer');
            pCachedPrimarySurface := nil;
            pBackBuffer.Emu.Surface8._Release();
            pBackBuffer.Emu.Surface8 := nil;
            BackBuffer := 0;
         end;

        // Debug: Save this image temporarily
        //D3DXSaveSurfaceToFileA('C:\Aaron\Textures\FrontBuffer.bmp', D3DXIFF_BMP, pBackBuffer.Emu.Surface8, NULL, NULL);
     end;

    if (BackBuffer <> -1) then
        hRet := IDirect3DDevice8(g_pD3DDevice8).GetBackBuffer(BackBuffer, D3DBACKBUFFER_TYPE_MONO, @(pBackBuffer.Emu.Surface8));
}

  if pBackBuffer = nil then // Dxbx addition, to initialize this 'static' var only once
    New({var PX_D3DSurface}pBackBuffer);

  if (BackBuffer = -1) then
      BackBuffer := 0; // TODO : Use actual FrontBuffer number here

  hRet := IDirect3DDevice8(g_pD3DDevice8).GetBackBuffer(BackBuffer, D3DBACKBUFFER_TYPE_MONO, @(pBackBuffer.Emu.Surface8));

  if (hRet <> D3D_OK) then
    DxbxKrnlCleanup('Unable to retrieve back buffer');

  // update data pointer
  pBackBuffer.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_SURFACE;

  EmuSwapFS(fsXbox);

  Result := pBackBuffer;
end;

function XTL_EmuIDirect3DDevice8_GetBackBuffer
(
  BackBuffer: INT;
  Type_: D3DBACKBUFFER_TYPE;
  ppBackBuffer: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetBackBuffer >>' +
      #13#10'(' +
      #13#10'   BackBuffer        : 0x%.08X' +
      #13#10'   Type              : 0x%.08X' +
      #13#10'   ppBackBuffer      : 0x%.08X' +
      #13#10');',
      [BackBuffer, Ord(Type_), ppBackBuffer]);
    EmuSwapFS(fsXbox);
  end;
{$ENDIF}

  // Allow backbuffer -1 and up to the allocated BackBufferCount, but nothing beyond this :
  if (BackBuffer < -1) or (BackBuffer >= int(g_EmuCDPD.pPresentationParameters.BackBufferCount)) then
    Result := D3DERR_INVALIDCALL
  else
  begin
    ppBackBuffer^ := XTL_EmuIDirect3DDevice8_GetBackBuffer2(BackBuffer);

    Result := D3D_OK;
  end;
end;

function XTL_EmuIDirect3DDevice8_SetViewport
(
  pViewport: PD3DVIEWPORT8
): HRESULT; stdcall;
// Branch:shogun  Revision:162+StrickerX3_patch  Translator:Shadow_Tj  Done:100
(*
var
  dwX: DWORD;
  dwY: DWORD;
  dwWidth: DWORD;
  dwHeight: DWORD;
  currentViewport: D3DVIEWPORT8;
*)
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetViewport' +
    #13#10'(' +
    #13#10'   pViewport         : 0x%.08X (%d, %d, %d, %d, %f, %f)' +
    #13#10');',
    [pViewport, pViewport.X, pViewport.Y, pViewport.Width,
    pViewport.Height, pViewport.MinZ, pViewport.MaxZ]);
{$ENDIF}

(*
  dwX := pViewport.X;
  dwY := pViewport.Y;
  dwWidth := pViewport.Width;
  dwHeight := pViewport.Height;

  IDirect3DDevice8(g_pD3DDevice8).GetViewport({out}currentViewport);
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
  Result := IDirect3DDevice8(g_pD3DDevice8).SetViewport(pViewport^);

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
    EmuWarning('Unable to set viewport!');
    Result := D3D_OK;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_GetViewport
(
  pViewport: PD3DVIEWPORT8
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetViewport' +
    #13#10'(' +
    #13#10'   pViewport         : 0x%.08X' +
    #13#10');',
    [pViewport]);
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).GetViewport({out}pViewport^);

  if FAILED(Result) then
  begin
    EmuWarning('Unable to get viewport!');
    Result := D3D_OK;
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_GetViewportOffsetAndScale
(
    pOffset: PD3DXVECTOR4;
    pScale: PD3DXVECTOR4
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
{  fScaleX: Float;
  fScaleY: Float;
  fScaleZ: Float;
  fOffsetX: Float;
  fOffsetY: Float;}
  Viewport: D3DVIEWPORT8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetViewportOffsetAndScale' +
    #13#10'(' +
    #13#10'   pOffset           : 0x%.08X' +
    #13#10'   pScale            : 0x%.08X' +
    #13#10');',
    [pOffset, pScale]);
{$ENDIF}

{
  Never used because Cxbx marked them out below
  fScaleX := 1.0;
  fScaleY := 1.0;
  fScaleZ := 1.0;
  fOffsetX := 0.5 + 1.0 / 32;
  fOffsetY := 0.5 + 1.0 / 32;
}

  EmuSwapFS(fsXbox);
  XTL_EmuIDirect3DDevice8_GetViewport(@Viewport);
  EmuSwapFS(fsWindows);

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

function XTL_EmuIDirect3DDevice8_SetShaderConstantMode
(
  Mode: X_VERTEXSHADERCONSTANTMODE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetShaderConstantMode' +
    #13#10'(' +
    #13#10'   Mode              : 0x%.08X' +
    #13#10');',
    [Mode]);
{$ENDIF}

  g_VertexShaderConstantMode := Mode;
  EmuSwapFS(fsXbox);

  Result := S_OK;
end;


function XTL_EmuIDirect3DDevice8_Reset
(
  pPresentationParameters: PX_D3DPRESENT_PARAMETERS
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_Reset' +
    #13#10'(' +
    #13#10'   pPresentationParameters: 0x%.08X' +
    #13#10');',
    [pPresentationParameters]);
{$ENDIF}
  EmuWarning('Device Reset is being utterly ignored');

  EmuSwapFS(fsXbox);
  Result := D3D_OK;
end;

function XTL_EmuIDirect3DDevice8_GetRenderTarget
(
  ppRenderTarget: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pSurface8: XTL_PIDirect3DSurface8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetRenderTarget' +
    #13#10'(' +
    #13#10'   ppRenderTarget    : 0x%.08X' +
    #13#10');',
    [ppRenderTarget]);
{$ENDIF}

  pSurface8 := g_pCachedRenderTarget.Emu.Surface8;

  IDirect3DSurface8(pSurface8)._AddRef();

  ppRenderTarget^ := g_pCachedRenderTarget;

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : RenderTarget := 0x%.08X', [pSurface8]);
{$ENDIF}

  // TODO -oDXBX: Should we nil-out pSurface ?

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuIDirect3DDevice8_GetRenderTarget2(): PX_D3DSurface; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pSurface8: XTL_PIDirect3DSurface8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetRenderTarget2();');
{$ENDIF}

  pSurface8 := g_pCachedRenderTarget.Emu.Surface8;

  IDirect3DSurface8(pSurface8)._AddRef();

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : RenderTarget := 0x%.08X', [pSurface8]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  Result := g_pCachedRenderTarget;
end;

function XTL_EmuIDirect3DDevice8_GetDepthStencilSurface
(
  ppZStencilSurface: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pSurface8: XTL_PIDirect3DSurface8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetDepthStencilSurface' +
    #13#10'(' +
    #13#10'   ppZStencilSurface : 0x%.08X' +
    #13#10');',
    [ppZStencilSurface]);
{$ENDIF}

  pSurface8 := g_pCachedZStencilSurface.Emu.Surface8;

  if (pSurface8 <> nil) then
    IDirect3DSurface8(pSurface8)._AddRef();

  ppZStencilSurface^ := g_pCachedZStencilSurface;

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : DepthStencilSurface := 0x%.08X', [pSurface8]);
{$ENDIF}
  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuIDirect3DDevice8_GetDepthStencilSurface2(): PX_D3DSurface; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pSurface8: XTL_PIDirect3DSurface8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetDepthStencilSurface2();');
{$ENDIF}

  pSurface8 := g_pCachedZStencilSurface.Emu.Surface8;

  if (pSurface8 <> nil) then
    IDirect3DSurface8(pSurface8)._AddRef();

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : DepthStencilSurface := 0x%.08X', [pSurface8]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  Result := g_pCachedZStencilSurface;
end;

function XTL_EmuIDirect3DDevice8_GetTile
(
  Index: DWORD;
  {CONST} pTile: PX_D3DTILE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetTile' +
    #13#10'(' +
    #13#10'   Index             : 0x%.08X' +
    #13#10'   pTile             : 0x%.08X' +
    #13#10');',
    [Index, pTile]);
{$ENDIF}

  if (pTile <> NULL) then
    memcpy(pTile, @(g_EmuD3DTileCache[Index]), sizeof(X_D3DTILE));

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuIDirect3DDevice8_SetTileNoWait
(
  Index: DWORD;
  {CONST} pTile: PX_D3DTILE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetTileNoWait' +
    #13#10'(' +
    #13#10'   Index             : 0x%.08X' +
    #13#10'   pTile             : 0x%.08X' +
    #13#10');',
    [Index, pTile]);
{$ENDIF}

  if (pTile <> NULL) then
    memcpy(@(g_EmuD3DTileCache[Index]), pTile, sizeof(X_D3DTILE));

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

{$ifdef _DEBUG_TRACK_VS}
{static}var FailedShaderCount: int = 0;
{$endif}
function XTL_EmuIDirect3DDevice8_CreateVertexShader
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreateVertexShader' +
    #13#10'(' +
    #13#10'   pDeclaration        : 0x%.08X' +
    #13#10'   pFunction           : 0x%.08X' +
    #13#10'   pHandle             : 0x%.08X' +
    #13#10'   Usage               : 0x%.08X' +
    #13#10');',
    [pDeclaration, pFunction, pHandle, Usage]);
{$ENDIF}

  // create emulated shader struct
  pD3DVertexShader := PX_D3DVertexShader(DxbxMalloc(sizeof(X_D3DVertexShader)));
  pVertexShader := PVERTEX_SHADER(DxbxMalloc(sizeof(VERTEX_SHADER)));

  // TODO -oCXBX: Intelligently fill out these fields as necessary
  ZeroMemory(pD3DVertexShader, sizeof(X_D3DVertexShader));
  ZeroMemory(pVertexShader, sizeof(VERTEX_SHADER));

  // CXBX HACK:
  // TODO -oCXBX: support this situation
  if (pDeclaration = NULL) then
  begin
    pHandle^ := HNULL;
    EmuSwapFS(fsXbox);
    Result := S_OK;
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
                                        (g_VertexShaderConstantMode and X_VSCM_NONERESERVED) > 0); // Dxbx fix
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

{$IFDEF DEBUG}
 // DbgPrintf('MaxVertexShaderConst = %d', [g_D3DCaps.MaxVertexShaderConst]);
{$ENDIF}

  if (SUCCEEDED(hRet)) then
  begin
    if pRecompiledFunction = NULL then
    begin
      // Dxbx Note : As suggested by StrikerX3
      hRet := D3DERR_INVALIDCALL; // Use fallback if recompilation failed
    end
    else
    begin
      hRet := IDirect3DDevice8(g_pD3DDevice8).CreateVertexShader
      (
          pRecompiledDeclaration,
          pRecompiledFunction,
          {out}Handle,
          g_dwVertexShaderUsage   // TODO -oCXBX: HACK: Xbox has extensions!
      );
    end;
    if Assigned(pRecompiledBuffer) then
    begin
      ID3DXBuffer(pRecompiledBuffer)._Release();
      pRecompiledBuffer := NULL;
    end;

    //* Fallback to dummy shader.
    if (FAILED(hRet)) then
    begin
      EmuWarning('Trying fallback:'#13#10'%s', [dummy]);
      hRet := D3DXAssembleShader(dummy,
                                 strlen(dummy),
                                 {Flags=}D3DXASM_SKIPVALIDATION,
                                 {ppConstants}NULL,
                                 {ppCompiledShader}@pRecompiledBuffer,
                                 {ppCompilationErrors}NULL);
      if not (FAILED(hRet)) then // Dxbx addition
        hRet := IDirect3DDevice8(g_pD3DDevice8).CreateVertexShader
        (
            pRecompiledDeclaration,
            PDWORD(ID3DXBuffer(pRecompiledBuffer).GetBufferPointer()),
            {out}Handle,
            g_dwVertexShaderUsage
        );
    end;
    //*/
  end;

  // Save the status, to remove things later
  pVertexShader.Status := hRet;

  DxbxFree(pRecompiledDeclaration);

  pVertexShader.pDeclaration := PDWORD(DxbxMalloc(DeclarationSize));
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
      pVertexShader.pFunction := PDWORD(DxbxMalloc(VertexShaderSize));
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

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

procedure XTL_EmuIDirect3DDevice8_SetPixelShaderConstant
(
  Register_: DWORD;
  {CONST} pConstantData: PVOID;
  ConstantCount: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetPixelShaderConstant' +
    #13#10'(' +
    #13#10'   Register          : 0x%.08X' +
    #13#10'   pConstantData     : 0x%.08X' +
    #13#10'   ConstantCount     : 0x%.08X' +
    #13#10');',
    [Register_, pConstantData, ConstantCount]);
{$ENDIF}

(* Dxbx note : Is this what's needed?
  {hRet :=} IDirect3DDevice8(g_pD3DDevice8).SetPixelShaderConstant
  (
    Register_,
    {untyped const}pConstantData^,
    ConstantCount
  );
*)

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_SetVertexShaderConstant
(
  Register_: INT;
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexShaderConstant' +
    #13#10'(' +
    #13#10'   Register          : 0x%.08X' +
    #13#10'   pConstantData     : 0x%.08X' +
    #13#10'   ConstantCount     : 0x%.08X' +
    #13#10');',
    [Register_, pConstantData, ConstantCount]);
{$ENDIF}

{$IFDEF _DEBUG_TRACK_VS_CONST}
  if ConstantCount > 0 then // Dxbx addition, to prevent underflow
  for i := 0 to ConstantCount - 1 do
  begin
   {$IFDEF DEBUG}
      DbgPrintf('SetVertexShaderConstant, c%d (c%d) = { %f, %f, %f, %f }',
             [Register_ + X_VSCM_CORRECTION{=96} + i, Register_ + i, // Dxbx fix
             Pfloats(pConstantData)[4 * i],
             Pfloats(pConstantData)[4 * i + 1],
             Pfloats(pConstantData)[4 * i + 2],
             Pfloats(pConstantData)[4 * i + 3]]);
   {$ENDIF}
  end;
{$ENDIF}

  // TODO -oCXBX: HACK: Since Xbox vertex shader constants range from -96 to 95, during conversion
  // some shaders need to add 96 to use ranges 0 to 191.  This fixes 3911 - 4361 games and XDK
  // samples, but breaks Turok.
  // Dxbx note : 4627 samples show that the Register value arriving in this function is already
  // incremented with 96 (even though the code for these samples supplies 0, maybe there's a
  // macro responsible for that?)
  if g_BuildVersion <= 4361 then
    Inc(Register_, X_VSCM_CORRECTION{=96});

  hRet := IDirect3DDevice8(g_pD3DDevice8).SetVertexShaderConstant
  (
    Register_,
    pConstantData,
    ConstantCount
  );

  if (FAILED(hRet)) then
  begin
    EmuWarning('We''re lying about setting a vertex shader constant!');

    hRet := D3D_OK;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

procedure XTL_EmuIDirect3DDevice8_SetVertexShaderConstant1(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}const pConstantData: PVOID;
  {1 ECX}Register_: INT
  ); register; // VALIDATED fastcall simulation - See Translation guide
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexShaderConstant1' +
    #13#10'(' +
    #13#10'   Register          : 0x%.08X' +
    #13#10'   pConstantData     : 0x%.08X' +
    #13#10');',
    [Register_, pConstantData]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  // Dxbx note: Shouldn't we return the result of this call?
  XTL_EmuIDirect3DDevice8_SetVertexShaderConstant(Register_, pConstantData, 1);
end;

procedure XTL_EmuIDirect3DDevice8_SetVertexShaderConstant4(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}const pConstantData: PVOID;
  {1 ECX}Register_: INT
  ); register; // VALIDATED fastcall simulation - See Translation guide
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexShaderConstant4' +
      #13#10'(' +
      #13#10'   Register          : 0x%.08X' +
      #13#10'   pConstantData     : 0x%.08X' +
      #13#10');',
      [Register_, pConstantData]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  // Dxbx note: Shouldn't we return the result of this call?
  XTL_EmuIDirect3DDevice8_SetVertexShaderConstant(Register_, pConstantData, 4);
end;

procedure XTL_EmuIDirect3DDevice8_SetVertexShaderConstantNotInline(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}const pConstantData: PVOID;
  {1 ECX}Register_: INT;
  {3 stack}ConstantCount: DWORD
  ); register; // fastcall simulation - See Translation guide
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexShaderConstantNotInline' +
      #13#10'(' +
      #13#10'   Register          : 0x%.08X' +
      #13#10'   pConstantData     : 0x%.08X' +
      #13#10'   ConstantCount     : 0x%.08X' +
      #13#10');',
      [Register_, pConstantData, ConstantCount]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  // Dxbx note: Shouldn't we return the result of this call?
  XTL_EmuIDirect3DDevice8_SetVertexShaderConstant(Register_, pConstantData, ConstantCount div 4);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

procedure XTL_EmuIDirect3DDevice8_DeletePixelShader
(
  Handle: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DeletePixelShader' +
    #13#10'(' +
    #13#10'   Handle            : 0x%.08X' +
    #13#10');',
    [Handle]);
{$ENDIF}

  if (Handle = X_PIXELSHADER_FAKE_HANDLE) then
  begin
    // Cxbx: Do Nothing!
  end
  else
  begin
    IDirect3DDevice8(g_pD3DDevice8).DeletePixelShader(Handle);
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_CreatePixelShader
(
  pPSDef: PX_D3DPIXELSHADERDEF;
  pHandle: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pFunction: PDWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreatePixelShader' +
    #13#10'(' +
    #13#10'   pPSDef            : 0x%.08X' +
    #13#10'   pHandle           : 0x%.08X' +
    #13#10');',
    [pPSDef, pHandle]);
{$ENDIF}

  pFunction := PDWORD(pPSDef);

  // Attempt to recompile PixelShader
  XTL_EmuRecompilePshDef(pPSDef, NULL);

  // redirect to windows d3d
  Result := IDirect3DDevice8(g_pD3DDevice8).CreatePixelShader
  (
    pFunction,
    {out}pHandle^
  );

  if FAILED(Result) then
  begin
    pHandle^ := X_PIXELSHADER_FAKE_HANDLE;
    EmuWarning('We''re lying about the creation of a pixel shader!');
    Result := D3D_OK;
  end;

  EmuSwapFS(fsXbox);
end;

{static}var dwHandle: DWORD = 0;
function XTL_EmuIDirect3DDevice8_SetPixelShader
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetPixelShader' +
    #13#10'(' +
    #13#10'   Handle             : 0x%.08X' +
    #13#10');',
    [Handle]);
{$ENDIF}

  // redirect to windows d3d
  Result := D3D_OK;

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
      D3DXAssembleShader(szDiffusePixelShader, strlen(szDiffusePixelShader), {Flags=}0, {ppConstants=}NULL, {ppCompiledShader=}@pShader, {ppCompilationErrors}@pErrors);

      // create the shader device handle
      Result := IDirect3DDevice8(g_pD3DDevice8).CreatePixelShader(ID3DXBuffer(pShader).GetBufferPointer(), {out}dwHandle);

      // TODO -oDxbx : Shouldn't we release pShader here? (It could be a resource leak!)

      if (FAILED(Result)) then
      begin
        EmuWarning('Could not create pixel shader');
        EmuWarning(string(AnsiString(PAnsiChar(ID3DXBuffer(pErrors).GetBufferPointer)))); // Dxbx addition
      end;
    end;

    // TODO -oDxbx : Shouldn't we release pErrors here? (It could be a resource leak!)

    if (not FAILED(Result)) then
      Result := IDirect3DDevice8(g_pD3DDevice8).SetPixelShader(dwHandle);

    if (FAILED(Result)) then
      EmuWarning('Could not set pixel shader!');
    //*/

    g_bFakePixelShaderLoaded := TRUE;
  end
  // Fixed Pipeline, or Recompiled Programmable Pipeline
  else if (Handle = HNULL) then
  begin
    g_bFakePixelShaderLoaded := FALSE;
    IDirect3DDevice8(g_pD3DDevice8).SetPixelShader(Handle);
  end;

  if FAILED(Result) then
  begin
    EmuWarning('We''re lying about setting a pixel shader!');

    Result := D3D_OK;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_CreateTexture2
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
  pTexture: PX_D3DTexture;
begin
  // Dxbx note : No EmuSwapFS needed here

  case D3DResource of
     X_D3DRTYPE_TEXTURE: // 3
        XTL_EmuIDirect3DDevice8_CreateTexture(Width, Height, Levels, Usage, Format, D3DPOOL_MANAGED, @pTexture);
     X_D3DRTYPE_VOLUMETEXTURE: // 4
        XTL_EmuIDirect3DDevice8_CreateVolumeTexture(Width, Height, Depth, Levels, Usage, Format, D3DPOOL_MANAGED, @pTexture);
     X_D3DRTYPE_CUBETEXTURE: // 5
       // TODO -oDxbx : Check what the actual EdgeLength value should be
       XTL_EmuIDirect3DDevice8_CreateCubeTexture({EdgeLength=?}Width, Levels, Usage, Format, D3DPOOL_MANAGED, @pTexture);
  else
    EmuSwapFS(fsWindows);
    DxbxKrnlCleanup('D3DResource := %d is not supported!', [Ord(D3DResource)]);
    EmuSwapFS(fsXbox);
  end;
  
  Result := pTexture;
end;

function XTL_EmuIDirect3DDevice8_CreateTexture
(
  Width: UINT;
  Height: UINT;
  Levels: UINT;
  Usage: DWORD;
  Format: X_D3DFORMAT;
  Pool: D3DPOOL;
  ppTexture: PPX_D3DTexture
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  PCFormat: D3DFORMAT;
  PCUsage: DWORD;
  PCPool: D3DPOOL;
  LockedRect: D3DLOCKED_RECT;
  dwSize: DWORD;
  dwPtr: DWORD;
  pRefCount: PDWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreateTexture' +
     #13#10'(' +
     #13#10'   Width               : 0x%.08X' +
     #13#10'   Height              : 0x%.08X' +
     #13#10'   Levels              : 0x%.08X' +
     #13#10'   Usage               : 0x%.08X' +
     #13#10'   Format              : 0x%.08X' +
     #13#10'   Pool                : 0x%.08X' +
     #13#10'   ppTexture           : 0x%.08X' +
     #13#10');',
     [Width, Height, Levels, Usage, Ord(Format), Ord(Pool), ppTexture]);
{$ENDIF}

  // Convert Format (Xbox->PC)
  PCFormat := EmuXB2PC_D3DFormat(Format);

  // TODO -oCXBX: HACK: Devices that don't support this should somehow emulate it!
  if (PCFormat = D3DFMT_D16) then
  begin
    EmuWarning('D3DFMT_D16 is an unsupported texture format!');
    PCFormat := D3DFMT_R5G6B5;
  end
  else if (PCFormat = D3DFMT_P8) then
  begin
    EmuWarning('D3DFMT_P8 is an unsupported texture format!');
    PCFormat := D3DFMT_X8R8G8B8;
  end
  else if (PCFormat = D3DFMT_D24S8) then
  begin
    EmuWarning('D3DFMT_D24S8 is an unsupported texture format!');
    PCFormat := D3DFMT_X8R8G8B8;
  end
  else if (PCFormat = D3DFMT_YUY2) then
  begin
    // cache the overlay size
    g_dwOverlayW := Width;
    g_dwOverlayH := Height;
    g_dwOverlayP := RoundUp(g_dwOverlayW, 64) * 2;
  end;

  if (PCFormat <> D3DFMT_YUY2) then
  begin
//    PCUsage := Usage and (D3DUSAGE_RENDERTARGET);
    PCUsage := Usage and (D3DUSAGE_RENDERTARGET or D3DUSAGE_DEPTHSTENCIL);
    PCPool := D3DPOOL_MANAGED;

    if ((g_D3DCaps.TextureCaps and D3DPTEXTURECAPS_POW2) <> 0) then
    begin
      EmuAdjustPower2(@Width, @Height);
    end;

    New({var PX_D3DTexture}ppTexture^);

    if (Usage and (D3DUSAGE_RENDERTARGET or D3DUSAGE_DEPTHSTENCIL)) > 0 then
//    if (Usage and (D3DUSAGE_RENDERTARGET)) > 0 then
      PCPool := D3DPOOL_DEFAULT;

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

    Result := IDirect3DDevice8_CreateTexture
    (g_pD3DDevice8,
      Width, Height, Levels,
      PCUsage, // TODO -oCXBX: Xbox Allows a border to be drawn (maybe hack this in software ;[)
      PCFormat, PCPool, @(ppTexture^.Emu.Texture8)
    );

    DbgPrintf('Texture created @ 0x%08X [hRet = 0x%08X]: %dx%d, %d levels, usage = 0x%08X, format = 0x%08X', [
            ppTexture^.Emu.Texture8, Result, Width, Height, Levels, Ord(PCUsage), Ord(PCFormat)]);

    if (FAILED(Result)) then
    begin
      EmuWarning('CreateTexture Failed!');
      ppTexture^.Data := $BEADBEAD;
    end
    else
    begin
      // Dxbx addition : Check if LockRect actually succeeds :
      IDirect3DTexture8(ppTexture^.Emu.Texture8).LockRect(0, {out}LockedRect, NULL, 0);
      ppTexture^.Data := DWORD(LockedRect.pBits);
      ppTexture^.Format := Ord(Format) shl X_D3DFORMAT_FORMAT_SHIFT;

      g_DataToTexture.insert(ppTexture^.Data, ppTexture^);

      IDirect3DTexture8(ppTexture^.Emu.Texture8).UnlockRect(0);
    end;

{$IFDEF DEBUG}
    DbgPrintf('EmuD3D8 : Created Texture: 0x%.08X (0x%.08X)', [ppTexture^, ppTexture^.Emu.Texture8]);
{$ENDIF}
  end
  else
  begin
    dwSize := g_dwOverlayP * g_dwOverlayH;
    dwPtr := DWORD(DxbxMalloc(dwSize + sizeof(DWORD)));

    pRefCount := PDWORD(dwPtr + dwSize);

    // initialize ref count
    pRefCount^ := 1;

    // If YUY2 is not supported in hardware, we'll actually mark this as a special fake texture (set highest bit)
    New({var PX_D3DTexture}ppTexture^);

    ppTexture^.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_YUVSURF;
    ppTexture^.Emu.Lock := dwPtr;
    ppTexture^.Format := X_D3DFMT_YUY2{=$24};

    ppTexture^.Size := (g_dwOverlayW and X_D3DSIZE_WIDTH_MASK)
                    or (g_dwOverlayH shl X_D3DSIZE_HEIGHT_SHIFT)
                    or (g_dwOverlayP shl X_D3DSIZE_PITCH_SHIFT);

    g_YuvSurface := PX_D3DSurface(ppTexture^);

    Result := D3D_OK;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_CreateVolumeTexture
(
    Width: UINT; 
    Height: UINT;
    Depth: UINT; 
    Levels: UINT; 
    Usage: DWORD;
    Format: X_D3DFORMAT;
    Pool: D3DPOOL; 
    ppVolumeTexture: PPX_D3DVolumeTexture
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  PCFormat: D3DFORMAT;
  dwSize: DWORD;
  dwPtr: DWORD;
  pRefCount: PDWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreateVolumeTexture' +
           #13#10'(' +
           #13#10'   Width             : 0x%.08X' +
           #13#10'   Height            : 0x%.08X' +
           #13#10'   Depth             : 0x%.08X' +
           #13#10'   Levels            : 0x%.08X' +
           #13#10'   Usage             : 0x%.08X' +
           #13#10'   Format            : 0x%.08X' +
           #13#10'   Pool              : 0x%.08X' +
           #13#10'   ppVolumeTexture   : 0x%.08X' +
           #13#10');',
           [Width, Height, Depth, Levels, Usage, Ord(Format), Ord(Pool), ppVolumeTexture]);
{$ENDIF}

  // Convert Format (Xbox->PC)
  PCFormat := EmuXB2PC_D3DFormat(Format);

  // TODO -oCXBX: HACK: Devices that don't support this should somehow emulate it!
  if (PCFormat = D3DFMT_D16) then
  begin
    EmuWarning('D3DFMT_16 is an unsupported texture format!');
    PCFormat := D3DFMT_X8R8G8B8;
  end
  else if (PCFormat = D3DFMT_P8) then
  begin
    EmuWarning('D3DFMT_P8 is an unsupported texture format!');
    PCFormat := D3DFMT_X8R8G8B8;
  end
  else if (PCFormat = D3DFMT_D24S8) then
  begin
    EmuWarning('D3DFMT_D24S8 is an unsupported texture format!');
    PCFormat := D3DFMT_X8R8G8B8;
  end
  else if (PCFormat = D3DFMT_YUY2) then
  begin
    // cache the overlay size
    g_dwOverlayW := Width;
    g_dwOverlayH := Height;
    g_dwOverlayP := RoundUp(g_dwOverlayW, 64)*2;
  end;

  if (PCFormat <> D3DFMT_YUY2) then
  begin
    EmuAdjustPower2(@Width, @Height);

    New({PX_D3DVolumeTexture}ppVolumeTexture^);

    hRet := IDirect3DDevice8_CreateVolumeTexture(g_pD3DDevice8,
        Width, Height, Depth, Levels,
        0,  // TODO -oCXBX: Xbox Allows a border to be drawn (maybe hack this in software ;[)
        PCFormat, D3DPOOL_MANAGED, @(ppVolumeTexture^.Emu.VolumeTexture8));

    if (FAILED(hRet)) then
        EmuWarning('CreateVolumeTexture Failed! (0x%.08X)', [hRet]);

{$IFDEF DEBUG}
    DbgPrintf('EmuD3D8 : Created Volume Texture: 0x%.08X (0x%.08X)', [@ppVolumeTexture, ppVolumeTexture^.Emu.VolumeTexture8]);
{$ENDIF}
  end
  else
  begin
    dwSize := g_dwOverlayP * g_dwOverlayH;
    dwPtr := DWORD(DxbxMalloc(dwSize + sizeof(DWORD)));

    pRefCount := PDWORD(dwPtr + dwSize);

    // initialize ref count
    pRefCount^ := 1;

    // If YUY2 is not supported in hardware, we'll actually mark this as a special fake texture (set highest bit)
    ppVolumeTexture^.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_YUVSURF;
    ppVolumeTexture^.Emu.Lock := dwPtr;
    ppVolumeTexture^.Format := $24;

    ppVolumeTexture^.Size  := (g_dwOverlayW and X_D3DSIZE_WIDTH_MASK)
                           or (g_dwOverlayH shl X_D3DSIZE_HEIGHT_SHIFT)
                           or (g_dwOverlayP shl X_D3DSIZE_PITCH_SHIFT);

    hRet := D3D_OK;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirect3DDevice8_CreateCubeTexture
(
    EdgeLength: UINT;
    Levels: UINT;
    Usage: DWORD;
    Format: X_D3DFORMAT;
    Pool: D3DPOOL;
    ppCubeTexture: PPX_D3DCubeTexture
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  PCFormat: D3DFORMAT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreateCubeTexture' +
      #13#10'(' +
      #13#10'   EdgeLength        : 0x%.08X' +
      #13#10'   Levels            : 0x%.08X' +
      #13#10'   Usage             : 0x%.08X' +
      #13#10'   Format            : 0x%.08X' +
      #13#10'   Pool              : 0x%.08X' +
      #13#10'   ppCubeTexture     : 0x%.08X' +
      #13#10');',
      [EdgeLength, Levels, Usage, Ord(Format), Ord(Pool), ppCubeTexture]);
{$ENDIF}

  // Convert Format (Xbox->PC)
  PCFormat := EmuXB2PC_D3DFormat(Format);

  // TODO -oCXBX: HACK: Devices that don't support this should somehow emulate it!
  if (PCFormat = D3DFMT_D16) then
  begin
    EmuWarning('D3DFMT_16 is an unsupported texture format!');
    PCFormat := D3DFMT_X8R8G8B8;
  end
  else if (PCFormat = D3DFMT_P8) then
  begin
    EmuWarning('D3DFMT_P8 is an unsupported texture format!');
    PCFormat := D3DFMT_X8R8G8B8;
  end
  else if (PCFormat = D3DFMT_D24S8) then
  begin
    EmuWarning('D3DFMT_D24S8 is an unsupported texture format!');
    PCFormat := D3DFMT_X8R8G8B8;
  end
  else if (PCFormat = D3DFMT_YUY2) then
  begin
    DxbxKrnlCleanup('YUV not supported for cube textures');
  end;

  New({var PX_D3DCubeTexture}ppCubeTexture^);

  Result := IDirect3DDevice8_CreateCubeTexture(g_pD3DDevice8,
      EdgeLength, Levels,
      0,  // TODO -oCXBX: Xbox Allows a border to be drawn (maybe hack this in software ;[)
      PCFormat, D3DPOOL_MANAGED, @(ppCubeTexture^.Emu.CubeTexture8)
  );

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : Created Cube Texture: 0x%.08X (0x%.08X)', [ppCubeTexture^, ppCubeTexture^.Emu.CubeTexture8]);
{$ENDIF}

  if (FAILED(Result)) then
      EmuWarning('CreateCubeTexture Failed!');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_CreateIndexBuffer
(
  Length: UINT;
  Usage: DWORD;
  Format: X_D3DFORMAT;
  Pool: D3DPOOL;
  ppIndexBuffer: PPX_D3DIndexBuffer
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  pData: PBYTE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreateIndexBuffer' +
         #13#10'(' +
         #13#10'   Length              : 0x%.08X' +
         #13#10'   Usage               : 0x%.08X' +
         #13#10'   Format              : 0x%.08X' +
         #13#10'   Pool                : 0x%.08X' +
         #13#10'   ppIndexBuffer       : 0x%.08X' +
         #13#10');',
         [Length, Usage, Ord(Format), Ord(Pool), ppIndexBuffer]);
{$ENDIF}

  New({var PX_D3DIndexBuffer}ppIndexBuffer^);

  hRet := IDirect3DDevice8_CreateIndexBuffer(g_pD3DDevice8,
      Length, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED,
      @(ppIndexBuffer^.Emu.IndexBuffer8));

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIndexBuffer8 := 0x%.08X', [ppIndexBuffer^.Emu.IndexBuffer8]);
{$ENDIF}

  if (FAILED(hRet)) then
      EmuWarning('CreateIndexBuffer Failed! (0x%.08X)', [hRet]);

  //
  // update data ptr
  //
  begin
    pData := NULL;

    IDirect3DIndexBuffer8(ppIndexBuffer^.Emu.IndexBuffer8).Lock(0, Length, {out PByte}pData, 0);

    ppIndexBuffer^.Data := DWORD(pData);

    // StrikerX3: experimenting...
    IDirect3DIndexBuffer8(ppIndexBuffer^.Emu.IndexBuffer8).Unlock();
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;


function XTL_EmuIDirect3DDevice8_CreateIndexBuffer2(Length: UINT): PX_D3DIndexBuffer; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  Result := NULL;

  XTL_EmuIDirect3DDevice8_CreateIndexBuffer
  (
      Length,
      0,
      X_D3DFMT_INDEX16,
      D3DPOOL_MANAGED,
      @Result);
end;


function XTL_EmuIDirect3DDevice8_SetIndices
(
  pIndexData: PX_D3DIndexBuffer;
  BaseVertexIndex: UINT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
label
  fail;
var
  pIndexBuffer: XTL_PIDirect3DIndexBuffer8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetIndices' +
      #13#10'(' +
      #13#10'   pIndexData          : 0x%.08X' +
      #13#10'   BaseVertexIndex     : 0x%.08X' +
      #13#10');',
      [pIndexData, BaseVertexIndex]);
{$ENDIF}

  { // Commented out by CXBX
  fflush(stdout);
  if (pIndexData <> 0) then
  begin
    chk := 0; // Dxbx : this should become a writeable const
    if (chk++ = 0) then
    begin
      asm int 3 end;
    end;
  end;
  }

  Result := D3D_OK;

  if (pIndexData <> NULL) then
    DbgPrintf('EmuIDirect3DDevice8_SetIndices(): pIndexData->EmuIndexBuffer8:= 0x%.08X', [pIndexData.Emu.IndexBuffer8]);

  g_dwBaseVertexIndex := BaseVertexIndex;

  if (pIndexData <> nil) then
  begin
    g_pIndexBuffer := pIndexData;

    // HACK: Halo Hack
    if (pIndexData.Emu.Lock = $00840863) then
        pIndexData.Emu.Lock := 0;

    EmuVerifyResourceIsRegistered(pIndexData);

    // HACK: Unreal Championship
    if ((pIndexData.Emu.Lock and $FFFF0000) = $00490000) or ((pIndexData.Emu.Lock and $F0000000) <> $00000000) or
        (pIndexData.Emu.Lock = $10) then
    begin
      Result := E_FAIL;
      goto fail;
    end;

    pIndexBuffer := pIndexData.Emu.IndexBuffer8;
    DxbxUnlockD3DResource(pIndexData); // Dxbx addition

    if (pIndexData.Emu.Lock <> X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
      Result := IDirect3DDevice8(g_pD3DDevice8).SetIndices(IDirect3DIndexBuffer8(pIndexBuffer), BaseVertexIndex);
  end
  else
  begin
    g_pIndexBuffer := nil;

    Result := IDirect3DDevice8(g_pD3DDevice8).SetIndices(nil, BaseVertexIndex);
  end;

fail:
  EmuSwapFS(fsXbox);
end;

{$ifdef _DEBUG_DUMP_TEXTURE_SETTEXTURE}
{static}var dwDumpTexture: int = 0;
{$endif}
function XTL_EmuIDirect3DDevice8_SetTexture
(
    Stage: DWORD;
    pTexture: PX_D3DResource
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pBaseTexture8: XTL_PIDirect3DBaseTexture8;
{$ifdef _DEBUG_DUMP_TEXTURE_SETTEXTURE}
  szBuffer: array [0..256-1] of _char;
  face: int;
{$endif}
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetTexture' +
    #13#10'(' +
    #13#10'   Stage               : 0x%.08X' +
    #13#10'   pTexture            : 0x%.08X' +
    #13#10');',
    [Stage, pTexture]);
{$ENDIF}

  pBaseTexture8 := NULL;

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
      XTL_EmuIDirect3DDevice8_EnableOverlay(BOOL_TRUE);
      XTL_EmuIDirect3DDevice8_UpdateOverlay(PX_D3DSurface(pTexture), nil, nil, BOOL_FALSE, 0);
      EmuSwapFS(fsWindows);
    end
    else
    begin
      pBaseTexture8 := pTexture.Emu.BaseTexture8;

      {$ifdef _DEBUG_DUMP_TEXTURE_SETTEXTURE}
        if (pTexture <> NULL) and (pTexture.Emu.Texture8 <> NULL) then
        begin
          // dwDumpTexture := 0; // Dxbx note : Do not reset 'static' var

          case (IDirect3DResource8(pTexture.Emu.Resource8).GetType()) of
            D3DRTYPE_TEXTURE:
            begin
              sprintf(@szBuffer[0], 'SetTextureNorm - %.03d (0x%.08X).bmp', [dwDumpTexture, UIntPtr(pTexture.Emu.Texture8)]);
              Inc(dwDumpTexture);
              IDirect3DTexture8(pTexture.Emu.Texture8).UnlockRect(0);

              D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, IDirect3DTexture8(pTexture.Emu.Texture8), NULL);
            end;

            D3DRTYPE_CUBETEXTURE:
            begin
              face := 0;
              while face < 6 do
              begin
                Inc(dwDumpTexture);
                sprintf(@szBuffer[0], 'SetTextureCube%d - %.03d (0x%.08X).bmp', [face, dwDumpTexture, UIntPtr(pTexture.Emu.Texture8)]);
                IDirect3DTexture8(pTexture.Emu.CubeTexture8).UnlockRect(face);
                D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, IDirect3DTexture8(pTexture.Emu.Texture8), NULL);
                Inc(Face);
              end;
            end;
          end;
        end;
      {$endif}
    end;
  end;

  (* --- MARKED OUT BY CXBX --
    IDirect3DTexture8 *pDummyTexture[4] := (0, 0, 0, 0);

    if (pDummyTexture[Stage] = 0) then
    begin
        if (Stage = 0) then
        begin
            if (D3DXCreateTextureFromFile(g_pD3DDevice8, 'C:\dummy1.bmp', @pDummyTexture[Stage]) <> D3D_OK) then
                DxbxKrnlCleanup('Could not create dummy texture!');
        end
        else if (Stage = 1) then
        begin
            if (D3DXCreateTextureFromFile(g_pD3DDevice8, 'C:\dummy2.bmp', @pDummyTexture[Stage]) <> D3D_OK) then
                DxbxKrnlCleanup('Could not create dummy texture!');
        end;
     end;
  //*)

  (* -- MARKED OUT BY CXBX
  int dwDumpTexture := 0;
  szBuffer: array [0..256-1] of _char;
  sprintf(szBuffer, 'C:\Aaron\Textures\DummyTexture - %.03d (0x%.08X).bmp', [dwDumpTexture, pDummyTexture]); Inc(dwDumpTexture);
  pDummyTexture.UnlockRect(0);
  D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, pDummyTexture, 0);
  //*)

  // Dxbx Note : As suggested by StrikerX3, as a fix for missing textures in Panzer :
  // Make sure the texture has no locks, otherwise we get blank polygons

  DxbxUnlockD3DResource(pTexture);
  // hRet = IDirect3DDevice8(g_pD3DDevice8).SetTexture(Stage, pDummyTexture[Stage]); // MARKED OUT BY CXBX
  Result := IDirect3DDevice8(g_pD3DDevice8).SetTexture(Stage, IDirect3DBaseTexture8(iif((g_iWireframe = 0), pBaseTexture8, nil)));

  if FAILED(Result) then
  begin
    EmuWarning('SetTextureFailed!\n');
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SwitchTexture
(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Data: DWORD;
  {1 ECX}Method: DWORD;
  {3 stack}Format: DWORD
); register; // fastcall simulation - See Translation guide
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
const
  StageLookup: array [0..4-1] of DWORD = ( $00081b00, $00081b40, $00081b80, $00081bc0 );
var
  Stage: DWORD;
  v: int;
  pTexture: PX_D3DTexture;
{  hRet: HRESULT;}
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SwitchTexture' +
    #13#10'(' +
    #13#10'   Method            : 0x%.08X' +
    #13#10'   Data              : 0x%.08X' +
    #13#10'   Format            : 0x%.08X' +
    #13#10');',
    [Method, Data, Format]);
{$ENDIF}

  Stage := DWord(-1);

  for v := 0 to 4-1 do
  begin
    if (StageLookup[v] = Method) then
      Stage := v;
  end;

  if (Stage = DWord(-1)) then
  begin
    EmuWarning('Unknown Method (0x%.08X)', [Method]);
  end
  else
  begin
    // WARNING: TODO -oCXBX: Correct reference counting has not been completely verified for this code

    pTexture := g_DataToTexture.get(Data);

    EmuWarning('Switching Texture 0x%.08X (0x%.08X) @ Stage %d', [pTexture, pTexture.Emu.BaseTexture8, Stage]);

//    DxbxUnlockD3DResource(pTexture); // Dxbx addition : Attempt to fix missing textures

    {hRet := }IDirect3DDevice8(g_pD3DDevice8).SetTexture(Stage, IDirect3DBaseTexture8(pTexture.Emu.BaseTexture8));

    { MARKED OUT BY CXBX
    if (pTexture.Emu.BaseTexture8 <> 0) then
    begin
         Integer dwDumpTexture := 0;

         szBuffer: array [0..255-1] of _char;

        sprintf(szBuffer, 'C:\Aaron\Textures\0x%.08X-SwitchTexture%.03d.bmp', [pTexture, dwDumpTexture]); Inc(dwDumpTexture);

        pTexture.Emu.Texture8.UnlockRect(0);

        D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, pTexture.Emu.BaseTexture8, 0);
     end;
    }
  end;

  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

function XTL_EmuIDirect3DDevice8_GetDisplayMode
(
  pMode: PX_D3DDISPLAYMODE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pPCMode: PD3DDISPLAYMODE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetDisplayMode' +
      #13#10'(' +
      #13#10'   pMode               : 0x%.08X' +
      #13#10');',
      [pMode]);
{$ENDIF}


  // make adjustments to parameters to make sense with windows d3d
  begin
    pPCMode := PD3DDISPLAYMODE(pMode);
    Result := IDirect3DDevice8(g_pD3DDevice8).GetDisplayMode({out}pPCMode^);

    // Convert Format (PC->Xbox)
    pMode.Format := EmuPC2XB_D3DFormat(pPCMode.Format);

    // TODO -oCXBX: Make this configurable in the future?
    pMode.Flags := X_D3DPRESENTFLAG_FIELD or X_D3DPRESENTFLAG_INTERLACED or X_D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;

    // TODO -oCXBX: Retrieve from current CreateDevice settings?
    pMode.Width := 640;
    pMode.Height := 480;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_Begin
(
    PrimitiveType: X_D3DPRIMITIVETYPE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_Begin' +
    #13#10'(' +
    #13#10'   PrimitiveType     : 0x%.08X' +
    #13#10');',
    [Ord(PrimitiveType)]);
{$ENDIF}

  g_IVBPrimitiveType := PrimitiveType;

  g_IVBTblOffs := 0;
  g_IVBFVF := 0;
  // default values
  if Length(g_IVBTable) > 0 then
    ZeroMemory(@g_IVBTable[0], sizeof(_D3DIVB)*Length(g_IVBTable));

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuIDirect3DDevice8_SetVertexData2f
(
    Register_: int;
    a: FLOAT;
    b: FLOAT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexData2f >>' +
      #13#10'(' +
      #13#10'   Register          : 0x%.08X' +
      #13#10'   a                 : %f' +
      #13#10'   b                 : %f' +
      #13#10');',
      [Register_, a, b]);
    EmuSwapFS(fsXbox);
  end;
{$ENDIF}
  // TODO -oDxbx : Handle Vertex Attributes that need a Color (in this case, r,g,b,a=0.0-1.0)
  Result := XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_, a, b, 0.0, 1.0);
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


function XTL_EmuIDirect3DDevice8_SetVertexData2s
(
    Register_: int;
    a: SHORT;
    b: SHORT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  dwA, dwB: DWORD;
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexData2s >>' +
      #13#10'(' +
      #13#10'   Register          : 0x%.08X' +
      #13#10'   a                 : %d' +
      #13#10'   b                 : %d' +
      #13#10');',
      [Register_, a, b]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  dwA := a; dwB := b;

  if (Register_ = X_D3DVSDE_DIFFUSE) or (Register_ = X_D3DVSDE_SPECULAR) then
  begin
    dwA := DWORD(a > 0);
    dwB := DWORD(b > 0);
  end;


  // TODO -oDxbx : Handle Vertex Attributes that need a Color (in this case, r,g,b,a=0 or 1)
  //Result := XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_, DWtoF(dwA), DWtoF(dwB), 0.0, 1.0);
  Result := XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_, (dwA), (dwB), 0.0, 1.0);
end;

function XTL_EmuIDirect3DDevice8_SetVertexData4f
(
    Register_: int;
    a: FLOAT;
    b: FLOAT;
    c: FLOAT;
    d: FLOAT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  o: int;

  ca: DWORD;
  cr: DWORD;
  cg: DWORD;
  cb: DWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexData4f' +
    #13#10'(' +
    #13#10'   Register          : 0x%.08X' +
    #13#10'   a                 : %f' +
    #13#10'   b                 : %f' +
    #13#10'   c                 : %f' +
    #13#10'   d                 : %f' +
    #13#10');',
    [Register_, a, b, c, d]);
{$ENDIF}

  hRet := S_OK;

  // Check if g_IVBTable has enough space for the current g_IVBTblOffs
  // (and one extra for the "Copy current color to next vertex" case) :
  if Length(g_IVBTable) <= (int(g_IVBTblOffs) + 1) then
  begin
    // Grow with 128 vertices at a time :
    SetLength(g_IVBTable, g_IVBTblOffs + 128);

    // default values
    ZeroMemory(@g_IVBTable[g_IVBTblOffs], sizeof(_D3DIVB)*128);
  end;

  case Cardinal(Register_) of
    // TODO -oCXBX: Blend weight.

    {0=}X_D3DVSDE_POSITION:
      begin
        o := g_IVBTblOffs;
        g_IVBTable[o].Position.x := a;
        g_IVBTable[o].Position.y := b;
        g_IVBTable[o].Position.z := c;
        g_IVBTable[o].Rhw := d; //1.0; // Dxbx note : Why set Rhw to 1.0? And why ignore d?

        Inc(g_IVBTblOffs);

        g_IVBFVF := g_IVBFVF or D3DFVF_XYZRHW;
      end;

    {1=}X_D3DVSDE_BLENDWEIGHT:
      begin
        o := g_IVBTblOffs;

        g_IVBTable[o].Position.x := a;
        g_IVBTable[o].Position.y := b;
        g_IVBTable[o].Position.z := c;
        g_IVBTable[o].Blend1 := d;

        Inc(g_IVBTblOffs);

        g_IVBFVF := g_IVBFVF or D3DFVF_XYZB1;
      end;

    {2=}X_D3DVSDE_NORMAL:
      begin
        o := g_IVBTblOffs;

        g_IVBTable[o].Normal.x := a;
        g_IVBTable[o].Normal.y := b;
        g_IVBTable[o].Normal.z := c;

        Inc(g_IVBTblOffs);

        g_IVBFVF := g_IVBFVF or D3DFVF_NORMAL;
      end;

   {3=}X_D3DVSDE_DIFFUSE:
      begin
        o := g_IVBTblOffs;
        ca := Trunc(d * 255) shl 24; //FtoDW(d) shl 24;
        cr := Trunc(a * 255) shl 16; //FtoDW(a) shl 16;
        cg := Trunc(b * 255) shl 8; //FtoDW(b) shl 8;
        cb := Trunc(c * 255) shl 0; //FtoDW(c) shl 0;

        g_IVBTable[o].dwDiffuse := ca or cr or cg or cb;

        g_IVBFVF := g_IVBFVF or D3DFVF_DIFFUSE;
      end;

    {4=}X_D3DVSDE_SPECULAR:
      begin
        o := g_IVBTblOffs;
        ca := Trunc(d * 255) shl 24; //FtoDW(d) shl 24;
        cr := Trunc(a * 255) shl 16; //FtoDW(a) shl 16;
        cg := Trunc(b * 255) shl 8; //FtoDW(b) shl 8;
        cb := Trunc(c * 255) shl 0; //FtoDW(c) shl 0;

        g_IVBTable[o].dwSpecular := ca or cr or cg or cb;

        g_IVBFVF := g_IVBFVF or D3DFVF_SPECULAR;
      end;
    {9=}X_D3DVSDE_TEXCOORD0:
      begin
        o := g_IVBTblOffs;
        g_IVBTable[o].TexCoord1.x := a;
        g_IVBTable[o].TexCoord1.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX1) then
        begin
          // Dxbx fix : Use mask, else the format might get expanded incorrectly :
          g_IVBFVF := (g_IVBFVF and (not D3DFVF_TEXCOUNT_MASK)) or D3DFVF_TEX1;
        end;
      end;

    {10=}X_D3DVSDE_TEXCOORD1:
      begin
        o := g_IVBTblOffs;
        g_IVBTable[o].TexCoord2.x := a;
        g_IVBTable[o].TexCoord2.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX2) then
        begin
          // Dxbx fix : Use mask, else the format might get expanded incorrectly :
          g_IVBFVF := (g_IVBFVF and (not D3DFVF_TEXCOUNT_MASK)) or D3DFVF_TEX2;
        end;
      end;

    {11=}X_D3DVSDE_TEXCOORD2:
      begin
        o := g_IVBTblOffs;
        g_IVBTable[o].TexCoord3.x := a;
        g_IVBTable[o].TexCoord3.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX3) then
        begin
          // Dxbx fix : Use mask, else the format might get expanded incorrectly :
          g_IVBFVF := (g_IVBFVF and (not D3DFVF_TEXCOUNT_MASK)) or D3DFVF_TEX3;
        end;
      end;

    {12=}X_D3DVSDE_TEXCOORD3:
      begin
        o := g_IVBTblOffs;
        g_IVBTable[o].TexCoord4.x := a;
        g_IVBTable[o].TexCoord4.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX4) then
        begin
          // Dxbx fix : Use mask, else the format might get expanded incorrectly :
          g_IVBFVF := (g_IVBFVF and (not D3DFVF_TEXCOUNT_MASK)) or D3DFVF_TEX4;
        end;
      end;

    { $FFFFFFFF=}X_D3DVSDE_VERTEX:
    begin
      o := g_IVBTblOffs;
      g_IVBTable[o].Position.x := a;
      g_IVBTable[o].Position.y := b;
      g_IVBTable[o].Position.z := c;
      g_IVBTable[o].Rhw := d;

      // Copy current color to next vertex
      g_IVBTable[o+1].dwDiffuse := g_IVBTable[o].dwDiffuse;
      g_IVBTable[o+1].dwSpecular := g_IVBTable[o].dwSpecular;
      // Dxbx note : Must we copy Blend1 (blendweight) too?

      Inc(g_IVBTblOffs);

      g_IVBFVF := g_IVBFVF or D3DFVF_XYZRHW;
    end;
  else
    DxbxKrnlCleanup('Unknown IVB Register: %d', [Register_]);
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirect3DDevice8_SetVertexData4ub
(
    Register_: INT;
    a: BYTE;
    b: BYTE;
    c: BYTE;
    d: BYTE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Patrickvl  Done:100
var
  dwA, dwB, dwC, dwD: FLOAT;
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexData4ub >>' +
      #13#10'(' +
      #13#10'   Register          : 0x%.08X' +
      #13#10'   a                 : %d' +
      #13#10'   b                 : %d' +
      #13#10'   c                 : %d' +
      #13#10'   d                 : %d' +
      #13#10');',
      [Register_, a, b, c, d]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  dwA := a; dwB := b; dwC := c; dwD := d;

  if (Register_ = X_D3DVSDE_DIFFUSE) or (Register_ = X_D3DVSDE_SPECULAR) then
  begin
    dwA := dwA / 255.0;
    dwB := dwB / 255.0;
    dwC := dwC / 255.0;
    dwD := dwD / 255.0;
  end;

  // TODO -oDxbx : Handle Vertex Attributes that need a Color (in this case, r,g,b=0.0-255.0, a=0.0-1.0)
  // TODO -oDxbx : Shouldn't these be multiplied with 256.0 ?
  //Result := XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_, DWtoF(dwA), DWtoF(dwB), DWtoF(dwC), DWtoF(dwD));
  Result := XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_, (dwA), (dwB), (dwC), (dwD));
end;

function XTL_EmuIDirect3DDevice8_SetVertexData4s
(
    Register_: INT;
    a: SHORT;
    b: SHORT;
    c: SHORT;
    d: SHORT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Patrickvl  Done:100
var
  dwA, dwB, dwC, dwD: DWORD;
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexData4s >>' +
      #13#10'(' +
      #13#10'   Register          : 0x%.08X' +
      #13#10'   a                 : %d' +
      #13#10'   b                 : %d' +
      #13#10'   c                 : %d' +
      #13#10'   d                 : %d' +
      #13#10');',
      [Register_, a, b, c, d]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  dwA := a; dwB := b; dwC := c; dwD := d;

  if (Register_ = X_D3DVSDE_DIFFUSE) or (Register_ = X_D3DVSDE_SPECULAR) then
  begin
    dwA := DWORD(a > 0);
    dwB := DWORD(b > 0);
    dwC := DWORD(c > 0);
    dwD := DWORD(d > 0);
  end;

  // TODO -oDxbx : Handle Vertex Attributes that need a Color
  //Result := XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_, DWtoF(dwA), DWtoF(dwB), DWtoF(dwC), DWtoF(dwD));
  Result := XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_, (dwA), (dwB), (dwC), (dwD));
end;

function XTL_EmuIDirect3DDevice8_SetVertexDataColor
(
    Register_: int;
    Color: D3DCOLOR
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  a: FLOAT;
  r: FLOAT;
  g: FLOAT;
  b: FLOAT;
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexDataColor >>' +
    #13#10'(' +
    #13#10'   Register          : 0x%.08X' +
    #13#10'   Color             : 0x%.08X' +
    #13#10');',
    [Register_, Color]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  // TODO -oDxbx note : Is this correct? Shouldn't it be r,b,g,a ?
  a := ((Color and $FF000000) shr 24);
  r := ((Color and $00FF0000) shr 16);
  g := ((Color and $0000FF00) shr 8);
  b := ((Color and $000000FF) shr 0);

  if (Register_ = X_D3DVSDE_DIFFUSE) or (Register_ = X_D3DVSDE_SPECULAR) then
  begin
    a := a / 255.0;
    r := r / 255.0;
    g := g / 255.0;
    b := b / 255.0;
  end;

  Result := XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_, r, g, b, a);
end;

function XTL_EmuIDirect3DDevice8_End(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_End();');
{$ENDIF}

  if (g_IVBTblOffs <> 0) then
      XTL_EmuFlushIVB();

    // MARKED OUT BY CXBX
    // TODO -oCXBX: Should technically clean this up at some point..but on XP doesnt matter much
//    DxbxFree(g_pIVBVertexBuffer);
//    DxbxFree(g_IVBTable);

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

procedure XTL_EmuIDirect3DDevice8_RunPushBuffer
(
    pPushBuffer: PX_D3DPushBuffer;
    pFixup: PX_D3DFixup
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_RunPushBuffer' +
    #13#10'(' +
    #13#10'   pPushBuffer       : 0x%.08X' +
    #13#10'   pFixup            : 0x%.08X' +
    #13#10');',
    [pPushBuffer, pFixup]);
{$ENDIF}

  XTL_EmuExecutePushBuffer(pPushBuffer, pFixup);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_Clear
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
  newFlags: DWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_Clear' +
           #13#10'(' +
           #13#10'   Count               : 0x%.08X' +
           #13#10'   pRects              : 0x%.08X' +
           #13#10'   Flags               : 0x%.08X' +
           #13#10'   Color               : 0x%.08X' +
           #13#10'   Z                   : %f' +
           #13#10'   Stencil             : 0x%.08X' +
           #13#10');',
           [Count, pRects, Flags,
           Color, Z, Stencil]);
{$ENDIF}

  // make adjustments to parameters to make sense with windows d3d
  begin
    // TODO -oCXBX: D3DCLEAR_TARGET_A, *R, *G, *B don't exist on windows
    newFlags := 0;

    if (Flags and X_D3DCLEAR_TARGET) > 0 then
        newFlags := newFlags or D3DCLEAR_TARGET;

    if (Flags and X_D3DCLEAR_ZBUFFER) > 0 then
        newFlags := newFlags or D3DCLEAR_ZBUFFER;

    if (Flags and X_D3DCLEAR_STENCIL) > 0 then
        newFlags := newFlags or D3DCLEAR_STENCIL;

    if (Flags and (not X_D3DCLEAR_ALL_SUPPORTED)) > 0 then
        EmuWarning('Unsupported Flag(s) for IDirect3DDevice8_Clear: 0x%.08X', [Flags and (not X_D3DCLEAR_ALL_SUPPORTED)]);

    Flags := newFlags;
  end;

  Result := IDirect3DDevice8(g_pD3DDevice8).Clear(Count, pRects, Flags, Color, Z, Stencil);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_Present
(
    {CONST} pSourceRect: PRECT;
    {CONST} pDestRect: PRECT;
    pDummy1: PVOID;
    pDummy2: PVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  pBackBuffer: XTL_PIDirect3DSurface8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_Present' +
         #13#10'(' +
         #13#10'   pSourceRect       : 0x%.08X' +
         #13#10'   pDestRect         : 0x%.08X' +
         #13#10'   pDummy1           : 0x%.08X' +
         #13#10'   pDummy2           : 0x%.08X' +
         #13#10');',
         [pSourceRect, pDestRect, pDummy1, pDummy2]);
{$ENDIF}

  // release back buffer lock
  begin
    if IDirect3DDevice8(g_pD3DDevice8).GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, @pBackBuffer) = D3D_OK then
    begin
      IDirect3DSurface8(pBackBuffer).UnlockRect();
      IDirect3DSurface8(pBackBuffer)._Release;
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
    g_SwapData.Swap := D3DSWAP_DEFAULT; // TODO -oDxbx : Should we do this ? Cxbx did Inc(g_SwapData.Swap);

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

function XTL_EmuIDirect3DDevice8_Swap
(
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
const
  X_D3DSWAP_DEFAULT = 0;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_Swap >>' +
    #13#10'(' +
    #13#10'   Flags               : 0x%.08X' +
    #13#10');',
    [Flags]);
{$ENDIF}

  // TODO -oCXBX: Ensure this flag is always the same across library versions
  if (Flags <> X_D3DSWAP_DEFAULT) then
    EmuWarning('XTL.EmuIDirect3DDevice8_Swap: Flags <> D3DSWAP_DEFAULT');

  EmuSwapFS(fsXbox);

  // Forward to Present
  Result := XTL_EmuIDirect3DDevice8_Present(nil, nil, nil, nil);
end;

function XTL_EmuIDirect3DResource8_Register
(
    pThis: PX_D3DResource; 
    pBase: PVOID
): HRESULT; stdcall;
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
  dwPtr: DWORD;
  pRefCount: PDWORD;
  pData: PBYTE;
  X_Format: X_D3DFORMAT;
  Format: D3DFORMAT;
  CacheFormat: D3DFORMAT;
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
  w: uint32;
  h: uint32;
  v: uint32;
  c: uint32;
  p: Byte;
  y: DWORD;

  pPalette: PX_D3DPalette;

  stop: uint32;
  r: uint32;
  dwCompressedOffset: DWORD;
  dwMipOffs: DWORD;
  dwMipWidth: DWORD;
  dwMipHeight: DWORD;
  dwMipPitch: DWORD;
  level: uint;
  LockedRect: D3DLOCKED_RECT;
  iRect: TRect;
  iPoint: TPOINT;
  pSrc: PByte;
  pDest: PByte;
  pPixelData: PByte;
  dwDataSize: DWORD;
  dwPaletteSize: DWORD;
  pTextureCache: PBytes;
  pExpandedTexture: PBytes;
  pTexturePalette: PBytes;

  dummy: Pointer;
  szString: string;

{$IFDEF _DEBUG_DUMP_TEXTURE_REGISTER}
  dwDumpSurface: int;
  szBuffer: array [0..255 - 1] of _char;
  dwDumpCube: Integer;
  pSurface: IDirect3DSurface8;
  dwDumpTex: Integer;
{$ENDIF}

begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DResource8_Register' +
    #13#10'(' +
    #13#10'   pThis             : 0x%.08X (->Data: 0x%.08X)' +
    #13#10'   pBase             : 0x%.08X' +
    #13#10');',
    [pThis, pThis.Data, pBase]);
{$ENDIF}

  hRet := S_OK;

  pResource := pThis;

  dwCommonType := pResource.Common and X_D3DCOMMON_TYPE_MASK;

  // add the offset of the current texture to the base
  pBase := PVOID(DWORD(pBase) + pThis.Data);

  // Determine the resource type, and initialize
  case dwCommonType of

    X_D3DCOMMON_TYPE_VERTEXBUFFER:
    begin
{$IFDEF DEBUG}
      DbgPrintf('EmuIDirect3DResource8_Register: Creating VertexBuffer...');
{$ENDIF}

      pVertexBuffer := PX_D3DVertexBuffer(pResource);

      // create vertex buffer
      begin
        dwSize := EmuCheckAllocationSize(pBase, true);

        if (dwSize = DWORD(-1)) or (dwSize = 0) then
        begin
          // TODO -oCXBX: once this is known to be working, remove the warning
          EmuWarning('Vertex buffer allocation size unknown');
          dwSize := $2000; // temporarily assign a small buffer, which will be increased later
        end;

        hRet := IDirect3DDevice8(g_pD3DDevice8).CreateVertexBuffer
        (
          dwSize, 0, 0, D3DPOOL_MANAGED,
          {ppVertexBuffer=}@(pVertexBuffer.Emu.VertexBuffer8)
        );

        if (FAILED(hRet)) then
        begin
          // TODO -oCXBX: Hack for Crazy Taxi 3?
          szString := SysUtils.Format('CreateVertexBuffer Failed!'#13#10'   VB Size = 0x%X', [dwSize]);

          if ( dwSize <> 0 ) then
            DxbxKrnlCleanup( szString )
          else
          begin
            EmuWarning( szString );

            EmuSwapFS(fsXbox);

            Result := hRet;
            Exit;
          end;
        end;

  {$IFDEF _DEBUG_TRACK_VB}
        g_VBTrackTotal.insert(pResource.Emu.VertexBuffer8);
  {$ENDIF}

        pData := nil;

        hRet := IDirect3DVertexBuffer8(pResource.Emu.VertexBuffer8).Lock(0, 0, {out}pData, 0);

        if FAILED(hRet) then
          DxbxKrnlCleanup('VertexBuffer Lock Failed!');

        memcpy(pData, pBase, dwSize);

        IDirect3DVertexBuffer8(pResource.Emu.VertexBuffer8).Unlock();

        pResource.Data := ULONG(pData);
      end;
{$IFDEF DEBUG}
      DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created VertexBuffer (0x%.08X)', [pResource.Emu.VertexBuffer8]);
{$ENDIF}
    end;


    X_D3DCOMMON_TYPE_INDEXBUFFER:
    begin
{$IFDEF DEBUG}
      DbgPrintf('EmuIDirect3DResource8_Register :-> IndexBuffer...');
{$ENDIF}
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
          // Halo dwSize = 0x336;
        end
        else
        begin
          hRet := IDirect3DDevice8_CreateIndexBuffer
          (g_pD3DDevice8,
            dwSize, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED,
            @(pIndexBuffer.Emu.IndexBuffer8)
          );

          if (FAILED(hRet)) then
            DxbxKrnlCleanup('CreateIndexBuffer Failed!');

          pData := nil;

          hRet := IDirect3DIndexBuffer8(pResource.Emu.IndexBuffer8).Lock(0, dwSize, {out}pData, 0);

          if (FAILED(hRet)) then
            DxbxKrnlCleanup('IndexBuffer Lock Failed!');

          memcpy(pData, pBase, dwSize);

          IDirect3DIndexBuffer8(pResource.Emu.IndexBuffer8).Unlock();

          pResource.Data := ULONG(pData);
        end;

{$IFDEF DEBUG}
        DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created IndexBuffer (0x%.08X)', [pResource.Emu.IndexBuffer8]);
{$ENDIF}
      end;
    end;


    X_D3DCOMMON_TYPE_PUSHBUFFER:
    begin
{$IFDEF DEBUG}
      DbgPrintf('EmuIDirect3DResource8_Register: PushBuffer...');
{$ENDIF}

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
          pResource.Data := ULONG(pBase);
      end;

{$IFDEF DEBUG}
      DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created PushBuffer (0x%.08X, 0x%.08X, 0x%.08X)', [pResource.Data, pPushBuffer.Size, pPushBuffer.AllocationSize]);
{$ENDIF}
    end;


    X_D3DCOMMON_TYPE_SURFACE,
    X_D3DCOMMON_TYPE_TEXTURE:
    begin
{$IFDEF DEBUG}
      if (dwCommonType = X_D3DCOMMON_TYPE_SURFACE) then
        DbgPrintf('EmuIDirect3DResource8_Register :-> Surface...')
      else
        DbgPrintf('EmuIDirect3DResource8_Register :-> Texture...');
{$ENDIF}

      pPixelContainer := PX_D3DPixelContainer(pResource);

      X_Format := X_D3DFORMAT((pPixelContainer.Format and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT);
      Format := EmuXB2PC_D3DFormat(X_Format);
      CacheFormat := {XTL.}D3DFORMAT(0);
      // TODO -oCXBX: check for dimensions

      // TODO -oCXBX: HACK: Temporary?
      if (X_Format = X_D3DFMT_LIN_D24S8) then // = $2E
      begin
        {DxbxKrnlCleanup}EmuWarning('D3DFMT_LIN_D24S8 not yet supported!');
        X_Format := X_D3DFMT_LIN_A8R8G8B8; // = $12
        Format := D3DFMT_A8R8G8B8;
      end;

      if(X_Format = X_D3DFMT_LIN_D16) then // = 0x30
      begin
        {DxbxKrnlCleanup}EmuWarning('D3DFMT_LIN_D16 not yet supported!');
        X_Format := X_D3DFMT_LIN_R5G6B5; // = 0x11;
        Format := D3DFMT_R5G6B5;
      end;

      dwWidth := 1; dwHeight := 1; dwBPP := 1; dwDepth := 1; dwPitch := 0; dwMipMapLevels := 1;
      bSwizzled := FALSE; bCompressed := FALSE; dwCompressedSize := 0;
      bCubemap := (pPixelContainer.Format and X_D3DFORMAT_CUBEMAP) > 0;

      // Interpret Width/Height/BPP
      if (X_Format = X_D3DFMT_X8R8G8B8) or (X_Format = X_D3DFMT_A8R8G8B8) then
      begin
        bSwizzled := TRUE;

        // Swizzled 32 Bit
        dwWidth  := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
        dwHeight := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
        dwMipMapLevels := (pPixelContainer.Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
        dwDepth  := 1;// HACK? 1 shl ((pPixelContainer.Format and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
        dwPitch  := dwWidth*4;
        dwBPP := 4;
      end else
      if (X_Format = X_D3DFMT_R5G6B5) or (X_Format = X_D3DFMT_A4R4G4B4)
      or (X_Format = X_D3DFMT_A1R5G5B5)
      or (X_Format = $28 { X_D3DFMT_G8B8 }) then
      begin
        bSwizzled := TRUE;

        // Swizzled 16 Bit
        dwWidth  := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
        dwHeight := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
        dwMipMapLevels := (pPixelContainer.Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
        dwDepth  := 1;// HACK? 1 shl ((pPixelContainer.Format and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
        dwPitch  := dwWidth*2;
        dwBPP := 2;
      end else
      if (X_Format = X_D3DFMT_L8) or (X_Format = X_D3DFMT_P8)
      or (X_Format = X_D3DFMT_AL8) or (X_Format = X_D3DFMT_A8L8)
      or (X_Format = X_D3DFMT_A8) then
      begin
        bSwizzled := TRUE;

        // Swizzled 8 Bit
        dwWidth  := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
        dwHeight := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
        dwMipMapLevels := (pPixelContainer.Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
        dwDepth  := 1;// HACK? 1 shl ((pPixelContainer.Format and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
        dwPitch  := dwWidth;
        dwBPP := 1;
      end else
      if (X_Format = X_D3DFMT_LIN_X8R8G8B8) or (X_Format = X_D3DFMT_LIN_A8R8G8B8)
      or (X_Format = X_D3DFMT_LIN_D24S8) or (X_Format = X_D3DFMT_LIN_A8B8G8R8) then
      begin
        // Linear 32 Bit
        dwWidth  := (pPixelContainer.Size and X_D3DSIZE_WIDTH_MASK) + 1;
        dwHeight := ((pPixelContainer.Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
        dwPitch  := (((pPixelContainer.Size and X_D3DSIZE_PITCH_MASK) shr X_D3DSIZE_PITCH_SHIFT) + 1) * 64;
        dwBPP := 4;
      end
      else if (X_Format = X_D3DFMT_LIN_R5G6B5) then
      begin
        // Linear 16 Bit
        dwWidth := (pPixelContainer.Size and X_D3DSIZE_WIDTH_MASK) + 1;
        dwHeight := ((pPixelContainer.Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
        dwPitch := (((pPixelContainer.Size and X_D3DSIZE_PITCH_MASK) shr X_D3DSIZE_PITCH_SHIFT) + 1) * 64;
        dwBPP := 2;
      end
      else if (X_Format = X_D3DFMT_DXT1) or (X_Format = X_D3DFMT_DXT3) or (X_Format = X_D3DFMT_DXT5) then
      begin
        bCompressed := TRUE;

        // Compressed
        dwWidth  := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
        dwHeight := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
        dwDepth  := 1 shl ((pPixelContainer.Format and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
        dwMipMapLevels := (pPixelContainer.Format and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;

        // D3DFMT_DXT2...D3DFMT_DXT5: 128bits per block/per 16 texels
        dwCompressedSize := dwWidth * dwHeight;

        if (X_Format = X_D3DFMT_DXT1) then // 64bits per block/per 16 texels
          dwCompressedSize := dwCompressedSize div 2;

        dwBPP := 1;
      end 
      else if (X_Format = X_D3DFMT_YUY2) then
      begin
        // Linear 32 Bit
        dwWidth := (pPixelContainer.Size and X_D3DSIZE_WIDTH_MASK) + 1;
        dwHeight := ((pPixelContainer.Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1;
        dwPitch := (((pPixelContainer.Size and X_D3DSIZE_PITCH_MASK) shr X_D3DSIZE_PITCH_SHIFT) + 1) * 64;
      end
      else
      begin
        DxbxKrnlCleanup('0x%.08X is not a supported format!', [X_Format]);
      end;

      if (X_Format = X_D3DFMT_YUY2) then
      begin
        //
        // cache the overlay size
        //

        g_dwOverlayW := dwWidth;
        g_dwOverlayH := dwHeight;
        g_dwOverlayP := RoundUp(g_dwOverlayW, 64) * 2;

        //
        // create texture resource
        //

        dwSize := g_dwOverlayP * g_dwOverlayH;
        dwPtr := DWORD(DxbxMalloc(dwSize + sizeof(DWORD)));

        pRefCount := PDWORD(dwPtr + dwSize);

        // initialize ref count
        pRefCount^ := 1;

        // If YUY2 is not supported in hardware, we'll actually mark this as a special fake texture (set highest bit)
        pPixelContainer.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_YUVSURF;
        pPixelContainer.Emu.Lock := dwPtr;
        pPixelContainer.Format := $24;

        pPixelContainer.Size := (g_dwOverlayW and X_D3DSIZE_WIDTH_MASK)
                             or (g_dwOverlayH shl X_D3DSIZE_HEIGHT_SHIFT)
                             or (g_dwOverlayP shl X_D3DSIZE_PITCH_SHIFT);
      end
      else // X_Format <> X_D3DFMT_YUY2
      begin
        if (bSwizzled or bCompressed) then
        begin
          w := dwWidth;
          h := dwHeight;

          if dwMipMapLevels > 0 then // Dxbx addition, to prevent underflow
          for v := 0 to dwMipMapLevels - 1 do
          begin
            if (((1 shl v) >= w) or ((1 shl v) >= h)) then
            begin
              dwMipMapLevels := v + 1;
              break;
            end;
          end;
        end;

        // create the happy little texture
        if (dwCommonType = X_D3DCOMMON_TYPE_SURFACE) then
        begin
          hRet := IDirect3DDevice8_CreateImageSurface(g_pD3DDevice8,
            dwWidth,
            dwHeight,
            Format,
            @(pResource.Emu.Surface8)
          );

          if (FAILED(hRet)) then
            DxbxKrnlCleanup('CreateImageSurface Failed!');

{$IFDEF DEBUG}
          DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created ImageSurface(0x%.08X, 0x%.08X)', [pResource, pResource.Emu.Surface8]);
          DbgPrintf('EmuIDirect3DResource8_Register: Width:%d, Height:%d, Format:%d', [dwWidth, dwHeight, Ord(Format)]);
{$ENDIF}

        end
        else
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

          // HACK HACK HACK HACK HACK HACK HACK HACK HACK HACK
          // Since most modern graphics cards does not support
          // palette based textures we need to expand it to
          // ARGB texture format
          if (Format = D3DFMT_P8) then //Palette
          begin
            EmuWarning('D3DFMT_P8 -> D3DFMT_A8R8G8B8');

            CacheFormat := Format; // Save this for later
            Format := D3DFMT_A8R8G8B8; // ARGB
          end;

          if (bCubemap) then
          begin
{$IFDEF DEBUG}
            DbgPrintf('CreateCubeTexture(%d,%d, 0,%d, D3DPOOL_MANAGED, 0x%.08X)',
              [dwWidth, dwMipMapLevels, Ord(Format), pResource.Emu.Texture8]);
{$ENDIF}

            hRet := IDirect3DDevice8_CreateCubeTexture(g_pD3DDevice8,
              dwWidth, dwMipMapLevels, 0, Format,
              D3DPOOL_MANAGED, @(pResource.Emu.CubeTexture8));

            if (FAILED(hRet)) then
              DxbxKrnlCleanup('CreateCubeTexture Failed!');

{$IFDEF DEBUG}
            DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created CubeTexture(0x%.08X, 0x%.08X)', [pResource, pResource.Emu.CubeTexture8]);
{$ENDIF}
          end
          else
          begin
{$IFDEF DEBUG}
            DbgPrintf('CreateTexture(%d,%d,%d, 0,%d, D3DPOOL_MANAGED, 0x%.08X)',
              [dwWidth, dwHeight, dwMipMapLevels, Ord(Format), @(pResource.Emu.Texture8)]);
{$ENDIF}

            hRet := IDirect3DDevice8_CreateTexture(g_pD3DDevice8,
              dwWidth, dwHeight, dwMipMapLevels, 0, Format,
              D3DPOOL_MANAGED, @(pResource.Emu.Texture8)
              );

            if (FAILED(hRet)) then
              DxbxKrnlCleanup('CreateTexture Failed!');

{$IFDEF DEBUG}
            DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created Texture (0x%.08X, 0x%.08X)', [pResource, pResource.Emu.Texture8]);
{$ENDIF}
          end;
        end;

        stop := ifThen(bCubemap, 6, 1);

        for r := 0 to stop - 1 do
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
            // copy over data (deswizzle if necessary)
            if (dwCommonType = X_D3DCOMMON_TYPE_SURFACE) then
              hRet := IDirect3DSurface8(pResource.Emu.Surface8).LockRect(LockedRect, NULL, 0)
            else
            begin
              if (bCubemap) then
                hRet := IDirect3DCubeTexture8(pResource.Emu.CubeTexture8).LockRect(D3DCUBEMAP_FACES(r), 0, LockedRect, NULL, 0)
              else
                hRet := IDirect3DTexture8(pResource.Emu.Texture8).LockRect(level, {out}LockedRect, NULL, 0);
            end;

            iRect := Classes.Rect(0, 0, 0, 0);
            iPoint := Classes.Point(0, 0);

            pSrc := pBase;
            pThis.Data := DWORD(pSrc);

            if (IsSpecialResource(pResource.Data) and ((pResource.Data and X_D3DRESOURCE_DATA_FLAG_SURFACE) > 0))
            or (IsSpecialResource(DWORD(pBase)) and ((DWORD(pBase) and X_D3DRESOURCE_DATA_FLAG_SURFACE) > 0)) then
            begin
              EmuWarning('Attempt to registered to another resource''s data (eww!)');

              // TODO -oCXBX: handle this horrible situation
              pDest := LockedRect.pBits;
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
              if (bSwizzled) then
              begin
                if (DWORD(pSrc) = $80000000) then
                begin
                  // TODO -oCXBX: Fix or handle this situation..?
                end
                else
                begin
                  if (CacheFormat = D3DFMT_P8) then //Palette
                  begin
                    EmuWarning('Unsupported texture format D3DFMT_P8, expanding to D3DFMT_A8R8G8B8');

                    //
                    // create texture resource
                    //
                    pPixelData := LockedRect.pBits;
                    dwDataSize := dwMipWidth * dwMipHeight * 4;
                    dwPaletteSize := 256 * 4; // Note: This is not allways true, it can be 256- 128- 64- or 32*4

                    pTextureCache := DxbxMalloc(dwDataSize);
                    pExpandedTexture := DxbxMalloc(dwDataSize);
                    pTexturePalette := DxbxMalloc(256 * 4);

                    // First we need to unswizzle the texture data
                    EmuXGUnswizzleRect
                    (
                      Pointer(DWORD(pSrc) + dwMipOffs), dwMipWidth, dwMipHeight, dwDepth, LockedRect.pBits,
                      LockedRect.Pitch, iRect, iPoint, dwBPP
                    );

                    // Copy the unswizzled data to a temporary buffer
                    memcpy(pTextureCache, pPixelData, dwDataSize);

                    // Copy the currently selected palette's data to the buffer
                    memcpy(pTexturePalette, g_pCurrentPalette, dwPaletteSize);

                    w := 0;
                    c := 0;
                    //p := 0;
                    if (dwDataSize div 4) > 0 then // Dxbx addition, to prevent underflow
                    for y := 0 to (dwDataSize div 4) - 1 do
                    begin
                      if (c = dwMipWidth) then
                      begin
                        w := w + dwMipWidth * 3;
                        c := 0;
                      end;
                      p := Byte(pTextureCache[w]);
                      pExpandedTexture[y * 4 + 0] := pTexturePalette[p * 4 + 0];
                      pExpandedTexture[y * 4 + 1] := pTexturePalette[p * 4 + 1];
                      pExpandedTexture[y * 4 + 2] := pTexturePalette[p * 4 + 2];
                      pExpandedTexture[y * 4 + 3] := pTexturePalette[p * 4 + 3];
                      w := w + 1;
                      c := c + 1;
                    end;

                    // Copy the expanded texture back to the buffer
                    memcpy(pPixelData, pExpandedTexture, dwDataSize);

                    // Flush unused data buffers
                    DxbxFree(pTexturePalette); pTexturePalette := nil;
                    DxbxFree(pExpandedTexture); pExpandedTexture := nil;
                    DxbxFree(pTextureCache); pTextureCache := nil;
                  end
                  else
                  begin
                    EmuXGUnswizzleRect
                    (
                      Pointer(DWORD(pSrc) + dwMipOffs), dwMipWidth, dwMipHeight, dwDepth, LockedRect.pBits,
                      LockedRect.Pitch, iRect, iPoint, dwBPP
                    );
                  end;
                end;
              end
              else if (bCompressed) then
              begin
                // NOTE: compressed size is (dwWidth/2)*(dwHeight/2)/2, so each level divides by 4
                dummy := Pointer(DWORD(pSrc) + dwCompressedOffset); // TODO -oDXBX: Do without dummy!
                memcpy(LockedRect.pBits, dummy, dwCompressedSize shr (level * 2));
                Inc(dwCompressedOffset, (dwCompressedSize shr (level * 2)));
              end
              else
              begin
                pDest := LockedRect.pBits;
                if (DWORD(LockedRect.Pitch) = dwMipPitch) and (dwMipPitch = dwMipWidth * dwBPP) then
                begin
                  memcpy(pDest, Pointer(DWORD(pSrc) + dwMipOffs), dwMipWidth * dwMipHeight * dwBPP);
                end
                else
                begin
                  if dwMipHeight > 0 then // Dxbx addition, to prevent underflow
                  for v := 0 to dwMipHeight - 1 do
                  begin
                    memcpy(pDest, Pointer(DWORD(pSrc) + dwMipOffs), dwMipWidth * dwBPP);

                    pDest := Pointer(DWORD(pDest) + DWORD(LockedRect.Pitch));
                    pSrc := Pointer(DWORD(pSrc) + dwMipPitch);
                  end;
                end;
              end;
            end;

            if (dwCommonType = X_D3DCOMMON_TYPE_SURFACE) then
              IDirect3DSurface8(pResource.Emu.Surface8).UnlockRect()
            else
            begin
              if (bCubemap) then
                IDirect3DCubeTexture8(pResource.Emu.CubeTexture8).UnlockRect(D3DCUBEMAP_FACES(r), 0)
              else
                IDirect3DTexture8(pResource.Emu.Texture8).UnlockRect(level);
            end;

            Inc(dwMipOffs, dwMipWidth * dwMipHeight * dwBPP);

            dwMipWidth := dwMipWidth div 2;
            dwMipHeight := dwMipHeight div 2;
            dwMipPitch := dwMipPitch div 2;
          end;
        end;


        // Debug Texture Dumping
{$IFDEF _DEBUG_DUMP_TEXTURE_REGISTER}
        if (dwCommonType = X_D3DCOMMON_TYPE_SURFACE) then
        begin
          dwDumpSurface := 0;

          sprintf(@szBuffer[0], '_DEBUG_DUMP_TEXTURE_REGISTER %.03d - RegSurface%.03d.bmp', [X_Format, dwDumpSurface]); Inc(dwDumpSurface);
          D3DXSaveSurfaceToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, IDirect3DSurface8(pResource.Emu.Surface8), 0, 0);
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

            IDirect3DCubeTexture8(pResource.Emu.CubeTexture8).GetCubeMapSurface(D3DCUBEMAP_FACES(v), 0, @pSurface);

            D3DXSaveSurfaceToFileA(szBuffer, D3DXIFF_BMP, pSurface, 0, 0);
            Inc(v);
          end;
        end
        else
        begin
          dwDumpTex := 0;
          sprintf(szBuffer,' _DEBUG_DUMP_TEXTURE_REGISTER %.03d - RegTexture%.03d.bmp', [X_Format, dwDumpTex]); Inc(dwDumpTex);
          D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, IDirect3DTexture8(pResource.Emu.Texture8), 0);
        end;
{$endif}
      end;
    end;


    X_D3DCOMMON_TYPE_PALETTE:
    begin
{$IFDEF DEBUG}
      DbgPrintf('EmuIDirect3DResource8_Register: .Palette...');
{$ENDIF}

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

        pResource.Data := ULONG(pBase);
      end;

{$IFDEF DEBUG}
(*      DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created Palette (0x%.08X, 0x%.08X, 0x%.08X)', [pResource.Data, pResource.Size, pResource.AllocationSize]);*)
{$ENDIF}
    end;


    X_D3DCOMMON_TYPE_FIXUP:
    begin
      pFixup := PX_D3DFixup(pResource);

      DxbxKrnlCleanup('IDirect3DResource8::Register -> X_D3DCOMMON_TYPE_FIXUP is not yet supported' +
                #13#10'0x%.08X (pFixup->Common)' +
                #13#10'0x%.08X (pFixup->Data)' +
                #13#10'0x%.08X (pFixup->Lock)' +
                #13#10'0x%.08X (pFixup->Run)' +
                #13#10'0x%.08X (pFixup->Next)' +
                #13#10'0x%.08X (pFixup->Size)',
                [pFixup.Common, pFixup.Data, pFixup.Emu.Lock, pFixup.Run, pFixup.Next, pFixup.Size]);
    end;

  else // case
    DxbxKrnlCleanup('IDirect3DResource8.Register.Common Type 0x%.08X not yet supported', [dwCommonType]);
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirect3DResource8_AddRef
(
  pThis: PX_D3DResource
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  uRet: ULONG;
  dwPtr: DWORD;
  pRefCount: PDWORD;
  pResource8: XTL_PIDirect3DResource8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DResource8_AddRef' +
    #13#10'(' +
    #13#10'   pThis             : 0x%.08X' +
    #13#10');',
    [pThis]);
{$ENDIF}

  uRet := 0;

  if (IsSpecialResource(pThis.Data)) and (pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF > 0) then
  begin
    dwPtr := DWORD(pThis.Emu.Lock);
    pRefCount := PDWORD(dwPtr + g_dwOverlayP*g_dwOverlayH);
    Inc(pRefCount^);
  end
  else
  begin
    pResource8 := pThis.Emu.Resource8;

    if(pThis.Emu.Lock = $8000BEEF) then
    begin
      Inc(pThis.Emu.Lock);
      uRet := pThis.Emu.Lock;
    end
    else if (pResource8 <> nil) then
      uRet := IDirect3DResource8(pResource8)._AddRef();

    pThis.Common := (pThis.Common and (not X_D3DCOMMON_REFCOUNT_MASK))
                 or ((pThis.Common and X_D3DCOMMON_REFCOUNT_MASK) + 1);
  end;

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function XTL_EmuIDirect3DResource8_Release(
  pThis: PX_D3DResource
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  uRet: ULONG;
  dwPtr: DWORD;
  pRefCount: PDWORD;
  pResource8: XTL_PIDirect3DResource8;
  v: int;
{$ifdef _DEBUG_TRACE_VB}
  Type_: D3DRESOURCETYPE;
{$endif}
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DResource8_Release' +
    #13#10'(' +
    #13#10'   pThis               : 0x%.08X' +
    #13#10');',
    [pThis]);
{$ENDIF}

  uRet := 0;

  // HACK: In case the clone technique fails...
  if not Assigned(pThis) then
  begin
    EmuWarning('NULL texture!');

    EmuSwapFS(fsXbox);

    Result := 0;
    Exit;
  end;

  // HACK: Clone textures generated by D3DDevice::GetTexture2
  if (IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_TEXCLON) > 0)) then
  begin
    EmuWarning('Deleting clone texture (from D3DDevice::GetTexture2)...');
//    uRet = IDirect3DBaseTexture8(pThis.Emu.BaseTexture8)._Release();
    Dispose(pThis);
  end
  else if(IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    dwPtr := DWORD(pThis.Emu.Lock);
    pRefCount := PDWORD(dwPtr + g_dwOverlayP*g_dwOverlayH);

    Dec(pRefCount^);
    if pRefCount^ = 0 then
    begin
      if (g_YuvSurface = pThis) then
        g_YuvSurface := NULL;

      // free memory associated with this special resource handle
      DxbxFree(PVOID(dwPtr));
    end;

    EmuSwapFS(fsXbox);
    XTL_EmuIDirect3DDevice8_EnableOverlay(BOOL_FALSE);
    EmuSwapFS(fsWindows);
  end
  else
  begin
    pResource8 := pThis.Emu.Resource8;

    if (pThis.Emu.Lock = $8000BEEF) then
    begin
      FreeMem(PVOID(pThis.Data)); // FreeMem, because XTL_EmuIDirect3DDevice8_CreatePalette2 used AllocMem
      PVOID(pThis.Data) := nil; // Make sure the Data pointer can't be accessed anymore
      Dec(pThis.Emu.Lock);
      uRet := pThis.Emu.Lock;
    end
    else
    begin
      if (pResource8 <> nil) then
      begin
        if (pThis.Data <> 0) then // Dxbx addition : This won't change here, so check outside the loop
          for v := 0 to RESOURCE_CACHE_SIZE-1 do
          begin
            if (g_EmuD3DResourceCache[v].Data = pThis.Data) then
            begin
              g_EmuD3DResourceCache[v].Data := 0;
              break;
            end;
          end;

{$ifdef _DEBUG_TRACE_VB}
        Type_ := IDirect3DResource8(pResource8).GetType();
{$endif}

        uRet := IDirect3DResource8(pResource8)._Release();

        if (uRet = 0) then
        begin
{$IFDEF DEBUG}
          DbgPrintf('EmuIDirect3DResource8_Release: Cleaned up a Resource!');
{$ENDIF}

{$ifdef _DEBUG_TRACE_VB}
          if (Type_ = D3DRTYPE_VERTEXBUFFER) then
          begin
            g_VBTrackTotal.remove(pResource8);
            g_VBTrackDisable.remove(pResource8);
          end;
{$endif}
          //delete pThis;

          pThis.Emu.Resource8 := nil; // Dxbx addition - nil out after decreasing reference count
        end;

        pResource8 := nil; // Dxbx addition - nil out after decreasing reference count
      end;
    end;

    pThis.Common := (pThis.Common and not X_D3DCOMMON_REFCOUNT_MASK) or ((pThis.Common and X_D3DCOMMON_REFCOUNT_MASK) - 1);
  end;

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function XTL_EmuIDirect3DResource8_IsBusy
(
    pThis: PX_D3DResource
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
//var
//  pResource8: XTL_PIDirect3DResource8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DResource8_IsBusy' +
    #13#10'(' +
    #13#10'   pThis             : 0x%.08X' +
    #13#10');',
    [pThis]);
{$ENDIF}
  // pResource8 := pThis.Emu.Resource8;
  EmuSwapFS(fsXbox);

  // TODO -oDxbx : This is just an experiment, as Cxbx returns False, while shadow_tj got
  // 'Dead or alive volleyball' to work when returning True here; Let's see what this flag does :
  Result := g_bIsBusy;
end;

procedure XTL_EmuIDirect3DResource8_GetDevice
(
  pThis: PX_D3DResource;
  ppDevice: XTL_PPIDirect3DDevice8 // PPD3DDevice ?
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : IDirect3DResource8_GetDevice' +
    #13#10'(' +
    #13#10'   pThis             : 0x%.08X' +
    #13#10'   ppDevice          : 0x%.08X' +
    #13#10');',
    [pThis, ppDevice]);
{$ENDIF}

  ppDevice^ := g_pD3DDevice8; // TODO -oDxbx : Is this correct?

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DResource8_GetType
(
    pThis: PX_D3DResource
): X_D3DRESOURCETYPE; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DResource8_GetType' +
    #13#10'(' +
    #13#10'   pThis             : 0x%.08X' +
    #13#10');',
    [pThis]);
{$ENDIF}

  // TODO -oCXBX: Handle situation where the resource type is >7
  Result := X_D3DRESOURCETYPE(IDirect3DResource8(pThis.Emu.Resource8).GetType());

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuLock2DSurface
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuLock2DSurface' +
      #13#10'(' +
      #13#10'   pPixelContainer   : 0x%.08X' +
      #13#10'   FaceType          : 0x%.08X' +
      #13#10'   Level             : 0x%.08X' +
      #13#10'   pLockedRect       : 0x%.08X' +
      #13#10'   pRect             : 0x%.08X' +
      #13#10'   Flags             : 0x%.08X' +
      #13#10');',
      [pPixelContainer, Ord(FaceType), Level, pLockedRect, pRect, Flags]);
{$ENDIF}

  EmuVerifyResourceIsRegistered(pPixelContainer);

  {ignore hRet:=}IDirect3DCubeTexture8(pPixelContainer.Emu.CubeTexture8).LockRect(FaceType, Level, {out}pLockedRect^, pRect, Flags);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuGet2DSurfaceDesc
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuGet2DSurfaceDesc' +
         #13#10'(' +
         #13#10'   pPixelContainer     : 0x%.08X' +
         #13#10'   dwLevel             : 0x%.08X' +
         #13#10'   pDesc               : 0x%.08X' +
         #13#10');',
         [pPixelContainer, dwLevel, pDesc]);
{$ENDIF}

  EmuVerifyResourceIsRegistered(pPixelContainer);

  ZeroMemory(@SurfaceDesc, sizeof(SurfaceDesc));

  if (dwLevel = $FEFEFEFE) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuSurface8: = 0x%.08X', [pPixelContainer.Emu.Surface8]);
{$ENDIF}
    hRet := IDirect3DSurface8(pPixelContainer.Emu.Surface8).GetDesc({out}SurfaceDesc);

    (* marked by cxbx
     Integer dwDumpSurface := 0;

     szBuffer: array [0..255-1] of _char;

     sprintf(szBuffer, 'C:\Aaron\Textures\Surface%.03d.bmp', [dwDumpTexture]); Inc(dwDumpTexture);

     D3DXSaveSurfaceToFileA(szBuffer, D3DXIFF_BMP, pPixelContainer.Emu.Surface8, 0, 0);
    *)
  end
  else
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuTexture8: = 0x%.08X', [pPixelContainer.Emu.Texture8]);
{$ENDIF}

    // TODO -oCXBX: Work on Namco Museum hack later...
    // if pPixelContainer.Emu.Texture8 = (IDirect3DTexture8($078A0044)) then

    hRet := IDirect3DTexture8(pPixelContainer.Emu.Texture8).GetLevelDesc(dwLevel, {out}SurfaceDesc);
    //hRet = IDirect3DSurface8(pPixelContainer.Emu.Surface8).GetDesc({out}SurfaceDesc);
    if FAILED(hRet) then
      EmuWarning('IDirect3DTexture8::GetSurfaceDesc failed!');

    DbgPrintf('Okay');

    (* marked by cxbx
    int dwDumpTexture := 0;

    szBuffer: array [0..255-1] of _char;

    sprintf(szBuffer, 'C:\Aaron\Textures\GetDescTexture%.03d.bmp', [dwDumpTexture]); Inc(dwDumpTexture);

    D3DXSaveTextureToFileA(PAnsiChar(@szBuffer[0]), D3DXIFF_BMP, pPixelContainer.Emu.Texture8, 0);
    *)
  end;

  // rearrange into xbox format (remove D3DPOOL)
  if SUCCEEDED(hRet) then
  begin
    // Convert Format (PC->Xbox)
    pDesc.Format := EmuPC2XB_D3DFormat(SurfaceDesc.Format);
    pDesc.Type_ := X_D3DRESOURCETYPE(SurfaceDesc._Type);

    if (Ord(pDesc.Type_) > 7) then
      DxbxKrnlCleanup('EmuGet2DSurfaceDesc: pDesc->Type > 7');

    pDesc.Usage := SurfaceDesc.Usage;
    pDesc.Size := SurfaceDesc.Size;

    // TODO -oCXBX: Convert from Xbox to PC!!
    if SurfaceDesc.MultiSampleType = D3DMULTISAMPLE_NONE then
      pDesc.MultiSampleType := D3DMULTISAMPLE_TYPE($0011)
    else
      DxbxKrnlCleanup(Format('EmuGet2DSurfaceDesc Unknown Multisample format! (%d)', [Ord(SurfaceDesc.MultiSampleType)]));

    pDesc.Width := SurfaceDesc.Width;
    pDesc.Height := SurfaceDesc.Height;
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuGet2DSurfaceDescD
(
  pPixelContainer: PX_D3DPixelContainer;
  pDesc: PX_D3DSURFACE_DESC
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuD3D8 : EmuGet2DSurfaceDescD' +
      #13#10'(' +
      #13#10'   pPixelContainer   : 0x%.08X' +
      #13#10'   pDesc             : 0x%.08X' +
      #13#10');',
           [pPixelContainer, pDesc]);
  EmuSwapFS(fsXbox);
{$endif}

  Xtl_EmuGet2DSurfaceDesc(pPixelContainer, $FEFEFEFE, pDesc);
end;

function XTL_EmuIDirect3DSurface8_GetDesc
(
  pThis: PX_D3DResource;
  pDesc: PX_D3DSURFACE_DESC
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pSurface8: XTL_PIDirect3DSurface8;
  SurfaceDesc: D3DSURFACE_DESC;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DSurface8_GetDesc' +
    #13#10'(' +
    #13#10'   pThis               : 0x%.08X' +
    #13#10'   pDesc               : 0x%.08X' +
    #13#10');',
    [pThis, pDesc]);
{$ENDIF}

  EmuVerifyResourceIsRegistered(pThis);

  if IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0) then
  begin
    pDesc.Format := EmuPC2XB_D3DFormat(D3DFMT_YUY2);
    pDesc.Height := g_dwOverlayH;
    pDesc.Width := g_dwOverlayW;
    pDesc.MultiSampleType := D3DMULTISAMPLE_TYPE(0);
    pDesc.Size := g_dwOverlayP * g_dwOverlayH;
    pDesc.Type_ := X_D3DRTYPE_SURFACE;
    pDesc.Usage := 0;

    Result := D3D_OK;
  end
  else
  begin
    pSurface8 := pThis.Emu.Surface8;



    Result := IDirect3DSurface8(pSurface8).GetDesc({out}SurfaceDesc);

    // rearrange into windows format (remove D3DPool)
    begin
      // Convert Format (PC->Xbox)
      pDesc.Format := EmuPC2XB_D3DFormat(SurfaceDesc.Format);
      pDesc.Type_ := X_D3DRESOURCETYPE(SurfaceDesc._Type);

      if (Ord(pDesc.Type_) > 7) then
        DxbxKrnlCleanup('EmuIDirect3DSurface8_GetDesc: pDesc->Type > 7');

      pDesc.Usage := SurfaceDesc.Usage;
      pDesc.Size := SurfaceDesc.Size;

      // TODO -oCXBX: Convert from Xbox to PC!!
      if (SurfaceDesc.MultiSampleType = D3DMULTISAMPLE_NONE) then
        pDesc.MultiSampleType := D3DMULTISAMPLE_TYPE($0011)
      else
        DxbxKrnlCleanup('EmuIDirect3DSurface8_GetDesc Unknown Multisample format! (%d)', [Ord(SurfaceDesc.MultiSampleType)]);

      pDesc.Width  := SurfaceDesc.Width;
      pDesc.Height := SurfaceDesc.Height;
    end;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DSurface8_LockRect
(
  pThis: PX_D3DResource;
  pLockedRect: PD3DLOCKED_RECT;
  pRect: PRECT;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  pSurface8 : XTL_PIDirect3DSurface8;
  NewFlags: DWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DSurface8_LockRect' +
    #13#10'(' +
    #13#10'   pThis               : 0x%.08X' +
    #13#10'   pLockedRect         : 0x%.08X' +
    #13#10'   pRect               : 0x%.08X' +
    #13#10'   Flags               : 0x%.08X' +
    #13#10');',
    [pThis, pLockedRect, pRect, Flags]);
{$ENDIF}

  hRet := 0; // Dxbx : Prevent 'not initialized' compiler warning

{$IFDEF DXBX_DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DSurface8_LockRect (pThis->Surface = 0x%8.8X)', [pThis.Emu.Surface8]);
{$ENDIF}

  // Cxbx (shogun) commented this :
  //if(nil=pThis.Emu.Surface8) or (pThis.Emu.Surface8 = XTL_PIDirect3DSurface8($00000004)) then
  //begin
  //  EmuWarning('Invalid Surface!');
  //  EmuSwapFS(fsWindows);
  //  Result := E_FAIL; Exit;
  //end;

  EmuVerifyResourceIsRegistered(pThis);

  if (IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    pLockedRect.Pitch := g_dwOverlayP;
    pLockedRect.pBits := PVOID(pThis.Emu.Lock);

    hRet := D3D_OK;
  end
  else
  begin
    if (Flags and X_D3DLOCK_TILED) > 0 then
      EmuWarning('D3DLOCK_TILED ignored!');

    pSurface8 := pThis.Emu.Surface8;

    NewFlags := 0;

    if (Flags and X_D3DLOCK_READONLY) > 0 then
      NewFlags := NewFlags or D3DLOCK_READONLY;

    if (Flags and (not X_D3DLOCK_ALL_SUPPORTED)) > 0 then
      DxbxKrnlCleanup('EmuIDirect3DTexture8_LockRect: Unknown Flags! (0x%.08X)', [Flags and (not X_D3DLOCK_ALL_SUPPORTED)]);

    try
      // Remove old lock(s)
      IDirect3DSurface8(pSurface8).UnlockRect();

      hRet := IDirect3DSurface8(pSurface8).LockRect({out}pLockedRect^, pRect, NewFlags);

      if (FAILED(hRet)) then
        EmuWarning('LockRect Failed!');
    except
      EmuWarning('Invalid Surface!');
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirect3DBaseTexture8_GetLevelCount
(
  pThis: PX_D3DBaseTexture
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DBaseTexture8_GetLevelCount' +
    #13#10'(' +
    #13#10'   pThis             : 0x%.08X' +
    #13#10');',
    [pThis]);
{$ENDIF}

  EmuVerifyResourceIsRegistered(pThis);
  
  Result := IDirect3DBaseTexture8(pThis.Emu.BaseTexture8).GetLevelCount();

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DTexture8_GetSurfaceLevel2
(
  pThis: PX_D3DTexture;
  Level: UINT
): PX_D3DResource; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pSurfaceLevel: PX_D3DSurface;
  dwSize: DWORD;
  pRefCount: PDWORD;
begin
  // Dxbx note : No EmuSwapFS needed here

  // In a special situation, we are actually returning a memory ptr with high bit set
  if (IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    dwSize := g_dwOverlayP*g_dwOverlayH;

    pRefCount := PDWORD(DWORD(pThis.Emu.Lock) + dwSize);

    // initialize ref count
    Inc(pRefCount^);

    Result := pThis;
    Exit;
  end;

  XTL_EmuIDirect3DTexture8_GetSurfaceLevel(pThis, Level, @pSurfaceLevel);

  Result := pSurfaceLevel;                                                
end;


function XTL_EmuIDirect3DTexture8_LockRect
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
  pTexture8: XTL_PIDirect3DTexture8;
  NewFlags: DWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DTexture8_LockRect' +
           #13#10'(' +
           #13#10'   pThis             : 0x%.08X' +
           #13#10'   Level             : 0x%.08X' +
           #13#10'   pLockedRect       : 0x%.08X' +
           #13#10'   pRect             : 0x%.08X' +
           #13#10'   Flags             : 0x%.08X' +
           #13#10');',
           [pThis, Level, pLockedRect, pRect, Flags]);

  DbgPrintf('EmuD3D8 : EmuIDirect3DTexture8_LockRect (pThis->Texture = 0x%8.8X)', [pThis.Emu.Texture8]);
{$ENDIF}

  EmuVerifyResourceIsRegistered(pThis);

  // check if we have an unregistered YUV2 resource
  if ((pThis <> nil) and IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    pLockedRect.Pitch := g_dwOverlayP;
    pLockedRect.pBits := PVOID(pThis.Emu.Lock);

    hRet := D3D_OK;
  end
  else
  begin
    // Dxbx note : Copied this over from EmuIDirect3DSurface8_LockRect, seems the right thing to do
    if (Flags and X_D3DLOCK_TILED) > 0 then
      EmuWarning('D3DLOCK_TILED ignored!');

    pTexture8 := pThis.Emu.Texture8;

    NewFlags := 0;

    if (Flags and X_D3DLOCK_READONLY) > 0 then
      NewFlags := NewFlags or D3DLOCK_READONLY;

    if (Flags and (not X_D3DLOCK_ALL_SUPPORTED)) > 0 then
      DxbxKrnlCleanup('EmuIDirect3DTexture8_LockRect: Unknown Flags! (0x%.08X)', [Flags and (not X_D3DLOCK_ALL_SUPPORTED)]);

    // Remove old lock(s)
    if (Level = 6) or (Level = 7) or (Level = 8) or (Level = 9) then
    begin
      // HACK: Unreal Championship crashes when the texture level reaches 9...
      EmuWarning('Unreal Championship texture hack applied!');
      hRet := D3DERR_INVALIDCALL;
    end
    else
    begin
      IDirect3DTexture8(pTexture8).UnlockRect(Level);

      hRet := IDirect3DTexture8(pTexture8).LockRect(Level, {out}pLockedRect^, pRect, NewFlags);

      pThis.Common := pThis.Common or X_D3DCOMMON_ISLOCKED;
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirect3DTexture8_GetSurfaceLevel
(
  pThis: PX_D3DTexture;
  Level: UINT;
  ppSurfaceLevel: PPX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  dwSize: DWORD;
  pRefCount: PDWORD;
  pTexture8: XTL_PIDirect3DTexture8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DTexture8_GetSurfaceLevel' +
      #13#10'(' +
      #13#10'   pThis               : 0x%.08X' +
      #13#10'   Level               : 0x%.08X' +
      #13#10'   ppSurfaceLevel      : 0x%.08X' +
      #13#10');',
      [pThis, Level, ppSurfaceLevel]);
{$ENDIF}

  EmuVerifyResourceIsRegistered(pThis);

  // if highest bit is set, this is actually a raw memory pointer (for YUY2 simulation)
  if (IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    dwSize := g_dwOverlayP*g_dwOverlayH;

    pRefCount := PDWORD(DWORD(pThis.Emu.Lock) + dwSize);

    // initialize ref count
    Inc(pRefCount^);

    ppSurfaceLevel^ := PX_D3DSurface(pThis);

    hRet := D3D_OK;
  end
  else
  begin
    pTexture8 := pThis.Emu.Texture8;

    New({var}ppSurfaceLevel^); // Cxbx : new X_D3DSurface();

    ppSurfaceLevel^.Data := $B00BBABE;
    ppSurfaceLevel^.Common := 0;
    ppSurfaceLevel^.Format := 0;
    ppSurfaceLevel^.Size := 0;

    hRet := IDirect3DTexture8(pTexture8).GetSurfaceLevel(Level, @(ppSurfaceLevel^.Emu.Surface8));

    if (FAILED(hRet)) then
    begin
      EmuWarning('EmuIDirect3DTexture8_GetSurfaceLevel Failed!');
    end
    else
    begin
{$IFDEF DEBUG}
      DbgPrintf('EmuD3D8 : EmuIDirect3DTexture8_GetSurfaceLevel := 0x%.08X', [ppSurfaceLevel^.Emu.Surface8]);
{$ENDIF}
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirect3DVolumeTexture8_LockBox
(
  pThis: PX_D3DVolumeTexture; 
  Level: UINT;
  pLockedVolume: PD3DLOCKED_BOX; 
  pBox: PD3DBOX; 
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pVolumeTexture8: XTL_PIDirect3DVolumeTexture8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DVolumeTexture8_LockBox' +
    #13#10'(' +
    #13#10'   pThis             : 0x%.08X' +
    #13#10'   Level             : 0x%.08X' +
    #13#10'   pLockedVolume     : 0x%.08X' +
    #13#10'   pBox              : 0x%.08X' +
    #13#10'   Flags             : 0x%.08X' +
    #13#10');',
    [pThis, Level, pLockedVolume, pBox, Flags]);
{$ENDIF}

  EmuVerifyResourceIsRegistered(pThis);

  pVolumeTexture8 := pThis.Emu.VolumeTexture8;

  Result := IDirect3DVolumeTexture8(pVolumeTexture8).LockBox(Level, {out}pLockedVolume^, pBox, Flags);

  if (FAILED(Result)) then
    EmuWarning('LockBox Failed!');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DCubeTexture8_LockRect
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DCubeTexture8_LockRect' +
    #13#10'(' +
    #13#10'   pThis             : 0x%.08X' +
    #13#10'   FaceType          : 0x%.08X' +
    #13#10'   Level             : 0x%.08X' +
    #13#10'   pLockedBox        : 0x%.08X' +
    #13#10'   pRect             : 0x%.08X' +
    #13#10'   Flags             : 0x%.08X' +
    #13#10');',
    [pThis, Ord(FaceType), Level, pLockedBox, pRect, Flags]);
{$ENDIF}

  EmuVerifyResourceIsRegistered(pThis);

  Result := IDirect3DCubeTexture8(pThis.Emu.CubeTexture8).LockRect(FaceType, Level, {out}pLockedBox^, pRect, Flags);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_Release(): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_Release();');
{$ENDIF}

  IDirect3DDevice8(g_pD3DDevice8)._AddRef();
  Result := IDirect3DDevice8(g_pD3DDevice8)._Release();
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
  begin
    Result := IDirect3DDevice8(g_pD3DDevice8)._Release();
    // Dxbx addition - TODO : is this better ?
    if Result = 0 then
      g_pD3DDevice8 := nil;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_CreateVertexBuffer
(
  Length: UINT;
  Usage: DWORD;
  FVF: DWORD;
  Pool: D3DPOOL;
  ppVertexBuffer: PPX_D3DVertexBuffer
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  // Dxbx note : No EmuSwapFS needed here

  ppVertexBuffer^ := XTL_EmuIDirect3DDevice8_CreateVertexBuffer2(Length);
  Result := D3D_OK;
end;

function XTL_EmuIDirect3DDevice8_CreateVertexBuffer2
(
  Length: UINT
): PX_D3DVertexBuffer; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pD3DVertexBuffer: PX_D3DVertexBuffer;
  hRet: HRESULT;
  NewLength: UINT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreateVertexBuffer2' +
         #13#10'(' +
         #13#10'   Length              : 0x%.08X' +
         #13#10');',
         [Length]);
{$ENDIF}

  NewLength := Length;

  // DXBX Addition, vertuxbuffer length need to be at least a vertexbuffer length large.
  // At least 1 vertexbuffer needs to fit in.
  if (NewLength = DWORD(-1)) or (Length = 0) then
  begin
    // TODO -oCXBX: once this is known to be working, remove the warning
    EmuWarning('Vertex buffer allocation size unknown');
    NewLength := $2000; // temporarily assign a small buffer, which will be increased later
  end;

  New({PX_D3DVertexBuffer}pD3DVertexBuffer);

  hRet := IDirect3DDevice8(g_pD3DDevice8).CreateVertexBuffer(
    NewLength,
    0,
    0,
    D3DPOOL_MANAGED,
    {ppVertexBuffer=}@(pD3DVertexBuffer.Emu.VertexBuffer8)
  );

  if (FAILED(hRet)) then
    EmuWarning('CreateVertexBuffer Failed!');

{$ifdef _DEBUG_TRACK_VB}
  g_VBTrackTotal.insert(pD3DVertexBuffer.Emu.VertexBuffer8);
{$endif}

  EmuSwapFS(fsXbox);

  Result := pD3DVertexBuffer;
end;

procedure XTL_EmuIDirect3DDevice8_EnableOverlay
(
  Enable: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  ddsd2: DDSURFACEDESC2;
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_EnableOverlay' +
    #13#10'(' +
    #13#10'   Enable            : 0x%.08X' +
    #13#10');',
    [Enable]);
{$ENDIF}

  if (Enable = BOOL_FALSE) and (g_pDDSOverlay7 <> NULL) then
  begin
    IDirectDrawSurface7(g_pDDSOverlay7).UpdateOverlay(NULL, IDirectDrawSurface7(g_pDDSPrimary), NULL, DDOVER_HIDE, nil);

    // cleanup overlay clipper
    if (g_pDDClipper <> nil) then
    begin
      IDirectDrawClipper(g_pDDClipper)._Release();
      g_pDDClipper := nil;
    end;

    // cleanup overlay surface
    if Assigned(g_pDDSOverlay7) then
    begin
      IDirectDrawSurface7(g_pDDSOverlay7)._Release();
      g_pDDSOverlay7 := nil;
    end;
  end
  else
  begin
    if (Enable <> BOOL_FALSE) and (g_pDDSOverlay7 = nil) then
    begin
        // initialize overlay surface
      if (g_bSupportsYUY2) then
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
          DxbxKrnlCleanup('Could not create overlay surface');

        hRet := IDirectDraw7(g_pDD7).CreateClipper(0, {out}IDirectDrawClipper(g_pDDClipper), NULL);

        if (FAILED(hRet)) then
          DxbxKrnlCleanup('Could not create overlay clipper');

        {Dxbx unused hRet :=} IDirectDrawClipper(g_pDDClipper).SetHWnd(0, g_hEmuWindow);
      end;
    end;
  end;
  
  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_UpdateOverlay
(
  pSurface: PX_D3DSurface;
  SrcRect: PRECT;
  DstRect: PRECT;
  EnableColorKey: BOOL;
  ColorKey: D3DCOLOR
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  ddsd2: DDSURFACEDESC2;
  pDest: P_char;
  pSour: P_char;
  w: DWORD;
  h: DWORD;
  v: UInt32;
  SourRect: TRect;
  DestRect: TRect;
  y: Integer;
  LockedRectDest: D3DLOCKED_RECT;
  pBackBuffer: XTL_PIDirect3DSurface8;
  hRet: HRESULT;

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
  Y2: array [0..2-1] of Float;
  U2: Float;
  V2: Float;
  a: Integer;
  x: Integer;

  Y3: uint08;

  R: Float;
  G: Float;
  B: Float;
  i: uint32;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_UpdateOverlay' +
    #13#10'(' +
    #13#10'   pSurface          : 0x%.08X' +
    #13#10'   SrcRect           : 0x%.08X' +
    #13#10'   DstRect           : 0x%.08X' +
    #13#10'   EnableColorKey    : 0x%.08X' +
    #13#10'   ColorKey          : 0x%.08X' +
    #13#10');',
    [pSurface, SrcRect, DstRect, EnableColorKey, ColorKey]);
{$ENDIF}

// Cxbx has #ifndef UnrealChampionshipHack
  if Assigned(pSurface) then
  begin
    // manually copy data over to overlay
    if (g_bSupportsYUY2) then
    begin
      ZeroMemory(@ddsd2, sizeof(ddsd2));

      ddsd2.dwSize := sizeof(ddsd2);

      if (FAILED(IDirectDrawSurface7(g_pDDSOverlay7).Lock(NULL, {out}ddsd2, DDLOCK_SURFACEMEMORYPTR or DDLOCK_WAIT, 0))) then
        EmuWarning('Unable to lock overlay surface!');

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
    end;

    // update overlay!
    if (g_bSupportsYUY2) then
    begin
      SourRect := Classes.Rect(0, 0, g_dwOverlayW, g_dwOverlayH);
      ZeroMemory(@aMonitorInfo, sizeof(aMonitorInfo));

      nTitleHeight  := 0;//GetSystemMetrics(SM_CYCAPTION);
      nBorderWidth  := 0;//GetSystemMetrics(SM_CXSIZEFRAME);
      nBorderHeight := 0;//GetSystemMetrics(SM_CYSIZEFRAME);

      aMonitorInfo.cbSize := sizeof(MONITORINFO);
      GetMonitorInfo(g_hMonitor, @aMonitorInfo);

      GetWindowRect(g_hEmuWindow, {var}DestRect);

      Inc(DestRect.left,   nBorderWidth);
      Dec(DestRect.right,  nBorderWidth);
      Inc(DestRect.top,    nTitleHeight + nBorderHeight);
      Dec(DestRect.bottom, nBorderHeight);

      Dec(DestRect.left,   aMonitorInfo.rcMonitor.left);
      Dec(DestRect.right,  aMonitorInfo.rcMonitor.left);
      Dec(DestRect.top,    aMonitorInfo.rcMonitor.top);
      Dec(DestRect.bottom, aMonitorInfo.rcMonitor.top);

      ZeroMemory(@ddofx, sizeof(ddofx));

      ddofx.dwSize := sizeof(DDOVERLAYFX);
      ddofx.dckDestColorkey.dwColorSpaceLowValue := 0;
      ddofx.dckDestColorkey.dwColorSpaceHighValue := 0;

      {Dxbx unused hRet :=} IDirectDrawSurface7(g_pDDSOverlay7).UpdateOverlay(@SourRect, IDirectDrawSurface7(g_pDDSPrimary), @DestRect, {DDOVER_KEYDESTOVERRIDE or} DDOVER_SHOW, {&ddofx}nil);
    end
    else
    begin
      // TODO -oCXBX: dont assume X8R8G8B8 ?
      pBackBuffer := nil;

      hRet := IDirect3DDevice8(g_pD3DDevice8).GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, @pBackBuffer);

      // if we obtained the backbuffer, manually translate the YUY2 into the backbuffer format
      if (hRet = D3D_OK) and (IDirect3DSurface8(pBackBuffer).LockRect({out}LockedRectDest, NULL, 0) = D3D_OK) then
      begin
        pCurByte := Puint08(pSurface.Emu.Lock);

        pDest2 := Puint08(LockedRectDest.pBits);

        dx:=0; dy:=0;

        dwImageSize := g_dwOverlayP*g_dwOverlayH;

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
        // full color conversion (YUY2->XRGB)
        else
        begin
          v := 0; while v < dwImageSize do
          begin
            Y2[0] := pCurByte^; Inc(pCurByte);
            U2 := pCurByte^; Inc(pCurByte);
            Y2[1] := pCurByte^; Inc(pCurByte);
            V2 := pCurByte^; Inc(pCurByte);

            a := 0;
            for x := 0 to 2-1 do
            begin
              R := Y2[a] + 1.402*(V2-128);
              G := Y2[a] - 0.344*(U2-128) - 0.714*(V2-128);
              B := Y2[a] + 1.772*(U2-128);

              if R < 0 then R := 0; if R > 255 then R := 255;
              if G < 0 then G := 0; if G > 255 then G := 255;
              if B < 0 then B := 0; if B > 255 then B := 255;

              i := (dy*uint32(LockedRectDest.Pitch)+(dx+uint32(x))*4);

              pDest2[i+0] := Round(B);
              pDest2[i+1] := Round(G);
              pDest2[i+2] := Round(R);
              pDest2[i+3] := $FF;

              Inc(a);
            end;

            Inc(dx, 2);

            if ((dx mod g_dwOverlayW) = 0) then
            begin
              Inc(dy);
              dx := 0;
            end;

            Inc(v, 4);
          end; // while
        end;

        IDirect3DSurface8(pBackBuffer).UnlockRect();
        IDirect3DSurface8(pBackBuffer)._Release;
      end;

      // Update overlay if present was not called since the last call to
      // EmuIDirect3DDevice8_UpdateOverlay.
      if g_bHackUpdateSoftwareOverlay then
        DxbxPresent(nil, nil, 0, nil);

      g_bHackUpdateSoftwareOverlay := TRUE;
    end;
  end
  else
  begin
    EmuWarning('pSurface == NULL!');
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_GetOverlayUpdateStatus(): LONGBOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetOverlayUpdateStatus();');
{$ENDIF}

//  Result := D3DDevice_GetOverlayUpdateStatus();

  EmuSwapFS(fsXbox);

  Result := True;
end;

procedure XTL_EmuIDirect3DDevice8_BlockUntilVerticalBlank(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_BlockUntilVerticalBlank();');
{$ENDIF}

  // Marked out by cxbx
  // segaGT tends to freeze with this on
  //    if (g_XBVideo.GetVSync())
  IDirectDraw7(g_pDD7).WaitForVerticalBlank(DDWAITVB_BLOCKBEGIN, 0);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetVerticalBlankCallback
(
  pCallback: D3DVBLANKCALLBACK
);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVerticalBlankCallback' +
    #13#10'(' +
    #13#10'   pCallback         : 0x%.08X' +
    #13#10');',
    [Addr(pCallback)]); // Dxbx: Check this causes no call!
{$ENDIF}

  if IsValidAddress(Addr(pCallback)) then
    g_pVBCallback := pCallback
  else
    // Dxbx note : Zapper passes the Handle of a previously created thread here... wierd!
    EmuWarning('SetVerticalBlankCallback ignored invalid Callback address');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetTextureState_TexCoordIndex
(
  Stage: DWORD;
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetTextureState_TexCoordIndex' +
    #13#10'(' +
    #13#10'   Stage               : 0x%.08X' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Stage, Value]);
{$ENDIF}

  if (Value > $00030000) then
    DxbxKrnlCleanup('EmuIDirect3DDevice8_SetTextureState_TexCoordIndex: Unknown TexCoordIndex Value (0x%.08X)', [Value]);

  IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(Stage, D3DTSS_TEXCOORDINDEX, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetTextureState_TwoSidedLighting
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetTextureState_TwoSidedLighting' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('TwoSidedLighting is not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_BackFillMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_BackFillMode' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('BackFillMode is not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetTextureState_BorderColor
(
  Stage: DWORD;
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetTextureState_BorderColor' +
    #13#10'(' +
    #13#10'   Stage             : 0x%.08X' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Stage, Value]);
{$ENDIF}

  IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(Stage, D3DTSS_BORDERCOLOR, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetTextureState_ColorKeyColor
(
  Stage: DWORD;
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetTextureState_ColorKeyColor' +
    #13#10'(' +
    #13#10'   Stage             : 0x%.08X' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Stage, Value]);
{$ENDIF}

  EmuWarning('SetTextureState_ColorKeyColor is not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetTextureState_BumpEnv
(
    Stage: DWORD;
    Type_: X_D3DTEXTURESTAGESTATETYPE;
    Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetTextureState_BumpEnv' +
         #13#10'(' +
         #13#10'   Stage             : 0x%.08X' +
         #13#10'   Type              : 0x%.08X' +
         #13#10'   Value             : 0x%.08X' +
         #13#10');',
         [Stage, Type_, Value]);
{$ENDIF}

  case (Type_) of
    X_D3DTSS_BUMPENVMAT00:
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(Stage, D3DTSS_BUMPENVMAT00, Value);
    X_D3DTSS_BUMPENVMAT01:
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(Stage, D3DTSS_BUMPENVMAT01, Value);
    X_D3DTSS_BUMPENVMAT11:
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(Stage, D3DTSS_BUMPENVMAT11, Value);
    X_D3DTSS_BUMPENVMAT10:
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(Stage, D3DTSS_BUMPENVMAT10, Value);
    X_D3DTSS_BUMPENVLSCALE:
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(Stage, D3DTSS_BUMPENVLSCALE, Value);
   end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_FrontFace
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_FrontFace' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('SetRenderState_FrontFace not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_LogicOp
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_LogicOp' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('SetRenderState_LogicOp is not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_NormalizeNormals
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_NormalizeNormals' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_NORMALIZENORMALS, Value);
  
  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_TextureFactor
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_TextureFactor' +
    #13#10'(' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_TEXTUREFACTOR, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_ZBias
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_ZBias' +
    #13#10'(' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_ZBIAS, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_EdgeAntiAlias
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_EdgeAntiAlias' +
    #13#10'(' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  // TODO -oCXBX: Analyze performance and compatibility (undefined behavior on PC with triangles or points)
  // IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_EDGEANTIALIAS, Value);
  
  // Cxbx commented this out: EmuWarning('SetRenderState_EdgeAntiAlias not implemented!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_FillMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  dwFillMode: DWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_FillMode' +
    #13#10'(' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  if ((Value and $FF00) = $1B00) then
  begin
    if(g_iWireframe = 0) then
      dwFillMode := EmuXB2PC_D3DFILLMODE(Value)
    else
    begin
      if(g_iWireframe = 1) then
        dwFillMode := D3DFILL_WIREFRAME
      else
        dwFillMode := D3DFILL_POINT;
    end;

    IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_FILLMODE, dwFillMode);
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_FogColor
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_FogColor' +
    #13#10'(' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_FOGCOLOR, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_Dxt1NoiseEnable
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_Dxt1NoiseEnable' +
    #13#10'(' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('SetRenderState_Dxt1NoiseEnable not implemented!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_Simple(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Value: DWORD;
  {1 ECX}Method: DWORD
  ); register; // VALIDATED fastcall simulation - See Translation guide
// Branch:shogun  Revision:20100412  Translator:Shadow_Tj  Done:100
var
  State: D3DRenderStateType;//int;
//  v: int;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_Simple' +
    #13#10'(' +
    #13#10'   Method              : 0x%.08X' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Method, Value]);
{$ENDIF}

  int(State) := -1;

(*  // TODO -oCXBX: make this faster and more elegant
  for v := 0 to 174-1 do
  begin
    if (EmuD3DRenderStateSimpleEncoded[v] = Method) then
    begin
      State := D3DRenderStateType(v);
      break;
    end;
  end;
*)
  // Dxbx note : Let the compiler sort this out, should be much quicker :
  case Method of
    $040300: State := D3DRS_ALPHATESTENABLE;
    $040304: State := D3DRS_ALPHABLENDENABLE;
    $040310: State := D3DRS_DITHERENABLE;
    $04033c: State := D3DRS_ALPHAFUNC;
    $040340: State := D3DRS_ALPHAREF;
    $040344: State := D3DRS_SRCBLEND;
    $040348: State := D3DRS_DESTBLEND;
    $040350: State := D3DRS_BLENDOP;
    $040354: State := D3DRS_ZFUNC;
    $040358: State := D3DRS_COLORWRITEENABLE;
    $04035c: State := D3DRS_ZWRITEENABLE;
    $040360: State := D3DRS_STENCILWRITEMASK;
    $040364: State := D3DRS_STENCILFUNC;
    $040368: State := D3DRS_STENCILREF;
    $04036c: State := D3DRS_STENCILMASK;
    $040374: State := D3DRS_STENCILZFAIL;
    $040378: State := D3DRS_STENCILPASS;
    $04037c: State := D3DRS_SHADEMODE;
  end;
(**)
  if (int(State) = -1) then
    EmuWarning('RenderState_Simple(0x%.08X, 0x%.08X) is unsupported!', [Method, Value])
  else
  begin
    case _D3DRENDERSTATETYPE(State) of
      D3DRS_COLORWRITEENABLE:
        begin
          Value := EmuXB2PC_D3DCOLORWRITEENABLE(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_COLORWRITEENABLE := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_SHADEMODE:
        begin
          Value := EmuXB2PC_D3DSHADEMODE(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_SHADEMODE := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_BLENDOP:
        begin
          Value := EmuXB2PC_D3DBLENDOP(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_BLENDOP := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_SRCBLEND:
        begin
          Value := EmuXB2PC_D3DBLEND(X_D3DBLEND(Value));
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_SRCBLEND := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_DESTBLEND:
        begin
          Value := EmuXB2PC_D3DBLEND(X_D3DBLEND(Value));
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_DESTBLEND := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_ZFUNC:
        begin
          Value := EmuXB2PC_D3DCMPFUNC(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ZFUNC := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_ALPHAFUNC:
        begin
          Value := EmuXB2PC_D3DCMPFUNC(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ALPHAFUNC := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_ALPHATESTENABLE:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ALPHATESTENABLE := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_ALPHABLENDENABLE:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ALPHABLENDENABLE := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_ALPHAREF:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ALPHAREF := %f', [DWtoF(Value)]);
{$ENDIF}
        end;

      D3DRS_ZWRITEENABLE:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ZWRITEENABLE := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_DITHERENABLE:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_DITHERENABLE := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_STENCILZFAIL:
        begin
          Value := EmuXB2PC_D3DSTENCILOP(X_D3DSTENCILOP(Value));
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_STENCILZFAIL := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_STENCILPASS:
        begin
          Value := EmuXB2PC_D3DSTENCILOP(X_D3DSTENCILOP(Value));
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_STENCILPASS := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_STENCILFUNC:
        begin
          Value := EmuXB2PC_D3DCMPFUNC(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_STENCILFUNC := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_STENCILREF:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_STENCILREF := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_STENCILMASK:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_STENCILMASK := 0x%.08X', [Value]);
{$ENDIF}
        end;

      D3DRS_STENCILWRITEMASK:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_STENCILWRITEMASK := 0x%.08X', [Value]);
{$ENDIF}
        end;

    else
      begin
        DxbxKrnlCleanup('Unsupported RenderState (0x%.08X)', [int(State)]);
      end;
    end;

    // TODO -oCXBX: verify these params as you add support for them!
    IDirect3DDevice8(g_pD3DDevice8).SetRenderState({D3DRENDERSTATETYPE}(State), Value);
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_VertexBlend
(
  Value: X_D3DVERTEXBLENDFLAGS
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_VertexBlend' +
    #13#10'(' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Ord(Value)]);
{$ENDIF}

  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_VERTEXBLEND, EmuXB2PC_D3DVERTEXBLENDFLAGS(Value));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_PSTextureModes
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_PSTextureModes' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  // TODO -oCXBX: do something..

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_CullMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:162  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_CullMode' +
    #13#10'(' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  // convert from Xbox D3D to PC D3D enumeration
  // TODO -oCXBX: XDK-Specific Tables? So far they are the same
  case (Value) of
    X_D3DCULL_NONE: // 0
      Value := D3DCULL_NONE;
    X_D3DCULL_CW: // $900
      Value := D3DCULL_CW;
    X_D3DCULL_CCW: // $901
      Value := D3DCULL_CCW;
  else
    DxbxKrnlCleanup('EmuIDirect3DDevice8_SetRenderState_CullMode: Unknown Cullmode (%d)', [Value]);
  end;

  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_CULLMODE, Value);
  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_LineWidth
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_LineWidth' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  // TODO -oCXBX: Convert to PC format??
//  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_LINEPATTERN, Value);
  EmuWarning('SetRenderState_LineWidth is not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_StencilFail
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_StencilFail' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_STENCILFAIL, Value);
  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_OcclusionCullEnable
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_OcclusionCullEnable' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('SetRenderState_OcclusionCullEnable not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_StencilCullEnable
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_StencilCullEnable' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('SetRenderState_StencilCullEnable not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_RopZCmpAlwaysRead
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_RopZCmpAlwaysRead' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('SetRenderState_RopZCmpAlwaysRead not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_RopZRead
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_RopZRead' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('SetRenderState_RopZRead not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_DoNotCullUncompressed
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_DoNotCullUncompressed' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('SetRenderState_DoNotCullUncompressed not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_ZEnable
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_ZEnable' +
    #13#10'(' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}
    
  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_ZENABLE, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_StencilEnable
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_StencilEnable' +
    #13#10'(' +
    #13#10'   Value               : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_STENCILENABLE, Value);
  
  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_MultiSampleAntiAlias
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_MultiSampleAntiAlias' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  // TODO -oDxbx: If Value is D3DMULTISAMPLE_TYPE, then we should convert it from Xbox to Native!
  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_MULTISAMPLEANTIALIAS, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_MultiSampleMask
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_MultiSampleMask' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_MULTISAMPLEMASK, Value);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_MultiSampleMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_MultiSampleMode' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('SetRenderState_MultiSampleMode is not supported!'); // TODO -oDxbx : Use D3DMULTISAMPLE_TYPE somewhere for this?

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_MultiSampleRenderTargetMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_MultiSampleRenderTargetMode' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('SetRenderState_MultiSampleRenderTargetMode is not supported!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_ShadowFunc
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_ShadowFunc' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('ShadowFunc not implemented');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_YuvEnable
(
  Enable: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_YuvEnable' +
    #13#10'(' +
    #13#10'   Enable            : 0x%.08X' +
    #13#10');',
    [Enable]);
{$ENDIF}

  // HACK: Display YUV surface by using an overlay.
  if (Enable <> BOOL_FALSE) <> g_fYuvEnabled then
  begin
    g_fYuvEnabled := (Enable <> BOOL_FALSE);

    EmuWarning('EmuIDirect3DDevice8_SetRenderState_YuvEnable using overlay!');
    EmuSwapFS(fsXbox);
    XTL_EmuIDirect3DDevice8_EnableOverlay(BOOL(g_fYuvEnabled));
    EmuSwapFS(fsWindows);
  end;

  if (g_fYuvEnabled) then
  begin
    EmuSwapFS(fsXbox);
    XTL_EmuIDirect3DDevice8_UpdateOverlay(g_YuvSurface, nil, nil, BOOL_FALSE, 0);
    EmuSwapFS(fsWindows);
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_SetTransform
(
  State: D3DTRANSFORMSTATETYPE;
  {CONST} pMatrix: PD3DMATRIX
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetTransform' +
    #13#10'(' +
    #13#10'   State               : 0x%.08X' +
    #13#10'   pMatrix             : 0x%.08X' +
    #13#10');',
    [Ord(State), pMatrix]);

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

  State := EmuXB2PC_D3DTS(State);

  Result := IDirect3DDevice8(g_pD3DDevice8).SetTransform(State, pMatrix);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_GetTransform
(
  State: D3DTRANSFORMSTATETYPE;
  pMatrix: PD3DMATRIX
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetTransform' +
    #13#10'(' +
    #13#10'   State             : 0x%.08X' +
    #13#10'   pMatrix           : 0x%.08X' +
    #13#10');',
    [Ord(State), pMatrix]);
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).GetTransform(EmuXB2PC_D3DTS(State), {out}pMatrix^);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DVertexBuffer8_Lock
(
  ppVertexBuffer: PX_D3DVertexBuffer;
  OffsetToLock: UINT;
  SizeToLock: UINT;
  ppbData: PPBYTE;
  Flags: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pVertexBuffer8: XTL_PIDirect3DVertexBuffer8;
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DVertexBuffer8_Lock' +
    #13#10'(' +
    #13#10'   ppVertexBuffer    : 0x%.08X' +
    #13#10'   OffsetToLock      : 0x%.08X' +
    #13#10'   SizeToLock        : 0x%.08X' +
    #13#10'   ppbData           : 0x%.08X' +
    #13#10'   Flags             : 0x%.08X' +
    #13#10');',
    [ppVertexBuffer, OffsetToLock, SizeToLock, ppbData, Flags]);
{$ENDIF}

  pVertexBuffer8 := ppVertexBuffer.Emu.VertexBuffer8;

  hRet := IDirect3DVertexBuffer8(pVertexBuffer8).Lock(OffsetToLock, SizeToLock, {out}ppbData^, Flags);
  (*DbgPrintf('VertexBuffer 0x%08X was locked: 0x%x - 0x%x, hRet = 0x%08x', [
        pVertexBuffer8,
        OffsetToLock,
        SizeToLock,
        hRet
        ]);*)

  if (FAILED(hRet)) then
    EmuWarning('VertexBuffer Lock Failed!');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DVertexBuffer8_Lock2
(
  ppVertexBuffer: PX_D3DVertexBuffer;
  Flags: DWORD
): PByte; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pVertexBuffer8: XTL_PIDirect3DVertexBuffer8;
  pbData: PBYTE;
//  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DVertexBuffer8_Lock2' +
         #13#10'(' +
         #13#10'   ppVertexBuffer      : 0x%.08X' +
         #13#10'   Flags               : 0x%.08X' +
         #13#10');',
         [ppVertexBuffer, Flags]);
{$ENDIF}

  pVertexBuffer8 := ppVertexBuffer^.Emu.VertexBuffer8;
  pbData := NULL;

  {Dxbx unused hRet :=} IDirect3DVertexBuffer8(pVertexBuffer8).Lock(0, 0, {out}pbData, EmuXB2PC_D3DLock(Flags));    // Fixed flags check, Battlestar Galactica now displays graphics correctly

{$IFDEF DEBUG}
  (*DbgPrintf('VertexBuffer 0x%08X was locked (2): hRet = 0x%08x', [
        pVertexBuffer8,
        hRet
        ]);*)
  DbgPrintf('pbData : 0x%.08X', [pbData]);
{$ENDIF}
  EmuSwapFS(fsXbox);

  Result := pbData;
end;

function XTL_EmuIDirect3DDevice8_GetStreamSource2
(
  StreamNumber: UINT;
  pStride: PUINT
): PX_D3DVertexBuffer; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pVertexBuffer: PX_D3DVertexBuffer;
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetStreamSource2' +
            #13#10'(' +
            #13#10'   StreamNumber             : 0x%.08X' +
            #13#10'   pStride                  : 0x%.08X' +
            #13#10');',
            [StreamNumber, pStride]);
{$ENDIF}

  EmuWarning('Not correctly implemented yet!');
  {ignore HRESULT?}IDirect3DDevice8(g_pD3DDevice8).GetStreamSource(
    StreamNumber,
    {ppVertexBuffer=}@pVertexBuffer,
    {out}pStride^);

  EmuSwapFS(fsXbox);
  Result := pVertexBuffer;
end;

function XTL_EmuIDirect3DDevice8_SetStreamSource
(
  StreamNumber: UINT;
  pStreamData: PX_D3DVertexBuffer;
  Stride: UINT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pVertexBuffer8: XTL_PIDirect3DVertexBuffer8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  if Assigned(pStreamData) then
    DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetStreamSource' +
      #13#10'(' +
      #13#10'   StreamNumber        : 0x%.08X' +
      #13#10'   pStreamData         : 0x%.08X (0x%.08X)' +
      #13#10'   Stride              : 0x%.08X' +
      #13#10');',
      [StreamNumber, pStreamData, pStreamData.Emu.VertexBuffer8, Stride])
  else
    DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetStreamSource' +
      #13#10'(' +
      #13#10'   StreamNumber        : 0x%.08X' +
      #13#10'   pStreamData         : 0x%.08X (0x%.08X)' +
      #13#10'   Stride              : 0x%.08X' +
      #13#10');',
      [StreamNumber, pStreamData, 0, Stride]);
{$ENDIF}

  if (StreamNumber = 0) then
    g_pVertexBuffer := pStreamData;

  pVertexBuffer8 := NULL;

  if (pStreamData <> NULL) then
  begin
    EmuVerifyResourceIsRegistered(pStreamData);

    pVertexBuffer8 := pStreamData.Emu.VertexBuffer8;
    if Assigned(pVertexBuffer8) then
      IDirect3DVertexBuffer8(pVertexBuffer8).Unlock();
  end;

  {$ifdef _DEBUG_TRACK_VB}
  if (pStreamData <> NULL) then
  begin
    g_bVBSkipStream := g_VBTrackDisable.exists(pStreamData.Emu.VertexBuffer8);
  end;
  {$endif}

  Result := IDirect3DDevice8(g_pD3DDevice8).SetStreamSource(StreamNumber, IDirect3DVertexBuffer8(pVertexBuffer8), Stride);

  if FAILED(RESULT) then
    DxbxKrnlCleanup('SetStreamSource Failed!');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_SetVertexShader
(
  Handle: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  RealHandle: DWORD;
const
  vScale: TD3DXVECTOR4 = (x: 2.0 / 640; y: -2.0 / 480; z: 0.0; w: 0.0);
  vOffset: TD3DXVECTOR4 = (x: -1.0; y: 1.0; z: 0.0; w: 1.0);
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexShader' +
    #13#10'(' +
    #13#10'   Handle              : 0x%.08X' +
    #13#10');',
    [Handle]);
{$ENDIF}

  // Result := D3D_OK;

  g_CurrentVertexShader := Handle;

  // Store viewport offset and scale in constant registers 58 (c-38) and
  // 59 (c-37) used for screen space transformation.
  if (g_VertexShaderConstantMode and X_VSCM_NONERESERVED) = 0 then // Dxbx fix
  begin
    // TODO -oCXBX: Proper solution.
//    with vScale do begin x := 2.0 / 640; y := -2.0 / 480; z := 0.0; w := 0.0; end;
//    with vOffset do begin x := -1.0; y := 1.0; z := 0.0; w := 1.0; end;

    IDirect3DDevice8(g_pD3DDevice8).SetVertexShaderConstant({58=}X_VSCM_RESERVED_CONSTANT1{=-38}+X_VSCM_CORRECTION{=96}, @vScale, 1);
    IDirect3DDevice8(g_pD3DDevice8).SetVertexShaderConstant({59=}X_VSCM_RESERVED_CONSTANT2{=-37}+X_VSCM_CORRECTION{=96}, @vOffset, 1);
  end;

  if (VshHandleIsVertexShader(Handle)) then
  begin
    RealHandle := PVERTEX_SHADER(VshHandleGetVertexShader(Handle).Handle).Handle
  end
  else
  begin
    // Dxbx addition : When setting D3DFVF_XYZRHW, g_VertexShaderSlots 0-D3DVS_XBOX_RESERVEDXYZRHWSLOTS-1 should be destroyed :
    if (Handle and D3DFVF_POSITION_MASK) = D3DFVF_XYZRHW then
      for RealHandle := 0 to D3DVS_XBOX_RESERVEDXYZRHWSLOTS-1 do
        g_VertexShaderSlots[RealHandle] := 0;

    RealHandle := Handle;
  end;
  Result := IDirect3DDevice8(g_pD3DDevice8).SetVertexShader(RealHandle);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_DrawVertices
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DrawVertices' +
    #13#10'(' +
    #13#10'   PrimitiveType       : 0x%.08X' +
    #13#10'   StartVertex         : 0x%.08X' +
    #13#10'   VertexCount         : 0x%.08X' +
    #13#10');',
    [Ord(PrimitiveType), StartVertex, VertexCount]);
{$ENDIF}

  XTL_EmuUpdateDeferredStates();

//  EmuUnswizzleActiveTexture(); // This messes up textures in PSTest2_4627

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

      IDirect3DDevice8(g_pD3DDevice8).DrawPrimitive
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

procedure XTL_EmuIDirect3DDevice8_DrawVerticesUP
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DrawVerticesUP' +
    #13#10'(' +
    #13#10'   PrimitiveType            : 0x%.08X' +
    #13#10'   VertexCount              : 0x%.08X' +
    #13#10'   pVertexStreamZeroData    : 0x%.08X' +
    #13#10'   VertexStreamZeroStride   : 0x%.08X' +
    #13#10');',
    [Ord(PrimitiveType), VertexCount, pVertexStreamZeroData,
    VertexStreamZeroStride]);
{$ENDIF}

  XTL_EmuUpdateDeferredStates();

//  EmuUnswizzleActiveTexture(); // This messes up the letters in Chunktro

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

      IDirect3DDevice8(g_pD3DDevice8).DrawPrimitiveUP
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

function XTL_EmuIDirect3DDevice8_DrawIndexedVertices
(
  PrimitiveType: X_D3DPRIMITIVETYPE;
  VertexCount: UINT; 
  pIndexData: PWORD
): HRESULT; stdcall;
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
  pIndexBuffer: XTL_PIDirect3DIndexBuffer8;
  BaseIndex: UINT;

  uiNumVertices: UINT;
  uiStartIndex: UINT;
  FatalError: _bool;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DrawIndexedVertices' +
      #13#10'(' +
      #13#10'   PrimitiveType       : 0x%.08X' +
      #13#10'   VertexCount         : 0x%.08X' +
      #13#10'   pIndexData          : 0x%.08X' +
      #13#10');',
    [Ord(PrimitiveType), VertexCount, pIndexData]);
{$ENDIF}

  // update index buffer, if necessary
  if (g_pIndexBuffer <> nil) and (g_pIndexBuffer.Emu.Lock = X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
  begin
    dwSize := VertexCount * SizeOf(Word);   // 16-bit indices

    hRet := IDirect3DDevice8_CreateIndexBuffer
    (g_pD3DDevice8,
        dwSize, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED,
        @(g_pIndexBuffer.Emu.IndexBuffer8)
    );

    if (FAILED(hRet)) then
        DxbxKrnlCleanup('CreateIndexBuffer Failed!');

    pData := nil;

    hRet := IDirect3DIndexBuffer8(g_pIndexBuffer.Emu.IndexBuffer8).Lock(0, dwSize, {out}pData, 0);

    if (FAILED(hRet)) then
        DxbxKrnlCleanup('IndexBuffer Lock Failed!');

    memcpy(pData, Pvoid(g_pIndexBuffer.Data), dwSize);

    IDirect3DIndexBuffer8(g_pIndexBuffer.Emu.IndexBuffer8).Unlock();

    g_pIndexBuffer.Data := ULONG(pData);

    hRet := IDirect3DDevice8(g_pD3DDevice8).SetIndices(IDirect3DIndexBuffer8(g_pIndexBuffer.Emu.IndexBuffer8), g_dwBaseVertexIndex);

    if (FAILED(hRet)) then
        DxbxKrnlCleanup('SetIndices Failed!');
  end;

  XTL_EmuUpdateDeferredStates();

  EmuUnswizzleActiveTexture();

  if (PrimitiveType = X_D3DPT_LINELOOP) or (PrimitiveType = X_D3DPT_QUADLIST) then
    EmuWarning('Unsupported PrimitiveType! (%d)', [DWORD(PrimitiveType)]);

  VPDesc.VertexPatchDesc(); // Dxbx addition : explicit initializer

  VPDesc.PrimitiveType := PrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
  VPDesc.dwOffset := 0;
  VPDesc.pVertexStreamZeroData := nil;
  VPDesc.uiVertexStreamZeroStride := 0;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.VertexPatcher(); // Dxbx addition : explicit initializer
  FatalError := false;

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc, @FatalError);
//  VertexCount := VPDesc.dwVertexCount; // TODO -oDxbx : Shouldn't we use the new VertexCount?!?

  {$ifdef _DEBUG_TRACK_VB}
  if not g_bVBSkipStream then
  begin
  {$endif}

    bActiveIB := false;

    pIndexBuffer := nil;

    // check if there is an active index buffer
    begin
      BaseIndex := 0;

      IDirect3DDevice8(g_pD3DDevice8).GetIndices(@pIndexBuffer, {out}BaseIndex);

      if (pIndexBuffer <> nil) then
      begin
        bActiveIB := true;
        IDirect3DIndexBuffer8(pIndexBuffer)._Release();
        pIndexBuffer := nil; // Dxbx addition - nil out after decreasing reference count
       end;
    end;

    // uiNumVertices := 0;
    // uiStartIndex := 0;

    // TODO -oCXBX: caching (if it becomes noticably slow to recreate the buffer each time)
    if(not bActiveIB) then
    begin
      IDirect3DDevice8(g_pD3DDevice8).CreateIndexBuffer(VertexCount*SizeOf(Word), D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_MANAGED, @pIndexBuffer);

      if (pIndexBuffer = nil) then
          DxbxKrnlCleanup('Could not create index buffer! (%d bytes)', [VertexCount*SizeOf(Word)]);

      pbData := nil;

      IDirect3DIndexBuffer8(pIndexBuffer).Lock(0, 0, {out}pbData, 0);

      if (pbData = nil) then
        DxbxKrnlCleanup('Could not lock index buffer!');

      memcpy(pbData, pIndexData, VertexCount * SizeOf(Word));

      IDirect3DIndexBuffer8(pIndexBuffer).Unlock();

      IDirect3DDevice8(g_pD3DDevice8).SetIndices(IDirect3DIndexBuffer8(pIndexBuffer), g_dwBaseVertexIndex);

      uiNumVertices := VertexCount;
      uiStartIndex := 0;
    end
    else
    begin
      // Dxbx note : Cxbx does this in reverse, but this is more efficient :
      uiStartIndex := DWORD(pIndexData) div SizeOf(Word);
      uiNumVertices := uiStartIndex + VertexCount; // TODO -oDxbx : Is adding uiStartIndex actually correct???
    end;

    if (IsValidCurrentShader()) and not FatalError then
    begin
      IDirect3DDevice8(g_pD3DDevice8).DrawIndexedPrimitive
      (
        EmuPrimitiveType(VPDesc.PrimitiveType), 0, uiNumVertices, uiStartIndex, VPDesc.dwPrimitiveCount
      );
      (* Cxbx has this commented out :
      if (PrimitiveType = X_D3DPT_LINELOOP) or (PrimitiveType = X_D3DPT_QUADLIST) then
      begin
        IDirect3DDevice8(g_pD3DDevice8).DrawPrimitive
        (
          EmuPrimitiveType(VPDesc.PrimitiveType), 0, VPDesc.dwPrimitiveCount
        );
      end
      else
      begin
        IDirect3DDevice8(g_pD3DDevice8).DrawIndexedPrimitive
        (
          EmuPrimitiveType(VPDesc.PrimitiveType), 0, uiNumVertices, uiStartIndex, VPDesc.dwPrimitiveCount
        );
      end; *)
    end;

    if(not bActiveIB) then
    begin
      IDirect3DDevice8(g_pD3DDevice8).SetIndices(nil, 0);
      IDirect3DIndexBuffer8(pIndexBuffer)._Release();
      pIndexBuffer := nil; // Dxbx addition - nil out after decreasing reference count
    end;

  {$ifdef _DEBUG_TRACK_VB}
  end;
  {$endif}

  VertPatch.Restore();

  EmuSwapFS(fsXbox);
  Result := D3D_OK;
end;

procedure XTL_EmuIDirect3DDevice8_DrawIndexedVerticesUP
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DrawIndexedVerticesUP' +
    #13#10'(' +
    #13#10'   PrimitiveType          : 0x%.08X' +
    #13#10'   VertexCount            : 0x%.08X' +
    #13#10'   pIndexData             : 0x%.08X' +
    #13#10'   pVertexStreamZeroData  : 0x%.08X' +
    #13#10'   VertexStreamZeroStride : 0x%.08X' +
    #13#10');',
    [Ord(PrimitiveType), VertexCount, pIndexData, pVertexStreamZeroData, VertexStreamZeroStride]);
{$ENDIF}

  // update index buffer, if necessary
  if (g_pIndexBuffer <> nil) and (g_pIndexBuffer.Emu.Lock = X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
    DxbxKrnlCleanup('g_pIndexBuffer != 0');

  XTL_EmuUpdateDeferredStates();

//  EmuUnswizzleActiveTexture(); // This messes up the loading screen background image in Rayman Arena

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
      IDirect3DDevice8(g_pD3DDevice8).DrawIndexedPrimitiveUP
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

function XTL_EmuIDirect3DDevice8_SetLight
(
  Index: DWORD;
  pLight: PD3DLIGHT8
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetLight' +
    #13#10'(' +
    #13#10'   Index               : 0x%.08X' +
    #13#10'   pLight              : 0x%.08X' +
    #13#10');',
    [Index, pLight]);
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).SetLight(Index, pLight^);
  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_SetMaterial
(
  {CONST} pMaterial: PD3DMATERIAL8
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetMaterial' +
    #13#10'(' +
    #13#10'   pMaterial           : 0x%.08X' +
    #13#10');',
    [pMaterial]);
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).SetMaterial(pMaterial^);
  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_LightEnable
(
  Index: DWORD;
  bEnable: BOOL
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_LightEnable' +
    #13#10'(' +
    #13#10'   Index               : 0x%.08X' +
    #13#10'   bEnable             : 0x%.08X' +
    #13#10');',
    [Index, bEnable]);
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).LightEnable(Index, bEnable <> BOOL_FALSE);
  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_SetRenderTarget
(
  pRenderTarget: PX_D3DSurface;
  pNewZStencil: PX_D3DSurface
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pRenderTarget_EmuSurface8: Pointer;
  pNewZStencil_EmuSurface8: Pointer;

  pPCRenderTarget: XTL_PIDirect3DSurface8;
  pPCNewZStencil: XTL_PIDirect3DSurface8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  if Assigned(pRenderTarget) then pRenderTarget_EmuSurface8 := pRenderTarget.Emu.Surface8 else pRenderTarget_EmuSurface8 := nil;
  if Assigned(pNewZStencil) then pNewZStencil_EmuSurface8 := pNewZStencil.Emu.Surface8 else pNewZStencil_EmuSurface8 := nil;
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderTarget' +
           #13#10'(' +
           #13#10'   pRenderTarget     : 0x%.08X (0x%.08X)' +
           #13#10'   pNewZStencil      : 0x%.08X (0x%.08X)' +
           #13#10');',
           [pRenderTarget, pRenderTarget_EmuSurface8,
           pNewZStencil, pNewZStencil_EmuSurface8]);
{$ENDIF}

  pPCRenderTarget := nil;
  pPCNewZStencil := nil;

  if (pRenderTarget <> nil) then
  begin
    if Assigned(pRenderTarget.Emu.Surface8) then
    begin
      EmuVerifyResourceIsRegistered(pRenderTarget);
      pPCRenderTarget := pRenderTarget.Emu.Surface8;
    end
    else
    begin
      pPCRenderTarget := g_pCachedRenderTarget.Emu.Surface8;
    end;
  end;

  if (pNewZStencil <> nil) then
  begin
    if Assigned(pNewZStencil.Emu.Surface8) then
    begin
      EmuVerifyResourceIsRegistered(pNewZStencil);
      pPCNewZStencil := pNewZStencil.Emu.Surface8;
    end
    else
    begin
      pPCNewZStencil := g_pCachedZStencilSurface.Emu.Surface8;
    end;
 end;

  // TODO -oCXBX: Follow that stencil!
  if Assigned(pRenderTarget) then pRenderTarget_EmuSurface8 := pRenderTarget.Emu.Surface8 else pRenderTarget_EmuSurface8 := nil;
  if Assigned(pNewZStencil) then pNewZStencil_EmuSurface8 := pNewZStencil.Emu.Surface8 else pNewZStencil_EmuSurface8 := nil;
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderTarget' +
      #13#10'(' +
      #13#10'   pRenderTarget       : 0x%.08X (0x%.08X)' +
      #13#10'   pNewZStencil        : 0x%.08X (0x%.08X)' +
      #13#10');',
      [ pRenderTarget, pRenderTarget_EmuSurface8,
        pNewZStencil,  pNewZStencil_EmuSurface8]);

  Result := IDirect3DDevice8(g_pD3DDevice8).SetRenderTarget(IDirect3DSurface8(pPCRenderTarget), IDirect3DSurface8(pPCNewZStencil));

  if FAILED(Result) then
    EmuWarning('SetRenderTarget failed!' +
            ' pRenderTarget = 0x%.08X, pPCRenderTarget = 0x%.08X,' +
            ' pNewZStencil = 0x%.08X, pPCNewZStencil = 0x%.08X,' +
            ' hRet = 0x%.08X',
            [pRenderTarget, pPCRenderTarget,
            pNewZStencil, pPCNewZStencil,
            Result]);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_CreatePalette
(
  Size: X_D3DPALETTESIZE;
  ppPalette: PPX_D3DPalette
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  // Dxbx note : No EmuSwapFS needed here

  ppPalette^ := XTL_EmuIDirect3DDevice8_CreatePalette2(Size);
  Result := D3D_OK;
end;


function XTL_EmuIDirect3DDevice8_CreatePalette2
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
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreatePalette2' +
         #13#10'(' +
         #13#10'   Size              : 0x%.08X' +
         #13#10');',
         [Ord(Size)]);
{$ENDIF}

  New({var PX_D3DPalette}pPalette);

  pPalette.Common := 0;
  pPalette.Emu.Lock := $8000BEEF; // emulated reference count for palettes
  pPalette.Data := DWORD(AllocMem(lk[Size] * sizeof(uint08)));

  EmuSwapFS(fsXbox);

  Result := pPalette;
end;

function XTL_EmuIDirect3DDevice8_SetPalette
(
  Stage: DWORD;
  pPalette: PX_D3DPalette
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetPalette' +
           #13#10'(' +
           #13#10'   Stage             : 0x%.08X' +
           #13#10'   pPalette          : 0x%.08X' +
           #13#10');',
           [Stage, pPalette]);
{$ENDIF}

  // Marked out by Cxbx
  //IDirect3DDevice8(g_pD3DDevice8).SetPaletteEntries(0, (PALETTEENTRY*)(*pPalette.Data);

  EmuWarning('Not setting palette');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

procedure XTL_EmuIDirect3DDevice8_SetFlickerFilter
(
  Filter: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetFlickerFilter' +
    #13#10'(' +
    #13#10'   Filter            : 0x%.08X' +
    #13#10');',
    [Filter]);
{$ENDIF}

  EmuWarning('Not setting flicker filter');
  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetSoftDisplayFilter
(
  Enable: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetSoftDisplayFilter' +
    #13#10'(' +
    #13#10'   Enable            : 0%10s' +
    #13#10');',
    [BoolToStr(Enable <> BOOL_FALSE)]);
{$ENDIF}

  EmuWarning('Not setting soft display filter');
  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DPalette8_Lock
(
  pThis: PX_D3DPalette;
  ppColors: PPD3DCOLOR;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  // Dxbx note : No EmuSwapFS needed here

  ppColors^ := XTL_EmuIDirect3DPalette8_Lock2(pThis, Flags);
  (*DbgPrintf('Pallete 0x%08X was locked: return = 0x%08x', [
        pThis,
        ppColors^
        ]);*)

  Result := D3D_OK;
end;

function XTL_EmuIDirect3DPalette8_Lock2
(
  pThis: PX_D3DPalette;
  Flags: DWORD
): PD3DCOLOR; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DPalette8_Lock' +
         #13#10'(' +
         #13#10'   pThis             : 0x%.08X' +
         #13#10'   Flags             : 0x%.08X' +
         #13#10');',
         [pThis, Flags]);
{$ENDIF}

  Result := PD3DCOLOR(pThis.Data);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_GetVertexShaderSize
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetVertexShaderSize' +
    #13#10'(' +
    #13#10'   Handle             : 0x%.08X' +
    #13#10'   pSize              : 0x%.08X' +
    #13#10');',
    [Handle_, pSize]);
{$ENDIF}

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

function XTL_EmuIDirect3DDevice8_DeleteVertexShader
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DeleteVertexShader' +
    #13#10'(' +
    #13#10'   Handle              : 0x%.08X' +
    #13#10');',
    [Handle]);
{$ENDIF}

  RealHandle := 0;

  if (VshHandleIsVertexShader(Handle)) then
  begin
    pD3DVertexShader := VshHandleGetVertexShader(Handle);
    pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);

    RealHandle := pVertexShader.Handle;
    DxbxFree(pVertexShader.pDeclaration);

    if Assigned(pVertexShader.pFunction) then
    begin
      DxbxFree(pVertexShader.pFunction);
    end;

    XTL_FreeVertexDynamicPatch(pVertexShader);

    DxbxFree(pVertexShader);
    DxbxFree(pD3DVertexShader);
  end;

  Result := IDirect3DDevice8(g_pD3DDevice8).DeleteVertexShader(RealHandle);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SelectVertexShaderDirect
(
  pVAF: PX_VERTEXATTRIBUTEFORMAT;
  Address: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SelectVertexShaderDirect' +
    #13#10'(' +
    #13#10'   pVAF              : 0x%.08X' +
    #13#10'   Address           : 0x%.08X' +
    #13#10');',
    [pVAF, Address]);

  DbgPrintf('NOT YET IMPLEMENTED!');
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_GetShaderConstantMode
(
  pMode: PDWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetShaderConstantMode' +
    #13#10'(' +
    #13#10'   pMode             : 0x%.08X' +
    #13#10');',
    [pMode]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  if Assigned(pMode) then
    pMode^ := g_VertexShaderConstantMode;
end;

function XTL_EmuIDirect3DDevice8_GetVertexShader
(
  {CONST} pHandle: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:162  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetVertexShader' +
    #13#10'(' +
    #13#10'   pHandle             : 0x%.08X' +
    #13#10');',
    [pHandle]);
{$ENDIF}

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

function XTL_EmuIDirect3DDevice8_GetVertexShaderConstant
(
  Register_: INT;
  pConstantData: Pvoid;
  ConstantCount: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetVertexShaderConstant' +
    #13#10'(' +
    #13#10'   Register          : 0x%.08X' +
    #13#10'   pConstantData     : 0x%.08X' +
    #13#10'   ConstantCount     : 0x%.08X' +
    #13#10');',
    [Register_, pConstantData, ConstantCount]);
{$ENDIF}

  // TODO -oDxbx: If we ever find a title that calls this, check if this correction
  // should indeed be done version-dependantly (like in SetVertexShaderConstant);
  // It seems logical that these two mirror eachother, but it could well be different:
  if g_BuildVersion <= 4361 then
    Inc(Register_, X_VSCM_CORRECTION{=96});

  Result := IDirect3DDevice8(g_pD3DDevice8).GetVertexShaderConstant
    (
    Register_,
    {out}pConstantData^,
    ConstantCount
    );

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_SetVertexShaderInputDirect
(
  pVAF: PX_VERTEXATTRIBUTEFORMAT;
  StreamCount: UINT; 
  pStreamInputs: PX_STREAMINPUT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SelectVertexShaderDirect' +
    #13#10'(' +
    #13#10'   pVAF              : 0x%.08X' +
    #13#10'   StreamCount       : 0x%.08X' +
    #13#10'   pStreamInputs     : 0x%.08X' +
    #13#10');',
    [pVAF, StreamCount, pStreamInputs]);

  DbgPrintf('NOT YET IMPLEMENTED!');
{$ENDIF}

  EmuSwapFS(fsXbox);
  Result := D3D_OK;
end;

function XTL_EmuIDirect3DDevice8_GetVertexShaderInput
(
  pHandle: PDWORD;
  pStreamCount: PUINT;
  pStreamInputs: PX_STREAMINPUT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetVertexShaderInput' +
    #13#10'(' +
    #13#10'   pHandle           : 0x%.08X' +
    #13#10'   pStreamCount      : 0x%.08X' +
    #13#10'   pStreamInputs     : 0x%.08X' +
    #13#10');',
    [pHandle, pStreamCount, pStreamInputs]);

  DbgPrintf('NOT YET IMPLEMENTED!');
{$ENDIF}

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuIDirect3DDevice8_SetVertexShaderInput
(
  aHandle: DWORD;
  StreamCount: UINT;
  pStreamInputs: PX_STREAMINPUT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexShaderInput' +
    #13#10'(' +
    #13#10'   Handle            : 0x%.08X' +
    #13#10'   StreamCount       : 0x%.08X' +
    #13#10'   pStreamInputs     : 0x%.08X' +
    #13#10');',
    [aHandle, StreamCount, pStreamInputs]);

  DbgPrintf('NOT YET IMPLEMENTED!');
{$ENDIF}

  EmuSwapFS(fsXbox);
  Result := D3D_OK;
end;

procedure XTL_EmuIDirect3DDevice8_RunVertexStateShader
(
  Address: DWORD;
  {CONST} pData: PFLOAT
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_RunVertexStateShader' +
    #13#10'(' +
    #13#10'   Address            : 0x%.08X' +
    #13#10'   pData              : 0x%.08X' +
    #13#10');',
    [Address, pData]);

  DbgPrintf('NOT YET IMPLEMENTED!');
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_LoadVertexShaderProgram
(
  {CONST} pFunction: PDWORD;
  Address: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_LoadVertexShaderProgram' +
    #13#10'(' +
    #13#10'   pFunction         : 0x%.08X' +
    #13#10'   Address           : 0x%.08X' +
    #13#10');',
    [pFunction, Address]);

  DbgPrintf('NOT YET IMPLEMENTED!');
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_GetVertexShaderType
(
  aHandle: DWORD;
  pType: PDWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetVertexShaderType' +
    #13#10'(' +
    #13#10'   Handle             : 0x%.08X' +
    #13#10'   pType              : 0x%.08X' +
    #13#10');',
    [aHandle, pType]);
{$ENDIF}

  if Assigned(pType) and VshHandleIsVertexShader(aHandle) then
  begin
    pType^ := PVERTEX_SHADER(VshHandleGetVertexShader(aHandle).Handle).Type_;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_GetVertexShaderDeclaration
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

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetVertexShaderDeclaration' +
    #13#10'(' +
    #13#10'   Handle             : 0x%.08X' +
    #13#10'   pData              : 0x%.08X' +
    #13#10'   pSizeOfData        : 0x%.08X' +
    #13#10');',
    [Handle, pData, pSizeOfData]);
{$ENDIF}

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

function XTL_EmuIDirect3DDevice8_GetVertexShaderFunction
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

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetVertexShaderFunction' +
    #13#10'(' +
    #13#10'   Handle             : 0x%.08X' +
    #13#10'   pData              : 0x%.08X' +
    #13#10'   pSizeOfData        : 0x%.08X' +
    #13#10');',
    [aHandle, pData, pSizeOfData]);
{$ENDIF}

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

function XTL_EmuIDirect3D8_AllocContiguousMemory
(
  dwSize: SIZE_T;
  dwAllocAttributes: DWORD
): PVOID; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Patrickvl  Done:100
var
  dwRet: DWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf( 'EmuD3D8 : EmuIDirect3D8_AllocContiguousMemory' +
             #13#10'(' +
             #13#10'   dwSize             : 0x%.08X' +
             #13#10'   dwAllocAttributes  : 0x%.08X' +
             #13#10');',
             [dwSize, dwAllocAttributes]);
{$ENDIF}

  //
  // Cxbx NOTE: Kludgey (but necessary) solution:
  //
  // Since this memory must be aligned on a page boundary, we must allocate an extra page
  // so that we can return a valid page aligned pointer
  //

  Result := DxbxMalloc(dwSize + $1000);

  // align to page boundary
  begin
    dwRet := DWORD(Result);

    Inc(dwRet, $1000 - dwRet mod $1000);

    g_AlignCache.insert(dwRet, Result);

    Result := PVOID(dwRet);
  end;

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_AllocContiguousMemory returned 0x%.08X', [Result]);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DTexture8_GetLevelDesc
(
  Level: UINT;
  pDesc: PX_D3DSURFACE_DESC
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DTexture8_GetLevelDesc' +
    #13#10'(' +
    #13#10'   Level               : 0x%.08X' +
    #13#10'   pDesc               : 0x%.08X' +
    #13#10');',
    [Level, pDesc]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuIDirect3D8_CheckDeviceMultiSampleType
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_CheckDeviceMultiSampleType' +
    #13#10'(' +
    #13#10'   Adapter             : 0x%.08X' +
    #13#10'   DeviceType          : 0x%.08X' +
    #13#10'   SurfaceFormat       : 0x%.08X' +
    #13#10'   Windowed            : 0x%.08X' +
    #13#10'   MultiSampleType     : 0x%.08X' +
    #13#10');',
    [Adapter, Ord(DeviceType), Ord(SurfaceFormat), Windowed, Ord(MultiSampleType)]);
{$ENDIF}

  if (Adapter <> D3DADAPTER_DEFAULT) then
  begin
    EmuWarning('Adapter is not D3DADAPTER_DEFAULT, correcting!');
    Adapter := D3DADAPTER_DEFAULT;
  end;

  if Ord(DeviceType) > Ord(High(D3DDEVTYPE)) then
    EmuWarning('DeviceType := D3DDEVTYPE_FORCE_DWORD');

  // Convert SurfaceFormat (Xbox->PC)
  PCSurfaceFormat := EmuXB2PC_D3DFormat(SurfaceFormat);

  // TODO -oCXBX: HACK: Devices that don't support this should somehow emulate it!
  if (PCSurfaceFormat = D3DFMT_D16) then
  begin
    EmuWarning('D3DFMT_D16 is an unsupported texture format!');
    PCSurfaceFormat := D3DFMT_X8R8G8B8;
  end
  else if (PCSurfaceFormat = D3DFMT_P8) then
  begin
    EmuWarning('D3DFMT_P8 is an unsupported texture format!');
    PCSurfaceFormat := D3DFMT_X8R8G8B8;
  end
  else if (PCSurfaceFormat = D3DFMT_D24S8) then
  begin
    EmuWarning('D3DFMT_D24S8 is an unsupported texture format!');
    PCSurfaceFormat := D3DFMT_X8R8G8B8;
  end;

  if (Windowed <> BOOL_FALSE) then
    Windowed := BOOL_FALSE;

  // TODO -oCXBX: Convert from Xbox to PC!!
  PCMultiSampleType := EmuXB2PC_D3DMultiSampleFormat(MultiSampleType);

 // Now call the real CheckDeviceMultiSampleType with the corrected parameters.
 Result := IDirect3D8(g_pD3D8).CheckDeviceMultiSampleType
    (
    Adapter,
    DeviceType,
    PCSurfaceFormat,
    Windowed <> BOOL_FALSE,
    PCMultiSampleType
    );

  EmuSwapFS(fsXbox);
end; // XTL_EmuIDirect3D8_CheckDeviceMultiSampleType

function XTL_EmuIDirect3D8_GetDeviceCaps
(
  Adapter: UINT;
  DeviceType: D3DDEVTYPE;
  pCaps: PD3DCAPS8
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_GetDeviceCaps' +
    #13#10'(' +
    #13#10'   Adapter                 : 0x%.08X' +
    #13#10'   DeviceType              : 0x%.08X' +
    #13#10'   pCaps                   : 0x%.08X' +
    #13#10');',
    [Adapter, Ord(DeviceType), pCaps]);
{$ENDIF}

  Result := IDirect3D8(g_pD3D8).GetDeviceCaps(Adapter, DeviceType, {out}pCaps^);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3D8_SetPushBufferSize
(
  PushBufferSize: DWORD;
  KickOffSize: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DXBX_DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_SetPushBufferSize' +
    #13#10'(' +
    #13#10'   PushBufferSize          : 0x%.08X' +
    #13#10'   KickOffSize             : 0x%.08X' +
    #13#10');',
    [PushBufferSize, KickOffSize]);
{$ENDIF}

  Result := D3D_OK; // This is a Xbox extension, meaning there is no pc counterpart.
  
  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_InsertFence(): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_InsertFence();');
{$ENDIF}

  // TODO -oCXBX: Actually implement this

  Result := $8000BEEF;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_IsFencePending
(
  Fence: DWORD
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_IsFencePending' +
    #13#10'(' +
    #13#10'   Fence              : 0x%.08X' +
    #13#10');',
    [Fence]);
{$ENDIF}

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);

  Result := BOOL_FALSE;
end;

procedure XTL_EmuIDirect3DDevice8_BlockOnFence
(
  Fence: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_BlockOnFence' +
    #13#10'(' +
    #13#10'   Fence                   : 0x%.08X' +
    #13#10');',
    [Fence]);
{$ENDIF}

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DResource8_BlockUntilNotBusy
(
  pThis: PX_D3DResource
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DResource8_BlockUntilNotBusy' +
    #13#10'(' +
    #13#10'   pThis                   : 0x%.08X' +
    #13#10');',
    [pThis]);
{$ENDIF}

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DResource8_SetPrivateData
(
  pThis: PX_D3DResource;
  refguid: REFGUID;
  pData: Pvoid;
  SizeOfData: DWORD;
  Flags: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XTL_EmuIDirect3DResource8_SetPrivateData');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirect3DResource8_GetPrivateData
(
  pThis: PX_D3DResource;
  refguid: REFGUID;
  pData: Pvoid;
  SizeOfData: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XTL_EmuIDirect3DResource8_GetPrivateData');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

procedure XTL_EmuIDirect3DResource8_FreePrivateData
(
  pThis: PX_D3DResource;
  refguid: REFGUID
); stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XTL_EmuIDirect3DResource8_FreePrivateData');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DVertexBuffer8_GetDesc
(
  pThis: PX_D3DVertexBuffer;
  pDesc: PD3DVERTEXBUFFER_DESC
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DVertexBuffer8_GetDesc' +
    #13#10'(' +
    #13#10'   pThis                   : 0x%.08X' +
    #13#10'   pDesc                   : 0x%.08X' +
    #13#10');',
    [pThis, pDesc]);
{$ENDIF}

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_SetScissors
(
  Count: DWORD;
  Exclusive: BOOL;
  {CONST} pRects: PD3DRECT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetScissors' +
    #13#10'(' +
    #13#10'   Count                   : 0x%.08X' +
    #13#10'   Exclusive               : 0x%.08X' +
    #13#10'   pRects                  : 0x%.08X' +
    #13#10');',
    [Count, Exclusive, pRects]);
{$ENDIF}

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

function XTL_EmuIDirect3DDevice8_SetScreenSpaceOffset
(
  x: FLOAT;
  y: FLOAT
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetScreenSpaceOffset' +
    #13#10'(' +
    #13#10'   x                  : %f' +
    #13#10'   y                  : %f' +
    #13#10');',
    [x, y]);
{$ENDIF}

  EmuWarning('EmuIDirect3DDevice8_SetScreenSpaceOffset ignored');

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;


function XTL_EmuIDirect3DDevice8_SetPixelShaderProgram
(
  {CONST} pPSDef: PX_D3DPIXELSHADERDEF
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwHandle: DWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetPixelShaderProgram' +
     #13#10'(' +
     #13#10'  pPSDef            : 0x%.08X' +
     #13#10');',
     [pPSDef]);
{$ENDIF}

  Result := E_FAIL;
  dwHandle := 0;

  // Redirect this call to windows Direct3D
  {hRet := IDirect3DDevice8(g_pD3DDevice8).CreatePixelShader(
        PDWORD(pPSDef),
        pHandle
    );}

  if (FAILED(Result)) then
  begin
    dwHandle := X_PIXELSHADER_FAKE_HANDLE;

    EmuWarning('We''re lying about the creation of a pixel shader!');
  end;

  // Now, redirect this to Xbox Direct3D
  EmuSwapFS(fsXbox);

  {hRet := }XTL_EmuIDirect3DDevice8_SetPixelShader(dwHandle);

  Result := S_OK;
end;

function XTL_EmuIDirect3DDevice8_CreateStateBlock
(
  Type_: D3DSTATEBLOCKTYPE;
  pToken: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreateStateBlock' +
    #13#10'(' +
    #13#10'   Type                      : %f' +
    #13#10'   pToken                    : %f' +
    #13#10');',
    [Ord(Type_), pToken]);
{$ENDIF}

  // blueshogun96 10/1/07
  // I'm assuming this is the same as the PC version...

  Result := IDirect3DDevice8(g_pD3DDevice8).CreateStateBlock(Type_, {out}pToken^);

  if (FAILED(Result)) then
    EmuWarning('CreateStateBlock failed!');

  EmuSwapFS(fsXbox);
end;


procedure XTL_EmuIDirect3DDevice8_InsertCallback
(
  Type_: X_D3DCALLBACKTYPE;
  pCallback: X_D3DCALLBACK;
  Context: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_InsertCallback' +
     #13#10'(' +
     #13#10'   Type                      : 0x%.08X' +
     #13#10'   pCallback                 : 0x%.08X' +
     #13#10'   Context                   : 0x%.08X' +
     #13#10');',
     [Ord(Type_), pCallback, Context]);
{$ENDIF}

  EmuWarning('InsertCallback ignored!');

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);
end;


function XTL_EmuIDirect3DDevice8_DrawRectPatch
(
  Handle: UINT;
  {CONST} pNumSegs: PFLOAT;
  {CONST} pRectPatchInfo: PD3DRECTPATCH_INFO
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DrawRectPatch' +
     #13#10'(' +
     #13#10'   Handle                    : 0x%.08X' +
     #13#10'   pNumSegs                  : %f' +
     #13#10'   pRectPatchInfo            : 0x%.08X' +
     #13#10');',
     [Handle, pNumSegs^, pRectPatchInfo]);
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).DrawRectPatch(Handle, PSingle(pNumSegs), pRectPatchInfo);

  if (FAILED(Result)) then
    EmuWarning('DrawRectPatch failed!');

  EmuSwapFS(fsXbox);
end;

//#pragma warning(disable:4244)
function XTL_EmuIDirect3DDevice8_GetProjectionViewportMatrix
(
  pProjectionViewport: PD3DXMATRIX
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  hRet: HRESULT;
  Out: D3DMATRIX;
  mtxProjection: D3DMATRIX;
  mtxViewport: D3DMATRIX;
  Viewport: D3DVIEWPORT8;
  ClipWidth, ClipHeight, ClipX, ClipY, Width, Height: Float;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetProjectionViewportMatrix' +
     #13#10'(' +
     #13#10'   pProjectionViewport       : 0x%.08X' +
     #13#10');',
     [pProjectionViewport]);
{$ENDIF}

  // blueshogun96 1/25/10
  // It's been almost 3 years, but I think this is a better
  // implementation.  Still probably not right, but better
  // then before.


  // Get current viewport
  hRet := IDirect3DDevice8(g_pD3DDevice8).GetViewport({out}Viewport);
  if (FAILED(hRet)) then
    EmuWarning('Unable to get viewport!');

  // Get current projection matrix
  hRet := IDirect3DDevice8(g_pD3DDevice8).GetTransform(D3DTS_PROJECTION, {out}mtxProjection);
  if (FAILED(hRet)) then
    EmuWarning('Unable to get projection matrix!');

  // Clear the destination matrix
  ZeroMemory(@Out, sizeof(D3DMATRIX));

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
  Out := D3DMATRIX_MULTIPLY(mtxProjection, mtxViewport);

  if Assigned(pProjectionViewport) then
    pProjectionViewport^ := Out;

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;
//#pragma warning(default:4244)

function XTL_EmuIDirect3DDevice8_BackFillMode
(
  Value: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_BackFillMode' +
     #13#10'(' +
     #13#10'   Value       : 0x%.08X' +
     #13#10');',
     [Value]);
{$ENDIF}

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

  Result := S_OK;
end;


// * func: EmuD3DDevice_KickOff (D3D::CDevice::KickOff)
procedure XTL_EmuIDevice3D8_KickOff(); stdcall;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuD3DDevice_KickOff();');
{$ENDIF}

  // TODO -oCXBX: Anything (kick off and NOT wait for idle)?
  // NOTE: We should actually emulate IDirect3DDevice8_KickPushBuffer()
  // instead of this function.  When needed, use the breakpoint (int 3)
  // to determine what is calling this function if it's something other
  // than IDirect3DDevice8_KickPushBuffer() itself.

//  asm int 3 end;

  EmuSwapFS(fsXbox);
end;


function XTL_EmuIDirect3DDevice8_GetTexture2
(
  pTexture: PX_D3DResource
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetTexture2' +
    #13#10'(' +
    #13#10'   pTexture            : 0x%.08X' +
    #13#10');',
    [pTexture]);
{$ENDIF}

  Result := S_OK;

  // TODO -oCXBX: I'm sure there is a better way to handle this.

  (* // Commented out by CXBX
  if (nil=pTexture) then
  begin
{$IFDEF DEBUG}
    DbgPrintf( 'Creating new texture...');
{$ENDIF}
    New({X_D3DResource}pTexture);
  end
  else
  begin
{$IFDEF DEBUG}
    DbgPrintf( 'pTexture: = 0x%.08X'#13#10'pTexture.EmuTexture8', [pTexture, pTexture.Emu.BaseTexture8]);
{$ENDIF}
  end; *)

  // Since this function does not specify any texture stages,
  // I guess we can assume it's just the first one.  According
  // to the XDK documentation, this function should also add
  // to the reference count.
  // TODO -oCXBX: Verify texture?

//  printf( 'Setting texture...' + );
//  pTexture.Emu.BaseTexture8 = g_EmuD3DActiveTexture[0].Emu.BaseTexture8;
  pTexture := g_EmuD3DActiveTexture[0];
  pTexture.Data := (X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_TEXCLON);

//  printf( 'Adding reference...' + );
//  IDirect3DBaseTexture8(pTexture.Emu.BaseTexture8)._AddRef();

//  EmuSwapFS(fsXbox);
//  EmuIDirect3DResource8_AddRef(pTexture);
//  EmuSwapFS(fsWindows);

//  Result := IDirect3DDevice8(g_pD3DDevice8).GetTexture(0, @(ppTexture^.Emu.BaseTexture8));

//  printf( 'Verifying resource...' + );
//  EmuVerifyResourceIsRegistered(ppTexture^);

  EmuSwapFS(fsXbox);
end;


// * func: EmuD3DDevice_SetStateVB (D3D::CDevice::SetStateVB)
procedure XTL_EmuIDirect3DDevice8_SetStateVB(Unknown1: ULONG); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuD3DDevice_SetStateVB' +
    #13#10'(' +
    #13#10'   Unknown1          : 0x%.08X' +
    #13#10')',
    [Unknown1]);
{$ENDIF}

  // TODO -oCXBX: Anything?

  EmuSwapFS(fsXbox);
end;

// * func: EmuD3DDevice_SetStateUP (D3D::CDevice::SetStateUP)
procedure XTL_EmuIDirect3DDevice8_SetStateUP(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuD3DDevice_SetStateUP();');
{$ENDIF}

  // TODO -oCXBX: Anything?

  EmuSwapFS(fsXbox);
end;


procedure XTL_EmuIDirect3DDevice8_SetStipple(
  pPattern: PDWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetStipple' +
      #13#10'(' +
      #13#10'   pPattern          : 0x%.08X' +
      #13#10')',
      [pPattern]);
{$ENDIF}

  // We need an OpenGL port... badly

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetSwapCallback
(
  pCallback: D3DSWAPCALLBACK
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetSwapCallback' +
      #13#10'(' +
      #13#10'   pCallback           : 0x%.08X' +
      #13#10');',
      [PPointer(@pCallback)^]);

  DbgPrintf('pCallback: = 0x%.08X', [PPointer(@pCallback)^]);
{$ENDIF}

  g_pSwapCallback := pCallback;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_PersistDisplay(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_PersistDisplay();');
{$ENDIF}

  Result := S_OK;

  // TODO -oCXBX: If this functionality is ever really needed, an idea for
  // implementation would be to save a copy of the backbuffer's contents
  // and free the memory after the next call to D3DDevice::Present().
  // This temporary data could also be made available to the Xbox game
  // through AvGetSavedDataAddress() since D3DDevice::GetPersistedDisplay2
  // just contains a call to that kernel function.  So far, Unreal Champ-
  // ionship is the only game that uses this functionality that I know of.
  // Other Unreal Engine 2.x games might as well.

  EmuWarning('(Temporarily) Not persisting display. Blueshogun can fix this.');

  if not Assigned(g_pD3DDevice8) then
  begin
    EmuWarning('Direct3D device not initialized!');
    Result := E_FAIL;
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_Unknown1(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_Unknown1();');
{$ENDIF}

  // TODO -oCXBX: Find out what this actually is.
  // This function was only found in Run Like Hell (5233) @ 0x11FCD0.
  // So far, this function hasn't been found in any other XDKs.  Since
  // the only major thing going on inside of it is a call to the kernel
  // function AvSendTVEncoderOption, we can probably ignore it.

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_PrimeVertexCache
(
  VertexCount: UINT;
  pIndexData: PWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_PrimeVertexCache' +
      #13#10'(' +
      #13#10'   VertexCount           : 0x%.08X' +
      #13#10'   pIndexData            : 0x%.08X' +
      #13#10');',
      [VertexCount, pIndexData]);
{$ENDIF}

  // TODO -oCXBX: Implement
  EmuWarning('PrimeVertexCache is not supported!');

  EmuSwapFS(fsWindows);

  Result := S_OK;
end;

function XTL_EmuIDirect3DDevice8_SetRenderState_SampleAlpha
(
  dwSampleAlpha: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_SampleAlpha' +
      #13#10'(' +
      #13#10'   dwSampleAlpha         : 0x%.08X' +
      #13#10');',
      [dwSampleAlpha]);
{$ENDIF}

  // TODO -oCXBX: Implement?

  EmuWarning('SetRenderState_SampleAlpha not supported!');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

(* Dxbx note : Disabled, as we DO have EmuD3DDeferredRenderState pin-pointed correctly
procedure XTL_EmuIDirect3DDevice8_SetRenderState_Deferred(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Value: DWORD;
  {1 ECX}State: DWORD
  ); register; // fastcall simulation - See Translation guide
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_Deferred' +
      #13#10'(' +
      #13#10'   State               : 0x%.08X' +
      #13#10'   Value               : 0x%.08X' +
      #13#10');',
      [State, Value]);
{$ENDIF}

  // TODO -oCXBX: HACK: Technically, this function doesn't need to be emulated.
  // The location of EmuD3DDeferredRenderState for 3911 isn't correct and at
  // the time of writing, I don't understand how to fix it.  Until then,
  // I'm going to implement this in a reckless manner.  When the offset for
  // EmuD3DDeferredRenderState is fixed for 3911, this function should be
  // obsolete!

  if Assigned(XTL_EmuD3DDeferredRenderState) then // Dxbx addition
  begin
    if (State > 81) and (State < 116) then
      XTL_EmuD3DDeferredRenderState[State-82] := Value
    else
      DxbxKrnlCleanup('Unknown Deferred RenderState! (%d)', [State]);
  end;

  {
  XDK 3911 Deferred RenderState values
    D3DRS_FOGENABLE                 = 82,   // TRUE to enable fog blending
    D3DRS_FOGTABLEMODE              = 83,   // D3DFOGMODE
    D3DRS_FOGSTART                  = 84,   // float fog start (for both vertex and pixel fog)
    D3DRS_FOGEND                    = 85,   // float fog end
    D3DRS_FOGDENSITY                = 86,   // float fog density
    D3DRS_RANGEFOGENABLE            = 87,   // TRUE to enable range-based fog
    D3DRS_WRAP0                     = 88,   // D3DWRAP* flags (D3DWRAP_U, D3DWRAPCOORD_0, etc.) for 1st texture coord.
    D3DRS_WRAP1                     = 89,   // D3DWRAP* flags (D3DWRAP_U, D3DWRAPCOORD_0, etc.) for 2nd texture coord.
    D3DRS_WRAP2                     = 90,   // D3DWRAP* flags (D3DWRAP_U, D3DWRAPCOORD_0, etc.) for 3rd texture coord.
    D3DRS_WRAP3                     = 91,   // D3DWRAP* flags (D3DWRAP_U, D3DWRAPCOORD_0, etc.) for 4th texture coord.
    D3DRS_LIGHTING                  = 92,   // TRUE to enable lighting
    D3DRS_SPECULARENABLE            = 93,   // TRUE to enable specular
    D3DRS_LOCALVIEWER               = 94,   // TRUE to enable camera-relative specular highlights
    D3DRS_COLORVERTEX               = 95,   // TRUE to enable per-vertex color
    D3DRS_BACKSPECULARMATERIALSOURCE= 96,   // D3DMATERIALCOLORSOURCE (Xbox extension)
    D3DRS_BACKDIFFUSEMATERIALSOURCE = 97,   // D3DMATERIALCOLORSOURCE (Xbox extension)
    D3DRS_BACKAMBIENTMATERIALSOURCE = 98,   // D3DMATERIALCOLORSOURCE (Xbox extension)
    D3DRS_BACKEMISSIVEMATERIALSOURCE= 99,   // D3DMATERIALCOLORSOURCE (Xbox extension)
    D3DRS_SPECULARMATERIALSOURCE    = 100,  // D3DMATERIALCOLORSOURCE
    D3DRS_DIFFUSEMATERIALSOURCE     = 101,  // D3DMATERIALCOLORSOURCE
    D3DRS_AMBIENTMATERIALSOURCE     = 102,  // D3DMATERIALCOLORSOURCE
    D3DRS_EMISSIVEMATERIALSOURCE    = 103,  // D3DMATERIALCOLORSOURCE
    D3DRS_BACKAMBIENT               = 104,  // D3DCOLOR (Xbox extension)
    D3DRS_AMBIENT                   = 105,  // D3DCOLOR
    D3DRS_POINTSIZE                 = 106,  // float point size
    D3DRS_POINTSIZE_MIN             = 107,  // float point size min threshold
    D3DRS_POINTSPRITEENABLE         = 108,  // TRUE to enable point sprites
    D3DRS_POINTSCALEENABLE          = 109,  // TRUE to enable point size scaling
    D3DRS_POINTSCALE_A              = 110,  // float point attenuation A value
    D3DRS_POINTSCALE_B              = 111,  // float point attenuation B value
    D3DRS_POINTSCALE_C              = 112,  // float point attenuation C value
    D3DRS_POINTSIZE_MAX             = 113,  // float point size max threshold
    D3DRS_PATCHEDGESTYLE            = 114,  // D3DPATCHEDGESTYLE
    D3DRS_PATCHSEGMENTS             = 115,  // DWORD number of segments per edge when drawing patches
  }

  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;
*)

function XTL_EmuIDirect3DDevice8_DeleteStateBlock
(
  Token: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DeleteStateBlock' +
      #13#10'(' +
      #13#10'   Token               : 0x%.08X' +
      #13#10');',
      [Token]);
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).DeleteStateBlock(Token);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_SetModelView
(
  {CONST} pModelView: PD3DMATRIX;
  {CONST} pInverseModelView: PD3DMATRIX;
  {CONST} pComposite: PD3DMATRIX
): HRESULT; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetModelView' +
      #13#10'(' +
      #13#10'   pModelView           : 0x%.08X' +
      #13#10'   pInverseModelView    : 0x%.08X' +
      #13#10'   pComposite           : 0x%.08X' +
      #13#10');',
      [pModelView, pInverseModelView, pComposite]);
{$ENDIF}

  // TODO -oCxbx: Implement
  DxbxKrnlCleanup('SetModelView not yet implemented (should be easy fix, tell blueshogun)');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirect3DDevice8_SetVertexBlendModelView
(
  Count: UINT;
  pModelViews: PD3DMATRIX;
  pInverseModelViews: PD3DMATRIX;
  pProjectionViewport: PD3DMATRIX
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XTL_EmuIDirect3DDevice8_SetVertexBlendModelView');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;


procedure XTL_EmuIDirect3DDevice8_FlushVertexCache(); stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_FlushVertexCache();');
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_BeginPushBuffer
(
   pPushBuffer: PX_D3DPushBuffer
): HRESULT; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_BeginPushBuffer' +
      #13#10'(' +
      #13#10'   pPushBuffer          : 0x%.08X' +
      #13#10');', [pPushBuffer]);
{$ENDIF}

  //DxbxKrnlCleanup('BeginPushBuffer is not yet implemented!');
  EmuWarning('BeginPushBuffer is not yet implemented!');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirect3DDevice8_EndPushBuffer(): HRESULT; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_EndPushBuffer();');
{$ENDIF}

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

procedure XTL_EmuXMETAL_StartPush(Unknown: Pvoid); stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuXMETAL_StartPush' +
      #13#10'(' +
      #13#10'   Unknown           : 0x%.08X' +
      #13#10');', [Unknown]);
{$ENDIF}

  // This function is too low level to actually emulate
  // Only use for debugging.
  asm int 3 end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_GetModelView(pModelView: PD3DXMATRIX): HRESULT; stdcall;
// Branch:shogun  Revision:161  Translator:PatrickvL  Done:100
var
  mtxWorld, mtxView: TD3DXMATRIX;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetModelView' +
      #13#10'(' +
      #13#10'   pModelView        : 0x%.08X' +
      #13#10');', [pModelView]);
{$ENDIF}

  // I hope this is right
  IDirect3DDevice8(g_pD3DDevice8).GetTransform(D3DTS_WORLD, {out}mtxWorld);
  IDirect3DDevice8(g_pD3DDevice8).GetTransform(D3DTS_VIEW, {out}mtxView);

//  pModelView^ := mtxWorld * mtxView;
  D3DXMatrixMultiply({out}pModelView^, mtxWorld, mtxView);

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirect3DDevice8_SetBackMaterial(pMaterial: PD3DMATERIAL8): HRESULT; stdcall;
// Branch:shogun  Revision:161  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetBackMaterial' +
      #13#10'(' +
      #13#10'   pMaterial         : 0x%.08X' +
      #13#10');', [pMaterial]);
{$ENDIF}

  EmuWarning('SetBackMaterial is not supported!');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

exports
  XTL_EmuGet2DSurfaceDesc,
  XTL_EmuGet2DSurfaceDescD, // TODO -oDXBX: Fix wrong prefix!

  XTL_EmuIDevice3D8_KickOff name PatchPrefix + '?KickOff@CDevice@D3D@@QAEXXZ',

//  XTL_EmuIDirect3D8_AllocContiguousMemory name PatchPrefix + 'D3D_AllocContiguousMemory@8', // better for dxbx logging
  XTL_EmuIDirect3D8_CheckDeviceFormat name PatchPrefix + 'Direct3D_CheckDeviceFormat',
  XTL_EmuIDirect3D8_CheckDeviceMultiSampleType name PatchPrefix + 'Direct3D_CheckDeviceMultiSampleType',
  XTL_EmuIDirect3D8_CreateDevice name PatchPrefix + 'Direct3D_CreateDevice',
  XTL_EmuIDirect3D8_EnumAdapterModes name PatchPrefix + 'Direct3D_EnumAdapterModes',
  XTL_EmuIDirect3D8_GetAdapterDisplayMode name PatchPrefix + 'Direct3D_GetAdapterDisplayMode',
  XTL_EmuIDirect3D8_GetAdapterModeCount name PatchPrefix + 'Direct3D_GetAdapterModeCount',
  XTL_EmuIDirect3D8_GetDeviceCaps name PatchPrefix + 'Direct3D_GetDeviceCaps',
  XTL_EmuIDirect3D8_KickOffAndWaitForIdle name PatchPrefix + 'KickOffAndWaitForIdle',
  XTL_EmuIDirect3D8_SetPushBufferSize name PatchPrefix + 'Direct3D_SetPushBufferSize',

  XTL_EmuIDirect3DBaseTexture8_GetLevelCount name PatchPrefix + 'D3DBaseTexture_GetLevelCount@4',

  XTL_EmuIDirect3DCubeTexture8_LockRect  name PatchPrefix + 'D3DCubeTexture8_LockRect', // _D3DCubeTexture_LockRect@24

  XTL_EmuIDirect3DDevice8_AddRef name PatchPrefix + 'D3DDevice_AddRef@0',
  XTL_EmuIDirect3DDevice8_ApplyStateBlock name PatchPrefix + 'D3DDevice_ApplyStateBlock',
  XTL_EmuIDirect3DDevice8_BackFillMode name PatchPrefix + 'D3DDevice_BackFillMode',
  XTL_EmuIDirect3DDevice8_Begin name PatchPrefix + 'D3DDevice_Begin',
  XTL_EmuIDirect3DDevice8_BeginPush name PatchPrefix + 'D3DDevice_BeginPushBuffer@4', // ??
  XTL_EmuIDirect3DDevice8_BeginPushBuffer name PatchPrefix + 'D3DDevice_BeginPushBuffer', // ??
//  XTL_EmuIDirect3DDevice8_BeginStateBig name PatchPrefix + 'D3DDevice_BeginStateBig', // MARKED OUT BY CXBX
  XTL_EmuIDirect3DDevice8_BeginStateBlock name PatchPrefix + 'D3DDevice_BeginStateBlock@0',
  XTL_EmuIDirect3DDevice8_BeginVisibilityTest name PatchPrefix + 'D3DDevice_BeginVisibilityTest@0', // [PvL] reviewed up to here
  XTL_EmuIDirect3DDevice8_BlockOnFence name PatchPrefix + 'D3DDevice_BlockOnFence',
  XTL_EmuIDirect3DDevice8_BlockUntilVerticalBlank name PatchPrefix + 'D3DDevice_BlockUntilVerticalBlank@0',
  XTL_EmuIDirect3DDevice8_CaptureStateBlock name PatchPrefix + 'D3DDevice_CaptureStateBlock',
  XTL_EmuIDirect3DDevice8_Clear name PatchPrefix + 'D3DDevice_Clear',
  XTL_EmuIDirect3DDevice8_CopyRects name PatchPrefix + 'D3DDevice_CopyRects',
  XTL_EmuIDirect3DDevice8_CreateCubeTexture name PatchPrefix + 'D3DDevice_CreateCubeTexture',
  XTL_EmuIDirect3DDevice8_CreateImageSurface name PatchPrefix + 'D3DDevice_CreateImageSurface',
  XTL_EmuIDirect3DDevice8_CreateIndexBuffer name PatchPrefix + 'D3DDevice_CreateIndexBuffer',
  XTL_EmuIDirect3DDevice8_CreateIndexBuffer2 name PatchPrefix + 'D3DDevice_CreateIndexBuffer2',
  XTL_EmuIDirect3DDevice8_CreatePalette name PatchPrefix + 'D3DDevice_CreatePalette',
  XTL_EmuIDirect3DDevice8_CreatePalette2 name PatchPrefix + 'D3DDevice_CreatePalette2',
  XTL_EmuIDirect3DDevice8_CreatePixelShader name PatchPrefix + 'D3DDevice_CreatePixelShader@8',
  XTL_EmuIDirect3DDevice8_CreateStateBlock name PatchPrefix + 'D3DDevice_CreateStateBlock',
  XTL_EmuIDirect3DDevice8_CreateTexture name PatchPrefix + 'D3DDevice_CreateTexture',
  XTL_EmuIDirect3DDevice8_CreateTexture2 name PatchPrefix + 'D3DDevice_CreateTexture2',
  XTL_EmuIDirect3DDevice8_CreateVertexBuffer name PatchPrefix + 'D3DDevice_CreateVertexBuffer',
  XTL_EmuIDirect3DDevice8_CreateVertexBuffer2 name PatchPrefix + 'D3DDevice_CreateVertexBuffer2',
  XTL_EmuIDirect3DDevice8_CreateVertexShader name PatchPrefix + 'D3DDevice_CreateVertexShader@16',
  XTL_EmuIDirect3DDevice8_CreateVolumeTexture name PatchPrefix + 'D3DDevice_CreateVolumeTexture',
  XTL_EmuIDirect3DDevice8_DeletePixelShader name PatchPrefix + 'D3DDevice_DeletePixelShader@4',
  XTL_EmuIDirect3DDevice8_DeleteStateBlock name PatchPrefix + 'D3DDevice_DeleteStateBlock', // TODO -oDXBX: Check name
  XTL_EmuIDirect3DDevice8_DeleteVertexShader name PatchPrefix + 'D3DDevice_DeleteVertexShader@4',
  XTL_EmuIDirect3DDevice8_DrawIndexedVertices name PatchPrefix + 'D3DDevice_DrawIndexedVertices@12',
  XTL_EmuIDirect3DDevice8_DrawIndexedVerticesUP name PatchPrefix + 'D3DDevice_DrawIndexedVerticesUP',
  XTL_EmuIDirect3DDevice8_DrawRectPatch name PatchPrefix + 'D3DDevice_DrawRectPatch',
  XTL_EmuIDirect3DDevice8_DrawVertices name PatchPrefix + 'D3DDevice_DrawVertices@12',
  XTL_EmuIDirect3DDevice8_DrawVerticesUP name PatchPrefix + 'D3DDevice_DrawVerticesUP@16',
  XTL_EmuIDirect3DDevice8_EnableOverlay name PatchPrefix + 'D3DDevice_EnableOverlay@4',
  XTL_EmuIDirect3DDevice8_End name PatchPrefix + 'D3DDevice_End',
  XTL_EmuIDirect3DDevice8_EndPush name PatchPrefix + 'D3DDevice_EndPushBuffer@0', // ??
  XTL_EmuIDirect3DDevice8_EndPushBuffer name PatchPrefix + 'D3DDevice_EndPushBuffer', // ??
  XTL_EmuIDirect3DDevice8_EndStateBlock name PatchPrefix + 'D3DDevice_EndStateBlock',
  XTL_EmuIDirect3DDevice8_EndVisibilityTest name PatchPrefix + 'D3DDevice_EndVisibilityTest@4',
  XTL_EmuIDirect3DDevice8_FlushVertexCache name PatchPrefix + 'D3DDevice_FlushVertexCache', // ??
  XTL_EmuIDirect3DDevice8_GetBackBuffer name PatchPrefix + 'D3DDevice_GetBackBuffer',
  XTL_EmuIDirect3DDevice8_GetBackBuffer2 name PatchPrefix + 'D3DDevice_GetBackBuffer2@4',
  XTL_EmuIDirect3DDevice8_GetCreationParameters name PatchPrefix + 'D3DDevice_GetCreationParameters',
  XTL_EmuIDirect3DDevice8_GetDepthStencilSurface name PatchPrefix + 'D3DDevice_GetDepthStencilSurface',
  XTL_EmuIDirect3DDevice8_GetDepthStencilSurface2 name PatchPrefix + 'D3DDevice_GetDepthStencilSurface2',
  XTL_EmuIDirect3DDevice8_GetDeviceCaps name PatchPrefix + 'D3DDevice_GetDeviceCaps',
  XTL_EmuIDirect3DDevice8_GetDisplayFieldStatus name PatchPrefix + 'D3DDevice_GetDisplayFieldStatus',
  XTL_EmuIDirect3DDevice8_GetDisplayMode name PatchPrefix + 'D3DDevice_GetDisplayMode',
  XTL_EmuIDirect3DDevice8_GetGammaRamp name PatchPrefix + 'D3DDevice_GetGammaRamp',
  XTL_EmuIDirect3DDevice8_GetModelView name PatchPrefix + 'D3DDevice_GetModelView', // ??
  XTL_EmuIDirect3DDevice8_GetOverlayUpdateStatus name PatchPrefix + 'D3DDevice_GetOverlayUpdateStatus',
  XTL_EmuIDirect3DDevice8_GetProjectionViewportMatrix name PatchPrefix + 'D3DDevice_GetProjectionViewportMatrix',
  XTL_EmuIDirect3DDevice8_GetRenderTarget name PatchPrefix + 'D3DDevice_GetRenderTarget',
  XTL_EmuIDirect3DDevice8_GetRenderTarget2 name PatchPrefix + 'D3DDevice_GetRenderTarget2',
  XTL_EmuIDirect3DDevice8_GetShaderConstantMode name PatchPrefix + 'D3DDevice_GetShaderConstantMode',
  XTL_EmuIDirect3DDevice8_GetStreamSource2 name PatchPrefix + 'D3DDevice_GetStreamSource2',
  XTL_EmuIDirect3DDevice8_GetTexture2 name PatchPrefix + 'D3DDevice_GetTexture2',
  XTL_EmuIDirect3DDevice8_GetTile name PatchPrefix + 'D3DDevice_GetTile',
  XTL_EmuIDirect3DDevice8_GetTransform name PatchPrefix + 'D3DDevice_GetTransform',
  XTL_EmuIDirect3DDevice8_GetVertexShader name PatchPrefix + 'D3DDevice_GetVertexShader',
  XTL_EmuIDirect3DDevice8_GetVertexShaderConstant name PatchPrefix + 'D3DDevice_GetVertexShaderConstant',
  XTL_EmuIDirect3DDevice8_GetVertexShaderDeclaration name PatchPrefix + 'D3DDevice_GetVertexShaderDeclaration',
  XTL_EmuIDirect3DDevice8_GetVertexShaderFunction name PatchPrefix + 'D3DDevice_GetVertexShaderFunction',
  XTL_EmuIDirect3DDevice8_GetVertexShaderInput name PatchPrefix + 'D3DDevice_GetVertexShaderInput',
  XTL_EmuIDirect3DDevice8_GetVertexShaderSize name PatchPrefix + 'D3DDevice_GetVertexShaderSize',
  XTL_EmuIDirect3DDevice8_GetVertexShaderType name PatchPrefix + 'D3DDevice_GetVertexShaderType',
  XTL_EmuIDirect3DDevice8_GetViewport name PatchPrefix + 'D3DDevice_GetViewport',
  XTL_EmuIDirect3DDevice8_GetViewportOffsetAndScale name PatchPrefix + 'D3DDevice_GetViewportOffsetAndScale',
  XTL_EmuIDirect3DDevice8_GetVisibilityTestResult name PatchPrefix + 'D3DDevice_GetVisibilityTestResult',
  XTL_EmuIDirect3DDevice8_InsertCallback name PatchPrefix + 'D3DDevice_InsertCallback',
  XTL_EmuIDirect3DDevice8_InsertFence name PatchPrefix + 'D3DDevice_InsertFence', // : DXBX for better logging.
  XTL_EmuIDirect3DDevice8_IsBusy name PatchPrefix + 'D3DDevice_IsBusy',
  XTL_EmuIDirect3DDevice8_IsFencePending name PatchPrefix + 'D3DDevice_IsFencePending',
  XTL_EmuIDirect3DDevice8_LightEnable name PatchPrefix + 'D3DDevice_LightEnable',
  XTL_EmuIDirect3DDevice8_LoadVertexShader name PatchPrefix + 'D3DDevice_LoadVertexShader',
  XTL_EmuIDirect3DDevice8_LoadVertexShaderProgram name PatchPrefix + 'D3DDevice_LoadVertexShaderProgram',
  XTL_EmuIDirect3DDevice8_PersistDisplay name PatchPrefix + 'D3DDevice_PersistDisplay',
  XTL_EmuIDirect3DDevice8_Present name PatchPrefix + 'D3DDevice_Present',
  XTL_EmuIDirect3DDevice8_PrimeVertexCache name PatchPrefix + 'D3DDevice_PrimeVertexCache',
  XTL_EmuIDirect3DDevice8_Release name PatchPrefix + 'D3DDevice_Release',
  XTL_EmuIDirect3DDevice8_Reset name PatchPrefix + 'D3DDevice_Reset',
  XTL_EmuIDirect3DDevice8_RunPushBuffer name PatchPrefix + 'D3DDevice_RunPushBuffer',
  XTL_EmuIDirect3DDevice8_RunVertexStateShader name PatchPrefix + 'D3DDevice_RunVertexStateShader',
  XTL_EmuIDirect3DDevice8_SelectVertexShader name PatchPrefix + 'D3DDevice_SelectVertexShader',
  XTL_EmuIDirect3DDevice8_SelectVertexShaderDirect name PatchPrefix + 'D3DDevice_SelectVertexShaderDirect',
  XTL_EmuIDirect3DDevice8_SetBackBufferScale name PatchPrefix + 'D3DDevice_SetBackBufferScale',
  XTL_EmuIDirect3DDevice8_SetBackMaterial name PatchPrefix + 'D3DDevice_SetBackMaterial', // ??
  XTL_EmuIDirect3DDevice8_SetFlickerFilter name PatchPrefix + 'D3DDevice_SetFlickerFilter',
  XTL_EmuIDirect3DDevice8_SetGammaRamp name PatchPrefix + 'Direct3D8_SetGammaRamp',
  XTL_EmuIDirect3DDevice8_SetIndices name PatchPrefix + 'D3DDevice_SetIndices',
  XTL_EmuIDirect3DDevice8_SetLight name PatchPrefix + 'D3DDevice_SetLight',
  XTL_EmuIDirect3DDevice8_SetMaterial name PatchPrefix + 'D3DDevice_SetMaterial',
  XTL_EmuIDirect3DDevice8_SetModelView name PatchPrefix + 'D3DDevice_SetModelView', // ??
  XTL_EmuIDirect3DDevice8_SetVertexBlendModelView name PatchPrefix + 'D3DDevice_SetVertexBlendModelView', // ??
  XTL_EmuIDirect3DDevice8_SetPalette name PatchPrefix + 'D3DDevice_SetPalette',
  XTL_EmuIDirect3DDevice8_SetPixelShader name PatchPrefix + 'D3DDevice_SetPixelShader',
  XTL_EmuIDirect3DDevice8_SetPixelShaderConstant name PatchPrefix + 'D3DDevice_SetPixelShaderConstant',
  XTL_EmuIDirect3DDevice8_SetPixelShaderProgram name PatchPrefix + 'D3DDevice_SetPixelShaderProgram',
  XTL_EmuIDirect3DDevice8_SetRenderState_BackFillMode name PatchPrefix + 'D3DDevice_SetRenderState_BackFillMode',
  XTL_EmuIDirect3DDevice8_SetRenderState_CullMode name PatchPrefix + 'D3DDevice_SetRenderState_CullMode',
//  XTL_EmuIDirect3DDevice8_SetRenderState_Deferred name PatchPrefix + 'D3DDevice_SetRenderState_Deferred',
  XTL_EmuIDirect3DDevice8_SetRenderState_DoNotCullUncompressed name PatchPrefix + 'D3DDevice_SetRenderState_DoNotCullUncompressed',
  XTL_EmuIDirect3DDevice8_SetRenderState_Dxt1NoiseEnable name PatchPrefix + 'D3DDevice_SetRenderState_Dxt1NoiseEnable',
  XTL_EmuIDirect3DDevice8_SetRenderState_EdgeAntiAlias name PatchPrefix + 'D3DDevice_SetRenderState_EdgeAntiAlias',
  XTL_EmuIDirect3DDevice8_SetRenderState_FillMode name PatchPrefix + 'D3DDevice_SetRenderState_FillMode',
  XTL_EmuIDirect3DDevice8_SetRenderState_FogColor name PatchPrefix + 'D3DDevice_SetRenderState_FogColor',
  XTL_EmuIDirect3DDevice8_SetRenderState_FrontFace name PatchPrefix + 'D3DDevice_SetRenderState_FrontFace',
  XTL_EmuIDirect3DDevice8_SetRenderState_LineWidth name PatchPrefix + 'D3DDevice_SetRenderState_LineWidth',
  XTL_EmuIDirect3DDevice8_SetRenderState_LogicOp name PatchPrefix + 'D3DDevice_SetRenderState_LogicOp',
  XTL_EmuIDirect3DDevice8_SetRenderState_MultiSampleAntiAlias name PatchPrefix + 'D3DDevice_SetRenderState_MultiSampleAntiAlias',
  XTL_EmuIDirect3DDevice8_SetRenderState_MultiSampleMask name PatchPrefix + 'D3DDevice_SetRenderState_MultiSampleMask',
  XTL_EmuIDirect3DDevice8_SetRenderState_MultiSampleMode name PatchPrefix + 'D3DDevice_SetRenderState_MultiSampleMode',
  XTL_EmuIDirect3DDevice8_SetRenderState_MultiSampleRenderTargetMode name PatchPrefix + 'D3DDevice_SetRenderState_MultiSampleRenderTargetMode',
  XTL_EmuIDirect3DDevice8_SetRenderState_NormalizeNormals name PatchPrefix + 'D3DDevice_SetRenderState_NormalizeNormals',
  XTL_EmuIDirect3DDevice8_SetRenderState_OcclusionCullEnable name PatchPrefix + 'D3DDevice_SetRenderState_OcclusionCullEnable',
  XTL_EmuIDirect3DDevice8_SetRenderState_PSTextureModes name PatchPrefix + 'D3DDevice_SetRenderState_PSTextureModes',
  XTL_EmuIDirect3DDevice8_SetRenderState_RopZCmpAlwaysRead name PatchPrefix + 'D3DDevice_SetRenderState_RopZCmpAlwaysRead',
  XTL_EmuIDirect3DDevice8_SetRenderState_RopZRead name PatchPrefix + 'D3DDevice_SetRenderState_RopZRead',
  XTL_EmuIDirect3DDevice8_SetRenderState_SampleAlpha name PatchPrefix + 'D3DDevice_SetRenderState_SampleAlpha',
  XTL_EmuIDirect3DDevice8_SetRenderState_ShadowFunc name PatchPrefix + 'D3DDevice_SetRenderState_ShadowFunc',
  XTL_EmuIDirect3DDevice8_SetRenderState_Simple name PatchPrefix + 'D3DDevice_SetRenderState_Simple',
  XTL_EmuIDirect3DDevice8_SetRenderState_StencilCullEnable name PatchPrefix + 'D3DDevice_SetRenderState_StencilCullEnable',
  XTL_EmuIDirect3DDevice8_SetRenderState_StencilEnable name PatchPrefix + 'D3DDevice_SetRenderState_StencilEnable',
  XTL_EmuIDirect3DDevice8_SetRenderState_StencilFail name PatchPrefix + 'D3DDevice_SetRenderState_StencilFail',
  XTL_EmuIDirect3DDevice8_SetRenderState_TextureFactor name PatchPrefix + 'D3DDevice_SetRenderState_TextureFactor',
  XTL_EmuIDirect3DDevice8_SetRenderState_VertexBlend name PatchPrefix + 'D3DDevice_SetRenderState_VertexBlend',
  XTL_EmuIDirect3DDevice8_SetRenderState_YuvEnable name PatchPrefix + 'D3DDevice_SetRenderState_YuvEnable',
  XTL_EmuIDirect3DDevice8_SetRenderState_ZBias name PatchPrefix + 'D3DDevice_SetRenderState_ZBias',
  XTL_EmuIDirect3DDevice8_SetRenderState_ZEnable name PatchPrefix + 'D3DDevice_SetRenderState_ZEnable',
  XTL_EmuIDirect3DDevice8_SetRenderTarget name PatchPrefix + 'D3DDevice_SetRenderTarget',
  XTL_EmuIDirect3DDevice8_SetScissors name PatchPrefix + 'D3DDevice_SetScissors',
  XTL_EmuIDirect3DDevice8_SetScreenSpaceOffset name PatchPrefix + 'D3DDevice_SetScreenSpaceOffset',
  XTL_EmuIDirect3DDevice8_SetShaderConstantMode name PatchPrefix + 'D3DDevice_SetShaderConstantMode',
  XTL_EmuIDirect3DDevice8_SetSoftDisplayFilter name PatchPrefix + 'D3DDevice_SetSoftDisplayFilter',
  XTL_EmuIDirect3DDevice8_SetStateUP name PatchPrefix + 'D3DDevice_SetStateUP',
  XTL_EmuIDirect3DDevice8_SetStateVB name PatchPrefix + 'D3DDevice_SetStateVB',
  XTL_EmuIDirect3DDevice8_SetStipple name PatchPrefix + 'D3DDevice_SetStipple',
  XTL_EmuIDirect3DDevice8_SetStreamSource name PatchPrefix + 'D3DDevice_SetStreamSource',
  XTL_EmuIDirect3DDevice8_SetSwapCallback name PatchPrefix + 'D3DDevice_SetSwapCallback',
  XTL_EmuIDirect3DDevice8_SetTexture name PatchPrefix + 'D3DDevice_SetTexture',
  XTL_EmuIDirect3DDevice8_SetTextureState_BorderColor name PatchPrefix + 'D3DDevice_SetTextureState_BorderColor',
  XTL_EmuIDirect3DDevice8_SetTextureState_BumpEnv name PatchPrefix + 'D3DDevice_SetTextureState_BumpEnv',
  XTL_EmuIDirect3DDevice8_SetTextureState_ColorKeyColor name PatchPrefix + 'D3DDevice_SetTextureState_ColorKeyColor',
  XTL_EmuIDirect3DDevice8_SetTextureState_TexCoordIndex name PatchPrefix + 'D3DDevice_SetTextureState_TexCoordIndex',
  XTL_EmuIDirect3DDevice8_SetTextureState_TwoSidedLighting name PatchPrefix + 'D3DDevice_SetTextureState_TwoSidedLighting',
  XTL_EmuIDirect3DDevice8_SetTextureState_TwoSidedLighting name PatchPrefix + 'D3DDevice_SetRenderState_TwoSidedLighting', // Cxbx:Beware of the typo Dxbx:Use patch for both!
  XTL_EmuIDirect3DDevice8_SetTileNoWait name PatchPrefix + '?SetTileNoWait@D3D@@YGXKPBU_D3DTILE@@@Z',
  XTL_EmuIDirect3DDevice8_SetTileNoWait name PatchPrefix + 'D3DDevice_SetTile', // Dxbx note : SetTileNoWait is applied to SetTile in Cxbx 4361 OOPVA's!
  XTL_EmuIDirect3DDevice8_SetTransform name PatchPrefix + 'D3DDevice_SetTransform',
  XTL_EmuIDirect3DDevice8_SetVertexData2f name PatchPrefix + 'D3DDevice_SetVertexData2f',
  XTL_EmuIDirect3DDevice8_SetVertexData2s name PatchPrefix + 'D3DDevice_SetVertexData2s',
  XTL_EmuIDirect3DDevice8_SetVertexData4f name PatchPrefix + 'D3DDevice_SetVertexData4f',
  XTL_EmuIDirect3DDevice8_SetVertexData4s name PatchPrefix + 'D3DDevice_SetVertexData4s',
  XTL_EmuIDirect3DDevice8_SetVertexData4ub name PatchPrefix + 'D3DDevice_SetVertexData4ub',
  XTL_EmuIDirect3DDevice8_SetVertexDataColor name PatchPrefix + 'D3DDevice_SetVertexDataColor',
  XTL_EmuIDirect3DDevice8_SetVertexShader name PatchPrefix + 'D3DDevice_SetVertexShader',
  XTL_EmuIDirect3DDevice8_SetVertexShaderConstant name PatchPrefix + 'D3DDevice_SetVertexShaderConstant',
  XTL_EmuIDirect3DDevice8_SetVertexShaderConstant1 name PatchPrefix + 'D3DDevice_SetVertexShaderConstant1',
  XTL_EmuIDirect3DDevice8_SetVertexShaderConstant4 name PatchPrefix + 'D3DDevice_SetVertexShaderConstant4',
  XTL_EmuIDirect3DDevice8_SetVertexShaderConstantNotInline name PatchPrefix + 'D3DDevice_SetVertexShaderConstantNotInline',
  XTL_EmuIDirect3DDevice8_SetVertexShaderInput name PatchPrefix + 'D3DDevice_SetVertexShaderInput',
  XTL_EmuIDirect3DDevice8_SetVertexShaderInputDirect name PatchPrefix + 'D3DDevice_SetVertexShaderInputDirect',
  XTL_EmuIDirect3DDevice8_SetVerticalBlankCallback name PatchPrefix + 'D3DDevice_SetVerticalBlankCallback',
  XTL_EmuIDirect3DDevice8_SetViewport name PatchPrefix + 'D3DDevice_SetViewport',
  XTL_EmuIDirect3DDevice8_Swap name PatchPrefix + 'D3DDevice_Swap',
  XTL_EmuIDirect3DDevice8_SwitchTexture name PatchPrefix + 'D3DDevice_SwitchTexture',
  XTL_EmuIDirect3DDevice8_Unknown1 name PatchPrefix + 'D3DDevice_Unknown', // TODO -oDXBX: Fix wrong prefix!
  XTL_EmuIDirect3DDevice8_UpdateOverlay name PatchPrefix + 'D3DDevice_UpdateOverlay',

  XTL_EmuIDirect3DPalette8_Lock name PatchPrefix + 'D3DPalette_Lock',
  XTL_EmuIDirect3DPalette8_Lock2 name PatchPrefix + 'D3DPalette_Lock2',

  XTL_EmuIDirect3DResource8_AddRef name PatchPrefix + 'D3DResource_AddRef',
  XTL_EmuIDirect3DResource8_IsBusy name PatchPrefix + 'D3DResource_IsBusy',
  XTL_EmuIDirect3DResource8_GetDevice name PatchPrefix + 'D3DResource_GetDevice',
  XTL_EmuIDirect3DResource8_Register name PatchPrefix + 'D3DResource_Register',
  XTL_EmuIDirect3DResource8_BlockUntilNotBusy name PatchPrefix + 'D3DResource_BlockUntilNotBusy',
  XTL_EmuIDirect3DResource8_SetPrivateData name PatchPrefix + 'D3DResource_SetPrivateData',
  XTL_EmuIDirect3DResource8_GetPrivateData name PatchPrefix + 'D3DResource_GetPrivateData',
  XTL_EmuIDirect3DResource8_FreePrivateData name PatchPrefix + 'D3DResource_FreePrivateData',

  XTL_EmuIDirect3DResource8_GetType name PatchPrefix + 'D3DResource_GetType',
  XTL_EmuIDirect3DResource8_Release name PatchPrefix + 'D3DResource_Release',

  XTL_EmuIDirect3DSurface8_GetDesc name PatchPrefix + 'D3DSurface_GetDesc',
  XTL_EmuIDirect3DSurface8_LockRect name PatchPrefix + 'D3DSurface_LockRect@16',

  XTL_EmuIDirect3DTexture8_GetLevelDesc name PatchPrefix + 'D3DTexture_GetLevelDesc', // DXBX : better
  XTL_EmuIDirect3DTexture8_GetSurfaceLevel name PatchPrefix + 'D3DTexture_GetSurfaceLevel',
  XTL_EmuIDirect3DTexture8_GetSurfaceLevel2 name PatchPrefix + 'D3DTexture_GetSurfaceLevel2',
  XTL_EmuIDirect3DTexture8_LockRect name PatchPrefix + 'D3DTexture_LockRect',

  XTL_EmuIDirect3DVertexBuffer8_GetDesc name PatchPrefix + 'D3DVertexBuffer_GetDesc',
  XTL_EmuIDirect3DVertexBuffer8_Lock name PatchPrefix + 'D3DVertexBuffer_Lock',
  XTL_EmuIDirect3DVertexBuffer8_Lock2 name PatchPrefix + 'D3DVertexBuffer_Lock2',

  XTL_EmuIDirect3DVolumeTexture8_LockBox name PatchPrefix + 'D3DVolumeTexture_LockBox',

  XTL_EmuLock2DSurface name PatchPrefix + 'Lock2DSurface';

//  XTL_EmuXMETAL_StartPush name PatchPrefix + 'XMETAL_StartPush';  // DXBX - MARKED OUT 4x4 EVO 2

initialization

  ZeroMemory(@g_VBData, SizeOf(g_VBData));
  ZeroMemory(@g_SwapData, SizeOf(g_SwapData));

end.
