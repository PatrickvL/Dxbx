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
  // DirectX
  Direct3D, // PD3DCOLOR
  Direct3D8, // D3DDEVTYPE
  DirectDraw, // IDIRECTDRAWSURFACE7
  D3DX8, // PD3DXVECTOR4
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uConsts,
  uTypes,
  uDxbxUtils,
  uLog,
  uXbe,
  uPushBuffer,
  uEmuDInput,
  uEmu,
  uEmuAlloc,
  uEmuKrnl,
  uEmuXTL,
  uVertexShader,
  uPixelShader,
  uResourceTracker,
  uConvert,
  uEmuD3D8Types,
  uEmuD3D8Utils,
  uXbVideo,
  uEmuShared,
  uEmuFS,
  uEmuXapi,
  uEmuXG,
  uDxbxKrnlUtils,
  uVertexBuffer,
  uState;

procedure XTL_EmuD3DInit(XbeHeader: PXBE_HEADER; XbeHeaderSize: UInt32); stdcall; // forward
function XTL_EmuIDirect3D8_CreateDevice(Adapter: UINT; DeviceType: D3DDEVTYPE;
  hFocusWindow: HWND; BehaviorFlags: DWORD;
  pPresentationParameters: PX_D3DPRESENT_PARAMETERS;
  ppReturnedDeviceInterface: XTL_PPIDirect3DDevice8): HRESULT; stdcall// forward

//function XTL_EmuIDirect3DDevice8_SetVertexData2f(Register_: Integer;
//  a: FLOAT; b: FLOAT): HRESULT; stdcall;
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
  g_bFakePixelShaderLoaded: BOOL = FALSE;
  g_bIsFauxFullscreen: BOOL = FALSE;
  g_bHackUpdateSoftwareOverlay: BOOL = FALSE;

function EmuRenderWindow(lpVoid: LPVOID): DWORD; // no stdcall !
function EmuCreateDeviceProxy(lpVoid: LPVOID): DWORD; // no stdcall !
function EmuMsgProc(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; // forward
function EmuUpdateTickCount(lpVoid: LPVOID): DWORD; // no stdcall !


var
  // Static Variable(s)
  g_ddguid: GUID;                // DirectDraw driver GUID
  g_hMonitor: HMONITOR = 0;      // Handle to DirectDraw monitor
  g_pD3D8: XTL_LPDIRECT3D8 = NULL;    // Direct3D8
  g_bSupportsYUY2: BOOL = FALSE; // Does device support YUY2 overlays?
  g_pDD7: XTL_LPDIRECTDRAW7 = NULL;   // DirectDraw7
  g_dwOverlayW: DWORD = 640;     // Cached Overlay Width
  g_dwOverlayH: DWORD = 480;     // Cached Overlay Height
  g_dwOverlayP: DWORD = 640;     // Cached Overlay Pitch

  g_XbeHeader: PXBE_HEADER = NULL;         // XbeHeader
  g_XbeHeaderSize: uint32 = 0;             // XbeHeaderSize
  g_D3DCaps: D3DCAPS8;                     // Direct3D8 Caps
  g_hBgBrush: HBRUSH = 0;                  // Background Brush
  g_bRenderWindowActive: bool = false;     // volatile?
  g_XBVideo: XBVideo;
  g_pVBCallback: D3DVBLANKCALLBACK = NULL; // Vertical-Blank callback routine
  g_pSwapCallback: D3DSWAPCALLBACK = NULL; // Swap/Present callback routine

  // wireframe toggle
  g_iWireframe: int = 0;

  // build version
  // g_BuildVersion: uint32;

  // resource caching for _Register
  pCache: array [0..16 - 1] of X_D3DResource; // = {0};

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
  g_SwapLast: DWORD = 0;

  // cached Direct3D state variable(s)
  g_pCachedRenderTarget: PX_D3DSurface = NULL;
  g_pCachedZStencilSurface: PX_D3DSurface = NULL;
  g_YuvSurface: PX_D3DSurface = NULL;
  g_fYuvEnabled: BOOL = FALSE;
  g_dwVertexShaderUsage: DWORD = 0;
  g_VertexShaderSlots: array [0..136 - 1] of DWORD;

  // cached palette pointer
  pCurrentPalette: PVOID;
  // cached palette size
  dwCurrentPaletteSize: DWORD = DWORD(-1);

  g_VertexShaderConstantMode: X_VERTEXSHADERCONSTANTMODE = X_VSCM_192;

// cached Direct3D tiles
// EmuD3DTileCache (8 tiles maximum)
var EmuD3DTileCache: array [0..8 - 1] of X_D3DTILE;

// cached active texture
var EmuD3DActiveTexture: array [0..4 - 1] of PX_D3DResource; // = {0,0,0,0};

// information passed to the create device proxy thread
type EmuD3D8CreateDeviceProxyData = packed record
    Adapter: UINT;
    DeviceType: D3DDEVTYPE;
    hFocusWindow: HWND;
    BehaviorFlags: DWORD;
    pPresentationParameters: PX_D3DPRESENT_PARAMETERS;
    ppReturnedDeviceInterface: XTL_PPIDirect3DDevice8;
    {volatile} bReady: bool;
    {union} case Integer of
      0: ({volatile} hRet: HRESULT);
      1: ({volatile} bCreate: bool); // False: release
  end;

var g_EmuCDPD: EmuD3D8CreateDeviceProxyData;

implementation

uses
  // Dxbx
  uDxbxKrnl
  , uXboxLibraryUtils; // Should not be here, but needed for CxbxKrnlRegisterThread

// Direct3D initialization (called before emulation begins)
procedure XTL_EmuD3DInit(XbeHeader: PXBE_HEADER; XbeHeaderSize: uint32); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  dwThreadId: DWORD;
  hThread: HANDLE;
  hDupHandle: HANDLE;
  DevType: D3DDEVTYPE;
  PresParam: X_D3DPRESENT_PARAMETERS;
begin
  g_EmuShared.GetXBVideo(@g_XBVideo);

  if g_XBVideo.GetFullscreen() then
    CxbxKrnl_hEmuParent := 0;

  // cache XbeHeader and size of XbeHeader
  g_XbeHeader := XbeHeader;
  g_XbeHeaderSize := XbeHeaderSize;
  
  // create timing thread
  begin
    dwThreadId := 0;
    hThread := BeginThread(nil, 0, @EmuUpdateTickCount, nil, 0, {var} dwThreadId);

    // we must duplicate this handle in order to retain Suspend/Resume thread rights from a remote thread
    begin
      hDupHandle := 0;

      DuplicateHandle(GetCurrentProcess(), hThread, GetCurrentProcess(), @hDupHandle, 0, False, DUPLICATE_SAME_ACCESS);

      CxbxKrnlRegisterThread(hDupHandle);
    end;
  end;

  // create the create device proxy thread
  begin
    BeginThread(nil, 0, @EmuCreateDeviceProxy, nil, 0, {var} dwThreadId);
  end;

  // create window message processing thread
  begin
    g_bRenderWindowActive := false;

    BeginThread(nil, 0, @EmuRenderWindow, nil, 0, {var} dwThreadId);

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
      CxbxKrnlCleanup('Could not initialize Direct3D8!');

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
    PresParam.EnableAutoDepthStencil := TRUE;
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

// cleanup Direct3D
procedure XTL_EmuD3DCleanup(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  XTL_EmuDInputCleanup();
end;

// enumeration procedure for locating display device GUIDs
function EmuEnumDisplayDevices(lpGUID: PGUID; lpDriverDescription: LPSTR;
  lpDriverName: LPSTR; lpContext: LPVOID; hm: HMONITOR): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
{$WRITEABLECONST ON}
const
  dwEnumCount: DWORD = 0;
{$WRITEABLECONST OFF}
begin
  Inc(dwEnumCount); // Cxbx uses post-increment and compares+1 :
  if (dwEnumCount = g_XBVideo.GetDisplayAdapter()) then
  begin
    g_hMonitor := hm;
    dwEnumCount := 0;
    if (lpGUID <> nil) then
      memcpy(@g_ddguid, lpGUID, sizeof(GUID))
    else
      memset(@g_ddguid, 0, sizeof(GUID));

    Result := FALSE;
    Exit;
  end;

  Result := TRUE;
end; // EmuEnumDisplayDevices

// window message processing thread
function EmuRenderWindow(lpVoid: LPVOID): DWORD; // no stdcall !
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
  lPrintfOn: bool;
begin
  // register window class
  begin
    hDxbxDLL := MainInstance;

    logBrush.lbStyle := BS_SOLID;
    logBrush.lbColor := RGB(0, 0, 0);
    logBrush.lbHatch := 0;

    g_hBgBrush := CreateBrushIndirect(logBrush);

    wc.cbSize := sizeof(WNDCLASSEX);
    wc.style := CS_CLASSDC;
    wc.lpfnWndProc := @EmuMsgProc;
    wc.cbClsExtra := 0;
    wc.cbWndExtra := 0;
    wc.hInstance := GetModuleHandle(NULL); // MainInstance; ??
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

    // Dxbx TODO : These $0C and 40 values should become a suiteably named constant :
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
    dwStyle := iif(g_XBVideo.GetFullscreen() or (CxbxKrnl_hEmuParent = 0), WS_OVERLAPPEDWINDOW, WS_CHILD);

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
      hwndParent := CxbxKrnl_hEmuParent;
    end;

    g_hEmuWindow := CreateWindow(
      DXBX_RENDER_CLASS,
      PChar(AsciiTitle),
      dwStyle, x, y, nWidth, nHeight,
      hwndParent,
      HMENU(0),
      GetModuleHandle(NULL), // MainInstance ??
      NULL
      );
  end;

  ShowWindow(g_hEmuWindow, iif((CxbxKrnl_hEmuParent = 0) or g_XBVideo.GetFullscreen, SW_SHOWDEFAULT, SW_SHOWMAXIMIZED));
  UpdateWindow(g_hEmuWindow);

  if (not g_XBVideo.GetFullscreen) and (CxbxKrnl_hEmuParent <> 0) then
    SetFocus(CxbxKrnl_hEmuParent);

  // initialize direct input
  if not XTL_EmuDInputInit() then
    CxbxKrnlCleanup('Could not initialize DirectInput!');

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : Message-Pump thread is running.');
{$ENDIF}

  SetFocus(g_hEmuWindow);

  { Dxbx TODO : Need to be translated to delphi }
  (*
  DbgConsole *dbgConsole := new DbgConsole();
  *)

  // message processing loop
  begin
    ZeroMemory(@msg, sizeof(msg));

    lPrintfOn := g_bPrintfOn;

    g_bRenderWindowActive := true;
    
    while msg.message <> WM_QUIT do
    begin
      if PeekMessage({var}msg, 0, 0, 0, PM_REMOVE) then
      begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end
      else
      begin
        Sleep(10); // Dxbx: Should we use SwitchToThread() or YieldProcessor() ?

        // if we've just switched back to display off, clear buffer & display prompt
        if not g_bPrintfOn and lPrintfOn then
          { Dxbx TODO : Need to be translated to delphi }
          ; // dbgConsole.Reset();

        lPrintfOn := g_bPrintfOn;

        (*dbgConsole.Process();
        *)
      end;
    end;

    g_bRenderWindowActive := false;

//        delete dbgConsole;

    CxbxKrnlCleanup('');
  end;

  Result := D3D_OK;
end; // EmuRenderWindow

// simple helper function
procedure ToggleFauxFullscreen(hWnd: HWND);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
{$WRITEABLECONST ON}
const
  lRestore: LONG = 0;
  lRestoreEx: LONG = 0;
  lRect: TRect = ();
{$WRITEABLECONST OFF}
begin
  if (g_XBVideo.GetFullscreen()) then
    Exit;

  if (not g_bIsFauxFullscreen) then
  begin
    if (CxbxKrnl_hEmuParent <> 0) then
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
    if CxbxKrnl_hEmuParent <> 0 then
    begin
      SetParent(hWnd, CxbxKrnl_hEmuParent);
      SetWindowLong(hWnd, GWL_STYLE, WS_CHILD);
      ShowWindow(hWnd, SW_MAXIMIZE);
      SetFocus(CxbxKrnl_hEmuParent);
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
function EmuMsgProc(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
{$WRITEABLECONST ON}
const
  bAutoPaused: bool = false;
{$WRITEABLECONST OFF}
begin
  case (msg) of
    WM_DESTROY:
      begin
        DeleteObject(g_hBgBrush);
        PostQuitMessage(0);
        Result := D3D_OK;
        Exit;
      end;

    WM_SYSKEYDOWN:
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

    WM_KEYDOWN:
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
          if g_iWireframe = 2-1 then
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
                CxbxKrnlResume();
              end;
            end;

          SIZE_MINIMIZED:
            begin
              if (g_XBVideo.GetFullscreen()) then
                CxbxKrnlCleanup('');

              if (not g_bEmuSuspended) then
              begin
                bAutoPaused := true;
                CxbxKrnlSuspend();
              end;
            end;
        end;
      end;

    WM_CLOSE:
      DestroyWindow(hWnd);

    WM_SETFOCUS:
      begin
        if (CxbxKrnl_hEmuParent <> 0) then
        begin
          SetFocus(CxbxKrnl_hEmuParent);
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
function EmuUpdateTickCount(lpVoid: LPVOID): DWORD; // no stdcall !
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  v: int;
  hDevice: HANDLE;
  dwLatency: DWORD;
  pFeedback: PXINPUT_FEEDBACK;
begin
  // since callbacks come from here
  EmuGenerateFS(CxbxKrnl_TLS, CxbxKrnl_TLSData);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : Timing thread is running.');
{$ENDIF}

  timeBeginPeriod(0);

  // current vertical blank count
  while true do
  begin
    xboxkrnl_KeTickCount := timeGetTime();
    Sleep(1); // Dxbx: Should we use SwitchToThread() or YieldProcessor() ?

    // Poll input
    begin
      for v := 0 to XINPUT_SETSTATE_SLOTS - 1 do
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
            SetEvent(pFeedback.Header.hEvent);

          pFeedback.Header.dwStatus := ERROR_SUCCESS;
        end;
      end;
    end;

    // trigger vblank callback
    begin
      Inc(g_VBData.VBlank);

			// Cxbx TODO: Fixme.  This may not be right...
			g_SwapData.SwapVBlank := 1;

      if (Addr(g_pVBCallback) <> NULL) then
      begin
        EmuSwapFS(fsXbox);
        g_pVBCallback(@g_VBData);
        EmuSwapFS(fsWindows);
      end;

      g_VBData.Swap := 0;
    end;
    
    // Cxbx TODO: This can't be accurate...
    g_SwapData.TimeUntilSwapVBlank := 0;

    // Cxbx TODO: Recalculate this for PAL version if necessary.
    // Also, we should check the D3DPRESENT_INTERVAL value for accurracy.
    //  g_SwapData.TimeBetweenSwapVBlanks = 1/60;
    g_SwapData.TimeBetweenSwapVBlanks := 0;
  end; // while

  timeEndPeriod(0);
end; // EmuUpdateTickCount

// thread dedicated to create devices
function EmuCreateDeviceProxy(lpVoid: LPVOID): DWORD; // no stdcall !
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  D3DDisplayMode: TD3DDisplayMode; // X_D3DDISPLAYMODE; // Dxbx TODO : What type should we use?
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
    if not g_EmuCDPD.bReady then
    begin
      Sleep(1); // Dxbx: Should we use SwitchToThread() or YieldProcessor() ?
      Continue;
    end;

    // if we have been signalled, create the device with cached parameters
{$IFDEF DEBUG}
    DbgPrintf('EmuD3D8 : CreateDevice proxy thread received request.');
{$ENDIF}

    if (g_EmuCDPD.bCreate) then
    begin
      // only one device should be created at once
      // Cxbx TODO : ensure all surfaces are somehow cleaned up?
      if (g_pD3DDevice8 <> nil) then
      begin
{$IFDEF DEBUG}
        DbgPrintf('EmuD3D8 : CreateDevice proxy thread releasing old Device.');
{$ENDIF}

        IDirect3DDevice8(g_pD3DDevice8).EndScene();

        while (IDirect3DDevice8(g_pD3DDevice8)._Release() <> 0) do
          ;

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


          // Cxbx HACK: Disable Tripple Buffering for now...
          // Cxbx TODO: Enumerate maximum BackBufferCount if possible.
          if g_EmuCDPD.pPresentationParameters.BackBufferCount > 1 then
          begin
            EmuWarning('Limiting BackBufferCount to 1...');
            g_EmuCDPD.pPresentationParameters.BackBufferCount := 1;
          end;
        end;

        // Cxbx TODO : Support Xbox extensions if possible
        if (g_EmuCDPD.pPresentationParameters.MultiSampleType <> D3DMULTISAMPLE_NONE) then
        begin
          EmuWarning('MultiSampleType 0x%.08X is not supported!', [Ord(g_EmuCDPD.pPresentationParameters.MultiSampleType)]);

          g_EmuCDPD.pPresentationParameters.MultiSampleType := D3DMULTISAMPLE_NONE;

          // Cxbx TODO : Check card for multisampling abilities // MARKDE OUT BY CXBX
          // if (g_EmuCDPD.pPresentationParameters.MultiSampleType = $00001121) then
          //   g_EmuCDPD.pPresentationParameters.MultiSampleType := D3DMULTISAMPLE_2_SAMPLES
          // else
          //   CxbxKrnlCleanup('Unknown MultiSampleType (0x%.08X)', [g_EmuCDPD.pPresentationParameters.MultiSampleType]);
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

      // redirect to windows Direct3D
      g_EmuCDPD.hRet := IDirect3D8_CreateDevice
      (g_pD3D8,
        g_EmuCDPD.Adapter,
        g_EmuCDPD.DeviceType,
        g_EmuCDPD.hFocusWindow,
        g_EmuCDPD.BehaviorFlags,
        g_EmuCDPD.pPresentationParameters,
        PIDirect3DDevice8(g_EmuCDPD.ppReturnedDeviceInterface)
      );

      // report error
      if (FAILED(g_EmuCDPD.hRet)) then
      begin
        // Dxbx TODO : Use DXGetErrorDescription(g_EmuCDPD.hRet); (requires another DLL though)
        if (g_EmuCDPD.hRet = D3DERR_INVALIDCALL) then
          CxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Invalid Call)')
        else if (g_EmuCDPD.hRet = D3DERR_NOTAVAILABLE) then
          CxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Not Available)')
        else if (g_EmuCDPD.hRet = D3DERR_OUTOFVIDEOMEMORY) then
          CxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Out of Video Memory)');

        CxbxKrnlCleanup('IDirect3D8.CreateDevice failed (Unknown)');
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
          CxbxKrnlCleanup('Could not initialize DirectDraw7');

        hRet := IDirectDraw7(g_pDD7).SetCooperativeLevel(0, DDSCL_NORMAL);
        if (FAILED(hRet)) then
          CxbxKrnlCleanup('Could not set cooperative level');
      end;

      // check for YUY2 overlay support Cxbx TODO : accept other overlay types
      begin
        dwCodes := 0;
        lpCodes := nil;
        IDirectDraw7(g_pDD7).GetFourCCCodes({var}dwCodes, lpCodes);
        lpCodes := CxbxMalloc(dwCodes*sizeof(DWORD));
        IDirectDraw7(g_pDD7).GetFourCCCodes({var}dwCodes, lpCodes);

        g_bSupportsYUY2 := false;
        for v := 0 to dwCodes - 1 do
        begin
          if (PDWORDs(lpCodes)[v] = MAKEFOURCC('Y', 'U', 'Y', '2')) then
          begin
            g_bSupportsYUY2 := true;
            Break;
          end;
        end;

        CxbxFree(lpCodes);

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

        hRet := IDirectDraw7(g_pDD7).CreateSurface(ddsd2, {out}PIDirectDrawSurface7(g_pDDSPrimary), nil);

        if (FAILED(hRet)) then
          CxbxKrnlCleanup('Could not create primary surface (0x%.08X)', [hRet]);
      end;

      // update render target cache
      New({var}g_pCachedRenderTarget);
      g_pCachedRenderTarget.Common := 0;
      g_pCachedRenderTarget.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_D3DREND;

      // Dxbx Note: Because g_pCachedRenderTarget.EmuSurface8 must be declared
      // as a property, we can't pass it directly as a var/out parameter.
      // So we use a little work-around here :
      IDirect3DDevice8_GetRenderTarget(g_pD3DDevice8, @(g_pCachedRenderTarget.Lock{EmuSurface8}));

      // update z-stencil surface cache
      New({var}g_pCachedZStencilSurface);
      g_pCachedZStencilSurface.Common := 0;
      g_pCachedZStencilSurface.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_D3DSTEN;

      // Dxbx Note: Because g_pCachedZStencilSurface.EmuSurface8 must be declared
      // as a property, we can't pass it directly as a var/out parameter.
      // So we use a little work-around here :
      IDirect3DDevice8_GetDepthStencilSurface(g_pD3DDevice8, @(g_pCachedZStencilSurface.Lock{EmuSurface8}));

      IDirect3DDevice8(g_pD3DDevice8).CreateVertexBuffer(
        {Length=}1,
        {Usage=}0,
        {FVF=}0,
        {Pool=}D3DPOOL_MANAGED,
        @g_pDummyBuffer
        );

      for Streams := 0 to 8-1 do
      begin
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
    else
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
          PIDirectDrawSurface7(g_pDDSPrimary)._Release();
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
  end; // while

  Result := D3D_OK;
end; // EmuCreateDeviceProxy

// check if a resource has been registered yet (if not, register it)
procedure EmuVerifyResourceIsRegistered(pResource: PX_D3DResource); //inline;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  v: int;
begin
  // 0xEEEEEEEE and 0xFFFFFFFF are somehow set in Halo :(
  if (pResource.Lock <> 0) and (pResource.Lock <> $EEEEEEEE) and (pResource.Lock <> $FFFFFFFF) then
    Exit;

  // Already 'Registered' implicitly
  if (    IsSpecialResource(pResource.Data)
      and (   ((pResource.Data and X_D3DRESOURCE_DATA_FLAG_D3DREND) > 0)
           or ((pResource.Data and X_D3DRESOURCE_DATA_FLAG_D3DSTEN) > 0)))
  or (pResource.Data = $B00BBABE) then
    Exit;

  for v := 0 to 16-1 do
  begin
    if (pCache[v].Data = pResource.Data) and (pResource.Data <> 0) then
    begin
      pResource.EmuResource8 := pCache[v].EmuResource8;
      Exit;
    end;
  end;

  EmuSwapFS(fsXbox);
  XTL_EmuIDirect3DResource8_Register(pResource, nil {(PVOID)pResource.Data});
  EmuSwapFS(fsWindows);

  if (pResource.Lock <> X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
  begin
    for v := 0 to 16 - 1 do
    begin
      if (pCache[v].Data = 0) then
      begin
        pCache[v].Data := pResource.Data;
        pCache[v].EmuResource8 := pResource.EmuResource8;
        break;
      end;

      if (v = 15) then // Dxbx note : Cxbx uses 16 here, but that's just plain wrong!
        CxbxKrnlCleanup('X_D3DResource cache is maxed out!');
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

function XTL_EmuIDirect3DDevice8_IsBusy: {LONG?}BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_IsBusy();');
{$ENDIF}

  EmuWarning('EmuIDirect3DDevice8_IsBusy ignored!');

  EmuSwapFS(fsXbox);

  Result := FALSE;
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

  if (Ord(RType) > 7) then
    CxbxKrnlCleanup('RType > 7');

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

  pFieldStatus.Field := X_D3DFIELDTYPE(iif(g_VBData.VBlank and 1 = 0, Ord(X_D3DFIELD_ODD), Ord(X_D3DFIELD_EVEN)));
  pFieldStatus.VBlankCount := g_VBData.VBlank;

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
    #13#10');)',
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

  // Cxbx TODO : actually emulate this!?

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
    #13#10'   pCaps                   : 0x%.08X' +
    #13#10');',
    [pCaps]);
{$ENDIF}

  IDirect3D8(g_pD3D8).GetDeviceCaps(g_XBVideo.GetDisplayAdapter(), iif(g_XBVideo.GetDirect3DDevice = 0, D3DDEVTYPE_HAL, D3DDEVTYPE_REF), pCaps^);

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

  if (Address < 136) and VshHandleIsVertexShader(Handle) then
  begin
    pVertexShader := PVERTEX_SHADER(VshHandleGetVertexShader(Handle).Handle);
    for i := Address to pVertexShader.Size - 1 do
    begin
      // Cxbx TODO : This seems very fishy
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
    pVertexShader := PVERTEX_SHADER((PX_D3DVertexShader(Handle and $7FFFFFFF)).Handle);
    IDirect3DDevice8(g_pD3DDevice8).SetVertexShader(pVertexShader.Handle);
  end
  else if (Handle = 0) then
  begin
    IDirect3DDevice8(g_pD3DDevice8).SetVertexShader(D3DFVF_XYZ or D3DFVF_TEX0);
  end
  else if (Address < 136) then
  begin
    pVertexShader2 := PX_D3DVertexShader(g_VertexShaderSlots[Address]);

    if (pVertexShader2 <> NULL) then
    begin
      IDirect3DDevice8(g_pD3DDevice8).SetVertexShader(PVERTEX_SHADER(PX_D3DVertexShader(g_VertexShaderSlots[Address]).Handle).Handle);
    end
    else
    begin
        EmuWarning('g_VertexShaderSlots[%d] := 0', [Address]);
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
  for v := 0 to ret - 1 do
  begin
    if (IDirect3D8(g_pD3D8).EnumAdapterModes(g_XBVideo.GetDisplayAdapter, v, {out}Mode) <> D3D_OK) then
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

    // Cxbx TODO : Make this configurable in the future?
    // D3DPRESENTFLAG_FIELD | D3DPRESENTFLAG_INTERLACED | D3DPRESENTFLAG_LOCKABLE_BACKBUFFER
    pMode.Flags := $000000A1;

    // Cxbx TODO : Retrieve from current CreateDevice settings?
    pMode.Width := 640;
    pMode.Height := 480;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3D8_EnumAdapterModes
(
  Adapter: UINT;
  Mode: UINT;
  pMode: PX_D3DDISPLAYMODE
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
{$WRITEABLECONST ON}
const
  ModeAdder: int = 0;
{$WRITEABLECONST OFF}
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

    // Cxbx TODO : Make this configurable in the future?
    // D3DPRESENTFLAG_FIELD | D3DPRESENTFLAG_INTERLACED | D3DPRESENTFLAG_LOCKABLE_BACKBUFFER
    pMode.Flags := $000000A1;
    pMode.Format := EmuPC2XB_D3DFormat(PCMode.Format)
  end
  else
    Result := D3DERR_INVALIDCALL;

  EmuSwapFS(fsXbox);
end; // XTL_EmuIDirect3D8_EnumAdapterModes

procedure XTL_EmuIDirect3D8_KickOffAndWaitForIdle(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_KickOffAndWaitForIdle();');
{$ENDIF}

  // Cxbx TODO : Actually do something here?

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3D8_KickOffAndWaitForIdle2(dwDummy1, dwDummy2: DWORD); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3D8_KickOffAndWaitForIdle' +
    #13#10'(' +
    #13#10'   dwDummy1          : 0x%.08X' +
    #13#10'   dwDummy2          : 0x%.08X' +
    #13#10');',
    [dwDummy1, dwDummy2]);
{$ENDIF}

  // Cxbx TODO : Actually do something here?

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

  for v := 0 to 255 - 1 do
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
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_AddRef()');
{$ENDIF}

  Result := ULong(IDirect3DDevice8(g_pD3DDevice8)._AddRef());

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

  CxbxKrnlCleanup('BeginStateBig is not implemented');
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
  FileName: array [0..255 - 1] of Char;}
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

  IDirect3DSurface8(pSourceSurface.EmuSurface8).UnlockRect();

  { MARKED OUT BY CXBX
  kthx := 0;
  StrFmt(FileName, 'C:\Aaron\Textures\SourceSurface-%d.bmp', kthx++);

  D3DXSaveSurfaceToFile(FileName, D3DXIFF_BMP, pSourceSurface.EmuSurface8, nil, nil);
  }

  Result := IDirect3DDevice8(g_pD3DDevice8).CopyRects
  (
    IDirect3DSurface8(pSourceSurface.EmuSurface8),
    pSourceRectsArray,
    cRects,
    IDirect3DSurface8(pDestinationSurface.EmuSurface8),
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
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
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
    PIDirect3DSurface8(@(ppBackBuffer^.Lock{EmuSurface8})));

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

function XTL_EmuIDirect3DDevice8_GetBackBuffer2
(
    BackBuffer: INT
): PX_D3DSurface; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pBackBuffer: PX_D3DSurface;
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetBackBuffer2' +
    #13#10'(' +
    #13#10'   BackBuffer        : 0x%.08X' +
    #13#10');',
    [BackBuffer]);
{$ENDIF}

{ unsafe, somehow  -- MARKED OUT BY CXBX --
    HRESULT hRet := S_OK;

    X_D3DSurface *pBackBuffer := new X_D3DSurface();

    if (BackBuffer = -1) then
    begin
         {static?}{pCachedPrimarySurface: XTL_PIDirect3DSurface8 := nil;

        if (pCachedPrimarySurface = 0) then
        begin
            // create a buffer to return
            // Cxbx TODO : Verify the surface is always 640x480
            IDirect3DDevice8(g_pD3DDevice8).CreateImageSurface(640, 480, D3DFMT_A8R8G8B8, {out}{IDirect3DSurface8(pCachedPrimarySurface));
         end;

        pBackBuffer.EmuSurface8 := pCachedPrimarySurface;

        hRet := IDirect3DDevice8(g_pD3DDevice8).GetFrontBuffer(pBackBuffer.EmuSurface8);

        if (FAILED(hRet)) then
        begin
            EmuWarning('Could not retrieve primary surface, using backbuffer');
            pCachedPrimarySurface := 0;
            pBackBuffer.EmuSurface8._Release();
            pBackBuffer.EmuSurface8 := nil;
            BackBuffer := 0;
         end;

        // Debug: Save this image temporarily
        //D3DXSaveSurfaceToFile('C:\\Aaron\\Textures\\FrontBuffer.bmp', D3DXIFF_BMP, pBackBuffer.EmuSurface8, NULL, NULL);
     end;

    if (BackBuffer <> -1) then
        hRet := IDirect3DDevice8_GetBackBuffer(g_pD3DDevice8, BackBuffer, D3DBACKBUFFER_TYPE_MONO, @(pBackBuffer.EmuSurface8{));
}

  New({var PX_D3DSurface}pBackBuffer);

  if (BackBuffer = -1) then
      BackBuffer := 0;

  hRet := IDirect3DDevice8_GetBackBuffer(g_pD3DDevice8, BackBuffer, D3DBACKBUFFER_TYPE_MONO, PIDirect3DSurface8(@(pBackBuffer^.Lock{EmuSurface8})));

  if (FAILED(hRet)) then
    CxbxKrnlCleanup('Unable to retrieve back buffer');

  // update data pointer
  pBackBuffer.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_SURFACE;

  EmuSwapFS(fsXbox);

  Result := pBackBuffer;
end;

procedure XTL_EmuIDirect3DDevice8_GetBackBuffer
(
  BackBuffer: INT;
  Type_: D3DBACKBUFFER_TYPE;
  ppBackBuffer: PPX_D3DSurface
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetBackBuffer' +
      #13#10'(' +
      #13#10'   BackBuffer        : 0x%.08X' +
      #13#10'   Type              : 0x%.08X' +
      #13#10'   ppBackBuffer      : 0x%.08X' +
      #13#10');',
      [BackBuffer, Ord(Type_), ppBackBuffer]);
    EmuSwapFS(fsXbox);
  end;
{$ENDIF}

  ppBackBuffer^ := XTL_EmuIDirect3DDevice8_GetBackBuffer2(BackBuffer);
end;

function XTL_EmuIDirect3DDevice8_SetViewport
(
  pViewport: PD3DVIEWPORT8
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  dwWidth: DWORD;
  dwHeight: DWORD;
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

  dwWidth := pViewport.Width;
  dwHeight := pViewport.Height;

  // resize to fit screen (otherwise crashes occur)
  begin
    if (dwWidth > 640) then
    begin
      EmuWarning('Resizing Viewport.Width to 640');
      pViewport.Width := 640;
    end;

    if (dwHeight > 480) then
    begin
      EmuWarning('Resizing Viewport.Height to 480');
      pViewport.Height := 480;
    end;
  end;

  Result := IDirect3DDevice8(g_pD3DDevice8).SetViewport(pViewport^);

  // restore originals
  begin
    if (dwWidth > 640) then
      pViewport.Width := dwWidth;

    if (dwHeight > 480) then
      pViewport.Height := dwHeight;
  end;

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

  pSurface8 := g_pCachedRenderTarget.EmuSurface8;

  IDirect3DSurface8(pSurface8)._AddRef();

  ppRenderTarget^ := g_pCachedRenderTarget;

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : RenderTarget := 0x%.08X', [pSurface8]);
{$ENDIF}

  // Dxbx TODO : Should we nil-out pSurface ?

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
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetRenderTarget2()');
{$ENDIF}

  pSurface8 := g_pCachedRenderTarget.EmuSurface8;

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

  pSurface8 := g_pCachedZStencilSurface.EmuSurface8;

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
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetDepthStencilSurface2()');
{$ENDIF}

  pSurface8 := g_pCachedZStencilSurface.EmuSurface8;

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
    memcpy(pTile, @(EmuD3DTileCache[Index]), sizeof(X_D3DTILE));

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
    memcpy(@(EmuD3DTileCache[Index]), pTile, sizeof(X_D3DTILE));

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
end;

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

  pRecompiledBuffer: LPD3DXBUFFER;
  pRecompiledDeclaration: PDWORD;
  pRecompiledFunction: PDWORD;
  VertexShaderSize: DWORD;
  DeclarationSize: DWORD;
  Handle: DWORD;

(*  pFileName: array [0..30-1] of Char;*)
(*  FailedShaderCount: Integer;*)
const
  dummy: AnsiString =
    'vs.1.1'#13#10 +
    'mov oPos, v0'#13#10;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreateVertexShader' +
    #13#10'(' +
    #13#10'   pDeclaration      : 0x%.08X' +
    #13#10'   pFunction         : 0x%.08X' +
    #13#10'   pHandle           : 0x%.08X' +
    #13#10'   Usage             : 0x%.08X' +
    #13#10');',
    [pDeclaration, pFunction, pHandle, Usage]);
{$ENDIF}

  // create emulated shader struct
  pD3DVertexShader := PX_D3DVertexShader(CxbxMalloc(sizeof(X_D3DVertexShader)));
  pVertexShader := PVERTEX_SHADER(CxbxMalloc(sizeof(VERTEX_SHADER)));

  // Cxbx TODO : Intelligently fill out these fields as necessary
  ZeroMemory(pD3DVertexShader, sizeof(X_D3DVertexShader));
  ZeroMemory(pVertexShader, sizeof(VERTEX_SHADER));

  // HACK: Cxbx TODO : support this situation
  if (pDeclaration = NULL) then
  begin
    pHandle^ := 0;
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
                                        g_VertexShaderConstantMode = X_VSCM_NONERESERVED);
    if (SUCCEEDED(hRet)) then
    begin
      pRecompiledFunction := PDWORD(pRecompiledBuffer.GetBufferPointer());
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
    hRet := IDirect3DDevice8(g_pD3DDevice8).CreateVertexShader
    (
        pRecompiledDeclaration,
        pRecompiledFunction,
        {out}Handle,
        g_dwVertexShaderUsage   // Cxbx TODO : HACK: Xbox has extensions!
    );
    if Assigned(pRecompiledBuffer) then
    begin
      pRecompiledBuffer._Release();
      pRecompiledBuffer := NULL;
    end;

    //* Fallback to dummy shader.
    if (FAILED(hRet)) then
    begin
      EmuWarning('Trying fallback:'#13#10'%s', [dummy]);
      hRet := D3DXAssembleShader(PAnsiChar(dummy),
                                 Length(dummy),
                                 D3DXASM_SKIPVALIDATION,
                                 NULL,
                                 @pRecompiledBuffer,
                                 NULL);
      if not (FAILED(hRet)) then
        hRet := IDirect3DDevice8(g_pD3DDevice8).CreateVertexShader
        (
            pRecompiledDeclaration,
            PDWORD(pRecompiledBuffer.GetBufferPointer()),
            {out}Handle,
            g_dwVertexShaderUsage
        );
    end;
    //*/
  end;

  // Save the status, to remove things later
  pVertexShader.Status := hRet;

  CxbxFree(pRecompiledDeclaration);

  pVertexShader.pDeclaration := PDWORD(CxbxMalloc(DeclarationSize));
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
      pVertexShader.pFunction := PDWORD(CxbxMalloc(VertexShaderSize));
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
      VSH_SHADER_HEADER *pHeader := (VSH_SHADER_HEADER)pFunction;
      EmuWarning('Couldn't create vertex shader!');
      sprintf(pFileName, 'failed%05d.xvu', FailedShaderCount);
      PFILE f := fopen(pFileName, 'wb');
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
  i: integer;
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
  for i := 0 to ConstantCount - 1 do
  begin
   {$IFDEF DEBUG}
      printf('SetVertexShaderConstant, c%d (c%d) = { %f, %f, %f, %f }',
             [Register_ - 96 + i, Register_ + i,
             (pConstantData + 4 * i),
             (pConstantData + 4 * i + 1),
             (pConstantData + 4 * i + 2),
             (pConstantData + 4 * i + 3)]);
   {$ENDIF}
  end;
{$ENDIF}

  // Cxbx TODO: HACK: Since Xbox vertex shader constants range from -96 to 96, during conversion
  // some shaders need to add 96 to use ranges 0 to 192.  This fixes 3911 - 4361 games and XDK
  // samples, but breaks Turok.
  if g_BuildVersion <= 4361 then
    Inc(Register_, 96);

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

// Dxbx: This argument makes the 'register' calling convention
// functionally equivalent to the 'fastcall' calling convention.
// Quote from http://www.codeguru.com/forum/showthread.php?t=466266 :
// They differ as follows:
// register: (left to right) EAX, EDX, ECX, remaining pushed on stack right to left, callee cleans
// fastcall: (left to right) ECX, EDX, remaining pushed on stack left to right, callee cleans
procedure XTL_EmuIDirect3DDevice8_SetVertexShaderConstant1(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}const pConstantData: PVOID; // Dxbx note: This argument should be here, to force it into EDX
  {1 ECX}Register_: INT // Dxbx note: The first argument should be here, to force it into ECX
  ); register; // __fastcall in Cxbx
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

// Dxbx: This argument makes the 'register' calling convention
// functionally equivalent to the 'fastcall' calling convention.
// Quote from http://www.codeguru.com/forum/showthread.php?t=466266 :
// They differ as follows:
// register: (left to right) EAX, EDX, ECX, remaining pushed on stack right to left, callee cleans
// fastcall: (left to right) ECX, EDX, remaining pushed on stack left to right, callee cleans
procedure XTL_EmuIDirect3DDevice8_SetVertexShaderConstant4(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}const pConstantData: PVOID; // Dxbx note: This argument should be here, to force it into EDX
  {1 ECX}Register_: INT // Dxbx note: The first argument should be here, to force it into ECX
  ); register; // __fastcall in Cxbx
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

// Dxbx: This argument makes the 'register' calling convention
// functionally equivalent to the 'fastcall' calling convention.
// Quote from http://www.codeguru.com/forum/showthread.php?t=466266 :
// They differ as follows:
// register: (left to right) EAX, EDX, ECX, remaining pushed on stack right to left, callee cleans
// fastcall: (left to right) ECX, EDX, remaining pushed on stack left to right, callee cleans
procedure XTL_EmuIDirect3DDevice8_SetVertexShaderConstantNotInline(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}const pConstantData: PVOID; // Dxbx note: This argument should be here, to force it into EDX
  {1 ECX}Register_: INT; // Dxbx note: This argument should be here, to force it into ECX
  {3 stack}ConstantCount: DWORD // Dxbx note: This argument should be here, to force it into the first stack-slot
  ); register; // __fastcall in Cxbx
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

function XTL_EmuIDirect3DDevice8_SetPixelShader
(
  Handle: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
{$WRITEABLECONST ON}
const
  dwHandle: DWORD = 0;
{$WRITEABLECONST OFF}
const
  szDiffusePixelShader: AnsiString =
    #13#10'ps.1.0'#13#10 +
    #13#10'tex t0'#13#10 +
    #13#10'mov r0, t0'#13#10;
var
  pShader: LPD3DXBUFFER;
  pErrors: LPD3DXBUFFER;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetPixelShader' +
    #13#10'(' +
    #13#10'   Handle           : 0x%.08X' +
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
      D3DXAssembleShader(PAnsiChar(szDiffusePixelShader), Length(szDiffusePixelShader) - 1, 0, nil, @pShader, @pErrors);

      // create the shader device handle
      Result := IDirect3DDevice8(g_pD3DDevice8).CreatePixelShader(pShader.GetBufferPointer(), {out}dwHandle);

      if (FAILED(Result)) then
      begin
        EmuWarning('Could not create pixel shader');
        EmuWarning(string(AnsiString(PAnsiChar(pErrors.GetBufferPointer))));
      end;
    end;

    if (not FAILED(Result)) then
      Result := IDirect3DDevice8(g_pD3DDevice8).SetPixelShader(dwHandle);

    if (FAILED(Result)) then
      EmuWarning('Could not set pixel shader!');
    //*/

    g_bFakePixelShaderLoaded := TRUE;
  end
  // Fixed Pipeline, or Recompiled Programmable Pipeline
  else if (Handle = 0) then
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
    D3DResource: D3DRESOURCETYPE
): PX_D3DResource; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pTexture: PX_D3DTexture;
begin
  case Ord(D3DResource) of
     3: //D3DRTYPE_TEXTURE
        XTL_EmuIDirect3DDevice8_CreateTexture(Width, Height, Levels, Usage, Format, D3DPOOL_MANAGED, @pTexture);
     4: //D3DRTYPE_VOLUMETEXTURE
        XTL_EmuIDirect3DDevice8_CreateVolumeTexture(Width, Height, Depth, Levels, Usage, Format, D3DPOOL_MANAGED, @pTexture);
     5: //D3DRTYPE_CUBETEXTURE
        CxbxKrnlCleanup('Cube textures temporarily not supported!');
  else
    CxbxKrnlCleanup('D3DResource := %d is not supported!', [Ord(D3DResource)]);
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
     #13#10'   Width             : 0x%.08X' +
     #13#10'   Height            : 0x%.08X' +
     #13#10'   Levels            : 0x%.08X' +
     #13#10'   Usage             : 0x%.08X' +
     #13#10'   Format            : 0x%.08X' +
     #13#10'   Pool              : 0x%.08X' +
     #13#10'   ppTexture         : 0x%.08X' +
     #13#10');',
     [Width, Height, Levels, Usage, Ord(Format), Ord(Pool), ppTexture]);
{$ENDIF}

  // Convert Format (Xbox->PC)
  PCFormat := EmuXB2PC_D3DFormat(Format);

  // Cxbx TODO : HACK: Devices that don't support this should somehow emulate it!
  if (PCFormat = D3DFMT_D16) then
  begin
    EmuWarning('D3DFMT_D16 is an unsupported texture format!');
    PCFormat := D3DFMT_R5G6B5;
  end
  else
  if (PCFormat = D3DFMT_P8) then
  begin
    EmuWarning('D3DFMT_P8 is an unsupported texture format!');
    PCFormat := D3DFMT_X8R8G8B8;
  end
  else
  if (PCFormat = D3DFMT_D24S8) then
  begin
    EmuWarning('D3DFMT_D24S8 is an unsupported texture format!');
    PCFormat := D3DFMT_X8R8G8B8;
  end
  else
  if (PCFormat = D3DFMT_YUY2) then
  begin
    // cache the overlay size
    g_dwOverlayW := Width;
    g_dwOverlayH := Height;
    g_dwOverlayP := RoundUp(g_dwOverlayW, 64) * 2;
  end;


  if (PCFormat <> D3DFMT_YUY2) then
  begin
    PCUsage := Usage and (D3DUSAGE_RENDERTARGET);
// //Cxbx    PCUsage := Usage and (D3DUSAGE_RENDERTARGET or D3DUSAGE_DEPTHSTENCIL);
    PCPool := D3DPOOL_MANAGED;

    EmuAdjustPower2(@Width, @Height);

    New({var PX_D3DTexture}ppTexture^);

// //Cxbx    if (Usage and (D3DUSAGE_RENDERTARGET or D3DUSAGE_DEPTHSTENCIL)) > 0 then
    if (Usage and (D3DUSAGE_RENDERTARGET)) > 0 then
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

    Result := IDirect3DDevice8_CreateTexture(g_pD3DDevice8,
      Width, Height, Levels,
      PCUsage, // Cxbx TODO : Xbox Allows a border to be drawn (maybe hack this in software ;[)
      PCFormat, PCPool, @(ppTexture^.Lock{EmuTexture8})
    );

    if (FAILED(Result)) then
    begin
      EmuWarning('CreateTexture Failed!');
      ppTexture^.Data := $BEADBEAD;
    end
    else
    begin
      IDirect3DTexture8(ppTexture^.EmuTexture8).LockRect(0, {out}LockedRect, NULL, 0);

      ppTexture^.Data := DWORD(LockedRect.pBits);
      ppTexture^.Format := Ord(Format) shl X_D3DFORMAT_FORMAT_SHIFT;

      g_DataToTexture.insert(ppTexture^.Data, ppTexture^);

      IDirect3DTexture8(ppTexture^.EmuTexture8).UnlockRect(0);
    end;

{$IFDEF DEBUG}
    DbgPrintf('EmuD3D8 : Created Texture: 0x%.08X (0x%.08X)', [ppTexture^, ppTexture^.EmuTexture8]);
{$ENDIF}
  end
  else
  begin
    dwSize := g_dwOverlayP * g_dwOverlayH;
    dwPtr := DWORD(CxbxMalloc(dwSize + sizeof(DWORD)));

    pRefCount := PDWORD(dwPtr + dwSize);

    // initialize ref count
    pRefCount^ := 1;

    // If YUY2 is not supported in hardware, we'll actually mark this as a special fake texture (set highest bit)
    New({var PX_D3DTexture}ppTexture^);

    ppTexture^.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_YUVSURF;
    ppTexture^.Lock := dwPtr;
    ppTexture^.Format := $24;

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
  PCFormat := EmuXB2PC_D3DFormat(Cardinal(Format));

  // Cxbx TODO : HACK: Devices that don't support this should somehow emulate it!
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
        0,  // Cxbx TODO : Xbox Allows a border to be drawn (maybe hack this in software ;[)
        PCFormat, D3DPOOL_MANAGED, @(ppVolumeTexture^.Lock{EmuVolumeTexture8}));

    if (FAILED(hRet)) then
        EmuWarning('CreateVolumeTexture Failed! (0x%.08X)', [hRet]);

{$IFDEF DEBUG}
    DbgPrintf('EmuD3D8 : Created Volume Texture: 0x%.08X (0x%.08X)', [@ppVolumeTexture, ppVolumeTexture^.EmuVolumeTexture8]);
{$ENDIF}
  end
  else
  begin
    dwSize := g_dwOverlayP * g_dwOverlayH;
    dwPtr := DWORD(CxbxMalloc(dwSize + sizeof(DWORD)));

    pRefCount := PDWORD(dwPtr + dwSize);

    // initialize ref count
    pRefCount^ := 1;

    // If YUY2 is not supported in hardware, we'll actually mark this as a special fake texture (set highest bit)
    ppVolumeTexture^.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_YUVSURF;
    ppVolumeTexture^.Lock := dwPtr;
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
    Format: D3DFORMAT;
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
  PCFormat := EmuXB2PC_D3DFormat(Cardinal(Format));

  // Cxbx TODO : HACK: Devices that don't support this should somehow emulate it!
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
    CxbxKrnlCleanup('YUV not supported for cube textures');
  end;

  New({var PX_D3DCubeTexture}ppCubeTexture^);

  Result := IDirect3DDevice8_CreateCubeTexture(g_pD3DDevice8,
      EdgeLength, Levels,
      0,  // Cxbx TODO : Xbox Allows a border to be drawn (maybe hack this in software ;[)
      PCFormat, D3DPOOL_MANAGED, @(ppCubeTexture^.Lock{EmuCubeTexture8})
  );

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : Created Cube Texture: 0x%.08X (0x%.08X)', [ppCubeTexture^, ppCubeTexture^.EmuCubeTexture8]);
{$ENDIF}

  if (FAILED(Result)) then
      EmuWarning('CreateCubeTexture Failed!');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_CreateIndexBuffer
(
  Length: UINT;
  Usage: DWORD;
  Format: D3DFORMAT;
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
         #13#10'   Length            : 0x%.08X' +
         #13#10'   Usage             : 0x%.08X' +
         #13#10'   Format            : 0x%.08X' +
         #13#10'   Pool              : 0x%.08X' +
         #13#10'   ppIndexBuffer     : 0x%.08X' +
         #13#10');',
         [Length, Usage, Ord(Format), Ord(Pool), ppIndexBuffer]);
{$ENDIF}

  New({var PX_D3DIndexBuffer}ppIndexBuffer^);

  hRet := IDirect3DDevice8_CreateIndexBuffer(g_pD3DDevice8,
      Length, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED,
      @(ppIndexBuffer^.Lock{EmuIndexBuffer8}));

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIndexBuffer8 := 0x%.08X', [ppIndexBuffer^.EmuIndexBuffer8]);
{$ENDIF}

  if (FAILED(hRet)) then
      EmuWarning('CreateIndexBuffer Failed! (0x%.08X)', [hRet]);

  //
  // update data ptr
  //
  begin
    pData := NULL;

    IDirect3DIndexBuffer8(ppIndexBuffer^.EmuIndexBuffer8).Lock(0, Length, {out PByte}pData, 0);

    ppIndexBuffer^.Data := DWORD(pData);
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
      D3DFMT_INDEX16, 
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
      #13#10'   pIndexData        : 0x%.08X' +
      #13#10'   BaseVertexIndex   : 0x%.08X' +
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
      asm int 3
    end;
  end;
  }

  Result := D3D_OK;

  if (pIndexData <> NULL) then
    DbgPrintf('EmuIDirect3DDevice8_SetIndcies(): pIndexData->EmuIndexBuffer8:= 0x%.08X', [pIndexData.EmuIndexBuffer8]);

  if (pIndexData <> nil) then
  begin
    g_pIndexBuffer := pIndexData;
    g_dwBaseVertexIndex := BaseVertexIndex;

    // HACK: Halo Hack
    if (pIndexData.Lock = $00840863) then
        pIndexData.Lock := 0;

    EmuVerifyResourceIsRegistered(pIndexData);

    // HACK: Unreal Championship
    if ((pIndexData.Lock and $FFFF0000) = $00490000) {or (pIndexData.Lock = $490046)} or (pIndexData.Lock = $10) then
    begin
      Result := E_FAIL;
      goto fail;
    end;

    pIndexBuffer := pIndexData.EmuIndexBuffer8;

    if (pIndexData.Lock <> X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
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

function XTL_EmuIDirect3DDevice8_SetTexture
(
    Stage: DWORD;
    pTexture: PX_D3DResource
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pBaseTexture8: XTL_PIDirect3DBaseTexture8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetTexture' +
    #13#10'(' +
    #13#10'   Stage             : 0x%.08X' +
    #13#10'   pTexture          : 0x%.08X' +
    #13#10');',
    [Stage, pTexture]);
{$ENDIF}

  pBaseTexture8 := NULL;

  EmuD3DActiveTexture[Stage] := pTexture;

  if (pTexture <> NULL) then
  begin
    EmuVerifyResourceIsRegistered(pTexture);

    if IsSpecialResource(pTexture.Data) and ((pTexture.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0) then
    begin
      //
      // NOTE: Cxbx TODO : This is almost a hack!
      //

      EmuSwapFS(fsXbox);
      XTL_EmuIDirect3DDevice8_EnableOverlay(TRUE);
      XTL_EmuIDirect3DDevice8_UpdateOverlay(PX_D3DSurface(pTexture), nil, nil, FALSE, 0);
      EmuSwapFS(fsWindows);
    end
    else
    begin
      pBaseTexture8 := pTexture.EmuBaseTexture8;

      {$ifdef _DEBUG_DUMP_TEXTURE_SETTEXTURE}
      if (pTexture <> NULL) and (pTexture.EmuTexture8 <> NULL) then
      begin
        int dwDumpTexture := 0; 

        szBuffer: array [0..256-1] of AnsiChar;

        case (pTexture.EmuResource8.GetType()) of
        begin
          D3DRTYPE_TEXTURE:
          begin
            sprintf(szBuffer, 'SetTextureNorm - %.03d (0x%.08X).bmp', [dwDumpTexture++, pTexture.EmuTexture8]);

            pTexture.EmuTexture8.UnlockRect(0);

            D3DXSaveTextureToFile(szBuffer, D3DXIFF_BMP, pTexture.EmuTexture8, NULL);
          end;

          D3DRTYPE_CUBETEXTURE:
          begin
            for(int face:=0;face<6;face++)
            begin
              sprintf(szBuffer, 'SetTextureCube%d - %.03d (0x%.08X).bmp', [face, dwDumpTexture++, pTexture.EmuTexture8]);

              pTexture.EmuCubeTexture8.UnlockRect(D3DCUBEMAP_FACES(face), 0);

              D3DXSaveTextureToFile(szBuffer, D3DXIFF_BMP, pTexture.EmuTexture8, NULL);
            end;
          end;
        end;
      end;
      {$endif}
    end;

  end;


    { --- MARKED OUT BY CXBX --
    IDirect3DTexture8 *pDummyTexture[4] := (0, 0, 0, 0);

    if (pDummyTexture[Stage] = 0) then
    begin
        if (Stage = 0) then
        begin
            if (D3DXCreateTextureFromFile(g_pD3DDevice8, 'C:\dummy1.bmp', @pDummyTexture[Stage]) <> D3D_OK) then
                CxbxKrnlCleanup('Could not create dummy texture!');
        end
        else if (Stage = 1) then
        begin
            if (D3DXCreateTextureFromFile(g_pD3DDevice8, 'C:\dummy2.bmp', @pDummyTexture[Stage]) <> D3D_OK) then
                CxbxKrnlCleanup('Could not create dummy texture!');
        end;
     end;
    }

  { -- MARKED OUT BY CXBX
  int dwDumpTexture := 0;
  szBuffer: array [0..256-1] of AnsiChar;
  StrFmt(szBuffer, 'C:\Aaron\Textures\DummyTexture - %.03d (0x%.08X).bmp', dwDumpTexture++, pDummyTexture);
  pDummyTexture.UnlockRect(0);
  D3DXSaveTextureToFile(szBuffer, D3DXIFF_BMP, pDummyTexture, 0);
  }

  // hRet = IDirect3DDevice8(g_pD3DDevice8).SetTexture(Stage, pDummyTexture[Stage]); // MARKED OUT BY CXBX
  Result := IDirect3DDevice8(g_pD3DDevice8).SetTexture(Stage, IDirect3DBaseTexture8(iif((g_iWireframe = 0), pBaseTexture8, nil)));

  EmuSwapFS(fsXbox);
end;

// Dxbx: This argument makes the 'register' calling convention
// functionally equivalent to the 'fastcall' calling convention.
// Quote from http://www.codeguru.com/forum/showthread.php?t=466266 :
// They differ as follows:
// register: (left to right) EAX, EDX, ECX, remaining pushed on stack right to left, callee cleans
// fastcall: (left to right) ECX, EDX, remaining pushed on stack left to right, callee cleans
procedure XTL_EmuIDirect3DDevice8_SwitchTexture
(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Data: DWORD; // Dxbx note: This argument should be here, to force it into EDX
  {1 ECX}Method: DWORD; // Dxbx note: This argument should be here, to force it into ECX
  {3 stack}Format: DWORD // Dxbx note: This argument should be here, to force it into the first stack-slot
); register; // __fastcall in Cxbx
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

  Stage := High(Stage); //  Dxbx note : was -1, but that violated subrange bound;

  for v := 0 to 4-1 do
    if (StageLookup[v] = Method) then
      Stage := v;

  if (Stage = High(Stage)) then
  begin
    EmuWarning('Unknown Method (0x%.08X)', [Method]);
  end
  else
  begin
    // WARNING: Cxbx TODO : Correct reference counting has not been completely verified for this code

    pTexture := g_DataToTexture.get(Data);

    EmuWarning('Switching Texture 0x%.08X (0x%.08X) @ Stage %d', [pTexture, pTexture.EmuBaseTexture8, Stage]);

    {hRet := }IDirect3DDevice8(g_pD3DDevice8).SetTexture(Stage, IDirect3DBaseTexture8(pTexture.EmuBaseTexture8));

    { MARKED OUT BY CXBX
    if (pTexture.EmuBaseTexture8 <> 0) then
    begin
         Integer dwDumpTexture := 0;

         szBuffer: array [0..255-1] of Char;

        StrFmt(szBuffer, 'C:\Aaron\Textures\0x%.08X-SwitchTexture%.03d.bmp', pTexture, dwDumpTexture++);

        pTexture.EmuTexture8.UnlockRect(0);

        D3DXSaveTextureToFile(szBuffer, D3DXIFF_BMP, pTexture.EmuBaseTexture8, 0);
     end;
    }
  end;

  EmuSwapFS(fsXbox);
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
      #13#10'   pMode             : 0x%.08X' +
      #13#10');',
      [pMode]);
{$ENDIF}


  // make adjustments to parameters to make sense with windows d3d
  begin
    pPCMode := PD3DDISPLAYMODE(pMode);
    Result := IDirect3DDevice8(g_pD3DDevice8).GetDisplayMode({out}pPCMode^);

    // Convert Format (PC->Xbox)
    pMode.Format := EmuPC2XB_D3DFormat(pPCMode.Format);

    // Cxbx TODO : Make this configurable in the future?
    pMode.Flags := $000000A1; // D3DPRESENTFLAG_FIELD | D3DPRESENTFLAG_INTERLACED | D3DPRESENTFLAG_LOCKABLE_BACKBUFFER

    // Cxbx TODO : Retrieve from current CreateDevice settings?
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

  if (g_IVBTable = nil) then
  begin
    g_IVBTable := CxbxMalloc(sizeof(_D3DIVB)*1024);
  end;

  g_IVBTblOffs := 0;
  g_IVBFVF := 0;

  // default values
  ZeroMemory(g_IVBTable, sizeof(_D3DIVB)*1024);

  if (g_pIVBVertexBuffer = nil) then
  begin
    g_pIVBVertexBuffer := CxbxMalloc(sizeof(_D3DIVB)*1024);
  end;

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

  Result := XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_, DWtoF(dwA), DWtoF(dwB), 0.0, 1.0);
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

  case Cardinal(Register_) of
    // Cxbx TODO: Blend weight.

    0: // D3DVSDE_POSITION
      begin
        o := g_IVBTblOffs;
        g_IVBTable[o].Position.x := a;
        g_IVBTable[o].Position.y := b;
        g_IVBTable[o].Position.z := c;
        g_IVBTable[o].Rhw := 1.0;

        Inc(g_IVBTblOffs);

        g_IVBFVF := g_IVBFVF or D3DFVF_XYZRHW;
      end;

    1: // D3DVSDE_BLENDWEIGHT
      begin
        o := g_IVBTblOffs;

        g_IVBTable[o].Position.x := a;
        g_IVBTable[o].Position.y := b;
        g_IVBTable[o].Position.z := c;
        g_IVBTable[o].Blend1 := d;

        Inc(g_IVBTblOffs);

        g_IVBFVF := g_IVBFVF or D3DFVF_XYZB1;
      end;

    2: // D3DVSDE_NORMAL
      begin
        o := g_IVBTblOffs;

        g_IVBTable[o].Normal.x := a;
        g_IVBTable[o].Normal.y := b;
        g_IVBTable[o].Normal.z := c;

        Inc(g_IVBTblOffs);

        g_IVBFVF := g_IVBFVF or D3DFVF_NORMAL;
      end;

   3: // D3DVSDE_DIFFUSE
      begin
        o := g_IVBTblOffs;
        ca := FtoDW(d) shl 24;
        cr := FtoDW(a) shl 16;
        cg := FtoDW(b) shl 8;
        cb := FtoDW(c) shl 0;

        g_IVBTable[o].dwDiffuse := ca or cr or cg or cb;

        g_IVBFVF := g_IVBFVF or D3DFVF_DIFFUSE;
      end;

    4: // D3DVSDE_SPECULAR
      begin
        o := g_IVBTblOffs;
        ca := FtoDW(d) shl 24;
        cr := FtoDW(a) shl 16;
        cg := FtoDW(b) shl 8;
        cb := FtoDW(c) shl 0;

        g_IVBTable[o].dwSpecular := ca or cr or cg or cb;

        g_IVBFVF := g_IVBFVF or D3DFVF_SPECULAR;
      end;

    9: // D3DVSDE_TEXCOORD0
      begin
        o := g_IVBTblOffs;
        g_IVBTable[o].TexCoord1.x := a;
        g_IVBTable[o].TexCoord1.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX1) then
        begin
          g_IVBFVF := g_IVBFVF or D3DFVF_TEX1;
        end;
      end;

    10: // D3DVSDE_TEXCOORD1
      begin
        o := g_IVBTblOffs;
        g_IVBTable[o].TexCoord2.x := a;
        g_IVBTable[o].TexCoord2.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX2) then
        begin
          g_IVBFVF := g_IVBFVF or D3DFVF_TEX2;
        end;
      end;

    11: // D3DVSDE_TEXCOORD2
      begin
        o := g_IVBTblOffs;
        g_IVBTable[o].TexCoord3.x := a;
        g_IVBTable[o].TexCoord3.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX3) then
        begin
          g_IVBFVF := g_IVBFVF or D3DFVF_TEX3;
        end;
      end;

    12: // D3DVSDE_TEXCOORD3
      begin
        o := g_IVBTblOffs;
        g_IVBTable[o].TexCoord4.x := a;
        g_IVBTable[o].TexCoord4.y := b;

        if ((g_IVBFVF and D3DFVF_TEXCOUNT_MASK) < D3DFVF_TEX4) then
        begin
          g_IVBFVF := g_IVBFVF or D3DFVF_TEX4;
        end;
      end;

    $FFFFFFFF:
    begin
      o := g_IVBTblOffs;
      g_IVBTable[o].Position.x := a;
      g_IVBTable[o].Position.y := b;
      g_IVBTable[o].Position.z := c;
      g_IVBTable[o].Rhw := d;

      // Copy current color to next vertex
      g_IVBTable[o+1].dwDiffuse := g_IVBTable[o].dwDiffuse;
      g_IVBTable[o+1].dwSpecular := g_IVBTable[o].dwSpecular;

      g_IVBTblOffs := g_IVBTblOffs + 1; // Dxbx TODO : Use Inc()?

      g_IVBFVF := g_IVBFVF or D3DFVF_XYZRHW;
    end;
  else
    CxbxKrnlCleanup('Unknown IVB Register: %d', [Register_]);
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
  dwA, dwB, dwC, dwD: DWORD;
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

  Result := XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_, DWtoF(dwA), DWtoF(dwB), DWtoF(dwC), DWtoF(dwD));
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

  Result := XTL_EmuIDirect3DDevice8_SetVertexData4f(Register_, DWtoF(dwA), DWtoF(dwB), DWtoF(dwC), DWtoF(dwD));
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
  DbgPrintf('EmuD3D8: EmuIDirect3DDevice8_SetVertexDataColor >>' +
    #13#10'(' +
    #13#10'   Register          : 0x%.08X' +
    #13#10'   Color             : 0x%.08X' +
    #13#10');',
    [Register_, Color]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  a := DWtoF((Color and $FF000000) shr 24);
  r := DWtoF((Color and $00FF0000) shr 16);
  g := DWtoF((Color and $0000FF00) shr 8);
  b := DWtoF((Color and $000000FF) shr 0);

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
    // Cxbx TODO : Should technically clean this up at some point..but on XP doesnt matter much
//    CxbxFree(g_pIVBVertexBuffer);
//    CxbxFree(g_IVBTable);

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
    DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_Clear'+
           #13#10'(' +
           #13#10'   Count             : 0x%.08X' +
           #13#10'   pRects            : 0x%.08X' +
           #13#10'   Flags             : 0x%.08X' +
           #13#10'   Color             : 0x%.08X' +
           #13#10'   Z                 : %f' +
           #13#10'   Stencil           : 0x%.08X' +
           #13#10');',
           [Count, pRects, Flags,
           Color, Z, Stencil]);
{$ENDIF}

  // make adjustments to parameters to make sense with windows d3d
  begin
    // Cxbx TODO : D3DCLEAR_TARGET_A, *R, *G, *B don't exist on windows
    newFlags := 0;

    if (Flags and $000000f0) > 0 then
        newFlags := newFlags or D3DCLEAR_TARGET;

    if (Flags and $00000001) > 0 then
        newFlags := newFlags or D3DCLEAR_ZBUFFER;

    if (Flags and $00000002) > 0 then
        newFlags := newFlags or D3DCLEAR_STENCIL;

    if (Flags and (not ($000000f0 or $00000001 or $00000002))) > 0 then
        EmuWarning('Unsupported Flag(s) for IDirect3DDevice8_Clear: 0x%.08X', [Flags and (not ($000000f0 or $00000001 or $00000002))]);

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
    pBackBuffer := nil;
    IDirect3DDevice8(g_pD3DDevice8).GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, @pBackBuffer);
    IDirect3DSurface8(pBackBuffer).UnlockRect();
  end;

  hRet := IDirect3DDevice8(g_pD3DDevice8).Present(pSourceRect, pDestRect, HWND(pDummy1), pDummy2);

  // not really accurate because you definately dont always present on every vblank
  g_VBData.Swap := g_VBData.VBlank;

  if (g_VBData.VBlank = g_VBLastSwap + 1) then
    g_VBData.Flags := 1 // D3DVBLANK_SWAPDONE
  else
  begin
    g_VBData.Flags := 2; // D3DVBLANK_SWAPMISSED
    Inc(g_SwapData.MissedVBlanks);
  end;

  // Handle Swap Callback function
  begin
    Inc(g_SwapData.Swap);

    if Assigned(g_pSwapCallback{ is func <> NULL}) then
    begin
      EmuSwapFS();  // Xbox FS
      g_pSwapCallback(@g_SwapData);
      EmuSwapFS();  // Win2k/XP FS
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
var
  pBackBuffer: XTL_PIDirect3DSurface8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_Swap' +
    #13#10'(' +
    #13#10'   Flags             : 0x%.08X' +
    #13#10');',
    [Flags]);
{$ENDIF}

  // Cxbx TODO : Ensure this flag is always the same across library versions
  if (Flags <> 0) then
    EmuWarning('XTL.EmuIDirect3DDevice8_Swap: Flags <> 0');

  // release back buffer lock
  begin
    pBackBuffer := nil;
    IDirect3DDevice8(g_pD3DDevice8).GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, @pBackBuffer);

    IDirect3DSurface8(pBackBuffer).UnlockRect();
  end;

  Result := IDirect3DDevice8(g_pD3DDevice8).Present(nil, nil, 0, nil);

  // Handle Swap Callback function
  begin
    Inc(g_SwapData.Swap);

    if Assigned(g_pSwapCallback { is func <> NULL}) then
    begin
      EmuSwapFS();  // Xbox FS
      g_pSwapCallback(@g_SwapData);
      EmuSwapFS();  // Win2k/XP FS
    end;
  end;
  
  g_bHackUpdateSoftwareOverlay := FALSE;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DResource8_Register
(
    pThis: PX_D3DResource; 
    pBase: PVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
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
  bSwizzled: BOOL;
  bCompressed: BOOL;
  dwCompressedSize: DWORD;
  bCubemap: BOOL;
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
  szString: array [0..256 -1] of AnsiChar;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DResource8_Register' +
    #13#10'(' +
    #13#10'   pThis             : 0x%.08X (.Data: 0x%.08X)' +
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
      dwSize := EmuCheckAllocationSize(pBase, true);

      if dwSize = DWORD(-1) then
      begin
        // Cxbx TODO : once this is known to be working, remove the warning
        EmuWarning('Vertex buffer allocation size unknown');
        dwSize := $2000; // temporarily assign a small buffer, which will be increased later
      end;

      hRet := IDirect3DDevice8_CreateVertexBuffer(g_pD3DDevice8,
        dwSize, 0, 0, D3DPOOL_MANAGED,
        {ppVertexBuffer}@(pResource^.Lock{EmuVertexBuffer8}));

      if (FAILED(hRet)) then
      begin
        // Cxbx TODO: Hack for Crazy Taxi 3?
        sprintf(szString, 'CreateVertexBuffer Failed!'#13#10'   VB Size = 0x%X', [dwSize]);

        if ( dwSize <> 0 ) then
          CxbxKrnlCleanup( szString )
        else
        begin
          EmuWarning( szString );
          EmuSwapFS(fsXbox);
          Result := hRet;
          Exit;
        end;
     end;


{$IFDEF _DEBUG_TRACK_VB}
      g_VBTrackTotal.insert(pResource.EmuVertexBuffer8);
{$ENDIF}

      pData := nil;

      hRet := IDirect3DVertexBuffer8(pResource.EmuVertexBuffer8).Lock(0, 0, {out}pData, 0);

      if FAILED(hRet) then
        CxbxKrnlCleanup('VertexBuffer Lock Failed!');

      memcpy(pData, pBase, dwSize);

      IDirect3DVertexBuffer8(pResource.EmuVertexBuffer8).Unlock();

      pResource.Data := ULONG(pData);

{$IFDEF DEBUG}
      DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created VertexBuffer (0x%.08X)', [pResource.EmuVertexBuffer8]);
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
          // Cxbx TODO : once this is known to be working, remove the warning
          EmuWarning('Index buffer allocation size unknown');

          pIndexBuffer.Lock := X_D3DRESOURCE_LOCK_FLAG_NOSIZE;

          // Cxbx has Break; Delphi can't do that, so we use an else-block
          // Halo dwSize = 0x336;
        end
        else
        begin
          hRet := IDirect3DDevice8_CreateIndexBuffer(g_pD3DDevice8,
            dwSize, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED,
            @(pIndexBuffer^.Lock{EmuIndexBuffer8}));

          if (FAILED(hRet)) then
            CxbxKrnlCleanup('CreateIndexBuffer Failed!');

          pData := nil;

          hRet := IDirect3DIndexBuffer8(pResource.EmuIndexBuffer8).Lock(0, dwSize, pData, 0);

          if (FAILED(hRet)) then
            CxbxKrnlCleanup('IndexBuffer Lock Failed!');

          memcpy(pData, pBase, dwSize);

          IDirect3DIndexBuffer8(pResource.EmuIndexBuffer8).Unlock();

          pResource.Data := ULONG(pData);
        end;

{$IFDEF DEBUG}
        DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created IndexBuffer (0x%.08X)', [pResource.EmuIndexBuffer8]);
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
      dwSize := EmuCheckAllocationSize(pBase, true);

      if dwSize = DWORD(-1) then
      begin
        // Cxbx TODO : once this is known to be working, remove the warning
        EmuWarning('Push buffer allocation size unknown');

        pPushBuffer.Lock := X_D3DRESOURCE_LOCK_FLAG_NOSIZE;
      end
      else
      begin
        pResource.Data := ULONG(pBase);

{$IFDEF DEBUG}
        DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created PushBuffer (0x%.08X, 0x%.08X, 0x%.08X)', [pResource.Data, pPushBuffer.Size, pPushBuffer.AllocationSize]);
{$ENDIF}
      end;
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
      // Cxbx TODO : check for dimensions

      // Cxbx TODO : HACK: Temporary?
      if (X_Format = X_D3DFMT_LIN_D24S8) then // = $2E
      begin
        {CxbxKrnlCleanup}EmuWarning('D3DFMT_LIN_D24S8 not yet supported!');
        X_Format := X_D3DFMT_LIN_A8R8G8B8; // = $12
        Format := D3DFMT_A8R8G8B8;
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
      or (X_Format = X_D3DFMT_LIN_A4R4G4B4) or (X_Format = X_D3DFMT_A1R5G5B5)
      or (X_Format = $28 { X_D3DFMT_G8B8 }) or (X_Format = X_D3DFMT_LIN_A1R5G5B5) then
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
        CxbxKrnlCleanup('0x%.08X is not a supported format!', [X_Format]);
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
        dwPtr := DWORD(CxbxMalloc(dwSize + sizeof(DWORD)));

        pRefCount := PDWORD(dwPtr + dwSize);

        // initialize ref count
        pRefCount^ := 1;

        // If YUY2 is not supported in hardware, we'll actually mark this as a special fake texture (set highest bit)
        pPixelContainer.Data := X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_YUVSURF;
        pPixelContainer.Lock := dwPtr;
        pPixelContainer.Format := $24;

        pPixelContainer.Size := (g_dwOverlayW and X_D3DSIZE_WIDTH_MASK)
                             or (g_dwOverlayH shl X_D3DSIZE_HEIGHT_SHIFT)
                             or (g_dwOverlayP shl X_D3DSIZE_PITCH_SHIFT);
      end
      else
      begin
        if (bSwizzled or bCompressed) then
        begin
          w := dwWidth;
          h := dwHeight;

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
            @(pResource^.Lock{EmuSurface8})
          );

          if (FAILED(hRet)) then
            CxbxKrnlCleanup('CreateImageSurface Failed!');

{$IFDEF DEBUG}
          DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created ImageSurface(0x%.08X, 0x%.08X)', [pResource, pResource.EmuSurface8]);
          DbgPrintf('EmuIDirect3DResource8_Register: Width:%d, Height:%d, Format:%d', [dwWidth, dwHeight, Ord(Format)]);
{$ENDIF}

        end
        else
        begin
          // Cxbx TODO : HACK: Figure out why this is necessary!
          // Cxbx TODO : This is necessary for DXT1 textures at least (4x4 blocks minimum)
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
              [dwWidth, dwMipMapLevels, Ord(Format), pResource.EmuTexture8]);
{$ENDIF}

            hRet := IDirect3DDevice8_CreateCubeTexture(g_pD3DDevice8,
              dwWidth, dwMipMapLevels, 0, Format,
              D3DPOOL_MANAGED, @(pResource^.Lock{EmuCubeTexture8}));

            if (FAILED(hRet)) then
              CxbxKrnlCleanup('CreateCubeTexture Failed!');

{$IFDEF DEBUG}
            DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created CubeTexture(0x%.08X, 0x%.08X)', [pResource, pResource.EmuCubeTexture8]);
{$ENDIF}
          end
          else
          begin
{$IFDEF DEBUG}
            DbgPrintf('CreateTexture(%d,%d,%d, 0,%d, D3DPOOL_MANAGED, 0x%.08X)',
              [dwWidth, dwHeight, dwMipMapLevels, Ord(Format), @(pResource^.Lock{EmuTexture8})]);
{$ENDIF}

            hRet := IDirect3DDevice8_CreateTexture(g_pD3DDevice8,
              dwWidth, dwHeight, dwMipMapLevels, 0, Format,
              D3DPOOL_MANAGED, @(pResource^.Lock{EmuTexture8})
              );

            if (FAILED(hRet)) then
              CxbxKrnlCleanup('CreateTexture Failed!');

{$IFDEF DEBUG}
            DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created Texture (0x%.08X, 0x%.08X)', [pResource, pResource.EmuTexture8]);
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
          for level := 0 to dwMipMapLevels - 1 do
          begin
            // copy over data (deswizzle if necessary)
            if (dwCommonType = X_D3DCOMMON_TYPE_SURFACE) then
              hRet := IDirect3DSurface8(pResource.EmuSurface8).LockRect(LockedRect, NULL, 0)
            else
            begin
              if (bCubemap) then
                hRet := IDirect3DCubeTexture8(pResource.EmuCubeTexture8).LockRect(D3DCUBEMAP_FACES(r), 0, LockedRect, NULL, 0)
              else
                hRet := IDirect3DTexture8(pResource.EmuTexture8).LockRect(level, LockedRect, NULL, 0);
            end;

            iRect := Classes.Rect(0, 0, 0, 0);
            iPoint := Classes.Point(0, 0);

            pSrc := pBase;
            pThis.Data := DWORD(pSrc);

            if (IsSpecialResource(pResource.Data) and ((pResource.Data and X_D3DRESOURCE_DATA_FLAG_SURFACE) > 0))
            or (IsSpecialResource(DWORD(pBase)) and ((DWORD(pBase) and X_D3DRESOURCE_DATA_FLAG_SURFACE) > 0)) then
            begin
              EmuWarning('Attempt to registered to another resource''s data (eww!)');

              // Cxbx TODO : handle this horrible situation
              pDest := LockedRect.pBits;
              for v := 0 to dwMipHeight - 1 do
              begin
                memset(pDest, 0, dwMipWidth * dwBPP);

                Inc(pDest, LockedRect.Pitch);
                Inc(pSrc, dwMipPitch);
              end;
            end
            else
            begin
              if (bSwizzled) then
              begin
                if (DWORD(pSrc) = $80000000) then
                begin
                  // Cxbx TODO : Fix or handle this situation..?
                end
                else
                begin
                  if (CacheFormat = D3DFMT_P8) then //Palette
                  begin
                    EmuWarning('Unsupported texture format D3DFMT_P8, expanding to D3DFMT_A8R8G8B8');
{#if 0
                    //
                    // create texture resource
                    //
                    pPixelData := LockedRect.pBits;
                    dwDataSize := dwMipWidth * dwMipHeight * 4;
                    dwPaletteSize := 256 * 4; // Note: This is not allways true, it can be 256- 128- 64- or 32*4

                    pTextureCache := CxbxMalloc(dwDataSize);
                    pExpandedTexture := CxbxMalloc(dwDataSize);
                    pTexturePalette := CxbxMalloc(256 * 4);

                    // First we need to unswizzle the texture data
                    XTL_EmuXGUnswizzleRect
                    (
                      Pointer(DWORD(pSrc) + dwMipOffs), dwMipWidth, dwMipHeight, dwDepth, LockedRect.pBits,
                      LockedRect.Pitch, iRect, iPoint, dwBPP
                    );

                    // Copy the unswizzled data to a temporary buffer
                    memcpy(pTextureCache, pPixelData, dwDataSize);

                    // Copy the currently selected palette's data to the buffer
                    memcpy(pTexturePalette, pCurrentPalette, dwPaletteSize);

                    w := 0;
                    c := 0;
                    p := 0;
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
                    CxbxFree(pTexturePalette);
                    CxbxFree(pExpandedTexture);
                    CxbxFree(pTextureCache);
}
                  end
                  else
                  begin
                    XTL_EmuXGUnswizzleRect
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
                dummy := Pointer(DWORD(pSrc) + dwCompressedOffset); // Dxbx TODO : Do without dummy!
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
              IDirect3DSurface8(pResource.EmuSurface8).UnlockRect()
            else
            begin
              if (bCubemap) then
                IDirect3DCubeTexture8(pResource.EmuCubeTexture8).UnlockRect(D3DCUBEMAP_FACES(r), 0)
              else
                IDirect3DTexture8(pResource.EmuTexture8).UnlockRect(level);
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
          int dwDumpSurface := 0;

          szBuffer: array [0..255 - 1] of AnsiChar;

          StrFmt(szBuffer, _DEBUG_DUMP_TEXTURE_REGISTER '%.03d - RegSurface%.03d.bmp', [X_Format, dwDumpSurface++]);

          D3DXSaveSurfaceToFile(szBuffer, D3DXIFF_BMP, pResource.EmuSurface8, 0, 0);
        end
        else
        if (bCubemap) then
        begin
          Integer dwDumpCube := 0;

          szBuffer: array [0..255 - 1] of Char;

          for (Integer v := 0; v < 6; v++)
          begin
            pSurface := 0;

            StrFmt(szBuffer, _DEBUG_DUMP_TEXTURE_REGISTER '%.03d - RegCubeTex%.03d -%d.bmp', [X_Format, dwDumpCube++, v]);

            IDirect3DCubeTexture8(pResource.EmuCubeTexture8).GetCubeMapSurface((D3DCUBEMAP_FACES)v, 0, @pSurface);

            D3DXSaveSurfaceToFile(szBuffer, D3DXIFF_BMP, pSurface, 0, 0);
          end;
        end
        else
        begin
          Integer dwDumpTex := 0;

          szBuffer: array [0..255 - 1] of Char;

          StrFmt(szBuffer, _DEBUG_DUMP_TEXTURE_REGISTER '%.03d - RegTexture%.03d.bmp', [X_Format, dwDumpTex++]);

          D3DXSaveTextureToFile(szBuffer, D3DXIFF_BMP, pResource.EmuTexture8, 0);
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
          // Cxbx TODO : once this is known to be working, remove the warning
          EmuWarning('Palette allocation size unknown');

          pPalette.Lock := X_D3DRESOURCE_LOCK_FLAG_NOSIZE;
        end;

        pCurrentPalette := pBase;
        dwCurrentPaletteSize := dwSize;

        pResource.Data := ULONG(pBase);
      end;

{$IFDEF DEBUG}
(*      DbgPrintf('EmuIDirect3DResource8_Register: Successfully Created Palette (0x%.08X, 0x%.08X, 0x%.08X)', [pResource.Data, pResource.Size, pResource.AllocationSize]);*)
{$ENDIF}
    end;


    X_D3DCOMMON_TYPE_FIXUP:
    begin
      pFixup := PX_D3DFixup(pResource);

      CxbxKrnlCleanup('IDirect3DResource8::Register -> X_D3DCOMMON_TYPE_FIXUP is not yet supported' +
                #13#10'0x%.08X (pFixup->Common)' +
                #13#10'0x%.08X (pFixup->Data)' +
                #13#10'0x%.08X (pFixup->Lock)' +
                #13#10'0x%.08X (pFixup->Run)' +
                #13#10'0x%.08X (pFixup->Next)' +
                #13#10'0x%.08X (pFixup->Size)',
                [pFixup.Common, pFixup.Data, pFixup.Lock, pFixup.Run, pFixup.Next, pFixup.Size]);
    end;

  else // case
    CxbxKrnlCleanup('IDirect3DResource8.Register.Common Type 0x%.08X not yet supported', [dwCommonType]);
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
    dwPtr := DWORD(pThis.Lock);
    pRefCount := PDWORD(dwPtr + g_dwOverlayP*g_dwOverlayH);
    Inc(pRefCount^);
  end
  else
  begin
    pResource8 := pThis.EmuResource8;

    if(pThis.Lock = $8000BEEF) then
    begin
      Inc(pThis.Lock);
      uRet := pThis.Lock;
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
    #13#10'   pThis             : 0x%.08X' +
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
//    uRet = IDirect3DBaseTexture8(pThis.EmuBaseTexture8)._Release();
    Dispose(pThis);
  end
  else  if (IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    dwPtr := DWORD(pThis.Lock);
    pRefCount := PDWORD(dwPtr + g_dwOverlayP*g_dwOverlayH);

    Dec(pRefCount^);
    if pRefCount^ = 0 then
    begin
      if (g_YuvSurface = pThis) then
        g_YuvSurface := NULL;

      // free memory associated with this special resource handle
      CxbxFree(PVOID(dwPtr));
    end;

    EmuSwapFS(fsXbox);
    XTL_EmuIDirect3DDevice8_EnableOverlay(FALSE);
    EmuSwapFS(fsWindows);
  end
  else
  begin
    pResource8 := pThis.EmuResource8;

    if (pThis.Lock = $8000BEEF) then
    begin
      Dispose(PVOID(pThis.Data));
      Dec(pThis.Lock);
      uRet := pThis.Lock;
    end
    else
    begin
      if (pResource8 <> nil) then
      begin
        for v := 0 to 16 - 1 do
        begin
          if (pCache[v].Data = pThis.Data) and (pThis.Data <> 0) then
          begin
            pCache[v].Data := 0;
            Break;
          end;
        end;

{$ifdef _DEBUG_TRACE_VB}
        Type_ := pResource8.GetType();
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
        end;
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
): LONGBOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pResource8: XTL_PIDirect3DResource8;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DResource8_IsBusy' +
    #13#10'(' +
    #13#10'   pThis             : 0x%.08X' +
    #13#10');',
    [pThis]);
{$ENDIF}
  pResource8 := pThis.EmuResource8;
  EmuSwapFS(fsXbox);

  Result := FALSE;
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

  // Cxbx TODO : Handle situation where the resource type is >7
  Result := X_D3DRESOURCETYPE(IDirect3DResource8(pThis.EmuResource8).GetType());

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

  IDirect3DCubeTexture8(pPixelContainer.EmuCubeTexture8).LockRect(FaceType, Level, pLockedRect^, pRect, Flags);

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
         #13#10'   pPixelContainer   : 0x%.08X' +
         #13#10'   dwLevel           : 0x%.08X' +
         #13#10'   pDesc             : 0x%.08X' +
         #13#10');',
         [pPixelContainer, dwLevel, pDesc]);
{$ENDIF}

  EmuVerifyResourceIsRegistered(pPixelContainer);

  ZeroMemory(@SurfaceDesc, sizeof(SurfaceDesc));

  if (dwLevel = $FEFEFEFE) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuSurface8: = 0x%.08X', [pPixelContainer.EmuSurface8]);
{$ENDIF}
    hRet := IDirect3DSurface8(pPixelContainer.EmuSurface8).GetDesc(SurfaceDesc);
    { marked by cxbx
     Integer dwDumpSurface := 0;
     szBuffer: array [0..255-1] of Char;
     StrFmt(szBuffer, 'C:\Aaron\Textures\Surface%.03d.bmp', dwDumpSurface++);
     D3DXSaveSurfaceToFile(szBuffer, D3DXIFF_BMP, pPixelContainer.EmuSurface8, 0, 0);
    }
  end
  else
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuTexture8: = 0x%.08X', [pPixelContainer.EmuTexture8]);
{$ENDIF}

    // Cxbx TODO: Work on Namco Museum hack later...
    // if pPixelContainer.EmuTexture8 = (IDirect3DTexture8($078A0044)) then

    hRet := IDirect3DTexture8(pPixelContainer.EmuTexture8).GetLevelDesc(dwLevel, SurfaceDesc);
    //hRet = IDirect3DSurface8(pPixelContainer.EmuSurface8).GetDesc(@SurfaceDesc);
    if FAILED(hRet) then
      EmuWarning('IDirect3DTexture8::GetSurfaceDesc failed!');

    DbgPrintf('Okay');

    { marked out by cxbx
     Integer dwDumpTexture := 0;
     szBuffer: array [0..255-1] of Char;
     StrFmt(szBuffer, 'C:\Aaron\Textures\GetDescTexture%.03d.bmp', dwDumpTexture++);
     D3DXSaveTextureToFile(szBuffer, D3DXIFF_BMP, pPixelContainer.EmuTexture8, 0);
    }
  end;

  // rearrange into xbox format (remove D3DPOOL)
  if SUCCEEDED(hRet) then
  begin
    // Convert Format (PC->Xbox)
    pDesc.Format := EmuPC2XB_D3DFormat(SurfaceDesc.Format);
    pDesc.Type_ := X_D3DRESOURCETYPE(SurfaceDesc._Type);

    if (Ord(pDesc.Type_) > 7) then
      CxbxKrnlCleanup('EmuGet2DSurfaceDesc: pDesc.Type > 7');

    pDesc.Usage := SurfaceDesc.Usage;
    pDesc.Size := SurfaceDesc.Size;

    // Cxbx TODO : Convert from Xbox to PC!!
    if SurfaceDesc.MultiSampleType = D3DMULTISAMPLE_NONE then
      pDesc.MultiSampleType := D3DMULTISAMPLE_TYPE($0011)
    else
      CxbxKrnlCleanup(Format('EmuGet2DSurfaceDesc Unknown Multisample format! (%d)', [Ord(SurfaceDesc.MultiSampleType)]));

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
    #13#10'   pThis             : 0x%.08X' +
    #13#10'   pDesc             : 0x%.08X' +
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
    pSurface8 := pThis.EmuSurface8;
    Result := IDirect3DSurface8(pSurface8).GetDesc(SurfaceDesc);

    // rearrange into windows format (remove D3DPool)
    // Convert Format (PC->Xbox)
    pDesc.Format := EmuPC2XB_D3DFormat(SurfaceDesc.Format);
    pDesc.Type_ := X_D3DRESOURCETYPE(SurfaceDesc._Type);

    if (Ord(pDesc.Type_) > 7) then
      CxbxKrnlCleanup('EmuIDirect3DSurface8_GetDesc: pDesc.Type > 7');

    pDesc.Usage := SurfaceDesc.Usage;
    pDesc.Size := SurfaceDesc.Size;

    // Cxbx TODO : Convert from Xbox to PC!!
    if (SurfaceDesc.MultiSampleType = D3DMULTISAMPLE_NONE) then
      pDesc.MultiSampleType := D3DMULTISAMPLE_TYPE($0011)
    else
      CxbxKrnlCleanup('EmuIDirect3DSurface8_GetDesc Unknown Multisample format! (%d)', [@SurfaceDesc.MultiSampleType]);

    pDesc.Width  := SurfaceDesc.Width;
    pDesc.Height := SurfaceDesc.Height;
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
    #13#10'   pThis             : 0x%.08X' +
    #13#10'   pLockedRect       : 0x%.08X' +
    #13#10'   pRect             : 0x%.08X' +
    #13#10'   Flags             : 0x%.08X' +
    #13#10');',
    [pThis, pLockedRect, pRect, Flags]);
{$ENDIF}

  hRet := 0; // Dxbx : Prevent 'not initialized' compiler warning

{$IFDEF DXBX_DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DSurface8_LockRect (pThis->Surface = 0x%8.8X)', [pThis.EmuSurface8]);
{$ENDIF}

  // Cxbx (shogun) commented this :
  //if not Assigned(pThis.EmuSurface8) or (pThis.EmuSurface8 = XTL_PIDirect3DSurface8($00000004)) then
  //begin
  //  EmuWarning('Invalid Surface!');
  //  EmuSwapFS(fsWindows);
  //  Result := E_FAIL; Exit;
  //end;

  EmuVerifyResourceIsRegistered(pThis);

  if (IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    pLockedRect.Pitch := g_dwOverlayP;
    pLockedRect.pBits := PVOID(pThis.Lock);

    hRet := D3D_OK;
  end
  else
  begin
    if (Flags and $40) > 0 then
      EmuWarning('D3DLOCK_TILED ignored!');

    pSurface8 := pThis.EmuSurface8;

    NewFlags := 0;

    if (Flags and $80) > 0 then
      NewFlags:= NewFlags or D3DLOCK_READONLY;

    if (Flags and not ($80 or $40)) > 0 then
      CxbxKrnlCleanup('EmuIDirect3DSurface8_LockRect: Unknown Flags! (0x%.08X)', [Flags]);

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
  
  Result := IDirect3DBaseTexture8(pThis.EmuBaseTexture8).GetLevelCount();

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
  // In a special situation, we are actually returning a memory ptr with high bit set
  if (IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    dwSize := g_dwOverlayP*g_dwOverlayH;

    pRefCount := PDWORD(DWORD(pThis.Lock) + dwSize);

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

	DbgPrintf('EmuD3D8 : EmuIDirect3DTexture8_LockRect (pThis->Texture = 0x%8.8X)', [pThis.EmuTexture8]);
{$ENDIF}

  EmuVerifyResourceIsRegistered(pThis);

  // check if we have an unregistered YUV2 resource
  if ((pThis <> nil) and IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    pLockedRect.Pitch := g_dwOverlayP;
    pLockedRect.pBits := PVOID(pThis.Lock);

    hRet := D3D_OK;
  end
  else
  begin
    pTexture8 := pThis.EmuTexture8;

    NewFlags := 0;

    if (Flags and $80) > 0 then
        NewFlags:= NewFlags or D3DLOCK_READONLY;

    if (Flags and not ($80 or $40)) > 0 then
        CxbxKrnlCleanup('EmuIDirect3DTexture8_LockRect: Unknown Flags! (0x%.08X)', [Flags]);

    if (Level = 6) or (Level = 7) or (Level = 8) or (Level = 9) then
    begin
      // HACK: Unreal Championship crashes when the texture level reaches 9...
      EmuWarning('Unreal Championship texture hack applied!');
      hRet := D3DERR_INVALIDCALL;
    end
    else
    begin
      // Remove old lock(s)
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
      #13#10'   pThis             : 0x%.08X' +
      #13#10'   Level             : 0x%.08X' +
      #13#10'   ppSurfaceLevel    : 0x%.08X' +
      #13#10');',
      [pThis, Level, ppSurfaceLevel]);
{$ENDIF}

  EmuVerifyResourceIsRegistered(pThis);

  // if highest bit is set, this is actually a raw memory pointer (for YUY2 simulation)
  if (IsSpecialResource(pThis.Data) and ((pThis.Data and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0)) then
  begin
    dwSize := g_dwOverlayP*g_dwOverlayH;

    pRefCount := PDWORD(DWORD(pThis.Lock) + dwSize);

    // initialize ref count
    Inc(pRefCount^);

    ppSurfaceLevel^ := PX_D3DSurface(pThis);

    hRet := D3D_OK;
  end
  else
  begin
    pTexture8 := pThis.EmuTexture8;

    New({var}ppSurfaceLevel^); // Cxbx : new X_D3DSurface();

    ppSurfaceLevel^.Data := $B00BBABE;
    ppSurfaceLevel^.Common := 0;
    ppSurfaceLevel^.Format := 0;
    ppSurfaceLevel^.Size := 0;

    hRet := IDirect3DTexture8_GetSurfaceLevel(pTexture8, Level, @(ppSurfaceLevel^.Lock{EmuSurface8}));

    if (FAILED(hRet)) then
    begin
      EmuWarning('EmuIDirect3DTexture8_GetSurfaceLevel Failed!');
    end
    else
    begin
{$IFDEF DEBUG}
      DbgPrintf('EmuD3D8 : EmuIDirect3DTexture8_GetSurfaceLevel := 0x%.08X', [ppSurfaceLevel^.EmuSurface8]);
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
  pVolumeTexture8 := pThis.EmuVolumeTexture8;
  Result := IDirect3DVolumeTexture8(pVolumeTexture8).LockBox(Level, pLockedVolume^, pBox, Flags);

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
  Result := IDirect3DCubeTexture8(pThis.EmuCubeTexture8).LockRect(FaceType, Level, pLockedBox^, pRect, Flags);

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
    // Signal proxy thread, and wait for completion
    g_EmuCDPD.bCreate := false; // Dxbx: bCreate should be set before bReady!
    g_EmuCDPD.bReady := true;

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
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_CreateVertexBuffer2' +
         #13#10'(' +
         #13#10'   Length            : 0x%.08X' +
         #13#10');',
         [Length]);
{$ENDIF}

  New({PX_D3DVertexBuffer}pD3DVertexBuffer);

  hRet := IDirect3DDevice8_CreateVertexBuffer(g_pD3DDevice8,
    Length, 
    0, 
    0, 
    D3DPOOL_MANAGED,
    @(pD3DVertexBuffer.Lock{EmuVertexBuffer8})
  );

  if (FAILED(hRet)) then
    EmuWarning('CreateVertexBuffer Failed!');

{$ifdef _DEBUG_TRACK_VB}
  g_VBTrackTotal.insert(pD3DVertexBuffer.EmuVertexBuffer8);
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

  if (Enable = FALSE) and (g_pDDSOverlay7 <> NULL) then
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
    if (Enable = TRUE) and (g_pDDSOverlay7 = nil) then
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
          CxbxKrnlCleanup('Could not create overlay surface');

        hRet := IDirectDraw7(g_pDD7).CreateClipper(0, {out}IDirectDrawClipper(g_pDDClipper), NULL);

        if (FAILED(hRet)) then
          CxbxKrnlCleanup('Could not create overlay clipper');

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
  pDest: PAnsiChar;
  pSour: PAnsiChar;
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

  if Assigned(pSurface) then
  begin
    // manually copy data over to overlay
    if (g_bSupportsYUY2) then
    begin
      ZeroMemory(@ddsd2, sizeof(ddsd2));

      ddsd2.dwSize := sizeof(ddsd2);

      if (FAILED(IDirectDrawSurface7(g_pDDSOverlay7).Lock(nil, ddsd2, DDLOCK_SURFACEMEMORYPTR or DDLOCK_WAIT, 0))) then
        EmuWarning('Unable to lock overlay surface!');

      // copy data
      begin
        pDest := PAnsiChar(ddsd2.lpSurface);
        pSour := PAnsiChar(pSurface.Lock);

        w := g_dwOverlayW;
        h := g_dwOverlayH;

        // Cxbx TODO : sucker the game into rendering directly to the overlay (speed boost)
        if ((DWORD(ddsd2.lPitch) = w * 2) and (DWORD(g_dwOverlayP) = w * 2)) then
          memcpy(pDest, pSour, h * w * 2)
        else
        begin
          for y := 0 to h - 1 do
          begin
            memcpy(pDest, pSour, w * 2);
            Inc(pDest, ddsd2.lPitch);
            Inc(pSour, g_dwOverlayP);
          end;
        end;
      end;

      IDirectDrawSurface7(g_pDDSOverlay7).Unlock(nil);
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

      DestRect.left   := DestRect.left + nBorderWidth;
      DestRect.right  := DestRect.right - nBorderWidth;
      DestRect.top    := DestRect.top + nTitleHeight + nBorderHeight;
      DestRect.bottom := DestRect.bottom - nBorderHeight;

      DestRect.left   := DestRect.left - aMonitorInfo.rcMonitor.left;
      DestRect.right  := DestRect.right - aMonitorInfo.rcMonitor.left;
      DestRect.top    := DestRect.top - aMonitorInfo.rcMonitor.top;
      DestRect.bottom := DestRect.bottom - aMonitorInfo.rcMonitor.top;

      ZeroMemory(@ddofx, sizeof(ddofx));

      ddofx.dwSize := sizeof(DDOVERLAYFX);
      ddofx.dckDestColorkey.dwColorSpaceLowValue := 0;
      ddofx.dckDestColorkey.dwColorSpaceHighValue := 0;

      {Dxbx unused hRet :=} IDirectDrawSurface7(g_pDDSOverlay7).UpdateOverlay(@SourRect, IDirectDrawSurface7(g_pDDSPrimary), @DestRect, {DDOVER_KEYDESTOVERRIDE or} DDOVER_SHOW, {&ddofx}nil);
    end
    else
    begin
      // Cxbx TODO : dont assume X8R8G8B8 ?
      pBackBuffer := nil;
      hRet := IDirect3DDevice8(g_pD3DDevice8).GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, @pBackBuffer);

      // if we obtained the backbuffer, manually translate the YUY2 into the backbuffer format
      if (hRet = D3D_OK) and (IDirect3DSurface8(pBackBuffer).LockRect(LockedRectDest, NULL, 0) = D3D_OK) then
      begin
        pCurByte := Puint08(pSurface.Lock);
        pDest2 := Puint08(LockedRectDest.pBits);

        dx := 0; dy := 0;
        
        dwImageSize := g_dwOverlayP*g_dwOverlayH;

        // grayscale
        if (false) then
        begin
          for y := 0 to g_dwOverlayH - 1 do
          begin
            stop := g_dwOverlayW * 4;
            while Uint32(x) < stop do
            begin
              Y3 := Uint08(pCurByte[0]);
              pDest2[x+0] := Y3;
              pDest2[x+1] := Y3;
              pDest2[x+2] := Y3;
              pDest2[x+3] := $FF;
              pCurByte := @(pCurByte[2]); // Inc(pCurByte, 2) doesn't work
              Inc(x, 4);
            end; // While

            pDest2:= @pDest2[LockedRectDest.Pitch];
         end;
        end
        // full color conversion (YUY2->XRGB)
        else
        begin
          v := 0;
          while v < dwImageSize do
          begin
            Y2[0] := pCurByte[0];
            U2 := pCurByte[1];
            Y2[1] := pCurByte[2];
            V2 := pCurByte[3];
            pCurByte := @pCurByte[4];

            a := 0;
            for x := 0 to 2 - 1 do
            begin
              R := Y2[a] + 1.402*(V2-128);
              G := Y2[a] - 0.344*(U2-128) - 0.714*(V2-128);
              B := Y2[a] + 1.772*(U2-128);

              if R < 0 then
                R := 0;
              if R > 255 then
                R := 255;

              if G < 0 then
                G := 0;
              if G > 255 then
                G := 255;

              if B < 0 then
                B := 0;
              if B > 255 then
                B := 255;

              i := (dy*uint32(LockedRectDest.Pitch)+(dx+uint32(x))*4);

              pDest2[i+0] := Round(B);
              pDest2[i+1] := Round(G);
              pDest2[i+2] := Round(R);
              pDest2[i+3] := $FF;

              Inc(a);
            end;

            Inc(Dx, 2);

            if ((dx mod g_dwOverlayW) = 0) then
            begin
              Inc(dy);
              dx := 0;
            end;

            Inc(v, 4);
          end; // While
        end;

        IDirect3DSurface8(pBackBuffer).UnlockRect();
      end;

      // Update overlay if present was not called since the last call to
      // EmuIDirect3DDevice8_UpdateOverlay.
      if g_bHackUpdateSoftwareOverlay then
        IDirect3DDevice8(g_pD3DDevice8).Present(nil, nil, 0, nil);

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

  // Cxbx TODO : Actually check for update status

  EmuSwapFS(fsXbox);

  Result := FALSE;
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

  g_pVBCallback := pCallback;

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
    #13#10'   Stage             : 0x%.08X' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Stage, Value]);
{$ENDIF}

  if (Value > $00030000) then
    CxbxKrnlCleanup('EmuIDirect3DDevice8_SetTextureState_TexCoordIndex: Unknown TexCoordIndex Value (0x%.08X)', [Value]);

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
    22:    // X_D3DTSS_BUMPENVMAT00
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(Stage, D3DTSS_BUMPENVMAT00, Value);
    23:    // X_D3DTSS_BUMPENVMAT01
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(Stage, D3DTSS_BUMPENVMAT01, Value);
    24:    // X_D3DTSS_BUMPENVMAT11
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(Stage, D3DTSS_BUMPENVMAT11, Value);
    25:    // X_D3DTSS_BUMPENVMAT10
      IDirect3DDevice8(g_pD3DDevice8).SetTextureStageState(Stage, D3DTSS_BUMPENVMAT10, Value);
    26:    // X_D3DTSS_BUMPENVLSCALE
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
    #13#10'   Value             : 0x%.08X' +
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
    #13#10'   Value             : 0x%.08X' +
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
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  // Cxbx TODO : Analyze performance and compatibility (undefined behavior on PC with triangles or points)
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
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  if (g_iWireframe = 0) then
    dwFillMode := EmuXB2PC_D3DFILLMODE(Value)
  else if (g_iWireframe = 1) then
    dwFillMode := D3DFILL_WIREFRAME
  else
    dwFillMode := D3DFILL_POINT;

  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_FILLMODE, dwFillMode);

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
    #13#10'   Value             : 0x%.08X' +
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
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  EmuWarning('SetRenderState_Dxt1NoiseEnable not implemented!');

  EmuSwapFS(fsXbox);
end;

// Dxbx: This argument makes the 'register' calling convention
// functionally equivalent to the 'fastcall' calling convention.
// Quote from http://www.codeguru.com/forum/showthread.php?t=466266 :
// They differ as follows:
// register: (left to right) EAX, EDX, ECX, remaining pushed on stack right to left, callee cleans
// fastcall: (left to right) ECX, EDX, remaining pushed on stack left to right, callee cleans
procedure XTL_EmuIDirect3DDevice8_SetRenderState_Simple(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Value: DWORD;
  {1 ECX}Method: DWORD // Dxbx note: The first argument should be here, to force it into ECX
  ); register; // __fastcall in Cxbx
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  State: int;
  v: int;
  OrigValue: DWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_Simple' +
    #13#10'(' +
    #13#10'   Method            : 0x%.08X' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Method, Value]);
{$ENDIF}

  State := -1;

  // Cxbx TODO : make this faster and more elegant
  for v := 0 to 174-1 do
  begin
    if (EmuD3DRenderStateSimpleEncoded[v] = Method) then
    begin
      State := v;
      Break;
    end;
  end;

  if (State = -1) then
    EmuWarning('RenderState_Simple(0x%.08X, 0x%.08X) is unsupported!', [Method, Value])
  else
  begin
    case (State) of
      168: //D3DRS_COLORWRITEENABLE:
        begin
          OrigValue := Value;

          Value := 0;

          if (OrigValue and (1 shl 16)) > 0 then
            Value:= Value or D3DCOLORWRITEENABLE_RED;
          if (OrigValue and (1 shl 8)) > 0 then
              Value:= Value or D3DCOLORWRITEENABLE_GREEN;
          if (OrigValue and (1 shl 0)) > 0 then
              Value:= Value or D3DCOLORWRITEENABLE_BLUE;
          if (OrigValue and (1 shl 24)) > 0 then
              Value:= Value or D3DCOLORWRITEENABLE_ALPHA;

{$IFDEF DEBUG}
          DbgPrintf('D3DRS_COLORWRITEENABLE := 0x%.08X', [Value]);
{$ENDIF}
        end;

      9: //D3DRS_SHADEMODE:
        begin
          Value := EmuXB2PC_D3DSHADEMODE(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_SHADEMODE := 0x%.08X', [Value]);
{$ENDIF}
        end;

      171: //D3DRS_BLENDOP:
        begin
          Value := EmuXB2PC_D3DBLENDOP(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_BLENDOP := 0x%.08X', [Value]);
{$ENDIF}
        end;

      19: //D3DRS_SRCBLEND:
        begin
          Value := EmuXB2PC_D3DBLEND(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_SRCBLEND := 0x%.08X', [Value]);
{$ENDIF}
        end;

      20: //D3DRS_DESTBLEND:
        begin
          Value := EmuXB2PC_D3DBLEND(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_DESTBLEND := 0x%.08X', [Value]);
{$ENDIF}
        end;

      23: //D3DRS_ZFUNC:
        begin
          Value := EmuXB2PC_D3DCMPFUNC(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ZFUNC := 0x%.08X', [Value]);
{$ENDIF}
        end;

      25: //D3DRS_ALPHAFUNC:
        begin
          Value := EmuXB2PC_D3DCMPFUNC(Value);
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ALPHAFUNC := 0x%.08X', [Value]);
{$ENDIF}
        end;

      15: //D3DRS_ALPHATESTENABLE:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ALPHATESTENABLE := 0x%.08X', [Value]);
{$ENDIF}
        end;

      27: //D3DRS_ALPHABLENDENABLE:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ALPHABLENDENABLE := 0x%.08X', [Value]);
{$ENDIF}
        end;

      24: //D3DRS_ALPHAREF:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ALPHAREF := %f', [DWtoF(Value)]);
{$ENDIF}
        end;

      14: //D3DRS_ZWRITEENABLE:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_ZWRITEENABLE := 0x%.08X', [Value]);
{$ENDIF}
        end;

      26: //D3DRS_DITHERENABLE:
        begin
{$IFDEF DEBUG}
          DbgPrintf('D3DRS_DITHERENABLE := 0x%.08X', [Value]);
{$ENDIF}
        end;
    else
      begin
        CxbxKrnlCleanup('Unsupported RenderState (0x%.08X)', [State]);
      end;
    end;

    // Cxbx TODO : verify these params as you add support for them!
    IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRENDERSTATETYPE(State), Value);
  end;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_VertexBlend
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_VertexBlend' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  // convert from Xbox direct3d to PC direct3d enumeration
  if (Value <= 1) then
    Value := Value
  else if (Value = 3) then
    Value := 2
  else if (Value = 5) then
    Value := 3
  else
    CxbxKrnlCleanup('Unsupported D3DVERTEXBLENDFLAGS (%d)', [Value]);

  IDirect3DDevice8(g_pD3DDevice8).SetRenderState(D3DRS_VERTEXBLEND, Value);

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

  // Cxbx TODO : do something..

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_SetRenderState_CullMode
(
  Value: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_CullMode' +
    #13#10'(' +
    #13#10'   Value             : 0x%.08X' +
    #13#10');',
    [Value]);
{$ENDIF}

  // convert from Xbox D3D to PC D3D enumeration
  // Cxbx TODO : XDK-Specific Tables? So far they are the same
  case (Value) of
    0:
      Value := D3DCULL_NONE;
    $900:
      Value := D3DCULL_CW;
    $901:
      Value := D3DCULL_CCW;
  else
    CxbxKrnlCleanup('EmuIDirect3DDevice8_SetRenderState_CullMode: Unknown Cullmode (%d)', [Value]);
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

  // Cxbx TODO : Convert to PC format??
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
    #13#10'   Value             : 0x%.08X' +
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
    #13#10'   Value             : 0x%.08X' +
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

  EmuWarning('SetRenderState_MultiSampleMode is not supported!');

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
  if (Enable <> g_fYuvEnabled) then
  begin
    g_fYuvEnabled := Enable;

    EmuWarning('EmuIDirect3DDevice8_SetRenderState_YuvEnable using overlay!');
    EmuSwapFS(fsXbox);
    XTL_EmuIDirect3DDevice8_EnableOverlay(g_fYuvEnabled);
    EmuSwapFS(fsWindows);
  end;

  if (g_fYuvEnabled) then
  begin
    EmuSwapFS(fsXbox);
    XTL_EmuIDirect3DDevice8_UpdateOverlay(g_YuvSurface, nil, nil, FALSE, 0);
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
    #13#10'   State             : 0x%.08X' +
    #13#10'   pMatrix           : 0x%.08X' +
    #13#10');',
    [Ord(State), pMatrix]);

  { Commented by CXBX

  DbgPrintf('pMatrix (%d)', [State]);
  DbgPrintf('begin ');
  DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._11, pMatrix._12, pMatrix._13, pMatrix._14]);
  DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._21, pMatrix._22, pMatrix._23, pMatrix._24]);
  DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._31, pMatrix._32, pMatrix._33, pMatrix._34]);
  DbgPrintf('    %.08f,%.08f,%.08f,%.08f', [pMatrix._41, pMatrix._42, pMatrix._43, pMatrix._44]);
  DbgPrintf(' end;');

  if (State = 6 and (pMatrix._11 = 1.0) and (pMatrix._22 = 1.0) and (pMatrix._33 = 1.0) and (pMatrix._44 = 1.0)) then
  begin
    Xtl_g_bSkipPush := TRUE;
    DbgPrintf('SkipPush ON');
  end
  else
  begin
    Xtl_g_bSkipPush := False;
    DbgPrintf('SkipPush OFF');
  end;
  }

  Result := IDirect3DDevice8(g_pD3DDevice8).SetTransform(EmuXB2PC_D3DTS(State), pMatrix^);

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
  ppVertexBuffer: PPX_D3DVertexBuffer;
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

  pVertexBuffer8 := ppVertexBuffer^.EmuVertexBuffer8;

  hRet := IDirect3DVertexBuffer8(pVertexBuffer8).Lock(OffsetToLock, SizeToLock, ppbData^, Flags);

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
         #13#10'   ppVertexBuffer    : 0x%.08X' +
         #13#10'   Flags             : 0x%.08X' +
         #13#10');',
         [ppVertexBuffer, Flags]);
{$ENDIF}

  pVertexBuffer8 := ppVertexBuffer^.EmuVertexBuffer8;
  pbData := NULL;

  {Dxbx unused hRet :=} IDirect3DVertexBuffer8(pVertexBuffer8).Lock(0, 0, {out}pbData, EmuXB2PC_D3DLock(Flags));    // Fixed flags check, Battlestar Galactica now displays graphics correctly

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
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetStreamSource2'+
            #13#10'(' +
            #13#10'   StreamNumber             : 0x%.08X' +
            #13#10'   pStride                  : 0x%.08X' +
            #13#10');',
            [StreamNumber, pStride]);
{$ENDIF}

  EmuWarning('Not correctly implemented yet!');
  {ignore HRESULT?}IDirect3DDevice8(g_pD3DDevice8).GetStreamSource(
    StreamNumber,
    @pVertexBuffer,
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
      #13#10'   StreamNumber      : 0x%.08X' +
      #13#10'   pStreamData       : 0x%.08X (0x%.08X)' +
      #13#10'   Stride            : 0x%.08X' +
      #13#10');',
      [StreamNumber, pStreamData, pStreamData.EmuVertexBuffer8, Stride])
  else
    DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetStreamSource' +
      #13#10'(' +
      #13#10'   StreamNumber      : 0x%.08X' +
      #13#10'   pStreamData       : 0x%.08X (0x%.08X)' +
      #13#10'   Stride            : 0x%.08X' +
      #13#10');',
      [StreamNumber, pStreamData, 0, Stride]);
{$ENDIF}

  if (StreamNumber = 0) then
    g_pVertexBuffer := pStreamData;

  pVertexBuffer8 := NULL;
  if (pStreamData <> NULL) then
  begin
    EmuVerifyResourceIsRegistered(pStreamData);

    pVertexBuffer8 := pStreamData.EmuVertexBuffer8;
    IDirect3DVertexBuffer8(pVertexBuffer8).Unlock();
   end;

  {$ifdef _DEBUG_TRACK_VB}
  if (pStreamData <> NULL) then
  begin
    g_bVBSkipStream := g_VBTrackDisable.exists(pStreamData.EmuVertexBuffer8);
  end;
  {$endif}

  Result := IDirect3DDevice8(g_pD3DDevice8).SetStreamSource(StreamNumber, IDirect3DVertexBuffer8(pVertexBuffer8), Stride);

  if FAILED(RESULT) then
    CxbxKrnlCleanup('SetStreamSource Failed!');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_SetVertexShader
(
  aHandle: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  RealHandle: DWORD;
  vOffset: TD3DXVECTOR4;
  vScale: TD3DXVECTOR4;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetVertexShader' +
    #13#10'(' +
    #13#10'   Handle            : 0x%.08X' +
    #13#10');',
    [aHandle]);
{$ENDIF}

  g_CurrentVertexShader := aHandle;

  // Store viewport offset and scale in constant registers 58 (c-38) and
  // 59 (c-37) used for screen space transformation.

  if (g_VertexShaderConstantMode <> X_VSCM_NONERESERVED) then
  begin
    // Cxbx TODO: Proper solution.
    with vScale do begin x := 2.0 / 640; y := -2.0 / 480; z := 0.0; w := 0.0; end;
    with vOffset do begin x := -1.0; y := 1.0; z := 0.0; w := 1.0; end;

    IDirect3DDevice8(g_pD3DDevice8).SetVertexShaderConstant(58, vScale, 1);
    IDirect3DDevice8(g_pD3DDevice8).SetVertexShaderConstant(59, vOffset, 1);
  end;

  if (VshHandleIsVertexShader(aHandle)) then
    RealHandle := PVERTEX_SHADER(VshHandleGetVertexShader(aHandle).Handle).Handle
  else
    RealHandle := aHandle;

  Result := IDirect3DDevice8(g_pD3DDevice8).SetVertexShader(RealHandle);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_DrawVertices
(
  PrimitiveType: X_D3DPRIMITIVETYPE;
  StartVertex: UINT;
  VertexCount: UINT); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  VPDesc: VertexPatchDesc;
  VertPatch: XTL_VertexPatcher;
//  bPatched: bool;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DrawVertices' +
    #13#10'(' +
    #13#10'   PrimitiveType     : 0x%.08X' +
    #13#10'   StartVertex       : 0x%.08X' +
    #13#10'   VertexCount       : 0x%.08X' +
    #13#10');',
    [Ord(PrimitiveType), StartVertex, VertexCount]);
{$ENDIF}

  XTL_EmuUpdateDeferredStates();

  VPDesc.PrimitiveType := PrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
  VPDesc.dwOffset := StartVertex;
  VPDesc.pVertexStreamZeroData := nil;
  VPDesc.uiVertexStreamZeroStride := 0;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.Create; // Dxbx addition

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc);

  if XTL_IsValidCurrentShader() then
  begin
    {$ifdef _DEBUG_TRACK_VB}
    if (not g_bVBSkipStream) then
    begin
    {$endif}
      IDirect3DDevice8(g_pD3DDevice8).DrawPrimitive
      (
          EmuPrimitiveType(VPDesc.PrimitiveType),
          StartVertex,
          VPDesc.dwPrimitiveCount
      );
    {$ifdef _DEBUG_TRACK_VB}
    end;
    {$endif}
  end;

  VertPatch.Restore();

  VertPatch.Destroy; // Dxbx addition

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_DrawVerticesUP
(
  PrimitiveType: X_D3DPRIMITIVETYPE;
  VertexCount: UINT; 
  pVertexStreamZeroData: PVOID; 
  VertexStreamZeroStride: UINT
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  VPDesc: VertexPatchDesc;
  VertPatch: XTL_VertexPatcher;
//  bPatched: bool;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DrawVerticesUP' +
    #13#10'(' +
    #13#10'   PrimitiveType          : 0x%.08X' +
    #13#10'   VertexCount            : 0x%.08X' +
    #13#10'   pVertexStreamZeroData  : 0x%.08X' +
    #13#10'   VertexStreamZeroStride : 0x%.08X' +
    #13#10');',
    [Ord(PrimitiveType), VertexCount, pVertexStreamZeroData,
    VertexStreamZeroStride]);
{$ENDIF}

  Xtl_EmuUpdateDeferredStates();

  VPDesc.PrimitiveType := PrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
  VPDesc.dwOffset := 0;
  VPDesc.pVertexStreamZeroData := pVertexStreamZeroData;
  VPDesc.uiVertexStreamZeroStride := VertexStreamZeroStride;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc);

  if (XTL_IsValidCurrentShader()) then
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

  VertPatch.Destroy; // Dxbx addition

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
  VertPatch: XTL_VertexPatcher;
//  bPatched: bool;
  pbData: PBYTE;
  bActiveIB: bool;
  pIndexBuffer: XTL_PIDirect3DIndexBuffer8;
  BaseIndex: UINT;

  uiNumVertices: UINT;
  uiStartIndex: UINT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DrawIndexedVertices' +
    #13#10'(' +
    #13#10'   PrimitiveType     : 0x%.08X' +
    #13#10'   VertexCount       : 0x%.08X' +
    #13#10'   pIndexData        : 0x%.08X' +
    #13#10');',
    [Ord(PrimitiveType), VertexCount, pIndexData]);
{$ENDIF}

    // update index buffer, if necessary

    if Assigned(g_pIndexBuffer) and (g_pIndexBuffer.Lock = X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
    begin
      dwSize := VertexCount*2;   // 16-bit indices

      hRet := IDirect3DDevice8_CreateIndexBuffer(g_pD3DDevice8,
          dwSize, 0, D3DFMT_INDEX16, D3DPOOL_MANAGED,
          @(g_pIndexBuffer.Lock{EmuIndexBuffer8}));

      if (FAILED(hRet)) then
          CxbxKrnlCleanup('CreateIndexBuffer Failed!');

      pData := nil;
      hRet := IDirect3DIndexBuffer8(g_pIndexBuffer.EmuIndexBuffer8).Lock(0, dwSize, pData, 0);

      if (FAILED(hRet)) then
          CxbxKrnlCleanup('IndexBuffer Lock Failed!');

      memcpy(pData, Pvoid(g_pIndexBuffer.Data), dwSize);

      IDirect3DIndexBuffer8(g_pIndexBuffer.EmuIndexBuffer8).Unlock();

      g_pIndexBuffer.Data := ULONG(pData);

      hRet := IDirect3DDevice8(g_pD3DDevice8).SetIndices(IDirect3DIndexBuffer8(g_pIndexBuffer.EmuIndexBuffer8), g_dwBaseVertexIndex);

      if (FAILED(hRet)) then
          CxbxKrnlCleanup('SetIndices Failed!');
    end;

    XTL_EmuUpdateDeferredStates();

    if (PrimitiveType = X_D3DPT_LINELOOP) or (PrimitiveType = X_D3DPT_QUADLIST) then
      EmuWarning('Unsupported PrimitiveType! (%d)', [DWORD(PrimitiveType)]);

    VPDesc.PrimitiveType := PrimitiveType;
    VPDesc.dwVertexCount := VertexCount;
    VPDesc.dwOffset := 0;
    VPDesc.pVertexStreamZeroData := nil;
    VPDesc.uiVertexStreamZeroStride := 0;
    VPDesc.hVertexShader := g_CurrentVertexShader;

    VertPatch.Create; // Dxbx addition;

    {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc);

    {$ifdef _DEBUG_TRACK_VB}
    if not g_bVBSkipStream then
    begin
    {$endif}

    bActiveIB := false;
    pIndexBuffer := nil;

    // check if there is an active index buffer
    begin
      BaseIndex := 0;

      IDirect3DDevice8(g_pD3DDevice8).GetIndices(@pIndexBuffer, BaseIndex);

      if (pIndexBuffer <> nil) then
      begin
        bActiveIB := true;
        IDirect3DIndexBuffer8(pIndexBuffer)._Release();
       end;
    end;

    // Cxbx TODO : caching (if it becomes noticably slow to recreate the buffer each time)
    if not bActiveIB then
    begin
      IDirect3DDevice8(g_pD3DDevice8).CreateIndexBuffer(VertexCount*2, D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_MANAGED, @pIndexBuffer);

      if (pIndexBuffer = NULL) then
          CxbxKrnlCleanup('Could not create index buffer! (%d bytes)', [VertexCount*2]);

      pbData := NULL;
      IDirect3DIndexBuffer8(pIndexBuffer).Lock(0, 0, {? var/@}pbData, 0);

      if (pbData = nil) then
        CxbxKrnlCleanup('Could not lock index buffer!');

      memcpy(pbData, pIndexData, VertexCount * 2);

      IDirect3DIndexBuffer8(pIndexBuffer).Unlock();

      IDirect3DDevice8(g_pD3DDevice8).SetIndices(IDirect3DIndexBuffer8(pIndexBuffer), 0);

      uiNumVertices := VertexCount;
      uiStartIndex := 0;
    end
    else
    begin
      uiNumVertices := DWORD(pIndexData) div 2 + VertexCount;
      uiStartIndex := DWORD(pIndexData) div 2;
    end;

    if (XTL_IsValidCurrentShader()) then
    begin
      IDirect3DDevice8(g_pD3DDevice8).DrawIndexedPrimitive(
        EmuPrimitiveType(VPDesc.PrimitiveType), 0, uiNumVertices, uiStartIndex, VPDesc.dwPrimitiveCount
      );
     end;

    if (not bActiveIB) then
    begin
      IDirect3DDevice8(g_pD3DDevice8).SetIndices(nil, 0);
      IDirect3DIndexBuffer8(pIndexBuffer)._Release();
    end;

    {$ifdef _DEBUG_TRACK_VB}
  end;
  {$endif}

  VertPatch.Restore();

  VertPatch.Destroy; // Dxbx addition;

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
);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  VPDesc: VertexPatchDesc;
  VertPatch: XTL_VertexPatcher;
//  bPatched: bool;
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

  EmuWarning('Using Indexed Vertices (UP)!');

  // update index buffer, if necessary
  if (g_pIndexBuffer <> nil) and (g_pIndexBuffer.Lock = X_D3DRESOURCE_LOCK_FLAG_NOSIZE) then
    CxbxKrnlCleanup('g_pIndexBuffer <> 0');

  Xtl_EmuUpdateDeferredStates();

  if (PrimitiveType = X_D3DPT_LINELOOP) or (PrimitiveType = X_D3DPT_QUADLIST) then
    EmuWarning('Unsupported PrimitiveType! (%d)', [Ord(PrimitiveType)]);


  VPDesc.PrimitiveType := PrimitiveType;
  VPDesc.dwVertexCount := VertexCount;
  VPDesc.dwOffset := 0;
  VPDesc.pVertexStreamZeroData := pVertexStreamZeroData;
  VPDesc.uiVertexStreamZeroStride := VertexStreamZeroStride;
  VPDesc.hVertexShader := g_CurrentVertexShader;

  VertPatch.Create; // Dxbx addition

  {Dxbx unused bPatched :=} VertPatch.Apply(@VPDesc);

  {$ifdef _DEBUG_TRACK_VB}
  if ( not g_bVBSkipStream) then
  begin
  {$endif}

    if (XTL_IsValidCurrentShader()) then
    begin
      IDirect3DDevice8(g_pD3DDevice8).DrawIndexedPrimitiveUP
      (
          EmuPrimitiveType(VPDesc.PrimitiveType), 0, VPDesc.dwVertexCount, VPDesc.dwPrimitiveCount, pIndexData, D3DFMT_INDEX16, VPDesc.pVertexStreamZeroData, VPDesc.uiVertexStreamZeroStride
      );
    end;

  {$ifdef _DEBUG_TRACK_VB}
   end;
  {$endif}

  VertPatch.Restore();

  VertPatch.Destroy; // Dxbx addition

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
    #13#10'   Index             : 0x%.08X' +
    #13#10'   pLight            : 0x%.08X' +
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
    #13#10'   pMaterial         : 0x%.08X' +
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
    #13#10'   Index             : 0x%.08X' +
    #13#10'   bEnable           : 0x%.08X' +
    #13#10');',
    [Index, bEnable]);
{$ENDIF}

  Result := IDirect3DDevice8(g_pD3DDevice8).LightEnable(Index, bEnable);
  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_SetRenderTarget
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

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderTarget' +
           #13#10'(' +
           #13#10'   pRenderTarget     : 0x%.08X' + // Dxbx TODO (0x%.08X)' +
           #13#10'   pNewZStencil      : 0x%.08X (0x%.08X)' +
           #13#10');',
           [pRenderTarget, {iif(pRenderTarget <> nil, pRenderTarget.EmuSurface8, nil),}
           pNewZStencil{,  iif(pNewZStencil <> nil, pNewZStencil.EmuSurface8, nil)}]);
{$ENDIF}


  pPCRenderTarget := nil;
  pPCNewZStencil := nil;

  if (pRenderTarget <> nil) then
  begin
    if Assigned(pRenderTarget.EmuSurface8) then
    begin
      EmuVerifyResourceIsRegistered(pRenderTarget);
      pPCRenderTarget := pRenderTarget.EmuSurface8;
    end
    else
      pPCRenderTarget := g_pCachedRenderTarget.EmuSurface8;
  end;

  if (pNewZStencil <> nil) then
  begin
    if Assigned(pNewZStencil.EmuSurface8) then
    begin
      EmuVerifyResourceIsRegistered(pNewZStencil);
      pPCNewZStencil := pNewZStencil.EmuSurface8;
    end
    else
      pPCNewZStencil := g_pCachedZStencilSurface.EmuSurface8;
 end;

  // Cxbx TODO : Follow that stencil!
  Result := IDirect3DDevice8(g_pD3DDevice8).SetRenderTarget(IDirect3DSurface8(pPCRenderTarget), IDirect3DSurface8(pPCNewZStencil));

  if FAILED(Result) then
    EmuWarning('SetRenderTarget failed! (0x%.08X)', [Result]);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirect3DDevice8_CreatePalette
(
  Size: X_D3DPALETTESIZE;
  ppPalette: PPX_D3DPalette
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  ppPalette^ := XTL_EmuIDirect3DDevice8_CreatePalette2(Size);
  Result := D3D_OK;
end;


function XTL_EmuIDirect3DDevice8_CreatePalette2
(
  Size: X_D3DPALETTESIZE
): PX_D3DPalette; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
const
  lk: array [0..5-1] of int = (
      256 * sizeof(D3DCOLOR),    // D3DPALETTE_256
      128 * sizeof(D3DCOLOR),    // D3DPALETTE_128
      64 * sizeof(D3DCOLOR),     // D3DPALETTE_64
      32 * sizeof(D3DCOLOR),     // D3DPALETTE_32
      32 * sizeof(D3DCOLOR)      // Dxbx addition for D3DPALETTE_MAX
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
  pPalette.Lock := $8000BEEF; // emulated reference count for palettes
  pPalette.Data := DWORD(AllocMem(lk[Ord(Size)] * sizeof(uint08)));

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
    [BoolToStr(Enable)]);
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
  ppColors^ := XTL_EmuIDirect3DPalette8_Lock2(pThis, Flags);

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

  Result := PD3DCOLOR(@(pThis.Data));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirect3DDevice8_GetVertexShaderSize
(
  Handle_: DWORD;
  pSize: PUINT
); stdcall;
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

  if Assigned(pSize) and VshHandleIsVertexShader(Handle_) then
  begin
    pD3DVertexShader := PX_D3DVertexShader(Handle_ and $7FFFFFFF);
    pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);
    pSize^ := pVertexShader.Size;
  end
  else if Assigned(pSize) then
  begin
    pSize^ := 0;
  end;

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
    pD3DVertexShader := PX_D3DVertexShader(Handle and $7FFFFFFF);
    pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);

    RealHandle := pVertexShader.Handle;
    CxbxFree(pVertexShader.pDeclaration);

    if Assigned(pVertexShader.pFunction) then
    begin
      CxbxFree(pVertexShader.pFunction);
    end;

    XTL_FreeVertexDynamicPatch(pVertexShader);

    CxbxFree(pVertexShader);
    CxbxFree(pD3DVertexShader);
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
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_GetVertexShader' +
    #13#10'(' +
    #13#10'   pHandle             : 0x%.08X' +
    #13#10');',
    [pHandle]);
{$ENDIF}

  if Assigned(pHandle) then
    pHandle ^:= g_CurrentVertexShader;

  EmuSwapFS(fsXbox);

  Result := D3D_OK;
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

  Result := IDirect3DDevice8(g_pD3DDevice8).GetVertexShaderConstant
    (
    Register_ + 96,
    pConstantData,
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

  Result := CxbxMalloc(dwSize + $1000);

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
  MultiSampleType: D3DMULTISAMPLE_TYPE
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

  // Cxbx TODO : HACK: Devices that don't support this should somehow emulate it!
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

  if (Windowed <> FALSE) then
    Windowed := FALSE;

  // Cxbx TODO : Convert from Xbox to PC!!
  PCMultiSampleType := EmuXB2PC_D3DMultiSampleFormat(DWORD(MultiSampleType));

 // Now call the real CheckDeviceMultiSampleType with the corrected parameters.
 Result := IDirect3D8(g_pD3D8).CheckDeviceMultiSampleType
    (
    Adapter,
    DeviceType,
    PCSurfaceFormat,
    Windowed,
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

  // Cxbx TODO : Actually implement this

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

  // Cxbx TODO : Implement

  EmuSwapFS(fsXbox);

  Result := FALSE;
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

  // Cxbx TODO : Implement

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

  // Cxbx TODO : Implement

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

  // Cxbx TODO : Implement

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

  // Cxbx TODO : Implement

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

  Result := IDirect3DDevice8(g_pD3DDevice8).CreateStateBlock(Type_, pToken^);

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

  // Cxbx TODO: Implement

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
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuD3DDevice_KickOff()');
{$ENDIF}

  // Cxbx TODO: Anything (kick off and NOT wait for idle)?

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

  // Cxbx TODO: I'm sure there is a better way to handle this.

  (* // Commented out by CXBX
  if not Assigned(pTexture) then
  begin
{$IFDEF DEBUG}
    DbgPrintf( 'Creating new texture...');
{$ENDIF}
    New({X_D3DResource}pTexture);
  end
  else
  begin
{$IFDEF DEBUG}
    DbgPrintf( 'pTexture: = 0x%.08X'#13#10'pTexture.EmuTexture8', [pTexture, (pTexture).EmuBaseTexture8]);
{$ENDIF}
  end; *)

  // Since this function does not specify any texture stages,
  // I guess we can assume it's just the first one.  According
  // to the XDK documentation, this function should also add
  // to the reference count.
  // Cxbx TODO: Verify texture?

//  printf( 'Setting texture...' + );
//  pTexture.EmuBaseTexture8 = EmuD3DActiveTexture[0].EmuBaseTexture8;
//  pTexture := EmuD3DActiveTexture[0];
//  pTexture.Data := (X_D3DRESOURCE_DATA_FLAG_SPECIAL or X_D3DRESOURCE_DATA_FLAG_TEXCLON);

//  printf( 'Adding reference...' + );
//  IDirect3DBaseTexture8(pTexture.EmuBaseTexture8)._AddRef();

//  EmuSwapFS(fsXbox);
//  EmuIDirect3DResource8_AddRef(pTexture);
//  EmuSwapFS(fsWindows);

//  Result := IDirect3DDevice8(g_pD3DDevice8).GetTexture(0, &(*ppTexture).EmuBaseTexture8);

//  printf( 'Verifying resource...' + );
//  EmuVerifyResourceIsRegistered((*ppTexture));

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

  // Cxbx TODO: Anything?

  EmuSwapFS(fsXbox);
end;

// * func: EmuD3DDevice_SetStateUP (D3D::CDevice::SetStateUP)
procedure XTL_EmuIDirect3DDevice8_SetStateUP(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuD3D8 : EmuD3DDevice_SetStateUP()');
{$ENDIF}

  // Cxbx TODO: Anything?

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
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetSwapCallback' +
      #13#10'(' +
      #13#10'   pCallback           : 0x%.08X' +
      #13#10');',
      [PPointer(@pCallback)^]);

  DbgPrintf('pCallback: = 0x%.08X', [PPointer(@pCallback)^]);

  g_pSwapCallback := pCallback;

  EmuSwapFS();   // XBox FS
end;

function XTL_EmuIDirect3DDevice8_PersistDisplay(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS();  // Win2k/XP FS

  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_PersistDisplay()');

  Result := S_OK;

  // Cxbx TODO: If this functionality is ever really needed, an idea for
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

  EmuSwapFS();  // Xbox FS
end;

procedure XTL_EmuIDirect3DDevice8_Unknown1(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS();  // Win2k/XP FS

  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_Unknown1()');

  // Cxbx TODO: Find out what this actually is.
  // This function was only found in Run Like Hell (5233) @ 0x11FCD0.
  // So far, this function hasn't been found in any other XDKs.  Since
  // the only major thing going on inside of it is a call to the kernel
  // function AvSendTVEncoderOption, we can probably ignore it.

  EmuSwapFS();  // Xbox FS
end;

function XTL_EmuIDirect3DDevice8_PrimeVertexCache
(
  VertexCount: UINT;
  pIndexData: PWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_PrimeVertexCache' +
      #13#10'(' +
      #13#10'   VertexCount           : 0x%.08X' +
      #13#10'   pIndexData            : 0x%.08X' +
      #13#10');',
      [VertexCount, pIndexData]);

  // Cxbx TODO: Implement
  EmuWarning('PrimeVertexCache is not supported!');

  EmuSwapFS();  // Win2k/XP FS

  Result := S_OK;
end;

function XTL_EmuIDirect3DDevice8_SetRenderState_SampleAlpha
(
  dwSampleAlpha: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_SampleAlpha' +
      #13#10'(' +
      #13#10'   dwSampleAlpha         : 0x%.08X' +
      #13#10');',
      [dwSampleAlpha]);

  // Cxbx TODO: Implement?

  EmuWarning('SampleAlpha not supported!');

  EmuSwapFS();  // Xbox FS

  Result := S_OK;
end;

// Dxbx: This argument makes the 'register' calling convention
// functionally equivalent to the 'fastcall' calling convention.
// Quote from http://www.codeguru.com/forum/showthread.php?t=466266 :
// They differ as follows:
// register: (left to right) EAX, EDX, ECX, remaining pushed on stack right to left, callee cleans
// fastcall: (left to right) ECX, EDX, remaining pushed on stack left to right, callee cleans
procedure XTL_EmuIDirect3DDevice8_SetRenderState_Deferred(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Value: DWORD;
  {1 ECX}State: DWORD // Dxbx note: The first argument should be here, to force it into ECX
  ); register; // __fastcall in Cxbx
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_SetRenderState_Deferred' +
      #13#10'(' +
      #13#10'   State               : 0x%.08X' +
      #13#10'   Value               : 0x%.08X' +
      #13#10');',
      [State, Value]);

  // Cxbx TODO: HACK: Technically, this function doesn't need to be emulated.
  // The location of EmuD3DDeferredRenderState for 3911 isn't correct and at
  // the time of writing, I don't understand how to fix it.  Until then,
  // I'm going to implement this in a reckless manner.  When the offset for
  // EmuD3DDeferredRenderState is fixed for 3911, this function should be
  // obsolete!

  if (State > 81) and (State < 116) then
    XTL_EmuD3DDeferredRenderState[State-82] := Value
  else
    CxbxKrnlCleanup('Unknown Deferred RenderState! (%d)', [State]);

  (*
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
  *)

  EmuSwapFS();  // Xbox FS
end;

function XTL_EmuIDirect3DDevice8_DeleteStateBlock
(
  Token: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS();  // Win2k/XP FS

  DbgPrintf('EmuD3D8 : EmuIDirect3DDevice8_DeleteStateBlock' +
      #13#10'(' +
      #13#10'   Token               : 0x%.08X' +
      #13#10');',
      [Token]);

  Result := IDirect3DDevice8(g_pD3DDevice8).DeleteStateBlock(Token);

  EmuSwapFS();  // Xbox FS
end;

exports
  XTL_EmuD3DCleanup,
  XTL_EmuD3DInit,

  XTL_EmuGet2DSurfaceDesc,
  XTL_EmuGet2DSurfaceDescD, // TODO : Fix wrong prefix!

  XTL_EmuIDevice3D8_KickOff name PatchPrefix + 'D3D_KickOff',

  XTL_EmuIDirect3D8_AllocContiguousMemory name PatchPrefix + 'D3D_AllocContiguousMemory@8',
  XTL_EmuIDirect3D8_CheckDeviceFormat name PatchPrefix + 'Direct3D_CheckDeviceFormat',
  XTL_EmuIDirect3D8_CheckDeviceMultiSampleType name PatchPrefix + 'Direct3D_CheckDeviceMultiSampleType',
  XTL_EmuIDirect3D8_CreateDevice name PatchPrefix + 'Direct3D_CreateDevice',
  XTL_EmuIDirect3D8_EnumAdapterModes name PatchPrefix + 'Direct3D_EnumAdapterModes',
  XTL_EmuIDirect3D8_GetAdapterDisplayMode name PatchPrefix + 'Direct3D_GetAdapterDisplayMode',
  XTL_EmuIDirect3D8_GetAdapterModeCount name PatchPrefix + 'Direct3D_GetAdapterModeCount',
  XTL_EmuIDirect3D8_GetDeviceCaps name PatchPrefix + 'Direct3D_GetDeviceCaps',
  XTL_EmuIDirect3D8_KickOffAndWaitForIdle name PatchPrefix + 'KickOffAndWaitForIdle',
  XTL_EmuIDirect3D8_KickOffAndWaitForIdle2 name PatchPrefix + 'KickOffAndWaitForIdle2', // no prefix also ?
  XTL_EmuIDirect3D8_SetPushBufferSize name PatchPrefix + 'Direct3D_SetPushBufferSize',

  XTL_EmuIDirect3DBaseTexture8_GetLevelCount name PatchPrefix + 'D3DBaseTexture_GetLevelCount',

  XTL_EmuIDirect3DCubeTexture8_LockRect  name PatchPrefix + 'D3DCubeTexture8_LockRect',

  XTL_EmuIDirect3DDevice8_AddRef name PatchPrefix + 'D3DDevice_AddRef@0',
  XTL_EmuIDirect3DDevice8_ApplyStateBlock name PatchPrefix + 'D3DDevice_ApplyStateBlock',
  XTL_EmuIDirect3DDevice8_BackFillMode name PatchPrefix + 'D3DDevice_BackFillMode',
  XTL_EmuIDirect3DDevice8_Begin name PatchPrefix + 'D3DDevice_Begin',
  XTL_EmuIDirect3DDevice8_BeginPush name PatchPrefix + 'D3DDevice_BeginPushBuffer@4',
  // XTL_EmuIDirect3DDevice8_BeginStateBig name PatchPrefix + 'D3DDevice_BeginStateBig', // MARKED OUT BY CXBX
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
  XTL_EmuIDirect3DDevice8_DeleteStateBlock name PatchPrefix + 'D3DDevice_DeleteStateBlock', // Dxbx TODO : Check name
  XTL_EmuIDirect3DDevice8_DeleteVertexShader name PatchPrefix + 'D3DDevice_DeleteVertexShader@4',
  XTL_EmuIDirect3DDevice8_DrawIndexedVertices name PatchPrefix + 'D3DDevice_DrawIndexedVertices@12',
  XTL_EmuIDirect3DDevice8_DrawIndexedVerticesUP name PatchPrefix + 'D3DDevice_DrawIndexedVerticesUP',
  XTL_EmuIDirect3DDevice8_DrawRectPatch name PatchPrefix + 'D3DDevice_DrawRectPatch',
  XTL_EmuIDirect3DDevice8_DrawVertices name PatchPrefix + 'D3DDevice_DrawVertices@12',
  XTL_EmuIDirect3DDevice8_DrawVerticesUP name PatchPrefix + 'D3DDevice_DrawVerticesUP@16',
  XTL_EmuIDirect3DDevice8_EnableOverlay name PatchPrefix + 'D3DDevice_EnableOverlay@4',
  XTL_EmuIDirect3DDevice8_End name PatchPrefix + 'D3DDevice_End',
  XTL_EmuIDirect3DDevice8_EndPush name PatchPrefix + 'D3DDevice_EndPushBuffer@0',
  XTL_EmuIDirect3DDevice8_EndStateBlock name PatchPrefix + 'D3DDevice_EndStateBlock',
  XTL_EmuIDirect3DDevice8_EndVisibilityTest name PatchPrefix + 'D3DDevice_EndVisibilityTest@4',
  XTL_EmuIDirect3DDevice8_GetBackBuffer name PatchPrefix + 'D3DDevice_GetBackBuffer',
  XTL_EmuIDirect3DDevice8_GetBackBuffer2 name PatchPrefix + 'D3DDevice_GetBackBuffer2@4',
  XTL_EmuIDirect3DDevice8_GetCreationParameters name PatchPrefix + 'D3DDevice_GetCreationParameters',
  XTL_EmuIDirect3DDevice8_GetDepthStencilSurface name PatchPrefix + 'D3DDevice_GetDepthStencilSurface',
  XTL_EmuIDirect3DDevice8_GetDepthStencilSurface2 name PatchPrefix + 'D3DDevice_GetDepthStencilSurface2',
  XTL_EmuIDirect3DDevice8_GetDeviceCaps name PatchPrefix + 'D3DDevice_GetDeviceCaps',
  XTL_EmuIDirect3DDevice8_GetDisplayFieldStatus name PatchPrefix + 'D3DDevice_GetDisplayFieldStatus',
  XTL_EmuIDirect3DDevice8_GetDisplayMode name PatchPrefix + 'D3DDevice_GetDisplayMode',
  XTL_EmuIDirect3DDevice8_GetGammaRamp name PatchPrefix + 'D3DDevice_GetGammaRamp',
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
  XTL_EmuIDirect3DDevice8_InsertFence name PatchPrefix + 'D3DDevice_InsertFence',
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
  XTL_EmuIDirect3DDevice8_SetFlickerFilter name PatchPrefix + 'D3DDevice_SetFlickerFilter',
  XTL_EmuIDirect3DDevice8_SetGammaRamp name PatchPrefix + 'Direct3D8_SetGammaRamp',
  XTL_EmuIDirect3DDevice8_SetIndices name PatchPrefix + 'D3DDevice_SetIndices',
  XTL_EmuIDirect3DDevice8_SetLight name PatchPrefix + 'D3DDevice_SetLight',
  XTL_EmuIDirect3DDevice8_SetMaterial name PatchPrefix + 'D3DDevice_SetMaterial',
  XTL_EmuIDirect3DDevice8_SetPalette name PatchPrefix + 'D3DDevice_SetPalette',
  XTL_EmuIDirect3DDevice8_SetPixelShader name PatchPrefix + 'D3DDevice_SetPixelShader',
  XTL_EmuIDirect3DDevice8_SetPixelShaderConstant name PatchPrefix + 'D3DDevice_SetPixelShaderConstant',
  XTL_EmuIDirect3DDevice8_SetPixelShaderProgram name PatchPrefix + 'D3DDevice_SetPixelShaderProgram',
  XTL_EmuIDirect3DDevice8_SetRenderState_BackFillMode name PatchPrefix + 'D3DDevice_SetRenderState_BackFillMode',
  XTL_EmuIDirect3DDevice8_SetRenderState_CullMode name PatchPrefix + 'D3DDevice_SetRenderState_CullMode',
  XTL_EmuIDirect3DDevice8_SetRenderState_Deferred name PatchPrefix + 'D3DDevice_SetRenderState_Deferred',
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
  XTL_EmuIDirect3DDevice8_SetTileNoWait name PatchPrefix + '?SetTileNoWait@D3D@@YGXKPBU_D3DTILE@@@Z',
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
  XTL_EmuIDirect3DDevice8_Unknown1 name PatchPrefix + 'D3DDevice_Unknown', // Dxbx TODO : Fix wrong prefix!
  XTL_EmuIDirect3DDevice8_UpdateOverlay name PatchPrefix + 'D3DDevice_UpdateOverlay',

  XTL_EmuIDirect3DPalette8_Lock name PatchPrefix + 'D3DDevice_Lock', // Dxbx TODO : Fix wrong prefix!
  XTL_EmuIDirect3DPalette8_Lock2 name PatchPrefix + 'D3DDevice_Lock2', // Dxbx TODO : Fix wrong prefix!

  XTL_EmuIDirect3DResource8_AddRef name PatchPrefix + 'D3DResource_AddRef',
  XTL_EmuIDirect3DResource8_BlockUntilNotBusy name PatchPrefix + 'D3DResource_BlockUntilNotBusy',
  XTL_EmuIDirect3DResource8_GetType name PatchPrefix + 'D3DResource_GetType',
  XTL_EmuIDirect3DResource8_IsBusy name PatchPrefix + 'D3DResource_IsBusy',
  XTL_EmuIDirect3DResource8_Register name PatchPrefix + 'D3DResource_Register',
  XTL_EmuIDirect3DResource8_Release name PatchPrefix + 'D3DResource_Release',

  XTL_EmuIDirect3DSurface8_GetDesc name PatchPrefix + 'D3DSurface_GetDesc',
  XTL_EmuIDirect3DSurface8_LockRect name PatchPrefix + 'D3DSurface_LockRect@16',

  XTL_EmuIDirect3DTexture8_GetLevelDesc name PatchPrefix + 'D3DTexture_GetLevelDesc',
  XTL_EmuIDirect3DTexture8_GetSurfaceLevel name PatchPrefix + 'D3DTexture_GetSurfaceLevel',
  XTL_EmuIDirect3DTexture8_GetSurfaceLevel2 name PatchPrefix + 'D3DTexture_GetSurfaceLevel2',
  XTL_EmuIDirect3DTexture8_LockRect name PatchPrefix + 'D3DTexture_LockRect',

  XTL_EmuIDirect3DVertexBuffer8_GetDesc name PatchPrefix + 'D3DVertexBuffer_GetDesc',
  XTL_EmuIDirect3DVertexBuffer8_Lock name PatchPrefix + 'D3DVertexBuffer_Lock',
  XTL_EmuIDirect3DVertexBuffer8_Lock2 name PatchPrefix + 'D3DVertexBuffer_Lock2',

  XTL_EmuIDirect3DVolumeTexture8_LockBox name PatchPrefix + 'D3DVolumeTexture_LockBox',

  XTL_EmuLock2DSurface name PatchPrefix + 'Lock2DSurface';

end.
