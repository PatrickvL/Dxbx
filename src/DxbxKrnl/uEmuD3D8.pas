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
  // Dxbx
  uLog,
  uTypes,
  uXbe,
  uEmuShared,
  uEmuFS;

procedure XTL__EmuD3DInit(XbeHeader: PXBE_HEADER; XbeHeaderSize: UInt32);

implementation


procedure XTL__EmuD3DInit(XbeHeader: PXBE_HEADER; XbeHeaderSize: UInt32);
(*
var
  dwThreadId: DWORD;
  hThread: THandle;
  hDupHandle: THandle;
*)
begin
(*
  g_EmuShared.GetXBVideo(@g_XBVideo);

  if g_XBVideo.GetFullscreen() then
    CxbxKrnl_hEmuParent := 0;

  // cache XbeHeader and size of XbeHeader
  g_XbeHeader     := XbeHeader;
  g_XbeHeaderSize := XbeHeaderSize;

  // create timing thread
  begin
    hThread := CreateThread(0, 0, EmuUpdateTickCount, 0, 0, {var}dwThreadId);

    // we must duplicate this handle in order to retain Suspend/Resume thread rights from a remote thread
    begin
      hDupHandle := 0;

      DuplicateHandle(GetCurrentProcess(), hThread, GetCurrentProcess(), @hDupHandle, 0, False, DUPLICATE_SAME_ACCESS);

      CxbxKrnlRegisterThread(hDupHandle);
    end;
  end;

  // create the create device proxy thread
  begin
    CreateThread(0, 0, EmuCreateDeviceProxy, 0, 0, {var}dwThreadId);
  end;

  // create window message processing thread
  begin
    g_bRenderWindowActive := False;

    CreateThread(0, 0, EmuRenderWindow, 0, 0, {var}dwThreadId);

    while not g_bRenderWindowActive do
      Sleep(10);

    Sleep(50);
  end;

  // create Direct3D8 and retrieve caps
  begin
    using namespace XTL;

    // xbox Direct3DCreate8 returns "1" always, so we need our own ptr
    g_pD3D8 := Direct3DCreate8(D3D_SDK_VERSION);

    if g_pD3D8 = 0 then
      CxbxKrnlCleanup('Could not initialize Direct3D8 not ');

    D3DDEVTYPE DevType := (g_XBVideo.GetDirect3DDevice() = 0) ? D3DDEVTYPE_HAL : D3DDEVTYPE_REF;

    g_pD3D8.GetDeviceCaps(g_XBVideo.GetDisplayAdapter(), DevType, @g_D3DCaps);
  end;

  SetFocus(g_hEmuWindow);

  // create default device
  begin
    XTL.X_D3DPRESENT_PARAMETERS PresParam;

    ZeroMemory(@PresParam, SizeOf(PresParam));

    PresParam.BackBufferWidth  := 640;
    PresParam.BackBufferHeight := 480;
    PresParam.BackBufferFormat := 6; (* X_D3DFMT_A8R8G8B8 * )
    PresParam.BackBufferCount  := 1;
    PresParam.EnableAutoDepthStencil := TRUE;
    PresParam.AutoDepthStencilFormat := $2A; (* X_D3DFMT_D24S8 * )
    PresParam.SwapEffect := XTL.D3DSWAPEFFECT_DISCARD;

    EmuSwapFS();    // XBox FS
    XTL.EmuIDirect3D8_CreateDevice(0, XTL.D3DDEVTYPE_HAL, 0, $00000040, @PresParam, @g_pD3DDevice8);
    EmuSwapFS();    // Win2k/XP FS
  end;
*)
end;

end.

