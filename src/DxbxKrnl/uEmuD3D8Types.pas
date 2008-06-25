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

unit uEmuD3D8Types;

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Windows
  Windows,
  // Directx
  Direct3D9
  ;



type
  X_D3DFORMAT = Dword;

  _X_D3DPRESENT_PARAMETERS = record
    BackBufferWidth: UINT;
    BackBufferHeight: UINT;
    BackBufferFormat: X_D3DFORMAT;
    BackBufferCount: UINT;
    MultiSampleType: D3DMULTISAMPLE_TYPE;
    SwapEffect: D3DSWAPEFFECT;
    hDeviceWindow: HWND;
    Windowed: LongBool;
    EnableAutoDepthStencil: LongBool;
    AutoDepthStencilFormat: X_D3DFORMAT;
    Flags: DWORD;
    FullScreen_RefreshRateInHz: UINT;
    FullScreen_PresentationInterval: UINT;
    BufferSurfaces: array[0..2] of IDirect3DSurface9;
    DepthStencilSurface: IDirect3DSurface9;
  end;
  
  X_D3DPRESENT_PARAMETERS = _X_D3DPRESENT_PARAMETERS;

  _X_D3DDISPLAYMODE = record
    Width: Integer;
    Height: Integer;
    RefreshRate: Integer;
    Flags: DWord;
    Format: X_D3DFORMAT;
  end;

  X_D3DDISPLAYMODE = _X_D3DDISPLAYMODE;



implementation

end.

