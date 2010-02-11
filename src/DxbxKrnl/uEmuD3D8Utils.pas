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
unit uEmuD3D8Utils;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // Directx
  D3DX8,
  Direct3D,
  Direct3D8,
  DirectDraw,
  // Dxbx
  uEmuD3D8Types;

type
  PIDirect3DDevice8 = ^IDirect3DDevice8;
  PIDirect3DSurface8 = ^IDirect3DSurface8;
  PIDirect3DVertexBuffer8 = ^IDirect3DVertexBuffer8;
  PIDirect3DIndexBuffer8 = ^IDirect3DIndexBuffer8;
  PIDirect3DTexture8 = ^IDirect3DTexture8;
  PIDirect3DCubeTexture8 = ^IDirect3DCubeTexture8;
  PIDirect3DVolumeTexture8 = ^IDirect3DVolumeTexture8;

  LPD3DXBUFFER = ID3DXBuffer;
  PLPD3DXBUFFER = PID3DXBuffer; // = ^LPD3DXBUFFER;

  LPCVOID = Pointer; // Pointer to a constant of any type
  PPD3DCOLOR = ^PD3DCOLOR;

function iif(AValue: Boolean; const ATrue: TD3DDevType; const AFalse: TD3DDevType): TD3DDevType; overload;
function iif(AValue: Boolean; const ATrue: IDirect3DSurface8; const AFalse: IDirect3DSurface8): IDirect3DSurface8; overload;
function iif(AValue: Boolean; const ATrue: IDirect3DBaseTexture8; const AFalse: IDirect3DBaseTexture8): IDirect3DBaseTexture8; overload;

function IDirect3D8_CreateDevice(const aDirect3D8: IDirect3D8;
  Adapter: UINT; DeviceType: D3DDEVTYPE; hFocusWindow: HWND;
  BehaviorFlags: DWORD; pPresentationParameters: PX_D3DPRESENT_PARAMETERS;
  ppReturnedDeviceInterface: PIDirect3DDevice8): HRESULT;
function IDirect3DDevice8_GetRenderTarget(const aDirect3DDevice8: IDirect3DDevice8;
  ppRenderTarget: PIDirect3DSurface8): HResult;
function IDirect3DDevice8_GetDepthStencilSurface(const aDirect3DDevice8: IDirect3DDevice8;
  ppZStencilSurface: PIDirect3DSurface8): HResult;
function IDirect3DDevice8_CreateVertexBuffer(const aDirect3DDevice8: IDirect3DDevice8;
  Length: LongWord; Usage, FVF: DWord; Pool: TD3DPool;
  ppVertexBuffer: PIDirect3DVertexBuffer8): HResult;
function IDirect3DDevice8_CreateImageSurface(const aDirect3DDevice8: IDirect3DDevice8;
  Width, Height: LongWord; Format: TD3DFormat;
  ppSurface: PIDirect3DSurface8): HResult;
function IDirect3DDevice8_GetBackBuffer(const aDirect3DDevice8: IDirect3DDevice8;
  BackBuffer: LongWord; _Type: TD3DBackBufferType;
  ppBackBuffer: PIDirect3DSurface8): HResult;
function IDirect3DDevice8_CreateIndexBuffer(const aDirect3DDevice8: IDirect3DDevice8;
  Length: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppIndexBuffer: PIDirect3DIndexBuffer8): HResult;
function IDirect3DDevice8_CreateTexture(const aDirect3DDevice8: IDirect3DDevice8;
  Width, Height, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppTexture: PIDirect3DTexture8): HResult;
function IDirect3DDevice8_CreateCubeTexture(const aDirect3DDevice8: IDirect3DDevice8;
  EdgeLength, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppCubeTexture: PIDirect3DCubeTexture8): HResult;
function IDirect3DDevice8_CreateVolumeTexture(const aDirect3DDevice8: IDirect3DDevice8;
  Width, Height, Depth, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppVolumeTexture: PIDirect3DVolumeTexture8): HResult;
function IDirect3DTexture8_GetSurfaceLevel(const aDirect3DTexture8: IDirect3DTexture8;
  Level: LongWord;
  ppSurfaceLevel: PIDirect3DSurface8): HResult;

function D3DMATRIX_MULTIPLY(const a, b: D3DMATRIX): D3DMATRIX;

implementation

function iif(AValue: Boolean; const ATrue: TD3DDevType; const AFalse: TD3DDevType): TD3DDevType; overload;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function iif(AValue: Boolean; const ATrue: IDirect3DSurface8; const AFalse: IDirect3DSurface8): IDirect3DSurface8; overload;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function iif(AValue: Boolean; const ATrue: IDirect3DBaseTexture8; const AFalse: IDirect3DBaseTexture8): IDirect3DBaseTexture8; overload;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IDirect3D8_CreateDevice(const aDirect3D8: IDirect3D8;
  Adapter: UINT; DeviceType: D3DDEVTYPE; hFocusWindow: HWND;
  BehaviorFlags: DWORD; pPresentationParameters: PX_D3DPRESENT_PARAMETERS;
  ppReturnedDeviceInterface: PIDirect3DDevice8): HRESULT;
var
  TmpIDirect3DDevice8: IDirect3DDevice8;
begin
  Result := aDirect3D8.CreateDevice(
    Adapter,
    DeviceType,
    hFocusWindow,
    BehaviorFlags,
    {var}PD3DPRESENT_PARAMETERS(pPresentationParameters)^,
    {out}TmpIDirect3DDevice8);

  PPointer(ppReturnedDeviceInterface)^ := Pointer(TmpIDirect3DDevice8);
  Pointer(TmpIDirect3DDevice8) := nil;
end;

function IDirect3DDevice8_GetRenderTarget(const aDirect3DDevice8: IDirect3DDevice8;
  ppRenderTarget: PIDirect3DSurface8): HResult;
var
  TmpIDirect3DSurface8: IDirect3DSurface8;
begin
  Result := aDirect3DDevice8.GetRenderTarget(
    {out}TmpIDirect3DSurface8);

  PPointer(ppRenderTarget)^ := Pointer(TmpIDirect3DSurface8);
  Pointer(TmpIDirect3DSurface8) := nil;
end;

function IDirect3DDevice8_GetDepthStencilSurface(const aDirect3DDevice8: IDirect3DDevice8;
  ppZStencilSurface: PIDirect3DSurface8): HResult;
var
  TmpIDirect3DSurface8: IDirect3DSurface8;
begin
  Result := aDirect3DDevice8.GetDepthStencilSurface(
    {out}TmpIDirect3DSurface8);

  PPointer(ppZStencilSurface)^ := Pointer(TmpIDirect3DSurface8);
  Pointer(TmpIDirect3DSurface8) := nil;
end;

function IDirect3DDevice8_CreateVertexBuffer(const aDirect3DDevice8: IDirect3DDevice8;
  Length: LongWord; Usage, FVF: DWord; Pool: TD3DPool;
  ppVertexBuffer: PIDirect3DVertexBuffer8): HResult;
var
  TmpIDirect3DVertexBuffer8: IDirect3DVertexBuffer8;
begin
  Result := aDirect3DDevice8.CreateVertexBuffer(
    Length, Usage, FVF, Pool,
    {out}TmpIDirect3DVertexBuffer8);

  PPointer(ppVertexBuffer)^ := Pointer(TmpIDirect3DVertexBuffer8);
  Pointer(TmpIDirect3DVertexBuffer8) := nil;
end;

function IDirect3DDevice8_CreateImageSurface(const aDirect3DDevice8: IDirect3DDevice8;
  Width, Height: LongWord; Format: TD3DFormat;
  ppSurface: PIDirect3DSurface8): HResult;
var
  TmpIDirect3DSurface8: IDirect3DSurface8;
begin
  Result := aDirect3DDevice8.CreateImageSurface(
    Width, Height, Format,
    {out}TmpIDirect3DSurface8);

  PPointer(ppSurface)^ := Pointer(TmpIDirect3DSurface8);
  Pointer(TmpIDirect3DSurface8) := nil;
end;

function IDirect3DDevice8_GetBackBuffer(const aDirect3DDevice8: IDirect3DDevice8;
  BackBuffer: LongWord; _Type: TD3DBackBufferType;
  ppBackBuffer: PIDirect3DSurface8): HResult;
var
  TmpIDirect3DSurface8: IDirect3DSurface8;
begin
  Result := aDirect3DDevice8.GetBackBuffer(
    BackBuffer, _Type,
    {out}TmpIDirect3DSurface8);

  PPointer(ppBackBuffer)^ := Pointer(TmpIDirect3DSurface8);
  Pointer(TmpIDirect3DSurface8) := nil;
end;

function IDirect3DDevice8_CreateIndexBuffer(const aDirect3DDevice8: IDirect3DDevice8;
  Length: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppIndexBuffer: PIDirect3DIndexBuffer8): HResult;
var
  TmpIDirect3DIndexBuffer8: IDirect3DIndexBuffer8;
begin
  Result := aDirect3DDevice8.CreateIndexBuffer(
    Length, Usage, Format, Pool,
    {out}TmpIDirect3DIndexBuffer8);

  PPointer(ppIndexBuffer)^ := Pointer(TmpIDirect3DIndexBuffer8);
  Pointer(TmpIDirect3DIndexBuffer8) := nil;
end;

function IDirect3DDevice8_CreateTexture(const aDirect3DDevice8: IDirect3DDevice8;
  Width, Height, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppTexture: PIDirect3DTexture8): HResult;
var
  TmpIDirect3DTexture8: IDirect3DTexture8;
begin
  Result := aDirect3DDevice8.CreateTexture(
    Width, Height, Levels, Usage, Format, Pool,
    {out}TmpIDirect3DTexture8);

  PPointer(ppTexture)^ := Pointer(TmpIDirect3DTexture8);
  Pointer(TmpIDirect3DTexture8) := nil;
end;

function IDirect3DDevice8_CreateCubeTexture(const aDirect3DDevice8: IDirect3DDevice8;
  EdgeLength, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppCubeTexture: PIDirect3DCubeTexture8): HResult;
var
  TmpIDirect3DCubeTexture8: IDirect3DCubeTexture8;
begin
  Result := aDirect3DDevice8.CreateCubeTexture(
    EdgeLength, Levels, Usage, Format, Pool,
    {out}TmpIDirect3DCubeTexture8);

  PPointer(ppCubeTexture)^ := Pointer(TmpIDirect3DCubeTexture8);
  Pointer(TmpIDirect3DCubeTexture8) := nil;
end;

function IDirect3DDevice8_CreateVolumeTexture(const aDirect3DDevice8: IDirect3DDevice8;
  Width, Height, Depth, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppVolumeTexture: PIDirect3DVolumeTexture8): HResult;
var
  TmpIDirect3DVolumeTexture8: IDirect3DVolumeTexture8;
begin
  Result := aDirect3DDevice8.CreateVolumeTexture(
    Width, Height, Depth, Levels, Usage, Format, Pool,
    {out}TmpIDirect3DVolumeTexture8);

  PPointer(ppVolumeTexture)^ := Pointer(TmpIDirect3DVolumeTexture8);
  Pointer(TmpIDirect3DVolumeTexture8) := nil;
end;

function IDirect3DTexture8_GetSurfaceLevel(const aDirect3DTexture8: IDirect3DTexture8;
  Level: LongWord;
  ppSurfaceLevel: PIDirect3DSurface8): HResult;
var
  TmpIDirect3DSurface8: IDirect3DSurface8;
begin
  Result := aDirect3DTexture8.GetSurfaceLevel(
    Level,
    {out}TmpIDirect3DSurface8);

  PPointer(ppSurfaceLevel)^ := Pointer(TmpIDirect3DSurface8);
  Pointer(TmpIDirect3DSurface8) := nil;
end;

function D3DMATRIX_MULTIPLY(const a, b: D3DMATRIX): D3DMATRIX;
begin
  asm int 13; end; // Dxbx TODO : Implement
end;

end.

