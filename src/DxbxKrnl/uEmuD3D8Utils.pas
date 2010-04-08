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
  // DirectX
  Direct3D, // PD3DCOLOR
  Direct3D8, // IDirect3DDevice8
  D3DX8, // ID3DXBuffer
  // Dxbx
  uEmuD3D8Types;

type
  LPD3DXBUFFER = ID3DXBuffer;
  PLPD3DXBUFFER = PID3DXBuffer; // = ^LPD3DXBUFFER;

  LPCVOID = Pointer; // Pointer to a constant of any type
  PPD3DCOLOR = ^PD3DCOLOR;

function iif(AValue: Boolean; const ATrue: TD3DDevType; const AFalse: TD3DDevType): TD3DDevType; overload;
function iif(AValue: Boolean; const ATrue: XTL_PIDirect3DSurface8; const AFalse: XTL_PIDirect3DSurface8): XTL_PIDirect3DSurface8; overload;
function iif(AValue: Boolean; const ATrue: XTL_PIDirect3DBaseTexture8; const AFalse: XTL_PIDirect3DBaseTexture8): XTL_PIDirect3DBaseTexture8; overload;

function IDirect3DDevice8_GetRenderTarget(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  ppRenderTarget: PIDirect3DSurface8): HResult;
function IDirect3DDevice8_GetDepthStencilSurface(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  ppZStencilSurface: PIDirect3DSurface8): HResult;
function IDirect3DDevice8_CreateVertexBuffer(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  Length: LongWord; Usage, FVF: DWord; Pool: TD3DPool;
  ppVertexBuffer: PIDirect3DVertexBuffer8): HResult;
function IDirect3DDevice8_CreateImageSurface(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  Width, Height: LongWord; Format: TD3DFormat;
  ppSurface: PIDirect3DSurface8): HResult;
function IDirect3DDevice8_GetBackBuffer(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  BackBuffer: LongWord; _Type: TD3DBackBufferType;
  ppBackBuffer: PIDirect3DSurface8): HResult;
function IDirect3DDevice8_CreateIndexBuffer(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  Length: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppIndexBuffer: PIDirect3DIndexBuffer8): HResult;
function IDirect3DDevice8_CreateTexture(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  Width, Height, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppTexture: PIDirect3DTexture8): HResult;
function IDirect3DDevice8_CreateCubeTexture(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  EdgeLength, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppCubeTexture: PIDirect3DCubeTexture8): HResult;
function IDirect3DDevice8_CreateVolumeTexture(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  Width, Height, Depth, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppVolumeTexture: PIDirect3DVolumeTexture8): HResult;

function D3DMATRIX_MULTIPLY(const a, b: D3DMATRIX): D3DMATRIX;

implementation

function iif(AValue: Boolean; const ATrue: TD3DDevType; const AFalse: TD3DDevType): TD3DDevType; overload;
// Branch:Dxbx  Translator:Shadow_Tj  Done:100
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function iif(AValue: Boolean; const ATrue: XTL_PIDirect3DSurface8; const AFalse: XTL_PIDirect3DSurface8): XTL_PIDirect3DSurface8; overload;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function iif(AValue: Boolean; const ATrue: XTL_PIDirect3DBaseTexture8; const AFalse: XTL_PIDirect3DBaseTexture8): XTL_PIDirect3DBaseTexture8; overload;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IDirect3DDevice8_GetRenderTarget(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  ppRenderTarget: PIDirect3DSurface8): HResult;
begin
  Result := IDirect3DDevice8(aDirect3DDevice8).GetRenderTarget(ppRenderTarget);
end;

function IDirect3DDevice8_GetDepthStencilSurface(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  ppZStencilSurface: PIDirect3DSurface8): HResult;
begin
  Result := IDirect3DDevice8(aDirect3DDevice8).GetDepthStencilSurface(ppZStencilSurface);
end;

function IDirect3DDevice8_CreateVertexBuffer(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  Length: LongWord; Usage, FVF: DWord; Pool: TD3DPool;
  ppVertexBuffer: PIDirect3DVertexBuffer8): HResult;
begin
  Result := IDirect3DDevice8(aDirect3DDevice8).CreateVertexBuffer(
    Length, Usage, FVF, Pool,
    ppVertexBuffer);
end;

function IDirect3DDevice8_CreateImageSurface(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  Width, Height: LongWord; Format: TD3DFormat;
  ppSurface: PIDirect3DSurface8): HResult;
begin
  Result := IDirect3DDevice8(aDirect3DDevice8).CreateImageSurface(
    Width, Height, Format,
    ppSurface);
end;

function IDirect3DDevice8_GetBackBuffer(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  BackBuffer: LongWord; _Type: TD3DBackBufferType;
  ppBackBuffer: PIDirect3DSurface8): HResult;
begin
  Result := IDirect3DDevice8(aDirect3DDevice8).GetBackBuffer(
    BackBuffer, _Type,
    ppBackBuffer);
end;

function IDirect3DDevice8_CreateIndexBuffer(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  Length: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppIndexBuffer: PIDirect3DIndexBuffer8): HResult;
begin
  Result := IDirect3DDevice8(aDirect3DDevice8).CreateIndexBuffer(
    Length, Usage, Format, Pool,
    ppIndexBuffer);
end;

function IDirect3DDevice8_CreateTexture(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  Width, Height, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppTexture: PIDirect3DTexture8): HResult;
begin
  Result := IDirect3DDevice8(aDirect3DDevice8).CreateTexture(
    Width, Height, Levels, Usage, Format, Pool,
    ppTexture);
end;

function IDirect3DDevice8_CreateCubeTexture(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  EdgeLength, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppCubeTexture: PIDirect3DCubeTexture8): HResult;
begin
  Result := IDirect3DDevice8(aDirect3DDevice8).CreateCubeTexture(
    EdgeLength, Levels, Usage, Format, Pool,
    ppCubeTexture);
end;

function IDirect3DDevice8_CreateVolumeTexture(const aDirect3DDevice8: XTL_PIDirect3DDevice8;
  Width, Height, Depth, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppVolumeTexture: PIDirect3DVolumeTexture8): HResult;
begin
  Result := IDirect3DDevice8(aDirect3DDevice8).CreateVolumeTexture(
    Width, Height, Depth, Levels, Usage, Format, Pool,
    ppVolumeTexture);
end;

function D3DMATRIX_MULTIPLY(const a, b: D3DMATRIX): D3DMATRIX;
begin
  asm int 13; end; // TODO -oDXBX: Implement
end;

end.

