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
{$IFDEF DXBX_USE_D3D9}
  Direct3D9,
  D3DX9,
{$ELSE}
  Direct3D8, // IDirect3DDevice8
  D3DX8, // ID3DXBuffer
{$ENDIF}
  // Dxbx
  uTypes,
  uLog,
  uEmuD3D8Types,
  uDxbxKrnlUtils;

type
  PLPD3DXBUFFER = PID3DXBuffer; // = ^LPD3DXBUFFER;

  LPCVOID = Pointer; // Pointer to a constant of any type
  PPD3DCOLOR = ^PD3DCOLOR;

function iif(AValue: Boolean; const ATrue: TD3DDevType; const AFalse: TD3DDevType): TD3DDevType; overload;
function iif(AValue: Boolean; const ATrue: XTL_PIDirect3DSurface8; const AFalse: XTL_PIDirect3DSurface8): XTL_PIDirect3DSurface8; overload;
function iif(AValue: Boolean; const ATrue: XTL_PIDirect3DBaseTexture8; const AFalse: XTL_PIDirect3DBaseTexture8): XTL_PIDirect3DBaseTexture8; overload;

function IDirect3DDevice_GetRenderTarget(const aDirect3DDevice: IDirect3DDevice;
  ppRenderTarget: PIDirect3DSurface): HResult;
function IDirect3DDevice_GetDepthStencilSurface(const aDirect3DDevice: IDirect3DDevice;
  ppZStencilSurface: PIDirect3DSurface): HResult;
function IDirect3DDevice_CreateImageSurface(const aDirect3DDevice: IDirect3DDevice;
  Width, Height: LongWord; Format: TD3DFormat;
  ppSurface: PIDirect3DSurface): HResult;
function IDirect3DDevice_CreateIndexBuffer(const aDirect3DDevice: IDirect3DDevice;
  Length: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppIndexBuffer: PIDirect3DIndexBuffer): HResult;
function IDirect3DDevice_CreateTexture(const aDirect3DDevice: IDirect3DDevice;
  Width, Height, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppTexture: PIDirect3DTexture): HResult;
function IDirect3DDevice_CreateCubeTexture(const aDirect3DDevice: IDirect3DDevice;
  EdgeLength, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppCubeTexture: PIDirect3DCubeTexture): HResult;
function IDirect3DDevice_CreateVolumeTexture(const aDirect3DDevice: IDirect3DDevice;
  Width, Height, Depth, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppVolumeTexture: PIDirect3DVolumeTexture): HResult;
function IDirect3DDevice_GetTextureStageState(const aDirect3DDevice: IDirect3DDevice;
  Sampler: DWORD; _Type: X_D3DTEXTURESTAGESTATETYPE; out Value: DWORD): HResult;
function IDirect3DDevice_SetTextureStageState(const aDirect3DDevice: IDirect3DDevice;
  Sampler: DWORD; _Type: X_D3DTEXTURESTAGESTATETYPE; Value: DWORD): HResult;

function EmuXB2PC_D3DTSS(Value: X_D3DTEXTURESTAGESTATETYPE): TD3DSamplerStateType;

function D3DMATRIX_MULTIPLY(const a, b: D3DMATRIX): D3DMATRIX;

function F2DW(const aValue: Float): DWORD; inline;

function GetSurfaceSize(const aSurface: PD3DSurfaceDesc): LongWord;
function GetVolumeSize(const aVolume: PD3DVolumeDesc): LongWord;
function X_D3DPRIMITIVETYPE2String(const aValue: X_D3DPRIMITIVETYPE): string;

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

function IDirect3DDevice_GetRenderTarget(const aDirect3DDevice: IDirect3DDevice;
  ppRenderTarget: PIDirect3DSurface): HResult;
begin
{$IFDEF DXBX_USE_D3D9}
  Result := aDirect3DDevice.GetRenderTarget(
    {RenderTargetIndex=}0,
    ppRenderTarget);
{$ELSE}
  Result := aDirect3DDevice.GetRenderTarget(
    ppRenderTarget);
{$ENDIF}
end;

function IDirect3DDevice_GetDepthStencilSurface(const aDirect3DDevice: IDirect3DDevice;
  ppZStencilSurface: PIDirect3DSurface): HResult;
begin
  Result := aDirect3DDevice.GetDepthStencilSurface(ppZStencilSurface);
end;

function IDirect3DDevice_CreateImageSurface(const aDirect3DDevice: IDirect3DDevice;
  Width, Height: LongWord; Format: TD3DFormat;
  ppSurface: PIDirect3DSurface): HResult;
begin
{$IFDEF DXBX_USE_D3D9}
  Result := aDirect3DDevice.CreateOffscreenPlainSurface(
    Width, Height, Format, D3DPOOL_SCRATCH, // D3DPOOL_SYSTEMMEM ? See http://us.generation-nt.com/slowdowns-dx9-software-vertex-processing-help-24501522.html
    ppSurface, {Handle=}NULL);
{$ELSE}
  Result := aDirect3DDevice.CreateImageSurface(
    Width, Height, Format,
    ppSurface);
{$ENDIF}
end;

function IDirect3DDevice_CreateIndexBuffer(const aDirect3DDevice: IDirect3DDevice;
  Length: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppIndexBuffer: PIDirect3DIndexBuffer): HResult;
begin
{$IFDEF DXBX_USE_D3D9}
  Result := aDirect3DDevice.CreateIndexBuffer(
    Length, Usage, Format, Pool,
    ppIndexBuffer, {Handle=}NULL);
{$ELSE}
  Result := aDirect3DDevice.CreateIndexBuffer(
    Length, Usage, Format, Pool,
    ppIndexBuffer);
{$ENDIF}

  if (FAILED(Result)) then
    DxbxKrnlCleanup('CreateIndexBuffer Failed! (0x%.08X) (Requested %d bytes)', [Result, Length]);
end;

function IDirect3DDevice_CreateTexture(const aDirect3DDevice: IDirect3DDevice;
  Width, Height, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppTexture: PIDirect3DTexture): HResult;
begin
{$IFDEF DXBX_USE_D3D9}
  Result := aDirect3DDevice.CreateTexture(
    Width, Height, Levels, Usage, Format, Pool,
    ppTexture, {Handle=}NULL);
{$ELSE}
  Result := aDirect3DDevice.CreateTexture(
    Width, Height, Levels, Usage, Format, Pool,
    ppTexture);
{$ENDIF}
end;

function IDirect3DDevice_CreateCubeTexture(const aDirect3DDevice: IDirect3DDevice;
  EdgeLength, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppCubeTexture: PIDirect3DCubeTexture): HResult;
begin
{$IFDEF DXBX_USE_D3D9}
  Result := aDirect3DDevice.CreateCubeTexture(
    EdgeLength, Levels, Usage, Format, Pool,
    ppCubeTexture, {Handle=}NULL);
{$ELSE}
  Result := aDirect3DDevice.CreateCubeTexture(
    EdgeLength, Levels, Usage, Format, Pool,
    ppCubeTexture);
{$ENDIF}
end;

function IDirect3DDevice_CreateVolumeTexture(const aDirect3DDevice: IDirect3DDevice;
  Width, Height, Depth, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool;
  ppVolumeTexture: PIDirect3DVolumeTexture): HResult;
begin
{$IFDEF DXBX_USE_D3D9}
  Result := aDirect3DDevice.CreateVolumeTexture(
    Width, Height, Depth, Levels, Usage, Format, Pool,
    ppVolumeTexture, {Handle=}NULL);
{$ELSE}
  Result := aDirect3DDevice.CreateVolumeTexture(
    Width, Height, Depth, Levels, Usage, Format, Pool,
    ppVolumeTexture);
{$ENDIF}
end;

// Note! Because of the split between texture-stage-state and sampler-state, this method
// must have the original (Xbox) value to work with (otherwise, we would have overlapping
// values in Direct3D9!). As a consequence, the conversion to native takes place in here.
function IDirect3DDevice_GetTextureStageState(const aDirect3DDevice: IDirect3DDevice;
  Sampler: DWORD; _Type: X_D3DTEXTURESTAGESTATETYPE; out Value: DWORD): HResult;
var
  PCState: TD3DSamplerStateType;
begin
  PCState := EmuXB2PC_D3DTSS(_Type);
  if Ord(PCState) < 0 then
  begin
    Result := 0;
    Exit;
  end;

{$IFDEF DXBX_USE_D3D9}
  // For Direct3D9, everything below X_D3DSAMP_MAXANISOTROPY needs to call SetSamplerState :
  if (_Type <= X_D3DTSS_MAXANISOTROPY) then
    Result := aDirect3DDevice.GetSamplerState(Sampler, PCState, {out}Value)
  else
    // TODO : Aren't we missing out on texture-states?
{$ENDIF}
    Result := aDirect3DDevice.GetTextureStageState(Sampler, TD3DTextureStageStateType(PCState), {out}Value);
end;

function IDirect3DDevice_SetTextureStageState(const aDirect3DDevice: IDirect3DDevice;
  Sampler: DWORD; _Type: X_D3DTEXTURESTAGESTATETYPE; Value: DWORD): HResult;
var
  PCState: TD3DSamplerStateType;
begin
  PCState := EmuXB2PC_D3DTSS(_Type);
  if Ord(PCState) < 0 then
  begin
    Result := 0;
    Exit;
  end;

{$IFDEF DXBX_USE_D3D9}
  // For Direct3D9, everything below D3DSAMP_MAXANISOTROPY needs to call SetSamplerState :
  if _Type <= X_D3DTSS_MAXANISOTROPY then
    Result := aDirect3DDevice.SetSamplerState(Sampler, PCState, Value)
  else
{$ENDIF}
    Result := aDirect3DDevice.SetTextureStageState(Sampler, TD3DTextureStageStateType(PCState), Value);
end;

// convert from xbox to pc texture stage state
function EmuXB2PC_D3DTSS(Value: X_D3DTEXTURESTAGESTATETYPE): TD3DSamplerStateType;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  case (Value) of
    X_D3DTSS_ADDRESSU                   : Result := D3DSAMP_ADDRESSU;
    X_D3DTSS_ADDRESSV                   : Result := D3DSAMP_ADDRESSV;
    X_D3DTSS_ADDRESSW                   : Result := D3DSAMP_ADDRESSW;
    X_D3DTSS_MAGFILTER                  : Result := D3DSAMP_MAGFILTER;
    X_D3DTSS_MINFILTER                  : Result := D3DSAMP_MINFILTER;
    X_D3DTSS_MIPFILTER                  : Result := D3DSAMP_MIPFILTER;
    X_D3DTSS_MIPMAPLODBIAS              : Result := D3DSAMP_MIPMAPLODBIAS;
    X_D3DTSS_MAXMIPLEVEL                : Result := D3DSAMP_MAXMIPLEVEL;
    X_D3DTSS_MAXANISOTROPY              : Result := D3DSAMP_MAXANISOTROPY;
    X_D3DTSS_COLOROP                    : Result := TD3DSamplerStateType(D3DTSS_COLOROP);
    X_D3DTSS_COLORARG0                  : Result := TD3DSamplerStateType(D3DTSS_COLORARG0);
    X_D3DTSS_COLORARG1                  : Result := TD3DSamplerStateType(D3DTSS_COLORARG1);
    X_D3DTSS_COLORARG2                  : Result := TD3DSamplerStateType(D3DTSS_COLORARG2);
    X_D3DTSS_ALPHAOP                    : Result := TD3DSamplerStateType(D3DTSS_ALPHAOP);
    X_D3DTSS_ALPHAARG0                  : Result := TD3DSamplerStateType(D3DTSS_ALPHAARG0);
    X_D3DTSS_ALPHAARG1                  : Result := TD3DSamplerStateType(D3DTSS_ALPHAARG1);
    X_D3DTSS_ALPHAARG2                  : Result := TD3DSamplerStateType(D3DTSS_ALPHAARG2);
    X_D3DTSS_RESULTARG                  : Result := TD3DSamplerStateType(D3DTSS_RESULTARG);
    X_D3DTSS_TEXTURETRANSFORMFLAGS      : Result := TD3DSamplerStateType(D3DTSS_TEXTURETRANSFORMFLAGS);
    X_D3DTSS_BUMPENVMAT00               : Result := TD3DSamplerStateType(D3DTSS_BUMPENVMAT00);
    X_D3DTSS_BUMPENVMAT01               : Result := TD3DSamplerStateType(D3DTSS_BUMPENVMAT01);
    X_D3DTSS_BUMPENVMAT10               : Result := TD3DSamplerStateType(D3DTSS_BUMPENVMAT10);
    X_D3DTSS_BUMPENVMAT11               : Result := TD3DSamplerStateType(D3DTSS_BUMPENVMAT11);
    X_D3DTSS_BUMPENVLSCALE              : Result := TD3DSamplerStateType(D3DTSS_BUMPENVLSCALE);
    X_D3DTSS_BUMPENVLOFFSET             : Result := TD3DSamplerStateType(D3DTSS_BUMPENVLOFFSET);
    X_D3DTSS_TEXCOORDINDEX              : Result := TD3DSamplerStateType(D3DTSS_TEXCOORDINDEX);
    X_D3DTSS_BORDERCOLOR                : Result := D3DSAMP_BORDERCOLOR;
  else
    Result := TD3DSamplerStateType(-1); // Unsupported
  end;
end;

function D3DMATRIX_MULTIPLY(const a, b: D3DMATRIX): D3DMATRIX;
begin
  // TODO -oDxbx : Test if this is a correct implementation :
  // Unrolled version source : http://www.tantalon.com/pete/cppopt/final.htm#TemplateMetaprogramming
  Result.m[0,0] := a.m[0,0]*b.m[0,0] + a.m[1,0]*b.m[0,1] + a.m[2,0]*b.m[0,2] + a.m[3,0]*b.m[0,3];
  Result.m[0,1] := a.m[0,1]*b.m[0,0] + a.m[1,1]*b.m[0,1] + a.m[2,1]*b.m[0,2] + a.m[3,1]*b.m[0,3];
  Result.m[0,2] := a.m[0,2]*b.m[0,0] + a.m[1,2]*b.m[0,1] + a.m[2,2]*b.m[0,2] + a.m[3,2]*b.m[0,3];
  Result.m[0,3] := a.m[0,3]*b.m[0,0] + a.m[1,3]*b.m[0,1] + a.m[2,3]*b.m[0,2] + a.m[3,3]*b.m[0,3];

  Result.m[1,0] := a.m[0,0]*b.m[1,0] + a.m[1,0]*b.m[1,1] + a.m[2,0]*b.m[1,2] + a.m[3,0]*b.m[1,3];
  Result.m[1,1] := a.m[0,1]*b.m[1,0] + a.m[1,1]*b.m[1,1] + a.m[2,1]*b.m[1,2] + a.m[3,1]*b.m[1,3];
  Result.m[1,2] := a.m[0,2]*b.m[1,0] + a.m[1,2]*b.m[1,1] + a.m[2,2]*b.m[1,2] + a.m[3,2]*b.m[1,3];
  Result.m[1,3] := a.m[0,3]*b.m[1,0] + a.m[1,3]*b.m[1,1] + a.m[2,3]*b.m[1,2] + a.m[3,3]*b.m[1,3];

  Result.m[2,0] := a.m[0,0]*b.m[2,0] + a.m[1,0]*b.m[2,1] + a.m[2,0]*b.m[2,2] + a.m[3,0]*b.m[2,3];
  Result.m[2,1] := a.m[0,1]*b.m[2,0] + a.m[1,1]*b.m[2,1] + a.m[2,1]*b.m[2,2] + a.m[3,1]*b.m[2,3];
  Result.m[2,2] := a.m[0,2]*b.m[2,0] + a.m[1,2]*b.m[2,1] + a.m[2,2]*b.m[2,2] + a.m[3,2]*b.m[2,3];
  Result.m[2,3] := a.m[0,3]*b.m[2,0] + a.m[1,3]*b.m[2,1] + a.m[2,3]*b.m[2,2] + a.m[3,3]*b.m[2,3];

  Result.m[3,0] := a.m[0,0]*b.m[3,0] + a.m[1,0]*b.m[3,1] + a.m[2,0]*b.m[3,2] + a.m[3,0]*b.m[3,3];
  Result.m[3,1] := a.m[0,1]*b.m[3,0] + a.m[1,1]*b.m[3,1] + a.m[2,1]*b.m[3,2] + a.m[3,1]*b.m[3,3];
  Result.m[3,2] := a.m[0,2]*b.m[3,0] + a.m[1,2]*b.m[3,1] + a.m[2,2]*b.m[3,2] + a.m[3,2]*b.m[3,3];
  Result.m[3,3] := a.m[0,3]*b.m[3,0] + a.m[1,3]*b.m[3,1] + a.m[2,3]*b.m[3,2] + a.m[3,3]*b.m[3,3];
end;

function F2DW(const aValue: Float): DWORD;
begin
  Result := PDWORD(@aValue)^;
end;

// Returns size of the surface, in bytes.
function GetSurfaceSize(const aSurface: PD3DSurfaceDesc): LongWord;
begin
{$IFDEF DXBX_USE_D3D9}
  Result := aSurface.Height * aSurface.Width; // Calculate size in pixels
  Result := Result * 1; // TODO : Determine BytesPerPixel based on format!
  // if aSurface is CubeTexture then
  //   Result := Result * 6; // Faces
{$ELSE}
  Result := aSurface.Size;
{$ENDIF}
end;

// Returns size of the volume, in bytes.
function GetVolumeSize(const aVolume: PD3DVolumeDesc): LongWord;
begin
{$IFDEF DXBX_USE_D3D9}
  Result := aVolume.Height * aVolume.Width; // Calculate size in pixels
  Result := Result * aVolume.Depth; // TODO : Determine BytesPerPixel based on format?
{$ELSE}
  Result := aVolume.Size;
{$ENDIF}
end;

function X_D3DPRIMITIVETYPE2String(const aValue: X_D3DPRIMITIVETYPE): string;
begin
  case aValue of
    X_D3DPT_NONE: Result := 'X_D3DPT_NONE';
    X_D3DPT_POINTLIST: Result := 'X_D3DPT_POINTLIST';
    X_D3DPT_LINELIST: Result := 'X_D3DPT_LINELIST';
    X_D3DPT_LINELOOP: Result := 'X_D3DPT_LINELOOP';
    X_D3DPT_LINESTRIP: Result := 'X_D3DPT_LINESTRIP';
    X_D3DPT_TRIANGLELIST: Result := 'X_D3DPT_TRIANGLELIST';
    X_D3DPT_TRIANGLESTRIP: Result := 'X_D3DPT_TRIANGLESTRIP';
    X_D3DPT_TRIANGLEFAN: Result := 'X_D3DPT_TRIANGLEFAN';
    X_D3DPT_QUADLIST: Result := 'X_D3DPT_QUADLIST';
    X_D3DPT_QUADSTRIP: Result := 'X_D3DPT_QUADSTRIP';
    X_D3DPT_POLYGON: Result := 'X_D3DPT_POLYGON';
    X_D3DPT_MAX: Result := 'X_D3DPT_MAX';
    X_D3DPT_INVALID: Result := 'X_D3DPT_INVALID';
  else Result := '';
  end;
end;

end.

