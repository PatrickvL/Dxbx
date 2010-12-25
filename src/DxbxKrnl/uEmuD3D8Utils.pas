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
  SysUtils, // Format
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
function IDirect3DDevice_CreateDepthStencilSurface(const aDirect3DDevice: IDirect3DDevice;
  Width, Height: LongWord; Format: TD3DFormat; MultiSample: TD3DMultiSampleType;
  ppSurface: PIDirect3DSurface): HResult;
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
  Sampler: DWORD; _Type: X_D3DTEXTURESTAGESTATETYPE; PCValue: DWORD): HResult;

function EmuXB2PC_D3DTSS(Value: X_D3DTEXTURESTAGESTATETYPE): TD3DSamplerStateType;

function D3DMATRIX_MULTIPLY(const a, b: D3DMATRIX): D3DMATRIX;

function F2DW(const aValue: Float): DWORD; inline;

function GetSurfaceSize(const aSurface: PD3DSurfaceDesc): LongWord;
function GetVolumeSize(const aVolume: PD3DVolumeDesc): LongWord;
function X_D3DPRIMITIVETYPE2String(const aValue: X_D3DPRIMITIVETYPE): string;
function X_D3DFORMAT2String(const aValue: X_D3DFORMAT): string;
function D3DFORMAT2String(const aValue: D3DFORMAT): string;
function CommonToStr(const Common: DWORD): string;
function ResourceToString(const aValue: PX_D3DResource): string;

const
  // X_D3D_FORMAT flags :
  FMFL_SWIZZLED   = $01;
  FMFL_LINEAR     = $02;
  FMFL_COMPRESSED = $04;
  FMFL_YUV        = $08;
  FMFL_PALETIZED  = $10;
  FMFL_APROX      = $20;
  FMFL_HASALPHA   = $40;

  D3DFMT_INFO: array [X_D3DFMT_L8..X_D3DFMT_INDEX16] of record Name: string; PC: D3DFORMAT; BPP, Flags: DWORD; end = (
    ({0x00}Name:'X_D3DFMT_L8';           PC:D3DFMT_L8;           BPP:1; Flags:FMFL_SWIZZLED),
    ({0x01}Name:'X_D3DFMT_AL8';          PC:D3DFMT_L8;           BPP:1; Flags:FMFL_SWIZZLED+FMFL_HASALPHA+FMFL_APROX), // Cxbx NOTE: Hack: Alpha ignored, basically
    ({0x02}Name:'X_D3DFMT_A1R5G5B5';     PC:D3DFMT_A1R5G5B5;     BPP:2; Flags:FMFL_SWIZZLED+FMFL_HASALPHA),
    ({0x03}Name:'X_D3DFMT_X1R5G5B5';     PC:D3DFMT_X1R5G5B5;     BPP:2; Flags:FMFL_SWIZZLED),
    ({0x04}Name:'X_D3DFMT_A4R4G4B4';     PC:D3DFMT_A4R4G4B4;     BPP:2; Flags:FMFL_SWIZZLED+FMFL_HASALPHA),
    ({0x05}Name:'X_D3DFMT_R5G6B5';       PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_SWIZZLED),
    ({0x06}Name:'X_D3DFMT_A8R8G8B8';     PC:D3DFMT_A8R8G8B8;     BPP:4; Flags:FMFL_SWIZZLED+FMFL_HASALPHA),
    ({0x07}Name:'X_D3DFMT_X8R8G8B8';     PC:D3DFMT_X8R8G8B8;     BPP:4; Flags:FMFL_SWIZZLED),
    ({0x08}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x09}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x0A}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x0B}Name:'X_D3DFMT_P8';           PC:D3DFMT_P8;           BPP:1; Flags:FMFL_SWIZZLED+FMFL_PALETIZED),
    ({0x0C}Name:'X_D3DFMT_DXT1';         PC:D3DFMT_DXT1;         BPP:1; Flags:FMFL_COMPRESSED),
    ({0x0D}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x0F}Name:'X_D3DFMT_DXT2';         PC:D3DFMT_DXT2;         BPP:1; Flags:FMFL_COMPRESSED),
    ({0x0E}Name:'X_D3DFMT_DXT4';         PC:D3DFMT_DXT4;         BPP:1; Flags:FMFL_COMPRESSED),
    ({0x10}Name:'X_D3DFMT_LIN_A1R5G5B5'; PC:D3DFMT_A1R5G5B5;     BPP:2; Flags:FMFL_LINEAR+FMFL_HASALPHA),
    ({0x11}Name:'X_D3DFMT_LIN_R5G6B5';   PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_LINEAR),
    ({0x12}Name:'X_D3DFMT_LIN_A8R8G8B8'; PC:D3DFMT_A8R8G8B8;     BPP:4; Flags:FMFL_LINEAR+FMFL_HASALPHA),
    ({0x13}Name:'X_D3DFMT_LIN_L8';       PC:D3DFMT_L8;           BPP:1; Flags:FMFL_LINEAR),
    ({0x14}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x15}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x16}Name:'X_D3DFMT_LIN_R8B8';     PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_LINEAR+FMFL_APROX), // Cxbx NOTE: HACK: Totally and utterly wrong :)
    ({0x17}Name:'X_D3DFMT_LIN_G8B8';     PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_LINEAR+FMFL_APROX), // Cxbx NOTE: HACK: Totally and utterly wrong :)
    ({0x18}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x19}Name:'X_D3DFMT_A8';           PC:D3DFMT_A8;           BPP:1; Flags:FMFL_SWIZZLED+FMFL_HASALPHA),
{$IFDEF DXBX_USE_D3D9} // Note : Also available on Direct3D8, but only use it on 9, as that maps formats differently anyways :
    ({0x1A}Name:'X_D3DFMT_A8L8';         PC:D3DFMT_A8L8;         BPP:2; Flags:FMFL_SWIZZLED+FMFL_HASALPHA),
{$ELSE}
    ({0x1A}Name:'X_D3DFMT_A8L8';         PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_SWIZZLED+FMFL_HASALPHA+FMFL_APROX), // Cxbx NOTE: Hack: Alpha ignored, basically
{$ENDIF}
    ({0x1B}Name:'X_D3DFMT_LIN_AL8';      PC:D3DFMT_A8;           BPP:1; Flags:FMFL_LINEAR+FMFL_HASALPHA+FMFL_APROX), // Cxbx NOTE: Hack: Alpha ignored, basically
    ({0x1C}Name:'X_D3DFMT_LIN_X1R5G5B5'; PC:D3DFMT_X1R5G5B5;     BPP:2; Flags:FMFL_LINEAR),
    ({0x1D}Name:'X_D3DFMT_LIN_A4R4G4B4'; PC:D3DFMT_A4R4G4B4;     BPP:2; Flags:FMFL_LINEAR+FMFL_HASALPHA),
    ({0x1E}Name:'X_D3DFMT_LIN_X8R8G8B8'; PC:D3DFMT_X8R8G8B8;     BPP:4; Flags:FMFL_LINEAR),
    ({0x1F}Name:'X_D3DFMT_LIN_A8';       PC:D3DFMT_A8;           BPP:1; Flags:FMFL_LINEAR+FMFL_HASALPHA),
    ({0x20}Name:'X_D3DFMT_LIN_A8L8';     PC:D3DFMT_A8L8;         BPP:2; Flags:FMFL_LINEAR+FMFL_HASALPHA),
    ({0x21}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x22}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x23}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x24}Name:'X_D3DFMT_YUY2';         PC:D3DFMT_YUY2;         BPP:2; Flags:FMFL_LINEAR+FMFL_YUV),
    ({0x25}Name:'X_D3DFMT_UYVY';         PC:D3DFMT_UYVY;         BPP:2; Flags:FMFL_LINEAR+FMFL_YUV),
    ({0x26}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x27}Name:'X_D3DFMT_R6G5B5';       PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_SWIZZLED+FMFL_APROX), // Dxbx Note: R 1 bit less, G 1 bit more
    ({0x28}Name:'X_D3DFMT_G8B8';         PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_SWIZZLED+FMFL_APROX), // Cxbx NOTE: HACK: Totally and utterly wrong :)
    ({0x29}Name:'X_D3DFMT_R8B8';         PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_SWIZZLED+FMFL_APROX), // Cxbx NOTE: HACK: Totally and utterly wrong :)
    ({0x2A}Name:'X_D3DFMT_D24S8';        PC:D3DFMT_D24S8;        BPP:4; Flags:FMFL_SWIZZLED),
{$IFDEF DXBX_USE_D3D9}
    ({0x2B}Name:'X_D3DFMT_F24S8';        PC:D3DFMT_D24FS8;       BPP:4; Flags:FMFL_SWIZZLED),
    ({0x2C}Name:'X_D3DFMT_D16';          PC:D3DFMT_D16;          BPP:2; Flags:FMFL_SWIZZLED), // NOTE: D3DFMT_D16 on Xbox is always lockable
    ({0x2D}Name:'X_D3DFMT_F16';          PC:D3DFMT_R16F;         BPP:2; Flags:FMFL_SWIZZLED+FMFL_APROX), // Dxbx : Better approximation than D16
    ({0x2E}Name:'X_D3DFMT_LIN_D24S8';    PC:D3DFMT_D24S8;        BPP:4; Flags:FMFL_LINEAR),
    ({0x2F}Name:'X_D3DFMT_LIN_F24S8';    PC:D3DFMT_D24FS8;       BPP:4; Flags:FMFL_LINEAR),
    ({0x30}Name:'X_D3DFMT_LIN_D16';      PC:D3DFMT_D16;          BPP:2; Flags:FMFL_LINEAR), // NOTE: D3DFMT_D16 on Xbox is always lockable
    ({0x31}Name:'X_D3DFMT_LIN_F16';      PC:D3DFMT_R16F;         BPP:2; Flags:FMFL_LINEAR+FMFL_APROX), // Dxbx : Better approximation than D16
    ({0x32}Name:'X_D3DFMT_L16';          PC:D3DFMT_L16;          BPP:2; Flags:FMFL_SWIZZLED),
    ({0x33}Name:'X_D3DFMT_V16U16';       PC:D3DFMT_V16U16;       BPP:4; Flags:FMFL_SWIZZLED),
    ({0x34}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x35}Name:'X_D3DFMT_LIN_L16';      PC:D3DFMT_L16;          BPP:2; Flags:FMFL_LINEAR),
    ({0x36}Name:'X_D3DFMT_LIN_V16U16';   PC:D3DFMT_V16U16;       BPP:4; Flags:FMFL_LINEAR),
    ({0x37}Name:'X_D3DFMT_LIN_R6G5B5';   PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_LINEAR+FMFL_APROX), // Dxbx Note: R 1 bit less, G 1 bit more
    ({0x38}Name:'X_D3DFMT_R5G5B5A1';     PC:D3DFMT_A1R5G5B5;     BPP:2; Flags:FMFL_SWIZZLED+FMFL_HASALPHA+FMFL_APROX),
    ({0x39}Name:'X_D3DFMT_R4G4B4A4';     PC:D3DFMT_A4R4G4B4;     BPP:2; Flags:FMFL_SWIZZLED+FMFL_HASALPHA+FMFL_APROX),
    ({0x3A}Name:'X_D3DFMT_A8B8G8R8';     PC:D3DFMT_A8B8G8R8;     BPP:4; Flags:FMFL_SWIZZLED+FMFL_HASALPHA),
    ({0x3B}Name:'X_D3DFMT_B8G8R8A8';     PC:D3DFMT_A8R8G8B8;     BPP:4; Flags:FMFL_SWIZZLED+FMFL_HASALPHA+FMFL_APROX), // Cxbx NOTE: HACK: R<->B Swapped!
    ({0x3C}Name:'X_D3DFMT_R8G8B8A8';     PC:D3DFMT_A8R8G8B8;     BPP:4; Flags:FMFL_SWIZZLED+FMFL_HASALPHA+FMFL_APROX),
    ({0x3D}Name:'X_D3DFMT_LIN_R5G5B5A1'; PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_LINEAR+FMFL_HASALPHA+FMFL_APROX),
    ({0x3E}Name:'X_D3DFMT_LIN_R4G4B4A4'; PC:D3DFMT_A4R4G4B4;     BPP:2; Flags:FMFL_LINEAR+FMFL_HASALPHA+FMFL_APROX),
    ({0x3F}Name:'X_D3DFMT_LIN_A8B8G8R8'; PC:D3DFMT_A8B8G8R8;     BPP:4; Flags:FMFL_LINEAR+FMFL_HASALPHA),
{$ELSE} // Direct3D8 :
    ({0x2B}Name:'X_D3DFMT_F24S8';        PC:D3DFMT_D24S8;        BPP:4; Flags:FMFL_SWIZZLED+FMFL_APROX), // NOTE: Hack!! PC does not have D3DFMT_F24S8 (Float vs Int)
    ({0x2C}Name:'X_D3DFMT_D16';          PC:D3DFMT_D16;          BPP:2; Flags:FMFL_SWIZZLED), // NOTE: D3DFMT_D16 on Xbox is always lockable
    ({0x2D}Name:'X_D3DFMT_F16';          PC:D3DFMT_D16;          BPP:2; Flags:FMFL_SWIZZLED+FMFL_APROX), // NOTE: Hack!! PC does not have D3DFMT_F16 (Float vs Int)
    ({0x2E}Name:'X_D3DFMT_LIN_D24S8';    PC:D3DFMT_D24S8;        BPP:4; Flags:FMFL_LINEAR),
    ({0x2F}Name:'X_D3DFMT_LIN_F24S8';    PC:D3DFMT_D24S8;        BPP:4; Flags:FMFL_LINEAR+FMFL_APROX), // NOTE: Hack!! PC does not have D3DFMT_F24S8 (Float vs Int)
    ({0x30}Name:'X_D3DFMT_LIN_D16';      PC:D3DFMT_D16;          BPP:2; Flags:FMFL_LINEAR), // NOTE: D3DFMT_D16 on Xbox is always lockable
    ({0x31}Name:'X_D3DFMT_LIN_F16';      PC:D3DFMT_D16;          BPP:2; Flags:FMFL_LINEAR+FMFL_APROX), // NOTE: Hack!! PC does not have D3DFMT_F16 (Float vs Int)
    ({0x32}Name:'X_D3DFMT_L16';          PC:D3DFMT_L6V5U5;       BPP:2; Flags:FMFL_SWIZZLED+FMFL_APROX),
    ({0x33}Name:'X_D3DFMT_V16U16';       PC:D3DFMT_V16U16;       BPP:4; Flags:FMFL_SWIZZLED),
    ({0x34}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x35}Name:'X_D3DFMT_LIN_L16';      PC:D3DFMT_L6V5U5;       BPP:2; Flags:FMFL_LINEAR+FMFL_APROX),
    ({0x36}Name:'X_D3DFMT_LIN_V16U16';   PC:D3DFMT_V16U16;       BPP:4; Flags:FMFL_LINEAR),
    ({0x37}Name:'X_D3DFMT_LIN_R6G5B5';   PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_LINEAR+FMFL_APROX), // Dxbx Note: R 1 bit less, G 1 bit more
    ({0x38}Name:'X_D3DFMT_R5G5B5A1';     PC:D3DFMT_A1R5G5B5;     BPP:2; Flags:FMFL_SWIZZLED+FMFL_HASALPHA+FMFL_APROX),
    ({0x39}Name:'X_D3DFMT_R4G4B4A4';     PC:D3DFMT_A4R4G4B4;     BPP:2; Flags:FMFL_SWIZZLED+FMFL_HASALPHA+FMFL_APROX),
    ({0x3A}Name:'X_D3DFMT_A8B8G8R8';     PC:D3DFMT_A8R8G8B8;     BPP:4; Flags:FMFL_SWIZZLED+FMFL_HASALPHA+FMFL_APROX), // Cxbx NOTE: HACK: R<->B Swapped!
    ({0x3B}Name:'X_D3DFMT_B8G8R8A8';     PC:D3DFMT_A8R8G8B8;     BPP:4; Flags:FMFL_SWIZZLED+FMFL_HASALPHA+FMFL_APROX), // Cxbx NOTE: HACK: R<->B Swapped!
    ({0x3C}Name:'X_D3DFMT_R8G8B8A8';     PC:D3DFMT_A8R8G8B8;     BPP:4; Flags:FMFL_SWIZZLED+FMFL_HASALPHA+FMFL_APROX),
    ({0x3D}Name:'X_D3DFMT_LIN_R5G5B5A1'; PC:D3DFMT_R5G6B5;       BPP:2; Flags:FMFL_LINEAR+FMFL_HASALPHA+FMFL_APROX),
    ({0x3E}Name:'X_D3DFMT_LIN_R4G4B4A4'; PC:D3DFMT_A4R4G4B4;     BPP:2; Flags:FMFL_LINEAR+FMFL_HASALPHA+FMFL_APROX),
    ({0x3F}Name:'X_D3DFMT_LIN_A8B8G8R8'; PC:D3DFMT_A8R8G8B8;     BPP:4; Flags:FMFL_LINEAR+FMFL_HASALPHA+FMFL_APROX), // Cxbx NOTE: HACK: R<->B Swapped!
{$ENDIF}
    ({0x40}Name:'X_D3DFMT_LIN_B8G8R8A8'; PC:D3DFMT_A8R8G8B8;     BPP:4; Flags:FMFL_LINEAR+FMFL_HASALPHA+FMFL_APROX),
    ({0x41}Name:'X_D3DFMT_LIN_R8G8B8A8'; PC:D3DFMT_A8R8G8B8;     BPP:4; Flags:FMFL_LINEAR+FMFL_HASALPHA+FMFL_APROX),
    // Maybe cut off here and handle the last two separately?
    ({0x42}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x43}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x44}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x45}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x46}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x47}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x48}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x49}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x4A}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x4B}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x4C}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x4D}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x4E}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x4F}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x50}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x51}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x52}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x53}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x54}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x55}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x56}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x57}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x58}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x59}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x5A}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x5B}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x5C}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x5D}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x5E}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x5F}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x60}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x61}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x62}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x63}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0; Flags:0),
    ({0x64}Name:'X_D3DFMT_VERTEXDATA';   PC:D3DFMT_VERTEXDATA;   BPP:1; Flags:FMFL_LINEAR),
    ({0x65}Name:'X_D3DFMT_INDEX16';      PC:D3DFMT_INDEX16;      BPP:2; Flags:FMFL_LINEAR) // Dxbx addition : Pass-through internal format that shouldn't raise a warning :
  );

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

function IDirect3DDevice_CreateDepthStencilSurface(const aDirect3DDevice: IDirect3DDevice;
  Width, Height: LongWord; Format: TD3DFormat; MultiSample: TD3DMultiSampleType;
  ppSurface: PIDirect3DSurface): HResult;
begin
{$IFDEF DXBX_USE_D3D9}
  Result := aDirect3DDevice.CreateDepthStencilSurface(
    Width, Height, Format, MultiSample, {MultiSampleQuality=}0, {Discard=}False,
    ppSurface, {Handle=}NULL);
{$ELSE}
  Result := aDirect3DDevice.CreateDepthStencilSurface(
    Width, Height, Format, MultiSample,
    ppSurface);
{$ENDIF}
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
  // For Direct3D9, everything below X_D3DSAMP_MAXANISOTROPY needs to call GetSamplerState :
  if (_Type <= X_D3DTSS_MAXANISOTROPY) then
    Result := aDirect3DDevice.GetSamplerState(Sampler, PCState, {out}Value)
  else
    // TODO : Aren't we missing out on texture-states?
{$ENDIF}
    Result := aDirect3DDevice.GetTextureStageState(Sampler, TD3DTextureStageStateType(PCState), {out}Value);
end;

function IDirect3DDevice_SetTextureStageState(const aDirect3DDevice: IDirect3DDevice;
  Sampler: DWORD; _Type: X_D3DTEXTURESTAGESTATETYPE; PCValue: DWORD): HResult;
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
    Result := aDirect3DDevice.SetSamplerState(Sampler, PCState, PCValue)
  else
{$ENDIF}
    Result := aDirect3DDevice.SetTextureStageState(Sampler, TD3DTextureStageStateType(PCState), PCValue);
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

function X_D3DFORMAT2String(const aValue: X_D3DFORMAT): string;
begin
  if aValue in [X_D3DFMT_L8..X_D3DFMT_INDEX16] then
    Result := D3DFMT_INFO[aValue].Name
  else
    Result := '';
end;

function D3DFORMAT2String(const aValue: D3DFORMAT): string;
begin
  case aValue of
    D3DFMT_UNKNOWN      : Result := 'D3DFMT_UNKNOWN';
    D3DFMT_R8G8B8       : Result := 'D3DFMT_R8G8B8';
    D3DFMT_A8R8G8B8     : Result := 'D3DFMT_A8R8G8B8';
    D3DFMT_X8R8G8B8     : Result := 'D3DFMT_X8R8G8B8';
    D3DFMT_R5G6B5       : Result := 'D3DFMT_R5G6B5';
    D3DFMT_X1R5G5B5     : Result := 'D3DFMT_X1R5G5B5';
    D3DFMT_A1R5G5B5     : Result := 'D3DFMT_A1R5G5B5';
    D3DFMT_A4R4G4B4     : Result := 'D3DFMT_A4R4G4B4';
    D3DFMT_R3G3B2       : Result := 'D3DFMT_R3G3B2';
    D3DFMT_A8           : Result := 'D3DFMT_A8 ';
    D3DFMT_A8R3G3B2     : Result := 'D3DFMT_A8R3G3B2';
    D3DFMT_X4R4G4B4     : Result := 'D3DFMT_X4R4G4B4';
    D3DFMT_A2B10G10R10  : Result := 'D3DFMT_A2B10G10R10';
    D3DFMT_G16R16       : Result := 'D3DFMT_G16R16';
    D3DFMT_A8P8         : Result := 'D3DFMT_A8P8';
    D3DFMT_P8           : Result := 'D3DFMT_P8 ';
    D3DFMT_L8           : Result := 'D3DFMT_L8 ';
    D3DFMT_A8L8         : Result := 'D3DFMT_A8L8';
    D3DFMT_A4L4         : Result := 'D3DFMT_A4L4';
    D3DFMT_V8U8         : Result := 'D3DFMT_V8U8';
    D3DFMT_L6V5U5       : Result := 'D3DFMT_L6V5U5';
    D3DFMT_X8L8V8U8     : Result := 'D3DFMT_X8L8V8U8';
    D3DFMT_Q8W8V8U8     : Result := 'D3DFMT_Q8W8V8U8';
    D3DFMT_V16U16       : Result := 'D3DFMT_V16U16';
{$IFNDEF DXBX_USE_D3D9} // Direct3D8 only :
    D3DFMT_W11V11U10    : Result := 'D3DFMT_W11V11U10';
{$ENDIF}
    D3DFMT_A2W10V10U10  : Result := 'D3DFMT_A2W10V10U10';
    D3DFMT_UYVY         : Result := 'D3DFMT_UYVY';
    D3DFMT_YUY2         : Result := 'D3DFMT_YUY2';
    D3DFMT_DXT1         : Result := 'D3DFMT_DXT1';
    D3DFMT_DXT2         : Result := 'D3DFMT_DXT2';
    D3DFMT_DXT3         : Result := 'D3DFMT_DXT3';
    D3DFMT_DXT4         : Result := 'D3DFMT_DXT4';
    D3DFMT_DXT5         : Result := 'D3DFMT_DXT5';
    D3DFMT_D16_LOCKABLE : Result := 'D3DFMT_D16_LOCKABLE';
    D3DFMT_D32          : Result := 'D3DFMT_D32';
    D3DFMT_D15S1        : Result := 'D3DFMT_D15S1';
    D3DFMT_D24S8        : Result := 'D3DFMT_D24S8';
    D3DFMT_D16          : Result := 'D3DFMT_D16';
    D3DFMT_D24X8        : Result := 'D3DFMT_D24X8';
    D3DFMT_D24X4S4      : Result := 'D3DFMT_D24X4S4';
    D3DFMT_VERTEXDATA   : Result := 'D3DFMT_VERTEXDATA';
    D3DFMT_INDEX16      : Result := 'D3DFMT_INDEX16';
    D3DFMT_INDEX32      : Result := 'D3DFMT_INDEX32';
{$IFDEF DXBX_USE_D3D9} // Direct3D9 only :
    D3DFMT_A8B8G8R8     : Result := 'D3DFMT_A8B8G8R8';
    D3DFMT_X8B8G8R8     : Result := 'D3DFMT_X8B8G8R8';
    D3DFMT_A2R10G10B10  : Result := 'D3DFMT_A2R10G10B10';
    D3DFMT_A16B16G16R16 : Result := 'D3DFMT_A16B16G16R16';
    D3DFMT_A8X8V8U8     : Result := 'D3DFMT_A8X8V8U8';
    D3DFMT_L8X8V8U8     : Result := 'D3DFMT_L8X8V8U8';
    D3DFMT_RGBG         : Result := 'D3DFMT_RGBG';
    D3DFMT_GRGB         : Result := 'D3DFMT_GRGB';
    D3DFMT_D32F_LOCKABLE: Result := 'D3DFMT_D32F_LOCKABLE';
    D3DFMT_D24FS8       : Result := 'D3DFMT_D24FS8';
    D3DFMT_L16          : Result := 'D3DFMT_L16';
    D3DFMT_Q16W16V16U16 : Result := 'D3DFMT_Q16W16V16U16';
    D3DFMT_MULTI2_ARGB8 : Result := 'D3DFMT_MULTI2_ARGB8';
    D3DFMT_R16F         : Result := 'D3DFMT_R16F';
    D3DFMT_G16R16F      : Result := 'D3DFMT_G16R16F';
    D3DFMT_A16B16G16R16F: Result := 'D3DFMT_A16B16G16R16F';
    D3DFMT_R32F         : Result := 'D3DFMT_R32F';
    D3DFMT_G32R32F      : Result := 'D3DFMT_G32R32F';
    D3DFMT_A32B32G32R32F: Result := 'D3DFMT_A32B32G32R32F';
    D3DFMT_CxV8U8       : Result := 'D3DFMT_CxV8U8';
{$ENDIF}
  else Result := '';
  end;
end;

function CommonToStr(const Common: DWORD): string;
var
  Value: DWORD;
begin
  Result := '';
  Value := Common and X_D3DCOMMON_REFCOUNT_MASK;
  if Value > 1 then Result := Format(' RefCount=%d', [Value]);

  Value := (Common and X_D3DCOMMON_TYPE_MASK);
  case Value of
    X_D3DCOMMON_TYPE_VERTEXBUFFER: Result := Result + ' VERTEXBUFFER';
    X_D3DCOMMON_TYPE_INDEXBUFFER : Result := Result + ' INDEXBUFFER';
    X_D3DCOMMON_TYPE_PUSHBUFFER  : Result := Result + ' PUSHBUFFER';
    X_D3DCOMMON_TYPE_PALETTE     : Result := Result + ' PALETTE';
    X_D3DCOMMON_TYPE_TEXTURE     : Result := Result + ' TEXTURE';
    X_D3DCOMMON_TYPE_SURFACE     : Result := Result + ' SURFACE';
    X_D3DCOMMON_TYPE_FIXUP       : Result := Result + ' FIXUP';
  end;

  Value := (Common and X_D3DCOMMON_INTREFCOUNT_MASK) shr X_D3DCOMMON_INTREFCOUNT_SHIFT;
  if Value > 0 then Result := Result + Format(' IntRefCount=%d', [Value]);

  if (Common and X_D3DCOMMON_VIDEOMEMORY) > 0 then Result := Result + ' VIDEOMEMORY';
  if (Common and X_D3DCOMMON_D3DCREATED) > 0 then Result := Result + ' D3DCREATED';
  if (Common and X_D3DCOMMON_ISLOCKED) > 0 then Result := Result + ' ISLOCKED';
end;

function ResourceFormatToStr(const ResourceFormat: DWORD): string;
var
  Value: DWORD;
begin
  Result := '';

  if (ResourceFormat and X_D3DFORMAT_CUBEMAP) > 0 then Result := Result + ' CUBEMAP';
  if (ResourceFormat and X_D3DFORMAT_BORDERSOURCE_COLOR) > 0 then Result := Result + ' BORDER';

  Value := (ResourceFormat and X_D3DFORMAT_DIMENSION_MASK) shr X_D3DFORMAT_DIMENSION_SHIFT;
  if Value > 2 then Result := Result + Format(' %dD', [Value]);

  Value := (ResourceFormat and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT;
  Result := Result + Format(' FORMAT=%s', [X_D3DFORMAT2String(Value)]);

  Value := (ResourceFormat and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
  if Value > 1 then Result := Result + Format(' MIPMAP=%d', [Value]);

  Value := (ResourceFormat and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT;
  if Value > 0 then Result := Result + Format(' USIZE=%d', [Value]);

  Value := (ResourceFormat and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT;
  if Value > 0 then Result := Result + Format(' VSIZE=%d', [Value]);

  Value := (ResourceFormat and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT;
  if Value > 0 then Result := Result + Format(' PSIZE=%d', [Value]);
end;

function ResourceSizeToStr(const ResourceSize: DWORD): string;
var
  Value: DWORD;
begin
  Result := '';
  Value := (ResourceSize and X_D3DSIZE_WIDTH_MASK) {shr X_D3DSIZE_WIDTH_SHIFT};
  if Value > 0 then Result := Result + Format(' WIDTH=%d', [Value]);

  Value := (ResourceSize and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT;
  if Value > 0 then Result := Result + Format(' HEIGHT=%d', [Value]);

  Value := (ResourceSize and X_D3DSIZE_PITCH_MASK) shr X_D3DSIZE_PITCH_SHIFT;
  if Value > 0 then Result := Result + Format(' PITCH=%d', [Value]);
end;

function ResourceToString(const aValue: PX_D3DResource): string;
var
  Flag: DWORD;
begin
  Result := Format('0x%.08x', [UIntPtr(aValue)]);
  if Assigned(aValue) then
  begin
    Result := Result + Format(' (Common=%.08x Data=%.08x Emu=%.08x', [
              aValue.Common, aValue.Data, aValue.Emu.Lock]);
    if IsSpecialResource(aValue.Data) then
    begin
      Result := Result + ' :' + CommonToStr(aValue.Common);
      Result := Result + ' IsSpecial:';
      Flag := aValue.Data and (not X_D3DRESOURCE_DATA_FLAG_SPECIAL);
      if (Flag and X_D3DRESOURCE_DATA_FLAG_SURFACE) > 0 then Result := Result + ' Surface';
      if (Flag and X_D3DRESOURCE_DATA_FLAG_YUVSURF) > 0 then Result := Result + ' YUV memory surface';
      if (Flag and X_D3DRESOURCE_DATA_FLAG_D3DREND) > 0 then Result := Result + ' D3D Render Target';
      if (Flag and X_D3DRESOURCE_DATA_FLAG_D3DSTEN) > 0 then Result := Result + ' D3D Stencil Surface';
    end
    else
    begin
      if IsResourcePixelContainer(aValue) then
      begin
        Result := Result + Format(' Format=%.08x Size=%.08x :', [
                  PX_D3DPixelContainer(aValue).Format, PX_D3DPixelContainer(aValue).Size]);

        Result := Result + CommonToStr(aValue.Common);
        Result := Result + ResourceFormatToStr(PX_D3DPixelContainer(aValue).Format);
        Result := Result + ResourceSizeToStr(PX_D3DPixelContainer(aValue).Size);
      end
      else
        Result := Result + ' :' + CommonToStr(aValue.Common);
    end;

    Result := Result + ')';
  end;
end;

end.

