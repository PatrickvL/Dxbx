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
  // Jedi Win32API
  JwaWinType, // PFLOAT
  // DirectX
  Direct3D, // PD3DCOLOR
{$IFDEF DXBX_USE_D3D9}
  Direct3D9,
  D3DX9,
  DXErr9, // DXGetErrorString9
{$ELSE}
  Direct3D8, // IDirect3DDevice8
  D3DX8, // ID3DXBuffer
{$ENDIF}
  // Dxbx
  uConsts,
  uTypes,
  uLog,
  uEmu, // EmuWarning
  uEmuD3D8Types,
  uDxbxUtils,
  uDxbxKrnlUtils;

type
  PLPD3DXBUFFER = PID3DXBuffer; // = ^LPD3DXBUFFER;

  LPCVOID = Pointer; // Pointer to a constant of any type
  PPD3DCOLOR = ^PD3DCOLOR;

  // D3DFORMAT values are build from 1 letter abbreviations followed by the number of bits for it.
  TDxbxChannel = (
    A, // Alpha
    R, // Red
    G, // Green
    B, // Blue
    P, // Palette
    L, // Luminance
    Q,W,V,U, // Signed values (used in bump maps)
    X, // Ignored Alpha
    D, // Depth (integer)
    F, // Depth (floating-point)
    S  // Stencil
    );
  // Inputs  : ARGB (X instead of A means ignore that number of bits)
  // Maps to : QWVU

  TDxbxChannels = set of TDxbxChannel;

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

function D3DMATRIX_MULTIPLY(const a, b: D3DMATRIX): D3DMATRIX;

function F2DW(const aValue: Float): DWORD; inline;
function DW2F(const aValue: DWORD): FLOAT; inline;
function ClampIntToByte(const aValue: Integer): Byte;

function EmuXBFormatBPP(Format: X_D3DFORMAT): DWORD;
function EmuXBFormatIsSwizzled(Format: X_D3DFORMAT): BOOL_;
function EmuXBFormatIsYUV(Format: X_D3DFORMAT): BOOL_;
function EmuXBFormatIsCompressed(Format: X_D3DFORMAT): BOOL_;
function EmuXBFormatIsLinear(Format: X_D3DFORMAT): BOOL_;
function EmuXBFormatIsRenderTarget(Format: X_D3DFORMAT): BOOL_;
function EmuXBFormatIsDepthBuffer(Format: X_D3DFORMAT): BOOL_;
function EmuXBFormatHasChannel(Format: X_D3DFORMAT; Channel: TDxbxChannel): BOOL_;
function EmuXBFormatHasAlpha(Format: X_D3DFORMAT): BOOL_;

function DxbxEncodeDimensionsIntoSize(const Width, Height, Pitch: DWORD): DWORD;
procedure DxbxDecodeSizeIntoDimensions(const Size: DWORD; pWidth, pHeight, pPitch: PDWORD);
function DxbxEncodeResourceFormat(const bCubeMap, bBorder: Boolean;
  const dwDimens, dwFormat, dwMipMap, dwUSize, dwVSize, dwPSize: DWORD): DWORD;
procedure DxbxDecodeResourceFormat(const ResourceFormat: DWORD;
  const pCubeMap: PBoolean;
  const pBorder: PBoolean;
  const pDimens: PDWORD;
  const pFormat: PDWORD;
  const pMipMap: PDWORD;
  const pUSize: PDWORD;
  const pVSize: PDWORD;
  const pPSize: PDWORD);

function GetSurfaceSize(const aSurface: PD3DSurfaceDesc): LongWord;
function X_D3DFORMAT2String(const aValue: X_D3DFORMAT): string;
function D3DFORMAT2String(const aValue: D3DFORMAT): string;

function DxbxGetResourceType(const pResource: PX_D3DResource): X_D3DRESOURCETYPE;
function IsResourcePixelContainer(const aResource: PX_D3DResource): Boolean;

function CommonToStr(const Common: DWORD): string;
function ResourceFormatToStr(const ResourceFormat: DWORD): string;
function ResourceToString(const aValue: PX_D3DResource): string;

function DxbxD3DErrorString(hResult: HRESULT): string;
procedure DxbxD3DError(aFunction, aMessage: string; pResource: PX_D3DResource = nil; hRet: HRESULT = 0; bHalt: Boolean = True);

function GetD3DFormat(aD3DPixelContainer: PX_D3DPixelContainer): X_D3DFORMAT;

const
  X_MAX_MIPMAPS_VOLUME = 9; // 2^9 = 512, the maximum NV2A volume texture dimension size
  X_MAX_MIPMAPS = 12; // 2^12 = 4096, the maximum NV2A (2D) texture dimension size

type
  RDxbxDecodedPixelContainer = record
    pPixelContainer: PX_D3DPixelContainer;
    X_Format: X_D3DFORMAT;
    dwWidth: DWORD; // Number of pixels on 1 line (measured at mipmap level 0)
    dwHeight: DWORD; // Number of lines (measured at mipmap level 0)
    dwDepth: DWORD; // Volume texture number of slices
    dwBPP: DWORD; // Bits per pixel, 8, 16 or 32 (and 4 for DXT1)
    dwRowPitch: DWORD; // Bytes to skip to get to the next line (measured at mipmap level 0)
    dwFacePitch: DWORD; // Cube maps must step this number of bytes per face to skip (this includes all mipmaps for 1 face)
    dwMipMapLevels: DWORD; // 1-10
    dwMinXYValue: DWORD; // 4 for compressed formats, 1 for everything else. Only applies to width & height. Depth can go to 1.
    SlicePitches: array[0..X_MAX_MIPMAPS-1] of uint; // Size of a slice per mipmap level (also zero based)
    MipMapSlices: array[0..X_MAX_MIPMAPS-1] of uint; // Number of slices per mipmap level
    MipMapFormats: array[0..X_MAX_MIPMAPS-1] of uint; // Altered Format per mipmap level
    MipMapOffsets: array[0..X_MAX_MIPMAPS] of uint; // [0]=0, [1]=level 0 size (=level 1 offset=volume slice size), [2..10]=offsets of those levels
    bIsSwizzled: BOOL_;
    bIsCompressed: BOOL_;
    bIsCubeMap: BOOL_;
    bIsBorderSource: BOOL_;
    bIs3D: BOOL_;
  end;

procedure DxbxGetFormatRelatedVariables(
  const pPixelContainer: PX_D3DPixelContainer;
  out DecodedInfo: RDxbxDecodedPixelContainer); overload;
procedure DxbxGetFormatRelatedVariables(
  LocalFormat, LocalSize: DWORD;
  out DecodedInfo: RDxbxDecodedPixelContainer); overload;

const
  // X_D3D_FORMAT flags :
  FMFL_SWIZZLED     = $01;
  FMFL_LINEAR       = $02;
  FMFL_COMPRESSED   = $04;
  FMFL_YUV          = $08;
  FMFL_APROX        = $20;
  FMFL_RENDERTARGET = $40; // Note : DepthBuffer formats are indicated by presence of channel 'D'epth or 'F'loat (with or without 'S'tencil)

(* D3DFORMAT =

  D3DFMT_A16B16G16R16            9
  D3DFMT_A16B16G16R16F           9
  D3DFMT_A1R5G5B5             8
  D3DFMT_A2B10G10R10          8, 9
  D3DFMT_A2R10G10B10             9
  D3DFMT_A2W10V10U10          8, 9
  D3DFMT_A32B32G32R32F           9
  D3DFMT_A4L4                 8, 9
  D3DFMT_A4R4G4B4             8, 9
  D3DFMT_A8                   8, 9
  D3DFMT_A8B8G8R8                9
  D3DFMT_A8L8                 8, 9
  D3DFMT_A8P8                 8, 9
  D3DFMT_A8R3G3B2             8, 9
  D3DFMT_A8R8G8B8             8, 9

  D3DFMT_ATI2                    9

  D3DFMT_CxV8U8                  9

  D3DFMT_D15S1                8, 9
  D3DFMT_D16                  8, 9
  D3DFMT_D16_LOCKABLE         8, 9
  D3DFMT_D24S8                8, 9
  D3DFMT_D24S8                   9
  D3DFMT_D24X4S4              8, 9
  D3DFMT_D24X8                8, 9
  D3DFMT_D32                  8, 9
  D3DFMT_D32F_LOCKABLE           9

  D3DFMT_G16R16               8, 9
  D3DFMT_G16R16F                 9
  D3DFMT_G32R32F                 9
  D3DFMT_G8R8_G8B8               9

  D3DFMT_L16                     9
  D3DFMT_L6V5U5               8, 9
  D3DFMT_L8                   8, 9

  D3DFMT_MULTI2_ARGB8            9

  D3DFMT_P8                   8, 9

  D3DFMT_Q16W16V16U16            9
  D3DFMT_Q8W8V8U8             8, 9

  D3DFMT_R32F                    9
  D3DFMT_R3G3B2               8, 9
  D3DFMT_R5G6B5               8, 9
  D3DFMT_R8G8_B8G8               9
  D3DFMT_R8G8B8               8, 9

  D3DFMT_V16U16               8, 9
  D3DFMT_V8U8                 8, 9

  D3DFMT_W11V11U10            8

  D3DFMT_X1R5G5B5             8, 9
  D3DFMT_X4R4G4B4             8, 9
  D3DFMT_X8B8G8R8                9
  D3DFMT_X8L8V8U8             8, 9
  D3DFMT_X8R8G8B8             8, 9

  D3DFMT_DXT1                 8, 9
  D3DFMT_DXT2                 8, 9
  D3DFMT_DXT3                 8, 9
  D3DFMT_DXT4                 8, 9
  D3DFMT_DXT5                 8, 9

  D3DFMT_UYVY                 8, 9
  D3DFMT_YUY2                 8, 9
*)

  D3DFMT_INFO: array [X_D3DFMT_L8..X_D3DFMT_INDEX16] of record Name: string; PC: D3DFORMAT; BPP: int; C: TDxbxChannels; Flags: int; end = (
    ({0x00}Name:'X_D3DFMT_L8';           PC:D3DFMT_L8;           BPP:8;  C:[  L    ]; Flags:FMFL_SWIZZLED),
    ({0x01}Name:'X_D3DFMT_AL8';          PC:D3DFMT_L8;           BPP:8;  C:[A,L    ]; Flags:FMFL_SWIZZLED+FMFL_APROX), // Cxbx NOTE: Hack: Alpha ignored, basically
    ({0x02}Name:'X_D3DFMT_A1R5G5B5';     PC:D3DFMT_A1R5G5B5;     BPP:16; C:[A,R,G,B]; Flags:FMFL_SWIZZLED),
    ({0x03}Name:'X_D3DFMT_X1R5G5B5';     PC:D3DFMT_X1R5G5B5;     BPP:16; C:[X,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_RENDERTARGET),
    ({0x04}Name:'X_D3DFMT_A4R4G4B4';     PC:D3DFMT_A4R4G4B4;     BPP:16; C:[A,R,G,B]; Flags:FMFL_SWIZZLED),
    ({0x05}Name:'X_D3DFMT_R5G6B5';       PC:D3DFMT_R5G6B5;       BPP:16; C:[  R,G,B]; Flags:FMFL_SWIZZLED+FMFL_RENDERTARGET),
    ({0x06}Name:'X_D3DFMT_A8R8G8B8';     PC:D3DFMT_A8R8G8B8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_RENDERTARGET),
    ({0x07}Name:'X_D3DFMT_X8R8G8B8';     PC:D3DFMT_X8R8G8B8;     BPP:32; C:[X,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_RENDERTARGET),
    ({0x08}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x09}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x0A}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x0B}Name:'X_D3DFMT_P8';           PC:D3DFMT_P8;           BPP:8;  C:[  P    ]; Flags:FMFL_SWIZZLED),
    ({0x0C}Name:'X_D3DFMT_DXT1';         PC:D3DFMT_DXT1;         BPP:4;  C:[       ]; Flags:FMFL_COMPRESSED),
    ({0x0D}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x0F}Name:'X_D3DFMT_DXT2';         PC:D3DFMT_DXT2;         BPP:8;  C:[       ]; Flags:FMFL_COMPRESSED),
    ({0x0E}Name:'X_D3DFMT_DXT4';         PC:D3DFMT_DXT4;         BPP:8;  C:[       ]; Flags:FMFL_COMPRESSED),
    ({0x10}Name:'X_D3DFMT_LIN_A1R5G5B5'; PC:D3DFMT_A1R5G5B5;     BPP:16; C:[A,R,G,B]; Flags:FMFL_LINEAR),
    ({0x11}Name:'X_D3DFMT_LIN_R5G6B5';   PC:D3DFMT_R5G6B5;       BPP:16; C:[  R,G,B]; Flags:FMFL_LINEAR+FMFL_RENDERTARGET),
    ({0x12}Name:'X_D3DFMT_LIN_A8R8G8B8'; PC:D3DFMT_A8R8G8B8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_LINEAR+FMFL_RENDERTARGET),
    ({0x13}Name:'X_D3DFMT_LIN_L8';       PC:D3DFMT_L8;           BPP:8;  C:[  L    ]; Flags:FMFL_LINEAR+FMFL_RENDERTARGET),
    ({0x14}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x15}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x16}Name:'X_D3DFMT_LIN_R8B8';     PC:D3DFMT_V8U8;         BPP:16; C:[  R,  B]; Flags:FMFL_LINEAR+FMFL_APROX), // Cxbx NOTE: HACK: Totally and utterly wrong :)
    ({0x17}Name:'X_D3DFMT_LIN_G8B8';     PC:D3DFMT_V8U8;         BPP:16; C:[    G,B]; Flags:FMFL_LINEAR+FMFL_RENDERTARGET), // No FMFL_APROX as GB map to VU
    ({0x18}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x19}Name:'X_D3DFMT_A8';           PC:D3DFMT_A8;           BPP:8;  C:[A      ]; Flags:FMFL_SWIZZLED),
    ({0x1A}Name:'X_D3DFMT_A8L8';         PC:D3DFMT_A8L8;         BPP:16; C:[A,L    ]; Flags:FMFL_SWIZZLED),
    ({0x1B}Name:'X_D3DFMT_LIN_AL8';      PC:D3DFMT_L8;           BPP:8;  C:[A,L    ]; Flags:FMFL_LINEAR+FMFL_APROX), // Cxbx NOTE: Hack: Alpha ignored, basically
    ({0x1C}Name:'X_D3DFMT_LIN_X1R5G5B5'; PC:D3DFMT_X1R5G5B5;     BPP:16; C:[X,R,G,B]; Flags:FMFL_LINEAR+FMFL_RENDERTARGET),
    ({0x1D}Name:'X_D3DFMT_LIN_A4R4G4B4'; PC:D3DFMT_A4R4G4B4;     BPP:16; C:[A,R,G,B]; Flags:FMFL_LINEAR),
    ({0x1E}Name:'X_D3DFMT_LIN_X8R8G8B8'; PC:D3DFMT_X8R8G8B8;     BPP:32; C:[X,R,G,B]; Flags:FMFL_LINEAR+FMFL_RENDERTARGET),
    ({0x1F}Name:'X_D3DFMT_LIN_A8';       PC:D3DFMT_A8;           BPP:8;  C:[A      ]; Flags:FMFL_LINEAR),
    ({0x20}Name:'X_D3DFMT_LIN_A8L8';     PC:D3DFMT_A8L8;         BPP:16; C:[A,L    ]; Flags:FMFL_LINEAR),
    ({0x21}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x22}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x23}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x24}Name:'X_D3DFMT_YUY2';         PC:D3DFMT_YUY2;         BPP:16; C:[       ]; Flags:FMFL_LINEAR+FMFL_YUV),
    ({0x25}Name:'X_D3DFMT_UYVY';         PC:D3DFMT_UYVY;         BPP:16; C:[       ]; Flags:FMFL_LINEAR+FMFL_YUV),
    ({0x26}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x27}Name:'X_D3DFMT_R6G5B5';       PC:D3DFMT_R5G6B5;       BPP:16; C:[  R,G,B]; Flags:FMFL_SWIZZLED+FMFL_APROX), // Ddxb : Could D3DFMT_L6V5U5 help?
    ({0x28}Name:'X_D3DFMT_V8U8';         PC:D3DFMT_V8U8;         BPP:16; C:[    V,U]; Flags:FMFL_SWIZZLED),
    ({0x29}Name:'X_D3DFMT_R8B8';         PC:D3DFMT_V8U8;         BPP:16; C:[  R,  B]; Flags:FMFL_SWIZZLED+FMFL_APROX), // Cxbx NOTE: HACK: Totally and utterly wrong :)
    ({0x2A}Name:'X_D3DFMT_D24S8';        PC:D3DFMT_D24S8;        BPP:32; C:[D,S    ]; Flags:FMFL_SWIZZLED),
{$IFDEF DXBX_USE_D3D9}
    ({0x2B}Name:'X_D3DFMT_F24S8';        PC:D3DFMT_D24FS8;       BPP:32; C:[F,S    ]; Flags:FMFL_SWIZZLED),
    ({0x2C}Name:'X_D3DFMT_D16';          PC:D3DFMT_D16;          BPP:16; C:[D      ]; Flags:FMFL_SWIZZLED), // NOTE: D3DFMT_D16 on Xbox is always lockable
    ({0x2D}Name:'X_D3DFMT_F16';          PC:D3DFMT_R16F;         BPP:16; C:[F      ]; Flags:FMFL_SWIZZLED+FMFL_APROX), // Dxbx : Better approximation than D16
    ({0x2E}Name:'X_D3DFMT_LIN_D24S8';    PC:D3DFMT_D24S8;        BPP:32; C:[D,S    ]; Flags:FMFL_LINEAR),
    ({0x2F}Name:'X_D3DFMT_LIN_F24S8';    PC:D3DFMT_D24FS8;       BPP:32; C:[F,S    ]; Flags:FMFL_LINEAR),
    ({0x30}Name:'X_D3DFMT_LIN_D16';      PC:D3DFMT_D16;          BPP:16; C:[D      ]; Flags:FMFL_LINEAR), // NOTE: D3DFMT_D16 on Xbox is always lockable
    ({0x31}Name:'X_D3DFMT_LIN_F16';      PC:D3DFMT_R16F;         BPP:16; C:[F      ]; Flags:FMFL_LINEAR+FMFL_APROX), // Dxbx : Better approximation than D16
    ({0x32}Name:'X_D3DFMT_L16';          PC:D3DFMT_L16;          BPP:16; C:[  L    ]; Flags:FMFL_SWIZZLED),
    ({0x33}Name:'X_D3DFMT_V16U16';       PC:D3DFMT_V16U16;       BPP:32; C:[    V,U]; Flags:FMFL_SWIZZLED),
    ({0x34}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x35}Name:'X_D3DFMT_LIN_L16';      PC:D3DFMT_L16;          BPP:16; C:[  L    ]; Flags:FMFL_LINEAR),
    ({0x36}Name:'X_D3DFMT_LIN_V16U16';   PC:D3DFMT_V16U16;       BPP:32; C:[    V,U]; Flags:FMFL_LINEAR),
    ({0x37}Name:'X_D3DFMT_LIN_R6G5B5';   PC:D3DFMT_R5G6B5;       BPP:16; C:[  R,G,B]; Flags:FMFL_LINEAR+FMFL_APROX), // Dxbx Note: R 1 bit less, G 1 bit more.
    ({0x38}Name:'X_D3DFMT_R5G5B5A1';     PC:D3DFMT_A1R5G5B5;     BPP:16; C:[A,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_APROX),
    ({0x39}Name:'X_D3DFMT_R4G4B4A4';     PC:D3DFMT_A4R4G4B4;     BPP:16; C:[A,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_APROX),
    ({0x3A}Name:'X_D3DFMT_A8B8G8R8';     PC:D3DFMT_A8B8G8R8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_SWIZZLED),
    ({0x3B}Name:'X_D3DFMT_B8G8R8A8';     PC:D3DFMT_A8R8G8B8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_APROX), // Cxbx NOTE: HACK: R<->B Swapped!
    ({0x3C}Name:'X_D3DFMT_R8G8B8A8';     PC:D3DFMT_A8R8G8B8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_APROX),
    ({0x3D}Name:'X_D3DFMT_LIN_R5G5B5A1'; PC:D3DFMT_R5G6B5;       BPP:16; C:[A,R,G,B]; Flags:FMFL_LINEAR+FMFL_APROX),
    ({0x3E}Name:'X_D3DFMT_LIN_R4G4B4A4'; PC:D3DFMT_A4R4G4B4;     BPP:16; C:[A,R,G,B]; Flags:FMFL_LINEAR+FMFL_APROX),
    ({0x3F}Name:'X_D3DFMT_LIN_A8B8G8R8'; PC:D3DFMT_A8B8G8R8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_LINEAR),
{$ELSE} // Direct3D8 :
    ({0x2B}Name:'X_D3DFMT_F24S8';        PC:D3DFMT_D24S8;        BPP:32; C:[F,S    ]; Flags:FMFL_SWIZZLED+FMFL_APROX), // NOTE: Hack!! PC does not have D3DFMT_F24S8 (Float vs Int)
    ({0x2C}Name:'X_D3DFMT_D16';          PC:D3DFMT_D16;          BPP:16; C:[D      ]; Flags:FMFL_SWIZZLED), // NOTE: D3DFMT_D16 on Xbox is always lockable
    ({0x2D}Name:'X_D3DFMT_F16';          PC:D3DFMT_D16;          BPP:16; C:[F      ]; Flags:FMFL_SWIZZLED+FMFL_APROX), // NOTE: Hack!! PC does not have D3DFMT_F16 (Float vs Int)
    ({0x2E}Name:'X_D3DFMT_LIN_D24S8';    PC:D3DFMT_D24S8;        BPP:32; C:[D,S    ]; Flags:FMFL_LINEAR),
    ({0x2F}Name:'X_D3DFMT_LIN_F24S8';    PC:D3DFMT_D24S8;        BPP:32; C:[F,S    ]; Flags:FMFL_LINEAR+FMFL_APROX), // NOTE: Hack!! PC does not have D3DFMT_F24S8 (Float vs Int)
    ({0x30}Name:'X_D3DFMT_LIN_D16';      PC:D3DFMT_D16;          BPP:16; C:[D      ]; Flags:FMFL_LINEAR), // NOTE: D3DFMT_D16 on Xbox is always lockable
    ({0x31}Name:'X_D3DFMT_LIN_F16';      PC:D3DFMT_D16;          BPP:16; C:[F      ]; Flags:FMFL_LINEAR+FMFL_APROX), // NOTE: Hack!! PC does not have D3DFMT_F16 (Float vs Int)
    ({0x32}Name:'X_D3DFMT_L16';          PC:D3DFMT_L6V5U5;       BPP:16; C:[  L    ]; Flags:FMFL_SWIZZLED+FMFL_APROX),
    ({0x33}Name:'X_D3DFMT_V16U16';       PC:D3DFMT_V16U16;       BPP:32; C:[    V,U]; Flags:FMFL_SWIZZLED),
    ({0x34}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[       ]; Flags:0),
    ({0x35}Name:'X_D3DFMT_LIN_L16';      PC:D3DFMT_L6V5U5;       BPP:16; C:[  L    ]; Flags:FMFL_LINEAR+FMFL_APROX),
    ({0x36}Name:'X_D3DFMT_LIN_V16U16';   PC:D3DFMT_V16U16;       BPP:32; C:[    V,U]; Flags:FMFL_LINEAR),
    ({0x37}Name:'X_D3DFMT_LIN_R6G5B5';   PC:D3DFMT_R5G6B5;       BPP:16; C:[  R,G,B]; Flags:FMFL_LINEAR+FMFL_APROX), // Dxbx Note: R 1 bit less, G 1 bit more
    ({0x38}Name:'X_D3DFMT_R5G5B5A1';     PC:D3DFMT_A1R5G5B5;     BPP:16; C:[A,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_APROX),
    ({0x39}Name:'X_D3DFMT_R4G4B4A4';     PC:D3DFMT_A4R4G4B4;     BPP:16; C:[A,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_APROX),
    ({0x3A}Name:'X_D3DFMT_A8B8G8R8';     PC:D3DFMT_A8R8G8B8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_APROX), // Cxbx NOTE: HACK: R<->B Swapped!
    ({0x3B}Name:'X_D3DFMT_B8G8R8A8';     PC:D3DFMT_A8R8G8B8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_APROX), // Cxbx NOTE: HACK: R<->B Swapped!
    ({0x3C}Name:'X_D3DFMT_R8G8B8A8';     PC:D3DFMT_A8R8G8B8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_SWIZZLED+FMFL_APROX),
    ({0x3D}Name:'X_D3DFMT_LIN_R5G5B5A1'; PC:D3DFMT_R5G6B5;       BPP:16; C:[A,R,G,B]; Flags:FMFL_LINEAR+FMFL_APROX), // Note : PC has no Alpha!
    ({0x3E}Name:'X_D3DFMT_LIN_R4G4B4A4'; PC:D3DFMT_A4R4G4B4;     BPP:16; C:[A,R,G,B]; Flags:FMFL_LINEAR+FMFL_APROX),
    ({0x3F}Name:'X_D3DFMT_LIN_A8B8G8R8'; PC:D3DFMT_A8R8G8B8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_LINEAR+FMFL_APROX), // Cxbx NOTE: HACK: R<->B Swapped!
{$ENDIF}
    ({0x40}Name:'X_D3DFMT_LIN_B8G8R8A8'; PC:D3DFMT_A8R8G8B8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_LINEAR+FMFL_APROX),
    ({0x41}Name:'X_D3DFMT_LIN_R8G8B8A8'; PC:D3DFMT_A8R8G8B8;     BPP:32; C:[A,R,G,B]; Flags:FMFL_LINEAR+FMFL_APROX),
    // Maybe cut off here and handle the last two separately?
    ({0x42}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x43}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x44}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x45}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x46}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x47}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x48}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x49}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x4A}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x4B}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x4C}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x4D}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x4E}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x4F}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x50}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x51}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x52}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x53}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x54}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x55}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x56}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x57}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x58}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x59}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x5A}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x5B}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x5C}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x5D}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x5E}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x5F}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x60}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x61}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x62}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x63}Name:'?';                     PC:D3DFMT_UNKNOWN;      BPP:0;  C:[]; Flags:0),
    ({0x64}Name:'X_D3DFMT_VERTEXDATA';   PC:D3DFMT_VERTEXDATA;   BPP:8;  C:[]; Flags:FMFL_LINEAR),
    ({0x65}Name:'X_D3DFMT_INDEX16';      PC:D3DFMT_INDEX16;      BPP:16; C:[]; Flags:FMFL_LINEAR) // Dxbx addition : Pass-through internal format that shouldn't raise a warning :
  );

implementation

uses
  uConvert; // EmuXB2PC_D3DTSS

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
  if PCState = D3DSAMP_UNSUPPORTED then
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
  if PCState = D3DSAMP_UNSUPPORTED then
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
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  Result := PDWORD(@aValue)^;
end;

function DW2F(const aValue: DWORD): FLOAT;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  Result := PFLOAT(@aValue)^;
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

function EmuXBFormatBPP(Format: X_D3DFORMAT): DWORD;
// Returns the number of bits per pixel for the given format
begin
  if Format in [X_D3DFMT_L8..X_D3DFMT_INDEX16] then
    Result := D3DFMT_INFO[Format].BPP
  else
    Result := 0;
end;

// is this format swizzled, and if so - how many BPP?
function EmuXBFormatIsSwizzled(Format: X_D3DFORMAT): BOOL_;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  if Format in [X_D3DFMT_L8..X_D3DFMT_INDEX16] then
    Result := (D3DFMT_INFO[Format].Flags and FMFL_SWIZZLED) > 0
  else
    Result := False;
end;

// is this format yuv?
function EmuXBFormatIsYUV(Format: X_D3DFORMAT): BOOL_;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if Format in [X_D3DFMT_L8..X_D3DFMT_INDEX16] then
    Result := (D3DFMT_INFO[Format].Flags and FMFL_YUV) > 0
  else
    Result := False;
end;

// is this format compressed?
function EmuXBFormatIsCompressed(Format: X_D3DFORMAT): BOOL_;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  if Format in [X_D3DFMT_L8..X_D3DFMT_INDEX16] then
    Result := (D3DFMT_INFO[Format].Flags and FMFL_COMPRESSED) > 0
  else
    Result := False;
end;

// is this format linear?
function EmuXBFormatIsLinear(Format: X_D3DFORMAT): BOOL_;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  if Format in [X_D3DFMT_L8..X_D3DFMT_INDEX16] then
    Result := (D3DFMT_INFO[Format].Flags and FMFL_LINEAR) > 0
  else
    Result := False;
end;

function EmuXBFormatIsRenderTarget(Format: X_D3DFORMAT): BOOL_;
begin
  if Format in [X_D3DFMT_L8..X_D3DFMT_INDEX16] then
    Result := (D3DFMT_INFO[Format].Flags and FMFL_RENDERTARGET) > 0
  else
    Result := False;
end;

function EmuXBFormatIsDepthBuffer(Format: X_D3DFORMAT): BOOL_;
begin
  if Format in [X_D3DFMT_L8..X_D3DFMT_INDEX16] then
    Result := ([D,F] * D3DFMT_INFO[Format].C) <> []
  else
    Result := False;
end;

function EmuXBFormatHasChannel(Format: X_D3DFORMAT; Channel: TDxbxChannel): BOOL_;
begin
  if Format in [X_D3DFMT_L8..X_D3DFMT_INDEX16] then
    Result := (Channel in D3DFMT_INFO[Format].C)
  else
    Result := False;
end;

function EmuXBFormatHasAlpha(Format: X_D3DFORMAT): BOOL_;
begin
  Result := EmuXBFormatHasChannel(Format, A);
end;

function DxbxEncodeDimensionsIntoSize(const Width, Height, Pitch: DWORD): DWORD;
begin
  Result := ((( Width                                   - 1){shl X_D3DSIZE_WIDTH_SHIFT}) and X_D3DSIZE_WIDTH_MASK )
         or ((( Height                                  - 1) shl X_D3DSIZE_HEIGHT_SHIFT) and X_D3DSIZE_HEIGHT_MASK)
         or ((((Pitch div X_D3DTEXTURE_PITCH_ALIGNMENT) - 1) shl X_D3DSIZE_PITCH_SHIFT ) and X_D3DSIZE_PITCH_MASK );
end;

procedure DxbxDecodeSizeIntoDimensions(const Size: DWORD; pWidth, pHeight, pPitch: PDWORD);
begin
  {out}pWidth^  := (((Size and X_D3DSIZE_WIDTH_MASK ){shr X_D3DSIZE_WIDTH_SHIFT}) + 1);
  {out}pHeight^ := (((Size and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT) + 1);
  if Assigned(pPitch) then
    {out}pPitch^  := (((Size and X_D3DSIZE_PITCH_MASK ) shr X_D3DSIZE_PITCH_SHIFT ) + 1) * X_D3DTEXTURE_PITCH_ALIGNMENT;
end;

function DxbxEncodeResourceFormat(const bCubeMap, bBorder: Boolean;
  const dwDimens, dwFormat, dwMipMap, dwUSize, dwVSize, dwPSize: DWORD): DWORD;
begin
  Result := 0;
  if bCubeMap then Result := Result or X_D3DFORMAT_CUBEMAP;
  if bBorder  then Result := Result or X_D3DFORMAT_BORDERSOURCE_COLOR;
  Result := Result or (dwDimens shl X_D3DFORMAT_DIMENSION_SHIFT);
  Result := Result or (dwFormat shl X_D3DFORMAT_FORMAT_SHIFT);
  Result := Result or (dwMipMap shl X_D3DFORMAT_MIPMAP_SHIFT);
  // TODO : Convert these power-of-2 sizes to the powers itself :
  Result := Result or (dwUSize  shl X_D3DFORMAT_USIZE_SHIFT);
  Result := Result or (dwVSize  shl X_D3DFORMAT_VSIZE_SHIFT);
  Result := Result or (dwPSize  shl X_D3DFORMAT_PSIZE_SHIFT);
end;

procedure DxbxDecodeResourceFormat(const ResourceFormat: DWORD;
  const pCubeMap: PBoolean;
  const pBorder: PBoolean;
  const pDimens: PDWORD;
  const pFormat: PDWORD;
  const pMipMap: PDWORD;
  const pUSize: PDWORD;
  const pVSize: PDWORD;
  const pPSize: PDWORD);
begin
  if Assigned(pCubeMap) then pCubeMap^ :=        (ResourceFormat and X_D3DFORMAT_CUBEMAP) > 0;
  if Assigned(pBorder)  then pBorder^  :=        (ResourceFormat and X_D3DFORMAT_BORDERSOURCE_COLOR) > 0;
  if Assigned(pDimens)  then pDimens^  :=        (ResourceFormat and X_D3DFORMAT_DIMENSION_MASK) shr X_D3DFORMAT_DIMENSION_SHIFT;
  if Assigned(pFormat)  then pFormat^  :=        (ResourceFormat and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT;
  if Assigned(pMipMap)  then pMipMap^  :=        (ResourceFormat and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
  if Assigned(pUSize)   then pUSize^   := 1 shl ((ResourceFormat and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
  if Assigned(pVSize)   then pVSize^   := 1 shl ((ResourceFormat and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
  if Assigned(pPSize)   then pPSize^   := 1 shl ((ResourceFormat and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
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
    D3DFMT_A8           : Result := 'D3DFMT_A8';
    D3DFMT_A8R3G3B2     : Result := 'D3DFMT_A8R3G3B2';
    D3DFMT_X4R4G4B4     : Result := 'D3DFMT_X4R4G4B4';
    D3DFMT_A2B10G10R10  : Result := 'D3DFMT_A2B10G10R10';
    D3DFMT_G16R16       : Result := 'D3DFMT_G16R16';
    D3DFMT_A8P8         : Result := 'D3DFMT_A8P8';
    D3DFMT_P8           : Result := 'D3DFMT_P8';
    D3DFMT_L8           : Result := 'D3DFMT_L8';
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

function DxbxGetResourceType(const pResource: PX_D3DResource): X_D3DRESOURCETYPE;
begin
  case pResource.Common and X_D3DCOMMON_TYPE_MASK of
    X_D3DCOMMON_TYPE_VERTEXBUFFER: Result := X_D3DRTYPE_VERTEXBUFFER;
    X_D3DCOMMON_TYPE_INDEXBUFFER: Result := X_D3DRTYPE_INDEXBUFFER;
    X_D3DCOMMON_TYPE_PUSHBUFFER: Result := X_D3DRTYPE_PUSHBUFFER;
    X_D3DCOMMON_TYPE_FIXUP: Result := X_D3DRTYPE_FIXUP;
    X_D3DCOMMON_TYPE_PALETTE: Result := X_D3DRTYPE_PALETTE;
    X_D3DCOMMON_TYPE_TEXTURE:
    begin
      if (PX_D3DBaseTexture(pResource).Format and X_D3DFORMAT_CUBEMAP) > 0 then
        Result := X_D3DRTYPE_CUBETEXTURE
      else
        if ((PX_D3DBaseTexture(pResource).Format and X_D3DFORMAT_DIMENSION_MASK) shr X_D3DFORMAT_DIMENSION_SHIFT) = 3 then
          Result := X_D3DRTYPE_VOLUMETEXTURE
        else
          Result := X_D3DRTYPE_TEXTURE;
    end;
    X_D3DCOMMON_TYPE_SURFACE:
    begin
      if ((PX_D3DPixelContainer(pResource).Format and X_D3DFORMAT_DIMENSION_MASK) shr X_D3DFORMAT_DIMENSION_SHIFT) = 3 then
        Result := X_D3DRTYPE_VOLUME
      else
        Result := X_D3DRTYPE_SURFACE;
    end;
  else
    Result := X_D3DRESOURCETYPE(0);
  end;
end;

function IsResourcePixelContainer(const aResource: PX_D3DResource): Boolean;
begin
  Result := ((aResource.Common and X_D3DCOMMON_TYPE_MASK) = X_D3DCOMMON_TYPE_TEXTURE)
         or ((aResource.Common and X_D3DCOMMON_TYPE_MASK) = X_D3DCOMMON_TYPE_SURFACE);
end;

function CommonToStr(const Common: DWORD): string;
var
  Value: DWORD;
begin
  Result := '';

  Value := Common and X_D3DCOMMON_REFCOUNT_MASK;
  Result := Format(' RefCount=%d', [Value]);

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
  Result := Result + Format(' IntRefCount=%d', [Value]);

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
  {$IFNDEF DXBX_USE_OPENGL}if Value > 2 then{$ENDIF} Result := Result + Format(' %dD', [Value]);

  Value := (ResourceFormat and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT;
  Result := Result + Format(' FORMAT=%s', [X_D3DFORMAT2String(Value)]);

  Value := (ResourceFormat and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
  {$IFNDEF DXBX_USE_OPENGL}if Value > 1 then{$ENDIF} Result := Result + Format(' MIPMAP=%d', [Value]);

  Value := (ResourceFormat and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT;
  {$IFNDEF DXBX_USE_OPENGL}if Value > 0 then{$ENDIF} Result := Result + Format(' USIZE=%d', [Value]);

  Value := (ResourceFormat and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT;
  {$IFNDEF DXBX_USE_OPENGL}if Value > 0 then{$ENDIF} Result := Result + Format(' VSIZE=%d', [Value]);

  Value := (ResourceFormat and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT;
  {$IFNDEF DXBX_USE_OPENGL}if Value > 0 then{$ENDIF} Result := Result + Format(' PSIZE=%d', [Value]);
end;

function ResourceSizeToStr(const ResourceSize: DWORD): string;
var
  Value: DWORD;
begin
  Result := '';
  Value := (ResourceSize and X_D3DSIZE_WIDTH_MASK) {shr X_D3DSIZE_WIDTH_SHIFT};
  if Value > 0 then Result := Result + Format(' WIDTH=%d', [Value + 1]);

  Value := (ResourceSize and X_D3DSIZE_HEIGHT_MASK) shr X_D3DSIZE_HEIGHT_SHIFT;
  if Value > 0 then Result := Result + Format(' HEIGHT=%d', [Value + 1]);

  Value := (ResourceSize and X_D3DSIZE_PITCH_MASK) shr X_D3DSIZE_PITCH_SHIFT;
  if Value > 0 then Result := Result + Format(' PITCH=%d', [(Value + 1) * X_D3DTEXTURE_PITCH_ALIGNMENT]);
end;

function ResourceToString(const aValue: PX_D3DResource): string;
begin
  Result := Format('0x%.08x', [UIntPtr(aValue)]);
  if Assigned(aValue) then
  begin
    Result := Result + Format(' (Common=%.08x Data=%.08x Emu=%.08x', [
              aValue.Common, aValue.Data, aValue.Emu.Lock]);
    if IsResourcePixelContainer(aValue) then
    begin
      Result := Result + Format(' Format=%.08x Size=%.08x :', [
                PX_D3DPixelContainer(aValue).Format, PX_D3DPixelContainer(aValue).Size]);

      Result := Result + CommonToStr(aValue.Common);
      Result := Result + ResourceFormatToStr(PX_D3DPixelContainer(aValue).Format);
      Result := Result + ResourceSizeToStr(PX_D3DPixelContainer(aValue).Size);
      if (aValue.Common and X_D3DCOMMON_TYPE_MASK) = X_D3DCOMMON_TYPE_SURFACE then
      begin
        if PX_D3DSurface(aValue).Parent = aValue then
          Result := Result + ' [Parent:CYLCLE!]'
        else
          Result := Result + ' [Parent:' + ResourceToString(PX_D3DSurface(aValue).Parent) + ']';
      end;
    end
    else
      Result := Result + ' :' + CommonToStr(aValue.Common);

    Result := Result + ')';
  end;
end;

function DxbxD3DErrorString(hResult: HRESULT): string;
begin
  // TODO -oDXBX: Use DXGetErrorDescription(hResult) (requires another DLL though)
  case hResult of
    D3DERR_INVALIDCALL: Result := 'Invalid Call';
    D3DERR_NOTAVAILABLE: Result := 'Not Available';
    D3DERR_OUTOFVIDEOMEMORY: Result := 'Out of Video Memory';
  else
{$IFDEF DXBX_USE_D3D9}
    Result := string(DXGetErrorString9(hResult)); // Source : http://www.fairyengine.com/articles/dxmultiviews.htm
    Result := Result + #13#10 + string(DXGetErrorDescription9(hResult)); // Source : http://www.gamedev.net/community/forums/showfaq.asp?forum_id=10
{$ELSE}
    Result := D3DXGetErrorString(hResult); // Source : http://www.gamedev.net/community/forums/topic.asp?topic_id=16157
{$ENDIF}

    // If this gives a too cryptic output, try the old method :
    if (Length(Result) < 8) or (Result[1] < 'A') or (Result[1] > 'z') then
      Result := D3DErrorString(hResult);
  end;
end;

procedure DxbxD3DError(aFunction, aMessage: string; pResource: PX_D3DResource = nil; hRet: HRESULT = 0; bHalt: Boolean = True);
var
  Line: string;
begin
  Line := Format('%s failed : %s', [aFunction, aMessage]);
  if Assigned(pResource) then
    Line := Line + #13#10'  Resource : ' + ResourceToString(pResource);
  if FAILED(hRet) then
    Line := Line + #13#10'  D3DError : ' + DxbxD3DErrorString(hRet);

  if bHalt then
    DxbxKrnlCleanup(Line)
  else
    EmuWarning(Line);
end;

function GetD3DFormat(aD3DPixelContainer: PX_D3DPixelContainer): X_D3DFORMAT;
begin
  Result := X_D3DFORMAT((aD3DPixelContainer.Format and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT);
end;

procedure DxbxGetFormatRelatedVariables(
  const pPixelContainer: PX_D3DPixelContainer;
  out DecodedInfo: RDxbxDecodedPixelContainer); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  x: uint;
begin
  DxbxGetFormatRelatedVariables(
    pPixelContainer.Format, pPixelContainer.Size,
    {out}DecodedInfo);
  {out}DecodedInfo.pPixelContainer := pPixelContainer;

  // Check that the maximum mipmap level is not exceeded :
  if pPixelContainer.Size = 0 then
  begin
    if DecodedInfo.bIs3D then
      x := X_MAX_MIPMAPS_VOLUME
    else
      x := X_MAX_MIPMAPS;
    if DecodedInfo.dwMipMapLevels > x then
      DxbxD3DError('DxbxGetFormatRelatedVariables', 'MipMapLevel exceeded!', pPixelContainer);
  end;
end;

procedure DxbxGetFormatRelatedVariables(
  LocalFormat, LocalSize: DWORD;
  out DecodedInfo: RDxbxDecodedPixelContainer); {NOPATCH}
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  v: uint32;
  x,y,d,minsize: uint;
begin
  ZeroMemory(@DecodedInfo, SizeOf(DecodedInfo));
  with {out}DecodedInfo do
  begin
    X_Format := X_D3DFORMAT((LocalFormat and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT);
    dwBPP := EmuXBFormatBPP(X_Format);

    // Cubemap textures have the X_D3DFORMAT_CUBEMAP bit set (also, their size is 6 times a normal texture)
    bIsCubeMap := (LocalFormat and X_D3DFORMAT_CUBEMAP) > 0;

    bIsBorderSource := (LocalFormat and X_D3DFORMAT_BORDERSOURCE_COLOR) > 0;

    // Volumes have 3 dimensions, the rest have 2.
    bIs3D := ((LocalFormat and X_D3DFORMAT_DIMENSION_MASK) shr X_D3DFORMAT_DIMENSION_SHIFT) = 3;
    bIsSwizzled := EmuXBFormatIsSwizzled(X_Format);
    bIsCompressed := EmuXBFormatIsCompressed(X_Format);

    if LocalSize > 0 then
    begin
      // This case cannot be reached for Cube maps or Volumes, as those use the 'power of two' "format" :
      DxbxDecodeSizeIntoDimensions(LocalSize, @dwWidth, @dwHeight, @dwRowPitch);
      dwDepth := 1;
      dwMipMapLevels := 1;
    end
    else
    begin
      // This case uses 'power of two' format, which can be used by Cube maps and Volumes
      // (which means we have to calculate Face and Slice pitch too) :
      dwWidth := 1 shl ((LocalFormat and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
      dwHeight := 1 shl ((LocalFormat and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);
      dwDepth  := 1 shl ((LocalFormat and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT);
      dwMipMapLevels := (LocalFormat and X_D3DFORMAT_MIPMAP_MASK) shr X_D3DFORMAT_MIPMAP_SHIFT;
      dwRowPitch := dwWidth * dwBPP div 8;
    end;

    // Calculate a few variables for the first mipmap level (even when this is not a mipmapped texture):
    SlicePitches[0] := dwRowPitch * dwHeight;
    MipMapSlices[0] := dwDepth;
    MipMapFormats[0] := LocalFormat;
    MipMapOffsets[0] := 0;

    // Also calculate mipmap level 1 offset (even when no mipmaps are allowed) as this is used for size :
    MipMapOffsets[1] := SlicePitches[0] * dwDepth; // Offset 2nd level = size of 1st level

    // Since we know how big the first mipmap level is, calculate all following mipmap offsets :
    MinSize := 0;
    if dwMipMapLevels > 1 then
    begin
      x := (LocalFormat and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT;
      y := (LocalFormat and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT;
      d := (LocalFormat and X_D3DFORMAT_PSIZE_MASK) shr X_D3DFORMAT_PSIZE_SHIFT;
      if bIsCompressed then
        MinSize := 2;
      if x < MinSize then x := MinSize;
      if y < MinSize then y := MinSize;
      // Note : d (depth) is not limited

      LocalFormat := LocalFormat and (not (X_D3DFORMAT_USIZE_MASK or X_D3DFORMAT_VSIZE_MASK or X_D3DFORMAT_PSIZE_MASK));
      for v := 1 to dwMipMapLevels - 1 do
      begin
        // Halve each dimension until it reaches it's lower bound :
        if x > MinSize then Dec(x);
        if y > MinSize then Dec(y);
        if d > 0 then Dec(d);

        MipMapSlices[v] := 1 shl d;
        MipMapFormats[v] := LocalFormat or (x shl X_D3DFORMAT_USIZE_SHIFT) or (y shl X_D3DFORMAT_VSIZE_SHIFT) or (d shl X_D3DFORMAT_PSIZE_SHIFT);
        SlicePitches[v] := ((1 shl (x+y)) * dwBPP) div 8;
        // Calculate the next offset by adding the size of this level to the previous offset :
        MipMapOffsets[v+1] := MipMapOffsets[v] + (SlicePitches[v] * (1 shl d));
      end;
    end;

    dwMinXYValue := 1 shl MinSize;

    if bIsCubeMap then
    begin
      // Calculate where the next face is located (step over all mipmaps) :
      dwFacePitch := MipMapOffsets[dwMipMapLevels];
      // Align it up :
      dwFacePitch := RoundUp(dwFacePitch, X_D3DTEXTURE_CUBEFACE_ALIGNMENT);
    end;
(*
    // Dxbx addition : Limit Turok to 6 MipMap levels - anything more leads to crashes... but WHY?!?
    if IsRunning(TITLEID_TurokNTSC) then
      MaxMipMapLevels := 6
    else
      // On the other hand, Battlestar Galactica crashes if we limit to 6 (9 does the trick). Here also : but WHY?!?
      MaxMipMapLevels := 9;

    if (dwMipMapLevels > MaxMipMapLevels) then
    begin
      EmuWarning('Limited MipMapLevels to %d (instead of %d)', [MaxMipMapLevels, dwMipMapLevels]);
      dwMipMapLevels := MaxMipMapLevels;
    end;
*)
  end; // with Format
end; // DxbxGetFormatRelatedVariables

end.

