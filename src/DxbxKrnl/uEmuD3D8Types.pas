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
  Direct3D8
  ;

const
  X_VSCM_96 = 0;
  X_VSCM_192 = 1;
  X_VSCM_192FIXEDPIPELINE = 2;
  X_VSCM_NONERESERVED = 16;

  // Vertex shader types
  X_VST_NORMAL = 1;
  X_VST_READWRITE = 2;
  X_VST_STATE = 3;
  X_PIXELSHADER_FAKE_HANDLE = $DEADBEEF;

  X_D3DLOCK_NOFLUSH  = $00000010; // Xbox extension
  X_D3DLOCK_NOOVERWRITE = $00000020;
  X_D3DLOCK_TILED = $00000040; // Xbox extension
  X_D3DLOCK_READONLY = $00000080;

type
  X_VERTEXSHADERCONSTANTMODE = DWORD;
  X_D3DFORMAT = Dword;

  _X_VERTEXSHADERINPUT = record
    IndexOfStream : DWORD;
    Offset : DWORD;
    Format : DWORD;
    TesselationType : BYTE;
    TesselationSource : BYTE;
  end;

  X_VERTEXSHADERINPUT = _X_VERTEXSHADERINPUT;




  X_D3DVertexShader = record
    UnknownA: DWORD;
    Handle: DWORD;
    UnknownB: DWORD;
    Flags: DWORD;
    UnknownC: array[0..59] of DWORD;
  end;


  _STREAM_DYNAMIC_PATCH_ = record
    NeedPatch : BOOL;       // This is to know whether is data which must be patched
    ConvertedStride : DWORD;
    NbrTypes : DWORD;        // Number of the stream data types
    pTypes : UINT;         // The stream data types (xbox)
  end;
  STREAM_DYNAMIC_PATCH = _STREAM_DYNAMIC_PATCH_;

  _VERTEX_DYNAMIC_PATCH_ = record
    NbrStreams : UINT; // The number of streams the vertex shader uses
    pStreamPatches : STREAM_DYNAMIC_PATCH;
  end;

  VERTEX_DYNAMIC_PATCH = _VERTEX_DYNAMIC_PATCH_;      

  _VERTEX_SHADER = record
    aHandle : DWORD;
    // These are the parameters given by the XBE,
    // we save them to be be able to return them when necassary.
    Size : UINT;
    pDeclaration : DWORD;
    DeclarationSize : DWORD;
    pFunction : DWORD;
    FunctionSize : DWORD;
    aType : DWORD;
    Status : DWORD;
    // Needed for dynamic stream patching
    VertexDynamicPatch : VERTEX_DYNAMIC_PATCH;
  end;
  VERTEX_SHADER = _VERTEX_SHADER;

  _X_VERTEXATTRIBUTEFORMAT = record
    pVertexShaderInput : Array [0..15] of X_VERTEXSHADERINPUT;
  end;
  X_VERTEXATTRIBUTEFORMAT = _X_VERTEXATTRIBUTEFORMAT;

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
    BufferSurfaces: array[0..2] of IDirect3DSurface8;
    DepthStencilSurface: IDirect3DSurface8;
  end;

  X_D3DPRESENT_PARAMETERS = _X_D3DPRESENT_PARAMETERS;

  _X_D3DGAMMARAMP = record
    red: array[0..255] of BYTE;
    green: array[0..255] of BYTE;
    blue: array[0..255] of BYTE;
  end;

  X_D3DGAMMARAMP = _X_D3DGAMMARAMP;

  _X_D3DPRIMITIVETYPE =
    (
    X_D3DPT_POINTLIST = 1,
    X_D3DPT_LINELIST = 2,
    X_D3DPT_LINELOOP = 3, // Xbox only
    X_D3DPT_LINESTRIP = 4,
    X_D3DPT_TRIANGLELIST = 5,
    X_D3DPT_TRIANGLESTRIP = 6,
    X_D3DPT_TRIANGLEFAN = 7,
    X_D3DPT_QUADLIST = 8, // Xbox only
    X_D3DPT_QUADSTRIP = 9, // Xbox only
    X_D3DPT_POLYGON = 10, // Xbox only

    X_D3DPT_MAX = 11,
    X_D3DPT_INVALID = $7FFFFFFF
    );
  X_D3DPRIMITIVETYPE = _X_D3DPRIMITIVETYPE;


  _X_D3DRESOURCETYPE =
    (
    X_D3DRTYPE_NONE = 0,
    X_D3DRTYPE_SURFACE = 1,
    X_D3DRTYPE_VOLUME = 2,
    X_D3DRTYPE_TEXTURE = 3,
    X_D3DRTYPE_VOLUMETEXTURE = 4,
    X_D3DRTYPE_CUBETEXTURE = 5,
    X_D3DRTYPE_VERTEXBUFFER = 6,
    X_D3DRTYPE_INDEXBUFFER = 7,
    X_D3DRTYPE_PUSHBUFFER = 8,
    X_D3DRTYPE_PALETTE = 9,
    X_D3DRTYPE_FIXUP = 10,

    X_D3DRTYPE_FORCE_DWORD = $7FFFFFFF
    );
  X_D3DRESOURCETYPE = _X_D3DRESOURCETYPE;

  _X_D3DDISPLAYMODE = record
    Width: Integer;
    Height: Integer;
    RefreshRate: Integer;
    Flags: DWord;
    Format: X_D3DFORMAT;
  end;

  X_D3DDISPLAYMODE = _X_D3DDISPLAYMODE;

  _X_D3DSURFACE_DESC = record
    Format: X_D3DFORMAT;
    aType: X_D3DRESOURCETYPE;
    Usage: DWORD;
    Size: UINT;
    MultiSampleType: D3DMULTISAMPLE_TYPE;
    Width: UINT;
    Height: UINT;
  end;

  X_D3DSURFACE_DESC = _X_D3DSURFACE_DESC;

  _X_D3DFIELDTYPE =
    (
    X_D3DFIELD_ODD = 1,
    X_D3DFIELD_EVEN = 2,
    X_D3DFIELD_PROGRESSIVE = 3,
    X_D3DFIELD_FORCE_DWORD = $7FFFFFFF
    );
  X_D3DFIELDTYPE = _X_D3DFIELDTYPE;

  _X_D3DFIELD_STATUS = record
    Field: X_D3DFIELDTYPE;
    VBlankCount: UINT;
  end;
  X_D3DFIELD_STATUS = _X_D3DFIELD_STATUS;

  _D3DVBLANKDATA = record
    VBlank: DWORD;
    Swap: DWORD;
    Flags: DWORD;
  end;
  D3DVBLANKDATA = _D3DVBLANKDATA;
  PD3DVBLANKDATA = ^D3DVBLANKDATA;

  D3DVBLANKCALLBACK = procedure (const pData: PD3DVBLANKDATA); cdecl;

  X_D3DResource = Class
  public
    Common : DWORD;
    Data : DWORD;
    Lock : DWORD;
    EmuResource8 : IDirect3DResource8;
    EmuBaseTexture8 : IDirect3DBaseTexture8;
    EmuTexture8 : IDirect3DTexture8;
    EmuVolumeTexture8 : IDirect3DVolumeTexture8;
    EmuCubeTexture8 : IDirect3DCubeTexture8;
    EmuSurface8 : IDirect3DSurface8;
    EmuVertexBuffer8 : IDirect3DVertexBuffer8;
    EmuIndexBuffer8 : IDirect3DIndexBuffer8;
  end;


  X_D3DPixelContainer = Class( X_D3DResource )
  public
    Format: X_D3DFORMAT;
    Size: DWORD;
  end;

  X_D3DVertexBuffer = Class ( X_D3DResource )

  end;

  X_D3DPushBuffer = Class ( X_D3DResource )
  public
    Size : ULONG;
    AllocationSize : ULONG;
  end;

  X_D3DFixup = Class ( X_D3DResource )
  public
    Run : ULONG;
    Next : ULONG;
    Size : ULONG;
  end;


  X_D3DBaseTexture = Class(X_D3DPixelContainer)

  end;

  X_D3DCubeTexture = Class( X_D3DBaseTexture)

  end;


  X_D3DSurface = class (X_D3DPixelContainer)
  end;

  X_D3DVolumeTexture = Class( X_D3DBaseTexture )

  end;

  _X_STREAMINPUT = record
    VertexBuffer : X_D3DVertexBuffer;
    Stride : UINT;
    Offset : UINT;
  end;

  X_STREAMINPUT = _X_STREAMINPUT;



implementation

end.

