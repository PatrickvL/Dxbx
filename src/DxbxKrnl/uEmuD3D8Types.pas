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
  // Delphi
  Windows,
  // Directx
  Direct3D8,
  // Xbox
  uXbe
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
  X_D3DFORMAT = DWord;
  X_D3DBLENDOP = DWord;
  X_D3DBLEND = DWord;
  X_D3DCMPFUNC = DWord;
  X_D3DFILLMODE = DWord;
  X_D3DSHADEMODE = DWord;
  X_D3DTEXTURESTAGESTATETYPE = DWord;
  X_VERTEXSHADERCONSTANTMODE = DWord;

  X_VERTEXSHADERINPUT = packed record
    IndexOfStream: DWord;
    Offset: DWord;
    Format: DWord;
    TesselationType: Byte;
    TesselationSource: Byte;
  end;

  X_D3DTILE = packed record
    Flags: DWORD;
    pMemory: PVOID;
    Size: DWORD;
    Pitch: DWORD;
    ZStartTag: DWORD;
    ZOffset: DWORD;
  end;
  PX_D3DTILE = ^X_D3DTILE;

  X_D3DVertexShader = packed record
    UnknownA: DWord;
    Handle: DWord;
    UnknownB: DWord;
    Flags: DWord;
    UnknownC: array [0..59] of DWord;
  end;
  PX_D3DVertexShader = ^X_D3DVertexShader;

  STREAM_DYNAMIC_PATCH = packed record
    NeedPatch: BOOL;       // This is to know whether is data which must be patched
    ConvertedStride: DWord;
    NbrTypes: DWord;        // Number of the stream data types
    pTypes: UINT;         // The stream data types (xbox)
  end;

  VERTEX_DYNAMIC_PATCH = packed record
    NbrStreams: UINT; // The number of streams the vertex shader uses
    pStreamPatches: STREAM_DYNAMIC_PATCH;
  end;

  VERTEX_SHADER = packed record
    Handle: DWord;
    // These are the parameters given by the XBE,
    // we save them to be be able to return them when necassary.
    Size: UINT;
    pDeclaration: PDWord;
    DeclarationSize: DWord;
    pFunction: PDWord;
    FunctionSize: DWord;
    _Type: DWord;
    Status: DWord;
    // Needed for dynamic stream patching
    VertexDynamicPatch: VERTEX_DYNAMIC_PATCH;
  end;
  PVERTEX_SHADER = ^VERTEX_SHADER;

  X_VERTEXATTRIBUTEFORMAT = packed record
    pVertexShaderInput: array [0..15] of X_VERTEXSHADERINPUT;
  end;
  PX_VERTEXATTRIBUTEFORMAT = ^X_VERTEXATTRIBUTEFORMAT;

  X_D3DPRESENT_PARAMETERS = packed record
    BackBufferWidth: UINT;
    BackBufferHeight: UINT;
    BackBufferFormat: X_D3DFORMAT;
    BackBufferCount: UINT;

    MultiSampleType: D3DMULTISAMPLE_TYPE;

    SwapEffect: D3DSWAPEFFECT;
    hDeviceWindow: HWND;
    Windowed: LONGBOOL;
    EnableAutoDepthStencil: LONGBOOL;
    AutoDepthStencilFormat: X_D3DFORMAT;
    Flags: DWord;

    FullScreen_RefreshRateInHz: UINT;
    FullScreen_PresentationInterval: UINT;
    // The Windows DirectX8 variant ends here
    // This check guarantees identical layout, compared to Direct3D8._D3DPRESENT_PARAMETERS_:
    // Assert(Integer(@(PX_D3DPRESENT_PARAMETERS(nil).BufferSurfaces[0])) = SizeOf(_D3DPRESENT_PARAMETERS_));
    BufferSurfaces: array[0..2] of IDirect3DSurface8;
    DepthStencilSurface: IDirect3DSurface8;
  end;
  PX_D3DPRESENT_PARAMETERS = ^X_D3DPRESENT_PARAMETERS;

  X_D3DGAMMARAMP = packed record
    Red: array[0..255] of Byte;
    Green: array[0..255] of Byte;
    Blue: array[0..255] of Byte;
  end;

  X_D3DPRIMITIVETYPE = (
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

  X_D3DRESOURCETYPE = (
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

  X_D3DDISPLAYMODE = packed record
    Width: UINT;
    Height: UINT;
    RefreshRate: UINT;
    Flags: DWORD;
    Format: X_D3DFORMAT;
  end;

  X_D3DSURFACE_DESC = packed record
    Format: X_D3DFORMAT;
    _Type: X_D3DRESOURCETYPE;
    Usage: DWord;
    Size: UINT;
    MultiSampleType: D3DMULTISAMPLE_TYPE;
    Width: UINT;
    Height: UINT;
  end;
  PX_D3DSURFACE_DESC = ^X_D3DSURFACE_DESC;

  X_D3DFIELDTYPE = (
    X_D3DFIELD_ODD = 1,
    X_D3DFIELD_EVEN = 2,
    X_D3DFIELD_PROGRESSIVE = 3,
    X_D3DFIELD_FORCE_DWORD = $7FFFFFFF
  );

  X_D3DFIELD_STATUS = packed record
    Field: X_D3DFIELDTYPE;
    VBlankCount: UINT;
  end;

  D3DVBLANKDATA = packed record
    VBlank: DWord;
    Swap: DWord;
    Flags: DWord;
  end;
  PD3DVBLANKDATA = ^D3DVBLANKDATA;

  D3DVBLANKCALLBACK = procedure (const pData: PD3DVBLANKDATA); cdecl;

  X_D3DResource = object
  protected
    function GetEmuResource8: IDirect3DResource8; inline;
    function GetEmuBaseTexture8: IDirect3DBaseTexture8; inline;
    function GetEmuTexture8: IDirect3DTexture8; inline;
    function GetEmuVolumeTexture8: IDirect3DVolumeTexture8; inline;
    function GetEmuCubeTexture8: IDirect3DCubeTexture8; inline;
    function GetEmuSurface8: IDirect3DSurface8; inline;
    function GetEmuVertexBuffer8: IDirect3DVertexBuffer8; inline;
    function GetEmuIndexBuffer8: IDirect3DIndexBuffer8; inline;
    procedure SetEmuResource8(Value: IDirect3DResource8); inline;
    procedure SetEmuBaseTexture8(Value: IDirect3DBaseTexture8); inline;
    procedure SetEmuTexture8(Value: IDirect3DTexture8); inline;
    procedure SetEmuVolumeTexture8(Value: IDirect3DVolumeTexture8); inline;
    procedure SetEmuCubeTexture8(Value: IDirect3DCubeTexture8); inline;
    procedure SetEmuSurface8(Value: IDirect3DSurface8); inline;
    procedure SetEmuVertexBuffer8(Value: IDirect3DVertexBuffer8); inline;
    procedure SetEmuIndexBuffer8(Value: IDirect3DIndexBuffer8); inline;
  public
    Common: DWord;
    Data: DWord;
    Lock: DWord;

    // Dxnx Note : Delphi doesn't accept interfaces in a union,
    // so we have to use another approach : the following properties
    // all store their data in the same location (which overlaps
    // with Lock, as in the original Cxbx declaration of this type).
    //
    // Be aware that there's no reference-counting possible this way!
    
    property EmuResource8: IDirect3DResource8 read GetEmuResource8 write SetEmuResource8;
    property EmuBaseTexture8: IDirect3DBaseTexture8 read GetEmuBaseTexture8 write SetEmuBaseTexture8;
    property EmuTexture8: IDirect3DTexture8 read GetEmuTexture8 write SetEmuTexture8;
    property EmuVolumeTexture8: IDirect3DVolumeTexture8 read GetEmuVolumeTexture8 write SetEmuVolumeTexture8;
    property EmuCubeTexture8: IDirect3DCubeTexture8 read GetEmuCubeTexture8 write SetEmuCubeTexture8;
    property EmuSurface8: IDirect3DSurface8 read GetEmuSurface8 write SetEmuSurface8;
    property EmuVertexBuffer8: IDirect3DVertexBuffer8 read GetEmuVertexBuffer8 write SetEmuVertexBuffer8;
    property EmuIndexBuffer8: IDirect3DIndexBuffer8 read GetEmuIndexBuffer8 write SetEmuIndexBuffer8;
  end;
  PX_D3DResource = ^X_D3DResource;

const
  // d3d resource "common" masks
  X_D3DCOMMON_REFCOUNT_MASK      = $0000FFFF;
  X_D3DCOMMON_TYPE_MASK          = $00070000;
  X_D3DCOMMON_TYPE_SHIFT         = 16;
  X_D3DCOMMON_TYPE_VERTEXBUFFER  = $00000000;
  X_D3DCOMMON_TYPE_INDEXBUFFER   = $00010000;
  X_D3DCOMMON_TYPE_PUSHBUFFER    = $00020000;
  X_D3DCOMMON_TYPE_PALETTE       = $00030000;
  X_D3DCOMMON_TYPE_TEXTURE       = $00040000;
  X_D3DCOMMON_TYPE_SURFACE       = $00050000;
  X_D3DCOMMON_TYPE_FIXUP         = $00060000;
  X_D3DCOMMON_INTREFCOUNT_MASK   = $00780000;
  X_D3DCOMMON_INTREFCOUNT_SHIFT  = 19;
  X_D3DCOMMON_D3DCREATED         = $01000000;
  X_D3DCOMMON_ISLOCKED           = $02000010; // Surface is currently locked (potential unswizzle candidate)
  X_D3DCOMMON_UNUSED_MASK        = $FE000000;
  X_D3DCOMMON_UNUSED_SHIFT       = 25;

  // special resource data flags (must set _SPECIAL *AND* specific flag(s))
  X_D3DRESOURCE_DATA_FLAG_SPECIAL = $FFFF0000;
  X_D3DRESOURCE_DATA_FLAG_SURFACE = $00000001; // Backbuffer surface, etc
  X_D3DRESOURCE_DATA_FLAG_YUVSURF = $00000002; // YUV memory surface
  X_D3DRESOURCE_DATA_FLAG_D3DREND = $00000004; // D3D Render Target
  X_D3DRESOURCE_DATA_FLAG_D3DSTEN = $00000008; // D3D Stencil Surface

  // special resource lock flags
  X_D3DRESOURCE_LOCK_FLAG_NOSIZE  = $EFFFFFFF;
  
  function IsSpecialResource(x: DWORD): Boolean;

type
  X_D3DPixelContainer = object(X_D3DResource)
  public
    Format: X_D3DFORMAT;
    Size: DWord;
  end;
  PX_D3DPixelContainer = ^X_D3DPixelContainer;

  X_D3DVertexBuffer = object(X_D3DResource)
  public
  end;
  PX_D3DVertexBuffer = ^X_D3DVertexBuffer;

  X_D3DIndexBuffer = object(X_D3DResource)
  public
  end;
  PX_D3DIndexBuffer = ^X_D3DIndexBuffer;

  X_D3DPushBuffer = object(X_D3DResource)
  public
    Size: ULONG;
    AllocationSize: ULONG;
  end;
  PX_D3DPushBuffer = ^X_D3DPushBuffer;

  X_D3DFixup = object(X_D3DResource)
  public
    Run: ULONG;
    Next: ULONG;
    Size: ULONG;
  end;
  PX_D3DFixup = ^X_D3DFixup;

  X_D3DBaseTexture = object(X_D3DPixelContainer)
  end;
  PX_D3DBaseTexture = ^X_D3DBaseTexture;

  X_D3DCubeTexture = object(X_D3DBaseTexture)
  end;
  PX_D3DCubeTexture = ^X_D3DCubeTexture;

  X_D3DSurface = object(X_D3DPixelContainer)
  end;
  PX_D3DSurface = ^X_D3DSurface;
  PPX_D3DSurface = ^PX_D3DSurface;

  X_D3DVolumeTexture = object(X_D3DBaseTexture)
  end;
  PX_D3DVolumeTexture = ^X_D3DVolumeTexture;

  X_STREAMINPUT = packed record
    VertexBuffer: PX_D3DVertexBuffer;
    Stride: UINT;
    Offset: UINT;
  end;
  PX_STREAMINPUT = ^X_STREAMINPUT;

implementation

function IsSpecialResource(x: DWORD): Boolean;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
 Result := (x and X_D3DRESOURCE_DATA_FLAG_SPECIAL) = X_D3DRESOURCE_DATA_FLAG_SPECIAL;
end;

{ X_D3DResource }

function X_D3DResource.GetEmuResource8: IDirect3DResource8;
begin
  Pointer(Result) := Pointer(Lock);
end;

function X_D3DResource.GetEmuBaseTexture8: IDirect3DBaseTexture8;
begin
  Pointer(Result) := Pointer(Lock);
end;

function X_D3DResource.GetEmuTexture8: IDirect3DTexture8;
begin
  Pointer(Result) := Pointer(Lock);
end;

function X_D3DResource.GetEmuVolumeTexture8: IDirect3DVolumeTexture8;
begin
  Pointer(Result) := Pointer(Lock);
end;

function X_D3DResource.GetEmuCubeTexture8: IDirect3DCubeTexture8;
begin
  Pointer(Result) := Pointer(Lock);
end;

function X_D3DResource.GetEmuSurface8: IDirect3DSurface8;
begin
  Pointer(Result) := Pointer(Lock);
end;

function X_D3DResource.GetEmuVertexBuffer8: IDirect3DVertexBuffer8;
begin
  Pointer(Result) := Pointer(Lock);
end;

function X_D3DResource.GetEmuIndexBuffer8: IDirect3DIndexBuffer8;
begin
  Pointer(Result) := Pointer(Lock);
end;

procedure X_D3DResource.SetEmuBaseTexture8(Value: IDirect3DBaseTexture8);
begin
  Pointer(Lock) := Pointer(Value);
end;

procedure X_D3DResource.SetEmuCubeTexture8(Value: IDirect3DCubeTexture8);
begin
  Pointer(Lock) := Pointer(Value);
end;

procedure X_D3DResource.SetEmuIndexBuffer8(Value: IDirect3DIndexBuffer8);
begin
  Pointer(Lock) := Pointer(Value);
end;

procedure X_D3DResource.SetEmuResource8(Value: IDirect3DResource8);
begin
  Pointer(Lock) := Pointer(Value);
end;

procedure X_D3DResource.SetEmuSurface8(Value: IDirect3DSurface8);
begin
  Pointer(Lock) := Pointer(Value);
end;

procedure X_D3DResource.SetEmuTexture8(Value: IDirect3DTexture8);
begin
  Pointer(Lock) := Pointer(Value);
end;

procedure X_D3DResource.SetEmuVertexBuffer8(Value: IDirect3DVertexBuffer8);
begin
  Pointer(Lock) := Pointer(Value);
end;

procedure X_D3DResource.SetEmuVolumeTexture8(Value: IDirect3DVolumeTexture8);
begin
  Pointer(Lock) := Pointer(Value);
end;

end.


