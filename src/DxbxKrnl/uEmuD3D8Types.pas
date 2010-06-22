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

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // DirectX
  Direct3D8, // D3DMULTISAMPLE_TYPE
  // Dxbx
  uTypes;

type
  // C++ nearly always works with pointer-to-interface, while Delphi's interfaces are already reference-types.
  // To simulate C++ behaviour, define an empty structure here, so we can declare a normal pointer to that as
  // a base for all the following interface-type declarations :
  CInterface = record end;
  // C++ does no reference-counting on interfaces, so translations to Delphi shouldn't do that either;
  // We realize this by using fake interface-types, all based on 'PInterface' which is just a plain pointer :
  PInterface = ^CInterface;

  LPUNKNOWN = type PInterface;

  // Direct3D interface types :

  XTL_PIDirect3D8 = type PInterface;

  XTL_PID3DXBuffer = type PInterface; // TODO -oDxbx: Review all GetBufferPointer calls (and prevent memory-leaks!)

  XTL_PIDirect3DDevice8 = type PInterface;
  XTL_PPIDirect3DDevice8 = ^XTL_PIDirect3DDevice8;

  XTL_PIDirect3DResource8 = type PInterface;
  XTL_PIDirect3DBaseTexture8 = type PInterface;
  XTL_PIDirect3DTexture8 = type PInterface;
  XTL_PIDirect3DVolumeTexture8 = type PInterface;
  XTL_PIDirect3DCubeTexture8 = type PInterface;
  XTL_PIDirect3DSurface8 = type PInterface;
  XTL_PIDirect3DVertexBuffer8 = type PInterface;
  XTL_PIDirect3DIndexBuffer8 = type PInterface;

  // DirectDraw interface types :

  XTL_PIDirectDraw7 = type PInterface;

  XTL_PIDirectDrawSurface7 = type PInterface;
  XTL_PIDirectDrawClipper = type PInterface;

  // DirectSound interface types :

  XTL_PIDirectSound8 = type PInterface;
  XTL_PIDirectSoundBuffer = type PInterface;
  XTL_PIDirectSoundBuffer8 = type PInterface;

  // DirectInput interface types :

  XTL_PIDirectInput8 = type PInterface;
  XTL_PIDirectInputDevice8 = type PInterface;

  // Long-Pointer aliases :

  XTL_LPD3DXBUFFER = XTL_PID3DXBuffer;
  XTL_PLPD3DXBUFFER = ^XTL_LPD3DXBUFFER;

  XTL_LPDIRECT3D8 = XTL_PIDirect3D8;
  XTL_LPDIRECTDRAW7 = XTL_PIDirectDraw7;
  XTL_LPDIRECT3DDEVICE8 = XTL_PIDirect3DDevice8;
  XTL_LPDIRECTDRAWSURFACE7 = XTL_PIDirectDrawSurface7;
  XTL_LPDIRECTDRAWCLIPPER = XTL_PIDirectDrawClipper;
  XTL_LPDIRECT3DVERTEXBUFFER8 = XTL_PIDirect3DVertexBuffer8;
  XTL_LPDIRECT3DINDEXBUFFER8 = XTL_PIDirect3DIndexBuffer8;

  XTL_LPDIRECTSOUND8 = XTL_PIDirectSound8;
  XTL_PLPDIRECTSOUND8 = ^XTL_LPDIRECTSOUND8;

  XTL_LPDIRECTSOUNDBUFFER8 = XTL_PIDirectSoundBuffer8;

  XTL_LPDIRECTINPUT8 = XTL_PIDirectInput8;
  XTL_LPDIRECTINPUTDEVICE8 = XTL_PIDirectInputDevice8;


// TODO -oCXBX: fill out these enumeration tables for convienance
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
type X_D3DFORMAT = DWORD;
type X_D3DBLENDOP = DWORD;
type X_D3DBLEND = DWORD;
type X_D3DCMPFUNC = DWORD;
type X_D3DFILLMODE = DWORD;
type X_D3DSHADEMODE = DWORD;
type X_D3DSTENCILOP = (
  X_D3DSTENCILOP_ZERO           = 0,
  X_D3DSTENCILOP_KEEP           = $1e00,
  X_D3DSTENCILOP_REPLACE        = $1e01,
  X_D3DSTENCILOP_INCRSAT        = $1e02,
  X_D3DSTENCILOP_DECRSAT        = $1e03,
  X_D3DSTENCILOP_INVERT         = $150a,
  X_D3DSTENCILOP_INCR           = $8507,
  X_D3DSTENCILOP_DECR           = $8508,
  X_D3DSTENCILOP_FORCE_DWORD    = $7fffffff
);
type X_D3DTEXTURESTAGESTATETYPE = DWORD;
type X_D3DCALLBACK = PVOID;

const
  X_D3DBLENDOP_ADD              = $8006;
  X_D3DBLENDOP_SUBTRACT         = $800a;
  X_D3DBLENDOP_REVSUBTRACT      = $800b;
  X_D3DBLENDOP_MIN              = $8007;
  X_D3DBLENDOP_MAX              = $8008;
  X_D3DBLENDOP_ADDSIGNED        = $f006;       // Xbox extension
  X_D3DBLENDOP_REVSUBTRACTSIGNED= $f005;       // Xbox extension

// Primitives supported by draw-primitive API
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
type _X_D3DPRIMITIVETYPE =
(
    X_D3DPT_NONE = 0, // Dxbx addition

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

// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
type _X_D3DRESOURCETYPE =
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

type _X_D3DDISPLAYMODE = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    Width: UINT;
    Height: UINT;
    RefreshRate: UINT;
    Flags: DWORD;
    Format: X_D3DFORMAT;
  end; // size = 20 (as in Cxbx)
  X_D3DDISPLAYMODE = _X_D3DDISPLAYMODE;
  PX_D3DDISPLAYMODE = ^X_D3DDISPLAYMODE;

type _X_D3DSURFACE_DESC = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    Format: X_D3DFORMAT;
    Type_: X_D3DRESOURCETYPE;
    Usage: DWORD;
    Size: UINT;
    MultiSampleType: D3DMULTISAMPLE_TYPE;
    Width: UINT;
    Height: UINT;
  end; // size = 28 (as in Cxbx)
  X_D3DSURFACE_DESC = _X_D3DSURFACE_DESC;
  PX_D3DSURFACE_DESC = ^X_D3DSURFACE_DESC;

type _X_D3DPRESENT_PARAMETERS = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    BackBufferWidth: UINT;
    BackBufferHeight: UINT;
    BackBufferFormat: X_D3DFORMAT;
    BackBufferCount: UINT;

    MultiSampleType: D3DMULTISAMPLE_TYPE;

    SwapEffect: D3DSWAPEFFECT;
    hDeviceWindow: HWND;
    Windowed: BOOL_;
    EnableAutoDepthStencil: BOOL;
    AutoDepthStencilFormat: X_D3DFORMAT;
    Flags: DWORD;

    FullScreen_RefreshRateInHz: UINT;
    FullScreen_PresentationInterval: UINT;
    // The Windows DirectX8 variant ends here
    // This check guarantees identical layout, compared to Direct3D8._D3DPRESENT_PARAMETERS_:
    // Assert(Integer(@(PX_D3DPRESENT_PARAMETERS(nil).BufferSurfaces[0])) = SizeOf(_D3DPRESENT_PARAMETERS_));
    BufferSurfaces: array [0..3-1] of XTL_PIDirect3DSurface8;
    DepthStencilSurface: XTL_PIDirect3DSurface8;
  end; // size = 68 (as in Cxbx)
  X_D3DPRESENT_PARAMETERS = _X_D3DPRESENT_PARAMETERS;
  PX_D3DPRESENT_PARAMETERS = ^X_D3DPRESENT_PARAMETERS;

type _X_D3DGAMMARAMP = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    Red: array [0..256-1] of BYTE;
    Green: array [0..256-1] of BYTE;
    Blue: array [0..256-1] of BYTE;
  end; // size = 768 (as in Cxbx)
  X_D3DGAMMARAMP = _X_D3DGAMMARAMP;
  PX_D3DGAMMARAMP = ^X_D3DGAMMARAMP;

const X_PIXELSHADER_FAKE_HANDLE = $DEADBEEF;

type X_D3DVertexShader = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    {union}case Integer of
     0: ( UnknownA: DWORD);
     1: ( Handle: DWORD;
    UnknownB: DWORD;
    Flags: DWORD;
    UnknownC: array [0..$59-1] of DWORD;
    ); // union
  end; // size = 368 (as in Cxbx)
  PX_D3DVertexShader = ^X_D3DVertexShader;

const
  X_D3DVS_XBOX_NR_ADDRESS_SLOTS = 136;

type _X_D3DPIXELSHADERDEF = record // <- blueshogun 10/1/07
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    PSAlphaInputs: array [0..8-1] of DWORD;  // Alpha inputs for each stage
    PSFinalCombinerInputsABCD: DWORD;        // Final combiner inputs
    PSFinalCombinerInputsEFG: DWORD;         // Final combiner inputs (continued)
    PSConstant0: array [0..8-1] of DWORD;    // C0 for each stage
    PSConstant1: array [0..8-1] of DWORD;    // C1 for each stage
    PSAlphaOutputs: array [0..8-1] of DWORD; // Alpha output for each stage
    PSRGBInputs: array [0..8-1] of DWORD;    // RGB inputs for each stage
    PSCompareMode: DWORD;                    // Compare modes for clipplane texture mode
    PSFinalCombinerConstant0: DWORD;         // C0 in final combiner
    PSFinalCombinerConstant1: DWORD;         // C1 in final combiner
    PSRGBOutputs: array [0..8-1] of DWORD;   // Stage 0 RGB outputs
    PSCombinerCount: DWORD;                  // Active combiner count (Stages 0-7)
    PSTextureModes: DWORD;                   // Texture addressing modes
    PSDotMapping: DWORD;                     // Input mapping for dot product modes
    PSInputTexture: DWORD;                   // Texture source for some texture modes
    PSC0Mapping: DWORD;                      // Mapping of c0 regs to D3D constants
    PSC1Mapping: DWORD;                      // Mapping of c1 regs to D3D constants
    PSFinalCombinerConstants: DWORD;         // Final combiner constant mapping
  end; // size = 240 (as in Cxbx)
  X_D3DPIXELSHADERDEF = _X_D3DPIXELSHADERDEF;
  PX_D3DPIXELSHADERDEF = ^X_D3DPIXELSHADERDEF;

type _STREAM_DYNAMIC_PATCH_ = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    NeedPatch: BOOL_;       // This is to know whether is data which must be patched
    ConvertedStride: DWORD;
    NbrTypes: DWORD;        // Number of the stream data types
    pTypes: PUINTs;         // The stream data types (xbox)
  end; // size = 16 (as in Cxbx)
  STREAM_DYNAMIC_PATCH = _STREAM_DYNAMIC_PATCH_;
  PSTREAM_DYNAMIC_PATCH = ^STREAM_DYNAMIC_PATCH;

  TSTREAM_DYNAMIC_PATCHArray = array [0..MaxInt div SizeOf(STREAM_DYNAMIC_PATCH) - 1] of STREAM_DYNAMIC_PATCH;
  PSTREAM_DYNAMIC_PATCHs = ^TSTREAM_DYNAMIC_PATCHArray;

type _VERTEX_DYNAMIC_PATCH_ = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    NbrStreams: UINT; // The number of streams the vertex shader uses
    pStreamPatches: PSTREAM_DYNAMIC_PATCHs;
  end; // size = 8 (as in Cxbx)
  VERTEX_DYNAMIC_PATCH = _VERTEX_DYNAMIC_PATCH_;
  PVERTEX_DYNAMIC_PATCH = ^VERTEX_DYNAMIC_PATCH;

type _VERTEX_SHADER = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    Handle: DWORD;
    // These are the parameters given by the XBE,
    // we save them to be be able to return them when necassary.
    Size: UINT;
    pDeclaration: PDWORD;
    DeclarationSize: DWORD;
    pFunction: PDWORD;
    FunctionSize: DWORD;
    Type_: DWORD;
    Status: DWORD;
    // Needed for dynamic stream patching
    VertexDynamicPatch: VERTEX_DYNAMIC_PATCH;
  end; // size = 40 (as in Cxbx)
  VERTEX_SHADER = _VERTEX_SHADER;
  PVERTEX_SHADER = ^VERTEX_SHADER;

type X_D3DResource = object
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  public
    Common: DWORD;
    Data: DWORD;
    Emu: record {union}case Integer of
    0: (Lock: DWORD);
    1: ({Emu}Resource8: XTL_PIDirect3DResource8);
    2: ({Emu}BaseTexture8: XTL_PIDirect3DBaseTexture8);
    3: ({Emu}Texture8: XTL_PIDirect3DTexture8);
    4: ({Emu}VolumeTexture8: XTL_PIDirect3DVolumeTexture8);
    5: ({Emu}CubeTexture8: XTL_PIDirect3DCubeTexture8);
    6: ({Emu}Surface8: XTL_PIDirect3DSurface8);
    7: ({Emu}VertexBuffer8: XTL_PIDirect3DVertexBuffer8);
    8: ({Emu}IndexBuffer8: XTL_PIDirect3DIndexBuffer8);
    end; // union
  end; // size = 12 (as in Cxbx)
  PX_D3DResource = ^X_D3DResource;



// d3d resource "common" masks
const X_D3DCOMMON_REFCOUNT_MASK      = $0000FFFF;
const X_D3DCOMMON_TYPE_MASK          = $00070000;
const X_D3DCOMMON_TYPE_SHIFT         = 16;
const X_D3DCOMMON_TYPE_VERTEXBUFFER  = $00000000;
const X_D3DCOMMON_TYPE_INDEXBUFFER   = $00010000;
const X_D3DCOMMON_TYPE_PUSHBUFFER    = $00020000;
const X_D3DCOMMON_TYPE_PALETTE       = $00030000;
const X_D3DCOMMON_TYPE_TEXTURE       = $00040000;
const X_D3DCOMMON_TYPE_SURFACE       = $00050000;
const X_D3DCOMMON_TYPE_FIXUP         = $00060000;
const X_D3DCOMMON_INTREFCOUNT_MASK   = $00780000;
const X_D3DCOMMON_INTREFCOUNT_SHIFT  = 19;
const X_D3DCOMMON_D3DCREATED         = $01000000;
const X_D3DCOMMON_ISLOCKED           = $02000010; // Surface is currently locked (potential unswizzle candidate)
const X_D3DCOMMON_UNUSED_MASK        = $FE000000;
const X_D3DCOMMON_UNUSED_SHIFT       = 25;

// special resource data flags (must set _SPECIAL *AND* specific flag(s))
const X_D3DRESOURCE_DATA_FLAG_SPECIAL = $FFFF0000;
const X_D3DRESOURCE_DATA_FLAG_SURFACE = $00000001; // Backbuffer surface, etc
const X_D3DRESOURCE_DATA_FLAG_YUVSURF = $00000002; // YUV memory surface
const X_D3DRESOURCE_DATA_FLAG_D3DREND = $00000004; // D3D Render Target
const X_D3DRESOURCE_DATA_FLAG_D3DSTEN = $00000008; // D3D Stencil Surface
const X_D3DRESOURCE_DATA_FLAG_TEXCLON = $00000010; // HACK: Cloned resource

function IsSpecialResource(x: DWORD): Boolean; // forward

// special resource lock flags
const X_D3DRESOURCE_LOCK_FLAG_NOSIZE  = $EFFFFFFF;

// Lock flags
const X_D3DLOCK_NOFLUSH  = $00000010; // Xbox extension
const X_D3DLOCK_NOOVERWRITE = $00000020;
const X_D3DLOCK_TILED = $00000040; // Xbox extension
const X_D3DLOCK_READONLY = $00000080;
const X_D3DLOCK_ALL_SUPPORTED = X_D3DLOCK_TILED or X_D3DLOCK_READONLY;


type X_D3DVertexBuffer = object(X_D3DResource)
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  public
  end; // size = 12 (as in Cxbx)
  PX_D3DVertexBuffer = ^X_D3DVertexBuffer;
  PPX_D3DVertexBuffer = ^PX_D3DVertexBuffer;

type X_D3DIndexBuffer = object(X_D3DResource)
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  public
  end; // size = 12 (as in Cxbx)
  PX_D3DIndexBuffer = ^X_D3DIndexBuffer;
  PPX_D3DIndexBuffer = ^PX_D3DIndexBuffer;

type X_D3DPushBuffer = object(X_D3DResource)
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  public
    Size: ULONG;
    AllocationSize: ULONG;
  end; // size = 20 (as in Cxbx)
  PX_D3DPushBuffer = ^X_D3DPushBuffer;
  PPX_D3DPushBuffer = ^PX_D3DPushBuffer;

type X_D3DFixup = object(X_D3DResource)
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  public
    Run: ULONG;
    Next: ULONG;
    Size: ULONG;
  end; // size = 24 (as in Cxbx)
  PX_D3DFixup = ^X_D3DFixup;
  PPX_D3DFixup = ^PX_D3DFixup;

type X_D3DPalette = object(X_D3DResource)
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  end; // size = 12 (as in Cxbx)
  PX_D3DPalette = ^X_D3DPalette;
  PPX_D3DPalette = ^PX_D3DPalette;

type _X_D3DPALETTESIZE =
(
    D3DPALETTE_256              = 0,
    D3DPALETTE_128              = 1,
    D3DPALETTE_64               = 2,
    D3DPALETTE_32               = 3,
    D3DPALETTE_MAX              = 4,
    D3DPALETTE_FORCE_DWORD      = $7FFFFFFF
);
X_D3DPALETTESIZE = _X_D3DPALETTESIZE;

type X_D3DPixelContainer = object(X_D3DResource)
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  public
    Format: X_D3DFORMAT; // Format information about the texture.
    Size: DWORD; // Size of a non power-of-2 texture, must be zero otherwise
  end; // size = 20 (as in Cxbx)
  PX_D3DPixelContainer = ^X_D3DPixelContainer;
  PPX_D3DPixelContainer = ^PX_D3DPixelContainer;

// pixel container "format" masks
const X_D3DFORMAT_RESERVED1_MASK      = $00000003;      // Must be zero
const X_D3DFORMAT_DMACHANNEL_MASK     = $00000003;
const X_D3DFORMAT_DMACHANNEL_A        = $00000001;      // DMA channel A - the default for all system memory
const X_D3DFORMAT_DMACHANNEL_B        = $00000002;      // DMA channel B - unused
const X_D3DFORMAT_CUBEMAP             = $00000004;      // Set if the texture if a cube map
const X_D3DFORMAT_BORDERSOURCE_COLOR  = $00000008;
const X_D3DFORMAT_DIMENSION_MASK      = $000000F0;      // # of dimensions
const X_D3DFORMAT_DIMENSION_SHIFT     = 4;
const X_D3DFORMAT_FORMAT_MASK         = $0000FF00;
const X_D3DFORMAT_FORMAT_SHIFT        = 8;
const X_D3DFORMAT_MIPMAP_MASK         = $000F0000;
const X_D3DFORMAT_MIPMAP_SHIFT        = 16;
const X_D3DFORMAT_USIZE_MASK          = $00F00000;      // Log 2 of the U size of the base texture
const X_D3DFORMAT_USIZE_SHIFT         = 20;
const X_D3DFORMAT_VSIZE_MASK          = $0F000000;      // Log 2 of the V size of the base texture
const X_D3DFORMAT_VSIZE_SHIFT         = 24;
const X_D3DFORMAT_PSIZE_MASK          = $F0000000;      // Log 2 of the P size of the base texture
const X_D3DFORMAT_PSIZE_SHIFT         = 28;

// pixel container "size" masks
// The layout of the size field, used for non swizzled or compressed textures.
//
// The Size field of a container will be zero if the texture is swizzled or compressed.
// It is guarenteed to be non-zero otherwise because either the height/width will be
// greater than one or the pitch adjust will be nonzero because the minimum texture
// pitch is 8 bytes.
const X_D3DSIZE_WIDTH_MASK   = $00000FFF;   // Width of the texture - 1, in texels
const X_D3DSIZE_HEIGHT_MASK  = $00FFF000;   // Height of the texture - 1, in texels
const X_D3DSIZE_HEIGHT_SHIFT = 12;
const X_D3DSIZE_PITCH_MASK   = $FF000000;   // Pitch / 64 - 1
const X_D3DSIZE_PITCH_SHIFT  = 24;

type X_D3DBaseTexture = object(X_D3DPixelContainer)
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  end; // size = 20 (as in Cxbx)
  PX_D3DBaseTexture = ^X_D3DBaseTexture;
  PPX_D3DBaseTexture = ^PX_D3DBaseTexture;

type X_D3DTexture = object(X_D3DBaseTexture)
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  end; // size = 20 (as in Cxbx)
  PX_D3DTexture = ^X_D3DTexture;
  PPX_D3DTexture = ^PX_D3DTexture;

type X_D3DVolumeTexture = object(X_D3DBaseTexture)
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  end; // size = 20 (as in Cxbx)
  PX_D3DVolumeTexture = ^X_D3DVolumeTexture;
  PPX_D3DVolumeTexture = ^PX_D3DVolumeTexture;

type X_D3DCubeTexture = object(X_D3DBaseTexture)
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  end; // size = 20 (as in Cxbx)
  PX_D3DCubeTexture = ^X_D3DCubeTexture;
  PPX_D3DCubeTexture = ^PX_D3DCubeTexture;

type X_D3DSurface = object(X_D3DPixelContainer)
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  end; // size = 20 (as in Cxbx)
  PX_D3DSurface = ^X_D3DSurface;
  PPX_D3DSurface = ^PX_D3DSurface;

type X_D3DTILE = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    Flags: DWORD;
    pMemory: PVOID;
    Size: DWORD;
    Pitch: DWORD;
    ZStartTag: DWORD;
    ZOffset: DWORD;
  end; // size = 24 (as in Cxbx)
  PX_D3DTILE = ^X_D3DTILE;

type X_D3DCALLBACKTYPE = ( // blueshogun96 10/1/07
    X_D3DCALLBACK_READ = 1,
    X_D3DCALLBACK_WRITE = 2
  );

type X_D3DFIELDTYPE = (
    X_D3DFIELD_ODD = 1,
    X_D3DFIELD_EVEN = 2,
    X_D3DFIELD_PROGRESSIVE = 3,
    X_D3DFIELD_FORCE_DWORD = $7FFFFFFF
  );

type _X_D3DFIELD_STATUS = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    Field: X_D3DFIELDTYPE;
    VBlankCount: UINT;
  end; // size = 8 (as in Cxbx)
  X_D3DFIELD_STATUS = _X_D3DFIELD_STATUS;
  PX_D3DFIELD_STATUS= ^X_D3DFIELD_STATUS;

type _D3DVBLANKDATA = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    VBlank: DWORD;
    Swap: DWORD;
    Flags: DWORD;
  end; // size = 12 (as in Cxbx)
  D3DVBLANKDATA = _D3DVBLANKDATA;
  PD3DVBLANKDATA = ^D3DVBLANKDATA;

type _D3DSWAPDATA = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    Swap: DWORD;
    SwapVBlank: DWORD;
    MissedVBlanks: DWORD;
    TimeUntilSwapVBlank: DWORD;
    TimeBetweenSwapVBlanks: DWORD;
  end; // size = 20 (as in Cxbx)
  D3DSWAPDATA = _D3DSWAPDATA;
  PD3DSWAPDATA = ^D3DSWAPDATA;

// D3DVBLANKCALLBACK
type D3DVBLANKCALLBACK = procedure (const pData: PD3DVBLANKDATA); cdecl;

// D3DSWAPCALLBACK
type D3DSWAPCALLBACK = procedure (const pData: PD3DSWAPDATA); cdecl;

const X_D3DRS_DEFERRED_START_3925 = 72;
const X_D3DRS_DEFERRED_START_4361 = 82;
const X_D3DRS_DEFERRED_START_4432 = 83;
const X_D3DRS_DEFERRED_START_5933 = 92;

const X_D3DRS_DEFERRED_SIZE_3925 = 35; // ??
const X_D3DRS_DEFERRED_SIZE_4361 = 35;
const X_D3DRS_DEFERRED_SIZE_4432 = 44; // ?? (4627 is 44)
const X_D3DRS_DEFERRED_SIZE_5933 = 44;

const X_D3DRS_DEFERRED_FOGENABLE = 0;
const X_D3DRS_DEFERRED_FOGTABLEMODE = 1;
const X_D3DRS_DEFERRED_FOGSTART = 2;
const X_D3DRS_DEFERRED_FOGEND = 3;
const X_D3DRS_DEFERRED_FOGDENSITY = 4;
const X_D3DRS_DEFERRED_RANGEFOGENABLE = 5;
const X_D3DRS_DEFERRED_WRAP0 = 6;
const X_D3DRS_DEFERRED_WRAP1 = 7;
const X_D3DRS_DEFERRED_LIGHTING = 10;
const X_D3DRS_DEFERRED_SPECULARENABLE = 11;
const X_D3DRS_DEFERRED_COLORVERTEX = 13;
const X_D3DRS_DEFERRED_DIFFUSEMATERIALSOURCE = 19;
const X_D3DRS_DEFERRED_AMBIENTMATERIALSOURCE = 20;
const X_D3DRS_DEFERRED_EMISSIVEMATERIALSOURCE = 21;
const X_D3DRS_DEFERRED_AMBIENT = 23;
const X_D3DRS_DEFERRED_POINTSIZE = 24;
const X_D3DRS_DEFERRED_POINTSIZE_MIN = 25;
const X_D3DRS_DEFERRED_POINTSPRITEENABLE = 26;
const X_D3DRS_DEFERRED_POINTSCALEENABLE = 27;
const X_D3DRS_DEFERRED_POINTSCALE_A = 28;
const X_D3DRS_DEFERRED_POINTSCALE_B = 29;
const X_D3DRS_DEFERRED_POINTSCALE_C = 30;
const X_D3DRS_DEFERRED_POINTSIZE_MAX = 31;
const X_D3DRS_DEFERRED_PATCHSEGMENTS = 33;

// deferred render state "unknown" flag
const X_D3DRS_UNK =  $7fffffff;

const X_D3DWRAP_U = $00000010;
const X_D3DWRAP_V = $00001000;
const X_D3DWRAP_W = $00100000;

const X_D3DTSS_ADDRESSU = 0;
const X_D3DTSS_ADDRESSV = 1;
const X_D3DTSS_ADDRESSW = 2;
const X_D3DTSS_MAGFILTER = 3;
const X_D3DTSS_MINFILTER = 4;
const X_D3DTSS_MIPFILTER = 5;
const X_D3DTSS_MIPMAPLODBIAS = 6;
const X_D3DTSS_MAXMIPLEVEL = 7;
const X_D3DTSS_MAXANISOTROPY = 8;
const X_D3DTSS_COLOROP = 12;
const X_D3DTSS_COLORARG0 = 13;
const X_D3DTSS_COLORARG1 = 14;
const X_D3DTSS_COLORARG2 = 15;
const X_D3DTSS_ALPHAOP = 16;
const X_D3DTSS_ALPHAARG0 = 17;
const X_D3DTSS_ALPHAARG1 = 18;
const X_D3DTSS_ALPHAARG2 = 19;
const X_D3DTSS_RESULTARG = 20;
const X_D3DTSS_TEXTURETRANSFORMFLAGS = 21;
const X_D3DTSS_BUMPENVMAT00 = 22;
const X_D3DTSS_BUMPENVMAT01 = 23;
const X_D3DTSS_BUMPENVMAT11 = 24;
const X_D3DTSS_BUMPENVMAT10 = 25;
const X_D3DTSS_BUMPENVLSCALE = 26;
//const X_D3DTSS_BORDERCOLOR = 29;

const X_D3DTOP_BLENDCURRENTALPHA = 13;
const X_D3DTOP_BLENDTEXTUREALPHA = 14;
const X_D3DTOP_BLENDFACTORALPHA = 15;
const X_D3DTOP_DOTPRODUCT3 = 22;

// deferred texture stage state "unknown" flag
const X_D3DTSS_UNK = $7fffffff;

type X_VERTEXSHADERCONSTANTMODE = DWORD;

const X_VSCM_96               = $00; // Enables constants 0..95
const X_VSCM_192              = $01; // Enables constants -96..-1 on top of 0..95
const X_VSCM_192FIXEDPIPELINE = $02; // Unsupported?
const X_VSCM_NONERESERVED     = $10; // Do not reserve constant -38 and -37

const X_VSCM_RESERVED_CONSTANT1 = -38; // Becomes 58 after correction
const X_VSCM_RESERVED_CONSTANT2 = -37; // Becomes 59 after correction

const X_VSCM_CORRECTION = 96; // Add 96 to arrive at the range 0..191 (instead of 96..95)

// Vertex shader types
const X_VST_NORMAL = 1;
const X_VST_READWRITE = 2;
const X_VST_STATE = 3;

// ******************************************************************
// * X_VERTEXSHADERINPUT
// ******************************************************************
type _X_VERTEXSHADERINPUT = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    IndexOfStream: DWORD;
    Offset: DWORD;
    Format: DWORD;
    TesselationType: BYTE;
    TesselationSource: BYTE;
  end; // size = 16 (as in Cxbx)
  X_VERTEXSHADERINPUT = _X_VERTEXSHADERINPUT;

// ******************************************************************
// * X_VERTEXATTRIBUTEFORMAT
// ******************************************************************
type _X_VERTEXATTRIBUTEFORMAT = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    pVertexShaderInput: array [0..15] of X_VERTEXSHADERINPUT;
  end; // size = 256 (as in Cxbx)
  X_VERTEXATTRIBUTEFORMAT = _X_VERTEXATTRIBUTEFORMAT;
  PX_VERTEXATTRIBUTEFORMAT = ^X_VERTEXATTRIBUTEFORMAT;

// ******************************************************************
// * X_STREAMINPUT
// ******************************************************************
type _X_STREAMINPUT = record
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    VertexBuffer: PX_D3DVertexBuffer;
    Stride: UINT;
    Offset: UINT;
  end; // size = 12 (as in Cxbx)
  X_STREAMINPUT = _X_STREAMINPUT;
  PX_STREAMINPUT = ^X_STREAMINPUT;
  PPX_STREAMINPUT = ^PX_STREAMINPUT;

const
  // Xbox D3DFORMAT types :
  // See http://wiki.beyondunreal.com/Legacy:Texture_Format

  // Swizzled Formats

  X_D3DFMT_L8 = $00;
  X_D3DFMT_AL8 = $01;
  X_D3DFMT_A1R5G5B5 = $02;
  X_D3DFMT_X1R5G5B5 = $03;
  X_D3DFMT_A4R4G4B4 = $04;
  X_D3DFMT_R5G6B5 = $05;
  X_D3DFMT_A8R8G8B8 = $06;
  X_D3DFMT_X8R8G8B8 = $07;
  X_D3DFMT_X8L8V8U8 = $07; // Alias
  X_D3DFMT_P8 = $0B; // 8-bit Palletized
  X_D3DFMT_A8 = $19;
  X_D3DFMT_A8L8 = $1A;
  X_D3DFMT_R6G5B5 = $27;
  X_D3DFMT_L6V5U5 = $27; // Alias
  X_D3DFMT_G8B8 = $28;
  X_D3DFMT_V8U8 = $28; // Alias
  X_D3DFMT_R8B8 = $29;
  X_D3DFMT_D24S8 = $2A;
  X_D3DFMT_F24S8 = $2B;
  X_D3DFMT_D16 = $2C;
  X_D3DFMT_D16_LOCKABLE = $2C; // Alias
  X_D3DFMT_F16 = $2D;
  X_D3DFMT_L16 = $32;
  X_D3DFMT_V16U16 = $33;
  X_D3DFMT_R5G5B5A1 = $38;
  X_D3DFMT_R4G4B4A4 = $39;
  X_D3DFMT_A8B8G8R8 = $3A;
  X_D3DFMT_Q8W8V8U8 = $3A; // Alias
  X_D3DFMT_B8G8R8A8 = $3B;
  X_D3DFMT_R8G8B8A8 = $3C;

  // YUV Formats

  X_D3DFMT_YUY2 = $24;
  X_D3DFMT_UYVY = $25;

  // Compressed Formats

  X_D3DFMT_DXT1 = $0C; // opaque/one-bit alpha
  X_D3DFMT_DXT2 = $0E;
  X_D3DFMT_DXT3 = $0E; // linear alpha
  X_D3DFMT_DXT4 = $0F;
  X_D3DFMT_DXT5 = $0F; // interpolated alpha

  // Linear Formats

  X_D3DFMT_LIN_A1R5G5B5 = $10;
  X_D3DFMT_LIN_R5G6B5 = $11;
  X_D3DFMT_LIN_A8R8G8B8 = $12;
  X_D3DFMT_LIN_L8 = $13;
  X_D3DFMT_LIN_R8B8 = $16;
  X_D3DFMT_LIN_G8B8 = $17;
  X_D3DFMT_LIN_V8U8 = $17; // Alias
  X_D3DFMT_LIN_AL8 = $1B;
  X_D3DFMT_LIN_X1R5G5B5 = $1C;
  X_D3DFMT_LIN_A4R4G4B4 = $1D;
  X_D3DFMT_LIN_X8R8G8B8 = $1E;
  X_D3DFMT_LIN_X8L8V8U8 = $1E; // Alias
  X_D3DFMT_LIN_A8 = $1F;
  X_D3DFMT_LIN_A8L8 = $20;
  X_D3DFMT_LIN_D24S8 = $2E;
  X_D3DFMT_LIN_F24S8 = $2F;
  X_D3DFMT_LIN_D16 = $30;
  X_D3DFMT_LIN_F16 = $31;
  X_D3DFMT_LIN_L16 = $35;
  X_D3DFMT_LIN_V16U16 = $36;
  X_D3DFMT_LIN_R6G5B5 = $37;
  X_D3DFMT_LIN_L6V5U5 = $37; // Alias
  X_D3DFMT_LIN_R5G5B5A1 = $3D;
  X_D3DFMT_LIN_R4G4B4A4 = $3E;
  X_D3DFMT_LIN_A8B8G8R8 = $3F;
  X_D3DFMT_LIN_B8G8R8A8 = $40;
  X_D3DFMT_LIN_R8G8B8A8 = $41;

  X_D3DFMT_VERTEXDATA = $64;

  X_D3DFMT_INDEX16 = DWord(D3DFMT_INDEX16); // Dxbx addition : Not an Xbox format, used internally

var
  // cached active texture
  EmuD3DActiveTexture: array [0..4 - 1] of PX_D3DResource; // = {0,0,0,0};

const
  X_D3DCLEAR_ZBUFFER = $00000001;
  X_D3DCLEAR_STENCIL = $00000002;
  X_D3DCLEAR_TARGET  = $000000f0;

  X_D3DCLEAR_ALL_SUPPORTED = X_D3DCLEAR_ZBUFFER or X_D3DCLEAR_STENCIL or X_D3DCLEAR_TARGET;

const
  // D3DRS_CULLMODE value
  X_D3DCULL_NONE = 0; // No culling
  X_D3DCULL_CW   = $900; // Clockwise culling
  X_D3DCULL_CCW  = $901; // Cull counter clockwise triangles

implementation

function IsSpecialResource(x: DWORD): Boolean;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
 Result := (x and X_D3DRESOURCE_DATA_FLAG_SPECIAL) = X_D3DRESOURCE_DATA_FLAG_SPECIAL;
end;

end.


