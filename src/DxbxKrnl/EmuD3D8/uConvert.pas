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

unit uConvert;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows
  , SysUtils
  // DirectX
  , Direct3D8 // was Direct3D9 ?!
  // Dxbx
  , uEmuD3D8Types
  , uConsts
  , uTypes
  , uLog
  , uXboxLibraryUtils
  , uDxbxKrnlUtils
  , uEmu;

function EmuXBFormatIsSwizzled(Format: X_D3DFORMAT; pBPP: PDWord): BOOL_;
function EmuXBFormatIsLinear(Format: X_D3DFORMAT): BOOL_;

function EmuXB2PC_D3DFormat(aFormat: X_D3DFORMAT): D3DFORMAT; inline;
function EmuPC2XB_D3DFormat(aFormat: D3DFORMAT): X_D3DFORMAT; inline;

function EmuXB2PC_D3DLock(Flags: DWORD): DWORD; inline;
function EmuXB2PC_D3DMultiSampleFormat(aType: DWORD): D3DMULTISAMPLE_TYPE;

function EmuXB2PC_D3DTS(State: D3DTRANSFORMSTATETYPE): D3DTRANSFORMSTATETYPE; inline;
function EmuXB2PC_D3DBLENDOP(Value: X_D3DBLENDOP): D3DBLENDOP; inline;
function EmuXB2PC_D3DBLEND(Value: X_D3DBLEND): D3DBLEND; inline;
function EmuXB2PC_D3DCMPFUNC(Value: X_D3DCMPFUNC): D3DCMPFUNC; inline;
function EmuXB2PC_D3DFILLMODE(Value: X_D3DFILLMODE): D3DFILLMODE; inline;
function EmuXB2PC_D3DSTENCILOP(Value: X_D3DSTENCILOP): D3DSTENCILOP; inline;
function EmuXB2PC_D3DSHADEMODE(Value: X_D3DSHADEMODE): D3DSHADEMODE; inline;

function EmuD3DVertex2PrimitiveCount(PrimitiveType: X_D3DPRIMITIVETYPE; VertexCount: int): INT; inline;
function EmuD3DPrimitive2VertexCount(PrimitiveType: X_D3DPRIMITIVETYPE; PrimitiveCount: int): int; inline;
function EmuPrimitiveType(PrimitiveType: X_D3DPRIMITIVETYPE): D3DPRIMITIVETYPE; inline;


// simple render state encoding lookup table
const X_D3DRSSE_UNK = $7fffffff;

// Code translated from Convert.cpp :

const
// lookup table for converting vertex count to primitive count
EmuD3DVertexToPrimitive: array [0..11-1] of array [0..2-1] of {U}INT = (
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    (0, 0), // NULL
    (1, 0), // X_D3DPT_POINTLIST
    (2, 0), // X_D3DPT_LINELIST
    (1, 1), // X_D3DPT_LINELOOP
    (1, 1), // X_D3DPT_LINESTRIP
    (3, 0), // X_D3DPT_TRIANGLELIST
    (1, 2), // X_D3DPT_TRIANGLESTRIP
    (1, 2), // X_D3DPT_TRIANGLEFAN
    (4, 0), // X_D3DPT_QUADLIST
    (2, 2), // X_D3DPT_QUADSTRIP
    (1, 0)  // X_D3DPT_POLYGON
);

const
// conversion table for xbox->pc primitive types
EmuPrimitiveTypeLookup: array [0..11] of D3DPRIMITIVETYPE = (
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    D3DPRIMITIVETYPE(0),   // NULL                 = 0
    D3DPT_POINTLIST,       // D3DPT_POINTLIST      = 1,
    D3DPT_LINELIST,        // D3DPT_LINELIST       = 2,
    D3DPT_LINESTRIP,       // D3DPT_LINELOOP       = 3,  Xbox
    D3DPT_LINESTRIP,       // D3DPT_LINESTRIP      = 4,
    D3DPT_TRIANGLELIST,    // D3DPT_TRIANGLELIST   = 5,
    D3DPT_TRIANGLESTRIP,   // D3DPT_TRIANGLESTRIP  = 6,
    D3DPT_TRIANGLEFAN,     // D3DPT_TRIANGLEFAN    = 7,
    D3DPT_TRIANGLELIST,    // D3DPT_QUADLIST       = 8,  Xbox
    D3DPT_TRIANGLESTRIP,   // D3DPT_QUADSTRIP      = 9,  Xbox
    D3DPT_TRIANGLEFAN,     // D3DPT_POLYGON        = 10, Xbox
    D3DPRIMITIVETYPE(11)   // D3DPT_MAX            = 11,
);

// render state conversion table
CONST {XTL.}EmuD3DRenderStateSimpleEncoded: array [0..174-1] of DWORD = (
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
    // WARNING: This lookup table strongly binds us to an SDK with these
    // specific #define values for D3DRS_*. Make VERY sure that you have
    // the correct lookup values;
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 0
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 2
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 4
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 6
    X_D3DRSSE_UNK,  $0004037c,     // 8  - , D3DRS_SHADEMODE
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 10
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 12
    $0004035c,     $00040300,     // 14 - D3DRS_ZWRITEENABLE, D3DRS_ALPHATESTENABLE
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 16
    X_D3DRSSE_UNK,  $00040344,     // 18 - , D3DRS_SRCBLEND
    $00040348,     X_D3DRSSE_UNK,  // 20 - D3DRS_DESTBLEND
    X_D3DRSSE_UNK,  $00040354,     // 22 - , D3DRS_ZFUNC
    $00040340,     $0004033c,     // 24 - D3DRS_ALPHAREF, D3DRS_ALPHAFUNC
    $00040310,     $00040304,     // 26 - D3DRS_DITHERENABLE, D3DRS_ALPHABLENDENABLE
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 28
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 30
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 32
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 34
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 36
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 38
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 40
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 42
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 44
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 46
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 48
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 50
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 52
    $00040374,      $00040378,      // 54 - D3DRS_STENCILZFAIL, D3DRS_STENCILPASS
    $00040364,      $00040368,      // 56 - D3DRS_STENCILFUNC, D3DRS_STENCILREF
    $0004036c,      $00040360,      // 58 - D3DRS_STENCILMASK, D3DRS_STENCILWRITEMASK
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 60
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 62
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 64
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 66
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 68
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 70
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 72
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 74
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 76
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 78
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 80
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 82
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 84
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 86
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 88
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 90
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 92
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 94
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 96
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 98
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 100
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 102
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 104
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 106
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 108
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 110
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 112
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 114
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 116
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 118
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 120
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 122
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 124
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 126
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 128
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 130
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 132
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 134
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 136
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 138
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 140
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 142
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 144
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 146
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 148
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 150
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 152
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 154
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 156
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 158
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 160
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 162
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 164
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 166
    $00040358,     X_D3DRSSE_UNK,  // 168 - D3DRS_COLORWRITEENABLE
    X_D3DRSSE_UNK,  $00040350,     // 170
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK  // 172
  );

implementation

// is this format swizzled, and if so - how many BPP?
function EmuXBFormatIsSwizzled(Format: X_D3DFORMAT; pBPP: PDWord): BOOL_;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := TRUE;
  case Format of
    X_D3DFMT_L8, // 0x00
    X_D3DFMT_AL8, // 0x01
    X_D3DFMT_P8, // 0x0B
    X_D3DFMT_A8: // 0x19
      pBPP^ := 1;

    X_D3DFMT_A1R5G5B5, // 0x02
    X_D3DFMT_X1R5G5B5, // 0x03
    X_D3DFMT_A4R4G4B4, // 0x04
    X_D3DFMT_R5G6B5, // 0x05
    X_D3DFMT_A8L8, // 0x1A
    X_D3DFMT_L6V5U5, // Added by Dxbx
    X_D3DFMT_V8U8, // Added by Dxbx
    X_D3DFMT_D16: // Added by Dxbx
      pBPP^ := 2;

    X_D3DFMT_A8R8G8B8, // 0x06
    X_D3DFMT_X8R8G8B8, // 0x07
    X_D3DFMT_YUY2, // Added by Dxbx
    X_D3DFMT_D24S8, // Added by Dxbx
    X_D3DFMT_F24S8, // Added by Dxbx
    X_D3DFMT_V16U16, // Added by Dxbx
    X_D3DFMT_A8B8G8R8: // Added by Dxbx
      pBPP^ := 4;

  // TODO -oDXBX: Where do we put X_D3DFMT_UYVY ?

  else
    Result := FALSE;
  end;
end;

// is this format linear?
function EmuXBFormatIsLinear(Format: X_D3DFORMAT): BOOL_;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  case (Format) of
    X_D3DFMT_LIN_A1R5G5B5, // 0x10
    X_D3DFMT_LIN_R5G6B5,   // 0x11
    X_D3DFMT_LIN_A8R8G8B8, // 0x12
    X_D3DFMT_LIN_L8, // Added by Dxbx
    X_D3DFMT_LIN_R8B8,     // 0x16
    X_D3DFMT_LIN_G8B8,     // 0x17
    X_D3DFMT_LIN_A4R4G4B4, // 0x1D
    X_D3DFMT_LIN_X8R8G8B8, // 0x1E
    X_D3DFMT_LIN_D24S8,    // 0x2E
    X_D3DFMT_LIN_D16,      // 0x30
    X_D3DFMT_LIN_A8B8G8R8: // 0x3F
      Result := TRUE;
  else
    Result := FALSE;
  end;
end;

// convert from xbox to pc color formats
function EmuXB2PC_D3DFormat(aFormat: X_D3DFORMAT): D3DFORMAT; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  case aFormat of
    X_D3DFMT_L8: // Swizzled
      Result := D3DFMT_L8;

    X_D3DFMT_AL8: // Swizzled    // Cxbx NOTE: Hack: Alpha ignored, basically
    begin
      EmuWarning('X_D3DFMT_AL8 -> D3DFMT_L8');
      Result := D3DFMT_L8;
    end;

    X_D3DFMT_LIN_A1R5G5B5, // Linear
    X_D3DFMT_A1R5G5B5: // Swizzled
      Result := D3DFMT_A1R5G5B5;

    X_D3DFMT_X1R5G5B5: // Swizzled
      Result := D3DFMT_X1R5G5B5;

    X_D3DFMT_A8L8: // Swizzled
    begin
      EmuWarning('X_D3DFMT_A8L8 -> D3DFMT_R5G6B5');
      Result := D3DFMT_R5G6B5; // Cxbx NOTE: HACK: Totally and utterly wrong :)
    end;

    X_D3DFMT_A8: // Swizzled
      Result := D3DFMT_A8;

    X_D3DFMT_LIN_A4R4G4B4, // Linear
    X_D3DFMT_A4R4G4B4: // Swizzled
      Result := D3DFMT_A4R4G4B4;

    X_D3DFMT_LIN_R5G6B5, // Linear
    X_D3DFMT_R5G6B5: // Swizzled
      Result := D3DFMT_R5G6B5;

    X_D3DFMT_LIN_A8R8G8B8, // Linear
    X_D3DFMT_A8R8G8B8: // Swizzled
      Result := D3DFMT_A8R8G8B8;

    X_D3DFMT_LIN_R8B8: // Linear
    begin
      EmuWarning('X_D3DFMT_LIN_R8B8 -> D3DFMT_R5G6B5');
      Result := D3DFMT_R5G6B5; // Cxbx NOTE: HACK: Totally and utterly wrong :)
    end;

    X_D3DFMT_LIN_G8B8: // Linear
    begin
      EmuWarning('X_D3DFMT_LIN_G8B8 -> D3DFMT_R5G6B5');
      Result := D3DFMT_R5G6B5; // Cxbx NOTE: HACK: Totally and utterly wrong :)
    end;

    X_D3DFMT_A8B8G8R8: // Swizzled
    begin
      EmuWarning('X_D3DFMT_A8B8G8R8 -> D3DFMT_A8R8G8B8');
      Result := D3DFMT_A8R8G8B8; // Cxbx NOTE: HACK: R<->B Swapped!
    end;

    X_D3DFMT_LIN_A8B8G8R8: // Linear
    begin
      EmuWarning('X_D3DFMT_LIN_A8B8G8R8 -> D3DFMT_A8R8G8B8');
      Result := D3DFMT_A8R8G8B8; // Cxbx NOTE: HACK: R<->B Swapped!
    end;

    X_D3DFMT_LIN_X8R8G8B8, // Linear
    X_D3DFMT_X8R8G8B8: // Swizzled
      Result := D3DFMT_X8R8G8B8;

    X_D3DFMT_P8: // Swizzled
      Result := D3DFMT_P8;

    X_D3DFMT_DXT1: // Compressed
      Result := D3DFMT_DXT1;

    //X_D3DFMT_DXT2,
    X_D3DFMT_DXT3: // Compressed
      Result := D3DFMT_DXT3;

    //X_D3DFMT_DXT4,
    X_D3DFMT_DXT5: // Compressed
      Result := D3DFMT_DXT5;

    X_D3DFMT_YUY2: // Swizzled
      Result := D3DFMT_YUY2;

    X_D3DFMT_UYVY: // Swizzled
      Result := D3DFMT_UYVY;

    X_D3DFMT_LIN_D24S8, // Linear
    X_D3DFMT_D24S8: // Swizzled
      Result := D3DFMT_D24S8;

    X_D3DFMT_F24S8: // Swizzled
    begin
      EmuWarning('X_D3DFMT_F24S8 -> D3DFMT_D24S8');
      Result := D3DFMT_D24S8; // NOTE: Hack!! PC does not have D3DFMT_F24S8 (Float vs Int)
    end;

    X_D3DFMT_LIN_D16, // Linear
    X_D3DFMT_D16: // Swizzled
      Result := D3DFMT_D16; // TODO -oCXBX: D3DDMT_D16 on Xbox is always lockable

    X_D3DFMT_L6V5U5: // Swizzled
      Result := D3DFMT_L6V5U5;

    X_D3DFMT_V8U8: // Swizzled
      Result := D3DFMT_V8U8;

    X_D3DFMT_V16U16: // Swizzled
      Result := D3DFMT_V16U16;

    X_D3DFMT_VERTEXDATA:
      Result := D3DFMT_VERTEXDATA;

    $FFFFFFFF:
      Result := D3DFMT_UNKNOWN; // TODO -oCXBX: Not sure if this counts as swizzled or not...

  else
    CxbxKrnlCleanup('EmuXB2PC_D3DFormat: Unknown Format (0x%.08X)', [aFormat]);
    Result := D3DFORMAT(aFormat);
  end;
end;

// convert from pc to xbox color formats
function EmuPC2XB_D3DFormat(aFormat: D3DFORMAT): X_D3DFORMAT; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  case aFormat of
    D3DFMT_YUY2:
      Result := X_D3DFMT_YUY2;
    D3DFMT_UYVY:
      Result := X_D3DFMT_UYVY;
    D3DFMT_R5G6B5:
      Result := X_D3DFMT_LIN_R5G6B5; // Linear
//      Result := X_D3DFMT_R5G6B5; // Swizzled
    D3DFMT_D24S8:
      Result := X_D3DFMT_D24S8; // Swizzled
    D3DFMT_DXT5:
      Result := X_D3DFMT_DXT5; // Compressed
    D3DFMT_DXT4:
      Result := X_D3DFMT_DXT4;
    D3DFMT_DXT3:
      Result := X_D3DFMT_DXT3; // Compressed
    D3DFMT_DXT2:
      Result := X_D3DFMT_DXT2;
    D3DFMT_DXT1:
      Result := X_D3DFMT_DXT1; // Compressed
    D3DFMT_A1R5G5B5:
      Result := X_D3DFMT_LIN_A1R5G5B5; // Linear
    D3DFMT_X8R8G8B8:
      Result := X_D3DFMT_LIN_X8R8G8B8; // Linear
//      Result := X_D3DFMT_X8R8G8B8; // Swizzled
    D3DFMT_A8R8G8B8:
//      Result := X_D3DFMT_LIN_A8R8G8B8; // Linear
      Result := X_D3DFMT_A8R8G8B8;

    D3DFMT_A4R4G4B4:
      Result := X_D3DFMT_LIN_A4R4G4B4; // Linear
//      Result := X_D3DFMT_A4R4G4B4; // Swizzled
    D3DFMT_L8:
        Result := X_D3DFMT_LIN_L8; // Linear
//        Result := X_D3DFMT_L8; // Swizzled
    D3DFMT_D16,
    D3DFMT_D16_LOCKABLE:
      Result := X_D3DFMT_D16_LOCKABLE; // Swizzled

    D3DFMT_UNKNOWN:
      Result := $FFFFFFFF;

// Dxbx additions :
    D3DFMT_L6V5U5:
      Result := X_D3DFMT_L6V5U5; // Swizzled

    D3DFMT_V8U8:
      Result := X_D3DFMT_V8U8; // Swizzled

    D3DFMT_V16U16:
      Result := X_D3DFMT_V16U16; // Swizzled

    D3DFMT_VERTEXDATA:
      Result := X_D3DFMT_VERTEXDATA;
  else
    CxbxKrnlCleanup('EmuPC2XB_D3DFormat: Unknown Format (%d)', [Ord(aFormat)]);
    Result := X_D3DFORMAT(aFormat);
  end;
end;

// convert from xbox to pc d3d lock flags
function EmuXB2PC_D3DLock(Flags: DWORD): DWORD; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  NewFlags: DWORD;
begin
  NewFlags := 0;

 // Need to convert the flags,
 //TODO -oCXBX: fix the xbox extensions
  if (Flags and X_D3DLOCK_NOFLUSH) > 0 then
    NewFlags := NewFlags xor 0;

  if (Flags and X_D3DLOCK_NOOVERWRITE) > 0 then
    NewFlags := NewFlags xor D3DLOCK_NOOVERWRITE;

  if (Flags and X_D3DLOCK_TILED) > 0 then
    NewFlags := NewFlags xor 0;

  if (Flags and X_D3DLOCK_READONLY) > 0 then
    NewFlags := NewFlags xor D3DLOCK_READONLY;

  Result := NewFlags;
end;

// Code translated from Convert.h :

// convert from xbox to pc multisample formats
function EmuXB2PC_D3DMultiSampleFormat(aType: DWORD): D3DMULTISAMPLE_TYPE;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  case aType of
    $0011:
      Result := D3DMULTISAMPLE_NONE;

    $1021,
    $1121,
    $2021,
    $2012:
      Result := D3DMULTISAMPLE_2_SAMPLES;

    $1022,
    $1222,
    $2022,
    $2222:
      Result := D3DMULTISAMPLE_4_SAMPLES;

    $1233,
    $2233:
      Result := D3DMULTISAMPLE_9_SAMPLES;
  else
    CxbxKrnlCleanup( 'Unknown Multisample Type (0x%X)!'#13#10 +
                 'If this value is greater than 0xFFFF contact blueshogun!', [aType] );

    Result := D3DMULTISAMPLE_NONE;
  end;
end;

// convert from xbox to pc texture transform state types
function EmuXB2PC_D3DTS(State: D3DTRANSFORMSTATETYPE): D3DTRANSFORMSTATETYPE; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  if (uint32(State) < 2) then
    Result := D3DTRANSFORMSTATETYPE(Ord(State) + 2)
  else if (uint32(State) < 6) then
    Result := D3DTRANSFORMSTATETYPE(Ord(State) + 14)
  else if (uint32(State) < 10) then
    Result := D3DTS_WORLDMATRIX(Ord(State) - 6)
  else if (uint32(State) = 10) then // Max
    Result := D3DTRANSFORMSTATETYPE(Ord(D3DTS_TEXTURE7) + 1)
  else
  begin
    CxbxKrnlCleanup('Unknown Transform State Type (%d)', [Ord(State)]);

    Result := State;
  end;
end;

// convert from xbox to pc blend ops
function EmuXB2PC_D3DBLENDOP(Value: X_D3DBLENDOP): D3DBLENDOP; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  case(Value) of
    $8006:
      Result := D3DBLENDOP_ADD;
    $800a:
      Result := D3DBLENDOP_SUBTRACT;
    $800b:
      Result := D3DBLENDOP_REVSUBTRACT;
    $8007:
      Result := D3DBLENDOP_MIN;
    $8008:
      Result := D3DBLENDOP_MAX;
    $F006:
    begin
      CxbxKrnlCleanup('D3DBLENDOP_ADDSIGNED is not supported!');
      Result := D3DBLENDOP_ADD;
    end;
    $F005:
    begin
      CxbxKrnlCleanup('D3DBLENDOP_REVSUBTRACTSIGNED is not supported!');
      Result := D3DBLENDOP_REVSUBTRACT;
    end;
  else

    CxbxKrnlCleanup('Unknown D3DBLENDOP (0x%.08X)', [Value]);

    Result := D3DBLENDOP(Value);
  end;
end;

// convert from xbox to pc blend types
function EmuXB2PC_D3DBLEND(Value: X_D3DBLEND): D3DBLEND; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  if (Value < 2) then
    Result := D3DBLEND(Value + 1)
  else if (Value < $309) then
    Result := D3DBLEND((Value and $F) + 3)
  else
  begin
    CxbxKrnlCleanup('Unknown Xbox D3DBLEND Extension (0x%.08X)', [Value]);

    Result := D3DBLEND(Value);
  end;
end;

// convert from xbox to pc comparison functions
function EmuXB2PC_D3DCMPFUNC(Value: X_D3DCMPFUNC): D3DCMPFUNC; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := D3DCMPFUNC((Value and $F) + 1);
end;

// convert from xbox to pc fill modes
function EmuXB2PC_D3DFILLMODE(Value: X_D3DFILLMODE): D3DFILLMODE; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := D3DFILLMODE((Value and $F) + 1);
end;

// convert from xbox to pc shade modes
function EmuXB2PC_D3DSHADEMODE(Value: X_D3DSHADEMODE): D3DSHADEMODE; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := D3DSHADEMODE((Value and $3) + 1);
end;

// convert from xbox to pc stencilop modes
function EmuXB2PC_D3DSTENCILOP(Value: X_D3DSTENCILOP): D3DSTENCILOP; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  case(Value)of
    $1e00:
      Result := D3DSTENCILOP_KEEP;
    0:
      Result := D3DSTENCILOP_ZERO;
    $1e01:
      Result := D3DSTENCILOP_REPLACE;
    $1e02:
      Result := D3DSTENCILOP_INCRSAT;
    $1e03:
      Result := D3DSTENCILOP_DECRSAT;
    $150a:
      Result := D3DSTENCILOP_INVERT;
    $8507:
      Result := D3DSTENCILOP_INCR;
    $8508:
      Result := D3DSTENCILOP_DECR;

  else //default:
    CxbxKrnlCleanup('Unknown D3DSTENCILOP (0x%.08X)', [Value]);
    Result := D3DSTENCILOP(Value);
  end;
end;

// convert from vertex count to primitive count (Xbox)
function EmuD3DVertex2PrimitiveCount(PrimitiveType: X_D3DPRIMITIVETYPE; VertexCount: int): INT; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := ((VertexCount - EmuD3DVertexToPrimitive[Ord(PrimitiveType)][1]) div EmuD3DVertexToPrimitive[Ord(PrimitiveType)][0]);
end;

// convert from primitive count to vertex count (Xbox)
function EmuD3DPrimitive2VertexCount(PrimitiveType: X_D3DPRIMITIVETYPE; PrimitiveCount: int): int; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  Result := (((PrimitiveCount)*EmuD3DVertexToPrimitive[Ord(PrimitiveType)][0]) + EmuD3DVertexToPrimitive[Ord(PrimitiveType)][1]);
end;

// convert xbox->pc primitive type
function EmuPrimitiveType(PrimitiveType: X_D3DPRIMITIVETYPE): D3DPRIMITIVETYPE; inline;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
begin
  if DWORD(PrimitiveType) > DWORD(Ord(High(D3DPRIMITIVETYPE))) then
    Result := D3DPRIMITIVETYPE($7FFFFFFF)
  else
    Result := EmuPrimitiveTypeLookup[Ord(PrimitiveType)];
end;

{.$MESSAGE 'PatrickvL reviewed up to here'}
end.

