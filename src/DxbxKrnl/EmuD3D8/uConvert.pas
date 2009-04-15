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

{$INCLUDE ..\..\Dxbx.inc}

interface

uses
  // Delphi
  Windows
  , SysUtils
  // Directx
  , Direct3D8 // was Direct3D9 ?!
  // Dxbx
  , uEmuD3D8Types
  , uTypes
  , uLog
  , uXboxLibraryUtils
  , uDxbxKrnlUtils
  , uEmu;

const
  X_D3DFMT_L8 = $00; // Swizzled
  X_D3DFMT_AL8 = $01; // Swizzled
  X_D3DFMT_A1R5G5B5 = $02; // Swizzled
  X_D3DFMT_X1R5G5B5 = $03; // Swizzled
  X_D3DFMT_A4R4G4B4 = $04; // Swizzled
  X_D3DFMT_R5G6B5 = $05; // Swizzled
  X_D3DFMT_A8R8G8B8 = $06; // Swizzled
  X_D3DFMT_X8R8G8B8 = $07; // Swizzled
  X_D3DFMT_P8 = $0B; // Swizzled
  X_D3DFMT_DXT1 = $0C; // Compressed
  X_D3DFMT_DXT2 = $0E; // Compressed
  X_D3DFMT_DXT3 = $0F; // Compressed
  X_D3DFMT_LIN_A1R5G5B5 = $10;
  X_D3DFMT_LIN_R5G6B5 = $11; // Linear
  X_D3DFMT_LIN_A8R8G8B8 = $12;
  X_D3DFMT_LIN_R8B8 = $16; // Linear
  X_D3DFMT_A8L8 = $1A; // Swizzled
  X_D3DFMT_LIN_A4R4G4B4 = $1D; // Linear
  X_D3DFMT_LIN_X8R8G8B8 = $1E; // Linear
  X_D3DFMT_YUY2 = $24;
//  X_D3DFMT_YUV2 = $24 ??; // Swizzled
  X_D3DFMT_V8U8 = $28; // Swizzled
  X_D3DFMT_D24S8 = $2A;
  X_D3DFMT_F24S8 = $2B; // Swizzled
  X_D3DFMT_D16 = $2C; // Swizzled
  X_D3DFMT_LIN_D24S8 = $2E; // Linear
  X_D3DFMT_LIN_A8B8G8R8 = $3F; // Linear
  X_D3DFMT_LIN_D16 = $30; // Linear
  X_D3DFMT_V16U16 = $33; // Swizzled

function EmuXB2PC_D3DFormat(aFormat: X_D3DFORMAT): D3DFORMAT; stdcall;
function EmuPC2XB_D3DFormat(aFormat: D3DFORMAT): X_D3DFORMAT; stdcall;

function EmuXB2PC_D3DFILLMODE(Value: X_D3DFILLMODE): D3DFILLMODE; stdcall;
function EmuXB2PC_D3DSHADEMODE(Value: X_D3DSHADEMODE): D3DSHADEMODE; stdcall;
function EmuXB2PC_D3DBLENDOP(Value: X_D3DBLENDOP): D3DBLENDOP; stdcall;
function EmuXB2PC_D3DBLEND(Value: X_D3DBLEND): D3DBLEND; stdcall;
function EmuXB2PC_D3DCMPFUNC(Value: X_D3DCMPFUNC): D3DCMPFUNC; stdcall;
function EmuXB2PC_D3DTS(State: D3DTRANSFORMSTATETYPE): D3DTRANSFORMSTATETYPE; stdcall;
function EmuXB2PC_D3DLock(Flags: DWord): DWord; stdcall;

Function EmuD3DVertex2PrimitiveCount(PrimitiveType : int; VertexCount : int) : INT;
Function EmuPrimitiveType(PrimitiveType : X_D3DPRIMITIVETYPE ) : D3DPRIMITIVETYPE;

var
  EmuD3DVertexToPrimitive : array [0..10] of array [0..2] of UINT;
  EmuPrimitiveTypeLookup : array of D3DPRIMITIVETYPE;
                                 

implementation


 // convert xbox->pc primitive type
Function EmuPrimitiveType(PrimitiveType : X_D3DPRIMITIVETYPE ) : D3DPRIMITIVETYPE;
begin
  if(DWORD(PrimitiveType) = $7FFFFFFF) then
    result := D3DPT_FORCE_DWORD;

  result:= EmuPrimitiveTypeLookup[Ord(PrimitiveType)];
end;

// convert from vertex count to primitive count (Xbox)
Function EmuD3DVertex2PrimitiveCount(PrimitiveType : int; VertexCount : int) : INT;
begin
  Result := Trunc((VertexCount - EmuD3DVertexToPrimitive[PrimitiveType][1]) / EmuD3DVertexToPrimitive[PrimitiveType][0]);
end;


function EmuXBFormatIsSwizzled(Format: X_D3DFORMAT; var pBPP: DWord): LONGBOOL; stdcall;
// Branch:martin  Revision:39 Done:100 Translator:Shadow_Tj
begin
  Result := True;
  case Format of
    $00,
      $01,
      $0B:
      pBPP := 1;
    $02,
      $03,
      $04,
      $05,
      $1A:
      pBPP := 2;
    $06,
      $07:
      pBPP := 4;
  else
    Result := False;
  end;
end;

function EmuXB2PC_D3DFormat(aFormat: X_D3DFORMAT): D3DFORMAT; stdcall;
// Branch:martin  Revision:39 Done:100 Translator:Shadow_Tj
begin
  case aFormat of
    X_D3DFMT_L8: // Swizzled
      Result := D3DFMT_L8;

    X_D3DFMT_AL8: // Swizzled    // NOTE: Hack: Alpha ignored, basically
      begin
        EmuWarning('X_D3DFMT_AL8.D3DFMT_L8');
        Result := D3DFMT_L8;
      end;

    X_D3DFMT_A1R5G5B5: // Swizzled
      Result := D3DFMT_A1R5G5B5;

    X_D3DFMT_X1R5G5B5: // Swizzled
      Result := D3DFMT_X1R5G5B5;

    X_D3DFMT_A8L8: // Swizzled
      begin
        EmuWarning('X_D3DFMT_A8L8.D3DFMT_R5G6B5');
        Result := D3DFMT_R5G6B5; // NOTE: HACK: Totally and utterly wrong :)
      end;

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
        EmuWarning('X_D3DFMT_LIN_R8B8.D3DFMT_R5G6B5');
        Result := D3DFMT_R5G6B5; // NOTE: HACK: Totally and utterly wrong :)
      end;
    X_D3DFMT_LIN_A8B8G8R8: // Linear
      begin
        EmuWarning('X_D3DFMT_LIN_A8B8G8R8.D3DFMT_A8R8G8B8');
        Result := D3DFMT_A8R8G8B8; // NOTE: HACK: R<->B Swapped!
      end;

    X_D3DFMT_LIN_X8R8G8B8, // Linear
      X_D3DFMT_X8R8G8B8: // Swizzled
      Result := D3DFMT_X8R8G8B8;

    X_D3DFMT_P8: // Swizzled
      Result := D3DFMT_P8;

    X_D3DFMT_DXT1: // Compressed
      Result := D3DFMT_DXT1;

    X_D3DFMT_DXT2: // Compressed
      Result := D3DFMT_DXT2;

    X_D3DFMT_DXT3: // Compressed
      Result := D3DFMT_DXT3;

    $24: // Swizzled   (X_D3DFMT_YUV2)
      Result := D3DFMT_YUY2;

    X_D3DFMT_LIN_D24S8, // Linear
      X_D3DFMT_D24S8: // Swizzled
      Result := D3DFMT_D24S8;

    X_D3DFMT_F24S8: // Swizzled
      begin
        EmuWarning('X_D3DFMT_F24S8.D3DFMT_D24S8');
        Result := D3DFMT_D24S8; // NOTE: Hack!! PC does not have D3DFMT_F24S8 (Float vs Int)
      end;

    X_D3DFMT_LIN_D16, // Linear
      X_D3DFMT_D16: // Swizzled
      Result := D3DFMT_D16;

    X_D3DFMT_V8U8: // Swizzled
      Result := D3DFMT_V8U8;

    X_D3DFMT_V16U16: // Swizzled
      Result := D3DFMT_V16U16;

    $64:
      Result := D3DFMT_VERTEXDATA;

  else
    CxbxKrnlCleanup('EmuXB2PC_D3DFormat: Unknown Format ($%.08X)', [aFormat]);
    Result := D3DFORMAT(aFormat);
  end;
end;

function EmuPC2XB_D3DFormat(aFormat: D3DFORMAT): X_D3DFORMAT; stdcall;
// Branch:martin  Revision:39 Done:100 Translator:Shadow_Tj
begin
  case aFormat of
    D3DFMT_YUY2:
      Result := X_D3DFMT_YUY2;
    D3DFMT_R5G6B5:
      Result := $11; // Linear
//            return 0x05;      // Swizzled
    D3DFMT_D24S8:
      Result := X_D3DFMT_D24S8;
    D3DFMT_DXT3:
      Result := X_D3DFMT_DXT3;
    D3DFMT_DXT2:
      Result := X_D3DFMT_DXT2;
    D3DFMT_DXT1:
      Result := X_D3DFMT_DXT1;
    D3DFMT_A1R5G5B5: // Linear ()
      Result := X_D3DFMT_LIN_A1R5G5B5;
    D3DFMT_X8R8G8B8:
      Result := X_D3DFMT_LIN_X8R8G8B8; // Linear
//      Result := X_D3DFMT_X8R8G8B8;      // Swizzled
    D3DFMT_A8R8G8B8:
//      Result := X_D3DFMT_LIN_A8R8G8B8;      // Linear
      Result := X_D3DFMT_A8R8G8B8;
  else
    CxbxKrnlCleanup('EmuPC2XB_D3DFormat: Unknown Format (%d)', [Ord(aFormat)]);
    Result := X_D3DFORMAT(aFormat);
  end;
end;

function EmuXB2PC_D3DLock(Flags: DWord): DWord; stdcall;
// Branch:martin  Revision:39 Done:100 Translator:Shadow_Tj
var
  NewFlags: DWord;
begin
  NewFlags := 0;

 // Need to convert the flags, Cxbx TODO: fix the xbox extensions
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

// lookup table for converting vertex count to primitive count
(*UINT XTL.EmuD3DVertexToPrimitive[11][2] =
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    begin 0, 0),
    begin 1, 0),
    begin 2, 0),
    begin 1, 1),
    begin 1, 1),
    begin 3, 0),
    begin 1, 2),
    begin 1, 2),
    begin 4, 0),
    begin 2, 2),
    begin 0, 0),
);
*)

// conversion table for xbox->pc primitive types
(*XTL.D3DPRIMITIVETYPE XTL.EmuPrimitiveTypeLookup[] =
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
     (XTL::D3DPRIMITIVETYPE)0,    // NULL                 = 0
      XTL::D3DPT_POINTLIST,       // D3DPT_POINTLIST      = 1,
      XTL::D3DPT_LINELIST,        // D3DPT_LINELIST       = 2,
      XTL::D3DPT_LINESTRIP,       // D3DPT_LINELOOP       = 3,  Xbox
      XTL::D3DPT_LINESTRIP,       // D3DPT_LINESTRIP      = 4,
      XTL::D3DPT_TRIANGLELIST,    // D3DPT_TRIANGLELIST   = 5,
      XTL::D3DPT_TRIANGLESTRIP,   // D3DPT_TRIANGLESTRIP  = 6,
      XTL::D3DPT_TRIANGLEFAN,     // D3DPT_TRIANGLEFAN    = 7,
      XTL::D3DPT_TRIANGLELIST,    // D3DPT_QUADLIST       = 8,  Xbox
      XTL::D3DPT_TRIANGLELIST,    // D3DPT_QUADSTRIP      = 9,  Xbox
      XTL::D3DPT_TRIANGLELIST,    // D3DPT_POLYGON        = 10, Xbox
     (XTL::D3DPRIMITIVETYPE)11    // D3DPT_MAX            = 11,
);*)

// render state conversion table
(*CONST DWord XTL.EmuD3DRenderStateSimpleEncoded[174] =
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    // WARNING: This lookup table strongly binds us to an SDK with these
    // specific #define values for D3DRS_*. Make VERY sure that you have
    // the correct lookup values;
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 0
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 2
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 4
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 6
    X_D3DRSSE_UNK,  $0004037c,     // 8  - D3DRS_SHADEMODE
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 10
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 12
    $0004035c,     $00040300,     // 14 - D3DRS_ZWRITEENABLE, D3DRS_ALPHATESTENABLE
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 16
    X_D3DRSSE_UNK,  $00040344,     // 18
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
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 54
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 56
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 58
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
    X_D3DRSSE_UNK,  X_D3DRSSE_UNK,  // 172
);
*)


// convert from xbox to pc fill modes
function EmuXB2PC_D3DFILLMODE(Value: X_D3DFILLMODE): D3DFILLMODE; stdcall;
// Branch:martin  Revision:39 Done:100 Translator:PatrickvL
begin
  Result := D3DFILLMODE((Value and $F) + 1);
end;

// convert from xbox to pc shade modes
function EmuXB2PC_D3DSHADEMODE(Value: X_D3DSHADEMODE): D3DSHADEMODE; stdcall;
// Branch:martin  Revision:39 Done:100 Translator:PatrickvL
begin
  Result := D3DSHADEMODE((Value and $3) + 1);
end;

function EmuXB2PC_D3DBLENDOP(Value: X_D3DBLENDOP): D3DBLENDOP; stdcall;
// Branch:martin  Revision:39 Done:100 Translator:PatrickvL
begin
  case Value of
    $8006:
      begin
        Result := D3DBLENDOP_ADD;
        Exit;
      end;
  end;

  CxbxKrnlCleanup(DxbxFormat('Unknown D3DBLENDOP (0x%.08X)', [Value]));

  Result := D3DBLENDOP(Value);
end;

function EmuXB2PC_D3DBLEND(Value: X_D3DBLEND): D3DBLEND; stdcall;
// Branch:martin  Revision:39 Done:100 Translator:Shadow_Tj
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

function EmuXB2PC_D3DCMPFUNC(Value: X_D3DCMPFUNC): D3DCMPFUNC; stdcall;
// Branch:martin  Revision:39 Done:100 Translator:PatrickvL
begin
  Result := D3DCMPFUNC((Value and $F) + 1);
end;

// convert from xbox to pc texture transform state types
function EmuXB2PC_D3DTS(State: D3DTRANSFORMSTATETYPE): D3DTRANSFORMSTATETYPE; stdcall;
// Branch:martin  Revision:39 Done:100 Translator:Shadow_Tj
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
    CxbxKrnlCleanup('Unknown Transform State cType (%d)', [Ord(State)]);

    Result := State;
  end;
end;

exports
  EmuPC2XB_D3DFormat name PatchPrefix + 'EmuPC2XB_D3DFormat',
  EmuXB2PC_D3DBLEND name PatchPrefix + 'EmuXB2PC_D3DBLEND',
  EmuXB2PC_D3DBLENDOP name PatchPrefix + 'EmuXB2PC_D3DBLENDOP',
  EmuXB2PC_D3DCMPFUNC name PatchPrefix + 'EmuXB2PC_D3DCMPFUNC',
  EmuXB2PC_D3DFILLMODE name PatchPrefix + 'EmuXB2PC_D3DFILLMODE',
  EmuXB2PC_D3DFormat name PatchPrefix + 'EmuXB2PC_D3DFormat',
  EmuXB2PC_D3DLock name PatchPrefix + 'EmuXB2PC_D3DLock',
  EmuXB2PC_D3DSHADEMODE name PatchPrefix + 'EmuXB2PC_D3DSHADEMODE',
  EmuXB2PC_D3DTS name PatchPrefix + 'EmuXB2PC_D3DTS',
  EmuXBFormatIsSwizzled name PatchPrefix + 'EmuXBFormatIsSwizzled';

end.

