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

implementation

uses
  // Windows
  Windows
  , SysUtils
  // Dxbx
  , uEmuD3D8Types
  , uDxbxKrnlUtils
  , uEmu
  , uLog
  // Directx
  , Direct3D9;


function XTL_EmuXBFormatIsSwizzled(Format: X_D3DFORMAT; var pBPP: DWORD): LONGBOOL;
// Branch:martin  Revision:39  Translator:Shadow_Tj
begin
  Result := FALSE; // in cxbx is the Result = false placed as last... bit strange.
  case (Format) of
    $00,
      $01,
      $0B: begin
        pBPP := 1;
        Result := TRUE;
      end;
    $02,
      $03,
      $04,
      $05,
      $1A: begin
        pBPP := 2;
        Result := TRUE;
      end;
    $06,
      $07: begin
        pBPP := 4;
        Result := TRUE;
      end;
  end;
end;

function XTL_EmuXB2PC_D3DFormat(aFormat: X_D3DFORMAT): D3DFORMAT;
// Branch:martin  Revision:39  Translator:Shadow_Tj
begin
  case (aFormat) of
    $00: // Swizzled   (X_D3DFMT_L8)
      Result := D3DFMT_L8;

    $01: // Swizzled   (X_D3DFMT_AL8) // NOTE: Hack: Alpha ignored, basically
      begin
        EmuWarning('X_D3DFMT_AL8.D3DFMT_L8');
        Result := D3DFMT_L8;
      end;

    $02: // Swizzled   (X_D3DFMT_A1R5G5B5)
      Result := D3DFMT_A1R5G5B5;

    $03: // Swizzled   (X_D3DFMT_X1R5G5B5)
      Result := D3DFMT_X1R5G5B5;

    $1A: // Swizzled   (X_D3DFMT_A8L8)
      begin
        EmuWarning('X_D3DFMT_A8L8.D3DFMT_R5G6B5');
        Result := D3DFMT_R5G6B5; // NOTE: HACK: Totally and utterly wrong :)
      end;

    $1D, // Linear     (X_D3DFMT_LIN_A4R4G4B4)
      $04: // Swizzled   (X_D3DFMT_A4R4G4B4)
      Result := D3DFMT_A4R4G4B4;

    $11, // Linear     (X_D3DFMT_LIN_R5G6B5)
      $05: // Swizzled   (X_D3DFMT_R5G6B5)
      Result := D3DFMT_R5G6B5;

    $12, // Linear     (X_D3DFMT_LIN_A8R8G8B8)
      $06: // Swizzled   (X_D3DFMT_A8R8G8B8)
      Result := D3DFMT_A8R8G8B8;

    $16: // Linear     (X_D3DFMT_LIN_R8B8)
      begin
        EmuWarning('X_D3DFMT_LIN_R8B8.D3DFMT_R5G6B5');
        Result := D3DFMT_R5G6B5; // NOTE: HACK: Totally and utterly wrong :)
      end;
    $3F: // Linear     (X_D3DFMT_LIN_A8B8G8R8)
      begin
        EmuWarning('X_D3DFMT_LIN_A8B8G8R8.D3DFMT_A8R8G8B8');
        Result := D3DFMT_A8R8G8B8; // NOTE: HACK: R<->B Swapped!
      end;

    $1E, // Linear     (X_D3DFMT_LIN_X8R8G8B8)
      $07: // Swizzled   (X_D3DFMT_X8R8G8B8)
      Result := D3DFMT_X8R8G8B8;

    $0B: // Swizzled   (X_D3DFMT_P8)
      Result := D3DFMT_P8;

    $0C: // Compressed (X_D3DFMT_DXT1)
      Result := D3DFMT_DXT1;

    $0E: // Compressed (X_D3DFMT_DXT2)
      Result := D3DFMT_DXT2;

    $0F: // Compressed (X_D3DFMT_DXT3)
      Result := D3DFMT_DXT3;

    $24: // Swizzled   (X_D3DFMT_YUV2)
      Result := D3DFMT_YUY2;

    $2E, // Linear     (X_D3DFMT_LIN_D24S8)
      $2A: // Swizzled   (X_D3DFMT_D24S8)
      Result := D3DFMT_D24S8;

    $2B: // Swizzled   (X_D3DFMT_F24S8)
      begin
        EmuWarning('X_D3DFMT_F24S8.D3DFMT_D24S8');
        Result := D3DFMT_D24S8; // NOTE: Hack!! PC does not have D3DFMT_F24S8 (Float vs Int)
      end;

    $30, // Linear     (X_D3DFMT_LIN_D16)
      $2C: // Swizzled   (X_D3DFMT_D16)
      Result := D3DFMT_D16;

    $28: // Swizzled   (X_D3DFMT_V8U8)
      Result := D3DFMT_V8U8;

    $33: // Swizzled   (X_D3DFMT_V16U16)
      Result := D3DFMT_V16U16;

    $64:
      Result := D3DFMT_VERTEXDATA;

  else begin
      CxbxKrnlCleanup(DxbxFormat('EmuXB2PC_D3DFormat: Unknown Format ($%.08X)', [aFormat]));
      Result := D3DFORMAT(aFormat);
    end;
  end;

end;

function XTL_EmuPC2XB_D3DFormat(aFormat: D3DFORMAT): X_D3DFORMAT;
// Branch:martin  Revision:39  Translator:Shadow_Tj
begin
  case (aFormat) of
    D3DFMT_YUY2:
      Result := $24;
    D3DFMT_R5G6B5:
      Result := $11; // Linear
//            return 0x05;      // Swizzled
    D3DFMT_D24S8:
      Result := $2A;
    D3DFMT_DXT3:
      Result := $0F;
    D3DFMT_DXT2:
      Result := $0E;
    D3DFMT_DXT1:
      Result := $0C;
    D3DFMT_A1R5G5B5: // Linear (X_D3DFMT_LIN_A1R5G5B5)
      Result := $10;
    D3DFMT_X8R8G8B8:
      Result := $1E; // Linear (X_D3DFMT_LIN_X8R8G8B8)
//            return 0x07;      // Swizzled
    D3DFMT_A8R8G8B8:
//            return 0x12;      // Linear (X_D3DFMT_LIN_A8R8G8B8)
      Result := $06;
  else begin
      CxbxKrnlCleanup(Format('EmuPC2XB_D3DFormat: Unknown Format (%d)', [@aFormat]));
      Result := X_D3DFORMAT(aFormat);
    end;
  end;
end;

(*Function XTL_EmuXB2PC_D3DLock(Flags : DWORD) : DWord;
// Branch:martin  Revision:39  Translator:Shadow_Tj
var
  NewFlags : DWORD;
begin
 NewFlags := 0;

 // Need to convert the flags, TODO: fix the xbox extensions
 if(Flags and X_D3DLOCK_NOFLUSH) then
 begin
  NewFlags:= NewFlags xor 0;
  end;
 if(Flags and X_D3DLOCK_NOOVERWRITE) then
 begin
  NewFlags:= NewFlags xor D3DLOCK_NOOVERWRITE;
  end;
 if(Flags and X_D3DLOCK_TILED) then
 begin
  NewFlags:= NewFlags xor 0;
  end;
 if(Flags and X_D3DLOCK_READONLY) then
 begin
  NewFlags:= NewFlags xor D3DLOCK_READONLY;
  end;

    Result:= NewFlags;
 end;         *)

// lookup table for converting vertex count to primitive count
(*UINT XTL.EmuD3DVertexToPrimitive[11][2] =
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
);      *)

// conversion table for xbox->pc primitive types
(*XTL.D3DPRIMITIVETYPE XTL.EmuPrimitiveTypeLookup[] =
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
(*CONST DWORD XTL.EmuD3DRenderStateSimpleEncoded[174] =
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
);            *)



end.

