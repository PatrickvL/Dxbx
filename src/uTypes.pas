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
unit uTypes;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  SysUtils,
  // Jedi
  JwaWinType;

const
  CLOCKS_PER_SEC = 1000; // Because we're implementing clock() using GetTickCount()

  NULL = nil;

  HexNibble: AnsiString = '0123456789ABCDEF';

type
  TStringArray = array of string;

  TVarByteArray = array of Byte;

  TRawSection = TVarByteArray;

  TDWordArray = array [0..(MaxInt div SizeOf(DWord)) - 1] of DWord;
  PDWordArray = ^TDWordArray;

  TByteArray = array [0..(MaxInt div SizeOf(Byte)) -1] of Byte;
  PByteArray = ^TByteArray;

{$IF NOT DECLARED(PDWord)}
  PDWord = ^DWord;
{$IFEND}

{$IF NOT DECLARED(Int32)}
  Int32 = Integer;
{$IFEND}

  UInt32 = Cardinal;
  UInt16 = Word;
  UInt8 = Byte;

  PUInt32 = ^UInt32;
  PUInt16 = ^UInt16;
  PUInt8 = PAnsiChar; //^UInt8;

  UInt08 = UInt8;
  PUInt08 = PUInt8;

{$IF DECLARED(UINT_PTR)}
  UIntPtr = UINT_PTR;
{$ELSE}
  UIntPtr = UInt32;
{$IFEND}

{$IF DECLARED(INT_PTR)}
  IntPtr = INT_PTR;
{$ELSE}
  IntPtr = Int32;
{$IFEND}

{$IFDEF SUPPORTS_POINTERMATH}
  MathPtr = PByte;
{$ELSE}
  // For compatibility with older Delphi versions :
  MathPtr = PAnsiChar;
{$ENDIF}

  PPByte = ^PByte;

  DIKEYSTATE = array [0..256-1] of BYTE; // Dxbx 'invention'

  TCodePointer = type Pointer;

  // Dxbx note : Signed is actually "signed int" - see http://home.att.net/~jackklein/c/inttypes.html
  Signed = Integer;

  // Note : These types are aliasses to JwaWinType, so we don't have to include that unit :
  PCSZ = JwaWinType.PCSZ; // = ^AnsiChar; // Dxbx assumption!
  PVOID = JwaWinType.PVOID; // = Pointer;
  LONG = JwaWinType.LONG; // = Longint;
  INT = JwaWinType.INT; // = Integer;
  size_t = JwaWinType.size_t; // = Longword;
  wchar_t = JwaWinType.wchar_t; // = WideChar;

  pwchar_t = PWideChar;
  
{$IFNDEF UNICODE}
  UnicodeString = WideString;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
{$ENDIF}

function tolower(c: AnsiChar): AnsiChar;
function toupper(c: AnsiChar): AnsiChar;
function isdigit(c: AnsiChar): Boolean;
function isxdigit(c: AnsiChar): Boolean;

function strcpy(dest, source: PAnsiChar): PAnsiChar; // cdecl
function strncpy(dest, source: PChar; len: Integer): PChar; // cdecl
procedure memset(p: Pointer; b: Byte; count: Integer); // cdecl;
procedure memcpy(dest, source: Pointer; count: Integer); // cdecl;
function clock(): DWord; // cdecl;
function mbstowcs(wcstr: pwchar_t; const mbstr: PAnsiChar; max: size_t): size_t;

procedure free(p: PVoid); inline;
function malloc(const number_of_bytes: size_t): PVoid; inline;
function calloc(num_elements, element_size: size_t): PVoid; inline;

function FIELD_OFFSET(var Variable): DWORD;
function DWord2Str(const aValue: DWORD): string;
function PCharToString(const aPtr: PAnsiChar; const aLen: Integer): AnsiString;
function PWideCharToString(const aPtr: PWideChar; const aLen: Integer): string;
function PByteToHexString(const aPtr: PByte; const aLen: Integer): AnsiString;
function IsPrintableChar(const aChar: AnsiChar): Boolean;
function IsPrintableAsciiChar(const aChar: AnsiChar): Boolean;
function ByteLength(const aStr: WideString): Integer;

implementation

{$IFNDEF UNICODE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

// c function implementations

function tolower(c: AnsiChar): AnsiChar;
begin
  Result := c;
  if (c >= 'A') and (c <= 'Z') then
    Result := AnsiChar(Ord(c) + Ord('a') - Ord('A'));
end;

function toupper(c: AnsiChar): AnsiChar;
begin
  Result := c;
  if (c >= 'a') and (c <= 'z') then
    Result := AnsiChar(Ord(c) + Ord('A') - Ord('a'));
end;

function isdigit(c: AnsiChar): Boolean;
begin
  Result := c in ['0'..'9'];
end;

function isxdigit(c: AnsiChar): Boolean;
begin
  Result := c in ['0'..'9', 'A'..'Z', 'a'..'z'];
end;

function strcpy(dest, source: PAnsiChar): PAnsiChar; // cdecl
begin
  Result := StrCopy(Dest, Source);
end;

function strncpy(dest, source: PChar; len: Integer): PChar; // cdecl
begin
  Result := StrLCopy(Dest, Source, Len);
end;

// Source: ZLib.pas
procedure memset(p: Pointer; b: Byte; count: Integer); // cdecl;
begin
  FillChar(p^, count, b);
end;

// Source: ZLib.pas
procedure memcpy(dest, source: Pointer; count: Integer); // cdecl;
begin
  Move(source^, dest^, count);
end;

function clock(): DWord;
begin
  Result := GetTickCount();
end;

function mbstowcs(wcstr: pwchar_t; const mbstr: PAnsiChar; max: size_t): size_t;
begin
  Result := MultiByteToWideChar(CP_ACP, 0, mbstr, strlen(mbstr), wcstr, max);
end;

procedure free(p: PVoid); inline;
begin
  FreeMem(p);
end;

function malloc(const number_of_bytes: size_t): PVoid; inline;
begin
  Result := AllocMem(number_of_bytes);
end;

function calloc(num_elements, element_size: size_t): PVoid; inline;
begin
  Result := malloc(num_elements * element_size);
  memset(Result, 0, num_elements * element_size);
end;

// Note: Instead of calling FIELD_OFFSET(Type, Member)
//       use the construct: FIELD_OFFSET(PType(nil).Member)
function FIELD_OFFSET(var Variable): DWORD;
begin
  Result := DWORD(@(Pointer(Variable)));
end;

function DWord2Str(const aValue: DWORD): string;
begin
  Result := IntToHex(aValue, 8);
end;

function PCharToString(const aPtr: PAnsiChar; const aLen: Integer): AnsiString;
var
  i: Integer;
begin
  i := 0;
  while (aPtr[i] > #0) and (i < aLen) do
    Inc(i);

  SetString(Result, aPtr, i);
end;

function PWideCharToString(const aPtr: PWideChar; const aLen: Integer): string;
var
  i: Integer;
  WStr: WideString;
begin
  i := 0;
  while (aPtr[i] > #0) and (i < aLen) do
    Inc(i);

  SetString(WStr, aPtr, i);
  Result := WStr;
end;

function PByteToHexString(const aPtr: PByte; const aLen: Integer): AnsiString;
var
  i: Integer;
begin
  if aPtr = nil then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, aLen * 2);
  for i := 0 to aLen - 1 do
  begin
    Result[1 + i + i] := HexNibble[1 + (PByteArray(aPtr)[i] shr 4)];
    Result[2 + i + i] := HexNibble[1 + (PByteArray(aPtr)[i] and 15)];
  end;
end;

function IsPrintableChar(const aChar: AnsiChar): Boolean;
begin
  Result := Ord(aChar) >= Ord(' ');
end;

function IsPrintableAsciiChar(const aChar: AnsiChar): Boolean;
begin
  Result := IsPrintableChar(aChar) and (Ord(aChar) < 127);
end;

function ByteLength(const aStr: WideString): Integer;
begin
  Result := Length(aStr) * SizeOf(WideChar);
end;

end.
