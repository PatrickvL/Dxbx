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
  SysUtils;

const
  CLOCKS_PER_SEC = 1000; // Because we're implementing clock() using GetTickCount()

  NULL = nil;

type
  TVarByteArray = array of Byte;

  TRawSection = TVarByteArray;
  
  TDWordArray = array[0..10000] of DWord;
  PDWordArray = ^TDWordArray;

  TByteArray = array [0..MaxInt-1] of Byte;
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

  // Note : These types are copied from JwaWinType, so we don't have to include that unit :
  PCSZ = ^AnsiChar; // Dxbx assumption!
  PVOID = Pointer;
  LONG = Longint;
  INT = Integer;
  size_t = Longword;
  wchar_t = WideChar;
  pwchar_t = PWideChar;
  
{$IFNDEF UNICODE}
  UnicodeString = WideString;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
{$ENDIF}

function strcpy(dest, source: PAnsiChar): PAnsiChar; // cdecl
function strncpy(dest, source: PChar; len: Integer): PChar; // cdecl
procedure memset(p: Pointer; b: Byte; count: Integer); // cdecl;
procedure memcpy(dest, source: Pointer; count: Integer); // cdecl;
function clock(): DWord; // cdecl;
function mbstowcs(wcstr: pwchar_t; const mbstr: PAnsiChar; max: size_t): size_t;

procedure free(p: PVoid); inline;
function malloc(const number_of_bytes: size_t): PVoid; inline;
function calloc(num_elements, element_size: size_t): PVoid; inline;

implementation

{$IFNDEF UNICODE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

// c function implementations

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

end.
