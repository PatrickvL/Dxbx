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
  MMSystem, // SEEK_SET
  SysUtils,
  // Jedi Win32API
  JwaWinType;

const
  CLOCKS_PER_SEC = 1000; // Because we're implementing clock() using GetTickCount()

  NULL = nil;

  HexNibble: string = '0123456789ABCDEF';

type
  TStringArray = array of string;

  TVarByteArray = array of Byte;

  TRawSection = TVarByteArray;

  TPWCharArray = array [0..(MaxInt div SizeOf(PWideChar)) - 1] of PWideChar;
  PPWCharArray = ^TPWCharArray;

  TDWordArray = array [0..(MaxInt div SizeOf(DWord)) - 1] of DWord;
  PDWordArray = ^TDWordArray;

  TBooleanArray = array [0..(MaxInt div SizeOf(Boolean)) -1] of Boolean;
  PBooleanArray = ^TBooleanArray;

  TByteArray = array [0..(MaxInt div SizeOf(Byte)) -1] of Byte;
  PByteArray = ^TByteArray;

{$IF NOT DECLARED(PDWord)}
  PDWord = ^DWord;
{$IFEND}

  PPDWORD = ^PDWORD;

{$IF NOT DECLARED(Int32)}
  Int32 = Integer;
{$IFEND}

  UInt32 = Cardinal;
  UInt16 = Word;
  UInt8 = Byte;

  PUInt32 = ^UInt32;
  PUInt16 = ^UInt16;
  PUInt8 = PByteArray;//PAnsiChar; //^UInt8;

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
  PPUChar = ^PUChar;

  DIKEYSTATE = array [0..256-1] of BYTE; // Dxbx 'invention'

  TCodePointer = type Pointer;

  // Dxbx note : Signed is actually "signed int" - see http://home.att.net/~jackklein/c/inttypes.html
  Signed = Integer;

  // Basic C types. See http://msdn.microsoft.com/en-us/library/aa383751(VS.85).aspx
  // and http://spreadsheets.google.com/ccc?key=t6Bxe-qEV-nDK4vYQsVIKBw
  // Note : These types are aliasses to JwaWinType, so we don't have to include that unit :
  PCSZ = JwaWinType.PCSZ; // = ^AnsiChar; // Dxbx assumption!
  PVOID = JwaWinType.PVOID; // = Pointer;
  LONG = JwaWinType.LONG; // = Longint;
  INT = JwaWinType.INT; // = Integer;
  size_t = JwaWinType.size_t; // = Longword;
  wchar_t = JwaWinType.wchar_t; // = WideChar;

  pwchar_t = PWideChar;

//  bool = ?;
//  BOOL = ?;
//  BOOLEAN = ?;
  CHARBOOL = ByteBool; // Cxbx : unsigned char = AnsiChar in Delphi

  PFILE = ^THandle;

{$IFNDEF UNICODE}
  UnicodeString = WideString;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; inline;
{$ENDIF}

function tolower(c: AnsiChar): AnsiChar;
function toupper(c: AnsiChar): AnsiChar;
function isdigit(c: AnsiChar): Boolean;
function isxdigit(c: AnsiChar): Boolean;

function strcpy(dest, source: PAnsiChar): PAnsiChar; // cdecl
function strncpy(dest, source: PChar; len: Integer): PChar; // cdecl
function wstrlen(const Str: PWideChar): Cardinal; overload;
function memcmp(const ptr1, ptr2: Pvoid; num: size_t): int;
function memcpy(destination: Pvoid; const source: Pvoid; num: size_t): Pvoid;// cdecl;
function memset(const ptr: Pvoid; value: Byte; num: size_t): Pvoid;// cdecl;
function clock(): DWord; // cdecl;

function mbstowcs(wcstr: pwchar_t; const mbstr: PAnsiChar; max: size_t): size_t;
function wcstombs(mbstr: PAnsiChar; const wcstr: pwchar_t; max: size_t): size_t;

procedure free(p: PVoid); inline;
function malloc(const number_of_bytes: size_t): PVoid; inline;
function calloc(num_elements, element_size: size_t): PVoid; inline;

function fopen(filename: PAnsiChar; mode: PAnsiChar): PFILE;
function fseek(stream: PFILE; offset: long; mode: int): int;
function fread(ptr: PVOID; size: size_t; nelem: size_t; stream: PFILE): size_t;
function fwrite(ptr: PVOID; size: size_t; nelem: size_t; stream: PFILE): size_t;
function fprintf(stream: PFILE; format: PAnsiChar): int; overload;
function fprintf(stream: PFILE; format: PAnsiChar; Args: array of const): int; overload;
function fclose(stream: PFILE): int;

function FIELD_OFFSET(var Variable): DWORD;
function DWord2Str(const aValue: DWORD): string;
function PCharToString(const aPtr: PAnsiChar; const aLen: Integer): AnsiString;
function PWideCharToString(const aPtr: PWideChar; const aLen: Integer): string;
function PByteToHexString(const aPtr: PByte; const aLen: Integer): string;
function IsPrintableChar(const aChar: AnsiChar): Boolean;
function IsPrintableAsciiChar(const aChar: AnsiChar): Boolean;
function ByteLength(const aStr: WideString): Integer;

function LastChar(const aStr: string): Char;

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

function wstrlen(const Str: PWideChar): Cardinal;
begin
  Result := Length(WideString(Str)); // Dxbx TODO : Do a faster #0000 search here!
end;

function memcmp(const ptr1, ptr2: Pvoid; num: size_t): int;
begin
  Result := int(not CompareMem(ptr1, ptr2, num));
end;

function memcpy(destination: Pvoid; const source: Pvoid; num: size_t): Pvoid;
begin
  Move(source^, destination^, num);
  Result := destination;
end;

function memset(const ptr: Pvoid; value: Byte; num: size_t): Pvoid;
begin
  FillChar(ptr^, num, value);
  Result := ptr;
end;

function clock(): DWord;
begin
  Result := GetTickCount();
end;

function mbstowcs(wcstr: pwchar_t; const mbstr: PAnsiChar; max: size_t): size_t;
begin
  Result := MultiByteToWideChar(CP_ACP, 0, mbstr, strlen(mbstr), wcstr, max);
  if Assigned(wcstr) and {(Result >= 0) and} (Result < max) then
    wcstr[Result] := #0;
end;

function wcstombs(mbstr: PAnsiChar; const wcstr: pwchar_t; max: size_t): size_t;
begin
  Result := WideCharToMultiByte(CP_ACP, 0, wcstr, wstrlen(wcstr), mbstr, max, '', nil);
  if Assigned(mbstr) and {(Result >= 0) and} (Result < max) then
    mbstr[Result] := #0;
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

// http://www.dinkumware.com/manuals/?manual=compleat&page=stdio.html#fopen
function fopen(filename: PAnsiChar; mode: PAnsiChar): PFILE;
var
  DesiredAccess: DWORD;
  ShareMode: DWORD;
  CreateDisposition: DWORD;
begin
(*
"r" -- to open an existing text file for reading
"r+" -- to open an existing text file for reading and writing
"rb" -- to open an existing binary file for reading
"r+b" or "rb+" -- to open an existing binary file for reading and writing

"w" -- to create a text file or to open and truncate an existing text file, for writing
"w+" -- to create a text file or to open and truncate an existing text file, for reading and writing
"wb" -- to create a binary file or to open and truncate an existing binary file, for writing
"w+b" or "wb+" -- to create a binary file or to open and truncate an existing binary file, for reading and writing

"a" -- to create a text file or to open an existing text file, for writing. The file-position indicator is positioned at the end of the file before each write
"a+" -- to create a text file or to open an existing text file, for reading and writing. The file-position indicator is positioned at the end of the file before each write
"ab" -- to create a binary file or to open an existing binary file, for writing. The file-position indicator is positioned at the end of the file (possibly after arbitrary null byte padding) before each write
"a+b" or "ab+" -- to create a binary file or to open an existing binary file, for reading and writing. The file-position indicator is positioned at the end of the file (possibly after arbitrary null byte padding) before each write
*)
  Result := nil;

  // DesiredAccess := GENERIC_READ or GENERIC_WRITE;
  // ShareMode := 0 / FILE_SHARE_DELETE / FILE_SHARE_READ / FILE_SHARE_WRITE;
  // CreateDisposition := CREATE_ALWAYS / CREATE_NEW / OPEN_ALWAYS / OPEN_EXISTING / TRUNCATE_EXISTING;

  case mode[0] of
    'r': begin
      DesiredAccess := GENERIC_READ;
      ShareMode := FILE_SHARE_READ;
      CreateDisposition := OPEN_EXISTING;
    end;
    'w': begin
      DesiredAccess := GENERIC_WRITE;
      ShareMode := 0;
      CreateDisposition := OPEN_ALWAYS;
    end;
    'a': begin
      DesiredAccess := GENERIC_WRITE;
      ShareMode := 0;
      CreateDisposition := OPEN_EXISTING;
    end;
    // 't': ??
    // 'x': ??
  else
    Exit;
  end;

  if (mode[1] = '+') or ((mode[1] <> #0) and (mode[2] = '+')) then
    DesiredAccess := GENERIC_READ or GENERIC_WRITE;

  New(Result);
  Result^ := CreateFileA(filename, DesiredAccess, ShareMode, nil, CreateDisposition, FILE_ATTRIBUTE_NORMAL, 0);
  if Result^ = INVALID_HANDLE_VALUE then
  begin
    Dispose(Result);
    Exit;
  end;

  // Append mode means we need to go to the end of the file :
  case mode[0] of
    'r': FileSeek(Result^, 0, 0);
    'a': FileSeek(Result^, 0, 2);
  else
    FileSeek(Result^, 0, 0);
  end;
end;

function fseek(stream: PFILE; offset: long; mode: int): int;
begin
  Result := FileSeek(stream^, offset, mode);
end;

function fread(ptr: PVOID; size: size_t; nelem: size_t; stream: PFILE): size_t;
begin
  Result := FileRead(stream^, ptr^, size * nelem);
end;

function fwrite(ptr: PVOID; size: size_t; nelem: size_t; stream: PFILE): size_t;
begin
  Result := FileWrite(stream^, ptr, size * nelem);
end;

function fprintf(stream: PFILE; format: PAnsiChar): int; // overload;
begin
  Result := fwrite(format, StrLen(format), 1, stream);
end;

// http://www.cplusplus.com/reference/clibrary/cstdio/fprintf/
function fprintf(stream: PFILE; format: PAnsiChar; Args: array of const): int; // overload;
begin
  Result := fprintf(stream, PAnsiChar(AnsiString(SysUtils.Format(string(format), Args))));
end;

function fclose(stream: PFILE): int;
begin
  FileClose(stream^);
  Dispose(stream);
  Result := 0;
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

function PByteToHexString(const aPtr: PByte; const aLen: Integer): string;
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

function LastChar(const aStr: string): Char;
begin
  if aStr <> '' then
    Result := aStr[Length(aStr)]
  else
    Result := #0;
end;

end.
