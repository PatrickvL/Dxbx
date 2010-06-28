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

{$IFDEF UNICODE}
  {$POINTERMATH ON}
{$ENDIF}

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
  GUID = TGUID;

  TStringArray = array of string;

  TVarByteArray = array of Byte;

  TRawSection = TVarByteArray;

  TWordArray = array [0..(MaxInt div SizeOf(Word)) - 1] of Word;
  PWORDs = ^TWordArray; // READ NOTE BELOW!

  TDWordArray = array [0..(MaxInt div SizeOf(DWord)) - 1] of DWord;
  PDWORDs = ^TDWordArray; // READ NOTE BELOW!

  TBooleanArray = array [0..(MaxInt div SizeOf(Boolean)) -1] of Boolean;
  PBooleans = ^TBooleanArray; // READ NOTE BELOW!

  TByteArray = array [0..(MaxInt div SizeOf(Byte)) -1] of Byte;
  PBytes = ^TByteArray; // READ NOTE BELOW!

  TSHORTArray = array [0..(MaxInt div SizeOf(SHORT)) - 1] of SHORT;
  PSHORTs = ^TSHORTArray; // READ NOTE BELOW!

  TFLOATArray = array [0..(MaxInt div SizeOf(FLOAT)) - 1] of FLOAT;
  PFLOATs = ^TFLOATArray; // READ NOTE BELOW!

  // Dxbx Note :
  // The above pointer-to-array types are only meant as a helper type for indexing.
  // (We use this because older Delphi's don't allow indexing of pointers.)
  // But be warned! Never do pointer arithmitic with these types!
  // If you do, the unit of calculation used by the compiler is equal to the size of
  // the array. A simple "Inc(PFLOATs(p))" would compile into "add esi, $7FFFFFFC",
  // which is not quite what we meant! In these cases write "Inc(PFLOAT(p))" (see? no 's'!)
  // that would compile into the following correct assembly : "add esi, 4".

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
  // IMPORTANT! Defining PUInt8 as 'PBytes' led to increments with whole arrays at a time!
  PUInt8 = PByte; // TODO : Find a better solution for older Delphi's to index this type (array [0..0] without bounds-checking?)

  UInt08 = UInt8;
  PUInt08 = PUInt8;
  PPUInt08 = ^PUInt08;

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
  {$IFDEF USE_WINDOWS_TYPES}
    PUINT = Windows.PUINT;
  {$ELSE}
    PUINT = ^UINT;
  {$ENDIF}
  PUINTs = PUINT; // Dxbx addition, to give older Delphis an indexable type
{$ELSE}
  TUINTArray = array [0..MaxInt div SizeOf(UINT)-1] of UINT;
  PUINTs = ^TUINTArray; // Dxbx addition, to give older Delphis an indexable type
{$ENDIF}

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
  PPvoid = JwaWinType.PPVOID; // = ^PVOID;
  PLPVOID = System.PPointer;
  LONG = JwaWinType.LONG; // = Longint;
  INT = JwaWinType.INT; // = Integer;
  size_t = JwaWinType.size_t; // = Longword;
  wchar_t = JwaWinType.wchar_t; // = WideChar;
  FLOAT = JwaWinType.FLOAT;

  pwchar_t = PWideChar;

  PFILE = ^THandle;

  _char = AnsiChar; // Use this to translate Cxbx's "char" (because newer Delphi's Char = WideChar)
  P_char = PAnsiChar; // Use this to translate Cxbx's "char *" (because newer Delphi's PChar = PWideChar)
  PP_char = ^P_char;

  _bool = Boolean; // Use this to translate Cxbx's "bool" (because Delphi is not case sensitive)
  P_bool = ^_bool;

  // TODO : How should we translate Cxbx's "boolean" (lowercase) to Delphi? Boolean?

  // LONGBOOL = ?;
  _BOOLEAN = ByteBool; // Use this to translate Cxbx's "BOOLEAN" (because Delphi is not case sensitive)
  P_BOOLEAN = ^_BOOLEAN;
  // From Cxbx EmuXapi.h : "typedef unsigned char BOOLEAN" (unsigned char = AnsiChar in Delphi)

  BOOL_ = LongBool; // Use this when Dxbx is better of with logical-BOOL (as opposed to int-BOOL below)
  BOOL = int; // Use this to translate Cxbx's "BOOL" to Delphi (pending casing, _bool might be better)

  // See http://blog.delphi-jedi.net/2008/09/25/bool-boolean-and-integer/
  // and http://discuss.joelonsoftware.com/default.asp?joel.3.355854.11

(* From http://www.codeguru.com/forum/showthread.php?t=332831
Q: What is the difference between 'BOOL' and 'bool'?
A: 'bool' is a built-in C++ type while 'BOOL' is a Microsoft specific type that is defined as an 'int'.
You can find it in 'windef.h':
Code:
  typedef int                 BOOL;

  #ifndef FALSE
  #define FALSE               0
  #endif

  #ifndef TRUE
  #define TRUE                1
  #endif

The only possible values for a 'bool' are 'true' and 'false', whereas for 'BOOL' you can
use any 'int' value, though 'TRUE' and 'FALSE' macros are defined in 'windef.h' header.

Q: What is the size of 'BOOL' and 'bool'?
A: If you use the 'sizeof' operator, it will yield 1 for 'bool',
   though according to the standard the size of' bool' is implementation defined,
   and 4 for 'BOOL', on 32-bits platform, where 'sizeof(int)' is 4 bytes.
   If the size of 'int' changes to 8 bytes on 64-bits platforms, 'sizeof(BOOL)' will yield 8 instead.

Q: Does 'BOOL' has to do with MFC?
A: 'BOOL' was used by Microsoft long before 'bool' was actually added to the C++ language,
   but it has nothing to do with MFC. Many Windows API returns a 'BOOL' to indicate success or failure.

Q: Is it OK if I test the return of a Windows SDK function against 'TRUE'?
Code:
  BOOL ret = SomeSDKFunction();
  if(TRUE == ret)
  {
    // do something
  }
A: Actually no, it is not OK. If you read the documentation for APIs you can see that for most of them:

Quote:
Originally Posted by MSDN
If the function succeeds, the return value is nonzero.

If the function fails, the return value is zero. To get extended error information, call GetLastError().
'TRUE' is defined as 1, but nothing guarantees that the function actually returns 'TRUE' (a value that is
nonzero) or some other value. In fact there are functions that indeed return multiple values like –1, 0 or 1.

Here are several examples:

Code:
  BOOL EnableMenuItem(HMENU hMenu, UINT uIDEnableItem, UINT uEnable );

This function returns 4 possible values.

Quote:
  Originally Posted by MSDN
  The return value specifies the previous state of the menu item (it is either MF_DISABLED,
  MF_ENABLED, or MF_GRAYED). If the menu item does not exist, the return value is -1.

Code:
  BOOL GetMessage(LPMSG lpMsg, HWND hWnd, UINT wMsgFilterMin, UINT wMsgFilterMax );

Quote:
  Originally Posted by MSDN
  If the function retrieves a message other than WM_QUIT, the return value is nonzero.
  If the function retrieves the WM_QUIT message, the return value is zero.
  If there is an error, the return value is -1.

This is another example that other values than 0 and 1 can be used as a 'BOOL'.

So you should code it like:

Code:
  BOOL ret = SomeWinAPI();
  if(ret)
  {
    // do something
  }

And for the functions that are know to return values like -1 you also must take that into account.
For instance in the case of 'GetMessage()':

Code:
  BOOL ret = GetMessage(...);
  if(ret == 0)
  {
    // WM_QUIT
  }
  else if(ret == -1)
  {
    // error
  }
  else
  {
    // success
  }

*)

const
  BOOL_TRUE = BOOL(1);
  BOOL_FALSE = BOOL(0);

const
  HNULL = 0;

{$IFNDEF UNICODE}
type
  UnicodeString = WideString;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; inline;
{$ENDIF}

function tolower(c: AnsiChar): AnsiChar;
function toupper(c: AnsiChar): AnsiChar;
function isdigit(c: AnsiChar): Boolean; overload;
function IsDigit(c: WideChar): Boolean; overload;
function isxdigit(c: AnsiChar): Boolean;

function strcpy(dest, source: PAnsiChar): PAnsiChar; // cdecl
function strncpy(dest, source: PAnsiChar; len: Integer): PAnsiChar; overload; // cdecl
function strncpy(dest, source: PWideChar; len: Integer): PWideChar; overload; // cdecl
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
procedure fflush(stream: PFILE);

function FIELD_OFFSET(var Variable): DWORD;
function DWord2Str(const aValue: DWORD): string;
function PCharToString(const aPtr: PAnsiChar; const aLen: Integer): AnsiString;
function PWideCharToString(const aPtr: PWideChar; const aLen: Integer): string;
function PByteToHexString(const aPtr: PByte; const aLen: Integer): string;
function IsPrintableChar(const aChar: AnsiChar): Boolean;
function IsPrintableAsciiChar(const aChar: AnsiChar): Boolean;
function ByteLength(const aStr: WideString): Integer;

function LastChar(const aStr: string): Char;

function ToFLOAT(const aValue: DWORD): FLOAT; overload;
function ToFLOAT(const aValue: SHORT): FLOAT; overload;

var
  stdout: PFILE = nil;

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

function isdigit(c: AnsiChar): Boolean; // overload;
begin
  Result := c in ['0'..'9'];
end;

function IsDigit(c: WideChar): Boolean; // overload;
begin
  Result := CharInSet(c, ['0'..'9']);
end;

function isxdigit(c: AnsiChar): Boolean;
begin
  Result := c in ['0'..'9', 'A'..'Z', 'a'..'z'];
end;

function strcpy(dest, source: PAnsiChar): PAnsiChar; // cdecl
begin
  Result := StrCopy(Dest, Source);
end;

function strncpy(dest, source: PAnsiChar; len: Integer): PAnsiChar; // cdecl
begin
  Result := StrLCopy(Dest, Source, Len);
end;

function strncpy(dest, source: PWideChar; len: Integer): PWideChar; // cdecl
begin
  Result := StrLCopy(Dest, Source, Len);
end;

function wstrlen(const Str: PWideChar): Cardinal;
begin
  Result := Length(WideString(Str)); // TODO -oDxbx: Do a faster #0000 search here!
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
    Result := nil;
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

procedure fflush(stream: PFILE);
begin
  if Assigned(stream) then
    FlushFileBuffers(stream^);
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
    Result[1 + i + i] := HexNibble[1 + (PBytes(aPtr)[i] shr 4)];
    Result[2 + i + i] := HexNibble[1 + (PBytes(aPtr)[i] and 15)];
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

function ToFLOAT(const aValue: DWORD): FLOAT;
begin
  Result := 0.0 + aValue;
end;

function ToFLOAT(const aValue: SHORT): FLOAT;
begin
  Result := 0.0 + aValue;
end;

initialization

  // Delphi boolean types :
  Assert(SizeOf(Boolean) = 1);
  Assert(SizeOf(ByteBool) = 1);
  Assert(SizeOf(LongBool) = 4);

  // C boolean types :
  Assert(SizeOf(_bool) = 1); // Same as in VisualStudio C++ : http://msdn.microsoft.com/en-us/library/tf4dy80a.aspx
  Assert(SizeOf(_BOOLEAN) = 1);
  Assert(SizeOf(BOOL_) = 4);
  Assert(SizeOf(BOOL) = 4);

end.
