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
unit uDxbxUtils;

interface

uses
  // Delphi
  Windows,
  SysUtils,
  Classes,
  Graphics,
  // Dxbx
  uTypes,
  uEmuD3D8Types;

const
  NUMBER_OF_THUNKS = 379;

  DXBX_CONSOLE_DEBUG_FILENAME = 'DxbxDebug.txt';
  DXBX_KERNEL_DEBUG_FILENAME = 'KrnlDebug.txt';

type
  EMU_STATE = (esNone, esFileOpen, esRunning);

  TDebugInfoType = (ditConsole, ditFile);
  EnumAutoConvert = (CONVERT_TO_MANUAL, CONVERT_TO_XBEPATH, CONVERT_TO_WINDOWSTEMP);

  TDebugMode = (dmNone, dmConsole, dmFile);

  TEntryProc = procedure();
  PEntryProc = ^TEntryProc;

  TSetXbePath = procedure(const Path: PAnsiChar); cdecl;

  TKernelThunkTable = packed array[0..NUMBER_OF_THUNKS - 1] of UIntPtr;
  PKernelThunkTable = ^TKernelThunkTable;

  TGetKernelThunkTable = function: PKernelThunkTable; cdecl;

  TLineCallback = function (aLinePtr: PAnsiChar; aLength: Integer; aData: Pointer): Boolean;

procedure SetFS(const aNewFS: WORD);
function GetFS(): WORD;
function GetTIBEntry(const aOffset: DWORD): Pointer;
function GetTIBEntryWord(const aOffset: DWORD): WORD;
function GetTIB(): Pointer;

procedure ScanPCharLines(const aPChar: PAnsiChar; const aLineCallback: TLineCallback; const aCallbackData: Pointer);

function ScanHexByte(aLine: PAnsiChar; var Value: Integer): Boolean;
function ScanHexWord(aLine: PAnsiChar; var Value: Integer): Boolean;
function ScanHexDWord(aLine: PAnsiChar; var Value: Integer): Boolean;

function Sscanf(const s: AnsiString; const fmt: AnsiString; const Pointers: array of Pointer): Integer;

function StrLenLimit(Src: PChar; MaxLen: Cardinal): Cardinal;
function StrLPas(const aPChar: PAnsiChar; const aMaxLength: Integer): AnsiString;

function iif(aTest: Boolean; const aTrue, aFalse: Integer): Integer; overload;
function iif(aTest: Boolean; const aTrue, aFalse: string): string; overload;

function FindFiles(const aFolder, aFileMask: TFileName; aFileNames: TStrings): Integer;

function StartsWithText(const aString, aPrefix: string): Boolean;

procedure Swap(var aElement1, aElement2); overload;
function RoundUp(dwValue, dwMult: DWord): DWord;

function FixInvalidFilePath(const aFilePath: string): string;

function RecapitalizeString(const aString: string): string;

function DebugModeToString(const aDebugMode: TDebugMode): string;

function IsValidHandle(const aHandle: LongWord): Boolean;
function IsValidLibraryHandle(const aHandle: LongWord): Boolean;

function GetLastErrorString: string;
function GetErrorString(const aError: DWord): string;

function PointerToString(const aPointer: Pointer): string;

type
  // Free interpretation of http://edn.embarcadero.com/article/29173
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  PRGB32 = ^TRGB32;

  TRGB32Array = packed array[0..MaxInt div SizeOf(TRGB32)-1] of TRGB32;
  PRGB32Array = ^TRGB32Array;

  RGB32Scanlines = record
  private
    FScanLines: array of PRGB32Array;
    FWidth: Cardinal;
    function GetScanline(Row: Integer): PRGB32Array;
    function GetPixel(Col, Row: Integer): PRGB32;
    function GetHeight: Cardinal;
  public
    procedure Initialize(const aBitmap: TBitmap);

    property Scanlines[Row: Integer]: PRGB32Array read GetScanline; default;
    property Pixels[Col, Row: Integer]: PRGB32 read GetPixel;

    property Height: Cardinal read GetHeight;
    property Width: Cardinal read FWidth;
  end;
  PRGB32Scanlines = ^RGB32Scanlines;

type
  TRGB16 = WORD;
  PRGB16 = ^TRGB16;

  TRGB16Array = packed array[0..MaxInt div SizeOf(TRGB16)-1] of TRGB16;
  PRGB16Array = ^TRGB16Array;

  RGB16Scanlines = record
  private
    FScanLines: array of PRGB16Array;
    FWidth: Cardinal;
    function GetScanline(Row: Integer): PRGB16Array;
    function GetPixel(Col, Row: Integer): PRGB16;
    function GetHeight: Cardinal;
  public
    procedure Initialize(const aBitmap: TBitmap);

    property Scanlines[Row: Integer]: PRGB16Array read GetScanline; default;
    property Pixels[Col, Row: Integer]: PRGB16 read GetPixel;

    property Height: Cardinal read GetHeight;
    property Width: Cardinal read FWidth;
  end;
  PRGB16Scanlines = ^RGB16Scanlines;


function ReadS3TCFormatIntoBitmap(const aFormat: Byte; const aData: PByteArray; const aDataSize: Cardinal; const aOutput: PRGB32Scanlines): Boolean;
function ReadSwizzledFormatIntoBitmap(const aFormat: Byte; const aData: PByteArray; const aDataSize: Cardinal; const aOutput: PRGB32Scanlines): Boolean;
function ReadD3DTextureFormatIntoBitmap(const aFormat: Byte; const aData: PByteArray; const aDataSize: Cardinal; const aOutput: PRGB32Scanlines): Boolean;

implementation

{$STACKFRAMES OFF}

procedure SetFS(const aNewFS: WORD);
asm
  MOV FS, aNewFS
end;

function GetFS(): WORD;
asm
  XOR EAX, EAX
  MOV AX, FS
end;

function GetTIBEntry(const aOffset: DWORD): Pointer;
asm
  MOV EAX, FS:[aOffset]
end;

function GetTIBEntryWord(const aOffset: DWORD): WORD;
asm
  MOV AX, FS:[aOffset]
end;

function GetTIB(): Pointer;
begin
  Result := GetTIBEntry({FS_Self=}$18);
end;

{$STACKFRAMES ON}

function FixInvalidFilePath(const aFilePath: string): string;
var
  i: Integer;
begin
  Result := aFilePath;
  for i := 1 to Length(Result) do
  begin
    case AnsiChar(Result[i]) of
      #0..#31, #127:
        Result[i] := ' ';
      '/', '\':
        Result[i] := '_';
      ':':
        Result[i] := ';';
      '*':
        Result[i] := '•';
      '?':
        Result[i] := '¿';
      '"':
        Result[i] := '”';
      '<':
        Result[i] := '‹';
      '>':
        Result[i] := '›';
      '|':
        Result[i] := '¦';
    end;
  end;
end;

function RecapitalizeString(const aString: string): string;

  procedure _ToUpper(aIndex: Integer);
  begin
    if Result[aIndex] in ['a'..'z'] then
      Result[aIndex] := Char(Ord(Result[aIndex]) - $20);
  end;

  procedure _ToLower(aIndex, aEndIndex: Integer);
  begin
    while aIndex <= aEndIndex do
    begin
      if Result[aIndex] in ['A'..'Z'] then
        Result[aIndex] := Char(Ord(Result[aIndex]) + $20);

      Inc(aIndex);
    end;
  end;

var
  i, j: Integer;
  NrOfChars: Integer;
  NrOfUppercase: Integer;
  DoOutput: Boolean;
begin
  // Start with input :
  Result := Trim(aString);

  // Insert spaces everywhere a uppercase follows a lowercase character :
  i := Length(Result);
  while i > 1 do
  begin
    if ((Result[i] in ['A'..'Z']) and (Result[i - 1] in ['a'..'z']))
    or ((Result[i] in ['0'..'9']) and (Result[i - 1] in [':'..'z']))
    or ((Result[i] in [':'..'z']) and (Result[i - 1] in ['0'..'9'])) then
      Insert(' ', Result, i);

    Dec(i);
  end;

  // Count all characters (uppercase separately) :
  j := 1;
  NrOfChars := 0;
  NrOfUppercase := 0;
  for i := 1 to Length(Result) do
  begin
    DoOutput := (i = Length(Result));
    case AnsiChar(Result[i]) of
      '''':
        ; // Do nothing - ' can be part of a word

      'a'..'z':
        Inc(NrOfChars);

      'A'..'Z':
      begin
        Inc(NrOfChars);
        Inc(NrOfUppercase);
      end;
    else
      DoOutput := True;
    end;

    if DoOutput then
    begin
      while Result[j] = ' ' do
        Inc(j);
      
      // Very small words go to all-lowercase:
      if NrOfChars <= 2 then
        _ToLower(j, i)
      else
        // All-uppercase, up to 3 characters, stays that way :
        if (NrOfUpperCase = NrOfChars) and (NrOfChars <= 3) then
          // do nothing
        else
        begin
          // The rest goes to Camel Caps :
          _ToUpper(j);
          _ToLower(j + 1, i);
        end;

      j := i + 1;
      NrOfChars := 0;
      NrOfUppercase := 0;
    end;
  end; // for
end;

procedure Swap(var aElement1, aElement2);
var
  Tmp: Pointer;
begin
  Tmp := Pointer(aElement1);
  Pointer(aElement1) := Pointer(aElement2);
  Pointer(aElement2) := Tmp;
end;

function RoundUp(dwValue, dwMult: DWord): DWord;
begin
  if dwMult = 0 then
    Result := dwValue
  else
    Result := dwValue - ((dwValue - 1) mod dwMult) + (dwMult - 1);
end;

function StartsWithText(const aString, aPrefix: string): Boolean;
begin
  Result := AnsiStrLIComp(PChar(aString), PChar(aPrefix), Length(aPrefix)) = 0;
end;

function FindFiles(const aFolder, aFileMask: TFileName; aFileNames: TStrings): Integer;
var
  Status: Integer;
  SearchRec: TSearchRec;
begin
  with aFileNames do
  begin
    BeginUpdate;
    try
      Clear;
      Status := FindFirst(IncludeTrailingPathDelimiter(aFolder) + aFileMask, faAnyFile, SearchRec);
      while Status = 0 do
      begin
        if (SearchRec.Attr and faDirectory) = 0 then
          Add(IncludeTrailingPathDelimiter(aFolder) + SearchRec.Name);

        Status := FindNext(SearchRec);
      end;

      FindClose(SearchRec);
    finally
      EndUpdate;
    end;

    Result := Count;
  end;
end;

function _ScanAndAddHexDigit(var Value: Integer; const aHexDigit: AnsiChar): Boolean;
begin
  Result := True;
  case aHexDigit of
    '0'..'9':
      Value := (Value * 16) + (Ord(aHexDigit) - Ord('0'));
    'A'..'F':
      Value := (Value * 16) + (Ord(aHexDigit) - Ord('A') + 10);
    'a'..'f':
      Value := (Value * 16) + (Ord(aHexDigit) - Ord('a') + 10);
  else
    Result := False;
  end;
end;

function _ScanHexDigits(aLine: PAnsiChar; var Value: Integer; Digits: Integer): Boolean;
begin
  Value := 0;
  while Digits > 0 do
  begin
    Result := _ScanAndAddHexDigit(Value, aLine^);
    if not Result then
      Exit;

    Inc(aLine);
    Dec(Digits);
  end;

  Result := True;
end;

function ScanHexByte(aLine: PAnsiChar; var Value: Integer): Boolean;
begin
  Result := _ScanHexDigits(aLine, Value, 2);
end;

function ScanHexWord(aLine: PAnsiChar; var Value: Integer): Boolean;
begin
  Result := _ScanHexDigits(aLine, Value, 4);
end;

function ScanHexDWord(aLine: PAnsiChar; var Value: Integer): Boolean;
begin
  Result := _ScanHexDigits(aLine, Value, 8);
end;

procedure ScanPCharLines(const aPChar: PAnsiChar; const aLineCallback: TLineCallback; const aCallbackData: Pointer);
var
  p1, p2: PAnsiChar;
begin
  // Scan Lines:
  p1 := aPChar;
  while p1^ > #0 do
  begin
    // Scan this line until end of line (#0..#13) :
    p2 := p1;
    while p2^ > #13 do
      Inc(p2);

    // Handle this line :
    if not aLineCallback(p1, {Length=}p2-p1, aCallbackData) then
      Exit;

    // Step over to the start of the next line :
    p1 := p2 + 1;
    while p1^ in [#10, #13] do
      Inc(p1);
  end;
end;

function Sscanf(const s: AnsiString; const fmt: AnsiString; const Pointers: array of Pointer): Integer;
var
  i, j, n, m: Integer;
  s1: AnsiString;
  L: LongInt;
  X: Extended;

  function GetInt: Integer;
  begin
    s1 := '';
    while (n <= Length(s)) and (s[n] = ' ') do
      Inc(n);

    while (n <= Length(s))
      and (s[n] in ['0'..'9', '+', '-']) do
    begin
      s1 := s1 + s[n];
      Inc(n);
    end;

    Result := Length(s1); 
  end; 

  function GetFloat: Integer; 
  begin 
    s1 := ''; 
    while (n <= Length(s)) and (s[n] = ' ') do
      Inc(n);

    while (s[n] in ['0'..'9', '+', '-', '.', 'e', 'E'])
      and (Length(s) >= n) do
    begin
      s1 := s1 + s[n];
      Inc(n);
    end;

    Result := Length(s1);
  end;

  function GetString: Integer;
  begin
    s1 := '';
    while (n <= Length(s)) and (s[n] = ' ') do
      Inc(n);

    while (n <= Length(s)) and (s[n] <> ' ') do
    begin
      s1 := s1 + s[n];
      Inc(n);
    end;

    Result := Length(s1);
  end;

  function ScanStr(c: AnsiChar): Boolean;
  begin
    while (n <= Length(s)) and (s[n] <> c) do
      Inc(n);

    Inc(n);

    if (n <= Length(s)) then
      Result := True
    else
      Result := False; 
  end; 

  function GetFmt: Integer; 
  begin 
    Result := -1; 

    while True do 
    begin
      while (m <= Length(fmt)) and (fmt[m] = ' ') do
        Inc(m);

      if m >= Length(fmt) then 
        Break;

      if fmt[m] = '%' then
      begin 
        Inc(m); 
        case fmt[m] of
          'd': Result := vtInteger;
          'f': Result := vtExtended;
          's': Result := vtString;
        end;

        Inc(m);
        Break;
      end;

      if (ScanStr(fmt[m]) = False) then
        Break;

      Inc(m);
    end; 
  end; 

begin 
  n := 1; 
  m := 1; 
  Result := 0; 

  for i := 0 to High(Pointers) do 
  begin 
    j := GetFmt; 

    case j of 
      vtInteger: 
        begin 
          if GetInt > 0 then 
          begin 
            L := StrToInt(string(s1));
            Move(L, Pointers[i]^, SizeOf(LongInt));
            Inc(Result);
          end
          else
            Break;
        end;

      vtExtended:
        begin
          if GetFloat > 0 then
          begin
            X := StrToFloat(string(s1));
            Move(X, Pointers[i]^, SizeOf(Extended));
            Inc(Result); 
          end 
          else 
            Break; 
        end; 

      vtString: 
        begin 
          if GetString > 0 then 
          begin 
            Move(s1, Pointers[i]^, Length(s1) + 1); 
            Inc(Result);
          end
          else
            Break;
        end;
    else
      Break;
    end;
  end;
end;

// Stupid Delphi has this hidden in the implementation section of SysUtils;
// StrLenLimit:  Scan Src for a null terminator up to MaxLen bytes
function StrLenLimit(Src: PChar; MaxLen: Cardinal): Cardinal;
begin
  if Src = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := MaxLen;
  while (Src^ <> #0) and (Result > 0) do
  begin
    Inc(Src);
    Dec(Result);
  end;
  Result := MaxLen - Result;
end;

function StrLPas(const aPChar: PAnsiChar; const aMaxLength: Integer): AnsiString;
var
  Len: Integer;
begin
  Len := StrLenLimit(aPChar, aMaxLength);
  SetLength(Result, Len);
  Move(aPChar[0], Result[1], Len * SizeOf(AnsiChar));
end;

function iif(aTest: Boolean; const aTrue, aFalse: Integer): Integer; overload;
begin
  if aTest then
    Result := aTrue
  else
    Result := aFalse;
end;

function iif(aTest: Boolean; const aTrue, aFalse: string): string; overload;
begin
  if aTest then
    Result := aTrue
  else
    Result := aFalse;
end;

function PointerToString(const aPointer: Pointer): string;
begin
  Result := IntToHex(Integer(aPointer), 8);
end;

function DebugModeToString(const aDebugMode: TDebugMode): string;
begin
  case aDebugMode of
    dmNone: Result := 'DM_NONE';
    dmConsole: Result := 'DM_CONSOLE';
    dmFile: Result := 'DM_FILE';
  else
    Result := '?Unknown?';
  end;
end;

function IsValidHandle(const aHandle: LongWord): Boolean;
begin
  Result := (aHandle <> INVALID_HANDLE_VALUE);
end;

// (Safe)LoadLibrary returns 32 or greater on a succesfull call.
// See http://support.microsoft.com/kb/142814 for details.
function IsValidLibraryHandle(const aHandle: LongWord): Boolean;
begin
  Result := IsValidHandle(aHandle) and (aHandle >= 32);
end;

function GetLastErrorString: string;
begin
  Result := GetErrorString(GetLastError);
end;

function GetErrorString(const aError: DWord): string;
begin
  Result := SysErrorMessage(aError);
  if Result = '' then
    Result := 'No description for error #' + IntToStr(aError)
  else
    Result := Result + ' (#' + IntToStr(aError) + ')';
end;

{ RGB32Scanlines }

procedure RGB32Scanlines.Initialize(const aBitmap: TBitmap);
var
  y: Integer;
begin
  Assert(Assigned(aBitmap) and (aBitmap.PixelFormat = pf32bit));

  FWidth := aBitmap.Width;
  SetLength(FScanLines, aBitmap.Height);
  for y := 0 to aBitmap.Height - 1 do
    FScanlines[y] := aBitmap.Scanline[y];
end;

function RGB32Scanlines.GetScanline(Row: Integer): PRGB32Array;
begin
  Result := FScanlines[Row];
end;

function RGB32Scanlines.GetPixel(Col, Row: Integer): PRGB32;
begin
  Result := @(FScanlines[Row][Col]);
end;

function RGB32Scanlines.GetHeight: Cardinal;
begin
  Result := Length(FScanlines);
end;

{ RGB16Scanlines }

procedure RGB16Scanlines.Initialize(const aBitmap: TBitmap);
var
  y: Integer;
begin
  Assert(Assigned(aBitmap) and (aBitmap.PixelFormat = pf16bit));

  FWidth := aBitmap.Width;
  SetLength(FScanLines, aBitmap.Height);
  for y := 0 to aBitmap.Height - 1 do
    FScanlines[y] := aBitmap.Scanline[y];
end;

function RGB16Scanlines.GetScanline(Row: Integer): PRGB16Array;
begin
  Result := FScanlines[Row];
end;

function RGB16Scanlines.GetPixel(Col, Row: Integer): PRGB16;
begin
  Result := @(FScanlines[Row][Col]);
end;

function RGB16Scanlines.GetHeight: Cardinal;
begin
  Result := Length(FScanlines);
end;

//

// Unswizzles a texture.
// Copied from:http://www.koders.com/cpp/fid2E8F6D4B34729B4E9C081DD26D63648187C55EBF.aspx?s=unswizzle#L4
// Parameters: width, height and depth (4 = 32bit, 1 = 8bit) and arrays.
procedure UnSwizzle(width, height, depth: Cardinal;
  const source: PByteArray;
  const dest: PByteArray);
var
  y, y_mask, x_mask, sy, sx, bit, d, x, pSource, i: Cardinal;
begin
  for y := 0 to height - 1 do
  begin
    sy := 0;
    if y < width then
    begin
      for bit := 0 to 15 do
        sy := sy or ((y shr bit) and 1) shl (2 * bit);

     sy := sy shl 1;
    end
    else
    begin
      y_mask := y mod width;
      for bit := 0 to 15 do
        sy := sy or ((y_mask shr bit) and 1) shl (2 * bit);

      sy := sy shl 1;
      sy := sy + (y div width) * width * width;
    end;

    d := y * width * depth;

    for x := 0 to width - 1 do
    begin
      sx := 0;
      if x < (height * 2) then
      begin
        for bit := 0 to 15 do
          sx := sx or (( x shr bit) and 1 ) shl (2 * bit);
     end
     else
     begin
       x_mask := x mod (2 * height);
       for bit := 0 to 15 do
         sx := sx or ((x_mask shr bit ) and 1) shl (2 * bit);

       sx := sx + (x div (2 * height)) * 2 * height * height;
     end;

     pSource := (sx + sy) * depth;
     for i := 0 to depth - 1 do
       dest[d + i] := source[pSource+i]
    end;
  end;
end;

(* The above (Unswizzle) doesn't seem to work. Here two other implementations I (PatrickvL) found :

// Source : http://forums.xbox-scene.com/lofiversion/index.php/t409716.html
procedure UnswizBlock(
  const InData: PByteArray;
  const InSize: Cardinal;
  const OutData: PByteArray;
  var Offs: Cardinal;
  OffsOut: Cardinal;
  const BlWidth: Cardinal;
  const BlHeight: Cardinal;
  const Stride: Cardinal);
begin
  if (Offs > InSize) then
    Exit;

  if (BlWidth < 2) or (BlHeight < 2) then
  begin
    // just copy data here
    CopyMemory(OutData[OffsOut], InData[Offs], BlWidth * BlHeight);
//    OffsOut := OffsOut + BlWidth * BlHeight;
  end
  else if (BlWidth = 2) and (BlHeight = 2) then
  begin
    // unswizzle block
    OutData[OffsOut] := InData[Offs];
    OutData[OffsOut + 1] := InData[Offs + 1];
    OutData[OffsOut + Stride] := InData[Offs + 2];
    OutData[OffsOut + Stride + 1] := InData[Offs + 3];
    Inc({var}Offs, 4);
  end
  else
  begin
    // break into 4 blocks and reprocess
    UnswizBlock(InData, InSize, OutData, {var}Offs, OffsOut, BlWidth div 2, BlHeight div 2, Stride);
    UnswizBlock(InData, InSize, OutData, {var}Offs, OffsOut + (BlWidth div 2), BlWidth div 2, BlHeight div 2, Stride);
    UnswizBlock(InData, InSize, OutData, {var}Offs, OffsOut + (Stride * (BlHeight div 2)), BlWidth div 2, BlHeight div 2, Stride);
    UnswizBlock(InData, InSize, OutData, {var}Offs, OffsOut + (Stride * (BlHeight div 2)) + (BlWidth div 2), BlWidth div 2, BlHeight div 2, Stride);
  end;
end;

function Unswizzle(aWidth, aHeight: Cardinal; bytData: PByteArray; DataSize: Cardinal): PByte;
var
  lOffs: Cardinal;
  i: Cardinal;
  MipLevels: Cardinal;
begin
  Result := AllocMem(DataSize);

  if aHeight > aWidth then
    MipLevels := Log(aHeight) / Log(2) + 1
  else
    MipLevels := Log(aWidth) / Log(2) + 1

  lOffs := 0;
  for i := 1 To MipLevels do
  begin
    UnswizBlock(bytData, DataSize, Result, {var}lOffs, lOffs, aWidth, aHeight, Width);
    aWidth := aWidth div 2;
    aHeight := aHeight div 2;
    if lOffs > DataSize then
      Exit;
  end;
end;


// http://www.modnexus.com/forums/viewtopic.php?p=631514&sid=e03fe94600009729bbfb7412997e0981#p631514
// pokecancer » June 27th, 2007, 5:15 am
// I figured I would release my deswizzle method for A8R8G8B8 so people can do more research on bitmaps.
// This applies to not only H3 but most game's resources for Xbox 360 and could probably be easily
// adjusted to deal with different argb formats by changing some of the math in my code.
// BTW this is using my own modified BinaryReader class so you will have to change that if you want to use this code.
// also keep in mind the data is in big endian format and need to 32 bit swapped in order
// to be in a format the pc can understand.
public static byte[] Deswizzle(byte[] raw, int width, int height)
begin
  MemoryStream ms := new MemoryStream();
  core.IO.EndianIO.EndianWriter ew := new core.IO.EndianIO.EndianWriter(ms, core.IO.EndianType.LittleEndian);
  ew.BaseStream.Position := 0;

  int realsize := width * height * 4;
  int lines := realsize div 16;

  int templine := 0;
  int rowcount := 0;
  int offsetby := 0;
  int i := 0;
  int sections := width div 32;

  for rowcount := 0 to (2 * lines) - 1 do
  begin
    if rowcount = height then
      Break;

    if (rowcount and 1) = 0 then // 0, 2, 4
    begin
      if (rowcount > 0) then
      begin
        if (rowcount and 31) = 0) then
        begin
          Inc(i);
          templine := i * ((width * 32) div 4);
        end;

        if (rowcount and 7) = 0 then
        begin
          if offsetby = 0 then
            offsetby := 8 * 16
          else
            offsetby := 0;
        end;
      end;

      offsetbx := 0;
    end
    else
    begin
      if (rowcount > 0) then
        Inc(templine, 16);

      offsetbx := 16;
    end;

    for y := 0 to sections - 1 do
    begin
      //get 32 pixels
      for z := 0 to 8 - 1 do
      begin
        offset := (templine * 16) + offsetbx + ((y * 256) * 16) + ((z * 2) * 16);
        if z < 4 then
          Inc(offset, offsetby)
        else
          Dec(offset, offsetby);

        oi1 := BitConverter.ToInt32(raw, offset + 0);
        oi2 := BitConverter.ToInt32(raw, offset + 4);
        oi3 := BitConverter.ToInt32(raw, offset + 8);
        oi4 := BitConverter.ToInt32(raw, offset + 12);

        ew.Write(oi1);
        ew.Write(oi2);
        ew.Write(oi3);
        ew.Write(oi4);
      end; // for z
    end; // for y
  end; // for rowcount

  return ms.ToArray();
end;
*)

function ReadSwizzledFormatIntoBitmap(
  const aFormat: Byte;
  const aData: PByteArray;
  const aDataSize: Cardinal;
  const aOutput: PRGB32Scanlines): Boolean;
var
  Unswizzled: array of Byte;
  j, x, y: Cardinal;
begin
  Result := (aFormat in [X_D3DFMT_A8R8G8B8])
        and Assigned(aData)
        and (aDataSize > 0)
        and Assigned(aOutput)
        and (aOutput.Height > 0)
        and (aOutput.Width > 0);
  if not Result then
    Exit;

  SetLength(Unswizzled, aDataSize);
  UnSwizzle(aOutput.Width, aOutput.Height, {Depth=}4, aData, PByteArray(Unswizzled));

  j := 0;
  while j < aDataSize do
  begin
    x := (j div 4) mod aOutput.Width;
    y := (j div 4) div aOutput.Width;

    with aOutput.Pixels[x, y]^ do
    begin
      R := Unswizzled[j + 0];
      G := Unswizzled[j + 1];
      B := Unswizzled[j + 2];
    end;

    j := j + 4;
  end; // while
end; // ReadSwizzledFormatIntoBitmap

// Official spec : http://oss.sgi.com/projects/ogl-sample/registry/EXT/texture_compression_s3tc.txt
function ReadS3TCFormatIntoBitmap(
  const aFormat: Byte;
  const aData: PByteArray;
  const aDataSize: Cardinal;
  const aOutput: PRGB32Scanlines): Boolean;
var
  color: array[0..3] of Word;
  color32b: array[0..4] of TRGB32;

  r, g, b, r1, g1, b1, bitmap: DWord;

  j, p, x, y, xo, yo, ci: Cardinal;
begin
  Result := (aFormat in [X_D3DFMT_DXT1, X_D3DFMT_DXT3, X_D3DFMT_DXT5])
        and Assigned(aData)
        and (aDataSize > 0)
        and Assigned(aOutput)
        and (aOutput.Height > 0)
        and (aOutput.Width > 0);
  if not Result then
    Exit;
  
  j := 0;
  while j < aDataSize do
  begin
    // Skip X_D3DFMT_DXT3 and X_D3DFMT_DXT5 alpha data for now :
    if aFormat <> X_D3DFMT_DXT1 then
      Inc(j, 8);

    // Read two 16-bit pixels (let's call them A and B) :
    color[0] := (aData[j + 0] shl 0)
              + (aData[j + 1] shl 8);

    color[1] := (aData[j + 2] shl 0)
              + (aData[j + 3] shl 8);

    // Read 5+6+5 bit color channels and convert them to 8+8+8 bit :
    r := ((color[0] shr 11) and 31) * 255 div 31;
    g := ((color[0] shr  5) and 63) * 255 div 63;
    b := ((color[0]       ) and 31) * 255 div 31;

    r1 := ((color[1] shr 11) and 31) * 255 div 31;
    g1 := ((color[1] shr  5) and 63) * 255 div 63;
    b1 := ((color[1]       ) and 31) * 255 div 31;

    // Build first half of RGB32 color map :
    color32b[0].R := r;
    color32b[0].G := g;
    color32b[0].B := b;

    color32b[1].R := r1;
    color32b[1].G := g1;
    color32b[1].B := b1;

    // Build second half of RGB32 color map :
    if color[0] > color[1] then
    begin
      // Make up 2 new colors, 1/3 A + 2/3 B and 2/3 A + 1/3 B :
      color32b[2].R := (r + r + r1 + 2) div 3;
      color32b[2].G := (g + g + g1 + 2) div 3;
      color32b[2].B := (b + b + b1 + 2) div 3;

      color32b[3].R := (r + r1 + r1 + 2) div 3;
      color32b[3].G := (g + g1 + g1 + 2) div 3;
      color32b[3].B := (b + b1 + b1 + 2) div 3;
    end
    else
    begin
      // Make up one new color : 1/2 A + 1/2 B :
      color32b[2].R := (r + r1) div 2;
      color32b[2].G := (g + g1) div 2;
      color32b[2].B := (b + b1) div 2;

      color32b[3].R := 0;
      color32b[3].G := 0;
      color32b[3].B := 0;
    end;

    x := (j div 2) mod aOutput.Width;
    y := (j div 2) div aOutput.Width * 4;

    bitmap := (aData[j + 4] shl 0)
            + (aData[j + 5] shl 8)
            + (aData[j + 6] shl 16)
            + (aData[j + 7] shl 24);

    for p := 0 to 15 do
    begin
      xo := p mod 4;
      yo := p div 4;
      ci := (bitmap shr (p * 2)) and 3;

      aOutput.Pixels[x+xo, y+yo]^ := color32b[ci];
    end;

    Inc(j, 8);
  end; // while
end; // ReadS3TCFormatIntoBitmap

function ReadD3DTextureFormatIntoBitmap(
  const aFormat: Byte;
  const aData: PByteArray;
  const aDataSize: Cardinal;
  const aOutput: PRGB32Scanlines): Boolean;
begin
  case aFormat of
    X_D3DFMT_DXT1,
    X_D3DFMT_DXT3,
    X_D3DFMT_DXT5:
      // Read the compressed texture into the bitmap :
      Result := ReadS3TCFormatIntoBitmap(aFormat, aData, aDataSize, aOutput);

    X_D3DFMT_A8R8G8B8:
      // Read the swizzled texture into the bitmap :
      Result := ReadSwizzledFormatIntoBitmap(aFormat, aData, aDataSize, aOutput);

  else
    Result := False;
  end;
end;

end.

