{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclUnicode.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Mike Lischke (public att lischke-online dott de).  }
{ Portions created by Mike Lischke are Copyright (C) 1999-2000 Mike Lischke. All Rights Reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Andreas Hausladen (ahuser)                                                                     }
{   Mike Lischke                                                                                   }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Peter Schraut (http://www.console-dev.de)                                                      }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ String conversion routines                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-10-05 16:26:49 +0200 (zo, 05 okt 2008)                        $ }
{ Revision:      $Rev:: 2524                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStringConversions;

{$I jcl.inc}

interface

uses
  Classes,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase;

type
  EJclStringConversionError = class(EJclError);
  EJclUnexpectedEOSequenceError = class (EJclStringConversionError)
  public
    constructor Create;
  end;

// conversion routines between Ansi, UTF-16, UCS-4 and UTF8 strings

{$IFNDEF CLR}
// one shot conversion between PAnsiChar and PWideChar
procedure ExpandANSIString(const Source: PAnsiChar; Target: PWideChar; Count: Cardinal);
{$ENDIF ~CLR}

// tpye of stream related functions
type
  TJclStreamGetNextCharFunc = function(S: TStream; var Ch: UCS4): Boolean;
  TJclStreamSkipCharsFunc = function(S: TStream; var NbSeq: Integer): Boolean;
  TJclStreamSetNextCharFunc = function(S: TStream; Ch: UCS4): Boolean;

// iterative conversions

// UTF8GetNextChar = read next UTF8 sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF8GetNextChar(const S: TUTF8String; var StrPos: Integer): UCS4;
function UTF8GetNextCharFromStream(S: TStream; var Ch: UCS4): Boolean;

// UTF8SkipChars = skip NbSeq UTF8 sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbSeq contains the number of UTF8 sequences that were skipped
function UTF8SkipChars(const S: TUTF8String; var StrPos: Integer; var NbSeq: Integer): Boolean;
function UTF8SkipCharsFromStream(S: TStream; var NbSeq: Integer): Boolean;

// UTF8SetNextChar = append an UTF8 sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF8SetNextChar(var S: TUTF8String; var StrPos: Integer; Ch: UCS4): Boolean;
function UTF8SetNextCharToStream(S: TStream; Ch: UCS4): Boolean;

// UTF16GetNextChar = read next UTF16 sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF16GetNextChar(const S: TUTF16String; var StrPos: Integer): UCS4; overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16GetNextChar(const S: UnicodeString; var StrPos: Integer): UCS4; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
function UTF16GetNextCharFromStream(S: TStream; var Ch: UCS4): Boolean;

// UTF16GetPreviousChar = read previous UTF16 sequence starting at StrPos-1
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be decremented by the number of chars that were read
function UTF16GetPreviousChar(const S: TUTF16String; var StrPos: Integer): UCS4; overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16GetPreviousChar(const S: UnicodeString; var StrPos: Integer): UCS4; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}

// UTF16SkipChars = skip NbSeq UTF16 sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of UTF16 sequences that were skipped
function UTF16SkipChars(const S: TUTF16String; var StrPos: Integer; var NbSeq: Integer): Boolean; overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16SkipChars(const S: UnicodeString; var StrPos: Integer; var NbSeq: Integer): Boolean; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
function UTF16SkipCharsFromStream(S: TStream; var NbSeq: Integer): Boolean;

// UTF16SetNextChar = append an UTF16 sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-16 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF16SetNextChar(var S: TUTF16String; var StrPos: Integer; Ch: UCS4): Boolean; overload;
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16SetNextChar(var S: UnicodeString; var StrPos: Integer; Ch: UCS4): Boolean; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}
function UTF16SetNextCharToStream(S: TStream; Ch: UCS4): Boolean;

// AnsiGetNextChar = read next character at StrPos
// StrPos will be incremented by the number of chars that were read (1)
function AnsiGetNextChar(const S: AnsiString; var StrPos: Integer): UCS4;
function AnsiGetNextCharFromStream(S: TStream; var Ch: UCS4): Boolean;

// AnsiSkipChars = skip NbSeq characters starting from StrPos
// returns False if String is too small
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of UTF16 sequences that were skipped
function AnsiSkipChars(const S: AnsiString; var StrPos: Integer; var NbSeq: Integer): Boolean;
function AnsiSkipCharsFromStream(S: TStream; var NbSeq: Integer): Boolean;

// AnsiSetNextChar = append a character at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an ansi string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written (1)
function AnsiSetNextChar(var S: AnsiString; var StrPos: Integer; Ch: UCS4): Boolean;
function AnsiSetNextCharToStream(S: TStream; Ch: UCS4): Boolean;

// StringGetNextChar = read next character/sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence for WideString)
// StrPos will be incremented by the number of chars that were read
function StringGetNextChar(const S: string; var StrPos: Integer): UCS4; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

// StringSkipChars = skip NbSeq characters/sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence for WideString)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of UTF16 sequences that were skipped
function StringSkipChars(const S: string; var StrPos: Integer; var NbSeq: Integer): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

// StringSetNextChar = append a character/sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to a string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function StringSetNextChar(var S: string; var StrPos: Integer; Ch: UCS4): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

// one shot conversions between WideString and others
function WideStringToUTF8(const S: WideString): TUTF8String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF8ToWideString(const S: TUTF8String): WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function WideStringToUCS4(const S: WideString): TUCS4Array; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UCS4ToWideString(const S: TUCS4Array): WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

// one shot conversions between AnsiString and others
function AnsiStringToUTF8(const S: AnsiString): TUTF8String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF8ToAnsiString(const S: TUTF8String): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function AnsiStringToUTF16(const S: AnsiString): TUTF16String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF16ToAnsiString(const S: TUTF16String): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function AnsiStringToUCS4(const S: AnsiString): TUCS4Array; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UCS4ToAnsiString(const S: TUCS4Array): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

// one shot conversions between string and others
function StringToUTF8(const S: string): TUTF8String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF8ToString(const S: TUTF8String): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function StringToUTF16(const S: string): TUTF16String; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UTF16ToString(const S: TUTF16String): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function StringToUCS4(const S: string): TUCS4Array; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function UCS4ToString(const S: TUCS4Array): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

function UTF8ToUTF16(const S: TUTF8String): TUTF16String;
function UTF16ToUTF8(const S: TUTF16String): TUTF8String;
function UTF8ToUCS4(const S: TUTF8String): TUCS4Array;
function UCS4ToUTF8(const S: TUCS4Array): TUTF8String;
function UTF16ToUCS4(const S: TUTF16String): TUCS4Array;
function UCS4ToUTF16(const S: TUCS4Array): TUTF16String;

// indexed conversions
function UTF8CharCount(const S: TUTF8String): Integer;
function UTF16CharCount(const S: TUTF16String): Integer;
function UCS2CharCount(const S: TUCS2String): Integer;
function UCS4CharCount(const S: TUCS4Array): Integer;
// returns False if string is too small
// if UNICODE_SILENT_FAILURE is not defined and an invalid UTFX sequence is detected, an exception is raised
// returns True on success and Value contains UCS4 character that was read
function GetUCS4CharAt(const UTF8Str: TUTF8String; Index: Integer; out Value: UCS4): Boolean; overload;
function GetUCS4CharAt(const WideStr: TUTF16String; Index: Integer; out Value: UCS4; IsUTF16: Boolean = True): Boolean; overload;
function GetUCS4CharAt(const UCS4Str: TUCS4Array; Index: Integer; out Value: UCS4): Boolean; overload;

function UCS4ToAnsiChar(Value: UCS4): AnsiChar;
function UCS4ToWideChar(Value: UCS4): WideChar;
function UCS4ToChar(Value: UCS4): Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

function AnsiCharToUCS4(Value: AnsiChar): UCS4;
function WideCharToUCS4(Value: WideChar): UCS4;
function CharToUCS4(Value: Char): UCS4; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net/svnroot/jcl/trunk/jcl/source/common/JclStringConversions.pas $';
    Revision: '$Revision: 2524 $';
    Date: '$Date: 2008-10-05 16:26:49 +0200 (zo, 05 okt 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclResources;

constructor EJclUnexpectedEOSequenceError.Create;
begin
  {$IFDEF CLR}
  inherited Create(RsEUnexpectedEOSeq);
  {$ELSE ~CLR}
  inherited CreateRes(@RsEUnexpectedEOSeq);
  {$ENDIF ~CLR}
end;

function StreamReadByte(S: TStream; out B: Byte): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  {$IFDEF CLR}
  Result := S.Read(B) = SizeOf(B);
  {$ELSE ~CLR}
  Result := S.Read(B, SizeOf(B)) = SizeOf(B);
  {$ENDIF ~CLR}
end;

function StreamWriteByte(S: TStream; B: Byte): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  {$IFDEF CLR}
  Result := S.Write(B) = SizeOf(B);
  {$ELSE ~CLR}
  Result := S.Write(B, SizeOf(B)) = SizeOf(B);
  {$ENDIF ~CLR}
end;

function StreamReadWord(S: TStream; out W: Word): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  {$IFDEF CLR}
  Result := S.Read(W) = SizeOf(W);
  {$ELSE ~CLR}
  Result := S.Read(W, SizeOf(W)) = SizeOf(W);
  {$ENDIF ~CLR}
end;

function StreamWriteWord(S: TStream; W: Word): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  {$IFDEF CLR}
  Result := S.Write(W) = SizeOf(W);
  {$ELSE ~CLR}
  Result := S.Write(W, SizeOf(W)) = SizeOf(W);
  {$ENDIF ~CLR}
end;

//----------------- conversion routines ------------------------------------------------------------

// Converts the given source ANSI string into a Unicode string by expanding each character
// from one byte to two bytes.
// EAX contains Source, EDX contains Target, ECX contains Count

{$IFNDEF CLR}
procedure ExpandANSIString(const Source: PAnsiChar; Target: PWideChar; Count: Cardinal);
// Source in EAX
// Target in EDX
// Count in ECX
asm
       JECXZ   @@Finish           // go out if there is nothing to do (ECX = 0)
       PUSH    ESI
       MOV     ESI, EAX
       XOR     EAX, EAX
@@1:
       MOV     AL, [ESI]
       INC     ESI
       MOV     [EDX], AX
       ADD     EDX, 2
       DEC     ECX
       JNZ     @@1
       POP     ESI
@@Finish:
end;
{$ENDIF ~CLR}

const
  HalfShift: Integer = 10;

  HalfBase: UCS4 = $0010000;
  HalfMask: UCS4 = $3FF;

  OffsetsFromUTF8: array [0..5] of UCS4 =
    ($00000000, $00003080, $000E2080,
     $03C82080, $FA082080, $82082080);

  BytesFromUTF8: array [0..255] of Byte =
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5);

  FirstByteMark: array [0..6] of Byte =
    ($00, $00, $C0, $E0, $F0, $F8, $FC);

procedure FlagInvalidSequence(var StrPos: Integer; Increment: Integer; var Ch: UCS4); overload;
begin
  {$IFDEF UNICODE_SILENT_FAILURE}
  Ch := UCS4ReplacementCharacter;
  Inc(StrPos, Increment);
  {$ELSE ~UNICODE_SILENT_FAILURE}
  StrPos := -1;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

procedure FlagInvalidSequence(var StrPos: Integer; Increment: Integer); overload;
begin
  {$IFDEF UNICODE_SILENT_FAILURE}
  Inc(StrPos, Increment);
  {$ELSE ~UNICODE_SILENT_FAILURE}
  StrPos := -1;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

procedure FlagInvalidSequence(var Ch: UCS4); overload;
begin
  {$IFDEF UNICODE_SILENT_FAILURE}
  Ch := UCS4ReplacementCharacter;
  {$ELSE ~UNICODE_SILENT_FAILURE}
  raise EJclUnexpectedEOSequenceError.Create;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

procedure FlagInvalidSequence; overload;
begin
  {$IFNDEF UNICODE_SILENT_FAILURE}
  raise EJclUnexpectedEOSequenceError.Create;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF8GetNextChar(const S: TUTF8String; var StrPos: Integer): UCS4;
var
  StrLength: Integer;
  ChNext: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= StrLength) and (StrPos > 0) then
  begin
    Result := UCS4(S[StrPos]);

    case Result of
      $00..$7F:
        // 1 byte to read
        Inc(StrPos);
      $C0..$DF:
        begin
          // 2 bytes to read
          if StrPos >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $1F) shl 6) or (ChNext and $3F);
          Inc(StrPos, 2);
        end;
      $E0..$EF:
        begin
          // 3 bytes to read
          if (StrPos + 1) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $0F) shl 12) or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 3);
        end;
      $F0..$F7:
        begin
          // 4 bytes to read
          if (StrPos + 2) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $07) shl 18) or ((ChNext and $3F) shl 12);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 3]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 3, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 4);
        end;
      $F8..$FB:
        begin
          // 5 bytes to read
          if (StrPos + 3) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $03) shl 24) or ((ChNext and $3F) shl 18);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 12);
          ChNext := UCS4(S[StrPos + 3]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 3, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 4]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 4, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 5);
        end;
      $FC..$FD:
        begin
          // 6 bytes to read
          if (StrPos + 4) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $01) shl 30) or ((ChNext and $3F) shl 24);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 18);
          ChNext := UCS4(S[StrPos + 3]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 3, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 12);
          ChNext := UCS4(S[StrPos + 4]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 4, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 5]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 5, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 6);
        end;
    else
      FlagInvalidSequence(StrPos, 1, Result);
      Exit;
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

function UTF8GetNextCharFromStream(S: TStream; var Ch: UCS4): Boolean;
var
  B: Byte;
begin
  Result := StreamReadByte(S,B);
  if not Result then
    Exit;
  Ch := UCS4(B);

  case Ch of
    $00..$7F: ;
      // 1 byte to read
      // nothing to do
    $C0..$DF:
      begin
        // 2 bytes to read
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := ((Ch and $1F) shl 6) or (B and $3F);
      end;
    $E0..$EF:
      begin
        // 3 bytes to read
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := ((Ch and $0F) shl 12) or ((B and $3F) shl 6);
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := Ch or (B and $3F);
      end;
    $F0..$F7:
      begin
        // 4 bytes to read
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := ((Ch and $07) shl 18) or ((B and $3F) shl 12);
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := Ch or ((B and $3F) shl 6);
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := Ch or (B and $3F);
      end;
    $F8..$FB:
      begin
        // 5 bytes to read
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := ((Ch and $03) shl 24) or ((B and $3F) shl 18);
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := Ch or ((B and $3F) shl 12);
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := Ch or ((B and $3F) shl 6);
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := Ch or (B and $3F);
      end;
    $FC..$FD:
      begin
        // 6 bytes to read
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := ((Ch and $01) shl 30) or ((B and $3F) shl 24);
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := Ch or ((B and $3F) shl 18);
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := Ch or ((B and $3F) shl 12);
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := Ch or ((B and $3F) shl 6);
        Result := StreamReadByte(S,B);
        if not Result then
          Exit;
        if (B and $C0) <> $80 then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := Ch or (B and $3F);
      end;
  else
    FlagInvalidSequence(Ch);
    Exit;
  end;
end;

// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF8 sequence)
// StrPos will be incremented by the number of ansi chars that were skipped
// On return, NbSeq contains the number of UTF8 sequences that were skipped
function UTF8SkipChars(const S: TUTF8String; var StrPos: Integer; var NbSeq: Integer): Boolean;
var
  StrLength: Integer;
  Ch: UCS4;
  Index: Integer;
begin
  Result := True;
  StrLength := Length(S);

  Index := 0;
  while (Index < NbSeq) and (StrPos > 0) do
  begin
    Ch := UCS4(S[StrPos]);

    case Ch of
      $00..$7F:
        // 1 byte to skip
        Inc(StrPos);
      $C0..$DF:
        // 2 bytes to skip
        if (StrPos >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
          Inc(StrPos, 2);
      $E0..$EF:
        // 3 bytes to skip
        if ((StrPos + 1) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
          Inc(StrPos, 3);
      $F0..$F7:
        // 4 bytes to skip
        if ((StrPos + 2) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
          Inc(StrPos, 4);
      $F8..$FB:
        // 5 bytes to skip
        if ((StrPos + 3) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
        if (UCS4(S[StrPos + 4]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 4)
        else
          Inc(StrPos, 5);
      $FC..$FD:
        // 6 bytes to skip
        if ((StrPos + 4) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
        if (UCS4(S[StrPos + 4]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 4)
        else
        if (UCS4(S[StrPos + 5]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 5)
        else
          Inc(StrPos, 6);
    else
      FlagInvalidSequence(StrPos, 1);
    end;

    if StrPos <> -1 then
      Inc(Index);
    if (StrPos > StrLength) and (Index < NbSeq) then
    begin
      Result := False;
      Break;
    end;
  end;
  NbSeq := Index;
end;

function UTF8SkipCharsFromStream(S: TStream; var NbSeq: Integer): Boolean;
var
  B: Byte;
  Index: Integer;
begin
  Index := 0;
  while (Index < NbSeq) do
  begin
    Result := StreamReadByte(S, B);
    if not Result then
      Break;
    case B of
      $00..$7F: ;
        // 1 byte to skip
        // nothing to do
      $C0..$DF:
        // 2 bytes to skip
        begin
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
        end;
      $E0..$EF:
        // 3 bytes to skip
        begin
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
        end;
      $F0..$F7:
        // 4 bytes to skip
        begin
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
        end;
      $F8..$FB:
        // 5 bytes to skip
        begin
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
        end;
      $FC..$FD:
        // 6 bytes to skip
        begin
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
          Result := StreamReadByte(S, B);
          if not Result then
            Break;
          if (B and $C0) <> $80 then
            FlagInvalidSequence;
        end;
    else
      FlagInvalidSequence;
    end;
    Inc(Index);
  end;
  Result := Index = NbSeq;
  NbSeq := Index;
end;

// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF8SetNextChar(var S: TUTF8String; var StrPos: Integer; Ch: UCS4): Boolean;
var
  StrLength: Integer;
begin
  StrLength := Length(S);

  if Ch <= $7F then
  begin
    // 7 bits to store
    Result := (StrPos > 0) and (StrPos <= StrLength);
    if Result then
    begin
      S[StrPos] := AnsiChar(Ch);
      Inc(StrPos);
    end;
  end
  else
  if Ch <= $7FF then
  begin
    // 11 bits to store
    Result := (StrPos > 0) and (StrPos < StrLength);
    if Result then
    begin
      S[StrPos] := AnsiChar($C0 or (Ch shr 6));  // 5 bits
      S[StrPos + 1] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 2);
    end;
  end
  else
  if Ch <= $FFFF then
  begin
    // 16 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 1));
    if Result then
    begin
      S[StrPos] := AnsiChar($E0 or (Ch shr 12)); // 4 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 3);
    end;
  end
  else
  if Ch <= $1FFFFF then
  begin
    // 21 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 2));
    if Result then
    begin
      S[StrPos] := AnsiChar($F0 or (Ch shr 18)); // 3 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 4);
    end;
  end
  else
  if Ch <= $3FFFFFF then
  begin
    // 26 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 2));
    if Result then
    begin
      S[StrPos] := AnsiChar($F8 or (Ch shr 24)); // 2 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 4] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 5);
    end;
  end
  else
  if Ch <= MaximumUCS4 then
  begin
    // 31 bits to store
    Result := (StrPos > 0) and (StrPos < (StrLength - 3));
    if Result then
    begin
      S[StrPos] := AnsiChar($FC or (Ch shr 30)); // 1 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 24) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 4] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 5] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 6);
    end;
  end
  else
  begin
    {$IFDEF UNICOLE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := (StrPos > 0) and (StrPos < (StrLength - 1));
    if Result then
    begin
      S[StrPos] := AnsiChar($E0 or (UCS4ReplacementCharacter shr 12)); // 4 bits
      S[StrPos + 1] := AnsiChar(((UCS4ReplacementCharacter shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar((UCS4ReplacementCharacter and $3F) or $80); // 6 bits
      Inc(StrPos, 3);
    end;
    {$ELSE ~UNICODE_SILENT_FAILURE}
    StrPos := -1;
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

function UTF8SetNextCharToStream(S: TStream; Ch: UCS4): Boolean;
begin
  if Ch <= $7F then
    // 7 bits to store
    Result := StreamWriteByte(S,Ch)
  else
  if Ch <= $7FF then
    // 11 bits to store
    Result := StreamWriteByte(S, $C0 or (Ch shr 6)) and  // 5 bits
              StreamWriteByte(S, (Ch and $3F) or $80)    // 6 bits
  else
  if Ch <= $FFFF then
    // 16 bits to store
    Result := StreamWriteByte(S, $E0 or (Ch shr 12))          and // 4 bits
              StreamWriteByte(S, ((Ch shr 6) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, (Ch and $3F) or $80)             // 6 bits
  else
  if Ch <= $1FFFFF then
    // 21 bits to store
    Result := StreamWriteByte(S, $F0 or (Ch shr 18))           and // 3 bits
              StreamWriteByte(S, ((Ch shr 12) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 6) and $3F) or $80)  and // 6 bits
              StreamWriteByte(S, (Ch and $3F) or $80)              // 6 bits
  else
  if Ch <= $3FFFFFF then
    // 26 bits to store
    Result := StreamWriteByte(S, $F8 or (Ch shr 24))           and // 2 bits
              StreamWriteByte(S, ((Ch shr 18) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 12) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 6) and $3F) or $80)  and // 6 bits
              StreamWriteByte(S, (Ch and $3F) or $80)              // 6 bits
  else
  if Ch <= MaximumUCS4 then
    // 31 bits to store
    Result := StreamWriteByte(S, $FC or (Ch shr 30))           and // 1 bits
              StreamWriteByte(S, ((Ch shr 24) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 18) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 12) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, ((Ch shr 6) and $3F) or $80)  and // 6 bits
              StreamWriteByte(S, (Ch and $3F) or $80)              // 6 bits
  else
    {$IFDEF UNICOLE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := StreamWriteByte(S, $E0 or (UCS4ReplacementCharacter shr 12))          and // 4 bits
              StreamWriteByte(S, ((UCS4ReplacementCharacter shr 6) and $3F) or $80) and // 6 bits
              StreamWriteByte(S, (UCS4ReplacementCharacter and $3F) or $80); // 6 bits
    {$ELSE ~UNICODE_SILENT_FAILURE}
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF16GetNextChar(const S: TUTF16String; var StrPos: Integer): UCS4;
var
  StrLength: Integer;
  ChNext: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= StrLength) and (StrPos > 0) then
  begin
    Result := UCS4(S[StrPos]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        begin
          // 2 bytes to read
          if StrPos >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext < SurrogateLowStart) or (ChNext > SurrogateLowEnd) then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result - SurrogateHighStart) shl HalfShift) +  (ChNext - SurrogateLowStart) + HalfBase;
          Inc(StrPos, 2);
        end;
      SurrogateLowStart..SurrogateLowEnd:
        FlagInvalidSequence(StrPos, 1, Result);
    else
      // 1 byte to read
      Inc(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were read
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16GetNextChar(const S: UnicodeString; var StrPos: Integer): UCS4;
var
  StrLength: Integer;
  ChNext: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= StrLength) and (StrPos > 0) then
  begin
    Result := UCS4(S[StrPos]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        begin
          // 2 bytes to read
          if StrPos >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext < SurrogateLowStart) or (ChNext > SurrogateLowEnd) then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result - SurrogateHighStart) shl HalfShift) +  (ChNext - SurrogateLowStart) + HalfBase;
          Inc(StrPos, 2);
        end;
      SurrogateLowStart..SurrogateLowEnd:
        FlagInvalidSequence(StrPos, 1, Result);
    else
      // 1 byte to read
      Inc(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function UTF16GetNextCharFromStream(S: TStream; var Ch: UCS4): Boolean;
var
  W: Word;
begin
  Result := StreamReadWord(S, W);
  if not Result then
    Exit;
  Ch := UCS4(W);

  case W of
    SurrogateHighStart..SurrogateHighEnd:
      begin
        // 2 bytes to read
        Result := StreamReadWord(S, W);
        if not Result then
          Exit;
        if (W < SurrogateLowStart) or (W > SurrogateLowEnd) then
        begin
          FlagInvalidSequence(Ch);
          Exit;
        end;
        Ch := ((Ch - SurrogateHighStart) shl HalfShift) +  (W - SurrogateLowStart) + HalfBase;
      end;
    SurrogateLowStart..SurrogateLowEnd:
      FlagInvalidSequence(Ch);
  else
    // 1 byte to read
    // nothing to do
  end;
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be decremented by the number of chars that were read
function UTF16GetPreviousChar(const S: TUTF16String; var StrPos: Integer): UCS4;
var
  StrLength: Integer;
  ChPrev: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= (StrLength + 1)) and (StrPos > 1) then
  begin
    Result := UCS4(S[StrPos - 1]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        FlagInvalidSequence(StrPos, -1, Result);
      SurrogateLowStart..SurrogateLowEnd:
        begin
          // 2 bytes to read
          if StrPos <= 2 then
          begin
            FlagInvalidSequence(StrPos, -1, Result);
            Exit;
          end;
          ChPrev := UCS4(S[StrPos - 2]);
          if (ChPrev < SurrogateHighStart) or (ChPrev > SurrogateHighEnd) then
          begin
            FlagInvalidSequence(StrPos, -1, Result);
            Exit;
          end;
          Result := ((ChPrev - SurrogateHighStart) shl HalfShift) +  (Result - SurrogateLowStart) + HalfBase;
          Dec(StrPos, 2);
        end;
    else
      // 1 byte to read
      Dec(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be decremented by the number of chars that were read
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16GetPreviousChar(const S: UnicodeString; var StrPos: Integer): UCS4;
var
  StrLength: Integer;
  ChPrev: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= (StrLength + 1)) and (StrPos > 1) then
  begin
    Result := UCS4(S[StrPos - 1]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        FlagInvalidSequence(StrPos, -1, Result);
      SurrogateLowStart..SurrogateLowEnd:
        begin
          // 2 bytes to read
          if StrPos <= 2 then
          begin
            FlagInvalidSequence(StrPos, -1, Result);
            Exit;
          end;
          ChPrev := UCS4(S[StrPos - 2]);
          if (ChPrev < SurrogateHighStart) or (ChPrev > SurrogateHighEnd) then
          begin
            FlagInvalidSequence(StrPos, -1, Result);
            Exit;
          end;
          Result := ((ChPrev - SurrogateHighStart) shl HalfShift) +  (Result - SurrogateLowStart) + HalfBase;
          Dec(StrPos, 2);
        end;
    else
      // 1 byte to read
      Dec(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbSeq contains the number of UTF16 sequences that were skipped
function UTF16SkipChars(const S: TUTF16String; var StrPos: Integer; var NbSeq: Integer): Boolean;
var
  StrLength, Index: Integer;
  Ch, ChNext: UCS4;
begin
  Result := True;
  StrLength := Length(S);

  Index := 0;
  if NbSeq >= 0 then
    while (Index < NbSeq) and (StrPos > 0) do
    begin
      Ch := UCS4(S[StrPos]);
  
      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // 2 bytes to skip
          if StrPos >= StrLength then
            FlagInvalidSequence(StrPos, 1)
          else
          begin
            ChNext := UCS4(S[StrPos + 1]);
            if (ChNext < SurrogateLowStart) or (ChNext > SurrogateLowEnd) then
              FlagInvalidSequence(StrPos, 1)
            else
              Inc(StrPos, 2);
          end;
        SurrogateLowStart..SurrogateLowEnd:
          // error
          FlagInvalidSequence(StrPos, 1);
      else
        // 1 byte to skip
        Inc(StrPos);
      end;

      if StrPos <> -1 then
        Inc(Index);

      if (StrPos > StrLength) and (Index < NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end
  else
    while (Index > NbSeq) and (StrPos > 1) do
    begin
      Ch := UCS4(S[StrPos - 1]);

      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // error
          FlagInvalidSequence(StrPos, -1);
        SurrogateLowStart..SurrogateLowEnd:
          // 2 bytes to skip
          if StrPos <= 2 then
            FlagInvalidSequence(StrPos, -1)
          else
          begin
            ChNext := UCS4(S[StrPos - 2]);
            if (ChNext < SurrogateHighStart) or (ChNext > SurrogateHighEnd) then
              FlagInvalidSequence(StrPos, -1)
            else
              Dec(StrPos, 2);
          end;
      else
        // 1 byte to skip
        Dec(StrPos);
      end;

      if StrPos <> -1 then
        Dec(Index);

      if (StrPos = 1) and (Index > NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end;
  NbSeq := Index;
end;

// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbSeq contains the number of UTF16 sequences that were skipped
{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16SkipChars(const S: UnicodeString; var StrPos: Integer; var NbSeq: Integer): Boolean;
var
  StrLength, Index: Integer;
  Ch, ChNext: UCS4;
begin
  Result := True;
  StrLength := Length(S);

  Index := 0;
  if NbSeq >= 0 then
    while (Index < NbSeq) and (StrPos > 0) do
    begin
      Ch := UCS4(S[StrPos]);

      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // 2 bytes to skip
          if StrPos >= StrLength then
            FlagInvalidSequence(StrPos, 1)
          else
          begin
            ChNext := UCS4(S[StrPos + 1]);
            if (ChNext < SurrogateLowStart) or (ChNext > SurrogateLowEnd) then
              FlagInvalidSequence(StrPos, 1)
            else
              Inc(StrPos, 2);
          end;
        SurrogateLowStart..SurrogateLowEnd:
          // error
          FlagInvalidSequence(StrPos, 1);
      else
        // 1 byte to skip
        Inc(StrPos);
      end;

      if StrPos <> -1 then
        Inc(Index);

      if (StrPos > StrLength) and (Index < NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end
  else
    while (Index > NbSeq) and (StrPos > 1) do
    begin
      Ch := UCS4(S[StrPos - 1]);

      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // error
          FlagInvalidSequence(StrPos, -1);
        SurrogateLowStart..SurrogateLowEnd:
          // 2 bytes to skip
          if StrPos <= 2 then
            FlagInvalidSequence(StrPos, -1)
          else
          begin
            ChNext := UCS4(S[StrPos - 2]);
            if (ChNext < SurrogateHighStart) or (ChNext > SurrogateHighEnd) then
              FlagInvalidSequence(StrPos, -1)
            else
              Dec(StrPos, 2);
          end;
      else
        // 1 byte to skip
        Dec(StrPos);
      end;

      if StrPos <> -1 then
        Dec(Index);

      if (StrPos = 1) and (Index > NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end;
  NbSeq := Index;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function UTF16SkipCharsFromStream(S: TStream; var NbSeq: Integer): Boolean;
var
  Index: Integer;
  W: Word;
begin
  Index := 0;
  while Index < NbSeq do
  begin
    Result := StreamReadWord(S, W);
    if not Result then
      Break;
    case W of
      SurrogateHighStart..SurrogateHighEnd:
        // 2 bytes to skip
        begin
          Result := StreamReadWord(S, W);
          if not Result then
            Break;
          if (W < SurrogateLowStart) or (W > SurrogateLowEnd) then
            FlagInvalidSequence;
        end;
      SurrogateLowStart..SurrogateLowEnd:
        // error
        FlagInvalidSequence;
    else
      // 1 byte to skip
      // nothing to do
    end;
    Inc(Index);
  end;
  Result := Index = NbSeq;
  NbSeq := Index;
end;

// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF16SetNextChar(var S: TUTF16String; var StrPos: Integer; Ch: UCS4): Boolean;
var
  StrLength: Integer;
begin
  StrLength := Length(S);

  if Ch <= MaximumUCS2 then
  begin
    // 16 bits to store in place
    Result := (StrPos > 0) and (StrPos <= StrLength);
    if Result then
    begin
      S[StrPos] := WideChar(Ch);
      Inc(StrPos);
    end;
  end
  else
  if Ch <= MaximumUTF16 then
  begin
    // stores a surrogate pair
    Result := (StrPos > 0) and (StrPos < StrLength);
    if Result then
    begin
      Ch := Ch - HalfBase;
      S[StrPos] := WideChar((Ch shr HalfShift) + SurrogateHighStart);
      S[StrPos + 1] := WideChar((Ch and HalfMask) + SurrogateLowStart);
      Inc(StrPos, 2);
    end;
  end
  else
  begin
    {$IFDEF UNICOLE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := (StrPos > 0) and (StrPos <= StrLength);
    if Result then
    begin
      S[StrPos] := WideChar(UCS4ReplacementCharacter);
      Inc(StrPos, 1);
    end;
    {$ELSE ~UNICODE_SILENT_FAILURE}
    StrPos := -1;
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
function UTF16SetNextChar(var S: UnicodeString; var StrPos: Integer; Ch: UCS4): Boolean;
var
  StrLength: Integer;
begin
  StrLength := Length(S);

  if Ch <= MaximumUCS2 then
  begin
    // 16 bits to store in place
    Result := (StrPos > 0) and (StrPos <= StrLength);
    if Result then
    begin
      S[StrPos] := WideChar(Ch);
      Inc(StrPos);
    end;
  end
  else
  if Ch <= MaximumUTF16 then
  begin
    // stores a surrogate pair
    Result := (StrPos > 0) and (StrPos < StrLength);
    if Result then
    begin
      Ch := Ch - HalfBase;
      S[StrPos] := WideChar((Ch shr HalfShift) + SurrogateHighStart);
      S[StrPos + 1] := WideChar((Ch and HalfMask) + SurrogateLowStart);
      Inc(StrPos, 2);
    end;
  end
  else
  begin
    {$IFDEF UNICOLE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := (StrPos > 0) and (StrPos <= StrLength);
    if Result then
    begin
      S[StrPos] := WideChar(ReplacementCharacter);
      Inc(StrPos, 1);
    end;
    {$ELSE ~UNICODE_SILENT_FAILURE}
    StrPos := -1;
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function UTF16SetNextCharToStream(S: TStream; Ch: UCS4): Boolean;
begin
  if Ch <= MaximumUCS2 then
    // 16 bits to store in place
    Result := StreamWriteWord(S, Ch)
  else
  if Ch <= MaximumUTF16 then
    // stores a surrogate pair
    Result := StreamWriteWord(S, (Ch shr HalfShift) + SurrogateHighStart) and
              StreamWriteWord(S, (Ch and HalfMask) + SurrogateLowStart)
  else
  begin
    {$IFDEF UNICOLE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := StreamWriteWord(S, UCS4ReplacementCharacter);
    {$ELSE ~UNICODE_SILENT_FAILURE}
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

// AnsiGetNextChar = read next character at StrPos
// StrPos will be incremented by the number of chars that were read (1)
function AnsiGetNextChar(const S: AnsiString; var StrPos: Integer): UCS4;
var
  StrLen, TmpPos: Integer;
  UTF16Buffer: TUTF16String;
begin
  StrLen := Length(S);

  if (StrPos <= StrLen) and (StrPos > 0) then
  begin
    UTF16Buffer := WideString(S[StrPos]);
    TmpPos := 1;
    Result := UTF16GetNextChar(UTF16Buffer, TmpPos);
    if TmpPos = -1 then
      StrPos := -1
    else
      Inc(StrPos);
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

function AnsiGetNextCharFromStream(S: TStream; var Ch: UCS4): Boolean;
var
  B: Byte;
  TmpPos: Integer;
  UTF16Buffer: TUTF16String;
begin
  Result := StreamReadByte(S, B);
  if not Result then
    Exit;
  UTF16Buffer := WideString(AnsiString(Chr(B)));
  TmpPos := 1;
  Ch := UTF16GetNextChar(UTF16Buffer, TmpPos);
  Result := TmpPos <> -1;
end;

// AnsiSkipChars = skip NbSeq characters starting from StrPos
// returns False if String is too small
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of UTF16 sequences that were skipped
function AnsiSkipChars(const S: AnsiString; var StrPos: Integer; var NbSeq: Integer): Boolean;
var
  StrLen: Integer;
begin
  StrLen := Length(S);

  if StrPos > 0 then
  begin
    if StrPos + NbSeq > StrLen then
    begin
      NbSeq := StrLen + 1 - StrPos;
      StrPos := StrLen + 1;
      Result := False;
    end
    else
    begin
      // NbSeq := NbSeq;
      StrPos := StrLen + NbSeq;
      Result := True;
    end;
  end
  else
  begin
    // previous error
    NbSeq := 0;
    // StrPos := -1;
    Result := False;
  end;
end;

function AnsiSkipCharsFromStream(S: TStream; var NbSeq: Integer): Boolean;
var
  Index: Integer;
  B: Byte;
begin
  Index := 0;
  while Index < NbSeq do
  begin
    Result := StreamReadByte(S, B);
    if not Result then
      Break;
    Inc(Index);
  end;
  Result := Index = NbSeq;
  NbSeq := Index;
end;

// AnsiSetNextChar = append a character at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an ansi string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written (1)
function AnsiSetNextChar(var S: AnsiString; var StrPos: Integer; Ch: UCS4): Boolean;
var
  StrLen, TmpPos: Integer;
  UTF16Buffer: TUTF16String;
  AnsiBuffer: AnsiString;
begin
  StrLen := Length(S);
  Result := (StrPos > 0) and (StrPos <= StrLen);
  if Result then
  begin
    SetLength(UTF16Buffer, 2);
    TmpPos := 1;
    Result := UTF16SetNextChar(UTF16Buffer, TmpPos, Ch);
    if Result and (TmpPos = 2) then
    begin
      // one wide character
      AnsiBuffer := AnsiString(WideString(UTF16Buffer[1]));
      S[StrPos] := AnsiBuffer[1];
      Inc(StrPos);
    end
    else
    if Result and (TmpPos = 3) then
    begin
      // one surrogate pair
      AnsiBuffer := AnsiString(UTF16Buffer);
      S[StrPos] := AnsiBuffer[1];
      Inc(StrPos);
    end
    else
    begin
      {$IFDEF UNICODE_SILENT_FAILURE}
      // add ReplacementCharacter
      S[StrPos] := AnsiReplacementCharacter;
      Inc(StrPos);
      {$ELSE ~UNICODE_SILENT_FAILURE}
      Result := False;
      StrPos := -1;
      {$ENDIF ~UNICODE_SILENT_FAILURE}
    end;
  end;
end;

function AnsiSetNextCharToStream(S: TStream; Ch: UCS4): Boolean;
var
  TmpPos: Integer;
  UTF16Buffer: TUTF16String;
  AnsiBuffer: AnsiString;
begin
  SetLength(UTF16Buffer, 2);
  TmpPos := 1;
  Result := UTF16SetNextChar(UTF16Buffer, TmpPos, Ch);

  if Result and (TmpPos = 2) then
  begin
    // one wide character
    AnsiBuffer := AnsiString(WideString(UTF16Buffer[1]));
    Result := StreamWriteByte(S, Ord(AnsiBuffer[1]));
  end
  else
  if Result and (TmpPos = 3) then
  begin
    // one surrogate pair
    AnsiBuffer := AnsiString(UTF16Buffer);
    Result := StreamWriteByte(S, Ord(AnsiBuffer[1]));
  end
  else
  begin
    {$IFDEF UNICODE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := StreamWriteByte(S, Ord(AnsiReplacementCharacter));
    {$ELSE ~UNICODE_SILENT_FAILURE}
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

// StringGetNextChar = read next character/sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence for WideString)
// StrPos will be incremented by the number of chars that were read
function StringGetNextChar(const S: string; var StrPos: Integer): UCS4;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := UTF16GetNextChar(S, StrPos);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiGetNextChar(S, StrPos);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

// StringSkipChars = skip NbSeq characters/sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence for WideString)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of UTF16 sequences that were skipped
function StringSkipChars(const S: string; var StrPos: Integer; var NbSeq: Integer): Boolean;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := UTF16SkipChars(S, StrPos, NbSeq);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiSkipChars(S, StrPos, NbSeq);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

// StringSetNextChar = append a character/sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to a string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function StringSetNextChar(var S: string; var StrPos: Integer; Ch: UCS4): Boolean;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := UTF16SetNextChar(S, StrPos, Ch);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiSetNextChar(S, StrPos, Ch);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

function WideStringToUTF8(const S: WideString): TUTF8String;
begin
  Result := UTF16ToUTF8(S);
end;

function UTF8ToWideString(const S: TUTF8String): WideString;
begin
  Result := UTF8ToUTF16(S);
end;

function WideStringToUCS4(const S: WideString): TUCS4Array;
begin
  Result := UTF16ToUCS4(S);
end;

function UCS4ToWideString(const S: TUCS4Array): WideString;
begin
  Result := UCS4ToUTF16(S);
end;

function AnsiStringToUTF8(const S: AnsiString): TUTF8String;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUTF8(WS);
end;

function UTF8ToAnsiString(const S: TUTF8String): AnsiString;
var
  WS: TUTF16String;
begin
  WS := UTF8ToUTF16(S);
  Result := AnsiString(WS);
end;

function AnsiStringToUTF16(const S: AnsiString): TUTF16String;
begin
  Result := TUTF16String(S);
end;

function UTF16ToAnsiString(const S: TUTF16String): AnsiString;
begin
  Result := AnsiString(S);
end;

function AnsiStringToUCS4(const S: AnsiString): TUCS4Array;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUCS4(WS);
end;

function UCS4ToAnsiString(const S: TUCS4Array): AnsiString;
var
  WS: TUTF16String;
begin
  WS := UCS4ToUTF16(S);
  Result := AnsiString(WS);
end;

function StringToUTF8(const S: string): TUTF8String;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUTF8(WS);
end;

function UTF8ToString(const S: TUTF8String): string;
var
  WS: TUTF16String;
begin
  WS := UTF8ToUTF16(S);
  Result := string(WS);
end;

function StringToUTF16(const S: string): TUTF16String;
begin
  Result := TUTF16String(S);
end;

function UTF16ToString(const S: TUTF16String): string;
begin
  Result := string(S);
end;

function StringToUCS4(const S: string): TUCS4Array;
var
  WS: TUTF16String;
begin
  WS := TUTF16String(S);
  Result := UTF16ToUCS4(WS);
end;

function UCS4ToString(const S: TUCS4Array): string;
var
  WS: WideString;
begin
  WS := UCS4ToUTF16(S);
  Result := string(WS);
end;

function UTF8ToUTF16(const S: TUTF8String): TUTF16String;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 1;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF8GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      UTF16SetNextChar(Result, DestIndex, Ch);
    end;
    SetLength(Result, DestIndex - 1); // now fix up length
  end;
end;

function UTF16ToUTF8(const S: TUTF16String): TUTF8String;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength * 3); // worste case

    SrcIndex := 1;
    DestIndex := 1;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      UTF8SetNextChar(Result, DestIndex, Ch);
    end;
    SetLength(Result, DestIndex - 1); // now fix up length
  end;
end;

function UTF8ToUCS4(const S: TUTF8String): TUCS4Array;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S <> '' then
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 0;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF8GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      Result[DestIndex] := Ch;
      Inc(DestIndex);
    end;
    SetLength(Result, DestIndex); // now fix up length
  end;
end;

function UCS4ToUTF8(const S: TUCS4Array): TUTF8String;
var
  SrcIndex, SrcLength, DestIndex: Integer;
begin
  SrcLength := Length(S);
  if Length(S) = 0 then
    Result := ''
  else
  begin
    SetLength(Result, SrcLength * 3); // assume worst case
    DestIndex := 1;

    for SrcIndex := 0 to SrcLength - 1 do
    begin
      UTF8SetNextChar(Result, DestIndex, S[SrcIndex]);
      if DestIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
    end;

    SetLength(Result, DestIndex - 1); // set to actual length
  end;
end;

function UTF16ToUCS4(const S: TUTF16String): TUCS4Array;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S <> '' then
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 0;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;

      Result[DestIndex] := Ch;
      Inc(DestIndex);
    end;
    SetLength(Result, DestIndex); // now fix up length
  end;
end;

function UCS4ToUTF16(const S: TUCS4Array): TUTF16String;
var
  SrcIndex, SrcLength, DestIndex: Integer;
begin
  SrcLength := Length(S);
  if SrcLength = 0 then
    Result := ''
  else
  begin
    SetLength(Result, SrcLength * 3); // assume worst case
    DestIndex := 1;

    for SrcIndex := 0 to SrcLength - 1 do
    begin
      UTF16SetNextChar(Result, DestIndex, S[SrcIndex]);
      if DestIndex = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
    end;

    SetLength(Result, DestIndex - 1); // set to actual length
  end;
end;

function UTF8CharCount(const S: TUTF8String): Integer;
var
  StrPos: Integer;
begin
  StrPos := 1;
  Result := Length(S);
  UTF8SkipChars(S, StrPos, Result);
  if StrPos = -1 then
    raise EJclUnexpectedEOSequenceError.Create;
end;

function UTF16CharCount(const S: TUTF16String): Integer;
var
  StrPos: Integer;
begin
  StrPos := 1;
  Result := Length(S);
  UTF16SkipChars(S, StrPos, Result);
  if StrPos = -1 then
    raise EJclUnexpectedEOSequenceError.Create;
end;

function UCS2CharCount(const S: TUCS2String): Integer;
begin
  Result := Length(S);
end;

function UCS4CharCount(const S: TUCS4Array): Integer;
begin
  Result := Length(S);
end;

function GetUCS4CharAt(const UTF8Str: TUTF8String; Index: Integer; out Value: UCS4): Boolean; overload;
var
  StrPos: Integer;
begin
  StrPos := 1;
  Result := Index >= 0;
  if Result then
    Result := UTF8SkipChars(UTF8Str, StrPos, Index);
  if StrPos = -1 then
    raise EJclUnexpectedEOSequenceError.Create;
  Result := Result and (StrPos <= Length(UTF8Str));
  if Result then
  begin
    Value := UTF8GetNextChar(UTF8Str, StrPos);
    if StrPos = -1 then
      raise EJclUnexpectedEOSequenceError.Create;
  end;
end;

function GetUCS4CharAt(const WideStr: WideString; Index: Integer; out Value: UCS4; IsUTF16: Boolean): Boolean; overload;
var
  StrPos: Integer;
begin
  if IsUTF16 then
  begin
    StrPos := 1;
    Result := Index >= 0;
    if Result then
      Result := UTF16SkipChars(WideStr, StrPos, Index);
    if StrPos = -1 then
      raise EJclUnexpectedEOSequenceError.Create;
    Result := Result and (StrPos <= Length(WideStr));
    if Result then
    begin
      Value := UTF16GetNextChar(WideStr, StrPos);
      if StrPos = -1 then
        raise EJclUnexpectedEOSequenceError.Create;
    end;
  end
  else
  begin
    Result := (Index >= 1) and (Index <= Length(WideStr));
    Value := UCS4(WideStr[Index]);
  end;
end;

function GetUCS4CharAt(const UCS4Str: TUCS4Array; Index: Integer; out Value: UCS4): Boolean; overload;
begin
  Result := (Index >= 0) and (Index < Length(UCS4Str));
  if Result then
    Value := UCS4Str[Index];
end;

function UCS4ToAnsiChar(Value: UCS4): AnsiChar;
var
  Buf: WideString;
  StrPos: Integer;
begin
  StrPos := 1;
  Buf := #0#0;
  if UTF16SetNextChar(Buf, StrPos, Value) then
    Result := AnsiString(Buf)[1]
  else
    Result := AnsiReplacementCharacter;
end;

function UCS4ToWideChar(Value: UCS4): WideChar;
begin
  if Value <= MaximumUCS2 then
    Result := WideChar(Value)
  else
    Result := WideChar(UCS4ReplacementCharacter);
end;

function UCS4ToChar(Value: UCS4): Char;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := UCS4ToWideChar(Value);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := UCS4ToAnsiChar(Value);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

function AnsiCharToUCS4(Value: AnsiChar): UCS4;
var
  Buf: WideString;
  StrPos: Integer;
begin
  StrPos := 1;
  Buf := WideString(AnsiString(Value));
  Result := UTF16GetNextChar(Buf, StrPos);
end;

function WideCharToUCS4(Value: WideChar): UCS4;
begin
  Result := UCS4(Value);
end;

function CharToUCS4(Value: Char): UCS4;
begin
  {$IFDEF SUPPORTS_UNICODE}
  Result := WideCharToUCS4(Value);
  {$ELSE ~SUPPORTS_UNICODE}
  Result := AnsiCharToUCS4(Value);
  {$ENDIF ~SUPPORTS_UNICODE}
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
