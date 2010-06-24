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
unit uEmuKrnlRtl;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  JwaWinBase,
  JwaWinNT,
  JwaNative,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uTypes,
  uLog,
  uEmuFS,
  uEmuFile,
  uEmuXapi,
  uEmuKrnl,
  uDxbxUtils,
  uDxbxKrnl,
  uDxbxKrnlUtils;

function xboxkrnl_RtlAnsiStringToUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PSTRING;
  AllocateDestinationString: UCHAR
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendStringToString(
  DestinationString: PSTRING;
  AppendThisString: PSTRING
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendUnicodeStringToString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendUnicodeToString(
  Destination: PUNICODE_STRING;
  Source: LPCWSTR
  ): NTSTATUS; stdcall;
procedure xboxkrnl_RtlAssert(
  FailedAssertion: PVOID;
  FileName: PVOID;
  LineNumber: ULONG;
  Message_: P_CHAR
  ); stdcall;
procedure xboxkrnl_RtlCaptureContext(
  ContextRecord: PCONTEXT
  ); stdcall;
function xboxkrnl_RtlCaptureStackBackTrace(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlCharToInteger(
  Str: PCSZ;
  Base: ULONG;
  Value: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlCompareMemory(
  Source1: PVOID;
  Source2: PVOID;
  Length: SIZE_T
  ): SIZE_T; stdcall;
function xboxkrnl_RtlCompareMemoryUlong(
  Source: PVOID;
  Length: ULONG;
  Value: ULONG
  ): ULONG; stdcall;
function xboxkrnl_RtlCompareString(
  String1: PSTRING;
  String2: PSTRING;
  CaseInsensitive: _BOOLEAN
  ): LONG; stdcall;
function xboxkrnl_RtlCompareUnicodeString(
  String1: PUNICODE_STRING;
  String2: PUNICODE_STRING;
  CaseInsensitive: _BOOLEAN
  ): LONG; stdcall;
procedure xboxkrnl_RtlCopyString(
  DestinationString: PSTRING;
  SourceString: PSTRING
  ); stdcall;
procedure xboxkrnl_RtlCopyUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING
  ); stdcall;
function xboxkrnl_RtlCreateUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PWSTR
  ): _BOOLEAN; stdcall;
function xboxkrnl_RtlDowncaseUnicodeChar(
  Source: WCHAR
  ): WCHAR; stdcall;
function xboxkrnl_RtlDowncaseUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
  ): NTSTATUS; stdcall;
procedure xboxkrnl_RtlEnterCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
function xboxkrnl_RtlEnterCriticalSectionAndRegion(
  CriticalSection: PRTL_CRITICAL_SECTION
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlEqualString(
  String1: PSTRING;
  String2: PSTRING;
  CaseInsensitive: _BOOLEAN
  ): _BOOLEAN; stdcall;
function xboxkrnl_RtlEqualUnicodeString(
  String1: PUNICODE_STRING;
  String2: PUNICODE_STRING;
  CaseInsensitive: _BOOLEAN
  ): _BOOLEAN; stdcall;
function xboxkrnl_RtlExtendedIntegerMultiply(
  Multiplicand: LARGE_INTEGER;
  Multiplier: LONG
  ): LARGE_INTEGER; stdcall;
function xboxkrnl_RtlExtendedLargeIntegerDivide(
  Dividend: LARGE_INTEGER;
  Divisor: ULONG;
  Remainder: PULONG
  ): LARGE_INTEGER; stdcall;
function xboxkrnl_RtlExtendedMagicDivide(
  Dividend: LARGE_INTEGER;
  MagicDivisor: LARGE_INTEGER;
  ShiftCount: CCHAR
  ): LARGE_INTEGER; stdcall;
procedure xboxkrnl_RtlFillMemory(
  Destination: PVOID;
  Length: SIZE_T;
  Fill: UCHAR
  ); stdcall;
procedure xboxkrnl_RtlFillMemoryUlong(
  Destination: PVOID;
  Length: ULONG;
  Fill: ULONG
  ); stdcall;
procedure xboxkrnl_RtlFreeAnsiString(
  AnsiString: PANSI_STRING
  ); stdcall;
procedure xboxkrnl_RtlFreeUnicodeString(
  UnicodeString: PUNICODE_STRING
  ); stdcall;
procedure xboxkrnl_RtlGetCallersAddress(
  CallersAddress: PPVOID;
  CallersCaller: PPVOID
  ); stdcall;
procedure xboxkrnl_RtlInitAnsiString(
  DestinationString: PANSI_STRING; // OUT
  SourceString: PCSZ
); stdcall;
procedure xboxkrnl_RtlInitUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: LPCWSTR
  ); stdcall;
procedure xboxkrnl_RtlInitializeCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
function xboxkrnl_RtlIntegerToChar(
  Value: ULONG;
  Base: ULONG;
  Length: ULONG;
  Str: PAnsiChar
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlIntegerToUnicodeString(
  Value: ULONG;
  Base: ULONG;
  Str: PUNICODE_STRING
  ): NTSTATUS; stdcall;
procedure xboxkrnl_RtlLeaveCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
function xboxkrnl_RtlLeaveCriticalSectionAndRegion(
  CriticalSection: PRTL_CRITICAL_SECTION
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlLowerChar(
  Character: _CHAR
  ): _CHAR; stdcall;
procedure xboxkrnl_RtlMapGenericMask(
  AccessMask: PACCESS_MASK;
  GenericMapping: PGENERIC_MAPPING
  ); stdcall;
procedure xboxkrnl_RtlMoveMemory(
  Destination: PVOID;
  Source: PVOID;
  Length: SIZE_T
  ); stdcall;
function xboxkrnl_RtlMultiByteToUnicodeN(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlMultiByteToUnicodeSize(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlNtStatusToDosError(
  Status: NTSTATUS
  ): ULONG; stdcall;
procedure xboxkrnl_RtlRaiseException(
  ExceptionRecord: PEXCEPTION_RECORD
  ); stdcall;
procedure xboxkrnl_RtlRaiseStatus(
  Status: NTSTATUS
  ); stdcall;
function xboxkrnl_RtlTimeFieldsToTime(
  TimeFields: PTIME_FIELDS;
  Time: PLARGE_INTEGER // OUT
  ): LONGBOOL; stdcall;
procedure xboxkrnl_RtlTimeToTimeFields(
  Time: PLARGE_INTEGER;
  TimeFields: PTIME_FIELDS // out
  ); stdcall;
function xboxkrnl_RtlTryEnterCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ): _BOOLEAN; stdcall;
function xboxkrnl_RtlUlongByteSwap(
  Source: ULONG
  ): ULONG; stdcall;
function xboxkrnl_RtlUnicodeStringToAnsiString(
  DestinationString: PSTRING; // OUT
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeStringToInteger(
  Str: PUNICODE_STRING;
  Base: ULONG;
  Value: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeToMultiByteN(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlUnicodeToMultiByteSize(
  BytesInMultiByteString: PULONG;
  UnicodeString: PWSTR;
  BytesInUnicodeString: ULONG
  ): NTSTATUS; stdcall;
procedure xboxkrnl_RtlUnwind(
  TargetFrame: PVOID;
  TargetIp: PVOID;
  ExceptionRecord: PEXCEPTION_RECORD;
  ReturnValue: PVOID
  ); stdcall;
function xboxkrnl_RtlUpcaseUnicodeChar(
  SourceCharacter: WCHAR
  ): WCHAR; stdcall;
function xboxkrnl_RtlUpcaseUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlUpcaseUnicodeToMultiByteN(
  MbString: PAnsiChar;
  MbSize: ULONG;
  ResultSize: PULONG;
  UnicodeString: PWSTR;
  UnicodeSize: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlUpperChar(
  Character: ANSICHAR
  ): ANSICHAR; stdcall;
procedure xboxkrnl_RtlUpperString(
  DestinationString: PSTRING;
  SourceString: PSTRING
  ); stdcall;
function xboxkrnl_RtlUshortByteSwap(
  Source: USHORT
  ): USHORT; // No stdcall (was fastcall)!
function xboxkrnl_RtlWalkFrameChain(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_RtlZeroMemory(
  Destination: PVOID;
  Length: SIZE_T
  ); stdcall;
procedure xboxkrnl_RtlRip(
  Part1: PCSZ;
  Part2: PCSZ;
  Part3: PCSZ
  ); stdcall;
function xboxkrnl_snprintf(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_sprintf(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_vsnprintf(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_vsprintf(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

uses
  uXboxLibraryUtils;

function xboxkrnl_RtlAnsiStringToUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PSTRING;
  AllocateDestinationString: UCHAR
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : RtlAnsiStringToUnicodeString'+
      #13#10'('+
      #13#10'   DestinationString         : 0x%.08X'+
      #13#10'   SourceString              : 0x%.08X ("%s")'+
      #13#10'   AllocateDestinationString : 0x%.08X'+
      #13#10');',
      [DestinationString,
       SourceString, PSTRING_String(SourceString),
       AllocateDestinationString]);
{$ENDIF}

  Result := JwaNative.RtlAnsiStringToUnicodeString(DestinationString, SourceString, Boolean(AllocateDestinationString));
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlAppendStringToString(
  DestinationString: PSTRING;
  AppendThisString: PSTRING
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlAppendStringToString(DestinationString, AppendThisString);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlAppendUnicodeStringToString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlAppendUnicodeStringToString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlAppendUnicodeToString(
  Destination: PUNICODE_STRING;
  Source: LPCWSTR
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlAppendUnicodeToString(Destination, Source);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlAssert(
  FailedAssertion: PVOID;
  FileName: PVOID;
  LineNumber: ULONG;
  Message_: P_CHAR
  ); stdcall;
// Source:shogun  Branch:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : RtlAssert' +
      #13#10'(' +
      #13#10'   FailedAssertion           : 0x%.08X ("%s")' +
      #13#10'   FileName                  : 0x%.08X ("%s")' +
      #13#10'   LineNumber                : 0x%.08X (%d)' +
      #13#10'   Message                   : 0x%.08X ("%s")' +
      #13#10');',
      [Pointer(FailedAssertion), PAnsiChar(FailedAssertion),
       Pointer(FileName), PAnsiChar(FileName),
       LineNumber, LineNumber,
       Pointer(Message_), PAnsiChar(Message_)]);
{$ENDIF}

  JwaNative.RtlAssert(FailedAssertion, FileName, LineNumber, Message_);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlCaptureContext(
  ContextRecord: PCONTEXT
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlCaptureContext(ContextRecord);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCaptureStackBackTrace(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlCaptureStackBackTrace');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCharToInteger(
  Str: PCSZ;
  Base: ULONG;
  Value: PULONG
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlCharToInteger(Str, Base, Value);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareMemory(
  Source1: PVOID;
  Source2: PVOID;
  Length: SIZE_T
  ): SIZE_T; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlCompareMemory(Source1, Source2, Length);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareMemoryUlong(
  Source: PVOID;
  Length: ULONG;
  Value: ULONG
  ): ULONG; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlCompareMemoryUlong(Source, Length, Value);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareString(
  String1: PSTRING;
  String2: PSTRING;
  CaseInsensitive: _BOOLEAN
  ): LONG; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlCompareString(String1, String2, CaseInsensitive);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareUnicodeString(
  String1: PUNICODE_STRING;
  String2: PUNICODE_STRING;
  CaseInsensitive: _BOOLEAN
  ): LONG; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlCompareUnicodeString(String1, String2, CaseInsensitive);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlCopyString(
  DestinationString: PSTRING;
  SourceString: PSTRING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlCopyString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlCopyUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlCopyUnicodeString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCreateUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PWSTR
  ): _BOOLEAN; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlCreateUnicodeString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlDowncaseUnicodeChar(
  Source: WCHAR
  ): WCHAR; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlDowncaseUnicodeChar(Source);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlDowncaseUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlDowncaseUnicodeString(DestinationString, SourceString, AllocateDestinationString);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlEnterCriticalSection
(
  CriticalSection: PRTL_CRITICAL_SECTION
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DXBX_EXTREME_LOGGING}
  DbgPrintf('EmuKrnl : RtlEnterCriticalSection' +
           #13#10'(' +
           #13#10'   CriticalSection              : 0x%.08X' +
           #13#10');',
           [CriticalSection]);
{$ENDIF}

  // Cxbx : This seems redundant, but xbox software doesn't always do it
  if(CriticalSection.LockCount = -1) then
    JwaNative.RtlInitializeCriticalSection(CriticalSection);

  JwaNative.RtlEnterCriticalSection(CriticalSection);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlEnterCriticalSectionAndRegion(
  CriticalSection: PRTL_CRITICAL_SECTION
  ): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:50
begin
  EmuSwapFS(fsWindows);
{$IFDEF DXBX_EXTREME_LOGGING}
  DbgPrintf('EmuKrnl : RtlEnterCriticalSectionAndRegion' +
           #13#10'(' +
           #13#10'   CriticalSection              : 0x%.08X' +
           #13#10');',
           [CriticalSection]);
{$ENDIF}

  JwaNative.RtlEnterCriticalSection(CriticalSection);
  Result := 0; // TODO -oDxbx : What should we return here?

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlEqualString
(
  String1: PSTRING;
  String2: PSTRING;
  CaseInsensitive: _BOOLEAN
): _BOOLEAN; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : RtlEqualString' +
      #13#10'(' +
      #13#10'  String1            : 0x%.08X ("%s")' +
      #13#10'  String2            : 0x%.08X ("%s")' +
      #13#10'  CaseInsensitive    : 0x%.08X' +
      #13#10');',
      [String1, PSTRING_String(String1), String2, PSTRING_String(String2), CaseInsensitive]);
{$ENDIF}

  Result := JwaNative.RtlEqualString(String1, String2, CaseInsensitive);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlEqualUnicodeString(
  String1: PUNICODE_STRING;
  String2: PUNICODE_STRING;
  CaseInsensitive: _BOOLEAN
  ): _BOOLEAN; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlEqualUnicodeString(String1, String2, CaseInsensitive);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlExtendedIntegerMultiply(
  Multiplicand: LARGE_INTEGER;
  Multiplier: LONG
  ): LARGE_INTEGER; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlExtendedIntegerMultiply(Multiplicand, Multiplier);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlExtendedLargeIntegerDivide(
  Dividend: LARGE_INTEGER;
  Divisor: ULONG;
  Remainder: PULONG
  ): LARGE_INTEGER; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlExtendedLargeIntegerDivide(Dividend, Divisor, Remainder);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlExtendedMagicDivide(
  Dividend: LARGE_INTEGER;
  MagicDivisor: LARGE_INTEGER;
  ShiftCount: CCHAR
  ): LARGE_INTEGER; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlExtendedMagicDivide(Dividend, MagicDivisor, JwaWinType.CCHAR(ShiftCount));
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlFillMemory(
  Destination: PVOID;
  Length: SIZE_T;
  Fill: UCHAR
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlFillMemory(Destination, Length, Fill);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlFillMemoryUlong(
  Destination: PVOID;
  Length: ULONG;
  Fill: ULONG
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlFillMemoryUlong(Destination, Length, Fill);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlFreeAnsiString(
  AnsiString: PANSI_STRING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlFreeAnsiString(AnsiString);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlFreeUnicodeString(
  UnicodeString: PUNICODE_STRING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlFreeUnicodeString(UnicodeString);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlGetCallersAddress(
  CallersAddress: PPVOID;
  CallersCaller: PPVOID
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlGetCallersAddress(CallersAddress, CallersCaller);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlInitAnsiString
(
  DestinationString: PANSI_STRING; // OUT
  SourceString: PCSZ
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : RtlInitAnsiString' +
      #13#10'(' +
      #13#10'   DestinationString   : 0x%.08X' +
      #13#10'   SourceString        : 0x%.08X ("%s")' +
      #13#10');',
      [DestinationString,
       SourceString, AnsiString(SourceString)]);
{$ENDIF}

  JwaNative.RtlInitAnsiString(DestinationString, SourceString);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlInitUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: LPCWSTR
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlInitUnicodeString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlInitializeCriticalSection
(
  CriticalSection: PRTL_CRITICAL_SECTION
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DXBX_DEBUG_TRACE}
{ MARKED OUT CXBX
  DbgPrintf('EmuKrnl : RtlInitializeCriticalSection' +
           #13#10'(' +
           #13#10'   CriticalSection              : 0x%.08X' +
           #13#10');',
           [CriticalSection]);   }
{$ENDIF}

  JwaNative.RtlInitializeCriticalSection(CriticalSection);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlIntegerToChar(
  Value: ULONG;
  Base: ULONG;
  Length: ULONG;
  Str: PAnsiChar
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlIntegerToChar(Value, Base, Length, Str);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlIntegerToUnicodeString(
  Value: ULONG;
  Base: ULONG;
  Str: PUNICODE_STRING
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlIntegerToUnicodeString(Value, Base, Str);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlLeaveCriticalSection
(
  CriticalSection: PRTL_CRITICAL_SECTION
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlLeaveCriticalSection(CriticalSection);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlLeaveCriticalSectionAndRegion(
  CriticalSection: PRTL_CRITICAL_SECTION
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlLeaveCriticalSectionAndRegion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlLowerChar(Character: _CHAR): _CHAR; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : RtlLowerChar(%c)', [Character]);
{$ENDIF}

  Result := tolower(Character);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlMapGenericMask(
  AccessMask: PACCESS_MASK;
  GenericMapping: PGENERIC_MAPPING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlMapGenericMask(AccessMask, GenericMapping);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlMoveMemory(
  Destination: PVOID;
  Source: PVOID;
  Length: SIZE_T
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlMoveMemory(Destination, Source, Length);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlMultiByteToUnicodeN(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlMultiByteToUnicodeN');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlMultiByteToUnicodeSize(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlMultiByteToUnicodeSize');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlNtStatusToDosError
(
  Status: NTSTATUS
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : RtlNtStatusToDosError' +
           #13#10'(' +
           #13#10'   Status              : 0x%.08X' +
           #13#10');',
           [Status]);
{$ENDIF}

  Result := JwaNative.RtlNtStatusToDosError(Status);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlRaiseException(
  ExceptionRecord: PEXCEPTION_RECORD
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('RtlRaiseException');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlRaiseStatus(
  Status: NTSTATUS
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlRaiseStatus(Status);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlTimeFieldsToTime
(
  TimeFields: PTIME_FIELDS;
  Time: PLARGE_INTEGER // OUT
): LONGBOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  
{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : RtlTimeFieldsToTime' +
        #13#10'(' +
        #13#10'   TimeFields          : 0x%.08X' +
        #13#10'   Time                : 0x%.08X' +
        #13#10');',
        [TimeFields, Time]);
{$ENDIF}

  Result := JwaNative.RtlTimeFieldsToTime(TimeFields, Time);
  
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlTimeToTimeFields
(
  Time: PLARGE_INTEGER;
  TimeFields: PTIME_FIELDS // out
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : RtlTimeToTimeFields' +
        #13#10'(' +
        #13#10'   Time                : 0x%.08X' +
        #13#10'   TimeFields          : 0x%.08X' +
        #13#10');',
        [Time, TimeFields]);
{$ENDIF}

  JwaNative.RtlTimeToTimeFields(Time, TimeFields);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlTryEnterCriticalSection
(
  CriticalSection: PRTL_CRITICAL_SECTION
): _BOOLEAN; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : RtlTryEnterCriticalSection' +
        #13#10'(' +
        #13#10'   CriticalSection     : 0x%.08X' +
        #13#10');',
        [CriticalSection]);
{$ENDIF}

  Result := JwaNative.RtlTryEnterCriticalSection(CriticalSection);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUlongByteSwap(
  Source: ULONG
  ): ULONG; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlUlongByteSwap(Source);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnicodeStringToAnsiString
(
  DestinationString: PSTRING; // OUT
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : RtlUnicodeStringToAnsiString'+
      #13#10'('+
      #13#10'   DestinationString         : 0x%.08X'+
      #13#10'   SourceString              : 0x%.08X ("%s")'+
      #13#10'   AllocateDestinationString : 0x%.08X'+
      #13#10');',
      [DestinationString,
       SourceString, PUNICODE_STRING_String(SourceString),
       AllocateDestinationString]);
{$ENDIF}

  Result := JwaNative.RtlUnicodeStringToAnsiString(DestinationString, SourceString, AllocateDestinationString);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnicodeStringToInteger(
  Str: PUNICODE_STRING;
  Base: ULONG;
  Value: PULONG
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlUnicodeStringToInteger(Str, Base, Value);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnicodeToMultiByteN(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUnicodeToMultiByteN');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnicodeToMultiByteSize(
  BytesInMultiByteString: PULONG;
  UnicodeString: PWSTR;
  BytesInUnicodeString: ULONG
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlUnicodeToMultiByteSize(BytesInMultiByteString, UnicodeString, BytesInUnicodeString);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlUnwind(
  TargetFrame: PVOID;
  TargetIp: PVOID;
  ExceptionRecord: PEXCEPTION_RECORD;
  ReturnValue: PVOID
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlUnwind(TargetFrame, TargetIp, ExceptionRecord, ReturnValue);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpcaseUnicodeChar(
  SourceCharacter: WCHAR
  ): WCHAR; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlUpcaseUnicodeChar(SourceCharacter);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpcaseUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlUpcaseUnicodeString(DestinationString, SourceString, Boolean(AllocateDestinationString));
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpcaseUnicodeToMultiByteN(
  MbString: PAnsiChar;
  MbSize: ULONG;
  ResultSize: PULONG;
  UnicodeString: PWSTR;
  UnicodeSize: ULONG
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlUpcaseUnicodeToMultiByteN(MbString, MbSize, {var}ResultSize^, UnicodeString, UnicodeSize);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpperChar(
  Character: ANSICHAR
  ): ANSICHAR; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlUpperChar(Character);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlUpperString(
  DestinationString: PSTRING;
  SourceString: PSTRING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlUpperString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUshortByteSwap(
  Source: USHORT
  ): USHORT; // No stdcall (was fastcall)!
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.RtlUshortByteSwap(Source);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlWalkFrameChain(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlWalkFrameChain');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlZeroMemory(
  Destination: PVOID;
  Length: SIZE_T
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  JwaNative.RtlZeroMemory(Destination, Length);
  EmuSwapFS(fsXbox);
end;

// RtlRip:
// Traps to the debugger with a certain message, then crashes.
//
// New to the XBOX.
procedure xboxkrnl_RtlRip(
  Part1: PCSZ;
  Part2: PCSZ;
  Part3: PCSZ
  ); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('RtlRip');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_snprintf(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('snprintf');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_sprintf(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('sprintf');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_vsnprintf(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('vsnprintf');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_vsprintf(): NTSTATUS; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('vsprintf');
  EmuSwapFS(fsXbox);
end;


end.

