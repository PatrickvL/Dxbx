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

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
  // Jedi
  JwaWinType,
  JwaWinBase,
  JwaWinNT,
  JwaNative,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uLog,
  uEmuFS,
  uEmuFile,
  uEmuXapi,
  uEmuKrnl,
  uDxbxKrnl;

function xboxkrnl_RtlAnsiStringToUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PSTRING;
  AllocateDestinationString: UCHAR
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendStringToString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlAppendUnicodeStringToString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlAppendUnicodeToString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_RtlAssert(
  FailedAssertion: PVOID;
  FileName: PVOID;
  LineNumber: ULONG;
  _Message: PCHAR
  ); stdcall;
function xboxkrnl_RtlCaptureContext(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlCaptureStackBackTrace(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlCharToInteger(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlCompareMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlCompareMemoryUlong(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlCompareString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlCompareUnicodeString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlCopyString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlCopyUnicodeString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlCreateUnicodeString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlDowncaseUnicodeChar(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlDowncaseUnicodeString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_RtlEnterCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
function xboxkrnl_RtlEnterCriticalSectionAndRegion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlEqualString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlEqualUnicodeString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlExtendedIntegerMultiply(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlExtendedLargeIntegerDivide(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlExtendedMagicDivide(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlFillMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlFillMemoryUlong(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlFreeAnsiString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlFreeUnicodeString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlGetCallersAddress(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_RtlInitAnsiString(
  DestinationString: PANSI_STRING; // OUT
  SourceString: PCSZ
); stdcall;
function xboxkrnl_RtlInitUnicodeString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_RtlInitializeCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
function xboxkrnl_RtlIntegerToChar(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlIntegerToUnicodeString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_RtlLeaveCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
function xboxkrnl_RtlLeaveCriticalSectionAndRegion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlLowerChar(
  Character: CHAR
  ): CHAR; stdcall;
function xboxkrnl_RtlMapGenericMask(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlMoveMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlMultiByteToUnicodeN(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlMultiByteToUnicodeSize(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlNtStatusToDosError(
  Status: NTSTATUS
  ): ULONG; stdcall;
function xboxkrnl_RtlRaiseException(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlRaiseStatus(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
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
  ): LONGBOOL; stdcall;
function xboxkrnl_RtlUlongByteSwap(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlUnicodeStringToAnsiString(
  DestinationString: PSTRING; // OUT
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: LONGBOOL
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeStringToInteger(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlUnicodeToMultiByteN(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlUnicodeToMultiByteSize(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlUnwind(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlUpcaseUnicodeChar(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlUpcaseUnicodeString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlUpcaseUnicodeToMultiByteN(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlUpperChar(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlUpperString(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlUshortByteSwap(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlWalkFrameChain(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlZeroMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlRip(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlSnprintf(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlSprintf(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlVsnprintf(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_RtlVsprintf(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

uses
  uXboxLibraryUtils;

function xboxkrnl_RtlAnsiStringToUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PSTRING;
  AllocateDestinationString: UCHAR
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlAnsiStringToUnicodeString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlAppendStringToString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlAppendStringToString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlAppendUnicodeStringToString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlAppendUnicodeStringToString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlAppendUnicodeToString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlAppendUnicodeToString');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlAssert(
  FailedAssertion: PVOID;
  FileName: PVOID;
  LineNumber: ULONG;
  _Message: PCHAR
  ); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('RtlAssert');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCaptureContext(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlCaptureContext');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCaptureStackBackTrace(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlCaptureStackBackTrace');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCharToInteger(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlCharToInteger');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlCompareMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareMemoryUlong(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlCompareMemoryUlong');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlCompareString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlCompareUnicodeString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCopyString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlCopyString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCopyUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlCopyUnicodeString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCreateUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlCreateUnicodeString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlDowncaseUnicodeChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlDowncaseUnicodeChar');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlDowncaseUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlDowncaseUnicodeString');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlEnterCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:80
begin
  EmuSwapFS(fsWindows);

  JwaNative.RtlEnterCriticalSection(CriticalSection);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlEnterCriticalSectionAndRegion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlEnterCriticalSectionAndRegion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlEqualString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlEqualString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlEqualUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlEqualUnicodeString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlExtendedIntegerMultiply(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlExtendedIntegerMultiply');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlExtendedLargeIntegerDivide(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlExtendedLargeIntegerDivide');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlExtendedMagicDivide(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlExtendedMagicDivide');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlFillMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlFillMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlFillMemoryUlong(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlFillMemoryUlong');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlFreeAnsiString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlFreeAnsiString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlFreeUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlFreeUnicodeString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlGetCallersAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlGetCallersAddress');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlInitAnsiString(
  DestinationString: PANSI_STRING; // OUT
  SourceString: PCSZ
  ); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuKrnl : RtlInitAnsiString' +
         #13#10'(' +
         #13#10'   DestinationString   : 0x%.08X' +
         #13#10'   SourceString        : 0x%.08X' +
         #13#10');',
         [DestinationString, SourceString]);

  JwaNative.RtlInitAnsiString(PANSI_STRING(DestinationString), PCSZ(SourceString));

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlInitUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlInitUnicodeString');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlInitializeCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:80
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DXBX_EXTENDED_DEBUG}
  DbgPrintf('EmuKrnl : RtlInitializeCriticalSection' +
           #13#10'(' +
           #13#10'   CriticalSection              : 0x%.08X' +
           #13#10');',
           [CriticalSection]);
{$ENDIF}

  JwaNative.RtlInitializeCriticalSection(CriticalSection);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlIntegerToChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlIntegerToChar');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlIntegerToUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlIntegerToUnicodeString');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlLeaveCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:80
begin
  EmuSwapFS(fsWindows);

  JwaNative.RtlLeaveCriticalSection(CriticalSection);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlLeaveCriticalSectionAndRegion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlLeaveCriticalSectionAndRegion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlLowerChar(
  Character: CHAR
  ): CHAR; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:80
begin
  EmuSwapFS(fsWindows);
//  Unimplemented('RtlLowerChar');
  Result := Low(Result);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlMapGenericMask(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlMapGenericMask');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlMoveMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlMoveMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlMultiByteToUnicodeN(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlMultiByteToUnicodeN');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlMultiByteToUnicodeSize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlMultiByteToUnicodeSize');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlNtStatusToDosError(
  Status: NTSTATUS
  ): ULONG; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuKrnl : RtlNtStatusToDosError' +
           #13#10'(' +
           #13#10'   Status              : 0x%.08X' +
           #13#10');',
           [Status]);

  Result := ULONG(JwaNative.RtlNtStatusToDosError(Status));

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlRaiseException(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlRaiseException');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlRaiseStatus(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlRaiseStatus');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlTimeFieldsToTime(
  TimeFields: PTIME_FIELDS;
  Time: PLARGE_INTEGER // OUT
  ): LONGBOOL; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  Result := JwaNative.RtlTimeFieldsToTime(TimeFields, Time);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlTimeToTimeFields(
  Time: PLARGE_INTEGER;
  TimeFields: PTIME_FIELDS // out
  ); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  JwaNative.RtlTimeToTimeFields(Time, TimeFields);
  
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlTryEnterCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ): LONGBOOL; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:80
begin
  EmuSwapFS(fsWindows);

  Result := JwaNative.RtlTryEnterCriticalSection(CriticalSection);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUlongByteSwap(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUlongByteSwap');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnicodeStringToAnsiString(
  DestinationString: PSTRING; // OUT
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: LONGBOOL
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUnicodeStringToAnsiString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnicodeStringToInteger(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUnicodeStringToInteger');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnicodeToMultiByteN(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUnicodeToMultiByteN');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnicodeToMultiByteSize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUnicodeToMultiByteSize');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnwind(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUnwind');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpcaseUnicodeChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUpcaseUnicodeChar');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpcaseUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUpcaseUnicodeString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpcaseUnicodeToMultiByteN(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUpcaseUnicodeToMultiByteN');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpperChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUpperChar');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpperString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUpperString');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUshortByteSwap(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlUshortByteSwap');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlWalkFrameChain(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlWalkFrameChain');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlZeroMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlZeroMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlRip(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlRip');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlSnprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlSnprintf');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlSprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlSprintf');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlVsnprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlVsnprintf');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlVsprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlVsprintf');
  EmuSwapFS(fsXbox);
end;


end.

