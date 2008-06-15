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
function xboxkrnl_RtlInitAnsiString(
  DestinationString: PANSI_STRING; // OUT
  SourceString: PCSZ
  ): NTSTATUS; stdcall;
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

function xboxkrnl_RtlAnsiStringToUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PSTRING;
  AllocateDestinationString: UCHAR
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlAnsiStringToUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlAppendStringToString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlAppendStringToString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlAppendUnicodeStringToString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlAppendUnicodeStringToString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlAppendUnicodeToString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlAppendUnicodeToString');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_RtlAssert(
  FailedAssertion: PVOID;
  FileName: PVOID;
  LineNumber: ULONG;
  _Message: PCHAR
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('RtlAssert');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCaptureContext(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCaptureContext');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCaptureStackBackTrace(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCaptureStackBackTrace');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCharToInteger(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCharToInteger');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCompareMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCompareMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCompareMemoryUlong(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCompareMemoryUlong');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCompareString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCompareString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCompareUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCompareUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCopyString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCopyString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCopyUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCopyUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCreateUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCreateUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlDowncaseUnicodeChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlDowncaseUnicodeChar');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlDowncaseUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlDowncaseUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_RtlEnterCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('RtlEnterCriticalSection');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlEnterCriticalSectionAndRegion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlEnterCriticalSectionAndRegion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlEqualString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlEqualString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlEqualUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlEqualUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlExtendedIntegerMultiply(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlExtendedIntegerMultiply');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlExtendedLargeIntegerDivide(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlExtendedLargeIntegerDivide');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlExtendedMagicDivide(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlExtendedMagicDivide');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlFillMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlFillMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlFillMemoryUlong(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlFillMemoryUlong');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlFreeAnsiString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlFreeAnsiString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlFreeUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlFreeUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlGetCallersAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlGetCallersAddress');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlInitAnsiString(
  DestinationString: PANSI_STRING; // OUT
  SourceString: PCSZ
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlInitAnsiString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlInitUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlInitUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_RtlInitializeCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('RtlInitializeCriticalSection');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlIntegerToChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlIntegerToChar');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlIntegerToUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlIntegerToUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_RtlLeaveCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('RtlLeaveCriticalSection');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlLeaveCriticalSectionAndRegion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlLeaveCriticalSectionAndRegion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlLowerChar(
  Character: CHAR
  ): CHAR; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('RtlLowerChar');
  Result := Low(Result);
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlMapGenericMask(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlMapGenericMask');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlMoveMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlMoveMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlMultiByteToUnicodeN(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlMultiByteToUnicodeN');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlMultiByteToUnicodeSize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlMultiByteToUnicodeSize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlNtStatusToDosError(
  Status: NTSTATUS
  ): ULONG; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlNtStatusToDosError');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlRaiseException(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlRaiseException');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlRaiseStatus(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlRaiseStatus');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlTimeFieldsToTime(
  TimeFields: PTIME_FIELDS;
  Time: PLARGE_INTEGER // OUT
  ): LONGBOOL; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('RtlTimeFieldsToTime');
  Result := Low(Result);
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_RtlTimeToTimeFields(
  Time: PLARGE_INTEGER;
  TimeFields: PTIME_FIELDS // out
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('RtlTimeToTimeFields');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlTryEnterCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ): LONGBOOL; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('RtlTryEnterCriticalSection');
  Result := Low(Result);
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUlongByteSwap(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUlongByteSwap');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUnicodeStringToAnsiString(
  DestinationString: PSTRING; // OUT
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: LONGBOOL
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUnicodeStringToAnsiString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUnicodeStringToInteger(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUnicodeStringToInteger');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUnicodeToMultiByteN(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUnicodeToMultiByteN');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUnicodeToMultiByteSize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUnicodeToMultiByteSize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUnwind(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUnwind');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUpcaseUnicodeChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUpcaseUnicodeChar');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUpcaseUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUpcaseUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUpcaseUnicodeToMultiByteN(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUpcaseUnicodeToMultiByteN');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUpperChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUpperChar');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUpperString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUpperString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUshortByteSwap(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUshortByteSwap');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlWalkFrameChain(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlWalkFrameChain');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlZeroMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlZeroMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlRip(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlRip');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlSnprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlSnprintf');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlSprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlSprintf');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlVsnprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlVsnprintf');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlVsprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlVsprintf');
  EmuSwapFS(); // Xbox FS
end;

end.

