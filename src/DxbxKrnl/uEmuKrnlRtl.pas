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

function xboxkrnl_RtlAnsiStringToUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendStringToString(): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendUnicodeStringToString(): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendUnicodeToString(): NTSTATUS; stdcall;
function xboxkrnl_RtlAssert(): NTSTATUS; stdcall;
function xboxkrnl_RtlCaptureContext(): NTSTATUS; stdcall;
function xboxkrnl_RtlCaptureStackBackTrace(): NTSTATUS; stdcall;
function xboxkrnl_RtlCharToInteger(): NTSTATUS; stdcall;
function xboxkrnl_RtlCompareMemory(): NTSTATUS; stdcall;
function xboxkrnl_RtlCompareMemoryUlong(): NTSTATUS; stdcall;
function xboxkrnl_RtlCompareString(): NTSTATUS; stdcall;
function xboxkrnl_RtlCompareUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlCopyString(): NTSTATUS; stdcall;
function xboxkrnl_RtlCopyUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlCreateUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlDowncaseUnicodeChar(): NTSTATUS; stdcall;
function xboxkrnl_RtlDowncaseUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlEnterCriticalSection(): NTSTATUS; stdcall;
function xboxkrnl_RtlEnterCriticalSectionAndRegion(): NTSTATUS; stdcall;
function xboxkrnl_RtlEqualString(): NTSTATUS; stdcall;
function xboxkrnl_RtlEqualUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlExtendedIntegerMultiply(): NTSTATUS; stdcall;
function xboxkrnl_RtlExtendedLargeIntegerDivide(): NTSTATUS; stdcall;
function xboxkrnl_RtlExtendedMagicDivide(): NTSTATUS; stdcall;
function xboxkrnl_RtlFillMemory(): NTSTATUS; stdcall;
function xboxkrnl_RtlFillMemoryUlong(): NTSTATUS; stdcall;
function xboxkrnl_RtlFreeAnsiString(): NTSTATUS; stdcall;
function xboxkrnl_RtlFreeUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlGetCallersAddress(): NTSTATUS; stdcall;
function xboxkrnl_RtlInitAnsiString(): NTSTATUS; stdcall;
function xboxkrnl_RtlInitUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlInitializeCriticalSection(): NTSTATUS; stdcall;
function xboxkrnl_RtlIntegerToChar(): NTSTATUS; stdcall;
function xboxkrnl_RtlIntegerToUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlLeaveCriticalSection(): NTSTATUS; stdcall;
function xboxkrnl_RtlLeaveCriticalSectionAndRegion(): NTSTATUS; stdcall;
function xboxkrnl_RtlLowerChar(): NTSTATUS; stdcall;
function xboxkrnl_RtlMapGenericMask(): NTSTATUS; stdcall;
function xboxkrnl_RtlMoveMemory(): NTSTATUS; stdcall;
function xboxkrnl_RtlMultiByteToUnicodeN(): NTSTATUS; stdcall;
function xboxkrnl_RtlMultiByteToUnicodeSize(): NTSTATUS; stdcall;
function xboxkrnl_RtlNtStatusToDosError(): NTSTATUS; stdcall;
function xboxkrnl_RtlRaiseException(): NTSTATUS; stdcall;
function xboxkrnl_RtlRaiseStatus(): NTSTATUS; stdcall;
function xboxkrnl_RtlTimeFieldsToTime(): NTSTATUS; stdcall;
function xboxkrnl_RtlTimeToTimeFields(): NTSTATUS; stdcall;
function xboxkrnl_RtlTryEnterCriticalSection(): NTSTATUS; stdcall;
function xboxkrnl_RtlUlongByteSwap(): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeStringToAnsiString(): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeStringToInteger(): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeToMultiByteN(): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeToMultiByteSize(): NTSTATUS; stdcall;
function xboxkrnl_RtlUnwind(): NTSTATUS; stdcall;
function xboxkrnl_RtlUpcaseUnicodeChar(): NTSTATUS; stdcall;
function xboxkrnl_RtlUpcaseUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlUpcaseUnicodeToMultiByteN(): NTSTATUS; stdcall;
function xboxkrnl_RtlUpperChar(): NTSTATUS; stdcall;
function xboxkrnl_RtlUpperString(): NTSTATUS; stdcall;
function xboxkrnl_RtlUshortByteSwap(): NTSTATUS; stdcall;
function xboxkrnl_RtlWalkFrameChain(): NTSTATUS; stdcall;
function xboxkrnl_RtlZeroMemory(): NTSTATUS; stdcall;
function xboxkrnl_RtlRip(): NTSTATUS; stdcall;
function xboxkrnl_RtlSnprintf(): NTSTATUS; stdcall;
function xboxkrnl_RtlSprintf(): NTSTATUS; stdcall;
function xboxkrnl_RtlVsnprintf(): NTSTATUS; stdcall;
function xboxkrnl_RtlVsprintf(): NTSTATUS; stdcall;

implementation

function xboxkrnl_RtlAnsiStringToUnicodeString(): NTSTATUS; stdcall;
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

function xboxkrnl_RtlAssert(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlAssert');
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

function xboxkrnl_RtlEnterCriticalSection(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlEnterCriticalSection');
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

function xboxkrnl_RtlInitAnsiString(): NTSTATUS; stdcall;
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

function xboxkrnl_RtlInitializeCriticalSection(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlInitializeCriticalSection');
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

function xboxkrnl_RtlLeaveCriticalSection(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlLeaveCriticalSection');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlLeaveCriticalSectionAndRegion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlLeaveCriticalSectionAndRegion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlLowerChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlLowerChar');
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

function xboxkrnl_RtlNtStatusToDosError(): NTSTATUS; stdcall;
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

function xboxkrnl_RtlTimeFieldsToTime(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlTimeFieldsToTime');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlTimeToTimeFields(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlTimeToTimeFields');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlTryEnterCriticalSection(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlTryEnterCriticalSection');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUlongByteSwap(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUlongByteSwap');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUnicodeStringToAnsiString(): NTSTATUS; stdcall;
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

