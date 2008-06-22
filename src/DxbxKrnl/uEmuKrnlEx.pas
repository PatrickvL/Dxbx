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
unit uEmuKrnlEx;

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

var
  {016}xboxkrnl_ExEventObjectType: POBJECT_TYPE = NULL;
  {022}xboxkrnl_ExMutantObjectType: POBJECT_TYPE = NULL;
  {030}xboxkrnl_ExSemaphoreObjectType: POBJECT_TYPE = NULL;
  {031}xboxkrnl_ExTimerObjectType: POBJECT_TYPE = NULL;

function {012}xboxkrnl_ExAcquireReadWriteLockExclusive(
  Arg1: DWORD
  ): NTSTATUS; stdcall; // Source: APILogger - Uncertain
function {013}xboxkrnl_ExAcquireReadWriteLockShared(
  Arg1: DWORD
  ): NTSTATUS; stdcall; // Source: APILogger - Uncertain
function {014}xboxkrnl_ExAllocatePool(
  NumberOfBytes: ULONG
  ): PVOID; stdcall; // Source: OpenXDK
function {015}xboxkrnl_ExAllocatePoolWithTag(
  NumberOfBytes: SIZE_T;
  Tag: ULONG
  ): PVOID; stdcall; // Source: OpenXDK
procedure {017}xboxkrnl_ExFreePool(
  Block: PVOID
  ); stdcall; // Source: ReactOS
function {018}xboxkrnl_ExInitializeReadWriteLock(
  Arg1: DWORD
  ): NTSTATUS; stdcall; // Source: APILogger - Uncertain
function {019}xboxkrnl_ExInterlockedAddLargeInteger(
  Addend: PLARGE_INTEGER;
  Increment: LARGE_INTEGER;
  Lock: PKSPIN_LOCK
  ): LARGE_INTEGER; stdcall; // Source: ReactOS
procedure {020}xboxkrnl_ExInterlockedAddLargeStatistic(
  Addend: PLARGE_INTEGER;
  Increment: ULONG
  ); stdcall; // Source: ReactOS
function {021}xboxkrnl_ExInterlockedCompareExchange64(
  Destination: PLONGLONG; // OUT
  Exchange: PLONGLONG;
  Comparand: PLONGLONG;
  Lock: PKSPIN_LOCK
  ): LONGLONG; stdcall; // Source: ReactOS
function {023}xboxkrnl_ExQueryPoolBlockSize(
  PoolBlock: PVOID;
  QuotaCharged: PBOOLEAN // OUT
  ): SIZE_T; stdcall; // Source: ReactOS
function {024}xboxkrnl_ExQueryNonVolatileSetting(
  ValueIndex: DWORD;
  _Type: PDWORD; // out
  Value: PUCHAR; // out
  ValueLength: SIZE_T;
  ResultLength: PSIZE_T // out, OPTIONAL
  ): NTSTATUS; stdcall; // Source: OpenXDK
function {025}xboxkrnl_ExReadWriteRefurbInfo(
  Arg1: PXBOX_REFURB_INFO;
  Arg2Size: DWORD;
  Arg3: LONGBOOL
  ): NTSTATUS; stdcall; // Source: XBMC - Uncertain
procedure {026}xboxkrnl_ExRaiseException(
  ExceptionRecord: PEXCEPTION_RECORD
  ); stdcall; // Source: ReactOS
procedure {027}xboxkrnl_ExRaiseStatus(
  Status: NTSTATUS
  ); stdcall; // Source: ReactOS
function {028}xboxkrnl_ExReleaseReadWriteLock(
  Arg1: DWORD
  ): NTSTATUS; stdcall; // Source: APILogger - Uncertain
function {029}xboxkrnl_ExSaveNonVolatileSetting(
  ValueIndex: DWORD;
  _Type: PDWORD; //   OUT
  Value: PUCHAR;
  ValueLength: SIZE_T
  ): NTSTATUS; stdcall; // Source: OpenXDK
function {032}xboxkrnl_ExfInterlockedInsertHeadList(
  ListHead: PLIST_ENTRY;
  ListEntry: PLIST_ENTRY;
  Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; stdcall; // Source: ReactOS
function {033}xboxkrnl_ExfInterlockedInsertTailList(
  ListHead: PLIST_ENTRY;
  ListEntry: PLIST_ENTRY;
  Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; stdcall; // Source: ReactOS
function {034}xboxkrnl_ExfInterlockedRemoveHeadList(
  ListHead: PLIST_ENTRY;
  Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; stdcall; // Source: ReactOS


implementation

function {012}xboxkrnl_ExAcquireReadWriteLockExclusive(
  Arg1: DWORD
  ): NTSTATUS; stdcall; // Source: APILogger - Uncertain
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAcquireReadWriteLockExclusive');
  // KeWaitForSingleObject
  EmuSwapFS(); // Xbox FS
end;

function {013}xboxkrnl_ExAcquireReadWriteLockShared(
  Arg1: DWORD
  ): NTSTATUS; stdcall; // Source: APILogger - Uncertain
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAcquireReadWriteLockShared');
  // KeWaitForSingleObject
  EmuSwapFS(); // Xbox FS
end;

// Differences from NT: There is no PoolType field, as the XBOX
// only has 1 pool, the non-paged pool.
function {014}xboxkrnl_ExAllocatePool(
  NumberOfBytes: ULONG
  ): PVOID; stdcall; // Source: OpenXDK
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExAllocatePool');
  Result := xboxkrnl_ExAllocatePoolWithTag(NumberOfBytes, ULONG($656E6F4E{?}));
  EmuSwapFS(); // Xbox FS
end;

function {015}xboxkrnl_ExAllocatePoolWithTag(
  NumberOfBytes: SIZE_T;
  Tag: ULONG
  ): PVOID; stdcall; // Source: OpenXDK
begin
  EmuSwapFS(); // Win2k/XP FS
//  RtlAssert(NumberOfBytes > 0);
  Unimplemented('ExAllocatePoolWithTag');
  Result := nil;
  EmuSwapFS(); // Xbox FS
end;

procedure {017}xboxkrnl_ExFreePool(
  Block: PVOID
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExFreePool');
  // ExFreeNonPagedPool
  EmuSwapFS(); // Xbox FS
end;

function {018}xboxkrnl_ExInitializeReadWriteLock(
  Arg1: DWORD
  ): NTSTATUS; stdcall; // Source: APILogger - Uncertain
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExInitializeReadWriteLock');
  EmuSwapFS(); // Xbox FS
end;

function {019}xboxkrnl_ExInterlockedAddLargeInteger(
  Addend: PLARGE_INTEGER;
  Increment: LARGE_INTEGER;
  Lock: PKSPIN_LOCK
  ): LARGE_INTEGER; stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExInterlockedAddLargeInteger');
  Result.QuadPart := 0;
  EmuSwapFS(); // Xbox FS
end;

procedure {020}xboxkrnl_ExInterlockedAddLargeStatistic(
  Addend: PLARGE_INTEGER;
  Increment: ULONG
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExInterlockedAddLargeStatistic');
  EmuSwapFS(); // Xbox FS
end;

function {021}xboxkrnl_ExInterlockedCompareExchange64(
  Destination: PLONGLONG; // OUT
  Exchange: PLONGLONG;
  Comparand: PLONGLONG;
  Lock: PKSPIN_LOCK
  ): LONGLONG; stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExInterlockedCompareExchange64');
  EmuSwapFS(); // Xbox FS
end;

function {023}xboxkrnl_ExQueryPoolBlockSize(
  PoolBlock: PVOID;
  QuotaCharged: PBOOLEAN // OUT
  ): SIZE_T; stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExQueryPoolBlockSize');
  EmuSwapFS(); // Xbox FS
end;

// ExQueryNonVolatileSetting retrieves EEPROM information -
// this function, when first called, creates a "shadow" copy
// of the EEPROM in RAM which is used in subsequent calls to Query,
// and updated by ExSaveNonVolatileSetting.
function {024}xboxkrnl_ExQueryNonVolatileSetting(
  ValueIndex: DWORD;
  _Type: PDWORD; // out
  Value: PUCHAR; // out
  ValueLength: SIZE_T;
  ResultLength: PSIZE_T // out, OPTIONAL
  ): NTSTATUS; stdcall; // Source: OpenXDK
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExQueryNonVolatileSetting');
  EmuSwapFS(); // Xbox FS
end;

function {025}xboxkrnl_ExReadWriteRefurbInfo(
  Arg1: PXBOX_REFURB_INFO;
  Arg2Size: DWORD;
  Arg3: LONGBOOL
  ): NTSTATUS; stdcall; // Source: XBMC - Uncertain
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExReadWriteRefurbInfo');
  EmuSwapFS(); // Xbox FS
end;

procedure {026}xboxkrnl_ExRaiseException(
  ExceptionRecord: PEXCEPTION_RECORD
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExRaiseException');
  // RtlRaiseException(ExceptionRecord);
  EmuSwapFS(); // Xbox FS
end;

procedure {027}xboxkrnl_ExRaiseStatus(
  Status: NTSTATUS
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExRaiseStatus');
  EmuSwapFS(); // Xbox FS
end;

function {028}xboxkrnl_ExReleaseReadWriteLock(
  Arg1: DWORD
  ): NTSTATUS; stdcall; // Source: APILogger - Uncertain
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExReleaseReadWriteLock');
  EmuSwapFS(); // Xbox FS
end;

function {029}xboxkrnl_ExSaveNonVolatileSetting(
  ValueIndex: DWORD;
  _Type: PDWORD; //   OUT
  Value: PUCHAR;
  ValueLength: SIZE_T
  ): NTSTATUS; stdcall; // Source: OpenXDK
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExSaveNonVolatileSetting');
  EmuSwapFS(); // Xbox FS
end;

function {032}xboxkrnl_ExfInterlockedInsertHeadList(
  ListHead: PLIST_ENTRY;
  ListEntry: PLIST_ENTRY;
  Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExfInterlockedInsertHeadList');
  Result := nil;
  EmuSwapFS(); // Xbox FS
end;

function {033}xboxkrnl_ExfInterlockedInsertTailList(
  ListHead: PLIST_ENTRY;
  ListEntry: PLIST_ENTRY;
  Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExfInterlockedInsertTailList');
  Result := nil;
  EmuSwapFS(); // Xbox FS
end;

function {034}xboxkrnl_ExfInterlockedRemoveHeadList(
  ListHead: PLIST_ENTRY;
  Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExfInterlockedRemoveHeadList');
  Result := nil;
  EmuSwapFS(); // Xbox FS
end;

end.

