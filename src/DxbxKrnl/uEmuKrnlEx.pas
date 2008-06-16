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
  xboxkrnl_ExEventObjectType: POBJECT_TYPE = NULL;

function xboxkrnl_ExAcquireReadWriteLockExclusive(
  Arg1: DWORD
  ): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExAcquireReadWriteLockShared(
  Arg1: DWORD
  ): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExAllocatePool(
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
function xboxkrnl_ExAllocatePoolWithTag(
  NumberOfBytes: SIZE_T;
  Tag: ULONG
  ): PVOID; stdcall;
procedure xboxkrnl_ExFreePool(
  Block: PVOID
  ); stdcall;
function xboxkrnl_ExInitializeReadWriteLock(
  Arg1: DWORD
  ): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExInterlockedAddLargeInteger(
  Addend: PLARGE_INTEGER;
  Increment: LARGE_INTEGER;
  Lock: PKSPIN_LOCK   
  ): LARGE_INTEGER; stdcall;
procedure xboxkrnl_ExInterlockedAddLargeStatistic(
  Addend: PLARGE_INTEGER;
  Increment: ULONG
  ); stdcall;
function xboxkrnl_ExInterlockedCompareExchange64(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExMutantObjectType(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExQueryPoolBlockSize(
  PoolBlock: PVOID;
  QuotaCharged: PBOOLEAN // OUT
  ): SIZE_T; stdcall;
function xboxkrnl_ExQueryNonVolatileSetting(
  ValueIndex: DWORD;
  _Type: PDWORD; // out
  Value: PUCHAR; // out
  ValueLength: SIZE_T;
  ResultLength: PSIZE_T // out, OPTIONAL
  ): NTSTATUS; stdcall;
function xboxkrnl_ExReadWriteRefurbInfo(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExRaiseException(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExRaiseStatus(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExReleaseReadWriteLock(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExSaveNonVolatileSetting(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExSemaphoreObjectType(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExTimerObjectType(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExfInterlockedInsertHeadList(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExfInterlockedInsertTailList(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExfInterlockedRemoveHeadList(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

function xboxkrnl_ExAcquireReadWriteLockExclusive(
  Arg1: DWORD
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAcquireReadWriteLockExclusive');
  // KeWaitForSingleObject
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExAcquireReadWriteLockShared(
  Arg1: DWORD
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAcquireReadWriteLockShared');
  // KeWaitForSingleObject
  EmuSwapFS(); // Xbox FS
end;

// Differences from NT: There is no PoolType field, as the XBOX
// only has 1 pool, the non-paged pool.
function xboxkrnl_ExAllocatePool(
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExAllocatePool');
  Result := xboxkrnl_ExAllocatePoolWithTag(NumberOfBytes, ULONG($656E6F4E{?}));
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExAllocatePoolWithTag(
  NumberOfBytes: SIZE_T;
  Tag: ULONG
  ): PVOID; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
//  RtlAssert(NumberOfBytes > 0);
  Unimplemented('ExAllocatePoolWithTag');
  Result := nil;
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_ExFreePool(
  Block: PVOID
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExFreePool');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExInitializeReadWriteLock(
  Arg1: DWORD
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExInitializeReadWriteLock');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExInterlockedAddLargeInteger(
  Addend: PLARGE_INTEGER;
  Increment: LARGE_INTEGER;
  Lock: PKSPIN_LOCK   
  ): LARGE_INTEGER; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExInterlockedAddLargeInteger');
  Result.QuadPart := 0;
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_ExInterlockedAddLargeStatistic(
  Addend: PLARGE_INTEGER;
  Increment: ULONG
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('ExInterlockedAddLargeStatistic');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExInterlockedCompareExchange64(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExInterlockedCompareExchange64');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExMutantObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExMutantObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExQueryPoolBlockSize(
  PoolBlock: PVOID;
  QuotaCharged: PBOOLEAN // OUT
  ): SIZE_T; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExQueryPoolBlockSize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExQueryNonVolatileSetting(
  ValueIndex: DWORD;
  _Type: PDWORD; // out
  Value: PUCHAR; // out
  ValueLength: SIZE_T;
  ResultLength: PSIZE_T // out, OPTIONAL
): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExQueryNonVolatileSetting');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExReadWriteRefurbInfo(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExReadWriteRefurbInfo');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExRaiseException(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExRaiseException');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExRaiseStatus(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExRaiseStatus');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExReleaseReadWriteLock(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExReleaseReadWriteLock');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExSaveNonVolatileSetting(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExSaveNonVolatileSetting');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExSemaphoreObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExSemaphoreObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExTimerObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExTimerObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExfInterlockedInsertHeadList(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExfInterlockedInsertHeadList');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExfInterlockedInsertTailList(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExfInterlockedInsertTailList');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExfInterlockedRemoveHeadList(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExfInterlockedRemoveHeadList');
  EmuSwapFS(); // Xbox FS
end;

end.

