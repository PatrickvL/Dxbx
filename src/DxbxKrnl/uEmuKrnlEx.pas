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

function xboxkrnl_ExAcquireReadWriteLockExclusive(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExAcquireReadWriteLockShared(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExAllocatePool(NumberOfBytes: ULONG): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExAllocatePoolWithTag(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExEventObjectType(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExFreePool(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExInitializeReadWriteLock(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExInterlockedAddLargeInteger(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExInterlockedAddLargeStatistic(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExInterlockedCompareExchange64(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExMutantObjectType(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ExQueryPoolBlockSize(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
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

function xboxkrnl_ExAcquireReadWriteLockExclusive(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAcquireReadWriteLockExclusive');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExAcquireReadWriteLockShared(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAcquireReadWriteLockShared');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExAllocatePool(NumberOfBytes: ULONG): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAllocatePool');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExAllocatePoolWithTag(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAllocatePoolWithTag');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExEventObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExEventObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExFreePool(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExFreePool');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExInitializeReadWriteLock(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExInitializeReadWriteLock');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExInterlockedAddLargeInteger(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExInterlockedAddLargeInteger');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExInterlockedAddLargeStatistic(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExInterlockedAddLargeStatistic');
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

function xboxkrnl_ExQueryPoolBlockSize(): NTSTATUS; stdcall;
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

