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
unit uEmuKrnlMm;

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

function xboxkrnl_MmGlobalData(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmAllocateContiguousMemory(
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
function xboxkrnl_MmAllocateContiguousMemoryEx(
  NumberOfBytes: ULONG;
  LowestAcceptableAddress: PHYSICAL_ADDRESS;
  HighestAcceptableAddress: PHYSICAL_ADDRESS;
  Alignment: ULONG; //OPTIONAL
  ProtectionType: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_MmAllocateSystemMemory(
  NumberOfBytes: ULONG;
  Protect: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_MmClaimGpuInstanceMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmCreateKernelStack(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmDeleteKernelStack(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmFreeContiguousMemory(
  BaseAddress: PVOID
  ): NTSTATUS; stdcall;
function xboxkrnl_MmFreeSystemMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_MmGetPhysicalAddress(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmIsAddressValid(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmLockUnlockBufferPages(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmLockUnlockPhysicalPage(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmMapIoSpace(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmPersistContiguousMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  Persist: LONGBOOL
  ): NTSTATUS; stdcall;
function xboxkrnl_MmQueryAddressProtect(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmQueryAllocationSize(
  BaseAddress: PVOID
  ): NTSTATUS; stdcall;
function xboxkrnl_MmQueryStatistics(
  MemoryStatistics: PMM_STATISTICS // out
  ): NTSTATUS; stdcall;
function xboxkrnl_MmSetAddressProtect(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  NewProtect: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_MmUnmapIoSpace(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmDbgAllocateMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmDbgFreeMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmDbgQueryAvailablePages(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmDbgReleaseAddress(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmDbgWriteCheck(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

function xboxkrnl_MmGlobalData(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmGlobalData');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmAllocateContiguousMemory(
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('MmAllocateContiguousMemory');
  Result := NULL;
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmAllocateContiguousMemoryEx(
  NumberOfBytes: ULONG;
  LowestAcceptableAddress: PHYSICAL_ADDRESS;
  HighestAcceptableAddress: PHYSICAL_ADDRESS;
  Alignment: ULONG; //OPTIONAL
  ProtectionType: ULONG
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmAllocateContiguousMemoryEx');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmAllocateSystemMemory(
  NumberOfBytes: ULONG;
  Protect: ULONG 
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmAllocateSystemMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmClaimGpuInstanceMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmClaimGpuInstanceMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmCreateKernelStack(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmCreateKernelStack');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDeleteKernelStack(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDeleteKernelStack');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmFreeContiguousMemory(
  BaseAddress: PVOID 
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmFreeContiguousMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmFreeSystemMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmFreeSystemMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmGetPhysicalAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmGetPhysicalAddress');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmIsAddressValid(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmIsAddressValid');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmLockUnlockBufferPages(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmLockUnlockBufferPages');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmLockUnlockPhysicalPage(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmLockUnlockPhysicalPage');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmMapIoSpace(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmMapIoSpace');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmPersistContiguousMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  Persist: LONGBOOL 
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmPersistContiguousMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmQueryAddressProtect(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmQueryAddressProtect');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmQueryAllocationSize(
  BaseAddress: PVOID
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmQueryAllocationSize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmQueryStatistics(
  MemoryStatistics: PMM_STATISTICS // out
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmQueryStatistics');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmSetAddressProtect(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  NewProtect: ULONG 
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmSetAddressProtect');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmUnmapIoSpace(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmUnmapIoSpace');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDbgAllocateMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDbgAllocateMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDbgFreeMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDbgFreeMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDbgQueryAvailablePages(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDbgQueryAvailablePages');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDbgReleaseAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDbgReleaseAddress');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDbgWriteCheck(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDbgWriteCheck');
  EmuSwapFS(); // Xbox FS
end;

end.

