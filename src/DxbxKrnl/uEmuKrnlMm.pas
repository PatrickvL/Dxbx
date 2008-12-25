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
  uEmu,
  uEmuAlloc,
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
procedure xboxkrnl_MmPersistContiguousMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  Persist: LONGBOOL
  ); stdcall;
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
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmGlobalData');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmAllocateContiguousMemory(
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
var
  pRet: PVOID;
  dwRet: DWORD;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuKrnl : MmAllocateContiguousMemory' +
         #13#10'(' +
         #13#10'   NumberOfBytes            : 0x%.08X' +
         #13#10');',
         [NumberOfBytes]);

  //
  // Cxbx NOTE: Kludgey (but necessary) solution:
  //
  // Since this memory must be aligned on a page boundary, we must allocate an extra page
  // so that we can return a valid page aligned pointer
  //

  pRet := CxbxMalloc(NumberOfBytes + $1000);

  // align to page boundary
  begin
    dwRet := DWORD(pRet);

    Inc(dwRet, $1000 - (dwRet mod $1000));

// Dxbx TODO :    g_AlignCache.insert(dwRet, pRet);

    pRet := PVOID(dwRet);
  end;

  DbgPrintf('EmuKrnl : MmAllocateContiguous returned 0x%.08X', [pRet]);

  EmuSwapFS(fsXbox);

  Result :=  pRet;
end;

function xboxkrnl_MmAllocateContiguousMemoryEx(
  NumberOfBytes: ULONG;
  LowestAcceptableAddress: PHYSICAL_ADDRESS;
  HighestAcceptableAddress: PHYSICAL_ADDRESS;
  Alignment: ULONG; //OPTIONAL
  ProtectionType: ULONG
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmAllocateContiguousMemoryEx');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmAllocateSystemMemory(
  NumberOfBytes: ULONG;
  Protect: ULONG
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmAllocateSystemMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmClaimGpuInstanceMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmClaimGpuInstanceMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmCreateKernelStack(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmCreateKernelStack');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDeleteKernelStack(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDeleteKernelStack');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmFreeContiguousMemory(
  BaseAddress: PVOID
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmFreeContiguousMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmFreeSystemMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmFreeSystemMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmGetPhysicalAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmGetPhysicalAddress');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmIsAddressValid(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmIsAddressValid');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmLockUnlockBufferPages(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmLockUnlockBufferPages');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmLockUnlockPhysicalPage(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmLockUnlockPhysicalPage');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmMapIoSpace(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmMapIoSpace');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_MmPersistContiguousMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  Persist: LONGBOOL
  ); stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuKrnl : MmPersistContiguousMemory' +
         #13#10'(' +
         #13#10'   BaseAddress              : 0x%.08X' +
         #13#10'   NumberOfBytes            : 0x%.08X' +
         #13#10'   Persist                  : 0x%.08X' +
         #13#10');',
         [BaseAddress, NumberOfBytes, Persist]);

  // Cxbx TODO: Actually set this up to be remember across a 'reboot'
  EmuWarning('MmPersistContiguousMemory is being ignored');

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmQueryAddressProtect(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmQueryAddressProtect');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmQueryAllocationSize(
  BaseAddress: PVOID
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmQueryAllocationSize');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmQueryStatistics(
  MemoryStatistics: PMM_STATISTICS // out
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmQueryStatistics');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmSetAddressProtect(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  NewProtect: ULONG
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmSetAddressProtect');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmUnmapIoSpace(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmUnmapIoSpace');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgAllocateMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgAllocateMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgFreeMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgFreeMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgQueryAvailablePages(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgQueryAvailablePages');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgReleaseAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgReleaseAddress');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgWriteCheck(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgWriteCheck');
  EmuSwapFS(fsXbox);
end;

end.
