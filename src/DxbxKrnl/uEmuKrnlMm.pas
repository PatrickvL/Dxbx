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

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
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
  uConsts,
  uTypes,
  uLog,
  uResourceTracker,
  uDxbxKrnlUtils,
  uEmuAlloc,
  uEmuFS,
  uEmu,
  uEmuKrnl,
  uDxbxKrnl;

var {102}xboxkrnl_MmGlobalData: array [0..8-1] of PVOID = (nil, nil, nil, nil, nil, nil, nil, nil);
// Source:?  Branch:Dxbx  Translator:PatrickvL  Done:0

function xboxkrnl_MmAllocateContiguousMemory(
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
function xboxkrnl_MmAllocateContiguousMemoryEx(
  NumberOfBytes: ULONG;
  LowestAcceptableAddress: PHYSICAL_ADDRESS;
  HighestAcceptableAddress: PHYSICAL_ADDRESS;
  Alignment: ULONG; //OPTIONAL
  ProtectionType: ULONG
  ): PVOID; stdcall;
function xboxkrnl_MmAllocateSystemMemory(
  NumberOfBytes: ULONG;
  Protect: ULONG
  ): PVOID; stdcall;
function xboxkrnl_MmClaimGpuInstanceMemory(
  NumberOfBytes: SIZE_T;
  NumberOfPaddingBytes: PSIZE_T // OUT
  ): PVOID; stdcall;
function xboxkrnl_MmCreateKernelStack(
  NumberOfBytes: ULONG;
  DebuggerThread: _BOOLEAN
  ): PVOID; stdcall;
procedure xboxkrnl_MmDeleteKernelStack(
  EndAddress: PVOID;
  BaseAddress: PVOID
  ); stdcall;
procedure xboxkrnl_MmFreeContiguousMemory(
  BaseAddress: PVOID
  ); stdcall;
function xboxkrnl_MmFreeSystemMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_MmGetPhysicalAddress(
  BaseAddress: PVOID
  ): PHYSICAL_ADDRESS; stdcall;
function xboxkrnl_MmIsAddressValid(
  VirtualAddress: PVOID
  ): _BOOLEAN; stdcall;
procedure xboxkrnl_MmLockUnlockBufferPages(
  BaseAddress: PVOID;
  NumberOfBytes: SIZE_T;
  UnlockPages: BOOLEAN
  ); stdcall;
procedure xboxkrnl_MmLockUnlockPhysicalPage(
  PhysicalAddress: PHYSICAL_ADDRESS;
  UnlockPage: _BOOLEAN
  ); stdcall;
function xboxkrnl_MmMapIoSpace(
  PhysicalAddress: PHYSICAL_ADDRESS;
  NumberOfBytes: ULONG;
  ProtectionType: ULONG
  ): PVOID; stdcall;
procedure xboxkrnl_MmPersistContiguousMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  Persist: LONGBOOL
  ); stdcall;
function xboxkrnl_MmQueryAddressProtect(
  VirtualAddress: PVOID
  ): ULONG; stdcall;
function xboxkrnl_MmQueryAllocationSize(
  BaseAddress: PVOID
  ): NTSTATUS; stdcall;
function xboxkrnl_MmQueryStatistics(
  MemoryStatistics: PMM_STATISTICS // OUT
  ): NTSTATUS; stdcall;
procedure xboxkrnl_MmSetAddressProtect(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  NewProtect: ULONG
  ); stdcall;
function xboxkrnl_MmUnmapIoSpace(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
function xboxkrnl_MmDbgAllocateMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmDbgFreeMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmDbgQueryAvailablePages(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmDbgReleaseAddress(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmDbgWriteCheck(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

const lfUnit = lfCxbx or lfKernel or lfMemory;

// MmAllocateContiguousMemory:
// Allocates a range of physically contiguous, cache-aligned memory from the
// non-paged pool (= main pool on XBOX).
//
// Differences from NT: HighestAcceptableAddress was deleted, opting instead
//     to not care about the highest address.
function xboxkrnl_MmAllocateContiguousMemory(
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pRet: PVOID;
  dwRet: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmAllocateContiguousMemory').
      _(NumberOfBytes, 'NumberOfBytes').
    LogEnd();


  //
  // Cxbx NOTE: Kludgey (but necessary) solution:
  //
  // Since this memory must be aligned on a page boundary, we must allocate an extra page
  // so that we can return a valid page aligned pointer
  //

  pRet := DxbxMalloc(NumberOfBytes + PAGE_SIZE);

  // align to page boundary
  begin
    dwRet := DWORD(pRet);
    Inc(dwRet, PAGE_SIZE - (dwRet mod PAGE_SIZE));
    g_AlignCache.insert({uiKey=}dwRet, {pResource=}pRet);
    pRet := PVOID(dwRet);
  end;

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : MmAllocateContiguous returned 0x%.08X', [pRet]);


  EmuSwapFS(fsXbox);
  Result := pRet;
end;

function xboxkrnl_MmAllocateContiguousMemoryEx(
  NumberOfBytes: ULONG;
  LowestAcceptableAddress: PHYSICAL_ADDRESS;
  HighestAcceptableAddress: PHYSICAL_ADDRESS;
  Alignment: ULONG; //OPTIONAL
  ProtectionType: ULONG
  ): PVOID; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
{$WRITEABLECONST ON}
const
  Count: Integer = 0;
{$WRITEABLECONST OFF}
var
  pRet: PVOID;
  dwRet: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmAllocateContiguousMemoryEx').
      _(NumberOfBytes, 'NumberOfBytes').
      _(LowestAcceptableAddress, 'LowestAcceptableAddress').
      _(HighestAcceptableAddress, 'HighestAcceptableAddress').
      _(Alignment, 'Alignment').
      _(ProtectionType, 'ProtectionType').
    LogEnd();

  //
  // NOTE: Kludgey (but necessary) solution:
  //
  // Since this memory must be aligned on a page boundary, we must allocate an extra page
  // so that we can return a valid page aligned pointer
  //

  pRet := DxbxMalloc(NumberOfBytes + PAGE_SIZE);

  // align to page boundary
  begin
    dwRet := DWORD(pRet);
    Inc(dwRet, PAGE_SIZE - (dwRet mod PAGE_SIZE));
    g_AlignCache.insert({uiKey=}dwRet, {pResource=}pRet);
    pRet := PVOID(dwRet);
  end;

  if IsRunning(TITLEID_Halo) then
  begin
    if Count < 4 then
      g_HaloHack[Count] := uint32(pRet);
    Inc(Count);
  end;

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : MmAllocateContiguousEx returned 0x%.08X', [pRet]);

  EmuSwapFS(fsXbox);

  Result := pRet;
end;

function xboxkrnl_MmAllocateSystemMemory(
  NumberOfBytes: ULONG;
  Protect: ULONG
  ): PVOID; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmAllocateSystemMemory').
      _(NumberOfBytes, 'NumberOfBytes').
      _(Protect, 'Protect').
    LogEnd();

  // TODO -oCXBX: should this be aligned?
  Result := DxbxMalloc(NumberOfBytes);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmClaimGpuInstanceMemory(
  NumberOfBytes: SIZE_T;
  NumberOfPaddingBytes: PSIZE_T // OUT
  ): PVOID; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := PVOID(Unimplemented('MmClaimGpuInstanceMemory'));
  EmuSwapFS(fsXbox);
end;

// Differences from NT: Custom stack size.
function xboxkrnl_MmCreateKernelStack(
  NumberOfBytes: ULONG;
  DebuggerThread: _BOOLEAN
  ): PVOID; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmCreateKernelStack').
      _(NumberOfBytes, 'NumberOfBytes').
      _(DebuggerThread, 'DebuggerThread').
    LogEnd();

  if (DebuggerThread <> FALSE) then
    EmuWarning('MmCreateKernelStack : DebuggerThread ignored');

  Result := NULL;
  if (FAILED(JwaNative.NtAllocateVirtualMemory(GetCurrentProcess(), @Result, 0, @NumberOfBytes, MEM_COMMIT, PAGE_READWRITE))) then
    EmuWarning('MmCreateKernelStack failed!')
  else
    Result := PVOID(ULONG(Result) + NumberOfBytes);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_MmDeleteKernelStack(
  EndAddress: PVOID;
  BaseAddress: PVOID
  ); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  RegionSize: ULONG;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmDeleteKernelStack').
      _(EndAddress, 'EndAddress').
      _(BaseAddress, 'BaseAddress').
    LogEnd();

  RegionSize := 0;
  if (FAILED(JwaNative.NtFreeVirtualMemory(GetCurrentProcess(), @BaseAddress, @RegionSize, MEM_RELEASE))) then
    EmuWarning('MmDeleteKernelStack failed!');

  EmuSwapFS(fsXbox);
end;

// MmFreeContiguousMemory:
// Frees memory allocated with MmAllocateContiguousMemory.
//
// Differences from NT: None.
procedure xboxkrnl_MmFreeContiguousMemory(
  BaseAddress: PVOID
  ); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  OrigBaseAddress: PVOID;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmFreeContiguousMemory').
      _(BaseAddress, 'BaseAddress').
    LogEnd();

  OrigBaseAddress := g_AlignCache.remove({uiKey=}uint32(BaseAddress));
  if OrigBaseAddress = nil then
    OrigBaseAddress := BaseAddress;

  if(OrigBaseAddress <> @xLaunchDataPage) then
  begin
    DxbxFree(OrigBaseAddress);
  end
  else
  begin
    if MayLog(lfUnit) then
      DbgPrintf('Ignored MmFreeContiguousMemory(&xLaunchDataPage)');

  end;

  // TODO -oDxbx: Sokoban crashes after this, at reset time (press Black + White to hit this).
  // Tracing in assembly shows the crash takes place quite a while further, so it's probably
  // not related to this call per-se. The strangest thing is, that if we let the debugger step
  // all the way through, the crash doesn't occur. Adding a Sleep(100) here doesn't help though.

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmFreeSystemMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmFreeSystemMemory').
      _(BaseAddress, 'BaseAddress').
      _(NumberOfBytes, 'NumberOfBytes').
    LogEnd();

  DxbxFree(BaseAddress);

  EmuSwapFS(fsXbox);
  Result := STATUS_SUCCESS;
end;

// MmGetPhysicalAddress:
// Translates a virtual address into a physical address.
//
// Differences from NT: PhysicalAddress is 32 bit, not 64.
function xboxkrnl_MmGetPhysicalAddress(
  BaseAddress: PVOID
  ): PHYSICAL_ADDRESS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:1
begin
  EmuSwapFS(fsWindows);
  Unimplemented('MmGetPhysicalAddress');
  Result := PHYSICAL_ADDRESS(BaseAddress); // Dxbx addition : For now, return the virtual address as if it was physical
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmIsAddressValid(
  VirtualAddress: PVOID
  ): _BOOLEAN; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := _BOOLEAN(Unimplemented('MmIsAddressValid'));
  // TODO -oDxbx : Could we use our IsValidAddress function for this perhaps?
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_MmLockUnlockBufferPages(
  BaseAddress: PVOID;
  NumberOfBytes: SIZE_T;
  UnlockPages: BOOLEAN
  ); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('MmLockUnlockBufferPages');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_MmLockUnlockPhysicalPage(
  PhysicalAddress: PHYSICAL_ADDRESS;
  UnlockPage: _BOOLEAN
  ); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('MmLockUnlockPhysicalPage');
  EmuSwapFS(fsXbox);
end;

// MmMapIoSpace:
// Maps a physical address area into the virtual address space.
// DO NOT USE MEMORY MAPPED WITH THIS AS A BUFFER TO OTHER CALLS.  For
// example, don't WriteFile or NtWriteFile these buffers.  Copy them first.
//
// Differences from NT: PhysicalAddress is 32 bit, not 64.  ProtectionType
//     specifies the page protections, but it's a Win32 PAGE_ macro instead
//     of the normal NT enumeration.  PAGE_READWRITE is probably what you
//     want...
function xboxkrnl_MmMapIoSpace(
  PhysicalAddress: PHYSICAL_ADDRESS;
  NumberOfBytes: ULONG;
  ProtectionType: ULONG
  ): PVOID; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('MmMapIoSpace');
  Result := nil;
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_MmPersistContiguousMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  Persist: LONGBOOL
  ); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmPersistContiguousMemory').
      _(BaseAddress, 'BaseAddress').
      _(NumberOfBytes, 'NumberOfBytes').
      _(Persist, 'Persist').
    LogEnd();

  // TODO -oCXBX: Actually set this up to be remember across a 'reboot'
  EmuWarning('MmPersistContiguousMemory is being ignored');

  // [PatrickvL] Shared memory would be a perfect fit for this,
  // but the supplied pointer is already allocated. In order to
  // change the 'shared' state of this memory, we would have to
  // obtain the complete memory range (via CreateFileMapping)
  // in one go, and use this for all allocation (much work, this).
  // Another way would be assume 'contiguous memory' is the
  // only type of memory being persisted, which would simplify
  // the allocation procedure significantly.
  // Another way could be to persist all registered blocks
  // of memory at application shutdown, but restoring it in
  // the next run at the same addresses could be troublesome.

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmQueryAddressProtect(
  VirtualAddress: PVOID
  ): ULONG; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmQueryAddressProtect');
  // TODO -oDxbx : Should we use VirtualQuery for implementing this, like in EmuCheckAllocationSize ?
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmQueryAllocationSize(
  BaseAddress: PVOID
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmQueryAllocationSize').
      _(BaseAddress, 'BaseAddress').
    LogEnd();

  Result := EmuCheckAllocationSize(BaseAddress, false);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmQueryStatistics(
  MemoryStatistics: PMM_STATISTICS // OUT
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  MemoryStatus: JwaWinBase.MEMORYSTATUS;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmQueryStatistics').
      _(MemoryStatistics, 'MemoryStatistics').
    LogEnd();

  if Assigned(MemoryStatistics) and (MemoryStatistics.Length >= SizeOf(MM_STATISTICS)) then
  begin
    GlobalMemoryStatus({var}MemoryStatus);

    // Dxbx addition : Fill each field :
    MemoryStatistics.TotalPhysicalPages := XBOX_MEMORY_SIZE div PAGE_SIZE; // 128 MB is enough for Debug Xbox and Chihiro
    MemoryStatistics.AvailablePages := MemoryStatus.dwAvailVirtual div PAGE_SIZE;
    MemoryStatistics.VirtualMemoryBytesReserved := MemoryStatus.dwTotalVirtual - MemoryStatus.dwAvailVirtual;
    // Was : MemoryStatistics.VirtualMemoryBytesReserved := MemoryStatus.dwTotalPhys - MemoryStatus.dwAvailPhys; // HACK (does this matter?)
    MemoryStatistics.CachePagesCommitted := 1;
    MemoryStatistics.PoolPagesCommitted := 1;
    MemoryStatistics.StackPagesCommitted := 1;
    MemoryStatistics.ImagePagesCommitted := 1;

    MemoryStatistics.VirtualMemoryBytesCommitted := PAGE_SIZE *
      (({NrAllocatedVirtualMemoryPages=}MemoryStatistics.VirtualMemoryBytesReserved div PAGE_SIZE) + MemoryStatistics.ImagePagesCommitted);

    Result := STATUS_SUCCESS;
  end
  else
    Result := STATUS_INVALID_PARAMETER;

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_MmSetAddressProtect(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  NewProtect: ULONG
  ); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwOldProtect: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmSetAddressProtect').
      _(BaseAddress, 'BaseAddress').
      _(NumberOfBytes, 'NumberOfBytes').
      _(NewProtect, 'NewProtect').
    LogEnd();


  if IsRunning(TITLEID_Halo) then
  begin
    // Halo Hack
    if(BaseAddress = PVOID($80366000)) then
    begin
      BaseAddress := PVOID((g_HaloHack[0] + ($80366000 - $80061000)));

      if MayLog(lfUnit) then
        DbgPrintf('EmuKrnl : Halo Access Adjust 3 was applied! (0x%.08X)', [BaseAddress]);
    end;
  end;

  if(not VirtualProtect(BaseAddress, NumberOfBytes, NewProtect and (not PAGE_WRITECOMBINE), @dwOldProtect)) then
    EmuWarning('VirtualProtect Failed!');

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : VirtualProtect was 0x%.08X -> 0x%.08X', [dwOldProtect, NewProtect and (not PAGE_WRITECOMBINE)]);


  EmuSwapFS(fsXbox);
end;

// MmUnmapIoSpace:
// Unmaps a virtual address mapping made by MmMapIoSpace.
//
// Differences from NT: None.
function xboxkrnl_MmUnmapIoSpace(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('MmUnmapIoSpace');
  Result := nil;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgAllocateMemory(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgAllocateMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgFreeMemory(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgFreeMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgQueryAvailablePages(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgQueryAvailablePages');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgReleaseAddress(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgReleaseAddress');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgWriteCheck(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgWriteCheck');
  EmuSwapFS(fsXbox);
end;

end.
