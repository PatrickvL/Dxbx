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
  JwaWinBase,
  JwaWinType,
  JwaNative,
  JwaWinNT,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uConsts,
  uTypes,
  uLog,
  uDxbxUtils,
  uResourceTracker,
  uDxbxKrnlUtils,
  uEmu,
  uEmuAlloc,
  uEmuFS,
  uEmuKrnl;

var {102}xboxkrnl_MmGlobalData: array [0..8-1] of PVOID = (nil, nil, nil, nil, nil, nil, nil, nil);
// Source:?  Branch:Dxbx  Translator:PatrickvL  Done:0

type
  RDxbxAllocationInfo = record
    PageNr: uint;
    BaseAddress: PVOID;
    Offset: uint;
    AllocatedSize: ULONG;
  end;

function DxbxGetAllocationInfo(const XboxData: UIntPtr): RDxbxAllocationInfo;
function DxbxGetNativeContiguousMemoryAddress(const XboxData: UIntPtr): Pointer;
// check the allocation size of a given virtual address
function EmuCheckAllocationSize(pBase: PVOID; largeBound: _bool): int;

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

const
  X_NR_PAGES = XBOX_MEMORY_SIZE div PAGE_SIZE; // NrPages = 64 MB / 4 Kb (PageSize) = 16384 pages
  X_PAGES_MASK = X_NR_PAGES - 1;

var
  MmAllocatedSize: array [0..X_NR_PAGES-1] of ULONG;
  MmAllocatedBase: array [0..X_NR_PAGES-1] of PVOID;

function DxbxGetPageNr(const XboxData: UIntPtr): uint;
begin
  Result := (XboxData shr PAGE_SHIFT) and X_PAGES_MASK;
end;

function DxbxGetAllocationInfo(const XboxData: UIntPtr): RDxbxAllocationInfo;
var
  PageNr: uint;
  BaseAddress: PVOID;
begin
  // Calculate the page of this address and look up the actual base address :
  PageNr := DxbxGetPageNr(XboxData);
  BaseAddress := MmAllocatedBase[PageNr];

  // Calculate if the given address is an offset into the real allocation :
  Result.Offset := XboxData - UIntPtr(BaseAddress);
  Result.BaseAddress := BaseAddress;

  // See if we need to adjust the real page number :
  if Result.Offset > 0 then
    PageNr := DxbxGetPageNr(UIntPtr(BaseAddress));

  Result.PageNr := PageNr;
  // Get the number of bytes that where allocated :
  Result.AllocatedSize := MmAllocatedSize[PageNr];
end;

function DxbxGetNativeContiguousMemoryAddress(const XboxData: UIntPtr): Pointer;
var
  Info: RDxbxAllocationInfo;
begin
  // Get the allocation information for this address :
  Info := DxbxGetAllocationInfo(XboxData);
  // Return the native address for this
  Result := Pointer(UIntPtr(Info.BaseAddress) + Info.Offset);
end;

// check how many bytes were allocated for a structure
function EmuCheckAllocationSize(pBase: PVOID; largeBound: _bool): Integer;
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
var
  Info: RDxbxAllocationInfo;
//  MemoryBasicInfo: MEMORY_BASIC_INFORMATION;
//  dwRet: DWORD;
begin
  Info := DxbxGetAllocationInfo(UIntPtr(pBase));
  Result := Info.AllocatedSize - Info.Offset;
(*
{$IFDEF _DEBUG_ALLOC}
  dwRet := DxbxVirtualQueryDebug(pBase, MemoryBasicInfo, SizeOf(MemoryBasicInfo));
  if (dwRet = -1) then
{$ENDIF}
    dwRet := VirtualQuery(pBase, {var}MemoryBasicInfo, SizeOf(MemoryBasicInfo));

  if dwRet = 0 then
  begin
    Result := 0;
    Exit;
  end;

  if MemoryBasicInfo.State <> MEM_COMMIT then
  begin
    Result := 0;
    Exit;
  end;

  // this is a hack in order to determine when pointers come from a large write-combined database
  if largeBound and (MemoryBasicInfo.RegionSize > (5 * 1024 * 1024)) then
  begin
    Result := -1;
    Exit;
  end;

  Result := Integer(MemoryBasicInfo.RegionSize) - (IntPtr(pBase) - IntPtr(MemoryBasicInfo.BaseAddress));
*)
end;

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
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmAllocateContiguousMemory >').
      _(NumberOfBytes, 'NumberOfBytes').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := xboxkrnl_MmAllocateContiguousMemoryEx(NumberOfBytes, 0, $7FFFFFFF, 0, PAGE_READWRITE);
end;

var
  TotalRequested: DWORD = 0;
  TotalAllocated: DWORD = 0;

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
  PageNr: uint;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmAllocateContiguousMemoryEx').
      _(NumberOfBytes, 'NumberOfBytes').
      _(LowestAcceptableAddress, 'LowestAcceptableAddress').
      _(HighestAcceptableAddress, 'HighestAcceptableAddress').
      _(Alignment, 'Alignment').
      _(ProtectionType, 'ProtectionType', AllocationTypeToString(ProtectionType)).
    LogEnd();

  pRet := NULL;
  if NumberOfBytes > 0 then
  begin
    Inc(TotalRequested, NumberOfBytes);
    pRet := VirtualAlloc(NULL, NumberOfBytes, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    Inc(TotalAllocated, RoundUp(NumberOfBytes, PAGE_SIZE));

    Assert((UIntPtr(pRet) and (PAGE_SIZE-1)) = 0); // Returned address should be page-aligned

    PageNr := DxbxGetPageNr(UIntPtr(pRet));

    Assert(MmAllocatedSize[PageNr] = 0, 'Allocation size conflict!');
    MmAllocatedSize[PageNr] := NumberOfBytes;

    while Integer(NumberOfBytes) > 0 do
    begin
      Assert(MmAllocatedBase[PageNr] = NULL, 'Allocation range conflict!');

      MmAllocatedBase[PageNr] := pRet;
      PageNr := (PageNr + 1) and X_PAGES_MASK;
      Dec(NumberOfBytes, PAGE_SIZE);
    end;

    if IsRunning(TITLEID_Halo) then
    begin
      if Count < 4 then
        g_HaloHack[Count] := uint32(pRet);
      Inc(Count);
    end;
  end;

  if MayLog(lfUnit) then
  begin
    DbgPrintf('EmuKrnl : MmAllocateContiguousMemoryEx returned 0x%.08X', [pRet]);
    DbgPrintf('EmuKrnl : Total requested memory increased to : 0x%.08X', [TotalRequested]);
    DbgPrintf('EmuKrnl : Total allocated memory increased to : 0x%.08X', [TotalAllocated]);
  end;

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
  Unimplemented('MmClaimGpuInstanceMemory');
  Result := PVOID(S_OK);
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
  Info: RDxbxAllocationInfo;
  PageNr: uint;
  NumberOfBytes: ULONG;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : MmFreeContiguousMemory').
      _(BaseAddress, 'BaseAddress').
    LogEnd();

  if (BaseAddress <> @xLaunchDataPage) then
    VirtualFree(BaseAddress, 0, MEM_RELEASE)
  else
  begin
    if MayLog(lfUnit) then
      DbgPrintf('Ignored MmFreeContiguousMemory(&xLaunchDataPage)');
  end;

  Info := DxbxGetAllocationInfo(UIntPtr(BaseAddress));
  PageNr := Info.PageNr;
  NumberOfBytes := Info.AllocatedSize;
  Dec(TotalRequested, NumberOfBytes);
  Dec(TotalAllocated, RoundUp(NumberOfBytes, PAGE_SIZE));

  // Check and reset :
  Assert(NumberOfBytes > 0);
  MmAllocatedSize[PageNr] := 0;

  while Integer(NumberOfBytes) > 0 do
  begin
    Assert(MmAllocatedBase[PageNr] = Info.BaseAddress, 'Allocation range conflict!');

    MmAllocatedBase[PageNr] := nil;
    PageNr := (PageNr + 1) and X_PAGES_MASK;
    Dec(NumberOfBytes, PAGE_SIZE);
  end;

  // Sokoban crashes after this, at reset time (press Black + White to hit this).
  // Tracing in assembly shows the crash takes place quite a while further, so it's probably
  // not related to this call per-se. The strangest thing is, that if we let the debugger step
  // all the way through, the crash doesn't occur. Adding a Sleep(100) here doesn't help though.
  // Note 2011-01-26: The new emulation seems to fix this!

  if MayLog(lfUnit) then
  begin
    DbgPrintf('EmuKrnl : Total requested memory reduced to : 0x%.08X', [TotalRequested]);
    DbgPrintf('EmuKrnl : Total allocated memory reduced to : 0x%.08X', [TotalAllocated]);
  end;

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
  Unimplemented('MmIsAddressValid');
  Result := _BOOLEAN(S_OK);
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
  Unimplemented('MmQueryAddressProtect');
  Result := S_OK;
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
  Unimplemented('MmDbgAllocateMemory');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgFreeMemory(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('MmDbgFreeMemory');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgQueryAvailablePages(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('MmDbgQueryAvailablePages');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgReleaseAddress(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('MmDbgReleaseAddress');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgWriteCheck(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('MmDbgWriteCheck');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

initialization

  ZeroMemory(@MmAllocatedSize[0], SizeOf(MmAllocatedSize));
  ZeroMemory(@MmAllocatedBase[0], SizeOf(MmAllocatedBase));

end.
