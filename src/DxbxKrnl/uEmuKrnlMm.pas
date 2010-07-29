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
  uTypes,
  uLog,
  uResourceTracker,
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
function xboxkrnl_MmClaimGpuInstanceMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
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
function xboxkrnl_MmLockUnlockBufferPages(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmLockUnlockPhysicalPage(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
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
function xboxkrnl_MmQueryAddressProtect(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
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

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmAllocateContiguousMemory' +
         #13#10'(' +
         #13#10'   NumberOfBytes            : 0x%.08X' +
         #13#10');',
         [NumberOfBytes]);
{$ENDIF}

  //
  // Cxbx NOTE: Kludgey (but necessary) solution:
  //
  // Since this memory must be aligned on a page boundary, we must allocate an extra page
  // so that we can return a valid page aligned pointer
  //

  pRet := DxbxMalloc(NumberOfBytes + $1000);

  // align to page boundary
  begin
    dwRet := DWORD(pRet);
    Inc(dwRet, $1000 - (dwRet mod $1000));
    g_AlignCache.insert(dwRet, pRet);
    pRet := PVOID(dwRet);
  end;

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmAllocateContiguous returned 0x%.08X', [pRet]);
{$ENDIF}

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

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmAllocateContiguousMemoryEx' +
         #13#10'(' +
         #13#10'   NumberOfBytes            : 0x%.08X' +
         #13#10'   LowestAcceptableAddress  : 0x%.08X' +
         #13#10'   HighestAcceptableAddress : 0x%.08X' +
         #13#10'   Alignment                : 0x%.08X' +
         #13#10'   ProtectionType           : 0x%.08X' +
         #13#10');',
         [NumberOfBytes, LowestAcceptableAddress, HighestAcceptableAddress,
         Alignment, ProtectionType]);
{$ENDIF}

  //
  // NOTE: Kludgey (but necessary) solution:
  //
  // Since this memory must be aligned on a page boundary, we must allocate an extra page
  // so that we can return a valid page aligned pointer
  //

  pRet := DxbxMalloc(NumberOfBytes + $1000);

  // align to page boundary
  begin
    dwRet := DWORD(pRet);
    Inc(dwRet, $1000 - (dwRet mod $1000));
    g_AlignCache.insert(dwRet, pRet);
    pRet := PVOID(dwRet);
  end;

{$IFDEF GAME_HACKS_ENABLED}
  if Count < 4 then
    g_HaloHack[Count] := uint32(pRet);
  Inc(Count);
{$ENDIF}

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmAllocateContiguousEx returned 0x%.08X', [pRet]);
{$ENDIF}

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

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmAllocateSystemMemory' +
         #13#10'(' +
         #13#10'   NumberOfBytes            : 0x%.08X' +
         #13#10'   Protect                  : 0x%.08X' +
         #13#10');',
         [NumberOfBytes, Protect]);
{$ENDIF}

  // TODO -oCXBX: should this be aligned?
  Result := DxbxMalloc(NumberOfBytes);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmClaimGpuInstanceMemory(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmClaimGpuInstanceMemory');
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

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmCreateKernelStack' +
      #13#10'(' +
      #13#10'   NumberOfBytes            : 0x%.08X' +
      #13#10'   DebuggerThread           : 0x%.08X' +
      #13#10');',
      [NumberOfBytes, DebuggerThread]);
{$ENDIF}

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

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmDeleteKernelStack' +
      #13#10'(' +
      #13#10'   EndAddress               : 0x%.08X' +
      #13#10'   BaseAddress              : 0x%.08X' +
      #13#10');',
      [EndAddress, BaseAddress]);
{$ENDIF}

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

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmFreeContiguousMemory' +
      #13#10'(' +
      #13#10'   BaseAddress              : 0x%.08X' +
      #13#10');',
      [BaseAddress]);
{$ENDIF}

  OrigBaseAddress := BaseAddress;

  if(g_AlignCache.exists(BaseAddress)) then
  begin
    OrigBaseAddress := g_AlignCache.get(BaseAddress);
    g_AlignCache.remove(BaseAddress);
  end;

  if(OrigBaseAddress <> @xLaunchDataPage) then
  begin
    DxbxFree(OrigBaseAddress);
  end
  else
  begin
{$IFDEF DEBUG}
    DbgPrintf('Ignored MmFreeContiguousMemory(&xLaunchDataPage)');
{$ENDIF}
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

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmFreeSystemMemory' +
      #13#10'(' +
      #13#10'   BaseAddress              : 0x%.08X' +
      #13#10'   NumberOfBytes            : 0x%.08X' +
      #13#10');',
      [BaseAddress, NumberOfBytes]);
{$ENDIF}

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
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmGetPhysicalAddress');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmIsAddressValid(
  VirtualAddress: PVOID
  ): _BOOLEAN; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := _BOOLEAN(Unimplemented('MmIsAddressValid'));
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmLockUnlockBufferPages(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmLockUnlockBufferPages');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmLockUnlockPhysicalPage(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmLockUnlockPhysicalPage');
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

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmPersistContiguousMemory' +
         #13#10'(' +
         #13#10'   BaseAddress              : 0x%.08X' +
         #13#10'   NumberOfBytes            : 0x%.08X' +
         #13#10'   Persist                  : 0x%.08X' +
         #13#10');',
         [BaseAddress, NumberOfBytes, Persist]);
{$ENDIF}

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

function xboxkrnl_MmQueryAddressProtect(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmQueryAddressProtect');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmQueryAllocationSize(
  BaseAddress: PVOID
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmQueryAllocationSize' +
      #13#10'(' +
      #13#10'   BaseAddress              : 0x%.08X' +
      #13#10');',
      [BaseAddress]);
{$ENDIF}

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

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmQueryStatistics' +
      #13#10'(' +
      #13#10'   MemoryStatistics         : 0x%.08X' +
      #13#10');',
      [MemoryStatistics]);
{$ENDIF}

  GlobalMemoryStatus({var}MemoryStatus);

  ZeroMemory(MemoryStatistics, sizeof(MM_STATISTICS));

  MemoryStatistics.Length := sizeof(MM_STATISTICS);
  MemoryStatistics.TotalPhysicalPages := MemoryStatus.dwTotalVirtual div 4096;
  MemoryStatistics.AvailablePages := MemoryStatus.dwAvailVirtual div 4096;

  // HACK (does this matter?)
  MemoryStatistics.VirtualMemoryBytesReserved := MemoryStatus.dwTotalPhys - MemoryStatus.dwAvailPhys;

  // the rest arent really used from what i've seen

  EmuSwapFS(fsXbox);
  Result := STATUS_SUCCESS;
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

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmSetAddressProtect' +
      #13#10'(' +
      #13#10'   BaseAddress              : 0x%.08X' +
      #13#10'   NumberOfBytes            : 0x%.08X' +
      #13#10'   NewProtect               : 0x%.08X' +
      #13#10');',
      [BaseAddress, NumberOfBytes, NewProtect]);
{$ENDIF}

{$IFDEF GAME_HACKS_ENABLED}
  // Halo Hack
  if(BaseAddress = PVOID($80366000)) then
  begin
    BaseAddress := PVOID((g_HaloHack[0] + ($80366000 - $80061000)));

    DbgPrintf('EmuKrnl : Halo Access Adjust 3 was applied! (0x%.08X)', [BaseAddress]);
  end;
{$ENDIF}

  if(not VirtualProtect(BaseAddress, NumberOfBytes, NewProtect and (not PAGE_WRITECOMBINE), @dwOldProtect)) then
    EmuWarning('VirtualProtect Failed!');

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : VirtualProtect was 0x%.08X -> 0x%.08X', [dwOldProtect, NewProtect and (not PAGE_WRITECOMBINE)]);
{$ENDIF}

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
