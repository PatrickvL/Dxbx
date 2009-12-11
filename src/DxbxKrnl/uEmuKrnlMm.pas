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
  ): PVOID; stdcall;
function xboxkrnl_MmAllocateSystemMemory(
  NumberOfBytes: ULONG;
  Protect: ULONG
  ): PVOID; stdcall;
function xboxkrnl_MmClaimGpuInstanceMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_MmCreateKernelStack(
  NumberOfBytes: ULONG;
  Unknown: ULONG
  ): PVOID; stdcall;
procedure xboxkrnl_MmDeleteKernelStack(
  EndAddress: PVOID;
  BaseAddress: PVOID
  ); stdcall;
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
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmGlobalData');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmAllocateContiguousMemory(
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:90
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

  pRet := CxbxMalloc(NumberOfBytes + $1000);

  // align to page boundary
  begin
    dwRet := DWORD(pRet);

    Inc(dwRet, $1000 - (dwRet mod $1000));

// Dxbx TODO :    g_AlignCache.insert(dwRet, pRet);

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
// Branch:martin  Revision:39  Translator:PatrickvL  Done:90
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

  pRet := CxbxMalloc(NumberOfBytes + $1000);

  // align to page boundary
  begin
    dwRet := DWORD(pRet);

    Inc(dwRet, $1000 - (dwRet mod $1000));

// Dxbx TODO :    g_AlignCache.insert(dwRet, pRet);

    pRet := PVOID(dwRet);
  end;

  if Count < 4 then
    g_HaloHack[Count] := uint32(pRet);
  Inc(Count);

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
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
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

  // TODO: should this be aligned?
  Result := CxbxMalloc(NumberOfBytes);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmClaimGpuInstanceMemory(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmClaimGpuInstanceMemory');
  EmuSwapFS(fsXbox);
end;

// Differences from NT: Custom stack size.
function xboxkrnl_MmCreateKernelStack(
  NumberOfBytes: ULONG;
  Unknown: ULONG
  ): PVOID; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmCreateKernelStack' +
      #13#10'(' +
      #13#10'   NumberOfBytes            : 0x%.08X' +
      #13#10'   Unknown                  : 0x%.08X' +
      #13#10');',
      [NumberOfBytes, Unknown]);
{$ENDIF}

  if (Unknown <> 0) then
    EmuWarning('MmCreateKernelStack unknown parameter ignored');

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
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
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

function xboxkrnl_MmFreeContiguousMemory(
  BaseAddress: PVOID
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:5
(*var
  OrigBaseAddress: PVoid; *)
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmFreeContiguousMemory' +
      #13#10'(' +
      #13#10'   BaseAddress              : 0x%.08X' +
      #13#10');',
      [BaseAddress]);
{$ENDIF}

  (*OrigBaseAddress := BaseAddress;

  if(g_AlignCache.exists(BaseAddress)) then
  begin
    OrigBaseAddress = g_AlignCache.get(BaseAddress);
    g_AlignCache.remove(BaseAddress);
  end;

  if(OrigBaseAddress <> &xLaunchDataPage)
  begin
    CxbxFree(OrigBaseAddress);
  end
  else
  begin
{$IFDEF DEBUG}
    DbgPrintf('Ignored MmFreeContiguousMemory(&xLaunchDataPage)');
{$ENDIF}
  end; *)

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmFreeSystemMemory(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmFreeSystemMemory'+
      #13#10'('+
      #13#10'   BaseAddress              : 0x%.08X' +
      #13#10'   NumberOfBytes            : 0x%.08X' +
      #13#10');',
      [BaseAddress, NumberOfBytes]);
{$ENDIF}

  CxbxFree(BaseAddress);

  EmuSwapFS(fsXbox);
  Result := STATUS_SUCCESS;
end;

function xboxkrnl_MmGetPhysicalAddress(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmGetPhysicalAddress');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmIsAddressValid(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmIsAddressValid');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmLockUnlockBufferPages(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmLockUnlockBufferPages');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmLockUnlockPhysicalPage(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmLockUnlockPhysicalPage');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmMapIoSpace(): NTSTATUS; stdcall;
// Branch:Dxbx
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
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
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

  // Cxbx TODO: Actually set this up to be remember across a 'reboot'
  EmuWarning('MmPersistContiguousMemory is being ignored');

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmQueryAddressProtect(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmQueryAddressProtect');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmQueryAllocationSize(
  BaseAddress: PVOID
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmQueryAllocationSize'+
      #13#10'('+
      #13#10'   BaseAddress              : 0x%.08X' +
      #13#10');',
      [BaseAddress]);
{$ENDIF}

  Result := EmuCheckAllocationSize(BaseAddress, false);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmQueryStatistics(
  MemoryStatistics: PMM_STATISTICS // out
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:5
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmQueryStatistics'+
      #13#10'(\'+
      #13#10'   MemoryStatistics         : 0x%.08X' +
      #13#10');',
      [MemoryStatistics]);
{$ENDIF}

  (*MEMORYSTATUS MemoryStatus;

  GlobalMemoryStatus(&MemoryStatus);

  ZeroMemory(MemoryStatistics, sizeof(MM_STATISTICS));

  MemoryStatistics->Length = sizeof(MM_STATISTICS);
  MemoryStatistics->TotalPhysicalPages = MemoryStatus.dwTotalVirtual / 4096;
  MemoryStatistics->AvailablePages = MemoryStatus.dwAvailVirtual / 4096;

  // HACK (does this matter?)
  MemoryStatistics.VirtualMemoryBytesReserved := MemoryStatus.dwTotalPhys - MemoryStatus.dwAvailPhys; *)

  // the rest arent really used from what i've seen

  EmuSwapFS(fsXbox);
  Result := STATUS_SUCCESS;
end;

function xboxkrnl_MmSetAddressProtect(
  BaseAddress: PVOID;
  NumberOfBytes: ULONG;
  NewProtect: ULONG
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:5
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : MmSetAddressProtect'+
      #13#10'('+
      #13#10'   BaseAddress              : 0x%.08X' +
      #13#10'   NumberOfBytes            : 0x%.08X' +
      #13#10'   NewProtect               : 0x%.08X' +
      #13#10');',
      [BaseAddress, NumberOfBytes, NewProtect]);
{$ENDIF}

  // Halo Hack
  (*if(BaseAddress == (PVOID)0x80366000)
  {
      BaseAddress = (PVOID)(g_HaloHack[0] + (0x80366000 - 0x80061000));

{$IFDEF DEBUG}
      DbgPrintf("EmuKrnl (0x%X): Halo Access Adjust 3 was applied! (0x%.08X)\n", GetCurrentThreadId(), BaseAddress);
{$ENDIF}
  }

  DWORD dwOldProtect;

  if(!VirtualProtect(BaseAddress, NumberOfBytes, NewProtect & (~PAGE_WRITECOMBINE), &dwOldProtect))
      EmuWarning("VirtualProtect Failed!"); *)

  (*
{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : VirtualProtect was 0x%.08X -> 0x%.08X\n", GetCurrentThreadId(), dwOldProtect, NewProtect & (~PAGE_WRITECOMBINE));
{$ENDIF}
  *)

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmUnmapIoSpace(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmUnmapIoSpace');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgAllocateMemory(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgAllocateMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgFreeMemory(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgFreeMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgQueryAvailablePages(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgQueryAvailablePages');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgReleaseAddress(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgReleaseAddress');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_MmDbgWriteCheck(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('MmDbgWriteCheck');
  EmuSwapFS(fsXbox);
end;

end.
