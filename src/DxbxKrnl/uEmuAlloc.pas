(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2007 Shadow_tj and other members of the development team.

    This program is Free software: you can redistribute it and/or modify
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
unit uEmuAlloc;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
//  Messages,
  SysUtils,
  Classes,
  // 3rd party
  JwaWinType,
  JwaNative,
  // Dxbx
  uTypes,
  uDxbxUtils,
  uMutex,
  uLog;


function CxbxMalloc(x: Integer): Pointer;
function CxbxCalloc(x, y: Integer): Pointer;
procedure CxbxFree(x: Pointer);
//function CxbxCallocDebug(NbrElements: size_t; ElementSize: size_t; pFile: PChar; Line: Int): PVoid;
function CxbxRtlAlloc(Heap: HANDLE; Flags: ULONG; Bytes: SIZE_T): PVOID;
function CxbxRtlFree(Heap: Handle; Flags: DWORD; pMem: PVOID): BOOL;
function CxbxRtlRealloc(Heap: HANDLE; Flags: ULONG; pMem: PVOID; Bytes: SIZE_T): PVOID;
function CxbxRtlSizeHeap(Heap: HANDLE; Flags: ULONG; pMem: PVOID): SIZE_T;

implementation

{$IFDEF _DEBUG_ALLOC}

(*
const CxbxMalloc(x)                            CxbxMallocDebug(x, __FILE__, __LINE__);
const CxbxCalloc(x, y)                         CxbxCallocDebug(x, y, __FILE__, __LINE__);
const CxbxFree(x)                              CxbxFreeDebug(x, __FILE__, __LINE__);
const CxbxRtlAlloc(Heap, Flags, Bytes)         CxbxRtlAllocDebug(Heap, Flags, Bytes, __FILE__, __LINE__);
const CxbxRtlFree(Heap, Flags, pMem)           CxbxRtlFreeDebug(Heap, Flags, pMem, __FILE__, __LINE__);
const CxbxRtlRealloc(Heap, Flags, pMem, Bytes) CxbxRtlReallocDebug(Heap, Flags, pMem, Bytes, __FILE__, __LINE__);
const CxbxRtlSizeHeap(Heap, Flags, pMem)       CxbxRtlSizeHeapDebug(Heap, Flags, pMem, __FILE__, __LINE__);
*)

{$ELSE !_DEBUG_ALLOC}

function CxbxMalloc(x: Integer): Pointer;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := malloc(x);
end;

function CxbxCalloc(x, y: Integer): Pointer;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := calloc(x, y);
end;

procedure CxbxFree(x: Pointer);
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  free(x);
end;

function CxbxRtlAlloc(Heap: HANDLE; Flags: ULONG; Bytes: SIZE_T): PVOID;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := JwaNative.RtlAllocateHeap(Heap, Flags, Bytes);
end;

function CxbxRtlFree(Heap: Handle; Flags: DWORD; pMem: PVOID): BOOL;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := JwaNative.RtlFreeHeap(Heap, Flags, pMem);
end;

function CxbxRtlRealloc(Heap: HANDLE; Flags: ULONG; pMem: PVOID; Bytes: SIZE_T): PVOID;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := JwaNative.RtlReAllocateHeap(Heap, Flags, pMem, Bytes);
end;

function CxbxRtlSizeHeap(Heap: HANDLE; Flags: ULONG; pMem: PVOID): SIZE_T;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := JwaNative.RtlSizeHeap(Heap, Flags, pMem);
end;

{$ENDIF !_DEBUG_ALLOC}
//end EMUALLOC_H


{$IFDEF _DEBUG_ALLOC}

//include 'mutex.h'

// ******************************************************************
// * prevent name collisions
// ******************************************************************
(*
namespace NtDll
begin
    //include 'EmuNtDll.h'
);
*)

// ******************************************************************
// * Memory tracking stuff
// ******************************************************************

const
  MEMORY_GUARD: UInt32 = $DEADFADE;

type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value ****)

  CXBX_ALLOC_TYPE = (
    CXBX_ALLOC_NORMAL,
    CXBX_ALLOC_RTL
  );

type
  PCXBX_MEMORY_BLOCK = ^CXBX_MEMORY_BLOCK;
  CXBX_MEMORY_BLOCK = packed record
    pMem: PVOID;
    Size: size_t;
    pFile: PChar;
    Line: uint32;
    cType: CXBX_ALLOC_TYPE;
    pNext: PCXBX_MEMORY_BLOCK;
 end;
 
var
 g_pFirstBlock: PCXBX_MEMORY_BLOCK = NULL;
 g_pLastBlock: PCXBX_MEMORY_BLOCK = NULL;
 g_MemoryMutex: Mutex;

// ******************************************************************
// * GetMemStart - Retrieves the actual start of the allocated memory
// *               block (first guard)
// ******************************************************************
function GetMemStart(pBlock: PCXBX_MEMORY_BLOCK): PVOID; inline;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := PVOID(PAnsiChar(pBlock.pMem) - SizeOf(MEMORY_GUARD));
end;

// ******************************************************************
// * GetMemEnd - Retrieves the end of the allocated memory block
// *             (second guard)
// ******************************************************************
function GetMemEnd(pBlock: PCXBX_MEMORY_BLOCK): PVOID; inline;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := PVOID(PAnsiChar(pBlock.pMem) + pBlock.Size);
end;

// ******************************************************************
// * CheckIntegrity - Prints if the memory block is overwritten
// ******************************************************************
function CheckIntegrity(pBlock: PCXBX_MEMORY_BLOCK): bool;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  Integrity: bool;
begin
  Integrity := True;

  if PUInt32(GetMemStart(pBlock))^ <> MEMORY_GUARD then
  begin
    DbgPrintf('    Memory block corrupted at start, overwrite: $%.04X',
              [PUInt32(GetMemStart(pBlock))^]);
    Integrity := False;
  end;

  if PUInt32(GetMemEnd(pBlock))^ <> MEMORY_GUARD then
  begin
    DbgPrintf('    Memory block corrupted at end, overwrite: $.04X',
              [PUInt32(GetMemEnd(pBlock))^]);
    Integrity := False;
  end;

  Result := Integrity;
end;


// ******************************************************************
// * IsThisMemoryBlock - Simple block matching function
// ******************************************************************
function IsThisMemoryBlock(pMem: PVoid; pBlock: PCXBX_MEMORY_BLOCK): bool; inline;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := Assigned(pBlock) and (pMem = pBlock.pMem);
end;

// ******************************************************************
// * InThisMemoryBlock - Simple block matching function
// ******************************************************************
function InThisMemoryBlock(pMem: PVoid; pBlock: PCXBX_MEMORY_BLOCK): bool; inline;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := Assigned(pBlock)
        and (IntPtr(pMem) >= IntPtr(pBlock.pMem))
        and (IntPtr(pMem) <= IntPtr(GetMemEnd(pBlock)));
end;


// ******************************************************************
// * InsertMemoryBlock - Inserts a new memory block in the tracker
// ******************************************************************
function InsertMemoryBlock(
  pMem: PVoid;
  Size: size_t;
  pFile: PChar;
  Line: Int;
  cType: CXBX_ALLOC_TYPE): PCXBX_MEMORY_BLOCK;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pBlock: PCXBX_MEMORY_BLOCK;
  Length: size_t;
begin
  pBlock := PCXBX_MEMORY_BLOCK(malloc(SizeOf(CXBX_MEMORY_BLOCK)));
  pBlock.pMem := PUInt8(IntPtr(pMem) + SizeOf(MEMORY_GUARD));
  pBlock.Size := Size;
  Length := strlen(pFile) + 1;
  pBlock.pFile := PChar(malloc(Length));
  memcpy(pBlock.pFile, pFile, Length);
  pBlock.pNext := 0;
  pBlock.Line := Line;
  pBlock.cType := cType;
  PUInt32(GetMemStart(pBlock))^ := MEMORY_GUARD;
  PUInt32(GetMemEnd(pBlock))^ := MEMORY_GUARD;

  if (g_pFirstBlock = NULL) then
  begin
    g_pFirstBlock := pBlock;
    g_pLastBlock := pBlock;
  end
  else
  begin
    g_pLastBlock.pNext := pBlock;
    g_pLastBlock := pBlock;
  end;

  Result := pBlock;
end;

// ******************************************************************
// * RemoveMemoryBlock - Removes a memory block from the tracker
// ******************************************************************
function RemoveMemoryBlock(pMem: PVoid): PCXBX_MEMORY_BLOCK;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pFree: PCXBX_MEMORY_BLOCK;
  pCur: PCXBX_MEMORY_BLOCK;
  pPrev: PCXBX_MEMORY_BLOCK;
begin
  pFree := NULL;
  if IsThisMemoryBlock(pMem, g_pFirstBlock) then
  begin
    pFree := g_pFirstBlock;
    g_pFirstBlock := g_pFirstBlock.pNext;
    if pFree = g_pLastBlock then
      g_pLastBlock := NULL;
  end
  else
  begin
    pPrev := NULL;
    pCur := g_pFirstBlock;
    while Assigned(pCur) do
    begin
      if IsThisMemoryBlock(pMem, pCur) then
      begin
        if pCur = g_pLastBlock then
          g_pLastBlock := pPrev;

        pFree := pCur;
        pPrev.pNext := pCur.pNext;
        Break;
      end;

      pPrev := pCur;
      pCur := pCur.pNext;
    end;
  end;

  Result := pFree;
end;


// ******************************************************************
// * FindMemoryBlock - Finds a memory block in the tracker
// ******************************************************************
function FindMemoryBlock(pMem: PVoid): PCXBX_MEMORY_BLOCK;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pCur: PCXBX_MEMORY_BLOCK;
begin
  pCur := g_pFirstBlock;
  while Assigned(pCur) do
  begin
    if IsThisMemoryBlock(pMem, pCur) then
    begin
      Result := pCur;
      Exit;
    end;

    pCur := pCur.pNext;
  end;

  Result := nil;
end;

// ******************************************************************
// * FindMemoryBlockIn - Finds a memory block in the tracker
// ******************************************************************
function FindMemoryBlockIn(const pMem: PVoid): PCXBX_MEMORY_BLOCK;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pCur: PCXBX_MEMORY_BLOCK;
begin
  pCur := g_pFirstBlock;
  while Assigned(pCur) do
  begin
    if InThisMemoryBlock(pMem, pCur) then
    begin
      Result := pCur;
      Exit;
    end;

    pCur := pCur.pNext;
  end;
  
  Result := NULL;
end;


// ******************************************************************
// * CxbxAllocDump - Dump the memory allocations
// ******************************************************************
procedure CxbxAllocDump(DumpData: bool);
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pCur: PCXBX_MEMORY_BLOCK;
begin
  g_MemoryMutex.Lock();

  DbgPrintf('******************************************************'#13#10 +
            '* Dumping memory allocations                         *'#13#10 +
            '******************************************************');
  pCur := g_pFirstBlock;
  while Assigned(pCur) do
  begin
    DbgPrintf(#13#10 +
           #13#10'    Block: $%.08X' +
           #13#10'    Size : %d' +
           #13#10'    File : %s' +
           #13#10'    Line : %d' +
           #13#10'    Type : %s',
           [pCur.pMem, pCur.Size, pCur.pFile, pCur.Line,
           iif(pCur.cType = CXBX_ALLOC_NORMAL, 'NORMAL', 'RTL')]);
    CheckIntegrity(pCur);
    pCur := pCur.pNext;
  end;

  g_MemoryMutex.Unlock();
end;


// ******************************************************************
// * CxbxMallocDebug - Debug track malloc
// ******************************************************************
function CxbxMallocDebug(
  Size: size_t;
  pFile: PChar;
  Line: Integer): PVoid;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pRetMem: Pointer;
  pMem: Pointer;
  pBlock: PCXBX_MEMORY_BLOCK;
begin
  pRetMem := NULL;
  g_MemoryMutex.Lock();

  pMem := malloc(Size + 2 * SizeOf(MEMORY_GUARD));
  if not Assigned(pMem) then
  begin
    DbgPrintf('CxbxMallocDebug: Allocation failed' +
           #13#10'    Size: %d' +
           #13#10'    File: %s' +
           #13#10'    Line: %d',
           [Size, pFile, Line]);
  end
  else
  begin
    pBlock := InsertMemoryBlock(pMem, Size, pFile, Line, CXBX_ALLOC_NORMAL);
    pRetMem := pBlock.pMem;
  end;

  g_MemoryMutex.Unlock();

  Result := pRetMem;
end;

// ******************************************************************
// * CxbxCallocDebug - Debug track calloc
// ******************************************************************
function CxbxCallocDebug(
  NbrElements: size_t;
  ElementSize: size_t;
  pFile: PChar;
  Line: Int): PVoid;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pRetMem: Pointer;
  pMem: Pointer;
  pBlock: PCXBX_MEMORY_BLOCK;
begin
  pRetMem := NULL;
  g_MemoryMutex.Lock();

  pMem := calloc(NbrElements * ElementSize + 2 * SizeOf(MEMORY_GUARD), 1);
  if not Assigned(pMem) then
  begin
    DbgPrintf('CxbxCallocDebug: Allocation failed' +
           #13#10'    NbrElements: %d' +
           #13#10'    ElementSize: %d' +
           #13#10'    File       : %s' +
           #13#10'    Line       : %d',
           [NbrElements, ElementSize, pFile, Line]);
  end
  else
  begin
    pBlock := InsertMemoryBlock(pMem, NbrElements * ElementSize, pFile, Line, CXBX_ALLOC_NORMAL);
    pRetMem := pBlock.pMem;
  end;

  g_MemoryMutex.Unlock();

  Result := pRetMem;
end;

// ******************************************************************
// * CxbxFreeDebug - Debug track Free
// ******************************************************************
procedure CxbxFreeDebug(
  pMem: PVoid;
  pFile: PChar;
  Line: Int);
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pFree: PCXBX_MEMORY_BLOCK;
begin
  if pMem = nil then
    Exit;

  g_MemoryMutex.Lock();

  pFree := RemoveMemoryBlock(pMem);
  if not Assigned(pFree) then
  begin
    DbgPrintf('CxbxFreeDebug: Free on non-existent block: $%.08X!' +
             #13#10'Possibly a multiple Free.' +
             #13#10'    File: %s' +
             #13#10'    Line: %d',
             [pMem, pFile, Line]);
  end
  else
  begin
    if not CheckIntegrity(pFree) then
    begin
      DbgPrintf('CxbxFreeDebug: Free on damaged block' +
               #13#10'    Block   : $%.08X' +
               #13#10'    Allocation' +
               #13#10'        File: %s' +
               #13#10'        Line: %d' +
               #13#10'    Free' +
               #13#10'        File: %s' +
               #13#10'        Line: %d',
               [pFree.pMem, pFree.pFile, pFree.Line, pFile, Line]);
    end;

    free(GetMemStart(pFree));
    free(pFree.pFile);
    free(pFree);
  end;

  g_MemoryMutex.Unlock();
end;

// ******************************************************************
// * CxbxRtlAllocDebug - Debug track RTL alloc
// ******************************************************************
function CxbxRtlAllocDebug(
  Heap: Handle;
  Flags: DWORD;
  Bytes: SIZE_T;
  pFile: PChar;
  Line: Int): PVoid;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pRetMem: PVoid;
  pMem: PVoid;
  pBlock: PCXBX_MEMORY_BLOCK;
begin
  pRetMem := NULL;
  g_MemoryMutex.Lock();

  //pMem := NtDll.RtlAllocateHeap(Heap, Flags, Bytes + 2 * SizeOf(MEMORY_GUARD));
  pMem := CxbxRtlAlloc(Heap, Flags, Bytes + 2 * SizeOf(MEMORY_GUARD));
  if not Assigned(pMem) then
  begin
    DbgPrintf('CxbxRtlAllocDebug: Allocation failed' +
             #13#10'    Heap  : $%.08X' +
             #13#10'    Flags : $%.08X' +
             #13#10'    Bytes : %d' +
             #13#10'    File  : %s' +
             #13#10'    Line  : %d',
             [Heap, Flags, Bytes, pFile, Line]);
  end
  else
  begin
    pBlock := InsertMemoryBlock(pMem, Bytes, pFile, Line, CXBX_ALLOC_RTL);
    pRetMem := pBlock.pMem;
  end;

  g_MemoryMutex.Unlock();

  Result := pRetMem;
end;

// ******************************************************************
// * CxbxRtlFreeDebug - Debug track RTL Free
// ******************************************************************
function CxbxRtlFreeDebug(
  Heap: Handle;
  Flags: DWORD;
  pMem: PVOID;
  pFile: PChar;
  Line: Int): BOOL;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pFree: PCXBX_MEMORY_BLOCK;
begin
  Result := False;
  if not Assigned(pMem) then
  begin
    Result := True;
    Exit;
  end;

  g_MemoryMutex.Lock();

  pFree := RemoveMemoryBlock(pMem);
  if not Assigned(pFree) then
  begin
    DbgPrintf('CxbxRtlFreeDebug: Free on non-existent block: $%.08X!' +
           #13#10'Possibly a multiple Free.' +
           #13#10'    File: %s' +
           #13#10'    Line: %d',
           [pMem, pFile, Line]);
  end
  else
  begin
    if not CheckIntegrity(pFree) then
    begin
      DbgPrintf('CxbxRtlFreeDebug: Free on damaged block' +
             #13#10'    Block   : $.%08X' +
             #13#10'    Allocation' +
             #13#10'        File: %s' +
             #13#10'        Line: %d' +
             #13#10'    Free' +
             #13#10'        File: %s' +
             #13#10'        Line: %d',
             [pFree.pMem, pFree.pFile, pFree.Line, pFile, Line]);
    end;

    Result := CxbxRtlFree(Heap, Flags, GetMemStart(pFree));
    free(pFree.pFile);
    free(pFree);
  end;

  g_MemoryMutex.Unlock();
end;

// ******************************************************************
// * CxbxRtlReallocDebug - Debug track RTL realloc
// ******************************************************************
function CxbxRtlReallocDebug(
  Heap: Handle;
  Flags: DWORD;
  pMem: PVOID;
  Bytes: SIZE_T;
  pFile: PChar;
  Line: Int): PVoid;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pRetMem: PVoid;
  pRealloc: PCXBX_MEMORY_BLOCK;
  pNewMem: Pointer;
  pBlock: PCXBX_MEMORY_BLOCK;
begin
  pRetMem := nil;
  g_MemoryMutex.Lock();

  pRealloc := FindMemoryBlock(pMem);
  if not Assigned(pRealloc) then
  begin
    DbgPrintf('CxbxRtlRealloc: realloc on non-existent block: $%.08X!' +
               #13#10'    File: %s' +
               #13#10'    Line: %d',
               [pMem, pFile, Line]);
  end
  else
  begin
    if not CheckIntegrity(pRealloc) then
    begin
      DbgPrintf('CxbxRtlReallocDebug: Realloc on damaged block' +
                   #13#10'    Block   : $.%08X' +
                   #13#10'    Allocation' +
                   #13#10'        Size: %d' +
                   #13#10'        File: %s' +
                   #13#10'        Line: %d' +
                   #13#10'    Reallocation' +
                   #13#10'        Size: %d' +
                   #13#10'        File: %s' +
                   #13#10'        Line: %d',
                   [pRealloc.pMem,
                   pRealloc.pFile, pRealloc.Size, pRealloc.Line,
                   Bytes, pFile, Line]);
    end;

    pNewMem := CxbxRtlReAlloc(Heap, Flags, GetMemStart(pRealloc), Bytes + 2 * SizeOf(MEMORY_GUARD));
    free(pRealloc.pFile);
    free(pRealloc);
    if not Assigned(pNewMem) then
    begin
      DbgPrintf('CxbxRtlReallocDebug: Reallocation failed' +
               #13#10'    Heap  : $%.08X' +
               #13#10'    Flags : $%.08X' +
               #13#10'    pMem  : $%.08X' +
               #13#10'    Bytes : %d' +
               #13#10'    File  : %s' +
               #13#10'    Line  : %d',
               [Heap, Flags, pMem, Bytes, pFile, Line]);
    end
    else
    begin
      pBlock := InsertMemoryBlock(pNewMem, Bytes, pFile, Line, CXBX_ALLOC_RTL);
      pRetMem := pBlock.pMem;
    end;
  end;

  g_MemoryMutex.Unlock();
  Result := pRetMem;
end;

// ******************************************************************
// * CxbxRtlSizeHeapDebug - Debug track RTL heap size
// ******************************************************************
function CxbxRtlSizeHeapDebug(
  Heap: Handle;
  Flags: DWORD;
  pMem: PVOID;
  pFile: PChar;
  Line: Int): SIZE_T;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  Size: SIZE_T;
  pBlock: PCXBX_MEMORY_BLOCK;
  ActualSize: SIZE_T;
begin
  Size := 0;
  g_MemoryMutex.Lock();

  pBlock := FindMemoryBlock(pMem);
  if not Assigned(pBlock) then
  begin
    DbgPrintf('CxbxRtlSizeHeap: size heap on non-existent block: $%.08X!' +
           #13#10'File: %s' +
           #13#10'    Line: %d',
           [pMem, pFile, Line]);
  end
  else
  begin
    ActualSize := CxbxRtlSizeHeap(Heap, Flags, GetMemStart(pBlock))
                        - 2 * SizeOf(MEMORY_GUARD);
    if ActualSize <> pBlock.Size then
    begin
      DbgPrintf('CxbxRtlSizeHeap: heap size mismatch, RtlSizeHeap: %d Tracker: %d' +
               #13#10'    File  : %s' +
               #13#10'    Line  : %d',
               [ActualSize,
               pBlock.Size,
               pFile,
               Line]);
    end;

    Size := ActualSize;
  end;

  g_MemoryMutex.Unlock();
  Result := Size;
end;

// ******************************************************************
// * CxbxVirtualQueryDebug - Debug virtual query
// ******************************************************************
function CxbxVirtualQueryDebug(
  lpAddress: LPCVOID;
  lpBuffer: PMEMORY_BASIC_INFORMATION;
  dwLength: DWORD): DWORD;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  Size: DWORD;
  pBlock: PCXBX_MEMORY_BLOCK;
begin
  Size := 0;
  g_MemoryMutex.Lock();

  lpBuffer.State := MEM_COMMIT;

  pBlock := FindMemoryBlockIn(lpAddress);

  if Assigned(pBlock) then
  begin
    Size := dwLength;
    lpBuffer.RegionSize := pBlock.Size;
    lpBuffer.BaseAddress := pBlock.pMem;
  end;


  g_MemoryMutex.Unlock();
  Result := Size;
end;

{$ENDIF !_DEBUG_ALLOC}

end.
