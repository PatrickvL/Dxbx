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

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  Windows,
//  Messages,
  SysUtils,
  Classes;


function CxbxMalloc(x: Integer): Pointer;
procedure CxbxFree(x: Pointer);

implementation

(*
#ifdef _DEBUG_ALLOC
const CxbxMalloc(x) =                            CxbxMallocDebug(x, __FILE__, __LINE__);
const CxbxCalloc(x, = y)                         CxbxCallocDebug(x, y, __FILE__, __LINE__);
const CxbxFree(x) =                              CxbxFreeDebug(x, __FILE__, __LINE__);
const CxbxRtlAlloc(Heap, = Flags, Bytes)         CxbxRtlAllocDebug(Heap, Flags, Bytes, __FILE__, __LINE__);
const CxbxRtlFree(Heap, = Flags, pMem)           CxbxRtlFreeDebug(Heap, Flags, pMem, __FILE__, __LINE__);
const CxbxRtlRealloc(Heap, = Flags, pMem, Bytes) CxbxRtlReallocDebug(Heap, Flags, pMem, Bytes, __FILE__, __LINE__);
const CxbxRtlSizeHeap(Heap, = Flags, pMem)       CxbxRtlSizeHeapDebug(Heap, Flags, pMem, __FILE__, __LINE__);

// ******************************************************************
// * CxbxMallocDebug - Debug track malloc
// ******************************************************************
function CxbxMallocDebug(Size: Integer; pFile: PChar; Line: Integer): Pointer;

// ******************************************************************
// * CxbxCallocDebug - Debug track calloc
// ******************************************************************
function CxbxCallocDebug(NbrElements: Integer; ElementSize: Integer; pFile: PChar; Line: Integer): Pointer;

// ******************************************************************
// * CxbxFreeDebug - Debug track Free
// ******************************************************************
procedure  CxbxFreeDebug(pMem: Pointer;
                    Char *pFile,
                    Integer   Line);

// ******************************************************************
// * CxbxRtlAllocDebug - Debug track RTL alloc
// ******************************************************************
function CxbxRtlAllocDebug(Heap: THandle; Flags: DWORD; Bytes: SIZE_T; pFile: PChar; Line: Integer): Pointer;

// ******************************************************************
// * CxbxRtlFreeDebug - Debug track RTL Free
// ******************************************************************
function  CxbxRtlFreeDebug(Heap: THandle; Flags: DWORD; pMem: PVOID; pFile: PChar; Line: Integer): BOOL;

// ******************************************************************
// * CxbxRtlReallocDebug - Debug track RTL realloc
// ******************************************************************
function CxbxRtlReallocDebug(Heap: THandle; Flags: DWORD; pMem: PVOID; Bytes: SIZE_T; pFile: PChar; Line: Integer): Pointer;

// ******************************************************************
// * CxbxRtlHeapSizeDebug - Debug track RTL heap size
// ******************************************************************
SIZE_T CxbxRtlSizeHeapDebug(THandle Heap,
                            DWORD  Flags,
                            PVOID  pMem,
                            Char  *pFile,
                            Integer    Line);

// ******************************************************************
// * CxbxVirtualQueryDebug - Debug virtual query
// ******************************************************************
DWORD CxbxVirtualQueryDebug(LPCVOID                   lpAddress,
                            PMEMORY_BASIC_INFORMATION lpBuffer,
                            DWORD                     dwLength);

// ******************************************************************
// * CxbxAllocDump - Dump the memory allocations
// ******************************************************************
procedure CxbxAllocDump(DumpData: bool);

//else // _DEBUG_ALLOC
*)

function CxbxMalloc(x: Integer): Pointer;
begin
  Result := AllocMem(x);
end;

procedure CxbxFree(x: Pointer);
begin
  FreeMem(x);
end;

(*
const CxbxCalloc(x, y) =                         calloc(x, y);
const CxbxFree(x) =                              Free(x);
const CxbxRtlAlloc(Heap, = Flags, Bytes)         NtDll.RtlAllocateHeap(Heap, Flags, Bytes);
const CxbxRtlFree(Heap, = Flags, pMem)           NtDll.RtlFreeHeap(Heap, Flags, pMem);
const CxbxRtlRealloc(Heap, = Flags, pMem, Bytes) NtDll.RtlReAllocateHeap(Heap, Flags, pMem, Bytes);
const CxbxRtlSizeHeap(Heap, = Flags, pMem)       NtDll.RtlSizeHeap(Heap, Flags, pMem);
//endif

//end EMUALLOC_H


#ifdef _DEBUG_ALLOC

//include 'mutex.h'

// ******************************************************************
// * prevent name collisions
// ******************************************************************
namespace NtDll
begin
    //include 'EmuNtDll.h'
);

// ******************************************************************
// * Memory tracking stuff
// ******************************************************************

  uint32 MEMORY_GUARD := $DEADFADE;

type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value **** )

begin
    CXBX_ALLOC_NORMAL,
    CXBX_ALLOC_RTL
end;
CXBX_ALLOC_TYPE;

type

CXBX_MEMORY_BLOCK  = record
    procedure                      *pMem;
    Integer                     Size;
    Char                      *pFile;
    uint32                     Line;
    CXBX_ALLOC_TYPE            cType;
    struct _CXBX_MEMORY_BLOCK *pNext;
 end;

 CXBX_MEMORY_BLOCK *g_pFirstBlock := 0;
 CXBX_MEMORY_BLOCK *g_pLastBlock := 0;
 Mutex g_MemoryMutex;

// ******************************************************************
// * GetMemStart - Retrieves the actual start of the allocated memory
// *               block (first guard)
// ******************************************************************
function GetMemStart(var pBlock: CXBX_MEMORY_BLOCK): Pointer;
begin
  Result := ((Char)pBlock.pMem - SizeOf(MEMORY_GUARD));
end;

// ******************************************************************
// * GetMemEnd - Retrieves the end of the allocated memory block
// *             (second guard)
// ******************************************************************
function GetMemEnd(var pBlock: CXBX_MEMORY_BLOCK): Pointer;
begin
  Result := ((Char)pBlock.pMem + pBlock.Size);
end;

// ******************************************************************
// * CheckIntegrity - Prints if the memory block is overwritten
// ******************************************************************
function CheckIntegrity(var pBlock: CXBX_MEMORY_BLOCK): bool;
begin
    bool Integrity := True;

    if (uint32)GetMemStart(pBlock) <> MEMORY_GUARD then
    begin
        printf('    Memory block corrupted at start, overwrite: $%.04X',
               *(uint32)GetMemStart(pBlock));
        Integrity := False;
    end;

    if (uint32)GetMemEnd(pBlock) <> MEMORY_GUARD then
    begin
        printf('    Memory block corrupted at end, overwrite: $.04X',
               *(uint32)GetMemEnd(pBlock));
        Integrity := False;
    end;

    Result := Integrity;
end;

// ******************************************************************
// * IsThisMemoryBlock - Simple block matching function
// ******************************************************************
function IsThisMemoryBlock(var pMem: Pointer; var pBlock: CXBX_MEMORY_BLOCK): bool;
begin
  Result := (pBlock and pMem = pBlock.pMem);
end;

// ******************************************************************
// * InThisMemoryBlock - Simple block matching function
// ******************************************************************
function InThisMemoryBlock(var InThisMemoryBlock(procedurepMem: bool): bool;     CXBX_MEMORY_BLOCK *pBlock)
begin
  Result := (pBlock and pMem >= pBlock.pMem and pMem <= GetMemEnd(pBlock));
end;


// ******************************************************************
// * InsertMemoryBlock - Inserts a new memory block in the tracker
// ******************************************************************
 CXBX_MEMORY_BLOCK *InsertMemoryBlock(Pointer           *pMem,
                                            Integer          Size,
                                            Char           *pFile,
                                            Integer             Line,
                                            CXBX_ALLOC_TYPE cType)
begin
    CXBX_MEMORY_BLOCK *pBlock = (CXBX_MEMORY_BLOCK)
                                malloc(SizeOf(CXBX_MEMORY_BLOCK));
    pBlock.pMem := (uint08)pMem + SizeOf(MEMORY_GUARD);
    pBlock.Size := Size;
    Integer Length := strlen(pFile) + 1;
    pBlock.pFile := (Char)malloc(Length);
    memcpy(pBlock.pFile, pFile, Length);
    pBlock.pNext := 0;
    pBlock.Line := Line;
    pBlock.cType := cType;
    *(uint32)(GetMemStart(pBlock)) := MEMORY_GUARD;
    *(uint32)(GetMemEnd(pBlock)) := MEMORY_GUARD;

    if not Assigned(g_pFirstBlock) then
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
CXBX_MEMORY_BLOCK *RemoveMemoryBlock(Pointer pMem)
begin
    CXBX_MEMORY_BLOCK *pFree := nil;
    if IsThisMemoryBlock(pMem, g_pFirstBlock) then
    begin
        pFree := g_pFirstBlock;
        g_pFirstBlock := g_pFirstBlock.pNext;
        if pFree = g_pLastBlock then
        begin
            g_pLastBlock := 0;
        end;
    end
    else
    begin
        CXBX_MEMORY_BLOCK *pCur;
        CXBX_MEMORY_BLOCK *pPrev := 0;
        for(pCur := g_pFirstBlock; pCur; pCur = pCur.pNext)
        begin
            if IsThisMemoryBlock(pMem, pCur) then
            begin
                if pCur = g_pLastBlock then
                begin
                    g_pLastBlock := pPrev;
                end;

                pFree := pCur;
                pPrev.pNext := pCur.pNext;
                break;
            end;
            pPrev := pCur;
        end;
    end;
    Result := pFree;
 end;

// ******************************************************************
// * FindMemoryBlock - Finds a memory block in the tracker
// ******************************************************************
 CXBX_MEMORY_BLOCK *FindMemoryBlock(Pointer pMem)
begin
    CXBX_MEMORY_BLOCK *pCur;

    for(pCur := g_pFirstBlock; pCur; pCur = pCur.pNext)
    begin
        if IsThisMemoryBlock(pMem, pCur) then
        begin
            Result := pCur;
        end;
    end;

   Result := 0;
end;

// ******************************************************************
// * FindMemoryBlockIn - Finds a memory block in the tracker
// ******************************************************************
 CXBX_MEMORY_BLOCK *FindMemoryBlockIn( Pointer pMem)
begin
    CXBX_MEMORY_BLOCK *pCur;

    for(pCur := g_pFirstBlock; pCur; pCur = pCur.pNext)
    begin
        if InThisMemoryBlock(pMem, pCur) then
        begin
            Result := pCur;
        end;
    end;

   Result := 0;
end;

// ******************************************************************
// * CxbxAllocDump - Dump the memory allocations
// ******************************************************************
procedure CxbxAllocDump(DumpData: bool);
begin
    g_MemoryMutex.Lock();

    CXBX_MEMORY_BLOCK *pCur;
    printf('******************************************************'
           '* Dumping memory allocations                         *'
           '******************************************************');
    for(pCur := g_pFirstBlock; pCur; pCur = pCur.pNext)
    begin
        printf(''
               '    Block: $%.08X'
               '    Size : %d'
               '    File : %s'
               '    Line : %d'
               '    cType : %s',
               pCur.pMem, pCur.Size, pCur.pFile, pCur.Line,
               pCur.cType := CXBX_ALLOC_NORMAL ? 'NORMAL' : 'RTL');
        CheckIntegrity(pCur);
     end;

    g_MemoryMutex.Unlock();
 end;

// ******************************************************************
// * CxbxMallocDebug - Debug track malloc
// ******************************************************************
function CxbxMallocDebug(Size: Integer; pFile: PChar; Line: Integer): Pointer;
begin
    Pointer pRetMem := 0;
    g_MemoryMutex.Lock();

    Pointer pMem := malloc(Size + 2 * SizeOf(MEMORY_GUARD));
    if not Assigned(pMem) then
    begin
        printf('CxbxMallocDebug: Allocation failed'
               '    Size: %d'
               '    File: %s'
               '    Line: %d',
               Size, pFile, Line);
    end
    else
    begin
        CXBX_MEMORY_BLOCK *pBlock = InsertMemoryBlock(pMem,
                                                      Size,
                                                      pFile,
                                                      Line,
                                                      CXBX_ALLOC_NORMAL);
        pRetMem := pBlock.pMem;
    end;

    g_MemoryMutex.Unlock();

    Result := pRetMem;
end;

// ******************************************************************
// * CxbxCallocDebug - Debug track calloc
// ******************************************************************
function CxbxCallocDebug(NbrElements: Integer; ElementSize: Integer; pFile: PChar; Line: Integer): Pointer;
begin
    Pointer pRetMem := 0;
    g_MemoryMutex.Lock();

    Pointer pMem := calloc(NbrElements * ElementSize + 2 * SizeOf(MEMORY_GUARD), 1);
    if not Assigned(pMem) then
    begin
        printf('CxbxCallocDebug: Allocation failed'
               '    NbrElements: %d'
               '    ElementSize: %d'
               '    File       : %s'
               '    Line       : %d',
               NbrElements, ElementSize, pFile, Line);
    end
    else
    begin
        CXBX_MEMORY_BLOCK *pBlock = InsertMemoryBlock(pMem,
                                                      NbrElements * ElementSize,
                                                      pFile,
                                                      Line,
                                                      CXBX_ALLOC_NORMAL);
        pRetMem := pBlock.pMem;
     end;

    g_MemoryMutex.Unlock();

    Result := pRetMem;
 end;

// ******************************************************************
// * CxbxFreeDebug - Debug track Free
// ******************************************************************
procedure  CxbxFreeDebug(pMem: Pointer;
                    Char *pFile,
                    Integer   Line)
begin
    if (pMem = 0) then
    begin
        Exit;
     end;
    g_MemoryMutex.Lock();

    CXBX_MEMORY_BLOCK *pFree := RemoveMemoryBlock(pMem);
    if not Assigned(pFree) then
    begin
        printf('CxbxFreeDebug: Free on non-existent block: $%.08X not  '
               'Possibly a multiple Free.'
               '    File: %s'
               '    Line: %d',
               pMem, pFile, Line);
    end
    else
    begin
        if not CheckIntegrity(pFree) then
        begin
            printf('CxbxFreeDebug: Free on damaged block'
                   '    Block   : $.%08X'
                   '    Allocation'
                   '        File: %s'
                   '        Line: %d'
                   '    Free'
                   '        File: %s'
                   '        Line: %d',
                   pFree.pMem, pFree.pFile, pFree.Line, pFile, Line);
        end;
        Free(GetMemStart(pFree));
        Free(pFree.pFile);
        Free(pFree);
     end;

    g_MemoryMutex.Unlock();
 end;

// ******************************************************************
// * CxbxRtlAllocDebug - Debug track RTL alloc
// ******************************************************************
function CxbxRtlAllocDebug(Heap: THandle; Flags: DWORD; Bytes: SIZE_T; pFile: PChar; Line: Integer): Pointer;
begin
    Pointer pRetMem := 0;
    g_MemoryMutex.Lock();

    Pointer pMem := NtDll.RtlAllocateHeap(Heap, Flags, Bytes + 2 * SizeOf(MEMORY_GUARD));
    if not Assigned(pMem) then
    begin
        printf('CxbxRtlAllocDebug: Allocation failed'
               '    Heap  : $%.08X'
               '    Flags : $%.08X'
               '    Bytes : %d'
               '    File  : %s'
               '    Line  : %d',
               Heap, Flags, Bytes, pFile, Line);
    end
    else
    begin
        CXBX_MEMORY_BLOCK *pBlock = InsertMemoryBlock(pMem,
                                                      Bytes,
                                                      pFile,
                                                      Line,
                                                      CXBX_ALLOC_RTL);
        pRetMem := pBlock.pMem;
     end;

    g_MemoryMutex.Unlock();

    Result := pRetMem;
 end;

// ******************************************************************
// * CxbxRtlFreeDebug - Debug track RTL Free
// ******************************************************************
function  CxbxRtlFreeDebug(Heap: THandle; Flags: DWORD; pMem: PVOID; pFile: PChar; Line: Integer): BOOL;
begin
    BOOL Ret := False;
    if (pMem = 0) then
    begin
        Result := True;
     end;
    g_MemoryMutex.Lock();

    CXBX_MEMORY_BLOCK *pFree := RemoveMemoryBlock(pMem);
    if not Assigned(pFree) then
    begin
        printf('CxbxRtlFreeDebug: Free on non-existent block: $%.08X not  '
               'Possibly a multiple Free.'
               '    File: %s'
               '    Line: %d',
               pMem, pFile, Line);
    end
    else
    begin
        if not CheckIntegrity(pFree) then
        begin
            printf('CxbxRtlFreeDebug: Free on damaged block'
                   '    Block   : $.%08X'
                   '    Allocation'
                   '        File: %s'
                   '        Line: %d'
                   '    Free'
                   '        File: %s'
                   '        Line: %d',
                   pFree.pMem, pFree.pFile, pFree.Line, pFile, Line);
         end;
        Ret := NtDll.RtlFreeHeap(Heap, Flags, GetMemStart(pFree));
        Free(pFree.pFile);
        Free(pFree);
     end;

    g_MemoryMutex.Unlock();
    Result := Ret;
 end;

// ******************************************************************
// * CxbxRtlReallocDebug - Debug track RTL realloc
// ******************************************************************
function CxbxRtlReallocDebug(Heap: THandle; Flags: DWORD; pMem: PVOID; Bytes: SIZE_T; pFile: PChar; Line: Integer): Pointer;
begin
    Pointer pRetMem := 0;
    g_MemoryMutex.Lock();

    CXBX_MEMORY_BLOCK *pRealloc := FindMemoryBlock(pMem);
    if not Assigned(pRealloc) then
    begin
        printf('CxbxRtlRealloc: realloc on non-existent block: $%.08X not  '
               '    File: %s'
               '    Line: %d',
               pMem, pFile, Line);
    end
    else
    begin
        if not CheckIntegrity(pRealloc) then
        begin
            printf('CxbxRtlReallocDebug: Realloc on damaged block'
                   '    Block   : $.%08X'
                   '    Allocation'
                   '        Size: %d'
                   '        File: %s'
                   '        Line: %d'
                   '    Reallocation'
                   '        Size: %d'
                   '        File: %s'
                   '        Line: %d',
                   pRealloc.pMem,
                   pRealloc.pFile, pRealloc.Size, pRealloc.Line,
                   Bytes, pFile, Line);
         end;
        Pointer pNewMem := NtDll.RtlReAllocateHeap(Heap, Flags, GetMemStart(pRealloc), Bytes + 2 * SizeOf(MEMORY_GUARD));
        Free(pRealloc.pFile);
        Free(pRealloc);
        if not Assigned(pNewMem) then
        begin
            printf('CxbxRtlReallocDebug: Reallocation failed'
                   '    Heap  : $%.08X'
                   '    Flags : $%.08X'
                   '    pMem  : $%.08X'
                   '    Bytes : %d'
                   '    File  : %s'
                   '    Line  : %d',
                   Heap, Flags, pMem, Bytes, pFile, Line);
        end
        else
        begin
            CXBX_MEMORY_BLOCK *pBlock = InsertMemoryBlock(pNewMem,
                                                          Bytes,
                                                          pFile,
                                                          Line,
                                                          CXBX_ALLOC_RTL);
            pRetMem := pBlock.pMem;
         end;
     end;

    g_MemoryMutex.Unlock();
    Result := pRetMem;
 end;

// ******************************************************************
// * CxbxRtlSizeHeapDebug - Debug track RTL heap size
// ******************************************************************
SIZE_T CxbxRtlSizeHeapDebug(THandle Heap,
                            DWORD  Flags,
                            PVOID  pMem,
                            Char  *pFile,
                            Integer    Line)
begin
    SIZE_T Size := 0;
    g_MemoryMutex.Lock();

    CXBX_MEMORY_BLOCK *pBlock := FindMemoryBlock(pMem);
    if not Assigned(pBlock) then
    begin
        printf('CxbxRtlSizeHeap: size heap on non-existent block: $%.08X not  '
               '    File: %s'
               '    Line: %d',
               pMem, pFile, Line);
    end
    else
    begin
        SIZE_T ActualSize = NtDll.RtlSizeHeap(Heap, Flags, GetMemStart(pBlock))
                            - 2 * SizeOf(MEMORY_GUARD);
        if ActualSize <> pBlock.Size then
        begin
            printf('CxbxRtlSizeHeap: heap size mismatch, RtlSizeHeap: %d Tracker: %d'
                   '    File  : %s'
                   '    Line  : %d',
                   ActualSize,
                   pBlock.Size,
                   pFile,
                   Line);
        end;
        Size := ActualSize;
    end;

    g_MemoryMutex.Unlock();
    Result := Size;
end;

// ******************************************************************
// * CxbxVirtualQueryDebug - Debug virtual query
// ******************************************************************
DWORD CxbxVirtualQueryDebug(LPCVOID                   lpAddress,
                            PMEMORY_BASIC_INFORMATION lpBuffer,
                            DWORD                     dwLength)
begin
    DWORD Size := 0;
    g_MemoryMutex.Lock();

    lpBuffer.State := MEM_COMMIT;

    CXBX_MEMORY_BLOCK *pBlock := FindMemoryBlockIn(lpAddress);

    if Assigned(pBlock) then
    begin
        Size := dwLength;
        lpBuffer.RegionSize := pBlock.Size;
        lpBuffer.BaseAddress := pBlock.pMem;
     end;
    g_MemoryMutex.Unlock();
    Result := Size;
 end;

//endif // _DEBUG_ALLOC
*)

end.
