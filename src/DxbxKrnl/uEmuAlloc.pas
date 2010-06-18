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
  SysUtils,
  Classes,
  // Jedi Win32API
  JwaWinType,
  JwaNative,
  // Dxbx
  uTypes,
  uDxbxUtils,
  uLog,
  uMutex;


function DxbxMalloc(x: Integer): Pointer;
function DxbxCalloc(x, y: Integer): Pointer;
procedure DxbxFree(x: Pointer);
//function DxbxCallocDebug(NbrElements: size_t; ElementSize: size_t; pFile: P_char; Line: int): Pvoid;
function DxbxRtlAlloc(Heap: HANDLE; Flags: ULONG; Bytes: SIZE_T): PVOID;
function DxbxRtlFree(Heap: Handle; Flags: DWORD; pMem: PVOID): BOOL;
function DxbxRtlRealloc(Heap: HANDLE; Flags: ULONG; pMem: PVOID; Bytes: SIZE_T): PVOID;
function DxbxRtlSizeHeap(Heap: HANDLE; Flags: ULONG; pMem: PVOID): SIZE_T;

implementation

{$IFDEF _DEBUG_ALLOC}

(*
const DxbxMalloc(x)                            DxbxMallocDebug(x, __FILE__, __LINE__);
const DxbxCalloc(x, y)                         DxbxCallocDebug(x, y, __FILE__, __LINE__);
const DxbxFree(x)                              DxbxFreeDebug(x, __FILE__, __LINE__);
const DxbxRtlAlloc(Heap, Flags, Bytes)         DxbxRtlAllocDebug(Heap, Flags, Bytes, __FILE__, __LINE__);
const DxbxRtlFree(Heap, Flags, pMem)           DxbxRtlFreeDebug(Heap, Flags, pMem, __FILE__, __LINE__);
const DxbxRtlRealloc(Heap, Flags, pMem, Bytes) DxbxRtlReallocDebug(Heap, Flags, pMem, Bytes, __FILE__, __LINE__);
const DxbxRtlSizeHeap(Heap, Flags, pMem)       DxbxRtlSizeHeapDebug(Heap, Flags, pMem, __FILE__, __LINE__);
*)

{$ELSE !_DEBUG_ALLOC}

function DxbxMalloc(x: Integer): Pointer;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := malloc(x);
end;

function DxbxCalloc(x, y: Integer): Pointer;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := calloc(x, y);
end;

procedure DxbxFree(x: Pointer);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  free(x);
end;

function DxbxRtlAlloc(Heap: HANDLE; Flags: ULONG; Bytes: SIZE_T): PVOID;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := JwaNative.RtlAllocateHeap(Heap, Flags, Bytes);
end;

function DxbxRtlFree(Heap: Handle; Flags: DWORD; pMem: PVOID): BOOL;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := BOOL(JwaNative.RtlFreeHeap(Heap, Flags, pMem));
end;

function DxbxRtlRealloc(Heap: HANDLE; Flags: ULONG; pMem: PVOID; Bytes: SIZE_T): PVOID;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := JwaNative.RtlReAllocateHeap(Heap, Flags, pMem, Bytes);
end;

function DxbxRtlSizeHeap(Heap: HANDLE; Flags: ULONG; pMem: PVOID): SIZE_T;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
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

const MEMORY_GUARD: uint32 = $DEADFADE;

type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value ****)

  CXBX_ALLOC_TYPE = (
    CXBX_ALLOC_NORMAL,
    CXBX_ALLOC_RTL
  );

type _CXBX_MEMORY_BLOCK = packed record
    pMem: Pvoid;
    Size: size_t;
    pFile: P_char;
    Line: uint32;
    Type_: CXBX_ALLOC_TYPE;
    pNext: PCXBX_MEMORY_BLOCK;
end;
CXBX_MEMORY_BLOCK = _CXBX_MEMORY_BLOCK;
PCXBX_MEMORY_BLOCK = ^CXBX_MEMORY_BLOCK;
 
var g_pFirstBlock: PCXBX_MEMORY_BLOCK = NULL;
var g_pLastBlock: PCXBX_MEMORY_BLOCK = NULL;
var g_MemoryMutex: Mutex;

// ******************************************************************
// * GetMemStart - Retrieves the actual start of the allocated memory
// *               block (first guard)
// ******************************************************************
function GetMemStart(pBlock: PCXBX_MEMORY_BLOCK): PVOID; inline;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := Pvoid(P_char(pBlock.pMem) - sizeof(MEMORY_GUARD));
end;

// ******************************************************************
// * GetMemEnd - Retrieves the end of the allocated memory block
// *             (second guard)
// ******************************************************************
function GetMemEnd(pBlock: PCXBX_MEMORY_BLOCK): PVOID; inline;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := Pvoid(P_char(pBlock.pMem) + pBlock.Size);
end;

// ******************************************************************
// * CheckIntegrity - Prints if the memory block is overwritten
// ******************************************************************
function CheckIntegrity(pBlock: PCXBX_MEMORY_BLOCK): _bool;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  Integrity: _bool;
begin
  Integrity := true;

  if Puint32(GetMemStart(pBlock))^ <> MEMORY_GUARD then
  begin
{$IFDEF DEBUG}
    DbgPrintf('    Memory block corrupted at start, overwrite: 0x%.04X',
              [Puint32(GetMemStart(pBlock))^]);
{$ENDIF}
    Integrity := false;
  end;

  if Puint32(GetMemEnd(pBlock))^ <> MEMORY_GUARD then
  begin
{$IFDEF DEBUG}
    DbgPrintf('    Memory block corrupted at end, overwrite: 0x%.04X',
              [Puint32(GetMemEnd(pBlock))^]);
{$ENDIF}
    Integrity := false;
  end;

  Result := Integrity;
end;


// ******************************************************************
// * IsThisMemoryBlock - Simple block matching function
// ******************************************************************
function IsThisMemoryBlock(pMem: Pvoid; 
                           pBlock: PCXBX_MEMORY_BLOCK): _bool; inline;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := Assigned(pBlock) and (pMem = pBlock.pMem);
end;

// ******************************************************************
// * InThisMemoryBlock - Simple block matching function
// ******************************************************************
function InThisMemoryBlock(const pMem: Pvoid; 
                           pBlock: PCXBX_MEMORY_BLOCK): _bool; inline;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := Assigned(pBlock)
        and (IntPtr(pMem) >= IntPtr(pBlock.pMem))
        and (IntPtr(pMem) <= IntPtr(GetMemEnd(pBlock)));
end;


// ******************************************************************
// * InsertMemoryBlock - Inserts a new memory block in the tracker
// ******************************************************************
function InsertMemoryBlock(pMem: Pvoid;
                           Size: size_t;
                           pFile: P_char;
                           Line: int;
                           Type_: CXBX_ALLOC_TYPE): PCXBX_MEMORY_BLOCK;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pBlock: PCXBX_MEMORY_BLOCK;
  Length: size_t;
begin
  pBlock := PCXBX_MEMORY_BLOCK(
            malloc(sizeof(CXBX_MEMORY_BLOCK)));
  pBlock.pMem := Puint08(UIntPtr(pMem) + sizeof(MEMORY_GUARD));
  pBlock.Size := Size;
  Length := strlen(pFile) + 1;
  pBlock.pFile := P_char(malloc(Length));
  memcpy(pBlock.pFile, pFile, Length);
  pBlock.pNext := NULL;
  pBlock.Line := Line;
  pBlock.Type_ := Type_;
  Puint32(GetMemStart(pBlock))^ := MEMORY_GUARD;
  Puint32(GetMemEnd(pBlock))^ := MEMORY_GUARD;

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
function RemoveMemoryBlock(pMem: Pvoid): PCXBX_MEMORY_BLOCK;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
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
    begin
      g_pLastBlock := NULL;
    end;
  end
  else
  begin
    pPrev := NULL;
    // Dxbx note : Translated 'for' to 'while', because counter is dereferenced instead of incremented :    
    pCur := g_pFirstBlock; while Assigned(pCur) do
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
      
      pCur := pCur.pNext;
    end; // while
  end;
  Result := pFree;
end;


// ******************************************************************
// * FindMemoryBlock - Finds a memory block in the tracker
// ******************************************************************
function FindMemoryBlock(pMem: Pvoid): PCXBX_MEMORY_BLOCK;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pCur: PCXBX_MEMORY_BLOCK;
begin
  // Dxbx note : Translated 'for' to 'while', because counter is dereferenced instead of incremented :    
  pCur := g_pFirstBlock; while Assigned(pCur) do
  begin
    if IsThisMemoryBlock(pMem, pCur) then
    begin
      Result := pCur;
      Exit;
    end;

    pCur := pCur.pNext;
  end; // while

  Result := NULL;
end;

// ******************************************************************
// * FindMemoryBlockIn - Finds a memory block in the tracker
// ******************************************************************
function FindMemoryBlockIn(const pMem: Pvoid): PCXBX_MEMORY_BLOCK;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pCur: PCXBX_MEMORY_BLOCK;
begin
  // Dxbx note : Translated 'for' to 'while', because counter is dereferenced instead of incremented :    
  pCur := g_pFirstBlock; while Assigned(pCur) do
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
// * DxbxAllocDump - Dump the memory allocations
// ******************************************************************
procedure DxbxAllocDump(DumpData: _bool);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pCur: PCXBX_MEMORY_BLOCK;
begin
  g_MemoryMutex.Lock();

{$IFDEF DEBUG}
  DbgPrintf('******************************************************'#13#10 +
            '* Dumping memory allocations                         *'#13#10 +
            '******************************************************');
{$ENDIF}
  // Dxbx note : Translated 'for' to 'while', because counter is dereferenced instead of incremented :    
  pCur := g_pFirstBlock; while Assigned(pCur) do
  begin
{$IFDEF DEBUG}
    DbgPrintf(#13#10 +
        #13#10'    Block: 0x%.08X' +
        #13#10'    Size : %d' +
        #13#10'    File : %s' +
        #13#10'    Line : %d' +
        #13#10'    Type : %s',
        [pCur.pMem, pCur.Size, pCur.pFile, pCur.Line,
        iif(pCur.Type_ = CXBX_ALLOC_NORMAL, 'NORMAL', 'RTL')]);
{$ENDIF}
    CheckIntegrity(pCur);
    
    pCur := pCur.pNext;
  end;

  g_MemoryMutex.Unlock();
end;


// ******************************************************************
// * DxbxMallocDebug - Debug track malloc
// ******************************************************************
function DxbxMallocDebug(Size: size_t;
                         pFile: P_char;
                         Line: int): Pvoid;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pRetMem: Pvoid;
  pMem: Pvoid;
  pBlock: PCXBX_MEMORY_BLOCK;
begin
  pRetMem := NULL;
  g_MemoryMutex.Lock();

  pMem := malloc(Size + 2 * sizeof(MEMORY_GUARD));
  if(nil=pMem) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('DxbxMallocDebug: Allocation failed' +
        #13#10'    Size: %d' +
        #13#10'    File: %s' +
        #13#10'    Line: %d',
        [Size, pFile, Line]);
{$ENDIF}
  end
  else
  begin
    pBlock := InsertMemoryBlock(pMem, 
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
// * DxbxCallocDebug - Debug track calloc
// ******************************************************************
function DxbxCallocDebug(NbrElements: size_t;
                         ElementSize: size_t;
                         pFile: P_char;
                         Line: int): Pvoid;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pRetMem: Pvoid;
  pMem: Pvoid;
  pBlock: PCXBX_MEMORY_BLOCK;
begin
  pRetMem := NULL;
  g_MemoryMutex.Lock();

  pMem := calloc(NbrElements * ElementSize + 2 * sizeof(MEMORY_GUARD), 1);
  if(nil=pMem) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('DxbxCallocDebug: Allocation failed' +
        #13#10'    NbrElements: %d' +
        #13#10'    ElementSize: %d' +
        #13#10'    File       : %s' +
        #13#10'    Line       : %d',
        [NbrElements, ElementSize, pFile, Line]);
{$ENDIF}
  end
  else
  begin
    pBlock := InsertMemoryBlock(pMem, 
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
// * DxbxFreeDebug - Debug track free
// ******************************************************************
procedure DxbxFreeDebug(pMem: Pvoid;
                        pFile: P_char;
                        Line: int);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pFree: PCXBX_MEMORY_BLOCK;
begin
  if pMem = NULL then
  begin
    Exit;
  end;
  g_MemoryMutex.Lock();

  pFree := RemoveMemoryBlock(pMem);
  if(pFree = NULL) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('DxbxFreeDebug: free on non-existent block: 0x%.08X! ' +
              'Possibly a multiple free.' +
        #13#10'    File: %s' +
        #13#10'    Line: %d',
        [pMem, pFile, Line]);
{$ENDIF}
  end
  else
  begin
    if not CheckIntegrity(pFree) then
    begin
{$IFDEF DEBUG}
      DbgPrintf('DxbxFreeDebug: Free on damaged block' +
          #13#10'    Block   : 0x%.08X' +
          #13#10'    Allocation' +
          #13#10'        File: %s' +
          #13#10'        Line: %d' +
          #13#10'    Free' +
          #13#10'        File: %s' +
          #13#10'        Line: %d',
          [pFree.pMem, pFree.pFile, pFree.Line, pFile, Line]);
{$ENDIF}
    end;

    free(GetMemStart(pFree));
    free(pFree.pFile);
    free(pFree);
  end;

  g_MemoryMutex.Unlock();
end;

// ******************************************************************
// * DxbxRtlAllocDebug - Debug track RTL alloc
// ******************************************************************
function DxbxRtlAllocDebug(Heap: HANDLE;
                           Flags: DWORD;
                           Bytes: SIZE_T;
                           pFile: P_char;
                           Line: int): Pvoid;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pRetMem: Pvoid;
  pMem: Pvoid;
  pBlock: PCXBX_MEMORY_BLOCK;
begin
  pRetMem := NULL;
  g_MemoryMutex.Lock();

  //pMem := NtDll.RtlAllocateHeap(Heap, Flags, Bytes + 2 * SizeOf(MEMORY_GUARD));
  pMem := DxbxRtlAlloc(Heap, Flags, Bytes + 2 * SizeOf(MEMORY_GUARD));
  if(nil=pMem) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('DxbxRtlAllocDebug: Allocation failed' +
        #13#10'    Heap  : 0x%.08X' +
        #13#10'    Flags : 0x%.08X' +
        #13#10'    Bytes : %d' +
        #13#10'    File  : %s' +
        #13#10'    Line  : %d',
        [Heap, Flags, Bytes, pFile, Line]);
{$ENDIF}
  end
  else
  begin
    pBlock := InsertMemoryBlock(pMem, 
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
// * DxbxRtlFreeDebug - Debug track RTL Free
// ******************************************************************
function DxbxRtlFreeDebug(Heap: HANDLE;
                          Flags: DWORD;
                          pMem: PVOID;
                          pFile: P_char;
                          Line: int): BOOL;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pFree: PCXBX_MEMORY_BLOCK;
begin
  Result := FALSE;
  if(pMem = NULL) then
  begin
    Result := TRUE;
    Exit;
  end;
  g_MemoryMutex.Lock();

  pFree := RemoveMemoryBlock(pMem);
  if(pFree = NULL) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('DxbxRtlFreeDebug: free on non-existent block: 0x%.08X! ' +
              'Possibly a multiple free.' +
        #13#10'    File: %s' +
        #13#10'    Line: %d',
        [pMem, pFile, Line]);
{$ENDIF}
  end
  else
  begin
    if not CheckIntegrity(pFree) then
    begin
{$IFDEF DEBUG}
      DbgPrintf('DxbxRtlFreeDebug: Free on damaged block' +
          #13#10'    Block   : $.%08X' +
          #13#10'    Allocation' +
          #13#10'        File: %s' +
          #13#10'        Line: %d' +
          #13#10'    Free' +
          #13#10'        File: %s' +
          #13#10'        Line: %d',
          [pFree.pMem, pFree.pFile, pFree.Line, pFile, Line]);
{$ENDIF}
    end;
    Result := DxbxRtlFree(Heap, Flags, GetMemStart(pFree));
    free(pFree.pFile);
    free(pFree);
  end;

  g_MemoryMutex.Unlock();
end;

// ******************************************************************
// * DxbxRtlReallocDebug - Debug track RTL realloc
// ******************************************************************
function DxbxRtlReallocDebug(Heap: HANDLE;
                             Flags: DWORD;
                             pMem: PVOID;
                             Bytes: SIZE_T;
                             pFile: P_char;
                             Line: int): Pvoid;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pRetMem: Pvoid;
  pRealloc: PCXBX_MEMORY_BLOCK;
  pNewMem: Pvoid;
  pBlock: PCXBX_MEMORY_BLOCK;
begin
  pRetMem := NULL;
  g_MemoryMutex.Lock();

  pRealloc := FindMemoryBlock(pMem);
  if(pRealloc = NULL) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('DxbxRtlRealloc: realloc on non-existent block: 0x%.08X!' +
        #13#10'    File: %s' +
        #13#10'    Line: %d',
        [pMem, pFile, Line]);
{$ENDIF}
  end
  else
  begin
    if not CheckIntegrity(pRealloc) then
    begin
{$IFDEF DEBUG}
      DbgPrintf('DxbxRtlReallocDebug: Realloc on damaged block' +
          #13#10'    Block   : 0x.%08X' +
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
{$ENDIF}
    end;
    pNewMem := DxbxRtlReAlloc(Heap, Flags, GetMemStart(pRealloc), Bytes + 2 * sizeof(MEMORY_GUARD));
    free(pRealloc.pFile);
    free(pRealloc);
    if(nil=pNewMem) then
    begin
{$IFDEF DEBUG}
      DbgPrintf('DxbxRtlReallocDebug: Reallocation failed' +
          #13#10'    Heap  : 0x%.08X' +
          #13#10'    Flags : 0x%.08X' +
          #13#10'    pMem  : 0x%.08X' +
          #13#10'    Bytes : %d' +
          #13#10'    File  : %s' +
          #13#10'    Line  : %d',
          [Heap, Flags, pMem, Bytes, pFile, Line]);
{$ENDIF}
    end
    else
    begin
      pBlock := InsertMemoryBlock(pNewMem, 
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
// * DxbxRtlSizeHeapDebug - Debug track RTL heap size
// ******************************************************************
function DxbxRtlSizeHeapDebug(Heap: HANDLE;
                              Flags: DWORD;
                              pMem: PVOID;
                              pFile: P_char;
                              Line: int): SIZE_T;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  Size: SIZE_T;
  pBlock: PCXBX_MEMORY_BLOCK;
  ActualSize: SIZE_T;
begin
  Size := 0;
  g_MemoryMutex.Lock();

  pBlock := FindMemoryBlock(pMem);
  if(pBlock = NULL) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('DxbxRtlSizeHeap: size heap on non-existent block: 0x%.08X! ' +
        #13#10'    File: %s' +
        #13#10'    Line: %d',
        [pMem, pFile, Line]);
{$ENDIF}
  end
  else
  begin
    ActualSize := DxbxRtlSizeHeap(Heap, Flags, GetMemStart(pBlock))
                        - 2 * SizeOf(MEMORY_GUARD);
{$IFDEF DEBUG}
    if(ActualSize <> pBlock.Size) then
    begin
      DbgPrintf('DxbxRtlSizeHeap: heap size mismatch, RtlSizeHeap: %d Tracker: %d' +
          #13#10'    File  : %s' +
          #13#10'    Line  : %d',
          [ActualSize,
           pBlock.Size,
           pFile,
           Line]);
    end;
{$ENDIF}
    Size := ActualSize;
  end;

  g_MemoryMutex.Unlock();
  Result := Size;
end;

// ******************************************************************
// * DxbxVirtualQueryDebug - Debug virtual query
// ******************************************************************
function DxbxVirtualQueryDebug(lpAddress: LPCVOID;
                               lpBuffer: PMEMORY_BASIC_INFORMATION;
                               dwLength: DWORD): DWORD;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
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

{.$MESSAGE 'PatrickvL reviewed up to here'}
end.
