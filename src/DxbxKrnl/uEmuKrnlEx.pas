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

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
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
  uEmu,
  uEmuAlloc,
  uEmuFS,
  uEmuFile,
  uEmuXapi,
  uEmuKrnl,
  uDxbxKrnl;

var
  {016}xboxkrnl_ExEventObjectType: POBJECT_TYPE = NULL;
  {022}xboxkrnl_ExMutantObjectType: POBJECT_TYPE = NULL;
  {030}xboxkrnl_ExSemaphoreObjectType: POBJECT_TYPE = NULL;
  {031}xboxkrnl_ExTimerObjectType: POBJECT_TYPE = NULL;

function {012} xboxkrnl_ExAcquireReadWriteLockExclusive(
  Arg1: DWORD
  ): NTSTATUS; stdcall;
function {013} xboxkrnl_ExAcquireReadWriteLockShared(
  Arg1: DWORD
  ): NTSTATUS; stdcall;
function {014} xboxkrnl_ExAllocatePool(
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
function {015} xboxkrnl_ExAllocatePoolWithTag(
  NumberOfBytes: SIZE_T;
  Tag: ULONG
  ): PVOID; stdcall;
procedure {017} xboxkrnl_ExFreePool(
  Block: PVOID
  ); stdcall;
function {018} xboxkrnl_ExInitializeReadWriteLock(
  Arg1: DWORD
  ): NTSTATUS; stdcall;
function {019} xboxkrnl_ExInterlockedAddLargeInteger(
  Addend: PLARGE_INTEGER;
  Increment: LARGE_INTEGER;
  Lock: PKSPIN_LOCK
  ): _LARGE_INTEGER; stdcall;
procedure {020} xboxkrnl_ExInterlockedAddLargeStatistic(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  Increment: ULONG;
  Addend: PLARGE_INTEGER
  ); register;
function {021} xboxkrnl_ExInterlockedCompareExchange64(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  Exchange: PLONGLONG;
  Destination: PLONGLONG; // OUT
  Comparand: PLONGLONG;
  Lock: PKSPIN_LOCK
  ): LONGLONG; register;
function {023} xboxkrnl_ExQueryPoolBlockSize(
  PoolBlock: PVOID;
  QuotaCharged: PBOOLEAN // OUT
  ): SIZE_T; stdcall;
function {024} xboxkrnl_ExQueryNonVolatileSetting(
  ValueIndex: DWORD;
  Type_: PDWORD; // out
  Value: PUCHAR; // out
  ValueLength: SIZE_T;
  ResultLength: PSIZE_T // out, OPTIONAL
  ): NTSTATUS; stdcall;
function {025} xboxkrnl_ExReadWriteRefurbInfo(
  Arg1: PXBOX_REFURB_INFO;
  Arg2Size: DWORD;
  Arg3: LONGBOOL
  ): NTSTATUS; stdcall;
procedure {026} xboxkrnl_ExRaiseException(
  ExceptionRecord: PEXCEPTION_RECORD
  ); stdcall;
procedure {027} xboxkrnl_ExRaiseStatus(
  Status: NTSTATUS
  ); stdcall;
function {028} xboxkrnl_ExReleaseReadWriteLock(
  Arg1: DWORD
  ): NTSTATUS; stdcall;
function {029} xboxkrnl_ExSaveNonVolatileSetting(
  ValueIndex: DWORD;
  Type_: PDWORD; //   OUT
  Value: PUCHAR;
  ValueLength: SIZE_T
  ): NTSTATUS; stdcall;
function {032} xboxkrnl_ExfInterlockedInsertHeadList(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  ListEntry: PLIST_ENTRY;
  ListHead: PLIST_ENTRY;
  Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; register;
function {033} xboxkrnl_ExfInterlockedInsertTailList(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  ListEntry: PLIST_ENTRY;
  ListHead: PLIST_ENTRY;
  Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; register;
function {034} xboxkrnl_ExfInterlockedRemoveHeadList(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  ListHead: PLIST_ENTRY;
  Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; register;


implementation

function {012} xboxkrnl_ExAcquireReadWriteLockExclusive(
  Arg1: DWORD
  ): NTSTATUS; stdcall;
// Source:APILogger - Uncertain  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ExAcquireReadWriteLockExclusive');
  // KeWaitForSingleObject
  EmuSwapFS(fsXbox);
end;

function {013} xboxkrnl_ExAcquireReadWriteLockShared(
  Arg1: DWORD
  ): NTSTATUS; stdcall;
// Source:APILogger - Uncertain  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ExAcquireReadWriteLockShared');
  // KeWaitForSingleObject
  EmuSwapFS(fsXbox);
end;

function {014} xboxkrnl_ExAllocatePool
(
  NumberOfBytes: ULONG
): PVOID; stdcall;
// Source:Cxbx  Branch:shogun  Revision:0.8.2-Pre2  Translator:PatrickvL  Done:100
var
  pRet: PVOID;
begin
  EmuSwapFS(fsWindows);

  //  Result := xboxkrnl_ExAllocatePoolWithTag(NumberOfBytes, ULONG($656E6F4E {?}));
{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : ExAllocatePool' +
           #13#10'(' +
           #13#10'   NumberOfBytes       : 0x%.08X' +
           #13#10');',
           [NumberOfBytes]);
{$ENDIF}

  pRet := DxbxMalloc(NumberOfBytes);
  EmuSwapFS(fsXbox);
  Result := pRet;
end;

// Differences from NT: There is no PoolType field, as the XBOX
// only has 1 pool, the non-paged pool.
function {015} xboxkrnl_ExAllocatePoolWithTag
(
  NumberOfBytes: SIZE_T;
  Tag: ULONG
): PVOID; stdcall;
// Source:Cxbx  Branch:shogun  Revision:0.8.2-Pre2  Translator:PatrickvL  Done:100
var
  pRet: PVOID;
begin
  EmuSwapFS(fsWindows);
  
{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : ExAllocatePoolWithTag' +
           #13#10'(' +
           #13#10'   NumberOfBytes       : 0x%.08X' +
           #13#10'   Tag                 : 0x%.08X' +
           #13#10');',
           [NumberOfBytes, Tag]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  pRet := DxbxMalloc(NumberOfBytes);

  EmuSwapFS(fsXbox);
  Result := pRet;
end;

procedure {017} xboxkrnl_ExFreePool(
  Block: PVOID
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('ExFreePool');
  // ExFreeNonPagedPool
  EmuSwapFS(fsXbox);
end;

function {018} xboxkrnl_ExInitializeReadWriteLock(
  Arg1: DWORD
  ): NTSTATUS; stdcall;
// Source:APILogger - Uncertain  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ExInitializeReadWriteLock');
  EmuSwapFS(fsXbox);
end;

function {019} xboxkrnl_ExInterlockedAddLargeInteger(
  Addend: PLARGE_INTEGER;
  Increment: LARGE_INTEGER;
  Lock: PKSPIN_LOCK
  ): _LARGE_INTEGER; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('ExInterlockedAddLargeInteger');
  LARGE_INTEGER(Result).QuadPart := 0;
  EmuSwapFS(fsXbox);
end;

procedure {020} xboxkrnl_ExInterlockedAddLargeStatistic(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Increment: ULONG;
  {1 ECX}Addend: PLARGE_INTEGER
  ); register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('ExInterlockedAddLargeStatistic');
  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

function {021} xboxkrnl_ExInterlockedCompareExchange64(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Exchange: PLONGLONG;
  {1 ECX}Destination: PLONGLONG; // OUT
  {3 stack}Comparand: PLONGLONG;
  {4 stack}Lock: PKSPIN_LOCK
  ): LONGLONG; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ExInterlockedCompareExchange64');
  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

function {023} xboxkrnl_ExQueryPoolBlockSize(
  PoolBlock: PVOID;
  QuotaCharged: PBOOLEAN // OUT
  ): SIZE_T; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ExQueryPoolBlockSize');
  EmuSwapFS(fsXbox);
end;

// ExQueryNonVolatileSetting retrieves EEPROM information -
// this function, when first called, creates a "shadow" copy
// of the EEPROM in RAM which is used in subsequent calls to Query,
// and updated by ExSaveNonVolatileSetting.
function {024} xboxkrnl_ExQueryNonVolatileSetting
(
  ValueIndex: DWORD;
  Type_: PDWORD; // out
  Value: PUCHAR; // out
  ValueLength: SIZE_T;
  ResultLength: PSIZE_T // out, OPTIONAL
): NTSTATUS; stdcall;
// Source:OpenXDK  Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : ExQueryNonVolatileSetting' +
         #13#10'(' +
         #13#10'   ValueIndex          : 0x%.08X' +
         #13#10'   Type                : 0x%.08X' +
         #13#10'   Value               : 0x%.08X' +
         #13#10'   ValueLength         : 0x%.08X' +
         #13#10'   ResultLength        : 0x%.08X' +
         #13#10');',
         [ValueIndex, Type_, Value, ValueLength, ResultLength]);
{$ENDIF}

  // handle eeprom read
  case ValueIndex of
    // Factory Game Region
    $104:
    begin
      // TODO -oCXBX: configurable region or autodetect of some sort
      if (Type_ <> nil) then
        Type_^ := $04;

      if (Value <> nil) then
        Value^ := $01;  // North America

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    // Factory AC Region
    $103:
    begin
      // TODO -oCXBX: configurable region or autodetect of some sort
      if (Type_ <> nil) then
        Type_^ := $04;

      if (Value <> nil) then
        Value^ := $01; // NTSC_M

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    // Language
    Ord(EEPROM_LANGUAGE):
    begin
      // TODO -oCXBX: configurable language or autodetect of some sort
      if (Type_ <> nil) then
        Type_^ := $04;

      if (Value <> nil) then
        Value^ := $01;  // English

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    // Video Flags
    Ord(EEPROM_VIDEO):
    begin
      // TODO -oCXBX: configurable video flags or autodetect of some sort
      if (Type_ <> nil) then
        Type_^ := $04;

      if (Value <> nil) then
        Value^ := $10;  // Letterbox

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    Ord(EEPROM_AUDIO):
    begin
      if (Type_ <> nil) then
        Type_^ := $04;

      if (Value <> nil) then
        Value^ := 0;

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    Ord(EEPROM_MISC):
    begin
      if (Type_ <> nil) then
        Type_^ := $04;

      if (Value <> nil) then
        Value^ := 0;

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    (* Timezone info
    $0FF:
    asm
      int 3
    end;
    //*)

  else
    EmuWarning('ExQueryNonVolatileSetting unknown ValueIndex (%d)', [ValueIndex]);
  end;

  EmuSwapFS(fsXbox);
  Result := STATUS_SUCCESS;
end;

function {025} xboxkrnl_ExReadWriteRefurbInfo(
  Arg1: PXBOX_REFURB_INFO;
  Arg2Size: DWORD;
  Arg3: LONGBOOL
  ): NTSTATUS; stdcall;
// Source:XBMC - Uncertain  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ExReadWriteRefurbInfo');
  EmuSwapFS(fsXbox);
end;

procedure {026} xboxkrnl_ExRaiseException(
  ExceptionRecord: PEXCEPTION_RECORD
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('ExRaiseException');
  // RtlRaiseException(ExceptionRecord);
  EmuSwapFS(fsXbox);
end;

procedure {027} xboxkrnl_ExRaiseStatus(
  Status: NTSTATUS
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('ExRaiseStatus');
  EmuSwapFS(fsXbox);
end;

function {028} xboxkrnl_ExReleaseReadWriteLock(
  Arg1: DWORD
  ): NTSTATUS; stdcall;
// Source:APILogger - Uncertain  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ExReleaseReadWriteLock');
  EmuSwapFS(fsXbox);
end;

function {029} xboxkrnl_ExSaveNonVolatileSetting(
  ValueIndex: DWORD;
  Type_: PDWORD; //   OUT
  Value: PUCHAR;
  ValueLength: SIZE_T
  ): NTSTATUS; stdcall;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ExSaveNonVolatileSetting');
  EmuSwapFS(fsXbox);
end;

function {032} xboxkrnl_ExfInterlockedInsertHeadList(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}ListEntry: PLIST_ENTRY;
  {1 ECX}ListHead: PLIST_ENTRY;
  {3 stack}Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('ExfInterlockedInsertHeadList');
  Result := nil;
  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

function {033} xboxkrnl_ExfInterlockedInsertTailList(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}ListEntry: PLIST_ENTRY;
  {1 ECX}ListHead: PLIST_ENTRY;
  {3 stack}Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('ExfInterlockedInsertTailList');
  Result := nil;
  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

function {034} xboxkrnl_ExfInterlockedRemoveHeadList(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}ListHead: PLIST_ENTRY;
  {1 ECX}Lock: PKSPIN_LOCK
  ): PLIST_ENTRY; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('ExfInterlockedRemoveHeadList');
  Result := nil;
  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

end.
