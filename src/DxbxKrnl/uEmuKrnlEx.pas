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
  Windows, // VirtualQuery
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
  uDxbxUtils,
  uLog,
  uEmuAlloc,
  uEmuFS,
  uEmu,
  uEmuFile,
  uEmuKrnl,
  uEmuKrnlNt,
  uDxbxKrnl;

var
  {016}xboxkrnl_ExEventObjectType: POBJECT_TYPE = NULL; // TODO -oDxbx : What should we initialize this to?
  {022}xboxkrnl_ExMutantObjectType: POBJECT_TYPE = NULL; // TODO -oDxbx : What should we initialize this to?
  {030}xboxkrnl_ExSemaphoreObjectType: POBJECT_TYPE = NULL; // TODO -oDxbx : What should we initialize this to?
  {031}xboxkrnl_ExTimerObjectType: POBJECT_TYPE = NULL; // TODO -oDxbx : What should we initialize this to?

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
  ValueIndex: ULONG;
  Type_: PULONG; // OUT
  Value: PDWORD; // OUT
  ValueLength: ULONG;
  ResultLength: PULONG // OUT, OPTIONAL
  ): NTSTATUS; stdcall;
function {025} xboxkrnl_ExReadWriteRefurbInfo(
  pRefurbInfo: PXBOX_REFURB_INFO; // OUT
  dwBufferSize: ULONG;
  aIsWriteMode: _BOOLEAN
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

const lfUnit = lfCxbx or lfKernel;

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
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    LogBegin('EmuKrnl : ExAllocatePool >>').
      _(NumberOfBytes, 'NumberOfBytes').
    LogEnd();
    EmuSwapFS(fsXbox);
  end;

  Result := xboxkrnl_ExAllocatePoolWithTag(NumberOfBytes, ULONG($656E6F4E)); // MakeFourCC('None');
end;

// Differences from NT: There is no PoolType field, as the XBOX
// only has 1 pool, the non-paged pool.
function {015} xboxkrnl_ExAllocatePoolWithTag
(
  NumberOfBytes: SIZE_T;
  Tag: ULONG
): PVOID; stdcall;
// Source:Cxbx  Branch:shogun  Revision:0.8.2-Pre2  Translator:PatrickvL  Done:100

  function _TagStr: AnsiString;
  begin
    SetString(Result, PAnsiChar(@Tag), 4);
  end;

begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : ExAllocatePoolWithTag').
      _(NumberOfBytes, 'NumberOfBytes').
      _(Tag, 'Tag', _TagStr).
    LogEnd();

  // TODO -oCXBX: Actually implement this
  Result := VirtualAlloc(NULL, NumberOfBytes, MEM_COMMIT, PAGE_EXECUTE_READWRITE);

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuKrnl : ExAllocatePoolWithTag returns 0x%.08X', [UIntPtr(Result)]);

  EmuSwapFS(fsXbox);
end;

procedure {017} xboxkrnl_ExFreePool(
  Block: PVOID
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : ExFreePool').
      _(Block, 'Block').
    LogEnd();

  VirtualFree(Block, 0, MEM_RELEASE); //  DxbxFree(Block); // ExFreeNonPagedPool

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
end;

function {023} xboxkrnl_ExQueryPoolBlockSize(
  PoolBlock: PVOID;
  QuotaCharged: PBOOLEAN // OUT
  ): SIZE_T; stdcall;
// Source:Dxbx  Translator:PatrickvL  Done:100
var
  LMemInfo: Windows.TMemoryBasicInformation;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : ExQueryPoolBlockSize').
      _(PoolBlock, 'PoolBlock').
      _(QuotaCharged, 'QuotaCharged').
    LogEnd();

  // Get the VM status for the pointer
  LMemInfo.RegionSize := 0;
  Windows.VirtualQuery(PoolBlock, {var}LMemInfo, SizeOf(LMemInfo));
  Result := LMemInfo.RegionSize;

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('EmuKrnl : ExQueryPoolBlockSize returns 0x%.08X', [Result]);

  EmuSwapFS(fsXbox);
end;

// ExQueryNonVolatileSetting retrieves EEPROM information -
// this function, when first called, creates a "shadow" copy
// of the EEPROM in RAM which is used in subsequent calls to Query,
// and updated by ExSaveNonVolatileSetting.
function {024} xboxkrnl_ExQueryNonVolatileSetting
(
  ValueIndex: ULONG;
  Type_: PULONG; // OUT
  Value: PDWORD; // OUT
  ValueLength: ULONG;
  ResultLength: PULONG // OUT, OPTIONAL
): NTSTATUS; stdcall;
// Source:OpenXDK  Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : ExQueryNonVolatileSetting' +
           #13#10'(' +
           #13#10'   ValueIndex          : 0x%.08X' +
           #13#10'   Type                : 0x%.08X' +
           #13#10'   Value               : 0x%.08X' +
           #13#10'   ValueLength         : 0x%.08X' +
           #13#10'   ResultLength        : 0x%.08X' +
           #13#10');',
           [ValueIndex, Type_, Value, ValueLength, ResultLength]);

  // handle eeprom read
  case XC_VALUE_INDEX(ValueIndex) of
    XC_FACTORY_GAME_REGION:
    begin
      // TODO -oCXBX: configurable region or autodetect of some sort
      if (Type_ <> nil) then
        Type_^ := $04;

      if (Value <> nil) then
        Value^ := $01;  // North America

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    XC_FACTORY_AV_REGION:
    begin
      // TODO -oCXBX: configurable region or autodetect of some sort
      if (Type_ <> nil) then
        Type_^ := $04;

      if (Value <> nil) then
        Value^ := $01; // NTSC_M

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    XC_LANGUAGE: // Was Ord(EEPROM_LANGUAGE):
    begin
      // TODO -oCXBX: configurable language or autodetect of some sort
      if (Type_ <> nil) then
        Type_^ := $04;

      if (Value <> nil) then
        Value^ := $01;  // English

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    XC_VIDEO_FLAGS: // Was Ord(EEPROM_VIDEO):
    begin
      // TODO -oCXBX: configurable video flags or autodetect of some sort
      if (Type_ <> nil) then
        Type_^ := $04;

      if (Value <> nil) then
        Value^ := $10;  // Letterbox

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    XC_AUDIO_FLAGS: // Was Ord(EEPROM_AUDIO):
    begin
      if (Type_ <> nil) then
        Type_^ := $04;

      if (Value <> nil) then
        Value^ := $0;

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    XC_PARENTAL_CONTROL_GAMES: // Zapper queries this
    begin
      if (Type_ <> nil) then
        Type_^ := $0;

      if (Value <> nil) then
        Value^ := $0; // = XC_PC_ESRB_ALL;

      if (ResultLength <> nil) then
        ResultLength^ := $04;
    end;

    XC_MISC_FLAGS: // Was Ord(EEPROM_MISC):
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
    EmuWarning('ExQueryNonVolatileSetting unhandled ValueIndex (%d)', [ValueIndex]);
  end;

  EmuSwapFS(fsXbox);
  Result := STATUS_SUCCESS;
end;

function {025} xboxkrnl_ExReadWriteRefurbInfo(
  pRefurbInfo: PXBOX_REFURB_INFO; // OUT
  dwBufferSize: ULONG;
  aIsWriteMode: _BOOLEAN
  ): NTSTATUS; stdcall;
// Source:XBMC - Uncertain  Branch:Dxbx  Translator:PatrickvL  Done:0
var
  FileName: _STRING;
  ObjectAttributes: OBJECT_ATTRIBUTES;
  IoStatusBlock: IO_STATUS_BLOCK;
  ConfigPartitionHandle: Handle;
  RefurbInfoCopy: XBOX_REFURB_INFO;
  ByteOffset: LARGE_INTEGER;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : ExReadWriteRefurbInfo' +
           #13#10'(' +
           #13#10'   pRefurbInfo         : 0x%.08X' +
           #13#10'   dwBufferSize        : 0x%.08X' +
           #13#10'   aIsWriteMode        : 0x%.08X' +
           #13#10');',
           [pRefurbInfo, dwBufferSize, aIsWriteMode]);

  if Assigned(pRefurbInfo) then
  begin
    if dwBufferSize <> SizeOf(XBOX_REFURB_INFO) then
      Result := STATUS_INVALID_PARAMETER
    else
    begin
      // Open partition 0 directly :
      RtlInitAnsiString(@FileName, PCSZ(PAnsiChar(DeviceHarddisk0Partition0)));
      InitializeObjectAttributes(@ObjectAttributes, @FileName, OBJ_CASE_INSENSITIVE, 0, NULL);
      Result := xboxkrnl_NtOpenFile(
                @ConfigPartitionHandle,
                GENERIC_READ or DWORD(iif(aIsWriteMode, GENERIC_WRITE, 0)) or SYNCHRONIZE,
                @ObjectAttributes,
                @IoStatusBlock,
                FILE_SHARE_READ or FILE_SHARE_WRITE,
                FILE_SYNCHRONOUS_IO_ALERT);

      if (NT_SUCCESS(Result)) then
      begin
        ByteOffset.QuadPart := XBOX_REFURB_INFO_SECTOR_INDEX * XBOX_HD_SECTOR_SIZE;
        if aIsWriteMode then
        begin
          RefurbInfoCopy := pRefurbInfo^;
          RefurbInfoCopy.Signature_ := XBOX_REFURB_INFO_SIGNATURE;
          Result := xboxkrnl_NtWriteFile(ConfigPartitionHandle, 0, NULL, NULL, @IoStatusBlock, @RefurbInfoCopy, XBOX_HD_SECTOR_SIZE, @ByteOffset);
        end
        else
        begin
          Result := xboxkrnl_NtReadFile(ConfigPartitionHandle, 0, NULL, NULL, @IoStatusBlock, @RefurbInfoCopy, XBOX_HD_SECTOR_SIZE, @ByteOffset);
          if (NT_SUCCESS(Result)) then
          begin
            if RefurbInfoCopy.Signature_ = XBOX_REFURB_INFO_SIGNATURE then
              // No signature - clear output buffer :
              ZeroMemory(pRefurbInfo, SizeOf(XBOX_REFURB_INFO))
            else
              pRefurbInfo^ := RefurbInfoCopy;
          end;
        end;
        NtClose(ConfigPartitionHandle);
      end;
    end;
  end
  else
    Result := STATUS_UNSUCCESSFUL; // This may never happen!

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
end;

end.
