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
unit uEmuKrnl;

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
  uEmuFS,
  uEmuFile,
  uEmuXapi,
  uDxbxKrnl;

const
// The following symbols come from The NASMX Project, nasmx\inc\xbox\kernel.inc
// See http://www.asmcommunity.net/projects/nasmx/

  // Global XBOX Kernel Definitions
  XBOX_SMB_IO_BASE = $C000;
  XBOX_SMB_GLOBAL_ENABLE = (XBOX_SMB_IO_BASE + 2);
  XBOX_SMB_HOST_ADDRESS = (XBOX_SMB_IO_BASE + 4);
  XBOX_SMB_HOST_DATA = (XBOX_SMB_IO_BASE + 6);
  XBOX_SMB_HOST_COMMAND = (XBOX_SMB_IO_BASE + 8);

  XBOX_PIC_ADDRESS = $10;

  SMC_CMD_POWER = $02;
  SMC_CMD_POWER_RESET = $01;
  SMC_CMD_POWER_CYCLE = $40;
  SMC_CMD_POWER_OFF = $80;

  VIDEO_MEMORY = $F0040000;
  VIDEO_LIMIT = VIDEO_MEMORY + (640 * 480 * 4);

  // Global XBOX NT Definitions
  DELETE = $00010000;
  SYNCHRONIZE = $00100000;
  GENERIC_ALL = $10000000;
  GENERIC_EXECUTE = $20000000;
  GENERIC_WRITE = $40000000;
  GENERIC_READ = $80000000;

  FILE_SHARE_READ = $00000001;
  FILE_SHARE_WRITE = $00000002;
  FILE_SHARE_DELETE = $00000004;

  CREATE_NEW = $00000001;
  CREATE_ALWAYS = $00000002;
  OPEN_EXISTING = $00000003;
  OPEN_ALWAYS = $00000004;
  TRUNCATE_EXISTING = $00000005;

  FILE_FLAG_OPEN_NO_RECALL = $00100000;
  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;
  FILE_FLAG_POSIX_SEMANTICS = $01000000;
  FILE_FLAG_BACKUP_SEMANTICS = $02000000;
  FILE_FLAG_DELETE_ON_CLOSE = $04000000;
  FILE_FLAG_SEQUENTIAL_SCAN = $08000000;
  FILE_FLAG_RANDOM_ACCESS = $10000000;
  FILE_FLAG_NO_BUFFERING = $20000000;
  FILE_FLAG_OVERLAPPED = $40000000;
  FILE_FLAG_WRITE_THROUGH = $80000000;
  FILE_ATTRIBUTE_READONLY = $00000001;
  FILE_ATTRIBUTE_HIDDEN = $00000002;
  FILE_ATTRIBUTE_SYSTEM = $00000004;
  FILE_ATTRIBUTE_DIRECTORY = $00000010;
  FILE_ATTRIBUTE_ARCHIVE = $00000020;
  FILE_ATTRIBUTE_DEVICE = $00000040;
  FILE_ATTRIBUTE_NORMAL = $00000080;
  FILE_ATTRIBUTE_TEMPORARY = $00000100;
  FILE_ATTRIBUTE_SPARSE_FILE = $00000200;
  FILE_ATTRIBUTE_REPARSE_POINT = $00000400;
  FILE_ATTRIBUTE_COMPRESSED = $00000800;
  FILE_ATTRIBUTE_OFFLINE = $00001000;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
  FILE_ATTRIBUTE_ENCRYPTED = $00004000;
  FILE_ATTRIBUTE_VALID_FLAGS = $00007FB7;
  FILE_ATTRIBUTE_VALID_SET_FLAGS = $000031A7;

  FILE_BEGIN = $00000000;
  FILE_CURRENT = $00000001;
  FILE_END = $00000002;

  FILE_LIST_DIRECTORY = $00000001;
  FILE_DIRECTORY_FILE = $00000001;
  FILE_WRITE_THROUGH = $00000002;
  FILE_SEQUENTIAL_ONLY = $00000004;
  FILE_NO_INTERMEDIATE_BUFFERING = $00000008;
  FILE_SYNCHRONOUS_IO_ALERT = $00000010;
  FILE_SYNCHRONOUS_IO_NONALERT = $00000020;
  FILE_NON_DIRECTORY_FILE = $00000040;
  FILE_CREATE_TREE_CONNECTION = $00000080;
  FILE_COMPLETE_IF_OPLOCKED = $00000100;
  FILE_NO_EA_KNOWLEDGE = $00000200;
  FILE_OPEN_FOR_RECOVERY = $00000400;
  FILE_RANDOM_ACCESS = $00000800;
  FILE_DELETE_ON_CLOSE = $00001000;
  FILE_OPEN_BY_FILE_ID = $00002000;
  FILE_OPEN_FOR_BACKUP_INTENT = $00004000;
  FILE_NO_COMPRESSION = $00008000;
  FILE_RESERVE_OPFILTER = $00100000;
  FILE_OPEN_REPARSE_POINT = $00200000;
  FILE_OPEN_NO_RECALL = $00400000;
  FILE_OPEN_FOR_FREE_SPACE_QUERY = $00800000;
  FILE_COPY_STRUCTURED_STORAGE = $00000041;
  FILE_STRUCTURED_STORAGE = $00000441;
  FILE_VALID_OPTION_FLAGS = $00FFFFFF;
  FILE_VALID_PIPE_OPTION_FLAGS = $00000032;
  FILE_VALID_MAILSLOT_OPTION_FLAGS = $00000032;
  FILE_VALID_SET_FLAGS = $00000036;

  FILE_SUPERCEDE = $00000000;
  FILE_OPEN = $00000001;
  FILE_CREATE = $00000002;
  FILE_OPEN_IF = $00000003;
  FILE_OVERWRITE = $00000004;
  FILE_OVERWRITE_IF = $00000005;
  FILE_MAXIMUM_DISPOSITION = $00000005;
  FILE_SUPERCEDED = $00000000;

  FILE_OPENED = $00000001;
  FILE_CREATED = $00000002;
  FILE_OVERWRITTEN = $00000003;
  FILE_EXISTS = $00000004;
  FILE_DOES_NOT_EXIST = $00000005;

type
  PKSTART_ROUTINE = PTHREAD_START_ROUTINE; // ?correct?

  Unknown = INT_PTR; // generic 32 bit argument type
  dtU32 = Unknown;
  dtObjectAttributes = Unknown;
  dtACCESS_MASK = Unknown;
  dtCreateDisposition = Unknown;
  dtCreateOptions = Unknown;
  dtBLOB = Unknown;
  dtFILE_INFORMATION_CLASSEnum = Unknown;
  dtANSI_STRING = AnsiString; // ??

// The following API names are derived from Pedro's APILogger V2
// See http://forums.xbox-scene.com/index.php?showtopic=456303

function xboxkrnl_UnknownAPI000(): NTSTATUS; stdcall;
function xboxkrnl_AvGetSavedDataAddress(): NTSTATUS; stdcall;
function xboxkrnl_AvSendTVEncoderOption(): NTSTATUS; stdcall;
function xboxkrnl_AvSetDisplayMode(): NTSTATUS; stdcall;
function xboxkrnl_AvSetSavedDataAddress(): NTSTATUS; stdcall;
function xboxkrnl_DbgBreakPoint(): NTSTATUS; stdcall;
function xboxkrnl_DbgBreakPointWithStatus(): NTSTATUS; stdcall;
function xboxkrnl_DbgLoadImageSymbols(): NTSTATUS; stdcall;
function xboxkrnl_DbgPrint(): NTSTATUS; stdcall;
function xboxkrnl_HalReadSMCTrayState(): NTSTATUS; stdcall;
function xboxkrnl_DbgPrompt(): NTSTATUS; stdcall;
function xboxkrnl_DbgUnLoadImageSymbols(): NTSTATUS; stdcall;
function xboxkrnl_ExAcquireReadWriteLockExclusive(): NTSTATUS; stdcall;
function xboxkrnl_ExAcquireReadWriteLockShared(): NTSTATUS; stdcall;
function xboxkrnl_ExAllocatePool(NumberOfBytes: ULONG): NTSTATUS; stdcall;
function xboxkrnl_ExAllocatePoolWithTag(): NTSTATUS; stdcall;
function xboxkrnl_ExEventObjectType(): NTSTATUS; stdcall;
function xboxkrnl_ExFreePool(): NTSTATUS; stdcall;
function xboxkrnl_ExInitializeReadWriteLock(): NTSTATUS; stdcall;
function xboxkrnl_ExInterlockedAddLargeInteger(): NTSTATUS; stdcall;
function xboxkrnl_ExInterlockedAddLargeStatistic(): NTSTATUS; stdcall;
function xboxkrnl_ExInterlockedCompareExchange64(): NTSTATUS; stdcall;
function xboxkrnl_ExMutantObjectType(): NTSTATUS; stdcall;
function xboxkrnl_ExQueryPoolBlockSize(): NTSTATUS; stdcall;
function xboxkrnl_ExQueryNonVolatileSetting(
  ValueIndex: DWORD;
  &Type: PDWORD; // out
  Value: PUCHAR; // out
  ValueLength: SIZE_T;
  ResultLength: PSIZE_T // out, OPTIONAL
  ): NTSTATUS; stdcall;
function xboxkrnl_ExReadWriteRefurbInfo(): NTSTATUS; stdcall;
function xboxkrnl_ExRaiseException(): NTSTATUS; stdcall;
function xboxkrnl_ExRaiseStatus(): NTSTATUS; stdcall;
function xboxkrnl_ExReleaseReadWriteLock(): NTSTATUS; stdcall;
function xboxkrnl_ExSaveNonVolatileSetting(): NTSTATUS; stdcall;
function xboxkrnl_ExSemaphoreObjectType(): NTSTATUS; stdcall;
function xboxkrnl_ExTimerObjectType(): NTSTATUS; stdcall;
function xboxkrnl_ExfInterlockedInsertHeadList(): NTSTATUS; stdcall;
function xboxkrnl_ExfInterlockedInsertTailList(): NTSTATUS; stdcall;
function xboxkrnl_ExfInterlockedRemoveHeadList(): NTSTATUS; stdcall;
function xboxkrnl_FscGetCacheSize(): NTSTATUS; stdcall;
function xboxkrnl_FscInvalidateIdleBlocks(): NTSTATUS; stdcall;
function xboxkrnl_FscSetCacheSize(uCachePages: ULONG): NTSTATUS; stdcall;
function xboxkrnl_HalClearSoftwareInterrupt(): NTSTATUS; stdcall;
function xboxkrnl_HalDisableSystemInterrupt(): NTSTATUS; stdcall;
function xboxkrnl_HalDiskCachePartitionCount(): NTSTATUS; stdcall;
function xboxkrnl_HalDiskModelNumber(): NTSTATUS; stdcall;
function xboxkrnl_HalDiskSerialNumber(): NTSTATUS; stdcall;
function xboxkrnl_HalEnableSystemInterrupt(): NTSTATUS; stdcall;
function xboxkrnl_HalGetInterruptVector(): NTSTATUS; stdcall;
function xboxkrnl_HalReadSMBusValue(): NTSTATUS; stdcall;
function xboxkrnl_HalReadWritePCISpace(): NTSTATUS; stdcall;
function xboxkrnl_HalRegisterShutdownNotification(): NTSTATUS; stdcall;
function xboxkrnl_HalRequestSoftwareInterrupt(): NTSTATUS; stdcall;
function xboxkrnl_HalReturnToFirmware(Routine: RETURN_FIRMWARE): NTSTATUS; stdcall;
function xboxkrnl_HalWriteSMBusValue(): NTSTATUS; stdcall;
function xboxkrnl_InterlockedCompareExchange(): NTSTATUS; stdcall;
function xboxkrnl_InterlockedDecrement(): NTSTATUS; stdcall;
function xboxkrnl_InterlockedIncrement(): NTSTATUS; stdcall;
function xboxkrnl_InterlockedExchange(): NTSTATUS; stdcall;
function xboxkrnl_InterlockedExchangeAdd(): NTSTATUS; stdcall;
function xboxkrnl_InterlockedFlushSList(): NTSTATUS; stdcall;
function xboxkrnl_InterlockedPopEntrySList(): NTSTATUS; stdcall;
function xboxkrnl_InterlockedPushEntrySList(): NTSTATUS; stdcall;
function xboxkrnl_IoAllocateIrp(): NTSTATUS; stdcall;
function xboxkrnl_IoBuildAsynchronousFsdRequest(): NTSTATUS; stdcall;
function xboxkrnl_IoBuildDeviceIoControlRequest(): NTSTATUS; stdcall;
function xboxkrnl_IoBuildSynchronousFsdRequest(): NTSTATUS; stdcall;
function xboxkrnl_IoCheckShareAccess(): NTSTATUS; stdcall;
function xboxkrnl_IoCompletionObjectType(): NTSTATUS; stdcall;
function xboxkrnl_IoCreateDevice(): NTSTATUS; stdcall;
function xboxkrnl_IoCreateFile(
  FileHandle: PHANDLE; // out
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // out
  AllocationSize: PLARGE_INTEGER;
  FileAttributes: ULONG;
  ShareAccess: ULONG;
  Disposition: ULONG;
  CreateOptions: ULONG;
  Options: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_IoCreateSymbolicLink(
  SymbolicLinkName: PSTRING;
  DeviceName: PSTRING
  ): NTSTATUS; stdcall;
function xboxkrnl_IoDeleteDevice(): NTSTATUS; stdcall;
function xboxkrnl_IoDeleteSymbolicLink(
  SymbolicLinkName: PSTRING
  ): NTSTATUS; stdcall;
function xboxkrnl_IoDeviceObjectType(): NTSTATUS; stdcall;
function xboxkrnl_IoFileObjectType(): NTSTATUS; stdcall;
function xboxkrnl_IoFreeIrp(): NTSTATUS; stdcall;
function xboxkrnl_IoInitializeIrp(): NTSTATUS; stdcall;
function xboxkrnl_IoInvalidDeviceRequest(): NTSTATUS; stdcall;
function xboxkrnl_IoQueryFileInformation(): NTSTATUS; stdcall;
function xboxkrnl_IoQueryVolumeInformation(): NTSTATUS; stdcall;
function xboxkrnl_IoQueueThreadIrp(): NTSTATUS; stdcall;
function xboxkrnl_IoRemoveShareAccess(): NTSTATUS; stdcall;
function xboxkrnl_IoSetIoCompletion(): NTSTATUS; stdcall;
function xboxkrnl_IoSetShareAccess(): NTSTATUS; stdcall;
function xboxkrnl_IoStartNextPacket(): NTSTATUS; stdcall;
function xboxkrnl_IoStartNextPacketByKey(): NTSTATUS; stdcall;
function xboxkrnl_IoStartPacket(): NTSTATUS; stdcall;
function xboxkrnl_IoSynchronousDeviceIoControlRequest(): NTSTATUS; stdcall;
function xboxkrnl_IoSynchronousFsdRequest(): NTSTATUS; stdcall;
function xboxkrnl_IofCallDriver(): NTSTATUS; stdcall;
function xboxkrnl_IofCompleteRequest(): NTSTATUS; stdcall;
function xboxkrnl_KdDebuggerEnabled(): NTSTATUS; stdcall;
function xboxkrnl_KdDebuggerNotPresent(): NTSTATUS; stdcall;
function xboxkrnl_IoDismountVolume(): NTSTATUS; stdcall;
function xboxkrnl_IoDismountVolumeByName(): NTSTATUS; stdcall;
function xboxkrnl_KeAlertResumeThread(): NTSTATUS; stdcall;
function xboxkrnl_KeAlertThread(): NTSTATUS; stdcall;
function xboxkrnl_KeBoostPriorityThread(): NTSTATUS; stdcall;
function xboxkrnl_KeBugCheck(): NTSTATUS; stdcall;
function xboxkrnl_KeBugCheckEx(): NTSTATUS; stdcall;
function xboxkrnl_KeCancelTimer(): NTSTATUS; stdcall;
function xboxkrnl_KeConnectInterrupt(): NTSTATUS; stdcall;
function xboxkrnl_KeDelayExecutionThread(
  WaitMode: KPROCESSOR_MODE;
  Alertable: BOOLEAN;
  Interval: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_KeDisconnectInterrupt(): NTSTATUS; stdcall;
function xboxkrnl_KeEnterCriticalRegion(): NTSTATUS; stdcall;
function xboxkrnl_MmGlobalData(): NTSTATUS; stdcall;
function xboxkrnl_KeGetCurrentIrql(): NTSTATUS; stdcall;
function xboxkrnl_KeGetCurrentThread(): NTSTATUS; stdcall;
function xboxkrnl_KeInitializeApc(): NTSTATUS; stdcall;
function xboxkrnl_KeInitializeDeviceQueue(): NTSTATUS; stdcall;
function xboxkrnl_KeInitializeDpc(
  Dpc: PKDPC;
  DeferredRoutine: PKDEFERRED_ROUTINE;
  DeferredContext: PVOID
  ): NTSTATUS; stdcall;
function xboxkrnl_KeInitializeEvent(): NTSTATUS; stdcall;
function xboxkrnl_KeInitializeInterrupt(): NTSTATUS; stdcall;
function xboxkrnl_KeInitializeMutant(): NTSTATUS; stdcall;
function xboxkrnl_KeInitializeQueue(): NTSTATUS; stdcall;
function xboxkrnl_KeInitializeSemaphore(): NTSTATUS; stdcall;
function xboxkrnl_KeInitializeTimerEx(
  Timer: PKTIMER;
  &Type: TIMER_TYPE
  ): NTSTATUS; stdcall;
function xboxkrnl_KeInsertByKeyDeviceQueue(): NTSTATUS; stdcall;
function xboxkrnl_KeInsertDeviceQueue(): NTSTATUS; stdcall;
function xboxkrnl_KeInsertHeadQueue(): NTSTATUS; stdcall;
function xboxkrnl_KeInsertQueue(): NTSTATUS; stdcall;
function xboxkrnl_KeInsertQueueApc(): NTSTATUS; stdcall;
function xboxkrnl_KeInsertQueueDpc(): NTSTATUS; stdcall;
function xboxkrnl_KeInterruptTime(): NTSTATUS; stdcall;
function xboxkrnl_KeIsExecutingDpc(): NTSTATUS; stdcall;
function xboxkrnl_KeLeaveCriticalRegion(): NTSTATUS; stdcall;
function xboxkrnl_KePulseEvent(): NTSTATUS; stdcall;
function xboxkrnl_KeQueryBasePriorityThread(): NTSTATUS; stdcall;
function xboxkrnl_KeQueryInterruptTime(): NTSTATUS; stdcall;
function xboxkrnl_KeQueryPerformanceCounter(): NTSTATUS; stdcall;
function xboxkrnl_KeQueryPerformanceFrequency(): NTSTATUS; stdcall;
procedure xboxkrnl_KeQuerySystemTime(
  CurrentTime: PLARGE_INTEGER
  ); stdcall;
function xboxkrnl_KeRaiseIrqlToDpcLevel(): NTSTATUS; stdcall;
function xboxkrnl_KeRaiseIrqlToSynchLevel(): NTSTATUS; stdcall;
function xboxkrnl_KeReleaseMutant(): NTSTATUS; stdcall;
function xboxkrnl_KeReleaseSemaphore(): NTSTATUS; stdcall;
function xboxkrnl_KeRemoveByKeyDeviceQueue(): NTSTATUS; stdcall;
function xboxkrnl_KeRemoveDeviceQueue(): NTSTATUS; stdcall;
function xboxkrnl_KeRemoveEntryDeviceQueue(): NTSTATUS; stdcall;
function xboxkrnl_KeRemoveQueue(): NTSTATUS; stdcall;
function xboxkrnl_KeRemoveQueueDpc(): NTSTATUS; stdcall;
function xboxkrnl_KeResetEvent(): NTSTATUS; stdcall;
function xboxkrnl_KeRestoreFloatingPointState(): NTSTATUS; stdcall;
function xboxkrnl_KeResumeThread(): NTSTATUS; stdcall;
function xboxkrnl_KeRundownQueue(): NTSTATUS; stdcall;
function xboxkrnl_KeSaveFloatingPointState(): NTSTATUS; stdcall;
function xboxkrnl_KeSetBasePriorityThread(): NTSTATUS; stdcall;
function xboxkrnl_KeSetDisableBoostThread(): NTSTATUS; stdcall;
function xboxkrnl_KeSetEvent(): NTSTATUS; stdcall;
function xboxkrnl_KeSetEventBoostPriority(): NTSTATUS; stdcall;
function xboxkrnl_KeSetPriorityProcess(): NTSTATUS; stdcall;
function xboxkrnl_KeSetPriorityThread(): NTSTATUS; stdcall;
function xboxkrnl_KeSetTimer(
  Timer: PKTIMER;
  DueTime: LARGE_INTEGER;
  Dpc: PKDPC // OPTIONAL
  ): BOOLEAN; stdcall;
function xboxkrnl_KeSetTimerEx(): NTSTATUS; stdcall;
function xboxkrnl_KeStallExecutionProcessor(): NTSTATUS; stdcall;
function xboxkrnl_KeSuspendThread(): NTSTATUS; stdcall;
function xboxkrnl_KeSynchronizeExecution(): NTSTATUS; stdcall;
function xboxkrnl_KeSystemTime(): NTSTATUS; stdcall;
function xboxkrnl_KeTestAlertThread(): NTSTATUS; stdcall;
function xboxkrnl_KeTickCount(): NTSTATUS; stdcall;
function xboxkrnl_KeTimeIncrement(): NTSTATUS; stdcall;
function xboxkrnl_KeWaitForMultipleObjects(): NTSTATUS; stdcall;
function xboxkrnl_KeWaitForSingleObject(): NTSTATUS; stdcall;
function xboxkrnl_KfRaiseIrql(): NTSTATUS; stdcall;
function xboxkrnl_KfLowerIrql(): NTSTATUS; stdcall;
function xboxkrnl_KiBugCheckData(): NTSTATUS; stdcall;
function xboxkrnl_KiUnlockDispatcherDatabase(): NTSTATUS; stdcall;
function xboxkrnl_LaunchDataPage(): NTSTATUS; stdcall;
function xboxkrnl_MmAllocateContiguousMemory(
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
function xboxkrnl_MmAllocateContiguousMemoryEx(): NTSTATUS; stdcall;
function xboxkrnl_MmAllocateSystemMemory(): NTSTATUS; stdcall;
function xboxkrnl_MmClaimGpuInstanceMemory(): NTSTATUS; stdcall;
function xboxkrnl_MmCreateKernelStack(): NTSTATUS; stdcall;
function xboxkrnl_MmDeleteKernelStack(): NTSTATUS; stdcall;
function xboxkrnl_MmFreeContiguousMemory(): NTSTATUS; stdcall;
function xboxkrnl_MmFreeSystemMemory(): NTSTATUS; stdcall;
function xboxkrnl_MmGetPhysicalAddress(): NTSTATUS; stdcall;
function xboxkrnl_MmIsAddressValid(): NTSTATUS; stdcall;
function xboxkrnl_MmLockUnlockBufferPages(): NTSTATUS; stdcall;
function xboxkrnl_MmLockUnlockPhysicalPage(): NTSTATUS; stdcall;
function xboxkrnl_MmMapIoSpace(): NTSTATUS; stdcall;
function xboxkrnl_MmPersistContiguousMemory(): NTSTATUS; stdcall;
function xboxkrnl_MmQueryAddressProtect(): NTSTATUS; stdcall;
function xboxkrnl_MmQueryAllocationSize(): NTSTATUS; stdcall;
function xboxkrnl_MmQueryStatistics(): NTSTATUS; stdcall;
function xboxkrnl_MmSetAddressProtect(): NTSTATUS; stdcall;
function xboxkrnl_MmUnmapIoSpace(): NTSTATUS; stdcall;
function xboxkrnl_NtAllocateVirtualMemory(): NTSTATUS; stdcall;
function xboxkrnl_NtCancelTimer(): NTSTATUS; stdcall;
function xboxkrnl_NtClearEvent(): NTSTATUS; stdcall;
function xboxkrnl_NtClose(Handle: THandle): NTSTATUS; stdcall; {EXPORTNUM(187)}
function xboxkrnl_NtCreateDirectoryObject(): NTSTATUS; stdcall;
function xboxkrnl_NtCreateEvent(pFileHandle: dtU32; pszUnknownArgs1: dtU32; pszUnknownArgs2: dtU32; pszUnknownArgs3: dtU32): NTSTATUS; stdcall;
function xboxkrnl_NtCreateFile(pFileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pIoStatusBlock: dtU32; AllocationSize: dtU32; FileAttributes: dtU32; ShareAccess: dtACCESS_MASK; CreateDisposition: dtCreateDisposition; CreateOptions: dtCreateOptions): NTSTATUS; stdcall;
function xboxkrnl_NtCreateIoCompletion(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
function xboxkrnl_NtCreateMutant(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
function xboxkrnl_NtCreateSemaphore(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
function xboxkrnl_NtCreateTimer(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
function xboxkrnl_NtDeleteFile(pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
function xboxkrnl_NtDeviceIoControlFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; pIoControlCode: dtU32; pInputBuffer: dtU32; InputBufferLength: dtU32; pOutputBuffer: dtU32; OutputBufferLength: dtU32): NTSTATUS; stdcall;
function xboxkrnl_NtDuplicateObject(): NTSTATUS; stdcall;
function xboxkrnl_NtFlushBuffersFile(): NTSTATUS; stdcall;
function xboxkrnl_NtFreeVirtualMemory(): NTSTATUS; stdcall;
function xboxkrnl_NtFsControlFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; FsControlCode: dtU32; pInputBuffer: dtU32; InputBufferLength: dtU32; pOutputBuffer: dtU32; OutputBufferLength: dtU32): NTSTATUS; stdcall;
function xboxkrnl_NtOpenDirectoryObject(): NTSTATUS; stdcall;
function xboxkrnl_NtOpenFile(pFileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pIoStatusBlock: dtU32; ShareAccess: dtACCESS_MASK; OpenOptions: dtCreateOptions): NTSTATUS; stdcall;
function xboxkrnl_NtOpenSymbolicLinkObject(pFileHandle: dtU32; pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
function xboxkrnl_NtProtectVirtualMemory(): NTSTATUS; stdcall;
function xboxkrnl_NtPulseEvent(): NTSTATUS; stdcall;
function xboxkrnl_NtQueueApcThread(): NTSTATUS; stdcall;
function xboxkrnl_NtQueryDirectoryFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; pFileInformation: dtU32; Length: dtU32; FileInformationClass: dtFILE_INFORMATION_CLASSEnum; pFileMask: dtANSI_STRING; RestartScan: dtU32): NTSTATUS; stdcall;
function xboxkrnl_NtQueryDirectoryObject(): NTSTATUS; stdcall;
function xboxkrnl_NtQueryEvent(): NTSTATUS; stdcall;
function xboxkrnl_NtQueryFullAttributesFile(pObjectAttributes: dtObjectAttributes; pAttributes: dtU32): NTSTATUS; stdcall;
function xboxkrnl_NtQueryInformationFile(FileHandle: dtU32; pIoStatusBlock: dtU32; pFileInformation: dtU32; Length: dtU32; FileInformationClass: dtFILE_INFORMATION_CLASSEnum): NTSTATUS; stdcall;
function xboxkrnl_NtQueryIoCompletion(): NTSTATUS; stdcall;
function xboxkrnl_NtQueryMutant(): NTSTATUS; stdcall;
function xboxkrnl_NtQuerySemaphore(): NTSTATUS; stdcall;
function xboxkrnl_NtQuerySymbolicLinkObject(): NTSTATUS; stdcall;
function xboxkrnl_NtQueryTimer(): NTSTATUS; stdcall;
function xboxkrnl_NtQueryVirtualMemory(): NTSTATUS; stdcall;
function xboxkrnl_NtQueryVolumeInformationFile(FileHandle: dtU32; pIoStatusBlock: dtU32; pFileSystemInformation: dtU32; Length: dtU32; FileInformationClass: dtFILE_INFORMATION_CLASSEnum): NTSTATUS; stdcall;
function xboxkrnl_NtReadFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; pBuffer: dtU32; Length: dtU32; pByteOffset: dtU32): NTSTATUS; stdcall;
function xboxkrnl_NtReadFileScatter(): NTSTATUS; stdcall;
function xboxkrnl_NtReleaseMutant(): NTSTATUS; stdcall;
function xboxkrnl_NtReleaseSemaphore(): NTSTATUS; stdcall;
function xboxkrnl_NtRemoveIoCompletion(): NTSTATUS; stdcall;
function xboxkrnl_NtResumeThread(): NTSTATUS; stdcall;
function xboxkrnl_NtSetEvent(): NTSTATUS; stdcall;
function xboxkrnl_NtSetInformationFile(FileHandle: dtU32; pIoStatusBlock: dtU32; FileInformation: dtU32; Length: dtU32; FileInformationClass: dtFILE_INFORMATION_CLASSEnum): NTSTATUS; stdcall;
function xboxkrnl_NtSetIoCompletion(): NTSTATUS; stdcall;
function xboxkrnl_NtSetSystemTime(): NTSTATUS; stdcall;
function xboxkrnl_NtSetTimerEx(): NTSTATUS; stdcall;
function xboxkrnl_NtSignalAndWaitForSingleObjectEx(): NTSTATUS; stdcall;
function xboxkrnl_NtSuspendThread(): NTSTATUS; stdcall;
function xboxkrnl_NtUserIoApcDispatcher(): NTSTATUS; stdcall;
function xboxkrnl_NtWaitForSingleObject(): NTSTATUS; stdcall;
function xboxkrnl_NtWaitForSingleObjectEx(): NTSTATUS; stdcall;
function xboxkrnl_NtWaitForMultipleObjectsEx(): NTSTATUS; stdcall;
function xboxkrnl_NtWriteFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; pBuffer: dtU32; Length: dtU32; pByteOffset: dtU32): NTSTATUS; stdcall;
function xboxkrnl_NtWriteFileGather(): NTSTATUS; stdcall;
procedure xboxkrnl_NtYieldExecution(); stdcall;
function xboxkrnl_ObCreateObject(): NTSTATUS; stdcall;
function xboxkrnl_ObDirectoryObjectType(): NTSTATUS; stdcall;
function xboxkrnl_ObInsertObject(): NTSTATUS; stdcall;
function xboxkrnl_ObMakeTemporaryObject(): NTSTATUS; stdcall;
function xboxkrnl_ObOpenObjectByName(): NTSTATUS; stdcall;
function xboxkrnl_ObOpenObjectByPointer(): NTSTATUS; stdcall;
function xboxkrnl_ObpObjectHandleTable(): NTSTATUS; stdcall;
function xboxkrnl_ObReferenceObjectByHandle(): NTSTATUS; stdcall;
function xboxkrnl_ObReferenceObjectByName(): NTSTATUS; stdcall;
function xboxkrnl_ObReferenceObjectByPointer(): NTSTATUS; stdcall;
function xboxkrnl_ObSymbolicLinkObjectType(): NTSTATUS; stdcall;
function xboxkrnl_ObfDereferenceObject(): NTSTATUS; stdcall;
function xboxkrnl_ObfReferenceObject(): NTSTATUS; stdcall;
function xboxkrnl_PhyGetLinkState(): NTSTATUS; stdcall;
function xboxkrnl_PhyInitialize(): NTSTATUS; stdcall;
function xboxkrnl_PsCreateSystemThread(
  lpThreadAttributes: PULONG;              // SD
  dwStackSize: DWORD;                      // initial stack size
  lpStartAddress: PKSTART_ROUTINE;         // thread function
  lpParameter: PVOID;                      // thread argument
  dwCreationFlags: DWORD;                  // creation option
  lpThreadId: PULONG                       // thread identifier
): NTSTATUS; stdcall;
function xboxkrnl_PsCreateSystemThreadEx(
  ThreadHandle: PHANDLE; // out
  ThreadExtraSize: ULONG;
  KernelStackSize: ULONG;
  TlsDataSize: ULONG;
  ThreadId: PULONG; // out, optional
  StartContext1: PVOID;
  StartContext2: PVOID;
  CreateSuspended: LONGBOOL;
  DebugStack: LONGBOOL;
  StartRoutine: PKSTART_ROUTINE): NTSTATUS; stdcall; {EXPORTNUM(255)}
function xboxkrnl_PsQueryStatistics(): NTSTATUS; stdcall;
function xboxkrnl_PsSetCreateThreadNotifyRoutine(): NTSTATUS; stdcall;
function xboxkrnl_PsTerminateSystemThread(): NTSTATUS; stdcall;
function xboxkrnl_PsThreadObjectType(): NTSTATUS; stdcall;
function xboxkrnl_RtlAnsiStringToUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendStringToString(): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendUnicodeStringToString(): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendUnicodeToString(): NTSTATUS; stdcall;
function xboxkrnl_RtlAssert(): NTSTATUS; stdcall;
function xboxkrnl_RtlCaptureContext(): NTSTATUS; stdcall;
function xboxkrnl_RtlCaptureStackBackTrace(): NTSTATUS; stdcall;
function xboxkrnl_RtlCharToInteger(): NTSTATUS; stdcall;
function xboxkrnl_RtlCompareMemory(): NTSTATUS; stdcall;
function xboxkrnl_RtlCompareMemoryUlong(): NTSTATUS; stdcall;
function xboxkrnl_RtlCompareString(): NTSTATUS; stdcall;
function xboxkrnl_RtlCompareUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlCopyString(): NTSTATUS; stdcall;
function xboxkrnl_RtlCopyUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlCreateUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlDowncaseUnicodeChar(): NTSTATUS; stdcall;
function xboxkrnl_RtlDowncaseUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlEnterCriticalSection(): NTSTATUS; stdcall;
function xboxkrnl_RtlEnterCriticalSectionAndRegion(): NTSTATUS; stdcall;
function xboxkrnl_RtlEqualString(): NTSTATUS; stdcall;
function xboxkrnl_RtlEqualUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlExtendedIntegerMultiply(): NTSTATUS; stdcall;
function xboxkrnl_RtlExtendedLargeIntegerDivide(): NTSTATUS; stdcall;
function xboxkrnl_RtlExtendedMagicDivide(): NTSTATUS; stdcall;
function xboxkrnl_RtlFillMemory(): NTSTATUS; stdcall;
function xboxkrnl_RtlFillMemoryUlong(): NTSTATUS; stdcall;
function xboxkrnl_RtlFreeAnsiString(): NTSTATUS; stdcall;
function xboxkrnl_RtlFreeUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlGetCallersAddress(): NTSTATUS; stdcall;
function xboxkrnl_RtlInitAnsiString(): NTSTATUS; stdcall;
function xboxkrnl_RtlInitUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlInitializeCriticalSection(): NTSTATUS; stdcall;
function xboxkrnl_RtlIntegerToChar(): NTSTATUS; stdcall;
function xboxkrnl_RtlIntegerToUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlLeaveCriticalSection(): NTSTATUS; stdcall;
function xboxkrnl_RtlLeaveCriticalSectionAndRegion(): NTSTATUS; stdcall;
function xboxkrnl_RtlLowerChar(): NTSTATUS; stdcall;
function xboxkrnl_RtlMapGenericMask(): NTSTATUS; stdcall;
function xboxkrnl_RtlMoveMemory(): NTSTATUS; stdcall;
function xboxkrnl_RtlMultiByteToUnicodeN(): NTSTATUS; stdcall;
function xboxkrnl_RtlMultiByteToUnicodeSize(): NTSTATUS; stdcall;
function xboxkrnl_RtlNtStatusToDosError(): NTSTATUS; stdcall;
function xboxkrnl_RtlRaiseException(): NTSTATUS; stdcall;
function xboxkrnl_RtlRaiseStatus(): NTSTATUS; stdcall;
function xboxkrnl_RtlTimeFieldsToTime(): NTSTATUS; stdcall;
function xboxkrnl_RtlTimeToTimeFields(): NTSTATUS; stdcall;
function xboxkrnl_RtlTryEnterCriticalSection(): NTSTATUS; stdcall;
function xboxkrnl_RtlUlongByteSwap(): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeStringToAnsiString(): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeStringToInteger(): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeToMultiByteN(): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeToMultiByteSize(): NTSTATUS; stdcall;
function xboxkrnl_RtlUnwind(): NTSTATUS; stdcall;
function xboxkrnl_RtlUpcaseUnicodeChar(): NTSTATUS; stdcall;
function xboxkrnl_RtlUpcaseUnicodeString(): NTSTATUS; stdcall;
function xboxkrnl_RtlUpcaseUnicodeToMultiByteN(): NTSTATUS; stdcall;
function xboxkrnl_RtlUpperChar(): NTSTATUS; stdcall;
function xboxkrnl_RtlUpperString(): NTSTATUS; stdcall;
function xboxkrnl_RtlUshortByteSwap(): NTSTATUS; stdcall;
function xboxkrnl_RtlWalkFrameChain(): NTSTATUS; stdcall;
function xboxkrnl_RtlZeroMemory(): NTSTATUS; stdcall;
function xboxkrnl_XboxEEPROMKey(): NTSTATUS; stdcall;
function xboxkrnl_XboxHardwareInfo(): NTSTATUS; stdcall;
function xboxkrnl_XboxHDKey(): NTSTATUS; stdcall;
function xboxkrnl_XboxKrnlVersion(): NTSTATUS; stdcall;
function xboxkrnl_XboxSignatureKey(): NTSTATUS; stdcall;
function xboxkrnl_XeImageFileName(): NTSTATUS; stdcall;
function xboxkrnl_XeLoadSection(): NTSTATUS; stdcall;
function xboxkrnl_XeUnloadSection(): NTSTATUS; stdcall;
function xboxkrnl_READ_PORT_BUFFER_UCHAR(): NTSTATUS; stdcall;
function xboxkrnl_READ_PORT_BUFFER_USHORT(): NTSTATUS; stdcall;
function xboxkrnl_READ_PORT_BUFFER_ULONG(): NTSTATUS; stdcall;
function xboxkrnl_WRITE_PORT_BUFFER_UCHAR(): NTSTATUS; stdcall;
function xboxkrnl_WRITE_PORT_BUFFER_USHORT(): NTSTATUS; stdcall;
function xboxkrnl_WRITE_PORT_BUFFER_ULONG(): NTSTATUS; stdcall;
function xboxkrnl_XcSHAInit(): NTSTATUS; stdcall;
function xboxkrnl_XcSHAUpdate(): NTSTATUS; stdcall;
function xboxkrnl_XcSHAFinal(): NTSTATUS; stdcall;
function xboxkrnl_XcRC4Key(): NTSTATUS; stdcall;
function xboxkrnl_XcRC4Crypt(): NTSTATUS; stdcall;
function xboxkrnl_XcHMAC(): NTSTATUS; stdcall;
function xboxkrnl_XcPKEncPublic(): NTSTATUS; stdcall;
function xboxkrnl_XcPKDecPrivate(): NTSTATUS; stdcall;
function xboxkrnl_XcPKGetKeyLen(): NTSTATUS; stdcall;
function xboxkrnl_XcVerifyPKCS1Signature(): NTSTATUS; stdcall;
function xboxkrnl_XcModExp(): NTSTATUS; stdcall;
function xboxkrnl_XcDESKeyParity(): NTSTATUS; stdcall;
function xboxkrnl_XcKeyTable(): NTSTATUS; stdcall;
function xboxkrnl_XcBlockCrypt(): NTSTATUS; stdcall;
function xboxkrnl_XcBlockCryptCBC(): NTSTATUS; stdcall;
function xboxkrnl_XcCryptService(): NTSTATUS; stdcall;
function xboxkrnl_XcUpdateCrypto(): NTSTATUS; stdcall;
function xboxkrnl_RtlRip(): NTSTATUS; stdcall;
function xboxkrnl_XboxLANKey(): NTSTATUS; stdcall;
function xboxkrnl_XboxAlternateSignatureKeys(): NTSTATUS; stdcall;
function xboxkrnl_XePublicKeyData(): NTSTATUS; stdcall;
function xboxkrnl_HalBootSMCVideoMode(): NTSTATUS; stdcall;
function xboxkrnl_IdexChannelObject(): NTSTATUS; stdcall;
function xboxkrnl_HalIsResetOrShutdownPending(): NTSTATUS; stdcall;
function xboxkrnl_IoMarkIrpMustComplete(): NTSTATUS; stdcall;
function xboxkrnl_HalInitiateShutdown(): NTSTATUS; stdcall;
function xboxkrnl_RtlSnprintf(): NTSTATUS; stdcall;
function xboxkrnl_RtlSprintf(): NTSTATUS; stdcall;
function xboxkrnl_RtlVsnprintf(): NTSTATUS; stdcall;
function xboxkrnl_RtlVsprintf(): NTSTATUS; stdcall;
function xboxkrnl_HalEnableSecureTrayEject(): NTSTATUS; stdcall;
function xboxkrnl_HalWriteSMCScratchRegister(): NTSTATUS; stdcall;
function xboxkrnl_UnknownAPI367(): NTSTATUS; stdcall;
function xboxkrnl_UnknownAPI368(): NTSTATUS; stdcall;
function xboxkrnl_UnknownAPI369(): NTSTATUS; stdcall;
function xboxkrnl_UnknownAPI370(): NTSTATUS; stdcall;
function xboxkrnl_UnknownAPI371(): NTSTATUS; stdcall;
function xboxkrnl_UnknownAPI372(): NTSTATUS; stdcall;
function xboxkrnl_UnknownAPI373(): NTSTATUS; stdcall;
function xboxkrnl_MmDbgAllocateMemory(): NTSTATUS; stdcall;
function xboxkrnl_MmDbgFreeMemory(): NTSTATUS; stdcall;
function xboxkrnl_MmDbgQueryAvailablePages(): NTSTATUS; stdcall;
function xboxkrnl_MmDbgReleaseAddress(): NTSTATUS; stdcall;
function xboxkrnl_MmDbgWriteCheck(): NTSTATUS; stdcall;

implementation

// Global Variable(s)
var
  g_pfnThreadNotification: PVOID = nil;

type
  // PsCreateSystemThread proxy parameters
  PPCSTProxyParam = ^PCSTProxyParam;
  PCSTProxyParam = packed record
    StartContext1: PVOID;
    StartContext2: PVOID;
    StartRoutine: PKSTART_ROUTINE;
    StartSuspended: BOOL;
    hStartedEvent: HANDLE;
  end;

// PsCreateSystemThread(Ex) proxy procedure
//pragma warning(push)
//pragma warning(disable: 4731)  // disable ebp modification warning
function {WINAPI} PCSTProxy(Parameter: PPCSTProxyParam): Integer;//Word;
label
  callComplete;
var
  StartContext1: PVOID;
  StartContext2: PVOID;
  StartRoutine: PKSTART_ROUTINE;
  StartSuspended: BOOL;
  pfnNotificationRoutine: XTHREAD_NOTIFY_PROC;
begin
  StartContext1 := Parameter.StartContext1;
  StartContext2 := Parameter.StartContext2;
  StartRoutine  := Parameter.StartRoutine;
  StartSuspended := Parameter.StartSuspended;

  DbgPrintf('EmuKrnl : PCSTProxy' +
    #13#10'(' +
    #13#10'   StartContext1       : 0x' + IntToHex(Integer(StartContext1), 8) +
    #13#10'   StartContext2       : 0x' + IntToHex(Integer(StartContext2), 8) +
    #13#10'   StartRoutine        : 0x' + IntToHex(Integer(Addr(StartRoutine)), 8) +
    #13#10');');

  if StartSuspended then
    SuspendThread(GetCurrentThread());

  EmuGenerateFS(CxbxKrnl_TLS, CxbxKrnl_TLSData);

  // call thread notification routine(s)
  if Assigned(g_pfnThreadNotification) then
  begin
    pfnNotificationRoutine := {XTL.}XTHREAD_NOTIFY_PROC(g_pfnThreadNotification);

    DbgPrintf('EmKrnl : Calling pfnNotificationRoutine (0x' + IntToHex(Integer(Addr(pfnNotificationRoutine)), 8) + ')');

    EmuSwapFS();   // Xbox FS

    pfnNotificationRoutine(True);

    EmuSwapFS();   // Win2k/XP FS
  end;

  // use the special calling convention
  try
    SetEvent(Parameter.hStartedEvent);

    EmuSwapFS();   // Xbox FS

    asm
        mov         esi, StartRoutine
        push        StartContext2
        push        StartContext1
        push        offset callComplete
        lea         ebp, [esp-4]
//        jmp near    esi
        jmp         esi
     end;
  except
    on E: Exception do
      DbgPrintf('EmuKrnl : PCSTProxy : Catched an exception : ' + E.Message);
//  __except(EmuException(GetExceptionInformation()))
//    EmuWarning('Problem with ExceptionFilter not ');
  end;

callComplete:

  EmuSwapFS();    // Win2k/XP FS

  // call thread notification routine(s)
  if Assigned(g_pfnThreadNotification) then
  begin
    pfnNotificationRoutine := {XTL.}XTHREAD_NOTIFY_PROC(g_pfnThreadNotification);

    EmuSwapFS();   // Xbox FS

    pfnNotificationRoutine(False);

    EmuSwapFS();   // Win2k/XP FS
  end;

  CxbxKrnlTerminateThread();

  Result := 0;
end;
//pragma warning(pop)

function Unimplemented(const aAPI: string): NTSTATUS;
begin
  WriteLog('Unimplemented xboxkrnl EmuAPI : ' + aAPI);
  Result := STATUS_PROCEDURE_NOT_FOUND; // abuse a standard NT error code
end;

/// ##########
/// ########## Start of Xbox Kernel API's :
/// ##########

function xboxkrnl_UnknownAPI000(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('UnknownAPI000');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_AvGetSavedDataAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('AvGetSavedDataAddress');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_AvSendTVEncoderOption(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('AvSendTVEncoderOption');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_AvSetDisplayMode(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('AvSetDisplayMode');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_AvSetSavedDataAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('AvSetSavedDataAddress');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_DbgBreakPoint(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('DbgBreakPoint');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_DbgBreakPointWithStatus(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('DbgBreakPointWithStatus');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_DbgLoadImageSymbols(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('DbgLoadImageSymbols');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_DbgPrint(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('DbgPrint');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalReadSMCTrayState(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalReadSMCTrayState');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_DbgPrompt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('DbgPrompt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_DbgUnLoadImageSymbols(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('DbgUnLoadImageSymbols');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExAcquireReadWriteLockExclusive(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAcquireReadWriteLockExclusive');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExAcquireReadWriteLockShared(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAcquireReadWriteLockShared');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExAllocatePool(NumberOfBytes: ULONG): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAllocatePool');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExAllocatePoolWithTag(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExAllocatePoolWithTag');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExEventObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExEventObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExFreePool(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExFreePool');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExInitializeReadWriteLock(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExInitializeReadWriteLock');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExInterlockedAddLargeInteger(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExInterlockedAddLargeInteger');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExInterlockedAddLargeStatistic(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExInterlockedAddLargeStatistic');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExInterlockedCompareExchange64(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExInterlockedCompareExchange64');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExMutantObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExMutantObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExQueryPoolBlockSize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExQueryPoolBlockSize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExQueryNonVolatileSetting(
  ValueIndex: DWORD;
  &Type: PDWORD; // out
  Value: PUCHAR; // out
  ValueLength: SIZE_T;
  ResultLength: PSIZE_T // out, OPTIONAL
): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExQueryNonVolatileSetting');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExReadWriteRefurbInfo(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExReadWriteRefurbInfo');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExRaiseException(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExRaiseException');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExRaiseStatus(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExRaiseStatus');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExReleaseReadWriteLock(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExReleaseReadWriteLock');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExSaveNonVolatileSetting(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExSaveNonVolatileSetting');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExSemaphoreObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExSemaphoreObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExTimerObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExTimerObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExfInterlockedInsertHeadList(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExfInterlockedInsertHeadList');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExfInterlockedInsertTailList(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExfInterlockedInsertTailList');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ExfInterlockedRemoveHeadList(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ExfInterlockedRemoveHeadList');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_FscGetCacheSize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('FscGetCacheSize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_FscInvalidateIdleBlocks(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('FscInvalidateIdleBlocks');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_FscSetCacheSize(uCachePages: ULONG): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('FscSetCacheSize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalClearSoftwareInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalClearSoftwareInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalDisableSystemInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalDisableSystemInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalDiskCachePartitionCount(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalDiskCachePartitionCount');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalDiskModelNumber(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalDiskModelNumber');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalDiskSerialNumber(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalDiskSerialNumber');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalEnableSystemInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalEnableSystemInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalGetInterruptVector(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalGetInterruptVector');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalReadSMBusValue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalReadSMBusValue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalReadWritePCISpace(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalReadWritePCISpace');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalRegisterShutdownNotification(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalRegisterShutdownNotification');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalRequestSoftwareInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalRequestSoftwareInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalReturnToFirmware(Routine: RETURN_FIRMWARE): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalReturnToFirmware');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalWriteSMBusValue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalWriteSMBusValue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_InterlockedCompareExchange(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('InterlockedCompareExchange');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_InterlockedDecrement(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('InterlockedDecrement');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_InterlockedIncrement(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('InterlockedIncrement');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_InterlockedExchange(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('InterlockedExchange');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_InterlockedExchangeAdd(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('InterlockedExchangeAdd');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_InterlockedFlushSList(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('InterlockedFlushSList');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_InterlockedPopEntrySList(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('InterlockedPopEntrySList');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_InterlockedPushEntrySList(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('InterlockedPushEntrySList');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoAllocateIrp(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoAllocateIrp');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoBuildAsynchronousFsdRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoBuildAsynchronousFsdRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoBuildDeviceIoControlRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoBuildDeviceIoControlRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoBuildSynchronousFsdRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoBuildSynchronousFsdRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoCheckShareAccess(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoCheckShareAccess');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoCompletionObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoCompletionObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoCreateDevice(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoCreateDevice');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoCreateFile(
  FileHandle: PHANDLE; // out
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // out
  AllocationSize: PLARGE_INTEGER;
  FileAttributes: ULONG;
  ShareAccess: ULONG;
  Disposition: ULONG;
  CreateOptions: ULONG;
  Options: ULONG
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoCreateFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoCreateSymbolicLink(
  SymbolicLinkName: PSTRING;
  DeviceName: PSTRING
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoCreateSymbolicLink');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoDeleteDevice(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoDeleteDevice');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoDeleteSymbolicLink(
  SymbolicLinkName: PSTRING
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoDeleteSymbolicLink');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoDeviceObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoDeviceObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoFileObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoFileObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoFreeIrp(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoFreeIrp');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoInitializeIrp(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoInitializeIrp');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoInvalidDeviceRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoInvalidDeviceRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoQueryFileInformation(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoQueryFileInformation');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoQueryVolumeInformation(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoQueryVolumeInformation');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoQueueThreadIrp(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoQueueThreadIrp');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoRemoveShareAccess(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoRemoveShareAccess');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoSetIoCompletion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoSetIoCompletion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoSetShareAccess(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoSetShareAccess');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoStartNextPacket(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoStartNextPacket');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoStartNextPacketByKey(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoStartNextPacketByKey');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoStartPacket(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoStartPacket');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoSynchronousDeviceIoControlRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoSynchronousDeviceIoControlRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoSynchronousFsdRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoSynchronousFsdRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IofCallDriver(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IofCallDriver');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IofCompleteRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IofCompleteRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KdDebuggerEnabled(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KdDebuggerEnabled');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KdDebuggerNotPresent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KdDebuggerNotPresent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoDismountVolume(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoDismountVolume');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoDismountVolumeByName(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoDismountVolumeByName');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeAlertResumeThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeAlertResumeThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeAlertThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeAlertThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeBoostPriorityThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeBoostPriorityThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeBugCheck(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeBugCheck');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeBugCheckEx(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeBugCheckEx');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeCancelTimer(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeCancelTimer');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeConnectInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeConnectInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeDelayExecutionThread(
  WaitMode: KPROCESSOR_MODE;
  Alertable: BOOLEAN;
  Interval: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeDelayExecutionThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeDisconnectInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeDisconnectInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeEnterCriticalRegion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeEnterCriticalRegion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmGlobalData(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmGlobalData');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeGetCurrentIrql(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeGetCurrentIrql');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeGetCurrentThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeGetCurrentThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeApc(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeApc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeDpc(
  Dpc: PKDPC;
  DeferredRoutine: PKDEFERRED_ROUTINE;
  DeferredContext: PVOID
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeDpc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeMutant(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeMutant');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeSemaphore(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeSemaphore');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeTimerEx(
  Timer: PKTIMER;
  &Type: TIMER_TYPE
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeTimerEx');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertByKeyDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertByKeyDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertHeadQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertHeadQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertQueueApc(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertQueueApc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertQueueDpc(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertQueueDpc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInterruptTime(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInterruptTime');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeIsExecutingDpc(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeIsExecutingDpc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeLeaveCriticalRegion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeLeaveCriticalRegion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KePulseEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KePulseEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeQueryBasePriorityThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeQueryBasePriorityThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeQueryInterruptTime(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeQueryInterruptTime');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeQueryPerformanceCounter(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeQueryPerformanceCounter');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeQueryPerformanceFrequency(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeQueryPerformanceFrequency');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_KeQuerySystemTime(
  CurrentTime: PLARGE_INTEGER
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('KeQuerySystemTime');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRaiseIrqlToDpcLevel(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRaiseIrqlToDpcLevel');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRaiseIrqlToSynchLevel(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRaiseIrqlToSynchLevel');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeReleaseMutant(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeReleaseMutant');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeReleaseSemaphore(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeReleaseSemaphore');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRemoveByKeyDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRemoveByKeyDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRemoveDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRemoveDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRemoveEntryDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRemoveEntryDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRemoveQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRemoveQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRemoveQueueDpc(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRemoveQueueDpc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeResetEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeResetEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRestoreFloatingPointState(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRestoreFloatingPointState');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeResumeThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeResumeThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRundownQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRundownQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSaveFloatingPointState(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSaveFloatingPointState');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetBasePriorityThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetBasePriorityThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetDisableBoostThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetDisableBoostThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetEventBoostPriority(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetEventBoostPriority');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetPriorityProcess(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetPriorityProcess');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetPriorityThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetPriorityThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetTimer(
  Timer: PKTIMER;
  DueTime: LARGE_INTEGER;
  Dpc: PKDPC // OPTIONAL
  ): BOOLEAN; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('KeSetTimer');
  Result := FALSE;
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetTimerEx(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetTimerEx');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeStallExecutionProcessor(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeStallExecutionProcessor');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSuspendThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSuspendThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSynchronizeExecution(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSynchronizeExecution');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSystemTime(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSystemTime');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeTestAlertThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeTestAlertThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeTickCount(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeTickCount');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeTimeIncrement(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeTimeIncrement');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeWaitForMultipleObjects(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeWaitForMultipleObjects');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeWaitForSingleObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeWaitForSingleObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KfRaiseIrql(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KfRaiseIrql');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KfLowerIrql(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KfLowerIrql');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KiBugCheckData(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KiBugCheckData');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KiUnlockDispatcherDatabase(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KiUnlockDispatcherDatabase');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_LaunchDataPage(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('LaunchDataPage');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmAllocateContiguousMemory(
  NumberOfBytes: ULONG
  ): PVOID; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('MmAllocateContiguousMemory');
  Result := NULL;
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmAllocateContiguousMemoryEx(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmAllocateContiguousMemoryEx');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmAllocateSystemMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmAllocateSystemMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmClaimGpuInstanceMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmClaimGpuInstanceMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmCreateKernelStack(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmCreateKernelStack');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDeleteKernelStack(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDeleteKernelStack');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmFreeContiguousMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmFreeContiguousMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmFreeSystemMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmFreeSystemMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmGetPhysicalAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmGetPhysicalAddress');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmIsAddressValid(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmIsAddressValid');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmLockUnlockBufferPages(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmLockUnlockBufferPages');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmLockUnlockPhysicalPage(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmLockUnlockPhysicalPage');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmMapIoSpace(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmMapIoSpace');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmPersistContiguousMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmPersistContiguousMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmQueryAddressProtect(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmQueryAddressProtect');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmQueryAllocationSize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmQueryAllocationSize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmQueryStatistics(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmQueryStatistics');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmSetAddressProtect(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmSetAddressProtect');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmUnmapIoSpace(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmUnmapIoSpace');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtAllocateVirtualMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtAllocateVirtualMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtCancelTimer(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtCancelTimer');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtClearEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtClearEvent');
  EmuSwapFS(); // Xbox FS
end;

// 0x00BB - NtClose
function xboxkrnl_NtClose(Handle: THandle): NTSTATUS; stdcall; {XBSYSAPI EXPORTNUM(187)}
{$IFDEF DXBX_EMUHANDLES}
var
  iEmuHandle: TEmuHandle;
{$ENDIF}
begin
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuKrnl : NtClose' +
    #13#10'(' +
    #13#10'   Handle              : 0x' + IntToHex(Integer(Handle), 8) +
    #13#10');');

{$IFDEF DXBX_EMUHANDLES}
  // delete 'special' handles
  if IsEmuHandle(Handle) then
  begin
    iEmuHandle := EmuHandleToPtr(Handle);

    iEmuHandle.Free;

    Result := STATUS_SUCCESS;
  end
  else // close normal handles
{$ENDIF}
    Result := NtClose(Handle);

  EmuSwapFS();   // Xbox FS
end;

function xboxkrnl_NtCreateDirectoryObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtCreateDirectoryObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtCreateEvent(pFileHandle: dtU32; pszUnknownArgs1: dtU32; pszUnknownArgs2: dtU32; pszUnknownArgs3: dtU32): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtCreateEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtCreateFile(pFileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pIoStatusBlock: dtU32; AllocationSize: dtU32; FileAttributes: dtU32; ShareAccess: dtACCESS_MASK; CreateDisposition: dtCreateDisposition; CreateOptions: dtCreateOptions): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtCreateFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtCreateIoCompletion(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtCreateIoCompletion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtCreateMutant(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtCreateMutant');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtCreateSemaphore(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtCreateSemaphore');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtCreateTimer(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtCreateTimer');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtDeleteFile(pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtDeleteFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtDeviceIoControlFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; pIoControlCode: dtU32; pInputBuffer: dtU32; InputBufferLength: dtU32; pOutputBuffer: dtU32; OutputBufferLength: dtU32): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtDeviceIoControlFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtDuplicateObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtDuplicateObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtFlushBuffersFile(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtFlushBuffersFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtFreeVirtualMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtFreeVirtualMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtFsControlFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; FsControlCode: dtU32; pInputBuffer: dtU32; InputBufferLength: dtU32; pOutputBuffer: dtU32; OutputBufferLength: dtU32): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtFsControlFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtOpenDirectoryObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtOpenDirectoryObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtOpenFile(pFileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pIoStatusBlock: dtU32; ShareAccess: dtACCESS_MASK; OpenOptions: dtCreateOptions): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtOpenFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtOpenSymbolicLinkObject(pFileHandle: dtU32; pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtOpenSymbolicLinkObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtProtectVirtualMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtProtectVirtualMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtPulseEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtPulseEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQueueApcThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQueueApcThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQueryDirectoryFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; pFileInformation: dtU32; Length: dtU32; FileInformationClass: dtFILE_INFORMATION_CLASSEnum; pFileMask: dtANSI_STRING; RestartScan: dtU32): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQueryDirectoryFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQueryDirectoryObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQueryDirectoryObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQueryEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQueryEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQueryFullAttributesFile(pObjectAttributes: dtObjectAttributes; pAttributes: dtU32): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQueryFullAttributesFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQueryInformationFile(FileHandle: dtU32; pIoStatusBlock: dtU32; pFileInformation: dtU32; Length: dtU32; FileInformationClass: dtFILE_INFORMATION_CLASSEnum): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQueryInformationFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQueryIoCompletion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQueryIoCompletion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQueryMutant(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQueryMutant');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQuerySemaphore(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQuerySemaphore');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQuerySymbolicLinkObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQuerySymbolicLinkObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQueryTimer(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQueryTimer');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQueryVirtualMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQueryVirtualMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtQueryVolumeInformationFile(FileHandle: dtU32; pIoStatusBlock: dtU32; pFileSystemInformation: dtU32; Length: dtU32; FileInformationClass: dtFILE_INFORMATION_CLASSEnum): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtQueryVolumeInformationFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtReadFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; pBuffer: dtU32; Length: dtU32; pByteOffset: dtU32): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtReadFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtReadFileScatter(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtReadFileScatter');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtReleaseMutant(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtReleaseMutant');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtReleaseSemaphore(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtReleaseSemaphore');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtRemoveIoCompletion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtRemoveIoCompletion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtResumeThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtResumeThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtSetEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtSetEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtSetInformationFile(FileHandle: dtU32; pIoStatusBlock: dtU32; FileInformation: dtU32; Length: dtU32; FileInformationClass: dtFILE_INFORMATION_CLASSEnum): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtSetInformationFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtSetIoCompletion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtSetIoCompletion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtSetSystemTime(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtSetSystemTime');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtSetTimerEx(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtSetTimerEx');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtSignalAndWaitForSingleObjectEx(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtSignalAndWaitForSingleObjectEx');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtSuspendThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtSuspendThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtUserIoApcDispatcher(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtUserIoApcDispatcher');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtWaitForSingleObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtWaitForSingleObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtWaitForSingleObjectEx(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtWaitForSingleObjectEx');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtWaitForMultipleObjectsEx(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtWaitForMultipleObjectsEx');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtWriteFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; pBuffer: dtU32; Length: dtU32; pByteOffset: dtU32): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtWriteFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_NtWriteFileGather(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('NtWriteFileGather');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_NtYieldExecution(); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS

  // NOTE: this eats up the debug log far too quickly
  //DbgPrintf("EmuKrnl (0x%X): NtYieldExecution();\n", GetCurrentThreadId());

  NtYieldExecution();
  
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObCreateObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObCreateObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObDirectoryObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObDirectoryObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObInsertObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObInsertObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObMakeTemporaryObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObMakeTemporaryObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObOpenObjectByName(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObOpenObjectByName');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObOpenObjectByPointer(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObOpenObjectByPointer');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObpObjectHandleTable(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObpObjectHandleTable');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObReferenceObjectByHandle(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObReferenceObjectByHandle');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObReferenceObjectByName(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObReferenceObjectByName');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObReferenceObjectByPointer(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObReferenceObjectByPointer');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObSymbolicLinkObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObSymbolicLinkObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObfDereferenceObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObfDereferenceObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObfReferenceObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObfReferenceObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_PhyGetLinkState(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('PhyGetLinkState');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_PhyInitialize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('PhyInitialize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_PsCreateSystemThread(
  lpThreadAttributes: PULONG;              // SD
  dwStackSize: DWORD;                      // initial stack size
  lpStartAddress: PKSTART_ROUTINE;         // thread function
  lpParameter: PVOID;                      // thread argument
  dwCreationFlags: DWORD;                  // creation option
  lpThreadId: PULONG                       // thread identifier
): NTSTATUS; stdcall;
var
  ThreadHandle: HANDLE;
  ThreadExtraSize: ULONG;
  KernelStackSize: ULONG;
  TlsDataSize: ULONG;
  StartContext1: PVOID;
  StartContext2: PVOID;
  CreateSuspended: LONGBOOL;
  DebugStack: LONGBOOL;
begin
  EmuSwapFS(); // Win2k/XP FS

  // TODO : How to apply the local arguments like lpThreadAttributes ?
  ThreadHandle := 0;
  ThreadExtraSize := dwStackSize; // ??
  KernelStackSize := dwStackSize; // ??
  TlsDataSize := 0; // ??
  StartContext1 := lpParameter; // ??
  StartContext2 := nil; // ??
  CreateSuspended := (dwCreationFlags and CREATE_SUSPENDED) > 0;
  DebugStack := False; // ??

  DbgPrintf('EmuKrnl : PsCreateSystemThread' +
    #13#10'(' +
    #13#10'   lpThreadAttributes  : 0x' + IntToHex(lpThreadAttributes^, 8) +
    #13#10'   dwStackSize         : 0x' + IntToHex(dwStackSize, 8) +
    #13#10'   lpStartAddress      : 0x' + IntToHex(Integer(Addr(lpStartAddress)), 8) +
    #13#10'   lpParameter         : 0x' + IntToHex(Integer(lpParameter), 8) +
    #13#10'   dwCreationFlags     : 0x' + IntToHex(Integer(dwCreationFlags), 8) +
    #13#10'   ThreadId            : 0x' + IntToHex(Integer(Addr(lpThreadId)), 8) +
    #13#10');');

  // Pass-through to Ex-implementation :
  Result := xboxkrnl_PsCreateSystemThreadEx(
    {dummy}@ThreadHandle,
    ThreadExtraSize,
    KernelStackSize,
    TlsDataSize,
    {ThreadId=}lpThreadId,
    StartContext1,
    StartContext2,
    CreateSuspended,
    DebugStack,
    {StartRoutine=}lpStartAddress
    );

  EmuSwapFS(); // Xbox FS
end;

// 0x00FF - PsCreateSystemThreadEx
function xboxkrnl_PsCreateSystemThreadEx(
  ThreadHandle: PHANDLE; // out
  ThreadExtraSize: ULONG;
  KernelStackSize: ULONG;
  TlsDataSize: ULONG;
  ThreadId: PULONG; // out, optional
  StartContext1: PVOID;
  StartContext2: PVOID;
  CreateSuspended: LONGBOOL;
  DebugStack: LONGBOOL;
  StartRoutine: PKSTART_ROUTINE): NTSTATUS; stdcall; {XBSYSAPI NTAPI}
var
  dwThreadId: DWORD;
  hDupHandle: THandle;
  iPCSTProxyParam: PCSTProxyParam;
begin
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuKrnl : PsCreateSystemThreadEx' +
    #13#10'(' +
    #13#10'   ThreadHandle        : 0x' + IntToHex(Integer(ThreadHandle^), 8) +
    #13#10'   ThreadExtraSize     : 0x' + IntToHex(ThreadExtraSize, 8) +
    #13#10'   KernelStackSize     : 0x' + IntToHex(KernelStackSize, 8) +
    #13#10'   TlsDataSize         : 0x' + IntToHex(TlsDataSize, 8) +
    #13#10'   ThreadId            : 0x' + IntToHex(Integer(Addr(ThreadId)), 8) +
    #13#10'   StartContext1       : 0x' + IntToHex(Integer(StartContext1), 8) +
    #13#10'   StartContext2       : 0x' + IntToHex(Integer(StartContext2), 8) +
    #13#10'   CreateSuspended     : 0x' + IntToHex(Integer(CreateSuspended), 8) +
    #13#10'   DebugStack          : 0x' + IntToHex(Integer(DebugStack), 8) +
    #13#10'   StartRoutine        : 0x' + IntToHex(Integer(Addr(StartRoutine)), 8) +
    #13#10');');

  // create thread, using our special proxy technique
  begin
    // PCSTProxy is responsible for cleaning up this pointer
    iPCSTProxyParam.StartContext1 := StartContext1;
    iPCSTProxyParam.StartContext2 := StartContext2;
    iPCSTProxyParam.StartRoutine  := StartRoutine;
    iPCSTProxyParam.StartSuspended := CreateSuspended;
    iPCSTProxyParam.hStartedEvent := CreateEvent(nil, False, False, nil);

    ThreadHandle^ := BeginThread(nil, 0, @PCSTProxy, @iPCSTProxyParam, 0, {var}dwThreadId);

    WaitForSingleObject(iPCSTProxyParam.hStartedEvent, 1000);

    DbgPrintf('EmuKrnl : ThreadHandle : 0x' + IntToHex(ThreadHandle^, 1) + ', ThreadId : 0x' + IntToHex(dwThreadId, 8));

    // we must duplicate this handle in order to retain Suspend/Resume thread rights from a remote thread
    begin
      hDupHandle := 0;

      DuplicateHandle(GetCurrentProcess(), ThreadHandle^, GetCurrentProcess(), @hDupHandle, 0, FALSE, DUPLICATE_SAME_ACCESS);

      CxbxKrnlRegisterThread(hDupHandle);
    end;

    if Assigned(ThreadId) then
      {out}ThreadId^ := dwThreadId;
  end;

  EmuSwapFS();   // Xbox FS

  Result := STATUS_SUCCESS;
end;

function xboxkrnl_PsQueryStatistics(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('PsQueryStatistics');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_PsSetCreateThreadNotifyRoutine(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('PsSetCreateThreadNotifyRoutine');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_PsTerminateSystemThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('PsTerminateSystemThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_PsThreadObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('PsThreadObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlAnsiStringToUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlAnsiStringToUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlAppendStringToString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlAppendStringToString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlAppendUnicodeStringToString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlAppendUnicodeStringToString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlAppendUnicodeToString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlAppendUnicodeToString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlAssert(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlAssert');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCaptureContext(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCaptureContext');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCaptureStackBackTrace(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCaptureStackBackTrace');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCharToInteger(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCharToInteger');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCompareMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCompareMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCompareMemoryUlong(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCompareMemoryUlong');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCompareString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCompareString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCompareUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCompareUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCopyString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCopyString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCopyUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCopyUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlCreateUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlCreateUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlDowncaseUnicodeChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlDowncaseUnicodeChar');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlDowncaseUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlDowncaseUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlEnterCriticalSection(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlEnterCriticalSection');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlEnterCriticalSectionAndRegion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlEnterCriticalSectionAndRegion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlEqualString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlEqualString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlEqualUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlEqualUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlExtendedIntegerMultiply(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlExtendedIntegerMultiply');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlExtendedLargeIntegerDivide(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlExtendedLargeIntegerDivide');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlExtendedMagicDivide(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlExtendedMagicDivide');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlFillMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlFillMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlFillMemoryUlong(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlFillMemoryUlong');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlFreeAnsiString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlFreeAnsiString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlFreeUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlFreeUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlGetCallersAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlGetCallersAddress');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlInitAnsiString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlInitAnsiString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlInitUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlInitUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlInitializeCriticalSection(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlInitializeCriticalSection');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlIntegerToChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlIntegerToChar');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlIntegerToUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlIntegerToUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlLeaveCriticalSection(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlLeaveCriticalSection');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlLeaveCriticalSectionAndRegion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlLeaveCriticalSectionAndRegion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlLowerChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlLowerChar');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlMapGenericMask(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlMapGenericMask');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlMoveMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlMoveMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlMultiByteToUnicodeN(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlMultiByteToUnicodeN');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlMultiByteToUnicodeSize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlMultiByteToUnicodeSize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlNtStatusToDosError(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlNtStatusToDosError');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlRaiseException(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlRaiseException');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlRaiseStatus(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlRaiseStatus');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlTimeFieldsToTime(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlTimeFieldsToTime');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlTimeToTimeFields(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlTimeToTimeFields');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlTryEnterCriticalSection(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlTryEnterCriticalSection');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUlongByteSwap(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUlongByteSwap');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUnicodeStringToAnsiString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUnicodeStringToAnsiString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUnicodeStringToInteger(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUnicodeStringToInteger');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUnicodeToMultiByteN(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUnicodeToMultiByteN');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUnicodeToMultiByteSize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUnicodeToMultiByteSize');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUnwind(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUnwind');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUpcaseUnicodeChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUpcaseUnicodeChar');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUpcaseUnicodeString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUpcaseUnicodeString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUpcaseUnicodeToMultiByteN(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUpcaseUnicodeToMultiByteN');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUpperChar(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUpperChar');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUpperString(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUpperString');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlUshortByteSwap(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlUshortByteSwap');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlWalkFrameChain(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlWalkFrameChain');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlZeroMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlZeroMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxEEPROMKey(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxEEPROMKey');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxHardwareInfo(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxHardwareInfo');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxHDKey(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxHDKey');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxKrnlVersion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxKrnlVersion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxSignatureKey(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxSignatureKey');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XeImageFileName(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XeImageFileName');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XeLoadSection(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XeLoadSection');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XeUnloadSection(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XeUnloadSection');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_READ_PORT_BUFFER_UCHAR(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('READ_PORT_BUFFER_UCHAR');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_READ_PORT_BUFFER_USHORT(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('READ_PORT_BUFFER_USHORT');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_READ_PORT_BUFFER_ULONG(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('READ_PORT_BUFFER_ULONG');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_WRITE_PORT_BUFFER_UCHAR(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('WRITE_PORT_BUFFER_UCHAR');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_WRITE_PORT_BUFFER_USHORT(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('WRITE_PORT_BUFFER_USHORT');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_WRITE_PORT_BUFFER_ULONG(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('WRITE_PORT_BUFFER_ULONG');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcSHAInit(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcSHAInit');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcSHAUpdate(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcSHAUpdate');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcSHAFinal(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcSHAFinal');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcRC4Key(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcRC4Key');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcRC4Crypt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcRC4Crypt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcHMAC(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcHMAC');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcPKEncPublic(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcPKEncPublic');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcPKDecPrivate(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcPKDecPrivate');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcPKGetKeyLen(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcPKGetKeyLen');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcVerifyPKCS1Signature(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcVerifyPKCS1Signature');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcModExp(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcModExp');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcDESKeyParity(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcDESKeyParity');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcKeyTable(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcKeyTable');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcBlockCrypt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcBlockCrypt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcBlockCryptCBC(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcBlockCryptCBC');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcCryptService(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcCryptService');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XcUpdateCrypto(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XcUpdateCrypto');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlRip(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlRip');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxLANKey(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxLANKey');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XboxAlternateSignatureKeys(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XboxAlternateSignatureKeys');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_XePublicKeyData(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('XePublicKeyData');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalBootSMCVideoMode(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalBootSMCVideoMode');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IdexChannelObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IdexChannelObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalIsResetOrShutdownPending(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalIsResetOrShutdownPending');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoMarkIrpMustComplete(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoMarkIrpMustComplete');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalInitiateShutdown(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalInitiateShutdown');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlSnprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlSnprintf');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlSprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlSprintf');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlVsnprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlVsnprintf');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_RtlVsprintf(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('RtlVsprintf');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalEnableSecureTrayEject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalEnableSecureTrayEject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalWriteSMCScratchRegister(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalWriteSMCScratchRegister');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_UnknownAPI367(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('UnknownAPI367');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_UnknownAPI368(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('UnknownAPI368');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_UnknownAPI369(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('UnknownAPI369');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_UnknownAPI370(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('UnknownAPI370');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_UnknownAPI371(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('UnknownAPI371');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_UnknownAPI372(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('UnknownAPI372');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_UnknownAPI373(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('UnknownAPI373');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDbgAllocateMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDbgAllocateMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDbgFreeMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDbgFreeMemory');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDbgQueryAvailablePages(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDbgQueryAvailablePages');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDbgReleaseAddress(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDbgReleaseAddress');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_MmDbgWriteCheck(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('MmDbgWriteCheck');
  EmuSwapFS(); // Xbox FS
end;

end.
