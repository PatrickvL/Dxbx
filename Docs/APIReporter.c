/* Pedro's APIReporter
	Version 1.0 - was the Proof-Of-Concept release.
	Version 2.0 - Logger and Reporter are split.
*/

#include <stdio.h>
#include <malloc.h>

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned long u32;

typedef struct _NAME_VALUE_PAIR
{
		const char* pszName;
		unsigned long dwValue;
} NAME_VALUE_PAIR;

#define OutputBitPattern( a, b ) OutputBitPatternFunc(a,b,sizeof(b)/sizeof(NAME_VALUE_PAIR))
#define OutputEnum( a, b ) OutputEnumFunc(a,b,sizeof(b)/sizeof(NAME_VALUE_PAIR))

enum eDATATYPE {
	dtBLOB,
	dtU8,
	dtU16,
	dtU32,
	dtObjectAttributes,
	dtOAAttributes,
	dtACCESS_MASK,
	dtCreateDisposition,
	dtCreateOptions,
	dtANSI_STRING,
	dtSTRING,
	dtFILE_INFORMATION_CLASSEnum
};

typedef struct _ARGDETAILS {
		const char* pszLegend;
		const enum eDATATYPE dt;
} ARGDETAILS;

typedef struct _PARAMDETAILS {
		const char* pszLegend;
		const ARGDETAILS* pArgDetails;
		const int nNumArgs;
} PARAMDETAILS;

/* Globals. */
const static char* pszHexPrefix = "0x";
const static char* pszEAX = " == ";

static FILE *fp;
static char szOutputBuffer[0x4096] = "\000";	/* Output buffer. */
static int iOutputBufferIndex = 0;
static int bPrnString = 0;

static int	bStringsOnly = 0;
static int	bOutputOn = 1;

/**************** Data tables *****************/

/* Structure definations. */
const static ARGDETAILS ANSI_STRINGMembers[] = { { "Length", dtU16 }, { "MaximumLength", dtU16 }, { "Buffer", dtSTRING } };
const static ARGDETAILS OBJECT_ATTRIBUTESMembers[] = { { "RootDirectory", dtU32 }, { "ObjectName", dtANSI_STRING }, { "Attributes", dtOAAttributes } };

/* Kernel call argument datatype definations. */
const static char pszUnknownArgs[] = "UnknownArgs";
const static ARGDETAILS AvGetSavedDataAddressArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS AvSendTVEncoderOptionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS AvSetDisplayModeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS AvSetSavedDataAddressArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS DbgBreakPointArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS DbgBreakPointWithStatusArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS DbgLoadImageSymbolsArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS DbgPrintArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalReadSMCTrayStateArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS DbgPromptArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS DbgUnLoadImageSymbolsArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExAcquireReadWriteLockExclusiveArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExAcquireReadWriteLockSharedArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExAllocatePoolArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExAllocatePoolWithTagArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExEventObjectTypeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExFreePoolArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExInitializeReadWriteLockArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExInterlockedAddLargeIntegerArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExInterlockedAddLargeStatisticArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExInterlockedCompareExchange64Args[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExMutantObjectTypeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExQueryPoolBlockSizeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExQueryNonVolatileSettingArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExReadWriteRefurbInfoArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExRaiseExceptionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExRaiseStatusArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExReleaseReadWriteLockArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExSaveNonVolatileSettingArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExSemaphoreObjectTypeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExTimerObjectTypeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExfInterlockedInsertHeadListArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExfInterlockedInsertTailListArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ExfInterlockedRemoveHeadListArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS FscGetCacheSizeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS FscInvalidateIdleBlocksArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS FscSetCacheSizeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalClearSoftwareInterruptArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalDisableSystemInterruptArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalDiskCachePartitionCountArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalDiskModelNumberArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalDiskSerialNumberArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalEnableSystemInterruptArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalGetInterruptVectorArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalReadSMBusValueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalReadWritePCISpaceArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalRegisterShutdownNotificationArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalRequestSoftwareInterruptArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalReturnToFirmwareArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalWriteSMBusValueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS InterlockedCompareExchangeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS InterlockedDecrementArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS InterlockedIncrementArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS InterlockedExchangeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS InterlockedExchangeAddArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS InterlockedFlushSListArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS InterlockedPopEntrySListArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS InterlockedPushEntrySListArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoAllocateIrpArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoBuildAsynchronousFsdRequestArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoBuildDeviceIoControlRequestArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoBuildSynchronousFsdRequestArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoCheckShareAccessArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoCompletionObjectTypeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoCreateDeviceArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoCreateFileArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoCreateSymbolicLinkArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoDeleteDeviceArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoDeleteSymbolicLinkArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoDeviceObjectTypeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoFileObjectTypeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoFreeIrpArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoInitializeIrpArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoInvalidDeviceRequestArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoQueryFileInformationArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoQueryVolumeInformationArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoQueueThreadIrpArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoRemoveShareAccessArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoSetIoCompletionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoSetShareAccessArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoStartNextPacketArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoStartNextPacketByKeyArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoStartPacketArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoSynchronousDeviceIoControlRequestArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoSynchronousFsdRequestArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IofCallDriverArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IofCompleteRequestArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KdDebuggerEnabledArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KdDebuggerNotPresentArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoDismountVolumeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoDismountVolumeByNameArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeAlertResumeThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeAlertThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeBoostPriorityThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeBugCheckArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeBugCheckExArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeCancelTimerArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeConnectInterruptArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeDelayExecutionThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeDisconnectInterruptArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeEnterCriticalRegionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmGlobalDataArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeGetCurrentIrqlArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeGetCurrentThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInitializeApcArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInitializeDeviceQueueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInitializeDpcArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInitializeEventArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInitializeInterruptArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInitializeMutantArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInitializeQueueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInitializeSemaphoreArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInitializeTimerExArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInsertByKeyDeviceQueueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInsertDeviceQueueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInsertHeadQueueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInsertQueueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInsertQueueApcArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInsertQueueDpcArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeInterruptTimeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeIsExecutingDpcArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeLeaveCriticalRegionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KePulseEventArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeQueryBasePriorityThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeQueryInterruptTimeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeQueryPerformanceCounterArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeQueryPerformanceFrequencyArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeQuerySystemTimeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeRaiseIrqlToDpcLevelArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeRaiseIrqlToSynchLevelArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeReleaseMutantArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeReleaseSemaphoreArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeRemoveByKeyDeviceQueueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeRemoveDeviceQueueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeRemoveEntryDeviceQueueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeRemoveQueueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeRemoveQueueDpcArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeResetEventArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeRestoreFloatingPointStateArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeResumeThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeRundownQueueArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSaveFloatingPointStateArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSetBasePriorityThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSetDisableBoostThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSetEventArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSetEventBoostPriorityArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSetPriorityProcessArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSetPriorityThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSetTimerArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSetTimerExArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeStallExecutionProcessorArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSuspendThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSynchronizeExecutionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeSystemTimeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeTestAlertThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeTickCountArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeTimeIncrementArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeWaitForMultipleObjectsArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KeWaitForSingleObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KfRaiseIrqlArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KfLowerIrqlArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KiBugCheckDataArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS KiUnlockDispatcherDatabaseArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS LaunchDataPageArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmAllocateContiguousMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmAllocateContiguousMemoryExArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmAllocateSystemMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmClaimGpuInstanceMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmCreateKernelStackArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmDeleteKernelStackArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmFreeContiguousMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmFreeSystemMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmGetPhysicalAddressArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmIsAddressValidArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmLockUnlockBufferPagesArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmLockUnlockPhysicalPageArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmMapIoSpaceArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmPersistContiguousMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmQueryAddressProtectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmQueryAllocationSizeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmQueryStatisticsArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmSetAddressProtectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmUnmapIoSpaceArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtAllocateVirtualMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtCancelTimerArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtClearEventArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtCloseArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtCreateDirectoryObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtCreateEventArgs[] = { { "pFileHandle", dtU32 }, { pszUnknownArgs, dtU32 }, { pszUnknownArgs, dtU32 }, { pszUnknownArgs, dtU32 } };
const static ARGDETAILS NtCreateFileArgs[] = { { "pFileHandle", dtU32 }, { "DesiredAccess", dtACCESS_MASK }, { "pObjectAttributes", dtObjectAttributes }, { "pIoStatusBlock", dtU32 }, { "AllocationSize", dtU32 }, { "FileAttributes", dtU32 }, { "ShareAccess", dtACCESS_MASK }, { "CreateDisposition", dtCreateDisposition }, { "CreateOptions", dtCreateOptions } };
const static ARGDETAILS NtCreateIoCompletionArgs[] = { { "FileHandle", dtU32 }, { "DesiredAccess", dtACCESS_MASK }, { "pObjectAttributes", dtObjectAttributes }, { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtCreateMutantArgs[] = { { "FileHandle", dtU32 }, { "DesiredAccess", dtACCESS_MASK }, { "pObjectAttributes", dtObjectAttributes } };
const static ARGDETAILS NtCreateSemaphoreArgs[] = { { "FileHandle", dtU32 }, { "DesiredAccess", dtACCESS_MASK }, { "pObjectAttributes", dtObjectAttributes }, { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtCreateTimerArgs[] = { { "FileHandle", dtU32 }, { "DesiredAccess", dtACCESS_MASK }, { "pObjectAttributes", dtObjectAttributes }, { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtDeleteFileArgs[] = { { "pObjectAttributes", dtObjectAttributes } };
const static ARGDETAILS NtDeviceIoControlFileArgs[] = { { "FileHandle", dtU32 }, { "Event", dtU32 }, { "pApcRoutine", dtU32 }, { "pApcContext", dtU32 }, { "pIoStatusBlock", dtU32 }, { "pIoControlCode", dtU32 }, { "pInputBuffer", dtU32 }, { "InputBufferLength", dtU32 }, { "pOutputBuffer", dtU32 }, { "OutputBufferLength", dtU32 } };
const static ARGDETAILS NtDuplicateObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtFlushBuffersFileArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtFreeVirtualMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtFsControlFileArgs[] = { { "FileHandle", dtU32 }, { "Event", dtU32 }, { "pApcRoutine", dtU32 }, { "pApcContext", dtU32 }, { "pIoStatusBlock", dtU32 }, { "FsControlCode", dtU32 }, { "pInputBuffer", dtU32 }, { "InputBufferLength", dtU32 }, { "pOutputBuffer", dtU32 }, { "OutputBufferLength", dtU32 } };
const static ARGDETAILS NtOpenDirectoryObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtOpenFileArgs[] = { { "pFileHandle", dtU32 }, { "DesiredAccess", dtACCESS_MASK }, { "pObjectAttributes", dtObjectAttributes }, { "pIoStatusBlock", dtU32 }, { "ShareAccess", dtACCESS_MASK }, { "OpenOptions", dtCreateOptions } };
const static ARGDETAILS NtOpenSymbolicLinkObjectArgs[] = { { "pFileHandle", dtU32 }, { "pObjectAttributes", dtObjectAttributes } };
const static ARGDETAILS NtProtectVirtualMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtPulseEventArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtQueueApcThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtQueryDirectoryFileArgs[] = { { "FileHandle", dtU32 }, { "Event", dtU32 }, { "pApcRoutine", dtU32 }, { "pApcContext", dtU32 }, { "pIoStatusBlock", dtU32 }, { "pFileInformation", dtU32 }, { "Length", dtU32 }, { "FileInformationClass", dtFILE_INFORMATION_CLASSEnum }, { "pFileMask", dtANSI_STRING }, { "RestartScan", dtU32 } };
const static ARGDETAILS NtQueryDirectoryObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtQueryEventArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtQueryFullAttributesFileArgs[]  = { { "pObjectAttributes", dtObjectAttributes }, { "pAttributes", dtU32 } };
const static ARGDETAILS NtQueryInformationFileArgs[] = { { "FileHandle", dtU32 }, { "pIoStatusBlock", dtU32 }, { "pFileInformation", dtU32 }, { "Length", dtU32 }, { "FileInformationClass", dtFILE_INFORMATION_CLASSEnum } };
const static ARGDETAILS NtQueryIoCompletionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtQueryMutantArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtQuerySemaphoreArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtQuerySymbolicLinkObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtQueryTimerArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtQueryVirtualMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtQueryVolumeInformationFileArgs[] = { { "FileHandle", dtU32 }, { "pIoStatusBlock", dtU32 }, { "pFileSystemInformation", dtU32 }, { "Length", dtU32 }, { "FileInformationClass", dtFILE_INFORMATION_CLASSEnum } };
const static ARGDETAILS NtReadFileArgs[] = { { "FileHandle", dtU32 }, { "Event", dtU32 }, { "pApcRoutine", dtU32 }, { "pApcContext", dtU32 }, { "pIoStatusBlock", dtU32 }, { "pBuffer", dtU32 }, { "Length", dtU32 }, { "pByteOffset", dtU32 } };
const static ARGDETAILS NtReadFileScatterArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtReleaseMutantArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtReleaseSemaphoreArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtRemoveIoCompletionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtResumeThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtSetEventArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtSetInformationFileArgs[] = { { "FileHandle", dtU32 }, { "pIoStatusBlock", dtU32 }, { "FileInformation", dtU32 }, { "Length", dtU32 }, { "FileInformationClass", dtFILE_INFORMATION_CLASSEnum } };
const static ARGDETAILS NtSetIoCompletionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtSetSystemTimeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtSetTimerExArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtSignalAndWaitForSingleObjectExArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtSuspendThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtUserIoApcDispatcherArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtWaitForSingleObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtWaitForSingleObjectExArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtWaitForMultipleObjectsExArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtWriteFileArgs[] = { { "FileHandle", dtU32 }, { "Event", dtU32 }, { "pApcRoutine", dtU32 }, { "pApcContext", dtU32 }, { "pIoStatusBlock", dtU32 }, { "pBuffer", dtU32 }, { "Length", dtU32 }, { "pByteOffset", dtU32 } };
const static ARGDETAILS NtWriteFileGatherArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS NtYieldExecutionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObCreateObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObDirectoryObjectTypeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObInsertObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObMakeTemporaryObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObOpenObjectByNameArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObOpenObjectByPointerArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObpObjectHandleTableArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObReferenceObjectByHandleArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObReferenceObjectByNameArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObReferenceObjectByPointerArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObSymbolicLinkObjectTypeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObfDereferenceObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS ObfReferenceObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS PhyGetLinkStateArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS PhyInitializeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS PsCreateSystemThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS PsCreateSystemThreadExArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS PsQueryStatisticsArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS PsSetCreateThreadNotifyRoutineArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS PsTerminateSystemThreadArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS PsThreadObjectTypeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlAnsiStringToUnicodeStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlAppendStringToStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlAppendUnicodeStringToStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlAppendUnicodeToStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlAssertArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlCaptureContextArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlCaptureStackBackTraceArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlCharToIntegerArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlCompareMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlCompareMemoryUlongArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlCompareStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlCompareUnicodeStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlCopyStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlCopyUnicodeStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlCreateUnicodeStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlDowncaseUnicodeCharArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlDowncaseUnicodeStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlEnterCriticalSectionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlEnterCriticalSectionAndRegionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlEqualStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlEqualUnicodeStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlExtendedIntegerMultiplyArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlExtendedLargeIntegerDivideArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlExtendedMagicDivideArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlFillMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlFillMemoryUlongArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlFreeAnsiStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlFreeUnicodeStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlGetCallersAddressArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlInitAnsiStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlInitUnicodeStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlInitializeCriticalSectionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlIntegerToCharArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlIntegerToUnicodeStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlLeaveCriticalSectionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlLeaveCriticalSectionAndRegionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlLowerCharArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlMapGenericMaskArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlMoveMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlMultiByteToUnicodeNArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlMultiByteToUnicodeSizeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlNtStatusToDosErrorArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlRaiseExceptionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlRaiseStatusArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlTimeFieldsToTimeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlTimeToTimeFieldsArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlTryEnterCriticalSectionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUlongByteSwapArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUnicodeStringToAnsiStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUnicodeStringToIntegerArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUnicodeToMultiByteNArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUnicodeToMultiByteSizeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUnwindArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUpcaseUnicodeCharArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUpcaseUnicodeStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUpcaseUnicodeToMultiByteNArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUpperCharArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUpperStringArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlUshortByteSwapArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlWalkFrameChainArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlZeroMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XboxEEPROMKeyArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XboxHardwareInfoArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XboxHDKeyArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XboxKrnlVersionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XboxSignatureKeyArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XeImageFileNameArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XeLoadSectionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XeUnloadSectionArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS READ_PORT_BUFFER_UCHARArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS READ_PORT_BUFFER_USHORTArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS READ_PORT_BUFFER_ULONGArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS WRITE_PORT_BUFFER_UCHARArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS WRITE_PORT_BUFFER_USHORTArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS WRITE_PORT_BUFFER_ULONGArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcSHAInitArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcSHAUpdateArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcSHAFinalArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcRC4KeyArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcRC4CryptArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcHMACArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcPKEncPublicArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcPKDecPrivateArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcPKGetKeyLenArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcVerifyPKCS1SignatureArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcModExpArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcDESKeyParityArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcKeyTableArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcBlockCryptArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcBlockCryptCBCArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcCryptServiceArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XcUpdateCryptoArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlRipArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XboxLANKeyArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XboxAlternateSignatureKeysArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS XePublicKeyDataArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalBootSMCVideoModeArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IdexChannelObjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalIsResetOrShutdownPendingArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS IoMarkIrpMustCompleteArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalInitiateShutdownArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlSnprintfArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlSprintfArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlVsnprintfArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS RtlVsprintfArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalEnableSecureTrayEjectArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS HalWriteSMCScratchRegisterArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmDbgAllocateMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmDbgFreeMemoryArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmDbgQueryAvailablePagesArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmDbgReleaseAddressArgs[] = { { pszUnknownArgs, dtBLOB } };
const static ARGDETAILS MmDbgWriteCheckArgs[] = { { pszUnknownArgs, dtBLOB } };

/* b argument not currently used (for future expansion ;) )*/
#define CALLARGS(a,b)	a, (sizeof(a)/sizeof(ARGDETAILS))
const static PARAMDETAILS aKernelCalls[] = {
	{ "AvGetSavedDataAddress", CALLARGS(AvGetSavedDataAddressArgs,0) }, /* Ordinal number 1, stack param usage 0 */
	{ "AvSendTVEncoderOption", CALLARGS(AvSendTVEncoderOptionArgs,16) }, /* Ordinal number 2, stack param usage 16 */
	{ "AvSetDisplayMode", CALLARGS(AvSetDisplayModeArgs,24) }, /* Ordinal number 3, stack param usage 24 */
	{ "AvSetSavedDataAddress", CALLARGS(AvSetSavedDataAddressArgs,4) }, /* Ordinal number 4, stack param usage 4 */
	{ "DbgBreakPoint", CALLARGS(DbgBreakPointArgs,0) }, /* Ordinal number 5, stack param usage 0 */
	{ "DbgBreakPointWithStatus", CALLARGS(DbgBreakPointWithStatusArgs,4) }, /* Ordinal number 6, stack param usage 4 */
	{ "DbgLoadImageSymbols", CALLARGS(DbgLoadImageSymbolsArgs,12) }, /* Ordinal number 7, stack param usage 12 */
	{ "DbgPrint", CALLARGS(DbgPrintArgs,0) }, /* Ordinal number 8, stack param usage 0 */
	{ "HalReadSMCTrayState", CALLARGS(HalReadSMCTrayStateArgs,8) }, /* Ordinal number 9, stack param usage 8 */
	{ "DbgPrompt", CALLARGS(DbgPromptArgs,12) }, /* Ordinal number 10, stack param usage 12 */
	{ "DbgUnLoadImageSymbols", CALLARGS(DbgUnLoadImageSymbolsArgs,12) }, /* Ordinal number 11, stack param usage 12 */
	{ "ExAcquireReadWriteLockExclusive", CALLARGS(ExAcquireReadWriteLockExclusiveArgs,4) }, /* Ordinal number 12, stack param usage 4 */
	{ "ExAcquireReadWriteLockShared", CALLARGS(ExAcquireReadWriteLockSharedArgs,4) }, /* Ordinal number 13, stack param usage 4 */
	{ "ExAllocatePool", CALLARGS(ExAllocatePoolArgs,4) }, /* Ordinal number 14, stack param usage 4 */
	{ "ExAllocatePoolWithTag", CALLARGS(ExAllocatePoolWithTagArgs,8) }, /* Ordinal number 15, stack param usage 8 */
	{ "ExEventObjectType", CALLARGS(ExEventObjectTypeArgs,0) }, /* Ordinal number 16, stack param usage 0 */
	{ "ExFreePool", CALLARGS(ExFreePoolArgs,4) }, /* Ordinal number 17, stack param usage 4 */
	{ "ExInitializeReadWriteLock", CALLARGS(ExInitializeReadWriteLockArgs,4) }, /* Ordinal number 18, stack param usage 4 */
	{ "ExInterlockedAddLargeInteger", CALLARGS(ExInterlockedAddLargeIntegerArgs,16) }, /* Ordinal number 19, stack param usage 16 */
	{ "ExInterlockedAddLargeStatistic", CALLARGS(ExInterlockedAddLargeStatisticArgs,8) }, /* Ordinal number 20, stack param usage 8 */
	{ "ExInterlockedCompareExchange64", CALLARGS(ExInterlockedCompareExchange64Args,12) }, /* Ordinal number 21, stack param usage 12 */
	{ "ExMutantObjectType", CALLARGS(ExMutantObjectTypeArgs,0) }, /* Ordinal number 22, stack param usage 0 */
	{ "ExQueryPoolBlockSize", CALLARGS(ExQueryPoolBlockSizeArgs,4) }, /* Ordinal number 23, stack param usage 4 */
	{ "ExQueryNonVolatileSetting", CALLARGS(ExQueryNonVolatileSettingArgs,20) }, /* Ordinal number 24, stack param usage 20 */
	{ "ExReadWriteRefurbInfo", CALLARGS(ExReadWriteRefurbInfoArgs,12) }, /* Ordinal number 25, stack param usage 12 */
	{ "ExRaiseException", CALLARGS(ExRaiseExceptionArgs,4) }, /* Ordinal number 26, stack param usage 4 */
	{ "ExRaiseStatus", CALLARGS(ExRaiseStatusArgs,4) }, /* Ordinal number 27, stack param usage 4 */
	{ "ExReleaseReadWriteLock", CALLARGS(ExReleaseReadWriteLockArgs,4) }, /* Ordinal number 28, stack param usage 4 */
	{ "ExSaveNonVolatileSetting", CALLARGS(ExSaveNonVolatileSettingArgs,16) }, /* Ordinal number 29, stack param usage 16 */
	{ "ExSemaphoreObjectType", CALLARGS(ExSemaphoreObjectTypeArgs,0) }, /* Ordinal number 30, stack param usage 0 */
	{ "ExTimerObjectType", CALLARGS(ExTimerObjectTypeArgs,0) }, /* Ordinal number 31, stack param usage 0 */
	{ "ExfInterlockedInsertHeadList", CALLARGS(ExfInterlockedInsertHeadListArgs,8) }, /* Ordinal number 32, stack param usage 8 */
	{ "ExfInterlockedInsertTailList", CALLARGS(ExfInterlockedInsertTailListArgs,8) }, /* Ordinal number 33, stack param usage 8 */
	{ "ExfInterlockedRemoveHeadList", CALLARGS(ExfInterlockedRemoveHeadListArgs,4) }, /* Ordinal number 34, stack param usage 4 */
	{ "FscGetCacheSize", CALLARGS(FscGetCacheSizeArgs,0) }, /* Ordinal number 35, stack param usage 0 */
	{ "FscInvalidateIdleBlocks", CALLARGS(FscInvalidateIdleBlocksArgs,0) }, /* Ordinal number 36, stack param usage 0 */
	{ "FscSetCacheSize", CALLARGS(FscSetCacheSizeArgs,4) }, /* Ordinal number 37, stack param usage 4 */
	{ "HalClearSoftwareInterrupt", CALLARGS(HalClearSoftwareInterruptArgs,4) }, /* Ordinal number 38, stack param usage 4 */
	{ "HalDisableSystemInterrupt", CALLARGS(HalDisableSystemInterruptArgs,4) }, /* Ordinal number 39, stack param usage 4 */
	{ "HalDiskCachePartitionCount", CALLARGS(HalDiskCachePartitionCountArgs,0) }, /* Ordinal number 40, stack param usage 0 */
	{ "HalDiskModelNumber", CALLARGS(HalDiskModelNumberArgs,0) }, /* Ordinal number 41, stack param usage 0 */
	{ "HalDiskSerialNumber", CALLARGS(HalDiskSerialNumberArgs,0) }, /* Ordinal number 42, stack param usage 0 */
	{ "HalEnableSystemInterrupt", CALLARGS(HalEnableSystemInterruptArgs,8) }, /* Ordinal number 43, stack param usage 8 */
	{ "HalGetInterruptVector", CALLARGS(HalGetInterruptVectorArgs,8) }, /* Ordinal number 44, stack param usage 8 */
	{ "HalReadSMBusValue", CALLARGS(HalReadSMBusValueArgs,16) }, /* Ordinal number 45, stack param usage 16 */
	{ "HalReadWritePCISpace", CALLARGS(HalReadWritePCISpaceArgs,24) }, /* Ordinal number 46, stack param usage 24 */
	{ "HalRegisterShutdownNotification", CALLARGS(HalRegisterShutdownNotificationArgs,8) }, /* Ordinal number 47, stack param usage 8 */
	{ "HalRequestSoftwareInterrupt", CALLARGS(HalRequestSoftwareInterruptArgs,4) }, /* Ordinal number 48, stack param usage 4 */
	{ "HalReturnToFirmware", CALLARGS(HalReturnToFirmwareArgs,4) }, /* Ordinal number 49, stack param usage 4 */
	{ "HalWriteSMBusValue", CALLARGS(HalWriteSMBusValueArgs,16) }, /* Ordinal number 50, stack param usage 16 */
	{ "InterlockedCompareExchange", CALLARGS(InterlockedCompareExchangeArgs,12) }, /* Ordinal number 51, stack param usage 12 */
	{ "InterlockedDecrement", CALLARGS(InterlockedDecrementArgs,4) }, /* Ordinal number 52, stack param usage 4 */
	{ "InterlockedIncrement", CALLARGS(InterlockedIncrementArgs,4) }, /* Ordinal number 53, stack param usage 4 */
	{ "InterlockedExchange", CALLARGS(InterlockedExchangeArgs,8) }, /* Ordinal number 54, stack param usage 8 */
	{ "InterlockedExchangeAdd", CALLARGS(InterlockedExchangeAddArgs,8) }, /* Ordinal number 55, stack param usage 8 */
	{ "InterlockedFlushSList", CALLARGS(InterlockedFlushSListArgs,4) }, /* Ordinal number 56, stack param usage 4 */
	{ "InterlockedPopEntrySList", CALLARGS(InterlockedPopEntrySListArgs,4) }, /* Ordinal number 57, stack param usage 4 */
	{ "InterlockedPushEntrySList", CALLARGS(InterlockedPushEntrySListArgs,8) }, /* Ordinal number 58, stack param usage 8 */
	{ "IoAllocateIrp", CALLARGS(IoAllocateIrpArgs,4) }, /* Ordinal number 59, stack param usage 4 */
	{ "IoBuildAsynchronousFsdRequest", CALLARGS(IoBuildAsynchronousFsdRequestArgs,24) }, /* Ordinal number 60, stack param usage 24 */
	{ "IoBuildDeviceIoControlRequest", CALLARGS(IoBuildDeviceIoControlRequestArgs,36) }, /* Ordinal number 61, stack param usage 36 */
	{ "IoBuildSynchronousFsdRequest", CALLARGS(IoBuildSynchronousFsdRequestArgs,28) }, /* Ordinal number 62, stack param usage 28 */
	{ "IoCheckShareAccess", CALLARGS(IoCheckShareAccessArgs,20) }, /* Ordinal number 63, stack param usage 20 */
	{ "IoCompletionObjectType", CALLARGS(IoCompletionObjectTypeArgs,0) }, /* Ordinal number 64, stack param usage 0 */
	{ "IoCreateDevice", CALLARGS(IoCreateDeviceArgs,24) }, /* Ordinal number 65, stack param usage 24 */
	{ "IoCreateFile", CALLARGS(IoCreateFileArgs,40) }, /* Ordinal number 66, stack param usage 40 */
	{ "IoCreateSymbolicLink", CALLARGS(IoCreateSymbolicLinkArgs,8) }, /* Ordinal number 67, stack param usage 8 */
	{ "IoDeleteDevice", CALLARGS(IoDeleteDeviceArgs,4) }, /* Ordinal number 68, stack param usage 4 */
	{ "IoDeleteSymbolicLink", CALLARGS(IoDeleteSymbolicLinkArgs,4) }, /* Ordinal number 69, stack param usage 4 */
	{ "IoDeviceObjectType", CALLARGS(IoDeviceObjectTypeArgs,0) }, /* Ordinal number 70, stack param usage 0 */
	{ "IoFileObjectType", CALLARGS(IoFileObjectTypeArgs,0) }, /* Ordinal number 71, stack param usage 0 */
	{ "IoFreeIrp", CALLARGS(IoFreeIrpArgs,4) }, /* Ordinal number 72, stack param usage 4 */
	{ "IoInitializeIrp", CALLARGS(IoInitializeIrpArgs,12) }, /* Ordinal number 73, stack param usage 12 */
	{ "IoInvalidDeviceRequest", CALLARGS(IoInvalidDeviceRequestArgs,8) }, /* Ordinal number 74, stack param usage 8 */
	{ "IoQueryFileInformation", CALLARGS(IoQueryFileInformationArgs,20) }, /* Ordinal number 75, stack param usage 20 */
	{ "IoQueryVolumeInformation", CALLARGS(IoQueryVolumeInformationArgs,20) }, /* Ordinal number 76, stack param usage 20 */
	{ "IoQueueThreadIrp", CALLARGS(IoQueueThreadIrpArgs,4) }, /* Ordinal number 77, stack param usage 4 */
	{ "IoRemoveShareAccess", CALLARGS(IoRemoveShareAccessArgs,8) }, /* Ordinal number 78, stack param usage 8 */
	{ "IoSetIoCompletion", CALLARGS(IoSetIoCompletionArgs,20) }, /* Ordinal number 79, stack param usage 20 */
	{ "IoSetShareAccess", CALLARGS(IoSetShareAccessArgs,16) }, /* Ordinal number 80, stack param usage 16 */
	{ "IoStartNextPacket", CALLARGS(IoStartNextPacketArgs,4) }, /* Ordinal number 81, stack param usage 4 */
	{ "IoStartNextPacketByKey", CALLARGS(IoStartNextPacketByKeyArgs,8) }, /* Ordinal number 82, stack param usage 8 */
	{ "IoStartPacket", CALLARGS(IoStartPacketArgs,12) }, /* Ordinal number 83, stack param usage 12 */
	{ "IoSynchronousDeviceIoControlRequest", CALLARGS(IoSynchronousDeviceIoControlRequestArgs,32) }, /* Ordinal number 84, stack param usage 32 */
	{ "IoSynchronousFsdRequest", CALLARGS(IoSynchronousFsdRequestArgs,20) }, /* Ordinal number 85, stack param usage 20 */
	{ "IofCallDriver", CALLARGS(IofCallDriverArgs,8) }, /* Ordinal number 86, stack param usage 8 */
	{ "IofCompleteRequest", CALLARGS(IofCompleteRequestArgs,8) }, /* Ordinal number 87, stack param usage 8 */
	{ "KdDebuggerEnabled", CALLARGS(KdDebuggerEnabledArgs,0) }, /* Ordinal number 88, stack param usage 0 */
	{ "KdDebuggerNotPresent", CALLARGS(KdDebuggerNotPresentArgs,0) }, /* Ordinal number 89, stack param usage 0 */
	{ "IoDismountVolume", CALLARGS(IoDismountVolumeArgs,4) }, /* Ordinal number 90, stack param usage 4 */
	{ "IoDismountVolumeByName", CALLARGS(IoDismountVolumeByNameArgs,4) }, /* Ordinal number 91, stack param usage 4 */
	{ "KeAlertResumeThread", CALLARGS(KeAlertResumeThreadArgs,4) }, /* Ordinal number 92, stack param usage 4 */
	{ "KeAlertThread", CALLARGS(KeAlertThreadArgs,8) }, /* Ordinal number 93, stack param usage 8 */
	{ "KeBoostPriorityThread", CALLARGS(KeBoostPriorityThreadArgs,8) }, /* Ordinal number 94, stack param usage 8 */
	{ "KeBugCheck", CALLARGS(KeBugCheckArgs,4) }, /* Ordinal number 95, stack param usage 4 */
	{ "KeBugCheckEx", CALLARGS(KeBugCheckExArgs,20) }, /* Ordinal number 96, stack param usage 20 */
	{ "KeCancelTimer", CALLARGS(KeCancelTimerArgs,4) }, /* Ordinal number 97, stack param usage 4 */
	{ "KeConnectInterrupt", CALLARGS(KeConnectInterruptArgs,4) }, /* Ordinal number 98, stack param usage 4 */
	{ "KeDelayExecutionThread", CALLARGS(KeDelayExecutionThreadArgs,12) }, /* Ordinal number 99, stack param usage 12 */
	{ "KeDisconnectInterrupt", CALLARGS(KeDisconnectInterruptArgs,4) }, /* Ordinal number 100, stack param usage 4 */
	{ "KeEnterCriticalRegion", CALLARGS(KeEnterCriticalRegionArgs,0) }, /* Ordinal number 101, stack param usage 0 */
	{ "MmGlobalData", CALLARGS(MmGlobalDataArgs,0) }, /* Ordinal number 102, stack param usage 0 */
	{ "KeGetCurrentIrql", CALLARGS(KeGetCurrentIrqlArgs,0) }, /* Ordinal number 103, stack param usage 0 */
	{ "KeGetCurrentThread", CALLARGS(KeGetCurrentThreadArgs,0) }, /* Ordinal number 104, stack param usage 0 */
	{ "KeInitializeApc", CALLARGS(KeInitializeApcArgs,28) }, /* Ordinal number 105, stack param usage 28 */
	{ "KeInitializeDeviceQueue", CALLARGS(KeInitializeDeviceQueueArgs,4) }, /* Ordinal number 106, stack param usage 4 */
	{ "KeInitializeDpc", CALLARGS(KeInitializeDpcArgs,12) }, /* Ordinal number 107, stack param usage 12 */
	{ "KeInitializeEvent", CALLARGS(KeInitializeEventArgs,12) }, /* Ordinal number 108, stack param usage 12 */
	{ "KeInitializeInterrupt", CALLARGS(KeInitializeInterruptArgs,28) }, /* Ordinal number 109, stack param usage 28 */
	{ "KeInitializeMutant", CALLARGS(KeInitializeMutantArgs,8) }, /* Ordinal number 110, stack param usage 8 */
	{ "KeInitializeQueue", CALLARGS(KeInitializeQueueArgs,8) }, /* Ordinal number 111, stack param usage 8 */
	{ "KeInitializeSemaphore", CALLARGS(KeInitializeSemaphoreArgs,12) }, /* Ordinal number 112, stack param usage 12 */
	{ "KeInitializeTimerEx", CALLARGS(KeInitializeTimerExArgs,8) }, /* Ordinal number 113, stack param usage 8 */
	{ "KeInsertByKeyDeviceQueue", CALLARGS(KeInsertByKeyDeviceQueueArgs,12) }, /* Ordinal number 114, stack param usage 12 */
	{ "KeInsertDeviceQueue", CALLARGS(KeInsertDeviceQueueArgs,8) }, /* Ordinal number 115, stack param usage 8 */
	{ "KeInsertHeadQueue", CALLARGS(KeInsertHeadQueueArgs,8) }, /* Ordinal number 116, stack param usage 8 */
	{ "KeInsertQueue", CALLARGS(KeInsertQueueArgs,8) }, /* Ordinal number 117, stack param usage 8 */
	{ "KeInsertQueueApc", CALLARGS(KeInsertQueueApcArgs,16) }, /* Ordinal number 118, stack param usage 16 */
	{ "KeInsertQueueDpc", CALLARGS(KeInsertQueueDpcArgs,12) }, /* Ordinal number 119, stack param usage 12 */
	{ "KeInterruptTime", CALLARGS(KeInterruptTimeArgs,0) }, /* Ordinal number 120, stack param usage 0 */
	{ "KeIsExecutingDpc", CALLARGS(KeIsExecutingDpcArgs,0) }, /* Ordinal number 121, stack param usage 0 */
	{ "KeLeaveCriticalRegion", CALLARGS(KeLeaveCriticalRegionArgs,0) }, /* Ordinal number 122, stack param usage 0 */
	{ "KePulseEvent", CALLARGS(KePulseEventArgs,12) }, /* Ordinal number 123, stack param usage 12 */
	{ "KeQueryBasePriorityThread", CALLARGS(KeQueryBasePriorityThreadArgs,4) }, /* Ordinal number 124, stack param usage 4 */
	{ "KeQueryInterruptTime", CALLARGS(KeQueryInterruptTimeArgs,0) }, /* Ordinal number 125, stack param usage 0 */
	{ "KeQueryPerformanceCounter", CALLARGS(KeQueryPerformanceCounterArgs,0) }, /* Ordinal number 126, stack param usage 0 */
	{ "KeQueryPerformanceFrequency", CALLARGS(KeQueryPerformanceFrequencyArgs,0) }, /* Ordinal number 127, stack param usage 0 */
	{ "KeQuerySystemTime", CALLARGS(KeQuerySystemTimeArgs,4) }, /* Ordinal number 128, stack param usage 4 */
	{ "KeRaiseIrqlToDpcLevel", CALLARGS(KeRaiseIrqlToDpcLevelArgs,0) }, /* Ordinal number 129, stack param usage 0 */
	{ "KeRaiseIrqlToSynchLevel", CALLARGS(KeRaiseIrqlToSynchLevelArgs,0) }, /* Ordinal number 130, stack param usage 0 */
	{ "KeReleaseMutant", CALLARGS(KeReleaseMutantArgs,16) }, /* Ordinal number 131, stack param usage 16 */
	{ "KeReleaseSemaphore", CALLARGS(KeReleaseSemaphoreArgs,16) }, /* Ordinal number 132, stack param usage 16 */
	{ "KeRemoveByKeyDeviceQueue", CALLARGS(KeRemoveByKeyDeviceQueueArgs,8) }, /* Ordinal number 133, stack param usage 8 */
	{ "KeRemoveDeviceQueue", CALLARGS(KeRemoveDeviceQueueArgs,4) }, /* Ordinal number 134, stack param usage 4 */
	{ "KeRemoveEntryDeviceQueue", CALLARGS(KeRemoveEntryDeviceQueueArgs,8) }, /* Ordinal number 135, stack param usage 8 */
	{ "KeRemoveQueue", CALLARGS(KeRemoveQueueArgs,12) }, /* Ordinal number 136, stack param usage 12 */
	{ "KeRemoveQueueDpc", CALLARGS(KeRemoveQueueDpcArgs,4) }, /* Ordinal number 137, stack param usage 4 */
	{ "KeResetEvent", CALLARGS(KeResetEventArgs,4) }, /* Ordinal number 138, stack param usage 4 */
	{ "KeRestoreFloatingPointState", CALLARGS(KeRestoreFloatingPointStateArgs,4) }, /* Ordinal number 139, stack param usage 4 */
	{ "KeResumeThread", CALLARGS(KeResumeThreadArgs,4) }, /* Ordinal number 140, stack param usage 4 */
	{ "KeRundownQueue", CALLARGS(KeRundownQueueArgs,4) }, /* Ordinal number 141, stack param usage 4 */
	{ "KeSaveFloatingPointState", CALLARGS(KeSaveFloatingPointStateArgs,4) }, /* Ordinal number 142, stack param usage 4 */
	{ "KeSetBasePriorityThread", CALLARGS(KeSetBasePriorityThreadArgs,8) }, /* Ordinal number 143, stack param usage 8 */
	{ "KeSetDisableBoostThread", CALLARGS(KeSetDisableBoostThreadArgs,8) }, /* Ordinal number 144, stack param usage 8 */
	{ "KeSetEvent", CALLARGS(KeSetEventArgs,12) }, /* Ordinal number 145, stack param usage 12 */
	{ "KeSetEventBoostPriority", CALLARGS(KeSetEventBoostPriorityArgs,8) }, /* Ordinal number 146, stack param usage 8 */
	{ "KeSetPriorityProcess", CALLARGS(KeSetPriorityProcessArgs,8) }, /* Ordinal number 147, stack param usage 8 */
	{ "KeSetPriorityThread", CALLARGS(KeSetPriorityThreadArgs,8) }, /* Ordinal number 148, stack param usage 8 */
	{ "KeSetTimer", CALLARGS(KeSetTimerArgs,16) }, /* Ordinal number 149, stack param usage 16 */
	{ "KeSetTimerEx", CALLARGS(KeSetTimerExArgs,20) }, /* Ordinal number 150, stack param usage 20 */
	{ "KeStallExecutionProcessor", CALLARGS(KeStallExecutionProcessorArgs,4) }, /* Ordinal number 151, stack param usage 4 */
	{ "KeSuspendThread", CALLARGS(KeSuspendThreadArgs,4) }, /* Ordinal number 152, stack param usage 4 */
	{ "KeSynchronizeExecution", CALLARGS(KeSynchronizeExecutionArgs,12) }, /* Ordinal number 153, stack param usage 12 */
	{ "KeSystemTime", CALLARGS(KeSystemTimeArgs,0) }, /* Ordinal number 154, stack param usage 0 */
	{ "KeTestAlertThread", CALLARGS(KeTestAlertThreadArgs,4) }, /* Ordinal number 155, stack param usage 4 */
	{ "KeTickCount", CALLARGS(KeTickCountArgs,0) }, /* Ordinal number 156, stack param usage 0 */
	{ "KeTimeIncrement", CALLARGS(KeTimeIncrementArgs,0) }, /* Ordinal number 157, stack param usage 0 */
	{ "KeWaitForMultipleObjects", CALLARGS(KeWaitForMultipleObjectsArgs,32) }, /* Ordinal number 158, stack param usage 32 */
	{ "KeWaitForSingleObject", CALLARGS(KeWaitForSingleObjectArgs,20) }, /* Ordinal number 159, stack param usage 20 */
	{ "KfRaiseIrql", CALLARGS(KfRaiseIrqlArgs,4) }, /* Ordinal number 160, stack param usage 4 */
	{ "KfLowerIrql", CALLARGS(KfLowerIrqlArgs,4) }, /* Ordinal number 161, stack param usage 4 */
	{ "KiBugCheckData", CALLARGS(KiBugCheckDataArgs,0) }, /* Ordinal number 162, stack param usage 0 */
	{ "KiUnlockDispatcherDatabase", CALLARGS(KiUnlockDispatcherDatabaseArgs,4) }, /* Ordinal number 163, stack param usage 4 */
	{ "LaunchDataPage", CALLARGS(LaunchDataPageArgs,0) }, /* Ordinal number 164, stack param usage 0 */
	{ "MmAllocateContiguousMemory", CALLARGS(MmAllocateContiguousMemoryArgs,4) }, /* Ordinal number 165, stack param usage 4 */
	{ "MmAllocateContiguousMemoryEx", CALLARGS(MmAllocateContiguousMemoryExArgs,20) }, /* Ordinal number 166, stack param usage 20 */
	{ "MmAllocateSystemMemory", CALLARGS(MmAllocateSystemMemoryArgs,8) }, /* Ordinal number 167, stack param usage 8 */
	{ "MmClaimGpuInstanceMemory", CALLARGS(MmClaimGpuInstanceMemoryArgs,8) }, /* Ordinal number 168, stack param usage 8 */
	{ "MmCreateKernelStack", CALLARGS(MmCreateKernelStackArgs,8) }, /* Ordinal number 169, stack param usage 8 */
	{ "MmDeleteKernelStack", CALLARGS(MmDeleteKernelStackArgs,8) }, /* Ordinal number 170, stack param usage 8 */
	{ "MmFreeContiguousMemory", CALLARGS(MmFreeContiguousMemoryArgs,4) }, /* Ordinal number 171, stack param usage 4 */
	{ "MmFreeSystemMemory", CALLARGS(MmFreeSystemMemoryArgs,8) }, /* Ordinal number 172, stack param usage 8 */
	{ "MmGetPhysicalAddress", CALLARGS(MmGetPhysicalAddressArgs,4) }, /* Ordinal number 173, stack param usage 4 */
	{ "MmIsAddressValid", CALLARGS(MmIsAddressValidArgs,4) }, /* Ordinal number 174, stack param usage 4 */
	{ "MmLockUnlockBufferPages", CALLARGS(MmLockUnlockBufferPagesArgs,12) }, /* Ordinal number 175, stack param usage 12 */
	{ "MmLockUnlockPhysicalPage", CALLARGS(MmLockUnlockPhysicalPageArgs,8) }, /* Ordinal number 176, stack param usage 8 */
	{ "MmMapIoSpace", CALLARGS(MmMapIoSpaceArgs,12) }, /* Ordinal number 177, stack param usage 12 */
	{ "MmPersistContiguousMemory", CALLARGS(MmPersistContiguousMemoryArgs,12) }, /* Ordinal number 178, stack param usage 12 */
	{ "MmQueryAddressProtect", CALLARGS(MmQueryAddressProtectArgs,4) }, /* Ordinal number 179, stack param usage 4 */
	{ "MmQueryAllocationSize", CALLARGS(MmQueryAllocationSizeArgs,4) }, /* Ordinal number 180, stack param usage 4 */
	{ "MmQueryStatistics", CALLARGS(MmQueryStatisticsArgs,4) }, /* Ordinal number 181, stack param usage 4 */
	{ "MmSetAddressProtect", CALLARGS(MmSetAddressProtectArgs,12) }, /* Ordinal number 182, stack param usage 12 */
	{ "MmUnmapIoSpace", CALLARGS(MmUnmapIoSpaceArgs,8) }, /* Ordinal number 183, stack param usage 8 */
	{ "NtAllocateVirtualMemory", CALLARGS(NtAllocateVirtualMemoryArgs,20) }, /* Ordinal number 184, stack param usage 20 */
	{ "NtCancelTimer", CALLARGS(NtCancelTimerArgs,8) }, /* Ordinal number 185, stack param usage 8 */
	{ "NtClearEvent", CALLARGS(NtClearEventArgs,4) }, /* Ordinal number 186, stack param usage 4 */
	{ "NtClose", CALLARGS(NtCloseArgs,4) }, /* Ordinal number 187, stack param usage 4 */
	{ "NtCreateDirectoryObject", CALLARGS(NtCreateDirectoryObjectArgs,8) }, /* Ordinal number 188, stack param usage 8 */
	{ "NtCreateEvent", CALLARGS(NtCreateEventArgs,16) }, /* Ordinal number 189, stack param usage 16 */
	{ "NtCreateFile", CALLARGS(NtCreateFileArgs,36) }, /* Ordinal number 190, stack param usage 36 */
	{ "NtCreateIoCompletion", CALLARGS(NtCreateIoCompletionArgs,16) }, /* Ordinal number 191, stack param usage 16 */
	{ "NtCreateMutant", CALLARGS(NtCreateMutantArgs,12) }, /* Ordinal number 192, stack param usage 12 */
	{ "NtCreateSemaphore", CALLARGS(NtCreateSemaphoreArgs,16) }, /* Ordinal number 193, stack param usage 16 */
	{ "NtCreateTimer", CALLARGS(NtCreateTimerArgs,12) }, /* Ordinal number 194, stack param usage 12 */
	{ "NtDeleteFile", CALLARGS(NtDeleteFileArgs,4) }, /* Ordinal number 195, stack param usage 4 */
	{ "NtDeviceIoControlFile", CALLARGS(NtDeviceIoControlFileArgs,40) }, /* Ordinal number 196, stack param usage 40 */
	{ "NtDuplicateObject", CALLARGS(NtDuplicateObjectArgs,12) }, /* Ordinal number 197, stack param usage 12 */
	{ "NtFlushBuffersFile", CALLARGS(NtFlushBuffersFileArgs,8) }, /* Ordinal number 198, stack param usage 8 */
	{ "NtFreeVirtualMemory", CALLARGS(NtFreeVirtualMemoryArgs,12) }, /* Ordinal number 199, stack param usage 12 */
	{ "NtFsControlFile", CALLARGS(NtFsControlFileArgs,40) }, /* Ordinal number 200, stack param usage 40 */
	{ "NtOpenDirectoryObject", CALLARGS(NtOpenDirectoryObjectArgs,8) }, /* Ordinal number 201, stack param usage 8 */
	{ "NtOpenFile", CALLARGS(NtOpenFileArgs,24) }, /* Ordinal number 202, stack param usage 24 */
	{ "NtOpenSymbolicLinkObject", CALLARGS(NtOpenSymbolicLinkObjectArgs,8) }, /* Ordinal number 203, stack param usage 8 */
	{ "NtProtectVirtualMemory", CALLARGS(NtProtectVirtualMemoryArgs,16) }, /* Ordinal number 204, stack param usage 16 */
	{ "NtPulseEvent", CALLARGS(NtPulseEventArgs,8) }, /* Ordinal number 205, stack param usage 8 */
	{ "NtQueueApcThread", CALLARGS(NtQueueApcThreadArgs,20) }, /* Ordinal number 206, stack param usage 20 */
	{ "NtQueryDirectoryFile", CALLARGS(NtQueryDirectoryFileArgs,40) }, /* Ordinal number 207, stack param usage 40 */
	{ "NtQueryDirectoryObject", CALLARGS(NtQueryDirectoryObjectArgs,24) }, /* Ordinal number 208, stack param usage 24 */
	{ "NtQueryEvent", CALLARGS(NtQueryEventArgs,8) }, /* Ordinal number 209, stack param usage 8 */
	{ "NtQueryFullAttributesFile", CALLARGS(NtQueryFullAttributesFileArgs,8) }, /* Ordinal number 210, stack param usage 8 */
	{ "NtQueryInformationFile", CALLARGS(NtQueryInformationFileArgs,20) }, /* Ordinal number 211, stack param usage 20 */
	{ "NtQueryIoCompletion", CALLARGS(NtQueryIoCompletionArgs,8) }, /* Ordinal number 212, stack param usage 8 */
	{ "NtQueryMutant", CALLARGS(NtQueryMutantArgs,8) }, /* Ordinal number 213, stack param usage 8 */
	{ "NtQuerySemaphore", CALLARGS(NtQuerySemaphoreArgs,8) }, /* Ordinal number 214, stack param usage 8 */
	{ "NtQuerySymbolicLinkObject", CALLARGS(NtQuerySymbolicLinkObjectArgs,12) }, /* Ordinal number 215, stack param usage 12 */
	{ "NtQueryTimer", CALLARGS(NtQueryTimerArgs,8) }, /* Ordinal number 216, stack param usage 8 */
	{ "NtQueryVirtualMemory", CALLARGS(NtQueryVirtualMemoryArgs,8) }, /* Ordinal number 217, stack param usage 8 */
	{ "NtQueryVolumeInformationFile", CALLARGS(NtQueryVolumeInformationFileArgs,20) }, /* Ordinal number 218, stack param usage 20 */
	{ "NtReadFile", CALLARGS(NtReadFileArgs,32) }, /* Ordinal number 219, stack param usage 32 */
	{ "NtReadFileScatter", CALLARGS(NtReadFileScatterArgs,32) }, /* Ordinal number 220, stack param usage 32 */
	{ "NtReleaseMutant", CALLARGS(NtReleaseMutantArgs,8) }, /* Ordinal number 221, stack param usage 8 */
	{ "NtReleaseSemaphore", CALLARGS(NtReleaseSemaphoreArgs,12) }, /* Ordinal number 222, stack param usage 12 */
	{ "NtRemoveIoCompletion", CALLARGS(NtRemoveIoCompletionArgs,20) }, /* Ordinal number 223, stack param usage 20 */
	{ "NtResumeThread", CALLARGS(NtResumeThreadArgs,8) }, /* Ordinal number 224, stack param usage 8 */
	{ "NtSetEvent", CALLARGS(NtSetEventArgs,8) }, /* Ordinal number 225, stack param usage 8 */
	{ "NtSetInformationFile", CALLARGS(NtSetInformationFileArgs,20) }, /* Ordinal number 226, stack param usage 20 */
	{ "NtSetIoCompletion", CALLARGS(NtSetIoCompletionArgs,20) }, /* Ordinal number 227, stack param usage 20 */
	{ "NtSetSystemTime", CALLARGS(NtSetSystemTimeArgs,8) }, /* Ordinal number 228, stack param usage 8 */
	{ "NtSetTimerEx", CALLARGS(NtSetTimerExArgs,32) }, /* Ordinal number 229, stack param usage 32 */
	{ "NtSignalAndWaitForSingleObjectEx", CALLARGS(NtSignalAndWaitForSingleObjectExArgs,20) }, /* Ordinal number 230, stack param usage 20 */
	{ "NtSuspendThread", CALLARGS(NtSuspendThreadArgs,8) }, /* Ordinal number 231, stack param usage 8 */
	{ "NtUserIoApcDispatcher", CALLARGS(NtUserIoApcDispatcherArgs,12) }, /* Ordinal number 232, stack param usage 12 */
	{ "NtWaitForSingleObject", CALLARGS(NtWaitForSingleObjectArgs,12) }, /* Ordinal number 233, stack param usage 12 */
	{ "NtWaitForSingleObjectEx", CALLARGS(NtWaitForSingleObjectExArgs,16) }, /* Ordinal number 234, stack param usage 16 */
	{ "NtWaitForMultipleObjectsEx", CALLARGS(NtWaitForMultipleObjectsExArgs,24) }, /* Ordinal number 235, stack param usage 24 */
	{ "NtWriteFile", CALLARGS(NtWriteFileArgs,32) }, /* Ordinal number 236, stack param usage 32 */
	{ "NtWriteFileGather", CALLARGS(NtWriteFileGatherArgs,32) }, /* Ordinal number 237, stack param usage 32 */
	{ "NtYieldExecution", CALLARGS(NtYieldExecutionArgs,0) }, /* Ordinal number 238, stack param usage 0 */
	{ "ObCreateObject", CALLARGS(ObCreateObjectArgs,16) }, /* Ordinal number 239, stack param usage 16 */
	{ "ObDirectoryObjectType", CALLARGS(ObDirectoryObjectTypeArgs,0) }, /* Ordinal number 240, stack param usage 0 */
	{ "ObInsertObject", CALLARGS(ObInsertObjectArgs,16) }, /* Ordinal number 241, stack param usage 16 */
	{ "ObMakeTemporaryObject", CALLARGS(ObMakeTemporaryObjectArgs,4) }, /* Ordinal number 242, stack param usage 4 */
	{ "ObOpenObjectByName", CALLARGS(ObOpenObjectByNameArgs,16) }, /* Ordinal number 243, stack param usage 16 */
	{ "ObOpenObjectByPointer", CALLARGS(ObOpenObjectByPointerArgs,12) }, /* Ordinal number 244, stack param usage 12 */
	{ "ObpObjectHandleTable", CALLARGS(ObpObjectHandleTableArgs,0) }, /* Ordinal number 245, stack param usage 0 */
	{ "ObReferenceObjectByHandle", CALLARGS(ObReferenceObjectByHandleArgs,12) }, /* Ordinal number 246, stack param usage 12 */
	{ "ObReferenceObjectByName", CALLARGS(ObReferenceObjectByNameArgs,20) }, /* Ordinal number 247, stack param usage 20 */
	{ "ObReferenceObjectByPointer", CALLARGS(ObReferenceObjectByPointerArgs,8) }, /* Ordinal number 248, stack param usage 8 */
	{ "ObSymbolicLinkObjectType", CALLARGS(ObSymbolicLinkObjectTypeArgs,0) }, /* Ordinal number 249, stack param usage 0 */
	{ "ObfDereferenceObject", CALLARGS(ObfDereferenceObjectArgs,4) }, /* Ordinal number 250, stack param usage 4 */
	{ "ObfReferenceObject", CALLARGS(ObfReferenceObjectArgs,4) }, /* Ordinal number 251, stack param usage 4 */
	{ "PhyGetLinkState", CALLARGS(PhyGetLinkStateArgs,4) }, /* Ordinal number 252, stack param usage 4 */
	{ "PhyInitialize", CALLARGS(PhyInitializeArgs,8) }, /* Ordinal number 253, stack param usage 8 */
	{ "PsCreateSystemThread", CALLARGS(PsCreateSystemThreadArgs,20) }, /* Ordinal number 254, stack param usage 20 */
	{ "PsCreateSystemThreadEx", CALLARGS(PsCreateSystemThreadExArgs,40) }, /* Ordinal number 255, stack param usage 40 */
	{ "PsQueryStatistics", CALLARGS(PsQueryStatisticsArgs,4) }, /* Ordinal number 256, stack param usage 4 */
	{ "PsSetCreateThreadNotifyRoutine", CALLARGS(PsSetCreateThreadNotifyRoutineArgs,4) }, /* Ordinal number 257, stack param usage 4 */
	{ "PsTerminateSystemThread", CALLARGS(PsTerminateSystemThreadArgs,4) }, /* Ordinal number 258, stack param usage 4 */
	{ "PsThreadObjectType", CALLARGS(PsThreadObjectTypeArgs,0) }, /* Ordinal number 259, stack param usage 0 */
	{ "RtlAnsiStringToUnicodeString", CALLARGS(RtlAnsiStringToUnicodeStringArgs,12) }, /* Ordinal number 260, stack param usage 12 */
	{ "RtlAppendStringToString", CALLARGS(RtlAppendStringToStringArgs,8) }, /* Ordinal number 261, stack param usage 8 */
	{ "RtlAppendUnicodeStringToString", CALLARGS(RtlAppendUnicodeStringToStringArgs,8) }, /* Ordinal number 262, stack param usage 8 */
	{ "RtlAppendUnicodeToString", CALLARGS(RtlAppendUnicodeToStringArgs,8) }, /* Ordinal number 263, stack param usage 8 */
	{ "RtlAssert", CALLARGS(RtlAssertArgs,16) }, /* Ordinal number 264, stack param usage 16 */
	{ "RtlCaptureContext", CALLARGS(RtlCaptureContextArgs,4) }, /* Ordinal number 265, stack param usage 4 */
	{ "RtlCaptureStackBackTrace", CALLARGS(RtlCaptureStackBackTraceArgs,16) }, /* Ordinal number 266, stack param usage 16 */
	{ "RtlCharToInteger", CALLARGS(RtlCharToIntegerArgs,12) }, /* Ordinal number 267, stack param usage 12 */
	{ "RtlCompareMemory", CALLARGS(RtlCompareMemoryArgs,12) }, /* Ordinal number 268, stack param usage 12 */
	{ "RtlCompareMemoryUlong", CALLARGS(RtlCompareMemoryUlongArgs,12) }, /* Ordinal number 269, stack param usage 12 */
	{ "RtlCompareString", CALLARGS(RtlCompareStringArgs,12) }, /* Ordinal number 270, stack param usage 12 */
	{ "RtlCompareUnicodeString", CALLARGS(RtlCompareUnicodeStringArgs,12) }, /* Ordinal number 271, stack param usage 12 */
	{ "RtlCopyString", CALLARGS(RtlCopyStringArgs,8) }, /* Ordinal number 272, stack param usage 8 */
	{ "RtlCopyUnicodeString", CALLARGS(RtlCopyUnicodeStringArgs,8) }, /* Ordinal number 273, stack param usage 8 */
	{ "RtlCreateUnicodeString", CALLARGS(RtlCreateUnicodeStringArgs,8) }, /* Ordinal number 274, stack param usage 8 */
	{ "RtlDowncaseUnicodeChar", CALLARGS(RtlDowncaseUnicodeCharArgs,4) }, /* Ordinal number 275, stack param usage 4 */
	{ "RtlDowncaseUnicodeString", CALLARGS(RtlDowncaseUnicodeStringArgs,12) }, /* Ordinal number 276, stack param usage 12 */
	{ "RtlEnterCriticalSection", CALLARGS(RtlEnterCriticalSectionArgs,4) }, /* Ordinal number 277, stack param usage 4 */
	{ "RtlEnterCriticalSectionAndRegion", CALLARGS(RtlEnterCriticalSectionAndRegionArgs,4) }, /* Ordinal number 278, stack param usage 4 */
	{ "RtlEqualString", CALLARGS(RtlEqualStringArgs,12) }, /* Ordinal number 279, stack param usage 12 */
	{ "RtlEqualUnicodeString", CALLARGS(RtlEqualUnicodeStringArgs,12) }, /* Ordinal number 280, stack param usage 12 */
	{ "RtlExtendedIntegerMultiply", CALLARGS(RtlExtendedIntegerMultiplyArgs,12) }, /* Ordinal number 281, stack param usage 12 */
	{ "RtlExtendedLargeIntegerDivide", CALLARGS(RtlExtendedLargeIntegerDivideArgs,16) }, /* Ordinal number 282, stack param usage 16 */
	{ "RtlExtendedMagicDivide", CALLARGS(RtlExtendedMagicDivideArgs,20) }, /* Ordinal number 283, stack param usage 20 */
	{ "RtlFillMemory", CALLARGS(RtlFillMemoryArgs,12) }, /* Ordinal number 284, stack param usage 12 */
	{ "RtlFillMemoryUlong", CALLARGS(RtlFillMemoryUlongArgs,12) }, /* Ordinal number 285, stack param usage 12 */
	{ "RtlFreeAnsiString", CALLARGS(RtlFreeAnsiStringArgs,4) }, /* Ordinal number 286, stack param usage 4 */
	{ "RtlFreeUnicodeString", CALLARGS(RtlFreeUnicodeStringArgs,4) }, /* Ordinal number 287, stack param usage 4 */
	{ "RtlGetCallersAddress", CALLARGS(RtlGetCallersAddressArgs,8) }, /* Ordinal number 288, stack param usage 8 */
	{ "RtlInitAnsiString", CALLARGS(RtlInitAnsiStringArgs,8) }, /* Ordinal number 289, stack param usage 8 */
	{ "RtlInitUnicodeString", CALLARGS(RtlInitUnicodeStringArgs,8) }, /* Ordinal number 290, stack param usage 8 */
	{ "RtlInitializeCriticalSection", CALLARGS(RtlInitializeCriticalSectionArgs,4) }, /* Ordinal number 291, stack param usage 4 */
	{ "RtlIntegerToChar", CALLARGS(RtlIntegerToCharArgs,16) }, /* Ordinal number 292, stack param usage 16 */
	{ "RtlIntegerToUnicodeString", CALLARGS(RtlIntegerToUnicodeStringArgs,12) }, /* Ordinal number 293, stack param usage 12 */
	{ "RtlLeaveCriticalSection", CALLARGS(RtlLeaveCriticalSectionArgs,4) }, /* Ordinal number 294, stack param usage 4 */
	{ "RtlLeaveCriticalSectionAndRegion", CALLARGS(RtlLeaveCriticalSectionAndRegionArgs,4) }, /* Ordinal number 295, stack param usage 4 */
	{ "RtlLowerChar", CALLARGS(RtlLowerCharArgs,4) }, /* Ordinal number 296, stack param usage 4 */
	{ "RtlMapGenericMask", CALLARGS(RtlMapGenericMaskArgs,8) }, /* Ordinal number 297, stack param usage 8 */
	{ "RtlMoveMemory", CALLARGS(RtlMoveMemoryArgs,12) }, /* Ordinal number 298, stack param usage 12 */
	{ "RtlMultiByteToUnicodeN", CALLARGS(RtlMultiByteToUnicodeNArgs,20) }, /* Ordinal number 299, stack param usage 20 */
	{ "RtlMultiByteToUnicodeSize", CALLARGS(RtlMultiByteToUnicodeSizeArgs,12) }, /* Ordinal number 300, stack param usage 12 */
	{ "RtlNtStatusToDosError", CALLARGS(RtlNtStatusToDosErrorArgs,4) }, /* Ordinal number 301, stack param usage 4 */
	{ "RtlRaiseException", CALLARGS(RtlRaiseExceptionArgs,4) }, /* Ordinal number 302, stack param usage 4 */
	{ "RtlRaiseStatus", CALLARGS(RtlRaiseStatusArgs,4) }, /* Ordinal number 303, stack param usage 4 */
	{ "RtlTimeFieldsToTime", CALLARGS(RtlTimeFieldsToTimeArgs,8) }, /* Ordinal number 304, stack param usage 8 */
	{ "RtlTimeToTimeFields", CALLARGS(RtlTimeToTimeFieldsArgs,8) }, /* Ordinal number 305, stack param usage 8 */
	{ "RtlTryEnterCriticalSection", CALLARGS(RtlTryEnterCriticalSectionArgs,4) }, /* Ordinal number 306, stack param usage 4 */
	{ "RtlUlongByteSwap", CALLARGS(RtlUlongByteSwapArgs,4) }, /* Ordinal number 307, stack param usage 4 */
	{ "RtlUnicodeStringToAnsiString", CALLARGS(RtlUnicodeStringToAnsiStringArgs,12) }, /* Ordinal number 308, stack param usage 12 */
	{ "RtlUnicodeStringToInteger", CALLARGS(RtlUnicodeStringToIntegerArgs,12) }, /* Ordinal number 309, stack param usage 12 */
	{ "RtlUnicodeToMultiByteN", CALLARGS(RtlUnicodeToMultiByteNArgs,20) }, /* Ordinal number 310, stack param usage 20 */
	{ "RtlUnicodeToMultiByteSize", CALLARGS(RtlUnicodeToMultiByteSizeArgs,12) }, /* Ordinal number 311, stack param usage 12 */
	{ "RtlUnwind", CALLARGS(RtlUnwindArgs,16) }, /* Ordinal number 312, stack param usage 16 */
	{ "RtlUpcaseUnicodeChar", CALLARGS(RtlUpcaseUnicodeCharArgs,4) }, /* Ordinal number 313, stack param usage 4 */
	{ "RtlUpcaseUnicodeString", CALLARGS(RtlUpcaseUnicodeStringArgs,12) }, /* Ordinal number 314, stack param usage 12 */
	{ "RtlUpcaseUnicodeToMultiByteN", CALLARGS(RtlUpcaseUnicodeToMultiByteNArgs,20) }, /* Ordinal number 315, stack param usage 20 */
	{ "RtlUpperChar", CALLARGS(RtlUpperCharArgs,4) }, /* Ordinal number 316, stack param usage 4 */
	{ "RtlUpperString", CALLARGS(RtlUpperStringArgs,8) }, /* Ordinal number 317, stack param usage 8 */
	{ "RtlUshortByteSwap", CALLARGS(RtlUshortByteSwapArgs,4) }, /* Ordinal number 318, stack param usage 4 */
	{ "RtlWalkFrameChain", CALLARGS(RtlWalkFrameChainArgs,12) }, /* Ordinal number 319, stack param usage 12 */
	{ "RtlZeroMemory", CALLARGS(RtlZeroMemoryArgs,8) }, /* Ordinal number 320, stack param usage 8 */
	{ "XboxEEPROMKey", CALLARGS(XboxEEPROMKeyArgs,0) }, /* Ordinal number 321, stack param usage 0 */
	{ "XboxHardwareInfo", CALLARGS(XboxHardwareInfoArgs,0) }, /* Ordinal number 322, stack param usage 0 */
	{ "XboxHDKey", CALLARGS(XboxHDKeyArgs,0) }, /* Ordinal number 323, stack param usage 0 */
	{ "XboxKrnlVersion", CALLARGS(XboxKrnlVersionArgs,0) }, /* Ordinal number 324, stack param usage 0 */
	{ "XboxSignatureKey", CALLARGS(XboxSignatureKeyArgs,0) }, /* Ordinal number 325, stack param usage 0 */
	{ "XeImageFileName", CALLARGS(XeImageFileNameArgs,0) }, /* Ordinal number 326, stack param usage 0 */
	{ "XeLoadSection", CALLARGS(XeLoadSectionArgs,4) }, /* Ordinal number 327, stack param usage 4 */
	{ "XeUnloadSection", CALLARGS(XeUnloadSectionArgs,4) }, /* Ordinal number 328, stack param usage 4 */
	{ "READ_PORT_BUFFER_UCHAR", CALLARGS(READ_PORT_BUFFER_UCHARArgs,12) }, /* Ordinal number 329, stack param usage 12 */
	{ "READ_PORT_BUFFER_USHORT", CALLARGS(READ_PORT_BUFFER_USHORTArgs,12) }, /* Ordinal number 330, stack param usage 12 */
	{ "READ_PORT_BUFFER_ULONG", CALLARGS(READ_PORT_BUFFER_ULONGArgs,12) }, /* Ordinal number 331, stack param usage 12 */
	{ "WRITE_PORT_BUFFER_UCHAR", CALLARGS(WRITE_PORT_BUFFER_UCHARArgs,12) }, /* Ordinal number 332, stack param usage 12 */
	{ "WRITE_PORT_BUFFER_USHORT", CALLARGS(WRITE_PORT_BUFFER_USHORTArgs,12) }, /* Ordinal number 333, stack param usage 12 */
	{ "WRITE_PORT_BUFFER_ULONG", CALLARGS(WRITE_PORT_BUFFER_ULONGArgs,12) }, /* Ordinal number 334, stack param usage 12 */
	{ "XcSHAInit", CALLARGS(XcSHAInitArgs,4) }, /* Ordinal number 335, stack param usage 4 */
	{ "XcSHAUpdate", CALLARGS(XcSHAUpdateArgs,12) }, /* Ordinal number 336, stack param usage 12 */
	{ "XcSHAFinal", CALLARGS(XcSHAFinalArgs,8) }, /* Ordinal number 337, stack param usage 8 */
	{ "XcRC4Key", CALLARGS(XcRC4KeyArgs,12) }, /* Ordinal number 338, stack param usage 12 */
	{ "XcRC4Crypt", CALLARGS(XcRC4CryptArgs,12) }, /* Ordinal number 339, stack param usage 12 */
	{ "XcHMAC", CALLARGS(XcHMACArgs,28) }, /* Ordinal number 340, stack param usage 28 */
	{ "XcPKEncPublic", CALLARGS(XcPKEncPublicArgs,12) }, /* Ordinal number 341, stack param usage 12 */
	{ "XcPKDecPrivate", CALLARGS(XcPKDecPrivateArgs,12) }, /* Ordinal number 342, stack param usage 12 */
	{ "XcPKGetKeyLen", CALLARGS(XcPKGetKeyLenArgs,4) }, /* Ordinal number 343, stack param usage 4 */
	{ "XcVerifyPKCS1Signature", CALLARGS(XcVerifyPKCS1SignatureArgs,12) }, /* Ordinal number 344, stack param usage 12 */
	{ "XcModExp", CALLARGS(XcModExpArgs,20) }, /* Ordinal number 345, stack param usage 20 */
	{ "XcDESKeyParity", CALLARGS(XcDESKeyParityArgs,8) }, /* Ordinal number 346, stack param usage 8 */
	{ "XcKeyTable", CALLARGS(XcKeyTableArgs,12) }, /* Ordinal number 347, stack param usage 12 */
	{ "XcBlockCrypt", CALLARGS(XcBlockCryptArgs,20) }, /* Ordinal number 348, stack param usage 20 */
	{ "XcBlockCryptCBC", CALLARGS(XcBlockCryptCBCArgs,28) }, /* Ordinal number 349, stack param usage 28 */
	{ "XcCryptService", CALLARGS(XcCryptServiceArgs,8) }, /* Ordinal number 350, stack param usage 8 */
	{ "XcUpdateCrypto", CALLARGS(XcUpdateCryptoArgs,8) }, /* Ordinal number 351, stack param usage 8 */
	{ "RtlRip", CALLARGS(RtlRipArgs,12) }, /* Ordinal number 352, stack param usage 12 */
	{ "XboxLANKey", CALLARGS(XboxLANKeyArgs,0) }, /* Ordinal number 353, stack param usage 0 */
	{ "XboxAlternateSignatureKeys", CALLARGS(XboxAlternateSignatureKeysArgs,0) }, /* Ordinal number 354, stack param usage 0 */
	{ "XePublicKeyData", CALLARGS(XePublicKeyDataArgs,0) }, /* Ordinal number 355, stack param usage 0 */
	{ "HalBootSMCVideoMode", CALLARGS(HalBootSMCVideoModeArgs,0) }, /* Ordinal number 356, stack param usage 0 */
	{ "IdexChannelObject", CALLARGS(IdexChannelObjectArgs,0) }, /* Ordinal number 357, stack param usage 0 */
	{ "HalIsResetOrShutdownPending", CALLARGS(HalIsResetOrShutdownPendingArgs,0) }, /* Ordinal number 358, stack param usage 0 */
	{ "IoMarkIrpMustComplete", CALLARGS(IoMarkIrpMustCompleteArgs,4) }, /* Ordinal number 359, stack param usage 4 */
	{ "HalInitiateShutdown", CALLARGS(HalInitiateShutdownArgs,0) }, /* Ordinal number 360, stack param usage 0 */
	{ "RtlSnprintf", CALLARGS(RtlSnprintfArgs,0) }, /* Ordinal number 361, stack param usage 0 */
	{ "RtlSprintf", CALLARGS(RtlSprintfArgs,0) }, /* Ordinal number 362, stack param usage 0 */
	{ "RtlVsnprintf", CALLARGS(RtlVsnprintfArgs,0) }, /* Ordinal number 363, stack param usage 0 */
	{ "RtlVsprintf", CALLARGS(RtlVsprintfArgs,0) }, /* Ordinal number 364, stack param usage 0 */
	{ "HalEnableSecureTrayEject", CALLARGS(HalEnableSecureTrayEjectArgs,0) }, /* Ordinal number 365, stack param usage 0 */
	{ "HalWriteSMCScratchRegister", CALLARGS(HalWriteSMCScratchRegisterArgs,4) }, /* Ordinal number 366, stack param usage 4 */
	{ "MmDbgAllocateMemory", CALLARGS(MmDbgAllocateMemoryArgs,8) }, /* Ordinal number 374, stack param usage 8 */
	{ "MmDbgFreeMemory", CALLARGS(MmDbgFreeMemoryArgs,8) }, /* Ordinal number 375, stack param usage 8 */
	{ "MmDbgQueryAvailablePages", CALLARGS(MmDbgQueryAvailablePagesArgs,0) }, /* Ordinal number 376, stack param usage 0 */
	{ "MmDbgReleaseAddress", CALLARGS(MmDbgReleaseAddressArgs,8) }, /* Ordinal number 377, stack param usage 8 */
	{ "MmDbgWriteCheck", CALLARGS(MmDbgWriteCheckArgs,8) } /* Ordinal number 378, stack param usage 8 */
};
#define TOTALKERNELCALLS (sizeof(aKernelCalls)/sizeof(PARAMDETAILS))


/* ACCESS MASK flags. */
const static NAME_VALUE_PAIR caACCESS_MASKFlags[] = {
	{ "DELETE", 0x00010000 },
	{ "READ_CONTROL", 0x00020000 },
	{ "SYNCHRONIZE", 0x00100000 },
	{ "GENERIC_ALL", 0x10000000 },
	{ "GENERIC_EXECUTE", 0x20000000 },
	{ "GENERIC_WRITE", 0x40000000 },
	{ "GENERIC_READ", 0x80000000 },
	{ "FILE_SHARE_READ", 0x00000001 },
	{ "FILE_SHARE_WRITE", 0x00000002 },
	{ "FILE_SHARE_DELETE", 0x00000004 },
	{ "FILE_ATTRIBUTE_NORMAL", 0x00000080 },
	{ "FILE_FLAG_RANDOM_ACCESS", 0x10000000 }
};

/* CreateDisposition values for NtCreateFile(). */
const static NAME_VALUE_PAIR caCreateDispositionFlags[] = {
/*	{ "FILE_SUPERSEDE", 0x00000000 }, */
	{ "FILE_OPEN", 0x00000001 },
	{ "FILE_CREATE", 0x00000002 },
	{ "FILE_OPEN_IF", 0x00000003 },
	{ "FILE_OVERWRITE", 0x00000004 },
	{ "FILE_OVERWRITE_IF", 0x00000005 },
	{ "FILE_MAXIMUM_DISPOSITION", 0x00000005 }
};

/* Create options flags. */
const static NAME_VALUE_PAIR caCreateOptionsFlags[] = {
	{ "FILE_DIRECTORY_FILE", 0x00000001 },
	{ "FILE_WRITE_THROUGH", 0x00000002 },
	{ "FILE_SEQUENTIAL_ONLY", 0x00000004 },
	{ "FILE_NO_INTERMEDIATE_BUFFERING", 0x00000008 },
	{ "FILE_SYNCHRONOUS_IO_ALERT", 0x00000010 },
	{ "FILE_SYNCHRONOUS_IO_NONALERT", 0x00000020 },
	{ "FILE_NON_DIRECTORY_FILE", 0x00000040 },
	{ "FILE_CREATE_TREE_CONNECTION", 0x00000080 },
	{ "FILE_COMPLETE_IF_OPLOCKED", 0x00000100 },
	{ "FILE_NO_EA_KNOWLEDGE", 0x00000200 },
	{ "FILE_OPEN_FOR_RECOVERY", 0x00000400 },
	{ "FILE_RANDOM_ACCESS", 0x00000800 },
	{ "FILE_DELETE_ON_CLOSE", 0x00001000 },
	{ "FILE_OPEN_BY_FILE_ID", 0x00002000 },
	{ "FILE_OPEN_FOR_BACKUP_INTENT", 0x00004000 },
	{ "FILE_NO_COMPRESSION", 0x00008000 },
	{ "FILE_RESERVE_OPFILTER", 0x00100000 },
	{ "FILE_OPEN_REPARSE_POINT", 0x00200000 },
	{ "FILE_OPEN_NO_RECALL", 0x00400000 },
	{ "FILE_OPEN_FOR_FREE_SPACE_QUERY", 0x00800000 },
	{ "FILE_COPY_STRUCTURED_STORAGE", 0x00000041 },
	{ "FILE_STRUCTURED_STORAGE", 0x00000441 },
	{ "FILE_VALID_OPTION_FLAGS", 0x00ffffff },
	{ "FILE_VALID_PIPE_OPTION_FLAGS", 0x00000032 },
	{ "FILE_VALID_MAILSLOT_OPTION_FLAGS", 0x00000032 },
	{ "FILE_VALID_SET_FLAGS", 0x00000036 }
};

/* ObjectAttributes 'Attributes' flags. */
const static NAME_VALUE_PAIR caOAAttributesFlags[] = {
	{ "OBJ_INHERIT", 0x00000002 },
	{ "OBJ_PERMANENT", 0x00000010 },
	{ "OBJ_EXCLUSIVE", 0x00000020 },
	{ "OBJ_CASE_INSENSITIVE", 0x00000040 },
	{ "OBJ_OPENIF", 0x00000080 },
	{ "OBJ_OPENLINK", 0x00000100 }
/*	{ "OBJ_VALID_ATTRIBUTES", 0x000001F2 } */
};

/* FILE_INFORMATION_CLASS enum (as used in NtSetInformationFile(). */
const static NAME_VALUE_PAIR caFILE_INFORMATION_CLASSEnum[] = {
	{ "FileDirectoryInformation", 1},
	{ "FileFullDirectoryInformation", 2},
	{ "FileBothDirectoryInformation", 3},
	{ "FileBasicInformation", 4},
	{ "FileStandardInformation", 5},
	{ "FileInternalInformation", 6},
	{ "FileEaInformation", 7},
	{ "FileAccessInformation", 8},
	{ "FileNameInformation", 9},
	{ "FileRenameInformation", 10},
	{ "FileLinkInformation", 11},
	{ "FileNamesInformation", 12},
	{ "FileDispositionInformation", 13},
	{ "FilePositionInformation", 14},
	{ "FileFullEaInformation", 15},
	{ "FileModeInformation", 16},
	{ "FileAlignmentInformation", 17},
	{ "FileAllInformation", 18},
	{ "FileAllocationInformation", 19},
	{ "FileEndOfFileInformation", 20},
	{ "FileAlternateNameInformation", 21},
	{ "FileStreamInformation", 22},
	{ "FilePipeInformation", 23},
	{ "FilePipeLocalInformation", 24},
	{ "FilePipeRemoteInformation", 25},
	{ "FileMailslotQueryInformation", 26},
	{ "FileMailslotSetInformation", 27},
	{ "FileCompressionInformation", 28},
	{ "FileCopyOnWriteInformation", 29},
	{ "FileCompletionInformation", 30},
	{ "FileMoveClusterInformation", 31},
	{ "FileQuotaInformation", 32},
	{ "FileReparsePointInformation", 33},
	{ "FileNetworkOpenInformation", 34},
	{ "FileObjectIdInformation", 35},
	{ "FileTrackingInformation", 36},
	{ "FileOleDirectoryInformation", 37},
	{ "FileContentIndexInformation", 38},
	{ "FileInheritContentIndexInformation", 39},
	{ "FileOleInformation", 40},
	{ "FileMaximumInformation", 41}
};

/**************** Functions *****************/

void OutputArgs(const ARGDETAILS* pArgDetails,
	int nNumArgs,
	const u8* pRecordBuffer,
	u32 dwRecordLength );

void OutputChar(char cChar) {
	if( bOutputOn )
		szOutputBuffer[iOutputBufferIndex++] = cChar;
}

void OutputString(const char* pszString) {
	while(*pszString)
		OutputChar(*pszString++);
}

void OutputQuotedString(const char* pszString) {
	int bOldOutputOn;

	bOldOutputOn = bOutputOn;
	bOutputOn = 1;

	OutputChar('"'); OutputString(pszString); OutputChar('"');
	bPrnString = 1;

	bOutputOn = bOldOutputOn;
}

int OutputU8( u8 byByte ) {
	static char szHex[] = "0123456789ABCDEF";
	OutputChar( szHex[(byByte >> 4) & 0x0F] );
	OutputChar( szHex[byByte & 0x0F] );
	return sizeof(u8);
}

int OutputU16(const unsigned char* pRecordHeader) {
	OutputU8( pRecordHeader[1] );
	OutputU8( pRecordHeader[0] );
	return sizeof(u16);
}

int OutputU32(const u8* pRecordHeader) {
	OutputU16( &pRecordHeader[2] );
	OutputU16( &pRecordHeader[0] );
	return sizeof(u32);
}

int OutputBYTE(const u8* pRecordHeader) {
	OutputString( pszHexPrefix );
	return OutputU8( pRecordHeader[0] );
}

int OutputWORD(const u8* pRecordHeader) {
	OutputString( pszHexPrefix );
	return OutputU16( pRecordHeader );
}

int OutputDWORD(const u8* pRecordHeader) {
	OutputString( pszHexPrefix );
	return OutputU32( pRecordHeader );
}

int OutputHexDump(const u8 *pBuffer, u32 dwLength ) {
	int nConsumed = 0;
	while( dwLength-- )
	{
		nConsumed += OutputU8(*pBuffer++);
		if(!(nConsumed % 4))
			OutputChar(' ');
	}
	return nConsumed;
}

void OutputBitPatternFunc(u32 dwValue,
	const NAME_VALUE_PAIR caNVPair[],
	int iTotalPairs )
{
	int i, bOutput = 0;

	OutputChar('(');
	for(i=0; i<iTotalPairs; ++i)
	{
		if( ( dwValue & caNVPair[i].dwValue ) == caNVPair[i].dwValue )
		{
			if(bOutput)
				OutputChar('|');
			OutputString(caNVPair[i].pszName);
			bOutput = 1;
		}
	}
	OutputChar(')');
}

void OutputEnumFunc(u32 dwValue,
	const NAME_VALUE_PAIR caNVPair[],
	int iTotalPairs )
{
	int i;

	OutputChar('(');
	for(i=0; i<iTotalPairs; ++i)
	{
		if( dwValue == caNVPair[i].dwValue )
		{
			OutputString(caNVPair[i].pszName);
			break;
		}
	}
	OutputChar(')');
}

const u8* ReadRecord(u32 *pdwRecordLength)
{
	u8* pBuffer = 0;

	fread(pdwRecordLength, sizeof(*pdwRecordLength), 1, fp);

	pBuffer = (u8*) malloc(*pdwRecordLength + 1);
	if(pBuffer)
	{
		if( *pdwRecordLength )
			fread(pBuffer, 1, *pdwRecordLength, fp);
		pBuffer[*pdwRecordLength] = '\000';	/* Zero terminate records */
	}
	return pBuffer;
}

void OutputSTRING()
{
	unsigned long dwRecordLength;
	const u8* pszString;
	int bOldOutputOn;

	bOldOutputOn = bOutputOn;
	bOutputOn = 1;

	OutputChar('{');
	pszString = ReadRecord(&dwRecordLength);
	OutputQuotedString(pszString);
	free((void*)pszString);
	OutputChar('}');

	bOutputOn = bOldOutputOn;
}

void OutputStructure(const ARGDETAILS* pMembers, int nTotalMembers)
{
	u32 dwRecordLength;
	const u8* pBuffer;

	OutputChar('{');
	pBuffer = ReadRecord(&dwRecordLength);
	OutputArgs(pMembers, nTotalMembers,pBuffer,dwRecordLength);
	free((void*)pBuffer);
	OutputChar('}');
}

int OutputArg(const ARGDETAILS* pArgDetails,
	const u8* pRecordBuffer,
	u32 dwRecordLength )
{
	int nConsumed = 0;

	if(pArgDetails->pszLegend)
	{
		OutputString(pArgDetails->pszLegend);
		OutputChar('=');
	}

	switch(pArgDetails->dt)
	{
		case dtSTRING:
			nConsumed = OutputDWORD(pRecordBuffer);
			if(*((u32 *)pRecordBuffer))
				OutputSTRING();
		break;

		case dtU8:
			nConsumed = OutputBYTE(pRecordBuffer);
		break;

		case dtU16:
			nConsumed = OutputWORD(pRecordBuffer);
		break;

		case dtU32:
			nConsumed = OutputDWORD(pRecordBuffer);
		break;

		case dtObjectAttributes:
			nConsumed = OutputDWORD(pRecordBuffer);
			if(*((u32 *)pRecordBuffer))
				OutputStructure(OBJECT_ATTRIBUTESMembers, sizeof(OBJECT_ATTRIBUTESMembers)/sizeof(ARGDETAILS));
		break;

		case dtACCESS_MASK:
			nConsumed = OutputDWORD(pRecordBuffer);
			OutputBitPattern(*((unsigned long*)pRecordBuffer),caACCESS_MASKFlags);
		break;

		case dtOAAttributes:
			nConsumed = OutputDWORD(pRecordBuffer);
			OutputBitPattern(*((unsigned long*)pRecordBuffer),caOAAttributesFlags);
		break;

		case dtANSI_STRING:
			nConsumed = OutputDWORD(pRecordBuffer);
			if(*((u32 *)pRecordBuffer))
				OutputStructure(ANSI_STRINGMembers, sizeof(ANSI_STRINGMembers)/sizeof(ARGDETAILS));
		break;

		case dtCreateDisposition:
			nConsumed = OutputDWORD(pRecordBuffer);
			OutputBitPattern(*((unsigned long*)pRecordBuffer),caCreateDispositionFlags);
		break;

		case dtCreateOptions:
			nConsumed = OutputDWORD(pRecordBuffer);
			OutputBitPattern(*((unsigned long*)pRecordBuffer),caCreateOptionsFlags);
		break;

		case dtFILE_INFORMATION_CLASSEnum:
			nConsumed = OutputDWORD(pRecordBuffer);
			OutputEnum(*((unsigned long*)pRecordBuffer),caFILE_INFORMATION_CLASSEnum);
		break;

		default:	/* BLOB */
			nConsumed = OutputHexDump(pRecordBuffer,dwRecordLength);
		break;
	}

	return nConsumed;
}

void OutputArgs(const ARGDETAILS* pArgDetails,
	int nNumArgs,
	const u8* pRecordBuffer,
	u32 dwRecordLength )
{
	const static ARGDETAILS BLOBArgs = {pszUnknownArgs,dtBLOB};

	int i,nConsumed,bOldOutputOn;

	bOldOutputOn = bOutputOn;
	if( bStringsOnly )
		bOutputOn = 0;

	for(i = 0; i<nNumArgs; ++i)
	{
		nConsumed = OutputArg(&pArgDetails[i],pRecordBuffer,dwRecordLength);
		pRecordBuffer += nConsumed;
		dwRecordLength -= nConsumed;
		if(dwRecordLength)
			OutputChar(',');
	}

	if(dwRecordLength)
		OutputArg(&BLOBArgs,pRecordBuffer,dwRecordLength);

	bOutputOn = bOldOutputOn;
}

int main(int argc, char* argv[])
{
	const static char* pszUsage = "\
Pedro's APIReporter - Version 2.0\n\
Usage: APIReporter <filename> [offset (in Hex)] [/STRINGS][>APIReport.txt]\n";

	if(argc >= 2)
	{
		fp = fopen(argv[1],"rb");
		if( fp )
		{
			u8 recordNum[4], threadID[4], aEAX[4];
			const u8* pRecordBuffer;
			u32 dwOrdinalNum, dwRecordLength = 0;
			const PARAMDETAILS* pKernelCallDetails;
			int nConsumed;

			if(argc == 3)
			{
				long lOffset = 0;

				sscanf(argv[2],"%lX", &lOffset );
				fseek(fp,lOffset,SEEK_SET);
			}

			if(argc == 4)	/* /STRINGS */
				bStringsOnly = 1;

			while(fread(recordNum, sizeof(recordNum),1,fp))
			{
				iOutputBufferIndex = 0;	/* Clear flags. */
				bPrnString = 0;

				OutputDWORD(recordNum); OutputChar(' '); /* Record number. */

				fread(threadID, sizeof(threadID), 1, fp);
				OutputDWORD(threadID); OutputChar(' '); /* Thread ID. */

				fread(&dwOrdinalNum, sizeof(dwOrdinalNum), 1, fp);
				if(dwOrdinalNum == 0 || dwOrdinalNum-1>TOTALKERNELCALLS)
					break;

				/* Get the details for this kernel call. */
				pKernelCallDetails = &aKernelCalls[dwOrdinalNum - 1];

				/* Read in stack arguments. */
				if(pRecordBuffer = ReadRecord(&dwRecordLength))
				{
					nConsumed = OutputDWORD(pRecordBuffer); OutputChar(' '); /* Call's return address. */
					dwRecordLength-=nConsumed;

					OutputString(pKernelCallDetails->pszLegend); OutputChar('(');
					if(pKernelCallDetails->nNumArgs || dwRecordLength)
						OutputArgs(pKernelCallDetails->pArgDetails,pKernelCallDetails->nNumArgs,pRecordBuffer+nConsumed,dwRecordLength);
					OutputChar(')'); OutputChar(';');

					free((void*)pRecordBuffer);
				}

#if 0
				/* Output returned value. */
				fread(aEAX, sizeof(aEAX), 1, fp);
				OutputString(pszEAX); OutputDWORD(aEAX); /* EAX value returned. */
#endif

				szOutputBuffer[iOutputBufferIndex] = '\000';	/* Terminate string. */

				if( !bStringsOnly ||
					bPrnString )
					puts(szOutputBuffer);
			}

			fclose(fp);
		}
	}
	else
		OutputString(pszUsage);
}
