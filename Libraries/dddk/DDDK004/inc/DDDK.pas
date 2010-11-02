//
// all DDDK drivers should include this unit
// this unit exports all currently supported kernel function, structures and constants
//
unit DDDK;

interface

const
 NtKernel='ntoskrnl.exe';

 STATUS_SUCCESS=0;
 STATUS_UNSUCCESSFUL            = $C0000001;
 STATUS_NOT_IMPLEMENTED         = $C0000002;
 STATUS_INVALID_INFO_CLASS      = $C0000003;
 STATUS_INFO_LENGTH_MISMATCH    = $C0000004;
 STATUS_ACCESS_VIOLATION        = $C0000005;
 STATUS_IN_PAGE_ERROR           = $C0000006;
 STATUS_PAGEFILE_QUOTA          = $C0000007;
 STATUS_INVALID_HANDLE          = $C0000008;
 STATUS_BAD_INITIAL_STACK       = $C0000009;
 STATUS_BAD_INITIAL_PC          = $C000000A;
 STATUS_INVALID_CID             = $C000000B;
 STATUS_TIMER_NOT_CANCELED      = $C000000C;
 STATUS_INVALID_PARAMETER       = $C000000D;
 STATUS_NO_SUCH_DEVICE          = $C000000E;
 STATUS_NO_SUCH_FILE            = $C000000F;
 STATUS_INVALID_DEVICE_REQUEST  = $C0000010;

 IRP_MJ_CREATE                  = $00;
 IRP_MJ_CREATE_NAMED_PIPE       = $01;
 IRP_MJ_CLOSE                   = $02;
 IRP_MJ_READ                    = $03;
 IRP_MJ_WRITE                   = $04;
 IRP_MJ_QUERY_INFORMATION       = $05;
 IRP_MJ_SET_INFORMATION         = $06;
 IRP_MJ_QUERY_EA                = $07;
 IRP_MJ_SET_EA                  = $08;
 IRP_MJ_FLUSH_BUFFERS           = $09;
 IRP_MJ_QUERY_VOLUME_INFORMATION= $0A;
 IRP_MJ_SET_VOLUME_INFORMATION  = $0B;
 IRP_MJ_DIRECTORY_CONTROL       = $0C;
 IRP_MJ_FILE_SYSTEM_CONTROL     = $0D;
 IRP_MJ_DEVICE_CONTROL          = $0E;
 IRP_MJ_INTERNAL_DEVICE_CONTROL = $0F;
 IRP_MJ_SHUTDOWN                = $10;
 IRP_MJ_LOCK_CONTROL            = $11;
 IRP_MJ_CLEANUP                 = $12;
 IRP_MJ_CREATE_MAILSLOT         = $13;
 IRP_MJ_QUERY_SECURITY          = $14;
 IRP_MJ_SET_SECURITY            = $15;
 IRP_MJ_POWER                   = $16;
 IRP_MJ_SYSTEM_CONTROL          = $17;
 IRP_MJ_DEVICE_CHANGE           = $18;
 IRP_MJ_QUERY_QUOTA             = $19;
 IRP_MJ_SET_QUOTA               = $1A;
 IRP_MJ_PNP                     = $1B;
 IRP_MJ_PNP_POWER               = IRP_MJ_PNP;
 IRP_MJ_MAXIMUM_FUNCTION        = $1B;

 DO_BUFFERED_IO                 = $00000004;
 DO_EXCLUSIVE                   = $00000008;
 DO_DIRECT_IO                   = $00000010;
 DO_MAP_IO_BUFFER               = $00000020;
 DO_DEVICE_INITIALIZING         = $00000080;
 DO_SHUTDOWN_REGISTERED         = $00000800;
 DO_BUS_ENUMERATED_DEVICE       = $00001000;
 DO_POWER_PAGABLE               = $00002000;
 DO_POWER_INRUSH                = $00004000;


 FILE_DEVICE_BEEP               = $00000001;
 FILE_DEVICE_CD_ROM             = $00000002;
 FILE_DEVICE_CD_ROM_FILE_SYSTEM = $00000003;
 FILE_DEVICE_CONTROLLER         = $00000004;
 FILE_DEVICE_DATALINK           = $00000005;
 FILE_DEVICE_DFS                = $00000006;
 FILE_DEVICE_DISK               = $00000007;
 FILE_DEVICE_DISK_FILE_SYSTEM   = $00000008;
 FILE_DEVICE_FILE_SYSTEM        = $00000009;
 FILE_DEVICE_INPORT_PORT        = $0000000A;
 FILE_DEVICE_KEYBOARD           = $0000000B;
 FILE_DEVICE_MAILSLOT           = $0000000C;
 FILE_DEVICE_MIDI_IN            = $0000000D;
 FILE_DEVICE_MIDI_OUT           = $0000000E;
 FILE_DEVICE_MOUSE              = $0000000F;
 FILE_DEVICE_MULTI_UNC_PROVIDER = $00000010;
 FILE_DEVICE_NAMED_PIPE         = $00000011;
 FILE_DEVICE_NETWORK            = $00000012;
 FILE_DEVICE_NETWORK_BROWSER    = $00000013;
 FILE_DEVICE_NETWORK_FILE_SYSTEM= $00000014;
 FILE_DEVICE_NULL               = $00000015;
 FILE_DEVICE_PARALLEL_PORT      = $00000016;
 FILE_DEVICE_PHYSICAL_NETCARD   = $00000017;
 FILE_DEVICE_PRINTER            = $00000018;
 FILE_DEVICE_SCANNER            = $00000019;
 FILE_DEVICE_SERIAL_MOUSE_PORT  = $0000001A;
 FILE_DEVICE_SERIAL_PORT        = $0000001B;
 FILE_DEVICE_SCREEN             = $0000001C;
 FILE_DEVICE_SOUND              = $0000001D;
 FILE_DEVICE_STREAMS            = $0000001E;
 FILE_DEVICE_TAPE               = $0000001F;
 FILE_DEVICE_TAPE_FILE_SYSTEM   = $00000020;
 FILE_DEVICE_TRANSPORT          = $00000021;
 FILE_DEVICE_UNKNOWN            = $00000022;
 FILE_DEVICE_VIDEO              = $00000023;
 FILE_DEVICE_VIRTUAL_DISK       = $00000024;
 FILE_DEVICE_WAVE_IN            = $00000025;
 FILE_DEVICE_WAVE_OUT           = $00000026;
 FILE_DEVICE_8042_PORT          = $00000027;
 FILE_DEVICE_NETWORK_REDIRECTOR = $00000028;
 FILE_DEVICE_BATTERY            = $00000029;
 FILE_DEVICE_BUS_EXTENDER       = $0000002A;
 FILE_DEVICE_MODEM              = $0000002B;
 FILE_DEVICE_VDM                = $0000002C;
 FILE_DEVICE_MASS_STORAGE       = $0000002D;
 FILE_DEVICE_SMB                = $0000002E;
 FILE_DEVICE_KS                 = $0000002F;
 FILE_DEVICE_CHANGER            = $00000030;
 FILE_DEVICE_SMARTCARD          = $00000031;
 FILE_DEVICE_ACPI               = $00000032;
 FILE_DEVICE_DVD                = $00000033;
 FILE_DEVICE_FULLSCREEN_VIDEO   = $00000034;
 FILE_DEVICE_DFS_FILE_SYSTEM    = $00000035;
 FILE_DEVICE_DFS_VOLUME         = $00000036;
 FILE_DEVICE_SERENUM            = $00000037;
 FILE_DEVICE_TERMSRV            = $00000038;
 FILE_DEVICE_KSEC               = $00000039;
 FILE_DEVICE_FIPS               = $0000003A;


 EVENT_INCREMENT                = 1;
 IO_NO_INCREMENT                = 0;
 IO_CD_ROM_INCREMENT            = 1;
 IO_DISK_INCREMENT              = 1;
 IO_KEYBOARD_INCREMENT          = 6;
 IO_MAILSLOT_INCREMENT          = 2;
 IO_MOUSE_INCREMENT             = 6;
 IO_NAMED_PIPE_INCREMENT        = 2;
 IO_NETWORK_INCREMENT           = 2;
 IO_PARALLEL_INCREMENT          = 1;
 IO_SERIAL_INCREMENT            = 2;
 IO_SOUND_INCREMENT             = 8;
 IO_VIDEO_INCREMENT             = 1;
 SEMAPHORE_INCREMENT            = 1;


 MAXIMUM_FILENAME_LENGTH        = 256;

 FILE_REMOVABLE_MEDIA           = $00000001;
 FILE_READ_ONLY_DEVICE          = $00000002;
 FILE_FLOPPY_DISKETTE           = $00000004;
 FILE_WRITE_ONCE_MEDIA          = $00000008;
 FILE_REMOTE_DEVICE             = $00000010;
 FILE_DEVICE_IS_MOUNTED         = $00000020;
 FILE_VIRTUAL_VOLUME            = $00000040;
 FILE_AUTOGENERATED_DEVICE_NAME = $00000080;
 FILE_DEVICE_SECURE_OPEN        = $00000100;
 FILE_CHARACTERISTIC_PNP_DEVICE = $00000800;


 FileBasicInformation           = 4;
 FileStandardInformation        = 5;
 FilePositionInformation        = 14;
 FileEndOfFileInformation       = 20;

 FileFsVolumeInformation        = 1;
 FileFsLabelInformation         = 2;
 FileFsSizeInformation          = 3;
 FileFsDeviceInformation        = 4;
 FileFsAttributeInformation     = 5;
 FileFsControlInformation       = 6;
 FileFsFullSizeInformation      = 7;
 FileFsObjectIdInformation      = 8;
 FileFsDriverPathInformation    = 9;
 FileFsMaximumInformation       = 10;

 BusRelations                   = 0;
 EjectionRelations              = 1;
 PowerRelations                 = 2;
 RemovalRelations               = 3;
 TargetDeviceRelation           = 4;
 SingleBusRelations             = 5;

 BusQueryDeviceID               = 0;            // <Enumerator>\<Enumerator-specific device id>
 BusQueryHardwareIDs            = 1;            // Hardware ids
 BusQueryCompatibleIDs          = 2;            // compatible device ids
 BusQueryInstanceID             = 3;            // persistent id for this instance of the device
 BusQueryDeviceSerialNumber     = 4;            // serial number for this device

 
 DeviceTextDescription          = 0;            // DeviceDesc property
 DeviceTextLocationInformation  = 1;            // DeviceLocation property

 DeviceUsageTypeUndefined       = 0;
 DeviceUsageTypePaging          = 1;
 DeviceUsageTypeHibernation     = 2;
 DeviceUsageTypeDumpFile        = 3;

 PowerSystemUnspecified         = 0;
 PowerSystemWorking             = 1;
 PowerSystemSleeping1           = 2;
 PowerSystemSleeping2           = 3;
 PowerSystemSleeping3           = 4;
 PowerSystemHibernate           = 5;
 PowerSystemShutdown            = 6;
 PowerSystemMaximum             = 7;
 
 PowerActionNone                = 0;
 PowerActionReserved            = 1;
 PowerActionSleep               = 2;
 PowerActionHibernate           = 3;
 PowerActionShutdown            = 4;
 PowerActionShutdownReset       = 5;
 PowerActionShutdownOff         = 6;
 PowerActionWarmEject           = 7;
 
 PowerDeviceUnspecified         = 0;
 PowerDeviceD0                  = 1;
 PowerDeviceD1                  = 2;
 PowerDeviceD2                  = 3;
 PowerDeviceD3                  = 4;
 PowerDeviceMaximum             = 5;
 
 SystemPowerState               = 0;
 DevicePowerState               = 1;

 Executive                      = 0;
 FreePage                       = 1; 
 PageIn                         = 2; 
 PoolAllocation                 = 3; 
 DelayExecution                 = 4; 
 Suspended                      = 5; 
 UserRequest                    = 6; 
 WrExecutive                    = 7; 
 WrFreePage                     = 8; 
 WrPageIn                       = 9; 
 WrPoolAllocation               = 10;
 WrDelayExecution               = 11; 
 WrSuspended                    = 12; 
 WrUserRequest                  = 13; 
 WrEventPair                    = 14; 
 WrQueue                        = 15; 
 WrLpcReceive                   = 16; 
 WrLpcReply                     = 17; 
 WrVirtualMemory                = 18; 
 WrPageOut                      = 19; 
 WrRendezvous                   = 20;
 Spare2                         = 21; 
 Spare3                         = 22; 
 Spare4                         = 23; 
 Spare5                         = 24; 
 Spare6                         = 25; 
 WrKernel                       = 26; 
 MaximumWaitReason              = 27; 

 KernelMode                     = 0;
 UserMode                       = 1;
 MaximumMode                    = 2;

 NonPagedPool                   = 0;
 PagedPool                      = 1;
 NonPagedPoolMustSucceed        = 2;
 DontUseThisType                = 3;
 NonPagedPoolCacheAligned       = 4;
 PagedPoolCacheAligned          = 5;
 NonPagedPoolCacheAlignedMustS  = 6;
 MaxPoolType                    = 7;

//
// types are very important, 
// because we want to code drivers in Delphi we use we use Delphi style 
// of types, but also we want to have some code compatibility so we implement
// also WinAPI (C) style of types 
//
type
 LONG=Integer;
 PLONG=^LONG;
 ULONG=Cardinal;
 PULONG=^ULONG;
 NTSTATUS=ULONG;
 LCID=ULONG;
 TDeviceType=ULONG;
 DEVICE_TYPE=TDeviceType;
 TKProcessorMode=Byte;
 KPROCESSOR_MODE=TKProcessorMode;
 TKIrql=Byte;
 KIRQL=TKIRQL;
 PEThread=Pointer;
 PEProcess=Pointer;
 PKThread=Pointer;                              //PKTHREAD
 PHandle=^THandle;
 TAccessMask=ULONG;

 PUnicodeString=^TUnicodeString;
 TUnicodeString=packed record
  Length:Word;
  MaximumLength:Word;
  Buffer:PWideChar;
 end;
 UNICODE_STRING=TUnicodeString;
 PUNICODE_STRING=^UNICODE_STRING;

 PLargeInteger=^TLargeInteger;
 TLargeInteger=packed record
  LowPart:Cardinal;
  HighPart:Integer;
 end;

 PObjectAttributes=^TObjectAttributes;
 TObjectAttributes=packed record
  Length:Cardinal;
  RootDirectory:THandle;
  ObjectName:PUnicodeString;
  Attributes:Cardinal;
  SecurityDescriptor:Pointer;
  SecurityQualityOfService:Pointer;
 end;
 OBJECT_ATTRIBUTES=^TObjectAttributes;
 POBJECT_ATTRIBUTES=^OBJECT_ATTRIBUTES;

 PClientId=^TClientId;
 TClientId=packed record
  UniqueProcess:Cardinal;
  UniqueThread:Cardinal;
 end;
 CLIENT_ID=TClientId;
 PCLIENT_ID=^CLIENT_ID;

 PDriverObject=^TDriverObject;
 PDeviceObject=^TDeviceObject;
 PIrp=^TIrp;

 PListEntry=^TListEntry;
 TListEntry=packed record
  Flink:PListEntry;
  BLink:PListEntry;
 end;
 LIST_ENTRY=TListEntry;
 PLIST_ENTRY=^LIST_ENTRY;
 PRLIST_ENTRY=PLIST_ENTRY;


 PKDeviceQueueEntry=^TKDeviceQueueEntry;
 TKDeviceQueueEntry=packed record
  DeviceListEntry:TListEntry;
  SortKey:Cardinal;
  Inserted:LongBool;
 end;
 KDEVICE_QUEUE_ENTRY=TKDeviceQueueEntry;
 PKDEVICE_QUEUE_ENTRY=^KDEVICE_QUEUE_ENTRY;
 PRKDEVICE_QUEUE_ENTRY=PKDEVICE_QUEUE_ENTRY;

 PWaitContextBlock=^TWaitContextBlock;
 TWaitContextBlock=packed record
  WaitQueueEntry:TKDeviceQueueEntry;
  DeviceRoutine:Pointer;                        //PDRIVER_CONTROL
  DeviceContext:Pointer;
  NumberOfMapRegisters:Cardinal;
  DeviceObject:Pointer;
  CurrentIrp:Pointer;
  BufferChainingDpc:Pointer;                    //PKDPC
 end;
 WAIT_CONTEXT_BLOCK=TWaitContextBlock;
 PWAIT_CONTEXT_BLOCK=^WAIT_CONTEXT_BLOCK;

 TKSpinLock=Pointer;                            //ULONG_PTR
 PKSpinLock=^TKSpinLock;
 KSPIN_LOCK=TKSpinLock;
 PKSPIN_LOCK=^KSPIN_LOCK;

 TDeviceObjectUnionQueue=packed record
  case Byte of
   0:(ListEntry:TListEntry);
   1:(Wcb:TWaitContextBlock);
 end;

 PDevObjExtension=^TDevObjExtension;
 TDevObjExtension=packed record
  wType:Word;
  Size:Word;
  DeviceObject:PDeviceObject;
 end;
 DEVOBJ_EXTENSION=TDevObjExtension;
 PDEVOBJ_EXTENSION=^DEVOBJ_EXTENSION;

 PKDeviceQueue=^TKDeviceQueue;
 TKDeviceQueue=packed record
  wType:Word;
  Size:Word;
  DeviceListHead:TListEntry;
  Lock:TKSpinLock;
  Busy:LongBool;
 end;
 KDEVICE_QUEUE=TKDeviceQueue;
 PKDEVICE_QUEUE=^KDEVICE_QUEUE;

 PKApc=^TKApc;
 TKApc=packed record
  wType:Word;
  Size:Word;
  Spare0:Cardinal;
  Thread:PKThread;
  ApcListEntry:TListEntry;
  KernelRoutine:Pointer;                        //PKKERNEL_ROUTINE
  RundownRoutine:Pointer;                       //PKRUNDOWN_ROUTINE
  NormalRoutine:Pointer;                        //PKNORMAL_ROUTINE
  NormalContext:Pointer;
  SystemArgument1:Pointer;
  SystemArgument2:Pointer;
  ApcStateIndex:Byte;
  ApcMode:TKProcessorMode;
  Inserted:WordBool;
 end;
 KAPC=TKApc;
 PRKAPC=PKApc;

 PKDpc=^TKDpc;
 TKDpc=packed record
  wType:Word;
  Number:Byte;
  Importance:Byte;
  DpcListEntry:TListEntry;
  DeferredRoutine:Pointer;                      //PKDEFERRED_ROUTINE
  DeferredContext:Pointer;
  SystemArgument1:Pointer;
  SystemArgument2:Pointer;
  Lock:Pointer;                                 //PULONG_PTR
 end;
 KDPC=TKDpc;
 PRKDPC=PKDPC;

 PDispatcherHeader=^TDispatcherHeader;
 TDispatcherHeader=packed record
  bType:Byte;
  bAbsolute:Byte;
  Size:Byte;
  Inserted:Byte;
  SignalState:Cardinal;
  WaitListHead:TListEntry
 end;
 DISPATCHER_HEADER=TDispatcherHeader;

 PKEvent=^TKEvent;
 TKEvent=packed record
  Header:TDispatcherHeader;
 end;
 KEVENT=TKEvent;
 PRKEVENT=PKEVENT;

 TDeviceObject=packed record
  wType:Word;
  Size:Word;
  ReferenceCount:Integer;
  DriverObject:PDriverObject;
  NextDevice:PDeviceObject;
  AttachedDevice:PDeviceObject;
  CurrentIrp:PIrp;
  Timer:Pointer;                                //PIO_TIMER
  Flags:Cardinal;
  Characteristics:Cardinal;
  DoNotUse1:Pointer;
  DeviceExtension:Pointer;
  DeviceType:TDeviceType;
  StackSize:Byte;
  Queue:TDeviceObjectUnionQueue;
  AlignmentRequirement:Cardinal;
  DeviceQueue:TKDeviceQueue;
  Dpc:TKDpc;

  ActiveThreadCount:Cardinal;
  SecurityDescriptor:Pointer;                   //PSECURITY_DESCRIPTOR
  DeviceLock:TKEvent;

  SectorSize:Word;
  Spare1:Word;

  DeviceObjectExtension:PDevObjExtension;
  Reserved:Pointer;
 end;
 DEVICE_OBJECT=TDeviceObject;
 PDEVICE_OBJECT=^DEVICE_OBJECT;

 TIrpUnionAssociatedIrp=packed record
  case Byte of
   0:(MasterIrp:PIrp);
   1:(IrpCount:Cardinal);
   2:(SystemBuffer:Pointer);
 end;

 PIoStatusBlock=^TIoStatusBlock;
 TIoStatusBlock=packed record
  Status:NTSTATUS;
  Information:Cardinal;                         //ULONG_PTR
 end;
 IO_STATUS_BLOCK=TIoStatusBlock;
 PIO_STATUS_BLOCK=^IO_STATUS_BLOCK;


 TIrpUnionOverlayStructAsynchronousParameters=packed record
  UserApcRoutine:Pointer;                       //PIO_APC_ROUTINE
  UserApcContext:Pointer;
 end;

 TIrpUnionOverlay=packed record
  case Byte of 
   0:(AsynchronousParameters:TIrpUnionOverlayStructAsynchronousParameters);
   1:(AllocationSize:TLargeInteger);
 end;

 TIrpUnionTailStructOverlayUnion1=packed record
  case Byte of 
   0:(DeviceQueueEntry:TKDeviceQueueEntry);
   1:(DriverContext:array[0..3] of Pointer);
 end;

 TIrpUnionTailStructOverlayStruct1Union1=packed record
  case Byte of 
   0:(CurrentStackLocation:Pointer);            //PIO_STACK_LOCATION
   1:(PacketType:Cardinal);
 end;

 TIrpUnionTailStructOverlayStruct1=packed record
  ListEntry:TListEntry;
  u1:TIrpUnionTailStructOverlayStruct1Union1;
 end;

 TIrpUnionTailStructOverlay=packed record
  u1:TIrpUnionTailStructOverlayUnion1;
  Thread:PEThread;
  AuxiliaryBuffer:PChar;
  s1:TIrpUnionTailStructOverlayStruct1;
  OriginalFileObject:Pointer;                   //PFILE_OBJECT
 end;

 TIrpUnionTail=packed record
  case Byte of 
   0:(Overlay:TIrpUnionTailStructOverlay);
   1:(Apc:TKApc);
   2:(CompletionKey:Pointer);
 end;

 TIrp=packed record
  wType:Word;
  Size:Word;
  MdlAddress:Pointer;                           //PMDL
  Flags:Cardinal;
  AssociatedIrp:TIrpUnionAssociatedIrp;
  ThreadListEntry:TListEntry;
  IoStatus:TIoStatusBlock;
  RequestorMode:TKProcessorMode;
  PendingReturned:Boolean;
  StackCount:Byte;
  CurrentLocation:Byte;
  Cancel:Boolean;
  CancelIrql:TKIrql;
  ApcEnvironment:Byte;
  AllocationFlags:Byte;
  UserIosb:PIoStatusBlock;
  UserEvent:PKEvent;
  Overlay:TIrpUnionOverlay;
  CancelRoutine:Pointer;                        //PDRIVER_CANCEL
  UserBuffer:Pointer;
  Tail:TIrpUnionTail;
 end;
 IRP=TIrp;


 TDriverObject=packed record
  wType:Word;
  Size:Word;
  DeviceObject:PDeviceObject;
  Flags:Cardinal;
  DriverStart:Pointer;
  DriverSize:Cardinal;
  DriverSection:Pointer;
  DriverExtension:Pointer;                      //PDRIVER_EXTENSION
  DriverName:TUnicodeString;
  HardwareDatabase:PUnicodeString;
  FastIoDispatch:Pointer;                       //PFAST_IO_DISPATCH
  DriverInit:Pointer;                           //PDRIVER_INITIALIZE
  DriverStartIo:Pointer;                        //PDRIVER_STARTIO
  DriverUnload:Pointer;                         //PDRIVER_UNLOAD
  MajorFunction:array[0..IRP_MJ_MAXIMUM_FUNCTION] of Pointer;   //PDRIVER_DISPATCH
 end;
 PDRIVER_OBJECT=PDriverObject;
 DRIVER_OBJECT=TDriverObject;

 PFileObject=^TFileObject;
 TFileObject=packed record 
  wType:Word;
  Size:Word;
  DeviceObject:PDeviceObject;
  DoNotUser1:Pointer;
  FsContext:Pointer;
  FsContext2:Pointer;
  SectionObjectPointer:Pointer;                 //PSECTION_OBJECT_POINTERS
  PrivateCacheMap:Pointer;
  FinalStatus:NTSTATUS;
  RelatedFileObject:PFileObject;
  LockOperation:Boolean;
  DeletePending:Boolean;
  ReadAccess:Boolean;
  WriteAccess:Boolean;
  DeleteAccess:Boolean;
  SharedRead:Boolean;
  SharedWrite:Boolean;
  SharedDelete:Boolean;
  Flags:Cardinal;
  FileName:TUnicodeString;
  CurrentByteOffset:TLargeInteger;
  Waiters:Cardinal;
  Busy:Cardinal;
  LastLock:Pointer;
  Lock:TKEvent;
  Event:TKEvent;
  CompletionContext:Pointer;                    //PIO_COMPLETION_CONTEXT
 end;
 FILE_OBJECT=TFileObject;
 PFILE_OBJECT=^FILE_OBJECT;

 TFileInformationClass=Cardinal;
 TFsInformationClass=Cardinal;
 TDeviceRelationType=Cardinal;
 TBusQueryIdType=Cardinal;
 TDeviceTextType=Cardinal;
 TDeviceUsageNotificationType=Cardinal;
 TSystemPowerState=Cardinal;
 TPowerAction=Cardinal;
 TDevicePowerState=Cardinal;
 TPowerStateType=Cardinal;
 TKWaitReason=Cardinal;
 TPoolType=Cardinal;

 PPowerState=^TPowerState;
 TPowerState=packed record 
  case Byte of 
   0:(SystemState:TSystemPowerState);
   1:(DeviceState:TDevicePowerState);
 end;
 POWER_STATE=TPowerState;
 PPOWER_STATE=^POWER_STATE;

 TIoStackLocationUnionParametersStructCreate=packed record
  SecurityContext:Pointer;                      //PIO_SECURITY_CONTEXT 
  Options:Cardinal;
  FileAttributes:Word;
  ShareAccess:Word;
  EaLength:Cardinal;
 end;

 TIoStackLocationUnionParametersStructRead=packed record
  Length:Cardinal;
  Key:Cardinal;
  ByteOffset:TLargeInteger;
 end;

 TIoStackLocationUnionParametersStructWrite=packed record
  Length:Cardinal;
  Key:Cardinal;
  ByteOffset:TLargeInteger;
 end;

 TIoStackLocationUnionParametersStructQueryFile=packed record
  Length:Cardinal;
  FileInformationClass:TFileInformationClass;
 end;

 TIoStackLocationUnionParametersStructSetFile=packed record
  Length:Cardinal;
  FileInformationClass:TFileInformationClass;
  FileObject:PFileObject;
  case Byte of
   0:(ReplaceIfExists:Boolean;
      AdvanceOnly:Boolean);
   1:(CluserCount:Cardinal);
   2:(DeleteHandle:THandle);
 end;

 TIoStackLocationUnionParametersStructQueryVolume=packed record
  Length:Cardinal;
  FsInformationClass:TFsInformationClass;
 end;

 TIoStackLocationUnionParametersStructDeviceIoControl=packed record
  OutputBufferLength:Cardinal;
  InputBufferLength:Cardinal;
  IoControlCode:Cardinal;
  Type3InputBuffer:Pointer;
 end;

 TIoStackLocationUnionParametersStructMountVolume=packed record
  DoNotUse1:Pointer;
  DeviceObject:PDeviceObject;
 end;

 TIoStackLocationUnionParametersStructVerifyVolume=packed record
  DoNotUse1:Pointer;
  DeviceObject:PDeviceObject;
 end;

 TIoStackLocationUnionParametersStructScsi=packed record
  Srn:Pointer;                                  //_SCSI_REQUEST_BLOCK *
 end;

 TIoStackLocationUnionParametersStructQueryDeviceRelations=packed record
  drType:TDeviceRelationType;
 end;

 TIoStackLocationUnionParametersStructQueryInterface=packed record
  InterfaceType:Pointer;                        //CONST GUID *
  Size:Word;
  Version:Word;
  pInterface:Pointer;                           //PINTERFACE
  InterfaceSpecificData:Pointer;
 end;

 TIoStackLocationUnionParametersStructDeviceCapabilities=packed record
  Capabilities:Pointer;                         //PDEVICE_CAPABILITIES
 end;

 TIoStackLocationUnionParametersStructFilterResourceRequirements=packed record
  IoResourceRequirementList:Pointer;            //PIO_RESOURCE_REQUIREMENTS_LIST
 end;

 TIoStackLocationUnionParametersStructReadWriteConfig=packed record
  WhichSpace:Cardinal;
  Buffer:Pointer;
  Offset:Cardinal;
  Length:Cardinal;
 end;

 TIoStackLocationUnionParametersStructSetLock=packed record
  Lock:LongBool;
 end;

 TIoStackLocationUnionParametersStructQueryId=packed record
  IdType:TBusQueryIdType;
 end;

 TIoStackLocationUnionParametersStructQueryDeviceText=packed record
  DeviceTextType:TDeviceTextType;
  LocaleId:LCID;
 end;

 TIoStackLocationUnionParametersStructUsageNotification=packed record
  InPath:Boolean;
  Reserved:array[0..2] of Boolean;
  dunType:TDeviceUsageNotificationType;
 end;

 TIoStackLocationUnionParametersStructWaitWake=packed record
  PowerState:TSystemPowerState;
 end;

 TIoStackLocationUnionParametersStructPowerSequence=packed record
  PowerSequence:Pointer;                        //PPOWER_SEQUENCE
 end;

 TIoStackLocationUnionParametersStructPower=packed record
  SystemContext:Cardinal;
  psType:TPowerStateType;
  State:TPowerState;
  ShutdownType:TPowerAction;
 end;

 TIoStackLocationUnionParametersStructStartDevice=packed record
  AllocatedResources:Pointer;                   //PCM_RESOURCE_LIST
  AllocatedResourcesTranslated:Pointer;         //PCM_RESOURCE_LIST
 end;

 TIoStackLocationUnionParametersStructWMI=packed record
  ProviderId:Pointer;                           //ULONG_PTR
  DataPath:Pointer;
  BufferSize:Cardinal;
  Buffer:Cardinal;
 end;

 TIoStackLocationUnionParametersStructOthers=packed record
  Argument1:Pointer;
  Argument2:Pointer;
  Argument3:Pointer;
  Argument4:Pointer;
 end;

 TIoStackLocationUnionParameters=packed record
  case Byte of
   00:(Create:TIoStackLocationUnionParametersStructCreate);
   01:(Read:TIoStackLocationUnionParametersStructRead);
   02:(Write:TIoStackLocationUnionParametersStructWrite);
   03:(QueryFile:TIoStackLocationUnionParametersStructQueryFile);
   04:(SetFile:TIoStackLocationUnionParametersStructSetFile);
   05:(QueryVolume:TIoStackLocationUnionParametersStructQueryVolume);
   06:(DeviceIoControl:TIoStackLocationUnionParametersStructDeviceIoControl);
   07:(MountVolume:TIoStackLocationUnionParametersStructMountVolume);
   08:(VerifyVolume:TIoStackLocationUnionParametersStructVerifyVolume);
   09:(Scsi:TIoStackLocationUnionParametersStructScsi);
   10:(QueryDeviceRelations:TIoStackLocationUnionParametersStructQueryDeviceRelations);
   11:(QueryInterface:TIoStackLocationUnionParametersStructQueryInterface);
   12:(DeviceCapabilities:TIoStackLocationUnionParametersStructDeviceCapabilities);
   13:(FilterResourceRequirements:TIoStackLocationUnionParametersStructFilterResourceRequirements);
   14:(ReadWriteConfig:TIoStackLocationUnionParametersStructReadWriteConfig);
   15:(SetLock:TIoStackLocationUnionParametersStructSetLock);
   16:(QueryId:TIoStackLocationUnionParametersStructQueryId);
   17:(QueryDeviceText:TIoStackLocationUnionParametersStructQueryDeviceText);
   18:(UsageNotification:TIoStackLocationUnionParametersStructUsageNotification);
   19:(WaitWake:TIoStackLocationUnionParametersStructWaitWake);
   20:(PowerSequence:TIoStackLocationUnionParametersStructPowerSequence);
   21:(Power:TIoStackLocationUnionParametersStructPower);
   22:(StartDevice:TIoStackLocationUnionParametersStructStartDevice);
   23:(WMI:TIoStackLocationUnionParametersStructWMI);
   24:(Others:TIoStackLocationUnionParametersStructOthers);
 end;

 PIoStackLocation=^TIoStackLocation;
 TIoStackLocation=packed record
  MajorFunction:Byte;
  MinorFunction:Byte;
  Flags:Byte;
  Control:Byte;
  Parameters:TIoStackLocationUnionParameters;
  DeviceObject:PDeviceObject;
  FileObject:PFileObject;
  CompletionRoutine:Pointer;                    //PIO_COMPLETION_ROUTINE
  Context:Pointer;
 end;
 IO_STACK_LOCATION=TIoStackLocation;
 PIO_STACK_LOCATION=^IO_STACK_LOCATION;

 PServiceDescriptorEntry=^TServiceDescriptorEntry;
 TServiceDescriptorEntry=packed record
  ServiceTableBase:PULONG;
  ServiceCounterTableBase:PULONG;
  NumberOfServices:ULONG;
  ParamTableBase:PByte;
 end;
 SERVICE_DESCRIPTOR_ENTRY=TServiceDescriptorEntry;
 PSERVICE_DESCRIPTOR_ENTRY=^SERVICE_DESCRIPTOR_ENTRY;

 PKMutant=^TKMutant;
 TKMutant=packed record
  Header:TDispatcherHeader;
  MutantListEntry:TListEntry;
  OwnerThread:PKThread;
  Abandoned:Boolean;
  ApcDisable:Byte;
  Alignment0:Word;
 end;
 TKMutex=TKMutant;
 PKMutex=^TKMutex;

function KeServiceDescriptorTable:PServiceDescriptorEntry; 

procedure IoCompleteRequest(Irp:PIrp;PriorityBoost:Integer); stdcall; 
function  IoCreateDevice(DriverObject:PDriverObject;DeviceExtensionSize:Cardinal;DeviceName:PUnicodeString;DeviceType:TDeviceType;DeviceCharacteristics:Cardinal;Reserved:Boolean;var DeviceObject:PDeviceObject):NTSTATUS; stdcall;
procedure IoDeleteDevice(DeviceObject:PDeviceObject); stdcall; 
function  IoCreateSymbolicLink(SymbolicLinkName,DeviceName:PUnicodeString):NTSTATUS; stdcall;
function  IoDeleteSymbolicLink(SymbolicLinkName:PUnicodeString):NTSTATUS; stdcall;
procedure RtlInitUnicodeString(DestinationString:PUnicodeString;SourceString:PWideChar); stdcall;
function  InterlockedExchange(Target:PLONG;Value:LONG):LONG; stdcall; 
function  ZwOpenProcess(ProcessHandle:PHandle;DesiredAccess:TAccessMask;ObjectAttributes:PObjectAttributes;ClientId:PClientId):NTSTATUS; stdcall; 
procedure ProbeForRead(Address:Pointer;Length:Cardinal;Alignment:Cardinal); stdcall; 
procedure ExFreePool(P:Pointer); stdcall; 
function  KeWaitForSingleObject(SObject:Pointer;WaitReason:TKWaitReason;WaitMode:TKProcessorMode;Alertable:LongBool;Timeout:PLargeInteger):NTSTATUS; stdcall; 
function  KeWaitForMutexObject(SObject:Pointer;WaitReason:TKWaitReason;WaitMode:TKProcessorMode;Alertable:LongBool;Timeout:PLargeInteger):NTSTATUS; stdcall; 
function  ExAllocatePool(PoolType:TPoolType;NumberOfBytes:Cardinal):Pointer; stdcall; 
function  ExAllocatePoolWithQuota(PoolType:TPoolType;NumberOfBytes:Cardinal):Pointer; stdcall;
function  ExAllocatePoolWithTag(PoolType:TPoolType;NumberOfBytes:Cardinal;Tag:ULONG):Pointer; stdcall;
procedure KeInitializeMutex(Mutex:PKMutex;Level:Cardinal); stdcall; 
function  KeReleaseMutex(Mutex:PKMutex;Wait:LongBool):LONG; stdcall; 


function  IoGetCurrentIrpStackLocation(Irp:PIrp):PIoStackLocation; stdcall;

function ZwOpenProcessAddr:Pointer;

function NT_SUCCESS(AStatus:NTSTATUS):Boolean;

function DbgPrint(Format:PChar;Args:array of const):NTSTATUS; stdcall;
function DbgMsg(Format:PChar;Args:array of const):NTSTATUS; stdcall;

implementation

function  krnlDbgPrint(Format:PChar;Args:array of const):NTSTATUS; stdcall; external NtKernel name 'DbgPrint';

function  krnlIoCreateDevice(DriverObject:PDriverObject;DeviceExtensionSize:Cardinal;DeviceName:PUnicodeString;DeviceType:TDeviceType;DeviceCharacteristics:Cardinal;Reserved:Boolean;var DeviceObject:PDeviceObject):NTSTATUS; stdcall; external NtKernel name 'IoCreateDevice';
procedure krnlIoCompleteRequest(Irp:PIrp;PriorityBoost:Integer); stdcall; external NtKernel name 'IoCompleteRequest';
procedure krnlIoDeleteDevice(DeviceObject:PDeviceObject); stdcall; external NtKernel name 'IoDeleteDevice';
function  krnlIoCreateSymbolicLink(SymbolicLinkName,DeviceName:PUnicodeString):NTSTATUS; stdcall; external NtKernel name 'IoCreateSymbolicLink';
function  krnlIoDeleteSymbolicLink(SymbolicLinkName:PUnicodeString):NTSTATUS; stdcall; external NtKernel name 'IoDeleteSymbolicLink';
procedure krnlRtlInitUnicodeString(DestinationString:PUnicodeString;SourceString:PWideChar); stdcall; external NtKernel name 'RtlInitUnicodeString';
function  krnlInterlockedExchange(Target:PLONG;Value:LONG):LONG; register; external NtKernel name 'InterlockedExchange';
function  krnlZwOpenProcess(ProcessHandle:PHandle;DesiredAccess:TAccessMask;ObjectAttributes:PObjectAttributes;ClientId:PClientId):NTSTATUS; stdcall; external NtKernel name 'ZwOpenProcess';
procedure krnlKeServiceDescriptorTable; external NtKernel name 'KeServiceDescriptorTable';
procedure krnlProbeForRead(Address:Pointer;Length:Cardinal;Alignment:Cardinal); stdcall; external NtKernel name 'ProbeForRead';
procedure krnlExFreePool(P:Pointer); stdcall; external NtKernel name 'ExFreePool';
function  krnlKeWaitForSingleObject(SObject:Pointer;WaitReason:TKWaitReason;WaitMode:TKProcessorMode;Alertable:LongBool;Timeout:PLargeInteger):NTSTATUS; stdcall; external NtKernel name 'KeWaitForSingleObject';
function  krnlExAllocatePool(PoolType:TPoolType;NumberOfBytes:Cardinal):Pointer; stdcall; external NtKernel name 'ExAllocatePool';
function  krnlExAllocatePoolWithQuota(PoolType:TPoolType;NumberOfBytes:Cardinal):Pointer; stdcall; external NtKernel name 'ExAllocatePoolWithQuota';
function  krnlExAllocatePoolWithTag(PoolType:TPoolType;NumberOfBytes:Cardinal;Tag:ULONG):Pointer; stdcall; external NtKernel name 'ExAllocatePoolWithTag';
procedure krnlKeInitializeMutex(Mutex:PKMutex;Level:Cardinal); stdcall; external NtKernel name 'KeInitializeMutex';
function  krnlKeReleaseMutex(Mutex:PKMutex;Wait:LongBool):LONG; stdcall; external NtKernel name 'KeReleaseMutex';

procedure IoCompleteRequest(Irp:PIrp;PriorityBoost:Integer); stdcall; begin krnlIoCompleteRequest(Irp,PriorityBoost); end;
function  IoCreateDevice(DriverObject:PDriverObject;DeviceExtensionSize:Cardinal;DeviceName:PUnicodeString;DeviceType:TDeviceType;DeviceCharacteristics:Cardinal;Reserved:Boolean;var DeviceObject:PDeviceObject):NTSTATUS; stdcall; begin  Result:=krnlIoCreateDevice(DriverObject,DeviceExtensionSize,DeviceName,DeviceType,DeviceCharacteristics,Reserved,DeviceObject); end;
procedure IoDeleteDevice(DeviceObject:PDeviceObject); stdcall; begin krnlIoDeleteDevice(DeviceObject); end; 
function  IoCreateSymbolicLink(SymbolicLinkName,DeviceName:PUnicodeString):NTSTATUS; stdcall; begin Result:=krnlIoCreateSymbolicLink(SymbolicLinkName,DeviceName); end;
function  IoDeleteSymbolicLink(SymbolicLinkName:PUnicodeString):NTSTATUS; stdcall; begin Result:=krnlIoDeleteSymbolicLink(SymbolicLinkName); end;
procedure RtlInitUnicodeString(DestinationString:PUnicodeString;SourceString:PWideChar); stdcall; begin krnlRtlInitUnicodeString(DestinationString,SourceString); end;
function  InterlockedExchange(Target:PLONG;Value:LONG):LONG; assembler; asm mov ecx,Target mov edx,Value call krnlInterlockedExchange end;
function  ZwOpenProcess(ProcessHandle:PHandle;DesiredAccess:TAccessMask;ObjectAttributes:PObjectAttributes;ClientId:PClientId):NTSTATUS; stdcall; begin Result:=krnlZwOpenProcess(ProcessHandle,DesiredAccess,ObjectAttributes,ClientId); end;
procedure ProbeForRead(Address:Pointer;Length:Cardinal;Alignment:Cardinal); stdcall; begin krnlProbeForRead(Address,Length,Alignment); end;
procedure ExFreePool(P:Pointer); stdcall; begin krnlExFreePool(P); end;
function  KeWaitForSingleObject(SObject:Pointer;WaitReason:TKWaitReason;WaitMode:TKProcessorMode;Alertable:LongBool;Timeout:PLargeInteger):NTSTATUS; stdcall; begin Result:=krnlKeWaitForSingleObject(SObject,WaitReason,WaitMode,Alertable,Timeout) end;
function  KeWaitForMutexObject(SObject:Pointer;WaitReason:TKWaitReason;WaitMode:TKProcessorMode;Alertable:LongBool;Timeout:PLargeInteger):NTSTATUS; stdcall; begin Result:=krnlKeWaitForSingleObject(SObject,WaitReason,WaitMode,Alertable,Timeout) end;
function  ExAllocatePool(PoolType:TPoolType;NumberOfBytes:Cardinal):Pointer; stdcall; begin Result:=krnlExAllocatePool(PoolType,NumberOfBytes); end;
function  ExAllocatePoolWithQuota(PoolType:TPoolType;NumberOfBytes:Cardinal):Pointer; stdcall; begin Result:=krnlExAllocatePoolWithQuota(PoolType,NumberOfBytes); end;
function  ExAllocatePoolWithTag(PoolType:TPoolType;NumberOfBytes:Cardinal;Tag:ULONG):Pointer; stdcall; begin Result:=krnlExAllocatePoolWithTag(PoolType,NumberOfBytes,Tag); end;
procedure KeInitializeMutex(Mutex:PKMutex;Level:Cardinal); stdcall; begin krnlKeInitializeMutex(Mutex,Level); end;
function  KeReleaseMutex(Mutex:PKMutex;Wait:LongBool):LONG; stdcall; begin Result:=krnlKeReleaseMutex(Mutex,Wait); end;

//
// this is 3 times ugly (take care while reading):
// messy Zw* function addressing, @krnlZw* points to jmp ntoskrnl.Zw* instruction
// which is jmp [IAT.ntoskrnl.Zw*] and so that we move our pointer 2 bytes right (jmp instruction is 2 byte long)
// to the pointer itself and dereference it twice to make it real pointer to Zw* function
//
function  ZwOpenProcessAddr:Pointer; begin Result:=PPointer(PPointer(Cardinal(@krnlZwOpenProcess)+2)^)^; end;

//
// this is just ugly (weird):
// somehow we don't know how to import data structure in Delphi, we need KeSDT import
// so we import it as a procedure krnlKeServiceDescriptorTable and make this function to return 
// a pointer to that table
//
function  KeServiceDescriptorTable:PServiceDescriptorEntry; begin Result:=PPointer(@krnlKeServiceDescriptorTable)^; end;

//
// this is a must (but still ugly):
// we do not have #define in Delphi, we have to rewrite non-statical #define asa function
//
function  IoGetCurrentIrpStackLocation(Irp:PIrp):PIoStackLocation; stdcall; begin Result:=Irp^.Tail.Overlay.s1.u1.CurrentStackLocation; end;


function NT_SUCCESS(AStatus:NTSTATUS):Boolean;
begin
 Result:=Integer(AStatus)>=0;
end;


{$IFDEF DEBUG}
//
// our Delphi implementation of DbgPrint
// very useful function for coding kernel driver 
// and the must is to have variable number of arguments like in original
// DbgPrint call
// this implementation is very ugly, it is because we don't know how to 
// code similar convetion as C language has for this type of functions
//
function DbgPrint(Format:PChar;Args:array of const):NTSTATUS;
var
 LI,LJ:Integer;
 LArgs:array[0..31] of Cardinal;
begin
 LJ:=0;
 //we fill our local arguments array
 for LI:=0 to High(Args) do
 begin
  with Args[LI] do
  begin
   case VType of
    vtInteger:LArgs[LJ]:=VInteger;
    vtBoolean:LArgs[LJ]:=Cardinal(VBoolean);
    vtChar:LArgs[LJ]:=Cardinal(VChar);
    vtString:LArgs[LJ]:=Cardinal(PChar(VString));
    vtPChar:LArgs[LJ]:=Cardinal(VPChar);
    vtPointer:LArgs[LJ]:=Cardinal(VPointer);
    vtAnsiString:LArgs[LJ]:=Cardinal(PChar(VAnsiString));
    vtCurrency:LArgs[LJ]:=Cardinal(VCurrency);
    vtVariant:LArgs[LJ]:=Cardinal(VVariant);
    else LArgs[LJ]:=$DEADBEEF;
   end;
  end;
  Inc(LJ);
 end;

 LJ:=High(Args);
 //and we simulate the calling convetion using lowlevel
 asm
  lea eax,LArgs
  mov ecx,LJ
  jmp @cmp_args_end
 @args_loop:
  push dword ptr [eax+4*ecx]
  dec ecx
 @cmp_args_end:
  cmp ecx,-001h
  jnz @args_loop
 @make_call:
  push Format
  call krnlDbgPrint

  mov ecx,LJ
  shl ecx,002h
  add ecx,008h
  add esp,ecx
  mov Result,eax
 end
end;

function DbgMsg(Format:PChar;Args:array of const):NTSTATUS; 
begin
 Result:=DbgPrint(Format,Args);
end;

{$ELSE}

function DbgPrint(Format:PChar;Args:array of const):NTSTATUS; assembler;
asm
end;

function DbgMsg(Format:PChar;Args:array of const):NTSTATUS; assembler;
asm
end;

{$ENDIF}

end.
