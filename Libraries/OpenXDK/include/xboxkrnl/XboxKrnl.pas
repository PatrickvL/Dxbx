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
unit XboxKrnl;

interface

uses
  // Jedi Win32API
  JwaWinType,
  JwaWinNT;

(*
// ******************************************************************
// *
// * proj : OpenXDK
// *
// * desc : Open Source XBox Development Kit
// *
// * file : xboxkrnl.h
// *
// * note : XBox Kernel Declarations
// *
// ******************************************************************

// ******************************************************************
// * dll import/export
// ******************************************************************
const DECLSPEC_IMPORT = __declspec(dllimport);
const DECLSPEC_EXPORT = __declspec(dllexport);

// ******************************************************************
// * kernel exports, others either import or link locally
// ******************************************************************
const XBSYSAPI = DECLSPEC_IMPORT;
#ifdef _XBOXKRNL_INTERNAL_
//undef  XBSYSAPI
const XBSYSAPI = DECLSPEC_EXPORT;
//endif
#ifdef _XBOXKRNL_DEFEXTRN_
//undef  XBSYSAPI
const XBSYSAPI =;
//endif
*)

// ******************************************************************
// * Null
// ******************************************************************
const
  NULL = nil;

(*
// ******************************************************************
// * TRUE / FALSE
// ******************************************************************
//ifndef FALSE
const FALSE =   0;
//endif
//ifndef TRUE
const TRUE =    1;
//endif

// ******************************************************************
// * CONST
// ******************************************************************
const CONST =;

// ******************************************************************
// * VOID
// ******************************************************************
//ifndef VOID
type                  VOID = procedure;
    //#define VOID                void
//endif
*)
// ******************************************************************
// * Basic types
// ******************************************************************
type
  CCHAR = AnsiChar;
(*
type                 SmallInt, CSHORT = SmallInt;
type                  LongInt = LongInt;
type         UCHAR = Byte;
type         BYTE = Byte;
type         BOOLEAN = Byte;
type        USHORT = Word;
type        WORD = Word;
type         ULONG = Cardinal;
type         DWORD = Cardinal;
type         SIZE_T, *PSIZE_T = Cardinal;
type         ACCESS_MASK, *PACCESS_MASK = Cardinal;
type         PHYSICAL_ADDRESS = Cardinal;
type                  INT_PTR = LongInt;
type   __int64      LONGLONG = integer;
type   __int64    ULONGLONG = Word;
type        WCHAR = Word;

// ******************************************************************
// * Pointer types
// ******************************************************************
type                 *PCHAR = CHAR;
type                 *PCSZ = CHAR;
type                 *PBYTE = BYTE;
type              *PBOOLEAN = BOOLEAN;
type                *PUCHAR = UCHAR;
type               *PUSHORT = USHORT;
type                *PULONG = ULONG;
type                *PDWORD, *LPDWORD = DWORD;
type          *PACCESS_MASK = ACCESS_MASK;
type                 *PLONG, *LONG_PTR = LongInt;
type                *ULONG_PTR = ULONG;
type              *PINT_PTR = INT_PTR;
type                 *PVOID, *Pointer = VOID;
type                 *THandle = procedure;
type               *PHANDLE = THandle;
*)
type
  // Because Delphi doesn't support forward declarations on record-pointers,
  // you should be alert that all occurences of this type are actually meant
  // to use PKDPC instead - so use typecasts where necessary!
  _PKDPC = Pointer;

(*
// ******************************************************************
// * LPSECURITY_ATTRIBUTES
// ******************************************************************
type    LPSECURITY_ATTRIBUTES = Pointer;

// ******************************************************************
// * NTSTATUS
// ******************************************************************
type                              NTSTATUS = LongInt;

const NT_SUCCESS(Status)              ((NTSTATUS) (Status) >= 0)
const STATUS_SUCCESS =                   ((DWORD   )$00000000L);
//ifndef STATUS_PENDING
const STATUS_PENDING =                   ((DWORD   )$00000103L);
//endif
const STATUS_TIMER_RESUME_IGNORED =      ((DWORD   )$40000025L);
const STATUS_UNSUCCESSFUL =              ((DWORD   )$C0000001);
const STATUS_UNRECOGNIZED_MEDIA =        ((DWORD   )$C0000014);
//ifndef STATUS_NO_MEMORY
const STATUS_NO_MEMORY =                 ((DWORD   )$C0000017L);
//endif
const STATUS_ALERTED =                   ((DWORD   )$00000101);
const STATUS_USER_APC =                  ((DWORD   )$000000C0L);
// The SCSI input buffer was too large (not necessarily an error!)
const STATUS_DATA_OVERRUN =              ((DWORD   )$C000003CL);
const STATUS_INVALID_IMAGE_FORMAT =      ((DWORD   )$C000007BL);
const STATUS_INSUFFICIENT_RESOURCES =    ((DWORD   )$C000009AL);
const STATUS_TOO_MANY_SECRETS =          ((DWORD   )$C0000156L);
const STATUS_XBE_REGION_MISMATCH =       ((DWORD   )$C0050001L);
const STATUS_XBE_MEDIA_MISMATCH =        ((DWORD   )$C0050002L);
const STATUS_OBJECT_NAME_NOT_FOUND =     ((DWORD   )$C0000034L);
const STATUS_OBJECT_NAME_COLLISION =     ((DWORD   )$C0000035L);

// ******************************************************************
// * PAGE Masks
// ******************************************************************
const PAGE_NOACCESS =          $01;
const PAGE_READONLY =          $02;
const PAGE_READWRITE =         $04;
const PAGE_WRITECOPY =         $08;
const PAGE_EXECUTE =           $10;
const PAGE_EXECUTE_READ =      $20;
const PAGE_EXECUTE_READWRITE = $40;
const PAGE_EXECUTE_WRITECOPY = $80;
const PAGE_GUARD =             $100;
const PAGE_NOCACHE =           $200;
const PAGE_WRITECOMBINE =      $400;

// ******************************************************************
// * calling conventions
// ******************************************************************
const NTAPI =               __stdcall;
const CDECL =               __cdecl;
const INLINE =              __inline;
const DECLSPEC_NORETURN =   __declspec(noreturn);

// ******************************************************************
// * documentation purposes only
// ******************************************************************
const EXPORTNUM(a) =;
const UNALIGNED =;
const OPTIONAL =;
const IN =;
const OUT =;
*)

// ******************************************************************
// * KPROCESSOR_MODE
// ******************************************************************
type
  KPROCESSOR_MODE = CCHAR;

(*
// ******************************************************************
// * MODE
// ******************************************************************
type
 MODE = (
    KernelMode,
    UserMode,
    MaximumMode
 );

// ******************************************************************
// * WAIT_TYPE
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value **** )
 _WAIT_TYPE
begin
    WaitAll = 0,
    WaitAny = 1
 end;
WAIT_TYPE;
*)

// ******************************************************************
// * LARGE_INTEGER
// ******************************************************************
type
  _LARGE_INTEGER = record
    case Integer of
      0: (
        LowPart: DWORD;
        HighPart: LongInt);
      1: (
        QuadPart: LONGLONG);
  end;

  LARGE_INTEGER = _LARGE_INTEGER;
  PLARGE_INTEGER = ^LARGE_INTEGER;

// ******************************************************************
// * ULARGE_INTEGER
// ******************************************************************
type
  _ULARGE_INTEGER = record
    case Integer of
      0: (
        LowPart: DWORD;
        HighPart: DWORD);
      1: (
        QuadPart: ULONGLONG);
  end;

  ULARGE_INTEGER = _ULARGE_INTEGER;
  PULARGE_INTEGER = ^ULARGE_INTEGER;

(*
// ******************************************************************
// * STRING
// ******************************************************************
type

STRING,ANSI_STRING,*PSTRING,*PANSI_STRING  = record
    USHORT  Length;
    USHORT  MaximumLength;
    PCHAR   Buffer;
 end;G;

// ******************************************************************
// * UNICODE_STRING
// ******************************************************************
type

UNICODE_STRING,*PUNICODE_STRING  = record
    USHORT  Length;
    USHORT  MaximumLength;
    USHORT *Buffer;
 end;

// ******************************************************************
// * LIST_ENTRY
// ******************************************************************
type

LIST_ENTRY,*PLIST_ENTRY  = record
    struct _LIST_ENTRY *Flink;
    struct _LIST_ENTRY *Blink;
 end;

// ******************************************************************
// * FILE_FS_SIZE_INFORMATION
// ******************************************************************
type

FILE_FS_SIZE_INFORMATION,*PFILE_FS_SIZE_INFORMATION  = record
    LARGE_INTEGER   TotalAllocationUnits;
    LARGE_INTEGER   AvailableAllocationUnits;
    ULONG           SectorsPerAllocationUnit;
    ULONG           BytesPerSector;
 end;

// ******************************************************************
// * FILE_INFORMATION_CLASS
// ******************************************************************
type
 _FILE_INFORMATION_CLASS = (
    FileFiller0,
    FileDirectoryInformation, // = 1
    FileFullDirectoryInformation,
    FileBothDirectoryInformation,
    FileBasicInformation,
    FileStandardInformation,
    FileInternalInformation,
    FileEaInformation,
    FileAccessInformation,
    FileNameInformation,
    FileRenameInformation,
    FileLinkInformation,
    FileNamesInformation,
    FileDispositionInformation,
    FilePositionInformation,
    FileFullEaInformation,
    FileModeInformation,
    FileAlignmentInformation,
    FileAllInformation,
    FileAllocationInformation,
    FileEndOfFileInformation,
    FileAlternateNameInformation,
    FileStreamInformation,
    FilePipeInformation,
    FilePipeLocalInformation,
    FilePipeRemoteInformation,
    FileMailslotQueryInformation,
    FileMailslotSetInformation,
    FileCompressionInformation,
    FileCopyOnWriteInformation,
    FileCompletionInformation,
    FileMoveClusterInformation,
    FileQuotaInformation,
    FileReparsePointInformation,
    FileNetworkOpenInformation,
    FileObjectIdInformation,
    FileTrackingInformation,
    FileOleDirectoryInformation,
    FileContentIndexInformation,
    FileInheritContentIndexInformation,
    FileOleInformation,
    FileMaximumInformation
 );
  FILE_INFORMATION_CLASS = _FILE_INFORMATION_CLASS;
  PFILE_INFORMATION_CLASS = ^FILE_INFORMATION_CLASS;

// ******************************************************************
// * CreateDisposition Values for NtCreateFile
// ******************************************************************
const FILE_SUPERSEDE =                          $00000000;
const FILE_OPEN =                               $00000001;
const FILE_CREATE =                             $00000002;
const FILE_OPEN_IF =                            $00000003;
const FILE_OVERWRITE =                          $00000004;
const FILE_OVERWRITE_IF =                       $00000005;
const FILE_MAXIMUM_DISPOSITION =                $00000005;

// ******************************************************************
// * CreateOption Values for NtCreateFile
// ******************************************************************
// FILE_NON_DIRECTORY_FILE | FILE_SYNCHRONOUS_IO_NONALERT is what CreateFile
// uses for most things when translating to NtCreateFile.
const FILE_DIRECTORY_FILE =                     $00000001;
const FILE_WRITE_THROUGH =                      $00000002;
const FILE_SEQUENTIAL_ONLY =                    $00000004;
const FILE_NO_INTERMEDIATE_BUFFERING =          $00000008;
const FILE_SYNCHRONOUS_IO_ALERT =               $00000010;
const FILE_SYNCHRONOUS_IO_NONALERT =            $00000020;
const FILE_NON_DIRECTORY_FILE =                 $00000040;
const FILE_CREATE_TREE_CONNECTION =             $00000080;
const FILE_COMPLETE_IF_OPLOCKED =               $00000100;
const FILE_NO_EA_KNOWLEDGE =                    $00000200;
const FILE_OPEN_FOR_RECOVERY =                  $00000400;
const FILE_RANDOM_ACCESS =                      $00000800;
const FILE_DELETE_ON_CLOSE =                    $00001000;
const FILE_OPEN_BY_FILE_ID =                    $00002000;
const FILE_OPEN_FOR_BACKUP_INTENT =             $00004000;
const FILE_NO_COMPRESSION =                     $00008000;
const FILE_RESERVE_OPFILTER =                   $00100000;
const FILE_OPEN_REPARSE_POINT =                 $00200000;
const FILE_OPEN_NO_RECALL =                     $00400000;
const FILE_OPEN_FOR_FREE_SPACE_QUERY =          $00800000;
const FILE_COPY_STRUCTURED_STORAGE =            $00000041;
const FILE_STRUCTURED_STORAGE =                 $00000441;
const FILE_VALID_OPTION_FLAGS =                 $00ffffff;
const FILE_VALID_PIPE_OPTION_FLAGS =            $00000032;
const FILE_VALID_MAILSLOT_OPTION_FLAGS =        $00000032;
const FILE_VALID_SET_FLAGS =                    $00000036;

// ******************************************************************
// * OBJECT_ATTRIBUTES
// ******************************************************************
type

OBJECT_ATTRIBUTES,*POBJECT_ATTRIBUTES  = record
    THandle  RootDirectory;
    PSTRING ObjectName;
    ULONG   Attributes;
 end;

// ******************************************************************
// * FSINFOCLASS
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value **** )
 _FSINFOCLASS
begin
    FileFsVolumeInformation       = 1,
    FileFsLabelInformation,      // 2
    FileFsSizeInformation,       // 3
    FileFsDeviceInformation,     // 4
    FileFsAttributeInformation,  // 5
    FileFsControlInformation,    // 6
    FileFsFullSizeInformation,   // 7
    FileFsObjectIdInformation,   // 8
    FileFsMaximumInformation
 end;
FS_INFORMATION_CLASS, *PFS_INFORMATION_CLASS;

// ******************************************************************
// * FILE_DIRECTORY_INFORMATION
// ******************************************************************
type

FILE_DIRECTORY_INFORMATION  = record _FILE_DIRECTORY_INFORMATION
 
    ULONG           NextEntryOffset;
    ULONG           FileIndex;
    LARGE_INTEGER   CreationTime;
    LARGE_INTEGER   LastAccessTime;
    LARGE_INTEGER   LastWriteTime;
    LARGE_INTEGER   ChangeTime;
    LARGE_INTEGER   EndOfFile;
    LARGE_INTEGER   AllocationSize;
    ULONG           FileAttributes;
    ULONG           FileNameLength;
                FileName: array[0..1-1] of CHAR;        // Offset: 0x40
 end;
*)
// *******************************************
// * MM_STATISTICS
// ******************************************************************
type
  PMM_STATISTICS = ^MM_STATISTICS;
  MM_STATISTICS = record
    Length: ULONG;
    TotalPhysicalPages: ULONG;
    AvailablePages: ULONG;
    VirtualMemoryBytesCommitted: ULONG;
    VirtualMemoryBytesReserved: ULONG;
    CachePagesCommitted: ULONG;
    PoolPagesCommitted: ULONG;
    StackPagesCommitted: ULONG;
    ImagePagesCommitted: ULONG;
  end;

(*
// ******************************************************************
// * IO_STATUS_BLOCK *Same as Win2k/XP*
// ******************************************************************
type

  u1  = record
    union
    begin
        NTSTATUS Status;
        PVOID    Pointer;
     end;;

    ULONG_PTR Information;
 end;
IO_STATUS_BLOCK, *PIO_STATUS_BLOCK;

// ******************************************************************
// * EVENT_TYPE
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value **** )
 _EVENT_TYPE
begin
    NotificationEvent = 0,
    SynchronizationEvent
 end;
EVENT_TYPE;

// ******************************************************************
// * BUS_DATA_TYPE
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value **** )
 _BUS_DATA_TYPE
begin
    ConfigurationSpaceUndefined = $FF,
    Cmos                        = $0,
    EisaConfiguration           = $1,
    Pos                         = $2,
    CbusConfiguration           = $3,
    PCIConfiguration            = $4,
    VMEConfiguration            = $5,
    NuBusConfiguration          = $6,
    PCMCIAConfiguration         = $7,
    MPIConfiguration            = $8,
    MPSAConfiguration           = $9,
    PNPISAConfiguration         = $A,
    SgiInternalConfiguration    = $B,
    MaximumBusDataType          = $C,
 end;
BUS_DATA_TYPE;

// ******************************************************************
// * PCI_SLOT_NUMBER
// ******************************************************************
type

    bits  = record
    union
    begin
        struct
        begin
            ULONG   DeviceNumber:5;
            ULONG   FunctionNumber:3;
            ULONG   Reserved:24;
         end;ts;

        ULONG   AsULONG;
     end;u;
 end;
PCI_SLOT_NUMBER, *PPCI_SLOT_NUMBER;

const PCI_TYPE0_ADDRESSES =             6;
const PCI_TYPE1_ADDRESSES =             2;
const PCI_TYPE2_ADDRESSES =             5;

// ******************************************************************
// * PCI_COMMON_CONFIG
// ******************************************************************
type

    type0  = record
    USHORT  VendorID;                   // 0x00 (ro)
    USHORT  DeviceID;                   // 0x02 (ro)
    USHORT  Command;                    // 0x04 Device control
    USHORT  Status;                     // 0x06
    UCHAR   RevisionID;                 // 0x08 (ro)
    UCHAR   ProgIf;                     // 0x09 (ro)
    UCHAR   SubClass;                   // 0x0A (ro)
    UCHAR   BaseClass;                  // 0x0B (ro)
    UCHAR   CacheLineSize;              // 0x0C (ro+)
    UCHAR   LatencyTimer;               // 0x0D (ro+)
    UCHAR   HeaderType;                 // 0x0E (ro)
    UCHAR   BIST;                       // 0x0F Built in self test

    union
    begin
        struct _PCI_HEADER_TYPE_0
        begin
               BaseAddresses: array[0..PCI_TYPE0_ADDRESSES-1] of ULONG; // 0x10
            ULONG   CIS;
            USHORT  SubVendorID;
            USHORT  SubSystemID;
            ULONG   ROMBaseAddress;
            UCHAR   CapabilitiesPtr;
               Reserved1: array[0..3-1] of UCHAR;
            ULONG   Reserved2;
            UCHAR   InterruptLine;      //
            UCHAR   InterruptPin;       // (ro)
            UCHAR   MinimumGrant;       // (ro)
            UCHAR   MaximumLatency;     // (ro)
         end;e0;
     end;u;

     DeviceSpecific: array[0..192-1] of UCHAR;

 end;
PCI_COMMON_CONFIG, *PPCI_COMMON_CONFIG;

const FIELD_OFFSET(ctype, = field)    ((LongInt)(LONG_PTR) and (((ctype )0).field));

const PCI_COMMON_HDR_LENGTH = (FIELD_OFFSET (PCI_COMMON_CONFIG, DeviceSpecific));

const PCI_MAX_DEVICES =                     32;
const PCI_MAX_FUNCTION =                    8;
const PCI_MAX_BRIDGE_NUMBER =               $FF;
const PCI_INVALID_VENDORID =                $FFFF;

const PCI_VENDOR_NVIDIA_CORPORATION =       $10DE;

const PCI_USB0_DEVICE_ID =                  2;
const PCI_USB0_FUNCTION_ID =                0;
const PCI_USB0_IRQ =                        1;
const PCI_USB0_REGISTER_BASE =              $FED00000;
const PCI_USB0_REGISTER_LENGTH =            $00001000;
const PCI_USB0_OHCI_CONTROLLER =            $01C2;
*)

// ******************************************************************
// * RETURN_FIRMWARE
// ******************************************************************
type
  LPRETURN_FIRMWARE = ^RETURN_FIRMWARE;
  RETURN_FIRMWARE = {enum}(
    ReturnFirmwareHalt {=$0},
    ReturnFirmwareReboot {=$1},
    ReturnFirmwareQuickReboot {=$2},
    ReturnFirmwareHard {=$3},
    ReturnFirmwareFatal {=$4},
    ReturnFirmwareAll {=$5});

// ******************************************************************
// * LAUNCH_DATA_HEADER
// ******************************************************************
type
  LAUNCH_DATA_HEADER = record
    dwLaunchDataType: DWORD;
    dwTitleId: DWORD;
    szLaunchPath: array[0..520 - 1] of UCHAR;
    dwFlags: DWORD;
  end;
  PLAUNCH_DATA_HEADER = ^LAUNCH_DATA_HEADER;

// ******************************************************************
// * LAUNCH_DATA_PAGE
// ******************************************************************
type
  LAUNCH_DATA_PAGE = record
    Header: LAUNCH_DATA_HEADER;
    Pad: array[0..492 - 1] of UCHAR;
    LaunchData: array[0..3072 - 1] of UCHAR;
  end;
  PLAUNCH_DATA_PAGE = ^LAUNCH_DATA_PAGE;

// ******************************************************************
// * DISPATCHER_HEADER
// ******************************************************************
type
  DISPATCHER_HEADER = record
    cType: UCHAR; // 0x00
    _Absolute: UCHAR; // 0x01
    Size: UCHAR; // 0x02
    Inserted: UCHAR; // 0x03
    SignalState: LongInt; // 0x04
    WaitListHead: LIST_ENTRY; // 0x08
  end;

(*
// ******************************************************************
// * TIMER_TYPE
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value **** )
 _TIMER_TYPE
begin
    NotificationTimer     = 0,
    SynchronizationTimer  = 1
 end;
TIMER_TYPE;
*)
// ******************************************************************
// * KTIMER (Timer Object)
// ******************************************************************
type
  PKTIMER = ^KTIMER;
  KTIMER = record
    Header: DISPATCHER_HEADER; // 0x00
    DueTime: ULARGE_INTEGER; // 0x10
    TimerListEntry: LIST_ENTRY; // 0x18
    Dpc: _PKDPC; // 0x20
    Period: LongInt; // 0x24
  end;
(*
// ******************************************************************
// * PKSTART_ROUTINE
// ******************************************************************
// *
// * NOTE: Non-standard call. Similar to stdcall, but first argument
// *       must be located at ebp+4 before calling.
// *
// *       This is different from the NT version: 2 parameters as
// *       opposed to 1.
// *
// ******************************************************************
type  VOID (NTAPI *PKSTART_ROUTINE)
(
    IN PVOID StartContext1,
    IN PVOID StartContext2
);

*)

// ******************************************************************
// * PKDEFERRED_ROUTINE
// ******************************************************************
type
  PKDEFERRED_ROUTINE = procedure(
    Dpc: _PKDPC;
    DeferredContext: PVOID;
    SystemArgument1: PVOID;
    SystemArgument2: PVOID
    ); stdcall; // TODO : Is this indeed the necessary calling convention?


// ******************************************************************
// * KDPC (Deferred Procedure Call (DPC) Object)
// ******************************************************************
type
  PKDPC = ^KDPC;
  KDPC = record
    cType: CSHORT; // 0x00
    Number: UCHAR; // 0x02
    Importance: UCHAR; // 0x03
    DpcListEntry: LIST_ENTRY; // 0x04
    DeferredRoutine: PKDEFERRED_ROUTINE; // 0x0C
    DeferredContext: PVOID;
    SystemArgument1: PVOID;
    SystemArgument2: PVOID;
  end;

(*
// ******************************************************************
// * KOBJECTS
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value **** )
 _KOBJECTS
begin
    DpcObject = $13,
 end;
KOBJECTS, *PKOBJECTS;

// ******************************************************************
// * RTL_CRITICAL_SECTION
// ******************************************************************
type

RTL_CRITICAL_SECTION,*PRTL_CRITICAL_SECTION  = record
                   Unknown: array[0..4-1] of DWORD;                                     // 0x00
    LongInt                LockCount;                                      // 0x10
    LongInt                RecursionCount;                                 // 0x14
    ULONG               OwningThread;                                   // 0x18
 end;
*)

// ******************************************************************
// * NT_TIB
// ******************************************************************
type
  PNT_TIB = ^NT_TIB;
  NT_TIB = record
    ExceptionList: PEXCEPTION_REGISTRATION_RECORD; // 0x00
    StackBase: Pointer; // 0x04
    StackLimit: Pointer; // 0x08
    SubSystemTib: Pointer; // 0x0C
//    case Boolean of
//      True: (
    FiberData: Pointer; // 0x10 for TIB
//        );
//      False: (
//        Version: ULONG;                                              // 0x10 for TEB (?)
//        )
//    end;
    ArbitraryUserPointer: Pointer; // 0x14
    Self: PNT_TIB; // 0x18
  end;

// ******************************************************************
// * KTHREAD
// ******************************************************************
// *
// * NOTE: INCOMPLETE!!
// *
// ******************************************************************
type
  PKTHREAD = ^KTHREAD;
  KTHREAD = record
    UnknownA: array[0..$28 - 1] of UCHAR;
    TlsData: PVOID; // 0x28
    UnknownB: array[0..$E4 - 1] of UCHAR; // 0x2C
  end;

// ******************************************************************
// * ETHREAD
// ******************************************************************
// *
// * NOTE: INCOMPLETE!!
// *
// ******************************************************************
type
  PETHREAD = ^ETHREAD;
  ETHREAD = record
    Tcb: KTHREAD;
    UnknownA: array[0..$1C - 1] of UCHAR; // 0x110
    UniqueThread: DWORD; // 0x12C
  end;

 // ******************************************************************
// * KPCRB
// ******************************************************************
// *
// * NOTE: INCOMPLETE!!
// *
// ******************************************************************
type
  PKPRCB = ^KPRCB;
  KPRCB = record
    CurrentThread: PKTHREAD; // 0x00, KPCR : 0x28
    NextThread: PKTHREAD; // 0x04, KPCR : 0x2C
    IdleThread: PKTHREAD; // 0x08, KPCR : 0x30

    // This is the total size of the structure (presumably)
    Unknown: array[0..$250 - 1] of UCHAR; // 0x0C, KPCR : 0x34
  end;

// ******************************************************************
// * KPCR
// ******************************************************************
// *
// * NOTE: KPCR is the structure which exists at the FS: segment.
// *
// ******************************************************************
type
  PKPCR = ^KPCR;
  KPCR = record
    NtTib: NT_TIB; // 0x00
    SelfPcr: PKPCR; // 0x1C
    Prcb: PKPRCB; // 0x20
    Irql: UCHAR; // 0x24
    PrcbData: KPRCB; // 0x28
  end;

(*
// ******************************************************************
// * EEPROM_INDEX
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value **** )
 _EEPROM_INDEX
begin
    EEPROM_MISC = $11
 end;
EEPROM_INDEX, *PEEPROM_INDEX;
*)
// ******************************************************************
// * XBOX_HARDWARE_INFO
// ******************************************************************
type
  XBOX_HARDWARE_INFO = record
    Flags: ULONG;
    Unknown1: UCHAR;
    Unknown2: UCHAR;
    Unknown3: UCHAR;
    Unknown4: UCHAR;
  end;

(*
// ******************************************************************
// * TIME_FIELDS
// ******************************************************************
type

TIME_FIELDS,*PTIME_FIELDS  = record
    USHORT  Year;
    USHORT  Month;
    USHORT  Day;
    USHORT  Hour;
    USHORT  Minute;
    USHORT  Second;
    USHORT  Millisecond;
    USHORT  Weekday;
 end;

// ******************************************************************
// * READ_REGISTER_UCHAR
// ******************************************************************
// *
// * Use this to access I/O mapped memory. Just a good standard.
// *
// ******************************************************************
INLINE  function READ_REGISTER_UCHAR(Address: PUCHAR): UCHAR;
begin
    result:= *(volatile UCHAR )Address;
 end;

// ******************************************************************
// * READ_REGISTER_USHORT
// ******************************************************************
// *
// * Use this to access I/O mapped memory. Just a good standard.
// *
// ******************************************************************
INLINE  USHORT READ_REGISTER_USHORT(PUSHORT Address)
begin
    result:= *(volatile USHORT )Address;
 end;

// ******************************************************************
// * READ_REGISTER_ULONG
// ******************************************************************
// *
// * Use this to access I/O mapped memory. Just a good standard.
// *
// ******************************************************************
INLINE  ULONG READ_REGISTER_ULONG(PULONG Address)
begin
    result:= *(volatile ULONG )Address;
 end;

// ******************************************************************
// * WRITE_REGISTER_UCHAR
// ******************************************************************
// *
// * Use this to access I/O mapped memory (without this, writing a
// * value and then reading it back can produce an incorrect result
// * because the write may not be completed yet.)
// *
// ******************************************************************
 VOID WRITE_REGISTER_function(Address: PVOID; Value: UCHAR): UCHAR;
begin
    asm
    begin
        mov edx, Address
        mov ah, Value
        mov [edx], ah
        lock or Address, edx
    );
 end;

// ******************************************************************
// * WRITE_REGISTER_USHORT
// ******************************************************************
// *
// * Use this to access I/O mapped memory (without this, writing a
// * value and then reading it back can produce an incorrect result
// * because the write may not be completed yet.)
// *
// ******************************************************************
 VOID WRITE_REGISTER_USHORT(PVOID Address, USHORT Value)
begin
    asm
    begin
        mov edx, Address
        mov ax, Value
        mov [edx], ax
        lock or Address, edx
    );
 end;

// ******************************************************************
// * WRITE_REGISTER_ULONG
// ******************************************************************
// *
// * Use this to access I/O mapped memory (without this, writing a
// * value and then reading it back can produce an incorrect result
// * because the write may not be completed yet.)
// *
// ******************************************************************
 VOID WRITE_REGISTER_ULONG(PVOID Address, ULONG Value)
begin
    asm
    begin
        mov edx, Address
        mov eax, Value
        mov [edx], eax
        lock or Address, edx
    );
 end;

// ******************************************************************
// * Debug
// ******************************************************************
//include 'dbg.h'

// ******************************************************************
// * Executive
// ******************************************************************
//include 'ex.h'

// ******************************************************************
// * Hardware Abstraction Layer
// ******************************************************************
//include 'hal.h'

// ******************************************************************
// * I/O Manager
// ******************************************************************
//include 'io.h'

// ******************************************************************
// * Kernel
// ******************************************************************
//include 'kernel.h'

// ******************************************************************
// * Memory Manager
// ******************************************************************
//include 'mm.h'

// ******************************************************************
// * NT
// ******************************************************************
//include 'nt.h'

// ******************************************************************
// * Object Manager
// ******************************************************************
//include 'ob.h'

// ******************************************************************
// * Process Structure
// ******************************************************************
//include 'ps.h'

// ******************************************************************
// * Run-time Library
// ******************************************************************
//include 'rtl.h'

// ******************************************************************
// * XBox
// ******************************************************************
//include 'xbox.h'

//if defined(__cplusplus)
 end;
//endif

//endif


*)

//
// DXBX Addition : Object Type Object
//
type
  _OBJECT_TYPE = record
(*
    ERESOURCE Mutex;
    LIST_ENTRY TypeList;
    UNICODE_STRING Name;
    PVOID DefaultObject;
    ULONG Index;
    ULONG TotalNumberOfObjects;
    ULONG TotalNumberOfHandles;
    ULONG HighWaterNumberOfObjects;
    ULONG HighWaterNumberOfHandles;
    OBJECT_TYPE_INITIALIZER TypeInfo;
    ULONG Key;
    ERESOURCE ObjectLocks[4];
*)
  end;
  POBJECT_TYPE = ^_OBJECT_TYPE;

//
// DXBX Addition : Xbox Refurb Info - TODO : Complete this!
//
type
  _XBOX_REFURB_INFO = record
    //
    FirstBootTime: FILETIME;
    //
    PowerCycleCount: DWORD;
    //
  end;
  XBOX_REFURB_INFO = _XBOX_REFURB_INFO;
  PXBOX_REFURB_INFO = ^_XBOX_REFURB_INFO;

//
// DXBX Addition : Xbox Interrupt Mode - Source: ReactOS
//
type
  KINTERRUPT_MODE = (LevelSensitive, Latched);

//
// DXBX Addition : Hardware Interface Type - Source: ReactOS
//
type
  _INTERFACE_TYPE = (
    InterfaceTypeUndefined = -1,
    Internal,
    Isa,
    Eisa,
    MicroChannel,
    TurboChannel,
    PCIBus,
    VMEBus,
    NuBus,
    PCMCIABus,
    CBus,
    MPIBus,
    MPSABus,
    ProcessorInternal,
    InternalPowerBus,
    PNPISABus,
    PNPBus,
    MaximumInterfaceType
    );
  INTERFACE_TYPE = _INTERFACE_TYPE;
  PINTERFACE_TYPE = ^INTERFACE_TYPE;

//
// DXBX Addition : Create Thread Notify Routine - Source: ReactOS
//
type
  PCREATE_THREAD_NOTIFY_ROUTINE = procedure(
    ProcessId: HANDLE;
    ThreadId: HANDLE;
    Create: LONGBOOL); stdcall;

implementation

end.
