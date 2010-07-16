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

{$DEFINE DXBX_USE_JWA_TYPES}

uses
  // Jedi Win32API
  JwaWinType,
  JwaWinNT,
  JwaNative;

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
type         BOOLEAN = ByteBool; Cxbx : unsigned char = AnsiChar in Delphi
type        USHORT = Word;
type        WORD = Word;
type         ULONG = Cardinal;
type         DWORD = Cardinal;
type         SIZE_T, *PSIZE_T = Cardinal;
type         ACCESS_MASK, *PACCESS_MASK = Cardinal;
type         PHYSICAL_ADDRESS = Cardinal;
type                  INT_PTR = LongInt;
type   __int64      LONGLONG = Integer;
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

// ******************************************************************
// * MODE
// ******************************************************************
type
  MODE = {enum}(
    KernelMode,
    UserMode,
    MaximumMode
  );

// ******************************************************************
// * WAIT_TYPE *Same as Win2k/XP*
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value ****)
{$IFDEF DXBX_USE_JWA_TYPES}
  _WAIT_TYPE = JwaWinType._WAIT_TYPE;
  WAIT_TYPE = JwaWinType.WAIT_TYPE;
{$ELSE}
  _WAIT_TYPE = {enum}(
    WaitAll = 0,
    WaitAny = 1
  );
  WAIT_TYPE = _WAIT_TYPE;
{$ENDIF}

// ******************************************************************
// * LARGE_INTEGER  *Same as Win2k/XP*
// ******************************************************************
type
{$IFDEF DXBX_USE_JWA_TYPES}

  LARGE_INTEGER = JwaWinType.LARGE_INTEGER;
  PLARGE_INTEGER = JwaWinType.PLARGE_INTEGER;
{$ELSE}
  LARGE_INTEGER = record
    case Integer of
      0: (
        {0x00}LowPart: DWORD;
        {0x04}HighPart: LongInt);
      1: (
        {0x00}QuadPart: LONGLONG);
  end; {=0x08}
  PLARGE_INTEGER = ^LARGE_INTEGER;
{$ENDIF}

  // Dxbx note : Use this as a return-type, to make Delphi fill EDX:EAX instead of stack!!
  // See http://rvelthuis.de/articles/articles-convert.html#returns
  _LARGE_INTEGER = Int64;

// ******************************************************************
// * ULARGE_INTEGER *Same as Win2k/XP*
// ******************************************************************
type
{$IFDEF DXBX_USE_JWA_TYPES}
  ULARGE_INTEGER = JwaWinType.ULARGE_INTEGER;
  PULARGE_INTEGER = JwaWinType.PULARGE_INTEGER;
{$ELSE}
  ULARGE_INTEGER = record
    case Integer of
      0: (
        {0x00}LowPart: DWORD;
        {0x04}HighPart: DWORD);
      1: (
        {0x00}QuadPart: ULONGLONG);
  end; {=0x08}
  PULARGE_INTEGER = ^ULARGE_INTEGER;
{$ENDIF}

// ******************************************************************
// * STRING  *Same as Win2k/XP*
// ******************************************************************
type
{$IFDEF DXBX_USE_JWA_TYPES}
  _STRING = JwaWinType._STRING;
  PSTRING = JwaWinType.PSTRING;

  ANSI_STRING = JwaWinType.ANSI_STRING;
  PANSI_STRING = JwaWinType.PANSI_STRING;
{$ELSE}
  _STRING = record
    {0x00}Length: USHORT;
    {0x02}MaximumLength: USHORT;
    {0x04}Buffer: PAnsiChar;
  end; {=0x08}
  // Delphi : Don't redeclare STRING = _STRING;
  PSTRING = ^_STRING;

  ANSI_STRING = _STRING;
  PANSI_STRING = ^ANSI_STRING;
{$ENDIF}

type
  POBJECT_STRING = PSTRING;

// ******************************************************************
// * UNICODE_STRING  *Same as Win2k/XP*
// ******************************************************************
type
{$IFDEF DXBX_USE_JWA_TYPES}
  _UNICODE_STRING = JwaWinType._UNICODE_STRING;
  UNICODE_STRING = JwaWinType.UNICODE_STRING;
  PUNICODE_STRING = JwaWinType.PUNICODE_STRING;
{$ELSE}
  _UNICODE_STRING = record
    {0x00}Length: USHORT;
    {0x02}MaximumLength: USHORT;
    {0x04}Buffer: PWideChar;
  end; {=0x08}
  UNICODE_STRING = _UNICODE_STRING;
  PUNICODE_STRING = ^UNICODE_STRING;
{$ENDIF}

// ******************************************************************
// * LIST_ENTRY  *Same as Win2k/XP*
// ******************************************************************
type
{$IFDEF DXBX_USE_JWA_TYPES}
  _LIST_ENTRY = JwaWinType._LIST_ENTRY;
  LIST_ENTRY = JwaWinType.LIST_ENTRY;
  PLIST_ENTRY = JwaWinType.PLIST_ENTRY;
{$ELSE}
  PLIST_ENTRY = ^LIST_ENTRY;
  _LIST_ENTRY = record
    {0x00}Flink: PLIST_ENTRY;
    {0x04}Blink: PLIST_ENTRY;
  end; {=0x08}
  LIST_ENTRY = _LIST_ENTRY;
{$ENDIF}

(*
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
*)

// ******************************************************************
// * FILE_INFORMATION_CLASS *Same as Win2k/XP*
// ******************************************************************
type
{$IFDEF DXBX_USE_JWA_TYPES}
  _FILE_INFORMATION_CLASS = JwaNative._FILE_INFORMATION_CLASS;
  FILE_INFORMATION_CLASS = JwaNative.FILE_INFORMATION_CLASS;
  PFILE_INFORMATION_CLASS = JwaNative.PFILE_INFORMATION_CLASS;
{$ELSE}
  _FILE_INFORMATION_CLASS = {enum}(
    FileFiller0,
    FileDirectoryInformation, // = 1
    FileFullDirectoryInformation, // = 2
    FileBothDirectoryInformation, // = 3
    FileBasicInformation, // = 4
    FileStandardInformation, // = 5
    FileInternalInformation, // = 6
    FileEaInformation, // = 7
    FileAccessInformation, // = 8
    FileNameInformation, // = 9
    FileRenameInformation, // = 10
    FileLinkInformation, // = 11
    FileNamesInformation, // = 12
    FileDispositionInformation, // = 13
    FilePositionInformation, // = 14
    FileFullEaInformation, // = 15
    FileModeInformation, // = 16
    FileAlignmentInformation, // = 17
    FileAllInformation, // = 18
    FileAllocationInformation, // = 19
    FileEndOfFileInformation, // = 20
    FileAlternateNameInformation, // = 21
    FileStreamInformation, // = 22
    FilePipeInformation, // = 23
    FilePipeLocalInformation, // = 24
    FilePipeRemoteInformation, // = 25
    FileMailslotQueryInformation, // = 26
    FileMailslotSetInformation, // = 27
    FileCompressionInformation, // = 28
    FileObjectIdInformation, // = 29
    FileCompletionInformation, // = 30
    FileMoveClusterInformation, // = 31
    FileQuotaInformation, // = 32
    FileReparsePointInformation, // = 33
    FileNetworkOpenInformation, // = 34
    FileAttributeTagInformation, // = 35
    FileTrackingInformation, // = 36
    FileMaximumInformation // = 37
  );
  FILE_INFORMATION_CLASS = _FILE_INFORMATION_CLASS;
  PFILE_INFORMATION_CLASS = ^FILE_INFORMATION_CLASS;
{$ENDIF}

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
// Object Attributes type
// Differences from NT: There are no Length, SecurityDescriptor, or
//     SecurityQualityOfService fields.  Also, ObjectName is ANSI, not
//     Unicode.
type
  _OBJECT_ATTRIBUTES = record
    {0x00}RootDirectory: HANDLE;
    {0x04}ObjectName: PANSI_STRING;
    {0x08}Attributes: ULONG;
  end; {=0x0C}
  OBJECT_ATTRIBUTES = _OBJECT_ATTRIBUTES;
  POBJECT_ATTRIBUTES = ^OBJECT_ATTRIBUTES;

// Flags for OBJECT_ATTRIBUTES::Attributes
const OBJ_INHERIT             = $00000002;
const OBJ_PERMANENT           = $00000010;
const OBJ_EXCLUSIVE           = $00000020;
const OBJ_CASE_INSENSITIVE    = $00000040;
const OBJ_OPENIF              = $00000080;
const OBJ_OPENLINK            = $00000100;
const OBJ_KERNEL_HANDLE       = $00000200;
const OBJ_VALID_ATTRIBUTES    = $000003F2;

// ******************************************************************
// * FSINFOCLASS *Same as Win2k/XP*
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value ****)
{$IFDEF DXBX_USE_JWA_TYPES}
  _FSINFOCLASS = JwaNative._FSINFOCLASS;
  FS_INFORMATION_CLASS = JwaNative.FS_INFORMATION_CLASS;
  PFS_INFORMATION_CLASS = JwaNative.PFS_INFORMATION_CLASS;
{$ELSE}
  _FSINFOCLASS = {enum}(
    FileFsVolumeInformation       = 1,
    FileFsLabelInformation,      // 2
    FileFsSizeInformation,       // 3
    FileFsDeviceInformation,     // 4
    FileFsAttributeInformation,  // 5
    FileFsControlInformation,    // 6
    FileFsFullSizeInformation,   // 7
    FileFsObjectIdInformation,   // 8
    FileFsMaximumInformation
  );
  FS_INFORMATION_CLASS = _FSINFOCLASS;
  PFS_INFORMATION_CLASS = ^FS_INFORMATION_CLASS;
{$ENDIF}

(*
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
                FileName: array [0..1-1] of CHAR;        // Offset: 0x40
 end;
*)

// ******************************************************************
// * MM_STATISTICS (Memory manager statistics)
// ******************************************************************
type
  _MM_STATISTICS = record
    {0x00}Length: ULONG;
    {0x04}TotalPhysicalPages: ULONG;
    {0x08}AvailablePages: ULONG;
    {0x0C}VirtualMemoryBytesCommitted: ULONG;
    {0x10}VirtualMemoryBytesReserved: ULONG;
    {0x14}CachePagesCommitted: ULONG;
    {0x18}PoolPagesCommitted: ULONG;
    {0x1C}StackPagesCommitted: ULONG;
    {0x20}ImagePagesCommitted: ULONG;
  end; {=0x24}
  MM_STATISTICS = _MM_STATISTICS;
  PMM_STATISTICS = ^MM_STATISTICS;

// ******************************************************************
// * PS_STATISTICS (Process statistics)
// ******************************************************************
type
  _PS_STATISTICS = record
    {0x00}Length: ULONG;
    {0x04}ThreadCount: ULONG;
    {0x08}HandleCount: ULONG;
  end; {=0x0C}
  PS_STATISTICS = _PS_STATISTICS;
  PPS_STATISTICS = ^PS_STATISTICS;

// ******************************************************************
// * IO_STATUS_BLOCK *Same as Win2k/XP*
// ******************************************************************
type
{$IFDEF DXBX_USE_JWA_TYPES}
  _IO_STATUS_BLOCK = JwaNative._IO_STATUS_BLOCK;
  IO_STATUS_BLOCK = JwaNative.IO_STATUS_BLOCK;
  PIO_STATUS_BLOCK = JwaNative.PIO_STATUS_BLOCK;
{$ELSE}
  _IO_STATUS_BLOCK = record
    {union} case Integer of
      0: ({0x00}Status: NTSTATUS);
      1: ({0x00}Pointer: PVOID;
    {0x04}Information: ULONG_PTR;
    ); // close last union case
  end; {=0x08}
  IO_STATUS_BLOCK = _IO_STATUS_BLOCK;
  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;
{$ENDIF}

// ******************************************************************
// * EVENT_TYPE *Same as Win2k/XP*
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value ****)
{$IFDEF DXBX_USE_JWA_TYPES}
  _EVENT_TYPE = JwaWinType._EVENT_TYPE;
  EVENT_TYPE = JwaWinType.EVENT_TYPE;
{$ELSE}
  _EVENT_TYPE = {enum}(
    NotificationEvent = 0,
    SynchronizationEvent
  );
  EVENT_TYPE = _EVENT_TYPE;
{$ENDIF}

// ******************************************************************
// * BUS_DATA_TYPE
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value ****)
  BUS_DATA_TYPE = {enum}(
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
    MaximumBusDataType          = $C
  );

(*
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
               BaseAddresses: array [0..PCI_TYPE0_ADDRESSES-1] of ULONG; // 0x10
            ULONG   CIS;
            USHORT  SubVendorID;
            USHORT  SubSystemID;
            ULONG   ROMBaseAddress;
            UCHAR   CapabilitiesPtr;
               Reserved1: array [0..3-1] of UCHAR;
            ULONG   Reserved2;
            UCHAR   InterruptLine;      //
            UCHAR   InterruptPin;       // (ro)
            UCHAR   MinimumGrant;       // (ro)
            UCHAR   MaximumLatency;     // (ro)
         end;e0;
     end;u;

     DeviceSpecific: array [0..192-1] of UCHAR;

 end;
PCI_COMMON_CONFIG, *PPCI_COMMON_CONFIG;

*)
//const FIELD_OFFSET(ctype, = field)    ((LongInt)(LONG_PTR) and (((ctype )0).field));
(*
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
  RETURN_FIRMWARE = {enum}(
    ReturnFirmwareHalt {=$0},
    ReturnFirmwareReboot {=$1},
    ReturnFirmwareQuickReboot {=$2},
    ReturnFirmwareHard {=$3},
    ReturnFirmwareFatal {=$4},
    ReturnFirmwareAll {=$5}
  );
  LPRETURN_FIRMWARE = ^RETURN_FIRMWARE;

// ******************************************************************
// * LAUNCH_DATA_HEADER
// ******************************************************************
type
  LAUNCH_DATA_HEADER = record
    {0x000}dwLaunchDataType: DWORD;
    {0x004}dwTitleId: DWORD;
    {0x008}szLaunchPath: array [0..520 - 1] of UCHAR;
    {0x210}dwFlags: DWORD;
  end; {=0x214}
  PLAUNCH_DATA_HEADER = ^LAUNCH_DATA_HEADER;

const PAGE_SIZE=$1000; // *Same as Win2k/XP*

// ******************************************************************
// * LAUNCH_DATA_PAGE
// ******************************************************************
const MAX_LAUNCH_DATA_SIZE = 3072;
type
  LAUNCH_DATA_PAGE = record
    {0x000}Header: LAUNCH_DATA_HEADER;
    {0x214}Pad: array [0..(PAGE_SIZE-MAX_LAUNCH_DATA_SIZE-SizeOf(LAUNCH_DATA_HEADER)) - 1] of UCHAR;
    {0x400}LaunchData: array [0..MAX_LAUNCH_DATA_SIZE - 1] of UCHAR;
  end; {=0x1000}
  PLAUNCH_DATA_PAGE = ^LAUNCH_DATA_PAGE;

// ******************************************************************
// * DISPATCHER_HEADER
// ******************************************************************
type
  DISPATCHER_HEADER = record
    {0x00}Type_: UCHAR;
    {0x01}Absolute: UCHAR;
    {0x02}Size: UCHAR;
    {0x03}Inserted: UCHAR;
    {0x04}SignalState: LongInt;
    {0x08}WaitListHead: LIST_ENTRY;
  end; {=0x10}

// ******************************************************************
// * TIMER_TYPE *Same as Win2k/XP*
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value ****)
{$IFDEF DXBX_USE_JWA_TYPES}
  _TIMER_TYPE = JwaWinType._TIMER_TYPE;
  TIMER_TYPE = JwaWinType.TIMER_TYPE;
{$ELSE}
  _TIMER_TYPE = {enum}(
    NotificationTimer     = 0,
    SynchronizationTimer  = 1
  );
  TIMER_TYPE = _TIMER_TYPE;
{$ENDIF}

  // ******************************************************************
// * KTIMER (Timer Object)
// ******************************************************************
type
  KTIMER = record
    {0x00}Header: DISPATCHER_HEADER;
    {0x10}DueTime: ULARGE_INTEGER;
    {0x18}TimerListEntry: LIST_ENTRY;
    {0x20}Dpc: _PKDPC;
    {0x24}Period: LongInt;
  end; {=0x28}
  PKTIMER = ^KTIMER;

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
type
  PKSTART_ROUTINE = procedure(
    StartContext1: PVOID;
    StartContext2: PVOID
    ); stdcall;

// ******************************************************************
// * PKDEFERRED_ROUTINE
// ******************************************************************
type
  PKDEFERRED_ROUTINE = procedure(
    Dpc: _PKDPC;
    DeferredContext: PVOID;
    SystemArgument1: PVOID;
    SystemArgument2: PVOID
    ); stdcall;


// ******************************************************************
// * KDPC (Deferred Procedure Call (DPC) Object)
// ******************************************************************
type
  KDPC = record
    {0x00}Type_: CSHORT;
    {0x02}Number: UCHAR;
    {0x03}Importance: UCHAR;
    {0x04}DpcListEntry: LIST_ENTRY;
    {0x0C}DeferredRoutine: PKDEFERRED_ROUTINE;
    {0x10}DeferredContext: PVOID;
    {0x14}SystemArgument1: PVOID;
    {0x18}SystemArgument2: PVOID;
  end; {=0x1C}
  PKDPC = ^KDPC;

// ******************************************************************
// * KOBJECTS
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value ****)
  KOBJECTS = {enum}(
    DpcObject = $13
  );
  PKOBJECTS = ^KOBJECTS;

// ******************************************************************
// * RTL_CRITICAL_SECTION
// ******************************************************************

(* Below, we define the Xbox1 CriticalSection struct to look as much as the Native version.
   Individually, they look like this :

  Native._RTL_CRITICAL_SECTION = record
    {0x00}DebugInfo: PRTL_CRITICAL_SECTION_DEBUG;
    {0x04}LockCount: LONG;
    {0x08}RecursionCount: LONG;
    {0x0C}OwningThread: HANDLE;
    {0x10}LockSemaphore: HANDLE;
    {0x14}SpinCount: ULONG_PTR;
  end; {=0x18}

  Xbox1._RTL_CRITICAL_SECTION = record
    {0x00}RawEvent: array [0..4-1] of ULONG_PTR
    {0x10}LockCount: LONG;
    {0x14}RecursionCount: LONG;
    {0x18}OwningThread: HANDLE;
  end; {=0x1C}
*)

type
  RTL_CRITICAL_SECTION = record
    // There's four different declarations possible of this section :
    Overlapped: record case Integer of
      0: (Unknown: array [0..4-1] of DWORD                ); // 0x00 .. 0x0F
      1: (RawEvent: array [0..4-1] of ULONG_PTR           ); // 0x00 .. 0x0F

      //  The following field is used for blocking when there is contention for
      //  the resource [Xbox1]
      2: (Event: record
            {0x00}Type_: UCHAR;
            {0x01}Absolute_: UCHAR;
            {0x02}Size: UCHAR;
            {0x03}Inserted: UCHAR;
            {0x04}SignalState: LONG;
            {0x08}WaitListHead: LIST_ENTRY;                  // 0x08 .. 0x0F
          end); {=0x10}

      // Dxbx Note : On the Xbox1, this data is an in-place event (also known as a RawEvent, which
      // you can see above), which doesn't have to be allocated anymore, as it's already in-place.
      //
      // However, I haven't found an way to initialize such a beast on WinNT.
      //
      // So as an alternative, I now try to mimick as much of the native _RTL_CRITICAL_SECTION in here,
      // in an attempt to make WinNT see this structure as if it was a native critical section
      // (which I hope will make it compatible with WaitForSingle/MultipleObjects, and other API's) :
      3: (
        {0x00}DebugInfo: PRTL_CRITICAL_SECTION_DEBUG;        // keep this nil
        {0x04}LockCount: LONG;                               // Copy Xbox1.LockCount into this
        {0x08}LockSemaphore: HANDLE;                         // overlaps Native.RecursionCount, safest to reuse
        {0x0C}OwningThread: HANDLE                           // Copy Xbox1.OwningThread into this
        );
    end;

    //  The following three fields control entering and exiting the critical
    //  section for the resource [Xbox1]
    {0x10}LockCount: LongInt;
    {0x14}RecursionCount: LongInt;
    {0x18}OwningThread: ULONG
  end; {=0x1C}
  PRTL_CRITICAL_SECTION = ^RTL_CRITICAL_SECTION;

// ******************************************************************
// * NT_TIB - "Thread Information Block" *Same as Win2k/XP*
// ******************************************************************
// * See http://www.nirsoft.net/kernel_struct/vista/NT_TIB.html
// ******************************************************************
type
{$IFDEF DXBX_USE_JWA_TYPES}
  _NT_TIB = JwaNative._NT_TIB;
  NT_TIB = JwaNative.NT_TIB;
  PNT_TIB = JwaNative.PNT_TIB;
{$ELSE}
  _NT_TIB = record
    {0x00}ExceptionList: PEXCEPTION_REGISTRATION_RECORD;
    {0x04}StackBase: PVOID;
    {0x08}StackLimit: PVOID;
    {0x0C}SubSystemTib: PVOID;
    {union}case Integer of
       0: ( {0x10}FiberData: PVOID);
       1: ( {0x10}Version: ULONG; // for TEB (?)
    {0x14}ArbitraryUserPointer: PVOID;
    {0x18}Self: PNT_TIB
    ); // close last union case
  end; {=0x1C}
  NT_TIB = _NT_TIB;
  PNT_TIB = ^NT_TIB;
{$ENDIF}

// StrikerX3 NOTE: The following table maps the NT TIB structure to Xbox TIB
//
//     offsets
// NT_TIB  XBOX_TIB   contents
//  0x00     0x00     Current SEH Frame
//  0x04     0x04     Stack Limit
//  0x08     0x08     Stack Base
//   ??      0x20     Unknown
//   ??      0x24     Unknown
//  0x18     0x28     Ptr to TIB (self)

// ******************************************************************
// * KTHREAD (Kernel Thread)
// ******************************************************************
// * NOTE: INCOMPLETE!!
// * See http://www.nirsoft.net/kernel_struct/vista/KTHREAD.html
// ******************************************************************
type
  KTHREAD = record
    {0x00}Header: DISPATCHER_HEADER;
    {0x10}UnknownA: array [0..$18 - 1] of UCHAR;
    {0x28}TlsData: PVOID;
    {0x2C}UnknownB: array [0..$E4 - 1] of UCHAR;
  end; {=0x110}
  PKTHREAD = ^KTHREAD;

// ******************************************************************
// * ETHREAD (Executive Thread Object)
// ******************************************************************
// * NOTE: INCOMPLETE!!
// * See http://www.nirsoft.net/kernel_struct/vista/ETHREAD.html
// ******************************************************************
type
  ETHREAD = record
    {0x000}Tcb: KTHREAD;
    {0x110}CreateTime: LARGE_INTEGER;
    {0x118}ExitTime: LARGE_INTEGER;
    {union}case Integer of
      0: ( {0x120}ExitStatus: NTSTATUS);
      1: ( {0x120}OfsChain: PVOID;
    {0x124}UnknownA: array [0..$8 - 1] of UCHAR;
    {0x12C}UniqueThread: DWORD; // StrikerX3 NOTE: GetCurrentThreadId() returns this
    {0x130}StartAddress: PVOID;
    {0x134}IrpList: LIST_ENTRY;
//#ifdef DEVKIT
    {0x13C}DebugData: PVOID;
//#endif // DEVKIT
    ); // close last union case
  end; {=0x140}
  PETHREAD = ^ETHREAD;

const
  SIZE_OF_FX_REGISTERS = 128;

// DXBX Addition
type _FLOATING_SAVE_AREA = record
    {0x000}ControlWord: USHORT;
    {0x002}StatusWord: USHORT;
    {0x004}TagWord: USHORT;
    {0x006}ErrorOpcode: USHORT;
    {0x008}ErrorOffset: ULONG;
    {0x00C}ErrorSelector: ULONG;
    {0x010}DataOffset: ULONG;
    {0x014}DataSelector: ULONG;
    {0x018}MXCsr: ULONG;
    {0x01C}Reserved2: ULONG;
    {0x020}RegisterArea: array [0..SIZE_OF_FX_REGISTERS-1] of UCHAR;
    {0x0A0}XmmRegisterArea: array [0..SIZE_OF_FX_REGISTERS-1] of UCHAR;
    {0x120}Reserved4: array [0..224-1] of UCHAR;
    {0x200}Cr0NpxState: ULONG;
  end; {=0x204}
  FLOATING_SAVE_AREA = _FLOATING_SAVE_AREA;

// DXBX Addition
type _FX_SAVE_AREA  = record
    {0x000}FloatSave: FLOATING_SAVE_AREA;
    {0x204}Align16Byte: array [0..3-1] of ULONG;
  end; {=0x210}
  FX_SAVE_AREA = _FX_SAVE_AREA;
  PFX_SAVE_AREA = ^FX_SAVE_AREA;

// ******************************************************************
// * KPRCB - "Kernel Processor Region Control Block"
// ******************************************************************
// * See http://www.nirsoft.net/kernel_struct/vista/KPRCB.html
// ******************************************************************
type
  KPRCB = record
    {0x000}CurrentThread: PKTHREAD; // KPCR : 0x28
    {0x004}NextThread: PKTHREAD; // KPCR : 0x2C
    {0x008}IdleThread: PKTHREAD; // KPCR : 0x30
    {0x00C}NpxThread: PKTHREAD; // KPCR : 0x34

    {0x010}InterruptCount: ULONG;             // per processor counts
    {0x014}DpcTime: ULONG;
    {0x018}InterruptTime: ULONG;
    {0x01C}DebugDpcTime: ULONG;               // per dpc tick count

    // Kernel performance counters.
    {0x020}KeContextSwitches: ULONG;

    // DPC interrupt requested.
    {0x024}DpcInterruptRequested: ULONG;

    // DPC list head, spinlock, and count.
    {0x028}DpcListHead: LIST_ENTRY;
    {0x030}DpcRoutineActive: ULONG;
    {0x034}DpcStack: PVOID;

    // QuantumEnd indicator
    {0x038}QuantumEnd: ULONG;

    // Npx save area
    {0x03C}NpxSaveArea: FX_SAVE_AREA;

    // network stack handshaking for debug monitor
    {0x24C}DmEnetFunc: PVOID;

    // devkit specific data
    {0x250}DebugMonitorData: PVOID; // debugger global data
//#ifdef DEVKIT
    {0x254}DebugHaltThread: PVOID; // function for debug synchronization
    {0x258}DebugDoubleFault: PVOID; // double-fault handler
//#endif // DEVKIT

  end; {=0x25C}
  PKPRCB = ^KPRCB;

type
  KIRQL = ULONG; // Dxbx addition, to ensure this types takes 4 bytes (instead of 1)

// ******************************************************************
// * KPCR
// ******************************************************************
// *
// * NOTE: KPCR is the structure which exists at the FS: segment.
// * KPCR stands for : "Kernel Processor Control Region"
// *
// ******************************************************************
type
  // Dxbx note : Pointer-type is normally defined AFTER record, but now BEFORE because of recursive declaration :
  PKPCR = ^KPCR;
  _KPCR = record
    {0x00}NtTib: NT_TIB;   // Thread Information Block
    {0x1C}SelfPcr: PKPCR;  // flat address of this PCR
    {0x20}Prcb: PKPRCB;    // Pointer to Prcb (PrcbData?)
    {0x24}Irql: KIRQL;
    {0x28}PrcbData: KPRCB; // Kernel Processor Region Control Block
  end; {=0x284}
  KPCR = _KPCR;


// ******************************************************************
// * EEPROM_INDEX
// ******************************************************************
type
  (**** Convert following enum types to constants. ****
   **** e.g. v1 = n, where v1 is constant and n is the value ****
   **** if a constant has a value, do not assign a new value ****)
  EEPROM_INDEX = {enum}(
    EEPROM_LANGUAGE = $007,
    EEPROM_VIDEO    = $008,
    EEPROM_AUDIO    = $009,
    EEPROM_MISC     = $11
  );
  PEEPROM_INDEX = ^EEPROM_INDEX;

// ******************************************************************
// * XBOX_HARDWARE_INFO
// ******************************************************************
type
  XBOX_HARDWARE_INFO = record
    {0x00}Flags: ULONG;
    {0x04}Unknown1: UCHAR;
    {0x05}Unknown2: UCHAR;
    {0x06}Unknown3: UCHAR;
    {0x07}Unknown4: UCHAR;
  end; {=0x08}

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
    Result := *(volatile UCHAR )Address;
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
    Result := *(volatile USHORT )Address;
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
    Result := *(volatile ULONG )Address;
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
    // TODO : FInd out how this struct is defined
  end;
  OBJECT_TYPE = _OBJECT_TYPE;
  POBJECT_TYPE = ^OBJECT_TYPE;

//
// DXBX Addition : Xbox Refurb Info
//
type
  _XBOX_REFURB_INFO = record
    {0x00}Signature: DWORD;
    {0x04}PowerCycleCount: DWORD;
    {0x08}FirstBootTime: FILETIME;
  end; {=0x10}
  XBOX_REFURB_INFO = _XBOX_REFURB_INFO;
  PXBOX_REFURB_INFO = ^XBOX_REFURB_INFO;

//
// DXBX Addition : Xbox Interrupt Mode - Source: ReactOS
//
type
  KINTERRUPT_MODE = (LevelSensitive, Latched);

//
// DXBX Addition : Hardware Interface Type - Source: ReactOS
//
type
  INTERFACE_TYPE = (
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
  PINTERFACE_TYPE = ^INTERFACE_TYPE;

//
// DXBX Addition : Create Thread Notify Routine - Source: ReactOS
//
type
  PCREATE_THREAD_NOTIFY_ROUTINE = procedure(
    ProcessId: HANDLE;
    ThreadId: HANDLE;
    Create: LONGBOOL
    ); stdcall;

implementation

end.
