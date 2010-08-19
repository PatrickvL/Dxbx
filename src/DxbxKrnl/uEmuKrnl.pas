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
  uLog,
  uEmuFS;

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

  XBOX_HD_SECTOR_SIZE           = 512;

  XBOX_BOOT_SECTOR_INDEX        = 0;
  XBOX_REFURB_INFO_SECTOR_INDEX = 3;
  XBOX_CACHE_DB_SECTOR_INDEX    = 4;
  XBOX_CONFIG_SECTOR_INDEX      = 8;
  XBOX_NUM_CONFIG_SECTORS       = 8;

  XBOX_REFURB_INFO_SIGNATURE    = 'RFRB';

  XBOX_CONFIG_DATA_SIZE         = SizeOf(PXBOX_CONFIG_SECTOR(nil).Data);
  XBOX_CONFIG_SECTOR_BEGIN_SIGNATURE = $79132568;
  XBOX_CONFIG_SECTOR_END_SIGNATURE   = $AA550000;
  XBOX_CONFIG_VERSION                = $00000001;
  XBOX_CONFIG_SECTOR_COUNT           = $00000001;

  XDISK_SECTOR_SIZE             = XBOX_HD_SECTOR_SIZE;

  XDISK_UNPARTITIONED_SECTORS   = 1024; // Total number of reserved sectors (for XBOX_NUM_CONFIG_SECTORS and more?)
  XDISK_SHELL_PARTITION_SECTORS = 1024000; // Number of sectors in shell partition (number 2)
  XDISK_CACHE_PARTITION_SECTORS = 1536000; // Number of sectors in cache partition (number 3 and upwards)

  XDISK_DATA_PARTITION          = 1;
  XDISK_SHELL_PARTITION         = 2;
  XDISK_FIRST_CACHE_PARTITION   = 3;
  XDISK_F_PARTITION             = 6;

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

function Unimplemented(const aAPI: string): NTSTATUS;

procedure InitializeObjectAttributes(
  p: POBJECT_ATTRIBUTES;
  n: PANSI_STRING;
  a: ULONG;
  r: HANDLE;
  dummy: Pointer
  );

var xLaunchDataPage: LAUNCH_DATA_PAGE; // (pointed to by xboxkrnl_LaunchDataPage)

var {156}xboxkrnl_KeTickCount: DWORD = 0; // (updated by EmuUpdateTickCount)
// Source:Cxbx  Branch:Shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100

var {162}xboxkrnl_KiBugCheckData: array [0..5 - 1] of ULONG_PTR;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100

var {164}xboxkrnl_LaunchDataPage: PLAUNCH_DATA_PAGE = @xLaunchDataPage;
// Source:?  Branch:Dxbx  Translator:PatrickvL  Done:0

var {357}xboxkrnl_IdexChannelObject: array [0..$1000] of Byte; // TODO -oDxbx : Determine size, structure & filling behind this
// Source:?  Branch:Dxbx  Translator:PatrickvL  Done:0

// The following API names are derived from Pedro's APILogger V2
// See http://forums.xbox-scene.com/index.php?showtopic=456303

function {051} xboxkrnl_InterlockedCompareExchange(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  Exchange: LONG;
  Destination: PLONG;
  Comparand: LONG
  ): LONG; register;
function {052} xboxkrnl_InterlockedDecrement(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  Addend: PLONG // OUT, volatile
  ): LONG; register;
function {053} xboxkrnl_InterlockedIncrement(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  Addend: PLONG // OUT, volatile
  ): LONG; register;
function {054} xboxkrnl_InterlockedExchange(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  Value: LONG;
  Destination: PLONG
  ): LONG; register;
function {055} xboxkrnl_InterlockedExchangeAdd(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  Value: LONG;
  Addend: PLONG
  ): LONG; register;
function {056} xboxkrnl_InterlockedFlushSList(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  ListHead: PSLIST_HEADER
  ): PSINGLE_LIST_ENTRY; register;
function {057} xboxkrnl_InterlockedPopEntrySList(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  ListHead: PSLIST_HEADER
  ): PSLIST_ENTRY; register;
function {058} xboxkrnl_InterlockedPushEntrySList(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  ListEntry: PSLIST_ENTRY;
  ListHead: PSLIST_HEADER
  ): PSLIST_ENTRY; register;
function {160} xboxkrnl_KfRaiseIrql(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  NewIrql: KIRQL
  ): KIRQL; register;
function {161} xboxkrnl_KfLowerIrql(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  NewIrql: KIRQL
  ): KIRQL; register;
function {163} xboxkrnl_KiUnlockDispatcherDatabase(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {252} xboxkrnl_PhyGetLinkState(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {253} xboxkrnl_PhyInitialize(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure {329} xboxkrnl_READ_PORT_BUFFER_UCHAR(
  Port: PUCHAR;
  Buffer: PUCHAR;
  Count: ULONG
  ); stdcall;
procedure {330} xboxkrnl_READ_PORT_BUFFER_USHORT(
  Port: PUSHORT;
  Buffer: PUSHORT;
  Count: ULONG
  ); stdcall;
procedure {331} xboxkrnl_READ_PORT_BUFFER_ULONG(
  Port: PULONG;
  Buffer: PULONG;
  Count: ULONG
  ); stdcall;
procedure {332} xboxkrnl_WRITE_PORT_BUFFER_UCHAR(
  Port: PUCHAR;
  Buffer: PUCHAR;
  Count: ULONG
  ); stdcall;
procedure {333} xboxkrnl_WRITE_PORT_BUFFER_USHORT(
  Port: PUSHORT;
  Buffer: PUSHORT;
  Count: ULONG
  ); stdcall;
procedure {334} xboxkrnl_WRITE_PORT_BUFFER_ULONG(
  Port: PULONG;
  Buffer: PULONG;
  Count: ULONG
  ); stdcall;

function {000} xboxkrnl_UnknownAPI000(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {367} xboxkrnl_UnknownAPI367(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {368} xboxkrnl_UnknownAPI368(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {369} xboxkrnl_UnknownAPI369(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {370} xboxkrnl_UnknownAPI370(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {371} xboxkrnl_UnknownAPI371(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {372} xboxkrnl_UnknownAPI372(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {373} xboxkrnl_UnknownAPI373(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

const lfUnit = lfCxbx or lfKernel;


function Unimplemented(const aAPI: string): NTSTATUS;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  WriteLog('EmuKrnl : Unimplemented API : ' + aAPI);
  Result := STATUS_PROCEDURE_NOT_FOUND; // abuse a standard NT error code

//  // Make sure we're noticing this :
//  Beep(750, 500);
//  Sleep(1250);
end;

// Initializes an OBJECT_ATTRIBUTES.
//
// Differences from NT: SECURITY_DESCRIPTOR support is gone.
procedure InitializeObjectAttributes(
  p: POBJECT_ATTRIBUTES;
  n: PANSI_STRING;
  a: ULONG;
  r: HANDLE;
  dummy: Pointer
  );
// Branch:shogun  Unit:EmuNtDll.h  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  p.RootDirectory := r;
  p.Attributes := a;
  p.ObjectName := n;
end;

/// ##########
/// ########## Start of Xbox Kernel API's :
/// ##########

function {000} xboxkrnl_UnknownAPI000(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI000');
  EmuSwapFS(fsXbox);
end;

function {051} xboxkrnl_InterlockedCompareExchange(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Exchange: LONG;
  {1 ECX}Destination: PLONG;
  {3 stack}Comparand: LONG
  ): LONG; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : InterlockedCompareExchange' +
        #13#10'(' +
        #13#10'   Exchange           : 0x%.08X' +
        #13#10'   Destination        : 0x%.08X' +
        #13#10'   Comparand          : 0x%.08X' +
        #13#10');',
        [Exchange, Destination, Comparand]);

  Result := InterlockedCompareExchange({var}Destination^, Exchange, Comparand);
  EmuSwapFS(fsXbox);
end;

function {052} xboxkrnl_InterlockedDecrement(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}Addend: PLONG // OUT, volatile
  ): LONG; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : InterlockedDecrement' +
        #13#10'(' +
        #13#10'   Addend             : 0x%.08X' +
        #13#10');',
        [Addend]);

  Result := InterlockedDecrement({var}Addend^);
  EmuSwapFS(fsXbox);
end;

function {053} xboxkrnl_InterlockedIncrement(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}Addend: PLONG // OUT, volatile
  ): LONG; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : InterlockedIncrement' +
        #13#10'(' +
        #13#10'   Addend             : 0x%.08X' +
        #13#10');',
        [Addend]);

  Result := InterlockedIncrement({var}Addend^);
  EmuSwapFS(fsXbox);
end;

function {054} xboxkrnl_InterlockedExchange(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Value: LONG;
  {1 ECX}Destination: PLONG
  ): LONG; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : InterlockedExchange' +
        #13#10'(' +
        #13#10'   Value              : 0x%.08X' +
        #13#10'   Destination        : 0x%.08X' +
        #13#10');',
        [Value, Destination]);

  Result := InterlockedExchange({var}Destination^, Value);
  EmuSwapFS(fsXbox);
end;

function {055} xboxkrnl_InterlockedExchangeAdd(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Value: LONG;
  {1 ECX}Addend: PLONG
  ): LONG; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : InterlockedExchangeAdd' +
        #13#10'(' +
        #13#10'   Value              : 0x%.08X' +
        #13#10'   Addend             : 0x%.08X' +
        #13#10');',
        [Value, Addend]);

  Result := InterlockedExchangeAdd({var}Addend^, Value);
  EmuSwapFS(fsXbox);
end;

// Dxbx Note : The Xbox1 SINGLE_LIST strucures are the same as in WinNT
function {056} xboxkrnl_InterlockedFlushSList(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}ListHead: PSLIST_HEADER
  ): PSINGLE_LIST_ENTRY; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : InterlockedFlushSList' +
        #13#10'(' +
        #13#10'   ListHead           : 0x%.08X' +
        #13#10');',
        [ListHead]);

  Result := InterlockedFlushSList(ListHead);
  EmuSwapFS(fsXbox);
end;

function {057} xboxkrnl_InterlockedPopEntrySList(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}ListHead: PSLIST_HEADER
  ): PSLIST_ENTRY; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : InterlockedPopEntrySList' +
        #13#10'(' +
        #13#10'   ListHead           : 0x%.08X' +
        #13#10');',
        [ListHead]);

  Result := InterlockedPopEntrySList(ListHead);
  EmuSwapFS(fsXbox);
end;

function {058} xboxkrnl_InterlockedPushEntrySList(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}ListEntry: PSLIST_ENTRY;
  {1 ECX}ListHead: PSLIST_HEADER
  ): PSLIST_ENTRY; register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : InterlockedPopEntrySList' +
        #13#10'(' +
        #13#10'   ListEntry          : 0x%.08X' +
        #13#10'   ListHead           : 0x%.08X' +
        #13#10');',
        [ListEntry, ListHead]);

  Result := InterlockedPushEntrySList(ListHead, ListEntry);
  EmuSwapFS(fsXbox);
end;

// Raises the hardware priority (irql)
// NewIrql = Irql to raise to
// RETURN VALUE previous irq level
// NOTES Uses fastcall convention
function {160} xboxkrnl_KfRaiseIrql(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}NewIrql: KIRQL
  ): KIRQL; register; // VALIDATED fastcall simulation - See Translation guide
// Source:shogun  Revision:0.8.1-Pre2  Branch:Dxbx  Translator:shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : KfRaiseIrql' +
        #13#10'(' +
        #13#10'   NewIrql            : 0x%.08X' +
        #13#10');',
        [NewIrql]);

  EmuSwapFS(fsXbox);
  Result := 0;
end;

// Restores the irq level on the current processor
// ARGUMENTS NewIrql = Irql to lower to
// NOTES Uses fastcall convention
function {161} xboxkrnl_KfLowerIrql(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}NewIrql: KIRQL
  ): KIRQL; register; // VALIDATED fastcall simulation - See Translation guide
// Source:shogun  Revision:0.8.1-Pre2  Branch:Dxbx  Translator:shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : KfLowerIrql' +
        #13#10'(' +
        #13#10'   NewIrql            : 0x%.08X' +
        #13#10');',
        [NewIrql]);

  EmuSwapFS(fsXbox);
  Result := 0;
end;

function {163} xboxkrnl_KiUnlockDispatcherDatabase(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KiUnlockDispatcherDatabase');
  EmuSwapFS(fsXbox);
end;

function {252} xboxkrnl_PhyGetLinkState(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('PhyGetLinkState');
  EmuSwapFS(fsXbox);
end;

function {253} xboxkrnl_PhyInitialize(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('PhyInitialize');
  EmuSwapFS(fsXbox);
end;

procedure {329} xboxkrnl_READ_PORT_BUFFER_UCHAR(
  Port: PUCHAR;
  Buffer: PUCHAR;
  Count: ULONG
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

(*
EAD_PORT_BUFFER_UCHAR proc near

arg_0       = dword ptr  4
arg_4       = dword ptr  8
arg_8       = dword ptr  0Ch

        mov eax, edi
        mov edx, [esp+arg_0]
        mov edi, [esp+arg_4]
        mov ecx, [esp+arg_8]
        rep ins     byte ptr es:[edi],  dx
        mov edi, eax
        retn    0Ch
READ_PORT_BUFFER_UCHAR endp
*)

  Unimplemented('READ_PORT_BUFFER_UCHAR');
  EmuSwapFS(fsXbox);
end;

procedure {330} xboxkrnl_READ_PORT_BUFFER_USHORT(
  Port: PUSHORT;
  Buffer: PUSHORT;
  Count: ULONG
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

(*
READ_PORT_BUFFER_USHORT proc near

arg_0       = dword ptr  4
arg_4       = dword ptr  8
arg_8       = dword ptr  0Ch

        mov eax, edi
        mov edx, [esp+arg_0]
        mov edi, [esp+arg_4]
        mov ecx, [esp+arg_8]
        rep ins     word ptr es:[edi],  dx
        mov edi, eax
        retn    0Ch
READ_PORT_BUFFER_USHORT endp
*)

  Unimplemented('READ_PORT_BUFFER_USHORT');
  EmuSwapFS(fsXbox);
end;

procedure {331} xboxkrnl_READ_PORT_BUFFER_ULONG(
  Port: PULONG;
  Buffer: PULONG;
  Count: ULONG
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

(*READ_PORT_BUFFER_ULONG proc near

arg_0       = dword ptr  4
arg_4       = dword ptr  8
arg_8       = dword ptr  0Ch

        mov eax, edi
        mov edx, [esp+arg_0]
        mov edi, [esp+arg_4]
        mov ecx, [esp+arg_8]
        rep ins     dword ptr es:[edi], dx
        mov edi, eax
        retn    0Ch
READ_PORT_BUFFER_ULONG endp
*)

  Unimplemented('READ_PORT_BUFFER_ULONG');
  EmuSwapFS(fsXbox);
end;

procedure {332} xboxkrnl_WRITE_PORT_BUFFER_UCHAR(
  Port: PUCHAR;
  Buffer: PUCHAR;
  Count: ULONG
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('WRITE_PORT_BUFFER_UCHAR');
  EmuSwapFS(fsXbox);
end;

procedure {333} xboxkrnl_WRITE_PORT_BUFFER_USHORT(
  Port: PUSHORT;
  Buffer: PUSHORT;
  Count: ULONG
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

(*
        public WRITE_PORT_BUFFER_USHORT
WRITE_PORT_BUFFER_USHORT proc near

arg_0       = dword ptr  4
arg_4       = dword ptr  8
arg_8       = dword ptr  0Ch

        mov eax, esi
        mov edx, [esp+arg_0]
        mov esi, [esp+arg_4]
        mov ecx, [esp+arg_8]
        rep outsw
        mov esi, eax
        retn    0Ch
WRITE_PORT_BUFFER_USHORT endp
end
*)

  Unimplemented('WRITE_PORT_BUFFER_USHORT');
  EmuSwapFS(fsXbox);
end;

procedure {334} xboxkrnl_WRITE_PORT_BUFFER_ULONG(
  Port: PULONG;
  Buffer: PULONG;
  Count: ULONG
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('WRITE_PORT_BUFFER_ULONG');
  EmuSwapFS(fsXbox);
end;

function {367} xboxkrnl_UnknownAPI367(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI367');
  EmuSwapFS(fsXbox);
end;

function {368} xboxkrnl_UnknownAPI368(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI368');
  EmuSwapFS(fsXbox);
end;

function {369} xboxkrnl_UnknownAPI369(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI369');
  EmuSwapFS(fsXbox);
end;

function {370} xboxkrnl_UnknownAPI370(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI370');
  EmuSwapFS(fsXbox);
end;

function {371} xboxkrnl_UnknownAPI371(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI371');
  EmuSwapFS(fsXbox);
end;

function {372} xboxkrnl_UnknownAPI372(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI372');
  EmuSwapFS(fsXbox);
end;

function {373} xboxkrnl_UnknownAPI373(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI373');
  EmuSwapFS(fsXbox);
end;

end.
