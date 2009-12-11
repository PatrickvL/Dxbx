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
  uEmuXapi;

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
  Unknown = INT_PTR; // generic 32 bit argument type
  dtU32 = Unknown;
  dtObjectAttributes = Unknown;
  dtACCESS_MASK = Unknown;
  dtBLOB = Unknown;

function Unimplemented(const aAPI: string): NTSTATUS;

var
  {156}xboxkrnl_KeTickCount: DWord;
  {162}xboxkrnl_KiBugCheckData: array [0..5 - 1] of ULONG_PTR; // Source: ReactOS
  {164}xboxkrnl_LaunchDataPage: PLAUNCH_DATA_PAGE;

// The following API names are derived from Pedro's APILogger V2
// See http://forums.xbox-scene.com/index.php?showtopic=456303

function {051} xboxkrnl_InterlockedCompareExchange(
  var Destination: LONG; // out, volatile
  Exchange: LONG;
  Comparand: LONG
  ): LONG; stdcall; // Source: ReactOS
function {052} xboxkrnl_InterlockedDecrement(
  var Addend: LONG // out, volatile
  ): LONG; stdcall; // Source: ReactOS
function {053} xboxkrnl_InterlockedIncrement(
  var Addend: LONG // out, volatile
  ): LONG; stdcall; // Source: ReactOS
function {054} xboxkrnl_InterlockedExchange(
  var Destination: LONG; // out, volatile
  Value: LONG
  ): LONG; stdcall; // Source: ReactOS
function {055} xboxkrnl_InterlockedExchangeAdd(
  var Addend: LONG; // out, volatile
  Value: LONG
  ): LONG; stdcall; // Source: ReactOS
function {056} xboxkrnl_InterlockedFlushSList(
  ListHead: PSLIST_HEADER
  ): PSINGLE_LIST_ENTRY; stdcall; // Source: ReactOS
function {057} xboxkrnl_InterlockedPopEntrySList(
  ListHead: PSLIST_HEADER
  ): PSLIST_ENTRY; stdcall; // Source: ReactOS
function {058} xboxkrnl_InterlockedPushEntrySList(
  ListHead: PSLIST_HEADER;
  ListEntry: PSLIST_ENTRY
  ): PSLIST_ENTRY; stdcall; // Source: ReactOS
function {160} xboxkrnl_KfRaiseIrql(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  NewIrql: KIRQL
  ): KIRQL; register; // Source: ReactOS
procedure {161} xboxkrnl_KfLowerIrql(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  NewIrql: KIRQL
  ); register; // Source: ReactOS
function {163} xboxkrnl_KiUnlockDispatcherDatabase(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {252} xboxkrnl_PhyGetLinkState(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {253} xboxkrnl_PhyInitialize(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure {329} xboxkrnl_READ_PORT_BUFFER_UCHAR(
  Port: PUCHAR;
  Buffer: PUCHAR;
  Count: ULONG
  ); stdcall; // Source: ReactOS
procedure {330} xboxkrnl_READ_PORT_BUFFER_USHORT(
  Port: PUSHORT;
  Buffer: PUSHORT;
  Count: ULONG
  ); stdcall; // Source: ReactOS
procedure {331} xboxkrnl_READ_PORT_BUFFER_ULONG(
  Port: PULONG;
  Buffer: PULONG;
  Count: ULONG
  ); stdcall; // Source: ReactOS
procedure {332} xboxkrnl_WRITE_PORT_BUFFER_UCHAR(
  Port: PUCHAR;
  Buffer: PUCHAR;
  Count: ULONG
  ); stdcall; // Source: ReactOS
procedure {333} xboxkrnl_WRITE_PORT_BUFFER_USHORT(
  Port: PUSHORT;
  Buffer: PUSHORT;
  Count: ULONG
  ); stdcall; // Source: ReactOS
procedure {334} xboxkrnl_WRITE_PORT_BUFFER_ULONG(
  Port: PULONG;
  Buffer: PULONG;
  Count: ULONG
  ); stdcall; // Source: ReactOS
function {357} xboxkrnl_IdexChannelObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

function {000} xboxkrnl_UnknownAPI000(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {367} xboxkrnl_UnknownAPI367(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {368} xboxkrnl_UnknownAPI368(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {369} xboxkrnl_UnknownAPI369(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {370} xboxkrnl_UnknownAPI370(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {371} xboxkrnl_UnknownAPI371(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {372} xboxkrnl_UnknownAPI372(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {373} xboxkrnl_UnknownAPI373(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

function Unimplemented(const aAPI: string): NTSTATUS;
begin
  WriteLog('Unimplemented xboxkrnl EmuAPI : ' + aAPI);
  Result := STATUS_PROCEDURE_NOT_FOUND; // abuse a standard NT error code
end;

/// ##########
/// ########## Start of Xbox Kernel API's :
/// ##########

function {000} xboxkrnl_UnknownAPI000(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI000');
  EmuSwapFS(fsXbox);
end;

// Dxbx TODO : All Interlocked functions are FASTCALL in ReactOS,
// so we need to hack this just like SetVertexShaderConstant1 for example.

function {051} xboxkrnl_InterlockedCompareExchange(
  var Destination: LONG; // out, volatile
  Exchange: LONG;
  Comparand: LONG
  ): LONG; stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Result := InterlockedCompareExchange({var}Destination, Exchange, Comparand);
  EmuSwapFS(fsXbox);
end;

function {052} xboxkrnl_InterlockedDecrement(
  var Addend: LONG // out, volatile
  ): LONG; stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Result := InterlockedDecrement({var}Addend);
  EmuSwapFS(fsXbox);
end;

function {053} xboxkrnl_InterlockedIncrement(
  var Addend: LONG // out, volatile
  ): LONG; stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Result := InterlockedIncrement({var}Addend);
  EmuSwapFS(fsXbox);
end;

function {054} xboxkrnl_InterlockedExchange(
  var Destination: LONG; // out, volatile
  Value: LONG
  ): LONG; stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Result := InterlockedExchange({var}Destination, Value);
  EmuSwapFS(fsXbox);
end;

function {055} xboxkrnl_InterlockedExchangeAdd(
  var Addend: LONG; // out, volatile
  Value: LONG
  ): LONG; stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Result := InterlockedExchangeAdd({var}Addend, Value);
  EmuSwapFS(fsXbox);
end;

function {056} xboxkrnl_InterlockedFlushSList(
  ListHead: PSLIST_HEADER
  ): PSINGLE_LIST_ENTRY; stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  // Cxbx TODO : Can we safely assume that the Xbox LIST strucures are the same as WinXP's ?
  Result := InterlockedFlushSList(ListHead);
  EmuSwapFS(fsXbox);
end;

function {057} xboxkrnl_InterlockedPopEntrySList(
  ListHead: PSLIST_HEADER
  ): PSLIST_ENTRY; stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Result := InterlockedPopEntrySList(ListHead);
  EmuSwapFS(fsXbox);
end;

function {058} xboxkrnl_InterlockedPushEntrySList(
  ListHead: PSLIST_HEADER;
  ListEntry: PSLIST_ENTRY
  ): PSLIST_ENTRY; stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Result := InterlockedPushEntrySList(ListHead, ListEntry);
  EmuSwapFS(fsXbox);
end;

// Raises the hardware priority (irql)
// NewIrql = Irql to raise to
// RETURN VALUE previous irq level
// NOTES Uses fastcall convention

function {160} xboxkrnl_KfRaiseIrql(
  // Dxbx : This argument makes the 'register' calling convention
  // functionally equivalent to the 'fastcall' calling convention.
  // Quote from http://www.codeguru.com/forum/showthread.php?t=466266 :
  // They differ as follows:
  // register: (left to right) EAX, EDX, ECX, remaining pushed on stack right to left, callee cleans
  // fastcall: (left to right) ECX, EDX, remaining pushed on stack left to right, callee cleans
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  NewIrql: KIRQL // Dxbx note : This argument should be here, to force it into ECX
  ): KIRQL; register; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KfRaiseIrql');
  Result := Low(Result);
  EmuSwapFS(fsXbox);
end;

// Restores the irq level on the current processor
// ARGUMENTS NewIrql = Irql to lower to
// NOTES Uses fastcall convention
procedure {161} xboxkrnl_KfLowerIrql(
  // Dxbx : This argument makes the 'register' calling convention
  // functionally equivalent to the 'fastcall' calling convention.
  // Quote from http://www.codeguru.com/forum/showthread.php?t=466266 :
  // They differ as follows:
  // register: (left to right) EAX, EDX, ECX, remaining pushed on stack right to left, callee cleans
  // fastcall: (left to right) ECX, EDX, remaining pushed on stack left to right, callee cleans
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  NewIrql: KIRQL // Dxbx note : This argument should be here, to force it into ECX
  ); register; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KfLowerIrql');
  EmuSwapFS(fsXbox);
end;

function {163} xboxkrnl_KiUnlockDispatcherDatabase(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KiUnlockDispatcherDatabase');
  EmuSwapFS(fsXbox);
end;

function {252} xboxkrnl_PhyGetLinkState(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('PhyGetLinkState');
  EmuSwapFS(fsXbox);
end;

function {253} xboxkrnl_PhyInitialize(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('PhyInitialize');
  EmuSwapFS(fsXbox);
end;

procedure {329} xboxkrnl_READ_PORT_BUFFER_UCHAR(
  Port: PUCHAR;
  Buffer: PUCHAR;
  Count: ULONG
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Unimplemented('READ_PORT_BUFFER_UCHAR');
  EmuSwapFS(fsXbox);
end;

procedure {330} xboxkrnl_READ_PORT_BUFFER_USHORT(
  Port: PUSHORT;
  Buffer: PUSHORT;
  Count: ULONG
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Unimplemented('READ_PORT_BUFFER_USHORT');
  EmuSwapFS(fsXbox);
end;

procedure {331} xboxkrnl_READ_PORT_BUFFER_ULONG(
  Port: PULONG;
  Buffer: PULONG;
  Count: ULONG
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Unimplemented('READ_PORT_BUFFER_ULONG');
  EmuSwapFS(fsXbox);
end;

procedure {332} xboxkrnl_WRITE_PORT_BUFFER_UCHAR(
  Port: PUCHAR;
  Buffer: PUCHAR;
  Count: ULONG
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Unimplemented('WRITE_PORT_BUFFER_UCHAR');
  EmuSwapFS(fsXbox);
end;

procedure {333} xboxkrnl_WRITE_PORT_BUFFER_USHORT(
  Port: PUSHORT;
  Buffer: PUSHORT;
  Count: ULONG
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Unimplemented('WRITE_PORT_BUFFER_USHORT');
  EmuSwapFS(fsXbox);
end;

procedure {334} xboxkrnl_WRITE_PORT_BUFFER_ULONG(
  Port: PULONG;
  Buffer: PULONG;
  Count: ULONG
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(fsWindows);
  Unimplemented('WRITE_PORT_BUFFER_ULONG');
  EmuSwapFS(fsXbox);
end;

function {357} xboxkrnl_IdexChannelObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IdexChannelObject');
  EmuSwapFS(fsXbox);
end;

function {367} xboxkrnl_UnknownAPI367(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI367');
  EmuSwapFS(fsXbox);
end;

function {368} xboxkrnl_UnknownAPI368(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI368');
  EmuSwapFS(fsXbox);
end;

function {369} xboxkrnl_UnknownAPI369(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI369');
  EmuSwapFS(fsXbox);
end;

function {370} xboxkrnl_UnknownAPI370(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI370');
  EmuSwapFS(fsXbox);
end;

function {371} xboxkrnl_UnknownAPI371(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI371');
  EmuSwapFS(fsXbox);
end;

function {372} xboxkrnl_UnknownAPI372(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI372');
  EmuSwapFS(fsXbox);
end;

function {373} xboxkrnl_UnknownAPI373(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('UnknownAPI373');
  EmuSwapFS(fsXbox);
end;

end.
