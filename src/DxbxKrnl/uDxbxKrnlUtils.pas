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
unit uDxbxKrnlUtils;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  JwaWinNT,
  JwaNTStatus,
  JwaNative,
  // OpenXdk
  XboxKrnl, // POBJECT_ATTRIBUTES
  // Dxbx
  uConsts,
  uTypes,
  uLog, // for WriteLog
  uXBE; // PXBE_TLS

function XBE_FindSectionHeaderByName(pSectionName: PAnsiChar): PXBE_SECTIONHEADER;
function IsRunning(const aTitleID: DWORD): Boolean;

{$IF NOT DECLARED(YieldProcessor)}
procedure YieldProcessor;
{$IFEND}

procedure DxbxKrnlCleanup(const szErrorMessage: string); overload;
procedure DxbxKrnlCleanup(const szErrorMessage: string; const Args: array of const); overload;

function GetDWordBits(const Bits: DWORD; const aIndex: Integer): Integer;
procedure SetDWordBits(var Bits: DWORD; const aIndex: Integer; const aValue: Integer);
function GetByteBits(const Bits: Byte; const aIndex: Integer): Byte;
procedure SetByteBits(var Bits: Byte; const aIndex: Integer; const aValue: Byte);

function PSTRING_String(const aValue: PANSI_STRING): AnsiString;
function PUNICODE_STRING_String(const aValue: PUNICODE_STRING): AnsiString;
function POBJECT_ATTRIBUTES_String(const aValue: XboxKrnl.POBJECT_ATTRIBUTES): AnsiString;

function CreateDispositionToString(CreateDisposition: ULONG): string;
function FileAttributesToString(FileAttributes: ULONG): string;
function CreateOptionsToString(CreateOptions: ULONG): string;
function AccessMaskToString(AccessMask: ACCESS_MASK): string;
function ShareAccessToString(ShareAccess: ULONG): string;
function NTStatusToString(aStatus: NTSTATUS): string;
function FileInformationClassToString(FileInformationClass: FILE_INFORMATION_CLASS): string;
function FsInformationClassToString(FsInformationClass: FS_INFORMATION_CLASS): string;
function AllocationTypeToString(AllocationType: DWORD): string;

var
  // ! thread local storage
  DxbxKrnl_TLS: PXBE_TLS;
  // thread local storage data
  DxbxKrnl_TLSData: PVOID;
  // xbe header structure
  DxbxKrnl_XbeHeader: PXBEIMAGE_HEADER; // TODO -oDxbx : How about using g_XbeHeader instead?
  // parent window handle
  DxbxKrnl_hEmuParent: HWND;
  DxbxKrnl_XapiInitProcessExecuted: Boolean = False;

  // thread handles
  g_hThreads: array [0..MAXIMUM_XBOX_THREADS - 1] of Handle;

  SymbolScanOnly: Boolean = False;


implementation

uses
  uDxbxKrnl;

function XBE_FindSectionHeaderByName(pSectionName: PAnsiChar): PXBE_SECTIONHEADER;
var
  i: Integer;
begin
  if Assigned(DxbxKrnl_XbeHeader) then
  begin
    Result := PXBE_SECTIONHEADER(DxbxKrnl_XbeHeader.dwSectionHeadersAddr);
    i := DxbxKrnl_XbeHeader.dwSections;
    while i > 0 do
    begin
      if (0=strncmp(pSectionName, PAnsiChar(Result.dwSectionNameAddr), XBE_SECTIONNAME_MAXLENGTH)) then
      begin
        // Dxbx Note : Delphi 2010 has a bug: If this Exit is NOT surrounded by a begin/end pair,
        // calling Exit just leaves the statement-block, resulting in the reset of Result - so
        // LEAVE THESE begin/end intact!!!  -PatrickvL 2010-11-10
        Exit;
      end;

      Inc(Result);
      Dec(i);
    end;
  end;

  Result := nil;
end;

function IsRunning(const aTitleID: DWORD): Boolean;
begin
  Result := PXBE_CERTIFICATE(DxbxKrnl_XbeHeader.dwCertificateAddr).dwTitleId = aTitleID;
end;

procedure YieldProcessor;
asm
  pause; // rep nop;
end;

procedure DxbxKrnlCleanup(const szErrorMessage: string; const Args: array of const);
begin
  uDxbxKrnl._DxbxKrnlCleanup(szErrorMessage, Args);
end;

procedure DxbxKrnlCleanup(const szErrorMessage: string);
begin
  uDxbxKrnl._DxbxKrnlCleanup(szErrorMessage);
end;

// Tooling methods to get and set stretches of bits inside a DWORD,
// which is used to simulate C-like bit-fields in Delphi.
// See http://stackoverflow.com/questions/282019/how-to-simulate-bit-fields-in-delphi-records#282385
// Registers:               EAX                EDX               EAX
function GetDWordBits(const Bits: DWORD; const aIndex: Integer): Integer;
{$IFDEF PURE_PASCAL}
begin
  Result := (Bits shr {Offset=}(aIndex shr 8))
        and {Mask =}((1 shl {NrBits=}Byte(aIndex)) - 1);
end;
{$ELSE}
asm
  push ebx
  mov ebx, $00000001 // EBX = 1
  mov cl, dl         // CL = NrBits
  shl ebx, cl        // EBX = (1 shl NrBits)
  mov cl, dh         // CL = Offset
  dec ebx            // EBX = (1 shl NrBits) - 1 // = Mask
  shr eax, cl        // EAX = Bits shr Offset
  and eax, ebx       // EAX = (Bits shr Offset) and Mask
  pop ebx
end;
{$ENDIF}

// Registers:              EAX                EDX                    ECX
procedure SetDWordBits(var Bits: DWORD; const aIndex: Integer; const aValue: Integer);
{$IFDEF PURE_PASCAL}
var
  Offset: Byte;
  Mask: Integer;
begin
  Mask := ((1 shl {NrBits=}Byte(aIndex)) - 1);
  Assert(aValue <= Mask);

  Offset := aIndex shr 8;
  {var}Bits := (Bits and (not (Mask shl Offset))) or DWORD(aValue shl Offset);
end;
{$ELSE}
asm
  push ebx
  push ecx
  mov ebx, $00000001 // EBX = 1
  mov cl, dl         // CL = NrBits
  shl ebx, cl        // EBX = (1 shl NrBits)
  mov cl, dh         // CL = Offset
  dec ebx            // EBX = (1 shl NrBits) - 1 // = Mask
  shl ebx, cl        // EBX = Mask shl Offset
  not ebx            // EBX = not Mask
  and ebx,[eax]      // EBX = Bits and Mask // = MaskedBits
  pop edx            // EDX = aValue
  shl edx, cl        // EDX = aValue shl Offset // = NewBits
  or  edx, ebx       // EDX = MaskedBits or NewBits
  mov [eax], edx     // {var}Bits = EDX
  pop ebx
end;
{$ENDIF}

function GetByteBits(const Bits: Byte; const aIndex: Integer): Byte;
begin
  Result := (Bits shr {Offset=}(aIndex shr 8))
        and {Mask =}((1 shl {NrBits=}Byte(aIndex)) - 1);
end;

procedure SetByteBits(var Bits: Byte; const aIndex: Integer; const aValue: Byte);
var
  Offset: Byte;
  Mask: Byte;
begin
  Mask := ((1 shl {NrBits=}Byte(aIndex)) - 1);
  Assert(aValue <= Mask);

  Offset := aIndex shr 8;
  {var}Bits := (Bits and (not (Mask shl Offset))) or DWORD(aValue shl Offset);
end;

function PSTRING_String(const aValue: PANSI_STRING): AnsiString;
begin
  if Assigned(aValue) then
    SetString(Result, aValue.Buffer, aValue.Length)
  else
    Result := '';
end;

function PUNICODE_STRING_String(const aValue: PUNICODE_STRING): AnsiString;
var
  TmpUnicodeStr: UnicodeString;
begin
  if Assigned(aValue) then
  begin
    SetString(TmpUnicodeStr, aValue.Buffer, aValue.Length div 2 );
    Result := AnsiString(TmpUnicodeStr);
  end
  else
    Result := '';
end;

function POBJECT_ATTRIBUTES_String(const aValue: XboxKrnl.POBJECT_ATTRIBUTES): AnsiString;
begin
  if Assigned(aValue) then
    Result := PSTRING_String(aValue.ObjectName) // Dxbx note : This is ANSI on Xbox!
  else
    Result := '';
end;


function CreateDispositionToString(CreateDisposition: ULONG): string;
begin
  case CreateDisposition of
    FILE_SUPERSEDE: Result := 'FILE_SUPERSEDE';
    FILE_OPEN: Result := 'FILE_OPEN';
    FILE_CREATE: Result := 'FILE_CREATE';
    FILE_OPEN_IF: Result := 'FILE_OPEN_IF';
    FILE_OVERWRITE: Result := 'FILE_OVERWRITE';
    FILE_OVERWRITE_IF: Result := 'FILE_OVERWRITE_IF';
  else
    // TODO -oDxbx: Check if we ever encounter unhandled values, and what those might mean.
    Result := '';
  end;
end;

function AppendFlagStr(var Output: string; const Flags: ULONG; const Mask: ULONG; const Str: string): ULONG;
begin
  // Check for presence of all flag bits :
  if (Flags and Mask) = Mask then
  begin
    // Add it's string to the output :
    if Output <> '' then
      Output := Output + '|';

    Output := Output + Str;

    Result := Mask;
  end
  else
    Result := 0;
end;

function FileAttributesToString(FileAttributes: ULONG): string;
const
  FILE_ATTRIBUTE_OLD_DOS_VOLID = $00000008;
var
  HandledFlags: ULONG;
begin
  Result := '';

  // Handle individual attributes :
  HandledFlags := 0
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_READONLY, 'FILE_ATTRIBUTE_READONLY')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_HIDDEN, 'FILE_ATTRIBUTE_HIDDEN')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_SYSTEM, 'FILE_ATTRIBUTE_SYSTEM')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_DIRECTORY, 'FILE_ATTRIBUTE_DIRECTORY')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_ARCHIVE, 'FILE_ATTRIBUTE_ARCHIVE')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_DEVICE, 'FILE_ATTRIBUTE_DEVICE')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_NORMAL, 'FILE_ATTRIBUTE_NORMAL')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_TEMPORARY, 'FILE_ATTRIBUTE_TEMPORARY')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_SPARSE_FILE, 'FILE_ATTRIBUTE_SPARSE_FILE')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_REPARSE_POINT, 'FILE_ATTRIBUTE_REPARSE_POINT')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_COMPRESSED, 'FILE_ATTRIBUTE_COMPRESSED')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_OFFLINE, 'FILE_ATTRIBUTE_OFFLINE')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_NOT_CONTENT_INDEXED, 'FILE_ATTRIBUTE_NOT_CONTENT_INDEXED')
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_ENCRYPTED, 'FILE_ATTRIBUTE_ENCRYPTED')

    // Handle individual flags :
    or AppendFlagStr({var}Result, FileAttributes, FILE_FLAG_WRITE_THROUGH, 'FILE_FLAG_WRITE_THROUGH')
    or AppendFlagStr({var}Result, FileAttributes, FILE_FLAG_OVERLAPPED, 'FILE_FLAG_OVERLAPPED')
    or AppendFlagStr({var}Result, FileAttributes, FILE_FLAG_NO_BUFFERING, 'FILE_FLAG_NO_BUFFERING')
    or AppendFlagStr({var}Result, FileAttributes, FILE_FLAG_RANDOM_ACCESS, 'FILE_FLAG_RANDOM_ACCESS')
    or AppendFlagStr({var}Result, FileAttributes, FILE_FLAG_SEQUENTIAL_SCAN, 'FILE_FLAG_SEQUENTIAL_SCAN')
    or AppendFlagStr({var}Result, FileAttributes, FILE_FLAG_DELETE_ON_CLOSE, 'FILE_FLAG_DELETE_ON_CLOSE')
    or AppendFlagStr({var}Result, FileAttributes, FILE_FLAG_BACKUP_SEMANTICS, 'FILE_FLAG_BACKUP_SEMANTICS')
    or AppendFlagStr({var}Result, FileAttributes, FILE_FLAG_POSIX_SEMANTICS, 'FILE_FLAG_POSIX_SEMANTICS')
    or AppendFlagStr({var}Result, FileAttributes, FILE_FLAG_FIRST_PIPE_INSTANCE, 'FILE_FLAG_FIRST_PIPE_INSTANCE')

    // Handle invalid, xbox only flags :
    or AppendFlagStr({var}Result, FileAttributes, FILE_ATTRIBUTE_OLD_DOS_VOLID, 'FILE_ATTRIBUTE_OLD_DOS_VOLID');

  // Add any unhandled flags as hexadecimal :
  FileAttributes := FileAttributes and (not HandledFlags);
  if FileAttributes > 0 then
    Result := Result + '|0x' + IntToHex(FileAttributes, 8);
end;

function CreateOptionsToString(CreateOptions: ULONG): string;
var
  HandledFlags: ULONG;
begin
  Result := '';
  HandledFlags := 0;

  // Handle combined flags (which must be handled before the individual flags, hence the specific order) :
  if AppendFlagStr({var}Result, CreateOptions, FILE_STRUCTURED_STORAGE, 'FILE_STRUCTURED_STORAGE') > 0 then
    HandledFlags := HandledFlags or FILE_STRUCTURED_STORAGE;

  if AppendFlagStr({var}Result, CreateOptions, FILE_COPY_STRUCTURED_STORAGE, 'FILE_COPY_STRUCTURED_STORAGE') > 0 then
    HandledFlags := HandledFlags or FILE_COPY_STRUCTURED_STORAGE;

  // Remove the combined flags that we handled thus far, so they don't show up individually :
  CreateOptions := CreateOptions and (not HandledFlags);

  // Handle individual flags :
  HandledFlags := HandledFlags
    or AppendFlagStr({var}Result, CreateOptions, FILE_DIRECTORY_FILE, 'FILE_DIRECTORY_FILE')
    or AppendFlagStr({var}Result, CreateOptions, FILE_WRITE_THROUGH, 'FILE_WRITE_THROUGH')
    or AppendFlagStr({var}Result, CreateOptions, FILE_SEQUENTIAL_ONLY, 'FILE_SEQUENTIAL_ONLY')
    or AppendFlagStr({var}Result, CreateOptions, FILE_NO_INTERMEDIATE_BUFFERING, 'FILE_NO_INTERMEDIATE_BUFFERING')
    or AppendFlagStr({var}Result, CreateOptions, FILE_SYNCHRONOUS_IO_ALERT, 'FILE_SYNCHRONOUS_IO_ALERT')
    or AppendFlagStr({var}Result, CreateOptions, FILE_SYNCHRONOUS_IO_NONALERT, 'FILE_SYNCHRONOUS_IO_NONALERT')
    or AppendFlagStr({var}Result, CreateOptions, FILE_NON_DIRECTORY_FILE, 'FILE_NON_DIRECTORY_FILE')
    or AppendFlagStr({var}Result, CreateOptions, FILE_CREATE_TREE_CONNECTION, 'FILE_CREATE_TREE_CONNECTION')
    or AppendFlagStr({var}Result, CreateOptions, FILE_COMPLETE_IF_OPLOCKED, 'FILE_COMPLETE_IF_OPLOCKED')
    or AppendFlagStr({var}Result, CreateOptions, FILE_NO_EA_KNOWLEDGE, 'FILE_NO_EA_KNOWLEDGE')
    or AppendFlagStr({var}Result, CreateOptions, FILE_OPEN_FOR_RECOVERY, 'FILE_OPEN_FOR_RECOVERY')
    or AppendFlagStr({var}Result, CreateOptions, FILE_RANDOM_ACCESS, 'FILE_RANDOM_ACCESS')
    or AppendFlagStr({var}Result, CreateOptions, FILE_DELETE_ON_CLOSE, 'FILE_DELETE_ON_CLOSE')
    or AppendFlagStr({var}Result, CreateOptions, FILE_OPEN_BY_FILE_ID, 'FILE_OPEN_BY_FILE_ID')
    or AppendFlagStr({var}Result, CreateOptions, FILE_OPEN_FOR_BACKUP_INTENT, 'FILE_OPEN_FOR_BACKUP_INTENT')
    or AppendFlagStr({var}Result, CreateOptions, FILE_NO_COMPRESSION, 'FILE_NO_COMPRESSION')
    or AppendFlagStr({var}Result, CreateOptions, FILE_RESERVE_OPFILTER, 'FILE_RESERVE_OPFILTER')
    or AppendFlagStr({var}Result, CreateOptions, FILE_OPEN_REPARSE_POINT, 'FILE_OPEN_REPARSE_POINT')
    or AppendFlagStr({var}Result, CreateOptions, FILE_OPEN_NO_RECALL, 'FILE_OPEN_NO_RECALL')
    or AppendFlagStr({var}Result, CreateOptions, FILE_OPEN_FOR_FREE_SPACE_QUERY, 'FILE_OPEN_FOR_FREE_SPACE_QUERY');

//const FILE_VALID_OPTION_FLAGS =                 $00ffffff;
//const FILE_VALID_PIPE_OPTION_FLAGS =            $00000032;
//const FILE_VALID_MAILSLOT_OPTION_FLAGS =        $00000032;
//const FILE_VALID_SET_FLAGS =                    $00000036;

  // Add any unhandled flags as hexadecimal :
  CreateOptions := CreateOptions and (not HandledFlags);
  if CreateOptions > 0 then
    Result := Result + '|0x' + IntToHex(CreateOptions, 8);
end;

function AccessMaskToString(AccessMask: ACCESS_MASK): string;
var
  HandledFlags: ACCESS_MASK;
begin
  Result := '';

  // Handle generic flags :
  HandledFlags := 0
    or AppendFlagStr({var}Result, AccessMask, GENERIC_READ, 'GENERIC_READ')
    or AppendFlagStr({var}Result, AccessMask, GENERIC_WRITE, 'GENERIC_WRITE')
    or AppendFlagStr({var}Result, AccessMask, GENERIC_EXECUTE, 'GENERIC_EXECUTE')
    or AppendFlagStr({var}Result, AccessMask, GENERIC_ALL, 'GENERIC_ALL');

  // Handle combined flags (which must be handled before the individual flags, hence the specific order) :
  if AppendFlagStr({var}Result, AccessMask, STANDARD_RIGHTS_ALL, 'STANDARD_RIGHTS_ALL') > 0 then
    HandledFlags := HandledFlags or STANDARD_RIGHTS_ALL;

  if AppendFlagStr({var}Result, AccessMask, STANDARD_RIGHTS_REQUIRED, 'STANDARD_RIGHTS_REQUIRED') > 0 then
    HandledFlags := HandledFlags or STANDARD_RIGHTS_REQUIRED;

  // Remove the combined flags that we handled thus far, so they don't show up individually :
  AccessMask := AccessMask and (not HandledFlags);

  // Handle individual flags :
  HandledFlags := HandledFlags
    or AppendFlagStr({var}Result, AccessMask, DELETE, 'DELETE')
    or AppendFlagStr({var}Result, AccessMask, READ_CONTROL, 'READ_CONTROL')
    or AppendFlagStr({var}Result, AccessMask, WRITE_DAC, 'WRITE_DAC')
    or AppendFlagStr({var}Result, AccessMask, WRITE_OWNER, 'WRITE_OWNER')
    or AppendFlagStr({var}Result, AccessMask, SYNCHRONIZE, 'SYNCHRONIZE')

    or AppendFlagStr({var}Result, AccessMask, ACCESS_SYSTEM_SECURITY, 'ACCESS_SYSTEM_SECURITY')
    or AppendFlagStr({var}Result, AccessMask, MAXIMUM_ALLOWED, 'MAXIMUM_ALLOWED');

  // TODO -oDxbx: Maybe we should add a parameter to this function, to choose between various sets of meanings for SPECIFIC_RIGHTS_ALL.

  // Add any unhandled flags as hexadecimal :
  AccessMask := AccessMask and (not HandledFlags);
  if AccessMask > 0 then
    Result := Result + '|0x' + IntToHex(AccessMask, 8);
end;

function ShareAccessToString(ShareAccess: ULONG): string;
var
  HandledFlags: ACCESS_MASK;
begin
  Result := '';

  // Handle individual flags :
  HandledFlags := 0
    or AppendFlagStr({var}Result, ShareAccess, FILE_SHARE_READ, 'FILE_SHARE_READ')
    or AppendFlagStr({var}Result, ShareAccess, FILE_SHARE_WRITE, 'FILE_SHARE_WRITE')
    or AppendFlagStr({var}Result, ShareAccess, FILE_SHARE_DELETE, 'FILE_SHARE_DELETE');

  // TODO -oDxbx: Maybe we should add a parameter to this function, to choose between various sets of meanings for SPECIFIC_RIGHTS_ALL.

  // Add any unhandled flags as hexadecimal :
  ShareAccess := ShareAccess and (not HandledFlags);
  if ShareAccess > 0 then
    Result := Result + '|0x' + IntToHex(ShareAccess, 8);
end;

function NTStatusToString(aStatus: NTSTATUS): string;
begin
  case aStatus of
    // $00000000 :
    STATUS_SUCCESS: Result := 'STATUS_SUCCESS';
(*
STATUS_SEVERITY_WARNING          0x2
STATUS_SEVERITY_SUCCESS          0x0
STATUS_SEVERITY_INFORMATIONAL    0x1
STATUS_SEVERITY_ERROR            0x3

STATUS_WAIT_0                    ((NTSTATUS)0x00000000L)    // winnt
STATUS_WAIT_1                    ((NTSTATUS)0x00000001L)
STATUS_WAIT_2                    ((NTSTATUS)0x00000002L)
STATUS_WAIT_3                    ((NTSTATUS)0x00000003L)
STATUS_WAIT_63                   ((NTSTATUS)0x0000003FL)
STATUS_ABANDONED                        ((NTSTATUS)0x00000080L)
STATUS_ABANDONED_WAIT_0          ((NTSTATUS)0x00000080L)    // winnt
STATUS_ABANDONED_WAIT_63         ((NTSTATUS)0x000000BFL)
STATUS_USER_APC                  ((NTSTATUS)0x000000C0L)    // winnt
STATUS_KERNEL_APC                ((NTSTATUS)0x00000100L)
STATUS_ALERTED                   ((NTSTATUS)0x00000101L)
STATUS_TIMEOUT                   ((NTSTATUS)0x00000102L)    // winnt
*)
    STATUS_PENDING: Result := 'STATUS_PENDING';
(*
STATUS_REPARSE                   ((NTSTATUS)0x00000104L)
STATUS_MORE_ENTRIES              ((NTSTATUS)0x00000105L)
STATUS_NOT_ALL_ASSIGNED          ((NTSTATUS)0x00000106L)
STATUS_SOME_NOT_MAPPED           ((NTSTATUS)0x00000107L)
STATUS_OPLOCK_BREAK_IN_PROGRESS  ((NTSTATUS)0x00000108L)
STATUS_VOLUME_MOUNTED            ((NTSTATUS)0x00000109L)
STATUS_RXACT_COMMITTED           ((NTSTATUS)0x0000010AL)
STATUS_NOTIFY_CLEANUP            ((NTSTATUS)0x0000010BL)
STATUS_NOTIFY_ENUM_DIR           ((NTSTATUS)0x0000010CL)
STATUS_NO_QUOTAS_FOR_ACCOUNT     ((NTSTATUS)0x0000010DL)
STATUS_PRIMARY_TRANSPORT_CONNECT_FAILED ((NTSTATUS)0x0000010EL)
STATUS_PAGE_FAULT_TRANSITION     ((NTSTATUS)0x00000110L)
STATUS_PAGE_FAULT_DEMAND_ZERO    ((NTSTATUS)0x00000111L)
STATUS_PAGE_FAULT_COPY_ON_WRITE  ((NTSTATUS)0x00000112L)
STATUS_PAGE_FAULT_GUARD_PAGE     ((NTSTATUS)0x00000113L)
STATUS_PAGE_FAULT_PAGING_FILE    ((NTSTATUS)0x00000114L)
STATUS_CACHE_PAGE_LOCKED         ((NTSTATUS)0x00000115L)
STATUS_CRASH_DUMP                ((NTSTATUS)0x00000116L)
STATUS_BUFFER_ALL_ZEROS          ((NTSTATUS)0x00000117L)
STATUS_REPARSE_OBJECT            ((NTSTATUS)0x00000118L)
STATUS_RESOURCE_REQUIREMENTS_CHANGED ((NTSTATUS)0x00000119L)
STATUS_TRANSLATION_COMPLETE      ((NTSTATUS)0x00000120L)

STATUS_OBJECT_NAME_EXISTS        ((NTSTATUS)0x40000000L)
STATUS_THREAD_WAS_SUSPENDED      ((NTSTATUS)0x40000001L)
STATUS_WORKING_SET_LIMIT_RANGE   ((NTSTATUS)0x40000002L)
STATUS_IMAGE_NOT_AT_BASE         ((NTSTATUS)0x40000003L)
STATUS_LOCAL_USER_SESSION_KEY    ((NTSTATUS)0x40000006L)
STATUS_BAD_CURRENT_DIRECTORY     ((NTSTATUS)0x40000007L)
STATUS_SERIAL_MORE_WRITES        ((NTSTATUS)0x40000008L)
STATUS_REGISTRY_RECOVERED        ((NTSTATUS)0x40000009L)
STATUS_SERIAL_COUNTER_TIMEOUT    ((NTSTATUS)0x4000000CL)
STATUS_NULL_LM_PASSWORD          ((NTSTATUS)0x4000000DL)
STATUS_IMAGE_MACHINE_TYPE_MISMATCH ((NTSTATUS)0x4000000EL)
STATUS_RECEIVE_PARTIAL           ((NTSTATUS)0x4000000FL)
STATUS_RECEIVE_EXPEDITED         ((NTSTATUS)0x40000010L)
STATUS_RECEIVE_PARTIAL_EXPEDITED ((NTSTATUS)0x40000011L)
STATUS_EVENT_DONE                ((NTSTATUS)0x40000012L)
STATUS_EVENT_PENDING             ((NTSTATUS)0x40000013L)
STATUS_CHECKING_FILE_SYSTEM      ((NTSTATUS)0x40000014L)
STATUS_FATAL_APP_EXIT            ((NTSTATUS)0x40000015L)
STATUS_PREDEFINED_HANDLE         ((NTSTATUS)0x40000016L)
STATUS_WAS_UNLOCKED              ((NTSTATUS)0x40000017L)
STATUS_SERVICE_NOTIFICATION      ((NTSTATUS)0x40000018L)
STATUS_WAS_LOCKED                ((NTSTATUS)0x40000019L)
STATUS_LOG_HARD_ERROR            ((NTSTATUS)0x4000001AL)
STATUS_ALREADY_WIN32             ((NTSTATUS)0x4000001BL)
STATUS_WX86_UNSIMULATE           ((NTSTATUS)0x4000001CL)
STATUS_WX86_CONTINUE             ((NTSTATUS)0x4000001DL)
STATUS_WX86_SINGLE_STEP          ((NTSTATUS)0x4000001EL)
STATUS_WX86_BREAKPOINT           ((NTSTATUS)0x4000001FL)
STATUS_WX86_EXCEPTION_CONTINUE   ((NTSTATUS)0x40000020L)
STATUS_WX86_EXCEPTION_LASTCHANCE ((NTSTATUS)0x40000021L)
STATUS_WX86_EXCEPTION_CHAIN      ((NTSTATUS)0x40000022L)
STATUS_IMAGE_MACHINE_TYPE_MISMATCH_EXE ((NTSTATUS)0x40000023L)
STATUS_NO_YIELD_PERFORMED        ((NTSTATUS)0x40000024L)
STATUS_TIMER_RESUME_IGNORED      ((NTSTATUS)0x40000025L)
STATUS_ARBITRATION_UNHANDLED     ((NTSTATUS)0x40000026L)
STATUS_CARDBUS_NOT_SUPPORTED     ((NTSTATUS)0x40000027L)
STATUS_WX86_CREATEWX86TIB        ((NTSTATUS)0x40000028L)
STATUS_MP_PROCESSOR_MISMATCH     ((NTSTATUS)0x40000029L)
*)
    // $80000001 and up :
    STATUS_GUARD_PAGE_VIOLATION: Result := 'STATUS_GUARD_PAGE_VIOLATION';
    STATUS_DATATYPE_MISALIGNMENT: Result := 'STATUS_DATATYPE_MISALIGNMENT';
    STATUS_BREAKPOINT: Result := 'STATUS_BREAKPOINT';
    STATUS_SINGLE_STEP: Result := 'STATUS_SINGLE_STEP';
    STATUS_BUFFER_OVERFLOW: Result := 'STATUS_BUFFER_OVERFLOW';
    STATUS_NO_MORE_FILES: Result := 'STATUS_NO_MORE_FILES';
    STATUS_WAKE_SYSTEM_DEBUGGER: Result := 'STATUS_WAKE_SYSTEM_DEBUGGER';
    STATUS_HANDLES_CLOSED: Result := 'STATUS_HANDLES_CLOSED';
    STATUS_NO_INHERITANCE: Result := 'STATUS_NO_INHERITANCE';
    STATUS_GUID_SUBSTITUTION_MADE: Result := 'STATUS_GUID_SUBSTITUTION_MADE';
    STATUS_PARTIAL_COPY: Result := 'STATUS_PARTIAL_COPY';
    STATUS_DEVICE_PAPER_EMPTY: Result := 'STATUS_DEVICE_PAPER_EMPTY';
    STATUS_DEVICE_POWERED_OFF: Result := 'STATUS_DEVICE_POWERED_OFF';
    STATUS_DEVICE_OFF_LINE: Result := 'STATUS_DEVICE_OFF_LINE';
    STATUS_DEVICE_BUSY: Result := 'STATUS_DEVICE_BUSY';
    STATUS_NO_MORE_EAS: Result := 'STATUS_NO_MORE_EAS';
    STATUS_INVALID_EA_NAME: Result := 'STATUS_INVALID_EA_NAME';
    STATUS_EA_LIST_INCONSISTENT: Result := 'STATUS_EA_LIST_INCONSISTENT';
    STATUS_INVALID_EA_FLAG: Result := 'STATUS_INVALID_EA_FLAG';
    STATUS_VERIFY_REQUIRED: Result := 'STATUS_VERIFY_REQUIRED';
    STATUS_EXTRANEOUS_INFORMATION: Result := 'STATUS_EXTRANEOUS_INFORMATION';
    STATUS_RXACT_COMMIT_NECESSARY: Result := 'STATUS_RXACT_COMMIT_NECESSARY';
    STATUS_NO_MORE_ENTRIES: Result := 'STATUS_NO_MORE_ENTRIES';
    STATUS_FILEMARK_DETECTED: Result := 'STATUS_FILEMARK_DETECTED';
    STATUS_MEDIA_CHANGED: Result := 'STATUS_MEDIA_CHANGED';
    STATUS_BUS_RESET: Result := 'STATUS_BUS_RESET';
    STATUS_END_OF_MEDIA: Result := 'STATUS_END_OF_MEDIA';
    STATUS_BEGINNING_OF_MEDIA: Result := 'STATUS_BEGINNING_OF_MEDIA';
    STATUS_MEDIA_CHECK: Result := 'STATUS_MEDIA_CHECK';
    STATUS_SETMARK_DETECTED: Result := 'STATUS_SETMARK_DETECTED';
    STATUS_NO_DATA_DETECTED: Result := 'STATUS_NO_DATA_DETECTED';
    STATUS_REDIRECTOR_HAS_OPEN_HANDLES: Result := 'STATUS_REDIRECTOR_HAS_OPEN_HANDLES';
    STATUS_SERVER_HAS_OPEN_HANDLES: Result := 'STATUS_SERVER_HAS_OPEN_HANDLES';
    STATUS_ALREADY_DISCONNECTED: Result := 'STATUS_ALREADY_DISCONNECTED';
    STATUS_LONGJUMP: Result := 'STATUS_LONGJUMP';
    // $C0000001 and up :
    STATUS_UNSUCCESSFUL: Result := 'STATUS_UNSUCCESSFUL';
    STATUS_NOT_IMPLEMENTED: Result := 'STATUS_NOT_IMPLEMENTED';
    STATUS_INVALID_INFO_CLASS: Result := 'STATUS_INVALID_INFO_CLASS';
    STATUS_INFO_LENGTH_MISMATCH: Result := 'STATUS_INFO_LENGTH_MISMATCH';
    STATUS_ACCESS_VIOLATION: Result := 'STATUS_ACCESS_VIOLATION';
    STATUS_IN_PAGE_ERROR: Result := 'STATUS_IN_PAGE_ERROR';
    STATUS_PAGEFILE_QUOTA: Result := 'STATUS_PAGEFILE_QUOTA';
    STATUS_INVALID_HANDLE: Result := 'STATUS_INVALID_HANDLE';
    STATUS_BAD_INITIAL_STACK: Result := 'STATUS_BAD_INITIAL_STACK';
    STATUS_BAD_INITIAL_PC: Result := 'STATUS_BAD_INITIAL_PC';
    STATUS_INVALID_CID: Result := 'STATUS_INVALID_CID';
    STATUS_TIMER_NOT_CANCELED: Result := 'STATUS_TIMER_NOT_CANCELED';
    STATUS_INVALID_PARAMETER: Result := 'STATUS_INVALID_PARAMETER';
    STATUS_NO_SUCH_DEVICE: Result := 'STATUS_NO_SUCH_DEVICE';
    STATUS_NO_SUCH_FILE: Result := 'STATUS_NO_SUCH_FILE';
    STATUS_INVALID_DEVICE_REQUEST: Result := 'STATUS_INVALID_DEVICE_REQUEST';
    STATUS_END_OF_FILE: Result := 'STATUS_END_OF_FILE';
    STATUS_WRONG_VOLUME: Result := 'STATUS_WRONG_VOLUME';
    STATUS_NO_MEDIA_IN_DEVICE: Result := 'STATUS_NO_MEDIA_IN_DEVICE';
    STATUS_UNRECOGNIZED_MEDIA: Result := 'STATUS_UNRECOGNIZED_MEDIA';
    STATUS_NONEXISTENT_SECTOR: Result := 'STATUS_NONEXISTENT_SECTOR';
    STATUS_MORE_PROCESSING_REQUIRED: Result := 'STATUS_MORE_PROCESSING_REQUIRED';
    STATUS_NO_MEMORY: Result := 'STATUS_NO_MEMORY';
    STATUS_CONFLICTING_ADDRESSES: Result := 'STATUS_CONFLICTING_ADDRESSES';
    STATUS_NOT_MAPPED_VIEW: Result := 'STATUS_NOT_MAPPED_VIEW';
    STATUS_UNABLE_TO_FREE_VM: Result := 'STATUS_UNABLE_TO_FREE_VM';
    STATUS_UNABLE_TO_DELETE_SECTION: Result := 'STATUS_UNABLE_TO_DELETE_SECTION';
    STATUS_INVALID_SYSTEM_SERVICE: Result := 'STATUS_INVALID_SYSTEM_SERVICE';
    STATUS_ILLEGAL_INSTRUCTION: Result := 'STATUS_ILLEGAL_INSTRUCTION';
    STATUS_INVALID_LOCK_SEQUENCE: Result := 'STATUS_INVALID_LOCK_SEQUENCE';
    STATUS_INVALID_VIEW_SIZE: Result := 'STATUS_INVALID_VIEW_SIZE';
    STATUS_INVALID_FILE_FOR_SECTION: Result := 'STATUS_INVALID_FILE_FOR_SECTION';
    STATUS_ALREADY_COMMITTED: Result := 'STATUS_ALREADY_COMMITTED';
    STATUS_ACCESS_DENIED: Result := 'STATUS_ACCESS_DENIED';
    STATUS_BUFFER_TOO_SMALL: Result := 'STATUS_BUFFER_TOO_SMALL';
    STATUS_OBJECT_TYPE_MISMATCH: Result := 'STATUS_OBJECT_TYPE_MISMATCH';
    STATUS_NONCONTINUABLE_EXCEPTION: Result := 'STATUS_NONCONTINUABLE_EXCEPTION';
    STATUS_INVALID_DISPOSITION: Result := 'STATUS_INVALID_DISPOSITION';
    STATUS_UNWIND: Result := 'STATUS_UNWIND';
    STATUS_BAD_STACK: Result := 'STATUS_BAD_STACK';
    STATUS_INVALID_UNWIND_TARGET: Result := 'STATUS_INVALID_UNWIND_TARGET';
    STATUS_NOT_LOCKED: Result := 'STATUS_NOT_LOCKED';
    STATUS_PARITY_ERROR: Result := 'STATUS_PARITY_ERROR';
    STATUS_UNABLE_TO_DECOMMIT_VM: Result := 'STATUS_UNABLE_TO_DECOMMIT_VM';
    STATUS_NOT_COMMITTED: Result := 'STATUS_NOT_COMMITTED';
    STATUS_INVALID_PORT_ATTRIBUTES: Result := 'STATUS_INVALID_PORT_ATTRIBUTES';
    STATUS_PORT_MESSAGE_TOO_LONG: Result := 'STATUS_PORT_MESSAGE_TOO_LONG';
    STATUS_INVALID_PARAMETER_MIX: Result := 'STATUS_INVALID_PARAMETER_MIX';
    STATUS_INVALID_QUOTA_LOWER: Result := 'STATUS_INVALID_QUOTA_LOWER';
    STATUS_DISK_CORRUPT_ERROR: Result := 'STATUS_DISK_CORRUPT_ERROR';
    STATUS_OBJECT_NAME_INVALID: Result := 'STATUS_OBJECT_NAME_INVALID';
    STATUS_OBJECT_NAME_NOT_FOUND: Result := 'STATUS_OBJECT_NAME_NOT_FOUND';
    STATUS_OBJECT_NAME_COLLISION: Result := 'STATUS_OBJECT_NAME_COLLISION';
    STATUS_PORT_DISCONNECTED: Result := 'STATUS_PORT_DISCONNECTED';
    STATUS_DEVICE_ALREADY_ATTACHED: Result := 'STATUS_DEVICE_ALREADY_ATTACHED';
    STATUS_OBJECT_PATH_INVALID: Result := 'STATUS_OBJECT_PATH_INVALID';
    STATUS_OBJECT_PATH_NOT_FOUND: Result := 'STATUS_OBJECT_PATH_NOT_FOUND';
    STATUS_OBJECT_PATH_SYNTAX_BAD: Result := 'STATUS_OBJECT_PATH_SYNTAX_BAD';
    STATUS_DATA_OVERRUN: Result := 'STATUS_DATA_OVERRUN';
    STATUS_DATA_LATE_ERROR: Result := 'STATUS_DATA_LATE_ERROR';
    STATUS_DATA_ERROR: Result := 'STATUS_DATA_ERROR';
    STATUS_CRC_ERROR: Result := 'STATUS_CRC_ERROR';
    STATUS_SECTION_TOO_BIG: Result := 'STATUS_SECTION_TOO_BIG';
    STATUS_PORT_CONNECTION_REFUSED: Result := 'STATUS_PORT_CONNECTION_REFUSED';
    STATUS_INVALID_PORT_HANDLE: Result := 'STATUS_INVALID_PORT_HANDLE';
    STATUS_SHARING_VIOLATION: Result := 'STATUS_SHARING_VIOLATION';
    STATUS_QUOTA_EXCEEDED: Result := 'STATUS_QUOTA_EXCEEDED';
    STATUS_INVALID_PAGE_PROTECTION: Result := 'STATUS_INVALID_PAGE_PROTECTION';
    STATUS_MUTANT_NOT_OWNED: Result := 'STATUS_MUTANT_NOT_OWNED';
    STATUS_SEMAPHORE_LIMIT_EXCEEDED: Result := 'STATUS_SEMAPHORE_LIMIT_EXCEEDED';
    STATUS_PORT_ALREADY_SET: Result := 'STATUS_PORT_ALREADY_SET';
    STATUS_SECTION_NOT_IMAGE: Result := 'STATUS_SECTION_NOT_IMAGE';
    STATUS_SUSPEND_COUNT_EXCEEDED: Result := 'STATUS_SUSPEND_COUNT_EXCEEDED';
    STATUS_THREAD_IS_TERMINATING: Result := 'STATUS_THREAD_IS_TERMINATING';
    STATUS_BAD_WORKING_SET_LIMIT: Result := 'STATUS_BAD_WORKING_SET_LIMIT';
    STATUS_INCOMPATIBLE_FILE_MAP: Result := 'STATUS_INCOMPATIBLE_FILE_MAP';
    STATUS_SECTION_PROTECTION: Result := 'STATUS_SECTION_PROTECTION';
    STATUS_EAS_NOT_SUPPORTED: Result := 'STATUS_EAS_NOT_SUPPORTED';
    STATUS_EA_TOO_LARGE: Result := 'STATUS_EA_TOO_LARGE';
    STATUS_NONEXISTENT_EA_ENTRY: Result := 'STATUS_NONEXISTENT_EA_ENTRY';
    STATUS_NO_EAS_ON_FILE: Result := 'STATUS_NO_EAS_ON_FILE';
    STATUS_EA_CORRUPT_ERROR: Result := 'STATUS_EA_CORRUPT_ERROR';
    STATUS_FILE_LOCK_CONFLICT: Result := 'STATUS_FILE_LOCK_CONFLICT';
    STATUS_LOCK_NOT_GRANTED: Result := 'STATUS_LOCK_NOT_GRANTED';
    STATUS_DELETE_PENDING: Result := 'STATUS_DELETE_PENDING';
    STATUS_CTL_FILE_NOT_SUPPORTED: Result := 'STATUS_CTL_FILE_NOT_SUPPORTED';
    STATUS_UNKNOWN_REVISION: Result := 'STATUS_UNKNOWN_REVISION';
    STATUS_REVISION_MISMATCH: Result := 'STATUS_REVISION_MISMATCH';
    STATUS_INVALID_OWNER: Result := 'STATUS_INVALID_OWNER';
    STATUS_INVALID_PRIMARY_GROUP: Result := 'STATUS_INVALID_PRIMARY_GROUP';
    STATUS_NO_IMPERSONATION_TOKEN: Result := 'STATUS_NO_IMPERSONATION_TOKEN';
    STATUS_CANT_DISABLE_MANDATORY: Result := 'STATUS_CANT_DISABLE_MANDATORY';
    STATUS_NO_LOGON_SERVERS: Result := 'STATUS_NO_LOGON_SERVERS';
    STATUS_NO_SUCH_LOGON_SESSION: Result := 'STATUS_NO_SUCH_LOGON_SESSION';
    STATUS_NO_SUCH_PRIVILEGE: Result := 'STATUS_NO_SUCH_PRIVILEGE';
    STATUS_PRIVILEGE_NOT_HELD: Result := 'STATUS_PRIVILEGE_NOT_HELD';
    STATUS_INVALID_ACCOUNT_NAME: Result := 'STATUS_INVALID_ACCOUNT_NAME';
    STATUS_USER_EXISTS: Result := 'STATUS_USER_EXISTS';
    STATUS_NO_SUCH_USER: Result := 'STATUS_NO_SUCH_USER';
    STATUS_GROUP_EXISTS: Result := 'STATUS_GROUP_EXISTS';
    STATUS_NO_SUCH_GROUP: Result := 'STATUS_NO_SUCH_GROUP';
    STATUS_MEMBER_IN_GROUP: Result := 'STATUS_MEMBER_IN_GROUP';
    STATUS_MEMBER_NOT_IN_GROUP: Result := 'STATUS_MEMBER_NOT_IN_GROUP';
    STATUS_LAST_ADMIN: Result := 'STATUS_LAST_ADMIN';
    STATUS_WRONG_PASSWORD: Result := 'STATUS_WRONG_PASSWORD';
    STATUS_ILL_FORMED_PASSWORD: Result := 'STATUS_ILL_FORMED_PASSWORD';
    STATUS_PASSWORD_RESTRICTION: Result := 'STATUS_PASSWORD_RESTRICTION';
    STATUS_LOGON_FAILURE: Result := 'STATUS_LOGON_FAILURE';
    STATUS_ACCOUNT_RESTRICTION: Result := 'STATUS_ACCOUNT_RESTRICTION';
    STATUS_INVALID_LOGON_HOURS: Result := 'STATUS_INVALID_LOGON_HOURS';
    STATUS_INVALID_WORKSTATION: Result := 'STATUS_INVALID_WORKSTATION';
    STATUS_PASSWORD_EXPIRED: Result := 'STATUS_PASSWORD_EXPIRED';
    STATUS_ACCOUNT_DISABLED: Result := 'STATUS_ACCOUNT_DISABLED';
    STATUS_NONE_MAPPED: Result := 'STATUS_NONE_MAPPED';
    STATUS_TOO_MANY_LUIDS_REQUESTED: Result := 'STATUS_TOO_MANY_LUIDS_REQUESTED';
    STATUS_LUIDS_EXHAUSTED: Result := 'STATUS_LUIDS_EXHAUSTED';
    STATUS_INVALID_SUB_AUTHORITY: Result := 'STATUS_INVALID_SUB_AUTHORITY';
    STATUS_INVALID_ACL: Result := 'STATUS_INVALID_ACL';
    STATUS_INVALID_SID: Result := 'STATUS_INVALID_SID';
    STATUS_INVALID_SECURITY_DESCR: Result := 'STATUS_INVALID_SECURITY_DESCR';
    STATUS_PROCEDURE_NOT_FOUND: Result := 'STATUS_PROCEDURE_NOT_FOUND';
    STATUS_INVALID_IMAGE_FORMAT: Result := 'STATUS_INVALID_IMAGE_FORMAT';
    STATUS_NO_TOKEN: Result := 'STATUS_NO_TOKEN';
    STATUS_BAD_INHERITANCE_ACL: Result := 'STATUS_BAD_INHERITANCE_ACL';
    STATUS_RANGE_NOT_LOCKED: Result := 'STATUS_RANGE_NOT_LOCKED';
    STATUS_DISK_FULL: Result := 'STATUS_DISK_FULL';
    STATUS_SERVER_DISABLED: Result := 'STATUS_SERVER_DISABLED';
    STATUS_SERVER_NOT_DISABLED: Result := 'STATUS_SERVER_NOT_DISABLED';
    STATUS_TOO_MANY_GUIDS_REQUESTED: Result := 'STATUS_TOO_MANY_GUIDS_REQUESTED';
    STATUS_GUIDS_EXHAUSTED: Result := 'STATUS_GUIDS_EXHAUSTED';
    STATUS_INVALID_ID_AUTHORITY: Result := 'STATUS_INVALID_ID_AUTHORITY';
    STATUS_AGENTS_EXHAUSTED: Result := 'STATUS_AGENTS_EXHAUSTED';
    STATUS_INVALID_VOLUME_LABEL: Result := 'STATUS_INVALID_VOLUME_LABEL';
    STATUS_SECTION_NOT_EXTENDED: Result := 'STATUS_SECTION_NOT_EXTENDED';
    STATUS_NOT_MAPPED_DATA: Result := 'STATUS_NOT_MAPPED_DATA';
    STATUS_RESOURCE_DATA_NOT_FOUND: Result := 'STATUS_RESOURCE_DATA_NOT_FOUND';
    STATUS_RESOURCE_TYPE_NOT_FOUND: Result := 'STATUS_RESOURCE_TYPE_NOT_FOUND';
    STATUS_RESOURCE_NAME_NOT_FOUND: Result := 'STATUS_RESOURCE_NAME_NOT_FOUND';
    STATUS_ARRAY_BOUNDS_EXCEEDED: Result := 'STATUS_ARRAY_BOUNDS_EXCEEDED';
    STATUS_FLOAT_DENORMAL_OPERAND: Result := 'STATUS_FLOAT_DENORMAL_OPERAND';
    STATUS_FLOAT_DIVIDE_BY_ZERO: Result := 'STATUS_FLOAT_DIVIDE_BY_ZERO';
    STATUS_FLOAT_INEXACT_RESULT: Result := 'STATUS_FLOAT_INEXACT_RESULT';
    STATUS_FLOAT_INVALID_OPERATION: Result := 'STATUS_FLOAT_INVALID_OPERATION';
    STATUS_FLOAT_OVERFLOW: Result := 'STATUS_FLOAT_OVERFLOW';
    STATUS_FLOAT_STACK_CHECK: Result := 'STATUS_FLOAT_STACK_CHECK';
    STATUS_FLOAT_UNDERFLOW: Result := 'STATUS_FLOAT_UNDERFLOW';
    STATUS_INTEGER_DIVIDE_BY_ZERO: Result := 'STATUS_INTEGER_DIVIDE_BY_ZERO';
    STATUS_INTEGER_OVERFLOW: Result := 'STATUS_INTEGER_OVERFLOW';
    STATUS_PRIVILEGED_INSTRUCTION: Result := 'STATUS_PRIVILEGED_INSTRUCTION';
    STATUS_TOO_MANY_PAGING_FILES: Result := 'STATUS_TOO_MANY_PAGING_FILES';
    STATUS_FILE_INVALID: Result := 'STATUS_FILE_INVALID';
    STATUS_ALLOTTED_SPACE_EXCEEDED: Result := 'STATUS_ALLOTTED_SPACE_EXCEEDED';
    STATUS_INSUFFICIENT_RESOURCES: Result := 'STATUS_INSUFFICIENT_RESOURCES';
    STATUS_DFS_EXIT_PATH_FOUND: Result := 'STATUS_DFS_EXIT_PATH_FOUND';
    STATUS_DEVICE_DATA_ERROR: Result := 'STATUS_DEVICE_DATA_ERROR';
    STATUS_DEVICE_NOT_CONNECTED: Result := 'STATUS_DEVICE_NOT_CONNECTED';
    STATUS_DEVICE_POWER_FAILURE: Result := 'STATUS_DEVICE_POWER_FAILURE';
    STATUS_FREE_VM_NOT_AT_BASE: Result := 'STATUS_FREE_VM_NOT_AT_BASE';
    STATUS_MEMORY_NOT_ALLOCATED: Result := 'STATUS_MEMORY_NOT_ALLOCATED';
    STATUS_WORKING_SET_QUOTA: Result := 'STATUS_WORKING_SET_QUOTA';
    STATUS_MEDIA_WRITE_PROTECTED: Result := 'STATUS_MEDIA_WRITE_PROTECTED';
    STATUS_DEVICE_NOT_READY: Result := 'STATUS_DEVICE_NOT_READY';
    STATUS_INVALID_GROUP_ATTRIBUTES: Result := 'STATUS_INVALID_GROUP_ATTRIBUTES';
    STATUS_BAD_IMPERSONATION_LEVEL: Result := 'STATUS_BAD_IMPERSONATION_LEVEL';
    STATUS_CANT_OPEN_ANONYMOUS: Result := 'STATUS_CANT_OPEN_ANONYMOUS';
    STATUS_BAD_VALIDATION_CLASS: Result := 'STATUS_BAD_VALIDATION_CLASS';
    STATUS_BAD_TOKEN_TYPE: Result := 'STATUS_BAD_TOKEN_TYPE';
    STATUS_BAD_MASTER_BOOT_RECORD: Result := 'STATUS_BAD_MASTER_BOOT_RECORD';
    STATUS_INSTRUCTION_MISALIGNMENT: Result := 'STATUS_INSTRUCTION_MISALIGNMENT';
    STATUS_INSTANCE_NOT_AVAILABLE: Result := 'STATUS_INSTANCE_NOT_AVAILABLE';
    STATUS_PIPE_NOT_AVAILABLE: Result := 'STATUS_PIPE_NOT_AVAILABLE';
    STATUS_INVALID_PIPE_STATE: Result := 'STATUS_INVALID_PIPE_STATE';
    STATUS_PIPE_BUSY: Result := 'STATUS_PIPE_BUSY';
    STATUS_ILLEGAL_FUNCTION: Result := 'STATUS_ILLEGAL_FUNCTION';
    STATUS_PIPE_DISCONNECTED: Result := 'STATUS_PIPE_DISCONNECTED';
    STATUS_PIPE_CLOSING: Result := 'STATUS_PIPE_CLOSING';
    STATUS_PIPE_CONNECTED: Result := 'STATUS_PIPE_CONNECTED';
    STATUS_PIPE_LISTENING: Result := 'STATUS_PIPE_LISTENING';
    STATUS_INVALID_READ_MODE: Result := 'STATUS_INVALID_READ_MODE';
    STATUS_IO_TIMEOUT: Result := 'STATUS_IO_TIMEOUT';
    STATUS_FILE_FORCED_CLOSED: Result := 'STATUS_FILE_FORCED_CLOSED';
    STATUS_PROFILING_NOT_STARTED: Result := 'STATUS_PROFILING_NOT_STARTED';
    STATUS_PROFILING_NOT_STOPPED: Result := 'STATUS_PROFILING_NOT_STOPPED';
    STATUS_COULD_NOT_INTERPRET: Result := 'STATUS_COULD_NOT_INTERPRET';
    STATUS_FILE_IS_A_DIRECTORY: Result := 'STATUS_FILE_IS_A_DIRECTORY';
    STATUS_NOT_SUPPORTED: Result := 'STATUS_NOT_SUPPORTED';
    STATUS_REMOTE_NOT_LISTENING: Result := 'STATUS_REMOTE_NOT_LISTENING';
    STATUS_DUPLICATE_NAME: Result := 'STATUS_DUPLICATE_NAME';
    STATUS_BAD_NETWORK_PATH: Result := 'STATUS_BAD_NETWORK_PATH';
    STATUS_NETWORK_BUSY: Result := 'STATUS_NETWORK_BUSY';
    STATUS_DEVICE_DOES_NOT_EXIST: Result := 'STATUS_DEVICE_DOES_NOT_EXIST';
    STATUS_TOO_MANY_COMMANDS: Result := 'STATUS_TOO_MANY_COMMANDS';
    STATUS_ADAPTER_HARDWARE_ERROR: Result := 'STATUS_ADAPTER_HARDWARE_ERROR';
    STATUS_INVALID_NETWORK_RESPONSE: Result := 'STATUS_INVALID_NETWORK_RESPONSE';
    STATUS_UNEXPECTED_NETWORK_ERROR: Result := 'STATUS_UNEXPECTED_NETWORK_ERROR';
    STATUS_BAD_REMOTE_ADAPTER: Result := 'STATUS_BAD_REMOTE_ADAPTER';
    STATUS_PRINT_QUEUE_FULL: Result := 'STATUS_PRINT_QUEUE_FULL';
    STATUS_NO_SPOOL_SPACE: Result := 'STATUS_NO_SPOOL_SPACE';
    STATUS_PRINT_CANCELLED: Result := 'STATUS_PRINT_CANCELLED';
    STATUS_NETWORK_NAME_DELETED: Result := 'STATUS_NETWORK_NAME_DELETED';
    STATUS_NETWORK_ACCESS_DENIED: Result := 'STATUS_NETWORK_ACCESS_DENIED';
    STATUS_BAD_DEVICE_TYPE: Result := 'STATUS_BAD_DEVICE_TYPE';
    STATUS_BAD_NETWORK_NAME: Result := 'STATUS_BAD_NETWORK_NAME';
    STATUS_TOO_MANY_NAMES: Result := 'STATUS_TOO_MANY_NAMES';
    STATUS_TOO_MANY_SESSIONS: Result := 'STATUS_TOO_MANY_SESSIONS';
    STATUS_SHARING_PAUSED: Result := 'STATUS_SHARING_PAUSED';
    STATUS_REQUEST_NOT_ACCEPTED: Result := 'STATUS_REQUEST_NOT_ACCEPTED';
    STATUS_REDIRECTOR_PAUSED: Result := 'STATUS_REDIRECTOR_PAUSED';
    STATUS_NET_WRITE_FAULT: Result := 'STATUS_NET_WRITE_FAULT';
    STATUS_PROFILING_AT_LIMIT: Result := 'STATUS_PROFILING_AT_LIMIT';
    STATUS_NOT_SAME_DEVICE: Result := 'STATUS_NOT_SAME_DEVICE';
    STATUS_FILE_RENAMED: Result := 'STATUS_FILE_RENAMED';
    STATUS_VIRTUAL_CIRCUIT_CLOSED: Result := 'STATUS_VIRTUAL_CIRCUIT_CLOSED';
    STATUS_NO_SECURITY_ON_OBJECT: Result := 'STATUS_NO_SECURITY_ON_OBJECT';
    STATUS_CANT_WAIT: Result := 'STATUS_CANT_WAIT';
    STATUS_PIPE_EMPTY: Result := 'STATUS_PIPE_EMPTY';
    STATUS_CANT_ACCESS_DOMAIN_INFO: Result := 'STATUS_CANT_ACCESS_DOMAIN_INFO';
    STATUS_CANT_TERMINATE_SELF: Result := 'STATUS_CANT_TERMINATE_SELF';
    STATUS_INVALID_SERVER_STATE: Result := 'STATUS_INVALID_SERVER_STATE';
    STATUS_INVALID_DOMAIN_STATE: Result := 'STATUS_INVALID_DOMAIN_STATE';
    STATUS_INVALID_DOMAIN_ROLE: Result := 'STATUS_INVALID_DOMAIN_ROLE';
    STATUS_NO_SUCH_DOMAIN: Result := 'STATUS_NO_SUCH_DOMAIN';
    STATUS_DOMAIN_EXISTS: Result := 'STATUS_DOMAIN_EXISTS';
    STATUS_DOMAIN_LIMIT_EXCEEDED: Result := 'STATUS_DOMAIN_LIMIT_EXCEEDED';
    STATUS_OPLOCK_NOT_GRANTED: Result := 'STATUS_OPLOCK_NOT_GRANTED';
    STATUS_INVALID_OPLOCK_PROTOCOL: Result := 'STATUS_INVALID_OPLOCK_PROTOCOL';
    STATUS_INTERNAL_DB_CORRUPTION: Result := 'STATUS_INTERNAL_DB_CORRUPTION';
    STATUS_INTERNAL_ERROR: Result := 'STATUS_INTERNAL_ERROR';
    STATUS_GENERIC_NOT_MAPPED: Result := 'STATUS_GENERIC_NOT_MAPPED';
    STATUS_BAD_DESCRIPTOR_FORMAT: Result := 'STATUS_BAD_DESCRIPTOR_FORMAT';
    STATUS_INVALID_USER_BUFFER: Result := 'STATUS_INVALID_USER_BUFFER';
    STATUS_UNEXPECTED_IO_ERROR: Result := 'STATUS_UNEXPECTED_IO_ERROR';
    STATUS_UNEXPECTED_MM_CREATE_ERR: Result := 'STATUS_UNEXPECTED_MM_CREATE_ERR';
    STATUS_UNEXPECTED_MM_MAP_ERROR: Result := 'STATUS_UNEXPECTED_MM_MAP_ERROR';
    STATUS_UNEXPECTED_MM_EXTEND_ERR: Result := 'STATUS_UNEXPECTED_MM_EXTEND_ERR';
    STATUS_NOT_LOGON_PROCESS: Result := 'STATUS_NOT_LOGON_PROCESS';
    STATUS_LOGON_SESSION_EXISTS: Result := 'STATUS_LOGON_SESSION_EXISTS';
    STATUS_INVALID_PARAMETER_1: Result := 'STATUS_INVALID_PARAMETER_1';
    STATUS_INVALID_PARAMETER_2: Result := 'STATUS_INVALID_PARAMETER_2';
    STATUS_INVALID_PARAMETER_3: Result := 'STATUS_INVALID_PARAMETER_3';
    STATUS_INVALID_PARAMETER_4: Result := 'STATUS_INVALID_PARAMETER_4';
    STATUS_INVALID_PARAMETER_5: Result := 'STATUS_INVALID_PARAMETER_5';
    STATUS_INVALID_PARAMETER_6: Result := 'STATUS_INVALID_PARAMETER_6';
    STATUS_INVALID_PARAMETER_7: Result := 'STATUS_INVALID_PARAMETER_7';
    STATUS_INVALID_PARAMETER_8: Result := 'STATUS_INVALID_PARAMETER_8';
    STATUS_INVALID_PARAMETER_9: Result := 'STATUS_INVALID_PARAMETER_9';
    STATUS_INVALID_PARAMETER_10: Result := 'STATUS_INVALID_PARAMETER_10';
    STATUS_INVALID_PARAMETER_11: Result := 'STATUS_INVALID_PARAMETER_11';
    STATUS_INVALID_PARAMETER_12: Result := 'STATUS_INVALID_PARAMETER_12';
    STATUS_REDIRECTOR_NOT_STARTED: Result := 'STATUS_REDIRECTOR_NOT_STARTED';
    STATUS_REDIRECTOR_STARTED: Result := 'STATUS_REDIRECTOR_STARTED';
    STATUS_STACK_OVERFLOW: Result := 'STATUS_STACK_OVERFLOW';
    STATUS_NO_SUCH_PACKAGE: Result := 'STATUS_NO_SUCH_PACKAGE';
    STATUS_BAD_FUNCTION_TABLE: Result := 'STATUS_BAD_FUNCTION_TABLE';
    STATUS_VARIABLE_NOT_FOUND: Result := 'STATUS_VARIABLE_NOT_FOUND';
    STATUS_DIRECTORY_NOT_EMPTY: Result := 'STATUS_DIRECTORY_NOT_EMPTY';
    STATUS_FILE_CORRUPT_ERROR: Result := 'STATUS_FILE_CORRUPT_ERROR';
    STATUS_NOT_A_DIRECTORY: Result := 'STATUS_NOT_A_DIRECTORY';
    STATUS_BAD_LOGON_SESSION_STATE: Result := 'STATUS_BAD_LOGON_SESSION_STATE';
    STATUS_LOGON_SESSION_COLLISION: Result := 'STATUS_LOGON_SESSION_COLLISION';
    STATUS_NAME_TOO_LONG: Result := 'STATUS_NAME_TOO_LONG';
    STATUS_FILES_OPEN: Result := 'STATUS_FILES_OPEN';
    STATUS_CONNECTION_IN_USE: Result := 'STATUS_CONNECTION_IN_USE';
    STATUS_MESSAGE_NOT_FOUND: Result := 'STATUS_MESSAGE_NOT_FOUND';
    STATUS_PROCESS_IS_TERMINATING: Result := 'STATUS_PROCESS_IS_TERMINATING';
    STATUS_INVALID_LOGON_TYPE: Result := 'STATUS_INVALID_LOGON_TYPE';
    STATUS_NO_GUID_TRANSLATION: Result := 'STATUS_NO_GUID_TRANSLATION';
    STATUS_CANNOT_IMPERSONATE: Result := 'STATUS_CANNOT_IMPERSONATE';
    STATUS_IMAGE_ALREADY_LOADED: Result := 'STATUS_IMAGE_ALREADY_LOADED';
    STATUS_ABIOS_NOT_PRESENT: Result := 'STATUS_ABIOS_NOT_PRESENT';
    STATUS_ABIOS_LID_NOT_EXIST: Result := 'STATUS_ABIOS_LID_NOT_EXIST';
    STATUS_ABIOS_LID_ALREADY_OWNED: Result := 'STATUS_ABIOS_LID_ALREADY_OWNED';
    STATUS_ABIOS_NOT_LID_OWNER: Result := 'STATUS_ABIOS_NOT_LID_OWNER';
    STATUS_ABIOS_INVALID_COMMAND: Result := 'STATUS_ABIOS_INVALID_COMMAND';
    STATUS_ABIOS_INVALID_LID: Result := 'STATUS_ABIOS_INVALID_LID';
    STATUS_ABIOS_SELECTOR_NOT_AVAILABLE: Result := 'STATUS_ABIOS_SELECTOR_NOT_AVAILABLE';
    STATUS_ABIOS_INVALID_SELECTOR: Result := 'STATUS_ABIOS_INVALID_SELECTOR';
    STATUS_NO_LDT: Result := 'STATUS_NO_LDT';
    STATUS_INVALID_LDT_SIZE: Result := 'STATUS_INVALID_LDT_SIZE';
    STATUS_INVALID_LDT_OFFSET: Result := 'STATUS_INVALID_LDT_OFFSET';
    STATUS_INVALID_LDT_DESCRIPTOR: Result := 'STATUS_INVALID_LDT_DESCRIPTOR';
    STATUS_MAPPED_FILE_SIZE_ZERO: Result := 'STATUS_MAPPED_FILE_SIZE_ZERO';
    STATUS_TOO_MANY_OPENED_FILES: Result := 'STATUS_TOO_MANY_OPENED_FILES';
    STATUS_CANCELLED: Result := 'STATUS_CANCELLED';
    STATUS_CANNOT_DELETE: Result := 'STATUS_CANNOT_DELETE';
    STATUS_INVALID_COMPUTER_NAME: Result := 'STATUS_INVALID_COMPUTER_NAME';
    STATUS_FILE_DELETED: Result := 'STATUS_FILE_DELETED';
    STATUS_SPECIAL_ACCOUNT: Result := 'STATUS_SPECIAL_ACCOUNT';
    STATUS_SPECIAL_GROUP: Result := 'STATUS_SPECIAL_GROUP';
    STATUS_SPECIAL_USER: Result := 'STATUS_SPECIAL_USER';
    STATUS_MEMBERS_PRIMARY_GROUP: Result := 'STATUS_MEMBERS_PRIMARY_GROUP';
    STATUS_FILE_CLOSED: Result := 'STATUS_FILE_CLOSED';
    STATUS_TOO_MANY_THREADS: Result := 'STATUS_TOO_MANY_THREADS';
    STATUS_THREAD_NOT_IN_PROCESS: Result := 'STATUS_THREAD_NOT_IN_PROCESS';
    STATUS_TOKEN_ALREADY_IN_USE: Result := 'STATUS_TOKEN_ALREADY_IN_USE';
    STATUS_PAGEFILE_QUOTA_EXCEEDED: Result := 'STATUS_PAGEFILE_QUOTA_EXCEEDED';
    STATUS_COMMITMENT_LIMIT: Result := 'STATUS_COMMITMENT_LIMIT';
    STATUS_INVALID_IMAGE_PROTECT: Result := 'STATUS_INVALID_IMAGE_PROTECT';
    STATUS_LOGON_SERVER_CONFLICT: Result := 'STATUS_LOGON_SERVER_CONFLICT';
    STATUS_TIME_DIFFERENCE_AT_DC: Result := 'STATUS_TIME_DIFFERENCE_AT_DC';
    STATUS_SYNCHRONIZATION_REQUIRED: Result := 'STATUS_SYNCHRONIZATION_REQUIRED';
    STATUS_DLL_NOT_FOUND: Result := 'STATUS_DLL_NOT_FOUND';
    STATUS_OPEN_FAILED: Result := 'STATUS_OPEN_FAILED';
    STATUS_IO_PRIVILEGE_FAILED: Result := 'STATUS_IO_PRIVILEGE_FAILED';
    STATUS_ORDINAL_NOT_FOUND: Result := 'STATUS_ORDINAL_NOT_FOUND';
    STATUS_ENTRYPOINT_NOT_FOUND: Result := 'STATUS_ENTRYPOINT_NOT_FOUND';
    STATUS_CONTROL_C_EXIT: Result := 'STATUS_CONTROL_C_EXIT';
    STATUS_LOCAL_DISCONNECT: Result := 'STATUS_LOCAL_DISCONNECT';
    STATUS_REMOTE_DISCONNECT: Result := 'STATUS_REMOTE_DISCONNECT';
    STATUS_REMOTE_RESOURCES: Result := 'STATUS_REMOTE_RESOURCES';
    STATUS_LINK_FAILED: Result := 'STATUS_LINK_FAILED';
    STATUS_LINK_TIMEOUT: Result := 'STATUS_LINK_TIMEOUT';
    STATUS_INVALID_CONNECTION: Result := 'STATUS_INVALID_CONNECTION';
    STATUS_INVALID_ADDRESS: Result := 'STATUS_INVALID_ADDRESS';
    STATUS_DLL_INIT_FAILED: Result := 'STATUS_DLL_INIT_FAILED';
    STATUS_MISSING_SYSTEMFILE: Result := 'STATUS_MISSING_SYSTEMFILE';
    STATUS_UNHANDLED_EXCEPTION: Result := 'STATUS_UNHANDLED_EXCEPTION';
    STATUS_APP_INIT_FAILURE: Result := 'STATUS_APP_INIT_FAILURE';
    STATUS_PAGEFILE_CREATE_FAILED: Result := 'STATUS_PAGEFILE_CREATE_FAILED';
    STATUS_NO_PAGEFILE: Result := 'STATUS_NO_PAGEFILE';
    STATUS_INVALID_LEVEL: Result := 'STATUS_INVALID_LEVEL';
    STATUS_WRONG_PASSWORD_CORE: Result := 'STATUS_WRONG_PASSWORD_CORE';
    STATUS_ILLEGAL_FLOAT_CONTEXT: Result := 'STATUS_ILLEGAL_FLOAT_CONTEXT';
    STATUS_PIPE_BROKEN: Result := 'STATUS_PIPE_BROKEN';
    STATUS_REGISTRY_CORRUPT: Result := 'STATUS_REGISTRY_CORRUPT';
    STATUS_REGISTRY_IO_FAILED: Result := 'STATUS_REGISTRY_IO_FAILED';
    STATUS_NO_EVENT_PAIR: Result := 'STATUS_NO_EVENT_PAIR';
    STATUS_UNRECOGNIZED_VOLUME: Result := 'STATUS_UNRECOGNIZED_VOLUME';
    STATUS_SERIAL_NO_DEVICE_INITED: Result := 'STATUS_SERIAL_NO_DEVICE_INITED';
    STATUS_NO_SUCH_ALIAS: Result := 'STATUS_NO_SUCH_ALIAS';
    STATUS_MEMBER_NOT_IN_ALIAS: Result := 'STATUS_MEMBER_NOT_IN_ALIAS';
    STATUS_MEMBER_IN_ALIAS: Result := 'STATUS_MEMBER_IN_ALIAS';
    STATUS_ALIAS_EXISTS: Result := 'STATUS_ALIAS_EXISTS';
    STATUS_LOGON_NOT_GRANTED: Result := 'STATUS_LOGON_NOT_GRANTED';
    STATUS_TOO_MANY_SECRETS: Result := 'STATUS_TOO_MANY_SECRETS';
    STATUS_SECRET_TOO_LONG: Result := 'STATUS_SECRET_TOO_LONG';
    STATUS_INTERNAL_DB_ERROR: Result := 'STATUS_INTERNAL_DB_ERROR';
    STATUS_FULLSCREEN_MODE: Result := 'STATUS_FULLSCREEN_MODE';
    STATUS_TOO_MANY_CONTEXT_IDS: Result := 'STATUS_TOO_MANY_CONTEXT_IDS';
    STATUS_LOGON_TYPE_NOT_GRANTED: Result := 'STATUS_LOGON_TYPE_NOT_GRANTED';
    STATUS_NOT_REGISTRY_FILE: Result := 'STATUS_NOT_REGISTRY_FILE';
    STATUS_NT_CROSS_ENCRYPTION_REQUIRED: Result := 'STATUS_NT_CROSS_ENCRYPTION_REQUIRED';
    STATUS_DOMAIN_CTRLR_CONFIG_ERROR: Result := 'STATUS_DOMAIN_CTRLR_CONFIG_ERROR';
    STATUS_ILL_FORMED_SERVICE_ENTRY: Result := 'STATUS_ILL_FORMED_SERVICE_ENTRY';
    STATUS_ILLEGAL_CHARACTER: Result := 'STATUS_ILLEGAL_CHARACTER';
    STATUS_UNMAPPABLE_CHARACTER: Result := 'STATUS_UNMAPPABLE_CHARACTER';
    STATUS_UNDEFINED_CHARACTER: Result := 'STATUS_UNDEFINED_CHARACTER';
    STATUS_FLOPPY_VOLUME: Result := 'STATUS_FLOPPY_VOLUME';
    STATUS_FLOPPY_ID_MARK_NOT_FOUND: Result := 'STATUS_FLOPPY_ID_MARK_NOT_FOUND';
    STATUS_FLOPPY_WRONG_CYLINDER: Result := 'STATUS_FLOPPY_WRONG_CYLINDER';
    STATUS_FLOPPY_UNKNOWN_ERROR: Result := 'STATUS_FLOPPY_UNKNOWN_ERROR';
    STATUS_FLOPPY_BAD_REGISTERS: Result := 'STATUS_FLOPPY_BAD_REGISTERS';
    STATUS_DISK_RECALIBRATE_FAILED: Result := 'STATUS_DISK_RECALIBRATE_FAILED';
    STATUS_DISK_OPERATION_FAILED: Result := 'STATUS_DISK_OPERATION_FAILED';
    STATUS_DISK_RESET_FAILED: Result := 'STATUS_DISK_RESET_FAILED';
    STATUS_SHARED_IRQ_BUSY: Result := 'STATUS_SHARED_IRQ_BUSY';
    STATUS_BIOS_FAILED_TO_CONNECT_INTERRUPT: Result := 'STATUS_BIOS_FAILED_TO_CONNECT_INTERRUPT';
    STATUS_PARTITION_FAILURE: Result := 'STATUS_PARTITION_FAILURE';
    STATUS_INVALID_BLOCK_LENGTH: Result := 'STATUS_INVALID_BLOCK_LENGTH';
    STATUS_DEVICE_NOT_PARTITIONED: Result := 'STATUS_DEVICE_NOT_PARTITIONED';
    STATUS_UNABLE_TO_LOCK_MEDIA: Result := 'STATUS_UNABLE_TO_LOCK_MEDIA';
    STATUS_UNABLE_TO_UNLOAD_MEDIA: Result := 'STATUS_UNABLE_TO_UNLOAD_MEDIA';
    STATUS_EOM_OVERFLOW: Result := 'STATUS_EOM_OVERFLOW';
    STATUS_NO_MEDIA: Result := 'STATUS_NO_MEDIA';
    STATUS_NO_SUCH_MEMBER: Result := 'STATUS_NO_SUCH_MEMBER';
    STATUS_INVALID_MEMBER: Result := 'STATUS_INVALID_MEMBER';
    STATUS_KEY_DELETED: Result := 'STATUS_KEY_DELETED';
    STATUS_NO_LOG_SPACE: Result := 'STATUS_NO_LOG_SPACE';
    STATUS_TOO_MANY_SIDS: Result := 'STATUS_TOO_MANY_SIDS';
    STATUS_LM_CROSS_ENCRYPTION_REQUIRED: Result := 'STATUS_LM_CROSS_ENCRYPTION_REQUIRED';
    STATUS_KEY_HAS_CHILDREN: Result := 'STATUS_KEY_HAS_CHILDREN';
    STATUS_CHILD_MUST_BE_VOLATILE: Result := 'STATUS_CHILD_MUST_BE_VOLATILE';
    STATUS_DEVICE_CONFIGURATION_ERROR: Result := 'STATUS_DEVICE_CONFIGURATION_ERROR';
    STATUS_DRIVER_INTERNAL_ERROR: Result := 'STATUS_DRIVER_INTERNAL_ERROR';
    STATUS_INVALID_DEVICE_STATE: Result := 'STATUS_INVALID_DEVICE_STATE';
    STATUS_IO_DEVICE_ERROR: Result := 'STATUS_IO_DEVICE_ERROR';
    STATUS_DEVICE_PROTOCOL_ERROR: Result := 'STATUS_DEVICE_PROTOCOL_ERROR';
    STATUS_BACKUP_CONTROLLER: Result := 'STATUS_BACKUP_CONTROLLER';
    STATUS_LOG_FILE_FULL: Result := 'STATUS_LOG_FILE_FULL';
    STATUS_TOO_LATE: Result := 'STATUS_TOO_LATE';
    STATUS_NO_TRUST_LSA_SECRET: Result := 'STATUS_NO_TRUST_LSA_SECRET';
    STATUS_NO_TRUST_SAM_ACCOUNT: Result := 'STATUS_NO_TRUST_SAM_ACCOUNT';
    STATUS_TRUSTED_DOMAIN_FAILURE: Result := 'STATUS_TRUSTED_DOMAIN_FAILURE';
    STATUS_TRUSTED_RELATIONSHIP_FAILURE: Result := 'STATUS_TRUSTED_RELATIONSHIP_FAILURE';
    STATUS_EVENTLOG_FILE_CORRUPT: Result := 'STATUS_EVENTLOG_FILE_CORRUPT';
    STATUS_EVENTLOG_CANT_START: Result := 'STATUS_EVENTLOG_CANT_START';
    STATUS_TRUST_FAILURE: Result := 'STATUS_TRUST_FAILURE';
    STATUS_MUTANT_LIMIT_EXCEEDED: Result := 'STATUS_MUTANT_LIMIT_EXCEEDED';
    STATUS_NETLOGON_NOT_STARTED: Result := 'STATUS_NETLOGON_NOT_STARTED';
    STATUS_ACCOUNT_EXPIRED: Result := 'STATUS_ACCOUNT_EXPIRED';
    STATUS_POSSIBLE_DEADLOCK: Result := 'STATUS_POSSIBLE_DEADLOCK';
    STATUS_NETWORK_CREDENTIAL_CONFLICT: Result := 'STATUS_NETWORK_CREDENTIAL_CONFLICT';
    STATUS_REMOTE_SESSION_LIMIT: Result := 'STATUS_REMOTE_SESSION_LIMIT';
    STATUS_EVENTLOG_FILE_CHANGED: Result := 'STATUS_EVENTLOG_FILE_CHANGED';
    STATUS_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT: Result := 'STATUS_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT';
    STATUS_NOLOGON_WORKSTATION_TRUST_ACCOUNT: Result := 'STATUS_NOLOGON_WORKSTATION_TRUST_ACCOUNT';
    STATUS_NOLOGON_SERVER_TRUST_ACCOUNT: Result := 'STATUS_NOLOGON_SERVER_TRUST_ACCOUNT';
    STATUS_DOMAIN_TRUST_INCONSISTENT: Result := 'STATUS_DOMAIN_TRUST_INCONSISTENT';
    STATUS_FS_DRIVER_REQUIRED: Result := 'STATUS_FS_DRIVER_REQUIRED';
    STATUS_NO_USER_SESSION_KEY: Result := 'STATUS_NO_USER_SESSION_KEY';
    STATUS_USER_SESSION_DELETED: Result := 'STATUS_USER_SESSION_DELETED';
    STATUS_RESOURCE_LANG_NOT_FOUND: Result := 'STATUS_RESOURCE_LANG_NOT_FOUND';
    STATUS_INSUFF_SERVER_RESOURCES: Result := 'STATUS_INSUFF_SERVER_RESOURCES';
    STATUS_INVALID_BUFFER_SIZE: Result := 'STATUS_INVALID_BUFFER_SIZE';
    STATUS_INVALID_ADDRESS_COMPONENT: Result := 'STATUS_INVALID_ADDRESS_COMPONENT';
    STATUS_INVALID_ADDRESS_WILDCARD: Result := 'STATUS_INVALID_ADDRESS_WILDCARD';
    STATUS_TOO_MANY_ADDRESSES: Result := 'STATUS_TOO_MANY_ADDRESSES';
    STATUS_ADDRESS_ALREADY_EXISTS: Result := 'STATUS_ADDRESS_ALREADY_EXISTS';
    STATUS_ADDRESS_CLOSED: Result := 'STATUS_ADDRESS_CLOSED';
    STATUS_CONNECTION_DISCONNECTED: Result := 'STATUS_CONNECTION_DISCONNECTED';
    STATUS_CONNECTION_RESET: Result := 'STATUS_CONNECTION_RESET';
    STATUS_TOO_MANY_NODES: Result := 'STATUS_TOO_MANY_NODES';
    STATUS_TRANSACTION_ABORTED: Result := 'STATUS_TRANSACTION_ABORTED';
    STATUS_TRANSACTION_TIMED_OUT: Result := 'STATUS_TRANSACTION_TIMED_OUT';
    STATUS_TRANSACTION_NO_RELEASE: Result := 'STATUS_TRANSACTION_NO_RELEASE';
    STATUS_TRANSACTION_NO_MATCH: Result := 'STATUS_TRANSACTION_NO_MATCH';
    STATUS_TRANSACTION_RESPONDED: Result := 'STATUS_TRANSACTION_RESPONDED';
    STATUS_TRANSACTION_INVALID_ID: Result := 'STATUS_TRANSACTION_INVALID_ID';
    STATUS_TRANSACTION_INVALID_TYPE: Result := 'STATUS_TRANSACTION_INVALID_TYPE';
    STATUS_NOT_SERVER_SESSION: Result := 'STATUS_NOT_SERVER_SESSION';
    STATUS_NOT_CLIENT_SESSION: Result := 'STATUS_NOT_CLIENT_SESSION';
    STATUS_CANNOT_LOAD_REGISTRY_FILE: Result := 'STATUS_CANNOT_LOAD_REGISTRY_FILE';
    STATUS_DEBUG_ATTACH_FAILED: Result := 'STATUS_DEBUG_ATTACH_FAILED';
    STATUS_SYSTEM_PROCESS_TERMINATED: Result := 'STATUS_SYSTEM_PROCESS_TERMINATED';
    STATUS_DATA_NOT_ACCEPTED: Result := 'STATUS_DATA_NOT_ACCEPTED';
    STATUS_NO_BROWSER_SERVERS_FOUND: Result := 'STATUS_NO_BROWSER_SERVERS_FOUND';
    STATUS_VDM_HARD_ERROR: Result := 'STATUS_VDM_HARD_ERROR';
    STATUS_DRIVER_CANCEL_TIMEOUT: Result := 'STATUS_DRIVER_CANCEL_TIMEOUT';
    STATUS_REPLY_MESSAGE_MISMATCH: Result := 'STATUS_REPLY_MESSAGE_MISMATCH';
    STATUS_MAPPED_ALIGNMENT: Result := 'STATUS_MAPPED_ALIGNMENT';
    STATUS_IMAGE_CHECKSUM_MISMATCH: Result := 'STATUS_IMAGE_CHECKSUM_MISMATCH';
    STATUS_LOST_WRITEBEHIND_DATA: Result := 'STATUS_LOST_WRITEBEHIND_DATA';
    STATUS_CLIENT_SERVER_PARAMETERS_INVALID: Result := 'STATUS_CLIENT_SERVER_PARAMETERS_INVALID';
    STATUS_PASSWORD_MUST_CHANGE: Result := 'STATUS_PASSWORD_MUST_CHANGE';
    STATUS_NOT_FOUND: Result := 'STATUS_NOT_FOUND';
    STATUS_NOT_TINY_STREAM: Result := 'STATUS_NOT_TINY_STREAM';
    STATUS_RECOVERY_FAILURE: Result := 'STATUS_RECOVERY_FAILURE';
    STATUS_STACK_OVERFLOW_READ: Result := 'STATUS_STACK_OVERFLOW_READ';
    STATUS_FAIL_CHECK: Result := 'STATUS_FAIL_CHECK';
    STATUS_DUPLICATE_OBJECTID: Result := 'STATUS_DUPLICATE_OBJECTID';
    STATUS_OBJECTID_EXISTS: Result := 'STATUS_OBJECTID_EXISTS';
    STATUS_CONVERT_TO_LARGE: Result := 'STATUS_CONVERT_TO_LARGE';
    STATUS_RETRY: Result := 'STATUS_RETRY';
    STATUS_FOUND_OUT_OF_SCOPE: Result := 'STATUS_FOUND_OUT_OF_SCOPE';
    STATUS_ALLOCATE_BUCKET: Result := 'STATUS_ALLOCATE_BUCKET';
    STATUS_PROPSET_NOT_FOUND: Result := 'STATUS_PROPSET_NOT_FOUND';
    STATUS_MARSHALL_OVERFLOW: Result := 'STATUS_MARSHALL_OVERFLOW';
    STATUS_INVALID_VARIANT: Result := 'STATUS_INVALID_VARIANT';
    STATUS_DOMAIN_CONTROLLER_NOT_FOUND: Result := 'STATUS_DOMAIN_CONTROLLER_NOT_FOUND';
    STATUS_ACCOUNT_LOCKED_OUT: Result := 'STATUS_ACCOUNT_LOCKED_OUT';
    STATUS_HANDLE_NOT_CLOSABLE: Result := 'STATUS_HANDLE_NOT_CLOSABLE';
    STATUS_CONNECTION_REFUSED: Result := 'STATUS_CONNECTION_REFUSED';
    STATUS_GRACEFUL_DISCONNECT: Result := 'STATUS_GRACEFUL_DISCONNECT';
    STATUS_ADDRESS_ALREADY_ASSOCIATED: Result := 'STATUS_ADDRESS_ALREADY_ASSOCIATED';
    STATUS_ADDRESS_NOT_ASSOCIATED: Result := 'STATUS_ADDRESS_NOT_ASSOCIATED';
    STATUS_CONNECTION_INVALID: Result := 'STATUS_CONNECTION_INVALID';
    STATUS_CONNECTION_ACTIVE: Result := 'STATUS_CONNECTION_ACTIVE';
    STATUS_NETWORK_UNREACHABLE: Result := 'STATUS_NETWORK_UNREACHABLE';
    STATUS_HOST_UNREACHABLE: Result := 'STATUS_HOST_UNREACHABLE';
    STATUS_PROTOCOL_UNREACHABLE: Result := 'STATUS_PROTOCOL_UNREACHABLE';
    STATUS_PORT_UNREACHABLE: Result := 'STATUS_PORT_UNREACHABLE';
    STATUS_REQUEST_ABORTED: Result := 'STATUS_REQUEST_ABORTED';
    STATUS_CONNECTION_ABORTED: Result := 'STATUS_CONNECTION_ABORTED';
    STATUS_BAD_COMPRESSION_BUFFER: Result := 'STATUS_BAD_COMPRESSION_BUFFER';
    STATUS_USER_MAPPED_FILE: Result := 'STATUS_USER_MAPPED_FILE';
    STATUS_AUDIT_FAILED: Result := 'STATUS_AUDIT_FAILED';
    STATUS_TIMER_RESOLUTION_NOT_SET: Result := 'STATUS_TIMER_RESOLUTION_NOT_SET';
    STATUS_CONNECTION_COUNT_LIMIT: Result := 'STATUS_CONNECTION_COUNT_LIMIT';
    STATUS_LOGIN_TIME_RESTRICTION: Result := 'STATUS_LOGIN_TIME_RESTRICTION';
    STATUS_LOGIN_WKSTA_RESTRICTION: Result := 'STATUS_LOGIN_WKSTA_RESTRICTION';
    STATUS_IMAGE_MP_UP_MISMATCH: Result := 'STATUS_IMAGE_MP_UP_MISMATCH';
    STATUS_INSUFFICIENT_LOGON_INFO: Result := 'STATUS_INSUFFICIENT_LOGON_INFO';
    STATUS_BAD_DLL_ENTRYPOINT: Result := 'STATUS_BAD_DLL_ENTRYPOINT';
    STATUS_BAD_SERVICE_ENTRYPOINT: Result := 'STATUS_BAD_SERVICE_ENTRYPOINT';
    STATUS_LPC_REPLY_LOST: Result := 'STATUS_LPC_REPLY_LOST';
    STATUS_IP_ADDRESS_CONFLICT1: Result := 'STATUS_IP_ADDRESS_CONFLICT1';
    STATUS_IP_ADDRESS_CONFLICT2: Result := 'STATUS_IP_ADDRESS_CONFLICT2';
    STATUS_REGISTRY_QUOTA_LIMIT: Result := 'STATUS_REGISTRY_QUOTA_LIMIT';
    STATUS_PATH_NOT_COVERED: Result := 'STATUS_PATH_NOT_COVERED';
    STATUS_NO_CALLBACK_ACTIVE: Result := 'STATUS_NO_CALLBACK_ACTIVE';
    STATUS_LICENSE_QUOTA_EXCEEDED: Result := 'STATUS_LICENSE_QUOTA_EXCEEDED';
    STATUS_PWD_TOO_SHORT: Result := 'STATUS_PWD_TOO_SHORT';
    STATUS_PWD_TOO_RECENT: Result := 'STATUS_PWD_TOO_RECENT';
    STATUS_PWD_HISTORY_CONFLICT: Result := 'STATUS_PWD_HISTORY_CONFLICT';
    STATUS_PLUGPLAY_NO_DEVICE: Result := 'STATUS_PLUGPLAY_NO_DEVICE';
    STATUS_UNSUPPORTED_COMPRESSION: Result := 'STATUS_UNSUPPORTED_COMPRESSION';
    STATUS_INVALID_HW_PROFILE: Result := 'STATUS_INVALID_HW_PROFILE';
    STATUS_INVALID_PLUGPLAY_DEVICE_PATH: Result := 'STATUS_INVALID_PLUGPLAY_DEVICE_PATH';
    STATUS_DRIVER_ORDINAL_NOT_FOUND: Result := 'STATUS_DRIVER_ORDINAL_NOT_FOUND';
    STATUS_DRIVER_ENTRYPOINT_NOT_FOUND: Result := 'STATUS_DRIVER_ENTRYPOINT_NOT_FOUND';
    STATUS_RESOURCE_NOT_OWNED: Result := 'STATUS_RESOURCE_NOT_OWNED';
    STATUS_TOO_MANY_LINKS: Result := 'STATUS_TOO_MANY_LINKS';
    STATUS_QUOTA_LIST_INCONSISTENT: Result := 'STATUS_QUOTA_LIST_INCONSISTENT';
    STATUS_FILE_IS_OFFLINE: Result := 'STATUS_FILE_IS_OFFLINE';
    STATUS_EVALUATION_EXPIRATION: Result := 'STATUS_EVALUATION_EXPIRATION';
    STATUS_ILLEGAL_DLL_RELOCATION: Result := 'STATUS_ILLEGAL_DLL_RELOCATION';
    STATUS_LICENSE_VIOLATION: Result := 'STATUS_LICENSE_VIOLATION';
    STATUS_DLL_INIT_FAILED_LOGOFF: Result := 'STATUS_DLL_INIT_FAILED_LOGOFF';
    STATUS_DRIVER_UNABLE_TO_LOAD: Result := 'STATUS_DRIVER_UNABLE_TO_LOAD';
    STATUS_DFS_UNAVAILABLE: Result := 'STATUS_DFS_UNAVAILABLE';
    STATUS_VOLUME_DISMOUNTED: Result := 'STATUS_VOLUME_DISMOUNTED';
    STATUS_WX86_INTERNAL_ERROR: Result := 'STATUS_WX86_INTERNAL_ERROR';
    STATUS_WX86_FLOAT_STACK_CHECK: Result := 'STATUS_WX86_FLOAT_STACK_CHECK';
    STATUS_VALIDATE_CONTINUE: Result := 'STATUS_VALIDATE_CONTINUE';
    STATUS_NO_MATCH: Result := 'STATUS_NO_MATCH';
    STATUS_NO_MORE_MATCHES: Result := 'STATUS_NO_MORE_MATCHES';
    STATUS_NOT_A_REPARSE_POINT: Result := 'STATUS_NOT_A_REPARSE_POINT';
    STATUS_IO_REPARSE_TAG_INVALID: Result := 'STATUS_IO_REPARSE_TAG_INVALID';
    STATUS_IO_REPARSE_TAG_MISMATCH: Result := 'STATUS_IO_REPARSE_TAG_MISMATCH';
    STATUS_IO_REPARSE_DATA_INVALID: Result := 'STATUS_IO_REPARSE_DATA_INVALID';
    STATUS_IO_REPARSE_TAG_NOT_HANDLED: Result := 'STATUS_IO_REPARSE_TAG_NOT_HANDLED';
    STATUS_REPARSE_POINT_NOT_RESOLVED: Result := 'STATUS_REPARSE_POINT_NOT_RESOLVED';
    STATUS_DIRECTORY_IS_A_REPARSE_POINT: Result := 'STATUS_DIRECTORY_IS_A_REPARSE_POINT';
    STATUS_RANGE_LIST_CONFLICT: Result := 'STATUS_RANGE_LIST_CONFLICT';
    STATUS_SOURCE_ELEMENT_EMPTY: Result := 'STATUS_SOURCE_ELEMENT_EMPTY';
    STATUS_DESTINATION_ELEMENT_FULL: Result := 'STATUS_DESTINATION_ELEMENT_FULL';
    STATUS_ILLEGAL_ELEMENT_ADDRESS: Result := 'STATUS_ILLEGAL_ELEMENT_ADDRESS';
    STATUS_MAGAZINE_NOT_PRESENT: Result := 'STATUS_MAGAZINE_NOT_PRESENT';
    STATUS_REINITIALIZATION_NEEDED: Result := 'STATUS_REINITIALIZATION_NEEDED';
    STATUS_DEVICE_REQUIRES_CLEANING: Result := 'STATUS_DEVICE_REQUIRES_CLEANING';
    STATUS_DEVICE_DOOR_OPEN: Result := 'STATUS_DEVICE_DOOR_OPEN';
    STATUS_ENCRYPTION_FAILED: Result := 'STATUS_ENCRYPTION_FAILED';
    STATUS_DECRYPTION_FAILED: Result := 'STATUS_DECRYPTION_FAILED';
    STATUS_RANGE_NOT_FOUND: Result := 'STATUS_RANGE_NOT_FOUND';
    STATUS_NO_RECOVERY_POLICY: Result := 'STATUS_NO_RECOVERY_POLICY';
    STATUS_NO_EFS: Result := 'STATUS_NO_EFS';
    STATUS_WRONG_EFS: Result := 'STATUS_WRONG_EFS';
    STATUS_NO_USER_KEYS: Result := 'STATUS_NO_USER_KEYS';
    STATUS_FILE_NOT_ENCRYPTED: Result := 'STATUS_FILE_NOT_ENCRYPTED';
    STATUS_NOT_EXPORT_FORMAT: Result := 'STATUS_NOT_EXPORT_FORMAT';
    STATUS_FILE_ENCRYPTED: Result := 'STATUS_FILE_ENCRYPTED';
    STATUS_WAKE_SYSTEM: Result := 'STATUS_WAKE_SYSTEM';
    STATUS_SHARED_POLICY: Result := 'STATUS_SHARED_POLICY';
    STATUS_POLICY_OBJECT_NOT_FOUND: Result := 'STATUS_POLICY_OBJECT_NOT_FOUND';
    STATUS_POLICY_ONLY_IN_DS: Result := 'STATUS_POLICY_ONLY_IN_DS';
    STATUS_VOLUME_NOT_UPGRADED: Result := 'STATUS_VOLUME_NOT_UPGRADED';
    STATUS_REPARSE_ATTRIBUTE_CONFLICT: Result := 'STATUS_REPARSE_ATTRIBUTE_CONFLICT';
    STATUS_CANT_ENABLE_DENY_ONLY: Result := 'STATUS_CANT_ENABLE_DENY_ONLY';
    STATUS_FLOAT_MULTIPLE_FAULTS: Result := 'STATUS_FLOAT_MULTIPLE_FAULTS';
    STATUS_FLOAT_MULTIPLE_TRAPS: Result := 'STATUS_FLOAT_MULTIPLE_TRAPS';
    STATUS_DEVICE_REMOVED: Result := 'STATUS_DEVICE_REMOVED';
    STATUS_NOINTERFACE: Result := 'STATUS_NOINTERFACE';
    STATUS_DRIVER_FAILED_SLEEP: Result := 'STATUS_DRIVER_FAILED_SLEEP';
    STATUS_MUTUAL_AUTHENTICATION_FAILED: Result := 'STATUS_MUTUAL_AUTHENTICATION_FAILED';
    STATUS_CORRUPT_SYSTEM_FILE: Result := 'STATUS_CORRUPT_SYSTEM_FILE';
    STATUS_DATATYPE_MISALIGNMENT_ERROR: Result := 'STATUS_DATATYPE_MISALIGNMENT_ERROR';
    STATUS_COMMITMENT_MINIMUM: Result := 'STATUS_COMMITMENT_MINIMUM';
    STATUS_REG_NAT_CONSUMPTION: Result := 'STATUS_REG_NAT_CONSUMPTION';
    STATUS_TRANSPORT_FULL: Result := 'STATUS_TRANSPORT_FULL';
    STATUS_ONLY_IF_CONNECTED: Result := 'STATUS_ONLY_IF_CONNECTED';
    STATUS_PNP_RESTART_ENUMERATION: Result := 'STATUS_PNP_RESTART_ENUMERATION';
    STATUS_SYSTEM_IMAGE_BAD_SIGNATURE: Result := 'STATUS_SYSTEM_IMAGE_BAD_SIGNATURE';
    STATUS_PNP_REBOOT_REQUIRED: Result := 'STATUS_PNP_REBOOT_REQUIRED';
    STATUS_POWER_STATE_INVALID: Result := 'STATUS_POWER_STATE_INVALID';
    STATUS_INSUFFICIENT_POWER: Result := 'STATUS_INSUFFICIENT_POWER';
    STATUS_SAM_NEED_BOOTKEY_PASSWORD: Result := 'STATUS_SAM_NEED_BOOTKEY_PASSWORD';
    STATUS_SAM_NEED_BOOTKEY_FLOPPY: Result := 'STATUS_SAM_NEED_BOOTKEY_FLOPPY';
    STATUS_MULTIPLE_FAULT_VIOLATION: Result := 'STATUS_MULTIPLE_FAULT_VIOLATION';
//    STATUS_CANNOT_MAKE: Result := 'STATUS_CANNOT_MAKE'; // Missing from JwaNtStatus
    STATUS_NOT_SUPPORTED_ON_SBS: Result := 'STATUS_NOT_SUPPORTED_ON_SBS';
    STATUS_WOW_ASSERTION: Result := 'STATUS_WOW_ASSERTION';
//    STATUS_IMAGE_GAME_REGION_VIOLATION: Result := 'STATUS_IMAGE_GAME_REGION_VIOLATION'; // XBOX
//    STATUS_IMAGE_MEDIA_TYPE_VIOLATION: Result := 'STATUS_IMAGE_MEDIA_TYPE_VIOLATION'; // XBOX
  else
    // TODO -oDxbx: Check if we ever encounter unhandled values, and what those might mean.
    Result := Format('0x%.08X', [aStatus]);
    Exit;
  end;

  // Note : Enable the following line to prepend the string with the NtStatus code (will result in longer log lines) :
  //  Result := Format('0x%.08X ', [aStatus]) + Result;
end;

function FileInformationClassToString(FileInformationClass: FILE_INFORMATION_CLASS): string;
begin
  case FileInformationClass of
    FileDirectoryInformation: Result := 'FileDirectoryInformation';
    FileFullDirectoryInformation: Result := 'FileFullDirectoryInformation';
    FileBothDirectoryInformation: Result := 'FileBothDirectoryInformation';
    FileBasicInformation: Result := 'FileBasicInformation';
    FileStandardInformation: Result := 'FileStandardInformation';
    FileInternalInformation: Result := 'FileInternalInformation';
    FileEaInformation: Result := 'FileEaInformation';
    FileAccessInformation: Result := 'FileAccessInformation';
    FileNameInformation: Result := 'FileNameInformation';
    FileRenameInformation: Result := 'FileRenameInformation';
    FileLinkInformation: Result := 'FileLinkInformation';
    FileNamesInformation: Result := 'FileNamesInformation';
    FileDispositionInformation: Result := 'FileDispositionInformation';
    FilePositionInformation: Result := 'FilePositionInformation';
    FileFullEaInformation: Result := 'FileFullEaInformation';
    FileModeInformation: Result := 'FileModeInformation';
    FileAlignmentInformation: Result := 'FileAlignmentInformation';
    FileAllInformation: Result := 'FileAllInformation';
    FileAllocationInformation: Result := 'FileAllocationInformation';
    FileEndOfFileInformation: Result := 'FileEndOfFileInformation';
    FileAlternateNameInformation: Result := 'FileAlternateNameInformation';
    FileStreamInformation: Result := 'FileStreamInformation';
    FilePipeInformation: Result := 'FilePipeInformation';
    FilePipeLocalInformation: Result := 'FilePipeLocalInformation';
    FilePipeRemoteInformation: Result := 'FilePipeRemoteInformation';
    FileMailslotQueryInformation: Result := 'FileMailslotQueryInformation';
    FileMailslotSetInformation: Result := 'FileMailslotSetInformation';
    FileCompressionInformation: Result := 'FileCompressionInformation';
    FileObjectIdInformation: Result := 'FileObjectIdInformation';
    FileCompletionInformation: Result := 'FileCompletionInformation';
    FileMoveClusterInformation: Result := 'FileMoveClusterInformation';
    FileQuotaInformation: Result := 'FileQuotaInformation';
    FileReparsePointInformation: Result := 'FileReparsePointInformation';
    FileNetworkOpenInformation: Result := 'FileNetworkOpenInformation';
    FileAttributeTagInformation: Result := 'FileAttributeTagInformation';
    FileTrackingInformation: Result := 'FileTrackingInformation';
    FileMaximumInformation: Result := 'FileMaximumInformation';
  else
    Result := '';
  end;
end;

function FsInformationClassToString(FsInformationClass: FS_INFORMATION_CLASS): string;
begin
  case FsInformationClass of
    FileFsVolumeInformation: Result := 'FileFsVolumeInformation';
    FileFsLabelInformation: Result := 'FileFsLabelInformation';
    FileFsSizeInformation: Result := 'FileFsSizeInformation';
    FileFsDeviceInformation: Result := 'FileFsDeviceInformation';
    FileFsAttributeInformation: Result := 'FileFsAttributeInformation';
    FileFsControlInformation: Result := 'FileFsControlInformation';
    FileFsFullSizeInformation: Result := 'FileFsFullSizeInformation';
    FileFsObjectIdInformation: Result := 'FileFsObjectIdInformation';
    FileFsMaximumInformation: Result := 'FileFsMaximumInformation';
  else
    Result := '';
  end;
end;

function AllocationTypeToString(AllocationType: DWORD): string;
begin
  Result := '';
  AppendFlagStr({var}Result, AllocationType, PAGE_NOACCESS, 'PAGE_NOACCESS');
  AppendFlagStr({var}Result, AllocationType, PAGE_READONLY, 'PAGE_READONLY');
  AppendFlagStr({var}Result, AllocationType, PAGE_READWRITE, 'PAGE_READWRITE');
  AppendFlagStr({var}Result, AllocationType, PAGE_WRITECOPY, 'PAGE_WRITECOPY');
  AppendFlagStr({var}Result, AllocationType, PAGE_EXECUTE, 'PAGE_EXECUTE');
  AppendFlagStr({var}Result, AllocationType, PAGE_EXECUTE_READ, 'PAGE_EXECUTE_READ');
  AppendFlagStr({var}Result, AllocationType, PAGE_EXECUTE_READWRITE, 'PAGE_EXECUTE_READWRITE');
  AppendFlagStr({var}Result, AllocationType, PAGE_EXECUTE_WRITECOPY, 'PAGE_EXECUTE_WRITECOPY');
  AppendFlagStr({var}Result, AllocationType, PAGE_GUARD, 'PAGE_GUARD');
  AppendFlagStr({var}Result, AllocationType, PAGE_NOCACHE, 'PAGE_NOCACHE');
  AppendFlagStr({var}Result, AllocationType, PAGE_WRITECOMBINE, 'PAGE_WRITECOMBINE');
  AppendFlagStr({var}Result, AllocationType, MEM_COMMIT, 'MEM_COMMIT');
  AppendFlagStr({var}Result, AllocationType, MEM_RESERVE, 'MEM_RESERVE');
  AppendFlagStr({var}Result, AllocationType, MEM_DECOMMIT, 'MEM_DECOMMIT');
  AppendFlagStr({var}Result, AllocationType, MEM_RELEASE, 'MEM_RELEASE');
  AppendFlagStr({var}Result, AllocationType, MEM_FREE, 'MEM_FREE');
  AppendFlagStr({var}Result, AllocationType, MEM_PRIVATE, 'MEM_PRIVATE');
  AppendFlagStr({var}Result, AllocationType, MEM_MAPPED, 'MEM_MAPPED');
  AppendFlagStr({var}Result, AllocationType, MEM_RESET, 'MEM_RESET');
  AppendFlagStr({var}Result, AllocationType, MEM_TOP_DOWN, 'MEM_TOP_DOWN');
  AppendFlagStr({var}Result, AllocationType, MEM_WRITE_WATCH, 'MEM_WRITE_WATCH');
  AppendFlagStr({var}Result, AllocationType, MEM_PHYSICAL, 'MEM_PHYSICAL');
  AppendFlagStr({var}Result, AllocationType, SEC_FILE, 'SEC_FILE');
  AppendFlagStr({var}Result, AllocationType, SEC_IMAGE, 'SEC_IMAGE');
  AppendFlagStr({var}Result, AllocationType, SEC_RESERVE, 'SEC_RESERVE');
  AppendFlagStr({var}Result, AllocationType, SEC_COMMIT, 'SEC_COMMIT');
  AppendFlagStr({var}Result, AllocationType, SEC_NOCACHE, 'SEC_NOCACHE');
  AppendFlagStr({var}Result, AllocationType, MEM_LARGE_PAGES, 'MEM_LARGE_PAGES');
  AppendFlagStr({var}Result, AllocationType, MEM_4MB_PAGES, 'MEM_4MB_PAGES');
//  MEM_IMAGE              = SEC_IMAGE;
//  WRITE_WATCH_FLAG_RESET = $01;
end;

end.
