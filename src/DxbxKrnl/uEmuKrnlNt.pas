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
unit uEmuKrnlNt;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  JwaWinBase,
  JwaWinNT,
  JwaNative,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl, // reintroduces PIO_STATUS_BLOCK !
  // Dxbx
  uTypes,
  uDxbxUtils,
  uLog,
  uEmu,
  uEmuFS,
  uEmuFile,
  uEmuXapi,
  uEmuKrnl,
  uEmuAlloc,
  uDxbxKrnl,
  uDxbxKrnlUtils;

function xboxkrnl_NtAllocateVirtualMemory(
  BaseAddress: PPVOID; // OUT * ?
  ZeroBits: ULONG;
  AllocationSize: PULONG; // OUT * ?
  AllocationType: DWORD;
  Protect: DWORD
  ): NTSTATUS; stdcall;
function xboxkrnl_NtCancelTimer(
  hTimerHandle: HANDLE;
  pbPreviousState: PBOOLEAN
  ): NTSTATUS; stdcall;
function xboxkrnl_NtClearEvent(
  EventHandle: HANDLE
  ): NTSTATUS; stdcall;
function xboxkrnl_NtClose(
  Handle: Handle
  ): NTSTATUS; stdcall; {EXPORTNUM(187)}
function xboxkrnl_NtCreateDirectoryObject(
  DirectoryHandle: PHANDLE; // OUT
  ObjectAttributes: POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
function xboxkrnl_NtCreateEvent(
  EventHandle: PHANDLE; // OUT
  ObjectAttributes: POBJECT_ATTRIBUTES; // OPTIONAL
  EventType: EVENT_TYPE;
  InitialState: LONGBOOL
  ): NTSTATUS; stdcall;
function xboxkrnl_NtCreateFile(
  FileHandle: PHANDLE; // OUT
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  AllocationSize: PLARGE_INTEGER; // OPTIONAL,
  FileAttributes: ULONG;
  ShareAccess: ACCESS_MASK;
  CreateDisposition: ULONG; // dtCreateDisposition;
  CreateOptions: ULONG // dtCreateOptions
  ): NTSTATUS; stdcall;
function xboxkrnl_NtCreateIoCompletion(
  FileHandle: dtU32;
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  pszUnknownArgs: dtBLOB
  ): NTSTATUS; stdcall;
function xboxkrnl_NtCreateMutant(
  MutantHandle: PHANDLE; // OUT
  ObjectAttributes: POBJECT_ATTRIBUTES;
  InitialOwner: Boolean
  ): NTSTATUS; stdcall;
function xboxkrnl_NtCreateSemaphore(
  SemaphoreHandle: PHANDLE;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  InitialCount: ULONG;
  MaximumCount: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_NtCreateTimer(
  pTimerHandle: PHANDLE;
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  TimerType: TIMER_TYPE
  ): NTSTATUS; stdcall;
function xboxkrnl_NtDeleteFile(
  ObjectAttributes: POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
function xboxkrnl_NtDeviceIoControlFile(
  FileHandle: dtU32;
  Event: dtU32;
  pApcRoutine: dtU32;
  pApcContext: dtU32;
  pIoStatusBlock: dtU32;
  pIoControlCode: dtU32;
  pInputBuffer: dtU32;
  InputBufferLength: dtU32;
  pOutputBuffer: dtU32;
  OutputBufferLength: dtU32
  ): NTSTATUS; stdcall;
function xboxkrnl_NtDuplicateObject(
  SourceHandle: HANDLE;
  TargetHandle: PHANDLE;
  Options: DWORD
  ): NTSTATUS; stdcall;
function xboxkrnl_NtFlushBuffersFile(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtFreeVirtualMemory(
  BaseAddress: PPVOID; // OUT
  FreeSize: PULONG; // OUT
  FreeType: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_NtFsControlFile(
  FileHandle: dtU32;
  Event: dtU32;
  pApcRoutine: dtU32;
  pApcContext: dtU32;
  pIoStatusBlock: dtU32;
  FsControlCode: dtU32;
  pInputBuffer: dtU32;
  InputBufferLength: dtU32;
  pOutputBuffer: dtU32;
  OutputBufferLength: dtU32
  ): NTSTATUS; stdcall;
function xboxkrnl_NtOpenDirectoryObject(
  DirectoryHandle: PHANDLE; // OUT
  ObjectAttributes: POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
function xboxkrnl_NtOpenFile(
  FileHandle: PHANDLE; // OUT
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  ShareAccess: ACCESS_MASK;
  OpenOptions: ULONG // dtCreateOptions
  ): NTSTATUS; stdcall;
function xboxkrnl_NtOpenSymbolicLinkObject(
  pFileHandle: dtU32;
  ObjectAttributes: POBJECT_ATTRIBUTES
  ): NTSTATUS; stdcall;
function xboxkrnl_NtProtectVirtualMemory(
  BaseAddress: PPVOID; // OUT
  RegionSize: PSIZE_T; // OUT
  NewProtect: ULONG;
  OldProtect: PULONG // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtPulseEvent(
  EventHandle: HANDLE;
  PreviousState: PLONG // OUT OPTIONAL
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueueApcThread(
  ThreadHandle: HANDLE;
  ApcRoutine: PIO_APC_ROUTINE;
  ApcRoutineContext: PVOID;
  ApcStatusBlock: PIO_STATUS_BLOCK;
  ApcReserved: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryDirectoryFile(
  FileHandle: HANDLE;
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PIO_APC_ROUTINE;
  ApcContext: PVOID;
  IoStatusBlock: PIO_STATUS_BLOCK; // out
  FileInformation: PFILE_DIRECTORY_INFORMATION; // out
  Length: ULONG;
  FileInformationClass: FILE_INFORMATION_CLASS;
  FileMask: PSTRING;
  RestartScan: _BOOLEAN
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryDirectoryObject(
  DirectoryHandle: HANDLE;
  Buffer: PVOID; // OUT
  Length: ULONG;
  RestartScan: _BOOLEAN;
  Context: PULONG; // OUT
  ReturnLength: PULONG  // OUT OPTIONAL
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryEvent(
  EventHandle: HANDLE;
  EventInformation: PEVENT_BASIC_INFORMATION // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryFullAttributesFile(
  ObjectAttributes: POBJECT_ATTRIBUTES;
  FileInformation: PFILE_NETWORK_OPEN_INFORMATION // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryInformationFile(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK; //   OUT
  FileInformation: PVOID; //   OUT
  Length: ULONG;
  FileInfo: FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryIoCompletion(
  IoCompletionHandle: HANDLE;
  IoCompletionInformation: PIO_COMPLETION_BASIC_INFORMATION // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryMutant(
  MutantHandle: HANDLE;
  MutantInformation: PMUTANT_BASIC_INFORMATION // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQuerySemaphore(
  SemaphoreHandle: HANDLE;
  SemaphoreInformation: PSEMAPHORE_BASIC_INFORMATION // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQuerySymbolicLinkObject(
  LinkHandle: HANDLE;
  LinkTarget: POBJECT_STRING; // OUT
  ReturnedLength: PULONG // OUT  OPTIONAL
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryTimer(
  hTimerHandle: HANDLE;
  TimerInformationClass: TIMER_INFORMATION_CLASS;
  pTimerInformation: PVOID;
  TimerInformationLength: ULONG;
  ResultLength: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryVirtualMemory(
  BaseAddress: PVOID;
  Buffer: PMEMORY_BASIC_INFORMATION
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryVolumeInformationFile(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  FileInformation: PFILE_FS_SIZE_INFORMATION; // OUT
  Length: ULONG;
  FileInformationClass: FS_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
function xboxkrnl_NtReadFile(
  FileHandle: HANDLE; // TODO -oCXBX: correct paramters
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PIO_APC_ROUTINE; // OPTIONAL
  ApcContext: PVOID;
  IoStatusBlock: PVOID; // OUT
  Buffer: PVOID; // OUT
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER // OPTIONAL
  ): NTSTATUS; stdcall;
function xboxkrnl_NtReadFileScatter(
  FileHandle: HANDLE;
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PIO_APC_ROUTINE; // OPTIONAL
  ApcContext: PVOID; // OPTIONAL
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  SegmentArray: PFILE_SEGMENT_ELEMENT;
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER // OPTIONAL
  ): NTSTATUS; stdcall;
function xboxkrnl_NtReleaseMutant(
  MutantHandle: HANDLE;
  PreviousCount: PULONG // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtReleaseSemaphore(
  SemaphoreHandle: HANDLE;
  ReleaseCount: ULONG;
  PreviousCount: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_NtRemoveIoCompletion(
  IoCompletionHandle: HANDLE;
  KeyContext: PPVOID; // OUT
  ApcContext: PPVOID; // OUT
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_NtResumeThread(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSetEvent(
  EventHandle: HANDLE;
  PreviousState: PLONG // OUT // Dxbx Note : Shouldn't this be PULONg instead?
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSetInformationFile(
  FileHandle: HANDLE; // TODO -oCXBX: correct paramters
  IoStatusBlock: PVOID; // OUT
  FileInformation: PVOID;
  Length: ULONG;
  FileInformationClass: FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSetIoCompletion(
  IoCompletionHandle: HANDLE;
  KeyContext: PVOID;
  ApcContext: PVOID;
  IoStatus: NTSTATUS;
  IoStatusInformation: ULONG_PTR
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSetSystemTime(
  {const}NewTime: PLARGE_INTEGER;
  OldTime: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSetTimerEx(
  TimerHandle: HANDLE;
  DueTime: PLARGE_INTEGER;
  TimerApcRoutine: PTIMER_APC_ROUTINE; {OPTIONAL}
  ApcMode: KPROCESSOR_MODE;
  TimerContext: PVOID;  {OPTIONAL}
  ResumeTimer: _BOOLEAN;
  Period: LONG; {OPTIONAL}
  {OUT} PreviousState: PBOOLEAN {OPTIONAL}
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSignalAndWaitForSingleObjectEx(
  SignalHandle: HANDLE;
  WaitHandle: HANDLE;
  WaitMode: KPROCESSOR_MODE;
  Alertable: BOOLEAN;
  Timeout: PLARGE_INTEGER // OPTIONAL
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSuspendThread(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG // OUT OPTIONAL
  ): NTSTATUS; stdcall;
procedure xboxkrnl_NtUserIoApcDispatcher(
  ApcContext: PVOID;
  IoStatusBlock: PIO_STATUS_BLOCK;
  Reserved: ULONG
  ); stdcall;
function xboxkrnl_NtWaitForSingleObject(
  Handle: HANDLE;
  Alertable: _BOOLEAN;
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_NtWaitForSingleObjectEx(
  Handle_: HANDLE;
  WaitMode: _CHAR;
  Alertable: LONGBOOL;
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_NtWaitForMultipleObjectsEx(
  Count: ULONG;
  Handles: PHANDLE;
  WaitType: WAIT_TYPE;
  WaitMode: _CHAR;
  Alertable: LONGBOOL;
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_NtWriteFile(
  FileHandle: HANDLE; // TODO -oCXBX: correct paramters
  Event: DWORD; // Dxbx correction (was PVOID)
  ApcRoutine: PIO_APC_ROUTINE;
  ApcContext: PVOID;
  IoStatusBlock: PVOID; // OUT
  Buffer: PVOID;
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_NtWriteFileGather(
  FileHandle: HANDLE;
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PIO_APC_ROUTINE; // OPTIONAL
  ApcContext: PVOID; // OPTIONAL
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  SegmentArray: PFILE_SEGMENT_ELEMENT;
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER // OPTIONAL
  ): NTSTATUS; stdcall;
procedure xboxkrnl_NtYieldExecution(); stdcall;

implementation

const lfUnit = lfCxbx or lfKernel;

type
  RNativeObjectAttributes = record
    // Internal variables :
    wszObjectName: UnicodeString;
    NtUnicodeString: UNICODE_STRING;
    NtObjAttr: JwaWinType.OBJECT_ATTRIBUTES;
    // This is what should be passed on to Windows
    // after ObjectAttributesToNT() has been called :
    NtObjAttrPtr: JwaWinType.POBJECT_ATTRIBUTES;
  end;

function ObjectAttributesToNT(ObjectAttributes: POBJECT_ATTRIBUTES; const aFileAPIName: string = ''): RNativeObjectAttributes;
var
  szBuffer: AnsiString;
begin
  if ObjectAttributes = nil then
  begin
    // When the pointer is nil, make sure we pass nil to Windows too :
    Result.NtObjAttrPtr := nil;
    Exit;
  end;

  // ObjectAttributes are given, so make sure the pointer we're going to pass to Windows is assigned :
  Result.NtObjAttrPtr := @(Result.NtObjAttr);

  szBuffer := POBJECT_ATTRIBUTES_String(ObjectAttributes);

{$IFDEF DEBUG}
  //printf('Orig : %s', szBuffer); // MARKED OUT BY CXBX
{$ENDIF}

  // Always trim '\??\' off :
  if  (Length(szBuffer) >= 4)
  and (szBuffer[1] = '\')
  and (szBuffer[2] = '?')
  and (szBuffer[3] = '?')
  and (szBuffer[4] = '\') then
    System.Delete(szBuffer, 1, 4);

  // Check if we where called from a File-handling API :
  if aFileAPIName <> '' then
  begin
    // Check if the path starts with a volume indicator :
    if (Length(szBuffer) >= 2) and (szBuffer[2] = ':') then
    begin
      System.Delete(szBuffer, 2, 1); // Remove ':'

      case szBuffer[1] of // Check the volume letter, and set the RootDirectory accordingly
        't', 'T': ObjectAttributes.RootDirectory := g_hTDrive;
        'u', 'U': ObjectAttributes.RootDirectory := g_hUDrive;
        'z', 'Z': ObjectAttributes.RootDirectory := g_hZDrive;
      else
//        'd', 'D', // D:\ should map to current directory
//        'y', 'Y', // Going to map Y:\ to current directory as well (dashboard test, 3944)
//        'q', 'Q': // Dxbx addition : Some homebrews map Q: to current Xbe dir too
        ObjectAttributes.RootDirectory := g_hCurDir;
        // TODO -oDxbx : We should probably use the settings made by IoCreateSymbolicLink here,
        // once that function is implemented.
      end;

      if MayLog(lfUnit or lfFile) then
      begin
        DbgPrintf('EmuKrnl : %s Corrected path...', [aFileAPIName]);
        DbgPrintf('  Org:"%s"', [POBJECT_ATTRIBUTES_String(ObjectAttributes)]);
      end;
      if ObjectAttributes.RootDirectory = g_hCurDir then
      begin
        System.Delete(szBuffer, 1, 2);
        if MayLog(lfUnit or lfFile) then
          DbgPrintf('  New:"$XbePath\%s"', [szBuffer])
      end
      else
      begin
        if MayLog(lfUnit or lfFile) then
          DbgPrintf('  New:"$DxbxPath\EmuDisk\%s"', [szBuffer]);
        System.Delete(szBuffer, 1, 2);
      end;
    end;
  end
  else
    // For non-file API calls, prefix with '\??\' again :
    szBuffer := '\??\' + szBuffer;

  // Convert Ansi to Unicode :
  Result.wszObjectName := UnicodeString(szBuffer);
  JwaNative.RtlInitUnicodeString(@Result.NtUnicodeString, PWideChar(Result.wszObjectName));

  // Initialize the NT ObjectAttributes :
  JwaWinType.InitializeObjectAttributes(@Result.NtObjAttr, @Result.NtUnicodeString, ObjectAttributes.Attributes, ObjectAttributes.RootDirectory, NULL)
end;


// NtAllocateVirtualMemory:
// Allocates virtual memory.
//
// Differences from NT: There is no ProcessHandle parameter.
function xboxkrnl_NtAllocateVirtualMemory
(
  BaseAddress: PPVOID; // OUT * ?
  ZeroBits: ULONG;
  AllocationSize: PULONG; // OUT * ?
  AllocationType: DWORD;
  Protect: DWORD
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtAllocateVirtualMemory'+
      #13#10'('+
      #13#10'   BaseAddress         : 0x%.8x (0x%.8x)'+
      #13#10'   ZeroBits            : 0x%.8x' +
      #13#10'   AllocationSize      : 0x%.8x (0x%.8x)'+
      #13#10'   AllocationType      : 0x%.8x' +
      #13#10'   Protect             : 0x%.8x' +
      #13#10');',
      [BaseAddress, BaseAddress^, ZeroBits, AllocationSize, AllocationSize^, AllocationType, Protect]);

  Result := JwaNative.NtAllocateVirtualMemory(GetCurrentProcess(), BaseAddress, ZeroBits, AllocationSize, AllocationType, Protect);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCancelTimer(
  hTimerHandle: HANDLE;
  pbPreviousState: PBOOLEAN
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfDxbx or lfKernel) then
    DbgPrintf('EmuKrnl : NtCancelTimer' +
      #13#10'(' +
      #13#10'   pTimerHandle        : 0x%.08X' +
      #13#10'   pbPreviousState     : 0x%.08X' +
      #13#10');',
      [hTimerHandle, pbPreviousState]);

  Result := JwaNative.NtCancelTimer(hTimerHandle, pbPreviousState);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtClearEvent
(
  EventHandle: HANDLE
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtClearEvent' +
      #13#10'('+
      #13#10'   EventHandle         : 0x%.8x' +
      #13#10');',
      [EventHandle]);

  Result := JwaNative.NtClearEvent(EventHandle);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtClearEvent failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

// NtClose:
// Closes a file or other handle.
//
// Differences from NT: None.
function xboxkrnl_NtClose
(
    Handle: HANDLE
): NTSTATUS; stdcall; {XBSYSAPI EXPORTNUM(187)}
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
{$IFDEF DXBX_EMUHANDLES}
var
  iEmuHandle: TEmuHandle;
{$ENDIF}
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtClose' +
      #13#10'(' +
      #13#10'   Handle              : 0x%.8x' +
      #13#10');', [Handle]);

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
    Result := JwaNative.NtClose(Handle);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateDirectoryObject(
  DirectoryHandle: PHANDLE; // OUT
  ObjectAttributes: POBJECT_ATTRIBUTES
): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  NativeObjectAttributes: RNativeObjectAttributes;
  DesiredAccess: ACCESS_MASK;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfDxbx or lfKernel or lfFile) then
    DbgPrintf('EmuKrnl : NtCreateDirectoryObject' +
        #13#10'(' +
        #13#10'   DirectoryHandle     : 0x%.08X' +
        #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
        #13#10');',
        [DirectoryHandle, ObjectAttributes, POBJECT_ATTRIBUTES_String(ObjectAttributes)]);

  // initialize object attributes
  NativeObjectAttributes := ObjectAttributesToNT(ObjectAttributes, 'NtCreateDirectoryObject');

  // TODO -oDxbx : Is this the correct ACCESS_MASK? :
  DesiredAccess := DIRECTORY_CREATE_OBJECT;

  // redirect to Win2k/XP
  Result := JwaNative.NtCreateDirectoryObject(DirectoryHandle, DesiredAccess, NativeObjectAttributes.NtObjAttrPtr);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtCreateDirectoryObject failed! (%s)', [NTStatusToString(Result)])
  else
    if MayLog(lfUnit or lfFile) then
      DbgPrintf('EmuKrnl : NtCreateDirectoryObject = 0x%.08X', [DirectoryHandle^]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateEvent
(
  EventHandle: PHANDLE; // OUT
  ObjectAttributes: POBJECT_ATTRIBUTES; // OPTIONAL
  EventType: EVENT_TYPE;
  InitialState: LONGBOOL
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  NativeObjectAttributes: RNativeObjectAttributes;
  DesiredAccess: ACCESS_MASK;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtCreateEvent' +
      #13#10'(' +
      #13#10'   EventHandle         : 0x%.08X' +
      #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
      #13#10'   EventType           : 0x%.08X' +
      #13#10'   InitialState        : 0x%.08X' +
      #13#10');',
      [EventHandle, ObjectAttributes, POBJECT_ATTRIBUTES_String(ObjectAttributes),
       Ord(EventType), Ord(InitialState)]);

  // initialize object attributes
  NativeObjectAttributes := ObjectAttributesToNT(ObjectAttributes);
//  NativeObjectAttributes.NtObjAttr.RootDirectory := 0;

  // TODO -oDxbx : Is this the correct ACCESS_MASK? :
  DesiredAccess := EVENT_ALL_ACCESS;

  // redirect to Win2k/XP
  Result := JwaNative.NtCreateEvent(EventHandle, DesiredAccess, NativeObjectAttributes.NtObjAttrPtr, EVENT_TYPE(EventType), InitialState);

  // From http://msdn.microsoft.com/en-us/library/ff566423(v=VS.85).aspx
  // "returns STATUS_SUCCESS" :
  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtCreateEvent failed! (%s)', [NTStatusToString(Result)])
  else
    if MayLog(lfUnit) then
      DbgPrintf('EmuKrnl : NtCreateEvent EventHandle = 0x%.08X', [EventHandle^]);

  EmuSwapFS(fsXbox);
end;

// NtCreateFile:
// Creates or opens a file or device object.
//
// Differences from NT: The EaBuffer and EaLength options are gone.
//     OBJECT_ATTRIBUTES uses ANSI_STRING, so only ANSI filenames work.
function xboxkrnl_NtCreateFile
(
  FileHandle: PHANDLE; // OUT
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  AllocationSize: PLARGE_INTEGER; // OPTIONAL,
  FileAttributes: ULONG;
  ShareAccess: ACCESS_MASK;
  CreateDisposition: ULONG; // dtCreateDisposition;
  CreateOptions: ULONG // dtCreateOptions
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  NativeObjectAttributes: RNativeObjectAttributes;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfFile) then
    DbgPrintf('EmuKrnl : NtCreateFile' +
     #13#10'(' +
     #13#10'   FileHandle          : 0x%.08X' +
     #13#10'   DesiredAccess       : 0x%.08X (%s)' +
     #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
     #13#10'   IoStatusBlock       : 0x%.08X' +
     #13#10'   AllocationSize      : 0x%.08X (%d)' +
     #13#10'   FileAttributes      : 0x%.08X' +
     #13#10'   ShareAccess         : 0x%.08X' +
     #13#10'   CreateDisposition   : 0x%.08X (%s)' +
     #13#10'   CreateOptions       : 0x%.08X (%s)' +
     #13#10');',
     [FileHandle,
      DesiredAccess, AccessMaskToString(DesiredAccess),
      ObjectAttributes, POBJECT_ATTRIBUTES_String(ObjectAttributes),
     IoStatusBlock, AllocationSize, QuadPart(AllocationSize), FileAttributes, ShareAccess,
     CreateDisposition, CreateDispositionToString(CreateDisposition),
     CreateOptions, CreateOptionsToString(CreateOptions)]);

  // initialize object attributes
  NativeObjectAttributes := ObjectAttributesToNT(ObjectAttributes, 'NtCreateFile');

  // Dxbx note : The messy business in Cxbx for supporting the calls from Xapi FindFirstFile
  // to NtOpenFile and NtQueryDirectoryFile, which use the same buffer (albeit with a different
  // Length) is fixed since we started to honour the Length field (see PSTRING_String).

  // redirect to Win2k/XP
  Result := JwaNative.NtCreateFile(
      FileHandle, DesiredAccess, NativeObjectAttributes.NtObjAttrPtr, JwaNative.PIO_STATUS_BLOCK(IoStatusBlock),
      JwaWinType.PLARGE_INTEGER(AllocationSize), FileAttributes, ShareAccess, CreateDisposition, CreateOptions, NULL, 0
  );

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtCreateFile failed! (%s)', [NTStatusToString(Result)])
  else
    if MayLog(lfUnit or lfFile) then
      DbgPrintf('EmuKrnl : NtCreateFile = 0x%.08X', [FileHandle^]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateIoCompletion(
  FileHandle: dtU32;
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  pszUnknownArgs: dtBLOB
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtCreateIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateMutant
(
  {OUT}MutantHandle: PHANDLE;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  InitialOwner: Boolean
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  NativeObjectAttributes: RNativeObjectAttributes;
  DesiredAccess: ACCESS_MASK;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtCreateMutant' +
      #13#10'(' +
      #13#10'   MutantHandle        : 0x%.08X' +
      #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
      #13#10'   InitialOwner        : 0x%.08X' +
      #13#10');',
      [MutantHandle, ObjectAttributes, POBJECT_ATTRIBUTES_String(ObjectAttributes), InitialOwner]);

  // initialize object attributes
  NativeObjectAttributes := ObjectAttributesToNT(ObjectAttributes);
//  NativeObjectAttributes.NtObjAttr.RootDirectory := 0;

  // TODO -oDxbx : Is this the correct ACCESS_MASK? :
  DesiredAccess := MUTANT_ALL_ACCESS;

  // redirect to Win2k/XP
  Result := JwaNative.NtCreateMutant(MutantHandle, DesiredAccess, NativeObjectAttributes.NtObjAttrPtr, InitialOwner);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtCreateMutant failed! (%s)', [NTStatusToString(Result)])
  else
    if MayLog(lfUnit) then
      DbgPrintf('EmuKrnl : NtCreateMutant MutantHandle = 0x%.08X', [MutantHandle]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateSemaphore
(
  SemaphoreHandle: PHANDLE;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  InitialCount: ULONG;
  MaximumCount: ULONG
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  NativeObjectAttributes: RNativeObjectAttributes;
  DesiredAccess: ACCESS_MASK;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtCreateSemaphore' +
      #13#10'(' +
      #13#10'   SemaphoreHandle     : 0x%.08X' +
      #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
      #13#10'   InitialCount        : 0x%.08X' +
      #13#10'   MaximumCount        : 0x%.08X' +
      #13#10');',
      [SemaphoreHandle, ObjectAttributes, POBJECT_ATTRIBUTES_String(ObjectAttributes),
      InitialCount, MaximumCount]);

  // Dxbx addition : Fix NT Unicode <> Xbox ANSI difference on ObjectName :
  NativeObjectAttributes := ObjectAttributesToNT(ObjectAttributes);
//  NativeObjectAttributes.NtObjAttr.RootDirectory := 0;

  // TODO -oDxbx : Is this the correct ACCESS_MASK? :
  DesiredAccess := SEMAPHORE_ALL_ACCESS;

  // redirect to Win2k/XP
  Result := JwaNative.NtCreateSemaphore
  (
      SemaphoreHandle,
      DesiredAccess,
      NativeObjectAttributes.NtObjAttrPtr,
      InitialCount,
      MaximumCount
  );

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtCreateSemaphore failed! (%s)', [NTStatusToString(Result)])
  else
    if MayLog(lfUnit) then
      DbgPrintf('EmuKrnl : NtCreateSemaphore SemaphoreHandle = 0x%.08X', [SemaphoreHandle^]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateTimer(
  pTimerHandle: PHANDLE;
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  TimerType: TIMER_TYPE
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  NativeObjectAttributes: RNativeObjectAttributes;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfDxbx or lfKernel) then
    DbgPrintf('EmuKrnl : NtCreateTimer' +
      #13#10'(' +
      #13#10'   pTimerHandle        : 0x%.08X' +
      #13#10'   DesiredAccess       : 0x%.08X' +
      #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
      #13#10'   TimerType           : 0x%.08X' +
      #13#10');',
      [pTimerHandle, DesiredAccess, ObjectAttributes, POBJECT_ATTRIBUTES_String(ObjectAttributes), Ord(TimerType)]);

  // Dxbx addition : Fix NT Unicode <> Xbox ANSI difference on ObjectName :
  NativeObjectAttributes := ObjectAttributesToNT(ObjectAttributes);
//  NativeObjectAttributes.NtObjAttr.RootDirectory := 0;

  Result := JwaNative.NtCreateTimer(pTimerHandle, DesiredAccess, NativeObjectAttributes.NtObjAttrPtr, TimerType);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtDeleteFile(ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  NativeObjectAttributes: RNativeObjectAttributes;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfDxbx or lfKernel or lfFile) then
   DbgPrintf('EmuKrnl : NtDeleteFile' +
    #13#10'(' +
    #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
    #13#10');',
    [ObjectAttributes, POBJECT_ATTRIBUTES_String(ObjectAttributes)]);

  // initialize object attributes
  NativeObjectAttributes := ObjectAttributesToNT(ObjectAttributes, 'NtDeleteFile');

  // Never delete from XbePath :
  if (NativeObjectAttributes.NtObjAttr.RootDirectory = g_hCurDir)
  // Nor when no filename was given (better not to trust the handle, or random files might get deleted!) :
  or (NativeObjectAttributes.wszObjectName = '') then
    Result := STATUS_OBJECT_PATH_NOT_FOUND
  else
    // redirect to Win2k/XP
    Result := JwaNative.NtDeleteFile(NativeObjectAttributes.NtObjAttrPtr);

  if (Result <> STATUS_SUCCESS) then
    if MayLog(lfDxbx or lfKernel or lfFile) then
      EmuWarning('NtDeleteFile failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

// NtDeviceIoControl:
// Does an IOCTL on a device.
//
// Differences from NT: None known.
function xboxkrnl_NtDeviceIoControlFile(
  FileHandle: dtU32;
  Event: dtU32;
  pApcRoutine: dtU32;
  pApcContext: dtU32;
  pIoStatusBlock: dtU32;
  pIoControlCode: dtU32;
  pInputBuffer: dtU32;
  InputBufferLength: dtU32;
  pOutputBuffer: dtU32;
  OutputBufferLength: dtU32
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtDeviceIoControlFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtDuplicateObject
(
  SourceHandle: HANDLE;
  TargetHandle: PHANDLE;
  Options: DWORD
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  DesiredAccess: ACCESS_MASK;
  Attributes: ULONG;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtDuplicateObject'+
      #13#10'('+
      #13#10'   SourceHandle        : 0x%.08X'+
      #13#10'   TargetHandle        : 0x%.08X'+
      #13#10'   Options             : 0x%.08X'+
      #13#10');',
      [SourceHandle, TargetHandle, Options]);
{$ENDIF}

  DesiredAccess := 0; // TODO -oDxbx : Should be set is Options <> DUPLICATE_SAME_ACCESS
  Attributes := 0; // TODO -oDxbx : Should be set is Options <> DUPLICATE_SAME_ATTRIBUTES

  // redirect to Win2k/XP
  Result := JwaNative.NtDuplicateObject
  (
      GetCurrentProcess(),
      SourceHandle,
      GetCurrentProcess(),
      TargetHandle,
      DesiredAccess,
      Attributes,
      Options
  );

  // From http://msdn.microsoft.com/en-us/library/ms724251(VS.85).aspx
  // "If the function fails, the return value is zero" :
  if Result = 0 then
    EmuWarning('Object was not duplicated!');

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtFlushBuffersFile
(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK // OUT
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtFlushBuffersFile'+
      #13#10'('+
      #13#10'   FileHandle          : 0x%.08X'+
      #13#10'   IoStatusBlock       : 0x%.08X'+
      #13#10');',
      [FileHandle, IoStatusBlock]);
{$ENDIF}

  Result := JwaNative.NtFlushBuffersFile(FileHandle, JwaNative.PIO_STATUS_BLOCK(IoStatusBlock));

  EmuSwapFS(fsXbox);
end;

// NtFreeVirtualMemory:
// Frees virtual memory.
//
// Differences from NT: There is no ProcessHandle parameter.
function xboxkrnl_NtFreeVirtualMemory
(
  BaseAddress: PPVOID; // OUT
  FreeSize: PULONG; // OUT
  FreeType: ULONG
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtFreeVirtualMemory'+
      #13#10'('+
      #13#10'   BaseAddress         : 0x%.08X'+
      #13#10'   FreeSize            : 0x%.08X'+
      #13#10'   FreeType            : 0x%.08X'+
      #13#10');',
      [BaseAddress, FreeSize, FreeType]);
{$ENDIF}

  Result := JwaNative.NtFreeVirtualMemory(GetCurrentProcess(), BaseAddress, FreeSize, FreeType);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtFsControlFile(
  FileHandle: dtU32;
  Event: dtU32;
  pApcRoutine: dtU32;
  pApcContext: dtU32;
  pIoStatusBlock: dtU32;
  FsControlCode: dtU32;
  pInputBuffer: dtU32;
  InputBufferLength: dtU32;
  pOutputBuffer: dtU32;
  OutputBufferLength: dtU32
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtFsControlFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtOpenDirectoryObject(
  DirectoryHandle: PHANDLE; // OUT
  ObjectAttributes: POBJECT_ATTRIBUTES
): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  NativeObjectAttributes: RNativeObjectAttributes;
  DesiredAccess: ACCESS_MASK;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfDxbx or lfKernel or lfFile) then
    DbgPrintf('EmuKrnl : NtOpenDirectoryObject' +
        #13#10'(' +
        #13#10'   DirectoryHandle     : 0x%.08X' +
        #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
        #13#10');',
        [DirectoryHandle, ObjectAttributes, POBJECT_ATTRIBUTES_String(ObjectAttributes)]);

  // initialize object attributes
  NativeObjectAttributes := ObjectAttributesToNT(ObjectAttributes, 'NtOpenDirectoryObject');

  // TODO -oDxbx : Is this the correct ACCESS_MASK? :
  DesiredAccess := DIRECTORY_TRAVERSE;

  // redirect to Win2k/XP
  Result := JwaNative.NtOpenDirectoryObject(DirectoryHandle, DesiredAccess, NativeObjectAttributes.NtObjAttrPtr);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtOpenDirectoryObject failed! (%s)', [NTStatusToString(Result)])
  else
    if MayLog(lfUnit or lfFile) then
      DbgPrintf('EmuKrnl : NtOpenDirectoryObject = 0x%.08X', [DirectoryHandle^]);

  EmuSwapFS(fsXbox);
end;

// NtOpenFile:
// Opens a file or device object.  Same as calling:
//   NtCreateFile(FileHandle, DesiredAccess, ObjectAttributes,
//     IoStatusBlock, NULL, 0, ShareAccess, OPEN_EXISTING, OpenOptions);
//
// Differences from NT: See NtCreateFile.
function xboxkrnl_NtOpenFile
(
  FileHandle: PHANDLE; // OUT
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  ShareAccess: ACCESS_MASK;
  OpenOptions: ULONG // dtCreateOptions
): NTSTATUS; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
var
  NativeObjectAttributes: RNativeObjectAttributes;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtOpenFile' +
    #13#10'(' +
    #13#10'   FileHandle          : 0x%.08X' +
    #13#10'   DesiredAccess       : 0x%.08X (%s)' +
    #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
    #13#10'   IoStatusBlock       : 0x%.08X' +
    #13#10'   ShareAccess         : 0x%.08X' +
    #13#10'   OpenOptions         : 0x%.08X (%s)' +
    #13#10');',
    [FileHandle,
     DesiredAccess, AccessMaskToString(DesiredAccess),
     ObjectAttributes, POBJECT_ATTRIBUTES_String(ObjectAttributes),
     IoStatusBlock, ShareAccess, OpenOptions, CreateOptionsToString(OpenOptions)]);
{$ENDIF}

  // initialize object attributes
  NativeObjectAttributes := ObjectAttributesToNT(ObjectAttributes, 'NtOpenFile');

  // redirect to Win2k/XP
  Result := JwaNative.NtOpenFile(
      FileHandle, DesiredAccess, NativeObjectAttributes.NtObjAttrPtr, JwaNative.PIO_STATUS_BLOCK(IoStatusBlock),
      ShareAccess, OpenOptions
  );

{$IFDEF DEBUG}
  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtOpenFile failed! (%s)', [NTStatusToString(Result)])
  else
    DbgPrintf('EmuKrnl : NtOpenFile = 0x%.08X', [FileHandle^]);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtOpenSymbolicLinkObject(pFileHandle: dtU32; ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtOpenSymbolicLinkObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtProtectVirtualMemory(
  BaseAddress: PPVOID; // OUT
  RegionSize: PSIZE_T; // OUT
  NewProtect: ULONG;
  OldProtect: PULONG // OUT
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtProtectVirtualMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtPulseEvent(
  EventHandle: HANDLE;
  PreviousState: PLONG // OUT OPTIONAL
): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtPulseEvent' +
      #13#10'('+
      #13#10'   EventHandle         : 0x%.8x' +
      #13#10'   PreviousState       : 0x%.8x' +
      #13#10');',
      [EventHandle, PreviousState]);

  Result := JwaNative.NtPulseEvent(EventHandle, PULONG(PreviousState));

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtPulseEvent failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueueApcThread
(
  ThreadHandle: HANDLE;
  ApcRoutine: PIO_APC_ROUTINE;
  ApcRoutineContext: PVOID;
  ApcStatusBlock: PIO_STATUS_BLOCK;
  ApcReserved: ULONG
): NTSTATUS; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtQueueApcThread' +
      #13#10'(' +
      #13#10'   ThreadHandle         : 0x%.08X' +
      #13#10'   ApcRoutine           : 0x%.08X' +
      #13#10'   ApcRoutineContext    : 0x%.08X' +
      #13#10'   ApcStatusBlock       : 0x%.08X' +
      #13#10'   ApcReserved          : 0x%.08X' +
      #13#10');',
      [ThreadHandle, Addr(ApcRoutine), ApcRoutineContext,
       ApcStatusBlock, ApcReserved]);
{$ENDIF}

  // TODO -oCXBX: Not too sure how this one works.  If there's any special *magic* that needs to be
  //     done, let me know!
  Result := JwaNative.NtQueueApcThread(
    ThreadHandle,
    PKNORMAL_ROUTINE(ApcRoutine),
    ApcRoutineContext,
    PIO_STATUS_BLOCK(ApcStatusBlock),
    Pointer(ApcReserved));

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtQueueApcThread failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryDirectoryFile
(
  FileHandle: HANDLE;
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PIO_APC_ROUTINE;
  ApcContext: PVOID;
  IoStatusBlock: PIO_STATUS_BLOCK; // out
  FileInformation: PFILE_DIRECTORY_INFORMATION; // out
  Length: ULONG;
  FileInformationClass: FILE_INFORMATION_CLASS;
  FileMask: PSTRING;
  RestartScan: _BOOLEAN
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  szBuffer: AnsiString;
  wszObjectName: UnicodeString;
  NtFileMask: UNICODE_STRING;
  FileDirInfo: PFILE_DIRECTORY_INFORMATION;
  mbstr: P_char;
  wcstr: pwchar_t;
begin
  EmuSwapFS(fsWindows);

  szBuffer := PSTRING_String(FileMask);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtQueryDirectoryFile' +
      #13#10'(' +
      #13#10'   FileHandle           : 0x%.08X' +
      #13#10'   Event                : 0x%.08X' +
      #13#10'   ApcRoutine           : 0x%.08X' +
      #13#10'   ApcContext           : 0x%.08X' +
      #13#10'   IoStatusBlock        : 0x%.08X' +
      #13#10'   FileInformation      : 0x%.08X' +
      #13#10'   Length               : 0x%.08X' +
      #13#10'   FileInformationClass : 0x%.08X' +
      #13#10'   FileMask             : 0x%.08X ("%s")' +
      #13#10'   RestartScan          : 0x%.08X' +
      #13#10');',
      [FileHandle, Event, Addr(ApcRoutine), ApcContext, IoStatusBlock,
       FileInformation, Length, Ord(FileInformationClass), FileMask,
       szBuffer, RestartScan]);
{$ENDIF}

  if (FileInformationClass <> FileDirectoryInformation) then   // Due to unicode->string conversion
    DxbxKrnlCleanup('Unsupported FileInformationClass');

  // initialize FileMask
  begin
    wszObjectName := UnicodeString(szBuffer);

    JwaNative.RtlInitUnicodeString(@NtFileMask, PWideChar(wszObjectName));
  end;

  FileDirInfo := PFILE_DIRECTORY_INFORMATION(DxbxMalloc($40 + 160*2));

  mbstr := @FileInformation.FileName[0]; // DXBX note : This is Ansi on XBox!
  wcstr := @FileDirInfo.FileName[0];

  repeat
    ZeroMemory(wcstr, 160*2);

    Result := JwaNative.NtQueryDirectoryFile
        (
            FileHandle, Event, ApcRoutine, ApcContext, JwaNative.PIO_STATUS_BLOCK(IoStatusBlock), FileDirInfo,
            $40+160*2, FILE_INFORMATION_CLASS(FileInformationClass), TRUE, @NtFileMask, RestartScan
        );

    // convert from PC to Xbox
    begin
      memcpy(FileInformation, FileDirInfo, $40);

      wcstombs(mbstr, wcstr, 160);

      FileInformation.FileNameLength := FileInformation.FileNameLength div 2;
    end;

    RestartScan := FALSE;

    // Xbox does not return . and ..
  until not ((strcmp(mbstr, '.') = 0) or (strcmp(mbstr, '..') = 0));

  // TODO -oCXBX: Cache the last search result for quicker access with CreateFile (xbox does this internally!)
  DxbxFree(FileDirInfo);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryDirectoryObject(
  DirectoryHandle: HANDLE;
  Buffer: PVOID; // OUT
  Length: ULONG;
  RestartScan: _BOOLEAN;
  Context: PULONG; // OUT
  ReturnLength: PULONG  // OUT OPTIONAL
): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfDxbx or lfKernel or lfFile) then
    DbgPrintf('EmuKrnl : NtQueryDirectoryObject' +
        #13#10'(' +
        #13#10'   DirectoryHandle     : 0x%.08X' +
        #13#10'   Buffer              : 0x%.08X' +
        #13#10'   Length              : 0x%.08X' +
        #13#10'   RestartScan         : 0x%.08X' +
        #13#10'   Context             : 0x%.08X' +
        #13#10'   ReturnLength        : 0x%.08X' +
        #13#10');',
        [DirectoryHandle, Buffer, Length, RestartScan, Context, ReturnLength]);

  // redirect to Win2k/XP
  Result := JwaNative.NtQueryDirectoryObject(DirectoryHandle, Buffer, Length,
    {ReturnSingleEntry=}False, // TODO -oDxbx : Is this the correct value?
    RestartScan, Context, ReturnLength);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtQueryDirectoryObject failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryEvent(
  EventHandle: HANDLE;
  EventInformation: PEVENT_BASIC_INFORMATION // OUT
): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  ResultLength: ULONG;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtQueryEvent' +
      #13#10'('+
      #13#10'   EventHandle         : 0x%.8x' +
      #13#10'   EventInformation    : 0x%.8x' +
      #13#10');',
      [EventHandle, EventInformation]);

  Result := JwaNative.NtQueryEvent(EventHandle, EventBasicInformation, EventInformation, SizeOf(EventInformation^), @ResultLength);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtQueryEvent failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryFullAttributesFile(
  ObjectAttributes: POBJECT_ATTRIBUTES;
  FileInformation: PFILE_NETWORK_OPEN_INFORMATION // OUT
): NTSTATUS; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
var
  NativeObjectAttributes: RNativeObjectAttributes;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtQueryFullAttributesFile'+
     #13#10'('+
     #13#10'   ObjectAttributes    : 0x%.08X ("%s")'+
     #13#10'   FileInformation     : 0x%.08X'+
     #13#10');',
     [ObjectAttributes, POBJECT_ATTRIBUTES_String(ObjectAttributes), FileInformation]);
{$ENDIF}

  // initialize object attributes
  NativeObjectAttributes := ObjectAttributesToNT(ObjectAttributes, 'NtQueryFullAttributesFile');

  Result := JwaNative.NtQueryFullAttributesFile(NativeObjectAttributes.NtObjAttrPtr, FileInformation);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtQueryFullAttributesFile failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryInformationFile(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK; //   OUT
  FileInformation: PVOID; //   OUT
  Length: ULONG;
  FileInfo: FILE_INFORMATION_CLASS
): NTSTATUS; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtQueryInformationFile' +
     #13#10'(' +
     #13#10'   FileHandle          : 0x%.08X' +
     #13#10'   IoStatusBlock       : 0x%.08X' +
     #13#10'   FileInformation     : 0x%.08X' +
     #13#10'   Length              : 0x%.08X' +
     #13#10'   FileInformationClass: 0x%.08X' +
     #13#10');',
     [FileHandle, IoStatusBlock, FileInformation,
      Length, Ord(FileInfo)]);
{$ENDIF}

  // TODO -oCxbx: IIRC, this function is depreciated.  Maybe we should just use
  // ZwQueryInformationFile instead?

// Cxbx commented this out :
//  if (FileInfo <> FilePositionInformation) and (FileInfo <> FileNetworkOpenInformation) then
//    DxbxKrnlCleanup('Unknown FILE_INFORMATION_CLASS 0x%.08X', [Ord(FileInfo)]);

  Result := JwaNative.NtQueryInformationFile(
    FileHandle,
    JwaNative.PIO_STATUS_BLOCK(IoStatusBlock),
    JwaNative.PFILE_FS_SIZE_INFORMATION(FileInformation),
    Length,
    JwaNative.FILE_INFORMATION_CLASS(FileInfo)
  );

  //
  // DEBUGGING!
  //
  begin
    (* Commented out by Cxbx
    asm int 3 end;
    NtDll::FILE_NETWORK_OPEN_INFORMATION *pInfo = (NtDll::FILE_NETWORK_OPEN_INFORMATION* )FileInformation;

    if (FileInfo = FileNetworkOpenInformation) and (pInfo.AllocationSize.LowPart = 57344) then
    begin
{$IFDEF DEBUG}
      DbgPrintf('pInfo.AllocationSize : %d', pInfo.AllocationSize.LowPart);
      DbgPrintf('pInfo.EndOfFile      : %d', pInfo.EndOfFile.LowPart);
{$ENDIF}

      pInfo.EndOfFile.LowPart := $1000;
      pInfo.AllocationSize.LowPart := $1000;

      fflush(stdout);
    end;
    *)
  end;

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtQueryInformationFile failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryIoCompletion(
  IoCompletionHandle: HANDLE;
  IoCompletionInformation: PIO_COMPLETION_BASIC_INFORMATION // OUT
): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryMutant(
  MutantHandle: HANDLE;
  MutantInformation: PMUTANT_BASIC_INFORMATION // OUT
): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  ResultLength: ULONG;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtQueryMutant' +
      #13#10'('+
      #13#10'   MutantHandle        : 0x%.8x' +
      #13#10'   MutantInformation   : 0x%.8x' +
      #13#10');',
      [MutantHandle, MutantInformation]);

  Result := JwaNative.NtQueryMutant(MutantHandle, MutantBasicInformation, MutantInformation, SizeOf(MutantInformation^), @ResultLength);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtQueryMutant failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQuerySemaphore(
  SemaphoreHandle: HANDLE;
  SemaphoreInformation: PSEMAPHORE_BASIC_INFORMATION // OUT
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  ResultLength: ULONG;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtQuerySemaphore' +
      #13#10'('+
      #13#10'   SemaphoreHandle     : 0x%.8x' +
      #13#10'   SemaphoreInformation: 0x%.8x' +
      #13#10');',
      [SemaphoreHandle, SemaphoreInformation]);

  Result := JwaNative.NtQuerySemaphore(SemaphoreHandle, SemaphoreBasicInformation, SemaphoreInformation, SizeOf(SemaphoreInformation^), @ResultLength);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtQuerySemaphore failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQuerySymbolicLinkObject(
  LinkHandle: HANDLE;
  LinkTarget: POBJECT_STRING; // OUT
  ReturnedLength: PULONG // OUT  OPTIONAL
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQuerySymbolicLinkObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryTimer(
  hTimerHandle: HANDLE;
  TimerInformationClass: TIMER_INFORMATION_CLASS;
  pTimerInformation: PVOID;
  TimerInformationLength: ULONG;
  ResultLength: PULONG
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfDxbx or lfKernel) then
    DbgPrintf('EmuKrnl : NtQueryTimer' +
      #13#10'(' +
      #13#10'   hTimerHandle           : 0x%.08X' +
      #13#10'   TimerInformationClass  : 0x%.08X' +
      #13#10'   pTimerInformation      : 0x%.08X' +
      #13#10'   TimerInformationLength : 0x%.08X' +
      #13#10'   ResultLength           : 0x%.08X' +
      #13#10');',
      [hTimerHandle, Ord(TimerInformationClass), pTimerInformation, TimerInformationLength, ResultLength]);

  Result := JwaNative.NtQueryTimer(hTimerHandle, TimerInformationClass, pTimerInformation, TimerInformationLength, ResultLength);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryVirtualMemory
(
  BaseAddress: PVOID;
  Buffer: PMEMORY_BASIC_INFORMATION
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  ReturnLength: ULONG;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtQueryVirtualMemory' +
     #13#10'(' +
     #13#10'   BaseAddress         : 0x%.08X' +
     #13#10'   Buffer              : 0x%.08X' +
     #13#10');',
     [BaseAddress, Buffer]);

  Result := JwaNative.NtQueryVirtualMemory
  (
      GetCurrentProcess(),
      BaseAddress,
      {(NtDll::MEMORY_INFORMATION_CLASS)NtDll::}MemoryBasicInformation,
      {(NtDll::PMEMORY_BASIC_INFORMATION)}Buffer,
      sizeof(MEMORY_BASIC_INFORMATION),
      @ReturnLength
  );

  if (Result <> STATUS_SUCCESS) then
  begin
    EmuWarning('NtQueryVirtualMemory failed! (%s)', [NTStatusToString(Result)]);
    if BaseAddress = Pointer($7FFF0000) then
    begin
      // Fix for "Forza Motorsport", which iterates over 2 Gb of memory in 64 Kb chunks,
      // but fails on this last query. It's not done though, as after this Forza tries to
      // NtAllocateVirtualMemory at address $00000000 (3 times, actually) which fails too...
      Buffer.BaseAddress := BaseAddress;
      Buffer.AllocationBase := BaseAddress;
      Buffer.AllocationProtect := 2;
      Buffer.RegionSize := 64 * 1024;
      Buffer.State := 4096;
      Buffer.Protect := 2;
      Buffer.Type_ := 262144;

      Result := STATUS_SUCCESS;
      DbgPrintf('EmuKrnl : NtQueryVirtualMemory : Applied fix for "Forza Motorsport" !');
    end;
  end;

  EmuSwapFS(fsXbox);
end;

// NtQueryVolumeInformation:
// Queries information about a file system.  This is not documented by
// Microsoft even under NT.
//
// Differences from NT: None known.
function xboxkrnl_NtQueryVolumeInformationFile
(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  FileInformation: PFILE_FS_SIZE_INFORMATION; // OUT
  Length: ULONG;
  FileInformationClass: FS_INFORMATION_CLASS
): NTSTATUS; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
var
  SizeInfo: PFILE_FS_SIZE_INFORMATION;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtQueryVolumeInformationFile' +
      #13#10'(' +
      #13#10'   FileHandle          : 0x%.08X' +
      #13#10'   IoStatusBlock       : 0x%.08X' +
      #13#10'   FileInformation     : 0x%.08X' +
      #13#10'   Length              : 0x%.08X' +
      #13#10'   FileInformationClass: 0x%.08X' +
      #13#10');',
      [FileHandle, IoStatusBlock, FileInformation,
       Length, Ord(FileInformationClass)]);

  // Safety/Sanity Check
  if (FileInformationClass <> FileFsSizeInformation) and (FileInformationClass <> FileFsVolumeInformation{FileDirectoryInformation}) then
    DxbxKrnlCleanup('NtQueryVolumeInformationFile: Unsupported FileInformationClass');

  Result := JwaNative.NtQueryVolumeInformationFile
  (
      FileHandle,
      JwaNative.PIO_STATUS_BLOCK(IoStatusBlock),
      PFILE_FS_SIZE_INFORMATION(FileInformation), Length,
      FS_INFORMATION_CLASS(FileInformationClass)
  );

  // NOTE: TODO -oCXBX: Dynamically fill in, or allow configuration?
  if (FileInformationClass = FileFsSizeInformation) then
  begin
    SizeInfo := PFILE_FS_SIZE_INFORMATION(FileInformation);

    SizeInfo.TotalAllocationUnits.QuadPart     := $4C468;
    SizeInfo.AvailableAllocationUnits.QuadPart := $2F125;
    SizeInfo.SectorsPerAllocationUnit          := 32;
    SizeInfo.BytesPerSector                    := 512;
  end;

  EmuSwapFS(fsXbox);
end;

// NtReadFile:
// Reads a file.
//
// Differences from NT: There is no Key parameter.
function xboxkrnl_NtReadFile
(
  FileHandle: HANDLE; // TODO -oCXBX: correct paramters
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PIO_APC_ROUTINE; // OPTIONAL
  ApcContext: PVOID;
  IoStatusBlock: PVOID; // OUT
  Buffer: PVOID; // OUT
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER // OPTIONAL
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfFile) then
    DbgPrintf('EmuKrnl : NtReadFile' +
        #13#10'(' +
        #13#10'   FileHandle          : 0x%.08X' +
        #13#10'   Event               : 0x%.08X' +
        #13#10'   ApcRoutine          : 0x%.08X' +
        #13#10'   ApcContext          : 0x%.08X' +
        #13#10'   IoStatusBlock       : 0x%.08X' +
        #13#10'   Buffer              : 0x%.08X' +
        #13#10'   Length              : 0x%.08X' +
        #13#10'   ByteOffset          : 0x%.08X (%d)' +
        #13#10');',
        [FileHandle, Event, Addr(ApcRoutine),
         ApcContext, IoStatusBlock, Buffer, Length, ByteOffset, QuadPart(ByteOffset)]);

// Halo...
//    if Assigned(ByteOffset) and (ByteOffset.QuadPart = $00120800) then
//      asm int 3 end;

  Result := JwaNative.NtReadFile(FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, JwaWinType.PLARGE_INTEGER(ByteOffset), nil);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtReadFile failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtReadFileScatter(
  FileHandle: HANDLE;
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PIO_APC_ROUTINE; // OPTIONAL
  ApcContext: PVOID; // OPTIONAL
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  SegmentArray: PFILE_SEGMENT_ELEMENT;
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER // OPTIONAL
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfFile) then
    DbgPrintf('EmuKrnl : NtReadFileScatter' +
        #13#10'(' +
        #13#10'   FileHandle          : 0x%.08X' +
        #13#10'   Event               : 0x%.08X' +
        #13#10'   ApcRoutine          : 0x%.08X' +
        #13#10'   ApcContext          : 0x%.08X' +
        #13#10'   IoStatusBlock       : 0x%.08X' +
        #13#10'   SegmentArray        : 0x%.08X' +
        #13#10'   Length              : 0x%.08X' +
        #13#10'   ByteOffset          : 0x%.08X (%d)' +
        #13#10');',
        [FileHandle, Event, Addr(ApcRoutine),
         ApcContext, IoStatusBlock, SegmentArray, Length, ByteOffset, QuadPart(ByteOffset)]);

  Result := JwaNative.NtReadFileScatter(FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, SegmentArray, Length, JwaWinType.PLARGE_INTEGER(ByteOffset), nil);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtReadFileScatter failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtReleaseMutant
(
  MutantHandle: HANDLE;
  PreviousCount: PULONG // OUT
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtReleaseMutant'+
      #13#10'('+
      #13#10'   MutantHandle         : 0x%.08X'+
      #13#10'   PreviousCount        : 0x%.08X'+
      #13#10');',
      [MutantHandle, PreviousCount]);

  // redirect to Win2k/XP
  Result := JwaNative.NtReleaseMutant(MutantHandle, PreviousCount);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtReleaseMutant failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);

  // TODO -oDxbx : Should we really fake success?
  Result := STATUS_SUCCESS;
end;

function xboxkrnl_NtReleaseSemaphore
(
  SemaphoreHandle: HANDLE;
  ReleaseCount: ULONG;
  PreviousCount: PULONG
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtReleaseSemaphore' +
     #13#10'(' +
     #13#10'   SemaphoreHandle      : 0x%.08X' +
     #13#10'   ReleaseCount         : 0x%.08X' +
     #13#10'   PreviousCount        : 0x%.08X' +
     #13#10');',
     [SemaphoreHandle, ReleaseCount, PreviousCount]);

  Result := JwaNative.NtReleaseSemaphore(SemaphoreHandle, ReleaseCount, PLONG(PreviousCount));

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtReleaseSemaphore failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtRemoveIoCompletion(
  IoCompletionHandle: HANDLE;
  KeyContext: PPVOID; // OUT
  ApcContext: PPVOID; // OUT
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtRemoveIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtResumeThread
(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG // OUT
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtResumeThread'+
      #13#10'('+
      #13#10'   ThreadHandle         : 0x%.08X'+
      #13#10'   PreviousSuspendCount : 0x%.08X'+
      #13#10');',
      [ThreadHandle, PreviousSuspendCount]);

  Result := JwaNative.NtResumeThread(ThreadHandle, PreviousSuspendCount);

  Sleep(10);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetEvent
(
  EventHandle: HANDLE;
  PreviousState: PLONG // OUT
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    DbgPrintf('EmuKrnl : NtSetEvent'+
      #13#10'('+
      #13#10'   EventHandle          : 0x%.08X'+
      #13#10'   PreviousState        : 0x%.08X'+
      #13#10');',
      [EventHandle, PreviousState]);

  Result := JwaNative.NtSetEvent(EventHandle, PULONG(PreviousState));

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtSetEvent failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetInformationFile
(
  FileHandle: HANDLE; // TODO -oCXBX: correct paramters
  IoStatusBlock: PVOID; // OUT
  FileInformation: PVOID;
  Length: ULONG;
  FileInformationClass: FILE_INFORMATION_CLASS
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    DbgPrintf('EmuKrnl : NtSetInformationFile' +
         #13#10'(' +
         #13#10'   FileHandle           : 0x%.08X' +
         #13#10'   IoStatusBlock        : 0x%.08X' +
         #13#10'   FileInformation      : 0x%.08X' +
         #13#10'   Length               : 0x%.08X' +
         #13#10'   FileInformationClass : 0x%.08X' +
         #13#10');',
         [FileHandle, IoStatusBlock, FileInformation,
         Length, Ord(FileInformationClass)]);

  // TODO some FileInformationClasses contain file paths.
  // These should be corrected just like NtCreateFile.
  // Other Nt functions might require the same attention
  Result := JwaNative.NtSetInformationFile(FileHandle, IoStatusBlock, FileInformation, Length, FileInformationClass);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetIoCompletion(
  IoCompletionHandle: HANDLE;
  KeyContext: PVOID;
  ApcContext: PVOID;
  IoStatus: NTSTATUS;
  IoStatusInformation: ULONG_PTR
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSetIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetSystemTime(
  {const}NewTime: PLARGE_INTEGER;
  OldTime: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
// Branch:wine  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  // TODO -oDxbx: Surely, we won't set the system time here, but we CAN remember a delta (and apply that in KeQuerySystemTime)
  Result := Unimplemented('NtSetSystemTime');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetTimerEx(
  TimerHandle: HANDLE;
  DueTime: PLARGE_INTEGER;
  TimerApcRoutine: PTIMER_APC_ROUTINE; {OPTIONAL}
  ApcMode: KPROCESSOR_MODE;
  TimerContext: PVOID;  {OPTIONAL}
  ResumeTimer: _BOOLEAN;
  Period: LONG; {OPTIONAL}
  {OUT} PreviousState: PBOOLEAN {OPTIONAL}
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfDxbx or lfKernel) then
    DbgPrintf('EmuKrnl : NtSetTimerEx' +
      #13#10'(' +
      #13#10'   TimerHandle            : 0x%.08X' +
      #13#10'   DueTime                : 0x%.08X (%d)' +
      #13#10'   TimerApcRoutine        : 0x%.08X' +
      #13#10'   ApcMode                : 0x%.08X' +
      #13#10'   TimerContext           : 0x%.08X' +
      #13#10'   ResumeTimer            : 0x%.08X' +
      #13#10'   Period                 : 0x%.08X' +
      #13#10'   PreviousState          : 0x%.08X' +
      #13#10');',
      [TimerHandle, DueTime, QuadPart(DueTime), Addr(TimerApcRoutine), Ord(ApcMode), TimerContext, ResumeTimer, Period, PreviousState]);

  Result := JwaNative.NtSetTimer{Ex}(TimerHandle, DueTime, TimerApcRoutine, {ApcMode,} TimerContext, ResumeTimer, Period, PreviousState);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSignalAndWaitForSingleObjectEx(
  SignalHandle: HANDLE;
  WaitHandle: HANDLE;
  WaitMode: KPROCESSOR_MODE;
  Alertable: BOOLEAN;
  Timeout: PLARGE_INTEGER // OPTIONAL
  ): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtSignalAndWaitForSingleObjectEx'+
      #13#10'('+
      #13#10'   SignalHandle         : 0x%.08X'+
      #13#10'   WaitHandle           : 0x%.08X'+
      #13#10'   WaitMode             : 0x%.08X'+
      #13#10'   Alertable            : 0x%.08X'+
      #13#10'   Timeout              : 0x%.08X (%d)'+
      #13#10');',
      [SignalHandle, WaitHandle, Ord(WaitMode), Alertable, Timeout, QuadPart(Timeout)]);

  // TODO -oDxbx : What should we do with the (currently ignored) WaitMode?
  Result := JwaNative.NtSignalAndWaitForSingleObject(SignalHandle, WaitHandle, Alertable, Timeout);

  if MayLog(lfUnit or lfTrace) then
    DbgPrintf('Finished waiting for 0x%.08X', [WaitHandle]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSuspendThread
(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG // OUT OPTIONAL
): NTSTATUS; stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtSuspendThread'+
      #13#10'('+
      #13#10'   ThreadHandle         : 0x%.08X'+
      #13#10'   PreviousSuspendCount : 0x%.08X'+
      #13#10');',
      [ThreadHandle, PreviousSuspendCount]);

  Result := JwaNative.NtSuspendThread(ThreadHandle, PreviousSuspendCount);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_NtUserIoApcDispatcher
(
  ApcContext: PVOID;
  IoStatusBlock: PIO_STATUS_BLOCK;
  Reserved: ULONG
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwEsi: uint32;
  dwEax: uint32;
  dwEcx: uint32;
  bWasXboxFS: Boolean; // Dxbx addition
begin
  // Cxbx Note: This function is called within Win2k/XP context, so no EmuSwapFS here

  // Dxbx note : Yeah, well, that may be so but it's still an kernel API,
  // so we'll just make sure that we're in PC mode for the logging at least!
  bWasXboxFS := EmuIsXboxFS(); // Dxbx addition
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    DbgPrintf('EmuKrnl : NtUserIoApcDispatcher' +
        #13#10'(' +
        #13#10'   ApcContext           : 0x%.08X' +
        #13#10'   IoStatusBlock        : 0x%.08X' +
        #13#10'   Reserved             : 0x%.08X' +
        #13#10');',
        [ApcContext, IoStatusBlock, Reserved]);

    DbgPrintf('IoStatusBlock->Pointer     : 0x%.08X' +
      #13#10'IoStatusBlock->Information : 0x%.08X', [IoStatusBlock.Status, IoStatusBlock.Information]);
  end;

  dwEsi := uint32(IoStatusBlock);

  if((IoStatusBlock.Status and $C0000000) = $C0000000) then
  begin
    dwEcx := 0;
    dwEax := JwaNative.RtlNtStatusToDosError(IoStatusBlock.Status);
  end
  else
  begin
    dwEcx := DWORD(IoStatusBlock.Information);
    dwEax := 0;
  end;

  if bWasXboxFS then // Dxbx addition : Swap back only here, if necessary
    EmuSwapFS(fsXbox);   // Xbox FS

  (*
  // ~XDK 3911??
  if(true) then
  begin
    dwEsi := dw2;
    dwEcx := dw1;
    dwEax := dw3;

  end
  else
  begin
    dwEsi := dw1;
    dwEcx := dw2;
    dwEax := dw3;
  end;*)

  asm
    pushad
    (*
    mov esi, IoStatusBlock
    mov ecx, dwEcx
    mov eax, dwEax
    *)
    // TODO -oCXBX: Figure out if/why this works!? Matches prototype, but not xboxkrnl disassembly
    // Seems to be XDK/version dependand??
    mov esi, dwEsi
    mov ecx, dwEcx
    mov eax, dwEax

    push esi
    push ecx
    push eax

    call ApcContext

    popad
  end;

  if MayLog(lfUnit or lfTrace) then
  begin
    EmuSwapFS(fsWindows);   // Win2k/XP FS
    DbgPrintf('EmuKrnl : NtUserIoApcDispatcher Completed');
    if bWasXboxFS then // Dxbx addition : Swap back only here, if necessary
    EmuSwapFS(fsXbox);
  end;
end;

function xboxkrnl_NtWaitForSingleObject(
  Handle: HANDLE;
  Alertable: _BOOLEAN;
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtWaitForSingleObject'+
      #13#10'('+
      #13#10'   Handle               : 0x%.08X'+
      #13#10'   Alertable            : 0x%.08X'+
      #13#10'   Timeout              : 0x%.08X (%d)'+
      #13#10');',
      [Handle, Alertable, Timeout, QuadPart(Timeout)]);

  Result := JwaNative.NtWaitForSingleObject(Handle, Alertable, Timeout);

  if MayLog(lfUnit or lfTrace) then
    DbgPrintf('Finished waiting for 0x%.08X', [Handle]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtWaitForSingleObjectEx
(
  Handle_: HANDLE;
  WaitMode: _CHAR;
  Alertable: LONGBOOL;
  Timeout: PLARGE_INTEGER
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtWaitForSingleObjectEx'+
      #13#10'('+
      #13#10'   Handle               : 0x%.08X'+
      #13#10'   WaitMode             : 0x%.08X'+
      #13#10'   Alertable            : 0x%.08X'+
      #13#10'   Timeout              : 0x%.08X (%d)'+
      #13#10');',
      [Handle_, Ord(WaitMode), Alertable, Timeout, QuadPart(Timeout)]);

  // TODO -oDxbx : What should we do with the (currently ignored) WaitMode?
  Result := JwaNative.NtWaitForSingleObject(Handle_, Alertable, Timeout);

  if MayLog(lfUnit or lfTrace) then
    DbgPrintf('Finished waiting for 0x%.08X', [Handle_]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtWaitForMultipleObjectsEx
(
  Count: ULONG;
  Handles: PHANDLE;
  WaitType: WAIT_TYPE;
  WaitMode: _CHAR;
  Alertable: LONGBOOL;
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : NtWaitForMultipleObjectsEx' +
      #13#10'(' +
      #13#10'   Count                : 0x%.08X' +
      #13#10'   Handles              : 0x%.08X' +
      #13#10'   WaitType             : 0x%.08X' +
      #13#10'   WaitMode             : 0x%.08X' +
      #13#10'   Alertable            : 0x%.08X' +
      #13#10'   Timeout              : 0x%.08X (%d)' +
      #13#10');',
      [Count, Handles, Ord(WaitType), Ord(WaitMode), Alertable,
       Timeout, QuadPart(Timeout)]);

  Result := JwaNative.NtWaitForMultipleObjects(Count, Handles, WaitType, Alertable, PLARGE_INTEGER(Timeout));

  EmuSwapFS(fsXbox);
end;

// NtWriteFile:
// Writes a file.
//
// Differences from NT: There is no Key parameter.
function xboxkrnl_NtWriteFile
(
  FileHandle: HANDLE; // TODO -oCXBX: correct paramters
  Event: DWORD; // Dxbx correction (was PVOID)
  ApcRoutine: PIO_APC_ROUTINE;
  ApcContext: PVOID;
  IoStatusBlock: PVOID; // OUT
  Buffer: PVOID;
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfFile) then
    DbgPrintf('EmuKrnl : NtWriteFile' +
       #13#10'(' +
       #13#10'   FileHandle          : 0x%.08X' +
       #13#10'   Event               : 0x%.08X' +
       #13#10'   ApcRoutine          : 0x%.08X' +
       #13#10'   ApcContext          : 0x%.08X' +
       #13#10'   IoStatusBlock       : 0x%.08X' +
       #13#10'   Buffer              : 0x%.08X' +
       #13#10'   Length              : 0x%.08X' +
       #13#10'   ByteOffset          : 0x%.08X (0x%.08X)' +
       #13#10');',
       [FileHandle, Event, Addr(ApcRoutine),
       ApcContext, IoStatusBlock, Buffer, Length, ByteOffset, QuadPart(ByteOffset)]);

  // Halo..
  //    if Assigned(ByteOffset) and (ByteOffset.QuadPart = $01C00800) then
  //      asm int 3 end

  Result := JwaNative.NtWriteFile(FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, JwaWinType.PLARGE_INTEGER(ByteOffset), nil);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtWriteFile failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtWriteFileGather(
  FileHandle: HANDLE;
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PIO_APC_ROUTINE; // OPTIONAL
  ApcContext: PVOID; // OPTIONAL
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  SegmentArray: PFILE_SEGMENT_ELEMENT;
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER // OPTIONAL
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfFile) then
    DbgPrintf('EmuKrnl : NtWriteFileGather' +
        #13#10'(' +
        #13#10'   FileHandle          : 0x%.08X' +
        #13#10'   Event               : 0x%.08X' +
        #13#10'   ApcRoutine          : 0x%.08X' +
        #13#10'   ApcContext          : 0x%.08X' +
        #13#10'   IoStatusBlock       : 0x%.08X' +
        #13#10'   SegmentArray        : 0x%.08X' +
        #13#10'   Length              : 0x%.08X' +
        #13#10'   ByteOffset          : 0x%.08X (%d)' +
        #13#10');',
        [FileHandle, Event, Addr(ApcRoutine),
         ApcContext, IoStatusBlock, SegmentArray, Length, ByteOffset, QuadPart(ByteOffset)]);

  Result := JwaNative.NtWriteFileGather(FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, SegmentArray, Length, JwaWinType.PLARGE_INTEGER(ByteOffset), nil);

  if (Result <> STATUS_SUCCESS) then
    EmuWarning('NtWriteFileGather failed! (%s)', [NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_NtYieldExecution(); stdcall;
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfExtreme) then
    // Cxbx NOTE: this eats up the debug log far too quickly
    DbgPrintf('EmuKrnl : NtYieldExecution();');

  JwaNative.NtYieldExecution();

  EmuSwapFS(fsXbox);
end;

end.
