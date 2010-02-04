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
  SysUtils,
  Windows,
  // Jedi
  JwaWinType,
  JwaWinBase,
  JwaWinNT,
  JwaNative,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uTypes,
  uLog,
  uEmu,
  uEmuFS,
  uEmuFile,
  uEmuXapi,
  uEmuKrnl,
  uDxbxUtils,
  uDxbxKrnl,
  uDxbxKrnlUtils;

function xboxkrnl_NtAllocateVirtualMemory(
  BaseAddress: PPVOID; // OUT * ?
  ZeroBits: ULONG;
  AllocationSize: PULONG; // OUT * ?
  AllocationType: DWORD;
  Protect: DWORD
  ): NTSTATUS; stdcall;
function xboxkrnl_NtCancelTimer(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtClearEvent(
  EventHandle: HANDLE
  ): NTSTATUS; stdcall;
function xboxkrnl_NtClose(
  Handle: Handle
  ): NTSTATUS; stdcall; {EXPORTNUM(187)}
function xboxkrnl_NtCreateDirectoryObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
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
  ShareAccess: ULONG; // dtACCESS_MASK;
  CreateDisposition: ULONG; // dtCreateDisposition;
  CreateOptions: ULONG // dtCreateOptions
  ): NTSTATUS; stdcall;
function xboxkrnl_NtCreateIoCompletion(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
function xboxkrnl_NtCreateMutant(
  MutantHandle: PHANDLE; // OUT
  ObjectAttributes: POBJECT_ATTRIBUTES;
  InitialOwner: LONGBOOL
  ): NTSTATUS; stdcall;
function xboxkrnl_NtCreateSemaphore(
  SemaphoreHandle: PHANDLE;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  InitialCount: ULONG;
  MaximumCount: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_NtCreateTimer(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
function xboxkrnl_NtDeleteFile(pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
function xboxkrnl_NtDeviceIoControlFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; pIoControlCode: dtU32; pInputBuffer: dtU32; InputBufferLength: dtU32; pOutputBuffer: dtU32; OutputBufferLength: dtU32): NTSTATUS; stdcall;
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
function xboxkrnl_NtFsControlFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; FsControlCode: dtU32; pInputBuffer: dtU32; InputBufferLength: dtU32; pOutputBuffer: dtU32; OutputBufferLength: dtU32): NTSTATUS; stdcall;
function xboxkrnl_NtOpenDirectoryObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtOpenFile(
  FileHandle: PHANDLE; // OUT
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  ShareAccess: ULONG; // dtACCESS_MASK;
  OpenOptions: ULONG // dtCreateOptions
  ): NTSTATUS; stdcall;
function xboxkrnl_NtOpenSymbolicLinkObject(pFileHandle: dtU32; pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
function xboxkrnl_NtProtectVirtualMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtPulseEvent(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
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
  ApcRoutine: PVOID; // Cxbx Todo: define this routine's prototype
  ApcContext: PVOID;
  IoStatusBlock: PIO_STATUS_BLOCK; // out
  FileInformation: PFILE_DIRECTORY_INFORMATION; // out
  Length: ULONG;
  FileInformationClass: FILE_INFORMATION_CLASS;
  FileMask: PSTRING;
  RestartScan: LONGBOOL
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryDirectoryObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtQueryEvent(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtQueryFullAttributesFile(
  ObjectAttributes: POBJECT_ATTRIBUTES;
  Attributes: PVOID // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryInformationFile(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK; //   OUT
  FileInformation: PVOID; //   OUT
  Length: ULONG;
  FileInfo: FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryIoCompletion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtQueryMutant(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtQuerySemaphore(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtQuerySymbolicLinkObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtQueryTimer(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtQueryVirtualMemory(
  pBaseAddress: PVOID;
  pBuffer: PMEMORY_BASIC_INFORMATION
  ): NTSTATUS; stdcall;
function xboxkrnl_NtQueryVolumeInformationFile(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  FileInformation: PFILE_FS_SIZE_INFORMATION; // OUT
  Length: ULONG;
  FileInformationClass: FS_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
function xboxkrnl_NtReadFile(
  FileHandle: HANDLE; // Cxbx TODO: correct paramters
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PVOID; // OPTIONAL
  ApcContext: PVOID;
  IoStatusBlock: PVOID; // OUT
  Buffer: PVOID; // OUT
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER // OPTIONAL
  ): NTSTATUS; stdcall;
function xboxkrnl_NtReadFileScatter(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtReleaseMutant(
  MutantHandle: HANDLE;
  PreviousCount: PULONG // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtReleaseSemaphore(
  SemaphoreHandle: HANDLE;
  ReleaseCount: ULONG;
  PreviousCount: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_NtRemoveIoCompletion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtResumeThread(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSetEvent(
  EventHandle: HANDLE;
  PreviousState: PULONG // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSetInformationFile(
  FileHandle: HANDLE; // Cxbx TODO: correct paramters
  IoStatusBlock: PVOID; // OUT
  FileInformation: PVOID;
  Length: ULONG;
  FileInformationClass: FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSetIoCompletion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtSetSystemTime(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtSetTimerEx(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtSignalAndWaitForSingleObjectEx(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtSuspendThread(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG // OUT OPTIONAL
  ): NTSTATUS; stdcall;
procedure xboxkrnl_NtUserIoApcDispatcher(
  ApcContext: PVOID;
  IoStatusBlock: PIO_STATUS_BLOCK;
  Reserved: ULONG
  ); stdcall;
function xboxkrnl_NtWaitForSingleObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtWaitForSingleObjectEx(
  Handle_: HANDLE;
  WaitMode: CHAR;
  Alertable: LONGBOOL;
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_NtWaitForMultipleObjectsEx(
  Count: ULONG;
  Handles: PHANDLE;
  WaitType: WAIT_TYPE;
  WaitMode: CHAR;
  Alertable: LONGBOOL;
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_NtWriteFile(
  FileHandle: HANDLE; // Cxbx TODO: correct paramters
  Event: DWORD; // Dxbx correction (was PVOID)
  ApcRoutine: PVOID;
  ApcContext: PVOID;
  IoStatusBlock: PVOID; // OUT
  Buffer: PVOID;
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_NtWriteFileGather(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_NtYieldExecution(); stdcall;

implementation

// NtAllocateVirtualMemory:
// Allocates virtual memory.
//
// Differences from NT: There is no ProcessHandle parameter.
function xboxkrnl_NtAllocateVirtualMemory(
  BaseAddress: PPVOID; // OUT * ?
  ZeroBits: ULONG;
  AllocationSize: PULONG; // OUT * ?
  AllocationType: DWORD;
  Protect: DWORD
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtAllocateVirtualMemory'+
      #13#10'('+
      #13#10'   BaseAddress         : 0x%.8x (0x%.8x)'+
      #13#10'   ZeroBits            : 0x%.8x' +
      #13#10'   AllocationSize      : 0x%.8x (0x%.8x)'+
      #13#10'   AllocationType      : 0x%.8x' +
      #13#10'   Protect             : 0x%.8x' +
      #13#10');',
      [BaseAddress, BaseAddress^, ZeroBits, AllocationSize, AllocationSize^, AllocationType, Protect]);
{$ENDIF}

  Result := JwaNative.NtAllocateVirtualMemory(GetCurrentProcess(), BaseAddress, ZeroBits, AllocationSize, AllocationType, Protect);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCancelTimer(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtCancelTimer');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtClearEvent(
  EventHandle: HANDLE
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtClearEvent'+
      #13#10'('+
      #13#10'   EventHandle         : 0x%.8x' +
      #13#10');',
      [EventHandle]);
{$ENDIF}

  Result := JwaNative.NtClearEvent(EventHandle);

  if (FAILED(Result)) then
    EmuWarning('NtClearEvent Failed!');

  EmuSwapFS(fsXbox);
end;

// NtClose:
// Closes a file or other handle.
//
// Differences from NT: None.
function xboxkrnl_NtClose(
  Handle: Handle
  ): NTSTATUS; stdcall; {XBSYSAPI EXPORTNUM(187)}
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
{$IFDEF DXBX_EMUHANDLES}
var
  iEmuHandle: TEmuHandle;
{$ENDIF}
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtClose' +
    #13#10'(' +
    #13#10'   Handle              : 0x%.8x' +
    #13#10');', [Handle]);
{$ENDIF}

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

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateDirectoryObject(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtCreateDirectoryObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateEvent(
  EventHandle: PHANDLE; // OUT
  ObjectAttributes: POBJECT_ATTRIBUTES; // OPTIONAL
  EventType: EVENT_TYPE;
  InitialState: LONGBOOL
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

(*    char *szBuffer = iif(Assigned(ObjectAttributes), ObjectAttributes.ObjectName.Buffer, '');

{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : NtCreateEvent' +
        #13#10'(' +
        #13#10'   EventHandle         : 0x%.08X' +
        #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
        #13#10'   EventType           : 0x%.08X' +
        #13#10'   InitialState        : 0x%.08X' +
        #13#10');',
        [EventHandle, ObjectAttributes, szBuffer,
         EventType, InitialState]);
{$ENDIF}

    wchar_t wszObjectName[160-1];

    NtDll::UNICODE_STRING    NtUnicodeString;
    NtDll::OBJECT_ATTRIBUTES NtObjAttr;

    // initialize object attributes
    if Assigned(szBuffer) then
    begin
        mbstowcs(wszObjectName, '\??\', 4);
        mbstowcs(wszObjectName+4, szBuffer, 160-1);

        NtDll::RtlInitUnicodeString(@NtUnicodeString, wszObjectName);

        InitializeObjectAttributes(@NtObjAttr, @NtUnicodeString, ObjectAttributes.Attributes, ObjectAttributes.RootDirectory, NULL);
    end;

    NtObjAttr.RootDirectory = 0;

    // redirect to NtCreateEvent
    NTSTATUS ret = NtDll::NtCreateEvent(EventHandle, EVENT_ALL_ACCESS, iif(Assigned(szBuffer), @NtObjAttr, nil), (NtDll::EVENT_TYPE)EventType, InitialState);

    if(FAILED(ret))
        EmuWarning('NtCreateEvent Failed!');

{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : NtCreateEvent EventHandle = 0x%.08X', [*EventHandle]);
{$ENDIF}


    return ret;*)



  Result := Unimplemented('NtCreateEvent');
  EmuSwapFS(fsXbox);
end;

// NtCreateFile:
// Creates or opens a file or device object.
//
// Differences from NT: The EaBuffer and EaLength options are gone.
//     OBJECT_ATTRIBUTES uses ANSI_STRING, so only ANSI filenames work.
function xboxkrnl_NtCreateFile(
  FileHandle: PHANDLE; // OUT
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  AllocationSize: PLARGE_INTEGER; // OPTIONAL,
  FileAttributes: ULONG;
  ShareAccess: ULONG; // dtACCESS_MASK;
  CreateDisposition: ULONG; // dtCreateDisposition;
  CreateOptions: ULONG // dtCreateOptions
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
var
  ReplaceChar: AnsiChar;
  ReplaceIndex: int;
  szBuffer: PAnsiChar;
  v: int;
  NtUnicodeString: UNICODE_STRING;
  wszObjectName: array [0..MAX_PATH-1] of wchar_t;
  NtObjAttr: JwaWinType.OBJECT_ATTRIBUTES;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtCreateFile' +
     #13#10'(' +
     #13#10'   FileHandle          : 0x%.08X' +
     #13#10'   DesiredAccess       : 0x%.08X' +
     #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
     #13#10'   IoStatusBlock       : 0x%.08X' +
     #13#10'   AllocationSize      : 0x%.08X' +
     #13#10'   FileAttributes      : 0x%.08X' +
     #13#10'   ShareAccess         : 0x%.08X' +
     #13#10'   CreateDisposition   : 0x%.08X' +
     #13#10'   CreateOptions       : 0x%.08X' +
     #13#10');',
     [FileHandle, DesiredAccess, ObjectAttributes, string(ObjectAttributes.ObjectName.Buffer),
     IoStatusBlock, AllocationSize, FileAttributes, ShareAccess, CreateDisposition, CreateOptions]);
{$ENDIF}

  ReplaceChar := #0;
  ReplaceIndex := -1;

  szBuffer := ObjectAttributes.ObjectName.Buffer;

  if Assigned(szBuffer) then
  begin
{$IFDEF DEBUG}
    //printf('Orig : %s', szBuffer); // MARKED OUT BY CXBX
{$ENDIF}

    // Trim this (\??\) off :
    if (szBuffer[0] = '\') and (szBuffer[1] = '?') and (szBuffer[2] = '?') and (szBuffer[3] = '\') then
      Inc(szBuffer, 4);

    // D:\ should map to current directory
    if ((szBuffer[0] = 'D') or (szBuffer[0] = 'd')) and (szBuffer[1] = ':') and (szBuffer[2] = '\') then
    begin
      Inc(szBuffer, 3);

      ObjectAttributes.RootDirectory := g_hCurDir;

{$IFDEF DEBUG}
      DbgPrintf('EmuKrnl : NtCreateFile Corrected path...');
      DbgPrintf('  Org:"%s"', [string(ObjectAttributes.ObjectName.Buffer)]);
      DbgPrintf('  New:"$XbePath\%s"', [szBuffer]);
{$ENDIF}
    end
    else
    if ((szBuffer[0] = 'T') or (szBuffer[0] = 't')) and (szBuffer[1] = ':') and (szBuffer[2] = '\') then
    begin
      Inc(szBuffer, 3);

      ObjectAttributes.RootDirectory := g_hTDrive;

{$IFDEF DEBUG}
      DbgPrintf('EmuKrnl : NtCreateFile Corrected path...');
      DbgPrintf('  Org:"%s"', [string(ObjectAttributes.ObjectName.Buffer)]);
      DbgPrintf('  New:"$CxbxPath\EmuDisk\T\%s"', [szBuffer]);
{$ENDIF}
    end
    else if ((szBuffer[0] = 'U') or (szBuffer[0] = 'u')) and (szBuffer[1] = ':') and (szBuffer[2] = '\') then
    begin
      Inc(szBuffer, 3);

      ObjectAttributes.RootDirectory := g_hUDrive;

{$IFDEF DEBUG}
      DbgPrintf('EmuKrnl : NtCreateFile Corrected path...');
      DbgPrintf('  Org:"%s"', [string(ObjectAttributes.ObjectName.Buffer)]);
      DbgPrintf('  New:"$CxbxPath\EmuDisk\U\%s"', [szBuffer]);
{$ENDIF}
    end
    else if ((szBuffer[0] = 'Z') or (szBuffer[0] = 'z')) and (szBuffer[1] = ':') and (szBuffer[2] = '\') then
    begin
      Inc(szBuffer, 3);

      ObjectAttributes.RootDirectory := g_hZDrive;

{$IFDEF DEBUG}
      DbgPrintf('EmuKrnl : NtCreateFile Corrected path...');
      DbgPrintf('  Org:"%s"', [string(ObjectAttributes.ObjectName.Buffer)]);
      DbgPrintf('  New:"$CxbxPath\EmuDisk\Z\%s"', [szBuffer]);
{$ENDIF}
    end;

    // Ignore wildcards. Xapi FindFirstFile uses the same path buffer for
    // NtOpenFile and NtQueryDirectoryFile. Wildcards are only parsed by
    // the latter.
    begin
      v := 0;
      while szBuffer[v] <> #0 do
      begin
        // FIXME: Fallback to parent directory if wildcard is found.
        if (szBuffer[v] = '*') then
        begin
          ReplaceIndex := v;
          Break;
        end;

        Inc(v);
      end;
    end;

    // Note: Hack: Not thread safe (if problems occur, create a temp buffer)
    if (ReplaceIndex > -1) then
    begin
      ReplaceChar := szBuffer[ReplaceIndex];
      szBuffer[ReplaceIndex] := #0;
    end;

    //printf('Aftr : %s', szBuffer); // MARKED OUT BY CXBX
  end;

  // initialize object attributes
  if Assigned(szBuffer) then
    mbstowcs(@(wszObjectName[0]), szBuffer, MAX_PATH-1)
  else
    wszObjectName[0] := 0;

  JwaNative.RtlInitUniCodeString(@NtUnicodeString, @(wszObjectName[0]));
  InitializeObjectAttributes(@NtObjAttr, @NtUnicodeString, ObjectAttributes.Attributes, ObjectAttributes.RootDirectory, NULL);

  // redirect to NtCreateFile
  Result := JwaNative.NtCreateFile(
      FileHandle, DesiredAccess, @NtObjAttr, JwaNative.PIO_STATUS_BLOCK(IoStatusBlock),
      JwaWinType.PLARGE_INTEGER(AllocationSize), FileAttributes, ShareAccess, CreateDisposition, CreateOptions, NULL, 0
  );

  // If we're trying to open a regular file as a directory, fallback to
  // parent directory. This behavior is required by Xapi FindFirstFile.
  if (Result = STATUS_NOT_A_DIRECTORY) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : NtCreateFile fallback to parent directory');
{$ENDIF}

    // Restore original buffer.
    if (ReplaceIndex > -1) then
      szBuffer[ReplaceIndex] := ReplaceChar;

    // Strip filename from path.
    v := strlen(szBuffer) - 1;
    while (v >= 0) do
    begin
      if (szBuffer[v] = '\') then
      begin
        ReplaceIndex := v;
        Break;
      end;

      Dec(v);
    end;

    if (v = -1) then
      ReplaceIndex := 0;

    // Modify buffer again.
    ReplaceChar := szBuffer[ReplaceIndex];
    szBuffer[ReplaceIndex] := #0;
{$IFDEF DEBUG}
    DbgPrintf('  New:"$CurRoot\%s"', [szBuffer]);
{$ENDIF}

    mbstowcs(@(wszObjectName[0]), szBuffer, MAX_PATH-1);
    JwaNative.RtlInitUnicodeString(@NtUnicodeString, @(wszObjectName[0]));

    Result := JwaNative.NtCreateFile(
        FileHandle, DesiredAccess, @NtObjAttr, JwaNative.PIO_STATUS_BLOCK(IoStatusBlock),
        JwaWinType.PLARGE_INTEGER(AllocationSize), FileAttributes, ShareAccess, CreateDisposition, CreateOptions, NULL, 0
    );
  end;

{$IFDEF DEBUG}
  if FAILED(Result) then
    DbgPrintf('EmuKrnl : NtCreateFile Failed! (0x%.08X)', [Result])
  else
    DbgPrintf('EmuKrnl : NtCreateFile = 0x%.08X', [FileHandle^]);
{$ENDIF}

  // restore original buffer
  if (ReplaceIndex <> -1) then
    szBuffer[ReplaceIndex] := ReplaceChar;

  // NOTE: We can map this to IoCreateFile once implemented (if ever necessary)
  //       xboxkrnl::IoCreateFile(FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, AllocationSize, FileAttributes, ShareAccess, CreateDisposition, CreateOptions, 0);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateIoCompletion(
  FileHandle: dtU32;
  DesiredAccess: dtACCESS_MASK;
  pObjectAttributes: dtObjectAttributes;
  pszUnknownArgs: dtBLOB
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtCreateIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateMutant(
  MutantHandle: PHANDLE; // OUT
  ObjectAttributes: POBJECT_ATTRIBUTES;
  InitialOwner: LONGBOOL
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

(*    char *szBuffer = iif(Assigned(ObjectAttributes), ObjectAttributes.ObjectName.Buffer, '');

{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : NtCreateMutant' +
       #13#10'(' +
       #13#10'   MutantHandle        : 0x%.08X' +
       #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
       #13#10'   InitialOwner        : 0x%.08X' +
       #13#10');',
       [MutantHandle, ObjectAttributes, szBuffer, InitialOwner]);
{$ENDIF}

    wchar_t wszObjectName[160-1];

    NtDll::UNICODE_STRING    NtUnicodeString;
    NtDll::OBJECT_ATTRIBUTES NtObjAttr;

    // initialize object attributes
    if Assigned((szBuffer) then
    begin
        mbstowcs(wszObjectName, '\??\', 4);
        mbstowcs(wszObjectName+4, szBuffer, 160-1);

        NtDll::RtlInitUnicodeString(@NtUnicodeString, wszObjectName);

        InitializeObjectAttributes(@NtObjAttr, @NtUnicodeString, ObjectAttributes.Attributes, ObjectAttributes.RootDirectory, NULL);
    end;

    NtObjAttr.RootDirectory = 0;

    // redirect to NtCreateMutant
    NTSTATUS ret = NtDll::NtCreateMutant(MutantHandle, MUTANT_ALL_ACCESS, iif(Assigned(szBuffer), @NtObjAttr, nil), InitialOwner);

    if(FAILED(ret))
        EmuWarning('NtCreateMutant Failed!');

{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : NtCreateMutant MutantHandle = 0x%.08X', [*MutantHandle]);
{$ENDIF}


    return ret;*)

  Result := Unimplemented('NtCreateMutant');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateSemaphore(
  SemaphoreHandle: PHANDLE;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  InitialCount: ULONG;
  MaximumCount: ULONG
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtCreateSemaphore' +
     #13#10'(' +
     #13#10'   SemaphoreHandle     : 0x%.08X' +
     #13#10'   ObjectAttributes    : 0x%.08X' +
     #13#10'   InitialCount        : 0x%.08X' +
     #13#10'   MaximumCount        : 0x%.08X' +
     #13#10');',
     [SemaphoreHandle, ObjectAttributes,
     InitialCount, MaximumCount]);
{$ENDIF}

  // redirect to Win2k/XP
  Result := JwaNative.NtCreateSemaphore
  (
      SemaphoreHandle,
      SEMAPHORE_ALL_ACCESS,
      JwaWinType.POBJECT_ATTRIBUTES(ObjectAttributes),
      InitialCount,
      MaximumCount
  );

  if (FAILED(Result)) then
    EmuWarning('NtCreateSemaphore failed!');

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtCreateSemaphore SemaphoreHandle = 0x%.08X', [SemaphoreHandle^]);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateTimer(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtCreateTimer');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtDeleteFile(pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtDeleteFile');
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
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtDeviceIoControlFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtDuplicateObject(
  SourceHandle: HANDLE;
  TargetHandle: PHANDLE;
  Options: DWORD
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  ret: NTSTATUS;
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

  // redirect to Win2k/XP
  ret := NtDuplicateObject
  (
      GetCurrentProcess(),
      SourceHandle,
      GetCurrentProcess(),
      TargetHandle,
      0, 0, Options
  );

  if not (ret = STATUS_SUCCESS) then
      EmuWarning('Object was not duplicated!');

  Result := ret;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtFlushBuffersFile(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK // OUT
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  ret: NTSTATUS;
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

  ret := JwaNative.NtFlushBuffersFile(FileHandle, PIO_STATUS_BLOCK(IoStatusBlock));
  Result := ret;
  EmuSwapFS(fsXbox);
end;

// NtFreeVirtualMemory:
// Frees virtual memory.
//
// Differences from NT: There is no ProcessHandle parameter.
function xboxkrnl_NtFreeVirtualMemory(
  BaseAddress: PPVOID; // OUT
  FreeSize: PULONG; // OUT
  FreeType: ULONG
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
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

  Result := NtFreeVirtualMemory(GetCurrentProcess(), BaseAddress, FreeSize, FreeType);

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
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtFsControlFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtOpenDirectoryObject(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtOpenDirectoryObject');
  EmuSwapFS(fsXbox);
end;

// NtOpenFile:
// Opens a file or device object.  Same as calling:
//   NtCreateFile(FileHandle, DesiredAccess, ObjectAttributes,
//     IoStatusBlock, NULL, 0, ShareAccess, OPEN_EXISTING, OpenOptions);
//
// Differences from NT: See NtCreateFile.
function xboxkrnl_NtOpenFile(
  FileHandle: PHANDLE; // OUT
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  ShareAccess: ULONG; // dtACCESS_MASK;
  OpenOptions: ULONG // dtCreateOptions
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuKrnl : NtOpenFile' +
    #13#10'(' +
    #13#10'   FileHandle          : 0x%.08X' +
    #13#10'   DesiredAccess       : 0x%.08X' +
    #13#10'   ObjectAttributes    : 0x%.08X ("%s")' +
    #13#10'   IoStatusBlock       : 0x%.08X' +
    #13#10'   ShareAccess         : 0x%.08X' +
    #13#10'   CreateOptions       : 0x%.08X' +
    #13#10');',
    [FileHandle, DesiredAccess, ObjectAttributes, ObjectAttributes.ObjectName.Buffer,
     IoStatusBlock, ShareAccess, OpenOptions]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  Result := xboxkrnl_NtCreateFile(FileHandle, DesiredAccess, ObjectAttributes,
    IoStatusBlock, NULL, 0, ShareAccess, OPEN_EXISTING, OpenOptions);
end;

function xboxkrnl_NtOpenSymbolicLinkObject(pFileHandle: dtU32; pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtOpenSymbolicLinkObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtProtectVirtualMemory(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtProtectVirtualMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtPulseEvent(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtPulseEvent');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueueApcThread(
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

  // Cxbx TODO: Not too sure how this one works.  If there's any special *magic* that needs to be
  //     done, let me know!
  Result := JwaNative.NtQueueApcThread(
    ThreadHandle,
    PKNORMAL_ROUTINE(ApcRoutine),
    ApcRoutineContext,
    PIO_STATUS_BLOCK(ApcStatusBlock),
    Pointer(ApcReserved));

  if (FAILED(Result)) then
    EmuWarning('NtQueueApcThread failed!');

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryDirectoryFile(
  FileHandle: HANDLE;
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PVOID; // Cxbx Todo: define this routine's prototype
  ApcContext: PVOID;
  IoStatusBlock: PIO_STATUS_BLOCK; // out
  FileInformation: PFILE_DIRECTORY_INFORMATION; // out
  Length: ULONG;
  FileInformationClass: FILE_INFORMATION_CLASS;
  FileMask: PSTRING;
  RestartScan: LONGBOOL
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

(*
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
        #13#10'   FileMask             : 0x%.08X (%s)' +
        #13#10'   RestartScan          : 0x%.08X' +
        #13#10');',
        [FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock,
         FileInformation, Length, FileInformationClass, FileMask,
         iif(Assigned(FileMask), FileMask.Buffer, ''), RestartScan]);
{$ENDIF}

    NTSTATUS ret;

    if (FileInformationClass <> 1)   // Due to unicode->string conversion
        CxbxKrnlCleanup('Unsupported FileInformationClass');

    NtDll::UNICODE_STRING NtFileMask;

    wchar_t wszObjectName[160-1];

    // initialize FileMask
    {
        if Assigned(FileMask) then
            mbstowcs(wszObjectName, FileMask.Buffer, 160-1);
        else
            mbstowcs(wszObjectName, '', 160-1);

        NtDll::RtlInitUnicodeString(@NtFileMask, wszObjectName);
    }

    NtDll::FILE_DIRECTORY_INFORMATION *FileDirInfo = (NtDll::FILE_DIRECTORY_INFORMATION*)(*CxbxMalloc($40 + 160*2);

    char    *mbstr = FileInformation.FileName;
    wchar_t *wcstr = FileDirInfo.FileName;

    do
    {
        ZeroMemory(wcstr, 160*2);

        ret = NtDll::NtQueryDirectoryFile
        (
            FileHandle, Event, (NtDll::PIO_APC_ROUTINE)ApcRoutine, ApcContext, (NtDll::IO_STATUS_BLOCK*)(*IoStatusBlock, FileDirInfo,
            $40+160*2, (NtDll::FILE_INFORMATION_CLASS)FileInformationClass, TRUE, @NtFileMask, RestartScan
        );

        // convert from PC to Xbox
        {
            memcpy(FileInformation, FileDirInfo, $40);

            wcstombs(mbstr, wcstr, 160);

            FileInformation.FileNameLength /= 2;
        }(*

        RestartScan = FALSE;
    }
    // Xbox does not return . and ..
    while (strcmp(mbstr, '.') = 0) or (strcmp(mbstr, '..') = 0);

    // TODO: Cache the last search result for quicker access with CreateFile (xbox does this internally!)
    CxbxFree(FileDirInfo);

    return ret;*)

  Result := Unimplemented('NtQueryDirectoryFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryDirectoryObject(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryDirectoryObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryEvent(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryEvent');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryFullAttributesFile(
  ObjectAttributes: POBJECT_ATTRIBUTES;
  Attributes: PVOID // OUT
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:10
var
(*  szBuffer: PAnsiChar;
  wszObjectName: array [0..MAX_PATH - 1] of wchar_t;
  NtUnicodeString: UNICODE_STRING; *)
  NtObjAttr: OBJECT_ATTRIBUTES;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtQueryFullAttributesFile'+
     #13#10'('+
     #13#10'   ObjectAttributes    : 0x%.08X (%s)'+
     #13#10'   Attributes          : 0x%.08X'+
     #13#10');',
     [ObjectAttributes, ObjectAttributes.ObjectName.Buffer, Attributes]);
{$ENDIF}

(*  szBuffer := ObjectAttributes.ObjectName.Buffer;

  // initialize object attributes
  mbstowcs(wszObjectName, szBuffer, 160-1);
  RtlInitUnicodeString(@NtUnicodeString, wszObjectName);
  InitializeObjectAttributes(@NtObjAttr, @NtUnicodeString, ObjectAttributes.Attributes, ObjectAttributes.RootDirectory, NULL);
*)
  Result := NtQueryFullAttributesFile(@NtObjAttr, Attributes);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryInformationFile(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK; //   OUT
  FileInformation: PVOID; //   OUT
  Length: ULONG;
  FileInfo: FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
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

// Cxbx commented this out :
//  if (FileInfo <> FilePositionInformation) and (FileInfo <> FileNetworkOpenInformation) then
//    CxbxKrnlCleanup('Unknown FILE_INFORMATION_CLASS 0x%.08X', [Ord(FileInfo)]);

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
    _asm int 3;
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

  if (FAILED(Result)) then
    EmuWarning('NtQueryInformationFile failed!');

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryIoCompletion(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryMutant(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryMutant');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQuerySemaphore(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQuerySemaphore');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQuerySymbolicLinkObject(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQuerySymbolicLinkObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryTimer(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryTimer');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryVirtualMemory(
  pBaseAddress: PVOID;
  pBuffer: PMEMORY_BASIC_INFORMATION
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtQueryVirtualMemory' +
     #13#10'(' +
     #13#10'   pBaseAddress         : 0x%.08X' +
     #13#10'   pBuffer              : 0x%.08X' +
     #13#10');',
     [pBaseAddress, pBuffer]);
{$ENDIF}

  Result := JwaNative.NtQueryVirtualMemory
  (
      GetCurrentProcess(),
      pBaseAddress,
      {(NtDll::MEMORY_INFORMATION_CLASS)NtDll::}MemoryBasicInformation,
      {(NtDll::PMEMORY_BASIC_INFORMATION)}pBuffer,
      SizeOf(MEMORY_BASIC_INFORMATION),
      nil
  );

  if (FAILED(Result)) then
    EmuWarning('NtQueryVirtualMemory failed!');

  EmuSwapFS(fsXbox);
end;

// NtQueryVolumeInformation:
// Queries information about a file system.  This is not documented by
// Microsoft even under NT.
//
// Differences from NT: None known.
function xboxkrnl_NtQueryVolumeInformationFile(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  FileInformation: PFILE_FS_SIZE_INFORMATION; // OUT
  Length: ULONG;
  FileInformationClass: FS_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

(*
{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : NtQueryVolumeInformationFile' +
        #13#10'(' +
        #13#10'   FileHandle          : 0x%.08X' +
        #13#10'   IoStatusBlock       : 0x%.08X' +
        #13#10'   FileInformation     : 0x%.08X' +
        #13#10'   Length              : 0x%.08X' +
        #13#10'   FileInformationClass: 0x%.08X' +
        #13#10');',
        [FileHandle, IoStatusBlock, FileInformation,
         Length, FileInformationClass]);
{$ENDIF}

    // Safety/Sanity Check
    if (FileInformationClass <> FileFsSizeInformation) and (FileInformationClass <> FileDirectoryInformation) then
        CxbxKrnlCleanup('NtQueryVolumeInformationFile: Unsupported FileInformationClass');

    NTSTATUS ret = NtDll::NtQueryVolumeInformationFile
    (
        FileHandle,
        (NtDll::PIO_STATUS_BLOCK)IoStatusBlock,
        (NtDll::PFILE_FS_SIZE_INFORMATION)FileInformation, Length,
        (NtDll::FS_INFORMATION_CLASS)FileInformationClass
    );

    // NOTE: TODO: Dynamically fill in, or allow configuration?
    if(FileInformationClass = FileFsSizeInformation)
    {
        FILE_FS_SIZE_INFORMATION *SizeInfo = (FILE_FS_SIZE_INFORMATION*)(*FileInformation;

        SizeInfo.TotalAllocationUnits.QuadPart     = $4C468;
        SizeInfo.AvailableAllocationUnits.QuadPart = $2F125;
        SizeInfo.SectorsPerAllocationUnit          = 32;
        SizeInfo.BytesPerSector                    = 512;
    }

    return ret;*)

  Result := Unimplemented('NtQueryVolumeInformationFile');
  EmuSwapFS(fsXbox);
end;

// NtReadFile:
// Reads a file.
//
// Differences from NT: There is no Key parameter.
function xboxkrnl_NtReadFile(
  FileHandle: HANDLE; // Cxbx TODO: correct paramters
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PVOID; // OPTIONAL
  ApcContext: PVOID;
  IoStatusBlock: PVOID; // OUT
  Buffer: PVOID; // OUT
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER // OPTIONAL
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl (0x%X): NtReadFile' +
        #13#10'(' +
        #13#10'   FileHandle          : 0x%.08X' +
        #13#10'   Event               : 0x%.08X' +
        #13#10'   ApcRoutine          : 0x%.08X' +
        #13#10'   ApcContext          : 0x%.08X' +
        #13#10'   IoStatusBlock       : 0x%.08X' +
        #13#10'   Buffer              : 0x%.08X' +
        #13#10'   Length              : 0x%.08X' +
        #13#10'   ByteOffset          : 0x%.08X' + // ' (0x%.08X)' +
        #13#10');',
        [FileHandle, Event, ApcRoutine,
         ApcContext, IoStatusBlock, Buffer, Length, ByteOffset{, ByteOffset = 0 ? 0 : ByteOffset.QuadPart}]);
{$ENDIF}

// Halo...
//    if Assigned(ByteOffset) and (ByteOffset.QuadPart = $00120800) then
//        _asm int 3

  Result := JwaNative.NtReadFile(FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, JwaWinType.PLARGE_INTEGER(ByteOffset), nil);

  if (FAILED(Result)) then
    EmuWarning('NtReadFile Failed! (0x%.08X)', [Result]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtReadFileScatter(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtReadFileScatter');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtReleaseMutant(
  MutantHandle: HANDLE;
  PreviousCount: PULONG // OUT
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:50
var
  ret: NTSTATUS;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtReleaseMutant'+
      #13#10'('+
      #13#10'   MutantHandle         : 0x%.08X'+
      #13#10'   PreviousCount        : 0x%.08X'+
      #13#10');',
      [MutantHandle, PreviousCount]);
{$ENDIF}

  // redirect to NtCreateMutant
  ret := JwaNative.NtReleaseMutant(MutantHandle, PreviousCount);

  if(FAILED(ret)) then
    EmuWarning('NtReleaseMutant Failed!');

  Result := STATUS_SUCCESS;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtReleaseSemaphore(
  SemaphoreHandle: HANDLE;
  ReleaseCount: ULONG;
  PreviousCount: PULONG
  ): NTSTATUS; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtReleaseSemaphore' +
     #13#10'(' +
     #13#10'   SemaphoreHandle      : 0x%.08X' +
     #13#10'   ReleaseCount         : 0x%.08X' +
     #13#10'   PreviousCount        : 0x%.08X' +
     #13#10');',
     [SemaphoreHandle, ReleaseCount, PreviousCount]);
{$ENDIF}

  Result := JwaNative.NtReleaseSemaphore(SemaphoreHandle, ReleaseCount, PLONG(PreviousCount));

  if (FAILED(Result)) then
    EmuWarning('NtReleaseSemaphore failed!');

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtRemoveIoCompletion(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtRemoveIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtResumeThread(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG // OUT
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  ret: NTSTATUS;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtResumeThread'+
      #13#10'('+
      #13#10'   ThreadHandle         : 0x%.08X'+
      #13#10'   PreviousSuspendCount : 0x%.08X'+
      #13#10');',
      [ThreadHandle, PreviousSuspendCount]);
{$ENDIF}

  ret := NtResumeThread(ThreadHandle, PreviousSuspendCount);

  Sleep(10);
  Result := ret;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetEvent(
  EventHandle: HANDLE;
  PreviousState: PULONG // OUT
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  ret: NTSTATUS;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtSetEvent'+
      #13#10'('+
      #13#10'   EventHandle          : 0x%.08X'+
      #13#10'   PreviousState        : 0x%.08X'+
      #13#10');',
      [EventHandle, PreviousState]);
{$ENDIF}

  ret := JwaNative.NtSetEvent(EventHandle, PreviousState);

  if(FAILED(ret)) then
      EmuWarning('NtSetEvent Failed!');

  Result := ret;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetInformationFile(
  FileHandle: HANDLE; // Cxbx TODO: correct paramters
  IoStatusBlock: PVOID; // OUT
  FileInformation: PVOID;
  Length: ULONG;
  FileInformationClass: FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
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
{$ENDIF}

  Result := JwaNative.NtSetInformationFile(FileHandle, IoStatusBlock, FileInformation, Length, FileInformationClass);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetIoCompletion(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSetIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetSystemTime(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSetSystemTime');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetTimerEx(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSetTimerEx');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSignalAndWaitForSingleObjectEx(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSignalAndWaitForSingleObjectEx');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSuspendThread(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG // OUT OPTIONAL
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  ret: NTSTATUS;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtSuspendThread'+
      #13#10'('+
      #13#10'   ThreadHandle         : 0x%.08X'+
      #13#10'   PreviousSuspendCount : 0x%.08X'+
      #13#10');',
      [ThreadHandle, PreviousSuspendCount]);
{$ENDIF}

  ret := NtSuspendThread(ThreadHandle, PreviousSuspendCount);
  Result := ret;
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_NtUserIoApcDispatcher(
  ApcContext: PVOID;
  IoStatusBlock: PIO_STATUS_BLOCK;
  Reserved: ULONG
  ); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    // Note: This function is called within Win2k/XP context, so no EmuSwapFS here

(*
{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtUserIoApcDispatcher' +
        #13#10'(' +
        #13#10'   ApcContext           : 0x%.08X' +
        #13#10'   IoStatusBlock        : 0x%.08X' +
        #13#10'   Reserved             : 0x%.08X' +
        #13#10');',
        [ApcContext, IoStatusBlock, Reserved]);

    DbgPrintf('IoStatusBlock.Pointer     : 0x%.08X' +
        #13#10'IoStatusBlock.Information : 0x%.08X',
        [IoStatusBlock.u1.Pointer, IoStatusBlock.Information]);
{$ENDIF}

    EmuSwapFS();   // Xbox FS

    uint32 dwEsi, dwEax, dwEcx;

    dwEsi = (uint32)IoStatusBlock;

    if((IoStatusBlock.u1.Status and $C0000000) = $C0000000)
    {
        dwEcx = 0;
        dwEax = NtDll::RtlNtStatusToDosError(IoStatusBlock.u1.Status);
    }
    else
    {
        dwEcx = (DWORD)IoStatusBlock.Information;
        dwEax = 0;
    }

    /*
    // ~XDK 3911??
    if(true)
    {
        dwEsi = dw2;
        dwEcx = dw1;
        dwEax = dw3;

    }
    else
    {
        dwEsi = dw1;
        dwEcx = dw2;
        dwEax = dw3;
    }//*/

    __asm
    {
        pushad
        /*
        mov esi, IoStatusBlock
        mov ecx, dwEcx
        mov eax, dwEax
        */
        // TODO: Figure out if/why this works!? Matches prototype, but not xboxkrnl disassembly
        // Seems to be XDK/version dependand??
        mov esi, dwEsi
        mov ecx, dwEcx
        mov eax, dwEax

        push esi
        push ecx
        push eax

        call ApcContext

        popad
    }

    EmuSwapFS();   // Win2k/XP FS

{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : NtUserIoApcDispatcher Completed');
{$ENDIF}

    return;*)


  EmuSwapFS(fsWindows);
  Unimplemented('NtUserIoApcDispatcher');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtWaitForSingleObject(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtWaitForSingleObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtWaitForSingleObjectEx(
  Handle_: HANDLE;
  WaitMode: CHAR;
  Alertable: LONGBOOL;
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
(*  DbgPrintf('EmuKrnl : NtWaitForSingleObjectEx'+
      #13#10'(\'+
      #13#10'   Handle               : 0x%.08X'+
      #13#10'   WaitMode             : 0x%.08X'+
      #13#10'   Alertable            : 0x%.08X'+
      #13#10'   Timeout              : 0x%.08X (%d)'+
      #13#10');',
      Handle, WaitMode, Alertable, Timeout, Timeout == 0 ? 0 : Timeout.QuadPart); *)
{$ENDIF}

(*    NTSTATUS ret = NtDll::NtWaitForSingleObject(Handle, Alertable, (NtDll::PLARGE_INTEGER)Timeout);

{$IFDEF DEBUG}
    DbgPrintf('Finished waiting for 0x%.08X', [Handle]);
{$ENDIF}

    return ret;
*)

  Result := Unimplemented('NtWaitForSingleObjectEx');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtWaitForMultipleObjectsEx(
  Count: ULONG;
  Handles: PHANDLE;
  WaitType: WAIT_TYPE;
  WaitMode: CHAR;
  Alertable: LONGBOOL;
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

(*
{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : NtWaitForMultipleObjectsEx' +
        #13#10'(' +
        #13#10'   Count                : 0x%.08X' +
        #13#10'   Handles              : 0x%.08X' +
        #13#10'   WaitType             : 0x%.08X' +
        #13#10'   WaitMode             : 0x%.08X' +
        #13#10'   Alertable            : 0x%.08X' +
        #13#10'   Timeout              : 0x%.08X (%d)' +
        #13#10');',
        [Count, Handles, WaitType, WaitMode, Alertable,
         Timeout, Timeout == 0 ? 0 : Timeout.QuadPart]);
{$ENDIF}

    NTSTATUS ret = NtDll::NtWaitForMultipleObjects(Count, Handles, (NtDll::OBJECT_WAIT_TYPE)WaitType, Alertable, (NtDll::PLARGE_INTEGER)Timeout);

    return ret;
*)

  Result := Unimplemented('NtWaitForMultipleObjectsEx');
  EmuSwapFS(fsXbox);
end;

// NtWriteFile:
// Writes a file.
//
// Differences from NT: There is no Key parameter.
function xboxkrnl_NtWriteFile(
  FileHandle: HANDLE; // Cxbx TODO: correct paramters
  Event: DWORD; // Dxbx correction (was PVOID)
  ApcRoutine: PVOID;
  ApcContext: PVOID;
  IoStatusBlock: PVOID; // OUT
  Buffer: PVOID;
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : NtWriteFile' +
       #13#10'(' +
       #13#10'   FileHandle          : 0x%.08X' +
       #13#10'   Event               : 0x%.08X' +
       #13#10'   ApcRoutine          : 0x%.08X' +
       #13#10'   ApcContext          : 0x%.08X' +
       #13#10'   IoStatusBlock       : 0x%.08X' +
       #13#10'   Buffer              : 0x%.08X' +
       #13#10'   Length              : 0x%.08X' +
       #13#10'   ByteOffset          : 0x%.08X' + {' (0x%.08X)' +}
       #13#10');',
       [FileHandle, Event, ApcRoutine,
       ApcContext, IoStatusBlock, Buffer, Length, ByteOffset{, iif(ByteOffset = nil, 0, ByteOffset.QuadPart)}]);
{$ENDIF}

  // Halo..
  //    if Assigned(ByteOffset) and (ByteOffset.QuadPart = $01C00800) then
  //        _asm int 3

  Result := JwaNative.NtWriteFile(FileHandle, Event, ApcRoutine, ApcContext, IoStatusBlock, Buffer, Length, JwaWinType.PLARGE_INTEGER(ByteOffset), nil);

  if (FAILED(Result)) then
    EmuWarning('NtWriteFile Failed! (0x%.08X)', [Result]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtWriteFileGather(): NTSTATUS; stdcall;
// Branch:Dxbx
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtWriteFileGather');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_NtYieldExecution(); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  // Cxbx NOTE: this eats up the debug log far too quickly
  // DbgPrintf('EmuKrnl : NtYieldExecution();');
{$ENDIF}

  NtYieldExecution();

  EmuSwapFS(fsXbox);
end;

end.
