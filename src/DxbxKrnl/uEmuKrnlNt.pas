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
  uEmuKrnl,
  uDxbxKrnl;

function xboxkrnl_NtAllocateVirtualMemory(
  BaseAddress: PVOID; // OUT * ?
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
  Handle: THandle
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
function xboxkrnl_NtCreateSemaphore(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
function xboxkrnl_NtCreateTimer(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
function xboxkrnl_NtDeleteFile(pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
function xboxkrnl_NtDeviceIoControlFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; pIoControlCode: dtU32; pInputBuffer: dtU32; InputBufferLength: dtU32; pOutputBuffer: dtU32; OutputBufferLength: dtU32): NTSTATUS; stdcall;
function xboxkrnl_NtDuplicateObject(
  SourceHandle: HANDLE;
  TargetHandle: PHANDLE;
  Options: DWORD
  ): NTSTATUS; stdcall;
function xboxkrnl_NtFlushBuffersFile(
  FileHandle: PVOID;
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
function xboxkrnl_NtQueueApcThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtQueryDirectoryFile(
  FileHandle: HANDLE;
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PVOID; // Todo: define this routine's prototype
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
function xboxkrnl_NtQueryVirtualMemory(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtQueryVolumeInformationFile(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  FileInformation: PFILE_FS_SIZE_INFORMATION; // OUT
  Length: ULONG;
  FileInformationClass: FS_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
function xboxkrnl_NtReadFile(
  FileHandle: HANDLE; // TODO: correct paramters
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
  PreviousCount: PLONG // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtReleaseSemaphore(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtRemoveIoCompletion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_NtResumeThread(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSetEvent(
  EventHandle: HANDLE;
  PreviousState: PLONG // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_NtSetInformationFile(
  FileHandle: HANDLE; // TODO: correct paramters
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
  _Handle: HANDLE;
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
  FileHandle: HANDLE; // TODO: correct paramters
  Event: PVOID;
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

function xboxkrnl_NtAllocateVirtualMemory(
  BaseAddress: PVOID; // OUT * ?
  ZeroBits: ULONG;
  AllocationSize: PULONG; // OUT * ?
  AllocationType: DWORD;
  Protect: DWORD
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtAllocateVirtualMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCancelTimer(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtCancelTimer');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtClearEvent(
  EventHandle: HANDLE
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtClearEvent');
  EmuSwapFS(fsXbox);
end;

// 0x00BB - NtClose

function xboxkrnl_NtClose(
  Handle: THandle
  ): NTSTATUS; stdcall; {XBSYSAPI EXPORTNUM(187)}
// Branch:martin  Revision:39  Translator:PatrickvL  Done:80
{$IFDEF DXBX_EMUHANDLES}
var
  iEmuHandle: TEmuHandle;
{$ENDIF}
begin
  EmuSwapFS(fsWindows);

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
    Result := NtClose(Handle);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateDirectoryObject(): NTSTATUS; stdcall;
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
  Result := Unimplemented('NtCreateEvent');
  EmuSwapFS(fsXbox);
end;

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
// Branch:martin  Revision:39  Translator:PatrickvL  Done:5
begin
  EmuSwapFS(fsWindows);
(*
    DbgPrintf('EmuKrnl (0x%X): NtCreateFile' +
           #13#10'(' +
           #13#10'   FileHandle          : 0x%.08X' +
           #13#10'   DesiredAccess       : 0x%.08X' +
           #13#10'   ObjectAttributes    : 0x%.08X (''%s'')' +
           #13#10'   IoStatusBlock       : 0x%.08X' +
           #13#10'   AllocationSize      : 0x%.08X' +
           #13#10'   FileAttributes      : 0x%.08X' +
           #13#10'   ShareAccess         : 0x%.08X' +
           #13#10'   CreateDisposition   : 0x%.08X' +
           #13#10'   CreateOptions       : 0x%.08X' +
           #13#10');',
           [FileHandle, DesiredAccess, ObjectAttributes, ObjectAttributes.ObjectName.Buffer,
           IoStatusBlock, AllocationSize, FileAttributes, ShareAccess, CreateDisposition, CreateOptions]);

    char ReplaceChar  := '\0';
    int  ReplaceIndex := -1;

    char *szBuffer := ObjectAttributes.ObjectName.Buffer;

    if Assigned(szBuffer) then
    begin
        //printf('Orig : %s', szBuffer);

        // trim this off
        if(szBuffer[0] = '''' and szBuffer[1] = '?' and szBuffer[2] = '?' and szBuffer[3] = '''')
            Inc(szBuffer, 4);
        end;

        // D:\ should map to current directory
	    if( (szBuffer[0] = 'D'  or  szBuffer[0] = 'd') and szBuffer[1] = ':' and szBuffer[2] = '''')
	    begin
		    Inc(szBuffer, 3);

		    ObjectAttributes.RootDirectory := g_hCurDir;

		    DbgPrintf('EmuKrnl (0x%X): NtCreateFile Corrected path...', GetCurrentThreadId());
		    DbgPrintf('  Org:''%s''', ObjectAttributes.ObjectName.Buffer);
		    DbgPrintf('  New:''$XbePath\\%s''', szBuffer);
	    end;
	    else if( (szBuffer[0] = 'T'  or  szBuffer[0] = 't') and szBuffer[1] = ':' and szBuffer[2] = '''')
	    begin
		    szBuffer += 3;

		    ObjectAttributes.RootDirectory := g_hTDrive;

		    DbgPrintf('EmuKrnl (0x%X): NtCreateFile Corrected path...', GetCurrentThreadId());
		    DbgPrintf('  Org:''%s''', ObjectAttributes.ObjectName.Buffer);
		    DbgPrintf('  New:''$CxbxPath\\EmuDisk\\T\\%s''', szBuffer);
	    end;
	    else if( (szBuffer[0] = 'U'  or  szBuffer[0] = 'u') and szBuffer[1] = ':' and szBuffer[2] = '''')
	    begin
		    Inc(szBuffer, 3);

		    ObjectAttributes.RootDirectory := g_hUDrive;

		    DbgPrintf('EmuKrnl (0x%X): NtCreateFile Corrected path...', GetCurrentThreadId());
		    DbgPrintf('  Org:''%s''', ObjectAttributes.ObjectName.Buffer);
		    DbgPrintf('  New:''$CxbxPath\\EmuDisk\\U\\%s''', szBuffer);
	    end;
	    else if( (szBuffer[0] = 'Z'  or  szBuffer[0] = 'z') and szBuffer[1] = ':' and szBuffer[2] = '''')
	    begin
		    Inc(szBuffer, 3);

		    ObjectAttributes.RootDirectory := g_hZDrive;

		    DbgPrintf('EmuKrnl (0x%X): NtCreateFile Corrected path...', GetCurrentThreadId());
		    DbgPrintf('  Org:''%s''', ObjectAttributes.ObjectName.Buffer);
		    DbgPrintf('  New:''$CxbxPath\\EmuDisk\\Z\\%s''', szBuffer);
	    end;

        //
        // TODO: Wildcards are not allowed??
        //

        begin
            for(int v=0;szBuffer[v] <> '\0';v++)
            begin
                if (szBuffer[v] = '*') then
                begin
                    if(v > 0) begin ReplaceIndex := v-1; end;
                    else begin ReplaceIndex := v; end;
                end;
            end;
        end;

        // Note: Hack: Not thread safe (if problems occur, create a temp buffer)
        if(ReplaceIndex <> -1)
        begin
            ReplaceChar := szBuffer[ReplaceIndex];
            szBuffer[ReplaceIndex] := #0;
        end;

        //printf('Aftr : %s', szBuffer);
    end;

    wchar_t wszObjectName[160];

    NtDll::UNICODE_STRING    NtUnicodeString;
    NtDll::OBJECT_ATTRIBUTES NtObjAttr;

    // initialize object attributes
    if Assigned(szBuffer) then
      mbstowcs(wszObjectName, szBuffer, 160)
    else
      wszObjectName[0] := WideChar(#0);

    NtDll::RtlInitUnicodeString(@NtUnicodeString, wszObjectName);

    InitializeObjectAttributes(@NtObjAttr, @NtUnicodeString, ObjectAttributes.Attributes, ObjectAttributes.RootDirectory, NULL);

    // redirect to NtCreateFile
    NTSTATUS ret := NtDll::NtCreateFile
    (
        FileHandle, DesiredAccess, @NtObjAttr, (NtDll::IO_STATUS_BLOCK* )IoStatusBlock,
        (NtDll::LARGE_INTEGER* )AllocationSize, FileAttributes, ShareAccess, CreateDisposition, CreateOptions, NULL, NULL
    );

    if FAILED(ret) then
      DbgPrintf('EmuKrnl : NtCreateFile Failed! (0x%.08X)', [ret])
    else
      DbgPrintf('EmuKrnl : NtCreateFile := 0x%.08X', [FileHandle^]);

    // restore original buffer
    if (ReplaceIndex <> -1) then
      szBuffer[ReplaceIndex] := ReplaceChar;

    // NOTE: We can map this to IoCreateFile once implemented (if ever necessary)
    //       xboxkrnl::IoCreateFile(FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, AllocationSize, FileAttributes, ShareAccess, CreateDisposition, CreateOptions, 0);
*)
  Result := Unimplemented('NtCreateFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateIoCompletion(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
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
  Result := Unimplemented('NtCreateMutant');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateSemaphore(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtCreateSemaphore');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtCreateTimer(FileHandle: dtU32; DesiredAccess: dtACCESS_MASK; pObjectAttributes: dtObjectAttributes; pszUnknownArgs: dtBLOB): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtCreateTimer');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtDeleteFile(pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtDeleteFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtDeviceIoControlFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; pIoControlCode: dtU32; pInputBuffer: dtU32; InputBufferLength: dtU32; pOutputBuffer: dtU32; OutputBufferLength: dtU32): NTSTATUS; stdcall;
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
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtDuplicateObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtFlushBuffersFile(
  FileHandle: PVOID;
  IoStatusBlock: PIO_STATUS_BLOCK // OUT
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtFlushBuffersFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtFreeVirtualMemory(
  BaseAddress: PPVOID; // OUT
  FreeSize: PULONG; // OUT
  FreeType: ULONG
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtFreeVirtualMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtFsControlFile(FileHandle: dtU32; Event: dtU32; pApcRoutine: dtU32; pApcContext: dtU32; pIoStatusBlock: dtU32; FsControlCode: dtU32; pInputBuffer: dtU32; InputBufferLength: dtU32; pOutputBuffer: dtU32; OutputBufferLength: dtU32): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtFsControlFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtOpenDirectoryObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtOpenDirectoryObject');
  EmuSwapFS(fsXbox);
end;

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
(*
  EmuSwapFS(fsWindows);
    // debug trace
        DbgPrintf("EmuKrnl (0x%X): NtOpenFile\n"
               "(\n"
               "   FileHandle          : 0x%.08X\n"
               "   DesiredAccess       : 0x%.08X\n"
               "   ObjectAttributes    : 0x%.08X (\"%s\")\n"
               "   IoStatusBlock       : 0x%.08X\n"
               "   ShareAccess         : 0x%.08X\n"
               "   CreateOptions       : 0x%.08X\n"
               ");\n",
               GetCurrentThreadId(), FileHandle, DesiredAccess, ObjectAttributes, ObjectAttributes->ObjectName->Buffer,
               IoStatusBlock, ShareAccess, OpenOptions);
  EmuSwapFS(fsXbox);
*)
  Result := xboxkrnl_NtCreateFile(FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, NULL, 0, ShareAccess, FILE_OPEN, OpenOptions);
end;

function xboxkrnl_NtOpenSymbolicLinkObject(pFileHandle: dtU32; pObjectAttributes: dtObjectAttributes): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtOpenSymbolicLinkObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtProtectVirtualMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtProtectVirtualMemory');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtPulseEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtPulseEvent');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueueApcThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueueApcThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryDirectoryFile(
  FileHandle: HANDLE;
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PVOID; // Todo: define this routine's prototype
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
  Result := Unimplemented('NtQueryDirectoryFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryDirectoryObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryDirectoryObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryEvent');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryFullAttributesFile(
  ObjectAttributes: POBJECT_ATTRIBUTES;
  Attributes: PVOID // OUT
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryFullAttributesFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryInformationFile(
  FileHandle: HANDLE;
  IoStatusBlock: PIO_STATUS_BLOCK; //   OUT
  FileInformation: PVOID; //   OUT
  Length: ULONG;
  FileInfo: FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryInformationFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryIoCompletion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryMutant(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryMutant');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQuerySemaphore(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQuerySemaphore');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQuerySymbolicLinkObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQuerySymbolicLinkObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryTimer(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryTimer');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtQueryVirtualMemory(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtQueryVirtualMemory');
  EmuSwapFS(fsXbox);
end;

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
  Result := Unimplemented('NtQueryVolumeInformationFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtReadFile(
  FileHandle: HANDLE; // TODO: correct paramters
  Event: HANDLE; // OPTIONAL
  ApcRoutine: PVOID; // OPTIONAL
  ApcContext: PVOID;
  IoStatusBlock: PVOID; // OUT
  Buffer: PVOID; // OUT
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER // OPTIONAL
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtReadFile');
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
  PreviousCount: PLONG // OUT
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtReleaseMutant');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtReleaseSemaphore(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtReleaseSemaphore');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtRemoveIoCompletion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtRemoveIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtResumeThread(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG // OUT
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtResumeThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetEvent(
  EventHandle: HANDLE;
  PreviousState: PLONG // OUT
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSetEvent');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetInformationFile(
  FileHandle: HANDLE; // TODO: correct paramters
  IoStatusBlock: PVOID; // OUT
  FileInformation: PVOID;
  Length: ULONG;
  FileInformationClass: FILE_INFORMATION_CLASS
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSetInformationFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetIoCompletion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSetIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetSystemTime(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSetSystemTime');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSetTimerEx(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSetTimerEx');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSignalAndWaitForSingleObjectEx(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSignalAndWaitForSingleObjectEx');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtSuspendThread(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG // OUT OPTIONAL
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtSuspendThread');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_NtUserIoApcDispatcher(
  ApcContext: PVOID;
  IoStatusBlock: PIO_STATUS_BLOCK;
  Reserved: ULONG
  ); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('NtUserIoApcDispatcher');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtWaitForSingleObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtWaitForSingleObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtWaitForSingleObjectEx(
  _Handle: HANDLE;
  WaitMode: CHAR;
  Alertable: LONGBOOL;
  Timeout: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
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
  Result := Unimplemented('NtWaitForMultipleObjectsEx');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtWriteFile(
  FileHandle: HANDLE; // TODO: correct paramters
  Event: PVOID;
  ApcRoutine: PVOID;
  ApcContext: PVOID;
  IoStatusBlock: PVOID; // OUT
  Buffer: PVOID;
  Length: ULONG;
  ByteOffset: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtWriteFile');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_NtWriteFileGather(): NTSTATUS; stdcall;
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('NtWriteFileGather');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_NtYieldExecution(); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  // NOTE: this eats up the debug log far too quickly
  //DbgPrintf('EmuKrnl : NtYieldExecution();');

  NtYieldExecution();

  EmuSwapFS(fsXbox);
end;

end.
