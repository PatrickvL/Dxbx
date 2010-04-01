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
unit uEmuKrnlPs;

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
  uTypes,
  uLog,
  uEmuFS,
  uEmuFile,
  uEmuXapi,
  uEmuKrnl,
  uDxbxKrnl,
  uDxbxKrnlUtils,
  uEmu,
  uDxbxDebugUtils;

var {259}xboxkrnl_PsThreadObjectType: POBJECT_TYPE;
// Source: OpenXDK - Uncertain  Branch:Dxbx  Translator:PatrickvL  Done:0

function {254} xboxkrnl_PsCreateSystemThread(
(* XBMC says :
 ThreadHandle: PHANDLE; // OUT
 ThreadId: PULONG; // OUT, OPTIONAL
 StartContext1: PVOID;
 StartContext2: PVOID;
 DebugStack: LONGBOOL
*)
  lpThreadAttributes: PULONG; // SD
  dwStackSize: DWORD; // initial stack size
  lpStartAddress: PKSTART_ROUTINE; // thread function
  lpParameter: PVOID; // thread argument
  dwCreationFlags: DWORD; // creation option
  lpThreadId: PULONG // thread identifier
  ): NTSTATUS; stdcall;
function {255} xboxkrnl_PsCreateSystemThreadEx(
  ThreadHandle: PHANDLE; // out
  ThreadExtraSize: ULONG; // XBMC Says : ObjectAttributes: PVOID; // OPTIONAL
  KernelStackSize: ULONG;
  TlsDataSize: ULONG;
  ThreadId: PULONG; // out, optional
  StartContext1: PVOID;
  StartContext2: PVOID;
  CreateSuspended: BOOLEAN;
  DebugStack: BOOLEAN;
  StartRoutine: PKSTART_ROUTINE
  ): NTSTATUS; stdcall;
function {256} xboxkrnl_PsQueryStatistics(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {257} xboxkrnl_PsSetCreateThreadNotifyRoutine(
  NotifyRoutine: PCREATE_THREAD_NOTIFY_ROUTINE
  ): NTSTATUS; stdcall;
procedure {258} xboxkrnl_PsTerminateSystemThread(
  ExitStatus: NTSTATUS
  ); stdcall;

// Global Variable(s)
var g_pfnThreadNotification: array [0..16-1] of XTHREAD_NOTIFY_PROC; //= { NULL };
var g_iThreadNotificationCount: int = 0;

implementation

// PsCreateSystemThread proxy parameters
type PCSTProxyParam = packed record
    StartContext1: PVOID;
    StartContext2: PVOID;
    StartRoutine: PKSTART_ROUTINE;
    StartSuspended: BOOL_;
    hStartedEvent: HANDLE;
  end;
  PPCSTProxyParam = ^PCSTProxyParam;

// PsCreateSystemThread(Ex) proxy procedure
//pragma warning(push)
//pragma warning(disable: 4731)  // disable ebp modification warning

// Note : The signature of this function should conform to System.TThreadFunc !
function PCSTProxy(Parameter: PPCSTProxyParam): Integer;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
label
  callComplete;
var
  StartContext1: PVOID;
  StartContext2: PVOID;
  StartRoutine: PKSTART_ROUTINE;
  StartSuspended: BOOL_;
  i: int;
  pfnNotificationRoutine: XTHREAD_NOTIFY_PROC;
  OldExceptionFilter: LPTOP_LEVEL_EXCEPTION_FILTER;
begin
  StartContext1 := Parameter.StartContext1;
  StartContext2 := Parameter.StartContext2;
  StartRoutine := Parameter.StartRoutine;
  StartSuspended := Parameter.StartSuspended;

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : PCSTProxy' +
    #13#10'(' +
    #13#10'   StartContext1       : 0x%.08x' +
    #13#10'   StartContext2       : 0x%.08x' +
    #13#10'   StartRoutine        : 0x%.08x' +
    #13#10');',
    [StartContext1, StartContext2, Addr(StartRoutine)]);
{$ENDIF}

  if(StartSuspended = TRUE) then
    SuspendThread(GetCurrentThread());

  EmuGenerateFS(CxbxKrnl_TLS, CxbxKrnl_TLSData);

  // call thread notification routine(s)
  if (g_iThreadNotificationCount <> 0) then
  begin
    for i := 0 to 16 - 1 do
    begin
      pfnNotificationRoutine := {XTL.}XTHREAD_NOTIFY_PROC(g_pfnThreadNotification[i]);

      // If the routine doesn't exist, don't execute it!
      if not Assigned(pfnNotificationRoutine) then
        continue;

      DbgPrintf('EmKrnl : Calling pfnNotificationRoutine[%d] (0x%.08X)', [g_iThreadNotificationCount, Addr(pfnNotificationRoutine)]);

      EmuSwapFS(fsXbox);

      pfnNotificationRoutine({Create=}BOOL_TRUE);

      EmuSwapFS(fsWindows);
    end;
  end;

{$IFNDEF DISABLE_THREAD_EXCEPTION_HANDLING}
  // Re-route unhandled exceptions to our emulation-execption handler :
  OldExceptionFilter := SetUnhandledExceptionFilter(LPTOP_LEVEL_EXCEPTION_FILTER(@EmuException));

  try
{$ENDIF}
    SetEvent(Parameter.hStartedEvent);

    EmuSwapFS(fsXbox);

//    StartRoutine(StartContext1, StartContext2);

    // use the special calling convention
    asm
      mov         esi, StartRoutine
      push        StartContext2
      push        StartContext1

      push        offset callComplete
      lea         ebp, [esp-4]

      jmp         esi

      // Note : This jmp reads like this in Cxbx (which doesn't compile) :
      // jmp near esi
    end;

{$IFNDEF DISABLE_THREAD_EXCEPTION_HANDLING}
  except
    on E: Exception do
    begin
      // TODO : How do we intercept ntdll.ZwRaiseException here ?
      //      EmuException(E);
      DbgPrintf('EmuKrnl : PCSTProxy : Catched an exception : ' + E.Message);
{$IFDEF DXBX_USE_JCLDEBUG}
      DbgPrintf(JclLastExceptStackListToString(False));
{$ENDIF}
    (*__except(EmuException(GetExceptionInformation())); *)
    //  EmuWarning('Problem with ExceptionFilter!');
    end;
  end; // try
{$ENDIF}

callComplete:
   EmuSwapFS(fsWindows);

{$IFNDEF DISABLE_THREAD_EXCEPTION_HANDLING}
  // Restore original exception filter :
  SetUnhandledExceptionFilter(OldExceptionFilter);
{$ENDIF}

  // call thread notification routine(s)
  if (g_iThreadNotificationCount <> 0) then
  begin
    for i := 0 to 16 - 1 do
    begin
      pfnNotificationRoutine := {XTL.}XTHREAD_NOTIFY_PROC(g_pfnThreadNotification[i]);

      // If the routine doesn't exist, don't execute it!
      if not Assigned(pfnNotificationRoutine) then
        continue;

      DbgPrintf('EmKrnl : Calling pfnNotificationRoutine[%d] (0x%.08X)', [g_iThreadNotificationCount, Addr(pfnNotificationRoutine)]);

      EmuSwapFS(fsXbox);

      pfnNotificationRoutine({Create=}BOOL_FALSE);
 
      EmuSwapFS(fsWindows);
    end;
  end;

  CxbxKrnlTerminateThread();

  Result := 0;
end; // PCSTProxy
//pragma warning(pop)

////

// PsCreateSystemThread:
// Creates a system thread.  Same as:
// PsCreateSystemThreadEx(ThreadHandle, NULL, 0x3000, 0, ThreadId, StartContext1,
//     StartContext2, FALSE, DebugStack, PspSystemThreadStartup);
//
// New to the XBOX.  (It is too different from NT to be considered the same)
function {254} xboxkrnl_PsCreateSystemThread(
(* XBMC says :
 ThreadHandle: PHANDLE; // OUT
 ThreadId: PULONG; // OUT, OPTIONAL
 StartContext1: PVOID;
 StartContext2: PVOID;
 DebugStack: LONGBOOL
*)
  lpThreadAttributes: PULONG; // SD
  dwStackSize: DWORD; // initial stack size
  lpStartAddress: PKSTART_ROUTINE; // thread function
  lpParameter: PVOID; // thread argument
  dwCreationFlags: DWORD; // creation option
  lpThreadId: PULONG // thread identifier
  ): NTSTATUS; stdcall;
// Source:Cxbx  Branch:Dxbx  Translator:PatrickvL  Done:25
// TODO : Should we use XBMC's version?
var
  ThreadHandle: HANDLE;
  ThreadExtraSize: ULONG;
  KernelStackSize: ULONG;
  TlsDataSize: ULONG;
  StartContext1: PVOID;
  StartContext2: PVOID;
  CreateSuspended: LONGBOOL;
  DebugStack: LONGBOOL;
begin
  EmuSwapFS(fsWindows);

  // Dxbx TODO : How to apply the local arguments like lpThreadAttributes ?
  ThreadHandle := 0;
  ThreadExtraSize := dwStackSize; // ??
  KernelStackSize := dwStackSize; // ??
  TlsDataSize := 0; // ??
  StartContext1 := lpParameter; // ??
  StartContext2 := nil; // ??
  CreateSuspended := (dwCreationFlags and CREATE_SUSPENDED) > 0;
  DebugStack := False; // ??

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : PsCreateSystemThread' +
    #13#10'(' +
    #13#10'   lpThreadAttributes  : 0x%.08x' +
    #13#10'   dwStackSize         : 0x%.08x' +
    #13#10'   lpStartAddress      : 0x%.08x' +
    #13#10'   lpParameter         : 0x%.08x' +
    #13#10'   dwCreationFlags     : 0x%.08x' +
    #13#10'   ThreadId            : 0x%.08x' +
    #13#10');',
    [lpThreadAttributes^, dwStackSize, Addr(lpStartAddress), lpParameter,
    dwCreationFlags, Addr(lpThreadId)]);
{$ENDIF}

// PsCreateSystemThreadEx(ThreadHandle, NULL, 0x3000, 0, ThreadId, StartContext1,
//     StartContext2, FALSE, DebugStack, PspSystemThreadStartup);

  // Pass-through to Ex-implementation :
  Result := xboxkrnl_PsCreateSystemThreadEx(
    {dummy}@ThreadHandle,
    ThreadExtraSize,
    KernelStackSize,
    TlsDataSize,
    {ThreadId=}lpThreadId,
    StartContext1,
    StartContext2,
    CreateSuspended,
    DebugStack,
    {StartRoutine=}lpStartAddress
    );

  EmuSwapFS(fsXbox);
end;

// PsCreateSystemThreadEx:
// Creates a system thread.
// ThreadHandle: Receives the thread handle
// ObjectAttributes: Unsure how this works (everything I've seen uses NULL)
// KernelStackSize: Size of the allocation for both stack and TLS data
// TlsDataSize: Size within KernelStackSize to use as TLS data
// ThreadId: Receives the thread ID number
// StartContext1: Parameter 1 to StartRoutine
// StartContext2: Parameter 2 to StartRoutine
// CreateSuspended: TRUE to create the thread as a suspended thread
// DebugStack: TRUE to allocate the stack from Debug Kit memory
// StartRoutine: Called when the thread is created
//
// New to the XBOX.
function {255} xboxkrnl_PsCreateSystemThreadEx
(
  ThreadHandle: PHANDLE; // out
  ThreadExtraSize: ULONG; // XBMC Says : ObjectAttributes: PVOID; // OPTIONAL
  KernelStackSize: ULONG;
  TlsDataSize: ULONG;
  ThreadId: PULONG; // out, optional
  StartContext1: PVOID;
  StartContext2: PVOID;
  CreateSuspended: BOOLEAN;
  DebugStack: BOOLEAN;
  StartRoutine: PKSTART_ROUTINE
): NTSTATUS; stdcall;
// Source:Cxbx/XBMC  Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  dwThreadId: DWORD;
  hDupHandle: Handle;
  iPCSTProxyParam: PCSTProxyParam;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : PsCreateSystemThreadEx' +
    #13#10'(' +
    #13#10'   ThreadHandle        : 0x%.08x' +
    #13#10'   ThreadExtraSize     : 0x%.08x' +
    #13#10'   KernelStackSize     : 0x%.08x' +
    #13#10'   TlsDataSize         : 0x%.08x' +
    #13#10'   ThreadId            : 0x%.08x' +
    #13#10'   StartContext1       : 0x%.08x' +
    #13#10'   StartContext2       : 0x%.08x' +
    #13#10'   CreateSuspended     : 0x%.08x' +
    #13#10'   DebugStack          : 0x%.08x' +
    #13#10'   StartRoutine        : 0x%.08x' +
    #13#10');',
    [ThreadHandle, ThreadExtraSize, KernelStackSize, TlsDataSize, ThreadId,
    StartContext1, StartContext2, CreateSuspended, DebugStack, Addr(StartRoutine)]);
{$ENDIF}

  // create thread, using our special proxy technique
  begin
    // PCSTProxy is responsible for cleaning up this pointer
    iPCSTProxyParam.StartContext1 := StartContext1;
    iPCSTProxyParam.StartContext2 := StartContext2;
    iPCSTProxyParam.StartRoutine := StartRoutine;
    iPCSTProxyParam.StartSuspended := CreateSuspended;
    iPCSTProxyParam.hStartedEvent := CreateEvent(NULL, FALSE, FALSE, NULL);

    ThreadHandle^ := BeginThread(NULL, 0, @PCSTProxy, @iPCSTProxyParam, 0, {var}dwThreadId);

    WaitForSingleObject(iPCSTProxyParam.hStartedEvent, 1000);

{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : ThreadHandle : 0x%.04x, ThreadId : 0x%.04x', [ThreadHandle^, dwThreadId]);
{$ENDIF}

    // we must duplicate this handle in order to retain Suspend/Resume thread rights from a remote thread
    begin
      hDupHandle := 0;

      if not DuplicateHandle(GetCurrentProcess(), ThreadHandle^, GetCurrentProcess(), @hDupHandle, 0, False, DUPLICATE_SAME_ACCESS) then
      begin
{$IFDEF DEBUG}
        DbgPrintf('EmuKrnl : PsCreateSystemThreadEx - Couldn''t duplicate handle!');
{$ENDIF}
      end;

      CxbxKrnlRegisterThread(hDupHandle);
    end;

    if(ThreadId <> NULL) then
      {out}ThreadId^ := dwThreadId;
  end;

  EmuSwapFS(fsXbox);

  Result := STATUS_SUCCESS;
end;

function {256} xboxkrnl_PsQueryStatistics(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('PsQueryStatistics');
  EmuSwapFS(fsXbox);
end;

function {257} xboxkrnl_PsSetCreateThreadNotifyRoutine(
  NotifyRoutine: PCREATE_THREAD_NOTIFY_ROUTINE
  ): NTSTATUS; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('PsSetCreateThreadNotifyRoutine');
  EmuSwapFS(fsXbox);
end;

// PsTerminateSystemThread:
// Exits the current system thread.  Must be called from a system thread.
//
// Differences from NT: None.
procedure {258} xboxkrnl_PsTerminateSystemThread({IN}ExitStatus: NTSTATUS); stdcall;
// Source:XBMC  Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  i: int;
  pfnNotificationRoutine: XTHREAD_NOTIFY_PROC;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuKrnl: PsTerminateSystemThread'+
         '('+
         '   ExitStatus          : 0x%.08X'+
         ')',
         [ExitStatus]);

  // call thread notification routine(s)
  if (g_iThreadNotificationCount <> 0) then
  begin
    for i := 0 to 16 - 1 do
    begin
      pfnNotificationRoutine := {XTL.}XTHREAD_NOTIFY_PROC(g_pfnThreadNotification[i]);

      // If the routine doesn't exist, don't execute it!
      if not Assigned(pfnNotificationRoutine) then
        continue;

      DbgPrintf('EmKrnl : Calling pfnNotificationRoutine[%d] (0x%.08X)', [g_iThreadNotificationCount, Addr(pfnNotificationRoutine)]);

      EmuSwapFS(fsXbox);

      pfnNotificationRoutine({Create=}BOOL_FALSE);

      EmuSwapFS(fsWindows);
    end;
  end;

  //CxbxKrnlTerminateThread();
  EmuCleanupFS();

  //_endthreadex(ExitStatus);
  ExitThread(ExitStatus);

end;

//

//exports
//  xboxkrnl_PsCreateSystemThreadEx name 'PsCreateSystemThreadEx';

end.
