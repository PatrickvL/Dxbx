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
  ThreadHandle: PHANDLE; // OUT
  ThreadId: PULONG; // OUT, OPTIONAL
  StartAddress: PKSTART_ROUTINE; // thread function
  StartContext: PVOID;
  DebugStack: _BOOLEAN
): NTSTATUS; stdcall;
function {255} xboxkrnl_PsCreateSystemThreadEx(
  pThreadHandle: PHANDLE; // OUT
  ThreadExtraSize: ULONG; // XBMC Says : ObjectAttributes: PVOID; // OPTIONAL
  KernelStackSize: ULONG;
  TlsDataSize: ULONG;
  pThreadId: PULONG; // OUT, OPTIONAL
  StartAddress: PKSTART_ROUTINE; // thread function
  StartContext: PVOID;
  CreateSuspended: _BOOLEAN;
  DebugStack: _BOOLEAN;
  pStartRoutine: PKSYSTEM_ROUTINE
): NTSTATUS; stdcall;
function {256} xboxkrnl_PsQueryStatistics(
  ProcessStatistics: PPS_STATISTICS
  ): NTSTATUS; stdcall;
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
type PCSTProxyParam = record
    StartAddress: PKSTART_ROUTINE;
    StartContext: PVOID;
    StartRoutine: PKSYSTEM_ROUTINE;
    StartSuspended: BOOL_;
    hStartedEvent: HANDLE;
  end; // size = 20
  PPCSTProxyParam = ^PCSTProxyParam;

// PsCreateSystemThread(Ex) proxy procedure
//pragma warning(push)
//pragma warning(disable: 4731)  // disable ebp modification warning

// Dxbx Note : The signature of this function should conform to System.TThreadFunc !
function PCSTProxy
(
    Parameter: PPCSTProxyParam
): Integer; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
label
  callComplete;
var
  StartAddress: PKSTART_ROUTINE;
  StartContext: PVOID;
  StartRoutine: PKSYSTEM_ROUTINE;
  StartSuspended: BOOL_;
  i: int;
  pfnNotificationRoutine: XTHREAD_NOTIFY_PROC;
  OldExceptionFilter: LPTOP_LEVEL_EXCEPTION_FILTER;
begin
  StartAddress := Parameter.StartAddress;
  StartContext := Parameter.StartContext;
  StartRoutine := Parameter.StartRoutine;
  StartSuspended := Parameter.StartSuspended;

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : PCSTProxy' +
    #13#10'(' +
    #13#10'   StartAddress        : 0x%.08x' +
    #13#10'   StartContext        : 0x%.08x' +
    #13#10'   StartRoutine        : 0x%.08x' +
    #13#10');',
    [Addr(StartAddress), StartContext, Addr(StartRoutine)]);
{$ENDIF}

  if(StartSuspended = TRUE) then
    SuspendThread(GetCurrentThread());

  EmuGenerateFS(DxbxKrnl_TLS, DxbxKrnl_TLSData);

  // call thread notification routine(s)
  if (g_iThreadNotificationCount <> 0) then
  begin
    for i := 0 to 16-1 do
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

    StartRoutine(StartAddress, StartContext);
(*
    // use the special calling convention
    asm
      mov         esi, StartRoutine
      push        StartContext
      push        StartAddress

      push        offset callComplete
      lea         ebp, [esp-4]

      jmp         esi

      // Note : This jmp reads like this in Cxbx (which doesn't compile) :
      // jmp near esi
    end;
*)
callComplete:

{$IFNDEF DISABLE_THREAD_EXCEPTION_HANDLING}
  except
    on E: Exception do
    begin
      EmuSwapFS(fsWindows);
      // TODO -oDXBX: How do we intercept ntdll.ZwRaiseException here ?
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

   EmuSwapFS(fsWindows);

{$IFNDEF DISABLE_THREAD_EXCEPTION_HANDLING}
  // Restore original exception filter :
  SetUnhandledExceptionFilter(OldExceptionFilter);
{$ENDIF}

  // call thread notification routine(s)
  if (g_iThreadNotificationCount <> 0) then
  begin
    for i := 0 to 16-1 do
    begin
      pfnNotificationRoutine := {XTL.}XTHREAD_NOTIFY_PROC(g_pfnThreadNotification[i]);

      // If the routine doesn't exist, don't execute it!
      if not Assigned(pfnNotificationRoutine) then
        continue;

      DbgPrintf('EmKrnl : Calling pfnNotificationRoutine[%d] (0x%.08X)',
        [g_iThreadNotificationCount, Addr(pfnNotificationRoutine)]);

      EmuSwapFS(fsXbox);

      pfnNotificationRoutine({Create=}BOOL_FALSE);
 
      EmuSwapFS(fsWindows);
    end;
  end;

  DxbxKrnlTerminateThread();

  Result := 0;
end; // PCSTProxy
//pragma warning(pop)

////

procedure PspSystemThreadStartup(
  StartRoutine: PKSTART_ROUTINE;
  StartContext: PVOID
); stdcall;
begin
  try
    StartRoutine(StartContext);
  except
    // (PspUnhandledExceptionInSystemThread(GetExceptionInformation()))
  end;

  xboxkrnl_PsTerminateSystemThread(STATUS_SUCCESS);
end;

const KERNEL_STACK_SIZE = $3000;

// PsCreateSystemThread:
// Creates a system thread.  Same as:
// PsCreateSystemThreadEx(ThreadHandle, NULL, 0x3000, 0, ThreadId, StartContext1,
//     StartContext2, FALSE, DebugStack, PspSystemThreadStartup);
//
// New to the XBOX.  (It is too different from NT to be considered the same)
function {254} xboxkrnl_PsCreateSystemThread(
  ThreadHandle: PHANDLE; // OUT
  ThreadId: PULONG; // OUT, OPTIONAL
  StartAddress: PKSTART_ROUTINE; // thread function
  StartContext: PVOID;
  DebugStack: _BOOLEAN
): NTSTATUS; stdcall;
// Source:XBMC  Branch:Dxbx  Translator:PatrickvL  Done:100
var
  ThreadExtraSize: ULONG;
  KernelStackSize: ULONG;
  TlsDataSize: ULONG;
  CreateSuspended: _BOOLEAN;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : PsCreateSystemThread >>' +
    #13#10'(' +
    #13#10'   ThreadHandle        : 0x%.08x' +
    #13#10'   ThreadId            : 0x%.08x' +
    #13#10'   StartAddress        : 0x%.08x' +
    #13#10'   StartContext        : 0x%.08x' +
    #13#10'   DebugStack          : 0x%.08x' +
    #13#10');',
    [ThreadHandle, ThreadId, Addr(StartAddress), StartContext,
    Ord(DebugStack)]);
{$ENDIF}

  ThreadExtraSize := 0;
  KernelStackSize := KERNEL_STACK_SIZE;
  TlsDataSize := 0;
  CreateSuspended := False;

  // Pass-through to Ex-implementation :
  Result := xboxkrnl_PsCreateSystemThreadEx(
    ThreadHandle,
    ThreadExtraSize,
    KernelStackSize,
    TlsDataSize,
    ThreadId,
    Addr(StartAddress),
    StartContext,
    CreateSuspended,
    DebugStack,
    Addr(PspSystemThreadStartup)
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
  pThreadHandle: PHANDLE; // OUT
  ThreadExtraSize: ULONG; // XBMC Says : ObjectAttributes: PVOID; // OPTIONAL
  KernelStackSize: ULONG;
  TlsDataSize: ULONG;
  pThreadId: PULONG; // OUT, OPTIONAL
  StartAddress: PKSTART_ROUTINE; // thread function
  StartContext: PVOID;
  CreateSuspended: _BOOLEAN;
  DebugStack: _BOOLEAN;
  pStartRoutine: PKSYSTEM_ROUTINE
): NTSTATUS; stdcall;
// Source:Cxbx/XBMC  Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
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
    #13#10'   StartAddress        : 0x%.08x' +
    #13#10'   StartContext        : 0x%.08x' +
    #13#10'   CreateSuspended     : 0x%.08x' +
    #13#10'   DebugStack          : 0x%.08x' +
    #13#10'   StartRoutine        : 0x%.08x' +
    #13#10');',
    [pThreadHandle, ThreadExtraSize, KernelStackSize, TlsDataSize, pThreadId,
    Addr(StartAddress), StartContext, CreateSuspended, DebugStack, Addr(pStartRoutine)]);
{$ENDIF}

  // create thread, using our special proxy technique
  begin
    // PCSTProxy is responsible for cleaning up this pointer
    iPCSTProxyParam.StartAddress := Addr(StartAddress);
    iPCSTProxyParam.StartContext := StartContext;
    iPCSTProxyParam.StartRoutine := Addr(pStartRoutine);
    iPCSTProxyParam.StartSuspended := CreateSuspended;
    iPCSTProxyParam.hStartedEvent := CreateEvent(NULL, FALSE, FALSE, NULL);

    pThreadHandle^ := CreateThread(NULL, 0, @PCSTProxy, @iPCSTProxyParam, 0, {var}@dwThreadId);

    // Make sure the Xbox1 code runs on one core :
    SetThreadAffinityMask(dwThreadId, g_XboxCPU);

    WaitForSingleObject(iPCSTProxyParam.hStartedEvent, 1000);

{$IFDEF DEBUG}
    DbgPrintf('EmuKrnl : ThreadHandle : 0x%.04x, ThreadId : 0x%.04x', [pThreadHandle^, dwThreadId]);
{$ENDIF}

    // we must duplicate this handle in order to retain Suspend/Resume thread rights from a remote thread
    begin
      hDupHandle := 0;

      if not DuplicateHandle(GetCurrentProcess(), pThreadHandle^, GetCurrentProcess(), @hDupHandle, 0, False, DUPLICATE_SAME_ACCESS) then
      begin
{$IFDEF DEBUG}
        DbgPrintf('EmuKrnl : PsCreateSystemThreadEx - Couldn''t duplicate handle!');
{$ENDIF}
      end;

      DxbxKrnlRegisterThread(hDupHandle);
    end;

    if(pThreadId <> NULL) then
      pThreadId^ := dwThreadId;
  end;

  EmuSwapFS(fsXbox);

  Result := STATUS_SUCCESS;
end;

function {256} xboxkrnl_PsQueryStatistics(
  ProcessStatistics: PPS_STATISTICS
  ): NTSTATUS; stdcall;
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
procedure {258} xboxkrnl_PsTerminateSystemThread(
  ExitStatus: NTSTATUS
  ); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
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
    for i := 0 to 16-1 do
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

  //DxbxKrnlTerminateThread();
  EmuCleanupFS();

  //_endthreadex(ExitStatus);
  ExitThread(ExitStatus);

end;

end.
