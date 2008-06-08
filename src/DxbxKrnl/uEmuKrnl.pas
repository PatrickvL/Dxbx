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
  // Dxbx
  uLog,
  uEmuFS,
  uEmuFile,
  uEmuXapi,
  uDxbxKrnl;

type
  PKSTART_ROUTINE = PTHREAD_START_ROUTINE; // ?correct?

  // PsCreateSystemThread proxy parameters
  PPCSTProxyParam = ^PCSTProxyParam;
  PCSTProxyParam = packed record
    StartContext1: PVOID;
    StartContext2: PVOID;
    StartRoutine: PKSTART_ROUTINE;
    StartSuspended: BOOL;
    hStartedEvent: HANDLE;
  end;

function xboxkrnl_NtClose(Handle: THandle): NTSTATUS; stdcall; {EXPORTNUM(187)}
function xboxkrnl_PsCreateSystemThreadEx(
  out ThreadHandle: THANDLE;
  ThreadExtraSize: ULONG;
  KernelStackSize: ULONG;
  TlsDataSize: ULONG;
  {out}ThreadId: PULONG {OPTIONAL};
  StartContext1: PVOID;
  StartContext2: PVOID;
  CreateSuspended: LONGBOOL;
  DebugStack: LONGBOOL;
  StartRoutine: PKSTART_ROUTINE): NTSTATUS; stdcall; {EXPORTNUM(255)}

implementation

// Global Variable(s)
var
  g_pfnThreadNotification: PVOID = nil;

// PsCreateSystemThread proxy procedure
//pragma warning(push)
//pragma warning(disable: 4731)  // disable ebp modification warning
function {WINAPI} PCSTProxy(Parameter: PPCSTProxyParam): Integer;//Word;
label
  callComplete;
var
  StartContext1: PVOID;
  StartContext2: PVOID;
  StartRoutine: PKSTART_ROUTINE;
  StartSuspended: BOOL;
  pfnNotificationRoutine: XTHREAD_NOTIFY_PROC;
begin
  StartContext1 := Parameter.StartContext1;
  StartContext2 := Parameter.StartContext2;
  StartRoutine  := Parameter.StartRoutine;
  StartSuspended := Parameter.StartSuspended;

  DbgPrintf('EmuKrnl : PCSTProxy' +
       #13'(' +
       #13'   StartContext1       : 0x' + IntToHex(Integer(StartContext1), 8) +
       #13'   StartContext2       : 0x' + IntToHex(Integer(StartContext2), 8) +
       #13'   StartRoutine        : 0x' + IntToHex(Integer(Addr(StartRoutine)), 8) +
       #13');');

  if StartSuspended then
    SuspendThread(GetCurrentThread());

  EmuGenerateFS(CxbxKrnl_TLS, CxbxKrnl_TLSData);

  // call thread notification routine(s)
  if Assigned(g_pfnThreadNotification) then
  begin
    pfnNotificationRoutine := {XTL.}XTHREAD_NOTIFY_PROC(g_pfnThreadNotification);

    DbgPrintf('EmKrnl : Calling pfnNotificationRoutine (0x' + IntToHex(Integer(Addr(pfnNotificationRoutine)), 8) + ')');

    EmuSwapFS();   // Xbox FS

    pfnNotificationRoutine(True);

    EmuSwapFS();   // Win2k/XP FS
  end;

  // use the special calling convention
  try
    SetEvent(Parameter.hStartedEvent);

    EmuSwapFS();   // Xbox FS

    asm
        mov         esi, StartRoutine
        push        StartContext2
        push        StartContext1
        push        offset callComplete
        lea         ebp, [esp-4]
//        jmp near    esi
        jmp         esi
     end;
  except
//  __except(EmuException(GetExceptionInformation()))
//    EmuWarning('Problem with ExceptionFilter not ');
  end;

callComplete:

  EmuSwapFS();    // Win2k/XP FS

  // call thread notification routine(s)
  if Assigned(g_pfnThreadNotification) then
  begin
    pfnNotificationRoutine := {XTL.}XTHREAD_NOTIFY_PROC(g_pfnThreadNotification);

    EmuSwapFS();   // Xbox FS

    pfnNotificationRoutine(False);

    EmuSwapFS();   // Win2k/XP FS
  end;

  CxbxKrnlTerminateThread();

  Result := 0;
end;
//pragma warning(pop)

// 0x00BB - NtClose
function xboxkrnl_NtClose(Handle: THandle): NTSTATUS; stdcall; {XBSYSAPI EXPORTNUM(187)}
var
  iEmuHandle: TEmuHandle;
begin
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuKrnl : NtClose' +
        #13'(' +
        #13'   Handle              : 0x' + IntToHex(Integer(Handle), 8) +
        #13');');

  // delete 'special' handles
  if IsEmuHandle(Handle) then
  begin
    iEmuHandle := EmuHandleToPtr(Handle);

    iEmuHandle.Free;

    Result := STATUS_SUCCESS;
  end
  else // close normal handles
    Result := NtClose(Handle);

  EmuSwapFS();   // Xbox FS
end;

// 0x00FF - PsCreateSystemThreadEx
function xboxkrnl_PsCreateSystemThreadEx(
  out ThreadHandle: THANDLE;
  ThreadExtraSize: ULONG;
  KernelStackSize: ULONG;
  TlsDataSize: ULONG;
  {out}ThreadId: PULONG {OPTIONAL};
  StartContext1: PVOID;
  StartContext2: PVOID;
  CreateSuspended: LONGBOOL;
  DebugStack: LONGBOOL;
  StartRoutine: PKSTART_ROUTINE): NTSTATUS; stdcall; {XBSYSAPI NTAPI}
var
  dwThreadId: DWORD;
  hDupHandle: THandle;
  iPCSTProxyParam: PCSTProxyParam;
begin
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuKrnl : PsCreateSystemThreadEx' +
       #13'(' +
       #13'   ThreadHandle        : 0x' + IntToHex(Integer(Addr(ThreadHandle)), 8) +
       #13'   ThreadExtraSize     : 0x' + IntToHex(ThreadExtraSize, 8) +
       #13'   KernelStackSize     : 0x' + IntToHex(KernelStackSize, 8) +
       #13'   TlsDataSize         : 0x' + IntToHex(TlsDataSize, 8) +
       #13'   ThreadId            : 0x' + IntToHex(Integer(Addr(ThreadId)), 8) +
       #13'   StartContext1       : 0x' + IntToHex(Integer(StartContext1), 8) +
       #13'   StartContext2       : 0x' + IntToHex(Integer(StartContext2), 8) +
       #13'   CreateSuspended     : 0x' + IntToHex(Integer(CreateSuspended), 8) +
       #13'   DebugStack          : 0x' + IntToHex(Integer(DebugStack), 8) +
       #13'   StartRoutine        : 0x' + IntToHex(Integer(Addr(StartRoutine)), 8) +
       #13');');

  // create thread, using our special proxy technique
  begin
    // PCSTProxy is responsible for cleaning up this pointer
    iPCSTProxyParam.StartContext1 := StartContext1;
    iPCSTProxyParam.StartContext2 := StartContext2;
    iPCSTProxyParam.StartRoutine  := StartRoutine;
    iPCSTProxyParam.StartSuspended := CreateSuspended;
    iPCSTProxyParam.hStartedEvent := CreateEvent(nil, False, False, nil);

    {out}ThreadHandle := BeginThread(nil, 0, @PCSTProxy, @iPCSTProxyParam, 0, {var}dwThreadId);

    WaitForSingleObject(iPCSTProxyParam.hStartedEvent, 1000);

    DbgPrintf('EmuKrnl : ThreadHandle : 0x' + IntToHex(ThreadHandle, 1) + ', ThreadId : 0x' + IntToHex(dwThreadId, 8));

    // we must duplicate this handle in order to retain Suspend/Resume thread rights from a remote thread
    begin
      hDupHandle := 0;

      DuplicateHandle(GetCurrentProcess(), ThreadHandle, GetCurrentProcess(), @hDupHandle, 0, FALSE, DUPLICATE_SAME_ACCESS);

      CxbxKrnlRegisterThread(hDupHandle);
    end;

    if Assigned(ThreadId) then
      {out}ThreadId^ := dwThreadId;
  end;

  EmuSwapFS();   // Xbox FS

  Result := STATUS_SUCCESS;
end;

end.
