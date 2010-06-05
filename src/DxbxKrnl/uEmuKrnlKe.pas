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
unit uEmuKrnlKe;

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
  uDxbxUtils,
  uDxbxKrnl,
  uDxbxKrnlUtils;

var
  {120}xboxkrnl_KeInterruptTime: DWord;
  {154}xboxkrnl_KeSystemTime: DWord;
  {157}xboxkrnl_KeTimeIncrement: DWord = $2710;

function xboxkrnl_KeAlertResumeThread(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_KeAlertThread(
  ThreadHandle: HANDLE
  ): NTSTATUS; stdcall;
function xboxkrnl_KeBoostPriorityThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_KeBugCheck(
  BugCheckMode: ULONG
  ); stdcall;
function xboxkrnl_KeBugCheckEx(
  BugCheckCode: DWORD;
  BugCheckParameter1: PVOID;
  BugCheckParameter2: PVOID;
  BugCheckParameter3: PVOID;
  BugCheckParameter4: PVOID
  ): NTSTATUS; stdcall;
function xboxkrnl_KeCancelTimer(
  hTimerHandle: HANDLE;
  pbPreviousState: PBOOLEAN
  ): NTSTATUS; stdcall;
function xboxkrnl_KeConnectInterrupt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeDelayExecutionThread(
  WaitMode: KPROCESSOR_MODE;
  Alertable: _BOOLEAN;
  Interval: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_KeDisconnectInterrupt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeEnterCriticalRegion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeGetCurrentIrql(): KIRQL; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeGetCurrentThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInitializeApc(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInitializeDeviceQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_KeInitializeDpc(
  Dpc: PKDPC;
  DeferredRoutine: PKDEFERRED_ROUTINE;
  DeferredContext: PVOID
  ); stdcall;
function xboxkrnl_KeInitializeEvent(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInitializeInterrupt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInitializeMutant(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInitializeQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInitializeSemaphore(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_KeInitializeTimerEx(
  Timer: PKTIMER;
  Type_: TIMER_TYPE
  ); stdcall;
function xboxkrnl_KeInsertByKeyDeviceQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInsertDeviceQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInsertHeadQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInsertQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInsertQueueApc(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInsertQueueDpc(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeIsExecutingDpc(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeLeaveCriticalRegion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KePulseEvent(
  hEventHandle: HANDLE;
  pPreviousState: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_KeQueryBasePriorityThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeQueryInterruptTime(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeQueryPerformanceCounter(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeQueryPerformanceFrequency(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_KeQuerySystemTime(
  CurrentTime: PLARGE_INTEGER
  ); stdcall;
function xboxkrnl_KeRaiseIrqlToDpcLevel(): KIRQL; stdcall;
function xboxkrnl_KeRaiseIrqlToSynchLevel(): KIRQL; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeReleaseMutant(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeReleaseSemaphore(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRemoveByKeyDeviceQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRemoveDeviceQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRemoveEntryDeviceQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRemoveQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRemoveQueueDpc(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeResetEvent(
  hEventHandle: HANDLE;
  pPreviousState: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_KeRestoreFloatingPointState(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeResumeThread(
  hThread: HANDLE;
  dwResumeCount: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_KeRundownQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSaveFloatingPointState(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSetBasePriorityThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSetDisableBoostThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSetEvent(
  hEventHandle: HANDLE;
  pPreviousState: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_KeSetEventBoostPriority(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSetPriorityProcess(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSetPriorityThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSetTimer(
  Timer: PKTIMER;
  DueTime: LARGE_INTEGER;
  Dpc: PKDPC // OPTIONAL
  ): LONGBOOL; stdcall;
function xboxkrnl_KeSetTimerEx(
  Timer: PKTIMER;
  DueTime: LARGE_INTEGER;
  Period: LONG; // OPTIONAL
  Dpc: PKDPC // OPTIONAL
  ): LONGBOOL; stdcall;
function xboxkrnl_KeStallExecutionProcessor(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSuspendThread(
  hThread: HANDLE;
  dwLastResumeCount: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_KeSynchronizeExecution(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeTestAlertThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeWaitForMultipleObjects(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeWaitForSingleObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

function xboxkrnl_KeAlertResumeThread(
  ThreadHandle: HANDLE;
  PreviousSuspendCount: PULONG
  ): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.NtAlertResumeThread(ThreadHandle, PreviousSuspendCount);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeAlertThread(
  ThreadHandle: HANDLE
  ): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.NtAlertThread(ThreadHandle);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeBoostPriorityThread(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeBoostPriorityThread');
  EmuSwapFS(fsXbox);
end;

// KeBugCheck:
// Bug checks the kernel.
// Same as KeBugCheckEx(BugCheckCode, 0, 0, 0, 0);
//
// Differences from NT: None, other than the reaction.
procedure xboxkrnl_KeBugCheck
(
  BugCheckMode: ULONG
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : KeBugCheck' +
      #13#10'(' +
      #13#10'   BugCheckMode      : 0x%.08X' +
      #13#10');',
      [BugCheckMode]);
{$ENDIF}

  // TODO -oCXBX: Investigate XapiFiberStartup maybe?

  EmuSwapFS(fsXbox);
end;

// KeBugCheckEx:
// Bug checks the kernel.
//
// Differences from NT: None, other than the reaction.
function xboxkrnl_KeBugCheckEx(
  BugCheckCode: DWORD;
  BugCheckParameter1: PVOID;
  BugCheckParameter2: PVOID;
  BugCheckParameter3: PVOID;
  BugCheckParameter4: PVOID
  ): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeBugCheckEx');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeCancelTimer(
  hTimerHandle: HANDLE;
  pbPreviousState: PBOOLEAN
  ): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.NtCancelTimer(hTimerHandle, pbPreviousState);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeConnectInterrupt(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeConnectInterrupt');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeDelayExecutionThread
(
  WaitMode: KPROCESSOR_MODE;
  Alertable: _BOOLEAN;
  Interval: PLARGE_INTEGER
): NTSTATUS; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
var
  ret: NTSTATUS;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : KeDelayExecutionThread'+
      #13#10'('+
      #13#10'   WaitMode            : 0x%.08X'+
      #13#10'   Alertable           : 0x%.08X'+
      #13#10'   Interval            : 0x%.16X' + // was %I64X
      #13#10');',
      [Ord(WaitMode), Alertable, Interval, QuadPart(Interval)]);
{$ENDIF}

  ret := NtDelayExecution(Alertable, PLARGE_INTEGER(Interval));
  EmuSwapFS(fsXbox);
  Result := ret;
end;

function xboxkrnl_KeDisconnectInterrupt(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeDisconnectInterrupt');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeEnterCriticalRegion(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeEnterCriticalRegion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeGetCurrentIrql(): KIRQL; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
var
  Pcr: PKPCR;
begin
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuKrnl : KeGetCurrentIrql()');
  EmuSwapFS(fsXbox);

  Pcr := GetCurrentKPCR(); // ReactOS calls this KeGetPcr();
  Result := Pcr.Irql;
end;

function xboxkrnl_KeGetCurrentThread(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeGetCurrentThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeApc(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeApc');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeDeviceQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeDeviceQueue');
  EmuSwapFS(fsXbox);
end;

// KeInitializeDpc:
// Initializes a DPC structure.
//
// Differences from NT: This function sets less fields than the NT version.
procedure xboxkrnl_KeInitializeDpc
(
  Dpc: PKDPC;
  DeferredRoutine: PKDEFERRED_ROUTINE;
  DeferredContext: PVOID
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : KeInitializeDpc' +
      #13#10'(' +
      #13#10'   Dpc                 : 0x%.08X' +
      #13#10'   DeferredRoutine     : 0x%.08X' +
      #13#10'   DeferredContext     : 0x%.08X' +
      #13#10');',
      [Dpc, Addr(DeferredRoutine), DeferredContext]);
{$ENDIF}

  // inialize Dpc field values
  Dpc.Number := 0;
  Dpc.DeferredRoutine := DeferredRoutine;
  Dpc.Type_ := CSHORT(Ord({enum KOBJECTS.}DpcObject));
  Dpc.DeferredContext := DeferredContext;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeEvent(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeEvent');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeInterrupt(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeInterrupt');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeMutant(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeMutant');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeSemaphore(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeSemaphore');
  EmuSwapFS(fsXbox);
end;

// KeInitializeTimerEx:
// Initializes a timer.
//
// Differences from NT: None.
procedure xboxkrnl_KeInitializeTimerEx
(
  Timer: PKTIMER;
  Type_: TIMER_TYPE
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : KeInitializeTimerEx' +
      #13#10'(' +
      #13#10'   Timer               : 0x%.08X' +
      #13#10'   Type                : 0x%.08X' +
      #13#10');',
      [Timer, Ord(Type_)]);
{$ENDIF}

  Timer.Header.Type_ := UCHAR(Ord(Type_) + 8);
  Timer.Header.Inserted := 0;
  Timer.Header.Size := sizeof(Timer^) div sizeof(ULONG);
  Timer.Header.SignalState := 0;
  Timer.TimerListEntry.Blink := NULL;
  Timer.TimerListEntry.Flink := NULL;
  Timer.Header.WaitListHead.Flink := @(Timer.Header.WaitListHead);
  Timer.Header.WaitListHead.Blink := @(Timer.Header.WaitListHead);
  Timer.DueTime.QuadPart := 0;
  Timer.Period := 0;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertByKeyDeviceQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertByKeyDeviceQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertDeviceQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertDeviceQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertHeadQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertHeadQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertQueueApc(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertQueueApc');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertQueueDpc(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertQueueDpc');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeIsExecutingDpc(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeIsExecutingDpc');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeLeaveCriticalRegion(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeLeaveCriticalRegion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KePulseEvent(
  hEventHandle: HANDLE;
  pPreviousState: PULONG
  ): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.NtPulseEvent(hEventHandle, pPreviousState);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeQueryBasePriorityThread(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeQueryBasePriorityThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeQueryInterruptTime(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeQueryInterruptTime');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeQueryPerformanceCounter(): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  Counter: LARGE_INTEGER;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : KeQueryPerformanceCounter();');
{$ENDIF}

  QueryPerformanceCounter({var}Counter);

  EmuSwapFS(fsXbox);

  Result := Counter.QuadPart;
end;

function xboxkrnl_KeQueryPerformanceFrequency(): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  Frequency: LARGE_INTEGER;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : KeQueryPerformanceFrequency()');
{$ENDIF}

  // Xbox Performance Counter Frequency := 337F98h
  QueryPerformanceFrequency({var}Frequency);

  EmuSwapFS(fsXbox);
  Result := Frequency.QuadPart;
end;

procedure xboxkrnl_KeQuerySystemTime
(
  CurrentTime: PLARGE_INTEGER
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  SystemTime: _SYSTEMTIME;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : KeQuerySystemTime'+
      #13#10'('+
      #13#10'   CurrentTime         : 0x%.08X'+
      #13#10');',
      [CurrentTime]);
{$ENDIF}

  // TODO -oCXBX: optimize for WinXP if speed ever becomes important here

  GetSystemTime({var}SystemTime);

  SystemTimeToFileTime({var}SystemTime, {var}PFILETIME(CurrentTime)^);

  EmuSwapFS(fsXbox);
end;

const DISPATCH_LEVEL = 2; // ??
const SYNCH_LEVEL = DISPATCH_LEVEL;

// KeRaiseIrqlToDpcLevel:
// Raises IRQL to DISPATCH_LEVEL.  Like KeRaiseIrql except returns old level directly.
//
// Differences from NT: None.
function xboxkrnl_KeRaiseIrqlToDpcLevel(): KIRQL; stdcall;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  Pcr: PKPCR;
  CurrentIrql: KIRQL;
begin
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuKrnl : KeRaiseIrqlToDpcLevel()');
  EmuSwapFS(fsXbox);

  Pcr := GetCurrentKPCR(); // ReactOS calls this KeGetPcr();

  // Save and update IRQL
  CurrentIrql := Pcr.Irql;
  Pcr.Irql := DISPATCH_LEVEL;

{$ifdef IRQL_DEBUG}
  // Validate correct raise
  if (CurrentIrql > DISPATCH_LEVEL) then
  begin
    // Crash system
    KeBugCheckEx(IRQL_NOT_GREATER_OR_EQUAL,
                 CurrentIrql,
                 DISPATCH_LEVEL,
                 0,
                 1);
  end;
{$endif}

  // Return the previous value
  Result := CurrentIrql;
end;

function xboxkrnl_KeRaiseIrqlToSynchLevel(): KIRQL; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
var
  Pcr: PKPCR;
  CurrentIrql: KIRQL;
begin
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuKrnl : KeRaiseIrqlToSynchLevel()');
  EmuSwapFS(fsXbox);

  Pcr := GetCurrentKPCR(); // ReactOS calls this KeGetPcr();

  // Save and update IRQL
  CurrentIrql := Pcr.Irql;
  Pcr.Irql := SYNCH_LEVEL;

{$ifdef IRQL_DEBUG}
  // Validate correct raise
  if (CurrentIrql > SYNCH_LEVEL) then
  begin
    // Crash system
    KeBugCheckEx(IRQL_NOT_GREATER_OR_EQUAL,
                 CurrentIrql,
                 SYNCH_LEVEL,
                 0,
                 1);
  end;
{$endif}

  // Return the previous value
  Result := CurrentIrql;
end;

function xboxkrnl_KeReleaseMutant(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeReleaseMutant');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeReleaseSemaphore(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeReleaseSemaphore');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveByKeyDeviceQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRemoveByKeyDeviceQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveDeviceQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRemoveDeviceQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveEntryDeviceQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRemoveEntryDeviceQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRemoveQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveQueueDpc(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRemoveQueueDpc');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeResetEvent(
  hEventHandle: HANDLE;
  pPreviousState: PULONG
  ): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.NtResetEvent(hEventHandle, pPreviousState);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRestoreFloatingPointState(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRestoreFloatingPointState');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeResumeThread(
  hThread : HANDLE;
  dwResumeCount : PULONG
  ): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.NtResumeThread(hThread, dwResumeCount);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRundownQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRundownQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSaveFloatingPointState(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSaveFloatingPointState');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetBasePriorityThread(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSetBasePriorityThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetDisableBoostThread(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSetDisableBoostThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetEvent(
  hEventHandle: HANDLE;
  pPreviousState: PULONG
  ): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.NtSetEvent(hEventHandle, pPreviousState);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetEventBoostPriority(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSetEventBoostPriority');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetPriorityProcess(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSetPriorityProcess');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetPriorityThread(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSetPriorityThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetTimer
(
  Timer: PKTIMER;
  DueTime: LARGE_INTEGER;
  Dpc: PKDPC // OPTIONAL
): LONGBOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
{$IFDEF DEBUG}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuKrnl : KeSetTimer' +
      #13#10'(' +
      #13#10'   Timer               : 0x%.08X' +
      #13#10'   DueTime             : 0x%.16X' + // was %I64X
      #13#10'   Dpc                 : 0x%.08X' +
      #13#10');',
      [Timer, QuadPart(@DueTime), Dpc]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  // Call the newer function and supply a period of 0 (source: ReactOS)
  Result := xboxkrnl_KeSetTimerEx(Timer, DueTime, {Period=}0, Dpc);
end;

function xboxkrnl_KeSetTimerEx
(
  Timer: PKTIMER;
  DueTime: LARGE_INTEGER;
  Period: LONG; // OPTIONAL
  Dpc: PKDPC // OPTIONAL
): LONGBOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : KeSetTimerEx' +
      #13#10'(' +
      #13#10'   Timer               : 0x%.08X' +
      #13#10'   DueTime             : 0x%.16X' + // was %I64X
      #13#10'   Period              : 0x%.08X' +
      #13#10'   Dpc                 : 0x%.08X' +
      #13#10');',
      [Timer, QuadPart(@DueTime), Period, Dpc]);
{$ENDIF}

  DxbxKrnlCleanup('KeSetTimerEx is not implemented');

  EmuSwapFS(fsXbox);

  Result := TRUE;
end;

function xboxkrnl_KeStallExecutionProcessor(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeStallExecutionProcessor');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSuspendThread(
  hThread: HANDLE;
  dwLastResumeCount: PULONG
  ): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Result := JwaNative.NtSuspendThread(hThread, dwLastResumeCount);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSynchronizeExecution(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSynchronizeExecution');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeTestAlertThread(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeTestAlertThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeWaitForMultipleObjects(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeWaitForMultipleObjects');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeWaitForSingleObject(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeWaitForSingleObject');
  EmuSwapFS(fsXbox);
end;

//

procedure xLaunchDataPage_Init;
begin
  xLaunchDataPage.Header.dwLaunchDataType := 2; // 2: dashboard, 0: title
  xLaunchDataPage.Header.dwTitleId := 0;
  strcopy(PAnsiChar(@(xLaunchDataPage.Header.szLaunchPath[0])), 'D:\default.xbe'#0);
  xLaunchDataPage.Header.dwFlags := 0;
end;

initialization

  xLaunchDataPage_Init;

end.
