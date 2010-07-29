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
  uDxbxUtils,
  uLog,
  uDxbxKrnlUtils,
  uEmuFS,
  uEmuKrnl;

const
  CLOCK_TIME_INCREMENT = $2710;

var
  // Dxbx note : These two where once values, but instead we now point to
  // the native Windows versions (see ConnectWindowsTimersToThunkTable) :
  xboxkrnl_KeInterruptTimePtr: PKSYSTEM_TIME; // Used for KernelThunk[120]
  xboxkrnl_KeSystemTimePtr: PKSYSTEM_TIME; // Used for KernelThunk[154]

  {157}xboxkrnl_KeTimeIncrement: ULONG = CLOCK_TIME_INCREMENT;

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
function xboxkrnl_KeConnectInterrupt(
  Interrupt: PKINTERRUPT
): _BOOLEAN; stdcall;
function xboxkrnl_KeDelayExecutionThread(
  WaitMode: KPROCESSOR_MODE;
  Alertable: _BOOLEAN;
  Interval: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_KeDisconnectInterrupt(
  Interrupt: PKINTERRUPT
): _BOOLEAN; stdcall;
procedure xboxkrnl_KeEnterCriticalRegion(); stdcall;
function xboxkrnl_KeGetCurrentIrql(): KIRQL; stdcall;
function xboxkrnl_KeGetCurrentThread(): PKTHREAD; stdcall;
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
procedure xboxkrnl_KeLeaveCriticalRegion(); stdcall;
function xboxkrnl_KePulseEvent(
  hEventHandle: HANDLE;
  pPreviousState: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_KeQueryBasePriorityThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeQueryInterruptTime(): ULONGLONG; stdcall;
function xboxkrnl_KeQueryPerformanceCounter(
  ): _LARGE_INTEGER; stdcall;
function xboxkrnl_KeQueryPerformanceFrequency(
  ): _LARGE_INTEGER; stdcall;
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

const
  XBOX_PERFORMANCE_FREQUENCY = $337F98; // = 3.375000 Mhz;

var
  NativePerformanceCounter: LARGE_INTEGER = (QuadPart:0);
  NativePerformanceFrequency: LARGE_INTEGER = (QuadPart:0);
  NativeToXbox_FactorForPerformanceFrequency: float;

  NativeToXbox_TickCountMultiplier: DWORD;
  NativeTickCountLowPtr: PDWORD;

function DxbxXboxGetTickCount(): DWORD;

implementation

const lfUnit = lfCxbx or lfKernel;

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

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : KeBugCheck' +
      #13#10'(' +
      #13#10'   BugCheckMode      : 0x%.08X' +
      #13#10');',
      [BugCheckMode]);

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

function xboxkrnl_KeConnectInterrupt(
  Interrupt: PKINTERRUPT
): _BOOLEAN; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeConnectInterrupt');
  Result := False;
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
  NativeInterval: LARGE_INTEGER;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : KeDelayExecutionThread' +
      #13#10'(' +
      #13#10'   WaitMode            : 0x%.08X' +
      #13#10'   Alertable           : 0x%.08X' +
      #13#10'   Interval            : 0x%.16X' + // was %I64X
      #13#10');',
      [Ord(WaitMode), Alertable, QuadPart(Interval)]);

  // Dxbx note : The Interval can be negative or positive.
  // When negative, it's a relative wait, when positive it's
  // a wait for an absolute time to pass.
  // For absolute waits, the Xbox seems to compare against the
  // KeSystemTime, while for relative waits, it corrects the
  // remaining time using KeInterruptTime.
  // A negative value is expressed in 100 nanosecond units,
  // which means 1 millisecond is indicated by the value
  // 10,000 and 1 second is indicated by 10,000,000.
  // The fact that this timer uses 100 nanosecond units,
  // and the Xbox combines these measures with the system/
  // interrupt timers, indicates that these timers use the
  // same scale... TODO : Take that into account in our
  // timing thread!
  if Interval.QuadPart < 0 then
  begin
    // TODO -oDxbx : Find out why NtDelayExecution causes long delays, disable it for now :
    NativeInterval.QuadPart := Interval.QuadPart div 10000;

    Result := NtDelayExecution(Alertable, @NativeInterval);
  end
  else
    Result := STATUS_SUCCESS;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeDisconnectInterrupt(
  Interrupt: PKINTERRUPT
): _BOOLEAN; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeDisconnectInterrupt');
  Result := False;
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_KeEnterCriticalRegion(); stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeEnterCriticalRegion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeGetCurrentIrql(): KIRQL; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
var
  Pcr: PKPCR;
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : KeGetCurrentIrql();');
    EmuSwapFS(fsXbox);
  end;

  Pcr := GetCurrentKPCR(); // ReactOS calls this KeGetPcr();
  Result := Pcr.Irql;
end;

function xboxkrnl_KeGetCurrentThread(): PKTHREAD; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
var
  Pcr: PKPCR;
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : KeGetCurrentThread();');
    EmuSwapFS(fsXbox);
  end;

  Pcr := GetCurrentKPCR(); // ReactOS calls this KeGetPcr();
  Result := Pcr.Prcb.CurrentThread;
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
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : KeInitializeDpc' +
      #13#10'(' +
      #13#10'   Dpc                 : 0x%.08X' +
      #13#10'   DeferredRoutine     : 0x%.08X' +
      #13#10'   DeferredContext     : 0x%.08X' +
      #13#10');',
      [Dpc, Addr(DeferredRoutine), DeferredContext]);
    EmuSwapFS(fsXbox);
  end;

  // inialize Dpc field values
  Dpc.Number := 0;
  Dpc.DeferredRoutine := DeferredRoutine;
  Dpc.Type_ := CSHORT(Ord({enum KOBJECTS.}DpcObject));
  Dpc.DeferredContext := DeferredContext;
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
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : KeInitializeTimerEx' +
      #13#10'(' +
      #13#10'   Timer               : 0x%.08X' +
      #13#10'   Type                : 0x%.08X' +
      #13#10');',
      [Timer, Ord(Type_)]);
    EmuSwapFS(fsXbox);
  end;

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

procedure xboxkrnl_KeLeaveCriticalRegion(); stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeLeaveCriticalRegion');
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

function xboxkrnl_KeQueryInterruptTime(): ULONGLONG; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
var
  CurrentTime: LARGE_INTEGER;
begin
  if MayLog(lfUnit or lfExtreme) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : KeQueryInterruptTime();');
    EmuSwapFS(fsXbox);
  end;

  // Dxbx note : We use a more direct implementation than Cxbx here,
  // which depends on xboxkrnl_KeSystemTimePtr set to the native
  // Windows SystemTimer (see ConnectWindowsTimersToThunkTable) :

  ReadSystemTimeIntoLargeInteger(xboxkrnl_KeInterruptTimePtr, @CurrentTime);

  Result := CurrentTime.QuadPart;
end;

function xboxkrnl_KeQueryPerformanceCounter(): _LARGE_INTEGER; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
const
  PerformanceFrequency: PLARGE_INTEGER = nil;
var
  PerformanceCounter: LARGE_INTEGER;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfExtreme) then
    DbgPrintf('EmuKrnl : KeQueryPerformanceCounter();');

  // Dxbx note : Xbox actually uses the RDTSC machine code instruction for this,
  // and we could do that too, because we're bound to a single core anyway.
  //
  // TODO -oDxbx: Switch over do RDTSC, and factor in the difference between
  // native and Xbox processor frequencies.


  JwaNative.NtQueryPerformanceCounter(@PerformanceCounter, PerformanceFrequency{=nil});

  // Re-base the performance counter to increase accuracy of the following conversion :
  PerformanceCounter.QuadPart := PerformanceCounter.QuadPart - NativePerformanceCounter.QuadPart;
  // We appy a conversion factor here, to fake Xbox1-like increment-speed behaviour :
  PerformanceCounter.QuadPart := Trunc(NativeToXbox_FactorForPerformanceFrequency * PerformanceCounter.QuadPart);

  EmuSwapFS(fsXbox);

  Result := _LARGE_INTEGER(PerformanceCounter);
end;

function xboxkrnl_KeQueryPerformanceFrequency(): _LARGE_INTEGER; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  PerformanceFrequency: LARGE_INTEGER;
begin
  if MayLog(lfUnit or lfExtreme) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : KeQueryPerformanceFrequency();');
    EmuSwapFS(fsXbox);
  end;

  // Dxbx note : We return the real Xbox1 frequency here,
  // to make subsequent calculations behave the same as on the real Xbox1 :
  PerformanceFrequency.QuadPart := XBOX_PERFORMANCE_FREQUENCY;

  Result := _LARGE_INTEGER(PerformanceFrequency);
end;

procedure xboxkrnl_KeQuerySystemTime
(
  CurrentTime: PLARGE_INTEGER
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  if MayLog(lfUnit or lfExtreme) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : KeQuerySystemTime' +
      #13#10'(' +
      #13#10'   CurrentTime         : 0x%.08X' +
      #13#10');',
      [CurrentTime]);
    EmuSwapFS(fsXbox);
  end;

  // Dxbx note : We use a more direct implementation than Cxbx here,
  // which depends on xboxkrnl_KeSystemTimePtr set to the native
  // Windows SystemTimer (see ConnectWindowsTimersToThunkTable) :
  ReadSystemTimeIntoLargeInteger(xboxkrnl_KeSystemTimePtr, CurrentTime);
end;

const DISPATCH_LEVEL = 2; // ??
const SYNCH_LEVEL = DISPATCH_LEVEL;

// KeRaiseIrqlToDpcLevel:
// Raises IRQL to DISPATCH_LEVEL.  Like KeRaiseIrql except returns old level directly.
//
// Differences from NT: None.
function xboxkrnl_KeRaiseIrqlToDpcLevel(): KIRQL; stdcall;
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
(*var
  Pcr: PKPCR;
  CurrentIrql: KIRQL; *)
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : KeRaiseIrqlToDpcLevel();');
    EmuSwapFS(fsXbox);
  end;

  // TODO : DXBX - This we get from reactos, but
  // Using the PCr gives exceptions.

(*  Pcr := GetCurrentKPCR(); // ReactOS calls this KeGetPcr();

  // Save and update IRQL
  CurrentIrql := Pcr.Irql;
//  Pcr.Irql := DISPATCH_LEVEL;

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
  Result := CurrentIrql; *)

  Result := 0;
end;

function xboxkrnl_KeRaiseIrqlToSynchLevel(): KIRQL; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
var
  Pcr: PKPCR;
  CurrentIrql: KIRQL;
begin
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : KeRaiseIrqlToSynchLevel();');
    EmuSwapFS(fsXbox);
  end;

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
  if MayLog(lfUnit) then
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuKrnl : KeSetTimer >>' +
      #13#10'(' +
      #13#10'   Timer               : 0x%.08X' +
      #13#10'   DueTime             : 0x%.16X' + // was %I64X
      #13#10'   Dpc                 : 0x%.08X' +
      #13#10');',
      [Timer, QuadPart(@DueTime), Dpc]);
    EmuSwapFS(fsXbox);
  end;

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

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : KeSetTimerEx' +
      #13#10'(' +
      #13#10'   Timer               : 0x%.08X' +
      #13#10'   DueTime             : 0x%.16X' + // was %I64X
      #13#10'   Period              : 0x%.08X' +
      #13#10'   Dpc                 : 0x%.08X' +
      #13#10');',
      [Timer, QuadPart(@DueTime), Period, Dpc]);

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

procedure PerformanceCounters_Init;
begin
  JwaNative.NtQueryPerformanceCounter(@NativePerformanceCounter, @NativePerformanceFrequency);

  NativeToXbox_FactorForPerformanceFrequency := XBOX_PERFORMANCE_FREQUENCY / NativePerformanceFrequency.QuadPart;
end;

procedure InitializeXboxTickCount();
begin
  if DxbxUserSharedData.NtMajorVersion < 6 then
    // For WinXP, use the deprecated tick counter :
    // See http://www.purebasic.fr/english/viewtopic.php?f=12&t=39017
    NativeTickCountLowPtr := DxbxNtTickCountLowDeprecated
  else
    // On Vista and above, we use KUSER_SHARED_DATA TickCount.Low here, as suggested by
    // http://undocumented.ntinternals.net/UserMode/Undocumented%20Functions/Time/NtGetTickCount.html
    NativeTickCountLowPtr := @(DxbxNtTickCount.LowPart);

  // TODO -oDxbx: Use DxbxMinimumResolution, XBOX_PERFORMANCE_FREQUENCY, CLOCK_TIME_INCREMENT
  // or any other magic to calculate the value for NativeToXbox_TickCountMultiplier so that
  // DxbxXboxGetTickCount will match the Xbox frequency !!!
  NativeToXbox_TickCountMultiplier := DxbxMinimumResolution; // ??
end;

//

// This is a modified copy of GetTickCount, which reads the native TickCount(Low),
// and corrects the result by a factor which takes care of the conversion from
// native to Xbox frequency, resulting in behaviour approaching that of the original Xbox :
{$IFDEF PURE_PASCAL}
function DxbxXboxGetTickCount(): DWORD;
begin
  Result := DWORD((ULONGLONG(NativeTickCountLowPtr^) * NativeToXbox_TickCountMultiplier) shr $18);
end;
{$ELSE}
function DxbxXboxGetTickCount(): DWORD;
asm
  mov edx, [NativeTickCountLowPtr]
  mov eax, [edx]
  mul dword ptr [NativeToXbox_TickCountMultiplier] // fill EAX, EDX with multiplication
  shrd eax,edx, $18 // Divide by (1 shl 24)
end;
{$ENDIF}

initialization

  xLaunchDataPage_Init;

  PerformanceCounters_Init;

  InitializeXboxTickCount();

end.
