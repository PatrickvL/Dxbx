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
  uConsts,
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
  Timer: PKTIMER
): _BOOLEAN; stdcall;
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
function xboxkrnl_KeInsertQueueDpc
(
  Dpc: PKDPC;
  SystemArgument1: PVOID;
  SystemArgument2: PVOID
): _BOOLEAN; stdcall;
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
function xboxkrnl_KeRemoveQueueDpc
(
  Dpc: PKDPC
): _BOOLEAN; stdcall;
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
  XBOX_PERFORMANCE_FREQUENCY = 3375000; // = ACPI timer frequency (3.375000 Mhz)

var
  NativePerformanceCounter: LARGE_INTEGER = (QuadPart:0);
  NativePerformanceFrequency: LARGE_INTEGER = (QuadPart:0);
  NativeToXbox_FactorForPerformanceFrequency: float;

function DxbxXboxGetTickCount(): DWORD;

implementation

const lfUnit = lfCxbx or lfKernel;

type DpcData = record
  Lock: CRITICAL_SECTION;//RTL_CRITICAL_SECTION; // JwaNative.RtlInitializeCriticalSection(Lock);
  DpcThread: THandle;
  DpcEvent: THandle;
  DpcQueue: LIST_ENTRY;
  TimerQueue: LIST_ENTRY;
end;

var g_DpcData: DpcData;

// http://www.koders.com/delphi/fid18D4CB8ADE82D407CF60EC8B45CB5A2AE79A10B9.aspx?s=thread
procedure InitializeListHead(pListHead: PListEntry);
begin
  pListHead^.Flink := pListHead;
  pListHead^.Blink := pListHead;
end;

procedure InsertHeadList(pListHead: PListEntry; pEntry: PListEntry);
var
  _EX_Flink: PListEntry;
  _EX_ListHead: PListEntry;
begin
  _EX_ListHead := pListHead;
  _EX_Flink := _EX_ListHead^.Flink;
  pEntry^.Flink := _EX_Flink;
  pEntry^.Blink := _EX_ListHead;
  _EX_Flink^.Blink := pEntry;
  _EX_ListHead^.Flink := pEntry;
end;

procedure RemoveEntryList(pEntry: PListEntry);
var
  _EX_Blink: PListEntry;
  _EX_Flink: PListEntry;
begin
  _EX_Flink := pEntry^.Flink;
  _EX_Blink := pEntry^.Blink;
  _EX_Blink^.Flink := _EX_Flink;
  _EX_Flink^.Blink := _EX_Blink;
end;

function IsListEmpty(pListHead: PListEntry): Boolean;
begin
  Result := (pListHead^.Flink = pListHead);
end;

function RemoveHeadList(pListHead: PListEntry): PListEntry;
begin
  Result := pListHead^.Flink;
  RemoveEntryList(pListHead^.Flink);
end;

procedure InsertTailList(pListHead: PListEntry; pEntry: PListEntry);
var
  _EX_Blink: PListEntry;
  _EX_ListHead: PListEntry;
begin
  _EX_ListHead := pListHead;
  _EX_Blink := _EX_ListHead^.Blink;
  pEntry^.Flink := _EX_ListHead;
  pEntry^.Blink := _EX_Blink;
  _EX_Blink^.Flink := pEntry;
  _EX_ListHead^.Blink := pEntry;
end;

function RemoveTailList(pListHead: PListEntry): PListEntry;
begin
  Result := pListHead^.Blink;
  RemoveEntryList(pListHead^.Blink);
end;

//

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
  Unimplemented('KeBoostPriorityThread');
  Result := S_OK;
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
      #13#10'   BugCheckMode         : 0x%.08X' +
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
  Unimplemented('KeBugCheckEx');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeCancelTimer(
  Timer: PKTIMER
): _BOOLEAN; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : KeCancelTimer' +
      #13#10'(' +
      #13#10'   Timer                : 0x%.08X' +
      #13#10');',
      [Timer]);

  if (Timer.TimerListEntry.Flink <> nil) then
  begin
    EnterCriticalSection({var}g_DpcData.Lock);

    if (Timer.TimerListEntry.Flink <> nil) then
    begin
      RemoveEntryList(@Timer.TimerListEntry);
      Timer.TimerListEntry.Flink := NULL;
    end;

    LeaveCriticalSection({var}g_DpcData.Lock);
  end;

  Result := TRUE;

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
//var
//  NativeInterval: LARGE_INTEGER;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfExtreme) then
    LogBegin('EmuKrnl : KeDelayExecutionThread').
      _(Ord(WaitMode), 'WaitMode').
      _(Alertable, 'Alertable').
      _(Interval, 'Interval').
    LogEnd();

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
//  if Interval.QuadPart < 0 then
//  begin
    Result := NtDelayExecution(Alertable, Interval);
//  end
//  else
//  begin
//    // TODO -oDxbx : Fix this case
//    Result := STATUS_SUCCESS;
//  end;

  // HACK : Since we have no APC implementation yet, we need to return the 'Alerted' state
  // now and then to stop callers from hanging indefinitely on the alert :
  if Alertable then
    if Random(10) = 0 then
      Result := STATUS_ALERTED;

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
  Unimplemented('KeInitializeApc');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeDeviceQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeInitializeDeviceQueue');
  Result := S_OK;
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
      #13#10'   Dpc                  : 0x%.08X' +
      #13#10'   DeferredRoutine      : 0x%.08X' +
      #13#10'   DeferredContext      : 0x%.08X' +
      #13#10');',
      [Dpc, Addr(DeferredRoutine), DeferredContext]);
    EmuSwapFS(fsXbox);
  end;

  // inialize Dpc field values
  Dpc.DeferredRoutine := DeferredRoutine;
  Dpc.DeferredContext := DeferredContext;
  Dpc.DpcListEntry.Flink := nil;
end;

function xboxkrnl_KeInitializeEvent(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeInitializeEvent');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeInterrupt(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeInitializeInterrupt');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeMutant(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeInitializeMutant');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeInitializeQueue');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeSemaphore(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeInitializeSemaphore');
  Result := S_OK;
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
      #13#10'   Timer                : 0x%.08X' +
      #13#10'   Type                 : 0x%.08X' +
      #13#10');',
      [Timer, Ord(Type_)]);
    EmuSwapFS(fsXbox);
  end;

  Timer.Header.Type_ := UCHAR(Ord(Type_) + 8); // 8 = TimerNotificationObject ?
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
  Unimplemented('KeInsertByKeyDeviceQueue');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertDeviceQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeInsertDeviceQueue');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertHeadQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeInsertHeadQueue');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeInsertQueue');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertQueueApc(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeInsertQueueApc');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertQueueDpc
(
  Dpc: PKDPC;
  SystemArgument1: PVOID;
  SystemArgument2: PVOID
): _BOOLEAN; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : KeInsertQueueDpc' +
      #13#10'(' +
      #13#10'   Dpc                  : 0x%.08X' +
      #13#10'   SystemArgument1      : 0x%.08X' +
      #13#10'   SystemArgument2      : 0x%.08X' +
      #13#10');',
      [Dpc, SystemArgument1, SystemArgument2]);

  if (Dpc.DpcListEntry.Flink = nil) then
  begin
    EnterCriticalSection({var}g_DpcData.Lock);

    if (Dpc.DpcListEntry.Flink = nil) then
    begin
      Dpc.SystemArgument1 := SystemArgument1;
      Dpc.SystemArgument2 := SystemArgument2;
      InsertTailList(@g_DpcData.DpcQueue, @Dpc.DpcListEntry);
    end;

    LeaveCriticalSection({var}g_DpcData.Lock);

    SetEvent(g_DpcData.DpcEvent);
  end;

  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeIsExecutingDpc(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeIsExecutingDpc');
  Result := S_OK;
  EmuSwapFS(fsXbox);

//??  Result := GetCurrentKPCR().PrcbData.DpcRoutineActive;
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
  Unimplemented('KeQueryBasePriorityThread');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeQueryInterruptTime(): ULONGLONG; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
var
  CurrentTime: LARGE_INTEGER;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfExtreme) then
    DbgPrintf('EmuKrnl : KeQueryInterruptTime();');

  // Dxbx note : We use a more direct implementation than Cxbx here,
  // which depends on xboxkrnl_KeSystemTimePtr set to the native
  // Windows SystemTimer (see ConnectWindowsTimersToThunkTable) :

  ReadSystemTimeIntoLargeInteger(xboxkrnl_KeInterruptTimePtr, @CurrentTime);

  Result := CurrentTime.QuadPart;

  EmuSwapFS(fsXbox);
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
  // and we we're bound to a single core, so we could do that too, but on Windows
  // rdtsc is not a very stable counter, so instead, we'll use the native PeformanceCounter :
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
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfExtreme) then
    DbgPrintf('EmuKrnl : KeQuerySystemTime' +
      #13#10'(' +
      #13#10'   CurrentTime          : 0x%.08X' +
      #13#10');',
      [CurrentTime]);

  // Dxbx note : We use a more direct implementation than Cxbx here,
  // which depends on xboxkrnl_KeSystemTimePtr set to the native
  // Windows SystemTimer (see ConnectWindowsTimersToThunkTable) :
  ReadSystemTimeIntoLargeInteger(xboxkrnl_KeSystemTimePtr, CurrentTime);
//  GetSystemTimeAsFileTime ??

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
  Unimplemented('KeReleaseMutant');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeReleaseSemaphore(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeReleaseSemaphore');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveByKeyDeviceQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeRemoveByKeyDeviceQueue');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveDeviceQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeRemoveDeviceQueue');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveEntryDeviceQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeRemoveEntryDeviceQueue');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveQueue(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeRemoveQueue');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveQueueDpc
(
  Dpc: PKDPC
): _BOOLEAN; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : KeRemoveQueueDpc' +
      #13#10'(' +
      #13#10'   Dpc                  : 0x%.08X' +
      #13#10');',
      [Dpc]);

  if (Dpc.DpcListEntry.Flink <> nil) then
  begin
    EnterCriticalSection({var}g_DpcData.Lock);

    if (Dpc.DpcListEntry.Flink <> nil) then
    begin
      RemoveEntryList(@Dpc.DpcListEntry);
      Dpc.DpcListEntry.Flink := NULL;
    end;

    LeaveCriticalSection({var}g_DpcData.Lock);
  end;

  Result := TRUE;

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
  Unimplemented('KeRestoreFloatingPointState');
  Result := S_OK;
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
  Unimplemented('KeRundownQueue');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSaveFloatingPointState(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeSaveFloatingPointState');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetBasePriorityThread(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeSetBasePriorityThread');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetDisableBoostThread(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeSetDisableBoostThread');
  Result := S_OK;
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
  Unimplemented('KeSetEventBoostPriority');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetPriorityProcess(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeSetPriorityProcess');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetPriorityThread(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeSetPriorityThread');
  Result := S_OK;
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
      #13#10'   Timer                : 0x%.08X' +
      #13#10'   DueTime              : 0x%.16X' + // was %I64X
      #13#10'   Dpc                  : 0x%.08X' +
      #13#10');',
      [Timer, DueTime.QuadPart, Dpc]);
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
      #13#10'   Timer                : 0x%.08X' +
      #13#10'   DueTime              : 0x%.16X' + // was %I64X
      #13#10'   Period               : 0x%.08X' +
      #13#10'   Dpc                  : 0x%.08X' +
      #13#10');',
      [Timer, DueTime.QuadPart, Period, Dpc]);

  EnterCriticalSection({var}g_DpcData.Lock);

  if (Timer.TimerListEntry.Flink = nil) then
  begin
    Timer.DueTime.QuadPart := (DueTime.QuadPart div -10000) + DxbxXboxGetTickCount();
    Timer.Period := Period;
    Timer.Dpc := Dpc;
    InsertTailList(@g_DpcData.TimerQueue, @Timer.TimerListEntry);
  end;

  LeaveCriticalSection({var}g_DpcData.Lock);

  SetEvent(g_DpcData.DpcEvent);

  EmuSwapFS(fsXbox);

  Result := TRUE;
end;

function xboxkrnl_KeStallExecutionProcessor(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeStallExecutionProcessor');
  Result := S_OK;
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
  Unimplemented('KeSynchronizeExecution');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeTestAlertThread(): NTSTATUS; stdcall;
// Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeTestAlertThread');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeWaitForMultipleObjects(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeWaitForMultipleObjects');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeWaitForSingleObject(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeWaitForSingleObject');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

//

procedure xLaunchDataPage_Init;
begin
  xLaunchDataPage.Header.dwLaunchDataType := 2; // 2: dashboard, 0: title
  xLaunchDataPage.Header.dwTitleId := 0;
  strcopy(PAnsiChar(@(xLaunchDataPage.Header.szLaunchPath[0])), DxbxDefaultXbeVolumeLetter +':\default.xbe'#0);
  xLaunchDataPage.Header.dwFlags := 0;
end;

var
  BootTickCount: DWORD;

procedure PerformanceCounters_Init;
begin
  BootTickCount := GetTickCount();

  JwaNative.NtQueryPerformanceCounter(@NativePerformanceCounter, @NativePerformanceFrequency);

  NativeToXbox_FactorForPerformanceFrequency := XBOX_PERFORMANCE_FREQUENCY / NativePerformanceFrequency.QuadPart;
end;

// The Xbox GetTickCount is measured in milliseconds, just like the native GetTickCount.
// The only difference we'll take into account here, is that the Xbox will probably reboot
// much more often than Windows, so we correct this with a 'BootTickCount' value :
function DxbxXboxGetTickCount(): DWORD;
begin
  Result := GetTickCount() - BootTickCount;
end;

function EmuThreadDpcHandler(lpVoid: LPVOID): DWORD; stdcall;
var
  pkdpc: XboxKrnl.PKDPC;
  dwWait: DWord;
  dwNow: DWord;
  lWait: LONG;
  pktimer: XboxKrnl.PKTIMER;
begin
  while True do
  begin
    EnterCriticalSection({var}g_DpcData.Lock);

//    if (g_DpcData._fShutdown)
//        break;

//    Assert(g_DpcData._dwThreadId == GetCurrentThreadId());
//    Assert(g_DpcData._dwDpcThreadId == 0);
//    g_DpcData._dwDpcThreadId := g_DpcData._dwThreadId;
//    Assert(g_DpcData._dwDpcThreadId != 0);

    while (not IsListEmpty(@g_DpcData.DpcQueue)) do
    begin
      pkdpc := XboxKrnl.PKDPC(RemoveHeadList(@g_DpcData.DpcQueue));
      pkdpc.DpcListEntry.Flink := NULL;
      pkdpc.DeferredRoutine(pkdpc,
                            pkdpc.DeferredContext,
                            pkdpc.SystemArgument1,
                            pkdpc.SystemArgument2);
    end;

    dwWait := INFINITE;

    if (not IsListEmpty(@g_DpcData.TimerQueue)) then
    begin
      while True do
      begin
        dwNow   := DxbxXboxGetTickCount();
        dwWait  := INFINITE;
        pktimer := XboxKrnl.PKTIMER(g_DpcData.TimerQueue.Flink);
        pkdpc   := NULL;

        while (pktimer <> XboxKrnl.PKTIMER(@g_DpcData.TimerQueue)) do
        begin
          lWait := LONG(pktimer.DueTime.LowPart - dwNow);

          if (lWait <= 0) then
          begin
            pktimer.DueTime.LowPart := pktimer.Period + dwNow;
            pkdpc := pktimer.Dpc;
            break;
          end;

          if (dwWait > DWORD(lWait)) then
            dwWait := DWORD(lWait);

          pktimer := XboxKrnl.PKTIMER(pktimer.TimerListEntry.Flink);
        end;

        if (pkdpc = NULL) then
          break;

        pkdpc.DeferredRoutine(pkdpc,
                              pkdpc.DeferredContext,
                              pkdpc.SystemArgument1,
                              pkdpc.SystemArgument2);
      end;
    end;

//    Assert(g_DpcData._dwThreadId = GetCurrentThreadId());
//    Assert(g_DpcData._dwDpcThreadId = g_DpcData._dwThreadId);
//    g_DpcData._dwDpcThreadId = 0;
    LeaveCriticalSection({var}g_DpcData.Lock);

    WaitForSingleObject(g_DpcData.DpcEvent, dwWait);
  end;

  Result := S_OK;
end;

procedure InitDpcAndTimerThread();
var
  dwThreadId: DWord;
begin
  InitializeCriticalSection({var}g_DpcData.Lock);
  InitializeListHead(@g_DpcData.DpcQueue);
  InitializeListHead(@g_DpcData.TimerQueue);
  g_DpcData.DpcEvent := CreateEvent(NULL, FALSE, FALSE, NULL);

  g_DpcData.DpcThread := CreateThread(nil, 0, @EmuThreadDpcHandler, nil, 0, @dwThreadId);
  SetThreadPriority(g_DpcData.DpcThread, THREAD_PRIORITY_HIGHEST);
end;

initialization

  xLaunchDataPage_Init;

  PerformanceCounters_Init;

  InitDpcAndTimerThread;
end.
