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
  uDxbxKrnl,
  uDxbxKrnlUtils;

function xboxkrnl_KeAlertResumeThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeAlertThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeBoostPriorityThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeBugCheck(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeBugCheckEx(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeCancelTimer(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeConnectInterrupt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeDelayExecutionThread(
  WaitMode: KPROCESSOR_MODE;
  Alertable: LONGBOOL;
  Interval: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
function xboxkrnl_KeDisconnectInterrupt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeEnterCriticalRegion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeGetCurrentIrql(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
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
  _Type: TIMER_TYPE
  ); stdcall;
function xboxkrnl_KeInsertByKeyDeviceQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInsertDeviceQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInsertHeadQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInsertQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInsertQueueApc(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInsertQueueDpc(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInterruptTime(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeIsExecutingDpc(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeLeaveCriticalRegion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KePulseEvent(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeQueryBasePriorityThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeQueryInterruptTime(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeQueryPerformanceCounter(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeQueryPerformanceFrequency(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_KeQuerySystemTime(
  CurrentTime: PLARGE_INTEGER
  ); stdcall;
function xboxkrnl_KeRaiseIrqlToDpcLevel(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRaiseIrqlToSynchLevel(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeReleaseMutant(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeReleaseSemaphore(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRemoveByKeyDeviceQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRemoveDeviceQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRemoveEntryDeviceQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRemoveQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRemoveQueueDpc(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeResetEvent(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRestoreFloatingPointState(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeResumeThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeRundownQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSaveFloatingPointState(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSetBasePriorityThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSetDisableBoostThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSetEvent(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
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
function xboxkrnl_KeSuspendThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSynchronizeExecution(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeSystemTime(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeTestAlertThread(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeTimeIncrement(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeWaitForMultipleObjects(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeWaitForSingleObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

function xboxkrnl_KeAlertResumeThread(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeAlertResumeThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeAlertThread(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeAlertThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeBoostPriorityThread(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeBoostPriorityThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeBugCheck(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeBugCheck');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeBugCheckEx(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeBugCheckEx');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeCancelTimer(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeCancelTimer');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeConnectInterrupt(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeConnectInterrupt');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeDelayExecutionThread(
  WaitMode: KPROCESSOR_MODE;
  Alertable: LONGBOOL;
  Interval: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeDelayExecutionThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeDisconnectInterrupt(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeDisconnectInterrupt');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeEnterCriticalRegion(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeEnterCriticalRegion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeGetCurrentIrql(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeGetCurrentIrql');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeGetCurrentThread(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeGetCurrentThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeApc(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeApc');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeDeviceQueue(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeDeviceQueue');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_KeInitializeDpc(
  Dpc: PKDPC;
  DeferredRoutine: PKDEFERRED_ROUTINE;
  DeferredContext: PVOID
  ); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:90
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuKrnl : KeInitializeDpc' +
         #13#10'(' +
         #13#10'   Dpc                 : 0x%.08X' +
         #13#10'   DeferredRoutine     : 0x%.08X' +
         #13#10'   DeferredContext     : 0x%.08X' +
         #13#10');',
         [ Dpc, Addr(DeferredRoutine), DeferredContext]);

  // inialize Dpc field values
  Dpc.Number := 0;
  Dpc.DeferredRoutine := DeferredRoutine;
  Dpc._Type := CSHORT(Ord({enum KOBJECTS.}DpcObject));
  Dpc.DeferredContext := DeferredContext;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeEvent(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeEvent');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeInterrupt(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeInterrupt');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeMutant(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeMutant');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeQueue(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInitializeSemaphore(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInitializeSemaphore');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_KeInitializeTimerEx(
  Timer: PKTIMER;
  _Type: TIMER_TYPE
  ); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuKrnl : KeInitializeTimerEx' +
         #13#10'(' +
         #13#10'   Timer               : 0x%.08X' +
         #13#10'   Type                : 0x%.08X' +
         #13#10');',
         [Timer, Ord(_Type)]);

  Timer.Header._Type              := UCHAR(Ord(_Type) + 8);
  Timer.Header.Inserted           := 0;
  Timer.Header.Size               := SizeOf(Timer^) div SizeOf(ULONG);
  Timer.Header.SignalState        := 0;
  Timer.TimerListEntry.Blink      := NULL;
  Timer.TimerListEntry.Flink      := NULL;
  Timer.Header.WaitListHead.Flink := @(Timer.Header.WaitListHead);
  Timer.Header.WaitListHead.Blink := @(Timer.Header.WaitListHead);
  Timer.DueTime.QuadPart          := 0;
  Timer.Period                    := 0;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertByKeyDeviceQueue(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertByKeyDeviceQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertDeviceQueue(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertDeviceQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertHeadQueue(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertHeadQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertQueue(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertQueueApc(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertQueueApc');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInsertQueueDpc(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInsertQueueDpc');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeInterruptTime(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeInterruptTime');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeIsExecutingDpc(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeIsExecutingDpc');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeLeaveCriticalRegion(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeLeaveCriticalRegion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KePulseEvent(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KePulseEvent');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeQueryBasePriorityThread(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeQueryBasePriorityThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeQueryInterruptTime(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeQueryInterruptTime');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeQueryPerformanceCounter(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeQueryPerformanceCounter');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeQueryPerformanceFrequency(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeQueryPerformanceFrequency');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_KeQuerySystemTime(
  CurrentTime: PLARGE_INTEGER
  ); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('KeQuerySystemTime');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRaiseIrqlToDpcLevel(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRaiseIrqlToDpcLevel');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRaiseIrqlToSynchLevel(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRaiseIrqlToSynchLevel');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeReleaseMutant(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeReleaseMutant');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeReleaseSemaphore(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeReleaseSemaphore');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveByKeyDeviceQueue(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRemoveByKeyDeviceQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveDeviceQueue(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRemoveDeviceQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveEntryDeviceQueue(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRemoveEntryDeviceQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveQueue(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRemoveQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRemoveQueueDpc(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRemoveQueueDpc');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeResetEvent(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeResetEvent');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRestoreFloatingPointState(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRestoreFloatingPointState');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeResumeThread(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeResumeThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeRundownQueue(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeRundownQueue');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSaveFloatingPointState(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSaveFloatingPointState');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetBasePriorityThread(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSetBasePriorityThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetDisableBoostThread(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSetDisableBoostThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetEvent(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSetEvent');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetEventBoostPriority(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSetEventBoostPriority');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetPriorityProcess(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSetPriorityProcess');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetPriorityThread(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSetPriorityThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetTimer(
  Timer: PKTIMER;
  DueTime: LARGE_INTEGER;
  Dpc: PKDPC // OPTIONAL
  ): LONGBOOL; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

	DbgPrintf('EmuKrnl : KeSetTimer' +
		   #13#10'(' +
		   #13#10'   Timer               : 0x%.08X' +
		   #13#10'   DueTime             : 0x%.16X' + // was %I64X
		   #13#10'   Dpc                 : 0x%.08X' +
		   #13#10');',
		   [Timer, DueTime.QUADPART, Dpc]);

  // Call KeSetTimerEx
  xboxkrnl_KeSetTimerEx(Timer, DueTime, 0, Dpc);

  Result := True;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSetTimerEx(
  Timer: PKTIMER;
  DueTime: LARGE_INTEGER;
  Period: LONG; // OPTIONAL
  Dpc: PKDPC // OPTIONAL
  ): LONGBOOL; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuKrnl : KeSetTimerEx' +
           #13#10'(' +
           #13#10'   Timer               : 0x%.08X' +
           #13#10'   DueTime             : 0x%.16X' + // was %I64X
           #13#10'   Period              : 0x%.08X' +
           #13#10'   Dpc                 : 0x%.08X' +
           #13#10');',
           [Timer, DueTime.QUADPART, Period, Dpc]);

  CxbxKrnlCleanup('KeSetTimerEx is not implemented');

  EmuSwapFS(fsXbox);

  Result := TRUE;
end;

function xboxkrnl_KeStallExecutionProcessor(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeStallExecutionProcessor');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSuspendThread(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSuspendThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSynchronizeExecution(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSynchronizeExecution');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeSystemTime(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeSystemTime');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeTestAlertThread(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeTestAlertThread');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeTimeIncrement(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeTimeIncrement');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeWaitForMultipleObjects(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeWaitForMultipleObjects');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_KeWaitForSingleObject(): NTSTATUS; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('KeWaitForSingleObject');
  EmuSwapFS(fsXbox);
end;

end.
