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
  uDxbxKrnl;

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
function xboxkrnl_KeInitializeDpc(
  Dpc: PKDPC;
  DeferredRoutine: PKDEFERRED_ROUTINE;
  DeferredContext: PVOID
  ): NTSTATUS; stdcall;
function xboxkrnl_KeInitializeEvent(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInitializeInterrupt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInitializeMutant(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInitializeQueue(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInitializeSemaphore(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_KeInitializeTimerEx(
  Timer: PKTIMER;
  _Type: TIMER_TYPE
  ): NTSTATUS; stdcall;
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
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeAlertResumeThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeAlertThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeAlertThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeBoostPriorityThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeBoostPriorityThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeBugCheck(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeBugCheck');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeBugCheckEx(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeBugCheckEx');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeCancelTimer(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeCancelTimer');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeConnectInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeConnectInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeDelayExecutionThread(
  WaitMode: KPROCESSOR_MODE;
  Alertable: LONGBOOL;
  Interval: PLARGE_INTEGER
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeDelayExecutionThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeDisconnectInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeDisconnectInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeEnterCriticalRegion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeEnterCriticalRegion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeGetCurrentIrql(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeGetCurrentIrql');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeGetCurrentThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeGetCurrentThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeApc(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeApc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeDpc(
  Dpc: PKDPC;
  DeferredRoutine: PKDEFERRED_ROUTINE;
  DeferredContext: PVOID
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeDpc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeMutant(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeMutant');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeSemaphore(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeSemaphore');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInitializeTimerEx(
  Timer: PKTIMER;
  _Type: TIMER_TYPE
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInitializeTimerEx');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertByKeyDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertByKeyDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertHeadQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertHeadQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertQueueApc(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertQueueApc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInsertQueueDpc(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInsertQueueDpc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeInterruptTime(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeInterruptTime');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeIsExecutingDpc(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeIsExecutingDpc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeLeaveCriticalRegion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeLeaveCriticalRegion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KePulseEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KePulseEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeQueryBasePriorityThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeQueryBasePriorityThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeQueryInterruptTime(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeQueryInterruptTime');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeQueryPerformanceCounter(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeQueryPerformanceCounter');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeQueryPerformanceFrequency(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeQueryPerformanceFrequency');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_KeQuerySystemTime(
  CurrentTime: PLARGE_INTEGER
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('KeQuerySystemTime');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRaiseIrqlToDpcLevel(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRaiseIrqlToDpcLevel');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRaiseIrqlToSynchLevel(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRaiseIrqlToSynchLevel');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeReleaseMutant(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeReleaseMutant');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeReleaseSemaphore(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeReleaseSemaphore');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRemoveByKeyDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRemoveByKeyDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRemoveDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRemoveDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRemoveEntryDeviceQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRemoveEntryDeviceQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRemoveQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRemoveQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRemoveQueueDpc(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRemoveQueueDpc');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeResetEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeResetEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRestoreFloatingPointState(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRestoreFloatingPointState');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeResumeThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeResumeThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeRundownQueue(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeRundownQueue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSaveFloatingPointState(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSaveFloatingPointState');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetBasePriorityThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetBasePriorityThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetDisableBoostThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetDisableBoostThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetEvent(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetEvent');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetEventBoostPriority(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetEventBoostPriority');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetPriorityProcess(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetPriorityProcess');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetPriorityThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSetPriorityThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetTimer(
  Timer: PKTIMER;
  DueTime: LARGE_INTEGER;
  Dpc: PKDPC // OPTIONAL
  ): LONGBOOL; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('KeSetTimer');
  Result := False;
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSetTimerEx(
  Timer: PKTIMER;
  DueTime: LARGE_INTEGER;
  Period: LONG; // OPTIONAL
  Dpc: PKDPC // OPTIONAL
  ): LONGBOOL; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('KeSetTimerEx');
  Result := Low(Result);
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeStallExecutionProcessor(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeStallExecutionProcessor');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSuspendThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSuspendThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSynchronizeExecution(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSynchronizeExecution');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeSystemTime(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeSystemTime');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeTestAlertThread(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeTestAlertThread');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeTimeIncrement(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeTimeIncrement');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeWaitForMultipleObjects(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeWaitForMultipleObjects');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_KeWaitForSingleObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('KeWaitForSingleObject');
  EmuSwapFS(); // Xbox FS
end;

end.
