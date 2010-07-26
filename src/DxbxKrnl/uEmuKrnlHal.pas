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
unit uEmuKrnlHal;

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
  uDxbxKrnlUtils;

var {040}xboxkrnl_HalDiskCachePartitionCount: DWORD = 4; // This enables Partition3..7 TODO -oDxbx : Make this configurable
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:100

var {041}xboxkrnl_HalDiskModelNumber: ANSI_STRING; // TODO -oDxbx: Fill this with something sensible
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:100

var {042}xboxkrnl_HalDiskSerialNumber: ANSI_STRING; // TODO -oDxbx: Fill this with something sensible
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:100

// TODO -oDxbx : Make this configurable
var {356}xboxkrnl_HalBootSMCVideoMode: DWORD = SMC_VIDEO_MODE_VMODE0;
// Source:Cxbx  Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100

var HalpSMCScratchRegister: DWORD;
// Source:Dxbx  Translator:PatrickvL  Done:100


type
  PHAL_SHUTDOWN_REGISTRATION = ^HAL_SHUTDOWN_REGISTRATION;

  PHAL_SHUTDOWN_NOTIFICATION = procedure(ShutdownRegistration: PHAL_SHUTDOWN_REGISTRATION);

  _HAL_SHUTDOWN_REGISTRATION = record
    NotificationRoutine: PHAL_SHUTDOWN_NOTIFICATION;
    Priority: LONG;
    ListEntry: LIST_ENTRY;
  end;
  HAL_SHUTDOWN_REGISTRATION = _HAL_SHUTDOWN_REGISTRATION;

const
  SMC_ADDRESS = 0; // TODO : What value should this have?

  SMC_COMMAND_SCRATCH = 0; // TODO : What value should this have?


procedure {009} xboxkrnl_HalReadSMCTrayState(
  State: PDWORD;
  Count: PDWORD
  ); stdcall;
procedure {038} xboxkrnl_HalClearSoftwareInterrupt(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  Request: KIRQL
  ); register;
function {039} xboxkrnl_HalDisableSystemInterrupt(
  Vector: ULONG;
  Irql: KIRQL
  ): LONGBOOL; stdcall;
function {043} xboxkrnl_HalEnableSystemInterrupt(
  Vector: ULONG;
  Irql: KIRQL;
  InterruptMode: KINTERRUPT_MODE
  ): LONGBOOL; stdcall;
function {044} xboxkrnl_HalGetInterruptVector(
  InterfaceType: INTERFACE_TYPE;
  BusNumber: ULONG;
  BusInterruptLevel: ULONG;
  BusInterruptVector: ULONG;
  Irql: PKIRQL;
  Affinity: PKAFFINITY
  ): ULONG; stdcall;
procedure {045} xboxkrnl_HalReadSMBusValue(
  Address: UCHAR;
  Command: UCHAR;
  WordFlag: _BOOLEAN;
  Value: PULONG
  ); stdcall;
procedure {046} xboxkrnl_HalReadWritePCISpace(
  BusNumber: ULONG;
  SlotNumber: ULONG;
  RegisterNumber: ULONG;
  Buffer: PVOID;
  Length: ULONG;
  WritePCISpace: LONGBOOL
  ); stdcall;
function {047} xboxkrnl_HalRegisterShutdownNotification(
  ShutdownRegistration: PHAL_SHUTDOWN_REGISTRATION;
  Register_: _BOOLEAN
  ): NTSTATUS; stdcall;
procedure {048} xboxkrnl_HalRequestSoftwareInterrupt(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  Request: KIRQL
  ); register;
procedure {049} xboxkrnl_HalReturnToFirmware(
  Routine: RETURN_FIRMWARE
  ); stdcall;
function {050} xboxkrnl_HalWriteSMBusValue(
  Address: UCHAR;
  Command: UCHAR;
  WordFlag: _BOOLEAN;
  Value: ULONG
  ): ULONG; stdcall;
function {358} xboxkrnl_HalIsResetOrShutdownPending(): _BOOLEAN; stdcall;
function {360} xboxkrnl_HalInitiateShutdown(): NTSTATUS; stdcall;
procedure {365} xboxkrnl_HalEnableSecureTrayEject(
  ); stdcall;
function {366} xboxkrnl_HalWriteSMCScratchRegister(
  ScratchRegister: DWORD
  ): NTSTATUS; stdcall;

implementation

procedure {009} xboxkrnl_HalReadSMCTrayState
(
  State: PDWORD;
  Count: PDWORD
); stdcall;
// Source:Cxbx  Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
const TRAY_CLOSED_MEDIA_PRESENT = 96;
const TRAY_CLOSED_NO_MEDIA = 64;
const TRAY_OPEN = 16;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : HalReadSMCTrayState' +
      #13#10'(' +
      #13#10'   State              : 0x%.08X' +
      #13#10'   Count              : 0x%.08X' +
      #13#10');',
      [State, Count]);
{$ENDIF}

  // TODO -oCXBX: Make this configurable?
  // TODO -oCXBX: What is the count parameter for??

  State^ := TRAY_CLOSED_NO_MEDIA;
//  Count^ := 1;

  EmuSwapFS(fsXbox);
end;

procedure {038} xboxkrnl_HalClearSoftwareInterrupt(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}Request: KIRQL
  ); register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('HalClearSoftwareInterrupt');
  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

function {039} xboxkrnl_HalDisableSystemInterrupt(
  Vector: ULONG;
  Irql: KIRQL
  ): LONGBOOL; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('HalDisableSystemInterrupt');
  Result := False;
  EmuSwapFS(fsXbox);
end;

function {043} xboxkrnl_HalEnableSystemInterrupt(
  Vector: ULONG;
  Irql: KIRQL;
  InterruptMode: KINTERRUPT_MODE
  ): LONGBOOL; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('HalEnableSystemInterrupt');
  Result := False;
  EmuSwapFS(fsXbox);
end;

function {044} xboxkrnl_HalGetInterruptVector(
  InterfaceType: INTERFACE_TYPE;
  BusNumber: ULONG;
  BusInterruptLevel: ULONG;
  BusInterruptVector: ULONG;
  Irql: PKIRQL;
  Affinity: PKAFFINITY
  ): ULONG; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('HalGetInterruptVector');
  EmuSwapFS(fsXbox);
end;


procedure {045} xboxkrnl_HalReadSMBusValue(
  Address: UCHAR;
  Command: UCHAR;
  WordFlag: _BOOLEAN;
  Value: PULONG
  ); stdcall;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:1
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : HalReadSMBusValue' +
      #13#10'(' +
      #13#10'   Address             : 0x%.08X' +
      #13#10'   Command             : 0x%.08X' +
      #13#10'   WordFlag            : 0x%.08X' +
      #13#10'   Value               : 0x%.08X' +
      #13#10');',
      [Address, Command, WordFlag, Value]);
{$ENDIF}

  Unimplemented('HalReadSMBusValue');
  Value^ := 0; // TODO : Zero is probably the safest value to return

  EmuSwapFS(fsXbox);
end;

procedure {046} xboxkrnl_HalReadWritePCISpace(
  BusNumber: ULONG;
  SlotNumber: ULONG;
  RegisterNumber: ULONG;
  Buffer: PVOID;
  Length: ULONG;
  WritePCISpace: LONGBOOL
  ); stdcall;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('HalReadWritePCISpace');
  EmuSwapFS(fsXbox);
end;

function {047} xboxkrnl_HalRegisterShutdownNotification(
  ShutdownRegistration: PHAL_SHUTDOWN_REGISTRATION;
  Register_: _BOOLEAN
  ): NTSTATUS; stdcall;
// Source:APILogger - Uncertain  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('HalRegisterShutdownNotification');
  EmuSwapFS(fsXbox);
end;

procedure {048} xboxkrnl_HalRequestSoftwareInterrupt(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}Request: KIRQL
  ); register; // fastcall simulation - See Translation guide
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('HalRequestSoftwareInterrupt');
  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

procedure {049} xboxkrnl_HalReturnToFirmware
(
  Routine: RETURN_FIRMWARE
); stdcall;
// Source:OpenXDK  Branch:shogun  Revision:0.8.2-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : HalReturnToFirmware' +
      #13#10'(' +
      #13#10'   Routine             : 0x%.08X' +
      #13#10');',
      [Ord(Routine)]);
{$ENDIF}

  DxbxKrnlCleanup('Xbe has rebooted : HalReturnToFirmware(%d)', [Ord(Routine)]);

  EmuSwapFS(fsXbox); // Dxbx addition : Not really necessary
end;

function {050} xboxkrnl_HalWriteSMBusValue(
  Address: UCHAR;
  Command: UCHAR;
  WordFlag: _BOOLEAN;
  Value: ULONG
  ): ULONG; stdcall;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : HalWriteSMBusValue' +
      #13#10'(' +
      #13#10'   Address             : 0x%.08X' +
      #13#10'   Command             : 0x%.08X' +
      #13#10'   WordFlag            : 0x%.08X' +
      #13#10'   Value               : 0x%.08X' +
      #13#10');',
      [Address, Command, WordFlag, Value]);
{$ENDIF}

  Result := Unimplemented('HalWriteSMBusValue');

  EmuSwapFS(fsXbox);
end;

function {358} xboxkrnl_HalIsResetOrShutdownPending(): _BOOLEAN; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('HalIsResetOrShutdownPending');
  Result := False;
  EmuSwapFS(fsXbox);
end;

function {360} xboxkrnl_HalInitiateShutdown(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('HalInitiateShutdown');
  EmuSwapFS(fsXbox);
end;

// HalEnableSecureTrayEject:
// Notifies the SMBUS that ejecting the DVD-ROM should not reset the system.
// Note that this function can't really be called directly...
//
// New to the XBOX.
procedure {365} xboxkrnl_HalEnableSecureTrayEject(
  ); stdcall;
// Source:XBMC Undocumented.h  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('HalEnableSecureTrayEject');
  EmuSwapFS(fsXbox);
end;

function {366} xboxkrnl_HalWriteSMCScratchRegister(
  ScratchRegister: DWORD
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:50
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : HalWriteSMCScratchRegister' +
      #13#10'(' +
      #13#10'   ScratchRegister     : 0x%.08X' +
      #13#10');',
      [ScratchRegister]);
{$ENDIF}

  HalpSMCScratchRegister := ScratchRegister;

  EmuSwapFS(fsXbox);

  // TODO : Is this the way we need to set the value?
  Result := xboxkrnl_HalWriteSMBusValue(SMC_ADDRESS, SMC_COMMAND_SCRATCH, {WordFlag=}False, ScratchRegister);
end;

end.
