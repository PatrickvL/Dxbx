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
  uLog,
  uEmuFS,
  uEmuFile,
  uEmuXapi,
  uEmuKrnl,
  uDxbxKrnl,
  uDxbxKrnlUtils;
  
var {040}xboxkrnl_HalDiskCachePartitionCount: DWORD;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:100

var {041}xboxkrnl_HalDiskModelNumber: DWORD;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:100

var {042}xboxkrnl_HalDiskSerialNumber: DWORD;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:100

var {356}xboxkrnl_HalBootSMCVideoMode: DWORD;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:100

procedure {009} xboxkrnl_HalReadSMCTrayState(
  State: PDWORD;
  Count: PDWORD
  ); stdcall;
procedure {038} xboxkrnl_HalClearSoftwareInterrupt(
  Request: KIRQL
  ); stdcall;
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
  BusNumber: ULONG;
  SlotNumber: ULONG;
  RegisterNumber: ULONG;
  Buffer: PVOID;
  Length: ULONG;
  WritePCISpace: LONGBOOL
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
  Arg1: UNKNOWN;
  Arg2: UNKNOWN
  ): NTSTATUS; stdcall;
procedure {048} xboxkrnl_HalRequestSoftwareInterrupt(
  Request: KIRQL
  ); stdcall;
procedure {049} xboxkrnl_HalReturnToFirmware(
  Routine: RETURN_FIRMWARE
  ); stdcall;
function {050} xboxkrnl_HalWriteSMBusValue(
  Address: UCHAR;
  Command: UCHAR;
  WordFlag: BOOLEAN;
  Value: ULONG
  ): ULONG; stdcall;
function {358} xboxkrnl_HalIsResetOrShutdownPending(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function {360} xboxkrnl_HalInitiateShutdown(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure {365} xboxkrnl_HalEnableSecureTrayEject(
  ); stdcall;
function {366} xboxkrnl_HalWriteSMCScratchRegister(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

procedure {009} xboxkrnl_HalReadSMCTrayState(
  State: PDWORD;
  Count: PDWORD
  ); stdcall;
// Source:OpenXdk  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('HalReadSMCTrayState');
  EmuSwapFS(fsXbox);
end;

procedure {038} xboxkrnl_HalClearSoftwareInterrupt(
  Request: KIRQL
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('HalClearSoftwareInterrupt');
  EmuSwapFS(fsXbox);
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
  Unimplemented('HalReadSMBusValue');
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
  Arg1: UNKNOWN;
  Arg2: UNKNOWN
  ): NTSTATUS; stdcall;
// Source:APILogger - Uncertain  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('HalRegisterShutdownNotification');
  EmuSwapFS(fsXbox);
end;

procedure {048} xboxkrnl_HalRequestSoftwareInterrupt(
  Request: KIRQL
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('HalRequestSoftwareInterrupt');
  EmuSwapFS(fsXbox);
end;

procedure {049} xboxkrnl_HalReturnToFirmware(
  Routine: RETURN_FIRMWARE
  ); stdcall;
// Source:OpenXDK  Branch:?  Translator:PatrickvL  Done:5
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuKrnl : HalReturnToFirmware' +
           #13#10'(' +
           #13#10'   Routine             : 0x%.08X' +
           #13#10');',
           [Ord(Routine)]);
{$ENDIF}

  CxbxKrnlCleanup('Xbe has rebooted : HalReturnToFirmware(%d)', [Ord(Routine)]);
end;

function {050} xboxkrnl_HalWriteSMBusValue(
  Address: UCHAR;
  Command: UCHAR;
  WordFlag: BOOLEAN; // Dxbx TODO : What should we use: LONGBOOL or WORDBOOL?
  Value: ULONG
  ): ULONG; stdcall;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('HalWriteSMBusValue');
  EmuSwapFS(fsXbox);
end;

function {358} xboxkrnl_HalIsResetOrShutdownPending(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('HalIsResetOrShutdownPending');
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

function {366} xboxkrnl_HalWriteSMCScratchRegister(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('HalWriteSMCScratchRegister');
  EmuSwapFS(fsXbox);
end;

end.
