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

var
  xboxkrnl_HalBootSMCVideoMode: DWORD; // Source: OpenXDK

procedure xboxkrnl_HalReadSMCTrayState(
  State: PDWORD;
  Count: PDWORD
  ); stdcall; // Source: OpenXdk
procedure xboxkrnl_HalClearSoftwareInterrupt(
  Request: KIRQL
  ); stdcall; // Source: ReactOS
function xboxkrnl_HalDisableSystemInterrupt(
  Vector: ULONG;
  Irql: KIRQL
  ): LONGBOOL; stdcall; // Source: ReactOS
function xboxkrnl_HalDiskCachePartitionCount(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalDiskModelNumber(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalDiskSerialNumber(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalEnableSystemInterrupt(
  Vector: ULONG;
  Irql: KIRQL;
  InterruptMode: KINTERRUPT_MODE
  ): LONGBOOL; stdcall; // Source: ReactOS
function xboxkrnl_HalGetInterruptVector(
  InterfaceType: INTERFACE_TYPE;
  BusNumber: ULONG;
  BusInterruptLevel: ULONG;
  BusInterruptVector: ULONG;
  Irql: PKIRQL;
  Affinity: PKAFFINITY
  ): ULONG; stdcall; // Source: ReactOS
procedure xboxkrnl_HalReadSMBusValue(
  BusNumber: ULONG;
  SlotNumber: ULONG;
  RegisterNumber: ULONG;
  Buffer: PVOID;
  Length: ULONG;
  WritePCISpace: LONGBOOL
  ); stdcall; // Source: OpenXDK
procedure xboxkrnl_HalReadWritePCISpace(
  BusNumber: ULONG;
  SlotNumber: ULONG;
  RegisterNumber: ULONG;
  Buffer: PVOID;
  Length: ULONG;
  WritePCISpace: LONGBOOL
  ); stdcall; // Source: OpenXDK
function xboxkrnl_HalRegisterShutdownNotification(
  Arg1: UNKNOWN;
  Arg2: UNKNOWN
  ): NTSTATUS; stdcall; // Source: APILogger - Uncertain
procedure xboxkrnl_HalRequestSoftwareInterrupt(
  Request: KIRQL
  ); stdcall; // Source: ReactOS
procedure xboxkrnl_HalReturnToFirmware(
  Routine: RETURN_FIRMWARE
  ); stdcall; // Source: OpenXDK
function xboxkrnl_HalWriteSMBusValue(
  Address: UCHAR;
  Command: UCHAR;
  WordFlag: BOOLEAN; // TODO : What should we use: LONGBOOL or WORDBOOL?
  Value: ULONG
  ): ULONG; stdcall; // Source: OpenXDK
function xboxkrnl_HalIsResetOrShutdownPending(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalInitiateShutdown(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_HalEnableSecureTrayEject(
  ); stdcall; // Source: XBMC Undocumented.h
function xboxkrnl_HalWriteSMCScratchRegister(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

procedure xboxkrnl_HalReadSMCTrayState(
  State: PDWORD;
  Count: PDWORD
  ); stdcall; // Source: OpenXdk
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('HalReadSMCTrayState');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_HalClearSoftwareInterrupt(
  Request: KIRQL
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('HalClearSoftwareInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalDisableSystemInterrupt(
  Vector: ULONG;
  Irql: KIRQL
  ): LONGBOOL; stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('HalDisableSystemInterrupt');
  Result := False;
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalDiskCachePartitionCount(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalDiskCachePartitionCount');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalDiskModelNumber(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalDiskModelNumber');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalDiskSerialNumber(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalDiskSerialNumber');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalEnableSystemInterrupt(
  Vector: ULONG;
  Irql: KIRQL;
  InterruptMode: KINTERRUPT_MODE
  ): LONGBOOL; stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('HalEnableSystemInterrupt');
  Result := False;
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalGetInterruptVector(
  InterfaceType: INTERFACE_TYPE;
  BusNumber: ULONG;
  BusInterruptLevel: ULONG;
  BusInterruptVector: ULONG;
  Irql: PKIRQL;
  Affinity: PKAFFINITY
  ): ULONG; stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalGetInterruptVector');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_HalReadSMBusValue(
  BusNumber: ULONG;
  SlotNumber: ULONG;
  RegisterNumber: ULONG;
  Buffer: PVOID;
  Length: ULONG;
  WritePCISpace: LONGBOOL
  ); stdcall; // Source: OpenXDK
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('HalReadSMBusValue');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_HalReadWritePCISpace(
  BusNumber: ULONG;
  SlotNumber: ULONG;
  RegisterNumber: ULONG;
  Buffer: PVOID;
  Length: ULONG;
  WritePCISpace: LONGBOOL
  ); stdcall; // Source: OpenXDK
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('HalReadWritePCISpace');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalRegisterShutdownNotification(
  Arg1: UNKNOWN;
  Arg2: UNKNOWN
  ): NTSTATUS; stdcall; // Source: APILogger - Uncertain
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalRegisterShutdownNotification');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_HalRequestSoftwareInterrupt(
  Request: KIRQL
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('HalRequestSoftwareInterrupt');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_HalReturnToFirmware(
  Routine: RETURN_FIRMWARE
  ); stdcall; // Source: OpenXDK
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('HalReturnToFirmware');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalWriteSMBusValue(
  Address: UCHAR;
  Command: UCHAR;
  WordFlag: BOOLEAN; // TODO : What should we use: LONGBOOL or WORDBOOL?
  Value: ULONG
  ): ULONG; stdcall; // Source: OpenXDK
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalWriteSMBusValue');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalIsResetOrShutdownPending(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalIsResetOrShutdownPending');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalInitiateShutdown(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalInitiateShutdown');
  EmuSwapFS(); // Xbox FS
end;

// HalEnableSecureTrayEject:
// Notifies the SMBUS that ejecting the DVD-ROM should not reset the system.
// Note that this function can't really be called directly...
//
// New to the XBOX.
procedure xboxkrnl_HalEnableSecureTrayEject(
  ); stdcall; // Source: XBMC Undocumented.h
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('HalEnableSecureTrayEject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalWriteSMCScratchRegister(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalWriteSMCScratchRegister');
  EmuSwapFS(); // Xbox FS
end;

end.

