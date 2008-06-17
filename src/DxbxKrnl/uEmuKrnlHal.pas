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

procedure xboxkrnl_HalReadSMCTrayState(
  State: PDWORD;
  Count: PDWORD
  ); stdcall; // Source: OpenXdk
function xboxkrnl_HalClearSoftwareInterrupt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalDisableSystemInterrupt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalDiskCachePartitionCount(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalDiskModelNumber(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalDiskSerialNumber(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalEnableSystemInterrupt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalGetInterruptVector(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
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
function xboxkrnl_HalRegisterShutdownNotification(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalRequestSoftwareInterrupt(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_HalReturnToFirmware(
  Routine: RETURN_FIRMWARE
  ); stdcall; // Source: OpenXDK
function xboxkrnl_HalWriteSMBusValue(
  Address: UCHAR;
  Command: UCHAR;
  WordFlag: BOOLEAN; // TODO : What should we use: LONGBOOL or WORDBOOL?
  Value: ULONG
  ): ULONG; stdcall; // Source: OpenXDK
function xboxkrnl_HalBootSMCVideoMode(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalIsResetOrShutdownPending(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalInitiateShutdown(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_HalEnableSecureTrayEject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
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

function xboxkrnl_HalClearSoftwareInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalClearSoftwareInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalDisableSystemInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalDisableSystemInterrupt');
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

function xboxkrnl_HalEnableSystemInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalEnableSystemInterrupt');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalGetInterruptVector(): NTSTATUS; stdcall;
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

function xboxkrnl_HalRegisterShutdownNotification(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalRegisterShutdownNotification');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalRequestSoftwareInterrupt(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalRequestSoftwareInterrupt');
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

function xboxkrnl_HalBootSMCVideoMode(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalBootSMCVideoMode');
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

function xboxkrnl_HalEnableSecureTrayEject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalEnableSecureTrayEject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_HalWriteSMCScratchRegister(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('HalWriteSMCScratchRegister');
  EmuSwapFS(); // Xbox FS
end;

end.

