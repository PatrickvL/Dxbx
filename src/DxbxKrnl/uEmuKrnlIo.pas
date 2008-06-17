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
unit uEmuKrnlIo;

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
  xboxkrnl_IoDeviceObjectType: POBJECT_TYPE = NULL;
  xboxkrnl_IoFileObjectType: POBJECT_TYPE = NULL;

function xboxkrnl_IoAllocateIrp(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoBuildAsynchronousFsdRequest(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoBuildDeviceIoControlRequest(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoBuildSynchronousFsdRequest(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoCheckShareAccess(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoCompletionObjectType(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoCreateDevice(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoCreateFile(
  FileHandle: PHANDLE; // out
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // out
  AllocationSize: PLARGE_INTEGER;
  FileAttributes: ULONG;
  ShareAccess: ULONG;
  Disposition: ULONG;
  CreateOptions: ULONG;
  Options: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_IoCreateSymbolicLink(
  SymbolicLinkName: PSTRING;
  DeviceName: PSTRING
  ): NTSTATUS; stdcall;
function xboxkrnl_IoDeleteDevice(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoDeleteSymbolicLink(
  SymbolicLinkName: PSTRING
  ): NTSTATUS; stdcall;
function xboxkrnl_IoFreeIrp(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoInitializeIrp(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoInvalidDeviceRequest(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoQueryFileInformation(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoQueryVolumeInformation(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoQueueThreadIrp(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoRemoveShareAccess(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoSetIoCompletion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoSetShareAccess(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoStartNextPacket(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoStartNextPacketByKey(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoStartPacket(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoSynchronousDeviceIoControlRequest(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoSynchronousFsdRequest(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IofCallDriver(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IofCompleteRequest(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoDismountVolume(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoDismountVolumeByName(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoMarkIrpMustComplete(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

function xboxkrnl_IoAllocateIrp(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoAllocateIrp');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoBuildAsynchronousFsdRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoBuildAsynchronousFsdRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoBuildDeviceIoControlRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoBuildDeviceIoControlRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoBuildSynchronousFsdRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoBuildSynchronousFsdRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoCheckShareAccess(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoCheckShareAccess');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoCompletionObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoCompletionObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoCreateDevice(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoCreateDevice');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoCreateFile(
  FileHandle: PHANDLE; // out
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // out
  AllocationSize: PLARGE_INTEGER;
  FileAttributes: ULONG;
  ShareAccess: ULONG;
  Disposition: ULONG;
  CreateOptions: ULONG;
  Options: ULONG
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoCreateFile');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoCreateSymbolicLink(
  SymbolicLinkName: PSTRING;
  DeviceName: PSTRING
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoCreateSymbolicLink');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoDeleteDevice(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoDeleteDevice');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoDeleteSymbolicLink(
  SymbolicLinkName: PSTRING
  ): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoDeleteSymbolicLink');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoFreeIrp(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoFreeIrp');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoInitializeIrp(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoInitializeIrp');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoInvalidDeviceRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoInvalidDeviceRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoQueryFileInformation(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoQueryFileInformation');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoQueryVolumeInformation(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoQueryVolumeInformation');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoQueueThreadIrp(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoQueueThreadIrp');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoRemoveShareAccess(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoRemoveShareAccess');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoSetIoCompletion(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoSetIoCompletion');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoSetShareAccess(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoSetShareAccess');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoStartNextPacket(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoStartNextPacket');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoStartNextPacketByKey(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoStartNextPacketByKey');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoStartPacket(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoStartPacket');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoSynchronousDeviceIoControlRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoSynchronousDeviceIoControlRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoSynchronousFsdRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoSynchronousFsdRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IofCallDriver(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IofCallDriver');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IofCompleteRequest(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IofCompleteRequest');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoDismountVolume(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoDismountVolume');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoDismountVolumeByName(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoDismountVolumeByName');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_IoMarkIrpMustComplete(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('IoMarkIrpMustComplete');
  EmuSwapFS(); // Xbox FS
end;

end.

