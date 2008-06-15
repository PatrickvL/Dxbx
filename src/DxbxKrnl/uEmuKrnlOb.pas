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
unit uEmuKrnlOb;

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

function xboxkrnl_ObCreateObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObDirectoryObjectType(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObInsertObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObMakeTemporaryObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObOpenObjectByName(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObOpenObjectByPointer(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObpObjectHandleTable(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObReferenceObjectByHandle(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObReferenceObjectByName(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObReferenceObjectByPointer(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObSymbolicLinkObjectType(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObfDereferenceObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObfReferenceObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

function xboxkrnl_ObCreateObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObCreateObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObDirectoryObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObDirectoryObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObInsertObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObInsertObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObMakeTemporaryObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObMakeTemporaryObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObOpenObjectByName(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObOpenObjectByName');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObOpenObjectByPointer(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObOpenObjectByPointer');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObpObjectHandleTable(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObpObjectHandleTable');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObReferenceObjectByHandle(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObReferenceObjectByHandle');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObReferenceObjectByName(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObReferenceObjectByName');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObReferenceObjectByPointer(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObReferenceObjectByPointer');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObSymbolicLinkObjectType(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObSymbolicLinkObjectType');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObfDereferenceObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObfDereferenceObject');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_ObfReferenceObject(): NTSTATUS; stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('ObfReferenceObject');
  EmuSwapFS(); // Xbox FS
end;

end.

