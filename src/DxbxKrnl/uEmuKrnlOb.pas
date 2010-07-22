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
  uDxbxKrnl;

var
  {240}xboxkrnl_ObDirectoryObjectType: POBJECT_TYPE = NULL; // TODO -oDxbx : What should we initialize this to?
  {245}xboxkrnl_ObpObjectHandleTable: array [0..0] of DWord; // TODO -oDXBX: Determine array size
  {249}xboxkrnl_ObSymbolicLinkObjectType: POBJECT_TYPE = NULL; // TODO -oDxbx : Assign TEmuNtSymbolicLinkObject ?

function xboxkrnl_ObCreateObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObInsertObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObMakeTemporaryObject(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObOpenObjectByName(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObOpenObjectByPointer(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObReferenceObjectByHandle(
  Handle: HANDLE;
  ObjectType: POBJECT_TYPE; // OPTIONAL?
  Object_: PPVOID
  ): NTSTATUS; stdcall;
function xboxkrnl_ObReferenceObjectByName(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_ObReferenceObjectByPointer(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
procedure xboxkrnl_ObfDereferenceObject(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  Object_: PVOID
  ); register;
procedure xboxkrnl_ObfReferenceObject(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  Object_: PVOID
  ); register;

implementation

function xboxkrnl_ObCreateObject(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ObCreateObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_ObInsertObject(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ObInsertObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_ObMakeTemporaryObject(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ObMakeTemporaryObject');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_ObOpenObjectByName(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ObOpenObjectByName');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_ObOpenObjectByPointer(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ObOpenObjectByPointer');
  EmuSwapFS(fsXbox);
end;

// ObReferenceObjectByHandle:
// Turns a handle into a kernel object pointer.  The ObjectType parameter
// specifies what type of object it is.  This function also increments the
// object's reference count.
//
// Differences from NT: There are no DesiredAccess, AccessMode, or
//     HandleInformation parameters.
function xboxkrnl_ObReferenceObjectByHandle(
  Handle: HANDLE;
  ObjectType: POBJECT_TYPE; // OPTIONAL?
  Object_: PPVOID
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ObReferenceObjectByHandle');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_ObReferenceObjectByName(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ObReferenceObjectByName');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_ObReferenceObjectByPointer(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('ObReferenceObjectByPointer');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_ObfDereferenceObject(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}Object_: PVOID
  ); register; // fastcall simulation - See Translation guide
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('ObfDereferenceObject');
  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

procedure xboxkrnl_ObfReferenceObject(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}Object_: PVOID
  ); register; // fastcall simulation - See Translation guide
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('ObfReferenceObject');
  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

end.
