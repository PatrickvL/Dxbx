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
unit uEmuKrnlDbg;

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

procedure xboxkrnl_DbgBreakPoint(
  ); stdcall; // Source: JwaNative.pas
procedure xboxkrnl_DbgBreakPointWithStatus(
  Status: ULONG
  ); stdcall; // Source: JwaNative.pas
function xboxkrnl_DbgLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ): NTSTATUS; stdcall; // Source: ReactOS
function xboxkrnl_DbgPrint(
  Format: PCCH;
  Args: array of const // TODO : Check if this is a correct translation of '...'
  ): ULONG; stdcall; // Source: ReactOS - Uncertain
function xboxkrnl_DbgPrompt(
  Prompt: PCCH;
  Response: PCH; // OUT
  MaximumResponseLength: ULONG
  ): ULONG; stdcall; // Source: ReactOS
procedure xboxkrnl_DbgUnLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ); stdcall; // Source: ReactOS

implementation

procedure xboxkrnl_DbgBreakPoint(
  ); stdcall; // Source: JwaNative.pas
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('DbgBreakPoint');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_DbgBreakPointWithStatus(
  Status: ULONG
  ); stdcall; // Source: JwaNative.pas
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('DbgBreakPointWithStatus');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_DbgLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ): NTSTATUS; stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('DbgLoadImageSymbols');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_DbgPrint(
  Format: PCCH;
  Args: array of const // TODO : Check if this is a correct translation of '...'
  ): ULONG; stdcall; // Source: ReactOS - Uncertain
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('DbgPrint');
  EmuSwapFS(); // Xbox FS
end;

function xboxkrnl_DbgPrompt(
  Prompt: PCCH;
  Response: PCH; // OUT
  MaximumResponseLength: ULONG
  ): ULONG; stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Result := Unimplemented('DbgPrompt');
  EmuSwapFS(); // Xbox FS
end;

procedure xboxkrnl_DbgUnLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ); stdcall; // Source: ReactOS
begin
  EmuSwapFS(); // Win2k/XP FS
  Unimplemented('DbgUnLoadImageSymbols');
  EmuSwapFS(); // Xbox FS
end;

end.

