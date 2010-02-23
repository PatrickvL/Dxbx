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

procedure {005} xboxkrnl_DbgBreakPoint(
  ); stdcall;
procedure {006} xboxkrnl_DbgBreakPointWithStatus(
  Status: ULONG
  ); stdcall;
function {007} xboxkrnl_DbgLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ): NTSTATUS; stdcall;
function {008} xboxkrnl_DbgPrint(
  Format: PCCH;
  Args: array of const
  ): ULONG; stdcall;
function {010} xboxkrnl_DbgPrompt(
  Prompt: PCCH;
  Response: PCH; // OUT
  MaximumResponseLength: ULONG
  ): ULONG; stdcall;
procedure {011} xboxkrnl_DbgUnLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ); stdcall;

implementation

procedure {005} xboxkrnl_DbgBreakPoint(
  ); stdcall;
// Source:JwaNative.pas  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('DbgBreakPoint');
  EmuSwapFS(fsXbox);
end;

procedure {006} xboxkrnl_DbgBreakPointWithStatus(
  Status: ULONG
  ); stdcall;
// Source:JwaNative.pas  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('DbgBreakPointWithStatus');
  EmuSwapFS(fsXbox);
end;

function {007} xboxkrnl_DbgLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ): NTSTATUS; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('DbgLoadImageSymbols');
  EmuSwapFS(fsXbox);
end;

function {008} xboxkrnl_DbgPrint(
  Format: PCCH;
  Args: array of const // Dxbx TODO : Check if this is a correct translation of '...'
  ): ULONG; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('DbgPrint');
  EmuSwapFS(fsXbox);
end;

function {010} xboxkrnl_DbgPrompt(
  Prompt: PCCH;
  Response: PCH; // OUT
  MaximumResponseLength: ULONG
  ): ULONG; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('DbgPrompt');
  EmuSwapFS(fsXbox);
end;

procedure {011} xboxkrnl_DbgUnLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('DbgUnLoadImageSymbols');
  EmuSwapFS(fsXbox);
end;

end.
