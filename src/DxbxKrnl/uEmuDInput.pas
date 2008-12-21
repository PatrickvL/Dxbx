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

unit uEmuDInput;

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
//  Dialogs,
  Windows,
  // 3rd party
  XInput,
  // Dxbx
  uError,
  uEmuShared,
  uEmu,
  uXBController;

function XTL_EmuDInputInit: bool; stdcall; // forward
procedure XTL_EmuDInputCleanup; stdcall; // forward

implementation

var
  g_XBController: XBController;

function XTL_EmuDInputInit: bool; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:98
begin
  g_EmuShared.GetXBController({var}g_XBController);

  g_XBController.ListenBegin(g_hEmuWindow);

  if Assigned(g_XBController.GetError()) then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

procedure XTL_EmuDInputCleanup; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  g_XBController.ListenEnd();
end;

procedure XTL_EmuDInputPoll(Controller: PXINPUT_STATE); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  g_XBController.ListenPoll(Controller);
  if Assigned(g_XBController.GetError()) then
    MessageBox(0, g_XBController.GetError(), 'Dxbx [*UNHANDLED!*]', MB_OK);  // Cxbx TODO: Handle this!
end;

exports
  XTL_EmuDInputCleanup,
  XTL_EmuDInputInit,
  XTL_EmuDInputPoll;

end.

