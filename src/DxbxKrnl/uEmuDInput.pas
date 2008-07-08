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
  Dialogs,
  // 3rd party
  XInput,
  // Dxbx
  uError,
  uEmuShared,
  uEmu,
  uXBController;


procedure XTL_EmuDInputCleanup;


implementation

var
  g_XBController: XBController;

// func: XTL::EmuDInputInit    
function XTL_EmuDInputInit: Longbool;
begin
  Result := True;
  g_EmuShared.GetXBController(g_XBController);

  g_XBController.ListenBegin(g_hEmuWindow);

  if(Error_GetError <> '') then
    result:= false;
end;

// func: XTL::EmuDInputCleanup
procedure XTL_EmuDInputCleanup;
begin
  g_XBController.ListenEnd();
end;

// func: XTL::EmuPollController
procedure XTL_EmuDInputPoll(Controller: XINPUT_STATE);
begin
  (*g_XBController.ListenPoll(Controller);
  *)

  if (Error_GetError <> '' ) then
    ShowMessage( 'Dxbx[* UNHANDLED! *]'); // TODO: Handle this! *)
end;

end.

