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
program Dxbx;

{$INCLUDE Dxbx.inc}

uses
  Forms,
  ufrm_Main in 'ufrm_Main.pas' {frm_Main},
  ufrm_ControllerConfig in 'ufrm_ControllerConfig.pas' {frm_ControllerConfig},
  ufrm_VideoConfig in 'ufrm_VideoConfig.pas' {frm_VideoConfig},
  uXbe in 'uXbe.pas',
  uEmuExe in 'uEmuExe.pas',
  uExe in 'uExe.pas',
  uConsts in 'uConsts.pas',
  ufrm_About in 'ufrm_About.pas' {frm_About},
  uProlog in 'uProlog.pas',
  uLog in 'uLog.pas',
  uLogConsole in 'uLogConsole.pas' {frm_LogConsole},
  uTime in 'uTime.pas',
  uBitsOps in 'uBitsOps.pas',
  uWindows in 'uWindows.pas',
  uTypes in 'uTypes.pas',
  uDxbxXml in 'uDxbxXml.pas' {DxbxXml: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Dxbx';
  Application.CreateForm(Tfrm_Main, frm_Main);
  Application.CreateForm(TDxbxXml, DxbxXml);
  Application.Run;
end.

