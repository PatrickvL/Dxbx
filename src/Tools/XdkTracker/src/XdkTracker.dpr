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
program XdkTracker;

uses
  Forms,
  u_About in 'u_About.pas' {frm_About},
  ufrm_Main in 'ufrm_Main.pas' {frmXdkTracker},
  u_xdkversions in 'u_xdkversions.pas' {frm_Xdkversion},
  uPublisher in 'uPublisher.pas' {frm_Publisher},
  uImportGames in 'uImportGames.pas' {frm_ImportGames},
  uData in 'uData.pas',
  uConsts in '..\..\..\uConsts.pas',
  uXbe in '..\..\..\uXbe.pas',
  uTypes in '..\..\..\uTypes.pas',
  uTime in '..\..\..\uTime.pas',
  uLog in '..\..\..\uLog.pas',
  uLogConsole in '..\..\..\uLogConsole.pas' {frm_LogConsole},
  uDxbxXml in '..\..\..\uDxbxXml.pas' {DxbxXml: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'XDK Tracker';
  Application.CreateForm(TfrmXdkTracker, frmXdkTracker);
  Application.CreateForm(Tfrm_LogConsole, frm_LogConsole);
  Application.CreateForm(TDxbxXml, DxbxXml);
  Application.Run;
end.
