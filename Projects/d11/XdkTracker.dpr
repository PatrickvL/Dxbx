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
  u_About in '..\..\src\Tools\XdkTracker\src\u_About.pas' {frm_About},
  ufrm_Main in '..\..\src\Tools\XdkTracker\src\ufrm_Main.pas' {frmXdkTracker},
  uPublisher in '..\..\src\Tools\XdkTracker\src\uPublisher.pas' {frm_Publisher},
  uImportGames in '..\..\src\Tools\XdkTracker\src\uImportGames.pas' {frm_XBEList},
  uData in '..\..\src\Tools\XdkTracker\src\uData.pas',
  uConsts in '..\..\src\uConsts.pas',
  uXbe in '..\..\src\uXbe.pas',
  uTypes in '..\..\src\uTypes.pas',
  uTime in '..\..\src\uTime.pas',
  uLog in '..\..\src\uLog.pas',
  uDxbxXml in '..\..\src\uDxbxXml.pas' {DxbxXml: TDataModule},
  uConsoleClass in '..\..\src\uConsoleClass.pas',
  uDxbxUtils in '..\..\src\uDxbxUtils.pas',
  uEmuD3D8Types in '..\..\src\DxbxKrnl\uEmuD3D8Types.pas';

{$R *.res}

// Remove relocation table (generates smaller executables) :
// (See http://hallvards.blogspot.com/2006/09/hack12-create-smaller-exe-files.html)
{$SetPEFlags 1} // 1 = Windows.IMAGE_FILE_RELOCS_STRIPPED

begin
  Application.Initialize;
  Application.Title := 'XDK Tracker';
  DumpToolString := Application.Title + ' (Version ' + _XDK_TRACKER_VERSION + ')';
  Application.CreateForm(TfrmXdkTracker, frmXdkTracker);
  Application.CreateForm(TDxbxXml, DxbxXml);
  Application.Run;
end.
