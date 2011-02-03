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
  u_About in '..\..\src2\Tools\XdkTracker\src\u_About.pas' {frm_About},
  ufrm_Main in '..\..\src2\Tools\XdkTracker\src\ufrm_Main.pas' {frmXdkTracker},
  uPublisher in '..\..\src2\Tools\XdkTracker\src\uPublisher.pas' {frm_Publisher},
  uImportGames in '..\..\src2\Tools\XdkTracker\src\uImportGames.pas' {frm_XBEList},
  uData in '..\..\src2\Tools\XdkTracker\src\uData.pas',
  uConsts in '..\..\src2\uConsts.pas',
  uXbe in '..\..\src2\uXbe.pas',
  uTypes in '..\..\src2\uTypes.pas',
  uTime in '..\..\src2\uTime.pas',
  uLog in '..\..\src2\uLog.pas',
  uDxbxXml in '..\..\src2\uDxbxXml.pas' {DxbxXml: TDataModule},
  uConsoleClass in '..\..\src2\uConsoleClass.pas',
  uDxbxUtils in '..\..\src2\uDxbxUtils.pas',
  uEmuD3D8Types in '..\..\src2\DxbxKrnl\uEmuD3D8Types.pas',
  uXDVDFS in '..\..\src2\uXDVDFS.pas',
  uFileSystem in '..\..\src2\uFileSystem.pas';

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
