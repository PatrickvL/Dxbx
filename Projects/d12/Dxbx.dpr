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
  SysUtils,
  Forms,
  uEmuExe in '..\..\src\uEmuExe.pas',
  uEmuShared in '..\..\src\uEmuShared.pas',
  uError in '..\..\src\uError.pas',
  uExe in '..\..\src\uExe.pas',
  ufrm_About in '..\..\src\ufrm_About.pas',
  ufrm_ControllerConfig in '..\..\src\ufrm_ControllerConfig.pas',
  ufrm_Main in '..\..\src\ufrm_Main.pas',
  ufrm_VideoConfig in '..\..\src\ufrm_VideoConfig.pas',
  uLog in '..\..\src\uLog.pas',
  uMutex in '..\..\src\uMutex.pas',
  uProlog in '..\..\src\uProlog.pas',
  uTime in '..\..\src\uTime.pas',
  uTypes in '..\..\src\uTypes.pas',
  uWindows in '..\..\src\uWindows.pas',
  uXBController in '..\..\src\uXBController.pas',
  uXbe in '..\..\src\uXbe.pas',
  uXbeConvert in '..\..\src\uXbeConvert.pas',
  uXbVideo in '..\..\src\uXbVideo.pas',
  uConsoleClass in '..\..\src\uConsoleClass.pas',
  uConsts in '..\..\src\uConsts.pas',
  uDxbxXml in '..\..\src\uDxbxXml.pas',
  XboxKrnl in '..\..\Libraries\OpenXDK\include\xboxkrnl\XboxKrnl.pas',
  uDxbxUtils in '..\..\src\uDxbxUtils.pas';

{$R *.RES}

// Remove relocation table (generates smaller executables) :
// (See http://hallvards.blogspot.com/2006/09/hack12-create-smaller-exe-files.html)
{$SetPEFlags 1} // 1 = Windows.IMAGE_FILE_RELOCS_STRIPPED

var
  XBEFilePath: string;
  Xbe: TXbe;
  tmpstr1, tmpstr2: string;
begin
  Application.Initialize;
  Application.Title := 'Dxbx';
  DumpToolString := Application.Title + ' (Version ' + _DXBX_VERSION + ')';

  Application.CreateForm(Tfrm_Main, frm_Main);
  XBEFilePath := ParamStr(1);

  if  (XBEFilePath <> '')
  and SameText(ExtractFileExt(XBEFilePath), '.xbe')
  and FileExists(XBEFilePath) then
  begin
    Xbe := nil; // prevent warning
    if ConvertXbeToExe(XBEFilePath, tmpstr1, tmpstr2, Xbe, 0{=No WindowHandle!}) then
      Exit;
    // TODO : Error logging should go here
  end;

  Application.CreateForm(TDxbxXml, DxbxXml);
  Application.Run;
end.

