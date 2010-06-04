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

{$R 'DxbxResources.res' '..\..\resource\DxbxResources.rc'}

uses
  SysUtils,
  Forms,
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
  uXbVideo in '..\..\src\uXbVideo.pas',
  uConsoleClass in '..\..\src\uConsoleClass.pas',
  uConsts in '..\..\src\uConsts.pas',
  uDxbxXml in '..\..\src\uDxbxXml.pas',
  XboxKrnl in '..\..\Libraries\OpenXDK\include\xboxkrnl\XboxKrnl.pas',
  uDxbxUtils in '..\..\src\uDxbxUtils.pas',
  uEmuD3D8Types in '..\..\src\DxbxKrnl\uEmuD3D8Types.pas',
  uXDVDFS in '..\..\src\uXDVDFS.pas',
  uFileSystem in '..\..\src\uFileSystem.pas',
  uData in '..\..\src\Tools\XdkTracker\src\uData.pas',
  VistaIconFix in '..\..\src\VistaIconFix.pas';

{$R *.RES}

// Reserve a block of Virtual Memory with a size equal the total memory
// available in an Xbox : 128 MB. This block has no predefined contents,
// and thus takes virtually no space in the EXE. This block makes sure that
// the whole 128MB range of Virtual Memory (from $00010000 up to $0800000)
// isn't occupied with anything else, and this fully available to our emulator.
const
  XBOX_MEMORY_SIZE = 128*1024*1024;
var
  Data: array [0..XBOX_MEMORY_SIZE-1] of Byte;

// Make sure this EXE gets loaded into the Xbox-specific Virtual Address Space :
{$IMAGEBASE $00010000} // = XBE_IMAGE_BASE

// Make sure we can't be loaded to another location either, by removing
// the relocation table. These two settings are the only way we know of,
// to reserver the Virtual Memory Range that the Xbox normally uses...
// This also removes the relocation table (and thus generates smaller executables)
// (See http://hallvards.blogspot.com/2006/09/hack12-create-smaller-exe-files.html)
{$SetPEFlags 1} // = Windows.IMAGE_FILE_RELOCS_STRIPPED

// Here, we create a static dependance on our actual emulation DLL,
// by importing the function that takes over control and start emulation :
procedure DxbxMain(const aData: Pointer; const aSize: Cardinal); stdcall; external 'DxbxKrnl';

var
  XBEFilePath: string;
begin
  XBEFilePath := ParamStr(2);

  // Check if Dxbx is invoked with the intention to map&run an XBE (if not, we'll show the GUI) :
  if  (XBEFilePath <> '')
  and SameText(ParamStr(1), '/load')
  and SameText(ExtractFileExt(XBEFilePath), '.xbe') // TODO : Add ISO support here
  and TXbe.FileExists(XBEFilePath) then
  begin
    // Transfer control to the main emulator-code inside our DLL :
    DxbxMain(@(Data[0]), SizeOf(Data));
    // Hopefully no finalization takes place after this :
    Halt(0);
  end;

  Application.Initialize;
  Application.Title := 'Dxbx';
  Application.Tag := IntPtr(@Data[0]); // Just a reference to Data so it won't get optimized away by the compiler 
  DumpToolString := Application.Title + ' (Version ' + _DXBX_VERSION + ')';
  Application.CreateForm(Tfrm_Main, frm_Main);
  Application.CreateForm(TDxbxXml, DxbxXml);
  Application.Run;
end.

