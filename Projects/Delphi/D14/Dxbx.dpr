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

{$R 'DxbxResources.res' '..\..\..\Resources\DxbxResources.rc'}
{$R 'SvnRevision.res' '..\..\..\Resources\SvnRevision.rc'}

uses
  SysUtils,
  Forms,
  uEmuShared in '..\..\..\Source\Delphi\src\uEmuShared.pas',
  uError in '..\..\..\Source\Delphi\src\uError.pas',
  ufrm_About in '..\..\..\Source\Delphi\src\ufrm_About.pas' {frm_About},
  ufrm_Main in '..\..\..\Source\Delphi\src\ufrm_Main.pas' {frm_Main},
  uLog in '..\..\..\Source\Delphi\src\uLog.pas',
  VistaIconFix in '..\..\..\Source\Delphi\src\VistaIconFix.pas',
  uXDVDFS in '..\..\..\Source\Delphi\src\uXDVDFS.pas',
  uXbVideo in '..\..\..\Source\Delphi\src\uXbVideo.pas',
  uXbSound in '..\..\..\Source\Delphi\src\uXbSound.pas',
  uMutex in '..\..\..\Source\Delphi\src\uMutex.pas',
  uXBController in '..\..\..\Source\Delphi\src\uXBController.pas',
  uWindows in '..\..\..\Source\Delphi\src\uWindows.pas',
  uTypes in '..\..\..\Source\Delphi\src\uTypes.pas',
  uImportGames in '..\..\..\Source\Delphi\src\uImportGames.pas' {frm_XBEList},
  ufrm_Configuration in '..\..\..\Source\Delphi\src\ufrm_Configuration.pas' {fmConfiguration},
  uFileSystem in '..\..\..\Source\Delphi\src\uFileSystem.pas',
  uConsoleClass in '..\..\..\Source\Delphi\src\uConsoleClass.pas',
  uConsts in '..\..\..\Source\Delphi\src\uConsts.pas',
  uDxbxUtils in '..\..\..\Source\Delphi\src\uDxbxUtils.pas',
  uEmuD3D8Types in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuD3D8Types.pas',
  uData in '..\..\..\Source\Delphi\src\uData.pas',
  uXbe in '..\..\..\Source\Delphi\src\uXbe.pas',
  uTime in '..\..\..\Source\Delphi\src\uTime.pas',
  uDxbxXml in '..\..\..\Source\Delphi\src\uDxbxXml.pas' {DxbxXml: TDataModule};

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
  DummyStr: string;
begin
  XBEFilePath := ParamStr(2);

  if (ParamStr(2) = '/SymbolScanOnly') and  (ParamStr(3) <> '') then
    XBEFilePath := ParamStr(3);

  // Check if Dxbx is invoked with the intention to map&run a supplied image (if not, we'll show the GUI) :
  if  (XBEFilePath <> '')
  and SameText(ParamStr(1), '/load')
  and Drives.D.OpenImage(XBEFilePath, {out}DummyStr) then // out ignored for now
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

