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
library DxbxKrnl;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{$INCLUDE Dxbx.inc}





{$R 'SvnRevision.res' '..\..\..\Resources\SvnRevision.rc'}

uses
  Windows,
  SysUtils,
  OpenGL1x in '..\..\..\Source\Delphi\Libraries\GLScene\OpenGL1x.pas',
  OpenGLTokens in '..\..\..\Source\Delphi\Libraries\GLScene\OpenGLTokens.pas',
  VectorTypes in '..\..\..\Source\Delphi\Libraries\GLScene\VectorTypes.pas',
  XboxKrnl in '..\..\..\Source\Delphi\Libraries\OpenXDK\include\xboxkrnl\XboxKrnl.pas',
  uConvert in '..\..\..\Source\Delphi\src\DxbxKrnl\EmuD3D8\uConvert.pas',
  uMiniport in '..\..\..\Source\Delphi\src\DxbxKrnl\EmuD3D8\uMiniport.pas',
  uNV2A in '..\..\..\Source\Delphi\src\DxbxKrnl\EmuD3D8\uNV2A.pas',
  uPixelShader in '..\..\..\Source\Delphi\src\DxbxKrnl\EmuD3D8\uPixelShader.pas',
  uPushBuffer in '..\..\..\Source\Delphi\src\DxbxKrnl\EmuD3D8\uPushBuffer.pas',
  uState in '..\..\..\Source\Delphi\src\DxbxKrnl\EmuD3D8\uState.pas',
  uVertexBuffer in '..\..\..\Source\Delphi\src\DxbxKrnl\EmuD3D8\uVertexBuffer.pas',
  uVertexShader in '..\..\..\Source\Delphi\src\DxbxKrnl\EmuD3D8\uVertexShader.pas',
  DbgConsole in '..\..\..\Source\Delphi\src\DxbxKrnl\DbgConsole.pas',
  DxLibraryAPIScanning in '..\..\..\Source\Delphi\src\DxbxKrnl\DxLibraryAPIScanning.pas',
  uDxbxDebugUtils in '..\..\..\Source\Delphi\src\DxbxKrnl\uDxbxDebugUtils.pas',
  uDxbxKrnl in '..\..\..\Source\Delphi\src\DxbxKrnl\uDxbxKrnl.pas',
  uDxbxKrnlUtils in '..\..\..\Source\Delphi\src\DxbxKrnl\uDxbxKrnlUtils.pas',
  uEmu in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmu.pas',
  uEmuAlloc in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuAlloc.pas',
  uEmuD3D8 in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuD3D8.pas',
  uEmuD3D8Types in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuD3D8Types.pas',
  uEmuD3D8Utils in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuD3D8Utils.pas',
  uEmuDInput in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuDInput.pas',
  uEmuDSound in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuDSound.pas',
  uEmuFile in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuFile.pas',
  uEmuFS in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuFS.pas',
  uEmuKrnl in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnl.pas',
  uEmuKrnlAv in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlAv.pas',
  uEmuKrnlDbg in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlDbg.pas',
  uEmuKrnlEx in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlEx.pas',
  uEmuKrnlFs in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlFs.pas',
  uEmuKrnlHal in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlHal.pas',
  uEmuKrnlIo in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlIo.pas',
  uEmuKrnlKd in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlKd.pas',
  uEmuKrnlKe in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlKe.pas',
  uEmuKrnlMm in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlMm.pas',
  uEmuKrnlNt in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlNt.pas',
  uEmuKrnlOb in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlOb.pas',
  uEmuKrnlPs in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlPs.pas',
  uEmuKrnlRtl in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlRtl.pas',
  uEmuKrnlXbox in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlXbox.pas',
  uEmuKrnlXc in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlXc.pas',
  uEmuKrnlXe in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuKrnlXe.pas',
  uEmuLDT in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuLDT.pas',
  uEmuXactEng in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuXactEng.pas',
  uEmuXapi in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuXapi.pas',
  uEmuXG in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuXG.pas',
  uEmuXOnline in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuXOnline.pas',
  uEmuXTL in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuXTL.pas',
  uHLEIntercept in '..\..\..\Source\Delphi\src\DxbxKrnl\uHLEIntercept.pas',
  uResourceTracker in '..\..\..\Source\Delphi\src\DxbxKrnl\uResourceTracker.pas',
  uBitUtils in '..\..\..\Source\Delphi\src\uBitUtils.pas',
  uConsoleClass in '..\..\..\Source\Delphi\src\uConsoleClass.pas',
  uConsts in '..\..\..\Source\Delphi\src\uConsts.pas',
  uCRC16 in '..\..\..\Source\Delphi\src\uCRC16.pas',
  uDisassembleUtils in '..\..\..\Source\Delphi\src\uDisassembleUtils.pas',
  uDxbxUtils in '..\..\..\Source\Delphi\src\uDxbxUtils.pas',
  uDxbxXml in '..\..\..\Source\Delphi\src\uDxbxXml.pas' {DxbxXml: TDataModule},
  uEmuExe in '..\..\..\Source\Delphi\src\uEmuExe.pas',
  uEmuShared in '..\..\..\Source\Delphi\src\uEmuShared.pas',
  uError in '..\..\..\Source\Delphi\src\uError.pas',
  uFileSystem in '..\..\..\Source\Delphi\src\uFileSystem.pas',
  ufrm_About in '..\..\..\Source\Delphi\src\ufrm_About.pas' {frm_About},
  ufrm_Configuration in '..\..\..\Source\Delphi\src\ufrm_Configuration.pas' {fmConfiguration},
  ufrm_Main in '..\..\..\Source\Delphi\src\ufrm_Main.pas' {frm_Main},
  uImportGames in '..\..\..\Source\Delphi\src\uImportGames.pas' {frm_XBEList},
  uKernelThunk in '..\..\..\Source\Delphi\src\uKernelThunk.pas',
  uLog in '..\..\..\Source\Delphi\src\uLog.pas',
  uMutex in '..\..\..\Source\Delphi\src\uMutex.pas',
  uStoredTrieTypes in '..\..\..\Source\Delphi\src\uStoredTrieTypes.pas',
  uTime in '..\..\..\Source\Delphi\src\uTime.pas',
  uTypes in '..\..\..\Source\Delphi\src\uTypes.pas',
  uWindows in '..\..\..\Source\Delphi\src\uWindows.pas',
  uXBController in '..\..\..\Source\Delphi\src\uXBController.pas',
  uXbe in '..\..\..\Source\Delphi\src\uXbe.pas',
  uXboxLibraryUtils in '..\..\..\Source\Delphi\src\uXboxLibraryUtils.pas',
  uXbSound in '..\..\..\Source\Delphi\src\uXbSound.pas',
  uXbVideo in '..\..\..\Source\Delphi\src\uXbVideo.pas',
  uXDVDFS in '..\..\..\Source\Delphi\src\uXDVDFS.pas',
  VistaIconFix in '..\..\..\Source\Delphi\src\VistaIconFix.pas',
  uData in '..\..\..\Source\Delphi\src\uData.pas',
  XbeHeaders in '..\..\..\Source\Delphi\src\Headers\XbeHeaders.pas';

{$IMAGEBASE $10000000}

procedure DllMain(Reason: Integer);
begin
  if Reason = DLL_PROCESS_ATTACH then
    PEmuShared(nil).Init
  else
    if Reason = DLL_PROCESS_DETACH then
      PEmuShared(nil).Cleanup;
end;

begin
{$IFDEF DXBX_TRACE_MEMLEAKS}
  ReportMemoryLeaksOnShutdown :=True;
{$ENDIF}
  DllProc := DllMain;
  DllProc(DLL_PROCESS_ATTACH);
end.
