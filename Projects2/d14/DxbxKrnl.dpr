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

{$R 'SvnRevision.res' '..\..\resource\SvnRevision.rc'}

uses
  Windows,
  SysUtils,
  uLog in '..\..\src2\uLog.pas',
  uMutex in '..\..\src2\uMutex.pas',
  uStoredTrieTypes in '..\..\src2\uStoredTrieTypes.pas',
  uTime in '..\..\src2\uTime.pas',
  uTypes in '..\..\src2\uTypes.pas',
  uXBController in '..\..\src2\uXBController.pas',
  uXbe in '..\..\src2\uXbe.pas',
  uXboxLibraryUtils in '..\..\src2\uXboxLibraryUtils.pas',
  uXbVideo in '..\..\src2\uXbVideo.pas',
  uConsoleClass in '..\..\src2\uConsoleClass.pas',
  uConsts in '..\..\src2\uConsts.pas',
  uCRC16 in '..\..\src2\uCRC16.pas',
  uEmuShared in '..\..\src2\uEmuShared.pas',
  uError in '..\..\src2\uError.pas',
  uKernelThunk in '..\..\src2\uKernelThunk.pas',
  XboxKrnl in '..\..\Libraries\OpenXDK\include\xboxkrnl\XboxKrnl.pas',
  JclDebug in '..\..\Libraries\Jcl\windows\JclDebug.pas',
  uEmuAlloc in '..\..\src2\DxbxKrnl\uEmuAlloc.pas',
  uEmuD3D8 in '..\..\src2\DxbxKrnl\uEmuD3D8.pas',
  uEmuD3D8Types in '..\..\src2\DxbxKrnl\uEmuD3D8Types.pas',
  uEmuDInput in '..\..\src2\DxbxKrnl\uEmuDInput.pas',
  uEmuDSound in '..\..\src2\DxbxKrnl\uEmuDSound.pas',
  uEmuFile in '..\..\src2\DxbxKrnl\uEmuFile.pas',
  uEmuFS in '..\..\src2\DxbxKrnl\uEmuFS.pas',
  uEmuKrnl in '..\..\src2\DxbxKrnl\uEmuKrnl.pas',
  uEmuKrnlAv in '..\..\src2\DxbxKrnl\uEmuKrnlAv.pas',
  uEmuKrnlDbg in '..\..\src2\DxbxKrnl\uEmuKrnlDbg.pas',
  uEmuKrnlEx in '..\..\src2\DxbxKrnl\uEmuKrnlEx.pas',
  uEmuKrnlFs in '..\..\src2\DxbxKrnl\uEmuKrnlFs.pas',
  uEmuKrnlHal in '..\..\src2\DxbxKrnl\uEmuKrnlHal.pas',
  uEmuKrnlIo in '..\..\src2\DxbxKrnl\uEmuKrnlIo.pas',
  uEmuKrnlKd in '..\..\src2\DxbxKrnl\uEmuKrnlKd.pas',
  uEmuKrnlKe in '..\..\src2\DxbxKrnl\uEmuKrnlKe.pas',
  uEmuKrnlMm in '..\..\src2\DxbxKrnl\uEmuKrnlMm.pas',
  uEmuKrnlNt in '..\..\src2\DxbxKrnl\uEmuKrnlNt.pas',
  uEmuKrnlOb in '..\..\src2\DxbxKrnl\uEmuKrnlOb.pas',
  uEmuKrnlPs in '..\..\src2\DxbxKrnl\uEmuKrnlPs.pas',
  uEmuKrnlRtl in '..\..\src2\DxbxKrnl\uEmuKrnlRtl.pas',
  uEmuKrnlXbox in '..\..\src2\DxbxKrnl\uEmuKrnlXbox.pas',
  uEmuKrnlXc in '..\..\src2\DxbxKrnl\uEmuKrnlXc.pas',
  uEmuKrnlXe in '..\..\src2\DxbxKrnl\uEmuKrnlXe.pas',
  uEmuLDT in '..\..\src2\DxbxKrnl\uEmuLDT.pas',
  uEmuXapi in '..\..\src2\DxbxKrnl\uEmuXapi.pas',
  uEmuXG in '..\..\src2\DxbxKrnl\uEmuXG.pas',
  uEmuXOnline in '..\..\src2\DxbxKrnl\uEmuXOnline.pas',
  uEmuXTL in '..\..\src2\DxbxKrnl\uEmuXTL.pas',
  uHLEIntercept in '..\..\src2\DxbxKrnl\uHLEIntercept.pas',
  DxLibraryAPIScanning in '..\..\src2\DxbxKrnl\DxLibraryAPIScanning.pas',
  uDxbxDebugUtils in '..\..\src2\DxbxKrnl\uDxbxDebugUtils.pas',
  uDxbxKrnl in '..\..\src2\DxbxKrnl\uDxbxKrnl.pas',
  uDxbxKrnlUtils in '..\..\src2\DxbxKrnl\uDxbxKrnlUtils.pas',
  uEmu in '..\..\src2\DxbxKrnl\uEmu.pas',
  uState in '..\..\src2\DxbxKrnl\EmuD3D8\uState.pas',
  uVertexBuffer in '..\..\src2\DxbxKrnl\EmuD3D8\uVertexBuffer.pas',
  uVertexShader in '..\..\src2\DxbxKrnl\EmuD3D8\uVertexShader.pas',
  uConvert in '..\..\src2\DxbxKrnl\EmuD3D8\uConvert.pas',
  uPushBuffer in '..\..\src2\DxbxKrnl\EmuD3D8\uPushBuffer.pas',
  uDxbxUtils in '..\..\src2\uDxbxUtils.pas',
  uEmuExe in '..\..\src2\uEmuExe.pas',
  uResourceTracker in '..\..\src2\DxbxKrnl\uResourceTracker.pas',
  uEmuD3D8Utils in '..\..\src2\DxbxKrnl\uEmuD3D8Utils.pas',
  uPixelShader in '..\..\src2\DxbxKrnl\EmuD3D8\uPixelShader.pas',
  uXDVDFS in '..\..\src2\uXDVDFS.pas',
  uFileSystem in '..\..\src2\uFileSystem.pas',
  uEmuXactEng in '..\..\src2\DxbxKrnl\uEmuXactEng.pas',
  uXbSound in '..\..\src2\uXbSound.pas',
  DbgConsole in '..\..\src2\DxbxKrnl\DbgConsole.pas',
  uMiniport in '..\..\src2\DxbxKrnl\EmuD3D8\uMiniport.pas',
  uNV2A in '..\..\src2\DxbxKrnl\EmuD3D8\uNV2A.pas';

{$IMAGEBASE $60000000}

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
