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

{$R 'StoredTrie.res' '..\..\src\Tools\PatternTrieBuilder\StoredTrie.rc'}

uses
  Windows,
  SysUtils,
  uLog in '..\..\src\uLog.pas',
  uMutex in '..\..\src\uMutex.pas',
  uStoredTrieTypes in '..\..\src\uStoredTrieTypes.pas',
  uTime in '..\..\src\uTime.pas',
  uTypes in '..\..\src\uTypes.pas',
  uXBController in '..\..\src\uXBController.pas',
  uXbe in '..\..\src\uXbe.pas',
  uXboxLibraryUtils in '..\..\src\uXboxLibraryUtils.pas',
  uXbVideo in '..\..\src\uXbVideo.pas',
  uConsoleClass in '..\..\src\uConsoleClass.pas',
  uConsts in '..\..\src\uConsts.pas',
  uCRC16 in '..\..\src\uCRC16.pas',
  uEmuShared in '..\..\src\uEmuShared.pas',
  uError in '..\..\src\uError.pas',
  uKernelThunk in '..\..\src\uKernelThunk.pas',
  XboxKrnl in '..\..\Libraries\OpenXDK\include\xboxkrnl\XboxKrnl.pas',
  JclDebug in '..\..\Libraries\Jcl\windows\JclDebug.pas',
  uEmuAlloc in '..\..\src\DxbxKrnl\uEmuAlloc.pas',
  uEmuD3D8 in '..\..\src\DxbxKrnl\uEmuD3D8.pas',
  uEmuDInput in '..\..\src\DxbxKrnl\uEmuDInput.pas',
  uEmuDSound in '..\..\src\DxbxKrnl\uEmuDSound.pas',
  uEmuFile in '..\..\src\DxbxKrnl\uEmuFile.pas',
  uEmuFS in '..\..\src\DxbxKrnl\uEmuFS.pas',
  uEmuKrnl in '..\..\src\DxbxKrnl\uEmuKrnl.pas',
  uEmuKrnlAv in '..\..\src\DxbxKrnl\uEmuKrnlAv.pas',
  uEmuKrnlDbg in '..\..\src\DxbxKrnl\uEmuKrnlDbg.pas',
  uEmuKrnlEx in '..\..\src\DxbxKrnl\uEmuKrnlEx.pas',
  uEmuKrnlFs in '..\..\src\DxbxKrnl\uEmuKrnlFs.pas',
  uEmuKrnlHal in '..\..\src\DxbxKrnl\uEmuKrnlHal.pas',
  uEmuKrnlIo in '..\..\src\DxbxKrnl\uEmuKrnlIo.pas',
  uEmuKrnlKd in '..\..\src\DxbxKrnl\uEmuKrnlKd.pas',
  uEmuKrnlKe in '..\..\src\DxbxKrnl\uEmuKrnlKe.pas',
  uEmuKrnlMm in '..\..\src\DxbxKrnl\uEmuKrnlMm.pas',
  uEmuKrnlNt in '..\..\src\DxbxKrnl\uEmuKrnlNt.pas',
  uEmuKrnlOb in '..\..\src\DxbxKrnl\uEmuKrnlOb.pas',
  uEmuKrnlPs in '..\..\src\DxbxKrnl\uEmuKrnlPs.pas',
  uEmuKrnlRtl in '..\..\src\DxbxKrnl\uEmuKrnlRtl.pas',
  uEmuKrnlXbox in '..\..\src\DxbxKrnl\uEmuKrnlXbox.pas',
  uEmuKrnlXc in '..\..\src\DxbxKrnl\uEmuKrnlXc.pas',
  uEmuKrnlXe in '..\..\src\DxbxKrnl\uEmuKrnlXe.pas',
  uEmuLDT in '..\..\src\DxbxKrnl\uEmuLDT.pas',
  uEmuXapi in '..\..\src\DxbxKrnl\uEmuXapi.pas',
  uEmuXG in '..\..\src\DxbxKrnl\uEmuXG.pas',
  uEmuXOnline in '..\..\src\DxbxKrnl\uEmuXOnline.pas',
  uEmuXTL in '..\..\src\DxbxKrnl\uEmuXTL.pas',
  uHLEDatabase in '..\..\src\DxbxKrnl\uHLEDatabase.pas',
  uHLEIntercept in '..\..\src\DxbxKrnl\uHLEIntercept.pas',
  DxLibraryAPIScanning in '..\..\src\DxbxKrnl\DxLibraryAPIScanning.pas',
  uDxbxDebugUtils in '..\..\src\DxbxKrnl\uDxbxDebugUtils.pas',
  uDxbxKrnl in '..\..\src\DxbxKrnl\uDxbxKrnl.pas',
  uDxbxKrnlUtils in '..\..\src\DxbxKrnl\uDxbxKrnlUtils.pas',
  uEmu in '..\..\src\DxbxKrnl\uEmu.pas',
  uState in '..\..\src\DxbxKrnl\EmuD3D8\uState.pas',
  uVertexBuffer in '..\..\src\DxbxKrnl\EmuD3D8\uVertexBuffer.pas',
  uVertexShader in '..\..\src\DxbxKrnl\EmuD3D8\uVertexShader.pas',
  uConvert in '..\..\src\DxbxKrnl\EmuD3D8\uConvert.pas',
  uPushBuffer in '..\..\src\DxbxKrnl\EmuD3D8\uPushBuffer.pas',
  uDxbxUtils in '..\..\src\uDxbxUtils.pas',
  uEmuExe in '..\..\src\uEmuExe.pas',
  uProlog in '..\..\src\uProlog.pas',
  uExe in '..\..\src\uExe.pas',
  uResourceTracker in '..\..\src\DxbxKrnl\uResourceTracker.pas',
  uEmuD3D8Utils in '..\..\src\DxbxKrnl\uEmuD3D8Utils.pas',
  uEmuD3D8Types in '..\..\src\DxbxKrnl\uEmuD3D8Types.pas';

// TODO : This is temporary, until uEmuExe can determine LoadTimeDLLBase correctly :
{$IMAGEBASE $10000000}

procedure DllMain(Reason: Integer);
begin
  if Reason = DLL_PROCESS_ATTACH then
    EmuShared.Init
  else
    if Reason = DLL_PROCESS_DETACH then
      EmuShared.Cleanup;
end;

begin
  DllProc := DllMain;
  DllProc(DLL_PROCESS_ATTACH);
end.
