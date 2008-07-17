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

uses
  Windows,
  SysUtils,
  uEmuShared in 'uEmuShared.pas',
  uLog in 'uLog.pas',
  uConsts in 'uConsts.pas',
  uMutex in 'uMutex.pas',
  uKernelThunk in 'uKernelThunk.pas',
  uTime in 'uTime.pas',
  uTypes in 'uTypes.pas',
  uDxbxKrnlUtils in 'DxbxKrnl\uDxbxKrnlUtils.pas',
  uEmuLDT in 'DxbxKrnl\uEmuLDT.pas',
  uDxbxKrnl in 'DxbxKrnl\uDxbxKrnl.pas',
  uEmuFS in 'DxbxKrnl\uEmuFS.pas',
  uEmu in 'DxbxKrnl\uEmu.pas',
  uEmuD3D8 in 'DxbxKrnl\uEmuD3D8.pas',
  uEmuAlloc in 'DxbxKrnl\uEmuAlloc.pas',
  uXbe in 'uXbe.pas',
  JwaWinType in '..\Libraries\jwapi\Win32API\JwaWinType.pas',
  JwaWinNT in '..\Libraries\jwapi\Win32API\JwaWinNT.pas',
  XboxKrnl in '..\Libraries\OpenXDK\include\xboxkrnl\XboxKrnl.pas',
  JwaWinBase in '..\Libraries\jwapi\Win32API\JwaWinBase.pas',
  JwaNative in '..\Libraries\jwapi\Win32API\JwaNative.pas',
  JwaNtStatus in '..\Libraries\jwapi\Win32API\JwaNtStatus.pas',
  uEmuKrnl in 'DxbxKrnl\uEmuKrnl.pas',
  uEmuXapi in 'DxbxKrnl\uEmuXapi.pas',
  uEmuFile in 'DxbxKrnl\uEmuFile.pas',
  uEmuKrnlDbg in 'DxbxKrnl\uEmuKrnlDbg.pas',
  uEmuKrnlEx in 'DxbxKrnl\uEmuKrnlEx.pas',
  uEmuKrnlFs in 'DxbxKrnl\uEmuKrnlFs.pas',
  uEmuKrnlHal in 'DxbxKrnl\uEmuKrnlHal.pas',
  uEmuKrnlIo in 'DxbxKrnl\uEmuKrnlIo.pas',
  uEmuKrnlKd in 'DxbxKrnl\uEmuKrnlKd.pas',
  uEmuKrnlKe in 'DxbxKrnl\uEmuKrnlKe.pas',
  uEmuKrnlMm in 'DxbxKrnl\uEmuKrnlMm.pas',
  uEmuKrnlNt in 'DxbxKrnl\uEmuKrnlNt.pas',
  uEmuKrnlOb in 'DxbxKrnl\uEmuKrnlOb.pas',
  uEmuKrnlPs in 'DxbxKrnl\uEmuKrnlPs.pas',
  uEmuKrnlRtl in 'DxbxKrnl\uEmuKrnlRtl.pas',
  uEmuKrnlXbox in 'DxbxKrnl\uEmuKrnlXbox.pas',
  uEmuKrnlXc in 'DxbxKrnl\uEmuKrnlXc.pas',
  uEmuKrnlXe in 'DxbxKrnl\uEmuKrnlXe.pas',
  uEmuKrnlAv in 'DxbxKrnl\uEmuKrnlAv.pas',
  uHLEDatabase in 'DxbxKrnl\uHLEDatabase.pas',
  uXbVideo in 'uXbVideo.pas',
  uEmuD3D8Types in 'DxbxKrnl\uEmuD3D8Types.pas',
  uEmuDInput in 'DxbxKrnl\uEmuDInput.pas',
  uEmuDSound in 'DxbxKrnl\uEmuDSound.pas',
  uEmuXG in 'DxbxKrnl\uEmuXG.pas',
  uEmuXOnline in 'DxbxKrnl\uEmuXOnline.pas',
  uEmuXTL in 'DxbxKrnl\uEmuXTL.pas',
  uConvert in 'DxbxKrnl\EmuD3D8\uConvert.pas',
  uPushBuffer in 'DxbxKrnl\EmuD3D8\uPushBuffer.pas',
  uState in 'DxbxKrnl\EmuD3D8\uState.pas',
  uVertexBuffer in 'DxbxKrnl\EmuD3D8\uVertexBuffer.pas',
  uVertexShader in 'DxbxKrnl\EmuD3D8\uVertexShader.pas',
  uXBController in 'uXBController.pas',
  uError in 'uError.pas',
  uBitUtils in 'uBitUtils.pas',
  uPatternScanner in 'uPatternScanner.pas',
  DxLibraryAPIScanning in 'DxbxKrnl\DxLibraryAPIScanning.pas',
  uCRC16 in 'uCRC16.pas',
  uConsoleClass in 'uConsoleClass.pas',
  uXboxLibraryPatches in 'DxbxKrnl\uXboxLibraryPatches.pas',
  uXboxLibraryUtils in 'uXboxLibraryUtils.pas',
  uHLEIntercept in 'DxbxKrnl\uHLEIntercept.pas',
  uPatterns in 'Tools\PatternCodeGenerator\uPatterns.pas';

// TODO : This is temporary, until uEmuExe can determine LoadTimeDLLBase correctly :
{$IMAGEBASE $10000000}

exports
  CxbxKrnlInit,
  CxbxKrnlNoFunc,
  SetXbePath name '?SetXbePath@EmuShared@@QAEXPBD@Z',
  CxbxKrnl_KernelThunkTable;

(*  Exports EmuVerifyVersion name '_EmuVerifyVersion@4';
  Exports EmuPanic name '_EmuPanic@0';
  Exports ;
  Exports EmuCleanup;
  Exports EmuCleanThread name '_EmuCleanThread@0';
  { TODO : name need to be set }
  (*Exports Init; // name must be "void EmuShared::Init (void)
  //  Exports KernelThunkTable;*)

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
