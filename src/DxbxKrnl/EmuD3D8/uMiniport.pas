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
unit uMiniport;

{$DEFINE _OPTIMIZE_UNIT}

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows
  // Dxbx
  , uTypes
  , uLog
  , uXboxLibraryUtils // PatchPrefix
  , uEmuKrnl // Unimplemented
  , uEmuFS
  ;

implementation

const lfUnit = lfDxbx or lfGraphics;

function XTL_EmuGlobalAlloc(uFlags, dwBytes: DWORD): DWORD; stdcall;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('GlobalAlloc').
      _(uFlags, 'uFlags').
      _(dwBytes, 'dwBytes').
    LogEnd();
  end;

  Result := GlobalAlloc(uFlags, dwBytes);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__InitHardware(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
  ): _bool; register; // fastcall simulation - See Translation guide
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    // D3D__CMiniport__InitHardware(v8 + 8968);
    LogBegin('XTL_EmuD3D__CMiniport__InitHardware').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('XTL_EmuD3D__CMiniport__InitHardware');
  Result := True;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__CreateCtxDmaObject(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {6 stack}a6: Pvoid;
  {5 stack}a5: Pvoid;
  {4 stack}a4: Pvoid;
  {3 stack}a3: int;
  {2 stack}a2: int
  ): int; register; // thiscall simulation - See Translation guide
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    // D3D__CMiniport__CreateCtxDmaObject(v8 + 8968, 6, 2, 0, 67088383, &v22);
    LogBegin('D3D__CMiniport__CreateCtxDmaObject').
      _(This, 'This').
      _(a2, 'a2').
      _(a3, 'a3').
      _(a4, 'a4').
      _(a5, 'a5').
      _(a6, 'a6').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__CreateCtxDmaObject');
  Result := 0;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__DacProgramGammaRamp(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}a2: Pvoid
  ): int; register; // thiscall simulation - See Translation guide
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    // result = D3D__CMiniport__DacProgramGammaRamp(D3D__g_pDevice + 8968, 768 * result + D3D__g_pDevice + 8968 + 532);
    LogBegin('D3D__CMiniport__DacProgramGammaRamp').
      _(This, 'This').
      _(a2, 'a2').
    LogEnd();
  end;

  Result := Unimplemented('D3D__CMiniport__DacProgramGammaRamp');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__InitDMAChannel(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {6 stack}a6: int;
  {5 stack}a5: int;
  {4 stack}a4: Pvoid;
  {3 stack}a3: int;
  {2 stack}a2: Pvoid
  ): int; register; // thiscall simulation - See Translation guide
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    // D3D__CMiniport__InitDMAChannel(v8 + 8968, 8302, 0, &v22, 0, &v23);
    LogBegin('D3D__CMiniport__InitDMAChannel').
      _(This, 'This').
      _(a2, 'a2').
      _(a3, 'a3').
      _(a4, 'a4').
      _(a5, 'a5').
      _(a6, 'a6').
    LogEnd();
  end;

  PDWORD(a6)^ := 0;

  Unimplemented('D3D__CMiniport__InitDMAChannel');
  Result := 0;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__BindToChannel(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}a2: Pvoid
  ): int; register; // thiscall simulation - See Translation guide
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    // D3D__CMiniport__BindToChannel(v8 + 8968, &v22);
    LogBegin('D3D__CMiniport__BindToChannel').
      _(This, 'This').
      _(a2, 'a2').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__BindToChannel');
  Result := 0;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__CreateGrObject(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {4 stack}a4: Pvoid;
  {3 stack}a3: int;
  {2 stack}a2: int
  ): int; register; // thiscall simulation - See Translation guide
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    // D3D__CMiniport__CreateGrObject(v8 + 8968, 17, 98, &v27);
    LogBegin('D3D__CMiniport__CreateGrObject').
      _(This, 'This').
      _(a2, 'a2').
      _(a3, 'a3').
      _(a4, 'a4').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__CreateGrObject');

  Result := 0;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__SetVideoMode(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {8 stack}a8: int;
  {7 stack}a7: int;
  {6 stack}a6: int;
  {5 stack}a5: int;
  {4 stack}a4: int;
  {3 stack}a3: int;
  {2 stack}a2: int
  ): int; register; // thiscall simulation - See Translation guide
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    // D3D__CMiniport__SetVideoMode(v8 + 8968, *(_DWORD *)a2, *(_DWORD *)(a2 + 4), *(_DWORD *)(a2 + 44),
   //   *(_DWORD *)(a2 + 40), *(_DWORD *)(a2 + 8), *(_DWORD *)(a2 + 48), v7);
    LogBegin('D3D__CMiniport__SetVideoMode').
      _(This, 'This').
      _(a2, 'a2').
      _(a3, 'a3').
      _(a4, 'a4').
      _(a5, 'a5').
      _(a6, 'a6').
      _(a7, 'a7').
      _(a8, 'a8').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__SetVideoMode');
  Result := 0;

  EmuSwapFS(fsXbox);
end;

// This unit is an attempt to run Direct3D_CreateDevice and D3D__CDevice__Init unpatched.
//
// D3D__CDevice__Init calls these methods, that should still be patched (if not already) :
//
//      D3D__BusyLoop();
//    D3D__InitializeHardware();
//    result = D3D__CDevice__InitializeFrameBuffers(v8, a2);
//      v7 = D3D__PixelJar__GetPitch(v8 + 8552);
//      D3DDevice_SetVertexShader(2);
//      D3DDevice_SetRenderTarget(v8 + 8528, v4);
//      D3D__InitializeD3dState();
//      D3DDevice_Clear(0, 0, 3, 0, 1065353216, 0);

exports
//  XTL_EmuGlobalAlloc, // TODO : Move this to a better unit. This patch crashes EviLoader!

  XTL_EmuD3D__CMiniport__BindToChannel name PatchPrefix + '?BindToChannel@CMiniport@D3D@@QAEHPAUOBJECTINFO@12@@Z',
  XTL_EmuD3D__CMiniport__CreateCtxDmaObject name PatchPrefix + '?CreateCtxDmaObject@CMiniport@D3D@@QAEHKKPAXKPAUOBJECTINFO@12@@Z',
  XTL_EmuD3D__CMiniport__CreateGrObject name PatchPrefix + '?CreateGrObject@CMiniport@D3D@@QAEHKKPAUOBJECTINFO@12@@Z',
  XTL_EmuD3D__CMiniport__DacProgramGammaRamp name PatchPrefix + '?DacProgramGammaRamp@CMiniport@D3D@@QAEXPAU_D3DGAMMARAMP@@@Z',
  XTL_EmuD3D__CMiniport__InitDMAChannel name PatchPrefix + '?InitDMAChannel@CMiniport@D3D@@QAEHKPAUOBJECTINFO@12@0KPAPAX@Z',
  XTL_EmuD3D__CMiniport__InitHardware name PatchPrefix + '?InitHardware@CMiniport@D3D@@QAEHXZ',
  XTL_EmuD3D__CMiniport__SetVideoMode name PatchPrefix + '';

end.

