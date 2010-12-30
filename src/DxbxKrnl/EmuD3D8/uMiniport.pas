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
  , uEmuD3D8Types
  , uXboxLibraryUtils // PatchPrefix
  , uEmuKrnl // Unimplemented
  , uEmuFS
  ;

implementation

const lfUnit = lfDxbx or lfGraphics;

function XTL_EmuGlobalAlloc(uFlags, dwBytes: DWORD): DWORD; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
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
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
  ): _bool; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:PatrickvL  Done:0
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
// Branch:Dxbx  Translator:PatrickvL  Done:0
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
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__DacProgramGammaRamp(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}a2: Pvoid
  ): int; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:PatrickvL  Done:0
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

  Unimplemented('D3D__CMiniport__DacProgramGammaRamp');
  Result := S_OK;

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
// Branch:Dxbx  Translator:PatrickvL  Done:0
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
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__BindToChannel(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}a2: Pvoid
  ): int; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:PatrickvL  Done:0
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
  Result := S_OK;

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
// Branch:Dxbx  Translator:PatrickvL  Done:0
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
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__CreateTile(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {8 stack}ZOffset: ULONG;
  {7 stack}ZTag: ULONG;
  {6 stack}MemType: ULONG;
  {5 stack}Pitch: ULONG;
  {4 stack}Size: ULONG;
  {3 stack}Offset: ULONG;
  {2 stack}TileRegion: ULONG
): _bool; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__CreateTile').
      _(This, 'This').
      _(TileRegion, 'TileRegion').
      _(Offset, 'Offset').
      _(Size, 'Size').
      _(Pitch, 'Pitch').
      _(MemType, 'MemType').
      _(ZTag, 'ZTag').
      _(ZOffset, 'ZOffset').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__CreateTile');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__DumpClocks(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid); register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__DumpClocks').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__DumpClocks');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__MapRegisters(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): _bool; register // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__MapRegisters').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__MapRegisters');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__GetGeneralInfo(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): _bool; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__GetGeneralInfo').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__GetGeneralInfo');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__ReserveInstMem(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}blocks: ULONG
): ULONG; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__ReserveInstMem').
      _(This, 'This').
      _(blocks, 'blocks').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__ReserveInstMem');
  Result := 0;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__InitEngines(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): _bool; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__InitEngines').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__InitEngines');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__LoadEngines(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): _bool; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__LoadEngines').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__LoadEngines');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__InitGammaRamp(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
); register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__InitGammaRamp').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__InitGammaRamp');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__GetAddressInfo(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {5 stack}IsAlias: BOOL;
  {4 stack}pAddressSpace: PULONG;
  {3 stack}ppAddress: PPVOID;
  {2 stack}pLinearAddress: PVOID
); register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__GetAddressInfo').
      _(This, 'This').
      _(pLinearAddress, 'pLinearAddress').
      _(ppAddress, 'ppAddress').
      _(pAddressSpace, 'pAddressSpace').
      _(IsAlias, 'IsAlias').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__GetAddressInfo');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__SetDmaRange(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {3 stack}pSurface: PX_D3DSurface; // Can be NULL
  {2 stack}DmaContext: ULONG
): DWORD; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__SetDmaRange').
      _(This, 'This').
      _(DmaContext, 'DmaContext').
      _(pSurface, 'pSurface').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__SetDmaRange');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__ShutdownEngines(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid); register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__ShutdownEngines').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__ShutdownEngines');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__DestroyTile(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {3 stack}ZOffset: ULONG;
  {2 stack}TileRegion: ULONG
): _bool; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__DestroyTile').
      _(This, 'This').
      _(TileRegion, 'TileRegion').
      _(ZOffset, 'ZOffset').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__DestroyTile');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__TilingUpdateIdle(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}dmapush: PULONG
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__TilingUpdateIdle').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__TilingUpdateIdle');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalMcControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalMcControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalMcControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalFbControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalFbControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalFbControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalDacControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalDacControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalDacControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalVideoControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalVideoControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalVideoControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalGrControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalGrControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalGrControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalFifoControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalFifoControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalFifoControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalGrControlLoad(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalGrControlLoad').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalGrControlLoad');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalGrIdle(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalGrIdle').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalGrIdle');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalGrLoadChannelContext(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}ChID: ULONG
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalGrLoadChannelContext').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalGrLoadChannelContext');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalGrUnloadChannelContext(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}ChID: ULONG
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalGrUnloadChannelContext').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalGrUnloadChannelContext');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalFifoControlLoad(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalFifoControlLoad').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalFifoControlLoad');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalFifoContextSwitch(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}ChID: ULONG
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalFifoContextSwitch').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalFifoContextSwitch');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalMpControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalMpControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalMpControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalDacUnload(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalDacUnload').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalDacUnload');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalDacProgramMClk(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalDacProgramMClk').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalDacProgramMClk');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalDacProgramNVClk(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalDacProgramNVClk').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalDacProgramNVClk');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalDacProgramPClk(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalDacProgramPClk').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalDacProgramPClk');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalFifoHashAdd(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {6 stack}engine: ULONG;
  {5 stack}instance: ULONG;
  {4 stack}chid: ULONG;
  {3 stack}handle: ULONG;
  {2 stack}entry: ULONG
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalFifoHashAdd').
      _(This, 'This').
      _(entry, 'entry').
      _(handle, 'handle').
      _(chid, 'chid').
      _(instance, 'instance').
      _(engine, 'engine').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalFifoHashAdd');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalGrInitObjectContext(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {3 stack}ClassNum: ULONG;
  {2 stack}Instance: ULONG
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalGrInitObjectContext').
      _(This, 'This').
      _(Instance, 'Instance').
      _(ClassNum, 'ClassNum').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalGrInitObjectContext');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalGrInit3d(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalGrInit3d').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalGrInit3d');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__HalDacLoad(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__HalDacLoad').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__HalDacLoad');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__GetPresentFlagsFromAvInfo(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}AvInfo: DWORD
): DWORD; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__GetPresentFlagsFromAvInfo').
      _(This, 'This').
      _(AvInfo, 'AvInfo').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__GetPresentFlagsFromAvInfo');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__GetDisplayCapabilities(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): DWORD; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__GetDisplayCapabilities').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__GetDisplayCapabilities');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__IsFlipPending(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): _bool; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__GetDisplayCapabilities').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__GetDisplayCapabilities');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__VBlankFlip(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {3 stack}FlipTime: ULONG;
  {2 stack}Offset: ULONG
); register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__VBlankFlip').
      _(This, 'This').
      _(Offset, 'Offset').
      _(FlipTime, 'FlipTime').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__VBlankFlip');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__VBlank(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): ULONG; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__VBlank').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__VBlank');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__ServiceGrInterrupt(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): ULONG; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__ServiceGrInterrupt').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__ServiceGrInterrupt');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__ServiceFifoInterrupt(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): ULONG; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__ServiceFifoInterrupt').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__ServiceFifoInterrupt');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__ServiceMediaPortInterrupt(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): ULONG; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__ServiceMediaPortInterrupt').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__ServiceMediaPortInterrupt');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D__CMiniport__ServiceVideoInterrupt(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): ULONG; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__ServiceVideoInterrupt').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__ServiceVideoInterrupt');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D__CMiniport__SoftwareMethod(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {3 stack}Data: ULONG;
  {2 stack}Method: ULONG
); register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D__CMiniport__ServiceVideoInterrupt').
      _(This, 'This').
      _(Method, 'Method').
      _(Data, 'Data').
    LogEnd();
  end;

  Unimplemented('D3D__CMiniport__ServiceVideoInterrupt');

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
// Branch:Dxbx  Translator:PatrickvL  Done:0
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
  Result := S_OK;

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

//  XTL_EmuD3D__CMiniport__ApplyLGSFix name PatchPrefix + '?ApplyLGSFix@CMiniport@D3D@@QAEXXZ', // not implemented
  XTL_EmuD3D__CMiniport__BindToChannel name PatchPrefix + '?BindToChannel@CMiniport@D3D@@QAEHPAUOBJECTINFO@12@@Z',
  XTL_EmuD3D__CMiniport__CreateCtxDmaObject name PatchPrefix + '?CreateCtxDmaObject@CMiniport@D3D@@QAEHKKPAXKPAUOBJECTINFO@12@@Z',
  XTL_EmuD3D__CMiniport__CreateGrObject name PatchPrefix + '?CreateGrObject@CMiniport@D3D@@QAEHKKPAUOBJECTINFO@12@@Z',
  XTL_EmuD3D__CMiniport__CreateTile name PatchPrefix + '?CreateTile@CMiniport@D3D@@QAEHKKKKKKK@Z',
  XTL_EmuD3D__CMiniport__DacProgramGammaRamp name PatchPrefix + '?DacProgramGammaRamp@CMiniport@D3D@@QAEXPAU_D3DGAMMARAMP@@@',
//  XTL_EmuD3D__CMiniport__DacProgramVideoStart name PatchPrefix + '?DacProgramVideoStart@CMiniport@D3D@@QAEXK@Z', // not implemented
//  XTL_EmuD3D__CMiniport__DisableInterrupts name PatchPrefix + '?DisableInterrupts@CMiniport@D3D@@QAEXPAX@Z', // not implemented
  XTL_EmuD3D__CMiniport__DumpClocks name PatchPrefix + '?DumpClocks@CMiniport@D3D@@AAEXXZ',
//  XTL_EmuD3D__CMiniport__EnableTimer name PatchPrefix + '?EnableTimer@CMiniport@D3D@@AAEXH@Z', // not implemented
//  XTL_EmuD3D__CMiniport__FixupPushBuffer name PatchPrefix + '?FixupPushBuffer@D3D@@YGXPAUPushBufferFixup@1@@Z', // not implemented
  XTL_EmuD3D__CMiniport__HalDacLoad name PatchPrefix + '?HalDacLoad@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalDacProgramMClk name PatchPrefix + '?HalDacProgramMClk@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalDacProgramNVClk name PatchPrefix + '?HalDacProgramNVClk@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalDacProgramPClk name PatchPrefix + '?HalDacProgramPClk@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalDacUnload name PatchPrefix + '?HalDacUnload@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__DestroyTile name PatchPrefix + '?DestroyTile@CMiniport@D3D@@QAEHKK@Z',
//  XTL_EmuD3D__CMiniport__DisableInterrupts name PatchPrefix + '?DisableInterrupts@CMiniport@D3D@@QAEXPAX@Z', // not implemented
//  XTL_EmuD3D__CMiniport__Dpc name PatchPrefix + '?Dpc@CMiniport@D3D@@SGXPAU_KDPC@@PAX11@Z', // not implemented
//  XTL_EmuD3D__CMiniport__EnableInterrupts name PatchPrefix + '?EnableInterrupts@CMiniport@D3D@@QAEXPAX@Z', // not implemented
  XTL_EmuD3D__CMiniport__GetAddressInfo name PatchPrefix + '?GetAddressInfo@CMiniport@D3D@@AAEXPAXPAPAXPAKH@Z',
  XTL_EmuD3D__CMiniport__GetDisplayCapabilities name PatchPrefix + '?GetDisplayCapabilities@CMiniport@D3D@@SGKXZ',
  XTL_EmuD3D__CMiniport__GetGeneralInfo name PatchPrefix + '?GetGeneralInfo@CMiniport@D3D@@AAEHXZ',
//  XTL_EmuD3D__CMiniport__GetRefreshRate name PatchPrefix + '?GetRefreshRate@CMiniport@D3D@@QAEKXZ', // not implemented
//  XTL_EmuD3D__CMiniport__GetTime name PatchPrefix + '?GetTime@CMiniport@D3D@@QAEKXZ', // not implemented
//  XTL_EmuD3D__CMiniport__GetTimerTime name PatchPrefix + '?GetTimerTime@CMiniport@D3D@@AAE_KXZ', // not implemented
//  XTL_EmuD3D__CMiniport__GetTSC name PatchPrefix + '?GetTSC@CMiniport@D3D@@SG_KXZ', // not implemented
  XTL_EmuD3D__CMiniport__GetPresentFlagsFromAvInfo name PatchPrefix + '?GetPresentFlagsFromAvInfo@CMiniport@D3D@@SGKK@ZZ',
//  XTL_EmuD3D__CMiniport__GrDone name PatchPrefix + '?GrDone@CMiniport@D3D@@AAEXXZ', // not implemented
  XTL_EmuD3D__CMiniport__HalDacControlInit name PatchPrefix + '?HalDacControlInit@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalFbControlInit name PatchPrefix + '?HalFbControlInit@CMiniport@D3D@@AAEXXZ',
//  XTL_EmuD3D__CMiniport__HalFifoAllocDMA name PatchPrefix + '?HalFifoAllocDMA@CMiniport@D3D@@AAEXKKKPAUOBJECTINFO@12@@Z', // not implemented
  XTL_EmuD3D__CMiniport__HalFifoContextSwitch name PatchPrefix + '?HalFifoContextSwitch@CMiniport@D3D@@AAEXK@Z',
  XTL_EmuD3D__CMiniport__HalFifoControlLoad name PatchPrefix + '?HalFifoControlLoad@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalFifoControlInit name PatchPrefix + '?HalFifoControlInit@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalFifoHashAdd name PatchPrefix + '?HalFifoHashAdd@CMiniport@D3D@@AAEXKKKKK@Z',
  XTL_EmuD3D__CMiniport__HalGrControlLoad name PatchPrefix + '?HalGrControlLoad@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalGrControlInit name PatchPrefix + '?HalGrControlInit@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalGrIdle name PatchPrefix + '?HalGrIdle@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalGrInit3d name PatchPrefix + '?HalGrInit3d@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalGrInitObjectContext name PatchPrefix + '?HalGrInitObjectContext@CMiniport@D3D@@AAEXKK@Z',
  XTL_EmuD3D__CMiniport__HalGrLoadChannelContext name PatchPrefix + '?HalGrLoadChannelContext@CMiniport@D3D@@AAEXK@Z',
  XTL_EmuD3D__CMiniport__HalGrUnloadChannelContext name PatchPrefix + '?HalGrUnloadChannelContext@CMiniport@D3D@@AAEXK@Z',
  XTL_EmuD3D__CMiniport__HalMcControlInit name PatchPrefix + '?HalMcControlInit@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalMpControlInit name PatchPrefix + '?HalMpControlInit@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__HalVideoControlInit name PatchPrefix + '?HalVideoControlInit@CMiniport@D3D@@AAEXXZ',
  XTL_EmuD3D__CMiniport__InitDMAChannel name PatchPrefix + '?InitDMAChannel@CMiniport@D3D@@QAEHKPAUOBJECTINFO@12@0KPAPAX@Z',
  XTL_EmuD3D__CMiniport__InitEngines name PatchPrefix + '?InitEngines@CMiniport@D3D@@AAEHXZ',
  XTL_EmuD3D__CMiniport__InitHardware name PatchPrefix + '?InitHardware@CMiniport@D3D@@QAEHXZ',
  XTL_EmuD3D__CMiniport__InitGammaRamp name PatchPrefix + '?InitGammaRamp@CMiniport@D3D@@AAEXK@Z',
//  XTL_EmuD3D__CMiniport__IsOddField name PatchPrefix + '?IsOddField@CMiniport@D3D@@QAEHXZ', // not implemented
//  XTL_EmuD3D__CMiniport__Isr name PatchPrefix + '?Isr@CMiniport@D3D@@SGEPAU_KINTERRUPT@@PAX@Z', // not implemented
  XTL_EmuD3D__CMiniport__IsFlipPending name PatchPrefix + '?IsFlipPending@CMiniport@D3D@@QAEHXZ',
  XTL_EmuD3D__CMiniport__LoadEngines name PatchPrefix + '?LoadEngines@CMiniport@D3D@@AAEHXZ',
  XTL_EmuD3D__CMiniport__MapRegisters name PatchPrefix + '?MapRegisters@CMiniport@D3D@@AAEHXZ',
  XTL_EmuD3D__CMiniport__ReserveInstMem name PatchPrefix + '?ReserveInstMem@CMiniport@D3D@@AAEKK@Z',
  XTL_EmuD3D__CMiniport__ServiceFifoInterrupt name PatchPrefix + '?ServiceFifoInterrupt@CMiniport@D3D@@AAEKXZ',
  XTL_EmuD3D__CMiniport__ServiceGrInterrupt name PatchPrefix + '?ServiceGrInterrupt@CMiniport@D3D@@AAEKXZ',
  XTL_EmuD3D__CMiniport__ServiceMediaPortInterrupt name PatchPrefix + '?ServiceMediaPortInterrupt@CMiniport@D3D@@AAEKXZ',
//  XTL_EmuD3D__CMiniport__ServiceTimerInterrupt name PatchPrefix + '?ServiceTimerInterrupt@CMiniport@D3D@@AAEKXZ', // not implemented
//  XTL_EmuD3D__Cminiport__ServiceQueuedFlips name PatchPrefix + '?ServiceQueuedFlips@CMiniport@D3D@@AAEKXZ', // // not implemented
  XTL_EmuD3D__CMiniport__ServiceVideoInterrupt name PatchPrefix + '?ServiceVideoInterrupt@CMiniport@D3D@@AAEKXZ',
  XTL_EmuD3D__CMiniport__SetDmaRange name PatchPrefix + '?SetDmaRange@CMiniport@D3D@@QAEKKPAUD3DSurface@@@Z',
//  XTL_EmuD3D__CMiniport__SetTimerAlarm name PatchPrefix + '?SetTimerAlarm@CMiniport@D3D@@AAEX_KH@Z', // not implemented
//  XTL_EmuD3D__CMiniport__SetTimerCallbackAlarm name PatchPrefix + '?SetTimerCallbackAlarm@CMiniport@D3D@@QAEX_KP6AXK@ZK@Z', // not implemented
  XTL_EmuD3D__CMiniport__SetVideoMode name PatchPrefix + '?SetVideoMode@CMiniport@D3D@@QAEXKKKKW4_D3DFORMAT@@KK@Z',
  XTL_EmuD3D__CMiniport__SoftwareMethod name PatchPrefix + '?SoftwareMethod@CMiniport@D3D@@AAEXK@Z',
  XTL_EmuD3D__CMiniport__ShutdownEngines name PatchPrefix + '?ShutdownEngines@CMiniport@D3D@@QAEXXZ',
//  XTL_EmuD3D__CMiniport__ShutdownNotification name PatchPrefix + '?ShutdownNotification@CMiniport@D3D@@SGXPAU_HAL_SHUTDOWN_REGISTRATION@@@Z', // not implemented
  XTL_EmuD3D__CMiniport__TilingUpdateIdle name PatchPrefix + '?TilingUpdateIdle@CMiniport@D3D@@QAEXPAK@Z',
  XTL_EmuD3D__CMiniport__VBlank name PatchPrefix + '?VBlank@CMiniport@D3D@@AAEKXZ',
  XTL_EmuD3D__CMiniport__VBlankFlip name PatchPrefix + '?VBlankFlip@CMiniport@D3D@@AAEXKK@Z'
  ;


end.

