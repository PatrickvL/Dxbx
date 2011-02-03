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

function XTL_EmuD3D_CMiniport_InitHardware(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
  ): _bool; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    // D3D_CMiniport_InitHardware(v8 + 8968);
    LogBegin('XTL_EmuD3D_CMiniport_InitHardware').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('XTL_EmuD3D_CMiniport_InitHardware');
  Result := True;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_CreateCtxDmaObject(
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
    // D3D_CMiniport_CreateCtxDmaObject(v8 + 8968, 6, 2, 0, 67088383, &v22);
    LogBegin('D3D_CMiniport_CreateCtxDmaObject').
      _(This, 'This').
      _(a2, 'a2').
      _(a3, 'a3').
      _(a4, 'a4').
      _(a5, 'a5').
      _(a6, 'a6').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_CreateCtxDmaObject');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_DacProgramGammaRamp(
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
    // result = D3D_CMiniport_DacProgramGammaRamp(D3D__g_pDevice + 8968, 768 * result + D3D__g_pDevice + 8968 + 532);
    LogBegin('D3D_CMiniport_DacProgramGammaRamp').
      _(This, 'This').
      _(a2, 'a2').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_DacProgramGammaRamp');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_InitDMAChannel(
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
    // D3D_CMiniport_InitDMAChannel(v8 + 8968, 8302, 0, &v22, 0, &v23);
    LogBegin('D3D_CMiniport_InitDMAChannel').
      _(This, 'This').
      _(a2, 'a2').
      _(a3, 'a3').
      _(a4, 'a4').
      _(a5, 'a5').
      _(a6, 'a6').
    LogEnd();
  end;

  PDWORD(a6)^ := 0;

  Unimplemented('D3D_CMiniport_InitDMAChannel');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_BindToChannel(
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
    // D3D_CMiniport_BindToChannel(v8 + 8968, &v22);
    LogBegin('D3D_CMiniport_BindToChannel').
      _(This, 'This').
      _(a2, 'a2').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_BindToChannel');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_CreateGrObject(
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
    // D3D_CMiniport_CreateGrObject(v8 + 8968, 17, 98, &v27);
    LogBegin('D3D_CMiniport_CreateGrObject').
      _(This, 'This').
      _(a2, 'a2').
      _(a3, 'a3').
      _(a4, 'a4').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_CreateGrObject');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_CreateTile(
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
    LogBegin('D3D_CMiniport_CreateTile').
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

  Unimplemented('D3D_CMiniport_CreateTile');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_DumpClocks(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid); register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_DumpClocks').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_DumpClocks');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_MapRegisters(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): _bool; register // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_MapRegisters').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_MapRegisters');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_GetGeneralInfo(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): _bool; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_GetGeneralInfo').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_GetGeneralInfo');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_ReserveInstMem(
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
    LogBegin('D3D_CMiniport_ReserveInstMem').
      _(This, 'This').
      _(blocks, 'blocks').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_ReserveInstMem');
  Result := 0;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_InitEngines(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): _bool; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_InitEngines').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_InitEngines');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_LoadEngines(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): _bool; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_LoadEngines').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_LoadEngines');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_InitGammaRamp(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
); register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_InitGammaRamp').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_InitGammaRamp');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_GetAddressInfo(
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
    LogBegin('D3D_CMiniport_GetAddressInfo').
      _(This, 'This').
      _(pLinearAddress, 'pLinearAddress').
      _(ppAddress, 'ppAddress').
      _(pAddressSpace, 'pAddressSpace').
      _(IsAlias, 'IsAlias').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_GetAddressInfo');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_SetDmaRange(
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
    LogBegin('D3D_CMiniport_SetDmaRange').
      _(This, 'This').
      _(DmaContext, 'DmaContext').
      _(pSurface, 'pSurface').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_SetDmaRange');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_ShutdownEngines(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid); register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_ShutdownEngines').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_ShutdownEngines');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_DestroyTile(
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
    LogBegin('D3D_CMiniport_DestroyTile').
      _(This, 'This').
      _(TileRegion, 'TileRegion').
      _(ZOffset, 'ZOffset').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_DestroyTile');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_TilingUpdateIdle(
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
    LogBegin('D3D_CMiniport_TilingUpdateIdle').
      _(This, 'This').
      _(dmapush, 'dmapush').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_TilingUpdateIdle');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalMcControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalMcControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalMcControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalFbControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalFbControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalFbControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalDacControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalDacControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalDacControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalVideoControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalVideoControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalVideoControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalGrControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalGrControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalGrControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalFifoControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalFifoControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalFifoControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalGrControlLoad(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalGrControlLoad').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalGrControlLoad');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalGrIdle(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalGrIdle').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalGrIdle');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalGrLoadChannelContext(
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
    LogBegin('D3D_CMiniport_HalGrLoadChannelContext').
      _(This, 'This').
      _(ChID, 'ChID').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalGrLoadChannelContext');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalGrUnloadChannelContext(
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
    LogBegin('D3D_CMiniport_HalGrUnloadChannelContext').
      _(This, 'This').
      _(ChID, 'ChID').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalGrUnloadChannelContext');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalFifoControlLoad(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalFifoControlLoad').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalFifoControlLoad');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalFifoContextSwitch(
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
    LogBegin('D3D_CMiniport_HalFifoContextSwitch').
      _(This, 'This').
      _(ChID, 'ChID').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalFifoContextSwitch');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalMpControlInit(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalMpControlInit').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalMpControlInit');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalDacUnload(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalDacUnload').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalDacUnload');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalDacProgramMClk(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalDacProgramMClk').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalDacProgramMClk');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalDacProgramNVClk(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalDacProgramNVClk').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalDacProgramNVClk');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalDacProgramPClk(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalDacProgramPClk').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalDacProgramPClk');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalFifoHashAdd(
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
    LogBegin('D3D_CMiniport_HalFifoHashAdd').
      _(This, 'This').
      _(entry, 'entry').
      _(handle, 'handle').
      _(chid, 'chid').
      _(instance, 'instance').
      _(engine, 'engine').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalFifoHashAdd');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalGrInitObjectContext(
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
    LogBegin('D3D_CMiniport_HalGrInitObjectContext').
      _(This, 'This').
      _(Instance, 'Instance').
      _(ClassNum, 'ClassNum').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalGrInitObjectContext');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalGrInit3d(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalGrInit3d').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalGrInit3d');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_HalDacLoad(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
);register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_HalDacLoad').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_HalDacLoad');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_GetPresentFlagsFromAvInfo(
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
    LogBegin('D3D_CMiniport_GetPresentFlagsFromAvInfo').
      _(This, 'This').
      _(AvInfo, 'AvInfo').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_GetPresentFlagsFromAvInfo');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_GetDisplayCapabilities(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): DWORD; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_GetDisplayCapabilities').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_GetDisplayCapabilities');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_IsFlipPending(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): _bool; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_GetDisplayCapabilities').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_GetDisplayCapabilities');
  Result := TRUE;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_VBlankFlip(
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
    LogBegin('D3D_CMiniport_VBlankFlip').
      _(This, 'This').
      _(Offset, 'Offset').
      _(FlipTime, 'FlipTime').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_VBlankFlip');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_VBlank(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): ULONG; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_VBlank').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_VBlank');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_ServiceGrInterrupt(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): ULONG; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_ServiceGrInterrupt').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_ServiceGrInterrupt');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_ServiceFifoInterrupt(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): ULONG; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_ServiceFifoInterrupt').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_ServiceFifoInterrupt');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_ServiceMediaPortInterrupt(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): ULONG; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_ServiceMediaPortInterrupt').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_ServiceMediaPortInterrupt');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_ServiceVideoInterrupt(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid
): ULONG; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_ServiceVideoInterrupt').
      _(This, 'This').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_ServiceVideoInterrupt');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuD3D_CMiniport_SoftwareMethod(
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
    LogBegin('D3D_CMiniport_ServiceVideoInterrupt').
      _(This, 'This').
      _(Method, 'Method').
      _(Data, 'Data').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_ServiceVideoInterrupt');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuD3D_CMiniport_SetVideoMode(
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
    // D3D_CMiniport_SetVideoMode(v8 + 8968, *(_DWORD *)a2, *(_DWORD *)(a2 + 4), *(_DWORD *)(a2 + 44),
   //   *(_DWORD *)(a2 + 40), *(_DWORD *)(a2 + 8), *(_DWORD *)(a2 + 48), v7);
    LogBegin('D3D_CMiniport_SetVideoMode').
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

  Unimplemented('D3D_CMiniport_SetVideoMode');
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

//  XTL_EmuD3D_CMiniport_ApplyLGSFix, // not implemented
  XTL_EmuD3D_CMiniport_BindToChannel,
  XTL_EmuD3D_CMiniport_CreateCtxDmaObject,
  XTL_EmuD3D_CMiniport_CreateGrObject,
  XTL_EmuD3D_CMiniport_CreateTile,
  XTL_EmuD3D_CMiniport_DacProgramGammaRamp,
//  XTL_EmuD3D_CMiniport_DacProgramVideoStart, // not implemented
  XTL_EmuD3D_CMiniport_DestroyTile,
//  XTL_EmuD3D_CMiniport_DisableInterrupts, // not implemented
//  XTL_EmuD3D_CMiniport_DisableInterrupts, // not implemented
//  XTL_EmuD3D_CMiniport_Dpc, // not implemented
  XTL_EmuD3D_CMiniport_DumpClocks,
//  XTL_EmuD3D_CMiniport_EnableInterrupts, // not implemented
//  XTL_EmuD3D_CMiniport_EnableTimer, // not implemented
//  XTL_EmuD3D_CMiniport_FixupPushBuffer, // not implemented
  XTL_EmuD3D_CMiniport_GetAddressInfo,
  XTL_EmuD3D_CMiniport_GetDisplayCapabilities,
  XTL_EmuD3D_CMiniport_GetGeneralInfo,
  XTL_EmuD3D_CMiniport_GetPresentFlagsFromAvInfo,
//  XTL_EmuD3D_CMiniport_GetRefreshRate, // not implemented
//  XTL_EmuD3D_CMiniport_GetTime, // not implemented
//  XTL_EmuD3D_CMiniport_GetTimerTime, // not implemented
//  XTL_EmuD3D_CMiniport_GetTSC, // not implemented
//  XTL_EmuD3D_CMiniport_GrDone, // not implemented
  XTL_EmuD3D_CMiniport_HalDacControlInit,
  XTL_EmuD3D_CMiniport_HalDacLoad,
  XTL_EmuD3D_CMiniport_HalDacProgramMClk,
  XTL_EmuD3D_CMiniport_HalDacProgramNVClk,
  XTL_EmuD3D_CMiniport_HalDacProgramPClk,
  XTL_EmuD3D_CMiniport_HalDacUnload,
  XTL_EmuD3D_CMiniport_HalFbControlInit,
//  XTL_EmuD3D_CMiniport_HalFifoAllocDMA, // not implemented
  XTL_EmuD3D_CMiniport_HalFifoContextSwitch,
  XTL_EmuD3D_CMiniport_HalFifoControlInit,
  XTL_EmuD3D_CMiniport_HalFifoControlLoad,
  XTL_EmuD3D_CMiniport_HalFifoHashAdd,
  XTL_EmuD3D_CMiniport_HalGrControlInit,
  XTL_EmuD3D_CMiniport_HalGrControlLoad,
  XTL_EmuD3D_CMiniport_HalGrIdle,
  XTL_EmuD3D_CMiniport_HalGrInit3d,
  XTL_EmuD3D_CMiniport_HalGrInitObjectContext,
  XTL_EmuD3D_CMiniport_HalGrLoadChannelContext,
  XTL_EmuD3D_CMiniport_HalGrUnloadChannelContext,
  XTL_EmuD3D_CMiniport_HalMcControlInit,
  XTL_EmuD3D_CMiniport_HalMpControlInit,
  XTL_EmuD3D_CMiniport_HalVideoControlInit,
  XTL_EmuD3D_CMiniport_InitDMAChannel,
  XTL_EmuD3D_CMiniport_InitEngines,
  XTL_EmuD3D_CMiniport_InitGammaRamp,
  XTL_EmuD3D_CMiniport_InitHardware,
//  XTL_EmuD3D_CMiniport_IsOddField, // not implemented
  XTL_EmuD3D_CMiniport_IsFlipPending,
//  XTL_EmuD3D_CMiniport_Isr, // not implemented
  XTL_EmuD3D_CMiniport_LoadEngines,
  XTL_EmuD3D_CMiniport_MapRegisters,
  XTL_EmuD3D_CMiniport_ReserveInstMem,
  XTL_EmuD3D_CMiniport_ServiceFifoInterrupt,
  XTL_EmuD3D_CMiniport_ServiceGrInterrupt,
  XTL_EmuD3D_CMiniport_ServiceMediaPortInterrupt,
//  XTL_EmuD3D_CMiniport_ServiceQueuedFlips, // not implemented
//  XTL_EmuD3D_CMiniport_ServiceTimerInterrupt, // not implemented
  XTL_EmuD3D_CMiniport_ServiceVideoInterrupt,
  XTL_EmuD3D_CMiniport_SetDmaRange,
//  XTL_EmuD3D_CMiniport_SetTimerAlarm, // not implemented
//  XTL_EmuD3D_CMiniport_SetTimerCallbackAlarm, // not implemented
  XTL_EmuD3D_CMiniport_SetVideoMode,
  XTL_EmuD3D_CMiniport_ShutdownEngines,
//  XTL_EmuD3D_CMiniport_ShutdownNotification, // not implemented
  XTL_EmuD3D_CMiniport_SoftwareMethod,
  XTL_EmuD3D_CMiniport_TilingUpdateIdle,
  XTL_EmuD3D_CMiniport_VBlank,
  XTL_EmuD3D_CMiniport_VBlankFlip
  ;


end.

