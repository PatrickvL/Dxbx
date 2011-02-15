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
  // Jedi Win32API
  , JwaWinType
  , JwaNative
  // DirectX
{$IFDEF DXBX_USE_D3D9}
  , Direct3D9
  , D3DX9
{$ELSE}
  , Direct3D8 // D3DDEVTYPE
  , D3DX8 // PD3DXVECTOR4
{$ENDIF}
  // Dxbx
  , uConsts
  , uTypes
  , uLog
  , uTime // DxbxTimer
  , uDxbxUtils // sscanf
  , uDxbxKrnl // g_CPUXbox
  , uDxbxKrnlUtils // DxbxKrnlCleanup
  , uEmuD3D8Types
  , uEmuD3D8Utils // iif
  , uConvert // EmuXB2PC_D3DMULTISAMPLE_TYPE
  , uState // XTL_D3D__Device
  , uXboxLibraryUtils // PatchPrefix
  , uEmu // g_hEmuWindow
  , uEmuKrnl // Unimplemented
  , uEmuFS
  , uEmuAlloc
  ;

type
  OBJECTINFO = record
    Handle: ULONG;
    SubChannel: USHORT;
    Engine: USHORT;
    ClassNum: ULONG;
    Instance: ULONG;
  end;
  POBJECTINFO = ^OBJECTINFO;

  Nv2AControlDma = record
    Ignored: array [0..$10-1] of DWORD;
    Put: PDWord; // On Xbox1, this field is only written to by the CPU (the GPU uses this as a trigger to start executing from the given address)
    Get: PDWord; // On Xbox1, this field is only read from by the CPU (the GPU reflects in here where it is/stopped executing)
    Reference: UInt32;
    Ignored2: array [0..$7ED-1] of DWORD;
  end;
  PNv2AControlDma = ^Nv2AControlDma;
  PPNv2AControlDma = ^PNv2AControlDma;

const
  NV2A_PFB_WC_CACHE = $00100410; // pbKit
  NV2A_PFB_WC_CACHE_FLUSH_TRIGGER = $00010000; // pbKit

var
  g_NV2ADMAChannel: PNv2AControlDma = nil;
  m_pCPUTime: PDWORD = nil;
  m_pGPUTime: PDWORD = nil;

procedure DxbxLogPushBufferPointers(heading: string);

implementation

uses
  uPushBuffer, // test
  uEmuD3D8; // g_EmuCDPD

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
    LogBegin('D3D_CMiniport_InitHardware').
      _(This, 'This').
    LogEnd();
  end;

  GPURegisterBase := DxbxCalloc(2*1024*1024, 1); // PatrickvL : I've seen RegisterBase offsets up to $00100410 so 2 MB should suffice
  PPointer(This)^ := GPURegisterBase;
  DbgPrintf('Allocated a block of 2 MB to serve as the GPUs RegisterBase at 0x%.08x', [GPURegisterBase]);

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

  case a2 of
    8:  // = notification of semaphore address
    begin
      // Remember where the semaphore (starting with a GPU Time DWORD) was allocated
      // (we could have trapped MmAllocateContiguousMemoryEx too, but this is simpler) :
      m_pGPUTime := PDWORD(a4);
      DbgPrintf('Registered m_pGPUTime at 0x%0.8x', [UIntPtr(m_pGPUTime)]);
    end;
  else
    Unimplemented('D3D_CMiniport_CreateCtxDmaObject');
  end;

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

procedure DxbxLogPushBufferPointers(heading: string);
var
  Pusher: PPusher;
  NV2ADMAChannel: PNv2AControlDma;
begin
  Pusher := PPusher(PPointer(XTL_D3D__Device)^);
  NV2ADMAChannel := g_NV2ADMAChannel;
  LogBegin(heading).
    _(NV2ADMAChannel, 'NV2ADMAChannel').
    _(NV2ADMAChannel.Put, 'NV2ADMAChannel.Put').
    _(NV2ADMAChannel.Get, 'NV2ADMAChannel.Get').
    _(NV2ADMAChannel.Reference, 'NV2ADMAChannel.Reference').
    _(Pusher, 'Pusher').
    _(Pusher.m_pPut, 'Pusher.m_pPut').
    _(Pusher.m_pThreshold, 'Pusher.m_pThreshold').
  LogEnd();
end;

function XTL_EmuD3D_CMiniport_InitDMAChannel(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {6 stack}ppChannel: PPNv2AControlDma;
  {5 stack}Offset: ULONG;
  {4 stack}DataContext: POBJECTINFO;
  {3 stack}ErrorContext: POBJECTINFO;
  {2 stack}Class_: ULONG
  ): int; register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:PatrickvL  Done:0
var
  dwThreadId: DWORD;
  i: Integer;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    // D3D_CMiniport_InitDMAChannel(v8 + 8968, 8302, 0, &v22, 0, &v23);
    LogBegin('D3D_CMiniport_InitDMAChannel').
      _(This, 'This').
      _(Class_, 'Class').
      _(ErrorContext, 'ErrorContext').
      _(DataContext, 'DataContext').
      _(Offset, 'Offset').
      _(ppChannel, 'ppChannel').
    LogEnd();
  end;

  // Allocate a fake DMA channel :
  g_NV2ADMAChannel := PNv2AControlDma(DxbxCalloc(SizeOf(Nv2AControlDma), 1));

  // Create our DMA pushbuffer 'handling' thread :
  begin
    dwThreadId := 0;
    {hThread :=} CreateThread(nil, 0, @EmuThreadHandleNV2ADMA, nil, 0, {var}dwThreadId);
//    // Make sure callbacks run on the same core as the one that runs Xbox1 code :
    SetThreadAffinityMask(dwThreadId, g_CPUXbox);
    // If possible, assign this thread to another core than the one that runs Xbox1 code :
//    SetThreadAffinityMask(dwThreadId, g_CPUOthers);
  end;

  // Return the channel :
  ppChannel^ := g_NV2ADMAChannel;

  // Also, find the address of the CPU Time variable (this is a temporary solution, until we have
  // a technique to read members from all g_pDevice (D3DDevice) versions in a generic way);
  begin
    // Walk through a few members of the D3D device struct :
    m_pCPUTime := PPointer(XTL_D3D__Device)^;
    DbgPrintf('Searching for m_pCPUTime from 0x%0.8x (m_pGPUTime is at 0x%0.8x)', [UIntPtr(m_pCPUTime), UIntPtr(m_pGPUTime)]);

    for i := 0 to 32 do
    begin
      if i = 32 then
        DxbxKrnlCleanup('m_pCPUTime not found!');

      // Look for the offset of the GPUTime pointer inside the D3D g_pDevice struct :
      if m_pCPUTime^ = DWORD(m_pGPUTime) then
      begin
        // The CPU time variable is located right before the GPUTime pointer :
        Dec(m_pCPUTime);
        Break;
      end;

      Inc(m_pCPUTime);
    end;
  end;

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
  Result := 0; // This must be 0, as it's used as a correction over RenderTarget and ZBuffer Data pointers!

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

(* Too high level
function XTL_EmuD3D_CMiniport_GetPresentFlagsFromAvInfo(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {2 stack}AvInfo: DWORD
): DWORD; register;// thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:Shadow_tj  Done:0
*)

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

procedure DxbxPostponedCreateDeviceViaSetRenderTarget
(
  pRenderTarget: PX_D3DSurface;
  pNewZStencil: PX_D3DSurface
); stdcall;
var
  hRet: HRESULT;
begin
  // If the g_EmuD3DActiveRenderTarget is not yet assigned, we're probably dealing with
  // the first call, right after creating the D3D device. Check this and couple the
  // native backbuffer to this buffer :
  if Assigned(pRenderTarget) then
  begin
    hRet := IDirect3DDevice_GetRenderTarget(g_pD3DDevice, @(pRenderTarget.Emu.Surface));
    if FAILED(hRet) then
      DxbxD3DError('XTL_EmuD3DDevice_SetRenderTarget', 'Initial IDirect3DDevice_GetRenderTarget failed!', pRenderTarget, hRet);
    IDirect3DSurface(pRenderTarget.Emu.Surface)._Release;

    // Until we can read the Xbox device frame buffers, fake them here :
    g_EmuD3DFrameBuffers[0] := pRenderTarget;
    g_EmuD3DFrameBuffers[1] := pRenderTarget;
    g_EmuD3DFrameBuffers[2] := pRenderTarget;
  end;

  if Assigned(pNewZStencil) then
  begin
    hRet := IDirect3DDevice_GetDepthStencilSurface(g_pD3DDevice, @(pNewZStencil.Emu.Surface));
    if FAILED(hRet) then
      DxbxD3DError('XTL_EmuD3DDevice_SetRenderTarget', 'Initial IDirect3DDevice_GetDepthStencilSurface failed!', pNewZStencil, hRet);
    IDirect3DSurface(pNewZStencil.Emu.Surface)._Release;
  end;
end;

procedure XTL_EmuD3D_CMiniport_SetVideoMode(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {0 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}This: Pvoid;
  {8 stack}FrontBufferPitch: DWORD;
  {7 stack}FullScreen_PresentationInterval: DWORD;
  {6 stack}BackBufferFormat: X_D3DFORMAT;
  {5 stack}Flags: DWORD;
  {4 stack}FullScreen_RefreshRateInHz: DWORD;
  {3 stack}BackBufferHeight: DWORD;
  {2 stack}BackBufferWidth: DWORD
  ); register; // thiscall simulation - See Translation guide
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
  begin
    LogBegin('D3D_CMiniport_SetVideoMode').
      _(This, 'This').
      _(BackBufferWidth, 'BackBufferWidth').
      _(BackBufferHeight, 'BackBufferHeight').
      _(FullScreen_RefreshRateInHz, 'FullScreen_RefreshRateInHz').
      _(Flags, 'Flags').
      _(BackBufferFormat, 'BackBufferFormat').
      _(FullScreen_PresentationInterval, 'FullScreen_PresentationInterval').
      _(FrontBufferPitch, 'FrontBufferPitch').
    LogEnd();
  end;

  Unimplemented('D3D_CMiniport_SetVideoMode');

{$IFNDEF PUSHBUFFER_ONLY}
  // Intercept the (already created) Xbox backbuffer and depthbuffer formats via the first call to SetRenderTarget:
  DxbxOnSetRenderTarget := DxbxPostponedCreateDeviceViaSetRenderTarget;
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

// This unit attempts to run Direct3D_CreateDevice, D3D__CDevice__Init and D3D__CDevice_UnInit unpatched.
//
// D3D__CDevice__Init calls these methods, that should still be patched (if not already) :
//
//      D3D__BusyLoop(); // Waits until the current pushbuffer contents are handled (we have EmuThreadHandleNV2ADMA for that)
//    D3D__InitializeHardware(); // Just send a few commands to the pushbuffer (no need to patch that)
//    result = D3D__CDevice__InitializeFrameBuffers(v8, a2); // Creates the backbuffer(s) and (optionally) a depthbuffer
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
//  XTL_EmuD3D_CMiniport_GetPresentFlagsFromAvInfo, // Too high level
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
  XTL_EmuD3D_CMiniport_IsFlipPending,
//  XTL_EmuD3D_CMiniport_IsOddField, // not implemented
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

