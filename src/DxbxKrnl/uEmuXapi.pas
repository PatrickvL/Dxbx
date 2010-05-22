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
unit uEmuXapi;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Types, // E_FAIL
  Windows,
  MMSystem, // SEEK_SET
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  JwaWinBase,
  JwaNative,
  // Dxbx
  uTypes,
  uDxbxUtils,
  uLog, // DbgPrintf
  uXbe,
  uEmu,
  uEmuFS, // EmuSwapFS
  uEmuAlloc,
  uEmuDInput,
  uXBController,
  uXboxLibraryUtils, // PatchPrefix
  uDxbxKrnlUtils; // DxbxKrnl_XbeHeader

var
  XTL_EmuXapiProcessHeap: PPVOID;

type
  LPTIMECALLBACK = TFNTIMECALLBACK;
  ProcedureStdCall = procedure; stdcall;

  StartRoutineFunc = function (const StartContext: PVOID): Int; stdcall;

  LPSECURITY_ATTRIBUTES = PVOID;

  PRTL_HEAP_COMMIT_ROUTINE = function(
    Base: PVOID;
    OUT CommitAddress: PPVOID;
    OUT CommitSize: PSIZE_T
    ): NTSTATUS; stdcall;

type _XINPUT_POLLING_PARAMETERS = packed record
  private
    _Flag: Byte;
    function GetBits(const aIndex: Integer): Byte;
  public
    property fAutoPoll: BYTE index $0001 read GetBits; // 1 bit at offset 0
    property fInterruptOut: BYTE index $0101 read GetBits; // 1 bit at offset 1
    property ReservedMBZ1: BYTE index $0206 read GetBits; // 6 bits at offset 2
  public
    bInputInterval: BYTE;
    bOutputInterval: BYTE;
    ReservedMBZ2: BYTE;
end;
XINPUT_POLLING_PARAMETERS = _XINPUT_POLLING_PARAMETERS;
PXINPUT_POLLING_PARAMETERS = ^XINPUT_POLLING_PARAMETERS;

type _POLLING_PARAMETERS_HANDLE = packed record
    pPollingParameters: PXINPUT_POLLING_PARAMETERS;
    
    dwPort: DWORD;
end;
POLLING_PARAMETERS_HANDLE = _POLLING_PARAMETERS_HANDLE;
PPOLLING_PARAMETERS_HANDLE = ^POLLING_PARAMETERS_HANDLE;

type _XPP_DEVICE_TYPE = packed record
    // Note : Cxbx has size 3, but XTL_EmuXGetDevices seems to indicate size 4 :
    Reserved: array [0..4-1] of ULONG;
end;
XPP_DEVICE_TYPE = _XPP_DEVICE_TYPE;
PXPP_DEVICE_TYPE = ^XPP_DEVICE_TYPE;

type _XINPUT_GAMEPAD = packed record
    wButtons: WORD;
    bAnalogButtons: array [0..8-1] of BYTE;
    sThumbLX: SHORT;
    sThumbLY: SHORT;
    sThumbRX: SHORT;
    sThumbRY: SHORT;
end;
XINPUT_GAMEPAD = _XINPUT_GAMEPAD;
PXINPUT_GAMEPAD = ^XINPUT_GAMEPAD;

type _XINPUT_RUMBLE = packed record
    wLeftMotorSpeed: WORD;
    wRightMotorSpeed: WORD;
end;
XINPUT_RUMBLE = _XINPUT_RUMBLE;
PXINPUT_RUMBLE = ^XINPUT_RUMBLE;

type _XINPUT_CAPABILITIES = packed record
    SubType: BYTE;
    Reserved: WORD;

    In_: record
      Gamepad: XINPUT_GAMEPAD;
    end;

    Out_: record
      Rumble: XINPUT_RUMBLE;
    end;
end;
XINPUT_CAPABILITIES = _XINPUT_CAPABILITIES;
PXINPUT_CAPABILITIES = ^XINPUT_CAPABILITIES;

// Device SubTypes
const XINPUT_DEVSUBTYPE_GC_GAMEPAD              = $01;
const XINPUT_DEVSUBTYPE_GC_GAMEPAD_ALT          = $02;
const XINPUT_DEVSUBTYPE_GC_WHEEL                = $10;
const XINPUT_DEVSUBTYPE_GC_ARCADE_STICK         = $20;
const XINPUT_DEVSUBTYPE_GC_DIGITAL_ARCADE_STICK = $21;
const XINPUT_DEVSUBTYPE_GC_FLIGHT_STICK         = $30;
const XINPUT_DEVSUBTYPE_GC_SNOWBOARD            = $40;

type _XINPUT_FEEDBACK_HEADER = packed record
    dwStatus: DWORD;
    hEvent: HANDLE; // OPTIONAL ;
    Reserved: array [0..58-1] of BYTE;
end;
XINPUT_FEEDBACK_HEADER = _XINPUT_FEEDBACK_HEADER;
PXINPUT_FEEDBACK_HEADER = ^XINPUT_FEEDBACK_HEADER;

type _XINPUT_FEEDBACK = packed record
    Header: XINPUT_FEEDBACK_HEADER;
    // union
    Rumble: XINPUT_RUMBLE;
end;
XINPUT_FEEDBACK = _XINPUT_FEEDBACK;
PXINPUT_FEEDBACK = ^XINPUT_FEEDBACK;

type _RTL_HEAP_PARAMETERS = packed record
    Length: ULONG;
    SegmentReserve: SIZE_T;
    SegmentCommit: SIZE_T;
    DeCommitFreeBlockThreshold: SIZE_T;
    DeCommitTotalFreeThreshold: SIZE_T;
    MaximumAllocationSize: SIZE_T;
    VirtualMemoryThreshold: SIZE_T;
    InitialCommit: SIZE_T;
    InitialReserve: SIZE_T;
    CommitRoutine: PRTL_HEAP_COMMIT_ROUTINE;
    Reserved: array [0..2-1] of SIZE_T;
// Was:
//    Length: UInt32;
//    Unknown: array [0..$2C-1] of BYTE;
end;
RTL_HEAP_PARAMETERS = _RTL_HEAP_PARAMETERS;
PRTL_HEAP_PARAMETERS = ^RTL_HEAP_PARAMETERS;

type _RTL_HEAP_DEFINITION = packed record
    Length: ULONG;
    Unknown: array [0..11-1] of ULONG;
end;
RTL_HEAP_DEFINITION = _RTL_HEAP_DEFINITION;
PRTL_HEAP_DEFINITION = ^RTL_HEAP_DEFINITION;

type XTHREAD_NOTIFY_PROC = procedure(fCreate: BOOL); stdcall;

type _XTHREAD_NOTIFICATION = packed record
    Reserved: LIST_ENTRY;
    pfnNotifyRoutine: XTHREAD_NOTIFY_PROC;
end;
XTHREAD_NOTIFICATION = _XTHREAD_NOTIFICATION;
PXTHREAD_NOTIFICATION = ^XTHREAD_NOTIFICATION;

const XCALCSIG_SIGNATURE_SIZE = 20;
  
type _XCALCSIG_SIGNATURE = packed record
    Signature: array [0..XCALCSIG_SIGNATURE_SIZE-1] of BYTE;
end;
XCALCSIG_SIGNATURE = _XCALCSIG_SIGNATURE;
PXCALCSIG_SIGNATURE = ^XCALCSIG_SIGNATURE;

const XCALCSIG_FLAG_NON_ROAMABLE = $00000001;

const MAX_LAUNCH_DATA_SIZE = 1024 * 3;

type _LAUNCH_DATA = packed record
  Data: array [0..MAX_LAUNCH_DATA_SIZE] of BYTE;
end;
LAUNCH_DATA = _LAUNCH_DATA;
PLAUNCH_DATA = ^LAUNCH_DATA;

const LDT_TITLE                 = 0;
const LDT_FROM_DASHBOARD        = 2;
const LDT_FROM_DEBUGGER_CMDLINE = 3;
const LDT_FROM_UPDATE           = 4;

// XInputSetState status waiters
var g_pXInputSetStateStatus: array [0..XINPUT_SETSTATE_SLOTS - 1] of XInputSetStateStatus;

// XInputOpen handles
var g_hInputHandle: array [0..XINPUT_HANDLE_SLOTS - 1] of HANDLE;

// Xbe section list
var g_SectionList: PXBE_SECTIONLIST;
// Number of sections
var g_NumSections: int;

var g_bXLaunchNewImageCalled: _bool = false;
var g_bXInputOpenCalled: _bool = false;

// Note : Cxbx log indicates 'g_pRtlCreateHeap' is indeed
// at the same address as 'EmuRtlCreateHeap'.

type
  XTL_SECTIONHANDLE = PXBE_SECTIONHEADER;

function XTL_EmuXLoadSectionByHandle
(
    hSection: XTL_SECTIONHANDLE
): LPVOID; stdcall; // published function, used for xboxkrnl_XeLoadSection

function XTL_EmuXFreeSectionByHandle
(
    hSection: XTL_SECTIONHANDLE
): BOOL; stdcall; // published function, used for xboxkrnl_XeUnloadSection

implementation

uses
  // Dxbx
  uEmuKrnlPs; // g_pfnThreadNotification

function _XINPUT_POLLING_PARAMETERS.GetBits(const aIndex: Integer): Byte;
begin
  Result := GetByteBits(_Flag, aIndex);
end;

const
  HEAP_HEADERSIZE = $20; // Dxbx note : Presumably, Xbox allocator uses $20 as alignment

// Saved launch data
var g_SavedLaunchData: {XTL_}LAUNCH_DATA;

procedure XTL_EmuXapiApplyKernelPatches(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuXapi : EmuXapiApplyKernelPatches()');
  EmuSwapFS(fsXbox);
{$ENDIF}

  // we dont really feel like patching, now do we?
end;

function XTL_EmuXFormatUtilityDrive(): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuXapi : EmuXFormatUtilityDrive()');
  EmuSwapFS(fsXbox);
{$ENDIF}

  // TODO -oCXBX: yeah... we'll format... riiiiight

  Result := BOOL_TRUE;
end;

function XTL_EmuGetTimeZoneInformation
(
  {Out}lpTimeZoneInformation: LPTIME_ZONE_INFORMATION
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuGetTimeZoneInformation' +
      #13#10'(' +
      #13#10'   lpTimeZoneInformation : 0x%.08X' +
      #13#10');',
      [lpTimeZoneInformation]);
{$ENDIF}

  Result := GetTimeZoneInformation({var}lpTimeZoneInformation^);

  EmuSwapFS(fsXBox);
end;


function XTL_EmuRtlCreateHeap
(
  Flags: ULONG;
  Base: PVOID; // OPTIONAL
  Reserve: ULONG; // OPTIONAL
  Commit: ULONG;
  Lock: PVOID; // OPTIONAL
  RtlHeapParams: PVOID // OPTIONAL
): PVOID; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  RtlHeapDefinition: RTL_HEAP_DEFINITION;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuRtlCreateHeap' +
    #13#10'(' +
    #13#10'   Flags               : 0x%.08X' +
    #13#10'   Base                : 0x%.08X' +
    #13#10'   Reserve             : 0x%.08X' +
    #13#10'   Commit              : 0x%.08X' +
    #13#10'   Lock                : 0x%.08X' +
    #13#10'   RtlHeapParams       : 0x%.08X' +
    #13#10');',
    [Flags, Base, Reserve, Commit, Lock, RtlHeapParams]);
{$ENDIF}

  ZeroMemory(@RtlHeapDefinition, sizeof(RtlHeapDefinition));

  RtlHeapDefinition.Length := sizeof(RtlHeapDefinition);

  Result := PVOID(JwaNative.RtlCreateHeap(
    Flags, Base, Reserve, Commit, Lock, @RtlHeapDefinition));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlAllocateHeap
(
  hHeap: HANDLE;
  dwFlags: DWORD;
  dwBytes: SIZE_T
): PVOID; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  AllocatedAddress: PVOID;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuRtlAllocateHeap' +
      #13#10'(' +
      #13#10'   hHeap               : 0x%.08X' +
      #13#10'   dwFlags             : 0x%.08X' +
      #13#10'   dwBytes             : 0x%.08X' +
      #13#10');',
      [hHeap, dwFlags, dwBytes]);
{$ENDIF}

  if dwBytes > 0 then
    Inc(dwBytes, HEAP_HEADERSIZE);

  AllocatedAddress := CxbxRtlAlloc(hHeap, dwFlags, dwBytes);
  if Assigned(AllocatedAddress) then
  begin
    Result := PVOID(RoundUp(uint32(AllocatedAddress) + 1, HEAP_HEADERSIZE));
    (PPointer(Result)-1)^ := AllocatedAddress;
  end
  else
    Result := nil;

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('pRet : 0x%.08X', [Result]);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlFreeHeap
(
  hHeap: HANDLE;
  dwFlags: DWORD;
  lpMem: PVOID
): BOOL; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DXBX_DEBUG_TRACE}
  DbgPrintf('EmuXapi : EmuRtlFreeHeap' +
    #13#10'(' +
    #13#10'   hHeap               : 0x%.08X' +
    #13#10'   dwFlags             : 0x%.08X' +
    #13#10'   lpMem               : 0x%.08X' +
    #13#10');',
    [hHeap, dwFlags, lpMem]);
{$ENDIF}

  if Assigned(lpMem) then
    lpMem := (PPointer(lpMem)-1)^;

  Result := CxbxRtlFree(hHeap, dwFlags, lpMem);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlReAllocateHeap
(
  hHeap: HANDLE;
  dwFlags: DWORD;
  lpMem: PVOID;
  dwBytes: SIZE_T
): PVOID; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  AllocatedAddress: PVOID;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuRtlReAllocateHeap' +
      #13#10'('+
      #13#10'   hHeap               : 0x%.08X' +
      #13#10'   dwFlags             : 0x%.08X' +
      #13#10'   lpMem               : 0x%.08X' +
      #13#10'   dwBytes             : 0x%.08X' +
      #13#10');',
      [hHeap, dwFlags, lpMem, dwBytes]);
{$ENDIF}

  // Dxbx note : Realloc cannot be implemented via CxbxRtlRealloc because of possible alignment-mismatches.
  // We solve this by doing a new Alloc, copying over the original lpMem contents and freeing it :

  if dwBytes > 0 then
    Inc(dwBytes, HEAP_HEADERSIZE);

  AllocatedAddress := CxbxRtlAlloc(hHeap, dwFlags, dwBytes);
  if Assigned(AllocatedAddress) then
  begin
    Result := PVOID(RoundUp(uint32(AllocatedAddress) + 1, HEAP_HEADERSIZE));
    (PPointer(Result)-1)^ := AllocatedAddress;
  end
  else
    Result := nil;

  if Assigned(lpMem) then
  begin
    AllocatedAddress := (PPointer(lpMem)-1)^;
    if Assigned(Result) then
      memcpy({destination=}Result, {source=}lpMem, {size=}CxbxRtlSizeHeap(hHeap, dwFlags, AllocatedAddress) - HEAP_HEADERSIZE);

    CxbxRtlFree(hHeap, dwFlags, AllocatedAddress);
  end;

{$IFDEF DXBX_DEBUG_TRACE}
  DbgPrintf('pRet : 0x%.08X', [Result]);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlSizeHeap
(
  hHeap: HANDLE;
  dwFlags: DWORD;
  lpMem: PVOID
): SIZE_T; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuXapi : EmuRtlSizeHeap' +
      #13#10'(' +
      #13#10'   hHeap               : 0x%.08X' +
      #13#10'   dwFlags             : 0x%.08X' +
      #13#10'   lpMem               : 0x%.08X' +
      #13#10');',
      [hHeap, dwFlags, lpMem]);
{$ENDIF}

  if Assigned(lpMem) then
    lpMem := (PPointer(lpMem)-1)^;

  Result := CxbxRtlSizeHeap(hHeap, dwFlags, lpMem);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuQueryPerformanceCounter
(
  lpPerformanceCount: PLARGE_INTEGER
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuQueryPerformanceCounter' +
      #13#10'(' +
      #13#10'   lpPerformanceCount  : 0x%.08X' +
      #13#10');',
      [lpPerformanceCount]);
{$ENDIF}

  Result := BOOL(QueryPerformanceCounter({var}lpPerformanceCount^));

  // debug - 4x speed
  //lpPerformanceCount.QuadPart *= 4;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuQueryPerformanceFrequency
(
  lpFrequency: PLARGE_INTEGER
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuQueryPerformanceFrequency' +
      #13#10'(' +
      #13#10'   lpFrequency         : 0x%.08X' +
      #13#10');',
      [lpFrequency]);
{$ENDIF}

  Result := BOOL(QueryPerformanceFrequency({var}lpFrequency^));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXMountUtilityDrive
(
    fFormatClean: BOOL
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuXapi : EmuXMountUtilityDrive' +
        #13#10'(' +
        #13#10'   fFormatClean        : 0x%.08X' +
        #13#10');', [Ord(fFormatClean)]);
    EmuSwapFS(fsXbox);
  end;
{$ENDIF}

  Result := BOOL_TRUE;
end;

procedure XTL_EmuXInitDevices
(
    Unknown1: DWORD; 
    Unknown2: PVOID
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  v: int;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXInitDevices' +
    #13#10'(' +
    #13#10'   Unknown1            : 0x%.08X' +
    #13#10'   Unknown2            : 0x%.08X' +
    #13#10');',
    [Unknown1, Unknown2]);
{$ENDIF}

  for v := 0 to XINPUT_SETSTATE_SLOTS - 1 do
  begin
    g_pXInputSetStateStatus[v].hDevice := 0;
    g_pXInputSetStateStatus[v].dwLatency := 0;
    g_pXInputSetStateStatus[v].pFeedback := nil;
  end;

  for v := 0 to XINPUT_HANDLE_SLOTS - 1 do
  begin
    g_hInputHandle[v] := 0;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXGetDevices
(
    DeviceType: PXPP_DEVICE_TYPE
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXGetDevices' +
    #13#10'(' +
    #13#10'   DeviceType          : 0x%.08X' +
    #13#10');',
    [DeviceType]);
{$ENDIF}

  Result := 0;

  if (DeviceType.Reserved[0] = 0) and (DeviceType.Reserved[1] = 0) and (DeviceType.Reserved[2] = 0) and (DeviceType.Reserved[3] = 0) then
    Result := (1 shl 0) // Return 1 Controller
  else
    EmuWarning('Unknown DeviceType (0x%.08X, 0x%.08X, 0x%.08X, 0x%.08X)', [DeviceType.Reserved[0], DeviceType.Reserved[1], DeviceType.Reserved[2], DeviceType.Reserved[3]]);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXGetDeviceChanges
(
  DeviceType: PXPP_DEVICE_TYPE;
  pdwInsertions: PDWORD;
  pdwRemovals: PDWORD
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
{$WRITEABLECONST ON}
const
  bFirst: BOOL_ = TRUE;
{$WRITEABLECONST OFF}
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXGetDeviceChanges' +
    #13#10'(' +
    #13#10'   DeviceType          : 0x%.08X' +
    #13#10'   pdwInsertions       : 0x%.08X' +
    #13#10'   pdwRemovals         : 0x%.08X' +
    #13#10');',
    [DeviceType, pdwInsertions, pdwRemovals]);
{$ENDIF}

  Result := BOOL_FALSE;

  // Return 1 Controller Inserted initially, then no changes forever
  if bFirst then
  begin
    pdwInsertions^ := (1 shl 0);
    pdwRemovals^ := 0;
    Result := BOOL_TRUE;
    bFirst := FALSE;
  end
  else
  begin
    pdwInsertions^ := 0;
    pdwRemovals^ := 0;
  end;

  EmuSwapFS(fsXbox);
end;


function XTL_EmuXInputOpen
(
  DeviceType: PXPP_DEVICE_TYPE;
  dwPort: DWORD;
  dwSlot: DWORD;
  pPollingParameters: PXINPUT_POLLING_PARAMETERS // OPTIONAL
): HANDLE; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pPH: PPOLLING_PARAMETERS_HANDLE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXInputOpen' +
      #13#10'(' +
      #13#10'   DeviceType          : 0x%.08X' +
      #13#10'   dwPort              : 0x%.08X' +
      #13#10'   dwSlot              : 0x%.08X' +
      #13#10'   pPollingParameters  : 0x%.08X' +
      #13#10');',
      [DeviceType, dwPort, dwSlot, pPollingParameters]);
{$ENDIF}

  pPH := nil;

  if {not nessecary : (dwPort >= 0) and} (dwPort <= 3) then
  begin
    if (g_hInputHandle[dwPort] = 0) then
    begin
      New({var PPOLLING_PARAMETERS_HANDLE}pPH);

      if (pPollingParameters <> NULL) then
      begin
        New({var XINPUT_POLLING_PARAMETERS}pPH.pPollingParameters);
        memcpy(pPollingParameters, pPH.pPollingParameters, sizeof(XINPUT_POLLING_PARAMETERS));
      end
      else
      begin
        pPH.pPollingParameters := NULL;
      end;

      g_hInputHandle[dwPort] := HANDLE(pPH);
    end
    else
    begin
      pPH := PPOLLING_PARAMETERS_HANDLE(g_hInputHandle[dwPort]);

      if (pPollingParameters <> nil) then
      begin
        if (pPH.pPollingParameters = nil) then
        begin
          New({var XINPUT_POLLING_PARAMETERS}pPH.pPollingParameters);
        end;

        memcpy(pPollingParameters, pPH.pPollingParameters, sizeof(XINPUT_POLLING_PARAMETERS));
      end
      else
      begin
        if (pPH.pPollingParameters <> nil) then
        begin
          Dispose(pPH.pPollingParameters);
          pPH.pPollingParameters := nil;
        end;
      end;
    end;

    pPH.dwPort := dwPort;
  end;

  g_bXInputOpenCalled := true;
  EmuSwapFS(fsXbox);

  Result := HANDLE(pPH);
end;


procedure XTL_EmuXInputClose
(
    hDevice: HANDLE
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
{var
  pPH: PPOLLING_PARAMETERS_HANDLE;} // DXBX - pph never used
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXInputClose' +
    #13#10'(' +
    #13#10'   hDevice             : 0x%.08X' +
    #13#10');',
    [hDevice]);
{$ENDIF}

  {pPH := PPOLLING_PARAMETERS_HANDLE(hDevice);} // DXBX - pph never used

  {  Markd out by CXBX
   no longer necessary
  if Assigned(pPH) then
  begin
    Integer v;

    for(v := 0;v<XINPUT_SETSTATE_SLOTS;v++)
    begin
      if (g_pXInputSetStateStatus[v].hDevice = hDevice) then
      begin
        // remove from slot
        g_pXInputSetStateStatus[v].hDevice := 0;
        g_pXInputSetStateStatus[v].pFeedback := 0;
        g_pXInputSetStateStatus[v].dwLatency := 0;
      end;
    end;

    if Assigned(pPH.pPollingParameters) then
      Dispose(pPH.pPollingParameters);

    Dispose(pPH);
   end;
  }

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXInputPoll
(
    hDevice: HANDLE
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  v: int;
  pFeedback: PXINPUT_FEEDBACK;
  {pPH: PPOLLING_PARAMETERS_HANDLE;} // DXBX - pph never used
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXInputPoll' +
      #13#10'(' +
      #13#10'   hDevice             : 0x%.08X' +
      #13#10');',
      [hDevice]);
{$ENDIF}

  {pPH := PPOLLING_PARAMETERS_HANDLE(hDevice);} // DXBX - pph never used

  //
  // Poll input
  //

  begin
    for v := 0 to XINPUT_SETSTATE_SLOTS - 1 do
    begin
      hDevice := g_pXInputSetStateStatus[v].hDevice;

      if hDevice = 0 then
          continue;

      g_pXInputSetStateStatus[v].dwLatency := 0;

      pFeedback := PXINPUT_FEEDBACK(g_pXInputSetStateStatus[v].pFeedback);

      if pFeedback = nil then
        continue;

      //
      // Only update slot if it has not already been updated
      //

      if (pFeedback.Header.dwStatus <> ERROR_SUCCESS) then
      begin
        if (pFeedback.Header.hEvent <> 0) then
        begin
          SetEvent(pFeedback.Header.hEvent);
        end;

        pFeedback.Header.dwStatus := ERROR_SUCCESS;
      end;
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := ERROR_SUCCESS;
end;

function XTL_EmuXInputGetCapabilities
(
  hDevice: HANDLE;
  {OUT} pCapabilities: PXINPUT_CAPABILITIES
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pPH: PPOLLING_PARAMETERS_HANDLE;
  dwPort: DWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXInputGetCapabilities' +
      #13#10'(' +
      #13#10'   hDevice             : 0x%.08X' +
      #13#10'   pCapabilities       : 0x%.08X' +
      #13#10');',
      [hDevice, pCapabilities]);
{$ENDIF}

  Result := ERROR_INVALID_HANDLE;

  pPH := PPOLLING_PARAMETERS_HANDLE(hDevice);

  if (pPH <> NULL) then
  begin
    dwPort := pPH.dwPort;

    if ({(dwPort >= 0) and} (dwPort <= 3)) then
    begin
      pCapabilities.SubType := XINPUT_DEVSUBTYPE_GC_GAMEPAD;

      ZeroMemory(@pCapabilities.In_.Gamepad, sizeof(pCapabilities.In_.Gamepad));

      Result := ERROR_SUCCESS;
    end;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXInputGetState
(
    hDevice: HANDLE;
    {OUT} pState: PXINPUT_STATE
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pPH: PPOLLING_PARAMETERS_HANDLE;
  dwPort: DWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXInputGetState' +
       #13#10'(' +
       #13#10'   hDevice             : 0x%.08X' +
       #13#10'   pState              : 0x%.08X' +
       #13#10');',
       [hDevice, pState]);
{$ENDIF}

  Result := ERROR_INVALID_HANDLE;

  pPH := PPOLLING_PARAMETERS_HANDLE(hDevice);

  if (pPH <> NULL) then
  begin
    if (pPH.pPollingParameters <> NULL) then
    begin
      if (pPH.pPollingParameters.fAutoPoll = Byte(FALSE)) then
      begin
        //
        // TODO -oCXBX: uh..
        //

        EmuWarning('EmuXInputGetState : fAutoPoll == FALSE');
      end;
    end;

    dwPort := pPH.dwPort;

    if ({(dwPort >= 0) and} (dwPort <= 3)) then
    begin
      if (dwPort = 0) then
      begin
        EmuDInputPoll(pState);

        Result := ERROR_SUCCESS;
      end;
    end;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXInputSetState
(
    hDevice: HANDLE;
    pFeedback: PXINPUT_FEEDBACK // IN OUT
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  ret: DWORD;
  pPH: PPOLLING_PARAMETERS_HANDLE;
  v: int;
  found: _bool;

begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXInputSetState' +
         #13#10'(' +
         #13#10'   hDevice             : 0x%.08X' +
         #13#10'   pFeedback           : 0x%.08X' +
         #13#10');',
         [hDevice, pFeedback]);
{$ENDIF}

  ret := ERROR_IO_PENDING;

  pPH := PPOLLING_PARAMETERS_HANDLE(hDevice);

  if (pPH <> NULL) then
  begin


    //
    // Check if this device is already being polled
    //
    found := false;

    for v := 0 to XINPUT_SETSTATE_SLOTS - 1 do
    begin
      if (g_pXInputSetStateStatus[v].hDevice = hDevice) then
      begin
        found := true;

        if (pFeedback.Header.dwStatus = ERROR_SUCCESS) then
        begin
          ret := ERROR_SUCCESS;

          // remove from slot
          g_pXInputSetStateStatus[v].hDevice := 0;
          g_pXInputSetStateStatus[v].pFeedback := NULL;
          g_pXInputSetStateStatus[v].dwLatency := 0;
        end;
      end;
    end;

    //
    // If device was not already slotted, queue it
    //

    if (not found) then
    begin
      for v := 0 to XINPUT_SETSTATE_SLOTS - 1 do
      begin
        if (g_pXInputSetStateStatus[v].hDevice = 0) then
        begin
          g_pXInputSetStateStatus[v].hDevice := hDevice;
          g_pXInputSetStateStatus[v].dwLatency := 0;
          g_pXInputSetStateStatus[v].pFeedback := pFeedback;

          pFeedback.Header.dwStatus := ERROR_IO_PENDING;

          break;
        end;
      end;

      if (v = XINPUT_SETSTATE_SLOTS) then
      begin
        CxbxKrnlCleanup('Ran out of XInputSetStateStatus slots!');
      end;
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := ret;
end;

function XTL_EmuCreateMutex
(
    lpMutexAttributes: LPSECURITY_ATTRIBUTES;
    bInitialOwner: BOOL;
    { TODO -oDXBX: Is this really an Ansi-type? Or should it be wide (LPCWSTR) ? }
    lpName: LPCSTR
): HANDLE; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuCreateMutex' +
      #13#10'(' +
      #13#10'   lpMutexAttributes   : 0x%.08X' +
      #13#10'   bInitialOwner       : 0x%.08X' +
      #13#10'   lpName              : 0x%.08X' +
      #13#10');',
      [lpMutexAttributes, bInitialOwner, lpName]);
{$ENDIF}

  Result := CreateMutexA(PSecurityAttributes(lpMutexAttributes), bInitialOwner <> BOOL_FALSE, lpName);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuCloseHandle
(
    hObject: HANDLE
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DXBX_DEBUG}
  DbgPrintf('EmuXapi : EmuCloseHandle' +
    #13#10'(' +
    #13#10'   hObject             : 0x%.08X' +
    #13#10');',
    [hObject]);
{$ENDIF}

  Result := BOOL(CloseHandle(hObject));

  EmuSwapFS(fsXbox);
end;

function XTL_EmuSetThreadPriorityBoost
(
    hThread: HANDLE;
    DisablePriorityBoost: BOOL
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuSetThreadPriorityBoost' +
      #13#10'(' +
      #13#10'   hThread             : 0x%.08X' +
      #13#10'   DisablePriorityBoost: 0x%.08X' +
      #13#10');',
      [hThread, DisablePriorityBoost]);
{$ENDIF}

  Result := BOOL(SetThreadPriorityBoost(hThread, DisablePriorityBoost <> BOOL_FALSE));

  if Result = BOOL_FALSE then
    EmuWarning('SetThreadPriorityBoost Failed!');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuSetThreadPriority
(
    hThread: HANDLE;
    nPriority: int
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  bRet: BOOL;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuSetThreadPriority' +
      #13#10'(' +
      #13#10'   hThread             : 0x%.08X' +
      #13#10'   nPriority           : 0x%.08X' +
      #13#10');',
      [hThread, nPriority]);
{$ENDIF}

  bRet := BOOL_TRUE; //SetThreadPriority(hThread, nPriority);  // marked by cxbx

  if bRet = BOOL_FALSE then
    EmuWarning('SetThreadPriority Failed!');

  // HACK! Commented by cxbx
  //Sleep(10);

  EmuSwapFS(fsXbox);

  Result := bRet;
end;


function XTL_EmuGetThreadPriority
(
    hThread: HANDLE
): int; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuGetThreadPriority' +
    #13#10'(' +
    #13#10'   hThread             : 0x%.08X' +
    #13#10');',
    [hThread]);
{$ENDIF}

  Result := GetThreadPriority(hThread);

  if Result = THREAD_PRIORITY_ERROR_RETURN then
    EmuWarning('GetThreadPriority Failed!');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuGetExitCodeThread
(
    hThread: HANDLE;
    lpExitCode: LPDWORD
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuGetExitCodeThread' +
      #13#10'(' +
      #13#10'   hThread             : 0x%.08X' +
      #13#10'   lpExitCode          : 0x%.08X' +
      #13#10');',
      [hThread, lpExitCode]);
{$ENDIF}

  Result := BOOL(GetExitCodeThread(hThread, {var}Cardinal(lpExitCode)));

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuXapiInitProcess(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
const
  HEAP_GROWABLE = $00000002;
var
  HeapParameters: RTL_HEAP_PARAMETERS;
  dwPeHeapReserve: UInt32;
  dwPeHeapCommit: UInt32;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXapiInitProcess();');
{$ENDIF}

  // call RtlCreateHeap
  begin
    ZeroMemory(@HeapParameters, sizeof(HeapParameters));
    HeapParameters.Length := sizeof(HeapParameters);

    EmuSwapFS(fsXbox);

    dwPeHeapReserve := DxbxKrnl_XbeHeader.dwPeHeapReserve;
    dwPeHeapCommit := DxbxKrnl_XbeHeader.dwPeHeapCommit;

    XTL_EmuXapiProcessHeap^ := XTL_EmuRtlCreateHeap(HEAP_GROWABLE, nil, dwPeHeapReserve, dwPeHeapCommit, NULL, @HeapParameters);
  end;
end;

procedure XTL_EmuXapiThreadStartup
(
    StartRoutine: StartRoutineFunc; 
    StartContext: PVOID
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DXBX_DEBUG}
  DbgPrintf('EmuXapi : EmuXapiThreadStartup' +
      #13#10'(' +
      #13#10'   StartRoutine        : 0x%.08X' +
      #13#10'   StartContext        : 0x%.08X' +
      #13#10')',
      [Addr(StartRoutine), StartContext]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  StartRoutine(StartContext);

  // TODO -oCXBX: Call thread notify routines ?
end;

(* Cxbx : Too High Level!
XTL.NTSTATUS CDECL XTL_XapiSetupPerTitleDriveLetters(DWord dwTitleId, PWideChar wszTitleName)
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : XapiSetupPerTitleDriveLetters' +
      #13#10'(' +
      #13#10'   dwTitleId           : 0x%.08X' +
      #13#10'   wszTitleName        : 0x%.08X' +
      #13#10');',
      [dwTitleId, wszTitleName);
{$ENDIF}

  NTSTATUS ret := STATUS_SUCCESS;

  EmuSwapFS(fsXbox);

  Result := ret;
end;
*)

procedure XTL_EmuXapiBootDash(UnknownA: DWORD; UnknownB: DWORD; UnknownC: DWORD); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXapiBootDash' +
      #13#10'(' +
      #13#10'   UnknownA            : 0x%.08X' +
      #13#10'   UnknownB            : 0x%.08X' +
      #13#10'   UnknownC            : 0x%.08X' +
      #13#10');',
      [UnknownA, UnknownB, UnknownC]);
{$ENDIF}

  CxbxKrnlCleanup('Emulation Terminated (XapiBootDash)');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuXRegisterThreadNotifyRoutine
(
    pThreadNotification: PXTHREAD_NOTIFICATION;
    fRegister: BOOL
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  i: int;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXRegisterThreadNotifyRoutine' +
      #13#10'(' +
      #13#10'   pThreadNotification : 0x%.08X (0x%.08X)' +
      #13#10'   fRegister           : 0x%.08X' +
      #13#10');',
      [pThreadNotification, Addr(pThreadNotification.pfnNotifyRoutine), Integer(fRegister)]);
{$ENDIF}

  if fRegister <> BOOL_FALSE then
  begin
    // I honestly don't expect this to happen, but if it does...
    if (g_iThreadNotificationCount >= 16) then
      CxbxKrnlCleanup('Too many thread notification routines installed'#13#10 +
                      'If you''re reading this message than tell blueshogun you saw it!!!');

    // Find an empty spot in the thread notification array
    for i := 0 to 16 - 1 do // TODO -oDxbx: Constantify 16
    begin
      // If we find one, then add it to the array, and break the loop so
      // that we don't accidently register the same routine twice!
      if (Addr(g_pfnThreadNotification[i]) = NULL) then
      begin
        g_pfnThreadNotification[i] := pThreadNotification.pfnNotifyRoutine;
        Inc(g_iThreadNotificationCount);
        break;
      end;
    end;
  end
  else
  begin
    // Go through each routine and nullify the routine passed in.
    for i := 0 to 16 - 1 do // TODO -oDxbx: Constantify 16
    begin
      if (Addr(pThreadNotification.pfnNotifyRoutine) = Addr(g_pfnThreadNotification[i])) then
      begin
        g_pfnThreadNotification[i] := NULL;
        Dec(g_iThreadNotificationCount);
        break;
      end;
    end;
  end;

  EmuSwapFS(fsXbox);
end;

type
  LPFIBER_START_ROUTINE = Pointer; // TODO -oDxbx: declare this better!
   
function XTL_EmuCreateFiber
(
  dwStackSize: DWORD;
  lpStartRoutine: LPFIBER_START_ROUTINE;
  lpParameter: LPVOID          
): LPVOID; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuCreateFiber' +
      #13#10'(' +
      #13#10'   dwStackSize         : 0x%.08X' +
      #13#10'   lpStartRoutine      : 0x%.08X' +
      #13#10'   lpParameter         : 0x%.08X' +
      #13#10');',
      [dwStackSize, lpStartRoutine, lpParameter]);
{$ENDIF}

  Result := CreateFiber(dwStackSize, lpStartRoutine, lpParameter);
  if (Result = nil) then
    EmuWarning('CreateFiber failed!');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuDeleteFiber
(
    lpFiber: LPVOID
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuDeleteFiber' +
      #13#10'(' +
      #13#10'  lpFiber            : 0x%.08X' +
      #13#10');',
      [lpFiber]);
{$ENDIF}

  DeleteFiber(lpFiber);

  EmuSwapFS(fsXbox);
end;


function XTL_EmuXGetSectionHandleA
(
    pSectionName: LPCSTR
): XTL_SECTIONHANDLE; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  SectionHeader: PXBE_SECTIONHEADER;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXGetSectionHandleA' +
      #13#10'(' +
      #13#10'   pSectionName       : 0x%.08X' +
      #13#10');',
      [pSectionName]);
{$ENDIF}

  SectionHeader := XBE_FindSectionHeaderByName(pSectionName);
  if Assigned(SectionHeader) then
    Result := XTL_SECTIONHANDLE(SectionHeader)
  else
    Result := XTL_SECTIONHANDLE(INVALID_HANDLE_VALUE);

  EmuSwapFS(fsXbox);
end;


// Adds one to the reference count of the specified section and loads if the
// count is now above zero.
function XTL_EmuXLoadSectionByHandle
(
    hSection: XTL_SECTIONHANDLE
): LPVOID; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  SectionHeader: PXBE_SECTIONHEADER;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXLoadSectionByHandle' +
      #13#10'(' +
      #13#10'   hSection           : 0x%.08X' +
      #13#10');',
      [hSection]);
{$ENDIF}

  // The handle should contain the address of this section by the hack
  // used in EmuXGetSectionHandleA.

  Result := NULL;
  SectionHeader := PXBE_SECTIONHEADER(hSection);
  if Assigned(SectionHeader) then // TODO -oDxbx : Check section handle more thoroughly than this
  begin
    Inc(SectionHeader.dwSectionRefCount);
    if SectionHeader.dwSectionRefCount > 0 then
      // TODO : Actually load the section here, including the symbol-detection + patching!
      Result := LPVOID(SectionHeader.dwVirtualAddr);
  end;

  EmuSwapFS(fsXbox);
end;


// Subtracts one from the reference count of the specified section and unloads
// if the count is now zero.
function XTL_EmuXFreeSectionByHandle
(
    hSection: XTL_SECTIONHANDLE
): BOOL; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  SectionHeader: PXBE_SECTIONHEADER;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXFreeSectionByHandle' +
      #13#10'(' +
      #13#10'   hSection           : 0x%.08X' +
      #13#10');',
      [hSection]);
{$ENDIF}

  SectionHeader := PXBE_SECTIONHEADER(hSection);
  if Assigned(SectionHeader) then // TODO -oDxbx : Check section handle more thoroughly than this
  begin
    Dec(SectionHeader.dwSectionRefCount);
    if SectionHeader.dwSectionRefCount = 0 then
      ; // TODO : Actually unload the section here

    Result := BOOL_TRUE;
  end
  else
    Result := BOOL_FALSE;

  EmuSwapFS(fsXbox);
end;


function XTL_EmuXLoadSectionA
(
    pSectionName: LPCSTR
): LPVOID; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  SectionHandle: XTL_SECTIONHANDLE;
begin
{$IFDEF DEBUG}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuXapi : EmuXLoadSectionA' +
      #13#10'(' +
      #13#10'   pSectionName       : 0x%.08X' +
      #13#10');',
      [pSectionName]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  SectionHandle := XTL_EmuXGetSectionHandleA(pSectionName);
  if SectionHandle = XTL_SECTIONHANDLE(INVALID_HANDLE_VALUE) then
    Result := NULL
  else
    Result := XTL_EmuXLoadSectionByHandle(SectionHandle);
end;

function XTL_EmuXFreeSectionA
(
    pSectionName: LPCSTR
): BOOL; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  SectionHandle: XTL_SECTIONHANDLE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXFreeSectionA' +
      #13#10'(' +
      #13#10'   pSectionName       : 0x%.08X' +
      #13#10');',
      [pSectionName]);
{$ENDIF}

  SectionHandle := XTL_EmuXGetSectionHandleA(pSectionName);
  if SectionHandle = XTL_SECTIONHANDLE(INVALID_HANDLE_VALUE) then
    Result := BOOL_FALSE
  else
    Result := XTL_EmuXFreeSectionByHandle(SectionHandle);

  EmuSwapFS(fsXbox);
end;

// Dxbx note : This patch is not really needed, as the Xbox1 seems to use the SectionHeader address as a handle too.
function XTL_EmuXGetSectionSize
(
  hSection: XTL_SECTIONHANDLE
): DWORD; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  SectionHeader: PXBE_SECTIONHEADER;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXGetSectionSize' +
      #13#10'(' +
      #13#10'   hSection           : 0x%.08X' +
      #13#10');',
      [hSection]);
{$ENDIF}

  SectionHeader := PXBE_SECTIONHEADER(hSection);
  if Assigned(SectionHeader) then // TODO -oDxbx : Check section handle more thoroughly than this
    Result := SectionHeader.dwVirtualSize
  else
    Result := 0;

  EmuSwapFS(fsXbox);
end;


function XTL_EmuRtlDestroyHeap
(
    {IN}HeapHandle: HANDLE
): PVOID; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuRtlDestroyHeap' +
      #13#10'(' +
      #13#10'   HeapHandle         : 0x%.08X' +
      #13#10');',
      [HeapHandle]);
{$ENDIF}

  HANDLE(Result) := JwaNative.RtlDestroyHeap(HeapHandle);

  EmuSwapFS(fsXbox);
end;


function XTL_EmuQueueUserAPC
(
  pfnAPC: PAPCFUNC;
  hThread: HANDLE;
  dwData: DWORD
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwRet: DWORD;
  hApcThread: HANDLE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuQueueUserAPC' +
      #13#10'(' +
      #13#10'   pfnAPC           : 0x%.08X' +
      #13#10'   hThread          : 0x%.08X' +
      #13#10'   dwData           : 0x%.08X' +
      #13#10');',
      [Addr(pfnAPC), hThread, dwData]);
{$ENDIF}

  // dwRet := 0;

  // If necessary, we can just continue to emulate NtQueueApcThread (0xCE).
  // I added this because NtQueueApcThread fails in Metal Slug 3.

  hApcThread := 0;
  if (not DuplicateHandle(GetCurrentProcess(), hThread, GetCurrentProcess(), @hApcThread, THREAD_SET_CONTEXT, FALSE, 0)) then
    EmuWarning('DuplicateHandle failed!');

  dwRet := QueueUserAPC(pfnAPC, hApcThread, dwData);
  if (0=dwRet) then
    EmuWarning('QueueUserAPC failed!');

  EmuSwapFS(fsXbox);

  Result := dwRet;
end;


function XTL_EmuGetOverlappedResult
(
  hFile: HANDLE;
  lpOverlapped: LPOVERLAPPED;
  lpNumberOfBytesTransferred: LPDWORD;
  bWait: BOOL
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuGetOverlappedResult' +
      #13#10'(' +
      #13#10'   hFile            : 0x%.08X' +
      #13#10'   lpOverlapped     : 0x%.08X' +
      #13#10'   lpNumberOfBytesTransformed : 0x%.08X' +
      #13#10'   bWait            : 0x%.08X' +
      #13#10');',
      [hFile, lpOverlapped, lpNumberOfBytesTransferred, bWait]);
{$ENDIF}

  Result := BOOL(GetOverlappedResult(hFile, lpOverlapped^, {var}lpNumberOfBytesTransferred^, bWait <> BOOL_FALSE));

//  if (bWait) then
//    bRet := TRUE; // Sucker...

  EmuSwapFS(fsXbox);
end;


function XTL_EmuXLaunchNewImage
(
  lpTitlePath: LPCSTR;
  pLaunchData: PLAUNCH_DATA
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwRet: DWORD;
  fp: PFILE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXLaunchNewImage' +
      #13#10'(' +
      #13#10'   lpTitlePath      : 0x%.08X (%s)' +
      #13#10'   pLaunchData      : 0x%.08X' +
      #13#10');',
      [lpTitlePath, lpTitlePath, pLaunchData]);
{$ENDIF}

  // If this function succeeds, it doesn't get a chance to return anything.
  dwRet := ERROR_GEN_FAILURE;

  // If no path is specified, then the xbe is rebooting to dashboard
  if (nil = lpTitlePath) then
    CxbxKrnlCleanup('The xbe is rebooting (XLaunchNewImage)');

  // Ignore any other attempts to execute other .xbe files (for now).
  EmuWarning('Not executing the xbe!');

  // Save the launch data
  if (pLaunchData <> NULL) then
  begin
    CopyMemory({Dest=}@g_SavedLaunchData, {Source=}pLaunchData, sizeof(LAUNCH_DATA));

    // Save the launch data parameters to disk for later.
{$IFDEF DEBUG}
    DbgPrintf('Saving launch data as CxbxLaunchData.bin...');
{$ENDIF}

    fp := fopen('CxbxLaunchData.bin', 'wb');
    if Assigned(fp) then
    begin
      fseek(fp, 0, SEEK_SET);
      fwrite(pLaunchData, sizeof(LAUNCH_DATA), 1, fp);
      fclose(fp);
    end;
  end;

  g_bXLaunchNewImageCalled := true;

  // Temporary Hack (Unreal): Jump back to the entry point
//  Puint32(start) := Puint32($21C13B);

  EmuSwapFS(fsXbox);

//  __asm jmp start;

  Result := dwRet;
end;


function XTL_EmuXGetLaunchInfo
(
  pdwLaunchDataType: PDWORD;
  pLaunchData: PLAUNCH_DATA
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
const
  ERROR_LAUNCHDATA_NOT_FOUND = 1168;
var
  dwRet: HRESULT; // Cxbx uses DWORD;
  fp: PFILE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXGetLaunchInfo' +
      #13#10'(' +
      #13#10'   pdwLaunchDataType : 0x%.08X' +
      #13#10'   pLaunchData       : 0x%.08X' +
      #13#10');',
      [pdwLaunchDataType, pLaunchData]);
{$ENDIF}

  dwRet := ERROR_LAUNCHDATA_NOT_FOUND; // Dxbx note : Cxbx incorrectly uses E_FAIL here!

  // Has XLaunchNewImage been called since we've started this round?
  if (g_bXLaunchNewImageCalled) then
  begin
    // I don't think we'll be emulating any other xbox apps
    // other than games anytime soon...
    pdwLaunchDataType^ := LDT_TITLE;

    // Copy saved launch data
    CopyMemory({Dest=}pLaunchData, {Source=}@g_SavedLaunchData, sizeof(LAUNCH_DATA));

    dwRet := ERROR_SUCCESS;
  end;

  fp := NULL;

  // Does CxbxLaunchData.bin exist?
  if (not g_bXLaunchNewImageCalled) then
    fp := fopen('CxbxLaunchData.bin', 'rb');

  // If it does exist, load it.
  if Assigned(fp) then
  begin
    // Data from Xbox game
    pdwLaunchDataType^ := LDT_TITLE;

    // Read in the contents.
    fseek(fp, 0, SEEK_SET);
    fread(@g_SavedLaunchData, sizeof(LAUNCH_DATA), 1, fp);
    memcpy(pLaunchData, @g_SavedLaunchData, sizeof(LAUNCH_DATA));
//    fread(pLaunchData, sizeof(LAUNCH_DATA), 1, fp);      // MARKED OUT CXBX
//    memcpy(@g_SavedLaunchData, pLaunchData, sizeof(LAUNCH_DATA)); // MARKED OUT CXBX
    fclose(fp);

    // Delete the file once we're done.
    DeleteFile('CxbxLaunchData.bin');

    // HACK: Initialize XInput from restart
    // MARKED OUT CXBX
    {if (g_bXInputOpenCalled)
    begin
      EmuSwapFS(fsWindows);
      XTL_EmuXInputOpen( NULL, 0, 0, NULL );
      EmuSwapFS(fsXbox)
    end;}

    dwRet := ERROR_SUCCESS;
  end;

  EmuSwapFS(fsXbox);

  Result := dwRet;
end;


procedure XTL_EmuXSetProcessQuantumLength
(
    dwMilliseconds: DWORD
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXSetProcessQuantumLength' +
      #13#10'(' +
      #13#10'   dwMilliseconds    : 0x%.08X' +
      #13#10');',
      [dwMilliseconds]);
{$ENDIF}

  // TODO -oCXBX: Implement?
  EmuWarning('XSetProcessQuantumLength is being ignored!');

  EmuSwapFS(fsXbox);
end;


function XTL_EmuXGetFileCacheSize(): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXGetFileCacheSize()');
{$ENDIF}

  // Return the default cache size for now.
  // TODO -oCXBX: Save the file cache size if/when set.
  Result := 64 * 1024;

  EmuSwapFS(fsXbox);
end;


function XTL_EmuSignalObjectAndWait
(
  hObjectToSignal: HANDLE;
  hObjectToWaitOn: HANDLE;
  dwMilliseconds: DWORD;
  bAlertable: BOOL
): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuSignalObjectAndWait' +
      #13#10'(' +
      #13#10'   hObjectToSignal   : 0x%.08X' +
      #13#10'   hObjectToWaitOn   : 0x%.08X' +
      #13#10'   dwMilliseconds    : 0x%.08X' +
      #13#10'   bAlertable        : 0x%.08X' +
      #13#10');',
      [hObjectToSignal, hObjectToWaitOn, dwMilliseconds, bAlertable]);
{$ENDIF}

  Result := BOOL(SignalObjectAndWait(hObjectToSignal, hObjectToWaitOn, dwMilliseconds, bAlertable <> BOOL_FALSE));

  EmuSwapFS(fsXbox);
end;


function XTL_EmuPulseEvent(hEvent: HANDLE): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuPulseEvent' +
      #13#10'(' +
      #13#10'   hEvent            : 0x%.08X' +
      #13#10');',
      [hEvent]);
{$ENDIF}

  // TODO -oCXBX: This function might be a bit too high level.  If it is,
  // feel free to implement NtPulseEvent in EmuKrnl.cpp

  Result := BOOL(PulseEvent(hEvent));

  EmuSwapFS(fsXbox);
end;


function XTL_EmuCreateSemaphore
(
  lpSemaphoreAttributes: LPVOID;
  lInitialCount: LONG;
  lMaximumCount: LONG;
  lpName: LPSTR
): HANDLE; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuCreateSemaphore' +
      #13#10'(' +
      #13#10'   lpSemaphoreAttributes : 0x%.08X' +
      #13#10'   lInitialCount         : 0x%.08X' +
      #13#10'   lMaximumCount         : 0x%.08X' +
      #13#10'   lpName                : 0x%.08X (%s)' +
      #13#10');',
      [lpSemaphoreAttributes, lInitialCount, lMaximumCount, lpName]);
{$ENDIF}

  if Assigned(lpSemaphoreAttributes) then
    EmuWarning( 'lpSemaphoreAttributes != NULL' );

  Result := CreateSemaphoreA(NULL, lInitialCount, lMaximumCount, lpName);
  if (0=Result) then
    EmuWarning( 'CreateSemaphore failed!' );

  EmuSwapFS(fsXbox);
end;


function XTL_EmuReleaseSemaphore
(
  hSemaphore: HANDLE;
  lReleaseCount: LONG;
  lpPreviousCount: LPLONG
): BOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuReleaseSemaphore' +
      #13#10'(' +
      #13#10'   hSemaphore        : 0x%.08X' +
      #13#10'   lReleaseCount     : 0x%.08X' +
      #13#10'   lpPreviousCount   : 0x%.08X' +
      #13#10');',
      [hSemaphore, lReleaseCount, lpPreviousCount]);
{$ENDIF}

  Result := BOOL(ReleaseSemaphore(hSemaphore, lReleaseCount, lpPreviousCount));
  if (BOOL_FALSE = Result) then
    EmuWarning('ReleaseSemaphore failed!');

  EmuSwapFS(fsXbox);
end;


function XTL_EmutimeSetEvent
(
  uDelay: UINT;
  uResolution: UINT;
  fptc: LPTIMECALLBACK;
  dwUser: DWORD;
  fuEvent: UINT
): MMRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmutimeSetEvent' +
      #13#10'(' +
      #13#10'   uDelay            : 0x%.08X' +
      #13#10'   uResolution       : 0x%.08X' +
      #13#10'   fptc              : 0x%.08X' +
      #13#10'   dwUser            : 0x%.08X' +
      #13#10'   fuEvent           : 0x%.08X' +
      #13#10');',
      [uDelay, uResolution, Addr(fptc), dwUser, fuEvent]);
{$ENDIF}

  Result := timeSetEvent(uDelay, uResolution, fptc, DWORD_PTR(dwUser), fuEvent);

  EmuSwapFS(fsXbox);
end;


function XTL_EmutimeKillEvent
(
  uTimerID: Windows.UINT
): MMRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmutimeKillEvent' +
      #13#10'(' +
      #13#10'   uTimerID          : 0x%.08X' +
      #13#10');',
      [uTimerID]);
{$ENDIF}

  Result := timeKillEvent(uTimerID);

  EmuSwapFS(fsXbox);
end;


procedure XTL_EmuRaiseException
(
  dwExceptionCode: DWORD;       // exception code
  dwExceptionFlags: DWORD;      // continuable exception flag
  nNumberOfArguments: DWORD;    // number of arguments
  lpArguments: PULONG_PTR       // array of arguments
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuRaiseException' +
      #13#10'(' +
      #13#10'   dwExceptionCode   : 0x%.08X' +
      #13#10'   dwExceptionFlags  : 0x%.08X' +
      #13#10'   nNumberOfArguments: 0x%.08X' +
      #13#10'   lpArguments       : 0x%.08X' +
      #13#10');',
      [dwExceptionCode, dwExceptionFlags, nNumberOfArguments, lpArguments]);
{$ENDIF}

  // TODO -oCXBX: Implement or not?
//  RaiseException(dwExceptionCode, dwExceptionFlags, nNumberOfArguments, (*(ULONG_PTR**) &lpArguments));

  EmuSwapFS(fsXbox);
end;

(*//  MARKED BY Cxbx : not necessary?
function XTL_EmuXCalculateSignatureBegin
(
  dwFlags: DWord
): HANDLE; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : XCalculateSignatureBegin' +
      #13#10'(' +
      #13#10'   dwFlags             : 0x%.08X' +
      #13#10');',
      [dwFlags]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  // return a fake HANDLE value for now
  Result := $AAAAAAAA;
end;

//  MARKED BY Cxbx : not necessary?
function XTL_EmuXCalculateSignatureBeginEx
(
    dwFlags: DWord; 
    dwAltTitleId: DWord
): HANDLE; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : XCalculateSignatureBeginEx' +
    #13#10'(' +
    #13#10'   dwFlags             : 0x%.08X' +
    #13#10'   dwAltTitleId        : 0x%.08X' +
    #13#10');',
    [dwFlags, dwAltTitleId]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  // return a fake HANDLE value for now
  Result := $AAAAAAAA;
end;

//  MARKED BY Cxbx : not necessary?
function XTL_EmuXCalculateSignatureUpdate
(
  hCalcSig: HANDLE;
  pbData: Byte; 
  cbData: ULONG
): DWord; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : XCalculateSignatureUpdate' +
           #13#10'(' +
           #13#10'   hCalcSig            : 0x%.08X' +
           #13#10'   pbData              : 0x%.08X' +
           #13#10'   cbData              : 0x%.08X' +
           #13#10');',
            [hCalcSig, pbData, cbData]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  Result := ERROR_SUCCESS;
end;

//  MARKED BY Cxbx : not necessary?
function XTL_EmuXCalculateSignatureEnd
(
  hCalcSig: HANDLE;
  pSignature: PXCALCSIG_SIGNATURE
): DWord; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : XCalculateSignatureEnd' +
      #13#10'(' +
      #13#10'   hCalcSig            : 0x%.08X' +
      #13#10'   pSignature          : 0x%.08X' +
      #13#10');',
      [hCalcSig, pSignature]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  Result := ERROR_SUCCESS;
end;
*)

exports
  XTL_EmuCloseHandle, // TODO -oDXBX: This makes emuclose instead of ntclose;
  XTL_EmuCreateFiber,
  XTL_EmuCreateMutex,
  XTL_EmuCreateSemaphore,
  XTL_EmuDeleteFiber,
  XTL_EmuGetExitCodeThread,
  XTL_EmuGetOverlappedResult,
  XTL_EmuGetThreadPriority,
  XTL_EmuGetTimeZoneInformation,
  XTL_EmuPulseEvent,
  XTL_EmuQueryPerformanceCounter,
  XTL_EmuQueryPerformanceFrequency,
  XTL_EmuQueueUserAPC,
  XTL_EmuRaiseException,
  XTL_EmuReleaseSemaphore,
  XTL_EmuRtlAllocateHeap,
  XTL_EmuRtlCreateHeap,
  XTL_EmuRtlDestroyHeap,
  XTL_EmuRtlDestroyHeap,
  XTL_EmuRtlFreeHeap,
  XTL_EmuRtlReAllocateHeap,
  XTL_EmuRtlSizeHeap,
  XTL_EmuSetThreadPriority,
  XTL_EmuSetThreadPriorityBoost,
  XTL_EmuSignalObjectAndWait,
  XTL_EmutimeKillEvent,
  XTL_EmutimeSetEvent,
  XTL_EmuXapiApplyKernelPatches,
  XTL_EmuXapiBootDash name PatchPrefix + 'XapiBootToDash',
  XTL_EmuXapiInitProcess,
  XTL_EmuXapiThreadStartup,
  XTL_EmuXFormatUtilityDrive,
  XTL_EmuXFreeSectionA,
  XTL_EmuXFreeSectionByHandle,
  XTL_EmuXGetDeviceChanges,
  XTL_EmuXGetDevices,
  XTL_EmuXGetFileCacheSize,
  XTL_EmuXGetLaunchInfo,
  XTL_EmuXGetSectionHandleA,
  XTL_EmuXGetSectionSize,
  XTL_EmuXInitDevices name PatchPrefix + '_USBD_Init@8', // Cxbx incorrectly calls this XInitDevices
  XTL_EmuXInputClose,
  XTL_EmuXInputGetCapabilities,
  XTL_EmuXInputGetState,
  XTL_EmuXInputOpen,
  XTL_EmuXInputPoll,
  XTL_EmuXInputSetState,
  XTL_EmuXLaunchNewImage name PatchPrefix + 'XLaunchNewImageA',
  XTL_EmuXLoadSectionA,
  XTL_EmuXLoadSectionByHandle,
  XTL_EmuXMountUtilityDrive,
  XTL_EmuXRegisterThreadNotifyRoutine,
  XTL_EmuXSetProcessQuantumLength;

end.

