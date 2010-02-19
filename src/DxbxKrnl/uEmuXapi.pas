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
  Windows,
  SysUtils,
  // Jedi
  JwaWinType,
  JwaWinBase,
  JwaNative,
  // Dxbx
  uTypes,
  uLog, // DbgPrintf
  uXbe,
  uEmu,
  uEmuFS, // EmuSwapFS
  uEmuAlloc,
  uEmuDInput,
  uXBController,
  uDxbxUtils,
  uDxbxKrnlUtils; // CxbxKrnl_XbeHeader

type
  ProcedureStdCall = procedure; stdcall;

  StartRoutineFunc = function (const StartContext: PVOID): Int; stdcall;

  LPSECURITY_ATTRIBUTES = PVOID;

  PRTL_HEAP_COMMIT_ROUTINE = function(
    Base: PVOID;
    OUT CommitAddress: PPVOID;
    OUT CommitSize: PSIZE_T
    ): NTSTATUS; stdcall;

  RTL_HEAP_PARAMETERS = packed record
    Length: ULONG ;
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
  PRTL_HEAP_PARAMETERS = ^RTL_HEAP_PARAMETERS;

  _XINPUT_POLLING_PARAMETERS = packed record
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

  _POLLING_PARAMETERS_HANDLE = packed record
    pPollingParameters: PXINPUT_POLLING_PARAMETERS;
    dwPort: DWORD;
  end;
  POLLING_PARAMETERS_HANDLE = _POLLING_PARAMETERS_HANDLE;
  PPOLLING_PARAMETERS_HANDLE = ^POLLING_PARAMETERS_HANDLE;

  XTHREAD_NOTIFY_PROC = procedure(fCreate: BOOL); stdcall;

  XTHREAD_NOTIFICATION = packed record
    Reserved: LIST_ENTRY;
    pfnNotifyRoutine: XTHREAD_NOTIFY_PROC;
  end;
  PXTHREAD_NOTIFICATION = ^XTHREAD_NOTIFICATION;

const
  XCALCSIG_SIGNATURE_SIZE = 20;
  
type
  XCALCSIG_SIGNATURE = packed record
    Signature: array [0..XCALCSIG_SIGNATURE_SIZE-1] of BYTE;
  end;
  PXCALCSIG_SIGNATURE = ^XCALCSIG_SIGNATURE;

  XPP_DEVICE_TYPE = packed record
    // Note : Cxbx has size 3, but XTL_EmuXGetDevices seems to indicate size 4 :
    Reserved: array [0..4-1] of ULONG;
  end;
  PXPP_DEVICE_TYPE = ^XPP_DEVICE_TYPE;

  XINPUT_GAMEPAD = packed record
    wButtons: WORD;
    bAnalogButtons: array [0..8-1] of BYTE;
    sThumbLX: SHORT;
    sThumbLY: SHORT;
    sThumbRX: SHORT;
    sThumbRY: SHORT;
  end;
  PXINPUT_GAMEPAD = ^XINPUT_GAMEPAD;

  XINPUT_RUMBLE = packed record
    wLeftMotorSpeed: Word;
    wRightMotorSpeed: Word;
  end;
  PXINPUT_RUMBLE = ^XINPUT_RUMBLE;

  XINPUT_CAPABILITIES = packed record
    SubType: BYTE;
    Reserved: WORD;

    In_: record
      Gamepad: XINPUT_GAMEPAD;
    end;

    Out_: record
      Rumble: XINPUT_RUMBLE;
    end;
  end;
  PXINPUT_CAPABILITIES = ^XINPUT_CAPABILITIES;

const
  // Device SubTypes
  XINPUT_DEVSUBTYPE_GC_GAMEPAD              = $01;
  XINPUT_DEVSUBTYPE_GC_GAMEPAD_ALT          = $02;
  XINPUT_DEVSUBTYPE_GC_WHEEL                = $10;
  XINPUT_DEVSUBTYPE_GC_ARCADE_STICK         = $20;
  XINPUT_DEVSUBTYPE_GC_DIGITAL_ARCADE_STICK = $21;
  XINPUT_DEVSUBTYPE_GC_FLIGHT_STICK         = $30;
  XINPUT_DEVSUBTYPE_GC_SNOWBOARD            = $40;

type
  XINPUT_FEEDBACK_HEADER = packed record
    dwStatus: DWord;
    hEvent: HANDLE; // OPTIONAL ;
    Reserved: array [1..58] of Byte;
  end;
  PXINPUT_FEEDBACK_HEADER = ^XINPUT_FEEDBACK_HEADER;

  XINPUT_FEEDBACK = packed record
    Header: XINPUT_FEEDBACK_HEADER;
//    union
    Rumble: XINPUT_RUMBLE;
  end;
  PXINPUT_FEEDBACK = ^XINPUT_FEEDBACK;

var
  // XInputSetState status waiters
  g_pXInputSetStateStatus: array [0..XINPUT_SETSTATE_SLOTS - 1] of XInputSetStateStatus;

  // XInputOpen handles
  g_hInputHandle: array [0..XINPUT_HANDLE_SLOTS - 1] of Handle;

  // Xbe section list
  g_SectionList: PXBE_SECTIONLIST;
  // Number of sections
  g_NumSections: int;

  XTL_EmuXapiProcessHeap: PPVOID;

  // Note : Cxbx log indicates 'g_pRtlCreateHeap' is indeed
  // at the same address as 'EmuRtlCreateHeap'.

implementation

uses
  // Dxbx
  uEmuKrnlPs; // g_pfnThreadNotification

function _XINPUT_POLLING_PARAMETERS.GetBits(const aIndex: Integer): Byte;
begin
  Result := GetByteBits(_Flag, aIndex);
end;

const
  HEAP_HEADERSIZE = $20;

procedure XTL_EmuXapiApplyKernelPatches(); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuXapi : EmuXapiApplyKernelPatches()');
  EmuSwapFS(fsXbox);
{$ENDIF}

  // we dont really feel like patching, now do we?
end;

function XTL_EmuXFormatUtilityDrive(): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuXapi : EmuXFormatUtilityDrive()');
  EmuSwapFS(fsXbox);
{$ENDIF}

  // Cxbx TODO: yeah... we'll format... riiiiight

  Result := True;
end;


function XTL_EmuFindFirstFileA(lpFileName: PChar;{out}lpFindFileData: LPWIN32_FIND_DATA): Handle; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  szBuffer: PChar;
  szRoot: string;
  hRet: Handle;
  bRet: BOOL;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuFindFirstFileA' +
    #13#10'(' +
    #13#10'   lpFileName          : 0x%.08X (%s)' +
    #13#10'   lpFindFileData      : 0x%.08X' +
    #13#10');',
    [lpFileName, lpFileName, lpFindFileData]);
{$ENDIF}

    //
    // Cxbx TODO: this code is replicated in NtCreateFile. make this a function
    //

    //
    // Cxbx TODO: replace full directories with their shorthand (D:\, etc)
    //

  szBuffer := lpFileName;
  szRoot := g_strCurDrive;

{$IFDEF DEBUG}
    //printf('bef : %s'#13#10, lpFileName);
{$ENDIF}

  if (szBuffer <> nil) then
  begin
    // trim this off
    if (szBuffer[0] = '\') and (szBuffer[1] = '?') and (szBuffer[2] = '?') and (szBuffer[3] = '\') then
      szBuffer := szBuffer + 4;

    // D:\ should map to current directory
    if ((szBuffer[0] = 'D') or (szBuffer[0] = 'd')) and (szBuffer[1] = ':') and (szBuffer[2] = '\') then
    begin
      szBuffer := szBuffer + 3;
    end
    else if ((szBuffer[0] = 'T') or (szBuffer[0] = 't')) and (szBuffer[1] = ':') and (szBuffer[2] = '\') then
    begin
      szBuffer := szBuffer + 3;

      szRoot := g_strTDrive;
    end
    else if ((szBuffer[0] = 'U') or (szBuffer[0] = 'u')) and (szBuffer[1] = ':') and (szBuffer[2] = '\') then
    begin
      szBuffer := szBuffer + 3;

      szRoot := g_strUDrive;
    end
    else if ((szBuffer[0] = 'Z') or (szBuffer[0] = 'z')) and (szBuffer[1] = ':') and (szBuffer[2] = '\') then
    begin
      szBuffer := szBuffer + 3;

      szRoot := g_strZDrive;
    end;
  end;

{$IFDEF DEBUG}
    //printf('af1 : %s'#13#10, szRoot);
    //printf('af2 : %s'#13#10, szBuffer);
{$ENDIF}

    //char szOldDir[MAX_PATH];

    //GetCurrentDirectory(MAX_PATH, szOldDir);

  SetCurrentDirectory(PChar(szRoot));

  hRet := FindFirstFile(szBuffer, WIN32_FIND_DATA(lpFindFileData^));

  if (not FAILED(hRet)) then
  begin
    while True do
    begin
      bRet := FindNextFile(hRet, WIN32_FIND_DATA(lpFindFileData^));

      if (not bRet) then
      begin
        hRet := INVALID_HANDLE_VALUE;
        Break;
      end;

      if ((StrComp(lpFindFileData.cFileName, '.') <> 0) and (StrComp(lpFindFileData.cFileName, '..') <> 0)) then
        Break;
    end;
  end;

    //SetCurrentDirectory(szOldDir);

  EmuSwapFS(fsXbox);

  Result := hRet;
end;



function XTL_EmuFindNextFileA(hFindFile: Handle; {out} lpFindFileData: LPWIN32_FIND_DATA): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  bRet: BOOL;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuFindNextFileA' +
    #13#10'(' +
    #13#10'   hFindFile           : 0x%.08X' +
    #13#10'   lpFindFileData      : 0x%.08X' +
    #13#10');',
    [hFindFile, lpFindFileData]);
{$ENDIF}

    //
    // Cxbx TODO: replace full directories with their shorthand (D:\, etc)
    //

  repeat
    bRet := FindNextFile(hFindFile, WIN32_FIND_DATA(lpFindFileData^));

    if (not bRet) then
      Break;

    if ((StrComp(lpFindFileData.cFileName, '.') <> 0) and (StrComp(lpFindFileData.cFileName, '..') <> 0)) then
      Break;
  until False;

{$IFDEF DEBUG}
    //printf('Found : %s'#13#10, lpFindFileData.cFileName);
{$ENDIF}

  EmuSwapFS(fsXbox);

  Result := bRet;
end;

function XTL_EmuGetTimeZoneInformation(
  lpTimeZoneInformation: LPTIME_ZONE_INFORMATION
  ): DWORD; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
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

type
  RTL_HEAP_DEFINITION = packed record
    Length: ULONG;
    Unknown: array [0..11-1] of ULONG;
  end;
  PRTL_HEAP_DEFINITION = ^RTL_HEAP_DEFINITION;

function XTL_EmuRtlCreateHeap(
  Flags: ULONG;
  Base: PVOID; // OPTIONAL
  Reserve: SIZE_T; // OPTIONAL
  Commit: SIZE_T;
  Lock: PVOID; // OPTIONAL
  Parameters: PVOID // OPTIONAL
  ): PVOID; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  NativeParameters: RTL_HEAP_PARAMETERS; // Was: RTL_HEAP_DEFINITION;
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
    #13#10'   Parameters          : 0x%.08X' +
    #13#10');',
    [Flags, Base, Reserve, Commit, Lock, Parameters]);
{$ENDIF}

  ZeroMemory(@NativeParameters, SizeOf(NativeParameters));
  NativeParameters.Length := SizeOf(NativeParameters);

  // Cxbx TODO : Find out how RtlHeapParams is defined on Xbox, and map this
  // as closely as possible to the native RTL_HEAP_PARAMETERS.

  Result := PVOID(JwaNative.RtlCreateHeap(
    Flags, Base, Reserve, Commit, Lock, @NativeParameters));

{$IFDEF DXBX_DEBUG}
  DbgPrintf('pRet : 0x%.08X', [Result]);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlAllocateHeap(
  hHeap: Handle;
  dwFlags: DWord;
  dwBytes: SIZE_T): PVOID; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  offs: Byte;
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
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

  Result := CxbxRtlAlloc(hHeap, dwFlags, dwBytes);
  if Assigned(Result) then
  begin
    offs := Byte(RoundUp(uint32(Result), HEAP_HEADERSIZE) - uint32(Result));
    if offs = 0 then
      offs := HEAP_HEADERSIZE;

    Result := PVOID(uint32(Result) + offs);
    PByte(uint32(Result) - 1)^ := offs;
  end;

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('pRet : 0x%.08X', [Result]);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlDestroyHeap(
  hHeap: Handle): Handle; stdcall;
// Branch:None  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuRtlDestroyHeap' +
    #13#10'(' +
    #13#10'   hHeap               : 0x%.08X' +
    #13#10');',
    [hHeap]);
{$ENDIF}

  Result := JwaNative.RtlDestroyHeap(hHeap);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlFreeHeap(
  hHeap: Handle;
  dwFlags: DWord;
  lpMem: PVOID): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  offs: Byte;
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
  begin
    offs := PByte(uint32(lpMem) - 1)^;
    lpMem := PVOID(uint32(lpMem) - offs);
  end;

  Result := CxbxRtlFree(hHeap, dwFlags, lpMem);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlReAllocateHeap(
  hHeap: Handle;
  dwFlags: DWord;
  lpMem: PVOID;
  dwBytes: SIZE_T): PVOID; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  offs: Byte;
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuXapi : EmuRtlReAllocateHeap' +
    #13#10'('+
    #13#10'   hHeap               : 0x%.08X' +
    #13#10'   dwFlags             : 0x%.08X' +
    #13#10'   lpMem               : 0x%.08X' +
    #13#10'   dwBytes             : 0x%.08X' +
    #13#10');',
    [hHeap, dwFlags, lpMem, dwBytes]);
{$ENDIF}

  if Assigned(lpMem) then
  begin
    offs := PByte(uint32(lpMem) - 1)^;
    lpMem := PVOID(uint32(lpMem) - offs);
  end
  else
    offs := 0;

  if dwBytes > 0 then
    Inc(dwBytes, HEAP_HEADERSIZE);

  Result := CxbxRtlRealloc(hHeap, dwFlags, lpMem, dwBytes);
  if Assigned(Result) then
  begin
    // Dxbx note : Realloc from nil is different from non-nil :
    if Assigned(lpMem) then
      // Realloc from non-nil, implies we need to keep using the same offset :
      Result := PVOID(uint32(Result) + offs)
    else
    begin
      // Realloc from nil, implies we need to determine offset just like in a new EmuRtlAllocateHeap :
      offs := Byte(RoundUp(uint32(Result), HEAP_HEADERSIZE) - uint32(Result));
      if offs = 0 then
        offs := HEAP_HEADERSIZE;

      Result := PVOID(uint32(Result) + offs);
      PByte(uint32(Result) - 1)^ := offs;
    end;
  end;

{$IFDEF DXBX_DEBUG_TRACE}
  DbgPrintf('pRet : 0x%.08X', [Result]);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlSizeHeap(
  hHeap: Handle;
  dwFlags: DWord;
  lpMem: PVOID): SIZE_T; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  offs: Byte;
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
  begin
    offs := PByte(uint32(lpMem) - 1)^;
    lpMem := PVOID(uint32(lpMem) - offs);
  end;

  Result := CxbxRtlSizeHeap(hHeap, dwFlags, lpMem);
  if Result > 0 then
    Dec(Result, HEAP_HEADERSIZE);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuQueryPerformanceCounter(
  lpPerformanceCount: PLARGE_INTEGER
  ): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuQueryPerformanceCounter' +
    #13#10'(' +
    #13#10'   lpPerformanceCount  : 0x%.08X' +
    #13#10');',
    [lpPerformanceCount]);
{$ENDIF}

  Result := QueryPerformanceCounter({var}lpPerformanceCount^);

    // debug - 4x speed
    //lpPerformanceCount.QuadPart *= 4;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuQueryPerformanceFrequency(
  lpFrequency: PLARGE_INTEGER
  ): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuQueryPerformanceFrequency' +
    #13#10'(' +
    #13#10'   lpFrequency         : 0x%.08X' +
    #13#10');',
    [lpFrequency]);
{$ENDIF}

  Result := QueryPerformanceFrequency({var}lpFrequency^);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXMountUtilityDrive(fFormatClean: BOOL): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
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

  Result := True;
end;

procedure XTL_EmuXInitDevices(Unknown1: DWord; Unknown2: PVOID); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  v: Integer;
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
    g_hInputHandle[v] := 0;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXGetDevices(DeviceType: PXPP_DEVICE_TYPE): DWord; stdcall;
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
    EmuWarning('Unknown DeviceType ($%.08X, 0x%.08X, 0x%.08X, 0x%.08X)', [DeviceType.Reserved[0], DeviceType.Reserved[1], DeviceType.Reserved[2], DeviceType.Reserved[3]]);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXGetDeviceChanges(
  DeviceType: PXPP_DEVICE_TYPE;
  pdwInsertions: PDWORD;
  pdwRemovals: PDWORD
): BOOL; stdcall;
// Branch:shogun  Revision:145  Translator:Shadow_Tj  Done:100
{$WRITEABLECONST ON}
const
  bFirst: BOOL = True;
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

  Result := False;

  // Return 1 Controller Inserted initially, then no changes forever
  if bFirst then
  begin
    pdwInsertions^ := (1 shl 0);
    pdwRemovals^ := 0;
    Result := True;
    bFirst := False;
  end
  else
  begin
    pdwInsertions^ := 0;
    pdwRemovals^ := 0;
  end;

  EmuSwapFS(fsXbox);
end;


function XTL_EmuXInputOpen(
  DeviceType: PXPP_DEVICE_TYPE;
  dwPort: DWord;
  dwSlot: DWord;
  pPollingParameters: PXINPUT_POLLING_PARAMETERS // OPTIONAL
): Handle; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
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

      if (pPollingParameters <> nil) then
      begin
        New({var XINPUT_POLLING_PARAMETERS}pPH.pPollingParameters);
        memcpy(pPH.pPollingParameters, pPollingParameters, SizeOf(XINPUT_POLLING_PARAMETERS));
      end
      else
      begin
        pPH.pPollingParameters := nil;
      end;

      g_hInputHandle[dwPort] := Handle(pPH);
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

        memcpy(pPH.pPollingParameters, pPollingParameters, SizeOf(XINPUT_POLLING_PARAMETERS));
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

  EmuSwapFS(fsXbox);

  Result := Handle(pPH);
end;


procedure XTL_EmuXInputClose(hDevice: Handle); stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pPH: PPOLLING_PARAMETERS_HANDLE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXInputClose' +
    #13#10'(' +
    #13#10'   hDevice             : 0x%.08X' +
    #13#10');',
    [hDevice]);
{$ENDIF}

  pPH := PPOLLING_PARAMETERS_HANDLE(hDevice);

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

function XTL_EmuXInputPoll(
  hDevice: Handle
): DWord; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  v: Integer;
  pFeedback: PXINPUT_FEEDBACK;
  pPH: PPOLLING_PARAMETERS_HANDLE;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXInputPoll' +
       #13#10'(' +
       #13#10'   hDevice             : 0x%.08X' +
       #13#10');',
       [hDevice]);
{$ENDIF}

  pPH := PPOLLING_PARAMETERS_HANDLE(hDevice);

  //
  // Poll input
  //

  begin
    for v := 0 to XINPUT_SETSTATE_SLOTS - 1 do
    begin
      hDevice := g_pXInputSetStateStatus[v].hDevice;

      if hDevice = 0 then
          Continue;

      g_pXInputSetStateStatus[v].dwLatency := 0;

      pFeedback := PXINPUT_FEEDBACK(g_pXInputSetStateStatus[v].pFeedback);

      if pFeedback = nil then
          Continue;

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

function XTL_EmuXInputGetCapabilities(
  hDevice: Handle;
  {OUT} pCapabilities: PXINPUT_CAPABILITIES
  ): DWord; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pPH: PPOLLING_PARAMETERS_HANDLE;
  dwPort: DWord;
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

  if (pPH <> nil) then
  begin
    dwPort := pPH.dwPort;

    if ({(dwPort >= 0) and} (dwPort <= 3)) then
    begin
      pCapabilities.SubType := XINPUT_DEVSUBTYPE_GC_GAMEPAD;

      ZeroMemory(@pCapabilities.In_.Gamepad, SizeOf(pCapabilities.In_.Gamepad));

      Result := ERROR_SUCCESS;
    end;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXInputGetState(
    hDevice: Handle;
    {OUT} pState: PXINPUT_STATE
): DWord; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
var
  pPH: PPOLLING_PARAMETERS_HANDLE;
  dwPort: DWord;
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

  if (pPH <> nil) then
  begin
    if (pPH.pPollingParameters <> nil) then
    begin
      if (pPH.pPollingParameters.fAutoPoll = Byte(False)) then
      begin
        //
        // Cxbx TODO: uh..
        //

        EmuWarning('EmuXInputGetState : fAutoPoll = False');
      end;
    end;

    dwPort := pPH.dwPort;

    if ({(dwPort >= 0) and} (dwPort <= 3)) then
    begin
      if (dwPort = 0) then
      begin
        XTL_EmuDInputPoll(pState);
        Result := ERROR_SUCCESS;
      end;
    end;
  end;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXInputSetState(
    hDevice: Handle;
    pFeedback: PXINPUT_FEEDBACK // IN OUT
): DWord; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  ret: DWord;
  pPH: PPOLLING_PARAMETERS_HANDLE;
  v: Integer;
  found: bool;

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

  if (pPH <> nil) then
  begin


    //
    // Check if this device is already being polled
    //
    found := False;

    for v := 0 to XINPUT_SETSTATE_SLOTS - 1 do
    begin
      if (g_pXInputSetStateStatus[v].hDevice = hDevice) then
      begin
        found := True;

        if (pFeedback.Header.dwStatus = ERROR_SUCCESS) then
        begin
          ret := ERROR_SUCCESS;

          // remove from slot
          g_pXInputSetStateStatus[v].hDevice := 0;
          g_pXInputSetStateStatus[v].pFeedback := nil;
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

          Break;
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

function XTL_EmuCreateMutex(
  lpMutexAttributes: LPSECURITY_ATTRIBUTES;
  bInitialOwner: BOOL;
  { Dxbx TODO : Is this really an Ansi-type? Or should it be wide (LPCWSTR) ? }
  lpName: LPCSTR
  ): Handle; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
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

  Result := CreateMutexA(PSecurityAttributes(lpMutexAttributes), bInitialOwner, lpName);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuCloseHandle(hObject: Handle): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DXBX_DEBUG}
  DbgPrintf('EmuXapi : EmuCloseHandle' +
    #13#10'(' +
    #13#10'   hObject             : 0x%.08X' +
    #13#10');',
    [hObject]);
{$ENDIF}

  Result := CloseHandle(hObject);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuSetThreadPriorityBoost(hThread: Handle; DisablePriorityBoost: BOOL): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
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

  Result := SetThreadPriorityBoost(hThread, DisablePriorityBoost);

  if not Result then
    EmuWarning('SetThreadPriorityBoost Failed!');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuSetThreadPriority(hThread: Handle; nPriority: Integer): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
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

  bRet := True; //SetThreadPriority(hThread, nPriority);  // marked by cxbx

  if not bRet then
    EmuWarning('SetThreadPriority Failed!');

  // HACK! Commente by cxbx
  //Sleep(10);

  EmuSwapFS(fsXbox);

  Result := bRet;
end;


function XTL_EmuGetThreadPriority(hThread: Handle): Integer; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
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

function XTL_EmuGetExitCodeThread(hThread: Handle; lpExitCode: Cardinal): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
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

  Result := GetExitCodeThread(hThread, lpExitCode);

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuXapiInitProcess(); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
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
    ZeroMemory(@HeapParameters, SizeOf(HeapParameters));
    HeapParameters.Length := SizeOf(HeapParameters);

    EmuSwapFS(fsXbox);

    dwPeHeapReserve := CxbxKrnl_XbeHeader.dwPeHeapReserve;
    dwPeHeapCommit := CxbxKrnl_XbeHeader.dwPeHeapCommit;

    XTL_EmuXapiProcessHeap^ := XTL_EmuRtlCreateHeap(HEAP_GROWABLE, nil,
      dwPeHeapReserve, dwPeHeapCommit, nil, @HeapParameters);
  end;
end;

procedure XTL_EmuXapiThreadStartup(StartRoutine: StartRoutineFunc; StartContext: PVOID); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
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

  // Cxbx TODO: Call thread notify routines ?
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

procedure XTL_EmuXapiBootDash(UnknownA: DWord; UnknownB: DWord; UnknownC: DWord); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
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

procedure XTL_EmuXRegisterThreadNotifyRoutine(
  pThreadNotification: PXTHREAD_NOTIFICATION;
  fRegister: BOOL
  ); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
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

  if fRegister then
  begin
    if Assigned(g_pfnThreadNotification) then
      CxbxKrnlCleanup('Multiple thread notification routines installed (caustik can fix this!)');

    // Dxbx TODO : Is this correct?
    // BlueShogun96 has code that connects notifications to the ListEntry chain.
    g_pfnThreadNotification := Addr(pThreadNotification.pfnNotifyRoutine);
  end
  else
  begin
    if Assigned(g_pfnThreadNotification) then
      g_pfnThreadNotification := nil;
  end;

  EmuSwapFS(fsXbox);
end;

type
  LPFIBER_START_ROUTINE = Pointer; // Dxbx TODO : declare this better!
   
function XTL_EmuCreateFiber(
  dwStackSize: DWORD;
  lpStartRoutine: LPFIBER_START_ROUTINE;
  lpParameter: LPVOID          
): LPVOID; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
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

procedure XTL_EmuDeleteFiber(
  lpFiber: LPVOID
  ); stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
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


function XTL_EmuXLoadSectionA(pSectionName: LPCSTR): LPVOID;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
var
  pRet: LPVOID;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXLoadSectionA' +
      #13#10'(' +
      #13#10'   pSectionName       : 0x%.08X' +
      #13#10');',
      [pSectionName]);
{$ENDIF}

  // Search this .xbe for the section it wants to load.
  // If we find it, return the address of it.

  // Get the .xbe header
  // MARKED OUT BY CXBX
  {Xbe::Header* pXbeHeader = (Xbe::Header* ) $00010000;

  // Get the number of sections this .xbe has and the
  // location of the section headers.
  DWORD dwNumSections = pXbeHeader.dwSections;
  DWORD dwSectionAddr = pXbeHeader.dwSectionHeadersAddr - pXbeHeader.dwBaseAddr;

  // Get section headers.
  Xbe::SectionHeader* pSectionHeaders = (Xbe::SectionHeader* ) CxbxMalloc( sizeof( Xbe::SectionHeader ) * dwNumSections );

  DWORD dwOffset = dwSectionAddr;

  for( DWORD i = 0; i < dwNumSections; i++ )
  begin
    memcpy( @pSectionHeaders[i], ((DWORD* ) dwOffset), sizeof( Xbe::SectionHeader ) );
    dwOffset += sizeof( Xbe::SectionHeader );
  end;

  // Find a match to the section name
  DWORD dwSection = -1;

  for( DWORD i = 0; i < dwNumSections; i++ )
  begin
    char szSectionName[32];
    dwOffset = pSectionHeaders[i].dwSectionNameAddr - pXbeHeader.dwBaseAddr;
    sprintf( szSectionName, '%s', ((DWORD* ) dwOffset) );

    // Do we have a match?
    if ( !strcmp( szSectionName, pSectionName ) ) then
    begin
      dwSection = i;
      break;
    end;
  end;

  // If we have a match, get the raw address of this section
  // and return a pointer to that address.
  if ( dwSection <> -1 ) then
  begin
    pRet = (LPVOID) pSectionHeaders[dwSection].dwRawAddr;
  end;

  // Free up the memory
  CxbxFree( pSectionHeaders );}

  pRet := NULL;

  // MARKED OUT BY CXBX
  (*int Section = -1;

{$IFDEF DEBUG}
  DbgPrintf('Sections: %d', [g_NumSections]);
  DbgPrintf('Section List 0x%.08X', [g_pSectionList]);
{$ENDIF}

  if ( g_pSectionList ) then
  begin
    for( int i = 0; i < (int) g_NumSections; i++ )
    begin
      if ( !strcmp( g_pSectionList[i].szSectionName, pSectionName ) ) then
      begin
        Section = i;
        break;
      end;
    end;

    for (int i = 0; i < g_NumSections; i++)
{$IFDEF DEBUG}
      DbgPrintf('Section #%d: %s', [i, g_pSectionList[i].szSectionName]);
{$ENDIF}

    if (Section <> -1) then
    begin
      pRet = ((LPVOID) g_pSectionList[Section].dwSectionAddr);
    end;

    __asm int 3;
  end
  else
  begin
    EmuWarning('Section List not initialized!');
    __asm int 3;
  end;*)

  EmuSwapFS(fsXbox);

  Result := pRet;
end;

function XTL_EmuXFreeSectionA(
  pSectionName: LPCSTR
  ): BOOL; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXFreeSectionA' +
      #13#10'(' +
      #13#10'   pSectionName       : 0x%.08X' +
      #13#10');',
      [pSectionName]);
{$ENDIF}

  // Cxbx TODO: Implement (if necessary)?
//  CxbxKrnlCleanup('XFreeSectionA is not implemented');

  EmuSwapFS(fsXbox);

  Result := TRUE;
end;

function XTL_EmuXGetSectionHandleA(
  pSectionName: LPCSTR
  ): HANDLE; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXGetSectionHandleA' +
      #13#10'(' +
      #13#10'   pSectionName       : 0x%.08X' +
      #13#10');',
      [pSectionName]);
{$ENDIF}

  // Cxbx TODO: Implement (if necessary)?
//  CxbxKrnlCleanup('XGetSectionHandleA is not implemented');

  EmuSwapFS(fsXbox);

  Result := 0;
end;

function XTL_EmuXLoadSectionByHandle(
  hSection: HANDLE
  ): LPVOID; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXLoadSectionByHandle' +
      #13#10'(' +
      #13#10'   hSection           : 0x%.08X' +
      #13#10');',
      [hSection]);
{$ENDIF}

  // Cxbx TODO: Implement (if necessary)?
//  CxbxKrnlCleanup('XLoadSectionByHandle is not implemented');

  EmuSwapFS(fsXbox);

  Result := NULL;
end;

function XTL_EmuXFreeSectionByHandle(
  hSection: HANDLE
  ): BOOL; stdcall;
// Branch:shogun  Revision:145  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuXapi : EmuXFreeSectionByHandle' +
      #13#10'(' +
      #13#10'   hSection           : 0x%.08X' +
      #13#10');',
      [hSection]);
{$ENDIF}

  // Cxbx TODO: Implement (if necessary)?
//  CxbxKrnlCleanup('XLoadSectionByHandle is not implemented');

  EmuSwapFS(fsXbox);

  Result := False;
end;


(*//  MARKED BY Cxbx : not necessary?
function XTL_EmuXCalculateSignatureBegin(dwFlags: DWord): Handle; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
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

  // return a fake handle value for now
  Result := $AAAAAAAA;
end;

//  MARKED BY Cxbx : not necessary?
function XTL_EmuXCalculateSignatureBeginEx(dwFlags: DWord; dwAltTitleId: DWord): Handle; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
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

  // return a fake handle value for now
  Result := $AAAAAAAA;
end;

//  MARKED BY Cxbx : not necessary?
function XTL_EmuXCalculateSignatureUpdate(hCalcSig: Handle; pbData: Byte; cbData: ULONG): DWord; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
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
function XTL_EmuXCalculateSignatureEnd(hCalcSig: Handle; pSignature: PXCALCSIG_SIGNATURE): DWord; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
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
  // XTL_EmuFindFirstFileA,
  // XTL_EmuFindNextFileA,
  // XTL_EmuXCalculateSignatureBegin,
  // XTL_EmuXCalculateSignatureBeginEx,
  // XTL_EmuXCalculateSignatureEnd,
  // XTL_EmuXCalculateSignatureUpdate,
  // XTL_EmuXLoadSectionA
  // XTL_XapiSetupPerTitleDriveLetters
  XTL_EmuCloseHandle,
  XTL_EmuCreateFiber,
  XTL_EmuCreateMutex,
  XTL_EmuDeleteFiber,
  XTL_EmuGetExitCodeThread,
  XTL_EmuGetThreadPriority,
  XTL_EmuGetTimeZoneInformation,
  XTL_EmuQueryPerformanceCounter,
  XTL_EmuQueryPerformanceFrequency,
  XTL_EmuRtlAllocateHeap,
  XTL_EmuRtlCreateHeap,
  XTL_EmuRtlDestroyHeap,
  XTL_EmuRtlFreeHeap,
  XTL_EmuRtlReAllocateHeap,
  XTL_EmuRtlSizeHeap,
  XTL_EmuSetThreadPriority,
  XTL_EmuSetThreadPriorityBoost,
  XTL_EmuXapiApplyKernelPatches,
  XTL_EmuXapiBootDash,
  XTL_EmuXapiInitProcess,
  XTL_EmuXapiThreadStartup,
  XTL_EmuXFormatUtilityDrive,
  XTL_EmuXFreeSectionA,
  XTL_EmuXFreeSectionByHandle,
  XTL_EmuXGetDeviceChanges,
  XTL_EmuXGetDevices,
  XTL_EmuXGetSectionHandleA,
  XTL_EmuXInitDevices,
  XTL_EmuXInputClose,
  XTL_EmuXInputGetCapabilities,
  XTL_EmuXInputGetState,
  XTL_EmuXInputOpen,
  XTL_EmuXInputPoll,
  XTL_EmuXInputSetState,
  XTL_EmuXLoadSectionByHandle,
  XTL_EmuXMountUtilityDrive,
  XTL_EmuXRegisterThreadNotifyRoutine
  ;

end.

