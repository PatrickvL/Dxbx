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

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // Jedi
  JwaNative,
  JwaWinType,
  // Dxbx
  uTypes,
  SysUtils,
  uLog, // DbgPrintf
  uEmu,
  uEmuFS, // EmuSwapFS
  uEmuAlloc,
  uXBController,
  uDxbxUtils,
  uDxbxKrnlUtils; // CxbxKrnl_XbeHeader

type
  ProcedureStdCall = procedure; stdcall;
  Function1ArgStdCall = function(const Arg1: DWord): Integer; stdcall;

  LPSECURITY_ATTRIBUTES = PVOID;

  RTL_HEAP_PARAMETERS = packed record
    Length: UInt32;
    Unknown: array [0..$2C-1] of BYTE;
  end;

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

  XINPUT_RUMBLE = packed record
    wLeftMotorSpeed: Word;
    wRightMotorSpeed: Word;
  end;
  PXINPUT_RUMBLE = ^XINPUT_RUMBLE;

  XINPUT_FEEDBACK_HEADER = packed record
    dwStatus: DWord;
    hEvent: HANDLE; // OPTIONAL ;
    Reserved: array[1..58] of Byte;
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
  g_pXInputSetStateStatus: array[0..XINPUT_SETSTATE_SLOTS - 1] of XInputSetStateStatus;

  // XInputOpen handles
  g_hInputHandle: array[0..XINPUT_HANDLE_SLOTS - 1] of THandle;

  XTL_EmuXapiProcessHeap: PPVOID;

// func
(*
  XTL_g_pRtlCreateHeap: XTL_pfRtlCreateHeap;
*)

implementation

uses
  // Dxbx
  uEmuKrnlPs; // g_pfnThreadNotification

const
  HEAP_HEADERSIZE = $20;

procedure XTL_EmuXapiApplyKernelPatches(); stdcall;
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuXapi : EmuXapiApplyKernelPatches()');
  EmuSwapFS(fsXbox);
{$ENDIF}

  // we dont really feel like patching, now do we?
end;

function XTL_EmuXFormatUtilityDrive(): BOOL; stdcall;
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuXapi : EmuXFormatUtilityDrive()');
  EmuSwapFS(fsXbox);
{$ENDIF}

  // Cxbx TODO: yeah... we'll format... riiiiight

  Result := True;
end;

(*
function XTL_EmuFindFirstFileA
(
  lpFileName: PAnsiChar;
  {out}lpFindFileData: LPWIN32_FIND_DATA
  ): THandle; stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuFindFirstFileA' +
    #13#10'(' +
    #13#10'   lpFileName          : 0x%.08X (%s)' +
    #13#10'   lpFindFileData      : 0x%.08X' +
    #13#10');',
    [lpFileName, lpFileName, lpFindFileData]);

    //
    // TODO: this code is replicated in NtCreateFile. make this a function
    //

    //
    // TODO: replace full directories with their shorthand (D:\, etc)
    //

  Char * szBuffer := (Char)lpFileName;
  Char * szRoot := g_strCurDrive;

    //printf('bef : %s\n', lpFileName);

  if (szBuffer <> 0) then
  begin
    // trim this off
    if (szBuffer[0] = '' and szBuffer[1] = '?' and szBuffer[2] = '?' and szBuffer[3] = '') then
    begin
      szBuffer := szBuffer + 4;
    end;

    // D:\ should map to current directory
    if ((szBuffer[0] = 'D' or szBuffer[0] = 'd') and szBuffer[1] = ':' and szBuffer[2] = '') then
    begin
      szBuffer := szBuffer + 3;
    end
    else if ((szBuffer[0] = 'T' or szBuffer[0] = 't') and szBuffer[1] = ':' and szBuffer[2] = '') then
    begin
      szBuffer := szBuffer + 3;

      szRoot := g_strTDrive;
    end
    else if ((szBuffer[0] = 'U' or szBuffer[0] = 'u') and szBuffer[1] = ':' and szBuffer[2] = '') then
    begin
      szBuffer := szBuffer + 3;

      szRoot := g_strUDrive;
    end
    else if ((szBuffer[0] = 'Z' or szBuffer[0] = 'z') and szBuffer[1] = ':' and szBuffer[2] = '') then
    begin
      szBuffer := szBuffer + 3;

      szRoot := g_strZDrive;
    end;
  end;

    //printf('af1 : %s\n', szRoot);
    //printf('af2 : %s\n', szBuffer);

    //char szOldDir[MAX_PATH];

    //GetCurrentDirectory(MAX_PATH, szOldDir);

  SetCurrentDirectory(szRoot);

  THandle hRet := FindFirstFile(szBuffer, lpFindFileData);

  if (not FAILED(hRet)) then
  begin
    while True do
    begin
      BOOL bRet := FindNextFile(hRet, lpFindFileData);

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
*)

(*
function XTL_EmuFindNextFileA
(
  in hFindFile: THandle;
  {out} lpFindFileData: LPWIN32_FIND_DATA 
  ): BOOL; stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuFindNextFileA' +
    #13#10'(' +
    #13#10'   hFindFile           : 0x%.08X' +
    #13#10'   lpFindFileData      : 0x%.08X' +
    #13#10');',
    [hFindFile, lpFindFileData]);

    //
    // TODO: replace full directories with their shorthand (D:\, etc)
    //

  BOOL bRet;

  repeat
    bRet := FindNextFile(hFindFile, lpFindFileData);

    if (not bRet) then
      Break;

    if ((StrComp(lpFindFileData.cFileName, '.') <> 0) and (StrComp(lpFindFileData.cFileName, '..') <> 0)) then
      Break;
  until False;

    //printf('Found : %s\n', lpFindFileData.cFileName);

  EmuSwapFS(fsXbox);

  Result := bRet;
end;
*)

type
  RTL_HEAP_DEFINITION = packed record
    Length: ULONG;
    Unknown: array [0..11-1] of ULONG;
  end;
  PRTL_HEAP_DEFINITION = ^RTL_HEAP_DEFINITION;

function XTL_EmuRtlCreateHeap(
  Flags: ULONG;
  Base: PVOID; // OPTIONAL
  Reserve: ULONG; // OPTIONAL
  Commit: ULONG;
  Lock: PVOID; // OPTIONAL
  RtlHeapParams: PVOID // OPTIONAL
  ): PVOID; stdcall;
var
  RtlHeapDefinition: RTL_HEAP_DEFINITION;
begin
  EmuSwapFS(fsWindows);

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

  ZeroMemory(@RtlHeapDefinition, SizeOf(RtlHeapDefinition));
  RtlHeapDefinition.Length := SizeOf(RtlHeapDefinition);

  Result := PVOID(JwaNative.RtlCreateHeap(Flags, Base, Reserve, Commit, Lock, @RtlHeapDefinition));

  DbgPrintf('pRet : 0x%.08X', [Result]);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlAllocateHeap(
  hHeap: THandle;
  dwFlags: DWord;
  dwBytes: SIZE_T): PVOID; stdcall;
var
  offs: Byte;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DXBX_EXTENSIVE_LOGGING}
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

  DbgPrintf('pRet : 0x%.08X', [Result]);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlFreeHeap(
  hHeap: THandle;
  dwFlags: DWord;
  lpMem: PVOID): BOOL; stdcall;
var
  offs: Byte;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DXBX_EXTENSIVE_LOGGING}
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
  hHeap: THandle;
  dwFlags: DWord;
  lpMem: PVOID;
  dwBytes: SIZE_T): PVOID; stdcall;
var
  offs: Byte;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DXBX_EXTENSIVE_LOGGING}
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
  end;

  if dwBytes > 0 then
    Inc(dwBytes, HEAP_HEADERSIZE);

  Result := CxbxRtlRealloc(hHeap, dwFlags, lpMem, dwBytes);
  if Assigned(Result) then
  begin
    // TODO Dxbx : Is this correct ? (See XTL_EmuRtlAllocateHeap)
    offs := Byte(RoundUp(uint32(Result), HEAP_HEADERSIZE) - uint32(Result));
    if offs = 0 then
      offs := HEAP_HEADERSIZE;

    Result := PVOID(uint32(Result) + offs);
    PByte(uint32(Result) - 1)^ := offs;
  end;

  DbgPrintf('pRet : 0x%.08X', [Result]);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuRtlSizeHeap(
  hHeap: THandle;
  dwFlags: DWord;
  lpMem: PVOID): SIZE_T; stdcall;
var
  offs: Byte;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DXBX_EXTENSIVE_LOGGING}
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

function XTL_EmuQueryPerformanceCounter(lpPerformanceCount: Int64): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuQueryPerformanceCounter' +
    #13#10'(' +
    #13#10'   lpPerformanceCount  : 0x%.08X' +
    #13#10');',
    [lpPerformanceCount]);

  bRet := QueryPerformanceCounter(lpPerformanceCount);

    // debug - 4x speed
    //lpPerformanceCount.QuadPart *= 4;

  EmuSwapFS(fsXbox);

  Result := bRet;
end;

function XTL_EmuQueryPerformanceFrequency(var lpFrequency: Int64): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuQueryPerformanceFrequency' +
    #13#10'(' +
    #13#10'   lpFrequency         : 0x%.08X' +
    #13#10');',
    [lpFrequency]);

  bRet := QueryPerformanceFrequency({var}lpFrequency);

  EmuSwapFS(fsXbox);

  Result := bRet;
end;

function XTL_EmuXMountUtilityDrive(fFormatClean: BOOL): BOOL; stdcall;
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
var
  v: Integer;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXInitDevices' +
    #13#10'(' +
    #13#10'   Unknown1            : 0x%.08X' +
    #13#10'   Unknown2            : 0x%.08X' +
    #13#10');',
    [Unknown1, Unknown2]);

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

function XTL_EmuXGetDevices(DeviceType: PXPP_DEVICE_TYPE): DWord; stdcall;
var
  ret: DWord;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXGetDevices' +
    #13#10'(' +
    #13#10'   DeviceType          : 0x%.08X' +
    #13#10');',
    [DeviceType]);

  ret := 0;

  if (DeviceType.Reserved[0] = 0) and (DeviceType.Reserved[1] = 0) and (DeviceType.Reserved[2] = 0) and (DeviceType.Reserved[3] = 0) then
    ret := (1 shl 0) // Return 1 Controller
  else
    EmuWarning('Unknown DeviceType ($%.08X, 0x%.08X, 0x%.08X, 0x%.08X)', [DeviceType.Reserved[0], DeviceType.Reserved[1], DeviceType.Reserved[2], DeviceType.Reserved[3]]);

  EmuSwapFS(fsXbox);

  Result := ret;
end;

function XTL_EmuXGetDeviceChanges(
  DeviceType: PXPP_DEVICE_TYPE;
  pdwInsertions: PDWORD;
  pdwRemovals: PDWORD
): BOOL; stdcall;
var
  bRet: BOOL;
  bFirst: BOOL;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXGetDeviceChanges' +
    #13#10'(' +
    #13#10'   DeviceType          : 0x%.08X' +
    #13#10'   pdwInsertions       : 0x%.08X' +
    #13#10'   pdwRemovals         : 0x%.08X' +
    #13#10');',
    [DeviceType, pdwInsertions, pdwRemovals]);

  bRet := False;
  bFirst := True;

  // Return 1 Controller Inserted initially, then no changes forever
  if bFirst then
  begin
    pdwInsertions^ := (1 shl 0);
    pdwRemovals^ := 0;
    bRet := True;
  end
  else
  begin
    pdwInsertions^ := 0;
    pdwRemovals^ := 0;
  end;

  EmuSwapFS(fsXbox);

  Result := bRet;
end;

(*
function XTL_EmuXInputOpen(
  DeviceType: PXPP_DEVICE_TYPE;
  dwPort: DWord;
  dwSlot: DWord:
  pPollingParameters: PXINPUT_POLLING_PARAMETERS // OPTIONAL
): THandle; stdcall;
var
  pph: PPOLLING_PARAMETERS_HANDLE;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXInputOpen' +
    #13#10'(' +
    #13#10'   DeviceType          : 0x%.08X' +
    #13#10'   dwPort              : 0x%.08X' +
    #13#10'   dwSlot              : 0x%.08X' +
    #13#10'   pPollingParameters  : 0x%.08X' +
    #13#10');',
    [DeviceType, dwPort, dwSlot, pPollingParameters]);

  pph := nil;

  if (dwPort >= 0) and (dwPort <= 3) then
  begin
    if (g_hInputHandle[dwPort] = 0) then
    begin
      pph := new POLLING_PARAMETERS_HANDLE();

      if (pPollingParameters <> nil) then
      begin
        pph.pPollingParameters := new XINPUT_POLLING_PARAMETERS();

        memcpy(@pph.pPollingParameters, pPollingParameters, SizeOf(XINPUT_POLLING_PARAMETERS));
      end;
    end
    else
    begin
      pph.pPollingParameters := nil;
    end;

    g_hInputHandle[dwPort] := pph;
  end
  else
  begin
    pph := (POLLING_PARAMETERS_HANDLE)g_hInputHandle[dwPort];

    if (pPollingParameters <> nil) then
    begin
      if (pph.pPollingParameters = nil) then
      begin
        pph.pPollingParameters := new XINPUT_POLLING_PARAMETERS();
      end;

      memcpy(@pph.pPollingParameters, pPollingParameters, SizeOf(XINPUT_POLLING_PARAMETERS));
    end
    else
    begin
      if (pph.pPollingParameters <> nil) then
      begin
        delete pph.pPollingParameters;

        pph.pPollingParameters := nil;
      end;
    end;
  end;

  pph.dwPort := dwPort;
  end;

  EmuSwapFS(fsXbox);

  Result := THandle(pph);
end;
*)

(*
procedure XTL_EmuXInputClose(hDevice: THandle); stdcall;
var
  pph: POLLING_PARAMETERS_HANDLE;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXInputClose' +
    #13#10'(' +
    #13#10'   hDevice             : 0x%.08X' +
    #13#10');',
    hDevice);

  POLLING_PARAMETERS_HANDLE * pph := (POLLING_PARAMETERS_HANDLE)hDevice;

  (* no longer necessary
  if (pph <> 0) then
  begin
    Integer v;

    for(v:=0;v<XINPUT_SETSTATE_SLOTS;v++)
    begin
      if (g_pXInputSetStateStatus[v].hDevice = hDevice) then
      begin
        // remove from slot
        g_pXInputSetStateStatus[v].hDevice := 0;
        g_pXInputSetStateStatus[v].pFeedback := 0;
        g_pXInputSetStateStatus[v].dwLatency := 0;
      end;
    end;

    if (pph.pPollingParameters <> 0) then
    begin
      delete pph.pPollingParameters;
    end;

    delete pph;
   end;
  //*/

  EmuSwapFS(fsXbox);
end;
*)

(*
function XTL_EmuXInputPoll
(
  hDevice: THandle
): DWord; stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXInputPoll' +
       #13#10'(' +
       #13#10'   hDevice             : 0x%.08X' +
       #13#10');',
       [hDevice);

  POLLING_PARAMETERS_HANDLE *pph := (POLLING_PARAMETERS_HANDLE)hDevice;

  //
  // Poll input
  //

  begin
    Integer v;

    for(v:=0;v<XINPUT_SETSTATE_SLOTS;v++)
    begin
      THandle hDevice := g_pXInputSetStateStatus[v].hDevice;

      if (hDevice = 0) then
          Continue;

      g_pXInputSetStateStatus[v].dwLatency := 0;

      XTL.PXINPUT_FEEDBACK pFeedback := (XTL.PXINPUT_FEEDBACK)g_pXInputSetStateStatus[v].pFeedback;

      if (pFeedback = 0) then
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
*)

(*
function XTL_EmuXInputGetCapabilities
(
    hDevice: THandle;
    {OUT} pCapabilities: PXINPUT_CAPABILITIES 
): DWord; stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXInputGetCapabilities' +
       #13#10'(' +
       #13#10'   hDevice             : 0x%.08X' +
       #13#10'   pCapabilities       : 0x%.08X' +
       #13#10');',
       [hDevice, pCapabilities]);

  DWord ret := ERROR_INVALID_HANDLE;

  POLLING_PARAMETERS_HANDLE *pph := (POLLING_PARAMETERS_HANDLE)hDevice;

  if (pph <> 0) then
  begin
    DWord dwPort := pph.dwPort;

    if ((dwPort >= 0) and (dwPort <= 3)) then
    begin
      pCapabilities.SubType := XINPUT_DEVSUBTYPE_GC_GAMEPAD;

      ZeroMemory(@pCapabilities.In.Gamepad, SizeOf(pCapabilities.In.Gamepad));

      ret := ERROR_SUCCESS;
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := ret;
end;
*)

(*
function XTL_EmuXInputGetState
(
    hDevice: THandle;
    {OUT} pState: PXINPUT_STATE
): DWord; stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXInputGetState' +
       #13#10'(' +
       #13#10'   hDevice             : 0x%.08X' +
       #13#10'   pState              : 0x%.08X' +
       #13#10');',
       [hDevice, pState);

  DWord ret := ERROR_INVALID_HANDLE;

  POLLING_PARAMETERS_HANDLE *pph := (POLLING_PARAMETERS_HANDLE)hDevice;

  if (pph <> 0) then
  begin
    if (pph.pPollingParameters <> 0) then
    begin
      if (pph.pPollingParameters.fAutoPoll = False) then
      begin
        //
        // TODO: uh..
        //

        EmuWarning('EmuXInputGetState : fAutoPoll := False');
      end;
    end;

    DWord dwPort := pph.dwPort;

    if ((dwPort >= 0) and (dwPort <= 3)) then
    begin
      if (dwPort = 0) then
      begin
        EmuDInputPoll(pState);
        ret := ERROR_SUCCESS;
      end;
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := ret;
end;
*)

(*
function XTL_EmuXInputSetState
(
    hDevice: THandle;
    pFeedback: PXINPUT_FEEDBACK // IN OUT
): DWord; stdcall;
begin
    EmuSwapFS(fsWindows);

    DbgPrintf('EmuXapi : EmuXInputSetState' +
           #13#10'(' +
           #13#10'   hDevice             : 0x%.08X' +
           #13#10'   pFeedback           : 0x%.08X' +
           #13#10');',
           [hDevice, pFeedback);

    DWord ret := ERROR_IO_PENDING;

    POLLING_PARAMETERS_HANDLE *pph := (POLLING_PARAMETERS_HANDLE)hDevice;

    if (pph <> 0) then
    begin
        Integer v;

        //
        // Check if this device is already being polled
        //

        bool found := False;

        for(v:=0;v<XINPUT_SETSTATE_SLOTS;v++)
        begin
            if (g_pXInputSetStateStatus[v].hDevice = hDevice) then
            begin
                found := True;

                if (pFeedback.Header.dwStatus = ERROR_SUCCESS) then
                begin
                    ret := ERROR_SUCCESS;

                    // remove from slot
                    g_pXInputSetStateStatus[v].hDevice := 0;
                    g_pXInputSetStateStatus[v].pFeedback := 0;
                    g_pXInputSetStateStatus[v].dwLatency := 0;
                 end;
             end;
         end;

        //
        // If device was not already slotted, queue it
        //

        if (not found) then
        begin
            for(v:=0;v<XINPUT_SETSTATE_SLOTS;v++)
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
*)

function XTL_EmuCreateMutex(
  lpMutexAttributes: LPSECURITY_ATTRIBUTES;
  bInitialOwner: BOOL;
  lpName: PAnsiChar): THandle; stdcall;
var
  hRet: THandle;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuCreateMutex' +
         #13#10'(' +
         #13#10'   lpMutexAttributes   : 0x%.08X' +
         #13#10'   bInitialOwner       : 0x%.08X' +
         #13#10'   lpName              : 0x%.08X (%s)' +
         #13#10');',
         [lpMutexAttributes, bInitialOwner, lpName, lpName]);

  hRet := CreateMutex(PSecurityAttributes(lpMutexAttributes), bInitialOwner, lpName);

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuCloseHandle(hObject: THandle): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuCloseHandle' +
    #13#10'(' +
    #13#10'   hObject             : 0x%.08X' +
    #13#10');',
    [hObject]);

  bRet := CloseHandle(hObject);

  EmuSwapFS(fsXbox);

  Result := bRet;
end;

function XTL_EmuSetThreadPriorityBoost(hThread: THandle; DisablePriorityBoost: BOOL): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuSetThreadPriorityBoost' +
    #13#10'(' +
    #13#10'   hThread             : 0x%.08X' +
    #13#10'   DisablePriorityBoost: 0x%.08X' +
    #13#10');',
    [hThread, DisablePriorityBoost]);

  bRet := SetThreadPriorityBoost(hThread, DisablePriorityBoost);

  if not bRet then
    EmuWarning('SetThreadPriorityBoost Failed!');

  EmuSwapFS(fsXbox);

  Result := bRet;
end;

function XTL_EmuSetThreadPriority(hThread: THandle; nPriority: Integer): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuSetThreadPriority' +
    #13#10'(' +
    #13#10'   hThread             : 0x%.08X' +
    #13#10'   nPriority           : 0x%.08X' +
    #13#10');',
    [hThread, nPriority]);

  bRet := True; //SetThreadPriority(hThread, nPriority);

  if not bRet then
    EmuWarning('SetThreadPriority Failed!');

  // HACK!
  //Sleep(10);

  EmuSwapFS(fsXbox);

  Result := bRet;
end;


function XTL_EmuGetThreadPriority(hThread: THandle): Integer; stdcall;
var
  iRet: Integer;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuGetThreadPriority' +
    #13#10'(' +
    #13#10'   hThread             : 0x%.08X' +
    #13#10');',
    [hThread]);

  iRet := GetThreadPriority(hThread);

  if iRet = THREAD_PRIORITY_ERROR_RETURN then
    EmuWarning('GetThreadPriority Failed!');

  EmuSwapFS(fsXbox);

  Result := iRet;
end;

function XTL_EmuGetExitCodeThread(hThread: THandle; lpExitCode: Cardinal): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuGetExitCodeThread' +
    #13#10'(' +
    #13#10'   hThread             : 0x%.08X' +
    #13#10'   lpExitCode          : 0x%.08X' +
    #13#10');',
    [hThread, lpExitCode]);

  bRet := GetExitCodeThread(hThread, lpExitCode);

  EmuSwapFS(fsXbox);

  Result := bRet;
end;

procedure XTL_EmuXapiInitProcess(); stdcall;
const
  HEAP_GROWABLE = $00000002;
var
  HeapParameters: RTL_HEAP_PARAMETERS;
  dwPeHeapReserve: UInt32;
  dwPeHeapCommit: UInt32;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXapiInitProcess();');

  // call RtlCreateHeap
  begin
    ZeroMemory(@HeapParameters, SizeOf(HeapParameters));

    HeapParameters.Length := SizeOf(HeapParameters);

    EmuSwapFS(fsXbox);

    dwPeHeapReserve := CxbxKrnl_XbeHeader.dwPeHeapReserve;
    dwPeHeapCommit := CxbxKrnl_XbeHeader.dwPeHeapCommit;

    // Dxbx TODO : Determine if XTL_g_pRtlCreateHeap is indeed the same as XTL_EmuRtlCreateHeap.

    XTL_EmuXapiProcessHeap^ := XTL_EmuRtlCreateHeap(HEAP_GROWABLE, nil, dwPeHeapReserve, dwPeHeapCommit, nil, @HeapParameters);
  end;
end;

procedure XTL_EmuXapiThreadStartup(dwDummy1, dwDummy2: DWord); stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXapiThreadStartup' +
    #13#10'(' +
    #13#10'   dwDummy1            : 0x%.08X' +
    #13#10'   dwDummy2            : 0x%.08X' +
    #13#10')',
    [dwDummy1, dwDummy2]);

  EmuSwapFS(fsXbox);

  Function1ArgStdCall(dwDummy1)(dwDummy2);

  // TODO: Call thread notify routines ?
end;

(* Too High Level!
XTL.NTSTATUS CDECL XTL.XapiSetupPerTitleDriveLetters(DWord dwTitleId, PWideChar wszTitleName)
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : XapiSetupPerTitleDriveLetters' +
         #13#10'(' +
         #13#10'   dwTitleId           : 0x%.08X' +
         #13#10'   wszTitleName        : 0x%.08X' +
         #13#10');',
          [dwTitleId, wszTitleName);

  NTSTATUS ret := STATUS_SUCCESS;

  EmuSwapFS(fsXbox);

  Result := ret;
end;
*)

procedure XTL_EmuXapiBootDash(UnknownA: DWord; UnknownB: DWord; UnknownC: DWord); stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXapiBootDash' +
    #13#10'(' +
    #13#10'   UnknownA            : 0x%.08X' +
    #13#10'   UnknownB            : 0x%.08X' +
    #13#10'   UnknownC            : 0x%.08X' +
    #13#10');',
    [UnknownA, UnknownB, UnknownC]);

  CxbxKrnlCleanup('Emulation Terminated (XapiBootDash)');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuXRegisterThreadNotifyRoutine(
  pThreadNotification: PXTHREAD_NOTIFICATION;
  fRegister: BOOL); stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXRegisterThreadNotifyRoutine' +
    #13#10'(' +
    #13#10'   pThreadNotification : 0x%.08X ($%.08X)' +
    #13#10'   fRegister           : 0x%.08X' +
    #13#10');',
    [pThreadNotification, Addr(pThreadNotification.pfnNotifyRoutine), Integer(fRegister)]);

  if fRegister then
  begin
    if Assigned(g_pfnThreadNotification) then
      CxbxKrnlCleanup('Multiple thread notification routines installed (caustik can fix this!)');

    // TODO : Is this correct?
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

(*// Cxbx : not necessary?
function XTL_EmuXCalculateSignatureBegin(dwFlags: DWord): THandle; stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : XCalculateSignatureBegin' +
           #13#10'(' +
           #13#10'   dwFlags             : 0x%.08X' +
           #13#10');',
            [dwFlags]);

  EmuSwapFS(fsXbox);

  // return a fake handle value for now
  Result := $AAAAAAAA;
end;

function XTL_EmuXCalculateSignatureBeginEx(dwFlags: DWord; dwAltTitleId: DWord): THandle; stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : XCalculateSignatureBeginEx' +
    #13#10'(' +
    #13#10'   dwFlags             : 0x%.08X' +
    #13#10'   dwAltTitleId        : 0x%.08X' +
    #13#10');',
    [dwFlags, dwAltTitleId]);

  EmuSwapFS(fsXbox);

  // return a fake handle value for now
  Result := $AAAAAAAA;
end;

function XTL_EmuXCalculateSignatureUpdate(hCalcSig: THandle; pbData: Byte; cbData: ULONG): DWord; stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : XCalculateSignatureUpdate' +
           #13#10'(' +
           #13#10'   hCalcSig            : 0x%.08X' +
           #13#10'   pbData              : 0x%.08X' +
           #13#10'   cbData              : 0x%.08X' +
           #13#10');',
            [hCalcSig, pbData, cbData]);

  EmuSwapFS(fsXbox);

  Result := ERROR_SUCCESS;
end;

function XTL_EmuXCalculateSignatureEnd(hCalcSig: THandle; pSignature: PXCALCSIG_SIGNATURE): DWord; stdcall;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : XCalculateSignatureEnd' +
           #13#10'(' +
           #13#10'   hCalcSig            : 0x%.08X' +
           #13#10'   pSignature          : 0x%.08X' +
           #13#10');',
            [hCalcSig, pSignature]);

  EmuSwapFS(fsXbox);

  Result := ERROR_SUCCESS;
end;
*)

exports
  XTL_EmuCloseHandle,
  XTL_EmuCreateMutex,
(*
  XTL_EmuFindFirstFileA
  XTL_EmuFindNextFileA
*)
  XTL_EmuGetExitCodeThread,
  XTL_EmuGetThreadPriority,
  XTL_EmuQueryPerformanceCounter,
  XTL_EmuQueryPerformanceFrequency,
  XTL_EmuRtlAllocateHeap,
  XTL_EmuRtlCreateHeap,
  XTL_EmuRtlFreeHeap,
  XTL_EmuRtlReAllocateHeap,
  XTL_EmuRtlSizeHeap,
  XTL_EmuSetThreadPriority,
  XTL_EmuSetThreadPriorityBoost,
  XTL_EmuXapiApplyKernelPatches,
  XTL_EmuXapiBootDash,
  XTL_EmuXapiInitProcess,
  XTL_EmuXapiThreadStartup,
(*
  XTL_EmuXCalculateSignatureBegin,
  XTL_EmuXCalculateSignatureBeginEx,
  XTL_EmuXCalculateSignatureEnd,
  XTL_EmuXCalculateSignatureUpdate,
*)
  XTL_EmuXFormatUtilityDrive,
  XTL_EmuXGetDevices,
  XTL_EmuXGetDeviceChanges,
  XTL_EmuXInitDevices,
  XTL_EmuXMountUtilityDrive,
  XTL_EmuXRegisterThreadNotifyRoutine
  ;

end.

