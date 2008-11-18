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
  uLog, // DbgPrintf
  uEmu,
  uEmuFS, // EmuSwapFS
  uEmuAlloc,
  uXBController,
  uDxbxUtils,
  uDxbxKrnlUtils; // CxbxKrnl_XbeHeader

type
  ProcedureStdCall = procedure; stdcall;
  Function1ArgStdCall = function(const Arg1: DWORD): Integer; stdcall;

  RTL_HEAP_PARAMETERS = packed record
    Length: UInt32;
    // TODO!
  end;

  XTHREAD_NOTIFY_PROC = procedure(fCreate: BOOL); stdcall;

  _XTHREAD_NOTIFICATION = record
    Reserved: LIST_ENTRY;
    pfnNotifyRoutine: XTHREAD_NOTIFY_PROC;
  end;
  XTHREAD_NOTIFICATION = _XTHREAD_NOTIFICATION;
  PXTHREAD_NOTIFICATION = ^XTHREAD_NOTIFICATION;

  _XINPUT_RUMBLE = record
    wLeftMotorSpeed: WORD;
    wRightMotorSpeed: WORD;
  end;
  XINPUT_RUMBLE = _XINPUT_RUMBLE;
  PXINPUT_RUMBLE = ^XINPUT_RUMBLE;

  _XINPUT_FEEDBACK_HEADER = record
    dwStatus: DWORD;
    hEvent: HANDLE; // OPTIONAL ;
    Reserved: array[1..58] of BYTE;
  end;
  XINPUT_FEEDBACK_HEADER = _XINPUT_FEEDBACK_HEADER;
  PXINPUT_FEEDBACK_HEADER = ^XINPUT_FEEDBACK_HEADER;

  _XINPUT_FEEDBACK = record
    Header: XINPUT_FEEDBACK_HEADER;
//    union
    Rumble: XINPUT_RUMBLE;
  end;
  XINPUT_FEEDBACK = _XINPUT_FEEDBACK;
  PXINPUT_FEEDBACK = ^XINPUT_FEEDBACK;

procedure XTL_EmuXapiApplyKernelPatches(); stdcall;
function XTL_EmuXFormatUtilityDrive(): BOOL; stdcall;
function XTL_EmuRtlCreateHeap(Flags: ULONG; Base: PVOID; Reserve: ULONG; Commit: ULONG; Lock: PVOID; RtlHeapParams: PVOID): PVOID; stdcall;
function XTL_EmuRtlAllocateHeap(hHeap: THandle; dwFlags: DWORD; dwBytes: SIZE_T): PVOID; stdcall;
function XTL_EmuRtlFreeHeap(hHeap: THandle; dwFlags: DWORD; lpMem: PVOID): BOOL; stdcall;
function XTL_EmuRtlReAllocateHeap(hHeap: THandle; dwFlags: DWORD; lpMem: PVOID; dwBytes: SIZE_T): PVOID; stdcall;
function XTL_EmuRtlSizeHeap(hHeap: THandle; dwFlags: DWORD; lpMem: PVOID): SIZE_T; stdcall;

function XTL_EmuQueryPerformanceCounter(lpPerformanceCount: Int64): BOOL; stdcall;
function XTL_EmuQueryPerformanceFrequency(lpFrequency: Int64): BOOL; stdcall;
function XTL_EmuXMountUtilityDrive(fFormatClean: BOOL): BOOL; stdcall;
procedure XTL_EmuXInitDevices(Unknown1: DWORD; Unknown2: PVOID); stdcall;
function XTL_EmuCloseHandle(hObject: THandle): BOOL; stdcall;
function XTL_EmuSetThreadPriorityBoost(hThread: THandle; DisablePriorityBoost: BOOL): BOOL; stdcall;
function XTL_EmuSetThreadPriority(hThread: THandle; nPriority: integer): BOOL; stdcall;
function XTL_EmuGetThreadPriority(hThread: THandle): integer; stdcall;
function XTL_EmuGetExitCodeThread(hThread: THandle; lpExitCode: Cardinal): BOOL; stdcall;

procedure XTL_EmuXapiInitProcess(); stdcall;
procedure XTL_EmuXapiThreadStartup(dwDummy1, dwDummy2: DWORD) stdcall;
procedure XTL_EmuXapiBootDash(UnknownA: DWORD; UnknownB: DWORD; UnknownC: DWORD); stdcall;

procedure XTL_EmuXRegisterThreadNotifyRoutine(pThreadNotification: PXTHREAD_NOTIFICATION; fRegister: BOOL); stdcall;
Function XTL_EmuXCalculateSignatureBegin ( dwFlags : DWORD ) : THandle; stdcall;
Function XTL_EmuXCalculateSignatureBeginEx (dwFlags : DWORD; dwAltTitleId : DWORD) : THandle; stdcall;
Function XTL_EmuXCalculateSignatureUpdate ( hCalcSig : THandle; pbData : BYTE; cbData : ULONG ) : DWORD; stdcall;

exports
  XTL_EmuXapiApplyKernelPatches,
  XTL_EmuXFormatUtilityDrive,
  XTL_EmuRtlCreateHeap,
  XTL_EmuRtlAllocateHeap,
  XTL_EmuRtlFreeHeap,
  XTL_EmuRtlReAllocateHeap,
  XTL_EmuRtlSizeHeap,

  XTL_EmuQueryPerformanceCounter,
  XTL_EmuQueryPerformanceFrequency,
  XTL_EmuXMountUtilityDrive,
  XTL_EmuXInitDevices,
  XTL_EmuCloseHandle,
  XTL_EmuSetThreadPriorityBoost,
  XTL_EmuSetThreadPriority,
  XTL_EmuGetThreadPriority,
  XTL_EmuGetExitCodeThread,

  XTL_EmuXapiInitProcess,
  XTL_EmuXapiThreadStartup,
  XTL_EmuXapiBootDash,

  XTL_EmuXRegisterThreadNotifyRoutine,
  XTL_EmuXCalculateSignatureBegin,
  XTL_EmuXCalculateSignatureBeginEx,
  XTL_EmuXCalculateSignatureUpdate
  ;

var
  // XInputSetState status waiters
  g_pXInputSetStateStatus: array[0..XINPUT_SETSTATE_SLOTS - 1] of XInputSetStateStatus;

  // XInputOpen handles
  g_hInputHandle: array[0..XINPUT_HANDLE_SLOTS - 1] of THandle;

implementation

uses
  uEmuKrnlPs; // g_pfnThreadNotification

{ TODO : Need to be translated to delphi }



// func: EmuXapiApplyKernelPatches

procedure XTL_EmuXapiApplyKernelPatches();
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(); // Win2k/XP FS
  DbgPrintf('EmuXapi : EmuXapiApplyKernelPatches()');
  EmuSwapFS(); // XBox FS
{$ENDIF}

    // we dont really feel like patching, now do we?

  Exit;
end;

// func: EmuXFormatUtilityDrive

function XTL_EmuXFormatUtilityDrive(): BOOL;
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(); // Win2k/XP FS
  DbgPrintf('EmuXapi : EmuXFormatUtilityDrive()');
  EmuSwapFS(); // XBox FS
{$ENDIF}

    // TODO: yeah... we'll format... riiiiight

  Result := True;
end;

// func: EmuFindFirstFileA
(*THandle WINAPI XTL.EmuFindFirstFileA
(
  in PAnsiChar lpFileName,
  out LPWIN32_FIND_DATA lpFindFileData
  )
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuFindFirstFileA' +
    #13#10'(' +
    #13#10'   lpFileName          : $%.08X (%s)' +
    #13#10'   lpFindFileData      : $%.08X' +
    #13#10');',
    [lpFileName, lpFileName, lpFindFileData);

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
    do
    begin
      BOOL bRet := FindNextFile(hRet, lpFindFileData);

      if (not bRet) then begin hRet := INVALID_HANDLE_VALUE; break; end;

      if ((StrComp(lpFindFileData.cFileName, '.') <> 0) and (StrComp(lpFindFileData.cFileName, '..') <> 0)) then
        break;
    end;
    while (True);
  end;

    //SetCurrentDirectory(szOldDir);

  EmuSwapFS(); // XBox FS

  Result := hRet;
end;    *)

// ******************************************************************
// * func: EmuFindNextFileA
// ******************************************************************
(*BOOL WINAPI XTL.EmuFindNextFileA
(
  in THandle hFindFile,
  out LPWIN32_FIND_DATA lpFindFileData
  )
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuFindNextFileA' +
    #13#10'(' +
    #13#10'   hFindFile           : $%.08X' +
    #13#10'   lpFindFileData      : $%.08X' +
    #13#10');',
    [hFindFile, lpFindFileData);

    //
    // TODO: replace full directories with their shorthand (D:\, etc)
    //

  BOOL bRet;

  do
  begin
    bRet := FindNextFile(hFindFile, lpFindFileData);

    if (not bRet) then begin break; end;

    if ((StrComp(lpFindFileData.cFileName, '.') <> 0) and (StrComp(lpFindFileData.cFileName, '..') <> 0)) then
      break;
  end;
  while (True);

    //printf('Found : %s\n', lpFindFileData->cFileName);

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;     *)
//*/

type
  RTL_HEAP_DEFINITION = record Length: Integer; end; // TODO

// ******************************************************************
// * func: EmuRtlCreateHeap
// ******************************************************************

function XTL_EmuRtlCreateHeap
  (
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
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuRtlCreateHeap' );
  DbgPrintf('(');
  DbgPrintf('   Flags               : $%.08X', [Flags]);
  DbgPrintf('   Base                : $%.08X', [@Base]);
  DbgPrintf('   Reserve             : $%.08X', [Reserve]);
  DbgPrintf('   Commit              : $%.08X', [Commit]);
  DbgPrintf('   Lock                : $%.08X', [@Lock]);
  DbgPrintf('   RtlHeapParams       : $%.08X', [@RtlHeapParams]);
  DbgPrintf(');');

  ZeroMemory(@RtlHeapDefinition, SizeOf(RtlHeapDefinition));

  RtlHeapDefinition.Length := SizeOf(RtlHeapDefinition);

  Result := PVOID(JwaNative.RtlCreateHeap(Flags, Base, Reserve, Commit, Lock, @RtlHeapDefinition));

  EmuSwapFS(); // XBox FS
end;

// ******************************************************************
// * func: EmuRtlAllocateHeap
// ******************************************************************

function XTL_EmuRtlAllocateHeap(
  hHeap: THandle;
  dwFlags: DWORD;
  dwBytes: SIZE_T
  ): PVOID; stdcall;
var
  offs: BYTE;
begin
  EmuSwapFS(); // Win2k/XP FS

  //* too much debug output
  DbgPrintf ( 'EmuXapi : EmuRtlAllocateHeap' );
  DbgPrintf ( '(' );
  DbgPrintf ( '   hHeap               : $%.08X', [hHeap] );
  DbgPrintf ( '   dwFlags             : $%.08X', [dwFlags] );
  DbgPrintf ( '   dwBytes             : $%.08X', [dwBytes] );
  DbgPrintf ( ');' );
  //*/

  Result := PVOID(JwaNative.RtlAllocateHeap(hHeap, dwFlags, dwBytes + $20));

  offs := BYTE(RoundUp(uint32(Result), $20) - uint32(Result));

  if offs = 0 then
    offs := $20;

  Result := PVOID(uint32(Result) + offs);

  PBYTE(uint32(Result) - 1)^ := offs;

  DbgPrintf('pRet : $%.08X', [Result]);

  EmuSwapFS(); // XBox FS
end;

// ******************************************************************
// * func: EmuRtlFreeHeap
// ******************************************************************

function XTL_EmuRtlFreeHeap(
  hHeap: THandle;
  dwFlags: DWORD;
  lpMem: PVOID
  ): BOOL; stdcall;
var
  offs: BYTE;
  bRet: BOOL;
begin
  EmuSwapFS(); // Win2k/XP FS

  //* too much debug output
  DbgPrintf('EmuXapi : EmuRtlFreeHeap');
  DbgPrintf('(');
  DbgPrintf('   hHeap               : $%.08X', [hHeap]);
  DbgPrintf('   dwFlags             : $%.08X', [dwFlags]);
  DbgPrintf('   lpMem               : $%.08X', [lpMem]);
  DbgPrintf(');');
  //*

  if Assigned(lpMem) then
  begin
    offs := PBYTE(uint32(Result) - 1)^;
    lpMem := PVOID(uint32(lpMem) - offs);
  end;

  bRet := CxbxRtlFree(hHeap, dwFlags, lpMem);

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;

// ******************************************************************
// * func: EmuRtlReAllocateHeap
// ******************************************************************

function XTL_EmuRtlReAllocateHeap(
  hHeap: THandle;
  dwFlags: DWORD;
  lpMem: PVOID;
  dwBytes: SIZE_T
  ): PVOID; stdcall;
var
  offs: BYTE;
  pRet: PVOID;
begin
  EmuSwapFS(); // Win2k/XP FS

  //* too much debug output
  DbgPrintf('EmuXapi : EmuRtlReAllocateHeap');
  DbgPrintf('(');
  DbgPrintf('   hHeap               : $%.08X', [hHeap]);
  DbgPrintf('   dwFlags             : $%.08X', [dwFlags]);
  DbgPrintf('   lpMem               : $%.08X', [lpMem]);
  DbgPrintf('   dwBytes             : $%.08X', [dwBytes]);
  DbgPrintf(');');
   //*/

  if Assigned(lpMem) then
  begin
    offs := PBYTE(uint32(lpMem) - 1)^;

    lpMem := PVOID(uint32(lpMem) - offs);
  end;

  pRet := CxbxRtlRealloc(hHeap, dwFlags, lpMem, dwBytes + $20);

  EmuSwapFS(); // XBox FS

  Result := pRet;
end;

// ******************************************************************
// * func: EmuRtlSizeHeap
// ******************************************************************

function XTL_EmuRtlSizeHeap(
  hHeap: THandle;
  dwFlags: DWORD;
  lpMem: PVOID
  ): SIZE_T; stdcall;
var
  offs: BYTE;
  ret: SIZE_T;
begin
  EmuSwapFS(); // Win2k/XP FS

  //* too much debug output
  DbgPrintf('EmuXapi : EmuRtlSizeHeap');
  DbgPrintf('(');
  DbgPrintf('   hHeap               : $%.08X', [hHeap]);
  DbgPrintf('   dwFlags             : $%.08X', [dwFlags]);
  DbgPrintf('   lpMem               : $%.08X', [lpMem]);
  DbgPrintf(');');
  //*/

  if Assigned(lpMem) then
  begin
    offs := PBYTE(uint32(Result) - 1)^;

    lpMem := PVOID(uint32(lpMem) - offs);
  end;

  ret := CxbxRtlSizeHeap(hHeap, dwFlags, lpMem) - $20;

  EmuSwapFS(); // XBox FS

  Result := ret;
end;

// ******************************************************************
// * func: EmuQueryPerformanceCounter
// ******************************************************************

function XTL_EmuQueryPerformanceCounter(lpPerformanceCount: Int64): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuQueryPerformanceCounter');
  DbgPrintf('(');
  DbgPrintf('   lpPerformanceCount  : $%.08X', [lpPerformanceCount]);
  DbgPrintf(');');

  bRet := QueryPerformanceCounter(lpPerformanceCount);

    // debug - 4x speed
    //lpPerformanceCount->QuadPart *= 4;

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;

// ******************************************************************
// * func: EmuQueryPerformanceFrequency
// ******************************************************************

function XTL_EmuQueryPerformanceFrequency(lpFrequency: Int64): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuQueryPerformanceFrequency');
  DbgPrintf('(');
  DbgPrintf('   lpFrequency         : $%.08X', [lpFrequency]);
  DbgPrintf(');');

  bRet := QueryPerformanceFrequency(lpFrequency);

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;

// ******************************************************************
// * func: EmuXMountUtilityDrive
// ******************************************************************

function XTL_EmuXMountUtilityDrive(fFormatClean: BOOL): BOOL; stdcall;
begin
{$IFDEF _DEBUG_TRACE}
  begin
    EmuSwapFS(); // Win2k/XP FS
    DbgPrintf('EmuXapi : EmuXMountUtilityDrive' +
      #13#10'(' +
      #13#10'   fFormatClean        : $%.08X' +
      #13#10');',
      fFormatClean);
    EmuSwapFS(); // XBox FS
  end;
{$ENDIF}

  Result := True;
end;

// ******************************************************************
// * func: EmuXInitDevices
// ******************************************************************

procedure XTL_EmuXInitDevices(Unknown1: DWORD; Unknown2: PVOID); stdcall;
var
  v: integer;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXInitDevices');
  DbgPrintf('(');
  DbgPrintf('   Unknown1            : $%.08X', [Unknown1]);
  DbgPrintf('   Unknown2            : $%.08X', [Unknown2]);
  DbgPrintf(');');

  for v := 0 to XINPUT_SETSTATE_SLOTS - 1 do
  begin
    g_pXInputSetStateStatus[v].hDevice := 0;
    g_pXInputSetStateStatus[v].dwLatency := 0;
    g_pXInputSetStateStatus[v].pFeedback := Nil;
  end;

  for v := 0 to XINPUT_HANDLE_SLOTS - 1 do
  begin
    g_hInputHandle[v] := 0;
  end;

  EmuSwapFS(); // XBox FS
end;

// ******************************************************************
// * func: EmuXGetDevices
// ******************************************************************
(*Function XTL_EmuXGetDevices ( DeviceType : PXPP_DEVICE_TYPE ): DWORD
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXGetDevices' +
    #13#10'(' +
    #13#10'   DeviceType          : $%.08X' +
    #13#10');',
    [DeviceType);

  DWORD ret := 0;

  if (DeviceType.Reserved[0] = 0 and DeviceType.Reserved[1] = 0 and DeviceType.Reserved[2] = 0 and DeviceType.Reserved[3] = 0) then
    ret := (1 shl 0) // Return 1 Controller
  else
    EmuWarning('Unknown DeviceType ($%.08X, $%.08X, $%.08X, $%.08X)', DeviceType.Reserved[0], DeviceType.Reserved[1], DeviceType.Reserved[2], DeviceType.Reserved[3]);

  EmuSwapFS(); // XBox FS

  Result := ret;
end;       *)

// ******************************************************************
// * func: EmuXGetDeviceChanges
// ******************************************************************
(*BOOL WINAPI XTL.EmuXGetDeviceChanges
(
  PXPP_DEVICE_TYPE DeviceType,
  PDWORD pdwInsertions,
  PDWORD pdwRemovals
  )
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXGetDeviceChanges' +
    #13#10'(' +
    #13#10'   DeviceType          : $%.08X' +
    #13#10'   pdwInsertions       : $%.08X' +
    #13#10'   pdwRemovals         : $%.08X' +
    #13#10');',
    [DeviceType, pdwInsertions, pdwRemovals);

  BOOL bRet := False;
  BOOL bFirst := True;

    // Return 1 Controller Inserted initially, then no changes forever
  if (bFirst) then
  begin
    * pdwInsertions := (1 shl 0);
    * pdwRemovals := 0;
    bRet := True;
  end;
else
  begin
    * pdwInsertions := 0;
    * pdwRemovals := 0;
  end;

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;        *)

// ******************************************************************
// * func: EmuXInputOpen
// ******************************************************************
(*Function XTL_EmuXInputOpen
(
  in PXPP_DEVICE_TYPE DeviceType,
  in DWORD dwPort,
  in DWORD dwSlot,
  in PXINPUT_POLLING_PARAMETERS pPollingParameters OPTIONAL
  ) : THandle
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXInputOpen' +
    #13#10'(' +
    #13#10'   DeviceType          : $%.08X' +
    #13#10'   dwPort              : $%.08X' +
    #13#10'   dwSlot              : $%.08X' +
    #13#10'   pPollingParameters  : $%.08X' +
    #13#10');',
    [DeviceType, dwPort, dwSlot, pPollingParameters);

  POLLING_PARAMETERS_HANDLE * pph := 0;

  if (dwPort >= 0) and (dwPort <= 3) then
  begin
    if (g_hInputHandle[dwPort] = 0) then
    begin
      pph := new POLLING_PARAMETERS_HANDLE();

      if (pPollingParameters <> 0) then
      begin
        pph.pPollingParameters := new XINPUT_POLLING_PARAMETERS();

        memcpy(@pph.pPollingParameters, pPollingParameters, SizeOf(XINPUT_POLLING_PARAMETERS));
      end;
    end
    else
    begin
      pph.pPollingParameters := 0;
    end;

    g_hInputHandle[dwPort] := pph;
  end
  else
  begin
    pph := (POLLING_PARAMETERS_HANDLE)g_hInputHandle[dwPort];

    if (pPollingParameters <> 0) then
    begin
      if (pph.pPollingParameters = 0) then
      begin
        pph.pPollingParameters := new XINPUT_POLLING_PARAMETERS();
      end;

      memcpy(@pph.pPollingParameters, pPollingParameters, SizeOf(XINPUT_POLLING_PARAMETERS));
    end
    else
    begin
      if (pph.pPollingParameters <> 0) then
      begin
        delete pph.pPollingParameters;

        pph.pPollingParameters := 0;
      end;
    end;
  end;

  pph.dwPort := dwPort;
  end;

  EmuSwapFS(); // XBox FS

  Result := (THandle)pph;
end;
*)

// ******************************************************************
// * func: EmuXInputClose
// ******************************************************************
(*procedure XTL_EmuXInputClose ( hDevice : THandle );
var pph : POLLING_PARAMETERS_HANDLE;

begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXInputClose' +
    #13#10'(' +
    #13#10'   hDevice             : $%.08X' +
    #13#10');',
    hDevice);

  POLLING_PARAMETERS_HANDLE * pph := (POLLING_PARAMETERS_HANDLE)hDevice;

    (* no longer necessary
    if (pph <> 0) then
    begin
        integer v;

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

    EmuSwapFS();   // XBox FS

    Exit;
end;    *)

// ******************************************************************
// * func: EmuXInputPoll
// ******************************************************************
(*DWORD WINAPI XTL.EmuXInputPoll
(
    IN THandle hDevice
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuXInputPoll' +
           #13#10'(' +
           #13#10'   hDevice             : $%.08X' +
           #13#10');',
           [hDevice);

    POLLING_PARAMETERS_HANDLE *pph := (POLLING_PARAMETERS_HANDLE)hDevice;

    //
    // Poll input
    //

    begin
        integer v;

        for(v:=0;v<XINPUT_SETSTATE_SLOTS;v++)
        begin
            THandle hDevice := g_pXInputSetStateStatus[v].hDevice;

            if (hDevice = 0) then
                continue;

            g_pXInputSetStateStatus[v].dwLatency := 0;

            XTL.PXINPUT_FEEDBACK pFeedback := (XTL.PXINPUT_FEEDBACK)g_pXInputSetStateStatus[v].pFeedback;

            if (pFeedback = 0) then
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

    EmuSwapFS();   // XBox FS

    Result := ERROR_SUCCESS;
end;       *)

// ******************************************************************
// * func: EmuXInputGetCapabilities
// ******************************************************************
(*DWORD WINAPI XTL.EmuXInputGetCapabilities
(
    IN  THandle               hDevice,
    OUT PXINPUT_CAPABILITIES pCapabilities
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuXInputGetCapabilities' +
           #13#10'(' +
           #13#10'   hDevice             : $%.08X' +
           #13#10'   pCapabilities       : $%.08X' +
           #13#10');',
           [hDevice, pCapabilities);

    DWORD ret := ERROR_INVALID_HANDLE;

    POLLING_PARAMETERS_HANDLE *pph := (POLLING_PARAMETERS_HANDLE)hDevice;

    if (pph <> 0) then
    begin
        DWORD dwPort := pph.dwPort;

        if ((dwPort >= 0) and (dwPort <= 3)) then
        begin
            pCapabilities.SubType := XINPUT_DEVSUBTYPE_GC_GAMEPAD;

            ZeroMemory(@pCapabilities.In.Gamepad, SizeOf(pCapabilities.In.Gamepad));

            ret := ERROR_SUCCESS;
         end;
     end;

    EmuSwapFS();   // XBox FS

    Result := ret;
end;        *)

// ******************************************************************
// * func: EmuInputGetState
// ******************************************************************
(*DWORD WINAPI XTL.EmuXInputGetState
(
    IN  THandle         hDevice,
    OUT PXINPUT_STATE  pState
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuXInputGetState' +
           #13#10'(' +
           #13#10'   hDevice             : $%.08X' +
           #13#10'   pState              : $%.08X' +
           #13#10');',
           [hDevice, pState);

    DWORD ret := ERROR_INVALID_HANDLE;

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

        DWORD dwPort := pph.dwPort;

        if ((dwPort >= 0) and (dwPort <= 3)) then
        begin
            if (dwPort = 0) then
            begin
                EmuDInputPoll(pState);
                ret := ERROR_SUCCESS;
             end;
         end;
     end;

    EmuSwapFS();   // XBox FS

    Result := ret;
end;            *)

// ******************************************************************
// * func: EmuInputGetState
// ******************************************************************
(*DWORD WINAPI XTL.EmuXInputSetState
(
    IN     THandle           hDevice,
    IN OUT PXINPUT_FEEDBACK pFeedback
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuXInputSetState' +
           #13#10'(' +
           #13#10'   hDevice             : $%.08X' +
           #13#10'   pFeedback           : $%.08X' +
           #13#10');',
           [hDevice, pFeedback);

    DWORD ret := ERROR_IO_PENDING;

    POLLING_PARAMETERS_HANDLE *pph := (POLLING_PARAMETERS_HANDLE)hDevice;

    if (pph <> 0) then
    begin
        integer v;

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

                    break;
                 end;
             end;

            if (v = XINPUT_SETSTATE_SLOTS) then
            begin
                CxbxKrnlCleanup('Ran out of XInputSetStateStatus slots!');
             end;
         end;
     end;

    EmuSwapFS();   // XBox FS

    Result := ret;
end;   *)

// ******************************************************************
// * func: EmuCreateMutex
// ******************************************************************
(*Function XTL_EmuCreateMutex (
    lpMutexAttributes : LPSECURITY_ATTRIBUTES;
    bInitialOwner : BOOL;
    lpName : PAnsiChar;
) : THandle
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuCreateMutex' +
           #13#10'(' +
           #13#10'   lpMutexAttributes   : $%.08X' +
           #13#10'   bInitialOwner       : $%.08X' +
           #13#10'   lpName              : $%.08X (%s)' +
           #13#10');',
           [lpMutexAttributes, bInitialOwner, lpName, lpName);

    THandle hRet := CreateMutex((SECURITY_ATTRIBUTES )lpMutexAttributes, bInitialOwner, lpName);

    EmuSwapFS();   // XBox FS

    Result := hRet;
end;          *)

// ******************************************************************
// * func: EmuCloseHandle
// ******************************************************************

function XTL_EmuCloseHandle(hObject: THandle): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuCloseHandle');
  DbgPrintf('(');
  DbgPrintf('   hObject             : $%.08X',[hObject]);
  DbgPrintf(');');

  bRet := CloseHandle(hObject);

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;

// ******************************************************************
// * func: EmuSetThreadPriorityBoost
// ******************************************************************

function XTL_EmuSetThreadPriorityBoost(hThread: THandle; DisablePriorityBoost: BOOL): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuSetThreadPriorityBoost');
  DbgPrintf('(');
  DbgPrintf('   hThread             : $%.08X', [hThread]);
  DbgPrintf('   DisablePriorityBoost: $%.08X', [DisablePriorityBoost]);
  DbgPrintf(');');

  bRet := SetThreadPriorityBoost(hThread, DisablePriorityBoost);

  if (bRet = False) then
    EmuWarning('SetThreadPriorityBoost Failed!');

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;

// ******************************************************************
// * func: EmuSetThreadPriority
// ******************************************************************

function XTL_EmuSetThreadPriority(hThread: THandle; nPriority: integer): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuSetThreadPriority');
  DbgPrintf('(');
  DbgPrintf('   hThread             : $%.08X', [hThread]);
  DbgPrintf('   nPriority           : $%.08X', [nPriority]);
  DbgPrintf(');');

  bRet := True; //SetThreadPriority(hThread, nPriority);

  if (bRet = False) then
    EmuWarning('SetThreadPriority Failed!');

    // HACK!
    //Sleep(10);

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;


// ******************************************************************
// * func: EmuGetThreadPriority
// ******************************************************************

function XTL_EmuGetThreadPriority(hThread: THandle): integer; stdcall;
var
  iRet: integer;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuGetThreadPriority');
  DbgPrintf('(');
  DbgPrintf('   hThread             : $%.08X', [hThread]);
  DbgPrintf(');');

  iRet := GetThreadPriority(hThread);

  if (iRet = THREAD_PRIORITY_ERROR_RETURN) then
    EmuWarning('GetThreadPriority Failed!');

  EmuSwapFS(); // XBox FS

  Result := iRet;
end;

// ******************************************************************
// * func: EmuGetExitCodeThread
// ******************************************************************

function XTL_EmuGetExitCodeThread(hThread: THandle; lpExitCode: Cardinal): BOOL; stdcall;
var
  bRet: BOOL;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuGetExitCodeThread');
  DbgPrintf('(');
  DbgPrintf('   hThread             : $%.08X', [hThread]);
  DbgPrintf('   lpExitCode          : $%.08X', [lpExitCode]);
  DbgPrintf(');');

  bRet := GetExitCodeThread(hThread, lpExitCode);

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;

// func: EmuXapiInitProcess

procedure XTL_EmuXapiInitProcess(); stdcall;
const
  HEAP_GROWABLE = $00000002;
var
  HeapParameters: RTL_HEAP_PARAMETERS;
  dwPeHeapReserve: UInt32;
  dwPeHeapCommit: UInt32;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXapiInitProcess();');

  // call RtlCreateHeap
  begin
    ZeroMemory(@HeapParameters, SizeOf(HeapParameters));

    HeapParameters.Length := SizeOf(HeapParameters);

    EmuSwapFS(); // XBox FS

    dwPeHeapReserve := CxbxKrnl_XbeHeader.dwPeHeapReserve;
    dwPeHeapCommit := CxbxKrnl_XbeHeader.dwPeHeapCommit;

//    PVOID dwResult := 0;

    XTL_EmuRtlCreateHeap(HEAP_GROWABLE, nil, dwPeHeapReserve, dwPeHeapCommit, nil, @HeapParameters);
  end;
end;

// ******************************************************************
// * data: EmuXapiProcessHeap
// ******************************************************************
(*PVOID* XTL.EmuXapiProcessHeap;*)

// ******************************************************************
// * func: g_pRtlCreateHeap
// ******************************************************************
(*XTL.pfRtlCreateHeap XTL.g_pRtlCreateHeap;*)

// ******************************************************************
// * func: EmuXapiThreadStartup
// ******************************************************************

procedure XTL_EmuXapiThreadStartup(dwDummy1, dwDummy2: DWORD) stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXapiThreadStartup');
  DbgPrintf('(');
  DbgPrintf('   dwDummy1            : 0x%.08X', [dwDummy1]);
  DbgPrintf('   dwDummy2            : 0x%.08X', [dwDummy2]);
  DbgPrintf(')');

  EmuSwapFS(); // XBox FS

  Function1ArgStdCall(dwDummy1)(dwDummy2);
(*
    type  integer (__stdcall *pfDummyFunc)(DWORD dwDummy);

    pfDummyFunc func := (pfDummyFunc)dwDummy1;

    func(dwDummy2);

    // TODO: Call thread notify routines ?

    ( *
    asm
    begin
        push dwDummy2
        call dwDummy1
     end;
    * )
*)
    //_asm int 3;
  Exit;
end;
(*

(* Too High Level!
// ******************************************************************
// * func: XapiSetupPerTitleDriveLetters
// ******************************************************************
XTL.NTSTATUS CDECL XTL.XapiSetupPerTitleDriveLetters(DWORD dwTitleId, PWideChar wszTitleName)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : XapiSetupPerTitleDriveLetters' +
           #13#10'(' +
           #13#10'   dwTitleId           : $%.08X' +
           #13#10'   wszTitleName        : $%.08X' +
           #13#10');',
            [dwTitleId, wszTitleName);

    NTSTATUS ret := STATUS_SUCCESS;

    EmuSwapFS();   // XBox FS

    Result := ret;
 end;
*)
// ******************************************************************
// * func: EmuXapiBootDash
// ******************************************************************

procedure XTL_EmuXapiBootDash(UnknownA: DWORD; UnknownB: DWORD; UnknownC: DWORD); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXapiBootDash');
  DbgPrintf('(');
  DbgPrintf('   UnknownA            : $%.08X', [UnknownA]);
  DbgPrintf('   UnknownB            : $%.08X', [UnknownB]);
  DbgPrintf('   UnknownC            : $%.08X', [UnknownC]);
  DbgPrintf(');');

  CxbxKrnlCleanup('Emulation Terminated (XapiBootDash)');

  EmuSwapFS(); // XBox FS

  Exit;
end;

// ******************************************************************
// * func: EmuXRegisterThreadNotifyRoutine
// ******************************************************************

procedure XTL_EmuXRegisterThreadNotifyRoutine(
  pThreadNotification: PXTHREAD_NOTIFICATION;
  fRegister: BOOL
  ); stdcall;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXRegisterThreadNotifyRoutine' +
    #13#10'(' +
    #13#10'   pThreadNotification : $%.08X ($%.08X)' +
    #13#10'   fRegister           : $%.08X' +
    #13#10');',
    [pThreadNotification, Addr(pThreadNotification.pfnNotifyRoutine), Integer(fRegister)]);

  if fRegister then
  begin
    if Assigned(g_pfnThreadNotification) then
      CxbxKrnlCleanup('Multiple thread notification routines installed (caustik can fix this not )');

    g_pfnThreadNotification := Addr(pThreadNotification.pfnNotifyRoutine); // TODO : Is this correct?
  end
  else
  begin
    if Assigned(g_pfnThreadNotification) then
      g_pfnThreadNotification := nil;
  end;

  EmuSwapFS(); // XBox FS
end;

// Cxbx : not necessary?
// ******************************************************************
// * func: EmuXCalculateSignatureBegin
// ******************************************************************
Function XTL_EmuXCalculateSignatureBegin ( dwFlags : DWORD ) : THandle; stdcall;
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : XCalculateSignatureBegin' +
           #13#10'(' +
           #13#10'   dwFlags             : $%.08X' +
           #13#10');',
            [dwFlags]);

    EmuSwapFS();   // XBox FS

    // return a fake handle value for now
    Result := $AAAAAAAA;
end;

// ******************************************************************
// * func: EmuXCalculateSignatureBeginEx
// ******************************************************************
Function XTL_EmuXCalculateSignatureBeginEx (dwFlags : DWORD; dwAltTitleId : DWORD) : THandle; stdcall;
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : XCalculateSignatureBeginEx' +
           #13#10'(' +
           #13#10'   dwFlags             : $%.08X' +
           #13#10'   dwAltTitleId        : $%.08X' +
           #13#10');',
            [dwFlags, dwAltTitleId]);

    EmuSwapFS();   // XBox FS

    // return a fake handle value for now
    Result := $AAAAAAAA;
end;

// ******************************************************************
// * func: EmuXCalculateSignatureUpdate
// ******************************************************************
Function XTL_EmuXCalculateSignatureUpdate ( hCalcSig : THandle; pbData : BYTE; cbData : ULONG ) : DWORD; stdcall;
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : XCalculateSignatureUpdate' +
           #13#10'(' +
           #13#10'   hCalcSig            : $%.08X' +
           #13#10'   pbData              : $%.08X' +
           #13#10'   cbData              : $%.08X' +
           #13#10');',
            [hCalcSig, pbData, cbData]);

    EmuSwapFS();   // XBox FS

    Result := ERROR_SUCCESS;
end;

// ******************************************************************
// * func: EmuXCalculateSignatureEnd
// ******************************************************************
(*Function XTL_EmuXCalculateSignatureEnd ( hCalcSig : THandle; pSignature : PXCALCSIG_SIGNATURE) : DWORD;
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : XCalculateSignatureEnd' +
           #13#10'(' +
           #13#10'   hCalcSig            : $%.08X', +
           #13#10'   pSignature          : $%.08X', +
           #13#10');',
            [hCalcSig, pSignature]);

    EmuSwapFS();   // XBox FS

    Result := ERROR_SUCCESS;
end; *)


end.

