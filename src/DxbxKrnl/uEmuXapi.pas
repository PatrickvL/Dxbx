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
  // Dxbx
  uTypes,
  uLog, // DbgPrintf
  uEmuFS, // EmuSwapFS
  uXBController,
  uDxbxKrnlUtils; // CxbxKrnl_XbeHeader

type
  XTHREAD_NOTIFY_PROC = procedure(fCreate: BOOL); stdcall;

procedure XTL_EmuXapiInitProcess(); stdcall;

implementation

{ TODO : Need to be translated to delphi }

(*var
  // XInputSetState status waiters
  g_pXInputSetStateStatus: array[0..XINPUT_SETSTATE_SLOTS - 1] of XInputSetStateStatus;

  // XInputOpen handles
  g_hInputHandle: array[0..XINPUT_HANDLE_SLOTS - 1] of THandle;
*)


// func: EmuXapiApplyKernelPatches
procedure XTL__EmuXapiApplyKernelPatches();
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

function XTL__EmuXFormatUtilityDrive(): BOOL;
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

  DbgPrintf('EmuXapi : EmuFindFirstFileA'
    '('
    '   lpFileName          : $%.08X (%s)'
    '   lpFindFileData      : $%.08X'
    ');',
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

  DbgPrintf('EmuXapi : EmuFindNextFileA'
    '('
    '   hFindFile           : $%.08X'
    '   lpFindFileData      : $%.08X'
    ');',
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

// ******************************************************************
// * func: EmuRtlCreateHeap
// ******************************************************************
(*PVOID WINAPI XTL.EmuRtlCreateHeap
(
  in ULONG Flags,
  in PVOID Base OPTIONAL,
  in ULONG Reserve OPTIONAL,
  in ULONG Commit,
  in PVOID Lock OPTIONAL,
  in PVOID RtlHeapParams OPTIONAL
  )
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuRtlCreateHeap'
    '('
    '   Flags               : $%.08X'
    '   Base                : $%.08X'
    '   Reserve             : $%.08X'
    '   Commit              : $%.08X'
    '   Lock                : $%.08X'
    '   RtlHeapParams       : $%.08X'
    ');',
    [Flags, Base, Reserve, Commit, Lock, RtlHeapParams);

  NtDll.RTL_HEAP_DEFINITION RtlHeapDefinition;

  ZeroMemory(@RtlHeapDefinition, SizeOf(RtlHeapDefinition));

  RtlHeapDefinition.Length := SizeOf(RtlHeapDefinition);

  PVOID pRet := NtDll.RtlCreateHeap(Flags, Base, Reserve, Commit, Lock, @RtlHeapDefinition);

  EmuSwapFS(); // XBox FS

  Result := pRet;
end;     *)

// ******************************************************************
// * func: EmuRtlAllocateHeap
// ******************************************************************
(*PVOID WINAPI XTL.EmuRtlAllocateHeap
(
  in THandle hHeap,
  in DWORD dwFlags,
  in SIZE_T dwBytes
  )
begin
  EmuSwapFS(); // Win2k/XP FS

    //* too much debug output
  DbgPrintf('EmuXapi : EmuRtlAllocateHeap'
    '('
    '   hHeap               : $%.08X'
    '   dwFlags             : $%.08X'
    '   dwBytes             : $%.08X'
    ');',
    [hHeap, dwFlags, dwBytes);
    //*/

  BYTE offs;

  PVOID pRet := CxbxRtlAlloc(hHeap, dwFlags, dwBytes + $20);

  offs := (BYTE)(RoundUp((uint32)pRet, $20) - (uint32)pRet);

  if (offs = 0) then
  begin
    offs := $20;
  end;

  pRet := (PVOID)((uint32)pRet + offs);

  * (BYTE)((uint32)pRet - 1) := offs;

  DbgPrintf('pRet : $%.08X', pRet);

  EmuSwapFS(); // XBox FS

  Result := pRet;
end;      *)

// ******************************************************************
// * func: EmuRtlFreeHeap
// ******************************************************************
(*BOOL WINAPI XTL.EmuRtlFreeHeap
(
  in THandle hHeap,
  in DWORD dwFlags,
  in PVOID lpMem
  )
begin
  EmuSwapFS(); // Win2k/XP FS

    //* too much debug output
  DbgPrintf('EmuXapi : EmuRtlFreeHeap'
    '('
    '   hHeap               : $%.08X'
    '   dwFlags             : $%.08X'
    '   lpMem               : $%.08X'
    ');',
    [hHeap, dwFlags, lpMem);
    //*/

  if (lpMem <> 0) then
  begin
    BYTE offs := * (BYTE)((uint32)lpMem - 1);

    lpMem := (PVOID)((uint32)lpMem - offs);
  end;

  BOOL bRet := CxbxRtlFree(hHeap, dwFlags, lpMem);

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;      *)

// ******************************************************************
// * func: EmuRtlReAllocateHeap
// ******************************************************************
(*PVOID WINAPI XTL.EmuRtlReAllocateHeap
(
  in THandle hHeap,
  in DWORD dwFlags,
  in PVOID lpMem,
  in SIZE_T dwBytes
  )
begin
  EmuSwapFS(); // Win2k/XP FS

    //* too much debug output
  DbgPrintf('EmuXapi : EmuRtlReAllocateHeap'
    '('
    '   hHeap               : $%.08X'
    '   dwFlags             : $%.08X'
    '   lpMem               : $%.08X'
    '   dwBytes             : $%.08X'
    ');',
    [hHeap, dwFlags, lpMem, dwBytes);
   //*/

  if (lpMem <> 0) then
  begin
    BYTE offs := * (BYTE)((uint32)lpMem - 1);

    lpMem := (PVOID)((uint32)lpMem - offs);
  end;

  PVOID pRet := CxbxRtlRealloc(hHeap, dwFlags, lpMem, dwBytes + $20);

  EmuSwapFS(); // XBox FS

  Result := pRet;
end;      *)

// ******************************************************************
// * func: EmuRtlSizeHeap
// ******************************************************************
(*SIZE_T WINAPI XTL.EmuRtlSizeHeap
(
  in THandle hHeap,
  in DWORD dwFlags,
  in PVOID lpMem
  )
begin
  EmuSwapFS(); // Win2k/XP FS

    //* too much debug output
  DbgPrintf('EmuXapi : EmuRtlSizeHeap'
    '('
    '   hHeap               : $%.08X'
    '   dwFlags             : $%.08X'
    '   lpMem               : $%.08X'
    ');',
    [hHeap, dwFlags, lpMem);
    //*/

  if (lpMem <> 0) then
  begin
    BYTE offs := * (BYTE)((uint32)lpMem - 1);

    lpMem := (PVOID)((uint32)lpMem - offs);
  end;

  SIZE_T ret := CxbxRtlSizeHeap(hHeap, dwFlags, lpMem) - $20;

  EmuSwapFS(); // XBox FS

  Result := ret;
end;            *)

// ******************************************************************
// * func: EmuQueryPerformanceCounter
// ******************************************************************
(*BOOL WINAPI XTL.EmuQueryPerformanceCounter
(
  PLARGE_INTEGER lpPerformanceCount
  )
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuQueryPerformanceCounter'
    '('
    '   lpPerformanceCount  : $%.08X'
    ');',
    [lpPerformanceCount);

  BOOL bRet := QueryPerformanceCounter(lpPerformanceCount);

    // debug - 4x speed
    //lpPerformanceCount->QuadPart *= 4;

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;        *)

// ******************************************************************
// * func: EmuQueryPerformanceFrequency
// ******************************************************************
(*BOOL WINAPI XTL.EmuQueryPerformanceFrequency
(
  PLARGE_INTEGER lpFrequency
  )
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuQueryPerformanceFrequency'
    '('
    '   lpFrequency         : $%.08X'
    ');',
    [lpFrequency);

  BOOL bRet := QueryPerformanceFrequency(lpFrequency);

  EmuSwapFS(); // XBox FS

  Result := bRet;
end;            *)

// ******************************************************************
// * func: EmuXMountUtilityDrive
// ******************************************************************
(*BOOL WINAPI XTL.EmuXMountUtilityDrive
(
  BOOL fFormatClean
  )
begin
{$IFDEF _DEBUG_TRACE}
  begin
    EmuSwapFS(); // Win2k/XP FS
    DbgPrintf('EmuXapi : EmuXMountUtilityDrive'
      '('
      '   fFormatClean        : $%.08X'
      ');',
      [fFormatClean);
    EmuSwapFS(); // XBox FS
  end;
{$ENDIF}

  Result := True;
end;             *)

// ******************************************************************
// * func: EmuXInitDevices
// ******************************************************************
(*VOID WINAPI XTL.EmuXInitDevices
(
  DWORD Unknown1,
  PVOID Unknown2
  )
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXInitDevices'
    '('
    '   Unknown1            : $%.08X'
    '   Unknown2            : $%.08X'
    ');',
    [Unknown1, Unknown2);

  integer v;

  for (v := 0; v < XINPUT_SETSTATE_SLOTS; v + +)
  begin
    g_pXInputSetStateStatus[v].hDevice := 0;
    g_pXInputSetStateStatus[v].dwLatency := 0;
    g_pXInputSetStateStatus[v].pFeedback := 0;
  end;

  for (v := 0; v < XINPUT_HANDLE_SLOTS; v + +)
  begin
    g_hInputHandle[v] := 0;
  end;

  EmuSwapFS(); // XBox FS

  Exit;
end;            *)

// ******************************************************************
// * func: EmuXGetDevices
// ******************************************************************
(*DWORD WINAPI XTL.EmuXGetDevices
(
  PXPP_DEVICE_TYPE DeviceType
  )
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXGetDevices'
    '('
    '   DeviceType          : $%.08X'
    ');',
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

  DbgPrintf('EmuXapi : EmuXGetDeviceChanges'
    '('
    '   DeviceType          : $%.08X'
    '   pdwInsertions       : $%.08X'
    '   pdwRemovals         : $%.08X'
    ');',
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
(*THandle WINAPI XTL.EmuXInputOpen
(
  in PXPP_DEVICE_TYPE DeviceType,
  in DWORD dwPort,
  in DWORD dwSlot,
  in PXINPUT_POLLING_PARAMETERS pPollingParameters OPTIONAL
  )
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXInputOpen'
    '('
    '   DeviceType          : $%.08X'
    '   dwPort              : $%.08X'
    '   dwSlot              : $%.08X'
    '   pPollingParameters  : $%.08X'
    ');',
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
    else
    begin
      pph.pPollingParameters := 0;
    end;

    g_hInputHandle[dwPort] := pph;
  end;
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
    end;
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
end;                  *)

// ******************************************************************
// * func: EmuXInputClose
// ******************************************************************
(*VOID WINAPI XTL.EmuXInputClose
(
  in THandle hDevice
  )
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXInputClose'
    '('
    '   hDevice             : $%.08X'
    ');',
    [hDevice);

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

    DbgPrintf('EmuXapi : EmuXInputPoll'
           '('
           '   hDevice             : $%.08X'
           ');',
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

    DbgPrintf('EmuXapi : EmuXInputGetCapabilities'
           '('
           '   hDevice             : $%.08X'
           '   pCapabilities       : $%.08X'
           ');',
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

    DbgPrintf('EmuXapi : EmuXInputGetState'
           '('
           '   hDevice             : $%.08X'
           '   pState              : $%.08X'
           ');',
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

    DbgPrintf('EmuXapi : EmuXInputSetState'
           '('
           '   hDevice             : $%.08X'
           '   pFeedback           : $%.08X'
           ');',
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
(*THandle WINAPI XTL.EmuCreateMutex
(
    LPSECURITY_ATTRIBUTES   lpMutexAttributes,
    BOOL                    bInitialOwner,
    PAnsiChar                  lpName
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuCreateMutex'
           '('
           '   lpMutexAttributes   : $%.08X'
           '   bInitialOwner       : $%.08X'
           '   lpName              : $%.08X (%s)'
           ');',
           [lpMutexAttributes, bInitialOwner, lpName, lpName);

    THandle hRet := CreateMutex((SECURITY_ATTRIBUTES )lpMutexAttributes, bInitialOwner, lpName);

    EmuSwapFS();   // XBox FS

    Result := hRet;
 end;          *)

// ******************************************************************
// * func: EmuCloseHandle
// ******************************************************************
(*BOOL WINAPI XTL.EmuCloseHandle
(
    THandle hObject
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuCloseHandle'
           '('
           '   hObject             : $%.08X'
           ');',
           [hObject);

    BOOL bRet := CloseHandle(hObject);

    EmuSwapFS();   // XBox FS

    Result := bRet;
 end;          *)

// ******************************************************************
// * func: EmuSetThreadPriorityBoost
// ******************************************************************
(*BOOL WINAPI XTL.EmuSetThreadPriorityBoost
(
    THandle  hThread,
    BOOL    DisablePriorityBoost
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuSetThreadPriorityBoost'
           '('
           '   hThread             : $%.08X'
           '   DisablePriorityBoost: $%.08X'
           ');',
           [hThread, DisablePriorityBoost);

    BOOL bRet := SetThreadPriorityBoost(hThread, DisablePriorityBoost);

    if (bRet = False) then
        EmuWarning('SetThreadPriorityBoost Failed!');

    EmuSwapFS();   // XBox FS

    Result := bRet;
 end;       *)

// ******************************************************************
// * func: EmuSetThreadPriority
// ******************************************************************
(*BOOL WINAPI XTL.EmuSetThreadPriority
(
    THandle  hThread,
    integer     nPriority
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuSetThreadPriority'
           '('
           '   hThread             : $%.08X'
           '   nPriority           : $%.08X'
           ');',
           [hThread, nPriority);

    BOOL bRet := True;//SetThreadPriority(hThread, nPriority);

    if (bRet = False) then
        EmuWarning('SetThreadPriority Failed!');

    // HACK!
    //Sleep(10);

    EmuSwapFS();   // XBox FS

    Result := bRet;
 end;        *)


// ******************************************************************
// * func: EmuGetThreadPriority
// ******************************************************************
(*integer WINAPI XTL.EmuGetThreadPriority
(
    THandle  hThread
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuGetThreadPriority'
           '('
           '   hThread             : $%.08X'
           ');',
           [hThread);

    integer iRet := GetThreadPriority(hThread);

    if (iRet = THREAD_PRIORITY_ERROR_RETURN) then
        EmuWarning('GetThreadPriority Failed!');

    EmuSwapFS();   // XBox FS

    Result := iRet;
 end;              *)

// ******************************************************************
// * func: EmuGetExitCodeThread
// ******************************************************************
(*BOOL WINAPI XTL.EmuGetExitCodeThread
(
    THandle  hThread,
    LPDWORD lpExitCode
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuGetExitCodeThread'
           '('
           '   hThread             : $%.08X'
           '   lpExitCode          : $%.08X'
           ');',
           [hThread, lpExitCode);

    BOOL bRet := GetExitCodeThread(hThread, lpExitCode);

    EmuSwapFS();   // XBox FS

    Result := bRet;
 end;          *)

type
  RTL_HEAP_PARAMETERS = packed record
    Length: UInt32;
    // TODO!
  end;
// ******************************************************************
// * func: EmuXapiInitProcess
// ******************************************************************
procedure XTL_EmuXapiInitProcess(); stdcall;
const
  HEAP_GROWABLE = $00000002;
var
  HeapParameters: RTL_HEAP_PARAMETERS;
  dwPeHeapReserve: UInt32;
  dwPeHeapCommit: UInt32;
begin
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXapiInitProcess();');

  // call RtlCreateHeap
  begin
    ZeroMemory(@HeapParameters, SizeOf(HeapParameters));

    HeapParameters.Length := SizeOf(HeapParameters);

    EmuSwapFS();   // XBox FS

    dwPeHeapReserve := CxbxKrnl_XbeHeader.dwPeHeapReserve;
    dwPeHeapCommit  := CxbxKrnl_XbeHeader.dwPeHeapCommit;

//    PVOID dwResult := 0;

// TODO    *XTL.EmuXapiProcessHeap := XTL.g_pRtlCreateHeap(HEAP_GROWABLE, 0, dwPeHeapReserve, dwPeHeapCommit, 0, @HeapParameters);
  end;

  Exit;
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
(*VOID WINAPI XTL.EmuXapiThreadStartup
(
    DWORD dwDummy1,
    DWORD dwDummy2
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : EmuXapiThreadStartup'
           '('
           '   dwDummy1            : $%.08X'
           '   dwDummy2            : $%.08X'
           ');',
            [dwDummy1, dwDummy2);

    EmuSwapFS();   // XBox FS

    type  integer (__stdcall *pfDummyFunc)(DWORD dwDummy);

    pfDummyFunc func := (pfDummyFunc)dwDummy1;

    func(dwDummy2);

    // TODO: Call thread notify routines ?

    (*
    asm
    begin
        push dwDummy2
        call dwDummy1
     end;
    *)

    //_asm int 3;
(*
  Exit;
end; *)

(* Too High Level!
// ******************************************************************
// * func: XapiSetupPerTitleDriveLetters
// ******************************************************************
XTL.NTSTATUS CDECL XTL.XapiSetupPerTitleDriveLetters(DWORD dwTitleId, PWideChar wszTitleName)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : XapiSetupPerTitleDriveLetters'
           '('
           '   dwTitleId           : $%.08X'
           '   wszTitleName        : $%.08X'
           ');',
            [dwTitleId, wszTitleName);

    NTSTATUS ret := STATUS_SUCCESS;

    EmuSwapFS();   // XBox FS

    Result := ret;
 end;
*)
// ******************************************************************
// * func: EmuXapiBootDash
// ******************************************************************
(*VOID WINAPI XTL.EmuXapiBootDash(DWORD UnknownA, DWORD UnknownB, DWORD UnknownC)
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXapiBootDash'
    '('
    '   UnknownA            : $%.08X'
    '   UnknownB            : $%.08X'
    '   UnknownC            : $%.08X'
    ');',
    [UnknownA, UnknownB, UnknownC);

  CxbxKrnlCleanup('Emulation Terminated (XapiBootDash)');

  EmuSwapFS(); // XBox FS

  Exit;
end;           *)

// ******************************************************************
// * func: EmuXRegisterThreadNotifyRoutine
// ******************************************************************
(*VOID WINAPI XTL.EmuXRegisterThreadNotifyRoutine
(
  PXTHREAD_NOTIFICATION pThreadNotification,
  BOOL fRegister
  )
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXRegisterThreadNotifyRoutine'
    '('
    '   pThreadNotification : $%.08X ($%.08X)'
    '   fRegister           : $%.08X'
    ');',
    [pThreadNotification, pThreadNotification.pfnNotifyRoutine, fRegister);

  if fRegister then
  begin
    if g_pfnThreadNotification <> 0 then
      CxbxKrnlCleanup('Multiple thread notification routines installed (caustik can fix this not )');

    g_pfnThreadNotification := pThreadNotification.pfnNotifyRoutine;
  end;
else
  begin
    if (g_pfnThreadNotification <> 0) then
      g_pfnThreadNotification := 0;
  end;

  EmuSwapFS(); // XBox FS
end;           *)

(* Cxbx : not necessary?
// ******************************************************************
// * func: EmuXCalculateSignatureBegin
// ******************************************************************
THandle WINAPI XTL.EmuXCalculateSignatureBegin
(
    DWORD dwFlags
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : XCalculateSignatureBegin'
           '('
           '   dwFlags             : $%.08X'
           ');',
            [dwFlags);

    EmuSwapFS();   // XBox FS

    // return a fake handle value for now
    Result := (PVOID)$AAAAAAAA;
 end;

// ******************************************************************
// * func: EmuXCalculateSignatureBeginEx
// ******************************************************************
THandle WINAPI XTL.EmuXCalculateSignatureBeginEx
(
    DWORD dwFlags,
    DWORD dwAltTitleId
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : XCalculateSignatureBeginEx'
           '('
           '   dwFlags             : $%.08X',
           '   dwAltTitleId        : $%.08X',
           ');',
            [dwFlags, dwAltTitleId);

    EmuSwapFS();   // XBox FS

    // return a fake handle value for now
    Result := PVOID($AAAAAAAA);
 end;

// ******************************************************************
// * func: EmuXCalculateSignatureUpdate
// ******************************************************************
DWORD WINAPI XTL.EmuXCalculateSignatureUpdate
(
  THandle        hCalcSig,
   BYTE    *pbData,
  ULONG         cbData
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : XCalculateSignatureUpdate'
           '('
           '   hCalcSig            : $%.08X',
           '   pbData              : $%.08X',
           '   cbData              : $%.08X',
           ');',
            [hCalcSig, pbData, cbData);

    EmuSwapFS();   // XBox FS

    Result := ERROR_SUCCESS;
 end;

// ******************************************************************
// * func: EmuXCalculateSignatureEnd
// ******************************************************************
DWORD WINAPI XTL.EmuXCalculateSignatureEnd
(
  THandle                hCalcSig,
  PXCALCSIG_SIGNATURE   pSignature
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi : XCalculateSignatureEnd'
           '('
           '   hCalcSig            : $%.08X',
           '   pSignature          : $%.08X',
           ');',
            [hCalcSig, pSignature]);

    EmuSwapFS();   // XBox FS

    Result := ERROR_SUCCESS;
 end;
*)

end.

