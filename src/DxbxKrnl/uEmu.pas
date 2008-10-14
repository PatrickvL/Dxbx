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
unit uEmu;

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  SysUtils
  , Dialogs
  , Windows // THandle
  // Jedi WinAPI
  , JwaWinBase
  , JwaWinType
  // DXBX
  , uLog
  , uEmuFS;

var
  g_hCurDir: THandle = 0;
  g_strCurDrive: string = '';
  g_hTDrive: THandle = 0;
  g_strTDrive: string = '';
  g_hUDrive: THandle = 0;
  g_strUDrive: string = '';
  g_hZDrive: THandle = 0;
  g_strZDrive: string = '';
  g_hEmuWindow: THandle;
  g_bPrintfOn: Boolean = True;
  g_bEmuSuspended: Boolean = False;
  g_bEmuException: Boolean = False;

procedure EmuWarning(szWarningMessage: string);
procedure EmuCleanup(const szErrorMessage: string);

const
  // NOTE: this is an arbitrary latency
  XINPUT_SETSTATE_LATENCY = 4;
  XINPUT_SETSTATE_SLOTS = 16;

type
  // XInputSetState status waiters
  XInputSetStateStatus = record
    hDevice: HANDLE;
    dwLatency: DWORD;
    pFeedback: PVOID;
  end;

var
  g_pXInputSetStateStatus: array[0..XINPUT_SETSTATE_SLOTS - 1] of XInputSetStateStatus;

const
  // 4 controllers
  XINPUT_HANDLE_SLOTS = 4;

var
  g_hInputHandle: array[0..XINPUT_HANDLE_SLOTS - 1] of HANDLE;

implementation



// print out a warning message to the kernel debug log file

procedure EmuWarning(szWarningMessage: string);
var
  szBuffer1: string;
  szBuffer2: string;
//  va_list : argp;
begin
   (*szBuffer1 := Format ( 'EmuWarn ($ mod X): ', [GetCurrentThreadId] );

    va_start(argp, szWarningMessage);

    StrFmt(szBuffer2, szWarningMessage, argp);

    va_end(argp);

    StrCat(szBuffer1, szBuffer2);
    *)

  if (g_bPrintfOn) then
  begin
    DbgPrintf(szWarningMessage);
  end;

    (*fflush(stdout); *)
end;

// check how many bytes were allocated for a structure

function EmuCheckAllocationSize(pBase: Pointer; largeBound: bool): integer;
var
  MemoryBasicInfo : MEMORY_BASIC_INFORMATION;
  dwRet : DWORD;
begin
  (*
{$IFDEF _DEBUG_ALLOC}
  dwRet := CxbxVirtualQueryDebug(pBase, MemoryBasicInfo, SizeOf(MemoryBasicInfo));
  if (dwRet = -1) then
{$ENDIF}
    dwRet := VirtualQuery(pBase, MemoryBasicInfo, SizeOf(MemoryBasicInfo));

  if (dwRet = 0) then
    result := 0;

  if (MemoryBasicInfo.State <> MEM_COMMIT) then
    result := 0;

    // this is a hack in order to determine when pointers come from a large write-combined database
  if (largeBound and MemoryBasicInfo.RegionSize > 5 * 1024 * 1024) then
    result := -1;

  result := MemoryBasicInfo.RegionSize - (pBase - MemoryBasicInfo.BaseAddress);
  *)
end;


// func: EmuCleanup
procedure EmuCleanup(const szErrorMessage: string);
var
  szBuffer1 : String;
  buffer : Array [0..15] of Char;
begin
    // Print out ErrorMessage (if exists)
  if (szErrorMessage <> '') then begin
    (*char szBuffer1[255];
    char szBuffer2[255];

    va_list argp;

    sprintf(szBuffer1, "Emu(0 x%X): Recieved Fatal Message - > \n\n", GetCurrentThreadId());

    va_start(argp, szErrorMessage);

    vsprintf(szBuffer2, szErrorMessage, argp);

    va_end(argp);

    strcat(szBuffer1, szBuffer2);


    printf("%s\n", szBuffer1);    *)
    szBuffer1 := Format('Emu(0 $%X): Recieved Fatal Message - > '  + szErrorMessage, [GetCurrentThreadId] );
    DbgPrintf ( szBuffer1 );

    MessageDlg( szBuffer1, mtError, [mbOk], 0 );
  end;

  DbgPrintf('DxbxKrnl: Terminating Process');
  (*fflush(stdout); *)

  //  Cleanup debug output
  FreeConsole();

  (*if (GetConsoleTitle(buffer, 16) <> '' ) then
    freopen("nul", "w", stdout); *)

  TerminateProcess(GetCurrentProcess(), 0);
end;


// exception handle for that tough final exit :)

function ExitException(e: LPEXCEPTION_POINTERS): integer;
var
  count : integer;
begin
  if (EmuIsXboxFS()) then
    EmuSwapFS();

  count := 0;

  // debug information
  DbgPrintf('EmuMain($ mod X): * * * * * EXCEPTION * * * * * ', GetCurrentThreadId());
  (*
  DbgPrintf(Format('EmuMain($ mod X): Recieved Exception[$ mod .08 X]@$ mod .08 X', GetCurrentThreadId(), [InttoStr (e.ExceptionRecord.ExceptionCode), IntToStr(e.ContextRecord.Eip)]));
  *)
  DbgPrintf('EmuMain($ mod X): * * * * * EXCEPTION * * * * * ', GetCurrentThreadId());

  (*fflush(stdout);*)

  MessageDlg( 'Warning: Could not safely terminate process not ', mtWarning, [mbOk], 0 );
  Inc ( Count );

  (*if (count > 1) then
  begin
    MessageDlg( 'Warning: Multiple Problems not ', mtWarning, [mbOk], 0 );
    result := EXCEPTION_CONTINUE_SEARCH;
  end;

  if (CxbxKrnl_hEmuParent <> 0) then
    SendMessage(CxbxKrnl_hEmuParent, WM_PARENTNOTIFY, WM_DESTROY, 0);

  ExitProcess(1);

  result := EXCEPTION_CONTINUE_SEARCH; *)
end;



end.

