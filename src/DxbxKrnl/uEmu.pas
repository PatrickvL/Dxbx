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

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows // THandle
  , Messages
  , SysUtils
  , Dialogs
  // Jedi Win32API
  , JwaWinBase
  , JwaWinType
  , JwaWinNt
  // DXBX
  , uEmuD3D8Types
  , uConsts
  , uTypes
  , uLog
  , uEmuFS
  , uDxbxKrnlUtils;

// exception handler
function EmuException(e: LPEXCEPTION_POINTERS): int; stdcall;

// check the allocation size of a given virtual address
function EmuCheckAllocationSize(pBase: PVOID; largeBound: _bool): int;

// print call stack trace
{$ifdef _DEBUG}
procedure EmuPrintStackTrace(ContextRecord: PCONTEXT);
{$endif}

// global flags specifying current emulation state
var g_bEmuException: _bool = False;
var g_bEmuSuspended: _bool = False;

// global exception patching address
var g_HaloHack: array [0..4-1] of uint32;

// Dead to Rights hack
var g_DeadToRightsHack: array [0..2-1] of uint32;

// global exception patching address
var funcExclude: array [0..2048-1] of uint32;

// partition emulation directory handles
var g_hCurDir: Handle = 0;
var g_strCurDrive: string = '';
var g_hTDrive: Handle = 0;
var g_strTDrive: string = '';
var g_hUDrive: Handle = 0;
var g_strUDrive: string = '';
var g_hZDrive: Handle = 0;
var g_strZDrive: string = '';
var g_hEmuWindow: Handle = 0; // rendering window

// thread notification routine
var g_pfnThreadNotification: array [0..16-1] of PVOID;
var g_iThreadNotificationCount: int;

// NOTE: this is an arbitrary latency
const XINPUT_SETSTATE_LATENCY = 4;
const XINPUT_SETSTATE_SLOTS = 16;

// XInputSetState status waiters
type XInputSetStateStatus = record
    hDevice: HANDLE;
    dwLatency: DWORD;
    pFeedback: PVOID;
  end; // size = 12

var g_pXInputSetStateStatus: array [0..XINPUT_SETSTATE_SLOTS - 1] of XInputSetStateStatus;

// 4 controllers
const XINPUT_HANDLE_SLOTS = 4;

var g_hInputHandle: array [0..XINPUT_HANDLE_SLOTS - 1] of HANDLE;

  g_bPrintfOn: Boolean = True;

procedure EmuWarning(szWarningMessage: string); overload;
procedure EmuWarning(szWarningMessage: string; const Args: array of const); overload;
procedure EmuCleanup(const szErrorMessage: string);
function ExitException(e: LPEXCEPTION_POINTERS): Integer;

implementation

// print out a warning message to the kernel debug log file
procedure EmuWarning(szWarningMessage: string);
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
begin
{$IFDEF DEBUG}
  if (g_bPrintfOn) then
  begin
    DbgPrintf('EmuWarn : ' + szWarningMessage);
  end;
{$ENDIF}
end;

procedure EmuWarning(szWarningMessage: string; const Args: array of const);
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
begin
  EmuWarning(DxbxFormat(szWarningMessage, Args, {MayRenderArguments=}True));
end;

// exception handler
function EmuException(E: LPEXCEPTION_POINTERS): int; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_tj  Done:30
var
  fix: UInt32;
  buffer: array [0..256 -1] of char;
  ret: int;
  dwESI: DWORD;
  dwSize: DWORD;
  v: DWORD;
  dwCur: DWORD;
  dwValue: DWORD;
  dwPtr: DWORD;
begin
  EmuSwapFS(fsWindows);

  g_bEmuException := true;

  // check for Halo hack
  begin
    if E.ExceptionRecord.ExceptionCode = $C0000005 then
    begin
      // Halo Access Adjust 1
      if E.ContextRecord.Eip = $0003394C then
      begin
        if E.ContextRecord.Ecx = $803BD800 then
        begin
          // Halo BINK skip
          begin
            // nop sled over bink calls
            (* Cxbx marked this out :
            memset(Pvoid($2CBA4), $90, $2CBAF - $2CBA4);
            memset(Pvoid($2CBBD), $90, $2CBD5 - $2CBBD);
            *)
            memset(Pvoid($2CAE0), $90, $2CE1E - $2CAE0);
          end;

          fix := g_HaloHack[1] + (e.ContextRecord.Eax - $803A6000);

          e.ContextRecord.Eax := fix; e.ContextRecord.Ecx := fix;
          Puint32(e.ContextRecord.Esp)^ := fix;

          PX_D3DResource(fix).Data := g_HaloHack[1] + (PX_D3DResource(fix).Data - $803A6000);

          // go through and fix any other pointers in the ESI allocation chunk
          begin
            dwESI := e.ContextRecord.Esi;
            dwSize := EmuCheckAllocationSize(PVOID(dwESI), False);

            // dword aligned
            Dec(dwSize, 4 - (dwSize mod 4));

            v := 0;
            while v < dwSize do
            begin
              dwCur := PDWORD(dwESI+v)^;

              if (dwCur >= $803A6000) and (dwCur < $819A6000) then
                  PDWORD(dwESI+v)^ := g_HaloHack[1] + (dwCur - $803A6000);

              Inc(v, 4);
            end;
          end;

          // fix this global pointer
          begin
            dwValue := PDWORD($39CE24)^;

            PDWORD($39CE24)^ := g_HaloHack[1] + (dwValue - $803A6000);
          end;

{$IFDEF DEBUG}
          DbgPrintf('EmuMain : Halo Access Adjust 1 was applied!');
{$ENDIF}

          g_bEmuException := false;

          Result := EXCEPTION_CONTINUE_EXECUTION;
          Exit;
        end;
      end
      // Halo Access Adjust 2
      else
        if E.ContextRecord.Eip = $00058D8C then
        begin
          if e.ContextRecord.Eax = $819A5818 then
          begin
            fix := g_HaloHack[1] + (e.ContextRecord.Eax - $803A6000);

            PDWORD($0039BE58)^ := fix; e.ContextRecord.Eax := fix;

            // go through and fix any other pointers in the $2DF1C8 allocation chunk
            begin
              dwPtr := PDWORD($2DF1C8)^;
              dwSize := EmuCheckAllocationSize(PVOID(dwPtr), false);

              // dword aligned
              dwSize := dwSize - (4 - dwSize mod 4);

              v := 0;
              while v < dwSize do
              begin
                dwCur := (dwPtr+v);

                if (dwCur >= $803A6000) and (dwCur < $819A6000) then
                  PDWORD(dwPtr+v)^ := g_HaloHack[1] + (dwCur - $803A6000);

                Inc(v, 4);
              end;
            end;

{$IFDEF DEBUG}
            DbgPrintf('EmuMain : Halo Access Adjust 2 was applied!');
{$ENDIF}
            g_bEmuException := false;

            Result := EXCEPTION_CONTINUE_EXECUTION;
            Exit;
          end;
        end;
    end; // if E.ExceptionRecord.ExceptionCode = $C0000005 then
  end;

  // print debug information
{$IFDEF DEBUG}
  begin
    if E.ExceptionRecord.ExceptionCode = $80000003 then
      DbgPrintf('Received Breakpoint Exception (int 3)')
    else
      DbgPrintf('Received Exception (Code := $%.08X)', [e.ExceptionRecord.ExceptionCode]);

    DbgPrintf(
      #13#10' EIP := $%.08X EFL := $%.08X' +
      #13#10' EAX := $%.08X EBX := $%.08X ECX := $%.08X EDX := $%.08X' +
      #13#10' ESI := $%.08X EDI := $%.08X ESP := $%.08X EBP := $%.08X' +
      #13#10, [
        e.ContextRecord.Eip, e.ContextRecord.EFlags,
        e.ContextRecord.Eax, e.ContextRecord.Ebx, e.ContextRecord.Ecx, e.ContextRecord.Edx,
        e.ContextRecord.Esi, e.ContextRecord.Edi, e.ContextRecord.Esp, e.ContextRecord.Ebp]);
  end;
{$ENDIF}

  fflush(stdout);

  // notify user
  begin

    if e.ExceptionRecord.ExceptionCode = $80000003 then
    begin
{$IFDEF DEBUG}
      DbgPrintf(
        'Received Breakpoint Exception (int 3) @ EIP := $%.08X'+
        ''+
        '  Press Abort to terminate emulation.'+
        '  Press Retry to debug.'+
        '  Press Ignore to continue emulation.',
        [e.ContextRecord.EFlags]);
{$ENDIF}
      Inc(e.ContextRecord.Eip);
      ret := MessageBox(g_hEmuWindow, buffer, 'Dxbx', MB_ICONSTOP or MB_ABORTRETRYIGNORE);
      if ret = IDABORT then
      begin
{$IFDEF DEBUG}
        DbgPrintf('EmuMain : Aborting Emulation');
{$ENDIF}
        fflush(stdout);

        if DxbxKrnl_hEmuParent <> 0 then
          SendMessage(DxbxKrnl_hEmuParent, WM_PARENTNOTIFY, WM_DESTROY, 0);

        ExitProcess(1);
      end
      else
        if ret = IDIGNORE then
        begin
{$IFDEF DEBUG}
          DbgPrintf('EmuMain : Ignored Breakpoint Exception');
{$ENDIF}

          g_bEmuException := False;

          Result := EXCEPTION_CONTINUE_EXECUTION;
          Exit;
        end;
    end
    else
    begin
{$IFDEF DEBUG}
      DbgPrintf(
              'Received Exception Code $%.08X @ EIP := $%.08X'+
              ''+
              '  Press ''OK'' to terminate emulation.'+
              '  Press ''Cancel'' to debug.',
              [e.ContextRecord.Eip, e.ContextRecord.EFlags]);
{$ENDIF}

      if MessageBox(g_hEmuWindow, buffer, 'Cxbx', MB_ICONSTOP or MB_OKCANCEL) = IDOK then
      begin
{$IFDEF DEBUG}
        DbgPrintf('EmuMain : Aborting Emulation');
{$ENDIF}
        fflush(stdout);

        if DxbxKrnl_hEmuParent <> 0 then
          SendMessage(DxbxKrnl_hEmuParent, WM_PARENTNOTIFY, WM_DESTROY, 0);

        ExitProcess(1);
      end;
    end;
  end;

  g_bEmuException := False;

  Result := EXCEPTION_CONTINUE_SEARCH;
end;

// check how many bytes were allocated for a structure
function EmuCheckAllocationSize(pBase: PVOID; largeBound: _bool): Integer;
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
var
  MemoryBasicInfo: MEMORY_BASIC_INFORMATION;

  dwRet: DWORD;
begin
{$IFDEF _DEBUG_ALLOC}
  dwRet := CxbxVirtualQueryDebug(pBase, MemoryBasicInfo, SizeOf(MemoryBasicInfo));
  if (dwRet = -1) then
{$ENDIF}
    dwRet := VirtualQuery(pBase, {var}MemoryBasicInfo, SizeOf(MemoryBasicInfo));

  if dwRet = 0 then
  begin
    Result := 0;
    Exit;
  end;

  if MemoryBasicInfo.State <> MEM_COMMIT then
  begin
    Result := 0;
    Exit;
  end;

  // this is a hack in order to determine when pointers come from a large write-combined database
  if largeBound and (MemoryBasicInfo.RegionSize > (5 * 1024 * 1024)) then
  begin
    Result := -1;
    Exit;
  end;

  Result := Integer(MemoryBasicInfo.RegionSize) - (IntPtr(pBase) - IntPtr(MemoryBasicInfo.BaseAddress));
end;

procedure EmuCleanup(const szErrorMessage: string);
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:70
var
  (*buffer: array [0..15] of Char;*)
  szBuffer1 : array [0..255] of char;
  szBuffer2 : array [0..255] of char;
  (*argp: va_list;*)
begin
  // Print out ErrorMessage (if exists)
  if (szErrorMessage <> '') then
  begin

{$IFDEF DEBUG}
    DbgPrintf(szBuffer1, 'Emu : Received Fatal Message - > '#13#10#13#10);
{$ENDIF}

(*    va_start(argp, szErrorMessage);

{$IFDEF DEBUG}
    DbgPrintf(szBuffer2, [szErrorMessage, argp]);
{$ENDIF}
    va_end(argp);*)

    strcat(szBuffer1, szBuffer2);


{$IFDEF DEBUG}
    DbgPrintf('%s'#13#10, [szBuffer1]);
{$ENDIF}
    szBuffer1 := 'Emu: Received Fatal Message - > ';(*  + szErrorMessage;*)
{$IFDEF DEBUG}
    DbgPrintf(szBuffer1);
{$ENDIF}

    MessageDlg(szBuffer1, mtError, [mbOk], 0);
  end;

{$IFDEF DEBUG}
  DbgPrintf('DxbxKrnl: Terminating Process');
{$ENDIF}


  //  Cleanup debug output
  FreeConsole();

  (*if (GetConsoleTitle(buffer, 16) <> '') then
    freopen('nul', 'w', stdout); *)

  TerminateProcess(GetCurrentProcess(), 0);
end;

// Exception handler for that tough final exit :)
function ExitException(e: LPEXCEPTION_POINTERS): int;
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
var
  count: int;
begin
  EmuSwapFS(fsWindows);

  count := 0;

  // debug information
{$IFDEF DEBUG}
  DbgPrintf('EmuMain : * * * * * EXCEPTION * * * * * ');
  DbgPrintf('EmuMain : Received Exception[$%.08x]@$%.08X', [ InttoStr(e.ExceptionRecord.ExceptionCode),
                                                             IntToStr(e.ContextRecord.Eip)]);
  DbgPrintf('EmuMain : * * * * * EXCEPTION * * * * * ');
{$ENDIF}

  MessageDlg('Warning: Could not safely terminate process!', mtWarning, [mbOk], 0);
  Inc(Count);

  if Count > 1 then
  begin
    MessageDlg('Warning: Multiple Problems!', mtWarning, [mbOk], 0);
    Result := EXCEPTION_CONTINUE_SEARCH;
    Exit;
  end;

  if (DxbxKrnl_hEmuParent <> 0) then
    SendMessage(DxbxKrnl_hEmuParent, WM_PARENTNOTIFY, WM_DESTROY, 0);

  ExitProcess(1);

  Result := EXCEPTION_CONTINUE_SEARCH;
end;

end.

