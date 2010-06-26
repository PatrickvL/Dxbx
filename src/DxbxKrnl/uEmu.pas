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

{.$define _DEBUG}

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
{$ifdef _DEBUG}
  // JCL
  , JclDebug
  // DXBX
  , uDxbxDebugUtils
{$endif}
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

var dbgCritical: CRITICAL_SECTION;
{$endif}

// global flags specifying current emulation state
var g_bEmuException: _bool = false;
var g_bEmuSuspended: _bool = false;
var g_bPrintfOn: _boolean = true;

// global exception patching address
var g_HaloHack: array [0..4-1] of uint32; // = {0};

// Dead to Rights hack
var g_DeadToRightsHack: array [0..2-1] of uint32; // = {0};

// global exception patching address
var funcExclude: array [0..2048-1] of uint32;

// partition emulation directory handles
var g_hCurDir: HANDLE = 0;
var g_strCurDrive: string = '';
var g_hTDrive: HANDLE = 0;
var g_strTDrive: string = '';
var g_hUDrive: HANDLE = 0;
var g_strUDrive: string = '';
var g_hZDrive: HANDLE = 0;
var g_strZDrive: string = '';
var g_hEmuWindow: HWND = 0; // rendering window

// thread notification routine
//var g_pfnThreadNotification: array [0..16-1] of PVOID; // Dxbx : Already declared in uEmuPS.pas
//var g_iThreadNotificationCount: int; // Dxbx : Already declared in uEmuPS.pas

// NOTE: this is an arbitrary latency
const XINPUT_SETSTATE_LATENCY = 4;
const XINPUT_SETSTATE_SLOTS = 16;

// XInputSetState status waiters
type XInputSetStateStatus = record
    hDevice: HANDLE;
    dwLatency: DWORD;
    pFeedback: PVOID;
end; // size = 12
//var g_pXInputSetStateStatus: array [0..XINPUT_SETSTATE_SLOTS - 1] of XInputSetStateStatus; // Dxbx : Already declared in uEmuXapi.pas

// 4 controllers
const XINPUT_HANDLE_SLOTS = 4;

//var g_hInputHandle: array [0..XINPUT_HANDLE_SLOTS - 1] of HANDLE; // Dxbx : Already declared in uEmuXapi.pas

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
  buffer: string;
  ret: int;
  dwESI: DWORD;
  dwSize: DWORD;
  v: DWORD;
  dwCur: DWORD;
  dwValue: DWORD;
  dwPtr: DWORD;
  Context: JwaWinNT.CONTEXT;
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
            memset(Pvoid($2CBA4), $90, $2CBAF - $2CBA4); // OPCODE_NOP
            memset(Pvoid($2CBBD), $90, $2CBD5 - $2CBBD); // OPCODE_NOP
            *)
            memset(Pvoid($2CAE0), $90, $2CE1E - $2CAE0); // OPCODE_NOP
          end;

          fix := g_HaloHack[1] + (e.ContextRecord.Eax - $803A6000);

          e.ContextRecord.Eax := fix; e.ContextRecord.Ecx := fix;
          Puint32(e.ContextRecord.Esp)^ := fix;

          PX_D3DResource(fix).Data := g_HaloHack[1] + (PX_D3DResource(fix).Data - $803A6000);

          // go through and fix any other pointers in the ESI allocation chunk
          begin
            dwESI := e.ContextRecord.Esi;
            dwSize := EmuCheckAllocationSize(PVOID(dwESI), false);

            // dword aligned
            Dec(dwSize, 4 - (dwSize mod 4));

            v := 0; while v < dwSize do
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
              Dec(dwSize, 4 - dwSize mod 4);

              v := 0; while v < dwSize do
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

{$ifdef _DEBUG}
     Context := (e.ContextRecord)^;
     EmuPrintStackTrace(@Context);
{$endif}
  end;
{$ENDIF}

  fflush(stdout);

  // notify user
  begin

    if e.ExceptionRecord.ExceptionCode = $80000003 then
    begin
      buffer := Format(
        'Received Breakpoint Exception (int 3) @ EIP := $%.08X'+
        ''+
        '  Press Abort to terminate emulation.'+
        '  Press Retry to debug.'+
        '  Press Ignore to continue emulation.',
        [e.ContextRecord.Eip]);

      Inc(e.ContextRecord.Eip);
      ret := MessageBox(g_hEmuWindow, PChar(buffer), 'Dxbx', MB_ICONSTOP or MB_ABORTRETRYIGNORE);
      if ret = IDABORT then
      begin
{$IFDEF DEBUG}
        DbgPrintf('EmuMain : Aborting Emulation');
{$ENDIF}
        fflush(stdout);

        if DxbxKrnl_hEmuParent <> 0 then
          SendMessage(DxbxKrnl_hEmuParent, WM_USER_PARENTNOTIFY, WM_DESTROY, 0);

        ExitProcess(1);
      end
      else
        if ret = IDIGNORE then
        begin
{$IFDEF DEBUG}
          DbgPrintf('EmuMain : Ignored Breakpoint Exception');
{$ENDIF}

          g_bEmuException := false;

          Result := EXCEPTION_CONTINUE_EXECUTION;
          Exit;
        end;
    end
    else
    begin
      buffer := Format(
              'Received Exception Code $%.08X @ EIP := $%.08X'+
              ''+
              '  Press ''OK'' to terminate emulation.'+
              '  Press ''Cancel'' to debug.',
              [e.ExceptionRecord.ExceptionCode, e.ContextRecord.Eip]);

      if MessageBox(g_hEmuWindow, PChar(buffer), 'Dxbx', MB_ICONSTOP or MB_OKCANCEL) = IDOK then
      begin
{$IFDEF DEBUG}
        DbgPrintf('EmuMain : Aborting Emulation');
{$ENDIF}
        fflush(stdout);

        if DxbxKrnl_hEmuParent <> 0 then
          SendMessage(DxbxKrnl_hEmuParent, WM_USER_PARENTNOTIFY, WM_DESTROY, 0);

        ExitProcess(1);
      end;
    end;
  end;

  g_bEmuException := false;

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
  dwRet := DxbxVirtualQueryDebug(pBase, MemoryBasicInfo, SizeOf(MemoryBasicInfo));
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
    SendMessage(DxbxKrnl_hEmuParent, WM_USER_PARENTNOTIFY, WM_DESTROY, 0);

  ExitProcess(1);

  Result := EXCEPTION_CONTINUE_SEARCH;
end;

{$ifdef _DEBUG}
// print call stack trace
procedure EmuPrintStackTrace(ContextRecord: PCONTEXT);
(*
const STACK_MAX     = 16;
const SYMBOL_MAXLEN = 64;
*)
var
  i: int;
(*
  module: IMAGEHLP_MODULE64;
  fSymInitialized: BOOL;
  frame: STACKFRAME64;
  dwDisplacement: DWORD64;
  pSymbol: PSYMBOL_INFO;
  symbol: array [0..sizeof(SYMBOL_INFO) + SYMBOL_MAXLEN-1] of BYTE;
*)
  Info: TJclLocationInfo;
begin

  EnterCriticalSection(dbgCritical);

  with JclCreateStackList({Raw=}True, {AIgnoreLevels=}0, {FirstCaller=}Pointer(ContextRecord.Eip)) do
  try
    for I := 0 to Count - 1 do
      if GetLocationInfo(Items[I].CallerAddr, {var}Info) then
        DbgPrintf(' %2d: %s', [i, LocationInfoToString(Info)])
      else
        DbgPrintf(' %2d: 0x%.08x', [i, Items[I].CallerAddr])
  finally
    Free;
  end;
(*
  IMAGEHLP_MODULE64 module = { sizeof(IMAGEHLP_MODULE) };

  fSymInitialized := SymInitialize(GetCurrentProcess(), NULL, TRUE);

  STACKFRAME64 frame := { sizeof(STACKFRAME64) };
  frame.AddrPC.Offset    := ContextRecord.Eip;
  frame.AddrPC.Mode      := AddrModeFlat;
  frame.AddrFrame.Offset := ContextRecord.Ebp;
  frame.AddrFrame.Mode   := AddrModeFlat;
  frame.AddrStack.Offset := ContextRecord.Esp;
  frame.AddrStack.Mode   := AddrModeFlat;

  for i = 0 to STACK_MAX-1 do
  begin
    if(not StackWalk64(
            IMAGE_FILE_MACHINE_I386,
            GetCurrentProcess(),
            GetCurrentThread(),
            @frame,
            ContextRecord,
            NULL,
            SymFunctionTableAccess64,
            SymGetModuleBase64,
            NULL)) then
            break;

    DWORD64 dwDisplacement := 0;
    PSYMBOL_INFO pSymbol := 0;
    BYTE symbol[sizeof(SYMBOL_INFO) + SYMBOL_MAXLEN];

    SymGetModuleInfo64(GetCurrentProcess(), frame.AddrPC.Offset, &module);

    if(fSymInitialized) then
    begin
      pSymbol := PSYMBOL_INFO(symbol);
      pSymbol.SizeOfStruct := sizeof(SYMBOL_INFO) + SYMBOL_MAXLEN - 1;
      pSymbol.MaxNameLen := SYMBOL_MAXLEN;

      if(not SymFromAddr(GetCurrentProcess(), frame.AddrPC.Offset, @dwDisplacement, pSymbol)) then
        pSymbol := 0;
    end;

    if(module.ModuleName) then
      printf(' %2d: %-8s 0x%.08X', [i, module.ModuleName, frame.AddrPC.Offset])
    else
      printf(' %2d: %8c 0x%.08X', [i, ' ', frame.AddrPC.Offset]);

    if(pSymbol) then
    begin
      printf(' %s+0x%.04X\n', [pSymbol.Name, dwDisplacement]);
    end
    else
      printf('\n');
  end;
  printf('\n');

  if(fSymInitialized) then
    SymCleanup(GetCurrentProcess());
*)
  LeaveCriticalSection(dbgCritical);
end;
{$endif}

end.
