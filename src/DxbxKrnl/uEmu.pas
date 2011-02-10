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
{.$define GAME_HACKS_ENABLED}

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
  , uConsts
  , uTypes
  , uLog
  , uDxbxKrnlUtils
  , uEmuFS
  , uEmuD3D8Types;

// exception handler
function EmuException(ExceptionInfo: LPEXCEPTION_POINTERS): int; stdcall;

// print call stack trace
{$ifdef _DEBUG}
procedure EmuPrintStackTrace(ContextRecord: PCONTEXT);

var dbgCritical: CRITICAL_SECTION;
{$endif}

// global flags specifying current emulation state
var g_bEmuException: _bool = false;
var g_bEmuSuspended: _bool = false;

// global exception patching address
var g_HaloHack: array [0..4-1] of uint32; // = {0};

// Dead to Rights hack
//var g_DeadToRightsHack: array [0..2-1] of uint32; // = {0};

// global exception patching address
var funcExclude: array [0..2048-1] of uint32;

// partition emulation directory handles
var g_hCurDir: HANDLE = 0;
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
//function ExitException(e: LPEXCEPTION_POINTERS): Integer;
function HandlePrivilegedInstruction(E: PEXCEPTION_RECORD; C: PCONTEXT): Boolean; // forward
function HandleAccessViolation(E: PEXCEPTION_RECORD; C: PCONTEXT): Boolean;
function HandleBreakpoint(E: PEXCEPTION_RECORD; C: PCONTEXT): Boolean;
procedure DumpException(E: PEXCEPTION_RECORD; C: PCONTEXT);

const
  // From Windows.pas :
  STATUS_BREAKPOINT               = DWORD($80000003);
  //  From System.MapToRunError:
  STATUS_ACCESS_VIOLATION         = $C0000005;
  STATUS_ARRAY_BOUNDS_EXCEEDED    = $C000008C;
  STATUS_FLOAT_DENORMAL_OPERAND   = $C000008D;
  STATUS_FLOAT_DIVIDE_BY_ZERO     = $C000008E;
  STATUS_FLOAT_INEXACT_RESULT     = $C000008F;
  STATUS_FLOAT_INVALID_OPERATION  = $C0000090;
  STATUS_FLOAT_OVERFLOW           = $C0000091;
  STATUS_FLOAT_STACK_CHECK        = $C0000092;
  STATUS_FLOAT_UNDERFLOW          = $C0000093;
  STATUS_INTEGER_DIVIDE_BY_ZERO   = $C0000094;
  STATUS_INTEGER_OVERFLOW         = $C0000095;
  STATUS_PRIVILEGED_INSTRUCTION   = $C0000096;
  STATUS_STACK_OVERFLOW           = $C00000FD;
  STATUS_CONTROL_C_EXIT           = $C000013A;

implementation

uses
  uMiniport, // g_NV2ADMAChannel
  uEmuKrnlMM;

// print out a warning message to the kernel debug log file
procedure EmuWarning(szWarningMessage: string);
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
begin
{$IFDEF DEBUG}
  DbgPrintf('EmuWarn : ' + szWarningMessage);
{$ENDIF}
end;

procedure EmuWarning(szWarningMessage: string; const Args: array of const);
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
begin
  EmuWarning(DxbxFormat(szWarningMessage, Args, {MayRenderArguments=}True));
end;

// exception handler
function EmuException(ExceptionInfo: LPEXCEPTION_POINTERS): int; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_tj  Done:30
var
  E: PEXCEPTION_RECORD;
  C: PCONTEXT;
begin
  // PLEASE DO NOT DO EMUSWAPFS HERE
  // HOW LESS HOW BETTER
  // Windows 7 is crashing on the EmuSwapFS and Logging

  Result := EXCEPTION_CONTINUE_SEARCH;
  C := ExceptionInfo.ContextRecord;

  // See if the instruction pointer is outside the Xbe region :
  // TODO : Xbe Upperbound should be calculated better :
  if (UIntPtr(C.Eip) < XBE_IMAGE_BASE) or (UIntPtr(C.Eip) > XBE_IMAGE_BASE + (32*1024*1024)) then
    // Let the Delphi debugger handle this exception (as it did not occur inside Xbe-related code) :
    Exit;

  g_bEmuException := true;

  E := ExceptionInfo.ExceptionRecord;

// W7 fix : No logging, to prevent kernel calls (via memory allocation and/or I/O) :
//  DbgPrintf('EmuMain : EmuException() with code 0x%.08x (%s) triggered at address 0x%.08x', [
//    E.ExceptionCode,
//    NTStatusToString(E.ExceptionCode),
//    UIntPtr(C.Eip)]);

  case E.ExceptionCode of
    // STATUS_ILLEGAL_INSTRUCTION ?
    STATUS_PRIVILEGED_INSTRUCTION:
      if HandlePrivilegedInstruction(E, C) then
      begin
        DbgPrintf('EmuMain : Handled privileged instruction!');
        Result := EXCEPTION_CONTINUE_EXECUTION;
      end;

    STATUS_ACCESS_VIOLATION:
      if HandleAccessViolation(E, C) then
      begin
        DbgPrintf('EmuMain : Handled access violation!');
        Result := EXCEPTION_CONTINUE_EXECUTION;
      end;

    STATUS_BREAKPOINT:
      if HandleBreakpoint(E, C) then
      begin
        Result := EXCEPTION_CONTINUE_EXECUTION;
      end;
  end; // case E.ExceptionCode

  if Result <> EXCEPTION_CONTINUE_EXECUTION then
  begin
    DumpException(E, C);

    fflush(stdout);
  end;

  g_bEmuException := false;
end; // EmuException

function HandlePrivilegedInstruction(E: PEXCEPTION_RECORD; C: PCONTEXT): Boolean;
begin
  Result := False;

  // Dxbx addition : Generic WBINVD skip :
  // See if the instruction pointer is a WBINVD opcode :
  if  (PBytes(E.ExceptionAddress)[0] = $0F)
  and (PBytes(E.ExceptionAddress)[1] = $09) then
  begin
    // Skip it, and continue :
    Inc(C.Eip, 2);

    Result := True;
    Exit;
  end;

  // TODO : Add other illegal opcodes here

  DbgPrintf('EmuMain : Unhandled opcode!?!');
end;

function HandleAccessViolation(E: PEXCEPTION_RECORD; C: PCONTEXT): Boolean;
begin
  Result := True; // assume we can fix this exception

  // Fix writes to $80000000 and above (as happens in '?Init@CDevice@D3D@@QAEJPAU_D3DPRESENT_PARAMETERS_@@@Z')  :
  if  (E.ExceptionInformation[1] >= $80000000)
  and (E.NumberParameters = 2) then
  begin
    // Decode the opcode to see if we can fix it :
    case PBytes(E.ExceptionAddress)[0] of
      $89: begin
        case PBytes(E.ExceptionAddress)[1] of
          $11: begin
            // From AlphaFog :
            // 00011F87 89 11 mov dword ptr [ecx],edx
            C.ECX := C.ECX and $7FFFFFFF; // Remove the high-bit from the destination address
            Exit;
          end;

          $15: begin
            if (E.ExceptionInformation[1] = $80000000) then
            begin
              // From CreateDevice :
              // 0001AD5C 89 1500000080 mov dword ptr [$80000000],edx
              g_NV2ADMAChannel.Put := PDWORD(DWORD(C.Edx) and (not 3)); // Trap the initial GPU jump (and mask off the jump-indicator)
              Inc(C.Eip, 6);
              Exit;
            end;
          end;

          $2B: begin
            // From Lights :
            // 00011136 89 2B mov dword ptr [ebx],ebp
            C.EBX := C.EBX and $7FFFFFFF; // Remove the high-bit from the destination address
            Exit;
          end;

          $32: begin
            // From AlphaFog :
            // 00012003 89 32 mov dword ptr [edx],esi
            C.EDX := C.EDX and $7FFFFFFF; // Remove the high-bit from the destination address
            Exit;
          end;

          $48, $68: begin
            // From AlphaFog :
            // 00012066 89 4828 mov dword ptr [eax+$28],ecx
            // 00013340 89 6804 mov dword ptr [eax+$04],ebp
            C.EAX := C.EAX and $7FFFFFFF; // Remove the high-bit from the destination address
            Exit;
          end;

        end;
      end;

      $A3: begin
        begin
          // From Wings of War :
          // 0011E20F A3 00000080 mov dword ptr [$80000000],eax
          Inc(C.Eip, 5);
          Exit;
        end;
      end;

      $C7: begin
        case PBytes(E.ExceptionAddress)[1] of
          $05: begin
            // From CreateDevice :
            // 0001ADA4 C7 0500000080EFBEADDE mov dword ptr [$80000000],$DEADBEEF
            Inc(C.Eip, 10);
            Exit;
          end;

          $40: begin
            if (PBytes(E.ExceptionAddress)[2] = $FC) then
            begin
              // From Textures :
              // 000112AB C740FC FFFFFFFF mov dword ptr [eax-$04],$FFFFFFFF
              C.EAX := C.EAX and $7FFFFFFF; // Remove the high-bit from the destination address
              Exit;
            end;
          end;
        end;
      end;

      $F3: begin
        // From Vertices :
        // 00011145 F3 A5 rep movsd
        if (PBytes(E.ExceptionAddress)[1] = $A5) then
        begin
          C.EDI := C.EDI and $7FFFFFFF; // Remove the high-bit from the destination address
          Exit;
        end;
      end;
    end;
  end;

  Result := False; // Alas, no fix
end;

procedure DumpException(E: PEXCEPTION_RECORD; C: PCONTEXT);
{$ifdef _DEBUG}
var
  Context: JwaWinNT.CONTEXT;
{$endif}
begin
  // print debug information
  DbgPrintf(
    #13#10' EIP := $%.08X EFL := $%.08X' +
    #13#10' EAX := $%.08X EBX := $%.08X ECX := $%.08X EDX := $%.08X' +
    #13#10' ESI := $%.08X EDI := $%.08X ESP := $%.08X EBP := $%.08X' +
    #13#10, [
      C.Eip, C.EFlags,
      C.Eax, C.Ebx, C.Ecx, C.Edx,
      C.Esi, C.Edi, C.Esp, C.Ebp]);

{$ifdef _DEBUG}
   Context := (C)^;
    // TODO -oDxbx : Once our detected symbols are used inside JclDebug we can also print EmuPrintStackTrace(C) here
   EmuPrintStackTrace(@Context);
{$endif}
(*
    buffer := Format(
            'Received Exception Code $%.08X @ EIP := $%.08X' +
            '' +
            '  Press ''OK'' to terminate emulation.' +
            '  Press ''Cancel'' to debug.',
            [E.ExceptionCode, C.Eip]);

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
*)
end;

function HandleBreakpoint(E: PEXCEPTION_RECORD; C: PCONTEXT): Boolean;
var
  buffer: string;
begin
  Result := False;

  // notify user
  buffer := Format(
    'Received Breakpoint Exception (int 3) @ EIP := $%.08X' +
    '' +
    '  Press Abort to terminate emulation.' +
    '  Press Retry to debug.' +
    '  Press Ignore to continue emulation.',
    [C.Eip]);

  Inc(C.Eip);

  case MessageBox(g_hEmuWindow, PChar(buffer), 'Dxbx', MB_ICONSTOP or MB_ABORTRETRYIGNORE) of

    IDABORT:
    begin
      DbgPrintf('EmuMain : Aborting Emulation');
      fflush(stdout);

      if DxbxKrnl_hEmuParent <> 0 then
        SendMessage(DxbxKrnl_hEmuParent, WM_USER_PARENTNOTIFY, WM_DESTROY, 0);

      ExitProcess(1);
    end;

    IDIGNORE:
    begin
      DbgPrintf('EmuMain : Ignored Breakpoint Exception');

      Result := True;
    end;
  end;
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

{$IFDEF DXBX_TRACE_MEMLEAKS}
  ExitProcess(0);
{$ELSE}
  TerminateProcess(GetCurrentProcess(), 0);
{$ENDIF}
end;

(*
// Exception handler for that tough final exit :)
function ExitException(ExceptionInfo: LPEXCEPTION_POINTERS): int;
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
var
  E: PEXCEPTION_RECORD;
  C: PCONTEXT;
  count: int;
begin
  EmuSwapFS(fsWindows);

  E := ExceptionInfo.ExceptionRecord;
  C := ExceptionInfo.ContextRecord;

  count := 0;

  // debug information
{$IFDEF DEBUG}
  DbgPrintf('EmuMain : * * * * * EXCEPTION * * * * * ');
  DbgPrintf('EmuMain : Received Exception[$%.08x]@$%.08X', [ InttoStr(E.ExceptionCode),
                                                             IntToStr(C.Eip)]);
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
*)

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
