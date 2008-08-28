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
  Windows, // THandle
  // Jedi WinAPI
  JwaWinType;

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
  g_pXInputSetStateStatus: array [0..XINPUT_SETSTATE_SLOTS-1] of XInputSetStateStatus;

const
  // 4 controllers
  XINPUT_HANDLE_SLOTS = 4;

var
  g_hInputHandle: array [0..XINPUT_HANDLE_SLOTS-1] of HANDLE;

implementation



// print out a warning message to the kernel debug log file

procedure EmuWarning(szWarningMessage: string);
begin
(*

  szBuffer1: array[0..255 - 1] of Char;
  szBuffer2: array[0..255 - 1] of Char;

  va_list argp;

  StrFmt(szBuffer1, 'EmuWarn ($ mod X): ", GetCurrentThreadId());

    va_start(argp, szWarningMessage);

    StrFmt(szBuffer2, szWarningMessage, argp);

    va_end(argp);

    StrCat(szBuffer1, szBuffer2);

    if (g_bPrintfOn) then
    begin
      printf(" mod s", szBuffer1);
    end;

    fflush(stdout);

    Exit;  *)
end;
//endif

// exception handler

(*function EmuException(e: LPEXCEPTION_POINTERS): integer;
begin
  if (EmuIsXboxFS()) then
    EmuSwapFS();

  g_bEmuException := true;

    // check for Halo hack
(*    begin
        if(e^.ExceptionRecord^.ExceptionCode = $C0000005) then
        begin
            // Halo Access Adjust 1
            if(e^.ContextRecord^.Eip = $0003394C) then
            begin
                if(e^.ContextRecord^.Ecx = $803BD800) then
                begin
                    // Halo BINK skip
                    begin
                        // nop sled over bink calls
                        (*
                        FillChar($2CBA4, $90, $2CBAF - $2CBA4);
                        FillChar($2CBBD, $90, $2CBD5 - $2CBBD);
                        //*/
                        FillChar($2CAE0, $90, $2CE1E - $2CAE0);
                     end;

                    uint32 fix := g_HaloHack[1] + (e^.ContextRecord^.Eax - $803A6000);

                    e^.ContextRecord^.Eax := e^.ContextRecord^.Ecx = fix;

                    *(uint32)e^.ContextRecord^.Esp := fix;

                    ((XTL.X_D3DResource)fix)^.Data := g_HaloHack[1] + (((XTL.X_D3DResource)fix)^.Data - $803A6000);

                    // go through and fix any other pointers in the ESI allocation chunk
                    begin
                        DWORD dwESI := e^.ContextRecord^.Esi;
                        DWORD dwSize := EmuCheckAllocationSize((PVOID)dwESI, False);

                        // dword aligned
                        dwSize:= dwSize - 4 - dwSize mod 4;

                        for(DWORD v:=0;v<dwSize;v+=4)
                        begin
                            DWORD dwCur := *(DWORD)(dwESI+v);

                            if(dwCur >= $803A6000 and dwCur < $819A6000) then
                                *(DWORD)(dwESI+v) := g_HaloHack[1] + (dwCur - $803A6000);
                         end;
                     end;

                    // fix this global pointer
                    begin
                        DWORD dwValue := *(DWORD)$39CE24;

                        *(DWORD)$39CE24 := g_HaloHack[1] + (dwValue - $803A6000);
                     end;

                    DbgPrintf("EmuMain ($ mod X): Halo Access Adjust 1 was applied not ", GetCurrentThreadId());

                    g_bEmuException := False;

                    result:= EXCEPTION_CONTINUE_EXECUTION;
                 end;
             end;
            // Halo Access Adjust 2
            else if(e^.ContextRecord^.Eip = $00058D8C) then
            begin
                if(e^.ContextRecord^.Eax = $819A5818) then
                begin
                    uint32 fix := g_HaloHack[1] + (e^.ContextRecord^.Eax - $803A6000);

                    *(DWORD)$0039BE58 := e^.ContextRecord^.Eax = fix;

                    // go through and fix any other pointers in the 0x2DF1C8 allocation chunk
                    begin
                        DWORD dwPtr := *(DWORD)$2DF1C8;
                        DWORD dwSize := EmuCheckAllocationSize((PVOID)dwPtr, False);

                        // dword aligned
                        dwSize:= dwSize - 4 - dwSize mod 4;

                        for(DWORD v:=0;v<dwSize;v+=4)
                        begin
                            DWORD dwCur := *(DWORD)(dwPtr+v);

                            if(dwCur >= $803A6000 and dwCur < $819A6000) then
                                *(DWORD)(dwPtr+v) := g_HaloHack[1] + (dwCur - $803A6000);
                         end;
                     end;

                    DbgPrintf("EmuMain ($ mod X): Halo Access Adjust 2 was applied not ", GetCurrentThreadId());

                    g_bEmuException := False;

                    result:= EXCEPTION_CONTINUE_EXECUTION;
                 end;
             end;
         end;
     end;

    // check for Battlestar Galactica hack *PAL Version*
    begin
        if(e^.ExceptionRecord^.ExceptionCode = $C0000096) then
        begin
            // Battlestar Galactica Hack 1
            if(e^.ContextRecord^.Eip = $000CB580) then
            begin
                //if(e->ContextRecord->Ecx == 0x00000200 || e->ContextRecord->Ecx == 0x00000100)
                //{
                    // Battlestar Galactica WBINVD skip
     e^.ContextRecord^.Eip:= e^.ContextRecord^.Eip + 2;

                    DbgPrintf("EmuMain ($ mod X): Battlestar Galactica Hack 1 was applied not ", GetCurrentThreadId());

                    g_bEmuException := False;

                    result:= EXCEPTION_CONTINUE_EXECUTION;
                //}
             end;
         end;
     end;

    // print debug information
    begin
        if(e^.ExceptionRecord^.ExceptionCode = $80000003) then
            printf("EmuMain ($ mod X): Recieved Breakpoint Exception (integer 3)", GetCurrentThreadId());
        else
            printf("EmuMain ($ mod X): Recieved Exception (Code := $ mod .08X)", GetCurrentThreadId(), e^.ExceptionRecord^.ExceptionCode);

        printf(""
            " EIP := $ mod .08X EFL := $ mod .08X"
            " EAX := $ mod .08X EBX := $ mod .08X ECX := $ mod .08X EDX := $ mod .08X"
            " ESI := $ mod .08X EDI := $ mod .08X ESP := $ mod .08X EBP := $ mod .08X"
            "",
            e^.ContextRecord^.Eip, e^.ContextRecord^.EFlags,
            e^.ContextRecord^.Eax, e^.ContextRecord^.Ebx, e^.ContextRecord^.Ecx, e^.ContextRecord^.Edx,
            e^.ContextRecord^.Esi, e^.ContextRecord^.Edi, e^.ContextRecord^.Esp, e^.ContextRecord^.Ebp);
     end;

    fflush(stdout);

    // notify user
    begin
         buffer: array[0..256-1] of Char;

        if(e^.ExceptionRecord^.ExceptionCode = $80000003) then
        begin
            StrFmt(buffer,
                "Recieved Breakpoint Exception (integer 3) @ EIP := $ mod .08X"
                ""
                "  Press Abort to terminate emulation."
                "  Press Retry to debug."
                "  Press Ignore to continue emulation.",
                e^.ContextRecord^.Eip, e^.ContextRecord^.EFlags);

            e->ContextRecord->Eip:= e->ContextRecord->Eip + 1;

            integer ret := MessageBox(g_hEmuWindow, buffer, "Cxbx", MB_ICONSTOP or MB_ABORTRETRYIGNORE);

            if(ret = IDABORT) then
            begin
                printf("EmuMain ($ mod X): Aborting Emulation", GetCurrentThreadId());
                fflush(stdout);

                if(CxbxKrnl_hEmuParent <> 0) then
                    SendMessage(CxbxKrnl_hEmuParent, WM_PARENTNOTIFY, WM_DESTROY, 0);

                ExitProcess(1);
             end;
            else if(ret = IDIGNORE) then
            begin
                printf("EmuMain ($ mod X): Ignored Breakpoint Exception", GetCurrentThreadId());

                g_bEmuException := False;

                result:= EXCEPTION_CONTINUE_EXECUTION;
             end;
         end;
        else
        begin
            StrFmt(buffer,
                "Recieved Exception Code $ mod .08X @ EIP := $ mod .08X"
                ""
                "  Press "OK" to terminate emulation."
                "  Press "Cancel" to debug.",
                e->ExceptionRecord->ExceptionCode, e->ContextRecord->Eip, e->ContextRecord->EFlags);

            if(MessageBox(g_hEmuWindow, buffer, "Cxbx", MB_ICONSTOP or MB_OKCANCEL) = IDOK) then
            begin
                printf("EmuMain ($ mod X): Aborting Emulation", GetCurrentThreadId());
                fflush(stdout);

                if(CxbxKrnl_hEmuParent <> 0) then
                    SendMessage(CxbxKrnl_hEmuParent, WM_PARENTNOTIFY, WM_DESTROY, 0);

                ExitProcess(1);
             end;
         end;
     end;

    g_bEmuException := False;

  result := EXCEPTION_CONTINUE_SEARCH;
end; *)

// check how many bytes were allocated for a structure
(* function EmuCheckAllocationSize(pBase: Pointer; largeBound: bool): integer;
begin
    MEMORY_BASIC_INFORMATION MemoryBasicInfo;

    DWORD dwRet;
#ifdef _DEBUG_ALLOC
    dwRet := CxbxVirtualQueryDebug(pBase, @MemoryBasicInfo, SizeOf(MemoryBasicInfo));
    if (dwRet = -1) then
//endif
    dwRet := VirtualQuery(pBase, @MemoryBasicInfo, SizeOf(MemoryBasicInfo));

    if(dwRet = 0) then
        result:= 0;

    if(MemoryBasicInfo.State <> MEM_COMMIT) then
        result:= 0;

    // this is a hack in order to determine when pointers come from a large write-combined database
    if(largeBound and MemoryBasicInfo.RegionSize > 5*1024*1024) then
        result:= -1;

    result:= MemoryBasicInfo.RegionSize - ((DWORD)pBase - (DWORD)MemoryBasicInfo.BaseAddress);
 end;   *)

// exception handle for that tough final exit :)
(*function ExitException(e: LPEXCEPTION_POINTERS): integer;
begin
    if(EmuIsXboxFS()) then
        EmuSwapFS();

     integer count := 0;

    // debug information
    printf("EmuMain ($ mod X): * * * * * EXCEPTION * * * * *", GetCurrentThreadId());
    printf("EmuMain ($ mod X): Recieved Exception [$ mod .08X]@$ mod .08X", GetCurrentThreadId(), e->ExceptionRecord->ExceptionCode, e->ContextRecord->Eip);
    printf("EmuMain ($ mod X): * * * * * EXCEPTION * * * * *", GetCurrentThreadId());

    fflush(stdout);

    MessageBox(g_hEmuWindow, "Warning: Could not safely terminate process not ", "Cxbx", MB_OK);

    count:= count + 1;

    if(count > 1) then
    begin
        MessageBox(g_hEmuWindow, "Warning: Multiple Problems not ", "Cxbx", MB_OK);
        result:= EXCEPTION_CONTINUE_SEARCH;
     end;

    if(CxbxKrnl_hEmuParent <> 0) then
        SendMessage(CxbxKrnl_hEmuParent, WM_PARENTNOTIFY, WM_DESTROY, 0);

    ExitProcess(1);

    result:= EXCEPTION_CONTINUE_SEARCH;
 end;      *)



end.

