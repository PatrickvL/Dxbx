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
unit uDxbxKrnlUtils;

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  SysUtils,
  // Dxbx
  uConsts,
  uLog, // for WriteLog
  uXBE; // PXBE_TLS

procedure CxbxKrnlCleanup(const szErrorMessage: string);

function iif(aTest: Boolean; const aTrue: Integer; const aFalse: Integer): Integer; overload;

function GetLastErrorString: string;
function GetErrorString(const aError: DWord): string;

function GetDWordBits(const Bits: DWORD; const aIndex: Integer): Integer;
procedure SetDWordBits(var Bits: DWORD; const aIndex: Integer; const aValue: Integer);

var
  // ! thread local storage
  CxbxKrnl_TLS: PXBE_TLS;
  // thread local storage data
  CxbxKrnl_TLSData: PVOID;
  // xbe header structure
  CxbxKrnl_XbeHeader: PXBE_HEADER;
  // parent window handle
  CxbxKrnl_hEmuParent: HWND;

  // thread handles
  g_hThreads: array[0..MAXIMUM_XBOX_THREADS - 1] of THandle;

implementation

procedure CxbxKrnlCleanup(const szErrorMessage: string);
var
  szBuffer1: string;
//  buffer: array [0..15] of char;
begin
  // Print out ErrorMessage (if exists)
  if szErrorMessage <> '' then
  begin
    szBuffer1 := {Format} 'CxbxKrnlCleanup : Received Fatal Message ->'#13#13 + szErrorMessage;
    DbgPrintf(szBuffer1);
    MessageBox(0, @(szBuffer1[1]), 'DxbxKrnl', MB_OK or MB_ICONEXCLAMATION);
  end;

  DbgPrintf('DxbxKrnl: Terminating Process');
//  FFlush(stdout);

  // Cleanup debug output
  CloseLogs(); // FreeConsole();

       (* if(GetConsoleTitle(buffer, 16) <> '')
            freopen('nul', 'w', stdout); *)

  TerminateProcess(GetCurrentProcess(), 0);
end;

function iif(aTest: Boolean; const aTrue: Integer; const aFalse: Integer): Integer; overload;
begin
  if aTest then
    Result := aTrue
  else
    Result := aFalse;
end;

// Tooling methods to get and set stretches of bits inside a DWORD,
// which is used to simulate C-like bit-fields in Delphi.  
// See http://stackoverflow.com/questions/282019/how-to-simulate-bit-fields-in-delphi-records#282385
// Registers:               EAX                EDX               EAX
function GetDWordBits(const Bits: DWORD; const aIndex: Integer): Integer;
{$IFDEF PURE_PASCAL}
begin
  Result := (Bits shr {Offset=}(aIndex shr 8))
        and {Mask =}((1 shl {NrBits=}Byte(aIndex)) - 1);
end;
{$ELSE}
asm
  push ebx
  mov ebx, $00000001 // EBX = 1
  mov cl, dl         // CL = NrBits
  shl ebx, cl        // EBX = (1 shl NrBits)
  mov cl, dh         // CL = Offset
  dec ebx            // EBX = (1 shl NrBits) - 1 // = Mask
  shr eax, cl        // EAX = Bits shr Offset
  and eax, ebx       // EAX = (Bits shr Offset) and Mask
  pop ebx
end;
{$ENDIF}

// Registers:              EAX                EDX                    ECX
procedure SetDWordBits(var Bits: DWORD; const aIndex: Integer; const aValue: Integer);
{$IFDEF PURE_PASCAL}
var
  Offset: Byte;
  Mask: Integer;
begin
  Mask := ((1 shl {NrBits=}Byte(aIndex)) - 1);
  Assert(aValue <= Mask);

  Offset := aIndex shr 8;
  {var}Bits := (Bits and (not (Mask shl Offset))) or DWORD(aValue shl Offset);
end;
{$ELSE}
asm
  push ebx
  push ecx
  mov ebx, $00000001 // EBX = 1
  mov cl, dl         // CL = NrBits
  shl ebx, cl        // EBX = (1 shl NrBits)
  mov cl, dh         // CL = Offset
  dec ebx            // EBX = (1 shl NrBits) - 1 // = Mask
  shl ebx, cl        // EBX = Mask shl Offset
  not ebx            // EBX = not Mask
  and ebx,[eax]      // EBX = Bits and Mask // = MaskedBits
  pop edx            // EDX = aValue
  shl edx, cl        // EDX = aValue shl Offset // = NewBits
  or  edx, ebx       // EDX = MaskedBits or NewBits
  mov [eax], edx     // {var}Bits = EDX
  pop ebx
end;
{$ENDIF}

function GetLastErrorString: string;
begin
  Result := GetErrorString(GetLastError);
end;

function GetErrorString(const aError: DWord): string;
begin
  Result := SysErrorMessage(aError);
  if Result = '' then
    Result := 'No description for error #' + IntToStr(aError)
  else
    Result := Result + ' (#' + IntToStr(aError) + ')';
end;

end.
