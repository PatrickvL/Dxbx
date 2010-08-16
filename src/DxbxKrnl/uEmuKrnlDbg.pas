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
unit uEmuKrnlDbg;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  JwaWinBase,
  JwaWinNT,
  JwaNative,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uDxbxUtils,
  uLog,
  uEmuFS,
  uEmuKrnl,
  uDxbxKrnl;

procedure {005} xboxkrnl_DbgBreakPoint(
  ); stdcall;
procedure {006} xboxkrnl_DbgBreakPointWithStatus(
  Status: ULONG
  ); stdcall;
function {007} xboxkrnl_DbgLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ): NTSTATUS; stdcall;
function {008} xboxkrnl_DbgPrint(
  Format: PCCH
  ): ULONG; cdecl; // varargs;
function {010} xboxkrnl_DbgPrompt(
  Prompt: PCCH;
  Response: PCH; // OUT
  MaximumResponseLength: ULONG
  ): ULONG; stdcall;
procedure {011} xboxkrnl_DbgUnLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ); stdcall;

implementation

procedure {005} xboxkrnl_DbgBreakPoint(
  ); stdcall;
// Source:JwaNative.pas  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('DbgBreakPoint');
  EmuSwapFS(fsXbox);
end;

procedure {006} xboxkrnl_DbgBreakPointWithStatus(
  Status: ULONG
  ); stdcall;
// Source:JwaNative.pas  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('DbgBreakPointWithStatus');
  EmuSwapFS(fsXbox);
end;

function {007} xboxkrnl_DbgLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ): NTSTATUS; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('DbgLoadImageSymbols');
  EmuSwapFS(fsXbox);
end;

// Dxbx note : In C, this function uses varargs ('...'), which Delphi doesn't
// support directly (we have array of const, but those are put on the stack
// as TVarRec's, which is quite different from C's varargs).
// Luckily, there's still a way to get to these arguments using RVarArgsReader!
function {008} xboxkrnl_DbgPrint(
  Format: PCCH
  ): ULONG; cdecl; // varargs;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:75
var
  va: RVarArgsReader;
  cp: PAnsiChar;
  ResultStr: AnsiString;
begin
  EmuSwapFS(fsWindows);

  // Parse the varargs :
  va.Create(@Format, SizeOf(Format));

  cp := PAnsiChar(Format);
  ResultStr := '';
  while True do
  begin
    case cp^ of
      #0: Break;
      '%':
      begin
        // Skip the '%' (and any intermediate number for now) :
        repeat
          Inc(cp);
        until not (cp^ in ['+','-','.',',','0'..'9']);

        // Handle the various types of input :
        case cp^ of
          'i', 'd', 'u',
          'I', 'D', 'U':
            ResultStr := ResultStr + IntToStr(va.ReadInt32);
          'f',
          'F':
            ResultStr := ResultStr + FloatToStr(va.ReadDouble);
          's',
          'S':
            ResultStr := ResultStr + AnsiString(va.ReadPAnsiChar);
          'x',
          'X':
            ResultStr := ResultStr + IntToHex(va.ReadInt32, 8);
        else
          ResultStr := ResultStr + '!DXBX WARNING:UNSUPPORTED INPUT!';
        end;
      end;
    else
      // Add the character (could be done faster, but suffices for now) :
      ResultStr := ResultStr + cp^;
    end;

    Inc(cp);
  end;

  // Return the length of the string (if that's what we're supposed to do?) :
  Result := Length(ResultStr);

  // Write it to our log :
  if MayLog(lfKernel or lfDebug) then
    DbgPrintf('EmuKrnl : ' + string(ResultStr));

  EmuSwapFS(fsXbox);
end;

function {010} xboxkrnl_DbgPrompt(
  Prompt: PCCH;
  Response: PCH; // OUT
  MaximumResponseLength: ULONG
  ): ULONG; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('DbgPrompt');
  EmuSwapFS(fsXbox);
end;

procedure {011} xboxkrnl_DbgUnLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('DbgUnLoadImageSymbols');
  EmuSwapFS(fsXbox);
end;

end.
