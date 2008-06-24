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

unit uEmuXOnline;

{$INCLUDE ..\Dxbx.inc}

interface

implementation

uses Windows, SysUtils,
  uLog, uEmuFS;

{ TODO : need to be translated to delphi }
// func: EmuWSAStartup

function XTL_EmuWSAStartup: integer;
var
(*(
    WORD        wVersionRequested,
    WSADATA    *lpWSAData
) *)
  ret : Integer;
begin
    EmuSwapFS;   // Win2k/XP FS

    (*DbgPrintf(Format ('EmuXapi (% mod X): EmuWSAStartup' +
           '( '+
           '   wVersionRequested   : % mod .08X' +
           '   lpWSAData           : % mod .08X' +
           ');',
           [GetCurrentThreadId(), wVersionRequested, lpWSAData]));

    ret := XTL.WSAStartup(wVersionRequested, lpWSAData); *)

    EmuSwapFS();   // XBox FS

    result:= ret;
end;

// func: EmuXNetStartup

function XTL_EmuXNetStartup: Integer;
(*(
     PVOID pDummy
) *)
begin
    EmuSwapFS();   // Win2k/XP FS

    (*DbgPrintf("EmuXapi ($ mod X): EmuXNetStartup"
           "("
           "   pDummy              : $ mod .08X"
           ");",
           GetCurrentThreadId(), pDummy); *)

    EmuSwapFS();   // XBox FS

    (*// Fake Successfull...hehehe...sucker...hehehehehe *)
    // Remark in cxbx code... wtf ??
    result:= 0;
end;

// func: EmuXNetGetEthernetLinkStatus

function XTL_EmuXNetGetEthernetLinkStatus: DWORD;
begin
    EmuSwapFS();   // Win2k/XP FS
    DbgPrintf( Format ('EmuXapi (% mod X): EmuXNetGetEthernetLinkStatus();', [GetCurrentThreadId]));
    EmuSwapFS();   // XBox FS

    // for now, no ethernet connection is available
    result:= 0; 
end;

// ******************************************************************
// * func: EmuThis::Emusocket
// ******************************************************************
(*SOCKET XTL.EmuThis.Emusocket
(
    integer   af,
    integer   ctype,
    integer   protocol
)
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuXapi ($ mod X): EmuThis.Emusocket"
           "("
           "   this                : $ mod .08X"
           "   af                  : $ mod .08X"
           "   ctype                : $ mod .08X"
           "   protocol            : $ mod .08X"
           ");",
           GetCurrentThreadId(), this, af, ctype, protocol);

    SOCKET ret = socket(af, ctype, protocol);

    EmuSwapFS();   // XBox FS

    result:= ret;
 end;             *)

// ******************************************************************
// * func: EmuThis::Emubind
// ******************************************************************
(*function XTL.EmuThis.Emubind(s: SOCKET; var sockaddrFARname: struct; namelen: integer): integer;
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuXapi ($ mod X): EmuThis.Emubind"
           "("
           "   this                : $ mod .08X"
           "   s                   : $ mod .08X"
           "   name                : $ mod .08X"
           "   namelen             : $ mod .08X"
           ");",
           GetCurrentThreadId(), this, s, name, namelen);

    // TODO: Host-To-Network order if necessary (probably not?)

    integer ret := bind(s, name, namelen);

    EmuSwapFS();   // XBox FS

    result:= ret;
 end;              *)

// ******************************************************************
// * func: EmuThis::Emulisten
// ******************************************************************
(*function XTL.EmuThis.Emulisten(s: SOCKET; backlog: integer): integer;
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuXapi ($ mod X): EmuThis.Emulisten"
           "("
           "   this                : $ mod .08X"
           "   s                   : $ mod .08X"
           "   listen              : $ mod .08X"
           ");",
           GetCurrentThreadId(), this, s, backlog);

    // TODO: Host-To-Network order if necessary (probably not?)

    integer ret := listen(s, backlog);

    EmuSwapFS();   // XBox FS

    result:= ret;
 end;              *)

// ******************************************************************
// * func: EmuThis::Emuioctlsocket
// ******************************************************************
(*function XTL.EmuThis.Emuioctlsocket(s: SOCKET; cmd: LongInt; var FARargp: u_long): integer;
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuXapi ($ mod X): EmuThis.Emuioctlsocket"
           "("
           "   this                : $ mod .08X"
           "   s                   : $ mod .08X"
           "   cmd                 : $ mod .08X"
           "   argp                : $ mod .08X"
           ");",
           GetCurrentThreadId(), this, s, cmd, argp);

    integer ret := ioctlsocket(s, cmd, argp);

    EmuSwapFS();   // XBox FS

    result:= ret;
 end;            *)



end.
