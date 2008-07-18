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

uses
  // Delphi
  Windows,
  SysUtils,
  Winsock,
  // Jedi
  JwaWinType,
  // Dxbx
  uLog,
  uEmuFS;

// func: EmuWSAStartup

function XTL_EmuWSAStartup(wVersionRequested: WORD; lpWSAData: WSADATA): integer;
var
  ret: Integer;
begin
  EmuSwapFS; // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuWSAStartup' +
    '( ' +
    '   wVersionRequested   : 0x%.08X' +
    '   lpWSAData           : 0x%.08X' +
    ');',
    [wVersionRequested, @lpWSAData]);

  ret := WSAStartup(wVersionRequested, lpWSAData);

  EmuSwapFS(); // XBox FS

  Result := ret;
end;

// func: EmuXNetStartup

function XTL_EmuXNetStartup(pDummy: PVOID): Integer;
begin
  EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('EmuXapi : EmuXNetStartup' +
    #13#10'(' +
    #13#10'   pDummy              : $%.08X' +
    #13#10');',
    [pDummy]);

  EmuSwapFS(); // XBox FS

    (*// Fake Successfull...hehehe...sucker...hehehehehe *)
    // Remark in cxbx code... wtf ??
  Result := 0;
end;

// func: EmuXNetGetEthernetLinkStatus

function XTL_EmuXNetGetEthernetLinkStatus: DWORD;
begin
  EmuSwapFS(); // Win2k/XP FS
  DbgPrintf('EmuXapi : EmuXNetGetEthernetLinkStatus();');
  EmuSwapFS(); // XBox FS

  // for now, no ethernet connection is available
  Result := 0;
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

    DbgPrintf('EmuXapi ($%X): EmuThis.Emusocket'
           '('
           '   this                : $%.08X'
           '   af                  : $%.08X'
           '   ctype                : $%.08X'
           '   protocol            : $%.08X'
           ');',
           [this, af, ctype, protocol);

    SOCKET ret = socket(af, ctype, protocol);

    EmuSwapFS();   // XBox FS

    Result:= ret;
 end;             *)

// ******************************************************************
// * func: EmuThis::Emubind
// ******************************************************************
(*function XTL.EmuThis.Emubind(s: SOCKET; var sockaddrFARname: struct; namelen: integer): integer;
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi ($%X): EmuThis.Emubind'
           '('
           '   this                : $%.08X'
           '   s                   : $%.08X'
           '   name                : $%.08X'
           '   namelen             : $%.08X'
           ');',
           [this, s, name, namelen);

    // TODO: Host-To-Network order if necessary (probably not?)

    integer ret := bind(s, name, namelen);

    EmuSwapFS();   // XBox FS

    Result:= ret;
 end;              *)

// ******************************************************************
// * func: EmuThis::Emulisten
// ******************************************************************
(*function XTL.EmuThis.Emulisten(s: SOCKET; backlog: integer): integer;
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi ($%X): EmuThis.Emulisten'
           '('
           '   this                : $%.08X'
           '   s                   : $%.08X'
           '   listen              : $%.08X'
           ');',
           [this, s, backlog);

    // TODO: Host-To-Network order if necessary (probably not?)

    integer ret := listen(s, backlog);

    EmuSwapFS();   // XBox FS

    Result:= ret;
 end;              *)

// ******************************************************************
// * func: EmuThis::Emuioctlsocket
// ******************************************************************
(*function XTL.EmuThis.Emuioctlsocket(s: SOCKET; cmd: LongInt; var FARargp: u_long): integer;
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuXapi ($%X): EmuThis.Emuioctlsocket'
           '('
           '   this                : $%.08X'
           '   s                   : $%.08X'
           '   cmd                 : $%.08X'
           '   argp                : $%.08X'
           ');',
           [this, s, cmd, argp]);

    integer ret := ioctlsocket(s, cmd, argp);

    EmuSwapFS();   // XBox FS

    Result := ret;
 end;            *)



end.

