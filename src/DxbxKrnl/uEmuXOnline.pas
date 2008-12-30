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

implementation

function XTL_EmuWSAStartup(wVersionRequested: Word; lpWSAData: WSADATA): Integer; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  ret: Integer;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuWSAStartup' +
    #13#10'( ' +
    #13#10'   wVersionRequested   : 0x%.08X' +
    #13#10'   lpWSAData           : 0x%.08X' +
    #13#10');',
    [wVersionRequested, @lpWSAData]);

  ret := WSAStartup(wVersionRequested, lpWSAData);

  EmuSwapFS(fsXbox);

  Result := ret;
end;

function XTL_EmuXNetStartup(pDummy: PVOID): Integer; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuXapi : EmuXNetStartup' +
    #13#10'(' +
    #13#10'   pDummy              : 0x%.08X' +
    #13#10');',
    [pDummy]);

  EmuSwapFS(fsXbox);

    (*// Fake Successfull...hehehe...sucker...hehehehehe *)
    // Remark in cxbx code... wtf ??
  Result := 0;
end;

function XTL_EmuXNetGetEthernetLinkStatus: DWord; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:5
begin
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuXapi : EmuXNetGetEthernetLinkStatus();');
  EmuSwapFS(fsXbox);

  // for now, no ethernet connection is available
  Result := 0;
end;

(*SOCKET XTL.EmuThis.Emusocket
(
    Integer   af,
    Integer   ctype,
    Integer   protocol
)
begin
    EmuSwapFS(fsWindows);

    DbgPrintf('EmuXapi ($%X): EmuThis.Emusocket'
           '('
           '   this                : $%.08X'
           '   af                  : $%.08X'
           '   ctype                : $%.08X'
           '   protocol            : $%.08X'
           ');',
           [this, af, ctype, protocol);

    SOCKET ret = socket(af, ctype, protocol);

    EmuSwapFS(fsXbox);

    Result:= ret;
end;             *)

(*function XTL.EmuThis.Emubind(s: SOCKET; var sockaddrFARname: struct; namelen: Integer): Integer;
begin
    EmuSwapFS(fsWindows);

    DbgPrintf('EmuXapi ($%X): EmuThis.Emubind'
           '('
           '   this                : $%.08X'
           '   s                   : $%.08X'
           '   name                : $%.08X'
           '   namelen             : $%.08X'
           ');',
           [this, s, name, namelen);

    // TODO: Host-To-Network order if necessary (probably not?)

    Integer ret := bind(s, name, namelen);

    EmuSwapFS(fsXbox);

    Result:= ret;
end;              *)

(*function XTL.EmuThis.Emulisten(s: SOCKET; backlog: Integer): Integer;
begin
    EmuSwapFS(fsWindows);

    DbgPrintf('EmuXapi ($%X): EmuThis.Emulisten'
           '('
           '   this                : $%.08X'
           '   s                   : $%.08X'
           '   listen              : $%.08X'
           ');',
           [this, s, backlog);

    // TODO: Host-To-Network order if necessary (probably not?)

    Integer ret := listen(s, backlog);

    EmuSwapFS(fsXbox);

    Result:= ret;
 end;              *)

(*function XTL.EmuThis.Emuioctlsocket(s: SOCKET; cmd: LongInt; var FARargp: u_long): Integer;
begin
    EmuSwapFS(fsWindows);

    DbgPrintf('EmuXapi ($%X): EmuThis.Emuioctlsocket'
           '('
           '   this                : $%.08X'
           '   s                   : $%.08X'
           '   cmd                 : $%.08X'
           '   argp                : $%.08X'
           ');',
           [this, s, cmd, argp]);

    Integer ret := ioctlsocket(s, cmd, argp);

    EmuSwapFS(fsXbox);

    Result := ret;
end;            *)

exports
  XTL_EmuWSAStartup,
  XTL_EmuXNetGetEthernetLinkStatus,
  XTL_EmuXNetStartup;

end.

