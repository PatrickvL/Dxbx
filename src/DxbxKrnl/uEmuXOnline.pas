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

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  Winsock,
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  // Dxbx
  uTypes,
  uLog,
  uEmuFS;

implementation

function XTL_EmuWSAStartup
(
  wVersionRequested: WORD;
  lpWSAData: PWSADATA
): int; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  ret: int;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  LogBegin('XOnline : EmuWSAStartup').
    _(wVersionRequested, 'wVersionRequested').
    _(lpWSAData, 'lpWSAData').
    LogEnd;
{$ENDIF}

  ret := WSAStartup(wVersionRequested, {var}lpWSAData^);

  EmuSwapFS(fsXbox);

  Result := ret;
end;

function XTL_EmuXNetStartup
(
    {const} pDummy: PVOID
): INT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  LogBegin('XOnline : EmuXNetStartup').
    _(pDummy, 'pDummy').
    LogEnd;
{$ENDIF}

  EmuSwapFS(fsXbox);

  // Cxbx : Fake Successfull...hehehe...sucker...hehehehehe

  Result := 0;
end;

function XTL_EmuXNetGetEthernetLinkStatus(): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
  DbgPrintf('XOnline : EmuXNetGetEthernetLinkStatus();');
{$ENDIF}
  EmuSwapFS(fsXbox);

  // Cxbx : for now, no ethernet connection is available
  Result := 0;
end;

(*
SOCKET XTL.EmuThis.Emusocket
(
    af: int;
    ctype: int;
    protocol: int
)
--DXBX:UNUSED_CODE Branch:martin  UNUSED_Revision:39  Translator:PatrickvL  Done:0
begin
    EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('XOnline : EmuThis.Emusocket' +
        #13#10'(' +
        #13#10'   this                : 0x%.08X' +
        #13#10'   af                  : 0x%.08X' +
        #13#10'   ctype               : 0x%.08X' +
        #13#10'   protocol            : 0x%.08X' +
        #13#10');',
        [this, af, ctype, protocol);
{$ENDIF}

    SOCKET ret = socket(af, ctype, protocol);

    EmuSwapFS(fsXbox);

    Result := ret;
end;

function XTL.EmuThis.Emubind(s: SOCKET; var sockaddrFARname: struct; namelen: Integer): Integer;
--DXBX:UNUSED_CODE Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('XOnline : EmuThis.Emubind' +
        #13#10'(' +
        #13#10'   this                : 0x%.08X' +
        #13#10'   s                   : 0x%.08X' +
        #13#10'   name                : 0x%.08X' +
        #13#10'   namelen             : 0x%.08X' +
        #13#10');',
        [this, s, name, namelen);
{$ENDIF}

    // TODO -oCXBX:: Host-To-Network order if necessary (probably not?)

    Integer ret := bind(s, name, namelen);

    EmuSwapFS(fsXbox);

    Result := ret;
end;

function XTL.EmuThis.Emulisten(s: SOCKET; backlog: Integer): Integer;
--DXBX:UNUSED_CODE Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('XOnline : EmuThis.Emulisten' +
        #13#10'(' +
        #13#10'   this                : 0x%.08X' +
        #13#10'   s                   : 0x%.08X' +
        #13#10'   listen              : 0x%.08X' +
        #13#10');',
        [this, s, backlog);
{$ENDIF}

    // TODO -oCXBX: Host-To-Network order if necessary (probably not?)

    Integer ret := listen(s, backlog);

    EmuSwapFS(fsXbox);

    Result := ret;
end;

function XTL.EmuThis.Emuioctlsocket(s: SOCKET; cmd: LongInt; var FARargp: u_long): Integer;
--DXBX:UNUSED_CODE Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('XOnline : EmuThis.Emuioctlsocket' +
        #13#10'(' +
        #13#10'   this                : 0x%.08X' +
        #13#10'   s                   : 0x%.08X' +
        #13#10'   cmd                 : 0x%.08X' +
        #13#10'   argp                : 0x%.08X' +
        #13#10');',
        [this, s, cmd, argp]);
{$ENDIF}

    Integer ret := ioctlsocket(s, cmd, argp);

    EmuSwapFS(fsXbox);

    Result := ret;
end;
*)

function XTL_EmuXOnlineLaunchNewImage
(
    lpImagePath: LPCSTR;
    pLaunchData: LPVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  LogBegin('XOnline : EmuXOnlineLaunchNewImage').
    _(UIntPtr(lpImagePath), 'lpImagePath').
    _(pLaunchData, 'pLaunchData').
    LogEnd;
{$ENDIF}
  // TODO -oCXBX: Launch another .xbe from Cxbx someday?

  EmuSwapFS(fsXbox);

  Result := E_FAIL;
end;


exports
  XTL_EmuWSAStartup,

  XTL_EmuXNetGetEthernetLinkStatus,
  XTL_EmuXNetStartup, //: DXBX marked out for better logging

  XTL_EmuXOnlineLaunchNewImage;

end.

