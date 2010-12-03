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
  uEmuKrnl,
  uTypes,
  uLog,
  uEmuFS;

implementation

const
  lfUnit = lfCxbx or lfXOnline;

function LogBegin(const aSymbolName: string): PLogStack;
begin
  Result := uLog.LogBegin(aSymbolName, {Category=}'XOnline');
end;

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

  if MayLog(lfUnit) then
    LogBegin('EmuWSAStartup').
      _(wVersionRequested, 'wVersionRequested').
      _(lpWSAData, 'lpWSAData').
    LogEnd();

  ret := WSAStartup(wVersionRequested, {var}lpWSAData^);

  EmuSwapFS(fsXbox);

  Result := ret;
end;

function XTL_EmuXNetCleanup(): Integer; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetStartup').
    LogEnd();

  Unimplemented('EmuXNetCleanup');
  Result := 0;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXNetConnect
(
    ina: IN_ADDR
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    // Todo: Log ina
    LogBegin('EmuXNetStartup').
    //      _(ina, 'ina').
    LogEnd();

  Unimplemented('EmuXNetConnect');
  Result := 0;

  EmuSwapFS(fsXbox);
end;

// Todo: PXNKID and PXNKEY not availible
(*function XTL_EmuXNetCreateKey
(
    pxnkid: PXNKID;
    pxnKey: PXNKEY;
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetCreateKey').
      _(pxnkid, 'pxnkid').
      _(pxnKey, 'pxnKey').
    LogEnd();

  Unimplemented('EmuXNetCreateKey');
  Result := 0;

  EmuSwapFS(fsXbox);
end; *)

// Todo: pszHost, hEvent, ppxndns not availible
(*function XTL_EmuXNetDnsLookup
(
    pszHost: PChar;
    hEvent: WSAEVENT;
    ppxndns: PPXNDNS;
): INT: stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetDnsLookup').
      _(pszHost, 'pszHost').
      _(hEvent, 'hEvent').
      _(ppxndns, 'ppxndns').
    LogEnd();

  Unimplemented('EmuXNetDnsLookup');
  Result := 0;

  EmuSwapFS(fsXbox);
end; *)

// Todo: PXNDNS not availible
(*function XTL_EmuXNetDnsRelease
(
    pxndns: PXNDNS
): INT: stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetDnsRelease').
      _(pxndns, 'pxndns').
    LogEnd();

  Unimplemented('EmuXNetDnsRelease');
  Result := 0;

  EmuSwapFS(fsXbox);
end; *)

function XTL_EmuXNetGetConnectStatus
(
    ina: IN_ADDR
): DWORD; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    // Todo: Log ina
    LogBegin('XNetGetConnectStatus').
//      _(ina, 'ina').
    LogEnd();

  Unimplemented('XNetGetConnectStatus');

  EmuSwapFS(fsXbox);
end;

// TODO: PXNADDR not availible
(*function XTL_EmuXNetGetDebugXnAddr
(
    pxna: PXNADDR
): DWORD; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetGetDebugXnAddr').
      _(pxna, 'pxna').
    LogEnd();

  Unimplemented('EmuXNetGetDebugXnAddr');

  EmuSwapFS(fsXbox);
end; *)

// TODO: PXNADDR not availible
(*function XTL_EmuXNetGetTitleXnAddr
(
    pxna: PXNADDR
): DWORD; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetGetTitleXnAddr').
      _(pxna, 'pxna').
    LogEnd();

  Unimplemented('EmuXNetGetTitleXnAddr');

  EmuSwapFS(fsXbox);
end; *)

procedure XTL_EmuXNetInAddrToString
(
    ina: IN_ADDR;
    pchBuf: Pchar;
    cchBuf: INT
); stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    // Todo: Log ina
    LogBegin('EmuXNetInAddrToString').
//      _(ina, 'ina').
      _(pchBuf, 'pchBuf').
      _(cchBuf, 'cchBuf').
    LogEnd();

  Unimplemented('EmuXNetInAddrToString');

  EmuSwapFS(fsXbox);
end;

// Todo: PXNADDR and PXNKID not availible
(*function XTL_EmuXNetInAddrToXnAddr
(
    ina: IN_ADDR;
    pxna: PXNADDR;
    pxnkid: PXNKID
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetInAddrToXnAddr').
      _(ina, 'ina').
      _(pxna, 'pxna').
      _(pxnkid, 'pxnkid').
    LogEnd();

  Unimplemented('EmuXNetInAddrToXnAddr');

  EmuSwapFS(fsXbox);
end; *)

// TODO: Implement
(*function XTL_EmuXNetQosListen(
    XNKID * pxnkid,
    BYTE * pb,
    UINT cb,
    DWORD dwBitsPerSec,
    DWORD dwFlags
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin

end; *)

// Todo: Implement
(*function XTL_EmuXNetQosLookup(
    UINT cxna,
    XNADDR * apxna[],
    XNKID * apxnkid[],
    XNKEY * apxnkey[],
    UINT cina,
    IN_ADDR aina[],
    DWORD adwServiceId[],
    UINT cProbes,
    DWORD dwBitsPerSec,
    DWORD dwFlags,
    WSAEVENT hEvent,
    XNQOS * * ppxnqos
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
egin

end; *)

// Todo: Implement
(*function XTL_EmuXNetQosRelease
(
    XNQOS * pxnqos
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin

end; *)

// Todo: Implement
(*function XTL_EmuXNetServerToInAddr
(
    ina: IN_ADDR;
    dwServiceId: DWORD;
    pina: PIN_ADDR
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin

end; *)

function XTL_EmuXNetUnregisterInAddr
(
    ina: IN_ADDR
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    // Todo: Log ina
    LogBegin('EmuXNetUnregisterInAddr').
//      _(ina, 'ina').
    LogEnd();

  Unimplemented('EmuXNetUnregisterInAddr');

  EmuSwapFS(fsXbox);

  Result := 0;
end;

// Todo: Implement
(*function XTL_EmuXNetXnAddrToInAddr
(
    const XNADDR *pxna,
    const XNKID *pxnkid,
    IN_ADDR *pina
): INT: stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin

end; *)

function XTL_EmuXNetStartup
(
    {const} pDummy: PVOID
): INT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetStartup').
      _(pDummy, 'pDummy').
    LogEnd();

  EmuSwapFS(fsXbox);

  // Cxbx : Fake Successfull...hehehe...sucker...hehehehehe

  Result := 0;
end;

function XTL_EmuXNetGetEthernetLinkStatus(): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetGetEthernetLinkStatus').
    LogEnd();

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

  if MayLog(lfUnit) then
    LogBegin('EmuThis.Emusocket').
      _(this, 'this').
      _(af, 'af').
      _(ctype, 'ctype').
      _(protocol, 'protocol').
    LogEnd();

    SOCKET ret = socket(af, ctype, protocol);

    EmuSwapFS(fsXbox);

    Result := ret;
end;

function XTL.EmuThis.Emubind(s: SOCKET; var sockaddrFARname: struct; namelen: Integer): Integer;
--DXBX:UNUSED_CODE Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuThis.Emubind').
      _(this, 'this').
      _(s, 's').
      _(name, 'name').
      _(namelen, 'namelen').
    LogEnd();

    // TODO -oCXBX:: Host-To-Network order if necessary (probably not?)

    Integer ret := bind(s, name, namelen);

    EmuSwapFS(fsXbox);

    Result := ret;
end;

function XTL.EmuThis.Emulisten(s: SOCKET; backlog: Integer): Integer;
--DXBX:UNUSED_CODE Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuThis.Emulisten').
      _(this, 'this').
      _(s, 's').
      _(listen, 'listen').
    LogEnd();

    // TODO -oCXBX: Host-To-Network order if necessary (probably not?)

    Integer ret := listen(s, backlog);

    EmuSwapFS(fsXbox);

    Result := ret;
end;

function XTL.EmuThis.Emuioctlsocket(s: SOCKET; cmd: LongInt; var FARargp: u_long): Integer;
--DXBX:UNUSED_CODE Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuThis.Emuioctlsocket').
      _(this, 'this').
      _(s, 's').
      _(cmd, 'cmd').
      _(argp, 'argp').
    LogEnd();

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

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineLaunchNewImage').
      _(UIntPtr(lpImagePath), 'lpImagePath').
      _(pLaunchData, 'pLaunchData').
    LogEnd();

  // TODO -oCXBX: Launch another .xbe from Cxbx someday?

  EmuSwapFS(fsXbox);

  Result := E_FAIL;
end;

function XTL_EmuXOnlineLogon
(
    pUsers: PVOID;
    pdwServiceIDs: PDWORD;
    dwServices: DWORD;
    hEvent: HANDLE;
    pHandle: HANDLE
): HRESULT; stdcall;
begin
	EmuSwapFS(fsWindows);	// Win2k/XP FS
  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineLogon').
      _(pUsers, 'pUsers').
      _(pdwServiceIDs, 'pdwServiceIDs').
      _(dwServices, 'dwServices').
      _(hEvent, 'hEvent').
      _(pHandle, 'pHandle').
    LogEnd();

	// TODO: What will it take to log on to Xbox Live?

	EmuSwapFS(fsXbox);	// Xbox FS

	Result := HResult($80151000);	// XONLINE_E_LOGON_NO_NETWORK_CONNECTION
end;





exports
  XTL_EmuWSAStartup,

  XTL_EmuXNetGetEthernetLinkStatus,
  XTL_EmuXNetStartup, //: DXBX marked out for better logging

  XTL_EmuXNetCleanup,
  XTL_EmuXNetConnect,
//  XTL_EmuXNetCreateKey, // not implemented
//  XTL_EmuXNetDnsLookup, // not implemented
//  XTL_EmuXNetDnsRelease, // not implemented
  XTL_EmuXNetGetConnectStatus,
//  XTL_EmuXNetGetDebugXnAddr, // not implemented
  XTL_EmuXNetGetEthernetLinkStatus,
//  XTL_EmuXNetGetTitleXnAddr, // not implemented
  XTL_EmuXNetInAddrToString,
//  XTL_EmuXNetInAddrToXnAddr, // not implemented
//  XTL_EmuXNetQosListen, // not implemented
//  XTL_EmuXNetQosLookup, // not implemented
//  XTL_EmuXNetQosRelease, // not implemented
//  XTL_EmuXNetServerToInAddr, // not implemented
//  XTL_EmuXNetStartup, // not implemented
  XTL_EmuXNetUnregisterInAddr,
//  XTL_EmuXNetXnAddrToInAddr, // not implemented

//  XTL_EmuXOnlineLaunchNewImage,
  XTL_EmuXOnlineLogon;


end.

