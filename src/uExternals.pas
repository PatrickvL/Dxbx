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
unit uExternals;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Types, // for DWord
  // Dxbx
  uConsts,
  uTypes, // for DebugMode
  uXbe; // for P_XBE_TLS

// Note :
// In order to place the address of these methods into the generated EXE
// we are using GetProcAddress - but this will only result in the correct
// addresses when we link them statically inside this project also!

procedure CxbxKrnl_Init(hwndParent: THandle;
                       pTLSData: Pointer;
                       pTLS: P_XBE_TLS;
                       pLibraryVersion: P_XBE_LIBRARYVERSION;
                       DbgMode: DebugMode;
                       szDebugFilename: PChar;
                       pXbeHeader: P_XBE_HEADER;
                       dwXbeHeaderSize: DWord;
                       Entry: PEntryProc); cdecl; external CCXBXKRNLDLLNAME;
procedure Cxbx_Init(hwndParent: THandle;
                       pTLSData: Pointer;
                       pTLS: P_XBE_TLS;
                       pLibraryVersion: P_XBE_LIBRARYVERSION;
                       DbgMode: DebugMode;
                       szDebugFilename: PChar;
                       pXbeHeader: P_XBE_HEADER;
                       dwXbeHeaderSize: DWord;
                       Entry: PEntryProc); cdecl; external CCXBXDLLNAME;
procedure DxbxKrnl_Init(hwndParent: THandle;
                       pTLSData: Pointer;
                       pTLS: P_XBE_TLS;
                       pLibraryVersion: P_XBE_LIBRARYVERSION;
                       DbgMode: DebugMode;
                       szDebugFilename: PChar;
                       pXbeHeader: P_XBE_HEADER;
                       dwXbeHeaderSize: DWord;
                       Entry: PEntryProc); cdecl; external CDXBXKRNLDLLNAME;
(*
procedure Dxbx_Init(hwndParent: THandle;
                       pTLSData: Pointer;
                       pTLS: P_XBE_TLS;
                       pLibraryVersion: P_XBE_LIBRARYVERSION;
                       DbgMode: DebugMode;
                       szDebugFilename: PChar;
                       pXbeHeader: P_XBE_HEADER;
                       dwXbeHeaderSize: DWord;
                       Entry: PEntryProc); cdecl; external CDXBXDLLNAME;
*)

procedure CxbxKrnl_SetXbePath(const path: PChar); cdecl; external CCXBXKRNLDLLNAME name CSETXBEPATHMANGLEDNAME;

implementation

end.
