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
  uConsts, // for cDLLName
  uTypes, // for DebugMode
  uXbe; // for P_XBE_TLS

type
  TEntryProc = procedure();
  PEntryProc = ^TEntryProc;

  TThunkTable = packed array[0..366] of DWord;
  PThunkTable = ^TThunkTable;

procedure CxbxKrnlInit(hwndParent: THandle;
                       pTLSData: Pointer;
                       pTLS: P_XBE_TLS;
                       pLibraryVersion: P_XBE_LIBRARYVERSION;
                       DbgMode: DebugMode;
                       szDebugFilename: PChar;
                       pXbeHeader: P_XBE_HEADER;
                       dwXbeHeaderSize: DWord;
                       Entry: PEntryProc); cdecl; external cDLLNAME;

procedure SetXbePath(const path: PChar); cdecl; external cDLLNAME name '?SetXbePath@EmuShared@@QAEXPBD@Z';
procedure CxbxKrnlNoFunc; cdecl; external cDLLNAME;

implementation

end.
