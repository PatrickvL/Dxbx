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
unit uEmuShared;

{$INCLUDE Dxbx.inc}

interface

procedure SetXbePath(const Path: PChar); cdecl;
procedure Init;
procedure Cleanup;

implementation

uses
  // Delphi
  Dialogs,
  // Dxbx
  uTypes, uLog, uMutex;

var
  m_XbePath: string;
  g_EmuSharedRefCount: Integer;

procedure Init;
begin
  WriteLog('Init');

  // Ensure initialization only occurs once

  // Prevent multiple initializations
   // if(hMapObject != NULL)
   //     return;

    // ******************************************************************
    // * Create the shared memory "file"
    // ******************************************************************
    {
        hMapObject = CreateFileMapping
        ( 
            INVALID_HANDLE_VALUE,   // Paging file
            NULL,                   // default security attributes
            PAGE_READWRITE,         // read/write access
            0,                      // size: high 32 bits
            sizeof(EmuShared),      // size: low 32 bits
            "Local\\EmuShared"      // name of map object
        );

        if(hMapObject == NULL)
			EmuCleanup("Could not map shared memory!");

        if(GetLastError() == ERROR_ALREADY_EXISTS)
            init = false;
    }

    // ******************************************************************
    // * Memory map this file
    // ******************************************************************
    {
//      g_EmuShared = (EmuShared*.MapViewOfFile
        (
            hMapObject,     // object to map view of
            FILE_MAP_WRITE, // read/write access
            0,              // high offset:  map from
            0,              // low offset:   beginning
            0               // default: map entire file
        );

        if(g_EmuShared == NULL) 
			EmuCleanup("Could not map view of shared memory!");
    }

    // ******************************************************************
    // * Executed only on first initialization of shared memory
    // ******************************************************************
   { if init then
       EmuShared();
                }
  Inc(g_EmuSharedRefCount);
end;

procedure Cleanup;
begin
  WriteLog('EmuSharedCleanup');
  Dec(g_EmuSharedRefCount);

  (*if(g_EmuSharedRefCount = 0)
    EmuShared();

  UnmapViewOfFile(g_EmuShared);*)
  CloseLogs;
end;

procedure SetXbePath(const Path: PChar); cdecl;
begin
  Lock();
  WriteLog('Emu: SetXbePath -  ' + Path);
  m_XbePath := Path;
  Unlock();
end;

end.
