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
unit uEmuLDT;

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // Dxbx
  uConsts;
  
procedure EmuInitLDT;

implementation

var
  // Table of free LDT entries
  FreeLDTEntries : array [00..MAXIMUM_XBOX_THREADS-1] of DWord;

  // Critical section lock
  EmuLDTLock: _RTL_CRITICAL_SECTION;

procedure EmuInitLDT;
var
  v: Integer;
begin
  InitializeCriticalSection(EmuLDTLock);

  for v := 0 to MAXIMUM_XBOX_THREADS - 1 do
    FreeLDTEntries[v] := DWord((v * 8) + 7 + 8);
end;

end.

