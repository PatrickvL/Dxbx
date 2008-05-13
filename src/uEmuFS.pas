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
unit uEmuFS;

interface

procedure EmuSwapFS;
procedure EmuCleanupFS;
function EmuIsXboxFS: Boolean;

var
  EmuAutoSleepRate: Integer = -1;

implementation

uses
  // Delphi
  SysUtils;

function EmuIsXboxFS: Boolean;
var
  chk: Char;
begin
  asm
    mov ah, fs:16h
    mov chk, ah
  end;

  Result := (chk = '1');
end;

procedure EmuSwapFS;
const
{$J+}
  dwInterceptionCount: Integer = 0;
{$J-}
begin
  // Note that this is only the *approximate* interception count,
  // because not all interceptions swap the FS register, and some
  // non-interception code uses it

  asm
    mov ax, fs:14h
    mov fs, ax
  end;

  // Every "N" interceptions, perform various periodic services
  if (dwInterceptionCount + 1) >= EmuAutoSleepRate then
  begin
    // If we're in the Xbox FS, wait until the next swap
    if EmuIsXboxFS then
      Dec(dwInterceptionCount)
    else
    begin
      // Yield!
      Sleep(1);

      // Back to Zero!
      dwInterceptionCount := 0;
    end;
  end;
end; // EmuSwapFS

procedure EmuCleanupFS;
//var
//  wSwapFS: Integer;
begin
(*{
    uint16 wSwapFS = 0;

    __asm
    {
        mov ax, fs:[0x14]   // FS.ArbitraryUserPointer
        mov wSwapFS, ax
    }

    if(wSwapFS == 0)
        return;

    if(!EmuIsXboxFS())
        EmuSwapFS();    // Xbox FS

    uint08 *pTLSData = NULL;

    __asm
    {
        mov eax, fs:[0x04]
        mov pTLSData, eax
    }

    EmuSwapFS(); // Win2k/XP FS

    if(pTLSData != 0)
        delete[] pTLSData;

    EmuDeallocateLDT(wSwapFS);
}          *)
end;

end.
