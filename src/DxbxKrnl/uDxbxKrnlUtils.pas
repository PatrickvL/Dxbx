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
unit uDxbxKrnlUtils;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // Dxbx
  uLog; // for WriteLog

procedure CxbxKrnlCleanup(const szErrorMessage: string);

implementation

procedure CxbxKrnlCleanup(const szErrorMessage: string);
var
  szBuffer1: string;
begin
  // Print out ErrorMessage (if exists)
  if szErrorMessage <> '' then
  begin
    szBuffer1 := {Format}'CxbxKrnlCleanup : Recieved Fatal Message ->'#13#13 + szErrorMessage;
    WriteLog(szBuffer1);
    MessageBox(0, @(szBuffer1[1]), 'DxbxKrnl', MB_OK or MB_ICONEXCLAMATION);
  end;

  WriteLog('DxbxKrnl: Terminating Process');
//  FFlush(stdout);

  // Cleanup debug output
  CloseLogs();// FreeConsole();

//        char buffer[16];
//        if(GetConsoleTitle(buffer, 16) != NULL)
//            freopen("nul", "w", stdout);

  TerminateProcess(GetCurrentProcess(), 0);

  Exit;
end;

end.

