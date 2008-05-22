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
unit uLog;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
  // Dxbx
  uTypes;

var
  m_DxbxDebug: DebugMode = DM_NONE;
  m_DxbxDebugFilename: string = '';
  m_KrnlDebug: DebugMode = DM_CONSOLE;
  m_KrnlDebugFilename: string = '';

procedure CreateLogs(aLogType: TLogType = ltKernel);
procedure CloseLogs;
procedure WriteLog(aText: string);
procedure SetLogMode(aLogMode: DebugMode = DM_NONE); export;


implementation

uses
  // Delphi
  Dialogs,
  // Dxbx
  uLogConsole;

var
  LogMode: DebugMode = DM_NONE;
  LogFileOpen: Boolean = False;
  LogFile: TextFile;

procedure SetLogMode(aLogMode: DebugMode = DM_NONE); export;
begin
  LogMode := aLogMode;
end;

procedure CreateLogs(aLogType: TLogType);
begin
  case m_DxbxDebug of
    DM_NONE:
      CloseLogs;

    DM_CONSOLE:
      try
        frm_LogConsole := Tfrm_LogConsole.Create(nil);
        if aLogType = ltGui then
          frm_LogConsole.Caption := 'DXBX : Debug Console'
        else // ltKernel
          frm_LogConsole.Caption := 'DXBX : Kernel Debug Console';

        frm_LogConsole.Show;
        LogMode := DM_CONSOLE;
      except
        ShowMessage('Could not create log console');
        FreeAndNil({var}frm_LogConsole);
        LogMode := DM_NONE;
      end;

    DM_FILE:
      try
        if aLogType = ltGui then
          AssignFile({var}LogFile, m_DxbxDebugFilename)
        else // ltKernel
          AssignFile({var}LogFile, m_KrnlDebugFilename);

        LogFileOpen := True;

        Rewrite({var}LogFile);
        LogMode := DM_FILE;
      except
        ShowMessage('Could not create log file');
        LogMode := DM_NONE;
      end;
  end; // case m_DxbxDebug
end;

procedure CloseLogs;
begin
  FreeAndNil({var}frm_LogConsole);

  if LogFileOpen then
  begin
    CloseFile({var}LogFile);
    LogFileOpen := False;
  end;

  LogMode := DM_NONE;
end;

procedure WriteLog(aText: string);
begin
  case LogMode of
    DM_CONSOLE:
      if Assigned(frm_LogConsole) then
        frm_LogConsole.Log.Lines.Add(aText);
    DM_FILE:
      if LogFileOpen then
        WriteLn({var}LogFile, aText);
  end;
end;

end.
