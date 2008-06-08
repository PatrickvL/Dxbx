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
  Windows,
  SysUtils,
  // Dxbx
  uTypes,
  uLogConsole;

var
  m_DxbxDebug: DebugMode = DM_NONE;
  m_DxbxDebugFilename: string = '';
  m_KrnlDebug: DebugMode = DM_CONSOLE;
  m_KrnlDebugFilename: string = '';

procedure CreateLogs(aLogType: TLogType = ltKernel);
procedure CloseLogs;
procedure WriteLog(aText: string);
procedure DbgPrintf(aStr: string);
procedure SetLogMode(aLogMode: DebugMode = DM_NONE); export;

implementation

var
  LogFileOpen: Boolean = False;
  LogFile: TextFile;

  DxbxLogLock: Windows._RTL_CRITICAL_SECTION;

procedure DbgPrintf(aStr: string);
begin
  WriteLog(aStr);
end;

procedure SetLogMode(aLogMode: DebugMode = DM_NONE); export;
begin
  WriteLog('SetLogMode(' + DebugModeToString(aLogMode) + ')');
  m_DxbxDebug := aLogMode;
end;

procedure CreateLogs(aLogType: TLogType);
var
  OutputFileName: string;
begin
  WriteLog('CreateLogs(' + LogTypeToString(aLogType) + ')');

  case m_DxbxDebug of
    DM_NONE:
      CloseLogs;

    DM_CONSOLE:
      if not Assigned(frm_LogConsole) then
      try
        frm_LogConsole := Tfrm_LogConsole.Create(nil);
        if aLogType = ltGui then
          frm_LogConsole.Caption := 'DXBX : Debug Console'
        else // ltKernel
          frm_LogConsole.Caption := 'DXBX : Kernel Debug Console';

        frm_LogConsole.Show;
      except
        m_DxbxDebug := DM_NONE;
        FreeAndNil({var}frm_LogConsole);
        raise Exception.Create('Could not create log console');
      end;

    DM_FILE:
      if not LogFileOpen then
      try
        if aLogType = ltGui then
          OutputFileName := m_DxbxDebugFilename
        else
          OutputFileName := m_KrnlDebugFilename;

        if OutputFileName = '' then
          OutputFileName := 'DxbxKrnlDebug.txt';

        AssignFile({var}LogFile, OutputFileName);

        Rewrite({var}LogFile);
        LogFileOpen := True;
      except
        m_DxbxDebug := DM_NONE;
        raise Exception.Create('Could not create log file');
      end;
  end; // case m_DxbxDebug

  WriteLog('Started logging.');
end;

procedure CloseLogs;
begin
  WriteLog('Stop logging.');
  FreeAndNil(frm_LogConsole);

  if LogFileOpen then
  begin
    CloseFile(LogFile);
    LogFileOpen := False;
  end;

  m_DxbxDebug := DM_NONE;
end;

procedure WriteLog(aText: string);

  function _Text: string;
  begin
    Result := '[0x' + IntToHex(GetCurrentThreadID(), 4) + '] ' + aText;
  end;

begin
  EnterCriticalSection({var}DxbxLogLock);

  case m_DxbxDebug of
    DM_CONSOLE:
      if Assigned(frm_LogConsole) then
        frm_LogConsole.Log.Lines.Add(_Text());
    DM_FILE:
      if LogFileOpen then
      begin
        WriteLn({var}LogFile, _Text());
        Flush({var}LogFile);
      end;
  end;
  
  LeaveCriticalSection({var}DxbxLogLock);
end;

initialization

  InitializeCriticalSection({var}DxbxLogLock);

finalization

  CloseLogs;

end.
