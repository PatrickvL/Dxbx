unit uLog;

interface

uses
  // Dxbx
  uEnums;

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
  case aLogType of
    ltGui    : begin
                 case m_DxbxDebug of
                   DM_NONE    : begin
                                  CloseLogs;
                                end;
                   DM_CONSOLE : begin
                                  try
                                    frm_LogConsole := Tfrm_LogConsole.Create(nil);
                                    frm_LogConsole.Caption := 'DXBX : Debug Console';
                                    frm_LogConsole.Show;
                                    LogMode := DM_CONSOLE;
                                  finally
                                  end;
                                end;
                   DM_FILE    : begin
                                  try
                                    AssignFile(LogFile, m_DxbxDebugFilename);
                                    Rewrite(LogFile);
                                    LogFileOpen := True;
                                    LogMode := DM_FILE;
                                  except
                                    ShowMessage('Could not create log file');
                                    LogMode := DM_NONE;
                                  end;
                                end;
                 end;
               end;
    ltKernel : begin
                 case m_KrnlDebug of
                   DM_NONE    : begin
                                  CloseLogs;
                                end;
                   DM_CONSOLE : begin
                                  frm_LogConsole := Tfrm_LogConsole.Create(nil);
                                  frm_LogConsole.Caption := 'DXBX : Kernel Debug Console';
                                  frm_LogConsole.Show;
                                  LogMode := DM_CONSOLE;
                                end;
                   DM_FILE    : begin
                                  try
                                    AssignFile(LogFile, m_KrnlDebugFilename);
                                    Rewrite(LogFile);
                                    LogFileOpen := True;
                                    LogMode := DM_FILE;
                                  except
                                    ShowMessage('Could not create log file');
                                    LogMode := DM_NONE;
                                  end;
                                end;
                 end;
               end;
  end;
end;

procedure CloseLogs;
begin
  if Assigned(frm_LogConsole) then
    frm_LogConsole.Close;

  if LogFileOpen then
  begin
    CloseFile(LogFile);
    LogFileOpen := False;
  end;

  LogMode := DM_NONE;
end;

procedure WriteLog(aText: string);
begin
  case LogMode of
    DM_CONSOLE: frm_LogConsole.Log.Lines.Add(aText);
    DM_FILE: WriteLn(LogFile, aText);
  end;
end;

end.
