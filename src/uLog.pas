unit uLog;

interface

uses
  uEnums;

var
  m_DxbxDebug: DebugMode;
  m_DxbxDebugFilename: string;
  m_KrnlDebug: DebugMode;
  m_KrnlDebugFilename: string;

procedure CreateLogs(aLogType: LogType = ltKernel);
procedure CloseLogs;
procedure WriteLog(aText: string);


implementation

uses
  uLogConsole;

var
  LogFile : TextFile;

procedure CreateLogs(aLogType: LogType = ltKernel);
begin
  frm_LogConsole := Tfrm_LogConsole.Create(nil);
  case aLogType of
    ltGui    : frm_LogConsole.Caption := 'DXBX : Debug Console';
    ltKernel : frm_LogConsole.Caption := 'DXBX : Kernel Debug Console';
  end;
  frm_LogConsole.Show;
end;

procedure CloseLogs;
begin
  frm_LogConsole.Free;
End;

procedure WriteLog(aText: string);
begin
  frm_LogConsole.Log.Lines.Add(aText);
end;


end.
