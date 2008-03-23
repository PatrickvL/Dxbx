program DxbxLogger;

uses
  Forms,
  uLogConsole in 'uLogConsole.pas' {frm_LogConsole},
  uKrnlDbgConsole in 'uKrnlDbgConsole.pas' {frm_KnlDbgLog},
  uGuiDbgConsole in 'uGuiDbgConsole.pas' {frm_GuiDbgLog};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tfrm_KnlDbgLog, frm_KnlDbgLog);
  Application.Run;
end.
