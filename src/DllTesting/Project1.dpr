program Project1;

uses
  Forms,
  ufrm_Main in 'ufrm_Main.pas' {Form1},
  uXbe in '..\uXbe.pas',
  uLog in '..\uLog.pas',
  BitsOps in '..\BitsOps.pas',
  CTime in '..\CTime.pas',
  uConsts in '..\uConsts.pas',
  uEmu in '..\uEmu.pas',
  uEmuExe in '..\uEmuExe.pas',
  uEmuFS in '..\uEmuFS.pas',
  uEmuShared in '..\uEmuShared.pas',
  uEnums in '..\uEnums.pas',
  uExe in '..\uExe.pas',
  ufrm_About in '..\ufrm_About.pas' {frm_About},
  ufrm_ControllerConfig in '..\ufrm_ControllerConfig.pas' {frm_ControllerConfig},
  uLogConsole in '..\uLogConsole.pas' {frm_LogConsole},
  uProlog in '..\uProlog.pas',
  uMutex in '..\uMutex.pas',
  uExternals in '..\uExternals.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(Tfrm_About, frm_About);
  Application.CreateForm(Tfrm_ControllerConfig, frm_ControllerConfig);
  Application.CreateForm(Tfrm_LogConsole, frm_LogConsole);
  Application.Run;
end.
