program Dxbx;

uses
  Forms,
  ufrm_Main in 'ufrm_Main.pas' {frm_Main},
  ufrm_ControllerConfig in 'ufrm_ControllerConfig.pas' {frm_ControllerConfig},
  ufrm_VideoConfig in 'ufrm_VideoConfig.pas' {frm_VideoConfig},
  uXbe in 'uXbe.pas',
  uEmuExe in 'uEmuExe.pas',
  uEnums in 'uEnums.pas',
  uExe in 'uExe.pas',
  uConsts in 'uConsts.pas',
  ufrm_About in 'ufrm_About.pas' {frm_About},
  CTime in 'CTime.pas',
  uProlog in 'uProlog.pas',
  uXML in 'uXML.pas',
  g_EmuShared in 'g_EmuShared.pas',
  uLog in 'uLog.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(Tfrm_Main, frm_Main);
  Application.Run;
end.

