program SvnBuilder;

uses
  Forms,
  frmMain in '..\..\src2\Tools\SvnBuilder\frmMain.pas' {Main},
  SvnClient in '..\..\Libraries\DelphiSvn\SvnClient.pas',
  apr in '..\..\Libraries\DelphiSvn\apr.pas',
  svn_client in '..\..\Libraries\DelphiSvn\svn_client.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
