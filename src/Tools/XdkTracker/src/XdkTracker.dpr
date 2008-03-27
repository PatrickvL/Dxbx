program XdkTracker;

uses
  Forms,
  u_About in 'u_About.pas' {frm_About},
  ufrm_Main in 'ufrm_Main.pas' {frmXdkTracker},
  u_xdkversions in 'u_xdkversions.pas' {frm_Xdkversion},
  uData in 'uData.pas',
  uPublisher in 'uPublisher.pas' {frm_Publisher},
  uImportGames in 'uImportGames.pas' {frm_ImportGames},
  uXML in '..\..\..\uXML.pas',
  uConsts in '..\..\..\uConsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'XDK Tracker';
  Application.CreateForm(TfrmXdkTracker, frmXdkTracker);
  Application.Run;
end.
