program XdkTracker;

uses
  Forms,
  u_About in 'u_About.pas' {frm_About},
  ufrm_Main in 'ufrm_Main.pas' {frmXdkTracker},
  u_xdkversions in 'u_xdkversions.pas' {frm_Xdkversion},
  uPublisher in 'uPublisher.pas' {frm_Publisher},
  uImportGames in 'uImportGames.pas' {frm_ImportGames},
  uData in 'uData.pas',
  uConsts in '..\..\..\uConsts.pas',
  uXML in '..\..\..\uXML.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'XDK Tracker';
  Application.CreateForm(TfrmXdkTracker, frmXdkTracker);
  Application.Run;
end.
