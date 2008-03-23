program XdkTracker;

uses
  Forms,
  u_About in 'u_About.pas' {frm_About},
  ufrm_Main in 'ufrm_Main.pas' {frmMain},
  u_xdkversions in 'u_xdkversions.pas' {frm_Xdkversion},
  uConst in 'uConst.pas',
  uData in 'uData.pas',
  uPublisher in 'uPublisher.pas' {frm_Publisher},
  uImportGames in 'uImportGames.pas' {frm_ImportGames},
  uXML in '..\..\..\uXML.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'XDK Tracker';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(Tfrm_Publisher, frm_Publisher);
  Application.CreateForm(Tfrm_ImportGames, frm_ImportGames);
  Application.Run;
end.
