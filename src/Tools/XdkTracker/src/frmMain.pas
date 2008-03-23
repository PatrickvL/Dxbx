unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, ShellApi;

type
  Tfrm_Main = class(TForm)
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Preferences1: TMenuItem;
    Viewxdkversion1: TMenuItem;
    Viewxdkversion2: TMenuItem;
    N3: TMenuItem;
    Gameerrors1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    VisitShadowTjwebsite1: TMenuItem;
    VisitCaustikswebsite1: TMenuItem;
    VisitCxbxForum1: TMenuItem;
    N2: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Preferences1Click(Sender: TObject);
    procedure Viewxdkversion2Click(Sender: TObject);
    procedure Gameerrors1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure VisitShadowTjwebsite1Click(Sender: TObject);
    procedure VisitCaustikswebsite1Click(Sender: TObject);
    procedure VisitCxbxForum1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_Main: Tfrm_Main;

implementation

uses u_Main, u_Preferences, u_xdkversions, u_AddGame, ADODB, u_GameErrors,
  u_About;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure Tfrm_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frm_login.close;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.FormDestroy(Sender: TObject);
begin
  frm_login.close;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.Exit1Click(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.Preferences1Click(Sender: TObject);
begin
  frm_Preferences := Tfrm_Preferences.Create ( Application );

  with frm_Preferences do
  begin
    edt_Username.Text := frm_Login.UserName;
    edt_password.Text := frm_Login.Password;

    // Get email address
    with frm_Login do
    begin
      adoquery.close;
      adoquery.sql.clear;
      adoquery.SQL.Add( 'Select * from login where username=:username and password=:password' );
      adoquery.Parameters.ParamByName( 'username' ).Value := username;
      adoquery.Parameters.ParamByName( 'password' ).Value := password;
      adoquery.Open;

      Email := adoquery.FieldByName ( 'Email' ).asString;
      edt_Email.Text := Email;
    end;

  end;

  if frm_Preferences.ShowModal = mrOk then
  begin

    with frm_Login do
    begin
      Adoquery.Close;
      Adoquery.SQL.Clear;
      Adoquery.SQL.Add ( 'Update login set Password=:Password, Email=:Email where username=:username' );
      adoquery.Parameters.ParamByName( 'Password' ).Value := frm_Preferences.edt_password.Text;
      Adoquery.Parameters.ParamByName( 'Email' ).Value    := frm_Preferences.edt_Email.Text;
      AdoQuery.Parameters.ParamByName( 'Username' ).Value := frm_Login.UserName;
      ADOQuery.ExecSQL;
    end;
  end;

  frm_Preferences.free;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.Viewxdkversion2Click(Sender: TObject);
begin
  frm_xdkversion := Tfrm_xdkversion.Create ( Application );

  if frm_XdkVersion.ShowModal = mrOk then
  begin

  end;

  frm_XdkVersion.Free;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.Gameerrors1Click(Sender: TObject);
begin
  frm_ErrorLogging := Tfrm_ErrorLogging.Create ( Application );

  with frm_ErrorLogging do
  begin
   // lst_GameList.refresh( False );
   // lst_Gamelist.Column [0].AutoSize := True;
  end;

  if frm_ErrorLogging.ShowModal = mrOk then
  begin
  end;

  frm_ErrorLogging.Free;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.About1Click(Sender: TObject);
begin
  frm_About := Tfrm_About.Create ( Application );

  with frm_About do
   // lbl_VersionNm.Caption := GetBuildInfo;

  if frm_About.ShowModal = mrOk then
  begin
  end;

  frm_About.free;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.VisitShadowTjwebsite1Click(Sender: TObject);
begin
  ShellExecute(0,'open','http://home.planet.nl/~woute596',nil,nil,SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.VisitCaustikswebsite1Click(Sender: TObject);
begin
  ShellExecute(0,'open','http://www.caustik.com/cxbx/',nil,nil,SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.VisitCxbxForum1Click(Sender: TObject);
begin
  ShellExecute(0,'open','http://www2.ngemu.com/forums/forumdisplay.php?f=63',nil,nil,SW_SHOWNORMAL);
end;
  
//------------------------------------------------------------------------------

end.
