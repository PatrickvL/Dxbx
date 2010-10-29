unit frmMain;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, IniFiles, FileCtrl,
  // Dxbx
  SvnClient
  ;

type
  TMain = class(TForm)
    cmbRevisions: TComboBox;
    memRevLog: TMemo;
    btnGetAndCompile: TButton;
    btnRefreshRivisions: TSpeedButton;
    lblRevisions: TLabel;
    Bevel1: TBevel;
    lblRevLog: TLabel;
    edtOutputPath: TEdit;
    btnOutputPath: TSpeedButton;
    lblOutputPath: TLabel;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOutputPathClick(Sender: TObject);
    procedure btnGetAndCompileClick(Sender: TObject);
  private
    { Private declarations }
    SvnClient: TSvnClient;

    FDelphiCompiler: string;
    FSvn: string;
    FOutputPath: string;

    FApplicationDir: string;

    procedure ReadSettings;
    procedure SaveSettings;

    procedure GetSvnRevisionList;

  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

const
  C_SETTINGS = 'Settings';

{ TMain }

procedure TMain.btnGetAndCompileClick(Sender: TObject);
begin
  if DirectoryExists(FOutputPath) then
    SvnClient.Checkout('http://dxbx.svn.sourceforge.net/svnroot/dxbx',
                       FOutputPath);
  else
    MessageDlg('Output path not found!', mtWarning, [mbOk], 0);
end;

procedure TMain.btnOutputPathClick(Sender: TObject);
var
  options : TSelectDirOpts;
  chosenDirectory : string;
begin
  SelectDirectory(FOutputPath, options, 0);
end;

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
  FreeAndNil(SvnClient);
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  FApplicationDir := ExtractFilePath(Application.ExeName);
  SvnClient := TSvnClient.Create;

  ReadSettings;
  GetSvnRevisionList;
end;

procedure TMain.GetSvnRevisionList;
begin
end;

procedure TMain.ReadSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FApplicationDir + 'SVNBuilder.Ini');
  try
    FOutputPath := IniFile.ReadString(C_SETTINGS, 'OutputPath', '');
  finally
    FreeAndNil({var}IniFile);
  end;
end;

procedure TMain.SaveSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FApplicationDir + 'SVNBuilder.Ini');
  try
    IniFile.WriteString(C_SETTINGS, 'OutputPath', FOutputPath);
  finally
    FreeAndNil(IniFile);
  end;
end;

end.
