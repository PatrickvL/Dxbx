unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, IniFiles;

type
  TMain = class(TForm)
    edtDelphiCompier: TEdit;
    edtSvnExecutable: TEdit;
    btnDelphiCompiler: TSpeedButton;
    btnSvnExecutable: TSpeedButton;
    cmbRevisions: TComboBox;
    memRevLog: TMemo;
    btnGetAndCompile: TButton;
    lblDelphiCompiler: TLabel;
    lblSvn: TLabel;
    btnRefreshRivisions: TSpeedButton;
    lblRevisions: TLabel;
    Bevel1: TBevel;
    lblRevLog: TLabel;
    Bevel2: TBevel;
    edtOutputPath: TEdit;
    btnOutputPath: TSpeedButton;
    lblOutputPath: TLabel;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnDelphiCompilerClick(Sender: TObject);
    procedure btnSvnExecutableClick(Sender: TObject);
  private
    { Private declarations }
    FDelphiCompiler: string;
    FSvn: string;
    FOutputPath: string;

    FApplicationDir: string;

    procedure ReadSettings;
    procedure SaveSettings;

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

procedure TMain.btnDelphiCompilerClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin

  end;
end;

procedure TMain.btnSvnExecutableClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin

  end;
end;

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  FApplicationDir := ExtractFilePath(Application.ExeName);
  ReadSettings;
end;

procedure TMain.ReadSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FApplicationDir + 'SVNBuilder.Ini');
  try
    FDelphiCompiler := IniFile.ReadString(C_SETTINGS, 'DelphiCompiler', '');
    FSvn := IniFile.ReadString(C_SETTINGS, 'SVN', '');
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
    IniFile.WriteString(C_SETTINGS, 'DelphiCompiler', FDelphiCompiler);
    IniFile.WriteString(C_SETTINGS, 'SVN', FSvn);
    IniFile.WriteString(C_SETTINGS, 'OutputPath', FOutputPath);
  finally
    FreeAndNil(IniFile);
  end;
end;

end.
