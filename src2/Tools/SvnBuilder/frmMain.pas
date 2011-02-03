unit frmMain;

interface

{$WARN UNIT_PLATFORM OFF} // Prevent warning on usage of FileCtrl

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
    procedure btnRefreshRivisionsClick(Sender: TObject);
  private
    { Private declarations }
    SvnClient: TSvnClient;
    FOutputPath: string;

    FApplicationDir: string;

    function GetRevisionNm: Integer;
    function isValidRevision(RevisionNm: Integer): Boolean;

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
  C_DXBXPATH = 'http://dxbx.svn.sourceforge.net/svnroot/dxbx';

{ TMain }

procedure TMain.btnGetAndCompileClick(Sender: TObject);
begin
  if DirectoryExists(FOutputPath) then
  begin
    if isValidRevision(GetRevisionNm) then
      SvnClient.Checkout(C_DXBXPATH,
                         FOutputPath,
                         nil,
                         True,
                         False,
                         GetRevisionNm)
    else
      MessageDlg('No valid revision inserted!', mtWarning, [mbOk], 0);
  end
  else
    MessageDlg('Output path not found!', mtWarning, [mbOk], 0);
end;

procedure TMain.btnOutputPathClick(Sender: TObject);
var
  options : TSelectDirOpts;
begin
  SelectDirectory(FOutputPath, options, 0);
end;

procedure TMain.btnRefreshRivisionsClick(Sender: TObject);
begin
  GetSvnRevisionList;
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

function TMain.GetRevisionNm: Integer;
begin
  Result := StrToInt(cmbRevisions.Text);
end;

procedure TMain.GetSvnRevisionList;
var
  RevisionNm: Integer;
  i: integer;
begin
  cmbRevisions.Clear;

  if SvnClient.IsPathVersioned(C_DXBXPATH) then
  begin
    RevisionNm := SvnClient.GetPathRevision(C_DXBXPATH).Value.number;

    // Todo, make this a configuration option, how much history
    for i := RevisionNm downto RevisionNm - 100 do
    begin
      cmbRevisions.AddItem(IntToStr(RevisionNm), nil);
    end;
  end;
end;

function TMain.isValidRevision(RevisionNm: Integer): Boolean;
begin
  // Todo: create real revision nm check
  Result := True;
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
