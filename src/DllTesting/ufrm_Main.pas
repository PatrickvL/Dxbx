unit ufrm_Main;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  // Dxbx
  uExternals, uXbe, uEnums, uEmu;

type
  TForm1 = class(TForm)
    Button1: TButton;
    XbeOpenDialog: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure OpenXbeFile(aFileName: string);
    procedure CloseXbe;
  public
    { Public declarations }
    m_Xbe: TXbe;
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  XbeOpenDialog.Filter := 'Xbox Executables (*.xbe)|*.xbe';
  if XbeOpenDialog.Execute then
  begin
    OpenXbeFile(XbeOpenDialog.FileName);
    CxbxKrnlInit(0, nil, nil, nil, DM_CONSOLE, nil, 0, 0, nil);
  end;
end;

procedure TForm1.CloseXbe;
begin
  FreeAndNil(m_Xbe);
end;

procedure TForm1.OpenXbeFile(aFileName: string);
begin
  if Assigned(m_Xbe) then
    CloseXbe();

  m_Xbe := TXbe.Create(aFileName, ftXbe);
end;

end.
