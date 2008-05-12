unit ufrm_About;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  Tfrm_About = class(TForm)
    Label1: TLabel;
    lblAbout: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_About: Tfrm_About;

implementation

uses
  // Dxbx
  uConsts;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure Tfrm_About.FormCreate(Sender: TObject);
begin
  Label1.Caption := Label1.Caption + ' ' + _DXBX_VERSION;
end; // Tfrm_About.FormCreate

//------------------------------------------------------------------------------

end.
