unit u_About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, jpeg;

type
  Tfrm_About = class(TForm)
    Image1: TImage;
    Bevel1: TBevel;
    mem_Credits: TMemo;
    lbl_Version: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_About: Tfrm_About;

implementation

Uses
  uConst;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure Tfrm_About.FormCreate(Sender: TObject);
begin
  lbl_Version.Caption := 'Version: ' + version;
end; // Tfrm_About.FormCreate

//------------------------------------------------------------------------------

end.
