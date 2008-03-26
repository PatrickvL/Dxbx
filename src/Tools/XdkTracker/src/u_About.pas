unit u_About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, jpeg;

type
  Tfrm_About = class(TForm)
    Image1: TImage;
    Bevel1: TBevel;
    lbl_Version: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_About: Tfrm_About;

implementation

uses uConsts;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure Tfrm_About.FormCreate(Sender: TObject);
begin
  lbl_Version.Caption := 'Version: ' + _XDK_TRACKER_VERSION;
end; // Tfrm_About.FormCreate

//------------------------------------------------------------------------------

end.
