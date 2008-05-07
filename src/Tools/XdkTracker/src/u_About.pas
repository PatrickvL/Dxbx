unit u_About;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, JPeg;

type
  Tfrm_About = class(TForm)
    Image1: TImage;
    Bevel1: TBevel;
    lbl_Version: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
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
  lbl_Version.Caption := 'Version: ' + _XDK_TRACKER_VERSION;
end; // Tfrm_About.FormCreate

//------------------------------------------------------------------------------

end.
