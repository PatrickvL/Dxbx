unit u_AddGame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type
  Tfrm_AddGame = class(TForm)
    Bevel2: TBevel;
    lbl_GameName: TLabel;
    edt_GameName: TEdit;
    lbl_GameType: TLabel;
    cmb_GameType: TComboBox;
    lbl_XdkVersion: TLabel;
    btn_Ok: TButton;
    bn_Cancel: TButton;
    cmb_XdkVersion: TComboBox;
    procedure btn_OkClick(Sender: TObject);
    procedure cmb_XdkVersionKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    function isAllFilled : Boolean;
  end;

var
  frm_AddGame: Tfrm_AddGame;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure Tfrm_AddGame.btn_OkClick(Sender: TObject);
begin
  if isAllFilled then begin
    ModalResult := mrOk; 
  end
  else begin
    MessageDlg ( 'Not all items are filled.', mtError, [mbOk], 0 );
  end;
end;

//------------------------------------------------------------------------------

function Tfrm_AddGame.isAllFilled: Boolean;
begin
  Result := False;  
end;

//------------------------------------------------------------------------------

procedure Tfrm_AddGame.cmb_XdkVersionKeyPress(Sender: TObject;
  var Key: Char);
begin
  Key := #0;
end;

//------------------------------------------------------------------------------

end.
