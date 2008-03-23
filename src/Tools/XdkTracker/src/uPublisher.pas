unit uPublisher;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  Tfrm_Publisher = class(TForm)
    Bevel1: TBevel;
    lbl_Publisher: TLabel;
    edtPublisher: TEdit;
    btn_Cancel: TButton;
    btn_Ok: TButton;
    procedure edtPublisherKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_Publisher: Tfrm_Publisher;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure Tfrm_Publisher.edtPublisherKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and (edtPublisher.Text <> '') then begin
    Self.ModalResult := mrOk;
    Self.CloseModal;
  end;
  if (Key = #27) then begin
    Self.ModalResult := mrCancel;
    Self.CloseModal;
  end;
end; // Tfrm_Publisher.edtPublisherKeyPress

//------------------------------------------------------------------------------

end.
