unit uImportGames;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls, ExtCtrls;

type
  Tfrm_ImportGames = class(TForm)
    Bevel1: TBevel;
    btn_Cancel: TButton;
    btn_Ok: TButton;
    lbl_Publisher: TLabel;
    edt_Publisher: TEdit;
    lst_Import: TListView;
    lbl_NewGames: TLabel;
    lbl_Date: TLabel;
    edt_Date: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_ImportGames: Tfrm_ImportGames;

implementation

{$R *.dfm}

end.
