{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
unit u_AddError;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, DB, ADODB, ComCtrls;

type
  Tfrm_AddError = class(TForm)
    Bevel1: TBevel;
    lbl_KnownErros: TLabel;
    lst_Errors: TListBox;
    lbl_ErrorNumber: TLabel;
    edt_ErrorNumber: TEdit;
    mem_Description: TMemo;
    lbl_When: TLabel;
    lbl_cxbxlog: TLabel;
    edt_cxbxlog: TEdit;
    lbl_krnllog: TLabel;
    edt_krnllog: TEdit;
    btn_cxbxlog: TButton;
    btn_krnllog: TButton;
    mem_ErrorDesc: TMemo;
    btn_Cancel: TButton;
    btn_Ok: TButton;
    lbl_WhenDesc: TLabel;
    CheckBox1: TCheckBox;
    DateTimePicker1: TDateTimePicker;
    Label1: TLabel;
    Bevel2: TBevel;
    procedure btn_cxbxlogClick(Sender: TObject);
    procedure btn_krnllogClick(Sender: TObject);
    procedure lst_ErrorsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Game : String;
  end;

var
  frm_AddError: Tfrm_AddError;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure Tfrm_AddError.btn_cxbxlogClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    edt_cxbxlog.Text := OpenDialog1.FileName;
  end;
end;

//------------------------------------------------------------------------------

procedure Tfrm_AddError.btn_krnllogClick(Sender: TObject);
begin
  if OpenDialog2.Execute then
  begin
    edt_krnllog.Text := OpenDialog2.FileName;
  end;
end;

//------------------------------------------------------------------------------

procedure Tfrm_AddError.lst_ErrorsClick(Sender: TObject);
Var Cnt : Integer;
begin
  ADOQuery.Close;
  AdoQuery.SQL.Clear;
  AdoQuery.SQL.Add( 'Select * from Error where game=:game and ErrorNumber=:ErrorNumber' );
  with AdoQuery.Parameters do
  begin
    ParamByName ( 'Game' ).Value := Game;
    for Cnt := 0 to lst_Errors.Count - 1 do
    begin
      if lst_Errors.Selected [ Cnt ] then
        ParamByName ( 'ErrorNumber' ).Value := lst_Errors.Items.Strings [ Cnt ];
    end;
  end;

  AdoQuery.Open;
  mem_ErrorDesc.Text := ADOQuery.FieldByName ( 'Description' ).AsString;

end;
 
//------------------------------------------------------------------------------

end.
