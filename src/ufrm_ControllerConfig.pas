unit ufrm_ControllerConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  Tfrm_ControllerConfig = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    btn_X: TButton;
    btn_Y: TButton;
    btn_A: TButton;
    btn_B: TButton;
    btn_White: TButton;
    btn_Black: TButton;
    btn_LeftTrigger: TButton;
    btn_RightTrigger: TButton;
    btn_LeftUp: TButton;
    btn_LeftDown: TButton;
    btn_LeftLeft: TButton;
    btn_LeftRight: TButton;
    btn_DPadUp: TButton;
    btn_DPadDown: TButton;
    btn_DPadLeft: TButton;
    btn_DPadRight: TButton;
    btn_Back: TButton;
    btn_Start: TButton;
    btnLeftThump: TButton;
    btn_RightThump: TButton;
    btn_RightUp: TButton;
    btn_RightDown: TButton;
    btn_RightLeft: TButton;
    btn_RightRight: TButton;
    btn_Accept: TButton;
    btn_Cancel: TButton;
    btn_LoadConfig: TButton;
    btn_SaveConfig: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  frm_ControllerConfig: Tfrm_ControllerConfig;

implementation

{$R *.DFM}



end.
