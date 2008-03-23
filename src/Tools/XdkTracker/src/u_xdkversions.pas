unit u_xdkversions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, DB,
  ADODB, Buttons, ufrm_Main;

type
  Tfrm_Xdkversion = class(TForm)
    Bevel1: TBevel;
    cmb_gametype: TComboBox;
    lbl_Gametype: TLabel;
    lbl_Xdk: TLabel;
    mem_XdkVersions: TMemo;
    lst_Games: TListBox;
    procedure lst_GamesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowXdkInfo;
  end;

var
  frm_Xdkversion: Tfrm_Xdkversion;

implementation

uses u_AddGame, uData;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.lst_GamesClick(Sender: TObject);
begin
  ShowXdkInfo;
end; // Tfrm_Xdkversion.lst_GamesClick

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.ShowXdkInfo;
begin
  mem_XdkVersions.Clear;
  if lst_Games.ItemIndex <> -1 then begin
    mem_XdkVersions.Lines.Add( 'XAPILIB  : ' + PXDKInfo ( GameList.Items[ lst_Games.ItemIndex ] )^.XAPILIB);
    mem_XdkVersions.Lines.Add( 'XBOXKRNL : ' + PXDKInfo ( GameList.Items[ lst_Games.ItemIndex ] )^.XBOXKRNL);
    mem_XdkVersions.Lines.Add( 'LIBCMT   : ' + PXDKInfo ( GameList.Items[ lst_Games.ItemIndex ] )^.LIBCMT);
    mem_XdkVersions.Lines.Add( 'D3D8     : ' + PXDKInfo ( GameList.Items[ lst_Games.ItemIndex ] )^.D3D8);
    mem_XdkVersions.Lines.Add( 'XGRAPHC  : ' + PXDKInfo ( GameList.Items[ lst_Games.ItemIndex ] )^.XGRAPHC);
    mem_XdkVersions.Lines.Add( 'DSOUND   : ' + PXDKInfo ( GameList.Items[ lst_Games.ItemIndex ] )^.DSOUND);
    mem_XdkVersions.Lines.Add( 'XMV      : ' + PXDKInfo ( GameList.Items[ lst_Games.ItemIndex ] )^.XMV);
  end;
end; // Tfrm_Xdkversion.ShowXdkInfo

//------------------------------------------------------------------------------

end.
