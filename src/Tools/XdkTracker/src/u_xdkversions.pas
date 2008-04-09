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
    procedure FillGameList;
    procedure cmb_gametypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

var
  FilteredList : Tlist;

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.cmb_gametypeChange(Sender: TObject);
begin
  FillGameList;
end; // Tfrm_Xdkversion.cmb_gametypeChange

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.FillGameList;
var
  lIndex : Integer;
begin
  FilteredList.Clear;
  for lIndex := 0 to GameList.count - 1 do begin
    if (cmb_gametype.ItemIndex = 0) or
       ((PXDKInfo(GameList.Items[lIndex])^.XAPILIB = cmb_gametype.Text) or
       (PXDKInfo(GameList.Items[lIndex])^.XBOXKRNL = cmb_gametype.Text) or
       (PXDKInfo(GameList.Items[lIndex])^.LIBCMT = cmb_gametype.Text) or
       (PXDKInfo(GameList.Items[lIndex])^.D3D8 = cmb_gametype.Text) or
       (PXDKInfo(GameList.Items[lIndex])^.XGRAPHC = cmb_gametype.Text) or
       (PXDKInfo(GameList.Items[lIndex])^.DSOUND = cmb_gametype.Text) or
       (PXDKInfo(GameList.Items[lIndex])^.XMV = cmb_gametype.Text))then
    begin
      FilteredList.Add(Gamelist.Items[lIndex]);
    end;
  end;
  lst_Games.Clear;
  mem_XdkVersions.Clear;
  for lIndex := 0 to FilteredList.count - 1 do begin
      lst_Games.Items.Add(PXDKInfo(FilteredList.Items[lIndex])^.GameName);
  end;
end; // Tfrm_Xdkversion.FillGameList

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.FormCreate(Sender: TObject);
begin
  FilteredList := TList.Create;
end; // Tfrm_Xdkversion.FormCreate

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
    mem_XdkVersions.Lines.Add( 'XAPILIB  : ' + PXDKInfo ( FilteredList.Items[ lst_Games.ItemIndex ] )^.XAPILIB);
    mem_XdkVersions.Lines.Add( 'XBOXKRNL : ' + PXDKInfo ( FilteredList.Items[ lst_Games.ItemIndex ] )^.XBOXKRNL);
    mem_XdkVersions.Lines.Add( 'LIBCMT   : ' + PXDKInfo ( FilteredList.Items[ lst_Games.ItemIndex ] )^.LIBCMT);
    mem_XdkVersions.Lines.Add( 'D3D8     : ' + PXDKInfo ( FilteredList.Items[ lst_Games.ItemIndex ] )^.D3D8);
    mem_XdkVersions.Lines.Add( 'XGRAPHC  : ' + PXDKInfo ( FilteredList.Items[ lst_Games.ItemIndex ] )^.XGRAPHC);
    mem_XdkVersions.Lines.Add( 'DSOUND   : ' + PXDKInfo ( FilteredList.Items[ lst_Games.ItemIndex ] )^.DSOUND);
    mem_XdkVersions.Lines.Add( 'XMV      : ' + PXDKInfo ( FilteredList.Items[ lst_Games.ItemIndex ] )^.XMV);
  end;
end; // Tfrm_Xdkversion.ShowXdkInfo

//------------------------------------------------------------------------------

end.
