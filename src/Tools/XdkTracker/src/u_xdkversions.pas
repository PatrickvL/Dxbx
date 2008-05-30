(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2007 Shadow_tj and other members of the development team.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
unit u_xdkversions;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, DB,
  ADODB, Buttons,
  // Dxbx
  u_AddGame,
  uData;


type
  Tfrm_Xdkversion = class(TForm)
    Bevel1: TBevel;
    cmb_gametype: TComboBox;
    lbl_Gametype: TLabel;
    lbl_Xdk: TLabel;
    mem_XdkVersions: TMemo;
    lst_Games: TListBox;
    procedure lst_GamesClick(Sender: TObject);
    procedure cmb_gametypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure FillGameList;
    procedure ShowXdkInfo;
  end;

var
  GameList: TList;
  frm_Xdkversion: Tfrm_Xdkversion;

implementation

{$R *.dfm}

var
  FilteredList: Tlist;

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.cmb_gametypeChange(Sender: TObject);
begin
  FillGameList;
end; // Tfrm_Xdkversion.cmb_gametypeChange

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.FillGameList;
var
  lIndex: Integer;
begin
  FilteredList.Clear;
  for lIndex := 0 to GameList.Count - 1 do
  begin
    if (cmb_gametype.ItemIndex = 0)
    or (PXDKInfo(GameList.Items[lIndex])^.XAPILIB = cmb_gametype.Text)
    or (PXDKInfo(GameList.Items[lIndex])^.XBOXKRNL = cmb_gametype.Text)
    or (PXDKInfo(GameList.Items[lIndex])^.LIBCMT = cmb_gametype.Text)
    or (PXDKInfo(GameList.Items[lIndex])^.D3D8 = cmb_gametype.Text)
    or (PXDKInfo(GameList.Items[lIndex])^.XGRAPHC = cmb_gametype.Text)
    or (PXDKInfo(GameList.Items[lIndex])^.DSOUND = cmb_gametype.Text)
    or (PXDKInfo(GameList.Items[lIndex])^.XMV = cmb_gametype.Text) then
      FilteredList.Add(Gamelist.Items[lIndex]);
  end;

  lst_Games.Clear;
  mem_XdkVersions.Clear;
  for lIndex := 0 to FilteredList.Count - 1 do
    lst_Games.Items.Add(PXDKInfo(FilteredList.Items[lIndex])^.GameName);
end; // Tfrm_Xdkversion.FillGameList

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.FormCreate(Sender: TObject);
begin
  FilteredList := TList.Create;
end; // Tfrm_Xdkversion.FormCreate

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.lst_GamesClick(Sender: TObject);
begin
  ShowXdkInfo();
end; // Tfrm_Xdkversion.lst_GamesClick

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.ShowXdkInfo;
begin
  mem_XdkVersions.Clear;
  if lst_Games.ItemIndex <> -1 then
  begin
    mem_XdkVersions.Lines.Add('XAPILIB  : ' + PXDKInfo(FilteredList.Items[lst_Games.ItemIndex])^.XAPILIB);
    mem_XdkVersions.Lines.Add('XBOXKRNL : ' + PXDKInfo(FilteredList.Items[lst_Games.ItemIndex])^.XBOXKRNL);
    mem_XdkVersions.Lines.Add('LIBCMT   : ' + PXDKInfo(FilteredList.Items[lst_Games.ItemIndex])^.LIBCMT);
    mem_XdkVersions.Lines.Add('D3D8     : ' + PXDKInfo(FilteredList.Items[lst_Games.ItemIndex])^.D3D8);
    mem_XdkVersions.Lines.Add('XGRAPHC  : ' + PXDKInfo(FilteredList.Items[lst_Games.ItemIndex])^.XGRAPHC);
    mem_XdkVersions.Lines.Add('DSOUND   : ' + PXDKInfo(FilteredList.Items[lst_Games.ItemIndex])^.DSOUND);
    mem_XdkVersions.Lines.Add('XMV      : ' + PXDKInfo(FilteredList.Items[lst_Games.ItemIndex])^.XMV);
  end;
end; // Tfrm_Xdkversion.ShowXdkInfo

//------------------------------------------------------------------------------

end.
