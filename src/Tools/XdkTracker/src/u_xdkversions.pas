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
  SysUtils, Classes, Contnrs, Controls, StdCtrls, ExtCtrls, Forms, 
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
  protected
    GameList: TObjectList;
  public
    procedure FillGameList(const aGameList: TObjectList);
    procedure ShowXdkInfo;
  end;

var
  frm_Xdkversion: Tfrm_Xdkversion;

implementation

{$R *.dfm}

var
  FilteredList: Tlist;

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.cmb_gametypeChange(Sender: TObject);
begin
  if Assigned(GameList) then
    FillGameList(GameList);
end; // Tfrm_Xdkversion.cmb_gametypeChange

//------------------------------------------------------------------------------

procedure Tfrm_Xdkversion.FillGameList(const aGameList: TObjectList);
var
  lIndex: Integer;
begin
  GameList := aGameList;
  FilteredList.Clear;
  for lIndex := 0 to GameList.Count - 1 do
  begin
    if (cmb_gametype.ItemIndex = 0)
    or TXDKInfo(GameList[lIndex]).MatchesVersion(cmb_gametype.Text) then
      FilteredList.Add(Gamelist.Items[lIndex]);
  end;

  lst_Games.Clear;
  mem_XdkVersions.Clear;
  for lIndex := 0 to FilteredList.Count - 1 do
    lst_Games.Items.Add(TXDKInfo(FilteredList.Items[lIndex]).GameName);
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
var
  XDKInfo: TXDKInfo;
  i: Integer;
begin
  mem_XdkVersions.Clear;
  if lst_Games.ItemIndex <> -1 then
  begin
    XDKInfo := TXDKInfo(FilteredList.Items[lst_Games.ItemIndex]);
    Assert(Assigned(XDKInfo));
    
    for i := 0 to XDKInfo.LibVersions.Count - 1 do
      mem_XdkVersions.Lines.Add(Format('%-8s : %s', [
        XDKInfo.LibVersions.Names[i],
        XDKInfo.LibVersions.ValueFromIndex[i]]));
  end;
end; // Tfrm_Xdkversion.ShowXdkInfo

//------------------------------------------------------------------------------

end.
