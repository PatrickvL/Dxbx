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
    MyFilteredXBEList: TStringList;
    FXBEList: TStringList;
  public
    procedure FillXBEList(const aXBEList: TStringList);
    procedure ShowXBEInfo(const aXBEInfo: TXBEInfo);
  end;

var
  frm_Xdkversion: Tfrm_Xdkversion;

implementation

{$R *.dfm}

{ Tfrm_Xdkversion }

procedure Tfrm_Xdkversion.FormCreate(Sender: TObject);
begin
  MyFilteredXBEList := TStringList.Create;
end;

procedure Tfrm_Xdkversion.cmb_gametypeChange(Sender: TObject);
begin
  if Assigned(FXBEList) then
    FillXBEList(FXBEList);
end;

procedure Tfrm_Xdkversion.lst_GamesClick(Sender: TObject);
var
  XBEInfo: TXBEInfo;
begin
  if lst_Games.ItemIndex <> -1 then
  begin
    XBEInfo := TXBEInfo(MyFilteredXBEList.Objects[lst_Games.ItemIndex]);
    Assert(Assigned(XBEInfo));

    ShowXBEInfo(XBEInfo);
  end;
end;

procedure Tfrm_Xdkversion.FillXBEList(const aXBEList: TStringList);
var
  XDKLibVersionsList: TStringList;
  i, j: Integer;
  XBEInfo: TXBEInfo;
begin
  // Cache a reference to aXBEList, for filtering screen-updates
  if FXBEList <> aXBEList then
  begin
    FXBEList := aXBEList;

    XDKLibVersionsList := TStringList.Create;
    try
      // Build up a list of all libraries :
      XDKLibVersionsList.Duplicates := dupIgnore;
      XDKLibVersionsList.Sorted := True;
      for i := 0 to FXBEList.Count - 1 do
      begin
        XBEInfo := TXBEInfo(FXBEList.Objects[i]);
        for j := 0 to XBEInfo.LibVersions.Count - 1 do
          XDKLibVersionsList.Add(XBEInfo.LibVersions.ValueFromIndex[j]);
      end;

      // Put this list in the dropdown box :
      cmb_gametype.Items.BeginUpdate;
      try
        cmb_gametype.Items.Clear;
        cmb_gametype.Items.Add('All XDK Versions');
        for i := 0 to XDKLibVersionsList.Count - 1 do
          if XDKLibVersionsList.Strings[i] <> '' then
            cmb_gametype.Items.Add(XDKLibVersionsList.Strings[i]);

        cmb_gametype.ItemIndex := 0;
      finally
        cmb_gametype.Items.EndUpdate;
      end;

    finally
      FreeAndNil(XDKLibVersionsList);
    end;
  end;

  // Apply the filter :
  MyFilteredXBEList.Clear;
  for i := 0 to FXBEList.Count - 1 do
  begin
    if (cmb_gametype.ItemIndex = 0)
    or TXBEInfo(FXBEList.Objects[i]).MatchesVersion(cmb_gametype.Text) then
      MyFilteredXBEList.AddObject(FXBEList[i], FXBEList.Objects[i]);
  end;

  // Populate the list with the resulting set of XBEs:
  mem_XdkVersions.Clear;
  lst_Games.Items.BeginUpdate;
  try
    lst_Games.Clear;
    for i := 0 to MyFilteredXBEList.Count - 1 do
      lst_Games.Items.Add(TXBEInfo(MyFilteredXBEList.Objects[i]).Title);
  finally
    lst_Games.Items.EndUpdate;
  end;
end; // Tfrm_Xdkversion.FillXBEList

procedure Tfrm_Xdkversion.ShowXBEInfo(const aXBEInfo: TXBEInfo);
var
  i: Integer;
begin
  mem_XdkVersions.Lines.BeginUpdate;
  try
    mem_XdkVersions.Clear;
    for i := 0 to aXBEInfo.LibVersions.Count - 1 do
      mem_XdkVersions.Lines.Add(Format('%-8s : %s', [
        aXBEInfo.LibVersions.Names[i],
        aXBEInfo.LibVersions.ValueFromIndex[i]]));
  finally
    mem_XdkVersions.Lines.EndUpdate;
  end;
end; // Tfrm_Xdkversion.ShowXBEInfo

end.
