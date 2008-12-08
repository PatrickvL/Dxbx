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
unit uImportGames;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls, ExtCtrls,
  // Dxbx
  uData;

type
  Tfrm_ImportGames = class(TForm)
    btn_Cancel: TButton;
    btn_Ok: TButton;
    lbl_Publisher: TLabel;
    edt_Publisher: TEdit;
    lst_Import: TListView;
    lbl_NewGames: TLabel;
    PopupMenu1: TPopupMenu;
    SelectAll1: TMenuItem;
    SelectInverse1: TMenuItem;
    SelectNone1: TMenuItem;
    mem_XdkVersions: TMemo;
    cmb_gametype: TComboBox;
    lbl_XDKFilter: TLabel;
    procedure SelectAll1Click(Sender: TObject);
    procedure SelectInverse1Click(Sender: TObject);
    procedure SelectNone1Click(Sender: TObject);
    procedure lst_ImportColumnClick(Sender: TObject; Column: TListColumn);
    procedure lst_ImportSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure cmb_gametypeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lst_ImportEdited(Sender: TObject; Item: TListItem; var S: string);
  protected
    FXBEList: TStringList;
    MyXDKLibNamesList: TStringList;
    MyXDKLibVersionsList: TStringList;
    MyFilteredXBEList: TStringList;
    procedure ShowXBEInfo(const aXBEInfo: TXBEInfo);
  public
    procedure FillXBEList(const aXBEList: TStringList);
  end;

var
  frm_ImportGames: Tfrm_ImportGames;

implementation

{$R *.dfm}

function ColumnSort(Item1, Item2: TListItem; Data: Integer): Integer; stdcall;
begin
  if Data < 1 then
    Result := lstrcmp(PChar(Item1.Caption), PChar(Item2.Caption))
  else
    Result := lstrcmp(PChar(Item1.SubItems[Data - 1]), PChar(Item2.SubItems[Data - 1]));
end;

{ Tfrm_ImportGames }

procedure Tfrm_ImportGames.FormCreate(Sender: TObject);
begin
  MyFilteredXBEList := TStringList.Create;

  MyXDKLibNamesList := TStringList.Create;
  MyXDKLibNamesList.Duplicates := dupIgnore;
  MyXDKLibNamesList.Sorted := True;

  MyXDKLibVersionsList := TStringList.Create;
  MyXDKLibVersionsList.Duplicates := dupIgnore;
  MyXDKLibVersionsList.Sorted := True;
end;

procedure Tfrm_ImportGames.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MyXDKLibNamesList);
  FreeAndNil(MyXDKLibVersionsList);
  FreeAndNil(MyFilteredXBEList);
end;

procedure Tfrm_ImportGames.cmb_gametypeChange(Sender: TObject);
begin
  FillXBEList(FXBEList);
end;

procedure Tfrm_ImportGames.lst_ImportColumnClick(Sender: TObject; Column: TListColumn);
begin
  lst_Import.CustomSort(@ColumnSort, Column.Index);
end;

procedure Tfrm_ImportGames.lst_ImportEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  TXBEInfo(Item.Data).Title := s;
end;

procedure Tfrm_ImportGames.SelectAll1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lst_Import.Items.Count - 1 do
    lst_Import.Items[i].Checked := True;
end;

procedure Tfrm_ImportGames.SelectInverse1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lst_Import.Items.Count - 1 do
    lst_Import.Items[i].Checked := not lst_Import.Items[i].Checked;
end;

procedure Tfrm_ImportGames.SelectNone1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lst_Import.Items.Count - 1 do
    lst_Import.Items[i].Checked := False;
end;

procedure Tfrm_ImportGames.lst_ImportSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  XBEInfo: TXBEInfo;
begin
  if Assigned(Item) then
  begin
    XBEInfo := TXBEInfo(Item.Data);
    Assert(Assigned(XBEInfo));

    ShowXBEInfo(XBEInfo);
  end;
end;

//

procedure Tfrm_ImportGames.FillXBEList(const aXBEList: TStringList);
var
  i, j: Integer;
  XBEInfo: TXBEInfo;
  CurrentSelection: string;
  Line: TListItem;
begin
  if not Assigned(aXBEList) then
    Exit;

  // Cache a reference to aXBEList, for filtering screen-updates
  if FXBEList <> aXBEList then
  begin
    FXBEList := aXBEList;

    // Build up a list of all libraries and versions :
    MyXDKLibNamesList.Clear;
    MyXDKLibVersionsList.Clear;
    for i := 0 to FXBEList.Count - 1 do
    begin
      XBEInfo := TXBEInfo(FXBEList.Objects[i]);
      for j := 0 to XBEInfo.LibVersions.Count - 1 do
      begin
        MyXDKLibNamesList.Add(XBEInfo.LibVersions.Names[j]);
        MyXDKLibVersionsList.Add(XBEInfo.LibVersions.ValueFromIndex[j]);
      end;
    end;

    // Put the versions in the dropdown box :
    cmb_gametype.Items.BeginUpdate;
    try
      cmb_gametype.Items.Clear;
      cmb_gametype.Items.Add('All XDK Versions');
      for i := 0 to MyXDKLibVersionsList.Count - 1 do
        if MyXDKLibVersionsList.Strings[i] <> '' then
          cmb_gametype.Items.Add(MyXDKLibVersionsList.Strings[i]);

      cmb_gametype.ItemIndex := 0;
    finally
      cmb_gametype.Items.EndUpdate;
    end;

    // Put the libraries in the columns of the list box :
    lst_Import.Columns.BeginUpdate;
    try
      lst_Import.Columns.Clear;
      with lst_Import.Columns.Add do
      begin
        Caption := 'Title';
        Width := 200;
      end;

      with lst_Import.Columns.Add do
      begin
        Caption := 'Game region';
        Width := 75;
      end;

      with lst_Import.Columns.Add do
      begin
        Caption := 'Dumped with';
        Width := 175;
      end;

      with lst_Import.Columns.Add do
      begin
        Caption := 'Filename';
        Width := 150;
      end;

      for i := 0 to MyXDKLibNamesList.Count - 1 do
        with lst_Import.Columns.Add do
        begin
          Caption := MyXDKLibNamesList[i];
          Width := 75;
        end;
    finally
      lst_Import.Columns.EndUpdate;
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

  mem_XdkVersions.Clear;

  // Remember current selection (by Title) :
  if Assigned(lst_Import.Selected) then
    CurrentSelection := lst_Import.Selected.Caption
  else
    CurrentSelection := '';

  // Populate the list with the filtered set of XBEs:
  lst_Import.Items.BeginUpdate;
  try
    lst_Import.Clear;

    for i := 0 to MyFilteredXBEList.Count - 1 do
    begin
      XBEInfo := TXBEInfo(MyFilteredXBEList.Objects[i]);

      Line := lst_Import.Items.Add;
      Line.Data := XBEInfo;
      Line.Caption := XBEInfo.Title;
      Line.Checked := not XBEInfo.IsDuplicate;
      Line.SubItems.Add(GameRegionToString(XBEInfo.GameRegion));
      Line.SubItems.Add(XBEInfo.DumpInfo);
      Line.SubItems.Add(XBEInfo.FileName);
      for j := 0 to MyXDKLibNamesList.Count - 1 do
        Line.SubItems.Add(XBEInfo.LibVersions.Values[MyXDKLibNamesList[j]]);
    end;

  finally
    // Reset previous selection :
    lst_Import.Selected := lst_Import.FindCaption(0, CurrentSelection, {Partial=}False, {Inclusive=}True, {Wrap=}False);
    if (lst_Import.Selected = nil) and (lst_Import.Items.Count > 0) then
      lst_Import.ItemIndex := 0;


    lst_Import.Items.EndUpdate;
  end;
end; // Tfrm_ImportGames.FillXBEList

procedure Tfrm_ImportGames.ShowXBEInfo(const aXBEInfo: TXBEInfo);
var
  i: Integer;
begin
  mem_XdkVersions.Lines.BeginUpdate;
  try
    mem_XdkVersions.Clear;
    mem_XdkVersions.Lines.Add('Title : ' + aXBEInfo.Title);
    mem_XdkVersions.Lines.Add('Region: ' + GameRegionToString(aXBEInfo.GameRegion));
    mem_XdkVersions.Lines.Add('DumpInfo: ' + aXBEInfo.DumpInfo);
    mem_XdkVersions.Lines.Add('Filename: ' + aXBEInfo.FileName);
    mem_XdkVersions.Lines.Add('');
    mem_XdkVersions.Lines.Add('Library versions:');
    for i := 0 to aXBEInfo.LibVersions.Count - 1 do
      mem_XdkVersions.Lines.Add(Format('%-8s : %s', [
        aXBEInfo.LibVersions.Names[i],
        aXBEInfo.LibVersions.ValueFromIndex[i]]));
  finally
    mem_XdkVersions.Lines.EndUpdate;
  end;
end; // Tfrm_ImportGames.ShowXBEInfo

end.
