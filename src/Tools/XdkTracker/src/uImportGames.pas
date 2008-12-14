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
  Tfrm_XBEList = class(TForm)
    btn_Cancel: TButton;
    btn_Ok: TButton;
    lbl_Publisher: TLabel;
    edt_Publisher: TEdit;
    lst_XBEs: TListView;
    lbl_NewGames: TLabel;
    PopupMenu1: TPopupMenu;
    SelectAll1: TMenuItem;
    SelectInverse1: TMenuItem;
    SelectNone1: TMenuItem;
    mem_XdkVersions: TMemo;
    cmb_XDKVersions: TComboBox;
    lbl_XDKFilter: TLabel;
    procedure SelectAll1Click(Sender: TObject);
    procedure SelectInverse1Click(Sender: TObject);
    procedure SelectNone1Click(Sender: TObject);
    procedure lst_XBEsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lst_XBEsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure cmb_XDKVersionsChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lst_XBEsEdited(Sender: TObject; Item: TListItem; var S: string);
  protected
    FShowAsImport: Boolean;
    FXBEList: TStringList;
    MyXDKLibNamesList: TStringList;
    MyXDKLibVersionsList: TStringList;
    MyFilteredXBEList: TStringList;
    procedure ShowXBEInfo(const aXBEInfo: TXBEInfo);
  public
    procedure FillXBEList(const aXBEList: TStringList; ShowAsImport: Boolean);
  end;

var
  frm_XBEList: Tfrm_XBEList;

implementation

{$R *.dfm}

function ColumnSort(Item1, Item2: TListItem; Data: Integer): Integer; stdcall;
begin
  if Data < 1 then
    Result := lstrcmp(PChar(Item1.Caption), PChar(Item2.Caption))
  else
    Result := lstrcmp(PChar(Item1.SubItems[Data - 1]), PChar(Item2.SubItems[Data - 1]));
end;

{ Tfrm_XBEList }

procedure Tfrm_XBEList.FormCreate(Sender: TObject);
begin
  MyFilteredXBEList := TStringList.Create;

  MyXDKLibNamesList := TStringList.Create;
  MyXDKLibNamesList.Duplicates := dupIgnore;
  MyXDKLibNamesList.Sorted := True;

  MyXDKLibVersionsList := TStringList.Create;
  MyXDKLibVersionsList.Duplicates := dupIgnore;
  MyXDKLibVersionsList.Sorted := True;
end;

procedure Tfrm_XBEList.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MyXDKLibNamesList);
  FreeAndNil(MyXDKLibVersionsList);
  FreeAndNil(MyFilteredXBEList);
end;

procedure Tfrm_XBEList.cmb_XDKVersionsChange(Sender: TObject);
begin
  FillXBEList(FXBEList, FShowAsImport);
end;

procedure Tfrm_XBEList.lst_XBEsColumnClick(Sender: TObject; Column: TListColumn);
begin
  lst_XBEs.CustomSort(@ColumnSort, Column.Index);
end;

procedure Tfrm_XBEList.lst_XBEsEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  TXBEInfo(Item.Data).Title := s;
end;

procedure Tfrm_XBEList.SelectAll1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lst_XBEs.Items.Count - 1 do
    lst_XBEs.Items[i].Checked := True;
end;

procedure Tfrm_XBEList.SelectInverse1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lst_XBEs.Items.Count - 1 do
    lst_XBEs.Items[i].Checked := not lst_XBEs.Items[i].Checked;
end;

procedure Tfrm_XBEList.SelectNone1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lst_XBEs.Items.Count - 1 do
    lst_XBEs.Items[i].Checked := False;
end;

procedure Tfrm_XBEList.lst_XBEsSelectItem(Sender: TObject;
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

procedure Tfrm_XBEList.FillXBEList(const aXBEList: TStringList; ShowAsImport: Boolean);
var
  i, j: Integer;
  XBEInfo: TXBEInfo;
  CurrentSelection: string;
  Line: TListItem;
  NrNew: Integer;
begin
  if not Assigned(aXBEList) then
    Exit;

  // Cache a reference to aXBEList, for filtering screen-updates
  if FXBEList <> aXBEList then
  begin
    FXBEList := aXBEList;

    // Handle the difference between import-mode and view-mode :
    FShowAsImport := ShowAsImport;
    lst_XBEs.Checkboxes := FShowAsImport;
    btn_Cancel.Visible := FShowAsImport;
    if FShowAsImport then
    begin
      lst_XBEs.PopupMenu := PopupMenu1;
      btn_Ok.Left := btn_Cancel.Left - 80;
      btn_Cancel.Cancel := True;
    end
    else
    begin
      lst_XBEs.PopupMenu := nil;
      btn_Ok.Left := btn_Cancel.Left;
      btn_Ok.Cancel := True;
    end;

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
    cmb_XDKVersions.Items.BeginUpdate;
    try
      cmb_XDKVersions.Items.Clear;
      cmb_XDKVersions.Items.Add('All XDK Versions');
      for i := 0 to MyXDKLibVersionsList.Count - 1 do
        if MyXDKLibVersionsList.Strings[i] <> '' then
          cmb_XDKVersions.Items.Add(MyXDKLibVersionsList.Strings[i]);

      cmb_XDKVersions.ItemIndex := 0;
    finally
      cmb_XDKVersions.Items.EndUpdate;
    end;

    // Put the libraries in the columns of the list box :
    lst_XBEs.Columns.BeginUpdate;
    try
      lst_XBEs.Columns.Clear;
      with lst_XBEs.Columns.Add do
      begin
        Caption := 'Title';
        Width := 200;
      end;

      with lst_XBEs.Columns.Add do
      begin
        Caption := 'Game region';
        Width := 75;
      end;

      with lst_XBEs.Columns.Add do
      begin
        Caption := 'Dumped with';
        Width := 175;
      end;

      with lst_XBEs.Columns.Add do
      begin
        Caption := 'Filename';
        Width := 150;
      end;

      for i := 0 to MyXDKLibNamesList.Count - 1 do
        with lst_XBEs.Columns.Add do
        begin
          Caption := MyXDKLibNamesList[i];
          Width := 75;
        end;
    finally
      lst_XBEs.Columns.EndUpdate;
    end;
  end;

  // Apply the filter :
  MyFilteredXBEList.Clear;
  for i := 0 to FXBEList.Count - 1 do
  begin
    if (cmb_XDKVersions.ItemIndex = 0)
    or TXBEInfo(FXBEList.Objects[i]).MatchesVersion(cmb_XDKVersions.Text) then
      MyFilteredXBEList.AddObject(FXBEList[i], FXBEList.Objects[i]);
  end;

  mem_XdkVersions.Clear;

  // Remember current selection (by Title) :
  if Assigned(lst_XBEs.Selected) then
    CurrentSelection := lst_XBEs.Selected.Caption
  else
    CurrentSelection := '';

  // Populate the list with the filtered set of XBEs:
  lst_XBEs.Items.BeginUpdate;
  try
    lst_XBEs.Clear;

    NrNew := 0;
    for i := 0 to MyFilteredXBEList.Count - 1 do
    begin
      XBEInfo := TXBEInfo(MyFilteredXBEList.Objects[i]);

      Line := lst_XBEs.Items.Add;
      Line.Data := XBEInfo;
      Line.Caption := XBEInfo.Title;
      Line.Checked := not XBEInfo.IsDuplicate;
      Line.SubItems.Add(GameRegionToString(XBEInfo.GameRegion));
      Line.SubItems.Add(XBEInfo.DumpInfo);
      Line.SubItems.Add(XBEInfo.FileName);
      for j := 0 to MyXDKLibNamesList.Count - 1 do
        Line.SubItems.Add(XBEInfo.LibVersions.Values[MyXDKLibNamesList[j]]);

      if Line.Checked then
        Inc(NrNew);
    end;

  finally
    if FXBEList.Count > MyFilteredXBEList.Count then
      lbl_NewGames.Caption := Format('Filtered %d Titles', [MyFilteredXBEList.Count])
    else
      lbl_NewGames.Caption := Format('All %d Titles', [MyFilteredXBEList.Count]);

    if ShowAsImport then
      lbl_NewGames.Caption := lbl_NewGames.Caption + Format(' (%d new)', [NrNew]);

    lbl_NewGames.Caption := lbl_NewGames.Caption + ' :';

    // Reset previous selection :
    lst_XBEs.Selected := lst_XBEs.FindCaption(0, CurrentSelection, {Partial=}False, {Inclusive=}True, {Wrap=}False);
    if (lst_XBEs.Selected = nil) and (lst_XBEs.Items.Count > 0) then
      lst_XBEs.ItemIndex := 0;


    lst_XBEs.Items.EndUpdate;
  end;
end; // Tfrm_XBEList.FillXBEList

procedure Tfrm_XBEList.ShowXBEInfo(const aXBEInfo: TXBEInfo);
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
end; // Tfrm_XBEList.ShowXBEInfo

end.
