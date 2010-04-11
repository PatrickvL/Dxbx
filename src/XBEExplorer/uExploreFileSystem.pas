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
unit uExploreFileSystem;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  // Dxbx
  uFileSystem;

type
  TfrmExploreFileSystem = class(TForm)
    TreeView1: TTreeView;
    Memo1: TMemo;
    ComboBox1: TComboBox;
    lblVolumeLetter: TLabel;
    lblFileSystemType: TLabel;
    lblMountPoint: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
  private
    FVolume: PLogicalVolume;
    procedure SelectVolume(const aVolumeTitle: string);
    function GetNodePath(Node: TTreeNode): string;
    function PopulateTrie(ParentNode: TTreeNode; ParentFolder: string): Integer;
  end;

var
  frmExploreFileSystem: TfrmExploreFileSystem;

implementation

function AttributesToString(Attr: Integer): string;
begin
  if Attr = Integer(INVALID_FILE_ATTRIBUTES) then
  begin
    Result := 'Invalid';
    Exit;
  end;

  Result := '';
  if Attr and FILE_ATTRIBUTE_READONLY             > 0 then Result := Result + ', ReadOnly';
  if Attr and FILE_ATTRIBUTE_HIDDEN               > 0 then Result := Result + ', Hidden';
  if Attr and FILE_ATTRIBUTE_SYSTEM               > 0 then Result := Result + ', System';
  if Attr and FILE_ATTRIBUTE_DIRECTORY            > 0 then Result := Result + ', Directory';
  if Attr and FILE_ATTRIBUTE_ARCHIVE              > 0 then Result := Result + ', Archive';
  if Attr and FILE_ATTRIBUTE_DEVICE               > 0 then Result := Result + ', Device';
  if Attr and FILE_ATTRIBUTE_NORMAL               > 0 then Result := Result + ', Normal';
  if Attr and FILE_ATTRIBUTE_TEMPORARY            > 0 then Result := Result + ', Temporary';
  if Attr and FILE_ATTRIBUTE_SPARSE_FILE          > 0 then Result := Result + ', Sparse file';
  if Attr and FILE_ATTRIBUTE_REPARSE_POINT        > 0 then Result := Result + ', Reparse point';
  if Attr and FILE_ATTRIBUTE_COMPRESSED           > 0 then Result := Result + ', Compressed';
  if Attr and FILE_ATTRIBUTE_OFFLINE              > 0 then Result := Result + ', Offline';
  if Attr and FILE_ATTRIBUTE_NOT_CONTENT_INDEXED  > 0 then Result := Result + ', Not content indexed';
  if Attr and FILE_ATTRIBUTE_ENCRYPTED            > 0 then Result := Result + ', Encrypted';
  if Attr and FILE_ATTRIBUTE_VIRTUAL              > 0 then Result := Result + ', Virtual';
  Delete(Result, 1, 2);
end;

{$R *.dfm}

procedure TfrmExploreFileSystem.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
end;

procedure TfrmExploreFileSystem.FormShow(Sender: TObject);
var
  l: Char;
  Volume: PLogicalVolume;
begin
  // Collect all mounted volumes (could be more than one) :
  ComboBox1.Items.Clear;
  for l := 'A' to 'Z' do
  begin
    Volume := Drives.GetVolume(l);
    if Assigned(Volume) and Volume.IsMounted then
      ComboBox1.Items.Add(l + ':');
  end;

  // Select first mounted volume :
  ComboBox1.ItemIndex := 0;
  // Simulate click on first item :
  ComboBox1Change(ComboBox1);
end;

procedure TfrmExploreFileSystem.ComboBox1Change(Sender: TObject);
begin
  // Switch to selected volume :
  SelectVolume(ComboBox1.Items[ComboBox1.ItemIndex]);
end;

procedure TfrmExploreFileSystem.SelectVolume(const aVolumeTitle: string);
begin
  FVolume := Drives.GetVolume(aVolumeTitle[1]);
  Assert(Assigned(FVolume));
  Assert(FVolume.IsMounted);

  Caption := 'Exporing ' + aVolumeTitle;

  // Show information on FileSystem :
  lblVolumeLetter.Caption := 'Volume : ' + FVolume^.Letter;
  lblFileSystemType.Caption := 'Filesystem type : ' + FVolume.FileSystem.TypeStr;
  lblMountPoint.Caption := 'Mount point : ' + FVolume.FileSystem.MountPoint;

  // Show folders and files in TrieView :
  TreeView1.Items.Clear;
  PopulateTrie(nil, '');
end;


function FileNodesSort(lParam1, lParam2, lParamSort: Longint): Integer; stdcall;
begin
  if TTreenode(lParam1).HasChildren = TTreenode(lParam2).HasChildren then
  begin
    Result := CompareText(TTreenode(lParam1).Text, TTreenode(lParam2).Text);
    Exit;
  end;

  if TTreenode(lParam1).HasChildren then
    Result := -1
  else
    Result := 1;
end;

function TfrmExploreFileSystem.PopulateTrie(ParentNode: TTreeNode; ParentFolder: string): Integer;
var
  SearchInfo: TSearchInfo;
begin
  Result := 0;
  SearchInfo := FVolume.FileSystem.FindFirst(ParentFolder + '\*');
  if Assigned(SearchInfo) then
  try
    repeat
      if SearchInfo.Attributes and FILE_ATTRIBUTE_DIRECTORY > 0 then
        TreeView1.Items.AddChild(ParentNode, SearchInfo.Filename).HasChildren := True
      else
        TreeView1.Items.AddChild(ParentNode, SearchInfo.Filename);

      Inc(Result);
    until not FVolume.FileSystem.FindNext(SearchInfo);

    if Assigned(ParentNode) then
      ParentNode.CustomSort(FileNodesSort, {Data=}0, {Recurse=}False)
    else
      TreeView1.Items.CustomSort(FileNodesSort, {Data=}0, {Recurse=}False);
  finally
    FVolume.FileSystem.FindClose(SearchInfo);
  end;
end;

// Show information on TrieView selection (Dir or File)
procedure TfrmExploreFileSystem.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  FileName: string;
  SearchInfo: TSearchInfo;
begin
  FileName := GetNodePath(Node);

  SearchInfo := FVolume.FileSystem.FindFirst(FileName);
  if Assigned(SearchInfo) then
  begin
    Memo1.Lines.Text := Format('%s'#13#10'%d'#13#10'%x = %s', [
      SearchInfo.Filename,
      SearchInfo.FileSize,
      SearchInfo.Attributes,
      AttributesToString(SearchInfo.Attributes)]);
    FVolume.FileSystem.FindClose(SearchInfo)
  end;
end;

function TfrmExploreFileSystem.GetNodePath(Node: TTreeNode): string;
begin
  Result := Node.Text;
  while Assigned(Node.Parent) do
  begin
    Node := Node.Parent;
    Result := Node.Text + '\' + Result;
  end;
end;

procedure TfrmExploreFileSystem.TreeView1Expanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
begin
  // Support unfolding of TrieNodes :
  if Node.getFirstChild = nil then
    // TODO : How to show empty folders? For now remove [+], later show [-]
    Node.HasChildren := PopulateTrie(Node, GetNodePath(Node)) > 0;
end;

// TODO : Show File contents (using normal file handling actions like: Open, Seek, Read and Close)

end.

