{**********************************************************************************************************************}
{                                                                                                                      }
{ delphisvn: Subversion plugin for CodeGear Delphi                                                                     }
{                                                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); you may not use     }
{ this file except in compliance with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either      }
{ express or implied. See the License for the specific language governing rights and limitations under the License.    }
{                                                                                                                      }
{ The Original Code is SvnListView.pas.                                                                                }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{   Uwe Schuster (uschuster)                                                                                           }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains a frame with a listview-style tree to display Subversion information about files and directories. }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnListView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, Menus, ImgList,
  ActnMan, ActnPopup,
  VirtualTrees,
  SvnClient;

type
  TColumnIndex = (cxPathName, cxSvnPathName, cxURL, cxRepository, cxKind, cxTextStatus, cxPropStatus,
    cxTextTime, cxPropTime, cxRemoteTextStatus, cxRemotePropStatus, cxBaseRevision, cxCommittedRevision, cxCommitAuthor,
    cxCommitTime, cxLastCommitRevision, cxLastCommitAuthor, cxLastCommitTime, cxLockOwner, cxLockComment, cxLockTime,
    cxFileAttr);

  PNodeData = ^TNodeData;
  TNodeData = record
    Item: TSvnItem;
    Hide: Boolean;
  end;

  TFrameSvnListView = class(TFrame)
    PopupActionBar: TPopupActionBar;
    Tree: TVirtualStringTree;

    procedure PopupActionBarPopup(Sender: TObject);
    procedure TreeAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: {$IFDEF UNICODE}string {$ELSE}WideString {$ENDIF});
    procedure TreeHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure TreeHeaderDraggedOut(Sender: TVTHeader; Column: TColumnIndex; DropPosition: TPoint);
  private
    FCheckBoxes: Boolean;
    FFullPaths: Boolean;

    function CheckItem(Item: TSvnItem): Boolean;
    procedure HeaderPopupItemClick(Sender: TObject);
  public
    function AddItem(Item: TSvnItem): PVirtualNode;

    property CheckBoxes: Boolean read FCheckBoxes write FCheckBoxes;
    property FullPaths: Boolean read FFullPaths write FFullPaths;
  end;

implementation

uses
  svn_client,
  SvnImages;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

function Compare(Value1, Value2: Double): Integer;

begin
  if Value1 = Value2 then
    Result := 0
  else if Value1 > Value2 then
    Result := 1
  else
    Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnListView private }

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnListView.CheckItem(Item: TSvnItem): Boolean;

begin
  Result := Assigned(Item) and (Item.FileAttr and FILE_ATTRIBUTE_HIDDEN = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnListView.HeaderPopupItemClick(Sender: TObject);

var
  Column: TVirtualTreeColumn;

begin
  if PopupActionBar.PopupComponent <> Tree then
    Exit;

  Column := Tree.Header.Columns[TMenuItem(Sender).Tag];
  if coVisible in Column.Options then
    Column.Options := Column.Options - [coVisible]
  else
    Column.Options := Column.Options + [coVisible];
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnListView.AddItem(Item: TSvnItem): PVirtualNode;

var
  Data: PNodeData;

begin
  Result := nil;
  
  if CheckItem(Item) then
  begin
    Result := Tree.AddChild(nil);
    if FCheckBoxes then
      Result^.CheckType := ctCheckBox;
    Data := Tree.GetNodeData(Result);
    if Assigned(Data) then
      Data^.Item := Item;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnListView event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnListView.PopupActionBarPopup(Sender: TObject);

var
  I: Integer;
  Item: TMenuItem;

begin
  with Sender as TPopupActionBar do
  begin
    Items.Clear;
    if PopupComponent <> Tree then
      Exit;

    for I := 0 to Tree.Header.Columns.Count - 1 do
    begin
      Item := TMenuItem.Create(Self);
      try
        Item.Caption := Tree.Header.Columns[I].Text;
        Item.Hint := Tree.Header.Columns[I].Hint;
        if Item.Hint = '' then
          Item.Hint := Item.Caption;
        Item.Checked := coVisible in Tree.Header.Columns[I].Options;
        Item.AutoCheck := True;
        Item.Enabled := I <> 0;
        Item.Tag := I;
        Item.OnClick := HeaderPopupItemClick;
        Items.Add(Item);
      except
        Item.Free;
        raise;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnListView.TreeAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

var
  Data: PNodeData;
  Icon: TIcon;
  IconWidth, IconHeight: Integer;

begin
  if Column <> cxPathName then
    Exit;

  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) or not Assigned(Data^.Item) then
    Exit;

  Icon := nil;
  if Data^.Item.Locked then
    Icon := SvnImageModule.OverlayIcons[oiLocked]
  else
    case Data^.Item.TextStatus of
      svnWcStatusNormal:
        Icon := SvnImageModule.OverlayIcons[oiSubversion];
      svnWcStatusAdded:
        Icon := SvnImageModule.OverlayIcons[oiAdded];
      svnWcStatusDeleted:
        Icon := SvnImageModule.OverlayIcons[oiDeleted];
      svnWcStatusModified:
        Icon := SvnImageModule.OverlayIcons[oiModified];
      svnWcStatusConflicted:
        Icon := SvnImageModule.OverlayIcons[oiConflict];
    end;

  if Assigned(Icon) then
  begin
    Inc(CellRect.Left, Tree.Margin);
    if Node^.CheckType <> ctNone then
      Inc(CellRect.Left, 16);
    if Assigned(Tree.Images) then
    begin
      IconWidth := Tree.Images.Width;
      IconHeight := Tree.Images.Height;
    end
    else
    begin
      IconWidth := Icon.Width;
      IconHeight := Icon.Height;
    end;
    DrawIconEx(TargetCanvas.Handle, CellRect.Left, CellRect.Top, Icon.Handle, IconWidth, IconHeight, 0, 0, DI_NORMAL);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnListView.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);

var
  Data1, Data2: PNodeData;
  Item1, Item2: TSvnItem;
  SortDirection: TSortDirection;

begin
  Result := 0;
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  if not Assigned(Data1) or not Assigned(Data1^.Item) or not Assigned(Data2) or not Assigned(Data2^.Item) then
    Exit;
  Item1 := Data1^.Item;
  Item2 := Data2^.Item;

  SortDirection := TVirtualStringTree(Sender).Header.SortDirection;

  if FFullPaths then
    Result := 0
  else
    case SortDirection of
      sdAscending:
        Result := Ord(Item2.IsDirectory) - Ord(Item1.IsDirectory);
      sdDescending:
        Result := Ord(Item1.IsDirectory) - Ord(Item2.IsDirectory);
    end;
  if Result = 0 then
    case Column of
      cxPathName:
        if FFullPaths then
          Result := AnsiCompareText(Item1.PathName, Item2.PathName)
        else
          Result := AnsiCompareText(ExtractFileName(Item1.PathName), ExtractFileName(Item2.PathName));
      cxSvnPathName:
        Result := AnsiCompareText(Item1.SvnPathName, Item2.SvnPathName);
      cxURL:
        Result := AnsiCompareText(Item1.URL, Item2.URL);
      cxRepository:
        Result := AnsiCompareText(Item1.Repository, Item2.Repository);
      cxKind:
        Result := AnsiCompareText(NodeKindStrings[Item1.Kind], NodeKindStrings[Item2.Kind]);
      cxTextStatus:
        Result := AnsiCompareText(StatusKindStr(Item1.TextStatus), StatusKindStr(Item2.TextStatus));
      cxPropStatus:
        Result := AnsiCompareText(StatusKindStr(Item1.PropStatus), StatusKindStr(Item2.PropStatus));
      cxTextTime:
        Result := Compare(Item1.TextTime, Item2.TextTime);
      cxPropTime:
        Result := Compare(Item1.PropTime, Item2.PropTime);
      cxRemoteTextStatus:
        Result := AnsiCompareText(StatusKindStr(Item1.RemoteTextStatus), StatusKindStr(Item2.RemoteTextStatus));
      cxRemotePropStatus:
        Result := AnsiCompareText(StatusKindStr(Item1.RemotePropStatus), StatusKindStr(Item2.RemotePropStatus));
      cxBaseRevision:
        Result := Item1.BaseRevision - Item2.BaseRevision;
      cxCommittedRevision:
        Result := Item1.CommittedRevision - Item2.CommittedRevision;
      cxCommitAuthor:
        Result := AnsiCompareText(Item1.CommitAuthor, Item2.CommitAuthor);
      cxCommitTime:
        Result := Compare(Item1.CommitTime, Item2.CommitTime);
      cxLastCommitRevision:
        Result := Item1.LastCommitRevision - Item2.LastCommitRevision;
      cxLastCommitAuthor:
        Result := AnsiCompareText(Item1.LastCommitAuthor, Item2.LastCommitAuthor);
      cxLastCommitTime:
        Result := Compare(Item1.LastCommitTime, Item2.LastCommitTime);
      cxLockOwner:
        Result := AnsiCompareText(Item1.LockOwner, Item2.LockOwner);
      cxLockComment:
        Result := AnsiCompareText(Item1.LockComment, Item2.LockComment);
      cxLockTime:
        Result := Compare(Item1.LockTime, Item2.LockTime);
      cxFileAttr:
        Result := Item1.FileAttr - Item2.FileAttr;
      else
      begin
        if FFullPaths then
          Result := AnsiCompareText(Item1.PathName, Item2.PathName)
        else
          Result := AnsiCompareText(ExtractFileName(Item1.PathName), ExtractFileName(Item2.PathName));
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnListView.TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

var
  Data: PNodeData;

begin
  ImageIndex := -1;
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) or not Assigned(Data^.Item) then
    Exit;

  case Column of
    cxPathName:
      case Kind of
        ikNormal, ikSelected:
          begin
            if Data^.Item.SmallImageIndex = -1 then
              Data^.Item.SmallImageIndex := SvnImageModule.GetShellImageIndex(Data^.Item.PathName);
            ImageIndex := Data^.Item.SmallImageIndex;
          end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnListView.TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);

begin
  NodeDataSize := SizeOf(TNodeData);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnListView.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: {$IFDEF UNICODE}string {$ELSE}WideString {$ENDIF});

var
  Data: PNodeData;

begin
  CellText := '';
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) or not Assigned(Data^.Item) then
    Exit;

  case Column of
    cxPathName:
      case TextType of
        ttNormal:
          if FFullPaths then
            CellText := Data^.Item.PathName
          else
            CellText := ExtractFileName(Data^.Item.PathName);
      end;
    cxSvnPathName:
      case TextType of
        ttNormal:
          CellText := Data^.Item.SvnPathName;
      end;
    cxURL:
      case TextType of
        ttNormal:
          CellText := Data^.Item.URL;
      end;
    cxRepository:
      case TextType of
        ttNormal:
          CellText := Data^.Item.Repository;
      end;
    cxKind:
      case TextType of
        ttNormal:
          CellText := NodeKindStrings[Data^.Item.Kind];
      end;
    cxTextStatus:
      case TextType of
        ttNormal:
          CellText := StatusKindStr(Data^.Item.TextStatus);
      end;
    cxPropStatus:
      case TextType of
        ttNormal:
          CellText := StatusKindStr(Data^.Item.PropStatus);
      end;
    cxTextTime:
      case TextType of
        ttNormal:
          if Data^.Item.TextTime <> 0 then
            CellText := DateTimeToStr(Data^.Item.TextTime);
      end;
    cxPropTime:
      case TextType of
        ttNormal:
          if Data^.Item.PropTime <> 0 then
            CellText := DateTimeToStr(Data^.Item.PropTime);
      end;
    cxRemoteTextStatus:
      case TextType of
        ttNormal:
          CellText := StatusKindStr(Data^.Item.RemoteTextStatus);
      end;
    cxRemotePropStatus:
      case TextType of
        ttNormal:
          CellText := StatusKindStr(Data^.Item.RemotePropStatus);
      end;
    cxBaseRevision:
      case TextType of
        ttNormal:
          if Data^.Item.BaseRevision > 0 then
            CellText := IntToStr(Data^.Item.BaseRevision);
      end;
    cxCommittedRevision:
      case TextType of
        ttNormal:
          if Data^.Item.CommittedRevision > 0 then
            CellText := IntToStr(Data^.Item.CommittedRevision);
      end;
    cxCommitAuthor:
      case TextType of
        ttNormal:
          CellText := Data^.Item.CommitAuthor;
      end;
    cxCommitTime:
      case TextType of
        ttNormal:
          if Data^.Item.CommitTime <> 0 then
            CellText := DateTimeToStr(Data^.Item.CommitTime);
      end;
    cxLastCommitRevision:
      case TextType of
        ttNormal:
          if Data^.Item.LastCommitRevision > 0 then
            CellText := IntToStr(Data^.Item.LastCommitRevision);
      end;
    cxLastCommitAuthor:
      case TextType of
        ttNormal:
          CellText := Data^.Item.LastCommitAuthor;
      end;
    cxLastCommitTime:
      case TextType of
        ttNormal:
          if Data^.Item.LastCommitTime <> 0 then
            CellText := DateTimeToStr(Data^.Item.LastCommitTime);
      end;
    cxLockOwner:
      case TextType of
        ttNormal:
          CellText := Data^.Item.LockOwner;
      end;
    cxLockComment:
      case TextType of
        ttNormal:
          CellText := Data^.Item.LockComment;
      end;
    cxLockTime:
      case TextType of
        ttNormal:
          if Data^.Item.LockTime <> 0 then
            CellText := DateTimeToStr(Data^.Item.LockTime);
      end;
    cxFileAttr:
      case TextType of
        ttNormal:
          CellText := FileAttrStr(Data^.Item.FileAttr);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnListView.TreeHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  if (Button = mbLeft) and (Shift = []) then
  begin
    if Integer(Column) = Sender.SortColumn then
      case Sender.SortDirection of
        sdAscending:
          Sender.SortDirection := sdDescending;
        sdDescending:
          begin
            Sender.SortDirection := sdAscending;
            Sender.SortColumn := NoColumn;
          end;
      end
    else
    begin
      Sender.SortColumn := Integer(Column);
      Sender.SortDirection := sdAscending;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnListView.TreeHeaderDraggedOut(Sender: TVTHeader; Column: TColumnIndex; DropPosition: TPoint);

begin
  if Column <> cxPathName then
    Sender.Columns[Integer(Column)].Options := Sender.Columns[Integer(Column)].Options - [coVisible];
end;

//----------------------------------------------------------------------------------------------------------------------

end.
