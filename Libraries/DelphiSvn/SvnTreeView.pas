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
{ The Original Code is SvnTreeView.pas.                                                                                }
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
{ This unit contains a frame with a treeview displaying Subversion directories.                                        }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnTreeView;

interface

{$include Compilers.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ImgList,
  VirtualTrees,
  SvnClient, Menus;

type
  PNodeData = ^TNodeData;
  TNodeData = record
    DisplayName: array[0..MAX_PATH] of Char;
    Attr: Cardinal;
    IconIndex: Integer;
    OpenIconIndex: Integer;
    Item: TSvnItem;
  end;

  TFrameSvnTreeView = class(TFrame)
    Tree: TVirtualStringTree;

    procedure TreeAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect);
    procedure TreeCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: {$IFDEF UNICODE}string {$ELSE}WideString {$ENDIF});
    procedure TreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    FCheckBoxes: Boolean;
    FClient: TSvnClient;
    FRootQueue: array of string;

    procedure AssignData(Data: PNodeData; Item: TSvnItem);
    function CheckIndex(Parent: TSvnItem; Index: Integer): Integer;
    function CheckItem(Item: TSvnItem): Boolean;
    function FindChildNode(Parent: PVirtualNode; const Name: string): PVirtualNode;
    function GetDisplayName(const PathName: string): string;
    procedure ItemDestroy(Sender: TObject);
    procedure SetClient(Value: TSvnClient);
  public
    function AddNode(const Path: string): PVirtualNode; overload;
    function AddNode(Item: TSvnItem): PVirtualNode; overload;
    function FindNode(const Path: string): PVirtualNode;
    function NodePath(Node: PVirtualNode): string;

    property CheckBoxes: Boolean read FCheckBoxes write FCheckBoxes;
    property Client: TSvnClient read FClient write SetClient;
  end;

implementation

uses
  svn_client, SvnImages;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnExplorer private }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.AssignData(Data: PNodeData; Item: TSvnItem);

begin
  FillChar(Data^, SizeOf(TNodeData), 0);
  Data^.Item := Item;
  Item.Tag := Integer(Data);
  Data^.Item.AddDestroyNotification(ItemDestroy);
  StrLCopy(Data^.DisplayName, PChar(GetDisplayName(Data^.Item.PathName)), SizeOf(Data^.DisplayName) - 1);
  Data^.Attr := GetFileAttributes(PChar(Data^.Item.PathName));
  Data^.IconIndex := -1;
  Data^.OpenIconIndex := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnTreeView.CheckIndex(Parent: TSvnItem; Index: Integer): Integer;

var
  I, J: Integer;

begin
  Result := -1;

  J := -1;
  for I := 0 to Parent.Count - 1 do
  begin
    if CheckItem(Parent[I]) then
      Inc(J);
    if J >= Index then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnTreeView.CheckItem(Item: TSvnItem): Boolean;

begin
  Result := Item.IsDirectory and (Item.FileAttr and FILE_ATTRIBUTE_HIDDEN = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnTreeView.FindChildNode(Parent: PVirtualNode; const Name: string): PVirtualNode;

var
  Node: PVirtualNode;
  Data: PNodeData;

begin
  Result := nil;

  if not Assigned(Parent) then
    Parent := Tree.RootNode;

  Node := Tree.GetFirstChild(Parent);
  while Assigned(Node) do
  begin
    Data := Tree.GetNodeData(Node);
    if Assigned(Data) and AnsiSameText(Name, Data^.DisplayName) then
    begin
      Result := Node;
      Break;
    end;

    Node := Tree.GetNextSibling(Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnTreeView.GetDisplayName(const PathName: string): string;

var
  VolumeName: array[0..MAX_PATH] of Char;
  MaxCompLen, Flags: Cardinal;
  S: string;

begin
  Result := ExcludeTrailingPathDelimiter(PathName);
  if AnsiSameText(Result, ExtractFileDrive(PathName)) then
  begin
    S := IncludeTrailingPathDelimiter(PathName);
    FillChar(VolumeName, SizeOf(VolumeName), 0);
    if GetVolumeInformation(PChar(S), VolumeName, SizeOf(VolumeName) - 1, nil, MaxCompLen, Flags, nil, 0) then
      Result := Format('%s (%s)', [VolumeName, Result]);
  end
  else
    Result := ExtractFileName(PathName);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.ItemDestroy(Sender: TObject);

var
  Data: PNodeData;

begin
  Data := Pointer(TSvnItem(Sender).Tag);
  if Assigned(Data) then
    Data^.Item := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.SetClient(Value: TSvnClient);

begin
  if Value <> FClient then
  begin
    Tree.Clear;
    FClient := Value;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnExplorer public }

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnTreeView.AddNode(const Path: string): PVirtualNode;

begin
  Result := nil;
  if Path = '' then
    Exit;

  SetLength(FRootQueue, Length(FRootQueue) + 1);
  FRootQueue[High(FRootQueue)] := Path;
  Tree.RootNodeCount := Tree.RootNodeCount + 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnTreeView.AddNode(Item: TSvnItem): PVirtualNode;

var
  Data: PNodeData;

begin
  Result := nil;
  if not Assigned(Item) then
    Exit;

  Result := Tree.AddChild(nil);
  Data := Tree.GetNodeData(Result);
  AssignData(Data, Item);
  Tree.HasChildren[Result] := Assigned(Data) and Assigned(Data^.Item) and (Data^.Item.Kind in [svnNodeDir]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnTreeView.FindNode(const Path: string): PVirtualNode;

var
  PStart, PEnd: PChar;
  S: string;
  Node: PVirtualNode;

begin
  Result := nil;
  if Path = '' then
    Exit;

  Node := nil;
  PStart := PChar(Path);
  while PStart^ = PathDelim do
    Inc(PStart);
  repeat
    PEnd := StrPos(PStart, PathDelim);
    if PEnd = nil then
      PEnd := StrEnd(PStart);
    SetString(S, PStart, PEnd - PStart);
    Node := FindChildNode(Node, S);
    if Node = nil then
      Exit;
    if PEnd^ = #0 then
      Result := Node;

    PStart := PEnd + 1;
  until Assigned(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnTreeView.NodePath(Node: PVirtualNode): string;

var
  Data: PNodeData;

begin
  Data := Tree.GetNodeData(Node);
  if Assigned(Data) and Assigned(Data^.Item) then
    Result := Data^.Item.PathName;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnExplorer event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.TreeAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

var
  Data: PNodeData;
  Icon: TIcon;
  IconWidth, IconHeight: Integer;

begin
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
    Inc(CellRect.Left, Tree.Indent * Tree.GetNodeLevel(Node) + Cardinal(Tree.Margin) + Cardinal(Tree.Images.Width) + 2);
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

procedure TFrameSvnTreeView.TreeCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);

begin
  if Assigned(Sender.FocusedNode) and Sender.HasAsParent(Sender.FocusedNode, Node) then
  begin
    Sender.ClearSelection;
    Sender.FocusedNode := Node;
    Sender.Selected[Node] := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);

var
  Data1, Data2: PNodeData;

begin
  Result := 0;
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  if not Assigned(Data1) or not Assigned(Data2) then
    Exit;

  Result := Ord(Data1^.Attr and FILE_ATTRIBUTE_DIRECTORY = 0) - Ord(Data2^.Attr and FILE_ATTRIBUTE_DIRECTORY = 0);
  if Result = 0 then
    Result := AnsiCompareText(Data1^.DisplayName, Data2^.DisplayName);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);

begin
  if not Assigned(Node^.FirstChild) then
  begin
    Screen.Cursor := crHourGlass;
    try
      Sender.Sort(Node, NoColumn, sdAscending);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  Data: PNodeData;

begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    FreeAndNil(Data^.Item);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

var
  Data: PNodeData;

begin
  ImageIndex := -1;

  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) or not Assigned(Data^.Item) then
    Exit;

  case Kind of
    ikNormal:
      begin
        if Data^.Item.SmallImageIndex = -1 then
          Data^.Item.SmallImageIndex := SvnImageModule.GetShellImageIndex(Data^.Item.PathName);
        ImageIndex := Data^.Item.SmallImageIndex;
      end;
    ikSelected:
      ImageIndex := SvnImageModule.GetShellImageIndex(Data^.Item.PathName, True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);

begin
  NodeDataSize := SizeOf(TNodeData);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: {$IFDEF UNICODE}string {$ELSE}WideString {$ENDIF});

var
  Data: PNodeData;

begin
  CellText := '';
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) or not Assigned(Data^.Item) then
    Exit;

  case TextType of
    ttNormal:
      CellText := Data^.DisplayName;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.TreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

var
  Data: PNodeData;
  I: Integer;

begin
  ChildCount := 0;

  Data := Sender.GetNodeData(Node);
  if Assigned(Data) and Assigned(Data^.Item) then
    for I := 0 to Data^.Item.Count - 1 do
      if CheckItem(Data^.Item[I]) then
        Inc(ChildCount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnTreeView.TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  ParentData, Data: PNodeData;
  S: string;

begin
  if FCheckBoxes then
    Node^.CheckType := ctTriStateCheckBox;
    
  ParentData := Sender.GetNodeData(ParentNode);
  if Assigned(ParentData) then
  begin
    Data := Sender.GetNodeData(Node);
    AssignData(Data, ParentData^.Item[CheckIndex(ParentData^.Item, Node^.Index)]);
    if Assigned(Data) and Assigned(Data^.Item) and ((Data^.Item.Kind = svnNodeDir) or
      (Data^.Item.TextStatus = svnWcStatusExternal)) then
      Include(InitialStates, ivsHasChildren);
    if ParentNode^.CheckState = csCheckedNormal then
      Node^.CheckState := csCheckedNormal;
  end
  else if Length(FRootQueue) > 0 then
  begin
    if AnsiSameText(ExcludeTrailingPathDelimiter(FRootQueue[0]), ExtractFileDrive(FRootQueue[0])) then
      S := IncludeTrailingPathDelimiter(FRootQueue[0])
    else
      S := ExcludeTrailingPathDelimiter(FRootQueue[0]);
    FRootQueue := Copy(FRootQueue, 1, Length(FRootQueue));

    Data := Sender.GetNodeData(Node);
    try
      AssignData(Data, TSvnItem.Create(FClient, nil, S));
    except
      Sender.DeleteNode(Node);
      raise;
    end;
    if Assigned(Data) and Assigned(Data^.Item) and (Data^.Item.Kind = svnNodeDir) then
      Include(InitialStates, ivsHasChildren);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
