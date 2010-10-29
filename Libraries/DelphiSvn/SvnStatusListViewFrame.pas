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
{ The Original Code is SvnStatusListView.pas.                                                                          }
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
{ This unit contains a frame to provide visual feedback from svn_client_status2 API call running in a separate thread. }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnStatusListViewFrame;

interface

{$INCLUDE Compilers.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ActnList,
  VirtualTrees,
  svn_client, SvnClient, SvnBaseFrame, SvnListView, Menus;

type
  TFrameSvnStatusListView = class(TFrameSvnBase)
    FrameSvnListView: TFrameSvnListView;

    procedure FrameSvnListViewTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FConflicts: TStringList;
    FItem: TSvnItem;
    FShowUnversioned: Boolean;
    FStatusException: string;

    procedure CheckUnversionedConflictFiles;
    procedure ItemDestroy(Sender: TObject);
    procedure SetShowUnversioned(Value: Boolean);
    procedure ShowUnversionedChanged;

    procedure AMUpdate(var Message: TMessage); message AM_UPDATE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure HandleAddExecute(Action: TAction); override;
    procedure HandleAddUpdate(Action: TAction); override;
    procedure HandleMergeConflictsExecute(Action: TAction); override;
    procedure HandleMergeConflictsUpdate(Action: TAction); override;
    procedure HandleOpenExecute(Action: TAction); override;
    procedure HandleOpenUpdate(Action: TAction); override;
    procedure HandleShowBlameExecute(Action: TAction); override;
    procedure HandleShowBlameUpdate(Action: TAction); override;
    procedure HandleShowDiffExecute(Action: TAction); override;
    procedure HandleShowDiffUpdate(Action: TAction); override;
    procedure HandleShowUnversionedExecute(Action: TAction); override;
    procedure HandleShowUnversionedUpdate(Action: TAction); override;
    procedure StartCheckModifications(AClient: TSvnClient; ADirectories: TStrings; ARecurse: Boolean = True;
      AUpdate: Boolean = True; AIgnoreExternals: Boolean = False; ARecurseUnversioned: Boolean = False);
  end;

implementation

uses
  SvnImages, SvnIDEClient;

{$R *.dfm}

type
  TSvnCheckModificationsThread = class(TThread)
  private
    FClient: TSvnClient;
    FDirectories: TStrings;
    FFrame: TFrameSvnStatusListView;
    FIgnoreExternals: Boolean;
    FRecurse: Boolean;
    FRecurseUnversioned: Boolean;
    FUpdate: Boolean;

    procedure SvnGetModifications(Sender: TObject; Item: TSvnItem; var Cancel: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create(AFrame: TFrameSvnStatusListView; AClient: TSvnClient; ADirectories: TStrings;
      ARecurse, AUpdate, AIgnoreExternals, ARecurseUnversioned: Boolean);
    destructor Destroy; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnCheckModificationsThread private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnCheckModificationsThread.SvnGetModifications(Sender: TObject; Item: TSvnItem; var Cancel: Boolean);

begin
  Cancel := FFrame.Cancelled;
  if Cancel then
    Exit;

  FFrame.FItem := Item;
  SendMessage(FFrame.Handle, AM_UPDATE, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnCheckModificationsThread protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnCheckModificationsThread.Execute;

var
  I: Integer;

begin
  try
    for I := 0 to FDirectories.Count - 1 do
      FClient.GetModifications(FDirectories[I], SvnGetModifications, FRecurse, FUpdate, FIgnoreExternals,
        FRecurseUnversioned);
    PostMessage(FFrame.Handle, AM_UPDATE, 1, 0);
  except
    on E: Exception do
    begin
      FFrame.FStatusException := E.Message;
      PostMessage(FFrame.Handle, AM_UPDATE, 1, 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnCheckModificationsThread public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnCheckModificationsThread.Create(AFrame: TFrameSvnStatusListView; AClient: TSvnClient;
  ADirectories: TStrings; ARecurse, AUpdate, AIgnoreExternals, ARecurseUnversioned: Boolean);

begin
  FFrame := AFrame;
  FClient := AClient;
  FDirectories := TStringList.Create;
  FDirectories.Assign(ADirectories);
  FRecurse := ARecurse;
  FUpdate := AUpdate;
  FIgnoreExternals := AIgnoreExternals;
  FRecurseUnversioned := ARecurseUnversioned;
  FreeOnTerminate := True;
  inherited Create(False);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnCheckModificationsThread.Destroy;

begin
  FDirectories.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnStatusListView private }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.CheckUnversionedConflictFiles;

var
  I: Integer;
  ConflictItem: TSvnItem;
  Node: PVirtualNode;
  Data: PNodeData;
  NewFileFound, OldFileFound, WorkingFileFound: Boolean;

begin
  if not Assigned(FConflicts) then
    Exit;

  for I := 0 to FConflicts.Count - 1 do
  begin
    ConflictItem := TSvnItem(FConflicts.Objects[I]);

    NewFileFound := False;
    OldFileFound := False;
    WorkingFileFound := False;
    Node := FrameSvnListView.Tree.GetFirst;
    while Assigned(Node) do
    begin
      Data := FrameSvnListView.Tree.GetNodeData(Node);
      if Assigned(Data) and Assigned(Data^.Item) then
      begin
        if not Data^.Hide and not NewFileFound then
        begin
          NewFileFound := AnsiSameText(Data^.Item.PathName, ExtractFilePath(ConflictItem.PathName) +
            ConflictItem.ConflictNewFile);
          if NewFileFound then
            Data^.Hide := True;
        end;

        if not Data^.Hide and not OldFileFound then
        begin
          OldFileFound := AnsiSameText(Data^.Item.PathName, ExtractFilePath(ConflictItem.PathName) +
            ConflictItem.ConflictOldFile);
          if OldFileFound then
            Data^.Hide := True;
        end;

        if not Data^.Hide and not WorkingFileFound then
        begin
          WorkingFileFound := AnsiSameText(Data^.Item.PathName, ExtractFilePath(ConflictItem.PathName) +
            ConflictItem.ConflictWorkingFile);
          if WorkingFileFound then
            Data^.Hide := True;
        end;
      end;

      if NewFileFound and OldFileFound and WorkingFileFound then
        Break;

      Node := FrameSvnListView.Tree.GetNext(Node);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.ItemDestroy(Sender: TObject);

var
  Data: PNodeData;

begin
  Data := Pointer(TSvnItem(Sender).Tag);
  if Assigned(Data) then
    Data^.Item := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.SetShowUnversioned(Value: Boolean);

begin
  if Value <> FShowUnversioned then
  begin
    FShowUnversioned := Value;
    ShowUnversionedChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.ShowUnversionedChanged;

var
  Node: PVirtualNode;
  Data: PNodeData;

begin
  FrameSvnListView.Tree.BeginUpdate;
  try
    FrameSvnListView.Tree.Cursor := crHourGlass;
    Node := FrameSvnListView.Tree.GetFirst;
    while Assigned(Node) do
    begin
      Data := FrameSvnListView.Tree.GetNodeData(Node);
      if Assigned(Data) and Assigned(Data^.Item) and (Data^.Item.TextStatus = svnWcStatusUnversioned) then
        FrameSvnListView.Tree.IsVisible[Node] := FShowUnversioned and not Data^.Hide;

      Node := FrameSvnListView.Tree.GetNext(Node);
    end;
  finally
    FrameSvnListView.Tree.EndUpdate;
    FrameSvnListView.Tree.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.AMUpdate(var Message: TMessage);

var
  Node: PVirtualNode;
  Data: PNodeData;
  TextWidth: Integer;

begin
  inherited;

  if Message.WParam <> 0 then
  begin
    CheckUnversionedConflictFiles;
    FreeAndNil(FConflicts);
    ShowUnversionedChanged;
    Finished;
  end;

  if not Running then
    FrameSvnListView.Tree.Cursor := crDefault;

  if FStatusException <> '' then
    raise Exception.Create(FStatusException);

  if Running and Assigned(FItem) and (FItem.TextStatus <> svnWcStatusExternal) then
  begin
    if FItem.TextStatus = svnWcStatusConflicted then
    begin
      if not Assigned(FConflicts) then
        FConflicts := TStringList.Create;
      FConflicts.AddObject(FItem.PathName, FItem);
    end;

    Node := FrameSvnListView.AddItem(FItem);
    if Assigned(Node) then
    begin
      Data := FrameSvnListView.Tree.GetNodeData(Node);
      FItem.Tag := Integer(Data);
      FItem.AddDestroyNotification(ItemDestroy);

      if FItem.TextStatus = svnWcStatusUnversioned then
        FrameSvnListView.Tree.IsVisible[Node] := False
      else
      begin
        FrameSvnListView.Tree.ScrollIntoView(Node, False);
        TextWidth := FrameSvnListView.Tree.Canvas.TextWidth(FItem.PathName);
        with FrameSvnListView do
          if TextWidth + Tree.Images.Width + 16 > Tree.Header.Columns[Ord(cxPathName)].Width then
            Tree.Header.Columns[Ord(cxPathName)].Width := TextWidth + Tree.Images.Width + 16;
      end;
    end
    else
      FreeAndNil(FItem);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnStatusListView public }

//----------------------------------------------------------------------------------------------------------------------

constructor TFrameSvnStatusListView.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FItem := nil;
  FConflicts := nil;
  FrameSvnListView.Tree.Images := SvnImageModule.ShellImagesSmall;
  FrameSvnListView.FullPaths := True;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TFrameSvnStatusListView.Destroy;

begin
  FConflicts.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleAddExecute(Action: TAction);

var
  SelectedNodes: TNodeArray;
  I: Integer;
  Data: PNodeData;
  PathNames: TStringList;

begin
  if SvnIDEModule.Settings.ConfirmAdd and (MessageDlg(SConfirmAdd, mtConfirmation, [mbYes, mbNo], 0 {$IFDEF COMPILER_10_UP}, mbNo {$ENDIF}) <> mrYes) then
    Exit;
    
  PathNames := TStringList.Create;
  try
    SelectedNodes := FrameSvnListView.Tree.GetSortedSelection(True);
    for I := Low(SelectedNodes) to High(SelectedNodes) do
    begin
      Data := FrameSvnListView.Tree.GetNodeData(SelectedNodes[I]);
      if not Assigned(Data) or not Assigned(Data^.Item) or (Data^.Item.TextStatus <> svnWcStatusUnversioned) then
        Exit;
      PathNames.AddObject(Data^.Item.PathName, Pointer(SelectedNodes[I]));
    end;

    // sort to make sure parents are added before children
    PathNames.Sort;
    for I := 0 to PathNames.Count - 1 do
    begin
      SvnIDEModule.SvnClient.Add(PathNames[I]);
      Data := FrameSvnListView.Tree.GetNodeData(Pointer(PathNames.Objects[I]));
      Data^.Item.ReloadStatus;
      FrameSvnListView.Tree.InvalidateNode(Pointer(PathNames.Objects[I]))
    end;
  finally
    PathNames.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleAddUpdate(Action: TAction);

var
  SelectedNodes: TNodeArray;
  I: Integer;
  Data: PNodeData;

begin
  Action.Visible := True;
  Action.Enabled := False;

  SelectedNodes := FrameSvnListView.Tree.GetSortedSelection(True);
  if Length(SelectedNodes) = 0 then
    Exit;

  for I := Low(SelectedNodes) to High(SelectedNodes) do
  begin
    Data := FrameSvnListView.Tree.GetNodeData(SelectedNodes[I]);
    if not Assigned(Data) or not Assigned(Data^.Item) or (Data^.Item.TextStatus <> svnWcStatusUnversioned) then
      Exit;
  end;
  Action.Enabled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleMergeConflictsExecute(Action: TAction);

var
  SelectedNodes: TNodeArray;
  Data: PNodeData;

begin
  SelectedNodes := FrameSvnListView.Tree.GetSortedSelection(True);
  if Length(SelectedNodes) <> 1 then
    Exit;

  Data := FrameSvnListView.Tree.GetNodeData(SelectedNodes[Low(SelectedNodes)]);
  if not Assigned(Data) or not Assigned(Data^.Item) or (Data^.Item.TextStatus <> svnWcStatusConflicted) then
    Exit;

  SvnIDEModule.ShowConflicts(Data^.Item.PathName);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleMergeConflictsUpdate(Action: TAction);

var
  SelectedNodes: TNodeArray;
  Data: PNodeData;

begin
  Action.Visible := False;
  Action.Enabled := False;

  SelectedNodes := FrameSvnListView.Tree.GetSortedSelection(True);
  if Length(SelectedNodes) <> 1 then
    Exit;

  Data := FrameSvnListView.Tree.GetNodeData(SelectedNodes[Low(SelectedNodes)]);
  if not Assigned(Data) or not Assigned(Data^.Item) or (Data^.Item.TextStatus <> svnWcStatusConflicted) then
    Exit;

  Action.Enabled := True;
  Action.Visible := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleOpenExecute(Action: TAction);

var
  SelectedNodes: TNodeArray;
  I: Integer;
  Data: PNodeData;

begin
  SelectedNodes := FrameSvnListView.Tree.GetSortedSelection(True);
  for I := Low(SelectedNodes) to High(SelectedNodes) do
  begin
    Data := FrameSvnListView.Tree.GetNodeData(SelectedNodes[I]);
    if Assigned(Data) and Assigned(Data^.Item) then
      SvnIDEModule.ShowEditor(Data^.Item.PathName);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleOpenUpdate(Action: TAction);

begin
  Action.Visible := True;
  Action.Enabled := FrameSvnListView.Tree.SelectedCount > 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleShowBlameExecute(Action: TAction);

var
  Data: PNodeData;

begin
  if FrameSvnListView.Tree.SelectedCount <> 1 then
    Exit;

  Data := FrameSvnListView.Tree.GetNodeData(FrameSvnListView.Tree.GetFirstSelected);
  if Assigned(Data) and Assigned(Data^.Item) and (Data^.Item.Kind = svnNodeFile) and
    FileExists(Data^.Item.PathName) then
    SvnIDEModule.ShowBlame(Data^.Item.PathName);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleShowBlameUpdate(Action: TAction);

begin
  Action.Enabled := FrameSvnListView.Tree.SelectedCount = 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleShowDiffExecute(Action: TAction);

var
  SelectedNodes: TNodeArray;
  I: Integer;
  Data: PNodeData;

begin
  SelectedNodes := FrameSvnListView.Tree.GetSortedSelection(True);
  for I := Low(SelectedNodes) to High(SelectedNodes) do
  begin
    Data := FrameSvnListView.Tree.GetNodeData(SelectedNodes[I]);
    if Assigned(Data) and Assigned(Data^.Item) then
      SvnIDEModule.ShowDiff(Data^.Item.PathName, -1, Data^.Item.CommittedRevision);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleShowDiffUpdate(Action: TAction);

begin
  Action.Visible := True;
  Action.Enabled := FrameSvnListView.Tree.SelectedCount > 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleShowUnversionedExecute(Action: TAction);

begin
  SetShowUnversioned(not FShowUnversioned);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.HandleShowUnversionedUpdate(Action: TAction);

begin
  Action.Visible := True;
  Action.Enabled := True;
  Action.Checked := FShowUnversioned;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.StartCheckModifications(AClient: TSvnClient; ADirectories: TStrings;
  ARecurse, AUpdate, AIgnoreExternals, ARecurseUnversioned: Boolean);

begin
  Starting;
  FItem := nil;
  FreeAndNil(FConflicts);
  FStatusException := '';
  FrameSvnListView.Tree.Clear;
  FrameSvnListView.Tree.Header.SortColumn := NoColumn;
  FrameSvnListView.Tree.Cursor := crHourGlass;
  TSvnCheckModificationsThread.Create(Self, AClient, ADirectories, ARecurse, AUpdate, AIgnoreExternals,
    ARecurseUnversioned);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnStatusListView event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnStatusListView.FrameSvnListViewTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  Data: PNodeData;

begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Data^.Item.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
