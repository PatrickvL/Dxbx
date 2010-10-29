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
{ The Original Code is SvnNotifyFrame.pas.                                                                             }
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
{ This unit contains a frame to provide visual feedback from Subversion API calls (which use TSvnWCNotifyFunc          }
{ callback) running in separate threads.                                                                               }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnNotifyFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ActnList,
  VirtualTrees,
  svn_client, SvnClient, SvnBaseFrame;

type
  PNodeData = ^TNodeData;
  TNodeData = record
    DirIndex: Integer;
    NodeIndex: Integer;
    Path: string;
    MimeType: string;
    Action: TSvnWCNotifyAction;
    Kind: TSvnNodeKind;
    ContentState: TSvnWCNotifyState;
    PropState: TSvnWCNotifyState;
    Revision: TSvnRevNum;
  end;

  TColumnIndex = (cxAction, cxPath, cxMimeType);

  TFrameSvnNotify = class(TFrameSvnBase)
    Tree: TVirtualStringTree;

    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: {$IFDEF UNICODE}string {$ELSE}WideString {$ENDIF});
    procedure TreeHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure TreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
  private
    FNodeData: TNodeData;
    FNotifyException: string;

    procedure AMUpdate(var Message: TMessage); message AM_UPDATE;
  public
    procedure HandleAddUpdate(Action: TAction); override;
    procedure HandleOpenExecute(Action: TAction); override;
    procedure HandleOpenUpdate(Action: TAction); override;
    procedure HandleShowBlameExecute(Action: TAction); override;
    procedure HandleShowBlameUpdate(Action: TAction); override;
    procedure HandleShowDiffExecute(Action: TAction); override;
    procedure HandleShowDiffUpdate(Action: TAction); override;
    procedure StartCommit(AClient: TSvnClient; APathNames: TStrings; const ALogMessage: string;
      ARecurse: Boolean = True; AKeepLocks: Boolean = False; ASeparateCommits: Boolean = False);
    procedure StartRevert(AClient: TSvnClient; APathNames: TStrings; ARecurse: Boolean = True);
    procedure StartUpdate(AClient: TSvnClient; APathNames: TStrings; ARecurse: Boolean = True;
      AIgnoreExternals: Boolean = False);
  end;

implementation

uses
  SvnIDEClient;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

type
  TSvnNotifyThread = class(TThread)
  private
    FClient: TSvnClient;
    FFrame: TFrameSvnNotify;
    FPathNames: TStrings;

    procedure NotifyCallback(Sender: TObject; const Path, MimeType: string; Action: TSvnWCNotifyAction;
      Kind: TSvnNodeKind; ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum; var Cancel: Boolean);
  public
    constructor Create(AFrame: TFrameSvnNotify; AClient: TSvnClient; APathNames: TStrings);
    destructor Destroy; override;
  end;

  TSvnUpdateThread = class(TSvnNotifyThread)
  private
    FIgnoreExternals: Boolean;
    FRecurse: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AFrame: TFrameSvnNotify; AClient: TSvnClient; APathNames: TStrings;
      ARecurse, AIgnoreExternals: Boolean);
  end;

  TSvnRevertThread = class(TSvnNotifyThread)
  private
    FRecurse: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AFrame: TFrameSvnNotify; AClient: TSvnClient; APathNames: TStrings; ARecurse: Boolean);
  end;

  TSvnCommitThread = class(TSvnNotifyThread)
  private
    FSeparateCommits: Boolean;
    FKeepLocks: Boolean;
    FLogMessage: string;
    FRecurse: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AFrame: TFrameSvnNotify; AClient: TSvnClient; APathNames: TStrings; const ALogMessage: string;
      ARecurse, AKeepLocks, ASeparateCommits: Boolean);
  end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnNotifyThread private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnNotifyThread.NotifyCallback(Sender: TObject; const Path, MimeType: string; Action: TSvnWCNotifyAction;
  Kind: TSvnNodeKind; ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum; var Cancel: Boolean);

begin
  Cancel := FFrame.Cancelled;
  if Cancel then
    Exit;

  FFrame.FNodeData.Path := FClient.SvnPathToNativePath(Path);
  FFrame.FNodeData.MimeType := MimeType;
  FFrame.FNodeData.Action := Action;
  FFrame.FNodeData.Kind := Kind;
  FFrame.FNodeData.ContentState := ContentState;
  FFrame.FNodeData.PropState := PropState;
  FFrame.FNodeData.Revision := Revision;
  SendMessage(FFrame.Handle, AM_UPDATE, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnNotifyThread public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnNotifyThread.Create(AFrame: TFrameSvnNotify; AClient: TSvnClient; APathNames: TStrings);

begin
  FFrame := AFrame;
  FClient := AClient;
  FPathNames := TStringList.Create;
  FPathNames.Assign(APathNames);
  FreeOnTerminate := True;
  inherited Create(False);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnNotifyThread.Destroy;

begin
  FPathNames.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnUpdateThread protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnUpdateThread.Execute;

begin
  try
    FClient.Update(FPathNames, NotifyCallback, FRecurse, FIgnoreExternals);
    PostMessage(FFrame.Handle, AM_UPDATE, 1, 0);
  except
    on E: Exception do
    begin
      FFrame.FNotifyException := E.Message;
      PostMessage(FFrame.Handle, AM_UPDATE, 1, 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnUpdateThread public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnUpdateThread.Create(AFrame: TFrameSvnNotify; AClient: TSvnClient; APathNames: TStrings;
  ARecurse, AIgnoreExternals: Boolean);

begin
  FRecurse := ARecurse;
  FIgnoreExternals := AIgnoreExternals;
  inherited Create(AFrame, AClient, APathNames);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnRevertThread protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnRevertThread.Execute;

begin
  try
    FClient.Revert(FPathNames, NotifyCallback, FRecurse);
    PostMessage(FFrame.Handle, AM_UPDATE, 1, 0);
  except
    on E: Exception do
    begin
      FFrame.FNotifyException := E.Message;
      PostMessage(FFrame.Handle, AM_UPDATE, 1, 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnRevertThread public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnRevertThread.Create(AFrame: TFrameSvnNotify; AClient: TSvnClient; APathNames: TStrings;
  ARecurse: Boolean);

begin
  FRecurse := ARecurse;
  inherited Create(AFrame, AClient, APathNames);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnCommitThread protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnCommitThread.Execute;

var
  PathNames: TStringList;
  I: Integer;

begin
  try
    if FSeparateCommits then
    begin
      PathNames := TStringList.Create;
      try
        for I := 0 to FPathNames.Count - 1 do
        begin
          PathNames.Text := FPathNames[I];
          FClient.Commit(PathNames, FLogMessage, NotifyCallback, FRecurse, FKeepLocks);
        end;
      finally
        PathNames.Free;
      end;
    end
    else
      FClient.Commit(FPathNames, FLogMessage, NotifyCallback, FRecurse, FKeepLocks);
    PostMessage(FFrame.Handle, AM_UPDATE, 1, 0);
  except
    on E: Exception do
    begin
      FFrame.FNotifyException := E.Message;
      PostMessage(FFrame.Handle, AM_UPDATE, 1, 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnCommitThread public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnCommitThread.Create(AFrame: TFrameSvnNotify; AClient: TSvnClient; APathNames: TStrings;
  const ALogMessage: string; ARecurse, AKeepLocks, ASeparateCommits: Boolean);

begin
  FLogMessage := ALogMessage;
  FRecurse := ARecurse;
  FKeepLocks := AKeepLocks;
  FSeparateCommits := ASeparateCommits;
  inherited Create(AFrame, AClient, APathNames);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnNotify private }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.AMUpdate(var Message: TMessage);

var
  Node: PVirtualNode;
  Data: PNodeData;

begin
  inherited;

  if Message.WParam <> 0 then
    Finished;

  if not Running then
    Tree.Cursor := crDefault;

  if FNotifyException <> '' then
    raise Exception.Create(FNotifyException);

  if Running then
  begin
    Node := Tree.AddChild(nil);
    Data := Tree.GetNodeData(Node);
    Data^.DirIndex := FNodeData.DirIndex;
    Data^.NodeIndex := -1;
    Data^.Path := FNodeData.Path;
    Data^.MimeType := FNodeData.MimeType;
    Data^.Action := FNodeData.Action;
    Data^.Kind := FNodeData.Kind;
    Data^.ContentState := FNodeData.ContentState;
    Data^.PropState := FNodeData.PropState;
    Data^.Revision := FNodeData.Revision;
    Tree.ScrollIntoView(Node, False);
    if Data^.Action = svnWcNotifyUpdateCompleted then
      Inc(FNodeData.DirIndex);
  end
  else
  begin
    // store the original order we can revert to when sorting by NoColumn
    Node := Tree.GetFirst;
    while Assigned(Node) do
    begin
      Data := Tree.GetNodeData(Node);
      if Assigned(Data) then
        Data^.NodeIndex := Node^.Index;

      Node := Tree.GetNext(Node);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnNotify public }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.HandleAddUpdate(Action: TAction);

begin
  Action.Visible := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.HandleOpenExecute(Action: TAction);

var
  SelectedNodes: TNodeArray;
  I: Integer;
  Data: PNodeData;

begin
  SelectedNodes := Tree.GetSortedSelection(True);
  for I := Low(SelectedNodes) to High(SelectedNodes) do
  begin
    Data := Tree.GetNodeData(SelectedNodes[I]);
    if Assigned(Data) and FileExists(Data^.Path) then
      SvnIDEModule.ShowEditor(Data^.Path);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.HandleOpenUpdate(Action: TAction);

begin
  Action.Visible := True;
  Action.Enabled := Tree.SelectedCount > 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.HandleShowBlameExecute(Action: TAction);

var
  Data: PNodeData;

begin
  if Tree.SelectedCount <> 1 then
    Exit;

  Data := Tree.GetNodeData(Tree.GetFirstSelected);
  if Assigned(Data) and FileExists(Data^.Path) then
    SvnIDEModule.ShowBlame(Data^.Path);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.HandleShowBlameUpdate(Action: TAction);

begin
  Action.Enabled := Tree.SelectedCount = 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.HandleShowDiffExecute(Action: TAction);

var
  SelectedNodes: TNodeArray;
  I: Integer;
  Data: PNodeData;
  Item: TSvnItem;

begin
  SelectedNodes := Tree.GetSortedSelection(True);
  for I := Low(SelectedNodes) to High(SelectedNodes) do
  begin
    Data := Tree.GetNodeData(SelectedNodes[I]);
    if Assigned(Data) and FileExists(Data^.Path) then
    begin
      Item := TSvnItem.Create(SvnIDEModule.SvnClient, nil, Data^.Path, False, False);
      try
        SvnIDEModule.ShowDiff(Item.PathName, -1, Item.CommittedRevision);
      finally
        Item.Free;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.HandleShowDiffUpdate(Action: TAction);

begin
  Action.Visible := True;
  Action.Enabled := Tree.SelectedCount > 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.StartCommit(AClient: TSvnClient; APathNames: TStrings; const ALogMessage: string;
  ARecurse, AKeepLocks, ASeparateCommits: Boolean);

begin
  Starting;
  FNotifyException := '';
  Tree.Clear;
  Tree.Header.SortColumn := NoColumn;
  Tree.Cursor := crHourGlass;
  TSvnCommitThread.Create(Self, AClient, APathNames, ALogMessage, ARecurse, AKeepLocks, ASeparateCommits);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.StartRevert(AClient: TSvnClient; APathNames: TStrings; ARecurse: Boolean);

begin
  Starting;
  FNotifyException := '';
  Tree.Clear;
  Tree.Header.SortColumn := NoColumn;
  Tree.Cursor := crHourGlass;
  TSvnRevertThread.Create(Self, AClient, APathNames, ARecurse);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.StartUpdate(AClient: TSvnClient; APathNames: TStrings; ARecurse, AIgnoreExternals: Boolean);

begin
  Starting;
  FNotifyException := '';
  Tree.Clear;
  Tree.Header.SortColumn := NoColumn;
  Tree.Cursor := crHourGlass;
  TSvnUpdateThread.Create(Self, AClient, APathNames, ARecurse, AIgnoreExternals);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnNotify event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);

var
  Data1, Data2: PNodeData;
  SortDirection: TSortDirection;

begin
  Result := 0;
  if Running then
    Exit;
    
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  if not Assigned(Data1) or not Assigned(Data2) then
    Exit;

  SortDirection := TVirtualStringTree(Sender).Header.SortDirection;
  case SortDirection of
    sdAscending:
      Result := Data1^.DirIndex - Data2^.DirIndex;
    sdDescending:
      Result := Data2^.DirIndex - Data1^.DirIndex;
  end;
  if Result = 0 then
  begin
    if Data1^.Action = svnWcNotifyUpdateCompleted then
      case TVirtualStringTree(Sender).Header.SortDirection of
        sdAscending:
          Result := 1;
        sdDescending:
          Result := -1;
      end
    else if Data2^.Action = svnWcNotifyUpdateCompleted then
      case TVirtualStringTree(Sender).Header.SortDirection of
        sdAscending:
          Result := -1;
        sdDescending:
          Result := 1;
      end
    else
      case Column of
        cxAction:
          Result := AnsiCompareText(NotifyActionStr(Data1^.Action), NotifyActionStr(Data2^.Action));
        cxPath:
          Result := AnsiCompareText(Data1^.Path, Data2^.Path);
        cxMimeType:
          Result := AnsiCompareText(Data1^.MimeType, Data2^.MimeType);
        else
          Result := Data1^.NodeIndex - Data2^.NodeIndex;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  Data: PNodeData;

begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Finalize(Data^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);

begin
  NodeDataSize := SizeOf(TNodeData);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: {$IFDEF UNICODE}string {$ELSE}WideString {$ENDIF});

var
  Data: PNodeData;

begin
  CellText := '';
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then
    Exit;

  case Column of
    cxAction:
      case TextType of
        ttNormal:
          CellText := NotifyActionStr(Data^.Action);
      end;
    cxPath:
      case TextType of
        ttNormal:
          case Data^.Action of
            svnWcNotifyUpdateCompleted:
              CellText := Format('Revision: %d', [Data^.Revision]);
            else
              CellText := Data^.Path;
          end;
      end;
    cxMimeType:
      case TextType of
        ttNormal:
          CellText := Data^.MimeType;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.TreeHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton;
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
      Sender.SortDirection := sdAscending;
      Sender.SortColumn := Integer(Column);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnNotify.TreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);

var
  Data: PNodeData;

begin
  if Sender.Selected[Node] then
    Exit;
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then
    Exit;
  case Data^.Action of
    svnWcNotifyUpdateDelete:
      TargetCanvas.Font.Color := clMaroon;
    svnWcNotifyUpdateAdd:
      TargetCanvas.Font.Color := clPurple;
    else
      TargetCanvas.Font.Color := Sender.Font.Color;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
