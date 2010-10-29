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
{ The Original Code is SvnEditorViewFrame.pas.                                                                         }
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
{ This unit contains the frame class used in the Subversion editor view.                                               }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnEditorViewFrame;

interface

{$INCLUDE Compilers.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ImgList, ActnList, StdCtrls,
  ComCtrls, ToolWin, Grids,
  SvnClient;

const
  AM_RELOAD = WM_USER + $100;

type
  TFrameSvnEditorView = class(TFrame)
    ActionCheckModifications: TAction;
    ActionDiff: TAction;
    ActionList: TActionList;
    ActionReload: TAction;
    ComboBox: TComboBox;
    EditBaseRevision: TEdit;
    EditCommitAuthor: TEdit;
    EditCommittedRevision: TEdit;
    EditCommitTime: TEdit;
    EditFileName: TEdit;
    EditLastCommitAuthor: TEdit;
    EditLastCommitRevision: TEdit;
    EditLastCommitTime: TEdit;
    EditLockOwner: TEdit;
    EditLockTime: TEdit;
    EditPropStatus: TEdit;
    EditRemotePropStatus: TEdit;
    EditRemoteTextStatus: TEdit;
    EditRepository: TEdit;
    EditTextStatus: TEdit;
    EditURL: TEdit;
    GroupBoxPropertyStatus: TGroupBox;
    GroupBoxTextStatus: TGroupBox;
    ImageList: TImageList;
    LabelBaseRevision: TLabel;
    LabelCommitAuthor: TLabel;
    LabelCommittedRevision: TLabel;
    LabelCommitTime: TLabel;
    LabelFileName: TLabel;
    LabelLastCommitAuthor: TLabel;
    LabelLastCommitRevision: TLabel;
    LabelLastCommitTime: TLabel;
    LabelLockComment: TLabel;
    LabelLockOwner: TLabel;
    LabelLockTime: TLabel;
    LabelProps: TLabel;
    LabelPropStatus: TLabel;
    LabelRemotePropStatus: TLabel;
    LabelRemoteTextStatus: TLabel;
    LabelRepository: TLabel;
    LabelTextStatus: TLabel;
    LabelURL: TLabel;
    MemoLockComment: TMemo;
    MemoProps: TMemo;
    ScrollBox: TScrollBox;
    ToolBar: TToolBar;
    ToolButtonReload: TToolButton;
    ToolButton1: TToolButton;
    ToolButtonSep: TToolButton;
    ToolButtonDiff: TToolButton;

    procedure ActionCheckModificationsExecute(Sender: TObject);
    procedure ActionDiffExecute(Sender: TObject);
    procedure ActionDiffUpdate(Sender: TObject);
    procedure ActionReloadExecute(Sender: TObject);
    procedure ComboBoxClick(Sender: TObject);
    procedure MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  private
    FItems: TSvnItemArray;

    function GetCurrentItem: TSvnItem;

    procedure AMReload(var Message: TMessage); message AM_RELOAD;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    procedure Display(Item: TSvnItem); overload;
    procedure Display(const Items: TSvnItemArray); overload;
    procedure FreeItems;
  end;

implementation

uses
  SvnIDEClient,
  svn_client;

{$R *.dfm}

resourcestring
  SProperty = 'Property';
  SValue = 'Value';

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnEditorView private }

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnEditorView.GetCurrentItem: TSvnItem;

begin
  Result := nil;
  if Length(FItems) = 0 then
    Exit;

  if Length(FItems) = 1 then
    Result := FItems[0]
  else if ComboBox.ItemIndex <> -1 then
    Result := FItems[ComboBox.ItemIndex];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.AMReload(var Message: TMessage);

begin
  inherited;
  if (Message.WParam >= Low(FItems)) and (Message.WParam <= High(FItems)) then
    Display(FItems[Message.WParam]);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnEditorView public }

//----------------------------------------------------------------------------------------------------------------------

constructor TFrameSvnEditorView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF COMPILER10_UP}
  ToolBar.DrawingStyle := dsGradient;
  ToolBar.GradientEndColor := clBtnFace;
  ToolBar.GradientStartColor := clWhite;
  {$ENDIF}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.Clear;

var
  I: Integer;

begin
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TCustomEdit then
      TCustomEdit(Components[I]).Text := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.Display(Item: TSvnItem);

var
  I: Integer;

begin
  Clear;

  if Assigned(Item) then
  begin
    EditFileName.Text := Item.PathName;
    EditURL.Text := Item.URL;
    EditRepository.Text := Item.Repository;
    EditTextStatus.Text := StatusKindStr(Item.TextStatus);
    EditRemoteTextStatus.Text := StatusKindStr(Item.RemoteTextStatus);
    EditPropStatus.Text := StatusKindStr(Item.PropStatus);
    EditRemotePropStatus.Text := StatusKindStr(Item.RemotePropStatus);
    if Item.BaseRevision > 0 then
      EditBaseRevision.Text := IntToStr(Item.BaseRevision);
    if Item.CommittedRevision > 0 then
      EditCommittedRevision.Text := IntToStr(Item.CommittedRevision);
    EditCommitAuthor.Text := Item.CommitAuthor;
    if Item.CommitTime <> 0 then
      EditCommitTime.Text := FormatDateTime(LongTimeFormat + ', ' + LongDateFormat, Item.CommitTime);
    if Item.LastCommitRevision <> -1 then
      EditLastCommitRevision.Text := IntToStr(Item.LastCommitRevision);
    EditLastCommitAuthor.Text := Item.LastCommitAuthor;
    if Item.LastCommitTime <> 0 then
      EditLastCommitTime.Text := FormatDateTime(LongTimeFormat + ', ' + LongDateFormat, Item.LastCommitTime);
    EditLockOwner.Text := Item.LockOwner;
    MemoLockComment.Text := Item.LockComment;
    if Item.LockTime <> 0 then
      EditLockTime.Text := FormatDateTime(LongTimeFormat + ', ' + LongDateFormat, Item.LockTime);
    MemoProps.Lines.BeginUpdate;
    try
      for I := 0 to Item.PropCount - 1 do
        MemoProps.Lines.Add(Format('%s=%s', [Item.PropNames[I], Item.PropValueFromIndex[I]]));
    finally
      MemoProps.Lines.EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.Display(const Items: TSvnItemArray);

var
  I: Integer;

begin
  ComboBox.Clear;
  Clear;
  SetLength(FItems, Length(Items));
  if Length(FItems) = 1 then
  begin
    FItems[0] := Items[0];
    ComboBox.Visible := False;
  end
  else
  begin
    for I := Low(Items) to High(Items) do
    begin
      FItems[I] := Items[I];
      ComboBox.Items.Add(ExtractFileName(FItems[I].PathName));
    end;
    ComboBox.ItemIndex := 0;
    ComboBox.Visible := True;
  end;
  PostMessage(Handle, AM_RELOAD, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.FreeItems;

var
  I: Integer;

begin
  for I := Low(FItems) to High(FItems) - 1 do
    FItems[I].Free;
  FItems := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnEditorView event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.ActionCheckModificationsExecute(Sender: TObject);

var
  I: Integer;

begin
  Screen.Cursor := crHourGlass;
  try
    for I := Low(FItems) to High(FItems) do
      FItems[I].Reload(False, True);
    if Length(FItems) = 1 then
      Display(FItems[0])
    else
      ComboBoxClick(ComboBox);
  finally
    Screen.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.ActionDiffExecute(Sender: TObject);

var
  Item: TSvnItem;

begin
  Item := GetCurrentItem;
  if Assigned(Item) then
    SvnIDEModule.ShowDiff(Item.PathName, -1, Item.CommittedRevision);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.ActionDiffUpdate(Sender: TObject);

var
  Item: TSvnItem;

begin
  Item := GetCurrentItem;
  (Sender as TAction).Enabled := Assigned(Item) and (Item.TextStatus = svnWcStatusModified);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.ActionReloadExecute(Sender: TObject);

var
  I: Integer;

begin
  Screen.Cursor := crHourGlass;
  try
    for I := Low(FItems) to High(FItems) do
      FItems[I].Reload(False);
    if Length(FItems) = 1 then
      Display(FItems[0])
    else
      ComboBoxClick(ComboBox);
  finally
    Screen.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.ComboBoxClick(Sender: TObject);

begin
  if ComboBox.ItemIndex = -1 then
    Clear
  else
    PostMessage(Handle, AM_RELOAD, ComboBox.ItemIndex, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);

begin
  SetFocus;
  with ScrollBox.VertScrollBar do
    Position := Position + Increment;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnEditorView.MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);

begin
  SetFocus;
  with ScrollBox.VertScrollBar do
    Position := Position - Increment;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
