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
{ The Original Code is SvnToolForm.pas.                                                                                }
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
{ This unit contains a dockable IDE form which provides user interface for running Subversion commands.                }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnToolForm;

interface

{$INCLUDE Compilers.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Forms, Controls, ComCtrls, ExtCtrls, ToolWin,
  Menus, ActnList, ActnPopup, Dialogs, IniFiles,
  Docktoolform,
  VirtualTrees,
  SvnClient, SvnBaseFrame;

type
  TColumnState = record
    Pos: Integer;
    Width: Integer;
    Visible: Boolean;
  end;
  TColumnStateArray = array of TColumnState;

  TFrameSettings = class
  private
    FColumns: TColumnStateArray;
    FName: string;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure Load(Desktop: TCustomIniFile); overload;
    procedure Load(Tree: TBaseVirtualTree); overload;
    procedure Load(Frame: TFrameSvnBase); overload;
    procedure Save(Desktop: TCustomIniFile); overload;
    procedure Save(Tree: TBaseVirtualTree); overload;
    procedure Save(Frame: TFrameSvnBase); overload;
  end;

  TFormSvnTools = class(TDockableToolbarForm)
    ActionAdd: TAction;
    ActionMergeConflicts: TAction;
    ActionOpen: TAction;
    ActionShowBlame: TAction;
    ActionShowDiff: TAction;
    ActionShowUnversioned: TAction;
    MenuAdd: TMenuItem;
    MenuDockable: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSeparator1: TMenuItem;
    MenuShowBlame: TMenuItem;
    MenuShowDiff: TMenuItem;
    MenuShowUnversioned: TMenuItem;
    MenuStayOnTop: TMenuItem;
    ToolButtonCancel: TToolButton;
    ToolButtonCheckModifications: TToolButton;
    ToolButtonCleanup: TToolButton;
    ToolButtonCommit: TToolButton;
    ToolButtonOptions: TToolButton;
    ToolButtonRevert: TToolButton;
    ToolButtonSeparator1: TToolButton;
    ToolButtonUpdate: TToolButton;
    MenuMergeConflicts: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure ActionAddExecute(Sender: TObject);
    procedure ActionAddUpdate(Sender: TObject);
    procedure ActionMergeConflictsExecute(Sender: TObject);
    procedure ActionMergeConflictsUpdate(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionOpenUpdate(Sender: TObject);
    procedure ActionShowBlameExecute(Sender: TObject);
    procedure ActionShowBlameUpdate(Sender: TObject);
    procedure ActionShowDiffExecute(Sender: TObject);
    procedure ActionShowDiffUpdate(Sender: TObject);
    procedure ActionShowUnversionedExecute(Sender: TObject);
    procedure ActionShowUnversionedUpdate(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    FFrame: TFrameSvnBase;
    FNotifyFrameSettings: TFrameSettings;
    FStatusFrameSettings: TFrameSettings;

    procedure FrameSvnCheckModificationsTreeDblClick(Sender: TObject);
    procedure SaveFrameSettings;
  public
    procedure Cancel;
    function IsBusy: Boolean;
    {$IFDEF COMPILER_9_UP}
    procedure LoadWindowState(Desktop: TCustomIniFile); override;
    procedure SaveWindowState(Desktop: TCustomIniFile; IsProject: Boolean); override;
    {$ELSE}
    procedure LoadWindowState(Desktop: TMemIniFile); override;
    procedure SaveWindowState(Desktop: TMemIniFile; IsProject: Boolean); override;
    {$ENDIF}
    procedure StartSvnCheckModifications(AClient: TSvnClient; APathNames: TStrings; ARecurse: Boolean = True;
      AUpdate: Boolean = True; AIgnoreExternals: Boolean = False; ARecurseUnversioned: Boolean = False);
    procedure StartSvnCleanup(AClient: TSvnClient; APathNames: TStrings);
    procedure StartSvnCommit(AClient: TSvnClient; APathNames: TStrings; const ALogMessage: string;
      ARecurse: Boolean = True; AKeepLocks: Boolean = False; ASeparateCommits: Boolean = False);
    procedure StartSvnRevert(AClient: TSvnClient; APathNames: TStrings; ARecurse: Boolean = True);
    procedure StartSvnUpdate(AClient: TSvnClient; APathNames: TStrings; ARecurse: Boolean = True;
      AIgnoreExternals: Boolean = False);
  end;

const
  SvnToolFormSection = 'SvnToolForm';

resourcestring
  SConfirmCancel = 'Cancel current operation?';

var
  FormSvnTools: TFormSvnTools = nil;

implementation

uses
  ToolsAPI,
  SvnIDEClient, SvnImages, SvnListView, SvnSimpleFrame, SvnNotifyFrame, SvnStatusListViewFrame;

{$R *.dfm}

type
  THackBaseVirtualTree = class(TBaseVirtualTree);

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSettings public }

//----------------------------------------------------------------------------------------------------------------------

constructor TFrameSettings.Create(const AName: string);

begin
  inherited Create;
  FColumns := nil;
  FName := AName;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TFrameSettings.Destroy;

begin
  FColumns := nil;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSettings.Load(Desktop: TCustomIniFile);

var
  Values: TStringList;
  Section: string;
  Count, I, Val: Integer;

begin
  Section := Format('SvnToolForm.%s', [FName]);
  Count := Desktop.ReadInteger(Section, 'ColumnCount', 0);
  SetLength(FColumns, Count);
  if Count = 0 then
    Exit;

  Values := TStringList.Create;
  try
    for I := 0 to Count - 1 do
    begin
      Values.CommaText := Desktop.ReadString(Section, Format('Column%d', [I]), '');

      if (Values.Count > 0) and TryStrToInt(Values[0], Val) then
        FColumns[I].Pos := Val
      else
        FColumns[I].Pos := I;
      if (Values.Count > 1) and TryStrToInt(Values[1], Val) then
        FColumns[I].Width := Val
      else
        FColumns[I].Width := 50;
      if (Values.Count > 2) and TryStrToInt(Values[2], Val) then
        FColumns[I].Visible := Val <> 0;
    end;
  finally
    Values.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSettings.Load(Tree: TBaseVirtualTree);

var
  Count, I: Integer;
  Column: TVirtualTreeColumn;

begin
  Count := THackBaseVirtualTree(Tree).Header.Columns.Count;
  SetLength(FColumns, Count);
  for I := 0 to Count - 1 do
  begin
    Column := THackBaseVirtualTree(Tree).Header.Columns[I];
    FColumns[I].Pos := Column.Position;
    FColumns[I].Width := Column.Width;
    FColumns[I].Visible := coVisible in Column.Options;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSettings.Load(Frame: TFrameSvnBase);

begin
  if Frame is TFrameSvnStatusListView then
    Load(TFrameSvnStatusListView(Frame).FrameSvnListView.Tree)
  else if Frame is TFrameSvnNotify then
    Load(TFrameSvnNotify(Frame).Tree);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSettings.Save(Desktop: TCustomIniFile);

var
  Section: string;
  I: Integer;

begin
  Section := Format('SvnToolForm.%s', [FName]);
  Desktop.EraseSection(Section);
  Desktop.WriteInteger(Section, 'ColumnCount', Length(FColumns));
  for I := Low(FColumns) to High(FColumns) do
    Desktop.WriteString(Section, Format('Column%d', [I]), Format('%d,%d,%d',
      [FColumns[I].Pos, FColumns[I].Width, Ord(FColumns[I].Visible)]));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSettings.Save(Tree: TBaseVirtualTree);

var
  Columns: TVirtualTreeColumns;
  I: Integer;

begin
  Columns := THackBaseVirtualTree(Tree).Header.Columns;
  for I := 0 to Columns.Count - 1 do
  begin
    if I > High(FColumns) then
      Break;
    with Columns[I] do
    begin
      Position := FColumns[I].Pos;
      Width := FColumns[I].Width;
      if FColumns[I].Visible then
        Options := Options + [coVisible]
      else
        Options := Options - [coVisible];
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSettings.Save(Frame: TFrameSvnBase);

begin
  if Frame is TFrameSvnStatusListView then
    Save(TFrameSvnStatusListView(Frame).FrameSvnListView.Tree)
  else if Frame is TFrameSvnNotify then
    Save(TFrameSvnNotify(Frame).Tree);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFormSvnTools private }

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.FrameSvnCheckModificationsTreeDblClick(Sender: TObject);

begin
  ActionOpen.Execute;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.SaveFrameSettings;

begin
  if FFrame is TFrameSvnNotify then
    FNotifyFrameSettings.Load(FFrame)
  else if FFrame is TFrameSvnStatusListView then
    FStatusFrameSettings.Load(FFrame);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFormSvnTools public }

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.Cancel;

begin
  if Assigned(FFrame) and (MessageDlg(SConfirmCancel, mtConfirmation, [mbYes, mbNo], 0 {$IFDEF COMPILER_10_UP}, mbNo {$ENDIF}) = mrYes) then
    FFrame.Cancel;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFormSvnTools.IsBusy: Boolean;

begin
  Result := Assigned(FFrame) and FFrame.Running;
end;

//----------------------------------------------------------------------------------------------------------------------

{$IFDEF COMPILER_9_UP}
procedure TFormSvnTools.LoadWindowState(Desktop: TCustomIniFile);
{$ELSE}
procedure TFormSvnTools.LoadWindowState(Desktop: TMemIniFile);
{$ENDIF}

begin
  inherited LoadWindowState(Desktop);
  FNotifyFrameSettings.Load(Desktop);
  FStatusFrameSettings.Load(Desktop);
  if FFrame is TFrameSvnNotify then
    FNotifyFrameSettings.Save(TFrameSvnNotify(FFrame).Tree)
  else if FFrame is TFrameSvnStatusListView then
    FStatusFrameSettings.Save(TFrameSvnStatusListView(FFrame).FrameSvnListView.Tree);
end;

//----------------------------------------------------------------------------------------------------------------------

{$IFDEF COMPILER_9_UP}
procedure TFormSvnTools.SaveWindowState(Desktop: TCustomIniFile; IsProject: Boolean);
{$ELSE}
procedure TFormSvnTools.SaveWindowState(Desktop: TMemIniFile; IsProject: Boolean);
{$ENDIF}

begin
  inherited SaveWindowState(Desktop, IsProject);
  if FFrame is TFrameSvnNotify then
    FNotifyFrameSettings.Load(TFrameSvnNotify(FFrame).Tree)
  else if FFrame is TFrameSvnStatusListView then
    FStatusFrameSettings.Load(TFrameSvnStatusListView(FFrame).FrameSvnListView.Tree);
  FNotifyFrameSettings.Save(Desktop);
  FStatusFrameSettings.Save(Desktop);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.StartSvnCheckModifications(AClient: TSvnClient; APathNames: TStrings;
  ARecurse, AUpdate, AIgnoreExternals, ARecurseUnversioned: Boolean);

begin
  if not (FFrame is TFrameSvnStatusListView) then
  begin
    SaveFrameSettings;
    FreeAndNil(FFrame);
    FFrame := TFrameSvnStatusListView.Create(Self);
    FFrame.Parent := Self;
    FFrame.Align := alClient;
    TFrameSvnStatusListView(FFrame).FrameSvnListView.Tree.OnDblClick := FrameSvnCheckModificationsTreeDblClick;
    FStatusFrameSettings.Save(FFrame);
  end;

  TFrameSvnStatusListView(FFrame).StartCheckModifications(AClient, APathNames, ARecurse, AUpdate, AIgnoreExternals,
    ARecurseUnversioned);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.StartSvnCleanup(AClient: TSvnClient; APathNames: TStrings);

begin
  if not (FFrame is TFrameSvnSimple) then
  begin
    SaveFrameSettings;
    FreeAndNil(FFrame);
    FFrame := TFrameSvnSimple.Create(Self);
    FFrame.Parent := Self;
    FFrame.Align := alClient;
  end;

  TFrameSvnSimple(FFrame).StartCleanup(AClient, APathNames);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.StartSvnCommit(AClient: TSvnClient; APathNames: TStrings; const ALogMessage: string;
  ARecurse, AKeepLocks, ASeparateCommits: Boolean);

begin
  if not (FFrame is TFrameSvnNotify) then
  begin
    SaveFrameSettings;
    FreeAndNil(FFrame);
    FFrame := TFrameSvnNotify.Create(Self);
    FFrame.Parent := Self;
    FFrame.Align := alClient;
    FNotifyFrameSettings.Save(FFrame);
  end;

  TFrameSvnNotify(FFrame).StartCommit(AClient, APathNames, ALogMessage, ARecurse, AKeepLocks, ASeparateCommits);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.StartSvnRevert(AClient: TSvnClient; APathNames: TStrings; ARecurse: Boolean);

begin
  if not (FFrame is TFrameSvnNotify) then
  begin
    SaveFrameSettings;
    FreeAndNil(FFrame);
    FFrame := TFrameSvnNotify.Create(Self);
    FFrame.Parent := Self;
    FFrame.Align := alClient;
    FNotifyFrameSettings.Save(FFrame);
  end;

  TFrameSvnNotify(FFrame).StartRevert(AClient, APathNames, ARecurse);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.StartSvnUpdate(AClient: TSvnClient; APathNames: TStrings; ARecurse, AIgnoreExternals: Boolean);

begin
  if not (FFrame is TFrameSvnNotify) then
  begin
    SaveFrameSettings;
    FreeAndNil(FFrame);
    FFrame := TFrameSvnNotify.Create(Self);
    FFrame.Parent := Self;
    FFrame.Align := alClient;
    FNotifyFrameSettings.Save(FFrame);
  end;
  
  TFrameSvnNotify(FFrame).StartUpdate(AClient, APathNames, ARecurse, AIgnoreExternals);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFormSvnTools event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.FormCreate(Sender: TObject);

begin
  inherited;
  FNotifyFrameSettings := TFrameSettings.Create('NotifyFrame');
  FStatusFrameSettings := TFrameSettings.Create('StatusFrame');
  Icon := SvnImageModule.Icon;
  DeskSection := SvnToolFormSection;
  AutoSave := True;
  {$IFDEF COMPILER_9_UP}
  VisibleOnUndock := True;
  {$ENDIF}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.FormDestroy(Sender: TObject);

begin
  FNotifyFrameSettings.Free;
  FStatusFrameSettings.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionAddExecute(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleAddExecute(Sender as TAction);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionAddUpdate(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleAddUpdate(Sender as TAction);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionMergeConflictsExecute(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleMergeConflictsExecute(Sender as TAction);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionMergeConflictsUpdate(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleMergeConflictsUpdate(Sender as TAction)
  else
    (Sender as TAction).Visible := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionOpenExecute(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleOpenExecute(Sender as TAction);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionOpenUpdate(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleOpenUpdate(Sender as TAction)
  else
    (Sender as TAction).Visible := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionShowBlameExecute(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleShowBlameExecute(Sender as TAction);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionShowBlameUpdate(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleShowBlameUpdate(Sender as TAction);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionShowDiffExecute(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleShowDiffExecute(Sender as TAction);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionShowDiffUpdate(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleShowDiffUpdate(Sender as TAction)
  else
    (Sender as TAction).Visible := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionShowUnversionedExecute(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleShowUnversionedExecute(Sender as TAction);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.ActionShowUnversionedUpdate(Sender: TObject);

begin
  inherited;
  if Assigned(FFrame) then
    FFrame.HandleShowUnversionedUpdate(Sender as TAction)
  else
    (Sender as TAction).Visible := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnTools.PopupMenu1Popup(Sender: TObject);

begin
  inherited;
  MenuSeparator1.Visible := FFrame is TFrameSvnStatusListView;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
