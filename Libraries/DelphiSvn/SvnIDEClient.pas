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
{ The Original Code is SvnIDEClient.pas.                                                                               }
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
{ This unit contains TSvnIDEClient, a utility data module class for Delphi IDE Subversion plugin (to be used as a      }
{ single global instance).                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnIDEClient;

interface

{$INCLUDE Compilers.inc}

uses
  Windows, Classes, SysUtils, Forms, Messages, Graphics, Dialogs, Controls, Menus, ActnList, ActnMenus, ImgList,
  ToolsAPI,
  {$IFDEF COMPILER_9_UP}
  FileHistoryAPI,
  {$ENDIF}
  {$IFNDEF COMPILER_14_UP}
  EditorViewSupport,
  {$ENDIF}
  SynEdit, SynEditHighlighter,
  SvnIDEHistory, Dockform,
  svn_client, SvnClient, SvnDiff3Frame;

type
  TSvnIDESettings = class
  private
    FAllowEmptyCommitMsg: Boolean;
    FCommitExternals: Boolean;
    FConfirmAdd: Boolean;
    FDirectories: string;
    FDirHistory: TStrings;
    FModified: Boolean;
    FRecurseUnversioned: Boolean;

    procedure SetAllowEmptyCommitMsg(Value: Boolean);
    procedure SetCommitExternals(Value: Boolean);
    procedure SetConfirmAdd(Value: Boolean);
    procedure SetDirectories(const Value: string);
    procedure SetRecurseUnversioned(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadSettings;
    procedure SaveSettings;

    property AllowEmptyCommitMsg: Boolean read FAllowEmptyCommitMsg write SetAllowEmptyCommitMsg;
    property CommitExternals: Boolean read FCommitExternals write SetCommitExternals;
    property ConfirmAdd: Boolean read FConfirmAdd write SetConfirmAdd;
    property Directories: string read FDirectories write SetDirectories;
    property DirHistory: TStrings read FDirHistory;
    property Modified: Boolean read FModified;
    property RecurseUnversioned: Boolean read FRecurseUnversioned write SetRecurseUnversioned;
  end;

  TSvnIDEClient = class(TDataModule)
    ActionCancel: TAction;
    ActionCheckModifications: TAction;
    ActionCleanup: TAction;
    ActionCommit: TAction;
    ActionOptions: TAction;
    ActionRevert: TAction;
    Actions: TActionList;
    ActionUpdate: TAction;
    ImageList: TImageList;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionCancelUpdate(Sender: TObject);
    procedure ActionCheckModificationsExecute(Sender: TObject);
    procedure ActionCheckModificationsUpdate(Sender: TObject);
    procedure ActionCleanupExecute(Sender: TObject);
    procedure ActionCleanupUpdate(Sender: TObject);
    procedure ActionCommitExecute(Sender: TObject);
    procedure ActionCommitUpdate(Sender: TObject);
    procedure ActionOptionsExecute(Sender: TObject);
    procedure ActionRevertExecute(Sender: TObject);
    procedure ActionRevertUpdate(Sender: TObject);
    procedure ActionUpdateExecute(Sender: TObject);
    procedure ActionUpdateUpdate(Sender: TObject);
  private
    FEditorView: Pointer;
    FEditorViewIntf: {$IFDEF COMPILER_14_UP}INTACustomEditorSubView {$ELSE}ICustomEditorFrameView {$ENDIF};
    {$IFNDEF COMPILER_9_UP}
    FHistoryView: Pointer;
    FHistoryViewIntf: ICustomEditorFrameView;
    {$ENDIF}
    FHistoryProviderIndex: Integer;
    FMenuItem: TMenuItem;
    FSettings: TSvnIDESettings;
    FSvnClient: TSvnClient;
    FSyncData: Pointer;

    procedure Finalize;
    procedure GetDirectories(Directories: TStrings);
    procedure Initialize;
    procedure InsertBlameControl(Form: TCustomForm);
    procedure SvnClientLoginPrompt(Sender: TObject; const Realm: string; var UserName, Password: string;
      var Cancel, Save: Boolean);
    procedure SvnClientSSLClientCertPrompt(Sender: TObject; const Realm: string; var CertFileName: string;
      var Cancel, Save: Boolean);
    procedure SvnClientSSLClientPasswordPrompt(Sender: TObject; const Realm: string; var Password: string;
      var Cancel, Save: Boolean);
    procedure SvnClientSSLServerTrustPrompt(Sender: TObject; const Realm: string;
      const CertInfo: TSvnAuthSSLServerCertInfo; Failures: TSSLServerTrustFailures; var Cancel, Save: Boolean);
    procedure SvnClientUserNamePrompt(Sender: TObject; const Realm: string; var UserName: string;
      var Cancel, Save: Boolean);

    procedure SyncLoginPrompt;
    procedure SyncSSLClientCertPrompt;
    procedure SyncSSLClientPasswordPrompt;
    procedure SyncSSLServerTrustPrompt;
    procedure SyncUserNamePrompt;
    {$IFDEF COMPILER_9_UP}
    procedure TabSheetShow(Sender: TObject);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FindFirstSvnHistoryNode(Tree: TObject): Pointer;
    function FindSvnHistoryNode(Tree: TObject; Revision: Integer): Pointer;
    procedure FrameSvnDiff3ItemResolved(Sender: TObject; Kind: TResolutionKind);
    function GetCurrentModuleFileName: string;
    function GetEditWindow: TCustomForm;
    function GetModule(const FileName: string): IOTAModule;
    function GetSvnHistory(const FileName: string): ISvnFileHistory;
    function GetSvnHistoryNodeItem(Tree: TObject; Node: Pointer): TSvnHistoryItem;
    procedure InitializeEdit(Edit: TSynEdit; const FileName: string);
    procedure InitializeHighlighter(const FileName: string; var Highlighter: TSynCustomHighlighter);
    {$IFNDEF COMPILER_9_UP}
    function SelectEditorView(const TabCaption: string): Boolean;
    {$ENDIF}
    procedure SetupBlameControl;
    {$IFDEF COMPILER_9_UP}
    procedure SetupDiff3Frame;
    {$ENDIF}
    function ShowBlame(const FileName: string): Boolean;
    function ShowConflicts(const FileName: string): Boolean;
    function ShowDiff(const FileName: string; FromRevision, ToRevision: Integer): Boolean;
    procedure ShowEditor(const FileName: string);
    procedure ShowHistoryEditControls;

    property SvnClient: TSvnClient read FSvnClient;
    property Settings: TSvnIDESettings read FSettings;
  end;

var
  SvnIDEModule: TSvnIDEClient = nil;

resourcestring
  SConfirmAdd = 'Add selected files and directories to Subversion?';
  SConfirmCleanup = 'Clean up working copy directories?';
  SConfirmRevert = 'Revert local changes?';

procedure Register;

implementation

uses
  Registry, ActnMan, StdCtrls, Tabs, ComCtrls, ExtCtrls,
  {$IFNDEF COMPILER_9_UP}
  DesignIntf, VirtualTrees, SvnHistoryManager, SvnHistoryView, SvnHistoryViewFrame,
  {$ENDIF}
  DeskUtil,
  SynHighlighterPas, SynHighlighterCpp, SynHighlighterCS, SynHighlighterHtml, SynHighlighterXML, SynHighlighterSQL,
  SynHighlighterIDL,
  SvnImages, SvnClientLoginPrompt, SvnClientSSLClientCertPrompt, SvnClientSSLServerTrustPrompt, SvnLogMessagePrompt,
  SvnEditorView, SvnOptionsDialog, SvnToolForm;

{$R *.dfm}

type
  THackActionMainMenuBar = class(TActionMainMenuBar);
  THackWinControl = class(TWinControl);

  PHistoryNodeData = ^THistoryNodeData;
  THistoryNodeData = record
    History: IOTAFileHistory;
    Index: Integer;
  end;

  PSyncLoginPrompt = ^TSyncLoginPrompt;
  TSyncLoginPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    UserName: string;
    Password: string;
    Cancel: Boolean;
    Save: Boolean;
  end;

  PSyncSSLClientCertPrompt = ^TSyncSSLClientCertPrompt;
  TSyncSSLClientCertPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    CertFileName: string;
    Cancel: Boolean;
    Save: Boolean;
  end;

  PSyncSSLClientPasswordPrompt = ^TSyncSSLClientPasswordPrompt;
  TSyncSSLClientPasswordPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    Password: string;
    Cancel: Boolean;
    Save: Boolean;
  end;

  PSyncSSLServerTrustPrompt = ^TSyncSSLServerTrustPrompt;
  TSyncSSLServerTrustPrompt = record
    SvnClient: TSvnClient;
    Realm: string;
    CertInfo: TSvnAuthSSLServerCertInfo;
    Failures: TSSLServerTrustFailures;
    Cancel: Boolean;
    Save: Boolean;
  end;

//----------------------------------------------------------------------------------------------------------------------

const
  {$IFDEF COMPILER_14}
  coreide = 'coreide140.bpl';
  vclide = 'vclide140.bpl';
  {$ENDIF}

  {$IFDEF COMPILER_12}
  coreide = 'coreide120.bpl';
  vclide = 'vclide120.bpl';
  {$ENDIF}

  {$IFDEF COMPILER_10}
  coreide = 'coreide100.bpl';
  vclide = 'vclide100.bpl';
  {$ENDIF}

  {$IFDEF COMPILER_9}
  coreide = 'coreide90.bpl';
  vclide = 'vclide90.bpl';
  {$ENDIF}

  {$IFDEF COMPILER_7}
  coreide = 'coreide70.bpl';
  vclide = 'vclide70.bpl';
  {$ENDIF}

  {$IFDEF COMPILER_12_UP}
  SExpandRootMacro = '@Uiutils@ExpandRootMacro$qqrx20System@UnicodeString';
  {$ELSE}
  SExpandRootMacro = '@Uiutils@ExpandRootMacro$qqrx17System@AnsiString';
  {$ENDIF}
  {$IFDEF COMPILER_9_UP}
  SEditControlGetLinesInWindow = '@Editorcontrol@TCustomEditControl@GetLinesInWindow$qqrv';
  SEditControlGetTopLine = '@Editorcontrol@TCustomEditControl@GetTopLine$qqrv';

  SBaseVirtualTreeGetFirst = '@Idevirtualtrees@TBaseVirtualTree@GetFirst$qqrv';
  SBaseVirtualTreeGetFirstSelected = '@Idevirtualtrees@TBaseVirtualTree@GetFirstSelected$qqrv';
  SBaseVirtualTreeGetNext = '@Idevirtualtrees@TBaseVirtualTree@GetNext$qqrp28Idevirtualtrees@TVirtualNode';
  SBaseVirtualTreeGetNodeData = '@Idevirtualtrees@TBaseVirtualTree@GetNodeData$qqrp28Idevirtualtrees@TVirtualNode';
  SBaseVirtualTreeScrollIntoView = '@Idevirtualtrees@TBaseVirtualTree@ScrollIntoView$qqrp28Idevirtualtrees@TVirtualNodeoo';
  SBaseVirtualTreeSetSelected = '@Idevirtualtrees@TBaseVirtualTree@SetSelected$qqrp28Idevirtualtrees@TVirtualNodeo';
  {$ENDIF}

function ExpandRootMacro(const S: string): string; external coreide name SExpandRootMacro;

{$IFDEF COMPILER_9_UP}
function EditControlGetLinesInWindow(Self: TObject): Integer; external coreide name SEditControlGetLinesInWindow;
function EditControlGetTopLine(Self: TObject): Integer; external coreide name SEditControlGetTopLine;
// vclide seems to contain a different version of Virtual TreeView; hence these imports as a workaround
function BaseVirtualTreeGetFirst(Self: TObject): Pointer; external vclide name SBaseVirtualTreeGetFirst;
function BaseVirtualTreeGetFirstSelected(Self: TObject): Pointer; external vclide name SBaseVirtualTreeGetFirstSelected;
function BaseVirtualTreeGetNext(Self: TObject; Node: Pointer): Pointer; external vclide name SBaseVirtualTreeGetNext;
function BaseVirtualTreeGetNodeData(Self: TObject; Node: Pointer): Pointer; external vclide
  name SBaseVirtualTreeGetNodeData;
function BaseVirtualTreeScrollIntoView(Self: TObject; Node: Pointer; Center, Horizontally: Boolean): Boolean;
  external vclide name SBaseVirtualTreeScrollIntoView;
procedure BaseVirtualTreeSetSelected(Self: TObject; Node: Pointer; Value: Boolean);
  external vclide name SBaseVirtualTreeSetSelected;
{$ELSE}
function EditControlGetLinesInWindow(Self: TObject): Integer;
begin
  Result := TSynEdit(Self).LinesInWindow;
end;

function EditControlGetTopLine(Self: TObject): Integer;
begin
  Result := TSynEdit(Self).TopLine;
end;

function BaseVirtualTreeGetFirst(Self: TObject): Pointer;
begin
  Result := TBaseVirtualTree(Self).GetFirst;
end;

function BaseVirtualTreeGetFirstSelected(Self: TObject): Pointer;
begin
  Result := TBaseVirtualTree(Self).GetFirstSelected;
end;

function BaseVirtualTreeGetNext(Self: TObject; Node: Pointer): Pointer;
begin
  Result := TBaseVirtualTree(Self).GetNext(Node);
end;

function BaseVirtualTreeGetNodeData(Self: TObject; Node: Pointer): Pointer;
begin
  Result := TBaseVirtualTree(Self).GetNodeData(Node);
end;

function BaseVirtualTreeScrollIntoView(Self: TObject; Node: Pointer; Center, Horizontally: Boolean): Boolean;
begin
  Result := TBaseVirtualTree(Self).ScrollIntoView(Node, Center, Horizontally);
end;

procedure BaseVirtualTreeSetSelected(Self: TObject; Node: Pointer; Value: Boolean);
begin
  TBaseVirtualTree(Self).Selected[Node] := Value;
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

function FindChildControl(Parent: TWinControl; const ClassName: string): TControl;

var
  I: Integer;

begin
  Result := nil;

  for I := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[I].ClassNameIs(ClassName) then
    begin
      Result := Parent.Controls[I];
      Break;
    end
    else if Parent.Controls[I] is TWinControl then
    begin
      Result := FindChildControl(TWinControl(Parent.Controls[I]), ClassName);
      if Assigned(Result) then
        Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnIDESettings private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDESettings.SetAllowEmptyCommitMsg(Value: Boolean);

begin
  if FAllowEmptyCommitMsg <> Value then
  begin
    FAllowEmptyCommitMsg := Value;
    FModified := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDESettings.SetCommitExternals(Value: Boolean);

begin
  if FCommitExternals <> Value then
  begin
    FCommitExternals := Value;
    FModified := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDESettings.SetConfirmAdd(Value: Boolean);

begin
  if FConfirmAdd <> Value then
  begin
    FConfirmAdd := Value;
    FModified := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDESettings.SetDirectories(const Value: string);

var
  I: Integer;

begin
  if not SameText(FDirectories, Value) then
  begin
    I := FDirHistory.IndexOf(FDirectories);
    if I = -1 then
      FDirHistory.Insert(0, FDirectories)
    else
      FDirHistory.Move(I, 0);

    FDirectories := Value;

    I := FDirHistory.IndexOf(Value);
    if I <> -1 then
      FDirHistory.Delete(I);

    FModified := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDESettings.SetRecurseUnversioned(Value: Boolean);

begin
  if FRecurseUnversioned <> Value then
  begin
    FRecurseUnversioned := Value;
    FModified := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnIDESettings public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnIDESettings.Create;

begin
  inherited Create;
  FDirectories := '';
  FDirHistory := TStringList.Create;
  FAllowEmptyCommitMsg := False;
  FCommitExternals := False;
  FConfirmAdd := True;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnIDESettings.Destroy;

begin
  if FModified then
    SaveSettings;
  FDirHistory.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDESettings.LoadSettings;

var
  Services: IOTAServices;
  Registry: TRegistry;
  SKey: string;
  Count, I: Integer;

begin
  FDirectories := '';
  FDirHistory.Clear;
  if not Assigned(BorlandIDEServices) or
    {$IFDEF COMPILER_9_UP}
    not BorlandIDEServices.GetService(IOTAServices, Services) then
    {$ELSE}
    not Supports(BorlandIDEServices, IOTAServices, Services) then
    {$ENDIF}
    Exit;

  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    SKey := Format('%s\Subversion', [Services.GetBaseRegistryKey]);
    if Registry.OpenKeyReadOnly(SKey) then
    begin
      FDirectories := Registry.ReadString('Directories');
      if Registry.ValueExists('AllowEmptyCommitMsg') then
        FAllowEmptyCommitMsg := Registry.ReadBool('AllowEmptyCommitMsg')
      else
        FAllowEmptyCommitMsg := False;
      if Registry.ValueExists('CommitExternals') then
        FCommitExternals := Registry.ReadBool('CommitExternals')
      else
        FCommitExternals := False;
      if Registry.ValueExists('ConfirmAdd') then
        FConfirmAdd := Registry.ReadBool('ConfirmAdd')
      else
        FConfirmAdd := True;
      if Registry.ValueExists('RecurseUnversioned') then
        FRecurseUnversioned := Registry.ReadBool('RecurseUnversioned')
      else
        FRecurseUnversioned := False;
    end;

    SKey := Format('%s\Subversion\hlDirectories', [Services.GetBaseRegistryKey]);
    if Registry.OpenKeyReadOnly(SKey) then
    begin
      Count := Registry.ReadInteger('Count');
      for I := 0 to Count - 1 do
        FDirHistory.Add(Registry.ReadString(Format('Item%d', [I])));
    end;

    FModified := False;
  finally
    Registry.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDESettings.SaveSettings;

var
  Services: IOTAServices;
  Registry: TRegistry;
  SKey: string;
  I: Integer;

begin
  if not Assigned(BorlandIDEServices) or
    {$IFDEF COMPILER_9_UP}
    not BorlandIDEServices.GetService(IOTAServices, Services) then
    {$ELSE}
    not Supports(BorlandIDEServices, IOTAServices, Services) then
    {$ENDIF}
    Exit;

  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    SKey := Format('%s\Subversion', [Services.GetBaseRegistryKey]);
    if Registry.OpenKey(SKey, True) then
    begin
      Registry.WriteString('Directories', FDirectories);
      Registry.WriteBool('AllowEmptyCommitMsg', FAllowEmptyCommitMsg);
      Registry.WriteBool('CommitExternals', FCommitExternals);
      Registry.WriteBool('ConfirmAdd', FConfirmAdd);
      Registry.WriteBool('RecurseUnversioned', FRecurseUnversioned);
    end;
    SKey := Format('%s\Subversion\hlDirectories', [Services.GetBaseRegistryKey]);
    if Registry.OpenKey(SKey, True) then
    begin
      Registry.WriteInteger('Count', FDirHistory.Count);
      for I := 0 to FDirHistory.Count - 1 do
        Registry.WriteString(Format('Item%d', [I]), FDirHistory[I]);
      I := FDirHistory.Count;
      while Registry.ValueExists(Format('Item%d', [I])) do
      begin
        Registry.DeleteValue(Format('Item%d', [I]));
        Inc(I);
      end;
    end;

    FModified := False;
  finally
    Registry.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  BlameColors: array[0..25] of TColor = (
    $00E0E1F5, //   0 220 120
    $00E0EAF5, //  20 220 120
    $00E0F5F5, //  40 220 120
    $00E0F5EA, //  60 220 120
    $00E1F5E0, //  80 220 120
    $00EAF5E0, // 100 220 120
    $00F5F5E0, // 120 220 120
    $00F5EAE0, // 140 220 120
    $00F5E0E0, // 160 220 120
    $00F5E0EA, // 180 220 120
    $00F5E0F5, // 200 220 120
    $00EAE0F5, // 220 220 120
    $00E0E0F5, // 240 220 120
    $00E0E6F5, //  10 220 120
    $00E0F0F5, //  30 220 120
    $00E0F5F0, //  50 220 120
    $00E0F5E6, //  70 220 120
    $00E6F5E0, //  90 220 120
    $00F0F5E0, // 110 220 120
    $00F5F0E0, // 130 220 120
    $00F5E6E0, // 150 220 120
    $00F5E0E6, // 170 220 120
    $00F5E0F0, // 190 220 120
    $00F0E0F5, // 210 220 120
    $00E6E0F5, // 230 220 120
    $00E0E0E0  // -10 220 120
  );

type
  TBlameControl = class(TCustomControl)
  private
    FAuthors: TStringList;
    FControl: TWinControl;
    FControlProc: TWndMethod;
    FLastItem: TSvnHistoryItem;
    FLastColor: Integer;
    FRevisionX: Integer;
    FTimeX: Integer;

    procedure BlameLoaded(Sender: TObject);
    procedure ControlProc(var Message: TMessage);
    function GetNextColor: TColor;

    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent; AControl: TWinControl); reintroduce;
    destructor Destroy; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

{ TBlameControl private }

//----------------------------------------------------------------------------------------------------------------------

procedure TBlameControl.BlameLoaded(Sender: TObject);

begin
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBlameControl.ControlProc(var Message: TMessage);

begin
  FControlProc(Message);
  case Message.Msg of
    WM_PAINT, WM_KEYDOWN, WM_VSCROLL:
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBlameControl.GetNextColor: TColor;

begin
  Result := BlameColors[FLastColor];
  Inc(FLastColor);
  if FLastColor > High(BlameColors) then
    FLastColor := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBlameControl.CMHintShow(var Message: TMessage);

var
  Tree: TObject;
  I, Index: Integer;
  HistoryItem, BlameHistoryItem: TSvnHistoryItem;
  {$IFDEF COMPILER_9_UP}
  TopLine, LinesInWindow, LineHeight: Integer;
  {$ENDIF}

begin
  if not Assigned(Parent) then
    Exit;

  TCMHintShow(Message).HintInfo^.ReshowTimeout := 500;
  TCMHintShow(Message).HintInfo^.HintStr := '';

  {$IFDEF COMPILER_9_UP}
  Tree := nil;
  for I := 0 to Parent.ControlCount - 1 do
    if SameText(Parent.Controls[I].Name, 'RevisionContentTree') then
    begin
      Tree := Parent.Controls[I];
      Break;
    end;
  {$ELSE}
  Tree := (FControl.Owner as TFrameSvnHistoryView).TreeContent;
  {$ENDIF}
  if not Assigned(Tree) then
    Exit;

  HistoryItem := SvnIDEModule.GetSvnHistoryNodeItem(Tree, BaseVirtualTreeGetFirstSelected(Tree));
  if not Assigned(HistoryItem) then
    Exit;

  {$IFDEF COMPILER_9_UP}
  TopLine := EditControlGetTopLine(FControl);
  LinesInWindow := EditControlGetLinesInWindow(FControl);
  LineHeight := FControl.Height div LinesInWindow;
  Index := TopLine + TCMHintShow(Message).HintInfo^.CursorPos.Y div LineHeight - 1;
  {$ELSE}
  Index := TSynEdit(FControl).PixelsToRowColumn(0, TCMHintShow(Message).HintInfo^.CursorPos.Y).Row - 1;
  {$ENDIF}
  if Index > HistoryItem.BlameCount - 1 then
    Exit;

  BlameHistoryItem := nil;
  if Assigned(HistoryItem.Owner) then
    for I := 0 to HistoryItem.Owner.HistoryCount - 1 do
      if HistoryItem.Owner.HistoryItems[I].Revision = HistoryItem.BlameItems[Index].Revision then
      begin
        BlameHistoryItem := HistoryItem.Owner.HistoryItems[I];
        Break;
      end;
  if Assigned(BlameHistoryItem) then
    TCMHintShow(Message).HintInfo^.HintStr := BlameHistoryItem.LogMessage;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TBlameControl protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TBlameControl.Paint;

var
  Frame: TWinControl;
  {$IFDEF COMPILER_9_UP}
  TabSet1: TTabSet;
  {$ENDIF}
  Tree: TObject;
  I, Index, W: Integer;
  EditorServices: IOTAEditorServices;
  EditOptions: IOTAEditOptions;
  HistoryItem: TSvnHistoryItem;
  TopLine, LinesInWindow, LineHeight, Y: Integer;
  BlameItem: TSvnBlameItem;
  R: TRect;

begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clWindow;
  Canvas.FillRect(ClientRect);

  {$IFDEF COMPILER_9_UP}
  if not Assigned(FControl) or not Assigned(Parent) or // tab sheet
    not Assigned(Parent.Parent) then // page control
    Exit;

  // find tabset and check its index
  Frame := Parent.Parent.Parent;
  if not Assigned(Frame) then
    Exit;
  TabSet1 := TTabSet(Frame.FindComponent('TabSet1'));
  if not Assigned(TabSet1) or (TabSet1.TabIndex <> 0) then
    Exit;

  // find treeview
  Tree := Frame.FindComponent('RevisionContentTree');
  {$ELSE}
  Frame := FControl.Owner as TFrameSvnHistoryView;
  Tree := TFrameSvnHistoryView(Frame).TreeContent;
  {$ENDIF}
  if not Assigned(Tree) then
    Exit;

  HistoryItem := SvnIDEModule.GetSvnHistoryNodeItem(Tree, BaseVirtualTreeGetFirstSelected(Tree));
  if not Assigned(HistoryItem) then
  begin
    FLastItem := nil;
    Exit;
  end;

  if not HistoryItem.HasBlameLoaded then
  begin
    Canvas.Font := Font;
    if HistoryItem.BlameError = '' then
    begin
      Canvas.Font.Color := clGrayText;
      Canvas.TextOut(8, 8, 'loading...');
      if not HistoryItem.IsLoadingBlame then
        HistoryItem.StartLoadingBlame(BlameLoaded);
      Exit;
    end
    else
    begin
      Canvas.Font.Color := clRed;
      Canvas.TextOut(8, 8, HistoryItem.BlameError);
      Exit;
    end;
  end;

  {$IFDEF COMPILER_9_UP}
  if BorlandIDEServices.GetService(IOTAEditorServices, EditorServices) then
  {$ELSE}
  if Supports(BorlandIDEServices, IOTAEditorServices, EditorServices) then
  {$ENDIF}
  begin
    EditOptions := EditorServices.GetEditOptions(cDefEdPascal);
    if Assigned(EditOptions) then
    begin
      Canvas.Font.Name := EditOptions.FontName;
      Canvas.Font.Size := EditOptions.FontSize;
      Canvas.Font.Color := clWindowText;
    end;
  end;

  TopLine := EditControlGetTopLine(FControl);
  LinesInWindow := EditControlGetLinesInWindow(FControl);
  {$IFDEF COMPILER_9_UP}
  LineHeight := FControl.Height div LinesInWindow;
  {$ELSE}
  LineHeight := TSynEdit(FControl).LineHeight;
  // include partially visible last line
  if FControl.Height > LinesInWindow * LineHeight then
    Inc(LinesInWindow);
  {$ENDIF}

  if HistoryItem <> FLastItem then
  begin
    FLastColor := 0;
    FAuthors.Clear;
    FRevisionX := 0;
    FTimeX := 0;
    for I := 0 to HistoryItem.BlameCount - 1 do
    begin
      BlameItem := HistoryItem.BlameItems[I];
      if FAuthors.IndexOf(BlameItem.Author) = -1 then
      begin
        if FAuthors.Count = 0 then
          FAuthors.AddObject(BlameItem.Author, TObject(clWhite))
        else
          FAuthors.AddObject(BlameItem.Author, TObject(GetNextColor));
      end;
      W := Canvas.TextWidth(BlameItem.Author);
      if W + 8 > FRevisionX then
        FRevisionX := W + 8;
      W := Canvas.TextWidth(IntToStr(BlameItem.Revision));
      if FRevisionX + W + 8 > FTimeX then
        FTimeX := FRevisionX + W + 8;
    end;
    FLastItem := HistoryItem;
  end;

  Y := 0;
  for I := TopLine - 1 to TopLine + LinesInWindow - 1 do
  begin
    if I >= HistoryItem.BlameCount then
      Break;
    BlameItem := HistoryItem.BlameItems[I];

    if FAuthors.Find(BlameItem.Author, Index) then
      Canvas.Brush.Color := TColor(FAuthors.Objects[Index])
    else
      Canvas.Brush.Color := clWhite;
    R := Rect(0, Y, ClientWidth, Y + LineHeight);
    Canvas.FillRect(R);

    Canvas.TextOut(0, Y, BlameItem.Author);
    R := Rect(FRevisionX, Y, FTimeX - 8, Y + LineHeight);
    DrawText(Canvas.Handle, PChar(IntToStr(BlameItem.Revision)), -1, R, DT_RIGHT);
    Canvas.TextOut(FTimeX, Y, FormatDateTime('yyyy/mm/dd hh:nn:ss', BlameItem.Time));

    Inc(Y, LineHeight);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TBlameControl public }

//----------------------------------------------------------------------------------------------------------------------

constructor TBlameControl.Create(AOwner: TComponent; AControl: TWinControl);

begin
  inherited Create(AOwner);
  FLastItem := nil;
  FAuthors := TStringList.Create;
  FAuthors.Sorted := True;
  FControl := AControl;
  FControlProc := FControl.WindowProc;
  FControl.WindowProc := ControlProc;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TBlameControl.Destroy;

begin
  FAuthors.Free;
  FControl.WindowProc := FControlProc;
  FControlProc := nil;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnIDEClient private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.Finalize;

var
  {$IFDEF COMPILER_9_UP}
  FileHistoryManager: IOTAFileHistoryManager;
  {$ENDIF}
  {$IFDEF COMPILER_14_UP}
  EditorViewServices: IOTAEditorViewServices;
  {$ENDIF}
  MainMenuBar: THackActionMainMenuBar;
  I: Integer;
  Item: TActionClientItem;
  S: string;

  procedure TraverseItem(Item: TActionClientItem);
  var
    I: Integer;
  begin
    for I := Item.Items.Count - 1 downto 0 do
      TraverseItem(Item.Items[I]);
    Item.Free;
  end;

begin
  if (FHistoryProviderIndex <> -1) and Assigned(BorlandIDEServices)
    {$IFDEF COMPILER_9_UP}
    and BorlandIDEServices.GetService(IOTAFileHistoryManager, FileHistoryManager)
    {$ENDIF}
    then
    FileHistoryManager.UnregisterHistoryProvider(FHistoryProviderIndex);
  FHistoryProviderIndex := -1;
  if Assigned(FEditorView) then
    {$IFDEF COMPILER_14_UP}
    if BorlandIDEServices.GetService(IOTAEditorViewServices, EditorViewServices) then
      EditorViewServices.UnregisterEditorSubView(FEditorView);
    {$ELSE}
    UnregisterEditorView(FEditorView);
    {$ENDIF}
  FEditorView := nil;
  FEditorViewIntf := nil;
  {$IFNDEF COMPILER_9_UP}
  if Assigned(FHistoryView) then
    UnregisterEditorView(FHistoryView);
  FHistoryView := nil;
  FHistoryViewIntf := nil;
  {$ENDIF}

  FreeAndNil(FSvnClient);
  FreeAndNil(FMenuItem);
  FreeAndNil(SvnImageModule);

  MainMenuBar := THackActionMainMenuBar(Application.MainForm.FindComponent('MenuBar'));
  if not Assigned(MainMenuBar) then
    Exit;
  Item := nil;
  for I := 0 to MainMenuBar.ItemCount - 1 do
  begin
    S := StringReplace(MainMenuBar.Items[I].Caption, '&', '', [rfReplaceAll]);
    if AnsiSameText(S, 'Subversion') then
    begin
      Item := MainMenuBar.Items[I];
      Break;
    end;
  end;
  if Assigned(Item) then
    TraverseItem(Item);
    
  FreeAndNil(FormSvnTools);
  if Assigned(UnregisterFieldAddress) then
    UnregisterFieldAddress(@FormSvnTools);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.GetDirectories(Directories: TStrings);

var
  StringList: TStringList;
  I: Integer;
  S: string;

begin
  StringList := TStringList.Create;
  try
    StringList.Delimiter := ';';
    {$IFDEF COMPILER_10_UP}
    StringList.StrictDelimiter := True;
    {$ENDIF}
    StringList.DelimitedText := FSettings.Directories;
    for I := StringList.Count - 1 downto 0 do
    begin
      S := ExpandRootMacro(StringList[I]);
      if DirectoryExists(S) then
        StringList[I] := S
      else
        StringList.Delete(I);
    end;

    Directories.BeginUpdate;
    try
      Directories.Assign(StringList);
    finally
      Directories.EndUpdate;
    end;
  finally
    StringList.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.Initialize;

var
  {$IFDEF COMPILER_9_UP}
  FileHistoryManager: IOTAFileHistoryManager;
  {$ENDIF}
  {$IFDEF COMPILER_14_UP}
  EditorViewServices: IOTAEditorViewServices;
  {$ENDIF}
  NTAServices: INTAServices;
  MenuItem: TMenuItem;
  I: Integer;
  {$IFNDEF COMPILER_9_UP}
  MainMenu1: TMainMenu;
  ToolsItem: TMenuItem;
  ToolsItemIndex: Integer;
  {$ENDIF}

begin
  FSettings.LoadSettings;

  SvnImageModule := TSvnImageModule.Create(Self);
  FSvnClient := TSvnClient.Create;
  FSvnClient.OnLoginPrompt := SvnClientLoginPrompt;
  FSvnClient.OnUserNamePrompt := SvnClientUserNamePrompt;
  FSvnClient.OnSSLServerTrustPrompt := SvnClientSSLServerTrustPrompt;
  FSvnClient.OnSSLClientCertPrompt := SvnClientSSLClientCertPrompt;
  FSvnClient.OnSSLClientPasswordPrompt := SvnClientSSLClientPasswordPrompt;
  FSvnClient.Initialize;

  if Assigned(BorlandIDEServices) then
  begin
    {$IFDEF COMPILER_9_UP}
    if BorlandIDEServices.GetService(IOTAFileHistoryManager, FileHistoryManager) then
    {$ENDIF}
      FHistoryProviderIndex := FileHistoryManager.RegisterHistoryProvider(TSvnFileHistoryProvider.Create);
    FEditorViewIntf := TSvnEditorView.Create;
    {$IFDEF COMPILER_14_UP}
    if BorlandIDEServices.GetService(IOTAEditorViewServices, EditorViewServices) then
      FEditorView := EditorViewServices.RegisterEditorSubView(FEditorViewIntf)
    else
      FEditorView := nil;
    {$ELSE}
    FEditorView := RegisterEditorView(FEditorViewIntf);
    {$ENDIF}

    {$IFDEF COMPILER_9_UP}
    if BorlandIDEServices.GetService(INTAServices, NTAServices) then
    {$ELSE}
    FHistoryViewIntf := TSvnHistoryView.Create;
    FHistoryView := RegisterEditorView(FHistoryViewIntf);
    if Supports(BorlandIDEServices, INTAServices, NTAServices) then
    {$ENDIF}
    begin
      FMenuItem := TMenuItem.Create(Application.MainForm);
      FMenuItem.Name := 'SubversionMenu';
      FMenuItem.Caption := 'Subversion';

      for I := 0 to Actions.ActionCount - 1 do
        if Actions[I] <> ActionOptions then
        begin
          MenuItem := nil;
          try
            MenuItem := TMenuItem.Create(Self);
            MenuItem.Name := 'Menu' + Actions[I].Name;
            MenuItem.Action := Actions[I];
            FMenuItem.Add(MenuItem);
          except
            FreeAndNil(MenuItem);
            raise;
          end;
        end;

      MenuItem := nil;
      try
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Name := 'MenuSeparator';
        MenuItem.Caption := '-';
        FMenuItem.Add(MenuItem);
      except
        FreeAndNil(MenuItem);
        raise;
      end;

      MenuItem := nil;
      try
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Name := 'Menu' + ActionOptions.Name;
        MenuItem.Action := ActionOptions;
        FMenuItem.Add(MenuItem);
      except
        FreeAndNil(MenuItem);
        raise;
      end;

      {$IFDEF COMPILER_9_UP}
      NTAServices.AddActionMenu('ToolsMenu', nil, FMenuItem);
      {$ELSE}
      MainMenu1 := Application.MainForm.FindComponent('MainMenu1') as TMainMenu;
      ToolsItem := Application.MainForm.FindComponent('ToolsMenu') as TMenuItem;
      ToolsItemIndex := MainMenu1.Items.IndexOf(ToolsItem);
      MainMenu1.Items.Insert(ToolsItemIndex + 1, FMenuItem);
      {$ENDIF}
    end;
  end;

  RegisterFieldAddress(SvnToolFormSection, @FormSvnTools);
  RegisterDesktopFormClass(TFormSvnTools, SvnToolFormSection, SvnToolFormSection);

  FormSvnTools := TFormSvnTools.Create(Application);
  FormSvnTools.Name := SvnToolFormSection;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.InsertBlameControl(Form: TCustomForm);

var
  HistoryFrame: TCustomFrame;
  RevisionContentTree: TObject;
  TabSheet1, EditControl: TWinControl;
  I: Integer;
  BlameControl: TBlameControl;
  Splitter: TSplitter;

begin
  {$IFDEF COMPILER_9_UP}
  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFileHistoryFrame'));
  {$ELSE}
  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFrameSvnHistoryView'));
  {$ENDIF}
  if not Assigned(HistoryFrame) then
    Exit;
  {$IFDEF COMPILER_9_UP}
  RevisionContentTree := HistoryFrame.FindComponent('RevisionContentTree');
  {$ELSE}
  RevisionContentTree := TFrameSvnHistoryView(HistoryFrame).TreeContent;
  {$ENDIF}
  if not Assigned(RevisionContentTree) then
    Exit;

  TabSheet1 := TTabSheet(HistoryFrame.FindComponent('TabSheet1'));
  if not Assigned(TabSheet1) then
    Exit;

  // find edit control
  {$IFDEF COMPILER_9_UP}
  EditControl := nil;
  for I := 0 to TabSheet1.ControlCount - 1 do
    if TabSheet1.Controls[I].ClassNameIs('TEditControl') then
    begin
      EditControl := TabSheet1.Controls[I] as TWinControl;
      Break;
    end;
  {$ELSE}
  EditControl := TFrameSvnHistoryView(HistoryFrame).EditContent;
  {$ENDIF}
  if not Assigned(EditControl) then
    Exit;

  // find blame control
  BlameControl := nil;
  for I := 0 to TabSheet1.ControlCount - 1 do
    if TabSheet1.Controls[I].ClassNameIs('TBlameControl') then
    begin
      BlameControl := TabSheet1.Controls[I] as TBlameControl;
      Break;
    end;

  if not Assigned(BlameControl) then
  begin
    if not Assigned(EditControl) then
      Exit;

    EditControl.Align := alNone;
    BlameControl := TBlameControl.Create(HistoryFrame, EditControl);
    BlameControl.Parent := TabSheet1;
    BlameControl.Name := 'SvnBlameControl';
    BlameControl.Width := 170;
    BlameControl.Align := alLeft;
    BlameControl.DoubleBuffered := True;
    BlameControl.ShowHint := True;
    Splitter := TSplitter.Create(EditControl.Parent);
    Splitter.Parent := TabSheet1;
    Splitter.Name := 'SvnBlameSplitter';
    Splitter.Left := BlameControl.Width + 1;
    Splitter.Width := 3;
    Splitter.Align := alLeft;
    EditControl.Parent := TabSheet1;
    EditControl.Align := alClient;
  end;
  EditControl.Visible := True;  
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SvnClientLoginPrompt(Sender: TObject; const Realm: string; var UserName, Password: string;
  var Cancel, Save: Boolean);

begin
  FSyncData := AllocMem(SizeOf(TSyncLoginPrompt));
  try
    PSyncLoginPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncLoginPrompt(FSyncData)^.Realm := Realm;
    PSyncLoginPrompt(FSyncData)^.UserName := UserName;
    PSyncLoginPrompt(FSyncData)^.Password := Password;
    PSyncLoginPrompt(FSyncData)^.Cancel := Cancel;
    PSyncLoginPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncLoginPrompt);
    UserName := PSyncLoginPrompt(FSyncData)^.UserName;
    Password := PSyncLoginPrompt(FSyncData)^.Password;
    Cancel := PSyncLoginPrompt(FSyncData)^.Cancel;
    Save := PSyncLoginPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncLoginPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SvnClientSSLClientCertPrompt(Sender: TObject; const Realm: string; var CertFileName: string;
  var Cancel, Save: Boolean);

begin
  FSyncData := AllocMem(SizeOf(TSyncSSLClientCertPrompt));
  try
    PSyncSSLClientCertPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncSSLClientCertPrompt(FSyncData)^.Realm := Realm;
    PSyncSSLClientCertPrompt(FSyncData)^.CertFileName := CertFileName;
    PSyncSSLClientCertPrompt(FSyncData)^.Cancel := Cancel;
    PSyncSSLClientCertPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncSSLClientCertPrompt);
    CertFileName := PSyncSSLClientCertPrompt(FSyncData)^.CertFileName;
    Cancel := PSyncSSLClientCertPrompt(FSyncData)^.Cancel;
    Save := PSyncSSLClientCertPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncSSLClientCertPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SvnClientSSLClientPasswordPrompt(Sender: TObject; const Realm: string; var Password: string;
  var Cancel, Save: Boolean);

begin
  FSyncData := AllocMem(SizeOf(TSyncSSLClientPasswordPrompt));
  try
    PSyncSSLClientPasswordPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Realm := Realm;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Password := Password;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Cancel := Cancel;
    PSyncSSLClientPasswordPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncSSLClientPasswordPrompt);
    Password := PSyncSSLClientPasswordPrompt(FSyncData)^.Password;
    Cancel := PSyncSSLClientPasswordPrompt(FSyncData)^.Cancel;
    Save := PSyncSSLClientPasswordPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncSSLClientPasswordPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SvnClientSSLServerTrustPrompt(Sender: TObject; const Realm: string;
  const CertInfo: TSvnAuthSSLServerCertInfo; Failures: TSSLServerTrustFailures; var Cancel, Save: Boolean);

begin
  FSyncData := AllocMem(SizeOf(TSyncSSLServerTrustPrompt));
  try
    PSyncSSLServerTrustPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncSSLServerTrustPrompt(FSyncData)^.Realm := Realm;
    PSyncSSLServerTrustPrompt(FSyncData)^.CertInfo := CertInfo;
    PSyncSSLServerTrustPrompt(FSyncData)^.Failures := Failures;
    PSyncSSLServerTrustPrompt(FSyncData)^.Cancel := Cancel;
    PSyncSSLServerTrustPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncSSLServerTrustPrompt);
    Cancel := PSyncSSLServerTrustPrompt(FSyncData)^.Cancel;
    Save := PSyncSSLServerTrustPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncSSLServerTrustPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SvnClientUserNamePrompt(Sender: TObject; const Realm: string; var UserName: string;
  var Cancel, Save: Boolean);

begin
  FSyncData := AllocMem(SizeOf(TSyncLoginPrompt));
  try
    PSyncLoginPrompt(FSyncData)^.SvnClient := Sender as TSvnClient;
    PSyncLoginPrompt(FSyncData)^.Realm := Realm;
    PSyncLoginPrompt(FSyncData)^.UserName := UserName;
    PSyncLoginPrompt(FSyncData)^.Password := '';
    PSyncLoginPrompt(FSyncData)^.Cancel := Cancel;
    PSyncLoginPrompt(FSyncData)^.Save := Save;
    TThread.Synchronize(nil, SyncUserNamePrompt);
    UserName := PSyncLoginPrompt(FSyncData)^.UserName;
    Cancel := PSyncLoginPrompt(FSyncData)^.Cancel;
    Save := PSyncLoginPrompt(FSyncData)^.Save;
  finally
    System.Finalize(TSyncLoginPrompt(FSyncData^));
    FreeMem(FSyncData);
    FSyncData := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SyncLoginPrompt;

begin
  with PSyncLoginPrompt(FSyncData)^ do
    Cancel := ShowSvnClientLoginPrompt(SvnClient, Realm, UserName, Password, Save) <> mrOK;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SyncSSLClientCertPrompt;

begin
  with PSyncSSLClientCertPrompt(FSyncData)^ do
    Cancel := ShowSvnClientSSLClientCertPrompt(SvnClient, Realm, CertFileName, Save) <> mrOK;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SyncSSLClientPasswordPrompt;

begin
  with PSyncSSLClientPasswordPrompt(FSyncData)^ do
    Cancel := ShowSvnClientSSLClientPasswordPrompt(SvnClient, Realm, Password, Save) <> mrOK;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SyncSSLServerTrustPrompt;

begin
  with PSyncSSLServerTrustPrompt(FSyncData)^ do
    Cancel := ShowSvnClientSSLServerTrustPrompt(SvnClient, Realm, CertInfo, Failures, Save) <> mrOK;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SyncUserNamePrompt;

begin
  with PSyncLoginPrompt(FSyncData)^ do
    Cancel := ShowSvnClientLoginPrompt(SvnClient, Realm, UserName, Password, Save, [lpoUserName]) <> mrOK;
end;

{$IFDEF COMPILER_9_UP}
//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.TabSheetShow(Sender: TObject);

var
  FileName: string;
  Frame: TFrameSvnDiff3;
  SvnHistory: ISvnFileHistory;
  SvnItem: TSvnItem;
  Diff: PSvnDiff;
  DiffOptions: TSvnDiffFileOptions;
  OriginalFileName, ModifiedFileName, LatestFileName: string;
  OriginalLines, ModifiedLines, LatestLines: TStringList;

begin
  Frame := TFrameSvnDiff3(FindChildControl(Sender as TTabSheet, 'TFrameSvnDiff3'));
  if not Assigned(Frame) then
    Exit;
  FileName := GetCurrentModuleFileName;
  if FileName = '' then
    Exit;
  SvnHistory := GetSvnHistory(FileName);
  if not Assigned(SvnHistory) then
    Exit;
  SvnItem := SvnHistory.Item;
  if not Assigned(SvnItem) or (SvnItem = Frame.Item) then
    Exit;
  SvnHistory := nil;

  if SvnItem.TextStatus = svnWcStatusConflicted then
  begin
    OriginalFileName := ExtractFilePath(SvnItem.PathName) + SvnItem.ConflictOldFile;
    ModifiedFileName := ExtractFilePath(SvnItem.PathName) + SvnItem.ConflictWorkingFile;
    LatestFileName := ExtractFilePath(SvnItem.PathName) + SvnItem.ConflictNewFile;

    DiffOptions.ignore_space := svnIgnoreSpaceAll;
    DiffOptions.ignore_eol_style := True;

    if Assigned(@svn_diff_file_diff3_2) then
      SvnCheck(svn_diff_file_diff3_2(Diff, PAnsiChar(AnsiString(OriginalFileName)), PAnsiChar(AnsiString(ModifiedFileName)), PAnsiChar(AnsiString(LatestFileName)),
        @DiffOptions, SvnItem.SvnClient.Pool))
    else
      SvnCheck(svn_diff_file_diff3(Diff, PAnsiChar(AnsiString(OriginalFileName)), PAnsiChar(AnsiString(ModifiedFileName)), PAnsiChar(AnsiString(LatestFileName)),
        SvnItem.SvnClient.Pool));

    OriginalLines := nil;
    ModifiedLines := nil;
    LatestLines := nil;
    try
      OriginalLines := TStringList.Create;
      OriginalLines.LoadFromFile(OriginalFileName);
      ModifiedLines := TStringList.Create;
      ModifiedLines.LoadFromFile(ModifiedFileName);
      LatestLines := TStringList.Create;
      LatestLines.LoadFromFile(LatestFileName);

      InitializeEdit(Frame.Edit1, FileName);
      InitializeEdit(Frame.Edit2, FileName);
      InitializeEdit(Frame.Edit3, FileName);
      Frame.Item := SvnItem;
      Frame.Initialize(OriginalLines, ModifiedLines, LatestLines, Diff);
    finally
      OriginalLines.Free;
      ModifiedLines.Free;
      LatestLines.Free;
    end;
  end
  else
  begin
    Frame.Item := nil;
    Frame.Initialize(nil, nil, nil, nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
{$ENDIF}

{ TSvnIDEClient public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnIDEClient.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FSvnClient := nil;
  FHistoryProviderIndex := -1;
  FEditorView := nil;
  FMenuItem := nil;
  FSettings := TSvnIDESettings.Create;
  FSyncData := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnIDEClient.Destroy;

begin
  FSettings.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.FindFirstSvnHistoryNode(Tree: TObject): Pointer;

var
  Node: Pointer;
  Data: PHistoryNodeData;
  SvnFileHistory: ISvnFileHistory;

begin
  Result := nil;

  Node := BaseVirtualTreeGetFirst(Tree);

  while Assigned(Node) do
  begin
    Data := BaseVirtualTreeGetNodeData(Tree, Node);
    if Assigned(Data) and Assigned(Data^.History) and
      Succeeded(Data^.History.QueryInterface(ISvnFileHistory, SvnFileHistory)) and Assigned(SvnFileHistory.Item) then
    begin
      Result := Node;
      Break;
    end;

    Node := BaseVirtualTreeGetNext(Tree, Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.FindSvnHistoryNode(Tree: TObject; Revision: Integer): Pointer;

var
  Node: Pointer;
  Data: PHistoryNodeData;
  SvnFileHistory: ISvnFileHistory;

begin
  Result := nil;

  Node := BaseVirtualTreeGetFirst(Tree);
  while Assigned(Node) do
  begin
    Data := BaseVirtualTreeGetNodeData(Tree, Node);
    if Assigned(Data) and Assigned(Data^.History) then
    begin
      if (Revision = -1) and (Data^.History.HistoryStyle[Data^.Index] in [hsBuffer, hsFile]) then
      begin
        Result := Node;
        Break;
      end
      else if Succeeded(Data^.History.QueryInterface(ISvnFileHistory, SvnFileHistory)) and
        Assigned(SvnFileHistory.Item) and (SvnFileHistory.Item.HistoryItems[Data^.Index].Revision = Revision) then
      begin
        Result := Node;
        Break;
      end;
    end;

    Node := BaseVirtualTreeGetNext(Tree, Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.FrameSvnDiff3ItemResolved(Sender: TObject; Kind: TResolutionKind);

var
  Frame: TFrameSvnDiff3;
  ActionServices: IOTAActionServices;

begin
  Frame := Sender as TFrameSvnDiff3;
  if not Assigned(Frame.Item) then
    Exit;

  {$IFDEF COMPILER_9_UP}
  if BorlandIDEServices.GetService(IOTAActionServices, ActionServices) then
  {$ELSE}
  if Supports(BorlandIDEServices, IOTAActionServices, ActionServices) then
  {$ENDIF}
    ActionServices.ReloadFile(Frame.Item.PathName);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.GetCurrentModuleFileName: string;

var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;

begin
  Result := '';

  {$IFDEF COMPILER_9_UP}
  if not BorlandIDEServices.GetService(IOTAModuleServices, ModuleServices) then
  {$ELSE}
  if not Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
  {$ENDIF}
    Exit;
  Module := ModuleServices.CurrentModule;
  if Assigned(Module) then
    Result := Module.FileName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.GetEditWindow: TCustomForm;

var
  EditorServices: IOTAEditorServices;
  EditView: IOTAEditView;
  NTAEditWindow: INTAEditWindow;

begin
  Result := nil;
  {$IFDEF COMPILER_9_UP}
  if not BorlandIDEServices.GetService(IOTAEditorServices, EditorServices) then
  {$ELSE}
  if not Supports(BorlandIDEServices, IOTAEditorServices, EditorServices) then
  {$ENDIF}
    Exit;
  EditView := EditorServices.TopView;
  if not Assigned(EditView) then
    Exit;
  NTAEditWindow := EditView.GetEditWindow;
  if not Assigned(NTAEditWindow) then
    Exit;
  Result := NTAEditWindow.Form;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.GetModule(const FileName: string): IOTAModule;

var
  ModuleServices: IOTAModuleServices;
  {$IFNDEF COMPILER_9_UP}
  ActionServices: IOTAActionServices;
  {$ENDIF}

begin
  Result := nil;
  {$IFDEF COMPILER_9_UP}
  if BorlandIDEServices.GetService(IOTAModuleServices, ModuleServices) then
  {$ELSE}
  if Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
  {$ENDIF}
  begin
    Result := ModuleServices.FindModule(FileName);
    if not Assigned(Result) then
    {$IFDEF COMPILER_9_UP}
      Result := ModuleServices.OpenModule(FileName);
    {$ELSE}
    begin
      if Supports(BorlandIDEServices, IOTAActionServices, ActionServices) then
        ActionServices.OpenFile(FileName);
      Result := ModuleServices.FindModule(FileName);
    end;
    {$ENDIF}
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.GetSvnHistory(const FileName: string): ISvnFileHistory;

var
  {$IFDEF COMPILER_9_UP}
  FileHistoryManager: IOTAFileHistoryManager;
  {$ENDIF}
  SvnHistoryProvider: IOTAFileHistoryProvider;
  History: IOTAFileHistory;

begin
  Result := nil;

  if (FHistoryProviderIndex = -1) or
    {$IFDEF COMPILER_9_UP}
    not BorlandIDEServices.GetService(IOTAFileHistoryManager, FileHistoryManager) then
    {$ELSE}
    not Assigned(FileHistoryManager) then
    {$ENDIF}
    Exit;
  SvnHistoryProvider := FileHistoryManager.FileHistoryProvider[FHistoryProviderIndex];
  if not Assigned(SvnHistoryProvider) then
    Exit;
  History := SvnHistoryProvider.GetFileHistory(FileName);
  if Assigned(History) then
    Supports(History, ISvnFileHistory, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.GetSvnHistoryNodeItem(Tree: TObject; Node: Pointer): TSvnHistoryItem;

var
  Data: PHistoryNodeData;
  SvnFileHistory: ISvnFileHistory;
  Item: TSvnItem;

begin
  Result := nil;

  if not Assigned(Node) then
    Exit;
  Data := BaseVirtualTreeGetNodeData(Tree, Node);
  if Assigned(Data) and Assigned(Data^.History) and
    Succeeded(Data^.History.QueryInterface(ISvnFileHistory, SvnFileHistory)) then
  begin
    Item := SvnFileHistory.Item;
    if Assigned(Item) and (Data^.Index >= 0) and (Data^.Index < Item.HistoryCount) then
      Result := Item.HistoryItems[Data^.Index];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.InitializeEdit(Edit: TSynEdit; const FileName: string);

var
  Services: IOTAServices;
  EditorServices: IOTAEditorServices;
  EditOptions: IOTAEditOptions;
  S: string;
  Reg: TRegistry;

begin
  if not Supports(BorlandIDEServices, IOTAEditorServices, EditorServices) then
    Exit;
  EditOptions := EditorServices.GetEditOptionsForFile(FileName);
  if not Assigned(EditOptions) then
    Exit;

  Edit.Font.Name := EditOptions.FontName;
  Edit.Font.Size := EditOptions.FontSize;
  Edit.RightEdge := EditOptions.BufferOptions.RightMargin;
  Edit.SelectedColor.Background := clHighlight;
  Edit.SelectedColor.Foreground := clHighlightText;
  if Supports(BorlandIDEServices, IOTAServices, Services) then
  begin
    S := IncludeTrailingBackslash(Services.GetBaseRegistryKey) + 'Editor\Highlight';
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKeyReadOnly(IncludeTrailingBackslash(S) + 'Right margin') and
        Reg.ValueExists('Foreground Color New') then
        Edit.RightEdgeColor := StringToColor(Reg.ReadString('Foreground Color New'));
    finally
      Reg.Free;
    end;
  end;
  Edit.Gutter.Font := Edit.Font;
  Edit.Gutter.Color := $F4F4F4;
  Edit.Gutter.Font.Color := $CC9999;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.InitializeHighlighter(const FileName: string; var Highlighter: TSynCustomHighlighter);


var
  Services: IOTAServices;
  EditorServices: IOTAEditorServices;
  EditOptions: IOTAEditOptions;
  SID, SRegKey: string;

begin
  if not Supports(BorlandIDEServices, IOTAServices, Services) or
    not Supports(BorlandIDEServices, IOTAEditorServices, EditorServices) then
    Exit;

  SRegKey := IncludeTrailingBackslash(Services.GetBaseRegistryKey) + 'Editor\Highlight';
  EditOptions := EditorServices.GetEditOptionsForFile(FileName);
  if not Assigned(EditOptions) then
    Exit;

  SID := EditOptions.IDString;
  if AnsiSameText(SID, cDefEdDefault) then
    FreeAndNil(Highlighter)
  else if AnsiSameText(SID, cDefEdPascal) then
  begin
    if not (Highlighter is TSynPasSyn) then
    begin
      FreeAndNil(Highlighter);
      Highlighter := TSynPasSyn.Create(Self);
      with TSynPasSyn(Highlighter) do
      begin
        AsmAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Assembler', False);
        CharAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Character', False);
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        DirectiveAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        FloatAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Float', False);
        HexAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Hex', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Identifier', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        NumberAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Number', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        StringAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdC) then
  begin
    if not (Highlighter is TSynCppSyn) then
    begin
      FreeAndNil(Highlighter);
      Highlighter := TSynCppSyn.Create(Self);
      with TSynCppSyn(Highlighter) do
      begin
        AsmAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Assembler', False);
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        DirecAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Identifier', False);
        InvalidAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Illegal Char', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        NumberAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Number', False);
        FloatAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Float', False);
        HexAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Hex', False);
        OctalAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Octal', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        StringAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        CharAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Character', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdCSharp) then
  begin
    if not (Highlighter is TSynCSSyn) then
    begin
      FreeAndNil(Highlighter);
      Highlighter := TSynCSSyn.Create(Self);
      with TSynCSSyn(Highlighter) do
      begin
        AsmAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Assembler', False);
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        DirecAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Identifier', False);
        InvalidAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Illegal Char', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        NumberAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Number', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        StringAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdHTML) then
  begin
    if not (Highlighter is TSynHTMLSyn) then
    begin
      FreeAndNil(Highlighter);
      Highlighter := TSynHTMLSyn.Create(Self);
      with TSynHTMLSyn(Highlighter) do
      begin
        // AndAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Names', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
        TextAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Plain text', False);
        // UndefKeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        ValueAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Values', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdXML) then
  begin
    if not (Highlighter is TSynXMLSyn) then
    begin
      FreeAndNil(Highlighter);
      Highlighter := TSynXMLSyn.Create(Self);
      with TSynXMLSyn(Highlighter) do
      begin
        ElementAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        AttributeAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Names', False);
        NamespaceAttributeAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Names', False);
        AttributeValueAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Values', False);
        NamespaceAttributeValueAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Attribute Values', False);
        TextAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Plain text', False);
        CDATAAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        EntityRefAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        ProcessingInstructionAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        // DocTypeAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdSQL) then
  begin
    if not (Highlighter is TSynSQLSyn) then
    begin
      FreeAndNil(Highlighter);
      Highlighter := TSynSQLSyn.Create(Self);
      with TSynSQLSyn(Highlighter) do
      begin
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        ConditionalCommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        // DataTypeAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        // DefaultPackageAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        // DelimitedIdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        ExceptionAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        FunctionAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Identifier', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        NumberAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Number', False);
        // PLSQLAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        // SQLPlusAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        StringAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
        // TableNameAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        // VariableAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
      end;
    end;
  end
  else if AnsiSameText(SID, cDefEdIDL) then
  begin
    if not (Highlighter is TSynIdlSyn) then
    begin
      FreeAndNil(Highlighter);
      Highlighter := TSynIdlSyn.Create(Self);
      with TSynIdlSyn(Highlighter) do
      begin
        CommentAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Comment', False);
        // DatatypeAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, '', False);
        IdentifierAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Identifier', False);
        KeyAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Reserved word', False);
        NumberAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Number', False);
        PreprocessorAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Preprocessor', False);
        SpaceAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Whitespace', False);
        StringAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'String', False);
        SymbolAttri.LoadFromBorlandRegistry(HKEY_CURRENT_USER, SRegKey, 'Symbol', False);
      end;
    end;
  end
  else
    FreeAndNil(Highlighter);
end;

{$IFNDEF COMPILER_9_UP}
//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.SelectEditorView(const TabCaption: string): Boolean;

var
  Form: TCustomForm;
  ViewBar: TTabSet;
  TabIndex: Integer;

begin
  Result := False;
  Form := GetEditWindow;
  if not Assigned(Form) then
    Exit;
  ViewBar := TTabSet(Form.FindComponent('ViewBar'));
  if not Assigned(ViewBar) then
    Exit;
  TabIndex := ViewBar.Tabs.IndexOf(TabCaption);
  if TabIndex = -1 then
    Exit;
  ViewBar.TabIndex := TabIndex;
  Result := True;
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SetupBlameControl;

begin
  InsertBlameControl(GetEditWindow);
end;

{$IFDEF COMPILER_9_UP}
//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.SetupDiff3Frame;

var
  Form: TCustomForm;
  HistoryFrame: TCustomFrame;
  TabSet1: TTabSet;
  Index: Integer;
  PageControl1: TPageControl;
  TabSheet: TTabSheet;
  Frame: TFrameSvnDiff3;

begin
  Form := GetEditWindow;
  if not Assigned(Form) then
    Exit;
  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFileHistoryFrame'));
  if not Assigned(HistoryFrame) then
    Exit;
  TabSet1 := TTabSet(HistoryFrame.FindComponent('TabSet1'));
  if not Assigned(TabSet1) then
    Exit;
  Index := TabSet1.Tabs.IndexOf('Merge Conflicts');
  if Index <> -1 then
    Exit;
  TabSet1.Tabs.Insert(3, 'Merge Conflicts');
  PageControl1 := TPageControl(HistoryFrame.FindComponent('PageControl1'));
  if not Assigned(PageControl1) then
    Exit;
  Index := PageControl1.ActivePageIndex;
  try
    TabSheet := TTabSheet.Create(SvnIDEModule);
    TabSheet.PageControl := PageControl1;
    TabSheet.Caption := 'Merge Conflicts';
    TabSheet.TabVisible := False;
    TabSheet.OnShow := TabSheetShow;
    Frame := TFrameSvnDiff3.Create(SvnIDEModule);
    Frame.Parent := TabSheet;
    Frame.Align := alClient;
    Frame.OnItemResolved := FrameSvnDiff3ItemResolved;
  finally
    PageControl1.ActivePageIndex := Index;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
{$ENDIF}

function TSvnIDEClient.ShowBlame(const FileName: string): Boolean;

var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  {$IFDEF COMPILER_9_UP}
  TabSet1: TTabSet;
  {$ELSE}
  ActionServices: IOTAActionServices;
  {$ENDIF}
  SourceEditor: IOTASourceEditor;
  I: Integer;
  Form: TCustomForm;
  HistoryFrame: TCustomFrame;
  Index: Integer;
  FileSelector: TComboBox;
  Tree: TObject;
  Node: Pointer;

begin
  Result := False;

  // open and show the file in source code editor
  {$IFDEF COMPILER_9_UP}
  if not BorlandIDEServices.GetService(IOTAModuleServices, ModuleServices) then
  {$ELSE}
  if not Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
  {$ENDIF}
    Exit;
  Module := ModuleServices.FindModule(FileName);
  if not Assigned(Module) then
  {$IFDEF COMPILER_9_UP}
    Module := ModuleServices.OpenModule(FileName);
  {$ELSE}
  begin
    if Supports(BorlandIDEServices, IOTAActionServices, ActionServices) then
      ActionServices.OpenFile(FileName);
    Module := ModuleServices.FindModule(FileName);
  end;
  {$ENDIF}
  if not Assigned(Module) then
    Exit;
  SourceEditor := nil;
  for I := 0 to Module.ModuleFileCount - 1 do
    if Succeeded(Module.ModuleFileEditors[I].QueryInterface(IOTASourceEditor, SourceEditor)) then
      Break;

  // switch to 'History' tab
  if not Assigned(SourceEditor) then
    Exit;
  SourceEditor.Show;
  {$IFDEF COMPILER_9_UP}
  SourceEditor.SwitchToView('Borland.FileHistoryView');
  {$ELSE}
  SelectEditorView('History');
  {$ENDIF}

  // find the history frame
  Form := GetEditWindow;
  if not Assigned(Form) then
    Exit;
  {$IFDEF COMPILER_9_UP}
  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFileHistoryFrame'));
  {$ELSE}
  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFrameSvnHistoryView'));
  {$ENDIF}
  if not Assigned(HistoryFrame) then
    Exit;

  // select file
  if Module.ModuleFileCount > 1 then
  begin
    FileSelector := TComboBox(HistoryFrame.FindComponent('FileSelector'));
    if not Assigned(FileSelector) then
      Exit;
    Index := FileSelector.Items.IndexOf(ExtractFileName(FileName));
    if Index = -1 then
      Exit;
    FileSelector.ItemIndex := Index;
    if Assigned(FileSelector.OnClick) then
      FileSelector.OnClick(FileSelector);
  end;

  {$IFDEF COMPILER_9_UP}
  // switch to 'Contents' tab
  TabSet1 := TTabSet(HistoryFrame.FindComponent('TabSet1'));
  if not Assigned(TabSet1) then
    Exit;
  Index := TabSet1.Tabs.IndexOf('Contents');
  if Index = -1 then
    Exit;
  TabSet1.TabIndex := Index;
  if Assigned(TabSet1.OnClick) then
    TabSet1.OnClick(TabSet1);

  // find the treeview
  Tree := HistoryFrame.FindComponent('RevisionContentTree');
  if not Assigned(Tree) then
    Exit;
  {$ELSE}
  TFrameSvnHistoryView(HistoryFrame).PageControl.ActivePageIndex := 0;
  Tree := TFrameSvnHistoryView(HistoryFrame).TreeContent;
  {$ENDIF}

  Node := FindFirstSvnHistoryNode(Tree);
  if not Assigned(Node) then
    Exit;

  // select and show node
  BaseVirtualTreeSetSelected(Tree, Node, True);
  BaseVirtualTreeScrollIntoView(Tree, Node, True, False);

  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.ShowConflicts(const FileName: string): Boolean;

var
  Module: IOTAModule;
  {$IFDEF COMPILER_9_UP}
  TabSet1: TTabSet;
  {$ENDIF}
  SourceEditor: IOTASourceEditor;
  I: Integer;
  Form: TCustomForm;
  HistoryFrame: TCustomFrame;
  Index: Integer;
  FileSelector: TComboBox;

begin
  Result := False;

  // open and show the file in source code editor
  Module := GetModule(FileName);
  if not Assigned(Module) then
    Exit;
  SourceEditor := nil;
  for I := 0 to Module.ModuleFileCount - 1 do
    if Succeeded(Module.ModuleFileEditors[I].QueryInterface(IOTASourceEditor, SourceEditor)) then
      Break;

  // switch to 'History' tab
  if not Assigned(SourceEditor) then
    Exit;
  SourceEditor.Show;
  {$IFDEF COMPILER_9_UP}
  SourceEditor.SwitchToView('Borland.FileHistoryView');

  // find the history frame
  Form := GetEditWindow;
  if not Assigned(Form) then
    Exit;
  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFileHistoryFrame'));
  if not Assigned(HistoryFrame) then
    Exit;

  // order is important; first select file, then switch tabs
  // otherwise selecting file reactivates first tab sheet

  // select file
  if Module.ModuleFileCount > 1 then
  begin
    FileSelector := TComboBox(HistoryFrame.FindComponent('FileSelector'));
    if not Assigned(FileSelector) then
      Exit;
    Index := FileSelector.Items.IndexOf(ExtractFileName(FileName));
    if Index = -1 then
      Exit;
    FileSelector.ItemIndex := Index;
    if Assigned(FileSelector.OnClick) then
      FileSelector.OnClick(FileSelector);
  end;

  // switch to 'Merge Conflicts' tab
  TabSet1 := TTabSet(HistoryFrame.FindComponent('TabSet1'));
  if not Assigned(TabSet1) then
    Exit;
  Index := TabSet1.Tabs.IndexOf('Merge Conflicts');
  if Index = -1 then
    Exit;
  TabSet1.TabIndex := Index;
  if Assigned(TabSet1.OnClick) then
    TabSet1.OnClick(TabSet1);
  {$ELSE}
  SelectEditorView('History');
  // find the history frame
  Form := GetEditWindow;
  HistoryFrame := TFrameSvnHistoryView(FindChildControl(Form, 'TFrameSvnHistoryView'));
  if not Assigned(HistoryFrame) then
    Exit;

  // select file
  if Module.ModuleFileCount > 1 then
  begin
    FileSelector := TFrameSvnHistoryView(HistoryFrame).ComboBoxFileSelector;
    if not Assigned(FileSelector) then
      Exit;
    Index := FileSelector.Items.IndexOf(ExtractFileName(FileName));
    if Index = -1 then
      Exit;
    FileSelector.ItemIndex := Index;
    if Assigned(FileSelector.OnClick) then
      FileSelector.OnClick(FileSelector);
  end;

  // switch to 'Merge Conflicts' tab
  TFrameSvnHistoryView(HistoryFrame).PageControl.ActivePageIndex := 3;
  if Assigned(TFrameSvnHistoryView(HistoryFrame).PageControl.OnChange) then
    TFrameSvnHistoryView(HistoryFrame).PageControl.OnChange(TFrameSvnHistoryView(HistoryFrame).PageControl);
  {$ENDIF}

  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnIDEClient.ShowDiff(const FileName: string; FromRevision, ToRevision: Integer): Boolean;

var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  {$IFDEF COMPILER_9_UP}
  TabSet1: TTabSet;
  {$ELSE}
  ActionServices: IOTAActionServices;
  {$ENDIF}
  SourceEditor: IOTASourceEditor;
  I: Integer;
  Form: TCustomForm;
  HistoryFrame: TCustomFrame;
  Index: Integer;
  FileSelector: TComboBox;
  FromTree, ToTree: TObject;
  Node: Pointer;

begin
  Result := False;

  // open and show the file in source code editor
  {$IFDEF COMPILER_9_UP}
  if not BorlandIDEServices.GetService(IOTAModuleServices, ModuleServices) then
  {$ELSE}
  if not Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
  {$ENDIF}
    Exit;
  Module := ModuleServices.FindModule(FileName);
  if not Assigned(Module) then
  {$IFDEF COMPILER_9_UP}
    Module := ModuleServices.OpenModule(FileName);
  {$ELSE}
  begin
    if Supports(BorlandIDEServices, IOTAActionServices, ActionServices) then
      ActionServices.OpenFile(FileName);
    Module := ModuleServices.FindModule(FileName);
  end;
  {$ENDIF}
  if not Assigned(Module) then
    Exit;
  SourceEditor := nil;
  for I := 0 to Module.ModuleFileCount - 1 do
    if Succeeded(Module.ModuleFileEditors[I].QueryInterface(IOTASourceEditor, SourceEditor)) then
      Break;

  // switch to 'History' tab
  if not Assigned(SourceEditor) then
    Exit;
  SourceEditor.Show;
  {$IFDEF COMPILER_9_UP}
  SourceEditor.SwitchToView('Borland.FileHistoryView');

  // find the history frame
  Form := GetEditWindow;
  if not Assigned(Form) then
    Exit;
  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFileHistoryFrame'));
  if not Assigned(HistoryFrame) then
    Exit;

  // order is important; first select file, then switch tabs
  // otherwise selecting file reactivates first tab sheet

  // select file
  if Module.ModuleFileCount > 1 then
  begin
    FileSelector := TComboBox(HistoryFrame.FindComponent('FileSelector'));
    if not Assigned(FileSelector) then
      Exit;
    Index := FileSelector.Items.IndexOf(ExtractFileName(FileName));
    if Index = -1 then
      Exit;
    FileSelector.ItemIndex := Index;
    if Assigned(FileSelector.OnClick) then
      FileSelector.OnClick(FileSelector);
  end;

  // switch to 'Diff' tab
  TabSet1 := TTabSet(HistoryFrame.FindComponent('TabSet1'));
  if not Assigned(TabSet1) then
    Exit;
  Index := TabSet1.Tabs.IndexOf('Diff');
  if Index = -1 then
    Exit;
  TabSet1.TabIndex := Index;
  if Assigned(TabSet1.OnClick) then
    TabSet1.OnClick(TabSet1);

  // find the treeviews
  FromTree := HistoryFrame.FindComponent('DiffFrom');
  if not Assigned(FromTree) then
    Exit;
  ToTree := HistoryFrame.FindComponent('DiffTo');
  if not Assigned(ToTree) then
    Exit;
  {$ELSE}
  SelectEditorView('History');
  // find the history frame
  Form := GetEditWindow;
  HistoryFrame := TFrameSvnHistoryView(FindChildControl(Form, 'TFrameSvnHistoryView'));
  if not Assigned(HistoryFrame) then
    Exit;

  // select file
  if Module.ModuleFileCount > 1 then
  begin
    FileSelector := TFrameSvnHistoryView(HistoryFrame).ComboBoxFileSelector;
    if not Assigned(FileSelector) then
      Exit;
    Index := FileSelector.Items.IndexOf(ExtractFileName(FileName));
    if Index = -1 then
      Exit;
    FileSelector.ItemIndex := Index;
    if Assigned(FileSelector.OnClick) then
      FileSelector.OnClick(FileSelector);
  end;

  // switch to 'Diff' tab
  TFrameSvnHistoryView(HistoryFrame).PageControl.ActivePageIndex := 2;
  if Assigned(TFrameSvnHistoryView(HistoryFrame).PageControl.OnChange) then
    TFrameSvnHistoryView(HistoryFrame).PageControl.OnChange(TFrameSvnHistoryView(HistoryFrame).PageControl);

  // find the treeviews
  FromTree := TFrameSvnHistoryView(HistoryFrame).TreeDiffLeft;
  if not Assigned(FromTree) then
    Exit;
  ToTree := TFrameSvnHistoryView(HistoryFrame).TreeDiffRight;
  if not Assigned(ToTree) then
    Exit;
  {$ENDIF}

  // select and show from node
  Node := FindSvnHistoryNode(FromTree, FromRevision);
  if not Assigned(Node) then
    Exit;
  BaseVirtualTreeSetSelected(FromTree, Node, True);
  BaseVirtualTreeScrollIntoView(FromTree, Node, True, False);

  // select and show to node
  Node := FindSvnHistoryNode(ToTree, ToRevision);
  if not Assigned(Node) then
    Exit;
  BaseVirtualTreeSetSelected(ToTree, Node, True);
  BaseVirtualTreeScrollIntoView(ToTree, Node, True, False);

  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ShowEditor(const FileName: string);

var
  Module: IOTAModule;
  Editor: IOTAEditor;
  I: Integer;

begin
  Module := GetModule(FileName);
  if not Assigned(Module) then
    Exit;

  Editor := nil;
  for I := 0 to Module.ModuleFileCount - 1 do
    if AnsiSameText(FileName, Module.ModuleFileEditors[I].FileName) then
    begin
      Editor := Module.ModuleFileEditors[I];
      Break;
    end;
  if Assigned(Editor) then
    Editor.Show
  else if Assigned(Module.CurrentEditor) then
    Module.CurrentEditor.Show;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ShowHistoryEditControls;

{$IFDEF COMPILER_9_UP}
var
  Form: TCustomForm;
  HistoryFrame: TCustomFrame;
  PageControl1: TPageControl;
  I, J: Integer;
  TabSheet: TTabSheet;
{$ENDIF}

begin
  {$IFDEF COMPILER_9_UP}
  Form := GetEditWindow;
  if not Assigned(Form) then
    Exit;

  HistoryFrame := TCustomFrame(FindChildControl(Form, 'TFileHistoryFrame'));
  if not Assigned(HistoryFrame) then
    Exit;

  PageControl1 := TPageControl(HistoryFrame.FindComponent('PageControl1'));
  if not Assigned(PageControl1) then
    Exit;

  for I := 0 to PageControl1.PageCount - 1 do
  begin
    TabSheet := PageControl1.Pages[I];
    for J := 0 to TabSheet.ControlCount - 1 do
      if TabSheet.Controls[J].ClassNameIs('TEditControl') then
        TabSheet.Controls[J].Visible := True;
  end;
  {$ENDIF}
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnIDEClient event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.DataModuleCreate(Sender: TObject);

begin
  SvnIDEModule := Self;
  Initialize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.DataModuleDestroy(Sender: TObject);

begin
  Finalize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCancelExecute(Sender: TObject);

begin
  if Assigned(FormSvnTools) and FormSvnTools.IsBusy then
    FormSvnTools.Cancel;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCancelUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := Assigned(FormSvnTools) and FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCheckModificationsExecute(Sender: TObject);

var
  Directories: TStringList;

begin
  Directories := TStringList.Create;
  try
    GetDirectories(Directories);
    if Directories.Count > 0 then
    begin
      if not Assigned(FormSvnTools) then
        FormSvnTools := TFormSvnTools.Create(Application);
      FormSvnTools.Show;
      FormSvnTools.StartSvnCheckModifications(FSvnClient, Directories, True, True, False, FSettings.RecurseUnversioned);
    end;
  finally
    Directories.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCheckModificationsUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := not Assigned(FormSvnTools) or not FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCleanupExecute(Sender: TObject);

var
  Directories: TStringList;

begin
  if MessageDlg(SConfirmCleanup, mtConfirmation, [mbYes, mbNo], 0 {$IFDEF COMPILER_10_UP}, mbNo {$ENDIF}) <> mrYes then
    Exit;

  Directories := TStringList.Create;
  try
    GetDirectories(Directories);
    if Directories.Count > 0 then
    begin
      if not Assigned(FormSvnTools) then
        FormSvnTools := TFormSvnTools.Create(Application);
      FormSvnTools.Show;
      FormSvnTools.StartSvnCleanup(FSvnClient, Directories);
    end;
  finally
    Directories.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCleanupUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := not Assigned(FormSvnTools) or not FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCommitExecute(Sender: TObject);

var
  LogMessage: string;
  Directories, Externals: TStringList;
  I: Integer;

begin
  LogMessage := '';
  if ShowSvnLogMessagePrompt('Enter your commit log message:', LogMessage) <> mrOK then
    Exit;

  Externals := nil;
  Directories := TStringList.Create;
  try
    if FSettings.CommitExternals then
    begin
      Externals := TStringList.Create;
      Externals.Duplicates := dupIgnore;
    end;
    GetDirectories(Directories);
    if Directories.Count > 0 then
    begin
      if FSettings.CommitExternals then
      begin
        for I := 0 to Directories.Count - 1 do
          FSvnClient.GetExternals(Directories[I], Externals);
        Directories.AddStrings(Externals);
      end;

      if not Assigned(FormSvnTools) then
        FormSvnTools := TFormSvnTools.Create(Application);
      FormSvnTools.Show;
      FormSvnTools.StartSvnCommit(FSvnClient, Directories, LogMessage, True, False, Assigned(Externals) and
        (Externals.Count > 0));
    end;
  finally
    Externals.Free;
    Directories.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionCommitUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := not Assigned(FormSvnTools) or not FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionOptionsExecute(Sender: TObject);

var
  Directories: string;
  History: TStrings;

begin
  FSettings.LoadSettings;

  Directories := FSettings.Directories;
  History := TStringList.Create;
  try
    History.Assign(FSettings.DirHistory);
    if ShowSvnOptionsDialog(FSettings) = mrOK then
      FSettings.SaveSettings
    else
      FSettings.LoadSettings;
  finally
    History.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionRevertExecute(Sender: TObject);

var
  Directories: TStringList;

begin
  if MessageDlg(SConfirmRevert, mtConfirmation, [mbYes, mbNo], 0 {$IFDEF COMPILER_10_UP}, mbNo {$ENDIF}) <> mrYes then
    Exit;
    
  Directories := TStringList.Create;
  try
    GetDirectories(Directories);
    if Directories.Count > 0 then
    begin
      if not Assigned(FormSvnTools) then
        FormSvnTools := TFormSvnTools.Create(Application);
      FormSvnTools.Show;
      FormSvnTools.StartSvnRevert(FSvnClient, Directories);
    end;
  finally
    Directories.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionRevertUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := not Assigned(FormSvnTools) or not FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionUpdateExecute(Sender: TObject);

var
  Directories: TStringList;

begin
  Directories := TStringList.Create;
  try
    GetDirectories(Directories);
    if Directories.Count > 0 then
    begin
      if not Assigned(FormSvnTools) then
        FormSvnTools := TFormSvnTools.Create(Application);
      FormSvnTools.Show;
      FormSvnTools.StartSvnUpdate(FSvnClient, Directories);
    end;
  finally
    Directories.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnIDEClient.ActionUpdateUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := not Assigned(FormSvnTools) or not FormSvnTools.IsBusy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  TSvnIDEClient.Create(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

initialization

finalization
  FreeAndNil(SvnIDEModule);

//----------------------------------------------------------------------------------------------------------------------

end.
