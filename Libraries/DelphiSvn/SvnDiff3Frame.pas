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
{ The Original Code is SvnDiff3Frame.pas.                                                                              }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains the frame class used to display 3-way diff and merge conflicts.                                   }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnDiff3Frame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ImgList, ActnList, StdCtrls,
  ExtCtrls, ComCtrls, ToolWin, Menus,
  SynEditTypes, SynEdit, SynEditHighlighter,
  svn_client, SvnClient;

const
  AM_RESETVSPLITTER = WM_USER + 1;

type
  TLineSource = (lsOriginal, lsMine, lsTheirs);
  PLineInfo = ^TLineInfo;
  TLineInfo = record
    Source: TLineSource;
    LineNo: Integer;
    Diff: Integer;
  end;
  TSvnDiffTypes = set of TSvnDiffType;

  TResolutionKind = (rkUseMine, rkUseTheirs, rkMerge);
  TResolutionEvent = procedure(Sender: TObject; Kind: TResolutionKind) of object;

  TFrameSvnDiff3 = class(TFrame)
    ActionList: TActionList;
    ActionNextConflict: TAction;
    ActionNextDiff: TAction;
    ActionPrevConflict: TAction;
    ActionPrevDiff: TAction;
    ActionResolved: TAction;
    ActionResolveMine: TAction;
    ActionResolveTheirs: TAction;
    ActionShowOriginal: TAction;
    ActionUseMine: TAction;
    ActionUseTheirs: TAction;
    ImageList: TImageList;
    MenuResolveMine: TMenuItem;
    MenuResolveTheirs: TMenuItem;
    MenuUseMine: TMenuItem;
    MenuUseTheirs: TMenuItem;
    PanelTop: TPanel;
    PopupMenu: TPopupMenu;
    PopupMenu2: TPopupMenu;
    Splitter: TSplitter;
    ToolBar: TToolBar;
    ToolButtonNextConflict: TToolButton;
    ToolButtonNextDiff: TToolButton;
    ToolButtonPrevConflict: TToolButton;
    ToolButtonPrevDiff: TToolButton;
    ToolButtonResolved: TToolButton;
    ToolButtonSeparator1: TToolButton;
    ToolButtonSeparator2: TToolButton;
    ToolButtonShowOriginal: TToolButton;
    ToolButtonUseMine: TToolButton;
    ToolButtonUserTheirs: TToolButton;

    procedure ActionNextConflictExecute(Sender: TObject);
    procedure ActionNextConflictUpdate(Sender: TObject);
    procedure ActionNextDiffExecute(Sender: TObject);
    procedure ActionNextDiffUpdate(Sender: TObject);
    procedure ActionPrevConflictExecute(Sender: TObject);
    procedure ActionPrevConflictUpdate(Sender: TObject);
    procedure ActionPrevDiffExecute(Sender: TObject);
    procedure ActionPrevDiffUpdate(Sender: TObject);
    procedure ActionResolvedExecute(Sender: TObject);
    procedure ActionResolvedUpdate(Sender: TObject);
    procedure ActionResolveMineExecute(Sender: TObject);
    procedure ActionResolveMineUpdate(Sender: TObject);
    procedure ActionResolveTheirsExecute(Sender: TObject);
    procedure ActionResolveTheirsUpdate(Sender: TObject);
    procedure ActionShowOriginalExecute(Sender: TObject);
    procedure ActionUseMineExecute(Sender: TObject);
    procedure ActionUseMineUpdate(Sender: TObject);
    procedure ActionUseTheirsExecute(Sender: TObject);
    procedure ActionUseTheirsUpdate(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
  private
    FDiffs: TList;
    FEdit1: TSynEdit;
    FEdit2: TSynEdit;
    FEdit3: TSynEdit;
    FHighlighter: TSynCustomHighlighter;
    FItem: TSvnItem;
    FHorzRatio: Single;
    FLabel1: TLabel;
    FLabel2: TLabel;
    FMyLines: TStrings;
    FOriginalLines: TStrings;
    FPanel1: TPanel;
    FPanel2: TPanel;
    FSelectedDiff: Integer;
    FSelecting: Boolean;
    FSplitterTop: TSplitter;
    FTheirLines: TStrings;
    FVertRatio: Single;

    FOnItemResolved: TResolutionEvent;

    procedure ClearLineInfos(Edit: TSynEdit);
    procedure EditGutterGetText(Sender: TObject; ALine: Integer; var AText: string);
    procedure EditGutterPaint(Sender: TObject; ALine, X, Y: Integer);
    procedure EditScroll(Sender: TObject; ScrollBar: TScrollBarKind);
    procedure EditSpecialLineColors(Sender: TObject; Line: integer; var Special: Boolean; var FG, BG: TColor);
    procedure EditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    function FindNextDiff(var Index: Integer; const Types: TSvnDiffTypes; Edit: TSynEdit = nil): PSvnDiff;
    function FindPrevDiff(var Index: Integer; const Types: TSvnDiffTypes; Edit: TSynEdit = nil): PSvnDiff;
    function FindFirstLine(Edit: TSynEdit; Diff: PSvnDiff): Integer; overload;
    function FindFirstLine(Edit: TSynEdit; Diff: PSvnDiff; Source: TLineSource): Integer; overload;
    function GetDiffCount: Integer;
    function GetDiffs(Index: Integer): TSvnDiff;
    function GetFocusedEdit: TSynEdit;
    procedure ReselectDiff(Index: Integer);
    procedure SelectDiff(P: PSvnDiff; Forwards: Boolean);
    procedure SelectLines(Edit: TSynEdit; StartLine, LineCount: Integer);
    procedure SetHighlighter(Value: TSynCustomHighlighter);
    procedure SetItem(Value: TSvnItem);
    procedure SplitterDblClick(Sender: TObject);

    procedure AMResetVSplitter(var Message: TWMNoParams); message AM_RESETVSPLITTER;
  protected
    procedure DoItemResolved(Kind: TResolutionKind); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize(OriginalLines, MyLines, TheirLines: TStrings; Diff: PSvnDiff);

    property DiffCount: Integer read GetDiffCount;
    property Diffs[Index: Integer]: TSvnDiff read GetDiffs;
    property Edit1: TSynEdit read FEdit1;
    property Edit2: TSynEdit read FEdit2;
    property Edit3: TSynEdit read FEdit3;
    property Highlighter: TSynCustomHighlighter read FHighlighter write SetHighlighter;
    property Item: TSvnItem read FItem write SetItem;
    property Label1: TLabel read FLabel1;
    property Label2: TLabel read FLabel2;

    property OnItemResolved: TResolutionEvent read FOnItemResolved write FOnItemResolved;
  end;

implementation

uses
  Math;

{$R *.dfm}

const
  DiffTypes: TSvnDiffTypes = [svnDiffTypeDiffModified, svnDiffTypeDiffLatest, svnDiffTypeDiffCommon,
    svnDiffTypeConflict];
  GutterMargin = 2;

resourcestring
  SConfirmResolve = 'Mark conflicts as resolved?';
  SConfirmResolveMine = 'Resolve conflicts using "mine" (''%s'')?';
  SConfirmResolveTheirs = 'Resolve conflicts using "theirs" (''%s'')?';

type
  THackControl = class(TControl);

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnDiff private }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ClearLineInfos(Edit: TSynEdit);

var
  I: Integer;
  P: PLineInfo;

begin
  for I := 0 to Edit.Lines.Count - 1 do
  begin
    P := PLineInfo(Edit.Lines.Objects[I]);
    if Assigned(P) then
    begin
      FreeMem(P);
      Edit.Lines.Objects[I] := nil;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.EditGutterGetText(Sender: TObject; ALine: Integer; var AText: string);

begin
  if (Sender <> FEdit3) or (ALine >= TSynEdit(Sender).Lines.Count) then
    AText := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.EditGutterPaint(Sender: TObject; ALine, X, Y: Integer);

var
  P: PLineInfo;
  LineHeight, LineNoWidth, ImageIndex: Integer;
  R: TRect;
  S: string;

begin
  P := PLineInfo(TSynEdit(Sender).Lines.Objects[ALine - 1]);
  if not Assigned(P) then
    Exit;

  TSynEdit(Sender).Canvas.Font := TSynEdit(Sender).Gutter.Font;
  LineHeight := TSynEdit(Sender).LineHeight;
  LineNoWidth := TSynEdit(Sender).Canvas.TextWidth(StringOfChar('X', TSynEdit(Sender).Gutter.DigitCount));
  R := Rect(0, 0, LineNoWidth, LineHeight);
  OffsetRect(R, X + GutterMargin, Y);
  case P^.Source of
    lsMine, lsTheirs:
      begin
        S := IntToStr(P^.LineNo + 1);
        DrawText(TSynEdit(Sender).Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_RIGHT);
      end;
  end;
  OffsetRect(R, LineNoWidth, 0);

  ImageIndex := -1;
  case P^.Source of
    lsOriginal:
      ImageIndex := 5;
    lsMine:
      case PSvnDiff(FDiffs[P^.Diff])^._type of
        svnDiffTypeDiffModified, svnDiffTypeDiffCommon:
          ImageIndex := 6;
        svnDiffTypeConflict:
          ImageIndex := 7;
      end;
    lsTheirs:
      case PSvnDiff(FDiffs[P^.Diff])^._type of
        svnDiffTypeDiffLatest, svnDiffTypeDiffCommon:
          ImageIndex := 6;
        svnDiffTypeConflict:
          ImageIndex := 8;
      end;
  end;

  if ImageIndex <> -1 then
    ImageList.Draw(TSynEdit(Sender).Canvas, R.Left, R.Top, ImageIndex);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.EditScroll(Sender: TObject; ScrollBar: TScrollBarKind);

var
  Targets: array[0..1] of TSynEdit;

begin
  if Sender = FEdit1 then
  begin
    Targets[0] := FEdit2;
    Targets[1] := FEdit3;
  end
  else if Sender = FEdit2 then
  begin
    Targets[0] := FEdit1;
    Targets[1] := FEdit3;
  end
  else
  begin
    Targets[0] := FEdit1;
    Targets[1] := FEdit2;
  end;

  case ScrollBar of
    sbHorizontal:
      begin
        Targets[0].LeftChar := TSynEdit(Sender).LeftChar;
        Targets[1].LeftChar := TSynEdit(Sender).LeftChar;
      end;
    sbVertical:
      begin
        Targets[0].TopLine := TSynEdit(Sender).TopLine;
        Targets[1].TopLine := TSynEdit(Sender).TopLine;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.EditSpecialLineColors(Sender: TObject; Line: integer; var Special: Boolean; var FG, BG: TColor);

const
  OriginalColor: TColor = $C1F5FF;
  MyColor: TColor = $D0FDD0;
  TheirColor: TColor = $C7C7FF;
  MergedColor: TColor = $FFD9D9;

var
  P: PLineInfo;

begin
  P := PLineInfo(TSynEdit(Sender).Lines.Objects[Line - 1]);
  if Assigned(P) then
  begin
    Special := True;
    if Sender = FEdit1 then
    begin
      case P^.Source of
        lsOriginal:
          BG := OriginalColor;
        lsMine:
          case PSvnDiff(FDiffs[P^.Diff])^._type of
            svnDiffTypeDiffModified, svnDiffTypeDiffCommon:
              BG := MergedColor;
            svnDiffTypeConflict:
              BG := MyColor;
          end;
      end;
    end
    else if Sender = FEdit2 then
    begin
      case P^.Source of
        lsOriginal:
          BG := OriginalColor;
        lsTheirs:
          case PSvnDiff(FDiffs[P^.Diff])^._type of
            svnDiffTypeDiffLatest, svnDiffTypeDiffCommon:
              BG := MergedColor;
            svnDiffTypeConflict:
              BG := TheirColor;
          end;
      end;
    end
    else if Sender = FEdit3 then
    begin
      case PSvnDiff(FDiffs[P^.Diff])^._type of
        svnDiffTypeDiffModified, svnDiffTypeDiffLatest, svnDiffTypeDiffCommon:
          BG := MergedColor;
        svnDiffTypeConflict:
          case P^.Source of
            lsMine:
              BG := MyColor;
            lsTheirs:
              BG := TheirColor;
          end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.EditStatusChange(Sender: TObject; Changes: TSynStatusChanges);

begin
  if (scSelection in Changes) and not FSelecting then
    FSelectedDiff := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnDiff3.FindNextDiff(var Index: Integer; const Types: TSvnDiffTypes; Edit: TSynEdit = nil): PSvnDiff;

var
  CaretPos: TBufferCoord;
  Start, I: Integer;
  P: PLineInfo;

begin
  Result := nil;

  if Assigned(Edit) then // start from caret position
  begin
    CaretPos := Edit.CaretXY;
    Index := CaretPos.Line - 1;
    if (Index >= 0) and (Index <= Edit.Lines.Count - 1) then
    begin
      P := PLineInfo(Edit.Lines.Objects[Index]);
      while not Assigned(P) do
      begin
        Inc(Index);
        if Index > Edit.Lines.Count - 1 then
          Break;
        P := PLineInfo(Edit.Lines.Objects[Index]);
      end;

      if Assigned(P) then
      begin
        Index := P^.Diff;
        case P^.Source of
          lsOriginal:
            Start := PSvnDiff(FDiffs[Index])^.original_start;
          lsMine:
            Start := PSvnDiff(FDiffs[Index])^.modified_start;
          lsTheirs:
            Start := PSvnDiff(FDiffs[Index])^.latest_start;
          else
            Start := -1;
        end;
        if ((CaretPos.Char = 1) and (P^.LineNo = Start)) or (P^.Source = lsOriginal) then
          Dec(Index);
      end
      else
        Index := -1;
    end
    else
      Index := -1;
  end;

  if Index < 0 then
    Start := 0
  else
    Start := Index + 1;

  Index := FDiffs.Count;
  for I := Start to FDiffs.Count - 1 do
    if PSvnDiff(FDiffs[I])^._type in Types then
    begin
      Index := I;
      Result := FDiffs[Index];
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnDiff3.FindPrevDiff(var Index: Integer; const Types: TSvnDiffTypes; Edit: TSynEdit = nil): PSvnDiff;

var
  CaretPos: TBufferCoord;
  Start, I: Integer;
  P: PLineInfo;

begin
  Result := nil;

  if Assigned(Edit) then
  begin
    CaretPos := Edit.CaretXY;
    Index := CaretPos.Line - 1;
    if (Index >= 0) and (Index <= Edit.Lines.Count - 1) then
    begin
      P := PLineInfo(Edit.Lines.Objects[Index]);
      while not Assigned(P) do
      begin
        Dec(Index);
        if Index < 0 then
          Break;
        P := PLineInfo(Edit.Lines.Objects[Index]);
      end;

      if Assigned(P) then
      begin
        Index := P^.Diff;
        case P^.Source of
          lsOriginal:
            Start := PSvnDiff(FDiffs[Index])^.original_start;
          lsMine:
            Start := PSvnDiff(FDiffs[Index])^.modified_start;
          lsTheirs:
            Start := PSvnDiff(FDiffs[Index])^.latest_start;
          else
            Start := FDiffs.Count;
        end;
        if ((CaretPos.Char > 1) or (P^.LineNo > Start)) and (P^.Source <> lsOriginal) then
          Inc(Index);
      end
      else
        Index := FDiffs.Count;
    end
    else
      Index := FDiffs.Count;
  end;
  
  if Index > FDiffs.Count - 1 then
    Start := FDiffs.Count - 1
  else
    Start := Index - 1;

  Index := -1;
  for I := Start downto 0 do
    if PSvnDiff(FDiffs[I])^._type in Types then
    begin
      Index := I;
      Result := FDiffs[Index];
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnDiff3.FindFirstLine(Edit: TSynEdit; Diff: PSvnDiff): Integer;

var
  I: Integer;
  P: PLineInfo;

begin
  Result := -1;

  for I := 0 to Edit.Lines.Count - 1 do
  begin
    P := PLineInfo(Edit.Lines.Objects[I]);
    if Assigned(P) and (P^.Diff <> -1) and (FDiffs[P^.Diff] = Diff) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnDiff3.FindFirstLine(Edit: TSynEdit; Diff: PSvnDiff; Source: TLineSource): Integer;

var
  I: Integer;
  P: PLineInfo;

begin
  Result := -1;

  I := FindFirstLine(Edit, Diff);
  if I = -1 then
    Exit;

  while I <= Edit.Lines.Count - 1 do
  begin
    P := PLineInfo(Edit.Lines.Objects[I]);
    if not Assigned(P) or (P^.Diff = -1) or (FDiffs[P^.Diff] <> Diff) then
      Exit;
      
    if (P^.Source = Source) then
    begin
      Result := I;
      Break;
    end;

    Inc(I);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnDiff3.GetDiffCount: Integer;

begin
  Result := FDiffs.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnDiff3.GetDiffs(Index: Integer): TSvnDiff;

begin
  Result := PSvnDiff(FDiffs[Index])^;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnDiff3.GetFocusedEdit: TSynEdit;

begin
  Result := nil;
  
  if FEdit1.Focused then
    Result := FEdit1
  else if FEdit2.Focused then
    Result := FEdit2
  else if FEdit3.Focused then
    Result := FEdit3;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ReselectDiff(Index: Integer);

begin
  if Index < 0 then
  begin
    FSelectedDiff := -1;
    SelectDiff(nil, False);
  end
  else if (Index >= 0) and (Index <= FDiffs.Count - 1) then
  begin
    FSelectedDiff := Index;
    SelectDiff(FDiffs[FSelectedDiff], True);
  end
  else
  begin
    FSelectedDiff := FDiffs.Count;
    SelectDiff(nil, True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.SelectDiff(P: PSvnDiff; Forwards: Boolean);

var
  I: Integer;
  LineInfo: PLineInfo;

begin
  FSelecting := True;
  try
    if Assigned(P) then
    begin
      SelectLines(FEdit1, FindFirstLine(FEdit1, P, lsMine), P^.modified_length);
      SelectLines(FEdit2, FindFirstLine(FEdit2, P, lsTheirs), P^.latest_length);
      I := FindFirstLine(FEdit3, P);
      if I <> -1 then
      begin
        LineInfo := PLineInfo(FEdit3.Lines.Objects[I]);
        case LineInfo^.Source of
          lsMine:
            SelectLines(FEdit3, I, P^.modified_length);
          lsTheirs:
            SelectLines(FEdit3, I, P^.latest_length);
        end;
      end;
    end
    else
    begin
      if Forwards then
      begin
        SelectLines(FEdit1, FEdit1.Lines.Count, 0);
        SelectLines(FEdit2, FEdit2.Lines.Count, 0);
        SelectLines(FEdit3, FEdit3.Lines.Count, 0);
      end
      else
      begin
        SelectLines(FEdit1, 0, 0);
        SelectLines(FEdit2, 0, 0);
        SelectLines(FEdit3, 0, 0);
      end;
    end;
  finally
    FSelecting := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.SelectLines(Edit: TSynEdit; StartLine, LineCount: Integer);

var
  C: TBufferCoord;

begin
  C.Char := 1;
  C.Line := StartLine + 1;
  Edit.SelStart := Edit.RowColToCharIndex(C);
  C.Line := StartLine + 1 + LineCount;
  Edit.SelEnd := Edit.RowColToCharIndex(C);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.SetHighlighter(Value: TSynCustomHighlighter);

begin
  if Value <> FHighlighter then
  begin
    FHighlighter := Value;
    Edit1.Highlighter := FHighlighter;
    Edit2.Highlighter := FHighlighter;
    Edit3.Highlighter := FHighlighter;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.SetItem(Value: TSvnItem);

begin
  if FItem <> Value then
  begin
    FItem := Value;
    ToolButtonResolved.Visible := Assigned(FItem) and (FItem.TextStatus = svnWcStatusConflicted);
    if Assigned(FItem) and (FItem.TextStatus = svnWcStatusConflicted) then
    begin
      FLabel1.Caption := FItem.ConflictWorkingFile;
      FLabel2.Caption := FItem.ConflictNewFile;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.SplitterDblClick(Sender: TObject);

begin
  PostMessage(Handle, AM_RESETVSPLITTER, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.AMResetVSplitter(var Message: TWMNoParams);

begin
  FVertRatio := 0.5;
  FPanel1.Width := Round((PanelTop.ClientWidth - FSplitterTop.Width) * FVertRatio);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnDiff protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.DoItemResolved(Kind: TResolutionKind);

begin
  if Assigned(FOnItemResolved) then
    FOnItemResolved(Self, Kind);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnDiff public }

//----------------------------------------------------------------------------------------------------------------------

constructor TFrameSvnDiff3.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FOriginalLines := TStringList.Create;
  FMyLines := TStringList.Create;
  FTheirLines := TStringList.Create;
  FDiffs := TList.Create;
  FHorzRatio := 0.5;
  FVertRatio := 0.5;
  FSelectedDiff := -1;

  FHighlighter := nil;

  FPanel1 := TPanel.Create(Self);
  FPanel1.Parent := PanelTop;
  FPanel1.Align := alLeft;
  FPanel1.BevelInner := bvNone;
  FPanel1.BevelOuter := bvNone;
  FPanel1.Caption := '';

  FSplitterTop := TSplitter.Create(Self);
  FSplitterTop.Parent := PanelTop;
  FSplitterTop.Left := FPanel1.Width + 1;
  FSplitterTop.Align := alLeft;
  FSplitterTop.OnMoved := SplitterMoved;
  THackControl(FSplitterTop).OnDblClick := SplitterDblClick;

  FPanel2 := TPanel.Create(Self);
  FPanel2.Parent := PanelTop;
  FPanel2.Left := FSplitterTop.Left + FSplitterTop.Width + 1;
  FPanel2.Align := alClient;
  FPanel2.BevelInner := bvNone;
  FPanel2.BevelOuter := bvNone;
  FPanel2.Caption := '';

  FLabel1 := TLabel.Create(Self);
  FLabel1.Parent := FPanel1;
  FLabel1.Align := alTop;
  FLabel1.Caption := '';

  FEdit1 := TSynEdit.Create(Self);
  FEdit1.Parent := FPanel1;
  FEdit1.Align := alClient;
  FEdit1.ReadOnly := True;
  FEdit1.Gutter.ShowLineNumbers := True;
  FEdit1.PopupMenu := PopupMenu;
  FEdit1.OnGutterGetText := EditGutterGetText;
  FEdit1.OnGutterPaint := EditGutterPaint;
  FEdit1.OnScroll := EditScroll;
  FEdit1.OnSpecialLineColors := EditSpecialLineColors;
  FEdit1.OnStatusChange := EditStatusChange;

  FLabel2 := TLabel.Create(Self);
  FLabel2.Parent := FPanel2;
  FLabel2.Align := alTop;
  FLabel2.Caption := '';

  FEdit2 := TSynEdit.Create(Self);
  FEdit2.Parent := FPanel2;
  FEdit2.Align := alClient;
  FEdit2.ReadOnly := True;
  FEdit2.Gutter.ShowLineNumbers := True;
  FEdit2.PopupMenu := PopupMenu;
  FEdit2.OnGutterGetText := EditGutterGetText;
  FEdit2.OnGutterPaint := EditGutterPaint;
  FEdit2.OnScroll := EditScroll;
  FEdit2.OnSpecialLineColors := EditSpecialLineColors;
  FEdit2.OnStatusChange := EditStatusChange;

  FEdit3 := TSynEdit.Create(Self);
  FEdit3.Parent := Self;
  FEdit3.Top := Splitter.Top + Splitter.Height + 1;
  FEdit3.Align := alClient;
  FEdit3.ReadOnly := True;
  FEdit3.Gutter.ShowLineNumbers := True;
  FEdit3.PopupMenu := PopupMenu;
  FEdit3.OnGutterGetText := EditGutterGetText;
  FEdit3.OnScroll := EditScroll;
  FEdit3.OnSpecialLineColors := EditSpecialLineColors;
  FEdit3.OnStatusChange := EditStatusChange;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TFrameSvnDiff3.Destroy;

begin
  ClearLineInfos(FEdit1);
  ClearLineInfos(FEdit2);
  ClearLineInfos(FEdit3);
  FDiffs.Free;
  FTheirLines.Free;
  FMyLines.Free;
  FOriginalLines.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.Initialize(OriginalLines, MyLines, TheirLines: TStrings; Diff: PSvnDiff);

var
  I: Integer;
  KeepMergedLines: Boolean;

  function AddLineInfo(Edit: TSynEdit; Lines: TStrings; LineNo: Integer; Source: TLineSource;
    Diff: PSvnDiff): PLineInfo;
  begin
    Result := AllocMem(SizeOf(TLineInfo));
    try
      Result^.Source := Source;
      Result^.LineNo := LineNo;
      Result^.Diff := FDiffs.IndexOf(Diff);
      if Result^.Diff = -1 then
        Result^.Diff := FDiffs.Add(Diff);
      Edit.Lines.AddObject(Lines[I], TObject(Result));
    except
      FreeMem(Result);
      raise;
    end;
  end;

  procedure AdjustGutter(Edit: TSynEdit);
  begin
    if Edit.Lines.Count > 9999 then
      Edit.Gutter.DigitCount := Trunc(Log10(Edit.Lines.Count)) + 1
    else
      Edit.Gutter.DigitCount := 4;
    Edit.Gutter.Width := Edit.Canvas.TextWidth(StringOfChar('X', Edit.Gutter.DigitCount)) +
      ImageList.Width +  3 * GutterMargin;
  end;

begin
  // reloading existing information with changed value of ActionShowOriginalLines.Checked:
  // reload FEdit1 and FEdit2, leave FEdit3 as is
  KeepMergedLines := Assigned(OriginalLines) and (OriginalLines = FOriginalLines) and
    Assigned(MyLines) and (MyLines = FMyLines) and
    Assigned(TheirLines) and (TheirLines = FTheirLines) and
    (FDiffs.Count > 0) and (Diff = FDiffs[0]);

  if not KeepMergedLines then
    FDiffs.Clear;
  FSelectedDiff := -1;

  FEdit1.Lines.BeginUpdate;
  try
    FEdit2.Lines.BeginUpdate;
    try
      FEdit3.Lines.BeginUpdate;
      try
        ClearLineInfos(FEdit1);
        ClearLineInfos(FEdit2);
        if not KeepMergedLines then
          ClearLineInfos(FEdit3);
        FEdit1.Clear;
        FEdit2.Clear;
        if not KeepMergedLines then
          FEdit3.Clear;

        if not Assigned(OriginalLines) or not Assigned(MyLines) or not Assigned(TheirLines) or not Assigned(Diff) then
        begin
          FOriginalLines.Clear;
          FMyLines.Clear;
          FTheirLines.Clear;
          Exit;
        end;

        if FOriginalLines <> OriginalLines then
          FOriginalLines.Assign(OriginalLines);

        if FMyLines <> MyLines then
          FMyLines.Assign(MyLines);

        if FTheirLines <> TheirLines then
          FTheirLines.Assign(TheirLines);

        while Assigned(Diff) do
        begin
          case Diff^._type of
            svnDifftypeCommon:
              begin
                for I := Diff^.modified_start to Diff^.modified_start + Diff^.modified_length - 1 do
                  AddLineInfo(FEdit1, FMyLines, I, lsMine, Diff);

                for I := Diff^.latest_start to Diff^.latest_start + Diff^.latest_length - 1 do
                  AddLineInfo(FEdit2, FTheirLines, I, lsTheirs, Diff);

                if not KeepMergedLines then
                  for I := Diff^.original_start to Diff^.original_start + Diff^.original_length - 1 do
                    AddLineInfo(FEdit3, FOriginalLines, I, lsOriginal, Diff);
              end;
            svnDiffTypeDiffModified:
              begin
                if ActionShowOriginal.Checked then
                  for I := Diff^.original_start to Diff^.original_start + Diff^.original_length - 1 do
                    AddLineInfo(FEdit1, FOriginalLines, I, lsOriginal, Diff);

                for I := Diff^.modified_start to Diff^.modified_start + Diff^.modified_length - 1 do
                begin
                  AddLineInfo(FEdit1, FMyLines, I, lsMine, Diff);
                  if not KeepMergedLines then
                    AddLineInfo(FEdit3, FMyLines, I, lsMine, Diff);
                end;

                for I := Diff^.latest_start to Diff^.latest_start + Diff^.latest_length - 1 do
                  AddLineInfo(FEdit2, FTheirLines, I, lsTheirs, Diff);
              end;
            svnDiffTypeDiffLatest:
              begin
                if ActionShowOriginal.Checked then
                  for I := Diff^.original_start to Diff^.original_start + Diff^.original_length - 1 do
                    AddLineInfo(FEdit2, FOriginalLines, I, lsOriginal, Diff);

                for I := Diff^.modified_start to Diff^.modified_start + Diff^.modified_length - 1 do
                  AddLineInfo(FEdit1, FMyLines, I, lsMine, Diff);

                for I := Diff^.latest_start to Diff^.latest_start + Diff^.latest_length - 1 do
                begin
                  AddLineInfo(FEdit2, FTheirLines, I, lsTheirs, Diff);
                  if not KeepMergedLines then
                    AddLineInfo(FEdit3, FTheirLines, I, lsTheirs, Diff);
                end;
              end;
            svnDiffTypeDiffCommon:
              begin
                for I := Diff^.modified_start to Diff^.modified_start + Diff^.modified_length - 1 do
                begin
                  AddLineInfo(FEdit1, FMyLines, I, lsMine, Diff);
                  if not KeepMergedLines then
                    AddLineInfo(FEdit3, FMyLines, I, lsMine, Diff);
                end;
                for I := Diff^.latest_start to Diff^.latest_start + Diff^.latest_length - 1 do
                  AddLineInfo(FEdit2, FTheirLines, I, lsTheirs, Diff);
              end;
            svnDiffTypeConflict:
              begin
                if ActionShowOriginal.Checked then
                  for I := Diff^.original_start to Diff^.original_start + Diff^.original_length - 1 do
                  begin
                    AddLineInfo(FEdit1, FOriginalLines, I, lsOriginal, Diff);
                    AddLineInfo(FEdit2, FOriginalLines, I, lsOriginal, Diff);
                  end;

                for I := Diff^.modified_start to Diff^.modified_start + Diff^.modified_length - 1 do
                begin
                  AddLineInfo(FEdit1, FMyLines, I, lsMine, Diff);
                  if not KeepMergedLines then
                    AddLineInfo(FEdit3, FMyLines, I, lsMine, Diff);
                end;

                for I := Diff^.latest_start to Diff^.latest_start + Diff^.latest_length - 1 do
                  AddLineInfo(FEdit2, FTheirLines, I, lsTheirs, Diff);
              end;
          end;

          Diff := Diff^.next;
        end;

        FEdit1.Lines.Add('');
        FEdit2.Lines.Add('');
        if not KeepMergedLines then
          FEdit3.Lines.Add('');

        AdjustGutter(FEdit1);
        AdjustGutter(FEdit2);
      finally
        FEdit3.Lines.EndUpdate;
      end;
    finally
      FEdit2.Lines.EndUpdate;
    end;
  finally
    FEdit1.Lines.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnDiff3 event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionNextConflictExecute(Sender: TObject);

var
  Edit: TSynEdit;

begin
  Edit := nil;
  
  if FSelectedDiff = -1 then
  begin
    Edit := GetFocusedEdit;
    if Assigned(Edit) and (Edit.SelStart = 0) and (Edit.SelLength = 0) then
      Edit := nil;
  end;

  SelectDiff(FindNextDiff(FSelectedDiff, [svnDiffTypeConflict], Edit), True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionNextConflictUpdate(Sender: TObject);

var
  Edit: TSynEdit;

begin
  Edit := nil;

  if FSelectedDiff = -1 then
  begin
    Edit := GetFocusedEdit;
    if Assigned(Edit) and (Edit.SelStart = 0) and (Edit.SelLength = 0) then
      Edit := nil;
  end;

  (Sender as TAction).Enabled := (FSelectedDiff < FDiffs.Count) or Assigned(Edit);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionNextDiffExecute(Sender: TObject);

var
  Edit: TSynEdit;

begin
  Edit := nil;

  if FSelectedDiff = -1 then
  begin
    Edit := GetFocusedEdit;
    if Assigned(Edit) and (Edit.SelStart = 0) and (Edit.SelLength = 0) then
      Edit := nil;
  end;

  SelectDiff(FindNextDiff(FSelectedDiff, DiffTypes, Edit), True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionNextDiffUpdate(Sender: TObject);

var
  Edit: TSynEdit;

begin
  Edit := nil;

  if FSelectedDiff = -1 then
  begin
    Edit := GetFocusedEdit;
    if Assigned(Edit) and (Edit.SelStart = 0) and (Edit.SelLength = 0) then
      Edit := nil;
  end;

  (Sender as TAction).Enabled := (FSelectedDiff < FDiffs.Count) or Assigned(Edit);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionPrevConflictExecute(Sender: TObject);

var
  Edit: TSynEdit;

begin
  Edit := nil;

  if FSelectedDiff = -1 then
  begin
    Edit := GetFocusedEdit;
    if Assigned(Edit) and (Edit.SelStart = 0) and (Edit.SelLength = 0) then
      Edit := nil;
  end;

  SelectDiff(FindPrevDiff(FSelectedDiff, [svnDiffTypeConflict], Edit), False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionPrevConflictUpdate(Sender: TObject);

var
  Edit: TSynEdit;

begin
  Edit := nil;

  if FSelectedDiff = -1 then
  begin
    Edit := GetFocusedEdit;
    if Assigned(Edit) and (Edit.SelStart = 0) and (Edit.SelLength = 0) then
      Edit := nil;
  end;

  (Sender as TAction).Enabled := (FSelectedDiff >= 0) or Assigned(Edit);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionPrevDiffExecute(Sender: TObject);

var
  Edit: TSynEdit;

begin
  Edit := nil;

  if FSelectedDiff = -1 then
  begin
    Edit := GetFocusedEdit;
    if Assigned(Edit) and (Edit.SelStart = 0) and (Edit.SelLength = 0) then
      Edit := nil;
  end;

  SelectDiff(FindPrevDiff(FSelectedDiff, DiffTypes, Edit), False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionPrevDiffUpdate(Sender: TObject);

var
  Edit: TSynEdit;

begin
  Edit := nil;

  if FSelectedDiff = -1 then
  begin
    Edit := GetFocusedEdit;
    if Assigned(Edit) and (Edit.SelStart = 0) and (Edit.SelLength = 0) then
      Edit := nil;
  end;

  (Sender as TAction).Enabled := (FSelectedDiff >= 0) or Assigned(Edit);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionResolvedExecute(Sender: TObject);

begin
  if not Assigned(FItem) or (FItem.TextStatus <> svnWcStatusConflicted) then
    Exit;

  if MessageDlg(Format(SConfirmResolve, [FItem.PathName]), mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
  begin
    FItem.Resolved;
    FEdit3.Lines.SaveToFile(FItem.PathName);
    DoItemResolved(rkMerge);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionResolvedUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := Assigned(FItem) and (FItem.TextStatus = svnWcStatusConflicted);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionResolveMineExecute(Sender: TObject);

var
  ModifiedFileName: string;

begin
  if not Assigned(FItem) or (FItem.TextStatus <> svnWcStatusConflicted) then
    Exit;

  if MessageDlg(Format(SConfirmResolveMine, [FItem.ConflictWorkingFile]), mtConfirmation,
    [mbOK, mbCancel], 0) = mrOK then
  begin
    FItem.Resolved;
    ModifiedFileName := ExtractFilePath(FItem.PathName) + FItem.ConflictWorkingFile;
    Win32Check(CopyFile(PChar(ModifiedFileName), PChar(FItem.PathName), False));
    DoItemResolved(rkUseMine);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionResolveMineUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := Assigned(FItem) and (FItem.TextStatus = svnWcStatusConflicted);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionResolveTheirsExecute(Sender: TObject);

var
  LatestFileName: string;

begin
  if not Assigned(FItem) or (FItem.TextStatus <> svnWcStatusConflicted) then
    Exit;

  if MessageDlg(Format(SConfirmResolveTheirs, [FItem.ConflictNewFile]), mtConfirmation,
    [mbOK, mbCancel], 0) = mrOK then
  begin
    FItem.Resolved;
    LatestFileName := ExtractFilePath(FItem.PathName) + FItem.ConflictNewFile;
    Win32Check(CopyFile(PChar(LatestFileName), PChar(FItem.PathName), False));
    DoItemResolved(rkUseTheirs);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionResolveTheirsUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled := Assigned(FItem) and (FItem.TextStatus = svnWcStatusConflicted);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionShowOriginalExecute(Sender: TObject);

var
  SaveDiff: Integer;

begin
  SaveDiff := FSelectedDiff;
  Initialize(FOriginalLines, FMyLines, FTheirLines, FDiffs[0]);
  ReselectDiff(SaveDiff);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionUseMineExecute(Sender: TObject);

var
  P: PSvnDiff;
  SaveDiff, I, FirstLine: Integer;
  LineInfo: TLineInfo;
  PNewLineInfo: PLineInfo;

begin
  if (FSelectedDiff < 0) or (FSelectedDiff >= FDiffs.Count) then
    Exit;
  P := FDiffs[FSelectedDiff];
  if not Assigned(P) or (P^._type <> svnDiffTypeConflict) then
    Exit;
  SaveDiff := FSelectedDiff;

  FirstLine := FindFirstLine(FEdit3, P);
  if FirstLine <> -1 then
  begin
    LineInfo := PLineInfo(FEdit3.Lines.Objects[FirstLine])^;
    case LineInfo.Source of
      lsTheirs:
        begin
          FEdit3.Lines.BeginUpdate;
          try
            for I := 0 to P^.latest_length - 1 do
            begin
              FreeMem(Pointer(FEdit3.Lines.Objects[FirstLine]));
              FEdit3.Lines.Delete(FirstLine);
            end;
            for I := 0 to P^.modified_length - 1 do
            begin
              PNewLineInfo := AllocMem(SizeOf(TLineInfo));
              try
                PNewLineInfo^.Source := lsMine;
                PNewLineInfo^.LineNo := LineInfo.LineNo;
                PNewLineInfo^.Diff := FDiffs.IndexOf(P);
                FEdit3.Lines.InsertObject(FirstLine + I, FMyLines[P^.modified_start + I], TObject(PNewLineInfo));
                Inc(LineInfo.LineNo);
              except
                FreeMem(PNewLineInfo);
                raise;
              end;
            end;
          finally
            FEdit3.Lines.EndUpdate;
          end;
        end;
    end;
  end;

  FSelectedDiff := SaveDiff;
  SelectDiff(FindNextDiff(FSelectedDiff, [svnDiffTypeConflict]), True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionUseMineUpdate(Sender: TObject);

var
  P: PSvnDiff;

begin
  if (FSelectedDiff < 0) or (FSelectedDiff >= FDiffs.Count) then
    TAction(Sender).Enabled := False
  else
  begin
    P := FDiffs[FSelectedDiff];
    TAction(Sender).Enabled := Assigned(P) and (P^._type = svnDiffTypeConflict);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionUseTheirsExecute(Sender: TObject);

var
  P: PSvnDiff;
  SaveDiff, I, FirstLine: Integer;
  LineInfo: TLineInfo;
  PNewLineInfo: PLineInfo;

begin
  if (FSelectedDiff < 0) or (FSelectedDiff >= FDiffs.Count) then
    Exit;
  P := FDiffs[FSelectedDiff];
  if not Assigned(P) or (P^._type <> svnDiffTypeConflict) then
    Exit;
  SaveDiff := FSelectedDiff;

  FirstLine := FindFirstLine(FEdit3, P);
  if FirstLine <> -1 then
  begin
    LineInfo := PLineInfo(FEdit3.Lines.Objects[FirstLine])^;
    case LineInfo.Source of
      lsMine:
        begin
          FEdit3.Lines.BeginUpdate;
          try
            for I := 0 to P^.modified_length - 1 do
            begin
              FreeMem(Pointer(FEdit3.Lines.Objects[FirstLine]));
              FEdit3.Lines.Delete(FirstLine);
            end;
            for I := 0 to P^.latest_length - 1 do
            begin
              PNewLineInfo := AllocMem(SizeOf(TLineInfo));
              try
                PNewLineInfo^.Source := lsTheirs;
                PNewLineInfo^.LineNo := LineInfo.LineNo;
                PNewLineInfo^.Diff := FDiffs.IndexOf(P);
                FEdit3.Lines.InsertObject(FirstLine + I, FTheirLines[P^.latest_start + I], TObject(PNewLineInfo));
                Inc(LineInfo.LineNo);
              except
                FreeMem(PNewLineInfo);
                raise;
              end;
            end;
          finally
            FEdit3.Lines.EndUpdate;
          end;
        end;
    end;
  end;

  FSelectedDiff := SaveDiff;
  SelectDiff(FindNextDiff(FSelectedDiff, [svnDiffTypeConflict]), True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.ActionUseTheirsUpdate(Sender: TObject);

var
  P: PSvnDiff;

begin
  if (FSelectedDiff < 0) or (FSelectedDiff >= FDiffs.Count) then
    TAction(Sender).Enabled := False
  else
  begin
    P := FDiffs[FSelectedDiff];
    TAction(Sender).Enabled := Assigned(P) and (P^._type = svnDiffTypeConflict);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.FrameResize(Sender: TObject);

begin
  FPanel1.Width := Round((PanelTop.ClientWidth - FSplitterTop.Width) * FVertRatio);
  PanelTop.Height := Round((ClientHeight - ToolBar.Height - Splitter.Height) * FHorzRatio);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnDiff3.SplitterMoved(Sender: TObject);

begin
  if Sender = FSplitterTop then
    FVertRatio := FEdit1.Width / (PanelTop.ClientWidth - FSplitterTop.Width)
  else if Sender = Splitter then
    FHorzRatio := PanelTop.Height / (ClientHeight - ToolBar.Height - Splitter.Height);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
