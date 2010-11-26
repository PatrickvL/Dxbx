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
unit uXBEExplorerMain;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, Grids, ExtCtrls, Clipbrd,
  StdActns, ActnList, XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls, ActnMenus,
  StdCtrls, // TMemo
  StrUtils, // ContainsText
  Math, // Min
  ShellAPI, // DragQueryFile
  ExtDlgs, // TSavePictureDialog
  // Dxbx
  uConsts,
  uTypes,
  uDxbxUtils,
  uXbe,
  uViewerUtils,
  uSectionViewer,
  uStringsViewer,
  uDisassembleUtils,
  uDisassembleViewer,
  uExploreFileSystem;

type
  TFormXBEExplorer = class(TForm)
    MainMenu: TMainMenu;
    PageControl: TPageControl;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    OpenDialog: TOpenDialog;
    Close1: TMenuItem;
    pmImage: TPopupMenu;
    SaveAs1: TMenuItem;
    SavePictureDialog: TSavePictureDialog;
    ActionMainMenuBar: TActionMainMenuBar;
    ActionManager: TActionManager;
    actExit: TFileExit;
    actFileOpen: TAction;
    actClose: TAction;
    actSaveAs: TAction;
    Extra1: TMenuItem;
    ExploreFileSystem1: TMenuItem;
    Edit1: TMenuItem;
    Copy2: TMenuItem;
    Panel1: TPanel;
    TreeView1: TTreeView;
    Splitter1: TSplitter;
    Panel2: TPanel;
    edt_SymbolFilter: TEdit;
    lst_DissambledFunctions: TListView;
    Splitter2: TSplitter;
    procedure actOpenExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure About1Click(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actCloseUpdate(Sender: TObject);
    procedure actSaveAsUpdate(Sender: TObject);
    procedure ExploreFileSystem1Click(Sender: TObject);
    procedure Copy2Click(Sender: TObject);
    procedure lst_DissambledFunctionsDblClick(Sender: TObject);
    procedure lst_DissambledFunctionsColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure edt_SymbolFilterChange(Sender: TObject);
  protected
    MyXBE: TXbe;
    MyRanges: TMemo;
    SectionsGrid: TStringGrid;
    FXBEFileName: string;
    LastSortedColumn: Integer;
    Ascending: Boolean;
    procedure CloseFile;
    procedure GridAddRow(const aStringGrid: TStringGrid; const aStrings: array of string);
    procedure HandleGridDrawCell(Sender: TObject; aCol, aRow: Integer; Rect: TRect; State: TGridDrawState);
    function NewGrid(const aFixedCols: Integer; const aTitles: array of string): TStringGrid;
    procedure ResizeGrid(const Grid: TStringGrid; const aHeightPercentage: Double);
    procedure SectionClick(Sender: TObject);
    procedure LibVersionClick(Sender: TObject);
    procedure OnDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer;  var CanSelect: Boolean);
    procedure UpdateSymbolListView;
    procedure GotoAddress(const aAddress: UIntPtr);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function OpenFile(const aFilePath: string): Boolean;
  end;

var
  FormXBEExplorer: TFormXBEExplorer;

implementation

uses uHexViewer; // THexViewer

type
  TStringsHelper = class(TStrings)
  public
    procedure Sort;
  end;

procedure TStringsHelper.Sort;
var
  TmpStrLst: TStringList;
begin
  TmpStrLst := TStringList.Create;
  try
    TmpStrLst.AddStrings(Self);
    TmpStrLst.Sort;
    Self.Assign(TmpStrLst);
  finally
    TmpStrLst.Free;
  end;
end;

type
  TWinControlHelper = class(TWinControl)
  public
    function FindChildControlClass(aClass: TClass): TControl;
  end;

function TWinControlHelper.FindChildControlClass(aClass: TClass): TControl;
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    Result := Controls[I];
    if Result.ClassType = aClass then
      Exit;

    if Result is TWinControl then
    begin
      Result := TWinControlHelper(Result).FindChildControlClass(aClass);
      if Assigned(Result) then
        Exit;
    end;
  end;

  Result := nil;
end;
{$R *.dfm}

constructor TFormXBEExplorer.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  // Start accepting WM_DROPFILES messages (see OnDropFiles) :
  DragAcceptFiles(Handle, True);

  CloseFile;
end;

destructor TFormXBEExplorer.Destroy;
begin
  CloseFile;

  inherited Destroy;
end;

procedure TFormXBEExplorer.edt_SymbolFilterChange(Sender: TObject);
begin
  UpdateSymbolListView();
end;

procedure TFormXBEExplorer.ExploreFileSystem1Click(Sender: TObject);
begin
  TfrmExploreFileSystem.Create(Self).ShowModal;
end;

procedure TFormXBEExplorer.actOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    OpenFile(OpenDialog.FileName);
end;

procedure TFormXBEExplorer.actCloseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(MyXBE);
end;

procedure TFormXBEExplorer.actCloseExecute(Sender: TObject);
begin
  CloseFile;
end;

procedure TFormXBEExplorer.actSaveAsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(PageControl)
    and Assigned(PageControl.ActivePage)
    and (PageControl.ActivePage.ControlCount > 0)
    and (PageControl.ActivePage.Controls[0] is TImage);
end;

procedure TFormXBEExplorer.actSaveAsExecute(Sender: TObject);
begin
  if PageControl.ActivePage.Controls[0] is TImage then
  begin
    SavePictureDialog.FileName := FixInvalidFilePath(
      GetReadableTitle(@(MyXBE.m_Certificate)) + '_' + PageControl.ActivePage.Caption
      ) + '.bmp';
    if SavePictureDialog.Execute then
    begin
      TImage(PageControl.ActivePage.Controls[0]).Picture.SaveToFile(SavePictureDialog.FileName);
      Exit;
    end;
  end;

  ShowMessage('Save cancelled');
end;

procedure TFormXBEExplorer.About1Click(Sender: TObject);
begin
  TaskMessageDlg('About ' + Application.Title,
    Application.Title + ' version ' + _XBE_EXPLORER_VERSION + ' © 2010, PatrickvL.  Released under GPL3.'#13#13 +
    Application.Title + ' is part of Dxbx - the Delphi Xbox1 emulator.'#13#13 +
    'Website : http://sourceforge.net/projects/dxbx/',
    mtInformation, [mbOK], 0);
end;

procedure TFormXBEExplorer.OnDropFiles(var Msg: TMessage);
var
  i: Integer;
  FileName: array [0..MAX_PATH] of Char;
begin
  // Scan for dropped XBE files in reverse, as Windows generally offers them
  // in reversed order (see http://www.techtricks.com/delphi/dropfiles.php) :
  for i := DragQueryFile(Msg.wParam, $FFFFFFFF, FileName, MAX_PATH) - 1 downto 0 do
  begin
    if  (DragQueryFile(Msg.wParam, i, FileName, MAX_PATH) > 0)
    and FileExists(FileName)
    and OpenFile(FileName) then
      // Accept only one file :
      Break;
  end;

  // Release memory
  DragFinish(Msg.wParam);
end;

procedure TFormXBEExplorer.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  PageControl.ActivePage := TTabSheet(Node.Data);
end;

procedure TFormXBEExplorer.SectionClick(Sender: TObject);
var
  Grid: TStringGrid;
  i: DWORD;
  Hdr: PXbeSectionHeader;
  RegionInfo: RRegionInfo;
begin
  if not (Sender is TStringGrid) then
    Exit;

  Grid := TStringGrid(Sender);
  i := Grid.Row - Grid.FixedRows;
  Hdr := @(MyXBE.m_SectionHeader[i]);
  if Assigned(Hdr) and (Hdr.dwSizeofRaw > 0) then
  begin
    i := (i * SizeOf(TXbeSectionHeader)) + MyXBE.m_Header.dwSectionHeadersAddr - MyXBE.m_Header.dwBaseAddr;

    Grid.Cells[2, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwFlags));
    Grid.Cells[3, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwVirtualAddr));
    Grid.Cells[4, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwVirtualSize));
    Grid.Cells[5, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwRawAddr));
    Grid.Cells[6, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwSizeofRaw));
    Grid.Cells[7, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwSectionNameAddr));
    Grid.Cells[8, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwSectionRefCount));
    Grid.Cells[9, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwHeadSharedRefCountAddr));
    Grid.Cells[10, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwTailSharedRefCountAddr));
    Grid.Cells[11, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeSectionHeader(nil).bzSectionDigest));

    RegionInfo.Buffer := @MyXBE.RawData[Hdr.dwRawAddr];
    RegionInfo.Size := Hdr.dwSizeofRaw;
    RegionInfo.FileOffset := Hdr.dwRawAddr;
    RegionInfo.VirtualAddres := Pointer(Hdr.dwVirtualAddr);
    RegionInfo.Name := 'section "' + Grid.Cells[0, Grid.Row] + '"';

    TSectionViewer(Grid.Tag).SetRegion(RegionInfo);
  end;
end; // SectionClick

procedure TFormXBEExplorer.StringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  g_SelectedText := TStringGrid(Sender).Cells[ACol, ARow];
end;

procedure TFormXBEExplorer.LibVersionClick(Sender: TObject);
var
  Grid: TStringGrid;
  i: DWord;
begin
  if not (Sender is TStringGrid) then
    Exit;

  Grid := TStringGrid(Sender);
  i := Grid.Row - Grid.FixedRows;
  i := (i * SizeOf(TXbeLibraryVersion)) + MyXBE.m_Header.dwLibraryVersionsAddr - MyXBE.m_Header.dwBaseAddr;

  Grid.Cells[1, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeLibraryVersion(nil).szName));
  Grid.Cells[2, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeLibraryVersion(nil).wMajorVersion));
  Grid.Cells[3, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeLibraryVersion(nil).wMinorVersion));
  Grid.Cells[4, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeLibraryVersion(nil).wBuildVersion));
  Grid.Cells[5, 2] :=  DWord2Str(i + FIELD_OFFSET(PXbeLibraryVersion(nil).dwFlags));
end;

function SortByColumn(Item1, Item2: TListItem; Data: Integer): Integer; stdcall;
begin
  if Abs(Data) = 1 then
    Result := AnsiCompareText(Item1.Caption, Item2.Caption)
  else
    Result := AnsiCompareText(Item1.SubItems[0], Item2.SubItems[0]);

  if Data < 0 then
    Result := -Result;
end;

procedure TFormXBEExplorer.lst_DissambledFunctionsColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if Column.Index = LastSortedColumn then
    Ascending := not Ascending
  else
    LastSortedColumn := Column.Index;

  if Ascending then
    // Sort 1 or 2 (for ascendig sort on column 0 or 1) :
    TListView(Sender).CustomSort(@SortByColumn, Column.Index + 1)
  else
    // Sort -1 or -2 (for descendig sort on column 0 or 1) :
    TListView(Sender).CustomSort(@SortByColumn, -(Column.Index + 1));
end;

procedure TFormXBEExplorer.GotoAddress(const aAddress: UIntPtr);
var
  i: Integer;
  Hdr: PXbeSectionHeader;
begin
  // Loop over all sections in the Xbe :
  for i := 0 to Length(MyXBE.m_SectionHeader) - 1 do
  begin
    // See if the given address falls inside this sections' range :
    Hdr := @(MyXBE.m_SectionHeader[i]);
    if (aAddress >= Hdr.dwVirtualAddr) and (aAddress < Hdr.dwVirtualAddr + Hdr.dwSizeofRaw) then
    begin
      // Switch the SectionGrid over to that section :
      SectionsGrid.Row := SectionsGrid.FixedRows + i;
      SectionClick(SectionsGrid);
      // Select the addess :
      if TSectionViewer(SectionsGrid.Tag).GotoAddress(aAddress) then
        SectionsGrid.BringToFront;

      Exit;
    end;
  end;
end;

procedure TFormXBEExplorer.lst_DissambledFunctionsDblClick(Sender: TObject);
begin
  if Assigned(lst_DissambledFunctions.Selected) then
    GotoAddress(UIntPtr(lst_DissambledFunctions.Selected.Data));
end;

procedure TFormXBEExplorer.UpdateSymbolListView;
var
  FilterText: string;
  i: Integer;
begin
  // Check if the SymbolList is available and update the visibility of elements likewise :
  Panel2.Visible := (SymbolList.Count > 0);
  Splitter2.Visible := Panel2.Visible;
  if not Panel2.Visible then
  begin
    TreeView1.Align := alClient;
    Exit;
  end;

  // Make some space and put the (filter-matching) elements in the listview :
  TreeView1.Align := alTop;
  TreeView1.Height := 300;
  FilterText := edt_SymbolFilter.Text;
  lst_DissambledFunctions.Items.BeginUpdate;
  try
    lst_DissambledFunctions.Clear;
    for i := 0 to SymbolList.Count - 1 do
      if (FilterText = '') or ContainsText(SymbolList.Strings[i], FilterText) then
        with lst_DissambledFunctions.Items.Add do
        begin
          Data := SymbolList.Objects[i]; // Address will be used in GOTO
          Caption := Format('%.08x', [UIntPtr(Data)]);
          SubItems.Add(SymbolList.Strings[i]);
        end;
  finally
    lst_DissambledFunctions.Items.EndUpdate;
  end;
end;

// LibVersionClick


procedure TFormXBEExplorer.HandleGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  Str: string;
begin
  // Determine if this is a TStringGrid draw event in a data-cell of a 'Meaning' column :
  if  (Sender is TStringGrid)
  and (aRow >= TStringGrid(Sender).FixedRows)
  and (aCol = TStringGrid(Sender).ColCount - 1)
  and (TStringGrid(Sender).Cells[aCol, 0] = 'Meaning') then
  begin
    // Redraw cell, but right-aligned :
    TStringGrid(Sender).Canvas.FillRect(Rect);
    Str := TStringGrid(Sender).Cells[aCol, aRow];
    DrawText(TStringGrid(Sender).Canvas.Handle, PChar(Str), Length(Str), Rect, DT_SINGLELINE or DT_VCENTER or DT_RIGHT);
  end;
end;

function TFormXBEExplorer.NewGrid(const aFixedCols: Integer; const aTitles: array of string): TStringGrid;
var
  i: Integer;
begin
  Result := TStringGrid.Create(Self);
  Result.OnDrawCell := HandleGridDrawCell;
  Result.OnSelectCell := StringGridSelectCell;
  Result.RowCount := 2;
  Result.DefaultRowHeight := Canvas.TextHeight('Wg') + 5;
  Result.ColCount := Length(aTitles);
  Result.FixedCols := aFixedCols;
  Result.Options := Result.Options + [goColSizing, goColMoving, goThumbTracking] - [goRowSizing, goRangeSelect];
  Result.ScrollBars := ssHorizontal;
  GridAddRow(Result, aTitles);
  for i := 0 to Length(aTitles) - 1 do
    Result.Cells[i, 0] := string(aTitles[i]);
end;

procedure TFormXBEExplorer.ResizeGrid(const Grid: TStringGrid; const aHeightPercentage: Double);
var
  i: Integer;
  NewHeight: Integer;
  MaxHeight: Integer;
begin
  // Calculate how heigh all rows together would be :
  NewHeight := GetSystemMetrics(SM_CYHSCROLL);
  for i := 0 to Grid.RowCount - 1 do
    Inc(NewHeight, Grid.RowHeights[i] + 1);
  NewHeight := 2 + NewHeight + 2;

  // Determine how heigh the parent is :
  MaxHeight := Grid.Parent.Height;
  // It that's not enough for 4 rows, switch to using the form height :
  if MaxHeight < 4*Grid.RowHeights[0] then
    MaxHeight := Self.Height;

  // See if the grid would exceed it's allotted height-percentage :
  MaxHeight := Trunc(MaxHeight * aHeightPercentage);
  if NewHeight > MaxHeight then
  begin
    // Reduce it's height and add the vertical scrollbar :
    NewHeight := MaxHeight;
    Grid.ScrollBars := ssBoth;
  end;

  // Set the newly determined height :
  Grid.Height := NewHeight;
end;

procedure TFormXBEExplorer.GridAddRow(const aStringGrid: TStringGrid; const aStrings: array of string);
var
  i, w: Integer;
  Row: TStrings;
begin
  i := 0;
  while (i <= aStringGrid.FixedRows) and (aStringGrid.Cells[0, i] <> '') do
    Inc(i);

  if i > aStringGrid.FixedRows then
  begin
    i := aStringGrid.RowCount;
    aStringGrid.RowCount := aStringGrid.RowCount + 1;
  end;

  Row := aStringGrid.Rows[i];
  for i := 0 to Length(aStrings) - 1 do
  begin
    Row[i] := aStrings[i];
    w := {aStringGrid.}Canvas.TextWidth(aStrings[i]) + 5;
    if aStringGrid.ColWidths[i] < w then
      aStringGrid.ColWidths[i] := w
  end;
end;

procedure TFormXBEExplorer.CloseFile;
begin
  FreeAndNil(MyXBE);
  SymbolList.Clear;
  TreeView1.Items.Clear;
  Caption := Application.Title;
  PageControl.Visible := False;
  while PageControl.PageCount > 0 do
    PageControl.Pages[0].Free;
  LastSortedColumn := -1;
  Ascending := True;
  edt_SymbolFilter.Text := '';
  UpdateSymbolListView();
end;

procedure TFormXBEExplorer.Copy2Click(Sender: TObject);
begin
  Clipboard.AsText := g_SelectedText;
end;

function TFormXBEExplorer.OpenFile(const aFilePath: string): Boolean;
var
  NodeResources: TTreeNode;
  RegionInfo: RRegionInfo;

  function _LoadSymbols: Boolean;
  var
    CacheFileName: string;
    SearchRec: TSearchRec;
  begin
    Result := False;

    SymbolList.Clear;
    CacheFileName := SymbolCacheFolder
            // TitleID
            + IntToHex(MyXBE.m_Certificate.dwTitleId, 8)
            // TODO : + CRC32 over XbeHeader :
            + '_*'
            // TitleName
            + '_' + GetReadableTitle(@MyXBE.m_Certificate)
            + SymbolCacheFileExt;
    if SysUtils.FindFirst(CacheFileName, faAnyFile, SearchRec) = 0 then
    begin
      LoadSymbolsFromCache(SymbolList, SymbolCacheFolder + SearchRec.Name);
      SysUtils.FindClose(SearchRec);
    end;

    Result := (SymbolList.Count > 0);
  end;

  procedure _AddRange(Start, Size: Integer; Title: string);
  begin
    MyRanges.Lines.Add(Format('%.08x .. %.08x  %-32s  (%9d bytes)', [Start, Start+Size-1, Title, Size]));
  end;

  function _Offset(var aValue; const aBaseAddr: DWord = 0): string;
  begin
    Result := DWord2Str(aBaseAddr + FIELD_OFFSET(aValue));
  end;

  function GetSectionNrByVA(const VA: DWord): Integer;
  var
    Hdr: PXbeSectionHeader;
  begin
    Result := Length(MyXBE.m_SectionHeader) - 1;
    while Result >= 0 do
    begin
      Hdr := @(MyXBE.m_SectionHeader[Result]);
      if Assigned(Hdr) and (Hdr.dwVirtualAddr <= VA) and (Hdr.dwVirtualAddr + Hdr.dwVirtualSize > VA) then
        Exit;

      Dec(Result);
    end;
  end;

  function GetSectionName(const aSectionNr: Integer): string;
  var
    Hdr: PXbeSectionHeader;
  begin
    Result := '';
    if (aSectionNr >= 0) and (aSectionNr < Length(MyXBE.m_SectionHeader)) then
    begin
      Hdr := @(MyXBE.m_SectionHeader[aSectionNr]);
      if Assigned(Hdr) then
        Result := MyXBE.GetAddrStr(Hdr.dwSectionNameAddr, XBE_SECTIONNAME_MAXLENGTH);
    end;
  end;

  function VA2RVA(const VA: DWord): DWord;
  var
    aSectionNr: Integer;
    Hdr: PXbeSectionHeader;
  begin
    aSectionNr := GetSectionNrByVA(VA);
    if (aSectionNr >= 0) and (aSectionNr < Length(MyXBE.m_SectionHeader)) then
    begin
      Hdr := @(MyXBE.m_SectionHeader[aSectionNr]);
      Result := Hdr.dwRawAddr + (VA - Hdr.dwVirtualAddr);
    end
    else
      Result := 0;
  end;

  function GetSectionNameByVA(const VA: DWord): string;
  var
    aSectionNr: Integer;
    Hdr: PXbeSectionHeader;
  begin
    Result := '';
    aSectionNr := GetSectionNrByVA(VA);
    if (aSectionNr >= 0) and (aSectionNr < Length(MyXBE.m_SectionHeader)) then
    begin
      Hdr := @(MyXBE.m_SectionHeader[aSectionNr]);
      if Assigned(Hdr) then
        Result := GetSectionName(aSectionNr) + Format(' + %x', [VA-Hdr.dwVirtualAddr]);
    end;
  end;

  function _CreateNode(const aParentNode: TTreeNode; const aName: string; aContents: TControl): TTreeNode;
  var
    TabSheet: TTabSheet;
  begin
    if aContents = nil then
    begin
      Result := nil;
      Exit;
    end;

    TabSheet := TTabSheet.Create(Self);
    TabSheet.PageControl := PageControl;
    TabSheet.TabVisible := False;
    TabSheet.Caption := aName;
    if Assigned(aContents) then
    begin
      aContents.Parent := TabSheet;
      aContents.Align := alClient;
    end
    else
      ; // TODO : Show 'Select subnode for more details' or something.

    Result := TreeView1.Items.AddChildObject(aParentNode, aName, TabSheet);
  end; // _CreateNode

  function _Initialize_XPRSection(const aXPR_IMAGE: PXPR_IMAGE): TImage;
  begin
    Result := TImage.Create(Self);
    Result.PopupMenu := pmImage;
    MyXbe.ExportXPRToBitmap(aXPR_IMAGE, Result.Picture.Bitmap);
  end;

  function _Initialize_IniSection(const aIni: WideString): TMemo;
  begin
    Result := TMemo.Create(Self);
    Result.SetTextBuf(PChar(string(aIni)));
  end;

  function _Initialize_Logo: TImage;
  begin
    Result := TImage.Create(Self);
    Result.PopupMenu := pmImage;
    MyXbe.ExportLogoBitmap(Result.Picture.Bitmap)
  end;

  function _CreateGrid_File: TStringGrid;
  begin
    Result := NewGrid(1, ['Property', 'Value']);
    GridAddRow(Result, ['File Name', aFilePath]);
    GridAddRow(Result, ['File Type', 'XBE File']);
    GridAddRow(Result, ['File Size', BytesToString(MyXBE.FileSize)]);
  end;

  function _Initialize_XBEHeader: TStringGrid;
  var
    Hdr: PXbeHeader;
  begin
    Hdr := @(MyXBE.m_Header);
    Result := NewGrid(3, ['Member', 'Type', 'Offset', 'Value', 'Meaning']);
    GridAddRow(Result, ['dwMagic', 'Char[4]', _offset(PXbeHeader(nil).dwMagic), string(Hdr.dwMagic)]);
    GridAddRow(Result, ['pbDigitalSignature', 'Byte[256]', _offset(PXbeHeader(nil).pbDigitalSignature), PByteToHexString(@Hdr.pbDigitalSignature[0], 16) + '...']);
    GridAddRow(Result, ['dwBaseAddr', 'Dword', _offset(PXbeHeader(nil).dwBaseAddr), DWord2Str(Hdr.dwBaseAddr)]);
    GridAddRow(Result, ['dwSizeofHeaders', 'Dword', _offset(PXbeHeader(nil).dwSizeofHeaders), DWord2Str(Hdr.dwSizeofHeaders), BytesToString(Hdr.dwSizeofHeaders)]);
    GridAddRow(Result, ['dwSizeofImage', 'Dword', _offset(PXbeHeader(nil).dwSizeofImage), DWord2Str(Hdr.dwSizeofImage), BytesToString(Hdr.dwSizeofImage)]);
    GridAddRow(Result, ['dwSizeofImageHeader', 'Dword', _offset(PXbeHeader(nil).dwSizeofImageHeader), DWord2Str(Hdr.dwSizeofImageHeader), BytesToString(Hdr.dwSizeofImageHeader)]);
    GridAddRow(Result, ['dwTimeDate', 'Dword', _offset(PXbeHeader(nil).dwTimeDate), DWord2Str(Hdr.dwTimeDate), BetterTime(Hdr.dwTimeDate)]);
    GridAddRow(Result, ['dwCertificateAddr', 'Dword', _offset(PXbeHeader(nil).dwCertificateAddr), DWord2Str(Hdr.dwCertificateAddr)]);
    GridAddRow(Result, ['dwSections', 'Dword', _offset(PXbeHeader(nil).dwSections), DWord2Str(Hdr.dwSections)]);
    GridAddRow(Result, ['dwSectionHeadersAddr', 'Dword', _offset(PXbeHeader(nil).dwSectionHeadersAddr), DWord2Str(Hdr.dwSectionHeadersAddr)]);
    GridAddRow(Result, ['dwInitFlags', 'Dword', _offset(PXbeHeader(nil).dwInitFlags), PByteToHexString(@Hdr.dwInitFlags[0], 4)]);
    GridAddRow(Result, ['dwEntryAddr', 'Dword', _offset(PXbeHeader(nil).dwEntryAddr), DWord2Str(Hdr.dwEntryAddr), Format('Retail: 0x%.8x, Debug: 0x%.8x', [Hdr.dwEntryAddr xor XOR_EP_Retail, Hdr.dwEntryAddr xor XOR_EP_DEBUG])]);
    GridAddRow(Result, ['dwTLSAddr', 'Dword', _offset(PXbeHeader(nil).dwTLSAddr), DWord2Str(Hdr.dwTLSAddr), GetSectionNameByVA(Hdr.dwTLSAddr)]);
    GridAddRow(Result, ['dwPeStackCommit', 'Dword', _offset(PXbeHeader(nil).dwPeStackCommit), DWord2Str(Hdr.dwPeStackCommit)]);
    GridAddRow(Result, ['dwPeHeapReserve', 'Dword', _offset(PXbeHeader(nil).dwPeHeapReserve), DWord2Str(Hdr.dwPeHeapReserve)]);
    GridAddRow(Result, ['dwPeHeapCommit', 'Dword', _offset(PXbeHeader(nil).dwPeHeapCommit), DWord2Str(Hdr.dwPeHeapCommit)]);
    GridAddRow(Result, ['dwPeBaseAddr', 'Dword', _offset(PXbeHeader(nil).dwPeBaseAddr), DWord2Str(Hdr.dwPeBaseAddr)]);
    GridAddRow(Result, ['dwPeSizeofImage', 'Dword', _offset(PXbeHeader(nil).dwPeSizeofImage), DWord2Str(Hdr.dwPeSizeofImage), BytesToString(Hdr.dwPeSizeofImage)]);
    GridAddRow(Result, ['dwPeChecksum', 'Dword', _offset(PXbeHeader(nil).dwPeChecksum), DWord2Str(Hdr.dwPeChecksum)]);
    GridAddRow(Result, ['dwPeTimeDate', 'Dword', _offset(PXbeHeader(nil).dwPeTimeDate), DWord2Str(Hdr.dwPeTimeDate), BetterTime(Hdr.dwPeTimeDate)]);
    GridAddRow(Result, ['dwDebugPathNameAddr', 'Dword', _offset(PXbeHeader(nil).dwDebugPathNameAddr), DWord2Str(Hdr.dwDebugPathNameAddr), MyXBE.GetAddrStr(Hdr.dwDebugPathNameAddr)]);
    GridAddRow(Result, ['dwDebugFileNameAddr', 'Dword', _offset(PXbeHeader(nil).dwDebugFileNameAddr), DWord2Str(Hdr.dwDebugFileNameAddr), MyXBE.GetAddrStr(Hdr.dwDebugFileNameAddr)]);
    GridAddRow(Result, ['dwDebugUnicodeFileNameAddr', 'Dword', _offset(PXbeHeader(nil).dwDebugUnicodeFileNameAddr), DWord2Str(Hdr.dwDebugUnicodeFileNameAddr), string(MyXBE.GetAddrWStr(Hdr.dwDebugUnicodeFileNameAddr, XBE_DebugUnicodeFileName_MAXLENGTH))]);
    GridAddRow(Result, ['dwKernelImageThunkAddr', 'Dword', _offset(PXbeHeader(nil).dwKernelImageThunkAddr), DWord2Str(Hdr.dwKernelImageThunkAddr), Format('Retail: 0x%.8x, Debug: 0x%.8x', [Hdr.dwKernelImageThunkAddr xor XOR_KT_RETAIL, Hdr.dwKernelImageThunkAddr xor XOR_KT_DEBUG])]);
    GridAddRow(Result, ['dwNonKernelImportDirAddr', 'Dword', _offset(PXbeHeader(nil).dwNonKernelImportDirAddr), DWord2Str(Hdr.dwNonKernelImportDirAddr)]);
    GridAddRow(Result, ['dwLibraryVersions', 'Dword', _offset(PXbeHeader(nil).dwLibraryVersions), DWord2Str(Hdr.dwLibraryVersions)]);
    GridAddRow(Result, ['dwLibraryVersionsAddr', 'Dword', _offset(PXbeHeader(nil).dwLibraryVersionsAddr), DWord2Str(Hdr.dwLibraryVersionsAddr)]);
    GridAddRow(Result, ['dwKernelLibraryVersionAddr', 'Dword', _offset(PXbeHeader(nil).dwKernelLibraryVersionAddr), DWord2Str(Hdr.dwKernelLibraryVersionAddr)]);
    GridAddRow(Result, ['dwXAPILibraryVersionAddr', 'Dword', _offset(PXbeHeader(nil).dwXAPILibraryVersionAddr), DWord2Str(Hdr.dwXAPILibraryVersionAddr)]);
    GridAddRow(Result, ['dwLogoBitmapAddr', 'Dword', _offset(PXbeHeader(nil).dwLogoBitmapAddr), DWord2Str(Hdr.dwLogoBitmapAddr)]);
    GridAddRow(Result, ['dwSizeofLogoBitmap', 'Dword', _offset(PXbeHeader(nil).dwSizeofLogoBitmap), DWord2Str(Hdr.dwSizeofLogoBitmap), BytesToString(Hdr.dwSizeofLogoBitmap)]);

    _AddRange(0, SizeOf(TXbeHeader), 'XBE Header');
    _AddRange(Hdr.dwCertificateAddr - Hdr.dwBaseAddr, SizeOf(TXbeCertificate), 'Certificate');
    _AddRange(Hdr.dwTLSAddr, SizeOf(TXbeTLS), 'TLS');
//    _AddRange(Hdr.dwPeBaseAddr - Hdr.dwBaseAddr, 0, 'PeBase');
    _AddRange(Hdr.dwDebugPathNameAddr - Hdr.dwBaseAddr, Length(MyXbe.GetAddrStr(Hdr.dwDebugPathNameAddr))+1, 'DebugPathName');
    _AddRange(Hdr.dwDebugUnicodeFileNameAddr - Hdr.dwBaseAddr, ByteLength(MyXbe.GetAddrWStr(Hdr.dwDebugUnicodeFileNameAddr))+2, 'DebugUnicodeFileName');
//    _AddRange(Hdr.dwKernelImageThunkAddr - Hdr.dwBaseAddr, 1, 'KernelImageThunk');
    if Hdr.dwNonKernelImportDirAddr > 0 then
      _AddRange(Hdr.dwNonKernelImportDirAddr - Hdr.dwBaseAddr, 1, 'NonKernelImportDir');
    _AddRange(Hdr.dwLogoBitmapAddr - Hdr.dwBaseAddr, Hdr.dwSizeofLogoBitmap, 'LogoBitmap');
  end; // _Initialize_XBEHeader

  function _Initialize_Certificate: TStringGrid;
  var
    o: DWord;
    i: Integer;
    Cert: PXbeCertificate;
  begin
    o := MyXBE.m_Header.dwCertificateAddr - MyXBE.m_Header.dwBaseAddr;
    Cert := @(MyXBE.m_Certificate);
    Result := NewGrid(3, ['Member', 'Type', 'Offset', 'Value', 'Meaning']);
    GridAddRow(Result, ['dwSize', 'Dword', _offset(PXbeCertificate(nil).dwSize, o), DWord2Str(Cert.dwSize), BytesToString(Cert.dwSize)]);
    GridAddRow(Result, ['dwTimeDate', 'Dword', _offset(PXbeCertificate(nil).dwTimeDate, o), DWord2Str(Cert.dwTimeDate), BetterTime(Cert.dwTimeDate)]);
    GridAddRow(Result, ['dwTitleId', 'Dword', _offset(PXbeCertificate(nil).dwTitleId, o), DWord2Str(Cert.dwTitleId)]);
    GridAddRow(Result, ['wszTitleName', 'WChar[40]', _offset(PXbeCertificate(nil).wszTitleName, o), PWideCharToString(@Cert.wszTitleName[0], 40)]);
    for i := Low(Cert.dwAlternateTitleId) to High(Cert.dwAlternateTitleId) do
      GridAddRow(Result, ['dwAlternateTitleId[' + IntToStr(i) + ']', 'Dword', _offset(PXbeCertificate(nil).dwAlternateTitleId[i], o), DWord2Str(Cert.dwAlternateTitleId[i])]);
    GridAddRow(Result, ['dwAllowedMedia', 'Dword', _offset(PXbeCertificate(nil).dwAllowedMedia, o), DWord2Str(Cert.dwAllowedMedia)]);
    GridAddRow(Result, ['dwGameRegion', 'Dword', _offset(PXbeCertificate(nil).dwGameRegion, o), DWord2Str(Cert.dwGameRegion), GameRegionToString(Cert.dwGameRegion)]);
    GridAddRow(Result, ['dwGameRatings', 'Dword', _offset(PXbeCertificate(nil).dwGameRatings, o), DWord2Str(Cert.dwGameRatings)]);
    GridAddRow(Result, ['dwDiskNumber', 'Dword', _offset(PXbeCertificate(nil).dwDiskNumber, o), DWord2Str(Cert.dwDiskNumber)]);
    GridAddRow(Result, ['dwVersion', 'Dword', _offset(PXbeCertificate(nil).dwVersion, o), DWord2Str(Cert.dwVersion)]);
    GridAddRow(Result, ['bzLanKey', 'Byte[16]', _offset(PXbeCertificate(nil).bzLanKey, o), PByteToHexString(@Cert.bzLanKey[0], 16)]);
    GridAddRow(Result, ['bzSignatureKey', 'Byte[16]', _offset(PXbeCertificate(nil).bzSignatureKey, o), PByteToHexString(@Cert.bzSignatureKey[0], 16)]);
    for i := Low(Cert.bzTitleAlternateSignatureKey) to High(Cert.bzTitleAlternateSignatureKey) do
      GridAddRow(Result, ['bzTitleAlternateSignatureKey[' + IntToStr(i) + ']', 'Byte[16]', _offset(PXbeCertificate(nil).bzTitleAlternateSignatureKey[i], o), PByteToHexString(@Cert.bzTitleAlternateSignatureKey[i][0], 16)]);
  end; // _Initialize_Certificate

  function _Initialize_SectionHeaders: TPanel;
  var
    i, o: Integer;
    Hdr: PXbeSectionHeader;
    ItemName: string;
    Splitter: TSplitter;
    SectionViewer: TSectionViewer;
  begin
    if Length(MyXBE.m_SectionHeader)  = 0 then
    begin
      Result := nil;
      Exit;
    end;

    Result := TPanel.Create(Self);

    SectionsGrid := NewGrid(2, [' ', 'Member:', 'Flags', 'Virtual Address', 'Virtual Size',
      'Raw Address', 'Raw Size', 'SectionNamAddr', 'dwSectionRefCount',
      'dwHeadSharedRefCountAddr', 'dwTailSharedRefCountAddr', 'bzSectionDigest']);
    SectionsGrid.Parent := Result;
    SectionsGrid.Align := alTop;
    SectionsGrid.RowCount := 4;
    SectionsGrid.Options := SectionsGrid.Options + [goRowSelect];
    SectionsGrid.FixedRows := 3;
    GridAddRow(SectionsGrid, [' ', 'Type:', 'Dword', 'Dword', 'Dword', 'Dword', 'Dword', 'Dword', 'Dword', 'Dword', 'Dword', 'Byte[20]']);
    GridAddRow(SectionsGrid, ['Section', 'Offset', '-Click row-']); // This is the offset-row

    o := MyXBE.m_Header.dwSectionHeadersAddr - MyXBE.m_Header.dwBaseAddr;
    for i := 0 to Length(MyXBE.m_SectionHeader) - 1 do
    begin
      Hdr := @(MyXBE.m_SectionHeader[i]);
      if Assigned(Hdr) then
      begin
        ItemName := GetSectionName(i);
        GridAddRow(SectionsGrid, [
          ItemName,
          DWord2Str(o),
          PByteToHexString(@Hdr.dwFlags[0], 4),
          DWord2Str(Hdr.dwVirtualAddr),
          DWord2Str(Hdr.dwVirtualSize),
          DWord2Str(Hdr.dwRawAddr),
          DWord2Str(Hdr.dwSizeofRaw),
          DWord2Str(Hdr.dwSectionNameAddr),
          DWord2Str(Hdr.dwSectionRefCount),
          DWord2Str(Hdr.dwHeadSharedRefCountAddr),
          DWord2Str(Hdr.dwTailSharedRefCountAddr),
          PByteToHexString(@Hdr.bzSectionDigest[0], 20)
        ]);

        // Add image tab when this section seems to contain a XPR resource :
        if Assigned(MyXBE.m_bzSection[i]) then
        begin
          if PXPR_IMAGE(MyXBE.m_bzSection[i]).hdr.Header.dwMagic = XPR_MAGIC_VALUE then
            _CreateNode(NodeResources, 'Image ' + ItemName,
              _Initialize_XPRSection(PXPR_IMAGE(MyXBE.m_bzSection[i])));

          // Add INI tab when this section seems to start with a BOM :
          if PWord(MyXBE.m_bzSection[i])^ = $FEFF then
            _CreateNode(NodeResources, 'INI ' + ItemName,
              _Initialize_IniSection(PWideCharToString(@MyXBE.m_bzSection[i][2], (Hdr.dwSizeofRaw div SizeOf(WideChar)) - 1)));
        end;

        _AddRange(o, SizeOf(TXbeSectionHeader), 'SectionHeader ' + ItemName);
        _AddRange(Hdr.dwRawAddr, Hdr.dwSizeofRaw, 'SectionContents ' + ItemName);
        _AddRange(Hdr.dwSectionNameAddr - MyXBE.m_Header.dwBaseAddr, Length(ItemName)+1, 'SectionName ' + ItemName);
      end
      else
        GridAddRow(SectionsGrid, ['!NIL!']);

      Inc(o, SizeOf(TXbeSectionHeader));
    end;

    // Resize SectionsGrid to fit into 40% of the height or less :
    ResizeGrid(SectionsGrid, 0.4);

    Splitter := TSplitter.Create(Self);
    Splitter.Parent := Result;
    Splitter.Align := alTop;
    Splitter.Top := SectionsGrid.Height;

    SectionViewer := TSectionViewer.Create(Self);
    SectionViewer.Parent := Result;
    SectionViewer.Align := alClient;

    SectionsGrid.Tag := Integer(SectionViewer);
    SectionsGrid.OnClick := SectionClick;
  end; // _Initialize_SectionHeaders

  function _Initialize_LibraryVersions: TStringGrid;
  var
    o: DWord;
    i: Integer;
    LibVer: PXbeLibraryVersion;
    ItemName: string;
  begin
    if Length(MyXBE.m_LibraryVersion) = 0 then
    begin
      Result := nil;
      Exit;
    end;

    Result := NewGrid(1, ['Member:', 'szName', 'wMajorVersion', 'wMinorVersion', 'wBuildVersion', 'dwFlags']);
    Result.RowCount := 4;
    Result.Options := Result.Options + [goRowSelect];
    Result.FixedRows := 3;
    Result.OnClick := LibVersionClick;
    GridAddRow(Result, ['Type:', 'Char[8]', 'Word', 'Word', 'Word', 'Byte[2]']);
    GridAddRow(Result, ['Offset', '-Click row-']); // This is the offset-row

    o := MyXBE.m_Header.dwLibraryVersionsAddr - MyXBE.m_Header.dwBaseAddr;
    for i := 0 to Length(MyXBE.m_LibraryVersion) - 1 do
    begin
      LibVer := @(MyXBE.m_LibraryVersion[i]);
      ItemName := string(PCharToString(PAnsiChar(@LibVer.szName[0]), 8));
      GridAddRow(Result, [
        DWord2Str(o),
        ItemName,
        DWord2Str(LibVer.wMajorVersion),
        DWord2Str(LibVer.wMinorVersion),
        IntToStr(LibVer.wBuildVersion),
        PByteToHexString(@LibVer.dwFlags[0], 2)
        ]);

      _AddRange(o, SizeOf(TXbeLibraryVersion), 'LibraryVersion ' + ItemName);
      Inc(o, SizeOf(LibVer^));
    end;
  end;

  function _Initialize_TLS: TStringGrid;
  var
    o: DWord;
    TLS: PXbeTls;
  begin
    if MyXBE.m_TLS = nil then
    begin
      Result := nil;
      Exit;
    end;

    o := VA2RVA(MyXBE.m_Header.dwTLSAddr);
    TLS := MyXBE.m_TLS;
    Result := NewGrid(3, ['Member', 'Type', 'Offset', 'Value', 'Meaning']);
    GridAddRow(Result, ['dwDataStartAddr', 'Dword', _offset(PXbeTls(nil).dwDataStartAddr, o), DWord2Str(TLS.dwDataStartAddr)]);
    GridAddRow(Result, ['dwDataEndAddr', 'Dword', _offset(PXbeTls(nil).dwDataEndAddr, o), DWord2Str(TLS.dwDataEndAddr)]);
    GridAddRow(Result, ['dwTLSIndexAddr', 'Dword', _offset(PXbeTls(nil).dwTLSIndexAddr, o), DWord2Str(TLS.dwTLSIndexAddr), GetSectionNameByVA(TLS.dwTLSIndexAddr)]);
    GridAddRow(Result, ['dwTLSCallbackAddr', 'Dword', _offset(PXbeTls(nil).dwTLSCallbackAddr, o), DWord2Str(TLS.dwTLSCallbackAddr), GetSectionNameByVA(TLS.dwTLSCallbackAddr)]);
    GridAddRow(Result, ['dwSizeofZeroFill', 'Dword', _offset(PXbeTls(nil).dwSizeofZeroFill, o), DWord2Str(TLS.dwSizeofZeroFill), BytesToString(TLS.dwSizeofZeroFill)]);
    GridAddRow(Result, ['dwCharacteristics', 'Dword', _offset(PXbeTls(nil).dwCharacteristics, o), DWord2Str(TLS.dwCharacteristics)]);

//    _AddRange(TLS.dwDataStartAddr, TLS.dwDataEndAddr - TLS.dwDataStartAddr + 1, 'DataStart');
  end;

  function _Initialize_HexViewer: THexViewer;
  var
    OrgVA: Pointer;
  begin
    Result := THexViewer.Create(Self);

    // Trick the HexViewer into thinking the VA = 0 :
    OrgVA := RegionInfo.VirtualAddres;
    RegionInfo.VirtualAddres := nil;

    // Signal the HexViewer and restore original VA :
    Result.SetRegion(RegionInfo);
    RegionInfo.VirtualAddres := OrgVA;
  end;

  function _Initialize_Strings: TStringsViewer;
  begin
    Result := TStringsViewer.Create(Self);
    Result.SetRegion(RegionInfo);
  end;

var
  Node0, Node1, NodeXBEHeader: TTreeNode;
  PreviousTreeNodeStr: string;
  i: Integer;
begin // OpenFile
  // Construct current tree path (excluding first node) :
  Node0 := TreeView1.Selected;
  PreviousTreeNodeStr := '';
  while Assigned(Node0) and Assigned(Node0.Parent) do
  begin
    PreviousTreeNodeStr := Node0.Text + #0 + PreviousTreeNodeStr;
    Node0 := Node0.Parent;
  end;

  CloseFile;
  MyXBE := TXbe.Create(aFilePath);
  FXBEFileName := ExtractFileName(aFilePath);

  Panel2.Visible := _LoadSymbols;
  Splitter2.Visible := Panel2.Visible;
  if Panel2.Visible then
    UpdateSymbolListView;

  Caption := Application.Title + ' - [' + FXBEFileName + ']';
  PageControl.Visible := True;

  MyRanges := TMemo.Create(Self);
  MyRanges.Parent := Self; // Temporarily set Parent, to allow adding lines already
  MyRanges.ScrollBars := ssBoth;
  MyRanges.Font.Name := 'Consolas';

  RegionInfo.Buffer := MyXBE.RawData;
  RegionInfo.Size := MyXBE.FileSize;
  RegionInfo.FileOffset := 0;
  RegionInfo.VirtualAddres := Pointer(MyXBE.m_Header.dwBaseAddr);
  RegionInfo.Name := 'whole file';

  Node0 := _CreateNode(nil, 'File : ' + FXBEFileName, _CreateGrid_File);
  NodeXBEHeader := _CreateNode(Node0, 'XBE Header', _Initialize_XBEHeader);

  NodeResources := _CreateNode(Node0, 'Resources', MyRanges);
  _CreateNode(NodeResources, 'Logo Bitmap', _Initialize_Logo);

  _CreateNode(NodeXBEHeader, 'Certificate', _Initialize_Certificate);
  _CreateNode(NodeXBEHeader, 'Section Headers', _Initialize_SectionHeaders); // Also adds image resource nodes
  _CreateNode(NodeXBEHeader, 'Library Versions', _Initialize_LibraryVersions);
  _CreateNode(NodeXBEHeader, 'TLS', _Initialize_TLS);

  _CreateNode(Node0, 'Contents', _Initialize_HexViewer);
  _CreateNode(Node0, 'Strings', _Initialize_Strings);

  TStringsHelper(MyRanges.Lines).Sort;
  MyRanges.Lines.Insert(0, Format('%-8s .. %-8s  %-32s  (%9s bytes)', ['Start', 'End', 'Range description', 'Size']));

  TreeView1.FullExpand;

  // Search for previously active node :
  while PreviousTreeNodeStr <> '' do
  begin
    Node1 := nil;
    for i := 0 to Node0.Count - 1 do
    begin
      if Node0.Item[i].Text = Copy(PreviousTreeNodeStr, 1, Length(Node0.Item[i].Text)) then
      begin
        Node1 := Node0.Item[i];
        Delete(PreviousTreeNodeStr, 1, Length(Node0.Item[i].Text) + 1);
        Break;
      end;
    end;

    if Node1 = nil then
      Break;

    Node0 := Node1;
  end;

  TreeView1.Selected := Node0;

  Result := True;
end; // OpenFile

end.

