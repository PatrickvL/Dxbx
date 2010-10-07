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
unit uDisassembleViewer;

interface

uses
  // Delphi
  Windows, Classes, SysUtils, Controls, Graphics, ExtCtrls, Forms, Grids, Menus, Dialogs,
  // Dxbx
  uConsts,
  uTypes,
  uDxbxUtils,
  uDisassembleUtils,
  uViewerUtils;

type
  // This code is loosely based on http://cheatengine.org disassembly viewer code
  // http://ce.colddot.nl/browser/Cheat%20Engine%206/disassemblerviewunit.pas
  TDisassembleViewer = class(TPanel)
  private
    FTextMetric: TTextMetric;
    function GetAddress: UIntPtr;
    procedure SetAddress(Value: UIntPtr);
  protected
    MyHeading: TPanel;
    MyDrawGrid: TDrawGrid;
    FRegionInfo: RRegionInfo;
    MyDisassemble: RDisassemble;
    MyInstructionOffsets: TList;
    function GetLabelByVA(const aVirtualAddress: Pointer): string;
    procedure GridDrawCellEvent(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
//    procedure CreatePatternExecute(Sender: TObject);
    procedure GotoAddressExecute(Sender: TObject);
  public
    OnGetLabel: TGetLabelEvent;

    property Address: UIntPtr read GetAddress write SetAddress;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure SetRegion(const aRegionInfo: RRegionInfo);
  end;

  TDisassemblerDrawGrid = class(TDrawGrid)
  protected
    procedure Paint; override;
  end;

implementation

var
  CYBorder: Integer;
  CXBorder: Integer;

{ TDisassembleViewer }

constructor TDisassembleViewer.Create(Owner: TComponent);

  function _MenuItem(const aCaption: string; const aOnClick: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(PopupMenu);
    Result.Caption := aCaption;
    Result.OnClick := aOnClick;
  end;

begin
  inherited Create(Owner);

  MyInstructionOffsets := TList.Create;

  BevelOuter := bvNone;
  Parent := TWinControl(Owner);

  Canvas.Font.Name := 'Consolas';
  GetTextMetrics(Canvas.Handle, FTextMetric);

  CYBorder := GetSystemMetrics(SM_CYBORDER);
  CXBorder := GetSystemMetrics(SM_CXBORDER);

  MyHeading := TPanel.Create(Self);
  with MyHeading do
  begin
    Align := alTop;
    Parent := Self;
    Height := 2 * (CYBorder + FTextMetric.tmExternalLeading + FTextMetric.tmHeight + CYBorder);
    BevelOuter := bvNone;
  end;

  MyDrawGrid := TDisassemblerDrawGrid.Create(Self);
  with MyDrawGrid do
  begin
    Align := alClient;
    BorderStyle := bsNone; // Remove unnessecary borders
    ColCount := 4; // 'Addresss', 'Hexdump', 'Disassembly', 'Comments'
    FixedCols := 0;
    RowCount := 2;
    FixedRows := 1;
    GridLineWidth := 0; // Remove the lines between cells
    DefaultRowHeight := CYBorder + FTextMetric.tmExternalLeading + FTextMetric.tmHeight + CYBorder;
    DefaultColWidth := 1000;
    ColWidths[0] := 80;
    ColWidths[1] := 120;
    ColWidths[2] := 200;
    Options := [goRangeSelect, goColSizing, goRowSelect];
//    Color := FixedColor;
//    DrawingStyle := gdsClassic;
    ShowHint := True;
    DefaultDrawing := False;
//    DoubleBuffered := True;
    OnDrawCell := GridDrawCellEvent;

    Font.Name := 'Consolas';
    Parent := Self;
    // ParentFont := True; // Why doesn't this take the parent font?
  end;

  PopupMenu := TPopupMenu.Create(Self);
  with PopupMenu do
  begin
    Items.Add(_MenuItem('&Goto address...', GotoAddressExecute));
//    Items.Add(_MenuItem('Create pattern from here...', CreatePatternExecute));
  end;
end;

destructor TDisassembleViewer.Destroy;
begin
  FreeAndNil(MyInstructionOffsets);

  inherited Destroy;
end;

(*
procedure TDisassembleViewer.CreatePatternExecute(Sender: TObject);
begin
end;
*)

procedure TDisassembleViewer.GotoAddressExecute(Sender: TObject);
var
  NewAddress: Integer;
  AddressStr: string;
  GotoAddressStr: string;
begin
  AddressStr := DWord2Str(Address);
  GotoAddressStr := InputBox('&Goto address', 'Enter hexadecimal address', AddressStr);
  if GotoAddressStr = AddressStr then
    Exit;

  if ScanHexDWord(PChar(GotoAddressStr), {var}NewAddress) then
    Address := NewAddress;
end;

procedure TDisassembleViewer.SetAddress(Value: UIntPtr);

  function _AddressCompare(const aInstructionOffsets: TList; const Index: Integer; const SearchData: Pointer): Integer;
  begin
    Result := Integer(aInstructionOffsets[Index]) - Integer(SearchData);
  end;

var
  Index: Integer;
  GridRect: TGridRect;
begin
  if Value < UIntPtr(FRegionInfo.VirtualAddres) then
    Exit;

  Dec(Value, UIntPtr(FRegionInfo.VirtualAddres));
  if Value > FRegionInfo.Size then
    Exit;


  GenericBinarySearch(
    {List=}MyInstructionOffsets,
    MyInstructionOffsets.Count,
    {SearchData=}Pointer(Value),
    @_AddressCompare,
    {out}Index);

  GridRect.Top := Index + MyDrawGrid.FixedRows;
  if GridRect.Top < MyDrawGrid.RowCount then
  begin
    GridRect.Left := MyDrawGrid.FixedCols;
    GridRect.Right := GridRect.Left;
    GridRect.Bottom := GridRect.Top;
    MyDrawGrid.Selection := GridRect;
    MyDrawGrid.TopRow := MyDrawGrid.Selection.Top;
  end;
end;

function TDisassembleViewer.GetAddress: UIntPtr;
var
  GridRect: TGridRect;
begin
  GridRect := MyDrawGrid.Selection;
  Result := GridRect.Top - 1;
  if Result >= UIntPtr(MyInstructionOffsets.Count) then
    Result := 0
  else
    Result := Cardinal(MyInstructionOffsets[Result ]);

  Result := UIntPtr(FRegionInfo.VirtualAddres) + Result;
end;

const
  ShowMangledNames = False; // Change this into a setting

function TDisassembleViewer.GetLabelByVA(const aVirtualAddress: Pointer): string;
var
  i: Integer;
begin
  Result := '';
  if SymbolList = nil then
    Exit;

  i := SymbolList.IndexOfObject(TObject(aVirtualAddress));
  if i < 0 then
    Exit;

  Result := SymbolList[i];
  if ShowMangledNames then
    Exit;

  Result := DxbxUnmangleSymbolName(Result);
end;

procedure TDisassembleViewer.SetRegion(const aRegionInfo: RRegionInfo);
begin
  FRegionInfo := aRegionInfo;

  MyDisassemble.Init(FRegionInfo.Buffer, FRegionInfo.Size, FRegionInfo.VirtualAddres);
  MyDisassemble.OnGetLabel := GetLabelByVA;

  MyHeading.Caption := Format('Disassemble viewing %s, %.08x .. %.08x (%s)', [
    FRegionInfo.Name,
    DWord(FRegionInfo.VirtualAddres),
    DWord(FRegionInfo.VirtualAddres) + FRegionInfo.Size,
    BytesToString(FRegionInfo.Size)]);

  // Scan whole region for opcodes offsets :
  MyInstructionOffsets.Clear;
  MyDisassemble.Offset := 0;
  while MyDisassemble.DoDisasm do
    MyInstructionOffsets.Add(Pointer(MyDisassemble.CurrentOffset));

  // Update the view on this data :
  MyDrawGrid.RowCount := 1 + MyInstructionOffsets.Count;
  MyDrawGrid.FixedRows := 1;
  MyDrawGrid.Invalidate;
end;

procedure TDisassembleViewer.GridDrawCellEvent(Sender: TObject; ACol, ARow: Longint;
  Rect: TRect; State: TGridDrawState);
var
  Offset: Cardinal;
  Address: UIntPtr;
  LineHeight: Integer;
  CommentStr: string;
  LineStr: string;
begin
  // Handle fixed cells (meaning: header-row) :
  LineStr := '';
  if gdFixed in State then
  begin
    Assert(aRow = 0);
    // Determine header text :
    case aCol of
      0: LineStr := 'Address';
      1: LineStr := 'Hexdump';
      2: LineStr := 'Disassembly';
      3: LineStr := 'Comment';
    end;

    // Clear background of header :
    MyDrawGrid.Canvas.Brush.Color := MyDrawGrid.FixedColor;
    MyDrawGrid.Canvas.FillRect(Rect);

    // Draw header text :
    if LineStr <> '' then
    begin
      MyDrawGrid.Canvas.Font.Color := clWindowText;
      MyDrawGrid.Canvas.Font.Style := [fsBold];
      MyDrawGrid.Canvas.TextRect(Rect, LineStr);
    end;

    // Done
    Exit;
  end;

  // Check end of data :
  if aRow > MyInstructionOffsets.Count then
  begin
    // Clear contents :
    MyDrawGrid.Canvas.Brush.Color := MyDrawGrid.Color;
    MyDrawGrid.Canvas.FillRect(Rect);
    Exit;
  end;

  // Update drawing colors, depending on state :
  if gdSelected in State then
  begin
    MyDrawGrid.Canvas.Brush.Color := clHighlight;
    MyDrawGrid.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    MyDrawGrid.Canvas.Brush.Color := MyDrawGrid.Color;
    MyDrawGrid.Canvas.Font.Color := clWindowText;
  end;

  // Retrieve the offset and address of this line :
  Offset := Cardinal(MyInstructionOffsets[aRow - 1]);
  Address := UIntPtr(FRegionInfo.VirtualAddres) + Offset;

  // Disassemble this (if not done already) :
  if MyDisassemble.CurrentOffset <> Offset then
  begin
    MyDisassemble.Offset := Offset;
    MyDisassemble.DoDisasm;

    // Double the LineHeight, if there's a label :
    if MyDisassemble.LabelStr <> '' then
      LineHeight := 2 * MyDrawGrid.DefaultRowHeight
    else
      LineHeight := MyDrawGrid.DefaultRowHeight;

    // Update this Row's height (only when needed) :
    if MyDrawGrid.RowHeights[aRow] <> LineHeight then
    begin
      MyDrawGrid.RowHeights[aRow] := LineHeight;
      MyDrawGrid.Invalidate;
      Exit;
    end;
  end;

  // Do we need to draw a label (we've already double the line-height for this) ?
  if MyDisassemble.LabelStr <> '' then
  begin
    // Append the label (only in the first column) :
    if aCol = 0 then
    begin
      // Extend this cell to a whole line :
      Inc(Rect.Right, ClientWidth);

      // Clear contents :
      MyDrawGrid.Canvas.FillRect(Rect);

      // Draw the label in bold :
      MyDrawGrid.Canvas.Font.Style := [fsBold];
      MyDrawGrid.Canvas.TextOut(Rect.Left + CXBorder, Rect.Top + CYBorder, MyDisassemble.LabelStr);

      // Deduce back to a single cell :
      Dec(Rect.Right, ClientWidth);
    end
    else
      ; // Don't clear the other columns (or the label would be gone)

    // Move the drawing rectangle to the next line :
    OffsetRect(Rect, 0, MyDrawGrid.DefaultRowHeight);
  end
  else
    // Single-line mode; Clear contents :
    MyDrawGrid.Canvas.FillRect(Rect);

  // Determine cell text :
  case aCol of
    0: LineStr := Format('%.08x', [Address]);
    1: LineStr := MyDisassemble.HexStr;
    2: LineStr := MyDisassemble.OpcodeStr;
    3:
    begin
      // Add interesting details, like referenced string contents, labels etc.

      // Only problem is, the referenced addresses in code assume post-load layout,
      // while we're working with a Raw Xbe - so we need to do a bit of addresss
      // conversion wizardry to make this work :
      CommentStr := '';
      if MyDisassemble.GetReferencedMemoryAddress({var}Address) then
      begin
        CommentStr := MyDisassemble.GetLabelStr(Pointer(Address));
        if  (CommentStr = '')
        // TODO : Check XBE Range better than this
        // Also, add support for strings referenced in another section :
        and (Address > UIntPtr(FRegionInfo.VirtualAddres))
        and (Address < UIntPtr(FRegionInfo.VirtualAddres) + FRegionInfo.Size) then
        begin
          Address := UIntPtr(FRegionInfo.Buffer) + Address - UIntPtr(FRegionInfo.VirtualAddres);
          CommentStr := TryReadLiteralString(PAnsiChar(Address));
        end;
      end;

      if CommentStr <> '' then
        CommentStr := '; ' + CommentStr;

      LineStr := CommentStr;
    end;
  end; // case

  // Draw text (if there is any) :
  if LineStr <> '' then
  begin
    // Shrink the drawing surface, by removing the border from all four sides :
    InflateRect(Rect, -CXBorder, -CYBorder);

    MyDrawGrid.Canvas.Font.Style := [];
    MyDrawGrid.Canvas.TextRect(Rect, LineStr);
  end;
end;

{ TDisassemblerDrawGrid }

// Copy of TCustomDrawGrid.Paint, with all needless cruft removed
// (DrawingStyle fixed on gdsClassic, no cell-lines, no HotTracking, etc.)
// What's left : scrolling (including focus), cell drawing, multi-select.
procedure TDisassemblerDrawGrid.Paint;
var
  DrawInfo: TGridDrawInfo;
  Sel: TGridRect;
  UpdateRect: TRect;

  // Copy from private TCustomGrid.IsActiveControl
  function IsActiveControl: Boolean;
  var
    H: Hwnd;
    ParentForm: TCustomForm;
  begin
    Result := False;
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) then
      Result := (ParentForm.ActiveControl = Self) and
        ((ParentForm = Screen.ActiveForm) or (ParentForm is TCustomActiveForm) or (ParentForm is TCustomDockForm))
    else
    begin
      H := GetFocus;
      while IsWindow(H) and not Result do
      begin
        if H = WindowHandle then
          Result := True
        else
          H := GetParent(H);
      end;
    end;
  end;

  // Copy from implementation-only Grids.PointInGridRect
  function PointInGridRect(Col, Row: Longint; const Rect: TGridRect): Boolean;
  begin
    Result := (Col >= Rect.Left) and (Col <= Rect.Right) and (Row >= Rect.Top)
      and (Row <= Rect.Bottom);
  end;

  procedure DrawCells(ACol, ARow: Longint; StartX, StartY, StopX, StopY: Integer;
    IncludeDrawState: TGridDrawState);
  var
    CurCol, CurRow: Longint;
    AWhere, Where: TRect;
    DrawState: TGridDrawState;
  begin
    CurRow := ARow;
    Where.Top := StartY;
    while (Where.Top < StopY) and (CurRow < RowCount) do
    begin
      CurCol := ACol;
      Where.Left := StartX;
      Where.Bottom := Where.Top + RowHeights[CurRow];
      while (Where.Left < StopX) and (CurCol < ColCount) do
      begin
        Where.Right := Where.Left + ColWidths[CurCol];
        if (Where.Right > Where.Left) and RectVisible(Canvas.Handle, Where) then
        begin
          DrawState := IncludeDrawState;
          if IsActiveControl and (CurRow = Row) and (CurCol = Col)  then
            SetCaretPos(Where.Left, Where.Top);
          if PointInGridRect(CurCol, CurRow, Sel) then
            Include(DrawState, gdSelected);
          AWhere := Where;
          DrawCell(CurCol, CurRow, AWhere, DrawState);
        end;
        Where.Left := Where.Right + DrawInfo.Horz.EffectiveLineWidth;
        Inc(CurCol);
      end;
      Where.Top := Where.Bottom + DrawInfo.Vert.EffectiveLineWidth;
      Inc(CurRow);
    end;
  end;

var
  FColOffset: Integer; // Can't access this private member, so we need to retrieve it otherwise:
begin
  FColOffset := GetScrollPos(Handle, SB_HORZ);

  UpdateRect := Canvas.ClipRect;
  CalcDrawInfo(DrawInfo);
  with DrawInfo do
  begin
    { Draw the cells in the four areas }
    Sel := Selection;
    // Fixed row
    DrawCells(
      LeftCol, 0,
      Horz.FixedBoundary - FColOffset, 0,
      Horz.GridBoundary, {!! clip} Vert.FixedBoundary,
      [gdFixed]);

    // Data
    DrawCells(
      LeftCol, TopRow,
      Horz.FixedBoundary - FColOffset, {!! clip} Vert.FixedBoundary,
      Horz.GridBoundary, Vert.GridBoundary,
      []);

    { Fill in area not occupied by cells }
    if Horz.GridBoundary < Horz.GridExtent then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect(Horz.GridBoundary, 0, Horz.GridExtent, Vert.GridBoundary));
    end;

    if Vert.GridBoundary < Vert.GridExtent then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect(0, Vert.GridBoundary, Horz.GridExtent, Vert.GridExtent));
    end;
  end;
end;

end.

