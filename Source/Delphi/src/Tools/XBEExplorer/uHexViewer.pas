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
unit uHexViewer;

interface

uses
  // Delphi
  Windows, Types, Classes, SysUtils, Controls, ExtCtrls, Forms, Grids, Dialogs, Menus,
  // Dxbx
  uTypes,
  uDxbxUtils,
  uViewerUtils;

type
  THexViewer = class(TPanel)
  private
    function ColRowToOffset(const aCol, aRow: Integer): DWORD;
    function RegionByOffset(const aOffset: DWord): PByte;
    function RegionByColRow(const aCol, aRow: Integer): PByte;
    function GetSelectionOffset: DWord;
    procedure SetSelectionOffset(const Value: DWord);
  protected
    MyHeader: TPanel;
    MyDrawGrid: TDrawGrid;
    FRegionInfo: RRegionInfo;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure GotoOffsetExecute(Sender: TObject);

    function GetTextByCell(ACol, ARow: Integer): string;
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer;  var CanSelect: Boolean);

  public
    property SelectionOffset: DWord read GetSelectionOffset write SetSelectionOffset;
    constructor Create(Owner: TComponent); override;

    procedure SetRegion(const aRegionInfo: RRegionInfo);
  end;

  function DWordToHexString(const aValue: DWord): string;

implementation

function DWordToHexString(const aValue: DWord): string;
begin
  Result := Format('%.08x', [aValue]);
end;

procedure ChangeHint(C: TControl; const Hint: string; p: TPoint);
var
  OldHint: string;
begin
  OldHint := C.Hint;
  if Hint <> OldHint then
  begin
    C.Hint := Hint;
    Application.ActivateHint(p);
  end;
end;

{ THexViewer }

constructor THexViewer.Create(Owner: TComponent);

  function _MenuItem(const aCaption: string; const aOnClick: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(PopupMenu);
    Result.Caption := aCaption;
    Result.OnClick := aOnClick;
  end;

begin
  inherited Create(Owner);
  BevelOuter := bvNone;

  MyHeader := TPanel.Create(Self);
  with MyHeader do
  begin
    Align := alTop;
    Parent := Self;
    Height := 32;
    BevelOuter := bvNone;
  end;

  MyDrawGrid := TDrawGrid.Create(Self);
  with MyDrawGrid do
  begin
    Align := alClient;
    Parent := Self;
    BevelEdges := [];
    BevelInner := bvNone;
    BevelOuter := bvNone;
    BorderStyle := bsNone;
    Font.Name := 'Consolas';
    DefaultRowHeight := 16;
    DefaultColWidth := 20;
    ColCount := 18;
    ColWidths[0] := 64;
    ColWidths[16] := 28; // extra padding between $F and Ascii data
    ColWidths[17] := 96;
    FixedCols := 1;
    RowCount := 2; // Needed to be able to set FixedRows
    FixedRows := 1;
    Options := [goRangeSelect, goDrawFocusSelected, goThumbTracking];
    OnDrawCell := DoDrawCell;
    OnSelectCell := StringGridSelectCell;
    OnMouseMove := DoMouseMove;
    ShowHint := True;
  end;

  PopupMenu := TPopupMenu.Create(Self);
  with PopupMenu do
  begin
    Items.Add(_MenuItem('&Goto offset...', GotoOffsetExecute));
  end;

  SetRegion(FRegionInfo);
end;

procedure THexViewer.GotoOffsetExecute(Sender: TObject);
var
  NewOffset: Integer;
  OffsetStr: string;
begin
  OffsetStr := DWord2Str(SelectionOffset);
  OffsetStr := InputBox('Goto offset', 'Enter hexadecimal offset', OffsetStr);
  if ScanHexDWord(PChar(OffsetStr), {var}NewOffset) then
  begin
    SelectionOffset := NewOffset;
    if SelectionOffset = DWord(NewOffset) then
      Exit;
  end;

  ShowMessage('Goto failed');
end;

function THexViewer.ColRowToOffset(const aCol, aRow: Integer): DWORD;
begin
  if (aCol in [1..16]) and (aRow >= 1) then
    Result := ((aRow - 1) * 16) + aCol - 1
  else
    Result := High(Result); // Anything >= FRegionInfo.Size is out of range
end;

function THexViewer.RegionByOffset(const aOffset: DWord): PByte;
begin
  if aOffset >= FRegionInfo.Size then
    Result := nil
  else
    Result := @(PAnsiChar(FRegionInfo.Buffer)[aOffset]);
end;

function THexViewer.RegionByColRow(const aCol, aRow: Integer): PByte;
begin
  Result := RegionByOffset(ColRowToOffset(aCol, aRow));
end;

procedure THexViewer.SetSelectionOffset(const Value: DWord);
var
  GridRect: TGridRect;
begin
  GridRect.Top := Integer(Value div 16) + MyDrawGrid.FixedRows;
  if GridRect.Top < MyDrawGrid.RowCount then
  begin
    GridRect.Left := Integer(Value mod 16) + MyDrawGrid.FixedCols;
    GridRect.Right := GridRect.Left;
    GridRect.Bottom := GridRect.Top;
    MyDrawGrid.Selection := GridRect;
    MyDrawGrid.TopRow := MyDrawGrid.Selection.Top;
  end;
end;

function THexViewer.GetSelectionOffset: DWord;
var
  GridRect: TGridRect;
begin
  GridRect := MyDrawGrid.Selection;
  Result := ColRowToOffset(GridRect.Left, GridRect.Top);
end;

function THexViewer.GetTextByCell(ACol, ARow: Integer): string;

  function _AsByte: string;
  var
    Ptr: PByte;
  begin
    Ptr := RegionByColRow(aCol, aRow);
    if Ptr = nil then
      Result := ''
    else
      Result := PByteToHexString(Ptr, 1);
  end;

  function _Ascii(Offset, Len: Integer): string;
  var
    i: Integer;
  begin
    if Len <= 0 then
    begin
      Result := '';
      Exit;
    end;

    SetLength(Result, Len);
    Dec(Offset);
    for i := 1 to Len do
    begin
      if IsPrintableChar(PAnsiChar(FRegionInfo.Buffer)[Offset + i]) then
        Result[i] := Char(PAnsiChar(FRegionInfo.Buffer)[Offset + i])
      else
        Result[i] := '.';
    end;
  end;

var
  Offset: DWord;
  Len: Integer;
begin
  Result := '';
  if aRow = 0 then
  begin
    case aCol of
      0: Result := ' Offset';
      1..16: Result := ' ' + HexNibble[aCol];
      17: Result := '     Ascii';
    end;
  end
  else
  begin
    case aCol of
      0:
        Result := DWordToHexString((aRow - 1) * 16);
      1..16:
        begin
          Result := _AsByte;
        end;
      17:
        begin
          Offset := ((aRow - 1) * 16);
          Len := 16;
          if Offset + DWord(Len) >= FRegionInfo.Size then
            Len := FRegionInfo.Size - Offset;

          Result := _Ascii(Offset, Len);
        end;
    end;
  end;
end;

procedure THexViewer.SetRegion(const aRegionInfo: RRegionInfo);
var
  NrRows: Integer;
begin
  FRegionInfo := aRegionInfo;

  MyHeader.Caption := Format('Hex viewing %s, %.08x .. %.08x (%s)', [
    FRegionInfo.Name,
    DWord(FRegionInfo.VirtualAddres),
    DWord(FRegionInfo.VirtualAddres) + FRegionInfo.Size,
    BytesToString(FRegionInfo.Size)]);

  NrRows := (FRegionInfo.Size + 15) shr 4;

  // We must have at least one row, for FixedRows to stay:
  if NrRows = 0 then
    Inc(NrRows);

  MyDrawGrid.RowCount := 1 + NrRows;
  MyDrawGrid.Repaint;
end;

procedure THexViewer.StringGridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  g_SelectedText := GetTextByCell(ACol, ARow);
end;

procedure THexViewer.DoDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  aStr: string;
begin
  aStr := GetTextByCell(ACol, ARow);
  MyDrawGrid.Canvas.FillRect(Rect);
  MyDrawGrid.Canvas.TextOut(Rect.Left, Rect.Top, aStr);
end;

procedure THexViewer.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Cell: TGridCoord;
  Offset: DWord;
  HintText: string;
begin
  if MyDrawGrid = nil then
    Exit;

  Cell := MyDrawGrid.MouseCoord(X, Y);
  Offset := ColRowToOffset(Cell.X, Cell.Y);
  if Offset >= FRegionInfo.Size then
  begin
    Application.HideHint;
    Exit;
  end;

  HintText := FRegionInfo.Name + '+' + DWordToHexString(Offset);
  ChangeHint(MyDrawGrid, HintText, TControl(Sender).ClientToScreen(Point(X, Y)));
end;

end.

