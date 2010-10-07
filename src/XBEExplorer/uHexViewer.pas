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
  Windows, Classes, SysUtils, Controls, ExtCtrls, Forms, Grids, Dialogs, Menus,
  // Dxbx
  uTypes,
  uDxbxUtils,
  uViewerUtils;

type
  THexViewer = class(TPanel)
  private
    function GetOffset: DWord;
    procedure SetOffset(const Value: DWord);
  protected
    MyHeader: TPanel;
    MyDrawGrid: TDrawGrid;
    FRegionInfo: RRegionInfo;
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure GotoOffsetExecute(Sender: TObject);
  public
    property Offset: DWord read GetOffset write SetOffset;
    constructor Create(Owner: TComponent); override;

    procedure SetRegion(const aRegionInfo: RRegionInfo);
  end;

implementation

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
  OffsetStr := DWord2Str(Offset);
  OffsetStr := InputBox('Goto offset', 'Enter hexadecimal offset', OffsetStr);
  if ScanHexDWord(PChar(OffsetStr), {var}NewOffset) then
  begin
    Offset := NewOffset;
    if Offset = DWord(NewOffset) then
      Exit;
  end;

  ShowMessage('Goto failed');
end;

procedure THexViewer.SetOffset(const Value: DWord);
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

function THexViewer.GetOffset: DWord;
var
  GridRect: TGridRect;
begin
  GridRect := MyDrawGrid.Selection;
  Result := ((GridRect.Top - 1) * 16) + GridRect.Left - 1
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

procedure THexViewer.DoDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);

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
  aStr: string;
  Offset: DWord;
  Len: Integer;
begin
  if aRow = 0 then
  begin
    case aCol of
      0: aStr := ' Offset';
      1..16: aStr := ' ' + HexNibble[aCol];
      17: aStr := '     Ascii';
    end;
  end
  else
  begin
    case aCol of
      0:
        aStr := Format('%.08x', [(aRow - 1) * 16]);
      1..16:
        begin
          Offset := ((aRow - 1) * 16) + aCol - 1;
          if Offset >= FRegionInfo.Size then
            aStr := ''
          else
            aStr := PByteToHexString(@(PAnsiChar(FRegionInfo.Buffer)[Offset]), 1);
        end;
      17:
        begin
          Offset := ((aRow - 1) * 16);
          Len := 16;
          if Offset + DWord(Len) >= FRegionInfo.Size then
            Len := FRegionInfo.Size - Offset;

          aStr := _Ascii(Offset, Len);
        end;
    end;
  end;

  MyDrawGrid.Canvas.FillRect(Rect);
  MyDrawGrid.Canvas.TextOut(Rect.Left, Rect.Top, aStr);
end;

end.

