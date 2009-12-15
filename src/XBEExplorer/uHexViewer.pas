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
  Windows, Classes, SysUtils, Controls, Graphics, Forms, Grids,
  // Dxbx
  uTypes;

type
  THexViewer = class(TDrawGrid)
  protected
    FBase: MathPtr;
    FSize: Integer;
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
  public
    constructor Create(Owner: TComponent); override;

    procedure SetRegion(const aBase: Pointer; const aSize: Integer);
  end;

implementation

constructor THexViewer.Create(Owner: TComponent);
begin
  inherited Create(Owner);

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
  Options := [goThumbTracking];
  OnDrawCell := DoDrawCell;

  SetRegion(nil, 0);
end;

procedure THexViewer.SetRegion(const aBase: Pointer; const aSize: Integer);
begin
  FBase := aBase;
  FSize := aSize;
  RowCount := 1 + ((aSize + 15) shr 4);
  Repaint;
end;

procedure THexViewer.DoDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);

  function _Ascii(Offset, Len: Integer): string;
  var
    i: Integer;
  begin
    SetLength(Result, Len);
    Dec(Offset);
    for i := 1 to Len do
    begin
      if IsPrintableChar(PAnsiChar(FBase)[Offset + i]) then
        Result[i] := Char(PAnsiChar(FBase)[Offset + i])
      else
        Result[i] := '.';
    end;
  end;

var
  aStr: string;
  Offset: Integer;
begin
  if aRow = TopRow then
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
          if Offset > FSize then
            aStr := ''
          else
            aStr := PByteToHexString(@FBase[Offset], 1);
        end;
      17:
        begin
          Offset := ((aRow - 1) * 16);
          if Offset + 16 > FSize then
            aStr := _Ascii(Offset, FSize - Offset + 1)
          else
            aStr := _Ascii(Offset, 16);
        end;
    end;
  end;

  if (aRow = TopRow) or (aCol = 0) then
    Canvas.Font.Color := clBackground
  else
    Canvas.Font.Color := clWindowText;

  Canvas.FillRect(Rect);
  Canvas.TextOut(Rect.Left, Rect.Top, aStr);
end;

end.

