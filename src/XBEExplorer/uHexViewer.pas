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
  DefaultColWidth := 20;
  DefaultRowHeight := 16;
  ColCount := 18;
  ColWidths[0] := 64;
  ColWidths[16] := 28; // extra padding between $F and Ascii data 
  ColWidths[17] := 96;
  FixedColor := clBtnHighlight;
  Options := [goThumbTracking];
  OnDrawCell := DoDrawCell;

  SetRegion(nil, 0);
end;

procedure THexViewer.SetRegion(const aBase: Pointer; const aSize: Integer);
begin
  FBase := aBase;
  FSize := aSize;
  RowCount := (aSize + 31) shr 4;
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
      Result[i] := Char(FBase[Offset + i]);
      if Ord(Result[i]) < Ord(' ') then
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

