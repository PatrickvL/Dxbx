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
unit uStringsViewer;

interface

uses
  // Delphi
  Windows, Classes, SysUtils, Controls, Graphics, Forms, Grids,
  // Dxbx
  uTypes;

type
  TStringsViewer = class(TDrawGrid)
  protected
    MyStrings: TStringList;
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure SetRegion(const aMemory: Pointer; const aSize: Integer);
  end;

implementation

{ TStringsViewer }

constructor TStringsViewer.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  MyStrings := TStringList.Create;
  
  BevelEdges := [];
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BorderStyle := bsNone;
  Font.Name := 'Consolas';
  DefaultRowHeight := 16;
  ColCount := 4;
  ColWidths[0] := 48; // Nr.
  ColWidths[1] := 64; // Offset
  ColWidths[2] := 56; // Type
  ColWidths[3] := 64000; // Value
  FixedCols := 3;
  FixedRows := 1;
  Options := [goThumbTracking];
  OnDrawCell := DoDrawCell;

  SetRegion(nil, 0);
end;

destructor TStringsViewer.Destroy;
begin
  FreeAndNil(MyStrings);

  inherited Destroy;
end;

procedure TStringsViewer.SetRegion(const aMemory: Pointer; const aSize: Integer);
const
  // String should at least be this long :
  MIN_STRING_LENGTH = 4;
  // Up until this length, the following chars invalidate the string :
  SHORT_STRING_LENGTH = 6;
  SHORT_STRING_FORBIDDEN_CHARS: set of AnsiChar = ['$', '^', '`', ']', '<', ';'];

var
  i: Integer;
  StartOffset: Integer;

  procedure _HandleStringEnd;
  var
    Len: Integer;
    Str: string;
  begin
    if StartOffset < 0 then
      Exit;

    Len := i - StartOffset;
    if Len >= MIN_STRING_LENGTH then
    begin
      SetString(Str, PAnsiChar(aMemory)+StartOffset, Len);

      // Relatively short strings need some extra pruning :
      if Len <= SHORT_STRING_LENGTH then
      begin
        // Process the contents of this string :
        for Len := Length(Str) downto 1 do
        begin
          // Check if there are forbidden characters in this string :
          if Str[Len] in SHORT_STRING_FORBIDDEN_CHARS then
          begin
            Str := '';
            Break;
          end;
        end;
      end;

      if Str <> '' then
        MyStrings.AddObject(Str, TObject(StartOffset));
    end;

    StartOffset := -1;
  end;

begin
  MyStrings.Clear;
  if Assigned(aMemory) and (aSize >= MIN_STRING_LENGTH) then
  begin
    StartOffset := -1;
    i := 0;
    while i < aSize do
    begin
      // TODO : Here we should add Unicode detection too
      
      if IsPrintableAsciiChar(PAnsiChar(aMemory)[i]) then
      begin
        if StartOffset < 0 then
          StartOffset := i;
      end
      else
        _HandleStringEnd;

      Inc(i);
    end;

    _HandleStringEnd;
  end;

  // We must have at least one string :
  if MyStrings.Count = 0 then
    MyStrings.Add('- no strings found -');

  RowCount := FixedRows + MyStrings.Count;
  Repaint;
end;

procedure TStringsViewer.DoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  aStr: string;
begin
  aStr := '';

  // No need to check range upto FixedRowCount, as that's always 1 :
  if aRow < TopRow then
  begin
    case aCol of
      0: aStr := 'Nr.';
      1: aStr := 'Offset';
      2: aStr := 'Type';
      3: aStr := 'Value';
    end;
  end
  else
  begin
    if aRow <= MyStrings.Count then
      case aCol of
        0: aStr := IntToStr(aRow);
        1: aStr := Format('%.08x', [UIntPtr(MyStrings.Objects[aRow - 1])]);
        2: aStr := Format('Char[%d]', [Length(MyStrings[aRow - 1])]);
        3: aStr := MyStrings.Strings[aRow - 1];
      end;
  end;

  Canvas.FillRect(Rect);
  Canvas.TextOut(Rect.Left, Rect.Top, aStr);
end;

end.

