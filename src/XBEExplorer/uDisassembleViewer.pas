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
  Windows, Types, Classes, SysUtils, Controls, ComCtrls, Graphics, ExtCtrls, Forms, Grids,
  StdCtrls, // TListBox
  // Dxbx
  uTypes,
  uDxbxUtils,
  uDisassembleUtils,
  uViewerUtils;

type
  TDisassembleViewer = class(TPanel)
  private
    function GetOffset: DWord;
    procedure SetOffset(const Value: DWord);
  protected
    MyHeader: TPanel;
    MyListBox: TListBox;
    FRegionInfo: RRegionInfo;
    MyDisassemble: RDisassemble;
    MyInstructionOffsets: TList;
    procedure DoDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  public
    property Offset: DWord read GetOffset write SetOffset;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure SetRegion(const aRegionInfo: RRegionInfo);
  end;

implementation

{ TDisassembleViewer }

constructor TDisassembleViewer.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  Parent := TWinControl(Owner);

  MyInstructionOffsets := TList.Create;

  BevelOuter := bvNone;

  MyHeader := TPanel.Create(Self);
  with MyHeader do
  begin
    Align := alTop;
    Parent := Self;
    Height := 32;
    BevelOuter := bvNone;
  end;

  MyListBox := TListBox.Create(Self);
  with MyListBox do
  begin
    Align := alClient;
    Parent := Self;
    BevelEdges := [];
    BevelInner := bvNone;
    BevelOuter := bvNone;
    BorderStyle := bsNone;
    Font.Name := 'Consolas';
//    Columns := 1;
    Style := lbVirtualOwnerDraw;
    OnDrawItem := DoDrawItem;
    ScrollWidth := 0;//MaxInt;
  end;


  SetRegion(FRegionInfo);
end;

destructor TDisassembleViewer.Destroy;
begin
  FreeAndNil(MyInstructionOffsets);

  inherited Destroy;
end;

procedure TDisassembleViewer.SetOffset(const Value: DWord);
//var
//  GridRect: TGridRect;
begin
//  GridRect.Top := Integer(Value div 16) + MyDrawGrid.FixedRows;
//  if GridRect.Top < MyDrawGrid.RowCount then
//  begin
//    GridRect.Left := Integer(Value mod 16) + MyDrawGrid.FixedCols;
//    GridRect.Right := GridRect.Left;
//    GridRect.Bottom := GridRect.Top;
//    MyDrawGrid.Selection := GridRect;
//    MyDrawGrid.TopRow := MyDrawGrid.Selection.Top;
//  end;
end;

function TDisassembleViewer.GetOffset: DWord;
//var
//  GridRect: TGridRect;
begin
//  GridRect := MyDrawGrid.Selection;
//  Result := ((GridRect.Top - 1) * 16) + GridRect.Left - 1
  Result := 0;
end;

procedure TDisassembleViewer.SetRegion(const aRegionInfo: RRegionInfo);
begin
  FRegionInfo := aRegionInfo;

  MyDisassemble.Init(FRegionInfo.Buffer, FRegionInfo.Size, FRegionInfo.VirtualAddres);

  MyHeader.Caption := Format('Disassemble viewing %s, %.08x .. %.08x (%s)', [
    FRegionInfo.Name,
    DWord(FRegionInfo.VirtualAddres),
    DWord(FRegionInfo.VirtualAddres) + FRegionInfo.Size,
    BytesToString(FRegionInfo.Size)]);

  MyInstructionOffsets.Clear;

  MyDisassemble.Offset := 0;
  while MyDisassemble.DoDisasm do
    MyInstructionOffsets.Add(Pointer(MyDisassemble.CurrentOffset));

  MyListBox.Count := MyInstructionOffsets.Count;
  MyListBox.Repaint;
end;

procedure TDisassembleViewer.DoDrawItem(Control: TWinControl; Index: Integer;
    Rect: TRect; State: TOwnerDrawState);
var
  Offset: Cardinal;
//  Addr: Pointer;
  CommentStr: string;
  LineStr: string;
begin
  Offset := Cardinal(MyInstructionOffsets[Index]);

  MyDisassemble.Offset := Offset;
  MyDisassemble.DoDisasm;

  // Add interesting details, like referenced string contents, labels etc.

  // Only problem is, the referenced addresses in code assume post-load layout,
  // while we're working with a Raw Xbe - so we need to do a bit of addresss
  // conversion wizardry to make this work :
  CommentStr := '';
//  if MyDisassemble.ArgReadsFromMemory(2) then
//  begin
//    Addr := PAnsiChar(FRegionInfo.Buffer) + MyDisassemble.ArgMemoryAddress(2) - UIntPtr(FRegionInfo.VirtualAddres);
//    CommentStr := TryReadLiteralString(PAnsiChar(Addr));
//  end
//  else
//  if MyDisassemble.ArgReadsFromMemory(1) then
//  begin
//    Addr := PAnsiChar(FRegionInfo.Buffer) + MyDisassemble.ArgMemoryAddress(1) - UIntPtr(FRegionInfo.VirtualAddres);
//    CommentStr := TryReadLiteralString(PAnsiChar(Addr));
//  end;

  if CommentStr <> '' then
    CommentStr := '; ' + CommentStr;

  LineStr := Format('%.08x %-20s %-40s %s', [
    {Address}UIntPtr(FRegionInfo.VirtualAddres) + Offset,
    MyDisassemble.HexStr,
    MyDisassemble.OpcodeStr,
    CommentStr
    ]);

  MyListBox.Canvas.FillRect(Rect);
  DrawTextW(MyListBox.Canvas.Handle, LineStr, Length(LineStr), Rect, 0);
end;

end.

