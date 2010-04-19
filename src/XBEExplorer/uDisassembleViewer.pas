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
  Windows, Classes, SysUtils, Controls, Graphics, ExtCtrls, Forms, Grids,
  // 3rd Party
  BeaEngine,
  // Dxbx
  uTypes,
  uViewerUtils;

type
  TDisassemblerViewer = class(TPanel)
  private
    function GetOffset: DWord;
    procedure SetOffset(const Value: DWord);
  protected
    MyHeader: TPanel;
    MyListBox: TDrawGrid;
    FRegionInfo: RRegionInfo;
    MyDisAsm: TDISASM;
    MyInstructionOffsets: TList;
    function DoDisasm(const aOffset: Cardinal): Cardinal;
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
  public
    property Offset: DWord read GetOffset write SetOffset;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure SetRegion(const aRegionInfo: RRegionInfo);
  end;

implementation

const
  DelphiSyntax = $00000800;

{ TDisassemblerViewer }

constructor TDisassemblerViewer.Create(Owner: TComponent);
begin
  inherited Create(Owner);

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

  MyListBox := TDrawGrid.Create(Self);
  with MyListBox do
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
    ColCount := 5;
    ColWidths[0] := 64; // Offset
    ColWidths[1] := 28; // extra padding
    ColWidths[2] := 210; // Disassembly
    ColWidths[3] := 28; // extra padding
    ColWidths[4] := 96; // Hex dump
    FixedCols := 1;
    RowCount := 2; // Needed to be able to set FixedRows
    FixedRows := 1;
    Options := [goRangeSelect, goDrawFocusSelected, goThumbTracking];
    OnDrawCell := DoDrawCell;
  end;

  SetRegion(FRegionInfo);
end;

destructor TDisassemblerViewer.Destroy;
begin
  FreeAndNil(MyInstructionOffsets);

  inherited Destroy;
end;

procedure TDisassemblerViewer.SetOffset(const Value: DWord);
var
  GridRect: TGridRect;
begin
  GridRect.Top := Integer(Value div 16) + MyListBox.FixedRows;
  if GridRect.Top < MyListBox.RowCount then
  begin
    GridRect.Left := Integer(Value mod 16) + MyListBox.FixedCols;
    GridRect.Right := GridRect.Left;
    GridRect.Bottom := GridRect.Top;
    MyListBox.Selection := GridRect;
    MyListBox.TopRow := MyListBox.Selection.Top;
  end;
end;

function TDisassemblerViewer.GetOffset: DWord;
var
  GridRect: TGridRect;
begin
  GridRect := MyListBox.Selection;
  Result := ((GridRect.Top - 1) * 16) + GridRect.Left - 1
end;

function TDisassemblerViewer.DoDisasm(const aOffset: Cardinal): Cardinal;
begin
  ZeroMemory(@MyDisasm, SizeOf(MyDisAsm));
  MyDisasm.VirtualAddr := UIntPtr(FRegionInfo.VirtualAddres) + aOffset;
  MyDisasm.SecurityBlock := FRegionInfo.Size;
  MyDisasm.EIP := UIntPtr(FRegionInfo.Buffer) + aOffset;

  MyDisasm.Options := Tabulation + DelphiSyntax + PrefixedNumeral;
//  MasmSyntax, GoAsmSyntax, NasmSyntax, ATSyntax;

  Result := Disasm({var}MyDisasm);
end;

procedure TDisassemblerViewer.SetRegion(const aRegionInfo: RRegionInfo);
var
  Offset: Cardinal;
  Len: Integer;
begin
  FRegionInfo := aRegionInfo;

//  MyHeader.Caption := Format('Disassemble viewing %s, %.08x .. %.08x (%s)', [aTitle, DWord(FBase), DWord(FBase + FSize), BytesToString(FSize)]);

  MyInstructionOffsets.Clear;

  Offset := 0;
  while Offset < FRegionInfo.Size do
  begin
    Len := DoDisasm(Offset);
    if Len = OUT_OF_BLOCK then
      Break;

    if Len = UNKNOWN_OPCODE then
    begin
      // Mark invalid opcode :
      MyInstructionOffsets.Add(Pointer(-Offset));
      Inc(Offset, 1);
      Continue;
    end;

    MyInstructionOffsets.Add(Pointer(Offset));

    Inc(Offset, Len);
  end;

  MyListBox.RowCount := 2 + MyInstructionOffsets.Count;
  MyListBox.FixedRows := 1;
  MyListBox.Repaint;
end;

procedure TDisassemblerViewer.DoDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  aStr: string;
  Offset: Integer;
  Len: Integer;
begin
  if aRow = 0 then
  begin
    case aCol of
      0: aStr := 'VirtAddr';
      2: aStr := 'Disassembly';
      4: aStr := 'Hex dump';  // Ascii dump? Flag?
    end;
  end
  else
  if aRow >= MyInstructionOffsets.Count then
    aStr := ''
  else
  begin
    Offset := Integer(MyInstructionOffsets[aRow - 1]);
    case aCol of
      0:
        if Offset < 0 then
          aStr := Format('%.08x', [UIntPtr(FRegionInfo.VirtualAddres) - Offset])
        else
          aStr := Format('%.08x', [UIntPtr(FRegionInfo.VirtualAddres) + Offset]);
      2:
        begin
          if Offset < 0 then
            aStr := '???'
          else
          begin
            DoDisasm(Offset);
            aStr := string(MyDisAsm.CompleteInstr);
            if (MyDisAsm.Options and DelphiSyntax) = DelphiSyntax then
            begin
              // Transform the disassembled output into something more Delphi-like :
              aStr := StringReplace(aStr, '0x', '$', [rfIgnoreCase, rfReplaceAll]);
              aStr := StringReplace(aStr, ', dword ptr ', ', ', [rfIgnoreCase, rfReplaceAll]);
              aStr := StringReplace(aStr, ' , ', ',', [rfIgnoreCase, rfReplaceAll]);
            end;
          end;
        end;
      4:
        begin
//          Offset := ((aRow - 1) * 16) + aCol - 1;
//          if Offset >= FSize then
          aStr := '';
//          else
//            aStr := PByteToHexString(@PBytes(FRegionInfo.Buffer)[Offset], 1);
        end;
    end;
  end;

  MyListBox.Canvas.FillRect(Rect);
  MyListBox.Canvas.TextOut(Rect.Left, Rect.Top, aStr);
end;

end.

