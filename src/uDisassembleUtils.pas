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
unit uDisassembleUtils;

interface

uses
  // Delphi
  Windows, SysUtils,
  Classes, // TStringList
  // 3rd Party
  BeaEngine,
  // Dxbx
  uTypes;

type
  PARGTYPE = ^TARGTYPE; // DO NOT USE OUTSIDE THIS UNIT!

  TGetLabelEvent = function (const aVirtualAddress: Pointer): string of object;

  // Dxbx type wrapping the BeaEngine.TDISASM type, so we can easily switch
  // disassemblers if the need ever arises, by hiding the internals in this unit.
  RDisassemble = record
  private
    MyDisAsm: TDISASM;
    FOptions: Cardinal;
    FBuffer: Pointer;
    FSize: Cardinal;
    FVirtualAddress: Cardinal;
    FCurrentOffset: Cardinal;
    FLen: Integer;
    function GetArg(const aArgNr: Integer): PARGTYPE;
  public
    Offset: Cardinal;
    OnGetLabel: TGetLabelEvent;
    property Buffer: Pointer read FBuffer;
    property CurrentOffset: Cardinal read FCurrentOffset;

    procedure Init(const aBuffer: Pointer; const aSize: Cardinal; const aVirtualAddress: Pointer);

    function DoDisasm: Boolean;
    function LabelStr: string;
    function OpcodeStr: string;
    function HexStr: string;
    function IsOpcode(const aOpcode: Integer): Boolean;
    function GetLabelStr(const aVirtualAddr: Pointer): string;
    function GetReferencedMemoryAddress(var TargetAddress: Cardinal): Boolean;
//    function ArgReadsFromMemory(const aArgNr: Integer): Boolean;
//    function ArgMemoryAddress(const aArgNr: Integer): Cardinal;
  end;

var
  SymbolList: TStringList;

const
  DelphiSyntax = $00000080;

implementation

procedure RDisassemble.Init(const aBuffer: Pointer; const aSize: Cardinal; const aVirtualAddress: Pointer);
begin
  FBuffer := aBuffer;
  FSize := aSize;
  FVirtualAddress := Cardinal(aVirtualAddress);
  FOptions := Tabulation + DelphiSyntax + PrefixedNumeral;
//  MasmSyntax, GoAsmSyntax, NasmSyntax, ATSyntax;
end;

function RDisassemble.DoDisasm: Boolean;
begin
  if (FBuffer = nil)
  or (Offset >= FSize) then
  begin
    Result := False;
    Exit;
  end;

  FCurrentOffset := Offset;

  ZeroMemory(@MyDisasm, SizeOf(MyDisasm));
  MyDisasm.EIP := Cardinal(FBuffer) + FCurrentOffset;
  MyDisasm.VirtualAddr := FVirtualAddress + FCurrentOffset;
  MyDisasm.SecurityBlock := FSize - FCurrentOffset;
  MyDisasm.Options := FOptions;

  FLen := BeaEngine.Disasm({var}MyDisasm);
  Result := (FLen <> OUT_OF_BLOCK);

  if FLen <= UNKNOWN_OPCODE then
    Inc(Offset, 1)
  else
    Inc(Offset, FLen);
end;

function RDisassemble.GetLabelStr(const aVirtualAddr: Pointer): string;
begin
  if Assigned(OnGetLabel) then
    Result := OnGetLabel(aVirtualAddr)
  else
    Result := '';
end;

function RDisassemble.LabelStr: string;
begin
  if FLen <> OUT_OF_BLOCK then
    Result := GetLabelStr(Pointer(MyDisasm.VirtualAddr))
  else
    Result := '';
end;

function RDisassemble.OpcodeStr: string;
begin
  if FLen <= UNKNOWN_OPCODE then
  begin
    Result := '???';
    Exit;
  end;

  Result := string(MyDisAsm.CompleteInstr);
  if (MyDisAsm.Options and DelphiSyntax) = DelphiSyntax then
  begin
    // Transform the disassembled output into something more Delphi-like :
    Result := StringReplace(Result, '0x', '$', [rfIgnoreCase, rfReplaceAll]);
    Result := StringReplace(Result, ', dword ptr ', ', ', [rfIgnoreCase, rfReplaceAll]);
    Result := StringReplace(Result, ' , ', ',', [rfIgnoreCase, rfReplaceAll]);
  end;
end;

function RDisassemble.HexStr: string;
var
  OpcodeLen: Byte;
begin
  if MyDisAsm.Instruction.Opcode > $FFFF then
    if MyDisAsm.Instruction.Opcode > $FFFFFF then
      OpcodeLen := 4
    else
      OpcodeLen := 3
  else
    if MyDisAsm.Instruction.Opcode > $FF then
      OpcodeLen := 2
    else
      OpcodeLen := 1;

  // Split HexStr in operand+space+arguments :
  Result := PByteToHexString(@PBytes(FBuffer)[FCurrentOffset], OpcodeLen)
    + ' ' + PByteToHexString(@PBytes(FBuffer)[FCurrentOffset + OpcodeLen], FLen - OpcodeLen);
end;

function RDisassemble.IsOpcode(const aOpcode: Integer): Boolean;
begin
  Result := MyDisAsm.Instruction.Opcode = aOpcode;
end;

function RDisassemble.GetArg(const aArgNr: Integer): PARGTYPE;
begin
  case aArgNr of
    1: Result := @(MyDisAsm.Argument1);
    2: Result := @(MyDisAsm.Argument2);
    3: Result := @(MyDisAsm.Argument3);
  else
    Result := nil;
  end;
end;

function RDisassemble.GetReferencedMemoryAddress(var TargetAddress: Cardinal): Boolean;
begin
  Result := {((MyDisAsm.Instruction.Category and CONTROL_TRANSFER) > 0)
        and} (MyDisasm.Instruction.AddrValue > 0);
  if Result then
  begin
    {var}TargetAddress := MyDisasm.Instruction.AddrValue;
    Exit;
  end;

  Result := {((MyDisAsm.Instruction.Category and CONTROL_TRANSFER) > 0)
        and} (MyDisasm.Instruction.Immediat > 0);
  if Result then
  begin
    {var}TargetAddress := MyDisasm.Instruction.Immediat;
    Exit;
  end;

  with GetArg(2)^ do
    {var}TargetAddress := Memory.Displacement;
  Result := (TargetAddress > 0);
  if Result then
    Exit;

  with GetArg(1)^ do
    {var}TargetAddress := Memory.Displacement;
  Result := (TargetAddress > 0);
  if Result then
    Exit;
end;

//  if ((MyDisasm.Instruction.ImplicitModifiedRegs and REG4) > 0)
//  or (((MyDisasm.Argument1.ArgType and REG4) > 0) and (MyDisasm.Argument1.AccessMode = WRITE))
//  or (((MyDisasm.Argument2.ArgType and REG4) > 0) and (MyDisasm.Argument2.AccessMode = WRITE)) then


//function RDisassemble.ArgReadsFromMemory(const aArgNr: Integer): Boolean;
//begin
//  if (MyDisAsm.Instruction.Category and DATA_TRANSFER) = 0 then
//  begin
//    Result := False;
//    Exit;
//  end;
//
//  with GetArg(aArgNr)^ do
//    Result := (SegmentReg = DSReg)
//          and (Memory.BaseRegister = 0)
//          and (ArgType = MEMORY_TYPE)
//          and (AccessMode = READ)
////          and (Memory.Displacement >= FVirtualAddress)
////          and (Memory.Displacement < FVirtualAddress + FSize);
//end;
//
//function RDisassemble.ArgMemoryAddress(const aArgNr: Integer): Cardinal;
//begin
//  with GetArg(aArgNr)^ do
//    Result := Memory.Displacement;
//end;

initialization

  SymbolList := TStringList.Create;

finalization

  FreeAndNil(SymbolList);

end.

