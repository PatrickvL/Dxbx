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
unit uData;

{$INCLUDE ..\..\..\DXBX.inc}

interface

uses
  // Delphi
  SysUtils,
  Classes;

type
  TXBEInfo = class(TObject)
  protected
    MyLibVersions: TStringList;
  public
    DumpInfo: string; // first line of dump file, mentions dump-tool & version
    FileName: string;
    Title: string;

    property LibVersions: TStringList read MyLibVersions;

    constructor Create;
    destructor Destroy; override;

    function MatchesVersion(const aVersion: string): Boolean;

    function DetermineDisplayTitle: string;
  end;

implementation

{ TXBEInfo }

constructor TXBEInfo.Create;
begin
  inherited Create;

  MyLibVersions := TStringList.Create;
end;

destructor TXBEInfo.Destroy;
begin
  FreeAndNil(MyLibVersions);

  inherited Destroy;
end;

function TXBEInfo.DetermineDisplayTitle: string;
begin
  Result := Title;
  if Result = '' then
    Result := FileName;  
end;

function TXBEInfo.MatchesVersion(const aVersion: string): Boolean;
var
  i: Integer;
begin
  for i := 0 to LibVersions.Count - 1 do
    if LibVersions.ValueFromIndex[i] = aVersion then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

end.
