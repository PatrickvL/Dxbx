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
    GameRegion: Integer;
    IsDuplicate: Boolean; // temporary

    property LibVersions: TStringList read MyLibVersions;

    constructor Create;
    destructor Destroy; override;

    function MatchesVersion(const aVersion: string): Boolean;
  end;

function GameRegionToString(const aGameRegion: Integer): string;

implementation

const
  XBEIMAGE_GAME_REGION_NTSC = $00000001;
  XBEIMAGE_GAME_REGION_JAPAN = $00000002;
  XBEIMAGE_GAME_REGION_RESTOFWORLD = $00000004;
  XBEIMAGE_GAME_REGION_MANUFACTURING = $80000000;

  XBEIMAGE_GAME_REGION_ALL = XBEIMAGE_GAME_REGION_NTSC + XBEIMAGE_GAME_REGION_JAPAN + XBEIMAGE_GAME_REGION_RESTOFWORLD;

function GameRegionToString(const aGameRegion: Integer): string;
begin
  if (aGameRegion and XBEIMAGE_GAME_REGION_ALL) = XBEIMAGE_GAME_REGION_ALL then
    Result := 'ALL'
  else
  begin
    Result := '';
    if (aGameRegion and XBEIMAGE_GAME_REGION_JAPAN) > 0 then
      Result := Result + ' JAP';

    if (aGameRegion and XBEIMAGE_GAME_REGION_NTSC) > 0 then
      Result := Result + ' NTSC';

    if (aGameRegion and XBEIMAGE_GAME_REGION_RESTOFWORLD) > 0 then
      Result := Result + ' PAL';
  end;

  if (aGameRegion and XBEIMAGE_GAME_REGION_MANUFACTURING) > 0 then
    Result := Result + ' DEBUG';

  Result := StringReplace(Trim(Result), ' ', '+', [rfReplaceAll]);
  if Result = '' then
  begin
    if aGameRegion = 0 then
      Result := 'UNKNOWN'
    else
      Result := 'REGION ' + IntToStr(aGameRegion);
  end;
end;

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

function TXBEInfo.MatchesVersion(const aVersion: string): Boolean;
var
  i: Integer;
begin
  for i := 0 to LibVersions.Count - 1 do
    if SameText(LibVersions.ValueFromIndex[i], aVersion) then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

end.
