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
unit uTime;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows; // TTimeZoneInformation

type
  TCTime = Int64;

const
  CTimeZero = 25569; //EncodeDate(1970, 1, 1);
  SecPerDay = 60 * 60 * 24;

function CTimeToDateTime(CTime: TCTime): TDateTime;
function DateTimeToCTime(DateTime: TDateTime): TCTime;
function WideStringToString(const ws: WideString; codePage: Word): AnsiString;
function StringToWideString(const s: AnsiString; codePage: Word): WideString;
function Bias: Integer;

implementation

function Bias: Integer;
var
  TimeZone: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TimeZone);
  Result := TimeZone.Bias * 60;
end;

function CTimeToDateTime(CTime: TCTime): TDateTime;
begin
  Result := CTimeZero + (CTime - Bias) / SecPerDay;
end;

function DateTimeToCTime(DateTime: TDateTime): TCTime;
begin
  Result := Round((DateTime - CTimeZero) * SecPerDay + Bias);
end;

{:Converts Unicode string to Ansi string using specified code page.
  @param   ws       Unicode string.
  @param   codePage Code page to be used in conversion.
  @returns Converted ansi string.
}
function WideStringToString(const ws: WideString; codePage: Word): AnsiString;
var
  l: Integer;
begin
  if ws = '' then
    Result := ''
  else
  begin
    l := WideCharToMultiByte(codePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @ws[1], -1, nil, 0, nil, nil);
    SetLength(Result, l - 1);
    if l > 1 then
      WideCharToMultiByte(codePage,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @ws[1], -1, @Result[1], l - 1, nil, nil);
  end;
end; { WideStringToString }


{:Converts Ansi string to Unicode string using specified code page.
  @param   s        Ansi string.
  @param   codePage Code page to be used in conversion.
  @returns Converted wide string.
}
function StringToWideString(const s: AnsiString; codePage: Word): WideString;
var
  l: Integer;
begin
  if s = '' then
    Result := ''
  else
  begin
    l := MultiByteToWideChar(codePage, MB_PRECOMPOSED, PChar(@s[1]), -1, nil, 0);
    SetLength(Result, l - 1);
    if l > 1 then
      MultiByteToWideChar(CodePage, MB_PRECOMPOSED, PChar(@s[1]),
        -1, PWideChar(@Result[1]), l - 1);
  end;
end; { StringToWideString }


end.
