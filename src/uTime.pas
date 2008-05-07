unit uTime;

interface

uses
  // Delphi
  SysUtils, Windows;

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
