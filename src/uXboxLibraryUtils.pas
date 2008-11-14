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
unit uXboxLibraryUtils;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
{$IFDEF DXBX_DEBUG}
  TypInfo,
{$ENDIF}
  // Dxbx
  uTypes
{$IFDEF DXBX_PATCH_ADDRESSES_ENABLED}
  , uEmuXapi // reference all patch-implementations
{$ENDIF}
  ;

type
  // This type is used to indicate all Xbox library functions that we patch.
  TXboxLibraryPatch = type Cardinal;

const
  // This is an TXboxLibraryPatch indicating the 'unknown patch'-state :
  xlp_Unknown = TXboxLibraryPatch(0);

var
  // This is the highest valid TXboxLibraryPatch value;
  // it gets updated by CheckAllPatches :
  High_TXboxLibraryPatch: TXboxLibraryPatch = xlp_Unknown;

{$IFDEF DXBX_PATCH_ADDRESSES_ENABLED}
  // This method returns the actual patch function address for each patched method.
  function XboxLibraryPatchToPatch(const aValue: TXboxLibraryPatch): TCodePointer;
{$ENDIF ~DXBX_PATCH_ADDRESSES_ENABLED}

  // This method returns the function-name for a (to be) patched method (indicated by aValue).
  function XboxLibraryPatchToString(const aValue: TXboxLibraryPatch): string;

  // This method creates a somewhat readable string for each patched method.
  function XboxLibraryPatchToDisplayString(const aValue: TXboxLibraryPatch): string;

  // This method determines which patch corresponds with the supplied aFunctionName.
  function XboxFunctionNameToLibraryPatch(const aFunctionName: string): TXboxLibraryPatch;

  function IsXboxLibraryPatch(const aFunctionName: string): Boolean;

  function Sscanf(const s: string; const fmt: string; const Pointers: array of Pointer): Integer;


implementation

{$IFDEF DXBX_PATCH_ADDRESSES_ENABLED}
function XboxLibraryPatchToPatch(const aValue: TXboxLibraryPatch): TCodePointer;
begin
  case aValue of
    // Note, that because these case-statements are reused below,
    // we read them twice from an include-file to prevent errors :
    {$INCLUDE XboxLibraryPatchCases.inc}
  else
    Result := nil;
  end;
end;
{$ENDIF ~DXBX_PATCH_ADDRESSES_ENABLED}

const
  // DIRTY LITTLE HACK : By redefining the emulation function symbols here,
  // we can re-use the case-statement that maps a patch enum element to the
  // accompanying data (only, now those symbols are strings instead) :
  {$INCLUDE XboxLibraryPatchNames.inc}

function XboxLibraryPatchToString(const aValue: TXboxLibraryPatch): string;
type
  PString = ^string;

  // Note : Because the included code returns the address of the symbol,
  // we're forced indicate that for strings too, thus the PString type :
  function _GetPString: PString;
  begin
    case aValue of
      // Here's the second time we include the case-statements,
      // but this time, the symbols are re-defined to contain strings
      // (see "DIRTY LITTLE HACK" comments above) :
      {$INCLUDE XboxLibraryPatchCases.inc}
    else
      Result := nil;
    end;
  end; // _GetPString

var
  aStr: PString;
begin
  aStr := _GetPString;
  if Assigned(aStr) then
    // If the _GetPString resulted in an assigned string pointer,
    // read the string itself by following the indirection :
    Result := aStr^
  else
    Result := '';
end; // XboxLibraryPatchToString

function Sscanf(const s: string; const fmt: string; const Pointers: array of Pointer): Integer;
var
  i, j, n, m: integer; 
  s1: string; 
  L: LongInt; 
  X: Extended; 

  function GetInt: Integer; 
  begin 
    s1 := ''; 
    while (s[n] = ' ') and (Length(s) > n) do 
      inc(n); 
    while (s[n] in ['0'..'9', '+', '-']) 
      and (Length(s) >= n) do 
    begin 
      s1 := s1 + s[n]; 
      inc(n); 
    end; 
    Result := Length(s1); 
  end; 

  function GetFloat: Integer; 
  begin 
    s1 := ''; 
    while (s[n] = ' ') and (Length(s) > n) do 
      inc(n); 
    while (s[n] in ['0'..'9', '+', '-', '.', 'e', 'E']) 
      and (Length(s) >= n) do 
    begin 
      s1 := s1 + s[n]; 
      inc(n); 
    end; 
    Result := Length(s1); 
  end; 

  function GetString: Integer; 
  begin 
    s1 := ''; 
    while (s[n] = ' ') and (Length(s) > n) do 
      inc(n); 
    while (s[n] <> ' ') and (Length(s) >= n) do 
    begin 
      s1 := s1 + s[n]; 
      inc(n); 
    end; 
    Result := Length(s1); 
  end; 

  function ScanStr(c: Char): Boolean; 
  begin 
    while (s[n] <> c) and (Length(s) > n) do 
      inc(n); 
    inc(n); 

    if (n <= Length(s)) then 
      Result := True 
    else Result := False; 
  end; 

  function GetFmt: Integer; 
  begin 
    Result := -1; 

    while (TRUE) do 
    begin 
      while (fmt[m] = ' ') and (Length(fmt) > m) do 
        inc(m); 
      if (m >= Length(fmt)) then 
        break; 

      if (fmt[m] = '%') then 
      begin 
        inc(m); 
        case fmt[m] of 
          'd': Result := vtInteger; 
          'f': Result := vtExtended; 
          's': Result := vtString; 
        end; 
        inc(m); 
        break; 
      end; 

      if (ScanStr(fmt[m]) = False) then 
        break; 
      inc(m); 
    end; 
  end; 

begin 
  n := 1; 
  m := 1; 
  Result := 0; 

  for i := 0 to High(Pointers) do 
  begin 
    j := GetFmt; 

    case j of 
      vtInteger: 
        begin 
          if GetInt > 0 then 
          begin 
            L := StrToInt(s1); 
            Move(L, Pointers[i]^, SizeOf(LongInt)); 
            inc(Result); 
          end 
          else 
            break; 
        end; 

      vtExtended: 
        begin 
          if GetFloat > 0 then 
          begin 
            X := StrToFloat(s1); 
            Move(X, Pointers[i]^, SizeOf(Extended)); 
            inc(Result); 
          end 
          else 
            break; 
        end; 

      vtString: 
        begin 
          if GetString > 0 then 
          begin 
            Move(s1, Pointers[i]^, Length(s1) + 1); 
            inc(Result); 
          end 
          else break; 
        end; 
    else 
      break; 
    end; 
  end; 
end; 


function XboxLibraryPatchToDisplayString(const aValue: TXboxLibraryPatch): string;
var
  i: Integer;
begin
  // Start out with the official pattern-name :
  Result := XboxLibraryPatchToString(aValue);
  // Now remove all prefix non-letters :
  while (Result <> '') and (not (Result[1] in ['a'..'z','A'..'Z'])) do
    Delete(Result, 1, 1);
  // And remove everything from '@' onward :
  i := Pos('@', Result);
  if i > 0 then
    Delete(Result, i, MaxInt);
end;

function XboxFunctionNameToLibraryPatch(const aFunctionName: string): TXboxLibraryPatch;
begin
  Result := High_TXboxLibraryPatch;
  repeat
    if SameText(XboxLibraryPatchToString(Result), aFunctionName) then
      Exit;

    Dec(Result);
  until (Result = xlp_Unknown);
end;

function IsXboxLibraryPatch(const aFunctionName: string): Boolean;
begin
  Result := XboxFunctionNameToLibraryPatch(aFunctionName) <> xlp_Unknown;
end;

// Checks if the XboxLibraryPatchToPatch() and XboxLibraryPatchToString()
// functions behave correctly - this cannot be done at compile-time,
// so we'll settle for a runtime check instead (just as long as we have
// some sort of test for this, we'll be fine).
procedure CheckAllPatches;
var
  p: TXboxLibraryPatch;
  NrEmptySlots: Cardinal;
const
  TRESHHOLD = 5; // bork out after 5 empty slots
begin
  p := xlp_Unknown;
  NrEmptySlots := 0;

{$IFDEF DXBX_PATCH_ADDRESSES_ENABLED}
  if XboxLibraryPatchToPatch(p) <> nil then
    raise Exception.Create('XboxLibraryPatchToPatch should return nil for ''xlp_Unknown''!');
{$ENDIF}
  if XboxLibraryPatchToString(p) <> '' then
    raise Exception.Create('XboxLibraryPatchToString should return empty string for ''xlp_Unknown''!');

  repeat
    Inc(p);

{$IFDEF DXBX_PATCH_ADDRESSES_ENABLED}
    if XboxLibraryPatchToPatch(p) = nil then
      Inc(NrEmptySlots)
    else
{$ENDIF}

    if XboxLibraryPatchToString(p) = '' then
      Inc(NrEmptySlots)
    else
      // Note this TXboxLibraryPatch offset is the highest as of yet :
      High_TXboxLibraryPatch := p;

  until NrEmptySlots >= TRESHHOLD;

  // Detect if there where any empty slots skipped :
  if (p - TRESHHOLD) <> High_TXboxLibraryPatch then
    raise Exception.CreateFmt('XboxLibraryPatchCases.inc has at least %d missing slot(s)!', [High_TXboxLibraryPatch - (p - TRESHHOLD)]);
end;

initialization

  CheckAllPatches;

end.

