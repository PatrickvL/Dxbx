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
  Classes,
{$IFDEF DXBX_DEBUG}
  TypInfo,
{$ENDIF}
  // 3rd party
  JclPeImage,
  // Dxbx
  uTypes;

type
  // This type is used to indicate all Xbox library functions that we patch.
  TXboxLibraryPatch = type Cardinal;

const
  PatchPrefix = 'XTL_Emu';

  // This is an TXboxLibraryPatch indicating the 'unknown patch'-state :
  xlp_Unknown = TXboxLibraryPatch(0);

var
  AvailablePatches: TStringList;

  // This method creates a somewhat readable string for each patched method string.
  function XboxLibraryPatchToDisplayString(const aValue: string): string;

  // This method determines which patch corresponds with the supplied aFunctionName.
  function XboxFunctionNameToLibraryPatch(aFunctionName: string): TXboxLibraryPatch;

  // This method returns the function-name for a (to be) patched method (indicated by aValue).
  function XboxLibraryPatchToString(const aValue: TXboxLibraryPatch): string;

  // This method returns the actual patch function address for each patched method.
  function XboxLibraryPatchToPatch(const aValue: TXboxLibraryPatch): TCodePointer;

implementation

function XboxLibraryPatchToDisplayString(const aValue: string): string;
var
  i: Integer;
begin
  // Start out with the official pattern-name :
  Result := aValue;

  // Now remove all prefix non-letters :
  while (Result <> '') and (not (Result[1] in ['a'..'z','A'..'Z'])) do
    Delete(Result, 1, 1);

  // And remove everything from '@' onward :
  i := Pos('@', Result);
  if i > 0 then
    Delete(Result, i, MaxInt);

  Result := PatchPrefix + Result;
end;

function XboxFunctionNameToLibraryPatch(aFunctionName: string): TXboxLibraryPatch;
var
  Index: Integer;
begin
  aFunctionName := XboxLibraryPatchToDisplayString(aFunctionName);

  Index := AvailablePatches.IndexOf(aFunctionName);
  // If found, make sure that value 0 keeps the meaning 'xlp_Unknown' :
  if Index >= 0 then
    Result := Index + 1
  else
    Result := xlp_Unknown;
end;

function XboxLibraryPatchToString(const aValue: TXboxLibraryPatch): string;
begin
  if aValue > xlp_Unknown then
    Result := AvailablePatches.Names[aValue - 1]
  else
    Result := '';
end; // XboxLibraryPatchToString

function XboxLibraryPatchToPatch(const aValue: TXboxLibraryPatch): TCodePointer;
begin
  if aValue > xlp_Unknown then
    Result := TCodePointer(AvailablePatches.Objects[aValue - 1])
  else
    Result := nil;
end;

procedure DetermineAvailablePatches;
var
  i: Integer;
  ExportFunctionName: string;
begin
  AvailablePatches := TStringList.Create;

  with TJclPeImage.Create({ANoExceptions=}True) do
  try
    AttachLoadedModule(HInstance);
    for i := 0 to ExportList.Count - 1 do
      if not ExportList[i].IsExportedVariable then
      begin
        ExportFunctionName := ExportList[i].Name;
        // Is this exported function a patch (does it start with our prefix) ?
        if StrLIComp(PChar(ExportFunctionName), PatchPrefix, Length(PatchPrefix)) = 0 then
          AvailablePatches.AddObject(ExportFunctionName, ExportList[i].MappedAddress);
      end;
  finally
    Free;
  end;

  AvailablePatches.Sort;
end;

initialization

  DetermineAvailablePatches;

end.

