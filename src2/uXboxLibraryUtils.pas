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
{$IFDEF DEBUG}
  TypInfo,
{$ENDIF}
  // 3rd party
  JclPeImage,
  // Dxbx
  uTypes,
  uDxbxUtils;

type
  // This type is used to indicate all Xbox library functions that we patch.
  TXboxLibraryPatch = type Cardinal;

const
  PatchPrefix = 'XTL_Emu';

  // This is an TXboxLibraryPatch indicating the 'unknown patch'-state :
  xlp_Unknown = TXboxLibraryPatch(0);

var
  AvailablePatches: TStringList;

  // This method determines which patch corresponds with the supplied aFunctionName.
  function XboxFunctionNameToLibraryPatch(const aMangledName, aUnmangledName: string): TXboxLibraryPatch;

  // This method returns the actual patch function address for each patched method.
  function XboxLibraryPatchToPatch(const aValue: TXboxLibraryPatch): TCodePointer;

procedure CleanupAvailablePatches;

implementation

function TransformExportFunctionNameIntoXboxFunctionName(const aValue: string): string;
begin
  Result := aValue;
  // Is this exported function a patch (does it start with our prefix) ?
  if StrLIComp(PChar(Result), PatchPrefix, Length(PatchPrefix)) = 0 then
    // Remove the prefix and return what's left after that, which is either
    // the (original) mangled name (when exported with a specific name
    // or the unmangled name (but having dots replaced by underscores) :
    Delete(Result, 1, Length(PatchPrefix))
  else
    // Any export without our specific prefix ('XTL_Emu') is not a patch, return an empty string :
    Result := '';
end;

function XboxFunctionNameToLibraryPatch(const aMangledName, aUnmangledName: string): TXboxLibraryPatch;
var
  Index: Integer;
  DemangledFunctionName: string;
begin
  // First option : Try to find exact string :
  if aUnmangledName <> '' then
  begin
    // Replace any dot with an underscore, as that's how we named our exports :
    DemangledFunctionName := StringReplace(aUnmangledName, '.', '_', [rfReplaceAll]);
    Index := AvailablePatches.IndexOf(DemangledFunctionName);
    if Index >= 0 then
    begin
      // If found, make sure that value 0 keeps the meaning 'xlp_Unknown' :
      Result := Index + 1;
      Exit;
    end;
  end;

  if aMangledName <> '' then
  begin
    Index := AvailablePatches.IndexOf(aMangledName);
    if Index >= 0 then
    begin
      // If found, make sure that value 0 keeps the meaning 'xlp_Unknown' :
      Result := Index + 1;
      Exit;
    end;
  end;

  Result := xlp_Unknown;
end;

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
        ExportFunctionName := TransformExportFunctionNameIntoXboxFunctionName(ExportList[i].Name);
        if ExportFunctionName <> '' then
          AvailablePatches.AddObject(ExportFunctionName, ExportList[i].MappedAddress);
      end;
  finally
    Free;
  end;

  AvailablePatches.Sort;
end;

procedure CleanupAvailablePatches;
begin
  FreeAndNil(AvailablePatches);
end;

initialization

  DetermineAvailablePatches;

finalization

  CleanupAvailablePatches;

end.

