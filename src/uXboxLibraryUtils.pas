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
  XTL_EmuRtlAllocateHeap: string = '_RtlAllocateHeap@12';
  XTL_EmuRtlCreateHeap: string = '_RtlCreateHeap@24';
  XTL_EmuXapiApplyKernelPatches: string = '_XapiApplyKernelPatches@0';
  XTL_EmuXapiInitProcess: string = '_XapiInitProcess@0';
  XTL_EmuXapiThreadStartup: string = '_XapiThreadStartup@8';
  // NOTE: ^^^ New patches go above this line ^^^

type
  PString = ^string;

function XboxLibraryPatchToString(const aValue: TXboxLibraryPatch): string;

  function _GetPString: PString;
  begin
    case aValue of
      // Here's the second time we include the case-statements,
      // but this time, the symbols are defined to contain strings :
      {$INCLUDE XboxLibraryPatchCases.inc}
    else
      Result := nil;
    end;
  end;

var
  aStr: PString;
begin
  aStr := _GetPString;
  if Assigned(aStr) then
    Result := aStr^
  else
    Result := '';
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
      // Note this TXboxLibraryPatch offset as the highest of yet :
      High_TXboxLibraryPatch := p;

  until NrEmptySlots >= TRESHHOLD;

  // Detect if there where any empty slots skipped :
  if (p - TRESHHOLD) <> High_TXboxLibraryPatch then
    raise Exception.CreateFmt('XboxLibraryPatchCases.inc has at least %d missing slot(s)!', [High_TXboxLibraryPatch - (p - TRESHHOLD)]);
end;

initialization

  CheckAllPatches;

end.

