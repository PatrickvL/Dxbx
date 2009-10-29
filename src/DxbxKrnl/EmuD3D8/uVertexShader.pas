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

unit uVertexShader;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows
  // Dxbx
  , uEmuD3D8Types;

function XTL_IsValidCurrentShader: Boolean; stdcall; // forward

function VshHandleIsVertexShader(aHandle: DWORD): Boolean;
function VshHandleGetVertexShader(aHandle: DWORD): PX_D3DVertexShader;

implementation

uses
  // Dxbx
  uEmuFS
  , uEmuD3D8;

// Checks for failed vertex shaders, and shaders that would need patching
function XTL_IsValidCurrentShader: Boolean; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  aHandle: DWORD;
  pVertexShader: PVERTEX_SHADER;
  pD3DVertexShader: PX_D3DVertexShader;
begin
  EmuSwapFS(fsWindows);
  XTL_EmuIDirect3DDevice8_GetVertexShader(@aHandle);
  EmuSwapFS(fsXbox);
  if (VshHandleIsVertexShader(aHandle)) then
  begin
    pD3DVertexShader := PX_D3DVertexShader(aHandle and $7FFFFFFF);
    pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);
    if (pVertexShader.Status <> 0) then
    begin
      Result := FALSE;
      Exit;
    end;
    (* Cxbx has this disabled :
    for i := 0 to pVertexShader.VertexDynamicPatch.NbrStreams - 1 do
    begin
      if (pVertexShader.VertexDynamicPatch.pStreamPatches[i].NeedPatch) then
      begin
       // Just for caching purposes
        pVertexShader.Status := $80000001;
        Result := FALSE;
        Exit;
      end;
    end;
    *)
  end;

  Result := True;
end;

function VshHandleIsVertexShader(aHandle: DWORD): Boolean;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  Result := (aHandle and $8000000) <> 0;
end;

function VshHandleGetVertexShader(aHandle: DWORD): PX_D3DVertexShader;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result :=  PX_D3DVertexShader(aHandle and $7FFFFFFF);
end;

exports
  XTL_IsValidCurrentShader;

end.

