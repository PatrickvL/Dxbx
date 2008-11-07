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

{$INCLUDE ..\..\Dxbx.inc}

interface

uses
  // Delphi
  Windows
  // Dxbx
  , uEmuD3D8Types;

function VshHandleIsVertexShader(aHandle: DWORD): boolean;
function VshHandleGetVertexShader(aHandle: DWORD): X_D3DVertexShader;
function XTL_IsValidCurrentShader: boolean;


implementation

uses
  uEmuFS
  , uEmuD3D8;

function XTL_IsValidCurrentShader: boolean;
var
  aHandle: DWORD;
  pVertexShader : VERTEX_SHADER;
  pD3DVertexShader : X_D3DVertexShader;
begin
  Result := TRUE;

  EmuSwapFS();
  XTL_EmuIDirect3DDevice8_GetVertexShader(aHandle);
  EmuSwapFS();
  if (VshHandleIsVertexShader(aHandle)) then begin
    (*pD3DVertexShader := (X_D3DVertexShader * )(Handle & 0 x7FFFFFFF);
    pVertexShader := (VERTEX_SHADER * )pD3DVertexShader - > Handle;
    if (pVertexShader.Status <> 0)begin
      Result := FALSE;
    end; *)
  end;
end;

function VshHandleIsVertexShader(aHandle: DWORD): boolean;
// Branch:martin  Revision:39  Translator:Shadow_Tj
begin
  result := (ahandle and $8000000) <> 0;
end;

function VshHandleGetVertexShader(aHandle: DWORD): X_D3DVertexShader;
// Branch:martin  Revision:39  Translator:Shadow_Tj
begin
(*  Result :=  aHandle and $7FFFFFFF; *)
{ return (X_D3DVertexShader *)(Handle & 0x7FFFFFFF); }
end;


end.

