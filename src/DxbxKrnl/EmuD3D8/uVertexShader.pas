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

Uses
  Windows
  , uEmuD3D8Types;

function VshHandleIsVertexShader(aHandle: DWORD): boolean;
Function VshHandleGetVertexShader(aHandle : DWORD) : X_D3DVertexShader;


implementation



function VshHandleIsVertexShader(aHandle: DWORD): boolean;
begin
  result := (ahandle and $8000000) <> 0; 
end;

Function VshHandleGetVertexShader(aHandle : DWORD) : X_D3DVertexShader;
begin
(*  Result :=  aHandle and $7FFFFFFF; *)
{ return (X_D3DVertexShader *)(Handle & 0x7FFFFFFF); }
end;


end.
