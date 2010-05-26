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
unit uError;

interface

{$INCLUDE Dxbx.inc}

uses
  // Delphi
  Windows,
  SysUtils,
  // Dxbx
  uTypes;

type
  // inherit from this class for handy error reporting capability
  Error = object
  // Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  public
    // return current error (zero if there is none)
    function GetError(): PChar;

    // is the current error fatal? (class is "dead" on fatal errors)
    function IsFatal(): _bool;

    // clear the current error (returns false if error was fatal)
    function ClearError(): _bool;
  protected
    // protected constructor so this class must be inherited from
    procedure Initialize; // was Error::Error

    // protected deconstructor
    procedure Finalize;

    // protected so only derived class may set an error
    procedure SetError(const x_szError: PChar; x_bFatal: _bool);
  private
    // current error information
    m_bFatal: _bool;
    m_szError: PChar;
  end; // size = 8 (as in Cxbx)

implementation

{ Error }

procedure Error.Initialize;
begin
  m_szError := nil;
  m_bFatal := False;
end;

procedure Error.Finalize;
begin
  StrDispose(m_szError);
end;

function Error.GetError(): PChar;
begin
  Result := m_szError;
end;

function Error.IsFatal: _bool;
begin
  Result := m_bFatal;
end;

procedure Error.SetError(const x_szError: PChar; x_bFatal: _bool);
begin
  if not Assigned(m_szError) then
    m_szError := StrAlloc(256);//{ m_szError = new char[256]; }

  strncpy(m_szError, x_szError, 255);

  if m_szError <> '' then
    m_szError := x_szError;

  m_bFatal := x_bFatal;
end;

// clear the current error (returns false if error was fatal)
function Error.ClearError(): _bool;
begin
  if m_bFatal then
  begin
    Result := False;
    Exit;
  end;

  StrDispose(m_szError);
  m_szError := nil;

  m_bFatal := False;

  Result := True;
end;

end.

