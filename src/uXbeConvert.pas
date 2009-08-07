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
unit uXbeConvert;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  Controls,
  SysUtils, // FileExists
  Dialogs,  // for MessageDlg
  // Dxbx
  uLog,
  uXbe,
  uEmuExe;

function ConvertToExe(x_FileName: string; x_bVerifyIfExists: Boolean; aXbe: TXbe; aHandle: THandle): Boolean;
function ConvertXbeToExe(aFileName, m_ExeFileName, m_XbeFileName: string; aXbe: TXbe; aHandle: THandle): Boolean;

implementation

function ConvertToExe(x_FileName: string; x_bVerifyIfExists: Boolean; aXbe: TXbe; aHandle: THandle): Boolean;
var
  i_EmuExe: TEmuExe;
begin
  Result := False;

  try
    if x_FileName <> '' then
    begin
      // ask permission to overwrite if file exists
      if x_bVerifyIfExists then
      begin
        if FileExists(x_FileName) then
        begin
          if MessageDlg('Overwrite existing file?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
            Exit;

          if not DeleteFile(x_FileName) then
            RaiseLastOSError;
        end;
      end;

      // convert file
      try
        i_EmuExe := TEmuExe.Create(aXbe, m_KrnlDebug, m_KrnlDebugFileName, aHandle);
        try
          if i_EmuExe.DoExport(x_FileName) then
          begin
            Result := True;
          end;
        finally
          FreeAndNil(i_EmuExe);
        end;
      except
        Result := False;
      end;

      if not Result then
        MessageDlg('Error converting to .exe', mtError, [mbOK], 0);
    end;
  Except
    raise exception.Create('ConvertToExe raises a exception');
  end;
end;

function ConvertXbeToExe(aFileName, m_ExeFileName, m_XbeFileName: string; aXbe: TXbe; aHandle: THandle): Boolean;
begin
  Result := False;
  try
    FreeAndNil(aXbe);
    if OpenXbe(aFileName, {var}aXbe, m_ExeFileName, m_XbeFileName) then
    begin
      ConvertToExe(ChangeFileExt(aFileName, '.exe'), {VerifyIfExists=}False, aXbe, aHandle);
      Result := True;
    end;
  Except
    raise exception.Create('ConvertXbeToExe raises a exception');
  end;
end;

end.

