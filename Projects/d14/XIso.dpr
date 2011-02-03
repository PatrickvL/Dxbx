{
   xISO
   Copyright 1984, 1986, 1989, 1992, 2000, 2001, 2002
   Free Software Foundation, Inc.

   This file is part of xISO, made it by Yursoft.com

   xISO is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Bison is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Bison; see the file COPYING.  If not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.
}


program XIso;

uses
  Forms,
  Windows,
  Messages,
  SysUtils,
  Reinit in '..\..\src2\Tools\XIso\src\Reinit.pas',
  ufrm_Main in '..\..\src2\Tools\XIso\src\ufrm_Main.pas' {frm_Main},
  uxiso in '..\..\src2\Tools\XIso\src\uxiso.pas',
  uxisomaker in '..\..\src2\Tools\XIso\src\uxisomaker.pas',
  TextConsts in '..\..\src2\Tools\XIso\src\TextConsts.pas',
  ufrm_Language in '..\..\src2\Tools\XIso\src\ufrm_Language.pas' {frmLanguage},
  ufrmProgress in '..\..\src2\Tools\XIso\src\ufrmProgress.pas' {frmProgress},
  GenerateXDFS in '..\..\src2\Tools\XIso\src\GenerateXDFS.pas',
  Grabacion in '..\..\src2\Tools\XIso\src\Grabacion.pas' {Form4},
  CreacionISO in '..\..\src2\Tools\XIso\src\CreacionISO.pas',
  FormCreacionISO in '..\..\src2\Tools\XIso\src\FormCreacionISO.pas' {frmCreateIso},
  ProgresoCreacionISO in '..\..\src2\Tools\XIso\src\ProgresoCreacionISO.pas',
  xisomakerv3 in '..\..\src2\Tools\XIso\src\xisomakerv3.pas',
  xisomakerv2 in '..\..\src2\Tools\XIso\src\xisomakerv2.pas',
  Win32ASPI in '..\..\src2\Tools\XIso\src\Win32ASPI.pas',
  CDROM in '..\..\src2\Tools\XIso\src\CDROM.pas';

{$R *.res}

// Remove relocation table (generates smaller executables) :
// (See http://hallvards.blogspot.com/2006/09/hack12-create-smaller-exe-files.html)
{$SetPEFlags IMAGE_FILE_RELOCS_STRIPPED}

var
  Parameter, FolderParameter, ImageParameter, S: string;
  i, j: Integer;
  MessagesEnabled: Boolean;
  Language: LANGID;
begin
  Language := GetUserDefaultLangID();
  if Word(Language and $000F) = LANG_SPANISH then
  begin
    // SetLocalOverrides(ParamStr(0),'esp');
    if LoadNewResourceModule(LANG_SPANISH) <> 0 then
      ReinitializeForms;
  end
  else
  begin
    // SetLocalOverrides(ParamStr(0),'enu');
    if LoadNewResourceModule(LANG_ENGLISH) <> 0 then
      ReinitializeForms;
  end;

  MessagesEnabled := True;
  for j := 0 to ParamCount - 1 do
    if Copy(ParamStr(j), 1, 2) = '-n' then
    begin
      MessagesEnabled := False;
      Break;
    end;

  for i := 0 to ParamCount - 1 do
  begin
    Parameter := Copy(ParamStr(i), 1, 2);

    if Parameter = '-e' then
    begin
      DataSource := OD_IMAGEN;
      if not OpenXISO(ParamStr(i + 1)) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SImagenNoXBOX), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      ImageName := ParamStr(i + 1);

      FolderParameter := '';
      for j := 0 to ParamCount - 1 do
        if Copy(ParamStr(j), 1, 2) = '-f' then
          FolderParameter := ParamStr(j + 1);

      if FolderParameter = '' then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      if not DirectoryExists(FolderParameter) then
        ForceDirectories(FolderParameter);

      if not DirectoryExists(FolderParameter) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      ExtractCD(0, 0, 0, 0, FolderParameter);
      if MessagesEnabled then
        MessageBox(Application.Handle, PChar(SFinExtraccion), PChar('xISO'), MB_ICONINFORMATION or MB_OK);

      Exit;
    end;

    if Parameter = '-m' then
    begin
      FolderParameter := '';
      for j := 0 to ParamCount - 1 do
        if Copy(ParamStr(j), 1, 2) = '-f' then
          FolderParameter := ParamStr(j + 1);

      if FolderParameter = '' then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      if not DirectoryExists(FolderParameter) then
        ForceDirectories(FolderParameter);

      if not DirectoryExists(FolderParameter) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      CreateImage(FolderParameter, ParamStr(i + 1));
      // MessageBox(Application.Handle, PChar(rcFinCreacion), PChar('xISO'), MB_ICONINFORMATION or MB_OK);
      Exit;
    end;

    if Parameter = '-d' then
    begin
      FolderParameter := '';
      for j := 0 to ParamCount - 1 do
        if Copy(ParamStr(j), 1, 2) = '-f' then
          FolderParameter := ParamStr(j + 1);

      if FolderParameter = '' then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      if not DirectoryExists(FolderParameter) then
        ForceDirectories(FolderParameter);

      if not DirectoryExists(FolderParameter) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      ImageParameter := ParamStr(i + 1);
      if ImageParameter[Length(ImageParameter)] = '\' then
        ImageParameter[Length(ImageParameter)] := ' ';

      ImageParameter := ExtractFileName(ImageParameter) + '.xiso';

      CreateImage(FolderParameter, ImageParameter);
      // MessageBox(Application.Handle, PChar(rcFinCreacion), PChar('xISO'), MB_ICONINFORMATION or MB_OK);
      Exit;
    end;

    if Parameter = '-x' then
    begin
      DataSource := OD_IMAGEN;
      if not OpenXISO(ParamStr(i + 1)) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SImagenNoXBOX), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      ImageName := ParamStr(i + 1);

      FolderParameter := '';
      for j := 0 to ParamCount - 1 do
        if Copy(ParamStr(j), 1, 2) = '-f' then
          FolderParameter := ParamStr(j + 1);

      if FolderParameter = '' then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;


      if lowercase(ExtractFileExt(ExtractFileName(ImageName))) = '.xiso' then
        S := Copy(ImageName, 1, Length(ImageName) - 4)
      else
        S := ImageName;

      FolderParameter := ExtractFilePath(FolderParameter) + Trim(ChangeFileExt(ExtractFileName(ImageName), ' ')) + '\';

      if not DirectoryExists(FolderParameter) then
        CreateDir(FolderParameter);

      if not DirectoryExists(FolderParameter) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      ExtractCD(0, 0, 0, 0, FolderParameter);
      if MessagesEnabled then
        MessageBox(Application.Handle, PChar(SFinExtraccion), PChar('xISO'), MB_ICONINFORMATION or MB_OK);

      Exit;
    end;
  end; // for ParamCount

  Application.Initialize;
  Application.Title := 'xISO 1.1.5';
  Application.CreateForm(Tfrm_Main, frm_Main);
  Application.CreateForm(TfrmLanguage, frmLanguage);
  Application.Run;
end.

