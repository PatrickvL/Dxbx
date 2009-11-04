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
  Reinit in '..\..\src\Tools\XIso\src\Reinit.pas',
  ufrm_Main in '..\..\src\Tools\XIso\src\ufrm_Main.pas' {frm_Main},
  uxiso in '..\..\src\Tools\XIso\src\uxiso.pas',
  uxisomaker in '..\..\src\Tools\XIso\src\uxisomaker.pas',
  TextConsts in '..\..\src\Tools\XIso\src\TextConsts.pas',
  ufrm_Language in '..\..\src\Tools\XIso\src\ufrm_Language.pas' {frmLanguage},
  ufrmProgress in '..\..\src\Tools\XIso\src\ufrmProgress.pas' {frmProgress},
  GenerateXDFS in '..\..\src\Tools\XIso\src\GenerateXDFS.pas',
  Grabacion in '..\..\src\Tools\XIso\src\Grabacion.pas' {Form4},
  CreacionISO in '..\..\src\Tools\XIso\src\CreacionISO.pas',
  FormCreacionISO in '..\..\src\Tools\XIso\src\FormCreacionISO.pas' {Form5},
  ProgresoCreacionISO in '..\..\src\Tools\XIso\src\ProgresoCreacionISO.pas',
  xisomakerv3 in '..\..\src\Tools\XIso\src\xisomakerv3.pas',
  xisomakerv2 in '..\..\src\Tools\XIso\src\xisomakerv2.pas',
  Win32ASPI in '..\..\src\Tools\XIso\src\Win32ASPI.pas',
  CDROM in '..\..\src\Tools\XIso\src\CDROM.pas',
  xbe in '..\..\src\Tools\XIso\src\xbe.pas';

{$R *.res}

// Remove relocation table (generates smaller executables) :
// (See http://hallvards.blogspot.com/2006/09/hack12-create-smaller-exe-files.html)
{$SetPEFlags IMAGE_FILE_RELOCS_STRIPPED}

var
  Parameter, CarpetaParametro, ImagenParametro, S: string;
  i, j: Integer;
  MessagesEnabled: Boolean;
  Idioma: LANGID;
begin
  Idioma := GetUserDefaultLangID();
  if Word(Idioma and $000F) = LANG_SPANISH then
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
      OrigenDatos := OD_IMAGEN;
      if not AbrirXISO(ParamStr(i + 1)) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SImagenNoXBOX), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      NombreImagen := ParamStr(i + 1);

      CarpetaParametro := '';
      for j := 0 to ParamCount - 1 do
        if Copy(ParamStr(j), 1, 2) = '-f' then
          CarpetaParametro := ParamStr(j + 1);

      if CarpetaParametro = '' then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      if not DirectoryExists(CarpetaParametro) then
        ForceDirectories(CarpetaParametro);

      if not DirectoryExists(CarpetaParametro) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      ExtraerCD(0, 0, 0, 0, CarpetaParametro);
      if MessagesEnabled then
        MessageBox(Application.Handle, PChar(SFinExtraccion), PChar('xISO'), MB_ICONINFORMATION or MB_OK);

      Exit;
    end;

    if Parameter = '-m' then
    begin
      CarpetaParametro := '';
      for j := 0 to ParamCount - 1 do
        if Copy(ParamStr(j), 1, 2) = '-f' then
          CarpetaParametro := ParamStr(j + 1);

      if CarpetaParametro = '' then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      if not DirectoryExists(CarpetaParametro) then
        ForceDirectories(CarpetaParametro);

      if not DirectoryExists(CarpetaParametro) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      CrearImagen(CarpetaParametro, ParamStr(i + 1));
      // MessageBox(Application.Handle, PChar(rcFinCreacion), PChar('xISO'), MB_ICONINFORMATION or MB_OK);
      Exit;
    end;

    if Parameter = '-d' then
    begin
      CarpetaParametro := '';
      for j := 0 to ParamCount - 1 do
        if Copy(ParamStr(j), 1, 2) = '-f' then
          CarpetaParametro := ParamStr(j + 1);

      if CarpetaParametro = '' then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      if not DirectoryExists(CarpetaParametro) then
        ForceDirectories(CarpetaParametro);

      if not DirectoryExists(CarpetaParametro) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      ImagenParametro := ParamStr(i + 1);
      if ImagenParametro[Length(ImagenParametro)] = '\' then
        ImagenParametro[Length(ImagenParametro)] := ' ';

      ImagenParametro := ExtractFileName(ImagenParametro) + '.xiso';

      CrearImagen(CarpetaParametro, ImagenParametro);
      // MessageBox(Application.Handle, PChar(rcFinCreacion), PChar('xISO'), MB_ICONINFORMATION or MB_OK);
      Exit;
    end;

    if Parameter = '-x' then
    begin
      OrigenDatos := OD_IMAGEN;
      if not AbrirXISO(ParamStr(i + 1)) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SImagenNoXBOX), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      NombreImagen := ParamStr(i + 1);

      CarpetaParametro := '';
      for j := 0 to ParamCount - 1 do
        if Copy(ParamStr(j), 1, 2) = '-f' then
          CarpetaParametro := ParamStr(j + 1);

      if CarpetaParametro = '' then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;


      if lowercase(ExtractFileExt(ExtractFileName(NombreImagen))) = '.xiso' then
        S := Copy(NombreImagen, 1, Length(NombreImagen) - 4)
      else
        S := NombreImagen;

      CarpetaParametro := ExtractFilePath(CarpetaParametro) + Trim(ChangeFileExt(ExtractFileName(NombreImagen), ' ')) + '\';

      if not DirectoryExists(CarpetaParametro) then
        CreateDir(CarpetaParametro);

      if not DirectoryExists(CarpetaParametro) then
      begin
        if MessagesEnabled then
          MessageBox(Application.Handle, PChar(SCarpetaExtError), PChar('xISO'), MB_ICONWARNING or MB_OK);

        Exit;
      end;

      ExtraerCD(0, 0, 0, 0, CarpetaParametro);
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

