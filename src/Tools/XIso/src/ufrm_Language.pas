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


unit ufrm_Language;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry, Reinit;

type
  TForm2 = class(TForm)
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

procedure SetLocalOverrides(FileName: string; LocaleOverride: string);

implementation

{$R *.dfm}

procedure SetLocalOverrides(FileName: string; LocaleOverride: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;
  try
    if Reg.OpenKey('Software\Borland\Locales', True) then
      Reg.WriteString(FileName, LocaleOverride);
  finally
    Reg.Free;
  end;
end;

procedure TForm2.RadioButton1Click(Sender: TObject);
begin
  SetLocalOverrides(ParamStr(0), 'esp');
  if LoadNewResourceModule(LANG_SPANISH) <> 0 then
    ReinitializeForms;
        //MessageBox(0,'Reinicia el programa, para que surtan efecto los cambios','Mensaje',MB_OK);
end;

procedure TForm2.RadioButton2Click(Sender: TObject);
begin
  SetLocalOverrides(ParamStr(0), 'enu');
  if LoadNewResourceModule(LANG_ENGLISH) <> 0 then
    ReinitializeForms;
        //MessageBox(0,'Reboot the program for translate it.','Mensaje',MB_OK);
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  Reg: TRegistry;
  s: string;
begin
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey('Software\Borland\Locales', True) then
      s := Reg.ReadString(ParamStr(0));
 {   if lowercase(s) = 'enu' then
    begin
       RadioButton1.Checked := False;
       RadioButton2.Checked := True;
    end
    else
    begin
       RadioButton1.Checked := True;
       RadioButton2.Checked := True;
    end;       }
  finally
    Reg.Free;
  end;
end;

end.
