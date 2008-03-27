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


unit progreso;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls;

type
  TForm3 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure FinCreacion(Sender: TObject);
  public
    FormPadre: TForm;
  end;

var
  Form3: TForm3;
  Cerrar: Boolean;
  Carpeta: string;

implementation

uses Textos, uxisomaker, GenerarXDFS, ufrm_Main, FormCreacionISO;

{$R *.dfm}

var
  Hilo: TGenerarXDFS;

procedure AvanzarProgreso(Fichero: string);
begin
  Form3.ProgressBar1.StepIt;
end;

procedure Mensajes(Textos: string);
begin
  Form3.Memo1.Lines.Add(Textos);
end;

procedure TForm3.FinCreacion(Sender: TObject);
begin
  ProgressBar1.Position := 0;
  Button1.Caption := rcEngCerrar;
  Cerrar := True;
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  if FormPadre = Form1 then
  begin
    GenerarXDFS.Imagen := SaveDialog1.FileName;
    GenerarXDFS.Carpeta := Carpeta;
    uxisomaker.Parar := False;
    ProgresoxISO := AvanzarProgreso;
    MensajesxISO := Mensajes;
    ProgressBar1.Position := 0;
    ProgressBar1.Min := 0;
    ProgressBar1.Max := NumeroFicheros(Carpeta);

    Cerrar := False;
    Hilo := TGenerarXDFS.Create(True);
    Hilo.OnTerminate := FinCreacion;
    Hilo.Resume;
    Hilo.FreeOnTerminate := True;
  end
  else
    if FormPadre = Form5 then
    begin

    end;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  if Button1.Caption = rcEngParar then
  begin
    uxisomaker.Parar := True;
    Button1.Caption := rcEngCerrar;
  end
  else
  begin
    if Cerrar then
      Close
    else
      Showmessage(rcEngEsperarHilo);
  end;
end;

end.
