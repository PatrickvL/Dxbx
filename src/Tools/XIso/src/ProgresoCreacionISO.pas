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


unit ProgresoCreacionISO;

interface

uses
  Classes;

type
  TProgresoCreacionISO = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

var
  Estado: Boolean;

implementation

{ ProgresoCreacionISO }

uses FormCreacionISO, xisomakerv2;

procedure AvanzarProgreso(Fichero: string);
begin
  Form5.ProgressBar1.StepIt;
end;

procedure TProgresoCreacionISO.Execute;
var
  ManagerXISO: TAdminXISO;
begin
  Estado := False;
  if not Form5.SaveDialog1.Execute then Exit;
  ManagerXISO := TAdminXISO.Create(Form5.Manager);
  ManagerXISO.ProgresoxISO := AvanzarProgreso;
  ManagerXISO.CrearXISO(Form5.SaveDialog1.FileName);
  ManagerXISO.Free;
  Estado := True;
end;

end.
