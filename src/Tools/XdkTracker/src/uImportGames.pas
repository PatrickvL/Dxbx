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
unit uImportGames;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls, ExtCtrls;

type
  Tfrm_ImportGames = class(TForm)
    Bevel1: TBevel;
    btn_Cancel: TButton;
    btn_Ok: TButton;
    lbl_Publisher: TLabel;
    edt_Publisher: TEdit;
    lst_Import: TListView;
    lbl_NewGames: TLabel;
  end;

var
  frm_ImportGames: Tfrm_ImportGames;

implementation

{$R *.dfm}

end.
