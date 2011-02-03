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
unit u_About;

interface

uses
  // Delphi
  StdCtrls, ExtCtrls, Classes, Controls, Forms, jpeg,
  // Dxbx
  uConsts;


type
  Tfrm_About = class(TForm)
    Image1: TImage;
    Bevel1: TBevel;
    lbl_Version: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

var
  frm_About: Tfrm_About;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure Tfrm_About.FormCreate(Sender: TObject);
begin
  lbl_Version.Caption := lbl_Version.Caption + _XDK_TRACKER_VERSION;
end; // Tfrm_About.FormCreate

//------------------------------------------------------------------------------

end.
