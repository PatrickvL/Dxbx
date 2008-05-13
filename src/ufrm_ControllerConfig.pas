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
unit ufrm_ControllerConfig;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Tabs,
  // AlphaSkin
  sTabControl;

type
  Tfrm_ControllerConfig = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    btn_X: TButton;
    btn_Y: TButton;
    btn_A: TButton;
    btn_B: TButton;
    btn_White: TButton;
    btn_Black: TButton;
    btn_LeftTrigger: TButton;
    btn_RightTrigger: TButton;
    btn_LeftUp: TButton;
    btn_LeftDown: TButton;
    btn_LeftLeft: TButton;
    btn_LeftRight: TButton;
    btn_DPadUp: TButton;
    btn_DPadDown: TButton;
    btn_DPadLeft: TButton;
    btn_DPadRight: TButton;
    btn_Back: TButton;
    btn_Start: TButton;
    btnLeftThumb: TButton;
    btn_RightThumb: TButton;
    btn_RightUp: TButton;
    btn_RightDown: TButton;
    btn_RightLeft: TButton;
    btn_RightRight: TButton;
    btn_Accept: TButton;
    btn_Cancel: TButton;
    btn_LoadConfig: TButton;
    btn_SaveConfig: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    sTabControl1: TsTabControl;
    chkForceFeedback: TCheckBox;
  end;


var
  frm_ControllerConfig: Tfrm_ControllerConfig;

implementation

{$R *.DFM}



end.
