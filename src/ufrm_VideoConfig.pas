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
unit ufrm_VideoConfig;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  // DXBX
  uEmuShared,
  uXbVideo,
  // DirectX
  DirectDraw;

type
  Tfrm_VideoConfig = class(TForm)
    GroupBox1: TGroupBox;
    lbl_DisplayAdapter: TLabel;
    edt_DisplayAdapter: TComboBox;
    lbl_Direct3DDevice: TLabel;
    edt_Direct3dDevice: TComboBox;
    lbl_VideoResolution: TLabel;
    edt_VideoResolution: TComboBox;
    btn_Accept: TButton;
    btn_Cancel: TButton;
    lbl_OtherOptions: TLabel;
    chk_FullScreen: TCheckBox;
    chk_VSync: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboReadOnly(Sender: TObject; var Key: Char);
    procedure btn_AcceptClick(Sender: TObject);
  private
    FDirectDraw: IDirectDraw7;
    FXBVideo : XBVideo;
  end;

var
  frm_VideoConfig: Tfrm_VideoConfig;

implementation

{$R *.DFM}

function EnumDevices(lpGUID: PGUID; lpDriverDescription,
  lpDriverName: PChar; lpContext: Pointer; Monitor: HMonitor): Bool; stdcall;
begin
  TStringList(lpContext).Add(lpDriverDescription);
  Result := True;
end; // EnumDevices

function EnumModeusCallBack(const lpDDSurfaceDesc: TDDSurfaceDesc2;
  lpContext: Pointer): HResult; stdcall;
begin
  TStringList(lpContext).Add(IntToStr(lpDDSurfaceDesc.dwWidth) + ' X ' +
    IntToStr(lpDDSurfaceDesc.dwHeight) + ', ' +
    IntToStr(lpDDSurfaceDesc.ddpfPixelFormat.dwRGBBitCount) +
    ' bits/pixel');
  Result := DDENUMRET_OK;
end; // EnumModeusCallBack

procedure Tfrm_VideoConfig.FormCreate(Sender: TObject);
var
  tempDirectDraw: IDirectDraw;
begin
  // Load configuration from registry
  g_EmuShared.GetXBVideo(@FXBVideo);

  chk_FullScreen.Checked := FXBVideo.GetFullscreen;
  chk_VSync.Checked := FXBVideo.GetVSync;

  DirectDrawEnumerateEx(EnumDevices, edt_DisplayAdapter.Items, 0);
  edt_DisplayAdapter.ItemIndex := 0;

  DirectDrawCreate(nil, tempDirectDraw, FDirectDraw);
  try
    tempDirectDraw.QueryInterface(IID_IDIRECTDRAW7, FDirectDraw);
  finally
    tempDirectDraw := nil;
  end;
  FDirectDraw.EnumDisplayModes(0, nil, edt_VideoResolution.Items, EnumModeusCallBack);

  edt_VideoResolution.ItemIndex := edt_VideoResolution.Items.Count - 1;
end; // Tfrm_VideoConfig

procedure Tfrm_VideoConfig.btn_AcceptClick(Sender: TObject);
begin
  FXBVideo.SetFullscreen(chk_FullScreen.Checked);
  FXBVideo.SetVSync(chk_VSync.Checked);

  g_EmuShared.SetXBVideo(@FXBVideo);
end;

procedure Tfrm_VideoConfig.ComboReadOnly(Sender: TObject; var Key: Char);
begin
  Key := #0;
end; // Tfrm_VideoConfig.ComboReadOnly

end.
