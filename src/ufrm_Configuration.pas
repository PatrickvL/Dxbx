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
unit ufrm_Configuration;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, jpeg
  // DirectX
  , DirectDraw
  // Dxbx
  , ufrm_About // GetJPEGResource
  , uTypes
  , uXBController
  , uXbVideo
  , uEmuShared
  ;

// The Xbox controller image is borrowed from http://halo.wikia.com/wiki/Halo_Controls
// Some day, we should probably photograph or draw our own. Until then, this one suffices.
//
// Also, take a look at the following shots of the controller configuration of pcsx2 :
// http://charlii-5800.blog.friendster.com/2007/07/how-to-use-pcsx2-the-playstation-2-emulator/
// These pad settings allow more space per button, while we reserve space for just one character.
// It's okay for keyboard control, but joystick and mouse control needs space for a wider description.

type
  TfmConfiguration = class(TForm)
    TreeView1: TTreeView;
    btnOk: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    btn_X: TPanel;
    btn_Y: TPanel;
    Image1: TImage;
    btn_A: TPanel;
    btn_B: TPanel;
    btn_White: TPanel;
    btn_Black: TPanel;
    btn_LeftTrigger: TPanel;
    btn_RightTrigger: TPanel;
    btn_DPadUp: TPanel;
    btn_DPadDown: TPanel;
    btn_DPadRight: TPanel;
    btn_DPadLeft: TPanel;
    btn_Back: TPanel;
    btn_Start: TPanel;
    btn_LeftThumb: TPanel;
    btn_RightThumb: TPanel;
    btn_LeftUp: TPanel;
    btn_LeftDown: TPanel;
    btn_RightUp: TPanel;
    btn_RightDown: TPanel;
    btn_RightRight: TPanel;
    btn_RightLeft: TPanel;
    btn_LeftLeft: TPanel;
    btn_LeftRight: TPanel;
    TabSheet3: TTabSheet;
    GroupBox1: TGroupBox;
    lbl_DisplayAdapter: TLabel;
    lbl_Direct3DDevice: TLabel;
    lbl_VideoResolution: TLabel;
    lbl_OtherOptions: TLabel;
    edt_DisplayAdapter: TComboBox;
    edt_Direct3dDevice: TComboBox;
    edt_VideoResolution: TComboBox;
    chk_FullScreen: TCheckBox;
    chk_VSync: TCheckBox;
    chk_HardwareYUV: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure btnOkClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCancelClick(Sender: TObject);
    procedure ChangeClick(Sender: TObject);
  private
    MyDirectDraw: IDirectDraw7;
    FXBVideo : XBVideo;
    FXBController: XBController;
    FHasChanges: Boolean;
    FBusyConfiguring: Boolean;
    procedure Apply;
    procedure ConfigureControllerInput(Sender: TObject);
    procedure SetHasChanges(aValue: Boolean);
    property HasChanges: Boolean read FHasChanges write SetHasChanges;
  end;

var
  fmConfiguration: TfmConfiguration;

implementation

function EnumDevices(lpGUID: PGUID; lpDriverDescription: PAnsiChar;
  lpDriverName: PAnsiChar; lpContext: Pointer; Monitor: HMonitor): Windows.BOOL; stdcall;
begin
  TStrings(lpContext).Add(string(AnsiString(lpDriverDescription)));
  Result := True;
end; // EnumDevices

function EnumDisplayModesCallBack(const lpDDSurfaceDesc: TDDSurfaceDesc2;
  lpContext: Pointer): HResult; stdcall;
var
  Width, Height: DWord;
begin
  Width := lpDDSurfaceDesc.dwWidth;
  Height := lpDDSurfaceDesc.dwHeight;

  // Ugly check for portrait display's :
  // TODO -cDxbx : This can surely be done much cleaner!
  if Width < Height then
  begin
    // Just swap the width & height for now :
    Width := lpDDSurfaceDesc.dwHeight;
    Height := lpDDSurfaceDesc.dwWidth;
  end;

  TStrings(lpContext).Add(IntToStr(Width) + ' X ' + IntToStr(Height) + ', ' +
    IntToStr(lpDDSurfaceDesc.ddpfPixelFormat.dwRGBBitCount) +
    ' bits/pixel');
  Result := DDENUMRET_OK;
end; // EnumDisplayModesCallBack

{$R *.dfm}

procedure TfmConfiguration.FormCreate(Sender: TObject);

  procedure _Register(const aPanel: TPanel; const aXBCtrlObject: XBCtrlObject);
  begin
    aPanel.Tag := Ord(aXBCtrlObject);
    aPanel.OnClick := ConfigureControllerInput;
//    aPanel.Caption := FXBController.?[aXBCtrlObject]?.? CharString
  end;

var
  JPEGImage: TJPEGImage;

  tempDirectDraw: IDirectDraw;
  VideoResolutionIndex: Integer;
begin
  // Read settings from shared memory :
  Assert(Assigned(g_EmuShared));

  g_EmuShared.GetXBVideo(@FXBVideo);
  g_EmuShared.GetXBController(@FXBController);

  // Draw the controller image :
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.Canvas.ClipRect);
  JPEGImage := GetJPEGResource('ConfigController');
  Image1.Canvas.Draw(40, 32, JPEGImage);

  // Analog Axis
  _Register(btn_LeftRight, XBCTRL_OBJECT_LTHUMBPOSX);
  _Register(btn_LeftLeft, XBCTRL_OBJECT_LTHUMBNEGX);
  _Register(btn_LeftDown, XBCTRL_OBJECT_LTHUMBNEGY);
  _Register(btn_LeftUp, XBCTRL_OBJECT_LTHUMBPOSY);

  _Register(btn_RightRight, XBCTRL_OBJECT_RTHUMBPOSX);
  _Register(btn_RightLeft, XBCTRL_OBJECT_RTHUMBNEGX);
  _Register(btn_RightDown, XBCTRL_OBJECT_RTHUMBNEGY);
  _Register(btn_RightUp, XBCTRL_OBJECT_RTHUMBPOSY);

  // Analog Buttons
  _Register(btn_A, XBCTRL_OBJECT_A);
  _Register(btn_B, XBCTRL_OBJECT_B);
  _Register(btn_X, XBCTRL_OBJECT_X);
  _Register(btn_Y, XBCTRL_OBJECT_Y);
  _Register(btn_Black, XBCTRL_OBJECT_BLACK);
  _Register(btn_White, XBCTRL_OBJECT_WHITE);
  _Register(btn_LeftTrigger, XBCTRL_OBJECT_LTRIGGER);
  _Register(btn_RightTrigger, XBCTRL_OBJECT_RTRIGGER);

  // Digital Buttons
  _Register(btn_DPadUp, XBCTRL_OBJECT_DPADUP);
  _Register(btn_DPadDown, XBCTRL_OBJECT_DPADDOWN);
  _Register(btn_DPadLeft, XBCTRL_OBJECT_DPADLEFT);
  _Register(btn_DPadRight, XBCTRL_OBJECT_DPADRIGHT);

  _Register(btn_Back, XBCTRL_OBJECT_BACK);
  _Register(btn_Start, XBCTRL_OBJECT_START);
  _Register(btn_LeftThumb, XBCTRL_OBJECT_LTHUMB);
  _Register(btn_RightThumb, XBCTRL_OBJECT_RTHUMB);

  // Retrieve possible video modes :
  DirectDrawEnumerateExA(EnumDevices, edt_DisplayAdapter.Items, 0);
  DirectDrawCreate(nil, {out}tempDirectDraw, MyDirectDraw);
  try
    tempDirectDraw.QueryInterface(IID_IDIRECTDRAW7, {out}MyDirectDraw);
  finally
    tempDirectDraw := nil;
  end;
  edt_VideoResolution.Items.Add('Automatic (Default)');
  MyDirectDraw.EnumDisplayModes(0, nil, edt_VideoResolution.Items, EnumDisplayModesCallBack);

  // Determine which mode was selected :
  VideoResolutionIndex := edt_VideoResolution.Items.IndexOf(string(AnsiString(FXBVideo.GetVideoResolution)));
  if VideoResolutionIndex < 0 then
    VideoResolutionIndex := 0; // Select default if registry value cannot be used

  // Put values into the controls :
  edt_DisplayAdapter.ItemIndex := FXBVideo.GetDisplayAdapter;
  edt_Direct3dDevice.ItemIndex := FXBVideo.GetDirect3DDevice;
  edt_VideoResolution.ItemIndex := VideoResolutionIndex;

  chk_FullScreen.Checked :=  FXBVideo.GetFullscreen;
  chk_VSync.Checked := FXBVideo.GetVSync;
  chk_HardwareYUV.Checked := FXBVideo.GetHardwareYUV;

  // Activate the first tab :
  TreeView1.Select(TreeView1.TopItem);

  // Reset changes flag
  HasChanges := False;
end;

procedure TfmConfiguration.SetHasChanges(aValue: Boolean);
begin
  FHasChanges := aValue;
  btnApply.Enabled := HasChanges;
end;

procedure TfmConfiguration.ConfigureControllerInput(Sender: TObject);
var
  hWndDlg: THandle;
  szNewText: string;
  OrgButtonCaption: string;
  OrgButtonColor: TColor;
  v: int;
  Msg: TMsg;
begin
  // Ensure only one input is configured at a time :
  if FBusyConfiguring then
    Exit;

  if not (Sender is TPanel) then
    Exit;

  hWndDlg := Handle;
  OrgButtonCaption := TPanel(Sender).Caption;
  OrgButtonColor := TPanel(Sender).Color;
  szNewText := 'Recieved no user input, try again...';

  // TODO : Disable all buttons

  Caption := 'Waiting for your input...';
  try
    FBusyConfiguring := True;

    // Change the color of the active 'button' :
    TPanel(Sender).Color := clRed;

    FXBController.ConfigBegin(hWndDlg, XBCtrlObject(TPanel(Sender).Tag));

    // wait for input, or 5 second timeout
    for v := 100 downto 0 do
    begin
      // update the button text every second
      if (v mod 20) = 0 then
        TPanel(Sender).Caption := IntToStr((v+19) div 20);

      if FXBController.GetError() <> '' then
        Break;

      // TODO : Find & fix the cause for exceptions that happen here :
      if FXBController.ConfigPoll({var}szNewText) then
      begin
        HasChanges := True;
        Break;
      end;

      // Make sure everything is rendered :
      Application.ProcessMessages;
      Sleep(50);
    end;

    if FXBController.GetError() = '' then
      FXBController.ConfigEnd();

    // TODO : Re-enable all buttons

    // Update window with status
    if FXBController.GetError() <> '' then
      szNewText := FXBController.GetError();

    // Remove all pending mouse and key messages :
    while(PeekMessage({var}Msg, hWndDlg, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE)) do
      ;
    while(PeekMessage({var}Msg, hWndDlg, WM_KEYFIRST,   WM_KEYLAST,   PM_REMOVE)) do
      ;

  finally
    FBusyConfiguring := False;

    // Restore the display :
    TPanel(Sender).Caption := OrgButtonCaption;
    TPanel(Sender).Color := OrgButtonColor;
    Caption := szNewText;

    // Make sure everything is rendered :
    Application.ProcessMessages;
  end;
end;

procedure TfmConfiguration.Apply;
begin
  // Read the controls back into the XBVideo structure :
  FXBVideo.SetDisplayAdapter(edt_DisplayAdapter.ItemIndex);
  FXBVideo.SetDirect3DDevice(edt_Direct3dDevice.ItemIndex);
  FXBVideo.SetVideoResolution(PAnsiChar(AnsiString(edt_VideoResolution.Text)));

  FXBVideo.SetFullscreen(chk_FullScreen.Checked);
  FXBVideo.SetVSync(chk_VSync.Checked);
  FXBVideo.SetHardwareYUV(chk_HardwareYUV.Checked);

  // Publish the XBVideo settings via shared memory :
  g_EmuShared.SetXBVideo(@FXBVideo);

  // Publish the Controller settings via shared memory :
  g_EmuShared.SetXBController(@FXBController);

  // Persist all settings to the registry :
  g_EmuShared.Save;

  // Reset changed state :
  HasChanges := False;
end;

procedure TfmConfiguration.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Normally, the form may close, especially if no changes have been made :
  {var}CanClose := True;
  if not HasChanges then
    Exit;

  // If changes have been made, check if the user wants to save them :
  case MessageBox(0, 'Do you wish to apply your changes?', 'Dxbx', MB_ICONQUESTION or MB_YESNOCANCEL) of
    IDYES:
      // Save before close :
      Apply;
    IDCANCEL:
      // Do not close :
      {var}CanClose := False;
  end;
end;

procedure TfmConfiguration.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  // Switch to the tab corresponding to the selected tree node :
  PageControl1.ActivePageIndex := Node.Index;
end;

procedure TfmConfiguration.ChangeClick(Sender: TObject);
begin
  HasChanges := True;
end;

procedure TfmConfiguration.btnApplyClick(Sender: TObject);
begin
  Apply;
end;

procedure TfmConfiguration.btnOkClick(Sender: TObject);
begin
  if HasChanges then
    Apply;

  Close;
end;

procedure TfmConfiguration.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.

