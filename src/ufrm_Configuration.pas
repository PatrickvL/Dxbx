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
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, jpeg, IniFiles
  // DirectX
  , DirectDraw
  , DirectSound
  // Dxbx
  , uLog
  , ufrm_About // GetJPEGResource
  , uTypes
  , uXBController
  , uXbVideo
  , uXBSound
  , uEmuShared, ImgList, Menus, ActnList
  ;

// The Xbox controller image is borrowed from http://halo.wikia.com/wiki/Halo_Controls
// Some day, we should probably photograph or draw our own. Until then, this one suffices.
//
// Also, take a look at the following shots of the controller configuration of pcsx2 :
// http://charlii-5800.blog.friendster.com/2007/07/how-to-use-pcsx2-the-playstation-2-emulator/
// These pad settings allow more space per button, while we reserve space for just one character.
// It's okay for keyboard control, but joystick and mouse control needs space for a wider description.

type
  lsStatus = (lsEnabled, lsIgnored, lsDisabled);

  TLogStatus = class(TObject)
  private
    FName: string;
    FStatus: lsStatus;
    FLogFlag: TLogFlags;
  public
    constructor Create(aName: string; aStatus: lsStatus; aLogFlag: TLogFlags);

    property Name: string read FName;
    property Status: lsStatus read FStatus write FStatus;
    property LogFlag: TLogFlags read FLogFlag;
  end;

  TfmConfiguration = class(TForm)
    TreeView1: TTreeView;
    btnOk: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    ConfigControl: TPageControl;
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
    GroupBox2: TGroupBox;
    lstLogging: TListView;
    ImageList1: TImageList;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    btnDisableAll: TButton;
    btnEnableAll: TButton;
    TabSheet4: TTabSheet;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    edt_AudioAdapter: TComboBox;
    chkMute: TCheckBox;
    MainMenu1: TMainMenu;
    ActionList1: TActionList;
    mnuConfig: TMenuItem;
    mnuLoadConfig: TMenuItem;
    mnuSaveConfig: TMenuItem;
    actLoadConfig: TAction;
    actSaveConfig: TAction;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure btnOkClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCancelClick(Sender: TObject);
    procedure ChangeClick(Sender: TObject);
    procedure ConfigControlChange(Sender: TObject);
    procedure lstLoggingDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure lstLoggingClick(Sender: TObject);
    procedure btnLoadLogConfigClick(Sender: TObject);
    procedure btnDisableAllClick(Sender: TObject);
    procedure btnEnableAllClick(Sender: TObject);
    procedure actSaveConfigExecute(Sender: TObject);
    procedure actLoadConfigExecute(Sender: TObject);
  private
    MyDirectDraw: IDirectDraw7;
    FXBVideo : XBVideo;
    FXBSound : XBSound;
    FXBController: XBController;
    FHasChanges: Boolean;
    FBusyConfiguring: Boolean;
    procedure RefreshItem(Item: TListItem);

    procedure ChangeStatusAllSettings(aStatus: lsStatus);

    procedure SaveLogConfig;
    procedure SaveVideoConfig;
    procedure SaveSoundConfig;
    procedure SaveControllerConfig;

    procedure LoadLogConfig;
    procedure LoadVideoConfig;
    procedure LoadSoundConfig;
    procedure LoadControllerConfig;

    procedure RefreshMenu;

    procedure AddAllLogItems;
    procedure AddLogItem(aName: string; aLogStatus: lsStatus; aLogFlag: TLogFlags);
    procedure Apply;
    procedure ConfigureControllerInput(Sender: TObject);
    procedure SetHasChanges(aValue: Boolean);
    property HasChanges: Boolean read FHasChanges write SetHasChanges;


  end;

var
  fmConfiguration: TfmConfiguration;

const
  idx_controller = 0;
  idx_video = 1;
  idx_sound = 2;
  idx_logging = 3;

implementation

type
  RSoundDeviceInfo = record
    GUID: TGuid;
    DriverDescription: AnsiString;
    DriverName: AnsiString;
  end;
  PSoundDeviceInfo = ^RSoundDeviceInfo;

function EnumSoundDevices(lpGUID: PGUID; lpDriverDescription: PAnsiChar;
  lpDriverName: PAnsiChar; lpContext: Pointer): Windows.BOOL; stdcall;
var
  SoundDeviceInfo: PSoundDeviceInfo;
begin
  New({var}SoundDeviceInfo);
  ZeroMemory(SoundDeviceInfo, SizeOf(SoundDeviceInfo));
  if Assigned(lpGUID) then
    SoundDeviceInfo.GUID := lpGUID^;
  if Assigned(lpDriverDescription) then
    SetString(SoundDeviceInfo.DriverDescription, lpDriverDescription, StrLen(lpDriverDescription));
  if Assigned(lpDriverName) then
    SetString(SoundDeviceInfo.DriverName, lpDriverName, StrLen(lpDriverName));

  TStrings(lpContext).AddObject(SoundDeviceInfo.DriverDescription, TObject(SoundDeviceInfo));
  Result := True;
end; // EnumSoundDevices

function EnumDevicesEx(lpGUID: PGUID; lpDriverDescription: PAnsiChar;
  lpDriverName: PAnsiChar; lpContext: Pointer; Monitor: HMonitor): Windows.BOOL; stdcall;
begin
  TStrings(lpContext).Add(string(AnsiString(lpDriverDescription)));
  Result := True;
end; // EnumDevicesEx

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

function SameGUID(const aGUID1, aGUID2: TGUID): Boolean;
begin
  Result := (memcmp(@aGUID1, @aGUID2, SizeOf(TGUID)) = 0);
end;

procedure TfmConfiguration.FormCreate(Sender: TObject);

  procedure _Register(const aPanel: TPanel; const aXBCtrlObject: XBCtrlObject);
  begin
    aPanel.Tag := Ord(aXBCtrlObject);
    aPanel.OnClick := ConfigureControllerInput;
  end;

var
  JPEGImage: TJPEGImage;

  tempDirectDraw: IDirectDraw;
  VideoResolutionIndex: Integer;

  i: Integer;
begin
  // Read settings from shared memory :
  Assert(Assigned(g_EmuShared));

  g_EmuShared.GetXBVideo(@FXBVideo);
  g_EmuShared.GetXBController(@FXBController);
  g_EmuShared.GetXBSound(@FXBSound);

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
  DirectDrawEnumerateExA(EnumDevicesEx, edt_DisplayAdapter.Items, 0);

  // Retrieve possible sound devices :
  DirectSoundEnumerateA(EnumSoundDevices, edt_AudioAdapter.Items);

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

  edt_AudioAdapter.ItemIndex := 0;
  for i := 0 to edt_AudioAdapter.Items.Count - 1 do
  begin
    if SameGUID(PSoundDeviceInfo(edt_AudioAdapter.Items.Objects[i]).GUID, FXBSound.GetSoundAdapterGUID) then
    begin
      edt_AudioAdapter.ItemIndex := i;
      Break;
    end;
  end;

  chkMute.Checked := FXBSound.GetMute;
  chk_FullScreen.Checked :=  FXBVideo.GetFullscreen;
  chk_VSync.Checked := FXBVideo.GetVSync;
  chk_HardwareYUV.Checked := FXBVideo.GetHardwareYUV;

  // Activate the first tab :
  TreeView1.Select(TreeView1.TopItem);

  AddAllLogItems;

  // Reset changes flag
  HasChanges := False;
  RefreshMenu;
end;

procedure TfmConfiguration.LoadControllerConfig;
begin
  OpenDialog1.Filter := 'Controller config (*.ccf)|*.ccf';

  if OpenDialog1.Execute then
  begin
    FXBController.LoadConfigFromIni(OpenDialog1.FileName);
    HasChanges := True;
  end;
end;

procedure TfmConfiguration.LoadLogConfig;
var
  IniFile: TIniFile;
  lIndex: Integer;
  LogStatus: TLogStatus;
begin
  OpenDialog1.Filter := 'Logging config (*.lcf)|*.lcf';

  if OpenDialog1.Execute then
  begin
    IniFile := TIniFile.Create(OpenDialog1.FileName);

    for lIndex := 0 to lstLogging.Items.Count -1 do
    begin
      LogStatus := TLogStatus(lstLogging.Items[lIndex].Data);
      LogStatus.Status := lsStatus(IniFile.ReadInteger('Logging', LogStatus.Name, Ord(lsIgnored)));
      RefreshItem(lstLogging.Items[lIndex]);
    end;

    HasChanges := True;
  end;
end;

procedure TfmConfiguration.LoadSoundConfig;
var
  IniFile: TIniFile;
begin
  OpenDialog1.Filter := 'Sound config (*.scf)|*.scf';

  if OpenDialog1.Execute then
  begin
    IniFile := TIniFile.Create(OpenDialog1.FileName);
    try

      edt_AudioAdapter.ItemIndex := edt_AudioAdapter.Items.IndexOf(IniFile.ReadString('Sound', 'Adapter', ''));
      chkMute.Checked := IniFile.ReadBool('Sound', 'Mute', False);

    finally
      FreeAndNil(IniFile);
    end;

    HasChanges := True;
  end;

end;

procedure TfmConfiguration.LoadVideoConfig;
var
  IniFile: TIniFile;
begin
  OpenDialog1.Filter := 'Video config (*.vcf)|*.vcf';

  if OpenDialog1.Execute then
  begin
    IniFile := TIniFile.Create(OpenDialog1.FileName);
    try

      edt_DisplayAdapter.ItemIndex := IniFile.ReadInteger('Video', 'Adapter', 0);
      edt_Direct3dDevice.ItemIndex := IniFile.ReadInteger('Video', 'Device', 0);
      edt_VideoResolution.ItemIndex := IniFile.ReadInteger('Video', 'Resolution', 0);
      chk_FullScreen.Checked := IniFile.ReadBool('Video', 'FullScreen', True);
      chk_HardwareYUV.Checked := IniFile.ReadBool('Video', 'YUV', True);
      chk_VSync.Checked := IniFile.ReadBool('Video', 'VSYNC', True);

    finally
      FreeAndNil(IniFile);
    end;

    HasChanges := True;
  end;
end;

procedure TfmConfiguration.lstLoggingClick(Sender: TObject);
var
  SelectedLogStatus: TLogStatus;
begin
  if not Assigned(lstLogging.Selected) then
    Exit;

  SelectedLogStatus := TLogStatus(lstLogging.Selected.Data);
  if SelectedLogStatus = nil then
    Exit;

  case SelectedLogStatus.Status of
    lsEnabled: SelectedLogStatus.Status := lsIgnored;
    lsIgnored: SelectedLogStatus.Status := lsDisabled;
    lsDisabled: SelectedLogStatus.Status := lsEnabled;
  end;

  RefreshItem(lstLogging.Selected);
  HasChanges := True;
end;

procedure TfmConfiguration.lstLoggingDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
begin
  Item.ImageIndex := -1;

  Item.SubItemImages[0] := -1;
  Item.SubItemImages[1] := -1;
  Item.SubItemImages[2] := -1;

  Item.SubItemImages[Ord(TLogStatus(Item.Data).Status)] := Ord(TLogStatus(Item.Data).Status);
end;

procedure TfmConfiguration.ConfigControlChange(Sender: TObject);
begin
  TreeView1.Items[ConfigControl.ActivePageIndex].Selected := True;
  RefreshMenu;
end;

procedure TfmConfiguration.RefreshItem(Item: TListItem);
begin
  Item.ImageIndex := -1;

  Item.SubItemImages[0] := -1;
  Item.SubItemImages[1] := -1;
  Item.SubItemImages[2] := -1;

  Item.SubItemImages[Ord(TLogStatus(Item.Data).Status)] := Ord(TLogStatus(Item.Data).Status);
end;

procedure TfmConfiguration.RefreshMenu;
var
  ConfigItem: string;
begin
  case ConfigControl.ActivePageIndex of
    idx_controller: ConfigItem := 'controller';
    idx_video: ConfigItem := 'video';
    idx_sound: ConfigItem := 'sound';
    idx_logging: ConfigItem := 'logging';
  end;

  actLoadConfig.Caption := format('Load %s config from file', [ConfigItem]);
  actSaveConfig.Caption := format('Save %s config from file', [ConfigItem]);
end;

procedure TfmConfiguration.SaveControllerConfig;
begin
  SaveDialog1.Filter := 'Controller config (*.ccf)|*.ccf';
  SaveDialog1.DefaultExt := '*.ccf';

  if SaveDialog1.Execute then
    FXBController.SaveConfigToIni(SaveDialog1.FileName);
end;

procedure TfmConfiguration.SaveLogConfig;
var
  IniFile: TIniFile;
  lIndex: Integer;
  LogStatus: TLogStatus;
begin
  SaveDialog1.Filter := 'Logging config (*.lcf)|*.lcf';
  SaveDialog1.DefaultExt := '*.lcf';

  if SaveDialog1.Execute then
  begin
    IniFile := TIniFile.Create(SaveDialog1.FileName);
    try
      for lIndex := 0 to lstLogging.Items.Count -1 do
      begin
        LogStatus := TLogStatus(lstLogging.Items[lIndex].Data);
        IniFile.WriteInteger('Logging', LogStatus.Name, ord(LogStatus.Status));
      end;

    finally
      FreeAndNil(IniFile);
    end;

  end;
end;

procedure TfmConfiguration.SaveSoundConfig;
var
  IniFile: TIniFile;
begin
  SaveDialog1.Filter := 'Sound config (*.scf)|*.scf';
  SaveDialog1.DefaultExt := '*.scf';

  if SaveDialog1.Execute then
  begin
    IniFile := TIniFile.Create(SaveDialog1.FileName);
    try
      IniFile.WriteString('Sound', 'Adapter', edt_AudioAdapter.Items[edt_AudioAdapter.ItemIndex]);
      IniFile.WriteBool('Sound', 'Mute', chkMute.Checked);
    finally
      FreeAndNil(IniFile);
    end;
  end;
end;

procedure TfmConfiguration.SaveVideoConfig;
var
  IniFile: TIniFile;
begin
  SaveDialog1.Filter := 'Video config (*.vcf)|*.vcf';
  SaveDialog1.DefaultExt := '*.vcf';

  if SaveDialog1.Execute then
  begin
    IniFile := TIniFile.Create(SaveDialog1.FileName);
    try
      IniFile.WriteInteger('Video', 'Adapter', edt_DisplayAdapter.ItemIndex);
      IniFile.WriteInteger('Video', 'Device', edt_Direct3dDevice.ItemIndex);
      IniFile.WriteInteger('Video', 'Resolution', edt_VideoResolution.ItemIndex);
      IniFile.WriteBool('Video', 'FullScreen', chk_FullScreen.Checked);
      IniFile.WriteBool('Video', 'YUV', chk_HardwareYUV.Checked);
      IniFile.WriteBool('Video', 'VSYNC', chk_VSync.Checked);
    finally
      FreeAndNil(IniFile);
    end;

  end;

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

procedure TfmConfiguration.actLoadConfigExecute(Sender: TObject);
begin
  case ConfigControl.ActivePageIndex of
    idx_controller: LoadControllerConfig;
    idx_video: LoadVideoConfig;
    idx_sound: LoadSoundConfig;
    idx_logging: LoadLogConfig;
  end;
end;

procedure TfmConfiguration.actSaveConfigExecute(Sender: TObject);
begin
  case ConfigControl.ActivePageIndex of
    idx_controller: SaveControllerConfig;
    idx_video: SaveVideoConfig;
    idx_sound: SaveSoundConfig;
    idx_logging: SaveLogConfig;
  end;
end;

procedure TfmConfiguration.AddAllLogItems;

  function _GetLogStatus(const Flag: TLogFlags): lsStatus;
  begin
    if (Flag and g_EmuShared.m_ActiveLogFlags) > 0 then
      Result := lsEnabled
    else if (Flag and g_EmuShared.m_DisabledLogFlags) = 0 then
      Result := lsIgnored
    else
      Result := lsDisabled;
  end;

begin
  AddLogItem('Debug', _GetLogStatus(lfDebug), lfDebug);
  AddLogItem('Trace', _GetLogStatus(lfTrace), lfTrace);
  AddLogItem('Extreme', _GetLogStatus(lfExtreme), lfExtreme);
  AddLogItem('Cxbx', _GetLogStatus(lfCxbx), lfCxbx);
  AddLogItem('Dxbx', _GetLogStatus(lfDxbx), lfDxbx);
  AddLogItem('Kernel', _GetLogStatus(lfKernel), lfKernel);
  AddLogItem('Patch', _GetLogStatus(lfPatch), lfPatch);
  AddLogItem('SymbolScan', _GetLogStatus(lfSymbolScan), lfSymbolScan);
  AddLogItem('PixelShader', _GetLogStatus(lfPixelShader), lfPixelShader);
  AddLogItem('VertexShader', _GetLogStatus(lfVertexShader), lfVertexShader);
  AddLogItem('VertexBuffer', _GetLogStatus(lfVertexBuffer), lfVertexBuffer);
  AddLogItem('PushBuffer', _GetLogStatus(lfPushBuffer), lfPushBuffer);
  AddLogItem('Invalid', _GetLogStatus(lfInvalid), lfInvalid);
  AddLogItem('Heap', _GetLogStatus(lfHeap), lfHeap);
  AddLogItem('File', _GetLogStatus(lfFile), lfFile);
  AddLogItem('Sound', _GetLogStatus(lfSound), lfSound);
  AddLogItem('Graphics', _GetLogStatus(lfGraphics), lfGraphics);
  AddLogItem('Threading', _GetLogStatus(lfThreading), lfThreading);
  AddLogItem('Online', _GetLogStatus(lfXOnline), lfXOnline);
  AddLogItem('Xapi', _GetLogStatus(lfXapi), lfXapi);
  AddLogItem('Memory', _GetLogStatus(lfMemory), lfMemory);
  AddLogItem('ReturnValue', _GetLogStatus(lfReturnValue), lfReturnValue);
end;

procedure TfmConfiguration.AddLogItem(
  aName: string;
  aLogStatus: lsStatus;
  aLogFlag: TLogFlags
);
var
  ListItem: TListItem;
begin
  ListItem := lstLogging.Items.Add;
  ListItem.Caption := aName;
  ListItem.Data := TLogStatus.Create(aName, aLogStatus, aLogFlag);
  ListItem.ImageIndex := -1;
  ListItem.SubItems.Add('');
  ListItem.SubItems.Add('');
  ListItem.SubItems.Add('');

  ListItem.SubItemImages[Ord(aLogStatus)] := Ord(aLogStatus);
end;

procedure TfmConfiguration.Apply;
var
  lIndex: Integer;
  LogStatus: TLogStatus;
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

  // Publish the XBSound settings via shared memory :
  FXBSound.SetSoundAdapterGUID(PSoundDeviceInfo(edt_AudioAdapter.Items.Objects[edt_AudioAdapter.ItemIndex]).GUID);
  FXBSound.SetMute(chkMute.Checked);
  g_EmuShared.SetXBSound(@FXBSound);

  // Publish the Controller settings via shared memory :
  g_EmuShared.SetXBController(@FXBController);

  // Set Active and Disabled LogFlags
  g_EmuShared.m_ActiveLogFlags := 0;
  g_EmuShared.m_DisabledLogFlags := 0;

  for lIndex := 0 to lstLogging.Items.Count - 1 do
  begin
    LogStatus := TLogStatus(lstLogging.Items[lIndex].Data);

    if LogStatus.Status = lsEnabled then
      g_EmuShared.m_ActiveLogFlags := g_EmuShared.m_ActiveLogFlags or LogStatus.LogFlag;
    if LogStatus.Status = lsDisabled then
      g_EmuShared.m_DisabledLogFlags := g_EmuShared.m_DisabledLogFlags or LogStatus.LogFlag;
  end;

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
  ConfigControl.ActivePageIndex := Node.Index;
end;

procedure TfmConfiguration.ChangeStatusAllSettings(aStatus: lsStatus);
var
  lIndex: Integer;
  LogStatus: TLogStatus;
begin
  for lIndex := 0 to lstLogging.Items.Count -1 do
  begin
    LogStatus := TLogStatus(lstLogging.Items[lIndex].Data);
    LogStatus.Status := aStatus;
    RefreshItem(lstLogging.Items[lIndex]);
  end;

  HasChanges := True;
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

procedure TfmConfiguration.btnDisableAllClick(Sender: TObject);
begin
  ChangeStatusAllSettings(lsDisabled);
end;

procedure TfmConfiguration.btnEnableAllClick(Sender: TObject);
begin
  ChangeStatusAllSettings(lsEnabled);
end;

procedure TfmConfiguration.btnLoadLogConfigClick(Sender: TObject);
begin
  LoadLogConfig;
end;

{ TLogStatus }

constructor TLogStatus.Create(aName: string; aStatus: lsStatus; aLogFlag: TLogFlags);
begin
  inherited Create;

  FName := aName;
  FStatus := aStatus;
  FLogFlag := aLogFlag;
end;

end.

