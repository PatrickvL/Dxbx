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

unit uXBController;

interface

{$INCLUDE Dxbx.inc}

uses
  // Delphi
    Windows
  , JwaWinType
  , Classes
  , SysUtils
  // 3rd party
  , DirectInput
  , Direct3D
  , XInput
  // Dxbx
  , uLog
  , uTypes
  , uError
  , XboxKrnl // NULL
  , uDxbxUtils
//  , uEmuXTL
  ;

type
  // Xbox Controller Object IDs
  // XBCtrlObject: Source=XBController.h Revision=martin#39 Translator=PatrickvL Done=100
  XBCtrlObject = (
    // Analog Axis
    XBCTRL_OBJECT_LTHUMBPOSX, // = 0
    XBCTRL_OBJECT_LTHUMBNEGX,
    XBCTRL_OBJECT_LTHUMBPOSY,
    XBCTRL_OBJECT_LTHUMBNEGY,
    XBCTRL_OBJECT_RTHUMBPOSX,
    XBCTRL_OBJECT_RTHUMBNEGX,
    XBCTRL_OBJECT_RTHUMBPOSY,
    XBCTRL_OBJECT_RTHUMBNEGY,
    // Analog Buttons
    XBCTRL_OBJECT_A,
    XBCTRL_OBJECT_B,
    XBCTRL_OBJECT_X,
    XBCTRL_OBJECT_Y,
    XBCTRL_OBJECT_BLACK,
    XBCTRL_OBJECT_WHITE,
    XBCTRL_OBJECT_LTRIGGER,
    XBCTRL_OBJECT_RTRIGGER,
    // Digital Buttons
    XBCTRL_OBJECT_DPADUP,
    XBCTRL_OBJECT_DPADDOWN,
    XBCTRL_OBJECT_DPADLEFT,
    XBCTRL_OBJECT_DPADRIGHT,
    XBCTRL_OBJECT_BACK,
    XBCTRL_OBJECT_START,
    XBCTRL_OBJECT_LTHUMB,
    XBCTRL_OBJECT_RTHUMB);

const
  // Total number of components
  XBCTRL_OBJECT_COUNT = (Ord(High(XBCtrlObject)) - Ord(Low(XBCtrlObject)) + 1);
  // Maximum number of devices allowed
  XBCTRL_MAX_DEVICES = XBCTRL_OBJECT_COUNT;

  m_DeviceNameLookup: array [0..XBCTRL_OBJECT_COUNT-1] of string = ( 'LThumbPosX', 'LThumbNegX',
                                           'LThumbPosY', 'LThumbNegY',
                                           'RThumbPosX', 'RThumbNegX',
                                           'RThumbPosY', 'RThumbNegY',
                                           'X', 'Y', 'A', 'B', 'White',
                                           'Black', 'LTrigger', 'RTrigger',
                                           'DPadUp', 'DPadDown', 'DPadLeft',
                                           'DPadRight', 'Back', 'Start',
                                           'LThumb', 'RThumb');

// Offsets into analog button array
const
  XINPUT_GAMEPAD_A = 0;
  XINPUT_GAMEPAD_B = 1;
  XINPUT_GAMEPAD_X = 2;
  XINPUT_GAMEPAD_Y = 3;
  XINPUT_GAMEPAD_BLACK = 4;
  XINPUT_GAMEPAD_WHITE = 5;
  XINPUT_GAMEPAD_LEFT_TRIGGER = 6;
  XINPUT_GAMEPAD_RIGHT_TRIGGER = 7;

// Masks for digital buttons
const
  XINPUT_GAMEPAD_DPAD_UP = $00000001;
  XINPUT_GAMEPAD_DPAD_DOWN = $00000002;
  XINPUT_GAMEPAD_DPAD_LEFT = $00000004;
  XINPUT_GAMEPAD_DPAD_RIGHT = $00000008;
  XINPUT_GAMEPAD_START = $00000010;
  XINPUT_GAMEPAD_BACK = $00000020;
  XINPUT_GAMEPAD_LEFT_THUMB = $00000040;
  XINPUT_GAMEPAD_RIGHT_THUMB = $00000080;

// Device Flags
const
  DEVICE_FLAG_JOYSTICK = (1 shl 0);
  DEVICE_FLAG_KEYBOARD = (1 shl 1);
  DEVICE_FLAG_MOUSE = (1 shl 2);
  DEVICE_FLAG_AXIS = (1 shl 3);
  DEVICE_FLAG_BUTTON = (1 shl 4);
  DEVICE_FLAG_POSITIVE = (1 shl 5);
  DEVICE_FLAG_NEGATIVE = (1 shl 6);
  DEVICE_FLAG_MOUSE_CLICK = (1 shl 7);
  DEVICE_FLAG_MOUSE_LX = (1 shl 8);
  DEVICE_FLAG_MOUSE_LY = (1 shl 9);
  DEVICE_FLAG_MOUSE_LZ = (1 shl 10);

// Detection Sensitivity
const
  DETECT_SENSITIVITY_JOYSTICK = 25000;
  DETECT_SENSITIVITY_BUTTON = 0;
  DETECT_SENSITIVITY_MOUSE = 5;
  DETECT_SENSITIVITY_POV = 50000;

type
  // DirectInput Enumeration Types
  XBCtrlState = (
    XBCTRL_STATE_NONE = 0,
    XBCTRL_STATE_CONFIG,
    XBCTRL_STATE_LISTEN
  );

  // Xbox Controller Object Config
  XBCtrlObjectCfg = packed record
    dwDevice: Integer; // offset into m_InputDevice
    dwInfo: Integer; // extended information, depending on dwFlags
    dwFlags: Integer; // flags explaining the data format
  end;

  XINPUT_GAMEPAD = packed record
    wButtons: Word;
    bAnalogButtons: array[0..7] of Byte;
    sThumbLX: SHORT;
    sThumbLY: SHORT;
    sThumbRX: SHORT;
    sThumbRY: SHORT;
  end;
  PXINPUT_GAMEPAD = ^XINPUT_GAMEPAD;

  XINPUT_STATE = packed record
    dwPacketNumber: DWord;
    Gamepad: XINPUT_GAMEPAD;
  end;
  PXINPUT_STATE = ^XINPUT_STATE;

  XTL_LPDIRECTINPUT8 = IDirectInput8; // TODO Dxbx : How is this type defined?
  XTL_LPDIRECTINPUTDEVICE8 = IDirectInputDevice8; // TODO Dxbx : How is this type defined?

  LPCDIDEVICEOBJECTINSTANCE = TDIDeviceObjectInstanceA;
  LPCDIDEVICEINSTANCE = TDIDeviceInstanceA;

  // DirectInput Devices
  InputDevice = packed record
    m_Device: XTL_LPDIRECTINPUTDEVICE8;
    m_Flags: Integer;
  end;

  // IMPORTANT NOTE : Keep the data-layout of this record in-sync with
  // the Cxbx version (if you want to maintain CxbxKrnl.DLL compatibility) !
  XBController = object(Error)
  private
    // Device Names
    m_DeviceName: array [0..XBCTRL_MAX_DEVICES - 1] of array [0..260-1] of AnsiChar;
    // Object Configuration
    m_ObjectConfig: array [XBCtrlObject] of XBCtrlObjectCfg;
    // DirectInput
    m_pDirectInput8: XTL_LPDIRECTINPUT8;
    // DirectInput Devices
    m_InputDevice: array [0..XBCTRL_MAX_DEVICES - 1] of InputDevice;
    // Current State
    m_CurrentState: XBCtrlState;
    // Config State Variables
    lPrevMouseX, lPrevMouseY, lPrevMouseZ: LongInt;
    CurConfigObject: XBCtrlObject;
    // Etc State Variables
    m_dwInputDeviceCount: Integer;
    m_dwCurObject: Integer;
  private
    // Object Mapping
    procedure Map(aobject: XBCtrlObject; szDeviceName: PAnsiChar; dwInfo: Integer; dwFlags: Integer);
    // Find the look-up value for a DeviceName (creating if needed)
    function Insert(szDeviceName: PAnsiChar): Integer;
    // Update the object lookup offsets for a device
    procedure ReorderObjects(szDeviceName: PAnsiChar; aPos: Integer);
    function EnumObjectsCallback(lpddoi: LPCDIDEVICEOBJECTINSTANCE): BOOL;
    function EnumGameCtrlCallback(var lpddi: TDIDeviceInstanceA): BOOL;
  public
    procedure Initialize;
    procedure Finalize;
    // Registry Load/Save
    procedure Load(szRegistryKey: PAnsiChar);
    procedure Save(szRegistryKey: PAnsiChar);
    // Configuration
    procedure ConfigBegin(ahwnd: Handle; aObject: XBCtrlObject);
    function ConfigPoll(szStatus: PAnsiChar): LongBool;
    procedure ConfigEnd;
    // Listening
    procedure ListenPoll(Controller: PXINPUT_STATE);
    procedure ListenBegin(ahwnd: Handle);
    procedure ListenEnd;
    // DirectInput Init / Cleanup
    procedure DInputInit(ahwnd: Handle);
    procedure DInputCleanup;
    // Check if a device is currently in the configuration
    function DeviceIsUsed(szDeviceName: PAnsiChar): LongBool;
  end;
  PXBController = ^XBController;

implementation

// Source=XBController.cpp Revision=martin#39 Translator=PatrickvL Done=100
procedure XBController.Initialize; // was XBController::XBController
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
var
  v: Integer;
begin
  inherited Initialize;

  m_CurrentState := XBCTRL_STATE_NONE;

  for v := 0 to XBCTRL_MAX_DEVICES - 1 do
  begin
    m_DeviceName[v][0] := #0;

    m_InputDevice[v].m_Device := NULL;
    m_InputDevice[v].m_Flags := 0;
  end;

  for v := 0 to XBCTRL_OBJECT_COUNT - 1 do
  begin
    m_ObjectConfig[XBCtrlObject(v)].dwDevice := -1;
    m_ObjectConfig[XBCtrlObject(v)].dwInfo := -1;
    m_ObjectConfig[XBCtrlObject(v)].dwFlags := 0;
  end;

  m_pDirectInput8 := NULL;

  m_dwInputDeviceCount := 0;
end;

// Source=XBController.cpp Revision=martin#39 Translator=PatrickvL Done=100
procedure XBController.Finalize; // was XBController::~XBController
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
begin
  if m_CurrentState = XBCTRL_STATE_CONFIG then
    ConfigEnd()
  else
    if m_CurrentState = XBCTRL_STATE_LISTEN then
      ListenEnd();

  inherited Finalize;
end;

function XBController.EnumObjectsCallback(lpddoi: LPCDIDEVICEOBJECTINSTANCE): BOOL;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  diprg: DIPROPRANGE;
  hRet: HRESULT;
begin
  if (lpddoi.dwType and DIDFT_AXIS) > 0 then
  begin
    diprg.diph.dwSize := SizeOf(DIPROPRANGE);
    diprg.diph.dwHeaderSize := SizeOf(DIPROPHEADER);
    diprg.diph.dwHow := DIPH_BYID;
    diprg.diph.dwObj := lpddoi.dwType;
    diprg.lMin := 0 - 32768;
    diprg.lMax := 0 + 32767;

    hRet := m_InputDevice[m_dwCurObject].m_Device.SetProperty(DIPROP_RANGE, diprg.diph);

    if FAILED(hRet) then
    begin
      if hRet = E_NOTIMPL then
        Result := DIENUM_CONTINUE
      else
        Result := DIENUM_STOP;

      Exit;
    end;
  end
  else if (lpddoi.dwType and DIDFT_BUTTON) > 0 then
  begin
    diprg.diph.dwSize := SizeOf(DIPROPRANGE);
    diprg.diph.dwHeaderSize := SizeOf(DIPROPHEADER);
    diprg.diph.dwHow := DIPH_BYID;
    diprg.diph.dwObj := lpddoi.dwType;
    diprg.lMin := 0;
    diprg.lMax := 255;

    hRet := m_InputDevice[m_dwCurObject].m_Device.SetProperty(DIPROP_RANGE, diprg.diph);

    if (FAILED(hRet)) then
    begin
      if (hRet = E_NOTIMPL) then
        Result := DIENUM_CONTINUE
      else
        Result := DIENUM_STOP;

      Exit;
    end;
  end;

  Result := DIENUM_CONTINUE;
end;

function WrapEnumObjectsCallback(var lpddoi: TDIDeviceObjectInstanceA; pvRef: Pointer): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := PXBController(pvRef).EnumObjectsCallback(lpddoi);
end;

function XBController.EnumGameCtrlCallback(var lpddi: LPCDIDEVICEINSTANCE): BOOL;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  hRet: HRESULT;
begin
  if (m_CurrentState = XBCTRL_STATE_LISTEN) and not DeviceIsUsed(lpddi.tszInstanceName) then
  begin
    Result := DIENUM_CONTINUE;
    Exit;
  end;

  hRet := m_pDirectInput8.CreateDevice(lpddi.guidInstance, {out}IDirectInputDevice8A(m_InputDevice[m_dwInputDeviceCount].m_Device), nil);

  if (not FAILED(hRet)) then
  begin
    m_InputDevice[m_dwInputDeviceCount].m_Flags := DEVICE_FLAG_JOYSTICK;
    m_InputDevice[m_dwInputDeviceCount].m_Device.SetDataFormat(c_dfDIJoystick);
    Inc(m_dwInputDeviceCount);

    if (m_CurrentState = XBCTRL_STATE_LISTEN) then
      ReorderObjects(lpddi.tszInstanceName, m_dwInputDeviceCount - 1);
  end;

  Result := DIENUM_CONTINUE;
end;

function WrapEnumGameCtrlCallback(var lpddi: TDIDeviceInstanceA; pvRef: Pointer): BOOL; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := PXBController(pvRef).EnumGameCtrlCallback(lpddi);
end;

procedure XBController.ListenPoll(Controller: PXINPUT_STATE);
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  hRet: HRESULT;
  v: XBCtrlObject;

  dwDevice: DWORD;
  dwFlags: DWORD;
  dwInfo: DWORD;

  wValue: SmallInt;
  pDevice: XTL_LPDIRECTINPUTDEVICE8;
  JoyState: DIJOYSTATE;
  pdwAxis: PLongInt;
  pbButton: PByte;
  KeyboardState: array [0..256-1] of BYTE;
  bKey: BYTE;
  MouseState: DIMOUSESTATE2;
{$WRITEABLECONST ON}
const
  lAccumX: LongInt = 0;
  lAccumY: LongInt = 0;
  lAccumZ: LongInt = 0;
{$WRITEABLECONST OFF}
begin
  if not Assigned(Controller) then
    Exit;

  pDevice := nil;
  // Never used : hRet := 0;
  // Never used : dwFlags := 0;

  // Default values necessary for axis
  Controller.Gamepad.sThumbLX := 0;
  Controller.Gamepad.sThumbLY := 0;
  Controller.Gamepad.sThumbRX := 0;
  Controller.Gamepad.sThumbRY := 0;

  // Poll all devices
  for v := Low(XBCtrlObject) to High(XBCtrlObject) do
  begin
    dwDevice := m_ObjectConfig[v].dwDevice;
    dwFlags := m_ObjectConfig[v].dwFlags;
    dwInfo := m_ObjectConfig[v].dwInfo;

    if (Integer(dwDevice) = -1) then
      Continue;

    pDevice := m_InputDevice[dwDevice].m_Device;

    hRet := pDevice.Poll();

    if FAILED(hRet) then
    begin
      hRet := pDevice.Acquire();

      while hRet = DIERR_INPUTLOST do
        hRet := pDevice.Acquire();
    end;

    wValue := 0;

    // Interpret PC Joystick Input
    if (dwFlags and DEVICE_FLAG_JOYSTICK) > 0 then
    begin
      if (pDevice.GetDeviceState(SizeOf(JoyState), @JoyState) <> DI_OK) then
        Continue;

      if (dwFlags and DEVICE_FLAG_AXIS) > 0 then
      begin
        pdwAxis := PLongInt(UIntPtr(@JoyState) + dwInfo);
        wValue := SmallInt(pdwAxis^);

        if (dwFlags and DEVICE_FLAG_NEGATIVE) > 0 then
        begin
          if (wValue < 0) then
            wValue := Abs(wValue+1)
          else
            wValue := 0;
        end
        else if (dwFlags and DEVICE_FLAG_POSITIVE) > 0 then
        begin
          if (wValue < 0) then
            wValue := 0;
        end;
      end
      else if (dwFlags and DEVICE_FLAG_BUTTON) > 0 then
      begin
        pbButton := PByte(UIntPtr(@JoyState) + dwInfo);

        if (pbButton^ and $80) > 0 then
          wValue := 32767
        else
          wValue := 0;
      end;
    end
    // Interpret PC KeyBoard Input
    else if (dwFlags and DEVICE_FLAG_KEYBOARD) > 0 then
    begin
      if (pDevice.GetDeviceState(SizeOf(KeyboardState), @KeyboardState) <> DI_OK) then
        Continue;

      bKey := KeyboardState[dwInfo];

      if (bKey and $80) > 0 then
        wValue := 32767
      else
        wValue := 0;
    end
    // Interpret PC Mouse Input
    else if (dwFlags and DEVICE_FLAG_MOUSE) > 0 then
    begin
      if (pDevice.GetDeviceState(SizeOf(MouseState), @MouseState) <> DI_OK) then
        Continue;

      if (dwFlags and DEVICE_FLAG_MOUSE_CLICK) > 0 then
      begin
        if (MouseState.rgbButtons[dwInfo] and $80) > 0 then
          wValue := 32767
        else
          wValue := 0;
      end
      else if (dwFlags and DEVICE_FLAG_AXIS) > 0 then
      begin
        Inc(lAccumX, MouseState.lX * 300);
        Inc(lAccumY, MouseState.lY * 300);
        Inc(lAccumZ, MouseState.lZ * 300);

        if (lAccumX > 32767) then
          lAccumX := 32767
        else if (lAccumX < -32768) then
          lAccumX := -32768;

        if (lAccumY > 32767) then
          lAccumY := 32767
        else if (lAccumY < -32768) then
          lAccumY := -32768;

        if (lAccumZ > 32767) then
          lAccumZ := 32767
        else if (lAccumZ < -32768) then
          lAccumZ := -32768;

        if (Integer(dwInfo) = FIELD_OFFSET(PDIMOUSESTATE(nil).lX)) then
          wValue := WORD(lAccumX)
        else if (Integer(dwInfo) = FIELD_OFFSET(PDIMOUSESTATE(nil).lY)) then
          wValue := WORD(lAccumY)
        else if (Integer(dwInfo) = FIELD_OFFSET(PDIMOUSESTATE(nil).lZ)) then
          wValue := WORD(lAccumZ);

        if (dwFlags and DEVICE_FLAG_NEGATIVE) > 0 then
        begin
          if (wValue < 0) then
            wValue := Abs(wValue + 1)
          else
            wValue := 0;
        end
        else if (dwFlags and DEVICE_FLAG_POSITIVE) > 0 then
        begin
          if (wValue < 0) then
            wValue := 0;
        end;
      end;
    end;
    
    // Map Xbox Joystick Input
    if (v >= XBCTRL_OBJECT_LTHUMBPOSX) and (v <= XBCTRL_OBJECT_RTHUMB) then
    begin
      case (v) of
        XBCTRL_OBJECT_LTHUMBPOSY:
          Controller.Gamepad.sThumbLY := Controller.Gamepad.sThumbLY + wValue;
        XBCTRL_OBJECT_LTHUMBNEGY:
          Controller.Gamepad.sThumbLY := Controller.Gamepad.sThumbLY - wValue;
        XBCTRL_OBJECT_RTHUMBPOSY:
          Controller.Gamepad.sThumbRY := Controller.Gamepad.sThumbRY + wValue;
        XBCTRL_OBJECT_RTHUMBNEGY:
          Controller.Gamepad.sThumbRY := Controller.Gamepad.sThumbRY - wValue;
        XBCTRL_OBJECT_LTHUMBPOSX:
          Controller.Gamepad.sThumbLX := Controller.Gamepad.sThumbLX + wValue;
        XBCTRL_OBJECT_LTHUMBNEGX:
          Controller.Gamepad.sThumbLX := Controller.Gamepad.sThumbLX - wValue;
        XBCTRL_OBJECT_RTHUMBPOSX:
          Controller.Gamepad.sThumbRX := Controller.Gamepad.sThumbRX + wValue;
        XBCTRL_OBJECT_RTHUMBNEGX:
          Controller.Gamepad.sThumbRX := Controller.Gamepad.sThumbRX - wValue;
        XBCTRL_OBJECT_A:
          Controller.Gamepad.bAnalogButtons[XINPUT_GAMEPAD_A] := (wValue div 128);
        XBCTRL_OBJECT_B:
          Controller.Gamepad.bAnalogButtons[XINPUT_GAMEPAD_B] := (wValue div 128);
        XBCTRL_OBJECT_X:
          Controller.Gamepad.bAnalogButtons[XINPUT_GAMEPAD_X] := (wValue div 128);
        XBCTRL_OBJECT_Y:
          Controller.Gamepad.bAnalogButtons[XINPUT_GAMEPAD_Y] := (wValue div 128);
        XBCTRL_OBJECT_WHITE:
          Controller.Gamepad.bAnalogButtons[XINPUT_GAMEPAD_WHITE] := (wValue div 128);
        XBCTRL_OBJECT_BLACK:
          Controller.Gamepad.bAnalogButtons[XINPUT_GAMEPAD_BLACK] := (wValue div 128);
        XBCTRL_OBJECT_LTRIGGER:
          Controller.Gamepad.bAnalogButtons[XINPUT_GAMEPAD_LEFT_TRIGGER] := (wValue div 128);
        XBCTRL_OBJECT_RTRIGGER:
          Controller.Gamepad.bAnalogButtons[XINPUT_GAMEPAD_RIGHT_TRIGGER] := (wValue div 128);
        XBCTRL_OBJECT_DPADUP:
          if (wValue > 0) then
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons or XINPUT_GAMEPAD_DPAD_UP
          else
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_UP;
        XBCTRL_OBJECT_DPADDOWN:
          if (wValue > 0) then
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons or XINPUT_GAMEPAD_DPAD_DOWN
          else
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_DOWN;
        XBCTRL_OBJECT_DPADLEFT:
          if (wValue > 0) then
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons or XINPUT_GAMEPAD_DPAD_LEFT
          else
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_LEFT;
        XBCTRL_OBJECT_DPADRIGHT:
          if (wValue > 0) then
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons or XINPUT_GAMEPAD_DPAD_RIGHT
          else
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_RIGHT;
        XBCTRL_OBJECT_BACK:
          if (wValue > 0) then
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons or XINPUT_GAMEPAD_BACK
          else
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons and XINPUT_GAMEPAD_BACK;
        XBCTRL_OBJECT_START:
          if (wValue > 0) then
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons or XINPUT_GAMEPAD_START
          else
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons and XINPUT_GAMEPAD_START;
        XBCTRL_OBJECT_LTHUMB:
          if (wValue > 0) then
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons or XINPUT_GAMEPAD_LEFT_THUMB
          else
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons and XINPUT_GAMEPAD_LEFT_THUMB;
        XBCTRL_OBJECT_RTHUMB:
          if (wValue > 0) then
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons or XINPUT_GAMEPAD_RIGHT_THUMB
          else
            Controller.Gamepad.wButtons := Controller.Gamepad.wButtons and XINPUT_GAMEPAD_RIGHT_THUMB;
      end;
    end;
  end;
end;

procedure XBController.ConfigBegin(ahwnd: Handle; aObject: XBCtrlObject);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  if m_CurrentState <> XBCTRL_STATE_NONE then
  begin
    Self{:Error}.SetError('Invalid State', False);
    Exit;
  end;

  m_CurrentState := XBCTRL_STATE_CONFIG;

  DInputInit(ahwnd);

  if Self{:Error}.GetError <> '' then
    Exit;

  lPrevMouseX := -1;
  lPrevMouseY := -1;
  lPrevMouseZ := -1;

  CurConfigObject := aobject;
end;

procedure XBController.ConfigEnd;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  if m_CurrentState <> XBCTRL_STATE_CONFIG then
  begin
    Self{:Error}.SetError('Invalid State', False);
    Exit;
  end;

  DInputCleanup();
  m_CurrentState := XBCTRL_STATE_NONE;
end;

function XBController.ConfigPoll(szStatus: PAnsiChar): LongBool;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  DeviceInstance: DIDEVICEINSTANCE;
  ObjectInstance: DIDEVICEOBJECTINSTANCE;
  v: Integer;
  hRet: HRESULT;
  dwHow: Int32; // was DWord but needed to be signed !
  dwFlags: Int32; // was DWord but needed to be signed !
  JoyState: DIJOYSTATE;
  KeyState: DIKEYSTATE;
  b: Integer;
  szDirection: string;
  szObjName: PAnsiChar;
  MouseState: DIMOUSESTATE2;

  lAbsDeltaX, lAbsDeltaY, lAbsDeltaZ: LONG;
  lDeltaX, lDeltaY, lDeltaZ: LONG;
  lMax: LONG;
begin
  if (m_CurrentState <> XBCTRL_STATE_CONFIG) then
  begin
    Self{:Error}.SetError('Invalid State', False);
    Result := False;
    Exit;
  end;

  DeviceInstance.dwSize := SizeOf(DIDEVICEINSTANCE);
  ObjectInstance.dwSize := SizeOf(DIDEVICEOBJECTINSTANCE);

  // Monitor for significant device state changes
  for v := m_dwInputDeviceCount - 1 downto 0 do
  begin
    // Poll the current device
    begin
      hRet := m_InputDevice[v].m_Device.Poll();
      if (FAILED(hRet)) then
      begin
        hRet := m_InputDevice[v].m_Device.Acquire();
        while (hRet = DIERR_INPUTLOST) do
          hRet := m_InputDevice[v].m_Device.Acquire();
      end;
    end;

    dwHow := -1;
    // Never used : dwFlags := m_InputDevice[v].m_Flags;

    // Detect Joystick Input
    if (m_InputDevice[v].m_Flags and DEVICE_FLAG_JOYSTICK) > 0 then
    begin
      // Get Joystick State
      begin
        hRet := m_InputDevice[v].m_Device.GetDeviceState(SizeOf(DIJOYSTATE), @JoyState);
        if FAILED(hRet) then
          Continue;
      end;

      dwFlags := DEVICE_FLAG_JOYSTICK;
      if (Abs(JoyState.lX) > DETECT_SENSITIVITY_JOYSTICK) then
      begin
        dwHow := FIELD_OFFSET(PDIJOYSTATE(nil).lX);
        dwFlags := dwFlags or iif(JoyState.lX > 0, (DEVICE_FLAG_AXIS or DEVICE_FLAG_POSITIVE), (DEVICE_FLAG_AXIS or DEVICE_FLAG_NEGATIVE));
      end
      else if (Abs(JoyState.lY) > DETECT_SENSITIVITY_JOYSTICK) then
      begin
        dwHow := FIELD_OFFSET(PDIJOYSTATE(nil).lY);
        dwFlags := dwFlags or iif(JoyState.lY > 0, (DEVICE_FLAG_AXIS or DEVICE_FLAG_POSITIVE), (DEVICE_FLAG_AXIS or DEVICE_FLAG_NEGATIVE));
      end
      else if (Abs(JoyState.lZ) > DETECT_SENSITIVITY_JOYSTICK) then
      begin
        dwHow := FIELD_OFFSET(PDIJOYSTATE(nil).lZ);
        dwFlags := dwFlags or iif(JoyState.lZ > 0, (DEVICE_FLAG_AXIS or DEVICE_FLAG_POSITIVE), (DEVICE_FLAG_AXIS or DEVICE_FLAG_NEGATIVE));
      end
      else if (Abs(JoyState.lRx) > DETECT_SENSITIVITY_JOYSTICK) then
      begin
        dwHow := FIELD_OFFSET(PDIJOYSTATE(nil).lRx);
        dwFlags := dwFlags or iif(JoyState.lRx > 0, (DEVICE_FLAG_AXIS or DEVICE_FLAG_POSITIVE), (DEVICE_FLAG_AXIS or DEVICE_FLAG_NEGATIVE));
      end
      else if (Abs(JoyState.lRy) > DETECT_SENSITIVITY_JOYSTICK) then
      begin
        dwHow := FIELD_OFFSET(PDIJOYSTATE(nil).lRy);
        dwFlags := dwFlags or iif(JoyState.lRy > 0, (DEVICE_FLAG_AXIS or DEVICE_FLAG_POSITIVE), (DEVICE_FLAG_AXIS or DEVICE_FLAG_NEGATIVE));
      end
      else if (Abs(JoyState.lRz) > DETECT_SENSITIVITY_JOYSTICK) then
      begin
        dwHow := FIELD_OFFSET(PDIJOYSTATE(nil).lRz);
        dwFlags := dwFlags or iif(JoyState.lRz > 0, (DEVICE_FLAG_AXIS or DEVICE_FLAG_POSITIVE), (DEVICE_FLAG_AXIS or DEVICE_FLAG_NEGATIVE));
      end
      else
      begin
        for b := 0 to 2 - 1 do
        begin
          if (Abs(JoyState.rglSlider[b]) > DETECT_SENSITIVITY_JOYSTICK) then
          begin
            dwHow := FIELD_OFFSET(PDIJOYSTATE(nil).rglSlider[b]);
            dwFlags := dwFlags or iif(JoyState.rglSlider[b] > 0, (DEVICE_FLAG_AXIS or DEVICE_FLAG_POSITIVE), (DEVICE_FLAG_AXIS or DEVICE_FLAG_NEGATIVE));
          end;
        end;
      end;

      (*/* Cxbx : temporarily disabled
      if (dwHow == -1)
      {
          for(int b=0;b<4;b++)
          {
              if (Abs(JoyState.rgdwPOV[b]) > DETECT_SENSITIVITY_POV)
              {
                  dwHow = FIELD_OFFSET(PDIJOYSTATE(nil).rgdwPOV[b]);
              }
          }
      }
      //*/ *)

      if (dwHow = -1) then
      begin
        for b := 0 to 32-1 do
        begin
          if (JoyState.rgbButtons[b] > DETECT_SENSITIVITY_BUTTON) then
          begin
            dwHow := FIELD_OFFSET(PDIJOYSTATE(nil).rgbButtons[b]);
            dwFlags := dwFlags or DEVICE_FLAG_BUTTON;
          end;
        end;
      end;

      // Retrieve Object Info
      if (dwHow <> -1) then
      begin
        szDirection := iif((dwFlags and DEVICE_FLAG_AXIS) > 0, iif((dwFlags and DEVICE_FLAG_POSITIVE) > 0, 'Positive ', 'Negative '), '');

        m_InputDevice[v].m_Device.GetDeviceInfo({var}DeviceInstance);

        m_InputDevice[v].m_Device.GetObjectInfo({var}ObjectInstance, dwHow, DIPH_BYOFFSET);

        Map(CurConfigObject, DeviceInstance.tszInstanceName, dwHow, dwFlags);

        DbgPrintf('Dxbx: Detected %s%s on %s', [szDirection, ObjectInstance.tszName, DeviceInstance.tszInstanceName, ObjectInstance.dwType]);

        DbgPrintf('Success: %s Mapped to "%s%s" on "%s"!', [m_DeviceNameLookup[Ord(CurConfigObject)], szDirection, ObjectInstance.tszName, DeviceInstance.tszInstanceName]);

        Result := True;
        Exit;
      end;
    end

    // Detect Keyboard Input
    else if (m_InputDevice[v].m_Flags and DEVICE_FLAG_KEYBOARD) > 0 then
    begin
      m_InputDevice[v].m_Device.GetDeviceState(SizeOf(DIKEYSTATE), @KeyState);

      dwFlags := DEVICE_FLAG_KEYBOARD;

      // Check for Keyboard State Change
      for b := 0 to 256 - 1 do
      begin
        if (KeyState[b] <> 0) then
        begin
          dwHow := b;
          Break;
        end;
      end;

      // Check for Success
      if (dwHow <> -1) then
      begin
        Map(CurConfigObject, 'SysKeyboard', dwHow, dwFlags);
        DbgPrintf('Dxbx: Detected Key %d on SysKeyboard', [dwHow]);
        DbgPrintf('Success: %s Mapped to Key %d on SysKeyboard', [m_DeviceNameLookup[Ord(CurConfigObject)], dwHow]);
        Result := True;
        Exit;
      end;
    end

    // Detect Mouse Input
    else if (m_InputDevice[v].m_Flags and DEVICE_FLAG_MOUSE) > 0 then
    begin
      m_InputDevice[v].m_Device.GetDeviceState(SizeOf(MouseState), @MouseState);

      dwFlags := DEVICE_FLAG_MOUSE;

      // Detect Button State Change
      for b := 0 to 4 - 1 do
      begin
        // 0x80 is the mask for button push
        if (MouseState.rgbButtons[b] and $80) > 0 then
        begin
          dwHow := b;
          dwFlags := dwFlags or DEVICE_FLAG_MOUSE_CLICK;
          Break;
        end;
      end;

      // Check for Success
      if (dwHow <> -1) then
      begin
        Map(CurConfigObject, 'SysMouse', dwHow, dwFlags);

        DbgPrintf('Dxbx: Detected Button %d on SysMouse', [dwHow]);

        DbgPrintf('Success: %s Mapped to Button %d on SysMouse', [m_DeviceNameLookup[Ord(CurConfigObject)], dwHow]);

        Result := True;
        Exit;
      end
      // Check for Mouse Movement
      else
      begin
        lAbsDeltaX := 0; lAbsDeltaY := 0; lAbsDeltaZ := 0;
        // Never used : lDeltaX := 0; lDeltaY := 0; lDeltaZ := 0;

        if (lPrevMouseX = -1)
        or (lPrevMouseY = -1)
        or (lPrevMouseZ = -1) then
        begin
          lDeltaX := 0;
          lDeltaY := 0;
          lDeltaZ := 0;
        end
        else
        begin
          lDeltaX := MouseState.lX - lPrevMouseX;
          lDeltaY := MouseState.lY - lPrevMouseY;
          lDeltaZ := MouseState.lZ - lPrevMouseZ;

          lAbsDeltaX := Abs(lDeltaX);
          lAbsDeltaY := Abs(lDeltaY);
          lAbsDeltaZ := Abs(lDeltaZ);
        end;

        lMax := iif(lAbsDeltaX > lAbsDeltaY, lAbsDeltaX, lAbsDeltaY);

        if (lAbsDeltaZ > lMax) then
          lMax := lAbsDeltaZ;

        lPrevMouseX := MouseState.lX;
        lPrevMouseY := MouseState.lY;
        lPrevMouseZ := MouseState.lZ;

        if (lMax > DETECT_SENSITIVITY_MOUSE) then
        begin
          dwFlags := dwFlags or DEVICE_FLAG_AXIS;

          if (lMax = lAbsDeltaX) then
          begin
            dwHow := FIELD_OFFSET(PDIMOUSESTATE(nil).lX);
            dwFlags := dwFlags or iif(lDeltaX > 0, DEVICE_FLAG_POSITIVE, DEVICE_FLAG_NEGATIVE);
          end
          else if (lMax = lAbsDeltaY) then
          begin
            dwHow := FIELD_OFFSET(PDIMOUSESTATE(nil).lY);
            dwFlags := dwFlags or iif(lDeltaY > 0, DEVICE_FLAG_POSITIVE, DEVICE_FLAG_NEGATIVE);
          end
          else if (lMax = lAbsDeltaZ) then
          begin
            dwHow := FIELD_OFFSET(PDIMOUSESTATE(nil).lZ);
            dwFlags := dwFlags or iif(lDeltaZ > 0, DEVICE_FLAG_POSITIVE, DEVICE_FLAG_NEGATIVE);
          end;
        end;

        // Check for Success
        if (dwHow <> -1) then
        begin
          szDirection := iif((dwFlags and DEVICE_FLAG_POSITIVE) > 0, 'Positive', 'Negative');
          szObjName := 'Unknown';

          ObjectInstance.dwSize := SizeOf(ObjectInstance);

          if (m_InputDevice[v].m_Device.GetObjectInfo({var}ObjectInstance, dwHow, DIPH_BYOFFSET) = DI_OK) then
            szObjName := ObjectInstance.tszName;

          Map(CurConfigObject, 'SysMouse', dwHow, dwFlags);

          DbgPrintf('Dxbx: Detected Movement on the %s%s on SysMouse', [szDirection, szObjName]);

          DbgPrintf('Success: %s Mapped to %s%s on SysMouse', [m_DeviceNameLookup[Ord(CurConfigObject)], szDirection, szObjName]);

          Result := True;
          Exit;
        end;
      end;    
    end;
  end;

  Result := False;
end;


function XBController.DeviceIsUsed(szDeviceName: PAnsiChar): LongBool;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  v: Integer;
begin
  Result := False;
  for v := 0 to XBCTRL_MAX_DEVICES - 1 do
  begin
    if (strcomp(m_DeviceName[v], szDeviceName) = 0) then // was AnsiCompareStr
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure XBController.DInputCleanup;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  v: Integer;
begin
  for v := m_dwInputDeviceCount downto 0 do
  begin
    m_InputDevice[v].m_Device.Unacquire();
    m_InputDevice[v].m_Device._Release(); //?? Release();
    m_InputDevice[v].m_Device := nil;
  end;

  m_dwInputDeviceCount := 0;
  if Assigned(m_pDirectInput8) then
  begin
    m_pDirectInput8._Release();//?? Release();
    m_pDirectInput8 := nil;
  end;
end;

procedure XBController.DInputInit(ahwnd: Handle);
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  ahRet: HResult;
  v: Integer;
begin
  m_dwInputDeviceCount := 0;

  // Create DirectInput Object
  begin
    ahRet := DirectInput8Create(GetModuleHandle(nil),
      DIRECTINPUT_VERSION,
      IID_IDirectInput8,
      {out}m_pDirectInput8,
      nil);

    if (FAILED(ahRet)) then
    begin
      Self{:Error}.SetError('Could not initialized DirectInput8', True);
      Exit;
    end;
  end;

  // Create all the devices available (well...most of them)
  if Assigned(m_pDirectInput8) then
  begin
    {ahRet :=} m_pDirectInput8.EnumDevices(DI8DEVCLASS_GAMECTRL,
                                           WrapEnumGameCtrlCallback,
                                           Addr(Self),
                                           DIEDFL_ATTACHEDONLY);
    // Dxbx TODO Add : if FAILED(hret) then what?

    if (m_CurrentState = XBCTRL_STATE_CONFIG) or DeviceIsUsed('SysKeyboard') then
    begin
      ahRet := m_pDirectInput8.CreateDevice(
        {rguid}GUID_SysKeyboard,
        {out}IDirectInputDevice8A(m_InputDevice[m_dwInputDeviceCount].m_Device),
        {pUnkOuter=}nil);

      if (not FAILED(ahRet)) then
      begin
        m_InputDevice[m_dwInputDeviceCount].m_Flags := DEVICE_FLAG_KEYBOARD;
        m_InputDevice[m_dwInputDeviceCount].m_Device.SetDataFormat(c_dfDIKeyboard);
        Inc(m_dwInputDeviceCount);
      end;

      if (m_CurrentState = XBCTRL_STATE_LISTEN) then
        ReorderObjects('SysKeyboard', m_dwInputDeviceCount - 1);
    end;

    if (m_CurrentState = XBCTRL_STATE_CONFIG) or DeviceIsUsed('SysMouse') then
    begin
      ahRet := m_pDirectInput8.CreateDevice(
        GUID_SysMouse,
        {out}IDirectInputDevice8A(m_InputDevice[m_dwInputDeviceCount].m_Device),
        nil);

      if (not FAILED(ahRet)) then
      begin
        m_InputDevice[m_dwInputDeviceCount].m_Flags := DEVICE_FLAG_MOUSE;
        m_InputDevice[m_dwInputDeviceCount].m_Device.SetDataFormat(c_dfDIMouse2);
        Inc(m_dwInputDeviceCount);
      end;

      if (m_CurrentState = XBCTRL_STATE_LISTEN) then
        ReorderObjects('SysMouse', m_dwInputDeviceCount - 1);
    end;
  end;

  // Enumerate Controller objects
  // Dxbx : for loop not possible here :
  m_dwCurObject := 0;
  while m_dwCurObject < m_dwInputDeviceCount do
  begin
    m_InputDevice[m_dwCurObject].m_Device.EnumObjects(WrapEnumObjectsCallback, Addr(Self), DIDFT_ALL);
    Inc(m_dwCurObject);
  end;


  // Set cooperative level and acquire
  begin
    for v := 0 to m_dwInputDeviceCount - 1 do
    begin
      m_InputDevice[v].m_Device.SetCooperativeLevel(ahwnd, DISCL_NONEXCLUSIVE or DISCL_FOREGROUND);
      m_InputDevice[v].m_Device.Acquire();

      ahRet := m_InputDevice[v].m_Device.Poll();

      if (FAILED(ahRet)) then
      begin
        ahRet := m_InputDevice[v].m_Device.Acquire();

        while (ahRet = DIERR_INPUTLOST) do
          ahRet := m_InputDevice[v].m_Device.Acquire();

        if (ahRet <> DIERR_INPUTLOST) then
          Break;
      end;
    end;
  end;
end;

function XBController.Insert(szDeviceName: PAnsiChar): Integer;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  v: Integer;
begin
  for v := 0 to XBCTRL_MAX_DEVICES - 1 do
    if (StrComp(m_DeviceName[v], szDeviceName) = 0) then
    begin
      Result := v;
      Exit;
    end;

  for v := 0 to XBCTRL_MAX_DEVICES - 1 do
  begin
    if (m_DeviceName[v][0] = #0) then
    begin
      strcpy(m_DeviceName[v], szDeviceName);
      Result := v;
      Exit;
    end;
  end;

  MessageBox(0, 'Unexpected Circumstance (Too Many Controller Devices)! Please contact caustik!', 'Cxbx', MB_OK or MB_ICONEXCLAMATION);

  ExitProcess(1);
  Result := 0;
end;

procedure XBController.ListenBegin(ahwnd: Handle);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  v: Integer;
begin
  if m_CurrentState <> XBCTRL_STATE_NONE then
  begin
    Self{:Error}.SetError('Invalid State', False);
    Exit;
  end;

  m_CurrentState := XBCTRL_STATE_LISTEN;

  DInputInit(ahwnd);

  for v := XBCTRL_MAX_DEVICES - 1 downto m_dwInputDeviceCount do
    m_DeviceName[v][0] := #0;

  for v := 0 to XBCTRL_OBJECT_COUNT - 1 do
  begin
    if m_ObjectConfig[XBCtrlObject(v)].dwDevice >= m_dwInputDeviceCount then
    begin
      DbgPrintf('Warning: Device Mapped to %s was not found!', [m_DeviceNameLookup[v]]);
      m_ObjectConfig[XBCtrlObject(v)].dwDevice := -1;
    end;
  end;
end;

procedure XBController.ListenEnd;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  if m_CurrentState <> XBCTRL_STATE_LISTEN then
  begin
    Self{:Error}.SetError('Invalid State', False);
    Exit;
  end;

  DInputCleanup();
  m_CurrentState := XBCTRL_STATE_NONE;
end;

procedure XBController.Load(szRegistryKey: PAnsiChar);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  dwType, dwSize: DWord;
  dwDisposition: DWord;
  ahKey: HKEY;
  v: Integer;
  szValueName: string;
begin
  if m_CurrentState <> XBCTRL_STATE_NONE then
  begin
    Self{:Error}.SetError('Invalid State', False);
    Exit;
  end;

  // Load Configuration from Registry
  if (RegCreateKeyExA(HKEY_CURRENT_USER, szRegistryKey, 0, nil, REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE, nil, ahKey, @dwDisposition) = ERROR_SUCCESS) then
  begin
    // Load Device Names
    for v := 0 to XBCTRL_MAX_DEVICES - 1 do
    begin
      // default is a null string
      m_DeviceName[v][0] := #0;
      szValueName := DxbxFormat('DeviceName 0x%.02X', [v]); // was sprintf
      dwType := REG_SZ;
      dwSize := 260;
      RegQueryValueEx(ahKey, PChar(szValueName), nil, @dwType, PByte(@(m_DeviceName[v])), @dwSize);
    end;

    // Load Object Configuration
    for v := 0 to XBCTRL_OBJECT_COUNT - 1 do
    begin
      // default object configuration
      m_ObjectConfig[XBCtrlObject(v)].dwDevice := -1;
      m_ObjectConfig[XBCtrlObject(v)].dwInfo := -1;
      m_ObjectConfig[XBCtrlObject(v)].dwFlags := 0;
      szValueName := DxbxFormat('Object : %s', [m_DeviceNameLookup[v]]); // was sprintf
      dwType := REG_BINARY;
      dwSize := SizeOf(XBCtrlObjectCfg);
      RegQueryValueEx(ahKey, PChar(szValueName), nil, @dwType, @m_ObjectConfig[XBCtrlObject(v)], @dwSize);
    end;

    RegCloseKey(ahKey);
  end;
end;

procedure XBController.Map(aobject: XBCtrlObject; szDeviceName: PAnsiChar; dwInfo, dwFlags: Integer);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  v: Integer;
  r: XBCtrlObject;
  InUse: Boolean;
begin
  // Initialize InputMapping instance
  m_ObjectConfig[aobject].dwDevice := Insert(szDeviceName);
  m_ObjectConfig[aobject].dwInfo := dwInfo;
  m_ObjectConfig[aobject].dwFlags := dwFlags;

  // Purge unused device slots
  for v := 0 to XBCTRL_MAX_DEVICES - 1 do
  begin
    InUse := False;

    for r := Low(XBCtrlObject) to High(XBCtrlObject) do
      if m_ObjectConfig[r].dwDevice = v then
        InUse := True;

    if not InUse then
      m_DeviceName[v][0] := #0;
  end;
end;

procedure XBController.ReorderObjects(szDeviceName: PAnsiChar; aPos: Integer);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  Old: Integer;
  v: Integer;
begin
  Old := -1;

  // locate Old device name position
  for v := 0 to XBCTRL_MAX_DEVICES - 1 do
  begin
    if (StrComp(m_DeviceName[v], szDeviceName) = 0) then
    begin
      Old := v;
      Break;
    end;
  end;

  // Swap names, if necessary
  if Old <> aPos then
  begin
    StrCopy(m_DeviceName[Old], m_DeviceName[aPos]);
    StrCopy(m_DeviceName[aPos], szDeviceName);
  end;

  // Update all Old values
  for v := 0 to XBCTRL_OBJECT_COUNT - 1 do
  begin
    if m_ObjectConfig[XBCtrlObject(v)].dwDevice = Old then
      m_ObjectConfig[XBCtrlObject(v)].dwDevice := aPos
    else
      if m_ObjectConfig[XBCtrlObject(v)].dwDevice = aPos then
        m_ObjectConfig[XBCtrlObject(v)].dwDevice := Old;
  end;
end;

procedure XBController.Save(szRegistryKey: PAnsiChar);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  dwType, dwSize: DWord;
  dwDisposition: DWord;
  ahKey: HKEY;
  v: Integer;
  szValueName: array[0..64 - 1] of Char;
begin
  if (m_CurrentState <> XBCTRL_STATE_NONE) then
  begin
    Self{:Error}.SetError('Invalid State', False);
    Exit;
  end;

  // Save Configuration to Registry
  if (RegCreateKeyExA(HKEY_CURRENT_USER, szRegistryKey, 0, nil, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, nil, ahKey, @dwDisposition) = ERROR_SUCCESS) then
  begin
    // Save Device Names
    for v := 0 to XBCTRL_MAX_DEVICES - 1 do
    begin
      StrFmt(szValueName, 'DeviceName $%.02X', [v]); // was sprintf

      dwType := REG_SZ;
      dwSize := 260;

      if (m_DeviceName[v][0] = #0) then
        RegDeleteValue(ahKey, szValueName)
      else
        RegSetValueEx(ahKey, szValueName, 0, dwType, @m_DeviceName[v], dwSize);
    end;

    // Save Object Configuration
    for v := 0 to XBCTRL_OBJECT_COUNT - 1 do
    begin
      StrFmt(szValueName, 'Object : "%s"', [m_DeviceNameLookup[v]]); // was sprintf
      dwType := REG_BINARY;
      dwSize := SizeOf(XBCtrlObjectCfg);

        if (m_ObjectConfig[XBCtrlObject(v)].dwDevice <> -1) then
          RegSetValueEx(ahKey, szValueName, 0, dwType, @m_ObjectConfig[XBCtrlObject(v)], dwSize);
    end;

    RegCloseKey(ahKey);
  end;
end;

end.

