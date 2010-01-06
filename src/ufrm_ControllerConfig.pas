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
  StdCtrls, ExtCtrls, Tabs, ComCtrls
  // Dxbx
  , uXBController
  , uEmuShared
  ;

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
    sTabControl1: TTabControl;
    chkForceFeedback: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    g_XBController: XBController;
    g_bHasChanges: Boolean;
  end;


var
  frm_ControllerConfig: Tfrm_ControllerConfig;

implementation

{$R *.DFM}

procedure Tfrm_ControllerConfig.FormCreate(Sender: TObject);
begin
  // reset changes flag
  g_bHasChanges := False;

  // retrieve controller configuration
  if Assigned(g_EmuShared) then
    g_EmuShared.GetXBController(@g_XBController);

  // set window icon
//  SetClassLong(hWndDlg, GCL_HICON, (LONG)LoadIcon(GetModuleHandle(NULL), MAKEINTRESOURCE(IDI_CXBX)));

  // set default focus to X button
//  SetFocus(GetDlgItem(hWndDlg, IDC_SET_X));
end;

procedure Tfrm_ControllerConfig.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  // if changes have been made, check if the user wants to save them */
  if g_bHasChanges then
  begin
    case MessageBox(0, 'Do you wish to apply your changes?', 'Dxbx', MB_ICONQUESTION or MB_YESNOCANCEL) of
      IDYES:
        begin
//        PostMessage(hWndDlg, WM_COMMAND, IDC_INPUT_CONFIG_ACCEPT, 0);
          {var}CanClose := True;
        end;
      IDNO:
        begin
//        PostMessage(hWndDlg, WM_COMMAND, IDC_INPUT_CONFIG_CANCEL, 0);
          {var}CanClose := True;
        end;
    else // IDCANCEL:
      {var}CanClose := False;
    end;
  end
  else
    {var}CanClose := True;
end;

(*
INT_PTR CALLBACK DlgControllerConfigProc(HWND hWndDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
begin
  if uMsg <> WM_COMMAND then
    Exit;

  HWND hWndButton := GetDlgItem(hWndDlg, LOWORD(wParam));
  case LOWORD(wParam) of
    IDC_INPUT_CONFIG_CANCEL:
      EndDialog(hWndDlg, wParam);

    IDC_INPUT_CONFIG_ACCEPT:
    begin
      if Assigned(g_EmuShared) then
        g_EmuShared.SetXBController(@g_XBController);
      EndDialog(hWndDlg, wParam);
    end;

    IDC_SET_LEFT_POSY:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_LTHUMBPOSY);

    IDC_SET_LEFT_NEGY:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_LTHUMBNEGY);

    IDC_SET_LEFT_POSX:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_LTHUMBPOSX);

    IDC_SET_LEFT_NEGX:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_LTHUMBNEGX);

    IDC_SET_RIGHT_POSY:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_RTHUMBPOSY);

    IDC_SET_RIGHT_NEGY:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_RTHUMBNEGY);

    IDC_SET_RIGHT_POSX:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_RTHUMBPOSX);

    IDC_SET_RIGHT_NEGX:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_RTHUMBNEGX);

    IDC_SET_X:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_X);

    IDC_SET_Y:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_Y);

    IDC_SET_A:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_A);

    IDC_SET_B:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_B);

    IDC_SET_WHITE:
      CnfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_WHITE);

    IDC_SET_BLACK:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_BLACK);
                    
    IDC_SET_LTRIGGER:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_LTRIGGER);

    IDC_SET_RTRIGGER:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_RTRIGGER);

    IDC_SET_DPAD_UP:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_DPADUP);

    IDC_SET_DPAD_DOWN:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_DPADDOWN);

    IDC_SET_DPAD_LEFT:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_DPADLEFT);

    IDC_SET_DPAD_RIGHT:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_DPADRIGHT);

    IDC_SET_BACK:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_BACK);

    IDC_SET_START:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_START);

    IDC_SET_LTHUMB:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_LTHUMB);

    IDC_SET_RTHUMB:
      ConfigureInput(hWndDlg, hWndButton, XBCTRL_OBJECT_RTHUMB);

    IDC_CONFIGURE_ALL:
    begin
      int v=0;

      struct _ConfigObj
      begin
        int idcVal;
        XBCtrlObject ctrl;
      end;

      configObj[] =
      begin
        begin IDC_SET_X, XBCTRL_OBJECT_X end;,
        begin IDC_SET_Y, XBCTRL_OBJECT_Y end;,
        begin IDC_SET_A, XBCTRL_OBJECT_A end;,
        begin IDC_SET_B, XBCTRL_OBJECT_B end;,
        begin IDC_SET_WHITE, XBCTRL_OBJECT_WHITE end;,
        begin IDC_SET_BLACK, XBCTRL_OBJECT_BLACK end;,
        begin IDC_SET_LTRIGGER, XBCTRL_OBJECT_LTRIGGER end;,
        begin IDC_SET_RTRIGGER, XBCTRL_OBJECT_RTRIGGER end;,
        begin IDC_SET_DPAD_UP, XBCTRL_OBJECT_DPADUP end;,
        begin IDC_SET_DPAD_DOWN, XBCTRL_OBJECT_DPADDOWN end;,
        begin IDC_SET_DPAD_LEFT, XBCTRL_OBJECT_DPADLEFT end;,
        begin IDC_SET_DPAD_RIGHT, XBCTRL_OBJECT_DPADRIGHT end;,
        begin IDC_SET_BACK, XBCTRL_OBJECT_BACK end;,
        begin IDC_SET_START, XBCTRL_OBJECT_START end;,
        begin IDC_SET_LTHUMB, XBCTRL_OBJECT_LTHUMB end;,
        begin IDC_SET_RTHUMB, XBCTRL_OBJECT_RTHUMB end;,
        begin IDC_SET_LEFT_POSY, XBCTRL_OBJECT_LTHUMBPOSY end;,
        begin IDC_SET_LEFT_NEGY, XBCTRL_OBJECT_LTHUMBNEGY end;,
        begin IDC_SET_LEFT_NEGX, XBCTRL_OBJECT_LTHUMBNEGX end;,
        begin IDC_SET_LEFT_POSX, XBCTRL_OBJECT_LTHUMBPOSX end;,
        begin IDC_SET_RIGHT_POSY, XBCTRL_OBJECT_RTHUMBPOSY end;,
        begin IDC_SET_RIGHT_NEGY, XBCTRL_OBJECT_RTHUMBNEGY end;,
        begin IDC_SET_RIGHT_NEGX, XBCTRL_OBJECT_RTHUMBNEGX end;,
        begin IDC_SET_RIGHT_POSX, XBCTRL_OBJECT_RTHUMBPOSX end;,
      end;

      for(v=0;v<sizeof(configObj) / sizeof(_ConfigObj);v++) do
      begin
        Sleep(500);
        ConfigureInput(hWndDlg, GetDlgItem(hWndDlg, configObj[v].idcVal), configObj[v].ctrl);
      end;
    end;
  end;

  Result := False;
end;

var
  bConfigDone: Boolean = True;

procedure ConfigureInput(HWND hWndDlg, HWND hWndButton, XBCtrlObject object);
begin
  // ensure only one input is configured at a time
  if not bConfigDone then
    Exit;

  bConfigDone := False;

  g_bHasChanges := True;

  // disable all buttons
  EnableButtonWindows(hWndDlg, hWndButton, False);

  char szOrgText[32];
  char szNewText[255] := 'Recieved no user input, try again...';

  SetWindowText(GetDlgItem(hWndDlg, IDC_CONFIG_STATUS), 'Waiting for your input...');
  GetWindowText(hWndButton, szOrgText, 32);

  g_XBController.ConfigBegin(hWndDlg, object);

  // wait for input, or 5 second timeout
  for(int v=100;v>0;v--) do
  begin
    // update the button text every second
    if (v mod 20) = 0 then
    begin
      char szBuffer[255];

      sprintf(szBuffer, '%d', (v+19)/20);

      SetWindowText(hWndButton, szBuffer);
    end;

    if g_XBController.GetError() then
    goto cleanup;

    if g_XBController.ConfigPoll(szNewText) then
    Break;

    Sleep(50);
  end;

  if g_XBController.GetError() then
    goto cleanup
  else
    g_XBController.ConfigEnd();

cleanup:

  // enable all buttons
  EnableButtonWindows(hWndDlg, hWndButton, True);

  // update window with status
  begin
    if g_XBController.GetError() then
      sprintf(szNewText, '%s', g_XBController.GetError());

    SetWindowText(hWndButton, szOrgText);

    SetWindowText(GetDlgItem(hWndDlg, IDC_CONFIG_STATUS), szNewText);

    MSG Msg;

    while(PeekMessage(@Msg, hWndDlg, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE)) do
      ;
    while(PeekMessage(@Msg, hWndDlg, WM_KEYFIRST,   WM_KEYLAST,   PM_REMOVE)) do
      ;
  end;

  bConfigDone := True;
end;

procedure EnableButtonWindows(HWND hWndDlg, HWND hExclude, BOOL bEnable)
begin
  int v=0;

  // list of applicable child windows
  int itemList[] =
  begin
    IDC_SET_X, IDC_SET_Y, IDC_SET_A, IDC_SET_B,
    IDC_SET_WHITE, IDC_SET_BLACK,
    IDC_SET_LTHUMB, IDC_SET_RTHUMB,
    IDC_SET_DPAD_UP, IDC_SET_DPAD_DOWN, IDC_SET_DPAD_LEFT, IDC_SET_DPAD_RIGHT,
    IDC_SET_BACK, IDC_SET_START, IDC_SET_LTRIGGER, IDC_SET_RTRIGGER,
    IDC_SET_LEFT_POSY, IDC_SET_LEFT_NEGY, IDC_SET_LEFT_NEGX, IDC_SET_LEFT_POSX,
    IDC_SET_RIGHT_POSY, IDC_SET_RIGHT_NEGY, IDC_SET_RIGHT_NEGX, IDC_SET_RIGHT_POSX,
    IDC_INPUT_CONFIG_CANCEL, IDC_INPUT_CONFIG_ACCEPT,
    IDC_CONFIGURE_ALL
  end;;

  // enable / disable all the listed windows
  for(v=0;v<sizeof(itemList) / sizeof(int);v++) do
  begin
    HWND hWnd := GetDlgItem(hWndDlg, itemList[v]);

    if hWnd <> hExclude then
      EnableWindow(hWnd, bEnable);
  end;
end;
*)

end.
