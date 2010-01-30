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
unit ufrm_Main;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes, JwaWinType,
  Dialogs, ActnList, Forms, Menus,
  Controls, ComCtrls, ExtCtrls,
  Graphics, JPeg,
  ShellAPI, IniFiles,
  // Dxbx
  uTypes,
  uConsts,
  uLog,
  uWindows,
  uDxbxUtils,
  uXbe,
  uEmuShared,
  uDxbxXml,
  ufrm_ControllerConfig,
  ufrm_VideoConfig,
  ufrm_About;


type
  Tfrm_Main = class(TForm)
    MainMenu1: TMainMenu;
    mnu_File: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Edit1: TMenuItem;
    View1: TMenuItem;
    Settings1: TMenuItem;
    Emulation1: TMenuItem;
    Help1: TMenuItem;
    mnu_About: TMenuItem;
    mnu_OpenXbe: TMenuItem;
    mnu_CloseXbe: TMenuItem;
    N2: TMenuItem;
    mnu_RecentXbefiles: TMenuItem;
    mnu_Logbitmap: TMenuItem;
    mnu_Patch: TMenuItem;
    N3: TMenuItem;
    mnu_DumpxbeinfoTo: TMenuItem;
    mnu_DebugoutputGUI: TMenuItem;
    mnu_DebugoutputKernel: TMenuItem;
    mnu_ConfigControler: TMenuItem;
    mnu_ConfigAudio: TMenuItem;
    mnu_Configvideo: TMenuItem;
    Start1: TMenuItem;
    ActionList: TActionList;
    ActStartEmulation: TAction;
    actAbout: TAction;
    actConfigController: TAction;
    actConfigAudio: TAction;
    actConfigVideo: TAction;
    actClose: TAction;
    actOpenXbe: TAction;
    actCloseXbe: TAction;
    actSaveXbe: TAction;
    actSaveXbeAs: TAction;
    XbeOpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Image1: TImage;
    mnu_GuiOutputConsole: TMenuItem;
    mnu_DebugOutputGuiFile: TMenuItem;
    mnu_DebugKernelOutputConsole: TMenuItem;
    mnu_DebugOutputKernelFile: TMenuItem;
    Console3: TMenuItem;
    mnu_DumpXbeInfoToFile: TMenuItem;
    actConsoleXbeInfo: TAction;
    actFileXbeInfo: TAction;
    actConsoleDebugGui: TAction;
    actFileDebugGui: TAction;
    actConsoleDebugKernel: TAction;
    actFileDebugKernel: TAction;
    ExeOpenDialog: TOpenDialog;
    Import1: TMenuItem;
    mnu_ExportLogoBitmap: TMenuItem;
    actImportLogo: TAction;
    actExportLogo: TAction;
    LogoSaveDialog: TSaveDialog;
    ExeSaveDialog: TSaveDialog;
    ools1: TMenuItem;
    XdkTracker1: TMenuItem;
    Iso1: TMenuItem;
    N5: TMenuItem;
    xIso1: TMenuItem;
    actXdkTracker: TAction;
    actXIso: TAction;
    N8: TMenuItem;
    XDKTracker2: TMenuItem;
    actXdkTrackerXbeInfo: TAction;
    StatusBar: TStatusBar;
    ImageIcon: TImage;
    ImageLogo: TImage;
    procedure ActStartEmulationExecute(Sender: TObject);
    procedure actOpenXbeExecute(Sender: TObject);
    procedure actCloseXbeExecute(Sender: TObject);
    procedure actConfigControllerExecute(Sender: TObject);
    procedure actConfigVideoExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actConsoleXbeInfoExecute(Sender: TObject);
    procedure actFileXbeInfoExecute(Sender: TObject);
    procedure actConsoleDebugGuiExecute(Sender: TObject);
    procedure actFileDebugGuiExecute(Sender: TObject);
    procedure actConsoleDebugKernelExecute(Sender: TObject);
    procedure actFileDebugKernelExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actExportLogoExecute(Sender: TObject);
    procedure actXdkTrackerExecute(Sender: TObject);
    procedure actXIsoExecute(Sender: TObject);
    procedure actXdkTrackerXbeInfoExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    m_Xbe: TXbe;

    Emulation_State: EMU_STATE;

    OldLBWindowProc: TWndMethod;

    procedure CloseXbe;

    procedure ReadSettingsIni;
    procedure WriteSettingsIni;

    procedure RecentXbeAdd(aFileName: string);
    procedure ReopenXbe(Sender: TObject);

    function SendCommandToXdkTracker: Boolean;

    procedure WMDROPFILES(var Msg: TMessage); //message WM_DROPFILES;
    procedure LBWindowProc(var Message: TMessage);

    procedure AddjustMenu;
    procedure UpdateIcon(const aXbe: TXBE);
  public
    FApplicationDir: string;

    destructor Destroy; override;
  end;

var
  KernelDebugMode: TDebugMode = dmFile;
  KernelDebugFileName: string = '';

  frm_Main: Tfrm_Main;

implementation

{ Tfrm_Main }

{$R *.DFM}

procedure LaunchXBE(const aXbe: TXbe; const aHandle: THandle);
begin
  ShellExecute(0, 'open', PChar(ParamStr(0)), PChar(
    '/load ' +
    AnsiQuotedStr(aXbe.XbePath, '"') + ' ' +
    IntToStr(aHandle) + ' ' +
    IntToStr(Ord(KernelDebugMode)) + ' ' +
    KernelDebugFileName
    ), nil, SW_SHOWNORMAL);
end;

procedure Tfrm_Main.actOpenXbeExecute(Sender: TObject);
begin
  XbeOpenDialog.Filter := DIALOG_FILTER_XBE;
  if not XbeOpenDialog.Execute then
    Exit;

  if Assigned(m_Xbe) then
    CloseXbe();

  if OpenXbe(XbeOpenDialog.FileName, {var}m_Xbe) then
  begin
    StatusBar.SimpleText := Format('DXBX: %s Loaded', [m_szAsciiTitle]);
    UpdateIcon(m_Xbe);
    RecentXbeAdd(XbeOpenDialog.FileName);
    Emulation_State := esFileOpen;
    AddjustMenu;
  end
  else
  begin
    MessageDlg('Can not open Xbe file.', mtWarning, [mbOk], 0);
    Emulation_State := esNone;
    AddjustMenu;
  end;
end; // Tfrm_Main.actOpenXbeExecute


procedure Tfrm_Main.actCloseXbeExecute(Sender: TObject);
begin
  CloseXbe();
end;

procedure Tfrm_Main.CloseXbe;
begin
  FreeAndNil(m_Xbe);
  Emulation_State := esNone;
  UpdateIcon(nil);
  AddjustMenu;

  WriteLog(Format('DXBX: %s Closed...', [m_szAsciiTitle]));
  StatusBar.SimpleText := 'DXBX:';
end;

function Tfrm_Main.SendCommandToXdkTracker: Boolean;
var
  stringToSend: string;
  copyDataStruct: TCopyDataStruct;

  function _SendData(copyDataStruct: TCopyDataStruct): Boolean;
  var
    receiverHandle: Windows.THandle;
    res: Integer;
  begin
    Result := False;
    receiverHandle := FindWindow(PChar('TfrmXdkTracker'), nil);
    if receiverHandle = 0 then
      Exit;

    res := SendMessage(receiverHandle, WM_COPYDATA, Integer(Handle), Integer(@copyDataStruct));
    if res > 0 then
      Result := True;
  end;

begin
  stringToSend := 'READXML';

  copyDataStruct.dwData := Integer(0); //use it to identify the message contents
  copyDataStruct.cbData := 1 + Length(stringToSend);
  copyDataStruct.lpData := PChar(stringToSend);

  Result := _SendData(copyDataStruct);
end;

procedure Tfrm_Main.actConfigControllerExecute(Sender: TObject);
begin
  frm_ControllerConfig := Tfrm_ControllerConfig.Create(nil);

  if frm_ControllerConfig.ShowModal = mrOk then
    ;

  FreeAndNil({var}frm_ControllerConfig);
end;

procedure Tfrm_Main.actConfigVideoExecute(Sender: TObject);
begin
  frm_VideoConfig := Tfrm_VideoConfig.Create(nil);

  if frm_VideoConfig.ShowModal = mrOk then
    ;

  FreeAndNil({var}frm_VideoConfig);
end;

procedure Tfrm_Main.ActStartEmulationExecute(Sender: TObject);
begin
  if not Assigned(m_Xbe) then
  begin
    MessageDlg('No xbe file loaded', mtInformation, [mbOk], 0);
    Exit;
  end;

  LaunchXBE(m_Xbe, Self.Handle);
end;

procedure Tfrm_Main.FormCreate(Sender: TObject);
var
  XBEFilePath: string;
begin
  EmuShared.Init;

  Emulation_State := esNone;

  // Dxbx Note : This prevents close-exceptions (we have with a "message WM_DROPFILES" procedure) :
  OldLBWindowProc := frm_Main.WindowProc; // store defualt WindowProc
  frm_Main.WindowProc := LBWindowProc; // replace default WindowProc
  DragAcceptFiles(frm_Main.Handle, True); // now ListBox1 accept dropped files

  FApplicationDir := ExtractFilePath(Application.ExeName);
  StatusBar.SimpleText := 'DXBX: No Xbe Loaded...';

  ReadSettingsIni;

  CreateLogs(DebugMode, DebugFileName);

  AddjustMenu;

  XBEFilePath := ParamStr(1);

  if  (XBEFilePath <> '')
  and SameText(ExtractFileExt(XBEFilePath), '.xbe')
  and FileExists(XBEFilePath) then
  begin
    if OpenXbe(XBEFilePath, {var}m_Xbe) then
      LaunchXBE(m_Xbe, Self.Handle);
    // TODO : Error logging should go here
  end;
end;

procedure Tfrm_Main.FormDestroy(Sender: TObject);
begin
  WriteSettingsIni();
  CloseLogs();
  if Assigned(m_Xbe) then
    CloseXbe();
end;

procedure Tfrm_Main.LBWindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_DROPFILES then
    WMDROPFILES(Message); // handle WM_DROPFILES message

  OldLBWindowProc(Message);
  // call default ListBox1 WindowProc method to handle all other messages
end;

procedure Tfrm_Main.ReadSettingsIni;
var
  DxbxIniFilePath: string;
  IniFile: TIniFile;
  i: Integer;
begin
  DxbxIniFilePath := FApplicationDir + 'Dxbx.Ini';
  if not FileExists(DxbxIniFilePath) then
  begin
    // Setting defaults
    DebugMode := dmNone;
    KernelDebugMode := dmFile;

    Exit;
  end;

  IniFile := TIniFile.Create(DxbxIniFilePath);
  try
    DebugMode := TDebugMode(IniFile.ReadInteger('Settings', 'DxbxDebug', Ord(dmNone)));
    DebugFileName := IniFile.ReadString('Settings', 'DxbxDebugFileName', '');

    KernelDebugMode := TDebugMode(IniFile.ReadInteger('Settings', 'KrnlDebug', Ord(dmNone)));
    KernelDebugFileName := IniFile.ReadString('Settings', 'KrnlDebugFileName', '');

    // Read recent XBE files
    with TStringList.Create do
    try
      Delimiter := '|';
      StrictDelimiter := True;

      DelimitedText := IniFile.ReadString('Recent', 'XBEs', '');
      mnu_RecentXbefiles.Clear;
      for i := 0 to Count - 1 do
        RecentXbeAdd(Strings[i]);
    finally
      Free;
    end;
  finally
    FreeAndNil({var}IniFile);
  end;
end; // Tfrm_Main.ReadSettingsIni

procedure Tfrm_Main.WMDROPFILES(var Msg: TMessage);
var
  pcFileName: PChar;
  i, iSize, iFileCount: Integer;
begin
  pcFileName := ''; // to avoid compiler warning message
  iFileCount := DragQueryFile(Msg.wParam, $FFFFFFFF, pcFileName, 255);
  for i := 0 to iFileCount - 1 do
  begin
    iSize := DragQueryFile(Msg.wParam, i, nil, 0) + 1;
    pcFileName := StrAlloc(iSize);
    DragQueryFile(Msg.wParam, i, pcFileName, iSize);
    if FileExists(pcFileName) then
    begin
      if Assigned(m_Xbe) then
        CloseXbe();

      if OpenXbe(pcFileName, {var}m_Xbe) then
      begin
        StatusBar.SimpleText := Format('DXBX: %s Loaded', [m_szAsciiTitle]);
        UpdateIcon(m_Xbe);
        RecentXbeAdd(XbeOpenDialog.FileName);
        Emulation_State := esFileOpen;
        AddjustMenu;
      end
      else
      begin
        MessageDlg('Can not open Xbe file.', mtWarning, [mbOk], 0);
        Emulation_State := esNone;
        AddjustMenu;
      end;
    end;

    StrDispose(pcFileName);
  end;

  DragFinish(Msg.wParam);
end;

procedure Tfrm_Main.WriteSettingsIni;
var
  IniFile: TIniFile;
  RecentTMP: string;
  i: Integer;
begin
  IniFile := TIniFile.Create(FApplicationDir + 'Dxbx.Ini');
  try
    // Recent XBEs write
    RecentTMP := '';
    if mnu_RecentXbefiles.Count >= 1 then
    begin
      RecentTMP := mnu_RecentXbefiles.Items[0].Hint;
      for i := 1 to mnu_RecentXbefiles.Count - 1 do
        RecentTMP := RecentTMP + '|' + mnu_RecentXbefiles.Items[i].Hint;
    end;
    IniFile.WriteString('Recent', 'XBEs', RecentTMP);

    IniFile.WriteInteger('Settings', 'DxbxDebug', Ord(DebugMode));
    IniFile.WriteString('Settings', 'DxbxDebugFileName', DebugFileName);

    IniFile.WriteInteger('Settings', 'KrnlDebug', Ord(KernelDebugMode));
    IniFile.WriteString('Settings', 'KrnlDebugFileName', KernelDebugFileName);
  finally
    FreeAndNil(IniFile);
  end;
end; // Tfrm_Main.WriteSettingsIni

procedure Tfrm_Main.actConsoleXbeInfoExecute(Sender: TObject);
begin
  // dump xbe information to debug console
  if m_Xbe.DumpInformation then
    WriteLog(m_szAsciiTitle + '`s .xbe info was successfully dumped.')
  else
    WriteLog(m_szAsciiTitle + '`s .xbe info was successfully dumped.'); // NOT!
end; // Tfrm_Main.actConsoleXbeInfoExecute

procedure Tfrm_Main.actFileXbeInfoExecute(Sender: TObject);
begin
  SaveDialog.FileName := m_Xbe.DetermineDumpFileName;
  SaveDialog.Filter := DIALOG_FILTER_TEXT;

  if SaveDialog.Execute then
  begin
    // ask permisssion to override if file exists
    if FileExists(SaveDialog.FileName) then
    begin
      if MessageDlg('Overwrite existing file?', mtConfirmation, [mbYes, mbNo], -1) = mrNo then
        Exit;
    end;

    m_Xbe.DumpInformation(SaveDialog.FileName);
    WriteLog(m_szAsciiTitle + '''s .xbe info was successfully dumped.');
    MessageDlg(m_szAsciiTitle + '''s .xbe info was successfully dumped.', mtInformation, [mbOk], 0);
  end;
end; // Tfrm_Main.actFileXbeInfoExecute

procedure Tfrm_Main.actConsoleDebugGuiExecute(Sender: TObject);
begin
  if DebugMode = dmConsole then
  begin
    actConsoleDebugGui.Checked := False;
    DebugMode := dmNone;
    CloseLogs;
  end
  else
  begin
    CloseLogs;
    actFileDebugGui.Checked := False;
    actConsoleDebugGui.Checked := True;
    DebugMode := dmConsole;
    CreateLogs(DebugMode, DebugFileName);
  end;
end; // Tfrm_Main.actConsoleDebugGuiExecute

procedure Tfrm_Main.actFileDebugGuiExecute(Sender: TObject);
begin
  if DebugMode = dmFile then
  begin
    actFileDebugGui.Checked := False;
    CloseLogs;
    AddjustMenu;
  end
  else
  begin
    if DebugFileName <> '' then
      SaveDialog.FileName := DebugFileName
    else
      SaveDialog.FileName := DXBX_CONSOLE_DEBUG_FILENAME;

    SaveDialog.Filter := DIALOG_FILTER_TEXT;
    if SaveDialog.Execute then
    begin
      CloseLogs;
      DebugMode := dmFile;
      DebugFileName := SaveDialog.FileName;

      CreateLogs(DebugMode, DebugFileName);
      AddjustMenu;
    end;
  end;
end; // Tfrm_Main.actFileDebugGuiExecute

procedure Tfrm_Main.actConsoleDebugKernelExecute(Sender: TObject);
begin
  if KernelDebugMode = dmConsole then
  begin
    actConsoleDebugKernel.Checked := False;
    CloseLogs;
  end
  else
  begin
    CloseLogs;
    actFileDebugKernel.Checked := False;
    actConsoleDebugKernel.Checked := True;
    KernelDebugMode := dmConsole;
  end;
end; // Tfrm_Main.actConsoleDebugKernelExecute

procedure Tfrm_Main.actFileDebugKernelExecute(Sender: TObject);
begin
  if KernelDebugMode = dmFile then
  begin
    actFileDebugKernel.Checked := False;
    CloseLogs;
    AddjustMenu;
  end
  else
  begin
    if KernelDebugFileName <> '' then
      SaveDialog.FileName := KernelDebugFileName
    else
      SaveDialog.FileName := DXBX_KERNEL_DEBUG_FILENAME;

    SaveDialog.Filter := DIALOG_FILTER_TEXT;
    if SaveDialog.Execute then
    begin
      CloseLogs;
      KernelDebugMode := dmFile;
      KernelDebugFileName := SaveDialog.FileName;
      AddjustMenu;
    end;
  end;
end; // Tfrm_Main.actFileDebugKernelExecute

procedure Tfrm_Main.actAboutExecute(Sender: TObject);
begin
  frm_About := Tfrm_About.Create(Self);

  if frm_About.ShowModal = mrOk then
  begin
  end;

  FreeAndNil({var}frm_About);
end; // Tfrm_Main.ActAboutExecute

procedure Tfrm_Main.actCloseExecute(Sender: TObject);
begin
  Close;
end; // Tfrm_Main.actCloseExecute

destructor Tfrm_Main.Destroy;
begin
  frm_Main.WindowProc := OldLBWindowProc;
  WriteSettingsIni;

  EmuShared.Cleanup;

  inherited Destroy;
end; // Tfrm_Main.Create

procedure Tfrm_Main.AddjustMenu;
begin
  // Init File
  actOpenXbe.Enabled := True;
  actCloseXbe.Enabled := False;

  actSaveXbe.Enabled := False;
  actSaveXbeAs.Enabled := False;

  mnu_RecentXbefiles.Enabled := (mnu_RecentXbefiles.Count > 0);

  actClose.Enabled := True;

  // Init Edit
  mnu_Logbitmap.Enabled := False;
  mnu_Patch.Enabled := False;
  mnu_DumpxbeinfoTo.Enabled := False;

  // Init View
  actConsoleDebugGui.Checked := (DebugMode = dmConsole);
  actFileDebugGui.Checked := (DebugMode = dmFile);

  actConsoleDebugKernel.Checked := (KernelDebugMode = dmConsole);
  actFileDebugKernel.Checked := (KernelDebugMode = dmFile);

  ActStartEmulation.Enabled := False;

  if Emulation_State = esFileOpen then
  begin
    mnu_Logbitmap.Enabled := True;
    mnu_DumpxbeinfoTo.Enabled := True;
    mnu_CloseXbe.Enabled := True;
    actCloseXbe.Enabled := True;
    ActStartEmulation.Enabled := True;
  end;
end;

procedure Tfrm_Main.actExportLogoExecute(Sender: TObject);
var
  bmp: TBitmap;
begin
  if not LogoSaveDialog.Execute then
    Exit;

  // ask permission to overwrite if file exists
  if FileExists(LogoSaveDialog.FileName) then
  begin
    if MessageDlg('Overwrite existing file?', mtConfirmation, [mbYes, mbNo], -1) = mrYes then
      DeleteFile(LogoSaveDialog.FileName)
    else
      Exit;
  end;

  // export logo bitmap
  bmp := TBitmap.Create;
  try
    m_Xbe.ExportLogoBitmap(bmp);
    bmp.SaveToFile(LogoSaveDialog.FileName);
  finally
    FreeAndNil(bmp);
    WriteLog(m_szAsciiTitle + '''s logo bitmap was successfully exported.');
  end;
end; // Tfrm_Main.actExportLogoExecute

procedure Tfrm_Main.actXdkTrackerExecute(Sender: TObject);
begin
  if FileExists(FApplicationDir + 'Tools\XdkTracker.exe') then
    ShellExecute(0, 'open', PChar(FApplicationDir + 'Tools\XdkTracker.exe'), nil, nil, SW_SHOWNORMAL);
end;

procedure Tfrm_Main.actXIsoExecute(Sender: TObject);
begin
  if FileExists(FApplicationDir + 'Tools\xIso.exe') then
    ShellExecute(0, 'open', PChar(FApplicationDir + 'Tools\xIso.exe'), nil, nil, SW_SHOWNORMAL);
end;

procedure Tfrm_Main.actXdkTrackerXbeInfoExecute(Sender: TObject);
var
  DumpFilePath: string;
begin
  if FileExists(FApplicationDir + 'Tools\XdkTracker.exe') then
  begin
    DumpFilePath := FApplicationDir + 'Tools\Dump.dat';
    DxbxXml.CreateXmlXbeDump(DumpFilePath, m_Xbe);

    if not SendCommandToXdkTracker then
      ShellExecute(0, 'open', PChar(FApplicationDir + 'Tools\XdkTracker.exe'), '/XBEDUMP', nil, SW_SHOWNORMAL);
  end;
end;

procedure Tfrm_Main.RecentXbeAdd(aFileName: string);
var
  TempItem: TMenuItem;
  i: Integer;
begin
  for i := 0 to mnu_RecentXbefiles.Count - 1 do
  begin
    if mnu_RecentXbefiles.Items[i].Hint = aFileName then
    begin
      mnu_RecentXbefiles.Remove(mnu_RecentXbefiles.Items[i]);
      Break;
    end;
  end;

  TempItem := TMenuItem.Create(mnu_RecentXbefiles);
  TempItem.Hint := aFileName;
  TempItem.Caption := aFileName;
  TempItem.OnClick := ReopenXbe;

  while mnu_RecentXbefiles.Count >= _RecentXbeLimit do
    mnu_RecentXbefiles.Remove(mnu_RecentXbefiles.Items[mnu_RecentXbefiles.Count - 1]);

  mnu_RecentXbefiles.Insert(0, TempItem);
  mnu_RecentXbefiles.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.ReopenXbe(Sender: TObject);
var
  TempItem: TMenuItem;
begin
  TempItem := Sender as TMenuItem;
  if not FileExists(TempItem.Hint) then
  begin
    MessageDlg('Could not locate file : ' + TempItem.Hint, mtWarning, [mbOk], 0);
    mnu_RecentXbefiles.Remove(TempItem);
    Exit;
  end;

  if Assigned(m_Xbe) then
    CloseXbe();

  if not OpenXbe(TempItem.Hint, {var}m_Xbe) then
  begin
    MessageDlg('Can not open Xbe file.', mtWarning, [mbOk], 0);
    Exit;
  end;

  StatusBar.SimpleText := Format('DXBX: %s Loaded', [m_szAsciiTitle]);
  UpdateIcon(m_Xbe);
  Emulation_State := esFileOpen;
  AddjustMenu;
end;

procedure Tfrm_Main.UpdateIcon(const aXbe: TXBE);
begin
  if Assigned(aXBE) and aXbe.ExportLogoBitmap(ImageLogo.Picture.Bitmap) then
    ImageLogo.Show
  else
    ImageLogo.Hide;

  if Assigned(aXBE) and aXbe.ExportIconBitmap(ImageIcon.Picture.Bitmap) then
    ImageIcon.Show
  else
    ImageIcon.Hide;
end;

end.

