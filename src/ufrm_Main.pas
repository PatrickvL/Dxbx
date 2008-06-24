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
  Windows, Messages, SysUtils, Classes,
  Dialogs, ActnList, Forms, Menus,
  Controls, ComCtrls, ExtCtrls,
  Graphics, JPeg,
  ShellAPI, IniFiles,
  // Dxbx
  uTypes,
  uConsts,
  uLog,
  uWindows,
  uXbe,
  uEmuExe,
  uXbeConvert,
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
    Importexe1: TMenuItem;
    mnu_ExportExe: TMenuItem;
    N6: TMenuItem;
    SaveXbe1: TMenuItem;
    SaveXbeas1: TMenuItem;
    N7: TMenuItem;
    mnu_RecentXbefiles: TMenuItem;
    mnu_RecentExefiles: TMenuItem;
    mnu_Logbitmap: TMenuItem;
    mnu_Patch: TMenuItem;
    N3: TMenuItem;
    mnu_DumpxbeinfoTo: TMenuItem;
    mnu_DebugoutputGUI: TMenuItem;
    mnu_DebugoutputKernel: TMenuItem;
    mnu_ConfigControler: TMenuItem;
    mnu_ConfigAudio: TMenuItem;
    mnu_Configvideo: TMenuItem;
    N4: TMenuItem;
    Executablegeneration1: TMenuItem;
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
    actImportExe: TAction;
    actExportExe: TAction;
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
    mnu_AutomaticWindowsyemp: TMenuItem;
    mnu_AutomaticDxbxpath: TMenuItem;
    Manual1: TMenuItem;
    ExeOpenDialog: TOpenDialog;
    actExeGenWindowsTemp: TAction;
    actExeGenDxbxPath: TAction;
    actExeGenManual: TAction;
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
    N9: TMenuItem;
    UsesDlltype1: TMenuItem;
    StatusBar: TStatusBar;
    procedure ActStartEmulationExecute(Sender: TObject);
    procedure actOpenXbeExecute(Sender: TObject);
    procedure actCloseXbeExecute(Sender: TObject);
    procedure actSaveXbeExecute(Sender: TObject);
    procedure actSaveXbeAsExecute(Sender: TObject);
    procedure actImportExeExecute(Sender: TObject);
    procedure actExportExeExecute(Sender: TObject);
    procedure actConfigControllerExecute(Sender: TObject);
    procedure actConfigVideoExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure actConsoleXbeInfoExecute(Sender: TObject);
    procedure actFileXbeInfoExecute(Sender: TObject);
    procedure actConsoleDebugGuiExecute(Sender: TObject);
    procedure actFileDebugGuiExecute(Sender: TObject);
    procedure actConsoleDebugKernelExecute(Sender: TObject);
    procedure actFileDebugKernelExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actExeGenWindowsTempExecute(Sender: TObject);
    procedure actExeGenDxbxPathExecute(Sender: TObject);
    procedure actExeGenManualExecute(Sender: TObject);
    procedure actExportLogoExecute(Sender: TObject);
    procedure actXdkTrackerExecute(Sender: TObject);
    procedure actXIsoExecute(Sender: TObject);
    procedure actXdkTrackerXbeInfoExecute(Sender: TObject);
    procedure actSwitchDLLExecute(Sender: TObject);
  private
    m_Xbe: TXbe;
    m_XbeFilename: string;
    m_ExeFilename: string;
    m_bExeChanged: Boolean;

    m_AutoConvertToExe: EnumAutoConvert;
    Emulation_State: EMU_STATE;

    procedure SetExeGen(ConvertTo: EnumAutoConvert);

    procedure CloseXbe;
    procedure SaveXbe(aFileName: string = '');
    procedure ImportExe(aFileName: string);

    procedure CreateDllMenuOptions;

    procedure ReadSettingsIni;
    procedure WriteSettingsIni;

    procedure RecentXbeAdd(aFileName: string);
    procedure RecentExeAdd(aFileName: string);
    procedure ReopenXbe(Sender: TObject);
    procedure ReopenExe(Sender: TObject);


    function StartEmulation(x_AutoConvert: EnumAutoConvert): Boolean;

    function SendCommandToXdkTracker: Boolean;


    procedure WMDROPFILES(var Msg: TMessage);
    procedure LBWindowProc(var Message: TMessage);

    procedure AddjustMenu;
  public
    FApplicationDir: string;

    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

var
  frm_Main: Tfrm_Main;
  OldLBWindowProc: TWndMethod;


implementation

{$R *.DFM}

//------------------------------------------------------------------------------

procedure Tfrm_Main.actOpenXbeExecute(Sender: TObject);
begin
  XbeOpenDialog.Filter := DIALOG_FILTER_XBE;
  if not XbeOpenDialog.Execute then
    Exit;

  if Assigned(m_Xbe) then begin
    CloseXbe();
  end;

  if OpenXbe(XbeOpenDialog.Filename, m_Xbe, m_ExeFilename, m_XbeFilename) then begin
    StatusBar.SimpleText := Format('DXBX: %s Loaded', [m_szAsciiTitle]);
    RecentXbeAdd( XbeOpenDialog.Filename );
    Emulation_State := esFileOpen;
    AddjustMenu;
  end
  else begin
    MessageDlg('Can not open Xbe file.', mtWarning, [mbOk], 0);
    Emulation_State := esNone;
    AddjustMenu;
  end;
end; // Tfrm_Main.actOpenXbeExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actCloseXbeExecute(Sender: TObject);
begin
  CloseXbe();
end; // Tfrm_Main.actCloseXbeExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actSaveXbeExecute(Sender: TObject);
begin
  SaveDialog.Filter := DIALOG_FILTER_XBE;
  if m_XbeFilename <> '' then
    SaveXbe()
  else
  begin
    if SaveDialog.Execute then
      SaveXbe(SaveDialog.Filename);
  end;
end; // Tfrm_Main.actSaveXbeExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actSaveXbeAsExecute(Sender: TObject);
begin
  SaveXbe(SaveDialog.Filename);
end; // Tfrm_Main.actSaveXbeAsExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actImportExeExecute(Sender: TObject);
begin
  XbeOpenDialog.Filter := DIALOG_FILTER_EXE;
  if XbeOpenDialog.Execute() then
  begin
    if Assigned(m_Xbe) then
      CloseXbe();

    ImportExe(XbeOpenDialog.Filename);
    RecentExeAdd(XbeOpenDialog.Filename);
  end;
end; // Tfrm_Main.actImportExeExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actExportExeExecute(Sender: TObject);
begin
  if ExeSaveDialog.Execute then begin
    if ConvertToExe('', True, m_Xbe, Self.Handle) then begin
      m_ExeFilename := ExeSaveDialog.FileName;
      WriteLog(m_szAsciiTitle + ' was converted to .exe.');
      m_bExeChanged := False;
      RecentExeAdd(m_ExeFilename);
    end
    else begin
      WriteLog('Export: Error converting ' + m_szAsciiTitle + ' to .exe');
    end;
  end;
end; // Tfrm_Main.actExportExeExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteSettingsIni();
  CloseLogs();
  if Assigned(m_Xbe) then
    CloseXbe();
end; // Tfrm_Main.FormClose

//------------------------------------------------------------------------------

procedure Tfrm_Main.CloseXbe;
begin
  FreeAndNil(m_Xbe);
  Emulation_State := esNone;
  AddjustMenu;

  WriteLog(Format('DXBX: %s Closed...', [m_szAsciiTitle]));
  StatusBar.SimpleText := 'DXBX:';
end; // Tfrm_Main.CloseXbe

//------------------------------------------------------------------------------

procedure Tfrm_Main.SaveXbe(aFileName: string);
begin
  { TODO : Not implemented yet }
end;

//------------------------------------------------------------------------------

function Tfrm_Main.SendCommandToXdkTracker: Boolean;
var
  stringToSend: string;
  copyDataStruct: TCopyDataStruct;

  function _SendData(copyDataStruct: TCopyDataStruct): Boolean;
  var
    receiverHandle: THandle;
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

//------------------------------------------------------------------------------

procedure Tfrm_Main.SetExeGen(ConvertTo: EnumAutoConvert);
begin
  m_AutoConvertToExe := ConvertTo;
  AddjustMenu;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.CreateDllMenuOptions;
  function _CreateDLLToUseMenuItem(const u: TUseDLL): TMenuItem;
  begin
    Result := TMenuItem.Create(self);
    Result.Caption := GetDllDescription(u);
    Result.Tag := Ord(u);
    Result.OnClick := actSwitchDLLExecute;
  end;
var
  u: TUseDLL;
begin
  for u := Low(TUseDLL) to High(TUseDLL) do begin
    UsesDlltype1.add(_CreateDLLToUseMenuItem(u));
  end;
end;

procedure Tfrm_Main.actConfigControllerExecute(Sender: TObject);
begin
  frm_ControllerConfig := Tfrm_ControllerConfig.Create(nil);

  if frm_ControllerConfig.ShowModal = mrOk then begin
  end;

  FreeAndNil({var}frm_ControllerConfig);
end; // Tfrm_Main.actConfigControllerExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actConfigVideoExecute(Sender: TObject);
begin
  frm_VideoConfig := Tfrm_VideoConfig.Create(nil);

  if frm_VideoConfig.ShowModal = mrOk then begin
  end;

  FreeAndNil({var}frm_VideoConfig);
end; // Tfrm_Main.actConfigVideoExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.ActStartEmulationExecute(Sender: TObject);
var
  FileConverted: Boolean;
begin
  if Assigned(m_Xbe) then
  begin
    FileConverted := StartEmulation(m_AutoConvertToExe);
    if FileConverted then
    begin
      try
        if FileExists(m_ExeFilename) then
        begin
          WriteLog('WndMain: ' + m_szAsciiTitle + ' emulation started.');
          WinExec(PChar(m_ExeFilename), SW_SHOWNORMAL);
        end
        else
        begin
          MessageDlg(m_ExeFilename + ' does not exists.', mtError, [mbOk], 0);
          WriteLog('WndMain: ' + m_ExeFilename + ' does not exists.');
        end;
      except
        MessageDlg('Emmulation failed. Try converting again. If this message repeats, the Xbe is not supported.', mtError, [mbOk], 0);
        WriteLog('WndMain: ' + m_szAsciiTitle + 'shell failed.');
      end;
    end;

  end
  else
    MessageDlg('No xbe file loaded', mtInformation, [mbOk], 0);
end; // Tfrm_Main.ActStartEmulationExecute

//------------------------------------------------------------------------------

function Tfrm_Main.StartEmulation(x_AutoConvert: EnumAutoConvert): Boolean;
var
  FileName: string;
begin
  Result := False;
  // Convert Xbe to Exe, if necessary
  if (m_ExeFilename = '\0') or (m_bExeChanged) then
  begin
    case x_AutoConvert of
      CONVERT_TO_WINDOWSTEMP: FileName := GetTempDirectory + ExtractFileName(ChangeFileExt(m_XbeFilename, '.exe'));
      CONVERT_TO_XBEPATH: FileName := ExtractFileName(ChangeFileExt(m_XbeFilename, '.exe'));
    else
      if ExeSaveDialog.Execute then
        FileName := ExeSaveDialog.FileName;
    end; // case

    try
      Result := ConvertToExe(FileName, False, m_Xbe, self.Handle);
    finally
    end;

    if Result then begin
      m_ExeFilename := FileName;
      WriteLog(m_szAsciiTitle + ' was converted to .exe.');
      m_bExeChanged := False;
      RecentExeAdd(m_ExeFilename);
    end
    else begin
      WriteLog('Export: Error converting ' + m_szAsciiTitle + ' to .exe');
    end;
  end;
end; // Tfrm_Main.StartEmulation

//------------------------------------------------------------------------------

procedure Tfrm_Main.FormCreate(Sender: TObject);
begin
  Emulation_State := esNone;

  OldLBWindowProc := frm_Main.WindowProc; // store defualt WindowProc
  frm_Main.WindowProc := LBWindowProc; // replace default WindowProc
  DragAcceptFiles(frm_Main.Handle, True); // now ListBox1 accept dropped files

  FApplicationDir := ExtractFilePath(Application.ExeName);
  StatusBar.SimpleText := 'DXBX: No Xbe Loaded...';

  CreateDllMenuOptions;
  ReadSettingsIni;
  CreateLogs(ltGui);

  AddjustMenu;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.ImportExe(aFileName: string);
begin
  m_ExeFilename := '\0';

  m_Xbe := TXbe.Create(aFileName, ftExe);
  try
    XbeLoaded();
    m_bExeChanged := True;
    StatusBar.SimpleText := Format('DXBX: %s Loaded', [m_szAsciiTitle]);
  except
    MessageDlg('Can not open Exe file.', mtWarning, [mbOk], 0);
    FreeAndNil({var}m_Xbe);
  end;
end;

procedure Tfrm_Main.LBWindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_DROPFILES then
    WMDROPFILES(Message); // handle WM_DROPFILES message
  OldLBWindowProc(Message);
  // call default ListBox1 WindowProc method to handle all other messages
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.ReadSettingsIni;
var
  DxbxIniFilePath: string;
  IniFile: TIniFile;
  RecentTMP: string;
  RecentTMPList: TStringList;
  lIndex: Integer;
begin
  DxbxIniFilePath := FApplicationDir + 'Dxbx.Ini';
  if FileExists(DxbxIniFilePath) then
  begin
    IniFile := TIniFile.Create(DxbxIniFilePath);

    case IniFile.ReadInteger('Settings', 'AutoConvertToExe', 1) of
      1: m_AutoConvertToExe := CONVERT_TO_WINDOWSTEMP;
      2: m_AutoConvertToExe := CONVERT_TO_XBEPATH;
      3: m_AutoConvertToExe := CONVERT_TO_MANUAL;
    else
      m_AutoConvertToExe := CONVERT_TO_MANUAL;
    end;

    m_DxbxDebug := DebugMode(IniFile.ReadInteger('Settings', 'DxbxDebug', Ord(DM_NONE)));
    m_DxbxDebugFilename := IniFile.ReadString('Settings', 'DxbxDebugFilename', '');
    m_KrnlDebug := DebugMode(IniFile.ReadInteger('Settings', 'KrnlDebug', Ord(DM_NONE)));
    m_KrnlDebugFilename := IniFile.ReadString('Settings', 'KrnlDebugFilename', '');

    // Dll settings
    DLLToUse := TUseDLL(StrToInt(IniFile.ReadString('Settings', 'DllToUse', '0')));
    for lIndex := 0 to UsesDlltype1.Count - 1 do
      UsesDlltype1.Items[lIndex].Checked := False;

    UsesDlltype1.Items[Ord(DLLToUse)].Checked := True;

    // Read recent XBE files
    RecentTMP := IniFile.ReadString('Recent', 'XBEs', '');
    RecentTMPList := TStringList.Create;
    RecentTMPList.Clear;
    RecentTMPList.Delimiter := '|';
    RecentTMPList.StrictDelimiter := True;
    RecentTMPList.DelimitedText := RecentTMP;
    mnu_RecentXbefiles.Clear;
    for lIndex := 0 to RecentTMPList.Count - 1 do
      RecentXbeAdd(RecentTMPList[lIndex]);

    // Read recent EXE files
    RecentTMP := IniFile.ReadString('Recent', 'EXEs', '');
    RecentTMPList := TStringList.Create;
    RecentTMPList.Clear;
    RecentTMPList.Delimiter := '|';
    RecentTMPList.StrictDelimiter;
    RecentTMPList.DelimitedText := RecentTMP;
    mnu_RecentExefiles.Clear;
    for lIndex := 0 to RecentTMPList.Count - 1 do
      RecentExeAdd(RecentTMPList[lIndex]);

    FreeAndNil({var}IniFile);
  end
  else
  begin
    // Setting defaults
    m_DxbxDebug := DM_CONSOLE;
    m_KrnlDebug := DM_CONSOLE;

    m_AutoConvertToExe := CONVERT_TO_MANUAL;
  end;
end; // Tfrm_Main.ReadSettingsIni

//------------------------------------------------------------------------------

procedure Tfrm_Main.WMDROPFILES(var Msg: TMessage);
var
  pcFileName: PChar;
  i, iSize, iFileCount: integer;
begin
  pcFileName := ''; // to avoid compiler warning message
  iFileCount := DragQueryFile(Msg.wParam, $FFFFFFFF, pcFileName, 255);
  for i := 0 to iFileCount - 1 do
  begin
    iSize := DragQueryFile(Msg.wParam, i, nil, 0) + 1;
    pcFileName := StrAlloc(iSize);
    DragQueryFile(Msg.wParam, i, pcFileName, iSize);
    if FileExists(pcFileName) then begin       

      if Assigned(m_Xbe) then begin
        CloseXbe();
      end;

      if OpenXbe(pcFileName, m_Xbe, m_ExeFilename, m_XbeFilename) then begin
        StatusBar.SimpleText := Format('DXBX: %s Loaded', [m_szAsciiTitle]);
        RecentXbeAdd( XbeOpenDialog.Filename );
        Emulation_State := esFileOpen;
        AddjustMenu;
      end
      else begin
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
  I: Integer;
begin
  // het opslaan van de inifile
  IniFile := TIniFile.Create(FApplicationDir + 'Dxbx.Ini');

  // Recent XBEs write
  RecentTMP := '';
  if mnu_RecentXbefiles.Count >= 1 then
    RecentTMP := mnu_RecentXbefiles.Items[0].Hint;

  for I := 1 to mnu_RecentXbefiles.Count - 1 do
    RecentTMP := RecentTMP + '|' + mnu_RecentXbefiles.Items[I].Hint;

  IniFile.WriteString('Recent', 'XBEs', RecentTMP);

  // Recent EXEs write
  RecentTMP := '';
  if mnu_RecentExefiles.Count >= 1 then
    RecentTMP := mnu_RecentExefiles.Items[0].Hint;
  for I := 0 to mnu_RecentExefiles.Count - 1 do
    RecentTMP := RecentTMP + '|' + mnu_RecentExefiles.Items[I].Hint;

  IniFile.WriteString('Recent', 'EXEs', RecentTMP);

  case m_AutoConvertToExe of
    CONVERT_TO_WINDOWSTEMP: IniFile.WriteInteger('Settings', 'AutoConvertToExe', 1);
    CONVERT_TO_XBEPATH: IniFile.WriteInteger('Settings', 'AutoConvertToExe', 2);
    CONVERT_TO_MANUAL: IniFile.WriteInteger('Settings', 'AutoConvertToExe', 3);
  end;

  IniFile.WriteInteger('Settings', 'DxbxDebug', ORD(m_DxbxDebug));
  IniFile.WriteString('Settings', 'DxbxDebugFilename', m_DxbxDebugFilename);
  IniFile.WriteInteger('Settings', 'KrnlDebug', ORD(m_KrnlDebug));
  IniFile.WriteString('Settings', 'KrnlDebugFilename', m_KrnlDebugFilename);
  IniFile.WriteString('Settings', 'DllToUse', IntToStr(Ord(DLLToUse)));

  FreeAndNil(inifile);
end; // Tfrm_Main.WriteSettingsIni

//------------------------------------------------------------------------------

procedure Tfrm_Main.actConsoleXbeInfoExecute(Sender: TObject);
begin
  // dump xbe information to debug console
  if m_Xbe.DumpInformation then
    WriteLog(m_szAsciiTitle + '`s .xbe info was successfully dumped.')
  else
    WriteLog(m_szAsciiTitle + '`s .xbe info was successfully dumped.');
end; // Tfrm_Main.actConsoleXbeInfoExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actFileXbeInfoExecute(Sender: TObject);
begin
  SaveDialog.Filename := 'xbe.txt';
  SaveDialog.Filter := DIALOG_FILTER_TEXT;

  if SaveDialog.Execute then
  begin
    // ask permisssion to override if file exists
    if FileExists(SaveDialog.Filename) then
    begin
      if MessageDlg('Overwrite existing file?', mtConfirmation, [mbYes, mbNo], -1) = mrNo then
        Exit;
    end;

    m_Xbe.DumpInformation(SaveDialog.Filename);
    WriteLog(m_szAsciiTitle + '''s .xbe info was successfully dumped.');
    MessageDlg(m_szAsciiTitle + '''s .xbe info was successfully dumped.', mtInformation, [mbOk], 0);
  end;
end; // Tfrm_Main.actFileXbeInfoExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actConsoleDebugGuiExecute(Sender: TObject);
begin
  if m_DxbxDebug = DM_CONSOLE then
  begin
    actConsoleDebugGui.Checked := False;
    m_DxbxDebug := DM_NONE;
    CloseLogs;
  end
  else
  begin
    CloseLogs;
    actFileDebugGui.Checked := False;
    actConsoleDebugGui.Checked := True;
    m_DxbxDebug := DM_CONSOLE;
    CreateLogs(ltGui);
  end;
end; // Tfrm_Main.actConsoleDebugGuiExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actFileDebugGuiExecute(Sender: TObject);
begin
  if m_DxbxDebug = DM_FILE then
  begin
    actFileDebugGui.Checked := False;
    m_DxbxDebug := DM_NONE;
    CloseLogs;
    AddjustMenu;
  end
  else
  begin
    SaveDialog.Filename := 'DxbxDebug.txt';
    SaveDialog.Filter := DIALOG_FILTER_TEXT;
    if SaveDialog.Execute then
    begin
      CloseLogs;
      m_DxbxDebug := DM_FILE;
      CreateLogs(ltGui);
      m_DxbxDebugFilename := SaveDialog.Filename;
      AddjustMenu;
    end;
  end;
end; // Tfrm_Main.actFileDebugGuiExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actConsoleDebugKernelExecute(Sender: TObject);
begin
  if m_KrnlDebug = DM_CONSOLE then
  begin
    actConsoleDebugKernel.Checked := False;
    m_KrnlDebug := DM_NONE;
  end
  else
  begin
    actFileDebugKernel.Checked := False;
    actConsoleDebugKernel.Checked := True;
    m_KrnlDebug := DM_CONSOLE;
  end;
end; // Tfrm_Main.actConsoleDebugKernelExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actFileDebugKernelExecute(Sender: TObject);
begin
  if m_KrnlDebug = DM_FILE then
  begin
    actFileDebugKernel.Checked := False;
    m_KrnlDebug := DM_NONE;
    AddjustMenu;
  end
  else
  begin
    SaveDialog.Filename := 'KernelDebug.txt';
    SaveDialog.Filter := DIALOG_FILTER_TEXT;
    if SaveDialog.Execute then
    begin
      m_KrnlDebug := DM_FILE;
      m_KrnlDebugFilename := SaveDialog.Filename;
      AddjustMenu;
    end;
  end;
end; // Tfrm_Main.actFileDebugKernelExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actAboutExecute(Sender: TObject);
begin
  frm_About := Tfrm_About.Create(Self);

  if frm_About.ShowModal = mrOk then
  begin
  end;

  FreeAndNil({var}frm_About);
end; // Tfrm_Main.ActAboutExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actCloseExecute(Sender: TObject);
begin
  Close;
end; // Tfrm_Main.actCloseExecute

//------------------------------------------------------------------------------

destructor Tfrm_Main.Destroy;
begin
  frm_Main.WindowProc := OldLBWindowProc;
  WriteSettingsIni;
  inherited Destroy;
end; // Tfrm_Main.Create

//------------------------------------------------------------------------------

procedure Tfrm_Main.AddjustMenu;
begin
  // Init File
  actOpenXbe.Enabled := True;
  actCloseXbe.Enabled := False;

  actImportExe.Enabled := True;
  actExportExe.Enabled := False;

  actSaveXbe.Enabled := False;
  actSaveXbeAs.Enabled := False;

  mnu_RecentXbefiles.Enabled := mnu_RecentXbefiles.Count > 0;
  mnu_RecentExefiles.Enabled := mnu_RecentExefiles.count > 0;

  actClose.Enabled := True;

  // Init Edit
  mnu_Logbitmap.Enabled := False;
  mnu_Patch.Enabled := False;
  mnu_DumpxbeinfoTo.Enabled := False;

  // Init View
  actConsoleDebugGui.Checked := m_DxbxDebug = DM_CONSOLE;
  actFileDebugGui.Checked := m_DxbxDebug = DM_FILE;
  actConsoleDebugKernel.Checked := m_KrnlDebug = DM_CONSOLE;
  actFileDebugKernel.Checked := m_KrnlDebug = DM_FILE;

  // Init Settings
  actExeGenWindowsTemp.Checked := m_AutoConvertToExe = CONVERT_TO_WINDOWSTEMP;
  actExeGenDxbxPath.Checked := m_AutoConvertToExe = CONVERT_TO_XBEPATH;
  actExeGenManual.Checked := m_AutoConvertToExe = CONVERT_TO_MANUAL;

  ActStartEmulation.Enabled := False;

  if Emulation_State = esFileOpen then begin
    mnu_Logbitmap.Enabled := True;
    mnu_DumpxbeinfoTo.Enabled := True;
    actExportExe.Enabled := True;
    mnu_CloseXbe.Enabled := True;
    actCloseXbe.Enabled := True;
    ActStartEmulation.Enabled := True;
  end;
end;

procedure Tfrm_Main.AfterConstruction;
begin
  inherited AfterConstruction;
  ReadSettingsIni;
end; // Tfrm_Main.AfterConstruction

//------------------------------------------------------------------------------

procedure Tfrm_Main.actExeGenWindowsTempExecute(Sender: TObject);
begin
  SetExeGen(CONVERT_TO_WINDOWSTEMP);
end; // Tfrm_Main.actExeGenWindowsTempExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actExeGenDxbxPathExecute(Sender: TObject);
begin
  SetExeGen(CONVERT_TO_XBEPATH);
end; // Tfrm_Main.actExeGenDxbxPathExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actExeGenManualExecute(Sender: TObject);
begin
  SetExeGen(CONVERT_TO_MANUAL);
end; // Tfrm_Main.actExeGenManualExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actExportLogoExecute(Sender: TObject);
var
  bmp: TBitmap;
begin
  if not LogoSaveDialog.Execute then
    Exit;

  // ask permission to overwrite if file exists
  if FileExists(LogoSaveDialog.Filename) then
  begin
    if MessageDlg('Overwrite existing file?', mtConfirmation, [mbYes, mbNo], -1) = mrYes then
      DeleteFile(LogoSaveDialog.Filename)
    else
      Exit;
  end;

  // export logo bitmap
  bmp := TBitmap.Create;
  try
    bmp.Width := 100;
    bmp.Height := 17;
    m_Xbe.ExportLogoBitmap(bmp);
    bmp.SaveToFile(LogoSaveDialog.Filename);
  finally
    WriteLog(m_szAsciiTitle + '''s logo bitmap was successfully exported.');
  end;
end; // Tfrm_Main.actExportLogoExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actSwitchDLLExecute(Sender: TObject);
var
  lIndex: Integer;
begin
  Assert(Sender is TMenuItem);

  for lIndex := 0 to UsesDlltype1.Count - 1 do
    UsesDlltype1.Items[lIndex].Checked := False;

  TMenuItem(Sender).Checked := True;

  DLLToUse := TUseDLL(TMenuItem(Sender).Tag);
end;

procedure Tfrm_Main.actXdkTrackerExecute(Sender: TObject);
begin
  if FileExists(FApplicationDir + 'Tools\XdkTracker.exe') then
    ShellExecute(0, 'open', PChar(FApplicationDir + 'Tools\XdkTracker.exe'), nil, nil, SW_SHOWNORMAL);
end; // Tfrm_Main.actXdkTrackerExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actXIsoExecute(Sender: TObject);
begin
  if FileExists(FApplicationDir + 'Tools\xIso.exe') then
    ShellExecute(0, 'open', PChar(FApplicationDir + 'Tools\xIso.exe'), nil, nil, SW_SHOWNORMAL);
end; // Tfrm_Main.actXIsoExecute

//------------------------------------------------------------------------------

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
end; // Tfrm_Main.actXdkTrackerXbeInfoExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.RecentXbeAdd(aFileName: string);
var
  TempItem: TMenuItem;
  I: Integer;
begin
  for I := 0 to mnu_RecentXbefiles.Count - 1 do
  begin
    if mnu_RecentXbefiles.Items[I].Hint = aFileName then
    begin
      mnu_RecentXbefiles.Remove(mnu_RecentXbefiles.Items[I]);
      Break;
    end;
  end;

  TempItem := TMenuItem.Create(mnu_RecentXbefiles);
  TempItem.Hint := aFileName;
  TempItem.Caption := aFileName;
  TempItem.OnClick := ReopenXbe;

  while (mnu_RecentXbefiles.Count >= _RecentXbeLimit) do
    mnu_RecentXbefiles.Remove(mnu_RecentXbefiles.Items[9]);

  mnu_RecentXbefiles.Insert(0, TempItem);
  mnu_RecentXbefiles.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.RecentExeAdd(aFileName: string);
var
  TempItem: TMenuItem;
  I: Integer;
begin
  for I := 0 to mnu_RecentExefiles.Count - 1 do
  begin
    if mnu_RecentExefiles.Items[I].Hint = aFileName then
    begin
      mnu_RecentExefiles.Remove(mnu_RecentExefiles.Items[I]);
      Break;
    end;
  end;

  TempItem := TMenuItem.Create(mnu_RecentExefiles);
  TempItem.Hint := aFileName;
  TempItem.Caption := ExtractFileName(aFileName);
  TempItem.OnClick := ReopenExe;

  while (mnu_RecentExefiles.Count >= _RecentExeLimit) do
    mnu_RecentExefiles.Remove(mnu_RecentExefiles.Items[9]);

  mnu_RecentExefiles.Insert(0, TempItem);
  mnu_RecentExefiles.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.ReopenXbe(Sender: TObject);
var
  TempItem: TMenuItem;
begin
  TempItem := Sender as TMenuItem;
  if FileExists(TempItem.Hint) then
  begin
    if Assigned(m_Xbe) then
      CloseXbe();

    if OpenXbe(TempItem.Hint, m_Xbe, m_ExeFilename, m_XbeFilename) then begin
      StatusBar.SimpleText := Format('DXBX: %s Loaded', [m_szAsciiTitle]);
      Emulation_State := esFileOpen;
      AddjustMenu;
    end
    else begin
      MessageDlg('Can not open Xbe file.', mtWarning, [mbOk], 0);
    end;
  end
  else
  begin
    MessageDlg('Could not locate file : ' + TempItem.Hint, mtWarning, [mbOk], 0);
    mnu_RecentXbefiles.Remove(TempItem);
  end;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.ReopenExe(Sender: TObject);
var
  TempItem: TMenuItem;
begin
  TempItem := Sender as TMenuItem;
  if FileExists(TempItem.Hint) then
  begin
    ShellExecute(application.Handle, 'open', PChar(TempItem.Hint), nil, nil, SW_SHOWDEFAULT);
  end
  else
  begin
    ShowMessage('Could not locate file : ' + TempItem.Hint);
    mnu_RecentExefiles.Remove(TempItem);
  end;
end;

//------------------------------------------------------------------------------

end.

