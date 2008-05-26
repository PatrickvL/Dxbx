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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, ShellAPI, jpeg, ExtCtrls, ComCtrls,
  // AlphaSkin
  sSkinProvider, sSkinManager, sStatusBar,
  // Dxbx
  uTypes, uXbe, uEmuExe, uExternals,
  ufrm_ControllerConfig, ufrm_VideoConfig;

type
  Tfrm_Main = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Edit1: TMenuItem;
    View1: TMenuItem;
    Settings1: TMenuItem;
    Emulation1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    OpenXbe1: TMenuItem;
    CloseXbe1: TMenuItem;
    N2: TMenuItem;
    Importexe1: TMenuItem;
    Exportexe1: TMenuItem;
    N6: TMenuItem;
    SaveXbe1: TMenuItem;
    SaveXbeas1: TMenuItem;
    N7: TMenuItem;
    RecentXbefiles1: TMenuItem;
    RecentExefiles1: TMenuItem;
    Logbitmap1: TMenuItem;
    Patch1: TMenuItem;
    N3: TMenuItem;
    Dumpxbeinfoto1: TMenuItem;
    DebugoutputGUI1: TMenuItem;
    DebugoutputKernel1: TMenuItem;
    ConfigControler1: TMenuItem;
    Configaudio1: TMenuItem;
    Configvideo1: TMenuItem;
    N4: TMenuItem;
    Executablegeneration1: TMenuItem;
    Start1: TMenuItem;
    ActionList: TActionList;
    ActStartEmulation: TAction;
    ActAbout: TAction;
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
    Console1: TMenuItem;
    File2: TMenuItem;
    Console2: TMenuItem;
    File3: TMenuItem;
    Console3: TMenuItem;
    File4: TMenuItem;
    actConsoleXbeInfo: TAction;
    actFileXbeInfo: TAction;
    actConsoleDebugGui: TAction;
    actFileDebugGui: TAction;
    actConsoleDebugKernel: TAction;
    actFileDebugKernel: TAction;
    AutomaticWindowsyemp1: TMenuItem;
    AutomaticDxbxpath1: TMenuItem;
    Manual1: TMenuItem;
    ExeOpenDialog: TOpenDialog;
    actExeGenWindowsTemp: TAction;
    actExeGenDxbxPath: TAction;
    actExeGenManual: TAction;
    Import1: TMenuItem;
    Export1: TMenuItem;
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
    sSkinManager1: TsSkinManager;
    sSkinProvider1: TsSkinProvider;
    imgSignature1: TImage;
    imgSignature2: TImage;
    imgSignature3: TImage;
    imgSignature4: TImage;
    StatusBar: TsStatusBar;
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
    procedure ActAboutExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actExeGenWindowsTempExecute(Sender: TObject);
    procedure actExeGenDxbxPathExecute(Sender: TObject);
    procedure actExeGenManualExecute(Sender: TObject);
    procedure actExportLogoExecute(Sender: TObject);
    procedure actXdkTrackerExecute(Sender: TObject);
    procedure actXIsoExecute(Sender: TObject);
    procedure actXdkTrackerXbeInfoExecute(Sender: TObject);
  private
    m_Xbe: TXbe;
    m_XbeFilename: string;
    m_ExeFilename: string;

    m_AutoConvertToExe: EnumAutoConvert;

    m_bExeChanged: Boolean;
    m_bCanStart: Boolean;

    procedure SetExeGen(ConvertTo: EnumAutoConvert);

    procedure CloseXbe;
    procedure SaveXbe(aFileName: string = '');
    procedure ImportExe(aFileName: string);

    procedure ReadSettingsIni;
    procedure WriteSettingsIni;

    procedure RecentXbeAdd(aFileName: string);
    procedure RecentExeAdd(aFileName: string);
    procedure ReopenXbe(Sender: TObject);
    procedure ReopenExe(Sender: TObject);

    function ConvertToExe(x_filename: string; x_bVerifyIfExists: Boolean): Boolean;
    function StartEmulation(x_AutoConvert: EnumAutoConvert): Boolean;

    function SendCommandToXdkTracker: Boolean;
  public
    FApplicationDir: string;

    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

var
  frm_Main: Tfrm_Main;

implementation

{$R *.DFM}

uses
  // Delphi
  IniFiles,
  uDxbxXml,
  // Dxbx
  ufrm_About, uConsts, uLog, uWindows;

//------------------------------------------------------------------------------

procedure Tfrm_Main.actOpenXbeExecute(Sender: TObject);
begin
  XbeOpenDialog.Filter := DIALOG_FILTER_XBE;
  if not XbeOpenDialog.Execute then
    Exit;

  if Assigned(m_Xbe) then
    CloseXbe();

  OpenXbe(XbeOpenDialog.Filename, m_Xbe, m_ExeFilename, m_XbeFilename, StatusBar );
  RecentXbeAdd(XbeOpenDialog.Filename);
  Logbitmap1.Enabled := True;
  Dumpxbeinfoto1.Enabled := True;
  Exportexe1.Enabled := True;
  CloseXbe1.Enabled := True;
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
  ConvertToExe('', True);
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
  FreeAndNil({var}m_Xbe);
  Logbitmap1.Enabled := False;
  Dumpxbeinfoto1.Enabled := False;
  Exportexe1.Enabled := False;
  CloseXbe1.Enabled := False;

  WriteLog(Format('DXBX: %s Closed...', [m_szAsciiTitle]));
  StatusBar.SimpleText := 'DXBX:';
end; // Tfrm_Main.CloseXbe

//------------------------------------------------------------------------------

procedure Tfrm_Main.SaveXbe(aFileName: string);
begin

  (*
	// ******************************************************************
	// * ask permission to overwrite if file exists
	// ******************************************************************
	if(_access(x_filename, 0) != -1)
	{
		if(MessageBox(m_hwnd, "Overwrite existing file?", "Cxbx", MB_ICONQUESTION | MB_YESNO) != IDYES)
			return;
	}

	// ******************************************************************
	// * export xbe file
	// ******************************************************************
	{
        m_Xbe->Export(x_filename);

		if(m_Xbe->GetError() != 0)
			MessageBox(m_hwnd, m_Xbe->GetError(), "Cxbx", MB_ICONSTOP | MB_OK);
        else
        {
            char buffer[255];

            sprintf(buffer, "%s was successfully saved.", m_Xbe->m_szAsciiTitle);

            printf("WndMain: %s was successfully saved.\n", m_Xbe->m_szAsciiTitle);

            MessageBox(m_hwnd, buffer, "Cxbx", MB_ICONINFORMATION | MB_OK);

            m_bXbeChanged = False;
		}
	} *)
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
  actExeGenWindowsTemp.Checked := ConvertTo = CONVERT_TO_WINDOWSTEMP;
  actExeGenDxbxPath.Checked := ConvertTo = CONVERT_TO_XBEPATH;
  actExeGenManual.Checked := ConvertTo = CONVERT_TO_MANUAL;
end;

//------------------------------------------------------------------------------

function Tfrm_Main.ConvertToExe(x_filename: string; x_bVerifyIfExists: Boolean): Boolean;
var
  Filename: string;
  i_EmuExe: TEmuExe;
begin
  Result := False;
  Filename := 'default.exe';

  if x_filename = '' then
  begin
    if ExeSaveDialog.Execute then
      Filename := ExeSaveDialog.Filename
    else
      Filename := '';
  end
  else
    Filename := x_filename;

  if Filename <> '' then
  begin
    // ask permission to overwrite if file exists
    if x_bVerifyIfExists then
    begin
      if FileExists(Filename) then
      begin
        if MessageDlg('Overwrite existing file?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
          Exit;

        DeleteFile(Filename);
      end;
    end;

    // convert file
    try
      i_EmuExe := TEmuExe.Create(m_Xbe, m_KrnlDebug, m_KrnlDebugFilename, Self.Handle);
      try
        if i_EmuExe.doExport(Filename) then
        begin
          m_ExeFilename := Filename;
          WriteLog(m_szAsciiTitle + ' was converted to .exe.');
          m_bExeChanged := False;
          RecentExeAdd(Filename);
          Result := True;
        end
        else
          WriteLog('Export: Error converting ' + m_szAsciiTitle + ' to .exe');
      finally
        FreeAndNil(i_EmuExe);    
      end;
    except
      MessageDlg('Error converting to .exe', mtError, [mbOK], 0);
    end;
  end;
end; // Tfrm_Main.ConvertToExe

//------------------------------------------------------------------------------

procedure Tfrm_Main.actConfigControllerExecute(Sender: TObject);
begin
  frm_ControllerConfig := Tfrm_ControllerConfig.Create(nil);

  if frm_ControllerConfig.ShowModal = mrOk then
  begin

  end;

  FreeAndNil({var}frm_ControllerConfig);
end; // Tfrm_Main.actConfigControllerExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actConfigVideoExecute(Sender: TObject);
begin
  frm_VideoConfig := Tfrm_VideoConfig.Create(nil);

  if frm_VideoConfig.ShowModal = mrOk then
  begin

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
      // register xbe path with Cxbx.dll
      CxbxKrnl_SetXbePath(PChar(m_Xbe.m_szPath));

      try
        if FileExists(m_ExeFilename) then
        begin
          m_bCanStart := (ShellExecute(Application.Handle, 'open', PChar(m_ExeFilename), nil, PChar(ExtractFilePath(m_ExeFilename)), SW_SHOWDEFAULT) >= 32);
          if m_bCanStart then
            WriteLog('WndMain: ' + m_szAsciiTitle + ' emulation started.')
          else
          begin
            WriteLog('WndMain: ' + m_szAsciiTitle + ' shell failed.');
            TaskMessageDlg('Dxbx', 'Emulation failed.'#13#13'Try converting again. If this message repeats, the Xbe is not supported.', mtError, [{MB_ICONSTOP,}mbOK], 0);
          end
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
  szTempPath: string;
begin
  Result := False;
  // Convert Xbe to Exe, if necessary
  if (m_ExeFilename = '\0') or (m_bExeChanged) then
  begin
    case x_AutoConvert of
      CONVERT_TO_WINDOWSTEMP:
        begin
          szTempPath := GetTempDirectory;
          try
            Result := ConvertToExe(szTempPath + ExtractFileName(ChangeFileExt(m_XbeFilename, '.exe')), False);
          except
          end;
        end;
      CONVERT_TO_XBEPATH:
        begin
          try
            Result := ConvertToExe(ExtractFileName(ChangeFileExt(m_XbeFilename, '.exe')), False);
          except
          end;
        end;
    else
        try
          Result := ConvertToExe('', True);
        except
        end;
    end; // case
  end;
end; // Tfrm_Main.StartEmulation

//------------------------------------------------------------------------------

procedure Tfrm_Main.FormCreate(Sender: TObject);
begin
  m_AutoConvertToExe := CONVERT_TO_WINDOWSTEMP;
  FApplicationDir := ExtractFilePath(Application.ExeName);

  StatusBar.SimpleText := 'DXBX: No Xbe Loaded...';   

  ReadSettingsIni;
  CreateLogs(ltGui);

  if IsWindowsVista then
    sSkinManager1.SkinningRules := [srStdForms, srThirdParty]
  else
    sSkinManager1.SkinningRules := [srStdForms, srStdDialogs, srThirdParty];
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

//------------------------------------------------------------------------------

procedure Tfrm_Main.ReadSettingsIni;
var
  DxbxIniFilePath: string;
  IniFile: TIniFile;
  RecentTMP: string;
  RecentTMPList: TStringList;
  I: Integer;
begin
  DxbxIniFilePath := FApplicationDir + 'Dxbx.Ini';
  if FileExists(DxbxIniFilePath) then
  begin
    IniFile := TIniFile.Create(DxbxIniFilePath);

    case IniFile.ReadInteger('Settings', 'AutoConvertToExe', 1) of
      1: SetExeGen(CONVERT_TO_WINDOWSTEMP);
      2: SetExeGen(CONVERT_TO_XBEPATH);
      3: SetExeGen(CONVERT_TO_MANUAL);
    else
      SetExeGen(CONVERT_TO_MANUAL);
    end;

    m_DxbxDebug := DebugMode(IniFile.ReadInteger('Settings', 'DxbxDebug', Ord(DM_NONE)));
    m_DxbxDebugFilename := IniFile.ReadString('Settings', 'DxbxDebugFilename', '');
    m_KrnlDebug := DebugMode(IniFile.ReadInteger('Settings', 'KrnlDebug', Ord(DM_NONE)));
    m_KrnlDebugFilename := IniFile.ReadString('Settings', 'KrnlDebugFilename', '');

    // Read recent XBE files
    RecentTMP := IniFile.ReadString('Recent', 'XBEs', '');
    RecentTMPList := TStringList.Create;
    RecentTMPList.Clear;
    RecentTMPList.Delimiter := '|';
    RecentTMPList.StrictDelimiter := True;
    RecentTMPList.DelimitedText := RecentTMP;
    RecentXbefiles1.Clear;
    for I := 0 to RecentTMPList.Count - 1 do
      RecentXbeAdd(RecentTMPList[I]);

    // Read recent EXE files
    RecentTMP := IniFile.ReadString('Recent', 'EXEs', '');
    RecentTMPList := TStringList.Create;
    RecentTMPList.Clear;
    RecentTMPList.Delimiter := '|';
    RecentTMPList.StrictDelimiter;
    RecentTMPList.DelimitedText := RecentTMP;
    RecentExefiles1.Clear;
    for I := 0 to RecentTMPList.Count - 1 do
      RecentExeAdd(RecentTMPList[I]);

    FreeAndNil({var}IniFile);
    // Set Menu checked options
    case m_DxbxDebug of
      DM_NONE: begin
          actConsoleDebugGui.Checked := False;
          actFileDebugGui.Checked := False;
        end;
      DM_CONSOLE: begin
          actConsoleDebugGui.Checked := True;
          actFileDebugGui.Checked := False;
        end;
      DM_FILE: begin
          actConsoleDebugGui.Checked := False;
          actFileDebugGui.Checked := True;
        end;
    end;
    case m_KrnlDebug of
      DM_NONE: begin
          actConsoleDebugKernel.Checked := False;
          actFileDebugKernel.Checked := False;
        end;
      DM_CONSOLE: begin
          actConsoleDebugKernel.Checked := True;
          actFileDebugKernel.Checked := False;
        end;
      DM_FILE: begin
          actConsoleDebugKernel.Checked := False;
          actFileDebugKernel.Checked := True;
        end;
    end;

  end
  else
  begin
    // Setting defaults
    m_DxbxDebug := DM_CONSOLE;
    m_KrnlDebug := DM_CONSOLE;

    actConsoleDebugGui.Checked := True;
    actFileDebugGui.Checked := False;
    actConsoleDebugKernel.Checked := True;
    actFileDebugKernel.Checked := False;

    SetExeGen ( CONVERT_TO_MANUAL );
  end;
end; // Tfrm_Main.ReadSettingsIni

//------------------------------------------------------------------------------

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
  if RecentXbefiles1.Count >= 1 then
    RecentTMP := RecentXbefiles1.Items[0].Hint;

  for I := 1 to RecentXbefiles1.Count - 1 do
    RecentTMP := RecentTMP + '|' + RecentXbefiles1.Items[I].Hint;

  IniFile.WriteString('Recent', 'XBEs', RecentTMP);

  // Recent EXEs write
  RecentTMP := '';
  if RecentExefiles1.Count >= 1 then
    RecentTMP := RecentExefiles1.Items[0].Hint;
  for I := 0 to RecentExefiles1.Count - 1 do
    RecentTMP := RecentTMP + '|' + RecentExefiles1.Items[I].Hint;

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

  FreeAndNil({var}inifile);
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
  end
  else
  begin
    SaveDialog.Filename := 'DxbxDebug.txt';
    SaveDialog.Filter := DIALOG_FILTER_TEXT;
    if SaveDialog.Execute then
    begin
      CloseLogs;
      actConsoleDebugGui.Checked := False;
      actFileDebugGui.Checked := True;
      m_DxbxDebug := DM_FILE;
      m_DxbxDebugFilename := SaveDialog.Filename;
      CreateLogs(ltGui);
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
  end
  else
  begin
    SaveDialog.Filename := 'KernelDebug.txt';
    SaveDialog.Filter := DIALOG_FILTER_TEXT;
    if SaveDialog.Execute then
    begin
      actConsoleDebugKernel.Checked := False;
      actFileDebugKernel.Checked := True;
      m_KrnlDebug := DM_FILE;
      m_KrnlDebugFilename := SaveDialog.Filename;
    end;
  end;
end; // Tfrm_Main.actFileDebugKernelExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.ActAboutExecute(Sender: TObject);
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
  WriteSettingsIni;
  inherited Destroy;
end; // Tfrm_Main.Create

//------------------------------------------------------------------------------

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
  for I := 0 to RecentXbefiles1.Count - 1 do
  begin
    if RecentXbefiles1.Items[I].Hint = aFileName then
    begin
      RecentXbefiles1.Remove(RecentXbefiles1.Items[I]);
      Break;
    end;
  end;

  TempItem := TMenuItem.Create(RecentXbefiles1);
  TempItem.Hint := aFileName;
  TempItem.Caption := ExtractFileName(aFileName);
  TempItem.OnClick := ReopenXbe;

  while (RecentXbefiles1.Count >= _RecentXbeLimit) do
    RecentXbefiles1.Remove(RecentXbefiles1.Items[9]);

  RecentXbefiles1.Insert(0, TempItem);
  RecentXbefiles1.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.RecentExeAdd(aFileName: string);
var
  TempItem: TMenuItem;
  I: Integer;
begin
  for I := 0 to RecentExefiles1.Count - 1 do
  begin
    if RecentExefiles1.Items[I].Hint = aFileName then
    begin
      RecentExefiles1.Remove(RecentExefiles1.Items[I]);
      Break;
    end;
  end;

  TempItem := TMenuItem.Create(RecentExefiles1);
  TempItem.Hint := aFileName;
  TempItem.Caption := ExtractFileName(aFileName);
  TempItem.OnClick := ReopenExe;

  while (RecentExefiles1.Count >= _RecentExeLimit) do
    RecentExefiles1.Remove(RecentExefiles1.Items[9]);

  RecentExefiles1.Insert(0, TempItem);
  RecentExefiles1.Enabled := True;
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

    OpenXbe(TempItem.Hint, m_Xbe, m_ExeFilename, m_XbeFilename, StatusBar );
    Logbitmap1.Enabled := True;
    Dumpxbeinfoto1.Enabled := True;
    Exportexe1.Enabled := True;
    CloseXbe1.Enabled := True;

    RecentXbeAdd(TempItem.Hint)
  end
  else
  begin
    ShowMessage('Could not locate file : ' + TempItem.Hint);
    RecentXbefiles1.Remove(TempItem);
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
    RecentExeAdd(TempItem.Hint);
  end
  else
  begin
    ShowMessage('Could not locate file : ' + TempItem.Hint);
    RecentExefiles1.Remove(TempItem);
  end;
end;

//------------------------------------------------------------------------------

end.

