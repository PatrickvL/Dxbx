unit ufrm_Main;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, ShellAPI, jpeg, ExtCtrls, ComCtrls,
  // Dxbx
  uXbe, uEmuExe, uEnums, ufrm_ControllerConfig, ufrm_VideoConfig, xmldom,
  XMLIntf, msxmldom, XMLDoc, uXml, sSkinProvider, sSkinManager, sStatusBar;


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
    XMLDocument: TXMLDocument;
    StatusBar: TsStatusBar;
    sSkinManager1: TsSkinManager;
    sSkinProvider1: TsSkinProvider;
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
    { Private declarations }
    m_Xbe: TXbe;
    m_XbeFilename: string;
    m_ExeFilename: string;

    m_AutoConvertToExe: EnumAutoConvert;

    m_bExeChanged: boolean;

    procedure CloseXbe;
    procedure XbeLoaded;
    procedure LoadLogo;
    procedure OpenXbe(aFileName: string);
    procedure SaveXbe(aFileName: string = '');
    procedure ImportExe(aFileName: string);

    procedure CreateXmlXbeDump(aFileName: string);

    procedure ReadSettingsIni;
    procedure WriteSettingsIni;

    procedure RecentXbeAdd(aFileName: string);
    procedure RecentExeAdd(aFileName: string);
    procedure ReopenXbe(Sender: TObject);
    procedure ReopenExe(Sender: TObject);

    function ConvertToExe(x_filename: string; x_bVerifyIfExists: Boolean): boolean;
    function StartEmulation(x_AutoConvert: EnumAutoConvert): Boolean;

    function SendCommandToXdkTracker: Boolean;
  public
    FApplicationDir: string;

    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

  { TODO : Need to be inserted later }
  //procedure SetXbePath(const path : String ); external 'DxbxKrnl.dll';

var
  frm_Main: Tfrm_Main;

implementation

{$R *.DFM}

uses
  ufrm_About, IniFiles, uConsts, uLog, g_EmuShared;

//------------------------------------------------------------------------------

function IsWindowsVista: Boolean;
var VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  Result := VerInfo.dwMajorVersion >= 6;
end;
     
//------------------------------------------------------------------------------

function GetTempDirectory: string;
var
  Buffer: array[0..Max_path] of char;
begin
  FillChar(Buffer, Max_Path + 1, 0);
  GetTempPath(Max_path, Buffer);
  Result := string(Buffer);
  if Result[Length(Result)] <> '\' then Result := Result + '\';
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.actOpenXbeExecute(Sender: TObject);
begin
  XbeOpenDialog.Filter := 'Xbox Executables (*.xbe)|*.xbe';
  if XbeOpenDialog.Execute then begin

    if Assigned(m_Xbe) then
    begin
      CloseXbe();
    end;

    OpenXbe(XbeOpenDialog.FileName);
    RecentXbeAdd(XbeOpenDialog.FileName);
    Logbitmap1.Enabled := True;
    Dumpxbeinfoto1.Enabled := True;
    Exportexe1.Enabled := True;
    CloseXbe1.Enabled := true;
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
  SaveDialog.Filter := 'Xbox Executables (*.xbe)|*.xbe';
  if m_XbeFilename <> '' then begin
    SaveXbe();
  end
  else
  begin
    if SaveDialog.Execute then begin
      SaveXbe(SaveDialog.FileName);
    end;
  end;
end; // Tfrm_Main.actSaveXbeExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actSaveXbeAsExecute(Sender: TObject);
begin
  SaveXbe(SaveDialog.FileName);
end; // Tfrm_Main.actSaveXbeAsExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actImportExeExecute(Sender: TObject);
begin
  XbeOpenDialog.Filter := 'Windows Executables (*.exe)|*.exe';
  if XbeOpenDialog.Execute then begin

    if Assigned(m_Xbe) then
    begin
      CloseXbe();
    end;

    ImportExe(XbeOpenDialog.FileName);
    RecentExeAdd(XbeOpenDialog.FileName);
  end;
end; // Tfrm_Main.actImportExeExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actExportExeExecute(Sender: TObject);
begin
  ConvertToExe('', true);
end; // Tfrm_Main.actExportExeExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteSettingsIni;

  CloseLogs;

  if Assigned(m_Xbe) then
  begin
    CloseXbe();
  end;
end; // Tfrm_Main.FormClose

//------------------------------------------------------------------------------

procedure Tfrm_Main.CloseXbe;
begin
  FreeAndNil(m_Xbe);
  Logbitmap1.Enabled := False;
  Dumpxbeinfoto1.Enabled := False;
  Exportexe1.Enabled := False;
  CloseXbe1.Enabled := False;

  WriteLog('DXBX: ' + m_szAsciiTitle + '  Closed...');
  StatusBar.SimpleText := 'DXBX:';       
end; // Tfrm_Main.CloseXbe

//------------------------------------------------------------------------------

procedure Tfrm_Main.OpenXbe(aFileName: string);
begin
  if Assigned(m_Xbe) or not (FileExists(aFileName)) then
    Exit;

  m_ExeFilename := '\0';
  m_XbeFilename := aFileName;

  try
    m_Xbe := TXbe.Create(m_XbeFilename);

    XbeLoaded;
    StatusBar.SimpleText := 'DXBX: ' + m_szAsciiTitle + ', Loaded';
  except
    Messagedlg('Can not open Xbe file.', mtWarning, [mbOk], 0);
    m_Xbe.Free;
  end;
end; // Tfrm_Main.OpenXbe

//------------------------------------------------------------------------------

procedure Tfrm_Main.SaveXbe(aFileName: string);
begin
  { TODO : Need to inserted }
end;
      
//------------------------------------------------------------------------------

function Tfrm_Main.SendCommandToXdkTracker: Boolean;
var
  stringToSend: string;
  copyDataStruct: TCopyDataStruct;

  function SendData(copyDataStruct: TCopyDataStruct): Boolean;
  var
    receiverHandle: THandle;
    res: integer;
  begin
    Result := False;
    receiverHandle := FindWindow(PChar('TfrmXdkTracker'), nil);
    if receiverHandle = 0 then
    begin
      Exit;
    end;

    res := SendMessage(receiverHandle, WM_COPYDATA, Integer(Handle), Integer(@copyDataStruct));
    if res > 0 then Result := True;
  end;

begin
  stringToSend := 'READXML';

  copyDataStruct.dwData := Integer(0); //use it to identify the message contents
  copyDataStruct.cbData := 1 + Length(stringToSend);
  copyDataStruct.lpData := PChar(stringToSend);

  Result := SendData(copyDataStruct);
end;

//------------------------------------------------------------------------------

function Tfrm_Main.ConvertToExe(x_filename: string; x_bVerifyIfExists: Boolean): boolean;
var
  filename: string;
  i_EmuExe: TEmuExe;
begin
  Result := False;
  filename := 'default.exe';

  if x_filename = '' then begin
    if ExeSaveDialog.execute then begin
      filename := ExeSaveDialog.FileName;
    end
    else begin
      filename := '';
    end;
  end
  else begin
    filename := x_filename;
  end;

  if filename <> '' then begin

    // ask permission to overwrite if file exists
    if x_bVerifyIfExists then begin
      if FileExists(filename) then begin
        if MessageDlg('Overwrite existing file?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then begin
          Exit;
        end
        else begin
          DeleteFile(filename);
        end;
      end;
    end;

    // convert file
    try
      i_EmuExe := TEmuExe.Create(m_Xbe, m_KrnlDebug, m_KrnlDebugFilename);
      if i_EmuExe.doExport(filename) then begin
        m_ExeFilename := filename;
        WriteLog(m_szAsciiTitle + ' was converted to .exe.');
        m_bExeChanged := False;
        RecentExeAdd(filename);
        Result := True;
      end
      else begin
        WriteLog('Export: Error converting ' + m_szAsciiTitle + ' to .exe');
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

  if frm_ControllerConfig.ShowModal = mrOk then begin

  end;

  frm_ControllerConfig.Free;
end; // Tfrm_Main.actConfigControllerExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actConfigVideoExecute(Sender: TObject);
begin
  frm_VideoConfig := Tfrm_VideoConfig.Create(nil);

  if frm_VideoConfig.ShowModal = mrOk then begin

  end;

  frm_VideoConfig.Free;
end; // Tfrm_Main.actConfigVideoExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.ActStartEmulationExecute(Sender: TObject);
var FileConverted: Boolean;
begin
  if Assigned(m_Xbe) then begin
    FileConverted := StartEmulation(m_AutoConvertToExe);
    if FileConverted then begin

      // register xbe path with Cxbx.dll
      { TODO : Need to be inserted later }
      //SetXbePath( PChar ( m_Xbe.m_szPath ) );

      try
        if FileExists(m_ExeFilename) then begin

          ShellExecute(application.Handle, 'open', PChar(m_ExeFilename), nil, nil, SW_SHOWDEFAULT);
          WriteLog('WndMain: ' + m_szAsciiTitle + ' emulation started.');
        end
        else begin
          Messagedlg(m_ExeFilename + ' does not exists.', mtError, [mbOk], 0);
          WriteLog('WndMain: ' + m_ExeFilename + ' does not exists.');
        end;
      except
        Messagedlg('Emmulation failed. Try converting again. If this message repeats, the Xbe is not supported.', mtError, [mbOk], 0);
        WriteLog('WndMain: ' + m_szAsciiTitle + 'shell failed.');
      end;
    end;

  end
  else begin
    MessageDlg('No xbe file loaded', mtInformation, [mbOk], 0);
  end;
end; // Tfrm_Main.ActStartEmulationExecute

//------------------------------------------------------------------------------

function Tfrm_Main.StartEmulation(x_AutoConvert: EnumAutoConvert): boolean;
var
  szTempPath: string;
begin
  Result := False;
  // Convert Xbe to Exe, if necessary
  if (m_ExeFilename = '\0') or (m_bExeChanged) then begin
    case x_AutoConvert of
      AUTO_CONVERT_WINDOWS_TEMP:
        begin
          szTempPath := GetTempDirectory;
          try
            Result := ConvertToExe(szTempPath + ExtractFileName(ChangeFileExt(m_XbeFilename, '.exe')), false);
          except
          end;
        end;
      AUTO_CONVERT_XBE_PATH:
        begin
          try
            Result := ConvertToExe(ExtractFileName(ChangeFileExt(m_XbeFilename, '.exe')), false);
          except
          end;
        end;
    else begin
        try
          Result := ConvertToExe('', True);
        except
        end;
      end;
    end;
  end;
end; // Tfrm_Main.StartEmulation

//------------------------------------------------------------------------------

procedure Tfrm_Main.FormCreate(Sender: TObject);
begin
  m_AutoConvertToExe := AUTO_CONVERT_WINDOWS_TEMP;
  FApplicationDir := ExtractFilePath(Application.ExeName);

  ReadSettingsIni;
  CreateLogs(ltGui);

  if IsWindowsVista then begin
    sSkinManager1.SkinningRules := [srStdForms, srThirdParty];
  end
  else begin
    sSkinManager1.SkinningRules := [srStdForms, srStdDialogs, srThirdParty];
  end;
end; procedure Tfrm_Main.ImportExe(aFileName: string);
begin

end;

procedure Tfrm_Main.LoadLogo;
begin

end;

// Tfrm_Main.FormCreate

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
      1: begin
          m_AutoConvertToExe := AUTO_CONVERT_WINDOWS_TEMP;
          actExeGenWindowsTemp.Checked := True;
          actExeGenDxbxPath.Checked := False;
          actExeGenManual.Checked := False;
        end;
      2: begin
          m_AutoConvertToExe := AUTO_CONVERT_XBE_PATH;
          actExeGenWindowsTemp.Checked := False;
          actExeGenDxbxPath.Checked := True;
          actExeGenManual.Checked := False;
        end;
      3: begin
          m_AutoConvertToExe := AUTO_CONVERT_MANUAL;
          actExeGenWindowsTemp.Checked := False;
          actExeGenDxbxPath.Checked := False;
          actExeGenManual.Checked := True;
        end;
    end;
    m_DxbxDebug := DebugMode(IniFile.ReadInteger('Settings', 'DxbxDebug', 0));
    m_DxbxDebugFilename := IniFile.ReadString('Settings', 'DxbxDebugFilename', '');
    m_KrnlDebug := DebugMode(IniFile.ReadInteger('Settings', 'KrnlDebug', 0));
    m_KrnlDebugFilename := IniFile.ReadString('Settings', 'KrnlDebugFilename', '');

    // Read recent XBE files
    RecentTMP := IniFile.ReadString('Recent', 'XBEs', '');
    RecentTMPList := TStringList.Create;
    RecentTMPList.Clear;
    RecentTMPList.Delimiter := '|';
    RecentTMPList.StrictDelimiter := True;
    RecentTMPList.DelimitedText := RecentTMP;
    RecentXbefiles1.Clear;
    for I := 0 to RecentTMPList.Count - 1 do begin
      RecentXbeAdd(RecentTMPList[I]);
    end;

    // Read recent EXE files
    RecentTMP := IniFile.ReadString('Recent', 'EXEs', '');
    RecentTMPList := TStringList.Create;
    RecentTMPList.Clear;
    RecentTMPList.Delimiter := '|';
    RecentTMPList.StrictDelimiter;
    RecentTMPList.DelimitedText := RecentTMP;
    RecentExefiles1.Clear;
    for I := 0 to RecentTMPList.Count - 1 do begin
      RecentExeAdd(RecentTMPList[I]);
    end;

    IniFile.Free;
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
  else begin
    // Setting defaults
    m_DxbxDebug := DM_CONSOLE;
    m_KrnlDebug := DM_CONSOLE;

    actConsoleDebugGui.Checked := True;
    actFileDebugGui.Checked := False;
    actConsoleDebugKernel.Checked := True;
    actFileDebugKernel.Checked := False;

    m_AutoConvertToExe := AUTO_CONVERT_WINDOWS_TEMP;
    actExeGenWindowsTemp.Checked := True;
    actExeGenDxbxPath.Checked := False;
    actExeGenManual.Checked := False;
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

  for I := 1 to RecentXbefiles1.Count - 1 do begin
    RecentTMP := RecentTMP + '|' + RecentXbefiles1.Items[I].Hint;
  end;
  IniFile.WriteString('Recent', 'XBEs', RecentTMP);

  // Recent EXEs write
  RecentTMP := '';
  if RecentExefiles1.Count >= 1 then
    RecentTMP := RecentExefiles1.Items[0].Hint;
  for I := 0 to RecentExefiles1.Count - 1 do begin
    RecentTMP := RecentTMP + '|' + RecentExefiles1.Items[I].Hint;
  end;
  IniFile.WriteString('Recent', 'EXEs', RecentTMP);

  case m_AutoConvertToExe of
    AUTO_CONVERT_WINDOWS_TEMP: IniFile.WriteInteger('Settings', 'AutoConvertToExe', 1);
    AUTO_CONVERT_XBE_PATH: IniFile.WriteInteger('Settings', 'AutoConvertToExe', 2);
    AUTO_CONVERT_MANUAL: IniFile.WriteInteger('Settings', 'AutoConvertToExe', 3);
  end;
  IniFile.WriteInteger('Settings', 'DxbxDebug', ORD(m_DxbxDebug));
  IniFile.WriteString('Settings', 'DxbxDebugFilename', m_DxbxDebugFilename);
  IniFile.WriteInteger('Settings', 'KrnlDebug', ORD(m_KrnlDebug));
  IniFile.WriteString('Settings', 'KrnlDebugFilename', m_KrnlDebugFilename);

  inifile.free;
end; // Tfrm_Main.WriteSettingsIni

//------------------------------------------------------------------------------

procedure Tfrm_Main.actConsoleXbeInfoExecute(Sender: TObject);
begin
  // dump xbe information to debug console
  if m_Xbe.DumpInformation then begin
    WriteLog(m_szAsciiTitle + '`s .xbe info was successfully dumped.');
  end
  else begin
    WriteLog(m_szAsciiTitle + '`s .xbe info was successfully dumped.');
  end;
end; // Tfrm_Main.actConsoleXbeInfoExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actFileXbeInfoExecute(Sender: TObject);
begin
  SaveDialog.FileName := 'xbe.txt';
  SaveDialog.Filter := 'Text Documents ( *.txt )|*.txt';

  if SaveDialog.Execute then begin
    // ask permisssion to override if file exists
    if FileExists(SaveDialog.FileName) then begin
      if MessageDlg('Overwrite existing file?', mtConfirmation, [mbYes, mbNo], -1) = mrNo then begin
        Exit;
      end;
    end;

    m_Xbe.DumpInformation(SaveDialog.FileName);
    WriteLog(m_szAsciiTitle + '''s .xbe info was successfully dumped.');
    MessageDlg(m_szAsciiTitle + '''s .xbe info was successfully dumped.', mtInformation, [mbOk], 0);
  end;
end; // Tfrm_Main.actFileXbeInfoExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actConsoleDebugGuiExecute(Sender: TObject);
begin
  if m_DxbxDebug = DM_CONSOLE then begin
    actConsoleDebugGui.Checked := False;
    m_DxbxDebug := DM_NONE;
    CloseLogs;
  end
  else begin
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
  if m_DxbxDebug = DM_FILE then begin
    actFileDebugGui.Checked := FALSE;
    m_DxbxDebug := DM_NONE;
    CloseLogs;
  end
  else begin
    SaveDialog.FileName := 'DxbxDebug.txt';
    SaveDialog.Filter := 'Text Documents ( *.txt )|*.txt';
    if SaveDialog.Execute then begin
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
  actConsoleDebugKernel.Checked := True;
  actFileDebugKernel.Checked := False;
end; // Tfrm_Main.actConsoleDebugKernelExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actFileDebugKernelExecute(Sender: TObject);
begin
  SaveDialog.FileName := 'KernelDebug.txt';
  SaveDialog.Filter := 'Text Documents ( *.txt )|*.txt';

  if SaveDialog.Execute then begin
    actFileDebugKernel.Checked := True;
    actConsoleDebugKernel.Checked := False;
  end;
end; // Tfrm_Main.actFileDebugKernelExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.XbeLoaded;
begin
  LoadLogo();
  WriteLog('DXBX: ' + m_szAsciiTitle + ' loaded.');
end; // Tfrm_Main.XbeLoaded

//------------------------------------------------------------------------------

procedure Tfrm_Main.ActAboutExecute(Sender: TObject);
begin
  frm_About := Tfrm_About.Create(Self);

  if frm_About.ShowModal = mrOk then begin
  end;

  frm_About.free;
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
  inherited;
end; // Tfrm_Main.Create

//------------------------------------------------------------------------------

procedure Tfrm_Main.AfterConstruction;
begin
  inherited;
  ReadSettingsIni;
end; // Tfrm_Main.AfterConstruction

//------------------------------------------------------------------------------

procedure Tfrm_Main.actExeGenWindowsTempExecute(Sender: TObject);
begin
  m_AutoConvertToExe := AUTO_CONVERT_WINDOWS_TEMP;
  actExeGenWindowsTemp.Checked := True;
  actExeGenDxbxPath.Checked := False;
  actExeGenManual.Checked := False;
end; // Tfrm_Main.actExeGenWindowsTempExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actExeGenDxbxPathExecute(Sender: TObject);
begin
  m_AutoConvertToExe := AUTO_CONVERT_XBE_PATH;
  actExeGenWindowsTemp.Checked := False;
  actExeGenDxbxPath.Checked := True;
  actExeGenManual.Checked := False;
end; // Tfrm_Main.actExeGenDxbxPathExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actExeGenManualExecute(Sender: TObject);
begin
  m_AutoConvertToExe := AUTO_CONVERT_MANUAL;
  actExeGenWindowsTemp.Checked := False;
  actExeGenDxbxPath.Checked := False;
  actExeGenManual.Checked := True;
end; // Tfrm_Main.actExeGenManualExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actExportLogoExecute(Sender: TObject);
var
  bmp: TBitmap;
begin
  if LogoSaveDialog.Execute then begin
    // ask permission to overwrite if file exists
    if FileExists(LogoSaveDialog.FileName) then begin
      if (MessageDlg('Overwrite existing file?', mtConfirmation, [mbYes, mbNo], -1) = mrYes) then begin
        DeleteFile(LogoSaveDialog.FileName);
      end
      else begin
        Exit;
      end;
    end;

    // export logo bitmap
    bmp := TBitmap.Create;
    try
      bmp.Width := 100;
      bmp.Height := 17;
      m_Xbe.ExportLogoBitmap(bmp);
      bmp.SaveToFile(LogoSaveDialog.FileName);
    finally
      WriteLog(m_szAsciiTitle + '''s logo bitmap was successfully exported.');
    end;
  end;
end; // Tfrm_Main.actExportLogoExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actXdkTrackerExecute(Sender: TObject);
begin
  if FileExists(FApplicationDir + 'Tools\XdkTracker.exe') then begin
    ShellExecute(0, 'open', PChar(FApplicationDir + 'Tools\XdkTracker.exe'), nil, nil, SW_SHOWNORMAL);
  end;
end; // Tfrm_Main.actXdkTrackerExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actXIsoExecute(Sender: TObject);
begin
  if FileExists(FApplicationDir + 'Tools\xIso.exe') then begin
    ShellExecute(0, 'open', PChar(FApplicationDir + 'Tools\xIso.exe'), nil, nil, SW_SHOWNORMAL);
  end;
end; // Tfrm_Main.actXIsoExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.actXdkTrackerXbeInfoExecute(Sender: TObject);
var
  DumpFilePath: string;
begin
  if FileExists(FApplicationDir + 'Tools\XdkTracker.exe') then begin

    DumpFilePath := FApplicationDir + 'Tools\Dump.dat';
    CreateXmlXbeDump(DumpFilePath);

    if not SendCommandToXdkTracker then
      ShellExecute(0, 'open', PChar(FApplicationDir + 'Tools\XdkTracker.exe'), '/XBEDUMP', nil, SW_SHOWNORMAL);
  end;
end; // Tfrm_Main.actXdkTrackerXbeInfoExecute

//------------------------------------------------------------------------------

procedure Tfrm_Main.CreateXmlXbeDump(aFileName: string);
var
  XmlRootNode: iXmlNode;
  lIndex: Integer;
  Version: string;
begin
  XMLDocument.Active := False;
  XMLDocument.Active := true;
  XmlRootNode := XMLDocument.AddChild('XDKINFO');

  XML_WriteString(XmlRootNode, 'Name', m_szAsciiTitle);

  for lIndex := 0 to m_Xbe.m_Header.dwLibraryVersions - 1 do begin
    Version := IntToStr(m_Xbe.m_LibraryVersion[lIndex].wMajorVersion) + '.' +
      IntToStr(m_Xbe.m_LibraryVersion[lIndex].wMinorVersion) + '.' +
      IntToStr(m_Xbe.m_LibraryVersion[lIndex].wBuildVersion);

    case lIndex of
      0: XML_WriteString(XmlRootNode, 'XAPILIB', Version);
      1: XML_WriteString(XmlRootNode, 'XBOXKRNL', Version);
      2: XML_WriteString(XmlRootNode, 'LIBCMT', Version);
      3: XML_WriteString(XmlRootNode, 'D3D8', Version);
      4: XML_WriteString(XmlRootNode, 'XGRAPHC', Version);
      5: XML_WriteString(XmlRootNode, 'DSOUND', Version);
      6: XML_WriteString(XmlRootNode, 'XMV', Version);
    end;
  end;

  XMLDocument.SaveToFile(aFileName);
end; // Tfrm_Main.CreateXmlXbeDump

//------------------------------------------------------------------------------

procedure Tfrm_Main.RecentXbeAdd(aFileName: string);
var
  TempItem: TMenuItem;
  I: Integer;
begin
  for I := 0 to RecentXbefiles1.Count - 1 do begin
    if RecentXbefiles1.Items[I].Hint = aFileName then begin
      RecentXbefiles1.Remove(RecentXbefiles1.Items[I]);
      Break;
    end;
  end;

  TempItem := TMenuItem.Create(RecentXbefiles1);
  TempItem.Hint := aFileName;
  TempItem.Caption := ExtractFileName(aFileName);
  TempItem.OnClick := ReopenXbe;

  while (RecentXbefiles1.Count >= _RecentXbeLimit) do begin
    RecentXbefiles1.Remove(RecentXbefiles1.Items[9]);
  end;

  RecentXbefiles1.Insert(0, TempItem);
  RecentXbefiles1.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.RecentExeAdd(aFileName: string);
var
  TempItem: TMenuItem;
  I: Integer;
begin
  for I := 0 to RecentExefiles1.Count - 1 do begin
    if RecentExefiles1.Items[I].Hint = aFileName then begin
      RecentExefiles1.Remove(RecentExefiles1.Items[I]);
      Break;
    end;
  end;

  TempItem := TMenuItem.Create(RecentExefiles1);
  TempItem.Hint := aFileName;
  TempItem.Caption := ExtractFileName(aFileName);
  TempItem.OnClick := ReopenExe;

  while (RecentExefiles1.Count >= _RecentExeLimit) do begin
    RecentExefiles1.Remove(RecentExefiles1.Items[9]);
  end;

  RecentExefiles1.Insert(0, TempItem);
  RecentExefiles1.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.ReopenXbe(Sender: TObject);
var
  TempItem: TMenuItem;
begin
  TempItem := Sender as TMenuItem;
  if FileExists(TempItem.Hint) then begin
    if Assigned(m_Xbe) then
      CloseXbe();
    OpenXbe(TempItem.Hint);
    Logbitmap1.Enabled := True;
    Dumpxbeinfoto1.Enabled := True;
    Exportexe1.Enabled := True;
    CloseXbe1.Enabled := true;

    RecentXbeAdd(TempItem.Hint)
  end
  else begin
    Showmessage('Could not locate file : ' + TempItem.Hint);
    RecentXbefiles1.Remove(TempItem);
  end;
end;

//------------------------------------------------------------------------------

procedure Tfrm_Main.ReopenExe(Sender: TObject);
var
  TempItem: TMenuItem;
begin
  TempItem := Sender as TMenuItem;
  if FileExists(TempItem.Hint) then begin
    ShellExecute(application.Handle, 'open', PChar(TempItem.Hint), nil, nil, SW_SHOWDEFAULT);
    RecentExeAdd(TempItem.Hint);
  end
  else begin
    Showmessage('Could not locate file : ' + TempItem.Hint);
    RecentExefiles1.Remove(TempItem);
  end;
end;
    
//------------------------------------------------------------------------------

end.

