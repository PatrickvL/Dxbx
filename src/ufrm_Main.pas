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
  Windows, SysUtils, StrUtils, Classes, Messages, Controls, StdCtrls, ComCtrls, ExtCtrls,
  Types, GraphUtil,
  Grids, Menus, ActnList, Forms, Dialogs, Graphics, jpeg,
  ShellAPI, IniFiles, ShlObj,
  xmldom, XMLIntf, msxmldom, XMLDoc,
  // Jedi Win32API
  JwaWinType,
  // Jcl
  JclShell, // SHDeleteFiles
  // Dxbx
  uTypes,
  uConsts,
  uLog,
  uWindows,
  uDxbxUtils,
  uFileSystem,
  uXbe,
  uEmuShared,
  uDxbxXml,
  uData,
  ufrm_Configuration,
  uImportGames,
  ufrm_About;

const
  clXboxGreen = $22CC88;//22BB77;//27BB73;//00DE97;//0DB366;//0FB869;

type
  Tfrm_Main = class(TForm)
    d: TMainMenu;
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
    mnu_Patch: TMenuItem;
    N3: TMenuItem;
    mnu_DumpxbeinfoTo: TMenuItem;
    mnu_DebugoutputGUI: TMenuItem;
    mnu_DebugoutputKernel: TMenuItem;
    mnu_ConfigControler: TMenuItem;
    Start1: TMenuItem;
    ActionList: TActionList;
    actStartEmulation: TAction;
    actAbout: TAction;
    actConfiguration: TAction;
    actClose: TAction;
    actOpenXbe: TAction;
    actCloseXbe: TAction;
    XbeOpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
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
    ExeSaveDialog: TSaveDialog;
    ools1: TMenuItem;
    xIso1: TMenuItem;
    actXIso: TAction;
    N8: TMenuItem;
    XDKTracker2: TMenuItem;
    actXdkTrackerXbeInfo: TAction;
    StatusBar: TStatusBar;
    ImageIcon: TImage;
    ImageLogo: TImage;
    miXbeExplorer: TMenuItem;
    actXbeExplorer: TAction;
    actCleanSymbolCache: TAction;
    miCleanSymbolCache: TMenuItem;
    dgXbeInfos: TDrawGrid;
    lblXbeInformation: TLabel;
    imgLaunchButton: TImage;
    XMLDocument: TXMLDocument;
    lblFreeTextFilter: TLabel;
    cbFreeTextFilter: TComboBox;
    actStopEmulation: TAction;
    Stop1: TMenuItem;
    N4: TMenuItem;
    Gamelist1: TMenuItem;
    Import1: TMenuItem;
    Export1: TMenuItem;
    actImportGameList: TAction;
    actExportGameList: TAction;
    ImportDialog: TOpenDialog;
    ExportDialog: TSaveDialog;
    mnu_BypassSymbolCache: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    Clear1: TMenuItem;
    actClearGameList: TAction;
    actDebugKernelNone: TAction;
    actDebugGuiNone: TAction;
    mnu_DebugOutputGuiNone: TMenuItem;
    mnu_DebugOutputKernelNone: TMenuItem;
    mnu_ImportXbes: TMenuItem;
    actImportXbes: TAction;
    procedure actStartEmulationExecute(Sender: TObject);
    procedure actOpenXbeExecute(Sender: TObject);
    procedure actCloseXbeExecute(Sender: TObject);
    procedure actConfigurationExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actConsoleXbeInfoExecute(Sender: TObject);
    procedure actFileXbeInfoExecute(Sender: TObject);
    procedure actConsoleDebugGuiExecute(Sender: TObject);
    procedure actFileDebugGuiExecute(Sender: TObject);
    procedure actConsoleDebugKernelExecute(Sender: TObject);
    procedure actFileDebugKernelExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actXIsoExecute(Sender: TObject);
    procedure actXdkTrackerXbeInfoExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actXbeExplorerExecute(Sender: TObject);
    procedure actCleanSymbolCacheExecute(Sender: TObject);
    procedure dgXbeInfosDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure imgLaunchButtonMouseLeave(Sender: TObject);
    procedure imgLaunchButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgLaunchButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgLaunchButtonMouseEnter(Sender: TObject);
    procedure imgLaunchButtonClick(Sender: TObject);
    procedure dgXbeInfosClick(Sender: TObject);
    procedure cbFreeTextFilterKeyPress(Sender: TObject; var Key: Char);
    procedure cbFreeTextFilterSelect(Sender: TObject);
    procedure actStopEmulationExecute(Sender: TObject);
    procedure actImportGameListExecute(Sender: TObject);
    procedure actExportGameListExecute(Sender: TObject);
    procedure dgXbeInfosDblClick(Sender: TObject);
    procedure dgXbeInfosKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mnu_BypassSymbolCacheClick(Sender: TObject);
    procedure actClearGameListExecute(Sender: TObject);
    procedure actDebugKernelNoneExecute(Sender: TObject);
    procedure actDebugGuiNoneExecute(Sender: TObject);
    procedure actImportXbesExecute(Sender: TObject);
  protected
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure WndProc(var Message: TMessage); override;
    procedure StopEmulation;
  private

    procedure SaveXBEList(const aFilePath, aPublishedBy: string);
    Function ImportXBEGameList(aImportFilePath: string = ''; aUseImportDialog: Boolean = False): Integer;
    function FindByFileName(const aFileName: string): Integer;
    function FindByName(const aName: string): Integer;

    procedure FindFiles(FilesList: TStringList; StartDir, FileMask: string);
    function BrowseDialog (const Title: string; const Flag: integer): string;

//    function ShowImportList(const XBEImportList: TStringList; Publisher: string): Integer;
    function FindDuplicate(const aXBEInfo: TXBEInfo): Integer;
    function _ReadXBEInfoFromNode(const XBEInfoNode: IXMLNode): TXBEInfo;
    procedure UpdateBackground;
    procedure UpdateLaunchButton;
    procedure UpdateTitleInformation;
    function StartTool(aToolName: string; const aParameters: string = ''): Boolean;
    procedure ShowXbeInfo(const aXbeInfo: TXbeInfo);
    function LoadXbe(const aFileName: string): Boolean;
    procedure OpenXbeFile(aFileName: string = '');
  private
    MyXBEList: TStringList;
    ApplicationDir: string;
    EnabledItems: array of TXbeInfo;
    procedure UpdateFilter;
    function InsertXBEInfo(const aXbeInfo: TXBEInfo; const aPreventDuplicates: Boolean = True): Boolean;
    function LoadXBEListByFile(aImportFilePath: string = ''; aUseImportDialog: Boolean = False): Integer;
    procedure SaveXBEListByFile(const aFilePath, aPublishedBy: string);

    function LoadXBEListByXml(aXml: String): Integer;
  private
    m_Xbe: TXbe;

    Emulation_State: EMU_STATE;

    OldLBWindowProc: TWndMethod;
    BackgroundImage: TBitmap;
    GUISelBg: TJPEGImage;
    GUISelBgLeft: TJPEGImage;
    GUISelBgRight: TJPEGImage;
    GUIHeadBgImg: TJPEGImage;
    GUIHeadSepImg: TJPEGImage;

    function GetEmuWindowHandle(const aEmuDisplayMode: Integer = 1): THandle;
    procedure LaunchXBE;
    procedure CloseXbe;

    procedure ReadSettingsIni;
    procedure WriteSettingsIni;

    procedure RecentXbeAdd(aFileName: string);
    procedure ReopenXbe(Sender: TObject);

    function SendCommandToXdkTracker: Boolean;

    procedure WMDROPFILES(var Msg: TMessage); //message WM_DROPFILES;
    procedure LBWindowProc(var Message: TMessage);

    procedure AdjustMenu;
    function GetCellText(aCol, aRow: Integer): string;
  public
    FApplicationDir: string;

    destructor Destroy; override;
  end;

var
  KernelDebugMode: TDebugMode = dmNone;

  KernelDebugFilePath: string = ''; // Just the default folder

  frm_Main: Tfrm_Main;

implementation

function BrowseDialogCallBack
  (Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM):
  integer stdcall;
var
  wa, rect : TRect;
  dialogPT : TPoint;
begin
  //center in work area
  if uMsg = BFFM_INITIALIZED then
  begin
    wa := Screen.WorkAreaRect;
    GetWindowRect(Wnd, Rect);
    dialogPT.X := ((wa.Right-wa.Left) div 2) -
                  ((rect.Right-rect.Left) div 2);
    dialogPT.Y := ((wa.Bottom-wa.Top) div 2) -
                  ((rect.Bottom-rect.Top) div 2);
    MoveWindow(Wnd,
               dialogPT.X,
               dialogPT.Y,
               Rect.Right - Rect.Left,
               Rect.Bottom - Rect.Top,
               True);
  end;

  Result := 0;
end;

function GetTitleSpecificKernelDebugFilePath: string;
begin
  // Was DXBX_KERNEL_DEBUG_FILENAME

  if KernelDebugFilePath[Length(KernelDebugFilePath) -1] = '\' then
    Result := KernelDebugFilePath + Format('DxbxKrnl %s (%d).txt', [m_szAsciiTitle, SvnRevision])
  else
    Result := KernelDebugFilePath + Format('\DxbxKrnl %s (%d).txt', [m_szAsciiTitle, SvnRevision]);
end;

procedure GradientHorizontalLineCanvas(const ACanvas: TCanvas;
  const AStartColor, AEndColor: TColor; const X, Y, Width: Integer); overload;
var
  ARect: TRect;
begin
  ARect := Types.Rect(X, Y, X + Width, Y + ACanvas.Pen.Width);
  GradientFillCanvas(ACanvas, AStartColor, AEndColor, ARect, gdHorizontal);
end;

procedure GradientVerticalLineCanvas(const ACanvas: TCanvas;
  const AStartColor, AEndColor: TColor; const X, Y, Height: Integer); overload;
var
  ARect: TRect;
begin
  ARect := Types.Rect(X, Y, X + ACanvas.Pen.Width, Y + Height);
  GradientFillCanvas(ACanvas, AStartColor, AEndColor, ARect, gdVertical);
end;

procedure GradientHorizontalLineCanvas(const ACanvas: TCanvas;
  const aColors: array of TColor; const X, Y, Width: Integer); overload;
var
  Steps: Integer;
  i: Integer;
  Current, Next: Integer;
begin
  Current := X;
  Steps := Length(aColors) - 1;
  for i := 1 to Steps do
  begin
    Next := X + ((Width * i) div Steps);
    GradientHorizontalLineCanvas(ACanvas, aColors[i-1], aColors[i], Current, Y, Next - Current);
    Current := Next;
  end;
end;

procedure GradientVerticalLineCanvas(const ACanvas: TCanvas;
  const aColors: array of TColor; const X, Y, Height: Integer); overload;
var
  Steps: Integer;
  i: Integer;
  Current, Next: Integer;
begin
  Current := Y;
  Steps := Length(aColors) - 1;
  for i := 1 to Steps do
  begin
    Next := Y + ((Height * i) div Steps);
    GradientVerticalLineCanvas(ACanvas, aColors[i-1], aColors[i], X, Current, Next - Current);
    Current := Next;
  end;
end;

{ Tfrm_Main }

{$R *.DFM}

const
  cXbeExplorerPath = 'XbeExplorer.exe';
  cXDKTrackerPath = 'XdkTracker.exe';
  cXIsoPath = 'xIso.exe';

var
  m_hwndChild: Handle = 0;

// Key messages can be trapped at the application level :
procedure Tfrm_Main.AppMessage(var Msg: TMsg; var Handled: Boolean);
begin
//case Msg.message of WM_PAINT,WM_NCMOUSEMOVE..WM_NCXBUTTONDBLCLK, WM_COMMAND..WM_GESTURENOTIFY, WM_MOUSEFIRST..WM_MOUSELAST, WM_NCMOUSEHOVER..WM_MOUSELEAVE:; else
//DbgPrintf('AppMessage() Msg.message = %d (%x)   LOWORD(Msg.wParam) = %d (%x)', [Msg.message, Msg.message, LOWORD(Msg.wParam), LOWORD(Msg.wParam)]);
//end;

  // Dxbx : Protect against a loss of the child handle (otherwise, the GUI we would hang) :
  if not IsWindow(m_hwndChild) then
    m_hwndChild := HNULL;

  Handled := False;
  case Msg.message of
//    WM_USER_PARENTNOTIFY,
//    WM_PARENTNOTIFY:
//      case LOWORD(Msg.wParam) of
//        WM_CREATE:
//        begin
//          m_hwndChild := GetWindow(Msg.hwnd, GW_CHILD);
//          UpdateTitleInformation;
//          Handled := True;
//        end;
//
//        WM_DESTROY:
//        begin
//          m_hwndChild := HNULL;
//          UpdateTitleInformation;
//          Handled := True;
//        end;
//      end;

    WM_SYSKEYDOWN:
      if m_hwndChild <> 0 then
      begin
        SendMessage(m_hwndChild, Msg.message, Msg.wParam, Msg.lParam);
        Handled := True;
      end;

    WM_KEYDOWN:
      case Msg.wParam of
        VK_F6:
        begin
          if (m_hwndChild <> 0) and (Emulation_State = esRunning) then
          begin
            StopEmulation;
            Handled := True;
          end;
        end;
      else
        if m_hwndChild <> 0 then
        begin
          SendMessage(m_hwndChild, Msg.message, Msg.wParam, Msg.lParam);
          Handled := True;
        end;
      end;
  end;
end;

function Tfrm_Main.BrowseDialog(const Title: string;
  const Flag: integer): string;
var
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;
  DisplayName : array[0..MAX_PATH] of char;
  TempPath : array[0..MAX_PATH] of char;
begin
  Result:='';
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do begin
    hwndOwner := Application.Handle;
    pszDisplayName := @DisplayName;
    lpszTitle := PChar(Title);
    ulFlags := Flag;
    lpfn := BrowseDialogCallBack;
  end;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then begin
    SHGetPathFromIDList(lpItemID, TempPath);
    Result := TempPath;
    GlobalFreePtr(lpItemID);
  end;
end;

procedure Tfrm_Main.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_USER_PARENTNOTIFY:
      case LOWORD(Message.WParam) of
        WM_CREATE:
        begin
          m_hwndChild := GetWindow(Self.Handle, GW_CHILD);
          UpdateTitleInformation;
          Message.Result := 1;
          Exit;
        end;
      end;

    WM_PARENTNOTIFY:
      case LOWORD(Message.WParam) of
        WM_DESTROY:
        begin
          m_hwndChild := HNULL;
          UpdateTitleInformation;
          Message.Result := 1;
          Exit;
        end;
      end;

//    WM_SYSKEYDOWN:
//      if m_hwndChild <> 0 then
//      begin
//        SendMessage(m_hwndChild, Message.Msg, Message.WParam, Message.LParam);
//        Message.Result := 1;
//        Exit;
//      end;
//
//    WM_KEYDOWN:
//      case Message.WParam of
//        VK_F6:
//        begin
//          if (m_hwndChild <> 0) and (Emulation_State = esRunning) then
//          begin
//            StopEmulation;
//            Message.Result := 1;
//            Exit;
//          end;
//        end;
//      else
//        if m_hwndChild <> 0 then
//        begin
//          SendMessage(m_hwndChild, Message.Msg, Message.WParam, Message.LParam);
//          Message.Result := 1;
//          Exit;
//        end;
//      end;
  end;

  inherited WndProc(Message);
end;

function Tfrm_Main.FindByFileName(const aFileName: string): Integer;
begin
  for Result := 0 to MyXBEList.Count - 1 do
    if SameText(TXBEInfo(MyXBEList.Objects[Result]).FileName, aFileName) then
      Exit;

  Result := -1;
end;

function Tfrm_Main.FindByName(const aName: string): Integer;
begin
  for Result := 0 to MyXBEList.Count - 1 do
    if MyXBEList.Strings[Result] = aName then
      Exit;

  Result := -1;
end;

procedure Tfrm_Main.FormCreate(Sender: TObject);
var
  XBEFilePath: string;
  DummyStr: string;
//  i: Integer;
begin
  Application.OnMessage := AppMessage;
  ApplicationDir := ExtractFilePath(Application.ExeName);

  dgXbeInfos.ColCount := 5;


  // GUI stuff...



  BackgroundImage := TBitmap.Create;

  UpdateBackground;
  UpdateLaunchButton;

  GUISelBg := TJPEGImage.Create;
  GUISelBgLeft := TJPEGImage.Create;
  GUISelBgRight := TJPEGImage.Create;
  GUIHeadBgImg := TJPEGImage.Create;
  GUIHeadSepImg := TJPEGImage.Create;

  GUISelBg.LoadFromResourceName(MainInstance, 'GUIGridSelBg');
  GUISelBgLeft.LoadFromResourceName(MainInstance, 'GUIGridSelBgLeft');
  GUISelBgRight.LoadFromResourceName(MainInstance, 'GUIGridSelBgRight');
  GUIHeadBgImg.LoadFromResourceName(MainInstance, 'GUIHeadBgImg');
  GUIHeadSepImg.LoadFromResourceName(MainInstance, 'GUIHeadSepImg');

  FApplicationDir := ExtractFilePath(Application.ExeName);

  MyXBEList := TStringList.Create;
  MyXBEList.CaseSensitive := False;
  if LoadXBEListByFile(FApplicationDir + cXDK_TRACKER_DATA_FILE) > 0 then
    UpdateFilter;

  PEmuShared(nil).Init;

(*
  // Calculated the exact X resolution :
  i := GetSystemMetrics(SM_CXFIXEDFRAME);
  i := i + 800 + i;
  Constraints.MaxWidth := i; Constraints.MinWidth := i;
  ClientWidth := i;

  // Calculated the exact Y resolution :
  i := GetSystemMetrics(SM_CYFIXEDFRAME);
  i := i + GetSystemMetrics(SM_CYMENU) + 600 + StatusBar.Height + i;
  Constraints.MaxHeight := i; Constraints.MinHeight := i;
  ClientHeight := i;

(*
  // This proves the emulated screen is resized :
  dgXbeInfos.Width := 320;
  dgXbeInfos.Height := 200;
*)
  Constraints.MinHeight := 480;

  // Dxbx Note : This prevents close-exceptions (we have with a "message WM_DROPFILES" procedure) :
  OldLBWindowProc := WindowProc; // store defualt WindowProc
  WindowProc := LBWindowProc; // replace default WindowProc
  DragAcceptFiles(Handle, True); // now ListBox1 accept dropped files

  UpdateTitleInformation;

  ReadSettingsIni;

  CreateLogs(DebugMode, DebugFileName);
  //CreateLogs(KernelDebugMode, KernelDebugFileName);

  XBEFilePath := ParamStr(1);

  if  (XBEFilePath <> '')
  and Drives.D.OpenImage(XBEFilePath, {out}DummyStr) then
  begin
    if OpenXbe(XBEFilePath, {var}m_Xbe) then
      LaunchXBE;
    // TODO : Error logging should go here
  end;
end; // FormCreate

procedure Tfrm_Main.FormDestroy(Sender: TObject);
begin
  WriteSettingsIni();
  SaveXBEListByFile(ApplicationDir + cXDK_TRACKER_DATA_FILE, {aPublishedBy=}'');
  CloseXbe();
  CloseLogs();
end;

destructor Tfrm_Main.Destroy;
begin
  WindowProc := OldLBWindowProc;

  PEmuShared(nil).Cleanup;

  FreeAndNil(BackgroundImage);

  inherited Destroy;
end;

procedure Tfrm_Main.WMDROPFILES(var Msg: TMessage);
var
  pcFileName: PChar;
  i, iSize, iFileCount: Integer;
  XbeXml: string;
begin
  pcFileName := ''; // to avoid compiler warning message
  iFileCount := DragQueryFile(Msg.wParam, $FFFFFFFF, pcFileName, 255);
  for i := 0 to iFileCount - 1 do
  begin
    iSize := DragQueryFile(Msg.wParam, i, nil, 0) + 1;
    pcFileName := StrAlloc(iSize);
    try
      DragQueryFile(Msg.wParam, i, pcFileName, iSize);
      if LoadXbe(pcFileName) then
      begin
        DxbxXml.CreateXmlXbeDumpAsText(XbeXml, m_Xbe, pcFileName);
        LoadXBEListByXml(XbeXml);
        Application.ProcessMessages; // Just to stay responsive with large drops
      end;
    finally
      StrDispose(pcFileName);
    end;
  end;

  UpdateFilter;

  DragFinish(Msg.wParam);
end; // WMDROPFILES

procedure Tfrm_Main.LBWindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_DROPFILES then
    WMDROPFILES(Message); // handle WM_DROPFILES message

  OldLBWindowProc(Message);
  // call default ListBox1 WindowProc method to handle all other messages
end;

procedure Tfrm_Main.FormPaint(Sender: TObject);
begin
  if Assigned(BackgroundImage) then
    Canvas.Draw(0, 0, BackgroundImage);
end;

procedure Tfrm_Main.FormResize(Sender: TObject);
begin
  UpdateBackground;
  Invalidate;
end;

type
  TProtectedJPEGImage = class(TJPEGImage); // Cast to this type to access protected members of TJPEGImage

procedure Tfrm_Main.UpdateBackground;
var
  MinWidth: Integer;
  JPEGImage: TJPEGImage;
begin
  if not Assigned(BackgroundImage) then
    Exit;

  // Make sure the background image covers the entire form :
  BackgroundImage.SetSize(Width, Height);
  BackgroundImage.Canvas.Brush.Color := Color;
  BackgroundImage.Canvas.FillRect(BackgroundImage.Canvas.ClipRect);

  // Draw the logo background
  JPEGImage := GetJPEGResource('GUIHeaderBg');
  BackgroundImage.Canvas.Brush.Bitmap := TProtectedJPEGImage(JPEGImage).Bitmap;
  BackgroundImage.Canvas.FillRect(Types.Rect(0, 0, Width, JPEGImage.Height));

  // Draw the footer bg
  // JPEGImage := GetJPEGResource('GUIFooterBg');
  // BackgroundImage.Canvas.DrawStretch?


  // Draw the logo in the center :
  JPEGImage := GetJPEGResource('GUIHeader');
  BackgroundImage.Canvas.Draw((Width-JPEGImage.Width) div 2, 0, JPEGImage);
  MinWidth := JPEGImage.Width;

  Constraints.MinWidth := MinWidth;

  // Draw a nice little gradient just above the grid :
  BackgroundImage.Canvas.Pen.Width := 1;
  GradientHorizontalLineCanvas(BackgroundImage.Canvas, [clBlack, clXboxGreen, clXboxGreen, clXboxGreen, clBlack], 0, dgXbeInfos.Top - 2, Width);

  // Draw a (tiled!) grating as background for the info-pane :
  JPEGImage := GetJPEGResource('GUIBackgroundGrating');
  BackgroundImage.Canvas.Brush.Bitmap := nil;
  BackgroundImage.Canvas.Brush.Bitmap := TProtectedJPEGImage(JPEGImage).Bitmap;
  //BackgroundImage.Canvas.FillRect(Types.Rect(dgXbeInfos.Width, dgXbeInfos.Top, Width, dgXbeInfos.Top + dgXbeInfos.Height));

  // Draw the info-pane:
  JPEGImage := GetJPEGResource('GUIBackgroundInfoPane');

  BackgroundImage.Canvas.Draw(dgXbeInfos.Width+3, dgXbeInfos.Top + 170, JPEGImage);

  // Draw nice stuff
  JPEGImage := GetJPEGResource('GUIGameIconFrame');
  BackgroundImage.Canvas.Draw(dgXbeInfos.Width+3, dgXbeInfos.Top, JPEGImage);

end; // UpdateBackground

function Tfrm_Main.GetCellText(aCol, aRow: Integer): string;
begin
  Result := '';
  if aRow = 0 then
  begin
    case aCol of
      0: Result := 'Title';
      1: Result := 'Region';
      2: Result := 'Version';
      3: Result := 'DumpInfo';
      4: Result := 'Path';
    end;
    Exit;
  end;

  Dec(aRow);
  if aRow < Length(EnabledItems) then
    case aCol of
      0: Result := EnabledItems[aRow].Title;
      1: Result := GameRegionToString(EnabledItems[aRow].GameRegion); // TODO : Show this as images later on
      2: Result := EnabledItems[aRow].LibVersions.Values['D3D8'];
      3: Result := EnabledItems[aRow].DumpInfo;
      4: Result := EnabledItems[aRow].FileName;
    end;
end; // GetCellText

procedure Tfrm_Main.dgXbeInfosClick(Sender: TObject);
var
  Row: Integer;
begin
  Row := dgXbeInfos.Row - 1;
  if (Row >= 0) and (Row < Length(EnabledItems)) then
    ShowXbeInfo(EnabledItems[Row]);
end;

procedure Tfrm_Main.ShowXbeInfo(const aXbeInfo: TXbeInfo);
begin
  // TODO : Change the GUI to work with XbeInfo's most of the time,
  // even when an Xbe was opened - we should just create an XbeInfo
  // for this (maybe even create a dump-file along the way).
  //
  // The GUI shouldn't access the Xbe all that much anyway.
  // (We've got XbeExplorer for that!)
  //
  // In order to show a title-logo, the Xbe dumps should be expanded
  // with a hexadecimal dump of the title-logo data (meaning we must
  // re-generate all of our Xbe dumps - as far as that's possible).
  //
  // Also, the XbeInfo is currently read from the old XdkTracker gamebase
  // XML file - perhaps it's better to read everything from the dumps
  // folder, as I don't see the value of an intermediate format.
  //
  // Another aspect is categorization - some titles are games, others
  // are applications, yet others are demos. The dump doesn't tell
  // us much in that regard, but it could become desirable to build a
  // database of details per title; Apart from a category, we could
  // later add specific emulation configuration, like patches to place
  // (or skip), regions to 'nop-out', and other emulation settings.
  // Also, compatibility-information could be added, so we could
  // filter on titles playable in Cxbx, showing intro's in Dxbx, etc.
  //
  // All this will render XdkTracker obsolete, and might one day
  // even function as a complete database of everything related to
  // Xbox1 emulation!
  if LoadXbe(aXbeInfo.FileName) then
    //
  else
  begin
    m_Xbe := nil;
    UpdateTitleInformation;
  end;
end;

procedure Tfrm_Main.dgXbeInfosDblClick(Sender: TObject);
begin
  actStartEmulationExecute(Sender);
end;

procedure Tfrm_Main.dgXbeInfosDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  Where: TRect;
  Format: UINT;
  cPos, cPosf: Integer;
  lf: TLogFont;
begin

  cPos := Rect.Left;
  dgXbeInfos.Canvas.Brush.Style := bsClear;
  dgXbeInfos.Canvas.Font.Size := 10;
  dgXbeInfos.Canvas.Font.Color := clWhite;
  GetObject(dgXbeInfos.Canvas.Font.Handle, sizeof(TLogFont), @lf);
  lf.lfQuality := ANTIALIASED_QUALITY or PROOF_QUALITY;
  lf.lfFaceName := 'Verdana';
  dgXbeInfos.Canvas.Font.Handle := CreateFontIndirect(lf);
  if ARow = 0 then
  begin

    while cPos < Rect.Right do
    begin
      dgXbeInfos.Canvas.Draw(cPos,0,GUIHeadBgImg);
      Inc(cPos);
    end;

    dgXbeInfos.Canvas.Draw(cPos-2,1,GUIHeadSepImg);

    // Should only be done once?


    //drawgrid1.Canvas.TextFlags := DT_RIGHT;
    //dgXbeInfos.Canvas.TextRect(Rect, Rect.Left+5,Rect.Top+8, 'Title');
    //drawgrid1.Canvas.TextOut(Rect.Left+2, Rect.Top+4, 'A');

  end
  else
  begin
    if (dgXbeInfos.Selection.Top = ARow) then
    begin
    cPos := Rect.Left;
    cPosf := 0;
      if ACol = 0 then
      begin
        dgXbeInfos.Canvas.Draw(Rect.Left, Rect.Top, GUISelBgLeft);
        cPos := cPos + GUISelBgLeft.Width;
      end;

      if ACol = dgXbeInfos.ColCount-1 then
      begin
        cPosf := GUISelBgRight.Width;
        dgXbeInfos.Canvas.Draw(Rect.Right-cPosf, Rect.Top, GUISelBgRight);
      end;

      cPosf := Rect.Right - cPosf;

      while cPos < cPosf do
      begin
        dgXbeInfos.Canvas.Draw(cPos,Rect.Top,GUISelBg);
        Inc(cPos);
      end;

    end
    else
    begin
      //dgXbeInfos.Canvas.Brush.Color := clBlack;
      dgXbeInfos.Canvas.Pen.Color := clBlack;
      dgXbeInfos.Canvas.Rectangle(0, Rect.Top, dgXbeInfos.Width, Rect.Bottom);
    end;
  end;


  /// ----
   {
  if gdFixed in State then
  begin
    // Put gradient lines next to the header columns :
    Where := Rect;
    TDrawGrid(Sender).Canvas.Pen.Width := 2;
    GradientVerticalLineCanvas(TDrawGrid(Sender).Canvas, [clBlack, clXboxGreen, clGray], Where.Right-2, Where.Top, Where.Bottom - Where.Top);
    // Render titles in bold :
    //TDrawGrid(Sender).Canvas.Font.Style := [fsBold];
  end;
  }
  // Draw cell's text :
  Where := Rect;
  InflateRect(Where, -4, -4);
  //Inc(Where.Top, 3);
  if aCol = 2 then // Align 'Version' column right :
    Format := DT_RIGHT
  else
    Format := DT_LEFT;
  DrawText(TDrawGrid(Sender).Canvas.Handle, PChar(GetCellText(aCol, aRow)), -1, Where, Format or DT_RTLREADING);
end; // dgXbeInfosDrawCell

procedure Tfrm_Main.dgXbeInfosKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;

  procedure RemoveGameFromList(aIndex: Integer);
  begin
    if MessageDlg('Remove title from list?', mtConfirmation, [mbYes, mbNo], -1) = mrYes then
    begin
      MyXBEList.Objects[i].Free;
      MyXBEList.Delete(i);
      UpdateFilter;
    end;
  end;

begin
  case Key of
    VK_RETURN: // Enter means Launch:
    begin
      actStartEmulationExecute(Sender);
      Key := 0;
    end;
    VK_DELETE: // Delete key means : ask to remove active xbe from list
    begin
      Key := 0;
      if Assigned(m_Xbe) and m_Xbe.isValid then
      begin
        i := FindByFileName(m_Xbe.XbePath);
        if i >= 0 then
          RemoveGameFromList(i);
      end
      else
      begin
        i := FindByName(EnabledItems[dgXbeInfos.Row - 1].Title);
        if i >= 0 then
          RemoveGameFromList(i);
      end;
    end;
  end;
end;

procedure Tfrm_Main.cbFreeTextFilterKeyPress(Sender: TObject; var Key: Char);
var
  i: Integer;
  Str: string;
begin
  if Key = #13 then
  begin
    // Make the current text the first in the dropdown list :
    Str := cbFreeTextFilter.Text;
    i := cbFreeTextFilter.Items.IndexOf(Str);
    if i >= 0 then
      cbFreeTextFilter.Items.Move(i, 0)
    else
    begin
      cbFreeTextFilter.Items.Add(Str);
      if cbFreeTextFilter.Items.Count > 10 then
        cbFreeTextFilter.Items.Delete(cbFreeTextFilter.Items.Count - 1);
    end;

    // Apply the chosen filter :
    UpdateFilter;
  end;
end; // cbFreeTextFilterKeyPress

procedure Tfrm_Main.cbFreeTextFilterSelect(Sender: TObject);
begin
  UpdateFilter;
end;

procedure Tfrm_Main.UpdateFilter;
var
  i, CurrentRow: Integer;
  FilterStr: string;

  function _MaySee: Boolean;
  var
    i: Integer;
  begin
    Result := (FilterStr = '');
    if not Result then
      for i := 0 to dgXbeInfos.ColCount - 1 do
      begin
        Result := AnsiContainsText(GetCellText(i, CurrentRow+1), FilterStr);
        if Result then
          Exit;
      end;
  end;

begin
  // Build a list of Xbe's that pass the filter :
  SetLength(EnabledItems, MyXBEList.Count);
  CurrentRow := 0;
  FilterStr := cbFreeTextFilter.Text;
  for i := 0 to MyXBEList.Count - 1 do
  begin
    EnabledItems[CurrentRow] := TXbeInfo(MyXBEList.Objects[i]);
    if _MaySee then
      Inc(CurrentRow);
  end;

  SetLength(EnabledItems, CurrentRow);
  // Make the grid show the filtered list :
  dgXbeInfos.RowCount := 1 + CurrentRow;
  dgXbeInfos.Repaint;
end; // UpdateFilter

procedure Tfrm_Main.UpdateLaunchButton;
var
  LaunchButtonResName: string;
begin
  case imgLaunchButton.Tag of
    2: LaunchButtonResName := 'ButtonLaunchPressed';
    1: LaunchButtonResName := 'ButtonLaunchHoover';
  else LaunchButtonResName := 'ButtonLaunchNormal';
  end;

  imgLaunchButton.Canvas.Draw(0, 0, GetJPEGResource(LaunchButtonResName));
end;

procedure Tfrm_Main.imgLaunchButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  imgLaunchButton.Tag := 2;
  UpdateLaunchButton;
end;

procedure Tfrm_Main.imgLaunchButtonMouseEnter(Sender: TObject);
begin
  imgLaunchButton.Tag := 1;
  UpdateLaunchButton;
end;

procedure Tfrm_Main.imgLaunchButtonMouseLeave(Sender: TObject);
begin
  imgLaunchButton.Tag := 0;
  UpdateLaunchButton;
end;

procedure Tfrm_Main.imgLaunchButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  imgLaunchButton.Tag := 1;
  UpdateLaunchButton;
end;

procedure Tfrm_Main.imgLaunchButtonClick(Sender: TObject);
begin
  actStartEmulationExecute(Sender);
end;

procedure Tfrm_Main.actConfigurationExecute(Sender: TObject);
begin
  fmConfiguration := TfmConfiguration.Create(nil);

  if fmConfiguration.ShowModal = mrOk then
  begin
  end;

  FreeAndNil({var}fmConfiguration);
end;

function CanRunXbe(const aXbe: TXbe; var NoRunReason: string): Boolean;
var
  i: Integer;
begin
  NoRunReason := '';
  if Length(aXbe.m_LibraryVersion) = 0 then
    NoRunReason := 'No linked libraries found!'
  else
    for i := 0 to Length(aXbe.m_LibraryVersion) - 1 do
    begin
      if Pos({D3D}'8LTCG', UpperCase(string(AnsiString(aXbe.m_LibraryVersion[i].szName)))) > 0 then
      begin
        NoRunReason := 'Cannot patch link-time optimized libraries!';
        Break;
      end;

      if Pos({D3D}'8D', UpperCase(string(AnsiString(aXbe.m_LibraryVersion[i].szName)))) > 0 then
      begin
        NoRunReason := 'Cannot patch debug libraries!';
        Break;
      end;
    end;

  Result := (NoRunReason = '');
end;

procedure Tfrm_Main.actStartEmulationExecute(Sender: TObject);
var
  NoRunReason: string;
begin
  if not Assigned(m_Xbe) then
  begin
    MessageDlg('No xbe file loaded', mtInformation, [mbOk], 0);
    Exit;
  end;

  if CanRunXbe(m_Xbe, {var}NoRunReason) then
    LaunchXBE
  else
    MessageDlg('Cannot launch xbe!'#13#10 + NoRunReason, mtError, [mbOk], 0);
end;

procedure Tfrm_Main.actStopEmulationExecute(Sender: TObject);
begin
  StopEmulation;
end;

procedure Tfrm_Main.actConsoleXbeInfoExecute(Sender: TObject);
begin
  // dump xbe information to debug console
  if m_Xbe.DumpInformation then
    WriteLog(m_szAsciiTitle + '`s .xbe info was successfully dumped.')
  else
    WriteLog(m_szAsciiTitle + '`s .xbe info was successfully dumped.'); // NOT!
end;

procedure Tfrm_Main.actDebugGuiNoneExecute(Sender: TObject);
begin
  CloseLogs;
  DebugMode := dmNone;
  AdjustMenu;
end;

procedure Tfrm_Main.actDebugKernelNoneExecute(Sender: TObject);
begin
  KernelDebugMode := dmNone;
  AdjustMenu;
end;

procedure Tfrm_Main.SaveXBEList(const aFilePath, aPublishedBy: string);
var
  XMLRootNode: IXMLNode;
  PublishedNode: IXMLNode;
  GameListNode: IXMLNode;
  XBEInfoNode: IXMLNode;
  XDKnode: IXMLNode;
  i, j: Integer;
  XBEInfo: TXBEInfo;
begin
  if not XMLDocument.Active then
    XMLDocument.Active := True;

  XMLDocument.ChildNodes.Clear;
  XMLRootNode := XMLDocument.AddChild('XBEInfo');
  XMLRootNode.SetAttribute('Version', cXDk_TRACKER_XML_VERSION);

  PublishedNode := XMLRootNode.AddChild('PublishedInfo');
  XML_WriteString(PublishedNode, 'PublishedBy', aPublishedBy);

  GameListNode := XMLRootNode.AddChild('GameList');

  for i := 0 to MyXBEList.Count - 1 do
  begin
    XBEInfo := TXBEInfo(MyXBEList.Objects[i]);
    XBEInfoNode := GameListNode.AddChild('Game');

    XML_WriteString(XBEInfoNode, 'FileName', XBEInfo.FileName);
    XML_WriteString(XBEInfoNode, 'Title', XBEInfo.Title);
    XML_WriteString(XBEInfoNode, 'GameRegion', IntToStr(XBEInfo.GameRegion));
    XML_WriteString(XBEInfoNode, 'DumpInfo', XBEInfo.DumpInfo);
    XDKnode := XBEInfoNode.AddChild('XDKVersions');

    for j := 0 to XBEInfo.LibVersions.Count - 1 do
      XML_WriteString(XDKnode, XBEInfo.LibVersions.Names[j], XBEInfo.LibVersions.ValueFromIndex[j]);
  end;

  XMLDocument.SaveToFile(aFilePath);
end;

procedure Tfrm_Main.SaveXBEListByFile(const aFilePath, aPublishedBy: string);
var
  XMLRootNode: IXMLNode;
  PublishedNode: IXMLNode;
  GameListNode: IXMLNode;
  XBEInfoNode: IXMLNode;
  XDKnode: IXMLNode;
  i, j: Integer;
  XBEInfo: TXBEInfo;
begin
  if not XMLDocument.Active then
    XMLDocument.Active := True;

  XMLDocument.ChildNodes.Clear;
  XMLRootNode := XMLDocument.AddChild('XBEInfo');
  XMLRootNode.SetAttribute('Version', cXDk_TRACKER_XML_VERSION);

  PublishedNode := XMLRootNode.AddChild('PublishedInfo');
  XML_WriteString(PublishedNode, 'PublishedBy', aPublishedBy);

  GameListNode := XMLRootNode.AddChild('GameList');

  for i := 0 to MyXBEList.Count - 1 do
  begin
    XBEInfo := TXBEInfo(MyXBEList.Objects[i]);
    XBEInfoNode := GameListNode.AddChild('Game');

    XML_WriteString(XBEInfoNode, 'FileName', XBEInfo.FileName);
    XML_WriteString(XBEInfoNode, 'Title', XBEInfo.Title);
    XML_WriteString(XBEInfoNode, 'GameRegion', IntToStr(XBEInfo.GameRegion));
    XML_WriteString(XBEInfoNode, 'DumpInfo', XBEInfo.DumpInfo);
    XDKnode := XBEInfoNode.AddChild('XDKVersions');

    for j := 0 to XBEInfo.LibVersions.Count - 1 do
      XML_WriteString(XDKnode, XBEInfo.LibVersions.Names[j], XBEInfo.LibVersions.ValueFromIndex[j]);
  end;

  XMLDocument.SaveToFile(aFilePath);
end;

// TfrmMain.SaveXBEList

procedure Tfrm_Main.actExportGameListExecute(Sender: TObject);
begin
  if ExportDialog.Execute then
    SaveXBEList(ExportDialog.FileName, '');
end;

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
end;

function Tfrm_Main._ReadXBEInfoFromNode(const XBEInfoNode: IXMLNode): TXBEInfo;
var
  XDKNode, LibNode: IXMLNode;
  GameRegion: String;
begin
  Result := TXBEInfo.Create;
  Result.FileName := XML_ReadString(XBEInfoNode, 'FileName');

  Result.Title := XML_ReadString(XBEInfoNode, 'Title');
  GameRegion := XML_ReadString(XBEInfoNode, 'GameRegion');
  if (GameRegion = '') or (GameRegion[1] = '-') then
    Result.GameRegion := 0
  else
    Result.GameRegion := StrToIntDef(GameRegion, 0);

  Result.DumpInfo := XML_ReadString(XBEInfoNode, 'DumpInfo');

  XDKNode := XBEInfoNode.ChildNodes.FindNode('XDKVersions');
  if Assigned(XDKNode) then
  begin
    LibNode := XDKNode.ChildNodes.First;
    while Assigned(LibNode) do
    begin
      Result.LibVersions.Values[LibNode.LocalName] := LibNode.Text;
      LibNode := LibNode.NextSibling;
    end;

    Result.LibVersions.Sort;
  end;
end;

function Tfrm_Main.FindDuplicate(const aXBEInfo: TXBEInfo): Integer;
begin
  // Initially we don't know if this XBE is already present :
  Result := -1;

  // Try searching by title :
  if (Result < 0) and (aXBEInfo.Title <> '') then
    Result := MyXBEList.IndexOf(aXBEInfo.Title);

  // Try searching by title, but use FileName for backwards compatibility :
  if (Result < 0) and (aXBEInfo.FileName <> '') then
    Result := MyXBEList.IndexOf(aXBEInfo.FileName);

  // Try searching by filename :
  if (Result < 0) and (aXBEInfo.FileName <> '') then
    Result := FindByFileName(aXBEInfo.FileName);

  // Try searching by filename, but use Title as last resort :
  if (Result < 0) and (aXBEInfo.Title <> '') then
    Result := FindByFileName(aXBEInfo.Title);

  // TODO : Add other search-methods here

  // Not found at all, no other searches possible :
  if Result < 0 then
    Exit;

  // Mark it a no-match when the region mis-matches :
  if (TXBEInfo(MyXBEList.Objects[Result]).GameRegion <> aXBEInfo.GameRegion) then
  begin
    Result := -1;
    Exit;
  end;

  // Mark it a no-match when the library versions mis-match :
  if (TXBEInfo(MyXBEList.Objects[Result]).LibVersions.Text <> aXBEInfo.LibVersions.Text) then
  begin
    Result := -1;
    Exit;
  end;

  // TODO : Add other non-duplicate tests here
end;

procedure Tfrm_Main.FindFiles(FilesList: TStringList; StartDir,
  FileMask: string);
var
  SR: TSearchRec;
  DirList: TStringList;
  IsFound: Boolean;
  i: integer;
begin
  if StartDir[length(StartDir)] <> '\' then
    StartDir := StartDir + '\';

  { Build a list of the files in directory StartDir
     (not the directories!)                         }

  IsFound :=
    FindFirst(StartDir+FileMask, faAnyFile-faDirectory, SR) = 0;
  while IsFound do begin
    FilesList.Add(StartDir + SR.Name);
    IsFound := FindNext(SR) = 0;
    Application.ProcessMessages;
  end;
  FindClose(SR);

  // Build a list of subdirectories
  DirList := TStringList.Create;
  IsFound := FindFirst(StartDir+'*.*', faAnyFile, SR) = 0;
  while IsFound do begin
    if ((SR.Attr and faDirectory) <> 0) and
         (SR.Name[1] <> '.') then
      DirList.Add(StartDir + SR.Name);
    IsFound := FindNext(SR) = 0;
    Application.ProcessMessages;
  end;
  FindClose(SR);

  // Scan the list of subdirectories
  for i := 0 to DirList.Count - 1 do
    FindFiles(FilesList, DirList[i], FileMask);

  DirList.Free;
end;

(*function Tfrm_Main.ShowImportList(const XBEImportList: TStringList; Publisher: string): Integer;
var
  i: Integer;
  XBEInfo: TXBEInfo;
begin
  Result := 0;

  frm_XBEList := Tfrm_XBEList.Create(Self);
  try
    // Precalculate the IsDuplicate members (this will be used to feed the checkboxes) :
    for i := 0 to XBEImportList.Count - 1 do
    begin
      XBEInfo := TXBEInfo(XBEImportList.Objects[i]);
      XBEInfo.IsDuplicate := FindDuplicate(XBEInfo) >= 0;
    end;

    // Put this list into the view :
    frm_XBEList.FillXBEList(XBEImportList, {ShowAsImport=}True);

    if frm_XBEList.ShowModal = mrOk then
    begin
      for i := 0 to frm_XBEList.lst_XBEs.Items.Count - 1 do
        if  frm_XBEList.lst_XBEs.Items[i].Checked
        and InsertXBEInfo(TXBEInfo(frm_XBEList.lst_XBEs.Items[i].Data)) then
          Inc(Result);

      MyXBEList.Sort;
    end;
  finally
    frm_XBEList.Release;
    frm_XBEList := nil;
  end;
end; // ShowImportList *)

function Tfrm_Main.ImportXBEGameList(aImportFilePath: string = '';
  aUseImportDialog: Boolean = False): Integer;
var
  XMLRootNode: IXMLNode;
  XMLNode: IXMLNode;
  XBEInfoNode: IXMLNode;
  Publisher: string;
  FileName: string;
  XBEImportList: TStringList;
  i: Integer;
begin
  Result := 0;
  if aImportFilePath = '' then
    aImportFilePath := ApplicationDir + 'Dump.dat';

  if not FileExists(aImportFilePath) then
    Exit;

  XmlDocument.Active := False;
  XmlDocument.FileName := aImportFilePath;
  try
    XmlDocument.Active := True;
  except
    on E: EDOMParseError do
    begin
      MessageDlg('Error parsing the file!', mtError, [mbOk], -1);
      XmlDocument.Active := False;
    end
    else
      XmlDocument.Active := False;
  end;

  if not XmlDocument.Active then
    Exit;

  XBEImportList := TStringList.Create;
  try
    XMLRootNode := XMLDocument.DocumentElement;

    XMLNode := XMLRootNode.ChildNodes.FindNode('PublishedInfo');
    if Assigned(XMLNode) then
      Publisher := XML_ReadString(XMLNode, 'PublishedBy');

    XMLNode := XMLRootNode.ChildNodes.FindNode('GameList');
    if Assigned(XMLNode) then
      XBEInfoNode := XMLNode.ChildNodes.First
    else
      XBEInfoNode := XMLRootNode;

    while Assigned(XBEInfoNode) do
    begin
      FileName := XML_ReadString(XBEInfoNode, 'FileName');
      if FileName = '' then
        // Old-style 'Name' values are read here, interpreted as FileName :
        FileName := XML_ReadString(XBEInfoNode, 'Name');

      // For now, only add to list when the user can intervene,
      // or when not yet present :
      if aUseImportDialog
      or (FindByFileName(FileName) < 0) then
        XBEImportList.AddObject('', _ReadXBEInfoFromNode(XBEInfoNode));

      XBEInfoNode := XBEInfoNode.NextSibling;
    end;

 //   if aUseImportDialog then
 //     Result := ShowImportList(XBEImportList, Publisher)
//    else
      for i := 0 to XBEImportList.Count - 1 do
        if InsertXBEInfo(TXBEInfo(XBEImportList.Objects[i])) then
          Inc(Result);

    MyXBEList.Sort;

  finally
    FreeAndNil({var}XBEImportList);
  end;
end;

procedure Tfrm_Main.actImportGameListExecute(Sender: TObject);
begin
  // Import another gamedata (next to the already loaded version)
  if ImportDialog.Execute then
  begin
    ImportXBEGameList(ImportDialog.FileName, {aUseImportDialog=}True);
    UpdateFilter;
    SaveXBEList(ApplicationDir + cXDK_TRACKER_DATA_FILE, {aPublishedBy=}'');
  end;
end;

procedure Tfrm_Main.actImportXbesExecute(Sender: TObject);
var
  sFolder : string;
  XbeFiles: TStringList;
  i: integer;
begin
  sFolder := BrowseDialog('Select a folder', BIF_RETURNONLYFSDIRS);

  if sFolder <> '' then
  begin
    XbeFiles := TStringList.Create;
    try
      FindFiles(XbeFiles, sFolder, '*.xbe');

      for i := 0 to XbeFiles.Count -1 do
      begin
        OpenXbeFile(XbeFiles[i]);
        Application.ProcessMessages;
      end;

    finally
      FreeAndNil(XbeFiles);
    end;
  end;
end;

// actFileXbeInfoExecute

procedure Tfrm_Main.actConsoleDebugGuiExecute(Sender: TObject);
begin
  CloseLogs;
  DebugMode := dmConsole;
  CreateLogs(DebugMode);
  AdjustMenu;
end; // actConsoleDebugGuiExecute

procedure Tfrm_Main.actFileDebugGuiExecute(Sender: TObject);
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
    AdjustMenu;
  end;
end; // actFileDebugGuiExecute

procedure Tfrm_Main.actConsoleDebugKernelExecute(Sender: TObject);
begin
  //CloseLogs;
  KernelDebugMode := dmConsole;
  //CreateLogs(KernelDebugMode);
  AdjustMenu;
end; // actConsoleDebugKernelExecute

function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam: LPARAM; lpData: LPARAM): Integer; stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) then
    SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
  BrowseCallbackProc := 0;
end;

function GetFolderDialog(Handle: Integer; Caption: string; var strFolder: string): Boolean;
const
  BIF_STATUSTEXT           = $0004;
  BIF_NEWDIALOGSTYLE       = $0040;
  BIF_RETURNONLYFSDIRS     = $0080;
  BIF_SHAREABLE            = $0100;
  BIF_USENEWUI             = BIF_EDITBOX or BIF_NEWDIALOGSTYLE;

var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  JtemIDList: PItemIDList;
  Path: PChar;
begin
  Result := False;
  Path := StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(Handle, CSIDL_DESKTOP, JtemIDList);
  with BrowseInfo do
  begin
    hwndOwner := GetActiveWindow;
    pidlRoot := JtemIDList;
    SHGetSpecialFolderLocation(hwndOwner, CSIDL_DESKTOP, JtemIDList);

    { return display name of item selected }
    pszDisplayName := StrAlloc(MAX_PATH);

    { set the title of dialog }
    lpszTitle := PChar(Caption);//'Select the folder';
    { flags that control the return stuff }
    lpfn := @BrowseCallbackProc;
    { extra info that's passed back in callbacks }
    lParam := LongInt(PChar(strFolder));
  end;

  ItemIDList := SHBrowseForFolder(BrowseInfo);

  if (ItemIDList <> nil) then
    if SHGetPathFromIDList(ItemIDList, Path) then
    begin
      strFolder := Path;
      Result := True
    end;
end;

procedure Tfrm_Main.actFileDebugKernelExecute(Sender: TObject);
begin
  GetFolderDialog(Application.Handle, 'Select a folder', KernelDebugFilePath);
  if KernelDebugFilePath <> '' then
  begin
    KernelDebugMode := dmFile;
    AdjustMenu;
  end;
end; // actFileDebugKernelExecute

procedure Tfrm_Main.actAboutExecute(Sender: TObject);
begin
  frm_About := Tfrm_About.Create(Self);

  if frm_About.ShowModal = mrOk then
  begin
  end;

  FreeAndNil({var}frm_About);
end;

procedure Tfrm_Main.actCleanSymbolCacheExecute(Sender: TObject);
begin
  SHDeleteFiles(Handle, SymbolCacheFolder + '\*' + SymbolCacheFileExt, [doAllowUndo, doFilesOnly]);
end;

procedure Tfrm_Main.actClearGameListExecute(Sender: TObject);
var
  lIndex: Integer;
begin
  // Import another gamedata (next to the already loaded version)
  for lIndex :=  MyXBEList.Count -1 downto 0 do
  begin
    MyXBEList.Delete(lIndex);
  end;
  UpdateFilter;
  SaveXBEList(ApplicationDir + cXDK_TRACKER_DATA_FILE, {aPublishedBy=}'');
end;

procedure Tfrm_Main.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure Tfrm_Main.actOpenXbeExecute(Sender: TObject);
begin
  if XbeOpenDialog.Execute then
    OpenXbeFile(XbeOpenDialog.FileName);
end;

procedure Tfrm_Main.actCloseXbeExecute(Sender: TObject);
begin
  CloseXbe();
end;

function Tfrm_Main.StartTool(aToolName: string; const aParameters: string = ''): Boolean;
begin
  aToolName := FApplicationDir + aToolName;
  Result := FileExists(aToolName);
  if Result then
    {Result := }ShellExecute(0, 'open', PChar(aToolName), PChar(aParameters), nil, SW_SHOWNORMAL);
end;

procedure Tfrm_Main.actXbeExplorerExecute(Sender: TObject);
var
  Parameters: string;
begin
  if Assigned(m_Xbe) and m_Xbe.isValid then
    Parameters := AnsiQuotedStr(m_Xbe.XbePath, '"')
  else
    Parameters := '';

  StartTool(cXbeExplorerPath, Parameters);
end;

procedure Tfrm_Main.actXIsoExecute(Sender: TObject);
begin
  StartTool(cXIsoPath);
end;

procedure Tfrm_Main.actXdkTrackerXbeInfoExecute(Sender: TObject);
var
  DumpFilePath: string;
begin
  if Assigned(m_Xbe) and m_Xbe.isValid then
  begin
    DumpFilePath := FApplicationDir + 'Tools\Dump.dat';
    DxbxXml.CreateXmlXbeDump(DumpFilePath, m_Xbe);

    if not SendCommandToXdkTracker then
      StartTool(cXDKTrackerPath, '/XBEDUMP');
  end;
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
    KernelDebugFilePath := IniFile.ReadString('Settings', 'KrnlDebugFilePath', '');

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
end; // ReadSettingsIni

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
    IniFile.WriteString('Settings', 'KrnlDebugFilePath', KernelDebugFilePath);
  finally
    FreeAndNil(IniFile);
  end;
end; // WriteSettingsIni

procedure Tfrm_Main.AdjustMenu;
begin
  // Update File menu actions :
  actOpenXbe.Enabled := True;
  actCloseXbe.Enabled := Assigned(m_Xbe) and m_Xbe.isValid;

  mnu_RecentXbefiles.Enabled := (mnu_RecentXbefiles.Count > 0);

  actClose.Enabled := True;

  // Update View menu actions :
  actConsoleDebugGui.Checked := (DebugMode = dmConsole);
  actFileDebugGui.Checked := (DebugMode = dmFile);
  actDebugGuiNone.Checked := (DebugMode = dmNone);

  actConsoleDebugKernel.Checked := (KernelDebugMode = dmConsole);
  actFileDebugKernel.Checked := (KernelDebugMode = dmFile);
  actDebugKernelNone.Checked := (KernelDebugMode = dmNone);

  // Update Emulation menu actions :
  actStartEmulation.Enabled := (Emulation_State = esFileOpen);
  actStopEmulation.Enabled := (Emulation_State = esRunning);

  // Update Tools menu actions :
  actXbeExplorer.Enabled := FileExists(FApplicationDir + cXbeExplorerPath);
  actXIso.Enabled := FileExists(FApplicationDir + cXIsoPath);

  // Update Edit menu actions (should be done after actXdkTracker update) :
  mnu_Patch.Enabled := False;
  mnu_DumpxbeinfoTo.Enabled := Assigned(m_Xbe) and m_Xbe.isValid;

  mnu_BypassSymbolCache.Checked := g_EmuShared.m_BypassSymbolCache;
end; // AdjustMenu

function Tfrm_Main.GetEmuWindowHandle(const aEmuDisplayMode: Integer = 1): THandle;
begin
  // Where should the xbox emulation screen be drawn?
  case aEmuDisplayMode of
    2: Result := MaxInt; // draw in a new window (resizes with window)
    1: Result := Self.Handle; // cover the complete mainform (does not resize with form?)
  else
    dgXbeInfos.ScrollBars := ssNone; // first, remove scrollbar (it would show over the emu-window)
    Result := dgXbeInfos.Handle; // cover the XbeInfo list (does not resize with form?)
  end;
end;

procedure Tfrm_Main.LaunchXBE;
var
  Parameters: string;
begin
  Parameters :=
    {LaunchArgument=}'/load ' +
    {XbePath=}AnsiQuotedStr(m_Xbe.XbePath, '"') + ' ' +
    {WindowHandle=}IntToStr(GetEmuWindowHandle) + ' ' +
    {DebugMode=}IntToStr(Ord(KernelDebugMode)) + ' ' +
    {DebugFileName=}AnsiQuotedStr(GetTitleSpecificKernelDebugFilePath, '"');

  // Dxbx uses itself as Xbe Launcher; In this new process, the '/load'-argument
  // transfers control to DxbxMain() in our emulation dll, which start emulation :
  ShellExecute(0, 'open', PChar(ParamStr(0)), PChar(Parameters), nil, SW_SHOWNORMAL);

  Emulation_State := esRunning; // m_bCanStart := false;
  UpdateTitleInformation;
end;

function Tfrm_Main.LoadXbe(const aFileName: string): Boolean;
begin
  // This is to prevent loading files dropped in to GUI from a container :
  Result := FileExists(aFileName);
  if not Result then
    Exit;

  CloseXbe();
  Result := OpenXbe(aFileName, {var}m_Xbe);
  if Result then
  begin
    RecentXbeAdd(XbeOpenDialog.FileName);
    UpdateTitleInformation;
  end
  else
  begin
    MessageDlg('Can not open Xbe file.', mtWarning, [mbOk], 0);
    UpdateTitleInformation;
  end;
end; // LoadXbe

procedure Tfrm_Main.CloseXbe;
begin
  if Assigned(m_Xbe) and m_Xbe.isValid then
  begin
    FreeAndNil(m_Xbe);

    UpdateTitleInformation;

    WriteLog(Format('DXBX: %s Closed...', [m_szAsciiTitle]));
  end;
end;

procedure Tfrm_Main.ReopenXbe(Sender: TObject);
var
  TempItem: TMenuItem;
begin
  TempItem := Sender as TMenuItem;
  if not TXbe.FileExists(TempItem.Hint) then
  begin
    MessageDlg('Could not locate file : ' + TempItem.Hint, mtWarning, [mbOk], 0);
    mnu_RecentXbefiles.Remove(TempItem);
    Exit;
  end;

  CloseXbe();
  if not OpenXbe(TempItem.Hint, {var}m_Xbe) then
  begin
    MessageDlg('Can not open Xbe file.', mtWarning, [mbOk], 0);
    Exit;
  end;
end; // ReopenXbe

procedure Tfrm_Main.RecentXbeAdd(aFileName: string);
var
  TempItem: TMenuItem;
  i: Integer;
begin
  // If file does not exists, for example game has been deleted
  // then do not insert him into the recent xbe list
  if not FileExists(aFileName) then
    Exit;

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
end; // RecentXbeAdd

procedure Tfrm_Main.UpdateTitleInformation;
var
  i: Integer;
  LibName: string;
  Version: string;
begin
  if Assigned(m_XBE) and m_xbe.isValid and m_XBE.ExportLogoBitmap(ImageLogo.Picture.Bitmap) then
    ImageLogo.Show
  else
    ImageLogo.Hide;

  if Assigned(m_XBE) and m_xbe.isValid then
  begin
    if not m_XBE.ExportIconBitmap(ImageIcon.Picture.Bitmap) then
      ImageIcon.Picture.Assign(GetJPEGResource('GUIIconNotAvailable'));

      // Round of the corners a bit :
      ImageIcon.Picture.Bitmap.Transparent := True;
      ImageIcon.Picture.Bitmap.TransparentColor := clRed;

      for i := 0 to 3 do
      begin
         ImageIcon.Picture.Bitmap.Canvas.Pixels[0,I] := clRed;
         ImageIcon.Picture.Bitmap.Canvas.Pixels[0,ImageIcon.Picture.Width-I] := clRed;
         ImageIcon.Picture.Bitmap.Canvas.Pixels[ImageIcon.Picture.Width-1,I] := clRed;
         ImageIcon.Picture.Bitmap.Canvas.Pixels[ImageIcon.Picture.Width-1,ImageIcon.Picture.Height-I] := clRed;
      end;

      ImageIcon.Picture.Bitmap.Canvas.Pixels[1,0] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[1,1] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[3,0] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[2,0] := clRed;

      ImageIcon.Picture.Bitmap.Canvas.Pixels[ImageIcon.Picture.Width-2,0] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[ImageIcon.Picture.Width-2,1] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[ImageIcon.Picture.Width-3,0] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[ImageIcon.Picture.Width-4,0] := clRed;

      ImageIcon.Picture.Bitmap.Canvas.Pixels[1,ImageIcon.Picture.Height-1] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[1,ImageIcon.Picture.Height-2] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[2,ImageIcon.Picture.Height-1] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[3,ImageIcon.Picture.Height-1] := clRed;

      ImageIcon.Picture.Bitmap.Canvas.Pixels[ImageIcon.Picture.Width-2,ImageIcon.Picture.Height-1] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[ImageIcon.Picture.Width-2,ImageIcon.Picture.Height-2] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[ImageIcon.Picture.Width-3,ImageIcon.Picture.Height-1] := clRed;
      ImageIcon.Picture.Bitmap.Canvas.Pixels[ImageIcon.Picture.Width-4,ImageIcon.Picture.Height-1] := clRed;

    ImageIcon.Show;
  end
  else
    ImageIcon.Hide;

  if Assigned(m_XBE) and m_Xbe.isValid then
  begin
    lblXbeInformation.Caption := Format(
      '%s'#13 +
      'ID:%.08x'#13 +
      'Region:%s'#13 +
      #13 +
      'Library versions', [
      PChar(m_szAsciiTitle),
      m_XBE.m_Certificate.dwTitleId,
      GameRegionToString(m_XBE.m_Certificate.dwGameRegion)]);

    // Add library versions (TODO : Sort them alfabetically)
    for i := 0 to Integer(m_XBE.m_Header.dwLibraryVersions) - 1 do
    begin
      LibName := string(Copy(m_XBE.m_LibraryVersion[i].szName, 1, XBE_LIBRARYNAME_MAXLENGTH));
      Version := IntToStr(m_XBE.m_LibraryVersion[i].wMajorVersion) + '.' +
        IntToStr(m_XBE.m_LibraryVersion[i].wMinorVersion) + '.' +
        IntToStr(m_XBE.m_LibraryVersion[i].wBuildVersion);
      lblXbeInformation.Caption := lblXbeInformation.Caption +
        #13 + LibName + StringOfChar(' ', 8 - Length(LibName)) + ':' + Version;
    end;

    if CanRunXbe(m_Xbe, {var}LibName) then
    begin
      // Update Xbe compatibility state only if not running already :
      if Emulation_State <> esRunning then
        Emulation_State := esFileOpen;
    end
    else
    begin
      // Update Xbe compatibility state only if not running already :
      if Emulation_State <> esRunning then
        Emulation_State := esInvalidFile;

      lblXbeInformation.Caption := lblXbeInformation.Caption + #13#13'Cannot start this XBE!'#13#10 + LibName;
      // TODO : Set a Disabled state in imgLaunchButton.Tag
    end;
  end
  else
  begin
    lblXbeInformation.Caption := '';
    Emulation_State := esNone;
  end;

  // Convert emulation state to a nice caption
  case Emulation_State of
    esNone:
      Caption := 'Dxbx';
    esFileOpen:
      Caption := m_szAsciiTitle + ' - Dxbx';
    esInvalidFile:
      Caption := m_szAsciiTitle + ' - Dxbx [Incompatible]';
    esRunning:
      Caption := m_szAsciiTitle + ' - Dxbx [Emulating]';
  end;

  if m_szAsciiTitle <> ''  then
    StatusBar.SimpleText := Format('DXBX: %s Loaded', [m_szAsciiTitle])
  else
    StatusBar.SimpleText := 'DXBX: No Xbe Loaded...';


  AdjustMenu;
end; // UpdateTitleInformation

function _ReadXBEInfoFromNode(const XBEInfoNode: IXMLNode): TXBEInfo;
var
  XDKNode, LibNode: IXMLNode;
begin
  Result := TXBEInfo.Create;
  Result.FileName := XML_ReadString(XBEInfoNode, 'FileName');
  if Result.FileName = '' then
  begin
    // Old-style 'Name' values are read here :
    Result.Title := XML_ReadString(XBEInfoNode, 'Name');
    Result.DumpInfo := '';
    Result.GameRegion := 0;
  end
  else
  begin
    Result.Title := XML_ReadString(XBEInfoNode, 'Title');
    Result.GameRegion := Cardinal(StrToIntDef(XML_ReadString(XBEInfoNode, 'GameRegion'), 0));
    Result.DumpInfo := XML_ReadString(XBEInfoNode, 'DumpInfo');
  end;

  XDKNode := XBEInfoNode.ChildNodes.FindNode('XDKVersions');
  if Assigned(XDKNode) then
  begin
    LibNode := XDKNode.ChildNodes.First;
    while Assigned(LibNode) do
    begin
      Result.LibVersions.Values[LibNode.LocalName] := LibNode.Text;
      LibNode := LibNode.NextSibling;
    end;

    Result.LibVersions.Sort;
  end;
end; // _ReadXBEInfoFromNode

function Tfrm_Main.InsertXBEInfo(const aXBEInfo: TXBEInfo; const aPreventDuplicates: Boolean = True): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(aXBEInfo) then
    Exit;

  if aPreventDuplicates then
    i := FindDuplicate(aXbeInfo)
  else
    i := -1;

  if i >= 0 then
  begin
    // Replace existing :
    MyXBEList.Objects[i].Free;
    MyXBEList.Objects[i] := aXBEInfo;
    MyXBEList.Strings[i] := aXBEInfo.Title;
  end
  else
    MyXBEList.AddObject(aXBEInfo.Title, aXBEInfo);

  Result := True;
end; // TfrmMain.InsertXBEInfo

function Tfrm_Main.LoadXBEListByFile(aImportFilePath: string = '';
  aUseImportDialog: Boolean = False): Integer;
var
  XMLRootNode: IXMLNode;
  XMLNode: IXMLNode;
  XBEInfoNode: IXMLNode;
  Publisher: string;
  FileName: string;
  XBEImportList: TStringList;
  i: Integer;
begin
  Result := 0;
  if aImportFilePath = '' then
    aImportFilePath := FApplicationDir + 'Dump.dat';

  if not FileExists(aImportFilePath) then
    Exit;

  XmlDocument.Active := False;
  XmlDocument.FileName := aImportFilePath;
  try
    XmlDocument.Active := True;
  except
    on E: EDOMParseError do
    begin
      MessageDlg('Error parsing the file!', mtError, [mbOk], -1);
      XmlDocument.Active := False;
    end
    else
      XmlDocument.Active := False;
  end;

  if not XmlDocument.Active then
    Exit;

  XBEImportList := TStringList.Create;
  try
    XMLRootNode := XMLDocument.DocumentElement;

    XMLNode := XMLRootNode.ChildNodes.FindNode('PublishedInfo');
    if Assigned(XMLNode) then
      Publisher := XML_ReadString(XMLNode, 'PublishedBy');

    XMLNode := XMLRootNode.ChildNodes.FindNode('GameList');
    if Assigned(XMLNode) then
      XBEInfoNode := XMLNode.ChildNodes.First
    else
      XBEInfoNode := XMLRootNode;

    while Assigned(XBEInfoNode) do
    begin
      FileName := XML_ReadString(XBEInfoNode, 'FileName');
      if FileName = '' then
        // Old-style 'Name' values are read here, interpreted as FileName :
        FileName := XML_ReadString(XBEInfoNode, 'Name');

      // For now, only add to list when the user can intervene,
      // or when not yet present :
//      if aUseImportDialog
//      or (FindByFileName(FileName) < 0) }then
        XBEImportList.AddObject('', _ReadXBEInfoFromNode(XBEInfoNode));

      XBEInfoNode := XBEInfoNode.NextSibling;
    end;

//    if aUseImportDialog then
//      Result := ShowImportList(XBEImportList, Publisher)
//    else
      for i := 0 to XBEImportList.Count - 1 do
        if InsertXBEInfo(TXBEInfo(XBEImportList.Objects[i])) then
          Inc(Result);

    MyXBEList.Sort;

  finally
    FreeAndNil({var}XBEImportList);
  end;
end;

function Tfrm_Main.LoadXBEListByXml(aXml: String): Integer;
var
  XMLRootNode: IXMLNode;
begin
  Result := 0;
  if aXml = '' then
    Exit;

  XmlDocument.Active := False;
  XmlDocument.LoadFromXML(aXml);
  try
    XmlDocument.Active := True;
  except
    on E: EDOMParseError do
    begin
      MessageDlg('Error parsing the file!', mtError, [mbOk], -1);
      XmlDocument.Active := False;
    end
    else
      XmlDocument.Active := False;
  end;

  if not XmlDocument.Active then
    Exit;

  XMLRootNode := XMLDocument.DocumentElement;
  InsertXBEInfo(TXBEInfo(_ReadXBEInfoFromNode(XMLRootNode)));
  MyXBEList.Sort;
end;

procedure Tfrm_Main.mnu_BypassSymbolCacheClick(Sender: TObject);
begin
  g_EmuShared.m_BypassSymbolCache := not g_EmuShared.m_BypassSymbolCache;
  g_EmuShared.Save;
  AdjustMenu;
end;

procedure Tfrm_Main.OpenXbeFile(aFileName: string);
var
  XbeXml: string;
begin
  if FileExists(aFileName) then
    if LoadXbe(aFileName) then
    begin
      DxbxXml.CreateXmlXbeDumpAsText(XbeXml, m_Xbe, aFileName);
      LoadXBEListByXml(XbeXml);
      UpdateFilter;
    end;
end;

// Tfrm_Main.LoadXBEList

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
end; // SendCommandToXdkTracker

procedure Tfrm_Main.StopEmulation;
begin
  if IsWindow(m_hwndChild) then
  begin
    SendMessage(m_hwndChild, WM_CLOSE, 0, 0);
    m_hwndChild := HNULL;
    Application.ProcessMessages;
    Emulation_State := esFileOpen;
    UpdateTitleInformation;
    AdjustMenu;
  end;
end;

end.
