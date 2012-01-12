unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  // Lazarus
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, StdCtrls, Grids, ExtCtrls,
  // Dxbx
  uDxbxUtils
  ;

type

  { TMain }

  TMain = class(TForm)
    actAbout: TAction;
    ActionList: TActionList;
    cbFreeTextFilter: TComboBox;
    DrawGrid1: TDrawGrid;
    Image1: TImage;
    Image2: TImage;
    lblFreeTextFilter: TLabel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    mnuAbout: TMenuItem;
    mnuXiso: TMenuItem;
    mnuXbeExplorer: TMenuItem;
    mnuStopEmu: TMenuItem;
    mnuStartEmu: TMenuItem;
    mnuConfiguration: TMenuItem;
    mnuBypassSymbolCache: TMenuItem;
    mnuCleanSymbolCache: TMenuItem;
    mnuDebugOutKrnlNone: TMenuItem;
    mnuDebugOutKrnlFile: TMenuItem;
    mnuDebugOutKrnlConsole: TMenuItem;
    mnuDebugOutGuiNone: TMenuItem;
    mnuDebugOutGuiFile: TMenuItem;
    mnuDebugOutGuiConsole: TMenuItem;
    mnuDebugOutKrnl: TMenuItem;
    mnuDebugOutGui: TMenuItem;
    mnuXDKTracker: TMenuItem;
    mnuDumpToFile: TMenuItem;
    mnuDumpConsole: TMenuItem;
    mnuDumpXbeInfoTo: TMenuItem;
    mnuPatch: TMenuItem;
    mnuClose: TMenuItem;
    mnuGameList: TMenuItem;
    mnuRecentXbefiles: TMenuItem;
    mnuCloseXbe: TMenuItem;
    mnuImportXbesFromDir: TMenuItem;
    mnuOpenXbe: TMenuItem;
    mnuHelp: TMenuItem;
    mnuTools: TMenuItem;
    mnuEmulation: TMenuItem;
    mnuSettings: TMenuItem;
    mnuView: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFile: TMenuItem;
    ImportDialog: TOpenDialog;
    ExportDialog: TSaveDialog;
    ExeSaveDialog: TSaveDialog;
    SaveDialog: TSaveDialog;
    XbeOpenDialog: TOpenDialog;
    procedure actAboutExecute(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Main: TMain;
  KernelDebugMode: TDebugMode = dmNone;
  KernelDebugFilePath: string = ''; // Just the default folder

implementation

{$R *.lfm}

{ TMain }

uses
  aboutFrm;

function BrowseDialogCallBack
  (Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM):
  integer stdcall;
var
  wa, rect: TRect;
  dialogPT: TPoint;
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
    SendMessage(wnd, BFFM_SETSELECTIONW, Longint(true), lpdata);
  end;

  Result := 0;
end;

function GetTitleSpecificKernelDebugFilePath: string;
begin
  // Was DXBX_KERNEL_DEBUG_FILENAME
  if KernelDebugFilePath <> '' then
    if LastChar(KernelDebugFilePath) <> '\' then
      KernelDebugFilePath := KernelDebugFilePath + '\';

  Result := KernelDebugFilePath + Format('DxbxKrnl %s (%d).txt', [TitleToNiceFilename(m_szAsciiTitle), SvnRevision])
end;

{ TMain }

procedure TMain.actAboutExecute(Sender: TObject);
begin
  TAboutFrm.Execute;
end;


end.

