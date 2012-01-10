unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, StdCtrls, Grids, ExtCtrls;

type

  { TMain }

  TMain = class(TForm)
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

  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }



end.

