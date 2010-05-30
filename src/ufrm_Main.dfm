object frm_Main: Tfrm_Main
  Left = 514
  Top = 225
  Caption = 'Dxbx'
  ClientHeight = 600
  ClientWidth = 803
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesigned
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  DesignSize = (
    803
    600)
  PixelsPerInch = 96
  TextHeight = 13
  object ImageIcon: TImage
    Left = 659
    Top = 120
    Width = 128
    Height = 128
    Anchors = [akTop, akRight]
    Center = True
    Stretch = True
  end
  object ImageLogo: TImage
    Left = 671
    Top = 254
    Width = 100
    Height = 17
    Anchors = [akTop, akRight]
    Center = True
    Stretch = True
  end
  object Label1: TLabel
    Left = 642
    Top = 277
    Width = 153
    Height = 244
    Anchors = [akTop, akRight]
    AutoSize = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Droid Sans Mono'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object imgLaunchButton: TImage
    Left = 645
    Top = 530
    Width = 150
    Height = 45
    Anchors = [akRight, akBottom]
    OnClick = imgLaunchButtonClick
    OnMouseDown = imgLaunchButtonMouseDown
    OnMouseEnter = imgLaunchButtonMouseEnter
    OnMouseLeave = imgLaunchButtonMouseLeave
    OnMouseMove = imgLaunchButtonMouseMove
    ExplicitLeft = 642
  end
  object lblFreeTextFilter: TLabel
    Left = 8
    Top = 536
    Width = 119
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&Show entries containing :'
    FocusControl = cbFreeTextFilter
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 581
    Width = 803
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitWidth = 800
  end
  object dgXbeInfos: TDrawGrid
    Left = 3
    Top = 120
    Width = 633
    Height = 404
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    DefaultColWidth = 120
    FixedColor = clGray
    FixedCols = 0
    RowCount = 99
    Options = [goFixedVertLine, goFixedHorzLine, goColSizing, goRowSelect]
    ParentColor = True
    TabOrder = 1
    OnClick = dgXbeInfosClick
    OnDrawCell = dgXbeInfosDrawCell
    ExplicitWidth = 640
  end
  object cbFreeTextFilter: TComboBox
    Left = 133
    Top = 533
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    ParentColor = True
    TabOrder = 2
    OnKeyPress = cbFreeTextFilterKeyPress
    OnSelect = cbFreeTextFilterSelect
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 12
    object mnu_File: TMenuItem
      Caption = '&File'
      object mnu_OpenXbe: TMenuItem
        Action = actOpenXbe
      end
      object mnu_CloseXbe: TMenuItem
        Action = actCloseXbe
        Enabled = False
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnu_RecentXbefiles: TMenuItem
        Caption = 'Recent Xbe files'
        Enabled = False
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actClose
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object mnu_Logbitmap: TMenuItem
        Caption = 'Logo bitmap'
        Enabled = False
        object Import1: TMenuItem
          Caption = 'Import'
          Enabled = False
        end
        object mnu_ExportLogoBitmap: TMenuItem
          Action = actExportLogo
        end
      end
      object mnu_Gambitmap: TMenuItem
        Caption = 'Game image'
        Enabled = False
        object mnu_ExportGameBitmap: TMenuItem
          Action = actExportGameImage
        end
      end
      object mnu_Patch: TMenuItem
        Caption = 'Patch'
        Enabled = False
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnu_DumpxbeinfoTo: TMenuItem
        Caption = 'Dump xbe info to'
        Enabled = False
        object Console3: TMenuItem
          Action = actConsoleXbeInfo
        end
        object mnu_DumpXbeInfoToFile: TMenuItem
          Action = actFileXbeInfo
        end
        object N8: TMenuItem
          Caption = '-'
        end
        object XDKTracker2: TMenuItem
          Action = actXdkTrackerXbeInfo
        end
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object mnu_DebugoutputGUI: TMenuItem
        Caption = 'Debug output (GUI)'
        object mnu_GuiOutputConsole: TMenuItem
          Action = actConsoleDebugGui
        end
        object mnu_DebugOutputGuiFile: TMenuItem
          Action = actFileDebugGui
        end
      end
      object mnu_DebugoutputKernel: TMenuItem
        Caption = 'Debug output (Kernel)'
        object mnu_DebugKernelOutputConsole: TMenuItem
          Action = actConsoleDebugKernel
        end
        object mnu_DebugOutputKernelFile: TMenuItem
          Action = actFileDebugKernel
        end
      end
    end
    object Settings1: TMenuItem
      Caption = '&Settings'
      object mnu_ConfigControler: TMenuItem
        Action = actConfigController
        Caption = 'Config Controllers'
      end
      object mnu_ConfigAudio: TMenuItem
        Action = actConfigAudio
      end
      object mnu_Configvideo: TMenuItem
        Action = actConfigVideo
      end
      object miCleanSymbolCache: TMenuItem
        Action = actCleanSymbolCache
      end
    end
    object Emulation1: TMenuItem
      Caption = '&Emulation'
      object Start1: TMenuItem
        Action = ActStartEmulation
      end
    end
    object ools1: TMenuItem
      Caption = 'Tools'
      object miXbeExplorer: TMenuItem
        Action = actXbeExplorer
      end
      object XdkTracker1: TMenuItem
        Action = actXdkTracker
      end
      object xIso1: TMenuItem
        Action = actXIso
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object mnu_About: TMenuItem
        Action = actAbout
      end
    end
  end
  object ActionList: TActionList
    Left = 40
    Top = 12
    object actXbeExplorer: TAction
      Category = 'Tools'
      Caption = 'Xbe Explorer'
      OnExecute = actXbeExplorerExecute
    end
    object ActStartEmulation: TAction
      Category = 'Emulation'
      Caption = 'Start'
      ShortCut = 116
      OnExecute = ActStartEmulationExecute
    end
    object actAbout: TAction
      Category = 'About'
      Caption = 'About'
      OnExecute = actAboutExecute
    end
    object actConfigController: TAction
      Category = 'Settings'
      Caption = 'Config Controller'
      OnExecute = actConfigControllerExecute
    end
    object actConfigVideo: TAction
      Category = 'Settings'
      Caption = 'Config Video'
      OnExecute = actConfigVideoExecute
    end
    object actConfigAudio: TAction
      Category = 'Settings'
      Caption = 'Config Audio'
      Enabled = False
    end
    object actOpenXbe: TAction
      Category = 'File'
      Caption = 'Open Xbe'
      OnExecute = actOpenXbeExecute
    end
    object actClose: TAction
      Category = 'File'
      Caption = 'Close'
      OnExecute = actCloseExecute
    end
    object actCloseXbe: TAction
      Category = 'File'
      Caption = 'Close Xbe'
      OnExecute = actCloseXbeExecute
    end
    object actSaveXbe: TAction
      Category = 'File'
      Caption = 'Save Xbe'
      Enabled = False
    end
    object actSaveXbeAs: TAction
      Category = 'File'
      Caption = 'Save Xbe As...'
      Enabled = False
    end
    object actConsoleXbeInfo: TAction
      Category = 'Edit'
      Caption = '&Console'
      OnExecute = actConsoleXbeInfoExecute
    end
    object actFileXbeInfo: TAction
      Category = 'Edit'
      Caption = '&File'
      OnExecute = actFileXbeInfoExecute
    end
    object actConsoleDebugGui: TAction
      Category = 'View'
      Caption = '&Console'
      Checked = True
      OnExecute = actConsoleDebugGuiExecute
    end
    object actFileDebugGui: TAction
      Category = 'View'
      Caption = '&File'
      OnExecute = actFileDebugGuiExecute
    end
    object actConsoleDebugKernel: TAction
      Category = 'View'
      Caption = '&Console'
      Checked = True
      OnExecute = actConsoleDebugKernelExecute
    end
    object actFileDebugKernel: TAction
      Category = 'View'
      Caption = '&File'
      OnExecute = actFileDebugKernelExecute
    end
    object actImportLogo: TAction
      Category = 'Edit'
      Caption = 'Import'
    end
    object actExportLogo: TAction
      Category = 'Edit'
      Caption = 'Export'
      OnExecute = actExportLogoExecute
    end
    object actXdkTracker: TAction
      Category = 'Tools'
      Caption = 'XdkTracker'
      OnExecute = actXdkTrackerExecute
    end
    object actXIso: TAction
      Category = 'Tools'
      Caption = 'xIso - DXBX Edition'
      OnExecute = actXIsoExecute
    end
    object actXdkTrackerXbeInfo: TAction
      Category = 'Edit'
      Caption = 'XDKTracker'
      OnExecute = actXdkTrackerXbeInfoExecute
    end
    object actExportGameImage: TAction
      Category = 'Edit'
      Caption = 'Export'
      OnExecute = actExportGameImageExecute
    end
    object actCleanSymbolCache: TAction
      Category = 'Settings'
      Caption = 'Clean Symbol Cache'
      OnExecute = actCleanSymbolCacheExecute
    end
  end
  object XbeOpenDialog: TOpenDialog
    FileName = 'default.xbe'
    Filter = 'Xbox Executable (*.xbe)|*.xbe|Xbox Iso (*.iso)|*.iso'
    Options = [ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofNoTestFileCreate, ofEnableSizing]
    Left = 72
    Top = 12
  end
  object SaveDialog: TSaveDialog
    FileName = 'DxbxDebug.txt'
    Filter = 'Text Documents ( *.txt )|*.txt'
    Left = 104
    Top = 12
  end
  object LogoSaveDialog: TSaveDialog
    FileName = 'logo.bmp'
    Filter = 'Bitmap Image Files (*.bmp)|*.bmp'
    Title = 'Export Logo Bitmap'
    Left = 104
    Top = 76
  end
  object ExeSaveDialog: TSaveDialog
    DefaultExt = '*.exe'
    FileName = 'default.exe'
    Filter = 'Windows Executable (*.exe)|*.exe'
    Left = 104
    Top = 44
  end
  object XMLDocument: TXMLDocument
    Left = 8
    Top = 80
    DOMVendorDesc = 'MSXML'
  end
end
