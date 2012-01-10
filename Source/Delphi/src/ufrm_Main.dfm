object frm_Main: Tfrm_Main
  Left = 514
  Top = 225
  Caption = 'Dxbx'
  ClientHeight = 600
  ClientWidth = 803
  Color = clBlack
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
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
    Left = 655
    Top = 134
    Width = 128
    Height = 128
    Anchors = [akTop, akRight]
    Center = True
    Stretch = True
    Transparent = True
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
  object lblXbeInformation: TLabel
    Left = 642
    Top = 304
    Width = 3
    Height = 14
    Anchors = [akTop, akRight]
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
    Height = 44
    Anchors = [akRight, akBottom]
    OnClick = imgLaunchButtonClick
    OnMouseDown = imgLaunchButtonMouseDown
    OnMouseEnter = imgLaunchButtonMouseEnter
    OnMouseLeave = imgLaunchButtonMouseLeave
    OnMouseMove = imgLaunchButtonMouseMove
  end
  object lblFreeTextFilter: TLabel
    Left = 12
    Top = 548
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
  end
  object dgXbeInfos: TDrawGrid
    Left = 3
    Top = 120
    Width = 633
    Height = 404
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    BiDiMode = bdLeftToRight
    BorderStyle = bsNone
    Ctl3D = False
    DefaultColWidth = 120
    DefaultDrawing = False
    DrawingStyle = gdsClassic
    FixedColor = clGray
    FixedCols = 0
    RowCount = 99
    GradientEndColor = clGray
    GradientStartColor = clBlack
    GridLineWidth = 0
    Options = [goColSizing, goRowSelect]
    ParentBiDiMode = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 1
    OnClick = dgXbeInfosClick
    OnDblClick = dgXbeInfosDblClick
    OnDrawCell = dgXbeInfosDrawCell
    OnKeyDown = dgXbeInfosKeyDown
  end
  object cbFreeTextFilter: TComboBox
    Left = 137
    Top = 545
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    ParentColor = True
    TabOrder = 2
    OnKeyPress = cbFreeTextFilterKeyPress
    OnSelect = cbFreeTextFilterSelect
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 12
    object mnu_File: TMenuItem
      Caption = '&File'
      object mnu_OpenXbe: TMenuItem
        Action = actOpenXbe
      end
      object mnu_ImportXbes: TMenuItem
        Action = actImportXbes
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
      object N4: TMenuItem
        Caption = '-'
      end
      object Gamelist1: TMenuItem
        Caption = '&Game list'
        object Import1: TMenuItem
          Action = actImportGameList
        end
        object Export1: TMenuItem
          Action = actExportGameList
        end
        object N6: TMenuItem
          Caption = '-'
        end
        object Removeinvalidfromlist: TMenuItem
          Caption = 'Remove Invalid XBE'#39's'
          OnClick = actRemoveInvalidFromListExecute
        end
        object N7: TMenuItem
          Caption = '-'
        end
        object Clear1: TMenuItem
          Action = actClearGameList
        end
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
        object mnu_DebugOutputGuiNone: TMenuItem
          Action = actDebugGuiNone
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
        object mnu_DebugOutputKernelNone: TMenuItem
          Action = actDebugKernelNone
        end
      end
    end
    object Settings1: TMenuItem
      Caption = '&Settings'
      object miCleanSymbolCache: TMenuItem
        Action = actCleanSymbolCache
      end
      object mnu_BypassSymbolCache: TMenuItem
        Caption = 'Bypass Symbol Cache'
        OnClick = mnu_BypassSymbolCacheClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mnu_ConfigControler: TMenuItem
        Action = actConfiguration
      end
    end
    object Emulation1: TMenuItem
      Caption = '&Emulation'
      object Start1: TMenuItem
        Action = ActStartEmulation
      end
      object Stop1: TMenuItem
        Action = actStopEmulation
      end
    end
    object ools1: TMenuItem
      Caption = 'Tools'
      object miXbeExplorer: TMenuItem
        Action = actXbeExplorer
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
    Left = 64
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
    object actConfiguration: TAction
      Category = 'Settings'
      Caption = 'Configuration'
      OnExecute = actConfigurationExecute
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
    object actXIso: TAction
      Category = 'Tools'
      Caption = 'xIso - Dxbx Edition'
      OnExecute = actXIsoExecute
    end
    object actXdkTrackerXbeInfo: TAction
      Category = 'Edit'
      Caption = 'XDKTracker'
      OnExecute = actXdkTrackerXbeInfoExecute
    end
    object actCleanSymbolCache: TAction
      Category = 'Settings'
      Caption = 'Clean Symbol Cache'
      OnExecute = actCleanSymbolCacheExecute
    end
    object actStopEmulation: TAction
      Category = 'Emulation'
      Caption = 'Stop'
      ShortCut = 117
      OnExecute = actStopEmulationExecute
    end
    object actImportGameList: TAction
      Category = 'File'
      Caption = '&Import'
      OnExecute = actImportGameListExecute
    end
    object actExportGameList: TAction
      Category = 'File'
      Caption = '&Export'
      OnExecute = actExportGameListExecute
    end
    object actClearGameList: TAction
      Category = 'File'
      Caption = '&Clear'
      OnExecute = actClearGameListExecute
    end
    object actDebugKernelNone: TAction
      Category = 'View'
      Caption = '&None'
      OnExecute = actDebugKernelNoneExecute
    end
    object actDebugGuiNone: TAction
      Category = 'View'
      Caption = '&None'
      OnExecute = actDebugGuiNoneExecute
    end
    object actImportXbes: TAction
      Category = 'File'
      Caption = 'Import Xbe`s from dir'
      OnExecute = actImportXbesExecute
    end
    object actRemoveInvalidFromList: TAction
      Category = 'File'
      Caption = 'Remove Invalid XBE'#39's'
    end
  end
  object XbeOpenDialog: TOpenDialog
    FileName = 'default.xbe'
    Filter = 'Xbox Executable (*.xbe)|*.xbe|Xbox Iso (*.iso)|*.iso'
    Options = [ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofNoTestFileCreate, ofEnableSizing]
    Left = 128
    Top = 12
  end
  object SaveDialog: TSaveDialog
    FileName = 'DxbxDebug.txt'
    Filter = 'Text Documents ( *.txt )|*.txt'
    Left = 216
    Top = 108
  end
  object ExeSaveDialog: TSaveDialog
    DefaultExt = '*.exe'
    FileName = 'default.exe'
    Filter = 'Windows Executable (*.exe)|*.exe'
    Left = 216
    Top = 60
  end
  object XMLDocument: TXMLDocument
    Left = 8
    Top = 80
    DOMVendorDesc = 'MSXML'
  end
  object ImportDialog: TOpenDialog
    DefaultExt = '*.xml'
    Filter = 'Xml Files ( *.xml )|*.xml'
    Left = 128
    Top = 68
  end
  object ExportDialog: TSaveDialog
    DefaultExt = '*.xml'
    Filter = 'Xml Files ( *.xml )|*.xml'
    Left = 216
    Top = 12
  end
end
