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
  Menu = MainMenu1
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
  object lblXbeInformation: TLabel
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
    DrawingStyle = gdsGradient
    FixedColor = clGray
    FixedCols = 0
    RowCount = 99
    GradientEndColor = clGray
    GradientStartColor = clBlack
    Options = [goColSizing, goRowSelect]
    ParentColor = True
    TabOrder = 1
    OnClick = dgXbeInfosClick
    OnDblClick = dgXbeInfosDblClick
    OnDrawCell = dgXbeInfosDrawCell
    OnKeyDown = dgXbeInfosKeyDown
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
      object miCleanSymbolCache: TMenuItem
        Action = actCleanSymbolCache
      end
      object mnu_BypassSymbolCache: TMenuItem
        Caption = 'Bypass symbol cache'
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
  object ImportDialog: TOpenDialog
    DefaultExt = '*.xml'
    Filter = 'Xml Files ( *.xml )|*.xml'
    Left = 184
    Top = 12
  end
  object ExportDialog: TSaveDialog
    DefaultExt = '*.xml'
    Filter = 'Xml Files ( *.xml )|*.xml'
    Left = 216
    Top = 12
  end
end
