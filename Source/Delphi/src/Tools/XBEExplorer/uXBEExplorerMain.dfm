object FormXBEExplorer: TFormXBEExplorer
  Left = 0
  Top = 0
  Caption = 'XBE Explorer'
  ClientHeight = 612
  ClientWidth = 836
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 219
    Top = 23
    Height = 589
    ExplicitLeft = 424
    ExplicitTop = 376
    ExplicitHeight = 100
  end
  object PageControl: TPageControl
    Left = 222
    Top = 23
    Width = 614
    Height = 589
    Align = alClient
    TabOrder = 0
    ExplicitTop = 25
    ExplicitHeight = 701
  end
  object ActionMainMenuBar: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 836
    Height = 23
    UseSystemFont = False
    ActionManager = ActionManager
    ColorMap.HighlightColor = 15660791
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 15660791
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Spacing = 0
    ExplicitHeight = 25
  end
  object Panel1: TPanel
    Left = 0
    Top = 23
    Width = 219
    Height = 589
    Align = alLeft
    TabOrder = 2
    ExplicitTop = 25
    ExplicitHeight = 701
    object Splitter2: TSplitter
      Left = 1
      Top = 1
      Width = 217
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitTop = 166
    end
    object TreeView1: TTreeView
      Left = 1
      Top = 4
      Width = 217
      Height = 322
      Align = alTop
      HideSelection = False
      Indent = 19
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      OnChange = TreeView1Change
    end
    object Panel2: TPanel
      Left = 1
      Top = 326
      Width = 217
      Height = 262
      Align = alClient
      Caption = 'Panel2'
      TabOrder = 1
      ExplicitHeight = 374
      object edt_SymbolFilter: TEdit
        Left = 1
        Top = 1
        Width = 215
        Height = 21
        Align = alTop
        TabOrder = 0
        OnChange = edt_SymbolFilterChange
      end
      object lst_DissambledFunctions: TListView
        Left = 1
        Top = 22
        Width = 215
        Height = 239
        Align = alClient
        Columns = <
          item
            Caption = 'Address'
            Width = 63
          end
          item
            AutoSize = True
            Caption = 'Symbol names'
          end>
        ReadOnly = True
        RowSelect = True
        SortType = stText
        TabOrder = 1
        ViewStyle = vsReport
        OnColumnClick = lst_DissambledFunctionsColumnClick
        OnDblClick = lst_DissambledFunctionsDblClick
        ExplicitHeight = 351
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 64
    Top = 32
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Action = actFileOpen
      end
      object Close1: TMenuItem
        Action = actClose
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Copy2: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
        OnClick = Copy2Click
      end
    end
    object Extra1: TMenuItem
      Caption = 'Extra'
      Enabled = False
      object ExploreFileSystem1: TMenuItem
        Caption = 'Explore Filesystem...'
        OnClick = ExploreFileSystem1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.XBE'
    Filter = 'XBE files|*.xbe|ISO files|*.iso'
    Options = [ofReadOnly, ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofShareAware, ofNoTestFileCreate, ofEnableSizing]
    Left = 104
    Top = 32
  end
  object pmImage: TPopupMenu
    Left = 64
    Top = 72
    object SaveAs1: TMenuItem
      Action = actSaveAs
    end
  end
  object SavePictureDialog: TSavePictureDialog
    DefaultExt = '.bmp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShareAware, ofNoTestFileCreate, ofEnableSizing]
    Left = 104
    Top = 72
  end
  object ActionManager: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = actFileOpen
            ShortCut = 16463
          end
          item
            Action = actClose
          end
          item
            Caption = '-'
          end
          item
            Action = actSaveAs
          end>
        ActionBar = ActionMainMenuBar
      end>
    Left = 24
    Top = 32
    StyleName = 'XP Style'
    object actFileOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      Hint = 'Open|Opens an existing file'
      ShortCut = 16463
      OnExecute = actOpenExecute
    end
    object actClose: TAction
      Category = 'File'
      Caption = 'C&lose'
      Enabled = False
      Hint = 'Close'
      OnExecute = actCloseExecute
      OnUpdate = actCloseUpdate
    end
    object actExit: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 43
    end
    object actSaveAs: TAction
      Category = 'Edit'
      Caption = 'Save &as...'
      Enabled = False
      Hint = 'Save as|Saves the active file with a new name'
      OnExecute = actSaveAsExecute
      OnUpdate = actSaveAsUpdate
    end
  end
end
