object FormXBEExplorer: TFormXBEExplorer
  Left = 0
  Top = 0
  Caption = 'XBE Explorer'
  ClientHeight = 788
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
    Top = 25
    Height = 763
    ExplicitLeft = 424
    ExplicitTop = 376
    ExplicitHeight = 100
  end
  object PageControl: TPageControl
    Left = 222
    Top = 25
    Width = 614
    Height = 763
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 304
    ExplicitTop = 23
    ExplicitWidth = 532
    ExplicitHeight = 765
  end
  object ActionMainMenuBar: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 836
    Height = 25
    ActionManager = ActionManager
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMenuText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    PersistentHotKeys = True
    Spacing = 0
    ExplicitHeight = 23
  end
  object Panel1: TPanel
    Left = 0
    Top = 25
    Width = 219
    Height = 763
    Align = alLeft
    TabOrder = 2
    ExplicitTop = 23
    ExplicitHeight = 765
    object Splitter2: TSplitter
      Left = 1
      Top = 369
      Width = 217
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitWidth = 395
    end
    object TreeView1: TTreeView
      Left = 1
      Top = 1
      Width = 217
      Height = 368
      Align = alTop
      HideSelection = False
      Indent = 19
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      OnChange = TreeView1Change
    end
    object lst_DissambledFunctions: TListView
      Left = 1
      Top = 372
      Width = 217
      Height = 390
      Align = alClient
      Columns = <
        item
          Caption = 'Dissambled Functions'
          Width = 150
        end
        item
          AutoSize = True
          Caption = 'Address'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
      ExplicitHeight = 392
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
