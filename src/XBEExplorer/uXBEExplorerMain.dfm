object FormXBEExplorer: TFormXBEExplorer
  Left = 0
  Top = 0
  Caption = 'XBE Explorer'
  ClientHeight = 704
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
  object TreeView1: TTreeView
    Left = 0
    Top = 23
    Width = 225
    Height = 681
    Align = alLeft
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    OnChange = TreeView1Change
  end
  object PageControl: TPageControl
    Left = 225
    Top = 23
    Width = 611
    Height = 681
    Align = alClient
    TabOrder = 1
  end
  object ActionMainMenuBar: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 836
    Height = 23
    UseSystemFont = False
    ActionManager = ActionManager
    ColorMap.HighlightColor = 14410210
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 14410210
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    PersistentHotKeys = True
    Spacing = 0
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
  object pmHexViewer: TPopupMenu
    Left = 64
    Top = 112
    object miGotoOffset: TMenuItem
      Action = actGotoOffset
    end
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
          end
          item
            Action = actGotoOffset
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
    object actGotoOffset: TAction
      Category = 'Edit'
      Caption = '&Goto offset...'
      Enabled = False
      OnExecute = actGotoOffsetExecute
      OnUpdate = actGotoOffsetUpdate
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
