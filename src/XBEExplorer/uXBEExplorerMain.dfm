object FormXBEExplorer: TFormXBEExplorer
  Left = 0
  Top = 0
  Caption = 'XBE Explorer'
  ClientHeight = 609
  ClientWidth = 836
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 225
    Height = 609
    Align = alLeft
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    OnChange = TreeView1Change
    ExplicitHeight = 465
  end
  object PageControl1: TPageControl
    Left = 225
    Top = 0
    Width = 611
    Height = 609
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 538
    ExplicitHeight = 465
  end
  object MainMenu1: TMainMenu
    Left = 40
    Top = 24
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open'
        OnClick = Open1Click
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Caption = 'About'
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.XBE'
    Filter = 'XBE files|*.xbe'
    Options = [ofReadOnly, ofPathMustExist, ofFileMustExist, ofShareAware, ofNoTestFileCreate, ofEnableSizing]
    Left = 104
    Top = 24
  end
end
