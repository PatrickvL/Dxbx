object frm_AddGame: Tfrm_AddGame
  Left = 545
  Top = 355
  BorderStyle = bsDialog
  Caption = 'Add Game'
  ClientHeight = 183
  ClientWidth = 230
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 4
    Top = 4
    Width = 221
    Height = 141
    Shape = bsFrame
  end
  object lbl_GameName: TLabel
    Left = 12
    Top = 12
    Width = 62
    Height = 13
    Caption = 'Game Name:'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lbl_GameType: TLabel
    Left = 12
    Top = 56
    Width = 58
    Height = 13
    Caption = 'Game Type:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lbl_XdkVersion: TLabel
    Left = 12
    Top = 100
    Width = 59
    Height = 13
    Caption = 'Xdk version:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object edt_GameName: TEdit
    Left = 12
    Top = 28
    Width = 205
    Height = 21
    TabOrder = 0
  end
  object cmb_GameType: TComboBox
    Left = 12
    Top = 72
    Width = 205
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnKeyPress = cmb_XdkVersionKeyPress
    Items.Strings = (
      'All games'
      'Action'
      'Adventure'
      'Fighting'
      'Platform'
      'Racing'
      'Role Playing'
      'Simulation'
      'Sport')
  end
  object btn_Ok: TButton
    Left = 72
    Top = 152
    Width = 75
    Height = 25
    Caption = '&Ok'
    TabOrder = 2
    OnClick = btn_OkClick
  end
  object bn_Cancel: TButton
    Left = 152
    Top = 152
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object cmb_XdkVersion: TComboBox
    Left = 12
    Top = 116
    Width = 205
    Height = 21
    ItemHeight = 13
    TabOrder = 4
    OnKeyPress = cmb_XdkVersionKeyPress
    Items.Strings = (
      'All games'
      'Action'
      'Adventure'
      'Fighting'
      'Platform'
      'Racing'
      'Role Playing'
      'Simulation'
      'Sport')
  end
end
