object frm_Xdkversion: Tfrm_Xdkversion
  Left = 281
  Top = 178
  BorderStyle = bsDialog
  Caption = 'Game List'
  ClientHeight = 299
  ClientWidth = 491
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    491
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 480
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object lbl_Gametype: TLabel
    Left = 12
    Top = 12
    Width = 54
    Height = 13
    Caption = 'Game type:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lbl_Xdk: TLabel
    Left = 263
    Top = 40
    Width = 80
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = 'XDK Information:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object cmb_gametype: TComboBox
    Left = 12
    Top = 28
    Width = 245
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = 'All XDK Versions'
    Items.Strings = (
      'All XDK Versions')
  end
  object mem_XdkVersions: TMemo
    Left = 264
    Top = 56
    Width = 212
    Height = 229
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
  object lst_Games: TListBox
    Left = 12
    Top = 56
    Width = 245
    Height = 229
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 2
    OnClick = lst_GamesClick
  end
end
