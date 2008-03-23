object frm_Publisher: Tfrm_Publisher
  Left = 426
  Top = 312
  Caption = 'Publisher'
  ClientHeight = 84
  ClientWidth = 266
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 265
    Height = 53
    Shape = bsFrame
  end
  object lbl_Publisher: TLabel
    Left = 12
    Top = 12
    Width = 75
    Height = 13
    Caption = 'Publisher name:'
  end
  object edtPublisher: TEdit
    Left = 12
    Top = 28
    Width = 249
    Height = 21
    TabOrder = 0
  end
  object btn_Cancel: TButton
    Left = 192
    Top = 64
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btn_Ok: TButton
    Left = 112
    Top = 64
    Width = 75
    Height = 25
    Caption = '&Ok'
    ModalResult = 1
    TabOrder = 2
  end
end
