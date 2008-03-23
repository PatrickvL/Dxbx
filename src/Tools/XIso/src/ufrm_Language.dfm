object Form2: TForm2
  Left = 528
  Top = 149
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 68
  ClientWidth = 142
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    142
    68)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 141
    Height = 67
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Language'
    TabOrder = 0
    object RadioButton1: TRadioButton
      Left = 16
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Spanish'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButton1Click
    end
    object RadioButton2: TRadioButton
      Left = 16
      Top = 40
      Width = 113
      Height = 17
      Caption = 'English'
      TabOrder = 1
      OnClick = RadioButton2Click
    end
  end
end
