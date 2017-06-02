object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 635
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    862
    635)
  PixelsPerInch = 96
  TextHeight = 13
  object EditName: TEdit
    Left = 40
    Top = 184
    Width = 345
    Height = 21
    TabOrder = 0
  end
  object MemoInput: TMemo
    Left = 40
    Top = 16
    Width = 793
    Height = 146
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 1
    OnChange = MemoInputChange
  end
  object EditVersion: TEdit
    Left = 40
    Top = 221
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object EditPattern: TEdit
    Left = 40
    Top = 264
    Width = 793
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    ExplicitWidth = 761
  end
  object MemoOutput: TMemo
    Left = 40
    Top = 328
    Width = 793
    Height = 228
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object Button1: TButton
    Left = 719
    Top = 569
    Width = 114
    Height = 34
    Anchors = [akRight, akBottom]
    Caption = 'Button1'
    TabOrder = 5
    OnClick = Button1Click
    ExplicitLeft = 726
    ExplicitTop = 576
  end
end
