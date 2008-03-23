object Form3: TForm3
  Left = 341
  Top = 220
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Make xISO'
  ClientHeight = 224
  ClientWidth = 474
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 474
    Height = 224
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Memo1: TMemo
      Left = 8
      Top = 16
      Width = 457
      Height = 65
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 96
      Width = 457
      Height = 73
      Caption = 'xISO Image'
      TabOrder = 1
      object Label1: TLabel
        Left = 11
        Top = 24
        Width = 41
        Height = 13
        Caption = 'Progress'
      end
      object Label2: TLabel
        Left = 328
        Top = 24
        Width = 53
        Height = 13
        Caption = 'Total Time:'
        Visible = False
      end
      object Label3: TLabel
        Left = 392
        Top = 24
        Width = 3
        Height = 13
      end
      object ProgressBar1: TProgressBar
        Left = 11
        Top = 40
        Width = 433
        Height = 20
        Smooth = True
        Step = 1
        TabOrder = 0
      end
    end
    object Button1: TButton
      Left = 200
      Top = 184
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 2
      OnClick = Button1Click
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.iso'
    Filter = 'ISO XBOX|*.iso;*.xiso'
    Left = 368
    Top = 192
  end
end
