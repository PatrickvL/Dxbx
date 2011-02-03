object Form4: TForm4
  Left = 317
  Top = 120
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Grabar ISO'
  ClientHeight = 269
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 269
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      426
      269)
    object Label1: TLabel
      Left = 32
      Top = 37
      Width = 71
      Height = 13
      Caption = 'CD/DVD Drive'
    end
    object Bevel1: TBevel
      Left = 0
      Top = 197
      Width = 424
      Height = 10
      Anchors = [akLeft, akRight, akBottom]
      Shape = bsBottomLine
    end
    object Label2: TLabel
      Left = 25
      Top = 177
      Width = 30
      Height = 13
      Caption = 'Status'
    end
    object Label3: TLabel
      Left = 8
      Top = 96
      Width = 18
      Height = 13
      Caption = 'ISO'
    end
    object SpeedButton1: TSpeedButton
      Left = 384
      Top = 112
      Width = 23
      Height = 22
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000CE0E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        77777777777777777777000000000007777700333333333077770B0333333333
        07770FB03333333330770BFB0333333333070FBFB000000000000BFBFBFBFB07
        77770FBFBFBFBF0777770BFB0000000777777000777777770007777777777777
        7007777777770777070777777777700077777777777777777777}
      OnClick = SpeedButton1Click
    end
    object Bevel2: TBevel
      Left = 69
      Top = 13
      Width = 342
      Height = 10
      Anchors = [akLeft, akBottom]
      Shape = bsBottomLine
    end
    object Label4: TLabel
      Left = 128
      Top = 68
      Width = 31
      Height = 13
      Caption = 'Speed'
    end
    object Label5: TLabel
      Left = 8
      Top = 16
      Width = 53
      Height = 13
      Caption = 'Parametros'
    end
    object Label6: TLabel
      Left = 280
      Top = 68
      Width = 64
      Height = 13
      Caption = 'Write Method'
    end
    object Bevel3: TBevel
      Left = 32
      Top = 93
      Width = 381
      Height = 10
      Anchors = [akLeft, akBottom]
      Shape = bsBottomLine
    end
    object Label7: TLabel
      Left = 8
      Top = 116
      Width = 16
      Height = 13
      Caption = 'File'
    end
    object Label8: TLabel
      Left = 8
      Top = 152
      Width = 41
      Height = 13
      Caption = 'Progress'
    end
    object Bevel4: TBevel
      Left = 56
      Top = 149
      Width = 357
      Height = 10
      Anchors = [akLeft, akBottom]
      Shape = bsBottomLine
    end
    object lContador: TLabel
      Left = 368
      Top = 177
      Width = 42
      Height = 13
      Caption = '00:00:00'
    end
    object BotonGrabar: TButton
      Left = 249
      Top = 213
      Width = 70
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Save'
      Enabled = False
      TabOrder = 0
      OnClick = BotonGrabarClick
    end
    object cGrabadoras: TComboBox
      Left = 112
      Top = 32
      Width = 297
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = cGrabadorasChange
    end
    object Button2: TButton
      Left = 335
      Top = 213
      Width = 70
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Close'
      TabOrder = 2
      OnClick = Button2Click
    end
    object ProgressBar1: TProgressBar
      Left = 64
      Top = 176
      Width = 297
      Height = 17
      Smooth = True
      Step = 1
      TabOrder = 3
    end
    object eImagenISO: TEdit
      Left = 64
      Top = 112
      Width = 313
      Height = 21
      TabOrder = 4
    end
    object cVelocidad: TComboBox
      Left = 168
      Top = 64
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 5
      Text = 'MAX'
      Items.Strings = (
        'MAX'
        '40x'
        '32x'
        '24x'
        '16x'
        '12x'
        '10x'
        '8x'
        '4x'
        '2x'
        '1x')
    end
    object ComboBox1: TComboBox
      Left = 352
      Top = 64
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 6
      Text = 'TAO'
      Items.Strings = (
        'TAO')
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.iso'
    Filter = 'ISO|*.iso;*.xiso'
    Left = 8
  end
  object tContadorGrabacion: TTimer
    Enabled = False
    OnTimer = tContadorGrabacionTimer
    Left = 40
  end
end
