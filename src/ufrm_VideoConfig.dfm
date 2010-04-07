object frm_VideoConfig: Tfrm_VideoConfig
  Left = 563
  Top = 388
  Caption = 'Video Configuration'
  ClientHeight = 211
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    337
    211)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 318
    Height = 166
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Direct3D Configuration'
    TabOrder = 0
    ExplicitWidth = 317
    ExplicitHeight = 133
    object lbl_DisplayAdapter: TLabel
      Left = 8
      Top = 24
      Width = 77
      Height = 13
      Caption = 'Display Adapter:'
    end
    object lbl_Direct3DDevice: TLabel
      Left = 8
      Top = 52
      Width = 82
      Height = 13
      Caption = 'Direct3D Device:'
    end
    object lbl_VideoResolution: TLabel
      Left = 8
      Top = 80
      Width = 83
      Height = 13
      Caption = 'Video Resolution:'
    end
    object lbl_OtherOptions: TLabel
      Left = 8
      Top = 112
      Width = 68
      Height = 13
      Caption = 'Other Options:'
    end
    object edt_DisplayAdapter: TComboBox
      Left = 96
      Top = 20
      Width = 213
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnKeyPress = ComboReadOnly
    end
    object edt_Direct3dDevice: TComboBox
      Left = 96
      Top = 48
      Width = 213
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'Direct3D HAL (Hardware Accelerated)'
      OnKeyPress = ComboReadOnly
      Items.Strings = (
        'Direct3D HAL (Hardware Accelerated)'
        'Direct3D REF (Software)')
    end
    object edt_VideoResolution: TComboBox
      Left = 96
      Top = 76
      Width = 213
      Height = 21
      ItemHeight = 13
      TabOrder = 2
      OnKeyPress = ComboReadOnly
    end
    object chk_FullScreen: TCheckBox
      Left = 96
      Top = 108
      Width = 109
      Height = 17
      Caption = 'Start in FullScreen'
      TabOrder = 3
    end
    object chk_VSync: TCheckBox
      Left = 208
      Top = 108
      Width = 97
      Height = 17
      Caption = 'Force VSync'
      TabOrder = 4
    end
    object chk_HardwareYUV: TCheckBox
      Left = 96
      Top = 131
      Width = 109
      Height = 17
      Caption = 'Hardware YUV'
      TabOrder = 5
    end
  end
  object btn_Accept: TButton
    Left = 170
    Top = 180
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Accept'
    ModalResult = 1
    TabOrder = 1
    OnClick = btn_AcceptClick
    ExplicitLeft = 169
    ExplicitTop = 147
  end
  object btn_Cancel: TButton
    Left = 251
    Top = 180
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 250
    ExplicitTop = 147
  end
end
