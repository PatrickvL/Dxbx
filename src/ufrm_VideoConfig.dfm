object frm_VideoConfig: Tfrm_VideoConfig
  Left = 563
  Top = 388
  Caption = 'Dxbx : Video Configuration'
  ClientHeight = 211
  ClientWidth = 453
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
    453
    211)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 434
    Height = 166
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Direct3D Configuration'
    TabOrder = 0
    ExplicitWidth = 318
    object lbl_DisplayAdapter: TLabel
      Left = 8
      Top = 24
      Width = 110
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Display Adapter:'
    end
    object lbl_Direct3DDevice: TLabel
      Left = 8
      Top = 52
      Width = 110
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Direct3D Device:'
    end
    object lbl_VideoResolution: TLabel
      Left = 8
      Top = 80
      Width = 110
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Video Resolution:'
    end
    object lbl_OtherOptions: TLabel
      Left = 8
      Top = 112
      Width = 110
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Other Options:'
    end
    object edt_DisplayAdapter: TComboBox
      Left = 128
      Top = 21
      Width = 297
      Height = 21
      TabOrder = 0
      OnKeyPress = ComboReadOnly
    end
    object edt_Direct3dDevice: TComboBox
      Left = 128
      Top = 48
      Width = 297
      Height = 21
      ItemIndex = 0
      TabOrder = 1
      Text = 'Direct3D HAL (Hardware Accelerated)'
      OnKeyPress = ComboReadOnly
      Items.Strings = (
        'Direct3D HAL (Hardware Accelerated)'
        'Direct3D REF (Software)')
    end
    object edt_VideoResolution: TComboBox
      Left = 128
      Top = 76
      Width = 297
      Height = 21
      TabOrder = 2
      OnKeyPress = ComboReadOnly
    end
    object chk_FullScreen: TCheckBox
      Left = 128
      Top = 108
      Width = 109
      Height = 17
      Caption = 'Start in FullScreen'
      TabOrder = 3
    end
    object chk_VSync: TCheckBox
      Left = 312
      Top = 108
      Width = 97
      Height = 17
      Caption = 'Force VSync'
      TabOrder = 4
    end
    object chk_HardwareYUV: TCheckBox
      Left = 128
      Top = 131
      Width = 297
      Height = 17
      Caption = 'Enable Hardware YUV Overlays'
      TabOrder = 5
    end
  end
  object btn_Accept: TButton
    Left = 370
    Top = 180
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Accept'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btn_AcceptClick
    ExplicitLeft = 254
  end
  object btn_Cancel: TButton
    Left = 289
    Top = 180
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 173
  end
end
