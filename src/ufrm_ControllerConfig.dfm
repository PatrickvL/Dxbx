object frm_ControllerConfig: Tfrm_ControllerConfig
  Left = 271
  Top = 245
  Caption = 'Controllers Config'
  ClientHeight = 311
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 22
    Top = 282
    Width = 497
    Height = 21
  end
  object Label1: TLabel
    Left = 143
    Top = 286
    Width = 293
    Height = 13
    Caption = 'Please choose one of the controller components from above...'
  end
  object sTabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 529
    Height = 233
    MultiLine = True
    TabOrder = 8
    Tabs.Strings = (
      'Controller 1'
      'Controller 2'
      'Controller 3'
      'Controller 4')
    TabIndex = 0
    object chkForceFeedback: TCheckBox
      Left = 384
      Top = 203
      Width = 101
      Height = 17
      Caption = 'Force feedback'
      TabOrder = 0
    end
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 36
    Width = 333
    Height = 81
    Caption = 'Analog Buttons'
    TabOrder = 0
    object btn_X: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'X'
      TabOrder = 0
    end
    object btn_Y: TButton
      Left = 88
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Y'
      TabOrder = 1
    end
    object btn_A: TButton
      Left = 168
      Top = 16
      Width = 75
      Height = 25
      Caption = 'A'
      TabOrder = 2
    end
    object btn_B: TButton
      Left = 248
      Top = 16
      Width = 75
      Height = 25
      Caption = 'B'
      TabOrder = 3
    end
    object btn_White: TButton
      Left = 8
      Top = 48
      Width = 75
      Height = 25
      Caption = 'White'
      TabOrder = 4
    end
    object btn_Black: TButton
      Left = 88
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Black'
      TabOrder = 5
    end
    object btn_LeftTrigger: TButton
      Left = 168
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Left Trigger'
      TabOrder = 6
    end
    object btn_RightTrigger: TButton
      Left = 248
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Right Trigger'
      TabOrder = 7
    end
  end
  object GroupBox2: TGroupBox
    Left = 356
    Top = 36
    Width = 173
    Height = 81
    Caption = 'Analog Thumbstick (Left)'
    TabOrder = 1
    object btn_LeftUp: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Up'
      TabOrder = 0
    end
    object btn_LeftDown: TButton
      Left = 88
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Down'
      TabOrder = 1
    end
    object btn_LeftLeft: TButton
      Left = 8
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Left'
      TabOrder = 2
    end
    object btn_LeftRight: TButton
      Left = 88
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Right'
      TabOrder = 3
    end
  end
  object GroupBox3: TGroupBox
    Left = 16
    Top = 120
    Width = 333
    Height = 81
    Caption = 'Digital Buttons'
    TabOrder = 2
    object btn_DPadUp: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'DPad Up'
      TabOrder = 0
    end
    object btn_DPadDown: TButton
      Left = 88
      Top = 16
      Width = 75
      Height = 25
      Caption = 'DPad Down'
      TabOrder = 1
    end
    object btn_DPadLeft: TButton
      Left = 168
      Top = 16
      Width = 75
      Height = 25
      Caption = 'DPad Left'
      TabOrder = 2
    end
    object btn_DPadRight: TButton
      Left = 248
      Top = 16
      Width = 75
      Height = 25
      Caption = 'DPad Right'
      TabOrder = 3
    end
    object btn_Back: TButton
      Left = 8
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Back'
      TabOrder = 4
    end
    object btn_Start: TButton
      Left = 88
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 5
    end
    object btnLeftThumb: TButton
      Left = 168
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Left Thumb'
      TabOrder = 6
    end
    object btn_RightThumb: TButton
      Left = 248
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Right Thumb'
      TabOrder = 7
    end
  end
  object GroupBox4: TGroupBox
    Left = 356
    Top = 120
    Width = 173
    Height = 81
    Caption = 'Analog Thumbstick (Right)'
    TabOrder = 3
    object btn_RightUp: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Up'
      TabOrder = 0
    end
    object btn_RightDown: TButton
      Left = 88
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Down'
      TabOrder = 1
    end
    object btn_RightLeft: TButton
      Left = 8
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Left'
      TabOrder = 2
    end
    object btn_RightRight: TButton
      Left = 88
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Right'
      TabOrder = 3
    end
  end
  object btn_Accept: TButton
    Left = 364
    Top = 247
    Width = 75
    Height = 25
    Caption = 'Accept'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btn_Cancel: TButton
    Left = 444
    Top = 247
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object btn_LoadConfig: TButton
    Left = 24
    Top = 207
    Width = 157
    Height = 25
    Caption = 'Load Configuration'
    TabOrder = 6
  end
  object btn_SaveConfig: TButton
    Left = 187
    Top = 206
    Width = 153
    Height = 25
    Caption = 'Save Configuration'
    TabOrder = 7
  end
end
