object FormSvnClientLoginPrompt: TFormSvnClientLoginPrompt
  Left = 0
  Top = 0
  ActiveControl = EditPassword
  BorderIcons = [biSystemMenu]
  Caption = 'Subversion Login'
  ClientHeight = 249
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    337
    249)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelRealm: TLabel
    Left = 112
    Top = 8
    Width = 219
    Height = 64
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Please log in to %s:'
    WordWrap = True
  end
  object LabelUserName: TLabel
    Left = 16
    Top = 86
    Width = 55
    Height = 13
    Caption = '&User name:'
    FocusControl = EditUserName
  end
  object LabelPassword: TLabel
    Left = 16
    Top = 126
    Width = 50
    Height = 13
    Caption = '&Password:'
    FocusControl = EditPassword
  end
  object ImageLogo: TImage
    Left = 8
    Top = 8
    Width = 88
    Height = 64
    AutoSize = True
  end
  object ButtonOK: TButton
    Left = 176
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object ButtonCancel: TButton
    Left = 256
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object EditUserName: TEdit
    Left = 16
    Top = 102
    Width = 235
    Height = 21
    TabOrder = 2
    OnChange = EditChange
  end
  object EditPassword: TEdit
    Left = 16
    Top = 142
    Width = 235
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
    OnChange = EditChange
  end
  object CheckBoxSave: TCheckBox
    Left = 16
    Top = 185
    Width = 121
    Height = 17
    Caption = '&Save authentication'
    TabOrder = 4
  end
end
