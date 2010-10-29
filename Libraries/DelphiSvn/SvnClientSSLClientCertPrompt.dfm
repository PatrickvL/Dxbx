object FormSvnClientSSLClientCertPrompt: TFormSvnClientSSLClientCertPrompt
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Subversion SSL Client Certificate'
  ClientHeight = 226
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
    226)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelRealm: TLabel
    Left = 112
    Top = 8
    Width = 219
    Height = 64
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Please provide SSL client certificate for %s:'
    WordWrap = True
  end
  object LabelCertFileName: TLabel
    Left = 16
    Top = 86
    Width = 71
    Height = 13
    Caption = '&Certificate file:'
    FocusControl = EditCertFileName
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
    Top = 193
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
    Top = 193
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object EditCertFileName: TEdit
    Left = 16
    Top = 102
    Width = 294
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = EditChange
  end
  object ButtonBrowse: TButton
    Left = 310
    Top = 102
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = ButtonBrowseClick
  end
  object CheckBoxSave: TCheckBox
    Left = 16
    Top = 152
    Width = 97
    Height = 17
    Caption = '&Save authentication data'
    TabOrder = 4
  end
  object OpenDialog: TOpenDialog
    Filter = 'CA SSL certificates (*.pem)|*.pem|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Open SSL Client Certificate'
    Left = 16
    Top = 16
  end
end
