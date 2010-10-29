object FormSvnOptions: TFormSvnOptions
  Left = 301
  Top = 78
  Width = 457
  Height = 333
  BorderIcons = [biSystemMenu]
  Caption = 'Subversion Options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    449
    306)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDirs: TLabel
    Left = 8
    Top = 8
    Width = 125
    Height = 13
    Caption = 'Working Copy &Directories:'
    FocusControl = ComboBoxDirs
  end
  object ButtonCancel: TButton
    Left = 366
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ButtonOK: TButton
    Left = 285
    Top = 273
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 0
  end
  object ComboBoxDirs: TComboBox
    Left = 8
    Top = 24
    Width = 406
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 2
    OnChange = ComboBoxDirsChange
  end
  object ButtonBrowse: TButton
    Left = 420
    Top = 24
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = ButtonBrowseClick
  end
  object CheckBoxConfirmAdd: TCheckBox
    Left = 8
    Top = 75
    Width = 257
    Height = 17
    Caption = 'Confirm &Add'
    TabOrder = 5
    OnClick = CheckBoxClick
  end
  object CheckBoxAllowEmptyCommitMsg: TCheckBox
    Left = 8
    Top = 122
    Width = 257
    Height = 17
    Caption = 'Allow empty Commit &message'
    TabOrder = 7
    OnClick = CheckBoxClick
  end
  object CheckBoxRecurseUnversioned: TCheckBox
    Left = 8
    Top = 50
    Width = 257
    Height = 17
    Caption = 'Recurse &unversioned subdirectories'
    TabOrder = 4
    OnClick = CheckBoxClick
  end
  object CheckBoxCommitExternals: TCheckBox
    Left = 8
    Top = 98
    Width = 257
    Height = 17
    Caption = 'Commit &externals'
    TabOrder = 6
    OnClick = CheckBoxClick
  end
end
