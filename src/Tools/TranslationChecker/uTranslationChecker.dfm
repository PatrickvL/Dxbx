object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Translation checker'
  ClientHeight = 613
  ClientWidth = 737
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblDxbxSrcPath: TLabel
    Left = 17
    Top = 21
    Width = 97
    Height = 13
    Caption = '&Dxbx sources path :'
    FocusControl = edDxbxSrcPath
  end
  object lblCxbxSrcPath: TLabel
    Left = 17
    Top = 48
    Width = 97
    Height = 13
    Caption = '&Cxbx sources path :'
    FocusControl = edCxbxSrcPath
  end
  object btnDxbxSourcesPath: TSpeedButton
    Left = 571
    Top = 11
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = btnDxbxSourcesPathClick
  end
  object btnCxbxSourcesPath: TSpeedButton
    Left = 571
    Top = 42
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = btnCxbxSourcesPathClick
  end
  object lblFilterDone: TLabel
    Left = 472
    Top = 77
    Width = 90
    Height = 13
    Caption = 'Done &percentage :'
    FocusControl = seFilterDone
  end
  object lblFilterTranslator: TLabel
    Left = 319
    Top = 77
    Width = 56
    Height = 13
    Caption = '&Translator :'
    FocusControl = ebFilterTranslator
  end
  object lblFilterBranch: TLabel
    Left = 17
    Top = 75
    Width = 67
    Height = 13
    Caption = 'Filter &Branch :'
    FocusControl = ebFilterBranch
  end
  object btnScanTranslation: TButton
    Left = 600
    Top = 8
    Width = 129
    Height = 25
    Caption = '&Scan translation'
    TabOrder = 0
    OnClick = btnScanTranslationClick
  end
  object memOutput: TMemo
    Left = 0
    Top = 96
    Width = 737
    Height = 517
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      
        'Specify Dxbx and Cxbx source paths, and press the "Scan translat' +
        'ion" button.')
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitLeft = -8
  end
  object edDxbxSrcPath: TEdit
    Left = 139
    Top = 13
    Width = 426
    Height = 21
    TabOrder = 2
    Text = '..\src\DxbxKrnl'
  end
  object edCxbxSrcPath: TEdit
    Left = 139
    Top = 40
    Width = 426
    Height = 21
    TabOrder = 3
    Text = '..\..\cxbx\branches\private\martin'
  end
  object btnSaveToXml: TButton
    Left = 600
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Save to disk'
    TabOrder = 4
    OnClick = btnSaveToXmlClick
  end
  object cbFilterDoneLess: TCheckBox
    Left = 620
    Top = 73
    Width = 50
    Height = 17
    Caption = '&Less?'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object seFilterDone: TSpinEdit
    Left = 568
    Top = 70
    Width = 46
    Height = 22
    MaxValue = 101
    MinValue = 0
    TabOrder = 6
    Value = 101
  end
  object ebFilterTranslator: TEdit
    Left = 381
    Top = 69
    Width = 85
    Height = 21
    TabOrder = 7
  end
  object ebFilterBranch: TEdit
    Left = 187
    Top = 67
    Width = 126
    Height = 21
    TabOrder = 8
  end
  object cbFilterNot: TCheckBox
    Left = 139
    Top = 73
    Width = 38
    Height = 17
    Caption = '&Not'
    TabOrder = 9
  end
  object SaveDialog: TSaveDialog
    Left = 508
    Top = 136
  end
end
