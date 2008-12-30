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
    Top = 80
    Width = 737
    Height = 533
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      
        'Specify Dxbx and Cxbx source paths, and press the "Scan translat' +
        'ion" button.')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object edDxbxSrcPath: TEdit
    Left = 120
    Top = 13
    Width = 445
    Height = 21
    TabOrder = 2
    Text = '..\..'
  end
  object edCxbxSrcPath: TEdit
    Left = 120
    Top = 40
    Width = 445
    Height = 21
    TabOrder = 3
    Text = '..\..\..\..\Cxbx\trunk\src'
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
  object JvSelectDirectory: TJvSelectDirectory
    Left = 28
    Top = 88
  end
  object SaveDialog: TSaveDialog
    Left = 60
    Top = 86
  end
end
