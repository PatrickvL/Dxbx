object FormSvnClientSSLServerTrustPrompt: TFormSvnClientSSLServerTrustPrompt
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Subversion SSL Server'
  ClientHeight = 385
  ClientWidth = 449
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
    449
    385)
  PixelsPerInch = 96
  TextHeight = 13
  object ImageLogo: TImage
    Left = 8
    Top = 8
    Width = 88
    Height = 64
    AutoSize = True
  end
  object LabelRealm: TLabel
    Left = 112
    Top = 8
    Width = 331
    Height = 64
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Server %s returned the following SSL server certificate:'
    WordWrap = True
  end
  object ButtonOK: TButton
    Left = 288
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object ButtonCancel: TButton
    Left = 368
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PageControl: TPageControl
    Left = 8
    Top = 80
    Width = 435
    Height = 257
    ActivePage = TabSheetPrompt
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object TabSheetPrompt: TTabSheet
      Caption = 'Server Trust'
      DesignSize = (
        427
        229)
      object LabelFailures: TLabel
        Left = 8
        Top = 8
        Width = 62
        Height = 13
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'LabelFailures'
      end
      object LabelTrustPrompt: TLabel
        Left = 8
        Top = 120
        Width = 198
        Height = 13
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'Do you accept the SSL server certificate?'
      end
      object CheckBoxSave: TCheckBox
        Left = 8
        Top = 144
        Width = 198
        Height = 17
        Caption = '&Save authentication data'
        TabOrder = 0
      end
    end
    object TabSheetCertificate: TTabSheet
      Caption = 'Certificate'
      ImageIndex = 1
      DesignSize = (
        427
        229)
      object LabelHostName: TLabel
        Left = 8
        Top = 11
        Width = 56
        Height = 13
        Caption = 'Host Name:'
        FocusControl = EditHostName
      end
      object LabelFingerprint: TLabel
        Left = 8
        Top = 35
        Width = 56
        Height = 13
        Caption = 'Fingerprint:'
        FocusControl = EditFingerprint
      end
      object LabelValidFrom: TLabel
        Left = 8
        Top = 59
        Width = 53
        Height = 13
        Caption = 'Valid From:'
        FocusControl = EditValidFrom
      end
      object LabelValidUntil: TLabel
        Left = 8
        Top = 83
        Width = 50
        Height = 13
        Caption = 'Valid Until:'
        FocusControl = EditValidUntil
      end
      object LabelIssuer: TLabel
        Left = 8
        Top = 107
        Width = 34
        Height = 13
        Caption = 'Issuer:'
        FocusControl = EditIssuer
      end
      object LabelCertData: TLabel
        Left = 8
        Top = 131
        Width = 80
        Height = 13
        Caption = 'Certificate Data:'
      end
      object EditHostName: TEdit
        Left = 104
        Top = 8
        Width = 315
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
      object EditFingerprint: TEdit
        Left = 104
        Top = 32
        Width = 315
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ParentColor = True
        ReadOnly = True
        TabOrder = 1
      end
      object EditValidFrom: TEdit
        Left = 104
        Top = 56
        Width = 153
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 2
      end
      object EditValidUntil: TEdit
        Left = 104
        Top = 80
        Width = 153
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 3
      end
      object EditIssuer: TEdit
        Left = 104
        Top = 104
        Width = 315
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ParentColor = True
        ReadOnly = True
        TabOrder = 4
      end
      object MemoCertData: TMemo
        Left = 104
        Top = 128
        Width = 315
        Height = 89
        Anchors = [akLeft, akTop, akRight, akBottom]
        ParentColor = True
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 5
      end
    end
  end
end
