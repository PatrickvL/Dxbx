object FormSvnLogMessagePrompt: TFormSvnLogMessagePrompt
  Left = 423
  Top = 133
  ActiveControl = MemoLogMessage
  BorderIcons = [biSystemMenu]
  Caption = 'Subversion Log Message'
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
  object LabelPrompt: TLabel
    Left = 8
    Top = 8
    Width = 117
    Height = 13
    Caption = 'Enter your log message:'
    FocusControl = MemoLogMessage
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
  object MemoLogMessage: TMemo
    Left = 8
    Top = 24
    Width = 321
    Height = 153
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
    OnChange = MemoLogMessageChange
  end
end
