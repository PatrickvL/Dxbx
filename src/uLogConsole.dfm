object frm_LogConsole: Tfrm_LogConsole
  Left = 486
  Top = 246
  Caption = 'Log Console'
  ClientHeight = 318
  ClientWidth = 494
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Log: TMemo
    Left = 0
    Top = 0
    Width = 494
    Height = 318
    Align = alClient
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
