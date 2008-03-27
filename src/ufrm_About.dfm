object frm_About: Tfrm_About
  Left = 453
  Top = 341
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 202
  ClientWidth = 283
  Color = clBackground
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 12
    Width = 126
    Height = 18
    Caption = 'DXBX: - Version :'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblAbout: TLabel
    Left = 16
    Top = 48
    Width = 248
    Height = 143
    Caption = 
      'Dxbx - Cxbx Delphi Port.'#13#10'Developers: Shadow_tj, Pcucho, Zomby a' +
      'nd Wayo'#13#10#13#10'This project is here to let the world see that Delphi' +
      ' is '#13#10'not dead, and we are still out there.'#13#10'And for those who w' +
      'ant to play commercial games....'#13#10'Just buy an xbox.'#13#10#13#10'SPECIAL T' +
      'HANKS TO: '#13#10'ChecKeR for C++ translation to delphi with pointers ' +
      #13#10'and other things'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
end
