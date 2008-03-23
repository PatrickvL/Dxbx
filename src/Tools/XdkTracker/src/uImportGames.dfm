object frm_ImportGames: Tfrm_ImportGames
  Left = 392
  Top = 282
  Caption = 'Import Games'
  ClientHeight = 301
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    395
    301)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 393
    Height = 269
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object lbl_Publisher: TLabel
    Left = 12
    Top = 12
    Width = 46
    Height = 13
    Caption = 'Publisher:'
  end
  object lbl_NewGames: TLabel
    Left = 12
    Top = 56
    Width = 61
    Height = 13
    Caption = 'New Games:'
  end
  object lbl_Date: TLabel
    Left = 252
    Top = 12
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object btn_Cancel: TButton
    Left = 320
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object btn_Ok: TButton
    Left = 240
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Ok'
    ModalResult = 1
    TabOrder = 1
  end
  object edt_Publisher: TEdit
    Left = 12
    Top = 28
    Width = 233
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
  end
  object lst_Import: TListView
    Left = 12
    Top = 72
    Width = 377
    Height = 193
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Game Name'
        Width = 250
      end
      item
        Caption = 'XAPILIB'
        Width = 75
      end
      item
        Caption = 'XBOXKRNL'
        Width = 75
      end
      item
        Caption = 'LIBCMT'
        Width = 75
      end
      item
        Caption = 'D3D8'
        Width = 75
      end
      item
        Caption = 'XGRAPHC'
        Width = 75
      end
      item
        Caption = 'DSOUND'
        Width = 75
      end
      item
        Caption = 'XMV'
        Width = 75
      end>
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
  end
  object edt_Date: TEdit
    Left = 252
    Top = 28
    Width = 137
    Height = 21
    Color = clMenu
    ReadOnly = True
    TabOrder = 4
  end
end
