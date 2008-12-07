object frm_ImportGames: Tfrm_ImportGames
  Left = 392
  Top = 282
  Caption = 'Import Games'
  ClientHeight = 309
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    420
    309)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 412
    Height = 271
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
    ExplicitWidth = 414
    ExplicitHeight = 273
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
  object btn_Cancel: TButton
    Left = 337
    Top = 281
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object btn_Ok: TButton
    Left = 257
    Top = 281
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
    Left = 8
    Top = 72
    Width = 395
    Height = 193
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
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
        Caption = 'LIBCPMT'
        Width = 75
      end
      item
        Caption = 'LIBCMT'
        Width = 75
      end
      item
        Caption = 'LIBC'
        Width = 75
      end
      item
        Caption = 'D3D8'
        Width = 75
      end
      item
        Caption = 'D3DX8'
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
        Caption = 'XVOICE'
        Width = 75
      end
      item
        Caption = 'XMV'
        Width = 75
      end
      item
        Caption = 'XONLINES'
        Width = 75
      end
      item
        Caption = 'UIX'
        Width = 75
      end
      item
        Caption = 'VOICMAIL'
        Width = 75
      end
      item
        Caption = 'XVOCREC'
        Width = 75
      end
      item
        Caption = 'XACTENG'
        Width = 75
      end
      item
        Caption = 'CalcSig'
        Width = 75
      end
      item
        Caption = 'DMUSIC'
        Width = 75
      end>
    RowSelect = True
    PopupMenu = PopupMenu1
    SortType = stBoth
    TabOrder = 3
    ViewStyle = vsReport
    OnColumnClick = lst_ImportColumnClick
  end
  object PopupMenu1: TPopupMenu
    Left = 32
    Top = 184
    object SelectAll1: TMenuItem
      Caption = 'Select &All'
      OnClick = SelectAll1Click
    end
    object SelectInverse1: TMenuItem
      Caption = 'Select &Inverse'
      OnClick = SelectInverse1Click
    end
    object SelectNone1: TMenuItem
      Caption = 'Select &None'
      OnClick = SelectNone1Click
    end
  end
end
