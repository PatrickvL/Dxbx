object frm_ImportGames: Tfrm_ImportGames
  Left = 392
  Top = 282
  Caption = 'Import Games'
  ClientHeight = 312
  ClientWidth = 488
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    488
    312)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl_Publisher: TLabel
    Left = 284
    Top = 20
    Width = 46
    Height = 13
    Caption = 'Publisher:'
  end
  object lbl_NewGames: TLabel
    Left = 8
    Top = 56
    Width = 3
    Height = 13
  end
  object lbl_XDKFilter: TLabel
    Left = 8
    Top = 20
    Width = 50
    Height = 13
    Caption = 'XDK &Filter:'
    FocusControl = cmb_gametype
  end
  object btn_Cancel: TButton
    Left = 405
    Top = 284
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 0
    ExplicitLeft = 337
    ExplicitTop = 281
  end
  object btn_Ok: TButton
    Left = 325
    Top = 284
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Ok'
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 257
    ExplicitTop = 281
  end
  object edt_Publisher: TEdit
    Left = 284
    Top = 36
    Width = 195
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
    ExplicitWidth = 233
  end
  object lst_Import: TListView
    Left = 8
    Top = 72
    Width = 270
    Height = 196
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <>
    RowSelect = True
    PopupMenu = PopupMenu1
    SortType = stBoth
    TabOrder = 3
    ViewStyle = vsReport
    OnColumnClick = lst_ImportColumnClick
    OnEdited = lst_ImportEdited
    OnSelectItem = lst_ImportSelectItem
  end
  object mem_XdkVersions: TMemo
    Left = 284
    Top = 72
    Width = 193
    Height = 196
    Anchors = [akTop, akRight, akBottom]
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 4
  end
  object cmb_gametype: TComboBox
    Left = 8
    Top = 36
    Width = 270
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = cmb_gametypeChange
    Items.Strings = (
      'All XDK Versions')
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
