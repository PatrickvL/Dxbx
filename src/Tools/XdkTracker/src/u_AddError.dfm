object frm_AddError: Tfrm_AddError
  Left = 232
  Top = 193
  Width = 555
  Height = 441
  Caption = 'Game Errors'
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    547
    414)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 538
    Height = 373
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object lbl_KnownErros: TLabel
    Left = 12
    Top = 12
    Width = 66
    Height = 13
    Caption = 'Known Errors:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lbl_ErrorNumber: TLabel
    Left = 12
    Top = 116
    Width = 150
    Height = 13
    Caption = 'Error number in xxx@yyy format:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lbl_When: TLabel
    Left = 12
    Top = 160
    Width = 95
    Height = 13
    Caption = 'When does it crash:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lbl_cxbxlog: TLabel
    Left = 12
    Top = 224
    Width = 96
    Height = 13
    Caption = 'Cxbx debug logging:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lbl_krnllog: TLabel
    Left = 12
    Top = 268
    Width = 103
    Height = 13
    Caption = 'Kernel debug logging:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lbl_WhenDesc: TLabel
    Left = 328
    Top = 16
    Width = 116
    Height = 13
    Caption = 'When does this happen:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 12
    Top = 348
    Width = 136
    Height = 13
    Caption = 'Date of SourceForge Source'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Bevel2: TBevel
    Left = 12
    Top = 312
    Width = 313
    Height = 50
    Shape = bsTopLine
  end
  object lst_Errors: TListBox
    Left = 12
    Top = 28
    Width = 309
    Height = 81
    ItemHeight = 13
    TabOrder = 0
    OnClick = lst_ErrorsClick
  end
  object edt_ErrorNumber: TEdit
    Left = 12
    Top = 132
    Width = 309
    Height = 21
    TabOrder = 1
  end
  object mem_Description: TMemo
    Left = 12
    Top = 176
    Width = 309
    Height = 41
    MaxLength = 200
    TabOrder = 2
  end
  object edt_cxbxlog: TEdit
    Left = 12
    Top = 240
    Width = 281
    Height = 21
    TabOrder = 3
  end
  object edt_krnllog: TEdit
    Left = 12
    Top = 284
    Width = 281
    Height = 21
    TabOrder = 4
  end
  object btn_cxbxlog: TButton
    Left = 300
    Top = 240
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 5
    OnClick = btn_cxbxlogClick
  end
  object btn_krnllog: TButton
    Left = 300
    Top = 284
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 6
    OnClick = btn_krnllogClick
  end
  object mem_ErrorDesc: TMemo
    Left = 328
    Top = 28
    Width = 205
    Height = 341
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clWhite
    ReadOnly = True
    TabOrder = 7
  end
  object btn_Cancel: TButton
    Left = 466
    Top = 384
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object btn_Ok: TButton
    Left = 386
    Top = 384
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Upload'
    ModalResult = 1
    TabOrder = 9
  end
  object CheckBox1: TCheckBox
    Left = 12
    Top = 324
    Width = 125
    Height = 17
    Caption = 'SourceForge Sources'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
  end
  object DateTimePicker1: TDateTimePicker
    Left = 156
    Top = 344
    Width = 93
    Height = 21
    Date = 38161.369613715300000000
    Time = 38161.369613715300000000
    TabOrder = 11
  end
end
