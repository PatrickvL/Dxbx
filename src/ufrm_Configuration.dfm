object fmConfiguration: TfmConfiguration
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Dxbx - Configuration'
  ClientHeight = 495
  ClientWidth = 729
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    729
    495)
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 8
    Top = 8
    Width = 153
    Height = 441
    Anchors = [akLeft, akTop, akBottom]
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = TreeView1Change
    Items.NodeData = {
      0304000000320000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      0000000000010A43006F006E00740072006F006C006C00650072002800000000
      00000000000000FFFFFFFFFFFFFFFF0000000000000000000000000105560069
      00640065006F00280000000000000000000000FFFFFFFFFFFFFFFF0000000000
      00000000000000010553006F0075006E0064002C0000000000000000000000FF
      FFFFFFFFFFFFFF00000000000000000000000001074C006F006700670069006E
      006700}
  end
  object btnOk: TButton
    Left = 557
    Top = 462
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    TabOrder = 3
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 646
    Top = 462
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = btnCancelClick
  end
  object btnApply: TButton
    Left = 466
    Top = 462
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Enabled = False
    TabOrder = 2
    OnClick = btnApplyClick
  end
  object PageControl1: TPageControl
    Left = 167
    Top = 8
    Width = 554
    Height = 441
    ActivePage = TabSheet4
    Anchors = [akLeft, akTop, akRight, akBottom]
    Style = tsFlatButtons
    TabOrder = 1
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'Controller'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 546
        Height = 410
        Align = alClient
        Center = True
        ExplicitLeft = -4
        ExplicitWidth = 538
        ExplicitHeight = 372
      end
      object btn_X: TPanel
        Left = 455
        Top = 81
        Width = 33
        Height = 25
        Caption = 'X'
        TabOrder = 10
      end
      object btn_Y: TPanel
        Left = 480
        Top = 50
        Width = 33
        Height = 25
        Caption = 'Y'
        TabOrder = 11
      end
      object btn_A: TPanel
        Left = 478
        Top = 112
        Width = 33
        Height = 25
        Caption = 'A'
        TabOrder = 8
      end
      object btn_B: TPanel
        Left = 502
        Top = 81
        Width = 33
        Height = 25
        Caption = 'B'
        TabOrder = 9
      end
      object btn_White: TPanel
        Left = 502
        Top = 220
        Width = 33
        Height = 25
        Caption = '<'
        TabOrder = 12
      end
      object btn_Black: TPanel
        Left = 502
        Top = 189
        Width = 33
        Height = 25
        Caption = '>'
        TabOrder = 13
      end
      object btn_LeftTrigger: TPanel
        Left = 211
        Top = 72
        Width = 33
        Height = 25
        Caption = 'LShft'
        TabOrder = 6
      end
      object btn_RightTrigger: TPanel
        Left = 297
        Top = 72
        Width = 33
        Height = 25
        Caption = 'RShft'
        TabOrder = 7
      end
      object btn_DPadUp: TPanel
        Left = 172
        Top = 328
        Width = 33
        Height = 25
        Caption = 'Up'
        TabOrder = 2
      end
      object btn_DPadDown: TPanel
        Left = 172
        Top = 359
        Width = 33
        Height = 25
        Caption = 'Down'
        TabOrder = 3
      end
      object btn_DPadRight: TPanel
        Left = 211
        Top = 359
        Width = 33
        Height = 25
        Caption = 'Right'
        TabOrder = 5
      end
      object btn_DPadLeft: TPanel
        Left = 133
        Top = 359
        Width = 33
        Height = 25
        Caption = 'Left'
        TabOrder = 4
      end
      object btn_Back: TPanel
        Left = 3
        Top = 181
        Width = 33
        Height = 25
        Caption = 'Bcksp'
        TabOrder = 1
      end
      object btn_Start: TPanel
        Left = 3
        Top = 212
        Width = 33
        Height = 25
        Caption = 'Space'
        TabOrder = 0
      end
      object btn_LeftThumb: TPanel
        Left = 33
        Top = 41
        Width = 33
        Height = 25
        Caption = 'LCtrl'
        TabOrder = 18
      end
      object btn_RightThumb: TPanel
        Left = 297
        Top = 328
        Width = 33
        Height = 25
        Caption = 'RCtrl'
        TabOrder = 23
      end
      object btn_LeftUp: TPanel
        Left = 72
        Top = 41
        Width = 33
        Height = 25
        Caption = 'E'
        TabOrder = 14
      end
      object btn_LeftDown: TPanel
        Left = 72
        Top = 72
        Width = 33
        Height = 25
        Caption = 'D'
        TabOrder = 15
      end
      object btn_RightUp: TPanel
        Left = 336
        Top = 328
        Width = 33
        Height = 25
        Caption = 'I'
        TabOrder = 19
      end
      object btn_RightDown: TPanel
        Left = 336
        Top = 359
        Width = 33
        Height = 25
        Caption = 'K'
        TabOrder = 20
      end
      object btn_RightRight: TPanel
        Left = 375
        Top = 359
        Width = 33
        Height = 25
        Caption = 'L'
        TabOrder = 22
      end
      object btn_RightLeft: TPanel
        Left = 297
        Top = 359
        Width = 33
        Height = 25
        Caption = 'J'
        TabOrder = 21
      end
      object btn_LeftLeft: TPanel
        Left = 33
        Top = 72
        Width = 33
        Height = 25
        Caption = 'S'
        TabOrder = 16
      end
      object btn_LeftRight: TPanel
        Left = 111
        Top = 72
        Width = 33
        Height = 25
        Caption = 'F'
        TabOrder = 17
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Video'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        546
        410)
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 434
        Height = 166
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Direct3D Configuration'
        TabOrder = 0
        object lbl_DisplayAdapter: TLabel
          Left = 8
          Top = 24
          Width = 110
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Display Adapter:'
        end
        object lbl_Direct3DDevice: TLabel
          Left = 8
          Top = 52
          Width = 110
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Direct3D Device:'
        end
        object lbl_VideoResolution: TLabel
          Left = 8
          Top = 80
          Width = 110
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Video Resolution:'
        end
        object lbl_OtherOptions: TLabel
          Left = 8
          Top = 112
          Width = 110
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Other Options:'
        end
        object edt_DisplayAdapter: TComboBox
          Left = 128
          Top = 21
          Width = 297
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = ChangeClick
        end
        object edt_Direct3dDevice: TComboBox
          Left = 128
          Top = 48
          Width = 297
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 1
          Text = 'Direct3D HAL (Hardware Accelerated)'
          OnChange = ChangeClick
          Items.Strings = (
            'Direct3D HAL (Hardware Accelerated)'
            'Direct3D REF (Software)')
        end
        object edt_VideoResolution: TComboBox
          Left = 128
          Top = 76
          Width = 297
          Height = 21
          Style = csDropDownList
          TabOrder = 2
          OnChange = ChangeClick
        end
        object chk_FullScreen: TCheckBox
          Left = 128
          Top = 108
          Width = 109
          Height = 17
          Caption = 'Start in FullScreen'
          TabOrder = 3
          OnClick = ChangeClick
        end
        object chk_VSync: TCheckBox
          Left = 312
          Top = 108
          Width = 97
          Height = 17
          Caption = 'Force VSync'
          TabOrder = 4
          OnClick = ChangeClick
        end
        object chk_HardwareYUV: TCheckBox
          Left = 128
          Top = 131
          Width = 297
          Height = 17
          Caption = 'Enable Hardware YUV Overlays'
          TabOrder = 5
          OnClick = ChangeClick
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Sound'
      ImageIndex = 3
      DesignSize = (
        546
        410)
      object GroupBox3: TGroupBox
        Left = 0
        Top = 0
        Width = 434
        Height = 166
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Direct Sound Configuration'
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 24
          Width = 110
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Audio Adapter:'
        end
        object Label4: TLabel
          Left = 8
          Top = 64
          Width = 110
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Other Options:'
        end
        object edt_AudioAdapter: TComboBox
          Left = 128
          Top = 21
          Width = 297
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = ChangeClick
        end
        object chkMute: TCheckBox
          Left = 128
          Top = 60
          Width = 109
          Height = 17
          Caption = 'Mute sound'
          TabOrder = 1
          OnClick = ChangeClick
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Logging'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox2: TGroupBox
        Left = 3
        Top = 0
        Width = 542
        Height = 409
        Caption = 'Logging configuration '
        TabOrder = 0
        object lstLogging: TListView
          Left = 16
          Top = 24
          Width = 433
          Height = 369
          Columns = <
            item
              Caption = 'Name'
              Width = 204
            end
            item
              Caption = 'Enabled'
              Width = 75
            end
            item
              Caption = 'Ignore'
              Width = 75
            end
            item
              AutoSize = True
              Caption = 'Disabled'
            end>
          LargeImages = ImageList1
          ReadOnly = True
          RowSelect = True
          SmallImages = ImageList1
          TabOrder = 0
          ViewStyle = vsReport
          OnClick = lstLoggingClick
          OnDrawItem = lstLoggingDrawItem
        end
        object btnLoadLogConfig: TButton
          Left = 455
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Load Config'
          TabOrder = 1
          OnClick = btnLoadLogConfigClick
        end
        object btnSaveLogConfig: TButton
          Left = 455
          Top = 55
          Width = 75
          Height = 25
          Caption = 'Save Config'
          TabOrder = 2
          OnClick = btnSaveLogConfigClick
        end
        object btnDisableAll: TButton
          Left = 455
          Top = 112
          Width = 75
          Height = 25
          Caption = '&Disable All'
          TabOrder = 3
          OnClick = btnDisableAllClick
        end
        object btnEnableAll: TButton
          Left = 455
          Top = 143
          Width = 75
          Height = 25
          Caption = '&Enable All'
          TabOrder = 4
          OnClick = btnEnableAllClick
        end
      end
    end
  end
  object ImageList1: TImageList
    Left = 32
    Top = 220
    Bitmap = {
      494C010103000500040010001000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FDFFFF00FEFEFE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E5E0EF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FDFEFA0000000000FDFEFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FEFEFE00002F0000FDFFFE00FEFEFE000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FC00CFD7E40001007400F4F0F600FDFFFB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000001030300001A00000005000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FEFEFE00000000000040050000060100FDFDFD000000
      00000000000000000000000000000000000000000000FFFBFF00FCFFFA000000
      8D001F17A0000029CC000018C600FFFCFF000000000000000000000000000000
      0000FAFEF800FFFFF70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFBFF00000304000003000036955200078A000095A19B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FAFFFE00055C22000032000000460A00FFFDFE000000
      000000000000000000000000000000000000FEFFFD00FFFFFE000006AB00153E
      DA001D3DD2001B41DB001F41D400221C7B00FEFEFE0000000000000000000000
      0000FFFEFF00FFFBFD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FDFFFE00FFFFFE00F6FEFD00FFFC
      FF00002C0600FFF2FF000671140003781B000089190027C13A00000000000000
      000000000000000000000000000000000000F8FFFF00FFFFFE00FCFFFD00FFFF
      FE0000000000FEFEFE0000360200FFFEFF00004A0C0001591700FFFCFF000000
      000000000000000000000000000000000000FCFBFF007994EB000F43CB001D44
      D6001F44D6001F44D6001F44D6000016CE009497C300F6FDF600FFFFFC000000
      6700181583000E117D00FAFEF900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FCFAFF00FFFEFF00011A00000731
      0100052C0000065F1600026F13000076140000891A00008F22007D657D000000
      0000000000000000000000000000000000003E654C0003240100001801000001
      01000200000003010000FFFFFB00004D1300025D110001681300FFFBFF000000
      000000000000000000000000000000000000FEFFFD00FBFEFF001A43D5001F44
      D6001F44D6001F44D6001F44D6002944D400FFFDFF0007057C001B147D000000
      65008192F9002246DA005477DF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F8FEFD00006D2800F5FFFB00013D
      0D00008A000000640F00006F170003791A00008A1A0002951B00009C23002E2C
      2B0000000000000000000000000000000000FAFBFF00FAFEFF00FFFFFE00FBFF
      FF00F4FFF400FFFCFE000D580C00035E1200006C140000761100003900007DA0
      7E00FDFFFE00FBFDFE00FDFDFD00FDFEFC0000000000FEF7FF00000BC7003353
      D0002141DA001F44D6001F44D6001F44D400153AD80000006C00F0FEFA001640
      D9001D42D4002243D8000923CD00FEFFFD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000043020000270000058500000081
      0000007C000007810000016E180000781A000089190000951D0001A11F002CAF
      4000384C3900000000000000000000000000FFFFFC0000930000027C0000017D
      0200018000000973080000641600006F15000078160000841900008B00000029
      120000330000003A02009BA39C00FEFAFF00FFFFFE00FFFAFF00FDFCFE000F3B
      CC006076E0001E45D7001C44D3002044D8002045D7001E43D5001F44D6001D44
      D6001F44D6001F44D6001841D300FFFDFE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000500010004830200017F0100017F
      0100027D0000008102000484000015860D000085170000932000009F1D002EAF
      46005EE5790000000000000000000000000000000000FFFEFF0000840000097C
      0700007E00000480040021951A00007F190000901E0001971F0000A21E0008B1
      250008BB230000BA000000000000FEFEFE00FDF8FF00FCFFF800020060001617
      79004A6AD300637CDC000F3AD7001D42D4001D42D4001F44D6001F44D6001F44
      D6001F44D6001D44D6001B43D7006C8DEA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFC00002C0600007D0000027D
      07000080000000370200FDFFFC003A934D0044A33B00008F150000A718002DA8
      46002FB74700B6EAD300000000000000000000000000FFFDFF00FBFFFF000483
      0200017D0100188818002E93290047A14100049B1C0001A5210000B3220000C1
      290000CA260002D92A009EF6B400FFFDFF00FBFFFC000A0DB500FFFCFF000410
      CE005574D7006078D8006A80D800002BD4001943D8002643DA001E3FD5002446
      D5001845D2000013CE00FFFEFF00FEFEFE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F8FDFC00FFFDFF00012B14000055
      1100FFFFF800F7FEFF00FFFEFF00F8FBFF00015905005FB365000EA1210023AF
      3D002DB9470024C148002C2B2D00000000000000000000000000FDFEFA00FAFD
      FF0013891200299328003D9D3F0050A5500073AF690000B4140005BB310001D2
      2E0000D20000FFFFFB00FEFEFE00000000006163C2000019CC003C5ED9004463
      D6004973DE005F78DA00697FD7007588D3003C5ED9002244D7001D40D600294C
      CC00FAF9FD00F6FFFF00FFFBFD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFE
      FF0000000000000000000000000000000000FCFEF8000775000090CA9000C3D5
      C40029B445002CC746002BCD4B00000000000000000000000000FAFFFF00054E
      000029922900419F400056A8550061B263007CBD780088C58D0000CE0A00FCF8
      FF00FEF9FA00FFFFFC000000000000000000EAE9ED00294CDC002F56D000415F
      D6004D6AD6005A79D6006B7DD6007684D6008C93E200B3BFDB001E43D900FFFE
      FE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFEFF00FBFF
      F90000000000000000000000000000000000FFFFFE00FCFBFD00007E0000C9DA
      BF00C9DFCD004CC77100B7E0C000000000000000000000000000FFFEFF002095
      28003C9A3C0055A153005CAE610083B6780094C19000A0D29C00E3D7D300FEFE
      FE000000000000000000000000000000000000000000FAFFFC003353DA003D60
      D5005773D8000213C700FDFFFB00969FDE008894DC008F9CE000A0AAE0000713
      CD00F9FCFA00FDFFF90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FAFBF900FAFFFE0002AA
      210006A50F000000000000000000000000000000000000000000FFFBFF0058A0
      4D000FA119004ED06500000000000000000097D29F00ACD9B80056DD6900FEFE
      FE000000000000000000000000000000000000000000FFFDFF00D6DEFB000C30
      D800FFFEFF00FFFCFF00F7FFFF00FBFFFE0099A8D90092A4DF00798AE100AAC1
      F100FEFFFD00FFFFFE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F9FBFF00FFFC
      FF00FAFFFD00F9FEFC0000000000FEFEFE00FFFDFF0080F59A000CDA16000000
      0000000000000000000000000000000000000000000000000000FEFEFE00FEFE
      FE00000000000000000000000000F8FFFE000008D200FFFEFC00FFFFFE00FFFF
      FE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEFEFE00FDFDFD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFAFF00FBFFFF00FCFDF900FEFEFE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFF3FFDFF0000FD7FFE1FE0FF0000
      FC7FFD1F80F30000F03FFC1F00730000003F081F00010000001F001F00010000
      000F000080000000000700000000000000078002000000000003800000000000
      0001C00100010000EF01C003000F0000CF01C00F80030000FF87C30F80030000
      FFFFC21FCE0F0000FFFFFF9FFE1F0000}
  end
  object OpenDialog1: TOpenDialog
    Left = 36
    Top = 100
  end
  object SaveDialog1: TSaveDialog
    Left = 36
    Top = 157
  end
end
