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
      0303000000320000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      0000000000010A43006F006E00740072006F006C006C00650072002800000000
      00000000000000FFFFFFFFFFFFFFFF0000000000000000000000000105560069
      00640065006F002C0000000000000000000000FFFFFFFFFFFFFFFF0000000000
      0000000000000001074C006F006700670069006E006700}
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
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    Style = tsFlatButtons
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
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
      Caption = 'TabSheet2'
      ImageIndex = 1
      DesignSize = (
        546
        410)
      object GroupBox1: TGroupBox
        Left = 3
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
    object TabSheet3: TTabSheet
      Caption = 'TabSheet3'
      ImageIndex = 2
    end
  end
end
