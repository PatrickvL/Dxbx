object frm_Main: Tfrm_Main
  Left = 263
  Top = 194
  Width = 571
  Height = 410
  Caption = 'CXBX XDK Tacker'
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 345
    Width = 563
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 8
    object File1: TMenuItem
      Caption = '&File'
      object Preferences1: TMenuItem
        Caption = '&Preferences'
        OnClick = Preferences1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Viewxdkversion1: TMenuItem
      Caption = 'Xdk versions'
      object Viewxdkversion2: TMenuItem
        Caption = 'View xdk version'
        OnClick = Viewxdkversion2Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Gameerrors1: TMenuItem
        Caption = 'Game errors'
        Enabled = False
        OnClick = Gameerrors1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object VisitShadowTjwebsite1: TMenuItem
        Caption = 'Visit website &Shadow_Tj '
        OnClick = VisitShadowTjwebsite1Click
      end
      object VisitCaustikswebsite1: TMenuItem
        Caption = 'Visit website &Cxbx '
        OnClick = VisitCaustikswebsite1Click
      end
      object VisitCxbxForum1: TMenuItem
        Caption = 'Visit Cxbx &Forum'
        OnClick = VisitCxbxForum1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = '&About...'
        OnClick = About1Click
      end
    end
  end
end
