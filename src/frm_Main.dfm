object Form1: TForm1
  Left = 455
  Top = 372
  Width = 521
  Height = 294
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 64
    Top = 60
    object File1: TMenuItem
      Caption = '&File'
      object OpenXbe1: TMenuItem
        Caption = 'Open Xbe'
      end
      object CloseXbe1: TMenuItem
        Caption = 'Close Xbe'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Importexe1: TMenuItem
        Caption = 'Import exe'
      end
      object Exportexe1: TMenuItem
        Caption = 'Export exe'
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object SaveXbe1: TMenuItem
        Caption = 'Save Xbe'
      end
      object SaveXbeas1: TMenuItem
        Caption = 'Save Xbe as..'
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object RecentXbefiles1: TMenuItem
        Caption = 'Recent Xbe files'
        object TMenuItem
        end
      end
      object RecentExefiles1: TMenuItem
        Caption = 'Recent Exe files'
        object TMenuItem
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Undo1: TMenuItem
        Caption = '&Undo'
        ShortCut = 16474
      end
      object Repeatcommand1: TMenuItem
        Caption = '&Repeat <command>'
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Caption = 'Cu&t'
        ShortCut = 16472
      end
      object Copy1: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
      end
      object Paste1: TMenuItem
        Caption = '&Paste'
        ShortCut = 16470
      end
      object PasteSpecial1: TMenuItem
        Caption = 'Paste &Special...'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Find1: TMenuItem
        Caption = '&Find...'
      end
      object Replace1: TMenuItem
        Caption = 'R&eplace...'
      end
      object GoTo1: TMenuItem
        Caption = '&Go To...'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Links1: TMenuItem
        Caption = 'Lin&ks...'
      end
      object Object1: TMenuItem
        Caption = '&Object'
      end
    end
    object View1: TMenuItem
      Caption = '&View'
    end
    object Settings1: TMenuItem
      Caption = '&Settings'
    end
    object Emulation1: TMenuItem
      Caption = '&Emulation'
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Contents1: TMenuItem
        Caption = '&Contents'
      end
      object SearchforHelpOn1: TMenuItem
        Caption = '&Search for Help On...'
      end
      object HowtoUseHelp1: TMenuItem
        Caption = '&How to Use Help'
      end
      object About1: TMenuItem
        Caption = '&About...'
      end
    end
  end
end
