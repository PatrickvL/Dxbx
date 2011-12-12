object frmExploreFileSystem: TfrmExploreFileSystem
  Left = 0
  Top = 0
  Caption = 'frmExploreFileSystem'
  ClientHeight = 482
  ClientWidth = 730
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    730
    482)
  PixelsPerInch = 96
  TextHeight = 13
  object lblVolumeLetter: TLabel
    Left = 280
    Top = 8
    Width = 73
    Height = 13
    Caption = 'lblVolumeLetter'
  end
  object lblFileSystemType: TLabel
    Left = 280
    Top = 32
    Width = 85
    Height = 13
    Caption = 'lblFileSystemType'
  end
  object lblMountPoint: TLabel
    Left = 280
    Top = 56
    Width = 64
    Height = 13
    Caption = 'lblMountPoint'
  end
  object TreeView1: TTreeView
    Left = 8
    Top = 40
    Width = 241
    Height = 434
    Anchors = [akLeft, akTop, akBottom]
    Indent = 19
    TabOrder = 1
    OnChange = TreeView1Change
    OnExpanding = TreeView1Expanding
  end
  object Memo1: TMemo
    Left = 272
    Top = 80
    Width = 441
    Height = 393
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 8
    Width = 241
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = ComboBox1Change
  end
end
