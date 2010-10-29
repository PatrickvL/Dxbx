inherited DockableToolbarForm: TDockableToolbarForm
  Left = 161
  Top = 152
  HorzScrollBar.Increment = 31
  VertScrollBar.Increment = 14
  Caption = 'ToolbarForm'
  ClientHeight = 157
  ClientWidth = 342
  PopupMenu = PopupMenu1
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter [0]
    Left = 0
    Top = 30
    Width = 342
    Height = 3
    Cursor = crSizeNS
    Align = alTop
    MinSize = 20
    ResizeStyle = rsUpdate
    OnCanResize = Splitter1CanResize
    OnMoved = Splitter1Moved
  end
  object ToolBar1: TToolBar [1]
    Left = 0
    Top = 0
    Width = 342
    Height = 30
    BorderWidth = 1
    EdgeBorders = [ebTop, ebBottom]
    Indent = 4
    PopupMenu = ToolbarPopupMenu
    TabOrder = 0
    Wrapable = False
  end
  object ToolbarPopupMenu: TPopupActionBar
    Left = 72
    Top = 72
    object TextLabels1: TMenuItem
      Action = TextLabelsCmd
    end
  end
  object ToolActionList: TActionList
    Left = 40
    Top = 72
    object ToolbarCmd: TAction
      Caption = '&Toolbar'
      Checked = True
      OnExecute = ToolbarCmdExecute
      OnUpdate = ToolbarCmdUpdate
    end
    object TextLabelsCmd: TAction
      Caption = 'Text &Labels'
      OnExecute = TextLabelsCmdExecute
      OnUpdate = TextLabelsCmdUpdate
    end
  end
  object PopupMenu1: TPopupActionBar
    Left = 8
    Top = 72
    object Toolbar2: TMenuItem
      Action = ToolbarCmd
    end
  end
end
