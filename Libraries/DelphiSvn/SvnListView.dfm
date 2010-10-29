object FrameSvnListView: TFrameSvnListView
  Left = 0
  Top = 0
  Width = 358
  Height = 263
  TabOrder = 0
  TabStop = True
  object Tree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 358
    Height = 263
    Align = alClient
    CheckImageKind = ckDarkTick
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.PopupMenu = PopupActionBar
    HintAnimation = hatNone
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
    OnAfterCellPaint = TreeAfterCellPaint
    OnCompareNodes = TreeCompareNodes
    OnGetText = TreeGetText
    OnGetImageIndex = TreeGetImageIndex
    OnGetNodeDataSize = TreeGetNodeDataSize
    OnHeaderClick = TreeHeaderClick
    OnHeaderDraggedOut = TreeHeaderDraggedOut
    Columns = <
      item
        Position = 0
        Width = 160
        WideText = 'File'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 1
        Width = 160
        WideText = 'Subversion File Name'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 2
        Width = 160
        WideText = 'URL'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 3
        Width = 160
        WideText = 'Repository'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 4
        Width = 60
        WideText = 'Kind'
      end
      item
        Position = 5
        Width = 72
        WideText = 'Text Status'
      end
      item
        Position = 6
        Width = 72
        WideText = 'Property Status'
      end
      item
        Position = 7
        Width = 140
        WideText = 'Text Time'
      end
      item
        Position = 8
        Width = 140
        WideText = 'Property Time'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 9
        Width = 72
        WideText = 'Remote Text Status'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 10
        Width = 72
        WideText = 'Remote Property Status'
      end
      item
        Position = 11
        WideText = 'Base Revision'
      end
      item
        Position = 12
        WideText = 'Committed Revision'
      end
      item
        Position = 13
        Width = 80
        WideText = 'Commit Author'
      end
      item
        Position = 14
        Width = 140
        WideText = 'Commit Time'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 15
        WideText = 'Last Commit Revision'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 16
        Width = 80
        WideText = 'Last Commit Author'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 17
        Width = 140
        WideText = 'Last Commit Time'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 18
        Width = 80
        WideText = 'Lock Owner'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 19
        Width = 160
        WideText = 'Lock Comment'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 20
        Width = 140
        WideText = 'Lock Time'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 21
        Width = 40
        WideText = 'Attributes'
        WideHint = 'Commit Author'
      end>
  end
  object PopupActionBar: TPopupActionBar
    OnPopup = PopupActionBarPopup
    Left = 24
    Top = 40
  end
end
