inherited FrameSvnNotify: TFrameSvnNotify
  Width = 513
  Height = 333
  object Tree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 513
    Height = 333
    Cursor = crHourGlass
    Align = alClient
    CheckImageKind = ckDarkTick
    DefaultNodeHeight = 14
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    HintAnimation = hatNone
    Indent = 14
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toHideFocusRect, toShowDropmark, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
    OnCompareNodes = TreeCompareNodes
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnPaintText = TreePaintText
    OnGetNodeDataSize = TreeGetNodeDataSize
    OnHeaderClick = TreeHeaderClick
    Columns = <
      item
        Position = 0
        Width = 70
        WideText = 'Action'
        WideHint = 'Update Action'
      end
      item
        Position = 1
        Width = 320
        WideText = 'Path'
      end
      item
        Position = 2
        Width = 70
        WideText = 'MIME Type'
      end>
  end
end
