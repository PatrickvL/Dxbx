object FrameSvnTreeView: TFrameSvnTreeView
  Left = 0
  Top = 0
  Width = 344
  Height = 230
  TabOrder = 0
  TabStop = True
  object Tree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 344
    Height = 230
    Align = alClient
    CheckImageKind = ckDarkTick
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    HintAnimation = hatNone
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    OnAfterCellPaint = TreeAfterCellPaint
    OnCollapsed = TreeCollapsed
    OnCompareNodes = TreeCompareNodes
    OnExpanding = TreeExpanding
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndex = TreeGetImageIndex
    OnGetNodeDataSize = TreeGetNodeDataSize
    OnInitChildren = TreeInitChildren
    OnInitNode = TreeInitNode
    Columns = <>
  end
end
