inherited FrameSvnStatusListView: TFrameSvnStatusListView
  Width = 441
  Height = 314
  inline FrameSvnListView: TFrameSvnListView
    Left = 0
    Top = 0
    Width = 441
    Height = 314
    Align = alClient
    TabOrder = 0
    TabStop = True
    inherited Tree: TVirtualStringTree
      Width = 441
      Height = 314
      Cursor = crHourGlass
      TreeOptions.PaintOptions = [toHideFocusRect, toShowDropmark, toThemeAware, toUseBlendedImages]
      OnFreeNode = FrameSvnListViewTreeFreeNode
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
          Position = 9
          Width = 72
          WideText = 'Remote Text Status'
        end
        item
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
          Position = 15
          WideText = 'Head Revision'
        end
        item
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
          Position = 16
          Width = 80
          WideText = 'Head Author'
        end
        item
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
          Position = 17
          Width = 140
          WideText = 'Head Time'
        end
        item
          Position = 18
          Width = 80
          WideText = 'Lock Owner'
        end
        item
          Position = 19
          Width = 160
          WideText = 'Lock Comment'
        end
        item
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
  end
end
