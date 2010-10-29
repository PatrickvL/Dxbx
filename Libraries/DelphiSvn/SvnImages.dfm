object SvnImageModule: TSvnImageModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 237
  Width = 313
  object ShellImagesSmall: TImageList
    ShareImages = True
    Left = 40
    Top = 8
  end
  object ShellImagesLarge: TImageList
    Height = 32
    ShareImages = True
    Width = 32
    Left = 40
    Top = 56
  end
end
