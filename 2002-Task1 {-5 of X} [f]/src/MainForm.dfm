object Form1: TForm1
  Left = 547
  Top = 352
  Width = 659
  Height = 523
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 651
    Height = 477
    Align = alClient
    OnMouseUp = Image1MouseUp
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 8
    object File1: TMenuItem
      Caption = '&File'
      ShortCut = 16467
      object Save1: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = Save1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Fractal1: TMenuItem
      Caption = 'F&ractal'
      object Unzoom1: TMenuItem
        Caption = '&Unzoom'
        OnClick = Unzoom1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Mandelbrot1: TMenuItem
        Caption = '&Mandelbrot'
        OnClick = Mandelbrot1Click
      end
      object Newton1: TMenuItem
        Caption = '&Newton'
        OnClick = Newton1Click
      end
      object Advanced1: TMenuItem
        Caption = '&Advanced'
        OnClick = Advanced1Click
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'bmp'
    Filter = '*.bmp|Bitmaps'
    InitialDir = '.'
    Options = [ofPathMustExist, ofEnableSizing]
    Left = 48
    Top = 8
  end
end
