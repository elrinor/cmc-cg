object Form1: TForm1
  Left = 544
  Top = 172
  Width = 687
  Height = 568
  Caption = 'Image Processing'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 137
    Top = 0
    Width = 542
    Height = 522
    Align = alClient
    TabOrder = 0
    object DocImage: TImage
      Left = 0
      Top = 0
      Width = 137
      Height = 121
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 137
    Height = 522
    Align = alLeft
    TabOrder = 1
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 135
      Height = 22
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 4
        Top = 4
        Width = 38
        Height = 13
        Caption = 'Results:'
      end
    end
    object ListBox1: TListBox
      Left = 1
      Top = 23
      Width = 135
      Height = 498
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 152
    object File1: TMenuItem
      Caption = '&File'
      Hint = 'File related commands'
      object FileOpenItem: TMenuItem
        Caption = '&Open'
        Hint = 'Open|Open a file'
        ImageIndex = 7
        ShortCut = 16463
        OnClick = FileOpen1Execute
      end
      object FileSaveItem: TMenuItem
        Caption = '&Save'
        Hint = 'Save|Save current file'
        ImageIndex = 8
        ShortCut = 16467
        OnClick = FileSave1Execute
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object FileExitItem: TMenuItem
        Caption = 'E&xit'
        Hint = 'Exit|Exit application'
        ShortCut = 27
        OnClick = FileExit1Execute
      end
    end
    object Process1: TMenuItem
      Caption = '&Binary Operations'
      object Binarize1: TMenuItem
        Caption = '&Binarize'
        ShortCut = 32834
        OnClick = Binarize1Click
      end
      object BinaryMedian1: TMenuItem
        Caption = '&Median (Radius = 1)'
        ShortCut = 32845
        OnClick = BinaryMedian1Click
      end
      object Erode1: TMenuItem
        Caption = '&Erode'
        ShortCut = 32837
        OnClick = Erode1Click
      end
      object Dilate1: TMenuItem
        Caption = '&Dilate'
        ShortCut = 32836
        OnClick = Dilate1Click
      end
      object Close1: TMenuItem
        Caption = '&Close'
        ShortCut = 32835
        OnClick = Close1Click
      end
      object Open1: TMenuItem
        Caption = '&Open'
        ShortCut = 32847
        OnClick = Open1Click
      end
      object RemoveSmallObjects1: TMenuItem
        Caption = '&Remove Small Objects'
        ShortCut = 32850
        OnClick = RemoveSmallObjects1Click
      end
    end
    object Processing1: TMenuItem
      Caption = '&Processing'
      object Contrast1: TMenuItem
        Caption = 'Enhance Con&trast'
        ShortCut = 32852
        OnClick = Contrast1Click
      end
      object ScanBinaryImageforElephants1: TMenuItem
        Caption = '&Scan Binary Image for Elephants'
        ShortCut = 32851
        OnClick = ScanBinaryImageforElephants1Click
      end
      object Simple1: TMenuItem
        Caption = 'Simple'
        ShortCut = 32817
        OnClick = Simple1Click
      end
      object Medium1and21: TMenuItem
        Caption = 'Medium 1 and 2'
        ShortCut = 32818
        OnClick = Medium1and21Click
      end
      object Medium3and41: TMenuItem
        Caption = 'Medium 3 and 4'
        ShortCut = 32819
        OnClick = Medium3and41Click
      end
      object Medium51: TMenuItem
        Caption = 'Medium 5'
        ShortCut = 32820
        OnClick = Medium51Click
      end
    end
  end
  object OpenDialog: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 112
    Top = 152
  end
  object SaveDialog: TSavePictureDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoReadOnlyReturn, ofEnableSizing]
    Left = 144
    Top = 152
  end
end
