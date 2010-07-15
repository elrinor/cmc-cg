object Form1: TForm1
  Left = 256
  Top = 102
  Width = 964
  Height = 741
  Caption = 'ElephantFinder'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 113
    Height = 714
    Align = alLeft
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 4
      Top = 77
      Width = 105
      Height = 113
      Caption = 'Progress'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object CGauge1: TCGauge
        Left = 4
        Top = 12
        Width = 97
        Height = 97
        Color = clBtnFace
        Kind = gkPie
        BorderStyle = bsNone
        BackColor = clCream
        ParentColor = False
      end
    end
    object GroupBox2: TGroupBox
      Left = 4
      Top = 1
      Width = 105
      Height = 77
      Caption = 'File Operations'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object LoadBtn: TBitBtn
        Left = 4
        Top = 16
        Width = 97
        Height = 25
        Caption = 'Open...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = OpenBtnClick
      end
      object SaveBtn: TBitBtn
        Left = 4
        Top = 48
        Width = 97
        Height = 25
        Caption = 'Save As...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = SaveBtnClick
      end
    end
    object GroupBox3: TGroupBox
      Left = 4
      Top = 190
      Width = 105
      Height = 173
      Caption = 'Binary Filters'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object BinBtn: TBitBtn
        Left = 4
        Top = 16
        Width = 97
        Height = 25
        Caption = 'Binarization'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = BinBtnClick
      end
      object DilBtn: TBitBtn
        Left = 4
        Top = 48
        Width = 97
        Height = 25
        Caption = 'Dilation'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = BitBtn2Click
      end
      object EroBtn: TBitBtn
        Left = 4
        Top = 80
        Width = 97
        Height = 25
        Caption = 'Erosion'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = BitBtn3Click
      end
      object OpenBtn: TBitBtn
        Left = 4
        Top = 144
        Width = 97
        Height = 25
        Caption = 'Opening'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = BitBtn4Click
      end
      object ClosBtn: TBitBtn
        Left = 4
        Top = 112
        Width = 97
        Height = 25
        Caption = 'Closing'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = BitBtn5Click
      end
    end
    object GroupBox4: TGroupBox
      Left = 4
      Top = 363
      Width = 105
      Height = 213
      Caption = 'Recognition'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      object Label1: TLabel
        Left = 4
        Top = 12
        Width = 97
        Height = 17
        AutoSize = False
        Caption = 'RGB Image:'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object Label2: TLabel
        Left = 4
        Top = 45
        Width = 97
        Height = 17
        AutoSize = False
        Caption = 'Binary Image:'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object Bevel1: TBevel
        Left = 1
        Top = 83
        Width = 102
        Height = 2
        Shape = bsTopLine
      end
      object RGBImage: TEdit
        Left = 4
        Top = 25
        Width = 97
        Height = 19
        Color = clCream
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 0
      end
      object BinImage: TEdit
        Left = 4
        Top = 61
        Width = 97
        Height = 19
        Color = clCream
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 1
      end
      object RGBButton: TStaticText
        Left = 84
        Top = 14
        Width = 17
        Height = 12
        AutoSize = False
        BorderStyle = sbsSingle
        Caption = ' ? '
        Color = 5987327
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        TabOrder = 2
        OnClick = RGBButtonClick
      end
      object BinButton: TStaticText
        Left = 84
        Top = 50
        Width = 17
        Height = 12
        AutoSize = False
        BorderStyle = sbsSingle
        Caption = ' ? '
        Color = 5987327
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        TabOrder = 3
        OnClick = BinButtonClick
      end
      object RecognBtn: TBitBtn
        Left = 4
        Top = 88
        Width = 97
        Height = 25
        Caption = 'Recognition'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = RecognBtnClick
      end
      object SmpBtn: TBitBtn
        Left = 4
        Top = 120
        Width = 97
        Height = 25
        Caption = 'Simple && Medium 4'
        TabOrder = 5
        OnClick = SmpBtnClick
      end
      object Med12Btn: TBitBtn
        Left = 4
        Top = 152
        Width = 97
        Height = 25
        Caption = 'Medium 1 && 2'
        TabOrder = 6
        OnClick = Med12BtnClick
      end
      object Med3Btn: TBitBtn
        Left = 4
        Top = 184
        Width = 97
        Height = 25
        Caption = 'Medium 3'
        TabOrder = 7
        OnClick = Med3BtnClick
      end
    end
    object GroupBox5: TGroupBox
      Left = 4
      Top = 576
      Width = 105
      Height = 46
      Caption = 'Additional Filters'
      TabOrder = 4
      object ContBtn: TBitBtn
        Left = 4
        Top = 16
        Width = 97
        Height = 25
        Caption = 'Contrast'
        TabOrder = 0
        OnClick = ContBtnClick
      end
    end
  end
  object Panel2: TPanel
    Left = 113
    Top = 0
    Width = 843
    Height = 714
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 843
      Height = 25
      Align = alTop
      TabOrder = 0
      object CheckBox1: TCheckBox
        Left = 4
        Top = 4
        Width = 197
        Height = 17
        Caption = 'Display filter results in a new page'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        TabOrder = 0
        OnClick = CheckBox1Click
      end
    end
    object TabControl: TTabControl
      Left = 0
      Top = 25
      Width = 843
      Height = 689
      Align = alClient
      HotTrack = True
      MultiLine = True
      PopupMenu = PopupMenu1
      TabOrder = 1
      Tabs.Strings = (
        '1'
        '2'
        '3')
      TabIndex = 0
      OnChange = TabControlChange
      OnContextPopup = FrameContextPopup
      inline Frame: TFrame1
        Left = 4
        Top = 24
        Width = 835
        Height = 661
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object OpenDialog: TOpenPictureDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 160
    Top = 40
  end
  object SaveDialog: TSavePictureDialog
    Left = 128
    Top = 40
  end
  object PopupMenu1: TPopupMenu
    Left = 192
    Top = 40
    object CloseTab1: TMenuItem
      Caption = 'Close Page'
      ShortCut = 16471
      OnClick = CloseTab1Click
    end
    object CopyPage1: TMenuItem
      Caption = 'Copy Page'
      ShortCut = 16462
      OnClick = CopyPage1Click
    end
    object NextTab1: TMenuItem
      Caption = 'Next Tab'
      ShortCut = 16393
      Visible = False
      OnClick = NextTab1Click
    end
  end
end
