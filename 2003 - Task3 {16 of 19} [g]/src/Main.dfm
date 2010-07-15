object Form1: TForm1
  Left = 285
  Top = 172
  Width = 870
  Height = 640
  Caption = 'Clock'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 862
    Height = 594
    Align = alClient
    TabOrder = 0
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 161
      Height = 209
    end
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 8
    object File1: TMenuItem
      Caption = #1060#1072#1081#1083
      object Open1: TMenuItem
        Caption = #1054#1090#1082#1088#1099#1090#1100
        ShortCut = 16463
        OnClick = Open1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = #1042#1099#1093#1086#1076
        ShortCut = 16472
      end
    end
    object Image2: TMenuItem
      Caption = #1048#1079#1086#1073#1088#1072#1078#1077#1085#1080#1077
      object Binarize1: TMenuItem
        Caption = #1041#1080#1085#1072#1088#1080#1079#1072#1094#1080#1103
        ShortCut = 32834
        OnClick = Binarize1Click
      end
      object N2: TMenuItem
        Caption = #1056#1072#1089#1096#1080#1088#1077#1085#1080#1077
        ShortCut = 32836
        OnClick = N2Click
      end
      object N3: TMenuItem
        Caption = #1057#1078#1072#1090#1080#1077
        ShortCut = 32837
        OnClick = N3Click
      end
      object N4: TMenuItem
        Caption = #1054#1090#1082#1088#1099#1090#1080#1077
        ShortCut = 32847
        OnClick = N4Click
      end
      object N5: TMenuItem
        Caption = #1047#1072#1082#1088#1099#1090#1080#1077
        ShortCut = 32835
        OnClick = N5Click
      end
      object N6: TMenuItem
        Caption = #1052#1077#1076#1080#1072#1085#1085#1072#1103' '#1092#1080#1083#1100#1090#1088#1072#1094#1080#1103
        ShortCut = 32845
        OnClick = N6Click
      end
    end
    object N7: TMenuItem
      Caption = #1056#1072#1089#1087#1086#1079#1085#1072#1074#1072#1085#1080#1077
      object N8: TMenuItem
        Caption = #1041#1080#1085#1072#1088#1085#1086#1075#1086' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103
        ShortCut = 32850
        OnClick = N8Click
      end
      object Simple1: TMenuItem
        Caption = 'Simple && Noisy 1'
        ShortCut = 32817
        OnClick = Simple1Click
      end
      object Noisy21311: TMenuItem
        Caption = 'Noisy 21, 31'
        ShortCut = 32818
        OnClick = Noisy21311Click
      end
      object SimpleNoisy11: TMenuItem
        Caption = 'Noisy 61, 81'
        ShortCut = 32819
        OnClick = SimpleNoisy11Click
      end
      object Noisy1: TMenuItem
        Caption = 'Noisy 52'
        ShortCut = 32820
        OnClick = Noisy1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 40
    Top = 8
  end
end
