object Form2: TForm2
  Left = 561
  Top = 196
  Width = 306
  Height = 205
  Caption = 'About'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 132
    BevelOuter = bvLowered
    Color = clWhite
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 48
      Width = 273
      Height = 41
      Alignment = taCenter
      AutoSize = False
      Caption = 'Delphi Image Processing Application'
    end
  end
  object OKButton: TButton
    Left = 120
    Top = 144
    Width = 65
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    IsControl = True
  end
end
