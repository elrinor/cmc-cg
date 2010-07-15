object Form2: TForm2
  Left = 408
  Top = 565
  BorderStyle = bsDialog
  Caption = 'Enter Radius'
  ClientHeight = 20
  ClientWidth = 177
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 3
    Width = 45
    Height = 13
    Caption = 'Radius = '
  end
  object Edit1: TEdit
    Left = 48
    Top = 0
    Width = 129
    Height = 21
    TabOrder = 0
    OnKeyPress = Edit1KeyPress
  end
end
