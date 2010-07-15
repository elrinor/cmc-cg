object Form3: TForm3
  Left = 361
  Top = 411
  BorderStyle = bsDialog
  Caption = 'Enter Square'
  ClientHeight = 21
  ClientWidth = 225
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
    Width = 66
    Height = 13
    Caption = 'Min. Square ='
  end
  object Edit1: TEdit
    Left = 72
    Top = 0
    Width = 153
    Height = 21
    TabOrder = 0
    OnKeyPress = Edit1KeyPress
  end
end
