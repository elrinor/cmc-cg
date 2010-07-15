object Form3: TForm3
  Left = 357
  Top = 257
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Processing...'
  ClientHeight = 57
  ClientWidth = 249
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
    Top = 8
    Width = 249
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Please Wait...'
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 32
    Width = 233
    Height = 17
    Min = 0
    Max = 100
    Position = 50
    Smooth = True
    TabOrder = 0
  end
end
