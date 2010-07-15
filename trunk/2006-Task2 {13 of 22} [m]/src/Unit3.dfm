object PasswordDlg1: TPasswordDlg1
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 142
  ClientWidth = 233
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 9
    Width = 217
    Height = 16
    AutoSize = False
    Caption = ':'
  end
  object Label2: TLabel
    Left = 8
    Top = 57
    Width = 217
    Height = 16
    AutoSize = False
    Caption = ':'
    WordWrap = True
  end
  object Password: TEdit
    Left = 8
    Top = 27
    Width = 217
    Height = 21
    TabOrder = 0
  end
  object OKBtn: TButton
    Left = 70
    Top = 107
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 150
    Top = 107
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 8
    Top = 75
    Width = 217
    Height = 21
    TabOrder = 1
  end
end
