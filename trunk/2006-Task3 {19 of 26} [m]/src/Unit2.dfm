object PasswordDlg: TPasswordDlg
  Left = 316
  Top = 236
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 93
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
  object Password: TEdit
    Left = 8
    Top = 27
    Width = 217
    Height = 21
    TabOrder = 0
  end
  object OKBtn: TButton
    Left = 70
    Top = 59
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 150
    Top = 59
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
