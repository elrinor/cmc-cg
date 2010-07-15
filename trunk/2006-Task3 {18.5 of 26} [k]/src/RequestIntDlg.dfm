object OKBottomDlg: TOKBottomDlg
  Left = 203
  Top = 169
  BorderIcons = []
  BorderStyle = bsDialog
  ClientHeight = 129
  ClientWidth = 217
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 201
    Height = 81
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 185
    Height = 33
    Alignment = taCenter
    AutoSize = False
    Caption = 'ASDF'
  end
  object OKBtn: TButton
    Left = 31
    Top = 100
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 111
    Top = 100
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 16
    Top = 56
    Width = 185
    Height = 19
    Color = clCream
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
  end
end
