object Form1: TForm1
  Left = 324
  Top = 287
  BorderStyle = bsSingle
  Caption = 'Color Adjuster'
  ClientHeight = 505
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 385
    Height = 249
    Caption = 'Target Image (Click to open)'
    TabOrder = 0
    object Image1: TImage
      Left = 8
      Top = 16
      Width = 369
      Height = 225
      OnClick = Image1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 392
    Top = 0
    Width = 385
    Height = 249
    Caption = 'Color Source  (Click to open)'
    TabOrder = 1
    object Image2: TImage
      Left = 8
      Top = 16
      Width = 369
      Height = 225
      OnClick = Image2Clic
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 256
    Width = 601
    Height = 249
    Caption = 'Result  (Click to save)'
    TabOrder = 2
    object Image3: TImage
      Left = 8
      Top = 16
      Width = 585
      Height = 225
      OnClick = Image3Click
    end
  end
  object GroupBox4: TGroupBox
    Left = 608
    Top = 256
    Width = 169
    Height = 249
    Caption = 'Settings'
    TabOrder = 3
    object Label1: TLabel
      Left = 8
      Top = 40
      Width = 73
      Height = 13
      Caption = 'Modify Channel'
    end
    object Label2: TLabel
      Left = 8
      Top = 64
      Width = 75
      Height = 13
      Caption = 'Contrast Affects'
    end
    object Label3: TLabel
      Left = 96
      Top = 16
      Width = 6
      Height = 13
      Caption = 'L'
    end
    object Label4: TLabel
      Left = 120
      Top = 16
      Width = 7
      Height = 13
      Caption = 'A'
    end
    object Label5: TLabel
      Left = 144
      Top = 16
      Width = 7
      Height = 13
      Caption = 'B'
    end
    object CheckBox1: TCheckBox
      Left = 96
      Top = 32
      Width = 17
      Height = 25
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 120
      Top = 32
      Width = 17
      Height = 25
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 144
      Top = 32
      Width = 17
      Height = 25
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox3Click
    end
  end
  object CheckBox4: TCheckBox
    Left = 704
    Top = 312
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CheckBox4Click
  end
  object CheckBox5: TCheckBox
    Left = 728
    Top = 312
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox5Click
  end
  object CheckBox6: TCheckBox
    Left = 752
    Top = 312
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = CheckBox6Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'All supported types|*.jpg; *.jpeg; *.bmp; *.gif; *.ico|JPEG|*.jp' +
      'g; *.jpeg|BMP|*.bmp|GIF|*.gif|ICO|*.ico'
    Left = 16
    Top = 272
  end
  object SaveDialog1: TSaveDialog
    Filter = 'BMP|*.bmp'
    Left = 48
    Top = 272
  end
end
