object Form1: TForm1
  Left = 342
  Top = 262
  Width = 571
  Height = 420
  Caption = 'Image DeNoising Application'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 563
    Height = 42
    Align = alTop
    TabOrder = 0
    object SpeedButton1: TSpeedButton
      Left = 48
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Load'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000C0CFD0C0CFD0
        C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CF
        D0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0
        CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0000000000000
        000000000000000000000000000000000000000000000000000000C0CFD0C0CF
        D0C0CFD0C0CFD0C0CFD000000000000000878000878000878000878000878000
        8780008780008780008780000000C0CFD0C0CFD0C0CFD0C0CFD000000000FFFF
        0000000087800087800087800087800087800087800087800087800087800000
        00C0CFD0C0CFD0C0CFD0000000FFFFFF00FFFF00000000878000878000878000
        8780008780008780008780008780008780000000C0CFD0C0CFD000000000FFFF
        FFFFFF00FFFF0000000087800087800087800087800087800087800087800087
        80008780000000C0CFD0000000FFFFFF00FFFFFFFFFF00FFFF00000000000000
        000000000000000000000000000000000000000000000000000000000000FFFF
        FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFF000000C0CFD0C0CF
        D0C0CFD0C0CFD0C0CFD0000000FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFF
        FFFF00FFFFFFFFFF000000C0CFD0C0CFD0C0CFD0C0CFD0C0CFD000000000FFFF
        FFFFFF00FFFF000000000000000000000000000000000000000000C0CFD0C0CF
        D0C0CFD0C0CFD0C0CFD0C0CFD0000000000000000000C0CFD0C0CFD0C0CFD0C0
        CFD0C0CFD0C0CFD0C0CFD0C0CFD0000000000000000000C0CFD0C0CFD0C0CFD0
        C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CF
        D0000000000000C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0
        CFD0000000C0CFD0C0CFD0C0CFD0000000C0CFD0000000C0CFD0C0CFD0C0CFD0
        C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0000000000000000000C0CF
        D0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0
        CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 72
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Save'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000C0CFD0C0CFD0
        C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CF
        D0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD000000000000000000000000000000000
        0000000000000000000000000000000000000000000000C0CFD0C0CFD0000000
        008780008780000000000000000000000000000000000000C0C7C0C0C7C00000
        00008780000000C0CFD0C0CFD000000000878000878000000000000000000000
        0000000000000000C0C7C0C0C7C0000000008780000000C0CFD0C0CFD0000000
        008780008780000000000000000000000000000000000000C0C7C0C0C7C00000
        00008780000000C0CFD0C0CFD000000000878000878000000000000000000000
        0000000000000000000000000000000000008780000000C0CFD0C0CFD0000000
        0087800087800087800087800087800087800087800087800087800087800087
        80008780000000C0CFD0C0CFD000000000878000878000000000000000000000
        0000000000000000000000000000008780008780000000C0CFD0C0CFD0000000
        008780000000C0C7C0C0C7C0C0C7C0C0C7C0C0C7C0C0C7C0C0C7C0C0C7C00000
        00008780000000C0CFD0C0CFD0000000008780000000C0C7C0C0C7C0C0C7C0C0
        C7C0C0C7C0C0C7C0C0C7C0C0C7C0000000008780000000C0CFD0C0CFD0000000
        008780000000C0C7C0C0C7C0C0C7C0C0C7C0C0C7C0C0C7C0C0C7C0C0C7C00000
        00008780000000C0CFD0C0CFD0000000008780000000C0C7C0C0C7C0C0C7C0C0
        C7C0C0C7C0C0C7C0C0C7C0C0C7C0000000008780000000C0CFD0C0CFD0000000
        008780000000C0C7C0C0C7C0C0C7C0C0C7C0C0C7C0C0C7C0C0C7C0C0C7C00000
        00000000000000C0CFD0C0CFD0000000008780000000C0C7C0C0C7C0C0C7C0C0
        C7C0C0C7C0C0C7C0C0C7C0C0C7C0000000C0C7C0000000C0CFD0C0CFD0000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0
        CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0C0CFD0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton2Click
    end
    object SpeedButton3: TSpeedButton
      Left = 0
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Set Image 1'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C30E0000C30E00000000000000000000C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000
        4000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C000400000B800C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000400000B80000
        B800C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C000400000B80000B80000B800C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000400000B80000B80000B80000
        B800C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        00400000B80000B80000B80000B80000B8000040000040000040000040000040
        00C0C0C0C0C0C0C0C0C0C0C0C000400000B80000B80000B80000B80000B80000
        B80000B80000B80000B80000B80000B80000B800C0C0C0C0C0C0C0C0C000B800
        00B80000B80000B80000B80000B80000B80000B80000B80000B80000B80000B8
        0000B800C0C0C0C0C0C0C0C0C0C0C0C000B80000B80000B80000B80000B80000
        B80000B80000B80000B80000B80000B80000B800C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C000B80000B80000B80000B80000B800C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000B80000B80000B80000
        B800C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C000B80000B80000B800C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000B80000
        B800C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000B800C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton3Click
    end
    object SpeedButton4: TSpeedButton
      Left = 24
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Set Image 2'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0004000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000B800004000C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C000B80000B800004000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000B80000B80000B800004000C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C000B80000B80000B80000B800004000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C000400000400000400000400000400000B80000B80000B80000B80000B8
        00004000C0C0C0C0C0C0C0C0C0C0C0C000B80000B80000B80000B80000B80000
        B80000B80000B80000B80000B80000B80000B800004000C0C0C0C0C0C0C0C0C0
        00B80000B80000B80000B80000B80000B80000B80000B80000B80000B80000B8
        0000B80000B800C0C0C0C0C0C0C0C0C000B80000B80000B80000B80000B80000
        B80000B80000B80000B80000B80000B80000B800C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000B80000B80000B80000B80000B8
        00C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C000B80000B80000B80000B800C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000B80000B80000B800C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C000B80000B800C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000B800C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton4Click
    end
    object SpeedButton5: TSpeedButton
      Left = 112
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Noise'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C30E0000C30E00000000000000000000C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0A098FF8CFFFFFF61FF008F00FFFF5A000000A1
        FF00000000431BFFFFFF02BAFFFFFF000021FFFF00FFB6C0C0C0C0C0C07FE9C6
        00C3FF00FFD66700FFC5FFAEFF00E7FFFF43B8FFF0FFFF0000A705003648FF0A
        E30057B6FFFFFFC0C0C0C0C0C092FFFFFF76008BFF0001FFFE5CAC9D00DF35FF
        3BFF91006000FFB200FF00FF00FF06FFFF8DFF008EFFFFC0C0C0C0C0C0000091
        000DFFFF2AD9FF0000FFFF00B6FFFFC1FFFF68D85DFF00000088FF0000E100FF
        00FFFF7AFF4BFFC0C0C0C0C0C0C79B38007CFFFFFFFF00D4FFD8FF00FF8CBAC8
        11DB0000003A974F00FF00E2E600000000FF00FF00FFFFC0C0C0C0C0C075DA00
        6300FF00FF00E5002600003000FFC70000FFF5FF28000000000000FFFFE39DFF
        DB9600ACFFFF74C0C0C0C0C0C0FFEE1E1DFFD70000C1B5FF0049FF00FFFFD3CF
        5100E9A6FFA0130F15295AFFFFD500C0FFFFE900CF0000C0C0C0C0C0C0FFFFFF
        33229F000000A8FF00FFFFEE00000000FFFF1AFF00B43300000BFFD70000F1FF
        FF009A0068FF00C0C0C0C0C0C000FF0000FFFF00E0FF0004DBE76DFF00FF9100
        AD00D6FFB8FFFF8526FF0000FFBF000E1380FF34FFFFFFC0C0C0C0C0C0FF0088
        FF00006FFF00FFFF08E000F1FFFFEE394AFF00FF8EFFFF00F000000F000000D3
        99387FFFFF006EC0C0C0C0C0C0FFBFB072FF00000000FF05C7FFFF00C1FFFFFF
        000000FF189FFFFF7C0008DBFF0000B1FA00FF00FFEDFFC0C0C0C0C0C0B3FF00
        0000FF00FFFF8CFFFF343B5D8A13FFB5004300FF00D49AD200FFFFFFFFFFFF00
        FF4DFFFF4AF6FFC0C0C0C0C0C0FF648900DB0200FF0049FFFFFFC9003C00EB00
        19FFFFAB8200FF0030FFFFFFFF33FCFF000000C7EEA600C0C0C0C0C0C054FFFF
        FFFFFFFF9ACF00A1FF00FFC7FFFF1FFF0000FFFF0BFFFF9DFFD4FFFFFFFF0036
        1E3DFFFF00FF00C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton5Click
    end
    object SpeedButton6: TSpeedButton
      Left = 136
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Blur 3x3'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C000000005050520202000000000000016161602
        0202000000202020050505000000202020050505000000C0C0C0C0C0C00B0B0B
        1616162E2E2E0B0B0B0B0B0B2525251010100909092D2D2D1616160B0B0B2E2E
        2E1616160B0B0BC0C0C0C0C0C02525252F2F2F4444442525252525255454546C
        6C6C6363636363632A2A2A2525254444442F2F2F252525C0C0C0C0C0C0000000
        0505052020200000000000003E3E3E6B6B6B6060605353530000000000002020
        20050505000000C0C0C0C0C0C00B0B0B1616162A2A2A0404040505054444446E
        6E6E6363635858580808080404042A2A2A1616160B0B0BC0C0C0C0C0C0252525
        2E2E2E6363635555555252527F7F7F9A9A9A9393938B8B8B5757575555556363
        632E2E2E252525C0C0C0C0C0C00000000404046363636262625B5B5B8787879E
        9E9E989898939393646464606060636363040404000000C0C0C0C0C0C0080808
        1111116C6C6C6C6C6C6666668F8F8FA4A4A49E9E9E9A9A9A6E6E6E6B6B6B6C6C
        6C111111080808C0C0C0C0C0C01C1C1C2525255555554242423F3F3F7171718F
        8F8F8787877F7F7F4444444141415454542525251C1C1CC0C0C0C0C0C0000000
        0808081F1F1F0000000000003B3B3B6666665B5B5B4F4F4F0000000000001F1F
        1F080808000000C0C0C0C0C0C00000000707072121210000000000003F3F3F6C
        6C6C616161545454000000000000212121070707000000C0C0C0C0C0C0252525
        2F2F2F4444442525252525255555556C6C6C6363636363632A2A2A2525254444
        442F2F2F252525C0C0C0C0C0C00B0B0B1616162E2E2E0B0B0B0B0B0B25252510
        10100909092D2D2D1616160B0B0B2E2E2E1616160B0B0BC0C0C0C0C0C0000000
        0404041F1F1F0000000000001515150000000000002222220808080000002222
        22080808000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton6Click
    end
    object SpeedButton7: TSpeedButton
      Left = 160
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Sharpen 3x3'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000C0C0C0C0C0BF
        BFBFBFBFBEBFBEBEBEBCBBBDBCBBBBB9BABAB9B9B9B8B7B8B8B9B8B9B9B7BABA
        BABBBCBCBEBDBEC0C0C0BFBFBF0301010807072321210505050709071E1D1C0E
        0F0E0F0F0F29282A151315100E0E2726260F0D0D0A0A0ABDBEBDBFBFC00D0D0D
        1617162D2E2D0D0C0D0D0E0E2322221313130E0F0F2827271819191110102A29
        2B171817131414BCBBBBBEBEBE2727262F2E2D40404023222323232137363728
        28272223223534332526252122223737372C2C292D2C2CBAB9BABFBFBE040505
        0707081F1F1F0606060808091E1F1E101111110E102423230F0E0E0D0D0D1E1F
        1F0E0E0F121111B8B8B7BDBEBE100F101617172827270B0B0C0D0D0C2425241C
        1C1D1718181F1F1F1312130F10102525251918191B1B1AB8B8B7BDBDBE292828
        2C2C2B3B3B3A272728222221ACADACACADACACADACACADAC2325242021213334
        34292A282F2D2EB8B7B7BCBDBD0808090908091F201F0E100F111111ACADACFF
        FFFFFFFFFFACADAC1515160F0F0F2527260C0D0C0F1111B9B7B8BCBCBD0F0F0F
        121312272726191918161616ACADACFFFFFFFFFFFFACADAC1C1C1D1313131F1F
        1E131414121313B9BABBBDBCBD2121212222243A3A3B292928232423ACADACAC
        ADACACADACACADAC282728232222393839242223222122BCBDBBBDBDBD070707
        0A0A0B1E1E1E1010111818172121201211130E0F0E1F201F0E0F0F090709292B
        2B0B0A0A060607BDBCBCBEBEBE0605060A090A2020200607060C0B0C2626261A
        19191211122627271818180F0F0E1F1F1F0A0908050404BFBEBDBEBEBF272727
        2E2E2E3F403F2223232322233A3A3B2B2B2A2425233C3D3C2D2B2C2524234141
        412E2D2F252625BEBFBFBFBFBE0D0E0D1616172D2C2C0C0D0D0D0D0D24232312
        12120D0C0B2B2B2B1616170C0C0C2E2D2D1716160D0B0DBEBFC0C0BFBF030202
        0708072222210605060607061A1A1A0808090807072525250C0C0C0203022423
        2408090A020100C0C0C0C0C0C0C0C0BFBFBFBFBEBFBEBDBEBEBEBDBDBDBCBDBE
        BCBCBDBEBEBFBCBEBEBDBEC0BFBFBFBFBFC0C0C0BFC0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton7Click
    end
    object SpeedButton8: TSpeedButton
      Left = 184
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Find Edges'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000C0C0C0C0C0BF
        BFBFBFBFBEBFBEBEBEBCBBBDBCBBBBB9BABAB9B9B9B8B7B8B8B9B8B9B9B7BABA
        BABBBCBCBEBDBEC0C0C0BFBFBF00000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000BDBEBDBFBFC0000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000BCBBBBBEBEBE000000000000000000FFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000BAB9BABFBFBE000000
        000000FFFFFFFFFFFF000000000000FFFFFFFFFFFF000000000000FFFFFFFFFF
        FF000000000000B8B8B7BDBEBE000000FFFFFFFFFFFF000000000000000000FF
        FFFF000000000000000000000000FFFFFFFFFFFF000000B8B8B7BDBDBE000000
        FFFFFF000000000000000000FFFFFFFFFFFF000000000000000000000000FFFF
        FFFFFFFF000000B8B7B7BCBDBD000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000000000000000000000FFFFFFFFFFFF000000B9B7B8BCBCBD000000
        FFFFFF000000000000000000000000000000000000000000FFFFFFFFFFFFFFFF
        FFFFFFFF000000B9BABBBDBCBD000000FFFFFF00000000000000000000000000
        0000000000FFFFFFFFFFFF000000000000FFFFFF000000BCBDBBBDBDBD000000
        FFFFFFFFFFFF000000000000000000000000000000FFFFFF0000000000000000
        00FFFFFF000000BDBCBCBEBEBE000000000000FFFFFFFFFFFF00000000000000
        0000000000FFFFFF000000000000FFFFFFFFFFFF000000BFBEBDBEBEBF000000
        000000000000FFFFFFFFFFFF000000000000000000FFFFFF000000FFFFFFFFFF
        FF000000000000BEBFBFBFBFBE000000000000000000000000FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000BEBFC0C0BFBF000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000C0C0C0C0C0C0C0C0BFBFBFBFBEBFBEBDBEBEBEBDBDBDBCBDBE
        BCBCBDBEBEBFBCBEBEBDBEC0BFBFBFBFBFC0C0C0BFC0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton8Click
    end
    object SpeedButton9: TSpeedButton
      Left = 208
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Gaussian Blur'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000C0C0C0C0C0BF
        BFBFBFBFBEBFBEBEBEBCBBBDBCBBBBB9BABAB9B9B9B8B7B8B8B9B8B9B9B7BABA
        BABBBCBCBEBDBEC0C0C0BFBFBF00000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000BDBEBDBFBFC0000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000BCBBBBBEBEBE00000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000BAB9BABFBFBEFFFFFF
        FFFFFFFFFFFF000000000000000000000000000000000000000000000000FFFF
        FFFFFFFFFFFFFFB8B8B7BDBEBE000000000000000000FFFFFF00000000000000
        0000000000000000000000FFFFFF000000000000000000B8B8B7BDBDBE000000
        000000000000FFFFFF000000000000000000000000000000000000FFFFFF0000
        00000000000000B8B7B7BCBDBD000000000000000000000000FFFFFF00000000
        0000000000000000FFFFFF000000000000000000000000B9B7B8BCBCBD000000
        000000000000000000FFFFFF000000000000000000000000FFFFFF0000000000
        00000000000000B9BABBBDBCBD000000000000000000000000FFFFFF00000000
        0000000000000000FFFFFF000000000000000000000000BCBDBBBDBDBD000000
        000000000000000000000000FFFFFF000000000000FFFFFF0000000000000000
        00000000000000BDBCBCBEBEBE000000000000000000000000000000FFFFFF00
        0000000000FFFFFF000000000000000000000000000000BFBEBDBEBEBF000000
        000000000000000000000000000000FFFFFFFFFFFF0000000000000000000000
        00000000000000BEBFBFBFBFBE00000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000BEBFC0C0BFBF000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000C0C0C0C0C0C0C0C0BFBFBFBFBEBFBEBDBEBEBEBDBDBDBCBDBE
        BCBCBDBEBEBFBCBEBEBDBEC0BFBFBFBFBFC0C0C0BFC0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton9Click
    end
    object SpeedButton10: TSpeedButton
      Left = 232
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Gaussian Blur 2D'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000C0C0C0C0C0BF
        BFBFBFBFBEBFBEBEBEBCBBBDBCBBBBB9BABAB9B9B9B8B7B8B8B9B8B9B9B7BABA
        BABBBCBCBEBDBEC0C0C0BFBFBF000000000000000000FF8300FF8300FF830000
        0000000000FF8300FF8300FF8300000000000000000000BDBEBDBFBFC0000000
        000000000000FF8300FF8300000000000000FF8300000000000000FF83000000
        00000000000000BCBBBBBEBEBE000000000000000000000000FF8300FF830000
        0000FF8300000000000000FF8300000000000000000000BAB9BABFBFBE000000
        000000000000000000000000FF8300000000FF8300000000000000FF83000000
        00000000000000B8B8B7BDBEBE000000000000000000FF8300000000FF830000
        0000000000FF8300FF8300FF8300000000000000000000B8B8B7BDBDBEFFFFFF
        FFFFFFFFFFFFFF8300FF8300FF8300000000000000000000000000FF8300FFFF
        FFFFFFFFFFFFFFB8B7B7BCBDBD000000000000000000FFFFFF00000000000000
        0000000000000000000000FF8300000000000000000000B9B7B8BCBCBD000000
        000000000000FFFFFF000000000000000000000000000000000000FF83000000
        00000000000000B9BABBBDBCBD000000000000000000000000FFFFFF00000000
        0000000000000000FFFFFF000000000000000000000000BCBDBBBDBDBD000000
        000000000000000000FFFFFF000000000000000000000000FFFFFF0000000000
        00000000000000BDBCBCBEBEBE000000000000000000000000FFFFFF00000000
        0000000000000000FFFFFF000000000000000000000000BFBEBDBEBEBF000000
        000000000000000000000000FFFFFF000000000000FFFFFF0000000000000000
        00000000000000BEBFBFBFBFBE000000000000000000000000000000FFFFFF00
        0000000000FFFFFF000000000000000000000000000000BEBFC0C0BFBF000000
        000000000000000000000000000000FFFFFFFFFFFF0000000000000000000000
        00000000000000C0C0C0C0C0C0C0C0BFBFBFBFBEBFBEBDBEBEBEBDBDBDBCBDBE
        BCBCBDBEBEBFBCBEBEBDBEC0BFBFBFBFBFC0C0C0BFC0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton10Click
    end
    object SpeedButton11: TSpeedButton
      Left = 256
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Median'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000C0C0C0C0C0BF
        BFBFBFBFBEBFBEBEBEBCBBBDBCBBBBB9BABAB9B9B9B8B7B8B8B9B8B9B9B7BABA
        BABBBCBCBDBEBEC0C0C0BFBFBF00000000000000000000000000000000000000
        0000000000000000000000000000000000000000BDBEBEBDBEBDBFBFC0000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000BDBEBEBCBBBBBEBEBE000000000000000000000000FF8300FF8300FF
        8300FF8300FF8300000000000000000000000000BDBEBEBAB9BABFBFBE000000
        000000000000FF8300FF8300FF8300FF8300FF8300FF8300FF83000000000000
        00000000BDBEBEB8B8B7BDBEBE000000000000FF8300FF8300000000FF8300FF
        8300FF8300000000FF8300FF8300000000000000BDBEBEB8B8B7BDBDBE000000
        FF8300FF8300FF8300000000FF8300FF8300FF8300000000FF8300FF8300FF83
        00000000BDBEBEB8B7B7BCBDBD000000FF8300FF8300FF8300000000FF8300FF
        8300FF8300000000FF8300FF8300FF8300000000BDBEBEB9B7B8BCBCBD000000
        FF8300FF8300FF8300000000FF8300000000FF8300000000FF8300FF8300FF83
        00000000BDBEBEB9BABBBDBCBD000000FF8300FF8300FF8300000000000000FF
        8300000000000000FF8300FF8300FF8300000000BDBEBEBCBDBBBDBDBD000000
        000000FF8300FF8300000000FF8300FF8300FF8300000000FF8300FF83000000
        00000000BDBEBEBDBCBCBEBEBE000000000000000000FF8300FF8300FF8300FF
        8300FF8300FF8300FF8300000000000000000000BDBEBEBFBEBDBEBEBF000000
        000000000000000000FF8300FF8300FF8300FF8300FF83000000000000000000
        00000000BDBEBEBEBFBFBFBFBE00000000000000000000000000000000000000
        0000000000000000000000000000000000000000BDBEBEBEBFC0C0BFBF000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000BDBEBEC0C0C0C0C0C0C0C0BFBFBFBFBEBFBEBDBEBEBEBDBDBDBCBDBE
        BCBCBDBEBEBFBCBEBEBDBEC0BFBFBFBFBFC0C0C0BFC0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton11Click
    end
    object SpeedButton12: TSpeedButton
      Left = 280
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Vector Median'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000C0C0C0C0C0BF
        BFBFBFBFBEBFBEBEBEBCBBBDBCBBBBB9BABAB9B9B9B8B7B8B8B9B8B9B9B7BABA
        BABBBCBCBDBEBEC0C0C0BFBFBF00000000000000000000000000000000000000
        0000000000000000000000000000000000000000BDBEBEBDBEBDBFBFC0000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000BDBEBEBCBBBBBEBEBE0000000000000000000000000000FF00FF0C00
        FF0C00FF0CFF6E67000000000000000000000000BDBEBEBAB9BABFBFBE000000
        0000000000000000FF0000FF00FF0C00FF0C00FF0CFF6E67FF6E670000000000
        00000000BDBEBEB8B8B7BDBEBE0000000000000000FF0000FF00000000FF0C00
        FF0C00FF0C000000FF6E67FF6E67000000000000BDBEBEB8B8B7BDBDBE000000
        0000FF0000FF0000FF00000000FF0C00FF0C00FF0C000000FF6E67FF6E67FF6E
        67000000BDBEBEB8B7B7BCBDBD0000000000FF0000FF0000FF00000000FF0C00
        FF0C00FF0C000000FF6E67FF6E67FF6E67000000BDBEBEB9B7B8BCBCBD000000
        0000FF0000FF0000FF00000000FF0C00000000FF0C000000FF6E67FF6E67FF6E
        67000000BDBEBEB9BABBBDBCBD0000000000FF0000FF0000FF00000000000000
        FF0C000000000000FF6E67FF6E67FF6E67000000BDBEBEBCBDBBBDBDBD000000
        0000000000FF0000FF00000000FF0C00FF0C00FF0C000000FF6E67FF6E670000
        00000000BDBEBEBDBCBCBEBEBE0000000000000000000000FF0000FF00FF0C00
        FF0C00FF0CFF6E67FF6E67000000000000000000BDBEBEBFBEBDBEBEBF000000
        0000000000000000000000FF00FF0C00FF0C00FF0CFF6E670000000000000000
        00000000BDBEBEBEBFBFBFBFBE00000000000000000000000000000000000000
        0000000000000000000000000000000000000000BDBEBEBEBFC0C0BFBF000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000BDBEBEC0C0C0C0C0C0C0C0BFBFBFBFBEBFBEBDBEBEBEBDBDBDBCBDBE
        BCBCBDBEBEBFBCBEBEBDBEC0BFBFBFBFBFC0C0C0BFC0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton12Click
    end
    object SpeedButton13: TSpeedButton
      Left = 304
      Top = 0
      Width = 25
      Height = 25
      Hint = 'K Nearest Neighbors DeNoise'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000C0C0C0BEBFBE
        BFBFBFBFBEBFBEBFBEBCBBBDBCBBBBBEBFBEB9B9B9BEBFBEB8B9B8BEBFBEBABA
        BABBBCBCBDBEBEC0C0C0BFBFBFBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBE
        BFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBDBEBDBEBFBEBEBFBE
        BEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBF
        BEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBE
        BFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBE
        BEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBF
        BEBEBFBEBEBFBEBEBFBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE000000BD
        BEBEBDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE000000000000BDBEBE
        BDBEBE000000BDBEBEBDBEBE000000BDBEBE000000000000BDBEBEBDBEBE0000
        00BDBEBE000000000000000000000000000000BDBEBEBDBEBEBDBEBE000000BD
        BEBE000000000000BDBEBEBDBEBE000000BDBEBE000000000000000000000000
        BDBEBEBDBEBEBDBEBEBDBEBE000000000000BDBEBE000000BDBEBEBDBEBE0000
        00000000BDBEBE000000000000BDBEBE000000BDBEBEBDBEBEBDBEBE00000000
        0000BDBEBE000000BDBEBEBDBEBE000000000000BDBEBE000000000000BDBEBE
        BDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE0000
        00BDBEBEBDBEBE000000BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBD
        BEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBE
        BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBE
        BEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBD
        BEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBE
        BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBE
        BEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBD
        BEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBE}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton13Click
    end
    object SpeedButton14: TSpeedButton
      Left = 328
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Non-Local Means DeNoise'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000C0C0C0BEBFBE
        BFBFBFBFBEBFBEBFBEBCBBBDBCBBBBBEBFBEB9B9B9BEBFBEB8B9B8BEBFBEBABA
        BABBBCBCBDBEBEC0C0C0BFBFBFBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBE
        BFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBDBEBDBEBFBEBEBFBE
        BEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBF
        BEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBE
        BFBEBDBEBEBDBEBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBDBEBEBDBEBE
        BDBEBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBDBEBEBDBEBEBDBEBEBDBE
        BEBDBEBEBEBFBEBEBFBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE00000000
        0000000000BDBEBEBDBEBE000000BDBEBE000000BDBEBE000000000000BDBEBE
        000000000000BDBEBEBDBEBE000000BDBEBEBDBEBEBDBEBEBDBEBE000000BDBE
        BE000000BDBEBE000000000000BDBEBE000000000000BDBEBEBDBEBE000000BD
        BEBEBDBEBEBDBEBEBDBEBE000000000000BDBEBE000000000000000000000000
        BDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBEBDBEBEBDBEBE0000000000
        00BDBEBE000000000000000000000000BDBEBE000000BDBEBEBDBEBE000000BD
        BEBEBDBEBEBDBEBEBDBEBE000000000000BDBEBE000000000000000000BDBEBE
        BDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBEBDBEBEBDBEBE0000000000
        00BDBEBE000000000000BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBD
        BEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBE
        BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBE
        BEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBD
        BEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBE
        BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBE
        BEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBD
        BEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBE}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton14Click
    end
    object SpeedButton15: TSpeedButton
      Left = 376
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Automatic Non-Local Means DeNoise'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000C0C0C0BEBFBE
        BFBFBFBFBEBFBEBFBEBCBBBDBCBBBBBEBFBEB9B9B9BEBFBEB8B9B8BEBFBEBABA
        BABBBCBCBDBEBEC0C0C0000000BDBEBEBDBEBE000000BDBEBEBDBEBE00000000
        0000000000BDBEBEBDBEBE000000BDBEBE000000BDBEBE000000000000BDBEBE
        000000000000BDBEBEBDBEBE000000BDBEBEBDBEBEBDBEBEBDBEBE000000BDBE
        BE000000BDBEBE000000000000BDBEBE000000000000BDBEBEBDBEBE000000BD
        BEBEBDBEBEBDBEBEBDBEBE000000000000BDBEBE000000000000000000000000
        BDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBEBDBEBEBDBEBE0000000000
        00BDBEBE000000000000000000000000BDBEBE000000BDBEBEBDBEBE000000BD
        BEBEBDBEBEBDBEBEBDBEBE000000000000BDBEBE000000000000000000BDBEBE
        BDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBEBDBEBEBDBEBE0000000000
        00BDBEBE000000000000BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBD
        BEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBE
        BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBE
        BEBDBEBEBDBEBEBDBEBE000000000000000000BDBEBE00000000000000000000
        0000BDBEBEBDBEBE000000000000BDBEBE000000000000BDBEBE000000BDBEBE
        000000BDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE000000BDBEBE0000
        00BDBEBEBDBEBE000000BDBEBE000000000000BDBEBE000000BDBEBEBDBEBE00
        0000BDBEBEBDBEBE000000BDBEBE000000BDBEBEBDBEBE000000000000BDBEBE
        000000BDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE000000BDBEBE0000
        00BDBEBEBDBEBE000000000000000000000000BDBEBE000000BDBEBEBDBEBE00
        0000BDBEBE000000000000000000BDBEBE000000000000BDBEBEBDBEBEBDBEBE
        BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBE000000BDBEBEBDBE
        BEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBD
        BEBEBDBEBEBDBEBE000000BDBEBEBDBEBEBDBEBEBDBEBEBDBEBE}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton15Click
    end
    object SpeedButton16: TSpeedButton
      Left = 352
      Top = 0
      Width = 25
      Height = 25
      Hint = 'Automatic K Nearest Neighbors DeNoise'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000BEBFBEBEBFBE
        BEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBFBEBEBF
        BEBEBFBEBEBFBEBEBFBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE000000BD
        BEBEBDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE000000000000BDBEBE
        BDBEBE000000BDBEBEBDBEBE000000BDBEBE000000000000BDBEBEBDBEBE0000
        00BDBEBE000000000000000000000000000000BDBEBEBDBEBEBDBEBE000000BD
        BEBE000000000000BDBEBEBDBEBE000000BDBEBE000000000000000000000000
        BDBEBEBDBEBEBDBEBEBDBEBE000000000000BDBEBE000000BDBEBEBDBEBE0000
        00000000BDBEBE000000000000BDBEBE000000BDBEBEBDBEBEBDBEBE00000000
        0000BDBEBE000000BDBEBEBDBEBE000000000000BDBEBE000000000000BDBEBE
        BDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE0000
        00BDBEBEBDBEBE000000BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBD
        BEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBE
        BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBE
        BEBDBEBEBDBEBEBDBEBE000000000000000000BDBEBE00000000000000000000
        0000BDBEBEBDBEBE000000000000BDBEBE000000000000BDBEBE000000BDBEBE
        000000BDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE000000BDBEBE0000
        00BDBEBEBDBEBE000000BDBEBE000000000000BDBEBE000000BDBEBEBDBEBE00
        0000BDBEBEBDBEBE000000BDBEBE000000BDBEBEBDBEBE000000000000BDBEBE
        000000BDBEBE000000BDBEBEBDBEBE000000BDBEBEBDBEBE000000BDBEBE0000
        00BDBEBEBDBEBE000000000000000000000000BDBEBE000000BDBEBEBDBEBE00
        0000BDBEBE000000000000000000BDBEBE000000000000BDBEBEBDBEBEBDBEBE
        BDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBE000000BDBEBEBDBE
        BEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBDBEBEBD
        BEBEBDBEBEBDBEBE000000BDBEBEBDBEBEBDBEBEBDBEBEBDBEBE}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton16Click
    end
    object Panel3: TPanel
      Left = 1
      Top = 25
      Width = 561
      Height = 16
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 0
        Top = 0
        Width = 89
        Height = 16
        Align = alLeft
        AutoSize = False
        Caption = 'Current Operation:'
      end
      object Gauge1: TGauge
        Left = 89
        Top = 0
        Width = 472
        Height = 16
        Align = alClient
        Progress = 0
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 42
    Width = 563
    Height = 351
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 409
      Top = 0
      Height = 351
      OnMoved = Splitter1Moved
    end
    object GroupBox1: TGroupBox
      Left = 0
      Top = 0
      Width = 409
      Height = 351
      Align = alLeft
      Caption = 'Image 1'
      TabOrder = 0
      object Image1: TImage
        Left = 2
        Top = 15
        Width = 405
        Height = 334
        Align = alClient
        OnClick = Image1Click
      end
    end
    object GroupBox2: TGroupBox
      Left = 412
      Top = 0
      Width = 151
      Height = 351
      Align = alClient
      Caption = 'Image 2'
      TabOrder = 1
      object Image2: TImage
        Left = 2
        Top = 15
        Width = 147
        Height = 334
        Align = alClient
        OnClick = Image2Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Bitmap (*.bmp)|*.bmp'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 268
    Top = 193
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Bitmap (*.bmp)|*.bmp'
    Left = 268
    Top = 233
  end
end
