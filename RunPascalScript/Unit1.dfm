object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Run Pascal Script With Params'
  ClientHeight = 58
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnStartScript: TButton
    Left = 184
    Top = 8
    Width = 97
    Height = 41
    Caption = 'Start Script'
    TabOrder = 0
    OnClick = btnStartScriptClick
  end
  object btnStart: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 42
    Caption = 'Start in Thread'
    TabOrder = 1
    OnClick = btnStartClick
  end
end
