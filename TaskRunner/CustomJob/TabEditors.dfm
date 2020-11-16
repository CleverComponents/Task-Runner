object TabEditorsFrame: TTabEditorsFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    TabOrder = 0
    TabStop = False
  end
  object PopupMenu: TPopupMenu
    Left = 144
    Top = 104
    object Save1: TMenuItem
      Caption = '&Save'
      Enabled = False
    end
    object Close1: TMenuItem
      Caption = '&Close'
    end
  end
end
