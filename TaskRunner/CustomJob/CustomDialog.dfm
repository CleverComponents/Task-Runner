object CustomDialogForm: TCustomDialogForm
  Left = 0
  Top = 0
  Width = 532
  Height = 451
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 532
    Height = 451
    ActivePage = tabAddition
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    object tabDetails: TTabSheet
      Caption = 'Details'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 341
    end
    object tabAddition: TTabSheet
      Caption = 'Addition'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pAddTop: TPanel
        Left = 0
        Top = 0
        Width = 524
        Height = 47
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 6
          Top = 16
          Width = 55
          Height = 13
          Caption = 'Flow Action'
        end
        object lblCanPerform: TLabel
          Left = 258
          Top = 16
          Width = 58
          Height = 13
          Caption = 'Can Perform'
        end
        object cmbFlowAction: TJobComboBox
          Left = 76
          Top = 12
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = AdditionDataChange
          OnCloseUp = AdditionDataChange
        end
        object edtCanPerform: TEdit
          Left = 328
          Top = 12
          Width = 145
          Height = 21
          TabOrder = 1
          OnChange = AdditionDataChange
        end
      end
      object MemoDescription: TJobRichEdit
        Left = 0
        Top = 47
        Width = 524
        Height = 359
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Lines.Strings = (
          'MemoDescription')
        ParentFont = False
        PlainText = True
        ScrollBars = ssBoth
        TabOrder = 1
        WantTabs = True
        WordWrap = False
        Zoom = 100
        OnChange = AdditionDataChange
        OnSelectionChange = MemoDescriptionSelectionChange
      end
      object sbDescription: TStatusBar
        Left = 0
        Top = 406
        Width = 524
        Height = 19
        Panels = <
          item
            Alignment = taRightJustify
            Text = 'Ln 1, Col 1'
            Width = 50
          end>
      end
    end
  end
end
