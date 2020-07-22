object CustomDialogForm: TCustomDialogForm
  Left = 264
  Top = 109
  Caption = 'CustomDialogForm'
  ClientHeight = 363
  ClientWidth = 498
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object pBottom: TPanel
    Left = 0
    Top = 328
    Width = 498
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel1: TPanel
      Left = 269
      Top = 0
      Width = 229
      Height = 35
      Align = alRight
      BevelOuter = bvNone
      Caption = 'Panel1'
      TabOrder = 0
      object btnOK: TButton
        Left = 6
        Top = 8
        Width = 72
        Height = 22
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TButton
        Left = 79
        Top = 8
        Width = 72
        Height = 22
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
        OnClick = btnCancelClick
      end
      object btnApply: TButton
        Left = 152
        Top = 8
        Width = 72
        Height = 22
        Caption = 'Apply'
        TabOrder = 2
        OnClick = btnApplyClick
      end
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 498
    Height = 328
    ActivePage = tabAddition
    Align = alClient
    TabOrder = 1
    object tabDetails: TTabSheet
      Caption = 'Details'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object tabAddition: TTabSheet
      Caption = 'Addition'
      ImageIndex = 1
      object pAddTop: TPanel
        Left = 0
        Top = 0
        Width = 490
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
        Width = 490
        Height = 234
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
        Top = 281
        Width = 490
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
