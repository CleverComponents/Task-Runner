inherited CustomScriptJobItemForm: TCustomScriptJobItemForm
  Caption = 'CustomScriptJobItemForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pBottom: TPanel
    TabOrder = 1
    inherited Panel1: TPanel
      inherited btnOK: TButton
        Caption = '&OK'
      end
      inherited btnCancel: TButton
        Caption = '&Cancel'
      end
      inherited btnApply: TButton
        Caption = '&Apply'
      end
    end
  end
  inherited PageControl: TPageControl
    ActivePage = tabDetails
    TabOrder = 0
    OnChange = PageControlChange
    inherited tabDetails: TTabSheet
      object memoScript: TJobRichEdit
        Left = 0
        Top = 0
        Width = 490
        Height = 281
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Lines.Strings = (
          'Memo')
        ParentFont = False
        PlainText = True
        ScrollBars = ssBoth
        TabOrder = 0
        WantTabs = True
        WordWrap = False
        Zoom = 100
        OnChange = memoScriptChange
        OnSelectionChange = memoScriptSelectionChange
      end
      object sbScript: TStatusBar
        Left = 0
        Top = 281
        Width = 490
        Height = 19
        BiDiMode = bdLeftToRight
        Panels = <
          item
            Alignment = taRightJustify
            BiDiMode = bdRightToLeft
            ParentBiDiMode = False
            Text = 'Ln 1, Col 1'
            Width = 10
          end>
        ParentBiDiMode = False
      end
    end
    inherited tabAddition: TTabSheet
      inherited pAddTop: TPanel
        Height = 87
        ExplicitHeight = 87
        object Label2: TLabel [1]
          Left = 6
          Top = 37
          Width = 46
          Height = 13
          Caption = 'Script File'
        end
        object Label3: TLabel [2]
          Left = 6
          Top = 59
          Width = 37
          Height = 13
          Caption = 'Log File'
        end
        inherited edtCanPerform: TEdit
          TabOrder = 6
        end
        object edtScriptFile: TEdit
          Left = 76
          Top = 34
          Width = 145
          Height = 21
          TabOrder = 1
          OnChange = EditFileChange
        end
        object edtLogFile: TEdit
          Left = 76
          Top = 56
          Width = 145
          Height = 21
          TabOrder = 3
          OnChange = EditFileChange
        end
        object btnErrorWords: TButton
          Left = 328
          Top = 56
          Width = 72
          Height = 22
          Caption = 'Error Words'
          TabOrder = 5
          OnClick = btnErrorWordsClick
        end
        object chkScriptFile: TCheckBox
          Left = 226
          Top = 36
          Width = 18
          Height = 17
          TabOrder = 2
          OnClick = CheckBoxChange
        end
        object chkLogFile: TCheckBox
          Left = 226
          Top = 58
          Width = 19
          Height = 17
          TabOrder = 4
          OnClick = CheckBoxChange
        end
      end
      inherited MemoDescription: TJobRichEdit
        Top = 87
        Height = 194
        ExplicitTop = 87
        ExplicitHeight = 194
      end
      inherited sbDescription: TStatusBar
        ExplicitWidth = 492
      end
    end
  end
end
