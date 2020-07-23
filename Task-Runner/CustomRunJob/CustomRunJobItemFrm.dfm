inherited CustomRunJobItemForm: TCustomRunJobItemForm
  Left = 594
  Top = 219
  Caption = 'Custom Run Editor'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl: TPageControl
    ActivePage = tabAddition
    inherited tabDetails: TTabSheet
      inherited memoScript: TJobRichEdit
        Top = 53
        Height = 229
        TabOrder = 1
      end
      inherited sbScript: TStatusBar
        Panels = <
          item
            Alignment = taRightJustify
            Text = 'Ln 1, Col 1'
            Width = 10
          end>
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 484
        Height = 53
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label4: TLabel
          Left = 2
          Top = 7
          Width = 70
          Height = 13
          Caption = 'Command Line'
        end
        object Label5: TLabel
          Left = 2
          Top = 29
          Width = 59
          Height = 13
          Caption = 'Param Prefix'
        end
        object Label6: TLabel
          Left = 262
          Top = 29
          Width = 73
          Height = 13
          Caption = 'Param Delimiter'
        end
        object edtCommandLine: TEdit
          Left = 79
          Top = 4
          Width = 410
          Height = 21
          TabOrder = 0
          OnChange = edtCommandLineChange
        end
        object edtParamPrefix: TEdit
          Left = 79
          Top = 26
          Width = 150
          Height = 21
          TabOrder = 1
          OnChange = edtCommandLineChange
        end
        object edtParamDelimiter: TEdit
          Left = 339
          Top = 26
          Width = 150
          Height = 21
          TabOrder = 2
          OnChange = edtCommandLineChange
        end
      end
    end
  end
end
