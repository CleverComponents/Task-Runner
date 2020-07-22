inherited SQLScriptJobItemForm: TSQLScriptJobItemForm
  Caption = 'SQL Script Editor'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pBottom: TPanel
    object btnConnection: TButton
      Left = 5
      Top = 8
      Width = 72
      Height = 22
      Caption = 'Connection'
      TabOrder = 1
      OnClick = btnConnectionClick
    end
  end
  inherited PageControl: TPageControl
    ActivePage = tabAddition
    inherited tabDetails: TTabSheet
      inherited sbScript: TStatusBar
        Panels = <
          item
            Alignment = taRightJustify
            Text = 'Ln 1, Col 1'
            Width = 10
          end>
      end
    end
    inherited tabAddition: TTabSheet
      inherited pAddTop: TPanel
        object lblPerformWith: TLabel [3]
          Left = 258
          Top = 37
          Width = 61
          Height = 13
          Caption = 'Perform With'
        end
        inherited edtCanPerform: TEdit
          TabOrder = 7
        end
        object cmbPerformWith: TEdit
          Left = 328
          Top = 34
          Width = 145
          Height = 21
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 6
        end
      end
    end
  end
end
