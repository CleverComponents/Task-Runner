inherited CommandBatchJobItemForm: TCommandBatchJobItemForm
  Caption = 'Command Batch Editor'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl: TPageControl
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
  end
end
