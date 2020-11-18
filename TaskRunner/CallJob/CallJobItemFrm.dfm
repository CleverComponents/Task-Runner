inherited CallJobItemForm: TCallJobItemForm
  Caption = 'CallJobItemForm'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl: TPageControl
    inherited tabDetails: TTabSheet
      ExplicitTop = 4
      ExplicitWidth = 506
      ExplicitHeight = 341
      inherited Panel2: TPanel
        Width = 506
        Height = 67
        ExplicitWidth = 506
        ExplicitHeight = 67
        object Label2: TLabel [0]
          Left = 235
          Top = 37
          Width = 37
          Height = 13
          Caption = 'Call Job'
        end
        object lblProject: TLabel [1]
          Left = 235
          Top = 16
          Width = 33
          Height = 13
          Caption = 'Project'
        end
        inherited Navigator: TDBNavigator
          Width = 224
          VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbEdit, nbPost, nbCancel, nbRefresh]
          Hints.Strings = ()
          ExplicitWidth = 224
        end
        object cmbCallJob: TComboBox
          Left = 281
          Top = 34
          Width = 192
          Height = 21
          TabOrder = 2
          OnChange = cmbCallJobChange
          OnDropDown = cmbCallJobDropDown
        end
        object edtMediaName: TEdit
          Left = 281
          Top = 12
          Width = 192
          Height = 21
          TabOrder = 1
          OnChange = edtMediaNameChange
        end
        object btnEditJob: TButton
          Left = 477
          Top = 33
          Width = 23
          Height = 23
          Caption = '...'
          TabOrder = 3
          OnClick = btnEditJobClick
        end
      end
      inherited List: TDBGrid
        Top = 67
        Width = 506
        Height = 274
        OnKeyDown = ListKeyDown
        Columns = <
          item
            Expanded = False
            FieldName = 'paramname'
            ReadOnly = True
            Title.Caption = 'Parameter Name'
            Width = 100
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'paramvalue'
            Title.Caption = 'Parameter Value'
            Width = 359
            Visible = True
          end>
      end
    end
    inherited tabAddition: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 4
      ExplicitWidth = 506
      ExplicitHeight = 341
    end
  end
end
