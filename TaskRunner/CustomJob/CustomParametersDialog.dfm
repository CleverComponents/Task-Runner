inherited CustomParamsJobItemForm: TCustomParamsJobItemForm
  Caption = 'Global Parameters Dialog'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl: TPageControl
    ActivePage = tabDetails
    inherited tabDetails: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 490
      ExplicitHeight = 300
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 490
        Height = 47
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Navigator: TDBNavigator
          Left = 0
          Top = 10
          Width = 234
          Height = 25
          DataSource = DataSource
          VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
          ConfirmDelete = False
          TabOrder = 0
        end
      end
      object List: TDBGrid
        Left = 0
        Top = 47
        Width = 490
        Height = 253
        Align = alClient
        DataSource = DataSource
        Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgCancelOnExit]
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'paramname'
            Title.Caption = 'Parameter Name'
            Width = 100
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'paramvalue'
            Title.Caption = 'Parameter Value'
            Width = 356
            Visible = True
          end>
      end
    end
  end
  object DataSource: TDataSource
    DataSet = MemData
    OnStateChange = DataSourceStateChange
    Left = 178
    Top = 152
  end
  object MemData: TJobMemData
    Aggregates = <>
    Params = <>
    AfterDelete = MemDataAfterDelete
    Left = 248
    Top = 152
    object MemDataparamname: TStringField
      FieldName = 'paramname'
      Size = 150
    end
    object MemDataparamvalue: TStringField
      FieldName = 'paramvalue'
      Size = 150
    end
  end
end
