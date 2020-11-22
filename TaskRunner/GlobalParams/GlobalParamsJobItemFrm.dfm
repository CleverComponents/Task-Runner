object GlobalParamsJobItemForm: TGlobalParamsJobItemForm
  Left = 264
  Top = 109
  Caption = 'Global Parameters Editor'
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
  Position = poOwnerFormCenter
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
  object pMain: TPanel
    Left = 0
    Top = 0
    Width = 498
    Height = 328
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 498
      Height = 47
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Navigator: TDBNavigator
        Left = 0
        Top = 10
        Width = 230
        Height = 25
        DataSource = DataSource
        ConfirmDelete = False
        TabOrder = 0
      end
    end
    object List: TDBGrid
      Left = 0
      Top = 47
      Width = 498
      Height = 281
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
