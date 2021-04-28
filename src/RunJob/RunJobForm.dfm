object RunJobfrm: TRunJobfrm
  Left = 313
  Top = 230
  Caption = 'Run Job'
  ClientHeight = 466
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object sidebarSplitter: TSplitter
    Left = 0
    Top = 233
    Width = 504
    Height = 4
    Cursor = crVSplit
    Align = alTop
    MinSize = 1
    ExplicitTop = 161
    ExplicitWidth = 500
  end
  object PageControl: TPageControl
    Left = 0
    Top = 237
    Width = 504
    Height = 229
    ActivePage = tabDescription
    Align = alClient
    TabOrder = 0
    object tabDescription: TTabSheet
      Caption = 'Description'
      object MemoLog: TJobRichEdit
        Left = 0
        Top = 0
        Width = 496
        Height = 201
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Constraints.MinHeight = 20
        ParentFont = False
        PlainText = True
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        Zoom = 100
      end
    end
    object tabErrors: TTabSheet
      Caption = 'Errors'
      ImageIndex = 1
      object MemoError: TJobRichEdit
        Left = 0
        Top = 0
        Width = 496
        Height = 201
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Constraints.MinHeight = 20
        ParentFont = False
        PlainText = True
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        Zoom = 100
      end
    end
  end
  object List: TDBGrid
    Left = 0
    Top = 0
    Width = 504
    Height = 233
    Align = alTop
    DataSource = DataSource
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect]
    ReadOnly = True
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnCellClick = ListCellClick
    OnKeyDown = ListKeyDown
    Columns = <
      item
        Expanded = False
        FieldName = 'jobname'
        Title.Caption = 'Job Name'
        Width = 133
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'currentjobname'
        Title.Caption = 'Current Job Name'
        Width = 261
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'jobstate'
        Title.Caption = 'State'
        Width = 82
        Visible = True
      end>
  end
  object DataSource: TDataSource
    DataSet = MemData
    Left = 152
    Top = 112
  end
  object MemData: TJobMemData
    Aggregates = <>
    Params = <>
    Left = 214
    Top = 114
    object MemDatajobname: TStringField
      DisplayWidth = 150
      FieldName = 'jobname'
      Size = 150
    end
    object MemDatacurrentjobname: TStringField
      DisplayWidth = 150
      FieldName = 'currentjobname'
      Size = 150
    end
    object MemDatajobstate: TStringField
      FieldName = 'jobstate'
    end
    object MemDatavisitor: TIntegerField
      FieldName = 'visitor'
    end
    object MemDatalog: TBlobField
      FieldName = 'log'
    end
    object MemDataerrors: TBlobField
      FieldName = 'errors'
    end
    object MemDataisrun: TBooleanField
      FieldName = 'isrun'
    end
    object MemDatacurrentjob: TIntegerField
      FieldName = 'currentjob'
    end
  end
end
