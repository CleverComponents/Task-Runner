object RunJobfrm: TRunJobfrm
  Left = 313
  Top = 230
  Caption = 'Run Job'
  ClientHeight = 291
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object pBottom: TPanel
    Left = 0
    Top = 256
    Width = 500
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel1: TPanel
      Left = 416
      Top = 0
      Width = 84
      Height = 35
      Align = alRight
      BevelOuter = bvNone
      Caption = 'Panel1'
      TabOrder = 0
      object btnClose: TButton
        Left = 8
        Top = 8
        Width = 72
        Height = 22
        Caption = 'Close'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = btnCloseClick
      end
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 500
    Height = 256
    ActivePage = tabJobs
    Align = alClient
    TabOrder = 1
    OnChange = PageControlChange
    object tabJobs: TTabSheet
      Caption = 'Jobs'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object List: TDBGrid
        Left = 0
        Top = 0
        Width = 492
        Height = 228
        Align = alClient
        DataSource = DataSource
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect]
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        OnDblClick = ListDblClick
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
    end
    object tabLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object sidebarSplitter: TSplitter
        Left = 0
        Top = 161
        Width = 492
        Height = 4
        Cursor = crVSplit
        Align = alTop
        MinSize = 1
        ExplicitWidth = 500
      end
      object MemoLog: TJobRichEdit
        Left = 0
        Top = 0
        Width = 492
        Height = 161
        Align = alTop
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
      object MemoError: TJobRichEdit
        Left = 0
        Top = 165
        Width = 492
        Height = 63
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
        TabOrder = 1
        Zoom = 100
      end
    end
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
