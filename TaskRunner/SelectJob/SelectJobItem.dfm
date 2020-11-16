object SelectjobItemForm: TSelectjobItemForm
  Left = 262
  Top = 107
  BorderStyle = bsDialog
  Caption = 'New Job Item'
  ClientHeight = 93
  ClientWidth = 238
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 13
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 6
    Top = 35
    Width = 24
    Height = 13
    Caption = 'Type'
  end
  object edtName: TEdit
    Left = 48
    Top = 8
    Width = 184
    Height = 21
    TabOrder = 0
    OnChange = edtNameChange
  end
  object cmbItemType: TJobComboBox
    Left = 48
    Top = 30
    Width = 184
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnCloseUp = cmbItemTypeCloseUp
  end
  object btnOK: TButton
    Left = 84
    Top = 62
    Width = 72
    Height = 22
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 160
    Top = 62
    Width = 72
    Height = 22
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
