inherited ItemListViewForm: TItemListViewForm
  Left = 262
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Edit Items'
  ClientHeight = 195
  ClientWidth = 318
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  ExplicitWidth = 324
  ExplicitHeight = 224
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 168
    Width = 48
    Height = 13
    Caption = 'Bind Type'
  end
  object btnOK: TButton
    Left = 164
    Top = 164
    Width = 72
    Height = 22
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 237
    Top = 164
    Width = 72
    Height = 22
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 318
    Height = 147
    Align = alTop
    TabOrder = 0
  end
  object cmbBindType: TJobComboBox
    Left = 64
    Top = 165
    Width = 87
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
end
