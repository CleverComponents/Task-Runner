inherited ConnectionSetupForm: TConnectionSetupForm
  Left = 266
  Top = 105
  BorderStyle = bsDialog
  Caption = 'Connection Setup'
  ClientHeight = 206
  ClientWidth = 352
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  ExplicitWidth = 358
  ExplicitHeight = 235
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 11
    Width = 26
    Height = 13
    Caption = 'Login'
  end
  object Label2: TLabel
    Left = 5
    Top = 33
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object Label3: TLabel
    Left = 5
    Top = 55
    Width = 31
    Height = 13
    Caption = 'Server'
  end
  object Label4: TLabel
    Left = 5
    Top = 77
    Width = 46
    Height = 13
    Caption = 'Database'
  end
  object Label5: TLabel
    Left = 5
    Top = 99
    Width = 43
    Height = 13
    Caption = 'Time Out'
  end
  object edtLogin: TEdit
    Left = 65
    Top = 8
    Width = 278
    Height = 21
    TabOrder = 0
  end
  object edtPassword: TEdit
    Left = 65
    Top = 30
    Width = 278
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
    OnChange = edtPasswordChange
  end
  object edtServer: TEdit
    Left = 65
    Top = 52
    Width = 278
    Height = 21
    TabOrder = 2
    OnChange = edtServerChange
  end
  object edtDatabase: TComboBox
    Left = 65
    Top = 74
    Width = 278
    Height = 21
    TabOrder = 3
    OnDropDown = edtDatabaseDropDown
  end
  object btnOK: TButton
    Left = 196
    Top = 176
    Width = 72
    Height = 22
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
  end
  object btnCancel: TButton
    Left = 272
    Top = 176
    Width = 72
    Height = 22
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object edtTimeOut: TEdit
    Left = 65
    Top = 96
    Width = 278
    Height = 21
    TabOrder = 4
  end
  object edtOLEDB: TEdit
    Left = 65
    Top = 145
    Width = 253
    Height = 21
    TabOrder = 6
  end
  object chkUseOLEDB: TCheckBox
    Left = 5
    Top = 126
    Width = 129
    Height = 17
    Caption = 'Use OLE DB Provider'
    TabOrder = 5
    OnClick = chkUseOLEDBClick
  end
  object btnBuildOLEDB: TButton
    Left = 320
    Top = 145
    Width = 23
    Height = 21
    Caption = '...'
    TabOrder = 7
    OnClick = btnBuildOLEDBClick
  end
end
