unit ItemListView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JobCtrls, JobConsts, CustomModalDlg;

type
  TItemListViewForm = class(TCustomModalDialog)
    btnOK: TButton;
    btnCancel: TButton;
    Memo: TMemo;
    Label1: TLabel;
    cmbBindType: TJobComboBox;
    procedure FormCreate(Sender: TObject);
  private
    FReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
  protected
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

function ShowItemList(AList: TStrings; var ABindType: TJobListBindType; AReadOnly: Boolean): Boolean;

implementation

{$R *.DFM}

function ShowItemList(AList: TStrings; var ABindType: TJobListBindType; AReadOnly: Boolean): Boolean;
var
  Dlg: TItemListViewForm;
begin
  Dlg := TItemListViewForm.Create(nil);
  try
    Dlg.cmbBindType.ItemIndex := Integer(ABindType);
    Dlg.Memo.Lines.Assign(AList);
    Dlg.ReadOnly := AReadOnly;
    Result := (Dlg.ShowModal() = mrOK) and (not AReadOnly);
    if Result then
    begin
      ABindType := TJobListBindType(Dlg.cmbBindType.ItemIndex);
      AList.Assign(Dlg.Memo.Lines);
    end;
  finally
    Dlg.Free();
  end;
end;

procedure TItemListViewForm.FormCreate(Sender: TObject);
var
  i: TJobListBindType;
begin
  cmbBindType.Items.Clear();
  for i := Low(cJobListBindTypeNames) to High(cJobListBindTypeNames) do
  begin
    cmbBindType.Items.Add(cJobListBindTypeNames[i]);
  end;
end;

procedure TItemListViewForm.SetReadOnly(const Value: Boolean);
begin
  if (FReadOnly <> Value) then
  begin
    FReadOnly := Value;
    Memo.ReadOnly := FReadOnly;
    cmbBindType.Enabled := not FReadOnly;
  end;
end;

end.
