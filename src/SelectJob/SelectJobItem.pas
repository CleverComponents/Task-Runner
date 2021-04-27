unit SelectJobItem;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JobClasses, JobCtrls;

type
  TSelectjobItemForm = class(TForm)
    edtName: TEdit;
    cmbItemType: TJobComboBox;
    Label1: TLabel;
    Label2: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmbItemTypeCloseUp(Sender: TObject);
  private
    FNameEdited: Boolean;
    procedure DoNewItemName(AName: String);
  end;

function SelectJobItemDlg(var AName: String; var AType: TJobDataItemClass): Boolean;

implementation

uses
  JobConsts;

{$R *.DFM}

procedure FillItemsWithJobTypes(AList: TStrings);
var
  i: Integer;
  Item: TJobEditorCategory;
begin
  AList.Clear();

  for i := 0 to TJobEditorManager.Instance.EditorCategoriesCount - 1 do
  begin
    Item := TJobEditorManager.Instance.EditorCategories[i];
    AList.AddObject(Item.DataName, TObject(Item.DataClass));
  end;
end;

function SelectJobItemDlg(var AName: String; var AType: TJobDataItemClass): Boolean;
var
  Dlg: TSelectjobItemForm;
begin
  Dlg := TSelectjobItemForm.Create(nil);
  try
    FillItemsWithJobTypes(Dlg.cmbItemType.Items);
    Result := (Dlg.cmbItemType.Items.Count > 0);

    if Result then
    begin
      Dlg.cmbItemType.ItemIndex := 0;
      Dlg.DoNewItemName(Dlg.cmbItemType.Text);
      Result := (Dlg.ShowModal() = mrOK);
    end;

    if Result then
    begin
      AName := Dlg.edtName.Text;
      AType := TJobDataItemClass(Dlg.cmbItemType.Items.Objects[Dlg.cmbItemType.ItemIndex]);
    end;
  finally
    Dlg.Free();
  end;
end;

{ TSelectjobItemForm }

procedure TSelectjobItemForm.btnOKClick(Sender: TObject);
begin
  if (Trim(edtName.Text) = '') or (cmbItemType.ItemIndex < 0) then Exit;
  ModalResult := mrOk;
end;

procedure TSelectjobItemForm.DoNewItemName(AName: String);
begin
  edtName.Text := Format(cNewJobItemName, [AName]);
  FNameEdited := False;
end;

procedure TSelectjobItemForm.edtNameChange(Sender: TObject);
begin
  FNameEdited := True;
end;

procedure TSelectjobItemForm.FormCreate(Sender: TObject);
begin
  FNameEdited := False;
end;

procedure TSelectjobItemForm.cmbItemTypeCloseUp(Sender: TObject);
begin
  if FNameEdited then Exit;
  DoNewItemName(cmbItemType.Text);
end;

end.
