unit ScripterJobItemFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CustomScriptDialog, CustomDialog, StdCtrls, ComCtrls, ExtCtrls, JobClasses,
  JobCtrls;

type
  TScripterJobItemForm = class(TCustomScriptJobItemForm)
    Label4: TLabel;
    cmbLanguage: TComboBox;
    procedure cmbLanguageChange(Sender: TObject);
  protected
    procedure UpdateControls; override;
    procedure AssignData(IsFromDataItem: Boolean = False); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TScripterJobEditorItem = class(TCustomJobEditorItem)
  protected
    function GetEditorFormClass: TCustomDialogFormClass; override;
  end;

implementation

{$R *.DFM}

uses
  ScripterJobItem, JobConsts;

{ TVBScriptJobEditorItem }

function TScripterJobEditorItem.GetEditorFormClass: TCustomDialogFormClass;
begin
  Result := TScripterJobItemForm;
end;

{ TScripterJobItemForm }

procedure TScripterJobItemForm.AssignData(IsFromDataItem: Boolean);
var
  Index: Integer;
begin
  inherited AssignData(IsFromDataItem);

  if IsFromDataItem then
  begin
    cmbLanguage.ItemIndex := Integer(TScripterDataItem(Data).Language);
  end else
  begin
    Index := cmbLanguage.ItemIndex;
    if (Index < 0) then
    begin
      TScripterDataItem(Data).Language := Low(TScripterLanguage);
    end else
    begin
      TScripterDataItem(Data).Language := TScripterLanguage(Index);
    end;
  end;
end;

constructor TScripterJobItemForm.Create(AOwner: TComponent);
var
  i: TScripterLanguage;
begin
  inherited Create(AOwner);
  cmbLanguage.Items.Clear();
  for i := Low(cScripterLanguages) to High(cScripterLanguages) do
  begin
    cmbLanguage.Items.Add(cScripterLanguages[i]);
  end;
end;

procedure TScripterJobItemForm.UpdateControls;
begin
  inherited UpdateControls();
  cmbLanguage.Enabled := not ReadOnly;
end;

procedure TScripterJobItemForm.cmbLanguageChange(Sender: TObject);
begin
  if IsLoading then Exit;
  IsModified := True;
  UpdateControls();
end;

initialization
  RegisterEditorItem(TScripterJobEditorItem, TScripterDataItem, 'Scripter');

end.
