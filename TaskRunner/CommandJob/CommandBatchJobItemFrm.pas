unit CommandBatchJobItemFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CustomScriptDialog, CustomDialog, StdCtrls, ComCtrls, ExtCtrls, JobClasses,
  JobCtrls;

type
  TCommandBatchJobItemForm = class(TCustomScriptJobItemForm)
  protected
    procedure UpdateControls; override;
  end;

  TCommandBatchJobEditorItem = class(TCustomJobEditorItem)
  protected
    function GetEditorFormClass: TCustomDialogFormClass; override;
  end;

implementation

{$R *.DFM}

uses
  CommandBatchJobItem;

{ TCommandBatchJobItem }

function TCommandBatchJobEditorItem.GetEditorFormClass: TCustomDialogFormClass;
begin
  Result := TCommandBatchJobItemForm;
end;

{ TCommandBatchJobItemForm }

procedure TCommandBatchJobItemForm.UpdateControls;
begin
  inherited UpdateControls();
  EnableControl(edtScriptFile, False);
  EnableControl(edtLogFile, False);
  chkScriptFile.Enabled := False;
  chkLogFile.Enabled := False;
end;

initialization
  RegisterEditorItem(TCommandBatchJobEditorItem, TCommandBatchDataItem, 'Command Batch');

end.
