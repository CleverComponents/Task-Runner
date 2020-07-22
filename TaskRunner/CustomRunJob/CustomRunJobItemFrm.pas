unit CustomRunJobItemFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CustomScriptDialog, StdCtrls, JobCtrls, ComCtrls, ExtCtrls, JobClasses, CustomDialog;

type
  TCustomRunJobItemForm = class(TCustomScriptJobItemForm)
    Panel2: TPanel;
    edtCommandLine: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    edtParamPrefix: TEdit;
    Label6: TLabel;
    edtParamDelimiter: TEdit;
    procedure edtCommandLineChange(Sender: TObject);
  protected
    procedure UpdateControls; override;
    procedure AssignData(IsFromDataItem: Boolean = False); override;
  end;

  TCustomRunJobEditorItem = class(TCustomJobEditorItem)
  protected
    function GetEditorFormClass: TCustomDialogFormClass; override;
  end;

implementation

uses
  CustomRunJobItem;

{$R *.DFM}

{ TCustomRunJobEditorItem }

function TCustomRunJobEditorItem.GetEditorFormClass: TCustomDialogFormClass;
begin
  Result := TCustomRunJobItemForm;
end;

procedure TCustomRunJobItemForm.AssignData(IsFromDataItem: Boolean);
begin
  inherited AssignData(IsFromDataItem);
  if IsFromDataItem then
  begin
    edtCommandLine.Text := TCustomRunDataItem(Data).CommandLine;
    edtParamPrefix.Text := TCustomRunDataItem(Data).ParamPrefix;
    edtParamDelimiter.Text := TCustomRunDataItem(Data).ParamDelimiter;
//TODO    edtScriptExt.Text := TCustomRunDataItem(Data).ScriptExt;
  end else
  begin
    TCustomRunDataItem(Data).CommandLine := edtCommandLine.Text;
    TCustomRunDataItem(Data).ParamPrefix := edtParamPrefix.Text;
    TCustomRunDataItem(Data).ParamDelimiter := edtParamDelimiter.Text;
//TODO    TCustomRunDataItem(Data).ScriptExt := edtScriptExt.Text;
  end;
end;

procedure TCustomRunJobItemForm.edtCommandLineChange(Sender: TObject);
begin
  if IsLoading then Exit;
  IsModified := True;
  UpdateControls();
end;

procedure TCustomRunJobItemForm.UpdateControls;
begin
  inherited UpdateControls();
  edtCommandLine.Enabled := not ReadOnly;
  edtParamPrefix.Enabled := not ReadOnly;
  edtParamDelimiter.Enabled := not ReadOnly;
//TODO  edtScriptExt.Enabled := not ReadOnly;
  EnableControl(edtScriptFile, False);
  EnableControl(edtLogFile, False);
  chkScriptFile.Enabled := False;
  chkLogFile.Enabled := False;
end;

initialization
  RegisterEditorItem(TCustomRunJobEditorItem, TCustomRunDataItem, 'Custom Run');

end.
