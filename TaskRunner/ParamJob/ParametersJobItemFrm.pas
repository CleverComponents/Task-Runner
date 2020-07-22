unit ParametersJobItemFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CustomParametersDialog,
  Db, StdCtrls, ComCtrls, Grids, DBGrids, DBCtrls, ExtCtrls, CustomDialog, JobClasses,
  JobCtrls, Datasnap.DBClient, JobMemData;

type
  TParametersJobItemForm = class(TCustomParamsJobItemForm)
  private
    procedure DoBeforeRefresh(DataSet: TDataSet);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TParametersJobEditorItem = class(TCustomJobEditorItem)
  protected
    function GetEditorFormClass: TCustomDialogFormClass; override;
  end;

implementation

{$R *.DFM}

uses
  ParametersJobItem, OperationClasses;

{ TParametersJobEditorItem }

function TParametersJobEditorItem.GetEditorFormClass: TCustomDialogFormClass;
begin
  Result := TParametersJobItemForm;
end;

{ TParametersJobItemForm }

constructor TParametersJobItemForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MemData.BeforeRefresh := DoBeforeRefresh;
end;

procedure TParametersJobItemForm.DoBeforeRefresh(DataSet: TDataSet);
{  procedure FillJobParams(AJob: TJobItem; AParams: TJobOperationParams);
  var
    i: Integer;
  begin
    for i := 0 to AJob.ItemsCount - 1 do
    begin
      AJob.Items[i].Data.GetParameterList(Params);
      FillJobParams(AJob.Items[i], AParams);
    end;
  end;}

var
  i: Integer;
  AJob: TJobItem;
  Params: TJobOperationParams;
begin
  if IsLoading then Exit;

  Params := TJobOperationParams.Create();
  try
    for i := 0 to Data.Owner.ItemsCount - 1 do
    begin
      AJob := Data.Owner.Items[i];
      AJob.Data.GetParameterList(Params);
    end;
    LoadMemData(Params, True);
    IsModified := True;
  finally
    Params.Free();
  end;
end;

initialization
  RegisterEditorItem(TParametersJobEditorItem, TParametersJobDataItem, 'Parameters');

end.
