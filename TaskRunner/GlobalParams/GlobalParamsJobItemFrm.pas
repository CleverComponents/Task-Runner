unit GlobalParamsJobItemFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CustomParametersDialog,
  Db, StdCtrls, ComCtrls, Grids, DBGrids, DBCtrls, ExtCtrls, CustomDialog, JobClasses,
  JobCtrls, OperationClasses, CustomJobItems, Datasnap.DBClient, JobMemData;

type
  TGlobalParamsJobDataItem = class(TCustomParametersJobDataItem)
  end;

  TGlobalParamsJobItemForm = class(TCustomParamsJobItemForm)
  private
    FParameters: TJobOperationParams;
    FJobManager: TJobManager;
    procedure DoBeforeRefresh(DataSet: TDataSet);
    procedure SetParameters(const Value: TJobOperationParams);
  protected
    constructor CreateInstance;
    class function AccessInstance(Request: integer): TGlobalParamsJobItemForm;
    procedure AssignData(IsFromDataItem: Boolean = False); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Instance: TGlobalParamsJobItemForm;
    class procedure ReleaseInstance;
    property Parameters: TJobOperationParams read FParameters write SetParameters;
    property JobManager: TJobManager read FJobManager write FJobManager;
  end;

procedure EditGlobalParameters(AJobManager: TJobManager; Params: TJobOperationParams);

implementation

{$R *.DFM}

uses
  OperationUtils;

procedure EditGlobalParameters(AJobManager: TJobManager; Params: TJobOperationParams);
begin
//  TGlobalParamsJobItemForm.Instance.ReadOnly := AJobManager.IsLocked;
  TGlobalParamsJobItemForm.Instance.JobManager := AJobManager;
  TGlobalParamsJobItemForm.Instance.Parameters := Params;
  TGlobalParamsJobItemForm.Instance.Show();
end;

{ TGlobalParamsJobItemForm }

var
  fInstance: TGlobalParamsJobItemForm = nil;

class function TGlobalParamsJobItemForm.AccessInstance(
  Request: integer): TGlobalParamsJobItemForm;
begin
  case Request of
    0 : ;
    1 : if not Assigned(fInstance) then fInstance := CreateInstance;
    2 : fInstance := nil;
  else raise Exception.CreateFmt('Illegal request %d in AccessInstance', [Request]);
  end;
  Result := fInstance;
end;

procedure TGlobalParamsJobItemForm.AssignData(IsFromDataItem: Boolean);
begin
  if (FParameters = nil) then Exit;
  if IsFromDataItem then
  begin
    TGlobalParamsJobDataItem(Data).Parameters.Assign(FParameters);
  end;
  inherited AssignData(IsFromDataItem);
  if (not IsFromDataItem) then
  begin
    FParameters.Assign(TGlobalParamsJobDataItem(Data).Parameters);
  end;
end;

constructor TGlobalParamsJobItemForm.Create(AOwner: TComponent);
begin
  if (csDesigning in ComponentState) then
  begin
    inherited Create(AOwner);
  end else
  begin
    raise Exception.CreateFmt('Access class %s through Instance only', [ClassName]);
  end;
end;

constructor TGlobalParamsJobItemForm.CreateInstance;
begin
  inherited Create(Application);
  Data := TGlobalParamsJobDataItem.Create(nil);
  Data.JobName := 'Global Parameters Editor';
  MemData.BeforeRefresh := DoBeforeRefresh;
  FParameters := nil;
  FJobManager := nil;
end;

destructor TGlobalParamsJobItemForm.Destroy;
begin
  if AccessInstance(0) = Self then AccessInstance(2);
  Data.Free();
  inherited Destroy();
end;

procedure TGlobalParamsJobItemForm.DoBeforeRefresh(DataSet: TDataSet);
  procedure DoGetParameters(AParams: TJobOperationParams; AItem: TJobItem);
  var
    i: Integer;
  begin
    AItem.Data.GetGlobalParameterList(AParams);
    for i := 0 to AItem.ItemsCount - 1 do
    begin
      DoGetParameters(AParams, AItem.Items[i]);
    end;
  end;

var
  i: Integer;
  Params: TJobOperationParams;
begin
  if IsLoading or (FJobManager = nil) then Exit;
  Params := TJobOperationParams.Create();
  try
    for i := 0 to FJobManager.RootItemsCount - 1 do
    begin
      DoGetParameters(Params, FJobManager.RootItems[i]);
    end;
    LoadMemData(Params, True, False);
    IsModified := True;
  finally
    Params.Free();
  end;
end;

class function TGlobalParamsJobItemForm.Instance: TGlobalParamsJobItemForm;
begin
  Result := AccessInstance(1);
end;

class procedure TGlobalParamsJobItemForm.ReleaseInstance;
begin
  AccessInstance(0).Free();
end;

procedure TGlobalParamsJobItemForm.SetParameters(const Value: TJobOperationParams);
begin
  if (FParameters <> Value) then
  begin
    FParameters := Value;
    IsLoading := True;
    try
      AssignData(True);
    finally
      IsLoading := False;
    end;
  end;
end;

initialization
  RegisterClass(TGlobalParamsJobDataItem);

finalization
  TGlobalParamsJobItemForm.ReleaseInstance();

end.
