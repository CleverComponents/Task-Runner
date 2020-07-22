unit CallJobItemFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CustomParametersDialog, StdCtrls, ComCtrls, ExtCtrls, CustomDialog, JobClasses,
  Db, Grids, DBGrids, DBCtrls, JobCtrls, Datasnap.DBClient, JobMemData;

type
  TCallJobItemForm = class(TCustomParamsJobItemForm)
    Label2: TLabel;
    cmbCallJob: TComboBox;
    btnEditJob: TButton;
    edtMediaName: TEdit;
    lblProject: TLabel;
    procedure cmbCallJobDropDown(Sender: TObject);
    procedure cmbCallJobChange(Sender: TObject);
    procedure btnEditJobClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtMediaNameChange(Sender: TObject);
  private
    procedure DoFillCallJobs;
    procedure DoBeforeRefresh(DataSet: TDataSet);
    procedure DoEditDefinedJob;
  protected
    procedure AssignData(IsFromDataItem: Boolean = False); override;
    procedure UpdateControls; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCallJobEditorItem = class(TCustomJobEditorItem)
  protected
    function GetEditorFormClass: TCustomDialogFormClass; override;
  end;

implementation

{$R *.DFM}

uses
  CallJobItem, OperationClasses, JobConsts;

{ TCallJobEditorItem }

function TCallJobEditorItem.GetEditorFormClass: TCustomDialogFormClass;
begin
  Result := TCallJobItemForm;
end;

procedure TCallJobItemForm.DoFillCallJobs();
var
  i: Integer;
  JobMgr: TJobManager;
  Job: TJobItem;
begin
  cmbCallJob.Items.BeginUpdate();
  try
    cmbCallJob.Items.Clear();
    JobMgr := TCallJobDataItem(Data).GetJobManager(edtMediaName.Text);
    
    for i := 0 to JobMgr.RootItemsCount - 1 do
    begin
      Job := JobMgr.RootItems[i];
      if (Job.Data <> Data) then
      begin
        cmbCallJob.Items.Add(Job.JobName);
      end;
    end;
  finally
    cmbCallJob.Items.EndUpdate();
  end;
end;

procedure TCallJobItemForm.cmbCallJobDropDown(Sender: TObject);
begin
  DoFillCallJobs();
end;

procedure TCallJobItemForm.cmbCallJobChange(Sender: TObject);
begin
  if IsLoading then Exit;
  MemData.Close();
  MemData.Open();
  IsModified := True;
  UpdateControls();
end;

procedure TCallJobItemForm.AssignData(IsFromDataItem: Boolean);
begin
  inherited AssignData(IsFromDataItem);

  if IsFromDataItem then
  begin
    cmbCallJob.Text := TCallJobDataItem(Data).CallJobName;
    edtMediaName.Text := TCallJobDataItem(Data).CallMediaName;
  end else
  begin
    TCallJobDataItem(Data).CallJobName := cmbCallJob.Text;
    TCallJobDataItem(Data).CallMediaName := edtMediaName.Text;
  end;
end;

procedure TCallJobItemForm.DoBeforeRefresh(DataSet: TDataSet);
var
  AJob: TJobItem;
  Params: TJobOperationParams;
begin
  if IsLoading or (cmbCallJob.Text = '') then Exit;
  AJob := TCallJobDataItem(Data).GetCallJobItem(cmbCallJob.Text, edtMediaName.Text);

  if (AJob <> nil) then
  begin
    Params := TJobOperationParams.Create();
    try
      AJob.Data.GetParameterList(Params);
      LoadMemData(Params, True);
      IsModified := True;
    finally
      Params.Free();
    end;
  end else
  begin
    MemData.Close();
    MemData.Open();
  end;
end;

constructor TCallJobItemForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MemData.BeforeRefresh := DoBeforeRefresh;
end;

procedure TCallJobItemForm.UpdateControls;
begin
  inherited UpdateControls();
  btnEditJob.Enabled := (cmbCallJob.Text <> '');
  cmbCallJob.Enabled := not ReadOnly;
end;

procedure TCallJobItemForm.DoEditDefinedJob();
var
  AJob: TJobItem;
  AMediaName, AName: String;
begin
  AMediaName := edtMediaName.Text;
  AName := cmbCallJob.Text;
  AJob := TCallJobDataItem(Data).FindCallJobItem(AName, AMediaName);
  TCallJobDataItem(Data).GetJobManager(AMediaName).EditJobItem(AJob, AMediaName <> '');
end;

procedure TCallJobItemForm.btnEditJobClick(Sender: TObject);
begin
  DoEditDefinedJob();
end;

procedure TCallJobItemForm.ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (not (Sender as TDBGrid).EditorMode and (Key in [VK_DELETE, VK_INSERT]))
    or ((Key = VK_DOWN) and (MemData.RecNo = MemData.RecordCount)) then
  begin
    Key := 0;
  end;
end;

procedure TCallJobItemForm.edtMediaNameChange(Sender: TObject);
begin
  if IsLoading then Exit;
  IsModified := True;
  cmbCallJob.Clear();
end;

initialization
  RegisterEditorItem(TCallJobEditorItem, TCallJobDataItem, 'Call Job');

end.
