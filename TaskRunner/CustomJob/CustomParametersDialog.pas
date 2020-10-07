unit CustomParametersDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, System.Variants, Graphics, Controls, Forms, Dialogs,
  CustomDialog, StdCtrls, ComCtrls, ExtCtrls, JobClasses, Grids, DBGrids,
  DBCtrls, Db, OperationClasses, JobCtrls, Datasnap.DBClient,
  JobMemData;

type
  TCustomParamsJobItemForm = class(TCustomDialogForm)
    Panel2: TPanel;
    List: TDBGrid;
    Navigator: TDBNavigator;
    DataSource: TDataSource;
    MemData: TJobMemData;
    MemDataparamname: TStringField;
    MemDataparamvalue: TStringField;
    procedure DataSourceStateChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MemDataAfterDelete(DataSet: TDataSet);
  protected
    procedure LoadMemData(AParams: TJobOperationParams; IsAppend: Boolean = False; NeedDelete: Boolean = True);
    procedure StoreMemData(AParams: TJobOperationParams);
    procedure AssignData(IsFromDataItem: Boolean = False); override;
    procedure UpdateControls; override;
  end;

implementation

{$R *.DFM}

uses
  CustomJobItems;

procedure TCustomParamsJobItemForm.AssignData(IsFromDataItem: Boolean);
begin
  inherited AssignData(IsFromDataItem);

  if IsFromDataItem then
  begin
    LoadMemData(TCustomParametersJobDataItem(Data).Parameters);
  end else
  begin
    StoreMemData(TCustomParametersJobDataItem(Data).Parameters);
  end;
end;

procedure TCustomParamsJobItemForm.LoadMemData(AParams: TJobOperationParams; IsAppend: Boolean = False;
  NeedDelete: Boolean = True);
var
  i: Integer;
  Param: TJobOperationParam;
  IsReadOnly: Boolean;
begin
  MemData.DisableControls();
  IsReadOnly := MemData.ReadOnly;
  try
    MemData.ReadOnly := False;
    if not IsAppend then
    begin
      MemData.Close();
    end;
    MemData.Open();

    for i := 0 to AParams.Count - 1 do
    begin
      Param := AParams.Items[i];

      if not (IsAppend and MemData.Locate('paramname', Param.Name, [])) then
      begin
        MemData.Append();
        try
          MemDataparamname.AsString := Param.Name;
          MemDataparamvalue.AsString := VarToStr(Param.Value);
          MemData.Post();
        except
          MemData.Cancel();
          raise;
        end;
      end;
    end;

    if IsAppend and NeedDelete then
    begin
      MemData.First();
      while not MemData.Eof do
      begin
        if (AParams.FindParam(MemDataparamname.AsString) = nil) then
        begin
          MemData.Delete();
        end else
        begin
          MemData.Next();
        end;
      end;
    end;
  finally
    MemData.ReadOnly := IsReadOnly;
    MemData.EnableControls();
  end;
end;

procedure TCustomParamsJobItemForm.StoreMemData(AParams: TJobOperationParams);
var
  bm: TBookMark;
begin
  if (MemData.State in dsEditModes) then
  begin
    MemData.Post();
  end;

  MemData.DisableControls();
  try
    if MemData.Active then
    begin
      bm := MemData.GetBookmark;
    end else
    begin
      bm := nil;
    end;

    AParams.Clear();

    MemData.First();
    while not MemData.Eof do
    begin
      AParams.Add(MemDataparamname.AsString, MemDataparamvalue.AsString);
      MemData.Next();
    end;

    if (bm <> nil) then
    begin
      if MemData.BookmarkValid(bm) then
      begin
        try
          MemData.GotoBookmark(bm);
        except
        end;
      end;
      MemData.FreeBookmark(bm);
    end;
  finally
    MemData.EnableControls();
  end;
end;

procedure TCustomParamsJobItemForm.DataSourceStateChange(Sender: TObject);
begin
  if IsLoading then Exit;
  if (DataSource.State in dsEditModes) then
  begin
    IsModified := True;
  end;
end;

procedure TCustomParamsJobItemForm.UpdateControls;
begin
  inherited UpdateControls();
  MemData.ReadOnly := ReadOnly;
end;

procedure TCustomParamsJobItemForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_F8) then
  begin
    MemData.Refresh();
  end;
end;

procedure TCustomParamsJobItemForm.MemDataAfterDelete(DataSet: TDataSet);
begin
  if IsLoading then Exit;
  IsModified := True;
end;

end.
