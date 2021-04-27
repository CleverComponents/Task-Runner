unit GlobalParamsJobItemFrm;

interface

uses
  Windows, Messages, System.SysUtils, System.Classes, Vcl.Graphics, System.UITypes, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, JobClasses, JobConsts, JobCtrls, JobDskClasses,
  Winapi.msxml, Data.DB, Vcl.Grids, Vcl.DBGrids, Vcl.DBCtrls, Datasnap.DBClient, System.Variants,
  CustomDialog, JobMemData, CustomJobItems, OperationClasses;

type
  TGlobalParamsJobItemForm = class(TForm)
    pBottom: TPanel;
    Panel1: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    pMain: TPanel;
    Panel2: TPanel;
    Navigator: TDBNavigator;
    List: TDBGrid;
    DataSource: TDataSource;
    MemData: TJobMemData;
    MemDataparamname: TStringField;
    MemDataparamvalue: TStringField;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MemDataAfterDelete(DataSet: TDataSet);
    procedure DataSourceStateChange(Sender: TObject);
  private
    FReadOnly: Boolean;
    FIsModified: Boolean;
    FIsLoading: Boolean;
    FParameters: TJobOperationParams;
    FJobManager: TJobManager;

    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure SetIsModified(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    function CanCloseForm: Boolean;
    procedure SetParameters(const Value: TJobOperationParams);
    procedure DoBeforeRefresh(DataSet: TDataSet);
  protected
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoShow; override;
    procedure DoOK; virtual;
    procedure DoCancel; virtual;
    procedure DoApply; virtual;
    procedure UpdateControls; virtual;
    procedure AssignData(IsFromDataItem: Boolean = False); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;
    function GetJobDskItemClass: TJobDskItemClass; virtual;
    procedure AssignDskData(IsFromDskItem: Boolean; var IsNewDskItem: Boolean); virtual;
    procedure LoadMemData(AParams: TJobOperationParams; IsAppend: Boolean = False; NeedDelete: Boolean = True);
    procedure StoreMemData(AParams: TJobOperationParams);

    constructor CreateInstance;
    class function AccessInstance(Request: integer): TGlobalParamsJobItemForm;

    property IsLoading: Boolean read FIsLoading write FIsLoading;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function Instance: TGlobalParamsJobItemForm;
    class procedure ReleaseInstance;

    class procedure EditGlobalParameters(AJobManager: TJobManager; Params: TJobOperationParams);

    property Parameters: TJobOperationParams read FParameters write SetParameters;
    property JobManager: TJobManager read FJobManager write FJobManager;
    property IsModified: Boolean read FIsModified write SetIsModified;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

implementation

{$R *.DFM}

uses
  XMLUtils;

{ TCustomDialogForm }

procedure TGlobalParamsJobItemForm.DoApply;
begin
  AssignData();
  IsModified := False;
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

procedure TGlobalParamsJobItemForm.DoCancel;
begin
  IsModified := False;
  Close();
end;

procedure TGlobalParamsJobItemForm.DoOK;
begin
  AssignData();
  IsModified := False;
  Close();
end;

procedure TGlobalParamsJobItemForm.btnApplyClick(Sender: TObject);
begin
  DoApply();
end;

procedure TGlobalParamsJobItemForm.btnCancelClick(Sender: TObject);
begin
  DoCancel();
end;

procedure TGlobalParamsJobItemForm.btnOKClick(Sender: TObject);
begin
  DoOK();
end;

procedure TGlobalParamsJobItemForm.UpdateControls;
var
  S: String;
begin
  btnApply.Enabled := (not FReadOnly) and IsModified;
  S := '';
  if FReadOnly then
  begin
    if (S = '') then
    begin
      S := S + '(';
    end;
    S := S + 'ReadOnly';
  end;
  if (S <> '') then
  begin
    S := S + ')';
  end;
  Caption := cGlobalParamsEditor + ' ' + S;
  MemData.ReadOnly := ReadOnly;
end;

procedure TGlobalParamsJobItemForm.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if (Message.Active <> WA_INACTIVE) then
  begin
    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  end;
end;

procedure TGlobalParamsJobItemForm.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if (AccessInstance(0) = nil) then Exit;
  if Visible and (Message.Msg = WM_ACTIVATEAPP) then
  begin
    if (BOOL(Message.WParam) = false)then
    begin
      SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
    end
    else
    begin
      if IsWindowEnabled(Handle) then
        SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE{ or SWP_NOACTIVATE});
    end;
  end;
end;

procedure TGlobalParamsJobItemForm.SetIsModified(const Value: Boolean);
begin
  if (FIsModified <> Value) then
  begin
    FIsModified := Value;
    UpdateControls();
  end;
end;

procedure TGlobalParamsJobItemForm.AssignData(IsFromDataItem: Boolean);
begin
  if (Parameters = nil) then Exit;

  if IsFromDataItem then
  begin
    LoadMemData(Parameters);
  end else
  begin
    StoreMemData(Parameters);
  end;
end;

procedure TGlobalParamsJobItemForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  Action := caFree;
end;

procedure TGlobalParamsJobItemForm.DoShow;
begin
  inherited DoShow();
  UpdateControls();
end;

class procedure TGlobalParamsJobItemForm.EditGlobalParameters(
  AJobManager: TJobManager; Params: TJobOperationParams);
begin
//  TGlobalParamsJobItemForm.Instance.ReadOnly := AJobManager.IsLocked;
  TGlobalParamsJobItemForm.Instance.JobManager := AJobManager;
  TGlobalParamsJobItemForm.Instance.Parameters := Params;
  TGlobalParamsJobItemForm.Instance.Show();
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
var
  b: Boolean;
begin
  inherited Create(Application);

  MemData.BeforeRefresh := DoBeforeRefresh;
  FParameters := nil;
  FJobManager := nil;
  FReadOnly := False;
  FIsLoading := False;
  FIsModified := False;

  AssignDskData(True, b);
end;

procedure TGlobalParamsJobItemForm.SetReadOnly(const Value: Boolean);
begin
  if (FReadOnly <> Value) then
  begin
    FReadOnly := Value;
    UpdateControls();
  end;
end;

procedure TGlobalParamsJobItemForm.StoreMemData(AParams: TJobOperationParams);
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

procedure TGlobalParamsJobItemForm.DataSourceStateChange(Sender: TObject);
begin
  if IsLoading then Exit;
  if (DataSource.State in dsEditModes) then
  begin
    IsModified := True;
  end;
end;

destructor TGlobalParamsJobItemForm.Destroy;
var
  b: Boolean;
begin
  AssignDskData(False, b);
  if AccessInstance(0) = Self then AccessInstance(2);

  inherited Destroy();
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

procedure TGlobalParamsJobItemForm.AssignDskData(IsFromDskItem: Boolean; var IsNewDskItem: Boolean);
var
  fullname: string;
  DskItem: TJobDskItem;
  CustDskItem: TCustomJobDskItem;
  Storage: TJobDskItemStorage;
begin
  Storage := TJobDskMediaStorage.Instance.CurrentItem;
  if (Storage = nil) then Exit;
  fullname := cGlobalParamsEditor;
  DskItem := Storage.GetItem(GetJobDskItemClass(), fullname);
  IsNewDskItem := (DskItem = nil);
  if IsNewDskItem then
  begin
    DskItem := GetJobDskItemClass().Create(fullname);
    Storage.PutItem(DskItem);
  end;
  if not (DskItem is TCustomJobDskItem) then Exit;

  CustDskItem := TCustomJobDskItem(DskItem);
  if IsFromDskItem and (not IsNewDskItem) then
  begin
    Self.Left := CustDskItem.WindowPos.Left;
    Self.Top := CustDskItem.WindowPos.Top;
    Self.Width := CustDskItem.WindowPos.Right - CustDskItem.WindowPos.Left;
    Self.Height := CustDskItem.WindowPos.Bottom - CustDskItem.WindowPos.Top;
    if CustDskItem.IsMaximized then
    begin
      Self.WindowState := wsMaximized;
    end;
  end else
  begin
    if (Self.WindowState = wsNormal) then
    begin
      CustDskItem.WindowPos.Left := Self.Left;
      CustDskItem.WindowPos.Top := Self.Top;
      CustDskItem.WindowPos.Right := Self.Left + Self.Width;
      CustDskItem.WindowPos.Bottom := Self.Top + Self.Height;
    end;
    CustDskItem.IsMaximized := (Self.WindowState = wsMaximized);
  end;
end;

function TGlobalParamsJobItemForm.GetJobDskItemClass: TJobDskItemClass;
begin
  Result := TCustomJobDskItem;
end;

class function TGlobalParamsJobItemForm.Instance: TGlobalParamsJobItemForm;
begin
  Result := AccessInstance(1);
end;

procedure TGlobalParamsJobItemForm.LoadMemData(AParams: TJobOperationParams;
  IsAppend, NeedDelete: Boolean);
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

procedure TGlobalParamsJobItemForm.MemDataAfterDelete(DataSet: TDataSet);
begin
  if IsLoading then Exit;
  IsModified := True;
end;

procedure TGlobalParamsJobItemForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := Application.MainForm.Handle;
end;

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

procedure TGlobalParamsJobItemForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = Ord('S')) and (ssCtrl	in Shift) then
  begin
    DoApply();
    Key := 0;
  end;
  if (Key = VK_F8) then
  begin
    MemData.Refresh();
  end;
end;

function TGlobalParamsJobItemForm.CanCloseForm: Boolean;
begin
  Result := True;
  if IsModified then
  begin
    case MessageDlg(cJobModifiedQuery, mtWarning, mbYesNoCancel, 0) of
      mrYES:
        begin
          AssignData();
          IsModified := False;
        end;
      mrNO: IsModified := False;
      else Result := False;
    end;
  end;
end;

procedure TGlobalParamsJobItemForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := CanCloseForm();
end;

class procedure TGlobalParamsJobItemForm.ReleaseInstance;
begin
  AccessInstance(0).Free();
end;

initialization

finalization
  TGlobalParamsJobItemForm.ReleaseInstance();

end.
