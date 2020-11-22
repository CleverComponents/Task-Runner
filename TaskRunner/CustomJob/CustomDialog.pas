unit CustomDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, System.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  JobClasses, JobConsts, JobCtrls, JobDskClasses, Winapi.msxml, TabEditors, OperationUtils, OperationClasses;

type
  TCustomJobEditorItem = class;

  TCustomDialogForm = class(TFrame)
    PageControl: TPageControl;
    tabDetails: TTabSheet;
    tabAddition: TTabSheet;
    pAddTop: TPanel;
    Label1: TLabel;
    cmbFlowAction: TJobComboBox;
    MemoDescription: TJobRichEdit;
    sbDescription: TStatusBar;
    lblCanPerform: TLabel;
    edtCanPerform: TEdit;
    procedure AdditionDataChange(Sender: TObject);
    procedure MemoDescriptionSelectionChange(Sender: TObject);
  private
    FIsModified: Boolean;
    FIsRunModified: Boolean;
    FData: TJobDataItem;
    FWrapper: TCustomJobEditorItem;
    FIsLoading: Boolean;
    FReadOnly: Boolean;
    FIsRunning: Boolean;
    FJobName: String;

    procedure SetIsModified(const Value: Boolean);
    procedure SetData(const Value: TJobDataItem);
    procedure SetReadOnly(const Value: Boolean);
    procedure DoOnDataStateChange(Sender: TObject);
    procedure DoOnDataChange(Sender: TObject);
    procedure SetIsRunning(const Value: Boolean);
    function GetIsModified: Boolean;
    function CanCloseForm: Boolean;
    function GetFullJobName: String;
    procedure DoSaveJob(Sender: TObject);
    procedure DoCloseJob(Sender: TObject);
  protected
    procedure DoApply; virtual;
    procedure UpdateControls; virtual;
    procedure AssignData(IsFromDataItem: Boolean = False); virtual;
    function GetJobDskItemClass: TJobDskItemClass; virtual;
    procedure AssignDskData(IsFromDskItem: Boolean; var IsNewDskItem: Boolean); virtual;
    procedure AddOperations(AOperationList: TFormOperationList); virtual;
    procedure RemoveOperations(AOperationList: TFormOperationList); virtual;
    procedure Activate;

    property IsLoading: Boolean read FIsLoading write FIsLoading;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsModified: Boolean read GetIsModified write SetIsModified;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property IsRunning: Boolean read FIsRunning write SetIsRunning;
    property Data: TJobDataItem read FData write SetData;
  end;

  TCustomDialogFormClass = class of TCustomDialogForm;

  TCustomJobEditorItem = class(TJobEditorItem)
  private
    FForm: TCustomDialogForm;
    FTabManager: TTabEditorsManager;
  protected
    function GetEditorFormClass: TCustomDialogFormClass; virtual; abstract;
    procedure SetReadOnly(const Value: Boolean); override;
  public
    constructor Create(AData: TJobDataItem); override;
    destructor Destroy; override;
    procedure Perform; override;

    property TabManager: TTabEditorsManager read FTabManager write FTabManager;
  end;

  TCustomJobDskItem = class(TJobDskItem)
  private
    FIsMaximized: Boolean;
  protected
    procedure Load(AStream: TStream; AVersion: Integer); overload; override;
    procedure Store(AStream: TStream); overload; override;
    procedure Load(ANode: IXMLDOMNode); overload; override;
    procedure Store(ANode: IXMLDOMNode); overload; override;
  public
    WindowPos: TRect;

    constructor Create(ADataItemFullName: string); override;
    property IsMaximized: Boolean read FIsMaximized write FIsMaximized;
  end;

implementation

{$R *.DFM}

uses
  XMLUtils;

{ TCustomDialogForm }

procedure TCustomDialogForm.Activate;
begin
  if (TJobOperationManager.Instance.CurrentOperationList <> nil) then
  begin
    RemoveOperations(TJobOperationManager.Instance.CurrentOperationList);
    AddOperations(TJobOperationManager.Instance.CurrentOperationList);
  end;

  UpdateControls();
end;

procedure TCustomDialogForm.DoApply;
begin
  AssignData();
  IsModified := False;
end;

procedure TCustomDialogForm.DoCloseJob(Sender: TObject);
begin
  if CanCloseForm() then
  begin
    FWrapper.Free();
  end;
end;

procedure TCustomDialogForm.UpdateControls;
var
  S: String;
begin
  MemoDescription.ReadOnly := FReadOnly;
  cmbFlowAction.Enabled := not FReadOnly;

  if (TJobOperationManager.Instance.CurrentOperationList <> nil) then
  begin
    TJobOperationManager.Instance.CurrentOperationList.EnableOperation(opSaveJob, (not FReadOnly) and IsModified);
    TJobOperationManager.Instance.CurrentOperationList.EnableOperation(opCloseJob, True);
  end;

  edtCanPerform.Enabled := not FReadOnly;
  S := '';
  if FReadOnly then
  begin
    if (S = '') then
    begin
      S := S + '(';
    end;
    S := S + 'ReadOnly';
  end;
  if IsRunning then
  begin
    if (S = '') then
    begin
      S := S + '(';
    end else
    begin
      S := S + ', ';
    end;
    S := S + 'Running';
  end;
  if (S <> '') then
  begin
    S := S + ')';
  end;
  Caption := GetFullJobName() + ' ' + S;
end;

function TCustomDialogForm.GetFullJobName: String;
  function GetOwnerJobName(AItem: TJobItem): String;
  var
    AOwner: TJobItem;
  begin
    AOwner := AItem.Owner;
    if (AOwner <> nil) then
    begin
      Result := GetOwnerJobName(AOwner);
      if (Result <> '') then
      begin
        Result := Result + '\';
      end;
      Result := Result + AOwner.JobName;
    end else
    begin
      Result := '';
    end;
  end;

begin
  if (Data.Owner <> nil) then
  begin
    Result := GetOwnerJobName(Data.Owner);
  end else
  begin
    Result := '';
  end;
  if (Result <> '') then
  begin
    Result := Result + '\';
  end;
  Result := Result + FJobName;
end;

procedure TCustomDialogForm.SetIsModified(const Value: Boolean);
begin
  if (FIsModified <> Value) then
  begin
    if IsRunning then
    begin
      FIsRunModified := Value;
    end else
    begin
      FIsModified := Value;
      if (FData <> nil) then
      begin
        if FIsModified then
        begin
          FData.DataState := jsEdited;
        end else
        begin
          FData.DataState := jsNormal;
        end;
      end;
    end;

    FWrapper.TabManager.UpdateEditor(FWrapper, FIsModified);

    UpdateControls();
  end;
end;

procedure TCustomDialogForm.SetData(const Value: TJobDataItem);
begin
  if (FData <> Value) then
  begin
    if (FData <> nil) and (FData.DataState = jsEdited) then
    begin
      raise Exception.Create(cJobDataLocked);
    end;
    FData := Value;
    IsLoading := True;
    try
      AssignData(True);
    finally
      IsLoading := False;
    end;
    if (FData <> nil) then
    begin
      FData.OnDataStateChange := DoOnDataStateChange;
      FData.OnDataChange := DoOnDataChange;
    end;
  end;
end;

procedure TCustomDialogForm.AssignData(IsFromDataItem: Boolean);
begin
  if (FData = nil) then
  begin
    raise Exception.Create('The Data item is not set');
  end;
  if IsFromDataItem then
  begin
    FJobName := FData.JobName;
    cmbFlowAction.ItemIndex := Integer(FData.FlowAction);
    MemoDescription.Lines.Assign(FData.Description);
    edtCanPerform.Text := FData.CanPerform;
    IsRunning := (FData.DataState = jsRun);
  end else
  begin
    if IsRunning then
    begin
      raise Exception.Create(cJobRunning);
    end;
    FData.FlowAction := TFlowAction(cmbFlowAction.ItemIndex);
    FData.Description.Assign(MemoDescription.Lines);
    FData.CanPerform := Trim(edtCanPerform.Text);
  end;
end;

constructor TCustomDialogForm.Create(AOwner: TComponent);
var
  i: TFlowAction;
begin
  inherited Create(AOwner);
  FReadOnly := False;
  FIsLoading := False;
  cmbFlowAction.Items.Clear();
  for i := Low(cFlowActionNames) to High(cFlowActionNames) do
  begin
    cmbFlowAction.Items.Add(cFlowActionNames[i]);
  end;
  FIsModified := False;
  FIsRunModified := False;
  FData := nil;
  PageControl.ActivePage := tabDetails;
end;

procedure TCustomDialogForm.SetReadOnly(const Value: Boolean);
begin
  if (FReadOnly <> Value) then
  begin
    FReadOnly := Value;
    UpdateControls();
  end;
end;

procedure TCustomDialogForm.DoOnDataChange(Sender: TObject);
begin
  FWrapper.TabManager.UpdateEditor(FWrapper, FIsModified);
end;

procedure TCustomDialogForm.DoOnDataStateChange(Sender: TObject);
begin
  IsRunning := (Sender as TJobDataItem).DataState = jsRun;
end;

procedure TCustomDialogForm.DoSaveJob(Sender: TObject);
begin
  DoApply();
end;

destructor TCustomDialogForm.Destroy;
var
  b: Boolean;
begin
  if (TJobOperationManager.Instance.CurrentOperationList <> nil) then
  begin
    RemoveOperations(TJobOperationManager.Instance.CurrentOperationList);
  end;

  AssignDskData(False, b);

  if (FWrapper <> nil) then
  begin
    FWrapper.FForm := nil;
    FWrapper.Free();
  end;

  if (FData <> nil) then
  begin
    FData.OnDataStateChange := nil;
    FData.OnDataChange := nil;
  end;

  inherited Destroy();
end;

procedure TCustomDialogForm.SetIsRunning(const Value: Boolean);
begin
  if (FIsRunning <> Value) then
  begin
    FIsRunning := Value;
    if not FIsRunning then
    begin
      ISModified := FIsRunModified;
    end;
    UpdateControls();
  end;
end;

function TCustomDialogForm.GetIsModified: Boolean;
begin
  Result := FIsRunModified or FIsModified;
end;

procedure TCustomDialogForm.AssignDskData(IsFromDskItem: Boolean; var IsNewDskItem: Boolean);
var
  fullname: string;
  DskItem: TJobDskItem;
  Storage: TJobDskItemStorage;
begin
  Storage := TJobDskMediaStorage.Instance.CurrentItem;
  if (Storage = nil) then Exit;
  fullname := GetFullJobName();
  DskItem := Storage.GetItem(GetJobDskItemClass(), fullname);
  IsNewDskItem := (DskItem = nil);
  if IsNewDskItem then
  begin
    DskItem := GetJobDskItemClass().Create(fullname);
    Storage.PutItem(DskItem);
  end;
end;

function TCustomDialogForm.GetJobDskItemClass: TJobDskItemClass;
begin
  Result := TCustomJobDskItem;
end;

{ TCustomJobEditorItem }

constructor TCustomJobEditorItem.Create(AData: TJobDataItem);
var
  b: Boolean;
begin
  inherited Create(AData);

  FForm := GetEditorFormClass().Create(nil);
  FForm.Data := AData;
  FForm.FWrapper := Self;
  FForm.AssignDskData(True, b);
end;

destructor TCustomJobEditorItem.Destroy;
begin
  if (FForm <> nil) then
  begin
    FForm.Parent := nil;
    FForm.FWrapper := nil;
    FForm.Free();
  end;

  TabManager.RemoveEditor(Self);

  inherited Destroy();
end;

procedure TCustomJobEditorItem.Perform;
begin
  FForm.IsLoading := True;
  try
    TabManager.AddEditor(Self, FForm);
    FForm.Activate();
  finally
    FForm.IsLoading := False;
  end;
end;

procedure TCustomDialogForm.AdditionDataChange(Sender: TObject);
begin
  if IsLoading then Exit;
  IsModified := True;
end;

procedure TCustomDialogForm.AddOperations(AOperationList: TFormOperationList);
begin
  AOperationList.AddOperation(opSaveJob, DoSaveJob);
  AOperationList.AddOperation(opCloseJob, DoCloseJob);
end;

procedure TCustomDialogForm.RemoveOperations(AOperationList: TFormOperationList);
begin
  AOperationList.RemoveOperation(opSaveJob);
  AOperationList.RemoveOperation(opCloseJob);
end;

function TCustomDialogForm.CanCloseForm: Boolean;
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

procedure TCustomJobEditorItem.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly(Value);
  FForm.ReadOnly := Value;
end;

procedure TCustomDialogForm.MemoDescriptionSelectionChange(Sender: TObject);
begin
  sbDescription.Panels[0].Text :=
    Format(cCursorPositionMask, [IntToStr(memoDescription.CaretPos.y + 1), IntToStr(memoDescription.CaretPos.x + 1)]);
end;

{ TCustomJobDskItem }

constructor TCustomJobDskItem.Create(ADataItemFullName: string);
begin
  inherited Create(ADataItemFullName);
  ZeroMemory(@WindowPos, SizeOf(TRect));
end;

procedure TCustomJobDskItem.Load(AStream: TStream; AVersion: Integer);
var
  v: Integer;
begin
  inherited Load(AStream, AVersion);
  AStream.Read(WindowPos.Left, SizeOf(Integer));
  AStream.Read(WindowPos.Right, SizeOf(Integer));
  AStream.Read(WindowPos.Top, SizeOf(Integer));
  AStream.Read(WindowPos.Bottom, SizeOf(Integer));
  AStream.Read(v, SizeOf(Integer));
  FIsMaximized := Boolean(v);
end;

procedure TCustomJobDskItem.Store(AStream: TStream);
var
  v: Integer;
begin
  inherited Store(AStream);
  AStream.Write(WindowPos.Left, SizeOf(Integer));
  AStream.Write(WindowPos.Right, SizeOf(Integer));
  AStream.Write(WindowPos.Top, SizeOf(Integer));
  AStream.Write(WindowPos.Bottom, SizeOf(Integer));
  v := Integer(FIsMaximized);
  AStream.Write(v, SizeOf(Integer));
end;

procedure TCustomJobDskItem.Load(ANode: IXMLDOMNode);
var
  Store: TStoreFormStruct;
begin
  inherited Load(ANode);
  XMLToForm(Store, ANode);
  WindowPos.Left := Store.Left;
  WindowPos.Top := Store.Top;
  WindowPos.Right := WindowPos.Left + Store.Width;
  WindowPos.Bottom := WindowPos.Top + Store.Height;
  FIsMaximized := Store.Maximized;
end;

procedure TCustomJobDskItem.Store(ANode: IXMLDOMNode);
var
  Store: TStoreFormStruct;
begin
  inherited Store(ANode);
  Store.Width := (WindowPos.Right - WindowPos.Left);
  Store.Height := (WindowPos.Bottom - WindowPos.Top);
  Store.Left := WindowPos.Left;
  Store.Top := WindowPos.Top;
  Store.Maximized := FIsMaximized;
  FormToXML(Store, ANode);
end;

initialization
  RegisterClass(TCustomJobDskItem);

end.
