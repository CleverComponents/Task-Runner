unit CustomDialog;

interface

uses
  Windows, Messages, System.SysUtils, System.Classes, Vcl.Graphics, System.UITypes, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, JobClasses, JobConsts, JobCtrls, JobDskClasses, Winapi.msxml;

type
  TCustomJobEditorItem = class;

  TCustomDialogForm = class(TForm)
    pBottom: TPanel;
    PageControl: TPageControl;
    tabDetails: TTabSheet;
    tabAddition: TTabSheet;
    pAddTop: TPanel;
    Label1: TLabel;
    cmbFlowAction: TJobComboBox;
    MemoDescription: TJobRichEdit;
    Panel1: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    sbDescription: TStatusBar;
    lblCanPerform: TLabel;
    edtCanPerform: TEdit;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure AdditionDataChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
    procedure SetIsRunning(const Value: Boolean);
    function GetIsModified: Boolean;
    function CanCloseForm: Boolean;
    function GetFullJobName: String;
  protected
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoShow; override;
    procedure DoOK; virtual;
    procedure DoCancel; virtual;
    procedure DoApply; virtual;
    procedure UpdateControls; virtual;
    procedure AssignData(IsFromDataItem: Boolean = False); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetJobDskItemClass: TJobDskItemClass; virtual;
    procedure AssignDskData(IsFromDskItem: Boolean; var IsNewDskItem: Boolean); virtual;
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
  protected
    function GetEditorFormClass: TCustomDialogFormClass; virtual; abstract;
    procedure SetReadOnly(const Value: Boolean); override;
  public
    constructor Create(AData: TJobDataItem); override;
    destructor Destroy; override;
    procedure Perform; override;
  end;

  TCustomJobDskItem = class(TJobDskItem)
  private
    FWindowPos: TRect;
    FIsMaximized: Boolean;
  protected
    procedure Load(AStream: TStream; AVersion: Integer); overload; override;
    procedure Store(AStream: TStream); overload; override;
    procedure Load(ANode: IXMLDOMNode); overload; override;
    procedure Store(ANode: IXMLDOMNode); overload; override;
  public
    constructor Create(ADataItemFullName: string); override;
    property WindowPos: TRect read FWindowPos;
    property IsMaximized: Boolean read FIsMaximized write FIsMaximized;
  end;

implementation

{$R *.DFM}

uses
  XMLUtils;

{ TCustomDialogForm }

procedure TCustomDialogForm.DoApply;
begin
  AssignData();
  IsModified := False;
end;

procedure TCustomDialogForm.DoCancel;
begin
  IsModified := False;
  Close();
end;

procedure TCustomDialogForm.DoOK;
begin
  AssignData();
  IsModified := False;
  Close();
end;

procedure TCustomDialogForm.btnApplyClick(Sender: TObject);
begin
  DoApply();
end;

procedure TCustomDialogForm.btnCancelClick(Sender: TObject);
begin
  DoCancel();
end;

procedure TCustomDialogForm.btnOKClick(Sender: TObject);
begin
  DoOK();
end;

procedure TCustomDialogForm.UpdateControls;
var
  S: String;
begin
  MemoDescription.ReadOnly := FReadOnly;
  cmbFlowAction.Enabled := not FReadOnly;
  btnApply.Enabled := (not FReadOnly) and IsModified;
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

procedure TCustomDialogForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  Action := caFree;
  if (FWrapper <> nil) then
  begin
    FWrapper.FForm := nil;
    FWrapper.Free();
  end;
end;

procedure TCustomDialogForm.DoShow;
begin
  inherited DoShow();
  UpdateControls();
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

procedure TCustomDialogForm.DoOnDataStateChange(Sender: TObject);
begin
  IsRunning := (Sender as TJobDataItem).DataState = jsRun;
end;

destructor TCustomDialogForm.Destroy;
var
  b: Boolean;
begin
  AssignDskData(False, b);
  if (FData <> nil) then
  begin
    FData.OnDataStateChange := nil;
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
  CustDskItem: TCustomJobDskItem;
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
      CustDskItem.FWindowPos.Left := Self.Left;
      CustDskItem.FWindowPos.Top := Self.Top;
      CustDskItem.FWindowPos.Right := Self.Left + Self.Width;
      CustDskItem.FWindowPos.Bottom := Self.Top + Self.Height;
    end;
    CustDskItem.IsMaximized := (Self.WindowState = wsMaximized);
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

procedure TCustomDialogForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := 0;
end;

destructor TCustomJobEditorItem.Destroy;
begin
  if (FForm <> nil) then
  begin
    FForm.FWrapper := nil;
    FForm.Free();
  end;
  inherited Destroy();
end;

procedure TCustomJobEditorItem.Perform;
begin
  FForm.Show();
end;

procedure TCustomDialogForm.AdditionDataChange(Sender: TObject);
begin
  if IsLoading then Exit;
  IsModified := True;
end;

procedure TCustomDialogForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = Ord('S')) and (ssCtrl	in Shift) then
  begin
    DoApply();
    Key := 0;
  end;
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

procedure TCustomDialogForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := CanCloseForm();
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
  ZeroMemory(@FWindowPos, SizeOf(TRect));
end;

procedure TCustomJobDskItem.Load(AStream: TStream; AVersion: Integer);
var
  v: Integer;
begin
  inherited Load(AStream, AVersion);
  AStream.Read(FWindowPos.Left, SizeOf(Integer));
  AStream.Read(FWindowPos.Right, SizeOf(Integer));
  AStream.Read(FWindowPos.Top, SizeOf(Integer));
  AStream.Read(FWindowPos.Bottom, SizeOf(Integer));
  AStream.Read(v, SizeOf(Integer));
  FIsMaximized := Boolean(v);
end;

procedure TCustomJobDskItem.Store(AStream: TStream);
var
  v: Integer;
begin
  inherited Store(AStream);
  AStream.Write(FWindowPos.Left, SizeOf(Integer));
  AStream.Write(FWindowPos.Right, SizeOf(Integer));
  AStream.Write(FWindowPos.Top, SizeOf(Integer));
  AStream.Write(FWindowPos.Bottom, SizeOf(Integer));
  v := Integer(FIsMaximized);
  AStream.Write(v, SizeOf(Integer));
end;

procedure TCustomJobDskItem.Load(ANode: IXMLDOMNode);
var
  Store: TStoreFormStruct;
begin
  inherited Load(ANode);
  XMLToForm(Store, ANode);
  FWindowPos.Left := Store.Left;
  FWindowPos.Top := Store.Top;
  FWindowPos.Right := FWindowPos.Left + Store.Width;
  FWindowPos.Bottom := FWindowPos.Top + Store.Height;
  FIsMaximized := Store.Maximized;
end;

procedure TCustomJobDskItem.Store(ANode: IXMLDOMNode);
var
  Store: TStoreFormStruct;
begin
  inherited Store(ANode);
  Store.Width := (FWindowPos.Right - FWindowPos.Left);
  Store.Height := (FWindowPos.Bottom - FWindowPos.Top);
  Store.Left := FWindowPos.Left;
  Store.Top := FWindowPos.Top;
  Store.Maximized := FIsMaximized;
  FormToXML(Store, ANode);
end;

initialization
  RegisterClass(TCustomJobDskItem);

end.
