unit OperationUtils;

interface

uses
  Classes, Menus, OperationClasses, ComCtrls, contnrs;

const
  opNewMedia = 'opNewMedia';
  opLoadMedia = 'opLoadMedia';
  opSaveMedia = 'opSaveMedia';
  opSaveMediaAs = 'opSaveMediaAs';
  opSaveJob = 'opSaveJob';
  opCloseJob = 'opCloseJob';
  opAddJobItem = 'opAddJobItem';
  opAddJobSubItem = 'opAddJobSubItem';
  opEditJobItem = 'opEditJobItem';
  opDeleteJobItem = 'opDeleteJobItem';
  opStartJobAt = 'opStartJobAt';
  opStopSelectedJob = 'opStopSelectedJob';
  opStopAllJobs = 'opStopAllJobs';
  opCutJob = 'opCutJob';
  opCopyJob = 'opCopyJob';
  opPasteJob = 'opPasteJob';
  opImportJob = 'opImportJob';
  opExportJob = 'opExportJob';
  opEnableJob = 'opEnableJob';
  opDisableJob = 'opDisableJob';
  opShowReferences = 'opShowReferences';
  gopLoadLastMedia = 'gopLoadLastMedia';

  gopLoadLastMediaFileName = 'FileName';

type
  TFormOperationList = class(TJobOperationList)
  public
    constructor Create();
    procedure AddOperationToPopup(APopupMenu: TPopupMenu; AOperationType: string; ABeginGroup: Boolean = False);
  end;

  TJobOperationManagerItem = class
  private
    FMenuItem: TMenuItem;
    FToolMenuItem: TToolButton;
    FOperationType: String;
  end;

  TJobGlobalOperationEvent = procedure (Params: TJobOperationParams; var Success: Boolean) of object;

  TJobOperationManager = class
  private
    FGlobalList: TObjectList;
    FList: TObjectList;
    FCurrentOperationList: TFormOperationList;
    function GetManagerItemByOperation(AOperationType: String): TJobOperationManagerItem;
    procedure PerformOperationByItem(Sender: TObject);
    procedure OperationEnabledEvent(Sender: TJobOperationItem);
    procedure OperationVisibleEvent(Sender: TJobOperationItem);

    procedure OperationListAddOperation(AItem: TJobOperationItem);
    procedure OperationListRemoveOperation(AItem: TJobOperationItem);
    procedure SetCurrentOperationList(const Value: TFormOperationList);
    procedure ShowMenuItems(AList: TFormOperationList; IsShow: Boolean);
    procedure InitMenuItems;
    function GetGlobalOperationEvent(AName: String): TJobGlobalOperationEvent;
  protected
    constructor CreateInstance(ADummy: Integer = 0);
    class function AccessInstance(Request: Integer): TJobOperationManager;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TJobOperationManager;
    class procedure ReleaseInstance;

    function PerformGlobalOperation(AName: String; Params: TJobOperationParams = nil): Boolean;
    procedure RegisterGlobalOperation(AName: String; AEvent: TJobGlobalOperationEvent);
    procedure UnRegisterGlobalOperation(AName: String);

    procedure RegisterMenuItem(AOperationType: String; AMenuItem: TMenuItem; AToolMenuItem: TToolButton);
    procedure UnRegisterMenuItem(AOperationType: String);
    procedure PerformOperation(AOperationType: String; Sender: TObject = nil);

    property CurrentOperationList: TFormOperationList read FCurrentOperationList write SetCurrentOperationList;
  end;

procedure RegisterMenuItem(AOperationType: String; AMenuItem: TMenuItem; AToolMenuItem: TToolButton = nil);
procedure PerformOperation(AOperationType: String; Sender: TObject = nil);
function PerformGlobalOperation(AName: String; Params: TJobOperationParams = nil): Boolean;
procedure RegisterGlobalOperation(AName: String; AEvent: TJobGlobalOperationEvent);
procedure UnRegisterGlobalOperation(AName: String);

implementation

uses
  SysUtils;

function PerformGlobalOperation(AName: String; Params: TJobOperationParams = nil): Boolean;
begin
  Result := TJobOperationManager.Instance.PerformGlobalOperation(AName, Params);
end;

procedure RegisterGlobalOperation(AName: String; AEvent: TJobGlobalOperationEvent);
begin
  TJobOperationManager.Instance.RegisterGlobalOperation(AName, AEvent);
end;

procedure UnRegisterGlobalOperation(AName: String);
begin
  TJobOperationManager.Instance.UnRegisterGlobalOperation(AName);
end;

type
  TJobGlobalOperationRecord = class
    Name: String;
    Event: TJobGlobalOperationEvent;
  end;
  
{ TJobOperationManager }

procedure RegisterMenuItem(AOperationType: String; AMenuItem: TMenuItem; AToolMenuItem: TToolButton = nil);
begin
  TJobOperationManager.Instance.RegisterMenuItem(AOperationType, AMenuItem, AToolMenuItem);
end;

procedure PerformOperation(AOperationType: String; Sender: TObject = nil);
begin
  TJobOperationManager.Instance.PerformOperation(AOperationType, Sender);
end;

var
  FInstance: TJobOperationManager = nil;

class function TJobOperationManager.AccessInstance(Request: Integer): TJobOperationManager;
begin
  case Request of
    0 : ;
    1 : if not Assigned(FInstance) then FInstance := CreateInstance;
    2 : FInstance := nil;
  else
    raise Exception.CreateFmt('Illegal request %d in AccessInstance',
        [Request]);
  end;
  Result := FInstance;
end;

constructor TJobOperationManager.Create;
begin
  inherited Create;
  raise Exception.CreateFmt('Access class %s through Instance only',
      [ClassName]);
end;

constructor TJobOperationManager.CreateInstance(ADummy: Integer);
begin
  inherited Create();
  FList := TObjectList.Create();
  FGlobalList := TObjectList.Create();
  FCurrentOperationList := nil;
end;

destructor TJobOperationManager.Destroy;
begin
  if AccessInstance(0) = Self then AccessInstance(2);
  FGlobalList.Free();
  FList.Free();
  inherited Destroy();
end;

function TJobOperationManager.GetManagerItemByOperation(AOperationType: String): TJobOperationManagerItem;
var
  i: Integer;
  AItem: TJobOperationManagerItem;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    AItem := TJobOperationManagerItem(FList[i]);
    if (CompareText(AItem.FOperationType, AOperationType) = 0) then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

class function TJobOperationManager.Instance: TJobOperationManager;
begin
  Result := AccessInstance(1);
end;

procedure TJobOperationManager.OperationEnabledEvent(Sender: TJobOperationItem);
var
  AItem: TJobOperationManagerItem;
begin
  AItem := GetManagerItemByOperation(Sender.OperationType);
  if (AItem <> nil) then
  begin
    AItem.FMenuItem.Enabled := Sender.Enabled;
    if (AItem.FToolMenuItem <> nil) then
    begin
      AItem.FToolMenuItem.Enabled := Sender.Enabled;
    end;
  end;
end;

procedure TJobOperationManager.OperationListAddOperation(AItem: TJobOperationItem);
begin
  AItem.OnEnabledChange := OperationEnabledEvent;
  AItem.OnVisibleChange := OperationVisibleEvent;
end;

procedure TJobOperationManager.OperationListRemoveOperation(
  AItem: TJobOperationItem);
begin
end;

procedure TJobOperationManager.OperationVisibleEvent(Sender: TJobOperationItem);
var
  AItem: TJobOperationManagerItem;
begin
  AItem := GetManagerItemByOperation(Sender.OperationType);
  if (AItem <> nil) then
  begin
    AItem.FMenuItem.Visible := Sender.Visible;
    if (AItem.FToolMenuItem <> nil) then
    begin
      AItem.FToolMenuItem.Visible := Sender.Visible;
    end;
  end;
end;

procedure TJobOperationManager.PerformOperation(AOperationType: String; Sender: TObject);
begin
  if (FCurrentOperationList <> nil) then
  begin
    FCurrentOperationList.PerformOperation(AOperationType, Sender);
  end;
end;

procedure TJobOperationManager.PerformOperationByItem(Sender: TObject);
  function GetOperationManagerItem: TJobOperationManagerItem;
  var
    i: Integer;
    AItem: TJobOperationManagerItem;
  begin
    Result := nil;
    for i := 0 to FList.Count - 1 do
    begin
      AItem := TJobOperationManagerItem(FList[i]);
      if (AItem.FMenuItem = Sender) then
      begin
        Result := AItem;
        Break;
      end else
      if (AItem.FToolMenuItem = Sender) then
      begin
        Result := AItem;
        Break;
      end;
    end;
  end;
  
var
  AItem: TJobOperationManagerItem;
begin
  AItem := GetOperationManagerItem();
  if (AItem <> nil) then
  begin
    PerformOperation(AItem.FOperationType, Sender);
  end;
end;

procedure TJobOperationManager.RegisterMenuItem(AOperationType: String;
  AMenuItem: TMenuItem; AToolMenuItem: TToolButton);
var
  AItem: TJobOperationManagerItem;
begin
  if (GetManagerItemByOperation(AOperationType) <> nil) then
  begin
    raise Exception.Create('The operation item is already registered');
  end;
  AItem := TJobOperationManagerItem.Create();
  FList.Add(AItem);
  AItem.FOperationType := AOperationType;
  AItem.FMenuItem := AMenuItem;
  AItem.FToolMenuItem := AToolMenuItem;
  AMenuItem.OnClick := PerformOperationByItem;
  if (AItem.FToolMenuItem <> nil) then
  begin
    AItem.FToolMenuItem.OnClick := PerformOperationByItem;
  end;
end;

class procedure TJobOperationManager.ReleaseInstance;
begin
  AccessInstance(0).Free();
end;

procedure TJobOperationManager.ShowMenuItems(AList: TFormOperationList; IsShow: Boolean);
var
  i: Integer;
  Item: TJobOperationItem;
  ManagerItem: TJobOperationManagerItem;
begin
  for i := 0 to AList.ItemsCount - 1 do
  begin
    Item := AList.Items[i];
    ManagerItem := GetManagerItemByOperation(Item.OperationType);
    if (ManagerItem <> nil) then
    begin
      ManagerItem.FMenuItem.Visible := IsShow and Item.Visible;
      if (ManagerItem.FToolMenuItem <> nil) then
      begin
        ManagerItem.FToolMenuItem.Visible := IsShow and Item.Visible;
      end;
    end;
  end;
end;

procedure TJobOperationManager.SetCurrentOperationList(const Value: TFormOperationList);
begin
  if (FCurrentOperationList <> Value) then
  begin
    if (FCurrentOperationList <> nil) then
    begin
      ShowMenuItems(FCurrentOperationList, False);
    end else
    begin
      InitMenuItems();
    end;
    FCurrentOperationList := Value;
    if (FCurrentOperationList <> nil) then
    begin
      ShowMenuItems(FCurrentOperationList, True);
    end else
    begin
      InitMenuItems();
    end;
  end;
end;

procedure TJobOperationManager.UnRegisterMenuItem(AOperationType: String);
var
  i: Integer;
  AItem: TJobOperationManagerItem;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    AItem := TJobOperationManagerItem(FList[i]);
    if (CompareText(AItem.FOperationType, AOperationType) = 0) then
    begin
      FList.Delete(i);
      Break;
    end;
  end;
end;

procedure TJobOperationManager.InitMenuItems;
var
  i: Integer;
  ManagerItem: TJobOperationManagerItem;
begin
  for i := 0 to FList.Count - 1 do
  begin
    ManagerItem := TJobOperationManagerItem(FList[i]);
    ManagerItem.FMenuItem.Visible := False;
    if (ManagerItem.FToolMenuItem <> nil) then
    begin
      ManagerItem.FToolMenuItem.Visible := False;
    end;
  end;
end;

function TJobOperationManager.GetGlobalOperationEvent(
  AName: String): TJobGlobalOperationEvent;
var
  i : Integer;
  Rec: TJobGlobalOperationRecord;
begin
  Result := nil;
  for i := 0 to FGlobalList.Count - 1 do
  begin
    Rec := TJobGlobalOperationRecord(FGlobalList[i]);
    if (CompareText(Rec.Name, AName) = 0) then
    begin
      Result := Rec.Event;
      Break;
    end;
  end;
end;

function TJobOperationManager.PerformGlobalOperation(AName: String;
  Params: TJobOperationParams): Boolean;
var
  AEvent: TJobGlobalOperationEvent;
begin
  Result := False;
  AEvent := GetGlobalOperationEvent(AName);
  if Assigned(AEvent) then
  begin
    AEvent(Params, Result);
  end;
end;

procedure TJobOperationManager.RegisterGlobalOperation(AName: String;
  AEvent: TJobGlobalOperationEvent);
begin
  FGlobalList.Add(TJobGlobalOperationRecord.Create());
  TJobGlobalOperationRecord(FGlobalList.Last).Name := AName;
  TJobGlobalOperationRecord(FGlobalList.Last).Event := AEvent;
end;

procedure TJobOperationManager.UnRegisterGlobalOperation(AName: String);
var
  i: Integer;
  AItem: TJobGlobalOperationRecord;
begin
  for i := FGlobalList.Count - 1 downto 0 do
  begin
    AItem := TJobGlobalOperationRecord(FGlobalList[i]);
    if (CompareText(AItem.Name, AName) = 0) then
    begin
      FGlobalList.Delete(i);
      Break;
    end;
  end;
end;

{ TFormOperationList }

procedure TFormOperationList.AddOperationToPopup(APopupMenu: TPopupMenu; AOperationType: string; ABeginGroup: Boolean);
var
  MenuItem: TMenuItem;
  OperationItem: TJobOperationItem;
  ManagerItem: TJobOperationManagerItem;
begin
  OperationItem := GetOperationItem(AOperationType);
  if (OperationItem <> nil) then
  begin
    if ABeginGroup then
    begin
      MenuItem := TMenuItem.Create(nil);
      APopupMenu.Items.Add(MenuItem);
      MenuItem.Caption := '-';
    end;
    MenuItem := TMenuItem.Create(nil);
    APopupMenu.Items.Add(MenuItem);
    MenuItem.Visible := OperationItem.Visible;
    MenuItem.Enabled := OperationItem.Enabled;
    MenuItem.OnClick := OperationItem.Operation;
    MenuItem.Caption := OperationItem.Caption;
    if (MenuItem.Caption = '') then
    begin
      ManagerItem := TJobOperationManager.Instance.GetManagerItemByOperation(AOperationType);
      if (ManagerItem <> nil) then
      begin
        MenuItem.Caption := ManagerItem.FMenuItem.Caption;
      end;
    end;
  end;
end;

constructor TFormOperationList.Create;
begin
  inherited Create();
  OnAddOperation := TJobOperationManager.Instance.OperationListAddOperation;
  OnRemoveOperation := TJobOperationManager.Instance.OperationListRemoveOperation;
end;

initialization

finalization
  TJobOperationManager.ReleaseInstance();

end.
