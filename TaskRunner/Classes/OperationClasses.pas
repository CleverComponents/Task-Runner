unit OperationClasses;

interface

uses
  System.Classes, System.SysUtils, System.Contnrs, Winapi.msxml, System.Variants,
  System.Types;

type
  TJobOperationParams = class;

  TJobOperationParam = class
  private
    FValue: Variant;
    FName: String;
    FOwner: TJobOperationParams;
    function GetIsNull: Boolean;
    procedure SetValue(const Value: Variant);
  public
    constructor Create(AOwner: TJobOperationParams; AName: String; AValue: Variant);
    destructor Destroy; override;
    property IsNull: Boolean read GetIsNull;
    property Name: String read FName;
    property Value: Variant read FValue write SetValue;
  end;

  TJobOperationParams = class
  private
    FList: TList;
    FOnChanged: TNotifyEvent;
    FUpdateCount: Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): TJobOperationParam;
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function FindParam(const Value: String): TJobOperationParam;
    function ParamByName(const Value: String): TJobOperationParam;
    procedure Assign(Source: TJobOperationParams);
    procedure Add(const AName: String; Value: Variant);
    procedure Remove(const AName: String);
    procedure Clear;
    procedure Load(AStream: TStream); overload; virtual;
    procedure Store(AStream: TStream); overload; virtual;
    procedure Load(ANode: IXMLDOMNode); overload; virtual;
    procedure Store(ANode: IXMLDOMNode); overload; virtual;
    procedure LoadFromFile(const AFileName: string);
    procedure StoreToFile(const AFileName: string);
    procedure BeginUpdate;
    procedure EndUpdate;
    property Items[Index: Integer]: TJobOperationParam read GetItem; default;
    property Count: Integer read GetCount;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TJobOperationList = class;
  TJobOperationItem = class;

  TJobOperationItemEvent = procedure (Sender: TJobOperationItem) of object;

  TJobOperationItem = class
  private
    FOwner: TJobOperationList;
    FVisible: Boolean;
    FLocked: Boolean;
    FEnabled: Boolean;
    FCaption: String;
    FOperationType: String;
    FOperation: TNotifyEvent;
    FOnEnabledChange: TJobOperationItemEvent;
    FOnVisibleChange: TJobOperationItemEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetLocked(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure DoOnEnabledChange; dynamic;
    procedure DoOnVisibleChange; dynamic;
  public
    constructor Create(AOwner: TJobOperationList; AOperationType: String;
      AOperation: TNotifyEvent; ACaption: String = '');
    destructor Destroy; override;
    property OperationType: String read FOperationType;
    property Locked: Boolean read FLocked write SetLocked;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Visible: Boolean read FVisible write SetVisible;
    property Caption: String read FCaption;
    property Operation: TNotifyEvent read FOperation;
    property OnEnabledChange: TJobOperationItemEvent read FOnEnabledChange write FOnEnabledChange;
    property OnVisibleChange: TJobOperationItemEvent read FOnVisibleChange write FOnVisibleChange;
  end;

  TJobOperationEvent = procedure (AItem: TJobOperationItem) of object;

  TJobOperationList = class
  private
    FList: TList;
    FOnAddOperation: TJobOperationEvent;
    FOnRemoveOperation: TJobOperationEvent;
    function GetItems(Index: Integer): TJobOperationItem;
    function GetItemsCount: Integer;
  protected
    function GetOperationItem(AOperationType: String): TJobOperationItem;
    procedure DoOnAddOperation(AItem: TJobOperationItem); dynamic;
    procedure DoOnRemoveOperation(AItem: TJobOperationItem); dynamic;
    property OnAddOperation: TJobOperationEvent read FOnAddOperation write FOnAddOperation;
    property OnRemoveOperation: TJobOperationEvent read FOnRemoveOperation write FOnRemoveOperation;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddOperation(AOperationType: String; AOperation: TNotifyEvent; ACaption: String = '');
    procedure RemoveOperation(AOperationType: String);
    procedure PerformOperation(AOperationType: String; Sender: TObject = nil);
    procedure LockOperation(AOperationType: String; IsLock: Boolean);
    procedure EnableOperation(AOperationType: String; IsEnable: Boolean);
    procedure VisibleOperation(AOperationType: String; IsVisible: Boolean);
    property Items[Index: Integer]: TJobOperationItem read GetItems;
    property ItemsCount: Integer read GetItemsCount;
  end;

implementation

{ TJobOperationParams }

procedure TJobOperationParams.Add(const AName: String; Value: Variant);
var
  Item: TJobOperationParam;
begin
  Item := FindParam(AName);
  if (Item = nil) then
  begin
    FList.Add(TJobOperationParam.Create(Self, AName, Value));
  end else
  begin
    Item.Value := Value;
  end;
end;

procedure TJobOperationParams.Assign(Source: TJobOperationParams);
var
  i: Integer;
  Item: TJobOperationParam;
begin
  BeginUpdate();
  try
    Clear();
    for i := 0 to Source.Count - 1 do
    begin
      Item := Source.Items[i];
      Add(Item.Name, Item.Value);
    end;
  finally
    EndUpdate();
  end;
end;

procedure TJobOperationParams.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJobOperationParams.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanged) then
  begin
    FOnChanged(Self);
  end;
end;

procedure TJobOperationParams.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    TJobOperationParam(FList.Items[i]).Free();
  end;

  FList.Clear();
end;

constructor TJobOperationParams.Create;
begin
  inherited Create();
  FList := TList.Create();
  FUpdateCount := 0;
end;

destructor TJobOperationParams.Destroy;
begin
  Clear();
  FList.Free();
  inherited Destroy();
end;

procedure TJobOperationParams.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed();
end;

function TJobOperationParams.FindParam(const Value: String): TJobOperationParam;
var
  i: Integer;
  Item: TJobOperationParam;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    Item := TJobOperationParam(FList.Items[i]);
    if (CompareText(Item.Name, Value) = 0) then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

function TJobOperationParams.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJobOperationParams.GetItem(Index: Integer): TJobOperationParam;
begin
  Result := TJobOperationParam(FList.Items[Index]);
end;

procedure TJobOperationParams.Load(AStream: TStream);
var
  i, Cnt, AType: Integer;
  R: TReader;
  AName: String;
  AVal: Variant;
begin
  BeginUpdate();
  R := TReader.Create(AStream, 1024);
  try
    Cnt := R.ReadInteger();
    Clear();
    for i := 0 to Cnt - 1 do
    begin
      AName := R.ReadString();
      AType := R.ReadInteger();
      AVal := R.ReadString();
      Add(AName, VarAsType(AVal, AType));
    end;
  finally
    R.Free();
    EndUpdate();
  end;
end;

procedure TJobOperationParams.Load(ANode: IXMLDOMNode);
var
  i: Integer;
  ChildNode: IXMLDOMNode;
  AName: String;
begin
  BeginUpdate();
  try
    Clear();
    for i := 0 to ANode.childNodes.length - 1 do
    begin
      ChildNode := ANode.childNodes.item[i];
      AName := (ChildNode as IXMLDOMElement).getAttribute('name');
      if (AName <> '') then
      begin
        Add(AName, ChildNode.text);
      end;
    end;
  finally
    EndUpdate();
  end;
end;

procedure TJobOperationParams.LoadFromFile(const AFileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    Load(Stream);
  finally
    Stream.Free();
  end;
end;

function TJobOperationParams.ParamByName(const Value: String): TJobOperationParam;
begin
  Result := FindParam(Value);
  if (Result = nil) then
  begin
    raise Exception.CreateFmt('Parameter %s not found', [Value]);
  end;
end;

procedure TJobOperationParams.Remove(const AName: String);
var
  Item: TJobOperationParam;
begin
  Item := FindParam(AName);
  if (Item <> nil) then
  begin
    FList.Remove(Item);
    Item.Free();
  end;
end;

procedure TJobOperationParams.Store(AStream: TStream);
var
  i: Integer;
  W: TWriter;
  Param: TJobOperationParam;
begin
  W := TWriter.Create(AStream, 1024);
  try
    W.WriteInteger(FList.Count);
    for i := 0 to FList.Count - 1 do
    begin
      Param := TJobOperationParam(FList[i]);
      W.WriteString(Param.Name);
      W.WriteInteger(VarType(Param.Value));
      W.WriteString(VarToStr(Param.Value));
    end;
  finally
    W.Free();
  end;
end;

procedure TJobOperationParams.Store(ANode: IXMLDOMNode);
var
  i: Integer;
  ChildNode: IXMLDOMNode;
  Param: TJobOperationParam;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Param := TJobOperationParam(FList[i]);
    ChildNode := ANode.ownerDocument.createElement('Param');
    ANode.appendChild(ChildNode);
    (ChildNode as IXMLDOMElement).setAttribute('name', Param.Name);
    ChildNode.text := VarToStr(Param.Value);
  end;
end;

procedure TJobOperationParams.StoreToFile(const AFileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Store(Stream);
  finally
    Stream.Free();
  end;
end;

{ TJobOperationParam }

constructor TJobOperationParam.Create(AOwner: TJobOperationParams; AName: String; AValue: Variant);
begin
  inherited Create();
  FName := AName;
  FValue := AValue;
  FOwner := AOwner;
  if (FOwner <> nil) then
  begin
    FOwner.Changed();
  end;
end;

destructor TJobOperationParam.Destroy;
begin
  if (FOwner <> nil) then
  begin
    FOwner.Changed();
  end;
  inherited Destroy();
end;

function TJobOperationParam.GetIsNull: Boolean;
begin
  Result := VarIsEmpty(FValue) or VarIsNull(FValue);
end;

procedure TJobOperationParam.SetValue(const Value: Variant);
begin
  if (FValue <> Value) then
  begin
    FValue := Value;
    if (FOwner <> nil) then
    begin
      FOwner.Changed();
    end;
  end;
end;

{ TJobOperationItem }

constructor TJobOperationItem.Create(AOwner: TJobOperationList;
  AOperationType: String; AOperation: TNotifyEvent; ACaption: String);
begin
  inherited Create();
  FOwner := AOwner;
  if (FOwner <> nil) then
  begin
    FOwner.FList.Add(Self);
  end;
  FVisible := True;
  FLocked := False;
  FEnabled := True;
  FCaption := ACaption;
  FOperationType := AOperationType;
  FOperation := AOperation;
end;

destructor TJobOperationItem.Destroy;
begin
  if (FOwner <> nil) then
  begin
    FOwner.FList.Remove(Self);
  end;
  inherited Destroy();
end;

procedure TJobOperationItem.DoOnEnabledChange;
begin
  if Assigned(FOnEnabledChange) then
  begin
    FOnEnabledChange(Self);
  end;
end;

procedure TJobOperationItem.DoOnVisibleChange;
begin
  if Assigned(FOnVisibleChange) then
  begin
    FOnVisibleChange(Self);
  end;
end;

procedure TJobOperationItem.SetEnabled(const Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    if FLocked then
    begin
      FEnabled := False;
    end else
    begin
      FEnabled := Value;
    end;
    DoOnEnabledChange();
  end;
end;

procedure TJobOperationItem.SetLocked(const Value: Boolean);
begin
  if (FLocked <> Value) then
  begin
    FLocked := Value;
    if FLocked then
    begin
      Enabled := not FLocked;
    end;
  end;
end;

procedure TJobOperationItem.SetVisible(const Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    DoOnVisibleChange();
  end;
end;

{ TJobOperationList }

procedure TJobOperationList.AddOperation(AOperationType: String; AOperation: TNotifyEvent;
  ACaption: String = '');
var
  Item: TJobOperationItem;
begin
  if (GetOperationItem(AOperationType) = nil) then
  begin
    Item := TJobOperationItem.Create(Self, AOperationType, AOperation, ACaption);
    DoOnAddOperation(Item);
    Item.DoOnVisibleChange();
    Item.DoOnEnabledChange();
  end else
  begin
    raise Exception.Create('The operation item already exists');
  end;
end;

constructor TJobOperationList.Create;
begin
  inherited Create();
  FList := TList.Create();
end;

destructor TJobOperationList.Destroy;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    TJobOperationItem(FList[i]).Free();
  end;
  FList.Free();
  inherited Destroy();
end;

procedure TJobOperationList.DoOnAddOperation(AItem: TJobOperationItem);
begin
  if Assigned(FOnAddOperation) then
  begin
    FOnAddOperation(AItem);
  end;
end;

procedure TJobOperationList.DoOnRemoveOperation(AItem: TJobOperationItem);
begin
  if Assigned(FOnRemoveOperation) then
  begin
    FOnRemoveOperation(AItem);
  end;
end;

procedure TJobOperationList.EnableOperation(AOperationType: String; IsEnable: Boolean);
var
  Item: TJobOperationItem;
begin
  Item := GetOperationItem(AOperationType);
  if (Item <> nil) then
  begin
    Item.Enabled := IsEnable;
  end;
end;

function TJobOperationList.GetItems(Index: Integer): TJobOperationItem;
begin
  Result := TJobOperationItem(FList[Index]);
end;

function TJobOperationList.GetItemsCount: Integer;
begin
  Result := FList.Count;
end;

function TJobOperationList.GetOperationItem(AOperationType: String): TJobOperationItem;
var
  i: Integer;
  Item: TJobOperationItem;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    Item := TJobOperationItem(FList[i]);
    if (CompareText(Item.OperationType, AOperationType) = 0) then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

procedure TJobOperationList.LockOperation(AOperationType: String; IsLock: Boolean);
var
  Item: TJobOperationItem;
begin
  Item := GetOperationItem(AOperationType);
  if (Item <> nil) then
  begin
    Item.Locked := IsLock;
  end;
end;

procedure TJobOperationList.PerformOperation(AOperationType: String; Sender: TObject);
var
  Item: TJobOperationItem;
begin
  Item := GetOperationItem(AOperationType);
  if (Item <> nil) and Assigned(Item.Operation) then
  begin
    Item.Operation(Sender);
  end;
end;

procedure TJobOperationList.RemoveOperation(AOperationType: String);
var
  i: Integer;
  Item: TJobOperationItem;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Item := TJobOperationItem(FList[i]);
    if (CompareText(Item.OperationType, AOperationType) = 0) then
    begin
      DoOnRemoveOperation(Item);
      Item.Free();
    end;
  end;
end;

procedure TJobOperationList.VisibleOperation(AOperationType: String; IsVisible: Boolean);
var
  Item: TJobOperationItem;
begin
  Item := GetOperationItem(AOperationType);
  if (Item <> nil) then
  begin
    Item.Visible := IsVisible;
  end;
end;

end.
