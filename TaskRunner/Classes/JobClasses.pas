unit JobClasses;

interface

uses
  Winapi.Windows, System.Classes, OperationClasses, System.SysUtils, JobConsts,
  System.Generics.Collections, Winapi.msxml, System.Variants;

type
  TJobState = (jsNormal, jsEdited, jsRun);

  TJobItem = class;
  TJobVisitor = class;

  TRunJobEvent = procedure (Visitor: TJobVisitor) of object;
  TRunJobMessageEvent = procedure (Visitor: TJobVisitor; const ALog, AErrors: string) of object;
  TJobItemEvent = procedure (JobItem: TJobItem) of object;
  TJobStateEvent = procedure (AJobItem: TJobItem; State: TJobState) of object;
  TOnGetGlobalParamsEvent = procedure (var Params: TJobOperationParams) of object;

  TJobVisitor = class
  private
    FParams: TJobOperationParams;
    FLog: TStrings;
    FErrors: TStrings;
    FFullLog: TStrings;
    FFullErrors: TStrings;
    FOnStart: TRunJobEvent;
    FOnFinish: TRunJobEvent;
    FOnItemProcessed: TRunJobEvent;
    FIsStarted: Boolean;
    FJobItem: TJobItem;
    FCurrentJobItem: TJobItem;
    FOnItemPerformed: TRunJobMessageEvent;
    function GetJobName: String;
  protected
    procedure DoStart; virtual;
    procedure DoFinish; virtual;
    procedure DoItemProcessed; virtual;
    procedure DoItemPerformed(const ALog, AErrors: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Perform(AJobItem: TJobItem; IsUnitJob: Boolean): Boolean;
    procedure Stop;
    function IsJobPerform(AItem: TJobItem): Boolean;

    property Params: TJobOperationParams read FParams;
    property Log: TStrings read FLog;
    property Errors: TStrings read FErrors;
    property FullLog: TStrings read FFullLog;
    property FullErrors: TStrings read FFullErrors;
    property JobName: String read GetJobName;
    property CurrentJobItem: TJobItem read FCurrentJobItem;
    property RootJobItem: TJobItem read FJobItem;

    property OnStart: TRunJobEvent read FOnStart write FOnStart;
    property OnFinish: TRunJobEvent read FOnFinish write FOnFinish;
    property OnItemProcessed: TRunJobEvent read FOnItemProcessed write FOnItemProcessed;
    property OnItemPerformed: TRunJobMessageEvent read FOnItemPerformed write FOnItemPerformed;
  end;

  TThreadVisitor = class(TThread)
  private
    FLastLog: string;
    FLastErrors: string;
    FVisitor: TJobVisitor;
    FOnStart: TRunJobEvent;
    FOnFinish: TRunJobEvent;
    FOnItemProcessed: TRunJobEvent;
    FIsUnitJob: Boolean;
    FJobItem: TJobItem;
    FOnItemPerformed: TRunJobMessageEvent;
    procedure DoOnStart(Visitor: TJobVisitor);
    procedure DoOnFinish(Visitor: TJobVisitor);
    procedure DoOnItemProcessed(Visitor: TJobVisitor);
    procedure DoOnItemPerformed(Visitor: TJobVisitor; const ALog, AErrors: string);
    procedure SyncOnStart;
    procedure SyncOnFinish;
    procedure SyncOnItemProcessed;
    procedure SyncOnItemPerformed;
  protected
    procedure Execute; override;
  public
    constructor Create(AItem: TJobItem; AIsUnitJob: Boolean);
    destructor Destroy; override;

    property OnStart: TRunJobEvent read FOnStart write FOnStart;
    property OnFinish: TRunJobEvent read FOnFinish write FOnFinish;
    property OnItemProcessed: TRunJobEvent read FOnItemProcessed write FOnItemProcessed;
    property OnItemPerformed: TRunJobMessageEvent read FOnItemPerformed write FOnItemPerformed;
  end;

  TJobDataItem = class(TPersistent)
  private
    FFlowAction: TFlowAction;
    FJobName: String;
    FDescription: TStrings;
    FOwner: TJobItem;
    FOnDataChange: TNotifyEvent;
    FUpdateCount: Integer;
    FDataState: TJobState;
    FOldDataState: TJobState;
    FOnDataStateChange: TNotifyEvent;
    FDataRunCount: Integer;
    FCanPerform: string;
    procedure SetDescription(const Value: TStrings);
    procedure SetFlowAction(const Value: TFlowAction);
    procedure SetJobName(const Value: String);
    procedure SetCanPerform(const Value: string);
    procedure DescriptionChangeEvent(Sender: TObject);
  protected
    procedure SetDataState(const Value: TJobState); virtual;
    function GetDataState: TJobState; virtual;
    procedure DoDataStateChanged; dynamic;
    procedure DoBeforePerform(Visitor: TJobVisitor); dynamic;
    procedure DoAfterPerform(Visitor: TJobVisitor); dynamic;
    procedure DoDataChanged; dynamic;
    procedure InternalSetDataState(const Value: TJobState); virtual;
    procedure InitData; virtual;
  public
    constructor Create(AOwner: TJobItem); virtual;
    destructor Destroy; override;

    function Load(AStream: TStream): Integer; overload; virtual;
    procedure Load(ANode: IXMLDOMNode); overload; virtual;
    procedure Store(ANode: IXMLDOMNode); virtual;
    procedure Perform(Visitor: TJobVisitor); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure GetParameterList(AList: TJobOperationParams); virtual;
    procedure GetGlobalParameterList(AList: TJobOperationParams); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    property CanPerform: string read FCanPerform write SetCanPerform;
    property DataState: TJobState read GetDataState write SetDataState;
    property FlowAction: TFlowAction read FFlowAction write SetFlowAction;
    property Description: TStrings read FDescription write SetDescription;
    property JobName: String read FJobName write SetJobName;
    property Owner: TJobItem read FOwner;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnDataStateChange: TNotifyEvent read FOnDataStateChange write FOnDataStateChange;
  end;

  TJobDataItemClass = class of TJobDataItem;

  TJobManager = class;

  TJobEditorItem = class
  private
    FData: TJobDataItem;
    FReadOnly: Boolean;
  protected
    procedure SetReadOnly(const Value: Boolean); virtual;
  public
    constructor Create(AData: TJobDataItem); virtual;
    destructor Destroy; override;
    procedure Perform; virtual; abstract;
    property Data: TJobDataItem read FData;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

  TJobEditorItemClass = class of TJobEditorItem;

  TJobList = class(TObjectList<TJobItem>)
  end;

  TJobItem = class
  private
    FJobList: TJobList;
    FOwner: TJobItem;
    FData: TJobDataItem;
    FJobManager: TJobManager;

    function GetItems(Index: Integer): TJobItem;
    function GetItemsCount: Integer;
    function GetIsLocked: Boolean;
    procedure ClearItems;
    function GetDescription: TStrings;
    function GetFlowAction: TFlowAction;
    function GetJobName: String;
    procedure SetJobName(const Value: String);
  protected
    procedure DoDataStateChanged; virtual;
    procedure DoDataChanged; virtual;
  public
    constructor Create(AJobManager: TJobManager; AOwner: TJobItem; AJobName: String; ADataClass: TJobDataItemClass);
    destructor Destroy; override;

    procedure Assign(Source: TJobItem);
    procedure Load(AStream: TStream); overload;
    procedure Load(ANode: IXMLDOMNode); overload;
    procedure Store(ANode: IXMLDOMNode);
    procedure Perform(Visitor: TJobVisitor);
    procedure SetJobState(AState: TJobState);
    function CheckJobState(AState: TJobState): Boolean;

    property Items[Index: Integer]: TJobItem read GetItems;
    property ItemsCount: Integer read GetItemsCount;
    property JobName: String read GetJobName write SetJobName;
    property Description: TStrings read GetDescription;
    property IsLocked: Boolean read GetIsLocked;
    property FlowAction: TFlowAction read GetFlowAction;
    property JobManager: TJobManager read FJobManager;
    property Data: TJobDataItem read FData;
    property Owner: TJobItem read FOwner;
  end;

  TJobEditorCategory = class
  private
    FEditorClass: TJobEditorItemClass;
    FDataClass: TJobDataItemClass;
    FDataName: String;
  public
    property EditorClass: TJobEditorItemClass read FEditorClass;
    property DataClass: TJobDataItemClass read FDataClass;
    property DataName: String read FDataName;
  end;

  TJobManager = class
  private
    FRootList: TJobList;
    FEditorList: TObjectList<TJobEditorItem>;
    FRunList: TObjectList<TJobVisitor>;
    FOnStartAction: TRunJobEvent;
    FOnItemProcessedAction: TRunJobEvent;
    FOnFinishAction: TRunJobEvent;
    FOnDataStateChanged: TJobStateEvent;
    FOnDataChanged: TJobItemEvent;
    FOnGetGlobalParams: TOnGetGlobalParamsEvent;
    FOnItemPerformedAction: TRunJobMessageEvent;
    FReferences: TJobOperationParams;

    function GetRootItems(Index: Integer): TJobItem;
    function GetRootItemsCount: Integer;
    function GetIsLocked: Boolean;

    procedure DoOnStart(Visitor: TJobVisitor);
    procedure DoOnFinish(Visitor: TJobVisitor);
    procedure DoOnItemProcessed(Visitor: TJobVisitor);
    procedure DoOnItemPerformed(Visitor: TJobVisitor; const ALog, AErrors: string);
    function GetEditor(AData: TJobDataItem): TJobEditorItem;
    procedure RemoveEditor(AData: TJobDataItem);
    procedure DoOnReferencesChanged(Sender: TObject);
  protected
    procedure DataStateChanged(AJobItem: TJobItem; State: TJobState); virtual;
    procedure DataChanged(AJobItem: TJobItem); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function GetGlobalParameters: TJobOperationParams;
    procedure RunJob(AItem: TJobItem; IsUnitJob: Boolean = True; IsAsynch: Boolean = True); overload;
    procedure RunJob(const AItem: string; IsUnitJob: Boolean = True; IsAsynch: Boolean = True); overload;
    procedure StopJob(AItem: TJobItem);
    procedure StopAllJobs;
    function AddJobItem(AParent: TJobItem; AJobName: String; ADataClass: TJobDataItemClass): TJobItem;
    function AddJobSubItem(AParent: TJobItem; AJobName: String; ADataClass: TJobDataItemClass): TJobItem;
    procedure MoveJobItem(DestItem, SourceItem: TJobItem; IsBeforeDestItem: Boolean = False);
    procedure RemoveJobItem(AItem: TJobItem);
    function Load(AStream: TStream): Integer; overload;
    procedure Load(ANode: IXMLDOMNode); overload; virtual;
    procedure Store(ANode: IXMLDOMNode); virtual;
    procedure EditJobItem(AItem: TJobItem; IsReadOnly: Boolean = False);
    procedure ClearItems;

    property References: TJobOperationParams read FReferences;
    property RootItems[Index: Integer]: TJobItem read GetRootItems;
    property RootItemsCount: Integer read GetRootItemsCount;
    property IsLocked: Boolean read GetIsLocked;

    property OnStartAction: TRunJobEvent read FOnStartAction write FOnStartAction;
    property OnFinishAction: TRunJobEvent read FOnFinishAction write FOnFinishAction;
    property OnItemProcessedAction: TRunJobEvent read FOnItemProcessedAction write FOnItemProcessedAction;
    property OnItemPerformedAction: TRunJobMessageEvent read FOnItemPerformedAction write FOnItemPerformedAction;
    property OnDataStateChanged: TJobStateEvent read FOnDataStateChanged write FOnDataStateChanged;
    property OnDataChanged: TJobItemEvent read FOnDataChanged write FOnDataChanged;
    property OnGetGlobalParams: TOnGetGlobalParamsEvent read FOnGetGlobalParams write FOnGetGlobalParams;
  end;

  TJobEditorManager = class
  private
    FEditorCategoriesList: TObjectList<TJobEditorCategory>;

    function GetEditorCategories(Index: Integer): TJobEditorCategory;
    function GetEditorCategoriesCount: Integer;
  protected
    constructor CreateInstance(ADummy: Integer = 0);
    class function AccessInstance(Request: Integer): TJobEditorManager;
    function GetEditorClass(ADataClass: TJobDataItemClass): TJobEditorItemClass;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TJobEditorManager;
    class procedure ReleaseInstance;

    procedure RegisterEditorItem(AEditorClass: TJobEditorItemClass;
      ADataClass: TJobDataItemClass; ADataItemName: String = '');
    property EditorCategories[Index: Integer]: TJobEditorCategory read GetEditorCategories;
    property EditorCategoriesCount: Integer read GetEditorCategoriesCount;
  end;

procedure RegisterEditorItem(AEditorClass: TJobEditorItemClass;
  ADataClass: TJobDataItemClass; ADataItemName: String = '');

implementation

uses
  ActiveX, XMLUtils;

type
  ESkipPerform = class(Exception);

procedure RegisterEditorItem(AEditorClass: TJobEditorItemClass;
  ADataClass: TJobDataItemClass; ADataItemName: String = '');
begin
  TJobEditorManager.Instance.RegisterEditorItem(AEditorClass, ADataClass, ADataItemName);
end;

{ TJobItem }

procedure TJobItem.Assign(Source: TJobItem);
var
  i: Integer;
  Item, SrcItem: TJobItem;
begin
  if (Source.Data.ClassType <> FData.ClassType) then Exit;
  ClearItems();
  FData.Assign(Source.Data);
  for i := 0 to Source.ItemsCount - 1 do
  begin
    SrcItem := Source.Items[i];
    Item := TJobItem.Create(FJobManager, Self, SrcItem.JobName, TJobDataItemClass(SrcItem.Data.ClassType));
    Item.Assign(SrcItem);
  end;
end;

procedure TJobItem.ClearItems;
var
  i: Integer;
begin
  for i := FJobList.Count - 1 downto 0 do
  begin
    FJobList[i].Free();
  end;

  FJobList.Clear();
end;

constructor TJobItem.Create(AJobManager: TJobManager; AOwner: TJobItem; AJobName: String;
  ADataClass: TJobDataItemClass);
begin
  inherited Create();

  FData := ADataClass.Create(Self);
  FJobList := TJobList.Create(False);

  FJobManager := AJobManager;
  FData.JobName := AJobName;
  FOwner := AOwner;

  if (FOwner <> nil) then
  begin
    FOwner.FJobList.Add(Self);
  end;
end;

destructor TJobItem.Destroy;
begin
  ClearItems();
  if (FOwner <> nil) then
  begin
    FOwner.FJobList.Remove(Self);
  end;
  if (FJobManager <> nil) then
  begin
    FJobManager.RemoveEditor(FData);
  end;
  FJobList.Free();
  FData.Free();
  inherited Destroy();
end;

procedure TJobItem.DoDataChanged;
begin
  if (FJobManager <> nil) then
  begin
    FJobManager.DataChanged(Self);
  end;
end;

procedure TJobItem.DoDataStateChanged;
begin
  if (FJobManager <> nil) then
  begin
    FJobManager.DataStateChanged(Self, Self.Data.DataState);
  end;
end;

function TJobItem.GetDescription: TStrings;
begin
  Result := FData.Description;
end;

function TJobItem.GetFlowAction: TFlowAction;
begin
  Result := FData.FlowAction;
end;

function TJobItem.GetIsLocked: Boolean;
var
  i: Integer;
begin
  Result := FData.DataState <> jsNormal;
  if not Result then
  begin
    for i := 0 to FJobList.Count - 1 do
    begin
      Result := FJobList[i].IsLocked;
      if Result then Break;
    end;
  end;
end;

function TJobItem.GetItems(Index: Integer): TJobItem;
begin
  Result := FJobList[Index];
end;

function TJobItem.GetItemsCount: Integer;
begin
  Result := FJobList.Count;
end;

function TJobItem.GetJobName: String;
begin
  Result := FData.JobName;
end;

procedure TJobItem.Load(AStream: TStream);
var
  i, Cnt: Integer;
  ADataItemClass: TJobDataItemClass;
  R: TReader;
begin
  ClearItems();
  FData.Load(AStream);
  AStream.Read(Cnt, SizeOf(Integer));
  for i := 0 to Cnt - 1 do
  begin
    R := TReader.Create(AStream, 1024);
    try
      ADataItemClass := TJobDataItemClass(GetClass(R.ReadString()));
    finally
      R.Free();
    end;
    TJobItem.Create(FJobManager, Self, '', ADataItemClass).Load(AStream);
  end;
end;

procedure TJobItem.SetJobState(AState: TJobState);
var
  i: Integer;
begin
  Data.DataState := AState;
  for i := 0 to FJobList.Count - 1 do
  begin
    FJobList[i].SetJobState(AState);
  end;
end;

procedure TJobItem.Perform(Visitor: TJobVisitor);
begin
  FData.DoBeforePerform(Visitor);
  try
    FData.Perform(Visitor);
  finally
    FData.DoAfterPerform(Visitor);
  end;
end;

procedure TJobItem.SetJobName(const Value: String);
begin
  FData.JobName := Value;
end;

procedure TJobItem.Store(ANode: IXMLDOMNode);
var
  i: Integer;
  AItem: TJobItem;
  ParamsNode, ChildNode: IXMLDOMNode;
begin
  ParamsNode := ANode.ownerDocument.createElement('Params');
  ANode.appendChild(ParamsNode);
  FData.Store(ParamsNode);
  for i := 0 to FJobList.Count - 1 do
  begin
    AItem := FJobList[i];
    ChildNode := ANode.ownerDocument.createElement('Job');
    ANode.appendChild(ChildNode);
    (ChildNode as IXMLDOMElement).setAttribute('dataclassname', AItem.FData.ClassName);
    AItem.Store(ChildNode);
  end;
end;

function TJobItem.CheckJobState(AState: TJobState): Boolean;
var
  i: Integer;
begin
  Result := Data.DataState = AState;
  if not Result then
  begin
    for i := 0 to FJobList.Count - 1 do
    begin
      if FJobList[i].CheckJobState(AState) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TJobItem.Load(ANode: IXMLDOMNode);
var
  i: Integer;
  ADataItemClass: TJobDataItemClass;
  ParamsNode, ChildNode: IXMLDOMNode;
  NodeList: IXMLDOMNodeList;
begin
  ClearItems();
  ParamsNode := ANode.selectSingleNode('Params');
  if ParamsNode = nil then
  begin
    raise Exception.Create(cLoadError);
  end;
  FData.Load(ParamsNode);
  NodeList := ANode.selectNodes('Job');
  if (NodeList = nil) then
  begin
    raise Exception.Create(cLoadError);
  end;
  for i := 0 to NodeList.length - 1 do
  begin
    ChildNode := NodeList.item[i];
    ADataItemClass := TJobDataItemClass(GetClass((ChildNode as IXMLDOMElement).getAttribute('dataclassname')));
    if (ADataItemClass <> nil) then
    begin
      TJobItem.Create(FJobManager, Self, '', ADataItemClass).Load(ChildNode);
    end;
  end;
end;

{ TJobDataItem }

procedure TJobDataItem.Assign(Source: TPersistent);
begin
  if (DataState <> jsNormal) then
  begin
    raise Exception.Create(cJobDataLocked);
  end;
  BeginUpdate();
  try
    if (Source is TJobDataItem) then
    begin
      FFlowAction := TJobDataItem(Source).FlowAction;
      FDescription.Assign(TJobDataItem(Source).Description);
      FCanPerform := TJobDataItem(Source).CanPerform;
    end else
    begin
      InitData();
    end;
  finally
    EndUpdate();
  end;
end;

procedure TJobDataItem.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TJobDataItem.Create(AOwner: TJobItem);
begin
  inherited Create();
  FDescription := TStringList.Create();
  TStringList(FDescription).OnChange := DescriptionChangeEvent;
  FDataState := jsNormal;
  FOldDataState := jsNormal;
  FDataRunCount := 0;
  FFlowAction := faSuccess;
  FOwner := AOwner;
  FUpdateCount := 0;
end;

procedure TJobDataItem.DescriptionChangeEvent(Sender: TObject);
begin
  DoDataChanged();
end;

destructor TJobDataItem.Destroy;
begin
  FDescription.Free();
  inherited Destroy();
end;

procedure TJobDataItem.DoAfterPerform(Visitor: TJobVisitor);
begin
end;

procedure TJobDataItem.DoBeforePerform(Visitor: TJobVisitor);
begin
end;

procedure TJobDataItem.DoDataChanged;
begin
  if (FUpdateCount > 0) then Exit;
  
  if Assigned(FOnDataChange) then
  begin
    FOnDataChange(Self);
  end;
  if (FOwner <> nil) then
  begin
    FOwner.DoDataChanged();
  end;
end;

procedure TJobDataItem.DoDataStateChanged;
begin
  if Assigned(FOnDataStateChange) then
  begin
    FOnDataStateChange(Self);
  end;
  if (FOwner <> nil) then
  begin
    FOwner.DoDataStateChanged();
  end;
end;

procedure TJobDataItem.EndUpdate;
begin
  Dec(FUpdateCount);
  DoDataChanged();
end;

function TJobDataItem.GetDataState: TJobState;
begin
  Result := FDataState;
end;

procedure TJobDataItem.GetGlobalParameterList(AList: TJobOperationParams);
begin
  if (DataState <> jsNormal) then
  begin
    raise Exception.Create(cJobDataLocked);
  end;
end;

procedure TJobDataItem.GetParameterList(AList: TJobOperationParams);
var
  s: string;
begin
  if (DataState <> jsNormal) then
  begin
    raise Exception.Create(cJobDataLocked);
  end;
  s := FCanPerform;
  if (Pos(cParseLexems, s) = 1) then
  begin
    system.Delete(s, 1, Length(cParseLexems));
    AList.Add(s, NULL);
  end;
end;

procedure TJobDataItem.InitData;
begin
  FFlowAction := faSuccess;
  FDescription.Clear();
  FCanPerform := '';
end;

procedure TJobDataItem.InternalSetDataState(const Value: TJobState);
begin
  FDataState := Value;
  case FDataState of
    jsNormal:
      begin
        Dec(FDataRunCount);
        if (FDataRunCount > 0) then
        begin
          FDataState := jsRun;
        end else
        begin
          FDataRunCount := 0;
        end;
      end;
    jsRun:
      begin
        Inc(FDataRunCount);
      end;
    jsEdited:
      begin
        if (FDataRunCount > 0) then
        begin
          raise Exception.Create(cJobDataLocked);
        end;
      end;
  end;
end;

function TJobDataItem.Load(AStream: TStream): Integer;
var
  R: TReader;
begin
  if (DataState <> jsNormal) then
  begin
    raise Exception.Create(cJobDataLocked);
  end;
  BeginUpdate();
  try
    R := TReader.Create(AStream, 1024);
    try
      FJobName := R.ReadString();
      Result := R.ReadInteger(); //Version
      FDescription.Text := R.ReadString();
      if (Result > 8) then
      begin
        FCanPerform := R.ReadString();
      end;
    finally
      R.Free();
    end;
    AStream.Read(FFlowAction, SizeOf(FFlowAction));
  finally
    EndUpdate();
  end;
end;

procedure TJobDataItem.Load(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
  ind: Integer;
begin
  if (DataState <> jsNormal) then
  begin
    raise Exception.Create(cJobDataLocked);
  end;
  BeginUpdate();
  try
    InitData();

    FJobName := (ANode.parentNode as IXMLDOMElement).getAttribute('name');

    ChildNode := ANode.selectSingleNode('Description');
    if ChildNode <> nil then FDescription.Text := ChildNode.text;

    ChildNode := ANode.selectSingleNode('CanPerform');
    if ChildNode <> nil then FCanPerform := ChildNode.text;

    ChildNode := ANode.selectSingleNode('FlowAction');
    if ChildNode <> nil then
    begin
      ind := GetArrayIndexByName(ChildNode.text, cStoreFlowActionNames);
      if (ind > -1) then FFlowAction := TFlowAction(ind);
    end;
  finally
    EndUpdate();
  end;
end;

procedure TJobDataItem.Perform(Visitor: TJobVisitor);
var
  s: string;
  Param: TJobOperationParam;
begin
  if (DataState <> jsRun) then
  begin
    raise Exception.Create(cJobDataLocked);
  end;
  s := FCanPerform;
  if (Pos(cParseLexems, s) = 1) then
  begin
    system.Delete(s, 1, Length(cParseLexems));
    Param := Visitor.Params.FindParam(s);
    if (Param <> nil) then
    begin
      s := VarToStr(Param.Value);
    end;
  end;
  if (CompareText(s, cNOClause) = 0) then
  begin
    raise ESkipPerform.CreateFmt(cJobSkipped, [FJobName]);
  end;
end;

procedure TJobDataItem.SetCanPerform(const Value: string);
begin
  if (FCanPerform <> Value) then
  begin
    FCanPerform := Value;
    DoDataChanged();
  end;
end;

procedure TJobDataItem.SetDataState(const Value: TJobState);
begin
  InternalSetDataState(Value);
  if (FDataState <> FOldDataState) then
  begin
    FOldDataState := FDataState;
    DoDataStateChanged();
  end;
end;

procedure TJobDataItem.SetDescription(const Value: TStrings);
begin
  FDescription.Assign(Value);
end;

procedure TJobDataItem.SetFlowAction(const Value: TFlowAction);
begin
  if (FFlowAction <> Value) then
  begin
    FFlowAction := Value;
    DoDataChanged();
  end;
end;

procedure TJobDataItem.SetJobName(const Value: String);
begin
  if (FJobName <> Value) then
  begin
    FJobName := Value;
    DoDataChanged();
  end;
end;

procedure TJobDataItem.Store(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  (ANode.parentNode as IXMLDOMElement).setAttribute('name', FJobName);

  ChildNode := ANode.ownerDocument.createElement('Description');
  ANode.appendChild(ChildNode);
  ChildNode.text := FDescription.Text;

  ChildNode := ANode.ownerDocument.createElement('CanPerform');
  ANode.appendChild(ChildNode);
  ChildNode.text := FCanPerform;

  ChildNode := ANode.ownerDocument.createElement('FlowAction');
  ANode.appendChild(ChildNode);
  ChildNode.text := cStoreFlowActionNames[FFlowAction];
end;

{ TJobManager }

function TJobManager.AddJobItem(AParent: TJobItem; AJobName: String; ADataClass: TJobDataItemClass): TJobItem;
var
  AOwner: TJobItem;
begin
  if (AParent <> nil) then
  begin
    AOwner := AParent.FOwner;
  end else
  begin
    AOwner := nil;
  end;

  Result := TJobItem.Create(Self, AOwner, AJobName, ADataClass);

  if (AOwner = nil) then
  begin
    FRootList.Add(Result);
  end;
end;

procedure TJobManager.ClearItems;
begin
  if IsLocked then
  begin
    raise Exception.Create(cJobLocked);
  end;
  while (FRootList.Count > 0) do
  begin
    RemoveJobItem(FRootList.Last());
  end;
end;

constructor TJobManager.Create;
begin
  inherited Create();

  FRootList := TJobList.Create(False);
  FEditorList := TObjectList<TJobEditorItem>.Create(False);
  FRunList := TObjectList<TJobVisitor>.Create(False);
  FReferences := TJobOperationParams.Create();
  FReferences.OnChanged := DoOnReferencesChanged;
end;

destructor TJobManager.Destroy;
var
  i: Integer;
begin
  ClearItems();
  for i := FRunList.Count - 1 downto 0 do
  begin
    FRunList[i].Free();
  end;
  FReferences.Free();
  FRunList.Free();
  FEditorList.Free();
  FRootList.Free();
  inherited Destroy();
end;

procedure TJobManager.EditJobItem(AItem: TJobItem; IsReadOnly: Boolean = False);
var
  EditorClass: TJobEditorItemClass;
  Editor: TJobEditorItem;
begin
  Editor := GetEditor(AItem.FData);
  if (Editor = nil) then
  begin
    EditorClass := TJobEditorManager.Instance.GetEditorClass(TJobDataItemClass(AItem.FData.ClassType));
    if (EditorClass <> nil) then
    begin
      Editor := EditorClass.Create(AItem.FData);
    end;
  end;
  if (Editor = nil) then
  begin
    raise Exception.Create(cNonRegisteredEditor);
  end;
  Editor.ReadOnly := IsReadOnly;
  Editor.Perform();
end;

function TJobManager.GetIsLocked: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FRootList.Count - 1 do
  begin
    Result := FRootList[i].IsLocked;
    if Result then Break;
  end;
end;

function TJobManager.GetRootItems(Index: Integer): TJobItem;
begin
  Result := FRootList[Index];
end;

function TJobManager.GetRootItemsCount: Integer;
begin
  Result := FRootList.Count;
end;

function TJobManager.Load(AStream: TStream): Integer;
var
  i, Cnt, AVersion: Integer;
  ADataItemClass: TJobDataItemClass;
  R: TReader;
begin
  AStream.Read(AVersion, SizeOf(Integer));
  Result := AVersion;
  ClearItems();
  AStream.Read(Cnt, SizeOf(Integer));
  for i := 0 to Cnt - 1 do
  begin
    R := TReader.Create(AStream, 1024);
    try
      ADataItemClass := TJobDataItemClass(GetClass(R.ReadString()));
    finally
      R.Free();
    end;
    AddJobItem(nil, '', ADataItemClass).Load(AStream);
  end;
end;

procedure TJobManager.RemoveJobItem(AItem: TJobItem);
begin
  if AItem.IsLocked then
  begin
    raise Exception.Create(cJobDataLocked);
  end;
  AItem.Free();
  FRootList.Remove(AItem);
end;

procedure TJobManager.RemoveEditor(AData: TJobDataItem);
var
  i: Integer;
  Editor: TJobEditorItem;
begin
  for i := FEditorList.Count - 1 downto 0 do
  begin
    Editor := FEditorList[i];
    if (Editor.Data = AData) then
    begin
      Editor.Free();
      Break;
    end;
  end;
end;

procedure TJobManager.RunJob(AItem: TJobItem; IsUnitJob: Boolean; IsAsynch: Boolean);
var
  Visitor: TThreadVisitor;
begin
  Visitor := TThreadVisitor.Create(AItem, IsUnitJob);
  Visitor.OnStart := DoOnStart;
  Visitor.OnFinish := DoOnFinish;
  Visitor.OnItemProcessed := DoOnItemProcessed;
  Visitor.OnItemPerformed := DoOnItemPerformed;
  Visitor.Start();
  if not IsAsynch then
  begin
    Visitor.WaitFor();
  end;
end;

procedure TJobManager.StopJob(AItem: TJobItem);
var
  i: Integer;
  Visitor: TJobVisitor;
begin
  for i := 0 to FRunList.Count - 1 do
  begin
    Visitor := FRunList[i];
    if Visitor.IsJobPerform(AItem) then
    begin
      Visitor.Stop();
    end;
  end;
end;

procedure TJobManager.Store(ANode: IXMLDOMNode);
var
  i: Integer;
  AItem: TJobItem;
  RootNode, ChildNode: IXMLDOMNode;
begin
  RootNode := ANode.ownerDocument.createElement('References');
  ANode.appendChild(RootNode);
  FReferences.Store(RootNode);

  RootNode := ANode.ownerDocument.createElement('Jobs');
  ANode.appendChild(RootNode);
  for i := 0 to FRootList.Count - 1 do
  begin
    AItem := FRootList[i];
    ChildNode := ANode.ownerDocument.createElement('Job');
    RootNode.appendChild(ChildNode);
    (ChildNode as IXMLDOMElement).setAttribute('dataclassname', AItem.FData.ClassName);
    AItem.Store(ChildNode);
  end;
end;

procedure TJobManager.DoOnFinish(Visitor: TJobVisitor);
begin
  FRunList.Remove(Visitor);
  Visitor.FJobItem.SetJobState(jsNormal);
  if Assigned(FOnFinishAction) then
  begin
    FOnFinishAction(Visitor);
  end;
end;

procedure TJobManager.DoOnItemProcessed(Visitor: TJobVisitor);
begin
  if Assigned(FOnItemProcessedAction) then
  begin
    FOnItemProcessedAction(Visitor);
  end;
end;

procedure TJobManager.DoOnStart(Visitor: TJobVisitor);
begin
  Visitor.FJobItem.SetJobState(jsRun);
  FRunList.Add(Visitor);
  if Assigned(FOnStartAction) then
  begin
    FOnStartAction(Visitor);
  end;
end;

function TJobManager.GetEditor(AData: TJobDataItem): TJobEditorItem;
var
  i: Integer;
  Item: TJobEditorItem;
begin
  Result := nil;
  for i := 0 to FEditorList.Count - 1 do
  begin
    Item := FEditorList[i];
    if (Item.Data = AData) then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

function TJobManager.AddJobSubItem(AParent: TJobItem; AJobName: String; ADataClass: TJobDataItemClass): TJobItem;
begin
  Result := TJobItem.Create(Self, AParent, AJobName, ADataClass);

  if (AParent = nil) then
  begin
    FRootList.Add(Result);
  end;
end;

procedure TJobManager.MoveJobItem(DestItem, SourceItem: TJobItem; IsBeforeDestItem: Boolean = False);
  procedure MoveInList(AList: TJobList);
  var
    ADestIndex, ASourceIndex: Integer;
  begin
    ADestIndex := AList.IndexOf(DestItem);
    ASourceIndex := AList.IndexOf(SourceItem);

    if (ADestIndex > - 1) and (ASourceIndex > - 1) then
    begin
      if (not IsBeforeDestItem) and (AList.Count - 1 = ADestIndex) then
      begin
        Inc(ADestIndex);
      end;
      AList.Move(ASourceIndex, ADestIndex);
    end;
  end;

  procedure InsertInList(AList: TJobList);
  var
    ADestIndex: Integer;
  begin
    ADestIndex := AList.IndexOf(DestItem);

    if (ADestIndex > - 1) then
    begin
      if (not IsBeforeDestItem) and (AList.Count - 1 = ADestIndex) then
      begin
        Inc(ADestIndex);
      end;
      AList.Insert(ADestIndex, SourceItem);
    end else
    begin
      AList.Add(SourceItem);
    end;
  end;

var
  ADestOwner, ASourceOwner: TJobItem;
begin
  if (DestItem = nil) or (SourceItem = nil) then Exit;

  ADestOwner := DestItem.FOwner;
  ASourceOwner := SourceItem.FOwner;

  if (ADestOwner = ASourceOwner) then
  begin
    if (ADestOwner <> nil) then
    begin
      MoveInList(ADestOwner.FJobList);
    end else
    begin
      MoveInList(FRootList);
    end;
  end else
  begin
    if (ASourceOwner <> nil) then
    begin
      ASourceOwner.FJobList.Remove(SourceItem);
    end else
    begin
      FRootList.Remove(SourceItem);
    end;

    if (ADestOwner <> nil) then
    begin
      InsertInList(ADestOwner.FJobList);
    end else
    begin
      InsertInList(FRootList);
    end;
    SourceItem.FOwner := ADestOwner;
  end;
end;

procedure TJobManager.DataStateChanged(AJobItem: TJobItem; State: TJobState);
begin
  if Assigned(FOnDataStateChanged) then
  begin
    FOnDataStateChanged(AJobItem, State);
  end;
end;

procedure TJobManager.StopAllJobs;
var
  i: Integer;
  Visitor: TJobVisitor;
begin
  for i := 0 to FRunList.Count - 1 do
  begin
    Visitor := FRunList[i];
    Visitor.Stop();
  end;
end;

procedure TJobManager.DataChanged(AJobItem: TJobItem);
begin
  if Assigned(FOnDataChanged) then
  begin
    FOnDataChanged(AJobItem);
  end;
end;

function TJobManager.GetGlobalParameters: TJobOperationParams;
begin
  Result := nil;
  if Assigned(FOnGetGlobalParams) then
  begin
    FOnGetGlobalParams(Result);
  end;
end;

procedure TJobManager.DoOnItemPerformed(Visitor: TJobVisitor; const ALog, AErrors: string);
begin
  if Assigned(FOnItemPerformedAction) then
  begin
    FOnItemPerformedAction(Visitor, ALog, AErrors);
  end;
end;

procedure TJobManager.RunJob(const AItem: string; IsUnitJob: Boolean; IsAsynch: Boolean);
var
  i: Integer;
  AJob: TJobItem;
begin
  for i := 0 to RootItemsCount - 1 do
  begin
    AJob := RootItems[i];
    if CompareText(AJob.JobName, AItem) = 0 then
    begin
      RunJob(AJob, IsUnitJob, IsAsynch);
      Exit;
    end;
  end;
end;

procedure TJobManager.Load(ANode: IXMLDOMNode);
var
  i: Integer;
  ADataItemClass: TJobDataItemClass;
  RootNode, ChildNode: IXMLDOMNode;
begin
  RootNode := ANode.selectSingleNode('References');
  if (RootNode <> nil) then
  begin
    FReferences.Load(RootNode);
  end;
  ClearItems();
  RootNode := ANode.selectSingleNode('Jobs');
  if (RootNode <> nil) then
  begin
    for i := 0 to RootNode.childNodes.length - 1 do
    begin
      ChildNode := RootNode.childNodes.item[i];
      ADataItemClass := TJobDataItemClass(GetClass((ChildNode as IXMLDOMElement).getAttribute('dataclassname')));
      if (ADataItemClass <> nil) then
      begin
        AddJobItem(nil, '', ADataItemClass).Load(ChildNode);
      end;
    end;
  end;
end;

procedure TJobManager.DoOnReferencesChanged(Sender: TObject);
begin
  DataStateChanged(nil, jsEdited);
  DataChanged(nil);
end;

{ TJobVisitor }

constructor TJobVisitor.Create;
begin
  inherited Create();
  FParams := TJobOperationParams.Create();
  FLog := TStringList.Create();
  FErrors := TStringList.Create();
  FFullLog := TStringList.Create();
  FFullErrors := TStringList.Create();
  FIsStarted := False;
  FJobItem := nil;
  FCurrentJobItem := nil;
end;

destructor TJobVisitor.Destroy;
begin
  FFullErrors.Free();
  FFullLog.Free();
  FErrors.Free();
  FLog.Free();
  FParams.Free();
  inherited Destroy();
end;

procedure TJobVisitor.DoFinish;
begin
  if Assigned(FOnFinish) then
  begin
    FOnFinish(Self);
  end;
end;

procedure TJobVisitor.DoItemPerformed(const ALog, AErrors: string);
begin
  if Assigned(FOnItemPerformed) then
  begin
    FOnItemPerformed(Self, ALog, AErrors);
  end;
end;

procedure TJobVisitor.DoItemProcessed;
begin
  if Assigned(FOnItemProcessed) then
  begin
    FOnItemProcessed(Self);
  end;
end;

procedure TJobVisitor.DoStart;
begin
  if Assigned(FOnStart) then
  begin
    FOnStart(Self);
  end;
end;

function TJobVisitor.GetJobName: String;
begin
  if (FJobItem <> nil) then
  begin
    Result := FJobItem.JobName;
  end else
  begin
    Result := '';
  end;
end;

function TJobVisitor.IsJobPerform(AItem: TJobItem): Boolean;
begin
  Result := (AItem = FJobItem);
end;

function TJobVisitor.Perform(AJobItem: TJobItem; IsUnitJob: Boolean): Boolean;

  function DoRunJob(AJob: TJobItem): Boolean;
  var
    i: Integer;
    AItem: TJobItem;
    IsSkipped: Boolean;
//    p, p1: Pointer; //TODO
  begin
    Result := True;
    IsSkipped := False;
    if not FIsStarted then
    begin
      Exit;
    end;
    Log.Clear();
    Errors.Clear();
    DoItemProcessed();
//    p := nil; //TODO
//    p1 := Pointer(p^);
    try
      Log.Add(#13#10 + Format(cJobPerformMessage, [AJob.JobName]));
      AItem := TJobItem.Create(AJob.JobManager, AJob.FOwner, AJob.JobName, TJobDataItemClass(AJob.Data.ClassType));
      try
        AItem.Data.Assign(AJob.Data);
        AItem.Data.InternalSetDataState(jsRun);
        AItem.Perform(Self);
      finally
        AItem.Data.InternalSetDataState(jsNormal);
        AItem.Free();
      end;
    except
      on E: ESkipPerform do
        begin
          Log.Add(E.Message);
          IsSkipped := True;
        end;
      on E: Exception do
        begin
          Errors.Add(E.Message);
          Errors.Insert(0, #13#10 + Format(cJobPerformMessage, [AJob.JobName]));
          Result := False;
        end;
    end;
    FullLog.AddStrings(Log);
    FullErrors.AddStrings(Errors);
    DoItemPerformed(Log.Text, Errors.Text);
    if (not IsUnitJob) and (not IsSkipped) and Result then
    begin
      for i := 0 to AJob.ItemsCount - 1 do
      begin
        AItem := AJob.Items[i];
        FCurrentJobItem := AItem;
        case AItem.FlowAction of
          faComplete:
            begin
              Result := DoRunJob(AItem);
            end;
          faCompleteThrow:
            begin
              DoRunJob(AItem);
            end;
          faSuccess:
            begin
              if Result then
              begin
                Result := DoRunJob(AItem);
              end;
            end;
          faFailure:
            begin
              if not Result then
              begin
                Result := DoRunJob(AItem);
              end;
            end;
          faFailThrow:
            begin
              if not Result then
              begin
                DoRunJob(AItem);
              end;
            end;
          faDisable:
            begin
              Log.Add(Format(cJobDisabled, [AItem.JobName]));
            end;
        end;
      end;
    end;
  end;

begin
  if FIsStarted then
  begin
    raise Exception.Create(cJobAlreadyRun);
  end;

  FIsStarted := True;
  FJobItem := AJobItem;
  FCurrentJobItem := AJobItem;

  DoStart();
  try
    Result := DoRunJob(AJobItem);
    if not FIsStarted then
    begin
      FErrors.Add(cPerformanceStopped);
    end;
  finally
    FIsStarted := False;
    DoFinish();
    FCurrentJobItem := nil;
    FJobItem := nil;
  end;
end;

procedure TJobVisitor.Stop;
begin
  FIsStarted := False;
end;

{ TJobEditorItem }

constructor TJobEditorItem.Create(AData: TJobDataItem);
begin
  inherited Create();
  FData := AData;
  if (FData <> nil) and (FData.FOwner.JobManager <> nil) then
  begin
    FData.Owner.JobManager.FEditorList.Add(Self);
  end;
end;

destructor TJobEditorItem.Destroy;
begin
  if (FData <> nil) and (FData.FOwner.JobManager <> nil) then
  begin
    FData.Owner.JobManager.FEditorList.Remove(Self);
  end;
  inherited Destroy();
end;

procedure TJobEditorItem.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

{ TJobEditorManager }
var
  FInstance: TJobEditorManager = nil;

class function TJobEditorManager.AccessInstance(Request: Integer): TJobEditorManager;
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

constructor TJobEditorManager.Create;
begin
  inherited Create;
  raise Exception.CreateFmt('Access class %s through Instance only',
      [ClassName]);
end;

constructor TJobEditorManager.CreateInstance(ADummy: Integer);
begin
  inherited Create();
  FEditorCategoriesList := TObjectList<TJobEditorCategory>.Create();
end;

destructor TJobEditorManager.Destroy;
begin
  if AccessInstance(0) = Self then AccessInstance(2);
  FEditorCategoriesList.Free();
  inherited Destroy();
end;

class function TJobEditorManager.Instance: TJobEditorManager;
begin
  Result := AccessInstance(1);
end;

class procedure TJobEditorManager.ReleaseInstance;
begin
  AccessInstance(0).Free();
end;

function TJobEditorManager.GetEditorCategories(Index: Integer): TJobEditorCategory;
begin
  Result := FEditorCategoriesList[Index];
end;

function TJobEditorManager.GetEditorCategoriesCount: Integer;
begin
  Result := FEditorCategoriesList.Count;
end;

procedure TJobEditorManager.RegisterEditorItem(AEditorClass: TJobEditorItemClass;
  ADataClass: TJobDataItemClass; ADataItemName: String = '');
var
  Item: TJobEditorCategory;
begin
  if (TJobEditorManager.Instance.GetEditorClass(ADataClass) = nil) then
  begin
    Item := TJobEditorCategory.Create();
    Item.FEditorClass := AEditorClass;
    Item.FDataClass := ADataClass;
    if (ADataItemName = '') then
    begin
      ADataItemName := AEditorClass.ClassName;
    end;
    Item.FDataName := ADataItemName;
    FEditorCategoriesList.Add(Item);
  end;
end;

function TJobEditorManager.GetEditorClass(ADataClass: TJobDataItemClass): TJobEditorItemClass;
var
  i: Integer;
  Item: TJobEditorCategory;
begin
  Result := nil;

  for i := 0 to FEditorCategoriesList.Count - 1 do
  begin
    Item := FEditorCategoriesList[i];
    if (Item.FDataClass = ADataClass) then
    begin
      Result := Item.FEditorClass;
      Break;
    end;
  end;
end;

{ TThreadVisitor }

constructor TThreadVisitor.Create(AItem: TJobItem; AIsUnitJob: Boolean);
begin
  inherited Create(True);
  FVisitor := TJobVisitor.Create();
  FVisitor.OnStart := DoOnStart;
  FVisitor.OnFinish := DoOnFinish;
  FVisitor.OnItemProcessed := DoOnItemProcessed;
  FVisitor.OnItemPerformed := DoOnItemPerformed;
  FreeOnTerminate := True;
  FJobItem := AItem;
  FIsUnitJob := AIsUnitJob;
end;

destructor TThreadVisitor.Destroy;
begin
  FVisitor.Free();
  inherited Destroy();
end;

procedure TThreadVisitor.DoOnFinish(Visitor: TJobVisitor);
begin
  Synchronize(SyncOnFinish);
end;

procedure TThreadVisitor.DoOnItemPerformed(Visitor: TJobVisitor; const ALog, AErrors: string);
begin
  FLastLog := ALog;
  FLastErrors := AErrors;
  Synchronize(SyncOnItemPerformed);
end;

procedure TThreadVisitor.DoOnItemProcessed(Visitor: TJobVisitor);
begin
  Synchronize(SyncOnItemProcessed);
end;

procedure TThreadVisitor.DoOnStart(Visitor: TJobVisitor);
begin
  Synchronize(SyncOnStart);
end;

procedure TThreadVisitor.Execute;
var
  b: Boolean;
begin
  b := Succeeded(CoInitialize(nil));
  try
    FVisitor.Perform(FJobItem, FIsUnitJob);
  except
  end;
  if b then
  begin
    CoUninitialize();
  end;
end;

procedure TThreadVisitor.SyncOnFinish;
begin
  if Assigned(FOnFinish) then
  begin
    FOnFinish(FVisitor);
  end;
end;

procedure TThreadVisitor.SyncOnItemPerformed;
begin
  if Assigned(FOnItemPerformed) then
  begin
    FOnItemPerformed(FVisitor, FLastLog, FLastErrors);
  end;
end;

procedure TThreadVisitor.SyncOnItemProcessed;
begin
  if Assigned(FOnItemProcessed) then
  begin
    FOnItemProcessed(FVisitor);
  end;
end;

procedure TThreadVisitor.SyncOnStart;
begin
  if Assigned(FOnStart) then
  begin
    FOnStart(FVisitor);
  end;
end;

initialization

finalization
  TJobEditorManager.ReleaseInstance();

end.
