unit CallJobItem;

interface

uses
  System.Classes, JobClasses, System.SysUtils, System.Variants, OperationClasses, CustomJobItems, Winapi.msxml;

type
  TCallJobDataItem = class(TCustomParametersJobDataItem)
  private
    FCallJobManagerFileName: String;
    FCallJobManager: TJobManager;
    FCallMediaName: String;
    FCallJobName: String;
    procedure SetCallJobName(const Value: String);
    procedure SetCallMediaName(const Value: String);
  protected
    procedure InternalSetDataState(const Value: TJobState); override;
    function GetDataState: TJobState; override;
    procedure InitData; override;
  public
    constructor Create(AOwner: TJobItem); override;
    destructor Destroy; override;
    procedure GetGlobalParameterList(AList: TJobOperationParams); override;
    function Load(AStream: TStream): Integer; overload; override;
    procedure Load(ANode: IXMLDOMNode); overload; override;
    procedure Store(ANode: IXMLDOMNode); override;
    procedure Assign(Source: TPersistent); override;
    procedure Perform(Visitor: TJobVisitor); override;
    function GetJobManager(AMediaName: String): TJobManager;
    function GetCallJobItem(AJobName: String; AMediaName: String): TJobItem;
    function FindCallJobItem(AJobName: String; AMediaName: String): TJobItem;
    property CallJobName: String read FCallJobName write SetCallJobName;
    property CallMediaName: String read FCallMediaName write SetCallMediaName;
  end;

implementation

uses
  JobConsts;

{ TCallJobDataItem }

procedure TCallJobDataItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  BeginUpdate();
  try
    if (Source is TCallJobDataItem) then
    begin
      FCallJobName := TCallJobDataItem(Source).CallJobName;
      FCallMediaName := TCallJobDataItem(Source).CallMediaName;
    end else
    begin
      InitData();
    end;
  finally
    EndUpdate();
  end;
end;

constructor TCallJobDataItem.Create(AOwner: TJobItem);
begin
  inherited Create(AOwner);
  FCallJobName := '';
  FCallMediaName := '';
  FCallJobManager := nil;
  FCallJobManagerFileName := '';
end;

function TCallJobDataItem.GetCallJobItem(AJobName: String; AMediaName: String): TJobItem;
var
  i: Integer;
  Item: TJobItem;
  AJobManager: TJobManager;
begin
  Result := nil;
  AJobManager := GetJobManager(AMediaName);
  if (AJobManager = nil) then Exit;
  for i := 0 to AJobManager.RootItemsCount - 1 do
  begin
    Item := AJobManager.RootItems[i];
    if (Item.Data <> Self) and (CompareText(Item.JobName, AJobName) = 0) then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

function TCallJobDataItem.GetJobManager(AMediaName: String): TJobManager;
  function GetJobLibPath: String;
  var
    Params: TJobOperationParams;
    Param: TJobOperationParam;
  begin
    Result := '';
    if (Owner <> nil) and (Owner.JobManager <> nil) then
    begin
      Params := Owner.JobManager.GetGlobalParameters();
      if (Params <> nil) then
      begin
        Param := Params.FindParam(cGlobalJobLibPath);
        if (Param <> nil) then
        begin
          Result := VarToStr(Param.Value);
        end;
      end;
    end;
    if (Result = '') then
    begin
      Result := ExtractFilePath(ParamStr(0));
    end;
    if (Result <> '') and (Result[Length(Result)] <> '\') then
    begin
      Result := Result + '\';
    end;
  end;

  function DoLoadAsXML(const AFileName: string): Boolean;
  var
    Doc: IXMLDOMDocument;
    MainNode: IXMLDOMNode;
  begin
    Doc := CoDOMDocument.Create();
    Doc.load(AFileName);
    Result := (Doc.xml <> '');
    if not Result then Exit;
    MainNode := Doc.selectSingleNode('Main');
    if MainNode = nil then
    begin
      raise Exception.CreateFmt(cUnknownImportMediaFile, [AFileName]);
    end;
    FCallJobManager.Load(MainNode);
  end;

  procedure DoLoadAsStream(const AFileName: string);
  var
    Stream: TStream;
  begin
    Stream := TFileStream.Create(AFileName, fmOpenRead);
    try
      FCallJobManager.Load(Stream);
    finally
      Stream.Free();
    end;
  end;

var
  AFileName: String;
begin
  Result := Owner.JobManager;
  if (FCallJobManagerFileName <> AMediaName) then
  begin
    FreeAndNil(FCallJobManager);
    FCallJobManagerFileName := AMediaName;
  end;
  if (FCallJobManagerFileName = '') then Exit;

  AFileName := FCallJobManagerFileName;
  if (ExtractFilePath(AFileName) = '') then
  begin
    AFileName := GetJobLibPath() + AFileName;
  end;

  if FileExists(AFileName) then
  begin
    if (FCallJobManager = nil) then
    begin
      FCallJobManager := TJobManager.Create();
      FCallJobManager.OnGetGlobalParams := Owner.JobManager.OnGetGlobalParams;
      if not DoLoadAsXML(AFileName) then
      begin
        DoLoadAsStream(AFileName);
      end;
    end;
    Result := FCallJobManager;
  end;
end;

function TCallJobDataItem.Load(AStream: TStream): Integer;
var
  R: TReader;
begin
  Result := inherited Load(AStream);
  BeginUpdate();
  R := TReader.Create(AStream, 1024);
  try
    FCallJobName := R.ReadString();
    if (Result > 7) then
    begin
      FCallMediaName := R.ReadString();
    end;
  finally
    R.Free();
    EndUpdate();
  end;
end;

procedure TCallJobDataItem.Perform(Visitor: TJobVisitor);
var
  Item: TJobItem;
  CallVisitor: TJobVisitor;
begin
  inherited Perform(Visitor);

  Item := FindCallJobItem(FCallJobName, FCallMediaName);
  CallVisitor := TJobVisitor.Create();
  try
    CallVisitor.Params.Assign(Parameters);
    AssignParams(CallVisitor.Params, Visitor.Params, cParseLexems);
    if not CallVisitor.Perform(Item, False) then
    begin
      raise Exception.CreateFmt(cCallJobError, [Item.JobName]);
    end;
    AssignParams(Visitor.Params, CallVisitor.Params, cParseLexems);
  finally
    Visitor.Log.AddStrings(CallVisitor.FullLog);
    Visitor.Errors.AddStrings(CallVisitor.FullErrors);
    CallVisitor.Free();
  end;
end;

procedure TCallJobDataItem.Store(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  inherited Store(ANode);
  
  ChildNode := ANode.ownerDocument.createElement('CallJobName');
  ANode.appendChild(ChildNode);
  ChildNode.text := FCallJobName;

  ChildNode := ANode.ownerDocument.createElement('CallMediaName');
  ANode.appendChild(ChildNode);
  ChildNode.text := FCallMediaName;
end;

procedure TCallJobDataItem.SetCallJobName(const Value: String);
begin
  if (FCallJobName <> Value) then
  begin
    FCallJobName := Value;
    DoDataChanged();
  end;
end;

function TCallJobDataItem.GetDataState: TJobState;
var
  Item: TJobItem;
begin
  Result := inherited GetDataState();
  if (Result = jsNormal) then
  begin
    if (FCallMediaName = '') then //TODO
    begin
      Item := GetCallJobItem(FCallJobName, '');
      if (Item <> nil) then
      begin
        if Item.CheckJobState(jsEdited) then
        begin
          Result := jsEdited;
        end else
        if Item.CheckJobState(jsRun) then
        begin
          Result := jsRun;
        end;
      end;
    end;
  end;
end;

function TCallJobDataItem.FindCallJobItem(AJobName: String; AMediaName: String): TJobItem;
begin
  Result := GetCallJobItem(AJobName, AMediaName);
  if (Result = nil) then
  begin
    raise Exception.CreateFmt(cCallJobNonExist, [FCallJobName, AMediaName]);
  end;
end;

procedure TCallJobDataItem.InternalSetDataState(const Value: TJobState);
var
  Item: TJobItem;
begin
  inherited InternalSetDataState(Value);
  if (DataState <> jsEdited) then
  begin
    if (FCallMediaName = '') then //TODO
    begin
      Item := GetCallJobItem(FCallJobName, '');
      if (Item <> nil) then
      begin
        Item.SetJobState(Value);
      end;
    end;
  end;
end;

procedure TCallJobDataItem.SetCallMediaName(const Value: String);
begin
  if (FCallMediaName <> Value) then
  begin
    FCallMediaName := Value;
    DoDataChanged();
  end;
end;

destructor TCallJobDataItem.Destroy;
begin
  FCallJobManager.Free();
  inherited Destroy();
end;

procedure TCallJobDataItem.GetGlobalParameterList(AList: TJobOperationParams);
begin
  inherited GetGlobalParameterList(AList);
  AList.Add(cGlobalJobLibPath, NULL);
end;

procedure TCallJobDataItem.Load(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  inherited Load(ANode);
  BeginUpdate();
  try
    ChildNode := ANode.selectSingleNode('CallJobName');
    if ChildNode <> nil then FCallJobName := ChildNode.text;

    ChildNode := ANode.selectSingleNode('CallMediaName');
    if ChildNode <> nil then FCallMediaName := ChildNode.text;
  finally
    EndUpdate();
  end;
end;

procedure TCallJobDataItem.InitData;
begin
  inherited InitData();
  FCallJobName := '';
  FCallMediaName := '';
end;

initialization
  RegisterClass(TCallJobDataItem);

end.
