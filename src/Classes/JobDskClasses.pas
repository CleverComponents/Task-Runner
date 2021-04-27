unit JobDskClasses;

interface

uses
  System.Classes, System.Generics.Collections, Winapi.msxml;

type
  TJobDskItem = class(TPersistent)
  private
    FName: string;
  protected
    procedure Load(AStream: TStream; AVersion: Integer); overload; virtual;
    procedure Store(AStream: TStream); overload; virtual;
    procedure Load(ANode: IXMLDOMNode); overload; virtual;
    procedure Store(ANode: IXMLDOMNode); overload; virtual;
  public
    constructor Create(ADataItemFullName: string); virtual;
    procedure Assign(Source: TPersistent); override;
    property Name: string read FName;
  end;

  TJobDskItemClass = class of TJobDskItem;

  TJobDskItemStorage = class
  private
    FList: TObjectList<TJobDskItem>;
    FMaxItemsCount: Integer;
    FName: string;

    procedure SetMaxItemsCount(const Value: Integer);
  public
    constructor Create(AName: string);
    destructor Destroy; override;

    procedure Load(AStream: TStream; AVersion: Integer); overload; virtual;
    procedure Store(AStream: TStream); overload; virtual;
    procedure Load(ANode: IXMLDOMNode); overload; virtual;
    procedure Store(ANode: IXMLDOMNode); overload; virtual;
    function GetItem(AItemClass: TJobDskItemClass; ADataItemFullName: string): TJobDskItem;
    procedure PutItem(AJobDskItem: TJobDskItem);
    procedure Assign(Source: TJobDskItemStorage); virtual;

    property MaxItemsCount: Integer read FMaxItemsCount write SetMaxItemsCount;
    property Name: string read FName;
  end;

  TJobDskMediaStorage = class
  private
    FList: TObjectList<TJobDskItemStorage>;
    FMaxItemsCount: Integer;
    FCurrentItem: TJobDskItemStorage;

    function GetItems(Index: Integer): TJobDskItemStorage;
    function GetItemsCount: Integer;
    procedure SetMaxItemsCount(const Value: Integer);
    function GetNamedItemsCount: Integer;
    procedure RestrictItems;
  protected
    constructor CreateInstance(ADummy: Integer = 0);
    class function AccessInstance(Request: Integer): TJobDskMediaStorage;
  public
    constructor Create;
    destructor Destroy; override;

    class function Instance: TJobDskMediaStorage;
    class procedure ReleaseInstance;
    procedure Load(AStream: TStream; AVersion: Integer); overload; virtual;
    procedure Store(AStream: TStream); overload; virtual;
    procedure Load(ANode: IXMLDOMNode); overload; virtual;
    procedure Store(ANode: IXMLDOMNode); overload; virtual;
    procedure SetCurrentItem(AJobDskItemStorageName: string);
    procedure RenameCurrentItem(AJobDskItemStorageName: string);

    property Items[Index: Integer]: TJobDskItemStorage read GetItems;
    property ItemsCount: Integer read GetItemsCount write SetMaxItemsCount;
    property MaxItemsCount: Integer read FMaxItemsCount;
    property CurrentItem: TJobDskItemStorage read FCurrentItem;
  end;

implementation

uses
  SysUtils, JobConsts;

{ TJobDskItemStorage }

procedure TJobDskItemStorage.Assign(Source: TJobDskItemStorage);
var
  i: Integer;
  Item, SrcItem: TJobDskItem;
begin
  FName := Source.Name;
  FList.Clear();
  for i := 0 to Source.FList.Count - 1 do
  begin
    SrcItem := Source.FList[i];
    Item := TJobDskItemClass(SrcItem.ClassType).Create('');
    FList.Add(Item);
    Item.Assign(SrcItem);
  end;
end;

constructor TJobDskItemStorage.Create(AName: string);
begin
  inherited Create();

  FList := TObjectList<TJobDskItem>.Create();
  FName := AName;
  FMaxItemsCount := cMinDskEditorsCount;
end;

destructor TJobDskItemStorage.Destroy;
begin
  FList.Free();
  inherited Destroy();
end;

function TJobDskItemStorage.GetItem(AItemClass: TJobDskItemClass; ADataItemFullName: string): TJobDskItem;
var
  i: Integer;
  AItem: TJobDskItem;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    AItem := FList[i];
    if (AItem.ClassType = AItemClass)
      and (CompareText(AItem.Name, ADataItemFullName) = 0) then
    begin
      Result := AItem;
      FList.Move(i, 0);
      Break;
    end;
  end;
end;

procedure TJobDskItemStorage.Load(AStream: TStream; AVersion: Integer);
var
  i, Cnt: Integer;
  AItemClass: TJobDskItemClass;
  R: TReader;
  AItem: TJobDskItem;
begin
  FList.Clear();
  R := TReader.Create(AStream, 1024);
  try
    FName := R.ReadString();
  finally
    R.Free();
  end;
  AStream.Read(Cnt, SizeOf(Integer));
  for i := 0 to Cnt - 1 do
  begin
    R := TReader.Create(AStream, 1024);
    try
      AItemClass := TJobDskItemClass(GetClass(R.ReadString()));
    finally
      R.Free();
    end;
    AItem := AItemClass.Create('');
    FList.Add(AItem);
    AItem.Load(AStream, AVersion);
  end;
end;

procedure TJobDskItemStorage.Load(ANode: IXMLDOMNode);
var
  i: Integer;
  AItemClass: TJobDskItemClass;
  AItem: TJobDskItem;
  MainNode, ChildNode: IXMLDOMNode;
begin
  FName := (ANode as IXMLDOMElement).getAttribute('name');

  FList.Clear();
  MainNode := ANode.selectSingleNode('Editors');
  if (MainNode = nil) then Exit;

  for i := 0 to MainNode.childNodes.length - 1 do
  begin
    ChildNode := MainNode.childNodes.item[i];
    AItemClass := TJobDskItemClass(GetClass((ChildNode as IXMLDOMElement).getAttribute('classname')));
    if (AItemClass <> nil) then
    begin
      AItem := AItemClass.Create('');
      FList.Add(AItem);
      AItem.Load(ChildNode);
    end;
  end;
end;

procedure TJobDskItemStorage.PutItem(AJobDskItem: TJobDskItem);
var
  AItem: TJobDskItem;
begin
  AItem := GetItem(TJobDskItemClass(AJobDskItem.ClassType), AJobDskItem.Name);
  if (AItem <> nil) then
  begin
    FList.Remove(AItem);
  end;
  FList.Insert(0, AJobDskItem);
  if (FList.Count > FMaxItemsCount) then
  begin
    FList.Delete(FList.Count - 1);
  end;
end;

procedure TJobDskItemStorage.SetMaxItemsCount(const Value: Integer);
begin
  if (Value < cMinDskEditorsCount) then
  begin
    FMaxItemsCount := cMinDskEditorsCount;
  end else
  begin
    FMaxItemsCount := Value;
  end;
  while (FList.Count > FMaxItemsCount) do
  begin
    FList.Delete(FList.Count - 1);
  end;
end;

procedure TJobDskItemStorage.Store(AStream: TStream);
var
  i: Integer;
  W: TWriter;
  AItem: TJobDskItem;
begin
  W := TWriter.Create(AStream, 1024);
  try
    W.WriteString(FName);
  finally
    W.Free();
  end;
  AStream.Write(FList.Count, SizeOf(Integer));
  for i := 0 to FList.Count - 1 do
  begin
    AItem := FList[i];
    W := TWriter.Create(AStream, 1024);
    try
      W.WriteString(AItem.ClassName);
    finally
      W.Free();
    end;
    AItem.Store(AStream);
  end;
end;

procedure TJobDskItemStorage.Store(ANode: IXMLDOMNode);
var
  i: Integer;
  AItem: TJobDskItem;
  MainNode, ChildNode: IXMLDOMNode;
begin
  (ANode as IXMLDOMElement).setAttribute('name', FName);
  MainNode := ANode.ownerDocument.createElement('Editors');
  ANode.appendChild(MainNode);
  for i := 0 to FList.Count - 1 do
  begin
    AItem := FList[i];

    ChildNode := MainNode.ownerDocument.createElement('Editor');
    MainNode.appendChild(ChildNode);
    (ChildNode as IXMLDOMElement).setAttribute('classname', AItem.ClassName);

    AItem.Store(ChildNode);
  end;
end;

{ TJobDskMediaStorage }

var
 FInstance: TJobDskMediaStorage = nil;

class function TJobDskMediaStorage.AccessInstance(Request: Integer): TJobDskMediaStorage;
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

constructor TJobDskMediaStorage.Create;
begin
  inherited Create;
  raise Exception.CreateFmt('Access class %s through Instance only',
      [ClassName]);
end;

constructor TJobDskMediaStorage.CreateInstance(ADummy: Integer);
begin
  inherited Create();

  FList := TObjectList<TJobDskItemStorage>.Create();
  FMaxItemsCount := cMinDskMediaCount;
end;

destructor TJobDskMediaStorage.Destroy;
begin
  if AccessInstance(0) = Self then AccessInstance(2);
  FList.Free();
  inherited Destroy();
end;

function TJobDskMediaStorage.GetItems(Index: Integer): TJobDskItemStorage;
begin
  Result := FList[Index];
end;

function TJobDskMediaStorage.GetItemsCount: Integer;
begin
  Result := FList.Count;
end;

class function TJobDskMediaStorage.Instance: TJobDskMediaStorage;
begin
  Result := AccessInstance(1);
end;

procedure TJobDskMediaStorage.Load(AStream: TStream; AVersion: Integer);
var
  i, Cnt: Integer;
  AItem: TJobDskItemStorage;
begin
  FCurrentItem := nil;
  FList.Clear();
  AStream.Read(Cnt, SizeOf(Integer));
  for i := 0 to Cnt - 1 do
  begin
    AItem := TJobDskItemStorage.Create('');
    FList.Add(AItem);
    AItem.Load(AStream, AVersion);
  end;
end;

class procedure TJobDskMediaStorage.ReleaseInstance;
begin
  AccessInstance(0).Free();
end;

procedure TJobDskMediaStorage.RenameCurrentItem(AJobDskItemStorageName: string);
var
  i: Integer;
  AItem: TJobDskItemStorage;
begin
  if (FCurrentItem = nil) then Exit;
  if (CompareText(FCurrentItem.Name, AJobDskItemStorageName) = 0) then Exit;
  for i := 0 to FList.Count - 1 do
  begin
    AItem := FList[i];
    if (CompareText(AItem.Name, AJobDskItemStorageName) = 0) then
    begin
      FList.Delete(i);
      Break;
    end;
  end;
  FCurrentItem.FName := AJobDskItemStorageName;
  RestrictItems();
end;

procedure TJobDskMediaStorage.SetCurrentItem(AJobDskItemStorageName: string);
var
  i: Integer;
  AItem: TJobDskItemStorage;
begin
  FCurrentItem := nil;
  for i := 0 to FList.Count - 1 do
  begin
    AItem := FList[i];
    if (CompareText(AItem.Name, AJobDskItemStorageName) = 0) then
    begin
      FCurrentItem := AItem;
      FList.Move(i, 0);
      Break;
    end;
  end;
  if (FCurrentItem = nil) then
  begin
    FCurrentItem := TJobDskItemStorage.Create(AJobDskItemStorageName);
    FList.Insert(0, FCurrentItem);
    RestrictItems();
  end;
end;

procedure TJobDskMediaStorage.RestrictItems;
begin
  if (GetNamedItemsCount() > FMaxItemsCount) then
  begin
    FList.Delete(FList.Count - 1);
  end;
end;

procedure TJobDskMediaStorage.SetMaxItemsCount(const Value: Integer);
begin
  if (Value < cMinDskMediaCount) then
  begin
    FMaxItemsCount := cMinDskMediaCount;
  end else
  begin
    FMaxItemsCount := Value;
  end;
  while (FList.Count > FMaxItemsCount) do
  begin
    FList.Delete(FList.Count - 1);
  end;
end;

function TJobDskMediaStorage.GetNamedItemsCount: Integer;
var
  i: Integer;
  AItem: TJobDskItemStorage;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
  begin
    AItem := FList[i];
    if (AItem.Name <> '') then
    begin
      Inc(Result);
    end;
  end;
end;

procedure TJobDskMediaStorage.Store(AStream: TStream);
var
  i, Cnt: Integer;
  AItem: TJobDskItemStorage;
begin
  Cnt := GetNamedItemsCount();
  AStream.Write(Cnt, SizeOf(Integer));
  for i := 0 to FList.Count - 1 do
  begin
    AItem := FList[i];
    if (AItem.Name <> '') then
    begin
      AItem.Store(AStream);
    end;
  end;
end;

procedure TJobDskMediaStorage.Load(ANode: IXMLDOMNode);
var
  i: Integer;
  AItem: TJobDskItemStorage;
  ChildNode: IXMLDOMNode;
begin
  FCurrentItem := nil;
  FList.Clear();
  for i := 0 to ANode.childNodes.length - 1 do
  begin
    ChildNode := ANode.childNodes.item[i];
    AItem := TJobDskItemStorage.Create('');
    FList.Add(AItem);
    AItem.Load(ChildNode);
  end;
end;

procedure TJobDskMediaStorage.Store(ANode: IXMLDOMNode);
var
  i: Integer;
  AItem: TJobDskItemStorage;
  ChildNode: IXMLDOMNode;
begin
  for i := 0 to FList.Count - 1 do
  begin
    AItem := FList[i];
    if (AItem.Name <> '') then
    begin
      ChildNode := ANode.ownerDocument.createElement('Project');
      ANode.appendChild(ChildNode);
      AItem.Store(ChildNode);
    end;
  end;
end;

{ TJobDskItem }

procedure TJobDskItem.Assign(Source: TPersistent);
begin
  if (Source is TJobDskItem) then
  begin
    FName := TJobDskItem(Source).Name;
  end else
  begin
    inherited Assign(Source);
  end;
end;

constructor TJobDskItem.Create(ADataItemFullName: string);
begin
  inherited Create();
  FName := ADataItemFullName;
end;

procedure TJobDskItem.Load(AStream: TStream; AVersion: Integer);
var
  R: TReader;
begin
  R := TReader.Create(AStream, 1024);
  try
    FName := R.ReadString();
  finally
    R.Free();
  end;
end;

procedure TJobDskItem.Store(AStream: TStream);
var
  W: TWriter;
begin
  W := TWriter.Create(AStream, 1024);
  try
    W.WriteString(FName);
  finally
    W.Free();
  end;
end;

procedure TJobDskItem.Load(ANode: IXMLDOMNode);
begin
  FName := (ANode as IXMLDOMElement).getAttribute('name');
end;

procedure TJobDskItem.Store(ANode: IXMLDOMNode);
begin
  (ANode as IXMLDOMElement).setAttribute('name', FName);
end;

initialization

finalization
  TJobDskMediaStorage.ReleaseInstance();

end.
