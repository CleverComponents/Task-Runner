unit SQLScriptJobItem;

interface

uses
  System.Classes, JobClasses, System.SysUtils, System.Variants, CustomJobItems, JobUtils, ADODB, OperationClasses, JobConsts,
  Winapi.msxml, Data.DB;

type
  TSQLScriptJobDataItem = class(TCustomScriptJobDataItem)
  private
    FConnectionInfo: TSQLConnectionInfo;
    FOldConnectionInfo: TSQLConnectionInfo;
    FPerformWith: TSQLPerformWith;
    FJobUtilities: TJobUtilities;
    procedure PerformWithADO(Visitor: TJobVisitor);
    procedure PerformWithOSQL(Visitor: TJobVisitor);
    procedure SetPerformWith(const Value: TSQLPerformWith);
    procedure SetConnectionInfo(const Value: TSQLConnectionInfo);
    procedure ConnectionChangeEvent(Source: TObject);
  protected
    procedure DoBeforePerform(Visitor: TJobVisitor); override;
    procedure DoAfterPerform(Visitor: TJobVisitor); override;
    function GetWordDelimiters: String; override;
    procedure InitData; override;
  public
    constructor Create(AOwner: TJobItem); override;
    destructor Destroy; override;

    function Load(AStream: TStream): Integer; overload; override;
    procedure Load(ANode: IXMLDOMNode); overload; override;
    procedure Store(ANode: IXMLDOMNode); override;
    procedure Perform(Visitor: TJobVisitor); override;
    procedure Assign(Source: TPersistent); override;
    procedure GetParameterList(AList: TJobOperationParams); override;

    property ConnectionInfo: TSQLConnectionInfo read FConnectionInfo write SetConnectionInfo;
    property PerformWith: TSQLPerformWith read FPerformWith write SetPerformWith;
  end;

implementation

uses
  XMLUtils;

{ TSQLScriptJobDataItem }

procedure TSQLScriptJobDataItem.Assign(Source: TPersistent);
var
  Data: TSQLScriptJobDataItem;
begin
  inherited Assign(Source);
  BeginUpdate();
  try
    if (Source is TSQLScriptJobDataItem) then
    begin
      Data := TSQLScriptJobDataItem(Source);
      FConnectionInfo.Assign(Data.ConnectionInfo);
      FPerformWith := Data.PerformWith;
    end else
    begin
      InitData();
    end;
  finally
    EndUpdate();
  end;
end;

procedure TSQLScriptJobDataItem.ConnectionChangeEvent(Source: TObject);
begin
  DoDataChanged();
end;

constructor TSQLScriptJobDataItem.Create(AOwner: TJobItem);
var
  i: Integer;
begin
  inherited Create(AOwner);
  FJobUtilities := TJobUtilities.Create();
  FConnectionInfo := TSQLConnectionInfo.Create();
  FConnectionInfo.OnChanged := ConnectionChangeEvent;
  FOldConnectionInfo := TSQLConnectionInfo.Create();
  FPerformWith := spOSQLUtilite;
  for i := Low(cSQLErrorWords) to High(cSQLErrorWords) do
  begin
    ErrorWords.Add(cSQLErrorWords[i]);
  end;
end;

destructor TSQLScriptJobDataItem.Destroy;
begin
  FOldConnectionInfo.Free();
  FConnectionInfo.Free();
  FJobUtilities.Free();
  inherited Destroy();
end;

procedure TSQLScriptJobDataItem.DoAfterPerform(Visitor: TJobVisitor);
begin
  FConnectionInfo.Assign(FOldConnectionInfo);
  inherited DoAfterPerform(Visitor);
end;

procedure TSQLScriptJobDataItem.DoBeforePerform(Visitor: TJobVisitor);
begin
  inherited DoBeforePerform(Visitor);

  FOldConnectionInfo.Assign(FConnectionInfo);

  FConnectionInfo.Server := ReplaceIfNeedParam(Visitor.Params, FConnectionInfo.Server);
  FConnectionInfo.User := ReplaceIfNeedParam(Visitor.Params, FConnectionInfo.User);
  FConnectionInfo.Password := ReplaceIfNeedParam(Visitor.Params, FConnectionInfo.Password);
  FConnectionInfo.Database := ReplaceIfNeedParam(Visitor.Params, FConnectionInfo.Database);
  FConnectionInfo.TimeOut := ReplaceIfNeedParam(Visitor.Params, FConnectionInfo.TimeOut);
  FConnectionInfo.ConnectionString := ReplaceIfNeedParam(Visitor.Params, FConnectionInfo.ConnectionString);
end;

procedure TSQLScriptJobDataItem.GetParameterList(AList: TJobOperationParams);
  procedure AddIfNeedParameter(AParam: String);
  var
    S: String;
  begin
    if IsValueParameter(AParam, S) then
    begin
      AList.Add(S, NULL);
    end;
  end;

begin
  inherited GetParameterList(AList);
  AddIfNeedParameter(FConnectionInfo.Server);
  AddIfNeedParameter(FConnectionInfo.User);
  AddIfNeedParameter(FConnectionInfo.Password);
  AddIfNeedParameter(FConnectionInfo.Database);
  AddIfNeedParameter(FConnectionInfo.TimeOut);
  AddIfNeedParameter(FConnectionInfo.ConnectionString);
end;

function TSQLScriptJobDataItem.GetWordDelimiters: String;
begin
  Result := cSQLScriptWordDelimiters;
end;

procedure TSQLScriptJobDataItem.InitData;
begin
  inherited InitData();
  FConnectionInfo.Assign(nil);
  FPerformWith := spOSQLUtilite;
end;

function TSQLScriptJobDataItem.Load(AStream: TStream): Integer;
var
  R: TReader;
begin
  Result := inherited Load(AStream);

  BeginUpdate();
  try
    if (Result > 3) then
    begin
      AStream.Read(FPerformWith, SizeOf(FPerformWith));
      FConnectionInfo.Load(AStream);
    end else
    begin
      R := TReader.Create(AStream, 1024);
      try
        FConnectionInfo.Server := R.ReadString();
        FConnectionInfo.User := R.ReadString();
        FConnectionInfo.Password := R.ReadString();
        FConnectionInfo.Database := R.ReadString();

        if (Result > 2) then
        begin
          FConnectionInfo.TimeOut := R.ReadString();
        end else
        begin
          FConnectionInfo.TimeOut := IntToStr(R.ReadInteger());
        end;
        FPerformWith := TSQLPerformWith(R.ReadInteger());
      finally
        R.Free();
      end;
    end;
  finally
    EndUpdate();
  end;
end;

procedure TSQLScriptJobDataItem.Load(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
  ind: Integer;
begin
  inherited Load(ANode);
  BeginUpdate();
  try
    ChildNode := ANode.selectSingleNode('PerformWith');
    if ChildNode <> nil then
    begin
      ind := GetArrayIndexByName(ChildNode.text, cStoreSQLPerformWithNames);
      if (ind > -1) then FPerformWith := TSQLPerformWith(ind);
    end;

    ChildNode := ANode.selectSingleNode('ConnectionInfo');
    if ChildNode <> nil then FConnectionInfo.Load(ChildNode);
  finally
    EndUpdate();
  end;
end;

procedure TSQLScriptJobDataItem.Perform(Visitor: TJobVisitor);
begin
  inherited Perform(Visitor);
  case FPerformWith of
    spOSQLUtilite: PerformWithOSQL(Visitor);
    spADOLibrary: PerformWithADO(Visitor);
  end;
end;

procedure TSQLScriptJobDataItem.PerformWithADO(Visitor: TJobVisitor);
var
  i: Integer;
  Query: TADOQuery;
  S: String;
  AOutput: TStrings;
begin
  Query := TADOQuery.Create(nil);
  AOutput := TStringList.Create();
  try
    if (FConnectionInfo.ConnectionString <> '') then
    begin
      Query.ConnectionString := FConnectionInfo.ConnectionString;
    end else
    begin
      Query.ConnectionString := FConnectionInfo.CreateConnectionString();
    end;
    if IsUseScriptFile then
    begin
      Query.SQL.LoadFromFile(ScriptFile);
    end else
    begin
      Query.SQL.Assign(Script);
    end;
    Query.Open();

    S := '';
    for i := 0 to Query.FieldCount - 1 do
    begin
      S := S + Query.Fields[i].FieldName + ', ';
    end;
    AOutput.Add(S);
    while not Query.Eof do
    begin
      S := '';
      for i := 0 to Query.FieldCount - 1 do
      begin
        S := S + Query.Fields[i].AsString + ', ';
      end;
      AOutput.Add(S);
      Query.Next();
    end;

    if IsUseLogFile then
    begin
      AOutput.SaveToFile(LogFile);
    end else
    begin
      Visitor.Log.Assign(AOutput);
    end;
  finally
    AOutput.Free();
    Query.Free();
  end;
end;

procedure TSQLScriptJobDataItem.PerformWithOSQL(Visitor: TJobVisitor);
var
  CommandLine: String;
  AOutput, AErrors: TStrings;
  ATimeOut: Integer;
begin
  try
    ATimeOut := StrToInt(FConnectionInfo.TimeOut);
  except
    ATimeOut := - 1;
  end;
  AOutput := TStringList.Create();
  AErrors := TStringList.Create();
  try
    CommandLine := 'osql.exe /U ' + FConnectionInfo.User + ' /d ' + FConnectionInfo.Database
      + ' /P ' + FConnectionInfo.Password + ' /S ' + FConnectionInfo.Server + ' /w8192 /n';
    if IsUseScriptFile then
    begin
      CommandLine := CommandLine + ' /i ' + ScriptFile;
    end else
    begin
      CommandLine := CommandLine + ' /i ' + cJobInputFile;
    end;
    if IsUseLogFile then
    begin
      CommandLine := CommandLine + ' /o ' + LogFile;
    end else
    begin
      CommandLine := CommandLine + ' /o ' + cJobOutFile;
    end;
    if (ATimeOut > 0) then
    begin
      CommandLine := CommandLine + ' /l ' + IntToStr(ATimeOut);
    end;

    FJobUtilities.PerformFile(CommandLine, Script, AOutput, AErrors);

    if IsUseLogFile then
    begin
      AOutput.Add(Format(cJobLogInFile, [LogFile]));
    end else
    begin
      CheckForErrorsInLog(AOutput);
    end;
  finally
    Visitor.Log.AddStrings(AOutput);
    Visitor.Errors.AddStrings(AErrors);
    AErrors.Free();
    AOutput.Free();
  end;
end;

procedure TSQLScriptJobDataItem.SetConnectionInfo(const Value: TSQLConnectionInfo);
begin
  FConnectionInfo.Assign(Value);
end;

procedure TSQLScriptJobDataItem.SetPerformWith(const Value: TSQLPerformWith);
begin
  if (FPerformWith <> Value) then
  begin
    FPerformWith := Value;
    DoDataChanged();
  end;
end;

procedure TSQLScriptJobDataItem.Store(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  inherited Store(ANode);

  ChildNode := ANode.ownerDocument.createElement('PerformWith');
  ANode.appendChild(ChildNode);
  ChildNode.text := cStoreSQLPerformWithNames[FPerformWith];
  
  ChildNode := ANode.ownerDocument.createElement('ConnectionInfo');
  ANode.appendChild(ChildNode);
  FConnectionInfo.Store(ChildNode);
end;

initialization
  RegisterClass(TSQLScriptJobDataItem);

end.
