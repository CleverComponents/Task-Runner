unit CustomJobItems;

interface

uses
  System.Classes, JobClasses, System.SysUtils, System.Variants, OperationClasses, JobConsts, System.Contnrs, Winapi.msxml;

type
  TCustomScriptJobDataItem = class(TJobDataItem)
  private
    FScript: TStrings;
    FOldScript: TStrings;
    FErrorWords: TStrings;
    FParseList: TObjectList;
    FIsParsed: Boolean;
    FScriptFile: String;
    FLogFile: String;
    FOldScriptFile: String;
    FOldLogFile: String;
    FIsUseScriptFile: Boolean;
    FIsUseLogFile: Boolean;
    FErrorBindType: TJobListBindType;
    procedure ParseParameters;
    procedure SetErrorBindType(const Value: TJobListBindType);
    procedure SetErrorWords(const Value: TStrings);
    procedure SetIsUseLogFile(const Value: Boolean);
    procedure SetIsUseScriptFile(const Value: Boolean);
    procedure SetLogFile(const Value: String);
    procedure SetScript(const Value: TStrings);
    procedure SetScriptFile(const Value: String);
    procedure StringsChangeEvent(Sender: TObject);
    procedure CorrectLineParseInfo(ALineNo, APosInLine, ADisplace: Integer);
  protected
    function CanAddParameter(const AParamName: string): Boolean; virtual;
    procedure RecoverParameters(); virtual;
    procedure ReplaceParameters(Params: TJobOperationParams); virtual;
    procedure CheckForErrorsInLog(LogList: TStrings);
    function GetParseLexems: String; virtual;
    function GetWordDelimiters: String; virtual;
    procedure DoDataStateChanged; override;
    procedure DoBeforePerform(Visitor: TJobVisitor); override;
    procedure DoAfterPerform(Visitor: TJobVisitor); override;
    function ReplaceIfNeedParam(AParams: TJobOperationParams; AName: String): String;
    procedure InitData; override;
  public
    constructor Create(AOwner: TJobItem); override;
    destructor Destroy; override;

    function IsValueParameter(AValue: String; var AParamName: String): Boolean;
    function Load(AStream: TStream): Integer; overload; override;
    procedure Load(ANode: IXMLDOMNode); overload; override;
    procedure Store(ANode: IXMLDOMNode); override;
    procedure Assign(Source: TPersistent); override;
    procedure GetParameterList(AList: TJobOperationParams); override;

    property Script: TStrings read FScript write SetScript;
    property ErrorWords: TStrings read FErrorWords write SetErrorWords;
    property ScriptFile: String read FScriptFile write SetScriptFile;
    property LogFile: String read FLogFile write SetLogFile;
    property IsUseScriptFile: Boolean read FIsUseScriptFile write SetIsUseScriptFile;
    property IsUseLogFile: Boolean read FIsUseLogFile write SetIsUseLogFile;
    property ErrorBindType: TJobListBindType read FErrorBindType write SetErrorBindType;
  end;

  TCustomParametersJobDataItem = class(TJobDataItem)
  private
    FParameters: TJobOperationParams;
    procedure SetParameters(const Value: TJobOperationParams);
    procedure ParamsChangeEvent(Sender: TObject);
  protected
    procedure AssignParams(Dest, Source: TJobOperationParams; ALexems: String);
    procedure InitData; override;
  public
    constructor Create(AOwner: TJobItem); override;
    destructor Destroy; override;

    procedure GetParameterList(AList: TJobOperationParams); override;
    procedure GetGlobalParameterList(AList: TJobOperationParams); override;
    function Load(AStream: TStream): Integer; overload; override;
    procedure Load(ANode: IXMLDOMNode); overload; override;
    procedure Store(ANode: IXMLDOMNode); override;
    procedure Assign(Source: TPersistent); override;
    procedure Perform(Visitor: TJobVisitor); override;

    property Parameters: TJobOperationParams read FParameters write SetParameters;
  end;

implementation

uses
  JobUtils, XMLUtils;

type
  TParseParamsInfo = class
    FLineNo: Integer;
    FPosInLine: Integer;
    FParamName: String;
    FReplaceValue: String;
  end;

{ TCustomScriptJobDataItem }

procedure TCustomScriptJobDataItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  BeginUpdate();
  try
    if (Source is TCustomScriptJobDataItem) then
    begin
      FScriptFile := TCustomScriptJobDataItem(Source).ScriptFile;
      FLogFile := TCustomScriptJobDataItem(Source).LogFile;
      FIsUseScriptFile := TCustomScriptJobDataItem(Source).IsUseScriptFile;
      FIsUseLogFile := TCustomScriptJobDataItem(Source).IsUseLogFile;

      FScript.Assign(TCustomScriptJobDataItem(Source).Script);
      FErrorWords.Assign(TCustomScriptJobDataItem(Source).ErrorWords);
      FErrorBindType := TCustomScriptJobDataItem(Source).ErrorBindType;
    end else
    begin
      InitData();
    end;
  finally
    EndUpdate();
  end;
end;

constructor TCustomScriptJobDataItem.Create(AOwner: TJobItem);
begin
  inherited Create(AOwner);
  FScript := TStringList.Create();
  FOldScript := TStringList.Create();
  FErrorWords := TStringList.Create();

  TStringList(FScript).OnChange := StringsChangeEvent;
  TStringList(FErrorWords).OnChange := StringsChangeEvent;

  FParseList := TObjectList.Create();
  FIsParsed := False;
  FScriptFile := '';
  FLogFile := '';
  FIsUseScriptFile := False;
  FIsUseLogFile := False;
  FErrorBindType := btAND;
end;

destructor TCustomScriptJobDataItem.Destroy;
begin
  FParseList.Free();
  FErrorWords.Free();
  FOldScript.Free();
  FScript.Free();
  inherited Destroy();
end;

function TCustomScriptJobDataItem.Load(AStream: TStream): Integer;
var
  R: TReader;
begin
  Result := inherited Load(AStream);
  BeginUpdate();
  R := TReader.Create(AStream, 1024);
  try
    if (Result > 5) then
    begin
      FErrorBindType := TJobListBindType(R.ReadInteger());
    end;
    if (Result > 4) then
    begin
      FScriptFile := R.ReadString();
      FLogFile := R.ReadString();
      FIsUseScriptFile := R.ReadBoolean();
      FIsUseLogFile := R.ReadBoolean();

      if FIsUseScriptFile then
      begin
        FScript.Clear();
      end else
      begin
        FScript.Text := R.ReadString();
      end;
    end else
    begin
      FScript.Text := R.ReadString();
    end;

    if (Result > 1) then
    begin
      FErrorWords.Text := R.ReadString();
    end;
  finally
    R.Free();
    EndUpdate();
  end;
end;

function TCustomScriptJobDataItem.GetParseLexems: String;
begin
  Result := cParseLexems;
end;

procedure TCustomScriptJobDataItem.ParseParameters;
var
  i, j, len, parselen, ParseCount, LineNo, PosInLine: Integer;
  ParseInfo: TParseParamsInfo;
  S, Name, Delimiters: String;
  ParseLexems: String;
  IsDelimiter: Boolean;
begin
  if FIsParsed then Exit;
  FParseList.Clear();
  ParseLexems := GetParseLexems();
  parselen := Length(ParseLexems);
  Delimiters := GetWordDelimiters();
  LineNo := - 1;
  PosInLine := - 1;
  if (ParseLexems = '') then Exit;

  FScript.BeginUpdate();
  try
    for i := 0 to FScript.Count - 1 do
    begin
      S := FScript[i];

      Name := '';
      ParseCount := 0;
      len := Length(S);
      for j := 1 to len do
      begin
        if (ParseCount = parselen) then
        begin
          IsDelimiter := (Pos(S[j], Delimiters) > 0);
          if IsDelimiter or (j = len) then
          begin
            if not IsDelimiter then
            begin
              Name := Name + S[j];
            end;
            if CanAddParameter(Name) then
            begin
              ParseInfo := TParseParamsInfo.Create();
              FParseList.Add(ParseInfo);
              ParseInfo.FLineNo := LineNo;
              ParseInfo.FPosInLine := PosInLine;
              ParseInfo.FParamName := Name;
            end;
            ParseCount := 0;
          end else
          begin
            Name := Name + S[j];
            Continue;
          end;
        end;

        if (S[j] = ParseLexems[ParseCount + 1]) then
        begin
          if (ParseCount = 0) then
          begin
            LineNo := i;
            PosInLine := j;
            Name := '';
          end;
          Inc(ParseCount);
        end else
        begin
          ParseCount := 0;
        end;
      end;
    end;
  finally
    FScript.EndUpdate();
  end;
  FIsParsed := True;
end;

procedure TCustomScriptJobDataItem.RecoverParameters;
begin
  FParseList.Clear();
  FScript.Assign(FOldScript);
  FOldScript.Clear();
end;

procedure TCustomScriptJobDataItem.CorrectLineParseInfo(ALineNo, APosInLine, ADisplace: Integer);
var
  i: Integer;
  ParseInfo: TParseParamsInfo;
begin
  for i := 0 to FParseList.Count - 1 do
  begin
    ParseInfo := TParseParamsInfo(FParseList[i]);
    if (ParseInfo.FLineNo = ALineNo) and (ParseInfo.FPosInLine > APosInLine) then
    begin
      ParseInfo.FPosInLine := ParseInfo.FPosInLine + ADisplace;
    end;
  end;
end;

procedure TCustomScriptJobDataItem.ReplaceParameters(Params: TJobOperationParams);
var
  i, paramlen: Integer;
  ParseInfo: TParseParamsInfo;
  Param: TJobOperationParam;
  S: String;
begin
  FOldScript.Assign(FScript);
  ParseParameters();
  for i := 0 to FParseList.Count - 1 do
  begin
    ParseInfo := TParseParamsInfo(FParseList[i]);
    Param := Params.FindParam(ParseInfo.FParamName);
    if (Param <> nil) then
    begin
      ParseInfo.FReplaceValue := VarToStr(Param.Value);
      S := FScript[ParseInfo.FLineNo];
      paramlen := Length(GetParseLexems() + ParseInfo.FParamName);
      system.Delete(S, ParseInfo.FPosInLine, paramlen);
      system.Insert(ParseInfo.FReplaceValue, S, ParseInfo.FPosInLine);
      CorrectLineParseInfo(ParseInfo.FLineNo, ParseInfo.FPosInLine, Length(ParseInfo.FReplaceValue) - paramlen);
      FScript[ParseInfo.FLineNo] := S;
    end else
    begin
      ParseInfo.FReplaceValue := '';
    end;
  end;
end;

procedure TCustomScriptJobDataItem.Store(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  inherited Store(ANode);

  ChildNode := ANode.ownerDocument.createElement('ErrorBindType');
  ANode.appendChild(ChildNode);
  ChildNode.text := cStoreBindTypeNames[FErrorBindType];

  ChildNode := ANode.ownerDocument.createElement('ScriptFile');
  ANode.appendChild(ChildNode);
  ChildNode.text := FScriptFile;

  ChildNode := ANode.ownerDocument.createElement('LogFile');
  ANode.appendChild(ChildNode);
  ChildNode.text := FLogFile;

  ChildNode := ANode.ownerDocument.createElement('IsUseScriptFile');
  ANode.appendChild(ChildNode);
  ChildNode.text := cStoreBoolean[FIsUseScriptFile];

  ChildNode := ANode.ownerDocument.createElement('IsUseLogFile');
  ANode.appendChild(ChildNode);
  ChildNode.text := cStoreBoolean[FIsUseLogFile];

  if not FIsUseScriptFile then
  begin
    ChildNode := ANode.ownerDocument.createElement('Script');
    ANode.appendChild(ChildNode);
    StringsToXML(FScript, ChildNode);
  end;

  ChildNode := ANode.ownerDocument.createElement('ErrorWords');
  ANode.appendChild(ChildNode);
  ChildNode.text := FErrorWords.Text;
end;

procedure TCustomScriptJobDataItem.DoDataStateChanged;
begin
  inherited DoDataStateChanged();
  if (DataState = jsEdited) then
  begin
    FIsParsed := False;
  end;
end;

procedure TCustomScriptJobDataItem.CheckForErrorsInLog(LogList: TStrings);
var
  i, Cnt: Integer;
  S: String;
begin
  S := LogList.Text;
  Cnt := 0;
  for i := 0 to FErrorWords.Count - 1 do
  begin
    if CheckWordExists(S, FErrorWords[i]) then
    begin
      case FErrorBindType of
        btAND: Inc(Cnt);
        btOR: raise Exception.Create(cScriptError);
      end;
    end;
  end;

  if (FErrorWords.Count > 0) and (Cnt = FErrorWords.Count) then
  begin
    raise Exception.Create(cScriptError);
  end;
end;

procedure TCustomScriptJobDataItem.DoAfterPerform(Visitor: TJobVisitor);
begin
  RecoverParameters();
  FScriptFile := FOldScriptFile;
  FLogFile := FOldLogFile;
  inherited DoAfterPerform(Visitor);
end;

function TCustomScriptJobDataItem.ReplaceIfNeedParam(AParams: TJobOperationParams; AName: String): String;
var
  Param: TJobOperationParam;
begin
  if IsValueParameter(AName, Result) then
  begin
    Param := AParams.FindParam(Result);
    if (Param <> nil) then
    begin
      Result := VarToStr(Param.Value);
    end;
  end;
end;

procedure TCustomScriptJobDataItem.DoBeforePerform(Visitor: TJobVisitor);
begin
  inherited DoBeforePerform(Visitor);
  ReplaceParameters(Visitor.Params);

  FOldScriptFile := FScriptFile;
  FOldLogFile := FLogFile;

  if FIsUseScriptFile then
  begin
    FScriptFile := ReplaceIfNeedParam(Visitor.Params, FScriptFile);
  end;
  if FIsUseLogFile then
  begin
    FLogFile := ReplaceIfNeedParam(Visitor.Params, FLogFile);
  end;
end;

procedure TCustomScriptJobDataItem.GetParameterList(AList: TJobOperationParams);
  procedure AddIfNeedParameter(AParam: String);
  var
    S: String;
  begin
    if IsValueParameter(AParam, S) then
    begin
      AList.Add(S, NULL);
    end;
  end;

var
  i: Integer;
begin
  inherited GetParameterList(AList);
  ParseParameters();
  for i := 0 to FParseList.Count - 1 do
  begin
    AList.Add(TParseParamsInfo(FParseList[i]).FParamName, NULL);
  end;

  if FIsUseScriptFile then
  begin
    AddIfNeedParameter(FScriptFile);
  end;
  if FIsUseLogFile then
  begin
    AddIfNeedParameter(FLogFile);
  end;
end;

function TCustomScriptJobDataItem.IsValueParameter(AValue: String; var AParamName: String): Boolean;
begin
  AParamName := AValue;
  Result := (Pos(cParseLexems, AParamName) = 1);
  if Result then
  begin
    system.Delete(AParamName, 1, Length(cParseLexems));
  end;
end;

function TCustomScriptJobDataItem.GetWordDelimiters: String;
begin
  Result := cWordDelimiters;
end;

procedure TCustomScriptJobDataItem.SetErrorBindType(const Value: TJobListBindType);
begin
  if (FErrorBindType <> Value) then
  begin
    FErrorBindType := Value;
    DoDataChanged();
  end;
end;

procedure TCustomScriptJobDataItem.SetErrorWords(const Value: TStrings);
begin
  FErrorWords.Assign(Value);
end;

procedure TCustomScriptJobDataItem.SetIsUseLogFile(const Value: Boolean);
begin
  if (FIsUseLogFile <> Value) then
  begin
    FIsUseLogFile := Value;
    DoDataChanged();
  end;
end;

procedure TCustomScriptJobDataItem.SetIsUseScriptFile(
  const Value: Boolean);
begin
  if (FIsUseScriptFile <> Value) then
  begin
    FIsUseScriptFile := Value;
    DoDataChanged();
  end;
end;

procedure TCustomScriptJobDataItem.SetLogFile(const Value: String);
begin
  if (FLogFile <> Value) then
  begin
    FLogFile := Value;
    DoDataChanged();
  end;
end;

procedure TCustomScriptJobDataItem.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
end;

procedure TCustomScriptJobDataItem.SetScriptFile(const Value: String);
begin
  if (FScriptFile <> Value) then
  begin
    FScriptFile := Value;
    DoDataChanged();
  end;
end;

procedure TCustomScriptJobDataItem.StringsChangeEvent(Sender: TObject);
begin
  DoDataChanged();
end;

function TCustomScriptJobDataItem.CanAddParameter(const AParamName: string): Boolean;
begin
  Result := True;
end;

procedure TCustomScriptJobDataItem.Load(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
  ind: Integer;
begin
  inherited Load(ANode);
  BeginUpdate();
  try
    ChildNode := ANode.selectSingleNode('ErrorBindType');
    if ChildNode <> nil then
    begin
      ind := GetArrayIndexByName(ChildNode.text, cStoreBindTypeNames);
      if (ind > -1) then FErrorBindType := TJobListBindType(ind);
    end;

    ChildNode := ANode.selectSingleNode('ScriptFile');
    if ChildNode <> nil then FScriptFile := ChildNode.text;

    ChildNode := ANode.selectSingleNode('LogFile');
    if ChildNode <> nil then FLogFile := ChildNode.text;

    ChildNode := ANode.selectSingleNode('IsUseScriptFile');
    if ChildNode <> nil then
    begin
      ind := GetArrayIndexByName(ChildNode.text, cStoreBoolean);
      if (ind > -1) then FIsUseScriptFile := Boolean(ind);
    end;

    ChildNode := ANode.selectSingleNode('IsUseLogFile');
    if ChildNode <> nil then
    begin
      ind := GetArrayIndexByName(ChildNode.text, cStoreBoolean);
      if (ind > -1) then FIsUseLogFile := Boolean(ind);
    end;

    if not FIsUseScriptFile then
    begin
      ChildNode := ANode.selectSingleNode('Script');
      if ChildNode <> nil then
      begin
        StringsToXML(FScript, ChildNode);
        XMLToStrings(FScript, ChildNode);
      end;
    end;

    ChildNode := ANode.selectSingleNode('ErrorWords');
    if ChildNode <> nil then FErrorWords.Text := ChildNode.text;
  finally
    EndUpdate();
  end;
end;

procedure TCustomScriptJobDataItem.InitData;
begin
  inherited InitData();
  FScriptFile := '';
  FLogFile := '';
  FIsUseScriptFile := False;
  FIsUseLogFile := False;

  FScript.Clear();
  FErrorWords.Clear();
  FErrorBindType := btAND;
end;

{ TCustomParametersJobDataItem }

procedure TCustomParametersJobDataItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  BeginUpdate();
  try
    if (Source is TCustomParametersJobDataItem) then
    begin
      FParameters.Assign(TCustomParametersJobDataItem(Source).Parameters);
    end else
    begin
      InitData();
    end;
  finally
    EndUpdate();
  end;
end;

procedure TCustomParametersJobDataItem.AssignParams(Dest, Source: TJobOperationParams; ALexems: String);
var
  i: Integer;
  ParamDest, ParamSource: TJobOperationParam;
  S: String;
begin
  for i := 0 to Dest.Count - 1 do
  begin
    ParamDest := Dest.Items[i];
    S := VarToStr(ParamDest.Value);
    if (Pos(ALexems, S) = 1) then
    begin
      system.Delete(S, 1, Length(ALexems));
      ParamSource := Source.FindParam(S);
      if (ParamSource <> nil) then
      begin
        ParamDest.Value := Null;
        ParamDest.Value := ParamSource.Value;
      end;
    end;
  end;
end;

constructor TCustomParametersJobDataItem.Create(AOwner: TJobItem);
begin
  inherited Create(AOwner);
  FParameters := TJobOperationParams.Create();
  FParameters.OnChanged := ParamsChangeEvent;
end;

destructor TCustomParametersJobDataItem.Destroy;
begin
  FParameters.Free();
  inherited Destroy();
end;

procedure TCustomParametersJobDataItem.GetGlobalParameterList(AList: TJobOperationParams);
var
  i: Integer;
  S: String;
begin
  inherited GetGlobalParameterList(AList);
  for i := 0 to FParameters.Count - 1 do
  begin
    S := VarToStr(FParameters.Items[i].Value);
    if (Pos(cGlobalParamsParseLexems, S) = 1) then
    begin
      system.Delete(S, 1, Length(cGlobalParamsParseLexems));
      AList.Add(S, NULL);
    end;
  end;
end;

procedure TCustomParametersJobDataItem.GetParameterList(AList: TJobOperationParams);
var
  i: Integer;
  S: String;
begin
  inherited GetParameterList(AList);
  for i := 0 to FParameters.Count - 1 do
  begin
    S := VarToStr(FParameters.Items[i].Value);
    if (Pos(cParseLexems, S) = 1) then
    begin
      system.Delete(S, 1, Length(cParseLexems));
      AList.Add(S, NULL);
    end;
  end;
end;

function TCustomParametersJobDataItem.Load(AStream: TStream): Integer;
begin
  Result := inherited Load(AStream);
  BeginUpdate();
  try
    FParameters.Load(AStream);
  finally
    EndUpdate();
  end;
end;

procedure TCustomParametersJobDataItem.InitData;
begin
  inherited InitData();
  FParameters.Clear();
end;

procedure TCustomParametersJobDataItem.Load(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  inherited Load(ANode);
  BeginUpdate();
  try
    ChildNode := ANode.selectSingleNode('ParamList');
    if ChildNode <> nil then FParameters.Load(ChildNode);
  finally
    EndUpdate();
  end;
end;

procedure TCustomParametersJobDataItem.ParamsChangeEvent(Sender: TObject);
begin
  DoDataChanged();
end;

procedure TCustomParametersJobDataItem.Perform(Visitor: TJobVisitor);
var
  AGlobalParams: TJobOperationParams;
begin
  inherited Perform(Visitor);
  if (Owner = nil) or (Owner.JobManager = nil) then Exit;
  AGlobalParams := Owner.JobManager.GetGlobalParameters();
  if (AGlobalParams <> nil) then
  begin
    AssignParams(Parameters, AGlobalParams, cGlobalParamsParseLexems);
  end;
end;

procedure TCustomParametersJobDataItem.SetParameters(const Value: TJobOperationParams);
begin
  FParameters.Assign(Value);
end;

procedure TCustomParametersJobDataItem.Store(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  inherited Store(ANode);
  ChildNode := ANode.ownerDocument.createElement('ParamList');
  ANode.appendChild(ChildNode);
  FParameters.Store(ChildNode);
end;

initialization
  AddCDataNodeName('Script');

end.
