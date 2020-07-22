unit CustomRunJobItem;

interface

uses
  Classes, JobClasses, CustomJobItems, OperationClasses, JobUtils, Winapi.msxml;

type
  TCustomRunDataItem = class(TCustomScriptJobDataItem)
  private
    FCommandLine: string;
    FParamDelimiter: string;
    FParamPrefix: string;
    FJobUtilities: TJobUtilities;
    FScriptExt: string;
    procedure SetCommandLine(const Value: string);
    procedure SetParamDelimiter(const Value: string);
    procedure SetParamPrefix(const Value: string);
    procedure SetScriptExt(const Value: string);
    procedure AssignDefaultParseParams;
  protected
    function CanAddParameter(const AParamName: string): Boolean; override;
    function GetParseLexems: String; override;
    function GetWordDelimiters: String; override;
    procedure DoBeforePerform(Visitor: TJobVisitor); override;
    procedure DoAfterPerform(Visitor: TJobVisitor); override;
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
    property CommandLine: string read FCommandLine write SetCommandLine;
    property ParamPrefix: string read FParamPrefix write SetParamPrefix;
    property ParamDelimiter: string read FParamDelimiter write SetParamDelimiter;
    property ScriptExt: string read FScriptExt write SetScriptExt;
  end;

implementation

uses
  JobConsts, SysUtils, XMLUtils;

{ TCustomRunDataItem }

procedure TCustomRunDataItem.Assign(Source: TPersistent);
var
  Data: TCustomRunDataItem;
begin
  inherited Assign(Source);
  BeginUpdate();
  try
    if (Source is TCustomRunDataItem) then
    begin
      Data := TCustomRunDataItem(Source);
      FCommandLine := Data.CommandLine;
      FParamDelimiter := Data.ParamDelimiter;
      FParamPrefix := Data.ParamPrefix;
      FScriptExt := Data.ScriptExt;
    end else
    begin
      InitData();
    end;
  finally
    EndUpdate();
  end;
end;

procedure TCustomRunDataItem.AssignDefaultParseParams;
begin
  FParamDelimiter := inherited GetWordDelimiters();
  FParamPrefix := inherited GetParseLexems();
  FScriptExt := '';
end;

function TCustomRunDataItem.CanAddParameter(const AParamName: string): Boolean;
begin
  Result := (AParamName <> cScriptClause);
end;

constructor TCustomRunDataItem.Create(AOwner: TJobItem);
begin
  inherited Create(AOwner);
  FJobUtilities := TJobUtilities.Create();
  AssignDefaultParseParams();
end;

destructor TCustomRunDataItem.Destroy;
begin
  FJobUtilities.Free();
  inherited Destroy();
end;

procedure TCustomRunDataItem.DoAfterPerform(Visitor: TJobVisitor);
begin
  inherited DoAfterPerform(Visitor);
  Script.Delete(Script.Count - 1);
end;

procedure TCustomRunDataItem.DoBeforePerform(Visitor: TJobVisitor);
begin
  Script.Add(FCommandLine);
  inherited DoBeforePerform(Visitor);
end;

procedure TCustomRunDataItem.GetParameterList(AList: TJobOperationParams);
begin
  Script.Add(FCommandLine);
  try
    inherited GetParameterList(AList);
  finally
    Script.Delete(Script.Count - 1);
  end;
end;

function TCustomRunDataItem.GetParseLexems: String;
begin
  Result := FParamPrefix;
end;

function TCustomRunDataItem.GetWordDelimiters: String;
begin
  Result := FParamDelimiter;
end;

procedure TCustomRunDataItem.InitData;
begin
  inherited InitData();
  FCommandLine := '';
  AssignDefaultParseParams();
end;

function TCustomRunDataItem.Load(AStream: TStream): Integer;
var
  R: TReader;
begin
  Result := inherited Load(AStream);
  BeginUpdate();
  try
    R := TReader.Create(AStream, 1024);
    try
      FCommandLine := R.ReadString();
      FParamDelimiter := R.ReadString();
      FParamPrefix := R.ReadString();
    finally
      R.Free();
    end;
  finally
    EndUpdate();
  end;
end;

procedure TCustomRunDataItem.Load(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  inherited Load(ANode);
  BeginUpdate();
  try
    ChildNode := ANode.selectSingleNode('CommandLine');
    if ChildNode <> nil then FCommandLine := ChildNode.text;

    ChildNode := ANode.selectSingleNode('ParamDelimiter');
    if ChildNode <> nil then FParamDelimiter := ChildNode.text;

    ChildNode := ANode.selectSingleNode('ParamPrefix');
    if ChildNode <> nil then FParamPrefix := ChildNode.text;

    ChildNode := ANode.selectSingleNode('ScriptExt');
    if ChildNode <> nil then FScriptExt := ChildNode.text;

  finally
    EndUpdate();
  end;
end;

procedure TCustomRunDataItem.Perform(Visitor: TJobVisitor);
var
  ACmdLine, s, ext: string;
  AOutput, AErrors: TStrings;
begin
  inherited Perform(Visitor);
  AOutput := TStringList.Create();
  AErrors := TStringList.Create();
  try
    ACmdLine := Script[Script.Count - 1];
    Script.Delete(Script.Count - 1);
    try
      Visitor.Log.Add(Format(cCommandLineDescr, [ACmdLine]));
      //TODO for run from file
      s := FJobUtilities.ReplaceString(ACmdLine, GetParseLexems() + cScriptClause, cJobInputFile);
      ext := ScriptExt;
      if (ext = '') then
      begin
        ext := 'run';
      end;
      FJobUtilities.PerformCmdLine(s, Script, AOutput, AErrors, ext);
      CheckForErrorsInLog(AOutput);
    finally
      Script.Add(ACmdLine);
    end;
  finally
    Visitor.Log.AddStrings(AOutput);
    Visitor.Errors.AddStrings(AErrors);
    AErrors.Free();
    AOutput.Free();
  end;
end;

procedure TCustomRunDataItem.SetCommandLine(const Value: string);
begin
  if (FCommandLine <> Value) then
  begin
    FCommandLine := Value;
    DoDataChanged();
  end;
end;

procedure TCustomRunDataItem.SetParamDelimiter(const Value: string);
begin
  if (FParamDelimiter <> Value) then
  begin
    FParamDelimiter := Value;
    DoDataChanged();
  end;
end;

procedure TCustomRunDataItem.SetParamPrefix(const Value: string);
begin
  if (FParamPrefix <> Value) then
  begin
    FParamPrefix := Value;
    DoDataChanged();
  end;
end;

procedure TCustomRunDataItem.SetScriptExt(const Value: string);
begin
  if (FScriptExt <> Value) then
  begin
    FScriptExt := Value;
    DoDataChanged();
  end;
end;

procedure TCustomRunDataItem.Store(ANode: IXMLDOMNode);
var
  ChildNode, CDATANode: IXMLDOMNode;
begin
  inherited Store(ANode);
  ChildNode := ANode.ownerDocument.createElement('CommandLine');
  ANode.appendChild(ChildNode);
  ChildNode.text := FCommandLine;

  ChildNode := ANode.ownerDocument.createElement('ParamDelimiter');
  ANode.appendChild(ChildNode);
  CDATANode := ChildNode.ownerDocument.createCDATASection(FParamDelimiter);
  ChildNode.appendChild(CDATANode);

  ChildNode := ANode.ownerDocument.createElement('ParamPrefix');
  ANode.appendChild(ChildNode);
  CDATANode := ChildNode.ownerDocument.createCDATASection(FParamPrefix);
  ChildNode.appendChild(CDATANode);

  ChildNode := ANode.ownerDocument.createElement('ScriptExt');
  ANode.appendChild(ChildNode);
  ChildNode.text := FScriptExt;
end;

initialization
  RegisterClass(TCustomRunDataItem);
  AddCDataNodeName('ParamDelimiter');
  AddCDataNodeName('ParamPrefix');

end.
