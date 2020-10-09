unit ScripterJobItem;

interface

uses
  Classes, JobClasses, sysutils, CustomJobItems, ActiveX,
  OperationClasses, JobConsts, JobUtils, inifiles, Winapi.msxml,
  ScriptExecutor, PascalScriptExecutor, JavaScriptExecutor;

type
  TScripterDataItem = class(TCustomScriptJobDataItem)
  private
    FLanguage: TScripterLanguage;

    procedure SetLanguage(const Value: TScripterLanguage);
    function CreateExecutor: TScriptExecutor;
  protected
    procedure RecoverParameters(); override;
    procedure ReplaceParameters(Params: TJobOperationParams); override;
    function GetParseLexems: String; override;
    function GetWordDelimiters: String; override;
    procedure InitData; override;
  public
    constructor Create(AOwner: TJobItem); override;

    function Load(AStream: TStream): Integer; overload; override;
    procedure Load(ANode: IXMLDOMNode); overload; override;
    procedure Store(ANode: IXMLDOMNode); override;
    procedure Assign(Source: TPersistent); override;

    procedure Perform(Visitor: TJobVisitor); override;

    property Language: TScripterLanguage read FLanguage write SetLanguage;
  end;

implementation

uses
  XMLUtils;

{ TScripterDataItem }

procedure TScripterDataItem.Assign(Source: TPersistent);
var
  Data: TScripterDataItem;
begin
  inherited Assign(Source);
  BeginUpdate();
  try
    if (Source is TScripterDataItem) then
    begin
      Data := TScripterDataItem(Source);
      FLanguage := Data.Language;
    end else
    begin
      InitData();
    end;
  finally
    EndUpdate();
  end;
end;

constructor TScripterDataItem.Create(AOwner: TJobItem);
begin
  inherited Create(AOwner);
  FLanguage := Low(TScripterLanguage);
end;

function TScripterDataItem.CreateExecutor: TScriptExecutor;
begin
  if (Language = slPascalScript) then
  begin
    Result := TPascalScriptExecutor.Create(Script, IsUseScriptFile, LogFile);
  end else
  if (Language = slJavaScript) then
  begin
    Result := TJavaScriptExecutor.Create(Script, IsUseScriptFile, LogFile);
  end else
  begin
    raise Exception.Create(cScriptError);
  end;
end;

function TScripterDataItem.GetParseLexems: String;
var
  executor: TScriptExecutor;
begin
  executor := CreateExecutor();
  try
    Result := executor.GetParseLexems();
  finally
    executor.Free();
  end;
end;

function TScripterDataItem.GetWordDelimiters: String;
var
  executor: TScriptExecutor;
begin
  executor := CreateExecutor();
  try
    Result := executor.GetWordDelimiters();
  finally
    executor.Free();
  end;
end;

function TScripterDataItem.Load(AStream: TStream): Integer;
var
  R: TReader;
begin
  Result := inherited Load(AStream);
  if (Result > 6) then
  begin
    BeginUpdate();
    R := TReader.Create(AStream, 1024);
    try
      FLanguage := TScripterLanguage(R.ReadInteger());
    finally
      R.Free();
      EndUpdate();
    end;
  end;
end;

procedure TScripterDataItem.Perform(Visitor: TJobVisitor);
var
  executor: TScriptExecutor;
begin
  inherited Perform(Visitor);

  executor := CreateExecutor();
  try
    executor.Execute(Visitor);
  finally
    executor.Free();
  end;
end;

procedure TScripterDataItem.RecoverParameters;
begin
end;

procedure TScripterDataItem.ReplaceParameters(Params: TJobOperationParams);
begin
end;

procedure TScripterDataItem.SetLanguage(const Value: TScripterLanguage);
begin
  if (FLanguage <> Value) then
  begin
    FLanguage := Value;
    DoDataChanged();
  end;
end;

procedure TScripterDataItem.Store(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  inherited Store(ANode);
  ChildNode := ANode.ownerDocument.createElement('Language');
  ANode.appendChild(ChildNode);
  ChildNode.text := cScripterLanguages[FLanguage];
end;

procedure TScripterDataItem.InitData;
begin
  inherited InitData();
  FLanguage := Low(TScripterLanguage);
end;

procedure TScripterDataItem.Load(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
  ind: Integer;
begin
  inherited Load(ANode);
  BeginUpdate();
  try
    ChildNode := ANode.selectSingleNode('Language');
    if ChildNode <> nil then
    begin
      ind := GetArrayIndexByName(ChildNode.text, cScripterLanguages);

      if (ind > -1) then
      begin
        FLanguage := TScripterLanguage(ind);
      end else
      if (CompareText(ChildNode.text, 'DELPHI SCRIPT') = 0) then
      begin
        FLanguage := slPascalScript;
      end;
    end;
  finally
    EndUpdate();
  end;
end;

initialization
  RegisterClass(TScripterDataItem);

end.
