unit ScripterJobItem;

interface

uses
  Classes, JobClasses, sysutils, CustomJobItems, ActiveX, OperationClasses,
  JobConsts, JobUtils, inifiles, Winapi.msxml, uPSComponent,
  uPSComponent_Default, uPSC_classes, uPSI_AbstractOperationParams;

type
  TScripterDataItem = class(TCustomScriptJobDataItem)
  private
    FVisitor: TJobVisitor;
    FLanguage: TScripterLanguage;
    FJobUtilities: TJobUtilities;
    FLog: TStrings;

    procedure SetLanguage(const Value: TScripterLanguage);

    procedure GetCompileErrors(AScript: TPSScript; AErrors: TStrings);
    procedure GetExecuteErrors(AScript: TPSScript; AErrors: TStrings);
    procedure ScriptCompile(AScript: TPSScript);
    procedure ScriptExecute(AScript: TPSScript);
  protected
    procedure RecoverParameters(); override;
    procedure ReplaceParameters(Params: TJobOperationParams); override;
    function GetParseLexems: String; override;
    function GetWordDelimiters: String; override;
    procedure InitData; override;
  public
    constructor Create(AOwner: TJobItem); override;
    destructor Destroy; override;
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
  FJobUtilities := TJobUtilities.Create();
  FVisitor := nil;

  FLanguage := Low(TScripterLanguage);
end;

destructor TScripterDataItem.Destroy;
begin
  FJobUtilities.Free();
  inherited Destroy();
end;

procedure TScripterDataItem.GetCompileErrors(AScript: TPSScript; AErrors: TStrings);
var
  i: Integer;
begin
  for i := 0 to AScript.CompilerMessageCount - 1 do
  begin
    AErrors.Add(string(AScript.CompilerMessages[i].MessageToString()));
  end;
end;

procedure TScripterDataItem.GetExecuteErrors(AScript: TPSScript; AErrors: TStrings);
begin
  AErrors.Add(string(AScript.ExecErrorToString) +
    Format('(Line: %d, Pos: %d)', [AScript.ExecErrorRow, AScript.ExecErrorCol]));
end;

function TScripterDataItem.GetParseLexems: String;
begin
  Result := cScripterParseLexems;
end;

function TScripterDataItem.GetWordDelimiters: String;
begin
  Result := cScripterWordDelimiters;
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

  procedure InitScripter(AScripter: TPSScript; ALog: TStrings);
  begin
    AScripter.Script := Script;
    FLog := ALog;
//TODO    AScripter.AddObjectToScript(FJobUtilities, cScripterJobUtilities, False);
  end;

var
  output: TStrings;
  classesPlugin: TPSImport_Classes;
  psScript: TPSScript;
  pluginItem: TPSPluginItem;
  paramsPlugin: TPSImport_AbstractOperationParams;
begin
  inherited Perform(Visitor);

  FVisitor := Visitor;
  psScript := nil;
  classesPlugin := nil;
  paramsPlugin := nil;
  try
    if (Language <> slDelphiScript) then
    begin
      FVisitor.Errors.Add('Unsupported scripter language.');
      raise Exception.Create(cScriptError);
    end;

    psScript := TPSScript.Create(nil);
    psScript.OnCompile := ScriptCompile;
    psScript.OnExecute := ScriptExecute;

    paramsPlugin := TPSImport_AbstractOperationParams.Create(nil);
    pluginItem := psScript.Plugins.Add() as TPSPluginItem;
    pluginItem.Plugin := paramsPlugin;

    classesPlugin := TPSImport_Classes.Create(nil);
    pluginItem := psScript.Plugins.Add() as TPSPluginItem;
    pluginItem.Plugin := classesPlugin;

    output := TStringList.Create();
    try
      if IsUseLogFile then
      begin
        InitScripter(psScript, output);
        FVisitor.Log.Add(Format(cJobLogInFile, [LogFile]));
      end else
      begin
        InitScripter(psScript, FVisitor.Log);
      end;

      if not psScript.Compile() then
      begin
        GetCompileErrors(psScript, FVisitor.Errors);
        raise Exception.Create(cScriptError);
      end;

      if not psScript.Execute() then
      begin
        GetExecuteErrors(psScript, FVisitor.Errors);
        raise Exception.Create(cScriptError);
      end;

      if (FVisitor.Errors.Count > 0) then
      begin
        raise Exception.Create(cScriptError);
      end;
    finally
      if IsUseLogFile then
      begin
        output.SaveToFile(LogFile);
      end;
      output.Free();
    end;
  finally
    FVisitor := nil;
    FLog := nil;
    paramsPlugin.Free();
    classesPlugin.Free();
    psScript.Free();
  end;
end;

procedure TScripterDataItem.RecoverParameters;
begin
end;

procedure TScripterDataItem.ReplaceParameters(Params: TJobOperationParams);
begin
end;

procedure TScripterDataItem.ScriptCompile(AScript: TPSScript);
begin
  AScript.AddRegisteredPTRVariable(cScripterParams, 'TAbstractOperationParams');
  AScript.AddRegisteredPTRVariable(cScripterJobLog, 'TStrings')
end;

procedure TScripterDataItem.ScriptExecute(AScript: TPSScript);
begin
  AScript.SetPointerToData(cScripterParams, @FVisitor.Params,
    AScript.FindNamedType('TAbstractOperationParams'));

  AScript.SetPointerToData(cScripterJobLog, @FLog,
    AScript.FindNamedType('TStrings'));
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
      if (ind > -1) then FLanguage := TScripterLanguage(ind);
    end;
  finally
    EndUpdate();
  end;
end;

initialization
  RegisterClass(TScripterDataItem);

end.
