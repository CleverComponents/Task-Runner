unit PascalScriptExecutor;

interface

uses
  System.Classes, System.SysUtils, System.Variants,
  JobClasses, ScriptExecutor, JobConsts, OperationClasses, PascalScriptClassesProxy,
  uPSComponent, uPSComponent_Default, uPSC_classes, uPSI_PascalScriptClassesProxy;

type
  TPascalScriptJobParamsProxyImpl = class(TPascalScriptJobParamsProxy)
  private
    FParams: TJobOperationParams;
  public
    constructor Create(AParams: TJobOperationParams);

    procedure SetParam(const AName, AValue: string); override;
    function GetParam(const AName: string): string; override;
  end;

  TPascalScriptExecutor = class(TScriptExecutor)
  private
    FLog: TStrings;
    FParamsProxy: TPascalScriptJobParamsProxy;

    procedure GetCompileErrors(AScript: TPSScript; AErrors: TStrings);
    procedure GetExecuteErrors(AScript: TPSScript; AErrors: TStrings);
    procedure ScriptCompile(AScript: TPSScript);
    procedure ScriptExecute(AScript: TPSScript);
  public
    function GetParseLexems: string; override;
    function GetWordDelimiters: string; override;
    procedure Execute(AVisitor: TJobVisitor); override;
  end;

implementation

{ TPascalScriptExecutor }

procedure TPascalScriptExecutor.Execute(AVisitor: TJobVisitor);
  procedure InitScripter(AScripter: TPSScript; ALog: TStrings);
  begin
    AScripter.Script := Script;
    FLog := ALog;
  end;

var
  output: TStrings;
  classesPlugin: TPSImport_Classes;
  psScript: TPSScript;
  pluginItem: TPSPluginItem;
  paramsPlugin: TPSImport_PascalScriptJobParamsProxy;
begin
  psScript := nil;
  classesPlugin := nil;
  paramsPlugin := nil;
  FParamsProxy := nil;
  FLog := nil;
  try
    psScript := TPSScript.Create(nil);
    psScript.OnCompile := ScriptCompile;
    psScript.OnExecute := ScriptExecute;

    paramsPlugin := TPSImport_PascalScriptJobParamsProxy.Create(nil);
    pluginItem := psScript.Plugins.Add() as TPSPluginItem;
    pluginItem.Plugin := paramsPlugin;

    classesPlugin := TPSImport_Classes.Create(nil);
    pluginItem := psScript.Plugins.Add() as TPSPluginItem;
    pluginItem.Plugin := classesPlugin;

    FParamsProxy := TPascalScriptJobParamsProxyImpl.Create(AVisitor.Params);

    output := TStringList.Create();
    try
      if IsUseLogFile then
      begin
        InitScripter(psScript, output);
        AVisitor.Log.Add(Format(cJobLogInFile, [LogFile]));
      end else
      begin
        InitScripter(psScript, AVisitor.Log);
      end;

      if not psScript.Compile() then
      begin
        GetCompileErrors(psScript, AVisitor.Errors);
        raise Exception.Create(cScriptError);
      end;

      if not psScript.Execute() then
      begin
        GetExecuteErrors(psScript, AVisitor.Errors);
        raise Exception.Create(cScriptError);
      end;

      if (AVisitor.Errors.Count > 0) then
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
    FLog := nil;
    FreeAndNil(FParamsProxy);
    paramsPlugin.Free();
    classesPlugin.Free();
    psScript.Free();
  end;
end;

procedure TPascalScriptExecutor.GetCompileErrors(AScript: TPSScript; AErrors: TStrings);
var
  i: Integer;
begin
  for i := 0 to AScript.CompilerMessageCount - 1 do
  begin
    AErrors.Add(string(AScript.CompilerMessages[i].MessageToString()));
  end;
end;

procedure TPascalScriptExecutor.GetExecuteErrors(AScript: TPSScript; AErrors: TStrings);
begin
  AErrors.Add(string(AScript.ExecErrorToString) +
    Format('(Line: %d, Pos: %d)', [AScript.ExecErrorRow, AScript.ExecErrorCol]));
end;

function TPascalScriptExecutor.GetParseLexems: string;
begin
  Result := cPascalScriptParseLexems;
end;

function TPascalScriptExecutor.GetWordDelimiters: string;
begin
  Result := cPascalScriptWordDelimiters;
end;

procedure TPascalScriptExecutor.ScriptCompile(AScript: TPSScript);
begin
  AScript.AddRegisteredPTRVariable(cPascalScriptParams, 'TPascalScriptJobParamsProxy');
  AScript.AddRegisteredPTRVariable(cPascalScriptJobLog, 'TStrings')
end;

procedure TPascalScriptExecutor.ScriptExecute(AScript: TPSScript);
begin
  AScript.SetPointerToData(cPascalScriptParams, @FParamsProxy,
    AScript.FindNamedType('TPascalScriptJobParamsProxy'));

  AScript.SetPointerToData(cPascalScriptJobLog, @FLog,
    AScript.FindNamedType('TStrings'));
end;

{ TPascalScriptJobParamsProxyImpl }

constructor TPascalScriptJobParamsProxyImpl.Create(AParams: TJobOperationParams);
begin
  inherited Create();
  FParams := AParams;
end;

function TPascalScriptJobParamsProxyImpl.GetParam(const AName: string): string;
var
  jobParam: TJobOperationParam;
begin
  jobParam := FParams.ParamByName(AName);
  Result := VarToStr(jobParam.Value);
end;

procedure TPascalScriptJobParamsProxyImpl.SetParam(const AName, AValue: string);
var
  jobParam: TJobOperationParam;
begin
  jobParam := FParams.ParamByName(AName);
  jobParam.Value := AValue;
end;

end.
