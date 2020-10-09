unit JavaScriptExecutor;

interface

uses
  System.Classes, System.SysUtils, Winapi.Windows,
  JobClasses, OperationClasses, ScriptExecutor, JobConsts,
  SpiderMonkey, SyNode, SynCommons, SyNodeProto, SyNodeSimpleProto;

type
{$M+}
  TJavaScriptJobParamsProxy = class
  private
    FParams: TJobOperationParams;
  public
    constructor Create(AParams: TJobOperationParams);
  published
    function setParam(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
    function getParam(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
  end;

  TJavaScriptJobLogProxy = class
  private
    FLog: TStrings;
  public
    constructor Create(ALog: TStrings);
  published
    function add(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
  end;
{$M-}


  TJavaScriptExecutor = class(TScriptExecutor)
  private
    FParamsProxy: TJavaScriptJobParamsProxy;
    FLogProxy: TJavaScriptJobLogProxy;

    procedure DoOnCreateNewEngine(const aEngine: TSMEngine);
    function DoOnGetEngineName(const AEngine: TSMEngine): RawUTF8;
  public
    function GetParseLexems: string; override;
    function GetWordDelimiters: string; override;
    procedure Execute(AVisitor: TJobVisitor); override;
  end;

implementation

{$I Synopse.inc}
{$I SynSM.inc}
{$I SyNode.inc}

{ TJavaScriptExecutor }

procedure TJavaScriptExecutor.DoOnCreateNewEngine(const AEngine: TSMEngine);
begin
  AEngine.defineClass(FParamsProxy.ClassType, TSMSimpleRTTIProtoObject, AEngine.GlobalObject);
  AEngine.GlobalObject.ptr.DefineProperty(AEngine.cx, cJavaScriptParams,
    CreateJSInstanceObjForSimpleRTTI(AEngine.cx, FParamsProxy, AEngine.GlobalObject),
    JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT
  );

  AEngine.defineClass(FLogProxy.ClassType, TSMSimpleRTTIProtoObject, AEngine.GlobalObject);
  AEngine.GlobalObject.ptr.DefineProperty(AEngine.cx, cJavaScriptJobLog,
    CreateJSInstanceObjForSimpleRTTI(AEngine.cx, FLogProxy, AEngine.GlobalObject),
    JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT
  );
end;

function TJavaScriptExecutor.DoOnGetEngineName(const AEngine: TSMEngine): RawUTF8;
begin
  Result := RawUTF8(Format('TaskRunnerScript%d', [GetCurrentThreadId()]));
end;

procedure TJavaScriptExecutor.Execute(AVisitor: TJobVisitor);
var
  res: jsval;
  manager: TSMEngineManager;
  engine: TSMEngine;
  output: TStrings;
begin
  manager := TSMEngineManager.Create('');
  try
    manager.MaxPerEngineMemory := 512 * 1024 * 1024;
    manager.OnNewEngine := DoOnCreateNewEngine;
    manager.OnGetName := DoOnGetEngineName;

    output := nil;
    FParamsProxy := nil;
    FLogProxy := nil;
    try
      FParamsProxy := TJavaScriptJobParamsProxy.Create(AVisitor.Params);

      if IsUseLogFile then
      begin
        FLogProxy := TJavaScriptJobLogProxy.Create(output);
        AVisitor.Log.Add(Format(cJobLogInFile, [LogFile]));
      end else
      begin
        FLogProxy := TJavaScriptJobLogProxy.Create(AVisitor.Log);
      end;

      engine := manager.ThreadSafeEngine(nil);
      engine.Evaluate(Script.Text, 'script.js', 1, res);
    finally
      FreeAndNil(FLogProxy);
      FreeAndNil(FParamsProxy);
      if IsUseLogFile then
      begin
        output.SaveToFile(LogFile);
      end;
      output.Free();
    end;
  finally
    manager.ReleaseCurrentThreadEngine();
    manager.Free();
  end;
end;

function TJavaScriptExecutor.GetParseLexems: string;
begin
  Result := cJavaScriptParseLexems;
end;

function TJavaScriptExecutor.GetWordDelimiters: string;
begin
  Result := cJavaScriptWordDelimiters;
end;

{ TJavaScriptJobParamsProxy }

constructor TJavaScriptJobParamsProxy.Create(AParams: TJobOperationParams);
begin
  inherited Create();
  FParams := AParams;
end;

function TJavaScriptJobParamsProxy.getParam(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
var
  jobParam: TJobOperationParam;
begin
  try
    if (argc <> 1) or (not vp.argv[0].isString)  then
    begin
      raise Exception.Create(cScriptGetParamError);
    end;

    jobParam := FParams.ParamByName(vp.argv[0].asJSString.ToString(cx));
    vp.rval := SimpleVariantToJSval(cx, jobParam.Value);

    Result :=  true;
  except
    on E: Exception do
    begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

function TJavaScriptJobParamsProxy.setParam(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
var
  jobParam: TJobOperationParam;
begin
  try
    if (argc <> 2) then
    begin
      raise Exception.Create(cScriptSetParamError);
    end;

    if not (vp.argv[0].isString and vp.argv[1].isString) then
    begin
      raise Exception.Create(cScriptSetParamError);
    end;

    jobParam := FParams.ParamByName(vp.argv[0].asJSString.ToString(cx));
    jobParam.Value := vp.argv[1].asJSString.ToString(cx);

    Result :=  True;
  except
    on E: Exception do
    begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

{ TJavaScriptJobLogProxy }

function TJavaScriptJobLogProxy.add(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
begin
  try
    if (argc <> 1) or (not vp.argv[0].isString)  then
    begin
      raise Exception.Create(cScriptAddLogError);
    end;

    FLog.Add(vp.argv[0].asJSString.ToString(cx));

    Result :=  True;
  except
    on E: Exception do
    begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

constructor TJavaScriptJobLogProxy.Create(ALog: TStrings);
begin
  inherited Create();
  FLog := ALog;
end;

initialization
  InitJS();

finalization
  ShutDownJS();

end.
