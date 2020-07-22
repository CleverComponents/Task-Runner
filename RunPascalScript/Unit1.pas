unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uPSComponent,
  uPSComponent_Default, uPSI_JobParams, JobParams ,uPSC_classes;

type
  TJobParam2 = class(TJobParam)
  public
    procedure TestSetParams(const AName: string); override;
  end;

  TForm1 = class(TForm)
    btnStartScript: TButton;
    btnStart: TButton;
    procedure btnStartScriptClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    FJobParam: TJobParam2;

    function GetCompilerMessageStr(AScript: TPSScript): string;
    procedure ScriptCompile(Sender: TPSScript);
    procedure ScriptExecute(Sender: TPSScript);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TScriptThread = class(TThread)
  private
    FJobParam: TJobParam2;

    function GetCompilerMessageStr(AScript: TPSScript): string;
    procedure ScriptCompile(Sender: TPSScript);
    procedure ScriptExecute(Sender: TPSScript);
  protected
    procedure Execute; override;
  public
    constructor Create(AJobParam: TJobParam2);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnStartScriptClick(Sender: TObject);
var
  thread: TThread;
begin
  thread := TThread.CreateAnonymousThread(
    procedure
    var
      jobParamsPlugin: TPSImport_JobParams;
      classesPlugin: TPSImport_Classes;
      script: TPSScript;
      pluginItem: TPSPluginItem;
    begin
      script := nil;
      classesPlugin := nil;
      jobParamsPlugin := nil;
      try
        script := TPSScript.Create(nil);
        script.OnCompile := ScriptCompile;
        script.OnExecute := ScriptExecute;

        classesPlugin := TPSImport_Classes.Create(nil);
        pluginItem := script.Plugins.Add() as TPSPluginItem;
        pluginItem.Plugin := classesPlugin;

        jobParamsPlugin := TPSImport_JobParams.Create(nil);
        pluginItem :=  script.Plugins.Add() as TPSPluginItem;
        pluginItem.Plugin := jobParamsPlugin;

        script.Script.Clear();
        script.Script.Add('var');
        script.Script.Add('list: TStringList;');
        script.Script.Add('begin');
        script.Script.Add('list := TStringList.Create();');
        script.Script.Add('try');
        script.Script.Add('list.Add(''Hello World'');');
        script.Script.Add('list.Add(JobParam.Value)');
        script.Script.Add('JobParam.Value := ''Test'';');
        script.Script.Add('JobParam.AddText(''CleverTest'')');
        script.Script.Add('list.SaveToFile(''c:\Users\Public\Test.txt'')');
        script.Script.Add('finally');
        script.Script.Add('list.Free;');
        script.Script.Add('end;');
        script.Script.Add('end.');

        if not script.Compile then
        begin
          raise Exception.Create('Script compilation errors: ' + GetCompilerMessageStr(script));
        end;

        if not script.Execute then
        begin
          raise Exception.Create('Script execution errors: ' + string(script.ExecErrorToString) +
            Format('(Line: %d, Pos: %d)', [script.ExecErrorRow, script.ExecErrorCol]));
        end;
      finally
        jobParamsPlugin.Free();
        classesPlugin.Free();
        script.Free();
      end;
    end);

  try
    thread.FreeOnTerminate := False;
    thread.Start();

    thread.WaitFor();
  finally
    thread.Free();
  end;

  ShowMessage('Done: ' + FJobParam.Value);
end;

procedure TForm1.btnStartClick(Sender: TObject);
var
  thread: TThread;
begin
  thread := TScriptThread.Create(FJobParam);
  try
    thread.FreeOnTerminate := False;
    thread.Start();

    thread.WaitFor();
  finally
    thread.Free();
  end;

  ShowMessage('Result: ' + FJobParam.Value);
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FJobParam := TJobParam2.Create();
  FJobParam.Value := 'Test';
end;

destructor TForm1.Destroy;
begin
  FJobParam.Free();
  inherited Destroy();
end;

function TForm1.GetCompilerMessageStr(AScript: TPSScript): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to AScript.CompilerMessageCount - 1 do
  begin
    Result := Result + string(AScript.CompilerMessages[i].MessageToString()) + '#13#10';
  end;
end;

procedure TForm1.ScriptCompile(Sender: TPSScript);
begin
  Sender.AddRegisteredPTRVariable('JobParam', 'TJobParam');
end;

procedure TForm1.ScriptExecute(Sender: TPSScript);
begin
  Sender.SetPointerToData('JobParam', @FJobParam, Sender.FindNamedType('TJobParam'));
end;

{ TScriptThread }

constructor TScriptThread.Create(AJobParam: TJobParam2);
begin
  inherited Create(True);

  FJobParam := AJobParam;
end;

procedure TScriptThread.Execute;
var
  jobParamsPlugin: TPSImport_JobParams;
  classesPlugin: TPSImport_Classes;
  script: TPSScript;
  pluginItem: TPSPluginItem;
begin
  script := nil;
  classesPlugin := nil;
  jobParamsPlugin := nil;
  try
    script := TPSScript.Create(nil);
    script.OnCompile := ScriptCompile;
    script.OnExecute := ScriptExecute;

    classesPlugin := TPSImport_Classes.Create(nil);
    pluginItem := script.Plugins.Add() as TPSPluginItem;
    pluginItem.Plugin := classesPlugin;

    jobParamsPlugin := TPSImport_JobParams.Create(nil);
    pluginItem :=  script.Plugins.Add() as TPSPluginItem;
    pluginItem.Plugin := jobParamsPlugin;

    script.Script.Clear();
    script.Script.Add('var');
    script.Script.Add('list: TStringList;');
    script.Script.Add('begin');
    script.Script.Add('list := TStringList.Create();');
    script.Script.Add('try');
    script.Script.Add('list.Add(''Hello World'');');
    script.Script.Add('JobParam.TestSetParams(''Test CleverComponents'');');
    script.Script.Add('list.Add(JobParam.Value);');
    script.Script.Add('list.SaveToFile(''c:\Users\Public\Test.txt'');');
    script.Script.Add('finally');
    script.Script.Add('list.Free;');
    script.Script.Add('end;');
    script.Script.Add('end.');

    if not script.Compile then
    begin
      raise Exception.Create('Script compilation errors: ' + GetCompilerMessageStr(script));
    end;

    if not script.Execute then
    begin
      raise Exception.Create('Script execution errors: ' + string(script.ExecErrorToString) +
        Format('(Line: %d, Pos: %d)', [script.ExecErrorRow, script.ExecErrorCol]));
    end;
  finally
    jobParamsPlugin.Free();
    classesPlugin.Free();
    script.Free();
  end;
end;

function TScriptThread.GetCompilerMessageStr(AScript: TPSScript): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to AScript.CompilerMessageCount - 1 do
  begin
    Result := Result + string(AScript.CompilerMessages[i].MessageToString()) + '#13#10';
  end;
end;

procedure TScriptThread.ScriptCompile(Sender: TPSScript);
begin
  Sender.AddRegisteredPTRVariable('JobParam', 'TJobParam');
end;

procedure TScriptThread.ScriptExecute(Sender: TPSScript);
begin
  Sender.SetPointerToData('JobParam', @FJobParam, Sender.FindNamedType('TJobParam'));
end;

{ TJobParam2 }

procedure TJobParam2.TestSetParams(const AName: string);
begin
  Value := AName;
end;

end.
