unit testmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, dcscript, ActiveX;

type
  TForm1 = class(TForm)
    btnStart: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLog: TStrings;
    procedure DoStart;
    procedure DoStartPerformFile();
    procedure DoStartScripter;

    function GetCount(Instance: TObject; Index: PArgList): OleVariant;
    procedure SetItems(Instance: TObject; Params: PArgList);
    function GetItems(Instance: TObject; Index: PArgList): OleVariant;
    procedure ScriptErrorEvent(Sender: TObject; const ExcepInfo: TExcepInfo;
                            CharNo, LineNo: Integer; var Handled: Boolean);
    function JobLogAdd(const S: String): Integer;
    procedure GetSettingsDirectoryTest;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  JobUtils, OperationClasses;

procedure TForm1.DoStartPerformFile;
var
  AFile, AInput, AOutput, AErrors: TStrings;
  JobUtilities: TJobUtilities;
begin
  AFile := nil;
  AInput := nil;
  AOutput := nil;
  AErrors := nil;
  JobUtilities := nil;
  try
    AFile := TStringList.Create();
    AInput := TStringList.Create();
    AOutput := TStringList.Create();
    AErrors := TStringList.Create();
    JobUtilities := TJobUtilities.Create();
    AFile.Add('echo test');
//    JobUtilities.PerformFile(AFile, AInput, AOutput, AErrors);
    if (Pos('test', AOutput.Text) < 1) then
    begin
      raise Exception.Create('cmd test failed');
    end;

    AInput.Clear();
    AInput.Add('select * from se_stringlist');
    JobUtilities.PerformFile('osql.exe /U sa /d se /P /S PINE /i ' + cJobInputFile
      + ' /o '+ cJobOutFile, AInput, AOutput, AErrors);
  finally
    JobUtilities.Free();
    AErrors.Free();
    AOutput.Free();
    AInput.Free();
    AFile.Free();
  end;
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  DoStart();
end;

type
  TJobLogAddMethod = function (const S: String): Integer of object;

function TForm1.JobLogAdd(const S: String): Integer;
begin
  Result := FLog.Add(S);
end;

procedure TForm1.DoStartScripter();
var
  Script: TStrings;
  Scripter: TDCScripter;
  Params: TJobOperationParams;
begin
  RegisterClassInScript(TJobOperationParams);
  RegisterProperty(TJobOperationParams, 'Count', GetCount, nil);
  RegisterIndexedProperty(TJobOperationParams, 'Items', 1, False, GetItems, SetItems);

  RegisterClassInScript(TStrings);
  RegRegisterMethod(TStrings, 'Add', TypeInfo(TJobLogAddMethod),
    [TypeInfo(String), TypeInfo(Integer)], Addr(TForm1.JobLogAdd));

  Script := TStringList.Create();
  Scripter := TDCScripter.Create(nil);
  Params := TJobOperationParams.Create();
  try
    Params.Add('param1', 10);
    Params.Add('param2', 'qwe');
    Scripter.AddObjectToScript(Params, 'Params', False);
    Scripter.AddObjectToScript(FLog, 'JobLog', False);
    Script.Add('Params.Items("param1") = 50');
    Script.Add('JobLog.Add("qwe")');
    Scripter.Language := 'VBScript';
    Scripter.OnError := ScriptErrorEvent;
    Scripter.Script := Script;
    Scripter.HookAllEvents := True;
    Scripter.Run();
    MessageDlg(VarToStr(Params.ParamByName('param1').Value), mtInformation, [mbOK], 0);
  finally
    Params.Free();
    Scripter.Free();
    Script.Free();
  end;
end;

procedure TForm1.DoStart();
begin
//  DoStartPerformFile();
//  DoStartScripter();
  GetSettingsDirectoryTest();
  ShowMessage('Done');
end;

procedure TForm1.GetSettingsDirectoryTest();
var
  s: string;
begin
  Assert(GetOwnProgramName() = 'Test');
{  s := GetSettingsDirectory();
  Assert(s = '');
  ZeroMemory(verinfo, SizeOf(verinfo));
  verinfo.dwOSVersionInfoSize := SizeOf(verinfo);
  GetVersionEx(verinfo);
  if (verinfo.dwPlatformId = VER_PLATFORM_WIN32_NT) and (verinfo.dwMajorVersion > 3) then
  begin
    Assert(s = '')
  end else
  begin
  end;}
end;

function TForm1.GetCount(Instance: TObject; Index: PArgList): OleVariant;
begin
  Result := TJobOperationParams(Instance).Count;
end;

function TForm1.GetItems(Instance: TObject; Index: PArgList): OleVariant;
var
  V: Variant;
begin
  V := OleVariant(Index^[0]);
  if (VarType(V) = varInteger) then
  begin
    Result := TJobOperationParams(Instance).Items[V].Value;
  end else
  begin
    Result := TJobOperationParams(Instance).ParamByName(VarToStr(V)).Value;
  end;
end;

procedure TForm1.SetItems(Instance: TObject; Params: PArgList);
var
  V: Variant;
begin
  V := OleVariant(Params^[1]);
  if (VarType(V) = varInteger) then
  begin
    TJobOperationParams(Instance).Items[V].Value := OleVariant(Params^[0]);
  end else
  begin
    TJobOperationParams(Instance).ParamByName(VarToStr(V)).Value := OleVariant(Params^[0]);
  end;
end;

procedure TForm1.ScriptErrorEvent(Sender: TObject; const ExcepInfo: TExcepInfo; CharNo, LineNo: Integer;
  var Handled: Boolean);
var
  S: String;
const
  cScripterError = 'Error occured in line %d, position %d, Code: %d, Source: ''%s'', Description: ''%s'', Help File: ''%s''';
begin
  S := Format(cScripterError, [LineNo, CharNo, ExcepInfo.wCode, ExcepInfo.bstrSource,
    ExcepInfo.bstrDescription, ExcepInfo.bstrHelpFile]) ;
  MessageDlg(S, mtError, [mbOK], 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLog := TStringList.Create();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FLog.Free();
end;

end.
