unit JobUtils;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, Winapi.Windows, System.IniFiles, Winapi.msxml;

type
  TJobUtilities = class
  private
    FJobTempPath: String;
    FIniFile: TIniFile;
    procedure ClearJobTempDir(IsDeleteContents: Boolean = False);
    function InternalPerform(const AFileName: string): Integer;
    function GetJobTempDirName: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GetIniFileObject(const AFileName: String): TIniFile;
    function DirectoryExists(const Name: string): Boolean;
    function ForceDirectories(Dir: String): Boolean;
    function RemoveDirectory(ADirName: String): Integer;
    function ReplaceString(ASource, AFind, AReplace: string): string;
    function GetJobTempDir: string;
    function GetUniqueID: Integer;
    function GetUniqueName: string;
    function PerformCmdFile(AFile, AOutput, AErrors: TStrings): Integer;
    function PerformCmdLine(const ACmdLine: string; AInput, AOutput, AErrors: TStrings; const AScriptExt: string): Integer;
    function PerformFile(AFileName: string; AInput, AOutput, AErrors: TStrings): Integer;
  end;

  TSQLConnectionInfo = class(TPersistent)
  private
    FServer: String;
    FUser: String;
    FPassword: String;
    FDatabase: String;
    FTimeOut: String;
    FOnChanged: TNotifyEvent;
    FUpdateCount: Integer;
    FConnectionString: string;
    procedure SetDatabase(const Value: String);
    procedure SetPassword(const Value: String);
    procedure SetServer(const Value: String);
    procedure SetTimeOut(const Value: String);
    procedure SetUser(const Value: String);
    procedure SetConnectionString(const Value: string);
  protected
    procedure Changed; virtual;
    procedure Clear; virtual;
  public
    constructor Create;
    function Load(AStream: TStream): Integer; overload; virtual;
    procedure Store(AStream: TStream); overload; virtual;
    procedure Load(ANode: IXMLDOMNode); overload; virtual;
    procedure Store(ANode: IXMLDOMNode); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    function CreateConnectionString: String;
    procedure BeginUpdate;
    procedure EndUpdate;

    property Server: String read FServer write SetServer;
    property User: String read FUser write SetUser;
    property Password: String read FPassword write SetPassword;
    property Database: String read FDatabase write SetDatabase;
    property TimeOut: String read FTimeOut write SetTimeOut;
    property ConnectionString: string read FConnectionString write SetConnectionString;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TOutPutDevice = class
  end;

  TFileOutPutDevice = class(TOutPutDevice)
  private
    FLogFileName: String;
  public
    constructor Create();
    property LogFileName: String read FLogFileName write FLogFileName;
  end;

  TStreamOutPutDevice = class(TOutPutDevice)
  private
    FLogStream: TStrings;
    FErrorStream: TStrings;
  public
    constructor Create();
    property LogStream: TStrings read FLogStream write FLogStream;
    property ErrorStream: TStrings read FErrorStream write FErrorStream;
  end;

  TMSSQLScriptEngine = class
  private
    FJobUtilities: TJobUtilities;
//TODO    FScriptManager: IScriptManager;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RunScriptFile(FileName: String; AOutPut: TOutPutDevice); overload;
    procedure RunScriptFile(AInput: TStrings; AOutPut: TOutPutDevice); overload;
    procedure RunBatchFile(FileName: String; AOutPut: TOutPutDevice); overload;
    procedure RunBatchFile(AInput: TStrings; AOutPut: TOutPutDevice); overload;
    procedure DoConnect(Info: TSQLConnectionInfo);
    procedure DoConnectEx(AConnectionString: String; ATimeOut: Integer);
    procedure DoDisconnect;
    function IsScriptUpToDate(FileName: String): Boolean;
    procedure GetScriptVersion(FileName: String; var Version, ScriptVer: String);
  end;

const
  cJobRunFile = '_jobrun_';
  cJobInputFile = '_jobin_';
  cJobOutFile = '_jobout_';
  cJobErrorFile = '_joberror_';

const
  BuildNo = {$I BuildNo.inc};

function CheckWordExists(const Buffer, NeededString: String): Boolean;
function GetSettingsDirectory: string;
function GetOwnProgramName: string;
function AddTrailingDirSeparator(const APath: string): string;

function GetMainFormCaption(const AMedia: string): string;

implementation

uses
  JobConsts, Consts, XMLUtils;


function GetMainFormCaption(const AMedia: string): string;
begin
  Result := Format('Task Runner %s %s', [BuildNo, {$IFDEF WIN64}'(x64)'{$ELSE}''{$ENDIF}]);
  if (AMedia <> '') then
  begin
    Result := AMedia + ' - ' + Result;
  end;
end;

function GetOwnProgramName: string;
begin
  Result := 'TaskRunner';
end;

function AddTrailingDirSeparator(const APath: string): string;
begin
  Result := APath;
  if (Result <> '') and (Result[Length(Result)] <> TPath.DirectorySeparatorChar) then
  begin
    Result := Result + TPath.DirectorySeparatorChar;
  end;
end;

function GetSettingsDirectory: string;
begin
  if (TOSVersion.Platform = pfWindows) and (TOSVersion.Major > 5) then
  begin
    Result := AddTrailingDirSeparator(TPath.GetHomePath())
      + GetOwnProgramName() + TPath.DirectorySeparatorChar;
  end else
  begin
    Result := AddTrailingDirSeparator(ExtractFilePath(ParamStr(0)));
  end;
end;

function CheckWordExists(const Buffer, NeededString: String): Boolean;
var
  i, curpos, EndSymbol, len: Integer;
begin
  Result := False;
  if (NeededString = '') then Exit;

  EndSymbol := Length(NeededString) + 1;
  curpos := 1;
  len := Length(Buffer);

  for i := 1 to Length(Buffer) do
  begin
    if (Buffer[i] = NeededString[curpos]) then
    begin
      Inc(curpos);
    end else
    begin
      Curpos := 1;
      Continue;
    end;

    if (Curpos = 2) then
    begin
      if (i > 1) and not CharInSet(Buffer[i - 1], [#32, #10]) then
      begin
        Curpos := 1;
        Continue;
      end;
    end else
    if (Curpos = EndSymbol) then
    begin
      if (i = len) or CharInSet(Buffer[i + 1], [#32, #13]) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ TJobUtilities }

procedure TJobUtilities.ClearJobTempDir(IsDeleteContents: Boolean = False);
var
  Res: Integer;
  sr: TSearchRec;
  Attr: Integer;
  ADir: String;
begin
  ADir := GetJobTempDirName();
  if not DirectoryExists(ADir) then Exit;

  if IsDeleteContents then
  begin
    Attr := faAnyFile and (not faDirectory);
    Res := System.SysUtils.FindFirst(ADir + '\*.*', Attr, sr);
    while (Res = 0) do
    begin
      System.SysUtils.DeleteFile(ADir + '\' + sr.Name);
      Res := System.SysUtils.FindNext(sr);
    end;
    System.SysUtils.FindClose(sr);
  end;

  System.SysUtils.RemoveDir(ADir);
end;

constructor TJobUtilities.Create;
begin
  inherited Create;
  FJobTempPath := '';
  FIniFile := nil;
end;

destructor TJobUtilities.Destroy;
begin
  FIniFile.Free();
  ClearJobTempDir();
  inherited Destroy();
end;

function TJobUtilities.GetJobTempDirName: String;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetTempPath);
  Result := Result + GetOwnProgramName();
end;

function TJobUtilities.DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function TJobUtilities.GetJobTempDir: string;
var
  ADir: string;
begin
  if (FJobTempPath = '') then
  begin
    ClearJobTempDir();
    ADir := GetJobTempDirName();

    if not DirectoryExists(ADir) then
    begin
      if not CreateDir(ADir) then
      begin
        raise Exception.CreateFmt(cCreateError, [ADir]);
      end;
    end;
    FJobTempPath := ADir;
  end;

  Result := FJobTempPath;
end;

var
  ID: Integer = 0;

function TJobUtilities.GetUniqueID: Integer;
begin
  InterlockedIncrement(ID);
  Result := ID;
end;

function TJobUtilities.ReplaceString(ASource, AFind, AReplace: string): string;
var
  i, len, repl, start: Integer;
begin
  Result := ASource;
  if AFind = AReplace then Exit;
  len := Length(AFind);
  repl := Length(AReplace);
  start := 1;
  i := Pos(AFind, Copy(Result, start, Length(Result)));
  while i > 0 do
  begin
    i := i + start - 1;
    Result := Copy(Result, 1, i - 1) + AReplace + Copy(Result, i + len, Length(Result));
    start := i + repl;
    i := Pos(AFind, Copy(Result, start, Length(Result)));
  end;
end;

function TJobUtilities.InternalPerform(const AFileName: string): Integer;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  Res: DWORD;
begin
  ZeroMemory(@StartupInfo, SizeOf(TStartupInfo));
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;

  ZeroMemory(@ProcessInfo, SizeOf(TProcessInformation));

  if not CreateProcess(nil, PChar(AFileName), nil, nil, False,
    CREATE_NEW_CONSOLE or CREATE_NO_WINDOW, nil, nil, StartupInfo, ProcessInfo) then
  begin
    raise Exception.CreateFmt(cCannotRunFile, [AFileName, GetLastError()]);
  end;

  CloseHandle(ProcessInfo.hThread);
  WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
  GetExitCodeProcess(ProcessInfo.hProcess, Res);
  Result := Res;
  CloseHandle(ProcessInfo.hProcess);
end;

function TJobUtilities.PerformFile(AFileName: string; AInput, AOutput, AErrors: TStrings): Integer;
var
  InputFileName, OutputFileName: string;
begin
  if (Pos(cJobInputFile, AFileName) > 0) then
  begin
    InputFileName := GetJobTempDir() + '\' + GetUniqueName() + '.run';
    AInput.WriteBOM := False;
    AInput.SaveToFile(InputFileName, TEncoding.UTF8);
    AFileName := ReplaceString(AFileName, cJobInputFile, '"' + InputFileName + '"');
  end else
  begin
    InputFileName := '';
  end;

  if (Pos(cJobOutFile, AFileName) > 0) then
  begin
    OutputFileName := GetJobTempDir() + '\' + GetUniqueName() + '.run';
    AFileName := ReplaceString(AFileName, cJobOutFile, '"' + OutputFileName + '"');
  end else
  begin
    OutputFileName := '';
  end;
  try
    Result := InternalPerform(AFileName);
  finally
    if (InputFileName <> '') and System.SysUtils.FileExists(InputFileName) then
    begin
      System.SysUtils.DeleteFile(InputFileName);
    end;
    if (OutputFileName <> '') and System.SysUtils.FileExists(OutputFileName) then
    begin
      AOutput.LoadFromFile(OutputFileName);
      System.SysUtils.DeleteFile(OutputFileName);
    end;
  end;
end;

function TJobUtilities.PerformCmdFile(AFile, AOutput, AErrors: TStrings): Integer;
var
  UniqueName: String;
  AInput: TStrings;
begin
  UniqueName := GetJobTempDir() + '\' + GetUniqueName() + '.cmd';
  AInput := TStringList.Create();
  try
    AFile.WriteBOM := False;
    AFile.SaveToFile(UniqueName, TEncoding.UTF8);
    Result := PerformFile(UniqueName + ' > ' + cJobOutFile, AInput, AOutput, AErrors);
  finally
    AInput.Free();
    if System.SysUtils.FileExists(UniqueName) then
    begin
      System.SysUtils.DeleteFile(UniqueName);
    end;
  end;
end;

function TJobUtilities.PerformCmdLine(const ACmdLine: string; AInput, AOutput, AErrors: TStrings; const AScriptExt: string): Integer;
var
  AFile: TStrings;
  InputFileName: string;
begin
  AFile := TStringList.Create();
  try
    AFile.Add(ACmdLine);
    if (Pos(cJobInputFile, AFile.Text) > 0) then
    begin
      InputFileName := GetJobTempDir() + '\' + GetUniqueName() + '.' + AScriptExt;
      AInput.WriteBOM := False;
      AInput.SaveToFile(InputFileName, TEncoding.UTF8);
      AFile.Text := ReplaceString(AFile.Text, cJobInputFile, '"' + InputFileName + '"');
    end else
    begin
      InputFileName := '';
    end;
    try
      Result := PerformCmdFile(AFile, AOutput, AErrors);
    finally
      if (InputFileName <> '') and System.SysUtils.FileExists(InputFileName) then
      begin
        System.SysUtils.DeleteFile(InputFileName);
      end;
    end;
  finally
    AFile.Free();
  end;
end;

function TJobUtilities.GetUniqueName: string;
begin
  Result := IntToStr(GetCurrentProcessId()) + IntToStr(GetUniqueID());
end;

function TJobUtilities.ForceDirectories(Dir: String): Boolean;
begin
  Result := System.SysUtils.ForceDirectories(Dir);
end;

function TJobUtilities.RemoveDirectory(ADirName: String): Integer;
begin
  Result := InternalPerform('cmd.exe /c rd /Q /S ' + AnsiQuotedStr(ADirName, '"'));
end;

function TJobUtilities.GetIniFileObject(const AFileName: String): TIniFile;
begin
  if (FIniFile <> nil) and (FIniFile.FileName <> AFileName) then
  begin
    FreeAndNil(FIniFile);
  end;
  if (FIniFile = nil) then
  begin
    FIniFile := TIniFile.Create(AFileName);
  end;
  Result := FIniFile;
end;

{ TMSSQLScriptEngine }

constructor TMSSQLScriptEngine.Create;
begin
  inherited Create();
  FJobUtilities := TJobUtilities.Create();
//TODO  FScriptManager := CoScriptManager.Create();
end;

destructor TMSSQLScriptEngine.Destroy;
begin
//  FScriptManager := nil;
  FJobUtilities.Free();
  inherited Destroy();
end;

procedure TMSSQLScriptEngine.DoConnect(Info: TSQLConnectionInfo);
//var
//  ATimeOut: Integer;
begin
{  try
    ATimeOut := StrToInt(Info.FTimeOut);
  except
    ATimeOut := - 1;
  end;
  FScriptManager.DoConnect(Info.Server, Info.Database, Info.User, Info.Password, ATimeOut);}
end;

procedure TMSSQLScriptEngine.DoConnectEx(AConnectionString: String; ATimeOut: Integer);
begin
//  FScriptManager.DoConnectEx(AConnectionString, ATimeOut);
end;

procedure TMSSQLScriptEngine.DoDisconnect;
begin
//  FScriptManager.DoDisconnect();
end;

procedure TMSSQLScriptEngine.GetScriptVersion(FileName: String;
  var Version, ScriptVer: String);
//var
//  pVersion, pScriptVer: OleVariant;
begin
//  FScriptManager.GetScriptVersion(FileName, pVersion, pScriptVer);
//  Version := VarToStr(pVersion);
//  ScriptVer := VarToStr(pScriptVer);
end;

function TMSSQLScriptEngine.IsScriptUpToDate(FileName: String): Boolean;
//var
//  IsUpToDate: Integer;
begin
{  FScriptManager.IsScriptUpToDate(FileName, IsUpToDate);
  Result := (IsUpToDate = 1);}
  Result := False;
end;

procedure TMSSQLScriptEngine.RunBatchFile(AInput: TStrings; AOutPut: TOutPutDevice);
var
  InputFileName: String;
begin
  InputFileName := FJobUtilities.GetJobTempDir() + '\' + FJobUtilities.GetUniqueName() + '.run';
  AInput.SaveToFile(InputFileName);

  try
    RunBatchFile(InputFileName, AOutPut);
  finally
    if System.SysUtils.FileExists(InputFileName) then
    begin
      System.SysUtils.DeleteFile(InputFileName);
    end;
  end;
end;

procedure TMSSQLScriptEngine.RunBatchFile(FileName: String; AOutPut: TOutPutDevice);
var
  OutputFileName: String;
  ISLogFileGenerated: Boolean;
begin
  ISLogFileGenerated := False;

  if (AOutPut is TFileOutPutDevice) then
  begin
    OutputFileName := TFileOutPutDevice(AOutPut).FLogFileName;
  end else
  begin
    OutputFileName := FJobUtilities.GetJobTempDir() + '\' + FJobUtilities.GetUniqueName() + '.run';
    ISLogFileGenerated := True;
  end;

  try
//    FScriptManager.RunBatchFile(FileName, OutputFileName);
  finally
    if System.SysUtils.FileExists(OutputFileName) then
    begin
      if (AOutPut is TStreamOutPutDevice) and (TStreamOutPutDevice(AOutPut).LogStream <> nil) then
      begin
        TStreamOutPutDevice(AOutPut).LogStream.LoadFromFile(OutputFileName);
      end;
      if ISLogFileGenerated then
      begin
        System.SysUtils.DeleteFile(OutputFileName);
      end;
    end;
  end;
end;

procedure TMSSQLScriptEngine.RunScriptFile(AInput: TStrings; AOutPut: TOutPutDevice);
var
  InputFileName: String;
begin
  InputFileName := FJobUtilities.GetJobTempDir() + '\' + FJobUtilities.GetUniqueName() + '.run';
  AInput.SaveToFile(InputFileName);

  try
    RunScriptFile(InputFileName, AOutPut);
  finally
    if System.SysUtils.FileExists(InputFileName) then
    begin
      System.SysUtils.DeleteFile(InputFileName);
    end;
  end;
end;

procedure TMSSQLScriptEngine.RunScriptFile(FileName: String; AOutPut: TOutPutDevice);
var
  OutputFileName: String;
  ISLogFileGenerated: Boolean;
begin
  ISLogFileGenerated := False;

  if (AOutPut is TFileOutPutDevice) then
  begin
    OutputFileName := TFileOutPutDevice(AOutPut).FLogFileName;
  end else
  begin
    OutputFileName := FJobUtilities.GetJobTempDir() + '\' + FJobUtilities.GetUniqueName() + '.run';
    ISLogFileGenerated := True;
  end;

  try
//    FScriptManager.RunScriptFile(FileName, OutputFileName);
  finally
    if System.SysUtils.FileExists(OutputFileName) then
    begin
      if (AOutPut is TStreamOutPutDevice) and (TStreamOutPutDevice(AOutPut).LogStream <> nil) then
      begin
        TStreamOutPutDevice(AOutPut).LogStream.LoadFromFile(OutputFileName);
      end;
      if ISLogFileGenerated then
      begin
        System.SysUtils.DeleteFile(OutputFileName);
      end;
    end;
  end;
end;

{ TSQLConnectionInfo }

procedure TSQLConnectionInfo.Assign(Source: TPersistent);
var
  Info: TSQLConnectionInfo;
begin
  BeginUpdate();
  try
    if (Source is TSQLConnectionInfo) then
    begin
      Info := TSQLConnectionInfo(Source);
      FServer := Info.Server;
      FUser := Info.User;
      FPassword := Info.Password;
      FDatabase := Info.Database;
      FTimeOut := Info.TimeOut;
      FConnectionString := Info.ConnectionString;
    end else
    begin
      Clear();
    end;
  finally
    EndUpdate();
  end;
end;

procedure TSQLConnectionInfo.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSQLConnectionInfo.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanged) then
  begin
    FOnChanged(Self);
  end;
end;

procedure TSQLConnectionInfo.Clear;
begin
  FServer := '';
  FUser := '';
  FPassword := '';
  FDatabase := '';
  FTimeOut := '';
  FConnectionString := '';
end;

constructor TSQLConnectionInfo.Create;
begin
  inherited Create();
  Clear();
end;

function TSQLConnectionInfo.CreateConnectionString: String;
begin
  Result := 'Provider=SQLOLEDB.1;'
      + 'User ID=' + FUser + ';Password=' + FPassword
      + ';Initial Catalog=' + FDatabase +';Data Source=' + FServer;
end;

procedure TSQLConnectionInfo.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed();
end;

function TSQLConnectionInfo.Load(AStream: TStream): Integer;
var
  R: TReader;
begin
  BeginUpdate();
  R := TReader.Create(AStream, 1024);
  try
    Result := R.ReadInteger(); //Version
    FServer := R.ReadString();
    FUser := R.ReadString();
    FPassword := R.ReadString();
    FDatabase := R.ReadString();
    FTimeOut := R.ReadString();
  finally
    R.Free();
    EndUpdate();
  end;
end;

procedure TSQLConnectionInfo.Load(ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  BeginUpdate();
  try
    ChildNode := ANode.selectSingleNode('Server');
    if ChildNode <> nil then FServer := ChildNode.text;

    ChildNode := ANode.selectSingleNode('User');
    if ChildNode <> nil then FUser := ChildNode.text;

    ChildNode := ANode.selectSingleNode('Password');
    if ChildNode <> nil then FPassword := ChildNode.text;

    ChildNode := ANode.selectSingleNode('Database');
    if ChildNode <> nil then FDatabase := ChildNode.text;

    ChildNode := ANode.selectSingleNode('TimeOut');
    if ChildNode <> nil then FTimeOut := ChildNode.text;

    ChildNode := ANode.selectSingleNode('ConnectionString');
    if ChildNode <> nil then FConnectionString := ChildNode.text;
  finally
    EndUpdate();
  end;
end;

procedure TSQLConnectionInfo.SetConnectionString(const Value: string);
begin
  if (FConnectionString <> Value) then
  begin
    FConnectionString := Value;
    Changed();
  end;
end;

procedure TSQLConnectionInfo.SetDatabase(const Value: String);
begin
  if (FDatabase <> Value) then
  begin
    FDatabase := Value;
    Changed();
  end;
end;

procedure TSQLConnectionInfo.SetPassword(const Value: String);
begin
  if (FPassword <> Value) then
  begin
    FPassword := Value;
    Changed();
  end;
end;

procedure TSQLConnectionInfo.SetServer(const Value: String);
begin
  if (FServer <> Value) then
  begin
    FServer := Value;
    Changed();
  end;
end;

procedure TSQLConnectionInfo.SetTimeOut(const Value: String);
begin
  if (FTimeOut <> Value) then
  begin
    FTimeOut := Value;
    Changed();
  end;
end;

procedure TSQLConnectionInfo.SetUser(const Value: String);
begin
  if (FUser <> Value) then
  begin
    FUser := Value;
    Changed();
  end;
end;

procedure TSQLConnectionInfo.Store(AStream: TStream);
var
  W: TWriter;
begin
  W := TWriter.Create(AStream, 1024);
  try
    W.WriteInteger(1);
    W.WriteString(FServer);
    W.WriteString(FUser);
    W.WriteString(FPassword);
    W.WriteString(FDatabase);
    W.WriteString(FTimeOut);
  finally
    W.Free();
  end;
end;

procedure TSQLConnectionInfo.Store(ANode: IXMLDOMNode);
var
  ChildNode, CDATANode: IXMLDOMNode;
begin
  ChildNode := ANode.ownerDocument.createElement('Server');
  ANode.appendChild(ChildNode);
  ChildNode.text := FServer;

  ChildNode := ANode.ownerDocument.createElement('User');
  ANode.appendChild(ChildNode);
  ChildNode.text := FUser;

  ChildNode := ANode.ownerDocument.createElement('Password');
  ANode.appendChild(ChildNode);
  ChildNode.text := FPassword;

  ChildNode := ANode.ownerDocument.createElement('Database');
  ANode.appendChild(ChildNode);
  ChildNode.text := FDatabase;

  ChildNode := ANode.ownerDocument.createElement('TimeOut');
  ANode.appendChild(ChildNode);
  ChildNode.text := FTimeOut;

  ChildNode := ANode.ownerDocument.createElement('ConnectionString');
  ANode.appendChild(ChildNode);
  CDATANode := ChildNode.ownerDocument.createCDATASection(FConnectionString);
  ChildNode.appendChild(CDATANode);
end;

{ TFileOutPutDevice }

constructor TFileOutPutDevice.Create;
begin
  inherited Create();
  FLogFileName := '';
end;

{ TStreamOutPutDevice }

constructor TStreamOutPutDevice.Create;
begin
  inherited Create();
  FLogStream := nil;
  FErrorStream := nil;
end;

initialization
  AddCDataNodeName('ConnectionString');
  
end.


