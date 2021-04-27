unit JobConverter;

interface

uses
  Classes, JobClasses;

type
  TJobRemoteCallConverter = class
  private
    FJobManager: TJobManager;
    FJobLibManager: TJobManager;
    FDuplicateJobs: TStrings;
    FJobLibManagerName: String;
    function IsJobsEqual(Job1, Job2: TJobItem): Boolean;
    procedure RemoveDuplicateJobs;
    procedure DoConvertJob(AJob: TJobItem);
  public
    constructor Create(AJobManager: TJobManager; AJobLibManagerName: String);
    destructor Destroy; override;
    procedure DoConvert;
  end;

implementation

uses
  CallJobItem, Sysutils;

{ TJobRemoteCallConverter }

constructor TJobRemoteCallConverter.Create(AJobManager: TJobManager; AJobLibManagerName: String);
begin
  inherited Create();
  FDuplicateJobs := TStringList.Create();
  FJobLibManager := TJobManager.Create();
  FJobManager := AJobManager;
  FJobLibManagerName := AJobLibManagerName;
end;

procedure TJobRemoteCallConverter.DoConvert;
var
  i: Integer;
  Stream: TStream;
  AFileName: String;
begin
  AFileName := FJobLibManagerName;
  if (ExtractFilePath(AFileName) = '') then
  begin
    AFileName := ExtractFilePath(ParamStr(0)) + AFileName;
  end;
  if not FileExists(AFileName) then Exit;
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    FJobLibManager.Load(Stream);
  finally
    Stream.Free();
  end;
  RemoveDuplicateJobs();
  for i := 0 to FJobManager.RootItemsCount - 1 do
  begin
    DoConvertJob(FJobManager.RootItems[i]);
  end;
end;

destructor TJobRemoteCallConverter.Destroy;
begin
  FJobLibManager.Free();
  FDuplicateJobs.Free();
  inherited Destroy();
end;

procedure TJobRemoteCallConverter.DoConvertJob(AJob: TJobItem);
var
  i: Integer;
  ACallData: TCallJobDataItem;
begin
  if (AJob.Data is TCallJobDataItem) then
  begin
    ACallData := AJob.Data as TCallJobDataItem;
    if (FDuplicateJobs.IndexOf(ACallData.CallJobName) > 0) then
    begin
      ACallData.CallMediaName := FJobLibManagerName;
    end;
  end;
  for i := 0 to AJob.ItemsCount - 1 do
  begin
    DoConvertJob(AJob.Items[i]);
  end;
end;

function TJobRemoteCallConverter.IsJobsEqual(Job1, Job2: TJobItem): Boolean;
begin
  //TODO
  Result := (Job1.JobName = Job2.JobName) and (Job1.ItemsCount = Job2.ItemsCount);
end;

procedure TJobRemoteCallConverter.RemoveDuplicateJobs;
  function GetJob(AName: String): TJobItem;
  var
    i: Integer;
    Item: TJobItem;
  begin
    Result := nil;
    for i := 0 to FJobManager.RootItemsCount - 1 do
    begin
      Item := FJobManager.RootItems[i];
      if (CompareText(Item.JobName, AName) = 0) then
      begin
        Result := Item;
        Break;
      end;
    end;
  end;

var
  i: Integer;
  AJob, AJobLib: TJobItem;
begin
  FDuplicateJobs.Clear();
  for i := FJobLibManager.RootItemsCount - 1 downto 0 do
  begin
    AJobLib := FJobLibManager.RootItems[i];
    AJob := GetJob(AJobLib.JobName);
    if (AJob <> nil) and IsJobsEqual(AJob, AJobLib) then
    begin
      FDuplicateJobs.Add(AJob.JobName);
      FJobManager.RemoveJobItem(AJob);
    end;
  end;
end;

end.
