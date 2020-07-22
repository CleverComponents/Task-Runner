unit CommandBatchJobItem;

interface

uses
  Classes, JobClasses, sysutils, CustomJobItems, JobUtils;

type
  TCommandBatchDataItem = class(TCustomScriptJobDataItem)
  private
    FJobUtilities: TJobUtilities;
  protected
    function GetParseLexems: String; override;
    function GetWordDelimiters: String; override;
  public
    constructor Create(AOwner: TJobItem); override;
    destructor Destroy; override;
    procedure Perform(Visitor: TJobVisitor); override;
  end;

implementation

uses
  JobConsts;

{ TCommandBatchDataItem }

constructor TCommandBatchDataItem.Create(AOwner: TJobItem);
begin
  inherited Create(AOwner);
  FJobUtilities := TJobUtilities.Create();
end;

destructor TCommandBatchDataItem.Destroy;
begin
  FJobUtilities.Free();
  inherited Destroy();
end;

function TCommandBatchDataItem.GetParseLexems: String;
begin
  Result := cCommandBatchParseLexems;
end;

function TCommandBatchDataItem.GetWordDelimiters: String;
begin
  Result := cCommandBatchWordDelimiters;
end;

procedure TCommandBatchDataItem.Perform(Visitor: TJobVisitor);
var
  AOutput, AErrors: TStrings;
begin
  inherited Perform(Visitor);
  AOutput := TStringList.Create();
  AErrors := TStringList.Create();
  try
    //TODO for run from file
    FJobUtilities.PerformCmdFile(Script, AOutput, AErrors);
    CheckForErrorsInLog(AOutput);
  finally
    Visitor.Log.AddStrings(AOutput);
    Visitor.Errors.AddStrings(AErrors);
    AErrors.Free();
    AOutput.Free();
  end;
end;

initialization
  RegisterClass(TCommandBatchDataItem);

end.
