unit AbstractOperationParams;

interface

type
  TAbstractOperationParams = class
  public
    procedure SetParam(const AName, AValue: string); virtual;
    function GetParam(const AName: string): string; virtual;
  end;

implementation

{ TAbstractOperationParams }

function TAbstractOperationParams.GetParam(const AName: string): string;
begin
  Assert(False);
end;

procedure TAbstractOperationParams.SetParam(const AName, AValue: string);
begin
  Assert(False);
end;

end.
