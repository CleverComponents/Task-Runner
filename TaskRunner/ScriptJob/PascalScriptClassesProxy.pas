unit PascalScriptClassesProxy;

interface

type
  TPascalScriptJobParamsProxy = class
  public
    procedure SetParam(const AName, AValue: string); virtual;
    function GetParam(const AName: string): string; virtual;
  end;

implementation

{ TPascalScriptJobParamsProxy }

function TPascalScriptJobParamsProxy.GetParam(const AName: string): string;
begin
  Assert(False);
end;

procedure TPascalScriptJobParamsProxy.SetParam(const AName, AValue: string);
begin
  Assert(False);
end;

end.
