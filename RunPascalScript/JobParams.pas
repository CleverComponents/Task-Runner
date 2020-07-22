unit JobParams;

interface

type
  TAbstractJobParam = class
  public
    procedure TestSetParams(const AName: string); virtual;
  end;

  TJobParam = class(TAbstractJobParam)
  private
    FValue: string;
    function GetIndexValue(Index: Integer): string;
    procedure SetIndexValue(Index: Integer; const Value: string);
  public
    procedure AddText(const AText: string);
    property Value: string read FValue write FValue;
    property IndexValue[Index: Integer]: string read GetIndexValue write SetIndexValue;
  end;

implementation

{ TJobParam }

procedure TJobParam.AddText(const AText: string);
begin
  Value := Value + AText;
end;

function TJobParam.GetIndexValue(Index: Integer): string;
begin
  Result := Value;
end;

procedure TJobParam.SetIndexValue(Index: Integer; const Value: string);
begin
  FValue := Value;
end;

{ TAbstractJobParam }

procedure TAbstractJobParam.TestSetParams(const AName: string);
begin
end;

end.
