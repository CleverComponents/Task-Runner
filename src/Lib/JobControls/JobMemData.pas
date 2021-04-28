unit JobMemData;

interface

uses
  System.Classes, Data.DB, Datasnap.DBClient;

type
  TJobMemData = class(TClientDataSet)
  private
    FInitDataSet: Boolean;
  protected
    function GetCanRefresh: Boolean; override;
    procedure SetActive(Value: Boolean); override;
    procedure InternalRefresh; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TJobMemData }

constructor TJobMemData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitDataSet := False;
end;

function TJobMemData.GetCanRefresh: Boolean;
begin
  Result := GetCanModify();
end;

procedure TJobMemData.InternalRefresh;
begin
end;

procedure TJobMemData.SetActive(Value: Boolean);
begin
  if (not Active) and (not FInitDataSet) then
  begin
    FInitDataSet := True;
    try
      CreateDataSet();
    finally
      FInitDataSet := False;
    end;
  end;

  inherited SetActive(Value);
end;

end.
