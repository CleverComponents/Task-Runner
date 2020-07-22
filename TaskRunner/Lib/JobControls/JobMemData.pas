unit JobMemData;

interface

uses
  System.Classes, Data.DB, Datasnap.DBClient;

type
  TJobMemData = class(TClientDataSet)
  private
    FInitDataSet: Boolean;
  protected
    procedure SetActive(Value: Boolean); override;
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
