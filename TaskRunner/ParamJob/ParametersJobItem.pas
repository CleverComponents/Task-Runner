unit ParametersJobItem;

interface

uses
  System.Classes, JobClasses, System.SysUtils, System.Variants, OperationClasses, CustomJobItems;

type
  TParametersJobDataItem = class(TCustomParametersJobDataItem)
    procedure Perform(Visitor: TJobVisitor); override;
  end;

implementation

uses
  JobConsts;

{ TParametersJobDataItem }

procedure TParametersJobDataItem.Perform(Visitor: TJobVisitor);
var
  i: Integer;
  Param, VisitorParam: TJobOperationParam;
begin
  inherited Perform(Visitor);

  AssignParams(Parameters, Visitor.Params, cParseLexems);
  for i := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters.Items[i];
    VisitorParam := Visitor.Params.FindParam(Param.Name);

    if (VisitorParam = nil) then
    begin
      Visitor.Params.Add(Param.Name, Param.Value);
      Visitor.Log.Add(Format(cParameterAdded, [Param.Name, VarToStr(Param.Value)]));
    end else
    begin
      VisitorParam.Value := Param.Value;
      Visitor.Log.Add(Format(cParameterReplaced, [Param.Name, VarToStr(Param.Value)]));
    end;
  end;
  AssignParams(Visitor.Params, Parameters, cParseLexems);
end;

initialization
  RegisterClass(TParametersJobDataItem);

end.
