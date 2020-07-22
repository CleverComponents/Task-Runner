unit JobRegister;

interface

uses
  System.Classes, JobCtrls, JobMemData;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Job Controls', [TJobComboBox, TJobRichEdit, TJobTreeView, TJobMemData]);
end;

end.
