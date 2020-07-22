program Test;

uses
  Forms,
  testmain in 'testmain.pas' {Form1},
  JobUtils in '..\Utils\JobUtils.pas',
  JobConsts in '..\Classes\JobConsts.pas',
  OperationClasses in '..\Classes\OperationClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
