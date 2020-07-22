program RunPascalScript;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uPSI_JobParams in 'uPSI_JobParams.pas',
  JobParams in 'JobParams.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
