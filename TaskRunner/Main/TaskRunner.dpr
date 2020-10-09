program TaskRunner;

uses
  FastMM4,
  FastMM4Messages,
  Vcl.Forms,
  JobsMain in 'JobsMain.pas' {JobsMainFrame},
  main in 'main.pas' {MainForm},
  JobClasses in '..\Classes\JobClasses.pas',
  JobConsts in '..\Classes\JobConsts.pas',
  JobDskClasses in '..\Classes\JobDskClasses.pas',
  OperationClasses in '..\Classes\OperationClasses.pas',
  AboutForm in '..\About\AboutForm.pas',
  CallJobItem in '..\CallJob\CallJobItem.pas',
  CallJobItemFrm in '..\CallJob\CallJobItemFrm.pas',
  CommandBatchJobItem in '..\CommandJob\CommandBatchJobItem.pas',
  CommandBatchJobItemFrm in '..\CommandJob\CommandBatchJobItemFrm.pas',
  ConnectionSetup in '..\ConnectionSetup\ConnectionSetup.pas',
  CustomModalDlg in '..\CustomForms\CustomModalDlg.pas' {CustomModalDialog},
  CustomDialog in '..\CustomJob\CustomDialog.pas' {CustomDialogForm},
  CustomJobItems in '..\CustomJob\CustomJobItems.pas',
  CustomParametersDialog in '..\CustomJob\CustomParametersDialog.pas' {CustomParamsJobItemForm},
  CustomScriptDialog in '..\CustomJob\CustomScriptDialog.pas' {CustomScriptJobItemForm},
  CustomRunJobItem in '..\CustomRunJob\CustomRunJobItem.pas',
  CustomRunJobItemFrm in '..\CustomRunJob\CustomRunJobItemFrm.pas' {CustomRunJobItemForm},
  GlobalParamsJobItemFrm in '..\GlobalParams\GlobalParamsJobItemFrm.pas' {GlobalParamsJobItemForm},
  ItemListView in '..\ItemListView\ItemListView.pas' {ItemListViewForm},
  JobCtrls in '..\Lib\JobControls\JobCtrls.pas',
  JobMemData in '..\Lib\JobControls\JobMemData.pas',
  ParametersJobItem in '..\ParamJob\ParametersJobItem.pas',
  ParametersJobItemFrm in '..\ParamJob\ParametersJobItemFrm.pas' {ParametersJobItemForm},
  ReferendesForm in '..\References\ReferendesForm.pas' {JobsReferencesFrame},
  RunJobForm in '..\RunJob\RunJobForm.pas' {RunJobfrm},
  ScripterJobItem in '..\ScriptJob\ScripterJobItem.pas',
  ScripterJobItemFrm in '..\ScriptJob\ScripterJobItemFrm.pas' {ScripterJobItemForm},
  SelectJobItem in '..\SelectJob\SelectJobItem.pas' {SelectjobItemForm},
  SQLScriptJobItem in '..\SQLJob\SQLScriptJobItem.pas',
  SQLScriptJobItemFrm in '..\SQLJob\SQLScriptJobItemFrm.pas' {SQLScriptJobItemForm},
  JobConverter in '..\Utils\JobConverter.pas',
  JobUtils in '..\Utils\JobUtils.pas',
  OperationUtils in '..\Utils\OperationUtils.pas',
  XMLUtils in '..\Utils\XMLUtils.pas',
  ScriptExecutor in '..\ScriptJob\ScriptExecutor.pas',
  PascalScriptExecutor in '..\ScriptJob\PascalScriptExecutor.pas',
  JavaScriptExecutor in '..\ScriptJob\JavaScriptExecutor.pas',
  PascalScriptClassesProxy in '..\ScriptJob\PascalScriptClassesProxy.pas',
  uPSI_PascalScriptClassesProxy in '..\ScriptJob\uPSI_PascalScriptClassesProxy.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
