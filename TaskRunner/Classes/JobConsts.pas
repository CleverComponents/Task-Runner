unit JobConsts;

interface

type
  TFlowAction = (faComplete, faSuccess, faFailure, faDisable, faFailThrow, faCompleteThrow);
  TJobRunState = (jsStarted, jsDone, jsFailed, jsWaiting, jsDisabled);
  TJobListBindType = (btAND, btOR);
  TScripterLanguage = (slPascalScript, slJavaScript);
  TSQLPerformWith = (spOSQLUtilite, spADOLibrary, spScriptEngine, spUpdateDBEngine);

const
  cGlobalJobLibPath = 'JOBLIBRARYPATH';
  cJobDeskTopFileName = 'TaskRunner.dsw';
  cJobGlobalParamsFileName = 'TaskRunner.prm';
  cParseLexems = ':';
  cWordDelimiters = #32;

  cGlobalParamsParseLexems = '@@';
  cCommandBatchParseLexems = '::';
  cCommandBatchWordDelimiters = #32 + '=';

  cSQLScriptWordDelimiters = #32 + '[]''"';

  cPascalScriptParams = 'JobParams';
  cPascalScriptJobLog = 'JobLog';
  cPascalScriptParseLexems = 'etParam(''';
  cPascalScriptWordDelimiters = '''GS';

  cJavaScriptParams = 'jobParams';
  cJavaScriptJobLog = 'jobLog';
  cJavaScriptParseLexems = 'etParam("';
  cJavaScriptWordDelimiters = '"gs';

  cNOClause = 'NO';
  cScriptClause = 'SCRIPT';
  cSQlErrorWords: array[0..1] of string = ('Msg', 'Level');

const
  cFlowActionNames: array[TFlowAction] of string = ('Complete', 'Success', 'Failure', 'Disable',
    'Fail&Throw', 'Complete&Throw');
  cJobStateNames: array[TJobRunState] of string = ('Started', 'Done', 'Failed', 'Waiting', 'Disabled');
  cJobListBindTypeNames: array[TJobListBindType] of string = ('AND', 'OR');

  cScripterLanguages: array[TScripterLanguage] of string = ('PascalScript', 'JavaScript');

  cSQLPerformWithNames: array[TSQLPerformWith] of string = (
    'OSQL Utilite', 'OLE DB', 'SQL Script Engine', 'SQL Update DB Engine');

  cStoreFlowActionNames: array[TFlowAction] of string = ('Complete', 'Success', 'Failure', 'Disable',
    'FailThrow', 'CompleteThrow');
  cStoreBindTypeNames: array[TJobListBindType] of string = ('and', 'or');
  cStoreBoolean: array[Boolean] of string = ('No', 'Yes');
  cStoreSQLPerformWithNames: array[TSQLPerformWith] of string = (
    'OSQLUtilite', 'ADOLibrary', 'ScriptEngine', 'UpdateDBEngine');

resourcestring
  cMainFormCaption = 'Task Runner';
  cMediaModified = 'The project has been modified, do you wish to save your changes?';
  cCallJobNonExist = 'The called job ''%s'' from project ''%s'' does not exist';
  cJobDataLocked = 'The job have been modified or run';
  cJobLocked = 'Some jobs have been modified or run';
  cJobModified = 'Some jobs have been modified';
  cJobNotFound = 'The job is not found in the job list';
  cJobAlreadyRun = 'The job is already run';
  cJobRunning = 'The job is run, cannot modify';
  cPerformanceStopped = 'The job performance was stopped';
  cNonRegisteredEditor = 'The editor for this job is not registered';
  cJobModifiedQuery = 'The job has been modified, do you wish to apply your changes?';
  cParameterAdded = 'The job parameter ''%s'' have been added with value = ''%s''';
  cParameterReplaced = 'The job parameter ''%s''have been replaced with value = ''%s''';
  cJobLogInFile = 'The job log is in ''%s'' file';
  cJobPerformMessage = 'Job name: %s =============================================';
  cNewJobItemName = 'New %s Item';
  cCallJobError = 'An error occured while the ''%s'' job run';
  cJobDisabled = 'The job ''%s'' is disabled';

  cCreateError = 'Cannot create ''%s''';
  cDeleteError = 'Cannot delete ''%s''';
  cCannotRunFile = 'Cannot run file ''%s'', GetLastError = %d';
  cFileNameEmpty = '%s file name is empty';
  cFileNotExists = 'The file ''%s'' does not exist';
  cUnkmownImportJobFile = 'Unknown import job file ''%s''';
  cUnknownImportMediaFile = 'Unknown import project file ''%s''';
  cLoadError = 'Loading error';

  cScriptError = 'The script engine was returned with some errors, see error log.';
  cJobSkipped = 'The performance of ''%s'' job have been skipped due to ''Can Perform'' condition';
  cCommandLineDescr = 'Command Line to Run: %s';

  cScriptSetParamError = 'setParam accepts two string parameters';
  cScriptGetParamError = 'getParam accepts one string parameter';
  cScriptAddLogError = 'add accepts one string parameter';

const
  cCursorPositionMask = 'Ln %s, Col %s';
  cMinDskEditorsCount = 10;
  cMinDskMediaCount = 5;
  
implementation

end.
