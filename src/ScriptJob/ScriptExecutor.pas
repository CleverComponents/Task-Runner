unit ScriptExecutor;

interface

uses
  System.Classes, JobClasses;

type
  TScriptExecutor = class
  private
    FScript: TStrings;
    FIsUseLogFile: Boolean;
    FLogFile: string;
  public
    constructor Create(AScript: TStrings; AIsUseLogFile: Boolean; const ALogFile: string);

    function GetParseLexems: string; virtual; abstract;
    function GetWordDelimiters: string; virtual; abstract;
    procedure Execute(AVisitor: TJobVisitor); virtual; abstract;

    property Script: TStrings read FScript;
    property IsUseLogFile: Boolean read FIsUseLogFile;
    property LogFile: string read FLogFile;
  end;

implementation

{ TScriptExecutor }

constructor TScriptExecutor.Create(AScript: TStrings; AIsUseLogFile: Boolean;
  const ALogFile: string);
begin
  inherited Create();

  FScript := AScript;
  FIsUseLogFile := AIsUseLogFile;
  FLogFile := ALogFile;
end;

end.
