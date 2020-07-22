unit SQLScriptJobItemFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ToolWin, ComCtrls, CustomDialog, JobConsts, JobClasses,
  CustomScriptDialog, JobUtils, JobCtrls;

type
  TSQLScriptJobItemForm = class(TCustomScriptJobItemForm)
    btnConnection: TButton;
    cmbPerformWith: TEdit;
    lblPerformWith: TLabel;
    procedure btnConnectionClick(Sender: TObject);
  private
    FConnectionInfo: TSQLConnectionInfo;
    FPerformWith: TSQLPerformWith;
    procedure DoChangeConnection();
  protected
    procedure UpdateControls; override;
    procedure AssignData(IsFromDataItem: Boolean = False); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TSQLScriptJobEditorItem = class(TCustomJobEditorItem)
  protected
    function GetEditorFormClass: TCustomDialogFormClass; override;
  end;

implementation

{$R *.DFM}

uses
  SQLScriptJobItem, ConnectionSetup;

{ TSQLScriptJobItemForm }

procedure TSQLScriptJobItemForm.AssignData(IsFromDataItem: Boolean);
begin
  inherited AssignData(IsFromDataItem);
  if IsFromDataItem then
  begin
    FConnectionInfo.Assign(TSQLScriptJobDataItem(Data).ConnectionInfo);
    FPerformWith := TSQLScriptJobDataItem(Data).PerformWith;
  end else
  begin
    TSQLScriptJobDataItem(Data).ConnectionInfo.Assign(FConnectionInfo);
    TSQLScriptJobDataItem(Data).PerformWith := FPerformWith;
  end;
end;

procedure TSQLScriptJobItemForm.btnConnectionClick(Sender: TObject);
begin
  DoChangeConnection();
end;

procedure TSQLScriptJobItemForm.DoChangeConnection;
begin
  if ShowConnectionSetup(FConnectionInfo, ReadOnly, FPerformWith) then
  begin
    IsModified := True;
    UpdateControls();
  end;
end;

{ TCustomJobEditorItem }

function TSQLScriptJobEditorItem.GetEditorFormClass: TCustomDialogFormClass;
begin
  Result := TSQLScriptJobItemForm;
end;

procedure TSQLScriptJobItemForm.UpdateControls;
begin
  inherited UpdateControls();
  btnErrorWords.Enabled := (FPerformWith = spOSQLUtilite);
  cmbPerformWith.Text := cSQLPerformWithNames[FPerformWith];
end;

constructor TSQLScriptJobItemForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectionInfo := TSQLConnectionInfo.Create();
end;

destructor TSQLScriptJobItemForm.Destroy;
begin
  FConnectionInfo.Free();
  inherited Destroy();
end;

initialization
  RegisterEditorItem(TSQLScriptJobEditorItem, TSQLScriptJobDataItem, 'SQL Script');

end.
