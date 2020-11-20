unit SQLScriptJobItemFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ToolWin, ComCtrls, CustomDialog, JobConsts, JobClasses,
  CustomScriptDialog, JobUtils, JobCtrls;

type
  TSQLScriptJobItemForm = class(TCustomScriptJobItemForm)
    btnConnection: TButton;
    cmbPerformWith: TJobComboBox;
    lblPerformWith: TLabel;
    pConnection: TPanel;
    procedure btnConnectionClick(Sender: TObject);
    procedure cmbPerformWithChange(Sender: TObject);
  private
    FConnectionInfo: TSQLConnectionInfo;
    procedure DoChangeConnection();
    function GetPerformWith: TSQLPerformWith;
    procedure SetPerformWith(AValue: TSQLPerformWith);
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
    SetPerformWith(TSQLScriptJobDataItem(Data).PerformWith);
  end else
  begin
    TSQLScriptJobDataItem(Data).ConnectionInfo.Assign(FConnectionInfo);
    TSQLScriptJobDataItem(Data).PerformWith := GetPerformWith();
  end;
end;

procedure TSQLScriptJobItemForm.btnConnectionClick(Sender: TObject);
begin
  DoChangeConnection();
end;

procedure TSQLScriptJobItemForm.cmbPerformWithChange(Sender: TObject);
begin
  if IsLoading then Exit;
  IsModified := True;
  UpdateControls();
end;

procedure TSQLScriptJobItemForm.DoChangeConnection;
var
  performWith: TSQLPerformWith;
begin
  performWith := GetPerformWith();
  if ShowConnectionSetup(FConnectionInfo, ReadOnly, performWith) then
  begin
    SetPerformWith(performWith);
    IsModified := True;
    UpdateControls();
  end;
end;

function TSQLScriptJobItemForm.GetPerformWith: TSQLPerformWith;
var
  Index: Integer;
begin
  Index := cmbPerformWith.ItemIndex;
  if (Index < 0) then
  begin
    Result := Low(TSQLPerformWith);
  end else
  begin
    Result := TSQLPerformWith(Index);
  end;
end;

procedure TSQLScriptJobItemForm.SetPerformWith(AValue: TSQLPerformWith);
begin
  cmbPerformWith.ItemIndex := Integer(AValue);
end;

procedure TSQLScriptJobItemForm.UpdateControls;
begin
  inherited UpdateControls();

  btnErrorWords.Enabled := (not ReadOnly) and (GetPerformWith() = spOSQLUtilite);
  cmbFlowAction.Enabled := not ReadOnly;
end;

constructor TSQLScriptJobItemForm.Create(AOwner: TComponent);
var
  i: TSQLPerformWith;
begin
  inherited Create(AOwner);

  FConnectionInfo := TSQLConnectionInfo.Create();

  cmbPerformWith.Items.Clear();
  for i := Low(cSQLPerformWithNames) to High(cSQLPerformWithNames) do
  begin
    cmbPerformWith.Items.Add(cSQLPerformWithNames[i]);
  end;
end;

destructor TSQLScriptJobItemForm.Destroy;
begin
  FConnectionInfo.Free();
  inherited Destroy();
end;

{ TCustomJobEditorItem }

function TSQLScriptJobEditorItem.GetEditorFormClass: TCustomDialogFormClass;
begin
  Result := TSQLScriptJobItemForm;
end;

initialization
  RegisterEditorItem(TSQLScriptJobEditorItem, TSQLScriptJobDataItem, 'SQL Script');

end.
