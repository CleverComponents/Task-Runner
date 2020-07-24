unit RunJobForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Data.DB, Datasnap.DBClient, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Grids, Vcl.DBGrids, JobClasses, JobConsts,
  JobCtrls, JobMemData;

type
  TRunJobfrm = class(TForm)
    pBottom: TPanel;
    Panel1: TPanel;
    btnClose: TButton;
    PageControl: TPageControl;
    tabJobs: TTabSheet;
    tabLog: TTabSheet;
    MemoLog: TJobRichEdit;
    sidebarSplitter: TSplitter;
    MemoError: TJobRichEdit;
    List: TDBGrid;
    DataSource: TDataSource;
    MemData: TJobMemData;
    MemDatajobname: TStringField;
    MemDatajobstate: TStringField;
    MemDatavisitor: TIntegerField;
    MemDatalog: TBlobField;
    MemDataerrors: TBlobField;
    MemDatacurrentjobname: TStringField;
    MemDataisrun: TBooleanField;
    MemDatacurrentjob: TIntegerField;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PageControlChange(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
  private
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure UpdateVisitorItem(AState: TJobRunState; Visitor: TJobVisitor; IsRootItem: Boolean;
      const ALog, AErrors: string);
    procedure DoStartJob(Visitor: TJobVisitor);
    procedure DoFinishJob(Visitor: TJobVisitor);
    procedure DoItemProcessed(Visitor: TJobVisitor);
    procedure DoItemPerformed(Visitor: TJobVisitor; const ALog, AErrors: string);
    procedure DoHandling();
    function CanCloseForm: Boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;
    constructor CreateInstance;
    class function AccessInstance(Request: integer): TRunJobfrm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Instance: TRunJobfrm;
    class procedure ReleaseInstance;
    procedure ShowProgress(AJobManager: TJobManager);
  end;

implementation

{$R *.DFM}

{ TRunJobfrm }

procedure TRunJobfrm.UpdateVisitorItem(AState: TJobRunState; Visitor: TJobVisitor; IsRootItem: Boolean;
  const ALog, AErrors: string);
var
  CurItem: TJobItem;
begin
  if IsRootItem then
  begin
    CurItem := Visitor.RootJobItem;
  end else
  begin
    CurItem := Visitor.CurrentJobItem;
  end;
  if MemData.Locate('visitor;currentjob;isrun',
    VarArrayOf([Integer(Visitor), Integer(CurItem), True]),
    []) then
  begin
    MemData.Edit();
    try
      MemDatajobstate.AsString := cJobStateNames[AState];
      if not IsRootItem then
      begin
        MemDatalog.AsString := ALog;
        MemDataerrors.AsString := AErrors;
      end;
      MemData.Post();
    except
      MemData.Cancel();
      raise;
    end;
  end;
end;

procedure TRunJobfrm.DoFinishJob(Visitor: TJobVisitor);
begin
  if (Visitor.Errors.Count > 0) then
  begin
    UpdateVisitorItem(jsFailed, Visitor, True, '', '');
  end else
  begin
    UpdateVisitorItem(jsDone, Visitor, True, '', '');
  end;
  if MemData.Locate('visitor;isrun', VarArrayOf([Integer(Visitor), True]), []) then
  begin
    while (MemDatavisitor.AsInteger = Integer(Visitor)) and (not MemData.Eof) do
    begin
      MemData.Edit();
      try
        MemDataisrun.AsBoolean := False;
        MemData.Post();
      except
        MemData.Cancel();
        raise;
      end;
      MemData.Next();
    end;
  end;
  DoHandling();
end;

procedure TRunJobfrm.DoItemProcessed(Visitor: TJobVisitor);
begin
  UpdateVisitorItem(jsStarted, Visitor, False, '', '');
  DoHandling();
end;

procedure TRunJobfrm.DoStartJob(Visitor: TJobVisitor);
  procedure AddRunItem(AName, AFullJobName: string; ACurrentJob: TJobItem; AState: TJobRunState);
  var
    i: Integer;
    Item: TJobItem;
    St: TJobRunState;
    s: string;
  begin
    MemData.Append();
    try
      MemDatavisitor.AsInteger := Integer(Visitor);
      MemDatajobname.AsString := AName;
      s := AFullJobName;
      if (s <> '') and (s[Length(s)] <> '\') then
      begin
        s := s + '\';
      end;
      s := s + ACurrentJob.JobName;
      MemDatacurrentjobname.AsString := s;
      if (AFullJobName = '') then
      begin
        AFullJobName := '\';
      end else
      begin
        AFullJobName := s;
      end;
      MemDatajobstate.AsString := cJobStateNames[AState];
      MemDatalog.Clear();
      MemDataerrors.Clear();
      MemDataisrun.AsBoolean := True;
      MemDatacurrentjob.AsInteger := Integer(ACurrentJob);
      MemData.Post();
    except
      MemData.Cancel();
      raise;
    end;
    for i := 0 to ACurrentJob.ItemsCount - 1 do
    begin
      Item := ACurrentJob.Items[i];
      if (Item.FlowAction = faDisable) then
      begin
        St := jsDisabled;
      end else
      begin
        St := jsWaiting;
      end;
      AddRunItem('', AFullJobName, Item, St);
    end;
  end;
begin
  if not MemData.Active then
  begin
    MemData.Open();
  end;
  AddRunItem(Visitor.JobName, '', Visitor.CurrentJobItem, jsStarted);
  DoHandling();
end;

var
  fInstance: TRunJobfrm = nil;

class function TRunJobfrm.AccessInstance(Request: integer): TRunJobfrm;
begin
  case Request of
    0 : ;
    1 : if not Assigned(fInstance) then fInstance := CreateInstance;
    2 : fInstance := nil;
  else raise Exception.CreateFmt('Illegal request %d in AccessInstance', [Request]);
  end;
  Result := fInstance;
end;

constructor TRunJobfrm.Create(AOwner: TComponent);
begin
  if (csDesigning in ComponentState) then
  begin
    inherited Create(AOwner);
  end else
  begin
    raise Exception.CreateFmt('Access class %s through Instance only', [ClassName]);
  end;
end;

constructor TRunJobfrm.CreateInstance;
begin
  inherited Create(Application);
end;

destructor TRunJobfrm.Destroy;
begin
  if AccessInstance(0) = Self then AccessInstance(2);
  inherited Destroy();
end;

class function TRunJobfrm.Instance: TRunJobfrm;
begin
  Result := AccessInstance(1);
end;

class procedure TRunJobfrm.ReleaseInstance;
begin
  AccessInstance(0).Free();
end;

procedure TRunJobfrm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := Application.MainForm.Handle;
end;

procedure TRunJobfrm.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if (AccessInstance(0) = nil) then Exit;
  if Visible and (Message.Msg = WM_ACTIVATEAPP) then
  begin
    if (BOOL(Message.WParam) = false)then
    begin
      SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
    end
    else
    begin
      if IsWindowEnabled(Handle) then
        SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE{ or SWP_NOACTIVATE});
    end;
  end;
end;

procedure TRunJobfrm.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if (Message.Active <> WA_INACTIVE) then
  begin
    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  end;
end;

procedure TRunJobfrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TRunJobfrm.DoHandling;
begin
  Application.ProcessMessages();
end;

procedure TRunJobfrm.btnCloseClick(Sender: TObject);
begin
  Close();
end;

function TRunJobfrm.CanCloseForm(): Boolean;
var
  S: String;
begin
  S := cJobStateNames[jsStarted];
  Result := not MemData.Locate('jobstate', S, []);
end;

procedure TRunJobfrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CanCloseForm();
end;

procedure TRunJobfrm.PageControlChange(Sender: TObject);
var
  bookmark: TBookmark;
  visitor: Integer;
  tab: TTabSheet;
begin
  tab := PageControl.ActivePage;
  if (tab = nil) or (CompareText(tab.Name, 'tabLog') <> 0) then Exit;
  MemData.DisableControls();
  try
    if (MemDatajobname.AsString <> '') then
    begin
      bookmark := MemData.Bookmark;
      MemData.Bookmark := bookmark;

      MemoLog.Lines.Clear();
      MemoError.Lines.Clear();
      visitor := MemDatavisitor.AsInteger;
      while (MemDatavisitor.AsInteger = visitor) and (not MemData.Eof) do
      begin
        if (MemDatalog.AsString <> '') then
        begin
          MemoLog.Lines.Add(MemDatalog.AsString);
        end;
        if (MemDataerrors.AsString <> '') then
        begin
          MemoError.Lines.Add(MemDataerrors.AsString);
        end;
        MemData.Next();
      end;

      MemData.Bookmark := bookmark;
    end else
    begin
      MemoLog.Lines.Text := MemDatalog.AsString;
      MemoError.Lines.Text := MemDataerrors.AsString;
    end;
  finally
    MemData.EnableControls();
  end;
  DoHandling();
end;

procedure TRunJobfrm.ListDblClick(Sender: TObject);
begin
  PageControl.ActivePage := tabLog;
  PageControlChange(nil);
end;

procedure TRunJobfrm.ShowProgress(AJobManager: TJobManager);
begin
  if (AJobManager <> nil) then
  begin
    AJobManager.OnStartAction := DoStartJob;
    AJobManager.OnFinishAction := DoFinishJob;
    AJobManager.OnItemProcessedAction := DoItemProcessed;
    AJobManager.OnItemPerformedAction := DoItemPerformed;
  end;
  Show();
end;

procedure TRunJobfrm.DoItemPerformed(Visitor: TJobVisitor; const ALog, AErrors: string);
begin
  if (AErrors <> '') then
  begin
    UpdateVisitorItem(jsFailed, Visitor, False, ALog, AErrors);
  end else
  begin
    UpdateVisitorItem(jsDone, Visitor, False, ALog, AErrors);
  end;
  DoHandling();
end;

initialization

finalization
  TRunJobfrm.ReleaseInstance();

end.
