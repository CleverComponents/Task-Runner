unit JobsMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.UITypes, System.Variants, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Grids,
  OperationUtils, JobClasses, JobCtrls, OperationClasses, JobDskClasses, Vcl.ImgList,
  Winapi.msxml, XMLUtils, Vcl.Menus, ReferendesForm, System.ImageList, JobConsts, JobUtils, TabEditors;

type
  TJobsMainFrame = class(TFrame)
    pCenter: TPanel;
    odMediaFile: TOpenDialog;
    sdMediaFile: TSaveDialog;
    pFlowAction: TPanel;
    pRightFlowAction: TPanel;
    Label1: TLabel;
    edtFlowAction: TJobComboBox;
    imgJobFlowAction: TImageList;
    MemoDescription: TJobRichEdit;
    JobsList: TJobTreeView;
    JobPopupMenu: TPopupMenu;
    Bevel1: TBevel;
    procedure JobsListChange(Sender: TObject; Node: TTreeNode);
    procedure JobsListDeletion(Sender: TObject; Node: TTreeNode);
    procedure JobsListDblClick(Sender: TObject);
    procedure JobsListStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure JobsListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure JobsListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure JobsListEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure MemoDescriptionChange(Sender: TObject);
    procedure edtFlowActionChange(Sender: TObject);
    procedure JobsListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure JobsListContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  private
    FJobManager: TJobManager;
    FMediaFileName: String;
    FOperationList: TFormOperationList;
    FIsLoading: Boolean;
    FIsModified: Boolean;
    FDraggedNode: TTreeNode;
    FIsChanging: Boolean;
    FJobClipBoard: TJobItem;
    FOnGetGlobalParams: TOnGetGlobalParamsEvent;
    FReferencesForm: TJobsReferencesFrame;
    FTabManager: TTabEditorsManager;

    procedure SetFlowAction(AFlowAction: TFlowAction);
    function InternalInsertItem(AParentItem: TJobItem; IsSubItem: Boolean = False): TJobItem;
    procedure InternalSaveMedia(AFileName: String);
    procedure LoadTreeList;
    procedure DoDataStateChanged(AJobItem: TJobItem; State: TJobState);
    procedure DoDataChanged(AJobItem: TJobItem);
    procedure SetIsModified(const Value: Boolean);
    procedure InsertTreeItem(ANode: TTreeNode; AItem: TJobItem);
    procedure DoGetGlobalParams(var Params: TJobOperationParams);
    procedure ClearControls;
    procedure DoShowReferences(Sender: TObject);
    procedure DoEnableJob(Sender: TObject);
    procedure DoDisableJob(Sender: TObject);
    procedure DoExportJob(Sender: TObject);
    procedure DoImportJob(Sender: TObject);
    procedure DoDeleteJobItem(Sender: TObject);
    procedure DoEditJobItem(Sender: TObject);
    procedure DoAddJobItem(Sender: TObject);
    procedure DoAddJobSubItem(Sender: TObject);
    procedure DoLoadMedia(Sender: TObject);
    procedure DoNewMedia(Sender: TObject);
    procedure DoSaveMedia(Sender: TObject);
    procedure DoSaveMediaAs(Sender: TObject);
    procedure DoStartJobAt(Sender: TObject);
    procedure DoStopSelectedJob(Sender: TObject);
    procedure DoStopAllJobs(Sender: TObject);
    procedure DoCopyJob(Sender: TObject);
    procedure DoPasteJob(Sender: TObject);
    procedure DoLoadLastMedia(Params: TJobOperationParams; var Success: Boolean);
    procedure UpdateTreeList;
    procedure UnlinkChildItems(Node: TTreeNode);
    procedure DoBeforeRun(AJobItem: TJobItem);
    procedure DoCreateEditor(AJobItem: TJobItem; AEditor: TJobEditorItem);
  protected
    procedure AddOperations; virtual;
    procedure UpdateControls; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanCloseForm: Boolean;
    procedure ShowForm;
    procedure NewMedia;
    procedure LoadMedia(AFileName: String);
    procedure LoadDesktop(ANode: IXMLDOMNode);
    procedure SaveDesktop(ANode: IXMLDOMNode);

    property OperationList: TFormOperationList read FOperationList;
    property IsModified: Boolean read FIsModified write SetIsModified;
    property JobManager: TJobManager read FJobManager;
    property TabManager: TTabEditorsManager read FTabManager write FTabManager;

    property OnGetGlobalParams: TOnGetGlobalParamsEvent read FOnGetGlobalParams write FOnGetGlobalParams;
  end;

implementation

uses
  SelectJobItem, RunJobForm, CustomDialog;

{$R *.DFM}

{ TJobsMainFrame }

procedure TJobsMainFrame.InsertTreeItem(ANode: TTreeNode; AItem: TJobItem);
var
  i: Integer;
  Node: TTreeNode;
  Item: TJobItem;
begin
  if (AItem.ItemsCount > 0) then
  begin
    Item := AItem.Items[0];
    Node := JobsList.Items.AddChildObjectFirst(ANode, Item.JobName, Item);
    Node.ImageIndex := Integer(Item.FlowAction);
    Node.SelectedIndex := Integer(Item.FlowAction);
    InsertTreeItem(Node, Item);
  end else
  begin
    Node := nil;
  end;

  for i := 1 to AItem.ItemsCount - 1 do
  begin
    Item := AItem.Items[i];
    Node := JobsList.Items.AddObject(Node, Item.JobName, Item);
    Node.ImageIndex := Integer(Item.FlowAction);
    Node.SelectedIndex := Integer(Item.FlowAction);
    InsertTreeItem(Node, Item);
  end;
end;

procedure TJobsMainFrame.LoadTreeList();
var
  i: Integer;
  Node: TTreeNode;
  Item: TJobItem;
begin
  Node := nil;
  for i := 0 to FJobManager.RootItemsCount - 1 do
  begin
    Item := FJobManager.RootItems[i];
    Node := JobsList.Items.AddObject(Node, Item.JobName, Item);
    Node.ImageIndex := Integer(Item.FlowAction);
    Node.SelectedIndex := Integer(Item.FlowAction);
    InsertTreeItem(Node, Item);
  end;
end;

procedure TJobsMainFrame.DoNewMedia(Sender: TObject);
begin
  NewMedia();
end;

procedure TJobsMainFrame.DoLoadMedia(Sender: TObject);
begin
  if not CanCloseForm() then Exit;
  if odMediaFile.Execute() then
  begin
    LoadMedia(odMediaFile.FileName);
  end;
end;

procedure TJobsMainFrame.InternalSaveMedia(AFileName: String);
var
  Doc: IXMLDOMDocument;
  MainNode: IXMLDOMNode;
begin
  if FJobManager.IsLocked then
  begin
    raise Exception.Create(cJobLocked);
  end;
  if (Trim(AFileName) = '') then
  begin
    if sdMediaFile.Execute() then
    begin
      AFileName := sdMediaFile.FileName;
    end else
    begin
      Exit;
    end;
  end;
  Doc := CoDOMDocument.Create();
  MainNode := Doc.createElement('Main');
  Doc.appendChild(MainNode);
  FJobManager.Store(MainNode);
  SaveXMLToFile(AFileName, Doc);
  FMediaFileName := AFileName;
  TJobDskMediaStorage.Instance.RenameCurrentItem(FMediaFileName);
  IsModified := False;
end;

procedure TJobsMainFrame.DoSaveMedia(Sender: TObject);
begin
  InternalSaveMedia(FMediaFileName);
end;

procedure TJobsMainFrame.DoSaveMediaAs(Sender: TObject);
begin
  InternalSaveMedia('');
  UpdateControls();
end;

function TJobsMainFrame.InternalInsertItem(AParentItem: TJobItem; IsSubItem: Boolean = False): TJobItem;
var
  ANewName: String;
  ANewType: TJobDataItemClass;
begin
  Result := nil;
  ANewName := 'New Item';

  if SelectJobItemDlg(ANewName, ANewType) then
  begin
    if IsSubItem then
    begin
      Result := FJobManager.AddJobSubItem(AParentItem, ANewName, ANewType);
    end else
    begin
      Result := FJobManager.AddJobItem(AParentItem, ANewName, ANewType);
//      FJobManager.MoveJobItem(AParentItem, Result);
    end;
    IsModified := True;
  end;
end;

procedure TJobsMainFrame.DoAddJobItem(Sender: TObject);
var
  Node, NewNode: TTreeNode;
  AItem: TJobItem;
begin
  Node := JobsList.Selected;
  if (Node <> nil) then
  begin
    AItem := TJobItem(Node.Data);
  end else
  begin
    AItem := nil;
  end;
  AItem := InternalInsertItem(AItem);
  if (AItem <> nil) then
  begin
    NewNode := JobsList.Items.AddObject(Node, AItem.JobName, AItem);
    NewNode.ImageIndex := Integer(AItem.FlowAction);
    NewNode.SelectedIndex := Integer(AItem.FlowAction);
    NewNode.Selected := True;
    FJobManager.EditJobItem(TJobItem(NewNode.Data));
    UpdateControls();
  end;
end;

procedure TJobsMainFrame.DoAddJobSubItem(Sender: TObject);
var
  Node, NewNode: TTreeNode;
  AItem: TJobItem;
begin
  Node := JobsList.Selected;
  if (Node = nil) then Exit;
  AItem := TJobItem(Node.Data);
  AItem := InternalInsertItem(AItem, True);
  if (AItem <> nil) then
  begin
    NewNode := JobsList.Items.AddChildObject(Node, AItem.JobName, AItem);
    NewNode.ImageIndex := Integer(AItem.FlowAction);
    NewNode.SelectedIndex := Integer(AItem.FlowAction);
    NewNode.Selected := True;
    Node.Expand(False);
    FJobManager.EditJobItem(TJobItem(NewNode.Data));
    UpdateControls();
  end;
end;

procedure TJobsMainFrame.DoCreateEditor(AJobItem: TJobItem; AEditor: TJobEditorItem);
begin
  (AEditor as TCustomJobEditorItem).TabManager := TabManager;
end;

procedure TJobsMainFrame.DoEditJobItem(Sender: TObject);
var
  Item: TJobItem;
begin
  Item := nil;
  if (JobsList.Selected <> nil) then
  begin
    Item := TJobItem(JobsList.Selected.Data);
  end;
  if (Item = nil) then Exit;
  FJobManager.EditJobItem(Item);
end;

procedure TJobsMainFrame.DoEnableJob(Sender: TObject);
begin
  SetFlowAction(faSuccess);
  edtFlowAction.ItemIndex := Integer(faSuccess);
end;

procedure TJobsMainFrame.DoDeleteJobItem(Sender: TObject);
var
  i: Integer;
  Node: TTreeNode;
  List: TList;
begin
  List := TList.Create();
  JobsList.Items.BeginUpdate();
  try
    for i := 0 to JobsList.Items.Count - 1 do
    begin
      Node := JobsList.Items[i];
      if Node.Selected then
      begin
        List.Add(Node);
      end;
    end;
    for i := 0 to List.Count - 1 do
    begin
      Node := TTreeNode(List[i]);
      JobsList.Items.Delete(Node);
    end;
  finally
    JobsList.Items.EndUpdate();
    List.Free();
  end;
  UpdateControls();
end;

procedure TJobsMainFrame.DoDisableJob(Sender: TObject);
begin
  SetFlowAction(faDisable);
  edtFlowAction.ItemIndex := Integer(faDisable);
end;

procedure TJobsMainFrame.DoStopSelectedJob(Sender: TObject);
var
  i: Integer;
  Node: TTreeNode;
begin
  if not CanCloseForm() then Exit;
  for i := 0 to JobsList.Items.Count - 1 do
  begin
    Node := JobsList.Items[i];
    if Node.Selected then
    begin
      FJobManager.StopJob(TJobItem(Node.Data));
    end;
  end;
end;

constructor TJobsMainFrame.Create(AOwner: TComponent);
var
  i: TFlowAction;
begin
  inherited Create(AOwner);

  FJobManager := TJobManager.Create();
  edtFlowAction.Items.Clear();
  for i := Low(cFlowActionNames) to High(cFlowActionNames) do
  begin
    edtFlowAction.Items.Add(cFlowActionNames[i]);
  end;
  FDraggedNode := nil;
  FJobClipBoard := nil;
  FIsLoading := False;
  FIsModified := False;
  FIsChanging := False;
  FOperationList := TFormOperationList.Create();
  FMediaFileName := '';
  AddOperations();
  UpdateControls();

  FJobManager.OnDataStateChanged := DoDataStateChanged;
  FJobManager.OnDataChanged := DoDataChanged;
  FJobManager.OnGetGlobalParams := DoGetGlobalParams;
  FJobManager.OnBeforeRun := DoBeforeRun;
  FJobManager.OnCreateEditor := DoCreateEditor;

  RegisterGlobalOperation(gopLoadLastMedia, DoLoadLastMedia);
end;

procedure TJobsMainFrame.DoGetGlobalParams(var Params: TJobOperationParams);
begin
  if Assigned(FOnGetGlobalParams) then
  begin
    FOnGetGlobalParams(Params);
  end;
end;

destructor TJobsMainFrame.Destroy;
begin
  FReferencesForm.Free();
  UnRegisterGlobalOperation(gopLoadLastMedia);
  if (FJobClipBoard <> nil) then
  begin
    FJobClipBoard.Free();
  end;
  FJobManager.OnDataStateChanged := nil;
  FOperationList.Free();
  FJobManager.Free();
  inherited Destroy();
end;

procedure TJobsMainFrame.DoImportJob(Sender: TObject);
var
  AFileName: String;
  Node, NewNode: TTreeNode;
  AItem: TJobItem;
  ADataItemClass: TJobDataItemClass;
  Doc: IXMLDOMDocument;
  JobNode: IXMLDOMNode;
begin
  if odMediaFile.Execute() then
  begin
    AFileName := odMediaFile.FileName;
  end else
  begin
    Exit;
  end;
  Node := JobsList.Selected;
  if (Node <> nil) then
  begin
    AItem := TJobItem(Node.Data);
  end else
  begin
    AItem := nil;
  end;
  Doc := CoDOMDocument.Create();
  Doc.load(AFileName);
  if (Doc.xml = '') then
  begin
    raise Exception.CreateFmt(cUnknownImportMediaFile, [AFileName]);
  end;
  JobNode := Doc.selectSingleNode('Job');
  if JobNode = nil then
  begin
    raise Exception.CreateFmt(cUnknownImportMediaFile, [AFileName]);
  end;
  ADataItemClass := TJobDataItemClass(GetClass((JobNode as IXMLDOMElement).getAttribute('dataclassname')));
  if (ADataItemClass <> nil) then
  begin
    AItem := FJobManager.AddJobItem(AItem, '', ADataItemClass);
    if (AItem <> nil) then
    begin
      AItem.Load(JobNode);
      NewNode := JobsList.Items.AddObject(Node, AItem.JobName, AItem);
      NewNode.ImageIndex := Integer(AItem.FlowAction);
      NewNode.SelectedIndex := Integer(AItem.FlowAction);
      NewNode.Selected := True;
      InsertTreeItem(NewNode, AItem);
      FJobManager.EditJobItem(TJobItem(NewNode.Data));
      IsModified := True;
    end;
  end else
  begin
    raise Exception.CreateFmt(cUnkmownImportJobFile, [AFileName]);
  end;
end;

procedure TJobsMainFrame.DoExportJob(Sender: TObject);
var
  AFileName: String;
  Node: TTreeNode;
  Doc: IXMLDOMDocument;
  JobNode: IXMLDOMNode;
begin
  Node := JobsList.Selected;
  if (Node = nil) then Exit;
  if sdMediaFile.Execute() then
  begin
    AFileName := sdMediaFile.FileName;
  end else
  begin
    Exit;
  end;
  Doc := CoDOMDocument.Create();
  JobNode := Doc.createElement('Job');
  Doc.appendChild(JobNode);
  (JobNode as IXMLDOMElement).setAttribute('dataclassname', TJobItem(Node.Data).Data.ClassName);
  TJobItem(Node.Data).Store(JobNode);
  Doc.save(AFileName);
end;

procedure TJobsMainFrame.AddOperations;
begin
  FOperationList.AddOperation(opNewMedia, DoNewMedia);
  FOperationList.AddOperation(opLoadMedia, DoLoadMedia);
  FOperationList.AddOperation(opSaveMedia, DoSaveMedia);
  FOperationList.AddOperation(opSaveMediaAs, DoSaveMediaAs);
  FOperationList.AddOperation(opAddJobItem, DoAddJobItem);
  FOperationList.AddOperation(opAddJobSubItem, DoAddJobSubItem);
  FOperationList.AddOperation(opEditJobItem, DoEditJobItem);
  FOperationList.AddOperation(opDeleteJobItem, DoDeleteJobItem);
  FOperationList.AddOperation(opStartJobAt, DoStartJobAt);
  FOperationList.AddOperation(opStopSelectedJob, DoStopSelectedJob);
  FOperationList.AddOperation(opStopAllJobs, DoStopAllJobs);
  FOperationList.AddOperation(opCopyJob, DoCopyJob);
  FOperationList.AddOperation(opPasteJob, DoPasteJob);
  FOperationList.AddOperation(opImportJob, DoImportJob);
  FOperationList.AddOperation(opExportJob, DoExportJob);
  FOperationList.AddOperation(opEnableJob, DoEnableJob);
  FOperationList.AddOperation(opDisableJob, DoDisableJob);
  FOperationList.AddOperation(opShowReferences, DoShowReferences);
end;

procedure TJobsMainFrame.UpdateControls;
var
  cnt: Integer;
  Node: TTreeNode;
  frm: TForm;
begin
  if FIsLoading or (csDestroying in ComponentState) then Exit;
  cnt := JobsList.SelectionCount;
  Node := JobsList.Selected;

  frm := Application.MainForm;
  if (frm <> nil) then
  begin
    frm.Caption := GetMainFormCaption(FMediaFileName);
  end;

  FOperationList.EnableOperation(opAddJobSubItem, cnt = 1);
  FOperationList.EnableOperation(opEditJobItem, cnt = 1);
  FOperationList.EnableOperation(opDeleteJobItem, cnt > 0);
  FOperationList.EnableOperation(opStartJobAt, cnt > 0);
  FOperationList.EnableOperation(opStopAllJobs, JobsList.Items.Count > 0);
  FOperationList.EnableOperation(opSaveMedia, FIsModified);
  FOperationList.EnableOperation(opSaveMediaAs, True);
  FOperationList.EnableOperation(opCopyJob, cnt = 1);
  FOperationList.EnableOperation(opPasteJob, FJobClipBoard <> nil);
  FOperationList.EnableOperation(opImportJob, True);
  FOperationList.EnableOperation(opExportJob, cnt = 1);
  FOperationList.EnableOperation(opEnableJob, cnt > 0);
  FOperationList.EnableOperation(opDisableJob, cnt > 0);
  FOperationList.EnableOperation(opStopSelectedJob, cnt > 0);
  
  MemoDescription.Enabled := (Node <> nil);
  edtFlowAction.Enabled := (Node <> nil);
end;

procedure TJobsMainFrame.JobsListChange(Sender: TObject; Node: TTreeNode);
begin
  if FIsLoading then Exit;
  if (Node <> nil) then
  begin
    TJobItem(Node.Data).JobName := Node.Text;
    FIsChanging := True;
    try
      MemoDescription.Lines.Assign(TJobItem(Node.Data).Description);
      edtFlowAction.ItemIndex := Integer(TJobItem(Node.Data).FlowAction);
    finally
      FIsChanging := False;
    end;
  end;
  UpdateControls();
end;

procedure TJobsMainFrame.UnlinkChildItems(Node: TTreeNode);
var
  i: Integer;
begin
  for i := 0 to Node.Count - 1 do
  begin
    Node[i].Data := nil;
    UnlinkChildItems(Node[i]);
  end;
end;

procedure TJobsMainFrame.JobsListDeletion(Sender: TObject; Node: TTreeNode);
var
  item: TJobItem;
begin
  if FIsLoading or (csDestroying in ComponentState) then Exit;

  item := TJobItem(Node.Data);
  if (item <> nil) then
  begin
    FJobManager.RemoveJobItem(item);
    UnlinkChildItems(Node);
  end;

  IsModified := True;
end;

function TJobsMainFrame.CanCloseForm: Boolean;
begin
  Result := not FJobManager.IsLocked;
  if not Result then
  begin
    raise Exception.Create(cJobLocked);
  end;
  if IsModified then
  begin
    case MessageDlg(cMediaModified, mtWarning, mbYesNoCancel, 0) of
      mrYES: InternalSaveMedia(FMediaFileName);
      mrNO: IsModified := False;
      else Result := False;
    end;
  end;
end;

procedure TJobsMainFrame.JobsListDblClick(Sender: TObject);
begin
  DoEditJobItem(nil);
end;

procedure TJobsMainFrame.DoBeforeRun(AJobItem: TJobItem);
var
  RunFrm: TRunJobfrm;
begin
  RunFrm := TRunJobfrm.Instance;
  RunFrm.FreeNotification(Self);
  RunFrm.ShowProgress(FJobManager);
end;

procedure TJobsMainFrame.DoStartJobAt(Sender: TObject);
var
  i: Integer;
  Node: TTreeNode;
begin
  for i := 0 to JobsList.Items.Count - 1 do
  begin
    Node := JobsList.Items[i];
    if Node.Selected then
    begin
      if TJobItem(Node.Data).CheckJobState(jsEdited) then
      begin
        raise Exception.Create(cJobModified);
      end;
      FJobManager.RunJob(TJobItem(Node.Data), False);
    end;
  end;
end;

procedure TJobsMainFrame.DoDataStateChanged(AJobItem: TJobItem; State: TJobState);
begin
  if (State = jsEdited) then
  begin
    IsModified := True;
  end;
end;

procedure TJobsMainFrame.SetFlowAction(AFlowAction: TFlowAction);
var
  i: Integer;
  Node: TTreeNode;
  JobItem: TJobItem;
begin
  if FIsChanging or (csDestroying in ComponentState) then Exit;
  JobsList.Items.BeginUpdate();
  try
    for i := 0 to JobsList.Items.Count - 1 do
    begin
      Node := JobsList.Items[i];
      if Node.Selected then
      begin
        JobItem := TJobItem(Node.Data);
        if JobItem.IsLocked then
        begin
          raise Exception.Create(cJobDataLocked);
        end;
        FIsChanging := True;
        try
          JobItem.Data.FlowAction := AFlowAction;
          Node.ImageIndex := Integer(AFlowAction);
          Node.SelectedIndex := Integer(AFlowAction);
          IsModified := True;
        finally
          FIsChanging := False;
        end;
      end;
    end;
  finally
    JobsList.Items.EndUpdate();
  end;
end;

procedure TJobsMainFrame.SetIsModified(const Value: Boolean);
begin
  if (FIsModified <> Value) then
  begin
    FIsModified := Value;
    UpdateControls();
  end;
end;

procedure TJobsMainFrame.ShowForm;
begin
  Parent.Width := 320;
end;

procedure TJobsMainFrame.JobsListStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  FDraggedNode := JobsList.Selected;
end;

procedure TJobsMainFrame.JobsListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Node: TTreeNode;
begin
  Node := JobsList.GetNodeAt(X, Y);
  FDraggedNode.MoveTo(Node, naInsert);
  FJobManager.MoveJobItem(TJobItem(Node.Data), TJobItem(FDraggedNode.Data), True);
  IsModified := True;
end;

procedure TJobsMainFrame.JobsListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Sender is TJobTreeView) and (Source is TJobTreeView) and
             Assigned(JobsList.GetNodeAt(X, Y));
end;

procedure TJobsMainFrame.DoStopAllJobs(Sender: TObject);
begin
  FJobManager.StopAllJobs();
end;

procedure TJobsMainFrame.NewMedia;
begin
  if CanCloseForm() then
  begin
    FMediaFileName := '';
    FIsLoading := True;
    try
      JobsList.Items.Clear();
      FJobManager.ClearItems();
    finally
      FIsLoading := False;
    end;
    ClearControls();
    TJobDskMediaStorage.Instance.SetCurrentItem(FMediaFileName);
    UpdateControls();
  end;
end;

procedure TJobsMainFrame.JobsListEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
begin
  TJobItem(Node.Data).JobName := S;
  IsModified := True;
end;

procedure TJobsMainFrame.DoDataChanged(AJobItem: TJobItem);
var
  Node: TTreeNode;
begin
  if FIsChanging or (csDestroying in ComponentState) or (AJobItem = nil) then Exit;
  Node := JobsList.Selected;
  if (Node <> nil) and (TJobItem(Node.Data) = AJobItem) then
  begin
    FIsChanging := True;
    try
      MemoDescription.Lines.Assign(AJobItem.Description);
      edtFlowAction.ItemIndex := Integer(AJobItem.FlowAction);
    finally
      FIsChanging := False;
    end;
  end;
  UpdateTreeList();
end;

procedure TJobsMainFrame.MemoDescriptionChange(Sender: TObject);
var
  Node: TTreeNode;
  JobItem: TJobItem;
begin
  if FIsChanging or (csDestroying in ComponentState) then Exit;

  Node := JobsList.Selected;
  if (Node <> nil) then
  begin
    JobItem := TJobItem(Node.Data);
    if JobItem.IsLocked then
    begin
      raise Exception.Create(cJobDataLocked);
    end;

    FIsChanging := True;
    try
      JobItem.Description.Assign(MemoDescription.Lines);
      IsModified := True;
    finally
      FIsChanging := False;
    end;
  end;
end;

procedure TJobsMainFrame.edtFlowActionChange(Sender: TObject);
begin
  SetFlowAction(TFlowAction(edtFlowAction.ItemIndex));
end;

procedure TJobsMainFrame.DoCopyJob(Sender: TObject);
var
  AJob: TJobItem;
  Node: TTreeNode;
begin
  Node := JobsList.Selected;
  if (Node <> nil) then
  begin
    AJob := TJobItem(Node.Data);
    if (FJobClipBoard <> nil) then
    begin
      FJobClipBoard.Free();
    end;
    FJobClipBoard := TJobItem.Create(nil, nil, AJob.JobName, TJobDataItemClass(AJob.Data.ClassType));
    FJobClipBoard.Assign(AJob);
    UpdateControls();
  end;
end;

procedure TJobsMainFrame.DoPasteJob(Sender: TObject);
var
  Node, NewNode: TTreeNode;
  AItem: TJobItem;
begin
  if (FJobClipBoard = nil) then Exit;
  Node := JobsList.Selected;
  if (Node <> nil) then
  begin
    AItem := TJobItem(Node.Data);
  end else
  begin
    AItem := nil;
  end;
  AItem := FJobManager.AddJobItem(AItem, 'Copy ' + FJobClipBoard.JobName,
    TJobDataItemClass(FJobClipBoard.Data.ClassType));
  if (AItem <> nil) then
  begin
    AItem.Assign(FJobClipBoard);
    NewNode := JobsList.Items.AddObject(Node, AItem.JobName, AItem);
    NewNode.ImageIndex := Integer(AItem.FlowAction);
    NewNode.SelectedIndex := Integer(AItem.FlowAction);
    NewNode.Selected := True;
    InsertTreeItem(NewNode, AItem);
    FJobManager.EditJobItem(TJobItem(NewNode.Data));
    IsModified := True;
  end;
end;

procedure TJobsMainFrame.JobsListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Node: TTreeNode;
begin
  if (Key = VK_F2) then
  begin
    Node := JobsList.Selected;
    if (Node <> nil) then
    begin
      Node.EditText();
      Key := 0;
    end;
  end else
  if (Key = VK_RETURN) then
  begin
    DoEditJobItem(Sender);
  end;
end;

procedure TJobsMainFrame.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TRunJobfrm) then
  begin
    FJobManager.OnStartAction := nil;
    FJobManager.OnFinishAction := nil;
    FJobManager.OnItemProcessedAction := nil;
    FJobManager.OnItemPerformedAction := nil;
  end; 
end;

procedure TJobsMainFrame.LoadMedia(AFileName: String);
  function DoLoadAsXML: Boolean;
  var
    Doc: IXMLDOMDocument;
    MainNode: IXMLDOMNode;
  begin
    Doc := CoDOMDocument.Create();
    Doc.load(FMediaFileName);
    Result := (Doc.xml <> '');
    if not Result then Exit;
    MainNode := Doc.selectSingleNode('Main');
    if MainNode = nil then
    begin
      raise Exception.CreateFmt(cUnknownImportMediaFile, [FMediaFileName]);
    end;
    FIsLoading := True;
    try
      JobsList.Items.Clear();
      FJobManager.Load(MainNode);
      LoadTreeList();
    finally
      FIsLoading := False;
    end;
  end;

  procedure DoLoadAsStream();
  var
    Position: Integer;
    Stream: TStream;
    ADataItemClass: TJobDataItemClass;
    R: TReader;
  begin
    Stream := TFileStream.Create(FMediaFileName, fmOpenRead);
    FIsLoading := True;
    try
      Position := Stream.Position;
      R := TReader.Create(Stream, 1024);
      try
        try
          ADataItemClass := TJobDataItemClass(GetClass(R.ReadString()));
        except
          on EReadError do ADataItemClass := nil;
        end;
      finally
        R.Free();
      end;
      if (ADataItemClass <> nil) then
      begin
        raise Exception.CreateFmt(cUnknownImportMediaFile, [FMediaFileName]);
      end;
      Stream.Position := Position;
      JobsList.Items.Clear();
      FJobManager.Load(Stream);
      LoadTreeList();
    finally
      FIsLoading := False;
      Stream.Free();
    end;
  end;
  
var
  NeedModify: Boolean;
begin
  NeedModify := False;
  FMediaFileName := AFileName;
  if not DoLoadAsXML() then
  begin
    DoLoadAsStream();
    NeedModify := True;
  end;
  ClearControls();
  IsModified := NeedModify;
  TJobDskMediaStorage.Instance.SetCurrentItem(FMediaFileName);
  UpdateControls();
end;

procedure TJobsMainFrame.ClearControls;
begin
  FIsChanging := True;
  try
    MemoDescription.Lines.Clear();
    edtFlowAction.Text := '';
  finally
    FIsChanging := False;
  end;
end;

procedure TJobsMainFrame.LoadDesktop(ANode: IXMLDOMNode);
var
  Store: TStoreFormStruct;
  ChildNode: IXMLDOMNode;
begin
  ChildNode := ANode.selectSingleNode('InspectorForm');
  if (ChildNode <> nil) then
  begin
    XMLToForm(Store, ChildNode);
    Self.Parent.Width := Store.Width;
  end;
  ChildNode := ANode.selectSingleNode('Projects');
  if (ChildNode <> nil) then
  begin
    TJobDskMediaStorage.Instance.Load(ChildNode);
  end;
end;

procedure TJobsMainFrame.SaveDesktop(ANode: IXMLDOMNode);
var
  Store: TStoreFormStruct;
  ChildNode: IXMLDOMNode;
begin
  ChildNode := ANode.ownerDocument.createElement('InspectorForm');
  ANode.appendChild(ChildNode);
  Store.Width := Self.Parent.Width;
  FormToXML(Store, ChildNode);
  ChildNode := ANode.ownerDocument.createElement('Projects');
  ANode.appendChild(ChildNode);
  TJobDskMediaStorage.Instance.Store(ChildNode);
end;

procedure TJobsMainFrame.DoLoadLastMedia(Params: TJobOperationParams; var Success: Boolean);
var
  s: string;
begin
  if (Params = nil) then Exit;
  if not CanCloseForm() then Exit;
  s := VarToStr(Params.ParamByName(gopLoadLastMediaFileName).Value);
  try
    LoadMedia(s);
    Success := True;
  except
    NewMedia();
  end;
end;

procedure TJobsMainFrame.JobsListContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  JobsList.PopupMenu.Items.Clear();

  FOperationList.AddOperationToPopup(JobsList.PopupMenu, opEnableJob, True);
  FOperationList.AddOperationToPopup(JobsList.PopupMenu, opDisableJob);

  FOperationList.AddOperationToPopup(JobsList.PopupMenu, opStartJobAt, True);
  FOperationList.AddOperationToPopup(JobsList.PopupMenu, opStopSelectedJob);

  FOperationList.AddOperationToPopup(JobsList.PopupMenu, opAddJobItem, True);
  FOperationList.AddOperationToPopup(JobsList.PopupMenu, opAddJobSubItem);
  FOperationList.AddOperationToPopup(JobsList.PopupMenu, opEditJobItem);
  FOperationList.AddOperationToPopup(JobsList.PopupMenu, opDeleteJobItem);

  FOperationList.AddOperationToPopup(JobsList.PopupMenu, opCopyJob, True);
  FOperationList.AddOperationToPopup(JobsList.PopupMenu, opPasteJob);
end;

procedure TJobsMainFrame.DoShowReferences(Sender: TObject);
begin
  if (FReferencesForm = nil) then
  begin
    FReferencesForm := TJobsReferencesFrame.Create(nil);
  end;
  FReferencesForm.ShowReferences(FOperationList, FJobManager);
end;

procedure TJobsMainFrame.UpdateTreeList();
var
  i: Integer;
  Node: TTreeNode;
  Item: TJobItem;
begin
  for i := 0 to JobsList.Items.Count - 1 do
  begin
    Node := JobsList.Items[i];
    Item := TJobItem(Node.Data);
    Node.ImageIndex := Integer(Item.FlowAction);
    Node.SelectedIndex := Integer(Item.FlowAction);
  end;
end;

end.
