unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ToolWin, ComCtrls, ImgList, OperationClasses, JobClasses, XMLUtils,
  System.ImageList, JobUtils;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuEdit: TMenuItem;
    mnuRun: TMenuItem;
    mnuTools: TMenuItem;
    mnuOpenMedia: TMenuItem;
    mnuSaveAsMedia: TMenuItem;
    mnuExit: TMenuItem;
    mnuLine: TMenuItem;
    mnuEditJobItem: TMenuItem;
    mnuAddJobItem: TMenuItem;
    mnuDeleteJobItem: TMenuItem;
    mnuStartJobAt: TMenuItem;
    mnuStopSelectedJob: TMenuItem;
    mnuLine1: TMenuItem;
    MainToolBar: TToolBar;
    btnOpenMedia: TToolButton;
    btnSaveMedia: TToolButton;
    btnNewMedia: TToolButton;
    imglMainBar: TImageList;
    ToolButton1: TToolButton;
    btnStarJobAt: TToolButton;
    btnStopAllJobs: TToolButton;
    mnuStopAllJobs: TMenuItem;
    mnuSaveMedia: TMenuItem;
    mnuLine2: TMenuItem;
    mnuNewMedia: TMenuItem;
    mnuAddJobSubItem: TMenuItem;
    btnAddJobItem: TToolButton;
    ToolButton3: TToolButton;
    btnEditJobItem: TToolButton;
    btnAddJobSubItem: TToolButton;
    btnDeleteJobItem: TToolButton;
    mnuCutJob: TMenuItem;
    mnuCopyJob: TMenuItem;
    mnuPasteJob: TMenuItem;
    N1: TMenuItem;
    mnuImportJob: TMenuItem;
    mnuExportJob: TMenuItem;
    mnuGlobalParameters: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    mnuJobInspector: TMenuItem;
    mnuJobReferences: TMenuItem;
    btnReferences: TToolButton;
    ToolButton4: TToolButton;
    mnuEnableJobItem: TMenuItem;
    mnuDisableJobItem: TMenuItem;
    N2: TMenuItem;
    procedure mnuExitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuGlobalParametersClick(Sender: TObject);
    procedure mnuFileClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure mnuJobInspectorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FConsoleRunLogName: string;
    FOldWindowPos: TStoreFormStruct;
    FGlobalParameters: TJobOperationParams;
    FJobForm: TForm;
    FJobDSKFileName: string;
    FJobParamsFileName: string;
    FIsConsoleErrors: Boolean;
    procedure RegisterMenuItems;
    procedure LoadDesktop(AFileName: String);
    procedure SaveDesktop(AFileName: String);
    procedure DoGetGlobalParams(var Params: TJobOperationParams);
    procedure DoOnLoadLastMedia(Sender: TObject);
    procedure BuildLastFileMenuItems(ABeforeItem: TMenuItem);
    procedure DoFinishConsoleJob(Visitor: TJobVisitor);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    function StartConsoleJob(const AJobName, ALogName: string): Boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  JobsMain, OperationUtils, JobConsts, GlobalParamsJobItemFrm, JobDskClasses, AboutForm,
  Winapi.msxml;

procedure TMainForm.RegisterMenuItems;
begin
  RegisterMenuItem(opNewMedia, mnuNewMedia, btnNewMedia);
  RegisterMenuItem(opLoadMedia, mnuOpenMedia, btnOpenMedia);
  RegisterMenuItem(opSaveMedia, mnuSaveMedia, btnSaveMedia);
  RegisterMenuItem(opSaveMediaAs, mnuSaveAsMedia);
  RegisterMenuItem(opAddJobItem, mnuAddJobItem, btnAddJobItem);
  RegisterMenuItem(opAddJobSubItem, mnuAddJobSubItem, btnAddJobSubItem);
  RegisterMenuItem(opEditJobItem, mnuEditJobItem, btnEditJobItem);
  RegisterMenuItem(opDeleteJobItem, mnuDeleteJobItem, btnDeleteJobItem);
  RegisterMenuItem(opStartJobAt, mnuStartJobAt, btnStarJobAt);
  RegisterMenuItem(opStopSelectedJob, mnuStopSelectedJob);
  RegisterMenuItem(opStopAllJobs, mnuStopAllJobs, btnStopAllJobs);
  RegisterMenuItem(opCutJob, mnuCutJob);
  RegisterMenuItem(opCopyJob, mnuCopyJob);
  RegisterMenuItem(opPasteJob, mnuPasteJob);
  RegisterMenuItem(opImportJob, mnuImportJob);
  RegisterMenuItem(opExportJob, mnuExportJob);
  RegisterMenuItem(opEnableJob, mnuEnableJobItem);
  RegisterMenuItem(opDisableJob, mnuDisableJobItem);
  RegisterMenuItem(opShowReferences, mnuJobReferences, btnReferences);
end;

procedure TMainForm.mnuExitClick(Sender: TObject);
begin
  Close();
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := TJobsMainFrame(FJobForm).CanCloseForm();
end;

constructor TMainForm.Create(Owner: TComponent);

  procedure LoadGlobalParameters(const AFileName: string);
  var
    Doc: IXMLDOMDocument;
    MainNode: IXMLDOMNode;
  begin
    FGlobalParameters.Clear();
    Doc := CoDOMDocument.Create();
    Doc.load(AFileName);
    if (Doc.xml <> '') then
    begin
      MainNode := Doc.selectSingleNode('GlobalParameters');
      if MainNode <> nil then
      begin
        FGlobalParameters.Load(MainNode);
      end;
    end else
    begin
      try
        FGlobalParameters.LoadFromFile(AFileName);
      except
      end;
    end;
  end;

begin
  inherited Create(Owner);
  FIsConsoleErrors := False;
  FGlobalParameters := TJobOperationParams.Create();

  Caption := Format('%s v.%s (%s)', [cMainFormCaption, BuildNo, {$IFDEF WIN64}'x64'{$ELSE}'x86'{$ENDIF}]);

  RegisterMenuItems();
  FJobForm := TJobsMainFrame.Create(nil);

  ClientHeight := MainToolBar.Height;
  Top := 0; Left := 0; Width := Screen.Width;
  FJobForm.Left := Left;
  FJobForm.Top := Top + Height;

  TJobsMainFrame(FJobForm).OnGetGlobalParams := DoGetGlobalParams;
  TJobOperationManager.Instance.CurrentOperationList := TJobsMainFrame(FJobForm).OperationList;
  FJobDSKFileName := ExtractFilePath(ParamStr(0));
  if (FJobDSKFileName <> '') and (FJobDSKFileName[Length(FJobDSKFileName)] <> '\') then
  begin
    FJobDSKFileName := FJobDSKFileName + '\';
  end;
  FJobParamsFileName := FJobDSKFileName + cJobGlobalParamsFileName;
  FJobDSKFileName := FJobDSKFileName + cJobDeskTopFileName;
  LoadGlobalParameters(FJobParamsFileName);
  LoadDesktop(FJobDSKFileName);
  if (ParamCount() > 0) then
  begin
    try
      TJobsMainFrame(FJobForm).LoadMedia(ParamStr(1));
    except
      TJobsMainFrame(FJobForm).NewMedia();
    end;
  end else
  begin
    TJobsMainFrame(FJobForm).NewMedia();
  end;
end;

destructor TMainForm.Destroy;
  procedure StoreGlobalParameters(const AFileName: string);
  var
    Doc: IXMLDOMDocument;
    MainNode: IXMLDOMNode;
  begin
    Doc := CoDOMDocument.Create();
    MainNode := Doc.createElement('GlobalParameters');
    Doc.appendChild(MainNode);
    FGlobalParameters.Store(MainNode);
    SaveXMLToFile(AFileName, Doc);
  end;

begin
  StoreGlobalParameters(FJobParamsFileName);
  SaveDesktop(FJobDSKFileName);
  FJobForm.Free();
  FGlobalParameters.Free();
  inherited Destroy();
end;

procedure TMainForm.SaveDesktop(AFileName: String);
var
  Store: TStoreFormStruct;
  Doc: IXMLDOMDocument;
  RootNode, MainNode: IXMLDOMNode;
begin
  Doc := CoDOMDocument.Create();
  RootNode := Doc.createElement('FrameWork');
  Doc.appendChild(RootNode);

  if (Self.WindowState = wsNormal) then
  begin
    Store.Left := Self.Left;
    Store.Top := Self.Top;
    Store.Width := Self.Width;
    Store.Height := Self.Height;
  end else
  begin
    Store := FOldWindowPos;
  end;
  Store.Maximized := (Self.WindowState = wsMaximized);

  MainNode := RootNode.ownerDocument.createElement('MainForm');
  RootNode.appendChild(MainNode);

  FormToXML(Store, MainNode);

  TJobsMainFrame(FJobForm).SaveDesktop(RootNode);
  SaveXMLToFile(AFileName, Doc);
end;

procedure TMainForm.LoadDesktop(AFileName: String);
var
  Store: TStoreFormStruct;
  Doc: IXMLDOMDocument;
  RootNode, MainNode: IXMLDOMNode;
begin
  Doc := CoDOMDocument.Create();
  Doc.load(AFileName);
  if (Doc.xml = '') then Exit;

  RootNode := Doc.selectSingleNode('FrameWork');
  if RootNode = nil then Exit;

  MainNode := RootNode.selectSingleNode('MainForm');
  if MainNode <> nil then
  begin
    XMLToForm(Store, MainNode);
    if (Store.Width <> 0) then Self.Width := Store.Width;
    if (Store.Height <> 0) then Self.Height := Store.Height;
    if (Store.Left <> 0) then Self.Left := Store.Left;
    if (Store.Top <> 0) then Self.Top := Store.Top;
    FOldWindowPos.Width := Self.Width;
    FOldWindowPos.Height := Self.Height;
    FOldWindowPos.Left := Self.Left;
    FOldWindowPos.Top := Self.Top;
    if Store.Maximized then
    begin
      Self.WindowState := wsMaximized;
    end;
  end;
  TJobsMainFrame(FJobForm).LoadDesktop(RootNode);
end;

procedure TMainForm.mnuGlobalParametersClick(Sender: TObject);
begin
  EditGlobalParameters(TJobsMainFrame(FJobForm).JobManager, FGlobalParameters);
end;

procedure TMainForm.DoGetGlobalParams(var Params: TJobOperationParams);
begin
  Params := FGlobalParameters;
end;

procedure TMainForm.mnuFileClick(Sender: TObject);
begin
  BuildLastFileMenuItems(mnuExit);
end;

procedure TMainForm.BuildLastFileMenuItems(ABeforeItem: TMenuItem);
var
  i, Ind, ShortCutID: Integer;
  ParentMenu, NewMenuItem: TMenuItem;
  DskStorage: TJobDskMediaStorage;
  DskItemStorage: TJobDskItemStorage;
  IsAdded: Boolean;
begin
  ParentMenu := ABeforeItem.Parent;
  if (ParentMenu = nil) then Exit;
  for i := ParentMenu.Count - 1 downto 0 do
  begin
    if (ParentMenu.Items[i].Tag <> 0) then
    begin
      ParentMenu.Delete(i);
    end;
  end;
  Ind := ParentMenu.IndexOf(ABeforeItem);
  if Ind < 0 then Exit;
  DskStorage := TJobDskMediaStorage.Instance;
  IsAdded := False;
  ShortCutID := 1;
  for i := 0 to DskStorage.ItemsCount - 1 do
  begin
    DskItemStorage := DskStorage.Items[i];
    if (DskItemStorage.Name <> '') then
    begin
      IsAdded := True;
      NewMenuItem := TMenuItem.Create(Self);
      ParentMenu.Insert(Ind, NewMenuItem);
      NewMenuItem.Caption := '&' + IntToStr(ShortCutID) + '. ' + DskItemStorage.Name;
      NewMenuItem.OnClick := DoOnLoadLastMedia;
      NewMenuItem.Tag := Integer(DskItemStorage);
      Inc(Ind);
      Inc(ShortCutID);
    end;
  end;
  if IsAdded then
  begin
    NewMenuItem := TMenuItem.Create(Self);
    ParentMenu.Insert(Ind, NewMenuItem);
    NewMenuItem.Caption := '-';
    NewMenuItem.Tag := 1;
  end;
end;

procedure TMainForm.DoOnLoadLastMedia(Sender: TObject);
var
  DskStorage: TJobDskItemStorage;
  Params: TJobOperationParams;
begin
  if not (Sender is TMenuItem) then Exit;
  DskStorage := TJobDskItemStorage(TMenuItem(Sender).Tag);
  if (DskStorage = nil) then Exit;
  Params := TJobOperationParams.Create();
  try
    Params.Add(gopLoadLastMediaFileName, DskStorage.Name);
    PerformGlobalOperation(gopLoadLastMedia, Params);
  finally
    Params.Free();
  end;
end;

function TMainForm.StartConsoleJob(const AJobName, ALogName: string): Boolean;
var
  JobMgr: TJobManager;
  OldEvent: TRunJobEvent;
begin
  FConsoleRunLogName := ALogName;
  JobMgr := (FJobForm as TJobsMainFrame).JobManager;
  OldEvent := JobMgr.OnFinishAction;
  JobMgr.OnFinishAction := DoFinishConsoleJob;
  JobMgr.RunJob(AJobName, False, False);
  JobMgr.OnFinishAction := OldEvent;
  Result := not FIsConsoleErrors;
end;

procedure TMainForm.DoFinishConsoleJob(Visitor: TJobVisitor);
begin
  if (FConsoleRunLogName <> '') then
  begin
    FIsConsoleErrors := (Visitor.FullErrors.Count > 0);
    if FIsConsoleErrors then
    begin
      Visitor.FullLog.Add(#13#10'Errors:');
      Visitor.FullLog.AddStrings(Visitor.FullErrors);
    end;
    Visitor.FullLog.SaveToFile(FConsoleRunLogName);
  end;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  ShowAbout();
end;

procedure TMainForm.mnuJobInspectorClick(Sender: TObject);
begin
  FJobForm.Show();
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FJobForm.Show();
end;

end.
