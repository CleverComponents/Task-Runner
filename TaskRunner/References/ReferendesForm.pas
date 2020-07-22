unit ReferendesForm;

interface

uses
  ComCtrls, Forms, OperationUtils, Menus, Classes, Controls, Dialogs, ExtCtrls, JobCtrls,
  ImgList, OperationClasses, JobClasses, System.ImageList;

type
  TJobsReferencesFrame = class(TForm)
    pCenter: TPanel;
    odMediaFile: TOpenDialog;
    ingJobFlowAction: TImageList;
    ReferencesList: TJobTreeView;
    JobPopupMenu: TPopupMenu;
  private
    FOperationList: TFormOperationList;
    FJobManager: TJobManager;
    procedure AddOperations;
    procedure FillList;
    procedure UpdateControls;
    procedure InsertJobs(ANode: TTreeNode; AProject: TJobOperationParam);
  public
    procedure ShowReferences(AOperationList: TFormOperationList; AJobManager: TJobManager);
  end;

implementation

{$R *.DFM}

const
  cProjectImage = 0;
  cJobImage = 1;

{ TJobsReferencesFrame }

procedure TJobsReferencesFrame.ShowReferences(AOperationList: TFormOperationList;
  AJobManager: TJobManager);
begin
  FOperationList := AOperationList;
  FJobManager := AJobManager;
  AddOperations();
  FillList();
  UpdateControls();
  Show();
end;

procedure TJobsReferencesFrame.AddOperations;
begin
  //TODO
end;

procedure TJobsReferencesFrame.FillList;
var
  i: Integer;
  Param: TJobOperationParam;
  Node: TTreeNode;
begin
  ReferencesList.Items.BeginUpdate();
  try
    ReferencesList.Items.Clear();
    Node := nil;
    for i := 0 to FJobManager.References.Count - 1 do
    begin
      Param := FJobManager.References[i];
      Node := ReferencesList.Items.AddObject(Node, Param.Name, Param);
      Node.ImageIndex := cProjectImage;
      Node.SelectedIndex := cProjectImage;
      InsertJobs(Node, Param);
    end;
  finally
    ReferencesList.Items.EndUpdate();
  end;
end;

procedure TJobsReferencesFrame.InsertJobs(ANode: TTreeNode; AProject: TJobOperationParam);
begin
  //TODO
end;

procedure TJobsReferencesFrame.UpdateControls;
begin
  //TODO
end;

end.
