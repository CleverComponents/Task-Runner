unit TabEditors;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Menus, System.UITypes,
  System.Generics.Collections, JobClasses, OperationUtils, System.ImageList,
  Vcl.ImgList;

type
  TTabEditorsManager = class;

  TTabEditorsFrame = class(TFrame)
    PageControl: TPageControl;
    PopupMenu: TPopupMenu;
    Close1: TMenuItem;
    imgTabs: TImageList;
    procedure PageControlContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure Close1Click(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    FTabManager: TTabEditorsManager;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TabManager: TTabEditorsManager read FTabManager;
  end;

  TTabItem = class
  private
    FTab: TTabSheet;
    FEditor: TJobEditorItem;
  public
    property Tab: TTabSheet read FTab write FTab;
    property Editor: TJobEditorItem read FEditor write FEditor;
  end;

  TTabEditorsManager = class
  private
    FTabEditorsFrame: TTabEditorsFrame;
    FTabItems: TObjectList<TTabItem>;

    function FindTabItem(AEditor: TJobEditorItem): TTabItem; overload;
    function FindTabItem(ATab: TTabSheet): TTabItem; overload;
  protected
    procedure ActivateEditor;
  public
    constructor Create(ATabEditorsFrame: TTabEditorsFrame);
    destructor Destroy; override;

    procedure AddEditor(AEditor: TJobEditorItem; AEditorControl: TWinControl);
    procedure RemoveEditor(AEditor: TJobEditorItem);
    procedure UpdateEditor(AEditor: TJobEditorItem; AIsModified: Boolean);
  end;

implementation

{$R *.dfm}

const
  ModifiedImages: array[Boolean] of Integer = (-1, 0);

{ TTabEditorsFrame }

procedure TTabEditorsFrame.Close1Click(Sender: TObject);
begin
  if (TJobOperationManager.Instance.CurrentOperationList <> nil) then
  begin
    TJobOperationManager.Instance.CurrentOperationList.PerformOperation(opCloseJob, Sender);
  end;
end;

constructor TTabEditorsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTabManager := TTabEditorsManager.Create(Self);
end;

destructor TTabEditorsFrame.Destroy;
begin
  FTabManager.Free();
  inherited Destroy();
end;

procedure TTabEditorsFrame.PageControlChange(Sender: TObject);
begin
  TabManager.ActivateEditor();
end;

procedure TTabEditorsFrame.PageControlContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  HitTests: THitTests;
  Point: TPoint;
  ind: Integer;
begin
  ind := PageControl.IndexOfTabAt(MousePos.X, MousePos.Y);
  if (ind > -1) then
  begin
    PageControl.ActivePageIndex := ind;
    TabManager.ActivateEditor();
  end;

  HitTests := PageControl.GetHitTestInfoAt(MousePos.X, MousePos.Y);
  Handled := (htOnLabel in HitTests);
  if Handled then
  begin
    Point := ClientToScreen(MousePos);
    PopupMenu.Popup(Point.X, Point.Y);
  end;
end;

{ TTabEditorsManager }

procedure TTabEditorsManager.ActivateEditor;
var
  ti: TTabItem;
begin
  ti := FindTabItem(FTabEditorsFrame.PageControl.ActivePage);
  if (ti <> nil) then
  begin
    ti.Editor.Perform();
  end;
end;

procedure TTabEditorsManager.AddEditor(AEditor: TJobEditorItem; AEditorControl: TWinControl);
var
  ti: TTabItem;
  ts: TTabSheet;
begin
  ti := FindTabItem(AEditor);
  if (ti = nil) then
  begin
    ti := TTabItem.Create();
    FTabItems.Add(ti);

    ts := TTabSheet.Create(FTabEditorsFrame);
    ts.PageControl := FTabEditorsFrame.PageControl;
    ts.Caption := AEditor.Data.JobName;
    ts.ImageIndex := -1;

    ti.Editor := AEditor;
    ti.Tab := ts;

    AEditorControl.Parent := ts;
    AEditorControl.Align := alClient;
  end;
  FTabEditorsFrame.PageControl.ActivePage := ti.Tab;
end;

constructor TTabEditorsManager.Create(ATabEditorsFrame: TTabEditorsFrame);
begin
  inherited Create();

  FTabItems := TObjectList<TTabItem>.Create();
  FTabEditorsFrame := ATabEditorsFrame;
end;

destructor TTabEditorsManager.Destroy;
begin
  FTabItems.Free();
  inherited Destroy();
end;

function TTabEditorsManager.FindTabItem(ATab: TTabSheet): TTabItem;
var
  i: Integer;
begin
  for i := 0 to FTabItems.Count - 1 do
  begin
    Result := FTabItems[i];
    if (Result.Tab = ATab) then
    begin
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TTabEditorsManager.RemoveEditor(AEditor: TJobEditorItem);
var
  ti: TTabItem;
begin
  ti := FindTabItem(AEditor);
  if (ti <> nil) then
  begin
    ti.Tab.Free();
    FTabItems.Remove(ti);
    ActivateEditor();
  end;
end;

procedure TTabEditorsManager.UpdateEditor(AEditor: TJobEditorItem; AIsModified: Boolean);
var
  ti: TTabItem;
begin
  ti := FindTabItem(AEditor);
  if (ti <> nil) then
  begin
    ti.Tab.ImageIndex := ModifiedImages[AIsModified];
    ti.Tab.Caption := AEditor.Data.JobName;
  end;
end;

function TTabEditorsManager.FindTabItem(AEditor: TJobEditorItem): TTabItem;
var
  i: Integer;
begin
  for i := 0 to FTabItems.Count - 1 do
  begin
    Result := FTabItems[i];
    if (Result.Editor = AEditor) then
    begin
      Exit;
    end;
  end;
  Result := nil;
end;

end.
