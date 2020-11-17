unit TabEditors;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Menus, System.UITypes,
  System.Generics.Collections, JobClasses;

type
  TTabEditorsManager = class;

  TTabEditorsFrame = class(TFrame)
    PageControl: TPageControl;
    PopupMenu: TPopupMenu;
    Close1: TMenuItem;
    procedure PageControlContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure Close1Click(Sender: TObject);
  private
    FTabEditorsManager: TTabEditorsManager;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Manager: TTabEditorsManager read FTabEditorsManager;
  end;

  TTabItem = class
  private
    FTabSheet: TTabSheet;
    FEditor: TJobEditorItem;
  public
    property TabSheet: TTabSheet read FTabSheet write FTabSheet;
    property Editor: TJobEditorItem read FEditor write FEditor;
  end;

  TTabEditorsManager = class
  private
    FTabEditorsFrame: TTabEditorsFrame;
    FTabItems: TObjectList<TTabItem>;

    function FindTabItem(AEditor: TJobEditorItem): TTabItem;
  public
    constructor Create(ATabEditorsFrame: TTabEditorsFrame);
    destructor Destroy; override;

    procedure AddEditor(AEditor: TJobEditorItem; AEditorControl: TWinControl);
  end;

implementation

{$R *.dfm}

{ TTabEditorsFrame }

procedure TTabEditorsFrame.Close1Click(Sender: TObject);
begin
  //TODO
end;

constructor TTabEditorsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTabEditorsManager := TTabEditorsManager.Create(Self);
end;

destructor TTabEditorsFrame.Destroy;
begin
  FTabEditorsManager.Free();
  inherited Destroy();
end;

procedure TTabEditorsFrame.PageControlContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  HitTests: THitTests;
  Point: TPoint;
begin
  HitTests := PageControl.GetHitTestInfoAt(MousePos.X, MousePos.Y);
  Handled := (htOnLabel in HitTests);
  if Handled then
  begin
    Point := ClientToScreen(MousePos);
    PopupMenu.Popup(Point.X, Point.Y);
  end;
end;

{ TTabEditorsManager }

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

    ti.Editor := AEditor;
    ti.TabSheet := ts;

    AEditorControl.Parent := ts;
    AEditorControl.Align := alClient;
  end;
  FTabEditorsFrame.PageControl.ActivePage := ti.TabSheet;
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
