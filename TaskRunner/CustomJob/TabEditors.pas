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
    Save1: TMenuItem;
  private
    FTabEditorsManager: TTabEditorsManager;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Manager: TTabEditorsManager read FTabEditorsManager;
  end;

  TTabEditorsManager = class
  private
    FFrame: TTabEditorsFrame;
  public
    constructor Create(AFrame: TTabEditorsFrame);

    procedure AddTab(AEditor: TJobEditorItem);
  end;

implementation

{$R *.dfm}

{ TTabEditorsFrame }

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

{ TTabEditorsManager }

procedure TTabEditorsManager.AddTab(AEditor: TJobEditorItem);
var
  ts: TTabSheet;
begin
  ts := TTabSheet.Create(FFrame);
  ts.PageControl := FFrame.PageControl;
  ts.Caption := AEditor.Data.JobName;
end;

constructor TTabEditorsManager.Create(AFrame: TTabEditorsFrame);
begin
  inherited Create();
  FFrame := AFrame;
end;

end.
