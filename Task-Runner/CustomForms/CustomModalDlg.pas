unit CustomModalDlg;

interface

uses
  Windows, Classes, Controls, Forms;

type
  TCustomModalDialog = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

{$R *.dfm}

{ TCustomModalDialog }

procedure TCustomModalDialog.CreateParams(var Params: TCreateParams);
var
  hwnd: THandle;
begin
  inherited CreateParams(Params);
  if (Screen.ActiveCustomForm <> nil) then
  begin
    hwnd := Screen.ActiveCustomForm.Handle;
  end else
  begin
    hwnd := 0;
  end;
  Params.WndParent := hwnd;
end;

end.