unit AboutForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CustomModalDlg, StdCtrls, ExtCtrls;

type
  TAboutDialog = class(TCustomModalDialog)
    Image1: TImage;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  end;

procedure ShowAbout;

implementation

{$R *.DFM}

procedure ShowAbout;
var
  Dlg: TAboutDialog;
begin
  Dlg := TAboutDialog.Create(nil);
  try
    Dlg.ShowModal();
  finally
    Dlg.Free();
  end;
end;

procedure TAboutDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close();
end;

procedure TAboutDialog.Image1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Close();
end;

procedure TAboutDialog.Image1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  //
end;

end.
