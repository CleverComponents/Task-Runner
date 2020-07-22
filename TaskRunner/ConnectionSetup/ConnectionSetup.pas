unit ConnectionSetup;

interface

uses
  Windows, Messages, System.SysUtils, System.Variants, System.Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ComCtrls, JobUtils, CustomModalDlg, JobConsts;

type
  TConnectionSetupForm = class(TCustomModalDialog)
    edtLogin: TEdit;
    edtPassword: TEdit;
    edtServer: TEdit;
    edtDatabase: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    edtTimeOut: TEdit;
    Label5: TLabel;
    edtOLEDB: TEdit;
    chkUseOLEDB: TCheckBox;
    btnBuildOLEDB: TButton;
    procedure edtDatabaseDropDown(Sender: TObject);
    procedure edtServerChange(Sender: TObject);
    procedure edtPasswordChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkUseOLEDBClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnBuildOLEDBClick(Sender: TObject);
  private
    FSection: Boolean;
    FReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure GetSQLDatabasesList(AServer, ALogin, APassword: String; List: TStrings);
    procedure UpdateControls;
  protected
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

function ShowConnectionSetup(Info: TSQLConnectionInfo; AReadOnly: Boolean;
  var APerformWith: TSQLPerformWith): Boolean;

implementation

{$R *.DFM}

uses
  ADODB, JobCtrls;

function ShowConnectionSetup(Info: TSQLConnectionInfo; AReadOnly: Boolean;
  var APerformWith: TSQLPerformWith): Boolean;
var
  Dlg: TConnectionSetupForm;
begin
  Dlg := TConnectionSetupForm.Create(nil);
  try
    Dlg.edtLogin.Text := Info.User;
    Dlg.edtPassword.Text := Info.Password;
    Dlg.edtServer.Text := Info.Server;
    Dlg.edtDatabase.Text := Info.Database;
    Dlg.edtTimeOut.Text := Info.TimeOut;
    Dlg.edtOLEDB.Text := Info.ConnectionString;
    Dlg.chkUseOLEDB.Checked := (APerformWith = spADOLibrary);
    Dlg.ReadOnly := AReadOnly;

    Result := (Dlg.ShowModal() = mrOK) and (not AReadOnly);

    if Result then
    begin
      Info.User := Dlg.edtLogin.Text;
      Info.Password := Dlg.edtPassword.Text;
      Info.Server := Dlg.edtServer.Text;
      Info.Database := Dlg.edtDatabase.Text;
      Info.TimeOut := Dlg.edtTimeOut.Text;
      Info.ConnectionString := Dlg.edtOLEDB.Text;
      if Dlg.chkUseOLEDB.Checked then
      begin
        APerformWith := spADOLibrary;
      end else
      begin
        APerformWith := spOSQLUtilite;
      end;
    end;
  finally
    Dlg.Free();
  end;
end;

{ TConnectionSetupForm }

procedure TConnectionSetupForm.GetSQLDatabasesList(AServer, ALogin,
  APassword: String; List: TStrings);
var
  Connection: TADOConnection;
  RS: _RecordSet;
  Info: TSQLConnectionInfo;
begin
  List.Clear();
  Connection := TADOConnection.Create(nil);
  try
    Connection.LoginPrompt := False;
    Info := TSQLConnectionInfo.Create();
    try
      Info.User := ALogin;
      Info.Password := APassword;
      Info.Database := 'master';
      Info.Server := AServer;
      Connection.ConnectionString := Info.CreateConnectionString();
    finally
      Info.Free();
    end;
    Connection.Connected := True;
    RS := Connection.Execute('select name from sysdatabases order by name');

    RS.MoveFirst();
    while (not RS.EOF) do
    begin
      List.Add(VarToStr(RS.Fields['name'].Value));
      RS.MoveNext();
    end;
  finally
    RS := nil;
    Connection.Free();
  end;
end;

procedure TConnectionSetupForm.edtDatabaseDropDown(Sender: TObject);
begin
  if (edtDatabase.Items.Count > 0) then Exit;

  try
    GetSQLDatabasesList(edtServer.Text, edtLogin.Text, edtPassword.Text, edtDatabase.Items);
  except
  end;
end;

procedure TConnectionSetupForm.edtServerChange(Sender: TObject);
begin
  edtDatabase.Text := '';
  edtDatabase.Items.Clear();
end;

procedure TConnectionSetupForm.SetReadOnly(const Value: Boolean);
begin
  if (FReadOnly <> Value) then
  begin
    FReadOnly := Value;
    UpdateControls();
  end;
end;

procedure TConnectionSetupForm.edtPasswordChange(Sender: TObject);
var
  s: string;
  FStart, FEnd: integer;
begin
  if FSection then Exit;
  FSection := True;
  try
    s := edtPassword.Text;
    SendMessage(edtPassword.Handle, EM_GETSEL, WPARAM(@FStart), LPARAM(@FEnd));
    if (s <> '') and (s[1] = ':') then
    begin
      edtPassword.PasswordChar := #0;
    end else
    begin
      edtPassword.PasswordChar := '*';
    end;
    SendMessage(edtPassword.Handle, EM_SETSEL, WPARAM(WORD(FStart)), LPARAM(WORD(FEnd)));
  finally
    FSection := False;
  end;
end;

procedure TConnectionSetupForm.FormCreate(Sender: TObject);
begin
  inherited;
  FSection := False;
end;

procedure TConnectionSetupForm.UpdateControls;
var
  IsOLEDB: Boolean;
begin
  IsOLEDB := chkUseOLEDB.Checked;
  chkUseOLEDB.Enabled := not FReadOnly;

  EnableControl(edtLogin, not IsOLEDB);
  EnableControl(edtPassword, not IsOLEDB);
  EnableControl(edtServer, not IsOLEDB);
  EnableControl(edtDatabase, not IsOLEDB);
  EnableControl(edtTimeOut, not IsOLEDB);
  EnableControl(edtOLEDB, IsOLEDB);
  btnBuildOLEDB.Enabled := IsOLEDB;
  if IsOLEDB then
  begin
    edtOLEDB.ReadOnly := FReadOnly;
    btnBuildOLEDB.Enabled := not FReadOnly;
  end else
  begin
    edtLogin.ReadOnly := FReadOnly;
    edtPassword.ReadOnly := FReadOnly;
    edtServer.ReadOnly := FReadOnly;
    edtDatabase.Enabled := not FReadOnly;
    edtTimeOut.ReadOnly := FReadOnly;
  end;
end;

procedure TConnectionSetupForm.chkUseOLEDBClick(Sender: TObject);
begin
  UpdateControls();
end;

procedure TConnectionSetupForm.FormShow(Sender: TObject);
begin
  inherited;
  UpdateControls();
end;

procedure TConnectionSetupForm.btnBuildOLEDBClick(Sender: TObject);
begin
  edtOLEDB.Text := PromptDataSource(Handle, edtOLEDB.Text);
end;

end.
