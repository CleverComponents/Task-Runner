unit CustomScriptDialog;

interface

uses
  System.UITypes, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ToolWin, ComCtrls, CustomDialog, JobConsts, JobClasses,
  JobCtrls;

type
  TCustomScriptJobItemForm = class(TCustomDialogForm)
    memoScript: TJobRichEdit;
    Label2: TLabel;
    edtScriptFile: TEdit;
    edtLogFile: TEdit;
    Label3: TLabel;
    btnErrorWords: TButton;
    chkScriptFile: TCheckBox;
    chkLogFile: TCheckBox;
    sbScript: TStatusBar;
    procedure memoScriptChange(Sender: TObject);
    procedure btnErrorWordsClick(Sender: TObject);
    procedure CheckBoxChange(Sender: TObject);
    procedure EditFileChange(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure memoScriptSelectionChange(Sender: TObject);
  private
    FErrorWords: TStrings;
    FErrorBindType: TJobListBindType;
    FScriptFileChanged: Boolean;
    procedure LoadIfNeedScript;
  protected
    procedure DoChangeErrorWords; virtual;
    procedure AssignData(IsFromDataItem: Boolean = False); override;
    procedure UpdateControls; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.DFM}

uses
  CustomJobItems, ItemListView;

{ TSQLScriptJobItemForm }

procedure TCustomScriptJobItemForm.AssignData(IsFromDataItem: Boolean);
var
  IsScriptFileChecked, IsLogFileChecked: Boolean;
  ScriptFileName, LogFileName, S: String;
  ScriptData: TCustomScriptJobDataItem;
begin
  inherited AssignData(IsFromDataItem);
  ScriptData := Data as TCustomScriptJobDataItem;

  if IsFromDataItem then
  begin
    IsScriptFileChecked := ScriptData.IsUseScriptFile;
    IsLogFileChecked := ScriptData.IsUseLogFile;
    ScriptFileName := ScriptData.ScriptFile;
    LogFileName := ScriptData.LogFile;

    edtScriptFile.Text := ScriptFileName;
    edtLogFile.Text := LogFileName;
    chkScriptFile.Checked := IsScriptFileChecked;
    chkLogFile.Checked := IsLogFileChecked;
    FErrorWords.Assign(ScriptData.ErrorWords);
    FErrorBindType := ScriptData.ErrorBindType;

    if IsScriptFileChecked then
    begin
      if FileExists(ScriptFileName) then
      begin
        memoScript.Lines.LoadFromFile(ScriptFileName);
      end else
      begin
        memoScript.Lines.Clear();
        if not ScriptData.IsValueParameter(ScriptFileName, S) then
        begin
          MessageDlg(Format(cFileNotExists, [ScriptFileName]), mtWarning, [mbOK], 0);
        end;
      end;
    end else
    begin
      memoScript.Lines.Assign(ScriptData.Script);
    end;
  end else
  begin
    LoadIfNeedScript();
    IsScriptFileChecked := chkScriptFile.Checked;
    IsLogFileChecked := chkLogFile.Checked;
    ScriptFileName := Trim(edtScriptFile.Text);
    LogFileName := Trim(edtLogFile.Text);

    if IsScriptFileChecked and (ScriptFileName = '') then
    begin
      raise Exception.CreateFmt(cFileNameEmpty, ['Script ']);
    end;
    if IsLogFileChecked and (LogFileName = '') then
    begin
      raise Exception.CreateFmt(cFileNameEmpty, ['Log ']);
    end;

    if IsScriptFileChecked then
    begin
      if not ScriptData.IsValueParameter(ScriptFileName, S) then
      begin
        memoScript.Lines.SaveToFile(ScriptFileName);
      end;
    end else
    begin
      ScriptData.Script.Assign(memoScript.Lines);
    end;

    ScriptData.ScriptFile := ScriptFileName;
    ScriptData.LogFile := LogFileName;
    ScriptData.IsUseScriptFile := IsScriptFileChecked;
    ScriptData.IsUseLogFile := IsLogFileChecked;
    ScriptData.ErrorWords.Assign(FErrorWords);
    ScriptData.ErrorBindType := FErrorBindType;
  end;
end;

procedure TCustomScriptJobItemForm.LoadIfNeedScript;
begin
  if FScriptFileChanged and chkScriptFile.Checked and FileExists(edtScriptFile.Text) then
  begin
    memoScript.Lines.LoadFromFile(edtScriptFile.Text);
  end;
  FScriptFileChanged := False;
end;

procedure TCustomScriptJobItemForm.memoScriptChange(Sender: TObject);
begin
  if IsLoading then Exit;
  IsModified := True;
end;

procedure TCustomScriptJobItemForm.DoChangeErrorWords();
begin
  if ShowItemList(FErrorWords, FErrorBindType, ReadOnly) then
  begin
    IsModified := True;
  end;
end;

procedure TCustomScriptJobItemForm.btnErrorWordsClick(Sender: TObject);
begin
  DoChangeErrorWords();
end;

constructor TCustomScriptJobItemForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FErrorWords := TStringList.Create();
  FScriptFileChanged := False;
end;

destructor TCustomScriptJobItemForm.Destroy;
begin
  FErrorWords.Free();
  inherited Destroy();
end;

procedure TCustomScriptJobItemForm.CheckBoxChange(Sender: TObject);
begin
  if IsLoading then Exit;
  IsModified := True;
  UpdateControls();
end;

procedure TCustomScriptJobItemForm.EditFileChange(Sender: TObject);
begin
  if IsLoading then Exit;
  IsModified := True;
  FScriptFileChanged := True;
end;

procedure TCustomScriptJobItemForm.UpdateControls;
begin
  inherited UpdateControls();
  memoScript.ReadOnly := ReadOnly;
  EnableControl(edtScriptFile, (not ReadOnly) and chkScriptFile.Checked);
  EnableControl(edtLogFile, (not ReadOnly) and chkLogFile.Checked);
  chkScriptFile.Enabled := not ReadOnly;
  chkLogFile.Enabled := not ReadOnly;
end;

procedure TCustomScriptJobItemForm.PageControlChange(Sender: TObject);
begin
  inherited;
  if (PageControl.ActivePage = tabDetails) then
  begin
    LoadIfNeedScript();
  end;
end;

procedure TCustomScriptJobItemForm.memoScriptSelectionChange(Sender: TObject);
begin
  inherited;
  sbScript.Panels[0].Text :=
    Format(cCursorPositionMask, [IntToStr(memoScript.CaretPos.y + 1), IntToStr(memoScript.CaretPos.x + 1)]);
end;

end.
