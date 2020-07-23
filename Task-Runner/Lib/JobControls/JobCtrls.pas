unit JobCtrls;

interface

uses
  Vcl.StdCtrls, Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Controls, Vcl.ComCtrls, Vcl.Dialogs;

type
  TJobComboBox = class(TComboBox)
  end;

  TJobRichEdit = class(TRichEdit)
  private
    FFindDialog: TFindDialog;
    FReplaceDialog: TReplaceDialog;

    procedure FindOne(Sender: TObject);
    procedure ReplaceOne(Sender: TObject);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FindDialog;
    procedure ReplaceDialog;
  end;

  TJobTreeView = class(TTreeView)
  end;

procedure EnableControl(AControl: TWinControl; IsEnable: Boolean);

const
  cRichEditTextNotFound = 'The search text is not found.';
  cRichEditReplaceAllResult = 'Replaced %d occurances.';
  cRichEditFoundResultCaption = 'Information';

implementation

uses
  Vcl.Forms, System.SysUtils, Vcl.Graphics;

type
  TD = class(TWinControl);

procedure EnableControl(AControl: TWinControl; IsEnable: Boolean);
begin
  AControl.Enabled := IsEnable;
  if IsEnable then
  begin
    TD(AControl).Color := clWindow;
  end else
  begin
    TD(AControl).Color := clBtnFace;
  end;
end;

{ TJobRichEdit }

constructor TJobRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFindDialog := nil;
  FReplaceDialog := nil;
end;

destructor TJobRichEdit.Destroy;
begin
  if (FFindDialog <> nil) then
  begin
    FFindDialog.Free();
  end;
  if (FReplaceDialog <> nil) then
  begin
    FReplaceDialog.Free();
  end;
  inherited Destroy();
end;

procedure TJobRichEdit.FindDialog;
begin
  SelLength := 0;
  SendMessage(Self.Handle, EM_SCROLLCARET, 0, 0);
  if (FFindDialog = nil) then
  begin
    FFindDialog := TFindDialog.Create(nil);
    FFindDialog.Options := FFindDialog.Options + [frDisableUpDown];
  end;
  FFindDialog.OnFind := FindOne;
  FFindDialog.Execute();
end;

procedure TJobRichEdit.FindOne(Sender: TObject);
var
  StartPos, FindLength, FoundAt: Integer;
  Flags: TSearchTypes;
  P: TPoint;
  CaretR, R, IntersectR: TRect;
  hwnd: THandle;
begin
  with TFindDialog(Sender) do
  begin
    if frDown in Options then
    begin
      if SelLength = 0 then StartPos := SelStart
      else StartPos := SelStart + SelLength;
      FindLength := Length(Text) - StartPos;
    end else
    begin
      StartPos := SelStart;
      FindLength := -StartPos;
    end;
    Flags := [];
    if frMatchCase in Options then Include(Flags, stMatchCase);
    if frWholeWord in Options then Include(Flags, stWholeWord);
    Screen.Cursor := crHourglass;
    FoundAt := Self.FindText(FindText, StartPos, FindLength, Flags);
    if not (frReplaceAll in Options) then
    begin
      Screen.Cursor := crDefault;
    end;
    if FoundAt > -1 then
    begin
      if frReplaceAll in Options then
      begin
        SelStart := FoundAt;
        SelLength := Length(FindText);
        SendMessage(Self.Handle, EM_SCROLLCARET, 0, 0);
      end else
      begin
        SetFocus();
        SelStart := FoundAt;
        SelLength := Length(FindText);
        SendMessage(Self.Handle, EM_SCROLLCARET, 0, 0);

        Winapi.Windows.GetCaretPos(P);
        P := ClientToScreen(P);
        CaretR := Rect(P.X, P.Y, P.X + 2, P.Y + 20);
        GetWindowRect(Handle, R);
        if IntersectRect(IntersectR, CaretR, R) then
        begin
          if P.Y < Screen.Height div 2 then
          begin
            Top := P.Y + 40;
          end else
          begin
            Top := P.Y - (R.Bottom - R.Top + 20);
          end;
        end;
      end;
    end else
    if not (frReplaceAll in Options) then
    begin
      if (Screen.ActiveCustomForm <> nil) then
      begin
        hwnd := Screen.ActiveCustomForm.Handle;
      end else
      begin
        hwnd := 0;
      end;
      MessageBox(hwnd, cRichEditTextNotFound, cRichEditFoundResultCaption, MB_OK);
    end;
  end;
end;

procedure TJobRichEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (ssCtrl in	Shift) and (Key = Ord('F')) then
  begin
    FindDialog();
    Key := 0;
  end else
  if (not ReadOnly) and (ssCtrl in	Shift) and (Key = Ord('H')) then
  begin
    ReplaceDialog();
    Key := 0;
  end;
end;

procedure TJobRichEdit.ReplaceDialog;
begin
  SelLength := 0;
  SendMessage(Self.Handle, EM_SCROLLCARET, 0, 0);
  if (FReplaceDialog = nil) then
  begin
    FReplaceDialog := TReplaceDialog.Create(nil);
  end;
  FReplaceDialog.OnFind := FindOne;
  FReplaceDialog.OnReplace := ReplaceOne;
  FReplaceDialog.Execute();
end;

procedure TJobRichEdit.ReplaceOne(Sender: TObject);
var
  ReplacedCount, OldSelStart, PrevSelStart: Integer;
  S: String;
  hwnd: THandle;
begin
  with TReplaceDialog(Sender) do
  begin
    ReplacedCount := 0;
    OldSelStart := SelStart;
    if frReplaceAll in Options then
      Screen.Cursor := crHourglass;
    repeat
      if (SelLength > 0) and ((SelText = FindText) or
        (not (frMatchCase in Options) and
         (AnsiUpperCase(SelText) = AnsiUpperCase(FindText)))) then
      begin
        SelText := ReplaceText;
        Inc(ReplacedCount);
      end;
      PrevSelStart := SelStart;
      FindOne(Sender);
    until not (frReplaceAll in Options) or (SelStart = PrevSelStart);
    if frReplaceAll in Options then
    begin
      Screen.Cursor := crDefault;
      if ReplacedCount = 0 then
      begin
        S := cRichEditTextNotFound;
      end else
      begin
        SelStart := OldSelStart;
        S := Format(cRichEditReplaceAllResult, [ReplacedCount]);
      end;
      if (Screen.ActiveCustomForm <> nil) then
      begin
        hwnd := Screen.ActiveCustomForm.Handle;
      end else
      begin
        hwnd := 0;
      end;
      MessageBox(hwnd, PChar(S), cRichEditFoundResultCaption, MB_OK);
    end;
  end;
end;

end.
