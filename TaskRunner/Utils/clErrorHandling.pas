{
  Copyright (C) 1999 - 2002 Clever Components
  www.CleverComponents.com
}

unit clErrorHandling;

interface

uses
  SysUtils, Forms;

type
  TclLogger = class
  private
    FPrevOnAppException: TExceptionEvent;
    procedure HandleAppException(Sender: TObject; E: Exception);
    procedure HookExceptions;
    procedure UnhookExceptions;
    function GetModuleVersionInfo: string;
    constructor CreateInstance;
    class function AccessInstance(Request: Integer): TclLogger;
    procedure PutMessageToFile(const AMessage: string);
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TclLogger;
    class procedure ReleaseInstance;
    procedure LogMessage(const AMessage: string);
  end;

implementation

uses
  Windows, VerInfo{, imagehlp};

const
  StoredCallStackDepth = 26;

type
	TCallStack = Array[0..StoredCallStackDepth] of Pointer;

var
  frame: TCallStack;
  fOldExceptObjProc: Pointer;

procedure FillCallStack(var St : TCallStack; const ExcludeFirstLevel: Boolean);
var
  i : integer;
  _EBP : Integer;
  _ESP : Integer;
begin
  asm
    mov  _ESP, esp
    mov  _EBP, ebp
  end;
  if ExcludeFirstLevel then
  begin
    _ESP:= _EBP;
    _EBP:= PInteger(_EBP)^;
  end;
  FillChar(St, SizeOf(St), 0);
  if (_EBP<_ESP) or (_EBP-_ESP>30000) then Exit;
  for i:= 0 to StoredCallStackDepth do
  begin
    _ESP:= _EBP;
    _EBP:= PInteger(_EBP)^;
    if (_EBP<_ESP) or (_EBP-_ESP>30000) then Exit;
    St[i]:= Pointer(PInteger(_EBP+4)^-4);
  end;
end;

{procedure GetCallStack(var St : TCallStack);
var
  i: Integer;
	stk: STACKFRAME;
  Cnt: _CONTEXT;
begin
  FillChar(St, SizeOf(St), 0);
	Cnt.ContextFlags := CONTEXT_CONTROL;
	GetThreadContext(GetCurrentThread(), Cnt);
	ZeroMemory(@stk, SizeOf(STACKFRAME));
	stk.AddrPC.Offset := Cnt.Eip;
	stk.AddrPC.Mode := AddrModeFlat;
	stk.AddrStack.Offset := Cnt.Esp;
	stk.AddrStack.Mode := AddrModeFlat;
	stk.AddrFrame.Offset := Cnt.Ebp;
	stk.AddrFrame.Mode := AddrModeFlat;
	for i := 0 to StoredCallStackDepth do
	begin
		if (not StackWalk(IMAGE_FILE_MACHINE_I386,	GetCurrentProcess(), GetCurrentThread(),
							@stk, @Cnt, nil,
							SymFunctionTableAccess, SymGetModuleBase, nil)) then
    begin
			Break;
    end;
    St[i]:= Pointer(stk.AddrPC.Offset);
	end;
end;}

function CallStackTextualRepresentation(const S: TCallStack; const LineHeader: string): string;
var
  i: integer;
begin
  i:= 0;
  Result:= '';
  while (i <= StoredCallStackDepth) and (S[i] <> Nil) do
  begin
    Result:= Result + LineHeader + 'call stack - ' + IntToStr(i) + ' : 0x' + IntToHex(Cardinal(S[i]), 8) + #13#10;
    i:= i + 1;
  end;
end;

procedure clUnwindStack(P: PExceptionRecord); stdcall;
var
  s: string;
begin
  s := Format('Exception Code: 0x%.8x', [P.ExceptionCode]);
  if (P.ExceptionCode = EXCEPTION_ACCESS_VIOLATION) then
  begin
    if (P.ExceptionInformation[0] = 1) then
      s := s + Format(#13#10 + 'AV at 0x%.8x, write at address 0x%.8x', [Integer(P.ExceptionAddress), Integer(P.ExceptionInformation[1])])
    else
      s := s + Format(#13#10 + 'AV at 0x%.8x, read at address 0x%.8x', [Integer(P.ExceptionAddress), Integer(P.ExceptionInformation[1])])
  end;
  FillCallStack(frame, True);
//  GetCallStack(frame);
  s := s + #13#10 + CallStackTextualRepresentation(frame, '');
  TclLogger.Instance().LogMessage('Stack Trace: ' + s);
end;

function clGetExceptionObject(P: PExceptionRecord): Exception;
asm
  pusha
  push p
  call clUnwindStack;
  popa
  mov edx, fOldExceptObjProc
  test edx, edx
  jz @@no_handler
  call edx
@@no_handler:
end;

{ TclLogger }

class function TclLogger.AccessInstance(Request: Integer): TclLogger;
const
  FInstance: TclLogger = nil;
begin
  case Request of
    0 : ;
    1 : if not Assigned(FInstance) then FInstance := CreateInstance;
    2 : FInstance := nil;
  else
    raise Exception.CreateFmt('Illegal request %d in AccessInstance',
        [Request]);
  end;
  Result := FInstance;
end;

constructor TclLogger.Create;
begin
  inherited Create;
  raise Exception.CreateFmt('Access class %s through Instance only',
      [ClassName]);
end;

constructor TclLogger.CreateInstance;
begin
  inherited Create();
end;

destructor TclLogger.Destroy;
begin
  if AccessInstance(0) = Self then AccessInstance(2);
  UnhookExceptions();
  inherited Destroy();
end;

function TclLogger.GetModuleVersionInfo: string;
var
  Info: TVersionInfo;
begin
  Info := TVersionInfo.Create(ParamStr(0));
  try
    Result := Info.FileVersion;
  finally
    Info.Free();
  end;
end;

procedure TclLogger.HandleAppException(Sender: TObject; E: Exception);
begin
  if (E is EAccessViolation) then Exit;
  LogMessage(Format('Exception (%s): %s', [E.ClassName, E.Message]));
end;

procedure TclLogger.HookExceptions;
begin
  FPrevOnAppException := Application.OnException;
  Application.OnException := HandleAppException;
end;

class function TclLogger.Instance: TclLogger;
begin
  Result := AccessInstance(1);
end;

procedure TclLogger.LogMessage(const AMessage: string);
var
  s: string;
begin
  s := GetModuleVersionInfo();
  if (s <> '') then
  begin
    s := 'File Version: ' + s + #13#10;
  end;
  s := DateTimeToStr(Now()) + ': ' + s + AMessage;
  PutMessageToFile(s + #13#10#13#10);
end;

procedure TclLogger.PutMessageToFile(const AMessage: string);
var
  FileName: string;
  hFile: THandle;
  len, cnt: Cardinal;
  buf: PChar;
begin
  FileName := ChangeFileExt(ParamStr(0), '.log');
  hFile := CreateFile(PChar(FileName), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (hFile = INVALID_HANDLE_VALUE) then
  begin
    hFile := CreateFile(PChar(FileName), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  end;
  if (hFile <> INVALID_HANDLE_VALUE) then
  begin
    SetFilePointer(hFile, 0, nil, FILE_END);
    len := Length(AMessage) + 1;
    GetMem(buf, len);
    StrCopy(buf, PCHAR(AMessage));
    WriteFile(hFile, buf[0], len - 1, cnt, nil);
    FreeMem(buf);
    CloseHandle(hFile);
  end;
end;

class procedure TclLogger.ReleaseInstance;
begin
  AccessInstance(0).Free();
end;

procedure TclLogger.UnhookExceptions;
begin
  Application.OnException := FPrevOnAppException;
end;

initialization
  fOldExceptObjProc := ExceptObjProc;
  ExceptObjProc := @clGetExceptionObject;
  TclLogger.Instance().HookExceptions();

finalization
  TclLogger.ReleaseInstance();

end.
