unit Test.Global;

interface

uses
  D2X.Global,
  D2X.IO,
  System.Classes,
  System.SysUtils,
  TestFramework;

type
  TTestProc = reference to procedure;

  TBaseTestCase = class(TTestCase)
  protected
    procedure CheckInvalidParam(pExp, pLabel: string; pProc: TTestProc);
    procedure CheckInterface(pExp: TGUID; pObj: TD2XInterfaced; pLabel: string);
    procedure CheckNotInterface(pExp: TGUID; pObj: TD2XInterfaced; pLabel: string);
  end;

  TStringTestCase = class(TBaseTestCase)
  protected
    fB: TStringBuilder;
    fS: TStringStream;
    fW: TStringWriter;
    fL: TStringList;

    fDS: ID2XFile;

    procedure CheckBuilder(pExp, pLabel: string; pB: TStringBuilder = nil);
    procedure CheckList(pExp, pLabel: string; pL: TStringList = nil);
    procedure CheckReader(pExp, pLabel: string; pR: TStreamReader);
    procedure CheckStream(pExp, pLabel: string; pS: TStringStream = nil);
    procedure CheckWriter(pExp, pLabel: string; pW: TStringWriter = nil);

  public
    procedure SetUp; override;
    procedure TearDown; override;

  end;

  TLoggerTestCase = class(TStringTestCase)
  protected
    fLog: TD2XLogger;

    procedure CheckLog(pExp, pLabel: string);
  public
    procedure SetUp; override;
    procedure TearDown; override;

  end;

implementation

uses
  D2X.Param,
  Test.Utils,
  System.StrUtils;

type
  TD2XTestStream = class(TD2XInterfaced, ID2XFile)
  private
    fSW: TStreamWriter;
    fSR: TStreamReader;

    fS: TStringStream;
  public
    constructor Create(pS: TStringStream);
    destructor Destroy; override;

    function Description: string;
    function Exists: Boolean;
    function ReadFrom: TStreamReader;
    function WriteTo(pAppend: Boolean = False): TStreamWriter;
  end;

  { TStringBuilderTestCase }

procedure TStringTestCase.CheckBuilder(pExp, pLabel: string; pB: TStringBuilder);
begin
  if not Assigned(pB) then
    pB := fB;
  CheckEqualsString(pExp, ReduceString(pB.ToString), pLabel);
  pB.Clear;
end;

procedure TStringTestCase.CheckList(pExp, pLabel: string; pL: TStringList);
begin
  if not Assigned(pL) then
    pL := fL;
  CheckEqualsString(pExp, ReduceString(pL.Text), pLabel);
  pL.Clear;
end;

procedure TStringTestCase.CheckReader(pExp, pLabel: string; pR: TStreamReader);
begin
  CheckEqualsString(pExp, ReduceString(pR.ReadToEnd), pLabel);
end;

procedure TStringTestCase.CheckStream(pExp, pLabel: string; pS: TStringStream);
begin
  if not Assigned(pS) then
    pS := fS;
  CheckEqualsString(pExp, ReduceString(pS.DataString), pLabel);
  pS.Clear;
end;

procedure TStringTestCase.CheckWriter(pExp, pLabel: string; pW: TStringWriter);
begin
  if Assigned(pW) then
    CheckEqualsString(pExp, ReduceString(pW.ToString), pLabel)
  else
  begin
    CheckEqualsString(pExp, ReduceString(fW.ToString), pLabel);
    fB.Clear;
  end;
end;

procedure TStringTestCase.SetUp;
begin
  inherited;

  fB := TStringBuilder.Create;
  fL := TStringList.Create;
  fS := TStringStream.Create;
  fW := TStringWriter.Create(fB);
  fDS := TD2XTestStream.Create(fS);
end;

procedure TStringTestCase.TearDown;
begin
  DisposeOf(fDS);
  FreeAndNil(fW);
  FreeAndNil(fS);
  FreeAndNil(fL);
  FreeAndNil(fB);

  inherited;
end;

{ TLoggerTestCase }

procedure TLoggerTestCase.CheckLog(pExp, pLabel: string);
begin
  if ContainsStr(pExp, '%s') then
    CheckBuilder(Format(pExp, [ChangeFileExt(ExtractFileName(ParamStr(0)), '')]), pLabel, fB)
  else
    CheckBuilder(pExp, pLabel, fB);
end;

procedure TLoggerTestCase.SetUp;
begin
  inherited;

  fLog := TD2XLogger.Create.StartLog(fB);
end;

procedure TLoggerTestCase.TearDown;
begin
  FreeAndNil(fLog);

  inherited;
end;

{ TD2XTestStream }

constructor TD2XTestStream.Create(pS: TStringStream);
begin
  fS := pS;
end;

function TD2XTestStream.Description: string;
begin
  Result := 'Test';
end;

destructor TD2XTestStream.Destroy;
begin
  FreeAndNil(fSR);
  FreeAndNil(fSW);

  inherited;
end;

function TD2XTestStream.Exists: Boolean;
begin
  Result := True;
end;

function TD2XTestStream.ReadFrom: TStreamReader;
begin
  if not Assigned(fSR) then
    fSR := TStreamReader.Create(fS);
  fS.Position := 0;
  Result := fSR;
end;

function TD2XTestStream.WriteTo(pAppend: Boolean): TStreamWriter;
begin
  if not Assigned(fSW) then
    fSW := TStreamWriter.Create(fS);
  Result := fSW;
end;

{ TBaseTestCase }

procedure TBaseTestCase.CheckInterface(pExp: TGUID; pObj: TD2XInterfaced; pLabel: string);
var
  lObj: IUnknown;
begin
  CheckTrue(pObj.GetInterface(pExp, lObj), 'Has ' + pLabel + ' interface');
end;

procedure TBaseTestCase.CheckInvalidParam(pExp, pLabel: string; pProc: TTestProc);
begin
  StartExpectingException(EInvalidParam);
  try
    pProc;
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString(pExp, E.Message, pLabel + ' Exception message');
      raise;
    end;
  end;
end;

procedure TBaseTestCase.CheckNotInterface(pExp: TGUID; pObj: TD2XInterfaced; pLabel: string);
var
  lObj: IUnknown;
begin
  CheckFalse(pObj.GetInterface(pExp, lObj), 'Not ' + pLabel + ' interface');
end;

end.
