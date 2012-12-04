unit Test.Test;

interface

uses
  D2X,
  D2X.IO,
  System.Classes,
  System.SysUtils,
  TestFramework;

type
  TStringTestCase = class(TTestCase)
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
  System.Rtti,
  System.StrUtils,
  Test.Utils;

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

  TestTD2X = class(TTestCase)
  strict private
    fD2X: TD2X;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestToLabel;
  end;

  TestTD2XUtils = class(TStringTestCase)
  private
    function PairFormat(pPair: TStrIntPair): string;
  published
    procedure TestReduceString;
    procedure TestMakeFileName;
    procedure TestTidyFilename;

    procedure TestOutputStrIntDictNoStream;
    procedure TestOutputStrIntDictNoFunc;
    procedure TestOutputStrIntDictNoDict;
    procedure TestOutputStrIntDict;
  end;

  TestID2XLogger = class(TLoggerTestCase)
  published
    procedure TestJoinLog;
    procedure TestLog;

  end;
  // Test methods for class TD2XLogger

  TestTD2XLogger = class(TStringTestCase)
  strict private
    FD2XLogger: TD2XLogger;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateStream;
    procedure TestCreateWriter;
    procedure TestCreateLogger;
    procedure TestCreateBuilder;

    procedure TestStartLogStream;
    procedure TestStartLogWriter;
    procedure TestStartLogLogger;
    procedure TestStartLogBuilder;

    procedure TestStartLogNilStream;
    procedure TestStartLogNilWriter;
    procedure TestStartLogNilLogger;
    procedure TestStartLogNilBuilder;

    procedure TestStopLogStream;
    procedure TestStopLogWriter;
    procedure TestStopLogLogger;
    procedure TestStopLogBuilder;

    procedure TestJoinLog;
    procedure TestJoinLogComplex;
  end;

  { TestTD2X }

procedure TestTD2X.SetUp;
begin
  fD2X := TD2X.Create;
end;

procedure TestTD2X.TearDown;
begin
  FreeAndNil(fD2X);
end;

procedure TestTD2X.TestToLabel;
var
  ReturnValue: string;
  pVal: TD2XParseMode;
begin
  pVal := pmFull;
  ReturnValue := fD2X.ToLabel(pVal);
  CheckEqualsString('Full', ReturnValue, 'ReturnValue');
end;

{ TestID2XLogger }

procedure TestID2XLogger.TestJoinLog;
var
  pLogger: TD2XLogger;
  lSB: TStringBuilder;
begin
  lSB := nil;
  pLogger := nil;

  fLog.Log('Log String', [], False);
  CheckBuilder('Log String', 'Check Logging');

  try
    lSB := TStringBuilder.Create;
    pLogger := TD2XLogger.Create(lSB);
    pLogger.Log('Log String1', [], False);
    CheckBuilder('Log String1', 'Check Logging 1', lSB);
    CheckLog('', 'Check Not Logging 1');

    fLog.JoinLog(pLogger);

    fLog.Log('Log String2', [], False);
    CheckBuilder('Log String2', 'Check Logging 2', lSB);
    CheckLog('', 'Check Not Logging 2');

    fLog.JoinLog(nil);

    fLog.Log('Log String3', [], False);
    CheckLog('Log String3', 'Check Logging 3');
    CheckBuilder('', 'Check Not Logging 3', lSB);
  finally
    pLogger.Free;
    lSB.Free;
  end;
end;

procedure TestID2XLogger.TestLog;
begin
  fLog.Log('Log', [], False);
  fLog.Log('String', [], False);
  CheckLog('LogString', 'No Arguments');

  fLog.Log('Log', []);
  fLog.Log('String', []);
  CheckLog('Log String', 'No Arguments Line');

  fLog.Log('Log %s %s', ['Arg1', 'Arg2'], False);
  fLog.Log('String', []);
  CheckLog('Log Arg1 Arg2String', 'Arguments');

  fLog.Log('Log %s %s', ['Arg1', 'Arg2']);
  fLog.Log('String', []);
  CheckLog('Log Arg1 Arg2 String', 'Arguments Line');
end;

{ TestTD2XLogger }

procedure TestTD2XLogger.SetUp;
begin
  inherited;

  FD2XLogger := TD2XLogger.Create;
end;

procedure TestTD2XLogger.TearDown;
begin
  FreeAndNil(FD2XLogger);

  inherited;
end;

procedure TestTD2XLogger.TestStartLogStream;
begin
  FD2XLogger.StartLog(fS);

  FD2XLogger.Log('Log simple', [], False);
  CheckStream('Log simple', 'Log simple');
end;

procedure TestTD2XLogger.TestStartLogWriter;
var
  pWriter: TStringWriter;
begin
  pWriter := TStringWriter.Create;
  try
    FD2XLogger.StartLog(pWriter);

    FD2XLogger.Log('Log simple', [], False);
    CheckEqualsString('Log simple', pWriter.ToString, 'Log simple');
  finally
    pWriter.Free;
  end;
end;

procedure TestTD2XLogger.TestStartLogBuilder;
begin
  FD2XLogger.StartLog(fB);

  FD2XLogger.Log('Log simple', [], False);
  CheckBuilder('Log simple', 'Log simple');
end;

procedure TestTD2XLogger.TestStartLogLogger;
var
  pLogger: TD2XLogger;
  lLogBuilder: TStringBuilder;
begin
  lLogBuilder := nil;
  pLogger := nil;
  try
    lLogBuilder := TStringBuilder.Create;
    FD2XLogger.StartLog(fB);
    FD2XLogger.Log('Log check 1', [], False);
    CheckBuilder('Log check 1', 'Log check 1');
    CheckBuilder('', 'Log check 1', lLogBuilder);

    pLogger := TD2XLogger.Create(lLogBuilder);

    pLogger.Log('Log check 2', [], False);
    CheckBuilder('Log check 2', 'Log check 2', lLogBuilder);
    CheckBuilder('', 'Log check 2');

    FD2XLogger.StartLog(pLogger);
    FD2XLogger.Log('Log simple 1', [], False);
    CheckBuilder('Log simple 1', 'Log simple 1', lLogBuilder);
    CheckBuilder('', 'Log simple 1');

    FD2XLogger.JoinLog(nil);
    FD2XLogger.Log('Log simple 2', [], False);
    CheckBuilder('', 'Log simple 2', lLogBuilder);
    CheckBuilder('', 'Log simple 2');
  finally
    pLogger.Free;
    lLogBuilder.Free;
  end;
end;

procedure TestTD2XLogger.TestStartLogNilBuilder;
var
  pBuilder: TStringBuilder;
begin
  pBuilder := nil;

  FD2XLogger.StartLog(pBuilder);

  FD2XLogger.Log('Log simple', [], False);
  Check(True, 'Nil succeeded');
end;

procedure TestTD2XLogger.TestStartLogNilLogger;
var
  pLogger: ID2XLogger;
begin
  pLogger := nil;

  FD2XLogger.StartLog(pLogger);

  FD2XLogger.Log('Log simple', [], False);
  Check(True, 'Nil succeeded');
end;

procedure TestTD2XLogger.TestStartLogNilStream;
var
  pStream: TStringStream;
begin
  pStream := nil;

  FD2XLogger.StartLog(pStream);

  FD2XLogger.Log('Log simple', [], False);
  Check(True, 'Nil succeeded');
end;

procedure TestTD2XLogger.TestStartLogNilWriter;
var
  pWriter: TStringWriter;
begin
  pWriter := nil;

  FD2XLogger.StartLog(pWriter);

  FD2XLogger.Log('Log simple', [], False);
  Check(True, 'Nil succeeded');
end;

procedure TestTD2XLogger.TestStopLogBuilder;
var
  pBuilder: TStringBuilder;
begin
  pBuilder := TStringBuilder.Create;
  try
    FD2XLogger.StartLog(pBuilder);

    FD2XLogger.Log('Log simple', [], False);
    CheckBuilder('Log simple', 'Log simple', pBuilder);

    FD2XLogger.StopLog;
    FD2XLogger.Log('Log none', [], False);
    CheckBuilder('', 'Log none', pBuilder);
  finally
    pBuilder.Free;
  end;
end;

procedure TestTD2XLogger.TestStopLogLogger;
var
  lMyBuilder: TStringBuilder;
  pLogger: TD2XLogger;
  lLogBuilder: TStringBuilder;
begin
  lMyBuilder := nil;
  lLogBuilder := nil;
  pLogger := nil;
  try
    lMyBuilder := TStringBuilder.Create;
    lLogBuilder := TStringBuilder.Create;
    FD2XLogger.StartLog(lMyBuilder);
    FD2XLogger.Log('Log check 1', [], False);
    CheckBuilder('Log check 1', 'Log check 1', lMyBuilder);
    CheckBuilder('', 'Log check 1', lLogBuilder);

    pLogger := TD2XLogger.Create(lLogBuilder);

    pLogger.Log('Log check 2', [], False);
    CheckBuilder('Log check 2', 'Log check 2', lLogBuilder);
    CheckBuilder('', 'Log check 2', lMyBuilder);

    FD2XLogger.StartLog(pLogger);
    FD2XLogger.Log('Log simple 1', [], False);
    CheckBuilder('Log simple 1', 'Log simple 1', lLogBuilder);
    CheckBuilder('', 'Log simple 1', lMyBuilder);

    FD2XLogger.StopLog;
    FD2XLogger.Log('Log simple 2', [], False);
    CheckBuilder('', 'Log simple 2', lLogBuilder);
    CheckBuilder('', 'Log simple 2', lMyBuilder);
  finally
    pLogger.Free;
    lLogBuilder.Free;
    lMyBuilder.Free;
  end;
end;

procedure TestTD2XLogger.TestStopLogStream;
var
  pStream: TStringStream;
begin
  pStream := TStringStream.Create;
  try
    FD2XLogger.StartLog(pStream);

    FD2XLogger.Log('Log simple', [], False);
    CheckStream('Log simple', 'Log simple', pStream);

    FD2XLogger.StopLog;
    FD2XLogger.Log('Log none', [], False);
    CheckStream('', 'Log none', pStream);
  finally
    pStream.Free;
  end;
end;

procedure TestTD2XLogger.TestStopLogWriter;
var
  pWriter: TStringWriter;
  lSB: TStringBuilder;
begin
  lSB := TStringBuilder.Create;
  pWriter := TStringWriter.Create(lSB);
  try
    FD2XLogger.StartLog(pWriter);

    FD2XLogger.Log('Log simple', [], False);
    CheckBuilder('Log simple', 'Log simple', lSB);

    FD2XLogger.StopLog;
    FD2XLogger.Log('Log none', [], False);
    CheckBuilder('', 'Log none', lSB);
  finally
    pWriter.Free;
    lSB.Free;
  end;
end;

procedure TestTD2XLogger.TestCreateBuilder;
var
  pBuilder: TStringBuilder;
begin
  FD2XLogger.Free;
  pBuilder := TStringBuilder.Create;
  try
    FD2XLogger := TD2XLogger.Create(pBuilder);

    FD2XLogger.Log('Log simple', [], False);
    CheckBuilder('Log simple', 'Log simple', pBuilder);
  finally
    pBuilder.Free;
  end;
end;

procedure TestTD2XLogger.TestCreateLogger;
var
  lMyBuilder: TStringBuilder;
  pLogger: TD2XLogger;
  lLogBuilder: TStringBuilder;
begin
  lMyBuilder := nil;
  lLogBuilder := nil;
  pLogger := nil;

  FD2XLogger.Free;
  try
    lMyBuilder := TStringBuilder.Create;
    lLogBuilder := TStringBuilder.Create;
    FD2XLogger := TD2XLogger.Create(lMyBuilder);
    FD2XLogger.Log('Log check', [], False);
    CheckBuilder('Log check', 'Log check', lMyBuilder);

    pLogger := TD2XLogger.Create(FD2XLogger);
    FD2XLogger.Log('Log simple', [], False);
    CheckBuilder('Log simple', 'Log simple', lMyBuilder);
  finally
    pLogger.Free;
    lLogBuilder.Free;
    lMyBuilder.Free;
  end;
end;

procedure TestTD2XLogger.TestCreateStream;
var
  pStream: TStringStream;
begin
  FD2XLogger.Free;
  pStream := TStringStream.Create;
  try
    FD2XLogger := TD2XLogger.Create(pStream);

    FD2XLogger.Log('Log simple', [], False);
    CheckStream('Log simple', 'Log simple', pStream);
  finally
    pStream.Free;
  end;
end;

procedure TestTD2XLogger.TestCreateWriter;
var
  pWriter: TStringWriter;
begin
  FD2XLogger.Free;
  pWriter := TStringWriter.Create;
  try
    FD2XLogger := TD2XLogger.Create(pWriter);

    FD2XLogger.Log('Log simple', [], False);
    CheckEqualsString('Log simple', pWriter.ToString, 'Log simple');
  finally
    pWriter.Free;
  end;
end;

procedure TestTD2XLogger.TestJoinLog;
var
  lMyBuilder: TStringBuilder;
  pLogger: TD2XLogger;
  lLogBuilder: TStringBuilder;
begin
  lMyBuilder := nil;
  lLogBuilder := nil;
  pLogger := nil;
  try
    lMyBuilder := TStringBuilder.Create;
    lLogBuilder := TStringBuilder.Create;
    FD2XLogger.StartLog(lMyBuilder);
    FD2XLogger.Log('Log check 1', [], False);
    CheckBuilder('Log check 1', 'Log check 1', lMyBuilder);
    CheckBuilder('', 'Log check 1', lLogBuilder);

    pLogger := TD2XLogger.Create(lLogBuilder);

    pLogger.Log('Log check 2', [], False);
    CheckBuilder('Log check 2', 'Log check 2', lLogBuilder);
    CheckBuilder('', 'Log check 2', lMyBuilder);

    FD2XLogger.JoinLog(pLogger);
    FD2XLogger.Log('Log simple 1', [], False);
    CheckBuilder('Log simple 1', 'Log simple 1', lLogBuilder);
    CheckBuilder('', 'Log simple 1', lMyBuilder);

    FD2XLogger.JoinLog(nil);
    FD2XLogger.Log('Log simple 2', [], False);
    CheckBuilder('', 'Log simple 2', lLogBuilder);
    CheckBuilder('Log simple 2', 'Log simple 2', lMyBuilder);
  finally
    pLogger.Free;
    lLogBuilder.Free;
    lMyBuilder.Free;
  end;
end;

procedure TestTD2XLogger.TestJoinLogComplex;
var
  lMyBuilder: TStringBuilder;
  pLogger1: TD2XLogger;
  lLogBuilder1: TStringBuilder;
  pLogger2: TD2XLogger;
  lLogBuilder2: TStringBuilder;

  procedure CheckLogs(pLabel: string; pLog: Byte);
  begin
    CheckBuilder(IfThen(pLog = 0, pLabel, ''), pLabel, lMyBuilder);
    CheckBuilder(IfThen(pLog = 1, pLabel, ''), pLabel, lLogBuilder1);
    CheckBuilder(IfThen(pLog = 2, pLabel, ''), pLabel, lLogBuilder2);
  end;

begin
  lMyBuilder := nil;
  lLogBuilder1 := nil;
  lLogBuilder2 := nil;
  pLogger1 := nil;
  pLogger2 := nil;
  try
    lMyBuilder := TStringBuilder.Create;
    lLogBuilder1 := TStringBuilder.Create;
    lLogBuilder2 := TStringBuilder.Create;

    FD2XLogger.StartLog(lMyBuilder);
    FD2XLogger.Log('Log check', [], False);
    CheckLogs('Log check', 0);

    pLogger1 := TD2XLogger.Create(lLogBuilder1);
    pLogger1.Log('Log check 1', [], False);
    CheckLogs('Log check 1', 1);

    pLogger2 := TD2XLogger.Create(lLogBuilder2);
    pLogger2.Log('Log check 2', [], False);
    CheckLogs('Log check 2', 2);

    FD2XLogger.JoinLog(pLogger1);
    FD2XLogger.Log('Log simple 1', [], False);
    CheckLogs('Log simple 1', 1);

    FD2XLogger.JoinLog(nil);
    FD2XLogger.Log('Log simple 2', [], False);
    CheckLogs('Log simple 2', 0);

    FD2XLogger.JoinLog(pLogger2);
    FD2XLogger.Log('Log simple 3', [], False);
    CheckLogs('Log simple 3', 2);

    pLogger2.JoinLog(pLogger1);
    FD2XLogger.Log('Log simple 4', [], False);
    CheckLogs('Log simple 4', 1);

    FD2XLogger.JoinLog(nil);
    FD2XLogger.Log('Log simple 5', [], False);
    CheckLogs('Log simple 5', 0);
  finally
    pLogger2.Free;
    pLogger1.Free;
    lLogBuilder2.Free;
    lLogBuilder1.Free;
    lMyBuilder.Free;
  end;
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
var
  lDS: TD2XInterfaced;
begin
  if Assigned(fDS) then
  begin
    lDS := fDS as TD2XInterfaced;
    fDS := nil;
    lDS.Free;
  end;
  FreeAndNil(fW);
  FreeAndNil(fS);
  FreeAndNil(fL);
  FreeAndNil(fB);

  inherited;
end;

{ TLoggerTestCase }

procedure TLoggerTestCase.CheckLog(pExp, pLabel: string);
begin
  CheckBuilder(pExp, pLabel, fB);
end;

procedure TLoggerTestCase.SetUp;
begin
  inherited;

  fLog := TD2XLogger.Create(fB);
end;

procedure TLoggerTestCase.TearDown;
begin
  FreeAndNil(fLog);

  inherited;
end;

{ TestTD2XUtils }

function TestTD2XUtils.PairFormat(pPair: TStrIntPair): string;
begin
  Result := pPair.Key + ' = ' + IntToStr(pPair.Value);
end;

procedure TestTD2XUtils.TestMakeFileName;
begin
  CheckEqualsString('', MakeFileName('', ''), 'Both Blank');
  CheckEqualsString('.D', MakeFileName('', '.D'), 'Default');
  CheckEqualsString('F', MakeFileName('F', ''), 'Filename');
  CheckEqualsString('F.E', MakeFileName('F.E', ''), 'Extension');
  CheckEqualsString('F.D', MakeFileName('F', '.D'), 'Filename Default');
  CheckEqualsString('F.E', MakeFileName('F.E', '.D'), 'Extension Default');
end;

procedure TestTD2XUtils.TestOutputStrIntDict;
var
  lDict: TStrIntDict;
begin
  lDict := TStrIntDict.Create;
  try
    OutputStrIntDict(lDict, fDS.WriteTo, PairFormat);
    CheckStream('', 'No Entries');
    lDict.Add('Test', 1);
    OutputStrIntDict(lDict, fDS.WriteTo, PairFormat);
    CheckStream('Test=Test = 1', 'An Entry');
  finally
    lDict.Free;
  end;
end;

procedure TestTD2XUtils.TestOutputStrIntDictNoDict;
begin
  StartExpectingException(EAssertionFailed);
  try
    OutputStrIntDict(nil, fDS.WriteTo, PairFormat);
  except
    on E: EAssertionFailed do
    begin
      CheckTrue(StartsText('Need a Dictionary', E.Message), 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XUtils.TestOutputStrIntDictNoFunc;
begin
  StartExpectingException(EAssertionFailed);
  try
    OutputStrIntDict(nil, fDS.WriteTo, nil);
  except
    on E: EAssertionFailed do
    begin
      CheckTrue(StartsText('Need a Function', E.Message), 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XUtils.TestOutputStrIntDictNoStream;
begin
  StartExpectingException(EAssertionFailed);
  try
    OutputStrIntDict(nil, nil, nil);
  except
    on E: EAssertionFailed do
    begin
      CheckTrue(StartsText('Need a Stream', E.Message), 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XUtils.TestReduceString;
begin
  CheckEqualsString('', ReduceString(' '), 'Blank');
  CheckEqualsString('A', ReduceString('A'), 'Simple');
  CheckEqualsString('A', ReduceString(' A'), 'Leading');
  CheckEqualsString('A', ReduceString('A'#13), 'Trailing');
  CheckEqualsString('A', ReduceString(#9'A'#10), 'Leading and Trailing');
  CheckEqualsString('A B', ReduceString('A '#13#10' B'), 'Extended');
  CheckEqualsString('A B C D', ReduceString('A  B'#13#10'C'#9#9'D'), 'Extended');
end;

procedure TestTD2XUtils.TestTidyFilename;
begin
  CheckEqualsString('', TidyFilename(''), 'Blank');
  CheckEqualsString('A', TidyFilename('A'), 'Simple');
  CheckEqualsString('A', TidyFilename('A*'), 'Star');
  CheckEqualsString('A', TidyFilename('A.'), 'Dot');
  CheckEqualsString('A', TidyFilename('A?'), 'Query');
  CheckEqualsString('A', TidyFilename('A...'), 'Many');
  CheckEqualsString('ABCDEF', TidyFilename('A?*B..?C?*?D***.E????*F'), 'Complex');
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

initialization

RegisterTests('Global', [TestTD2X.Suite, TestTD2XUtils.Suite, TestID2XLogger.Suite,
    TestTD2XLogger.Suite]);

end.
