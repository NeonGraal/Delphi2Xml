unit Test.Global.Tests;

interface

implementation

uses
  D2X.Global,
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  Test.Global,
  Test.Utils,
  TestFramework;

type
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
  published
    procedure TestReduceString;
    procedure TestMakeFileName;

    procedure TestTestStream;
    procedure TestTestStreamRead;
    procedure TestTestStreamWrite;
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
    pLogger := TD2XLogger.Create.StartLog(lSB);
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

    pLogger := TD2XLogger.Create.StartLog(lLogBuilder);

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

    pLogger := TD2XLogger.Create.StartLog(lLogBuilder);

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
    FD2XLogger := TD2XLogger.Create.StartLog(pBuilder);

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
    FD2XLogger := TD2XLogger.Create.StartLog(lMyBuilder);
    FD2XLogger.Log('Log check', [], False);
    CheckBuilder('Log check', 'Log check', lMyBuilder);

    pLogger := TD2XLogger.Create.StartLog(FD2XLogger);
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
    FD2XLogger := TD2XLogger.Create.StartLog(pStream);

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
    FD2XLogger := TD2XLogger.Create.StartLog(pWriter);

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

    pLogger := TD2XLogger.Create.StartLog(lLogBuilder);

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

    pLogger1 := TD2XLogger.Create.StartLog(lLogBuilder1);
    pLogger1.Log('Log check 1', [], False);
    CheckLogs('Log check 1', 1);

    pLogger2 := TD2XLogger.Create.StartLog(lLogBuilder2);
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

{ TestTD2XUtils }

procedure TestTD2XUtils.TestMakeFileName;
begin
  CheckEqualsString('', MakeFileName('', ''), 'Both Blank');
  CheckEqualsString('.D', MakeFileName('', '.D'), 'Default');
  CheckEqualsString('F', MakeFileName('F', ''), 'Filename');
  CheckEqualsString('F.E', MakeFileName('F.E', ''), 'Extension');
  CheckEqualsString('F.D', MakeFileName('F', '.D'), 'Filename Default');
  CheckEqualsString('F.E', MakeFileName('F.E', '.D'), 'Extension Default');
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

procedure TestTD2XUtils.TestTestStream;
begin
  CheckEqualsString('Test', fDS.Description, 'Test Stream Description');
  CheckTrue(fDS.Exists, 'Test Stream Exists');
end;

procedure TestTD2XUtils.TestTestStreamRead;
begin
  fS.WriteString('Test');

  CheckReader('Test', 'Test Stream Reading', fDS.ReadFrom);
end;

procedure TestTD2XUtils.TestTestStreamWrite;
begin
  fDS.WriteTo.Write('Test');
  CheckStream('Test', 'Test Stream Writing');
end;

initialization

RegisterTests('Global', [TestTD2X.Suite, TestTD2XUtils.Suite, TestID2XLogger.Suite,
    TestTD2XLogger.Suite]);

end.
