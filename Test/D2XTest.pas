unit D2XTest;

interface

implementation

uses
  TestFramework,
  D2X,
  D2XOptions,
  System.Classes,
  System.Rtti,
  System.StrUtils,
  System.SysUtils;

type
  TStringBuilderTestCase = class(TTestCase)
  protected
    procedure CheckString(pSB: TStringBuilder; pExp, pLabel: string); overload;
    procedure CheckString(pSS: TStringStream; pExp, pLabel: string); overload;

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

  TestID2XLogger = class(TStringBuilderTestCase)
  strict private
    fID2XLogger: ID2XLogger;
    fSB: TStringBuilder;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestJoinLog;
    procedure TestLog;

  end;
  // Test methods for class TD2XLogger

  TestTD2XLogger = class(TStringBuilderTestCase)
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
  fD2X.Free;
  fD2X := nil;
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

procedure TestID2XLogger.SetUp;
begin
  fSB := TStringBuilder.Create;
  fID2XLogger := TD2XLogger.Create(fSB);
end;

procedure TestID2XLogger.TearDown;
begin
  fID2XLogger := nil;
  FreeAndNil(fSB);
end;

procedure TestID2XLogger.TestJoinLog;
var
  pLogger: ID2XLogger;
  lSB: TStringBuilder;
begin
  fID2XLogger.Log('Log String', [], False);
  CheckString(fSB, 'Log String', 'Check Logging');

  lSB := TStringBuilder.Create;
  try
    pLogger := TD2XLogger.Create(lSB);
    pLogger.Log('Log String1', [], False);
    CheckString(lSB, 'Log String1', 'Check Logging 1');
    CheckString(fSB, '', 'Check Not Logging 1');

    fID2XLogger.JoinLog(pLogger);

    fID2XLogger.Log('Log String2', [], False);
    CheckString(lSB, 'Log String2', 'Check Logging 2');
    CheckString(fSB, '', 'Check Not Logging 2');

    fID2XLogger.JoinLog(nil);

    fID2XLogger.Log('Log String3', [], False);
    CheckString(fSB, 'Log String3', 'Check Logging 3');
    CheckString(lSB, '', 'Check Not Logging 3');
  finally
    lSB.Free;
  end;
end;

procedure TestID2XLogger.TestLog;
begin
  fID2XLogger.Log('Log String', [], False);
  CheckString(fSB, 'Log String', 'No Arguments');

  fID2XLogger.Log('Log String', []);
  CheckString(fSB, 'Log String'#13#10, 'No Arguments Line');

  fID2XLogger.Log('Log %s %s', ['Arg1', 'Arg2'], False);
  CheckString(fSB, 'Log Arg1 Arg2', 'Arguments');

  fID2XLogger.Log('Log %s %s', ['Arg1', 'Arg2']);
  CheckString(fSB, 'Log Arg1 Arg2'#13#10, 'Arguments Line');
end;

procedure TestTD2XLogger.SetUp;
begin
  FD2XLogger := TD2XLogger.Create;
end;

procedure TestTD2XLogger.TearDown;
begin
  FD2XLogger.Free;
  FD2XLogger := nil;
end;

procedure TestTD2XLogger.TestStartLogStream;
var
  pStream: TStringStream;
begin
  pStream := TStringStream.Create;
  try
    FD2XLogger.StartLog(pStream);

    FD2XLogger.Log('Log simple', [], False);
    CheckString(pStream, 'Log simple', 'Log simple');
  finally
    pStream.Free;
  end;
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
var
  pBuilder: TStringBuilder;
begin
  pBuilder := TStringBuilder.Create;
  try
    FD2XLogger.StartLog(pBuilder);

    FD2XLogger.Log('Log simple', [], False);
    CheckString(pBuilder, 'Log simple', 'Log simple');
  finally
    pBuilder.Free;
  end;
end;

procedure TestTD2XLogger.TestStartLogLogger;
var
  lMyBuilder: TStringBuilder;
  pLogger: ID2XLogger;
  lLogBuilder: TStringBuilder;
begin
  lMyBuilder := TStringBuilder.Create;
  lLogBuilder := TStringBuilder.Create;
  try
    FD2XLogger.StartLog(lMyBuilder);
    FD2XLogger.Log('Log check 1', [], False);
    CheckString(lMyBuilder, 'Log check 1', 'Log check 1');
    CheckString(lLogBuilder, '', 'Log check 1');

    pLogger := TD2XLogger.Create(lLogBuilder);

    pLogger.Log('Log check 2', [], False);
    CheckString(lLogBuilder, 'Log check 2', 'Log check 2');
    CheckString(lMyBuilder, '', 'Log check 2');

    FD2XLogger.StartLog(pLogger);
    FD2XLogger.Log('Log simple 1', [], False);
    CheckString(lLogBuilder, 'Log simple 1', 'Log simple 1');
    CheckString(lMyBuilder, '', 'Log simple 1');

    FD2XLogger.JoinLog(nil);
    FD2XLogger.Log('Log simple 2', [], False);
    CheckString(lLogBuilder, '', 'Log simple 2');
    CheckString(lMyBuilder, '', 'Log simple 2');
  finally
    lLogBuilder.Free;
    lMyBuilder.Free;
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
    CheckString(pBuilder, 'Log simple', 'Log simple');

    FD2XLogger.StopLog;
    FD2XLogger.Log('Log none', [], False);
    CheckString(pBuilder, '', 'Log none');
  finally
    pBuilder.Free;
  end;
end;

procedure TestTD2XLogger.TestStopLogLogger;
var
  lMyBuilder: TStringBuilder;
  pLogger: ID2XLogger;
  lLogBuilder: TStringBuilder;
begin
  lMyBuilder := TStringBuilder.Create;
  lLogBuilder := TStringBuilder.Create;
  try
    FD2XLogger.StartLog(lMyBuilder);
    FD2XLogger.Log('Log check 1', [], False);
    CheckString(lMyBuilder, 'Log check 1', 'Log check 1');
    CheckString(lLogBuilder, '', 'Log check 1');

    pLogger := TD2XLogger.Create(lLogBuilder);

    pLogger.Log('Log check 2', [], False);
    CheckString(lLogBuilder, 'Log check 2', 'Log check 2');
    CheckString(lMyBuilder, '', 'Log check 2');

    FD2XLogger.StartLog(pLogger);
    FD2XLogger.Log('Log simple 1', [], False);
    CheckString(lLogBuilder, 'Log simple 1', 'Log simple 1');
    CheckString(lMyBuilder, '', 'Log simple 1');

    FD2XLogger.StopLog;
    FD2XLogger.Log('Log simple 2', [], False);
    CheckString(lLogBuilder, '', 'Log simple 2');
    CheckString(lMyBuilder, '', 'Log simple 2');
  finally
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
    CheckString(pStream, 'Log simple', 'Log simple');

    FD2XLogger.StopLog;
    FD2XLogger.Log('Log none', [], False);
    CheckString(pStream, '', 'Log none');
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
    CheckString(lSB, 'Log simple', 'Log simple');

    FD2XLogger.StopLog;
    FD2XLogger.Log('Log none', [], False);
    CheckString(lSB, '', 'Log none');
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
    CheckString(pBuilder, 'Log simple', 'Log simple');
  finally
    pBuilder.Free;
  end;
end;

procedure TestTD2XLogger.TestCreateLogger;
var
  lMyBuilder: TStringBuilder;
  pLogger: ID2XLogger;
  lLogBuilder: TStringBuilder;
begin
  FD2XLogger.Free;
  lMyBuilder := TStringBuilder.Create;
  lLogBuilder := TStringBuilder.Create;
  try
    FD2XLogger := TD2XLogger.Create(lMyBuilder);
    FD2XLogger.Log('Log check 1', [], False);
    CheckString(lMyBuilder, 'Log check 1', 'Log check 1');
    CheckString(lLogBuilder, '', 'Log check 1');

    pLogger := TD2XLogger.Create(lLogBuilder);

    pLogger.Log('Log check 2', [], False);
    CheckString(lLogBuilder, 'Log check 2', 'Log check 2');
    CheckString(lMyBuilder, '', 'Log check 2');

    FD2XLogger.StartLog(pLogger);
    FD2XLogger.Log('Log simple 1', [], False);
    CheckString(lLogBuilder, 'Log simple 1', 'Log simple 1');
    CheckString(lMyBuilder, '', 'Log simple 1');

    FD2XLogger.JoinLog(nil);
    FD2XLogger.Log('Log simple 2', [], False);
    CheckString(lLogBuilder, '', 'Log simple 2');
    CheckString(lMyBuilder, '', 'Log simple 2');
  finally
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
    CheckString(pStream, 'Log simple', 'Log simple');
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
  pLogger: ID2XLogger;
  lLogBuilder: TStringBuilder;
begin
  lMyBuilder := TStringBuilder.Create;
  lLogBuilder := TStringBuilder.Create;
  try
    FD2XLogger.StartLog(lMyBuilder);
    FD2XLogger.Log('Log check 1', [], False);
    CheckString(lMyBuilder, 'Log check 1', 'Log check 1');
    CheckString(lLogBuilder, '', 'Log check 1');

    pLogger := TD2XLogger.Create(lLogBuilder);

    pLogger.Log('Log check 2', [], False);
    CheckString(lLogBuilder, 'Log check 2', 'Log check 2');
    CheckString(lMyBuilder, '', 'Log check 2');

    FD2XLogger.JoinLog(pLogger);
    FD2XLogger.Log('Log simple 1', [], False);
    CheckString(lLogBuilder, 'Log simple 1', 'Log simple 1');
    CheckString(lMyBuilder, '', 'Log simple 1');

    FD2XLogger.JoinLog(nil);
    FD2XLogger.Log('Log simple 2', [], False);
    CheckString(lLogBuilder, '', 'Log simple 2');
    CheckString(lMyBuilder, 'Log simple 2', 'Log simple 2');
  finally
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
    CheckString(lMyBuilder, IfThen(pLog = 0, pLabel, ''), pLabel);
    CheckString(lLogBuilder1, IfThen(pLog = 1, pLabel, ''), pLabel);
    CheckString(lLogBuilder2, IfThen(pLog = 2, pLabel, ''), pLabel);
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
    pLogger1.Lock;
    pLogger1.Log('Log check 1', [], False);
    CheckLogs('Log check 1', 1);

    pLogger2 := TD2XLogger.Create(lLogBuilder2);
    pLogger2.Lock;
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
    pLogger1.Unlock;
    pLogger2.Unlock;
    lLogBuilder2.Free;
    lLogBuilder1.Free;
    lMyBuilder.Free;
  end;
end;

{ TStringBuilderTestCase }

procedure TStringBuilderTestCase.CheckString(pSB: TStringBuilder; pExp, pLabel: string);
begin
  CheckEqualsString(pExp, pSB.ToString, pLabel);
  pSB.Clear;
end;

procedure TStringBuilderTestCase.CheckString(pSS: TStringStream; pExp, pLabel: string);
begin
  CheckEqualsString(pExp, pSS.DataString, pLabel);
  pSS.Clear;
end;

initialization

RegisterTests('Global', [TestTD2X.Suite, TestID2XLogger.Suite, TestTD2XLogger.Suite]);

end.
