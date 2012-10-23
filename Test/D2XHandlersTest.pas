unit D2XHandlersTest;

interface

implementation

uses
  D2X,
  D2XParser,
  D2XHandlers,
  D2XHandler,
  D2XTest,
  System.Classes,
  System.Generics.Collections,
  TestFramework;

type
  TestTD2XLogHandler = class(TLoggerTestCase)
  strict private
    FD2XLogHandler: TD2XLogHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInit;
    procedure TestCopy;
    procedure TestBeginMethod;
    procedure TestEndMethod;
  end;

  TestTD2XCountHandler = class(TStringTestCase)
  strict private
    FD2XCountHandler: TD2XCountHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEndProcessing;
    procedure TestBeginFile;
    procedure TestEndFile;
    procedure TestBeginMethod;
    procedure TestEndMethod;
  end;

  { TestTD2XLogHandler }

procedure TestTD2XLogHandler.SetUp;
begin
  inherited;

  FD2XLogHandler := TD2XLogHandler.Create;
end;

procedure TestTD2XLogHandler.TearDown;
begin
  FD2XLogHandler.Free;
  FD2XLogHandler := nil;

  inherited;
end;

procedure TestTD2XLogHandler.TestInit;
var
  pLexer: TD2XLexer;
begin
  pLexer := TD2XLexer.Create;
  try
    FD2XLogHandler.Init(pLexer);

    Check(pLexer = FD2XLogHandler.Lexer, 'Lexer set');
  finally
    pLexer.Free;
  end;
end;

procedure TestTD2XLogHandler.TestCopy;
var
  pFrom: TD2XLogHandler;
  pLexer: TD2XLexer;
begin
  pFrom := nil;
  pLexer := nil;
  try
    pFrom := TD2XLogHandler.Create;
    pLexer := TD2XLexer.Create;
    pFrom.Init(pLexer);
    pFrom.L.JoinLog(fID2XLogger);

    CheckFalse(Assigned(FD2XLogHandler.Lexer), 'Lexer not set');
    FD2XLogHandler.L.Log('Log Simple 1', []);
    CheckLog('', 'Log Simple 1');

    FD2XLogHandler.Copy(pFrom);
    Check(pLexer = FD2XLogHandler.Lexer, 'Lexer set');

    FD2XLogHandler.L.Log('Log Simple 2', []);
    CheckLog('Log Simple 2', 'Log Simple 2');
  finally
    pLexer.Free;
    pFrom.Free;
  end;
end;

procedure TestTD2XLogHandler.TestBeginMethod;
begin
  FD2XLogHandler.L.JoinLog(fID2XLogger);

  FD2XLogHandler.BeginMethod('Method');

  CheckLog('BEFORE Method', 'Begin Method');
end;

procedure TestTD2XLogHandler.TestEndMethod;
begin
  FD2XLogHandler.L.JoinLog(fID2XLogger);

  FD2XLogHandler.EndMethod('Method');

  CheckLog('AFTER Method', 'End Method');
end;

{ TestTD2XCountHandler }

procedure TestTD2XCountHandler.SetUp;
begin
  inherited;

  FD2XCountHandler := TD2XCountHandler.Create;
end;

procedure TestTD2XCountHandler.TearDown;
begin
  FD2XCountHandler.Free;
  FD2XCountHandler := nil;

  inherited;
end;

procedure TestTD2XCountHandler.TestEndProcessing;
begin
  FD2XCountHandler.EndProcessing(function: TStream begin Result := fSS; end);

  CheckStream('', 'End Processing');
end;

procedure TestTD2XCountHandler.TestBeginFile;
begin
  FD2XCountHandler.BeginFile;

  CheckStream('', 'Begin File');
end;

procedure TestTD2XCountHandler.TestEndFile;
begin
  FD2XCountHandler.EndFile(function: TStream begin Result := fSS; end);

  CheckStream('', 'End File');
end;

procedure TestTD2XCountHandler.TestBeginMethod;
var
  pMethod: string;
begin
  FD2XCountHandler.BeginFile;

  FD2XCountHandler.BeginMethod(pMethod);

  CheckStream('', 'Begin Method');
end;

procedure TestTD2XCountHandler.TestEndMethod;
var
  pMethod: string;
begin
  FD2XCountHandler.BeginFile;

  FD2XCountHandler.EndMethod(pMethod);

  CheckStream('', 'End Method');
end;

initialization

// Register any test cases with the test runner
RegisterTests('Handlers', [TestTD2XLogHandler.Suite, TestTD2XCountHandler.Suite]);

end.
