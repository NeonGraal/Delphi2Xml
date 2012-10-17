unit D2XHandlersTest;

interface

implementation

uses
  D2X,
  D2XHandlers,
  D2XHandler,
  D2XParser,
  D2XTest,
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

initialization

// Register any test cases with the test runner
RegisterTests('Handlers', [TestTD2XLogHandler.Suite]);

end.
