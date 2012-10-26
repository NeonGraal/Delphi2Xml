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
  System.StrUtils,
  System.SysUtils,
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

    procedure TestProcessing;
  end;

  TestTD2XSkipHandler = class(TStringTestCase)
  strict private
    FD2XSkipHandler: TD2XSkipHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEndProcessing;
    procedure TestBeginFile;
    procedure TestCheckBeforeMethod;
    procedure TestCheckAfterMethod;

    procedure TestProcessing;
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

procedure TestTD2XCountHandler.TestProcessing;
begin
  FD2XCountHandler.BeginFile(nil);
  FD2XCountHandler.BeginMethod('Alpha');
  FD2XCountHandler.BeginMethod('Beta');
  FD2XCountHandler.EndMethod('Beta');
  FD2XCountHandler.BeginMethod('Gamma');
  FD2XCountHandler.EndMethod('Gamma');
  FD2XCountHandler.EndMethod('Alpha');
  FD2XCountHandler.EndFile(nil);

  FD2XCountHandler.EndProcessing(MakeStream(fSS));

  CheckStream('Alpha=2,2', 'Processing');
end;

procedure TestTD2XCountHandler.TestBeginFile;
begin
  FD2XCountHandler.BeginFile(nil);

  CheckStream('', 'Begin File');
end;

procedure TestTD2XCountHandler.TestEndFile;
var
  lCalled: Boolean;
begin
  lCalled := False;

  FD2XCountHandler.EndFile(
    function: TStream
    begin
      Result := nil;
      lCalled := True;
    end);

  CheckFalse(lCalled, 'Stream Creator called');
  CheckStream('', 'End File');
end;

procedure TestTD2XCountHandler.TestBeginMethod;
var
  pMethod: string;
begin
  FD2XCountHandler.BeginFile(nil);

  FD2XCountHandler.BeginMethod(pMethod);

  CheckStream('', 'Begin Method');
end;

procedure TestTD2XCountHandler.TestEndMethod;
var
  pMethod: string;
begin
  FD2XCountHandler.BeginFile(nil);

  FD2XCountHandler.EndMethod(pMethod);

  CheckStream('', 'End Method');
end;

procedure TestTD2XCountHandler.TestEndProcessing;
var
  lCalled: Boolean;
begin
  lCalled := False;
  StartExpectingException(EAssertionFailed);
  try
    FD2XCountHandler.EndProcessing(
      function: TStream
      begin
        Result := nil;
        lCalled := True;
      end);
  except
    on E: EAssertionFailed do
    begin
      CheckTrue(lCalled, 'Stream Creator called');
      CheckTrue(StartsText('Need a Stream', E.Message), 'Exception message');
      raise;
    end;
  end;
end;

{ TestTD2XSkipHandler }

procedure TestTD2XSkipHandler.SetUp;
begin
  inherited;

  FD2XSkipHandler := TD2XSkipHandler.Create;
end;

procedure TestTD2XSkipHandler.TearDown;
begin
  FD2XSkipHandler.Free;
  FD2XSkipHandler := nil;

  inherited;
end;

procedure TestTD2XSkipHandler.TestBeginFile;
begin
  fSS.WriteString('Alpha=1'#13#10'Gamma');
  FD2XSkipHandler.BeginFile(MakeStream(fSS));
  CheckStream('Alpha=1 Gamma', 'Stream');
end;

procedure TestTD2XSkipHandler.TestCheckBeforeMethod;
begin
  CheckFalse(FD2XSkipHandler.CheckBeforeMethod('Alpha'), 'Check Before Method');
end;

procedure TestTD2XSkipHandler.TestCheckAfterMethod;
begin
  CheckFalse(FD2XSkipHandler.CheckAfterMethod('Alpha'), 'Check After Method');
end;

procedure TestTD2XSkipHandler.TestEndProcessing;
begin
  FD2XSkipHandler.EndProcessing(MakeStream(fSS));
  CheckStream('', 'End Processing');
end;

procedure TestTD2XSkipHandler.TestProcessing;
begin
  fSS.WriteString('Alpha=1'#13#10'Gamma');
  FD2XSkipHandler.BeginFile(MakeStream(fSS));
  CheckStream('Alpha=1 Gamma', 'Stream');

  CheckTrue(FD2XSkipHandler.CheckBeforeMethod('Alpha'), 'Check Before Alpha');
  CheckFalse(FD2XSkipHandler.CheckBeforeMethod('Beta'), 'Check Before Beta');
  CheckTrue(FD2XSkipHandler.CheckBeforeMethod('Gamma'), 'Check Before Gamma');

  CheckTrue(FD2XSkipHandler.CheckAfterMethod('Alpha'), 'Check After Alpha');
  CheckFalse(FD2XSkipHandler.CheckAfterMethod('Beta'), 'Check After Beta');
  CheckTrue(FD2XSkipHandler.CheckAfterMethod('Gamma'), 'Check After Gamma');

  FD2XSkipHandler.EndProcessing(MakeStream(fSS));
  CheckStream('Alpha=1 Gamma=1', 'End Processing');
end;

initialization

// Register any test cases with the test runner
RegisterTests('Handlers', [TestTD2XLogHandler.Suite, TestTD2XCountHandler.Suite,
  TestTD2XSkipHandler.Suite]);

end.
