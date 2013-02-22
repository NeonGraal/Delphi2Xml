unit Test.Handler.Tests;

interface

implementation

uses
  CastaliaPasLexTypes,
  D2X.Handler,
  System.SysUtils,
  Test.Handler,
  TestFramework;

type
  TestTD2XHandler = class(TTestCase)
  protected
    FD2XHandler: TD2XHandler;
    FD2XTester: TD2XHandlerTester;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestUseProxy;
    procedure TestEndProcessing;
    procedure TestBeginFile;
    procedure TestEndFile;
    procedure TestBeginResults;
    procedure TestEndResults;
    procedure TestCheckBeforeMethod;
    procedure TestCheckAfterMethod;
    procedure TestBeginMethod;
    procedure TestEndMethod;
    procedure TestParserMessage;
    procedure TestLexerInclude;
  end;

  { TestTD2XHandler }

procedure TestTD2XHandler.SetUp;
begin
  FD2XTester := TD2XHandlerTester.Create;
  FD2XHandler := FD2XTester;
end;

procedure TestTD2XHandler.TearDown;
begin
  FreeAndNil(FD2XHandler);
end;

procedure TestTD2XHandler.TestEndProcessing;
begin
  (FD2XHandler as ID2XProcessingHandler).EndProcessing(nil);

  CheckTrue(FD2XTester.CalledEndProcessing, 'Called End Processing');
end;

procedure TestTD2XHandler.TestBeginResults;
begin
  (FD2XHandler as ID2XResultsHandler).BeginResults;

  CheckTrue(FD2XTester.CalledBeginResults, 'Called Begin Results');
end;

procedure TestTD2XHandler.TestCheckAfterMethod;
begin
  (FD2XHandler as ID2XCheckHandler).CheckAfterMethod('');

  CheckTrue(FD2XTester.CalledCheckAfterMethod, 'Called Check After Method');
end;

procedure TestTD2XHandler.TestCheckBeforeMethod;
begin
  (FD2XHandler as ID2XCheckHandler).CheckBeforeMethod('');

  CheckTrue(FD2XTester.CalledCheckBeforeMethod, 'Called Check Before Method');
end;

procedure TestTD2XHandler.TestDescription;
begin
  CheckEqualsString('Handler Tester', FD2XHandler.Description, 'Description');
end;

procedure TestTD2XHandler.TestBeginMethod;
begin
  (FD2XHandler as ID2XMethodHandler).BeginMethod('');

  CheckTrue(FD2XTester.CalledBeginMethod, 'Called Begin Method');
end;

procedure TestTD2XHandler.TestEndMethod;
begin
  (FD2XHandler as ID2XMethodHandler).EndMethod('');

  CheckTrue(FD2XTester.CalledEndMethod, 'Called End Method');
end;

procedure TestTD2XHandler.TestEndResults;
begin
  (FD2XHandler as ID2XResultsHandler).EndResults(nil);

  CheckTrue(FD2XTester.CalledEndResults, 'Called End Results');
end;

procedure TestTD2XHandler.TestLexerInclude;
begin
  (FD2XHandler as ID2XMessagesHandler).LexerInclude('', 0, 0);

  CheckTrue(FD2XTester.CalledLexerInclude, 'Called Lexer Include');
end;

procedure TestTD2XHandler.TestParserMessage;
begin
  (FD2XHandler as ID2XMessagesHandler).ParserMessage(meError, '', 0, 0);

  CheckTrue(FD2XTester.CalledParserMessage, 'Called Parser Message');
end;

procedure TestTD2XHandler.TestUseProxy;
begin
  CheckFalse(FD2XHandler.UseProxy, 'Use Proxy');
end;

procedure TestTD2XHandler.TestBeginFile;
begin
  (FD2XHandler as ID2XFileHandler).BeginFile('', nil);

  CheckTrue(FD2XTester.CalledBeginFile, 'Called Begin File');
end;

procedure TestTD2XHandler.TestEndFile;
begin
  (FD2XHandler as ID2XFileHandler).EndFile('', nil);

  CheckTrue(FD2XTester.CalledEndFile, 'Called End File');
end;

initialization

// Register any test cases with the test runner
RegisterTests('Handler', [TestTD2XHandler.Suite]);

end.
