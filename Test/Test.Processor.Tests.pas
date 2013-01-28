unit Test.Processor.Tests;

interface

implementation

uses
  CastaliaPasLexTypes,
  D2X.Xml,
  D2X.Handler,
  D2X.Options,
  D2X.Param,
  D2X.Parser,
  D2X.Processor,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.SysUtils,
  System.Rtti,
  Test.Global,
  Test.Handler,
  Test.Processor,
  TestFramework;

type
  TestTD2XProcessor = class(TFlagTestCase)
  strict private
    FD2XProcessor: TTestProcessor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInvalidCreate;
    procedure TestUseProxy;
    procedure TestSetParser;
    procedure TestBeginProcessing;
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

  { TestTD2XProcessor }

procedure TestTD2XProcessor.SetUp;
begin
  inherited;

  FD2XProcessor := TTestProcessor.CreateActive(fActive);
end;

procedure TestTD2XProcessor.TearDown;
begin
  FreeAndNil(FD2XProcessor);

  inherited;
end;

procedure TestTD2XProcessor.TestBeginFile;
begin
  FD2XProcessor.BeginFile('');

  CheckTrue(FD2XProcessor.CalledBeginFile, 'Called Begin File');
end;

procedure TestTD2XProcessor.TestBeginMethod;
begin
  FD2XProcessor.BeginMethod('Test');

  CheckTrue(FD2XProcessor.CalledBeginMethod, 'Called Begin Method');
end;

procedure TestTD2XProcessor.TestBeginProcessing;
begin
  FD2XProcessor.BeginProcessing;

  CheckTrue(FD2XProcessor.CalledBeginProcessing, 'Called Begin Processing');
end;

procedure TestTD2XProcessor.TestBeginResults;
begin
  FD2XProcessor.BeginResults;

  CheckTrue(FD2XProcessor.CalledBeginResults, 'Called Begin Results');
end;

procedure TestTD2XProcessor.TestCheckAfterMethod;
begin
  CheckTrue(FD2XProcessor.CheckAfterMethod('Test'), 'Call Check After Method');

  CheckTrue(FD2XProcessor.CalledCheckAfterMethod, 'Called Check After Method');
end;

procedure TestTD2XProcessor.TestCheckBeforeMethod;
begin
  CheckTrue(FD2XProcessor.CheckBeforeMethod('Test'), 'Call Check Before Method');

  CheckTrue(FD2XProcessor.CalledCheckBeforeMethod, 'Called Check Before Method');
end;

procedure TestTD2XProcessor.TestEndFile;
begin
  FD2XProcessor.EndFile('');

  CheckTrue(FD2XProcessor.CalledEndFile, 'Called End File');
end;

procedure TestTD2XProcessor.TestEndMethod;
begin
  FD2XProcessor.EndMethod('Test');

  CheckTrue(FD2XProcessor.CalledEndMethod, 'Called End Method');
end;

procedure TestTD2XProcessor.TestEndProcessing;
begin
  FD2XProcessor.EndProcessing;

  CheckTrue(FD2XProcessor.CalledEndProcessing, 'Called End Processing');
end;

procedure TestTD2XProcessor.TestEndResults;
begin
  FD2XProcessor.EndResults('Test');

  CheckTrue(FD2XProcessor.CalledEndResults, 'Called End Results');
end;

procedure TestTD2XProcessor.TestInvalidCreate;
begin
  StartExpectingException(EInvalidParam);
  try
    TTestProcessor.Create;
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XProcessor.TestLexerInclude;
begin
  FD2XProcessor.LexerInclude('Test', 0, 0);

  CheckTrue(FD2XProcessor.CalledLexerInclude, 'Called Lexer Include');
end;

procedure TestTD2XProcessor.TestParserMessage;
begin
  FD2XProcessor.ParserMessage(meNotSupported, 'Test', 0, 0);

  CheckTrue(FD2XProcessor.CalledParserMessage, 'Called Parser Message');
end;

procedure TestTD2XProcessor.TestSetParser;
begin
  FD2XProcessor.SetParser(nil);

  CheckTrue(FD2XProcessor.CalledSetParser, 'Called Set Parser');
end;

procedure TestTD2XProcessor.TestUseProxy;
begin
  CheckFalse(FD2XProcessor.UseProxy, 'Call Use Proxy');

  CheckTrue(FD2XProcessor.CalledUseProxy, 'Called Use Proxy');
end;

initialization

// Register any test cases with the test runner
RegisterTests('Processor', [TestTD2XProcessor.Suite]);

end.
