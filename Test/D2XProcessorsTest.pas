unit D2XProcessorsTest;

interface

implementation

uses
  TestFramework,
  CastaliaPasLexTypes,
  D2XTest,
  D2XHandler,
  D2XHandlerTest,
  D2XParam,
  D2XParser,
  D2XProcessors,
  D2XProcessorTest,
  System.SysUtils;

type
  TestTD2XLogProcessor = class(TLoggerTestCase)
  strict private
    FD2XProcessor: TD2XLogProcessor;

    fActive: IParamFlag;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetLexer;
    procedure TestUseProxy;
    procedure TestBeginMethod;
    procedure TestEndMethod;
    procedure TestLexerInclude;
    procedure TestParserMessage;
  end;

  TestTD2XHandlerProcessor = class(TStringTestCase)
  strict private
    FHandler: TD2XHandlerTester;
    FD2XProcessor: TD2XHandlerProcessor;

    fActive: IParamFlag;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetProcessingInput;
    procedure TestSetProcessingOutput;
    procedure TestSetResultsOutput;
    procedure TestSetFileInput;
    procedure TestSetFileOutput;

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

    procedure TestLexerInclude;
    procedure TestParserMessage;
  end;

  { TestTD2XHandlerProcessor }

procedure TestTD2XHandlerProcessor.SetUp;
begin
  inherited;

  FHandler := TD2XHandlerTester.Create;
  fActive := TTestBoolFlag.Create;
  FD2XProcessor := TD2XHandlerProcessor.CreateHandler(fActive, FHandler);
end;

procedure TestTD2XHandlerProcessor.TearDown;
begin
  FD2XProcessor.Free;
  FD2XProcessor := nil;
  FHandler.Free;
  FHandler := nil;

  inherited;
end;

procedure TestTD2XHandlerProcessor.TestBeginFile;
begin
  FD2XProcessor.SetFileInput(
      function: string
    begin
      Result := '';
    end);

  FD2XProcessor.BeginFile;
  CheckFalse(FHandler.CalledBeginFile, 'Ignored Begin File');

  fActive.Flag := True;
  FD2XProcessor.BeginFile;
  CheckTrue(FHandler.CalledBeginFile, 'Called Begin File');
end;

procedure TestTD2XHandlerProcessor.TestBeginMethod;
begin
  FD2XProcessor.BeginMethod('');
  CheckFalse(FHandler.CalledBeginMethod, 'Ignored Begin Method');

  fActive.Flag := True;
  FD2XProcessor.BeginMethod('');
  CheckTrue(FHandler.CalledBeginMethod, 'Called Begin Method');
end;

procedure TestTD2XHandlerProcessor.TestBeginProcessing;
begin
  FD2XProcessor.SetProcessingInput(
    function: string
    begin
      Result := '';
    end);

  FD2XProcessor.BeginProcessing;
  CheckFalse(FHandler.CalledBeginProcessing, 'Ignored Begin Processing');

  fActive.Flag := True;
  FD2XProcessor.BeginProcessing;
  CheckTrue(FHandler.CalledBeginProcessing, 'Called Begin Processing');
end;

procedure TestTD2XHandlerProcessor.TestBeginResults;
begin
  FD2XProcessor.BeginResults;
  CheckFalse(FHandler.CalledBeginResults, 'Ignored Begin Results');

  fActive.Flag := True;
  FD2XProcessor.BeginResults;
  CheckTrue(FHandler.CalledBeginResults, 'Called Begin Results');
end;

procedure TestTD2XHandlerProcessor.TestCheckAfterMethod;
begin
  FD2XProcessor.CheckAfterMethod('');
  CheckFalse(FHandler.CalledCheckAfterMethod, 'Ignored Check After Method');

  fActive.Flag := True;
  FD2XProcessor.CheckAfterMethod('');
  CheckTrue(FHandler.CalledCheckAfterMethod, 'Called Check After Method');
end;

procedure TestTD2XHandlerProcessor.TestCheckBeforeMethod;
begin
  FD2XProcessor.CheckBeforeMethod('');
  CheckFalse(FHandler.CalledCheckBeforeMethod, 'Ignored Check Before Method');

  fActive.Flag := True;
  FD2XProcessor.CheckBeforeMethod('');
  CheckTrue(FHandler.CalledCheckBeforeMethod, 'Called Check Before Method');
end;

procedure TestTD2XHandlerProcessor.TestEndFile;
begin
  FD2XProcessor.SetFileOutput(
    function: string
    begin
      Result := '';
    end);

  FD2XProcessor.EndFile;
  CheckFalse(FHandler.CalledEndFile, 'Ignored End File');

  fActive.Flag := True;
  FD2XProcessor.EndFile;
  CheckTrue(FHandler.CalledEndFile, 'Called End File');
end;

procedure TestTD2XHandlerProcessor.TestEndMethod;
begin
  FD2XProcessor.EndMethod('');
  CheckFalse(FHandler.CalledEndMethod, 'Ignored End Method');

  fActive.Flag := True;
  FD2XProcessor.EndMethod('');
  CheckTrue(FHandler.CalledEndMethod, 'Called End Method');
end;

procedure TestTD2XHandlerProcessor.TestEndProcessing;
begin
  FD2XProcessor.SetProcessingOutput(
    function: string
    begin
      Result := '';
    end);

  FD2XProcessor.EndProcessing;
  CheckFalse(FHandler.CalledEndProcessing, 'Ignored End Processing');

  fActive.Flag := True;
  FD2XProcessor.EndProcessing;
  CheckTrue(FHandler.CalledEndProcessing, 'Called End Processing');
end;

procedure TestTD2XHandlerProcessor.TestEndResults;
begin
  FD2XProcessor.SetResultsOutput(
    function(pFile: string): string
    begin
      Result := '';
    end);

  FD2XProcessor.EndResults('');
  CheckFalse(FHandler.CalledEndResults, 'Ignored End Results');

  fActive.Flag := True;
  FD2XProcessor.EndResults('');
  CheckTrue(FHandler.CalledEndResults, 'Called End Results');
end;

procedure TestTD2XHandlerProcessor.TestLexerInclude;
begin
  FD2XProcessor.LexerInclude('Test', 1, 2);
  CheckFalse(FHandler.CalledLexerInclude, 'Ignored Lexer Include');

  fActive.Flag := True;
  FD2XProcessor.LexerInclude('Test', 1, 2);
  CheckTrue(FHandler.CalledLexerInclude, 'Called Lexer Include');
end;

procedure TestTD2XHandlerProcessor.TestParserMessage;
begin
  FD2XProcessor.ParserMessage(meNotSupported, 'Test', 1, 2);
  CheckFalse(FHandler.CalledParserMessage, 'Ignored Parser Message');

  fActive.Flag := True;
  FD2XProcessor.ParserMessage(meNotSupported, 'Test', 1, 2);
  CheckTrue(FHandler.CalledParserMessage, 'Called Parser Message');
end;

procedure TestTD2XHandlerProcessor.TestSetFileInput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;

  FD2XProcessor.SetFileInput(
    function: string
    begin
      lCalled := True;
      Result := '';
    end);

  FD2XProcessor.BeginFile;
  CheckFalse(lCalled, 'Ignored File Input');

  fActive.Flag := True;
  FD2XProcessor.BeginFile;
  CheckTrue(lCalled, 'Called File Input');
end;

procedure TestTD2XHandlerProcessor.TestSetFileOutput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;

  FD2XProcessor.SetFileOutput(
    function: string
    begin
      lCalled := True;
      Result := '';
    end);

  FD2XProcessor.EndFile;
  CheckFalse(lCalled, 'Ignored File Output');

  fActive.Flag := True;
  FD2XProcessor.EndFile;
  CheckTrue(lCalled, 'Called File Output');
end;

procedure TestTD2XHandlerProcessor.TestSetProcessingInput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;
  FD2XProcessor.SetProcessingInput(
    function: string
    begin
      lCalled := True;
      Result := '';
    end);

  FD2XProcessor.BeginProcessing;
  CheckFalse(lCalled, 'Ignored Processing Input');

  fActive.Flag := True;
  FD2XProcessor.BeginProcessing;
  CheckTrue(lCalled, 'Called Processing Input');
end;

procedure TestTD2XHandlerProcessor.TestSetProcessingOutput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;
  FD2XProcessor.SetProcessingOutput(
    function: string
    begin
      lCalled := True;
      Result := '';
    end);

  FD2XProcessor.EndProcessing;
  CheckFalse(lCalled, 'Ignored Processing Output');

  fActive.Flag := True;
  FD2XProcessor.EndProcessing;
  CheckTrue(lCalled, 'Called Processing Output');
end;

procedure TestTD2XHandlerProcessor.TestSetResultsOutput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;
  FD2XProcessor.SetResultsOutput(
    function(pFile: string): string
    begin
      lCalled := True;
      Result := '';
    end);

  FD2XProcessor.EndResults('');
  CheckFalse(lCalled, 'Ignored Results Output');

  fActive.Flag := True;
  FD2XProcessor.EndResults('');
  CheckTrue(lCalled, 'Called Results Output');
end;

{ TestTD2XLogProcessor }

procedure TestTD2XLogProcessor.SetUp;
begin
  inherited;

  fActive := TTestBoolFlag.Create;

  FD2XProcessor := TD2XLogProcessor.Create(fActive);
end;

procedure TestTD2XLogProcessor.TearDown;
begin
  FreeAndNil(FD2XProcessor);

  inherited;
end;

procedure TestTD2XLogProcessor.TestBeginMethod;
begin
  FD2XProcessor.JoinLog(fID2XLogger);

  FD2XProcessor.BeginMethod('Method');
  CheckLog('', 'Ignored Begin Method');

  fActive.Flag := True;
  FD2XProcessor.BeginMethod('Method');
  CheckLog('BEFORE Method', 'Begin Method');
end;

procedure TestTD2XLogProcessor.TestEndMethod;
begin
  FD2XProcessor.JoinLog(fID2XLogger);

  FD2XProcessor.EndMethod('Method');
  CheckLog('', 'Ignored End Method');

  fActive.Flag := True;
  FD2XProcessor.EndMethod('Method');
  CheckLog('AFTER Method', 'End Method');
end;

procedure TestTD2XLogProcessor.TestLexerInclude;
begin
  FD2XProcessor.JoinLog(fID2XLogger);

  FD2XProcessor.LexerInclude('Test', 1, 2);
  CheckLog('', 'Ignored Lexer Include');

  fActive.Flag := True;
  FD2XProcessor.LexerInclude('Test', 1, 2);
  CheckLog('INCLUDE @ 1,2: Test', 'Lexer Include');
end;

procedure TestTD2XLogProcessor.TestParserMessage;
begin
  FD2XProcessor.JoinLog(fID2XLogger);

  FD2XProcessor.ParserMessage(meNotSupported, 'Test', 1, 2);
  CheckLog('', 'Ignored Parser Message');

  fActive.Flag := True;
  FD2XProcessor.ParserMessage(meNotSupported, 'Test', 1, 2);
  CheckLog('NOT SUPPORTED @ 1,2: Test', 'Not Supported Parser Message');

  FD2XProcessor.ParserMessage(meError, 'Test', 1, 2);
  CheckLog('ERROR @ 1,2: Test', 'Error Parser Message');
end;

procedure TestTD2XLogProcessor.TestSetLexer;
var
  lLexer: TD2XLexer;
begin
  FD2XProcessor.JoinLog(fID2XLogger);
  fActive.Flag := True;

  lLexer := TD2XLexer.Create;
  try
    FD2XProcessor.SetLexer(lLexer);
    FD2XProcessor.BeginMethod('Method');
    CheckLog('BEFORE Method @', 'Begin Method');
  finally
    lLexer.Free;
  end;
end;

procedure TestTD2XLogProcessor.TestUseProxy;
begin
  CheckFalse(FD2XProcessor.UseProxy, 'Ignored Use Proxy');

  fActive.Flag := True;
  CheckTrue(FD2XProcessor.UseProxy, 'Allowed Use Proxy');
end;

initialization

RegisterTests('Processor', [TestTD2XLogProcessor.Suite, TestTD2XHandlerProcessor.Suite]);

end.
