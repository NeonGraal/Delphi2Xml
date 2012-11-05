unit D2XHandlersTest;

interface

implementation

uses
  CastaliaPasLexTypes,
  D2X,
  D2XParser,
  D2XHandlers,
  D2XHandler,
  D2Xml,
  D2XTest,
  System.Classes,
  System.Generics.Collections,
  System.StrUtils,
  System.SysUtils,
  TestFramework;

type
  TTestLogHandler = class(TD2XLogHandler)
  public
    property Lexer: TD2XLexer read fLexer;

  end;

  TestTD2XLogHandler = class(TLoggerTestCase)
  strict private
    FD2XLogHandler: TTestLogHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestUseProxy;
    procedure TestInit;
    procedure TestCopy;
    procedure TestBeginMethod;
    procedure TestEndMethod;
    procedure TestLexerInclude;
    procedure TestParserMessage;
  end;

  TestTD2XCountHandler = class(TStringTestCase)
  strict private
    FD2XCountHandler: TD2XCountHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestUseProxy;
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
    procedure TestDescription;
    procedure TestEndProcessing;
    procedure TestBeginFile;
    procedure TestCheckBeforeMethod;
    procedure TestCheckAfterMethod;

    procedure TestProcessing;
  end;

  TTestXmlHandler = class(TD2XXmlHandler)
  public
    property XmlDoc: TD2XmlDoc read fXmlDoc;
    property XmlNode: TD2XmlNode read fXmlNode;

    property Parser: TD2XDefinesParser read fParser;
    property FinalToken: TD2XCheckRef read fFinalToken;
    property ParseMode: TD2XStringRef read fParseMode;

  end;

  TestTD2XXmlHandler = class(TStringTestCase)
  strict private
    FD2XXmlHandler: TTestXmlHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestUseProxy;
    procedure TestInit;
    procedure TestCopy;
    procedure TestBeginResults;
    procedure TestEndResults;
    procedure TestBeginMethod;
    procedure TestEndMethod;
    procedure TestAddAttr;
    procedure TestAddText;
    procedure TestRollbackTo;
    procedure TestLexerInclude;
    procedure TestParserMessage;

    procedure TestProcessing;
  end;

  { TestTD2XLogHandler }

procedure TestTD2XLogHandler.SetUp;
begin
  inherited;

  FD2XLogHandler := TTestLogHandler.Create;
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

procedure TestTD2XLogHandler.TestLexerInclude;
begin
  FD2XLogHandler.L.JoinLog(fID2XLogger);

  FD2XLogHandler.LexerInclude('Test', 1, 2);

  CheckLog('INCLUDE @ 1,2: Test', 'Lexer Include');
end;

procedure TestTD2XLogHandler.TestParserMessage;
begin
  FD2XLogHandler.L.JoinLog(fID2XLogger);

  FD2XLogHandler.ParserMessage(meNotSupported, 'Test', 1, 2);

  CheckLog('NOT SUPPORTED @ 1,2: Test', 'Begin Method');

  FD2XLogHandler.ParserMessage(meError, 'Test', 1, 2);

  CheckLog('ERROR @ 1,2: Test', 'Begin Method');
end;

procedure TestTD2XLogHandler.TestUseProxy;
begin
  CheckTrue(FD2XLogHandler.UseProxy, 'Uses proxy');
end;

procedure TestTD2XLogHandler.TestCopy;
var
  pFrom: TD2XLogHandler;
  pLexer: TD2XLexer;
begin
  pFrom := nil;
  pLexer := nil;

  FD2XLogHandler.Copy(pFrom);
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

procedure TestTD2XLogHandler.TestDescription;
begin
  CheckEqualsString('Verbose Logging', FD2XLogHandler.Description, 'Description');
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

procedure TestTD2XCountHandler.TestUseProxy;
begin
  CheckTrue(FD2XCountHandler.UseProxy, 'Uses proxy');
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

procedure TestTD2XCountHandler.TestDescription;
begin
  CheckEqualsString('Count Children', FD2XCountHandler.Description, 'Description');
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

procedure TestTD2XSkipHandler.TestDescription;
begin
  CheckEqualsString('Skip Methods', FD2XSkipHandler.Description, 'Description');
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

{ TestTD2XXmlHandler }

procedure TestTD2XXmlHandler.SetUp;
begin
  inherited;

  FD2XXmlHandler := TTestXmlHandler.Create;
end;

procedure TestTD2XXmlHandler.TearDown;
begin
  FD2XXmlHandler.Free;
  FD2XXmlHandler := nil;

  inherited;
end;

procedure TestTD2XXmlHandler.TestAddAttr;
begin
  FD2XXmlHandler.BeginResults;
  FD2XXmlHandler.HasFiles := True;
  FD2XXmlHandler.BeginMethod('Test');
  FD2XXmlHandler.AddAttr('Test', 'Test');
  FD2XXmlHandler.EndResults(
    function: TStream
    begin
      Result := fSS;
    end);
  CheckStream('<?xml version="1.0"?> <Test Test="Test" />', 'End Results');
end;

procedure TestTD2XXmlHandler.TestAddText;
begin
  FD2XXmlHandler.BeginResults;
  FD2XXmlHandler.HasFiles := True;
  FD2XXmlHandler.BeginMethod('Test');
  FD2XXmlHandler.AddText('Test');
  FD2XXmlHandler.EndResults(
    function: TStream
    begin
      Result := fSS;
    end);
  CheckStream('<?xml version="1.0"?> <Test>Test</Test>', 'End Results');
end;

procedure TestTD2XXmlHandler.TestBeginMethod;
begin
  FD2XXmlHandler.BeginResults;
  FD2XXmlHandler.HasFiles := True;
  FD2XXmlHandler.BeginMethod('Test');
  FD2XXmlHandler.EndResults(
    function: TStream
    begin
      Result := fSS;
    end);
  CheckStream('<?xml version="1.0"?> <Test />', 'End Results');
end;

procedure TestTD2XXmlHandler.TestBeginResults;
begin
  FD2XXmlHandler.HasFiles := True;

  CheckTrue(FD2XXmlHandler.HasFiles, 'Has files set');

  FD2XXmlHandler.BeginResults;
  CheckFalse(FD2XXmlHandler.HasFiles, 'Has files reset');
end;

procedure TestTD2XXmlHandler.TestCopy;
var
  pFrom: TD2XXmlHandler;
  pParser: TD2XDefinesParser;
  lCalledFinalToken, lCalledParseMode: Boolean;
begin
  pFrom := nil;
  pParser := nil;
  lCalledFinalToken := False;
  lCalledParseMode := False;

  FD2XXmlHandler.Copy(pFrom);
  try
    pFrom := TD2XXmlHandler.Create;
    pParser := TD2XDefinesParser.Create;

    pFrom.Init(pParser,
      function: Boolean
      begin
        lCalledFinalToken := True;
        Result := True;
      end,
      function: string
      begin
        lCalledParseMode := True;
        Result := 'ParseMode';
      end);

    CheckFalse(Assigned(FD2XXmlHandler.Parser), 'Parser not set');
    CheckFalse(Assigned(FD2XXmlHandler.FinalToken), 'Final Token not set');
    CheckFalse(Assigned(FD2XXmlHandler.ParseMode), 'Parse Mode not set');

    FD2XXmlHandler.Copy(pFrom);

    Check(pParser = FD2XXmlHandler.Parser, 'Parser set');
    CheckTrue(Assigned(FD2XXmlHandler.FinalToken), 'Final Token set');
    CheckTrue(FD2XXmlHandler.FinalToken(), 'Final Token correct');
    CheckTrue(lCalledFinalToken, 'Final Token called');
    CheckTrue(Assigned(FD2XXmlHandler.ParseMode), 'Parse Mode set');
    CheckEqualsString('ParseMode', FD2XXmlHandler.ParseMode(), 'Parse Mode correct');
    CheckTrue(lCalledParseMode, 'Parse Mode called');
  finally
    pParser.Free;
    pFrom.Free;
  end;
end;

procedure TestTD2XXmlHandler.TestDescription;
begin
  CheckEqualsString('Xml', FD2XXmlHandler.Description, 'Description');
end;

procedure TestTD2XXmlHandler.TestEndMethod;
begin
  FD2XXmlHandler.BeginResults;
  FD2XXmlHandler.HasFiles := True;
  FD2XXmlHandler.BeginMethod('Test');
  FD2XXmlHandler.EndMethod('Test');
  FD2XXmlHandler.EndResults(
    function: TStream
    begin
      Result := fSS;
    end);
  CheckStream('<?xml version="1.0"?> <Test />', 'End Results');
end;

procedure TestTD2XXmlHandler.TestEndResults;
begin
  FD2XXmlHandler.BeginResults;
  FD2XXmlHandler.EndResults(
    function: TStream
    begin
      Result := fSS;
    end);
  CheckStream('', 'End Results');

  FD2XXmlHandler.BeginResults;
  FD2XXmlHandler.HasFiles := True;
  FD2XXmlHandler.EndResults(
    function: TStream
    begin
      Result := fSS;
    end);
  CheckStream('<?xml version="1.0"?>', 'End Results');
end;

procedure TestTD2XXmlHandler.TestInit;
var
  lParser: TD2XDefinesParser;
  lCalledFinalToken, lCalledParseMode: Boolean;
begin
  lCalledFinalToken := False;
  lCalledParseMode := False;
  lParser := TD2XDefinesParser.Create;
  try
    FD2XXmlHandler.Init(lParser,
      function: Boolean
      begin
        lCalledFinalToken := True;
        Result := True;
      end,
      function: string
      begin
        lCalledParseMode := True;
        Result := 'ParseMode';
      end);

    Check(lParser = FD2XXmlHandler.Parser, 'Parser set');
    CheckTrue(Assigned(FD2XXmlHandler.FinalToken), 'Final Token set');
    CheckTrue(FD2XXmlHandler.FinalToken(), 'Final Token correct');
    CheckTrue(lCalledFinalToken, 'Final Token called');
    CheckTrue(Assigned(FD2XXmlHandler.Parser), 'Parse Mode set');
    CheckEqualsString('ParseMode', FD2XXmlHandler.ParseMode(), 'Parse Mode correct');
    CheckTrue(lCalledParseMode, 'Parse Mode called');
  finally
    lParser.Free;
  end;
end;

procedure TestTD2XXmlHandler.TestLexerInclude;
begin
  FD2XXmlHandler.BeginResults;
  FD2XXmlHandler.HasFiles := True;
  FD2XXmlHandler.BeginMethod('Test');

  FD2XXmlHandler.LexerInclude('Test', 1, 2);

  FD2XXmlHandler.EndResults(
    function: TStream
    begin
      Result := fSS;
    end);
  CheckStream('<?xml version="1.0"?> <Test> <IncludeFile filename="Test" msgAt="1,2" /> </Test>', 'End Results');
end;

procedure TestTD2XXmlHandler.TestParserMessage;
begin
  FD2XXmlHandler.BeginResults;
  FD2XXmlHandler.HasFiles := True;
  FD2XXmlHandler.BeginMethod('Test');

  FD2XXmlHandler.ParserMessage(meNotSupported, 'Test', 1, 2);

  FD2XXmlHandler.EndResults(
    function: TStream
    begin
      Result := fSS;
    end);
  CheckStream('<?xml version="1.0"?> <Test> <D2X_notSuppMsg msgAt="1,2">Test</D2X_notSuppMsg> </Test>', 'End Results');

  FD2XXmlHandler.BeginResults;
  FD2XXmlHandler.HasFiles := True;
  FD2XXmlHandler.BeginMethod('Test');
  FD2XXmlHandler.ParserMessage(meError, 'Test', 1, 2);
  FD2XXmlHandler.EndResults(
    function: TStream
    begin
      Result := fSS;
    end);
  CheckStream('<?xml version="1.0"?> <Test> <D2X_errorMsg msgAt="1,2">Test</D2X_errorMsg> </Test>', 'End Results');
end;

procedure TestTD2XXmlHandler.TestProcessing;
begin
  FD2XXmlHandler.BeginResults;
  FD2XXmlHandler.HasFiles := True;
  FD2XXmlHandler.BeginMethod('Test');
  FD2XXmlHandler.BeginMethod('Test1');
  FD2XXmlHandler.AddAttr('Test', 'Test');
  FD2XXmlHandler.BeginMethod('Test2');
  FD2XXmlHandler.AddText('Test');
  FD2XXmlHandler.EndMethod('Test2');
  FD2XXmlHandler.BeginMethod('Test3');
  FD2XXmlHandler.AddAttr('Test', 'Test');
  FD2XXmlHandler.RollbackTo('Test1');
  FD2XXmlHandler.BeginMethod('Test4');
  FD2XXmlHandler.EndResults(
    function: TStream
    begin
      Result := fSS;
    end);
  CheckStream('<?xml version="1.0"?> <Test> <Test1 Test="Test"> <Test2>Test</Test2> <Test3 Test="Test" /> <Test4 /> </Test1> </Test>', 'End Results');
end;

procedure TestTD2XXmlHandler.TestRollbackTo;
begin
  FD2XXmlHandler.BeginResults;
  FD2XXmlHandler.HasFiles := True;
  FD2XXmlHandler.BeginMethod('Test');
  FD2XXmlHandler.BeginMethod('Test1');
  FD2XXmlHandler.BeginMethod('Test2');
  FD2XXmlHandler.BeginMethod('Test3');
  FD2XXmlHandler.RollbackTo('Test1');
  FD2XXmlHandler.BeginMethod('Test4');
  FD2XXmlHandler.EndResults(
    function: TStream
    begin
      Result := fSS;
    end);
  CheckStream('<?xml version="1.0"?> <Test> <Test1> <Test2> <Test3 /> </Test2> <Test4 /> </Test1> </Test>', 'End Results');
end;

procedure TestTD2XXmlHandler.TestUseProxy;
begin
  CheckTrue(FD2XXmlHandler.UseProxy, 'Uses proxy');
end;

initialization

// Register any test cases with the test runner
RegisterTests('Handlers', [TestTD2XLogHandler.Suite, TestTD2XCountHandler.Suite,
  TestTD2XSkipHandler.Suite, TestTD2XXmlHandler.Suite]);

end.
