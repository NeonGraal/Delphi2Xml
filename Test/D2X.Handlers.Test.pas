unit D2X.Handlers.Test;

interface

uses
  D2X.Parser,
  D2X.Test;

type
  TParserTestCase = class(TLoggerTestCase)
  protected
    fParser: TD2XDefinesParser;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  CastaliaPasLexTypes,
  D2X,
  D2X.Handlers,
  D2X.Handler,
  D2X.Xml,
  System.Classes,
  System.Generics.Collections,
  System.StrUtils,
  System.SysUtils,
  TestFramework;

type
  TestTD2XCountHandler = class(TStringTestCase)
  strict private
    fHndlr: TD2XCountHandler;
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

  TestTD2XDefinesUsedHandler = class(TStringTestCase)
  strict private
    fHndlr: TD2XDefinesUsedHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestUseProxy;
    procedure TestEndProcessing;
    procedure TestDefineUsed;

    procedure TestProcessing;
  end;

  TestTD2XErrorHandler = class(TStringTestCase)
  strict private
    fHndlr: TD2XErrorHandler;

    procedure LogMessage(pType, pMsg: string; pX, pY: Integer);

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInvalidCreate;
    procedure TestDescription;
    procedure TestParserMessage;
  end;

  TestTD2XParserDefinesHandler = class(TParserTestCase)
  strict private
    fHndlr: TD2XParserDefinesHandler;
    fDefs: TStringList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestUseProxy;
    procedure TestInit;
    procedure TestCopy;
    procedure TestBeginFile;
  end;

  TestTD2XSkipHandler = class(TStringTestCase)
  strict private
    fHndlr: TD2XSkipHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestUseProxy;
    procedure TestEndProcessing;
    procedure TestBeginFile;
    procedure TestCheckBeforeMethod;
    procedure TestCheckAfterMethod;

    procedure TestProcessing;
  end;

  TestTD2XWriteDefinesHandler = class(TParserTestCase)
  strict private
    fHndlr: TD2XWriteDefinesHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestUseProxy;
    procedure TestInit;
    procedure TestCopy;
    procedure TestEndResults;
  end;

  TTestXmlHandler = class(TD2XXmlHandler)
  public
    property XmlDoc: TD2XmlDoc read fXmlDoc;
    property XmlNode: TD2XmlNode read fXmlNode;

    property Parser: TD2XDefinesParser read fParser;
    property FinalToken: TD2XCheckRef read fFinalToken;
    property ParseMode: TD2XStringRef read fParseMode;

  end;

  TestTD2XXmlHandler = class(TParserTestCase)
  strict private
    fHndlr: TTestXmlHandler;
    fCalledFinalToken: Boolean;
    fCalledParseMode: Boolean;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInvalidCreate;
    procedure TestDescription;
    procedure TestUseProxy;
    procedure TestInitParser;
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

  { TestTD2XCountHandler }

procedure TestTD2XCountHandler.SetUp;
begin
  inherited;

  fHndlr := TD2XCountHandler.Create;
end;

procedure TestTD2XCountHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XCountHandler.TestProcessing;
begin
  fHndlr.BeginFile('',
      function: TStream
    begin
      Result := nil;
    end);
  fHndlr.BeginMethod('Alpha');
  fHndlr.BeginMethod('Beta');
  fHndlr.EndMethod('Beta');
  fHndlr.BeginMethod('Gamma');
  fHndlr.EndMethod('Gamma');
  fHndlr.EndMethod('Alpha');
  fHndlr.EndFile('',
    function: TStream
    begin
      Result := nil;
    end);

  fHndlr.EndProcessing(MakeStream(fSS));

  CheckStream('Alpha=2,2', 'Processing');
end;

procedure TestTD2XCountHandler.TestUseProxy;
begin
  CheckTrue(fHndlr.UseProxy, 'Uses proxy');
end;

procedure TestTD2XCountHandler.TestBeginFile;
var
  lCalled: Boolean;
begin
  lCalled := False;

  fHndlr.BeginFile('',
    function: TStream
    begin
      Result := nil;
      lCalled := True;
    end);

  Check(lCalled, 'Stream Creator called');
  CheckStream('', 'Begin File');
end;

procedure TestTD2XCountHandler.TestEndFile;
var
  lCalled: Boolean;
begin
  lCalled := False;

  fHndlr.EndFile('',
    function: TStream
    begin
      Result := nil;
      lCalled := True;
    end);

  Check(lCalled, 'Stream Creator called');
  CheckStream('', 'End File');
end;

procedure TestTD2XCountHandler.TestBeginMethod;
var
  pMethod: string;
begin
  fHndlr.BeginFile('',
    function: TStream
    begin
      Result := nil;
    end);

  fHndlr.BeginMethod(pMethod);

  CheckStream('', 'Begin Method');
end;

procedure TestTD2XCountHandler.TestDescription;
begin
  CheckEqualsString('Count Children', fHndlr.Description, 'Description');
end;

procedure TestTD2XCountHandler.TestEndMethod;
var
  pMethod: string;
begin
  fHndlr.BeginFile('',
    function: TStream
    begin
      Result := nil;
    end);

  fHndlr.EndMethod(pMethod);

  CheckStream('', 'End Method');
end;

procedure TestTD2XCountHandler.TestEndProcessing;
var
  lCalled: Boolean;
begin
  lCalled := False;
  StartExpectingException(EAssertionFailed);
  try
    fHndlr.EndProcessing(
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

  fHndlr := TD2XSkipHandler.Create;
end;

procedure TestTD2XSkipHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XSkipHandler.TestBeginFile;
begin
  fSS.WriteString('Alpha=1'#13#10'Gamma');
  fHndlr.BeginFile('', MakeStream(fSS));
  CheckStream('Alpha=1 Gamma', 'Stream');
end;

procedure TestTD2XSkipHandler.TestCheckBeforeMethod;
begin
  CheckFalse(fHndlr.CheckBeforeMethod('Alpha'), 'Check Before Method');
end;

procedure TestTD2XSkipHandler.TestDescription;
begin
  CheckEqualsString('Skip Methods', fHndlr.Description, 'Description');
end;

procedure TestTD2XSkipHandler.TestCheckAfterMethod;
begin
  CheckFalse(fHndlr.CheckAfterMethod('Alpha'), 'Check After Method');
end;

procedure TestTD2XSkipHandler.TestEndProcessing;
begin
  fHndlr.EndProcessing(MakeStream(fSS));
  CheckStream('', 'End Processing');
end;

procedure TestTD2XSkipHandler.TestProcessing;
begin
  fSS.WriteString('Alpha=1'#13#10'Gamma');
  fHndlr.BeginFile('', MakeStream(fSS));
  CheckStream('Alpha=1 Gamma', 'Stream');

  CheckTrue(fHndlr.CheckBeforeMethod('Alpha'), 'Check Before Alpha');
  CheckFalse(fHndlr.CheckBeforeMethod('Beta'), 'Check Before Beta');
  CheckTrue(fHndlr.CheckBeforeMethod('Gamma'), 'Check Before Gamma');

  CheckTrue(fHndlr.CheckAfterMethod('Alpha'), 'Check After Alpha');
  CheckFalse(fHndlr.CheckAfterMethod('Beta'), 'Check After Beta');
  CheckTrue(fHndlr.CheckAfterMethod('Gamma'), 'Check After Gamma');

  fHndlr.EndProcessing(MakeStream(fSS));
  CheckStream('Alpha=1 Gamma=1', 'End Processing');
end;

procedure TestTD2XSkipHandler.TestUseProxy;
begin
  CheckFalse(fHndlr.UseProxy, 'Uses proxy');
end;

{ TestTD2XXmlHandler }

procedure TestTD2XXmlHandler.SetUp;
begin
  inherited;

  fHndlr := TTestXmlHandler.CreateXml(
    function: Boolean
    begin
      fCalledFinalToken := True;
      Result := True;
    end,
    function: string
    begin
      fCalledParseMode := True;
      Result := 'ParseMode';
    end);
end;

procedure TestTD2XXmlHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XXmlHandler.TestAddAttr;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.AddAttr('Test', 'Test');
  fHndlr.EndResults(MakeStream(fSS));
  CheckStream('<?xml version="1.0"?> <Test parseMode="ParseMode" Test="Test" />',
    'End Results');
end;

procedure TestTD2XXmlHandler.TestAddText;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.AddText('Test');
  fHndlr.EndResults(MakeStream(fSS));
  CheckStream('<?xml version="1.0"?> <Test parseMode="ParseMode">Test</Test>', 'End Results');
end;

procedure TestTD2XXmlHandler.TestBeginMethod;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.EndResults(MakeStream(fSS));
  CheckStream('<?xml version="1.0"?> <Test parseMode="ParseMode" />', 'End Results');
end;

procedure TestTD2XXmlHandler.TestBeginResults;
begin
  fHndlr.HasFiles := True;

  CheckTrue(fHndlr.HasFiles, 'Has files set');

  fHndlr.BeginResults;
  CheckFalse(fHndlr.HasFiles, 'Has files reset');
end;

procedure TestTD2XXmlHandler.TestCopy;
var
  pFrom: TD2XXmlHandler;
begin
  pFrom := nil;
  fCalledFinalToken := False;
  fCalledParseMode := False;

  fHndlr.Copy(pFrom);
  try
    pFrom := TD2XXmlHandler.CreateXml(nil, nil);

    pFrom.InitParser(fParser);

    CheckFalse(Assigned(fHndlr.Parser), 'Parser not set');

    fHndlr.Copy(pFrom);

    Check(fParser = fHndlr.Parser, 'Parser set');
  finally
    pFrom.Free;
  end;
end;

procedure TestTD2XXmlHandler.TestDescription;
begin
  CheckEqualsString('Xml', fHndlr.Description, 'Description');
end;

procedure TestTD2XXmlHandler.TestEndMethod;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.EndMethod('Test');
  fHndlr.EndResults(MakeStream(fSS));
  CheckStream('<?xml version="1.0"?> <Test parseMode="ParseMode" />', 'End Results');
end;

procedure TestTD2XXmlHandler.TestEndResults;
begin
  fHndlr.BeginResults;
  fHndlr.EndResults(MakeStream(fSS));
  CheckStream('', 'End Results');

  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.EndResults(MakeStream(fSS));
  CheckStream('<?xml version="1.0"?>', 'End Results');
end;

procedure TestTD2XXmlHandler.TestInitParser;
begin
  fCalledFinalToken := False;
  fCalledParseMode := False;

  fHndlr.InitParser(fParser);

  Check(fParser = fHndlr.Parser, 'Parser set');
  CheckTrue(Assigned(fHndlr.FinalToken), 'Final Token set');
  CheckTrue(fHndlr.FinalToken(), 'Final Token correct');
  CheckTrue(fCalledFinalToken, 'Final Token called');
  CheckTrue(Assigned(fHndlr.ParseMode), 'Parse Mode set');
  CheckEqualsString('ParseMode', fHndlr.ParseMode(), 'Parse Mode correct');
  CheckTrue(fCalledParseMode, 'Parse Mode called');
end;

procedure TestTD2XXmlHandler.TestInvalidCreate;
begin
  StartExpectingException(EInvalidHandler);
  try
    TD2XXmlHandler.Create;
  except
    on E: EInvalidHandler do
    begin
      CheckEqualsString('Invalid constructor called', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XXmlHandler.TestLexerInclude;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');

  fHndlr.LexerInclude('Test', 1, 2);

  fHndlr.EndResults(MakeStream(fSS));
  CheckStream
    ('<?xml version="1.0"?> <Test parseMode="ParseMode"> <IncludeFile filename="Test" msgAt="1,2" /> </Test>',
    'End Results');
end;

procedure TestTD2XXmlHandler.TestParserMessage;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');

  fHndlr.ParserMessage(meNotSupported, 'Test', 1, 2);

  fHndlr.EndResults(MakeStream(fSS));
  CheckStream
    ('<?xml version="1.0"?> <Test parseMode="ParseMode"> <D2X_notSuppMsg msgAt="1,2">Test</D2X_notSuppMsg> </Test>',
    'End Results');

  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.ParserMessage(meError, 'Test', 1, 2);
  fHndlr.EndResults(MakeStream(fSS));
  CheckStream
    ('<?xml version="1.0"?> <Test parseMode="ParseMode"> <D2X_errorMsg msgAt="1,2">Test</D2X_errorMsg> </Test>',
    'End Results');
end;

procedure TestTD2XXmlHandler.TestProcessing;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.BeginMethod('Test1');
  fHndlr.AddAttr('Test', 'Test');
  fHndlr.BeginMethod('Test2');
  fHndlr.AddText('Test');
  fHndlr.EndMethod('Test2');
  fHndlr.BeginMethod('Test3');
  fHndlr.AddAttr('Test', 'Test');
  fHndlr.RollbackTo('Test1');
  fHndlr.BeginMethod('Test4');
  fHndlr.EndResults(MakeStream(fSS));
  CheckStream
    ('<?xml version="1.0"?> <Test parseMode="ParseMode"> <Test1 Test="Test"> <Test2>Test</Test2> <Test3 Test="Test" /> <Test4 /> </Test1> </Test>',
    'End Results');
end;

procedure TestTD2XXmlHandler.TestRollbackTo;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.BeginMethod('Test1');
  fHndlr.BeginMethod('Test2');
  fHndlr.BeginMethod('Test3');
  fHndlr.RollbackTo('Test1');
  fHndlr.BeginMethod('Test4');
  fHndlr.EndResults(MakeStream(fSS));
  CheckStream
    ('<?xml version="1.0"?> <Test parseMode="ParseMode"> <Test1> <Test2> <Test3 /> </Test2> <Test4 /> </Test1> </Test>',
    'End Results');
end;

procedure TestTD2XXmlHandler.TestUseProxy;
begin
  CheckTrue(fHndlr.UseProxy, 'Uses proxy');
end;

{ TestTD2XWriteDefinesHandler }

procedure TestTD2XWriteDefinesHandler.SetUp;
begin
  inherited;

  fHndlr := TD2XWriteDefinesHandler.Create;
end;

procedure TestTD2XWriteDefinesHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XWriteDefinesHandler.TestCopy;
var
  pFrom: TD2XWriteDefinesHandler;
begin
  pFrom := nil;

  fHndlr.Copy(pFrom);
  try
    pFrom := TD2XWriteDefinesHandler.Create;

    pFrom.InitParser(fParser);

    CheckFalse(Assigned(fHndlr.Parser), 'Parser not set');

    fHndlr.Copy(pFrom);

    Check(fParser = fHndlr.Parser, 'Parser set');
  finally
    pFrom.Free;
  end;
end;

procedure TestTD2XWriteDefinesHandler.TestDescription;
begin
  CheckEqualsString('Write Defines', fHndlr.Description, 'Description');
end;

procedure TestTD2XWriteDefinesHandler.TestEndResults;
begin
  fHndlr.InitParser(fParser);

  fHndlr.EndResults(MakeStream(fSS));
  CheckStream('**** CONDITIONALEXPRESSIONS CPU386 MSWINDOWS UNICODE VER230 WIN32',
    'End Results');
end;

procedure TestTD2XWriteDefinesHandler.TestInit;
begin
  fHndlr.InitParser(fParser);

  Check(fParser = fHndlr.Parser, 'Parser set');
  CheckTrue(Assigned(fHndlr.Parser), 'Parse Mode set');
end;

procedure TestTD2XWriteDefinesHandler.TestUseProxy;
begin
  CheckFalse(fHndlr.UseProxy, 'Uses proxy');
end;

{ TestTD2XDefinesUsedHandler }

procedure TestTD2XDefinesUsedHandler.SetUp;
begin
  inherited;

  fHndlr := TD2XDefinesUsedHandler.Create;
end;

procedure TestTD2XDefinesUsedHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XDefinesUsedHandler.TestDefineUsed;
begin
  fHndlr.DefineUsed('Test');

  fHndlr.EndProcessing(MakeStream(fSS));
  CheckStream('Test=1', 'Define Used');
end;

procedure TestTD2XDefinesUsedHandler.TestDescription;
begin
  CheckEqualsString('Defines Used', fHndlr.Description, 'Description');
end;

procedure TestTD2XDefinesUsedHandler.TestEndProcessing;
var
  lCalled: Boolean;
begin
  lCalled := False;
  StartExpectingException(EAssertionFailed);
  try
    fHndlr.EndProcessing(
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

procedure TestTD2XDefinesUsedHandler.TestProcessing;
begin
  fHndlr.DefineUsed('Alpha');
  fHndlr.DefineUsed('Beta');
  fHndlr.DefineUsed('Alpha');
  fHndlr.DefineUsed('Gamma');
  fHndlr.DefineUsed('Alpha');
  fHndlr.DefineUsed('Beta');

  fHndlr.EndProcessing(MakeStream(fSS));
  CheckStream('Alpha=3 Beta=2 Gamma=1', 'Define Used');
end;

procedure TestTD2XDefinesUsedHandler.TestUseProxy;
begin
  CheckFalse(fHndlr.UseProxy, 'Uses proxy');
end;

{ TestTD2XParserDefinesHandler }

procedure TestTD2XParserDefinesHandler.SetUp;
begin
  inherited;

  fDefs := TStringList.Create;
  fDefs.CommaText := 'Alpha,Beta,Gamma';
  fDefs.Sorted := True;

  fHndlr := TD2XParserDefinesHandler.CreateDefines(fDefs);
end;

procedure TestTD2XParserDefinesHandler.TearDown;
begin
  FreeAndNil(fHndlr);
  FreeAndNil(fDefs);

  inherited;
end;

procedure TestTD2XParserDefinesHandler.TestBeginFile;
begin
  fHndlr.InitParser(fParser);

  fHndlr.BeginFile('', nil);
  CheckList('CONDITIONALEXPRESSIONS CPU386 MSWINDOWS UNICODE VER230 WIN32', 'Begin File',
    fParser.StartDefines);

  fDefs.CommaText := '';
  fHndlr.BeginFile('', nil);
  CheckList('', 'Cleared Defines', fParser.StartDefines);

  fDefs.CommaText := 'Test';
  fHndlr.BeginFile('', nil);
  CheckList('Test', 'Test Define', fParser.StartDefines);
end;

procedure TestTD2XParserDefinesHandler.TestCopy;
var
  pFrom: TD2XParserDefinesHandler;
begin
  pFrom := nil;

  fHndlr.Copy(pFrom);
  try
    pFrom := TD2XParserDefinesHandler.Create;

    pFrom.InitParser(fParser);

    CheckFalse(Assigned(fHndlr.Parser), 'Parser not set');

    fHndlr.Copy(pFrom);

    Check(fParser = fHndlr.Parser, 'Parser set');
  finally
    pFrom.Free;
  end;
end;

procedure TestTD2XParserDefinesHandler.TestDescription;
begin
  CheckEqualsString('Parser Defines', fHndlr.Description, 'Description');
end;

procedure TestTD2XParserDefinesHandler.TestInit;
begin
  fHndlr.InitParser(fParser);

  Check(fParser = fHndlr.Parser, 'Parser set');
  CheckTrue(Assigned(fHndlr.Parser), 'Parse Mode set');
end;

procedure TestTD2XParserDefinesHandler.TestUseProxy;
begin
  CheckFalse(fHndlr.UseProxy, 'Uses proxy');
end;

{ TParserTestCase }

procedure TParserTestCase.SetUp;
begin
  inherited;

  fParser := TD2XDefinesParser.Create;
end;

procedure TParserTestCase.TearDown;
begin
  FreeAndNil(fParser);

  inherited;
end;

{ TestTD2XErrorHandler }

procedure TestTD2XErrorHandler.LogMessage(pType, pMsg: string; pX, pY: Integer);
begin
  fSB.AppendFormat('%s: %s (%d,%d)', [pType, pMsg, pX, pY]);
end;

procedure TestTD2XErrorHandler.SetUp;
begin
  inherited;

  fHndlr := TD2XErrorHandler.CreateError(meError, Self.LogMessage);
end;

procedure TestTD2XErrorHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XErrorHandler.TestDescription;
begin
  CheckEqualsString('Error Logging', fHndlr.Description, 'Description');
end;

procedure TestTD2XErrorHandler.TestInvalidCreate;
begin
  StartExpectingException(EInvalidHandler);
  try
    TD2XErrorHandler.Create;
  except
    on E: EInvalidHandler do
    begin
      CheckEqualsString('Invalid constructor called', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XErrorHandler.TestParserMessage;
begin
  fHndlr.ParserMessage(meError, 'Test', 1, 2);
  CheckBuilder('Error: Test (1,2)', 'Correct Parser Message');
end;

initialization

// Register any test cases with the test runner
RegisterTests('Handlers', [TestTD2XCountHandler.Suite, TestTD2XDefinesUsedHandler.Suite,
  TestTD2XParserDefinesHandler.Suite, TestTD2XErrorHandler.Suite, TestTD2XSkipHandler.Suite,
  TestTD2XWriteDefinesHandler.Suite, TestTD2XXmlHandler.Suite]);

end.
