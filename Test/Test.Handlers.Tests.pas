unit Test.Handlers.Tests;

interface

implementation

uses
  CastaliaPasLexTypes,
  D2X,
  D2X.Handler,
  D2X.Handlers,
  D2X.IO,
  D2X.Param,
  D2X.Parser,
  D2X.Xml,
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  Test.Constants,
  Test.Global,
  Test.Handlers,
  TestFramework;

type
  TestTD2XCountChildrenHandler = class(TStringTestCase)
  strict private
    fHndlr: TD2XCountChildrenHandler;
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

  TestTD2XCountDefinesHandler = class(TParserTestCase)
  strict private
    fHndlr: TD2XCountDefinesHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestUseProxy;
    procedure TestEndProcessing;
    procedure TestEndFile;

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
    procedure TestUseProxy;
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

  TestTD2XHeldDefinesHandler = class(TParserTestCase)
  strict private
    fHndlr: TD2XHeldDefinesHandler;
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
    property FinalToken: ID2XFlag read fFinalToken;
    property ParseMode: TD2XStringRef read fParseMode;

  end;

  TestTD2XXmlHandler = class(TParserTestCase)
  strict private
    fHndlr: TTestXmlHandler;
    fFinalToken: TD2XBoolFlag;
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
    procedure TestTrimChildren;

    procedure TestProcessing;
  end;

function NilReader: TStreamReader;
begin
  Result := nil;
end;

function NilWriter: TStreamWriter;
begin
  Result := nil;
end;

//procedure InitStringListLoad;
//var
//  lS: TStream;
//  lSL: TStringList;
//begin
//  lS := nil;
//  lSL := nil;
//  try
//    lSL := TStringList.Create;
//    lS := TStringStream.Create;
//    lSL.LoadFromStream(lS);
//  finally
//    lS.Free;
//    lSL.Free;
//  end;
//end;

{ TestTD2XCountChildrenHandler }

procedure TestTD2XCountChildrenHandler.SetUp;
begin
  inherited;

  fHndlr := TD2XCountChildrenHandler.Create;
end;

procedure TestTD2XCountChildrenHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XCountChildrenHandler.TestProcessing;
begin
  fHndlr.BeginFile('', NilReader);
  fHndlr.BeginMethod('Alpha');
  fHndlr.BeginMethod('Beta');
  fHndlr.EndMethod('Beta');
  fHndlr.BeginMethod('Gamma');
  fHndlr.EndMethod('Gamma');
  fHndlr.EndMethod('Alpha');
  fHndlr.EndFile('', NilWriter);

  fHndlr.EndProcessing(
      function: TStreamWriter
    begin
      Result := fDS.WriteTo;
    end);

  CheckStream('Alpha=2,2', 'Processing');
end;

procedure TestTD2XCountChildrenHandler.TestUseProxy;
begin
  CheckTrue(fHndlr.UseProxy, 'Uses proxy');
end;

procedure TestTD2XCountChildrenHandler.TestBeginFile;
var
  lCalled: Boolean;
begin
  lCalled := False;

  fHndlr.BeginFile('',
    function: TStreamReader
    begin
      Result := nil;
      lCalled := True;
    end);

  Check(lCalled, 'Stream Creator called');
  CheckStream('', 'Begin File');
end;

procedure TestTD2XCountChildrenHandler.TestEndFile;
var
  lCalled: Boolean;
begin
  lCalled := False;

  fHndlr.EndFile('',
    function: TStreamWriter
    begin
      Result := nil;
      lCalled := True;
    end);

  Check(lCalled, 'Stream Creator called');
  CheckStream('', 'End File');
end;

procedure TestTD2XCountChildrenHandler.TestBeginMethod;
var
  pMethod: string;
begin
  fHndlr.BeginFile('', NilReader);

  fHndlr.BeginMethod(pMethod);

  CheckStream('', 'Begin Method');
end;

procedure TestTD2XCountChildrenHandler.TestDescription;
begin
  CheckEqualsString('Count Children', fHndlr.Description, 'Description');
end;

procedure TestTD2XCountChildrenHandler.TestEndMethod;
var
  pMethod: string;
begin
  fHndlr.BeginFile('', NilReader);

  fHndlr.EndMethod(pMethod);

  CheckStream('', 'End Method');
end;

procedure TestTD2XCountChildrenHandler.TestEndProcessing;
var
  lCalled: Boolean;
begin
  lCalled := False;
  StartExpectingException(EAssertionFailed);
  try
    fHndlr.EndProcessing(
      function: TStreamWriter
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
  fS.WriteString('Alpha=1'#13#10'Gamma');
  fHndlr.BeginFile('', FileReaderRef(fDS));
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
  fHndlr.EndProcessing(FileWriterRef(fDS));
  CheckStream('', 'End Processing');
end;

procedure TestTD2XSkipHandler.TestProcessing;
begin
  fS.WriteString('Alpha=1'#13#10'Gamma');
  fHndlr.BeginFile('', FileReaderRef(fDS));
  CheckStream('Alpha=1 Gamma', 'Stream');

  CheckTrue(fHndlr.CheckBeforeMethod('Alpha'), 'Check Before Alpha');
  CheckFalse(fHndlr.CheckBeforeMethod('Beta'), 'Check Before Beta');
  CheckTrue(fHndlr.CheckBeforeMethod('Gamma'), 'Check Before Gamma');

  CheckTrue(fHndlr.CheckAfterMethod('Alpha'), 'Check After Alpha');
  CheckFalse(fHndlr.CheckAfterMethod('Beta'), 'Check After Beta');
  CheckTrue(fHndlr.CheckAfterMethod('Gamma'), 'Check After Gamma');

  fHndlr.EndProcessing(FileWriterRef(fDS));
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

  fFinalToken := TD2XBoolFlag.Create;
  ID2XFlag(fFinalToken).Flag := True;

  fHndlr := TTestXmlHandler.CreateXml(fFinalToken,
    function: string
    begin
      fCalledParseMode := True;
      Result := 'ParseMode';
    end);
end;

procedure TestTD2XXmlHandler.TearDown;
begin
  FreeAndNil(fHndlr);
  FreeAndNil(fFinalToken);

  inherited;
end;

procedure TestTD2XXmlHandler.TestAddAttr;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.AddAttr('Test', 'Test');
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('<?xml version="1.0"?> <Test parseMode="ParseMode" Test="Test" />',
    'End Results');
end;

procedure TestTD2XXmlHandler.TestAddText;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.AddText('Test');
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('<?xml version="1.0"?> <Test parseMode="ParseMode">Test</Test>', 'End Results');
end;

procedure TestTD2XXmlHandler.TestBeginMethod;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.EndResults(FileWriterRef(fDS));
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
  fCalledParseMode := False;

  fHndlr.Copy(pFrom);
  try
    pFrom := TD2XXmlHandler.CreateXml(fFinalToken, nil);

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
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('<?xml version="1.0"?> <Test parseMode="ParseMode" />', 'End Results');
end;

procedure TestTD2XXmlHandler.TestEndResults;
begin
  fHndlr.BeginResults;
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('', 'End Results');

  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('<?xml version="1.0"?>', 'End Results');
end;

procedure TestTD2XXmlHandler.TestInitParser;
begin
  fCalledParseMode := False;

  fHndlr.InitParser(fParser);

  Check(fParser = fHndlr.Parser, 'Parser set');
  CheckTrue(Assigned(fHndlr.FinalToken), 'Final Token set');
  CheckTrue(fHndlr.FinalToken.Flag, 'Final Token correct');
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

  fHndlr.EndResults(FileWriterRef(fDS));
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

  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream
    ('<?xml version="1.0"?> <Test parseMode="ParseMode"> <D2X_notSuppMsg msgAt="1,2">Test</D2X_notSuppMsg> </Test>',
    'End Results');

  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.ParserMessage(meError, 'Test', 1, 2);
  fHndlr.EndResults(FileWriterRef(fDS));
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
  fHndlr.EndResults(FileWriterRef(fDS));
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
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream
    ('<?xml version="1.0"?> <Test parseMode="ParseMode"> <Test1> <Test2> <Test3 /> </Test2> <Test4 /> </Test1> </Test>',
    'End Results');
end;

procedure TestTD2XXmlHandler.TestTrimChildren;
begin
  fHndlr.BeginResults;
  fHndlr.HasFiles := True;
  fHndlr.BeginMethod('Test');
  fHndlr.BeginMethod('Test1');
  fHndlr.BeginMethod('Test2');
  fHndlr.BeginMethod('Test3');
  fHndlr.RollbackTo('Test1');
  fHndlr.BeginMethod('Test4');
  fHndlr.EndMethod('Test4');
  fHndlr.BeginMethod('Test5');
  fHndlr.EndMethod('Test5');
  fHndlr.TrimChildren('Test4');
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream
    ('<?xml version="1.0"?> <Test parseMode="ParseMode"> <Test1> <Test2> <Test3 /> </Test2> <Test5 /> </Test1> </Test>',
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

    CheckFalse(Assigned(TTestParserHandler(fHndlr).Parser), 'Parser not set');

    fHndlr.Copy(pFrom);

    Check(fParser = TTestParserHandler(fHndlr).Parser, 'Parser set');
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

  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('**** ' + ReplaceStr(EXPECTED_DEFINES, ',', ' '), 'End Results');
end;

procedure TestTD2XWriteDefinesHandler.TestInit;
begin
  fHndlr.InitParser(fParser);

  CheckTrue(Assigned(TTestParserHandler(fHndlr).Parser), 'Parse Mode set');
  Check(fParser = TTestParserHandler(fHndlr).Parser, 'Parser set');
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
  fHndlr.DefineUsed(' Test');
  fHndlr.DefineUsed('Test ');

  fHndlr.EndProcessing(FileWriterRef(fDS));
  CheckStream('Test=3', 'Define Used');
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
      function: TStreamWriter
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

  fHndlr.EndProcessing(FileWriterRef(fDS));
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
  CheckList(ReplaceStr(EXPECTED_DEFINES, ',', ' '), 'Begin File', fParser.StartDefines);

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

    CheckFalse(Assigned(TTestParserHandler(fHndlr).Parser), 'Parser not set');

    fHndlr.Copy(pFrom);

    Check(fParser = TTestParserHandler(fHndlr).Parser, 'Parser set');
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

  CheckTrue(Assigned(TTestParserHandler(fHndlr).Parser), 'Parse Mode set');
  Check(fParser = TTestParserHandler(fHndlr).Parser, 'Parser set');
end;

procedure TestTD2XParserDefinesHandler.TestUseProxy;
begin
  CheckFalse(fHndlr.UseProxy, 'Uses proxy');
end;

{ TestTD2XErrorHandler }

procedure TestTD2XErrorHandler.LogMessage(pType, pMsg: string; pX, pY: Integer);
begin
  fB.AppendFormat('%s: %s (%d,%d)', [pType, pMsg, pX, pY]);
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

procedure TestTD2XErrorHandler.TestUseProxy;
begin
  CheckFalse(fHndlr.UseProxy, 'Uses proxy');
end;

{ TestTD2XCountDefinesHandler }

procedure TestTD2XCountDefinesHandler.SetUp;
begin
  inherited;

  fHndlr := TD2XCountDefinesHandler.Create;
end;

procedure TestTD2XCountDefinesHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XCountDefinesHandler.TestDescription;
begin
  CheckEqualsString('Count Defines', fHndlr.Description, 'Description');
end;

procedure TestTD2XCountDefinesHandler.TestEndFile;
var
  lCalled: Boolean;
begin
  lCalled := False;

  fHndlr.InitParser(fParser);
  fParser.GetLexerDefines(fParser.StartDefines);

  fHndlr.EndFile('',
    function: TStreamWriter
    begin
      Result := nil;
      lCalled := True;
    end);

  Check(lCalled, 'Stream Creator called');
  CheckStream('', 'End File');
end;

procedure TestTD2XCountDefinesHandler.TestEndProcessing;
var
  lCalled: Boolean;
begin
  lCalled := False;

  StartExpectingException(EAssertionFailed);
  try
    fHndlr.InitParser(fParser);
    fParser.GetLexerDefines(fParser.StartDefines);

    fHndlr.EndProcessing(
      function: TStreamWriter
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

procedure TestTD2XCountDefinesHandler.TestProcessing;
begin
  fHndlr.InitParser(fParser);
  fParser.GetLexerDefines(fParser.StartDefines);

  fParser.Lexer.AddDefine('Test');
  fParser.Lexer.AddDefine(' Test');
  fParser.Lexer.AddDefine('Test ');
  fParser.Lexer.AddDefine(' Test ');
  fParser.Lexer.AddDefine('Alpha');

  fHndlr.EndFile('', NilWriter);

  fHndlr.EndProcessing(
    function: TStreamWriter
    begin
      Result := fDS.WriteTo;
    end);

  CheckStream('Alpha=1 Test=4', 'Processing');
end;

procedure TestTD2XCountDefinesHandler.TestUseProxy;
begin
  CheckTrue(fHndlr.UseProxy, 'Uses proxy');
end;

{ TestTD2XHeldDefinesHandler }

procedure TestTD2XHeldDefinesHandler.SetUp;
begin
  inherited;

  fDefs := TStringList.Create;
  fDefs.Sorted := True;

  fHndlr := TD2XHeldDefinesHandler.CreateDefines(fDefs);
end;

procedure TestTD2XHeldDefinesHandler.TearDown;
begin
  FreeAndNil(fHndlr);
  FreeAndNil(fDefs);

  inherited;
end;

procedure TestTD2XHeldDefinesHandler.TestBeginFile;
begin
  fHndlr.InitParser(fParser);

  fHndlr.BeginFile('', nil);
  CheckList('', 'Begin File', fParser.HeldDefines);

  fDefs.CommaText := 'Test';
  fHndlr.BeginFile('', nil);
  CheckList('Test', 'Test Define', fParser.HeldDefines);

  fDefs.CommaText := '';
  fHndlr.BeginFile('', nil);
  CheckList('', 'Cleared Defines', fParser.HeldDefines);
end;

procedure TestTD2XHeldDefinesHandler.TestCopy;
var
  pFrom: TD2XHeldDefinesHandler;
begin
  pFrom := nil;

  fHndlr.Copy(pFrom);
  try
    pFrom := TD2XHeldDefinesHandler.Create;

    pFrom.InitParser(fParser);

    CheckFalse(Assigned(TTestParserHandler(fHndlr).Parser), 'Parser not set');

    fHndlr.Copy(pFrom);

    Check(fParser = TTestParserHandler(fHndlr).Parser, 'Parser set');
  finally
    pFrom.Free;
  end;
end;

procedure TestTD2XHeldDefinesHandler.TestDescription;
begin
  CheckEqualsString('Held Defines', fHndlr.Description, 'Description');
end;

procedure TestTD2XHeldDefinesHandler.TestInit;
begin
  fHndlr.InitParser(fParser);

  CheckTrue(Assigned(TTestParserHandler(fHndlr).Parser), 'Parse Mode set');
  Check(fParser = TTestParserHandler(fHndlr).Parser, 'Parser set');
end;

procedure TestTD2XHeldDefinesHandler.TestUseProxy;
begin
  CheckFalse(fHndlr.UseProxy, 'Uses proxy');
end;

initialization

// Register any test cases with the test runner
RegisterTests('Handlers', [TestTD2XCountChildrenHandler.Suite,
  TestTD2XCountDefinesHandler.Suite, TestTD2XDefinesUsedHandler.Suite,
  TestTD2XParserDefinesHandler.Suite, TestTD2XHeldDefinesHandler.Suite,
  TestTD2XErrorHandler.Suite, TestTD2XSkipHandler.Suite, TestTD2XWriteDefinesHandler.Suite,
  TestTD2XXmlHandler.Suite]);

//InitStringListLoad;

end.
