unit Test.Handlers.Tests;

interface

implementation

uses
  CastaliaPasLexTypes,
  D2X.Global,
  D2X.Handler,
  D2X.Handlers,
  D2X.IO,
  D2X.Param,
  D2X.Parser,
  D2X.Tree,
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  Test.Constants,
  Test.Flag,
  Test.Global,
  Test.Handlers,
  Test.IO,
  TestFramework;

type
  TestHandlerUtils = class(TStringTestCase)
  private
    function PairFormat(pPair: TStrIntPair): string;
  published
    procedure TestOutputStrIntDictNoStream;
    procedure TestOutputStrIntDictNoFunc;
    procedure TestOutputStrIntDictNoDict;
    procedure TestOutputStrIntDict;
  end;

  TestTD2XParserHandler = class(TParserTestCase)
  strict private
    fHndlr: TTestParserHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitParser;
    procedure TestDoParserMessage;
    procedure TestDoLexerInclude;
    procedure TestOnParserMessage;
    procedure TestOnLexerInclude;

  end;

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

  TestTD2XCountFinalDefinesHandler = class(TParserTestCase)
  strict private
    fHndlr: TD2XCountFinalDefinesHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestUseProxy;
    procedure TestEndProcessing;
    procedure TestBeginFile;
    procedure TestEndFile;

    procedure TestProcessing;
  end;

  TestTD2XCountDefinesUsedHandler = class(TParserTestCase)
  strict private
    fHndlr: TTestCountDefinesUsedHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitParser;
    procedure TestUseProxy;
    procedure TestEndProcessing;
    procedure TestOnIf;
    procedure TestOnIfDef;
    procedure TestDefineUsed;

    procedure TestProcessing;
  end;

  TestTD2XErrorHandler = class(TParserTestCase)
  strict private
    fHndlr: TTestErrorHandler;

    procedure LogMessage(pType, pMsg: string; pX, pY: Integer);

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInvalidCreate;
    procedure TestInitParser;
    procedure TestUseProxy;
    procedure TestOnParserMessage;
  end;

  TestTD2XLogHandler = class(TParserTestCase)
  strict private
    fHndlr: TTestLogHandler;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitParser;
    procedure TestUseProxy;
    procedure TestBeginMethod;
    procedure TestEndMethod;
    procedure TestOnParserMessage;
    procedure TestOnLexerInclude;

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
    procedure TestBeginFile;
    procedure TestEndFile;
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
    procedure TestBeginFile;
    procedure TestEndFile;
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
    procedure TestEndFile;
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
    procedure TestUseProxy;
    procedure TestInit;
    procedure TestBeginResults;
    procedure TestEndResults;
  end;

  TestTD2XTreeHandler = class(TParserTestCase)
  strict private
    fHndlr: TTestTreeHandler;
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
    procedure TestBeginResults;
    procedure TestEndResults;
    procedure TestBeginFile;
    procedure TestEndFile;
    procedure TestBeginMethod;
    procedure TestEndMethod;
    procedure TestAddAttr;
    procedure TestAddText;
    procedure TestRollbackTo;
    procedure TestOnLexerInclude;
    procedure TestOnParserMessage;
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
  CheckInterface(ID2XFullProxy, fHndlr, 'Uses full proxy');
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
begin
  fHndlr.BeginFile('', NilReader);

  fHndlr.BeginMethod('Test');

  CheckStream('', 'Begin Method');
end;

procedure TestTD2XCountChildrenHandler.TestDescription;
begin
  CheckEqualsString('Count Children', fHndlr.Description, 'Description');
end;

procedure TestTD2XCountChildrenHandler.TestEndMethod;
begin
  fHndlr.BeginFile('', NilReader);

  fHndlr.EndMethod('Test');

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

procedure TestTD2XSkipHandler.TestEndFile;
begin
  fHndlr.EndFile('', FileWriterRef(fDS));
  CheckStream('', 'Stream');
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
  CheckNotInterface(ID2XFullProxy, fHndlr, 'Full proxy');
end;

{ TestTD2XTreeHandler }

procedure TestTD2XTreeHandler.SetUp;
begin
  inherited;

  fFinalToken := TD2XBoolFlag.Create;
  fFinalToken.SetFlag(True);

  fHndlr := TTestTreeHandler.CreateTree(TD2XTreeWriter, fFinalToken.FlagRef,
    function: string
    begin
      fCalledParseMode := True;
      Result := 'ParseMode';
    end);
end;

procedure TestTD2XTreeHandler.TearDown;
begin
  FreeAndNil(fHndlr);
  FreeAndNil(fFinalToken);

  inherited;
end;

procedure TestTD2XTreeHandler.TestAddAttr;
begin
  fHndlr.BeginResults;
  fHndlr.BeginFile('Test', nil);
  fHndlr.BeginMethod('Test');
  fHndlr.AddAttr('Test', 'Test');
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('$Test< @parseMode:ParseMode; @Test:Test; >', 'End Results');
end;

procedure TestTD2XTreeHandler.TestAddText;
begin
  fHndlr.BeginResults;
  fHndlr.BeginFile('Test', nil);
  fHndlr.BeginMethod('Test');
  fHndlr.AddText('Test');
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('$Test< @parseMode:ParseMode; :Test; >', 'End Results');
end;

procedure TestTD2XTreeHandler.TestBeginFile;
begin
  CheckFalse(fHndlr.HasFiles, 'Has files not set');

  fHndlr.BeginFile('Test', nil);

  CheckTrue(fHndlr.HasFiles, 'Has files set');
end;

procedure TestTD2XTreeHandler.TestBeginMethod;
begin
  fHndlr.BeginResults;
  fHndlr.BeginFile('Test', nil);
  fHndlr.BeginMethod('Test');
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('$Test< @parseMode:ParseMode; >', 'End Results');
end;

procedure TestTD2XTreeHandler.TestBeginResults;
begin
  fHndlr.BeginFile('Test', nil);

  CheckTrue(fHndlr.HasFiles, 'Has files set');

  fHndlr.BeginResults;
  CheckFalse(fHndlr.HasFiles, 'Has files reset');
end;

procedure TestTD2XTreeHandler.TestDescription;
begin
  CheckEqualsString('Xml', fHndlr.Description, 'Description');
end;

procedure TestTD2XTreeHandler.TestEndFile;
begin
  fHndlr.EndFile('Test', nil);

  CheckTrue(True, 'Check');
end;

procedure TestTD2XTreeHandler.TestEndMethod;
begin
  fHndlr.BeginResults;
  fHndlr.BeginFile('Test', nil);
  fHndlr.BeginMethod('Test');
  fHndlr.EndMethod('Test');
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('$Test< @parseMode:ParseMode; >', 'End Results');
end;

procedure TestTD2XTreeHandler.TestEndResults;
begin
  fHndlr.BeginResults;
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('', 'End Results');

  fHndlr.BeginResults;
  fHndlr.BeginFile('Test', nil);
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('', 'End Results');
end;

procedure TestTD2XTreeHandler.TestInitParser;
begin
  fCalledParseMode := False;

  fHndlr.InitParser(fParser, fFlag.FlagRef);

  Check(fParser = fHndlr.Parser, 'Parser set');
  CheckTrue(Assigned(fHndlr.FinalToken), 'Final Token set');
  CheckTrue(fHndlr.FinalToken(), 'Final Token correct');
  CheckTrue(Assigned(fHndlr.ParseMode), 'Parse Mode set');
  CheckEqualsString('ParseMode', fHndlr.ParseMode(), 'Parse Mode correct');
  CheckTrue(fCalledParseMode, 'Parse Mode called');
end;

procedure TestTD2XTreeHandler.TestInvalidCreate;
begin
  StartExpectingException(EInvalidHandler);
  try
    TD2XTreeHandler.Create;
  except
    on E: EInvalidHandler do
    begin
      CheckEqualsString('Invalid constructor called', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XTreeHandler.TestOnLexerInclude;
begin
  fHndlr.BeginResults;
  fHndlr.BeginFile('Test', nil);
  fHndlr.BeginMethod('Test');

  fHndlr.TestLexerInclude('Test', 1, 2);

  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('$Test< @parseMode:ParseMode; $IncludeFile< @filename:Test; @msgAt:1,2; > >',
    'End Results');
end;

procedure TestTD2XTreeHandler.TestOnParserMessage;
begin
  fHndlr.BeginResults;
  fHndlr.BeginFile('Test', nil);
  fHndlr.BeginMethod('Test');

  fHndlr.TestParserMessage(meNotSupported, 'Test', 1, 2);

  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('$Test< @parseMode:ParseMode; $D2X_notSuppMsg< @msgAt:1,2; :Test; > >',
    'End Results');

  fHndlr.BeginResults;
  fHndlr.BeginFile('Test', nil);
  fHndlr.BeginMethod('Test');
  fHndlr.TestParserMessage(meError, 'Test', 1, 2);
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('$Test< @parseMode:ParseMode; $D2X_errorMsg< @msgAt:1,2; :Test; > >',
    'End Results');
end;

procedure TestTD2XTreeHandler.TestProcessing;
begin
  fHndlr.BeginResults;
  fHndlr.BeginFile('Test', nil);
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
    ('$Test< @parseMode:ParseMode; $Test1< @Test:Test; $Test2:Test; $Test3< @Test:Test; > $Test4 > >',
    'End Results');
end;

procedure TestTD2XTreeHandler.TestRollbackTo;
begin
  fHndlr.BeginResults;
  fHndlr.BeginFile('Test', nil);
  fHndlr.BeginMethod('Test');
  fHndlr.BeginMethod('Test1');
  fHndlr.BeginMethod('Test2');
  fHndlr.BeginMethod('Test3');
  fHndlr.RollbackTo('Test1');
  fHndlr.BeginMethod('Test4');
  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('$Test< @parseMode:ParseMode; $Test1< $Test2< $Test3 > $Test4 > >',
    'End Results');
end;

procedure TestTD2XTreeHandler.TestTrimChildren;
begin
  fHndlr.BeginResults;
  fHndlr.BeginFile('Test', nil);
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
  CheckStream('$Test< @parseMode:ParseMode; $Test1< $Test2< $Test3 > $Test5 > >',
    'End Results');
end;

procedure TestTD2XTreeHandler.TestUseProxy;
begin
  CheckInterface(ID2XFullProxy, fHndlr, 'Full proxy');
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

procedure TestTD2XWriteDefinesHandler.TestBeginResults;
begin
  fHndlr.BeginResults;
  CheckStream('', 'Begin Results');
end;

procedure TestTD2XWriteDefinesHandler.TestEndResults;
begin
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  fHndlr.EndResults(FileWriterRef(fDS));
  CheckStream('**** ' + ReplaceStr(EXPECTED_DEFINES, ',', ' '), 'End Results');
end;

procedure TestTD2XWriteDefinesHandler.TestInit;
begin
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  CheckTrue(Assigned(TTestParserHandler(fHndlr).Parser), 'Parse Mode set');
  Check(fParser = TTestParserHandler(fHndlr).Parser, 'Parser set');
end;

procedure TestTD2XWriteDefinesHandler.TestUseProxy;
begin
  CheckNotInterface(ID2XFullProxy, fHndlr, 'Full proxy');
end;

{ TestTD2XDefinesUsedHandler }

procedure TestTD2XCountDefinesUsedHandler.SetUp;
begin
  inherited;

  fHndlr := TTestCountDefinesUsedHandler.Create;
end;

procedure TestTD2XCountDefinesUsedHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XCountDefinesUsedHandler.TestDefineUsed;
begin
  fHndlr.TestDefineUsed('Test');
  fHndlr.TestDefineUsed(' Test');
  fHndlr.TestDefineUsed('Test ');

  fHndlr.EndProcessing(FileWriterRef(fDS));
  CheckStream('Test=3', 'Define Used');
end;

procedure TestTD2XCountDefinesUsedHandler.TestEndProcessing;
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

procedure TestTD2XCountDefinesUsedHandler.TestInitParser;
begin
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  CheckTrue(fHndlr.fInitParserCalled, 'Init Parser called');
end;

procedure TestTD2XCountDefinesUsedHandler.TestOnIf;
begin
  StartExpectingException(EAccessViolation);

  fHndlr.TestOnIf;
end;

procedure TestTD2XCountDefinesUsedHandler.TestOnIfDef;
begin
  StartExpectingException(EAccessViolation);

  fHndlr.TestOnIfDef;
end;

procedure TestTD2XCountDefinesUsedHandler.TestProcessing;
begin
  fHndlr.TestDefineUsed('Alpha');
  fHndlr.TestDefineUsed('Beta');
  fHndlr.TestDefineUsed('Alpha');
  fHndlr.TestDefineUsed('Gamma');
  fHndlr.TestDefineUsed('Alpha');
  fHndlr.TestDefineUsed('Beta');

  fHndlr.EndProcessing(FileWriterRef(fDS));
  CheckStream('Alpha=3 Beta=2 Gamma=1', 'Define Used');
end;

procedure TestTD2XCountDefinesUsedHandler.TestUseProxy;
begin
  CheckNotInterface(ID2XFullProxy, fHndlr, 'Full proxy');
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
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  fHndlr.BeginFile('', nil);
  CheckList(ReplaceStr(EXPECTED_DEFINES, ',', ' '), 'Begin File', fParser.StartDefines);

  fDefs.CommaText := '';
  fHndlr.BeginFile('', nil);
  CheckList('', 'Cleared Defines', fParser.StartDefines);

  fDefs.CommaText := 'Test';
  fHndlr.BeginFile('', nil);
  CheckList('Test', 'Test Define', fParser.StartDefines);
end;

procedure TestTD2XParserDefinesHandler.TestDescription;
begin
  CheckEqualsString('Parser Defines', fHndlr.Description, 'Description');
end;

procedure TestTD2XParserDefinesHandler.TestEndFile;
begin
  fHndlr.EndFile('', nil);
  CheckLog('', 'End File');
end;

procedure TestTD2XParserDefinesHandler.TestInit;
begin
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  CheckTrue(Assigned(TTestParserHandler(fHndlr).Parser), 'Parse Mode set');
  Check(fParser = TTestParserHandler(fHndlr).Parser, 'Parser set');
end;

procedure TestTD2XParserDefinesHandler.TestUseProxy;
begin
  CheckNotInterface(ID2XFullProxy, fHndlr, 'Full proxy');
end;

{ TestTD2XErrorHandler }

procedure TestTD2XErrorHandler.LogMessage(pType, pMsg: string; pX, pY: Integer);
begin
  fB.AppendFormat('%s: %s (%d,%d)', [pType, pMsg, pX, pY]);
end;

procedure TestTD2XErrorHandler.SetUp;
begin
  inherited;

  fHndlr := TTestErrorHandler.CreateError(meError, Self.LogMessage);
end;

procedure TestTD2XErrorHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XErrorHandler.TestInitParser;
begin
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  CheckTrue(fHndlr.fInitParserCalled, 'Init Parser called');
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

procedure TestTD2XErrorHandler.TestOnParserMessage;
begin
  fHndlr.TestParserMessage(meError, 'Test', 1, 2);
  CheckBuilder('Error: Test (1,2)', 'Correct Parser Message');
end;

procedure TestTD2XErrorHandler.TestUseProxy;
begin
  CheckNotInterface(ID2XFullProxy, fHndlr, 'Full proxy');
end;

{ TestTD2XCountDefinesHandler }

procedure TestTD2XCountFinalDefinesHandler.SetUp;
begin
  inherited;

  fHndlr := TD2XCountFinalDefinesHandler.Create;
end;

procedure TestTD2XCountFinalDefinesHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XCountFinalDefinesHandler.TestBeginFile;
var
  lCalled: Boolean;
begin
  lCalled := False;

  fHndlr.InitParser(fParser, fFlag.FlagRef);
  fParser.GetLexerDefines(fParser.StartDefines);

  fHndlr.BeginFile('',
    function: TStreamReader
    begin
      Result := nil;
      lCalled := True;
    end);

  CheckFalse(lCalled, 'Stream Creator called');
  CheckStream('', 'End File');
end;

procedure TestTD2XCountFinalDefinesHandler.TestDescription;
begin
  CheckEqualsString('Count Defines', fHndlr.Description, 'Description');
end;

procedure TestTD2XCountFinalDefinesHandler.TestEndFile;
var
  lCalled: Boolean;
begin
  lCalled := False;

  fHndlr.InitParser(fParser, fFlag.FlagRef);
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

procedure TestTD2XCountFinalDefinesHandler.TestEndProcessing;
var
  lCalled: Boolean;
begin
  lCalled := False;

  StartExpectingException(EAssertionFailed);
  try
    fHndlr.InitParser(fParser, fFlag.FlagRef);
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

procedure TestTD2XCountFinalDefinesHandler.TestProcessing;
begin
  fHndlr.InitParser(fParser, fFlag.FlagRef);
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

procedure TestTD2XCountFinalDefinesHandler.TestUseProxy;
begin
  CheckInterface(ID2XFullProxy, fHndlr, 'Full proxy');
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
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  fHndlr.BeginFile('', nil);
  CheckList('', 'Begin File', fParser.HeldDefines);

  fDefs.CommaText := 'Test';
  fHndlr.BeginFile('', nil);
  CheckList('Test', 'Test Define', fParser.HeldDefines);

  fDefs.CommaText := '';
  fHndlr.BeginFile('', nil);
  CheckList('', 'Cleared Defines', fParser.HeldDefines);
end;

procedure TestTD2XHeldDefinesHandler.TestDescription;
begin
  CheckEqualsString('Held Defines', fHndlr.Description, 'Description');
end;

procedure TestTD2XHeldDefinesHandler.TestEndFile;
begin
  fHndlr.EndFile('', nil);
  CheckLog('', 'End File');
end;

procedure TestTD2XHeldDefinesHandler.TestInit;
begin
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  CheckTrue(Assigned(TTestParserHandler(fHndlr).Parser), 'Parse Mode set');
  Check(fParser = TTestParserHandler(fHndlr).Parser, 'Parser set');
end;

procedure TestTD2XHeldDefinesHandler.TestUseProxy;
begin
  CheckNotInterface(ID2XFullProxy, fHndlr, 'Full proxy');
end;

{ TestTD2XLogHandler }

procedure TestTD2XLogHandler.SetUp;
begin
  inherited;

  fHndlr := TTestLogHandler.Create;
  fHndlr.StartLog(fLog);
end;

procedure TestTD2XLogHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XLogHandler.TestBeginMethod;
begin
  fHndlr.BeginMethod('Test');

  CheckLog('BEFORE Test', 'Begin Method');

  fHndlr.InitParser(fParser, fFlag.FlagRef);
  fHndlr.BeginMethod('Test');

  CheckLog('BEFORE Test @', 'Begin Method');
end;

procedure TestTD2XLogHandler.TestEndMethod;
begin
  fHndlr.EndMethod('Test');

  CheckLog('AFTER Test', 'Begin Method');
end;

procedure TestTD2XLogHandler.TestInitParser;
begin
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  CheckTrue(fHndlr.fInitParserCalled, 'Init Parser called');
end;

procedure TestTD2XLogHandler.TestOnLexerInclude;
begin
  fHndlr.TestLexerInclude('Test', 1, 2);
  CheckLog('INCLUDE @ 1,2: Test', 'Correct Parser Message');
end;

procedure TestTD2XLogHandler.TestOnParserMessage;
begin
  fHndlr.TestParserMessage(meError, 'Test', 1, 2);
  CheckLog('ERROR @ 1,2: Test', 'Correct Parser Message');
end;

procedure TestTD2XLogHandler.TestUseProxy;
begin
  CheckNotInterface(ID2XFullProxy, fHndlr, 'Full proxy');
end;

{ TestTD2XParserHandler }

procedure TestTD2XParserHandler.SetUp;
begin
  inherited;

  fHndlr := TTestParserHandler.Create;
end;

procedure TestTD2XParserHandler.TearDown;
begin
  FreeAndNil(fHndlr);

  inherited;
end;

procedure TestTD2XParserHandler.TestDoLexerInclude;
begin
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  fHndlr.TestDoLexerInclude('Test', 1, 2);
  CheckTrue(fHndlr.fLexerIncludeCalled, 'Lexer Include called');
end;

procedure TestTD2XParserHandler.TestDoParserMessage;
begin
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  fHndlr.TestDoParserMessage(meError, 'Test', 1, 2);
  CheckTrue(fHndlr.fParserMessageCalled, 'Parser Message called');
end;

procedure TestTD2XParserHandler.TestInitParser;
begin
  fHndlr.InitParser(fParser, fFlag.FlagRef);

  CheckTrue(fHndlr.fInitParserCalled, 'Init Parser called');
end;

procedure TestTD2XParserHandler.TestOnLexerInclude;
begin
  fHndlr.TestOnLexerInclude('Test', 1, 2);
  CheckTrue(fHndlr.fLexerIncludeCalled, 'Lexer Include called');
end;

procedure TestTD2XParserHandler.TestOnParserMessage;
begin
  fHndlr.TestOnParserMessage(meError, 'Test', 1, 2);
  CheckTrue(fHndlr.fParserMessageCalled, 'Parser Message called');
end;

{ TestHandlerUtils }

function TestHandlerUtils.PairFormat(pPair: TStrIntPair): string;
begin
  Result := pPair.Key + ' = ' + IntToStr(pPair.Value);
end;

procedure TestHandlerUtils.TestOutputStrIntDict;
var
  lDict: TStrIntDict;
begin
  lDict := TStrIntDict.Create;
  try
    OutputStrIntDict(lDict, fDS.WriteTo, PairFormat);
    CheckStream('', 'No Entries');
    lDict.Add('Test', 1);
    OutputStrIntDict(lDict, fDS.WriteTo, PairFormat);
    CheckStream('Test=Test = 1', 'An Entry');
  finally
    lDict.Free;
  end;
end;

procedure TestHandlerUtils.TestOutputStrIntDictNoDict;
begin
  StartExpectingException(EAssertionFailed);
  try
    OutputStrIntDict(nil, fDS.WriteTo, PairFormat);
  except
    on E: EAssertionFailed do
    begin
      CheckTrue(StartsText('Need a Dictionary', E.Message), 'Exception message');
      raise;
    end;
  end;
end;

procedure TestHandlerUtils.TestOutputStrIntDictNoFunc;
begin
  StartExpectingException(EAssertionFailed);
  try
    OutputStrIntDict(nil, fDS.WriteTo, nil);
  except
    on E: EAssertionFailed do
    begin
      CheckTrue(StartsText('Need a Function', E.Message), 'Exception message');
      raise;
    end;
  end;
end;

procedure TestHandlerUtils.TestOutputStrIntDictNoStream;
begin
  StartExpectingException(EAssertionFailed);
  try
    OutputStrIntDict(nil, nil, nil);
  except
    on E: EAssertionFailed do
    begin
      CheckTrue(StartsText('Need a Stream', E.Message), 'Exception message');
      raise;
    end;
  end;
end;

initialization

// Register any test cases with the test runner
RegisterTests('Handlers', [TestHandlerUtils.Suite, TestTD2XParserHandler.Suite,
  TestTD2XCountChildrenHandler.Suite, TestTD2XCountFinalDefinesHandler.Suite,
  TestTD2XCountDefinesUsedHandler.Suite, TestTD2XParserDefinesHandler.Suite,
  TestTD2XHeldDefinesHandler.Suite, TestTD2XErrorHandler.Suite, TestTD2XLogHandler.Suite,
  TestTD2XSkipHandler.Suite, TestTD2XWriteDefinesHandler.Suite, TestTD2XTreeHandler.Suite]);

//InitStringListLoad;

end.
