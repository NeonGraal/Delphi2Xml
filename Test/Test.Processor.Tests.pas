unit Test.Processor.Tests;

interface

implementation

uses
  CastaliaPasLexTypes,

  D2X.IO,
  D2X.Param,

  D2X.Processor,
  System.SysUtils,
  Test.Global,
  Test.Handler,

  Test.Processor,
  TestFramework;

type
  TestTD2XProcessor = class(TFlagTestCase)
  strict private
    FHandler: TD2XHandlerTester;
    FD2XProcessor: TD2XProcessor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInvalidCreate;

    procedure TestCreateClass;

    procedure TestSetProcessingOutput;
    procedure TestSetResultsOutput;
    procedure TestSetFileInput;
    procedure TestSetFileOutput;

    procedure TestSetParser;
    procedure TestUseProxy;

    procedure TestEndProcessing;

    procedure TestBeginFile;
    procedure TestEndFile;

    procedure TestBeginResults;
    procedure TestEndResults;

    procedure TestAddAttr;
    procedure TestAddText;
    procedure TestRollBackTo;
    procedure TestTrimChildren;

    procedure TestCheckBeforeMethod;
    procedure TestCheckAfterMethod;

    procedure TestBeginMethod;
    procedure TestEndMethod;

    //    procedure TestLexerInclude;
    //    procedure TestParserMessage;
  end;

  TestTD2XParserHandlerProcessor = class(TFlagTestCase)
  strict private
    FHandler: TD2XParserHandlerTester;
    FD2XProcessor: TD2XProcessor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetParser;
  end;

function NilFile: ID2XIOFile;
begin
  Result := nil;
end;

function NilNamedFile(pName: string): ID2XIOFile;
begin
  Result := nil;
end;

{ TestTD2XProcessor }

procedure TestTD2XProcessor.SetUp;
begin
  inherited;

  FHandler := TD2XHandlerTester.Create;
  FD2XProcessor := TD2XProcessor.CreateHandler(fActive, FHandler, True);
end;

procedure TestTD2XProcessor.TearDown;
begin
  FreeAndNil(FD2XProcessor);

  inherited;
end;

procedure TestTD2XProcessor.TestAddAttr;
begin
  FD2XProcessor.AddAttr('Test', 'test');
  CheckFalse(FHandler.CalledAddAttr, 'Ignored Add Attr');

  fActive.Flag := True;
  FD2XProcessor.AddAttr('Test', 'test');
  CheckTrue(FHandler.CalledAddAttr, 'Called Add Attr');
end;

procedure TestTD2XProcessor.TestAddText;
begin
  FD2XProcessor.AddText('Test');
  CheckFalse(FHandler.CalledAddText, 'Ignored Add Text');

  fActive.Flag := True;
  FD2XProcessor.AddText('Test');
  CheckTrue(FHandler.CalledAddText, 'Called Add Text');
end;

procedure TestTD2XProcessor.TestBeginFile;
begin
  FD2XProcessor.SetFileInput(NilFile);

  FD2XProcessor.BeginFile('');
  CheckFalse(FHandler.CalledBeginFile, 'Ignored Begin File');

  fActive.Flag := True;
  FD2XProcessor.BeginFile('');
  CheckTrue(FHandler.CalledBeginFile, 'Called Begin File');
end;

procedure TestTD2XProcessor.TestBeginMethod;
begin
  FD2XProcessor.BeginMethod('');
  CheckFalse(FHandler.CalledBeginMethod, 'Ignored Begin Method');

  fActive.Flag := True;
  FD2XProcessor.BeginMethod('');
  CheckTrue(FHandler.CalledBeginMethod, 'Called Begin Method');
end;

procedure TestTD2XProcessor.TestBeginResults;
begin
  FD2XProcessor.BeginResults;
  CheckFalse(FHandler.CalledBeginResults, 'Ignored Begin Results');

  fActive.Flag := True;
  FD2XProcessor.BeginResults;
  CheckTrue(FHandler.CalledBeginResults, 'Called Begin Results');
end;

procedure TestTD2XProcessor.TestCheckAfterMethod;
begin
  FD2XProcessor.CheckAfterMethod('');
  CheckFalse(FHandler.CalledCheckAfterMethod, 'Ignored Check After Method');

  fActive.Flag := True;
  FD2XProcessor.CheckAfterMethod('');
  CheckTrue(FHandler.CalledCheckAfterMethod, 'Called Check After Method');
end;

procedure TestTD2XProcessor.TestCheckBeforeMethod;
begin
  FD2XProcessor.CheckBeforeMethod('');
  CheckFalse(FHandler.CalledCheckBeforeMethod, 'Ignored Check Before Method');

  fActive.Flag := True;
  FD2XProcessor.CheckBeforeMethod('');
  CheckTrue(FHandler.CalledCheckBeforeMethod, 'Called Check Before Method');
end;

procedure TestTD2XProcessor.TestCreateClass;
begin
  FreeAndNil(FD2XProcessor);
  FD2XProcessor := TD2XProcessor.CreateClass(fActive, TD2XHandlerTester);
  CheckTrue(FD2XProcessor.HandlerIs(TD2XHandlerTester), 'Check Handler class');
end;

procedure TestTD2XProcessor.TestEndFile;
begin
  FD2XProcessor.SetFileOutput(NilFile);

  FD2XProcessor.EndFile('');
  CheckFalse(FHandler.CalledEndFile, 'Ignored End File');

  fActive.Flag := True;
  FD2XProcessor.EndFile('');
  CheckTrue(FHandler.CalledEndFile, 'Called End File');
end;

procedure TestTD2XProcessor.TestEndMethod;
begin
  FD2XProcessor.EndMethod('');
  CheckFalse(FHandler.CalledEndMethod, 'Ignored End Method');

  fActive.Flag := True;
  FD2XProcessor.EndMethod('');
  CheckTrue(FHandler.CalledEndMethod, 'Called End Method');
end;

procedure TestTD2XProcessor.TestEndProcessing;
begin
  FD2XProcessor.SetProcessingOutput(NilFile);

  FD2XProcessor.EndProcessing;
  CheckFalse(FHandler.CalledEndProcessing, 'Ignored End Processing');

  fActive.Flag := True;
  FD2XProcessor.EndProcessing;
  CheckTrue(FHandler.CalledEndProcessing, 'Called End Processing');
end;

procedure TestTD2XProcessor.TestEndResults;
begin
  FD2XProcessor.SetResultsOutput(NilNamedFile);

  FD2XProcessor.EndResults('');
  CheckFalse(FHandler.CalledEndResults, 'Ignored End Results');

  fActive.Flag := True;
  FD2XProcessor.EndResults('');
  CheckTrue(FHandler.CalledEndResults, 'Called End Results');
end;

procedure TestTD2XProcessor.TestInvalidCreate;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XProcessor.Create',
      procedure
    begin
      TD2XProcessor.Create;
    end);
end;

//procedure TestTD2XProcessor.TestLexerInclude;
//begin
//  FD2XProcessor.LexerInclude('Test', 1, 2);
//  CheckFalse(FHandler.CalledLexerInclude, 'Ignored Lexer Include');
//
//  fActive.Flag := True;
//  FD2XProcessor.LexerInclude('Test', 1, 2);
//  CheckTrue(FHandler.CalledLexerInclude, 'Called Lexer Include');
//end;
//
//procedure TestTD2XProcessor.TestParserMessage;
//begin
//  FD2XProcessor.ParserMessage(meNotSupported, 'Test', 1, 2);
//  CheckFalse(FHandler.CalledParserMessage, 'Ignored Parser Message');
//
//  fActive.Flag := True;
//  FD2XProcessor.ParserMessage(meNotSupported, 'Test', 1, 2);
//  CheckTrue(FHandler.CalledParserMessage, 'Called Parser Message');
//end;

procedure TestTD2XProcessor.TestRollBackTo;
begin
  FD2XProcessor.RollBackTo('Test');
  CheckFalse(FHandler.CalledRollBackTo, 'Ignored RollBack To');

  fActive.Flag := True;
  FD2XProcessor.RollBackTo('Test');
  CheckTrue(FHandler.CalledRollBackTo, 'Called RollBack To');
end;

procedure TestTD2XProcessor.TestSetFileInput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;

  FD2XProcessor.SetFileInput(
    function: ID2XIOFile
    begin
      lCalled := True;
      Result := nil;
    end);

  FD2XProcessor.BeginFile('');
  CheckFalse(lCalled, 'Ignored File Input');

  fActive.Flag := True;
  FD2XProcessor.BeginFile('');
  CheckTrue(lCalled, 'Called File Input');
end;

procedure TestTD2XProcessor.TestSetFileOutput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;

  FD2XProcessor.SetFileOutput(
    function: ID2XIOFile
    begin
      lCalled := True;
      Result := nil;
    end);

  FD2XProcessor.EndFile('');
  CheckFalse(lCalled, 'Ignored File Output');

  fActive.Flag := True;
  FD2XProcessor.EndFile('');
  CheckTrue(lCalled, 'Called File Output');
end;

procedure TestTD2XProcessor.TestSetParser;
begin
  FD2XProcessor.InitParser(nil);
  CheckTrue(True, 'Init Parser missing');
end;

procedure TestTD2XProcessor.TestSetProcessingOutput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;
  FD2XProcessor.SetProcessingOutput(
    function: ID2XIOFile
    begin
      lCalled := True;
      Result := nil;
    end);

  FD2XProcessor.EndProcessing;
  CheckFalse(lCalled, 'Ignored Processing Output');

  fActive.Flag := True;
  FD2XProcessor.EndProcessing;
  CheckTrue(lCalled, 'Called Processing Output');
end;

procedure TestTD2XProcessor.TestSetResultsOutput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;
  FD2XProcessor.SetResultsOutput(
    function(pFile: string): ID2XIOFile
    begin
      lCalled := True;
      Result := nil;
    end);

  FD2XProcessor.EndResults('');
  CheckFalse(lCalled, 'Ignored Results Output');

  fActive.Flag := True;
  FD2XProcessor.EndResults('');
  CheckTrue(lCalled, 'Called Results Output');
end;

procedure TestTD2XProcessor.TestTrimChildren;
begin
  FD2XProcessor.TrimChildren('Test');
  CheckFalse(FHandler.CalledTrimChildren, 'Ignored Trim Children');

  fActive.Flag := True;
  FD2XProcessor.TrimChildren('Test');
  CheckTrue(FHandler.CalledTrimChildren, 'Called Trim Children');
end;

procedure TestTD2XProcessor.TestUseProxy;
begin
  CheckFalse(FD2XProcessor.UseProxy, 'Dont Use Proxy');

  fActive.Flag := True;
  CheckFalse(FD2XProcessor.UseProxy, 'Use Proxy');
end;

{ TestTD2XParserHandlerProcessor }

procedure TestTD2XParserHandlerProcessor.SetUp;
begin
  inherited;

  FHandler := TD2XParserHandlerTester.Create;
  FD2XProcessor := TD2XProcessor.CreateHandler(fActive, FHandler, True);
end;

procedure TestTD2XParserHandlerProcessor.TearDown;
begin
  FreeAndNil(FD2XProcessor);

  inherited;
end;

procedure TestTD2XParserHandlerProcessor.TestSetParser;
begin
  FD2XProcessor.InitParser(nil);
  CheckTrue(FHandler.CalledInitParser, 'Called Init Parser');
end;

initialization

RegisterTests('Processors', [TestTD2XProcessor.Suite, TestTD2XParserHandlerProcessor.Suite]);

end.
