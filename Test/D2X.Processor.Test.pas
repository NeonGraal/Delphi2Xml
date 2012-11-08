unit D2X.Processor.Test;

interface

uses
  CastaliaPasLexTypes,
  D2X.Param,
  D2X.Processor;

type
  TTestBoolFlag = class(TInterfacedObject, IParamFlag)
  private
    fFlag: Boolean;

    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);
  end;

  TTestProcessor = class(TD2XProcessor)
  public
    CalledUseProxy: Boolean;
    CalledBeginProcessing: Boolean;
    CalledEndProcessing: Boolean;
    CalledBeginFile: Boolean;
    CalledEndFile: Boolean;
    CalledBeginResults: Boolean;
    CalledEndResults: Boolean;
    CalledCheckBeforeMethod: Boolean;
    CalledCheckAfterMethod: Boolean;
    CalledBeginMethod: Boolean;
    CalledEndMethod: Boolean;
    CalledParserMessage: Boolean;
    CalledLexerInclude: Boolean;

    function UseProxy: Boolean; override;
    procedure BeginProcessing; override;
    procedure EndProcessing; override;
    procedure BeginFile; override;
    procedure EndFile; override;
    procedure BeginResults; override;
    procedure EndResults(pFile: string); override;
    function CheckBeforeMethod(pMethod: string): Boolean; override;
    function CheckAfterMethod(pMethod: string): Boolean; override;
    procedure BeginMethod(pMethod: string); override;
    procedure EndMethod(pMethod: string); override;
    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer); override;
    procedure LexerInclude(const pFile: string; pX, pY: Integer); override;

  end;


implementation

uses
  TestFramework,
  D2X.Xml,
  D2X.Test,
  D2X.Handler,
  D2X.Handler.Test,
  D2X.Options,
  D2X.Parser,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.SysUtils,
  System.Rtti;

type
  TestTD2XProcessor = class(TStringTestCase)
  strict private
    FD2XProcessor: TTestProcessor;
    fFlag: TTestBoolFlag;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUseProxy;
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

  fFlag := TTestBoolFlag.Create;
  FD2XProcessor := TTestProcessor.Create(fFlag);
end;

procedure TestTD2XProcessor.TearDown;
begin
  FD2XProcessor := nil;
  fFlag := nil;

  inherited;
end;

procedure TestTD2XProcessor.TestBeginFile;
begin
  FD2XProcessor.BeginFile;

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
  FD2XProcessor.EndFile;

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

procedure TestTD2XProcessor.TestUseProxy;
begin
  CheckTrue(FD2XProcessor.UseProxy, 'Call Use Proxy');

  CheckTrue(FD2XProcessor.CalledUseProxy, 'Called Use Proxy');
end;

{ TTestBoolFlag }

function TTestBoolFlag.GetFlag: Boolean;
begin
  Result := fFlag;
end;

procedure TTestBoolFlag.SetFlag(pVal: Boolean);
begin
  fFlag := pVal;
end;

{ TTestProcessor }

procedure TTestProcessor.BeginFile;
begin
  CalledBeginFile := True;
end;

procedure TTestProcessor.BeginMethod(pMethod: string);
begin
  CalledBeginMethod := True;
end;

procedure TTestProcessor.BeginProcessing;
begin
  CalledBeginProcessing := True;
end;

procedure TTestProcessor.BeginResults;
begin
  CalledBeginResults := True;
end;

function TTestProcessor.CheckAfterMethod(pMethod: string): Boolean;
begin
  Result := True;
  CalledCheckAfterMethod := True;
end;

function TTestProcessor.CheckBeforeMethod(pMethod: string): Boolean;
begin
  Result := True;
  CalledCheckBeforeMethod := True;
end;

procedure TTestProcessor.EndFile;
begin
  CalledEndFile := True;
end;

procedure TTestProcessor.EndMethod(pMethod: string);
begin
  CalledEndMethod := True;
end;

procedure TTestProcessor.EndProcessing;
begin
  CalledEndProcessing := True;
end;

procedure TTestProcessor.EndResults(pFile: string);
begin
  CalledEndResults := True;
end;

procedure TTestProcessor.LexerInclude(const pFile: string; pX, pY: Integer);
begin
  CalledLexerInclude := True;
end;

procedure TTestProcessor.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  CalledParserMessage := True;
end;

function TTestProcessor.UseProxy: Boolean;
begin
  CalledUseProxy := True;
  Result := True;
end;

initialization

// Register any test cases with the test runner
RegisterTests('Processors', [TestTD2XProcessor.Suite]);

end.
