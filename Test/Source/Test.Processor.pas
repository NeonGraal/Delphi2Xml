unit Test.Processor;

interface

uses
  CastaliaPasLexTypes,
  D2X.Param,
  D2X.Parser,
  D2X.Processor,
  Test.Global;

type
  TTestProcessor = class(TD2XProcessor)
  public
    CalledUseProxy: Boolean;
    CalledSetParser: Boolean;
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
    procedure SetParser(pParser: TD2XDefinesParser); override;
    procedure EndProcessing; override;
    procedure BeginFile(pFile: string); override;
    procedure EndFile(pFile: string); override;
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

type
  TFlagTestCase = class(TStringTestCase)
  strict private
    fFlag: TD2XBoolFlag;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  protected
    fActive: ID2XFlag;
  end;

implementation

uses
  System.SysUtils;

{ TTestProcessor }

procedure TTestProcessor.BeginFile(pFile: string);
begin
  CalledBeginFile := True;
  inherited;
end;

procedure TTestProcessor.BeginMethod(pMethod: string);
begin
  CalledBeginMethod := True;
  inherited;
end;

procedure TTestProcessor.BeginResults;
begin
  CalledBeginResults := True;
  inherited;
end;

function TTestProcessor.CheckAfterMethod(pMethod: string): Boolean;
begin
  CalledCheckAfterMethod := True;
  Result := inherited CheckAfterMethod(pMethod);
end;

function TTestProcessor.CheckBeforeMethod(pMethod: string): Boolean;
begin
  CalledCheckBeforeMethod := True;
  Result := inherited CheckBeforeMethod(pMethod);
end;

procedure TTestProcessor.EndFile(pFile: string);
begin
  CalledEndFile := True;
  inherited;
end;

procedure TTestProcessor.EndMethod(pMethod: string);
begin
  CalledEndMethod := True;
  inherited;
end;

procedure TTestProcessor.EndProcessing;
begin
  CalledEndProcessing := True;
  inherited;
end;

procedure TTestProcessor.EndResults(pFile: string);
begin
  CalledEndResults := True;
  inherited;
end;

procedure TTestProcessor.LexerInclude(const pFile: string; pX, pY: Integer);
begin
  CalledLexerInclude := True;
  inherited;
end;

procedure TTestProcessor.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  CalledParserMessage := True;
  inherited;
end;

procedure TTestProcessor.SetParser(pParser: TD2XDefinesParser);
begin
  CalledSetParser := True;
  inherited;
end;

function TTestProcessor.UseProxy: Boolean;
begin
  CalledUseProxy := True;
  Result := inherited UseProxy;
end;

{ TFlagTestCase }

procedure TFlagTestCase.SetUp;
begin
  inherited;

  fFlag := TD2XBoolFlag.Create;
  fActive := fFlag;
end;

procedure TFlagTestCase.TearDown;
begin
  fActive := nil;
  FreeAndNil(fFlag);

  inherited;
end;

end.
