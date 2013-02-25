unit Test.Handler;

interface

uses
  CastaliaPasLexTypes,
  D2X.Handler,
  D2X.IO,
  D2X.Parser;

type
  TD2XHandlerTester = class(TD2XHandler, ID2XHandler, ID2XProcessingHandler, ID2XFileHandler,
    ID2XResultsHandler, ID2XMethods, ID2XChecks, ID2XMessages)
  public
    CalledEndProcessing: Boolean;
    CalledBeginFile: Boolean;
    CalledEndFile: Boolean;
    CalledBeginResults: Boolean;
    CalledEndResults: Boolean;
    CalledCheckBeforeMethod: Boolean;
    CalledCheckAfterMethod: Boolean;
    CalledBeginMethod: Boolean;
    CalledEndMethod: Boolean;
    CalledLexerInclude: Boolean;
    CalledParserMessage: Boolean;

    CreateStreams: Boolean;

    function Description: String; override;
    procedure EndProcessing(pOutput: TStreamWriterRef);
    procedure BeginFile(pFile: String; pInput: TStreamReaderRef);
    procedure EndFile(pFile: String; pOutput: TStreamWriterRef);
    procedure BeginResults;
    procedure EndResults(pOutput: TStreamWriterRef);
    function CheckBeforeMethod(pMethod: String): Boolean;
    function CheckAfterMethod(pMethod: String): Boolean;
    procedure BeginMethod(pMethod: String);
    procedure EndMethod(pMethod: String);
    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: String;
      pX, pY: Integer);
    procedure LexerInclude(const pFile: String; pX, pY: Integer);

  end;

  TD2XParserHandlerTester = class(TD2XHandler, ID2XParser)
  public
    CalledInitParser: Boolean;

    procedure InitParser(pParser: TD2XDefinesParser);
  end;

implementation

{ TD2XHandlerTester }

procedure TD2XHandlerTester.BeginMethod(pMethod: String);
begin
  CalledBeginMethod := true;
  inherited;
end;

procedure TD2XHandlerTester.BeginFile(pFile: String; pInput: TStreamReaderRef);
begin
  CalledBeginFile := true;
  inherited;
  if CreateStreams then
    pInput;
end;

procedure TD2XHandlerTester.BeginResults;
begin
  CalledBeginResults := true;
  inherited;
end;

function TD2XHandlerTester.CheckAfterMethod(pMethod: String): Boolean;
begin
  CalledCheckAfterMethod := true;

  Result := True;
end;

function TD2XHandlerTester.CheckBeforeMethod(pMethod: String): Boolean;
begin
  CalledCheckBeforeMethod := true;

  Result := True;
end;

function TD2XHandlerTester.Description: String;
begin
  inherited;
  Result := 'Handler Tester';
end;

procedure TD2XHandlerTester.EndMethod(pMethod: String);
begin
  CalledEndMethod := true;
  inherited;
end;

procedure TD2XHandlerTester.EndFile(pFile: String; pOutput: TStreamWriterRef);
begin
  CalledEndFile := true;
  inherited;
  if CreateStreams then
    pOutput;
end;

procedure TD2XHandlerTester.EndProcessing(pOutput: TStreamWriterRef);
begin
  CalledEndProcessing := true;
  inherited;
  if CreateStreams then
    pOutput;
end;

procedure TD2XHandlerTester.EndResults(pOutput: TStreamWriterRef);
begin
  CalledEndResults := true;
  inherited;
  if CreateStreams then
    pOutput;
end;

procedure TD2XHandlerTester.LexerInclude(const pFile: String; pX, pY: Integer);
begin
  CalledLexerInclude := true;
  inherited;
end;

procedure TD2XHandlerTester.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: String; pX, pY: Integer);
begin
  CalledParserMessage := true;
  inherited;
end;

{ TD2XParserHandlerTester }

procedure TD2XParserHandlerTester.InitParser(pParser: TD2XDefinesParser);
begin
  CalledInitParser := true;
end;

end.
