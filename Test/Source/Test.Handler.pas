unit Test.Handler;

interface

uses
  CastaliaPasLexTypes,
  D2X.Handler,
  D2X.IO,
  D2X.Parser;

type
  TD2XHandlerTester = class(TD2XHandler)
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
    CalledCopy: Boolean;
    CalledLexerInclude: Boolean;
    CalledParserMessage: Boolean;

    CreateStreams: Boolean;

    function Description: String; override;
    function UseProxy: Boolean; override;
    procedure BeginProcessing(pInput: TStreamReaderRef); override;
    procedure EndProcessing(pOutput: TStreamWriterRef); override;
    procedure BeginFile(pFile: String; pInput: TStreamReaderRef); override;
    procedure EndFile(pFile: String; pOutput: TStreamWriterRef); override;
    procedure BeginResults; override;
    procedure EndResults(pOutput: TStreamWriterRef); override;
    function CheckBeforeMethod(pMethod: String): Boolean; override;
    function CheckAfterMethod(pMethod: String): Boolean; override;
    procedure BeginMethod(pMethod: String); override;
    procedure EndMethod(pMethod: String); override;
    procedure Copy(pFrom: TD2XHandler); override;
    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: String;
      pX, pY: Integer); override;
    procedure LexerInclude(const pFile: String; pX, pY: Integer); override;

  end;

  TD2XParserHandlerTester = class(TD2XParserHandler)
  public
    CalledInitParser: Boolean;

    procedure InitParser(pParser: TD2XDefinesParser); override;
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

procedure TD2XHandlerTester.BeginProcessing(pInput: TStreamReaderRef);
begin
  CalledBeginProcessing := true;
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

  Result := inherited CheckAfterMethod(pMethod);
end;

function TD2XHandlerTester.CheckBeforeMethod(pMethod: String): Boolean;
begin
  CalledCheckBeforeMethod := true;

  Result := inherited CheckBeforeMethod(pMethod);
end;

procedure TD2XHandlerTester.Copy(pFrom: TD2XHandler);
begin
  CalledCopy := true;
  inherited;
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

function TD2XHandlerTester.UseProxy: Boolean;
begin
  CalledUseProxy := True;
  Result := inherited UseProxy;
end;

{ TD2XParserHandlerTester }

procedure TD2XParserHandlerTester.InitParser(pParser: TD2XDefinesParser);
begin
  CalledInitParser := True;
  inherited;
end;

end.
