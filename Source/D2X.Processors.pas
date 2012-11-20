unit D2X.Processors;

interface

uses
  CastaliaPasLexTypes,
  D2X,
  D2X.Param,
  D2X.Parser,
  D2X.Handler,
  D2X.Processor,
  D2X.Stream,
  System.Generics.Collections;

type
  TD2XLexerProcessor = class(TD2XProcessor)
  protected
    fLexer: TD2XLexer;
  public
    procedure SetParser(pParser: TD2XDefinesParser); override;
  end;

  TD2XLogProcessor = class(TD2XLexerProcessor)
  public
    function UseProxy: Boolean; override;

    procedure BeginMethod(pMethod: string); override;
    procedure EndMethod(pMethod: string); override;

    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer); override;
    procedure LexerInclude(const pFile: string; pX, pY: Integer); override;

  end;

  TD2XHandlerProcessor = class(TD2XProcessor)
  public
    constructor CreateHandler(pActive: ID2XFlag; pHandler: TD2XHandler; pMine: Boolean);
    constructor CreateClass(pActive: ID2XFlag; pHandler: TD2XHandlerClass);
    destructor Destroy; override;

    function UseProxy: Boolean; override;

    procedure SetParser(pParser: TD2XDefinesParser); override;

    procedure BeginProcessing; override;
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

    function SetProcessingInput(pFilename: TD2XStreamRef): TD2XHandlerProcessor;
    function SetProcessingOutput(pFilename: TD2XStreamRef): TD2XHandlerProcessor;
    function SetResultsOutput(pFilename: TD2XNamedStreamRef): TD2XHandlerProcessor;
    function SetFileInput(pFilename: TD2XStreamRef): TD2XHandlerProcessor;
    function SetFileOutput(pFilename: TD2XStreamRef): TD2XHandlerProcessor;

  private
    fMyHandler: Boolean;
    fHandler: TD2XHandler;

    fProcessingInput: TD2XStreamRef;
    fProcessingOutput: TD2XStreamRef;
    fResultsOutput: TD2XNamedStreamRef;
    fFileInput: TD2XStreamRef;
    fFileOutput: TD2XStreamRef;

  end;

implementation

uses
  D2X.Handlers,
  System.Classes,
  System.IOUtils,
  System.SysUtils;

{ TD2XHandlerProcessor  }

procedure TD2XHandlerProcessor.BeginFile(pFile: string);
var
  lS: TD2XStream;
begin
  lS := nil;
  if fActive.Flag then
    if Assigned(fFileInput) then
      try
        fHandler.BeginFile(pFile,
            function: TStreamReader
          begin
            Result := nil;
            lS := fFileInput;
            if Assigned(lS) then
            if lS.Exists then
              Result := lS.ReadFrom
            else
              Log('WARNING: %1 file "%2" not found', [fHandler.Description, lS.Description]);
          end);
      finally
        FreeAndNil(lS);
      end
    else
      fHandler.BeginFile(pFile, nil);
end;

procedure TD2XHandlerProcessor.BeginMethod(pMethod: string);
begin
  if fActive.Flag then
    fHandler.BeginMethod(pMethod);
end;

procedure TD2XHandlerProcessor.BeginProcessing;
var
  lS: TD2XStream;
begin
  lS := nil;
  if fActive.Flag then
    if Assigned(fProcessingInput) then
      try
        fHandler.BeginProcessing(
            function: TStreamReader
          begin
            Result := nil;
            lS := fProcessingInput;
            if Assigned(lS) then
              if lS.Exists then
                Result := lS.ReadFrom
              else
                Log('WARNING: %1 file "%2" not found', [fHandler.Description, lS.Description]);
          end);
      finally
        FreeAndNil(lS);
      end
    else
      fHandler.BeginProcessing(nil);
end;

procedure TD2XHandlerProcessor.BeginResults;
begin
  if fActive.Flag then
    fHandler.BeginResults;
end;

function TD2XHandlerProcessor.CheckAfterMethod(pMethod: string): Boolean;
begin
  Result := not fActive.Flag or fHandler.CheckAfterMethod(pMethod);
end;

function TD2XHandlerProcessor.CheckBeforeMethod(pMethod: string): Boolean;
begin
  Result := not fActive.Flag or fHandler.CheckBeforeMethod(pMethod);
end;

constructor TD2XHandlerProcessor.CreateClass(pActive: ID2XFlag;
  pHandler: TD2XHandlerClass);
begin
  inherited Create(pActive);

  fMyHandler := True;
  fHandler := pHandler.Create;
end;

constructor TD2XHandlerProcessor.CreateHandler(pActive: ID2XFlag; pHandler: TD2XHandler;
  pMine: Boolean);
begin
  inherited Create(pActive);

  fMyHandler := pMine;
  fHandler := pHandler;
end;

destructor TD2XHandlerProcessor.Destroy;
begin
  if fMyHandler then
    FreeAndNil(fHandler);

  inherited;
end;

procedure TD2XHandlerProcessor.EndFile(pFile: string);
var
  lS: TD2XStream;
begin
  lS := nil;
  if fActive.Flag then
    if Assigned(fFileOutput) then
      try
        fHandler.EndFile(pFile,
            function: TStreamWriter
          begin
            lS := fFileOutput;
            if Assigned(lS) then
              Result := lS.WriteTo
            else
              Result := nil;
          end);
      finally
        FreeAndNil(lS);
      end
    else
      fHandler.EndFile(pFile, nil);
end;

procedure TD2XHandlerProcessor.EndMethod(pMethod: string);
begin
  if fActive.Flag then
    fHandler.EndMethod(pMethod);
end;

procedure TD2XHandlerProcessor.EndProcessing;
var
  lS: TD2XStream;
begin
  lS := nil;
  if fActive.Flag then
    if Assigned(fProcessingOutput) then
      try
        fHandler.EndProcessing(
            function: TStreamWriter
          begin
            lS := fProcessingOutput;
            if Assigned(lS) then
              Result := lS.WriteTo
            else
              Result := nil;
          end);
      finally
        FreeAndNil(lS);
      end
    else
      fHandler.EndProcessing(nil);
end;

procedure TD2XHandlerProcessor.EndResults(pFile: string);
var
  lS: TD2XStream;
begin
  lS := nil;
  if fActive.Flag then
    if Assigned(fResultsOutput) then
      try
        fHandler.EndResults(
            function: TStreamWriter
          begin
            lS := fResultsOutput(pFile);
            if Assigned(lS) then
              Result := lS.WriteTo
            else
              Result := nil;
          end);
      finally
        FreeAndNil(lS);
      end
    else
      fHandler.EndResults(nil);
end;

procedure TD2XHandlerProcessor.LexerInclude(const pFile: string; pX, pY: Integer);
begin
  if fActive.Flag then
    fHandler.LexerInclude(pFile, pX, pY);
end;

procedure TD2XHandlerProcessor.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  if fActive.Flag then
    fHandler.ParserMessage(pTyp, pMsg, pX, pY);
end;

function TD2XHandlerProcessor.SetFileInput(pFilename: TD2XStreamRef)
  : TD2XHandlerProcessor;
begin
  fFileInput := pFilename;
  Result := Self;
end;

function TD2XHandlerProcessor.SetFileOutput(pFilename: TD2XStreamRef)
  : TD2XHandlerProcessor;
begin
  fFileOutput := pFilename;
  Result := Self;
end;

procedure TD2XHandlerProcessor.SetParser(pParser: TD2XDefinesParser);
begin
  inherited;

  if fHandler is TD2XParserHandler then
    TD2XParserHandler(fHandler).InitParser(pParser);
end;

function TD2XHandlerProcessor.SetProcessingInput(pFilename: TD2XStreamRef)
  : TD2XHandlerProcessor;
begin
  fProcessingInput := pFilename;
  Result := Self;
end;

function TD2XHandlerProcessor.SetProcessingOutput(pFilename: TD2XStreamRef)
  : TD2XHandlerProcessor;
begin
  fProcessingOutput := pFilename;
  Result := Self;
end;

function TD2XHandlerProcessor.SetResultsOutput(pFilename: TD2XNamedStreamRef)
  : TD2XHandlerProcessor;
begin
  fResultsOutput := pFilename;
  Result := Self;
end;

function TD2XHandlerProcessor.UseProxy: Boolean;
begin
  Result := fActive.Flag and fHandler.UseProxy;
end;

{ TD2XLogProcessor }

procedure TD2XLogProcessor.BeginMethod(pMethod: string);
begin
  if fActive.Flag then
  begin
    if Assigned(fLexer) then
      Log('BEFORE %s @ %s', [pMethod, fLexer.Token])
    else
      Log('BEFORE %s ', [pMethod]);
  end;
end;

procedure TD2XLogProcessor.EndMethod(pMethod: string);
begin
  if fActive.Flag then
    Log('AFTER  %s', [pMethod]);
end;

procedure TD2XLogProcessor.LexerInclude(const pFile: string; pX, pY: Integer);
begin
  if fActive.Flag then
    Log('INCLUDE @ %d,%d: %s', [pX, pY, pFile]);
end;

procedure TD2XLogProcessor.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  if fActive.Flag then
    case pTyp of
      meError:
        Log('ERROR @ %d,%d: %s', [pX, pY, pMsg]);
      meNotSupported:
        Log('NOT SUPPORTED @ %d,%d: %s', [pX, pY, pMsg]);
    else
      Log('???? @ %d,%d: %s', [pX, pY, pMsg]);
    end;
end;

function TD2XLogProcessor.UseProxy: Boolean;
begin
  Result := fActive.Flag;
end;

{ TD2XLexerProcessor }

procedure TD2XLexerProcessor.SetParser(pParser: TD2XDefinesParser);
begin
  inherited;

  fLexer := pParser.Lexer;
end;

end.
