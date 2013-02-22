unit D2X.Processors;

interface

uses
  CastaliaPasLexTypes,
  D2X.Global,
  D2X.Param,
  D2X.Parser,
  D2X.Handler,
  D2X.Processor,
  D2X.IO,
  System.Generics.Collections;

type
  TD2XHandlerProcessor = class(TD2XProcessor)
  public
    constructor Create; override;
    constructor CreateActive(pActive: ID2XFlag); override;
    constructor CreateHandler(pActive: ID2XFlag; pHandler: TD2XHandler; pMine: Boolean);
    constructor CreateClass(pActive: ID2XFlag; pHandler: TD2XHandlerClass);
    destructor Destroy; override;

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

    function SetProcessingOutput(pFilename: TD2XFileRef): TD2XHandlerProcessor;
    function SetResultsOutput(pFilename: TD2XNamedStreamRef): TD2XHandlerProcessor;
    function SetFileInput(pFilename: TD2XFileRef): TD2XHandlerProcessor;
    function SetFileOutput(pFilename: TD2XFileRef): TD2XHandlerProcessor;

    function HandlerIs(pHandler: TD2XHandlerClass): Boolean;
  private
    fMyHandler: Boolean;
    fHandler: TD2XHandler;

    fProcessingInput: TD2XFileRef;
    fProcessingOutput: TD2XFileRef;
    fResultsOutput: TD2XNamedStreamRef;
    fFileInput: TD2XFileRef;
    fFileOutput: TD2XFileRef;

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
  lS: ID2XFile;
  lFls: ID2XFileHandler;
begin
  lS := nil;
  if fHandler.GetInterface(ID2XFileHandler,lFls) and fActive.Flag then
    if Assigned(fFileInput) then
      try
        lFls.BeginFile(pFile,
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
        DisposeOf(lS);
      end
    else
      lFls.BeginFile(pFile, nil);
end;

procedure TD2XHandlerProcessor.BeginMethod(pMethod: string);
var
  lMthds: ID2XMethodHandler;
begin
  if fHandler.GetInterface(ID2XMethodHandler, lMthds) and fActive.Flag then
    lMthds.BeginMethod(pMethod);
end;

procedure TD2XHandlerProcessor.BeginResults;
var
  lRslts: ID2XResultsHandler;
begin
  if fHandler.GetInterface(ID2XResultsHandler,lRslts) and fActive.Flag then
    lRslts.BeginResults;
end;

function TD2XHandlerProcessor.CheckAfterMethod(pMethod: string): Boolean;
var
  lChks: ID2XCheckHandler;
begin
  Result := not(fHandler.GetInterface(ID2XCheckHandler,lChks) and fActive.Flag) or lChks.CheckAfterMethod(pMethod);
end;

function TD2XHandlerProcessor.CheckBeforeMethod(pMethod: string): Boolean;
var
  lChks: ID2XCheckHandler;
begin
  Result := not(fHandler.GetInterface(ID2XCheckHandler,lChks) and fActive.Flag) or lChks.CheckBeforeMethod(pMethod);
end;

constructor TD2XHandlerProcessor.Create;
begin
  InvalidConstructor;
end;

constructor TD2XHandlerProcessor.CreateActive(pActive: ID2XFlag);
begin
  InvalidConstructor;
end;

constructor TD2XHandlerProcessor.CreateClass(pActive: ID2XFlag;
  pHandler: TD2XHandlerClass);
begin
  CreateHandler(pActive, pHandler.Create, True);
end;

constructor TD2XHandlerProcessor.CreateHandler(pActive: ID2XFlag; pHandler: TD2XHandler;
  pMine: Boolean);
var
  lLog: ID2XLogger;
begin
  inherited CreateActive(pActive);

  fMyHandler := pMine;
  fHandler := pHandler;
  if fHandler.GetInterface(ID2XLogger, lLog) then
    lLog.JoinLog(Self);
end;

destructor TD2XHandlerProcessor.Destroy;
begin
  if fMyHandler then
    FreeAndNil(fHandler);

  inherited;
end;

procedure TD2XHandlerProcessor.EndFile(pFile: string);
var
  lS: ID2XFile;
  lFls: ID2XFileHandler;
begin
  lS := nil;
  if fHandler.GetInterface(ID2XFileHandler,lFls) and fActive.Flag then
    if Assigned(fFileOutput) then
      try
        lFls.EndFile(pFile,
          function: TStreamWriter
          begin
            lS := fFileOutput;
            if Assigned(lS) then
              Result := lS.WriteTo
            else
              Result := nil;
          end);
      finally
        DisposeOf(lS);
      end
    else
      lFls.EndFile(pFile, nil);
end;

procedure TD2XHandlerProcessor.EndMethod(pMethod: string);
var
  lMthds: ID2XMethodHandler;
begin
  if fHandler.GetInterface(ID2XMethodHandler, lMthds) and fActive.Flag then
    lMthds.EndMethod(pMethod);
end;

procedure TD2XHandlerProcessor.EndProcessing;
var
  lS: ID2XFile;
  lPrcs: ID2XProcessingHandler;
begin
  lS := nil;
  if fHandler.GetInterface(ID2XProcessingHandler,lPrcs) and fActive.Flag then
    if Assigned(fProcessingOutput) then
      try
        lPrcs.EndProcessing(
          function: TStreamWriter
          begin
            lS := fProcessingOutput;
            if Assigned(lS) then
              Result := lS.WriteTo
            else
              Result := nil;
          end);
      finally
        DisposeOf(lS);
      end
    else
      lPrcs.EndProcessing(nil);
end;

procedure TD2XHandlerProcessor.EndResults(pFile: string);
var
  lS: ID2XFile;
  lRslts: ID2XResultsHandler;
begin
  lS := nil;
  if fHandler.GetInterface(ID2XResultsHandler,lRslts) and fActive.Flag then
    if Assigned(fResultsOutput) then
      try
        lRslts.EndResults(
          function: TStreamWriter
          begin
            lS := fResultsOutput(pFile);
            if Assigned(lS) then
              Result := lS.WriteTo
            else
              Result := nil;
          end);
      finally
        DisposeOf(lS);
      end
    else
      lRslts.EndResults(nil);
end;

function TD2XHandlerProcessor.HandlerIs(pHandler: TD2XHandlerClass): Boolean;
begin
  Result := fHandler is pHandler;
end;

procedure TD2XHandlerProcessor.LexerInclude(const pFile: string; pX, pY: Integer);
var
  lMsgs: ID2XMessagesHandler;
begin
  if fHandler.GetInterface(ID2XMessagesHandler, lMsgs) and fActive.Flag then
    lMsgs.LexerInclude(pFile, pX, pY);
end;

procedure TD2XHandlerProcessor.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
var
  lMsgs: ID2XMessagesHandler;
begin
  if fHandler.GetInterface(ID2XMessagesHandler, lMsgs) and fActive.Flag then
    lMsgs.ParserMessage(pTyp, pMsg, pX, pY);
end;

function TD2XHandlerProcessor.SetFileInput(pFilename: TD2XFileRef): TD2XHandlerProcessor;
begin
  fFileInput := pFilename;
  Result := Self;
end;

function TD2XHandlerProcessor.SetFileOutput(pFilename: TD2XFileRef): TD2XHandlerProcessor;
begin
  fFileOutput := pFilename;
  Result := Self;
end;

procedure TD2XHandlerProcessor.SetParser(pParser: TD2XDefinesParser);
var
  lPrsrs: ID2XParserHandler;
begin
  inherited;

  if fHandler.GetInterface(ID2XParserHandler, lPrsrs) then
    lPrsrs.InitParser(pParser);
end;

function TD2XHandlerProcessor.SetProcessingOutput(pFilename: TD2XFileRef)
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

end.
