unit D2X.Processor;

interface

uses
  CastaliaPasLexTypes,
  D2X.Global,
  D2X.Param,
  D2X.Parser,
  D2X.Handler,
  D2X.IO,
  System.Generics.Collections;

type
  TD2XProcessor = class(TD2XLogger)
  public
    constructor Create; override;
    constructor CreateHandler(pActive: ID2XFlag; pHandler: TD2XHandler; pMine: Boolean);
    constructor CreateClass(pActive: ID2XFlag; pHandler: TD2XHandlerClass);
    destructor Destroy; override;

    function UseProxy: Boolean; virtual;

    procedure InitParser(pParser: TD2XDefinesParser);

    procedure EndProcessing; virtual;

    procedure BeginFile(pFile: string); virtual;
    procedure EndFile(pFile: string); virtual;

    procedure BeginResults; virtual;
    procedure EndResults(pFile: string); virtual;

    function CheckBeforeMethod(pMethod: string): Boolean;
    function CheckAfterMethod(pMethod: string): Boolean;

    procedure BeginMethod(pMethod: string);
    procedure EndMethod(pMethod: string);

    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer);
    procedure LexerInclude(const pFile: string; pX, pY: Integer);

    function SetProcessingOutput(pFilename: TD2XFileRef): TD2XProcessor;
    function SetResultsOutput(pFilename: TD2XNamedStreamRef): TD2XProcessor;
    function SetFileInput(pFilename: TD2XFileRef): TD2XProcessor;
    function SetFileOutput(pFilename: TD2XFileRef): TD2XProcessor;

    function HandlerIs(pHandler: TD2XHandlerClass): Boolean;
  private
    fActive: ID2XFlag;
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

{ TD2XProcessor  }

procedure TD2XProcessor.BeginFile(pFile: string);
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

procedure TD2XProcessor.BeginMethod(pMethod: string);
var
  lMthds: ID2XMethods;
begin
  if fHandler.GetInterface(ID2XMethods, lMthds) and fActive.Flag then
    lMthds.BeginMethod(pMethod);
end;

procedure TD2XProcessor.BeginResults;
var
  lRslts: ID2XResultsHandler;
begin
  if fHandler.GetInterface(ID2XResultsHandler,lRslts) and fActive.Flag then
    lRslts.BeginResults;
end;

function TD2XProcessor.CheckAfterMethod(pMethod: string): Boolean;
var
  lChks: ID2XChecks;
begin
  Result := not(fHandler.GetInterface(ID2XChecks,lChks) and fActive.Flag) or lChks.CheckAfterMethod(pMethod);
end;

function TD2XProcessor.CheckBeforeMethod(pMethod: string): Boolean;
var
  lChks: ID2XChecks;
begin
  Result := not(fHandler.GetInterface(ID2XChecks,lChks) and fActive.Flag) or lChks.CheckBeforeMethod(pMethod);
end;

constructor TD2XProcessor.Create;
begin
  InvalidConstructor;
end;

constructor TD2XProcessor.CreateClass(pActive: ID2XFlag;
  pHandler: TD2XHandlerClass);
begin
  CreateHandler(pActive, pHandler.Create, True);
end;

constructor TD2XProcessor.CreateHandler(pActive: ID2XFlag; pHandler: TD2XHandler;
  pMine: Boolean);
var
  lLog: ID2XLogger;
begin
  Assert(Assigned(pActive), 'Active Flag must exist');

  inherited Create;

  fActive := pActive;
  fMyHandler := pMine;
  fHandler := pHandler;
  if fHandler.GetInterface(ID2XLogger, lLog) then
    lLog.JoinLog(Self);
end;

destructor TD2XProcessor.Destroy;
begin
  if fMyHandler then
    FreeAndNil(fHandler);
  fActive := nil;

  inherited;
end;

procedure TD2XProcessor.EndFile(pFile: string);
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

procedure TD2XProcessor.EndMethod(pMethod: string);
var
  lMthds: ID2XMethods;
begin
  if fHandler.GetInterface(ID2XMethods, lMthds) and fActive.Flag then
    lMthds.EndMethod(pMethod);
end;

procedure TD2XProcessor.EndProcessing;
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

procedure TD2XProcessor.EndResults(pFile: string);
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

function TD2XProcessor.HandlerIs(pHandler: TD2XHandlerClass): Boolean;
begin
  Result := fHandler is pHandler;
end;

procedure TD2XProcessor.LexerInclude(const pFile: string; pX, pY: Integer);
var
  lMsgs: ID2XMessages;
begin
  if fHandler.GetInterface(ID2XMessages, lMsgs) and fActive.Flag then
    lMsgs.LexerInclude(pFile, pX, pY);
end;

procedure TD2XProcessor.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
var
  lMsgs: ID2XMessages;
begin
  if fHandler.GetInterface(ID2XMessages, lMsgs) and fActive.Flag then
    lMsgs.ParserMessage(pTyp, pMsg, pX, pY);
end;

function TD2XProcessor.SetFileInput(pFilename: TD2XFileRef): TD2XProcessor;
begin
  fFileInput := pFilename;
  Result := Self;
end;

function TD2XProcessor.SetFileOutput(pFilename: TD2XFileRef): TD2XProcessor;
begin
  fFileOutput := pFilename;
  Result := Self;
end;

procedure TD2XProcessor.InitParser(pParser: TD2XDefinesParser);
var
  lPrsrs: ID2XParser;
begin
  inherited;

  if fHandler.GetInterface(ID2XParser, lPrsrs) then
    lPrsrs.InitParser(pParser);
end;

function TD2XProcessor.SetProcessingOutput(pFilename: TD2XFileRef)
  : TD2XProcessor;
begin
  fProcessingOutput := pFilename;
  Result := Self;
end;

function TD2XProcessor.SetResultsOutput(pFilename: TD2XNamedStreamRef)
  : TD2XProcessor;
begin
  fResultsOutput := pFilename;
  Result := Self;
end;

function TD2XProcessor.UseProxy: Boolean;
var
  lProxy: ID2XFullProxy;
begin
  Result := fActive.Flag and fHandler.GetInterface(ID2XFullProxy, lProxy);
end;

end.
