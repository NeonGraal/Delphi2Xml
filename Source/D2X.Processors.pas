unit D2X.Processors;

interface

uses
  CastaliaPasLexTypes,
  D2X,
  D2X.Param,
  D2X.Parser,
  D2X.Handler,
  D2X.Processor;

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

  TD2XErrorProcessor = class(TD2XLexerProcessor)
  public
    procedure SetFilename(pLexer: TD2XLexer; pFilename: string);
    procedure SetErrorFile(pFilename: TD2XStringRef);

    procedure LogMessage(pType, pMsg: string); overload;
    procedure LogMessage(pType, pMsg: string; pX, pY: Integer); overload;

  private
    fFilename: string;
    fErrorFile: TD2XStringRef;
  end;

  TD2XHandlerProcessor = class(TD2XProcessor)
  public
    constructor CreateHandler(pActive: IParamFlag; pHandler: TD2XHandler);

    function UseProxy: Boolean; override;

    procedure SetParser(pParser: TD2XDefinesParser); override;

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

    procedure SetProcessingInput(pFilename: TD2XStringRef);
    procedure SetProcessingOutput(pFilename: TD2XStringRef);
    procedure SetResultsOutput(pFilename: TD2XNamedStringRef);
    procedure SetFileInput(pFilename: TD2XStringRef);
    procedure SetFileOutput(pFilename: TD2XStringRef);

  private
    fHandler: TD2XHandler;

    fProcessingInput: TD2XStringRef;
    fProcessingOutput: TD2XStringRef;
    fResultsOutput: TD2XNamedStringRef;
    fFileInput: TD2XStringRef;
    fFileOutput: TD2XStringRef;

  end;

implementation

uses
  D2X.Handlers,
  System.Classes,
  System.IOUtils,
  System.SysUtils;

{ TD2XHandlerProcessor  }

procedure TD2XHandlerProcessor.BeginFile;
var
  lFS: TFileStream;
begin
  lFS := nil;
  if fActive.Flag then
    if Assigned(fFileInput) then
      try
        fHandler.BeginFile(
            function: TStream
          var
            lFile: string;
          begin
            lFile := fFileInput;
            if FileExists(lFile) then
              lFS := TFileStream.Create(lFile, fmOpenRead)
            else
              Log('WARNING: %1 file "%2" not found', [fHandler.Description, lFile]);
            Result := lFS;
          end);
      finally
        FreeAndNil(lFS);
      end
    else
      fHandler.BeginFile(nil);
end;

procedure TD2XHandlerProcessor.BeginMethod(pMethod: string);
begin
  if fActive.Flag then
    fHandler.BeginMethod(pMethod);
end;

procedure TD2XHandlerProcessor.BeginProcessing;
var
  lFS: TFileStream;
begin
  lFS := nil;
  if fActive.Flag then
    if Assigned(fProcessingInput) then
      try
        fHandler.BeginProcessing(
          function: TStream
          var
            lFile: string;
          begin
            lFile := fProcessingInput;
            if FileExists(lFile) then
              lFS := TFileStream.Create(lFile, fmOpenRead);
            Result := lFS;
          end);
      finally
        FreeAndNil(lFS);
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

constructor TD2XHandlerProcessor.CreateHandler(pActive: IParamFlag; pHandler: TD2XHandler);
begin
  inherited Create(pActive);

  fHandler := pHandler;
end;

procedure TD2XHandlerProcessor.EndFile;
var
  lFS: TFileStream;
begin
  lFS := nil;
  if fActive.Flag then
    if Assigned(fFileOutput) then
      try
        fHandler.EndFile(
          function: TStream
          var
            lFile: string;
          begin
            lFile := fFileOutput;
            if lFile > '' then
              lFS := TFileStream.Create(lFile, fmOpenWrite);
            Result := lFS;
          end);
      finally
        FreeAndNil(lFS);
      end
    else
      fHandler.EndFile(nil);
end;

procedure TD2XHandlerProcessor.EndMethod(pMethod: string);
begin
  if fActive.Flag then
    fHandler.EndMethod(pMethod);
end;

procedure TD2XHandlerProcessor.EndProcessing;
var
  lFS: TFileStream;
begin
  lFS := nil;
  if fActive.Flag then
    if Assigned(fProcessingOutput) then
      try
        fHandler.EndProcessing(
          function: TStream
          var
            lFile: string;
          begin
            lFile := fProcessingOutput;
            if lFile > '' then
              lFS := TFileStream.Create(lFile, fmOpenWrite);
            Result := lFS;
          end);
      finally
        FreeAndNil(lFS);
      end
    else
      fHandler.EndProcessing(nil);
end;

procedure TD2XHandlerProcessor.EndResults(pFile: string);
var
  lFS: TFileStream;
begin
  lFS := nil;
  if fActive.Flag then
    if Assigned(fResultsOutput) then
      try
        fHandler.EndResults(
          function: TStream
          var
            lFile: string;
          begin
            lFile := fResultsOutput(pFile);
            if lFile > '' then
              lFS := TFileStream.Create(lFile, fmOpenWrite);
            Result := lFS;
          end);
      finally
        FreeAndNil(lFS);
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

procedure TD2XHandlerProcessor.SetFileInput(pFilename: TD2XStringRef);
begin
  fFileInput := pFilename;
end;

procedure TD2XHandlerProcessor.SetFileOutput(pFilename: TD2XStringRef);
begin
  fFileOutput := pFilename;
end;

procedure TD2XHandlerProcessor.SetParser(pParser: TD2XDefinesParser);
begin
  inherited;

  if fHandler is TD2XParserHandler then
    TD2XParserHandler(fHandler).InitParser(pParser);
end;

procedure TD2XHandlerProcessor.SetProcessingInput(pFilename: TD2XStringRef);
begin
  fProcessingInput := pFilename;
end;

procedure TD2XHandlerProcessor.SetProcessingOutput(pFilename: TD2XStringRef);
begin
  fProcessingOutput := pFilename;
end;

procedure TD2XHandlerProcessor.SetResultsOutput(pFilename: TD2XNamedStringRef);
begin
  fResultsOutput := pFilename;
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

{ TD2XErrorProcessor }

procedure TD2XErrorProcessor.LogMessage(pType, pMsg: string; pX, pY: Integer);
var
  lErrFile: string;
  lExists: Boolean;
begin
  if Assigned(fErrorFile) then
  begin
    lErrFile := fErrorFile;
    if lErrFile > '' then
    begin
      lExists := TFile.Exists(lErrFile);
      with TFile.AppendText(lErrFile) do
        try
          if not lExists then
            WriteLine('Filename,Timestamp,Line,Char,Method,Type,Message');
          write(fFilename);
          write(',');
          write(FormatDateTime('yyyy-mmm-dd HH:nn:ss.zzz', Now));
          write(',');
          write(pY);
          write(',');
          write(pX);
          write(',');
          write(''{fCurrent.Method});
          write(',');
          write(pType);
          write(',');
          WriteLine(pMsg);
        finally
          Free;
        end;
    end;
  end;
end;

procedure TD2XErrorProcessor.LogMessage(pType, pMsg: string);
begin
  if Assigned(fLexer) then
    LogMessage(pType, pMsg, fLexer.PosXY.X, fLexer.PosXY.y)
  else
    LogMessage(pType, pMsg, -1, -1);
end;

procedure TD2XErrorProcessor.SetErrorFile(pFilename: TD2XStringRef);
begin

end;

procedure TD2XErrorProcessor.SetFilename(pLexer: TD2XLexer; pFilename: string);
begin
  fLexer := pLexer;
  fFilename := pFilename;
end;

{ TD2XLexerProcessor }

procedure TD2XLexerProcessor.SetParser(pParser: TD2XDefinesParser);
begin
  inherited;

  fLexer := pParser.Lexer;
end;

end.
