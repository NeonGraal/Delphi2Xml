unit D2XProcessors;

interface

uses
  CastaliaPasLexTypes,
  D2X,
  D2XParam,
  D2XHandler,
  D2XProcessor;

type
  TD2XHandlerProcessor = class(TD2XProcessor)
  public
    constructor CreateHandler(pActive: IParamFlag; pHandler: TD2XHandler);

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
  System.Classes,
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

end.
