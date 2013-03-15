unit D2X.Processor;

interface

uses
  CastaliaPasLexTypes,
  D2X.Flag,
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
    constructor CreateHandler(pActive: TD2XFlagRef; pHandler: TD2XLogger; pMine: Boolean);
    constructor CreateClass(pActive: TD2XFlagRef; pHandler: TD2XHandlerClass);
    destructor Destroy; override;

    function UseProxy: Boolean;

    //  ID2XParser interface
    procedure InitParser(pParser: TD2XDefinesParser);

    //  ID2XProcessing interface
    procedure EndProcessing;

    //  ID2XFiles interface
    procedure BeginFile(pFile: string);
    procedure EndFile(pFile: string);

    //  ID2XResults interface
    procedure BeginResults;
    procedure EndResults(pFile: string);

    //  ID2XTrees interface
    procedure AddAttr(pName: string; pValue: string);
    procedure AddText(pText: string);
    procedure RollBackTo(pElement: string);
    procedure TrimChildren(pElement: string);

    //  ID2XChecks interface
    function CheckBeforeMethod(pMethod: string): Boolean;
    function CheckAfterMethod(pMethod: string): Boolean;

    //  ID2XMethods interface
    procedure BeginMethod(pMethod: string);
    procedure EndMethod(pMethod: string);

    function SetProcessingOutput(pFilename: TD2XFileRef): TD2XProcessor;
    function SetResultsOutput(pFilename: TD2XNamedFileRef): TD2XProcessor;
    function SetFileInput(pFilename: TD2XFileRef): TD2XProcessor;
    function SetFileOutput(pFilename: TD2XFileRef): TD2XProcessor;

    function HandlerIs(pHandler: TD2XHandlerClass): Boolean;
  private
    fActive: TD2XFlagRef;
    fMyHandler: Boolean;
    fHandler: TD2XLogger;

    fProcessingOutput: TD2XFileRef;
    fResultsOutput: TD2XNamedFileRef;
    fFileInput: TD2XFileRef;
    fFileOutput: TD2XFileRef;

  end;

implementation

uses

  System.Classes,
  System.SysUtils;

{ TD2XProcessor  }

procedure TD2XProcessor.AddAttr(pName, pValue: string);
var
  lTrees: ID2XTrees;
begin
  if fHandler.GetInterface(ID2XTrees, lTrees) and fActive() then
    lTrees.AddAttr(pName, pValue);
end;

procedure TD2XProcessor.AddText(pText: string);
var
  lTrees: ID2XTrees;
begin
  if fHandler.GetInterface(ID2XTrees, lTrees) and fActive() then
    lTrees.AddText(pText);
end;

procedure TD2XProcessor.BeginFile(pFile: string);
var
  lS: ID2XIOFile;
  lFls: ID2XFiles;
begin
  lS := nil;
  if fHandler.GetInterface(ID2XFiles, lFls) and fActive() then
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
                Log('WARNING: %1 file "%2" not found', [(fHandler as ID2XHandler).Description,
                    lS.Description]);
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
  if fHandler.GetInterface(ID2XMethods, lMthds) and fActive() then
    lMthds.BeginMethod(pMethod);
end;

procedure TD2XProcessor.BeginResults;
var
  lRslts: ID2XResults;
begin
  if fHandler.GetInterface(ID2XResults, lRslts) and fActive() then
    lRslts.BeginResults;
end;

function TD2XProcessor.CheckAfterMethod(pMethod: string): Boolean;
var
  lChks: ID2XChecks;
begin
  Result := not(fHandler.GetInterface(ID2XChecks, lChks) and fActive()) or
    lChks.CheckAfterMethod(pMethod);
end;

function TD2XProcessor.CheckBeforeMethod(pMethod: string): Boolean;
var
  lChks: ID2XChecks;
begin
  Result := not(fHandler.GetInterface(ID2XChecks, lChks) and fActive()) or
    lChks.CheckBeforeMethod(pMethod);
end;

constructor TD2XProcessor.Create;
begin
  InvalidConstructor;
end;

constructor TD2XProcessor.CreateClass(pActive: TD2XFlagRef; pHandler: TD2XHandlerClass);
begin
  CreateHandler(pActive, pHandler.Create, True);
end;

constructor TD2XProcessor.CreateHandler(pActive: TD2XFlagRef; pHandler: TD2XLogger;
  pMine: Boolean);
begin
  Assert(Assigned(pActive), 'Active Flag must exist');

  inherited Create;

  fActive := pActive;
  fMyHandler := pMine;
  fHandler := pHandler;
  fHandler.JoinLog(Self);
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
  lS: ID2XIOFile;
  lFls: ID2XFiles;
begin
  lS := nil;
  if fHandler.GetInterface(ID2XFiles, lFls) and fActive() then
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
  if fHandler.GetInterface(ID2XMethods, lMthds) and fActive() then
    lMthds.EndMethod(pMethod);
end;

procedure TD2XProcessor.EndProcessing;
var
  lS: ID2XIOFile;
  lPrcs: ID2XProcessing;
begin
  lS := nil;
  if fHandler.GetInterface(ID2XProcessing, lPrcs) and fActive() then
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
  lS: ID2XIOFile;
  lRslts: ID2XResults;
begin
  lS := nil;
  if fHandler.GetInterface(ID2XResults, lRslts) and fActive() then
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

procedure TD2XProcessor.RollBackTo(pElement: string);
var
  lTrees: ID2XTrees;
begin
  if fHandler.GetInterface(ID2XTrees, lTrees) and fActive() then
    lTrees.RollBackTo(pElement);
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
    lPrsrs.InitParser(pParser, fActive);
end;

function TD2XProcessor.SetProcessingOutput(pFilename: TD2XFileRef): TD2XProcessor;
begin
  fProcessingOutput := pFilename;
  Result := Self;
end;

function TD2XProcessor.SetResultsOutput(pFilename: TD2XNamedFileRef): TD2XProcessor;
begin
  fResultsOutput := pFilename;
  Result := Self;
end;

procedure TD2XProcessor.TrimChildren(pElement: string);
var
  lTrees: ID2XTrees;
begin
  if fHandler.GetInterface(ID2XTrees, lTrees) and fActive() then
    lTrees.TrimChildren(pElement);
end;

function TD2XProcessor.UseProxy: Boolean;
var
  lProxy: ID2XFullProxy;
begin
  Result := fActive() and fHandler.GetInterface(ID2XFullProxy, lProxy);
end;

end.
