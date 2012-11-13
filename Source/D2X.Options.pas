unit D2X.Options;

interface

uses
  CastaliaPasLexTypes,
  D2X,
  D2X.Handlers,
  D2X.Param,
  D2X.Parser,
  D2X.Processor,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.Rtti,
  System.StrUtils,
  System.SysUtils;

type
  ED2XOptionsException = class(Exception);

  TD2XFileOptions = class
  public
    constructor Create(pGlobalValidator: TD2XSingleParam<string>.TspValidator);

    function ConfigFileOrExtn(pFileOrExtn: string): string;
    function LogFileOrExtn(pFileOrExtn: string): string;
    function BaseFileOrDir(pFileOrDir: string): string;

    procedure RegisterParams(pParams: TD2XParams);

  private
    fLogBase: TD2XFlaggedStringParam;
    fConfigBase: TD2XFlaggedStringParam;
    fInputBase: TD2XFlaggedStringParam;
    fGlobalName: TD2XStringParam;
    fTimestampFiles: TD2XBooleanParam;

    fOutputTimestamp: string;

    function GetGlobalName: string;
    function GetTimestampFiles: Boolean;
    procedure SetGlobalName(const Value: string);

  public
    property OutputTimestamp: string read fOutputTimestamp;
    property GlobalName: string read GetGlobalName write SetGlobalName;
    property TimestampFiles: Boolean read GetTimestampFiles;

    function ForcePath(pFilename: string): string;
  end;

  TD2XOptions = class(TD2XLogger)
  strict private
    fParams: TD2XParams;
    fProcs: TObjectList<TD2XProcessor>;

    fProgramDir: string;

    fDuration: TStopwatch;

    fParser: TD2XDefinesParser;
    fVMI: TVirtualMethodInterceptor;

    fFilename: string;

    fXmlHandler: TD2XXmlHandler;
    fDefinesUsedHandler: TD2XDefinesUsedHandler;

    fFileOpts: TD2XFileOptions;

    fVerbose: TD2XBooleanParam;
    fRecurse: TD2XBooleanParam;
    fParseMode: TD2XSingleParam<TD2XParseMode>;
    fResultPer: TD2XSingleParam<TD2XResultPer>;
    fElapsedMode: TD2XSingleParam<TD2XElapsedMode>;

    function ConvertParsingMode(pStr: string; pDflt: TD2XParseMode;
      out pVal: TD2XParseMode): Boolean;

    procedure RemoveProxy;
    procedure SetProxy;
    function UseProxy: Boolean;

    function IsInternalMethod(pMethod: string): Boolean;

    procedure LogParser(pType, pMsg: string);
    procedure LogMessage(pType, pMsg: string; pX, pY: Integer);

    procedure ParserMessage(pSender: TObject; const pTyp: TMessageEventType;
      const pMsg: string; pX, pY: Integer);

    procedure LexerOnInclude(pLex: TD2XLexer);

    // procedure LexerOnDefine(pLex: TD2XLexer);
    // procedure LexerOnUnDef(pLex: TD2XLexer);

    procedure LexerOnIfDef(pLex: TD2XLexer);
    procedure LexerOnIfNDef(pLex: TD2XLexer);
    procedure LexerOnIfOpt(pLex: TD2XLexer);
    procedure LexerOnIf(pLex: TD2XLexer);
    procedure LexerOnElseIf(pLex: TD2XLexer);
    // procedure LexerOnElse(pLex: TD2XLexer);
    // procedure LexerOnEndIf(pLex: TD2XLexer);
    // procedure LexerOnIfEnd(pLex: TD2XLexer);

    procedure InitProcessors;

    procedure InitParser;

    function ProcessStream(pStream: TStringStream): Boolean;

    function GetRecurse: Boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessParamOption(pOpt: string): Boolean;
    function ConfigFileOrExtn(pFileOrExtn: string): string;

    procedure BeginResults(pNodename: string; pPer: TD2XResultPer);
    procedure EndResults(pFilename: string; pPer: TD2XResultPer);

    function ProcessInput: Boolean;
    function ProcessFile(pFilename: string): Boolean;
    function ProcessDirectory(pDir, pWildCards: string): Boolean;
    function RecurseDirectory(pDir, pWildCards: string; pMainDir: Boolean): Boolean;

    procedure EndProcessing;

    property Recurse: Boolean read GetRecurse;
  end;

  TD2XRunOptions = class(TObject, ID2XLogger)
  private
    fOpts: TD2XOptions;

    function ProcessParamsFile(pFileOrExtn: string): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure EndProcessing;

    function ProcessParam(pStr, pFrom: string; pIdx: Integer): Boolean;

    property L: TD2XOptions read fOpts implements ID2XLogger;
  end;

function ConvertDir(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertExtn(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertFile(pStr, pDflt: string; out pVal: string): Boolean;

function MakeFileName(pStr, pDflt: string): string;
function TidyFilename(pFilename: string): string;

implementation

uses
  D2X.Processors,
  System.IOUtils,
  System.TypInfo,
  Winapi.Windows;

type
  TD2XSetterFunc = reference to function(pVal: string): Boolean;

function ConvertDir(pStr, pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  if pStr > '' then
    pVal := IncludeTrailingPathDelimiter(pStr)
  else
    pVal := '';
end;

function ConvertExtn(pStr, pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  if pStr = '' then
    pVal := pDflt
  else
    if ExtractFileExt(pStr) > '' then
      pVal := pStr
    else
      pVal := '.' + pStr;
end;

function MakeFileName(pStr, pDflt: string): string;
begin
  if pStr = '' then
    Result := pDflt
  else
    if ExtractFileExt(pStr) > '' then
      Result := pStr
    else
      Result := pStr + pDflt;
end;

function TidyFilename(pFilename: string): string;
begin
  Result := ReplaceStr(ReplaceStr(ReplaceStr(pFilename, '*', ''), '.', ''), '?', '');
end;

function ConvertFile(pStr, pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  pVal := MakeFileName(pStr, pDflt);
end;

function ConvertResultPer(pStr: string; pDflt: TD2XResultPer;
  out pVal: TD2XResultPer): Boolean;
begin
  Result := pStr > '';
  if Result then
    case pStr[1] of
      'R', 'r':
        pVal := rpRun;
      'P', 'p':
        pVal := rpParam;
      'W', 'w':
        pVal := rpWildcard;
      'S', 's':
        pVal := rpSubDir;
      'D', 'd':
        pVal := rpDir;
      '!':
        pVal := pDflt;
    else
      pVal := rpFile;
    end;
end;

function ConvertElapsedMode(pStr: string; pDflt: TD2XElapsedMode;
  out pVal: TD2XElapsedMode): Boolean;
begin
  Result := pStr > '';
  if Result then
    case pStr[1] of
      'N', 'n':
        pVal := emNone;
      'Q', 'q':
        pVal := emQuiet;
      'T', 't':
        pVal := emTotal;
      'P', 'p':
        pVal := emProcessing;
      '!':
        pVal := pDflt;
    else
      pVal := emQuiet;
    end;
end;

{ TD2XOptions }

procedure TD2XOptions.BeginResults(pNodename: string; pPer: TD2XResultPer);
var
  lP: TD2XProcessor;
begin
  if fResultPer.Value = pPer then
    for lP in fProcs do
      lP.BeginResults;

  if fResultPer.Value >= pPer then
    fXmlHandler.BeginMethod(pNodename);
end;

function TD2XOptions.ConvertParsingMode(pStr: string;
  pDflt: TD2XParseMode; out pVal: TD2XParseMode): Boolean;
begin
  Result := pStr > '';
  if Result then
    case pStr[1] of
      'U', 'u':
        begin
          pVal := pmUses;
          fFileOpts.GlobalName := 'Uses';
        end;
      '!', 'D', 'd':
        pVal := pDflt;
    else
      pVal := pmFull;
    end;
end;

constructor TD2XOptions.Create;
begin
  inherited Create;

  fProgramDir := ExtractFilePath(ParamStr(0));
  fDuration := TStopwatch.StartNew;

  fParams := TD2XParams.Create;
  fParams.L.JoinLog(Self);

  fProcs := TObjectList<TD2XProcessor>.Create;
  InitProcessors;

  InitParser;
end;

destructor TD2XOptions.Destroy;
begin
  fDuration.Stop;
  case fElapsedMode.Value of
    emNone:
      ;
    emQuiet:
      Log('Processing finished!', []);
  else
    Log('Total processing time %0.3f', [fDuration.Elapsed.TotalSeconds]);
  end;

  RemoveProxy;

  FreeAndNil(fParser);

  FreeAndNil(fProcs);

  inherited;
end;

procedure TD2XOptions.EndProcessing;
var
  lP: TD2XProcessor;
begin
  EndResults('', rpRun);

  for lP in fProcs do
    lP.EndProcessing;
end;

procedure TD2XOptions.EndResults(pFilename: string; pPer: TD2XResultPer);
var
  lP: TD2XProcessor;
begin
  if fResultPer.Value >= pPer then
  begin
    fXmlHandler.AddAttr('fileName', pFilename);
    fXmlHandler.EndMethod('');
  end;

  if fResultPer.Value = pPer then
  begin
    if pFilename = '' then
      pFilename := '(' + TD2X.ToLabel(pPer) + ')';
    for lP in fProcs do
      lP.EndResults(pFilename);
  end;
end;

function TD2XOptions.GetRecurse: Boolean;
begin
  Result := fRecurse.Value;
end;

function TD2XOptions.IsInternalMethod(pMethod: string): Boolean;
begin
  Result := MatchText(pMethod, ['SynError', 'Run']);
end;

procedure TD2XOptions.LogParser(pType, pMsg: string);
begin
  LogMessage(pType, pMsg, fParser.Lexer.PosXY.X, fParser.Lexer.PosXY.Y);
end;

procedure TD2XOptions.LogMessage(pType, pMsg: string; pX, pY: Integer);
var
  lErrFile: string;
  lExists: Boolean;
begin
  lErrFile := fFileOpts.LogFileOrExtn('.err');
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

procedure TD2XOptions.ParserMessage(pSender: TObject; const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
var
  lP: TD2XProcessor;
begin
  for lP in fProcs do
    lP.ParserMessage(pTyp, pMsg, pX, pY);
end;

function TD2XOptions.ProcessDirectory(pDir, pWildCards: string): Boolean;
var
  lFF: TSearchRec;
  lPath: string;
  lFile: string;
begin
  Result := False;

  lPath := fFileOpts.BaseFileOrDir(pDir);

  for lFile in SplitString(pWildCards, ',') do
    if System.SysUtils.FindFirst(lPath + lFile, faAnyFile - faDirectory, lFF) = 0 then
      try
        BeginResults('D2X_Pattern', rpWildcard);
        repeat
          Result := ProcessFile(pDir + lFF.Name) or Result;
        until System.SysUtils.FindNext(lFF) <> 0;
        EndResults(pDir + 'Pattern-' + TidyFilename(lFile), rpWildcard);
      finally
        System.SysUtils.FindClose(lFF);
      end;
end;

function TD2XOptions.ProcessFile(pFilename: string): Boolean;
var
  lSS: TStringStream;
  lFile: string;
begin
  Result := False;
  fFilename := pFilename;
  lFile := fFileOpts.BaseFileOrDir(fFilename);
  if FileExists(lFile) then
  begin
    lSS := TStringStream.Create;
    try
      try
        lSS.LoadFromFile(lFile);
      except
        on E: Exception do
        begin
          LogParser('EXCEPTION', '(' + E.ClassName + ')' + E.Message);
          Result := False;
        end;
      end;

      Result := ProcessStream(lSS);
    finally
      FreeAndNil(lSS);
    end;
  end
  else
    { TODO -oStruan -cTodo : Is thi sthe correct flag to check? }
    if fVerbose.Value then
      Log('Cannot find "%s"', [lFile]);
end;

function TD2XOptions.ProcessInput: Boolean;
var
  lSS: TStringStream;
  lIS: THandleStream;
begin
  lSS := TStringStream.Create;
  try
    lIS := THandleStream.Create(GetStdHandle(STD_INPUT_HANDLE));
    try
      lSS.CopyFrom(lIS, 0);
    finally
      FreeAndNil(lIS);
    end;

    fFilename := '(Input)';
    Result := ProcessStream(lSS);
  finally
    FreeAndNil(lSS);
  end;
end;

(*
 procedure TD2XOptions.LexerOnDefine(pLex: TD2XLexer);
 begin
 pLex.Next;
 end;

 procedure TD2XOptions.LexerOnElse(pLex: TD2XLexer);
 begin
 pLex.Next;
 end;
*)
procedure TD2XOptions.LexerOnElseIf(pLex: TD2XLexer);
begin
  pLex.Next;
end;

(*
 procedure TD2XOptions.LexerOnEndIf(pLex: TD2XLexer);
 begin
 pLex.Next;
 end;
*)
procedure TD2XOptions.LexerOnIf(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XOptions.LexerOnIfDef(pLex: TD2XLexer);
begin
  fDefinesUsedHandler.DefineUsed(pLex.DirectiveParam);
  pLex.Next;
end;

(*
 procedure TD2XOptions.LexerOnIfEnd(pLex: TD2XLexer);
 begin
 pLex.Next;
 end;
*)
procedure TD2XOptions.LexerOnIfNDef(pLex: TD2XLexer);
begin
  fDefinesUsedHandler.DefineUsed(pLex.DirectiveParam);
  pLex.Next;
end;

procedure TD2XOptions.LexerOnIfOpt(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XOptions.LexerOnInclude(pLex: TD2XLexer);
var
  lFile: string;
  lX, lY: Integer;
  lP: TD2XProcessor;
begin
  lFile := pLex.DirectiveParam;
  lX := pLex.PosXY.X;
  lY := pLex.PosXY.Y;

  for lP in fProcs do
    lP.LexerInclude(lFile, lX, lY);

  pLex.Next;
end;

(*
 procedure TD2XOptions.LexerOnUnDef(pLex: TD2XLexer);
 begin
 pLex.Next;
 end;
*)

function TD2XOptions.ProcessParamOption(pOpt: string): Boolean;
var
  lPrevPer: TD2XResultPer;
  lPrm: TD2XParam;

begin
  lPrevPer := fResultPer.Value;
  Result := False;
  if Length(pOpt) < 1 then
    raise ED2XOptionsException.Create('Invalid option: ' + pOpt)
  else
  begin
    lPrm := fParams.ForCode(Copy(pOpt, 1, 1));
    if Assigned(lPrm) then
      if lPrm.Parse(pOpt) then
        Result := True
      else
        Log('%s option: %s', ['Invalid ' + lPrm.ParamLabel, pOpt])
    else
      Log('%s option: %s', ['Unknown', pOpt]);
  end;
  if lPrevPer <> fResultPer.Value then
  begin
    if lPrevPer = rpRun then
      EndResults('', fResultPer.Value);
    BeginResults('D2X_Run', rpRun);
  end;

end;

procedure TD2XOptions.InitParser;
var
  lP: TD2XProcessor;
  lC: TD2XDefinesParserClass;
begin
  case fParseMode.Value of
    pmUses:
      lC := TD2XUsesParser
  else
    lC := TD2XFullParser;
  end;

  if Assigned(fParser) then
  begin
    RemoveProxy;
    if not(fParser is lC) then
      FreeAndNil(fParser);
  end;

  if not Assigned(fParser) then
  begin
    case fParseMode.Value of
      pmUses:
        fParser := lC.Create
    else
      fParser := lC.Create;
    end;

    fParser.OnMessage := ParserMessage;
    fParser.AddAttribute := fXmlHandler.AddAttr;
    fParser.AddText := fXmlHandler.AddText;

    fParser.Lexer.OnIncludeDirect := LexerOnInclude;
    // fParser.Lexer.OnDefineDirect := LexerOnDefine;
    // fParser.Lexer.OnUnDefDirect := LexerOnUnDef;
    fParser.Lexer.OnIfDirect := LexerOnIf;
    fParser.Lexer.OnIfDefDirect := LexerOnIfDef;
    fParser.Lexer.OnIfNDefDirect := LexerOnIfNDef;
    fParser.Lexer.OnIfOptDirect := LexerOnIfOpt;
    // fParser.Lexer.OnElseDirect := LexerOnElse;
    fParser.Lexer.OnElseIfDirect := LexerOnElseIf;
    // fParser.Lexer.OnEndIfDirect := LexerOnEndIf;
    // fParser.Lexer.OnIfEndDirect := LexerOnIfEnd;

    for lP in fProcs do
      lP.SetParser(fParser);
  end;
end;

procedure TD2XOptions.InitProcessors;
var
  lLogProcessor: TD2XLogProcessor;

  lParserDefinesHandler: TD2XParserDefinesHandler;

  lLogErrors: TD2XBooleanParam;
  lLogNotSupported: TD2XBooleanParam;
  lFinalToken: TD2XBooleanParam;

  lSkipMethods: TD2XFlaggedStringParam;
  lCountChildren: TD2XFlaggedStringParam;
  lDefinesUsed: TD2XFlaggedStringParam;
  lWriteXml: TD2XFlaggedStringParam;
  lWriteDefines: TD2XFlaggedStringParam;
begin
  fFileOpts := TD2XFileOptions.Create(
      function(pVal: string): Boolean
    begin
      if Assigned(lWriteXml) then
        lWriteXml.Value := IncludeTrailingPathDelimiter(pVal);
      if Assigned(lWriteDefines) then
        lWriteDefines.Value := IncludeTrailingPathDelimiter(pVal);
      Result := True;
    end);

  fVerbose := TD2XBooleanParam.CreateBool('V', 'Verbose', 'Log all Parser methods called');
  fRecurse := TD2XBooleanParam.CreateBool('R', 'Recurse', 'Recurse into subdirectories');
  fParseMode := TD2XSingleParam<TD2XParseMode>.CreateParam('M', 'Parse mode', '<mode>',
    'Set Parsing mode (F[ull], U[ses])', pmFull, ConvertParsingMode,
    TD2X.ToLabel<TD2XParseMode>, nil);
  fResultPer := TD2XSingleParam<TD2XResultPer>.CreateParam('P', 'Results per', '<per>',
    'Set Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un])', rpFile,
    ConvertResultPer, TD2X.ToLabel<TD2XResultPer>, nil);
  fElapsedMode := TD2XSingleParam<TD2XElapsedMode>.CreateParam('E', 'Show elapsed', '<mode>',
    'Set Elapsed time display to be (N[one], Q[uiet], T[otal], P[rocessing])', emQuiet,
    ConvertElapsedMode, TD2X.ToLabel<TD2XElapsedMode>, nil);

  fXmlHandler := TD2XXmlHandler.CreateXml(
    function: Boolean
    begin
      Result := lFinalToken.Value;
    end,
    function: string
    begin
      Result := TD2X.ToLabel(fParseMode.Value);
    end);
  fDefinesUsedHandler := TD2XDefinesUsedHandler.Create;

  fParams.Add(TD2XParam.Create('?', 'Options', '', 'Show valid options',
      function(pStr: string): Boolean
    begin
      Result := True;
      fParams.DescribeAll;
    end));
  fParams.Add(TD2XParam.Create('!', 'Reset', '', 'Reset all options to defaults',
    function(pStr: string): Boolean
    begin
      Result := True;
      if StartsText('!', pStr) then
        fParams.ZeroAll
      else
        fParams.ResetAll;
    end));
  fParams.Add(TD2XParam.Create('@', 'Report', '<file>', 'Report/Output Current options',
    function(pStr: string): Boolean
    var
      lSL: TStringList;
      lFile: string;
      lPrm: TD2XParam;
    begin
      Result := True;
      if pStr > '' then
        if StartsText('-', pStr) and (Length(pStr) > 1) then
        begin
          lPrm := fParams.ForCode(Copy(pStr, 2, 1));
          if Assigned(lPrm) then
            lPrm.Report(Self)
          else
            Log('%s option: %s', ['Unknown', pStr]);
        end
        else
        begin
          lFile := fFileOpts.LogFileOrExtn(MakeFileName(pStr, '.prm'));
          lSL := TStringList.Create;
          try
            fParams.OutputAll(lSL);
            lParserDefinesHandler.OutputDefines(lSL);
            if lSL.Count > 0 then
              lSL.SaveToFile(lFile);
          finally
            FreeAndNil(lSL);
          end;
        end
      else
      begin
        fParams.ReportAll;
        lParserDefinesHandler.ReportDefines(fParams);
      end;
    end));

  lLogErrors := TD2XBooleanParam.CreateBool('L', 'Log Errors', 'Log Error messages', True);
  lLogNotSupported := TD2XBooleanParam.CreateBool('N', 'Log Not Supp',
    'Log Not Supported messages');
  lFinalToken := TD2XBooleanParam.CreateBool('F', 'Final Token', 'Record Final Token', True);
  lSkipMethods := TD2XFlaggedStringParam.CreateFlagStr('S', 'Skipped Methods', '<f/e>',
    'Load Skipped Methods from <f/e>', '.skip', True, ConvertFile, nil, nil);
  lCountChildren := TD2XFlaggedStringParam.CreateFlagStr('C', 'Count Children', '<f/e>',
    'Report Min/Max Children into <f/e>', '.cnt', True, ConvertExtn, nil, nil);
  lWriteXml := TD2XFlaggedStringParam.CreateFlagStr('X', 'Generate XML', '<dir>',
    'Generate XML files into current or given <dir>', 'Xml\', True, ConvertDir, nil, nil);
  lWriteDefines := TD2XFlaggedStringParam.CreateFlagStr('W', 'Write Defines', '<dir>',
    'Generate Final Defines files into current or given <dir>', 'Defines\', False, ConvertDir,
    nil, nil);
  lDefinesUsed := TD2XFlaggedStringParam.CreateFlagStr('U', 'Defines Used', '<f/e>',
    'Report Defines Used into <f/e>', '.used', True, ConvertExtn, nil, nil);

  lParserDefinesHandler := TD2XParserDefinesHandler.Create;
  lParserDefinesHandler.SetDefinesFileName(fFileOpts.ConfigFileOrExtn);

  lLogProcessor := TD2XLogProcessor.Create(fVerbose);
  lLogProcessor.JoinLog(Self);

  fProcs.Add(lLogProcessor);
  fProcs.Add(TD2XHandlerProcessor.CreateHandler(lLogErrors,
    TD2XErrorHandler.CreateError(meError, LogMessage)));
  fProcs.Add(TD2XHandlerProcessor.CreateHandler(lLogNotSupported,
    TD2XErrorHandler.CreateError(meNotSupported, LogMessage)));
  fProcs.Add(TD2XHandlerProcessor.CreateClass(lSkipMethods, TD2XSkipHandler).SetFileInput(
    function(pFile: string): string
    begin
      Result := fFileOpts.ConfigFileOrExtn(lSkipMethods.Value);
    end).SetProcessingOutput(
    function: string
    begin
      Result := fFileOpts.LogFileOrExtn(lSkipMethods.Value + '.log');
    end));
  fProcs.Add(TD2XHandlerProcessor.CreateClass(lCountChildren, TD2XCountHandler).SetFileInput(
    function(pFile: string): string
    begin
      Result := '';
    end).SetFileOutput(
    function(pFile: string): string
    begin
      Result := '';
    end).SetProcessingOutput(
    function: string
    begin
      Result := fFileOpts.LogFileOrExtn(lCountChildren.Value);
    end));
  fProcs.Add(TD2XHandlerProcessor.CreateHandler(lWriteXml, fXmlHandler).SetResultsOutput(
    function(pFilename: string): string
    begin
      Result := fFileOpts.ForcePath(lWriteXml.Value + pFilename + '.xml');
    end));
  fProcs.Add(TD2XHandlerProcessor.CreateClass(lWriteDefines, TD2XWriteDefinesHandler)
    .SetResultsOutput(
    function(pFilename: string): string
    begin
      Result := fFileOpts.ForcePath(lWriteDefines.Value + pFilename + '.def');
    end));
  fProcs.Add(TD2XHandlerProcessor.CreateHandler(lDefinesUsed, fDefinesUsedHandler)
    .SetProcessingOutput(
    function: string
    begin
      Result := fFileOpts.ForcePath(fFileOpts.LogFileOrExtn(lDefinesUsed.Value));
    end));
  fProcs.Add(TD2XHandlerProcessor.CreateHandler(lParserDefinesHandler, lParserDefinesHandler));

  fParams.AddRange([fVerbose, lLogErrors, lLogNotSupported, lFinalToken, fRecurse]);
  fFileOpts.RegisterParams(fParams);
  fParams.AddRange([fParseMode, fResultPer, fElapsedMode, lWriteXml]);
  fParams.AddRange([lWriteDefines, lDefinesUsed, lCountChildren, lSkipMethods]);

  fParams.Add(TD2XResettableParam.CreateReset('D', 'Defines', '[+-!:]<def>',
    'Add(+), Remove(-), Clear(!) or Load(:) Defines', lParserDefinesHandler.ParseDefines,
    lParserDefinesHandler.ClearDefines, lParserDefinesHandler.ClearDefines));
end;

function TD2XOptions.ConfigFileOrExtn(pFileOrExtn: string): string;
begin
  Result := fFileOpts.ConfigFileOrExtn(pFileOrExtn);
end;

function TD2XOptions.ProcessStream(pStream: TStringStream): Boolean;
var
  lTimer: TStopwatch;
  lFile: string;
  lP: TD2XProcessor;
begin
  if fElapsedMode.Value <> emNone then
    Log('Processing %s ... ', [fFilename], False);
  lTimer := TStopwatch.StartNew;
  try
    InitParser;
    if UseProxy then
      SetProxy;

    Result := False;
    lFile := pStream.DataString;

    if ContainsText(LeftStr(lFile, 16), '<') then
      Exit;

    BeginResults('D2X_File', rpFile);
    fXmlHandler.HasFiles := True;

    for lP in fProcs do
      lP.BeginFile(fFilename);

    try
      fParser.ProcessString(fFilename, lFile);
      Result := True;
    except
      on E: Exception do
      begin
        LogParser('EXCEPTION', '(' + E.ClassName + ')' + E.Message);

        fXmlHandler.RollbackTo('D2X_File');
      end;
    end;

    for lP in fProcs do
      lP.EndFile(fFilename);

    EndResults(fFilename, rpFile);
  finally
    lTimer.Stop;
    case fElapsedMode.Value of
      emNone:
        ;
      emQuiet:
        Log('done', []);
    else
      Log('%0.3f', [lTimer.Elapsed.TotalSeconds]);
    end;

  end;
end;

{$WARN SYMBOL_PLATFORM OFF}

function TD2XOptions.RecurseDirectory(pDir, pWildCards: string;
pMainDir: Boolean): Boolean;
var
  lFF: TSearchRec;
  lPath: string;
  lFile: string;
begin
  Result := False;

  lPath := fFileOpts.BaseFileOrDir(pDir);

  if System.SysUtils.FindFirst(lPath + '*', faAnyFile - faNormal - faTemporary, lFF) = 0 then
    try
      repeat
        if (lFF.Name <> '.') and (lFF.Name <> '..') and ((lFF.Attr and faDirectory) <> 0) then
        begin
          lFile := IncludeTrailingPathDelimiter(pDir + lFF.Name);
          if pMainDir then
            BeginResults('D2X_Dir', rpDir)
          else
            BeginResults('D2X_SubDir', rpSubDir);
          Result := ProcessDirectory(lFile, pWildCards) or Result;
          if not pMainDir then
            EndResults(ExcludeTrailingPathDelimiter(lFile), rpSubDir);
          Result := RecurseDirectory(lFile, pWildCards, False) or Result;
          if pMainDir then
            EndResults(ExcludeTrailingPathDelimiter(lFile), rpDir);
        end;
      until System.SysUtils.FindNext(lFF) <> 0;
    finally
      System.SysUtils.FindClose(lFF);
    end;
end;
{$WARN SYMBOL_PLATFORM ON}

procedure TD2XOptions.RemoveProxy;
begin
  if Assigned(fVMI) then
  begin
    fVMI.Unproxify(fParser);
    FreeAndNil(fVMI);
  end;
end;

procedure TD2XOptions.SetProxy;
begin
  fVMI := TVirtualMethodInterceptor.Create(TObject(fParser).ClassType);
  fVMI.Proxify(fParser);
  fVMI.OnBefore :=
      procedure(pInst: TObject; pMethod: TRttiMethod; const pArgs: TArray<TValue>;
    out pDoInvoke: Boolean; out pResult: TValue)
    var
      lP: TD2XProcessor;
    begin
      pDoInvoke := True;
      if IsInternalMethod(pMethod.Name) then
        Exit;
      for lP in fProcs do
        if not lP.CheckBeforeMethod(pMethod.Name) then
          Exit;
      for lP in fProcs do
        lP.BeginMethod(pMethod.Name);
    end;
  fVMI.OnAfter :=
      procedure(pInst: TObject; pMethod: TRttiMethod; const pArgs: TArray<TValue>;
    var pResult: TValue)
    var
      lP: TD2XProcessor;
    begin
      if IsInternalMethod(pMethod.Name) then
        Exit;
      for lP in fProcs do
        if not lP.CheckAfterMethod(pMethod.Name) then
          Exit;
      for lP in fProcs do
        lP.EndMethod(pMethod.Name);
    end;
end;

function TD2XOptions.UseProxy: Boolean;
var
  lP: TD2XProcessor;
begin
  Result := False;
  for lP in fProcs do
    Result := Result or lP.UseProxy;
end;

{ TD2XFileOptions }

constructor TD2XFileOptions.Create(pGlobalValidator: TD2XSingleParam<string>.TspValidator);
begin
  inherited Create;

  fConfigBase := TD2XFlaggedStringParam.CreateFlagStr('I', 'Config dir', '<dir>',
    'Use <dir> as a base for all Config files', 'Config\', True, ConvertDir, nil, nil);
  fLogBase := TD2XFlaggedStringParam.CreateFlagStr('O', 'Log dir', '<dir>',
    'Use <dir> as a base for all Log files', 'Log\', True, ConvertDir, nil, nil);
  fInputBase := TD2XFlaggedStringParam.CreateFlagStr('B', 'Base dir', '<dir>',
    'Use <dir> as a base for all Input files', '', False, ConvertDir, nil, nil);
  fGlobalName := TD2XStringParam.CreateStr('G', 'Global name', '<str>', 'Sets global name',
    ChangeFileExt(ExtractFileName(ParamStr(0)), ''),
    function(pStr: string; pDflt: string; out pVal: string): Boolean
    begin
      Result := True;
      if pStr = '' then
        pVal := ChangeFileExt(ExtractFileName(ParamStr(0)), '')
      else
        pVal := pStr;
    end, pGlobalValidator);
  fTimestampFiles := TD2XBooleanParam.CreateBool('T', 'Timestamp',
    'Timestamp global output files');

  fOutputTimestamp := FormatDateTime('-HH-mm', Now);
end;

function TD2XFileOptions.ForcePath(pFilename: string): string;
begin
  Result := pFilename;
  ForceDirectories(ExtractFilePath(ParamStr(0)) + ExtractFilePath(pFilename));
end;

function TD2XFileOptions.GetGlobalName: string;
begin
  Result := fGlobalName.Value;
end;

function TD2XFileOptions.GetTimestampFiles: Boolean;
begin
  Result := fTimestampFiles.Value;
end;

function TD2XFileOptions.BaseFileOrDir(pFileOrDir: string): string;
begin
  if fInputBase.FlagValue then
    Result := fInputBase.Value + pFileOrDir
  else
    Result := pFileOrDir;
end;

function TD2XFileOptions.ConfigFileOrExtn(pFileOrExtn: string): string;
  function GlobalFileOrExtn(pFileOrExtn: string): string;
  begin
    if StartsText('.', pFileOrExtn) then
      Result := ChangeFileExt(fGlobalName.Value, pFileOrExtn)
    else
      Result := pFileOrExtn;
  end;

begin
  if IParamFlag(fConfigBase).Flag then
    Result := fConfigBase.Value + GlobalFileOrExtn(pFileOrExtn)
  else
    Result := GlobalFileOrExtn(pFileOrExtn);
end;

function TD2XFileOptions.LogFileOrExtn(pFileOrExtn: string): string;
  function GlobalFileOrExtn(pFileOrExtn: string): string;
  var
    lExtn: string;
  begin
    if StartsText('.', pFileOrExtn) then
      if fTimestampFiles.Value then
        Result := ChangeFileExt(fGlobalName.Value, fOutputTimestamp + pFileOrExtn)
      else
        Result := ChangeFileExt(fGlobalName.Value, pFileOrExtn)
    else
      if fTimestampFiles.Value then
      begin
        lExtn := ExtractFileExt(pFileOrExtn);
        Result := ChangeFileExt(pFileOrExtn, fOutputTimestamp + lExtn);
      end
      else
        Result := pFileOrExtn;
  end;

begin
  if IParamFlag(fLogBase).Flag then
    Result := ForcePath(fLogBase.Value + GlobalFileOrExtn(pFileOrExtn))
  else
    Result := ForcePath(GlobalFileOrExtn(pFileOrExtn));
end;

procedure TD2XFileOptions.RegisterParams(pParams: TD2XParams);
begin
  pParams.Add(fTimestampFiles);
  pParams.Add(fGlobalName);
  pParams.Add(fConfigBase);
  pParams.Add(fLogBase);
  pParams.Add(fInputBase);
end;

procedure TD2XFileOptions.SetGlobalName(const Value: string);
begin
  fGlobalName.Value := Value;
end;

{ TD2XRunOptions }

constructor TD2XRunOptions.Create;
begin
  inherited;

  fOpts := TD2XOptions.Create;
end;

destructor TD2XRunOptions.Destroy;
begin
  fOpts := nil;

  inherited;
end;

procedure TD2XRunOptions.EndProcessing;
begin
  fOpts.EndProcessing;
end;

function TD2XRunOptions.ProcessParam(pStr, pFrom: string;
  pIdx: Integer): Boolean;
var
  lPath, lFile: string;
begin
  Result := False;
  try
    if (Length(pStr) > 1) then
    begin
      case pStr[1] of
        '-', '/':
          begin
            Result := fOpts.ProcessParamOption(Copy(pStr, 2));
            Exit;
          end;
        '@':
          begin
            Result := ProcessParamsFile(Copy(pStr, 2));
            Exit;
          end;
        '#':
          begin
            Result := True;
            Exit;
          end;
      end;
    end;

    fOpts.BeginResults('D2X_Param', rpParam);
    if pStr = '-' then
      Result := fOpts.ProcessInput
    else
    begin
      Result := fOpts.ProcessFile(pStr);
      if not Result then
      begin
        lPath := ExtractFilePath(pStr);
        lFile := ExtractFileName(pStr);
        fOpts.BeginResults('D2X_Dir', rpDir);
        Result := fOpts.ProcessDirectory(lPath, lFile);
        fOpts.EndResults(ExcludeTrailingPathDelimiter(lPath), rpDir);
        if fOpts.Recurse then
          Result := fOpts.RecurseDirectory(lPath, lFile, True) or Result;
      end;
    end;
    fOpts.EndResults(pFrom + '-' + IntToStr(pIdx), rpParam);
  except
    on E: Exception do
      L.Log('EXCEPTION (%s) processing "%s" : %s', [E.ClassName, pStr, E.Message]);
  end;
end;

function TD2XRunOptions.ProcessParamsFile(pFileOrExtn: string): Boolean;
var
  lSL: TStringList;
  i: Integer;
  lFile: string;
begin
  Result := True;
  lSL := TStringList.Create;
  lFile := fOpts.ConfigFileOrExtn(pFileOrExtn);
  try
    lSL.LoadFromFile(lFile);
    for i := 0 to lSL.Count - 1 do
      Result := ProcessParam(lSL[i], lFile, i + 1) and Result;
  finally
    FreeAndNil(lSL);
  end;
end;

end.
