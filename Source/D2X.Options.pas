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

    function InputFileOrExtn(pFileOrExtn: string): string;
    function OutputFileOrExtn(pFileOrExtn: string): string;

    procedure RegisterParams(pParams: TD2XParams);

  private
    fUseOutput: TD2XFlaggedStringParam;
    fUseInput: TD2XFlaggedStringParam;
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

    function ForcePath(pFilename: string): String;
  end;

  TD2XOptions = class(TD2XLogger)
  private
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
    fLogErrors: TD2XBooleanParam;
    fLogNotSupported: TD2XBooleanParam;
    fFinalToken: TD2XBooleanParam;
    fRecurse: TD2XBooleanParam;
    fParseMode: TD2XSingleParam<TD2XParseMode>;
    fResultPer: TD2XSingleParam<TD2XResultPer>;
    fElapsedMode: TD2XSingleParam<TD2XElapsedMode>;
    fUseBase: TD2XFlaggedStringParam;

    function ConvertParsingMode(pStr: string; pDflt: TD2XParseMode;
      out pVal: TD2XParseMode): Boolean;

    procedure RemoveProxy;
    procedure SetProxy;
    function UseProxy: Boolean;

    function IsInternalMethod(pMethod: string): Boolean;

    procedure LogMessage(pType, pMsg: string); overload;
    procedure LogMessage(pType, pMsg: string; pX, pY: Integer); overload;

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

    function ProcessParamOption(pOpt: string): Boolean;
    function ProcessParamsFile(pFileOrExtn: string): Boolean;

    procedure InitProcessors;

    procedure InitParser;

    function TidyFilename(pFilename: string): string;
    procedure BeginResults(pNodename: string; pPer: TD2XResultPer);
    procedure EndResults(pFilename: string; pPer: TD2XResultPer);

    function ProcessStream(pStream: TStringStream): Boolean;
    function ProcessInput: Boolean;
    function ProcessFile(pFilename: string): Boolean;
    function ProcessDirectory(pDir, pWildCards: string): Boolean;
    function RecurseDirectory(pDir, pWildCards: string; pMainDir: Boolean): Boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure EndProcessing;

    function ProcessParam(pStr, pFrom: string; pIdx: Integer): Boolean;

  end;

function ConvertDir(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertExtn(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertFile(pStr, pDflt: string; out pVal: string): Boolean;

function MakeFileName(pStr, pDflt: string): string;

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

function TD2XOptions.IsInternalMethod(pMethod: string): Boolean;
begin
  Result := MatchText(pMethod, ['SynError', 'Run']);
end;

procedure TD2XOptions.LogMessage(pType, pMsg: string);
begin
  LogMessage(pType, pMsg, fParser.Lexer.PosXY.X, fParser.Lexer.PosXY.Y);
end;

procedure TD2XOptions.LogMessage(pType, pMsg: string; pX, pY: Integer);
var
  lErrFile: string;
  lExists: Boolean;
begin
  lErrFile := fFileOpts.OutputFileOrExtn('.err');
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
  case pTyp of
    meError:
      if fLogErrors.Value then
        LogMessage('Error', pMsg, pX, pY);
    meNotSupported:
      if fLogNotSupported.Value then
        LogMessage('Not Supported', pMsg, pX, pY);
  else
    LogMessage('????', pMsg, pX, pY);
  end;

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

  if fUseBase.FlagValue then
    lPath := fUseBase.Value + pDir
  else
    lPath := pDir;

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
  lFile := fFilename;
  if fUseBase.FlagValue then
    lFile := fUseBase.Value + lFile;
  if FileExists(lFile) then
  begin
    lSS := TStringStream.Create;
    try
      try
        lSS.LoadFromFile(lFile);
      except
        on E: Exception do
        begin
          LogMessage('EXCEPTION', '(' + E.ClassName + ')' + E.Message);
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
function TD2XOptions.ProcessParam(pStr, pFrom: string; pIdx: Integer): Boolean;
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
            Result := ProcessParamOption(Copy(pStr, 2));
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

    BeginResults('D2X_Param', rpParam);
    if pStr = '-' then
      Result := ProcessInput
    else
    begin
      Result := ProcessFile(pStr);
      if not Result then
      begin
        lPath := ExtractFilePath(pStr);
        lFile := ExtractFileName(pStr);
        BeginResults('D2X_Dir', rpDir);
        Result := ProcessDirectory(lPath, lFile);
        EndResults(ExcludeTrailingPathDelimiter(lPath), rpDir);
        if fRecurse.Value then
          Result := RecurseDirectory(lPath, lFile, True) or Result;
      end;
    end;
    EndResults(pFrom + '-' + IntToStr(pIdx), rpParam);
  except
    on E: Exception do
      Log('EXCEPTION (%s) processing "%s" : %s', [E.ClassName, pStr, E.Message]);
  end;
end;

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
begin
  if Assigned(fParser) then
  begin
    RemoveProxy;
    case fParseMode.Value of
      pmUses:
        begin
          if not(fParser is TD2XUsesParser) then
            FreeAndNil(fParser);
        end
    else
      begin
        if not(fParser is TD2XFullParser) then
          FreeAndNil(fParser);
      end
    end;
  end;

  if not Assigned(fParser) then
  begin
    case fParseMode.Value of
      pmUses:
        fParser := TD2XUsesParser.Create
    else
      fParser := TD2XFullParser.Create;
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
  lHProc: TD2XHandlerProcessor;
  lLogProcessor: TD2XLogProcessor;

  lParserDefinesHandler: TD2XParserDefinesHandler;

  lSkipMethods: TD2XFlaggedStringParam;
  lCountChildren: TD2XFlaggedStringParam;
  lDefinesUsed: TD2XFlaggedStringParam;
  lWriteXml : TD2XFlaggedStringParam;
  lWriteDefines : TD2XFlaggedStringParam;
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

  lWriteXml := TD2XFlaggedStringParam.CreateFlagStr('X', 'Generate XML', '<dir>',
    'Generate XML files into current or given <dir>', 'Xml\', True, ConvertDir, nil, nil);
  lWriteDefines := TD2XFlaggedStringParam.CreateFlagStr('W', 'Write Defines', '<dir>',
    'Generate Final Defines files into current or given <dir>', 'Defines\', False, ConvertDir,
    nil, nil);

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
    begin
      Result := True;
      if pStr > '' then
      begin
        lFile := fFileOpts.OutputFileOrExtn(MakeFileName(pStr, '.prm'));
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

  fVerbose := TD2XBooleanParam.CreateBool('V', 'Verbose', 'Log all Parser methods called');
  fParams.Add(fVerbose);

  lLogProcessor := TD2XLogProcessor.Create(fVerbose);
  lLogProcessor.JoinLog(Self);
  fProcs.Add(lLogProcessor);

  fLogErrors := TD2XBooleanParam.CreateBool('L', 'Log Errors', 'Log Error messages', True);
  fParams.Add(fLogErrors);

  fLogNotSupported := TD2XBooleanParam.CreateBool('N', 'Log Not Supp',
    'Log Not Supported messages');
  fParams.Add(fLogNotSupported);

  fFinalToken := TD2XBooleanParam.CreateBool('F', 'Final Token', 'Record Final Token', True);
  fParams.Add(fFinalToken);

  fRecurse := TD2XBooleanParam.CreateBool('R', 'Recurse', 'Recurse into subdirectories');
  fParams.Add(fRecurse);

  lSkipMethods := TD2XFlaggedStringParam.CreateFlagStr('S', 'Skipped Methods', '<f/e>',
    'Load Skipped Methods from <f/e>', '.skip', True, ConvertFile, nil, nil);

  lHProc := TD2XHandlerProcessor.CreateHandler(lSkipMethods, TD2XSkipHandler.Create);
  lHProc.SetFileInput(
    function: string
    begin
      Result := fFileOpts.InputFileOrExtn(lSkipMethods.Value);
    end);
  lHProc.SetProcessingOutput(
    function: string
    begin
      Result := fFileOpts.OutputFileOrExtn(lSkipMethods.Value + '.log');
    end);
  fProcs.Add(lHProc);

  lCountChildren := TD2XFlaggedStringParam.CreateFlagStr('C', 'Count Children', '<f/e>',
    'Report Min/Max Children into <f/e>', '.cnt', True, ConvertExtn, nil, nil);
  lHProc := TD2XHandlerProcessor.CreateHandler(lCountChildren, TD2XCountHandler.Create);
  lHProc.SetFileInput(
    function: string
    begin
      Result := '';
    end);
  lHProc.SetFileOutput(
    function: string
    begin
      Result := '';
    end);
  lHProc.SetProcessingOutput(
    function: string
    begin
      Result := fFileOpts.OutputFileOrExtn(lCountChildren.Value);
    end);
  fProcs.Add(lHProc);

  fXmlHandler := TD2XXmlHandler.Create;
  fXmlHandler.Init(
      function: Boolean
    begin
      Result := fFinalToken.Value;
    end,
    function: string
    begin
      Result := TD2X.ToLabel(fParseMode.Value);
    end);
  lHProc := TD2XHandlerProcessor.CreateHandler(lWriteXml, fXmlHandler);
  lHProc.SetResultsOutput(
    function(pFilename: string): string
    begin
      Result := fFileOpts.ForcePath(lWriteXml.Value + pFilename + '.xml');
    end);
  fProcs.Add(lHProc);

  lHProc := TD2XHandlerProcessor.CreateHandler(lWriteDefines, TD2XWriteDefinesHandler.Create);
  lHProc.SetResultsOutput(
    function(pFilename: string): string
    begin
      Result := fFileOpts.ForcePath(lWriteDefines.Value + pFilename + '.def');
    end);
  fProcs.Add(lHProc);

  lDefinesUsed := TD2XFlaggedStringParam.CreateFlagStr('U', 'Defines Used', '<f/e>',
    'Report Defines Used into <f/e>', '.used', True, ConvertExtn, nil, nil);
  fDefinesUsedHandler := TD2XDefinesUsedHandler.Create;
  lHProc := TD2XHandlerProcessor.CreateHandler(lDefinesUsed, fDefinesUsedHandler);
  lHProc.SetProcessingOutput(
    function: string
    begin
      Result := fFileOpts.ForcePath(fFileOpts.OutputFileOrExtn(lDefinesUsed.Value));
    end);
  fProcs.Add(lHProc);

  fFileOpts.RegisterParams(fParams);

  fParseMode := TD2XSingleParam<TD2XParseMode>.CreateParam('M', 'Parse mode', '<mode>',
    'Set Parsing mode (F[ull], U[ses])', pmFull, ConvertParsingMode,
    TD2X.ToLabel<TD2XParseMode>, nil);
  fParams.Add(fParseMode);

  fResultPer := TD2XSingleParam<TD2XResultPer>.CreateParam('P', 'Results per', '<per>',
    'Set Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un])', rpFile,
    ConvertResultPer, TD2X.ToLabel<TD2XResultPer>, nil);
  fParams.Add(fResultPer);

  fElapsedMode := TD2XSingleParam<TD2XElapsedMode>.CreateParam('E', 'Show elapsed', '<mode>',
    'Set Elapsed time display to be (N[one], Q[uiet], T[otal], P[rocessing])', emQuiet,
    ConvertElapsedMode, TD2X.ToLabel<TD2XElapsedMode>, nil);
  fParams.Add(fElapsedMode);

  fUseBase := TD2XFlaggedStringParam.CreateFlagStr('B', 'Base dir', '<dir>',
    'Use <dir> as a base for all file lookups', '', False, ConvertDir, nil, nil);
  fParams.Add(fUseBase);

  fParams.Add(lWriteXml);
  fParams.Add(lWriteDefines);
  fParams.Add(lDefinesUsed);
  fParams.Add(lCountChildren);
  fParams.Add(lSkipMethods);

  lParserDefinesHandler := TD2XParserDefinesHandler.Create;
  lParserDefinesHandler.SetDefinesFileName(fFileOpts.InputFileOrExtn);

  fParams.Add(TD2XResettableParam.CreateReset('D', 'Defines', '[+-!:]<def>',
    'Add(+), Remove(-), Clear(!) or Load(:) Defines', lParserDefinesHandler.ParseDefines,
    lParserDefinesHandler.ClearDefines, lParserDefinesHandler.ClearDefines));
end;

function TD2XOptions.ProcessParamsFile(pFileOrExtn: string): Boolean;
var
  lSL: TStringList;
  i: Integer;
  lFile: string;
begin
  Result := True;
  lSL := TStringList.Create;
  lFile := fFileOpts.InputFileOrExtn(pFileOrExtn);
  try
    lSL.LoadFromFile(lFile);
    for i := 0 to lSL.Count - 1 do
      Result := ProcessParam(lSL[i], lFile, i + 1) and Result;
  finally
    FreeAndNil(lSL);
  end;
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
      lP.BeginFile;

    try
      fParser.ProcessString(fFilename, lFile);
      Result := True;
    except
      on E: Exception do
      begin
        LogMessage('EXCEPTION', '(' + E.ClassName + ')' + E.Message);

        fXmlHandler.RollbackTo('D2X_File');
      end;
    end;

    for lP in fProcs do
      lP.EndFile;

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

  if fUseBase.FlagValue then
    lPath := fUseBase.Value + pDir
  else
    lPath := pDir;

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

function TD2XOptions.TidyFilename(pFilename: string): string;
begin
  Result := ReplaceStr(ReplaceStr(ReplaceStr(pFilename, '*', ''), '.', ''), '?', '');
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

  fUseInput := TD2XFlaggedStringParam.CreateFlagStr('I', 'Input dir', '<dir>',
    'Use <dir> as a base for all file input', 'Config\', True, ConvertDir, nil, nil);
  fUseOutput := TD2XFlaggedStringParam.CreateFlagStr('O', 'Output dir', '<dir>',
    'Use <dir> as a base for all file output', 'Log\', True, ConvertDir, nil, nil);
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

function TD2XFileOptions.ForcePath(pFilename: string): String;
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

function TD2XFileOptions.InputFileOrExtn(pFileOrExtn: string): string;
  function GlobalFileOrExtn(pFileOrExtn: string): string;
  begin
    if StartsText('.', pFileOrExtn) then
      Result := ChangeFileExt(fGlobalName.Value, pFileOrExtn)
    else
      Result := pFileOrExtn;
  end;

begin
  if IParamFlag(fUseInput).Flag then
    Result := fUseInput.Value + GlobalFileOrExtn(pFileOrExtn)
  else
    Result := GlobalFileOrExtn(pFileOrExtn);
end;

function TD2XFileOptions.OutputFileOrExtn(pFileOrExtn: string): string;
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
  if IParamFlag(fUseOutput).Flag then
    Result := ForcePath(fUseOutput.Value + GlobalFileOrExtn(pFileOrExtn))
  else
    Result := ForcePath(GlobalFileOrExtn(pFileOrExtn));
end;

procedure TD2XFileOptions.RegisterParams(pParams: TD2XParams);
begin
  pParams.Add(fTimestampFiles);
  pParams.Add(fGlobalName);
  pParams.Add(fUseInput);
  pParams.Add(fUseOutput);
end;

procedure TD2XFileOptions.SetGlobalName(const Value: string);
begin
  fGlobalName.Value := Value;
end;

end.
