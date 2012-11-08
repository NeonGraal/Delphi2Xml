unit D2X.ParamProcessor;

interface

uses
  CastaliaPasLexTypes,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.Rtti,
  D2X.Param,
  D2X.Options,
  D2X.Handlers,
  D2X.Parser,
  D2X.Processor,
  D2X.Processors,
  D2X;

type
  TMethodCount = record
    Method: string;
    Children: Integer;
  end;

  TD2XParamProcessor = class(TD2XLogger)
  private
    fOpts: TD2XOptions;

    fParams: TD2XParams;
    fProcs: TObjectList<TD2XProcessor>;

    fProgramDir: string;

    fDuration: TStopwatch;

    fParser: TD2XDefinesParser;
    fVMI: TVirtualMethodInterceptor;

    fDefinesDict: TStrIntDict;

    fFilename: string;

    fLogProcessor: TD2XLogProcessor;
    fXmlHandler: TD2XXmlHandler;
    fWriteDefinesHandler : TD2XWriteDefinesHandler;

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

    procedure DefineUsed(pDef: string);

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

    function SimplePairLog(pPair: TStrIntPair): string;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure EndProcessing;

    function ProcessParam(pStr, pFrom: string; pIdx: Integer): Boolean;

    property Options: TD2XOptions read fOpts;

  end;

implementation

uses
  System.IOUtils,
  System.StrUtils,
  System.SysUtils,
  Xml.XMLIntf,
  Winapi.Windows;

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

{ TD2XParamProcessor }

procedure TD2XParamProcessor.BeginResults(pNodename: string; pPer: TD2XResultPer);
var
  lP: TD2XProcessor;
begin
  if fResultPer.Value = pPer then
    for lP in fProcs do
      lP.BeginResults;

  if fResultPer.Value >= pPer then
    fXmlHandler.BeginMethod(pNodename);
end;

function TD2XParamProcessor.ConvertParsingMode(pStr: string;
  pDflt: TD2XParseMode; out pVal: TD2XParseMode): Boolean;
begin
  Result := pStr > '';
  if Result then
    case pStr[1] of
      'U', 'u':
        begin
          pVal := pmUses;
          fOpts.FileOpts.GlobalName := 'Uses';
        end;
      '!', 'D', 'd':
        pVal := pDflt;
    else
      pVal := pmFull;
    end;
end;

constructor TD2XParamProcessor.Create;
begin
  inherited Create;

  fProgramDir := ExtractFilePath(ParamStr(0));
  fDuration := TStopwatch.StartNew;

  fOpts := TD2XOptions.Create(Self);

  fParams := TD2XParams.Create;
  fParams.L.JoinLog(Self);

  fProcs := TObjectList<TD2XProcessor>.Create;
  InitProcessors;

  fDefinesDict := TStrIntDict.Create;

  InitParser;

  fParser.Lexer.InitDefines;
  fParser.Lexer.GetDefines(fOpts.Defines);
end;

procedure TD2XParamProcessor.DefineUsed(pDef: string);
var
  lVal: Integer;
begin
  if fDefinesDict.TryGetValue(pDef, lVal) then
    fDefinesDict[pDef] := lVal + 1
  else
    fDefinesDict.Add(pDef, 1)
end;

destructor TD2XParamProcessor.Destroy;
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

  FreeAndNil(fDefinesDict);

  FreeAndNil(fProcs);
  FreeAndNil(fOpts);

  inherited;
end;

procedure TD2XParamProcessor.EndProcessing;
  procedure OutputStrIntDict(pDict: TStrIntDict; pExtn: string; pFunc: TPairLogMethod);
  var
    lP: TStrIntPair;
  begin
    with TStringList.Create do
      try
        for lP in pDict do
          if lP.Value > 0 then
            Values[lP.Key] := pFunc(lP);
        Sort;
        SaveToFile(fOpts.FileOpts.OutputFileOrExtn(pExtn));
      finally
        Free;
      end;
  end;

var
  lP: TD2XProcessor;
begin
  EndResults('', rpRun);

  if fOpts.DefinesUsed then
    OutputStrIntDict(fDefinesDict, fOpts.DefinesUsedFoE, SimplePairLog);

  for lP in fProcs do
    lP.EndProcessing;
end;

procedure TD2XParamProcessor.EndResults(pFilename: string; pPer: TD2XResultPer);
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

function TD2XParamProcessor.IsInternalMethod(pMethod: string): Boolean;
begin
  Result := MatchText(pMethod, ['SynError', 'Run']);
end;

procedure TD2XParamProcessor.LogMessage(pType, pMsg: string);
begin
  LogMessage(pType, pMsg, fParser.Lexer.PosXY.X, fParser.Lexer.PosXY.Y);
end;

procedure TD2XParamProcessor.LogMessage(pType, pMsg: string; pX, pY: Integer);
var
  lErrFile: string;
  lExists: Boolean;
begin
  lErrFile := fOpts.FileOpts.OutputFileOrExtn('.err');
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

procedure TD2XParamProcessor.ParserMessage(pSender: TObject; const pTyp: TMessageEventType;
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

function TD2XParamProcessor.ProcessDirectory(pDir, pWildCards: string): Boolean;
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

function TD2XParamProcessor.ProcessFile(pFilename: string): Boolean;
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

function TD2XParamProcessor.ProcessInput: Boolean;
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
 procedure TD2XParamProcessor.LexerOnDefine(pLex: TD2XLexer);
 begin
 pLex.Next;
 end;

 procedure TD2XParamProcessor.LexerOnElse(pLex: TD2XLexer);
 begin
 pLex.Next;
 end;
*)
procedure TD2XParamProcessor.LexerOnElseIf(pLex: TD2XLexer);
begin
  pLex.Next;
end;

(*
 procedure TD2XParamProcessor.LexerOnEndIf(pLex: TD2XLexer);
 begin
 pLex.Next;
 end;
*)
procedure TD2XParamProcessor.LexerOnIf(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XParamProcessor.LexerOnIfDef(pLex: TD2XLexer);
begin
  DefineUsed(pLex.DirectiveParam);
  pLex.Next;
end;

(*
 procedure TD2XParamProcessor.LexerOnIfEnd(pLex: TD2XLexer);
 begin
 pLex.Next;
 end;
*)
procedure TD2XParamProcessor.LexerOnIfNDef(pLex: TD2XLexer);
begin
  DefineUsed(pLex.DirectiveParam);
  pLex.Next;
end;

procedure TD2XParamProcessor.LexerOnIfOpt(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XParamProcessor.LexerOnInclude(pLex: TD2XLexer);
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
 procedure TD2XParamProcessor.LexerOnUnDef(pLex: TD2XLexer);
 begin
 pLex.Next;
 end;
*)
function TD2XParamProcessor.ProcessParam(pStr, pFrom: string; pIdx: Integer): Boolean;
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

function TD2XParamProcessor.ProcessParamOption(pOpt: string): Boolean;
var
  lPrevPer: TD2XResultPer;
  lPrm: TD2XParam;

begin
  lPrevPer := fResultPer.Value ;
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
  if lPrevPer <> fResultPer.Value  then
  begin
    if lPrevPer = rpRun then
      EndResults('', fResultPer.Value );
    BeginResults('D2X_Run', rpRun);
  end;

end;

procedure TD2XParamProcessor.InitParser;
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

    fLogProcessor.SetLexer(fParser.Lexer);
    fXmlHandler.Init(fParser,
        function: Boolean
      begin
        Result := fFinalToken.Value;
      end,
      function: string
      begin
        Result := TD2X.ToLabel(fParseMode.Value);
      end);
    fWriteDefinesHandler.Init(fParser);
  end;
end;

procedure TD2XParamProcessor.InitProcessors;
var
  lHProc: TD2XHandlerProcessor;
  lSkipMethods: TD2XFlaggedStringParam;
  lCountChildren: TD2XFlaggedStringParam;
begin
  fOpts.RegisterOptionParams(fParams, fOpts.FileOpts);

  fVerbose := TD2XBooleanParam.CreateBool('V', 'Verbose', 'Log all Parser methods called');
  fParams.Add(fVerbose);

  fLogProcessor := TD2XLogProcessor.Create(fVerbose);
  fLogProcessor.JoinLog(Self);
  fProcs.Add(fLogProcessor);

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
      Result := fOpts.FileOpts.InputFileOrExtn(lSkipMethods.Value);
    end);
  lHProc.SetProcessingOutput(
    function: string
    begin
      Result := fOpts.FileOpts.OutputFileOrExtn(lSkipMethods.Value + '.log');
    end);
  fProcs.Add(lHProc);

  lCountChildren := TD2XFlaggedStringParam.CreateFlagStr('C', 'Count Children', '<f/e>',
    'Report Min/Max Children into <f/e>', '.cnt', True, ConvertExtn, nil, nil);
  lHProc := TD2XHandlerProcessor.CreateHandler(lCountChildren,
    TD2XCountHandler.Create);
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
      Result := fOpts.FileOpts.OutputFileOrExtn(lCountChildren.Value);
    end);
  fProcs.Add(lHProc);

  fXmlHandler := TD2XXmlHandler.Create;
  lHProc := TD2XHandlerProcessor.CreateHandler(fOpts.WriteXmlFlag, fXmlHandler);
  lHProc.SetResultsOutput(
    function(pFilename: String): string
    var
      lFile: String;
    begin
      lFile := fProgramDir + fOpts.XmlDirectory + ExtractFilePath(pFilename);
      ForceDirectories(lFile);
      Result := fOpts.XmlDirectory + pFilename + '.xml';
    end);
  fProcs.Add(lHProc);

  fWriteDefinesHandler := TD2XWriteDefinesHandler.Create;
  lHProc := TD2XHandlerProcessor.CreateHandler(fOpts.WriteDefinesFlag, fWriteDefinesHandler);
  lHProc.SetResultsOutput(
    function(pFilename: String): string
    var
      lFile: String;
    begin
      lFile := fProgramDir + fOpts.DefinesDirectory + ExtractFilePath(pFilename);
      ForceDirectories(lFile);
      Result := fOpts.DefinesDirectory + pFilename + '.def';
    end);
  fProcs.Add(lHProc);

  fOpts.FileOpts.RegisterParams(fParams);

  fParseMode := TD2XSingleParam<TD2XParseMode>.CreateParam('M', 'Parse mode', '<mode>',
    'Set Parsing mode (F[ull], U[ses])', pmFull, ConvertParsingMode,
    TD2X.ToLabel<TD2XParseMode>, nil);
  fParams.Add(fParseMode);

  fResultPer := TD2XSingleParam<TD2XResultPer>.CreateParam('P', 'Results per', '<per>',
    'Set Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un])', rpFile,
    ConvertResultPer, TD2X.ToLabel<TD2XResultPer>, nil);
  fParams.Add(fResultPer );

  fElapsedMode := TD2XSingleParam<TD2XElapsedMode>.CreateParam('E', 'Show elapsed', '<mode>',
    'Set Elapsed time display to be (N[one], Q[uiet], T[otal], P[rocessing])', emQuiet,
    ConvertElapsedMode, TD2X.ToLabel<TD2XElapsedMode>, nil);
  fParams.Add(fElapsedMode);

  fUseBase := TD2XFlaggedStringParam.CreateFlagStr('B', 'Base dir', '<dir>',
    'Use <dir> as a base for all file lookups', '', False, ConvertDir, nil, nil);
  fParams.Add(fUseBase);

  fOpts.RegisterOtherParams(fParams);

  fParams.Add(lCountChildren);
  fParams.Add(lSkipMethods);

  fOpts.RegisterDefineParams(fParams);
end;

function TD2XParamProcessor.ProcessParamsFile(pFileOrExtn: string): Boolean;
var
  lSL: TStringList;
  i: Integer;
  lFile: string;
begin
  Result := True;
  lSL := TStringList.Create;
  lFile := fOpts.FileOpts.InputFileOrExtn(pFileOrExtn);
  try
    lSL.LoadFromFile(lFile);
    for i := 0 to lSL.Count - 1 do
      Result := ProcessParam(lSL[i], lFile, i + 1) and Result;
  finally
    FreeAndNil(lSL);
  end;
end;

function TD2XParamProcessor.ProcessStream(pStream: TStringStream): Boolean;
var
  lTimer: TStopwatch;
  lFile: string;
  lP: TD2XProcessor;
begin
  if fElapsedMode.Value <> emNone then
    Log('Processing %s ... ', [fFilename], False);
  lTimer := TStopwatch.StartNew;
  try
    for lP in fProcs do
      lP.BeginFile;

    InitParser;
    if UseProxy then
      SetProxy;

    Result := False;
    lFile := pStream.DataString;

    if ContainsText(LeftStr(lFile, 16), '<') then
      Exit;

    BeginResults('D2X_File', rpFile);
    fXmlHandler.HasFiles := True;

    if fOpts.LoadDefines then
      fParser.StartDefines.Assign(fOpts.Defines);

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

function TD2XParamProcessor.RecurseDirectory(pDir, pWildCards: string;
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

procedure TD2XParamProcessor.RemoveProxy;
begin
  if Assigned(fVMI) then
  begin
    fVMI.Unproxify(fParser);
    FreeAndNil(fVMI);
  end;
end;

procedure TD2XParamProcessor.SetProxy;
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

function TD2XParamProcessor.SimplePairLog(pPair: TStrIntPair): string;
begin
  Result := IntToStr(pPair.Value);
end;

function TD2XParamProcessor.TidyFilename(pFilename: string): string;
begin
  Result := ReplaceStr(ReplaceStr(ReplaceStr(pFilename, '*', ''), '.', ''), '?', '');
end;

function TD2XParamProcessor.UseProxy: Boolean;
var
  lP: TD2XProcessor;
begin
  Result := False;
  for lP in fProcs do
    Result := Result or lP.UseProxy;
end;

end.
