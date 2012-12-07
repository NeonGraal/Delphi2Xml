unit D2X.Options;

interface

uses
  CastaliaPasLexTypes,
  D2X,
  D2X.Handlers,
  D2X.Param,
  D2X.Params,
  D2X.Parser,
  D2X.Processor,
  D2X.IO,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils;

type
  ED2XOptionsException = class(Exception);

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

    fIOFact: ID2XIOFactory;

    fVerbose: TD2XBooleanParam;
    fRecurse: TD2XBooleanParam;
    fParseMode: TD2XSingleParam<TD2XParseMode>;
    fResultPer: TD2XSingleParam<TD2XResultPer>;
    fElapsedMode: TD2XSingleParam<TD2XElapsedMode>;
    fParserDefines: TD2XDefinesParam;

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

    procedure InitParser;

    function GetRecurse: Boolean;

    function MakeNamedRef(pVal: TD2XSingleParam<string>; pSuffix: string): TD2XNamedStreamRef;
    function MakeFileRef(pVal: TD2XSingleParam<string>; pSuffix: string = ''): TD2XFileRef;

  protected
    function GetDefines: TStringList;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure InitProcessors(pFileOpts: ID2XIOFactory);
    procedure InitFirstProcessors;
    procedure InitOtherProcessors;

    function ProcessOption(pOpt: string): Boolean;
    function ConfigFileOrExtn(pFileOrExtn: string): ID2XFile;

    procedure BeginResults(pNodename: string; pPer: TD2XResultPer);
    procedure EndResults(pFilename: string; pPer: TD2XResultPer);

    function ProcessStream(pFilename: string; pStream: TStreamReader): Boolean;
    function ProcessInput: Boolean;
    function ProcessFile(pFilename: string): Boolean;
    function ProcessDirectory(pDir, pWildCards: string): Boolean;
    function RecurseDirectory(pDir, pWildCards: string; pMainDir: Boolean): Boolean;
    function ProcessParam(pParam, pFrom: string): Boolean;

    procedure EndProcessing;

    property Recurse: Boolean read GetRecurse;
  end;

implementation

uses
  D2X.Processors,
  D2X.IO.Actual,
  D2X.IO.Options,
  System.IOUtils,
  System.StrUtils,
  System.TypInfo,
  Winapi.Windows;

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

constructor TD2XOptions.Create;
begin
  inherited Create;

  fProgramDir := ExtractFilePath(ParamStr(0));
  fDuration := TStopwatch.StartNew;

  fParams := TD2XParams.Create;

  fProcs := TObjectList<TD2XProcessor>.Create;
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

  DisposeOf(fIOFact);
  FreeAndNil(fProcs);
  FreeAndNil(fParams);

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

function TD2XOptions.GetDefines: TStringList;
begin
  Result := fParserDefines.Defines;
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

function TD2XOptions.MakeFileRef(pVal: TD2XSingleParam<string>; pSuffix: string): TD2XFileRef;
begin
  Result := function: ID2XFile
    begin
      Result := fIOFact.LogFileOrExtn(pVal.Value + pSuffix);
    end;
end;

function TD2XOptions.MakeNamedRef(pVal: TD2XSingleParam<string>;
  pSuffix: string): TD2XNamedStreamRef;
begin
  Result := function(pFilename: string): ID2XFile
    begin
      Result := fIOFact.SimpleFile(pVal.Value + pFilename + pSuffix);
    end;
end;

procedure TD2XOptions.LogMessage(pType, pMsg: string; pX, pY: Integer);
var
  lErrFile: ID2XFile;
  lExists: Boolean;
  lDS: TD2XInterfaced;
begin
  lErrFile := fIOFact.LogFileOrExtn('.err');
  try
    lExists := lErrFile.Exists;
    with lErrFile.WriteTo(True) do
    begin
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
      { TODO : Determine current Method name }
      write(''{fCurrent.Method});
      write(',');
      write(pType);
      write(',');
      WriteLine(pMsg);
    end;
  finally
    if Assigned(lErrFile) then
    begin
      lDS := lErrFile as TD2XInterfaced;
      lErrFile := nil;
      lDS.Free;
    end;
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
  lPath: ID2XDir;
  lFile: string;
begin
  Result := False;

  lPath := fIOFact.BaseDir(pDir);
  try
    for lFile in SplitString(pWildCards, ',') do
      if lPath.FirstFile(lFile) then
        try
          BeginResults('D2X_Pattern', rpWildcard);
          repeat
            Result := ProcessFile(lPath.Current) or Result;
          until not lPath.Next;
          EndResults(pDir + 'Pattern-' + TidyFilename(lFile), rpWildcard);
        finally
          lPath.Close;
        end;
  finally
    DisposeOf(lPath);
  end;
end;

function TD2XOptions.ProcessFile(pFilename: string): Boolean;
var
  lFile: ID2XFile;
begin
  Result := False;
  lFile := fIOFact.BaseFile(pFilename);
  if Assigned(lFile) then
    try
      if lFile.Exists then
        Result := ProcessStream(pFilename, lFile.ReadFrom)
      else
        { TODO -oStruan -cTodo : Is thi sthe correct flag to check? }
        if fVerbose.Value then
          Log('Cannot find "%s"', [lFile]);
    finally
      DisposeOf(lFile);
    end;
end;

function TD2XOptions.ProcessInput: Boolean;
var
  lSR: TStreamReader;
begin
  try
    lSR := TStreamReader.Create(THandleStream.Create(GetStdHandle(STD_INPUT_HANDLE)));
    lSR.OwnStream;

    Result := ProcessStream('(Input)', lSR);
  finally
    FreeAndNil(lSR);
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

function TD2XOptions.ProcessParam(pParam, pFrom: string): Boolean;
var
  lPath, lFile: string;
begin
  BeginResults('D2X_Param', rpParam);
  if pParam = '-' then
    Result := ProcessInput
  else
  begin
    Result := ProcessFile(pParam);
    if not Result then
    begin
      lPath := ExtractFilePath(pParam);
      lFile := ExtractFileName(pParam);
      BeginResults('D2X_Dir', rpDir);
      Result := ProcessDirectory(lPath, lFile);
      EndResults(ExcludeTrailingPathDelimiter(lPath), rpDir);
      if Recurse then
        Result := RecurseDirectory(lPath, lFile, True) or Result;
    end;
  end;
  EndResults(pFrom, rpParam);
end;

function TD2XOptions.ProcessOption(pOpt: string): Boolean;
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

procedure TD2XOptions.InitFirstProcessors;
var
  lLogErrors: TD2XBooleanParam;
  lLogNotSupported: TD2XBooleanParam;

  lLogProcessor: TD2XLogProcessor;
begin
  fVerbose := TD2XBooleanParam.CreateBool('V', 'Verbose', 'Log all Parser methods called');
  lLogErrors := TD2XBooleanParam.CreateBool('L', 'Log Errors', 'Log Error messages', True);
  lLogNotSupported := TD2XBooleanParam.CreateBool('N', 'Log Not Supp',
    'Log Not Supported messages');

  fProcs.Add(TD2XHandlerProcessor.CreateHandler(lLogErrors,
    TD2XErrorHandler.CreateError(meError, LogMessage), True));
  fProcs.Add(TD2XHandlerProcessor.CreateHandler(lLogNotSupported,
    TD2XErrorHandler.CreateError(meNotSupported, LogMessage), True));

  lLogProcessor := TD2XLogProcessor.Create(fVerbose);
  lLogProcessor.JoinLog(Self);
  fProcs.Add(lLogProcessor);

  fParams.Add(TD2XParam.Create('?', 'Options', '', 'Show valid options',
      function(pStr: string): Boolean
    begin
      Result := True;
      fParams.DescribeAll(Self);
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
      lFile: ID2XFile;
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
          lFile := fIOFact.LogFileOrExtn(MakeFileName(pStr, '.prm'));
          lSL := TStringList.Create;
          try
            fParams.OutputAll(lSL);
            if lSL.Count > 0 then
              lSL.SaveToStream(lFile.WriteTo.BaseStream);
          finally
            FreeAndNil(lSL);
            DisposeOf(lFile);
          end;
        end
      else
        fParams.ReportAll(Self);
    end));

  fParams.AddRange([fVerbose, lLogErrors, lLogNotSupported]);
end;

procedure TD2XOptions.InitOtherProcessors;
var
  lParserDefinesHandler: TD2XParserDefinesHandler;

  lDefinesUsed: TD2XFlaggedStringParam;
  lSkipMethods: TD2XFlaggedStringParam;
  lCountChildren: TD2XFlaggedStringParam;

  function NilFileRef: TD2XFileRef;
  begin
    Result := function: ID2XFile
      begin
        Result := nil;
      end;
  end;

begin
  fParserDefines := TD2XDefinesParam.CreateDefines('D', 'Defines', ConfigFileOrExtn);
  lDefinesUsed := TD2XFlaggedStringParam.CreateFlagStr('U', 'Defines Used', '<f/e>',
    'Report Defines Used into <f/e>', '.used', True, ConvertExtn, nil, nil);
  lSkipMethods := TD2XFlaggedStringParam.CreateFlagStr('S', 'Skipped Methods', '<f/e>',
    'Load Skipped Methods from <f/e>', '.skip', True, ConvertFile, nil, nil);
  lCountChildren := TD2XFlaggedStringParam.CreateFlagStr('C', 'Count Children', '<f/e>',
    'Report Min/Max Children into <f/e>', '.cnt', True, ConvertExtn, nil, nil);

  lParserDefinesHandler := TD2XParserDefinesHandler.CreateDefines(fParserDefines.Defines);

  fProcs.Add(TD2XHandlerProcessor.CreateHandler(lDefinesUsed, fDefinesUsedHandler, True)
    .SetProcessingOutput(MakeFileRef(lDefinesUsed)));
  fProcs.Add(TD2XHandlerProcessor.CreateClass(lSkipMethods, TD2XSkipHandler).SetFileInput(
    function: ID2XFile
    begin
      Result := fIOFact.ConfigFileOrExtn(lSkipMethods.Value);
    end).SetProcessingOutput(MakeFileRef(lSkipMethods, '.log')));
  fProcs.Add(TD2XHandlerProcessor.CreateClass(lCountChildren, TD2XCountHandler)
    .SetFileInput(NilFileRef()).SetFileOutput(NilFileRef())
    .SetProcessingOutput(MakeFileRef(lCountChildren)));
  fProcs.Add(TD2XHandlerProcessor.CreateHandler(fParserDefines, lParserDefinesHandler, True));

  fParams.AddRange([lDefinesUsed, lCountChildren, lSkipMethods, fParserDefines]);
end;

procedure TD2XOptions.InitParser;
var
  lP: TD2XProcessor;
  lC: TD2XDefinesParserClass;
begin
  if Assigned(fParseMode) and (fParseMode.Value = pmUses) then
    lC := TD2XUsesParser
  else
    lC := TD2XFullParser;

  if Assigned(fParser) then
  begin
    RemoveProxy;
    if not(fParser is lC) then
      FreeAndNil(fParser);
  end;

  if not Assigned(fParser) then
  begin
    fParser := lC.Create;

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

procedure TD2XOptions.InitProcessors(pFileOpts: ID2XIOFactory);
var
  lFinalToken: TD2XBooleanParam;
  lWriteXml: TD2XFlaggedStringParam;
  lWriteDefines: TD2XFlaggedStringParam;
begin
  fIOFact := pFileOpts;

  fIOFact.SetGlobalValidator(
      function(pVal: string): Boolean
    begin
      if Assigned(lWriteXml) then
        lWriteXml.Value := IncludeTrailingPathDelimiter(pVal);
      if Assigned(lWriteDefines) then
        lWriteDefines.Value := IncludeTrailingPathDelimiter(pVal);
      Result := True;
    end);

  fRecurse := TD2XBooleanParam.CreateBool('R', 'Recurse', 'Recurse into subdirectories');
  fParseMode := TD2XSingleParam<TD2XParseMode>.CreateParam('M', 'Parse mode', '<mode>',
    'Set Parsing mode (F[ull], U[ses])', pmFull, TD2X.CnvEnum<TD2XParseMode>,
    TD2X.ToLabel<TD2XParseMode>,
    function(pVal: TD2XParseMode): Boolean
    begin
      Result := True;
      if pVal = pmUses then
        fIOFact.SetGlobalName('Uses');
    end);
  fResultPer := TD2XSingleParam<TD2XResultPer>.CreateParam('P', 'Results per', '<per>',
    'Set Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un])', rpFile,
    TD2X.CnvEnum<TD2XResultPer>, TD2X.ToLabel<TD2XResultPer>, nil);
  fElapsedMode := TD2XSingleParam<TD2XElapsedMode>.CreateParam('E', 'Show elapsed', '<mode>',
    'Set Elapsed time display to be (N[one], Q[uiet], T[otal], P[rocessing])', emQuiet,
    TD2X.CnvEnum<TD2XElapsedMode>, TD2X.ToLabel<TD2XElapsedMode>, nil);

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

  lFinalToken := TD2XBooleanParam.CreateBool('F', 'Final Token', 'Record Final Token', True);
  lWriteXml := TD2XFlaggedStringParam.CreateFlagStr('X', 'Generate XML', '<dir>',
    'Generate XML files into current or given <dir>', 'Xml\', True, ConvertDir, nil, nil);
  lWriteDefines := TD2XFlaggedStringParam.CreateFlagStr('W', 'Write Defines', '<dir>',
    'Generate Final Defines files into current or given <dir>', 'Defines\', False, ConvertDir,
    nil, nil);

  fProcs.Add(TD2XHandlerProcessor.CreateHandler(lWriteXml, fXmlHandler, True)
    .SetResultsOutput(MakeNamedRef(lWriteXml, '.xml')));
  fProcs.Add(TD2XHandlerProcessor.CreateClass(lWriteDefines, TD2XWriteDefinesHandler)
    .SetResultsOutput(MakeNamedRef(lWriteDefines, '.def')));

  InitFirstProcessors;

  fParams.AddRange([lFinalToken, fRecurse]);
  fIOFact.RegisterParams(fParams);
  fParams.AddRange([fParseMode, fResultPer, fElapsedMode, lWriteXml, lWriteDefines]);

  InitOtherProcessors;

  InitParser;
end;

function TD2XOptions.ConfigFileOrExtn(pFileOrExtn: string): ID2XFile;
begin
  Result := fIOFact.ConfigFileOrExtn(pFileOrExtn);
end;

function TD2XOptions.ProcessStream(pFilename: string; pStream: TStreamReader): Boolean;
var
  lTimer: TStopwatch;
  lContent: string;
  lP: TD2XProcessor;
begin
  if fElapsedMode.Value <> emNone then
    Log('Processing %s ... ', [pFilename], False);
  lTimer := TStopwatch.StartNew;
  try
    InitParser;
    if UseProxy then
      SetProxy;

    Result := False;
    lContent := pStream.ReadToEnd;

    if ContainsText(LeftStr(lContent, 16), '<') then
      Exit;

    BeginResults('D2X_File', rpFile);
    fXmlHandler.HasFiles := True;

    fFilename := pFilename;
    for lP in fProcs do
      lP.BeginFile(pFilename);

    try
      fParser.ProcessString(pFilename, lContent);
      Result := True;
    except
      on E: Exception do
      begin
        LogParser('EXCEPTION', '(' + E.ClassName + ')' + E.Message);

        fXmlHandler.RollbackTo('D2X_File');
      end;
    end;

    for lP in fProcs do
      lP.EndFile(pFilename);

    EndResults(pFilename, rpFile);
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

function TD2XOptions.RecurseDirectory(pDir, pWildCards: string;
pMainDir: Boolean): Boolean;
var
  lPath: ID2XDir;
  lFile: string;
begin
  Result := False;

  lPath := fIOFact.BaseDir(pDir);
  try
    if lPath.FirstDir then
      try
        repeat
          lFile := lPath.Current;
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
        until not lPath.Next;
      finally
        lPath.Close;
      end;
  finally
    DisposeOf(lPath);
  end;
  {
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
  }
end;

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

end.
