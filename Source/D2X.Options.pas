unit D2X.Options;

interface

uses
  CastaliaPasLexTypes,
  D2X.Global,
  D2X.Handlers,
  D2X.Param,
  D2X.Params,
  D2X.Parser,
  D2X.Processor,
  D2X.IO,
  D2X.IO.ErrorLog,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils;

type
  ED2XOptionsException = class(Exception);

  TD2XOptions = class(TD2XLogger)
  strict private
    fProcs: TObjectList<TD2XProcessor>;

    fDuration: TStopwatch;

    fParser: TD2XDefinesParser;
    fVMI: TVirtualMethodInterceptor;

    fErrorLog: TD2XErrorLog;

    fIOFact: ID2XIOFactory;

    fFlags: TD2XFlagsParam;
    fParseMode: TD2XSingleParam<TD2XParseMode>;
    fResultPer: TD2XSingleParam<TD2XResultPer>;
    fElapsedMode: TD2XSingleParam<TD2XElapsedMode>;
    fParserDefines: TD2XDefinesParam;
    fHeldDefines: TD2XDefinesParam;

    procedure RemoveProxy;
    procedure SetBaseProxy;
    procedure SetMinProxy;
    procedure SetFullProxy;
    function UseFullProxy: Boolean;

    function IsInternalMethod(pMethod: string): Boolean;

    procedure LogParser(pType, pMsg: string);

    procedure LexerOnInclude(pLex: TD2XLexer);

    procedure InitParser;

    function GetRecurse: Boolean;

    function MakeNamedRef(pVal: TD2XSingleParam<string>): TD2XNamedFileRef;
    function MakeFileRef(pVal: TD2XSingleParam<string>; pSuffix: string = ''): TD2XFileRef;

  protected
    fParams: TD2XParams;
    function GetParserDefines: TStringList;
    function GetHeldDefines: TStringList;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure InitProcessors(pFileOpts: ID2XIOFactory);
    procedure InitFlags;
    procedure InitFirstProcessors;
    procedure InitOtherProcessors;

    function ProcessOption(pOpt: string): Boolean;
    function ConfigFileOrExtn(pFileOrExtn: string): ID2XIOFile;

    procedure BeginResults(pNodename: string; pPer: TD2XResultPer);
    procedure EndResults(pFilename: string; pPer: TD2XResultPer);

    function ProcessStream(pFilename: string; pStream: TStreamReader): Boolean;
    function ProcessInput: Boolean;
    function ProcessFile(pFilename: string; pInDir: Boolean): Boolean;
    function ProcessDirectory(pDir, pWildCards: string): Boolean;
    function RecurseDirectory(pDir, pWildCards: string; pMainDir: Boolean): Boolean;
    function ProcessParam(pParam, pFrom: string): Boolean;

    procedure EndProcessing;

    procedure DescribeAll;

    property Recurse: Boolean read GetRecurse;
  end;

implementation

uses
  D2X.IO.Options,

  D2X.Tree.Json,
  D2X.Tree.Xml,
  System.StrUtils;

{ TD2XOptions }

procedure TD2XOptions.BeginResults(pNodename: string; pPer: TD2XResultPer);
var
  lP: TD2XProcessor;
begin
  if fResultPer.Value = pPer then
    for lP in fProcs do
      lP.BeginResults;

  if fResultPer.Value >= pPer then
    for lP in fProcs do
      lP.BeginMethod(pNodename);
end;

constructor TD2XOptions.Create;
begin
  inherited Create;

  fDuration := TStopwatch.StartNew;

  fParams := TD2XParams.Create;

  fProcs := TObjectList<TD2XProcessor>.Create;
end;

procedure TD2XOptions.DescribeAll;
begin
  fParams.DescribeAll(Self);
end;

destructor TD2XOptions.Destroy;
begin
  RemoveProxy;
  FreeAndNil(fParser);
  FreeAndNil(fErrorLog);

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

  fDuration.Stop;
  if fElapsedMode.Value = emQuiet then
    Log('Processing finished!', [])
  else
    if fElapsedMode.Value > emNone then
      Log('Total processing time %0.3f', [fIOFact.GetDuration(fDuration)]);
end;

procedure TD2XOptions.EndResults(pFilename: string; pPer: TD2XResultPer);
var
  lP: TD2XProcessor;
begin
  if fResultPer.Value >= pPer then
    for lP in fProcs do
    begin
      lP.AddAttr('fileName', pFilename);
      lP.EndMethod('');
    end;

  if fResultPer.Value = pPer then
  begin
    if pFilename = '' then
      pFilename := '(' + TD2X.ToLabel(pPer) + ')';
    for lP in fProcs do
      lP.EndResults(pFilename);
  end;
end;

function TD2XOptions.GetHeldDefines: TStringList;
begin
  Result := fHeldDefines.Defines;
end;

function TD2XOptions.GetParserDefines: TStringList;
begin
  Result := fParserDefines.Defines;
end;

function TD2XOptions.GetRecurse: Boolean;
begin
  Result := fFlags.ByLabel['Recurse'].Flag;
end;

function TD2XOptions.IsInternalMethod(pMethod: string): Boolean;
begin
  Result := MatchText(pMethod, ['SynError', 'Run']);
end;

procedure TD2XOptions.LogParser(pType, pMsg: string);
begin
  fErrorLog.LogMessage(pType, pMsg, fParser.Lexer.PosXY.X, fParser.Lexer.PosXY.Y);
end;

function TD2XOptions.MakeFileRef(pVal: TD2XSingleParam<string>; pSuffix: string): TD2XFileRef;
begin
  Result := function: ID2XIOFile
    begin
      Result := fIOFact.LogFileOrExtn(pVal.Value + pSuffix);
    end;
end;

function TD2XOptions.MakeNamedRef(pVal: TD2XSingleParam<string>): TD2XNamedFileRef;
begin
  Result := function(pFilename: string): ID2XIOFile
    var
      lDir, lExtn: string;
    begin
      SplitDirExtn(pVal.Value, lDir, lExtn);
      if lDir > '' then
        lDir := IncludeTrailingPathDelimiter(lDir);
      if (lExtn > '') and not StartsText('.', lExtn) then
        lExtn := '.' + lExtn;
      Result := fIOFact.SimpleFile(lDir + pFilename + lExtn);
    end;
end;

function TD2XOptions.ProcessDirectory(pDir, pWildCards: string): Boolean;
var
  lPath: ID2XIODir;
  lFile, lGroup, lS: string;
  lTimer: TStopwatch;
  lP: TD2XProcessor;
const
  INIT_GROUP = '~~~~';
  procedure DirEndResults(pName: string; pPer: TD2XResultPer);
  begin
    if pDir > '' then
      EndResults(IncludeTrailingPathDelimiter(pDir) + pName, pPer)
    else
      EndResults(pName, pPer);
  end;

begin
  Result := False;
  lTimer := TStopwatch.Create;

  lPath := fIOFact.BaseDir(pDir);
  try
    for lFile in SplitString(pWildCards, ',') do
      if lPath.FirstFile(lFile) then
        try
          if not lTimer.IsRunning then
          begin
            if fElapsedMode.Value >= emDir then
              Log('Processing %s ... ', [pDir], fElapsedMode.Value > emDir);
            BeginResults('D2X_SubDir', rpSubDir);
            lTimer.Start;
          end;
          BeginResults('D2X_Pattern', rpWildcard);
          lGroup := INIT_GROUP;
          repeat
            lS := ExtractGroup(lPath.Current);
            if lGroup <> lS then
            begin
              if lGroup <> INIT_GROUP then
                DirEndResults('Group-' + lGroup, rpGroup);
              lGroup := lS;
              BeginResults('D2X_Group', rpGroup);
              for lP in fProcs do
                lP.AddAttr('name', lGroup);
            end;
            Result := ProcessFile(lPath.Current, True) or Result;
          until not lPath.Next;
          if lGroup <> INIT_GROUP then
            DirEndResults('Group-' + lGroup, rpGroup);
          DirEndResults('Pattern-' + TidyFilename(lFile), rpWildcard);
        finally
          lPath.Close;
        end;
  finally
    if lTimer.IsRunning then
    begin
      lTimer.Stop;
      if ExcludeTrailingPathDelimiter(pDir) = '' then
        EndResults('(Dir)', rpSubDir)
      else
        EndResults(ExcludeTrailingPathDelimiter(pDir), rpSubDir);
      case fElapsedMode.Value of
        emDir:
          Log('%0.3f', [fIOFact.GetDuration(lTimer)]);
        emFile, emProcessing:
          Log('Processed %s in %0.3f', [pDir, fIOFact.GetDuration(lTimer)]);
        emQuiet:
          Log('Processed %s', [pDir]);
      end;

    end;
    DisposeOf(lPath);
  end;
end;

function TD2XOptions.ProcessFile(pFilename: string; pInDir: Boolean): Boolean;
var
  lFile: ID2XIOFile;
begin
  Result := False;
  lFile := fIOFact.BaseFile(pFilename);
  if Assigned(lFile) then
    try
      try
        if lFile.Exists then
          Result := ProcessStream(pFilename, lFile.ReadFrom)
        else
          if pInDir then
            Log('Cannot find "%s"', [lFile.Description]);
      finally
        DisposeOf(lFile);
      end;
    except
      on e: Exception do
        fErrorLog.LogMessage('EXCEPTION', '(' + e.ClassName + ')' + e.Message, 0, 0);
    end;
end;

function TD2XOptions.ProcessInput: Boolean;
var
  lSR: TStreamReader;
begin
  lSR := fIOFact.GetInputStream;
  try
    Result := ProcessStream('(Input)', lSR);
  finally
    FreeAndNil(lSR);
  end;
end;

procedure TD2XOptions.LexerOnInclude(pLex: TD2XLexer);
begin
  pLex.Next;
end;

function TD2XOptions.ProcessParam(pParam, pFrom: string): Boolean;
var
  lPath, lFile: string;
begin
  BeginResults('D2X_Param', rpParam);
  if pParam = '-' then
    Result := ProcessInput
  else
  begin
    Result := ProcessFile(pParam, False);
    if not Result then
    begin
      lPath := ExtractFilePath(pParam);
      lFile := ExtractFileName(pParam);
      BeginResults('D2X_Dir', rpDir);
      Result := ProcessDirectory(lPath, lFile);
      if ExcludeTrailingPathDelimiter(lPath) = '' then
        EndResults('(Base)', rpDir)
      else
        EndResults(ExcludeTrailingPathDelimiter(lPath), rpDir);
      if Recurse then
        Result := RecurseDirectory(lPath, lFile, True) or Result;
    end;
  end;
  EndResults(TidyFilename(pFrom), rpParam);
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
    lPrm := fParams.ForCode(pOpt);
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
begin
  fProcs.Add(TD2XProcessor.CreateHandler(fFlags.ByLabel['LogErrors'],
      TD2XErrorHandler.CreateError(meError, fErrorLog.LogMessage), True));
  fProcs.Add(TD2XProcessor.CreateHandler(fFlags.ByLabel['LogNotSupp'],
      TD2XErrorHandler.CreateError(meNotSupported, fErrorLog.LogMessage), True));

  fProcs.Add(TD2XProcessor.CreateClass(fFlags.ByLabel['Verbose'], TD2XLogHandler));

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
      lFile: ID2XIOFile;
      lPrm: TD2XParam;
    begin
      Result := True;
      if pStr > '' then
        if StartsText('-', pStr) and (Length(pStr) > 1) then
        begin
          lPrm := fParams.ForCode(Copy(pStr, 2, Length(pStr)));
          if Assigned(lPrm) then
          begin
            if Length(pStr) > 2 then
              lPrm.Report(Self, Copy(pStr, 3, Length(pStr)))
            else
              lPrm.Report(Self);
          end
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
end;

procedure TD2XOptions.InitFlags;
var
  lFlags: TD2XFlagDefines;
begin
  SetLength(lFlags, 6);
  lFlags[0] := FlagDef('V', 'Verbose', 'Log all Parser methods called');
  lFlags[1] := FlagDef('T', 'Timestamp', 'Timestamp global output files');
  lFlags[2] := FlagDef('R', 'Recurse', 'Recurse into subdirectories');
  lFlags[3] := FlagDef('N', 'LogNotSupp', 'Log Not Supported messages');
  lFlags[4] := FlagDef('F', 'FinalToken', 'Record Final Token', True);
  lFlags[5] := FlagDef('E', 'LogErrors', 'Log Error messages', True);

  fFlags := TD2XFlagsParam.CreateFlags(lFlags);
end;

procedure TD2XOptions.InitOtherProcessors;
var
  lParserDefinesHandler: TD2XParserDefinesHandler;
  lHeldDefinesHandler: TD2XHeldDefinesHandler;

  lCountDefinesUsed: TD2XFlaggedStringParam;
  lSkipMethods: TD2XFlaggedStringParam;
  lCountChildren: TD2XFlaggedStringParam;
  lCountFinalDefines: TD2XFlaggedStringParam;

  function NilFileRef: TD2XFileRef;
  begin
    Result := function: ID2XIOFile
      begin
        Result := nil;
      end;
  end;

begin
  fParserDefines := TD2XDefinesParam.CreateDefines('D', 'Defines', ConfigFileOrExtn);
  fHeldDefines := TD2XDefinesParam.CreateDefines('H', 'Held Defines', ConfigFileOrExtn);

  lCountDefinesUsed := TD2XFlaggedStringParam.CreateFlagStr('CU', 'Count Defines Used',
    '<f/e>', 'Count Defines Used into <f/e>', '.used', True, ConvertExtn);
  lSkipMethods := TD2XFlaggedStringParam.CreateFlagStr('S', 'Skipped Methods', '<f/e>',
    'Load Skipped Methods from <f/e>', '.skip', False, ConvertFile);
  lCountChildren := TD2XFlaggedStringParam.CreateFlagStr('CC', 'Count Children', '<f/e>',
    'Report Min/Max Children into <f/e>', '.chld', True, ConvertExtn);
  lCountFinalDefines := TD2XFlaggedStringParam.CreateFlagStr('CF', 'Count Final Defines',
    '<f/e>', 'Count Final Defines into <f/e>', '.final', True, ConvertExtn);

  lParserDefinesHandler := TD2XParserDefinesHandler.CreateDefines(fParserDefines.Defines);
  lHeldDefinesHandler := TD2XHeldDefinesHandler.CreateDefines(fHeldDefines.Defines);

  fProcs.Add(TD2XProcessor.CreateClass(lSkipMethods, TD2XSkipHandler).SetFileInput(
    function: ID2XIOFile
    begin
      Result := fIOFact.ConfigFileOrExtn(lSkipMethods.Value);
    end).SetProcessingOutput(MakeFileRef(lSkipMethods, '.log')));
  fProcs.Add(TD2XProcessor.CreateClass(lCountChildren, TD2XCountChildrenHandler)
    .SetFileInput(NilFileRef()).SetFileOutput(NilFileRef())
    .SetProcessingOutput(MakeFileRef(lCountChildren)));
  fProcs.Add(TD2XProcessor.CreateClass(lCountFinalDefines, TD2XCountFinalDefinesHandler)
    .SetFileOutput(NilFileRef()).SetProcessingOutput(MakeFileRef(lCountFinalDefines)));
  fProcs.Add(TD2XProcessor.CreateClass(lCountDefinesUsed, TD2XCountDefinesUsedHandler)
    .SetProcessingOutput(MakeFileRef(lCountDefinesUsed)));
  fProcs.Add(TD2XProcessor.CreateHandler(fParserDefines, lParserDefinesHandler, True));
  fProcs.Add(TD2XProcessor.CreateHandler(fHeldDefines, lHeldDefinesHandler, True));

  fParams.AddRange([lCountChildren, lCountFinalDefines, lCountDefinesUsed, lSkipMethods,
    fParserDefines, fHeldDefines]);
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

    fParser.AddAttribute := procedure(pName, pValue: string)
      var
        lP: TD2XProcessor;
      begin
        for lP in fProcs do
          lP.AddAttr(pName, pValue);
      end;

    fParser.AddText := procedure(pText: string)
      var
        lP: TD2XProcessor;
      begin
        for lP in fProcs do
          lP.AddText(pText);
      end;
    fParser.TrimChildren := procedure(pElement: string)
      var
        lP: TD2XProcessor;
      begin
        for lP in fProcs do
          lP.TrimChildren(pElement);
      end;

    fParser.Lexer.OnIncludeDirect := LexerOnInclude;

    for lP in fProcs do
      lP.InitParser(fParser);
  end;

  if fElapsedMode.Value = emProcessing then
  begin
    fParser.OnProgress := procedure(pProgress: Integer)
      begin
        Log('%3d%%'#8#8#8#8, [pProgress], False);
      end;
  end
  else
    fParser.OnProgress := nil;
end;

procedure TD2XOptions.InitProcessors(pFileOpts: ID2XIOFactory);
var
  lWriteXml, lWriteJson, lWriteDefines: TD2XFlaggedStringParam;
  lGetParseMode: TD2XStringRef;
begin
  InitFlags;

  fIOFact := pFileOpts;
  fErrorLog := TD2XErrorLog.Create(pFileOpts);

  fIOFact.SetGlobalValidator(
    function(pVal: string): Boolean
    begin
      if Assigned(lWriteXml) then
        lWriteXml.Convert(pVal);
      if Assigned(lWriteJson) then
        lWriteJson.Convert(pVal);
      if Assigned(lWriteDefines) then
        lWriteDefines.Convert(pVal);
      Result := True;
    end);
  fIOFact.SetTimestampFlag(fFlags.ByLabel['Timestamp']);

  fParseMode := TD2XSingleParam<TD2XParseMode>.CreateParamOnSet('M', 'Parse mode', '<mode>',
    'Parser type (F[ull], U[ses])', pmFull, TD2X.CnvEnum<TD2XParseMode>,
    TD2X.ToLabel<TD2XParseMode>,
    procedure(pOld, pNew, pDflt: TD2XParseMode)
    begin
      if pNew = pmUses then
        fIOFact.SetGlobalName('Uses');
    end);
  fResultPer := TD2XSingleParam<TD2XResultPer>.CreateParam('P', 'Results per', '<per>',
    'Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un])', rpFile,
    TD2X.CnvEnum<TD2XResultPer>, TD2X.ToLabel<TD2XResultPer>);
  fElapsedMode := TD2XSingleParam<TD2XElapsedMode>.CreateParam('E', 'Show elapsed', '<mode>',
    'Elapsed time display (N[one], T[otal], D[ir], F[ile], P[rocessing], [Q]uiet)', emQuiet,
    TD2X.CnvEnum<TD2XElapsedMode>, TD2X.ToLabel<TD2XElapsedMode>);

  lGetParseMode := function: string
    begin
      Result := TD2X.ToLabel(fParseMode.Value);
    end;

  lWriteXml := TD2XFlaggedStringParam.CreateFlagStr('WX', 'Write XML', '<d/e>',
    'Write XML files into current or given <d/e>', 'Xml,xml', True, ConvertDirExtn);
  lWriteJson := TD2XFlaggedStringParam.CreateFlagStr('WJ', 'Write JSON', '<d/e>',
    'Write JSON files into current or given <d/e>', 'Json,json', False, ConvertDirExtn);
  lWriteDefines := TD2XFlaggedStringParam.CreateFlagStr('WD', 'Write Defines', '<d/e>',
    'Write Final Defines files into current or given <d/e>', 'Defines,def', False,
    ConvertDirExtn);

  fProcs.Add(TD2XProcessor.CreateHandler(lWriteXml, TD2XTreeHandler.CreateTree(TD2XXmlWriter,
      fFlags.ByLabel['FinalToken'], lGetParseMode), True)
    .SetResultsOutput(MakeNamedRef(lWriteXml)));
  fProcs.Add(TD2XProcessor.CreateHandler(lWriteJson, TD2XTreeHandler.CreateTree(TD2XJsonWriter,
      fFlags.ByLabel['FinalToken'], lGetParseMode), True)
    .SetResultsOutput(MakeNamedRef(lWriteJson)));
  fProcs.Add(TD2XProcessor.CreateClass(lWriteDefines, TD2XWriteDefinesHandler)
    .SetResultsOutput(MakeNamedRef(lWriteDefines)));

  InitFirstProcessors;

  fParams.AddRange([fFlags]);
  fIOFact.RegisterParams(fParams);
  fParams.AddRange([fParseMode, fResultPer, fElapsedMode, lWriteXml, lWriteJson,
    lWriteDefines]);

  InitOtherProcessors;

  InitParser;
end;

function TD2XOptions.ConfigFileOrExtn(pFileOrExtn: string): ID2XIOFile;
begin
  Result := fIOFact.ConfigFileOrExtn(pFileOrExtn);
end;

function TD2XOptions.ProcessStream(pFilename: string; pStream: TStreamReader): Boolean;
var
  lTimer: TStopwatch;
  lContent: string;
  lP: TD2XProcessor;
begin
  if fElapsedMode.Value >= emFile then
    Log('Processing %s ... ', [pFilename], False);
  lTimer := TStopwatch.StartNew;
  try
    InitParser;
    if UseFullProxy then
      SetFullProxy
    else
      SetMinProxy;

    Result := False;
    lContent := pStream.ReadToEnd;

    if ContainsText(LeftStr(lContent, 16), '<') then
      Exit;

    BeginResults('D2X_File', rpFile);

    fErrorLog.SetFilename(pFilename);
    for lP in fProcs do
      lP.BeginFile(pFilename);

    try
      fErrorLog.ResetFile;

      fParser.ProcessString(pFilename, lContent);
      Result := True;
    except
      on e: Exception do
      begin
        LogParser('EXCEPTION', '(' + e.ClassName + ')' + e.Message);

        for lP in fProcs do
          lP.RollbackTo('D2X_File');
      end;
    end;

    for lP in fProcs do
      lP.EndFile(pFilename);

    EndResults(pFilename, rpFile);
  finally
    lTimer.Stop;
    case fElapsedMode.Value of
      emFile, emProcessing:
        Log('%0.3f', [fIOFact.GetDuration(lTimer)]);
      emQuiet:
        Log('done', []);
    end;

  end;
end;

function TD2XOptions.RecurseDirectory(pDir, pWildCards: string;
  pMainDir: Boolean): Boolean;
var
  lPath: ID2XIODir;
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
            BeginResults('D2X_Dir', rpDir);
          Result := ProcessDirectory(lFile, pWildCards) or Result;
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
end;

procedure TD2XOptions.RemoveProxy;
begin
  if Assigned(fVMI) then
  begin
    fVMI.Unproxify(fParser);
    FreeAndNil(fVMI);
  end;
end;

procedure TD2XOptions.SetFullProxy;
begin
  SetBaseProxy;
  fVMI.OnBefore :=
      procedure(pInst: TObject; pMethod: TRttiMethod; const pArgs: TArray<TValue>;
    out pDoInvoke: Boolean; out pResult: TValue)
    var
      lP: TD2XProcessor;
    begin
      pDoInvoke := True;
      fErrorLog.SetMethod(pMethod.Name);
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

procedure TD2XOptions.SetMinProxy;
begin
  SetBaseProxy;
  fVMI.OnBefore :=
      procedure(pInst: TObject; pMethod: TRttiMethod; const pArgs: TArray<TValue>;
    out pDoInvoke: Boolean; out pResult: TValue)
    begin
      pDoInvoke := True;
      fErrorLog.SetMethod(pMethod.Name);
    end;
end;

procedure TD2XOptions.SetBaseProxy;
begin
  fVMI := TVirtualMethodInterceptor.Create(TObject(fParser).ClassType);
  fVMI.Proxify(fParser);
end;

function TD2XOptions.UseFullProxy: Boolean;
var
  lP: TD2XProcessor;
begin
  Result := False;
  for lP in fProcs do
    Result := Result or lP.UseProxy;
end;

end.
