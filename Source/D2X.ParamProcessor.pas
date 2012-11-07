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
    fProcs: TObjectList<TD2XProcessor>;

    fProgramDir: string;

    fDuration: TStopwatch;

    fParser: TD2XDefinesParser;
    fVMI: TVirtualMethodInterceptor;

    fDefinesUsed: TStrIntDict;

    fFilename: string;

    fLogProcessor: TD2XLogProcessor;
    fXmlHandler: TD2XXmlHandler;

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

    function ProcessParamsFile(pFileOrExtn: string): Boolean;

    procedure InitProcessors;
    procedure InitParser;

    function TidyFilename(pFilename: string): string;
    procedure BeginResults(pNodename: string; pPer: TD2XResultPer);
    procedure EndResults(pFilename: string; pPer: TD2XResultPer);

    procedure DoEndResults(pFilename: string);

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

{ TD2XParamProcessor }

procedure TD2XParamProcessor.BeginResults(pNodename: string; pPer: TD2XResultPer);
var
  lP: TD2XProcessor;
begin
  if fOpts.ResultPer = pPer then
    for lP in fProcs do
      lP.BeginResults;

  if fOpts.ResultPer >= pPer then
    fXmlHandler.BeginMethod(pNodename);
end;

constructor TD2XParamProcessor.Create;
begin
  inherited Create;

  fProgramDir := ExtractFilePath(ParamStr(0));
  fDuration := TStopwatch.StartNew;

  fOpts := TD2XOptions.Create(Self);
  fProcs := TObjectList<TD2XProcessor>.Create;
  InitProcessors;

  fDefinesUsed := TStrIntDict.Create;

  InitParser;

  fParser.Lexer.InitDefines;
  fParser.Lexer.GetDefines(fOpts.Defines);
end;

procedure TD2XParamProcessor.DefineUsed(pDef: string);
var
  lVal: Integer;
begin
  if fDefinesUsed.TryGetValue(pDef, lVal) then
    fDefinesUsed[pDef] := lVal + 1
  else
    fDefinesUsed.Add(pDef, 1)
end;

destructor TD2XParamProcessor.Destroy;
begin
  fDuration.Stop;
  case fOpts.ElapsedMode of
    emNone:
      ;
    emQuiet:
      Log('Processing finished!', []);
  else
    Log('Total processing time %0.3f', [fDuration.Elapsed.TotalSeconds]);
  end;

  RemoveProxy;

  FreeAndNil(fParser);

  FreeAndNil(fDefinesUsed);

  FreeAndNil(fProcs);
  FreeAndNil(fOpts);

  inherited;
end;

procedure TD2XParamProcessor.DoEndResults(pFilename: string);
var
  lFile: string;
  lSL: TStringList;
  lFS: TFileStream;
  i: Integer;
const
  DEF_BREAK: array [0 .. 9] of Byte = (13, 10, 42, 42, 42, 42, 13, 10, 13, 10);
begin
  if fOpts.WriteDefines then
  begin
    lSL := TStringList.Create;
    try
      fParser.GetLexerDefines(lSL);
      fParser.StartDefines.Sort;
      lSL.Sort;
      for i := lSL.Count - 1 downto 1 do
        if lSL[i] = lSL[i - 1] then
          lSL.Delete(i);
      if lSL.Text <> fParser.StartDefines.Text then
      begin
        lFile := fProgramDir + fOpts.DefinesDirectory + ExtractFilePath(pFilename);
        ForceDirectories(lFile);
        lFS := TFileStream.Create(fOpts.DefinesDirectory + pFilename + '.def', fmCreate);
        try
          fParser.StartDefines.SaveToStream(lFS);
          lFS.Write(DEF_BREAK, 10);
          lSL.SaveToStream(lFS);
        finally
          FreeAndNil(lFS);
        end;
      end;
    finally
      FreeAndNil(lSL);
    end;
  end;
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
    OutputStrIntDict(fDefinesUsed, fOpts.DefinesUsedFoE, SimplePairLog);

  for lP in fProcs do
    lP.EndProcessing;
end;

procedure TD2XParamProcessor.EndResults(pFilename: string; pPer: TD2XResultPer);
var
  lP: TD2XProcessor;
begin
  if fOpts.ResultPer >= pPer then
  begin
    fXmlHandler.AddAttr('fileName', pFilename);
    fXmlHandler.EndMethod('');
  end;

  if fOpts.ResultPer = pPer then
  begin
    if pFilename = '' then
      pFilename := '(' + TD2X.ToLabel(pPer) + ')';
    for lP in fProcs do
      lP.EndResults(pFilename);
    DoEndResults(pFilename);
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
      if fOpts.LogErrors then
        LogMessage('Error', pMsg, pX, pY);
    meNotSupported:
      if fOpts.LogNotSupported then
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

  if fOpts.UseBase then
    lPath := fOpts.BaseDirectory + pDir
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
  if fOpts.UseBase then
    lFile := fOpts.BaseDirectory + lFile;
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
    if fOpts.Verbose then
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
  lPrevPer: TD2XResultPer;
begin
  Result := False;
  try
    if (Length(pStr) > 1) then
    begin
      case pStr[1] of
        '-', '/':
          begin
            lPrevPer := fOpts.ResultPer;
            Result := Options.ParseOption(pStr);
            if lPrevPer <> fOpts.ResultPer then
            begin
              if lPrevPer = rpRun then
                EndResults('', fOpts.ResultPer);
              BeginResults('D2X_Run', rpRun);
            end;
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
        if fOpts.Recurse then
          Result := RecurseDirectory(lPath, lFile, True) or Result;
      end;
    end;
    EndResults(pFrom + '-' + IntToStr(pIdx), rpParam);
  except
    on E: Exception do
      Log('EXCEPTION (%s) processing "%s" : %s', [E.ClassName, pStr, E.Message]);
  end;
end;

procedure TD2XParamProcessor.InitParser;
begin
  if Assigned(fParser) then
  begin
    RemoveProxy;
    case fOpts.ParseMode of
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
    case fOpts.ParseMode of
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
        Result := fOpts.FinalToken;
      end,
      function: string
      begin
        Result := TD2X.ToLabel(fOpts.ParseMode);
      end);
  end;
end;

procedure TD2XParamProcessor.InitProcessors;
var
  lHProc: TD2XHandlerProcessor;
begin
  fLogProcessor := TD2XLogProcessor.Create(fOpts.VerboseFlag);
  fLogProcessor.JoinLog(Self);

  fProcs.Add(fLogProcessor);

  lHProc := TD2XHandlerProcessor.CreateHandler(fOpts.SkipMethodsFlag, TD2XSkipHandler.Create);
  lHProc.SetFileInput(
    function: string
    begin
      Result := fOpts.FileOpts.InputFileOrExtn(fOpts.SkipMethodsFoE);
    end);
  lHProc.SetProcessingOutput(
    function: string
    begin
      Result := fOpts.FileOpts.OutputFileOrExtn(fOpts.SkipMethodsFoE + '.log');
    end);
  fProcs.Add(lHProc);

  lHProc := TD2XHandlerProcessor.CreateHandler(fOpts.CountChildrenFlag,
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
      Result := fOpts.FileOpts.OutputFileOrExtn(fOpts.SkipMethodsFoE + '.log');
    end);
  fProcs.Add(lHProc);

  fXmlHandler := TD2XXmlHandler.Create;
  fProcs.Add(TD2XHandlerProcessor.CreateHandler(fOpts.WriteXmlFlag, fXmlHandler));
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
  if fOpts.ElapsedMode <> emNone then
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
    case fOpts.ElapsedMode of
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

  if fOpts.UseBase then
    lPath := fOpts.BaseDirectory + pDir
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
