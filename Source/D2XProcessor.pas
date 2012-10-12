unit D2XProcessor;

interface

uses
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  CastaliaPasLexTypes,
  D2XOptions,
  D2XParser,
  D2Xml,
  D2X;

type
  TStrIntPair = TPair<string, Integer>;
  TStrIntDict = TDictionary<string, Integer>;

  TPairLogMethod = function(pPair: TStrIntPair): string of object;

  TMethodCount = record
    Method: string;
    Children: Integer;
  end;

  TD2XProcessor = class(TD2XLogger)
  private
    fOpts: TD2XOptions;
    fProgramDir: string;

    fDuration: TStopwatch;

    fParser: TD2XDefinesParser;
    fVMI: TVirtualMethodInterceptor;

    fXmlDoc: TD2XmlDoc;
    fXmlNode: TD2XmlNode;

    fStack: TStack<TMethodCount>;
    fCurrent: TMethodCount;

    fDefinesUsed: TStrIntDict;
    fMinChildren: TStrIntDict;
    fMaxChildren: TStrIntDict;
    fSkippedMethods: TStrIntDict;

    fFilename: string;
    fHasFiles: Boolean;

    procedure LogBefore(pMethod: string);
    procedure LogAfter(pMethod: string);

    procedure CountBefore(pMethod: string);
    procedure CountAfter(pMethod: string);

    function SkipBefore(pMethod: string): Boolean;
    function SkipAfter(pMethod: string): Boolean;

    procedure XmlAddAttribute(pName: string); overload;
    procedure XmlAddAttribute(pName, pValue: string); overload;
    procedure XmlAddText; overload;
    procedure XmlAddText(pText: string); overload;

    procedure XmlNodeStart(pMethod: string);
    procedure XmlNodeEnd;

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

    procedure InitParser;

    function TidyFilename(pFilename: string): string;
    procedure BeginResults(pNodename: string; pPer: TD2XResultPer);
    procedure EndResults(pFilename: string; pPer: TD2XResultPer);

    procedure DoBeginResults;
    procedure DoEndResults(pFilename: string);

    function ProcessStream(pStream: TStringStream): Boolean;
    function ProcessInput: Boolean;
    function ProcessFile(pFilename: string): Boolean;
    function ProcessDirectory(pDir, pWildCards: string): Boolean;
    function RecurseDirectory(pDir, pWildCards: string; pMainDir: Boolean): Boolean;

    function SimplePairLog(pPair: TStrIntPair): string;
    function MinMaxPairLog(pPair: TStrIntPair): string;

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
  Xml.XMLIntf,
  Winapi.Windows;

{ TD2XProcessor }

procedure TD2XProcessor.BeginResults(pNodename: string; pPer: TD2XResultPer);
begin
  if fOpts.ResultPer = pPer then
    DoBeginResults;

  if fOpts.ResultPer >= pPer then
  begin
    if fOpts.WriteXml then
      XmlNodeStart(pNodename);
  end;
end;

procedure TD2XProcessor.CountAfter(pMethod: string);
var
  lVal: Integer;
begin
  if fCurrent.Method = pMethod then
  begin
    if fMaxChildren.TryGetValue(fCurrent.Method, lVal) then
    begin
      if fCurrent.Children > lVal then
        fMaxChildren.AddOrSetValue(fCurrent.Method, fCurrent.Children);
    end
    else
      fMaxChildren.AddOrSetValue(fCurrent.Method, fCurrent.Children);

    if fMinChildren.TryGetValue(fCurrent.Method, lVal) then
    begin
      if fCurrent.Children < lVal then
        fMinChildren.AddOrSetValue(fCurrent.Method, fCurrent.Children);
    end
    else
      fMinChildren.AddOrSetValue(fCurrent.Method, fCurrent.Children);
  end;

  if fStack.Count > 0 then
    fCurrent := fStack.Pop
  else
  begin
    fCurrent.Method := '';
    fCurrent.Children := 0;
  end;
end;

procedure TD2XProcessor.CountBefore(pMethod: string);
begin
  Inc(fCurrent.Children);
  fStack.Push(fCurrent);
  fCurrent.Method := pMethod;
  fCurrent.Children := 0;
end;

constructor TD2XProcessor.Create;
begin
  inherited Create;

  fProgramDir := ExtractFilePath(ParamStr(0));
  fDuration := TStopwatch.StartNew;

  fStack := nil;

  fOpts := TD2XOptions.Create(Self);

  fDefinesUsed := TStrIntDict.Create;
  fMaxChildren := TStrIntDict.Create;
  fMinChildren := TStrIntDict.Create;
  fSkippedMethods := TStrIntDict.Create;

  fXmlDoc := nil;
  fXmlNode := nil;

  InitParser;

  fParser.Lexer.InitDefines;
  fParser.Lexer.GetDefines(fOpts.Defines);
end;

procedure TD2XProcessor.DefineUsed(pDef: string);
var
  lVal: Integer;
begin
  if fDefinesUsed.TryGetValue(pDef, lVal) then
    fDefinesUsed[pDef] := lVal + 1
  else
    fDefinesUsed.Add(pDef, 1)
end;

destructor TD2XProcessor.Destroy;
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
  FreeAndNil(fMinChildren);
  FreeAndNil(fMaxChildren);
  FreeAndNil(fSkippedMethods);

  FreeAndNil(fOpts);

  inherited;
end;

procedure TD2XProcessor.DoBeginResults;
begin
  if fOpts.WriteXml then
  begin
    fXmlDoc := NewXmlDocument;
    fXmlDoc.Options := fXmlDoc.Options + [doNodeAutoIndent];
    fHasFiles := False;
  end;
end;

procedure TD2XProcessor.DoEndResults(pFilename: string);
var
  lFile: string;
  lSL: TStringList;
  lFS: TFileStream;
  i: Integer;
const
  DEF_BREAK: array [0 .. 9] of Byte = (13, 10, 42, 42, 42, 42, 13, 10, 13, 10);
begin
  if fOpts.WriteXml then
  begin
    if fHasFiles then
    begin
      lFile := fProgramDir + fOpts.XmlDirectory + ExtractFilePath(pFilename);
      ForceDirectories(lFile);
      lFile := fOpts.XmlDirectory + pFilename;
      fXmlDoc.Xml.SaveToFile(lFile + '.xml');
    end;

    fXmlNode := nil;
    FreeAndNil(fXmlDoc);
  end;

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

procedure TD2XProcessor.EndProcessing;
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
        SaveToFile(fOpts.OutputFileOrExtn(pExtn));
      finally
        Free;
      end;
  end;

begin
  EndResults('', rpRun);

  if fOpts.DefinesUsed then
    OutputStrIntDict(fDefinesUsed, fOpts.DefinesUsedFoE, SimplePairLog);

  if fOpts.CountChildren then
    OutputStrIntDict(fMaxChildren, fOpts.CountChildrenFoE, MinMaxPairLog);

  if fOpts.SkipMethods then
    OutputStrIntDict(fSkippedMethods, fOpts.SkipMethodsFoE + '.log', SimplePairLog);
end;

procedure TD2XProcessor.EndResults(pFilename: string; pPer: TD2XResultPer);
begin
  if fOpts.ResultPer >= pPer then
  begin
    if fOpts.WriteXml then
    begin
      XmlAddAttribute('fileName', pFilename);
      XmlNodeEnd;
    end;
  end;

  if fOpts.ResultPer = pPer then
    if pFilename = '' then
      DoEndResults('(' + TD2X.ToLabel(pPer) + ')')
    else
      DoEndResults(pFilename);
end;

function TD2XProcessor.IsInternalMethod(pMethod: string): Boolean;
begin
  Result := MatchText(pMethod, ['SynError', 'Run']);
end;

procedure TD2XProcessor.LogAfter(pMethod: string);
begin
  Log('AFTER  %s', [pMethod]);
end;

procedure TD2XProcessor.LogBefore(pMethod: string);
begin
  Log('BEFORE %s @ %s', [pMethod, fParser.Lexer.Token]);
end;

procedure TD2XProcessor.LogMessage(pType, pMsg: string);
begin
  LogMessage(pType, pMsg, fParser.Lexer.PosXY.X, fParser.Lexer.PosXY.Y);
end;

procedure TD2XProcessor.LogMessage(pType, pMsg: string; pX, pY: Integer);
var
  lErrFile: string;
  lExists: Boolean;
begin
  lErrFile := fOpts.OutputFileOrExtn('.err');
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
      write(fCurrent.Method);
      write(',');
      write(pType);
      write(',');
      WriteLine(pMsg);
    finally
      Free;
    end;
end;

function TD2XProcessor.MinMaxPairLog(pPair: TStrIntPair): string;
var
  lMin: Integer;
begin
  if fMinChildren.TryGetValue(pPair.Key, lMin) then
    Result := IntToStr(lMin) + ',' + IntToStr(pPair.Value)
  else
    Result := '0,' + IntToStr(pPair.Value);
end;

procedure TD2XProcessor.ParserMessage(pSender: TObject; const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
var
  lNode, lAttr: TD2XmlNode;
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

  if fOpts.Verbose then
    case pTyp of
      meError:
        Log('ERROR @ %d,%d: %s', [pX, pY, pMsg]);
      meNotSupported:
        Log('NOT SUPPORTED @ %d,%d: %s', [pX, pY, pMsg]);
    else
      Log('???? @ %d,%d: %s', [pX, pY, pMsg]);
    end;

  if fOpts.WriteXml and Assigned(fXmlNode) then
  begin
    case pTyp of
      meError:
        lNode := fXmlNode.AddChild('D2X_errorMsg');
      meNotSupported:
        lNode := fXmlNode.AddChild('D2X_notSuppMsg');
    else
      lNode := fXmlNode.AddChild('D2X_unknownMsg');
    end;
    lNode.Text := pMsg;
    // lAttr := fXmlDoc.CreateNode('msgAt', ntAttribute);
    // lNode.AttributeNodes.Add(lAttr);
    lAttr := lNode.AddAttribute('msgAt');
    lAttr.Text := IntToStr(pX) + ',' + IntToStr(pY);
  end;
end;

function TD2XProcessor.ProcessDirectory(pDir, pWildCards: string): Boolean;
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

function TD2XProcessor.ProcessFile(pFilename: string): Boolean;
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
    try
      lSS := TStringStream.Create;
      try
        lSS.LoadFromFile(lFile);

        Result := ProcessStream(lSS);
      finally
        FreeAndNil(lSS);
      end;
    except
      on E: Exception do
      begin
        LogMessage('EXCEPTION', '(' + E.ClassName + ')' + E.Message);
        Result := False;
      end;
    end
  else
    if fOpts.Verbose then
      Log('Cannot find "%s"', [lFile]);
end;

function TD2XProcessor.ProcessInput: Boolean;
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
  procedure TD2XProcessor.LexerOnDefine(pLex: TD2XLexer);
  begin
  pLex.Next;
  end;

  procedure TD2XProcessor.LexerOnElse(pLex: TD2XLexer);
  begin
  pLex.Next;
  end;
*)
procedure TD2XProcessor.LexerOnElseIf(pLex: TD2XLexer);
begin
  pLex.Next;
end;

(*
  procedure TD2XProcessor.LexerOnEndIf(pLex: TD2XLexer);
  begin
  pLex.Next;
  end;
*)
procedure TD2XProcessor.LexerOnIf(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XProcessor.LexerOnIfDef(pLex: TD2XLexer);
begin
  DefineUsed(pLex.DirectiveParam);
  pLex.Next;
end;

(*
  procedure TD2XProcessor.LexerOnIfEnd(pLex: TD2XLexer);
  begin
  pLex.Next;
  end;
*)
procedure TD2XProcessor.LexerOnIfNDef(pLex: TD2XLexer);
begin
  DefineUsed(pLex.DirectiveParam);
  pLex.Next;
end;

procedure TD2XProcessor.LexerOnIfOpt(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XProcessor.LexerOnInclude(pLex: TD2XLexer);
var
  lFile: string;
begin
  lFile := pLex.DirectiveParam;

  if fOpts.Verbose then
    Log('INCLUDE @ %d,%d: %s', [pLex.PosXY.X, pLex.PosXY.Y, lFile]);

  if fOpts.WriteXml and Assigned(fXmlNode) then
  begin
    XmlNodeStart('IncludeFile');
    XmlAddAttribute('filename', lFile);
    XmlNodeEnd;
  end;

  pLex.Next;
end;

(*
  procedure TD2XProcessor.LexerOnUnDef(pLex: TD2XLexer);
  begin
  pLex.Next;
  end;
*)
function TD2XProcessor.ProcessParam(pStr, pFrom: string; pIdx: Integer): Boolean;
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

procedure TD2XProcessor.InitParser;
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
    fParser.AddAttribute := XmlAddAttribute;
    fParser.AddText := XmlAddText;

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
  end;
end;

function TD2XProcessor.ProcessParamsFile(pFileOrExtn: string): Boolean;
var
  lSL: TStringList;
  i: Integer;
  lFile: string;
begin
  Result := True;
  lSL := TStringList.Create;
  lFile := fOpts.InputFileOrExtn(pFileOrExtn);
  try
    lSL.LoadFromFile(lFile);
    for i := 0 to lSL.Count - 1 do
      Result := ProcessParam(lSL[i], lFile, i + 1) and Result;
  finally
    FreeAndNil(lSL);
  end;
end;

function TD2XProcessor.ProcessStream(pStream: TStringStream): Boolean;
var
  lTimer: TStopwatch;
  i: Integer;
  lFile: string;
  lCurrNode: TD2XmlNode;
begin
  if fOpts.ElapsedMode <> emNone then
    Log('Processing %s ... ', [fFilename], False);
  lTimer := TStopwatch.StartNew;
  try
    if fOpts.SkipMethods then
      with TStringList.Create do
        try
          LoadFromFile(fOpts.InputFileOrExtn(fOpts.SkipMethodsFoE));
          fSkippedMethods.Clear;
          for i := 0 to Count - 1 do
            if Names[i] = '' then
              fSkippedMethods.Add(Strings[i], 0)
            else
              fSkippedMethods.Add(Names[i], 0);
        finally
          Free;
        end;

    InitParser;
    if UseProxy then
      SetProxy;

    Result := False;
    lFile := pStream.DataString;

    if ContainsText(LeftStr(lFile, 16), '<') then
      Exit;

    BeginResults('D2X_File', rpFile);
    fHasFiles := True;

    if fOpts.LoadDefines then
      fParser.StartDefines.Assign(fOpts.Defines);

    try
      fParser.ProcessString(fFilename, lFile);
      Result := True;
    except
      on E: Exception do
      begin
        LogMessage('EXCEPTION', '(' + E.ClassName + ')' + E.Message);

        lCurrNode := fXmlNode;
        while Assigned(lCurrNode) and (lCurrNode.LocalName <> 'D2X_File') do
          lCurrNode := lCurrNode.ParentNode;

        if Assigned(lCurrNode) then
          fXmlNode := lCurrNode;
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

procedure TD2XProcessor.XmlAddAttribute(pName, pValue: string);
var
  lAttr: TD2XmlNode;
begin
  if Assigned(fXmlNode) then
  begin
    // lAttr := fXmlDoc.CreateNode(pName, ntAttribute);
    // fXmlNode.AttributeNodes.Add(lAttr);
    lAttr := fXmlNode.AddAttribute(pName);
    lAttr.Text := pValue;
  end;
end;

procedure TD2XProcessor.XmlAddText;
begin
  XmlAddText(fParser.LastTokens);
  fParser.LastTokens := '';
end;

procedure TD2XProcessor.XmlAddAttribute(pName: string);
begin
  XmlAddAttribute(pName, fParser.LastTokens);
  fParser.LastTokens := '';
end;

procedure TD2XProcessor.XmlAddText(pText: string);
var
  lText: TD2XmlNode;
begin
  if Assigned(fXmlNode) then
    if fXmlNode.HasChildNodes then
    begin
      // lText := fXmlDoc.CreateNode('', ntText);
      // fXmlNode.ChildNodes.Add(lText);
      lText := fXmlNode.AddChild('');
      lText.Text := pText;
    end
    else
      fXmlNode.Text := fXmlNode.Text + pText;
end;

{$WARN SYMBOL_PLATFORM OFF}

function TD2XProcessor.RecurseDirectory(pDir, pWildCards: string; pMainDir: Boolean): Boolean;
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

procedure TD2XProcessor.RemoveProxy;
begin
  if Assigned(fVMI) then
  begin
    FreeAndNil(fStack);

    fVMI.Unproxify(fParser);
    FreeAndNil(fVMI);
  end;
end;

procedure TD2XProcessor.SetProxy;
begin
  if fOpts.CountChildren then
  begin
    fCurrent.Method := '';
    fCurrent.Children := 0;
    fStack := TStack<TMethodCount>.Create;
  end;

  fVMI := TVirtualMethodInterceptor.Create(TObject(fParser).ClassType);
  fVMI.Proxify(fParser);
  fVMI.OnBefore :=
      procedure(pInst: TObject; pMethod: TRttiMethod; const pArgs: TArray<TValue>;
      out pDoInvoke: Boolean; out pResult: TValue)
    begin
      pDoInvoke := True;
      if IsInternalMethod(pMethod.Name) then
        Exit;
      if fOpts.SkipMethods and SkipBefore(pMethod.Name) then
        Exit;
      if fOpts.Verbose then
        LogBefore(pMethod.Name);
      if fOpts.CountChildren then
        CountBefore(pMethod.Name);
      if fOpts.WriteXml then
        XmlNodeStart(pMethod.Name);
    end;
  fVMI.OnAfter :=
      procedure(pInst: TObject; pMethod: TRttiMethod; const pArgs: TArray<TValue>;
      var pResult: TValue)
    begin
      if IsInternalMethod(pMethod.Name) then
        Exit;
      if fOpts.SkipMethods and SkipAfter(pMethod.Name) then
        Exit;
      if fOpts.WriteXml then
        XmlNodeEnd;
      if fOpts.CountChildren then
        CountAfter(pMethod.Name);
      if fOpts.Verbose then
        LogAfter(pMethod.Name);
    end;
end;

function TD2XProcessor.SimplePairLog(pPair: TStrIntPair): string;
begin
  Result := IntToStr(pPair.Value);
end;

function TD2XProcessor.SkipAfter(pMethod: string): Boolean;
begin
  Result := fSkippedMethods.ContainsKey(pMethod);
end;

function TD2XProcessor.SkipBefore(pMethod: string): Boolean;
var
  lVal: Integer;
begin
  Result := fSkippedMethods.TryGetValue(pMethod, lVal);
  if Result then
    fSkippedMethods[pMethod] := lVal + 1;
end;

function TD2XProcessor.TidyFilename(pFilename: string): string;
begin
  Result := ReplaceStr(ReplaceStr(ReplaceStr(pFilename, '*', ''), '.', ''), '?', '');
end;

function TD2XProcessor.UseProxy: Boolean;
begin
  Result := fOpts.WriteXml or fOpts.CountChildren;
end;

procedure TD2XProcessor.XmlNodeEnd;
begin
  if Assigned(fXmlNode) then
  begin
    if fOpts.FinalToken and (Length(fParser.LastTokens) > 1) then
      XmlAddAttribute('lastToken');

    fXmlNode.Xml;
    fXmlNode := fXmlNode.ParentNode;
  end;
end;

procedure TD2XProcessor.XmlNodeStart(pMethod: string);
begin
  if Assigned(fXmlDoc) then
  begin
    if Assigned(fXmlNode) then
      fXmlNode := fXmlNode.AddChild(pMethod)
    else
    begin
      fXmlNode := fXmlDoc.AddChild(pMethod);
      XmlAddAttribute('parseMode', TD2X.ToLabel(fOpts.ParseMode));
    end;
    fParser.LastTokens := '';
  end;
end;

{ TD2XOptions }

end.
