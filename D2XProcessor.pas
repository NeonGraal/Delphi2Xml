unit D2XProcessor;

interface

uses
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  Xml.XMLIntf,
  CastaliaPasLexTypes,
  D2XOptions,
  D2XParser;

type
  TPairLogMethod = function(pPair: TPair<string, Integer>): string of object;

  TMethodCount = record
    Method: string;
    Children: Integer;
  end;

  TStrIntDict = TDictionary<string, Integer>;

  TD2XProcessor = class
  private
    fOpts: TD2XOptions;
    fOutputTimestamp: string;
    fProgramDir: string;

    fDuration: TStopwatch;

    fParser: TD2XDefinesParser;
    fVMI: TVirtualMethodInterceptor;

    fXmlDoc: IXMLDocument;
    fXmlNode: IXMLNode;

    fStack: TStack<TMethodCount>;
    fCurrent: TMethodCount;

    fDefinesUsed: TStrIntDict;
    fMinChildren: TStrIntDict;
    fMaxChildren: TStrIntDict;
    fSkippedMethods: TStrIntDict;

    fFilename: string;

    procedure LogBefore(pMethod: string);
    procedure LogAfter(pMethod: string);

    procedure CountBefore(pMethod: string);
    procedure CountAfter(pMethod: string);

    function SkipBefore(pMethod: string): Boolean;
    function SkipAfter(pMethod: string): Boolean;

    procedure XmlAddAttribute(pName, pValue: string);

    procedure XmlNodeStart(pMethod: string);
    procedure XmlNodeEnd(_pMethod: string);

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

    procedure WriteChangedDefines;

    function GlobalFilename(pOutput: Boolean; pBaseExtn: string): string;

    function ProcessParamsFile(pFilename: string): Boolean;

    procedure InitParser;

    function ProcessFile: Boolean; overload;
    function ProcessFile(pFilename: string): Boolean; overload;
    function ProcessDirectory(pDir, pWildCards: string): Boolean;
    function RecurseDirectory(pDir, pWildCards: string): Boolean;

    function SimplePairLog(pPair: TPair<string, Integer>): string;
    function MinMaxPairLog(pPair: TPair<string, Integer>): string;

  public
    constructor Create;
    destructor Destroy; override;

    procedure EndProcessing;

    function ProcessParam(pStr: string): Boolean;

    property Options: TD2XOptions read fOpts;

  end;

implementation

uses
  Xml.XMLDoc,
  System.IOUtils,
  System.StrUtils;

{ TD2XProcessor }

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
  fOutputTimestamp := FormatDateTime('-HH-mm', Now);
  fDuration := TStopwatch.StartNew;

  fStack := nil;

  fOpts := TD2XOptions.Create;

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
  Writeln(Format('Total processing time %0.3f', [fDuration.Elapsed.TotalSeconds]));

  RemoveProxy;

  FreeAndNil(fParser);

  FreeAndNil(fDefinesUsed);
  FreeAndNil(fMinChildren);
  FreeAndNil(fMaxChildren);
  FreeAndNil(fSkippedMethods);

  FreeAndNil(fOpts);

  inherited;
end;

procedure TD2XProcessor.EndProcessing;
  procedure OutputStrIntDict(pDict: TStrIntDict; pExtn: string; pFunc: TPairLogMethod);
  var
    lP: TPair<string, Integer>;
  begin
    with TStringList.Create do
      try
        for lP in pDict do
          if lP.Value > 0 then
            Values[lP.Key] := pFunc(lP);
        Sort;
        SaveToFile(GlobalFilename(True, pExtn));
      finally
        Free;
      end;
  end;

begin
  if fOpts.DefinesUsed then
    OutputStrIntDict(fDefinesUsed, fOpts.UsedExtension, SimplePairLog);

  if fOpts.CountChildren then
    OutputStrIntDict(fMaxChildren, fOpts.CountExtension, MinMaxPairLog);

  if fOpts.SkipMethods then
    OutputStrIntDict(fSkippedMethods, fOpts.SkipExtension + '.log', SimplePairLog);
end;

function TD2XProcessor.IsInternalMethod(pMethod: string): Boolean;
begin
  Result := MatchText(pMethod, ['SynError', 'Run']);
end;

procedure TD2XProcessor.LogAfter(pMethod: string);
begin
  Writeln('AFTER  ', pMethod);
end;

procedure TD2XProcessor.LogBefore(pMethod: string);
begin
  Writeln('BEFORE ', pMethod, ' @ ', fParser.Lexer.Token);
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
  lErrFile := GlobalFilename(True, '.err');
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

function TD2XProcessor.MinMaxPairLog(pPair: TPair<string, Integer>): string;
var
  lMin: Integer;
begin
  if fMinChildren.TryGetValue(pPair.Key, lMin) then
    Result := IntToStr(lMin) + ',' + IntToStr(pPair.Value)
  else
    Result := '0,' + IntToStr(pPair.Value);
end;

function TD2XProcessor.GlobalFilename(pOutput: Boolean; pBaseExtn: string): string;
  function MakeGlobalFilename(pDir, pExtn: string): string;
  var
    lPath: string;
  begin
    lPath := fProgramDir + pDir;
    ForceDirectories(lPath);
    Result := IncludeTrailingPathDelimiter(lPath) +
      ChangeFileExt(ExtractFileName(ParamStr(0)), pExtn)
  end;

begin
  if pOutput then
    if fOpts.TimestampFiles then
      Result := MakeGlobalFilename(fOpts.OutputDirectory, fOutputTimestamp + pBaseExtn)
    else
      Result := MakeGlobalFilename(fOpts.OutputDirectory, pBaseExtn)
  else
    Result := MakeGlobalFilename(fOpts.InputDirectory, pBaseExtn)
end;

procedure TD2XProcessor.ParserMessage(pSender: TObject; const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
var
  lAttr: IXMLNode;
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
        Writeln('ERROR @ ', pX, ',', pY, ': ', pMsg);
      meNotSupported:
        Writeln('NOT SUPPORTED @ ', pX, ',', pY, ': ', pMsg);
    else
      Writeln('???? @ ', pX, ',', pY, ': ', pMsg);
    end;

  if fOpts.Xml and Assigned(fXmlNode) then
  begin
    case pTyp of
      meError:
        lAttr := fXmlDoc.CreateNode('errorMsg', ntAttribute);
      meNotSupported:
        lAttr := fXmlDoc.CreateNode('notSuppMsg', ntAttribute);
    else
      lAttr := fXmlDoc.CreateNode('unknownMsg', ntAttribute);
    end;
    lAttr.Text := pMsg;
    fXmlNode.AttributeNodes.Add(lAttr);
    lAttr := fXmlDoc.CreateNode('msgAt', ntAttribute);
    lAttr.Text := IntToStr(pX) + ',' + IntToStr(pY);
    fXmlNode.AttributeNodes.Add(lAttr);
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
    if FindFirst(lPath + lFile, faAnyFile - faDirectory, lFF) = 0 then
      try
        repeat
          Result := ProcessFile(pDir + lFF.Name) or Result;
        until FindNext(lFF) <> 0;
      finally
        FindClose(lFF);
      end
end;

function TD2XProcessor.ProcessFile(pFilename: string): Boolean;
begin
  fFilename := pFilename;

  Result := ProcessFile;
end;

function TD2XProcessor.ProcessFile: Boolean;
var
  lSS: TStringStream;
  lFile: string;
  lPhase: string;
  i: Integer;
  lTimer: TStopwatch;
begin
  Result := False;
  lFile := fFilename;
  if fOpts.UseBase then
    lFile := fOpts.BaseDirectory + lFile;
  if FileExists(lFile) then
    try
      lPhase := 'Initial';
      write('Processing ', fFilename, ' ... ');
      lTimer := TStopwatch.StartNew;
      try
        if fOpts.SkipMethods then
          with TStringList.Create do
            try
              LoadFromFile(GlobalFilename(False, fOpts.SkipExtension));
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
        RemoveProxy;
        if UseProxy then
          SetProxy;

        lSS := TStringStream.Create;
        try
          lPhase := 'Loading';
          lSS.LoadFromFile(lFile);

          lFile := lSS.DataString;

          if ContainsText(LeftStr(lFile, 16), '<') then
            Exit;

          if fOpts.LoadDefines then
            fParser.StartDefines.Assign(fOpts.Defines);

          lPhase := 'Parsing';
          fParser.ProcessString(fFilename, lFile);

          if fOpts.Xml then
          begin
            lPhase := 'Preparing Xml';
            lFile := fProgramDir + fOpts.XmlDirectory + ExtractFilePath(fFilename);
            ForceDirectories(lFile);
            lFile := fOpts.XmlDirectory + fFilename;

            lPhase := 'Writing Xml';
            lFile := lFile + '.xml';
            fXmlDoc.Xml.SaveToFile(lFile);
          end;

          lPhase := 'Writing Defines';
          if fOpts.WriteDefines then
            WriteChangedDefines;

          Result := True;
        finally
          FreeAndNil(lSS);
        end;
      finally
        lTimer.Stop;
        Writeln(Format('%0.3f', [lTimer.Elapsed.TotalSeconds]));
      end;
    except
      on E: Exception do
      begin
        LogMessage('EXCEPTION', '(' + E.ClassName + ')' + E.Message + ' at ' + lPhase);
        Result := False;
      end;
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
    Writeln('INCLUDE @ ', pLex.PosXY.X, ',', pLex.PosXY.Y, ': ', lFile);

  if fOpts.Xml and Assigned(fXmlNode) then
  begin
    XmlNodeStart('IncludeFile');
    XmlAddAttribute('filename', lFile);
    XmlNodeEnd('IncludeFile');
  end;

  pLex.Next;
end;

(*
  procedure TD2XProcessor.LexerOnUnDef(pLex: TD2XLexer);
  begin
  pLex.Next;
  end;
*)
function TD2XProcessor.ProcessParam(pStr: string): Boolean;
var
  lPath, lFile: string;
begin
  Result := False;
  try
    if (Length(pStr) > 1) and CharInSet(pStr[1], ['-', '/']) then
      Result := Options.ParseOption(pStr)
    else
      if (Length(pStr) > 1) and (pStr[1] = '@') then
        Result := ProcessParamsFile(Copy(pStr, 2))
      else
      begin
        Result := ProcessFile(pStr);
        if not Result then
        begin
          lPath := ExtractFilePath(pStr);
          lFile := ExtractFileName(pStr);
          Result := ProcessDirectory(lPath, lFile);
          if fOpts.Recurse then
            Result := RecurseDirectory(lPath, lFile) or Result;
        end;
      end;
  except
    on E: Exception do
      Writeln('EXCEPTION (', E.ClassName, ') processing "', pStr, '" : ', E.Message);
  end;
end;

procedure TD2XProcessor.InitParser;
begin
  if Assigned(fParser) then
  begin
    RemoveProxy;
    if StartsText('U', fOpts.ParseMode) then
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

  if not Assigned(fParser) then
  begin
    if StartsText('U', fOpts.ParseMode) then
      fParser := TD2XUsesParser.Create
    else
      fParser := TD2XFullParser.Create;

    fParser.OnMessage := ParserMessage;
    fParser.AddAttribute := XmlAddAttribute;

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

function TD2XProcessor.ProcessParamsFile(pFilename: string): Boolean;
var
  lSL: TStringList;
  lS: string;
begin
  Result := True;
  lSL := TStringList.Create;
  try
    lSL.LoadFromFile(pFilename);
    for lS in lSL do
      Result := ProcessParam(lS) and Result;
  finally
    FreeAndNil(lSL);
  end;
end;

procedure TD2XProcessor.WriteChangedDefines;
var
  lFile: string;
  lSL: TStringList;
  lFS: TFileStream;
  i: Integer;
const
  DEF_BREAK: array [0 .. 9] of Byte = (13, 10, 42, 42, 42, 42, 13, 10, 13, 10);
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
      lFile := fProgramDir + fOpts.DefinesDirectory + ExtractFilePath(fFilename);
      ForceDirectories(lFile);
      lFS := TFileStream.Create(fOpts.DefinesDirectory + fFilename + '.def', fmCreate);
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

procedure TD2XProcessor.XmlAddAttribute(pName, pValue: string);
var
  lAttr: IXMLNode;
begin
  lAttr := fXmlDoc.CreateNode(pName, ntAttribute);
  lAttr.Text := pValue;
  fXmlNode.AttributeNodes.Add(lAttr);
  fParser.LastTokens := '';
end;

function TD2XProcessor.RecurseDirectory(pDir, pWildCards: string): Boolean;
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

  if FindFirst(lPath + '*', faAnyFile - faNormal - faTemporary, lFF) = 0 then
    try
      repeat
        if (lFF.Name <> '.') and (lFF.Name <> '..') then
        begin
          lFile := IncludeTrailingPathDelimiter(pDir + lFF.Name);
          Result := ProcessDirectory(lFile, pWildCards) or Result;
          Result := RecurseDirectory(lFile, pWildCards) or Result;
        end;
      until FindNext(lFF) <> 0;
    finally
      FindClose(lFF);
    end;
end;

procedure TD2XProcessor.RemoveProxy;
begin
  if Assigned(fVMI) then
  begin
    FreeAndNil(fStack);

    fXmlDoc := nil;
    fXmlNode := nil;

    fVMI.Unproxify(fParser);
    FreeAndNil(fVMI);
  end;
end;

procedure TD2XProcessor.SetProxy;
begin
  if fOpts.Xml then
  begin
    fXmlDoc := NewXmlDocument;
    fXmlDoc.Options := fXmlDoc.Options + [doNodeAutoIndent];
  end;

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
      if fOpts.Xml then
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
      if fOpts.Xml then
        XmlNodeEnd(pMethod.Name);
      if fOpts.CountChildren then
        CountAfter(pMethod.Name);
      if fOpts.Verbose then
        LogAfter(pMethod.Name);
    end;
end;

function TD2XProcessor.SimplePairLog(pPair: TPair<string, Integer>): string;
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

function TD2XProcessor.UseProxy: Boolean;
begin
  Result := fOpts.Xml or fOpts.CountChildren;
end;

procedure TD2XProcessor.XmlNodeEnd(_pMethod: string);
begin
  if Assigned(fXmlNode) then
  begin
    if Length(fParser.LastTokens) > 1 then
      XmlAddAttribute('lastToken', fParser.LastTokens);

    fXmlNode := fXmlNode.ParentNode;
  end;
end;

procedure TD2XProcessor.XmlNodeStart(pMethod: string);
begin
  if Assigned(fXmlDoc) then
  begin
    if Assigned(fXmlNode) then
      fXmlNode := fXmlNode.AddChild(pMethod)
    else begin
      fXmlNode := fXmlDoc.AddChild(pMethod);
      XmlAddAttribute('parseMode', fOpts.ParseMode);
    end;
    fParser.LastTokens := '';
  end;
end;

{ TD2XOptions }

end.
