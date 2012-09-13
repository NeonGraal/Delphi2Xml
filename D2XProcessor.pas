unit D2XProcessor;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  Xml.XMLIntf,
  CastaliaPasLexTypes,
  D2XParser;

type
  TD2XOptions = class
  private
    fVerbose: Boolean;
    fXml: Boolean;
    fCountChildren: Boolean;
    fCountExtension: string;
    fXmlDirectory: string;
    fSkipExtension: string;
    fSkipMethods: Boolean;
    fUseBase: Boolean;
    fBaseDirectory: string;
    fRecurse: Boolean;
    fLogErrors: Boolean;
    fLogNotSupported: Boolean;
    fTimestampFiles: Boolean;
    fDefinesUsed: Boolean;
    fUsedExtension: string;
    fLoadExtension: string;
    fLoadDefines: Boolean;
    fDefines: TStringList;
    fWriteDefines: Boolean;
    fDefinesDirectory: string;

    procedure AddDefine(pDef: string);
    procedure DeleteDefine(pDef: string);
    procedure LoadDefinesFile(pFile: string);

  public
    property LogErrors: Boolean read fLogErrors;
    property LogNotSupported: Boolean read fLogNotSupported;
    property TimestampFiles: Boolean read fTimestampFiles;

    property Verbose: Boolean read fVerbose;
    property Recurse: Boolean read fRecurse;

    property UseBase: Boolean read fUseBase;
    property BaseDirectory: string read fBaseDirectory;

    property WriteDefines: Boolean read fWriteDefines;
    property DefinesDirectory: string read fDefinesDirectory;

    property Xml: Boolean read fXml;
    property XmlDirectory: string read fXmlDirectory;

    property DefinesUsed: Boolean read fDefinesUsed;
    property UsedExtension: string read fUsedExtension;

    property LoadDefines: Boolean read fLoadDefines;
    property LoadExtension: string read fLoadExtension;

    property Defines: TStringList read fDefines;

    property CountChildren: Boolean read fCountChildren;
    property CountExtension: string read fCountExtension;

    property SkipMethods: Boolean read fSkipMethods;
    property SkipExtension: string read fSkipExtension;

    constructor Create;
    destructor Destroy; override;

    function ParseOption(pOpt: string): Boolean;

    function ReportOptions: Boolean;
    procedure ShowOptions;
  end;

  ED2XOptionsException = class(Exception);

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

    fParser: TD2XParser;
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

    function IsInternalMethod(pMethod: string): Boolean;

    procedure LogMessage(pType, pMsg: string; pX, pY: Integer);

    procedure ParserMessage(pSender: TObject; const pTyp: TMessageEventType;
      const pMsg: string; pX, pY: Integer);

    procedure LexerOnInclude(pLex: TD2XLexer);

    procedure LexerOnDefine(pLex: TD2XLexer);
    procedure LexerOnUnDef(pLex: TD2XLexer);

    procedure DefineUsed(pDef: string);

    procedure LexerOnIfDef(pLex: TD2XLexer);
    procedure LexerOnIfNDef(pLex: TD2XLexer);
    procedure LexerOnIfOpt(pLex: TD2XLexer);
    procedure LexerOnIf(pLex: TD2XLexer);
    procedure LexerOnElseIf(pLex: TD2XLexer);
    procedure LexerOnElse(pLex: TD2XLexer);
    procedure LexerOnEndIf(pLex: TD2XLexer);
    procedure LexerOnIfEnd(pLex: TD2XLexer);

    procedure BeforeParseFile;

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

procedure TD2XProcessor.BeforeParseFile;
var
  lS: string;
begin
  if fOpts.LoadDefines then
  begin
    fParser.Lexer.ClearDefines;
    for lS in fOpts.Defines do
      fParser.Lexer.AddDefine(lS);
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

  fOutputTimestamp := FormatDateTime('-HH-mm', Now);

  fStack := nil;

  fOpts := TD2XOptions.Create;

  fDefinesUsed := TStrIntDict.Create;
  fMaxChildren := TStrIntDict.Create;
  fMinChildren := TStrIntDict.Create;
  fSkippedMethods := TStrIntDict.Create;

  fXmlDoc := nil;
  fXmlNode := nil;

  fParser := TD2XParser.Create;
  fParser.OnMessage := ParserMessage;

  fParser.Lexer.OnIncludeDirect := LexerOnInclude;
  fParser.Lexer.OnDefineDirect := LexerOnDefine;
  fParser.Lexer.OnUnDefDirect := LexerOnUnDef;

  fParser.Lexer.OnIfDirect := LexerOnIf;
  fParser.Lexer.OnIfDefDirect := LexerOnIfDef;
  fParser.Lexer.OnIfNDefDirect := LexerOnIfNDef;
  fParser.Lexer.OnIfOptDirect := LexerOnIfOpt;
  fParser.Lexer.OnElseDirect := LexerOnElse;
  fParser.Lexer.OnElseIfDirect := LexerOnElseIf;
  fParser.Lexer.OnEndIfDirect := LexerOnEndIf;
  fParser.Lexer.OnIfEndDirect := LexerOnIfEnd;

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
        if fOpts.TimestampFiles then
          SaveToFile(ChangeFileExt(ParamStr(0), fOutputTimestamp + pExtn))
        else
          SaveToFile(ChangeFileExt(ParamStr(0), pExtn));
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

procedure TD2XProcessor.LogMessage(pType, pMsg: string; pX, pY: Integer);
var
  lErrFile: string;
  lExists: Boolean;
begin
  if fOpts.TimestampFiles then
    lErrFile := ChangeFileExt(ParamStr(0), fOutputTimestamp + '.err')
  else
    lErrFile := ChangeFileExt(ParamStr(0), '.err');
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
  lMS: TMemoryStream;
  lFile: string;
  lPhase: string;
  i: Integer;
  lSL: TStringList;
begin
  Result := False;
  lFile := fFilename;
  if fOpts.UseBase then
    lFile := fOpts.BaseDirectory + lFile;
  if FileExists(lFile) then
    try
      if fOpts.SkipMethods then
        with TStringList.Create do
          try
            LoadFromFile(ChangeFileExt(ParamStr(0), fOpts.SkipExtension));
            fSkippedMethods.Clear;
            for i := 0 to Count - 1 do
              if Names[i] = '' then
                fSkippedMethods.Add(Strings[i], 0)
              else
                fSkippedMethods.Add(Names[i], 0);
          finally
            Free;
          end;

      lPhase := 'Initial';
      Writeln('Processing ', fFilename, ' ...');
      SetProxy;

      lMS := nil;
      lSS := TStringStream.Create;
      try
        lPhase := 'Loading';
        lSS.LoadFromFile(lFile);

        lFile := lSS.DataString;

        if ContainsText(LeftStr(lFile, 16), '<') then
          Exit;

        lPhase := 'Converting ' + IntToStr(Length(lFile)) + ' x ' + IntToStr(Sizeof(Char));
        lMS := TMemoryStream.Create;
        lMS.Write(PChar(lFile)^, Length(lFile) * Sizeof(Char));

        lPhase := 'Parsing';
        fParser.Run(fFilename, lMS);

        if fOpts.Xml then
        begin
          lPhase := 'Preparing Xml';
          lFile := ExtractFilePath(ParamStr(0)) + fOpts.XmlDirectory +
            ExtractFilePath(fFilename);
          ForceDirectories(lFile);
          lFile := fOpts.XmlDirectory + fFilename;

          lPhase := 'Writing Xml';
          lFile := lFile + '.xml';
          fXmlDoc.Xml.SaveToFile(lFile);
        end;

        lSL := nil;
        if fOpts.WriteDefines then
          try
            lSL := TStringList.Create;
            lSL.Sorted := True;
            lPhase := 'Preparing Defines';
            fParser.Lexer.GetDefines(lSL);

            if lSL.Text <> fOpts.Defines.Text then
            begin
              lFile := ExtractFilePath(ParamStr(0)) + fOpts.DefinesDirectory +
                ExtractFilePath(fFilename);
              ForceDirectories(lFile);
              lFile := fOpts.DefinesDirectory + fFilename;

              lPhase := 'Writing Defines';
              lFile := lFile + '.def';
              lSL.SaveToFile(lFile);
            end;
          finally
            FreeAndNil(lSL);
          end;

        Result := True;
      finally
        FreeAndNil(lMS);
        FreeAndNil(lSS);
      end;
    except
      on E: Exception do
      begin
        Writeln('EXCEPTION (', E.ClassName, ') processing "', fFilename, '" at ', lPhase,
          ' : ', E.Message);
        Result := False;
      end;
    end;
end;

procedure TD2XProcessor.LexerOnDefine(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XProcessor.LexerOnElse(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XProcessor.LexerOnElseIf(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XProcessor.LexerOnEndIf(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XProcessor.LexerOnIf(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XProcessor.LexerOnIfDef(pLex: TD2XLexer);
begin
  DefineUsed(pLex.DirectiveParam);
  pLex.Next;
end;

procedure TD2XProcessor.LexerOnIfEnd(pLex: TD2XLexer);
begin
  pLex.Next;
end;

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

procedure TD2XProcessor.LexerOnUnDef(pLex: TD2XLexer);
begin
  pLex.Next;
end;

function TD2XProcessor.ProcessParam(pStr: string): Boolean;
var
  lPath, lFile: string;
begin
  Result := False;
  try
    if (Length(pStr) > 1) and CharInSet(pStr[1], ['-', '/']) then
      Result := Options.ParseOption(pStr)
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
  RemoveProxy;

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
      if pMethod.Name = 'ParseFile' then
        BeforeParseFile;
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
    else
      fXmlNode := fXmlDoc.AddChild(pMethod);
    fParser.LastTokens := '';
  end;
end;

{ TD2XOptions }

procedure TD2XOptions.AddDefine(pDef: string);
begin
  fLoadDefines := True;
  if fDefines.IndexOf(pDef) < 0 then
    fDefines.Add(pDef);
end;

constructor TD2XOptions.Create;
begin
  inherited;

  fLogErrors := True;
  fLogNotSupported := False;
  fTimestampFiles := False;
  fVerbose := False;
  fUseBase := False;
  fBaseDirectory := '';
  fXml := True;
  fXmlDirectory := '';
  fWriteDefines := False;
  fDefinesDirectory := '';
  fDefinesUsed := True;
  fUsedExtension := '.used';
  fCountChildren := True;
  fCountExtension := '.cnt';
  fLoadDefines := True;
  fLoadExtension := '.def';
  fSkipMethods := True;
  fSkipExtension := '.skip';
  fDefines := TStringList.Create;
  fDefines.Sorted := True;
end;

procedure TD2XOptions.DeleteDefine(pDef: string);
var
  lIdx: Integer;
begin
  lIdx := fDefines.IndexOf(pDef);
  if lIdx >= 0 then
  begin
    fDefines.Delete(lIdx);
    fLoadDefines := True;
  end;
end;

destructor TD2XOptions.Destroy;
begin
  FreeAndNil(fDefines);

  inherited;
end;

procedure TD2XOptions.LoadDefinesFile(pFile: string);
begin
  if pFile = '' then
    fDefines.Clear
  else
    if StartsText('.', pFile) then
      fDefines.LoadFromFile(ChangeFileExt(ParamStr(0), pFile))
    else
      fDefines.LoadFromFile(pFile);
end;

function TD2XOptions.ParseOption(pOpt: string): Boolean;
  function ErrorUnlessSet(out pFlag: Boolean): Boolean;
  begin
    Result := False;
    if (Length(pOpt) = 2) or (pOpt[3] = '+') then
      pFlag := True
    else
      if pOpt[3] = '-' then
        pFlag := False
      else
        Result := True;
  end;
  function ErrorUnlessValue(out pVal: string): Boolean;
  begin
    Result := False;
    if (Length(pOpt) > 2) and (pOpt[3] = ':') then
      pVal := Copy(pOpt, 4, 99)
    else
      Result := True;
  end;
  function ErrorUnlessSetValue(out pFlag: Boolean; out pVal: string): Boolean;
  begin
    Result := False;
    if ErrorUnlessSet(pFlag) then
      if pOpt[3] = ':' then
      begin
        pFlag := True;
        pVal := Copy(pOpt, 4, 99)
      end
      else
        Result := True;
  end;
  function ErrorUnlessSetExtension(out pFlag: Boolean; out pExtn: string;
    pDflt: string): Boolean;
  begin
    Result := False;
    if ErrorUnlessSetValue(pFlag, pExtn) then
      Result := True
    else
      if pExtn = '' then
        pExtn := pDflt
      else
        if pExtn[1] <> '.' then
          pExtn := '.' + pExtn;
  end;
  function ErrorUnlessSetDir(out pFlag: Boolean; out pDir: string): Boolean;
  begin
    Result := False;
    if ErrorUnlessSetValue(pFlag, pDir) then
      Result := True
    else
      if pDir > '' then
        pDir := IncludeTrailingPathDelimiter(pDir);
  end;

var
  lDefine: string;

begin
  Result := False;
  if (Length(pOpt) < 2) or not CharInSet(pOpt[1], ['-', '/']) then
    Writeln('Invalid option: ' + pOpt)
  else
    case pOpt[2] of
      '?':
        Result := False;
      '!':
        Result := ReportOptions;
      'E', 'e':
        if ErrorUnlessSet(fLogErrors) then
          Writeln('Invalid Log Error messages option: ' + pOpt)
        else
          Result := True;
      'N', 'n':
        if ErrorUnlessSet(fLogNotSupported) then
          Writeln('Invalid Log Not Supported messages option: ' + pOpt)
        else
          Result := True;
      'T', 't':
        if ErrorUnlessSet(fTimestampFiles) then
          Writeln('Invalid Timestamp Files option: ' + pOpt)
        else
          Result := True;
      'V', 'v':
        if ErrorUnlessSet(fVerbose) then
          Writeln('Invalid Verbose option: ' + pOpt)
        else
          Result := True;
      'R', 'r':
        if ErrorUnlessSet(fRecurse) then
          Writeln('Invalid Recurse Directories option: ' + pOpt)
        else
          Result := True;
      'D', 'd':
        if ErrorUnlessValue(lDefine) then
          Writeln('Invalid Define option: ' + pOpt)
        else
        begin
          AddDefine(lDefine);
          Result := True;
        end;
      'Z', 'z':
        if ErrorUnlessValue(lDefine) then
          Writeln('Invalid Undefine option: ' + pOpt)
        else
        begin
          DeleteDefine(lDefine);
          Result := True;
        end;
      'L', 'l':
        if ErrorUnlessSetValue(fLoadDefines, fLoadExtension) then
          Writeln('Invalid Load Defines option: ' + pOpt)
        else
        begin
          LoadDefinesFile(fLoadExtension);
          Result := True;
        end;
      'W', 'w':
        if ErrorUnlessSetDir(fWriteDefines, fDefinesDirectory) then
          Writeln('Invalid Write Defines option: ' + pOpt)
        else
          Result := True;
      'B', 'b':
        if ErrorUnlessSetDir(fUseBase, fBaseDirectory) then
          Writeln('Invalid Use Base Directory option: ' + pOpt)
        else
          Result := True;
      'X', 'x':
        if ErrorUnlessSetDir(fXml, fXmlDirectory) then
          Writeln('Invalid Xml option: ' + pOpt)
        else
          Result := True;
      'U', 'u':
        if ErrorUnlessSetExtension(fDefinesUsed, fUsedExtension, '.used') then
          Writeln('Invalid Count Defines Used option: ' + pOpt)
        else
          Result := True;
      'C', 'c':
        if ErrorUnlessSetExtension(fCountChildren, fCountExtension, '.cnt') then
          Writeln('Invalid Count Children option: ' + pOpt)
        else
          Result := True;
      'S', 's':
        if ErrorUnlessSetExtension(fSkipMethods, fSkipExtension, '.skip') then
          Writeln('Invalid Load Skipped Methods option: ' + pOpt)
        else
          Result := True;
    else
      Writeln('Unknown option: ' + pOpt);
    end;
end;

function TD2XOptions.ReportOptions: Boolean;
  function ShowEnabled(pOpt: Boolean; pLabel, pVal: string): string;
  begin
    if pOpt then
    begin
      if pVal > '' then
        Result := 'Enabled  ' + pLabel + pVal
      else
        Result := 'Enabled  ';
    end
    else
      Result := 'Disabled ';
  end;

var
  lS: string;
  w: Integer;

  procedure WriteWidth(pStr: string);
  begin
    write(pStr);
    Inc(w, Length(pStr));
  end;

begin
  Result := True;
  Writeln('Current option settings:');
  Writeln('  Errors                  ', ShowEnabled(fLogErrors, '', ''));
  Writeln('  Not Supported           ', ShowEnabled(fLogNotSupported, '', ''));
  Writeln('  Timestamp Files         ', ShowEnabled(fTimestampFiles, '', ''));
  Writeln('  Verbose                 ', ShowEnabled(fVerbose, '', ''));
  Writeln('  Recurse                 ', ShowEnabled(fRecurse, '', ''));
  Writeln('  Write defines           ', ShowEnabled(fWriteDefines, 'Dir  ',
      fDefinesDirectory));
  Writeln('  Directory base          ', ShowEnabled(fUseBase, 'Dir  ', fBaseDirectory));
  Writeln('  Xml output              ', ShowEnabled(fXml, 'Dir  ', fXmlDirectory));
  Writeln('  Count defines used      ', ShowEnabled(fDefinesUsed, 'Extn ', fUsedExtension));
  Writeln('  Count min/max children  ', ShowEnabled(fCountChildren, 'Extn ', fCountExtension));
  Writeln('  Skip methods in         ', ShowEnabled(fSkipMethods, 'Extn ', fSkipExtension));

  if fLoadDefines then
    if fDefines.Count < 1 then
      Writeln('Use NO Defines')
    else
    begin
      Writeln('Use these Defines:');
      w := 0;
      for lS in fDefines do
      begin
        if w = 0 then
          WriteWidth('    ')
        else
          if (w + Length(lS)) > 78 then
          begin
            Writeln;
            w := 0;
            WriteWidth('    ');
          end
          else
            WriteWidth(', ');

        WriteWidth(lS);
      end;
    end
  else
    Writeln('Use default Defines');
end;

procedure TD2XOptions.ShowOptions;
var
  lBase: string;
begin
  lBase := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  Writeln('Usage: ', lBase, ' [ Option | Filename | Wildcard ] ... ');
  Writeln('  Options:        Default   Description');
  Writeln('    E[+-]         -         Log Error messages');
  Writeln('    N[+-]         -         Log Not Supported messages');
  Writeln('    T[+-]         -         Timestamp global output files');
  Writeln('    V[+-]         -         Log all Parser methods called');
  Writeln('    R[+-]         +         Recurse into subdirectories');
  Writeln('    D:<define>              Define <define> (also enables "Load Defines")');
  Writeln('    Z:<define>              Undefine <define> (also enables "Load Defines")');
  Writeln('    L[+-]|:<file> -         Load Defines from <file> (no <file> clears all defines)');
  Writeln('    W[+-]|:<dir>  -         Generate Final Defines files into current or given <dir>');
  Writeln('    B[+-]|:<dir>  -         Use <dir> a base for all file lookups');
  Writeln('    X[+-]|:<dir>  +         Generate XML files into current or given <dir>');
  Writeln('    C[+-]|:<ext>  +:cnt     Report Min/Max Children into ', lBase, '.<ext>');
  Writeln('    U[+-]|:<ext>  +:used    Report Defines Used into ', lBase, '.<ext>');
  Writeln('    S[+-]|:<ext>  +:skip    Load Skipped Methods from ', lBase, '.<ext>');
end;

end.
