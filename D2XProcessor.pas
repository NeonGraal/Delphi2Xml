unit D2XProcessor;

interface

uses
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
    fCountExtension: String;
    fXmlDirectory: String;
    fSkipExtension: String;
    fSkipMethods: Boolean;
    fUseBase: Boolean;
    fBaseDirectory: String;
    fRecurse: Boolean;

  public
    property Verbose: Boolean read fVerbose;
    property Recurse: Boolean read fRecurse;

    property UseBase: Boolean read fUseBase;
    property BaseDirectory: String read fBaseDirectory;

    property Xml: Boolean read fXml;
    property XmlDirectory: String read fXmlDirectory;

    property CountChildren: Boolean read fCountChildren;
    property CountExtension: String read fCountExtension;

    property SkipMethods: Boolean read fSkipMethods;
    property SkipExtension: String read fSkipExtension;

    constructor Create;

    function ParseOption(pOpt: String): Boolean;

    function ReportOptions: Boolean;
    procedure ShowOptions;
  end;

  ED2XOptionsException = class(Exception);

  TMethodCount = record
    Method: String;
    Children: Integer;
  end;

  TStrIntDict = TDictionary<string, Integer>;

  TD2XProcessor = class
  private
    fOpts: TD2XOptions;

    fParser: TD2XParser;
    fVMI: TVirtualMethodInterceptor;

    fXmlDoc: IXMLDocument;
    fXmlNode: IXMLNode;

    fStack: TStack<TMethodCount>;
    fCurrent: TMethodCount;
    fMaxChildren: TStrIntDict;
    fSkippedMethods: TStrIntDict;

    procedure LogBefore(pMethod: String);
    procedure LogAfter(pMethod: String);

    procedure CountBefore(pMethod: String);
    procedure CountAfter(pMethod: String);

    function SkipBefore(pMethod: String): Boolean;
    function SkipAfter(pMethod: String): Boolean;

    procedure XmlAddAttribute(pName, pValue: String);

    procedure XmlNodeStart(pMethod: String);
    procedure XmlNodeEnd(_pMethod: String);

    procedure RemoveProxy;
    procedure SetProxy;

    procedure ParserMessage(pSender: TObject; const pTyp: TMessageEventType;
      const pMsg: string; pX, pY: Integer);

    procedure LexerOnInclude(pLex: TD2XLexer);

    function ProcessFile(pFilename: String): Boolean;
    function ProcessDirectory(pDir, pWildCards: String): Boolean;
    function RecurseDirectory(pDir, pWildCards: String): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginProcessing;
    procedure EndProcessing;

    function ProcessParam(pStr: String): Boolean;

    property Options: TD2XOptions read fOpts;

  end;

implementation

uses
  Xml.XMLDoc,
  System.Classes,
  System.StrUtils;

{ TD2XProcessor }

procedure TD2XProcessor.BeginProcessing;
var
  i: Integer;
begin
  if fOpts.CountChildren then
    fMaxChildren := TStrIntDict.Create;
  if fOpts.SkipMethods  then begin
    fSkippedMethods := TStrIntDict.Create;
    with TStringList.Create do
      try
        LoadFromFile(ChangeFileExt(ParamStr(0), fOpts.SkipExtension));
        for i := 0 to Count-1 do
        if Names[i] = '' then
          fSkippedMethods.Add(Strings[i], 0)
        else
          fSkippedMethods.Add(Names[i], 0);
      finally
        Free;
      end;
  end;
end;

procedure TD2XProcessor.CountAfter(pMethod: String);
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
  end;

  if fStack.Count > 0 then
    fCurrent := fStack.Pop
  else
  begin
    fCurrent.Method := '';
    fCurrent.Children := 0;
  end;
end;

procedure TD2XProcessor.CountBefore(pMethod: String);
begin
  Inc(fCurrent.Children);
  fStack.Push(fCurrent);
  fCurrent.Method := pMethod;
  fCurrent.Children := 0;
end;

constructor TD2XProcessor.Create;
begin
  inherited Create;

  fStack := nil;
  fMaxChildren := nil;

  fXmlDoc := nil;
  fXmlNode := nil;

  fOpts := TD2XOptions.Create;
  fParser := TD2XParser.Create;
  fParser.OnMessage := ParserMessage;
  fParser.Lexer.OnIncludeDirect := LexerOnInclude;
end;

destructor TD2XProcessor.Destroy;
begin
  RemoveProxy;

  FreeAndNil(fMaxChildren);
  FreeAndNil(fParser);
  FreeAndNil(fOpts);

  inherited;
end;

procedure TD2XProcessor.EndProcessing;
var
  lP: TPair<string, Integer>;
begin
  if fOpts.CountChildren then
    with TStringList.Create do
      try
        for lP in fMaxChildren do
          if lP.Value > 0 then
            Values[lP.Key] := IntToStr(lP.Value);
        Sort;
        SaveToFile(ChangeFileExt(ParamStr(0), fOpts.CountExtension));
      finally
        Free;
      end;
  if fOpts.SkipMethods then
    with TStringList.Create do
      try
        for lP in fSkippedMethods do
          if lP.Value > 0 then
            Values[lP.Key] := IntToStr(lP.Value);
        Sort;
        SaveToFile(ChangeFileExt(ParamStr(0), fOpts.SkipExtension + '.log'));
      finally
        Free;
      end;
end;

procedure TD2XProcessor.LogAfter(pMethod: String);
begin
  Writeln('AFTER  ', pMethod);
end;

procedure TD2XProcessor.LogBefore(pMethod: String);
begin
  Writeln('BEFORE ', pMethod, ' @ ', fParser.Lexer.Token);
end;

procedure TD2XProcessor.ParserMessage(pSender: TObject;
  const pTyp: TMessageEventType; const pMsg: string; pX, pY: Integer);
var
  lAttr: IXMLNode;
begin
  if fOpts.Verbose then
    if pTyp = meError then
      Writeln('ERROR @ ', pX, ',', pY, ': ', pMsg)
    else
      Writeln('NOT SUPPORTED @ ', pX, ',', pY, ': ', pMsg);

  if fOpts.Xml and Assigned(fXmlNode) then
  begin
    if pTyp = meError then
      lAttr := fXmlDoc.CreateNode('errorMsg', ntAttribute)
    else
      lAttr := fXmlDoc.CreateNode('notSuppMsg', ntAttribute);
    lAttr.Text := pMsg;
    fXmlNode.AttributeNodes.Add(lAttr);
    lAttr := fXmlDoc.CreateNode('msgAt', ntAttribute);
    lAttr.Text := IntToStr(pX) + ',' + IntToStr(pY);
    fXmlNode.AttributeNodes.Add(lAttr);
  end;
end;

function TD2XProcessor.ProcessDirectory(pDir, pWildCards: String): Boolean;
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

function TD2XProcessor.ProcessFile(pFilename: String): Boolean;
var
  lSS: TStringStream;
  lMS: TMemoryStream;
  lFile: string;
  lPhase: String;
begin
  Result := False;
  lFile := pFilename;
  if fOpts.UseBase then
    lFile := fOpts.BaseDirectory + lFile;
  if FileExists(lFile) then
    try
      lPhase := 'Initial';
      Writeln('Processing ', pFilename, ' ...');
      SetProxy;

      lMS := nil;
      lSS := TStringStream.Create;
      try
        lPhase := 'Loading';
        lSS.LoadFromFile(lFile);

        lFile := lSS.DataString;
        lPhase := 'Converting ' + IntToStr(Length(lFile)) + ' x ' +
          IntToStr(Sizeof(Char));
        lMS := TMemoryStream.Create;
        lMS.Write(PChar(lFile)^, Length(lFile) * Sizeof(Char));

        lPhase := 'Parsing';
        fParser.Run(pFilename, lMS);

        if fOpts.Xml then
        begin
          lPhase := 'Preparing Xml';
          if fOpts.XmlDirectory > '' then
          begin
            lFile := ExtractFilePath(ParamStr(0)) + fOpts.XmlDirectory +
              ExtractFilePath(pFilename);
            ForceDirectories(lFile);
            lFile := fOpts.XmlDirectory + pFilename;
          end
          else
            lFile := pFilename;

          lPhase := 'Writing Xml';
          lFile := lFile + '.xml';
          fXmlDoc.Xml.SaveToFile(lFile);
        end;
        Result := True;
      finally
        FreeAndNil(lMS);
        FreeAndNil(lSS);
      end;
    except
      on E: Exception do
      begin
        Writeln('EXCEPTION (', E.ClassName, ') processing "', pFilename,
          '" at ', lPhase, ' : ', E.Message);
        Result := False;
      end;
    end;
end;

procedure TD2XProcessor.LexerOnInclude(pLex: TD2XLexer);
var
  lFile: String;
begin
  lFile := pLex.Token;
  if StartsText('{$include ', lFile) then
    Delete(lFile, 1, 10);
  if StartsText('{$i ', lFile) then
    Delete(lFile, 1, 4);
  if EndsText('}', lFile) then
    SetLength(lFile, Length(lFile) - 1);

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

function TD2XProcessor.ProcessParam(pStr: String): Boolean;
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
      Writeln('EXCEPTION (', E.ClassName, ') processing "', pStr, '" : ',
        E.Message);
  end;
end;

procedure TD2XProcessor.XmlAddAttribute(pName, pValue: String);
var
  lAttr: IXMLNode;
begin
  lAttr := fXmlDoc.CreateNode(pName, ntAttribute);
  lAttr.Text := pValue;
  fXmlNode.AttributeNodes.Add(lAttr);
  fParser.LastTokens := '';
end;

function TD2XProcessor.RecurseDirectory(pDir, pWildCards: String): Boolean;
var
  lFF: TSearchRec;
  lPath: string;
  lFile: String;
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
      procedure(pInst: TObject; pMethod: TRttiMethod;
      const pArgs: TArray<TValue>; out pDoInvoke: Boolean; out pResult: TValue)
    begin
      pDoInvoke := True;
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
      procedure(pInst: TObject; pMethod: TRttiMethod;
      const pArgs: TArray<TValue>; var pResult: TValue)
    begin
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

function TD2XProcessor.SkipAfter(pMethod: String): Boolean;
begin
  Result := fSkippedMethods.ContainsKey(pMethod);
end;

function TD2XProcessor.SkipBefore(pMethod: String): Boolean;
var
  lVal: Integer;
begin
  Result := fSkippedMethods.TryGetValue(pMethod, lVal);
  if Result then
    fSkippedMethods[pMethod] := lVal + 1;
end;

procedure TD2XProcessor.XmlNodeEnd(_pMethod: String);
begin
  if Assigned(fXmlNode) then
  begin
    if Length(fParser.LastTokens) > 1 then
      XmlAddAttribute('lastToken', fParser.LastTokens);

    fXmlNode := fXmlNode.ParentNode;
  end;
end;

procedure TD2XProcessor.XmlNodeStart(pMethod: String);
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

constructor TD2XOptions.Create;
begin
  inherited;

  fVerbose := False;
  fUseBase := False;
  fBaseDirectory := '';
  fXml := True;
  fXmlDirectory := '';
  fCountChildren := True;
  fCountExtension := '.cnt';
  fSkipMethods := True;
  fSkipExtension := '.skip';
end;

function TD2XOptions.ParseOption(pOpt: String): Boolean;
  function ErrorUnlessSet(out pFlag: Boolean): Boolean;
  begin
    Result := False;
    if (Length(pOpt) = 2) or (pOpt[3] = '+') then
      pFlag := True
    else if pOpt[3] = '-' then
      pFlag := False
    else
      Result := True;
  end;
  function ErrorUnlessSetValue(out pFlag: Boolean; out pVal: String): Boolean;
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
  function ErrorUnlessSetExtension(out pFlag: Boolean; out pExtn: String;
    pDflt: String): Boolean;
  begin
    Result := False;
    if ErrorUnlessSetValue(pFlag, pExtn) then
      Result := True
    else if pExtn = '' then
      pExtn := pDflt
    else if pExtn[1] <> '.' then
      pExtn := '.' + pExtn;
  end;
  function ErrorUnlessSetDir(out pFlag: Boolean; out pDir: String): Boolean;
  begin
    Result := False;
    if ErrorUnlessSetValue(pFlag, pDir) then
      Result := True
    else if pDir > '' then
      pDir := IncludeTrailingPathDelimiter(pDir);
  end;

begin
  Result := False;
  if (Length(pOpt) < 2) or not CharInSet(pOpt[1], ['-', '/']) then
    Writeln('Invalid option: ' + pOpt)
  else
    case pOpt[2] of
      '!':
        Result := ReportOptions;
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
      'C', 'c':
        if ErrorUnlessSetExtension(fCountChildren, fCountExtension, '.cnt') then
          Writeln('Invalid Count Children option: ' + pOpt)
        else
          Result := True;
      'S', 's':
        if ErrorUnlessSetExtension(fSkipMethods, fSkipExtension, '.skip') then
          Writeln('Invalid Skip Methods option: ' + pOpt)
        else
          Result := True;
    else
      Writeln('Unknown option: ' + pOpt);
    end;
end;

function TD2XOptions.ReportOptions: Boolean;
  function ShowEnabled(pOpt: Boolean; pLabel, pVal: String): string;
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

begin
  Result := True;
  Writeln('Current option settings:');
  Writeln('  Verbose             ', ShowEnabled(fVerbose, '', ''));
  Writeln('  Recurse             ', ShowEnabled(fRecurse, '', ''));
  Writeln('  Directory base      ', ShowEnabled(fUseBase, 'Dir  ',
    fBaseDirectory));
  Writeln('  Xml output          ', ShowEnabled(fXml, 'Dir  ', fXmlDirectory));
  Writeln('  Count max children  ', ShowEnabled(fCountChildren, 'Extn ',
    fCountExtension));
  Writeln('  Skip methods        ', ShowEnabled(fSkipMethods, 'Extn ',
    fSkipExtension));
end;

procedure TD2XOptions.ShowOptions;
var
  lBase: String;
begin
  lBase := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  Writeln('Usage: ', lBase, ' [ Option | Filename | Wildcard ] ... ');
  Writeln('  Options:        Default   Description');
  Writeln('    V[+-]         -         Log all Parser methods called');
  Writeln('    R[+-]         +         Recurse into subdirectories');
  Writeln('    B[+-]|:<dir>  -         Use <dir> a base for all file lookups');
  Writeln('    X[+-]|:<dir>  +         Generate XML files into current or given <dir>');
  Writeln('    C[+-]|:<ext>  +:cnt     Count max Children into ', lBase,
    '.<ext>');
  Writeln('    S[+-]|:<ext>  +:skip    Skip Parse methods listed in ', lBase,
    '.<ext>');
end;

end.
