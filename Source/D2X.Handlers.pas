unit D2X.Handlers;

interface

uses
  CastaliaPasLex,
  CastaliaPasLexTypes,
  D2X.Global,
  D2X.Parser,
  D2X.Handler,
  D2X.IO,
  D2X.Tree,
  System.Classes,
  System.Generics.Collections;

type
  TD2XParserHandler = class(TD2XLogger, ID2XParser)
  strict private
    fPrevParserMessage: TMessageEvent;
    fPrevLexerInclude: TDirectiveEvent;

  protected
    fParser: TD2XDefinesParser;
    fActive: TD2XFlagRef;

    procedure SetupParserMessage;
    procedure SetupLexerInclude;

    procedure DoParserMessage(pSender: TObject; const pTyp: TMessageEventType;
      const pMsg: string; pX, pY: Integer);
    procedure DoLexerInclude(pSender: TmwBasePasLex);

    procedure OnParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer); virtual;
    procedure OnLexerInclude(const pFile: string; pX, pY: Integer); virtual;

  public
    procedure InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef); virtual;

  end;

  TMethodCount = record
    Method: string;
    Children: Integer;
  end;

  TD2XCountChildrenHandler = class(TD2XParserHandler, ID2XFullProxy,
    ID2XProcessing, ID2XFiles, ID2XMethods)
  private
    fCurrent: TMethodCount;
    fStack: TStack<TMethodCount>;

    fMaxChildren: TStrIntDict;
    fMinChildren: TStrIntDict;

    function MinMaxPairLog(pPair: TStrIntPair): string;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Description: string;

    procedure EndProcessing(pOutput: TStreamWriterRef);

    procedure BeginFile(pFile: string; pInput: TStreamReaderRef);
    procedure EndFile(pFile: string; pOutput: TStreamWriterRef);

    procedure BeginMethod(pMethod: string);
    procedure EndMethod(pMethod: string);
  end;

  TD2XCountFinalDefinesHandler = class(TD2XParserHandler, ID2XFullProxy,
    ID2XProcessing, ID2XFiles)
  private
    fDefines: TStrIntDict;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Description: string;

    procedure EndProcessing(pOutput: TStreamWriterRef);

    procedure BeginFile(pFile: string; pInput: TStreamReaderRef);
    procedure EndFile(pFile: string; pOutput: TStreamWriterRef);
  end;

  TD2XCountDefinesUsedHandler = class(TD2XParserHandler, ID2XProcessing)
  private
    fDefinesDict: TStrIntDict;

  protected
    procedure OnIf(pLex: TD2XLexer);
    procedure OnIfDef(pLex: TD2XLexer);

    procedure DefineUsed(pDef: string);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef); override;

    procedure EndProcessing(pOutput: TStreamWriterRef);

  end;

  TD2XErrorHandler = class(TD2XParserHandler)
  public
    constructor Create; override;
    constructor CreateError(pTyp: TMessageEventType; pLogMessage: TD2XLogMessage);

    procedure InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef); override;

  protected
    procedure OnParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer); override;

  private
    fTyp: TMessageEventType;
    fLogMessage: TD2XLogMessage;

  end;

  TD2XLogHandler = class(TD2XParserHandler, ID2XMethods)
  public
    procedure InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef); override;

    procedure BeginMethod(pMethod: string);
    procedure EndMethod(pMethod: string);

  protected
    procedure OnParserMessage(const pTyp: TMessageEventType;
      const pMsg: string; pX, pY: Integer); override;
    procedure OnLexerInclude(const pFile: string; pX, pY: Integer); override;

  end;

  TD2XParserDefinesHandler = class(TD2XParserHandler, ID2XFiles)
  private
    fDefines: TStringList;
    fInitOnce: Boolean;

    procedure InitDefines;

  public
    constructor CreateDefines(pDefines: TStringList);

    procedure InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef); override;

    function Description: string;

    procedure BeginFile(pFile: string; pInput: TStreamReaderRef);
    procedure EndFile(pFile: string; pOutput: TStreamWriterRef);
  end;

  TD2XHeldDefinesHandler = class(TD2XParserHandler, ID2XFiles)
  private
    fDefines: TStringList;

  public
    constructor CreateDefines(pDefines: TStringList);

    function Description: string;

    procedure BeginFile(pFile: string; pInput: TStreamReaderRef);
    procedure EndFile(pFile: string; pOutput: TStreamWriterRef);
  end;

  TD2XSkipHandler = class(TD2XLogger, ID2XProcessing, ID2XFiles, ID2XChecks)
  private
    fSkippedMethods: TStrIntDict;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Description: string;

    function CheckBeforeMethod(pMethod: string): Boolean;
    function CheckAfterMethod(pMethod: string): Boolean;

    procedure BeginFile(pFile: string; pInput: TStreamReaderRef);
    procedure EndFile(pFile: string; pOutput: TStreamWriterRef);

    procedure EndProcessing(pOutput: TStreamWriterRef);
  end;

  TD2XWriteDefinesHandler = class(TD2XParserHandler, ID2XResults)
  public
    procedure BeginResults;
    procedure EndResults(pOutput: TStreamWriterRef);
  end;

  TD2XTreeHandler = class(TD2XParserHandler, ID2XFullProxy, ID2XResults,
    ID2XFiles, ID2XMethods, ID2XTrees)
  private
    fHasFiles: Boolean;

  protected
    fTreeDoc: TD2XTreeDoc;
    fTreeNode: TD2XTreeNode;
    fWriter: TD2XTreeWriterClass;

    fFinalToken: TD2XFlagRef;
    fParseMode: TD2XStringRef;

    procedure OnParserMessage(const pTyp: TMessageEventType;
      const pMsg: string; pX, pY: Integer); override;
    procedure OnLexerInclude(const pFile: string; pX, pY: Integer); override;

  public
    constructor Create; override;
    constructor CreateTree(pWriter: TD2XTreeWriterClass; pFinalToken: TD2XFlagRef;
      pParseMode: TD2XStringRef);
    destructor Destroy; override;

    function Description: string;

    procedure InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef); override;

    procedure BeginResults;
    procedure EndResults(pOutput: TStreamWriterRef);

    procedure BeginFile(pFile: string; pInput: TStreamReaderRef);
    procedure EndFile(pFile: string; pOutput: TStreamWriterRef);

    procedure BeginMethod(pMethod: string);
    procedure EndMethod(pMethod: string);

    procedure AddAttr(pName: string; pValue: string);
    procedure AddText(pText: string);
    procedure TrimChildren(pElement: string);
    procedure RollbackTo(pNodeName: string);

    property HasFiles: Boolean read fHasFiles;
  end;

implementation

uses
  System.SysUtils;

function PairToStr(pPair: TStrIntPair): string;
begin
  Result := IntToStr(pPair.Value);
end;

{ TD2XParserHandler }

procedure TD2XParserHandler.DoLexerInclude(pSender: TmwBasePasLex);
begin
  if fActive() then
    if Assigned(pSender) then
      OnLexerInclude(pSender.DirectiveParam, pSender.PosXY.X, pSender.PosXY.Y)
    else
      OnLexerInclude('Test', 0, 0);

  if Assigned(fPrevLexerInclude) and Assigned(pSender) then
    fPrevLexerInclude(pSender);
end;

procedure TD2XParserHandler.DoParserMessage(pSender: TObject; const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  if fActive() then
    OnParserMessage(pTyp, pMsg, pX, pY);

  if Assigned(fPrevParserMessage) then
    fPrevParserMessage(pSender, pTyp, pMsg, pX, pY);
end;

procedure TD2XParserHandler.InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef);
begin
  fParser := pParser;
  fActive := pActive;

  fPrevParserMessage := fParser.OnMessage;
  fPrevLexerInclude := fParser.Lexer.OnIncludeDirect;
end;

procedure TD2XParserHandler.OnLexerInclude(const pFile: string; pX, pY: Integer);
begin

end;

procedure TD2XParserHandler.OnParserMessage(const pTyp: TMessageEventType; const pMsg: string;
  pX, pY: Integer);
begin

end;

procedure TD2XParserHandler.SetupLexerInclude;
begin
  fPrevLexerInclude := fParser.Lexer.OnIncludeDirect;
  fParser.Lexer.OnIncludeDirect := DoLexerInclude;
end;

procedure TD2XParserHandler.SetupParserMessage;
begin
  fPrevParserMessage := fParser.OnMessage;
  fParser.OnMessage := DoParserMessage;
end;

{ TD2XCountChildrenHandler }

procedure TD2XCountChildrenHandler.BeginFile(pFile: string; pInput: TStreamReaderRef);
begin
  fCurrent.Method := '';
  fCurrent.Children := 0;
  fStack := TStack<TMethodCount>.Create;
  pInput;
end;

constructor TD2XCountChildrenHandler.Create;
begin
  inherited;

  fStack := nil;
  fMaxChildren := TStrIntDict.Create;
  fMinChildren := TStrIntDict.Create;
end;

function TD2XCountChildrenHandler.Description: string;
begin
  Result := 'Count Children';
end;

destructor TD2XCountChildrenHandler.Destroy;
begin
  FreeAndNil(fMinChildren);
  FreeAndNil(fMaxChildren);
  FreeAndNil(fStack);

  inherited;
end;

procedure TD2XCountChildrenHandler.BeginMethod(pMethod: string);
begin
  if Assigned(fStack) then
  begin
    Inc(fCurrent.Children);
    fStack.Push(fCurrent);
    fCurrent.Method := pMethod;
    fCurrent.Children := 0;
  end;
end;

procedure TD2XCountChildrenHandler.EndMethod(pMethod: string);
var
  lVal: Integer;
begin
  if Assigned(fStack) then
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
end;

procedure TD2XCountChildrenHandler.EndFile(pFile: string; pOutput: TStreamWriterRef);
begin
  FreeAndNil(fStack);
  pOutput;
end;

procedure TD2XCountChildrenHandler.EndProcessing(pOutput: TStreamWriterRef);
begin
  OutputStrIntDict(fMaxChildren, pOutput, MinMaxPairLog);
end;

function TD2XCountChildrenHandler.MinMaxPairLog(pPair: TStrIntPair): string;
var
  lMin: Integer;
begin
  if fMinChildren.TryGetValue(pPair.Key, lMin) then
    Result := IntToStr(lMin) + ',' + IntToStr(pPair.Value)
  else
    Result := '0,' + IntToStr(pPair.Value);
end;

{ TD2XSkipHandler }

procedure TD2XSkipHandler.BeginFile(pFile: string; pInput: TStreamReaderRef);
var
  i: Integer;
  lS: TStreamReader;
  lSL: TStringList;
begin
  lSL := TStringList.Create;
  try
    lS := pInput;
    if Assigned(lS) then
      lSL.LoadFromStream(lS.BaseStream);
    fSkippedMethods.Clear;
    for i := 0 to lSL.Count - 1 do
      if lSL.Names[i] = '' then
        fSkippedMethods.Add(lSL[i], 0)
      else
        fSkippedMethods.Add(lSL.Names[i], 0);
  finally
    lSL.Free;
  end
end;

function TD2XSkipHandler.CheckAfterMethod(pMethod: string): Boolean;
begin
  Result := fSkippedMethods.ContainsKey(pMethod);
end;

function TD2XSkipHandler.CheckBeforeMethod(pMethod: string): Boolean;
var
  lVal: Integer;
begin
  Result := fSkippedMethods.TryGetValue(pMethod, lVal);
  if Result then
    fSkippedMethods[pMethod] := lVal + 1;
end;

constructor TD2XSkipHandler.Create;
begin
  inherited;

  fSkippedMethods := TStrIntDict.Create;
end;

function TD2XSkipHandler.Description: string;
begin
  Result := 'Skip Methods';
end;

destructor TD2XSkipHandler.Destroy;
begin
  FreeAndNil(fSkippedMethods);

  inherited;
end;

procedure TD2XSkipHandler.EndFile(pFile: string; pOutput: TStreamWriterRef);
begin
  // Empty
end;

procedure TD2XSkipHandler.EndProcessing(pOutput: TStreamWriterRef);
begin
  OutputStrIntDict(fSkippedMethods, pOutput, PairToStr);
end;

{ TD2XTreeHandler }

procedure TD2XTreeHandler.AddAttr(pName, pValue: string);
var
  lAttr: TD2XTreeNode;
begin
  if Assigned(fTreeNode) then
  begin
    lAttr := fTreeNode.AddAttribute(pName);
    if pValue = '' then
    begin
      lAttr.Text := fParser.LastTokens;
      fParser.LastTokens := '';
    end
    else
      lAttr.Text := pValue;
  end;
end;

procedure TD2XTreeHandler.AddText(pText: string);
var
  lText: TD2XTreeNode;
begin
  if Assigned(fTreeNode) then
    if fTreeNode.HasChildren then
    begin
      lText := fTreeNode.AddChild('');
      if pText = '' then
      begin
        lText.Text := fParser.LastTokens;
        fParser.LastTokens := '';
      end
      else
        lText.Text := pText;
    end
    else
      fTreeNode.Text := fTreeNode.Text + pText;
end;

procedure TD2XTreeHandler.BeginFile(pFile: string; pInput: TStreamReaderRef);
begin
  fHasFiles := True;
end;

procedure TD2XTreeHandler.BeginMethod(pMethod: string);
begin
  if Assigned(fTreeDoc) then
  begin
    if Assigned(fTreeNode) then
      fTreeNode := fTreeNode.AddChild(pMethod)
    else
    begin
      fTreeNode := fTreeDoc.AddChild(pMethod);
      if Assigned(fParseMode) then
        AddAttr('parseMode', fParseMode); // TD2X.ToLabel(fOpts.ParseMode));
    end;
    if Assigned(fParser) then
      fParser.LastTokens := '';
  end;
end;

procedure TD2XTreeHandler.BeginResults;
begin
  fTreeDoc := TD2XTreeDoc.CreateDoc(fWriter);
  fTreeDoc.AddOptions([toAutoIndent]);
  fHasFiles := False;
end;

constructor TD2XTreeHandler.Create;
begin
  raise EInvalidHandler.Create('Invalid constructor called');
end;

constructor TD2XTreeHandler.CreateTree(pWriter: TD2XTreeWriterClass; pFinalToken: TD2XFlagRef;
  pParseMode: TD2XStringRef);
begin
  Assert(Assigned(pFinalToken), 'Final Token Flag must be specified');

  inherited Create;

  fTreeDoc := nil;
  fTreeNode := nil;

  fParser := nil;
  fHasFiles := False;

  fWriter := pWriter;
  fFinalToken := pFinalToken;
  fParseMode := pParseMode;
end;

function TD2XTreeHandler.Description: string;
begin
  Result := 'Xml';
end;

destructor TD2XTreeHandler.Destroy;
begin
  fTreeNode := nil;
  FreeAndNil(fTreeDoc);

  inherited;
end;

procedure TD2XTreeHandler.EndFile(pFile: string; pOutput: TStreamWriterRef);
begin

end;

procedure TD2XTreeHandler.EndMethod(pMethod: string);
begin
  if Assigned(fTreeNode) then
  begin
    if Assigned(fFinalToken) and fFinalToken() and Assigned(fParser) and
      not fParser.KeepTokens and (Length(fParser.LastTokens) > 1) then
      AddAttr('lastToken', '');

    fTreeNode.Stream;
    fTreeNode := fTreeNode.ParentNode;
  end;
end;

procedure TD2XTreeHandler.EndResults(pOutput: TStreamWriterRef);
var
  lFile: TStreamWriter;
begin
  if fHasFiles and Assigned(pOutput) then
  begin
    lFile := pOutput;
    if Assigned(lFile) then
      fTreeDoc.Stream.SaveToStream(lFile.BaseStream);
    fHasFiles := False;
  end;

  fTreeNode := nil;
  FreeAndNil(fTreeDoc);
end;

procedure TD2XTreeHandler.InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef);
begin
  inherited;

  SetupParserMessage;
  SetupLexerInclude;
end;

procedure TD2XTreeHandler.OnLexerInclude(const pFile: string; pX, pY: Integer);
begin
  BeginMethod('IncludeFile');
  AddAttr('filename', pFile);
  AddAttr('msgAt', IntToStr(pX) + ',' + IntToStr(pY));
  EndMethod('');
end;

procedure TD2XTreeHandler.OnParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  case pTyp of
    meError:
      BeginMethod('D2X_errorMsg');
    meNotSupported:
      BeginMethod('D2X_notSuppMsg');
  else
    BeginMethod('D2X_unknownMsg');
  end;
  AddText(pMsg);
  AddAttr('msgAt', IntToStr(pX) + ',' + IntToStr(pY));
  EndMethod('');
end;

procedure TD2XTreeHandler.RollbackTo(pNodeName: string);
var
  lCurrNode: TD2XTreeNode;
begin
  lCurrNode := fTreeNode;
  while Assigned(lCurrNode) and (lCurrNode.LocalName <> pNodeName) do
    lCurrNode := lCurrNode.ParentNode;

  if Assigned(lCurrNode) then
    fTreeNode := lCurrNode;
end;

procedure TD2XTreeHandler.TrimChildren(pElement: string);
begin
  if Assigned(fTreeNode) then
    fTreeNode.TrimChildren(pElement);
end;

{ TD2XWriteDefinesHandler }

procedure TD2XWriteDefinesHandler.BeginResults;
begin
  // Empty
end;

procedure TD2XWriteDefinesHandler.EndResults(pOutput: TStreamWriterRef);
var
  lSL: TStringList;
  i: Integer;
  lS: TStreamWriter;
const
  DEF_BREAK = #13#10'****'#13#10#13#10;
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
      //        lFile := fProgramDir + fOpts.DefinesDirectory + ExtractFilePath(pFilename);
      //        ForceDirectories(lFile);
      //        lFS := TFileStream.Create(fOpts.DefinesDirectory + pFilename + '.def', fmCreate);
      lS := pOutput;
      if Assigned(lS) then
      begin
        fParser.StartDefines.SaveToStream(lS.BaseStream);
        lS.Write(DEF_BREAK);
        lS.Flush;
        lSL.SaveToStream(lS.BaseStream);
      end;
    end;
  finally
    FreeAndNil(lSL);
  end;
end;

{ TD2XDefinesUsedHandler }

constructor TD2XCountDefinesUsedHandler.Create;
begin
  inherited;

  fDefinesDict := TStrIntDict.Create;
end;

procedure TD2XCountDefinesUsedHandler.DefineUsed(pDef: string);
var
  lVal: Integer;
begin
  pDef := Trim(pDef);
  if fDefinesDict.TryGetValue(pDef, lVal) then
    fDefinesDict[pDef] := lVal + 1
  else
    fDefinesDict.Add(pDef, 1)
end;

destructor TD2XCountDefinesUsedHandler.Destroy;
begin
  FreeAndNil(fDefinesDict);

  inherited;
end;

procedure TD2XCountDefinesUsedHandler.EndProcessing(pOutput: TStreamWriterRef);
begin
  OutputStrIntDict(fDefinesDict, pOutput, PairToStr);
end;

procedure TD2XCountDefinesUsedHandler.InitParser(pParser: TD2XDefinesParser;
  pActive: TD2XFlagRef);
begin
  inherited;

  pParser.Lexer.OnIfDirect := OnIf;
  pParser.Lexer.OnIfDefDirect := OnIfDef;
  pParser.Lexer.OnIfNDefDirect := OnIfDef;
  pParser.Lexer.OnIfOptDirect := OnIf;
  pParser.Lexer.OnElseIfDirect := OnIf;
end;

procedure TD2XCountDefinesUsedHandler.OnIf(pLex: TD2XLexer);
begin
  pLex.Next;
end;

procedure TD2XCountDefinesUsedHandler.OnIfDef(pLex: TD2XLexer);
begin
  DefineUsed(pLex.DirectiveParam);
  pLex.Next;
end;

{ TD2XParserDefinesHandler }

procedure TD2XParserDefinesHandler.BeginFile(pFile: string;
  pInput: TStreamReaderRef);
begin
  fParser.StartDefines.Assign(fDefines);
end;

constructor TD2XParserDefinesHandler.CreateDefines(pDefines: TStringList);
begin
  Create;

  fDefines := pDefines;
  fInitOnce := True;
end;

function TD2XParserDefinesHandler.Description: string;
begin
  Result := 'Parser Defines';
end;

procedure TD2XParserDefinesHandler.EndFile(pFile: string; pOutput: TStreamWriterRef);
begin
  // Empty
end;

procedure TD2XParserDefinesHandler.InitDefines;
begin
  fParser.Lexer.InitDefines;
  if Assigned(fDefines) and fInitOnce then
  begin
    fParser.Lexer.GetDefines(fDefines);
    fInitOnce := False;
  end;
end;

procedure TD2XParserDefinesHandler.InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef);
begin
  inherited;

  InitDefines;
end;

{ TD2XErrorHandler }

constructor TD2XErrorHandler.Create;
begin
  raise EInvalidHandler.Create('Invalid constructor called');
end;

constructor TD2XErrorHandler.CreateError(pTyp: TMessageEventType;
  pLogMessage: TD2XLogMessage);
begin
  inherited Create;

  fTyp := pTyp;
  fLogMessage := pLogMessage;
end;

procedure TD2XErrorHandler.InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef);
begin
  inherited;

  SetupParserMessage;
end;

procedure TD2XErrorHandler.OnParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  if pTyp = fTyp then
    case pTyp of
      meError:
        fLogMessage('Error', pMsg, pX, pY);
      meNotSupported:
        fLogMessage('Not Supported', pMsg, pX, pY);
    else
      fLogMessage('????', pMsg, pX, pY);
    end;
end;

{ TD2XCountDefinesHandler }

procedure TD2XCountFinalDefinesHandler.BeginFile(pFile: string; pInput: TStreamReaderRef);
begin
  // Empty
end;

constructor TD2XCountFinalDefinesHandler.Create;
begin
  inherited;

  fDefines := TStrIntDict.Create;
end;

function TD2XCountFinalDefinesHandler.Description: string;
begin
  Result := 'Count Defines';
end;

destructor TD2XCountFinalDefinesHandler.Destroy;
begin
  FreeAndNil(fDefines);

  inherited;
end;

procedure TD2XCountFinalDefinesHandler.EndFile(pFile: string; pOutput: TStreamWriterRef);
var
  lSL: TStringList;
  lS, lT: string;
  lVal: Integer;
begin
  lSL := TStringList.Create;
  try
    fParser.GetLexerDefines(lSL);
    for lS in lSL do
    begin
      lT := Trim(lS);
      if fParser.StartDefines.IndexOf(lT) < 0 then
        if fDefines.TryGetValue(lT, lVal) then
          fDefines.AddOrSetValue(lT, lVal + 1)
        else
          fDefines.AddOrSetValue(lT, 1);
    end;
  finally
    lSL.Free;
  end;
  pOutput;
end;

procedure TD2XCountFinalDefinesHandler.EndProcessing(pOutput: TStreamWriterRef);
begin
  OutputStrIntDict(fDefines, pOutput, PairToStr);
end;

{ TD2XHeldDefinesHandler }

procedure TD2XHeldDefinesHandler.BeginFile(pFile: string; pInput: TStreamReaderRef);
begin
  fParser.HeldDefines.Assign(fDefines);
end;

constructor TD2XHeldDefinesHandler.CreateDefines(pDefines: TStringList);
begin
  Create;

  fDefines := pDefines;
end;

function TD2XHeldDefinesHandler.Description: string;
begin
  Result := 'Held Defines';
end;

procedure TD2XHeldDefinesHandler.EndFile(pFile: string; pOutput: TStreamWriterRef);
begin
  // Empty
end;

{ TD2XLogHandler }

procedure TD2XLogHandler.BeginMethod(pMethod: string);
begin
  if Assigned(fParser) and Assigned(fParser.Lexer) then
    Log('BEFORE %s @ %s', [pMethod, fParser.Lexer.Token])
  else
    Log('BEFORE %s ', [pMethod]);
end;

procedure TD2XLogHandler.EndMethod(pMethod: string);
begin
  Log('AFTER  %s', [pMethod]);
end;

procedure TD2XLogHandler.InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef);
begin
  inherited;

  SetupParserMessage;
  SetupLexerInclude;
end;

procedure TD2XLogHandler.OnLexerInclude(const pFile: string; pX, pY: Integer);
begin
  Log('INCLUDE @ %d,%d: %s', [pX, pY, pFile]);
end;

procedure TD2XLogHandler.OnParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  case pTyp of
    meError:
      Log('ERROR @ %d,%d: %s', [pX, pY, pMsg]);
    meNotSupported:
      Log('NOT SUPPORTED @ %d,%d: %s', [pX, pY, pMsg]);
  else
    Log('???? @ %d,%d: %s', [pX, pY, pMsg]);
  end;
end;

end.
