unit D2X.Handlers;

interface

uses
  CastaliaPasLexTypes,
  D2X.Global,
  D2X.Param,
  D2X.Parser,
  D2X.Handler,
  D2X.IO,
  D2X.Tree,
  D2X.Tree.Json,
  D2X.Tree.Xml,
  System.Classes,
  System.Generics.Collections;

type
  TMethodCount = record
    Method: string;
    Children: Integer;
  end;

  TD2XCountChildrenHandler = class(TD2XHandler)
  private
    fCurrent: TMethodCount;
    fStack: TStack<TMethodCount>;

    fMaxChildren: TStrIntDict;
    fMinChildren: TStrIntDict;

    function MinMaxPairLog(pPair: TStrIntPair): string;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure EndProcessing(pOutput: TStreamWriterRef); override;

    procedure BeginFile(pFile: string; pInput: TStreamReaderRef); override;
    procedure EndFile(pFile: string; pOutput: TStreamWriterRef); override;

    procedure BeginMethod(pMethod: string); override;
    procedure EndMethod(pMethod: string); override;
  end;

  TD2XCountFinalDefinesHandler = class(TD2XParserHandler)
  private
    fDefines: TStrIntDict;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure EndProcessing(pOutput: TStreamWriterRef); override;

    procedure EndFile(pFile: string; pOutput: TStreamWriterRef); override;
  end;

  TD2XCountDefinesUsedHandler = class(TD2XHandler)
  private
    fDefinesDict: TStrIntDict;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure EndProcessing(pOutput: TStreamWriterRef); override;

    procedure DefineUsed(pDef: string);

  end;

  TD2XErrorHandler = class(TD2XParserHandler)
  public
    constructor Create; override;
    constructor CreateError(pTyp: TMessageEventType; pLogMessage: TD2XLogMessage);

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer); override;

  private
    fTyp: TMessageEventType;
    fLogMessage: TD2XLogMessage;

  end;

  TD2XParserDefinesHandler = class(TD2XParserHandler)
  private
    fDefines: TStringList;
    fInitOnce: Boolean;

    procedure InitDefines;

  public
    constructor CreateDefines(pDefines: TStringList);

    procedure InitParser(pParser: TD2XDefinesParser); override;

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure Copy(pFrom: TD2XHandler); override;

    procedure BeginFile(pFile: string; pInput: TStreamReaderRef); override;
  end;

  TD2XHeldDefinesHandler = class(TD2XParserHandler)
  private
    fDefines: TStringList;

  public
    constructor CreateDefines(pDefines: TStringList);

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure BeginFile(pFile: string; pInput: TStreamReaderRef); override;
  end;

  TD2XSkipHandler = class(TD2XHandler)
  private
    fSkippedMethods: TStrIntDict;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Description: string; override;
    function UseProxy: Boolean; override;

    function CheckBeforeMethod(pMethod: string): Boolean; override;
    function CheckAfterMethod(pMethod: string): Boolean; override;

    procedure BeginFile(pFile: string; pInput: TStreamReaderRef); override;

    procedure EndProcessing(pOutput: TStreamWriterRef); override;
  end;

  TD2XWriteDefinesHandler = class(TD2XParserHandler)
  public
    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure EndResults(pOutput: TStreamWriterRef); override;
  end;

  TD2XTreeHandler = class(TD2XParserHandler)
  private
    fHasFiles: Boolean;

  protected
    fTreeDoc: TD2XTreeDoc;
    fTreeNode: TD2XTreeNode;
    fWriter: TD2XTreeWriterClass;

    fFinalToken: ID2XFlag;
    fParseMode: TD2XStringRef;

  public
    constructor Create; override;
    constructor CreateTree(pWriter: TD2XTreeWriterClass; pFinalToken: ID2XFlag;
      pParseMode: TD2XStringRef);
    destructor Destroy; override;

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure Copy(pFrom: TD2XHandler); override;

    procedure BeginResults; override;
    procedure EndResults(pOutput: TStreamWriterRef); override;

    procedure BeginMethod(pMethod: string); override;
    procedure EndMethod(pMethod: string); override;

    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer); override;
    procedure LexerInclude(const pFile: string; pX, pY: Integer); override;

    procedure AddAttr(pName: string; pValue: string = '');
    procedure AddText(pText: string = '');
    procedure TrimChildren(pElement: string);

    procedure RollbackTo(pNodeName: string);

    property HasFiles: Boolean read fHasFiles write fHasFiles;
  end;

implementation

uses
  D2X.Options,
  System.IOUtils,
  System.SysUtils,
  Xml.XMLIntf;

function PairToStr(pPair: TStrIntPair): string;
begin
  Result := IntToStr(pPair.Value);
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

function TD2XCountChildrenHandler.UseProxy: Boolean;
begin
  Result := True;
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

procedure TD2XSkipHandler.EndProcessing(pOutput: TStreamWriterRef);
begin
  OutputStrIntDict(fSkippedMethods, pOutput, PairToStr);
end;

function TD2XSkipHandler.UseProxy: Boolean;
begin
  Result := False;
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
  fTreeDoc.Options := fTreeDoc.Options + [toAutoIndent];
  fHasFiles := False;
end;

procedure TD2XTreeHandler.Copy(pFrom: TD2XHandler);
var
  lFrom: TD2XTreeHandler;
begin
  if Assigned(pFrom) then
  begin
    inherited;

    lFrom := TD2XTreeHandler(pFrom);
    fFinalToken := lFrom.fFinalToken;
    fParseMode := lFrom.fParseMode;
  end;
end;

constructor TD2XTreeHandler.Create;
begin
  raise EInvalidHandler.Create('Invalid constructor called');
end;

constructor TD2XTreeHandler.CreateTree(pWriter: TD2XTreeWriterClass; pFinalToken: ID2XFlag;
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

procedure TD2XTreeHandler.EndMethod(pMethod: string);
begin
  if Assigned(fTreeNode) then
  begin
    if Assigned(fFinalToken) and fFinalToken.Flag and Assigned(fParser) and
      not fParser.KeepTokens and (Length(fParser.LastTokens) > 1) then
      AddAttr('lastToken');

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

procedure TD2XTreeHandler.LexerInclude(const pFile: string; pX, pY: Integer);
begin
  BeginMethod('IncludeFile');
  AddAttr('filename', pFile);
  AddAttr('msgAt', IntToStr(pX) + ',' + IntToStr(pY));
  EndMethod('');
end;

procedure TD2XTreeHandler.ParserMessage(const pTyp: TMessageEventType;
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

function TD2XTreeHandler.UseProxy: Boolean;
begin
  Result := True;
end;

{ TD2XWriteDefinesHandler }

function TD2XWriteDefinesHandler.Description: string;
begin
  Result := 'Write Defines';
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

function TD2XWriteDefinesHandler.UseProxy: Boolean;
begin
  Result := False;
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

function TD2XCountDefinesUsedHandler.Description: string;
begin
  Result := 'Defines Used'
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

function TD2XCountDefinesUsedHandler.UseProxy: Boolean;
begin
  Result := False;
end;

{ TD2XParserDefinesHandler }

procedure TD2XParserDefinesHandler.BeginFile(pFile: string;
  pInput: TStreamReaderRef);
begin
  fParser.StartDefines.Assign(fDefines);
end;

procedure TD2XParserDefinesHandler.Copy(pFrom: TD2XHandler);
begin
  inherited;
  if Assigned(pFrom) then
    InitDefines;
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

procedure TD2XParserDefinesHandler.InitDefines;
begin
  fParser.Lexer.InitDefines;
  if Assigned(fDefines) and fInitOnce then
  begin
    fParser.Lexer.GetDefines(fDefines);
    fInitOnce := False;
  end;
end;

procedure TD2XParserDefinesHandler.InitParser(pParser: TD2XDefinesParser);
begin
  inherited;

  InitDefines;
end;

function TD2XParserDefinesHandler.UseProxy: Boolean;
begin
  Result := False;
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

function TD2XErrorHandler.Description: string;
begin
  Result := 'Error Logging';
end;

procedure TD2XErrorHandler.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  inherited;

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

function TD2XErrorHandler.UseProxy: Boolean;
begin
  Result := False;
end;

{ TD2XCountDefinesHandler }

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

function TD2XCountFinalDefinesHandler.UseProxy: Boolean;
begin
  Result := True;
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

function TD2XHeldDefinesHandler.UseProxy: Boolean;
begin
  Result := False;
end;

end.
