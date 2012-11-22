unit D2X.Handlers;

interface

uses
  CastaliaPasLexTypes,
  D2X,
  D2X.Param,
  D2X.Parser,
  D2X.Handler,
  D2X.Stream,
  D2X.Xml,
  System.Classes,
  System.Generics.Collections;

type
  TMethodCount = record
    Method: string;
    Children: Integer;
  end;

  TD2XCountHandler = class(TD2XHandler)
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

  TD2XDefinesUsedHandler = class(TD2XHandler)
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

    procedure InitDefines;

  public
    constructor CreateDefines(pDefines: TStringList);

    procedure InitParser(pParser: TD2XDefinesParser); override;

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure Copy(pFrom: TD2XHandler); override;

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

  TD2XXmlHandler = class(TD2XParserHandler)
  private
    fHasFiles: Boolean;

  protected
    fXmlDoc: TD2XmlDoc;
    fXmlNode: TD2XmlNode;

    fFinalToken: TD2XCheckRef;
    fParseMode: TD2XStringRef;

  public
    constructor Create; override;
    constructor CreateXml(pFinalToken: TD2XCheckRef; pParseMode: TD2XStringRef);
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

    procedure RollbackTo(pNodeName: string);

    property HasFiles: Boolean read fHasFiles write fHasFiles;
  end;

implementation

uses
  D2X.Options,
  System.IOUtils,
  System.SysUtils,
  Xml.XMLIntf;

{ TD2XCountHandler }

procedure TD2XCountHandler.BeginFile(pFile: string; pInput: TStreamReaderRef);
begin
  fCurrent.Method := '';
  fCurrent.Children := 0;
  fStack := TStack<TMethodCount>.Create;
  pInput;
end;

constructor TD2XCountHandler.Create;
begin
  inherited;

  fStack := nil;
  fMaxChildren := TStrIntDict.Create;
  fMinChildren := TStrIntDict.Create;
end;

function TD2XCountHandler.Description: string;
begin
  Result := 'Count Children';
end;

destructor TD2XCountHandler.Destroy;
begin
  FreeAndNil(fMinChildren);
  FreeAndNil(fMaxChildren);
  FreeAndNil(fStack);

  inherited;
end;

procedure TD2XCountHandler.BeginMethod(pMethod: string);
begin
  Assert(Assigned(fStack), 'Begin Method called out of order');

  Inc(fCurrent.Children);
  fStack.Push(fCurrent);
  fCurrent.Method := pMethod;
  fCurrent.Children := 0;
end;

procedure TD2XCountHandler.EndMethod(pMethod: string);
var
  lVal: Integer;
begin
  Assert(Assigned(fStack), 'End Method called out of order');

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

procedure TD2XCountHandler.EndFile(pFile: string; pOutput: TStreamWriterRef);
begin
  FreeAndNil(fStack);
  pOutput;
end;

procedure TD2XCountHandler.EndProcessing(pOutput: TStreamWriterRef);
begin
  OutputStrIntDict(fMaxChildren, pOutput, MinMaxPairLog);
end;

function TD2XCountHandler.MinMaxPairLog(pPair: TStrIntPair): string;
var
  lMin: Integer;
begin
  if fMinChildren.TryGetValue(pPair.Key, lMin) then
    Result := IntToStr(lMin) + ',' + IntToStr(pPair.Value)
  else
    Result := '0,' + IntToStr(pPair.Value);
end;

function TD2XCountHandler.UseProxy: Boolean;
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
  OutputStrIntDict(fSkippedMethods, pOutput,
      function(pPair: TStrIntPair): string
    begin
      Result := IntToStr(pPair.Value);
    end);
end;

function TD2XSkipHandler.UseProxy: Boolean;
begin
  Result := False;
end;

{ TD2XXmlHandler }

procedure TD2XXmlHandler.AddAttr(pName, pValue: string);
var
  lAttr: TD2XmlNode;
begin
  if Assigned(fXmlNode) then
  begin
    lAttr := fXmlNode.AddAttribute(pName);
    if pValue = '' then
    begin
      lAttr.Text := fParser.LastTokens;
      fParser.LastTokens := '';
    end
    else
      lAttr.Text := pValue;
  end;
end;

procedure TD2XXmlHandler.AddText(pText: string);
var
  lText: TD2XmlNode;
begin
  if Assigned(fXmlNode) then
    if fXmlNode.HasChildNodes then
    begin
      lText := fXmlNode.AddChild('');
      if pText = '' then
      begin
        lText.Text := fParser.LastTokens;
        fParser.LastTokens := '';
      end
      else
        lText.Text := pText;
    end
    else
      fXmlNode.Text := fXmlNode.Text + pText;
end;

procedure TD2XXmlHandler.BeginMethod(pMethod: string);
begin
  if Assigned(fXmlDoc) then
  begin
    if Assigned(fXmlNode) then
      fXmlNode := fXmlNode.AddChild(pMethod)
    else
    begin
      fXmlNode := fXmlDoc.AddChild(pMethod);
      if Assigned(fParseMode) then
        AddAttr('parseMode', fParseMode); // TD2X.ToLabel(fOpts.ParseMode));
    end;
    if Assigned(fParser) then
      fParser.LastTokens := '';
  end;
end;

procedure TD2XXmlHandler.BeginResults;
begin
  fXmlDoc := NewXmlDocument;
  fXmlDoc.Options := fXmlDoc.Options + [doNodeAutoIndent];
  fHasFiles := False;
end;

procedure TD2XXmlHandler.Copy(pFrom: TD2XHandler);
var
  lFrom: TD2XXmlHandler;
begin
  if Assigned(pFrom) then
  begin
    inherited;

    lFrom := TD2XXmlHandler(pFrom);
    fFinalToken := lFrom.fFinalToken;
    fParseMode := lFrom.fParseMode;
  end;
end;

constructor TD2XXmlHandler.Create;
begin
  raise EInvalidHandler.Create('Invalid constructor called');
end;

constructor TD2XXmlHandler.CreateXml(pFinalToken: TD2XCheckRef; pParseMode: TD2XStringRef);
begin
  inherited Create;

  fXmlDoc := nil;
  fXmlNode := nil;

  fParser := nil;
  fHasFiles := False;

  fFinalToken := pFinalToken;
  fParseMode := pParseMode;
end;

function TD2XXmlHandler.Description: string;
begin
  Result := 'Xml';
end;

destructor TD2XXmlHandler.Destroy;
begin
  fXmlNode := nil;
  FreeAndNil(fXmlDoc);

  inherited;
end;

procedure TD2XXmlHandler.EndMethod(pMethod: string);
begin
  if Assigned(fXmlNode) then
  begin
    if Assigned(fFinalToken) and Assigned(fParser) and fFinalToken and
      (Length(fParser.LastTokens) > 1) then
      AddAttr('lastToken');

    fXmlNode.Xml;
    fXmlNode := fXmlNode.ParentNode;
  end;
end;

procedure TD2XXmlHandler.EndResults(pOutput: TStreamWriterRef);
var
  lFile: TStreamWriter;
begin
  if fHasFiles and Assigned(pOutput) then
  begin
    lFile := pOutput;
    if Assigned(lFile) then
      fXmlDoc.Xml.SaveToStream(lFile.BaseStream);
  end;

  fXmlNode := nil;
  FreeAndNil(fXmlDoc);
end;

procedure TD2XXmlHandler.LexerInclude(const pFile: string; pX, pY: Integer);
begin
  BeginMethod('IncludeFile');
  AddAttr('filename', pFile);
  AddAttr('msgAt', IntToStr(pX) + ',' + IntToStr(pY));
  EndMethod('');
end;

procedure TD2XXmlHandler.ParserMessage(const pTyp: TMessageEventType;
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

procedure TD2XXmlHandler.RollbackTo(pNodeName: string);
var
  lCurrNode: TD2XmlNode;
begin
  lCurrNode := fXmlNode;
  while Assigned(lCurrNode) and (lCurrNode.LocalName <> pNodeName) do
    lCurrNode := lCurrNode.ParentNode;

  if Assigned(lCurrNode) then
    fXmlNode := lCurrNode;
end;

function TD2XXmlHandler.UseProxy: Boolean;
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
  DEF_BREAK= #13#10'****'#13#10#13#10;
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

constructor TD2XDefinesUsedHandler.Create;
begin
  inherited;

  fDefinesDict := TStrIntDict.Create;
end;

procedure TD2XDefinesUsedHandler.DefineUsed(pDef: string);
var
  lVal: Integer;
begin
  if fDefinesDict.TryGetValue(pDef, lVal) then
    fDefinesDict[pDef] := lVal + 1
  else
    fDefinesDict.Add(pDef, 1)
end;

function TD2XDefinesUsedHandler.Description: string;
begin
  Result := 'Defines Used'
end;

destructor TD2XDefinesUsedHandler.Destroy;
begin
  FreeAndNil(fDefinesDict);

  inherited;
end;

procedure TD2XDefinesUsedHandler.EndProcessing(pOutput: TStreamWriterRef);
begin
  OutputStrIntDict(fDefinesDict, pOutput,
    function(pPair: TStrIntPair): string
    begin
      Result := IntToStr(pPair.Value);
    end);
end;

function TD2XDefinesUsedHandler.UseProxy: Boolean;
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
end;

function TD2XParserDefinesHandler.Description: string;
begin
  Result := 'Parser Defines';
end;

procedure TD2XParserDefinesHandler.InitDefines;
begin
  fParser.Lexer.InitDefines;
  if Assigned(fDefines) then
    fParser.Lexer.GetDefines(fDefines);
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

end.
