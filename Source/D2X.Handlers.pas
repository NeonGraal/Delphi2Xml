unit D2X.Handlers;

interface

uses
  CastaliaPasLexTypes,
  D2X,
  D2X.Parser,
  D2X.Handler,
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
    constructor Create;
    destructor Destroy; override;

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure EndProcessing(pOutput: TD2XHandler.ThStreamCreator); override;

    procedure BeginFile(pInput: TD2XHandler.ThStreamCreator); override;
    procedure EndFile(pOutput: TD2XHandler.ThStreamCreator); override;

    procedure BeginMethod(pMethod: string); override;
    procedure EndMethod(pMethod: string); override;
  end;

  TD2XDefinesUsedHandler = class(TD2XHandler)
  private
    fDefinesDict: TStrIntDict;

  public
    constructor Create;
    destructor Destroy; override;

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure EndProcessing(pOutput: TD2XHandler.ThStreamCreator); override;

    procedure DefineUsed(pDef: string);

  end;

  TD2XParserHandler = class(TD2XHandler)
  protected
    fParser: TD2XDefinesParser;

  public
    procedure InitParser(pParser: TD2XDefinesParser); virtual;

    procedure Copy(pFrom: TD2XHandler); override;

    property Parser: TD2XDefinesParser read fParser;
  end;

  TD2XParserDefinesHandler = class(TD2XParserHandler)
  private
    fDefines: TStringList;
    fLoadDefines: Boolean;
    fDefinesFileName: TD2XNamedStringRef;

    procedure InitDefines;

  public
    constructor Create;
    destructor Destroy; override;

    procedure SetDefinesFileName(pDefinesFileName: TD2XNamedStringRef);

    procedure InitParser(pParser: TD2XDefinesParser); override;

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure Copy(pFrom: TD2XHandler); override;

    procedure BeginFile(pInput: TD2XHandler.ThStreamCreator); override;

    function ParseDefines(pStr: string): Boolean;
    procedure ClearDefines;

    procedure OutputDefines(pSL: TStringList);
    procedure ReportDefines(pL: ID2XLogger);
  end;

  TD2XSkipHandler = class(TD2XHandler)
  private
    fSkippedMethods: TStrIntDict;

  public
    constructor Create;
    destructor Destroy; override;

    function Description: string; override;
    function UseProxy: Boolean; override;

    function CheckBeforeMethod(pMethod: string): Boolean; override;
    function CheckAfterMethod(pMethod: string): Boolean; override;

    procedure BeginFile(pInput: TD2XHandler.ThStreamCreator); override;

    procedure EndProcessing(pOutput: TD2XHandler.ThStreamCreator); override;
  end;

  TD2XWriteDefinesHandler = class(TD2XParserHandler)
  public
    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure EndResults(pOutput: TD2XHandler.ThStreamCreator); override;
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
    constructor Create;
    destructor Destroy; override;

    procedure Init(pFinalToken: TD2XCheckRef; pParseMode: TD2XStringRef);

    function Description: string; override;
    function UseProxy: Boolean; override;

    procedure Copy(pFrom: TD2XHandler); override;

    procedure BeginResults; override;
    procedure EndResults(pOutput: TD2XHandler.ThStreamCreator); override;

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
  System.SysUtils,
  Xml.XMLIntf;

{ TD2XCountHandler }

procedure TD2XCountHandler.BeginFile(pInput: TD2XHandler.ThStreamCreator);
begin
  fCurrent.Method := '';
  fCurrent.Children := 0;
  fStack := TStack<TMethodCount>.Create;
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

procedure TD2XCountHandler.EndFile(pOutput: TD2XHandler.ThStreamCreator);
begin
  FreeAndNil(fStack);
end;

procedure TD2XCountHandler.EndProcessing(pOutput: TD2XHandler.ThStreamCreator);
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

procedure TD2XSkipHandler.BeginFile(pInput: TD2XHandler.ThStreamCreator);
var
  i: Integer;
  lS: TStream;
begin
  with TStringList.Create do
    try
      lS := pInput;
      if Assigned(lS) then
        LoadFromStream(lS);
      fSkippedMethods.Clear;
      for i := 0 to Count - 1 do
        if Names[i] = '' then
          fSkippedMethods.Add(Strings[i], 0)
        else
          fSkippedMethods.Add(Names[i], 0);
    finally
      Free;
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

procedure TD2XSkipHandler.EndProcessing(pOutput: TD2XHandler.ThStreamCreator);
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
  inherited;

  fXmlDoc := nil;
  fXmlNode := nil;

  fParser := nil;
  fFinalToken := nil;
  fParseMode := nil;
  fHasFiles := False;
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

procedure TD2XXmlHandler.EndResults(pOutput: TD2XHandler.ThStreamCreator);
var
  lFile: TStream;
begin
  if fHasFiles and Assigned(pOutput) then
  begin
    lFile := pOutput;
    if Assigned(lFile) then
      fXmlDoc.Xml.SaveToStream(lFile);
  end;

  fXmlNode := nil;
  FreeAndNil(fXmlDoc);
end;

procedure TD2XXmlHandler.Init(pFinalToken: TD2XCheckRef;
  pParseMode: TD2XStringRef);
begin
  fFinalToken := pFinalToken;
  fParseMode := pParseMode;
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

procedure TD2XWriteDefinesHandler.EndResults(pOutput: TD2XHandler.ThStreamCreator);
var
  lSL: TStringList;
  i: Integer;
  lFS: TStream;
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
      //        lFile := fProgramDir + fOpts.DefinesDirectory + ExtractFilePath(pFilename);
      //        ForceDirectories(lFile);
      //        lFS := TFileStream.Create(fOpts.DefinesDirectory + pFilename + '.def', fmCreate);
      lFS := pOutput;
      if Assigned(lFS) then
      begin
        fParser.StartDefines.SaveToStream(lFS);
        lFS.Write(DEF_BREAK, 10);
        lSL.SaveToStream(lFS);
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

procedure TD2XDefinesUsedHandler.EndProcessing(pOutput: TD2XHandler.ThStreamCreator);
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

procedure TD2XParserDefinesHandler.BeginFile(pInput: TD2XHandler.ThStreamCreator);
begin
  fParser.StartDefines.Assign(fDefines);
end;

procedure TD2XParserDefinesHandler.ClearDefines;
begin
  fLoadDefines := False;
  fDefines.Clear;
end;

procedure TD2XParserDefinesHandler.Copy(pFrom: TD2XHandler);
begin
  inherited;
  if Assigned(pFrom) then
    InitDefines;
end;

constructor TD2XParserDefinesHandler.Create;
begin
  inherited;

  fDefines := TStringList.Create;
  fDefines.Sorted := True;
end;

function TD2XParserDefinesHandler.Description: string;
begin
  Result := 'Parser Defines';
end;

destructor TD2XParserDefinesHandler.Destroy;
begin
  FreeAndNil(fDefines);

  inherited;
end;

procedure TD2XParserDefinesHandler.InitDefines;
begin
  fParser.Lexer.InitDefines;
  fParser.Lexer.GetDefines(fDefines);
end;

procedure TD2XParserDefinesHandler.InitParser(pParser: TD2XDefinesParser);
begin
  inherited;

  InitDefines;
end;

procedure TD2XParserDefinesHandler.OutputDefines(pSL: TStringList);
var
  lS: string;
begin
  if fLoadDefines then
  begin
    pSL.Add('-D:');
    for lS in fDefines do
      pSL.Add('-D+' + lS);
  end;
end;

function TD2XParserDefinesHandler.ParseDefines(pStr: string): Boolean;
var
  lStr: string;
  lIdx: Integer;
begin
  Result := False;
  if (pStr = '!') or (pStr = ':') then
  begin
    Result := True;
    fDefines.Clear;
    fLoadDefines := pStr = ':';
  end
  else
    if Length(pStr) > 1 then
    begin
      lStr := System.Copy(pStr, 2, Length(pStr));
      case pStr[1] of
        '+':
          begin
            Result := True;
            fLoadDefines := True;
            if fDefines.IndexOf(lStr) < 0 then
              fDefines.Add(lStr);
          end;
        '-':
          begin
            Result := True;
            lIdx := fDefines.IndexOf(lStr);
            if lIdx >= 0 then
            begin
              fDefines.Delete(lIdx);
              fLoadDefines := True;
            end;
          end;
        ':':
          begin
            Result := True;
            fLoadDefines := True;
            fDefines.LoadFromFile(fDefinesFileName(MakeFileName(lStr, '.def')));
          end;
      end;
    end;
end;

procedure TD2XParserDefinesHandler.ReportDefines(pL: ID2XLogger);
var
  lS: string;
  w: Integer;

  procedure WriteWidth(pStr: string);
  begin
    pL.Log('%s', [pStr], False);
    Inc(w, Length(pStr));
  end;

begin
  if fLoadDefines then
    if fDefines.Count < 1 then
      pL.Log('Use NO Defines', [])
    else
    begin
      pL.Log('Use these Defines:', []);
      w := 0;
      for lS in fDefines do
      begin
        if w = 0 then
          WriteWidth('    ')
        else
          if (w + Length(lS)) > 78 then
          begin
            pL.Log('', []);
            w := 0;
            WriteWidth('    ');
          end
          else
            WriteWidth(', ');
        WriteWidth(lS);
      end;
      pL.Log('', []);
    end
  else
    pL.Log('Use default Defines', []);
end;

procedure TD2XParserDefinesHandler.SetDefinesFileName(pDefinesFileName: TD2XNamedStringRef);
begin
  fDefinesFileName := pDefinesFileName;
end;

function TD2XParserDefinesHandler.UseProxy: Boolean;
begin
  Result := False;
end;

{ TD2XParserHandler }

procedure TD2XParserHandler.Copy(pFrom: TD2XHandler);
var
  lFrom: TD2XParserHandler;
begin
  if Assigned(pFrom) then
  begin
    lFrom := TD2XParserHandler(pFrom);
    fParser := lFrom.fParser;
  end;
end;

procedure TD2XParserHandler.InitParser(pParser: TD2XDefinesParser);
begin
  fParser := pParser;
end;

end.
