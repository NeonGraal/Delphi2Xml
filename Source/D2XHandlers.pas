unit D2XHandlers;

interface

uses
  CastaliaPasLexTypes,
  D2X,
  D2XParser,
  D2XHandler,
  D2Xml,
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

  TD2XXmlHandler = class(TD2XHandler)
  private
    fHasFiles: Boolean;

  protected
    fXmlDoc: TD2XmlDoc;
    fXmlNode: TD2XmlNode;

    fParser: TD2XDefinesParser;
    fFinalToken: TD2XCheckRef;
    fParseMode: TD2XStringRef;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(pParser: TD2XDefinesParser; pFinalToken: TD2XCheckRef;
      pParseMode: TD2XStringRef);

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

implementation

uses
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
    lFrom := TD2XXmlHandler(pFrom);
    fParser := lFrom.fParser;
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

procedure TD2XXmlHandler.Init(pParser: TD2XDefinesParser; pFinalToken: TD2XCheckRef;
  pParseMode: TD2XStringRef);
begin
  fParser := pParser;
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

end.
