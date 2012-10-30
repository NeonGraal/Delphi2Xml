unit D2XHandlers;

interface

uses
  D2X,
  D2XParser,
  D2XHandler,
  System.Classes,
  System.Generics.Collections;

type
  TD2XLogHandler = class(TD2XHandler, ID2XLogger)
  private
    fLogger: ID2XLogger;
    fLexer: TD2XLexer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(pLexer: TD2XLexer);

    function Description: string; override;

    procedure Copy(pFrom: TD2XHandler); override;

    procedure BeginMethod(pMethod: string); override;
    procedure EndMethod(pMethod: string); override;

    property L: ID2XLogger read fLogger implements ID2XLogger;
    property Lexer: TD2XLexer read fLexer;
  end;

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

    procedure EndProcessing(pOutput: TD2XHandler.ThStreamCreator); override;

    procedure BeginFile(pInput: TD2XHandler.ThStreamCreator); override;
    procedure EndFile(pOutput: TD2XHandler.ThStreamCreator); override;

    procedure BeginMethod(pMethod: string); override;
    procedure EndMethod(pMethod: string); override;
  end;

  TD2XSkipHandler = class(TD2XHandler)
  private
    fSkippedMethods: TStrIntDict;

  public
    constructor Create;
    destructor Destroy; override;

    function Description: string; override;

    function CheckBeforeMethod(pMethod: string): Boolean; override;
    function CheckAfterMethod(pMethod: string): Boolean; override;

    procedure BeginFile(pInput: TD2XHandler.ThStreamCreator); override;

    procedure EndProcessing(pOutput: TD2XHandler.ThStreamCreator); override;
  end;

implementation

uses
  System.SysUtils;

{ TD2XLogHandler }

procedure TD2XLogHandler.BeginMethod(pMethod: string);
begin
  if Assigned(fLexer) then
    L.Log('BEFORE %s @ %s', [pMethod, fLexer.Token])
  else
    L.Log('BEFORE %s ', [pMethod]);
end;

procedure TD2XLogHandler.Copy(pFrom: TD2XHandler);
var
  lFrom: TD2XLogHandler;
begin
  lFrom := TD2XLogHandler(pFrom);
  fLogger := lFrom.fLogger;
  fLexer := lFrom.fLexer;
end;

constructor TD2XLogHandler.Create;
begin
  fLogger := TD2XLogger.Create;
end;

function TD2XLogHandler.Description: string;
begin
  Result := 'Verbose Logging';
end;

destructor TD2XLogHandler.Destroy;
begin
  fLogger := nil;

  inherited;
end;

procedure TD2XLogHandler.EndMethod(pMethod: string);
begin
  L.Log('AFTER  %s', [pMethod]);
end;

procedure TD2XLogHandler.Init(pLexer: TD2XLexer);
begin
  fLexer := pLexer;
end;

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

end.
