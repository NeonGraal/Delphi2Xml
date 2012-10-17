unit D2XHandlers;

interface

uses
  D2X,
  D2XParser,
  D2XHandler;

type
  TD2XLogHandler = class(TD2XHandler, ID2XLogger)
  private
    fLogger: ID2XLogger;
    fLexer: TD2XLexer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(pLexer: TD2XLexer);
    procedure Copy(pFrom: TD2XHandler); override;

    procedure BeginMethod(pMethod: string); override;
    procedure EndMethod(pMethod: string); override;

    property L: ID2XLogger read fLogger implements ID2XLogger;
    property Lexer: TD2XLexer read fLexer;
  end;

implementation

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

end.
