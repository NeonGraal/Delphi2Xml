unit D2X.Processor;

interface

uses
  CastaliaPasLexTypes,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.Rtti,
  D2X.Param,
  D2X.Handlers,
  D2X.Parser,
  D2X;

type
  TD2XProcessor = class abstract(TD2XLogger)
  public
    constructor Create(pActive: ID2XFlag);
    destructor Destroy; override;

    function UseProxy: Boolean; virtual;

    procedure SetParser(pParser: TD2XDefinesParser); virtual;

    procedure BeginProcessing; virtual;
    procedure EndProcessing; virtual;

    procedure BeginFile(pFile: string); virtual;
    procedure EndFile(pFile: string); virtual;

    procedure BeginResults; virtual;
    procedure EndResults(pFile: string); virtual;

    function CheckBeforeMethod(pMethod: string): Boolean; virtual;
    function CheckAfterMethod(pMethod: string): Boolean; virtual;

    procedure BeginMethod(pMethod: string); virtual;
    procedure EndMethod(pMethod: string); virtual;

    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer); virtual;
    procedure LexerInclude(const pFile: string; pX, pY: Integer); virtual;

  protected
    fActive: ID2XFlag;

  end;

implementation

{ TD2XProcessor }

procedure TD2XProcessor.BeginFile(pFile: string);
begin

end;

procedure TD2XProcessor.BeginMethod(pMethod: string);
begin

end;

procedure TD2XProcessor.BeginProcessing;
begin

end;

procedure TD2XProcessor.BeginResults;
begin

end;

function TD2XProcessor.CheckAfterMethod(pMethod: string): Boolean;
begin
  Result := True;
end;

function TD2XProcessor.CheckBeforeMethod(pMethod: string): Boolean;
begin
  Result := True;
end;

constructor TD2XProcessor.Create(pActive: ID2XFlag);
begin
  Assert(Assigned(pActive), 'Active Flag must exist');

  inherited Create;

  fActive := pActive;
end;

destructor TD2XProcessor.Destroy;
begin
  fActive := nil;

  inherited;
end;

procedure TD2XProcessor.EndFile(pFile: string);
begin

end;

procedure TD2XProcessor.EndMethod(pMethod: string);
begin

end;

procedure TD2XProcessor.EndProcessing;
begin

end;

procedure TD2XProcessor.EndResults(pFile: string);
begin

end;

procedure TD2XProcessor.LexerInclude(const pFile: string; pX, pY: Integer);
begin

end;

procedure TD2XProcessor.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin

end;

procedure TD2XProcessor.SetParser(pParser: TD2XDefinesParser);
begin

end;

function TD2XProcessor.UseProxy: Boolean;
begin
  Result := False;
end;

end.
