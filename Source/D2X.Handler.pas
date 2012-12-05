unit D2X.Handler;

interface

uses
  CastaliaPasLexTypes,
  D2X.IO,
  D2X.Parser,
  System.Classes,
  System.Generics.Collections,
  System.SysUtils;

type
  EInvalidHandler = class(Exception);

  TD2XHandler = class abstract
  public
    constructor Create; virtual;

    function Description: String; virtual;
    function UseProxy: Boolean; virtual;

    procedure Copy(pFrom: TD2XHandler); virtual;

    procedure BeginProcessing(pInput: TStreamReaderRef); virtual;
    procedure EndProcessing(pOutput: TStreamWriterRef); virtual;

    procedure BeginFile(pFile: String; pInput: TStreamReaderRef); virtual;
    procedure EndFile(pFile: String; pOutput: TStreamWriterRef); virtual;

    procedure BeginResults; virtual;
    procedure EndResults(pOutput: TStreamWriterRef); virtual;

    function CheckBeforeMethod(pMethod: String): Boolean; virtual;
    function CheckAfterMethod(pMethod: String): Boolean; virtual;

    procedure BeginMethod(pMethod: String); virtual;
    procedure EndMethod(pMethod: String); virtual;

    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: String;
      pX, pY: Integer); virtual;
    procedure LexerInclude(const pFile: String; pX, pY: Integer); virtual;
  end;

  TD2XHandlerClass = class of TD2XHandler;

  TD2XParserHandler = class(TD2XHandler)
  protected
    fParser: TD2XDefinesParser;

  public
    procedure InitParser(pParser: TD2XDefinesParser); virtual;

    procedure Copy(pFrom: TD2XHandler); override;
  end;

implementation

{ TD2XHandler }

procedure TD2XHandler.BeginFile(pFile: String; pInput: TStreamReaderRef);
begin

end;

procedure TD2XHandler.BeginMethod(pMethod: String);
begin

end;

procedure TD2XHandler.BeginProcessing(pInput: TStreamReaderRef);
begin

end;

procedure TD2XHandler.BeginResults;
begin

end;

function TD2XHandler.CheckAfterMethod(pMethod: String): Boolean;
begin
  Result := True;
end;

function TD2XHandler.CheckBeforeMethod(pMethod: String): Boolean;
begin
  Result := True;
end;

procedure TD2XHandler.Copy(pFrom: TD2XHandler);
begin

end;

constructor TD2XHandler.Create;
begin

end;

function TD2XHandler.Description: String;
begin
  Result := 'Base Handler';
end;

procedure TD2XHandler.EndFile(pFile: String; pOutput: TStreamWriterRef);
begin

end;

procedure TD2XHandler.EndMethod(pMethod: String);
begin

end;

procedure TD2XHandler.EndProcessing(pOutput: TStreamWriterRef);
begin

end;

procedure TD2XHandler.EndResults(pOutput: TStreamWriterRef);
begin

end;

procedure TD2XHandler.LexerInclude(const pFile: String; pX, pY: Integer);
begin

end;

procedure TD2XHandler.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: String; pX, pY: Integer);
begin

end;

function TD2XHandler.UseProxy: Boolean;
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
