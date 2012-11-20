unit D2X.Handler;

interface

uses
  CastaliaPasLexTypes,
  System.Classes,
  System.Generics.Collections,
  System.SysUtils;

type
  EInvalidHandler = class(Exception);

  TD2XHandler = class abstract
  public type
    ThStreamReaderRef = reference to function: TStreamReader;
    ThStreamWriterRef = reference to function: TStreamWriter;

  public
    constructor Create; virtual;

    function Description: String; virtual; abstract;
    function UseProxy: Boolean; virtual; abstract;

    procedure Copy(pFrom: TD2XHandler); virtual;

    procedure BeginProcessing(pInput: ThStreamReaderRef); virtual;
    procedure EndProcessing(pOutput: ThStreamWriterRef); virtual;

    procedure BeginFile(pFile: String; pInput: ThStreamReaderRef); virtual;
    procedure EndFile(pFile: String; pOutput: ThStreamWriterRef); virtual;

    procedure BeginResults; virtual;
    procedure EndResults(pOutput: ThStreamWriterRef); virtual;

    function CheckBeforeMethod(pMethod: String): Boolean; virtual;
    function CheckAfterMethod(pMethod: String): Boolean; virtual;

    procedure BeginMethod(pMethod: String); virtual;
    procedure EndMethod(pMethod: String); virtual;

    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: String;
      pX, pY: Integer); virtual;
    procedure LexerInclude(const pFile: String; pX, pY: Integer); virtual;
  end;

  TD2XHandlerClass = class of TD2XHandler;

function MakeReader(pS: TStream): TD2XHandler.ThStreamReaderRef;
function MakeWriter(pS: TStream): TD2XHandler.ThStreamWriterRef;

implementation

function MakeReader(pS: TStream): TD2XHandler.ThStreamReaderRef;
begin
  Result := function: TStreamReader
    begin
      pS.Position := 0;
      Result := TStreamReader.Create(pS);
    end;
end;

function MakeWriter(pS: TStream): TD2XHandler.ThStreamWriterRef;
begin
  Result := function: TStreamWriter
    begin
      pS.Position := 0;
      Result := TStreamWriter.Create(pS);
    end;
end;

{ TD2XHandler }

procedure TD2XHandler.BeginFile(pFile: String; pInput: ThStreamReaderRef);
begin

end;

procedure TD2XHandler.BeginMethod(pMethod: String);
begin

end;

procedure TD2XHandler.BeginProcessing(pInput: ThStreamReaderRef);
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

procedure TD2XHandler.EndFile(pFile: String; pOutput: ThStreamWriterRef);
begin

end;

procedure TD2XHandler.EndMethod(pMethod: String);
begin

end;

procedure TD2XHandler.EndProcessing(pOutput: ThStreamWriterRef);
begin

end;

procedure TD2XHandler.EndResults(pOutput: ThStreamWriterRef);
begin

end;

procedure TD2XHandler.LexerInclude(const pFile: String; pX, pY: Integer);
begin

end;

procedure TD2XHandler.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: String; pX, pY: Integer);
begin

end;

end.
