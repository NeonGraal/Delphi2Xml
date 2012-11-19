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
    ThStreamCreator = reference to function: TStream;

  public
    constructor Create; virtual;

    function Description: String; virtual; abstract;
    function UseProxy: Boolean; virtual; abstract;

    procedure Copy(pFrom: TD2XHandler); virtual;

    procedure BeginProcessing(pInput: ThStreamCreator); virtual;
    procedure EndProcessing(pOutput: ThStreamCreator); virtual;

    procedure BeginFile(pFile: String; pInput: ThStreamCreator); virtual;
    procedure EndFile(pFile: String; pOutput: ThStreamCreator); virtual;

    procedure BeginResults; virtual;
    procedure EndResults(pOutput: ThStreamCreator); virtual;

    function CheckBeforeMethod(pMethod: String): Boolean; virtual;
    function CheckAfterMethod(pMethod: String): Boolean; virtual;

    procedure BeginMethod(pMethod: String); virtual;
    procedure EndMethod(pMethod: String); virtual;

    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: String;
      pX, pY: Integer); virtual;
    procedure LexerInclude(const pFile: String; pX, pY: Integer); virtual;
  end;

  TD2XHandlerClass = class of TD2XHandler;

function MakeStream(pS: TStream): TD2XHandler.ThStreamCreator;

implementation

function MakeStream(pS: TStream): TD2XHandler.ThStreamCreator;
begin
  Result := function: TStream
    begin
      pS.Position := 0;
      Result := pS;
    end;
end;

{ TD2XHandler }

procedure TD2XHandler.BeginFile(pFile: String; pInput: ThStreamCreator);
begin

end;

procedure TD2XHandler.BeginMethod(pMethod: String);
begin

end;

procedure TD2XHandler.BeginProcessing(pInput: ThStreamCreator);
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

procedure TD2XHandler.EndFile(pFile: String; pOutput: ThStreamCreator);
begin

end;

procedure TD2XHandler.EndMethod(pMethod: String);
begin

end;

procedure TD2XHandler.EndProcessing(pOutput: ThStreamCreator);
begin

end;

procedure TD2XHandler.EndResults(pOutput: ThStreamCreator);
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
