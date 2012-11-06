unit D2X.Handler;

interface

uses
  CastaliaPasLexTypes,
  System.Classes,
  System.Generics.Collections;

type
  TD2XHandler = class abstract
  public type
    ThStreamCreator = reference to function: TStream;

  public
    function Description: String; virtual; abstract;
    function UseProxy: Boolean; virtual; abstract;

    procedure Copy(pFrom: TD2XHandler); virtual;

    procedure BeginProcessing(pInput: ThStreamCreator); virtual;
    procedure EndProcessing(pOutput: ThStreamCreator); virtual;

    procedure BeginFile(pInput: ThStreamCreator); virtual;
    procedure EndFile(pOutput: ThStreamCreator); virtual;

    procedure BeginResults; virtual;
    procedure EndResults(pOutput: ThStreamCreator); virtual;

    function CheckBeforeMethod(pMethod: string): Boolean; virtual;
    function CheckAfterMethod(pMethod: string): Boolean; virtual;

    procedure BeginMethod(pMethod: string); virtual;
    procedure EndMethod(pMethod: string); virtual;

    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: string; pX, pY: Integer); virtual;
    procedure LexerInclude(const pFile: string; pX, pY: Integer); virtual;
  end;

function MakeStream(pS: TStream): TD2XHandler.ThStreamCreator;

implementation

uses
  System.SysUtils;

function MakeStream(pS: TStream): TD2XHandler.ThStreamCreator;
begin
  Result := function: TStream begin pS.Position := 0; Result := pS; end;
end;
{ TD2XHandler }

procedure TD2XHandler.BeginFile(pInput: ThStreamCreator);
begin

end;

procedure TD2XHandler.BeginMethod(pMethod: string);
begin

end;

procedure TD2XHandler.BeginProcessing(pInput: ThStreamCreator);
begin

end;

procedure TD2XHandler.BeginResults;
begin

end;

function TD2XHandler.CheckAfterMethod(pMethod: string): Boolean;
begin
  Result := True;
end;

function TD2XHandler.CheckBeforeMethod(pMethod: string): Boolean;
begin
  Result := True;
end;

procedure TD2XHandler.Copy(pFrom: TD2XHandler);
begin

end;

procedure TD2XHandler.EndFile(pOutput: ThStreamCreator);
begin

end;

procedure TD2XHandler.EndMethod(pMethod: string);
begin

end;

procedure TD2XHandler.EndProcessing(pOutput: ThStreamCreator);
begin

end;

procedure TD2XHandler.EndResults(pOutput: ThStreamCreator);
begin

end;

procedure TD2XHandler.LexerInclude(const pFile: string; pX, pY: Integer);
begin

end;

procedure TD2XHandler.ParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin

end;

end.
