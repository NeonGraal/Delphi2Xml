unit D2X.Handler;

interface

uses
  CastaliaPasLexTypes,
  D2X.Global,
  D2X.IO,
  D2X.Parser,
  System.SysUtils;

type
  EInvalidHandler = class(Exception);

  ID2XHandler = interface
    ['{3E71A83A-82B2-42DC-AA5F-F734D77306BE}']
    function Description: String;
    function UseProxy: Boolean;
  end;

  ID2XParserHandler = interface(ID2XHandler)
    ['{7F3065AD-B574-4214-BDC9-B4DC211F5752}']
    procedure InitParser(pParser: TD2XDefinesParser);
  end;

  ID2XProcessingHandler = interface(ID2XHandler)
    ['{679284DC-8E45-471B-B303-528763507690}']
    procedure EndProcessing(pOutput: TStreamWriterRef);
  end;

  ID2XFileHandler = interface(ID2XHandler)
    ['{7FEC9B9F-1502-42F6-BF00-C969B4C6051F}']
    procedure BeginFile(pFile: String; pInput: TStreamReaderRef);
    procedure EndFile(pFile: String; pOutput: TStreamWriterRef);
  end;

  ID2XResultsHandler = interface(ID2XHandler)
    ['{F3A952D9-E9A2-4923-8EA2-585DBD4C8125}']
    procedure BeginResults;
    procedure EndResults(pOutput: TStreamWriterRef);
  end;

  ID2XMethodHandler = interface(ID2XHandler)
    ['{D95F54A0-45DD-48C9-949A-ECC913DDCB5E}']
    procedure BeginMethod(pMethod: String);
    procedure EndMethod(pMethod: String);
  end;

  ID2XCheckHandler = interface(ID2XHandler)
    ['{125163C9-AB41-4DF4-8135-8D9E9EB1CF79}']
    function CheckBeforeMethod(pMethod: String): Boolean;
    function CheckAfterMethod(pMethod: String): Boolean;
  end;

  ID2XMessagesHandler = interface(ID2XHandler)
    ['{D0406504-283B-4768-B6E4-27EBDEC4FBD0}']
    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: String;
      pX, pY: Integer);
    procedure LexerInclude(const pFile: String; pX, pY: Integer);
  end;

  TD2XHandler = class abstract(TD2XLogger, ID2XHandler)
  public
    constructor Create; virtual;

    function Description: String; virtual;
    function UseProxy: Boolean; virtual;
  end;

  TD2XHandlerClass = class of TD2XHandler;

  TD2XParserHandler = class(TD2XHandler, ID2XParserHandler)
  protected
    fParser: TD2XDefinesParser;

  public
    procedure InitParser(pParser: TD2XDefinesParser); virtual;

  end;

implementation

{ TD2XHandler }

constructor TD2XHandler.Create;
begin

end;

function TD2XHandler.Description: String;
begin
  Result := 'Base Handler';
end;

function TD2XHandler.UseProxy: Boolean;
begin
  Result := False;
end;

{ TD2XParserHandler }

procedure TD2XParserHandler.InitParser(pParser: TD2XDefinesParser);
begin
  fParser := pParser;
end;

end.
