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
  end;

  ID2XFullProxy = interface
    ['{90414BA6-D237-4893-B3FF-EE8654938F67}']
  end;

  ID2XParser = interface
    ['{7F3065AD-B574-4214-BDC9-B4DC211F5752}']
    procedure InitParser(pParser: TD2XDefinesParser);
  end;

  ID2XProcessingHandler = interface
    ['{679284DC-8E45-471B-B303-528763507690}']
    procedure EndProcessing(pOutput: TStreamWriterRef);
  end;

  ID2XFileHandler = interface
    ['{7FEC9B9F-1502-42F6-BF00-C969B4C6051F}']
    procedure BeginFile(pFile: String; pInput: TStreamReaderRef);
    procedure EndFile(pFile: String; pOutput: TStreamWriterRef);
  end;

  ID2XResultsHandler = interface
    ['{F3A952D9-E9A2-4923-8EA2-585DBD4C8125}']
    procedure BeginResults;
    procedure EndResults(pOutput: TStreamWriterRef);
  end;

  ID2XMethods = interface
    ['{D95F54A0-45DD-48C9-949A-ECC913DDCB5E}']
    procedure BeginMethod(pMethod: String);
    procedure EndMethod(pMethod: String);
  end;

  ID2XChecks = interface
    ['{125163C9-AB41-4DF4-8135-8D9E9EB1CF79}']
    function CheckBeforeMethod(pMethod: String): Boolean;
    function CheckAfterMethod(pMethod: String): Boolean;
  end;

  ID2XMessages = interface
    ['{D0406504-283B-4768-B6E4-27EBDEC4FBD0}']
    procedure ParserMessage(const pTyp: TMessageEventType; const pMsg: String;
      pX, pY: Integer);
    procedure LexerInclude(const pFile: String; pX, pY: Integer);
  end;

  TD2XHandlerClass = class of TD2XLogger;

implementation

end.
