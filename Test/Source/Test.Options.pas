unit Test.Options;

interface

uses
  D2X.Global,
  D2X.Options;

type
  TTestOptions = class(TD2XOptions)
    property ParserDefines: TD2XStringListRef read fGetParserDefines;
    property HeldDefines: TD2XStringListRef read fGetHeldDefines;
  end;

implementation

end.
