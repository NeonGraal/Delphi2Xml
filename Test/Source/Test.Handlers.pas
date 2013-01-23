unit Test.Handlers;

interface

uses
  Test.Global,
  D2X.Handler,
  D2X.Parser;

type
  TParserTestCase = class(TLoggerTestCase)
  protected
    fParser: TD2XDefinesParser;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestParserHandler = class(TD2XParserHandler)
  public
    property Parser: TD2XDefinesParser read fParser;

  end;

implementation

uses
  System.SysUtils;

{ TParserTestCase }

procedure TParserTestCase.SetUp;
begin
  inherited;

  fParser := TD2XDefinesParser.Create;
end;

procedure TParserTestCase.TearDown;
begin
  FreeAndNil(fParser);

  inherited;
end;

end.
