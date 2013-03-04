unit Test.Handlers;

interface

uses
  Test.Global,

  D2X.Flag,
  D2X.Handlers,
  D2X.Parser;

type
  TParserTestCase = class(TLoggerTestCase)
  protected
    fParser: TD2XDefinesParser;
    fActive: ID2XFlag;
    fFlag: TD2XBoolFlag;
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
  fFlag := TD2XBoolFlag.Create;
  fActive := fFlag;
  fActive.Flag := True;
end;

procedure TParserTestCase.TearDown;
begin
  fActive := nil;
  FreeAndNil(fFlag);
  FreeAndNil(fParser);

  inherited;
end;

end.
