unit Test.Tree.Json;

interface

uses
  D2X.Tree.Json,
  System.Classes,
  TestFramework;

type
  TD2JsonTestCase = class(TTestCase)
  protected
    procedure CheckJson(pExpected, pLabel: String; pJson: TStringStream);
    procedure CheckDoc(pExpected, pLabel: String; pJson: TStringStream);

  end;

  TD2JsonDocTestCase = class(TD2JsonTestCase)
  protected
    fDoc: TD2JsonDoc;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  end;

  TD2JsonElementTestCase = class(TD2JsonDocTestCase)
  protected
    fElem: TD2JsonElement;

  public
    procedure SetUp; override;

  end;

  TD2JsonNodeTester = class(TD2JsonNode)
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  Test.Utils;

{ TD2JsonTestCase }

procedure TD2JsonTestCase.CheckDoc(pExpected, pLabel: String;
  pJson: TStringStream);
begin
  CheckEqualsString('<?Json version="1.0"?> ' + pExpected, ReduceString(pJson.DataString),
    pLabel + ' Document');
end;

procedure TD2JsonTestCase.CheckJson(pExpected, pLabel: String;
  pJson: TStringStream);
begin
  CheckEqualsString(pExpected, ReduceString(pJson.DataString), pLabel + ' Json');
end;

{ TD2JsonElementTestCase }

procedure TD2JsonElementTestCase.SetUp;
begin
  inherited;

  fElem := fDoc.AddChild('Elem') as TD2JsonElement;
end;

{ TD2JsonNodeTester }

constructor TD2JsonNodeTester.Create;
begin
  inherited CreateTag('Test', nil);
end;

{ TD2JsonDocTestCase }

procedure TD2JsonDocTestCase.SetUp;
begin
  inherited;

  fDoc := NewJsonDocument;
end;

procedure TD2JsonDocTestCase.TearDown;
begin
  FreeAndNil(fDoc);

  inherited;
end;

end.
