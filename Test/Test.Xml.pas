unit Test.Xml;

interface

uses
  D2X.Xml,
  System.Classes,
  TestFramework;

type
  TD2XmlTestCase = class(TTestCase)
  protected
    procedure CheckXml(pExpected, pLabel: String; pXml: TStringStream);
    procedure CheckDoc(pExpected, pLabel: String; pXml: TStringStream);

  end;

  TD2XmlDocTestCase = class(TD2XmlTestCase)
  protected
    fDoc: TD2XmlDoc;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  end;

  TD2XmlElementTestCase = class(TD2XmlDocTestCase)
  protected
    fElem: TD2XmlElement;

  public
    procedure SetUp; override;

  end;

  TD2XmlNodeTester = class(TD2XmlNode)
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  Test.Utils;

{ TD2XmlTestCase }

procedure TD2XmlTestCase.CheckDoc(pExpected, pLabel: String;
  pXml: TStringStream);
begin
  CheckEqualsString('<?xml version="1.0"?> ' + pExpected, ReduceString(pXml.DataString),
    pLabel + ' Document');
end;

procedure TD2XmlTestCase.CheckXml(pExpected, pLabel: String;
  pXml: TStringStream);
begin
  CheckEqualsString(pExpected, ReduceString(pXml.DataString), pLabel + ' XML');
end;

{ TD2XmlElementTestCase }

procedure TD2XmlElementTestCase.SetUp;
begin
  inherited;

  fElem := fDoc.AddChild('Elem') as TD2XmlElement;
end;

{ TD2XmlNodeTester }

constructor TD2XmlNodeTester.Create;
begin
  inherited CreateTag('Test', nil);
end;

{ TD2XmlDocTestCase }

procedure TD2XmlDocTestCase.SetUp;
begin
  inherited;

  fDoc := NewXmlDocument;
end;

procedure TD2XmlDocTestCase.TearDown;
begin
  FreeAndNil(fDoc);

  inherited;
end;

end.
