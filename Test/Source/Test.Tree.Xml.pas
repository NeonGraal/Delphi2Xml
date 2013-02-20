unit Test.Tree.Xml;

interface

uses
  D2X.Tree,
  D2X.Tree.Xml,
  System.Classes,
  Test.Tree,
  TestFramework;

type
  TD2XmlTestCase = class(TD2XTreeTestCase)
  protected
    procedure CheckDoc(pExpected, pLabel: String; pXml: TStringStream);

  end;

  TD2XmlDocTestCase = class(TD2XmlTestCase)
  protected
    fDoc: TD2XTreeDoc;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  end;

  TD2XmlElementTestCase = class(TD2XmlDocTestCase)
  protected
    fElem: TD2XTreeElement;

  public
    procedure SetUp; override;

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

{ TD2XmlElementTestCase }

procedure TD2XmlElementTestCase.SetUp;
begin
  inherited;

  fElem := fDoc.AddChild('Elem') as TD2XTreeElement;
end;

{ TD2XmlDocTestCase }

procedure TD2XmlDocTestCase.SetUp;
begin
  inherited;

  fDoc := TD2XTreeDoc.CreateDoc(TD2XXmlWriter);
end;

procedure TD2XmlDocTestCase.TearDown;
begin
  FreeAndNil(fDoc);

  inherited;
end;

end.
