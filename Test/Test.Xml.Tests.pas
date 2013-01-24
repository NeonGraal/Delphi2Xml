unit Test.Xml.Tests;

interface

implementation

uses
  D2X.Xml,
  System.Classes,
  System.SysUtils,
  Test.Xml,
  TestFramework,
  Xml.XMLIntf;

type
  TestTD2XmlNode = class(TD2XmlTestCase)
  strict private
    FD2XmlNode: TD2XmlNode;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestGetXml;
    procedure TestAddChild;
    procedure TestAddAttribute;
    procedure TestHasChildNodes;
    procedure TestTrimChildren;
  end;

  TestTD2XmlAttribute = class(TD2XmlElementTestCase)
  strict private
    FD2XmlAttribute: TD2XmlAttribute;
  public
    procedure SetUp; override;
  published
    procedure TestGetXml;
    procedure TestText;
  end;

  TestTD2XmlElement = class(TD2XmlDocTestCase)
  strict private
    FD2XmlElement: TD2XmlElement;
  public
    procedure SetUp; override;
  published
    procedure TestGetXml;
    procedure TestText;
    procedure TestAddChild;
    procedure TestAddChildren;
    procedure TestAddAttribute;
    procedure TestAddAttributes;
    procedure TestHasChildNodes;
    procedure TestTrimChildren;
  end;

  TestTD2XmlDoc = class(TD2XmlTestCase)
  strict private
    FD2XmlDoc: TD2XmlDoc;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetXml;
    procedure TestGetIndentedXml;
    procedure TestAddChild;
  end;

  { TestTD2XmlNode }

procedure TestTD2XmlNode.SetUp;
begin
  inherited;

  FD2XmlNode := TD2XmlNodeTester.Create;
end;

procedure TestTD2XmlNode.TestAddChild;
var
  ReturnValue: TD2XmlNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2XmlNode.AddChild(pTag);

  CheckFalse(Assigned(ReturnValue), 'No Child for Base Node');
end;

procedure TestTD2XmlNode.TestCreate;
begin
  StartExpectingException(EAssertionFailed);
  TD2XmlNode.Create;
  StopExpectingException('Invalid Node Create');
end;

procedure TestTD2XmlNode.TestGetXml;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2XmlNode.Xml;

  CheckXml('', 'Base Node', ReturnValue);
end;

procedure TestTD2XmlNode.TearDown;
begin
  FreeAndNil(FD2XmlNode);
end;

procedure TestTD2XmlNode.TestAddAttribute;
var
  ReturnValue: TD2XmlNode;
  pTag: String;
begin
  pTag := 'Attr';

  ReturnValue := FD2XmlNode.AddAttribute(pTag);

  CheckFalse(Assigned(ReturnValue), 'No Attribute for Base Node');
end;

procedure TestTD2XmlNode.TestHasChildNodes;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FD2XmlNode.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2XmlNode.AddChild('Child1');
  ReturnValue := FD2XmlNode.HasChildNodes;
  CheckFalse(ReturnValue, 'Never child Nodes');
end;

procedure TestTD2XmlNode.TestTrimChildren;
var
  ReturnValue: Boolean;
begin
  FD2XmlNode.TrimChildren('Child');

  ReturnValue := FD2XmlNode.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes');
end;

{ TestTD2XmlAttribute }

procedure TestTD2XmlAttribute.SetUp;
begin
  inherited;

  FD2XmlAttribute := fElem.AddAttribute('Test') as TD2XmlAttribute;
end;

procedure TestTD2XmlAttribute.TestGetXml;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2XmlAttribute.Xml;

  CheckXml('Test=""', 'Attribute', ReturnValue);
end;

procedure TestTD2XmlAttribute.TestText;
var
  ReturnValue: TStringStream;
begin
  FD2XmlAttribute.Text := 'Value';

  ReturnValue := FD2XmlAttribute.Xml;

  CheckXml('Test="Value"', 'Attribute', ReturnValue);
end;

{ TestTD2XmlElement }

procedure TestTD2XmlElement.SetUp;
begin
  inherited;

  FD2XmlElement := fDoc.AddChild('Test') as TD2XmlElement;
end;

procedure TestTD2XmlElement.TestAddChild;
var
  ReturnValue: TD2XmlNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2XmlElement.AddChild(pTag);

  CheckXml('<Child />', 'Child Node', ReturnValue.Xml);
  CheckDoc('<Test><Child /></Test>', 'Simple Node', FD2XmlElement.Xml);
end;

procedure TestTD2XmlElement.TestAddChildren;
var
  ReturnValue: TD2XmlNode;
  pTag: String;
begin
  pTag := 'Child1';
  ReturnValue := FD2XmlElement.AddChild(pTag);
  CheckXml('<Child1 />', 'Child1 Node', ReturnValue.Xml);

  pTag := 'Child2';
  ReturnValue := FD2XmlElement.AddChild(pTag);
  CheckXml('<Child2 />', 'Child2 Node', ReturnValue.Xml);

  CheckDoc('<Test><Child1 /><Child2 /></Test>', 'Simple Node', FD2XmlElement.Xml);
end;

procedure TestTD2XmlElement.TestGetXml;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2XmlElement.Xml;

  CheckDoc('<Test />', 'Simple Node', ReturnValue);
end;

procedure TestTD2XmlElement.TestAddAttribute;
var
  ReturnValue: TD2XmlNode;
  pTag: String;
begin
  pTag := 'Attr';

  ReturnValue := FD2XmlElement.AddAttribute(pTag);

  CheckXml('Attr=""', 'Attr', ReturnValue.Xml);
  CheckDoc('<Test Attr="" />', 'Simple Node', FD2XmlElement.Xml);
end;

procedure TestTD2XmlElement.TestAddAttributes;
var
  ReturnValue: TD2XmlNode;
  pTag: String;
begin
  pTag := 'Attr1';
  ReturnValue := FD2XmlElement.AddAttribute(pTag);
  ReturnValue.Text := 'Value1';
  CheckXml('Attr1="Value1"', 'Attr1', ReturnValue.Xml);

  pTag := 'Attr2';
  ReturnValue := FD2XmlElement.AddAttribute(pTag);
  ReturnValue.Text := 'Value2';
  CheckXml('Attr2="Value2"', 'Attr2', ReturnValue.Xml);

  CheckDoc('<Test Attr1="Value1" Attr2="Value2" />', 'Simple Node', FD2XmlElement.Xml);
end;

procedure TestTD2XmlElement.TestHasChildNodes;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FD2XmlElement.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2XmlElement.AddChild('Child1');
  ReturnValue := FD2XmlElement.HasChildNodes;
  Check(ReturnValue, 'Has child Node');

  FD2XmlElement.AddChild('Child2');
  ReturnValue := FD2XmlElement.HasChildNodes;
  Check(ReturnValue, 'Has child Nodes');
end;

procedure TestTD2XmlElement.TestText;
var
  ReturnValue: TStringStream;
begin
  FD2XmlElement.Text := 'Value';
  ReturnValue := FD2XmlElement.Xml;

  CheckDoc('<Test>Value</Test>', 'Simple Node', ReturnValue);
end;

procedure TestTD2XmlElement.TestTrimChildren;
var
  ReturnValue: Boolean;
begin
  FD2XmlElement.TrimChildren('Child');
  ReturnValue := FD2XmlElement.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2XmlElement.AddChild('Child1');
  FD2XmlElement.TrimChildren('Child');
  ReturnValue := FD2XmlElement.HasChildNodes;
  Check(ReturnValue, 'Still has child Node');

  FD2XmlElement.TrimChildren('Child1');
  ReturnValue := FD2XmlElement.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes again');

  FD2XmlElement.AddChild('Child1');
  FD2XmlElement.AddChild('Child2');

  FD2XmlElement.TrimChildren('Child2');
  ReturnValue := FD2XmlElement.HasChildNodes;
  Check(ReturnValue, 'Still has child Nodes again');

  FD2XmlElement.TrimChildren('Child1');
  ReturnValue := FD2XmlElement.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes finally');
end;

{ TestTD2XmlDoc }

procedure TestTD2XmlDoc.SetUp;
begin
  FD2XmlDoc := NewXmlDocument;
end;

procedure TestTD2XmlDoc.TearDown;
begin
  FreeAndNil(FD2XmlDoc);
end;

procedure TestTD2XmlDoc.TestAddChild;
var
  ReturnValue: TD2XmlNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2XmlDoc.AddChild(pTag);

  CheckDoc('<Child />', 'Child Node', ReturnValue.Xml);
  CheckDoc('<Child />', 'Simple Doc', FD2XmlDoc.Xml);
end;

procedure TestTD2XmlDoc.TestGetIndentedXml;
var
  ReturnValue: TStringStream;
  lNode: TD2XmlNode;
begin
  lNode := FD2XmlDoc.AddChild('Test1');
  lNode.AddChild('Test2');
  FD2XmlDoc.Options := [doNodeAutoIndent];

  ReturnValue := FD2XmlDoc.Xml;

  CheckDoc('<Test1> <Test2 /> </Test1>', 'Indented', ReturnValue);
end;

procedure TestTD2XmlDoc.TestGetXml;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2XmlDoc.Xml;

  CheckXml('<?xml version="1.0"?>', 'Simple Doc', ReturnValue);
end;

initialization

// Register any test cases with the test runner
RegisterTests('Xml', [TestTD2XmlNode.Suite, TestTD2XmlAttribute.Suite, TestTD2XmlElement.Suite,
    TestTD2XmlDoc.Suite]);

end.