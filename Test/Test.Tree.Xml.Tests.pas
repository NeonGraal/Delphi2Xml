unit Test.Tree.Xml.Tests;

interface

implementation

uses
  D2X.Tree,
  D2X.Tree.Xml,
  System.Classes,
  System.SysUtils,
  Test.Tree.Xml,
  TestFramework;

type
  TestTD2XmlElement = class(TD2XmlDocTestCase)
  strict private
    FD2XmlElement: TD2XTreeElement;
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
    FD2XmlDoc: TD2XTreeDoc;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetXml;
    procedure TestGetIndentedXml;
    procedure TestAddChild;
  end;

  { TestTD2XmlElement }

procedure TestTD2XmlElement.SetUp;
begin
  inherited;

  FD2XmlElement := fDoc.AddChild('Test') as TD2XTreeElement;
end;

procedure TestTD2XmlElement.TestAddChild;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2XmlElement.AddChild(pTag);

  CheckStream('<Child />', 'Child Node', ReturnValue.Stream);
  CheckDoc('<Test><Child /></Test>', 'Simple Node', FD2XmlElement.Stream);
end;

procedure TestTD2XmlElement.TestAddChildren;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Child1';
  ReturnValue := FD2XmlElement.AddChild(pTag);
  CheckStream('<Child1 />', 'Child1 Node', ReturnValue.Stream);

  pTag := 'Child2';
  ReturnValue := FD2XmlElement.AddChild(pTag);
  CheckStream('<Child2 />', 'Child2 Node', ReturnValue.Stream);

  CheckDoc('<Test><Child1 /><Child2 /></Test>', 'Simple Node', FD2XmlElement.Stream);
end;

procedure TestTD2XmlElement.TestGetXml;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2XmlElement.Stream;

  CheckDoc('<Test />', 'Simple Node', ReturnValue);
end;

procedure TestTD2XmlElement.TestAddAttribute;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Attr';

  ReturnValue := FD2XmlElement.AddAttribute(pTag);

  CheckDoc('<Test Attr="" />', 'Simple Node', FD2XmlElement.Stream);
end;

procedure TestTD2XmlElement.TestAddAttributes;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Attr1';
  ReturnValue := FD2XmlElement.AddAttribute(pTag);
  ReturnValue.Text := 'Value1';

  pTag := 'Attr2';
  ReturnValue := FD2XmlElement.AddAttribute(pTag);
  ReturnValue.Text := 'Value2';

  CheckDoc('<Test Attr1="Value1" Attr2="Value2" />', 'Simple Node', FD2XmlElement.Stream);
end;

procedure TestTD2XmlElement.TestHasChildNodes;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FD2XmlElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2XmlElement.AddChild('Child1');
  ReturnValue := FD2XmlElement.HasChildren;
  Check(ReturnValue, 'Has child Node');

  FD2XmlElement.AddChild('Child2');
  ReturnValue := FD2XmlElement.HasChildren;
  Check(ReturnValue, 'Has child Nodes');
end;

procedure TestTD2XmlElement.TestText;
var
  ReturnValue: TStringStream;
begin
  FD2XmlElement.Text := 'Value';
  ReturnValue := FD2XmlElement.Stream;

  CheckDoc('<Test>Value</Test>', 'Simple Node', ReturnValue);
end;

procedure TestTD2XmlElement.TestTrimChildren;
var
  ReturnValue: Boolean;
begin
  FD2XmlElement.TrimChildren('Child');
  ReturnValue := FD2XmlElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2XmlElement.AddChild('Child1');
  FD2XmlElement.TrimChildren('Child');
  ReturnValue := FD2XmlElement.HasChildren;
  Check(ReturnValue, 'Still has child Node');

  FD2XmlElement.TrimChildren('Child1');
  ReturnValue := FD2XmlElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes again');

  FD2XmlElement.AddChild('Child1');
  FD2XmlElement.AddChild('Child2');

  FD2XmlElement.TrimChildren('Child2');
  ReturnValue := FD2XmlElement.HasChildren;
  Check(ReturnValue, 'Still has child Nodes again');

  FD2XmlElement.TrimChildren('Child1');
  ReturnValue := FD2XmlElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes finally');
end;

{ TestTD2XmlDoc }

procedure TestTD2XmlDoc.SetUp;
begin
  FD2XmlDoc := TD2XTreeDoc.CreateDoc(TD2XXmlWriter);
end;

procedure TestTD2XmlDoc.TearDown;
begin
  FreeAndNil(FD2XmlDoc);
end;

procedure TestTD2XmlDoc.TestAddChild;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2XmlDoc.AddChild(pTag);

  CheckDoc('<Child />', 'Child Node', ReturnValue.Stream);
  CheckDoc('<Child />', 'Simple Doc', FD2XmlDoc.Stream);
end;

procedure TestTD2XmlDoc.TestGetIndentedXml;
var
  ReturnValue: TStringStream;
  lNode: TD2XTreeNode;
begin
  lNode := FD2XmlDoc.AddChild('Test1');
  lNode.AddChild('Test2');
  FD2XmlDoc.Options := [toAutoIndent];

  ReturnValue := FD2XmlDoc.Stream;

  CheckDoc('<Test1> <Test2 /> </Test1>', 'Indented', ReturnValue);
end;

procedure TestTD2XmlDoc.TestGetXml;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2XmlDoc.Stream;

  CheckStream('<?xml version="1.0"?>', 'Simple Doc', ReturnValue);
end;

initialization

// Register any test cases with the test runner
RegisterTests('Tree Xml', [TestTD2XmlElement.Suite, TestTD2XmlDoc.Suite]);

end.
