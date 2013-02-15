unit Test.Tree.Json.Tests;

interface

implementation

uses
  D2X.Tree.Json,
  System.Classes,
  System.SysUtils,
  Test.Tree.Json,
  TestFramework;

type
  TestTD2JsonNode = class(TD2JsonTestCase)
  strict private
    FD2JsonNode: TD2JsonNode;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestGetJson;
    procedure TestAddChild;
    procedure TestAddAttribute;
    procedure TestHasChildNodes;
    procedure TestTrimChildren;
  end;

  TestTD2JsonAttribute = class(TD2JsonElementTestCase)
  strict private
    FD2JsonAttribute: TD2JsonAttribute;
  public
    procedure SetUp; override;
  published
    procedure TestGetJson;
    procedure TestText;
  end;

  TestTD2JsonElement = class(TD2JsonDocTestCase)
  strict private
    FD2JsonElement: TD2JsonElement;
  public
    procedure SetUp; override;
  published
    procedure TestGetJson;
    procedure TestText;
    procedure TestAddChild;
    procedure TestAddChildren;
    procedure TestAddAttribute;
    procedure TestAddAttributes;
    procedure TestHasChildNodes;
    procedure TestTrimChildren;
  end;

  TestTD2JsonDoc = class(TD2JsonTestCase)
  strict private
    FD2JsonDoc: TD2JsonDoc;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetJson;
    procedure TestGetIndentedJson;
    procedure TestAddChild;
  end;

  { TestTD2JsonNode }

procedure TestTD2JsonNode.SetUp;
begin
  inherited;

  FD2JsonNode := TD2JsonNodeTester.Create;
end;

procedure TestTD2JsonNode.TestAddChild;
var
  ReturnValue: TD2JsonNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2JsonNode.AddChild(pTag);

  CheckFalse(Assigned(ReturnValue), 'No Child for Base Node');
end;

procedure TestTD2JsonNode.TestCreate;
begin
  StartExpectingException(EAssertionFailed);
  TD2JsonNode.Create;
  StopExpectingException('Invalid Node Create');
end;

procedure TestTD2JsonNode.TestGetJson;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2JsonNode.Json;

  CheckJson('', 'Base Node', ReturnValue);
end;

procedure TestTD2JsonNode.TearDown;
begin
  FreeAndNil(FD2JsonNode);
end;

procedure TestTD2JsonNode.TestAddAttribute;
var
  ReturnValue: TD2JsonNode;
  pTag: String;
begin
  pTag := 'Attr';

  ReturnValue := FD2JsonNode.AddAttribute(pTag);

  CheckFalse(Assigned(ReturnValue), 'No Attribute for Base Node');
end;

procedure TestTD2JsonNode.TestHasChildNodes;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FD2JsonNode.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2JsonNode.AddChild('Child1');
  ReturnValue := FD2JsonNode.HasChildNodes;
  CheckFalse(ReturnValue, 'Never child Nodes');
end;

procedure TestTD2JsonNode.TestTrimChildren;
var
  ReturnValue: Boolean;
begin
  FD2JsonNode.TrimChildren('Child');

  ReturnValue := FD2JsonNode.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes');
end;

{ TestTD2JsonAttribute }

procedure TestTD2JsonAttribute.SetUp;
begin
  inherited;

  FD2JsonAttribute := fElem.AddAttribute('Test') as TD2JsonAttribute;
end;

procedure TestTD2JsonAttribute.TestGetJson;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2JsonAttribute.Json;

  CheckJson('Test=""', 'Attribute', ReturnValue);
end;

procedure TestTD2JsonAttribute.TestText;
var
  ReturnValue: TStringStream;
begin
  FD2JsonAttribute.Text := 'Value';

  ReturnValue := FD2JsonAttribute.Json;

  CheckJson('Test="Value"', 'Attribute', ReturnValue);
end;

{ TestTD2JsonElement }

procedure TestTD2JsonElement.SetUp;
begin
  inherited;

  FD2JsonElement := fDoc.AddChild('Test') as TD2JsonElement;
end;

procedure TestTD2JsonElement.TestAddChild;
var
  ReturnValue: TD2JsonNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2JsonElement.AddChild(pTag);

  CheckJson('<Child />', 'Child Node', ReturnValue.Json);
  CheckDoc('<Test><Child /></Test>', 'Simple Node', FD2JsonElement.Json);
end;

procedure TestTD2JsonElement.TestAddChildren;
var
  ReturnValue: TD2JsonNode;
  pTag: String;
begin
  pTag := 'Child1';
  ReturnValue := FD2JsonElement.AddChild(pTag);
  CheckJson('<Child1 />', 'Child1 Node', ReturnValue.Json);

  pTag := 'Child2';
  ReturnValue := FD2JsonElement.AddChild(pTag);
  CheckJson('<Child2 />', 'Child2 Node', ReturnValue.Json);

  CheckDoc('<Test><Child1 /><Child2 /></Test>', 'Simple Node', FD2JsonElement.Json);
end;

procedure TestTD2JsonElement.TestGetJson;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2JsonElement.Json;

  CheckDoc('<Test />', 'Simple Node', ReturnValue);
end;

procedure TestTD2JsonElement.TestAddAttribute;
var
  ReturnValue: TD2JsonNode;
  pTag: String;
begin
  pTag := 'Attr';

  ReturnValue := FD2JsonElement.AddAttribute(pTag);

  CheckJson('Attr=""', 'Attr', ReturnValue.Json);
  CheckDoc('<Test Attr="" />', 'Simple Node', FD2JsonElement.Json);
end;

procedure TestTD2JsonElement.TestAddAttributes;
var
  ReturnValue: TD2JsonNode;
  pTag: String;
begin
  pTag := 'Attr1';
  ReturnValue := FD2JsonElement.AddAttribute(pTag);
  ReturnValue.Text := 'Value1';
  CheckJson('Attr1="Value1"', 'Attr1', ReturnValue.Json);

  pTag := 'Attr2';
  ReturnValue := FD2JsonElement.AddAttribute(pTag);
  ReturnValue.Text := 'Value2';
  CheckJson('Attr2="Value2"', 'Attr2', ReturnValue.Json);

  CheckDoc('<Test Attr1="Value1" Attr2="Value2" />', 'Simple Node', FD2JsonElement.Json);
end;

procedure TestTD2JsonElement.TestHasChildNodes;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FD2JsonElement.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2JsonElement.AddChild('Child1');
  ReturnValue := FD2JsonElement.HasChildNodes;
  Check(ReturnValue, 'Has child Node');

  FD2JsonElement.AddChild('Child2');
  ReturnValue := FD2JsonElement.HasChildNodes;
  Check(ReturnValue, 'Has child Nodes');
end;

procedure TestTD2JsonElement.TestText;
var
  ReturnValue: TStringStream;
begin
  FD2JsonElement.Text := 'Value';
  ReturnValue := FD2JsonElement.Json;

  CheckDoc('<Test>Value</Test>', 'Simple Node', ReturnValue);
end;

procedure TestTD2JsonElement.TestTrimChildren;
var
  ReturnValue: Boolean;
begin
  FD2JsonElement.TrimChildren('Child');
  ReturnValue := FD2JsonElement.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2JsonElement.AddChild('Child1');
  FD2JsonElement.TrimChildren('Child');
  ReturnValue := FD2JsonElement.HasChildNodes;
  Check(ReturnValue, 'Still has child Node');

  FD2JsonElement.TrimChildren('Child1');
  ReturnValue := FD2JsonElement.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes again');

  FD2JsonElement.AddChild('Child1');
  FD2JsonElement.AddChild('Child2');

  FD2JsonElement.TrimChildren('Child2');
  ReturnValue := FD2JsonElement.HasChildNodes;
  Check(ReturnValue, 'Still has child Nodes again');

  FD2JsonElement.TrimChildren('Child1');
  ReturnValue := FD2JsonElement.HasChildNodes;
  CheckFalse(ReturnValue, 'No child Nodes finally');
end;

{ TestTD2JsonDoc }

procedure TestTD2JsonDoc.SetUp;
begin
  FD2JsonDoc := NewJsonDocument;
end;

procedure TestTD2JsonDoc.TearDown;
begin
  FreeAndNil(FD2JsonDoc);
end;

procedure TestTD2JsonDoc.TestAddChild;
var
  ReturnValue: TD2JsonNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2JsonDoc.AddChild(pTag);

  CheckDoc('<Child />', 'Child Node', ReturnValue.Json);
  CheckDoc('<Child />', 'Simple Doc', FD2JsonDoc.Json);
end;

procedure TestTD2JsonDoc.TestGetIndentedJson;
var
  ReturnValue: TStringStream;
  lNode: TD2JsonNode;
begin
  lNode := FD2JsonDoc.AddChild('Test1');
  lNode.AddChild('Test2');
//  FD2JsonDoc.Options := [doNodeAutoIndent];

  ReturnValue := FD2JsonDoc.Json;

  CheckDoc('<Test1> <Test2 /> </Test1>', 'Indented', ReturnValue);
end;

procedure TestTD2JsonDoc.TestGetJson;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2JsonDoc.Json;

  CheckJson('<?Json version="1.0"?>', 'Simple Doc', ReturnValue);
end;

initialization

// Register any test cases with the test runner
RegisterTests('Tree Json', [TestTD2JsonNode.Suite, TestTD2JsonAttribute.Suite, TestTD2JsonElement.Suite,
    TestTD2JsonDoc.Suite]);

end.
