unit Test.Tree.Json.Tests;

interface

implementation

uses
  D2X.Tree,
  D2X.Tree.Json,
  System.Classes,
  System.SysUtils,
  Test.Tree,
  Test.Tree.Json,
  TestFramework;

type
  TestTD2JsonElement = class(TD2JsonDocTestCase)
  strict private
    FD2JsonElement: TD2XTreeElement;
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

  TestTD2JsonDoc = class(TD2XTreeTestCase)
  strict private
    FD2JsonDoc: TD2XTreeDoc;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetJson;
    procedure TestGetIndentedJson;
    procedure TestAddChild;
  end;

  { TestTD2JsonElement }

procedure TestTD2JsonElement.SetUp;
begin
  inherited;

  FD2JsonElement := fDoc.AddChild('Test') as TD2XTreeElement;
end;

procedure TestTD2JsonElement.TestAddChild;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2JsonElement.AddChild(pTag);

  CheckStream('Child:""', 'Child Node', ReturnValue.Stream);
  CheckStream('Test:{Child:""}', 'Simple Node', FD2JsonElement.Stream);
end;

procedure TestTD2JsonElement.TestAddChildren;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Child1';
  ReturnValue := FD2JsonElement.AddChild(pTag);
  CheckStream('Child1:""', 'Child1 Node', ReturnValue.Stream);

  pTag := 'Child2';
  ReturnValue := FD2JsonElement.AddChild(pTag);
  CheckStream('Child2:""', 'Child2 Node', ReturnValue.Stream);

  CheckStream('Test:{Child1:"",Child2:""}', 'Simple Node', FD2JsonElement.Stream);
end;

procedure TestTD2JsonElement.TestGetJson;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2JsonElement.Stream;

  CheckStream('Test:""', 'Simple Node', ReturnValue);
end;

procedure TestTD2JsonElement.TestAddAttribute;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Attr';

  ReturnValue := FD2JsonElement.AddAttribute(pTag);

  CheckStream('Test:{_Attr:""}', 'Simple Node', FD2JsonElement.Stream);
end;

procedure TestTD2JsonElement.TestAddAttributes;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Attr1';
  ReturnValue := FD2JsonElement.AddAttribute(pTag);
  ReturnValue.Text := 'Value1';

  pTag := 'Attr2';
  ReturnValue := FD2JsonElement.AddAttribute(pTag);
  ReturnValue.Text := 'Value2';

  CheckStream('Test:{_Attr1:"Value1",_Attr2:"Value2"}', 'Simple Node', FD2JsonElement.Stream);
end;

procedure TestTD2JsonElement.TestHasChildNodes;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FD2JsonElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2JsonElement.AddChild('Child1');
  ReturnValue := FD2JsonElement.HasChildren;
  Check(ReturnValue, 'Has child Node');

  FD2JsonElement.AddChild('Child2');
  ReturnValue := FD2JsonElement.HasChildren;
  Check(ReturnValue, 'Has child Nodes');
end;

procedure TestTD2JsonElement.TestText;
var
  ReturnValue: TStringStream;
begin
  FD2JsonElement.Text := 'Value';
  ReturnValue := FD2JsonElement.Stream;

  CheckStream('Test:"Value"', 'Simple Node', ReturnValue);
end;

procedure TestTD2JsonElement.TestTrimChildren;
var
  ReturnValue: Boolean;
begin
  FD2JsonElement.TrimChildren('Child');
  ReturnValue := FD2JsonElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2JsonElement.AddChild('Child1');
  FD2JsonElement.TrimChildren('Child');
  ReturnValue := FD2JsonElement.HasChildren;
  Check(ReturnValue, 'Still has child Node');

  FD2JsonElement.TrimChildren('Child1');
  ReturnValue := FD2JsonElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes again');

  FD2JsonElement.AddChild('Child1');
  FD2JsonElement.AddChild('Child2');

  FD2JsonElement.TrimChildren('Child2');
  ReturnValue := FD2JsonElement.HasChildren;
  Check(ReturnValue, 'Still has child Nodes again');

  FD2JsonElement.TrimChildren('Child1');
  ReturnValue := FD2JsonElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes finally');
end;

{ TestTD2JsonDoc }

procedure TestTD2JsonDoc.SetUp;
begin
  FD2JsonDoc := TD2XTreeDoc.CreateDoc(TD2XJsonWriter);
end;

procedure TestTD2JsonDoc.TearDown;
begin
  FreeAndNil(FD2JsonDoc);
end;

procedure TestTD2JsonDoc.TestAddChild;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2JsonDoc.AddChild(pTag);

  CheckStream('Child:""', 'Child Node', ReturnValue.Stream);
  CheckStream('Child:""', 'Simple Doc', FD2JsonDoc.Stream);
end;

procedure TestTD2JsonDoc.TestGetIndentedJson;
var
  ReturnValue: TStringStream;
  lNode: TD2XTreeNode;
begin
  lNode := FD2JsonDoc.AddChild('Test1');
  lNode.AddChild('Test2');
  FD2JsonDoc.Options := [toAutoIndent];

  ReturnValue := FD2JsonDoc.Stream;

  CheckStream('Test1:{ Test2:"" }', 'Indented', ReturnValue);
end;

procedure TestTD2JsonDoc.TestGetJson;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2JsonDoc.Stream;

  CheckStream('', 'Simple Doc', ReturnValue);
end;

initialization

// Register any test cases with the test runner
RegisterTests('Tree Json', [TestTD2JsonElement.Suite, TestTD2JsonDoc.Suite]);

end.
