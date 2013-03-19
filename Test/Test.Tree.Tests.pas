unit Test.Tree.Tests;

interface

implementation

uses
  D2X.Tree,
  System.Classes,
  System.SysUtils,
  Test.Tree,
  TestFramework;

type
  TestTD2XTreeNode = class(TD2XTreeTestCase)
  strict private
    FD2XTreeNode: TD2XTreeNode;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestGetStream;
    procedure TestChild;
    procedure TestAttribute;
    procedure TestHasChildren;
    procedure TestHasAttributes;
    procedure TestTrimChildren;
  end;

  TestTD2XTreeElement = class(TD2XTreeDocTestCase)
  strict private
    FD2XTreeElement: TD2XTreeElement;
  public
    procedure SetUp; override;
  published
    procedure TestGetStream;
    procedure TestText;
    procedure TestChild;
    procedure TestChildren;
    procedure TestTextChildren;
    procedure TestAttribute;
    procedure TestAttributes;
    procedure TestTextAttributes;
    procedure TestTextAttributesChildren;
    procedure TestHasChildren;
    procedure TestHasAttributes;
    procedure TestTrimChildren;
  end;

  TestTD2XTreeDoc = class(TD2XTreeTestCase)
  strict private
    FD2XTreeDoc: TD2XTreeDoc;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetStream;
    procedure TestGetIndentedStream;
    procedure TestChild;
    procedure TestAddOptions;
    procedure TestRemoveOptions;
  end;

  { TestTD2XTreeNode }

procedure TestTD2XTreeNode.SetUp;
begin
  inherited;

  FD2XTreeNode := TD2XTreeNodeTester.Create;
end;

procedure TestTD2XTreeNode.TestChild;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2XTreeNode.AddChild(pTag);

  CheckFalse(Assigned(ReturnValue), 'No Child for Base Node');
end;

procedure TestTD2XTreeNode.TestCreate;
begin
  StartExpectingException(EAssertionFailed);
  TD2XTreeNode.Create;
  StopExpectingException('Invalid Node Create');
end;

procedure TestTD2XTreeNode.TestGetStream;
begin
  StartExpectingException(ETreeWriter);
  FD2XTreeNode.Stream;
  StopExpectingException('Invalid Node GetStream');
end;

procedure TestTD2XTreeNode.TearDown;
begin
  FreeAndNil(FD2XTreeNode);
end;

procedure TestTD2XTreeNode.TestAttribute;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Attr';

  ReturnValue := FD2XTreeNode.AddAttribute(pTag);

  CheckFalse(Assigned(ReturnValue), 'No Attribute for Base Node');
end;

procedure TestTD2XTreeNode.TestHasAttributes;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FD2XTreeNode.HasAttributes;
  CheckFalse(ReturnValue, 'No attribute Nodes');

  FD2XTreeNode.AddAttribute('Attr1');
  ReturnValue := FD2XTreeNode.HasAttributes;
  CheckFalse(ReturnValue, 'Never attribute Nodes');
end;

procedure TestTD2XTreeNode.TestHasChildren;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FD2XTreeNode.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2XTreeNode.AddChild('Child1');
  ReturnValue := FD2XTreeNode.HasChildren;
  CheckFalse(ReturnValue, 'Never child Nodes');
end;

procedure TestTD2XTreeNode.TestTrimChildren;
var
  ReturnValue: Boolean;
begin
  FD2XTreeNode.TrimChildren('Child');

  ReturnValue := FD2XTreeNode.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes');
end;

{ TestTD2XTreeElement }

procedure TestTD2XTreeElement.SetUp;
begin
  inherited;

  FD2XTreeElement := fDoc.AddChild('Test') as TD2XTreeElement;
end;

procedure TestTD2XTreeElement.TestChild;
var
  ReturnValue: TD2XTreeNode;
begin
  ReturnValue := PrepareChild('', FD2XTreeElement);

  CheckStream('$Child', 'Child Node', ReturnValue.Stream);
  CheckStream('$Test<$Child>', 'Child', FD2XTreeElement.Stream);
end;

procedure TestTD2XTreeElement.TestChildren;
begin
  PrepareChildren(FD2XTreeElement);

  CheckStream('$Test<$Child1$Child2>', 'Children', FD2XTreeElement.Stream);
end;

procedure TestTD2XTreeElement.TestGetStream;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2XTreeElement.Stream;

  CheckStream('$Test', 'Simple Node', ReturnValue);
end;

procedure TestTD2XTreeElement.TestAttribute;
begin
  PrepareAttribute('', FD2XTreeElement);

  CheckStream('$Test<@Attr:Value;>', 'Attribute', FD2XTreeElement.Stream);
end;

procedure TestTD2XTreeElement.TestAttributes;
begin
  PrepareAttributes(FD2XTreeElement);

  CheckStream('$Test<@Attr1:Value1;@Attr2:Value2;>', 'Attributes', FD2XTreeElement.Stream);
end;

procedure TestTD2XTreeElement.TestHasAttributes;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FD2XTreeElement.HasAttributes;
  CheckFalse(ReturnValue, 'No attribute Nodes');

  FD2XTreeElement.AddAttribute('Attr1');
  ReturnValue := FD2XTreeElement.HasAttributes;
  Check(ReturnValue, 'Has attribute Node');

  FD2XTreeElement.AddAttribute('Attr2');
  ReturnValue := FD2XTreeElement.HasAttributes;
  Check(ReturnValue, 'Has attribute Nodes');
end;

procedure TestTD2XTreeElement.TestHasChildren;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FD2XTreeElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2XTreeElement.AddChild('Child1');
  ReturnValue := FD2XTreeElement.HasChildren;
  Check(ReturnValue, 'Has child Node');

  FD2XTreeElement.AddChild('Child2');
  ReturnValue := FD2XTreeElement.HasChildren;
  Check(ReturnValue, 'Has child Nodes');
end;

procedure TestTD2XTreeElement.TestText;
begin
  PrepareText(FD2XTreeElement);

  CheckStream('$Test:Value;', 'Text', FD2XTreeElement.Stream);
end;

procedure TestTD2XTreeElement.TestTextAttributes;
begin
  PrepareText(FD2XTreeElement);
  PrepareAttributes(FD2XTreeElement);

  CheckStream('$Test<@Attr1:Value1;@Attr2:Value2;:Value;>', 'Text Attributes',
    FD2XTreeElement.Stream);
end;

procedure TestTD2XTreeElement.TestTextAttributesChildren;
begin
  PrepareText(FD2XTreeElement);
  PrepareAttributes(FD2XTreeElement);
  PrepareChildren(FD2XTreeElement);

  CheckStream('$Test<@Attr1:Value1;@Attr2:Value2;$Child1$Child2:Value;>',
    'Text Attributes Children', FD2XTreeElement.Stream);
end;

procedure TestTD2XTreeElement.TestTextChildren;
begin
  PrepareText(FD2XTreeElement);
  PrepareChildren(FD2XTreeElement);

  CheckStream('$Test<$Child1$Child2:Value;>', 'Text Children', FD2XTreeElement.Stream);
end;

procedure TestTD2XTreeElement.TestTrimChildren;
var
  ReturnValue: Boolean;
begin
  FD2XTreeElement.TrimChildren('Child');
  ReturnValue := FD2XTreeElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes');

  FD2XTreeElement.AddChild('Child1');
  FD2XTreeElement.TrimChildren('Child');
  ReturnValue := FD2XTreeElement.HasChildren;
  Check(ReturnValue, 'Still has child Node');

  FD2XTreeElement.TrimChildren('Child1');
  ReturnValue := FD2XTreeElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes again');

  FD2XTreeElement.AddChild('Child1');
  FD2XTreeElement.AddChild('Child2');

  FD2XTreeElement.TrimChildren('Child2');
  ReturnValue := FD2XTreeElement.HasChildren;
  Check(ReturnValue, 'Still has child Nodes again');

  FD2XTreeElement.TrimChildren('Child1');
  ReturnValue := FD2XTreeElement.HasChildren;
  CheckFalse(ReturnValue, 'No child Nodes finally');
end;

{ TestTD2XTreeDoc }

procedure TestTD2XTreeDoc.SetUp;
begin
  FD2XTreeDoc := NewTreeDocument;
end;

procedure TestTD2XTreeDoc.TearDown;
begin
  FreeAndNil(FD2XTreeDoc);
end;

procedure TestTD2XTreeDoc.TestChild;
var
  ReturnValue: TD2XTreeNode;
  pTag: String;
begin
  pTag := 'Child';

  ReturnValue := FD2XTreeDoc.AddChild(pTag);

  CheckStream('$Child', 'Child Node', ReturnValue.Stream);
  CheckStream('$Child', 'Simple Doc', FD2XTreeDoc.Stream);
end;

procedure TestTD2XTreeDoc.TestAddOptions;
var
  ReturnValue: TStringStream;
  lNode: TD2XTreeNode;
begin
  lNode := FD2XTreeDoc.AddChild('Test1');
  lNode.AddChild('Test2');
  FD2XTreeDoc.AddOptions([toAutoIndent]);

  ReturnValue := FD2XTreeDoc.Stream;

  CheckStream('$Test1< $Test2 >', 'Indented', ReturnValue);
end;

procedure TestTD2XTreeDoc.TestGetIndentedStream;
var
  ReturnValue: TStringStream;
  lNode: TD2XTreeNode;
begin
  lNode := FD2XTreeDoc.AddChild('Test1');
  lNode.AddChild('Test2');
  FD2XTreeDoc.AddOptions([toAutoIndent]);

  ReturnValue := FD2XTreeDoc.Stream;

  CheckStream('$Test1< $Test2 >', 'Indented', ReturnValue);
end;

procedure TestTD2XTreeDoc.TestGetStream;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2XTreeDoc.Stream;

  CheckStream('', 'Simple Doc', ReturnValue);
end;

procedure TestTD2XTreeDoc.TestRemoveOptions;
var
  ReturnValue: TStringStream;
  lNode: TD2XTreeNode;
begin
  lNode := FD2XTreeDoc.AddChild('Test1');
  lNode.AddChild('Test2');
  FD2XTreeDoc.AddOptions([toAutoIndent]);
  FD2XTreeDoc.RemoveOptions([toAutoIndent]);

  ReturnValue := FD2XTreeDoc.Stream;

  CheckStream('$Test1<$Test2>', 'Plain', ReturnValue);
end;

initialization

// Register any test cases with the test runner
RegisterTests('Tree', [TestTD2XTreeNode.Suite, TestTD2XTreeElement.Suite,
    TestTD2XTreeDoc.Suite]);

end.
