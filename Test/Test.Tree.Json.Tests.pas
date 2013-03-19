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

  TestTD2JsonDoc = class(TD2XTreeTestCase)
  strict private
    FD2JsonDoc: TD2XTreeDoc;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetBlankJson;
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

procedure TestTD2JsonElement.TestAttribute;
begin
  PrepareAttribute('', FD2JsonElement);

  CheckStream('Test:{_Attr:"Value"}', 'Simple Node', FD2JsonElement.Stream);
end;

procedure TestTD2JsonElement.TestAttributes;
begin
  PrepareAttributes(FD2JsonElement);

  CheckStream('Test:{_Attr1:"Value1",_Attr2:"Value2"}', 'Simple Node', FD2JsonElement.Stream);
end;

procedure TestTD2JsonElement.TestChild;
var
  ReturnValue: TD2XTreeNode;
begin
  ReturnValue := PrepareChild('', FD2JsonElement);

  CheckStream('Child:{}', 'Child Node', ReturnValue.Stream);
  CheckStream('Test:{Child:{}}', 'Simple Node', FD2JsonElement.Stream);
end;

procedure TestTD2JsonElement.TestChildren;
begin
  PrepareChildren(FD2JsonElement);

  CheckStream('Test:{Child1:{},Child2:{}}', 'Simple Node', FD2JsonElement.Stream);
end;

procedure TestTD2JsonElement.TestGetJson;
var
  ReturnValue: TStringStream;
begin
  ReturnValue := FD2JsonElement.Stream;

  CheckStream('Test:{}', 'Simple Node', ReturnValue);
end;

procedure TestTD2JsonElement.TestHasAttributes;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FD2JsonElement.HasAttributes;
  CheckFalse(ReturnValue, 'No attribute Nodes');

  FD2JsonElement.AddAttribute('Attr1');
  ReturnValue := FD2JsonElement.HasAttributes;
  Check(ReturnValue, 'Has attribute Node');

  FD2JsonElement.AddAttribute('Attr2');
  ReturnValue := FD2JsonElement.HasAttributes;
  Check(ReturnValue, 'Has attribute Nodes');
end;

procedure TestTD2JsonElement.TestHasChildren;
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
begin
  PrepareText(FD2JsonElement);

  CheckStream('Test:"Value"', 'Simple Node', FD2JsonElement.Stream);
end;

procedure TestTD2JsonElement.TestTextAttributes;
begin
  PrepareText(FD2JsonElement);
  PrepareAttributes(FD2JsonElement);

  CheckStream('Test:{_Attr1:"Value1",_Attr2:"Value2",_:"Value"}', 'Simple Node', FD2JsonElement.Stream);
end;

procedure TestTD2JsonElement.TestTextAttributesChildren;
begin
  PrepareText(FD2JsonElement);
  PrepareAttributes(FD2JsonElement);
  PrepareChildren(FD2JsonElement);

  CheckStream('Test:{_Attr1:"Value1",_Attr2:"Value2",Child1:{},Child2:{},_:"Value"}',
    'Text Attributes Children', FD2JsonElement.Stream);
end;

procedure TestTD2JsonElement.TestTextChildren;
begin
  PrepareText(FD2JsonElement);
  PrepareChildren(FD2JsonElement);

  CheckStream('Test:{Child1:{},Child2:{},_:"Value"}', 'Text Children', FD2JsonElement.Stream);
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

  CheckStream('Child:{}', 'Child Node', ReturnValue.Stream);
  CheckStream('Child:{}', 'Simple Doc', FD2JsonDoc.Stream);
end;

procedure TestTD2JsonDoc.TestGetIndentedJson;
var
  ReturnValue: TStringStream;
  lNode, lChild: TD2XTreeNode;
begin
  lNode := FD2JsonDoc.AddChild('Test');
  lChild := lNode.AddChild('Test1');
  lChild.AddAttribute('Test11').Text := 'Val11';
  lChild.AddAttribute('Test12').Text := 'Val12';
  lChild.AddChild('Test13').Text := 'Val13';
  lChild.AddChild('Test14').Text := 'Val14';
  lChild := lNode.AddChild('Test2');
  lChild.AddAttribute('Test21').Text := 'Val21';
  lChild.AddAttribute('Test22').Text := 'Val22';
  lChild.AddChild('Test23').Text := 'Val23';
  lChild.AddChild('Test24').Text := 'Val24';
  lNode.AddAttribute('Test3').Text := 'Val3';
  lNode.AddAttribute('Test4').Text := 'Val4';
  FD2JsonDoc.AddOptions([toAutoIndent]);

  ReturnValue := FD2JsonDoc.Stream;

  CheckEqualsString('Test:{'#13#10'  _Test3:"Val3"'#13#10', _Test4:"Val4"'#13#10', Test1:{' +
      #13#10'    _Test11:"Val11"'#13#10'  , _Test12:"Val12"'#13#10'  , Test13:"Val13"' +
      #13#10'  , Test14:"Val14"'#13#10'  }'#13#10', Test2:{'#13#10'    _Test21:"Val21"' +
      #13#10'  , _Test22:"Val22"'#13#10'  , Test23:"Val23"'#13#10'  , Test24:"Val24"' +
      #13#10'  }'#13#10'}', ReturnValue.DataString, 'Indented Stream');
end;

procedure TestTD2JsonDoc.TestGetJson;
var
  ReturnValue: TStringStream;
  lNode, lChild: TD2XTreeNode;
begin
  lNode := FD2JsonDoc.AddChild('Test');
  lChild := lNode.AddChild('Test1');
  lChild.AddAttribute('Test11').Text := 'Val11';
  lChild.AddAttribute('Test12').Text := 'Val12';
  lChild.AddChild('Test13').Text := 'Val13';
  lChild.AddChild('Test14').Text := 'Val14';
  lChild := lNode.AddChild('Test2');
  lChild.AddAttribute('Test21').Text := 'Val21';
  lChild.AddAttribute('Test22').Text := 'Val22';
  lChild.AddChild('Test23').Text := 'Val23';
  lChild.AddChild('Test24').Text := 'Val24';
  lNode.AddAttribute('Test3').Text := 'Val3';
  lNode.AddAttribute('Test4').Text := 'Val4';
  FD2JsonDoc.RemoveOptions([toAutoIndent]);

  ReturnValue := FD2JsonDoc.Stream;

  CheckStream('Test:{_Test3:"Val3",_Test4:"Val4",Test1:{_Test11:"Val11",_Test12:"Val12",' +
      'Test13:"Val13",Test14:"Val14"},Test2:{_Test21:"Val21",_Test22:"Val22",Test23:"Val23",' +
      'Test24:"Val24"}}', 'Complete', ReturnValue);
end;

procedure TestTD2JsonDoc.TestGetBlankJson;
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
