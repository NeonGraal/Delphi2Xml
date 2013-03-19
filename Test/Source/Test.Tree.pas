unit Test.Tree;

interface

uses
  D2X.Tree,
  System.Classes,
  TestFramework;

type
  TD2XTreeTestCase = class(TTestCase)
  protected
    procedure CheckStream(pExpected, pLabel: String; pStream: TStringStream);

    procedure PrepareText(pNode: TD2XTreeElement);
    function PrepareChild(pTag: String; pNode: TD2XTreeElement): TD2XTreeNode;
    procedure PrepareChildren(pNode: TD2XTreeElement);
    procedure PrepareAttribute(pTag: String; pNode: TD2XTreeElement);
    procedure PrepareAttributes(pNode: TD2XTreeElement);

  end;

  TD2XTreeDocTestCase = class(TD2XTreeTestCase)
  protected
    fDoc: TD2XTreeDoc;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  end;

  TD2XTreeNodeTester = class(TD2XTreeNode)
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  Test.Utils;

{ TD2XTreeTestCase }

procedure TD2XTreeTestCase.CheckStream(pExpected, pLabel: String;
  pStream: TStringStream);
begin
  CheckEqualsString(pExpected, ReduceString(pStream.DataString), pLabel + ' Stream');
end;

procedure TD2XTreeTestCase.PrepareAttribute(pTag: String; pNode: TD2XTreeElement);
var
  lNode: TD2XTreeNode;
begin
  lNode := pNode.AddAttribute('Attr' + pTag);
  lNode.Text := 'Value' + pTag;
end;

procedure TD2XTreeTestCase.PrepareAttributes(pNode: TD2XTreeElement);
begin
  PrepareAttribute('1', pNode);
  PrepareAttribute('2', pNode);
end;

function TD2XTreeTestCase.PrepareChild(pTag: String; pNode: TD2XTreeElement): TD2XTreeNode;
begin
  Result := pNode.AddChild('Child' + pTag);
end;

procedure TD2XTreeTestCase.PrepareChildren(pNode: TD2XTreeElement);
begin
  PrepareChild('1', pNode);
  PrepareChild('2', pNode);
end;

procedure TD2XTreeTestCase.PrepareText(pNode: TD2XTreeElement);
begin
  pNode.Text := 'Value';
end;

{ TD2XTreeNodeTester }

constructor TD2XTreeNodeTester.Create;
begin
  inherited CreateTag('Test', nil);
end;

{ TD2XTreeDocTestCase }

procedure TD2XTreeDocTestCase.SetUp;
begin
  inherited;

  fDoc := NewTreeDocument;
end;

procedure TD2XTreeDocTestCase.TearDown;
begin
  FreeAndNil(fDoc);

  inherited;
end;

end.
