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

  end;

  TD2XTreeDocTestCase = class(TD2XTreeTestCase)
  protected
    fDoc: TD2XTreeDoc;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  end;

  TD2XTreeElementTestCase = class(TD2XTreeDocTestCase)
  protected
    fElem: TD2XTreeElement;

  public
    procedure SetUp; override;

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

{ TD2XTreeElementTestCase }

procedure TD2XTreeElementTestCase.SetUp;
begin
  inherited;

  fElem := fDoc.AddChild('Elem') as TD2XTreeElement;
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
