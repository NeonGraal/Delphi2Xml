unit Test.Tree.Json;

interface

uses
  D2X.Tree,
  D2X.Tree.Json,
  System.Classes,
  Test.Tree,
  TestFramework;

type
  TD2JsonDocTestCase = class(TD2XTreeTestCase)
  protected
    fDoc: TD2XTreeDoc;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  end;

  TD2JsonElementTestCase = class(TD2JsonDocTestCase)
  protected
    fElem: TD2XTreeElement;

  public
    procedure SetUp; override;

  end;

implementation

uses
  System.SysUtils,
  Test.Utils;

{ TD2JsonElementTestCase }

procedure TD2JsonElementTestCase.SetUp;
begin
  inherited;

  fElem := fDoc.AddChild('Elem') as TD2XTreeElement;
end;

{ TD2JsonDocTestCase }

procedure TD2JsonDocTestCase.SetUp;
begin
  inherited;

  fDoc := TD2XTreeDoc.CreateDoc(TD2XJsonWriter);
end;

procedure TD2JsonDocTestCase.TearDown;
begin
  FreeAndNil(fDoc);

  inherited;
end;

end.
