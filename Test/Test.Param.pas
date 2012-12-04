unit Test.Param;

interface

uses
  D2X.Param,
  TestFramework;

type
  TParamsTestCase = class(TTestCase)
  protected
    fParams: TD2XParams;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  System.SysUtils;

{ TParamsTestCase }

procedure TParamsTestCase.SetUp;
begin
  inherited;

  fParams := TD2XParams.Create;
end;

procedure TParamsTestCase.TearDown;
begin
  FreeAndNil(fParams);

  inherited;
end;

end.
