unit Test.Param;

interface

uses
  D2X.Global,
  D2X.Param,
  Test.Global,
  TestFramework;

type
  TParamsTestCase = class(TLoggerTestCase)
  protected
    fParams: TD2XParams;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFlagRefParamTestCase = class(TLoggerTestCase)
  strict private
    fFlag: TD2XFlagRef;

  protected
    procedure CheckFlag(pExp: Boolean; pMsg: string);
    procedure AssignFlag(pFlag: TD2XFlagRef);

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

{ TFlagRefParamTestCase }

procedure TFlagRefParamTestCase.AssignFlag(pFlag: TD2XFlagRef);
begin
  fFlag := pFlag;
end;

procedure TFlagRefParamTestCase.CheckFlag(pExp: Boolean; pMsg: string);
begin
  CheckEquals(pExp, fFlag(), pMsg);
end;

end.
