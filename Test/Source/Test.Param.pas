unit Test.Param;

interface

uses
  D2X.Flag,
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

  TFlagParamTestCase = class(TLoggerTestCase)
  strict private
    fFlag: ID2XFlag;

  protected
    procedure CheckFlag(pExp: Boolean; pMsg: string);
    procedure SetFlag(pVal: Boolean);
    procedure AssignFlag(pFlag: ID2XFlag);
    procedure ClearFlag(pObj: TObject);

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestGetFlag; virtual;
    procedure TestSetFlag; virtual;

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

{ TFlagParamTestCase }

procedure TFlagParamTestCase.AssignFlag(pFlag: ID2XFlag);
begin
  fFlag := pFlag;
end;

procedure TFlagParamTestCase.CheckFlag(pExp: Boolean; pMsg: string);
begin
  CheckEquals(pExp, fFlag.Flag, pMsg);
end;

procedure TFlagParamTestCase.ClearFlag(pObj: TObject);
begin
  fFlag := nil;
  pObj.Free;
end;

procedure TFlagParamTestCase.SetFlag(pVal: Boolean);
begin
  fFlag.Flag := pVal;
end;

procedure TFlagParamTestCase.SetUp;
begin
  inherited;

  fFlag := nil;
end;

procedure TFlagParamTestCase.TearDown;
begin
  CheckFalse(Assigned(fFlag), 'Flag should have been cleared before Teardown');

  inherited;
end;

procedure TFlagParamTestCase.TestGetFlag;
begin

end;

procedure TFlagParamTestCase.TestSetFlag;
begin

end;

end.
