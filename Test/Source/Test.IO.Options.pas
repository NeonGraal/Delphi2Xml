unit Test.IO.Options;

interface

uses
  D2X.Flag,
  D2X.Param,
  D2X.IO.Options,
  Test.Param;

type
  TD2XFileOptionsTest = class(TD2XFileOptions)
  public
    property OutputTimestamp: string read fOutputTimestamp;
    property TimestampFiles: Boolean read GetTimestampFiles;

  end;

  TFileFactoryTestCase = class(TParamsTestCase)
  private

    function TestValidator(pStr: string): Boolean;
  protected
    fFileOpts: TD2XFileOptionsTest;
    fValidatorCalled: Boolean;
    fTimeBool: TD2XBoolFlag;

    function ForCode(pCode: string): TD2XParam;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  System.SysUtils;

{ TD2XFileFactoryTest }

function TFileFactoryTestCase.ForCode(pCode: string): TD2XParam;
begin
  Result := fParams.ForCode(pCode);
end;

procedure TFileFactoryTestCase.SetUp;
begin
  inherited;

  fTimeBool := TD2XBoolFlag.Create;

  fFileOpts := TD2XFileOptionsTest.Create;
  fFileOpts.SetGlobalValidator(TestValidator);
  fFileOpts.SetTimestampFlag(fTimeBool.FlagRef);
  fFileOpts.RegisterParams(fParams);
end;

procedure TFileFactoryTestCase.TearDown;
begin
  FreeAndNil(fFileOpts);
  FreeAndNil(fTimeBool);

  inherited;
end;

function TFileFactoryTestCase.TestValidator(pStr: string): Boolean;
begin
  fValidatorCalled := True;
  Result := True;
end;

end.
