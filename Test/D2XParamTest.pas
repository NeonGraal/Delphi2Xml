unit D2XParamTest;

interface

uses
  TestFramework,
  D2XParam;

type

  TestTD2XSingleParam = class(TTestCase)
  private
    function GetStrProp: string;
    function SetStrProp(pVal: string): Boolean;
    function CnvStrProp(pVal: string): string;
    function InvStrProp(pVal: string): Boolean;

    function GetBoolProp: Boolean;
    function SetBoolProp(pVal: Boolean): Boolean;
    function CnvBoolProp(pVal: string): Boolean;
    function FmtBoolProp(pVal: Boolean): string;

    function GetObjProp: TObject;
    function SetObjProp(pVal: TObject): Boolean;
    function CnvObjProp(pVal: string): TObject;
    function FmtObjProp(pVal: TObject): string;

  published
    procedure TestInvalidAllNil;
    procedure TestInvalidNilGetter;
    procedure TestInvalidNilSetter;
    procedure TestInvalidNilConverter;
    procedure TestInvalidNilFormatter;

    procedure TestInvalidCreate;
    procedure TestInvalidAllBlank;
    procedure TestInvalidBlankCode;
    procedure TestInvalidBlankLabel;
    procedure TestInvalidDefaultValue;

    procedure TestIsCode;

    procedure TestObjectParam;
    procedure TestStringParam;
    procedure TestBooleanParam;
  end;

  TestTD2XBooleanParam = class(TTestCase)
  strict private
    fBoolP: TD2XBooleanParam;
    fBooleanProp: Boolean;

    function GetBooleanProp: Boolean;
    function SetBooleanProp(pVal: Boolean): Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParse;
    procedure TestReset;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestValue;
    procedure TestToString;
  end;

  TestTD2XSimpleStringParam = class(TTestCase)
  strict private
    fStrP: TD2XSimpleStringParam;
    fStringProp: string;

    function GetStringProp: string;
    function SetStringProp(pVal: string): Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParse;
    procedure TestReset;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestValue;
    procedure TestToString;
  end;

implementation

uses
  System.Classes,
  System.StrUtils,
  System.SysUtils;

{ TestTD2XSingleParam }

function TestTD2XSingleParam.CnvBoolProp(pVal: string): Boolean;
begin
  Result := False;
end;

function TestTD2XSingleParam.CnvObjProp(pVal: string): TObject;
begin
  Result := nil;
end;

function TestTD2XSingleParam.CnvStrProp(pVal: string): string;
begin
  Result := pVal;
end;

function TestTD2XSingleParam.FmtBoolProp(pVal: Boolean): string;
begin
  Result := '-';
end;

function TestTD2XSingleParam.FmtObjProp(pVal: TObject): string;
begin
  Result := 'nil';
end;

function TestTD2XSingleParam.GetBoolProp: Boolean;
begin
  Result := False;
end;

function TestTD2XSingleParam.GetObjProp: TObject;
begin
  Result := nil;
end;

function TestTD2XSingleParam.GetStrProp: string;
begin
  Result := '';
end;

function TestTD2XSingleParam.InvStrProp(pVal: string): Boolean;
begin
  Result := False;
end;

function TestTD2XSingleParam.SetBoolProp(pVal: Boolean): Boolean;
begin
  Result := True;
end;

function TestTD2XSingleParam.SetObjProp(pVal: TObject): Boolean;
begin
  Result := True;
end;

function TestTD2XSingleParam.SetStrProp(pVal: string): Boolean;
begin
  Result := True;
end;

procedure TestTD2XSingleParam.TestBooleanParam;
var
  lBoolP: TD2XSingleParam<Boolean>;
begin
  lBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '', '', False, GetBoolProp,
    SetBoolProp, CnvBoolProp, FmtBoolProp);

  try
    CheckEqualsString('  -T Test                            -               ', lBoolP.Describe,
      'Describe Param');
    CheckEqualsString(' Test            -', lBoolP.Report, 'Report Default Value');

    CheckFalse(lBoolP.IsCode('A'), 'Check wrong code');
    Check(lBoolP.IsCode('T'), 'Check correct code');

    CheckFalse(lBoolP.Parse('A'), 'Parse wrong code');
    Check(lBoolP.Parse('T'), 'Parse right code with No value');
    Check(lBoolP.Parse('T0'), 'Parse right code with False value');
    Check(lBoolP.Parse('T+'), 'Parse right code with True value');

    CheckFalse(lBoolP.Value, 'Returned value');
    CheckEqualsString('-', lBoolP.ToString, 'String representation');
  finally
    lBoolP.Free;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidAllBlank;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('', '', '', '', '', GetStrProp, SetStrProp, CnvStrProp,
      CnvStrProp);
  except
    on E: EInvalidParam do begin
      CheckEqualsString('Need a Code', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidDefaultValue;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('Code', 'Label', '', '', '', GetStrProp, InvStrProp, CnvStrProp,
      CnvStrProp);
  except
    on E: EInvalidParam do begin
      CheckEqualsString('Invalid default value', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidAllNil;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('', '', '', '', '', nil, nil, nil, nil);
  except
    on E: EInvalidParam do begin
      CheckEqualsString('Need a Getter', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidBlankCode;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('', 'Label', '', '', '', GetStrProp, SetStrProp,
      CnvStrProp, CnvStrProp);
  except
    on E: EInvalidParam do begin
      CheckEqualsString('Need a Code', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidBlankLabel;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('Code', '', '', '', '', GetStrProp, SetStrProp,
      CnvStrProp, CnvStrProp);
  except
    on E: EInvalidParam do begin
      CheckEqualsString('Need a Label', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidCreate;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.Create;
  except
    on E: EInvalidParam do begin
      CheckEqualsString('Need to use Specific constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidNilConverter;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('', '', '', '', '', GetStrProp, SetStrProp, nil,
      CnvStrProp);
  except
    on E: EInvalidParam do begin
      CheckEqualsString('Need a Converter', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidNilFormatter;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('', '', '', '', '', GetStrProp, SetStrProp,
      CnvStrProp, nil);
  except
    on E: EInvalidParam do begin
      CheckEqualsString('Need a Formatter', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidNilGetter;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('', '', '', '', '', nil, SetStrProp, CnvStrProp,
      CnvStrProp);
  except
    on E: EInvalidParam do begin
      CheckEqualsString('Need a Getter', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidNilSetter;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('', '', '', '', '', GetStrProp, nil, CnvStrProp,
      CnvStrProp);
  except
    on E: EInvalidParam do begin
      CheckEqualsString('Need a Setter', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestIsCode;
begin
  with TD2XSingleParam<string>.CreateParam('T', 'Test', '', '', '', GetStrProp, SetStrProp,
    CnvStrProp, CnvStrProp) do
    try
      CheckFalse(IsCode('A'), 'Wrong code');

      Check(IsCode('T'), 'Correct code');
    finally
      Free;
    end;
end;

procedure TestTD2XSingleParam.TestObjectParam;
var
  lObjP: TD2XSingleParam<TObject>;
begin
  lObjP := TD2XSingleParam<TObject>.CreateParam('T', 'Test', '', '', nil, GetObjProp,
    SetObjProp, CnvObjProp, FmtObjProp);

  try
    CheckEqualsString('  -T Test                            nil             ', lObjP.Describe,
      'Describe Param');
    CheckEqualsString(' Test            nil', lObjP.Report, 'Report Default Value');

    CheckFalse(lObjP.IsCode('A'), 'Check Wrong code');
    Check(lObjP.IsCode('T'), 'Check correct code');

    Check(lObjP.Parse('T'), 'Parse right code with No value');
    Check(lObjP.Parse('TSimple'), 'Parse right code with value');

    Check(nil = lObjP.Value, 'Returned value');
    CheckEqualsString('nil', lObjP.ToString, 'String representation');
  finally
    lObjP.Free;
  end;
end;

procedure TestTD2XSingleParam.TestStringParam;
var
  lStrP: TD2XSingleParam<string>;
begin
  lStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '', '', '', GetStrProp, SetStrProp,
    CnvStrProp, CnvStrProp);

  try
    CheckEqualsString('  -T Test                                            ', lStrP.Describe,
      'Describe Param');
    CheckEqualsString(' Test            ', lStrP.Report, 'Report Default Value');

    CheckFalse(lStrP.IsCode('A'), 'Check Wrong code');
    Check(lStrP.IsCode('T'), 'Check correct code');

    Check(lStrP.Parse('T'), 'Parse right code with No value');
    Check(lStrP.Parse('TSimple'), 'Parse right code with value');

    CheckEqualsString('', lStrP.Value, 'Returned value');
    CheckEqualsString('', lStrP.ToString, 'String representation');
  finally
    lStrP.Free;
  end;
end;

{ TestTD2XBooleanParam }

function TestTD2XBooleanParam.GetBooleanProp: Boolean;
begin
  Result := fBooleanProp
end;

function TestTD2XBooleanParam.SetBooleanProp(pVal: Boolean): Boolean;
begin
  Result := True;
  fBooleanProp := pVal;
end;

procedure TestTD2XBooleanParam.SetUp;
begin
  fBoolP := TD2XBooleanParam.CreateBool('T', 'Test', '[+|-]', 'Test Boolean Param',
    GetBooleanProp, SetBooleanProp);
end;

procedure TestTD2XBooleanParam.TearDown;
begin
  fBoolP.Free;
  fBoolP := nil;
end;

procedure TestTD2XBooleanParam.TestDescribe;
begin
  CheckEqualsString('  -T Test            [+|-]           -               Test Boolean Param',
    fBoolP.Describe, 'Describe Param');
end;

procedure TestTD2XBooleanParam.TestParse;
begin
  CheckFalse(fBoolP.Parse('A'), 'Parse wrong code');

  Check(fBoolP.Parse('T'), 'Parse right code with No value');
  CheckEquals(True, fBooleanProp, 'Blank Value');

  Check(fBoolP.Parse('T0'), 'Parse right code with False value');
  CheckEquals(False, fBooleanProp, 'False Value');

  Check(fBoolP.Parse('T+'), 'Parse right code with True value');
  CheckEquals(True, fBooleanProp, 'True Value');
end;

procedure TestTD2XBooleanParam.TestReport;
begin
  CheckEqualsString(' Test            -', fBoolP.Report, 'Report Default Value');
end;

procedure TestTD2XBooleanParam.TestReset;
begin
  CheckEquals(False, fBooleanProp, 'Default Value Set');

  Check(fBoolP.Parse('T'), 'Parse right code with No value');
  CheckEquals(True, fBooleanProp, 'Blank Value');

  fBoolP.Reset;
  CheckEquals(False, fBooleanProp, 'Default Value Reset');
end;

procedure TestTD2XBooleanParam.TestToString;
begin
  CheckEqualsString('-', fBoolP.ToString, 'Report Default Value');
end;

procedure TestTD2XBooleanParam.TestValue;
begin
  CheckEquals(False, fBoolP.Value, 'Default Value Set');
end;

{ TestTD2XSimpleStringParam }

function TestTD2XSimpleStringParam.GetStringProp: string;
begin
  Result := fStringProp;
end;

function TestTD2XSimpleStringParam.SetStringProp(pVal: string): Boolean;
begin
  Result := True;
  fStringProp := pVal;
end;

procedure TestTD2XSimpleStringParam.SetUp;
begin
  fStrP := TD2XSimpleStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param',
    'Tst', GetStringProp, SetStringProp);
end;

procedure TestTD2XSimpleStringParam.TearDown;
begin
  fStrP.Free;
  fStrP := nil;
end;

procedure TestTD2XSimpleStringParam.TestDescribe;
begin
  CheckEqualsString('  -T Test            <Example>       Tst             Test String Param',
    fStrP.Describe, 'Describe Param');
end;

procedure TestTD2XSimpleStringParam.TestParse;
begin
  CheckEqualsString('Tst', fStringProp, 'Default Value Set');

  Check(fStrP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('', fStringProp, 'Blank Value');

  Check(fStrP.Parse('TSimple'), 'Parse right code with value');
  CheckEqualsString('Simple', fStringProp, 'Simple Value');
end;

procedure TestTD2XSimpleStringParam.TestReport;
begin
  CheckEqualsString(' Test            Tst', fStrP.Report, 'Report Default Value');
end;

procedure TestTD2XSimpleStringParam.TestReset;
begin
  CheckEqualsString('Tst', fStringProp, 'Default Value Set');

  Check(fStrP.Parse('TSimple'), 'Parse right code with value');
  CheckEqualsString('Simple', fStringProp, 'Simple Value');

  fStrP.Reset;
  CheckEqualsString('Tst', fStringProp, 'Default Value Set');
end;

procedure TestTD2XSimpleStringParam.TestToString;
begin
  CheckEqualsString('Tst', fStrP.ToString, 'Check Value');
end;

procedure TestTD2XSimpleStringParam.TestValue;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Check Value');
end;

initialization

RegisterTests([TestTD2XBooleanParam.Suite, TestTD2XSimpleStringParam.Suite,
    TestTD2XSingleParam.Suite]);

end.
