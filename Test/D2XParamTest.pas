unit D2XParamTest;

interface

uses
  TestFramework,
  D2XParam;

type

  TestTD2XParam = class(TTestCase)
  strict private
    fPrm: TD2XParam;
    fCalledParser: Boolean;

    function TstParser(pStr: string): Boolean;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestInvalidAllBlank;
    procedure TestInvalidBlankCode;
    procedure TestInvalidBlankLabel;

    procedure TestIsCode;
    procedure TestParse;
    procedure TestDescribe;
    procedure TestReport;
  end;

  TestTD2XSingleParam = class(TTestCase)
  private
    function TstParser(pStr: string): Boolean;

    function CnvStrProp(pVal: string): string;
    function InvStrProp(pVal: string): Boolean;

    function CnvBoolProp(pVal: string): Boolean;
    function FmtBoolProp(pVal: Boolean): string;

    function CnvObjProp(pVal: string): TObject;
    function FmtObjProp(pVal: TObject): string;

  published
    procedure TestInvalidAllNil;
    procedure TestInvalidNilConverter;
    procedure TestInvalidNilFormatter;

    procedure TestInvalidCreate;
    procedure TestInvalidAllBlank;
    procedure TestInvalidDefaultValue;

    procedure TestObjectParam;
    procedure TestStringParam;
    procedure TestBooleanParam;
  end;

  TestTD2XBooleanParam = class(TTestCase)
  strict private
    fBoolP: TD2XBooleanParam;

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

  TestTD2XStringParam = class(TTestCase)
  strict private
    fStrP: TD2XStringParam;

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

  TestTD2XParams = class(TTestCase)
  strict private
    fPs: TD2XParams;
    fCalledParser: Boolean;

    function TstParser(pStr: string): Boolean;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestForCode;
    procedure TestDescribeAll;
    procedure TestReportAll;
  end;

implementation

uses
  System.Classes,
  System.RegularExpressions,
  System.StrUtils,
  System.SysUtils;

function ReduceString(pStr: string): string;
begin
  Result := Trim(TRegEx.Replace(pStr, '\s+', ' ', []));
end;

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
  Result := '';
end;

function TestTD2XSingleParam.FmtBoolProp(pVal: Boolean): string;
begin
  Result := '-';
end;

function TestTD2XSingleParam.FmtObjProp(pVal: TObject): string;
begin
  Result := 'nil';
end;

function TestTD2XSingleParam.InvStrProp(pVal: string): Boolean;
begin
  Result := False;
end;

procedure TestTD2XSingleParam.TestBooleanParam;
var
  lBoolP: TD2XSingleParam<Boolean>;
begin
  lBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '', '', False, CnvBoolProp,
    FmtBoolProp, nil);

  try
    CheckEqualsString('-T Test -', ReduceString(lBoolP.Describe), 'Describe Param');
    CheckEqualsString('Test -', ReduceString(lBoolP.Report), 'Report Default Value');

    CheckFalse(lBoolP.IsCode('A'), 'Check wrong code');
    Check(lBoolP.IsCode('T'), 'Check correct code');

    CheckFalse(lBoolP.Parse('A'), 'Parse wrong code');
    Check(lBoolP.Parse('T'), 'Parse right code with No value');
    Check(lBoolP.Parse('T0'), 'Parse right code with False value');
    Check(lBoolP.Parse('T+'), 'Parse right code with True value');

    CheckFalse(lBoolP.Value, 'Returned value');
    CheckEqualsString('-', lBoolP.ToString, 'String representation');
  finally
    FreeAndNil(lBoolP);
  end;
end;

procedure TestTD2XSingleParam.TestInvalidAllBlank;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('', '', '', '', '', CnvStrProp, CnvStrProp, nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need a Code', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidDefaultValue;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('Code', 'Label', '', '', '', CnvStrProp, CnvStrProp,
      InvStrProp);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Invalid default value', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidAllNil;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('', '', '', '', '', nil, nil, nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need a Code', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidCreate;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.Create('', '', '', TstParser);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use specific constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidNilConverter;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('Code', 'Label', '', '', '', nil, CnvStrProp, nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need a Converter', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestInvalidNilFormatter;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('Code', 'Label', '', '', '', CnvStrProp, nil, nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need a Formatter', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XSingleParam.TestObjectParam;
var
  lObjP: TD2XSingleParam<TObject>;
begin
  lObjP := TD2XSingleParam<TObject>.CreateParam('T', 'Test', '', '', nil, CnvObjProp,
    FmtObjProp, nil);

  try
    CheckEqualsString('-T Test nil', ReduceString(lObjP.Describe), 'Describe Param');
    CheckEqualsString('Test nil', ReduceString(lObjP.Report), 'Report Default Value');

    CheckFalse(lObjP.IsCode('A'), 'Check Wrong code');
    Check(lObjP.IsCode('T'), 'Check correct code');

    Check(lObjP.Parse('T'), 'Parse right code with No value');
    Check(lObjP.Parse('TSimple'), 'Parse right code with value');

    Check(nil = lObjP.Value, 'Returned value');
    CheckEqualsString('nil', lObjP.ToString, 'String representation');
  finally
    FreeAndNil(lObjP);
  end;
end;

procedure TestTD2XSingleParam.TestStringParam;
var
  lStrP: TD2XSingleParam<string>;
begin
  lStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '', '', '', CnvStrProp,
    CnvStrProp, nil);

  try
    CheckEqualsString('-T Test', ReduceString(lStrP.Describe), 'Describe Param');
    CheckEqualsString('Test', ReduceString(lStrP.Report), 'Report Default Value');

    CheckFalse(lStrP.IsCode('A'), 'Check Wrong code');
    Check(lStrP.IsCode('T'), 'Check correct code');

    Check(lStrP.Parse('T'), 'Parse right code with No value');
    Check(lStrP.Parse('TSimple'), 'Parse right code with value');

    CheckEqualsString('', lStrP.Value, 'Returned value');
    CheckEqualsString('', lStrP.ToString, 'String representation');
  finally
    FreeAndNil(lStrP);
  end;
end;

function TestTD2XSingleParam.TstParser(pStr: string): Boolean;
begin
  Result := True;
end;

{ TestTD2XBooleanParam }

procedure TestTD2XBooleanParam.SetUp;
begin
  fBoolP := TD2XBooleanParam.CreateBool('T', 'Test', 'Test Boolean Param');
end;

procedure TestTD2XBooleanParam.TearDown;
begin
  FreeAndNil(fBoolP);
end;

procedure TestTD2XBooleanParam.TestDescribe;
begin
  CheckEqualsString('-T Test [+|-] - Test Boolean Param', ReduceString(fBoolP.Describe),
    'Describe Param');
end;

procedure TestTD2XBooleanParam.TestParse;
begin
  CheckFalse(fBoolP.Parse('A'), 'Parse wrong code');

  Check(fBoolP.Parse('T'), 'Parse right code with No value');
  CheckEquals(True, fBoolP.Value, 'Blank Value');

  Check(fBoolP.Parse('T0'), 'Parse right code with False value');
  CheckEquals(False, fBoolP.Value, 'False Value');

  Check(fBoolP.Parse('T+'), 'Parse right code with True value');
  CheckEquals(True, fBoolP.Value, 'True Value');
end;

procedure TestTD2XBooleanParam.TestReport;
begin
  CheckEqualsString('Test -', ReduceString(fBoolP.Report), 'Report Default Value');
end;

procedure TestTD2XBooleanParam.TestReset;
begin
  CheckEquals(False, fBoolP.Value, 'Default Value Set');

  Check(fBoolP.Parse('T'), 'Parse right code with No value');
  CheckEquals(True, fBoolP.Value, 'Blank Value');

  fBoolP.Reset;
  CheckEquals(False, fBoolP.Value, 'Default Value Reset');
end;

procedure TestTD2XBooleanParam.TestToString;
begin
  CheckEqualsString('-', fBoolP.ToString, 'Report Default Value');
end;

procedure TestTD2XBooleanParam.TestValue;
begin
  CheckEquals(False, fBoolP.Value, 'Default Value Set');
end;

{ TestTD2XStringParam }

procedure TestTD2XStringParam.SetUp;
begin
  fStrP := TD2XStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param',
    'Tst', nil);
end;

procedure TestTD2XStringParam.TearDown;
begin
  FreeAndNil(fStrP);
end;

procedure TestTD2XStringParam.TestDescribe;
begin
  CheckEqualsString('-T Test <Example> Tst Test String Param', ReduceString(fStrP.Describe),
    'Describe Param');
end;

procedure TestTD2XStringParam.TestParse;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Default Value Set');

  Check(fStrP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('', fStrP.Value, 'Blank Value');

  Check(fStrP.Parse('TSimple'), 'Parse right code with value');
  CheckEqualsString('Simple', fStrP.Value, 'Simple Value');
end;

procedure TestTD2XStringParam.TestReport;
begin
  CheckEqualsString('Test Tst', ReduceString(fStrP.Report), 'Report Default Value');
end;

procedure TestTD2XStringParam.TestReset;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Default Value Set');

  Check(fStrP.Parse('TSimple'), 'Parse right code with value');
  CheckEqualsString('Simple', fStrP.Value, 'Simple Value');

  fStrP.Reset;
  CheckEqualsString('Tst', fStrP.Value, 'Default Value Set');
end;

procedure TestTD2XStringParam.TestToString;
begin
  CheckEqualsString('Tst', fStrP.ToString, 'Check Value');
end;

procedure TestTD2XStringParam.TestValue;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Check Value');
end;

{ TestTD2XParam }

procedure TestTD2XParam.SetUp;
begin
  inherited;

  fPrm := nil;
end;

procedure TestTD2XParam.TearDown;
begin
  FreeAndNil(fPrm);

  inherited;
end;

procedure TestTD2XParam.TestDescribe;
begin
  fPrm := TD2XParam.Create('T', 'Test', '', TstParser);

  CheckEqualsString('-T Test', ReduceString(fPrm.Describe), 'Describe');
end;

procedure TestTD2XParam.TestInvalidAllBlank;
begin
  StartExpectingException(EInvalidParam);
  try
    fPrm := TD2XParam.Create('', '', '', TstParser);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need a Code', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XParam.TestInvalidBlankCode;
begin
  StartExpectingException(EInvalidParam);
  try
    fPrm := TD2XParam.Create('', 'Label', '', TstParser);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need a Code', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XParam.TestInvalidBlankLabel;
begin
  StartExpectingException(EInvalidParam);
  try
    fPrm := TD2XParam.Create('Code', '', '', TstParser);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need a Label', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XParam.TestIsCode;
begin
  fPrm := TD2XParam.Create('T', 'Test', '', TstParser);

  CheckFalse(fPrm.IsCode('A'), 'Wrong code');

  Check(fPrm.IsCode('T'), 'Correct code');
end;

procedure TestTD2XParam.TestParse;
begin
  fPrm := TD2XParam.Create('T', 'Test', '', TstParser);

  fCalledParser := False;
  CheckFalse(fPrm.Parse('A'), 'Wrong code');
  CheckFalse(fCalledParser, 'Wrong code didn''t call Parser');

  fCalledParser := False;
  Check(fPrm.Parse('T'), 'Correct code');
  Check(fCalledParser, 'Correct code called Parser');
end;

procedure TestTD2XParam.TestReport;
begin
  fPrm := TD2XParam.Create('T', 'Test', '', TstParser);

  CheckEqualsString('', fPrm.Report, 'Report');
end;

function TestTD2XParam.TstParser(pStr: string): Boolean;
begin
  fCalledParser := True;
  Result := True;
end;

{ TestTD2XParams }

procedure TestTD2XParams.SetUp;
begin
  inherited;

  fPs := TD2XParams.Create;
end;

procedure TestTD2XParams.TearDown;
begin
  FreeAndNil(fPs);

  inherited;
end;

procedure TestTD2XParams.TestDescribeAll;
begin
  CheckEqualsString('', fPs.DescribeAll, 'Describe No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', 'Test param', TstParser));
  CheckEqualsString('-T Test Test param', ReduceString(fPs.DescribeAll), 'Describe One Param');

  fPs.Add(TD2XBooleanParam.CreateBool('B', 'Boolean', 'Boolean param'));
  CheckEqualsString('-T Test Test param -B Boolean [+|-] - Boolean param',
    ReduceString(fPs.DescribeAll), 'Describe Two Params');
end;

procedure TestTD2XParams.TestForCode;
begin
  CheckFalse(Assigned(fPs.ForCode('T')), 'Code not found with No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', 'Testing', TstParser));
  CheckFalse(Assigned(fPs.ForCode('Z')), 'Wrong Code not found with One Param');
  Check(Assigned(fPs.ForCode('T')), 'Code found with One Param');

  fPs.Add(TD2XBooleanParam.CreateBool('B', 'Boolean', 'Boolean param'));
  CheckFalse(Assigned(fPs.ForCode('Z')), 'Wrong Code not found with Two Params');
  Check(Assigned(fPs.ForCode('B')), 'Code found with Two Params');
  Check(Assigned(fPs.ForCode('T')), 'Code found with Two Params');
end;

procedure TestTD2XParams.TestReportAll;
begin
  CheckEqualsString('', fPs.ReportAll, 'Report No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', 'Testing', TstParser));
  CheckEqualsString('', fPs.ReportAll, 'Report One Param');

  fPs.Add(TD2XBooleanParam.CreateBool('B', 'Boolean', 'Boolean param'));
  CheckEqualsString('Boolean -', ReduceString(fPs.ReportAll), 'Report Two Params');
end;

function TestTD2XParams.TstParser(pStr: string): Boolean;
begin
  fCalledParser := True;
end;

initialization

RegisterTests([TestTD2XBooleanParam.Suite, TestTD2XStringParam.Suite,
    TestTD2XSingleParam.Suite, TestTD2XParam.Suite, TestTD2XParams.Suite]);

end.
