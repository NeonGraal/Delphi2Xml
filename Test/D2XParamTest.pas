unit D2XParamTest;

interface

uses
  TestFramework,
  D2XParam,
  System.SysUtils;

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

  TestTD2XParams = class(TTestCase)
  strict private
    fPs: TD2XParams;
    fCalledParser: Boolean;
    fSB: TStringBuilder;

    function TstParser(pStr: string): Boolean;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestForCode;
    procedure TestDescribeAll;
    procedure TestReportAll;
  end;

  TestTD2XSingleParam = class(TTestCase)
  private
    function TstParser(pStr: string): Boolean;

    function CnvDefault<T>(pStr: string; pDflt: T; out pVal: T): Boolean;

    function CnvStrProp(pStr: string; pDflt: string; out pVal: string): Boolean;
    function FmtStrProp(pVal: string): string;
    function InvStrProp(pVal: string): Boolean;

    function CnvBoolProp(pStr: string; pDflt: Boolean; out pVal: Boolean): Boolean;
    function FmtBoolProp(pVal: Boolean): string;

    function CnvObjProp(pStr: string; pDflt: TObject; out pVal: TObject): Boolean;
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

    procedure TestStringDfltParam;
    procedure TestBooleanDfltParam;
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

  TestTD2XValidStringParam = class(TTestCase)
  strict private
    fStrP: TD2XStringParam;

    function NotBlank(pStr: string): Boolean;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestParse;
    procedure TestValue;
  end;

  TestTD2XFormatStringParam = class(TTestCase)
  strict private
    fStrP: TD2XStringParam;

    function LeadingColon(pStr: string; pDflt: string; out pVal: string): Boolean;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestParse;
  end;

  TestTD2XFormatValidStringParam = class(TTestCase)
  strict private
    fStrP: TD2XStringParam;

    function LeadingColon(pStr: string; pDflt: string; out pVal: string): Boolean;
    function NotBlank(pStr: string): Boolean;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestParse;
    procedure TestValue;
  end;

  TestTD2XFlaggedStringParam = class(TTestCase)
  strict private
    fFlagP: TD2XFlaggedStringParam;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestParse;
    procedure TestReset;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestFlag;
    procedure TestValue;
    procedure TestToString;
  end;

implementation

uses
  System.Classes,
  System.RegularExpressions,
  System.StrUtils;

function ReduceString(pStr: string): string;
begin
  Result := Trim(TRegEx.Replace(pStr, '\s+', ' ', []));
end;

{ TestTD2XSingleParam }

function TestTD2XSingleParam.CnvBoolProp(pStr: string; pDflt: Boolean; out pVal: Boolean): Boolean;
begin
  pVal := False;
  Result := True;
end;

function TestTD2XSingleParam.CnvDefault<T>(pStr: string; pDflt: T;
  out pVal: T): Boolean;
begin
  Result := True;
  pVal := pDflt;
end;

function TestTD2XSingleParam.CnvObjProp(pStr: string; pDflt: TObject; out pVal: TObject): Boolean;
begin
  Result := True;
  pVal := nil;
end;

function TestTD2XSingleParam.CnvStrProp(pStr: string; pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  pVal := '';
end;

function TestTD2XSingleParam.FmtBoolProp(pVal: Boolean): string;
begin
  Result := IfThen(pVal, '+', '-');
end;

function TestTD2XSingleParam.FmtObjProp(pVal: TObject): string;
begin
  Result := 'nil';
end;

function TestTD2XSingleParam.FmtStrProp(pVal: string): string;
begin
  Result := pVal;
end;

function TestTD2XSingleParam.InvStrProp(pVal: string): Boolean;
begin
  Result := False;
end;

procedure TestTD2XSingleParam.TestBooleanDfltParam;
var
  lBoolP: TD2XSingleParam<Boolean>;
begin
  lBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '', '', True, CnvDefault<Boolean>,
    FmtBoolProp, nil);

  try
    CheckEqualsString('-T Test +', ReduceString(lBoolP.Describe), 'Describe Param');
    CheckEqualsString('Test +', ReduceString(lBoolP.Report), 'Report Default Value');

    CheckFalse(lBoolP.IsCode('A'), 'Check wrong code');
    Check(lBoolP.IsCode('T'), 'Check correct code');

    CheckFalse(lBoolP.Parse('A'), 'Parse wrong code');
    Check(lBoolP.Parse('T'), 'Parse right code with No value');
    Check(lBoolP.Parse('T0'), 'Parse right code with False value');
    Check(lBoolP.Parse('T+'), 'Parse right code with True value');

    CheckEquals(True, lBoolP.Value, 'Returned value');
    CheckEqualsString('+', lBoolP.ToString, 'String representation');
  finally
    FreeAndNil(lBoolP);
  end;
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

    CheckEquals(False, lBoolP.Value, 'Returned value');
    CheckEqualsString('-', lBoolP.ToString, 'String representation');
  finally
    FreeAndNil(lBoolP);
  end;
end;

procedure TestTD2XSingleParam.TestInvalidAllBlank;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('', '', '', '', '', CnvStrProp, FmtStrProp, nil);
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
    TD2XSingleParam<string>.CreateParam('Code', 'Label', '', '', '', CnvStrProp, FmtStrProp,
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
    TD2XSingleParam<string>.CreateParam('Code', 'Label', '', '', '', nil, FmtStrProp, nil);
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

procedure TestTD2XSingleParam.TestStringDfltParam;
var
  lStrP: TD2XSingleParam<string>;
begin
  lStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '', '', 'Tst', CnvDefault<string>,
    FmtStrProp, nil);

  try
    CheckEqualsString('-T Test Tst', ReduceString(lStrP.Describe), 'Describe Param');
    CheckEqualsString('Test Tst', ReduceString(lStrP.Report), 'Report Default Value');

    CheckFalse(lStrP.IsCode('A'), 'Check Wrong code');
    Check(lStrP.IsCode('T'), 'Check correct code');

    Check(lStrP.Parse('T'), 'Parse right code with No value');
    Check(lStrP.Parse('TSimple'), 'Parse right code with value');

    CheckEqualsString('Tst', lStrP.Value, 'Returned value');
    CheckEqualsString('Tst', lStrP.ToString, 'String representation');
  finally
    FreeAndNil(lStrP);
  end;
end;

procedure TestTD2XSingleParam.TestStringParam;
var
  lStrP: TD2XSingleParam<string>;
begin
  lStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '', '', '', CnvStrProp,
    FmtStrProp, nil);

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

  fBoolP.Value := True;
  CheckEquals(True, fBoolP.Value, 'Set Value');

  fBoolP.Reset;
  CheckEquals(False, fBoolP.Value, 'Default Value Reset');
end;

procedure TestTD2XBooleanParam.TestToString;
begin
  CheckEqualsString('-', fBoolP.ToString, 'Report Default Value');

  fBoolP.Value := True;
  CheckEqualsString('+', fBoolP.ToString, 'Report Value');
end;

procedure TestTD2XBooleanParam.TestValue;
begin
  CheckEquals(False, fBoolP.Value, 'Default Value Set');

  fBoolP.Value := True;
  CheckEquals(True, fBoolP.Value, 'Value Set');
end;

{ TestTD2XStringParam }

procedure TestTD2XStringParam.SetUp;
begin
  fStrP := TD2XStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param', 'Tst',
    nil, nil);
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

  fStrP.Value := 'Simple';
  CheckEqualsString('Test Simple', ReduceString(fStrP.Report), 'Report Simple Value');
end;

procedure TestTD2XStringParam.TestReset;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Default Value Set');

  fStrP.Value := 'Simple';
  CheckEqualsString('Simple', fStrP.Value, 'Simple Value Set');

  fStrP.Reset;
  CheckEqualsString('Tst', fStrP.Value, 'Default Value Set');
end;

procedure TestTD2XStringParam.TestToString;
begin
  CheckEqualsString('Tst', fStrP.ToString, 'Check Default Value');

  fStrP.Value := 'Simple';
  CheckEqualsString('Simple', fStrP.ToString, 'Check Simple Value');
end;

procedure TestTD2XStringParam.TestValue;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Check Default Value');

  fStrP.Value := 'Simple';
  CheckEqualsString('Simple', fStrP.Value, 'Check Simple Value');
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
  fSB := TStringBuilder.Create;
  fPs.Log := TStringWriter.Create(fSB);
end;

procedure TestTD2XParams.TearDown;
begin
  FreeAndNil(fSB);
  fPs.Log.Free;
  FreeAndNil(fPs);

  inherited;
end;

procedure TestTD2XParams.TestDescribeAll;
begin
  fPs.DescribeAll;
  CheckEqualsString('', fSB.ToString, 'Describe No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', 'Test param', TstParser));
  fSB.Clear;
  fPs.DescribeAll;
  CheckEqualsString('-T Test Test param', ReduceString(fSB.ToString), 'Describe One Param');

  fPs.Add(TD2XBooleanParam.CreateBool('B', 'Boolean', 'Boolean param'));
  fSB.Clear;
  fPs.DescribeAll;
  CheckEqualsString('-T Test Test param -B Boolean [+|-] - Boolean param',
    ReduceString(fSB.ToString), 'Describe Two Params');
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
  fPs.ReportAll;
  CheckEqualsString('', fSB.ToString, 'Report No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', 'Testing', TstParser));
  fSB.Clear;
  fPs.ReportAll;
  CheckEqualsString('', fSB.ToString, 'Report One Param');

  fPs.Add(TD2XBooleanParam.CreateBool('B', 'Boolean', 'Boolean param'));
  fSB.Clear;
  fPs.ReportAll;
  CheckEqualsString('Boolean -', ReduceString(fSB.ToString), 'Report Two Params');
end;

function TestTD2XParams.TstParser(pStr: string): Boolean;
begin
  fCalledParser := True;
  Result := True;
end;

{ TestTD2XValidStringParam }

function TestTD2XValidStringParam.NotBlank(pStr: string): Boolean;
begin
  Result := pStr > '';
end;

procedure TestTD2XValidStringParam.SetUp;
begin
  fStrP := TD2XStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param', 'Tst', nil,
    NotBlank);
end;

procedure TestTD2XValidStringParam.TearDown;
begin
  FreeAndNil(fStrP);
end;

procedure TestTD2XValidStringParam.TestParse;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Default Value Set');

  CheckFalse(fStrP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('Tst', fStrP.Value, 'Value unchanged');

  Check(fStrP.Parse('TSimple'), 'Parse right code with value');
  CheckEqualsString('Simple', fStrP.Value, 'Simple Value');
end;

procedure TestTD2XValidStringParam.TestValue;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Check Default Value');

  fStrP.Value := 'Simple';
  CheckEqualsString('Simple', fStrP.Value, 'Check Simple Value');

  StartExpectingException(EInvalidParam);
  try
    fStrP.Value := '';
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Invalid value', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

{ TestTD2XFormatStringParam }

function TestTD2XFormatStringParam.LeadingColon(pStr: string; pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  if pStr = '' then
    pVal := ''
  else
    if pStr[1] = ':' then
      pVal := Copy(pStr, 2, Length(pStr))
    else
      Result := False;
end;

procedure TestTD2XFormatStringParam.SetUp;
begin
  fStrP := TD2XStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param', 'Tst',
    LeadingColon, nil);
end;

procedure TestTD2XFormatStringParam.TearDown;
begin
  FreeAndNil(fStrP);
end;

procedure TestTD2XFormatStringParam.TestParse;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Default Value Set');

  Check(fStrP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('', fStrP.Value, 'Blank Value');

  CheckFalse(fStrP.Parse('T-Simple'), 'Parse right code with Wrong value');
  CheckEqualsString('', fStrP.Value, 'Blank Value');

  Check(fStrP.Parse('T:Simple'), 'Parse right code with Right value');
  CheckEqualsString('Simple', fStrP.Value, 'Simple Value');

  Check(fStrP.Parse('T:'), 'Parse right code with Right Blank value');
  CheckEqualsString('', fStrP.Value, 'Simple Value');
end;

{ TestTD2XFormatValidStringParam }

function TestTD2XFormatValidStringParam.LeadingColon(pStr: string; pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  if pStr = '' then
    pVal := ''
  else
    if pStr[1] = ':' then
      pVal := Copy(pStr, 2, Length(pStr))
    else
      Result := False;
end;

function TestTD2XFormatValidStringParam.NotBlank(pStr: string): Boolean;
begin
  Result := pStr > '';
end;

procedure TestTD2XFormatValidStringParam.SetUp;
begin
  fStrP := TD2XStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param', 'Tst',
    LeadingColon, NotBlank);
end;

procedure TestTD2XFormatValidStringParam.TearDown;
begin
  FreeAndNil(fStrP);
end;

procedure TestTD2XFormatValidStringParam.TestParse;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Default Value Set');

  CheckFalse(fStrP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('Tst', fStrP.Value, 'Blank Value');

  CheckFalse(fStrP.Parse('T-Simple'), 'Parse right code with Wrong value');
  CheckEqualsString('Tst', fStrP.Value, 'Blank Value');

  Check(fStrP.Parse('T:Simple'), 'Parse right code with Right value');
  CheckEqualsString('Simple', fStrP.Value, 'Simple Value');

  CheckFalse(fStrP.Parse('T:'), 'Parse right code with Right Blank value');
  CheckEqualsString('Simple', fStrP.Value, 'Simple Value');
end;

procedure TestTD2XFormatValidStringParam.TestValue;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Check Default Value');

  fStrP.Value := 'Simple';
  CheckEqualsString('Simple', fStrP.Value, 'Check Simple Value');

  StartExpectingException(EInvalidParam);
  try
    fStrP.Value := '';
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Invalid value', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

{ TestTD2XFlaggedStringParam }

procedure TestTD2XFlaggedStringParam.SetUp;
begin
  fFlagP := TD2XFlaggedStringParam.CreateFlagStr('T', 'Test', '<Example>', 'Test String Param',
    'Tst', False, nil, nil, nil);
end;

procedure TestTD2XFlaggedStringParam.TearDown;
begin
  FreeAndNil(fFlagP);
end;

procedure TestTD2XFlaggedStringParam.TestDescribe;
begin
  CheckEqualsString('-T Test [+-]:<Example> -(Tst) Test String Param', ReduceString(fFlagP.Describe),
    'Describe Param');
end;

procedure TestTD2XFlaggedStringParam.TestFlag;
begin
  CheckEquals(False, fFlagP.Flag, 'Default Flag Set');

  fFlagP.Flag := True;
  CheckEquals(True, fFlagP.Flag, 'Other Flag Set');
end;

procedure TestTD2XFlaggedStringParam.TestParse;
begin
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Set');
  CheckEquals(False, fFlagP.Flag, 'Default Flag Set');

  Check(fFlagP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('Tst', fFlagP.Value, 'Remains default Value');
  CheckEquals(True, fFlagP.Flag, 'Blank Flag Set');

  Check(fFlagP.Parse('T-'), 'Parse right code with Flag off');
  CheckEqualsString('Tst', fFlagP.Value, 'Remains default Value');
  CheckEquals(False, fFlagP.Flag, 'Flag Off');

  Check(fFlagP.Parse('T+'), 'Parse right code with Flag on');
  CheckEqualsString('Tst', fFlagP.Value, 'Remains default Value');
  CheckEquals(True, fFlagP.Flag, 'Flag On');

  fFlagP.Flag := False;
  CheckEquals(False, fFlagP.Flag, 'Flag Unset');

  Check(fFlagP.Parse('T:Simple'), 'Parse right code with value');
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value');
  CheckEquals(True, fFlagP.Flag, 'Flag Set');

  Check(fFlagP.Parse('T-'), 'Parse right code with Flag off');
  CheckEqualsString('Simple', fFlagP.Value, 'Remains previous Value');
  CheckEquals(False, fFlagP.Flag, 'Flag Off');

  Check(fFlagP.Parse('T:'), 'Parse right code with no value');
  CheckEqualsString('', fFlagP.Value, 'Blank Value');
  CheckEquals(True, fFlagP.Flag, 'Flag Set');
end;

procedure TestTD2XFlaggedStringParam.TestReport;
begin
  CheckEqualsString('Test -(Tst)', ReduceString(fFlagP.Report), 'Report Default Value');

  fFlagP.Value := '';
  CheckEqualsString('Test -', ReduceString(fFlagP.Report), 'Report Blank value off');

  fFlagP.Flag := True;
  CheckEqualsString('Test +', ReduceString(fFlagP.Report), 'Report Blank value on');

  fFlagP.Value := 'Simple';
  CheckEqualsString('Test :Simple', ReduceString(fFlagP.Report), 'Report Simple Value on');
end;

procedure TestTD2XFlaggedStringParam.TestReset;
begin
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Set');
  CheckEquals(False, fFlagP.Flag, 'Default Flag Set');

  fFlagP.Value := 'Simple';
  fFlagP.Flag := True;
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value Set');
  CheckEquals(True, fFlagP.Flag, 'Other Flag Set');

  fFlagP.Reset;
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Reset');
  CheckEquals(False, fFlagP.Flag, 'Default Flag Reset');
end;

procedure TestTD2XFlaggedStringParam.TestToString;
begin
  CheckEqualsString('-(Tst)', fFlagP.ToString, 'Check Default Value');

  fFlagP.Value := '';
  CheckEqualsString('-', fFlagP.ToString, 'Blank value off');

  fFlagP.Flag := True;
  CheckEqualsString('+', fFlagP.ToString, 'Blank value on');

  fFlagP.Value := 'Simple';
  CheckEqualsString(':Simple', fFlagP.ToString, 'Check Simple Value');
end;

procedure TestTD2XFlaggedStringParam.TestValue;
begin
  CheckEqualsString('Tst', fFlagP.Value, 'Check Default Value');

  fFlagP.Value := 'Simple';
  CheckEqualsString('Simple', fFlagP.Value, 'Check Simple Value');
end;

initialization

RegisterTests('Params', [TestTD2XParam.Suite, TestTD2XParams.Suite, TestTD2XSingleParam.Suite,
    TestTD2XBooleanParam.Suite, TestTD2XStringParam.Suite, TestTD2XValidStringParam.Suite,
    TestTD2XFormatStringParam.Suite, TestTD2XFormatValidStringParam.Suite,
    TestTD2XFlaggedStringParam.Suite]);

end.
