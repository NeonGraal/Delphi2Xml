unit Test.Params;

interface

implementation

uses
  D2X,
  D2X.IO,
  D2X.IO.Actual,
  D2X.Param,
  D2X.Params,
  System.Classes,
  System.Rtti,
  System.StrUtils,
  System.SysUtils,
  Test.Global,
  Test.IO,
  Test.Param,
  Test.Utils,
  TestFramework;

type
  TestTD2XParam = class(TLoggerTestCase)
  strict private
    fPrm: TD2XParam;

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
    procedure TestOutput;
    procedure TestToString;
    procedure TestIsDefault;
  end;

  TestTD2XParams = class(TLoggerTestCase)
  strict private
    fPs: TD2XParams;

    function TstParser(pStr: string): Boolean;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestForCode;
    procedure TestDescribeAll;
    procedure TestOutputAll;
    procedure TestReportAll;
    procedure TestResetAll;
    procedure TestZeroAll;
  end;

  TestTD2XSingleParam = class(TLoggerTestCase)
  private
    function FmtStrProp(pVal: string): string;
    function InvStrProp(pVal: string): Boolean;

    function FmtBoolProp(pVal: Boolean): string;

  published
    procedure TestInvalidAllNil;
    procedure TestInvalidNilConverter;
    procedure TestInvalidNilFormatter;

    procedure TestInvalidAllBlank;
    procedure TestInvalidDefaultValue;

    procedure TestObjectParam;
    procedure TestStringParam;
    procedure TestBooleanParam;

    procedure TestStringDfltParam;
    procedure TestBooleanDfltParam;
  end;

  TFlagParamTestCase = class(TLoggerTestCase)
  protected
    fFlag: ID2XFlag;

  public
    procedure SetUp; override;

  published
    procedure TestGetFlag; virtual; abstract;
    procedure TestSetFlag; virtual; abstract;

  end;

  TestTD2XFlagsParam = class(TLoggerTestCase)
  strict private
    fFlagsP: TD2XFlagsParam;

    function TstParser(pStr: string): Boolean;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestInvalidCreate;
    procedure TestInvalidCreateFlags;

    procedure TestParse;
    procedure TestReset;
    procedure TestZero;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestOutput;
    procedure TestToString;
    procedure TestIsDefault;
  end;

  TestTD2XBooleanParam = class(TFlagParamTestCase)
  strict private
    fBoolP: TD2XBooleanParam;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestInvalidCreateReset;
    procedure TestInvalidCreateParam;
    procedure TestParse;
    procedure TestReset;
    procedure TestZero;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestOutput;
    procedure TestValue;
    procedure TestToString;
    procedure TestIsDefault;
    procedure TestGetFlag; override;
    procedure TestSetFlag; override;
  end;

  TestTD2XStringParam = class(TLoggerTestCase)
  strict private
    fStrP: TD2XStringParam;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestInvalidCreateReset;
    procedure TestInvalidCreateParam;
    procedure TestCreateStr;
    procedure TestParse;
    procedure TestReset;
    procedure TestZero;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestOutput;
    procedure TestValue;
    procedure TestToString;
    procedure TestIsDefault;
  end;

  TestTD2XValidStringParam = class(TTestCase)
  strict private
    fStrP: TD2XStringParam;

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

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestParse;
  end;

  TestTD2XFormatValidStringParam = class(TTestCase)
  strict private
    fStrP: TD2XStringParam;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestParse;
    procedure TestValue;
  end;

  TTestFlaggedStringParam = class(TD2XFlaggedStringParam)
  public
    function OldFormatted(pVal: string): string;
  end;

  TestTD2XFlaggedStringParam = class(TFlagParamTestCase)
  strict private
    fFlagP: TTestFlaggedStringParam;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestInvalidCreateParam;
    procedure TestOldFormatted;
    procedure TestParse;
    procedure TestReset;
    procedure TestZero;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestOutput;
    procedure TestFlag;
    procedure TestValue;
    procedure TestToString;
    procedure TestIsDefault;
    procedure TestGetFlag; override;
    procedure TestSetFlag; override;
  end;

  TestTD2XDefinesParam = class(TFlagParamTestCase)
  strict private
    fDefP: TD2XDefinesParam;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestInvalidCreateParam;
    procedure TestParse;
    procedure TestReset;
    procedure TestZero;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestOutput;
    procedure TestValue;
    procedure TestToString;
    procedure TestIsDefault;
    procedure TestGetFlag; override;
    procedure TestSetFlag; override;
  end;

  { TestTD2XSingleParam }

function TestTD2XSingleParam.FmtBoolProp(pVal: Boolean): string;
begin
  Result := IfThen(pVal, '+', '-');
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
  lBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '', '', True,
    TD2X.CnvDflt<Boolean>, FmtBoolProp, nil);

  try
    lBoolP.Describe(fLog);
    CheckLog('T +', 'Describe Param');
    lBoolP.Report(fLog);
    CheckLog('Test +', 'Report Default Value');
    Check(lBoolP.IsDefault, 'Check is Default');

    CheckFalse(lBoolP.IsCode('A'), 'Check wrong code');
    Check(lBoolP.IsCode('T'), 'Check correct code');

    CheckFalse(lBoolP.Parse('A'), 'Parse wrong code');
    Check(lBoolP.Parse('T'), 'Parse right code with No value');
    Check(lBoolP.Parse('T0'), 'Parse right code with False value');
    Check(lBoolP.Parse('T+'), 'Parse right code with True value');

    CheckEquals(True, lBoolP.Value, 'Returned value');
    CheckEqualsString('T+', lBoolP.ToString, 'String representation');
  finally
    FreeAndNil(lBoolP);
  end;
end;

procedure TestTD2XSingleParam.TestBooleanParam;
var
  lBoolP: TD2XSingleParam<Boolean>;
begin
  lBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '', '', False,
      function(pStr: string; pDflt: Boolean; out pVal: Boolean): Boolean
    begin
      pVal := False;
      Result := True;
    end, FmtBoolProp, nil);

  try
    lBoolP.Describe(fLog);
    CheckLog('T -', 'Describe Param');
    lBoolP.Report(fLog);
    CheckLog('Test -', 'Report Default Value');
    Check(lBoolP.IsDefault, 'Check is Default');

    CheckFalse(lBoolP.IsCode('A'), 'Check wrong code');
    Check(lBoolP.IsCode('T'), 'Check correct code');

    CheckFalse(lBoolP.Parse('A'), 'Parse wrong code');
    Check(lBoolP.Parse('T'), 'Parse right code with No value');
    Check(lBoolP.Parse('T0'), 'Parse right code with False value');
    Check(lBoolP.Parse('T+'), 'Parse right code with True value');

    CheckEquals(False, lBoolP.Value, 'Returned value');
    CheckEqualsString('T-', lBoolP.ToString, 'String representation');
  finally
    FreeAndNil(lBoolP);
  end;
end;

procedure TestTD2XSingleParam.TestInvalidAllBlank;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.CreateParam('', '', '', '', '', TD2X.CnvDflt<string>,
      FmtStrProp, nil);
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
    TD2XSingleParam<string>.CreateParam('Code', 'Label', '', '', '', TD2X.CnvDflt<string>,
      FmtStrProp, InvStrProp);
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
    TD2XSingleParam<string>.CreateParam('Code', 'Label', '', '', '', TD2X.CnvDflt<string>,
      nil, nil);
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
  lObjP := TD2XSingleParam<TObject>.CreateParam('T', 'Test', '', '', nil,
    function(pStr: string; pDflt: TObject; out pVal: TObject): Boolean
    begin
      Result := True;
      pVal := nil;
    end,
    function(pVal: TObject): string
    begin
      Result := 'nil';
    end, nil);

  try
    lObjP.Describe(fLog);
    CheckLog('T nil', 'Describe Param');
    lObjP.Report(fLog);
    CheckLog('Test nil', 'Report Default Value');
    Check(lObjP.IsDefault, 'Check is Default');

    CheckFalse(lObjP.IsCode('A'), 'Check Wrong code');
    Check(lObjP.IsCode('T'), 'Check correct code');

    Check(lObjP.Parse('T'), 'Parse right code with No value');
    Check(lObjP.Parse('TSimple'), 'Parse right code with value');

    Check(nil = lObjP.Value, 'Returned value');
    CheckEqualsString('Tnil', lObjP.ToString, 'String representation');
  finally
    FreeAndNil(lObjP);
  end;
end;

procedure TestTD2XSingleParam.TestStringDfltParam;
var
  lStrP: TD2XSingleParam<string>;
begin
  lStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '', '', 'Tst',
    TD2X.CnvDflt<string>, FmtStrProp, nil);

  try
    lStrP.Describe(fLog);
    CheckLog('T Tst', 'Describe Param');
    lStrP.Report(fLog);
    CheckLog('Test Tst', 'Report Default Value');
    Check(lStrP.IsDefault, 'Check is Default');

    CheckFalse(lStrP.IsCode('A'), 'Check Wrong code');
    Check(lStrP.IsCode('T'), 'Check correct code');

    Check(lStrP.Parse('T'), 'Parse right code with No value');
    Check(lStrP.Parse('TSimple'), 'Parse right code with value');

    CheckEqualsString('Tst', lStrP.Value, 'Returned value');
    CheckEqualsString('TTst', lStrP.ToString, 'String representation');
  finally
    FreeAndNil(lStrP);
  end;
end;

procedure TestTD2XSingleParam.TestStringParam;
var
  lStrP: TD2XSingleParam<string>;
begin
  lStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '', '', '',
    function(pStr: string; pDflt: string; out pVal: string): Boolean
    begin
      Result := True;
      pVal := '';
    end, FmtStrProp, nil);

  try
    lStrP.Describe(fLog);
    CheckLog('T', 'Describe Param');
    lStrP.Report(fLog);
    CheckLog('Test', 'Report Default Value');
    Check(lStrP.IsDefault, 'Check is Default');

    CheckFalse(lStrP.IsCode('A'), 'Check Wrong code');
    Check(lStrP.IsCode('T'), 'Check correct code');

    Check(lStrP.Parse('T'), 'Parse right code with No value');
    Check(lStrP.Parse('TSimple'), 'Parse right code with value');

    CheckEqualsString('', lStrP.Value, 'Returned value');
    CheckEqualsString('T', lStrP.ToString, 'String representation');
  finally
    FreeAndNil(lStrP);
  end;
end;

{ TestTD2XBooleanParam }

procedure TestTD2XBooleanParam.SetUp;
begin
  inherited;

  fBoolP := TD2XBooleanParam.CreateBool('T', 'Test', 'Test Boolean Param');
  fFlag := fBoolP;
end;

procedure TestTD2XBooleanParam.TearDown;
begin
  fFlag := nil;
  FreeAndNil(fBoolP);

  inherited;
end;

procedure TestTD2XBooleanParam.TestDescribe;
begin
  fBoolP.Describe(fLog);
  CheckLog('T[+|-] - Test Boolean Param', 'Describe Param');
end;

procedure TestTD2XBooleanParam.TestGetFlag;
begin
  CheckFalse(fFlag.Flag, 'Check is Default');

  fBoolP.Value := True;
  CheckTrue(fFlag.Flag, 'Check is not Default');

  fBoolP.Reset;
  CheckFalse(fFlag.Flag, 'Check is Default');
end;

procedure TestTD2XBooleanParam.TestInvalidCreateParam;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XBooleanParam.CreateParam('', '', '', '', False, nil, nil, nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XBooleanParam.TestInvalidCreateReset;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<Boolean>.Create('', '', '', '', nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XBooleanParam.TestIsDefault;
begin
  Check(fBoolP.IsDefault, 'Check is Default');

  fBoolP.Value := True;
  CheckFalse(fBoolP.IsDefault, 'Check is not Default');

  fBoolP.Reset;
  Check(fBoolP.IsDefault, 'Check is Default');
end;

procedure TestTD2XBooleanParam.TestOutput;
begin
  fBoolP.Output(fL);
  CheckList('', 'Output Default Value');

  fBoolP.Value := True;
  fBoolP.Output(fL);
  CheckList('-T+', 'Output True Value');
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
  fBoolP.Report(fLog);
  CheckLog('Test -', 'Report Default Value');

  fBoolP.Value := True;
  fBoolP.Report(fLog);
  CheckLog('Test +', 'Report True Value');
end;

procedure TestTD2XBooleanParam.TestReset;
begin
  CheckEquals(False, fBoolP.Value, 'Default Value Set');

  fBoolP.Value := True;
  CheckEquals(True, fBoolP.Value, 'Set Value');

  fBoolP.Reset;
  CheckEquals(False, fBoolP.Value, 'Default Value Reset');
end;

procedure TestTD2XBooleanParam.TestSetFlag;
begin
  CheckFalse(fBoolP.Value, 'Check is Default');

  fFlag.Flag := True;
  CheckTrue(fBoolP.Value, 'Check is not Default');

  fFlag.Flag := False;
  CheckFalse(fBoolP.Value, 'Check is Default');
end;

procedure TestTD2XBooleanParam.TestToString;
begin
  CheckEqualsString('T-', fBoolP.ToString, 'Report Default Value');

  fBoolP.Value := True;
  CheckEqualsString('T+', fBoolP.ToString, 'Report Value');
end;

procedure TestTD2XBooleanParam.TestValue;
begin
  CheckEquals(False, fBoolP.Value, 'Default Value Set');

  fBoolP.Value := True;
  CheckEquals(True, fBoolP.Value, 'Value Set');
end;

procedure TestTD2XBooleanParam.TestZero;
begin
  CheckEquals(False, fBoolP.Value, 'Default Value Set');

  fBoolP.Value := True;
  CheckEquals(True, fBoolP.Value, 'Set Value');

  fBoolP.Zero;
  CheckEquals(False, fBoolP.Value, 'Value Zeroed');
end;

{ TestTD2XStringParam }

procedure TestTD2XStringParam.SetUp;
begin
  inherited;

  fStrP := TD2XStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param', 'Tst',
    nil, nil);
end;

procedure TestTD2XStringParam.TearDown;
begin
  FreeAndNil(fStrP);

  inherited;
end;

procedure TestTD2XStringParam.TestCreateStr;
begin
  FreeAndNil(fStrP);

  fStrP := TD2XStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param', 'Tst',
    function(pStr, pDflt: string; out pVal: string): Boolean
    begin
      pVal := pStr + pDflt;
      Result := True;
    end, nil);

  CheckEqualsString('Tst', fStrP.Value, 'Default Value Set');

  Check(fStrP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('Tst', fStrP.Value, 'Blank Value');
  CheckEqualsString('TTst', fStrP.ToString, 'Blank ToString');

  Check(fStrP.Parse('TSimple'), 'Parse right code with value');
  CheckEqualsString('SimpleTst', fStrP.Value, 'Simple Value');
  CheckEqualsString('TSimpleTst', fStrP.ToString, 'Simple ToString');
end;

procedure TestTD2XStringParam.TestDescribe;
begin
  fStrP.Describe(fLog);
  CheckLog('T<Example> Tst Test String Param', 'Describe Param');
end;

procedure TestTD2XStringParam.TestInvalidCreateParam;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XStringParam.CreateParam('', '', '', '', '', nil, nil, nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XStringParam.TestInvalidCreateReset;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<string>.Create('', '', '', '', nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XStringParam.TestIsDefault;
begin
  Check(fStrP.IsDefault, 'Check is Default');

  fStrP.Value := 'Simple';
  CheckFalse(fStrP.IsDefault, 'Check is not Default');

  fStrP.Reset;
  Check(fStrP.IsDefault, 'Check is Default');
end;

procedure TestTD2XStringParam.TestOutput;
begin
  fStrP.Output(fL);
  CheckList('', 'Output Default Value');

  fStrP.Value := 'Simple';
  fStrP.Output(fL);
  CheckList('-TSimple', 'Output Simple Value');
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
  fStrP.Report(fLog);
  CheckLog('Test Tst', 'Report Default Value');

  fStrP.Value := 'Simple';
  fStrP.Report(fLog);
  CheckLog('Test Simple', 'Report Simple Value');
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
  CheckEqualsString('TTst', fStrP.ToString, 'Check Default Value');

  fStrP.Value := 'Simple';
  CheckEqualsString('TSimple', fStrP.ToString, 'Check Simple Value');
end;

procedure TestTD2XStringParam.TestValue;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Check Default Value');

  fStrP.Value := 'Simple';
  CheckEqualsString('Simple', fStrP.Value, 'Check Simple Value');
end;

procedure TestTD2XStringParam.TestZero;
begin
  CheckEqualsString('Tst', fStrP.Value, 'Default Value Set');

  fStrP.Value := 'Simple';
  CheckEqualsString('Simple', fStrP.Value, 'Simple Value Set');

  fStrP.Zero;
  CheckEqualsString('', fStrP.Value, 'Value Zeroed');
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
  fPrm := TD2XParam.Create('T', 'Test', '', '', TstParser);

  fPrm.Describe(fLog);
  CheckLog('T', 'Describe');
end;

procedure TestTD2XParam.TestInvalidAllBlank;
begin
  StartExpectingException(EInvalidParam);
  try
    fPrm := TD2XParam.Create('', '', '', '', TstParser);
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
    fPrm := TD2XParam.Create('', 'Label', '', '', TstParser);
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
    fPrm := TD2XParam.Create('Code', '', '', '', TstParser);
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
  fPrm := TD2XParam.Create('T', 'Test', '', '', TstParser);

  CheckFalse(fPrm.IsCode('A'), 'Wrong code');

  Check(fPrm.IsCode('T'), 'Correct code');
end;

procedure TestTD2XParam.TestIsDefault;
begin
  fPrm := TD2XParam.Create('T', 'Test', '', '', TstParser);

  Check(fPrm.IsDefault, 'Default true');
end;

procedure TestTD2XParam.TestOutput;
begin
  fPrm := TD2XParam.Create('T', 'Test', '', '', TstParser);

  fPrm.Output(fL);
  CheckList('', 'Output');
end;

procedure TestTD2XParam.TestParse;
var
  lCalledParser: Boolean;
begin
  fPrm := TD2XParam.Create('T', 'Test', '', '',
    function(pStr: string): Boolean
    begin
      lCalledParser := True;
      Result := True;
    end);

  lCalledParser := False;
  CheckFalse(fPrm.Parse('A'), 'Wrong code');
  CheckFalse(lCalledParser, 'Wrong code didn''t call Parser');

  lCalledParser := False;
  Check(fPrm.Parse('T'), 'Correct code');
  Check(lCalledParser, 'Correct code called Parser');
end;

procedure TestTD2XParam.TestReport;
begin
  fPrm := TD2XParam.Create('T', 'Test', '', '', TstParser);

  fPrm.Report(fLog);
  CheckLog('', 'Report');
end;

procedure TestTD2XParam.TestToString;
begin
  fPrm := TD2XParam.Create('T', 'Test', '', '', TstParser);

  CheckEqualsString('T', fPrm.ToString, 'ToString');
end;

function TestTD2XParam.TstParser(pStr: string): Boolean;
begin
  Result := True;
end;

{ TestTD2XParams }

const
  REPORT_HEADING = 'Current option settings:';

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
const
  DESCRIPTION_PREFIX = 'Usage: Delphi2XmlTests ' +
    '[ Option | @Params | mFilename | Wildcard ] ... Options: Default Description';
  DESCRIPTION_SUFFIX = ' Definitions: ' +
    '<f/e> If value begins with "." is appended to global name to give file name';
begin
  fPs.DescribeAll(fLog);
  CheckLog(DESCRIPTION_PREFIX + DESCRIPTION_SUFFIX, 'Describe No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Test param', TstParser));
  fPs.DescribeAll(fLog);
  CheckLog(DESCRIPTION_PREFIX + ' T<tst> Test param' + DESCRIPTION_SUFFIX,
    'Describe One Param');

  fPs.Add(TD2XBooleanParam.CreateBool('B', 'Boolean', 'Boolean param'));
  fPs.DescribeAll(fLog);
  CheckLog(DESCRIPTION_PREFIX + ' T<tst> Test param B[+|-] - Boolean param' +
    DESCRIPTION_SUFFIX, 'Describe Two Params');
end;

procedure TestTD2XParams.TestForCode;
begin
  CheckFalse(Assigned(fPs.ForCode('T')), 'Code not found with No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Testing', TstParser));
  CheckFalse(Assigned(fPs.ForCode('Z')), 'Wrong Code not found with One Param');
  Check(Assigned(fPs.ForCode('T')), 'Code found with One Param');

  fPs.Add(TD2XBooleanParam.CreateBool('B', 'Boolean', 'Boolean param'));
  CheckFalse(Assigned(fPs.ForCode('Z')), 'Wrong Code not found with Two Params');
  Check(Assigned(fPs.ForCode('B')), 'Code found with Two Params');
  Check(Assigned(fPs.ForCode('T')), 'Code found with Two Params');
end;

procedure TestTD2XParams.TestOutputAll;
var
  lBP: TD2XBooleanParam;
begin
  fPs.OutputAll(fL);
  CheckList('', 'Output No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Testing', TstParser));
  fPs.OutputAll(fL);
  CheckList('', 'Output One Param');

  lBP := TD2XBooleanParam.CreateBool('B', 'Boolean', 'Boolean param');
  fPs.Add(lBP);
  fPs.OutputAll(fL);
  CheckList('', 'Output Two Default Params');

  lBP.Value := True;
  fPs.OutputAll(fL);
  CheckList('-B+', 'Output Two Changed Params');
end;

procedure TestTD2XParams.TestReportAll;
begin
  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING, 'Report No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Testing', TstParser));
  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING, 'Report One Param');

  fPs.Add(TD2XBooleanParam.CreateBool('B', 'Boolean', 'Boolean param'));
  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + ' Boolean -', 'Report Two Params');
end;

procedure TestTD2XParams.TestResetAll;
var
  lBP: TD2XBooleanParam;
  lSP: TD2XStringParam;
  lFP: TD2XFlaggedStringParam;
begin
  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Testing', TstParser));
  lBP := TD2XBooleanParam.CreateBool('B', 'Boolean', 'Boolean param');
  fPs.Add(lBP);
  lSP := TD2XStringParam.CreateStr('S', 'String', '<str>', 'String param', 'Str', nil, nil);
  fPs.Add(lSP);
  lFP := TD2XFlaggedStringParam.CreateFlagStr('F', 'Flagged', '<str>', 'String param', 'Flg',
    True, nil, nil, nil);
  fPs.Add(lFP);

  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + ' Boolean - String Str Flagged :Flg', 'All Params Default');

  lBP.Value := True;
  lSP.Value := 'Value';
  lFP.Value := 'Value';
  ID2XFlag(lFP).Flag := False;
  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + ' Boolean + String Value Flagged -(Value)', 'All Params Changed');

  fPs.ResetAll;

  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + ' Boolean - String Str Flagged :Flg', 'All Params Reset');
end;

procedure TestTD2XParams.TestZeroAll;
var
  lBP: TD2XBooleanParam;
  lSP: TD2XStringParam;
  lFP: TD2XFlaggedStringParam;
begin
  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Testing', TstParser));
  lBP := TD2XBooleanParam.CreateBool('B', 'Boolean', 'Boolean param');
  fPs.Add(lBP);
  lSP := TD2XStringParam.CreateStr('S', 'String', '<str>', 'String param', 'Str', nil, nil);
  fPs.Add(lSP);
  lFP := TD2XFlaggedStringParam.CreateFlagStr('F', 'Flagged', '<str>', 'String param', 'Flg',
    False, nil, nil, nil);
  fPs.Add(lFP);

  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + ' Boolean - String Str Flagged -(Flg)', 'All Params Default');

  lBP.Value := True;
  lSP.Value := 'Value';
  lFP.Value := 'Value';
  ID2XFlag(lFP).Flag := True;
  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + ' Boolean + String Value Flagged :Value', 'All Params Changed');

  fPs.ZeroAll;

  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + ' Boolean - String Flagged -', 'All Params Zeroed');
end;

function TestTD2XParams.TstParser(pStr: string): Boolean;
begin
  Result := True;
end;

{ TestTD2XValidStringParam }

procedure TestTD2XValidStringParam.SetUp;
begin
  fStrP := TD2XStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param', 'Tst', nil,
    function(pStr: string): Boolean
    begin
      Result := pStr > '';
    end);
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

procedure TestTD2XFormatStringParam.SetUp;
begin
  fStrP := TD2XStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param', 'Tst',
    function(pStr: string; pDflt: string; out pVal: string): Boolean
    begin
      Result := True;
      if pStr = '' then
        pVal := ''
      else
        if pStr[1] = ':' then
          pVal := Copy(pStr, 2, Length(pStr))
        else
          Result := False;
    end, nil);
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

procedure TestTD2XFormatValidStringParam.SetUp;
begin
  fStrP := TD2XStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param', 'Tst',
    function(pStr: string; pDflt: string; out pVal: string): Boolean
    begin
      Result := True;
      if pStr = '' then
        pVal := ''
      else
        if pStr[1] = ':' then
          pVal := Copy(pStr, 2, Length(pStr))
        else
          Result := False;
    end,
    function(pStr: string): Boolean
    begin
      Result := pStr > '';
    end);
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
  inherited;

  fFlagP := TTestFlaggedStringParam.CreateFlagStr('T', 'Test', '<Example>',
    'Test String Param', 'Tst', False, nil, nil, nil);
  fFlag := fFlagP;
end;

procedure TestTD2XFlaggedStringParam.TearDown;
begin
  fFlag := nil;
  FreeAndNil(fFlagP);

  inherited;
end;

procedure TestTD2XFlaggedStringParam.TestDescribe;
begin
  fFlagP.Describe(fLog);
  CheckLog('T[+-]:<Example> -(Tst) Test String Param', 'Describe Param');
end;

procedure TestTD2XFlaggedStringParam.TestFlag;
begin
  CheckEquals(False, ID2XFlag(fFlagP).Flag, 'Default Flag Set');

  ID2XFlag(fFlagP).Flag := True;
  CheckEquals(True, ID2XFlag(fFlagP).Flag, 'Other Flag Set');
end;

procedure TestTD2XFlaggedStringParam.TestGetFlag;
begin
  CheckFalse(fFlag.Flag, 'Check is Default');

  fFlagP.FlagValue := True;
  CheckTrue(fFlag.Flag, 'Check is not Default');

  fFlagP.Reset;
  CheckFalse(fFlag.Flag, 'Check is Default');
end;

procedure TestTD2XFlaggedStringParam.TestInvalidCreateParam;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XFlaggedStringParam.CreateParam('', '', '', '', '', nil, nil, nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XFlaggedStringParam.TestIsDefault;
begin
  Check(fFlagP.IsDefault, 'Check is Default');

  fFlagP.Value := '';
  CheckFalse(fFlagP.IsDefault, 'Check is not Default');

  ID2XFlag(fFlagP).Flag := True;
  CheckFalse(fFlagP.IsDefault, 'Check is not Default');

  fFlagP.Value := 'Tst';
  CheckFalse(fFlagP.IsDefault, 'Check is not Default');

  fFlagP.Value := 'Simple';
  CheckFalse(fFlagP.IsDefault, 'Check is not Default');

  fFlagP.Reset;
  Check(fFlagP.IsDefault, 'Check is Default');
end;

procedure TestTD2XFlaggedStringParam.TestOldFormatted;
begin
  StartExpectingException(EInvalidParam);
  try
    CheckEqualsString('Tst', fFlagP.OldFormatted('Tst'), 'Old Value Formatting');
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Incorrect call to TD2XFlaggedStringParam.FormatString', E.Message,
        'Exception message');
      raise;
    end;
  end;

end;

procedure TestTD2XFlaggedStringParam.TestOutput;
begin
  fFlagP.Output(fL);
  CheckList('', 'Output Default Value');

  fFlagP.Value := '';
  fFlagP.Output(fL);
  CheckList('-T-', 'Output Blank value off');

  ID2XFlag(fFlagP).Flag := True;
  fFlagP.Output(fL);
  CheckList('-T+', 'Output Blank value on');

  fFlagP.Value := 'Simple';
  fFlagP.Output(fL);
  CheckList('-T:Simple', 'Output Simple Value on');
end;

procedure TestTD2XFlaggedStringParam.TestParse;
begin
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Set');
  CheckEquals(False, ID2XFlag(fFlagP).Flag, 'Default Flag Set');

  Check(fFlagP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('Tst', fFlagP.Value, 'Remains default Value');
  CheckEquals(True, ID2XFlag(fFlagP).Flag, 'Blank Flag Set');

  Check(fFlagP.Parse('T-'), 'Parse right code with Flag off');
  CheckEqualsString('Tst', fFlagP.Value, 'Remains default Value');
  CheckEquals(False, ID2XFlag(fFlagP).Flag, 'Flag Off');

  Check(fFlagP.Parse('T+'), 'Parse right code with Flag on');
  CheckEqualsString('Tst', fFlagP.Value, 'Remains default Value');
  CheckEquals(True, ID2XFlag(fFlagP).Flag, 'Flag On');

  ID2XFlag(fFlagP).Flag := False;
  CheckEquals(False, ID2XFlag(fFlagP).Flag, 'Flag Unset');

  Check(fFlagP.Parse('T:Simple'), 'Parse right code with value');
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value');
  CheckEquals(True, ID2XFlag(fFlagP).Flag, 'Flag Set');

  Check(fFlagP.Parse('T-'), 'Parse right code with Flag off');
  CheckEqualsString('Simple', fFlagP.Value, 'Remains previous Value');
  CheckEquals(False, ID2XFlag(fFlagP).Flag, 'Flag Off');

  Check(fFlagP.Parse('T:'), 'Parse right code with no value');
  CheckEqualsString('', fFlagP.Value, 'Blank Value');
  CheckEquals(True, ID2XFlag(fFlagP).Flag, 'Flag Set');
end;

procedure TestTD2XFlaggedStringParam.TestReport;
begin
  fFlagP.Report(fLog);
  CheckLog('Test -(Tst)', 'Report Default Value');

  fFlagP.Value := '';
  fFlagP.Report(fLog);
  CheckLog('Test -', 'Report Blank value off');

  ID2XFlag(fFlagP).Flag := True;
  fFlagP.Report(fLog);
  CheckLog('Test +', 'Report Blank value on');

  fFlagP.Value := 'Simple';
  fFlagP.Report(fLog);
  CheckLog('Test :Simple', 'Report Simple Value on');
end;

procedure TestTD2XFlaggedStringParam.TestReset;
begin
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Set');
  CheckEquals(False, ID2XFlag(fFlagP).Flag, 'Default Flag Set');

  fFlagP.Value := 'Simple';
  ID2XFlag(fFlagP).Flag := True;
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value Set');
  CheckEquals(True, ID2XFlag(fFlagP).Flag, 'Other Flag Set');

  fFlagP.Reset;
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Reset');
  CheckEquals(False, ID2XFlag(fFlagP).Flag, 'Default Flag Reset');
end;

procedure TestTD2XFlaggedStringParam.TestSetFlag;
begin
  CheckFalse(fFlagP.FlagValue, 'Check is Default');

  fFlag.Flag := True;
  CheckTrue(fFlagP.FlagValue, 'Check is not Default');

  fFlag.Flag := False;
  CheckFalse(fFlagP.FlagValue, 'Check is Default');
end;

procedure TestTD2XFlaggedStringParam.TestToString;
begin
  CheckEqualsString('T-(Tst)', fFlagP.ToString, 'Check Default Value');

  fFlagP.Value := '';
  CheckEqualsString('T-', fFlagP.ToString, 'Blank value off');

  ID2XFlag(fFlagP).Flag := True;
  CheckEqualsString('T+', fFlagP.ToString, 'Blank value on');

  fFlagP.Value := 'Simple';
  CheckEqualsString('T:Simple', fFlagP.ToString, 'Check Simple Value');
end;

procedure TestTD2XFlaggedStringParam.TestValue;
begin
  CheckEqualsString('Tst', fFlagP.Value, 'Check Default Value');

  fFlagP.Value := 'Simple';
  CheckEqualsString('Simple', fFlagP.Value, 'Check Simple Value');
end;

procedure TestTD2XFlaggedStringParam.TestZero;
begin
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Set');
  CheckEquals(False, ID2XFlag(fFlagP).Flag, 'Default Flag Set');

  fFlagP.Value := 'Simple';
  ID2XFlag(fFlagP).Flag := True;
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value Set');
  CheckEquals(True, ID2XFlag(fFlagP).Flag, 'Other Flag Set');

  fFlagP.Zero;
  CheckEqualsString('', fFlagP.Value, 'Value Zeroed');
  CheckEquals(False, ID2XFlag(fFlagP).Flag, 'Flag Zeroed');
end;

{ TestFlagParam }

procedure TFlagParamTestCase.SetUp;
begin
  inherited;

  fFlag := nil;
end;

{ TestTD2XDefinesParam }

procedure TestTD2XDefinesParam.SetUp;
begin
  inherited;

  fDefP := TD2XDefinesParam.CreateDefines('T', 'Test',
    function(pFile: string): ID2XFile
    begin
      Result := TTestFile.Create('Config\' + pFile, True, 'Tango'#13#10'Uniform', nil);
    end);
  fFlag := fDefP as ID2XFlag;
  fDefP.Defines.CommaText := 'ALPHA,BETA,GAMMA';
end;

procedure TestTD2XDefinesParam.TearDown;
begin
  fFlag := nil;
  FreeAndNil(fDefP);

  inherited;
end;

procedure TestTD2XDefinesParam.TestDescribe;
begin
  fDefP.Describe(fLog);
  CheckLog('T[+-!:]<def> Add(+), Remove(-), Clear(!) or Load(:) Test', 'Describe Param');
end;

procedure TestTD2XDefinesParam.TestGetFlag;
begin
  CheckFalse(fFlag.Flag, 'Check is Default');

  fDefP.Value := True;
  CheckTrue(fFlag.Flag, 'Check is not Default');

  fDefP.Reset;
  CheckFalse(fFlag.Flag, 'Check is Default');
end;

procedure TestTD2XDefinesParam.TestInvalidCreateParam;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XDefinesParam.CreateParam('', '', '', '', False, nil, nil, nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XDefinesParam.TestIsDefault;
begin
  Check(fDefP.IsDefault, 'Check is Default');

  fDefP.Value := True;
  CheckFalse(fDefP.IsDefault, 'Check is not Default');

  fDefP.Reset;
  Check(fDefP.IsDefault, 'Check is Default');
end;

procedure TestTD2XDefinesParam.TestOutput;
begin
  fDefP.Output(fL);
  CheckList('', 'Report Default Value');

  fDefP.Value := True;
  fDefP.Output(fL);
  CheckList('-T: -T+ALPHA,BETA,GAMMA', 'Report Blank value on');
end;

procedure TestTD2XDefinesParam.TestParse;
begin
  CheckEquals(False, fDefP.Value, 'Default Value Set');
  fDefP.Report(fLog);
  CheckLog('Use default Test', 'Report Default value');

  CheckFalse(fDefP.Parse('T'), 'Parse right code with No value');
  CheckEquals(False, fDefP.Value, 'Blank Flag Set');
  fDefP.Report(fLog);
  CheckLog('Use default Test', 'Report Default value');

  CheckFalse(fDefP.Parse('T-'), 'Parse right code with Flag off');
  CheckEquals(False, fDefP.Value, 'Flag Off');
  fDefP.Report(fLog);
  CheckLog('Use default Test', 'Report Default value');

  CheckFalse(fDefP.Parse('T+'), 'Parse right code with Flag on');
  CheckEquals(False, fDefP.Value, 'Flag On');
  fDefP.Report(fLog);
  CheckLog('Use default Test', 'Report Default value');

  fDefP.Value := True;
  CheckEquals(True, ID2XFlag(fDefP).Flag, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Use these Test: ALPHA, BETA, GAMMA', 'Report simple values');

  Check(fDefP.Parse('T+Test'), 'Parse right code add value');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Use these Test: ALPHA, BETA, GAMMA, TEST', 'Report add value');

  Check(fDefP.Parse('T+Test1,Test2'), 'Parse right code add values');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Use these Test: ALPHA, BETA, GAMMA, TEST, TEST1, TEST2', 'Report add values');

  Check(fDefP.Parse('T-Beta'), 'Parse right code remove value');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Use these Test: ALPHA, GAMMA, TEST, TEST1, TEST2', 'Report remove value');

  Check(fDefP.Parse('T-Test,Test1'), 'Parse right code remove values');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Use these Test: ALPHA, GAMMA, TEST2', 'Report remove values');

  Check(fDefP.Parse('T+~Test'), 'Parse right code add file');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Use these Test: ALPHA, GAMMA, TANGO, TEST2, UNIFORM', 'Report add file');

  Check(fDefP.Parse('T-~Test'), 'Parse right code remove file');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Use these Test: ALPHA, GAMMA, TEST2', 'Report remove file');

  Check(fDefP.Parse('T:Test'), 'Parse right code set file');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Use these Test: TANGO, UNIFORM', 'Report set file');
end;

procedure TestTD2XDefinesParam.TestReport;
begin
  fDefP.Report(fLog);
  CheckLog('Use default Test', 'Report Default Value');

  fDefP.Value := True;
  fDefP.Report(fLog);
  CheckLog('Use these Test: ALPHA, BETA, GAMMA', 'Report Blank value on');
end;

procedure TestTD2XDefinesParam.TestReset;
begin
  CheckEquals(False, fDefP.Value, 'Default Value Set');

  fDefP.Value := True;
  CheckEquals(True, fDefP.Value, 'Simple Value Set');

  fDefP.Reset;
  CheckEquals(False, fDefP.Value, 'Default Value Reset');
end;

procedure TestTD2XDefinesParam.TestSetFlag;
begin
  CheckFalse(fDefP.Value, 'Check is Default');

  fFlag.Flag := True;
  CheckTrue(fDefP.Value, 'Check is not Default');

  fFlag.Flag := False;
  CheckFalse(fDefP.Value, 'Check is Default');
end;

procedure TestTD2XDefinesParam.TestToString;
begin
  CheckEqualsString('T', fDefP.ToString, 'Check Default Value');

  fDefP.Value := True;
  CheckEqualsString('T', fDefP.ToString, 'Blank value on');
end;

procedure TestTD2XDefinesParam.TestValue;
begin
  CheckEquals(False, ID2XFlag(fDefP).Flag, 'Default Flag Set');

  ID2XFlag(fDefP).Flag := True;
  CheckEquals(True, ID2XFlag(fDefP).Flag, 'Other Flag Set');
end;

procedure TestTD2XDefinesParam.TestZero;
begin
  CheckEquals(False, fDefP.Value, 'Default Value Set');

  fDefP.Value := True;
  CheckEquals(True, fDefP.Value, 'Simple Value Set');

  fDefP.Zero;
  CheckEquals(False, fDefP.Value, 'Value Zeroed');
end;

{ TTestFlaggedStringParam }

function TTestFlaggedStringParam.OldFormatted(pVal: string): string;
begin
  Result := Self.fFormatter(pVal);
end;

{ TestTD2XFlagsParam }

procedure TestTD2XFlagsParam.SetUp;
var
  lFlags: TD2XFlagDefines;
begin
  inherited;

  SetLength(lFlags, 2);
  lFlags[0] := FlagDef('1', 'Test1', 'Test Flag 1');
  lFlags[1] := FlagDef('2', 'Test2', 'Test Flag 2', True);

  fFlagsP := TD2XFlagsParam.CreateFlags(lFlags);
end;

procedure TestTD2XFlagsParam.TearDown;
begin
  FreeAndNil(fFlagsP);

  inherited;
end;

procedure TestTD2XFlagsParam.TestDescribe;
begin
  fFlagsP.Describe(fLog);
  CheckLog('F[[+|-|Code]*|:[[+|-]Label[+|-],]*] Flags Code Label Def Description ' +
    '1 Test1 - Test Flag 1 2 Test2 + Test Flag 2', 'Describe Param');
end;

procedure TestTD2XFlagsParam.TestInvalidCreate;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XFlagsParam.Create('', '', '', '', nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XFlagsParam.TestInvalidCreateFlags;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XFlagsParam.CreateFlags(nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to initialize some Flags', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XFlagsParam.TestIsDefault;
begin
  Check(fFlagsP.IsDefault, 'Check is Default');

  fFlagsP.ByCode['1'].Flag := True;
  CheckFalse(fFlagsP.IsDefault, 'Check is not Default');

  fFlagsP.Reset;
  Check(fFlagsP.IsDefault, 'Check is Default');
end;

procedure TestTD2XFlagsParam.TestOutput;
begin
  fFlagsP.Output(fL);
  CheckList('', 'Output Default Value');

  fFlagsP.ByCode['1'].Flag := True;
  fFlagsP.Output(fL);
  CheckList('-F! -F+1', 'Output True Value');

  fFlagsP.Zero;
  fFlagsP.Output(fL);
  CheckList('-F! -F-2', 'Output Zeroed Value');
end;

procedure TestTD2XFlagsParam.TestParse;
begin
  CheckFalse(fFlagsP.Parse('A'), 'Parse wrong code');

  CheckFalse(fFlagsP.Parse('F'), 'Parse right code with No value');

  CheckFalse(fFlagsP.Parse('F?'), 'Parse right code with Bad value');

  Check(fFlagsP.Parse('F+1'), 'Parse right code with True code');
  CheckEquals(True, fFlagsP.ByCode['1'].Flag, 'Frue Value');

  Check(fFlagsP.Parse('F-12'), 'Parse right code with multiple False codes');
  CheckEquals(False, fFlagsP.ByCode['1'].Flag, 'False 1 Value');
  CheckEquals(False, fFlagsP.ByCode['2'].Flag, 'False 2 Value');

  Check(fFlagsP.Parse('F+12'), 'Parse right code with multiple True codes');
  CheckEquals(True, fFlagsP.ByCode['1'].Flag, 'Frue 1 Value');
  CheckEquals(True, fFlagsP.ByCode['2'].Flag, 'Frue 2 Value');

  Check(fFlagsP.Parse('F-1'), 'Parse right code with False code');
  CheckEquals(False, fFlagsP.ByCode['1'].Flag, 'False Value');
end;

procedure TestTD2XFlagsParam.TestReport;
begin
  fFlagsP.Report(fLog);
  CheckLog('Flags Test1-,Test2+', 'Report Default Value');

  fFlagsP.ByCode['1'].Flag := True;
  fFlagsP.Report(fLog);
  CheckLog('Flags Test1+,Test2+', 'Report True Value');

  fFlagsP.Zero;
  fFlagsP.Report(fLog);
  CheckLog('Flags Test1-,Test2-', 'Report Zeroed Value');
end;

procedure TestTD2XFlagsParam.TestReset;
begin
  CheckEquals(False, fFlagsP.ByCode['1'].Flag, 'Default Value Set');
  CheckEquals(True, fFlagsP.ByCode['2'].Flag, 'Default Value Set');

  fFlagsP.ByCode['2'].Flag := False;
  CheckEquals(False, fFlagsP.ByCode['2'].Flag, 'Set Value');

  fFlagsP.Reset;
  CheckEquals(False, fFlagsP.ByCode['1'].Flag, 'Value Zeroed');
  CheckEquals(True, fFlagsP.ByCode['2'].Flag, 'Default Value Set');
end;

procedure TestTD2XFlagsParam.TestToString;
begin
  CheckEqualsString('F+2-1', fFlagsP.ToString, 'Report Default Value');

  fFlagsP.ByCode['1'].Flag := True;
  CheckEqualsString('F+12', fFlagsP.ToString, 'Report True Value');

  fFlagsP.Zero;
  CheckEqualsString('F-12', fFlagsP.ToString, 'Report Zeroed Value');
end;

procedure TestTD2XFlagsParam.TestZero;
begin
  CheckEquals(False, fFlagsP.ByCode['1'].Flag, 'Default Value Set');
  CheckEquals(True, fFlagsP.ByCode['2'].Flag, 'Default Value Set');

  fFlagsP.ByCode['1'].Flag := True;
  CheckEquals(True, fFlagsP.ByCode['1'].Flag, 'Set Value');

  fFlagsP.Zero;
  CheckEquals(False, fFlagsP.ByCode['1'].Flag, 'Value Zeroed');
  CheckEquals(False, fFlagsP.ByCode['2'].Flag, 'Default Value Set');
end;

function TestTD2XFlagsParam.TstParser(pStr: string): Boolean;
begin
  Result := True;
end;

initialization

RegisterTests('Params', [TestTD2XParam.Suite, TestTD2XParams.Suite, TestTD2XSingleParam.Suite,
  TestTD2XFlagsParam.Suite, TestTD2XBooleanParam.Suite, TestTD2XStringParam.Suite,
  TestTD2XValidStringParam.Suite, TestTD2XFormatStringParam.Suite,
  TestTD2XFormatValidStringParam.Suite, TestTD2XFlaggedStringParam.Suite,
  TestTD2XDefinesParam.Suite]);

end.
