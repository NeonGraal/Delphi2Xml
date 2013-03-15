unit Test.Params.Tests;

interface

implementation

uses
  D2X.Flag,
  D2X.Global,
  D2X.IO,
  D2X.Param,
  D2X.Params,
  System.Classes,
  System.Rtti,
  System.StrUtils,
  System.SysUtils,
  Test.Constants,
  Test.Global,
  Test.IO,
  Test.Param,
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
    procedure TestConvert;
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
    function TstFormatter(pVal: Boolean): string;

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
    procedure TestInvalidStrCreate;
    procedure TestInvalidBoolCreate;

    procedure TestInvalidAllNil;
    procedure TestInvalidNilConverter;
    procedure TestInvalidNilFormatter;
    procedure TestInvalidNilValidator;
    procedure TestInvalidNilOnSet;

    procedure TestInvalidAllBlank;
    procedure TestInvalidDefaultValue;

    procedure TestObjectParam;
    procedure TestStringParam;
    procedure TestBooleanParam;
    procedure TestBooleanCreates;

    procedure TestStringDfltParam;
    procedure TestBooleanDfltParam;
  end;

  TestTD2XSingleParamEnum = class(TLoggerTestCase)
  published
    procedure TestParseModeParam;
    procedure TestElapsedModeParam;
    procedure TestResultPerParam;
  end;

  TestTD2XFlagsParam = class(TLoggerTestCase)
  strict private
    fFlagsP: TD2XFlagsParam;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestInvalidCreate;
    procedure TestInvalidCreateFlags;

    procedure TestParse;
    procedure TestParseCode;
    procedure TestParseLabel;
    procedure TestReset;
    procedure TestZero;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestOutput;
    procedure TestToString;
    procedure TestIsDefault;
  end;

  TestTD2XListParam = class(TLoggerTestCase)
  strict private
    fListP: TD2XListParam;

    procedure CheckListParam(pExp, pLbl: string);

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestInvalidCreate;

    procedure TestParse;
    procedure TestReset;
    procedure TestZero;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestOutput;
    procedure TestToString;
    procedure TestIsDefault;
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
    procedure TestInvalidCreateParamValid;
    procedure TestInvalidCreateParamOnSet;
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

  TestTD2XValidStringParam = class(TBaseTestCase)
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

  TestTD2XFormatValidStringParam = class(TBaseTestCase)
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
    procedure TestInvalidCreateParamValid;
    procedure TestInvalidCreateParamOnSet;
    procedure TestOldFormatted;

    procedure TestParse;
    procedure TestParseReset;
    procedure TestReset;
    procedure TestZero;
    procedure TestConvert;
    procedure TestIsDefault;

    procedure TestDescribe;
    procedure TestReport;
    procedure TestOutput;
    procedure TestFlag;
    procedure TestValue;
    procedure TestToString;
    procedure TestGetFlag; override;
    procedure TestSetFlag; override;
  end;

  TestTD2XFlaggedStringConvertParam = class(TFlagParamTestCase)
  strict private
    fFlagP: TTestFlaggedStringParam;

    function TestConverter(pStr: string; pDflt: string; out pVal: string): Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestParse;
    procedure TestParseReset;
    procedure TestReset;
    procedure TestZero;
    procedure TestConvert;
    procedure TestIsDefault;
    procedure TestGetFlag; override;
    procedure TestSetFlag; override;
  end;

  TestTD2XFlaggedStringFormatParam = class(TFlagParamTestCase)
  strict private
    fFlagP: TTestFlaggedStringParam;

    function FormatFlagString(pFlag: Boolean; pVal: string): string;
  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestDescribe;
    procedure TestReport;
    procedure TestOutput;
    procedure TestToString;
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
    procedure TestParseInvalid;
    procedure TestParseReset;
    procedure TestReset;
    procedure TestZero;
    procedure TestIsDefault;

    procedure TestDescribe;
    procedure TestReport;
    procedure TestOutput;
    procedure TestValue;
    procedure TestToString;
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

procedure TestTD2XSingleParam.TestBooleanCreates;
var
  lBoolP: TD2XSingleParam<Boolean>;
  lCalled: Boolean;
begin
  lCalled := False;
  lBoolP := TD2XSingleParam<Boolean>.CreateParamValid('T', 'Test', '', '', False,
      function(pStr: string; pDflt: Boolean; out pVal: Boolean): Boolean
    begin
      pVal := False;
      Result := True;
    end, FmtBoolProp,
    function(pVal: Boolean): Boolean
    begin
      Result := True;
      lCalled := True;
    end);
  try
    Check(lCalled, 'Validity Called');
  finally
    FreeAndNil(lBoolP);
  end;

  lCalled := False;
  lBoolP := TD2XSingleParam<Boolean>.CreateParamOnSet('T', 'Test', '', '', False,
    function(pStr: string; pDflt: Boolean; out pVal: Boolean): Boolean
    begin
      pVal := False;
      Result := True;
    end, FmtBoolProp,
    procedure(pOld, pVal, pDflt: Boolean)
    begin
      lCalled := True;
    end);
  try
    Check(lCalled, 'On Set Called');
  finally
    FreeAndNil(lBoolP);
  end;
end;

procedure TestTD2XSingleParam.TestBooleanDfltParam;
var
  lBoolP: TD2XSingleParam<Boolean>;
begin
  lBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '', '', True,
    TD2X.CnvDflt<Boolean>, FmtBoolProp);

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
    end, FmtBoolProp);

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

    lBoolP.Convert('+');
    CheckEquals(False, lBoolP.Value, 'Returned value');
    CheckEqualsString('T-', lBoolP.ToString, 'String representation');
  finally
    FreeAndNil(lBoolP);
  end;
end;

procedure TestTD2XSingleParam.TestInvalidAllBlank;
begin
  CheckInvalidParam('Code cannot be blank', 'TD2XSingleParam<string>.CreateParam',
    procedure
    begin
      TD2XSingleParam<string>.CreateParam('', '', '', '', '', TD2X.CnvDflt<string>,
        FmtStrProp);
    end);
end;

procedure TestTD2XSingleParam.TestInvalidDefaultValue;
begin
  CheckInvalidParam('Invalid default value', 'TD2XSingleParam<string>.CreateParam',
    procedure
    begin
      TD2XSingleParam<string>.CreateParamValid('Code', 'Label', '', '', '',
        TD2X.CnvDflt<string>, FmtStrProp, InvStrProp);
    end);
end;

procedure TestTD2XSingleParam.TestInvalidAllNil;
begin
  CheckInvalidParam('Converter invalid', 'TD2XSingleParam<string>.CreateParam',
    procedure
    begin
      TD2XSingleParam<string>.CreateParam('', '', '', '', '', nil, nil);
    end);
end;

procedure TestTD2XSingleParam.TestInvalidBoolCreate;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XSingleParam<Boolean>.Create',
    procedure
    begin
      TD2XSingleParam<Boolean>.Create('', '', '', '', nil);
    end);
end;

procedure TestTD2XSingleParam.TestInvalidNilConverter;
begin
  CheckInvalidParam('Converter invalid', 'TD2XSingleParam<string>.CreateParam',
    procedure
    begin
      TD2XSingleParam<string>.CreateParam('Code', 'Label', '', '', '', nil, FmtStrProp);
    end);
end;

procedure TestTD2XSingleParam.TestInvalidNilFormatter;
begin
  CheckInvalidParam('Formatter invalid', 'TD2XSingleParam<string>.CreateParam',
    procedure
    begin
      TD2XSingleParam<string>.CreateParam('Code', 'Label', '', '', '',
        TD2X.CnvDflt<string>, nil);
    end);
end;

procedure TestTD2XSingleParam.TestInvalidNilOnSet;
begin
  CheckInvalidParam('OnSet invalid', 'TD2XSingleParam<string>.CreateParam',
    procedure
    begin
      TD2XSingleParam<string>.CreateParamOnSet('Code', 'Label', '', '', '',
        TD2X.CnvDflt<string>, FmtStrProp, nil);
    end);
end;

procedure TestTD2XSingleParam.TestInvalidNilValidator;
begin
  CheckInvalidParam('Validator invalid', 'TD2XSingleParam<string>.CreateParam',
    procedure
    begin
      TD2XSingleParam<string>.CreateParamValid('Code', 'Label', '', '', '',
        TD2X.CnvDflt<string>, FmtStrProp, nil);
    end);
end;

procedure TestTD2XSingleParam.TestInvalidStrCreate;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XSingleParam<string>.Create',
    procedure
    begin
      TD2XSingleParam<string>.Create('', '', '', '', nil);
    end);
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
    end);

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
    TD2X.CnvDflt<string>, FmtStrProp);

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
    end, FmtStrProp);

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

    lStrP.Convert('Simple');
    CheckEqualsString('', lStrP.Value, 'Returned value');
    CheckEqualsString('T', lStrP.ToString, 'String representation');
  finally
    FreeAndNil(lStrP);
  end;
end;

{ TestTD2XStringParam }

procedure TestTD2XStringParam.SetUp;
begin
  inherited;

  fStrP := TD2XStringParam.CreateStr('T', 'Test', '<Example>', 'Test String Param',
    'Tst', nil);
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
    end);

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
  CheckInvalidParam('Need to use correct constructor', 'TD2XStringParam.CreateParam',
    procedure
    begin
      TD2XStringParam.CreateParam('', '', '', '', '', nil, nil);
    end);
end;

procedure TestTD2XStringParam.TestInvalidCreateParamOnSet;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XStringParam.CreateParam',
    procedure
    begin
      TD2XStringParam.CreateParamOnSet('', '', '', '', '', nil, nil, nil);
    end);
end;

procedure TestTD2XStringParam.TestInvalidCreateParamValid;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XStringParam.CreateParam',
    procedure
    begin
      TD2XStringParam.CreateParamValid('', '', '', '', '', nil, nil, nil);
    end);
end;

procedure TestTD2XStringParam.TestInvalidCreateReset;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XSingleParam<string>.Create',
    procedure
    begin
      TD2XSingleParam<string>.Create('', '', '', '', nil);
    end);
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

procedure TestTD2XParam.TestConvert;
begin
  fPrm := TD2XParam.Create('T', 'Test', '', '', TstParser);

  fPrm.Convert('Test');
  CheckEqualsString('T', fPrm.ToString, 'ToString');
end;

procedure TestTD2XParam.TestDescribe;
begin
  fPrm := TD2XParam.Create('T', 'Test', '', '', TstParser);

  fPrm.Describe(fLog);
  CheckLog('T', 'Describe');
end;

procedure TestTD2XParam.TestInvalidAllBlank;
begin
  CheckInvalidParam('Code cannot be blank', 'TD2XSingleParam<string>.CreateParam',
    procedure
    begin
      TD2XParam.Create('', '', '', '', TstParser);
    end);
end;

procedure TestTD2XParam.TestInvalidBlankCode;
begin
  CheckInvalidParam('Code cannot be blank', 'TD2XSingleParam<string>.CreateParam',
    procedure
    begin
      TD2XParam.Create('', 'Label', '', '', TstParser);
    end);
end;

procedure TestTD2XParam.TestInvalidBlankLabel;
begin
  CheckInvalidParam('Label cannot be blank', 'TD2XSingleParam<string>.CreateParam',
    procedure
    begin
      TD2XParam.Create('Code', '', '', '', TstParser);
    end);
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
  fPs.DescribeAll(fLog);
  CheckLog(UsageDescription + DESCRIPTION_SUFFIX, 'Describe No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Test param', TstParser));
  fPs.DescribeAll(fLog);
  CheckLog(UsageDescription + ' T<tst> Test param' + DESCRIPTION_SUFFIX, 'Describe One Param');

  fPs.Add(TD2XSingleParam<Boolean>.CreateParam('B', 'Boolean', '[+|-]', 'Boolean param', False,
    TD2X.CnvDflt<Boolean>, TstFormatter));
  fPs.DescribeAll(fLog);
  CheckLog(UsageDescription + ' T<tst> Test param B[+|-] - Boolean param' + DESCRIPTION_SUFFIX,
    'Describe Two Params');
end;

procedure TestTD2XParams.TestForCode;
begin
  CheckFalse(Assigned(fPs.ForCode('T')), 'Code not found with No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Testing', TstParser));
  CheckFalse(Assigned(fPs.ForCode('Z')), 'Wrong Code not found with One Param');
  Check(Assigned(fPs.ForCode('T')), 'Code found with One Param');

  fPs.Add(TD2XSingleParam<Boolean>.CreateParam('B', 'Boolean', '[+|-]', 'Boolean param', False,
    TD2X.CnvDflt<Boolean>, TstFormatter));
  CheckFalse(Assigned(fPs.ForCode('Z')), 'Wrong Code not found with Two Params');
  Check(Assigned(fPs.ForCode('B')), 'Code found with Two Params');
  Check(Assigned(fPs.ForCode('T')), 'Code found with Two Params');
end;

procedure TestTD2XParams.TestOutputAll;
var
  lBP: TD2XSingleParam<Boolean>;
begin
  fPs.OutputAll(fL);
  CheckList('', 'Output No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Testing', TstParser));
  fPs.OutputAll(fL);
  CheckList('', 'Output One Param');

  lBP := TD2XSingleParam<Boolean>.CreateParam('B', 'Boolean', '[+|-]', 'Boolean param', False,
    TD2X.CnvDflt<Boolean>, TstFormatter);
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
  CheckLog(Trim(REPORT_HEADING), 'Report No Params');

  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Testing', TstParser));
  fPs.ReportAll(fLog);
  CheckLog(Trim(REPORT_HEADING), 'Report One Param');

  fPs.Add(TD2XSingleParam<Boolean>.CreateParam('B', 'Boolean', '[+|-]', 'Boolean param', False,
    TD2X.CnvDflt<Boolean>, TstFormatter));
  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + 'Boolean -', 'Report Two Params');
end;

procedure TestTD2XParams.TestResetAll;
var
  lBP: TD2XSingleParam<Boolean>;
  lSP: TD2XStringParam;
  lFP: TD2XFlaggedStringParam;
begin
  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Testing', TstParser));
  lBP := TD2XSingleParam<Boolean>.CreateParam('B', 'Boolean', '[+|-]', 'Boolean param', False,
    TD2X.CnvDflt<Boolean>, TstFormatter);
  fPs.Add(lBP);
  lSP := TD2XStringParam.CreateStr('S', 'String', '<str>', 'String param', 'Str', nil);
  fPs.Add(lSP);
  lFP := TD2XFlaggedStringParam.CreateFlagStr('F', 'Flagged', '<str>', 'String param', 'Flg',
    True, nil);
  fPs.Add(lFP);

  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + 'Boolean - String Str Flagged :Flg', 'All Params Default');

  lBP.Value := True;
  lSP.Value := 'Value';
  lFP.Value := 'Value';
  lFP.FlagValue := False;
  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + 'Boolean + String Value Flagged -(Value)', 'All Params Changed');

  fPs.ResetAll;

  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + 'Boolean - String Str Flagged :Flg', 'All Params Reset');
end;

procedure TestTD2XParams.TestZeroAll;
var
  lBP: TD2XSingleParam<Boolean>;
  lSP: TD2XStringParam;
  lFP: TD2XFlaggedStringParam;
begin
  fPs.Add(TD2XParam.Create('T', 'Test', '<tst>', 'Testing', TstParser));
  lBP := TD2XSingleParam<Boolean>.CreateParam('B', 'Boolean', '[+|-]', 'Boolean param', False,
    TD2X.CnvDflt<Boolean>, TstFormatter);
  fPs.Add(lBP);
  lSP := TD2XStringParam.CreateStr('S', 'String', '<str>', 'String param', 'Str', nil);
  fPs.Add(lSP);
  lFP := TD2XFlaggedStringParam.CreateFlagStr('F', 'Flagged', '<str>', 'String param', 'Flg',
    False, nil);
  fPs.Add(lFP);

  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + 'Boolean - String Str Flagged -(Flg)', 'All Params Default');

  lBP.Value := True;
  lSP.Value := 'Value';
  lFP.Value := 'Value';
  ID2XFlag(lFP).Flag := True;
  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + 'Boolean + String Value Flagged :Value', 'All Params Changed');

  fPs.ZeroAll;

  fPs.ReportAll(fLog);
  CheckLog(REPORT_HEADING + 'Boolean - String Flagged -', 'All Params Zeroed');
end;

function TestTD2XParams.TstFormatter(pVal: Boolean): string;
begin
  Result := IfThen(pVal, '+', '-');
end;

function TestTD2XParams.TstParser(pStr: string): Boolean;
begin
  Result := True;
end;

{ TestTD2XValidStringParam }

procedure TestTD2XValidStringParam.SetUp;
begin
  fStrP := TD2XStringParam.CreateStrValid('T', 'Test', '<Example>', 'Test String Param',
    'Tst', nil,
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

  CheckInvalidParam('Invalid value', 'TD2XStringParam.Value',
    procedure
    begin
      fStrP.Value := '';
    end);
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
    end);
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
  fStrP := TD2XStringParam.CreateStrValid('T', 'Test', '<Example>', 'Test String Param', 'Tst',
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

  CheckInvalidParam('Invalid value', 'TD2XStringParam.Value',
    procedure
    begin
      fStrP.Value := '';
    end);
end;

{ TestTD2XFlaggedStringParam }

procedure TestTD2XFlaggedStringParam.SetUp;
begin
  inherited;

  fFlagP := TTestFlaggedStringParam.CreateFlagStr('T', 'Test', '<Example>',
    'Test String Param', 'Tst', False, nil);
  AssignFlag(fFlagP);
end;

procedure TestTD2XFlaggedStringParam.TearDown;
begin
  ClearFlag(fFlagP);

  inherited;
end;

procedure TestTD2XFlaggedStringParam.TestConvert;
begin
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Set');
  CheckFlag(False, 'Default Flag Set');

  fFlagP.Convert('Simple');
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value Set');
  CheckFlag(False, 'Other Flag Set');
end;

procedure TestTD2XFlaggedStringParam.TestDescribe;
begin
  fFlagP.Describe(fLog);
  CheckLog('T[+-]:<Example> -(Tst) Test String Param', 'Describe Param');
end;

procedure TestTD2XFlaggedStringParam.TestFlag;
begin
  CheckFlag(False, 'Default Flag Set');

  SetFlag(True);
  CheckFlag(True, 'Other Flag Set');
end;

procedure TestTD2XFlaggedStringParam.TestGetFlag;
begin
  inherited;

  CheckFlag(False, 'Check is Default');

  fFlagP.FlagValue := True;
  CheckFlag(True, 'Check is not Default');

  fFlagP.Reset;
  CheckFlag(False, 'Check is Default');
end;

procedure TestTD2XFlaggedStringParam.TestInvalidCreateParam;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XFlaggedStringParam.CreateParam',
    procedure
    begin
      TD2XFlaggedStringParam.CreateParam('', '', '', '', '', nil, nil);
    end);
end;

procedure TestTD2XFlaggedStringParam.TestInvalidCreateParamOnSet;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XFlaggedStringParam.CreateParam',
    procedure
    begin
      TD2XFlaggedStringParam.CreateParamOnSet('', '', '', '', '', nil, nil, nil);
    end);
end;

procedure TestTD2XFlaggedStringParam.TestInvalidCreateParamValid;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XFlaggedStringParam.CreateParam',
    procedure
    begin
      TD2XFlaggedStringParam.CreateParamValid('', '', '', '', '', nil, nil, nil);
    end);
end;

procedure TestTD2XFlaggedStringParam.TestIsDefault;
begin
  Check(fFlagP.IsDefault, 'Check is Default');

  fFlagP.Value := '';
  CheckFalse(fFlagP.IsDefault, 'Check is not Default');

  SetFlag(True);
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
  CheckInvalidParam('Incorrect call to TD2XFlaggedStringParam.FormatString',
    'TTestFlaggedStringParam.OldFormatted',
    procedure
    begin
      CheckEqualsString('Tst', fFlagP.OldFormatted('Tst'), 'Old Value Formatting');
    end);
end;

procedure TestTD2XFlaggedStringParam.TestOutput;
begin
  fFlagP.Output(fL);
  CheckList('', 'Output Default Value');

  fFlagP.Value := '';
  fFlagP.Output(fL);
  CheckList('-T-', 'Output Blank value off');

  SetFlag(True);
  fFlagP.Output(fL);
  CheckList('-T+', 'Output Blank value on');

  fFlagP.Value := 'Simple';
  fFlagP.Output(fL);
  CheckList('-T:Simple', 'Output Simple Value on');
end;

procedure TestTD2XFlaggedStringParam.TestParse;
begin
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Set');
  CheckFlag(False, 'Default Flag Set');

  Check(fFlagP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('Tst', fFlagP.Value, 'Remains default Value');
  CheckFlag(True, 'Blank Flag Set');

  Check(fFlagP.Parse('T-'), 'Parse right code with Flag off');
  CheckEqualsString('Tst', fFlagP.Value, 'Remains default Value');
  CheckFlag(False, 'Flag Off');

  Check(fFlagP.Parse('T+'), 'Parse right code with Flag on');
  CheckEqualsString('Tst', fFlagP.Value, 'Remains default Value');
  CheckFlag(True, 'Flag On');

  SetFlag(False);
  CheckFlag(False, 'Flag Unset');

  Check(fFlagP.Parse('T:Simple'), 'Parse right code with value');
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value');
  CheckFlag(True, 'Flag Set');

  Check(fFlagP.Parse('T-'), 'Parse right code with Flag off');
  CheckEqualsString('Simple', fFlagP.Value, 'Remains previous Value');
  CheckFlag(False, 'Flag Off');

  Check(fFlagP.Parse('T:'), 'Parse right code with no value');
  CheckEqualsString('', fFlagP.Value, 'Blank Value');
  CheckFlag(True, 'Flag Set');
end;

procedure TestTD2XFlaggedStringParam.TestParseReset;
begin
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Set');
  CheckFlag(False, 'Default Flag Set');

  SetFlag(False);
  fFlagP.Value := 'Simple';
  CheckFlag(False, 'Flag Unset');
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value');

  Check(fFlagP.Parse('T!'), 'Parse right code with reset');
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Reset');
  CheckFlag(False, 'Default Flag Reset');
end;

procedure TestTD2XFlaggedStringParam.TestReport;
begin
  fFlagP.Report(fLog);
  CheckLog('Test -(Tst)', 'Report Default Value');

  fFlagP.Value := '';
  fFlagP.Report(fLog);
  CheckLog('Test -', 'Report Blank value off');

  SetFlag(True);
  fFlagP.Report(fLog);
  CheckLog('Test +', 'Report Blank value on');

  fFlagP.Value := 'Simple';
  fFlagP.Report(fLog);
  CheckLog('Test :Simple', 'Report Simple Value on');
end;

procedure TestTD2XFlaggedStringParam.TestReset;
begin
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Set');
  CheckFlag(False, 'Default Flag Set');

  fFlagP.Value := 'Simple';
  SetFlag(True);
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value Set');
  CheckFlag(True, 'Other Flag Set');

  fFlagP.Reset;
  CheckEqualsString('Tst', fFlagP.Value, 'Default Value Reset');
  CheckFlag(False, 'Default Flag Reset');
end;

procedure TestTD2XFlaggedStringParam.TestSetFlag;
begin
  inherited;

  CheckFalse(fFlagP.FlagValue, 'Check is Default');

  SetFlag(True);
  CheckTrue(fFlagP.FlagValue, 'Check is not Default');

  SetFlag(False);
  CheckFalse(fFlagP.FlagValue, 'Check is Default');
end;

procedure TestTD2XFlaggedStringParam.TestToString;
begin
  CheckEqualsString('T-(Tst)', fFlagP.ToString, 'Check Default Value');

  fFlagP.Value := '';
  CheckEqualsString('T-', fFlagP.ToString, 'Blank value off');

  SetFlag(True);
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
  CheckFlag(False, 'Default Flag Set');

  fFlagP.Value := 'Simple';
  SetFlag(True);
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value Set');
  CheckFlag(True, 'Other Flag Set');

  fFlagP.Zero;
  CheckEqualsString('', fFlagP.Value, 'Value Zeroed');
  CheckFlag(False, 'Flag Zeroed');
end;

{ TestTD2XDefinesParam }

procedure TestTD2XDefinesParam.SetUp;
begin
  inherited;

  fDefP := TD2XDefinesParam.CreateDefines('T', 'Test',
    function(pFile: string): ID2XIOFile
    begin
      Result := TTestFile.Create('Config\' + pFile, True, 'Tango'#13#10'Uniform', nil);
    end);
  AssignFlag(fDefP);
  fDefP.Defines.CommaText := 'ALPHA,BETA,GAMMA';
end;

procedure TestTD2XDefinesParam.TearDown;
begin
  ClearFlag(fDefP);

  inherited;
end;

procedure TestTD2XDefinesParam.TestDescribe;
begin
  fDefP.Describe(fLog);
  CheckLog('T[+-!:]<def> Add(+), Remove(-), Clear(!) or Load(:) Test', 'Describe Param');
end;

procedure TestTD2XDefinesParam.TestGetFlag;
begin
  inherited;

  CheckFlag(False, 'Check is Default');

  fDefP.Value := True;
  CheckFlag(True, 'Check is not Default');

  fDefP.Reset;
  CheckFlag(False, 'Check is Default');
end;

procedure TestTD2XDefinesParam.TestInvalidCreateParam;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XDefinesParam.CreateParam',
    procedure
    begin
      TD2XDefinesParam.CreateParam('', '', '', '', False, nil, nil);
    end);
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
  CheckLog('Test Default', 'Report Default value');

  fDefP.Value := True;
  CheckFlag(True, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Test ALPHA, BETA, GAMMA', 'Report simple values');

  Check(fDefP.Parse('T+Test'), 'Parse right code add value');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Test ALPHA, BETA, GAMMA, TEST', 'Report add value');

  Check(fDefP.Parse('T+Test1,Test2'), 'Parse right code add values');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Test ALPHA, BETA, GAMMA, TEST, TEST1, TEST2', 'Report add values');

  Check(fDefP.Parse('T-Beta'), 'Parse right code remove value');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Test ALPHA, GAMMA, TEST, TEST1, TEST2', 'Report remove value');

  Check(fDefP.Parse('T-Test,Test1'), 'Parse right code remove values');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Test ALPHA, GAMMA, TEST2', 'Report remove values');

  Check(fDefP.Parse('T+~Test'), 'Parse right code add file');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Test ALPHA, GAMMA, TANGO, TEST2, UNIFORM', 'Report add file');

  Check(fDefP.Parse('T-~Test'), 'Parse right code remove file');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Test ALPHA, GAMMA, TEST2', 'Report remove file');

  Check(fDefP.Parse('T:Test'), 'Parse right code set file');
  CheckEquals(True, fDefP.Value, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Test TANGO, UNIFORM', 'Report set file');
end;

procedure TestTD2XDefinesParam.TestParseInvalid;
begin
  CheckEquals(False, fDefP.Value, 'Default Value Set');
  fDefP.Report(fLog);
  CheckLog('Test Default', 'Report Default value');

  CheckFalse(fDefP.Parse('T'), 'Parse right code with No value');
  CheckEquals(False, fDefP.Value, 'Blank Flag Set');
  fDefP.Report(fLog);
  CheckLog('Test Default', 'Report Default value');

  CheckFalse(fDefP.Parse('T-'), 'Parse right code with Flag off');
  CheckEquals(False, fDefP.Value, 'Flag Off');
  fDefP.Report(fLog);
  CheckLog('Test Default', 'Report Default value');

  CheckFalse(fDefP.Parse('T+'), 'Parse right code with Flag on');
  CheckEquals(False, fDefP.Value, 'Flag On');
  fDefP.Report(fLog);
  CheckLog('Test Default', 'Report Default value');
end;

procedure TestTD2XDefinesParam.TestParseReset;
begin
  CheckEquals(False, fDefP.Value, 'Default Value Set');
  fDefP.Report(fLog);
  CheckLog('Test Default', 'Report Default value');

  fDefP.Value := True;
  CheckFlag(True, 'Flag Set');
  fDefP.Report(fLog);
  CheckLog('Test ALPHA, BETA, GAMMA', 'Report simple values');

  CheckTrue(fDefP.Parse('T!'), 'Parse right code with Reset');
  CheckEquals(False, fDefP.Value, 'Flag Off');
  fDefP.Report(fLog);
  CheckLog('Test Default', 'Report Reset value');
end;

procedure TestTD2XDefinesParam.TestReport;
begin
  fDefP.Report(fLog);
  CheckLog('Test Default', 'Report Default Value');

  fDefP.Value := True;
  fDefP.Report(fLog);
  CheckLog('Test ALPHA, BETA, GAMMA', 'Report Blank value on');
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
  inherited;

  CheckFalse(fDefP.Value, 'Check is Default');

  SetFlag(True);
  CheckTrue(fDefP.Value, 'Check is not Default');

  SetFlag(False);
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
  CheckFlag(False, 'Default Flag Set');

  SetFlag(True);
  CheckFlag(True, 'Other Flag Set');
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

  SetLength(lFlags, 4);
  lFlags[0] := FlagDef('1', 'Test1', 'Test Flag 1');
  lFlags[1] := FlagDef('2', 'Test2', 'Test Flag 2', True);
  lFlags[2] := FlagDef('U', 'TestU', 'Test Flag Upper');
  lFlags[3] := FlagDef('l', 'Testl', 'Test Flag Lower');

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
  CheckLog('F<codes> | :<labels> Flags 1 Test1 - Test Flag 1 2 Test2 + Test Flag 2 ' +
    'U TestU - Test Flag Upper L Testl - Test Flag Lower', 'Describe Param');
end;

procedure TestTD2XFlagsParam.TestInvalidCreate;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XFlagsParam.Create',
    procedure
    begin
      TD2XFlagsParam.Create('', '', '', '', nil);
    end);
end;

procedure TestTD2XFlagsParam.TestInvalidCreateFlags;
begin
  CheckInvalidParam('Need to initialize some Flags', 'TD2XFlagsParam.CreateFlags',
    procedure
    begin
      TD2XFlagsParam.CreateFlags(nil);
    end);
end;

procedure TestTD2XFlagsParam.TestIsDefault;
begin
  Check(fFlagsP.IsDefault, 'Check is Default');

  fFlagsP.SetCode('1', True);
  CheckFalse(fFlagsP.IsDefault, 'Check is not Default');

  fFlagsP.Reset;
  Check(fFlagsP.IsDefault, 'Check is Default');
end;

procedure TestTD2XFlagsParam.TestOutput;
begin
  fFlagsP.Output(fL);
  CheckList('', 'Output Default Value');

  fFlagsP.SetCode('1', True);
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
end;

procedure TestTD2XFlagsParam.TestParseCode;
begin
  Check(fFlagsP.Parse('F+1'), 'Parse right code with True code');
  CheckEquals(True, fFlagsP.RefCode['1'](), 'Frue Value');

  Check(fFlagsP.Parse('F-12'), 'Parse right code with multiple False codes');
  CheckEquals(False, fFlagsP.RefCode['1'](), 'False 1 Value');
  CheckEquals(False, fFlagsP.RefCode['2'](), 'False 2 Value');

  Check(fFlagsP.Parse('F+12'), 'Parse right code with multiple True codes');
  CheckEquals(True, fFlagsP.RefCode['1'](), 'Frue 1 Value');
  CheckEquals(True, fFlagsP.RefCode['2'](), 'Frue 2 Value');

  Check(fFlagsP.Parse('F-1'), 'Parse right code with False code');
  CheckEquals(False, fFlagsP.RefCode['1'](), 'False Value');

  Check(fFlagsP.Parse('F+u'), 'Parse right code with Lowercase code');
  CheckEquals(True, fFlagsP.RefCode['u'](), 'True Lowercase Value');
  CheckEquals(True, fFlagsP.RefCode['U'](), 'True Uppercase Value');

  Check(fFlagsP.Parse('F-U'), 'Parse right code with Uppercase code');
  CheckEquals(False, fFlagsP.RefCode['u'](), 'False Lowercase Value');
  CheckEquals(False, fFlagsP.RefCode['U'](), 'False Uppercase Value');

  Check(fFlagsP.Parse('F+l'), 'Parse right code with Lowercase code');
  CheckEquals(True, fFlagsP.RefCode['l'](), 'True Lowercase Value');
  CheckEquals(True, fFlagsP.RefCode['L'](), 'True Uppercase Value');

  Check(fFlagsP.Parse('F-L'), 'Parse right code with Uppercase code');
  CheckEquals(False, fFlagsP.RefCode['l'](), 'False Lowercase Value');
  CheckEquals(False, fFlagsP.RefCode['L'](), 'False Uppercase Value');
end;

procedure TestTD2XFlagsParam.TestParseLabel;
begin
  Check(fFlagsP.Parse('F:Test1'), 'Parse right label with True label');
  CheckEquals(True, fFlagsP.RefLabel['Test1'](), 'Frue Value');

  Check(fFlagsP.Parse('F:Test1-,-Test2'), 'Parse right label with multiple False labels');
  CheckEquals(False, fFlagsP.RefLabel['Test1'](), 'False 1 Value');
  CheckEquals(False, fFlagsP.RefLabel['Test2'](), 'False 2 Value');

  Check(fFlagsP.Parse('F:+Test1,Test2+'), 'Parse right label with multiple True labels');
  CheckEquals(True, fFlagsP.RefLabel['Test1'](), 'Frue 1 Value');
  CheckEquals(True, fFlagsP.RefLabel['Test2'](), 'Frue 2 Value');

  Check(fFlagsP.Parse('F:-Test1'), 'Parse right label with False label');
  CheckEquals(False, fFlagsP.RefLabel['Test1'](), 'False Value');

  Check(fFlagsP.Parse('F:testu'), 'Parse right label with Lowercase label');
  CheckEquals(True, fFlagsP.RefLabel['testu'](), 'True Lowercase Value');
  CheckEquals(True, fFlagsP.RefLabel['TestU'](), 'True Uppercase Value');
  CheckEquals(True, fFlagsP.RefLabel['TESTU'](), 'True Uppercase Value');

  Check(fFlagsP.Parse('F:TESTL-'), 'Parse right label with Uppercase label');
  CheckEquals(False, fFlagsP.RefLabel['testl'](), 'False Lowercase Value');
  CheckEquals(False, fFlagsP.RefLabel['Testl'](), 'False Lowercase Value');
  CheckEquals(False, fFlagsP.RefLabel['TESTL'](), 'False Uppercase Value');
end;

procedure TestTD2XFlagsParam.TestReport;
begin
  fFlagsP.Report(fLog);
  CheckLog('Flags Test1-,Test2+,Testl-,TestU-', 'Report Default Value');

  fFlagsP.SetCode('1', True);
  fFlagsP.SetCode('U', True);
  fFlagsP.SetCode('l', True);
  fFlagsP.Report(fLog);
  CheckLog('Flags Test1+,Test2+,Testl+,TestU+', 'Report True Value');

  fFlagsP.Zero;
  fFlagsP.Report(fLog);
  CheckLog('Flags Test1-,Test2-,Testl-,TestU-', 'Report Zeroed Value');
end;

procedure TestTD2XFlagsParam.TestReset;
begin
  CheckEquals(False, fFlagsP.RefCode['1'](), 'Default Value Set');
  CheckEquals(True, fFlagsP.RefCode['2'](), 'Default Value Set');

  fFlagsP.SetCode('2', False);
  CheckEquals(False, fFlagsP.RefCode['2'](), 'Set Value');

  fFlagsP.Reset;
  CheckEquals(False, fFlagsP.RefCode['1'](), 'Value Zeroed');
  CheckEquals(True, fFlagsP.RefCode['2'](), 'Default Value Set');
end;

procedure TestTD2XFlagsParam.TestToString;
begin
  CheckEqualsString('F+2-1UL', fFlagsP.ToString, 'Report Default Value');

  fFlagsP.SetCode('1', True);
  fFlagsP.SetCode('U', True);
  fFlagsP.SetCode('l', True);
  CheckEqualsString('F+12UL', fFlagsP.ToString, 'Report True Value');

  fFlagsP.Zero;
  CheckEqualsString('F-12UL', fFlagsP.ToString, 'Report Zeroed Value');
end;

procedure TestTD2XFlagsParam.TestZero;
begin
  CheckEquals(False, fFlagsP.RefCode['1'](), 'Default Value Set');
  CheckEquals(True, fFlagsP.RefCode['2'](), 'Default Value Set');

  fFlagsP.SetCode('1', True);
  CheckEquals(True, fFlagsP.RefCode['1'](), 'Set Value');

  fFlagsP.Zero;
  CheckEquals(False, fFlagsP.RefCode['1'](), 'Value Zeroed');
  CheckEquals(False, fFlagsP.RefCode['2'](), 'Default Value Set');
end;

{ TestTD2XListParam }

const
  LIST_TEST_VALUE = 'Alpha,Beta,Gamma';

procedure TestTD2XListParam.CheckListParam(pExp, pLbl: string);
begin
  CheckList(pExp, pLbl, fListP.List);
end;

procedure TestTD2XListParam.SetUp;
begin
  inherited;

  fListP := TD2XListParam.CreateList('T', 'Test', 'Test', '.tst',
    function(pFile: string): ID2XIOFile
    begin
      Result := TTestFile.Create('Config\' + pFile, True, 'Tango'#13#10'Uniform', nil);
    end);
end;

procedure TestTD2XListParam.TearDown;
begin
  FreeAndNil(fListP);

  inherited;
end;

procedure TestTD2XListParam.TestDescribe;
begin
  fListP.Describe(fLog);
  CheckLog('T[!:]<list> Clear(!), Load(:,.tst) or Add items to Test', 'Describe Param');
end;

procedure TestTD2XListParam.TestInvalidCreate;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XListParam.Create',
    procedure
    begin
      TD2XListParam.Create('', '', '', '', nil);
    end);
end;

procedure TestTD2XListParam.TestIsDefault;
begin
  Check(fListP.IsDefault, 'Check is Default');

  fListP.List.CommaText := LIST_TEST_VALUE;
  CheckFalse(fListP.IsDefault, 'Check is not Default');

  fListP.Reset;
  Check(fListP.IsDefault, 'Check is Default');
end;

procedure TestTD2XListParam.TestOutput;
begin
  fListP.Output(fL);
  CheckList('-T:', 'Report Default Value');

  fListP.List.CommaText := LIST_TEST_VALUE;
  fListP.Output(fL);
  CheckList('-T: -T+Alpha,Beta,Gamma', 'Report Blank value on');
end;

procedure TestTD2XListParam.TestParse;
begin
  CheckListParam('', 'Default Value Set');
  fListP.Report(fLog);
  CheckLog('Test', 'Report Default value');

  CheckFalse(fListP.Parse('T'), 'Parse right code with No value');
  fListP.Report(fLog);
  CheckLog('Test', 'Report Default value');

  Check(fListP.Parse('TTest'), 'Parse right code add value');
  fListP.Report(fLog);
  CheckLog('Test Test', 'Report add value');

  Check(fListP.Parse('TTest1,Test2'), 'Parse right code add values');
  fListP.Report(fLog);
  CheckLog('Test Test, Test1, Test2', 'Report add values');

  Check(fListP.Parse('T:Test'), 'Parse right code set file');
  fListP.Report(fLog);
  CheckLog('Test Tango, Uniform', 'Report set file');
end;

procedure TestTD2XListParam.TestReport;
begin
  fListP.Report(fLog);
  CheckLog('Test', 'Report Default Value');

  fListP.List.CommaText := LIST_TEST_VALUE;
  fListP.Report(fLog);
  CheckLog('Test Alpha, Beta, Gamma', 'Report Blank value on');
end;

procedure TestTD2XListParam.TestReset;
begin
  CheckList('', 'Default Value Set', fListP.List);

  fListP.List.CommaText := LIST_TEST_VALUE;
  CheckList('Alpha Beta Gamma', 'Simple Value Set', fListP.List);

  fListP.Reset;
  CheckList('', 'Default Value Reset', fListP.List);
end;

procedure TestTD2XListParam.TestToString;
begin
  CheckEqualsString('T', fListP.ToString, 'Check Default Value');

  fListP.List.CommaText := LIST_TEST_VALUE;
  CheckEqualsString('T', fListP.ToString, 'Blank value on');
end;

procedure TestTD2XListParam.TestZero;
begin
  CheckList('', 'Default Value Set', fListP.List);

  fListP.List.CommaText := LIST_TEST_VALUE;
  CheckList('Alpha Beta Gamma', 'Simple Value Set', fListP.List);

  fListP.Zero;
  CheckList('', 'Default Value Reset', fListP.List);
end;

{ TestTD2XFlaggedStringConvertParam }

procedure TestTD2XFlaggedStringConvertParam.SetUp;
begin
  inherited;

  fFlagP := TTestFlaggedStringParam.CreateFlagStr('T', 'Test', '<Example>',
    'Test String Param', 'tsT', False, TestConverter);
  AssignFlag(fFlagP);
end;

procedure TestTD2XFlaggedStringConvertParam.TearDown;
begin
  ClearFlag(fFlagP);

  inherited;
end;

procedure TestTD2XFlaggedStringConvertParam.TestConvert;
begin
  CheckEqualsString('tsT', fFlagP.Value, 'Default Value Set');
  CheckFlag(False, 'Default Flag Set');

  fFlagP.Convert('Simple');
  CheckEqualsString('elpmiS', fFlagP.Value, 'Simple Value Set');
  CheckFlag(False, 'Other Flag Set');
end;

function TestTD2XFlaggedStringConvertParam.TestConverter(pStr, pDflt: string;
  out pVal: string): Boolean;
var
  i: integer;
begin
  Result := Length(pStr) <> 1;
  pVal := '';
  for i := Length(pStr) downto 1 do
    pVal := pVal + pStr[i];
end;

procedure TestTD2XFlaggedStringConvertParam.TestGetFlag;
begin
  inherited;

  CheckFlag(False, 'Check is Default');

  fFlagP.FlagValue := True;
  CheckFlag(True, 'Check is not Default');

  fFlagP.Reset;
  CheckFlag(False, 'Check is Default');
end;

procedure TestTD2XFlaggedStringConvertParam.TestIsDefault;
begin
  Check(fFlagP.IsDefault, 'Check is Default');

  fFlagP.Value := '';
  CheckFalse(fFlagP.IsDefault, 'Check is not Default');

  SetFlag(True);
  CheckFalse(fFlagP.IsDefault, 'Check is not Default');

  fFlagP.Value := 'Tst';
  CheckFalse(fFlagP.IsDefault, 'Check is not Default');

  fFlagP.Value := 'Simple';
  CheckFalse(fFlagP.IsDefault, 'Check is not Default');

  fFlagP.Reset;
  Check(fFlagP.IsDefault, 'Check is Default');
end;

procedure TestTD2XFlaggedStringConvertParam.TestParse;
begin
  CheckEqualsString('tsT', fFlagP.Value, 'Default Value Set');
  CheckFlag(False, 'Default Flag Set');

  Check(fFlagP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('tsT', fFlagP.Value, 'Remains default Value');
  CheckFlag(True, 'Blank Flag Set');

  Check(fFlagP.Parse('T-'), 'Parse right code with Flag off');
  CheckEqualsString('tsT', fFlagP.Value, 'Remains default Value');
  CheckFlag(False, 'Flag Off');

  Check(fFlagP.Parse('T+'), 'Parse right code with Flag on');
  CheckEqualsString('tsT', fFlagP.Value, 'Remains default Value');
  CheckFlag(True, 'Flag On');

  SetFlag(False);
  CheckFlag(False, 'Flag Unset');

  CheckFalse(fFlagP.Parse('T:S'), 'Parse right code with Short value');
  CheckEqualsString('tsT', fFlagP.Value, 'Simple Value');
  CheckFlag(False, 'Flag Unset');

  Check(fFlagP.Parse('T:Simple'), 'Parse right code with value');
  CheckEqualsString('elpmiS', fFlagP.Value, 'Simple Value');
  CheckFlag(True, 'Flag Set');

  Check(fFlagP.Parse('T-'), 'Parse right code with Flag off');
  CheckEqualsString('elpmiS', fFlagP.Value, 'Remains previous Value');
  CheckFlag(False, 'Flag Off');

  Check(fFlagP.Parse('T:'), 'Parse right code with blank value');
  CheckEqualsString('', fFlagP.Value, 'Blank Value');
  CheckFlag(True, 'Flag Set');
end;

procedure TestTD2XFlaggedStringConvertParam.TestParseReset;
begin
  CheckEqualsString('tsT', fFlagP.Value, 'Default Value Set');
  CheckFlag(False, 'Default Flag Set');

  SetFlag(False);
  fFlagP.Value := 'elpmiS';
  CheckFlag(False, 'Flag Unset');
  CheckEqualsString('elpmiS', fFlagP.Value, 'Simple Value');

  Check(fFlagP.Parse('T!'), 'Parse right code with reset');
  CheckEqualsString('tsT', fFlagP.Value, 'Default Value Reset');
  CheckFlag(False, 'Default Flag Reset');
end;

procedure TestTD2XFlaggedStringConvertParam.TestReset;
begin
  CheckEqualsString('tsT', fFlagP.Value, 'Default Value Set');
  CheckFlag(False, 'Default Flag Set');

  fFlagP.Value := 'Simple';
  SetFlag(True);
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value Set');
  CheckFlag(True, 'Other Flag Set');

  fFlagP.Reset;
  CheckEqualsString('tsT', fFlagP.Value, 'Default Value Reset');
  CheckFlag(False, 'Default Flag Reset');
end;

procedure TestTD2XFlaggedStringConvertParam.TestSetFlag;
begin
  inherited;

  CheckFalse(fFlagP.FlagValue, 'Check is Default');

  SetFlag(True);
  CheckTrue(fFlagP.FlagValue, 'Check is not Default');

  SetFlag(False);
  CheckFalse(fFlagP.FlagValue, 'Check is Default');
end;

procedure TestTD2XFlaggedStringConvertParam.TestZero;
begin
  CheckEqualsString('tsT', fFlagP.Value, 'Default Value Set');
  CheckFlag(False, 'Default Flag Set');

  fFlagP.Value := 'Simple';
  SetFlag(True);
  CheckEqualsString('Simple', fFlagP.Value, 'Simple Value Set');
  CheckFlag(True, 'Other Flag Set');

  fFlagP.Zero;
  CheckEqualsString('', fFlagP.Value, 'Value Zeroed');
  CheckFlag(False, 'Flag Zeroed');
end;

{ TestTD2XSingleParamEnum }

procedure TestTD2XSingleParamEnum.TestElapsedModeParam;
var
  lEnumP: TD2XSingleParam<TD2XElapsedMode>;
begin
  lEnumP := TD2XSingleParam<TD2XElapsedMode>.CreateParam('T', 'Test', '', '', emNone,
    function(pStr: string; pDflt: TD2XElapsedMode; out pVal: TD2XElapsedMode): Boolean
    begin
      pVal := emNone;
      Result := True;
    end, TD2X.ToLabel<TD2XElapsedMode>);

  try
    lEnumP.Describe(fLog);
    CheckLog('T None', 'Describe Param');
    lEnumP.Report(fLog);
    CheckLog('Test None', 'Report Default Value');
    Check(lEnumP.IsDefault, 'Check is Default');

    CheckFalse(lEnumP.IsCode('A'), 'Check wrong code');
    Check(lEnumP.IsCode('T'), 'Check correct code');

    CheckFalse(lEnumP.Parse('A'), 'Parse wrong code');
    Check(lEnumP.Parse('T'), 'Parse right code with No value');
    Check(lEnumP.Parse('T0'), 'Parse right code with False value');
    Check(lEnumP.Parse('T+'), 'Parse right code with True value');

    Check(emNone = lEnumP.Value, 'Returned value');
    CheckEqualsString('TNone', lEnumP.ToString, 'String representation');

    lEnumP.Convert('+');
    Check(emNone = lEnumP.Value, 'Returned value');
    CheckEqualsString('TNone', lEnumP.ToString, 'String representation');
  finally
    FreeAndNil(lEnumP);
  end;
end;

procedure TestTD2XSingleParamEnum.TestParseModeParam;
var
  lEnumP: TD2XSingleParam<TD2XParseMode>;
begin
  lEnumP := TD2XSingleParam<TD2XParseMode>.CreateParam('T', 'Test', '', '', pmFull,
    function(pStr: string; pDflt: TD2XParseMode; out pVal: TD2XParseMode): Boolean
    begin
      pVal := pmFull;
      Result := True;
    end, TD2X.ToLabel<TD2XParseMode>);

  try
    lEnumP.Describe(fLog);
    CheckLog('T Full', 'Describe Param');
    lEnumP.Report(fLog);
    CheckLog('Test Full', 'Report Default Value');
    Check(lEnumP.IsDefault, 'Check is Default');

    CheckFalse(lEnumP.IsCode('A'), 'Check wrong code');
    Check(lEnumP.IsCode('T'), 'Check correct code');

    CheckFalse(lEnumP.Parse('A'), 'Parse wrong code');
    Check(lEnumP.Parse('T'), 'Parse right code with No value');
    Check(lEnumP.Parse('T0'), 'Parse right code with False value');
    Check(lEnumP.Parse('T+'), 'Parse right code with True value');

    Check(pmFull = lEnumP.Value, 'Returned value');
    CheckEqualsString('TFull', lEnumP.ToString, 'String representation');

    lEnumP.Convert('+');
    Check(pmFull = lEnumP.Value, 'Returned value');
    CheckEqualsString('TFull', lEnumP.ToString, 'String representation');
  finally
    FreeAndNil(lEnumP);
  end;
end;

procedure TestTD2XSingleParamEnum.TestResultPerParam;
var
  lEnumP: TD2XSingleParam<TD2XResultPer>;
begin
  lEnumP := TD2XSingleParam<TD2XResultPer>.CreateParam('T', 'Test', '', '', rpFile,
    function(pStr: string; pDflt: TD2XResultPer; out pVal: TD2XResultPer): Boolean
    begin
      pVal := rpFile;
      Result := True;
    end, TD2X.ToLabel<TD2XResultPer>);

  try
    lEnumP.Describe(fLog);
    CheckLog('T File', 'Describe Param');
    lEnumP.Report(fLog);
    CheckLog('Test File', 'Report Default Value');
    Check(lEnumP.IsDefault, 'Check is Default');

    CheckFalse(lEnumP.IsCode('A'), 'Check wrong code');
    Check(lEnumP.IsCode('T'), 'Check correct code');

    CheckFalse(lEnumP.Parse('A'), 'Parse wrong code');
    Check(lEnumP.Parse('T'), 'Parse right code with No value');
    Check(lEnumP.Parse('T0'), 'Parse right code with False value');
    Check(lEnumP.Parse('T+'), 'Parse right code with True value');

    Check(rpFile = lEnumP.Value, 'Returned value');
    CheckEqualsString('TFile', lEnumP.ToString, 'String representation');

    lEnumP.Convert('+');
    Check(rpFile = lEnumP.Value, 'Returned value');
    CheckEqualsString('TFile', lEnumP.ToString, 'String representation');
  finally
    FreeAndNil(lEnumP);
  end;
end;

{ TestTD2XFlaggedStringFormatParam }

function TestTD2XFlaggedStringFormatParam.FormatFlagString(pFlag: Boolean;
  pVal: string): string;
begin
  Result := pVal + IfThen(pFlag, ' +', ' -');
end;

procedure TestTD2XFlaggedStringFormatParam.SetUp;
begin
  inherited;

  fFlagP := TTestFlaggedStringParam.CreateFlagStrFmt('T', 'Test', '<Example>',
    'Test String Param', 'Tst', False, nil, FormatFlagString);
  AssignFlag(fFlagP);
end;

procedure TestTD2XFlaggedStringFormatParam.TearDown;
begin
  ClearFlag(fFlagP);

  inherited;
end;

procedure TestTD2XFlaggedStringFormatParam.TestDescribe;
begin
  fFlagP.Describe(fLog);
  CheckLog('T[+-]:<Example> Tst - Test String Param', 'Describe Param');
end;

procedure TestTD2XFlaggedStringFormatParam.TestGetFlag;
begin
  inherited;

  CheckFlag(False, 'Check is Default');

  fFlagP.FlagValue := True;
  CheckFlag(True, 'Check is not Default');

  fFlagP.Reset;
  CheckFlag(False, 'Check is Default');
end;

procedure TestTD2XFlaggedStringFormatParam.TestOutput;
begin
  fFlagP.Output(fL);
  CheckList('', 'Output Default Value');

  fFlagP.Value := '';
  fFlagP.Output(fL);
  CheckList('-T -', 'Output Blank value off');

  SetFlag(True);
  fFlagP.Output(fL);
  CheckList('-T +', 'Output Blank value on');

  fFlagP.Value := 'Simple';
  fFlagP.Output(fL);
  CheckList('-TSimple +', 'Output Simple Value on');
end;

procedure TestTD2XFlaggedStringFormatParam.TestReport;
begin
  fFlagP.Report(fLog);
  CheckLog('Test Tst -', 'Report Default Value');

  fFlagP.Value := '';
  fFlagP.Report(fLog);
  CheckLog('Test -', 'Report Blank value off');

  SetFlag(True);
  fFlagP.Report(fLog);
  CheckLog('Test +', 'Report Blank value on');

  fFlagP.Value := 'Simple';
  fFlagP.Report(fLog);
  CheckLog('Test Simple +', 'Report Simple Value on');
end;

procedure TestTD2XFlaggedStringFormatParam.TestSetFlag;
begin
  inherited;

  CheckFalse(fFlagP.FlagValue, 'Check is Default');

  SetFlag(True);
  CheckTrue(fFlagP.FlagValue, 'Check is not Default');

  SetFlag(False);
  CheckFalse(fFlagP.FlagValue, 'Check is Default');
end;

procedure TestTD2XFlaggedStringFormatParam.TestToString;
begin
  CheckEqualsString('TTst -', fFlagP.ToString, 'Check Default Value');

  fFlagP.Value := '';
  CheckEqualsString('T -', fFlagP.ToString, 'Blank value off');

  SetFlag(True);
  CheckEqualsString('T +', fFlagP.ToString, 'Blank value on');

  fFlagP.Value := 'Simple';
  CheckEqualsString('TSimple +', fFlagP.ToString, 'Check Simple Value');
end;

initialization

RegisterTests('Params', [TestTD2XParam.Suite, TestTD2XParams.Suite, TestTD2XSingleParam.Suite,
  TestTD2XSingleParamEnum.Suite, TestTD2XFlagsParam.Suite, TestTD2XListParam.Suite,
  TestTD2XStringParam.Suite, TestTD2XValidStringParam.Suite, TestTD2XFormatStringParam.Suite,
  TestTD2XFormatValidStringParam.Suite, TestTD2XFlaggedStringParam.Suite,
  TestTD2XFlaggedStringConvertParam.Suite, TestTD2XFlaggedStringFormatParam.Suite,
  TestTD2XDefinesParam.Suite]);

end.
