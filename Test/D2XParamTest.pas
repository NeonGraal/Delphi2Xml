unit D2XParamTest;

interface

uses
  TestFramework,
  D2XParam;

type

  TestTD2XSingleParam = class(TTestCase)
  private
    fStrP: TD2XSingleParam<string>;
    fBoolP: TD2XSingleParam<Boolean>;

    fStringProp: string;
    fBooleanProp: Boolean;

    function GetStringProp: string;
    function SetSimpleStringProp(pVal: string): Boolean;
    function SetNonBlankStringProp(pVal: string): Boolean;
    function ConvertStringProp(pVal: string): string;

    function GetBooleanProp: Boolean;
    function SetBooleanProp(pVal: Boolean): Boolean;
    function ConvertBooleanProp(pVal: string): Boolean;
    function FormatBooleanProp(pVal: Boolean): string;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestInvalidAllNil;
    procedure TestInvalidNilGetter;
    procedure TestInvalidNilSetter;
    procedure TestInvalidNilConverter;
    procedure TestInvalidNilFormatter;

    procedure TestInvalidAllBlank;
    procedure TestInvalidAllBlankNonBlank;

    procedure TestStringIsCode;
    procedure TestStringParse;
    procedure TestStringReset;
    procedure TestStringDescribe;
    procedure TestStringReport;
    procedure TestStringValue;
    procedure TestStringToString;

    procedure TestBooleanIsCode;
    procedure TestBooleanParse;
    procedure TestBooleanReset;
    procedure TestBooleanDescribe;
    procedure TestBooleanReport;
    procedure TestBooleanValue;
    procedure TestBooleanToString;

    procedure TestSimpleString;
    procedure TestNonBlankString;

    procedure TestSimpleBoolean;
  end;

  TestTD2XBooleanParam = class(TTestCase)
  strict private
    fBoolP: TD2XBooleanParam;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestIsCode;
    procedure TestParse;
    procedure TestValue;
    procedure TestToString;
  end;

  TestTD2XSimpleStringParam = class(TTestCase)
  strict private
    fStrP: TD2XSimpleStringParam;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestDescribe;
    procedure TestReport;
    procedure TestIsCode;
    procedure TestParse;
    procedure TestValue;
    procedure TestToString;
  end;

implementation

uses
  System.Classes,
  System.StrUtils,
  System.SysUtils;

{ TestTD2XSingleParam }

function TestTD2XSingleParam.ConvertBooleanProp(pVal: string): Boolean;
begin
  if Length(pVal) > 0 then
    case pVal[1] of
      '+', 'T', 't', 'Y', 'y', '1':
        Result := True;
      '-', 'F', 'f', 'N', 'n', '0':
        Result := False;
    else
      raise EParserError.Create('Invalid Value for Boolean');
    end
  else
    Result := True;
end;

function TestTD2XSingleParam.ConvertStringProp(pVal: string): string;
begin
  Result := pVal;
end;

function TestTD2XSingleParam.FormatBooleanProp(pVal: Boolean): string;
begin
  Result := IfThen(pVal, '+', '-');
end;

function TestTD2XSingleParam.GetBooleanProp: Boolean;
begin
  Result := fBooleanProp;
end;

function TestTD2XSingleParam.GetStringProp: string;
begin
  Result := fStringProp;
end;

function TestTD2XSingleParam.SetBooleanProp(pVal: Boolean): Boolean;
begin
  fBooleanProp := pVal;
  Result := True;
end;

function TestTD2XSingleParam.SetNonBlankStringProp(pVal: string): Boolean;
begin
  Result := pVal > '';
  if Result then
    fStringProp := pVal;
end;

function TestTD2XSingleParam.SetSimpleStringProp(pVal: string): Boolean;
begin
  fStringProp := pVal;
  Result := True;
end;

procedure TestTD2XSingleParam.SetUp;
begin
  inherited;

  fStrP := nil;
  fBoolP := nil;
end;

procedure TestTD2XSingleParam.TearDown;
begin
  fStrP.Free;
  fStrP := nil;

  fBoolP.Free;
  fBoolP := nil;

  inherited;
end;

procedure TestTD2XSingleParam.TestSimpleBoolean;
begin
  fBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '[+|-]', 'Test Boolean Param',
    False, GetBooleanProp, SetBooleanProp, ConvertBooleanProp, FormatBooleanProp);

  CheckEquals(False, fBooleanProp, 'Default Value Set');
  CheckEqualsString('  -T Test            [+|-]           -               Test Boolean Param',
    fBoolP.Describe, 'Describe Param');
  CheckEqualsString(' Test            -', fBoolP.Report, 'Report Default Value');

  CheckFalse(fBoolP.Parse('A'), 'Parse wrong code');

  Check(fBoolP.Parse('T'), 'Parse right code with No value');
  CheckEquals(True, fBooleanProp, 'Blank Value');

  Check(fBoolP.Parse('T0'), 'Parse right code with False value');
  CheckEquals(False, fBooleanProp, 'False Value');

  Check(fBoolP.Parse('T+'), 'Parse right code with True value');
  CheckEquals(True, fBooleanProp, 'True Value');

  fBoolP.Reset;
  CheckEquals(False, fBooleanProp, 'Default Value Set');
end;

procedure TestTD2XSingleParam.TestBooleanDescribe;
begin
  fBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '[+|-]', 'Test Boolean Param',
    False, GetBooleanProp, SetBooleanProp, ConvertBooleanProp, FormatBooleanProp);

  CheckEqualsString('  -T Test            [+|-]           -               Test Boolean Param',
    fBoolP.Describe, 'Describe Param');
end;

procedure TestTD2XSingleParam.TestBooleanIsCode;
begin
  fBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '[+|-]', 'Test Boolean Param',
    False, GetBooleanProp, SetBooleanProp, ConvertBooleanProp, FormatBooleanProp);

  CheckFalse(fBoolP.IsCode('A'), 'Wrong code');

  Check(fBoolP.IsCode('T'), 'Correct code');
end;

procedure TestTD2XSingleParam.TestBooleanParse;
begin
  fBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '[+|-]', 'Test Boolean Param',
    False, GetBooleanProp, SetBooleanProp, ConvertBooleanProp, FormatBooleanProp);

  CheckFalse(fBoolP.Parse('A'), 'Parse wrong code');

  Check(fBoolP.Parse('T'), 'Parse right code with No value');
  CheckEquals(True, fBooleanProp, 'Blank Value');

  Check(fBoolP.Parse('T0'), 'Parse right code with False value');
  CheckEquals(False, fBooleanProp, 'False Value');

  Check(fBoolP.Parse('T+'), 'Parse right code with True value');
  CheckEquals(True, fBooleanProp, 'True Value');
end;

procedure TestTD2XSingleParam.TestBooleanReport;
begin
  fBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '[+|-]', 'Test Boolean Param',
    False, GetBooleanProp, SetBooleanProp, ConvertBooleanProp, FormatBooleanProp);

  CheckEqualsString(' Test            -', fBoolP.Report, 'Report Default Value');
end;

procedure TestTD2XSingleParam.TestBooleanReset;
begin
  fBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '[+|-]', 'Test Boolean Param',
    False, GetBooleanProp, SetBooleanProp, ConvertBooleanProp, FormatBooleanProp);

  CheckEquals(False, fBooleanProp, 'Default Value Set');

  Check(fBoolP.Parse('T'), 'Parse right code with No value');
  CheckEquals(True, fBooleanProp, 'Blank Value');

  fBoolP.Reset;
  CheckEquals(False, fBooleanProp, 'Default Value Reset');
end;

procedure TestTD2XSingleParam.TestBooleanToString;
begin
  fBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '[+|-]', 'Test Boolean Param',
    False, GetBooleanProp, SetBooleanProp, ConvertBooleanProp, FormatBooleanProp);

  CheckEqualsString('-', fBoolP.ToString, 'Report Default Value');
end;

procedure TestTD2XSingleParam.TestBooleanValue;
begin
  fBoolP := TD2XSingleParam<Boolean>.CreateParam('T', 'Test', '[+|-]', 'Test Boolean Param',
    False, GetBooleanProp, SetBooleanProp, ConvertBooleanProp, FormatBooleanProp);

  CheckEquals(False, fBoolP.Value, 'Default Value Set');
end;

procedure TestTD2XSingleParam.TestStringDescribe;
begin
  fStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '<Example>', 'Test String Param',
    'Tst', GetStringProp, SetSimpleStringProp, ConvertStringProp, ConvertStringProp);

  CheckEqualsString('  -T Test            <Example>       Tst             Test String Param',
    fStrP.Describe, 'Describe Param');
end;

procedure TestTD2XSingleParam.TestInvalidAllBlank;
begin
  StartExpectingException(EInvalidParam);
  fStrP := TD2XSingleParam<string>.CreateParam('', '', '', '', '', GetStringProp,
    SetSimpleStringProp, ConvertStringProp, ConvertStringProp);
  StopExpectingException('All blank');
end;

procedure TestTD2XSingleParam.TestInvalidAllBlankNonBlank;
begin
  StartExpectingException(EInvalidParam);
  fStrP := TD2XSingleParam<string>.CreateParam('', '', '', '', '', GetStringProp,
    SetNonBlankStringProp, ConvertStringProp, ConvertStringProp);
  StopExpectingException('All blank');
end;

procedure TestTD2XSingleParam.TestInvalidAllNil;
begin
  StartExpectingException(EInvalidParam);
  fStrP := TD2XSingleParam<string>.CreateParam('', '', '', '', '', nil, nil, nil, nil);
  StopExpectingException('All nil');
end;

procedure TestTD2XSingleParam.TestInvalidNilConverter;
begin
  StartExpectingException(EInvalidParam);
  fStrP := TD2XSingleParam<string>.CreateParam('', '', '', '', '', GetStringProp,
    SetNonBlankStringProp, nil, ConvertStringProp);
  StopExpectingException('Converter nil');
end;

procedure TestTD2XSingleParam.TestInvalidNilFormatter;
begin
  StartExpectingException(EInvalidParam);
  fStrP := TD2XSingleParam<string>.CreateParam('', '', '', '', '', GetStringProp,
    SetNonBlankStringProp, ConvertStringProp, nil);
  StopExpectingException('Formatter nil');
end;

procedure TestTD2XSingleParam.TestInvalidNilGetter;
begin
  StartExpectingException(EInvalidParam);
  fStrP := TD2XSingleParam<string>.CreateParam('', '', '', '', '', nil, SetNonBlankStringProp,
    ConvertStringProp, ConvertStringProp);
  StopExpectingException('Getter nil');
end;

procedure TestTD2XSingleParam.TestInvalidNilSetter;
begin
  StartExpectingException(EInvalidParam);
  TD2XSingleParam<string>.CreateParam('', '', '', '', '', GetStringProp, nil,
    ConvertStringProp, ConvertStringProp);
  StopExpectingException('Setter nil');
end;

procedure TestTD2XSingleParam.TestStringIsCode;
begin
  fStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '<Example>', 'Test String Param',
    'Tst', GetStringProp, SetSimpleStringProp, ConvertStringProp, ConvertStringProp);

  CheckFalse(fStrP.IsCode('A'), 'Is wrong code');
  Check(fStrP.IsCode('T'), 'Is right code');
end;

procedure TestTD2XSingleParam.TestNonBlankString;
begin
  fStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '<Example>', 'Test String Param',
    'Tst', GetStringProp, SetNonBlankStringProp, ConvertStringProp, ConvertStringProp);

  CheckFalse(fStrP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('Tst', fStringProp, 'No Value doesn''t change value');

  Check(fStrP.Parse('TSimple'), 'Parse right code with value');
  CheckEqualsString('Simple', fStringProp, 'Simple Value');
end;

procedure TestTD2XSingleParam.TestStringParse;
begin
  fStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '<Example>', 'Test String Param',
    'Tst', GetStringProp, SetSimpleStringProp, ConvertStringProp, ConvertStringProp);

  CheckEqualsString('Tst', fStringProp, 'Default Value Set');

  Check(fStrP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('', fStringProp, 'Blank Value');

  Check(fStrP.Parse('TSimple'), 'Parse right code with value');
  CheckEqualsString('Simple', fStringProp, 'Simple Value');
end;

procedure TestTD2XSingleParam.TestStringReport;
begin
  fStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '<Example>', 'Test String Param',
    'Tst', GetStringProp, SetSimpleStringProp, ConvertStringProp, ConvertStringProp);

  CheckEqualsString(' Test            Tst', fStrP.Report, 'Report Default Value');
end;

procedure TestTD2XSingleParam.TestStringReset;
begin
  fStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '<Example>', 'Test String Param',
    'Tst', GetStringProp, SetSimpleStringProp, ConvertStringProp, ConvertStringProp);

  CheckEqualsString('Tst', fStringProp, 'Default Value Set');

  Check(fStrP.Parse('TSimple'), 'Parse right code with value');
  CheckEqualsString('Simple', fStringProp, 'Simple Value');

  fStrP.Reset;
  CheckEqualsString('Tst', fStringProp, 'Default Value Set');
end;

procedure TestTD2XSingleParam.TestSimpleString;
begin
  fStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '<Example>', 'Test String Param',
    'Tst', GetStringProp, SetSimpleStringProp, ConvertStringProp, ConvertStringProp);

  CheckEqualsString('Tst', fStringProp, 'Default Value Set');

  Check(fStrP.Parse('T'), 'Parse right code with No value');
  CheckEqualsString('', fStringProp, 'Blank Value');

  Check(fStrP.Parse('TSimple'), 'Parse right code with value');
  CheckEqualsString('Simple', fStringProp, 'Simple Value');

  fStrP.Reset;
  CheckEqualsString('Tst', fStringProp, 'Default Value Set');
end;

procedure TestTD2XSingleParam.TestStringToString;
begin
  fStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '<Example>', 'Test String Param',
    'Tst', GetStringProp, SetSimpleStringProp, ConvertStringProp, ConvertStringProp);

  CheckEqualsString('Tst', fStrP.ToString, 'Check Value');
end;

procedure TestTD2XSingleParam.TestStringValue;
begin
  fStrP := TD2XSingleParam<string>.CreateParam('T', 'Test', '<Example>', 'Test String Param',
    'Tst', GetStringProp, SetSimpleStringProp, ConvertStringProp, ConvertStringProp);

  CheckEqualsString('Tst', fStrP.Value, 'Check Value');
end;

{ TestTD2XBooleanParam }

procedure TestTD2XBooleanParam.SetUp;
begin
  fBoolP := TD2XBooleanParam.Create;
end;

procedure TestTD2XBooleanParam.TearDown;
begin
  fBoolP.Free;
  fBoolP := nil;
end;

procedure TestTD2XBooleanParam.TestDescribe;
begin

end;

procedure TestTD2XBooleanParam.TestIsCode;
begin

end;

procedure TestTD2XBooleanParam.TestParse;
begin

end;

procedure TestTD2XBooleanParam.TestReport;
begin

end;

procedure TestTD2XBooleanParam.TestReset;
begin

end;

procedure TestTD2XBooleanParam.TestToString;
begin

end;

procedure TestTD2XBooleanParam.TestValue;
begin

end;

{ TestTD2XSimpleStringParam }

procedure TestTD2XSimpleStringParam.SetUp;
begin
  fStrP := TD2XSimpleStringParam.Create;
end;

procedure TestTD2XSimpleStringParam.TearDown;
begin
  fStrP.Free;
  fStrP := nil;
end;

procedure TestTD2XSimpleStringParam.TestDescribe;
begin

end;

procedure TestTD2XSimpleStringParam.TestIsCode;
begin

end;

procedure TestTD2XSimpleStringParam.TestParse;
begin

end;

procedure TestTD2XSimpleStringParam.TestReport;
begin

end;

procedure TestTD2XSimpleStringParam.TestReset;
begin

end;

procedure TestTD2XSimpleStringParam.TestToString;
begin

end;

procedure TestTD2XSimpleStringParam.TestValue;
begin

end;

initialization

RegisterTests([TestTD2XBooleanParam.Suite, TestTD2XSimpleStringParam.Suite,
    TestTD2XSingleParam.Suite]);

end.
