unit D2XParamTest;

interface

uses
  TestFramework,
  D2XParam;

type
  TestTD2XParam = class(TTestCase)
  private
    fStringProp: String;

    function GetStringProp: String;
    function SetStringProp(pVal: string): Boolean;
    function ConvertStringProp(pVal: string): string;

  published
    procedure TestStringDescription;
  end;

implementation

{ TestTD2XParam }

function TestTD2XParam.ConvertStringProp(pVal: string): string;
begin
  Result := pVal;
end;

function TestTD2XParam.GetStringProp: String;
begin
  Result := fStringProp;
end;

function TestTD2XParam.SetStringProp(pVal: string): Boolean;
begin
  fStringProp := pVal;
  Result := True;
end;

procedure TestTD2XParam.TestStringDescription;
var
  lParam: TD2XSingleParam<String>;
begin
  lParam := TD2XSingleParam<String>.Create('T', 'Test', 'Test String Param', 'Tst',
    GetStringProp, SetStringProp, ConvertStringProp);

  CheckEqualsString('Tst', fStringProp, 'Default Value Set');
  CheckEqualsString('  -T Test            Tst             Test String Param', lParam.Describe, 'Describe Param');
  CheckEqualsString(' Test            Tst', lParam.Report, 'Report Default Value');

  CheckFalse(lParam.Parse('A'), 'Parse wrong code');

  Check(lParam.Parse('T'), 'Parse right code');
  CheckEqualsString('', fStringProp, 'Blank Value');

  Check(lParam.Parse('TSimple'), 'Parse right code with value');
  CheckEqualsString('Simple', fStringProp, 'Simple Value');

  lParam.Reset;
  CheckEqualsString('Tst', fStringProp, 'Default Value Set');
end;

initialization

RegisterTests([TestTD2XParam.Suite]);

end.
