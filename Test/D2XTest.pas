unit D2XTest;

interface

uses
  TestFramework,
  D2X;

type
  TestTD2X = class(TTestCase)
  strict private
    FD2X: TD2X;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestToLabel;
  end;

implementation

uses
  D2XOptions;

{ TestTD2X }

procedure TestTD2X.SetUp;
begin
  FD2X := TD2X.Create;
end;

procedure TestTD2X.TearDown;
begin
  FD2X.Free;
  FD2X := nil;
end;

procedure TestTD2X.TestToLabel;
var
  ReturnValue: string;
  pVal: TD2XParseMode;
begin
  pVal := pmFull;
  ReturnValue := FD2X.ToLabel(pVal);
  CheckEqualsString('Full', ReturnValue, 'ReturnValue');
end;

initialization

RegisterTests([TestTD2X.Suite]);

end.
