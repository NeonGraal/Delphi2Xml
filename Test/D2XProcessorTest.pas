unit D2XProcessorTest;

interface

implementation

uses
  TestFramework,
  D2XProcessor,
  System.Diagnostics,
  System.Generics.Collections,
  System.Classes,
  D2XOptions,
  D2XParser,
  CastaliaPasLexTypes,
  System.SysUtils,
  System.Rtti,
  D2Xml;

type
  // Test methods for class TD2XProcessor

  TestTD2XProcessor = class(TTestCase)
  strict private
    FD2XProcessor: TD2XProcessor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEndProcessing;
    procedure TestProcessParam;
  end;

  { TestTD2XProcessor }

procedure TestTD2XProcessor.SetUp;
begin
  FD2XProcessor := TD2XProcessor.Create;
end;

procedure TestTD2XProcessor.TearDown;
begin
  FD2XProcessor.Free;
  FD2XProcessor := nil;
end;

procedure TestTD2XProcessor.TestEndProcessing;
begin
  FD2XProcessor.EndProcessing;
  // TODO: Validate method results
end;

procedure TestTD2XProcessor.TestProcessParam;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  // TODO: Setup method call parameters
  ReturnValue := FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);
  // TODO: Validate method results
end;

initialization

// Register any test cases with the test runner
//RegisterTest('Processor', TestTD2XProcessor.Suite);

end.
