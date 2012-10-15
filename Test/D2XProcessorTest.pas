unit D2XProcessorTest;

interface

implementation

uses
  TestFramework,
  D2XProcessor,
  System.Diagnostics,
  System.Generics.Collections,
  System.Classes,
  D2XTest,
  D2XOptions,
  D2XParser,
  CastaliaPasLexTypes,
  System.SysUtils,
  System.Rtti,
  D2Xml;

type
  // Test methods for class TD2XProcessor

  TestTD2XProcessor = class(TStringBuilderTestCase)
  strict private
    FD2XProcessor: TD2XProcessor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEndProcessing;

    procedure TestProcessParam;
    procedure TestProcessParamPasFiles;
    procedure TestProcessParamParamFile;

    procedure TestProcessCountChildren;
  end;

  { TestTD2XProcessor }

procedure TestTD2XProcessor.SetUp;
begin
  inherited;

  FD2XProcessor := TD2XProcessor.Create(fSB);
end;

procedure TestTD2XProcessor.TearDown;
begin
  FD2XProcessor := nil;

  inherited;
end;

procedure TestTD2XProcessor.TestEndProcessing;
begin
  FD2XProcessor.EndProcessing;
  CheckString(fSB, '', 'Nothing');
end;

procedure TestTD2XProcessor.TestProcessCountChildren;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;

const
  EXPECTED_RESULT = 'Processing D2XmlTest.pas ... done ' +
  'Processing D2XOptionsTest.pas ... done Processing D2XParamTest.pas ... done ' +
  'Processing D2XParserTest.pas ... done Processing D2XProcessorTest.pas ... done ' +
  'Processing D2XTest.pas ... done Processing D2XUtils.pas ... done';
begin
  pStr := '-C+';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);

  pStr := '*.pas';
  pIdx := 1;

  ReturnValue := FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckString(fSB, EXPECTED_RESULT , 'Nothing');
end;

procedure TestTD2XProcessor.TestProcessParam;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);

  CheckFalse(ReturnValue, 'Return Value');
  CheckString(fSB, '', 'Nothing');
end;

procedure TestTD2XProcessor.TestProcessParamParamFile;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
const
  EXPECTED_RESULT = 'Current option settings: Verbose - Log Errors + Log Not Supp - ' +
  'Timestamp - Final Token + Recurse - Global name Delphi2XmlTests ' +
  'Parse mode Full Results per File Show elapsed Quiet Base dir - ' +
  'Input dir :Config\ Output dir :Log\ Generate XML :Xml\ ' +
  'Write Defines -(Defines\) Defines Used :.used ' +
  'Count Children :.cnt Skipped Methods :.skip ' +
  'Use these Defines: CONDITIONALEXPRESSIONS, CPU386, MSWINDOWS, UNICODE, VER230, WIN32';
begin
  pStr := '@Test.prm';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckString(fSB, EXPECTED_RESULT , 'Nothing');
end;

procedure TestTD2XProcessor.TestProcessParamPasFiles;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;

const
  EXPECTED_RESULT = 'Processing D2XmlTest.pas ... done ' +
  'Processing D2XOptionsTest.pas ... done Processing D2XParamTest.pas ... done ' +
  'Processing D2XParserTest.pas ... done Processing D2XProcessorTest.pas ... done ' +
  'Processing D2XTest.pas ... done Processing D2XUtils.pas ... done';
begin
  pStr := '*.pas';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckString(fSB, EXPECTED_RESULT , 'Nothing');
end;

initialization

// Register any test cases with the test runner
RegisterTest('Processor', TestTD2XProcessor.Suite);

end.
