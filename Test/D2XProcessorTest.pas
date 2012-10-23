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
  TestTD2XProcessor = class(TStringTestCase)
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
    procedure TestProcessVerbose;
  end;

const
  EXPECTED_PROCESSING = 'Processing Test.pas ... done';

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
begin
  pStr := '-!!';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);
  Check(ReturnValue, 'Return Value 1');

  pStr := '-C+';
  pIdx := 1;

  ReturnValue := FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);
  Check(ReturnValue, 'Return Value 2');

  pStr := 'Test.pas';
  pIdx := 2;

  ReturnValue := FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckString(fSB, '', 'Empty Log');
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
  EXPECTED_REPORT = 'Current option settings: Verbose - Log Errors + Log Not Supp - ' +
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
  CheckString(fSB, EXPECTED_REPORT, 'Nothing');
end;

procedure TestTD2XProcessor.TestProcessParamPasFiles;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := 'Test.pas';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckString(fSB, EXPECTED_PROCESSING, 'Nothing');
end;

procedure TestTD2XProcessor.TestProcessVerbose;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '-!!';
  pFrom := 'Test';
  pIdx := 0;
  FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);

  pStr := '-V+';
  pIdx := 1;
  FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);

  pStr := 'Test.pas';
  pIdx := 2;
  ReturnValue := FD2XProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckNotEquals('', fSB.ToString, 'Log');
end;

initialization

// Register any test cases with the test runner
RegisterTest('Processor', TestTD2XProcessor.Suite);

end.
