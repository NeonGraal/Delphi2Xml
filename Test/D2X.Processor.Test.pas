unit D2X.Processor.Test;

interface

uses
  D2X.Param;

type
  TTestBoolFlag = class(TInterfacedObject, IParamFlag)
  private
    fFlag: Boolean;

    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);
  end;

implementation

uses
  TestFramework,
  CastaliaPasLexTypes,
  D2X.Xml,
  D2X.Test,
  D2X.Handler,
  D2X.Handler.Test,
  D2X.Options,
  D2X.Parser,
  D2X.ParamProcessor,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.SysUtils,
  System.Rtti;

type
  TestTD2XParamProcessor = class(TStringTestCase)
  strict private
    FD2XParamProcessor: TD2XParamProcessor;
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

procedure TestTD2XParamProcessor.SetUp;
begin
  inherited;

  FD2XParamProcessor := TD2XParamProcessor.Create(fSB);
end;

procedure TestTD2XParamProcessor.TearDown;
begin
  FD2XParamProcessor := nil;

  inherited;
end;

procedure TestTD2XParamProcessor.TestEndProcessing;
begin
  FD2XParamProcessor.EndProcessing;
  CheckString(fSB, '', 'Nothing');
end;

procedure TestTD2XParamProcessor.TestProcessCountChildren;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '-!!';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);
  Check(ReturnValue, 'Return Value 1');

  pStr := '-C+';
  pIdx := 1;

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);
  Check(ReturnValue, 'Return Value 2');

  pStr := 'Test.pas';
  pIdx := 2;

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckString(fSB, '', 'Empty Log');
end;

procedure TestTD2XParamProcessor.TestProcessParam;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  CheckFalse(ReturnValue, 'Return Value');
  CheckString(fSB, '', 'Nothing');
end;

procedure TestTD2XParamProcessor.TestProcessParamParamFile;
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

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckString(fSB, EXPECTED_REPORT, 'Nothing');
end;

procedure TestTD2XParamProcessor.TestProcessParamPasFiles;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := 'Test.pas';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckString(fSB, EXPECTED_PROCESSING, 'Nothing');
end;

procedure TestTD2XParamProcessor.TestProcessVerbose;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '-!!';
  pFrom := 'Test';
  pIdx := 0;
  FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  pStr := '-V+';
  pIdx := 1;
  FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  pStr := 'Test.pas';
  pIdx := 2;
  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckNotEquals('', fSB.ToString, 'Log');
end;

{ TTestBoolFlag }

function TTestBoolFlag.GetFlag: Boolean;
begin
  Result := fFlag;
end;

procedure TTestBoolFlag.SetFlag(pVal: Boolean);
begin
  fFlag := pVal;
end;

initialization

// Register any test cases with the test runner
RegisterTests('Processor', [TestTD2XParamProcessor.Suite]);

end.
