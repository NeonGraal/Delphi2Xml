unit Test.RunParam.Tests;

interface

implementation

uses
  D2X.RunParam,
  System.Classes,
  System.SysUtils,
  Test.Constants,
  Test.Global,
  Test.IO,
  Test.Options,
  TestFramework;

type
  TTestRunParam = class(TD2XRunParam)
  private
    function GetParserDefines: TStringList;

  public
    property ParserDefines: TStringList read GetParserDefines;

  end;

  TestTD2XRunParam = class(TLoggerTestCase)
  private
    fIdx: Integer;

  protected
    fOpts: TTestRunParam;

    function ParseOption(pOpt: String): Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescribeAll;
    procedure TestEndProcessing;

    procedure TestProcessParam;
    procedure TestProcessParamPasFiles;
    procedure TestProcessParamParamFile;
    procedure TestProcessParamInput;
    procedure TestProcessParamComment;
    procedure TestProcessParamException;
  end;

  { TTestRunParam }

function TTestRunParam.GetParserDefines: TStringList;
begin
  Result := TTestOptions(fOpts).ParserDefines();
end;

{ TestTD2XRunParam }

function TestTD2XRunParam.ParseOption(pOpt: String): Boolean;
begin
  Result := fOpts.ProcessParam(pOpt, fIdx);
  Inc(fIdx);
end;

procedure TestTD2XRunParam.SetUp;
begin
  inherited;

  fOpts := TTestRunParam.Create;
  fOpts.JoinLog(fLog);
  fOpts.InitOptions(TTestFactory.Create);

  fIdx := 0;
end;

procedure TestTD2XRunParam.TearDown;
begin
  FreeAndNil(fOpts);

  inherited;
end;

{ TestTD2XRunOptsAll }

procedure TestTD2XRunParam.TestDescribeAll;
begin
  fOpts.DescribeAll;
  CheckLog(UsageDescription + EXPECTED_SHOW_OPTIONS, 'Describe All');
end;

procedure TestTD2XRunParam.TestEndProcessing;
begin
  fOpts.EndRunProcessing;
  CheckBuilder('Processing finished!', 'End Processing');
end;

procedure TestTD2XRunParam.TestProcessParam;
begin
  Check(ParseOption('-?'), 'Return Value');
  CheckLog(UsageDescription + EXPECTED_SHOW_OPTIONS, 'Simple Process Param');

  Check(ParseOption('"-?"'), 'Return Value');
  CheckLog(UsageDescription + EXPECTED_SHOW_OPTIONS, 'Quoted Process Param');

  Check(ParseOption('''-?'''), 'Return Value');
  CheckLog(UsageDescription + EXPECTED_SHOW_OPTIONS, 'Double Quoted Process Param');
end;

procedure TestTD2XRunParam.TestProcessParamComment;
begin
  Check(ParseOption('#Test.prm'), 'Return Value');
  CheckBuilder('', 'Processing Comment param');
end;

procedure TestTD2XRunParam.TestProcessParamException;
const
  BAD_FILE_EXCEPTION = 'EXCEPTION (ETestIOException) processing (Param)@0 "@Bad.file" : Unknown test Config file: Bad.file';
begin
  CheckFalse(ParseOption('@Bad.file'), 'Return Value');
  CheckLog(BAD_FILE_EXCEPTION, 'Processing Bad file (Exception)');
end;

procedure TestTD2XRunParam.TestProcessParamInput;
begin
  Check(ParseOption('-!!'), 'Return Value 1');
  Check(ParseOption('-E!'), 'Return Value 2');
  Check(ParseOption('-'), 'Return Value 3');
  CheckBuilder(INPUT_PROCESSING, 'Nothing');
end;

procedure TestTD2XRunParam.TestProcessParamParamFile;
begin
  Check(ParseOption('@Test.prm'), 'Return Value');
  CheckBuilder(DEFAULT_REPORT_OPTIONS, 'Processing Param file');
end;

procedure TestTD2XRunParam.TestProcessParamPasFiles;
begin
  Check(ParseOption('-!!'), 'Return Value 1');
  Check(ParseOption('-E!'), 'Return Value 2');
  Check(ParseOption('Testing.Test*'), 'Return Value 3');
  CheckBuilder(BOTH_PROCESSING, 'Processing Pas files');
end;

initialization

RegisterTests('RunParam', [TestTD2XRunParam.Suite]);

end.
