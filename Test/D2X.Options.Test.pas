unit D2X.Options.Test;

interface

uses
  D2X.Param,
  TestFramework;

type
  TParamsTestCase = class(TTestCase)
  protected
    fParams: TD2XParams;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestBoolFlag = class(TInterfacedObject, IParamFlag)
  private
    fFlag: Boolean;

    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);
  end;

implementation

uses
  D2X,
  D2X.Options,
  D2X.Test,
  D2X.Utils,
  System.Classes,
  System.StrUtils,
  System.SysUtils;

type
  TestTD2XOptionEnums = class(TTestCase)
  published
    procedure TestElapsedModeInvalidCreate;
    procedure TestParseModeInvalidCreate;
    procedure TestResultPerInvalidCreate;

    procedure TestElapsedModeParam;
    procedure TestParseModeParam;
    procedure TestResultPerParam;
  end;

  TestTD2XOptionGeneral = class(TTestCase)
  published
    procedure TestConvertDir;
    procedure TestConvertExtn;
    procedure TestConvertFile;
  end;

  TestTD2XFileOptions = class(TParamsTestCase)
  private
    fFileOpts: TD2XFileOptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGobalName;
    procedure TestTimestampFiles;

    procedure TestInputFile;
    procedure TestInputExtn;
    procedure TestInputDirFile;
    procedure TestInputDirExtn;
    procedure TestInputOffFile;
    procedure TestInputOffExtn;
    procedure TestInputOnFile;
    procedure TestInputOnExtn;

    procedure TestOutputFile;
    procedure TestOutputExtn;
    procedure TestOutputDirFile;
    procedure TestOutputDirExtn;
    procedure TestOutputOffFile;
    procedure TestOutputOffExtn;
    procedure TestOutputOnFile;
    procedure TestOutputOnExtn;

    procedure TestOutputTimestampFile;
    procedure TestOutputTimestampExtn;
    procedure TestOutputNoTimestampFile;
    procedure TestOutputNoTimestampExtn;
  end;

  TOptionsTestCase = class(TStringTestCase)
  protected
    fPP: TD2XOptions;

    procedure CheckLog(pMsg: string);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTD2XOptions = class(TOptionsTestCase)
  published
    procedure TestEndProcessing;

    procedure TestProcessParam;
    procedure TestProcessParamPasFiles;
    procedure TestProcessParamParamFile;

    procedure TestProcessCountChildren;
    procedure TestProcessVerbose;
  end;

  TestTD2XOptionsGeneral = class(TOptionsTestCase)
  private
    procedure SetAllOptions;
  published
    procedure TestDefaultOptions;
    procedure TestReportOptions;
    procedure TestReportOptionsDefault;
    procedure TestReportOptionsFileDefault;
    procedure TestReportOptionsFile;
    procedure TestReportOptionsExtn;
    procedure TestReportOptionsDefines;
    procedure TestReportOptionsEmpty;
    procedure TestReportOptionsReset;
    procedure TestResetOptions;
    procedure TestZeroOptions;
    procedure TestShowOptions;
  end;

const
  EXPECTED_PROCESSING = 'Processing Test.pas ... done';
  EXPECTED_SHOW_OPTIONS =
    'Usage: Delphi2XmlTests [ Option | @Params | mFilename | Wildcard ] ... ' +
    'Options: Default Description ? Show valid options ' +
    '! Reset all options to defaults @<file> Report/Output Current options ' +
    'V[+|-] - Log all Parser methods called L[+|-] + Log Error messages ' +
    'N[+|-] - Log Not Supported messages F[+|-] + Record Final Token ' +
    'R[+|-] - Recurse into subdirectories T[+|-] - Timestamp global output files ' +
    'G<str> Delphi2XmlTests Sets global name ' +
    'I[+-]:<dir> :Config\ Use <dir> as a base for all file input ' +
    'O[+-]:<dir> :Log\ Use <dir> as a base for all file output ' +
    'M<mode> Full Set Parsing mode (F[ull], U[ses]) ' +
    'P<per> File Set Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un]) ' +
    'E<mode> Quiet Set Elapsed time display to be (N[one], Q[uiet], T[otal], P[rocessing]) ' +
    'B[+-]:<dir> - Use <dir> as a base for all file lookups ' +
    'X[+-]:<dir> :Xml\ Generate XML files into current or given <dir> ' +
    'W[+-]:<dir> -(Defines\) Generate Final Defines files into current or given <dir> ' +
    'U[+-]:<f/e> :.used Report Defines Used into <f/e> ' +
    'C[+-]:<f/e> :.cnt Report Min/Max Children into <f/e> ' +
    'S[+-]:<f/e> :.skip Load Skipped Methods from <f/e> ' +
    'D[+-!:]<def> Add(+), Remove(-), Clear(!) or Load(:) Defines ' +
    'Definitions: <f/e> If value begins with "." is appended to global name to give file name';

  { TestTD2XFileOptions }

procedure TestTD2XFileOptions.SetUp;
begin
  inherited;

  fFileOpts := TD2XFileOptions.Create(nil);
  fFileOpts.RegisterParams(fParams);
end;

procedure TestTD2XFileOptions.TearDown;
begin
  FreeAndNil(fFileOpts);

  inherited;
end;

procedure TestTD2XFileOptions.TestGobalName;
begin
  fParams.ForCode('G').Parse('GGlobal');
  CheckEqualsString('Global', fFileOpts.GlobalName, 'GlobalName');

  fFileOpts.GlobalName := 'Test';
  CheckEqualsString('Test', fFileOpts.GlobalName, 'GlobalName');
end;

procedure TestTD2XFileOptions.TestInputDirExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I:In');

  ReturnValue := fFileOpts.InputFileOrExtn(pExtn);
  CheckEqualsString('In\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.InputFileOrExtn(pExtn);
  CheckEqualsString('In\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputDirFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I:In');

  ReturnValue := fFileOpts.InputFileOrExtn(pFile);

  CheckEqualsString('In\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOffExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I-');

  ReturnValue := fFileOpts.InputFileOrExtn(pExtn);
  CheckEqualsString(fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.InputFileOrExtn(pExtn);
  CheckEqualsString('Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOffFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I-');

  ReturnValue := fFileOpts.InputFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOnExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I+');

  ReturnValue := fFileOpts.InputFileOrExtn(pExtn);
  CheckEqualsString('Config\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.InputFileOrExtn(pExtn);
  CheckEqualsString('Config\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOnFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I+');

  ReturnValue := fFileOpts.InputFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';

  ReturnValue := fFileOpts.InputFileOrExtn(pExtn);
  CheckEqualsString('Config\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.InputFileOrExtn(pExtn);
  CheckEqualsString('Config\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fFileOpts.InputFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputDirExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O:Out');

  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString('Out\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString('Out\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputDirFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O:Out');

  ReturnValue := fFileOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Out\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';

  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString('Log\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fFileOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputNoTimestampExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('T').Parse('T-');

  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString('Log\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputNoTimestampFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('T').Parse('T-');

  ReturnValue := fFileOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOffExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O-');

  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString(fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString('Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOffFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O-');

  ReturnValue := fFileOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOnExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O+');

  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString('Log\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOnFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O+');

  ReturnValue := fFileOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputTimestampExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('T').Parse('T');

  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + fFileOpts.OutputTimestamp + '.Extn',
    ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.OutputFileOrExtn(pExtn);
  CheckEqualsString('Log\Global' + fFileOpts.OutputTimestamp + '.Extn', ReturnValue,
    'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputTimestampFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('T').Parse('T');

  ReturnValue := fFileOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File' + fFileOpts.OutputTimestamp + '.Extn', ReturnValue,
    'ReturnValue');
end;

procedure TestTD2XFileOptions.TestTimestampFiles;
begin
  CheckFalse(fFileOpts.TimestampFiles, 'GlobalName');

  fParams.ForCode('T').Parse('T');
  CheckTrue(fFileOpts.TimestampFiles, 'GlobalName');
end;

{ TestTD2XOptionEnums }

procedure TestTD2XOptionEnums.TestElapsedModeInvalidCreate;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<TD2XElapsedMode>.Create('', '', '', '', nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XOptionEnums.TestElapsedModeParam;
var
  lPrm: TD2XSingleParam<TD2XElapsedMode>;
begin
  lPrm := TD2XSingleParam<TD2XElapsedMode>.CreateParam('T', 'Test', '<tst>',
    'Test Elapsed mode', emQuiet, TD2X.CnvDflt<TD2XElapsedMode>,
    TD2X.ToLabel<TD2XElapsedMode>, nil);
  try
    CheckEqualsString('T<tst> Quiet Test Elapsed mode', ReduceString(lPrm.Describe),
      'Describe Param');
    CheckEqualsString('Test Quiet', ReduceString(lPrm.Report), 'Report Default Value');
    Check(lPrm.IsDefault, 'Check is Default');

    Check(lPrm.Parse('T'), 'Parse right code with No value');
    Check(lPrm.Parse('TTotal'), 'Parse right code with value');

    Check(emQuiet = lPrm.Value, 'Returned value');
    CheckEqualsString('TQuiet', lPrm.ToString, 'String representation');

    lPrm.Value := emTotal;
    CheckEqualsString('TTotal', lPrm.ToString, 'String representation');
  finally
    FreeAndNil(lPrm);
  end;
end;

procedure TestTD2XOptionEnums.TestParseModeInvalidCreate;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<TD2XParseMode>.Create('', '', '', '', nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XOptionEnums.TestParseModeParam;
var
  lPrm: TD2XSingleParam<TD2XParseMode>;
begin
  lPrm := TD2XSingleParam<TD2XParseMode>.CreateParam('T', 'Test', '<tst>', 'Test Parse mode',
    pmFull, TD2X.CnvDflt<TD2XParseMode>, TD2X.ToLabel<TD2XParseMode>, nil);
  try
    CheckEqualsString('T<tst> Full Test Parse mode', ReduceString(lPrm.Describe),
      'Describe Param');
    CheckEqualsString('Test Full', ReduceString(lPrm.Report), 'Report Default Value');
    Check(lPrm.IsDefault, 'Check is Default');

    Check(lPrm.Parse('T'), 'Parse right code with No value');
    Check(lPrm.Parse('TUses'), 'Parse right code with value');

    Check(pmFull = lPrm.Value, 'Returned value');
    CheckEqualsString('TFull', lPrm.ToString, 'String representation');

    lPrm.Value := pmUses;
    CheckEqualsString('TUses', lPrm.ToString, 'String representation');
  finally
    FreeAndNil(lPrm);
  end;
end;

procedure TestTD2XOptionEnums.TestResultPerInvalidCreate;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<TD2XResultPer>.Create('', '', '', '', nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XOptionEnums.TestResultPerParam;
var
  lPrm: TD2XSingleParam<TD2XResultPer>;
begin
  lPrm := TD2XSingleParam<TD2XResultPer>.CreateParam('T', 'Test', '<tst>', 'Test Parse mode',
    rpFile, TD2X.CnvDflt<TD2XResultPer>, TD2X.ToLabel<TD2XResultPer>, nil);
  try
    CheckEqualsString('T<tst> File Test Parse mode', ReduceString(lPrm.Describe),
      'Describe Param');
    CheckEqualsString('Test File', ReduceString(lPrm.Report), 'Report Default Value');
    Check(lPrm.IsDefault, 'Check is Default');

    Check(lPrm.Parse('T'), 'Parse right code with No value');
    Check(lPrm.Parse('TDir'), 'Parse right code with value');

    Check(rpFile = lPrm.Value, 'Returned value');
    CheckEqualsString('TFile', lPrm.ToString, 'String representation');

    lPrm.Value := rpDir;
    CheckEqualsString('TDir', lPrm.ToString, 'String representation');
  finally
    FreeAndNil(lPrm);
  end;
end;

{ TestTD2XOptionGeneral }

procedure TestTD2XOptionGeneral.TestConvertDir;
var
  lResult: string;
begin
  ConvertDir('', '', lResult);
  CheckEqualsString('', lResult, 'Empty All, No change');

  ConvertDir('', 'Default', lResult);
  CheckEqualsString('', lResult, 'Default only, No change');

  ConvertDir('Test', '', lResult);
  CheckEqualsString('Test\', lResult, 'Dir only, Path Char added');

  ConvertDir('Test', 'Default', lResult);
  CheckEqualsString('Test\', lResult, 'Both, Path Char added');
end;

procedure TestTD2XOptionGeneral.TestConvertExtn;
var
  lResult: string;
begin
  ConvertExtn('', '', lResult);
  CheckEqualsString('', lResult, 'Empty All, No change');

  ConvertExtn('', 'Default', lResult);
  CheckEqualsString('Default', lResult, 'Default only, Just Default');

  ConvertExtn('Test', '', lResult);
  CheckEqualsString('.Test', lResult, 'Extn, Extn Char added');

  ConvertExtn('Test', 'Default', lResult);
  CheckEqualsString('.Test', lResult, 'Both, Extn Char added');

  ConvertExtn('Test.Test', 'Default', lResult);
  CheckEqualsString('Test.Test', lResult, 'File, File returned');
end;

procedure TestTD2XOptionGeneral.TestConvertFile;
var
  lResult: string;
begin
  ConvertFile('', '', lResult);
  CheckEqualsString('', lResult, 'Empty All, No change');

  ConvertFile('', '.Def', lResult);
  CheckEqualsString('.Def', lResult, 'Default only, Just Default');

  ConvertFile('Test', '', lResult);
  CheckEqualsString('Test', lResult, 'File only, No Change');

  ConvertFile('Test', '.Def', lResult);
  CheckEqualsString('Test.Def', lResult, 'Both, Extn added');

  ConvertFile('Test.Test', '.Def', lResult);
  CheckEqualsString('Test.Test', lResult, 'File, No Change');
end;

{ TParamsTestCase }

procedure TParamsTestCase.SetUp;
begin
  inherited;

  fParams := TD2XParams.Create;
end;

procedure TParamsTestCase.TearDown;
begin
  fParams := nil;

  inherited;
end;

{ TestTD2XOptions }

procedure TestTD2XOptions.TestEndProcessing;
begin
  fPP.EndProcessing;
  CheckBuilder('', 'Nothing');
end;

procedure TestTD2XOptions.TestProcessCountChildren;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '-!!';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := fPP.ProcessParam(pStr, pFrom, pIdx);
  Check(ReturnValue, 'Return Value 1');

  pStr := '-C+';
  pIdx := 1;

  ReturnValue := fPP.ProcessParam(pStr, pFrom, pIdx);
  Check(ReturnValue, 'Return Value 2');

  pStr := 'Test.pas';
  pIdx := 2;

  ReturnValue := fPP.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckBuilder('', 'Empty Log');
end;

procedure TestTD2XOptions.TestProcessParam;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '-?';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := fPP.ProcessParam(pStr, pFrom, pIdx);

  CheckTrue(ReturnValue, 'Return Value');
  CheckBuilder(EXPECTED_SHOW_OPTIONS, 'Nothing');
end;

procedure TestTD2XOptions.TestProcessParamParamFile;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
const
  EXPECTED_REPORT = 'Current option settings: Verbose - Log Errors + Log Not Supp - ' +
    'Final Token + Recurse - Timestamp - Global name Delphi2XmlTests ' +
    'Input dir :Config\ Output dir :Log\ Parse mode Full Results per File ' +
    'Show elapsed Quiet Base dir - Generate XML :Xml\ ' +
    'Write Defines -(Defines\) Defines Used :.used ' +
    'Count Children :.cnt Skipped Methods :.skip ' + 'Use default Defines';
begin
  pStr := '@Test.prm';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := fPP.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckBuilder(EXPECTED_REPORT, 'Nothing');
end;

procedure TestTD2XOptions.TestProcessParamPasFiles;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := 'Test.pas';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := fPP.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckBuilder(EXPECTED_PROCESSING, 'Nothing');
end;

procedure TestTD2XOptions.TestProcessVerbose;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '-!!';
  pFrom := 'Test';
  pIdx := 0;
  fPP.ProcessParam(pStr, pFrom, pIdx);

  pStr := '-V+';
  pIdx := 1;
  fPP.ProcessParam(pStr, pFrom, pIdx);

  pStr := 'Test.pas';
  pIdx := 2;
  ReturnValue := fPP.ProcessParam(pStr, pFrom, pIdx);

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

const
  ALTERED_REPORT_OPTIONS =
    'Current option settings: Verbose + Log Errors + Log Not Supp + Final Token + ' +
    'Recurse + Timestamp - Global name :Test Input dir :Test\ Output dir :Test\ ' +
    'Parse mode Full Results per File Show elapsed Quiet Base dir :Test\ ' +
    'Generate XML :Test\ Write Defines :Test\ Defines Used :.Test Count Children :.Test ' +
    'Skipped Methods :Test.skip Use these Defines: Tango, Uniform';
  ZERO_REPORT_OPTIONS =
    'Current option settings: Verbose - Log Errors - Log Not Supp - Final Token - ' +
    'Recurse - Timestamp - Global name Input dir - Output dir - Parse mode Full ' +
    'Results per File Show elapsed None Base dir - Generate XML - ' +
    'Write Defines - Defines Used - Count Children - Skipped Methods - Use NO Defines';
  BASE_REPORT_OPTIONS =
    'Current option settings: Verbose - Log Errors + Log Not Supp - Final Token + ' +
    'Recurse - Timestamp - Global name Delphi2XmlTests Input dir :Config\ ' +
    'Output dir :Log\ Parse mode Full Results per File Show elapsed Quiet ' +
    'Base dir - Generate XML :Xml\ Write Defines -(Defines\) Defines Used :.used ' +
    'Count Children :.cnt Skipped Methods :.skip ';
  DEFAULT_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use default Defines';
  EMPTY_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use NO Defines';
  DEFINED_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use these Defines: CPU32';

  { TestTD2XOptionsGeneral }

procedure TestTD2XOptionsGeneral.SetAllOptions;
var
  C: Char;
begin
  for C := 'A' to 'Z' do
    if not fPP.ProcessParam('-' + C + ':Test', '', 0) then
      fPP.ProcessParam('-' + C + '+', '', 0);
  fPP.ProcessParam('-T-', '', 0);
  fSB.Clear;
end;

procedure TestTD2XOptionsGeneral.TestDefaultOptions;
begin
  //  Check(fPP.Options.LogErrors, 'LogErrors');
  //  CheckFalse(fPP.Options.LogNotSupported, 'LogNotSupported');
  //  CheckFalse(fPP.Options.FileOpts.TimestampFiles, 'TimestampFiles');
  //  CheckFalse(fPP.Verbose, 'Verbose');
  //  CheckFalse(fPP.Options.Recurse, 'Recurse');
  //  CheckEqualsString(ChangeFileExt(ExtractFileName(ParamStr(0)), ''), fPP.Options.FileOpts.GlobalName,
  //    'SkipFileOrExtn');
  //  CheckFalse(fPP.Options.UseBase, 'UseBase');
  //  CheckEqualsString('', fPP.Options.BaseDirectory, 'BaseDirectory');
  //  CheckFalse(fPP.Options.WriteDefines, 'WriteDefines');
  //  CheckEqualsString('Defines\', fPP.Options.DefinesDirectory, 'DefinesDirectory');
  //  Check(fPP.Options.WriteXml, 'Xml');
  //  CheckEqualsString('Xml\', fPP.Options.XmlDirectory, 'XmlDirectory');
  //  Check(fPP.Options.DefinesUsed, 'DefinesUsed');
  //  CheckEqualsString('.used', fPP.Options.DefinesUsedFoE, 'UsedFileOrExtn');
  //  Check(fPP.Options.LoadDefines, 'LoadDefines');
  //  CheckEqualsString('', fPP.Options.Defines.CommaText, 'Defines');
  //  Check(fPP.Options.CountChildren, 'CountChildren');
  //  CheckEqualsString('.cnt', fPP.Options.CountChildrenFoE, 'CountFileOrExtn');
  //  Check(fPP.Options.SkipMethods, 'SkipMethods');
  //  CheckEqualsString('.skip', fPP.Options.SkipMethodsFoE, 'SkipFileOrExtn');
  //  Check(fPP.Options.ParseMode = pmFull, 'ParseMode');
  //  Check(fPP.Options.ResultPer = rpFile, 'ResultPer');
  //  Check(fPP.Options.ElapsedMode = emQuiet, 'ElapsedMode');
  //  Check(fPP.Options.FinalToken, 'FinalToken');
end;

procedure TestTD2XOptionsGeneral.TestReportOptions;
var
  ReturnValue: Boolean;
begin
  SetAllOptions;

  ReturnValue := fPP.ProcessParam('-@', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog(ALTERED_REPORT_OPTIONS);
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsDefault;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fPP.ProcessParam('-@', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog(EMPTY_REPORT_OPTIONS);
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsDefines;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fPP.ProcessParam('-D+CPU32', '', 0);
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fPP.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(DEFINED_REPORT_OPTIONS);
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsEmpty;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fPP.ProcessParam('-D:', '', 0);
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fPP.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(EMPTY_REPORT_OPTIONS);
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsExtn;
var
  ReturnValue: Boolean;
begin
  SetAllOptions;

  ReturnValue := fPP.ProcessParam('-@Test.tst', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsFile;
var
  ReturnValue: Boolean;
begin
  SetAllOptions;

  ReturnValue := fPP.ProcessParam('-@Test', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsFileDefault;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fPP.ProcessParam('-@Test', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsReset;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fPP.ProcessParam('-D!', '', 0);
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fPP.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(DEFAULT_REPORT_OPTIONS);
end;

procedure TestTD2XOptionsGeneral.TestResetOptions;
var
  ReturnValue: Boolean;

begin
  SetAllOptions;

  ReturnValue := fPP.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(ALTERED_REPORT_OPTIONS);

  ReturnValue := fPP.ProcessParam('-!', '', 0);
  Check(ReturnValue, 'ReturnValue');

  fSB.Clear;
  ReturnValue := fPP.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(DEFAULT_REPORT_OPTIONS);
end;

procedure TestTD2XOptionsGeneral.TestShowOptions;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fPP.ProcessParam('-?', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog(EXPECTED_SHOW_OPTIONS);
end;

procedure TestTD2XOptionsGeneral.TestZeroOptions;
var
  ReturnValue: Boolean;

begin
  SetAllOptions;

  ReturnValue := fPP.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(ALTERED_REPORT_OPTIONS);

  ReturnValue := fPP.ProcessParam('-!!', '', 0);
  Check(ReturnValue, 'ReturnValue');

  fSB.Clear;
  ReturnValue := fPP.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(ZERO_REPORT_OPTIONS);
end;

{ TOptionsTestCase }

procedure TOptionsTestCase.CheckLog(pMsg: string);
begin
  CheckEqualsString(pMsg, ReduceString(fSB.ToString), 'Log');
end;

procedure TOptionsTestCase.SetUp;
begin
  inherited;

  fPP := TD2XOptions.Create(fSB);
end;

procedure TOptionsTestCase.TearDown;
begin
  fPP := nil;

  inherited;
end;

initialization

RegisterTests('Options', [TestTD2XOptionEnums.Suite, TestTD2XOptionGeneral.Suite,
    TestTD2XFileOptions.Suite, TestTD2XOptions.Suite, TestTD2XOptionsGeneral.Suite]);

end.
