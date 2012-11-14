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
  TestTD2XOptionEnums = class(TLoggerTestCase)
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

  TRunOptsTestCase = class(TStringTestCase)
  protected
    fOpts: TD2XRunOptions;

    procedure CheckLog(pMsg: string);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTD2XRunOptions = class(TRunOptsTestCase)
  published
    procedure TestEndProcessing;

    procedure TestProcessParam;
    procedure TestProcessParamPasFiles;
    procedure TestProcessParamParamFile;

    procedure TestProcessCountChildren;
    procedure TestProcessVerbose;
  end;

  TestTD2XRunOptsAll = class(TRunOptsTestCase)
  private
    function ParseOption(pOpt: string): Boolean;
    procedure CheckUnknown(pOpt: string);
    procedure CheckInvalid(pOpt, pExp: string);
    procedure CheckSimple(pOpt, pExp: string);
  public
    procedure SetUp; override;
  published
    procedure TestParseOptionA;
    procedure TestParseOptionB;
    procedure TestParseOptionBOff;
    procedure TestParseOptionBOn;
    procedure TestParseOptionBBlank;
    procedure TestParseOptionBValue;
    procedure TestParseOptionC;
    procedure TestParseOptionCBlank;
    procedure TestParseOptionCExtn;
    procedure TestParseOptionCOff;
    procedure TestParseOptionCOn;
    procedure TestParseOptionCFile;
    procedure TestParseOptionD;
    procedure TestParseOptionDAdd;
    procedure TestParseOptionDDelete;
    procedure TestParseOptionDClear;
    procedure TestParseOptionDEmpty;
    procedure TestParseOptionDLoad;
    procedure TestParseOptionDMany;
    procedure TestParseOptionE;
    procedure TestParseOptionEValue;
    procedure TestParseOptionF;
    procedure TestParseOptionFOff;
    procedure TestParseOptionFOn;
    procedure TestParseOptionG;
    procedure TestParseOptionGValue;
    procedure TestParseOptionH;
    procedure TestParseOptionI;
    procedure TestParseOptionIOff;
    procedure TestParseOptionIOn;
    procedure TestParseOptionIBlank;
    procedure TestParseOptionIValue;
    procedure TestParseOptionJ;
    procedure TestParseOptionK;
    procedure TestParseOptionL;
    procedure TestParseOptionLOff;
    procedure TestParseOptionLOn;
    procedure TestParseOptionM;
    procedure TestParseOptionMValue;
    procedure TestParseOptionN;
    procedure TestParseOptionNOff;
    procedure TestParseOptionNOn;
    procedure TestParseOptionO;
    procedure TestParseOptionOOff;
    procedure TestParseOptionOOn;
    procedure TestParseOptionOBlank;
    procedure TestParseOptionOValue;
    procedure TestParseOptionP;
    procedure TestParseOptionPValue;
    procedure TestParseOptionQ;
    procedure TestParseOptionR;
    procedure TestParseOptionROff;
    procedure TestParseOptionROn;
    procedure TestParseOptionS;
    procedure TestParseOptionSBlank;
    procedure TestParseOptionSExtn;
    procedure TestParseOptionSFile;
    procedure TestParseOptionSOff;
    procedure TestParseOptionSOn;
    procedure TestParseOptionSSimple;
    procedure TestParseOptionT;
    procedure TestParseOptionTOff;
    procedure TestParseOptionTOn;
    procedure TestParseOptionU;
    procedure TestParseOptionUBlank;
    procedure TestParseOptionUExtn;
    procedure TestParseOptionUFile;
    procedure TestParseOptionUOff;
    procedure TestParseOptionUOn;
    procedure TestParseOptionV;
    procedure TestParseOptionVOff;
    procedure TestParseOptionVOn;
    procedure TestParseOptionW;
    procedure TestParseOptionWOff;
    procedure TestParseOptionWOn;
    procedure TestParseOptionWBlank;
    procedure TestParseOptionWValue;
    procedure TestParseOptionX;
    procedure TestParseOptionXOff;
    procedure TestParseOptionXOn;
    procedure TestParseOptionXBlank;
    procedure TestParseOptionXValue;
    procedure TestParseOptionY;
    procedure TestParseOptionZ;
  end;

  TestTD2XRunOptsGeneral = class(TRunOptsTestCase)
  private
    procedure SetAllOptions;
  published
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
    'I[+-]:<dir> :Config\ Use <dir> as a base for all Config files ' +
    'O[+-]:<dir> :Log\ Use <dir> as a base for all Log files ' +
    'B[+-]:<dir> - Use <dir> as a base for all Input files ' +
    'M<mode> Full Set Parsing mode (F[ull], U[ses]) ' +
    'P<per> File Set Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un]) ' +
    'E<mode> Quiet Set Elapsed time display to be (N[one], Q[uiet], T[otal], P[rocessing]) ' +
    'X[+-]:<dir> :Xml\ Generate XML files into current or given <dir> ' +
    'W[+-]:<dir> -(Defines\) Generate Final Defines files into current or given <dir> ' +
    'U[+-]:<f/e> :.used Report Defines Used into <f/e> ' +
    'C[+-]:<f/e> :.cnt Report Min/Max Children into <f/e> ' +
    'S[+-]:<f/e> :.skip Load Skipped Methods from <f/e> ' +
    'D[+-!:]<def> Add(+), Remove(-), Clear(!) or Load(:) Defines ' +
    'Definitions: <f/e> If value begins with "." is appended to global name to give file name';

  ALTERED_REPORT_OPTIONS =
    'Current option settings: Verbose + Log Errors + Log Not Supp + Final Token + ' +
    'Recurse + Timestamp - Global name :Test Config dir :Test\ Log dir :Test\ ' +
    'Base dir :Test\ Parse mode Full Results per File Show elapsed Quiet ' +
    'Generate XML :Test\ Write Defines :Test\ Defines Used :.Test Count Children :.Test ' +
    'Skipped Methods :Test.skip Use these Defines: Tango, Uniform';
  ZERO_REPORT_OPTIONS =
    'Current option settings: Verbose - Log Errors - Log Not Supp - Final Token - ' +
    'Recurse - Timestamp - Global name Config dir - Log dir - Base dir - ' +
    'Parse mode Full Results per File Show elapsed None Generate XML - ' +
    'Write Defines - Defines Used - Count Children - Skipped Methods - Use default Defines';
  BASE_REPORT_OPTIONS =
    'Current option settings: Verbose - Log Errors + Log Not Supp - Final Token + ' +
    'Recurse - Timestamp - Global name Delphi2XmlTests Config dir :Config\ ' +
    'Log dir :Log\ Base dir - Parse mode Full Results per File Show elapsed Quiet ' +
    'Generate XML :Xml\ Write Defines -(Defines\) Defines Used :.used ' +
    'Count Children :.cnt Skipped Methods :.skip ';
  DEFAULT_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use default Defines';
  EMPTY_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use NO Defines';
  DEFINED_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use these Defines: ' +
    'CONDITIONALEXPRESSIONS, CPU32, CPU386, MSWINDOWS, UNICODE, VER230, WIN32';

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

  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('In\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('In\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputDirFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I:In');

  ReturnValue := fFileOpts.ConfigFileOrExtn(pFile);

  CheckEqualsString('In\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOffExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I-');

  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString(fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOffFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I-');

  ReturnValue := fFileOpts.ConfigFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOnExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I+');

  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('Config\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('Config\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOnFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I+');

  ReturnValue := fFileOpts.ConfigFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';

  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('Config\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('Config\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fFileOpts.ConfigFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputDirExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O:Out');

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Out\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Out\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputDirFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O:Out');

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

  CheckEqualsString('Out\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputNoTimestampExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('T').Parse('T-');

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputNoTimestampFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('T').Parse('T-');

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOffExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O-');

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString(fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOffFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O-');

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOnExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O+');

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\Global.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOnFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O+');

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputTimestampExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('T').Parse('T');

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + fFileOpts.OutputTimestamp + '.Extn',
    ReturnValue, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
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

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

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
    lPrm.Describe(fLog);
    CheckLog('T<tst> Quiet Test Elapsed mode', 'Describe Param');
    lPrm.Report(fLog);
    CheckLog('Test Quiet', 'Report Default Value');
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
    lPrm.Describe(fLog);
    CheckLog('T<tst> Full Test Parse mode', 'Describe Param');
    lPrm.Report(fLog);
    CheckLog('Test Full', 'Report Default Value');
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
    lPrm.Describe(fLog);
    CheckLog('T<tst> File Test Parse mode', 'Describe Param');
    lPrm.Report(fLog);
    CheckLog('Test File', 'Report Default Value');
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

{ TestTD2XRunOptsAll }

procedure TestTD2XRunOptions.TestEndProcessing;
begin
  fOpts.EndProcessing;
  CheckBuilder('', 'Nothing');
end;

procedure TestTD2XRunOptions.TestProcessCountChildren;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '-!!';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := fOpts.ProcessParam(pStr, pFrom, pIdx);
  Check(ReturnValue, 'Return Value 1');

  pStr := '-C+';
  pIdx := 1;

  ReturnValue := fOpts.ProcessParam(pStr, pFrom, pIdx);
  Check(ReturnValue, 'Return Value 2');

  pStr := 'Test.pas';
  pIdx := 2;

  ReturnValue := fOpts.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckBuilder('', 'Empty Log');
end;

procedure TestTD2XRunOptions.TestProcessParam;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '-?';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := fOpts.ProcessParam(pStr, pFrom, pIdx);

  CheckTrue(ReturnValue, 'Return Value');
  CheckBuilder(EXPECTED_SHOW_OPTIONS, 'Nothing');
end;

procedure TestTD2XRunOptions.TestProcessParamParamFile;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '@Test.prm';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := fOpts.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckBuilder(DEFAULT_REPORT_OPTIONS, 'Nothing');
end;

procedure TestTD2XRunOptions.TestProcessParamPasFiles;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := 'Test.pas';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := fOpts.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckBuilder(EXPECTED_PROCESSING, 'Nothing');
end;

procedure TestTD2XRunOptions.TestProcessVerbose;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '-!!';
  pFrom := 'Test';
  pIdx := 0;
  fOpts.ProcessParam(pStr, pFrom, pIdx);

  pStr := '-V+';
  pIdx := 1;
  fOpts.ProcessParam(pStr, pFrom, pIdx);

  pStr := 'Test.pas';
  pIdx := 2;
  ReturnValue := fOpts.ProcessParam(pStr, pFrom, pIdx);

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

{ TestTD2XOptionsGeneral }

procedure TestTD2XRunOptsGeneral.SetAllOptions;
var
  C: Char;
begin
  for C := 'A' to 'Z' do
    if not fOpts.ProcessParam('-' + C + ':Test', '', 0) then
      fOpts.ProcessParam('-' + C + '+', '', 0);
  fOpts.ProcessParam('-T-', '', 0);
  fSB.Clear;
end;

procedure TestTD2XRunOptsGeneral.TestReportOptions;
var
  ReturnValue: Boolean;
begin
  SetAllOptions;

  ReturnValue := fOpts.ProcessParam('-@', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog(ALTERED_REPORT_OPTIONS);
end;

procedure TestTD2XRunOptsGeneral.TestReportOptionsDefault;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ProcessParam('-@', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog(DEFAULT_REPORT_OPTIONS);
end;

procedure TestTD2XRunOptsGeneral.TestReportOptionsDefines;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ProcessParam('-D+CPU32', '', 0);
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fOpts.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(DEFINED_REPORT_OPTIONS);
end;

procedure TestTD2XRunOptsGeneral.TestReportOptionsEmpty;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ProcessParam('-D:', '', 0);
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fOpts.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(EMPTY_REPORT_OPTIONS);
end;

procedure TestTD2XRunOptsGeneral.TestReportOptionsExtn;
var
  ReturnValue: Boolean;
begin
  SetAllOptions;

  ReturnValue := fOpts.ProcessParam('-@Test.tst', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XRunOptsGeneral.TestReportOptionsFile;
var
  ReturnValue: Boolean;
begin
  SetAllOptions;

  ReturnValue := fOpts.ProcessParam('-@Test', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XRunOptsGeneral.TestReportOptionsFileDefault;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ProcessParam('-@Test', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XRunOptsGeneral.TestReportOptionsReset;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ProcessParam('-D!', '', 0);
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fOpts.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(DEFAULT_REPORT_OPTIONS);
end;

procedure TestTD2XRunOptsGeneral.TestResetOptions;
var
  ReturnValue: Boolean;

begin
  SetAllOptions;

  ReturnValue := fOpts.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(ALTERED_REPORT_OPTIONS);

  ReturnValue := fOpts.ProcessParam('-!', '', 0);
  Check(ReturnValue, 'ReturnValue');

  fSB.Clear;
  ReturnValue := fOpts.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(DEFAULT_REPORT_OPTIONS);
end;

procedure TestTD2XRunOptsGeneral.TestShowOptions;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ProcessParam('-?', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog(EXPECTED_SHOW_OPTIONS);
end;

procedure TestTD2XRunOptsGeneral.TestZeroOptions;
var
  ReturnValue: Boolean;

begin
  SetAllOptions;

  ReturnValue := fOpts.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(ALTERED_REPORT_OPTIONS);

  ReturnValue := fOpts.ProcessParam('-!!', '', 0);
  Check(ReturnValue, 'ReturnValue');

  fSB.Clear;
  ReturnValue := fOpts.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(ZERO_REPORT_OPTIONS);
end;

{ TOptionsTestCase }

procedure TRunOptsTestCase.CheckLog(pMsg: string);
begin
  CheckEqualsString(pMsg, ReduceString(fSB.ToString), 'Log');
end;

procedure TRunOptsTestCase.SetUp;
begin
  inherited;

  fOpts := TD2XRunOptions.Create;
  fOpts.L.StartLog(fSB);
end;

procedure TRunOptsTestCase.TearDown;
begin
  fOpts := nil;

  inherited;
end;

{ TestTD2XRunOptsAll }

procedure TestTD2XRunOptsAll.SetUp;
begin
  inherited;

  fOpts.Defines.CommaText := 'Alpha,Beta,Gamma';
end;

procedure TestTD2XRunOptsAll.CheckInvalid(pOpt, pExp: string);
begin
  CheckFalse(ParseOption('-' + pOpt), pOpt + ' Return Value');
  CheckLog(pExp);
end;

procedure TestTD2XRunOptsAll.CheckSimple(pOpt, pExp: string);
begin
  Check(ParseOption('-' + pOpt), pOpt + ' Return Value');
  CheckLog('');
  ParseOption('-@-' + pOpt);
  CheckLog(pExp);
end;

procedure TestTD2XRunOptsAll.CheckUnknown(pOpt: string);
begin
  CheckFalse(ParseOption('-' + pOpt), pOpt + ' Return Value');
  CheckLog('Unknown option: ' + pOpt);
end;

function TestTD2XRunOptsAll.ParseOption(pOpt: string): Boolean;
begin
  Result := fOpts.ProcessParam(pOpt, 'test', 0);
end;

procedure TestTD2XRunOptsAll.TestParseOptionA;
begin
  CheckUnknown('A');
end;

procedure TestTD2XRunOptsAll.TestParseOptionB;
begin
  CheckSimple('B', 'Base dir +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionBBlank;
begin
  CheckSimple('B:', 'Base dir +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionBOff;
begin
  CheckSimple('B-', 'Base dir -');
end;

procedure TestTD2XRunOptsAll.TestParseOptionBOn;
begin
  CheckSimple('B+', 'Base dir +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionBValue;
begin
  CheckSimple('B:Base', 'Base dir :Base\');
end;

procedure TestTD2XRunOptsAll.TestParseOptionC;
begin
  CheckSimple('C', 'Count Children :.cnt');
end;

procedure TestTD2XRunOptsAll.TestParseOptionCBlank;
begin
  CheckSimple('C:', 'Count Children :.cnt');
end;

procedure TestTD2XRunOptsAll.TestParseOptionCExtn;
begin
  CheckSimple('C:Extn', 'Count Children :.Extn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionCOff;
begin
  CheckSimple('C-', 'Count Children -(.cnt)');
end;

procedure TestTD2XRunOptsAll.TestParseOptionCOn;
begin
  CheckSimple('C+', 'Count Children :.cnt');
end;

procedure TestTD2XRunOptsAll.TestParseOptionCFile;
begin
  CheckSimple('C:File.Extn', 'Count Children :File.Extn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionD;
begin
  CheckInvalid('D', 'Invalid Defines option: D');
end;

procedure TestTD2XRunOptsAll.TestParseOptionDAdd;
begin
  CheckSimple('D+Value', 'Use these Defines: Alpha, Beta, Gamma, Value');
end;

procedure TestTD2XRunOptsAll.TestParseOptionDClear;
begin
  CheckSimple('D!', 'Use default Defines');
end;

procedure TestTD2XRunOptsAll.TestParseOptionDDelete;
begin
  CheckSimple('D-Beta', 'Use these Defines: Alpha, Gamma');
end;

procedure TestTD2XRunOptsAll.TestParseOptionDEmpty;
begin
  CheckSimple('D:', 'Use NO Defines');
end;

procedure TestTD2XRunOptsAll.TestParseOptionDLoad;
begin
  CheckSimple('D:Test', 'Use these Defines: Tango, Uniform');
end;

procedure TestTD2XRunOptsAll.TestParseOptionDMany;
var
  ReturnValue: Boolean;
begin
  ReturnValue := ParseOption('-D+Value1');
  Check(ReturnValue, 'ReturnValue1');
  ReturnValue := ParseOption('-D+Value2');
  Check(ReturnValue, 'ReturnValue2');
  ParseOption('-@-D');
  CheckLog('Use these Defines: Alpha, Beta, Gamma, Value1, Value2');
end;

procedure TestTD2XRunOptsAll.TestParseOptionE;
begin
  CheckInvalid('E', 'Invalid Show elapsed option: E');
end;

procedure TestTD2XRunOptsAll.TestParseOptionEValue;
begin
  CheckSimple('ETotal', 'Show elapsed Total');
end;

procedure TestTD2XRunOptsAll.TestParseOptionF;
begin
  CheckSimple('F', 'Final Token +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionFOff;
begin
  CheckSimple('F-', 'Final Token -');
end;

procedure TestTD2XRunOptsAll.TestParseOptionFOn;
begin
  CheckSimple('F+', 'Final Token +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionG;
begin
  CheckSimple('G', 'Global name ' + ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
end;

procedure TestTD2XRunOptsAll.TestParseOptionGValue;
begin
  CheckSimple('GGlobal', 'Global name Global');
  //  CheckEqualsString('Global\', fOpts.XmlDirectory, 'XmlDirectory');
  //  CheckEqualsString('Global\', fOpts.DefinesDirectory, 'DefinesDirectory');
end;

procedure TestTD2XRunOptsAll.TestParseOptionH;
begin
  CheckUnknown('H');
end;

procedure TestTD2XRunOptsAll.TestParseOptionI;
begin
  CheckSimple('I', 'Config dir :Config\');
  //  CheckEqualsString('Config\Input.File', fOpts.InputFileOrExtn('Input.File'),
  //    'InputFileOrExtn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionIBlank;
begin
  CheckSimple('I:', 'Config dir +');
  //  CheckEqualsString('Input.File', fOpts.InputFileOrExtn('Input.File'), 'InputFileOrExtn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionIOff;
begin
  CheckSimple('I-', 'Config dir -(Config\)');
  //  CheckEqualsString('Input.File', fOpts.InputFileOrExtn('Input.File'), 'InputFileOrExtn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionIOn;
begin
  CheckSimple('I+', 'Config dir :Config\');
  //  CheckEqualsString('Config\Input.File', fOpts.InputFileOrExtn('Input.File'),
  //    'InputFileOrExtn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionIValue;
begin
  CheckSimple('I:Input', 'Config dir :Input\');
  //  CheckEqualsString('Input\Input.File', fOpts.InputFileOrExtn('Input.File'),
  //    'InputFileOrExtn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionJ;
begin
  CheckUnknown('J');
end;

procedure TestTD2XRunOptsAll.TestParseOptionK;
begin
  CheckUnknown('K');
end;

procedure TestTD2XRunOptsAll.TestParseOptionL;
begin
  CheckSimple('L', 'Log Errors +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionLOff;
begin
  CheckSimple('L-', 'Log Errors -');
end;

procedure TestTD2XRunOptsAll.TestParseOptionLOn;
begin
  CheckSimple('L+', 'Log Errors +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionM;
begin
  CheckInvalid('M', 'Invalid Parse mode option: M');
end;

procedure TestTD2XRunOptsAll.TestParseOptionMValue;
begin
  CheckSimple('MUses', 'Parse mode Uses');
  //  CheckEqualsString('Uses', fOpts.GlobalName, 'GlobalName');
end;

procedure TestTD2XRunOptsAll.TestParseOptionN;
begin
  CheckSimple('N', 'Log Not Supp +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionNOff;
begin
  CheckSimple('N-', 'Log Not Supp -');
end;

procedure TestTD2XRunOptsAll.TestParseOptionNOn;
begin
  CheckSimple('N+', 'Log Not Supp +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionO;
begin
  CheckSimple('O', 'Log dir :Log\');
  //  CheckEqualsString('Log\Output.File', fOpts.OutputFileOrExtn('Output.File'),
  //    'OutputFileOrExtn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionOBlank;
begin
  CheckSimple('O:', 'Log dir +');
  //  CheckEqualsString('Output.File', fOpts.OutputFileOrExtn('Output.File'), 'OutputFileOrExtn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionOOff;
begin
  CheckSimple('O-', 'Log dir -(Log\)');
  //  CheckEqualsString('Output.File', fOpts.OutputFileOrExtn('Output.File'), 'OutputFileOrExtn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionOOn;
begin
  CheckSimple('O+', 'Log dir :Log\');
  //  CheckEqualsString('Log\Output.File', fOpts.OutputFileOrExtn('Output.File'),
  //    'OutputFileOrExtn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionOValue;
begin
  CheckSimple('O:Output', 'Log dir :Output\');
  //  CheckEqualsString('Output\Output.File', fOpts.OutputFileOrExtn('Output.File'),
  //    'OutputFileOrExtn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionP;
begin
  CheckInvalid('P', 'Invalid Results per option: P');
end;

procedure TestTD2XRunOptsAll.TestParseOptionPValue;
begin
  CheckSimple('PDir', 'Results per Dir');
end;

procedure TestTD2XRunOptsAll.TestParseOptionQ;
begin
  CheckUnknown('Q');
end;

procedure TestTD2XRunOptsAll.TestParseOptionR;
begin
  CheckSimple('R', 'Recurse +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionROff;
begin
  CheckSimple('R-', 'Recurse -');
end;

procedure TestTD2XRunOptsAll.TestParseOptionROn;
begin
  CheckSimple('R+', 'Recurse +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionS;
begin
  CheckSimple('S', 'Skipped Methods :.skip');
end;

procedure TestTD2XRunOptsAll.TestParseOptionSBlank;
begin
  CheckSimple('S:', 'Skipped Methods :.skip');
end;

procedure TestTD2XRunOptsAll.TestParseOptionSExtn;
begin
  CheckSimple('S:.Extn', 'Skipped Methods :.Extn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionSSimple;
begin
  CheckSimple('S:Extn', 'Skipped Methods :Extn.skip');
end;

procedure TestTD2XRunOptsAll.TestParseOptionSFile;
begin
  CheckSimple('S:File.Extn', 'Skipped Methods :File.Extn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionSOff;
begin
  CheckSimple('S-', 'Skipped Methods -(.skip)');
end;

procedure TestTD2XRunOptsAll.TestParseOptionSOn;
begin
  CheckSimple('S+', 'Skipped Methods :.skip');
  //  CheckEqualsString('.skip', fOpts.SkipMethodsFoE, 'SkipFileOrExtn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionT;
begin
  CheckSimple('T', 'Timestamp +');
  //  CheckEqualsString(FormatDateTime('-HH-mm', Now), fOpts.OutputTimestamp, 'OutputTimestamp');
end;

procedure TestTD2XRunOptsAll.TestParseOptionTOff;
begin
  CheckSimple('T-', 'Timestamp -');
  //  CheckEqualsString(FormatDateTime('-HH-mm', Now), fOpts.OutputTimestamp, 'OutputTimestamp');
end;

procedure TestTD2XRunOptsAll.TestParseOptionTOn;
begin
  CheckSimple('T+', 'Timestamp +');
  //  CheckEqualsString(FormatDateTime('-HH-mm', Now), fOpts.OutputTimestamp, 'OutputTimestamp');
end;

procedure TestTD2XRunOptsAll.TestParseOptionU;
begin
  CheckSimple('U', 'Defines Used :.used');
end;

procedure TestTD2XRunOptsAll.TestParseOptionUBlank;
begin
  CheckSimple('U:', 'Defines Used :.used');
end;

procedure TestTD2XRunOptsAll.TestParseOptionUExtn;
begin
  CheckSimple('U:Extn', 'Defines Used :.Extn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionUFile;
begin
  CheckSimple('U:File.Extn', 'Defines Used :File.Extn');
end;

procedure TestTD2XRunOptsAll.TestParseOptionUOff;
begin
  CheckSimple('U-', 'Defines Used -(.used)');
end;

procedure TestTD2XRunOptsAll.TestParseOptionUOn;
begin
  CheckSimple('U+', 'Defines Used :.used');
end;

procedure TestTD2XRunOptsAll.TestParseOptionV;
begin
  CheckSimple('V', 'Verbose +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionVOff;
begin
  CheckSimple('V-', 'Verbose -');
end;

procedure TestTD2XRunOptsAll.TestParseOptionVOn;
begin
  CheckSimple('V+', 'Verbose +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionW;
begin
  CheckSimple('W', 'Write Defines :Defines\');
  //  CheckEqualsString('Defines\', fOpts.DefinesDirectory, 'DefinesDirectory');
end;

procedure TestTD2XRunOptsAll.TestParseOptionWBlank;
begin
  CheckSimple('W:', 'Write Defines +');
  //  CheckEqualsString('', fOpts.DefinesDirectory, 'DefinesDirectory');
end;

procedure TestTD2XRunOptsAll.TestParseOptionWOff;
begin
  CheckSimple('W-', 'Write Defines -(Defines\)');
  //  CheckEqualsString('Defines\', fOpts.DefinesDirectory, 'DefinesDirectory');
end;

procedure TestTD2XRunOptsAll.TestParseOptionWOn;
begin
  CheckSimple('W+', 'Write Defines :Defines\');
end;

procedure TestTD2XRunOptsAll.TestParseOptionWValue;
begin
  CheckSimple('W:Write', 'Write Defines :Write\');
end;

procedure TestTD2XRunOptsAll.TestParseOptionX;
begin
  CheckSimple('X', 'Generate XML :Xml\');
end;

procedure TestTD2XRunOptsAll.TestParseOptionXBlank;
begin
  CheckSimple('X:', 'Generate XML +');
end;

procedure TestTD2XRunOptsAll.TestParseOptionXOff;
begin
  CheckSimple('X-', 'Generate XML -(Xml\)');
end;

procedure TestTD2XRunOptsAll.TestParseOptionXOn;
begin
  CheckSimple('X+', 'Generate XML :Xml\');
end;

procedure TestTD2XRunOptsAll.TestParseOptionXValue;
begin
  CheckSimple('X:Value', 'Generate XML :Value\');
end;

procedure TestTD2XRunOptsAll.TestParseOptionY;
begin
  CheckUnknown('Y');
end;

procedure TestTD2XRunOptsAll.TestParseOptionZ;
begin
  CheckUnknown('Z');
end;

initialization

RegisterTests('Options', [TestTD2XOptionEnums.Suite, TestTD2XOptionGeneral.Suite,
    TestTD2XFileOptions.Suite, TestTD2XRunOptions.Suite, TestTD2XRunOptsAll.Suite,
    TestTD2XRunOptsGeneral.Suite]);

end.
