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

  TOptionsTestCase = class(TLoggerTestCase)
  private
    fOpts: TD2XOptions;

    function ParseOption(pOpt: string): Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTD2XOptions = class(TOptionsTestCase)
  published
    procedure TestProcessParamOption;
    procedure TestConfigFileOrExtn;
    procedure TestBeginResults;
    procedure TestEndResults;
    procedure TestProcessStream;
    procedure TestProcessInput;
    procedure TestProcessFile;
    procedure TestProcessDirectory;
    procedure TestRecurseDirectory;
    procedure TestEndProcessing;
    procedure TestRecurse;
    procedure TestDefines;
  end;

  TestTD2XOptionsAll = class(TOptionsTestCase)
  private
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

  TestTD2XOptionsGeneral = class(TOptionsTestCase)
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

  TestTD2XOptionsSpecific = class(TOptionsTestCase)
  published
    procedure TestCountChildren;
    procedure TestNotSupported;
    procedure TestWriteDefines;
  end;

  TTestRunOptions = class(TD2XRunOptions)
  private
    function GetDefines: TStringList;

  public
    property Defines: TStringList read GetDefines;

  end;

  TestTD2XRunOptions = class(TLoggerTestCase)
  private
    fIdx: Integer;

  protected
    fOpts: TTestRunOptions;

    function ParseOption(pOpt: string): Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEndProcessing;

    procedure TestProcessParam;
    procedure TestProcessParamPasFiles;
    procedure TestProcessParamParamFile;
    procedure TestProcessParamInput;
  end;

const
  INPUT_PROCESSING = 'Processing (Input) ... done';
  STREAM_PROCESSING = 'Processing (Stream) ... done';
  UNIT_PROCESSING = 'Processing TestUnit.pas ... done';
  PROGRAM_PROCESSING = 'Processing TestProgram.dpr ... done';
  RECURSE_PROCESSING = 'Processing Config\Test.pas ... done';
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

procedure TestTD2XRunOptions.TestProcessParam;
begin
  Check(ParseOption('-?'), 'Return Value');
  CheckLog(EXPECTED_SHOW_OPTIONS, 'Nothing');
end;

procedure TestTD2XRunOptions.TestProcessParamInput;
begin
  Check(ParseOption('-!!'), 'Return Value 1');
  Check(ParseOption('-E!'), 'Return Value 2');
  Check(ParseOption('-'), 'Return Value 3');
  CheckBuilder(INPUT_PROCESSING, 'Nothing');
end;

procedure TestTD2XRunOptions.TestProcessParamParamFile;
begin
  Check(ParseOption('@Test.prm'), 'Return Value');
  CheckBuilder(DEFAULT_REPORT_OPTIONS, 'Nothing');
end;

procedure TestTD2XRunOptions.TestProcessParamPasFiles;
begin
  Check(ParseOption('-!!'), 'Return Value 1');
  Check(ParseOption('-E!'), 'Return Value 2');
  Check(ParseOption('Test*.pas'), 'Return Value 3');
  CheckBuilder(UNIT_PROCESSING, 'Nothing');
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

procedure TestTD2XOptionsGeneral.SetAllOptions;
var
  C: Char;
begin
  for C := 'A' to 'Z' do
    if not ParseOption(C + ':Test') then
      ParseOption(C + '+');
  ParseOption('T-');
  fSB.Clear;
end;

procedure TestTD2XOptionsGeneral.TestReportOptions;
begin
  SetAllOptions;

  Check(ParseOption('@'), 'Return Value');
  CheckLog(ALTERED_REPORT_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsDefault;
begin
  Check(ParseOption('@'), 'Return Value');
  CheckLog(DEFAULT_REPORT_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsDefines;
begin
  Check(ParseOption('D+CPU32'), 'Return Value 1');

  Check(ParseOption('@'), 'Return Value 2');
  CheckLog(DEFINED_REPORT_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsEmpty;
begin
  Check(ParseOption('D:'), 'Return Value 1');

  Check(ParseOption('@'), 'Return Value 2');
  CheckLog(EMPTY_REPORT_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsExtn;
begin
  SetAllOptions;

  Check(ParseOption('@Test.tst'), 'Return Value');
  CheckLog('', 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsFile;
begin
  SetAllOptions;

  Check(ParseOption('@Test'), 'Return Value');
  CheckLog('', 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsFileDefault;
begin
  Check(ParseOption('@Test'), 'ReturnValue');
  CheckLog('', 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsReset;
begin
  Check(ParseOption('!'), 'Return Value 1');

  Check(ParseOption('@'), 'Return Value 2');
  CheckLog(DEFAULT_REPORT_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestResetOptions;
begin
  SetAllOptions;

  Check(ParseOption('@'), 'Return Value 1');
  CheckLog(ALTERED_REPORT_OPTIONS, 'Report Options');

  Check(ParseOption('!'), 'Return Value 2');

  fSB.Clear;
  Check(ParseOption('@'), 'Return Value 3');
  CheckLog(DEFAULT_REPORT_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestShowOptions;
begin
  Check(ParseOption('?'), 'Return Value');
  CheckLog(EXPECTED_SHOW_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestZeroOptions;
begin
  SetAllOptions;

  Check(ParseOption('@'), 'Return Value 1');
  CheckLog(ALTERED_REPORT_OPTIONS, 'Report Options');

  Check(ParseOption('!!'), 'Return Value 2');

  fSB.Clear;
  Check(ParseOption('@'), 'Return Value 3');
  CheckLog(ZERO_REPORT_OPTIONS, 'Report Options');
end;

{ TOptionsTestCase }

function TestTD2XRunOptions.ParseOption(pOpt: string): Boolean;
begin
  Result := fOpts.ProcessParam(pOpt, 'Test', fIdx);
  Inc(fIdx);
end;

procedure TestTD2XRunOptions.SetUp;
begin
  inherited;

  fOpts := TTestRunOptions.Create;
  fOpts.L.StartLog(fLog);

  fIdx := 0;
end;

procedure TestTD2XRunOptions.TearDown;
begin
  fOpts := nil;

  inherited;
end;

{ TestTD2XRunOptsAll }

procedure TestTD2XOptionsAll.SetUp;
begin
  inherited;

  fOpts.Defines.CommaText := 'Alpha,Beta,Gamma';
end;

procedure TestTD2XOptionsAll.CheckInvalid(pOpt, pExp: string);
begin
  CheckFalse(ParseOption(pOpt), pOpt + ' Return Value');
  CheckLog(pExp, 'Report Options');
end;

procedure TestTD2XOptionsAll.CheckSimple(pOpt, pExp: string);
begin
  Check(ParseOption(pOpt), pOpt + ' Return Value');
  CheckLog('', 'Report Options');
  ParseOption('@-' + pOpt);
  CheckLog(pExp, 'Report Options');
end;

procedure TestTD2XOptionsAll.CheckUnknown(pOpt: string);
begin
  CheckFalse(ParseOption(pOpt), pOpt + ' Return Value');
  CheckLog('Unknown option: ' + pOpt, 'Report Options');
end;

procedure TestTD2XOptionsAll.TestParseOptionA;
begin
  CheckUnknown('A');
end;

procedure TestTD2XOptionsAll.TestParseOptionB;
begin
  CheckSimple('B', 'Base dir +');
end;

procedure TestTD2XOptionsAll.TestParseOptionBBlank;
begin
  CheckSimple('B:', 'Base dir +');
end;

procedure TestTD2XOptionsAll.TestParseOptionBOff;
begin
  CheckSimple('B-', 'Base dir -');
end;

procedure TestTD2XOptionsAll.TestParseOptionBOn;
begin
  CheckSimple('B+', 'Base dir +');
end;

procedure TestTD2XOptionsAll.TestParseOptionBValue;
begin
  CheckSimple('B:Base', 'Base dir :Base\');
end;

procedure TestTD2XOptionsAll.TestParseOptionC;
begin
  CheckSimple('C', 'Count Children :.cnt');
end;

procedure TestTD2XOptionsAll.TestParseOptionCBlank;
begin
  CheckSimple('C:', 'Count Children :.cnt');
end;

procedure TestTD2XOptionsAll.TestParseOptionCExtn;
begin
  CheckSimple('C:Extn', 'Count Children :.Extn');
end;

procedure TestTD2XOptionsAll.TestParseOptionCOff;
begin
  CheckSimple('C-', 'Count Children -(.cnt)');
end;

procedure TestTD2XOptionsAll.TestParseOptionCOn;
begin
  CheckSimple('C+', 'Count Children :.cnt');
end;

procedure TestTD2XOptionsAll.TestParseOptionCFile;
begin
  CheckSimple('C:File.Extn', 'Count Children :File.Extn');
end;

procedure TestTD2XOptionsAll.TestParseOptionD;
begin
  CheckInvalid('D', 'Invalid Defines option: D');
end;

procedure TestTD2XOptionsAll.TestParseOptionDAdd;
begin
  CheckSimple('D+Value', 'Use these Defines: Alpha, Beta, Gamma, Value');
end;

procedure TestTD2XOptionsAll.TestParseOptionDClear;
begin
  CheckSimple('D!', 'Use default Defines');
end;

procedure TestTD2XOptionsAll.TestParseOptionDDelete;
begin
  CheckSimple('D-Beta', 'Use these Defines: Alpha, Gamma');
end;

procedure TestTD2XOptionsAll.TestParseOptionDEmpty;
begin
  CheckSimple('D:', 'Use NO Defines');
end;

procedure TestTD2XOptionsAll.TestParseOptionDLoad;
begin
  CheckSimple('D:Test', 'Use these Defines: Tango, Uniform');
end;

procedure TestTD2XOptionsAll.TestParseOptionDMany;
begin
  Check(ParseOption('D+Value1'), 'ReturnValue1');
  Check(ParseOption('D+Value2'), 'ReturnValue2');
  ParseOption('@-D');
  CheckLog('Use these Defines: Alpha, Beta, Gamma, Value1, Value2', 'Report Options');
end;

procedure TestTD2XOptionsAll.TestParseOptionE;
begin
  CheckInvalid('E', 'Invalid Show elapsed option: E');
end;

procedure TestTD2XOptionsAll.TestParseOptionEValue;
begin
  CheckSimple('ETotal', 'Show elapsed Total');
end;

procedure TestTD2XOptionsAll.TestParseOptionF;
begin
  CheckSimple('F', 'Final Token +');
end;

procedure TestTD2XOptionsAll.TestParseOptionFOff;
begin
  CheckSimple('F-', 'Final Token -');
end;

procedure TestTD2XOptionsAll.TestParseOptionFOn;
begin
  CheckSimple('F+', 'Final Token +');
end;

procedure TestTD2XOptionsAll.TestParseOptionG;
begin
  CheckSimple('G', 'Global name ' + ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
end;

procedure TestTD2XOptionsAll.TestParseOptionGValue;
begin
  CheckSimple('GGlobal', 'Global name Global');
  //  CheckEqualsString('Global\', fOpts.XmlDirectory, 'XmlDirectory');
  //  CheckEqualsString('Global\', fOpts.DefinesDirectory, 'DefinesDirectory');
end;

procedure TestTD2XOptionsAll.TestParseOptionH;
begin
  CheckUnknown('H');
end;

procedure TestTD2XOptionsAll.TestParseOptionI;
begin
  CheckSimple('I', 'Config dir :Config\');
  //  CheckEqualsString('Config\Input.File', fOpts.InputFileOrExtn('Input.File'),
  //    'InputFileOrExtn');
end;

procedure TestTD2XOptionsAll.TestParseOptionIBlank;
begin
  CheckSimple('I:', 'Config dir +');
  //  CheckEqualsString('Input.File', fOpts.InputFileOrExtn('Input.File'), 'InputFileOrExtn');
end;

procedure TestTD2XOptionsAll.TestParseOptionIOff;
begin
  CheckSimple('I-', 'Config dir -(Config\)');
  //  CheckEqualsString('Input.File', fOpts.InputFileOrExtn('Input.File'), 'InputFileOrExtn');
end;

procedure TestTD2XOptionsAll.TestParseOptionIOn;
begin
  CheckSimple('I+', 'Config dir :Config\');
  //  CheckEqualsString('Config\Input.File', fOpts.InputFileOrExtn('Input.File'),
  //    'InputFileOrExtn');
end;

procedure TestTD2XOptionsAll.TestParseOptionIValue;
begin
  CheckSimple('I:Input', 'Config dir :Input\');
  //  CheckEqualsString('Input\Input.File', fOpts.InputFileOrExtn('Input.File'),
  //    'InputFileOrExtn');
end;

procedure TestTD2XOptionsAll.TestParseOptionJ;
begin
  CheckUnknown('J');
end;

procedure TestTD2XOptionsAll.TestParseOptionK;
begin
  CheckUnknown('K');
end;

procedure TestTD2XOptionsAll.TestParseOptionL;
begin
  CheckSimple('L', 'Log Errors +');
end;

procedure TestTD2XOptionsAll.TestParseOptionLOff;
begin
  CheckSimple('L-', 'Log Errors -');
end;

procedure TestTD2XOptionsAll.TestParseOptionLOn;
begin
  CheckSimple('L+', 'Log Errors +');
end;

procedure TestTD2XOptionsAll.TestParseOptionM;
begin
  CheckInvalid('M', 'Invalid Parse mode option: M');
end;

procedure TestTD2XOptionsAll.TestParseOptionMValue;
begin
  CheckSimple('MUses', 'Parse mode Uses');
  //  CheckEqualsString('Uses', fOpts.GlobalName, 'GlobalName');
end;

procedure TestTD2XOptionsAll.TestParseOptionN;
begin
  CheckSimple('N', 'Log Not Supp +');
end;

procedure TestTD2XOptionsAll.TestParseOptionNOff;
begin
  CheckSimple('N-', 'Log Not Supp -');
end;

procedure TestTD2XOptionsAll.TestParseOptionNOn;
begin
  CheckSimple('N+', 'Log Not Supp +');
end;

procedure TestTD2XOptionsAll.TestParseOptionO;
begin
  CheckSimple('O', 'Log dir :Log\');
  //  CheckEqualsString('Log\Output.File', fOpts.OutputFileOrExtn('Output.File'),
  //    'OutputFileOrExtn');
end;

procedure TestTD2XOptionsAll.TestParseOptionOBlank;
begin
  CheckSimple('O:', 'Log dir +');
  //  CheckEqualsString('Output.File', fOpts.OutputFileOrExtn('Output.File'), 'OutputFileOrExtn');
end;

procedure TestTD2XOptionsAll.TestParseOptionOOff;
begin
  CheckSimple('O-', 'Log dir -(Log\)');
  //  CheckEqualsString('Output.File', fOpts.OutputFileOrExtn('Output.File'), 'OutputFileOrExtn');
end;

procedure TestTD2XOptionsAll.TestParseOptionOOn;
begin
  CheckSimple('O+', 'Log dir :Log\');
  //  CheckEqualsString('Log\Output.File', fOpts.OutputFileOrExtn('Output.File'),
  //    'OutputFileOrExtn');
end;

procedure TestTD2XOptionsAll.TestParseOptionOValue;
begin
  CheckSimple('O:Output', 'Log dir :Output\');
  //  CheckEqualsString('Output\Output.File', fOpts.OutputFileOrExtn('Output.File'),
  //    'OutputFileOrExtn');
end;

procedure TestTD2XOptionsAll.TestParseOptionP;
begin
  CheckInvalid('P', 'Invalid Results per option: P');
end;

procedure TestTD2XOptionsAll.TestParseOptionPValue;
begin
  CheckSimple('PDir', 'Results per Dir');
end;

procedure TestTD2XOptionsAll.TestParseOptionQ;
begin
  CheckUnknown('Q');
end;

procedure TestTD2XOptionsAll.TestParseOptionR;
begin
  CheckSimple('R', 'Recurse +');
end;

procedure TestTD2XOptionsAll.TestParseOptionROff;
begin
  CheckSimple('R-', 'Recurse -');
end;

procedure TestTD2XOptionsAll.TestParseOptionROn;
begin
  CheckSimple('R+', 'Recurse +');
end;

procedure TestTD2XOptionsAll.TestParseOptionS;
begin
  CheckSimple('S', 'Skipped Methods :.skip');
end;

procedure TestTD2XOptionsAll.TestParseOptionSBlank;
begin
  CheckSimple('S:', 'Skipped Methods :.skip');
end;

procedure TestTD2XOptionsAll.TestParseOptionSExtn;
begin
  CheckSimple('S:.Extn', 'Skipped Methods :.Extn');
end;

procedure TestTD2XOptionsAll.TestParseOptionSSimple;
begin
  CheckSimple('S:Extn', 'Skipped Methods :Extn.skip');
end;

procedure TestTD2XOptionsAll.TestParseOptionSFile;
begin
  CheckSimple('S:File.Extn', 'Skipped Methods :File.Extn');
end;

procedure TestTD2XOptionsAll.TestParseOptionSOff;
begin
  CheckSimple('S-', 'Skipped Methods -(.skip)');
end;

procedure TestTD2XOptionsAll.TestParseOptionSOn;
begin
  CheckSimple('S+', 'Skipped Methods :.skip');
  //  CheckEqualsString('.skip', fOpts.SkipMethodsFoE, 'SkipFileOrExtn');
end;

procedure TestTD2XOptionsAll.TestParseOptionT;
begin
  CheckSimple('T', 'Timestamp +');
  //  CheckEqualsString(FormatDateTime('-HH-mm', Now), fOpts.OutputTimestamp, 'OutputTimestamp');
end;

procedure TestTD2XOptionsAll.TestParseOptionTOff;
begin
  CheckSimple('T-', 'Timestamp -');
  //  CheckEqualsString(FormatDateTime('-HH-mm', Now), fOpts.OutputTimestamp, 'OutputTimestamp');
end;

procedure TestTD2XOptionsAll.TestParseOptionTOn;
begin
  CheckSimple('T+', 'Timestamp +');
  //  CheckEqualsString(FormatDateTime('-HH-mm', Now), fOpts.OutputTimestamp, 'OutputTimestamp');
end;

procedure TestTD2XOptionsAll.TestParseOptionU;
begin
  CheckSimple('U', 'Defines Used :.used');
end;

procedure TestTD2XOptionsAll.TestParseOptionUBlank;
begin
  CheckSimple('U:', 'Defines Used :.used');
end;

procedure TestTD2XOptionsAll.TestParseOptionUExtn;
begin
  CheckSimple('U:Extn', 'Defines Used :.Extn');
end;

procedure TestTD2XOptionsAll.TestParseOptionUFile;
begin
  CheckSimple('U:File.Extn', 'Defines Used :File.Extn');
end;

procedure TestTD2XOptionsAll.TestParseOptionUOff;
begin
  CheckSimple('U-', 'Defines Used -(.used)');
end;

procedure TestTD2XOptionsAll.TestParseOptionUOn;
begin
  CheckSimple('U+', 'Defines Used :.used');
end;

procedure TestTD2XOptionsAll.TestParseOptionV;
begin
  CheckSimple('V', 'Verbose +');
end;

procedure TestTD2XOptionsAll.TestParseOptionVOff;
begin
  CheckSimple('V-', 'Verbose -');
end;

procedure TestTD2XOptionsAll.TestParseOptionVOn;
begin
  CheckSimple('V+', 'Verbose +');
end;

procedure TestTD2XOptionsAll.TestParseOptionW;
begin
  CheckSimple('W', 'Write Defines :Defines\');
  //  CheckEqualsString('Defines\', fOpts.DefinesDirectory, 'DefinesDirectory');
end;

procedure TestTD2XOptionsAll.TestParseOptionWBlank;
begin
  CheckSimple('W:', 'Write Defines +');
  //  CheckEqualsString('', fOpts.DefinesDirectory, 'DefinesDirectory');
end;

procedure TestTD2XOptionsAll.TestParseOptionWOff;
begin
  CheckSimple('W-', 'Write Defines -(Defines\)');
  //  CheckEqualsString('Defines\', fOpts.DefinesDirectory, 'DefinesDirectory');
end;

procedure TestTD2XOptionsAll.TestParseOptionWOn;
begin
  CheckSimple('W+', 'Write Defines :Defines\');
end;

procedure TestTD2XOptionsAll.TestParseOptionWValue;
begin
  CheckSimple('W:Write', 'Write Defines :Write\');
end;

procedure TestTD2XOptionsAll.TestParseOptionX;
begin
  CheckSimple('X', 'Generate XML :Xml\');
end;

procedure TestTD2XOptionsAll.TestParseOptionXBlank;
begin
  CheckSimple('X:', 'Generate XML +');
end;

procedure TestTD2XOptionsAll.TestParseOptionXOff;
begin
  CheckSimple('X-', 'Generate XML -(Xml\)');
end;

procedure TestTD2XOptionsAll.TestParseOptionXOn;
begin
  CheckSimple('X+', 'Generate XML :Xml\');
end;

procedure TestTD2XOptionsAll.TestParseOptionXValue;
begin
  CheckSimple('X:Value', 'Generate XML :Value\');
end;

procedure TestTD2XOptionsAll.TestParseOptionY;
begin
  CheckUnknown('Y');
end;

procedure TestTD2XOptionsAll.TestParseOptionZ;
begin
  CheckUnknown('Z');
end;

{ TTestRunOptions }

function TTestRunOptions.GetDefines: TStringList;
begin
  Result := fOpts.Defines;
end;

{ TOptionsTestCase }

function TOptionsTestCase.ParseOption(pOpt: string): Boolean;
begin
  Result := fOpts.ProcessParamOption(pOpt);
end;

procedure TOptionsTestCase.SetUp;
begin
  inherited;

  fOpts := TD2XOptions.Create;
  fOpts.JoinLog(fLog);
end;

procedure TOptionsTestCase.TearDown;
begin
  fOpts := nil;

  inherited;
end;

{ TestTD2XOptions }

procedure TestTD2XOptions.TestBeginResults;
begin
  fOpts.BeginResults('File', rpFile);
  fOpts.BeginResults('Wildcard', rpWildcard);
  fOpts.BeginResults('SubDir', rpSubDir);
  fOpts.BeginResults('Dir', rpDir);
  fOpts.BeginResults('Param', rpParam);
  fOpts.BeginResults('Run', rpRun);
  CheckLog('', 'Begin Results');
end;

procedure TestTD2XOptions.TestConfigFileOrExtn;
begin
  CheckEqualsString('Config\File', fOpts.ConfigFileOrExtn('File'), 'Default File');
  CheckEqualsString('Config\Delphi2XmlTests.Extn', fOpts.ConfigFileOrExtn('.Extn'),
    'Default Extn');
  CheckEqualsString('Config\File.Extn', fOpts.ConfigFileOrExtn('File.Extn'),
    'Default File.Extn');
end;

procedure TestTD2XOptions.TestDefines;
begin
  CheckList('VER230 WIN32 CPU386 MSWINDOWS CONDITIONALEXPRESSIONS UNICODE', 'Defines',
    fOpts.Defines);
end;

procedure TestTD2XOptions.TestEndProcessing;
begin
  fOpts.EndProcessing;
  CheckLog('', 'End Processing');
end;

procedure TestTD2XOptions.TestEndResults;
begin
  fOpts.EndResults('File', rpFile);
  fOpts.EndResults('Wildcard', rpWildcard);
  fOpts.EndResults('SubDir', rpSubDir);
  fOpts.EndResults('Dir', rpDir);
  fOpts.EndResults('Param', rpParam);
  fOpts.EndResults('Run', rpRun);
  CheckLog('', 'End Results');
end;

procedure TestTD2XOptions.TestProcessDirectory;
begin
  Check(fOpts.ProcessDirectory('Config\', 'Test*.pas'), 'Process Directory');
  CheckLog(RECURSE_PROCESSING, 'Process Directory');
end;

procedure TestTD2XOptions.TestProcessFile;
begin
  Check(fOpts.ProcessFile('TestUnit.pas'), 'Process Unit');
  CheckLog(UNIT_PROCESSING, 'Process Unit');
  Check(fOpts.ProcessFile('TestProgram.dpr'), 'Process Program');
  CheckLog(PROGRAM_PROCESSING, 'Process Program');
end;

procedure TestTD2XOptions.TestProcessInput;
begin
  Check(fOpts.ProcessInput, 'Process Input');
  CheckLog(INPUT_PROCESSING, 'Process Input');
end;

procedure TestTD2XOptions.TestProcessParamOption;
begin
  Check(ParseOption('?'), 'Return Value');
  CheckLog(EXPECTED_SHOW_OPTIONS, 'Nothing');
end;

procedure TestTD2XOptions.TestProcessStream;
begin
  fSS.WriteString('unit Test');
  CheckFalse(fOpts.ProcessStream('(Stream)', fSS), 'Process Stream');
  CheckLog(STREAM_PROCESSING, 'Process Stream');

  fSS.WriteString('; end.');
  Check(fOpts.ProcessStream('(Stream)', fSS), 'Process Stream');
  CheckLog(STREAM_PROCESSING, 'Process Stream');
end;

procedure TestTD2XOptions.TestRecurse;
begin
  CheckFalse(fOpts.Recurse, 'Recurse Default');
end;

procedure TestTD2XOptions.TestRecurseDirectory;
begin
  Check(fOpts.RecurseDirectory('', 'Test*.pas', false), 'Recurse Directory');
  CheckLog(RECURSE_PROCESSING, 'Recurse Directory');
end;

{ TestTD2XOptionsSpecific }

procedure TestTD2XOptionsSpecific.TestCountChildren;
begin
  Check(ParseOption('!!'), 'Return Value 1');
  Check(ParseOption('C'), 'Return Value 2');
  fSB.Clear;

  Check(fOpts.ProcessFile('TestUnit.pas'), 'Process Unit');
  CheckLog('', 'Process Unit');
end;

procedure TestTD2XOptionsSpecific.TestNotSupported;
begin
  Check(ParseOption('!!'), 'Return Value 1');
  Check(ParseOption('G'), 'Return Value 2');
  Check(ParseOption('N'), 'Return Value 3');
  fSB.Clear;

  Check(fOpts.ProcessFile('TestUnit.pas'), 'Process Unit');
  CheckLog('', 'Process Unit');
end;

procedure TestTD2XOptionsSpecific.TestWriteDefines;
begin
  Check(ParseOption('!!'), 'Return Value 1');
  Check(ParseOption('W'), 'Return Value 2');
  fSB.Clear;

  Check(fOpts.ProcessFile('TestUnit.pas'), 'Process Unit');
  CheckLog('', 'Process Unit');
end;

initialization

RegisterTests('Options', [TestTD2XOptionEnums.Suite, TestTD2XOptionGeneral.Suite,
    TestTD2XFileOptions.Suite, TestTD2XOptions.Suite, TestTD2XOptionsAll.Suite,
    TestTD2XOptionsGeneral.Suite, TestTD2XOptionsSpecific.Suite, TestTD2XRunOptions.Suite]);

end.
