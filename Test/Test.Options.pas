unit Test.Options;

interface

uses
  D2X.Options,
  D2X.Param,
  System.Classes,
  Test.Global,
  Test.IO,
  TestFramework;

type
  TTestBoolFlag = class(TInterfacedObject, ID2XFlag)
  private
    fFlag: Boolean;

    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);
  end;

  TTestOptions = class(TD2XOptions)
    function ParamValue(pOpt: string): string;

    property Defines: TStringList read GetDefines;
  end;

const
  INPUT_PROCESSING = 'Processing (Input) ... done';
  UNIT_PROCESSING = 'Processing Testing.TestUnit.pas ... done';
  PROGRAM_PROCESSING = 'Processing Testing.TestProgram.dpr ... done';
  BOTH_PROCESSING = 'Processing ... ' + UNIT_PROCESSING + ' ' + PROGRAM_PROCESSING +
    ' Processed';
  EXPECTED_SHOW_OPTIONS = 'Usage: %s [ Option | @Params | mFilename | Wildcard ] ... ' +
    'Options: Default Description ? Show valid options ' +
    '! Reset all options to defaults @<file> Report/Output Current options ' +
    'V[+|-] - Log all Parser methods called L[+|-] + Log Error messages ' +
    'N[+|-] - Log Not Supported messages F[+|-] + Record Final Token ' +
    'R[+|-] - Recurse into subdirectories ' +
  //    'R[+|-] - Recurse into subdirectories T[+|-] - Timestamp global output files ' +
  //    'G<str> Delphi2XmlTests Sets global name ' +
  //    'I[+-]:<dir> :Config\ Use <dir> as a base for all Config files ' +
  //    'O[+-]:<dir> :Log\ Use <dir> as a base for all Log files ' +
  //    'B[+-]:<dir> -(.\) Use <dir> as a base for all Input files ' +
    'M<mode> Full Set Parsing mode (F[ull], U[ses]) ' +
    'P<per> File Set Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un]) ' +
    'E<mode> Quiet Set Elapsed time display to be (N[one], T[otal], D[ir], F[ile], P[rocessing], [Q]uiet) '
    + 'X[+-]:<dir> :Xml\ Generate XML files into current or given <dir> ' +
    'W[+-]:<dir> -(Defines\) Generate Final Defines files into current or given <dir> ' +
    'U[+-]:<f/e> :.used Report Defines Used into <f/e> ' +
    'CC[+-]:<f/e> :.chld Report Min/Max Children into <f/e> ' +
    'CD[+-]:<f/e> :.defs Report Defines Used into <f/e> ' +
    'S[+-]:<f/e> :.skip Load Skipped Methods from <f/e> ' +
    'D[+-!:]<def> Add(+), Remove(-), Clear(!) or Load(:) Defines ' +
    'Definitions: <f/e> If value begins with "." is appended to global name to give file name';
  BASE_REPORT_OPTIONS =
    'Current option settings: Verbose - Log Errors + Log Not Supp - Final Token + ' +
  //    'Recurse - Timestamp - Global name Delphi2XmlTests Config dir :Config\ ' +
  //    'Log dir :Log\ Base dir -(.\) Parse mode Full Results per File Show elapsed Quiet ' +
    'Recurse - Parse mode Full Results per File Show elapsed Quiet ' +
    'Generate XML :Xml\ Write Defines -(Defines\) Defines Used :.used ' +
    'Count Children :.chld Count Defines :.defs Skipped Methods :.skip ';
  DEFAULT_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use default Defines';

implementation

uses
  D2X,
  D2X.IO,
  D2X.Params,
  System.StrUtils,
  System.SysUtils,
  System.Types,
  Test.Param,
  Test.Utils,
  Winapi.Windows;

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

  TOptionsTestCase = class(TLoggerTestCase)
  private
    fFact: TTestFactory;
    fOpts: TTestOptions;

    procedure CheckFile(pMethod, pFile: string);
    procedure CheckFiles(pMethod: string);
    procedure CheckErrorLog(pExpected: string);
  protected
    procedure Invoke(AMethod: TTestMethod); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTD2XOptionsSpecific = class(TOptionsTestCase)
  public
    procedure SetUp; override;
  published
    procedure TestNotSupported;
    procedure TestLogErrors;
    procedure TestElapsedMode;

    procedure TestCountChildren;
    procedure TestCountDefines;
    procedure TestDefinesUsed;
    procedure TestLoadSkipped;
    procedure TestWriteDefines;

    procedure TestProcessXml;
    procedure TestProcessXmlUses;
  end;

  TestTD2XOptions = class(TOptionsTestCase)
  published
    procedure TestProcessOption;
    procedure TestConfigFileOrExtn;
    procedure TestBeginResults;
    procedure TestEndResults;
    procedure TestProcessStream;
    procedure TestProcessStreamError;
    procedure TestProcessInput;
    procedure TestProcessFile;
    procedure TestProcessDirectory;
    procedure TestRecurseDirectory;
    procedure TestProcessParam;
    procedure TestEndProcessing;
    procedure TestRecurse;
    procedure TestDefines;
    procedure TestGlobal;
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
    //    procedure TestParseOptionB;
    procedure TestParseOptionCC;
    procedure TestParseOptionCCBlank;
    procedure TestParseOptionCCExtn;
    procedure TestParseOptionCCOff;
    procedure TestParseOptionCCOn;
    procedure TestParseOptionCCFile;
    procedure TestParseOptionCD;
    procedure TestParseOptionCDBlank;
    procedure TestParseOptionCDExtn;
    procedure TestParseOptionCDOff;
    procedure TestParseOptionCDOn;
    procedure TestParseOptionCDFile;
    procedure TestParseOptionD;
    procedure TestParseOptionDAdd;
    procedure TestParseOptionDDelete;
    procedure TestParseOptionDClear;
    procedure TestParseOptionDEmpty;
    procedure TestParseOptionDAddLoad;
    procedure TestParseOptionDClearLoad;
    procedure TestParseOptionDMany;
    procedure TestParseOptionE;
    procedure TestParseOptionEValue;
    procedure TestParseOptionF;
    procedure TestParseOptionFOff;
    procedure TestParseOptionFOn;
    //    procedure TestParseOptionG;
    procedure TestParseOptionH;
    //    procedure TestParseOptionI;
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
    //    procedure TestParseOptionO;
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
    //    procedure TestParseOptionT;
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

const
  STREAM_PROCESSING = 'Processing (Stream) ... done';
  DIRECTORY_PROCESSING =
    'Processing Config ... Processing Config\Testing.TestDir.pas ... done Processed Config';
  RECURSE_PROCESSING =
    'Processing Config\Test ... Processing Config\Test\Testing.TestSubDir.pas ... done Processed Config\Test';
  ALL_PROCESSING = BOTH_PROCESSING + ' ' + DIRECTORY_PROCESSING + ' ' + RECURSE_PROCESSING;
  ALTERED_REPORT_OPTIONS =
    'Current option settings: Verbose + Log Errors + Log Not Supp + Final Token + ' +
  //    'Recurse + Timestamp - Global name :Test Config dir :Test\ Log dir :Test\ ' +
  //    'Base dir :Test\ Parse mode Full Results per File Show elapsed Quiet ' +
    'Recurse + Parse mode Full Results per File Show elapsed Quiet ' +
    'Generate XML :Test\ Write Defines :Test\ Defines Used :.Test Count Children :.Test ' +
    'Count Defines :.Test Skipped Methods :Test.skip Use these Defines: TANGO, UNIFORM, VICTOR';
  ZERO_REPORT_OPTIONS =
    'Current option settings: Verbose - Log Errors - Log Not Supp - Final Token - ' +
  //    'Recurse - Timestamp - Global name Config dir - Log dir - Base dir - ' +
  //    'Parse mode Full Results per File Show elapsed None Generate XML - ' +
    'Recurse - Parse mode Full Results per File Show elapsed None Generate XML - ' +
    'Write Defines - Defines Used - Count Children - Count Defines - Skipped Methods - ' +
    'Use default Defines';
  EMPTY_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use NO Defines';
{$IFDEF WIN32}
  DEFINED_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use these Defines: ' +
    'CONDITIONALEXPRESSIONS, CPU32, CPU386, MSWINDOWS, UNICODE, VER230, WIN32';
{$ELSE}
  DEFINED_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use these Defines: ' +
    'CONDITIONALEXPRESSIONS, CPU32, MSWINDOWS, UNICODE, VER230';
{$ENDIF}
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
  C, C1: Char;
begin
  for C := 'A' to 'Z' do
  begin
    if not fOpts.ProcessOption(C + ':Test') then
      fOpts.ProcessOption(C + '+');
    for C1 := 'A' to 'Z' do
      if not fOpts.ProcessOption(C + C1 + ':Test') then
        fOpts.ProcessOption(C + C1 + '+');
  end;
  fOpts.ProcessOption('T-');
  fB.Clear;
end;

procedure TestTD2XOptionsGeneral.TestReportOptions;
begin
  SetAllOptions;

  Check(fOpts.ProcessOption('@'), 'Return Value');
  CheckLog(ALTERED_REPORT_OPTIONS, 'Altered Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsDefault;
begin
  Check(fOpts.ProcessOption('@'), 'Return Value');
  CheckLog(DEFAULT_REPORT_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsDefines;
begin
  Check(fOpts.ProcessOption('D+CPU32'), 'Return Value 1');

  Check(fOpts.ProcessOption('@'), 'Return Value 2');
  CheckLog(DEFINED_REPORT_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsEmpty;
begin
  Check(fOpts.ProcessOption('D:'), 'Return Value 1');

  Check(fOpts.ProcessOption('@'), 'Return Value 2');
  CheckLog(EMPTY_REPORT_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsExtn;
begin
  SetAllOptions;

  Check(fOpts.ProcessOption('@Test.tst'), 'Return Value');
  CheckLog('', 'Report Options');
  //  CheckFile('Test.tst',
  //    '-V+ -N+ -R+ -X:Test\ -W:Test\ -U:.Test -C:.Test -S:Test.skip -D: -D+Tango -D+Uniform');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsFile;
begin
  SetAllOptions;

  Check(fOpts.ProcessOption('@Test'), 'Return Value');
  CheckLog('', 'Report Options');
  //  CheckFile('Test.prm',
  //    '-V+ -N+ -R+ -X:Test\ -W:Test\ -U:.Test -C:.Test -S:Test.skip -D: -D+Tango -D+Uniform');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsFileDefault;
begin
  Check(fOpts.ProcessOption('@Test'), 'ReturnValue');
  CheckLog('', 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestReportOptionsReset;
begin
  Check(fOpts.ProcessOption('!'), 'Return Value 1');

  Check(fOpts.ProcessOption('@'), 'Return Value 2');
  CheckLog(DEFAULT_REPORT_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestResetOptions;
begin
  SetAllOptions;

  Check(fOpts.ProcessOption('@'), 'Return Value 1');
  CheckLog(ALTERED_REPORT_OPTIONS, 'Altered Report Options');

  Check(fOpts.ProcessOption('!'), 'Return Value 2');

  fB.Clear;
  Check(fOpts.ProcessOption('@'), 'Return Value 3');
  CheckLog(DEFAULT_REPORT_OPTIONS, 'Default Report Options');
end;

procedure TestTD2XOptionsGeneral.TestShowOptions;
begin
  Check(fOpts.ProcessOption('?'), 'Return Value');
  CheckLog(EXPECTED_SHOW_OPTIONS, 'Report Options');
end;

procedure TestTD2XOptionsGeneral.TestZeroOptions;
begin
  SetAllOptions;

  Check(fOpts.ProcessOption('@'), 'Return Value 1');
  CheckLog(ALTERED_REPORT_OPTIONS, 'Altered Report Options');

  Check(fOpts.ProcessOption('!!'), 'Return Value 2');

  fB.Clear;
  Check(fOpts.ProcessOption('@'), 'Return Value 3');
  CheckLog(ZERO_REPORT_OPTIONS, 'Zero Report Options');
end;

{ TestTD2XRunOptsAll }

procedure TestTD2XOptionsAll.SetUp;
begin
  inherited;

  fOpts.Defines.CommaText := 'ALPHA,BETA,GAMMA';
end;

procedure TestTD2XOptionsAll.CheckInvalid(pOpt, pExp: string);
begin
  CheckFalse(fOpts.ProcessOption(pOpt), pOpt + ' Return Value');
  CheckLog(pExp, 'Report Options');
end;

procedure TestTD2XOptionsAll.CheckSimple(pOpt, pExp: string);
begin
  Check(fOpts.ProcessOption(pOpt), pOpt + ' Return Value');
  CheckLog('', 'Report Options');
  fOpts.ProcessOption('@-' + pOpt);
  CheckLog(pExp, 'Report Options');
end;

procedure TestTD2XOptionsAll.CheckUnknown(pOpt: string);
begin
  CheckFalse(fOpts.ProcessOption(pOpt), pOpt + ' Return Value');
  CheckLog('Unknown option: ' + pOpt, 'Report Options');
end;

procedure TestTD2XOptionsAll.TestParseOptionA;
begin
  CheckUnknown('A');
end;

procedure TestTD2XOptionsAll.TestParseOptionCC;
begin
  CheckSimple('CC', 'Count Children :.chld');
end;

procedure TestTD2XOptionsAll.TestParseOptionCCBlank;
begin
  CheckSimple('CC:', 'Count Children :.chld');
end;

procedure TestTD2XOptionsAll.TestParseOptionCCExtn;
begin
  CheckSimple('CC:Extn', 'Count Children :.Extn');
end;

procedure TestTD2XOptionsAll.TestParseOptionCCOff;
begin
  CheckSimple('CC-', 'Count Children -(.chld)');
end;

procedure TestTD2XOptionsAll.TestParseOptionCCOn;
begin
  CheckSimple('CC+', 'Count Children :.chld');
end;

procedure TestTD2XOptionsAll.TestParseOptionCCFile;
begin
  CheckSimple('CC:File.Extn', 'Count Children :File.Extn');
end;

procedure TestTD2XOptionsAll.TestParseOptionCD;
begin
  CheckSimple('CD', 'Count Defines :.defs');
end;

procedure TestTD2XOptionsAll.TestParseOptionCDBlank;
begin
  CheckSimple('CD:', 'Count Defines :.defs');
end;

procedure TestTD2XOptionsAll.TestParseOptionCDExtn;
begin
  CheckSimple('CD:Extn', 'Count Defines :.Extn');
end;

procedure TestTD2XOptionsAll.TestParseOptionCDOff;
begin
  CheckSimple('CD-', 'Count Defines -(.defs)');
end;

procedure TestTD2XOptionsAll.TestParseOptionCDOn;
begin
  CheckSimple('CD+', 'Count Defines :.defs');
end;

procedure TestTD2XOptionsAll.TestParseOptionCDFile;
begin
  CheckSimple('CD:File.Extn', 'Count Defines :File.Extn');
end;

procedure TestTD2XOptionsAll.TestParseOptionD;
begin
  CheckInvalid('D', 'Invalid Defines option: D');
end;

procedure TestTD2XOptionsAll.TestParseOptionDAdd;
begin
  CheckSimple('D+Value', 'Use these Defines: ALPHA, BETA, GAMMA, VALUE');
end;

procedure TestTD2XOptionsAll.TestParseOptionDClear;
begin
  CheckSimple('D!', 'Use default Defines');
end;

procedure TestTD2XOptionsAll.TestParseOptionDClearLoad;
begin
  CheckSimple('D:Test', 'Use these Defines: TANGO, UNIFORM, VICTOR');
end;

procedure TestTD2XOptionsAll.TestParseOptionDDelete;
begin
  CheckSimple('D-Beta', 'Use these Defines: ALPHA, GAMMA');
end;

procedure TestTD2XOptionsAll.TestParseOptionDEmpty;
begin
  CheckSimple('D:', 'Use NO Defines');
end;

procedure TestTD2XOptionsAll.TestParseOptionDAddLoad;
begin
  CheckSimple('D~Test', 'Use these Defines: ALPHA, BETA, GAMMA, TANGO, UNIFORM, VICTOR');
end;

procedure TestTD2XOptionsAll.TestParseOptionDMany;
begin
  Check(fOpts.ProcessOption('D+Value1'), 'ReturnValue1');
  Check(fOpts.ProcessOption('D+Value2'), 'ReturnValue2');
  fOpts.ProcessOption('@-D');
  CheckLog('Use these Defines: ALPHA, BETA, GAMMA, VALUE1, VALUE2', 'Report Options');
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

procedure TestTD2XOptionsAll.TestParseOptionH;
begin
  CheckUnknown('H');
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
  CheckEqualsString('Uses', fFact.GlobalName, 'GlobalName');
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
end;

procedure TestTD2XOptionsAll.TestParseOptionWBlank;
begin
  CheckSimple('W:', 'Write Defines +');
end;

procedure TestTD2XOptionsAll.TestParseOptionWOff;
begin
  CheckSimple('W-', 'Write Defines -(Defines\)');
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

{ TOptionsTestCase }

procedure TOptionsTestCase.CheckErrorLog(pExpected: string);
var
  lErr: string;
begin
  lErr := fFact.CheckOutput('.err');
  if ContainsText(lErr, pExpected) then
    Check(True, 'Error Log')
  else
    CheckEqualsString(pExpected, lErr, 'Error Log containing');
end;

procedure TOptionsTestCase.CheckFile(pMethod, pFile: string);
var
  lExtn, lFile, lExpected: string;
begin
  lExtn := '-' + pMethod + ExtractFileExt(pFile);
  lFile := 'Test\' + TidyFilename(ChangeFileExt(pFile, '')) + lExtn;
  ForceDirectories(ExtractFilePath(ExtractFilePath(ParamStr(0)) + 'Test'));
  if FileExists(lFile) then
    with TStreamReader.Create(lFile) do
      try
        lExpected := ReadToEnd;
      finally
        Free;
      end
  else
    with TStreamWriter.Create(lFile) do
      try
        lExpected := lFile + ' did not exist.';
        WriteLine(lExpected);
      finally
        Free;
      end;
  CheckEqualsString(ReduceString(lExpected), ReduceString(fFact.CheckOutput(pFile)),
    pFile + '-' + pMethod);
end;

procedure TOptionsTestCase.CheckFiles(pMethod: string);
var
  lF: string;
begin
  for lF in fFact.CheckFiles do
    CheckFile(pMethod, lF);
end;

procedure TOptionsTestCase.Invoke(AMethod: TTestMethod);
begin
  inherited;

  CheckFiles(FTestName);
end;

procedure TOptionsTestCase.SetUp;
begin
  inherited;

  fFact := TTestFactory.Create;
  fOpts := TTestOptions.Create;
  fOpts.JoinLog(fLog);
  fOpts.InitProcessors(fFact);
end;

procedure TOptionsTestCase.TearDown;
begin
  FreeAndNil(fOpts);

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
var
  ReturnValue: ID2XFile;
begin
  ReturnValue := fOpts.ConfigFileOrExtn('File');
  //  CheckEqualsString('Config\File', ReturnValue.Description, 'Default File');
  CheckEqualsString('File', ReturnValue.Description, 'Default File');
  DisposeOf(ReturnValue);

  ReturnValue := fOpts.ConfigFileOrExtn('.Extn');
  //  CheckEqualsString('Config\Delphi2XmlTests.Extn', ReturnValue.Description, 'Default Extn');
  CheckEqualsString('.Extn', ReturnValue.Description, 'Default Extn');
  DisposeOf(ReturnValue);

  ReturnValue := fOpts.ConfigFileOrExtn('File.Extn');
  //  CheckEqualsString('Config\File.Extn', ReturnValue.Description, 'Default File.Extn');
  CheckEqualsString('File.Extn', ReturnValue.Description, 'Default File.Extn');
  DisposeOf(ReturnValue);
end;

procedure TestTD2XOptions.TestDefines;
const
{$IFDEF WIN32}
  EXPECTED = 'CONDITIONALEXPRESSIONS CPU386 MSWINDOWS UNICODE VER230 WIN32';
{$ELSE}
  EXPECTED = 'CONDITIONALEXPRESSIONS MSWINDOWS UNICODE VER230';
{$ENDIF}
begin
  CheckList(EXPECTED, 'Defines', fOpts.Defines);
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

procedure TestTD2XOptions.TestGlobal;
begin
  fFact.SetGlobalName('Global');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Return Value');
  CheckLog(BOTH_PROCESSING, 'Process Param');
end;

procedure TestTD2XOptions.TestProcessDirectory;
begin
  Check(fOpts.ProcessDirectory('Config', 'Testing.Test*'), 'Process Directory');
  CheckLog(DIRECTORY_PROCESSING, 'Process Directory');
  CheckErrorLog('');
end;

procedure TestTD2XOptions.TestProcessFile;
begin
  Check(fOpts.ProcessFile('Testing.TestUnit.pas'), 'Process Unit');
  CheckLog(UNIT_PROCESSING, 'Process Unit');

  Check(fOpts.ProcessFile('Testing.TestProgram.dpr'), 'Process Program');
  CheckLog(PROGRAM_PROCESSING, 'Process Program');
end;

procedure TestTD2XOptions.TestProcessInput;
begin
  CloseHandle(GetStdHandle(STD_INPUT_HANDLE));
  Check(fOpts.ProcessInput, 'Process Input');
  CheckLog(INPUT_PROCESSING, 'Process Input');
end;

procedure TestTD2XOptions.TestProcessParam;
begin
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Return Value');
  CheckLog(BOTH_PROCESSING, 'Process Param');
end;

procedure TestTD2XOptions.TestProcessOption;
begin
  Check(fOpts.ProcessOption('?'), 'Return Value');
  CheckLog(EXPECTED_SHOW_OPTIONS, 'Nothing');
end;

procedure TestTD2XOptions.TestProcessStream;
begin
  fS.WriteString('program Testing.TestProgram; uses TestUnit in ''TestUnit.pas''; begin end.');
  fS.Position := 0;
  Check(fOpts.ProcessStream('(Stream)', fDS.ReadFrom), 'Process Stream');
  CheckLog(STREAM_PROCESSING, 'Process Stream');
end;

procedure TestTD2XOptions.TestProcessStreamError;
begin
  fS.WriteString('program ');
  fS.Position := 0;
  CheckFalse(fOpts.ProcessStream('(Stream)', fDS.ReadFrom), 'Process Stream');
  CheckLog(STREAM_PROCESSING, 'Process Stream');
  CheckErrorLog('(ESyntaxError)''Identifier'' expected found ''end of file''');
end;

procedure TestTD2XOptions.TestRecurse;
begin
  CheckFalse(fOpts.Recurse, 'Recurse Default');
end;

procedure TestTD2XOptions.TestRecurseDirectory;
begin
  Check(fOpts.RecurseDirectory('', 'Testing.Test*', false), 'Recurse Directory');
  CheckLog(DIRECTORY_PROCESSING + ' ' + RECURSE_PROCESSING, 'Recurse Directory');
  CheckErrorLog('');
end;

{ TestTD2XOptionsSpecific }

procedure TestTD2XOptionsSpecific.SetUp;
begin
  inherited;

  fOpts.ProcessOption('!!');
  fOpts.ProcessOption('R');
end;

procedure TestTD2XOptionsSpecific.TestCountChildren;
begin
  Check(fOpts.ProcessOption('CC'), 'Return Value 2');
  fB.Clear;

  Check(fOpts.ProcessParam('Testing.Test*', 'Count Children'), 'Process Unit');
  CheckLog('', 'Process Unit');

  fOpts.EndProcessing;
end;

procedure TestTD2XOptionsSpecific.TestCountDefines;
begin
  Check(fOpts.ProcessOption('CD'), 'Return Value 2');
  fB.Clear;

  Check(fOpts.ProcessParam('Testing.Test*', 'Count Defines'), 'Process Unit');
  CheckLog('', 'Process Unit');

  fOpts.EndProcessing;
end;

procedure TestTD2XOptionsSpecific.TestDefinesUsed;
begin
  Check(fOpts.ProcessOption('U'), 'Return Value 2');
  fB.Clear;

  Check(fOpts.ProcessParam('Testing.Test*', 'Defines Used'), 'Process Unit');
  CheckLog('', 'Process Unit');

  fOpts.EndProcessing;
end;

procedure TestTD2XOptionsSpecific.TestElapsedMode;
  function PJ(pA: array of string): string;
  var
    lS, lSep: string;
  begin
    Result := '';
    lSep := '';
    for lS in pA do
      if lS > '' then
      begin
        Result := Result + lSep + lS;
        lSep := ' ';
      end;
  end;
  function PB(pS: string): string;
  begin
    Result := PJ(['Processing', pS, '...']);
  end;
  function PE(pS: string): string;
  begin
    Result := PJ(['Processed', pS, 'in']);
  end;
  function PF(pS: string): string;
  begin
    Result := pS + ' 1.234';
  end;
  function PP(pS: string; pPs: array of integer): string;
  var
    lI: integer;
  begin
    Result := pS + ' ';
    for lI in pPs do
      Result := Result + Format('%3d%%%%'#8#8#8#8, [lI]);
    Result := Result + '1.234';
  end;
  function PD(pDir: string; pFiles: array of string): string;
  var
    lA: array of string;
    i: integer;
  begin
    SetLength(lA, Length(pFiles) + 2);
    lA[0] := PB(pDir);
    for i := 0 to High(pFiles) do
      if pDir > '' then
        lA[i + 1] := PF(PB(pDir + '\' + pFiles[i]))
      else
        lA[i + 1] := PF(PB(pFiles[i]));
    lA[High(lA)] := PF(PE(pDir));
    Result := PJ(lA);
  end;
  function PDP(pDir: string; pFiles: array of string;
    pPercs: array of TIntegerDynArray): string;
  var
    lA: array of string;
    i: integer;
  begin
    SetLength(lA, Length(pFiles) + 2);
    lA[0] := PB(pDir);
    for i := 0 to High(pFiles) do
      if pDir > '' then
        lA[i + 1] := PP(PB(pDir + '\' + pFiles[i]), pPercs[i])
      else
        lA[i + 1] := PP(PB(pFiles[i]), pPercs[i]);
    lA[High(lA)] := PF(PE(pDir));
    Result := ReduceString(PJ(lA));
  end;
  procedure SA(var pA: TIntegerDynArray; pS: array of integer);
  var
    i: integer;
  begin
    SetLength(pA, Length(pS));
    for i := 0 to High(pS) do
      pA[i] := pS[i];
  end;

var
  lP: array of TIntegerDynArray;
  lC: array of TIntegerDynArray;
  lT: array of TIntegerDynArray;

begin
  Check(fOpts.ProcessOption('EQ'), 'Return Value 1');
  fB.Clear;
  Check(fOpts.ProcessParam('Testing.Test*', 'Count Children'), 'Process Units');
  fOpts.EndProcessing;
  CheckLog(ALL_PROCESSING, 'Elapsed Quiet');

  Check(fOpts.ProcessOption('ER'), 'Return Value 1');
  fB.Clear;
  Check(fOpts.ProcessParam('Testing.Test*', 'Count Children'), 'Process Units');
  fOpts.EndProcessing;
  CheckLog(ALL_PROCESSING, 'Elapsed Run');

  Check(fOpts.ProcessOption('ED'), 'Return Value 1');
  fB.Clear;
  Check(fOpts.ProcessParam('Testing.Test*', 'Count Children'), 'Process Units');
  fOpts.EndProcessing;
  CheckLog(PJ([PF(PB('')), PF(PB('Config')), PF(PB('Config\Test'))]), 'Elapsed Dir');

  Check(fOpts.ProcessOption('EF'), 'Return Value 1');
  fB.Clear;
  Check(fOpts.ProcessParam('Testing.Test*', 'Count Children'), 'Process Units');
  fOpts.EndProcessing;
  CheckLog(PJ([PD('', ['Testing.TestUnit.pas', 'Testing.TestProgram.dpr']),
        PD('Config', ['Testing.TestDir.pas']), PD('Config\Test', ['Testing.TestSubDir.pas'])]),
    'Elapsed File');

  Check(fOpts.ProcessOption('EP'), 'Return Value 1');
  fB.Clear;
  Check(fOpts.ProcessParam('Testing.Test*', 'Count Children'), 'Process Units');
  fOpts.EndProcessing;
  SetLength(lP, 2);
  SA(lP[0], [1, 3, 6, 9, 10, 12, 13, 15, 31, 32, 34, 37, 47, 54, 65, 75, 98, 99, 100]);
  SA(lP[1], [7, 17, 18, 30, 31, 37, 46, 47, 56, 60, 86, 87, 94, 98, 100]);
  SetLength(lC, 1);
  SA(lC[0], [7, 23, 25, 39, 41, 60, 90, 98, 100]);
  SetLength(lT, 1);
  SA(lT[0], [7, 22, 24, 42, 44, 62, 90, 98, 100]);
  CheckLog(PJ([PDP('', ['Testing.TestUnit.pas', 'Testing.TestProgram.dpr'], lP),
        PDP('Config', ['Testing.TestDir.pas'], lC), PDP('Config\Test',
          ['Testing.TestSubDir.pas'], lT)]), 'Elapsed Processing');
end;

procedure TestTD2XOptionsSpecific.TestLoadSkipped;
begin
  Check(fOpts.ProcessOption('S'), 'Return Value 2');
  fB.Clear;

  Check(fOpts.ProcessParam('Testing.Test*', 'Load Skipped Methods'), 'Process Unit');
  CheckLog('', 'Process Unit');

  fOpts.EndProcessing;
end;

procedure TestTD2XOptionsSpecific.TestLogErrors;
begin
  Check(fOpts.ProcessOption('L'), 'Return Value 3');
  fB.Clear;

  fS.WriteString('unit Test; interface procedure A; beg');
  fS.Position := 0;
  CheckTrue(fOpts.ProcessStream('(Stream)', fDS.ReadFrom), 'Error Stream');
  CheckLog('', 'Error Stream');
  CheckErrorLog('''Point'' expected found ''beg''');

  fS.WriteString('in end; end.');
  fS.Position := 0;
  Check(fOpts.ProcessStream('(Stream)', fDS.ReadFrom), 'Process Stream');
  CheckLog('', 'Process Stream');

  fOpts.EndProcessing;
end;

procedure TestTD2XOptionsSpecific.TestNotSupported;
begin
  Check(fOpts.ProcessOption('N'), 'Return Value 3');
  fB.Clear;

  Check(fOpts.ProcessFile('Testing.TestUnit.pas'), 'Process Unit');

  CheckLog('', 'Process Unit');
  CheckErrorLog('Currently not supported {$D+}');

  fOpts.EndProcessing;
end;

procedure TestTD2XOptionsSpecific.TestProcessXml;
begin
  Check(fOpts.ProcessOption('E!'), 'Option 3');
  Check(fOpts.ProcessOption('X:'), 'Option 4');
  Check(fOpts.ProcessOption('F'), 'Option 5');

  Check(fOpts.ProcessOption('PD'), 'Option per Dir');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Dir');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Dir');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerDir');

  Check(fOpts.ProcessOption('PF'), 'Option per File');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per File');
  CheckLog(ALL_PROCESSING, 'Processing Xml per File');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerFile');

  Check(fOpts.ProcessOption('PP'), 'Option per Param');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per FilParam');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Param');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerParam');

  Check(fOpts.ProcessOption('PR'), 'Option per Run');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Run');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Run');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerRun');

  Check(fOpts.ProcessOption('PS'), 'Option per SubDir');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per SubDir');
  CheckLog(ALL_PROCESSING, 'Processing Xml per SubDir');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerSubDir');

  Check(fOpts.ProcessOption('PW'), 'Option per Wildcard');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Wildcard');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Wildcard');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerWildcard');
end;

procedure TestTD2XOptionsSpecific.TestProcessXmlUses;
begin
  Check(fOpts.ProcessOption('E!'), 'Option 2');
  Check(fOpts.ProcessOption('MU'), 'Option 3');
  Check(fOpts.ProcessOption('X:'), 'Option 4');
  Check(fOpts.ProcessOption('F'), 'Option 5');

  Check(fOpts.ProcessOption('PD'), 'Option per Dir');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Dir');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Dir');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerDir');

  Check(fOpts.ProcessOption('PF'), 'Option per File');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per File');
  CheckLog(ALL_PROCESSING, 'Processing Xml per File');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerFile');

  Check(fOpts.ProcessOption('PP'), 'Option per Param');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per FilParam');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Param');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerParam');

  Check(fOpts.ProcessOption('PR'), 'Option per Run');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Run');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Run');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerRun');

  Check(fOpts.ProcessOption('PS'), 'Option per SubDir');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per SubDir');
  CheckLog(ALL_PROCESSING, 'Processing Xml per SubDir');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerSubDir');

  Check(fOpts.ProcessOption('PW'), 'Option per Wildcard');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Wildcard');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Wildcard');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerWildcard');
end;

procedure TestTD2XOptionsSpecific.TestWriteDefines;
begin
  Check(fOpts.ProcessOption('W'), 'Return Value 2');
  fB.Clear;

  Check(fOpts.ProcessParam('Testing.Test*', 'Write Defines'), 'Process Unit');
  CheckLog('', 'Process Unit');

  fOpts.EndProcessing;
end;

{ TTestOptions }

function TTestOptions.ParamValue(pOpt: string): string;
var
  lPrm: TD2XParam;
begin
  lPrm := fParams.ForCode(Copy(pOpt, 1, 1));
  if Assigned(lPrm) then
    Result := lPrm.ToString
  else
    Result := '';
end;

initialization

RegisterTests('Options', [TestTD2XOptionEnums.Suite, TestTD2XOptions.Suite,
    TestTD2XOptionsAll.Suite, TestTD2XOptionsGeneral.Suite, TestTD2XOptionsSpecific.Suite]);

end.
