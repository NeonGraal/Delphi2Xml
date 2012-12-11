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
  BOTH_PROCESSING = UNIT_PROCESSING + ' ' + PROGRAM_PROCESSING;
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
    'E<mode> Quiet Set Elapsed time display to be (N[one], Q[uiet], T[otal], P[rocessing]) ' +
    'X[+-]:<dir> :Xml\ Generate XML files into current or given <dir> ' +
    'W[+-]:<dir> -(Defines\) Generate Final Defines files into current or given <dir> ' +
    'U[+-]:<f/e> :.used Report Defines Used into <f/e> ' +
    'C[+-]:<f/e> :.cnt Report Min/Max Children into <f/e> ' +
    'S[+-]:<f/e> :.skip Load Skipped Methods from <f/e> ' +
    'D[+-!:]<def> Add(+), Remove(-), Clear(!) or Load(:) Defines ' +
    'Definitions: <f/e> If value begins with "." is appended to global name to give file name';
  BASE_REPORT_OPTIONS =
    'Current option settings: Verbose - Log Errors + Log Not Supp - Final Token + ' +
  //    'Recurse - Timestamp - Global name Delphi2XmlTests Config dir :Config\ ' +
  //    'Log dir :Log\ Base dir -(.\) Parse mode Full Results per File Show elapsed Quiet ' +
    'Recurse - Parse mode Full Results per File Show elapsed Quiet ' +
    'Generate XML :Xml\ Write Defines -(Defines\) Defines Used :.used ' +
    'Count Children :.cnt Skipped Methods :.skip ';
  DEFAULT_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use default Defines';

implementation

uses
  D2X,
  D2X.IO,
  D2X.Params,
  System.StrUtils,
  System.SysUtils,
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

    procedure CheckFile(pFile: string);
    procedure CheckErrorLog(pExpected: string);
  protected
    procedure Invoke(AMethod: TTestMethod); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTD2XOptionsSpecific = class(TOptionsTestCase)
  published
    procedure TestCountChildren;
    procedure TestNotSupported;
    procedure TestLogErrors;
    procedure TestWriteDefines;

    procedure TestProcessXml;

    procedure TestProcessXmlPerFile;
    procedure TestProcessXmlPerWildcard;
    procedure TestProcessXmlPerSubDir;
    procedure TestProcessXmlPerDir;
    procedure TestProcessXmlPerParam;
    procedure TestProcessXmlPerRun;
  end;

  TestTD2XOptions = class(TOptionsTestCase)
  published
    procedure TestProcessOption;
    procedure TestConfigFileOrExtn;
    procedure TestBeginResults;
    procedure TestEndResults;
    procedure TestProcessStream;
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
  DIRECTORY_PROCESSING = 'Processing Config\Testing.TestDir.pas ... done';
  RECURSE_PROCESSING = 'Processing Config\Test\Testing.TestSubDir.pas ... done';
  ALTERED_REPORT_OPTIONS =
    'Current option settings: Verbose + Log Errors + Log Not Supp + Final Token + ' +
  //    'Recurse + Timestamp - Global name :Test Config dir :Test\ Log dir :Test\ ' +
  //    'Base dir :Test\ Parse mode Full Results per File Show elapsed Quiet ' +
    'Recurse + Parse mode Full Results per File Show elapsed Quiet ' +
    'Generate XML :Test\ Write Defines :Test\ Defines Used :.Test Count Children :.Test ' +
    'Skipped Methods :Test.skip Use these Defines: Tango, Uniform';
  ZERO_REPORT_OPTIONS =
    'Current option settings: Verbose - Log Errors - Log Not Supp - Final Token - ' +
  //    'Recurse - Timestamp - Global name Config dir - Log dir - Base dir - ' +
  //    'Parse mode Full Results per File Show elapsed None Generate XML - ' +
    'Recurse - Parse mode Full Results per File Show elapsed None Generate XML - ' +
    'Write Defines - Defines Used - Count Children - Skipped Methods - Use default Defines';
  EMPTY_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use NO Defines';
{$IFDEF WIN32}
  DEFINED_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use these Defines: ' +
    'CONDITIONALEXPRESSIONS, CPU32, CPU386, MSWINDOWS, UNICODE, VER230, WIN32';
{$ELSE}
  DEFINED_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Use these Defines: ' +
    'CONDITIONALEXPRESSIONS, CPU32, MSWINDOWS, UNICODE, VER230';
{$ENDIF}
  TESTFILE_DIR = '<?xml version="1.0"?> ' +
    '<D2X_File parseMode="Full" fileName="Config\Testing.TestDir.pas" lastToken="end.">' +
    'Testing.TestDir</D2X_File>';
  TESTFILE_SUBDIR = '<?xml version="1.0"?> ' +
    '<D2X_File parseMode="Full" fileName="Config\Test\Testing.TestSubDir.pas" lastToken="end.">'
    + 'Testing.TestSubDir</D2X_File>';
  TESTFILE_UNIT = '<?xml version="1.0"?> ' +
    '<D2X_File parseMode="Full" fileName="Testing.TestUnit.pas" ' +
    'lastToken=";{$ELSE}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}end.">Testing.TestUnitSystem.Classes</D2X_File>';
  TESTFILE_PROGRAM = '<?xml version="1.0"?> ' +
    '<D2X_File parseMode="Full" file="''Testing.TestUnit.pas''" fileName="Testing.TestProgram.dpr" '
    + 'lastToken=";beginend.">Testing.TestProgramTesting.TestUnit</D2X_File>';
  TESTFILE_INPUT = '<?xml version="1.0"?> <D2X_File parseMode="Full" fileName="(Input)" />';
  TESTFILE_DEF = '**** CONDITIONALEXPRESSIONS CPU386 MSWINDOWS ' +
    'TEST TEST1 TEST2 TEST3 TEST4 TEST5 TEST6 UNICODE VER230 WIN32';
  TESTFILE_XML = '<?xml version="1.0"?> ' +
    '<D2X_File parseMode="Full" fileName="Testing.TestUnit.pas"> <ParseFile> <UnitFile> ' +
    '<MainUnitName> <UnitName lastToken="Testing.TestUnit" /> </MainUnitName> ' +
    '<InterfaceSection> <UsesClause> <UsedUnitsList> <UsedUnitName> ' +
    '<UnitName lastToken="System.Classes" /> </UsedUnitName> </UsedUnitsList> ' +
    '<IncludeFile filename="TEST.INC" msgAt="68,0" /> ' +
    '<D2X_notSuppMsg msgAt="88,0">Currently not supported {$D+}</D2X_notSuppMsg> ' +
    '</UsesClause> </InterfaceSection> <ImplementationSection> ' +
    '<UsesClause lastToken=";{$ELSE}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}"> <UsedUnitsList> ' +
    '<UsedUnitName> <UnitName lastToken="System.SysUtils" /> </UsedUnitName> ' +
    '</UsedUnitsList> </UsesClause> </ImplementationSection> ' +
    '<InitializationSection lastToken="end" /> </UnitFile> </ParseFile> </D2X_File>';
  TESTFILE_XML_WILDCARD = '<?xml version="1.0"?> ' +
    '<D2X_Pattern parseMode="Full" fileName="Pattern-TestingTestpas"> ' +
    '<D2X_File fileName="Testing.TestUnit.pas"> <ParseFile> <UnitFile> <MainUnitName> ' +
    '<UnitName lastToken="Testing.TestUnit" /> </MainUnitName> <InterfaceSection> ' +
    '<UsesClause> <UsedUnitsList> <UsedUnitName> <UnitName lastToken="System.Classes" /> ' +
    '</UsedUnitName> </UsedUnitsList> <IncludeFile filename="TEST.INC" msgAt="68,0" /> ' +
    '<D2X_notSuppMsg msgAt="88,0">Currently not supported {$D+}</D2X_notSuppMsg> ' +
    '</UsesClause> </InterfaceSection> <ImplementationSection> ' +
    '<UsesClause lastToken=";{$ELSE}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}"> <UsedUnitsList> ' +
    '<UsedUnitName> <UnitName lastToken="System.SysUtils" /> </UsedUnitName> ' +
    '</UsedUnitsList> </UsesClause> </ImplementationSection> ' +
    '<InitializationSection lastToken="end" /> </UnitFile> </ParseFile> </D2X_File> </D2X_Pattern>';
  TESTFILE_XML_DIR = '<?xml version="1.0"?> <D2X_Dir parseMode="Full" fileName="."> ' +
    '<D2X_Pattern fileName="Pattern-TestingTestpas"> ' +
    '<D2X_File fileName="Testing.TestUnit.pas"> <ParseFile> <UnitFile> <MainUnitName> ' +
    '<UnitName lastToken="Testing.TestUnit" /> </MainUnitName> <InterfaceSection> ' +
    '<UsesClause> <UsedUnitsList> <UsedUnitName> <UnitName lastToken="System.Classes" /> ' +
    '</UsedUnitName> </UsedUnitsList> <IncludeFile filename="TEST.INC" msgAt="68,0" /> ' +
    '<D2X_notSuppMsg msgAt="88,0">Currently not supported {$D+}</D2X_notSuppMsg> ' +
    '</UsesClause> </InterfaceSection> <ImplementationSection> ' +
    '<UsesClause lastToken=";{$ELSE}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}"> <UsedUnitsList> ' +
    '<UsedUnitName> <UnitName lastToken="System.SysUtils" /> </UsedUnitName> ' +
    '</UsedUnitsList> </UsesClause> </ImplementationSection> ' +
    '<InitializationSection lastToken="end" /> </UnitFile> </ParseFile> </D2X_File> ' +
    '</D2X_Pattern> </D2X_Dir>';
  TESTFILE_XML_PARAM = '<?xml version="1.0"?> ' +
    '<D2X_Param parseMode="Full" fileName="XmlPerParam"> <D2X_Dir fileName="."> ' +
    '<D2X_Pattern fileName="Pattern-TestingTestpas"> ' +
    '<D2X_File fileName="Testing.TestUnit.pas"> <ParseFile> <UnitFile> <MainUnitName> ' +
    '<UnitName lastToken="Testing.TestUnit" /> </MainUnitName> <InterfaceSection> ' +
    '<UsesClause> <UsedUnitsList> <UsedUnitName> <UnitName lastToken="System.Classes" /> ' +
    '</UsedUnitName> </UsedUnitsList> <IncludeFile filename="TEST.INC" msgAt="68,0" /> ' +
    '<D2X_notSuppMsg msgAt="88,0">Currently not supported {$D+}</D2X_notSuppMsg> ' +
    '</UsesClause> </InterfaceSection> <ImplementationSection> ' +
    '<UsesClause lastToken=";{$ELSE}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}"> <UsedUnitsList> ' +
    '<UsedUnitName> <UnitName lastToken="System.SysUtils" /> </UsedUnitName> ' +
    '</UsedUnitsList> </UsesClause> </ImplementationSection> ' +
    '<InitializationSection lastToken="end" /> </UnitFile> </ParseFile> </D2X_File> </D2X_Pattern> </D2X_Dir> </D2X_Param>';

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
  C: Char;
begin
  for C := 'A' to 'Z' do
    if not fOpts.ProcessOption(C + ':Test') then
      fOpts.ProcessOption(C + '+');
  fOpts.ProcessOption('T-');
  fB.Clear;
end;

procedure TestTD2XOptionsGeneral.TestReportOptions;
begin
  SetAllOptions;

  Check(fOpts.ProcessOption('@'), 'Return Value');
  CheckLog(ALTERED_REPORT_OPTIONS, 'Report Options');
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
  CheckLog(ALTERED_REPORT_OPTIONS, 'Report Options');

  Check(fOpts.ProcessOption('!'), 'Return Value 2');

  fB.Clear;
  Check(fOpts.ProcessOption('@'), 'Return Value 3');
  CheckLog(DEFAULT_REPORT_OPTIONS, 'Report Options');
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
  CheckLog(ALTERED_REPORT_OPTIONS, 'Report Options');

  Check(fOpts.ProcessOption('!!'), 'Return Value 2');

  fB.Clear;
  Check(fOpts.ProcessOption('@'), 'Return Value 3');
  CheckLog(ZERO_REPORT_OPTIONS, 'Report Options');
end;

{ TestTD2XRunOptsAll }

procedure TestTD2XOptionsAll.SetUp;
begin
  inherited;

  fOpts.Defines.CommaText := 'Alpha,Beta,Gamma';
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
  Check(fOpts.ProcessOption('D+Value1'), 'ReturnValue1');
  Check(fOpts.ProcessOption('D+Value2'), 'ReturnValue2');
  fOpts.ProcessOption('@-D');
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

procedure TOptionsTestCase.CheckFile(pFile: string);
var
  lFile, lExpected: string;
begin
  lFile := 'Test\' + TidyFilename(pFile) + '.tst';
  ForceDirectories(ExtractFilePath(ExtractFilePath(ParamStr(0)) + lFile));
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
  CheckEqualsString(ReduceString(lExpected), ReduceString(fFact.CheckOutput(pFile)), pFile);
end;

procedure TOptionsTestCase.Invoke(AMethod: TTestMethod);
var
  lF: string;
begin
  inherited;

  for lF in fFact.CheckFiles do
    CheckFile(lF);
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
  Check(fOpts.ProcessDirectory('Config\', 'Testing.Test*'), 'Process Directory');
  CheckLog(DIRECTORY_PROCESSING, 'Process Directory');
  CheckErrorLog('''Implementation'' expected found ''end''');
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
  fS.WriteString('program ');
  fS.Position := 0;
  CheckFalse(fOpts.ProcessStream('(Stream)', fDS.ReadFrom), 'Process Stream');
  CheckLog(STREAM_PROCESSING, 'Process Stream');
  CheckErrorLog('(ESyntaxError)''Identifier'' expected found ''end of file''');

  fS.WriteString('Testing.Program; uses TestUnit in ''TestUnit.pas''; begin end.');
  fS.Position := 0;
  Check(fOpts.ProcessStream('(Stream)', fDS.ReadFrom), 'Process Stream');
  CheckLog(STREAM_PROCESSING, 'Process Stream');
end;

procedure TestTD2XOptions.TestRecurse;
begin
  CheckFalse(fOpts.Recurse, 'Recurse Default');
end;

procedure TestTD2XOptions.TestRecurseDirectory;
begin
  Check(fOpts.RecurseDirectory('', 'Testing.Test*', false), 'Recurse Directory');
  CheckLog(DIRECTORY_PROCESSING + ' ' + RECURSE_PROCESSING, 'Recurse Directory');
  CheckErrorLog('''Implementation'' expected found ''end''');
end;

{ TestTD2XOptionsSpecific }

procedure TestTD2XOptionsSpecific.TestCountChildren;
begin
  Check(fOpts.ProcessOption('!!'), 'Return Value 1');
  Check(fOpts.ProcessOption('C'), 'Return Value 2');
  fB.Clear;

  Check(fOpts.ProcessFile('Testing.TestUnit.pas'), 'Process Unit');
  CheckLog('', 'Process Unit');
end;

procedure TestTD2XOptionsSpecific.TestLogErrors;
begin
  Check(fOpts.ProcessOption('!!'), 'Return Value 1');
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
end;

procedure TestTD2XOptionsSpecific.TestNotSupported;
begin
  Check(fOpts.ProcessOption('!!'), 'Return Value 1');
  Check(fOpts.ProcessOption('N'), 'Return Value 3');
  fB.Clear;

  Check(fOpts.ProcessFile('Testing.TestUnit.pas'), 'Process Unit');
  CheckLog('', 'Process Unit');
  CheckErrorLog('Currently not supported {$D+}');
end;

procedure TestTD2XOptionsSpecific.TestProcessXml;
begin
  Check(fOpts.ProcessOption('!!'), 'Return Value 1');
  Check(fOpts.ProcessOption('E!'), 'Return Value 2');
  Check(fOpts.ProcessOption('X'), 'Return Value 3');
  Check(fOpts.ProcessOption('F'), 'Return Value 4');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Return Value 5');
  CheckLog(BOTH_PROCESSING, 'Processing Xml');
end;

procedure TestTD2XOptionsSpecific.TestProcessXmlPerDir;
begin
  Check(fOpts.ProcessOption('!!'), 'Return Value 1');
  Check(fOpts.ProcessOption('E!'), 'Return Value 2');
  Check(fOpts.ProcessOption('X'), 'Return Value 3');
  Check(fOpts.ProcessOption('PD'), 'Return Value 4');
  Check(fOpts.ProcessOption('F'), 'Return Value 4');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Return Value 5');
  CheckLog(BOTH_PROCESSING, 'Processing Xml');
end;

procedure TestTD2XOptionsSpecific.TestProcessXmlPerFile;
begin
  Check(fOpts.ProcessOption('!!'), 'Return Value 1');
  Check(fOpts.ProcessOption('E!'), 'Return Value 2');
  Check(fOpts.ProcessOption('X'), 'Return Value 3');
  Check(fOpts.ProcessOption('PF'), 'Return Value 4');
  Check(fOpts.ProcessOption('F'), 'Return Value 4');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Return Value 5');
  CheckLog(BOTH_PROCESSING, 'Processing Xml');
end;

procedure TestTD2XOptionsSpecific.TestProcessXmlPerParam;
begin
  Check(fOpts.ProcessOption('!!'), 'Return Value 1');
  Check(fOpts.ProcessOption('E!'), 'Return Value 2');
  Check(fOpts.ProcessOption('X'), 'Return Value 3');
  Check(fOpts.ProcessOption('PP'), 'Return Value 4');
  Check(fOpts.ProcessOption('F'), 'Return Value 4');
  Check(fOpts.ProcessParam('Testing.Test*', 'XmlPerParam'), 'Return Value 5');
  CheckLog(BOTH_PROCESSING, 'Processing Xml');
end;

procedure TestTD2XOptionsSpecific.TestProcessXmlPerRun;
begin
  Check(fOpts.ProcessOption('!!'), 'Return Value 1');
  Check(fOpts.ProcessOption('E!'), 'Return Value 2');
  Check(fOpts.ProcessOption('X'), 'Return Value 3');
  Check(fOpts.ProcessOption('PR'), 'Return Value 4');
  Check(fOpts.ProcessOption('F'), 'Return Value 4');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Return Value 5');
  CheckLog(BOTH_PROCESSING, 'Processing Xml');
end;

procedure TestTD2XOptionsSpecific.TestProcessXmlPerSubDir;
begin
  Check(fOpts.ProcessOption('!!'), 'Return Value 1');
  Check(fOpts.ProcessOption('E!'), 'Return Value 2');
  Check(fOpts.ProcessOption('X'), 'Return Value 3');
  Check(fOpts.ProcessOption('PS'), 'Return Value 4');
  Check(fOpts.ProcessOption('F'), 'Return Value 4');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Return Value 5');
  CheckLog(BOTH_PROCESSING, 'Processing Xml');
end;

procedure TestTD2XOptionsSpecific.TestProcessXmlPerWildcard;
begin
  Check(fOpts.ProcessOption('!!'), 'Return Value 1');
  Check(fOpts.ProcessOption('E!'), 'Return Value 2');
  Check(fOpts.ProcessOption('X'), 'Return Value 3');
  Check(fOpts.ProcessOption('PW'), 'Return Value 4');
  Check(fOpts.ProcessOption('F'), 'Return Value 4');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Return Value 5');
  CheckLog(BOTH_PROCESSING, 'Processing Xml');
end;

procedure TestTD2XOptionsSpecific.TestWriteDefines;
begin
  Check(fOpts.ProcessOption('!!'), 'Return Value 1');
  Check(fOpts.ProcessOption('W'), 'Return Value 2');
  fB.Clear;

  Check(fOpts.ProcessFile('Testing.TestUnit.pas'), 'Process Unit');
  CheckLog('', 'Process Unit');
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
