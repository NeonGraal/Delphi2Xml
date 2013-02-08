unit Test.Options.Tests;

interface

implementation

uses
  D2X,
  D2X.IO,
  D2X.Param,
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  System.Types,
  Test.Utils,
  Test.Constants,
  Test.Global,
  Test.IO,
  Test.Options,
  TestFramework,
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

    procedure TestElapsedModeParamCreates;
    procedure TestParseModeParamCreates;
    procedure TestResultPerParamCreates;
  end;

  TOptionsTestCase = class(TLoggerTestCase)
  private
    fFact: TTestFactory;
    fOpts: TTestOptions;

    function CheckFile(pMethod, pFile: string): string;
    procedure CheckFiles(pMethod: string);
    procedure CheckErrorLog(pExpected: string);
  protected
    procedure Invoke(AMethod: TTestMethod); override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TOptionsParseTestCase = class(TOptionsTestCase)
  protected
    procedure CheckSimple(pOpt, pExp: string);
  end;

  TOptionsParseDefinesTestCase = class(TOptionsParseTestCase)
  protected
    fCode, fLabel: string;
    procedure CheckDefines(pParam, pResult: string);
  end;

  TestTD2XOptionsSpecific = class(TOptionsTestCase)
  public
    procedure SetUp; override;
  published
    procedure TestNotSupported;
    procedure TestLogErrors;
    procedure TestElapsedMode;

    procedure TestCountChildren;
    procedure TestCountFinalDefines;
    procedure TestCountDefinesUsed;
    procedure TestLoadSkipped;
    procedure TestWriteDefines;
  end;

  TestTD2XOptionsXmlPer = class(TOptionsTestCase)
  public
    procedure SetUp; override;
  published
    procedure TestProcessXmlPerDir;
    procedure TestProcessXmlPerFile;
    procedure TestProcessXmlPerParam;
    procedure TestProcessXmlPerRun;
    procedure TestProcessXmlPerSubDir;
    procedure TestProcessXmlPerWildcard;

    procedure TestProcessXmlUsesPerDir;
    procedure TestProcessXmlUsesPerFile;
    procedure TestProcessXmlUsesPerParam;
    procedure TestProcessXmlUsesPerRun;
    procedure TestProcessXmlUsesPerSubDir;
    procedure TestProcessXmlUsesPerWildcard;
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
    procedure TestDescribeAll;
    procedure TestEndProcessing;
    procedure TestRecurse;
    procedure TestParserDefines;
    procedure TestHeldDefines;
    procedure TestGlobal;
  end;

  TestTD2XOptionsParseAll = class(TOptionsParseTestCase)
  private
    procedure CheckInvalid(pOpt, pExp: string);
  published
    //    procedure TestParseOptionB; // Tested in Test.IO.Options
    procedure TestParseOptionCC;
    procedure TestParseOptionCF;
    procedure TestParseOptionCU;
    procedure TestParseOptionD;
    procedure TestParseOptionE;
    procedure TestParseOptionF;
    //    procedure TestParseOptionG; // Tested in Test.IO.Options
    procedure TestParseOptionH;
    //    procedure TestParseOptionI; // Tested in Test.IO.Options
    procedure TestParseOptionM;
    //    procedure TestParseOptionO; // Tested in Test.IO.Options
    procedure TestParseOptionP;
    procedure TestParseOptionS;
    //    procedure TestParseOptionT; // Tested in Test.IO.Options
    procedure TestParseOptionWD;
    procedure TestParseOptionWX;
  end;

  TestTD2XOptionsParseUnused = class(TOptionsParseTestCase)
  private
    procedure CheckUnknown(pOpt: string);
  published
    procedure TestParseOptionA;
    procedure TestParseOptionC;
    procedure TestParseOptionJ;
    procedure TestParseOptionK;
    procedure TestParseOptionL;
    procedure TestParseOptionN;
    procedure TestParseOptionQ;
    procedure TestParseOptionR;
    procedure TestParseOptionU;
    procedure TestParseOptionV;
    procedure TestParseOptionW;
    procedure TestParseOptionX;
    procedure TestParseOptionY;
    procedure TestParseOptionZ;
  end;

  TestTD2XOptionsParseEnumerated = class(TOptionsParseTestCase)
  published
    procedure TestParseOptionEValue;
    procedure TestParseOptionMValue;
    procedure TestParseOptionPValue;

    procedure TestConvertEnumerated;
  end;

  TestTD2XOptionsParseFlags = class(TOptionsTestCase)
  protected
    procedure CheckFlag(pFlag, pDesc: string);
  published
    procedure TestParseOptionFinal;
    procedure TestParseOptionLogErrors;
    procedure TestParseOptionLogNotSupported;
    procedure TestParseOptionRecurse;
    procedure TestParseOptionTimestamp;
    procedure TestParseOptionVerbose;
  end;

  TestTD2XOptionsParseCountChildren = class(TOptionsParseTestCase)
  published
    procedure TestParseOptionCCBlank;
    procedure TestParseOptionCCExtn;
    procedure TestParseOptionCCOff;
    procedure TestParseOptionCCOn;
    procedure TestParseOptionCCFile;
  end;

  TestTD2XOptionsParseCountFinalDefines = class(TOptionsParseTestCase)
  published
    procedure TestParseOptionCFBlank;
    procedure TestParseOptionCFExtn;
    procedure TestParseOptionCFOff;
    procedure TestParseOptionCFOn;
    procedure TestParseOptionCFFile;
  end;

  TestTD2XOptionsParseDefines = class(TOptionsParseDefinesTestCase)
  public
    procedure SetUp; override;
  published
    procedure TestParseOptionDAdd;
    procedure TestParseOptionDAddLoad;
    procedure TestParseOptionDAddMany;
    procedure TestParseOptionDDelete;
    procedure TestParseOptionDDeleteLoad;
    procedure TestParseOptionDDeleteMany;
    procedure TestParseOptionDClear;
    procedure TestParseOptionDEmpty;
    procedure TestParseOptionDClearLoad;
    procedure TestParseOptionDMany;
  end;

  TestTD2XOptionsParseHeldDefines = class(TOptionsParseDefinesTestCase)
  public
    procedure SetUp; override;
  published
    procedure TestParseOptionHAdd;
    procedure TestParseOptionHAddLoad;
    procedure TestParseOptionHAddMany;
    procedure TestParseOptionHDelete;
    procedure TestParseOptionHDeleteLoad;
    procedure TestParseOptionHDeleteMany;
    procedure TestParseOptionHClear;
    procedure TestParseOptionHEmpty;
    procedure TestParseOptionHClearLoad;
    procedure TestParseOptionHMany;
  end;

  TestTD2XOptionsParseSkippedMethods = class(TOptionsParseTestCase)
  published
    procedure TestParseOptionSBlank;
    procedure TestParseOptionSExtn;
    procedure TestParseOptionSFile;
    procedure TestParseOptionSOff;
    procedure TestParseOptionSOn;
    procedure TestParseOptionSSimple;
  end;

  TestTD2XOptionsParseCountDefinesUsed = class(TOptionsParseTestCase)
  published
    procedure TestParseOptionCUBlank;
    procedure TestParseOptionCUExtn;
    procedure TestParseOptionCUFile;
    procedure TestParseOptionCUOff;
    procedure TestParseOptionCUOn;
  end;

  TestTD2XOptionsParseWriteDefines = class(TOptionsParseTestCase)
  published
    procedure TestParseOptionWDOff;
    procedure TestParseOptionWDOn;
    procedure TestParseOptionWDBlank;
    procedure TestParseOptionWDDir;
    procedure TestParseOptionWDExtn;
    procedure TestParseOptionWDValue;
  end;

  TestTD2XOptionsParseWriteXml = class(TOptionsParseTestCase)
  published
    procedure TestParseOptionWXOff;
    procedure TestParseOptionWXOn;
    procedure TestParseOptionWXBlank;
    procedure TestParseOptionWXDir;
    procedure TestParseOptionWXExtn;
    procedure TestParseOptionWXValue;
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
    'Processing Config ... Processing Config\Testing.Test.Dir.pas ... done Processed Config';
  RECURSE_PROCESSING =
    'Processing Config\Test ... Processing Config\Test\Testing.Test.SubDir.pas ... done Processed Config\Test';
  FULLDIR_PROCESSING = DIRECTORY_PROCESSING + ' ' + RECURSE_PROCESSING;
  END_PROCESSING = ' Processing finished!';
  ALL_PROCESSING = BOTH_PROCESSING + ' ' + FULLDIR_PROCESSING;
  ALTERED_REPORT_OPTIONS = REPORT_HEADING + 'Flags ' +
    'FinalToken+,LogErrors+,LogNotSupp+,Recurse+,Timestamp-,Verbose+ ' +
  //    'Recurse + Timestamp - Global name :Test Config dir :Test\ Log dir :Test\ ' +
  //    'Base dir :Test\ Parse mode Full Results per File Show elapsed Quiet ' +
    'Parse mode Full Results per File Show elapsed Quiet ' +
    'Write XML :Test,xml Write Defines :Test,def Count Children :.Test ' +
    'Count Final Defines :.Test Count Defines Used :.Test Skipped Methods :Test.skip ' +
    'Defines TANGO, UNIFORM, VICTOR Held Defines TANGO, UNIFORM, VICTOR';
  ZERO_REPORT_OPTIONS = REPORT_HEADING + 'Flags ' +
    'FinalToken-,LogErrors-,LogNotSupp-,Recurse-,Timestamp-,Verbose- ' +
  //    'Recurse - Timestamp - Global name Config dir - Log dir - Base dir - ' +
  //    'Parse mode Full Results per File Show elapsed None Write XML - ' +
    'Parse mode Full Results per File Show elapsed None Write XML -(,xml) ' +
    'Write Defines -(,def) Count Children -(.chld) Count Final Defines -(.final) ' +
    'Count Defines Used -(.used) Skipped Methods -(.skip) ' +
    'Defines Default Held Defines Default';
  EMPTY_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Defines NONE Held Defines Default';
{$IFDEF WIN32}
  DEFINED_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Defines ' +
    'CONDITIONALEXPRESSIONS, CPU32, CPU386, MSWINDOWS, UNICODE, VER230, WIN32 ' +
    'Held Defines Default';
{$ELSE}
  DEFINED_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Defines ' +
    'CONDITIONALEXPRESSIONS, CPU32, MSWINDOWS, UNICODE, VER230 ' + 'Held Defines Default';
{$ENDIF}
  { TestTD2XOptionEnums }

procedure TestTD2XOptionEnums.TestElapsedModeInvalidCreate;
begin
  CheckInvalidParam('Need to use correct constructor',
    'TD2XSingleParam<TD2XElapsedMode>.Create',
      procedure
    begin
      TD2XSingleParam<TD2XElapsedMode>.Create('', '', '', '', nil);
    end);
end;

procedure TestTD2XOptionEnums.TestElapsedModeParam;
var
  lPrm: TD2XSingleParam<TD2XElapsedMode>;
begin
  lPrm := TD2XSingleParam<TD2XElapsedMode>.CreateParam('T', 'Test', '<tst>',
    'Test Elapsed mode', emQuiet, TD2X.CnvDflt<TD2XElapsedMode>,
    TD2X.ToLabel<TD2XElapsedMode>);
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

procedure TestTD2XOptionEnums.TestElapsedModeParamCreates;
var
  lCalled: Boolean;
  lPrm: TD2XSingleParam<TD2XElapsedMode>;
begin
  lCalled := False;
  lPrm := TD2XSingleParam<TD2XElapsedMode>.CreateParamValid('T', 'Test', '<tst>', 'Test Elapsed mode',
    emQuiet, TD2X.CnvDflt<TD2XElapsedMode>, TD2X.ToLabel<TD2XElapsedMode>,
    function(pVal: TD2XElapsedMode): Boolean
    begin
      lCalled := True;
      Result := True;
    end);
  try
    CheckTrue(lCalled, 'Validator called');
  finally
    lPrm.Free;
  end;

  lCalled := False;
  lPrm := TD2XSingleParam<TD2XElapsedMode>.CreateParamOnSet('T', 'Test', '<tst>', 'Test Elapsed mode',
    emQuiet, TD2X.CnvDflt<TD2XElapsedMode>, TD2X.ToLabel<TD2XElapsedMode>,
    procedure(pOld, pNew, pDflt: TD2XElapsedMode)
    begin
      lCalled := True;
    end);
  try
    CheckTrue(lCalled, 'OnSet called');
  finally
    lPrm.Free;
  end;
end;

procedure TestTD2XOptionEnums.TestParseModeInvalidCreate;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XSingleParam<TD2XParseMode>.Create',
    procedure
    begin
      TD2XSingleParam<TD2XParseMode>.Create('', '', '', '', nil);
    end);
end;

procedure TestTD2XOptionEnums.TestParseModeParam;
var
  lPrm: TD2XSingleParam<TD2XParseMode>;
begin
  lPrm := TD2XSingleParam<TD2XParseMode>.CreateParam('T', 'Test', '<tst>', 'Test Parse mode',
    pmFull, TD2X.CnvDflt<TD2XParseMode>, TD2X.ToLabel<TD2XParseMode>);
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

procedure TestTD2XOptionEnums.TestParseModeParamCreates;
var
  lCalled: Boolean;
  lPrm: TD2XSingleParam<TD2XParseMode>;
begin
  lCalled := False;
  lPrm := TD2XSingleParam<TD2XParseMode>.CreateParamValid('T', 'Test', '<tst>', 'Test Elapsed mode',
    pmFull, TD2X.CnvDflt<TD2XParseMode>, TD2X.ToLabel<TD2XParseMode>,
    function(pVal: TD2XParseMode): Boolean
    begin
      lCalled := True;
      Result := True;
    end);
  try
    CheckTrue(lCalled, 'Validator called');
  finally
    lPrm.Free;
  end;

  lCalled := False;
  lPrm := TD2XSingleParam<TD2XParseMode>.CreateParamOnSet('T', 'Test', '<tst>', 'Test Elapsed mode',
    pmFull, TD2X.CnvDflt<TD2XParseMode>, TD2X.ToLabel<TD2XParseMode>,
    procedure(pOld, pNew, pDflt: TD2XParseMode)
    begin
      lCalled := True;
    end);
  try
    CheckTrue(lCalled, 'OnSet called');
  finally
    lPrm.Free;
  end;
end;

procedure TestTD2XOptionEnums.TestResultPerInvalidCreate;
begin
  CheckInvalidParam('Need to use correct constructor', 'TD2XSingleParam<TD2XResultPer>.Create',
    procedure
    begin
      TD2XSingleParam<TD2XResultPer>.Create('', '', '', '', nil);
    end);
end;

procedure TestTD2XOptionEnums.TestResultPerParam;
var
  lPrm: TD2XSingleParam<TD2XResultPer>;
begin
  lPrm := TD2XSingleParam<TD2XResultPer>.CreateParam('T', 'Test', '<tst>', 'Test Parse mode',
    rpFile, TD2X.CnvDflt<TD2XResultPer>, TD2X.ToLabel<TD2XResultPer>);
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

procedure TestTD2XOptionEnums.TestResultPerParamCreates;
var
  lCalled: Boolean;
  lPrm: TD2XSingleParam<TD2XResultPer>;
begin
  lCalled := False;
  lPrm := TD2XSingleParam<TD2XResultPer>.CreateParamValid('T', 'Test', '<tst>', 'Test Elapsed mode',
    rpDir, TD2X.CnvDflt<TD2XResultPer>, TD2X.ToLabel<TD2XResultPer>,
    function(pVal: TD2XResultPer): Boolean
    begin
      lCalled := True;
      Result := True;
    end);
  try
    CheckTrue(lCalled, 'Validator called');
  finally
    lPrm.Free;
  end;

  lCalled := False;
  lPrm := TD2XSingleParam<TD2XResultPer>.CreateParamOnSet('T', 'Test', '<tst>', 'Test Elapsed mode',
    rpDir, TD2X.CnvDflt<TD2XResultPer>, TD2X.ToLabel<TD2XResultPer>,
    procedure(pOld, pNew, pDflt: TD2XResultPer)
    begin
      lCalled := True;
    end);
  try
    CheckTrue(lCalled, 'OnSet called');
  finally
    lPrm.Free;
  end;
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
  fOpts.ProcessOption('F-T');
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
  CheckLog(UsageDescription + EXPECTED_SHOW_OPTIONS, 'Report Options');
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

procedure TestTD2XOptionsParseAll.CheckInvalid(pOpt, pExp: string);
begin
  CheckFalse(fOpts.ProcessOption(pOpt), pOpt + ' Return Value');
  CheckLog(pExp, 'Report Options');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionCC;
begin
  CheckSimple('CC', 'Count Children :.chld');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionCF;
begin
  CheckSimple('CF', 'Count Final Defines :.final');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionD;
begin
  CheckInvalid('D', 'Invalid Defines option: D');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionE;
begin
  CheckInvalid('E', 'Invalid Show elapsed option: E');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionF;
begin
  CheckInvalid('F', 'Invalid Flags option: F');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionH;
begin
  CheckInvalid('H', 'Invalid Held Defines option: H');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionM;
begin
  CheckInvalid('M', 'Invalid Parse mode option: M');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionP;
begin
  CheckInvalid('P', 'Invalid Results per option: P');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionS;
begin
  CheckSimple('S', 'Skipped Methods :.skip');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionCU;
begin
  CheckSimple('CU', 'Count Defines Used :.used');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionWD;
begin
  CheckSimple('WD', 'Write Defines :Defines,def');
end;

procedure TestTD2XOptionsParseAll.TestParseOptionWX;
begin
  CheckSimple('WX', 'Write XML :Xml,xml');
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

function TOptionsTestCase.CheckFile(pMethod, pFile: string): string;
var
  lExtn, lFile, lExpected, lOutput: string;
begin
  lExtn := '-' + pMethod + ExtractFileExt(pFile);
  lFile := 'Test\' + TidyFilename(ChangeFileExt(pFile, '')) + lExtn;
  Result := ' ' + ExtractFileName(lFile);

  ForceDirectories(ExtractFilePath(ParamStr(0)) + 'Test');
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
  lOutput := fFact.CheckOutput(pFile);
  if ReduceString(lExpected) = ReduceString(lOutput) then
  begin
    Result := '';
    if FileExists('Actual\' + ExtractFileName(lFile)) then
      System.SysUtils.DeleteFile('Actual\' + ExtractFileName(lFile));
  end
  else
  begin
    ForceDirectories(ExtractFilePath(ParamStr(0)) + 'Actual');
    with TStreamWriter.Create('Actual\' + ExtractFileName(lFile)) do
      try
        WriteLine(lOutput);
      finally
        Free;
      end;
  end;
end;

procedure TOptionsTestCase.CheckFiles(pMethod: string);
var
  lRes, lF: string;
begin
  lRes := '';
  for lF in fFact.CheckFiles do
    lRes := lRes + CheckFile(pMethod, lF);

  CheckEqualsString('', lRes, 'Errored File checks for ' + pMethod);
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

procedure TestTD2XOptions.TestDescribeAll;
begin
  fOpts.DescribeAll;
  CheckLog(UsageDescription + EXPECTED_SHOW_OPTIONS, 'Describe All');
end;

procedure TestTD2XOptions.TestEndProcessing;
begin
  fOpts.EndProcessing;
  CheckLog('Processing finished!', 'End Processing');
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

procedure TestTD2XOptions.TestHeldDefines;
begin
  CheckList('', 'Defines', fOpts.HeldDefines);
end;

procedure TestTD2XOptions.TestParserDefines;
begin
  CheckList(ReplaceStr(EXPECTED_DEFINES, ',', ' '), 'Defines', fOpts.ParserDefines);
end;

procedure TestTD2XOptions.TestProcessDirectory;
begin
  Check(fOpts.ProcessDirectory('Config', 'Testing.Test*'), 'Process Directory');
  CheckLog(DIRECTORY_PROCESSING, 'Process Directory');
  CheckErrorLog('');
end;

procedure TestTD2XOptions.TestProcessFile;
begin
  Check(fOpts.ProcessFile('Testing.Test.AUnit.pas', True), 'Process Unit');
  CheckLog(UNIT_PROCESSING, 'Process Unit');

  Check(fOpts.ProcessFile('Testing.Test.AProgram.dpr', True), 'Process Program');
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
  CheckLog(UsageDescription + EXPECTED_SHOW_OPTIONS, 'Nothing');
end;

procedure TestTD2XOptions.TestProcessStream;
begin
  fS.WriteString
    ('program Testing.Test.AProgram; uses Testing.Test.AUnit in ''Testing.Test.AUnit.pas''; begin end.');
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
  fOpts.ProcessOption('FR');
end;

procedure TestTD2XOptionsSpecific.TestCountChildren;
begin
  Check(fOpts.ProcessOption('CC'), 'Return Value 2');
  fB.Clear;

  Check(fOpts.ProcessParam('Testing.Test*', 'Count Children'), 'Process Unit');
  CheckLog('', 'Process Unit');

  fOpts.EndProcessing;
end;

procedure TestTD2XOptionsSpecific.TestCountFinalDefines;
begin
  Check(fOpts.ProcessOption('CF'), 'Return Value 2');
  fB.Clear;

  Check(fOpts.ProcessParam('Testing.Test*', 'Count Defines'), 'Process Unit');
  CheckLog('', 'Process Unit');

  fOpts.EndProcessing;
end;

procedure TestTD2XOptionsSpecific.TestCountDefinesUsed;
begin
  Check(fOpts.ProcessOption('CU'), 'Return Value 2');
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
  function PT(pA: array of string): string;
  begin
    Result := PJ([PJ(pA), PF('Total processing time')]);
  end;
  function PP(pS: string; pPs: array of integer): string;
  var
    lI: integer;
  begin
    Result := pS + ' ';
    for lI in pPs do
      Result := Result + Format('%3d%%'#8#8#8#8, [lI]);
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
  CheckLog(ALL_PROCESSING + END_PROCESSING, 'Elapsed Quiet');

  Check(fOpts.ProcessOption('ER'), 'Return Value 1');
  fB.Clear;
  Check(fOpts.ProcessParam('Testing.Test*', 'Count Children'), 'Process Units');
  fOpts.EndProcessing;
  CheckLog(ALL_PROCESSING + END_PROCESSING, 'Elapsed Run');

  Check(fOpts.ProcessOption('ED'), 'Return Value 1');
  fB.Clear;
  Check(fOpts.ProcessParam('Testing.Test*', 'Count Children'), 'Process Units');
  fOpts.EndProcessing;
  CheckLog(PT([PF(PB('')), PF(PB('Config')), PF(PB('Config\Test'))]), 'Elapsed Dir');

  Check(fOpts.ProcessOption('EF'), 'Return Value 1');
  fB.Clear;
  Check(fOpts.ProcessParam('Testing.Test*', 'Count Children'), 'Process Units');
  fOpts.EndProcessing;
  CheckLog(PT([PD('', ['Testing.Test.AUnit.pas', 'Testing.Test.AProgram.dpr',
          'Testing.Test.APackage.dpk']), PD('Config', ['Testing.Test.Dir.pas']),
      PD('Config\Test', ['Testing.Test.SubDir.pas'])]), 'Elapsed File');

  Check(fOpts.ProcessOption('EP'), 'Return Value 1');
  fB.Clear;
  Check(fOpts.ProcessParam('Testing.Test*', 'Count Children'), 'Process Units');
  fOpts.EndProcessing;
  SetLength(lP, 3);
  SA(lP[0], [1, 3, 4, 6, 9, 10, 12, 14, 15, 30, 31, 33, 34, 36, 46, 53, 64, 75, 98, 99, 100]);
  SA(lP[1], [7, 15, 17, 21, 22, 30, 31, 37, 45, 46, 51, 52, 57, 60, 87, 88, 94, 98, 100]);
  SA(lP[2], [7, 16, 17, 21, 22, 31, 32, 42, 51, 52, 56, 57, 63, 66, 93, 94, 98, 100]);
  SetLength(lC, 1);
  SA(lC[0], [7, 23, 25, 32, 34, 40, 42, 61, 90, 98, 100]);
  SetLength(lT, 1);
  SA(lT[0], [7, 21, 23, 30, 32, 43, 45, 63, 90, 98, 100]);
  CheckLog(PT([PDP('', ['Testing.Test.AUnit.pas', 'Testing.Test.AProgram.dpr',
          'Testing.Test.APackage.dpk'], lP), PDP('Config', ['Testing.Test.Dir.pas'], lC),
      PDP('Config\Test', ['Testing.Test.SubDir.pas'], lT)]), 'Elapsed Processing');
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
  Check(fOpts.ProcessOption('FE'), 'Return Value 3');
  fB.Clear;

  fS.WriteString('unit Test; interface procedure A; imp');
  fS.Position := 0;
  CheckTrue(fOpts.ProcessStream('(Stream)', fDS.ReadFrom), 'Error Stream');
  CheckLog('', 'Error Stream');
  CheckErrorLog('''Implementation'' expected found ''imp''');

  fS.WriteString('lementation procedure A; begin end; end.');
  fS.Position := 0;
  Check(fOpts.ProcessStream('(Stream)', fDS.ReadFrom), 'Process Stream');
  CheckLog('', 'Process Stream');

  fOpts.EndProcessing;
end;

procedure TestTD2XOptionsSpecific.TestNotSupported;
begin
  Check(fOpts.ProcessOption('FN'), 'Return Value 3');
  fB.Clear;

  Check(fOpts.ProcessFile('Testing.Test.AUnit.pas', True), 'Process Unit');

  CheckLog('', 'Process Unit');
  CheckErrorLog('Currently not supported {$D+}');

  fOpts.EndProcessing;
end;

procedure TestTD2XOptionsSpecific.TestWriteDefines;
begin
  Check(fOpts.ProcessOption('WD'), 'Return Value 2');
  fB.Clear;

  Check(fOpts.ProcessParam('Testing.Test*', 'Write Defines'), 'Process Unit');
  CheckLog('', 'Process Unit');

  fOpts.EndProcessing;
end;

{ TOptionsParseTestCase }

procedure TOptionsParseTestCase.CheckSimple(pOpt, pExp: string);
begin
  Check(fOpts.ProcessOption(pOpt), pOpt + ' Return Value');
  CheckLog('', 'Report Options');
  fOpts.ProcessOption('@-' + pOpt);
  CheckLog(pExp, 'Report Options');
end;

{ TestTD2XOptionsParseXml }

procedure TestTD2XOptionsParseWriteXml.TestParseOptionWXBlank;
begin
  CheckSimple('WX:', 'Write XML :,xml');
end;

procedure TestTD2XOptionsParseWriteXml.TestParseOptionWXDir;
begin
  CheckSimple('WX:Value', 'Write XML :Value,xml');
end;

procedure TestTD2XOptionsParseWriteXml.TestParseOptionWXExtn;
begin
  CheckSimple('WX:,val', 'Write XML :,val');
end;

procedure TestTD2XOptionsParseWriteXml.TestParseOptionWXOff;
begin
  CheckSimple('WX-', 'Write XML -(Xml,xml)');
end;

procedure TestTD2XOptionsParseWriteXml.TestParseOptionWXOn;
begin
  CheckSimple('WX+', 'Write XML :Xml,xml');
end;

procedure TestTD2XOptionsParseWriteXml.TestParseOptionWXValue;
begin
  CheckSimple('WX:Value,val', 'Write XML :Value,val');
end;

{ TestTD2XOptionsParseWriteDefines }

procedure TestTD2XOptionsParseWriteDefines.TestParseOptionWDBlank;
begin
  CheckSimple('WD:', 'Write Defines :,def');
end;

procedure TestTD2XOptionsParseWriteDefines.TestParseOptionWDDir;
begin
  CheckSimple('WD:Write', 'Write Defines :Write,def');
end;

procedure TestTD2XOptionsParseWriteDefines.TestParseOptionWDExtn;
begin
  CheckSimple('WD:,wrt', 'Write Defines :,wrt');
end;

procedure TestTD2XOptionsParseWriteDefines.TestParseOptionWDOff;
begin
  CheckSimple('WD-', 'Write Defines -(Defines,def)');
end;

procedure TestTD2XOptionsParseWriteDefines.TestParseOptionWDOn;
begin
  CheckSimple('WD+', 'Write Defines :Defines,def');
end;

procedure TestTD2XOptionsParseWriteDefines.TestParseOptionWDValue;
begin
  CheckSimple('WD:Write,wrt', 'Write Defines :Write,wrt');
end;

{ TestTD2XOptionsParseDefinesUsed }

procedure TestTD2XOptionsParseCountDefinesUsed.TestParseOptionCUBlank;
begin
  CheckSimple('CU:', 'Count Defines Used :.used');
end;

procedure TestTD2XOptionsParseCountDefinesUsed.TestParseOptionCUExtn;
begin
  CheckSimple('CU:Extn', 'Count Defines Used :.Extn');
end;

procedure TestTD2XOptionsParseCountDefinesUsed.TestParseOptionCUFile;
begin
  CheckSimple('CU:File.Extn', 'Count Defines Used :File.Extn');
end;

procedure TestTD2XOptionsParseCountDefinesUsed.TestParseOptionCUOff;
begin
  CheckSimple('CU-', 'Count Defines Used -(.used)');
end;

procedure TestTD2XOptionsParseCountDefinesUsed.TestParseOptionCUOn;
begin
  CheckSimple('CU+', 'Count Defines Used :.used');
end;

{ TestTD2XOptionsParseSkippedMethods }

procedure TestTD2XOptionsParseSkippedMethods.TestParseOptionSBlank;
begin
  CheckSimple('S:', 'Skipped Methods :.skip');
end;

procedure TestTD2XOptionsParseSkippedMethods.TestParseOptionSExtn;
begin
  CheckSimple('S:.Extn', 'Skipped Methods :.Extn');
end;

procedure TestTD2XOptionsParseSkippedMethods.TestParseOptionSFile;
begin
  CheckSimple('S:File.Extn', 'Skipped Methods :File.Extn');
end;

procedure TestTD2XOptionsParseSkippedMethods.TestParseOptionSOff;
begin
  CheckSimple('S-', 'Skipped Methods -(.skip)');
end;

procedure TestTD2XOptionsParseSkippedMethods.TestParseOptionSOn;
begin
  CheckSimple('S+', 'Skipped Methods :.skip');
end;

procedure TestTD2XOptionsParseSkippedMethods.TestParseOptionSSimple;
begin
  CheckSimple('S:Extn', 'Skipped Methods :Extn.skip');
end;

{ TestTD2XOptionsParseDefines }

procedure TestTD2XOptionsParseDefines.SetUp;
begin
  inherited;

  fOpts.ParserDefines.CommaText := 'ALPHA,BETA,GAMMA,UNIFORM';

  fCode := 'D';
  fLabel := 'Defines ';
end;

procedure TestTD2XOptionsParseDefines.TestParseOptionDAdd;
begin
  CheckDefines('+Value', 'ALPHA, BETA, GAMMA, UNIFORM, VALUE');
end;

procedure TestTD2XOptionsParseDefines.TestParseOptionDAddLoad;
begin
  CheckDefines('+~Test.def', 'ALPHA, BETA, GAMMA, TANGO, UNIFORM, VICTOR');
end;

procedure TestTD2XOptionsParseDefines.TestParseOptionDAddMany;
begin
  CheckDefines('+Test1,Test2', 'ALPHA, BETA, GAMMA, TEST1, TEST2, UNIFORM');
end;

procedure TestTD2XOptionsParseDefines.TestParseOptionDClear;
begin
  CheckDefines('!', 'Default');
end;

procedure TestTD2XOptionsParseDefines.TestParseOptionDClearLoad;
begin
  CheckDefines(':Test', 'TANGO, UNIFORM, VICTOR');
end;

procedure TestTD2XOptionsParseDefines.TestParseOptionDDelete;
begin
  CheckDefines('-Beta', 'ALPHA, GAMMA, UNIFORM');
end;

procedure TestTD2XOptionsParseDefines.TestParseOptionDDeleteLoad;
begin
  CheckDefines('-~Test.def', 'ALPHA, BETA, GAMMA');
end;

procedure TestTD2XOptionsParseDefines.TestParseOptionDDeleteMany;
begin
  CheckDefines('-Beta,Gamma', 'ALPHA, UNIFORM');
end;

procedure TestTD2XOptionsParseDefines.TestParseOptionDEmpty;
begin
  CheckDefines(':', 'NONE');
end;

procedure TestTD2XOptionsParseDefines.TestParseOptionDMany;
begin
  Check(fOpts.ProcessOption('D+Value1'), 'ReturnValue1');
  Check(fOpts.ProcessOption('D+Value2,Value3'), 'ReturnValue2');
  fOpts.ProcessOption('@-D');
  CheckLog('Defines ALPHA, BETA, GAMMA, UNIFORM, VALUE1, VALUE2, VALUE3', 'Report Options');

  Check(fOpts.ProcessOption('D-Value2'), 'ReturnValue1');
  Check(fOpts.ProcessOption('D-Uniform,Value1'), 'ReturnValue2');
  fOpts.ProcessOption('@-D');
  CheckLog('Defines ALPHA, BETA, GAMMA, VALUE3', 'Report Options');
end;

{ TestTD2XOptionsParseCountDefines }

procedure TestTD2XOptionsParseCountFinalDefines.TestParseOptionCFBlank;
begin
  CheckSimple('CF:', 'Count Final Defines :.final');
end;

procedure TestTD2XOptionsParseCountFinalDefines.TestParseOptionCFExtn;
begin
  CheckSimple('CF:Extn', 'Count Final Defines :.Extn');
end;

procedure TestTD2XOptionsParseCountFinalDefines.TestParseOptionCFFile;
begin
  CheckSimple('CF:File.Extn', 'Count Final Defines :File.Extn');
end;

procedure TestTD2XOptionsParseCountFinalDefines.TestParseOptionCFOff;
begin
  CheckSimple('CF-', 'Count Final Defines -(.final)');
end;

procedure TestTD2XOptionsParseCountFinalDefines.TestParseOptionCFOn;
begin
  CheckSimple('CF+', 'Count Final Defines :.final');
end;

{ TestTD2XOptionsParseCountChildren }

procedure TestTD2XOptionsParseCountChildren.TestParseOptionCCBlank;
begin
  CheckSimple('CC:', 'Count Children :.chld');
end;

procedure TestTD2XOptionsParseCountChildren.TestParseOptionCCExtn;
begin
  CheckSimple('CC:Extn', 'Count Children :.Extn');
end;

procedure TestTD2XOptionsParseCountChildren.TestParseOptionCCFile;
begin
  CheckSimple('CC:File.Extn', 'Count Children :File.Extn');
end;

procedure TestTD2XOptionsParseCountChildren.TestParseOptionCCOff;
begin
  CheckSimple('CC-', 'Count Children -(.chld)');
end;

procedure TestTD2XOptionsParseCountChildren.TestParseOptionCCOn;
begin
  CheckSimple('CC+', 'Count Children :.chld');
end;

{ TestTD2XOptionsParseUnused }

procedure TestTD2XOptionsParseUnused.CheckUnknown(pOpt: string);
begin
  CheckFalse(fOpts.ProcessOption(pOpt), pOpt + ' Return Value');
  CheckLog('Unknown option: ' + pOpt, 'Report Options');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionA;
begin
  CheckUnknown('A');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionC;
begin
  CheckUnknown('C');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionJ;
begin
  CheckUnknown('J');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionK;
begin
  CheckUnknown('K');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionL;
begin
  CheckUnknown('L');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionN;
begin
  CheckUnknown('N');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionQ;
begin
  CheckUnknown('Q');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionR;
begin
  CheckUnknown('R');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionU;
begin
  CheckUnknown('U');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionV;
begin
  CheckUnknown('V');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionW;
begin
  CheckUnknown('W');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionX;
begin
  CheckUnknown('X');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionY;
begin
  CheckUnknown('Y');
end;

procedure TestTD2XOptionsParseUnused.TestParseOptionZ;
begin
  CheckUnknown('Z');
end;

{ TestTD2XOptionsParseEnumerated }

procedure TestTD2XOptionsParseEnumerated.TestConvertEnumerated;
var
  lPM: TD2XSingleParam<TD2XParseMode>;
  lRP: TD2XSingleParam<TD2XResultPer>;
  lEM: TD2XSingleParam<TD2XElapsedMode>;
begin
  lPM := TD2XSingleParam<TD2XParseMode>.CreateParam('M', 'Parse mode', '<mode>',
    'Parser type (F[ull], U[ses])', pmFull, TD2X.CnvEnum<TD2XParseMode>,
    TD2X.ToLabel<TD2XParseMode>);
  try
    lPM.Convert('Uses');
    Check(pmUses = lPM.Value, 'Parse mode convert');
  finally
    FreeAndNil(lPM);
  end;

  lRP := TD2XSingleParam<TD2XResultPer>.CreateParam('P', 'Results per', '<per>',
    'Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un])', rpFile,
    TD2X.CnvEnum<TD2XResultPer>, TD2X.ToLabel<TD2XResultPer>);
  try
    lRP.Convert('Dir');
    Check(rpDir = lRP.Value, 'Results per convert');
  finally
    FreeAndNil(lRP);
  end;
  lEM := TD2XSingleParam<TD2XElapsedMode>.CreateParam('E', 'Show elapsed', '<mode>',
    'Elapsed time display (N[one], T[otal], D[ir], F[ile], P[rocessing], [Q]uiet)', emQuiet,
    TD2X.CnvEnum<TD2XElapsedMode>, TD2X.ToLabel<TD2XElapsedMode>);
  try
    lEM.Convert('Dir');
    Check(emDir = lEM.Value, 'Elapsed mode convert');
  finally
    FreeAndNil(lEM);
  end;
end;

procedure TestTD2XOptionsParseEnumerated.TestParseOptionEValue;
begin
  CheckSimple('ETotal', 'Show elapsed Total');
end;

procedure TestTD2XOptionsParseEnumerated.TestParseOptionMValue;
begin
  CheckSimple('MUses', 'Parse mode Uses');
  CheckEqualsString('Uses', fFact.GlobalName, 'GlobalName');
end;

procedure TestTD2XOptionsParseEnumerated.TestParseOptionPValue;
begin
  CheckSimple('PDir', 'Results per Dir');
end;

{ TestTD2XOptionsParseFlags }

procedure TestTD2XOptionsParseFlags.CheckFlag(pFlag, pDesc: string);
begin
  Check(fOpts.ProcessOption('F-' + pFlag), 'F- Return Value');
  CheckLog('', 'Report Options');
  fOpts.ProcessOption('@-F' + pFlag);
  CheckLog('Flags ' + pDesc + '-', 'Report Options');

  Check(fOpts.ProcessOption('F+' + pFlag), 'F+ Return Value');
  CheckLog('', 'Report Options');
  fOpts.ProcessOption('@-F' + pFlag);
  CheckLog('Flags ' + pDesc + '+', 'Report Options');
end;

procedure TestTD2XOptionsParseFlags.TestParseOptionFinal;
begin
  CheckFlag('F', 'FinalToken');
end;

procedure TestTD2XOptionsParseFlags.TestParseOptionLogErrors;
begin
  CheckFlag('E', 'LogErrors');
end;

procedure TestTD2XOptionsParseFlags.TestParseOptionLogNotSupported;
begin
  CheckFlag('N', 'LogNotSupp');
end;

procedure TestTD2XOptionsParseFlags.TestParseOptionRecurse;
begin
  CheckFlag('R', 'Recurse');
end;

procedure TestTD2XOptionsParseFlags.TestParseOptionTimestamp;
begin
  CheckFlag('T', 'Timestamp');
end;

procedure TestTD2XOptionsParseFlags.TestParseOptionVerbose;
begin
  CheckFlag('V', 'Verbose');
end;

{ TestTD2XOptionsHeldDefines }

procedure TestTD2XOptionsParseHeldDefines.SetUp;
begin
  inherited;

  fOpts.HeldDefines.CommaText := 'ALPHA,BETA,GAMMA,UNIFORM';

  fCode := 'H';
  fLabel := 'Held Defines ';
end;

procedure TestTD2XOptionsParseHeldDefines.TestParseOptionHAdd;
begin
  CheckDefines('+Value', 'ALPHA, BETA, GAMMA, UNIFORM, VALUE');
end;

procedure TestTD2XOptionsParseHeldDefines.TestParseOptionHAddLoad;
begin
  CheckDefines('+~Test.def', 'ALPHA, BETA, GAMMA, TANGO, UNIFORM, VICTOR');
end;

procedure TestTD2XOptionsParseHeldDefines.TestParseOptionHAddMany;
begin
  CheckDefines('+Test1,Test2', 'ALPHA, BETA, GAMMA, TEST1, TEST2, UNIFORM');
end;

procedure TestTD2XOptionsParseHeldDefines.TestParseOptionHClear;
begin
  CheckDefines('!', 'Default');
end;

procedure TestTD2XOptionsParseHeldDefines.TestParseOptionHClearLoad;
begin
  CheckDefines(':Test', 'TANGO, UNIFORM, VICTOR');
end;

procedure TestTD2XOptionsParseHeldDefines.TestParseOptionHDelete;
begin
  CheckDefines('-Beta', 'ALPHA, GAMMA, UNIFORM');
end;

procedure TestTD2XOptionsParseHeldDefines.TestParseOptionHDeleteLoad;
begin
  CheckDefines('-~Test.def', 'ALPHA, BETA, GAMMA');
end;

procedure TestTD2XOptionsParseHeldDefines.TestParseOptionHDeleteMany;
begin
  CheckDefines('-Beta,Gamma', 'ALPHA, UNIFORM');
end;

procedure TestTD2XOptionsParseHeldDefines.TestParseOptionHEmpty;
begin
  CheckDefines(':', 'NONE');
end;

procedure TestTD2XOptionsParseHeldDefines.TestParseOptionHMany;
begin
  Check(fOpts.ProcessOption('H+Value1'), 'ReturnValue1');
  Check(fOpts.ProcessOption('H+Value2,Value3'), 'ReturnValue2');
  fOpts.ProcessOption('@-H');
  CheckLog('Held Defines ALPHA, BETA, GAMMA, UNIFORM, VALUE1, VALUE2, VALUE3',
    'Report Options');

  Check(fOpts.ProcessOption('H-Value2'), 'ReturnValue1');
  Check(fOpts.ProcessOption('H-Uniform,Value1'), 'ReturnValue2');
  fOpts.ProcessOption('@-H');
  CheckLog('Held Defines ALPHA, BETA, GAMMA, VALUE3', 'Report Options');
end;

{ TestTD2XOptionsXmlPer }

procedure TestTD2XOptionsXmlPer.SetUp;
begin
  inherited;

  fOpts.ProcessOption('!!');
  fOpts.ProcessOption('FRF');
  fOpts.ProcessOption('E!');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlPerDir;
begin
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PD');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Dir');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Dir');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerDir');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlPerFile;
begin
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PF');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per File');
  CheckLog(ALL_PROCESSING, 'Processing Xml per File');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerFile');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlPerParam;
begin
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PP');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per FilParam');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Param');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerParam');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlPerRun;
begin
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PR');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Run');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Run');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerRun');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlPerSubDir;
begin
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PS');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per SubDir');
  CheckLog(ALL_PROCESSING, 'Processing Xml per SubDir');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerSubDir');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlPerWildcard;
begin
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PW');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Wildcard');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Wildcard');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlPerWildcard');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlUsesPerDir;
begin
  fOpts.ProcessOption('MU');
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PD');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Dir');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Dir');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerDir');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlUsesPerFile;
begin
  fOpts.ProcessOption('MU');
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PF');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per File');
  CheckLog(ALL_PROCESSING, 'Processing Xml per File');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerFile');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlUsesPerParam;
begin
  fOpts.ProcessOption('MU');
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PP');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per FilParam');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Param');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerParam');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlUsesPerRun;
begin
  fOpts.ProcessOption('MU');
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PR');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Run');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Run');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerRun');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlUsesPerSubDir;
begin
  fOpts.ProcessOption('MU');
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PS');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per SubDir');
  CheckLog(ALL_PROCESSING, 'Processing Xml per SubDir');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerSubDir');
end;

procedure TestTD2XOptionsXmlPer.TestProcessXmlUsesPerWildcard;
begin
  fOpts.ProcessOption('MU');
  fOpts.ProcessOption('WX:');
  fOpts.ProcessOption('PW');
  Check(fOpts.ProcessParam('Testing.Test*', 'Process Xml'), 'Param per Wildcard');
  CheckLog(ALL_PROCESSING, 'Processing Xml per Wildcard');
  fOpts.EndProcessing;
  CheckFiles('TestProcessXmlUsesPerWildcard');
end;

{ TOptionsParseDefinesTestCase }

procedure TOptionsParseDefinesTestCase.CheckDefines(pParam, pResult: string);
begin
  CheckSimple(fCode + pParam, fLabel + pResult);
end;

initialization

RegisterTests('Options', [TestTD2XOptionEnums.Suite, TestTD2XOptions.Suite,
  TestTD2XOptionsParseAll.Suite, TestTD2XOptionsParseUnused.Suite,
  TestTD2XOptionsParseEnumerated.Suite, TestTD2XOptionsParseFlags.Suite,
  TestTD2XOptionsParseCountChildren.Suite, TestTD2XOptionsParseCountFinalDefines.Suite,
  TestTD2XOptionsParseDefines.Suite, TestTD2XOptionsParseHeldDefines.Suite,
  TestTD2XOptionsParseSkippedMethods.Suite, TestTD2XOptionsParseCountDefinesUsed.Suite,
  TestTD2XOptionsParseWriteDefines.Suite, TestTD2XOptionsParseWriteXml.Suite,
  TestTD2XOptionsGeneral.Suite, TestTD2XOptionsSpecific.Suite, TestTD2XOptionsXmlPer.Suite]);

end.
