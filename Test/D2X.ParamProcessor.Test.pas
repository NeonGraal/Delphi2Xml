unit D2X.ParamProcessor.Test;

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
  D2X,
  D2X.Xml,
  D2X.Test,
  D2X.Handler,
  D2X.Handler.Test,
  D2X.Options,
  D2X.Parser,
  D2X.ParamProcessor,
  D2X.Utils,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.SysUtils,
  System.Rtti;

type
  TestTD2XParamProcessorBase = class(TStringTestCase)
  protected
    fPP: TD2XParamProcessor;

    procedure CheckLog(pMsg: string);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTD2XParamProcessor = class(TestTD2XParamProcessorBase)
  published
    procedure TestEndProcessing;

    procedure TestProcessParam;
    procedure TestProcessParamPasFiles;
    procedure TestProcessParamParamFile;

    procedure TestProcessCountChildren;
    procedure TestProcessVerbose;
  end;

  TestTD2XParamProcessorGeneral = class(TestTD2XParamProcessorBase)
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

  { TestTD2XProcessor }

procedure TestTD2XParamProcessor.TestEndProcessing;
begin
  fPP.EndProcessing;
  CheckBuilder('', 'Nothing');
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

procedure TestTD2XParamProcessor.TestProcessParam;
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

procedure TestTD2XParamProcessor.TestProcessParamParamFile;
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
    'Count Children :.cnt Skipped Methods :.skip ' +
    'Use default Defines';
begin
  pStr := '@Test.prm';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := fPP.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckBuilder(EXPECTED_REPORT, 'Nothing');
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

  ReturnValue := fPP.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckBuilder(EXPECTED_PROCESSING, 'Nothing');
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

  { TestTD2XParamProcessorGeneral }

procedure TestTD2XParamProcessorGeneral.SetAllOptions;
var
  C: Char;
begin
  for C := 'A' to 'Z' do
    if not fPP.ProcessParam('-' + C + ':Test', '', 0) then
      fPP.ProcessParam('-' + C + '+', '', 0);
  fPP.ProcessParam('-T-', '', 0);
  fSB.Clear;
end;

procedure TestTD2XParamProcessorGeneral.TestDefaultOptions;
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

procedure TestTD2XParamProcessorGeneral.TestReportOptions;
var
  ReturnValue: boolean;
begin
  SetAllOptions;

  ReturnValue := fPP.ProcessParam('-@', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog(ALTERED_REPORT_OPTIONS);
end;

procedure TestTD2XParamProcessorGeneral.TestReportOptionsDefault;
var
  ReturnValue: boolean;
begin
  ReturnValue := fPP.ProcessParam('-@', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog(EMPTY_REPORT_OPTIONS);
end;

procedure TestTD2XParamProcessorGeneral.TestReportOptionsDefines;
var
  ReturnValue: boolean;
begin
  ReturnValue := fPP.ProcessParam('-D+CPU32', '', 0);
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fPP.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(DEFINED_REPORT_OPTIONS);
end;

procedure TestTD2XParamProcessorGeneral.TestReportOptionsEmpty;
var
  ReturnValue: boolean;
begin
  ReturnValue := fPP.ProcessParam('-D:', '', 0);
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fPP.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(EMPTY_REPORT_OPTIONS);
end;

procedure TestTD2XParamProcessorGeneral.TestReportOptionsExtn;
var
  ReturnValue: boolean;
begin
  SetAllOptions;

  ReturnValue := fPP.ProcessParam('-@Test.tst', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XParamProcessorGeneral.TestReportOptionsFile;
var
  ReturnValue: boolean;
begin
  SetAllOptions;

  ReturnValue := fPP.ProcessParam('-@Test', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XParamProcessorGeneral.TestReportOptionsFileDefault;
var
  ReturnValue: boolean;
begin
  ReturnValue := fPP.ProcessParam('-@Test', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XParamProcessorGeneral.TestReportOptionsReset;
var
  ReturnValue: boolean;
begin
  ReturnValue := fPP.ProcessParam('-D!', '', 0);
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fPP.ProcessParam('-@', '', 0);
  Check(ReturnValue, 'ReturnValue');
  CheckLog(DEFAULT_REPORT_OPTIONS);
end;

procedure TestTD2XParamProcessorGeneral.TestResetOptions;
var
  ReturnValue: boolean;

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

procedure TestTD2XParamProcessorGeneral.TestShowOptions;
var
  ReturnValue: boolean;
begin
  ReturnValue := fPP.ProcessParam('-?', '', 0);

  Check(ReturnValue, 'ReturnValue');
  CheckLog(EXPECTED_SHOW_OPTIONS);
end;

procedure TestTD2XParamProcessorGeneral.TestZeroOptions;
var
  ReturnValue: boolean;

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

{ TestTD2XParamProcessorBase }

procedure TestTD2XParamProcessorBase.CheckLog(pMsg: string);
begin
  CheckEqualsString(pMsg, ReduceString(fSB.ToString), 'Log');
end;

procedure TestTD2XParamProcessorBase.SetUp;
begin
  inherited;

  fPP := TD2XParamProcessor.Create(fSB);
end;

procedure TestTD2XParamProcessorBase.TearDown;
begin
  fPP := nil;

  inherited;
end;

initialization

// Register any test cases with the test runner
RegisterTests('ParamProcessor', [TestTD2XParamProcessor.Suite]);

end.
