unit Test.IO.Options.Tests;

interface

implementation

uses
  D2X.Global,
  D2X.IO,
  D2X.IO.Options,
  D2X.Param,
  System.Classes,
  System.Diagnostics,
  System.SysUtils,
  Test.Constants,
  Test.IO.Options,

  TestFramework,
  Winapi.Windows;

type
  TestTD2XFileFactory = class(TFileFactoryTestCase)
  private
    procedure CheckIO(var pDS: ID2XIO; pExp, pLabel: string);
    procedure CheckValidator(pLabel: string);
  published
    procedure TestGobalName;
    procedure TestTimestampFiles;
    procedure TestConfigFileOrExtn;
    procedure TestLogFileOrExtn;
    procedure TestBaseFile;
    procedure TestBaseDir;
    procedure TestSimpleFile;
    procedure TestGetNow;
    procedure TestGetDuration;
    procedure TestGetInputStream;

    procedure TestFull;
    procedure TestFullBase;
    procedure TestFullExcluding;
    procedure TestFullBaseExcluding;
  end;

  TestTD2XFileOptions = class(TFileFactoryTestCase)
  published
    procedure TestParseB;
    procedure TestParseG;
    procedure TestParseI;
    procedure TestParseO;
    procedure TestParseX;

    procedure TestForCode;
    procedure TestDescribeAll;
    procedure TestOutputAll;
    procedure TestReportAll;
    procedure TestResetAll;
    procedure TestZeroAll;

  end;

  TestTD2XOptionGeneral = class(TTestCase)
  published
    procedure TestSplitDirExtn;

    procedure TestConvertDir;
    procedure TestConvertDirExtn;
    procedure TestConvertExtn;
    procedure TestConvertFile;
  end;

const
  FILE_CHANGED_OPTIONS = 'Global name :Test Config dir ::Test Log dir ::Test ' +
    'Base dir -(:Test) Exclude Files/Dirs';
  { TestTD2XFileOptions }

procedure TestTD2XFileFactory.CheckIO(var pDS: ID2XIO; pExp, pLabel: string);
begin
  try
    CheckEqualsString(pExp, pDS.Description, pLabel);
  finally
    DisposeOf(pDS);
  end;
end;

procedure TestTD2XFileFactory.CheckValidator(pLabel: string);
begin
  CheckTrue(fValidatorCalled, pLabel);
  fValidatorCalled := False;
end;

procedure TestTD2XFileFactory.TestBaseDir;
var
  lBase: TD2XParam;
  lDS: ID2XIO;
begin
  lBase := ForCode('B');

  lDS := fFileOpts.BaseDir('Config');
  CheckIO(lDS, 'Config\', 'Default Dir');

  lBase.Parse('B+');

  lDS := fFileOpts.BaseDir('Config');
  CheckIO(lDS, '.\Config\', 'Base On Dir');

  lBase.Parse('B-');

  lDS := fFileOpts.BaseDir('Config');
  CheckIO(lDS, 'Config\', 'Base Off Dir');

  lBase.Parse('B:Base');

  lDS := fFileOpts.BaseDir('Config');
  CheckIO(lDS, 'Base\Config\', 'Base Dir');
end;

procedure TestTD2XFileFactory.TestBaseFile;
var
  lBase: TD2XParam;
  lDS: ID2XIO;
begin
  lBase := ForCode('B');

  lDS := fFileOpts.BaseFile('File.Extn');
  CheckIO(lDS, 'File.Extn', 'Default File');

  lBase.Parse('B+');

  lDS := fFileOpts.BaseFile('File.Extn');
  CheckIO(lDS, '.\File.Extn', 'Base On File');

  lBase.Parse('B-');

  lDS := fFileOpts.BaseFile('File.Extn');
  CheckIO(lDS, 'File.Extn', 'Base Off File');

  lBase.Parse('B:Base');

  lDS := fFileOpts.BaseFile('File.Extn');
  CheckIO(lDS, 'Base\File.Extn', 'Base File');
end;

procedure TestTD2XFileFactory.TestConfigFileOrExtn;
var
  lGlobal, lInput: TD2XParam;
  lDS: ID2XIO;
  pExtn: string;
begin
  lGlobal := ForCode('G');
  lInput := ForCode('I');
  pExtn := '.Extn';

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');
  CheckIO(lDS, 'Config\File.Extn', 'Config File');

  lGlobal.Parse('G');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'Config\' + fFileOpts.GlobalName + '.Extn', 'Config Default .Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'Config\Global.Extn', 'Config Global .Extn');

  lInput.Parse('I-');

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');
  CheckIO(lDS, 'File.Extn', 'Input Off File');

  lGlobal.Parse('G');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, fFileOpts.GlobalName + '.Extn', 'Input Off Default .Extn ');

  ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'Global.Extn', 'Input Off Global .Extn');

  lInput.Parse('I+');

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');
  CheckIO(lDS, 'Config\File.Extn', 'Input On File');

  lGlobal.Parse('G');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'Config\' + fFileOpts.GlobalName + '.Extn', 'Input On Default .Extn ');

  ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'Config\Global.Extn', 'Input On Global .Extn');

  lInput.Parse('I:In');

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');
  CheckIO(lDS, 'In\File.Extn', 'Input File');

  lGlobal.Parse('G');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'In\' + fFileOpts.GlobalName + '.Extn', 'Input Default .Extn ');

  ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'In\Global.Extn', 'Input Global .Extn');
end;

procedure TestTD2XFileFactory.TestFull;
var
  lD, lSD: ID2XIODir;
begin
  lSD := nil;
  lD := fFileOpts.BaseDir('Test');
  try
    CheckEqualsString('Test\', lD.Description, 'Dir Description');

    CheckTrue(lD.FirstFile('File*.txt'), 'First File');
    try
      CheckEqualsString('Test\FileExclude.txt', lD.Current, 'First File Name');
      CheckTrue(lD.Next, 'Next File');

      CheckEqualsString('Test\FileTest.txt', lD.Current, 'Next File Name');
      CheckFalse(lD.Next, 'Last File');
    finally
      lD.Close;
    end;

    CheckTrue(lD.FirstDir, 'First Dir');
    try
      CheckEqualsString('Test\DirExclude', lD.Current, 'Exclude Dir Name');
      CheckTrue(lD.Next, 'Next Dir');

      CheckEqualsString('Test\DirTest', lD.Current, 'First Dir Name');
      lSD := fFileOpts.BaseDir(lD.Current);
      CheckEqualsString('Test\DirTest\', lSD.Description);

      CheckFalse(lD.Next, 'Last Dir');
    finally
      lD.Close;
    end;
  finally
    DisposeOf(lSD);
    DisposeOf(lD);
  end;
end;

procedure TestTD2XFileFactory.TestFullBase;
var
  lD, lSD: ID2XIODir;
begin
  ForCode('B').Parse('B:Test');

  lSD := nil;
  lD := fFileOpts.BaseDir('DirTest');
  try
    CheckEqualsString('Test\DirTest\', lD.Description, 'Dir Description');

    CheckTrue(lD.FirstFile('*.txt'), 'First File');
    try
      CheckEqualsString('DirTest\DirFileExclude.txt', lD.Current, 'First File Name');
      CheckTrue(lD.Next, 'Next File');

      CheckEqualsString('DirTest\DirFileTest.txt', lD.Current, 'Next File Name');
      CheckFalse(lD.Next, 'Last File');
    finally
      lD.Close;
    end;

    CheckTrue(lD.FirstDir, 'First Dir');
    try
      CheckEqualsString('DirTest\SubDirExclude', lD.Current, 'Exclude Dir Name');
      CheckTrue(lD.Next, 'Next Dir');

      CheckEqualsString('DirTest\SubDirTest', lD.Current, 'First Dir Name');
      lSD := fFileOpts.BaseDir(lD.Current);
      CheckEqualsString('Test\DirTest\SubDirTest\', lSD.Description, 'First Dir Description');

      CheckFalse(lD.Next, 'Last Dir');
    finally
      lD.Close;
    end;
  finally
    DisposeOf(lSD);
    DisposeOf(lD);
  end;
end;

procedure TestTD2XFileFactory.TestFullBaseExcluding;
var
  lD, lSD: ID2XIODir;
begin
  ForCode('X').Parse('XEx\w+de');
  ForCode('B').Parse('B:Test');

  lSD := nil;
  lD := fFileOpts.BaseDir('DirTest');
  try
    CheckEqualsString('Test\DirTest\', lD.Description, 'Dir Description');

    CheckTrue(lD.FirstFile('DirFile*.txt'), 'First File');
    try
      CheckEqualsString('DirTest\DirFileTest.txt', lD.Current, 'First File Name');
      CheckFalse(lD.Next, 'Last File');
    finally
      lD.Close;
    end;

    CheckTrue(lD.FirstDir, 'First Dir');
    try
      CheckEqualsString('DirTest\SubDirTest', lD.Current, 'First Dir Name');
      lSD := fFileOpts.BaseDir(lD.Current);
      CheckEqualsString('Test\DirTest\SubDirTest\', lSD.Description);

      CheckFalse(lD.Next, 'Next Dir');
    finally
      lD.Close;
    end;
  finally
    DisposeOf(lSD);
    DisposeOf(lD);
  end;
end;

procedure TestTD2XFileFactory.TestFullExcluding;
var
  lD, lSD: ID2XIODir;
begin
  ForCode('X').Parse('XEx\w+de');

  lSD := nil;
  lD := fFileOpts.BaseDir('Test');
  try
    CheckEqualsString('Test\', lD.Description, 'Dir Description');

    CheckTrue(lD.FirstFile('File*.txt'), 'First File');
    try
      CheckEqualsString('Test\FileTest.txt', lD.Current, 'First File Name');
      CheckFalse(lD.Next, 'Last File');
    finally
      lD.Close;
    end;

    CheckTrue(lD.FirstDir, 'First Dir');
    try
      CheckEqualsString('Test\DirTest', lD.Current, 'First Dir Name');
      lSD := fFileOpts.BaseDir(lD.Current);
      CheckEqualsString('Test\DirTest\', lSD.Description);

      CheckFalse(lD.Next, 'Last Dir');
    finally
      lD.Close;
    end;
  finally
    DisposeOf(lSD);
    DisposeOf(lD);
  end;
end;

procedure TestTD2XFileFactory.TestGetDuration;
var
  lW: TStopWatch;
begin
  lW := TStopWatch.StartNew;
  Sleep(1234);
  lW.Stop;
  CheckEquals(lW.Elapsed.TotalSeconds, fFileOpts.GetDuration(lW), 'Duration seconds');
end;

procedure TestTD2XFileFactory.TestGetInputStream;
var
  lSR: TStreamReader;
begin
  lSR := fFileOpts.GetInputStream;
  try
    CloseHandle(GetStdHandle(STD_INPUT_HANDLE));
    CheckEqualsString('', lSR.ReadToEnd, 'Read Closed Standard Input');
  finally
    FreeAndNil(lSR);
  end;
end;

procedure TestTD2XFileFactory.TestGetNow;
begin
  CheckEqualsString(FormatDateTime('yyyy-mmm-dd HH:nn:ss.zzz', Now), fFileOpts.GetNow,
    'Get Now');
end;

procedure TestTD2XFileFactory.TestGobalName;
var
  lGlobal: TD2XParam;
  lDefault: string;
begin
  lGlobal := ForCode('G');
  lDefault := ChangeFileExt(ExtractFileName(ParamStr(0)), '');

  CheckEqualsString(lDefault, fFileOpts.GlobalName, 'Default Global Name');

  lGlobal.Parse('GGlobal');
  CheckEqualsString('Global', fFileOpts.GlobalName, 'Global Name');
  CheckValidator('Global Name');

  fFileOpts.SetGlobalName('Test');
  CheckEqualsString('Test', fFileOpts.GlobalName, 'Test Global Name');
  CheckValidator('Test Global Name');

  lGlobal.Parse('G');
  CheckEqualsString(lDefault, fFileOpts.GlobalName, 'Reset Global Name');
  CheckValidator('Reset Global Name');
end;

procedure TestTD2XFileFactory.TestLogFileOrExtn;
var
  lGlobal, lOutput: TD2XParam;
  pExtn: string;
  lDS: ID2XIO;
begin
  pExtn := '.Extn';
  lGlobal := ForCode('G');
  lOutput := ForCode('O');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'Log\File.Extn', 'Log File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Log Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\Global.Extn', 'Log Global Extn');

  lOutput.Parse('O-');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'File.Extn', 'Output Off File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, fFileOpts.GlobalName + '.Extn', 'Output Off Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Global.Extn', 'Output Off Global Extn');

  lOutput.Parse('O+');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'Log\File.Extn', 'Output On File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Output On Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\Global.Extn', 'Output On Global Extn');

  lOutput.Parse('O:Out');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'Out\File.Extn', 'Output File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Out\' + fFileOpts.GlobalName + '.Extn', 'Output Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Out\Global.Extn', 'Output Global Extn');
end;

procedure TestTD2XFileFactory.TestSimpleFile;
var
  lDS: ID2XIO;
begin
  lDS := fFileOpts.SimpleFile('File.Extn');
  CheckIO(lDS, 'File.Extn', 'Simple File');
end;

procedure TestTD2XFileFactory.TestTimestampFiles;
var
  lGlobal: TD2XParam;
  pExtn: string;
  lDS: ID2XIO;
begin
  pExtn := '.Extn';
  lGlobal := ForCode('G');

  CheckFalse(fFileOpts.TimestampFiles, 'Timestamp Files Default');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'Log\File.Extn', 'Default Timestamp File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Default Timestamp Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\Global.Extn', 'Default Timestamp Global Extn');

  fTimeBool.SetFlag(True);
  CheckTrue(fFileOpts.TimestampFiles, 'Timestamp Files On');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'Log\File' + fFileOpts.OutputTimestamp + '.Extn', 'On Timestamp File');

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\' + fFileOpts.GlobalName + fFileOpts.OutputTimestamp + '.Extn',
    'On Timestamp Default Extn');

  ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\Global' + fFileOpts.OutputTimestamp + '.Extn', 'On Timestamp Global Extn');

  fTimeBool.SetFlag(False);
  CheckFalse(fFileOpts.TimestampFiles, 'Timestamp Files Off');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'Log\File.Extn', 'Off Timestamp File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Off Timestamp Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\Global.Extn', 'Off Timestamp Global Extn');

  fTimeBool.SetFlag(True);
  CheckTrue(fFileOpts.TimestampFiles, 'Timestamp Files');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'Log\File' + fFileOpts.OutputTimestamp + '.Extn', 'Timestamp File');

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\' + fFileOpts.GlobalName + fFileOpts.OutputTimestamp + '.Extn',
    'Timestamp Default Extn');

  ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\Global' + fFileOpts.OutputTimestamp + '.Extn', 'Timestamp Global Extn');
end;

{ TestTD2XFileOptions }

procedure TestTD2XFileOptions.TestDescribeAll;
begin
  fParams.DescribeAll(fLog);
  CheckLog(UsageDescription + GlobalDescription + FILE_SHOW_OPTIONS + DESCRIPTION_SUFFIX,
    'Describe No Params');
end;

procedure TestTD2XFileOptions.TestForCode;
begin
  Check(Assigned(ForCode('I')), 'Code found for Input Param');
  Check(Assigned(ForCode('O')), 'Code found for Output Param');
  Check(Assigned(ForCode('B')), 'Code found for Base Param');
  Check(Assigned(ForCode('G')), 'Code found for Global Param');
  Check(Assigned(ForCode('X')), 'Code found for Exclusions Param');
end;

procedure TestTD2XFileOptions.TestOutputAll;
var
  lPrm: TD2XParam;
begin
  fParams.OutputAll(fL);
  CheckList('-X:', 'Output No Params');

  for lPrm in fParams do
    lPrm.Convert(':Test');

  fParams.OutputAll(fL);
  CheckList('-G:Test -I::Test -O::Test -B-(:Test) -X:', 'All Params Changed');

  fParams.ZeroAll;

  fParams.OutputAll(fL);
  CheckList('-G -I- -O- -B- -X:', 'All Params Zeroed');
end;

procedure TestTD2XFileOptions.TestParseB;
var
  lBase: TD2XParam;
begin
  lBase := ForCode('B');
  CheckTrue(lBase.Parse('B'), 'Parse just Base code');
  CheckTrue(lBase.Parse('B-'), 'Parse Base Off');
  CheckTrue(lBase.Parse('B+'), 'Parse Base On');
  CheckTrue(lBase.Parse('B!'), 'Parse Base Reset');
  CheckTrue(lBase.Parse('B:Test'), 'Parse Base Value');
end;

procedure TestTD2XFileOptions.TestParseG;
var
  lGlobal: TD2XParam;
begin
  lGlobal := ForCode('G');
  CheckTrue(lGlobal.Parse('G'), 'Parse just Global code');
  CheckTrue(lGlobal.Parse('G-'), 'Parse Global Off');
  CheckTrue(lGlobal.Parse('G+'), 'Parse Global On');
  CheckTrue(lGlobal.Parse('G!'), 'Parse Global Reset');
  CheckTrue(lGlobal.Parse('G:Test'), 'Parse Global Value');
end;

procedure TestTD2XFileOptions.TestParseI;
var
  lInput: TD2XParam;
begin
  lInput := ForCode('I');
  CheckTrue(lInput.Parse('I'), 'Parse just Input code');
  CheckTrue(lInput.Parse('I-'), 'Parse Input Off');
  CheckTrue(lInput.Parse('I+'), 'Parse Input On');
  CheckTrue(lInput.Parse('I!'), 'Parse Input Reset');
  CheckTrue(lInput.Parse('I:Test'), 'Parse Input Value');
end;

procedure TestTD2XFileOptions.TestParseO;
var
  lOutput: TD2XParam;
begin
  lOutput := ForCode('O');
  CheckTrue(lOutput.Parse('O'), 'Parse just Output code');
  CheckTrue(lOutput.Parse('O-'), 'Parse Output Off');
  CheckTrue(lOutput.Parse('O+'), 'Parse Output On');
  CheckTrue(lOutput.Parse('O!'), 'Parse Output Reset');
  CheckTrue(lOutput.Parse('O:Test'), 'Parse Output Value');
end;

procedure TestTD2XFileOptions.TestParseX;
var
  lExclude: TD2XParam;
begin
  ForCode('I').Parse('I:Test');

  lExclude := ForCode('X');
  CheckFalse(lExclude.Parse('X'), 'Parse just Exclude code');
  CheckTrue(lExclude.Parse('X!'), 'Parse Exclude Reset');
  CheckTrue(lExclude.Parse('X:Test'), 'Parse Exclude Value');
end;

procedure TestTD2XFileOptions.TestReportAll;
begin
  fParams.ReportAll(fLog);
  CheckLog(REPORT_HEADING + FILE_REPORT_OPTIONS, 'All Params Default');
end;

procedure TestTD2XFileOptions.TestResetAll;
var
  lPrm: TD2XParam;
begin
  fParams.ReportAll(fLog);
  CheckLog(REPORT_HEADING + FILE_REPORT_OPTIONS, 'All Params Default');

  for lPrm in fParams do
    lPrm.Convert(':Test');

  fParams.ReportAll(fLog);
  CheckLog(REPORT_HEADING + FILE_CHANGED_OPTIONS, 'All Params Changed');

  fParams.ResetAll;

  fParams.ReportAll(fLog);
  CheckLog(REPORT_HEADING + FILE_REPORT_OPTIONS, 'All Params Reset');
end;

procedure TestTD2XFileOptions.TestZeroAll;
var
  lPrm: TD2XParam;
begin
  fParams.ReportAll(fLog);
  CheckLog(REPORT_HEADING + FILE_REPORT_OPTIONS, 'All Params Default');

  for lPrm in fParams do
    lPrm.Convert(':Test');

  fParams.ReportAll(fLog);
  CheckLog(REPORT_HEADING + FILE_CHANGED_OPTIONS, 'All Params Changed');

  fParams.ZeroAll;

  fParams.ReportAll(fLog);
  CheckLog(REPORT_HEADING + 'Global name Config dir - Log dir - Base dir - Exclude Files/Dirs',
    'All Params Zeroed');
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
  CheckEqualsString('Test', lResult, 'Dir only');

  ConvertDir('Test\', '', lResult);
  CheckEqualsString('Test', lResult, 'Dir only, Path Char removed');

  ConvertDir('Test', 'Default', lResult);
  CheckEqualsString('Test', lResult, 'Both');
end;

procedure TestTD2XOptionGeneral.TestConvertDirExtn;
var
  lResult: string;
begin
  ConvertDirExtn('', '', lResult);
  CheckEqualsString('', lResult, 'Empty All, No change');

  ConvertDirExtn('', 'Default,def', lResult);
  CheckEqualsString(',def', lResult, 'Default only, No change');

  ConvertDirExtn('!', 'Default,def', lResult);
  CheckEqualsString('Default,def', lResult, 'Default only, Default Dir');

  ConvertDirExtn('Test', '', lResult);
  CheckEqualsString('Test', lResult, 'Dir only');

  ConvertDirExtn('Test\', '', lResult);
  CheckEqualsString('Test', lResult, 'Dir only, Path Char removed');

  ConvertDirExtn(',tst', '', lResult);
  CheckEqualsString(',tst', lResult, 'Extn only');

  ConvertDirExtn(',.tst', '', lResult);
  CheckEqualsString(',tst', lResult, 'Extn only, Dot Char removed');

  ConvertDirExtn('Test', 'Default,def', lResult);
  CheckEqualsString('Test,def', lResult, 'Both, Dir only');

  ConvertDirExtn(',tst', 'Default,def', lResult);
  CheckEqualsString(',tst', lResult, 'Both, Dir only');

  ConvertDirExtn('!,tst', 'Default,def', lResult);
  CheckEqualsString('Default,tst', lResult, 'Both, Dir Default only');

  ConvertDirExtn('!,!', 'Default,def', lResult);
  CheckEqualsString('Default,def', lResult, 'Both, Both Default');

  ConvertDirExtn('Test,tst', 'Default,def', lResult);
  CheckEqualsString('Test,tst', lResult, 'Both, Both');
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

procedure TestTD2XOptionGeneral.TestSplitDirExtn;
var
  lDir, lExtn: string;
begin
  SplitDirExtn('', lDir, lExtn);
  CheckEqualsString('', lDir, 'Blank string dir');
  CheckEqualsString('', lExtn, 'Blank string extn');

  SplitDirExtn('Dir', lDir, lExtn);
  CheckEqualsString('Dir', lDir, 'Dir string dir');
  CheckEqualsString('', lExtn, 'Dir string extn');

  SplitDirExtn('Dir\', lDir, lExtn);
  CheckEqualsString('Dir', lDir, 'Dir\ string dir');
  CheckEqualsString('', lExtn, 'Dir\ string extn');

  SplitDirExtn('\', lDir, lExtn);
  CheckEqualsString('', lDir, '\ string dir');
  CheckEqualsString('', lExtn, '\ string extn');

  SplitDirExtn(',ext', lDir, lExtn);
  CheckEqualsString('', lDir, ',ext string dir');
  CheckEqualsString('ext', lExtn, ',ext string extn');

  SplitDirExtn(',.ext', lDir, lExtn);
  CheckEqualsString('', lDir, ',.ext string dir');
  CheckEqualsString('ext', lExtn, ',.ext string extn');

  SplitDirExtn(',.', lDir, lExtn);
  CheckEqualsString('', lDir, ',. string dir');
  CheckEqualsString('', lExtn, ',. string extn');

  SplitDirExtn('Dir,ext', lDir, lExtn);
  CheckEqualsString('Dir', lDir, 'Dir,ext string dir');
  CheckEqualsString('ext', lExtn, 'Dir,ext string extn');
end;

initialization

RegisterTests('IO Options', [TestTD2XOptionGeneral.Suite, TestTD2XFileFactory.Suite,
    TestTD2XFileOptions.Suite]);

end.
