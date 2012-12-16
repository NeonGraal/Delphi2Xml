unit Test.IO.Options;

interface

implementation

uses
  D2X,
  D2X.IO,
  D2X.IO.Options,
  D2X.Param,
  System.Diagnostics,
  System.SysUtils,
  Test.Param,
  TestFramework;

type
  TD2XFileOptionsTest = class(TD2XFileOptions)
  public
    property OutputTimestamp: string read fOutputTimestamp;
    property TimestampFiles: Boolean read GetTimestampFiles;

  end;

  TestTD2XFileOptions = class(TParamsTestCase)
  private
    fFileOpts: TD2XFileOptionsTest;
    fValidatorCalled: Boolean;

    procedure CheckIO(var pDS: ID2XIO; pExp, pLabel: string);
    procedure CheckValidator(pLabel: string);
    function TestValidator(pStr: string): Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
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

    procedure TestFull;
    procedure TestFullBase;

  end;

  TestTD2XOptionGeneral = class(TTestCase)
  published
    procedure TestConvertDir;
    procedure TestConvertExtn;
    procedure TestConvertFile;
  end;

  { TestTD2XFileOptions }

procedure TestTD2XFileOptions.CheckIO(var pDS: ID2XIO; pExp, pLabel: string);
begin
  try
    CheckEqualsString(pExp, pDS.Description, pLabel);
  finally
    DisposeOf(pDS);
  end;
end;

procedure TestTD2XFileOptions.CheckValidator(pLabel: string);
begin
  CheckTrue(fValidatorCalled, pLabel);
  fValidatorCalled := False;
end;

procedure TestTD2XFileOptions.SetUp;
begin
  inherited;

  fFileOpts := TD2XFileOptionsTest.Create;
  fFileOpts.SetGlobalValidator(TestValidator);
  fFileOpts.RegisterParams(fParams);
end;

procedure TestTD2XFileOptions.TearDown;
begin
  FreeAndNil(fFileOpts);

  inherited;
end;

procedure TestTD2XFileOptions.TestBaseDir;
var
  lBase: TD2XParam;
  lDS: ID2XIO;
begin
  lBase := fParams.ForCode('B');

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

procedure TestTD2XFileOptions.TestBaseFile;
var
  lBase: TD2XParam;
  lDS: ID2XIO;
begin
  lBase := fParams.ForCode('B');

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

procedure TestTD2XFileOptions.TestConfigFileOrExtn;
var
  lGlobal, lInput: TD2XParam;
  lDS: ID2XIO;
  pExtn: string;
begin
  lGlobal := fParams.ForCode('G');
  lInput := fParams.ForCode('I');
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

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'Global.Extn', 'Input Off Global .Extn');

  lInput.Parse('I+');

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');
  CheckIO(lDS, 'Config\File.Extn', 'Input On File');

  lGlobal.Parse('G');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'Config\' + fFileOpts.GlobalName + '.Extn', 'Input On Default .Extn ');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'Config\Global.Extn', 'Input On Global .Extn');

  lInput.Parse('I:In');

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');
  CheckIO(lDS, 'In\File.Extn', 'Input File');

  lGlobal.Parse('G');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'In\' + fFileOpts.GlobalName + '.Extn', 'Input Default .Extn ');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckIO(lDS, 'In\Global.Extn', 'Input Global .Extn');
end;

procedure TestTD2XFileOptions.TestFull;
var
  lD, lSD: ID2XDir;
begin
  lSD := nil;
  lD := fFileOpts.BaseDir('Test');
  try
    CheckEqualsString('Test\', lD.Description, 'Dir Description');

    CheckTrue(lD.FirstFile('File*.txt'), 'First File');
    try
      CheckEqualsString('Test\FileTest.txt', lD.Current, 'First File Name');

      CheckFalse(lD.Next, 'Next File');
    finally
      lD.Close;
    end;

    CheckTrue(lD.FirstDir, 'First Dir');
    try
      CheckEqualsString('Test\DirTest', lD.Current, 'First Dir Name');
      lSD := fFileOpts.BaseDir(lD.Current);
      CheckEqualsString('Test\DirTest\', lSD.Description);

      CheckFalse(lD.Next, 'Next Dir');
    finally
      lD.Close;
    end;
  finally
    DisposeOf(lSD);
    DisposeOf(lD);
  end;
end;

procedure TestTD2XFileOptions.TestFullBase;
var
  lD, lSD: ID2XDir;
begin
  fParams.ForCode('B').Parse('B:Test');

  lD := fFileOpts.BaseDir('DirTest');
  try
    CheckEqualsString('Test\DirTest\', lD.Description, 'Dir Description');

    CheckTrue(lD.FirstFile('*.txt'), 'First File');
    try
      CheckEqualsString('DirTest\DirFileTest.txt', lD.Current, 'First File Name');

      CheckFalse(lD.Next, 'Next File');
    finally
      lD.Close;
    end;

    CheckTrue(lD.FirstDir, 'First Dir');
    try
      CheckEqualsString('DirTest\SubDirTest', lD.Current, 'First Dir Name');
      lSD := fFileOpts.BaseDir(lD.Current);
      CheckEqualsString('Test\DirTest\SubDirTest\', lSD.Description, 'First Dir Description');

      CheckFalse(lD.Next, 'Next Dir');
    finally
      lD.Close;
    end;
  finally
    DisposeOf(lSD);
    DisposeOf(lD);
  end;
end;

procedure TestTD2XFileOptions.TestGetDuration;
var
  lW: TStopWatch;
begin
  lW := TStopwatch.StartNew;
  Sleep(1234);
  lW.Stop;
  CheckEquals(lW.Elapsed.TotalSeconds, fFileOpts.GetDuration(lW), 'Duration seconds');
end;

procedure TestTD2XFileOptions.TestGetNow;
begin
  CheckEqualsString(FormatDateTime('yyyy-mmm-dd HH:nn:ss.zzz', Now), fFileOpts.GetNow,
    'Get Now');
end;

procedure TestTD2XFileOptions.TestGobalName;
var
  lGlobal: TD2XParam;
  lDefault: string;
begin
  lGlobal := fParams.ForCode('G');
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

procedure TestTD2XFileOptions.TestLogFileOrExtn;
var
  lGlobal, lOutput: TD2XParam;
  pExtn: string;
  lDS: ID2XIO;
begin
  pExtn := '.Extn';
  lGlobal := fParams.ForCode('G');
  lOutput := fParams.ForCode('O');

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

procedure TestTD2XFileOptions.TestSimpleFile;
var
  lDS: ID2XIO;
begin
  lDS := fFileOpts.SimpleFile('File.Extn');
  CheckIO(lDS, 'File.Extn', 'Simple File');
end;

procedure TestTD2XFileOptions.TestTimestampFiles;
var
  lGlobal, lTimestamp: TD2XParam;
  pExtn: string;
  lDS: ID2XIO;
begin
  pExtn := '.Extn';
  lGlobal := fParams.ForCode('G');
  lTimestamp := fParams.ForCode('T');

  CheckFalse(fFileOpts.TimestampFiles, 'Timestamp Files Default');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'Log\File.Extn', 'Default Timestamp File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Default Timestamp Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\Global.Extn', 'Default Timestamp Global Extn');

  lTimestamp.Parse('T+');
  CheckTrue(fFileOpts.TimestampFiles, 'Timestamp Files On');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'Log\File' + fFileOpts.OutputTimestamp + '.Extn', 'On Timestamp File');

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\' + fFileOpts.GlobalName + fFileOpts.OutputTimestamp + '.Extn',
    'On Timestamp Default Extn');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\Global' + fFileOpts.OutputTimestamp + '.Extn', 'On Timestamp Global Extn');

  lTimestamp.Parse('T-');
  CheckFalse(fFileOpts.TimestampFiles, 'Timestamp Files Off');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'Log\File.Extn', 'Off Timestamp File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Off Timestamp Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\Global.Extn', 'Off Timestamp Global Extn');

  lTimestamp.Parse('T');
  CheckTrue(fFileOpts.TimestampFiles, 'Timestamp Files');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckIO(lDS, 'Log\File' + fFileOpts.OutputTimestamp + '.Extn', 'Timestamp File');

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\' + fFileOpts.GlobalName + fFileOpts.OutputTimestamp + '.Extn',
    'Timestamp Default Extn');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckIO(lDS, 'Log\Global' + fFileOpts.OutputTimestamp + '.Extn', 'Timestamp Global Extn');
end;

function TestTD2XFileOptions.TestValidator(pStr: string): Boolean;
begin
  fValidatorCalled := True;
  Result := True;
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

initialization

RegisterTests('IO', [TestTD2XOptionGeneral.Suite, TestTD2XFileOptions.Suite]);

end.
