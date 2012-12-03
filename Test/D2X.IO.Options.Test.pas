unit D2X.IO.Options.Test;

interface

implementation

uses
  D2X,
  D2X.IO.Options,
  D2X.Param,
  D2X.Param.Test,
  D2X.IO,
  System.SysUtils,
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

    procedure CheckStream(var pDS: ID2XFile; pExp, pLabel: string);
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

  end;

  { TestTD2XFileOptions }

procedure TestTD2XFileOptions.CheckStream(var pDS: ID2XFile; pExp, pLabel: string);
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
begin
  fFileOpts.BaseDir('File.Extn');

end;

procedure TestTD2XFileOptions.TestBaseFile;
begin
  fFileOpts.BaseFile('File.Extn');

end;

procedure TestTD2XFileOptions.TestConfigFileOrExtn;
var
  lGlobal, lInput: TD2XParam;
  lDS: ID2XFile;
  pExtn: string;
begin
  lGlobal := fParams.ForCode('G');
  lInput := fParams.ForCode('I');
  pExtn := '.Extn';

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');
  CheckStream(lDS, 'Config\File.Extn', 'Config File');

  lGlobal.Parse('G');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'Config\' + fFileOpts.GlobalName + '.Extn', 'Config Default .Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'Config\Global.Extn', 'Config Global .Extn');

  lInput.Parse('I-');

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');
  CheckStream(lDS, 'File.Extn', 'Input Off File');

  lGlobal.Parse('G');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, fFileOpts.GlobalName + '.Extn', 'Input Off Default .Extn ');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'Global.Extn', 'Input Off Global .Extn');

  lInput.Parse('I+');

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');
  CheckStream(lDS, 'Config\File.Extn', 'Input On File');

  lGlobal.Parse('G');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'Config\' + fFileOpts.GlobalName + '.Extn', 'Input On Default .Extn ');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'Config\Global.Extn', 'Input On Global .Extn');

  lInput.Parse('I:In');

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');
  CheckStream(lDS, 'In\File.Extn', 'Input File');

  lGlobal.Parse('G');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'In\' + fFileOpts.GlobalName + '.Extn', 'Input Default .Extn ');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'In\Global.Extn', 'Input Global .Extn');
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
  lDS: ID2XFile;
begin
  pExtn := '.Extn';
  lGlobal := fParams.ForCode('G');
  lOutput := fParams.ForCode('O');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckStream(lDS, 'Log\File.Extn', 'Log File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Log Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\Global.Extn', 'Log Global Extn');

  lOutput.Parse('O-');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckStream(lDS, 'File.Extn', 'Output Off File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, fFileOpts.GlobalName + '.Extn', 'Output Off Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Global.Extn', 'Output Off Global Extn');

  lOutput.Parse('O+');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckStream(lDS, 'Log\File.Extn', 'Output On File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Output On Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\Global.Extn', 'Output On Global Extn');

  lOutput.Parse('O:Out');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckStream(lDS, 'Out\File.Extn', 'Output File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Out\' + fFileOpts.GlobalName + '.Extn', 'Output Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Out\Global.Extn', 'Output Global Extn');
end;

procedure TestTD2XFileOptions.TestSimpleFile;
begin
  fFileOpts.SimpleFile('File.Extn');

end;

procedure TestTD2XFileOptions.TestTimestampFiles;
var
  lGlobal, lTimestamp: TD2XParam;
  pExtn: string;
  lDS: ID2XFile;
begin
  pExtn := '.Extn';
  lGlobal := fParams.ForCode('G');
  lTimestamp := fParams.ForCode('T');

  CheckFalse(fFileOpts.TimestampFiles, 'Timestamp Files Default');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckStream(lDS, 'Log\File.Extn', 'Default Timestamp File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Default Timestamp Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\Global.Extn', 'Default Timestamp Global Extn');

  lTimestamp.Parse('T+');
  CheckTrue(fFileOpts.TimestampFiles, 'Timestamp Files On');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckStream(lDS, 'Log\File' + fFileOpts.OutputTimestamp + '.Extn', 'On Timestamp File');

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\' + fFileOpts.GlobalName + fFileOpts.OutputTimestamp + '.Extn',
    'On Timestamp Default Extn');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\Global' + fFileOpts.OutputTimestamp + '.Extn',
    'On Timestamp Global Extn');

  lTimestamp.Parse('T-');
  CheckFalse(fFileOpts.TimestampFiles, 'Timestamp Files Off');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckStream(lDS, 'Log\File.Extn', 'Off Timestamp File');

  lGlobal.Parse('G');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Off Timestamp Default Extn');

  lGlobal.Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\Global.Extn', 'Off Timestamp Global Extn');

  lTimestamp.Parse('T');
  CheckTrue(fFileOpts.TimestampFiles, 'Timestamp Files');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');
  CheckStream(lDS, 'Log\File' + fFileOpts.OutputTimestamp + '.Extn', 'Timestamp File');

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\' + fFileOpts.GlobalName + fFileOpts.OutputTimestamp + '.Extn',
    'Timestamp Default Extn');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\Global' + fFileOpts.OutputTimestamp + '.Extn',
    'Timestamp Global Extn');
end;

function TestTD2XFileOptions.TestValidator(pStr: string): Boolean;
begin
  fValidatorCalled := True;
  Result := True;
end;

initialization

RegisterTests('FileOpts', [TestTD2XFileOptions.Suite]);

end.
