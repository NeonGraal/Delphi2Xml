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

implementation

uses
  D2X,
  D2X.Utils,
  D2X.Options,
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

  TestTD2XOptions = class(TParamsTestCase)
  private
    fOpts: TD2XOptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  end;

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

{ TestTD2XOptions }

procedure TestTD2XOptions.SetUp;
begin
  inherited;

  fOpts := TD2XOptions.Create;

end;

procedure TestTD2XOptions.TearDown;
begin
  FreeAndNil(fOpts);

  inherited;
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

initialization

RegisterTests('Options', [TestTD2XOptionEnums.Suite, TestTD2XOptionGeneral.Suite,
    TestTD2XFileOptions.Suite, TestTD2XOptions.Suite]);

end.
