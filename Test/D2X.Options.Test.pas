unit D2X.Options.Test;

interface

implementation

uses
  D2X,
  D2X.Utils,
  D2X.Param,
  D2X.Options,
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  TestFramework;

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

  TestTD2XOptionFilenames = class(TTestCase)
  private
    fFileOpts: TD2XFileOptions;
    fParams: TD2XParams;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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

{ TestTD2XOptionFilenames }

procedure TestTD2XOptionFilenames.SetUp;
begin
  inherited;

  fFileOpts := TD2XFileOptions.Create(nil);

  fParams := TD2XParams.Create;
  fFileOpts.RegisterParams(fParams);
end;

procedure TestTD2XOptionFilenames.TearDown;
begin
  FreeAndNil(fFileOpts);
  FreeAndNil(fParams);

  inherited;
end;

procedure TestTD2XOptionFilenames.TestInputDirExtn;
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

procedure TestTD2XOptionFilenames.TestInputDirFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I:In');

  ReturnValue := fFileOpts.InputFileOrExtn(pFile);

  CheckEqualsString('In\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestInputOffExtn;
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

procedure TestTD2XOptionFilenames.TestInputOffFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I-');

  ReturnValue := fFileOpts.InputFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestInputOnExtn;
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

procedure TestTD2XOptionFilenames.TestInputOnFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I+');

  ReturnValue := fFileOpts.InputFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestInputExtn;
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

procedure TestTD2XOptionFilenames.TestInputFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fFileOpts.InputFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputDirExtn;
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

procedure TestTD2XOptionFilenames.TestOutputDirFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O:Out');

  ReturnValue := fFileOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Out\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputExtn;
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

procedure TestTD2XOptionFilenames.TestOutputFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fFileOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputNoTimestampExtn;
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

procedure TestTD2XOptionFilenames.TestOutputNoTimestampFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('T').Parse('T-');

  ReturnValue := fFileOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputOffExtn;
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

procedure TestTD2XOptionFilenames.TestOutputOffFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O-');

  ReturnValue := fFileOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputOnExtn;
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

procedure TestTD2XOptionFilenames.TestOutputOnFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O+');

  ReturnValue := fFileOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputTimestampExtn;
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

procedure TestTD2XOptionFilenames.TestOutputTimestampFile;
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

initialization

RegisterTests('Options', [TestTD2XOptionEnums.Suite, TestTD2XOptionFilenames.Suite{,
     TestTD2XOptionGeneral.Suite, TestTD2XOptions.Suite}]);

end.
