unit D2X.IO.Options.Test;

interface

implementation

uses
  D2X,
  D2X.IO.Options,
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

    procedure CheckStream(var pDS: ID2XFile; pExp, pLabel: String);
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

  { TestTD2XFileOptions }

procedure TestTD2XFileOptions.CheckStream(var pDS: ID2XFile; pExp, pLabel: String);
begin
  CheckEqualsString(pExp, pDS.Description, pLabel);
  DisposeOf(pDS);
end;

procedure TestTD2XFileOptions.SetUp;
begin
  inherited;

  fFileOpts := TD2XFileOptionsTest.Create;
  fFileOpts.SetGlobalValidator(nil);
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
  pExtn: String;
  lDS : ID2XFile;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I:In');

  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'In\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'In\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestInputDirFile;
var
  lDS : ID2XFile;
begin
  fParams.ForCode('I').Parse('I:In');

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');

  CheckStream(lDS, 'In\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestInputOffExtn;
var
  pExtn: String;
  lDS : ID2XFile;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I-');

  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestInputOffFile;
var
  lDS : ID2XFile;
begin
  fParams.ForCode('I').Parse('I-');

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');

  CheckStream(lDS, 'File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestInputOnExtn;
var
  pExtn: String;
  lDS : ID2XFile;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I+');

  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'Config\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'Config\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestInputOnFile;
var
  lDS : ID2XFile;
begin
  fParams.ForCode('I').Parse('I+');

  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');

  CheckStream(lDS, 'Config\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestInputExtn;
var
  pExtn: String;
  lDS : ID2XFile;
begin
  pExtn := '.Extn';

  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'Config\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(lDS, 'Config\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestInputFile;
var
  lDS : ID2XFile;
begin
  lDS := fFileOpts.ConfigFileOrExtn('File.Extn');

  CheckStream(lDS, 'Config\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputDirExtn;
var
  pExtn: String;
  lDS : ID2XFile;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O:Out');

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Out\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Out\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputDirFile;
var
  lDS : ID2XFile;
begin
  fParams.ForCode('O').Parse('O:Out');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');

  CheckStream(lDS, 'Out\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputExtn;
var
  pExtn: String;
  lDS : ID2XFile;
begin
  pExtn := '.Extn';

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputFile;
var
  lDS : ID2XFile;
begin
  lDS := fFileOpts.LogFileOrExtn('File.Extn');

  CheckStream(lDS, 'Log\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputNoTimestampExtn;
var
  pExtn: String;
  lDS : ID2XFile;
begin
  pExtn := '.Extn';
  fParams.ForCode('T').Parse('T-');

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputNoTimestampFile;
var
  lDS : ID2XFile;
begin
  fParams.ForCode('T').Parse('T-');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');

  CheckStream(lDS, 'Log\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputOffExtn;
var
  pExtn: String;
  lDS : ID2XFile;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O-');

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputOffFile;
var
  lDS : ID2XFile;
begin
  fParams.ForCode('O').Parse('O-');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');

  CheckStream(lDS, 'File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputOnExtn;
var
  pExtn: String;
  lDS : ID2XFile;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O+');

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputOnFile;
var
  lDS : ID2XFile;
begin
  fParams.ForCode('O').Parse('O+');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');

  CheckStream(lDS, 'Log\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputTimestampExtn;
var
  pExtn: String;
  lDS : ID2XFile;
begin
  pExtn := '.Extn';
  fParams.ForCode('T').Parse('T');

  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\' + fFileOpts.GlobalName + fFileOpts.OutputTimestamp + '.Extn',
    'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  lDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(lDS, 'Log\Global' + fFileOpts.OutputTimestamp + '.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputTimestampFile;
var
  lDS : ID2XFile;
begin
  fParams.ForCode('T').Parse('T');

  lDS := fFileOpts.LogFileOrExtn('File.Extn');

  CheckStream(lDS, 'Log\File' + fFileOpts.OutputTimestamp + '.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestTimestampFiles;
begin
  CheckFalse(fFileOpts.TimestampFiles, 'GlobalName');

  fParams.ForCode('T').Parse('T');
  CheckTrue(fFileOpts.TimestampFiles, 'GlobalName');
end;

initialization

RegisterTests('FileOpts', [TestTD2XFileOptions.Suite]);

end.
