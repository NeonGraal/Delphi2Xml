unit D2X.FileOpts.Test;

interface

implementation

uses
  D2X.FileOpts,
  D2X.Param.Test,
  D2X.Stream,
  System.SysUtils,
  TestFramework;

type
  TestTD2XFileOptions = class(TParamsTestCase)
  private
    fFileOpts: TD2XFileOptions;

    fDS: TD2XStream;

    procedure CheckStream(pExp, pLabel: String);
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

procedure TestTD2XFileOptions.CheckStream(pExp, pLabel: String);
begin
  CheckEqualsString(pExp, fDS.Description, pLabel);
  FreeAndNil(fDS);
end;

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
  pExtn: String;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I:In');

  fDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream('In\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  fDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream('In\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestInputDirFile;
var
  pFile: String;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I:In');

  fDS := fFileOpts.ConfigFileOrExtn(pFile);

  CheckStream('In\File.Extn', 'Return Value');

end;

procedure TestTD2XFileOptions.TestInputOffExtn;
var

  pExtn: String;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I-');

  fDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream(fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  fDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream('Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestInputOffFile;
var

  pFile: String;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I-');

  fDS := fFileOpts.ConfigFileOrExtn(pFile);

  CheckStream('File.Extn', 'Return Value');

end;

procedure TestTD2XFileOptions.TestInputOnExtn;
var

  pExtn: String;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I+');

  fDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream('Config\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  fDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream('Config\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestInputOnFile;
var

  pFile: String;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I+');

  fDS := fFileOpts.ConfigFileOrExtn(pFile);

  CheckStream('Config\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestInputExtn;
var

  pExtn: String;
begin
  pExtn := '.Extn';

  fDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream('Config\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  fDS := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckStream('Config\Global.Extn', 'Return Value');

end;

procedure TestTD2XFileOptions.TestInputFile;
var
  pFile: String;
begin
  pFile := 'File.Extn';

  fDS := fFileOpts.ConfigFileOrExtn(pFile);

  CheckStream('Config\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputDirExtn;
var
  pExtn: String;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O:Out');

  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream('Out\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream('Out\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputDirFile;
var
  pFile: String;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O:Out');

  fDS := fFileOpts.LogFileOrExtn(pFile);

  CheckStream('Out\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputExtn;
var

  pExtn: String;
begin
  pExtn := '.Extn';

  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream('Log\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream('Log\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputFile;
var

  pFile: String;
begin
  pFile := 'File.Extn';

  fDS := fFileOpts.LogFileOrExtn(pFile);

  CheckStream('Log\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputNoTimestampExtn;
var

  pExtn: String;
begin
  pExtn := '.Extn';
  fParams.ForCode('T').Parse('T-');

  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream('Log\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream('Log\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputNoTimestampFile;
var
  pFile: String;
begin
  pFile := 'File.Extn';
  fParams.ForCode('T').Parse('T-');

  fDS := fFileOpts.LogFileOrExtn(pFile);

  CheckStream('Log\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputOffExtn;
var

  pExtn: String;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O-');

  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream(fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream('Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputOffFile;
var

  pFile: String;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O-');

  fDS := fFileOpts.LogFileOrExtn(pFile);

  CheckStream('File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputOnExtn;
var
  pExtn: String;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O+');

  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream('Log\' + fFileOpts.GlobalName + '.Extn', 'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream('Log\Global.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputOnFile;
var
  pFile: String;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O+');

  fDS := fFileOpts.LogFileOrExtn(pFile);

  CheckStream('Log\File.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputTimestampExtn;
var

  pExtn: String;
begin
  pExtn := '.Extn';
  fParams.ForCode('T').Parse('T');

  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream('Log\' + fFileOpts.GlobalName + fFileOpts.OutputTimestamp + '.Extn',
    'Return Value');

  fParams.ForCode('G').Parse('GGlobal');
  fDS := fFileOpts.LogFileOrExtn(pExtn);
  CheckStream('Log\Global' + fFileOpts.OutputTimestamp + '.Extn', 'Return Value');
end;

procedure TestTD2XFileOptions.TestOutputTimestampFile;
var

  pFile: String;
begin
  pFile := 'File.Extn';
  fParams.ForCode('T').Parse('T');

  fDS := fFileOpts.LogFileOrExtn(pFile);

  CheckStream('Log\File' + fFileOpts.OutputTimestamp + '.Extn', 'Return Value');
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
