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
  ReturnValue: TD2XStream;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I:In');

  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('In\' + fFileOpts.GlobalName + '.Extn', ReturnValue.Description, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('In\Global.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputDirFile;
var
  ReturnValue: TD2XStream;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I:In');

  ReturnValue := fFileOpts.ConfigFileOrExtn(pFile);

  CheckEqualsString('In\File.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOffExtn;
var
  ReturnValue: TD2XStream;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I-');

  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString(fFileOpts.GlobalName + '.Extn', ReturnValue.Description, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('Global.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOffFile;
var
  ReturnValue: TD2XStream;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I-');

  ReturnValue := fFileOpts.ConfigFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOnExtn;
var
  ReturnValue: TD2XStream;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('I').Parse('I+');

  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('Config\' + fFileOpts.GlobalName + '.Extn', ReturnValue.Description, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('Config\Global.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputOnFile;
var
  ReturnValue: TD2XStream;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('I').Parse('I+');

  ReturnValue := fFileOpts.ConfigFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputExtn;
var
  ReturnValue: TD2XStream;
  pExtn: string;
begin
  pExtn := '.Extn';

  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('Config\' + fFileOpts.GlobalName + '.Extn', ReturnValue.Description, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.ConfigFileOrExtn(pExtn);
  CheckEqualsString('Config\Global.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestInputFile;
var
  ReturnValue: TD2XStream;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fFileOpts.ConfigFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputDirExtn;
var
  ReturnValue: TD2XStream;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O:Out');

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Out\' + fFileOpts.GlobalName + '.Extn', ReturnValue.Description, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Out\Global.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputDirFile;
var
  ReturnValue: TD2XStream;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O:Out');

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

  CheckEqualsString('Out\File.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputExtn;
var
  ReturnValue: TD2XStream;
  pExtn: string;
begin
  pExtn := '.Extn';

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + '.Extn', ReturnValue.Description, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\Global.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputFile;
var
  ReturnValue: TD2XStream;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputNoTimestampExtn;
var
  ReturnValue: TD2XStream;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('T').Parse('T-');

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + '.Extn', ReturnValue.Description, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\Global.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputNoTimestampFile;
var
  ReturnValue: TD2XStream;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('T').Parse('T-');

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOffExtn;
var
  ReturnValue: TD2XStream;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O-');

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString(fFileOpts.GlobalName + '.Extn', ReturnValue.Description, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Global.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOffFile;
var
  ReturnValue: TD2XStream;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O-');

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOnExtn;
var
  ReturnValue: TD2XStream;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('O').Parse('O+');

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + '.Extn', ReturnValue.Description, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\Global.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputOnFile;
var
  ReturnValue: TD2XStream;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('O').Parse('O+');

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue.Description, 'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputTimestampExtn;
var
  ReturnValue: TD2XStream;
  pExtn: string;
begin
  pExtn := '.Extn';
  fParams.ForCode('T').Parse('T');

  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\' + fFileOpts.GlobalName + fFileOpts.OutputTimestamp + '.Extn',
    ReturnValue.Description, 'ReturnValue');

  fParams.ForCode('G').Parse('GGlobal');
  ReturnValue := fFileOpts.LogFileOrExtn(pExtn);
  CheckEqualsString('Log\Global' + fFileOpts.OutputTimestamp + '.Extn', ReturnValue.Description,
    'ReturnValue');
end;

procedure TestTD2XFileOptions.TestOutputTimestampFile;
var
  ReturnValue: TD2XStream;
  pFile: string;
begin
  pFile := 'File.Extn';
  fParams.ForCode('T').Parse('T');

  ReturnValue := fFileOpts.LogFileOrExtn(pFile);

  CheckEqualsString('Log\File' + fFileOpts.OutputTimestamp + '.Extn', ReturnValue.Description,
    'ReturnValue');
end;

procedure TestTD2XFileOptions.TestTimestampFiles;
begin
  CheckFalse(fFileOpts.TimestampFiles, 'GlobalName');

  fParams.ForCode('T').Parse('T');
  CheckTrue(fFileOpts.TimestampFiles, 'GlobalName');
end;

initialization

RegisterTests('Stream', [TestTD2XFileOptions.Suite]);

end.
