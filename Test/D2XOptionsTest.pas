unit D2XOptionsTest;

interface

uses
  System.Classes,
  TestFramework,
  System.StrUtils,
  D2XOptions,
  System.SysUtils;

type
  // Test methods for class TD2X
  TestTD2X = class(TTestCase)
  strict private
    FD2X: TD2X;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestToLabel;
  end;

  TestTD2XOptionBase = class(TTestCase)
  strict protected
    fOpts: TD2XOptions;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TD2XOptions
  TestTD2XOptionFilenames = class(TestTD2XOptionBase)
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

  // Test methods for class TD2XOptions
  TestTD2XOptions = class(TestTD2XOptionBase)
  private
    fLog: TStringStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure CheckLog(pMsg: String);
  published
    procedure TestParseOptionBlank;
    procedure TestParseOptionWrongPrefix;
    procedure TestParseOptionOptionA;
    procedure TestParseOptionOptionH;
    procedure TestParseOptionOptionJ;
    procedure TestParseOptionOptionK;
    procedure TestParseOptionOptionQ;
    procedure TestParseOptionOptionY;

    procedure TestReportOptions;
    procedure TestShowOptions;
  end;

implementation

{ TestTD2X }

procedure TestTD2X.SetUp;
begin
  FD2X := TD2X.Create;
end;

procedure TestTD2X.TearDown;
begin
  FD2X.Free;
  FD2X := nil;
end;

procedure TestTD2X.TestToLabel;
var
  ReturnValue: string;
  pVal: TD2XParseMode;
begin
  pVal := pmFull;

  ReturnValue := FD2X.ToLabel(pVal);

  CheckEqualsString('Full', ReturnValue);
end;

{ TestTD2XOptionBase }

procedure TestTD2XOptionBase.SetUp;
begin
  fOpts := TD2XOptions.Create;
end;

procedure TestTD2XOptionBase.TearDown;
begin
  FreeAndNil(fOpts);
end;

{ TestTD2XOptionFilenames }

procedure TestTD2XOptionFilenames.TestInputDirExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-i:In');

  ReturnValue := fOpts.InputFileOrExtn(pExtn);

  CheckEqualsString('In\' + fOpts.GlobalName + '.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestInputDirFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-i:In');

  ReturnValue := fOpts.InputFileOrExtn(pFile);

  CheckEqualsString('In\File.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestInputOffExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-i-');

  ReturnValue := fOpts.InputFileOrExtn(pExtn);

  CheckEqualsString(fOpts.GlobalName + '.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestInputOffFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-i-');

  ReturnValue := fOpts.InputFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestInputOnExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-i+');

  ReturnValue := fOpts.InputFileOrExtn(pExtn);

  CheckEqualsString('Config\' + fOpts.GlobalName + '.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestInputOnFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-i+');

  ReturnValue := fOpts.InputFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestInputExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';

  ReturnValue := fOpts.InputFileOrExtn(pExtn);

  CheckEqualsString('Config\' + fOpts.GlobalName + '.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestInputFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fOpts.InputFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputDirExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-o:Out');

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString('Out\' + fOpts.GlobalName + '.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputDirFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-o:Out');

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Out\File.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString('Log\' + fOpts.GlobalName + '.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputNoTimestampExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-t-');

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString('Log\' + fOpts.GlobalName + '.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputNoTimestampFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-t-');

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputOffExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-o-');

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString(fOpts.GlobalName + '.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputOffFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-o-');

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputOnExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-o+');

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString('Log\' + fOpts.GlobalName + '.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputOnFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-o+');

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputTimestampExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-t');

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString('Log\' + fOpts.GlobalName + fOpts.OutputTimestamp + '.Extn', ReturnValue);
end;

procedure TestTD2XOptionFilenames.TestOutputTimestampFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-t');

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File' + fOpts.OutputTimestamp + '.Extn', ReturnValue);
end;

{ TestTD2XOptions }

procedure TestTD2XOptions.CheckLog(pMsg: String);
begin
  CheckEqualsString(pMsg, Trim(fLog.DataString));
end;

procedure TestTD2XOptions.SetUp;
begin
  inherited;

  fLog := TStringStream.Create;
  fOpts.SetLog(fLog);
end;

procedure TestTD2XOptions.TearDown;
begin
  FreeAndNil(fLog);

  inherited;
end;

procedure TestTD2XOptions.TestParseOptionBlank;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '';
  StartExpectingException(ED2XOptionsException);
  ReturnValue := fOpts.ParseOption(pOpt);
  StopExpectingException;
  CheckFalse(ReturnValue);
end;

procedure TestTD2XOptions.TestParseOptionOptionA;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-A';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue);
  CheckLog('Unknown option: -A');
end;

procedure TestTD2XOptions.TestParseOptionOptionH;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-H';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue);
  CheckLog('Unknown option: -H');
end;

procedure TestTD2XOptions.TestParseOptionOptionJ;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-J';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue);
  CheckLog('Unknown option: -J');
end;

procedure TestTD2XOptions.TestParseOptionOptionK;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-K';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue);
  CheckLog('Unknown option: -K');
end;

procedure TestTD2XOptions.TestParseOptionOptionQ;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-Q';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue);
  CheckLog('Unknown option: -Q');
end;

procedure TestTD2XOptions.TestParseOptionOptionY;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-Y';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue);
  CheckLog('Unknown option: -Y');
end;

procedure TestTD2XOptions.TestParseOptionWrongPrefix;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '=V';
  StartExpectingException(ED2XOptionsException);
  ReturnValue := fOpts.ParseOption(pOpt);
  StopExpectingException;
  CheckFalse(ReturnValue);
end;

procedure TestTD2XOptions.TestReportOptions;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ReportOptions;

  Check(ReturnValue);
  CheckNotEqualsString('', fLog.DataString);
end;

procedure TestTD2XOptions.TestShowOptions;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ShowOptions;

  Check(ReturnValue);
  CheckNotEqualsString('', fLog.DataString);
end;

initialization

RegisterTests([TestTD2X.Suite, TestTD2XOptionFilenames.Suite, TestTD2XOptions.Suite]);

end.
