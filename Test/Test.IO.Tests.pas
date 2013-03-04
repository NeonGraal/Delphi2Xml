unit Test.IO.Tests;

interface

implementation

uses
  D2X.Flag,
  D2X.Global,
  D2X.IO,
  D2X.Param,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.SysUtils,
  System.Types,



  Test.IO,
  TestFramework;

type
  TestID2XIO = class(TTestCase)
  private
    fIO: TTestIO;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestExists;

  end;

  TestID2XIOFile = class(TTestCase)
  private
    fFile: TTestFile;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestExists;
    procedure TestReadFrom;
    procedure TestWriteTo;

  end;

  TestID2XDir = class(TTestCase)
  private
    fDir: TTestDir;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestExists;

    procedure TestFirstFile;
    procedure TestFirstDir;
    procedure TestNext;
    procedure TestClose;
    procedure TestCurrent;

  end;

  TestID2XIOFactory = class(TTestCase)
  private
    fFact: TTestFactory;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConfigFileOrExtn;
    procedure TestLogFileOrExtn;
    procedure TestBaseFile;
    procedure TestBaseDir;
    procedure TestSimpleFile;

    procedure TestSetGlobalName;
    procedure TestSetGlobalValidator;
    procedure TestSetTimestampFlag;
    procedure TestRegisterParams;
    procedure TestGetNow;
    procedure TestGetDuration;
    procedure TestGetInputStream;

    procedure TestCheckOutput;
  end;

  { TestTD2XFile }

procedure TestID2XIOFile.SetUp;
begin
  inherited;

  fFile := TTestFile.Create('Test', True, 'Test Input', nil);
end;

procedure TestID2XIOFile.TearDown;
begin
  FreeAndNil(fFile);

  inherited;
end;

procedure TestID2XIOFile.TestDescription;
begin
  CheckEqualsString('Test', fFile.Description, 'Description');
end;

procedure TestID2XIOFile.TestExists;
begin
  CheckTrue(fFile.Exists, 'Exists');
end;

procedure TestID2XIOFile.TestReadFrom;
begin
  CheckEqualsString('Test Input', fFile.ReadFrom.ReadToEnd, 'Read From');
end;

procedure TestID2XIOFile.TestWriteTo;
begin
  with fFile.WriteTo do
  begin
    write('Testing');
    Flush;
  end;
  CheckEqualsString('Testing', fFile.Written, 'Write To');
end;

{ TestTD2XIO }

procedure TestID2XIO.SetUp;
begin
  inherited;

  fIO := TTestIO.Create('Test', True);
end;

procedure TestID2XIO.TearDown;
begin
  FreeAndNil(fIO);

  inherited;
end;

procedure TestID2XIO.TestDescription;
begin
  CheckEqualsString('Test', fIO.Description, 'Description');
end;

procedure TestID2XIO.TestExists;
begin
  CheckTrue(fIO.Exists, 'Exists');
end;

{ TestTD2XDir }

procedure TestID2XDir.SetUp;
begin
  inherited;

  fDir := TTestDir.Create('Test', True, [], []);
end;

procedure TestID2XDir.TearDown;
begin
  FreeAndNil(fDir);

  inherited;
end;

procedure TestID2XDir.TestClose;
begin
  fDir.Close;
  Check(True, 'Close');
end;

procedure TestID2XDir.TestCurrent;
begin
  CheckEqualsString('', fDir.Current, 'Current');
end;

procedure TestID2XDir.TestDescription;
begin
  CheckEqualsString('Test', fDir.Description, 'Description');
end;

procedure TestID2XDir.TestExists;
begin
  CheckTrue(fDir.Exists, 'Exists');
end;

procedure TestID2XDir.TestFirstDir;
begin
  CheckFalse(fDir.FirstDir, 'First Dir');
end;

procedure TestID2XDir.TestFirstFile;
begin
  CheckFalse(fDir.FirstFile('*'), 'First File');
end;

procedure TestID2XDir.TestNext;
begin
  CheckFalse(fDir.Next, 'Next');
end;

{ TestID2XIOFactory }

procedure TestID2XIOFactory.SetUp;
begin
  inherited;

  fFact := TTestFactory.Create;
end;

procedure TestID2XIOFactory.TearDown;
begin
  FreeAndNil(fFact);

  inherited;
end;

procedure TestID2XIOFactory.TestBaseDir;
var
  lDir: ID2XIODir;
begin
  lDir := fFact.BaseDir('Test');
  try
    CheckEqualsString('Test', lDir.Description, 'Base Dir Description');
  finally
    DisposeOf(lDir);
  end;
end;

procedure TestID2XIOFactory.TestBaseFile;
var
  lFile: ID2XIOFile;
begin
  lFile := fFact.BaseFile('File');
  try
    CheckEqualsString('File', lFile.Description, 'Base File Description');
  finally
    DisposeOf(lFile);
  end;
end;

procedure TestID2XIOFactory.TestCheckOutput;
var
  lFile: ID2XIOFile;
begin
  lFile := fFact.SimpleFile('File');
  try
    lFile.WriteTo.Write('Test');
  finally
    DisposeOf(lFile);
  end;
  CheckEquals(1, Length(fFact.CheckFiles), 'Files output');
  CheckEqualsString('Test', fFact.CheckOutput('File'), 'Simple File Output');
end;

procedure TestID2XIOFactory.TestConfigFileOrExtn;
var
  lFile: ID2XIOFile;
begin
  lFile := fFact.ConfigFileOrExtn('File');
  try
    CheckEqualsString('File', lFile.Description, 'Config File Description');
  finally
    DisposeOf(lFile);
  end;
end;

procedure TestID2XIOFactory.TestGetDuration;
var
  lWatch: TStopwatch;
begin
  lWatch := TStopwatch.Create.StartNew;
  lWatch.Stop;

  CheckEquals(1.234, fFact.GetDuration(lWatch), 0.0001, 'Get Duration');
end;

procedure TestID2XIOFactory.TestGetInputStream;
var
  lSR: TStreamReader;
begin
  lSR := fFact.GetInputStream;
  try
    CheckEqualsString('', lSR.ReadToEnd, 'Read Test Input Stream');
  finally
    FreeAndNil(lSR);
  end;
end;

procedure TestID2XIOFactory.TestGetNow;
begin
  CheckEqualsString('2011-Nov-11 11:11:11.111', fFact.GetNow, 'Get Now')
end;

procedure TestID2XIOFactory.TestLogFileOrExtn;
var
  lFile: ID2XIOFile;
begin
  lFile := fFact.LogFileOrExtn('File');
  try
    CheckEqualsString('File', lFile.Description, 'Log File Description');
  finally
    DisposeOf(lFile);
  end;
end;

procedure TestID2XIOFactory.TestRegisterParams;
var
  lParams: TD2XParams;
begin
  lParams := TD2XParams.Create;
  try
    fFact.RegisterParams(lParams);
    CheckEquals(0, lParams.Count, 'No params created');
  finally
    FreeAndNil(lParams);
  end;
end;

procedure TestID2XIOFactory.TestSetGlobalName;
begin
  fFact.SetGlobalName('Test');
  CheckEqualsString('Test', fFact.GlobalName, 'Global name');
end;

procedure TestID2XIOFactory.TestSetGlobalValidator;
var
  lGVCalled: Boolean;
begin
  lGVCalled := False;
  fFact.SetGlobalValidator(
      function(pVal: string): Boolean
    begin
      lGVCalled := True;
      Result := True;
    end);

  CheckFalse(lGVCalled, 'Validator not called yet');
  fFact.SetGlobalName('Test');
  CheckTrue(lGVCalled, 'Validator called');
end;

procedure TestID2XIOFactory.TestSetTimestampFlag;
var
  lFlag: TD2XBoolFlag;
  lFile: ID2XIOFile;
begin
  lFlag := TD2XBoolFlag.Create;
  try
    fFact.SetTimestampFlag(lFlag);

    ID2XFlag(lFlag).Flag := False;
    lFile := fFact.LogFileOrExtn('File.Extn');
    CheckEqualsString('File.Extn', lFile.Description, 'Log File not Timestamped');
    DisposeOf(lFile);

    ID2XFlag(lFlag).Flag := True;
    lFile := fFact.LogFileOrExtn('File.Extn');
    CheckEqualsString('File-Timestamp.Extn', lFile.Description, 'Log File Timestamped');

    fFact.SetTimestampFlag(nil);
  finally
    DisposeOf(lFile);
    FreeAndNil(lFlag);
  end;
end;

procedure TestID2XIOFactory.TestSimpleFile;
var
  lFile: ID2XIOFile;
begin
  lFile := fFact.SimpleFile('File');
  try
    CheckEqualsString('File', lFile.Description, 'Simple File Description');
  finally
    DisposeOf(lFile);
  end;
end;

initialization

RegisterTests('IO', [TestID2XIO.Suite, TestID2XIOFile.Suite, TestID2XDir.Suite,
  TestID2XIOFactory.Suite]);

end.
