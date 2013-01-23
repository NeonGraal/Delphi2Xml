unit Test.IO.Tests;

interface

implementation

uses
  D2X,
  D2X.IO,
  D2X.Param,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.SysUtils,
  System.Types,
  System.RegularExpressions,
  System.StrUtils,
  Test.Constants,
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

  TestID2XFile = class(TTestCase)
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

  { TestTD2XFile }

procedure TestID2XFile.SetUp;
begin
  inherited;

  fFile := TTestFile.Create('Test', True, 'Test Input', nil);
end;

procedure TestID2XFile.TearDown;
begin
  FreeAndNil(fFile);

  inherited;
end;

procedure TestID2XFile.TestDescription;
begin
  CheckEqualsString('Test', fFile.Description, 'Description');
end;

procedure TestID2XFile.TestExists;
begin
  CheckTrue(fFile.Exists, 'Exists');
end;

procedure TestID2XFile.TestReadFrom;
begin
  CheckEqualsString('Test Input', fFile.ReadFrom.ReadToEnd, 'Read From');
end;

procedure TestID2XFile.TestWriteTo;
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

initialization

RegisterTests('IO', [TestID2XIO.Suite, TestID2XFile.Suite, TestID2XDir.Suite]);

end.
