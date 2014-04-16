unit Test.IO.Actual.Tests;

interface

implementation

uses
  D2X.Global,

  D2X.IO.Actual,
  System.Classes,
  System.SysUtils,
  Test.Constants,
  Test.Global,
  TestFramework;

type
  TestTD2XFileBase = class(TStringTestCase)
  private
    fFile: TD2XFileBase;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestExists;

  end;

  TestTD2XFileStream = class(TStringTestCase)
  private
    fFile: TD2XFileStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReadFrom;
    procedure TestWriteTo;

  end;

  TestTD2XFileNil = class(TStringTestCase)
  private
    fFile: TD2XFileNil;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReadFrom;
    procedure TestWriteTo;

  end;

  TestTD2XDirPath = class(TTestCase)
  private
    fDir: TD2XDirPath;
    fExcludes: TStringList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestExists;

    procedure TestFiles;
    procedure TestDirs;
    procedure TestExcludes;

  end;

  { TestTD2XFileStream }

procedure TestTD2XFileStream.SetUp;
begin
  inherited;

  fFile := TD2XFileStream.Create('Testing.Test.AUnit.pas');
end;

procedure TestTD2XFileStream.TearDown;
begin
  FreeAndNil(fFile);

  inherited;
end;

procedure TestTD2XFileStream.TestReadFrom;
begin
  CheckReader(TESTING_UNIT, 'Read From', fFile.ReadFrom);
end;

procedure TestTD2XFileStream.TestWriteTo;
var
  lExpected: String;
const
  TEST_FILE_NAME = 'Test\TestTD2XFileStream-OutTest.txt';
begin
  fFile.Free;

  lExpected := 'This is a test text file written at ' + FormatDateTime('dddddd tt', now);

  fFile := TD2XFileStream.Create(TEST_FILE_NAME);
  fFile.WriteTo.Write(lExpected);
  fFile.Free;

  fFile := TD2XFileStream.Create(TEST_FILE_NAME);
  CheckTrue(fFile.Exists, 'Test file exists');
  CheckReader(lExpected, 'Write To', fFile.ReadFrom);

  DeleteFile(TEST_FILE_NAME);
end;

{ TestTD2XDirPath }

procedure TestTD2XDirPath.SetUp;
begin
  inherited;

  fExcludes := TStringList.Create;
  fDir := TD2XDirPath.Create('', 'Test', fExcludes);
end;

procedure TestTD2XDirPath.TearDown;
begin
  FreeAndNil(fDir);
  FreeAndNil(fExcludes);

  inherited;
end;

procedure TestTD2XDirPath.TestDescription;
begin
  CheckEqualsString('Test\', fDir.Description);
end;

procedure TestTD2XDirPath.TestExcludes;
begin
  fExcludes.CommaText := 'Exclude';
  CheckTrue(fDir.FirstFile('File*.txt'), 'First File');
  CheckEqualsString('Test\FileTest.txt', fDir.Current, 'First File Current');
  CheckFalse(fDir.Next, 'First File Last');
  fDir.Close;

  CheckTrue(fDir.FirstDir, 'First Dir');
  CheckEqualsString('Test\DirTest', fDir.Current, 'First Dir Current');
  CheckFalse(fDir.Next, 'First Dir Next');
  fDir.Close;
end;

procedure TestTD2XDirPath.TestExists;
begin
  CheckTrue(fDir.Exists, 'Exists');
end;

procedure TestTD2XDirPath.TestDirs;
begin
  CheckTrue(fDir.FirstDir, 'First Dir');
  CheckEqualsString('Test\DirExclude', fDir.Current, 'Exclude Dir Current');
  CheckTrue(fDir.Next, 'First Dir Next');
  CheckEqualsString('Test\DirTest', fDir.Current, 'First Dir Current');
  CheckFalse(fDir.Next, 'First Dir Last');
  fDir.Close;
end;

procedure TestTD2XDirPath.TestFiles;
begin
  CheckTrue(fDir.FirstFile('File*.txt'), 'First File');
  CheckEqualsString('Test\FileExclude.txt', fDir.Current, 'Exclude File Current');
  CheckTrue(fDir.Next, 'First File Next');
  CheckEqualsString('Test\FileTest.txt', fDir.Current, 'First File Current');
  CheckFalse(fDir.Next, 'First File Last');
  fDir.Close;
end;

{ TestTD2XFileBase }

procedure TestTD2XFileBase.SetUp;
begin
  inherited;

  fFile := TD2XFileBase.Create('Testing.Test.AUnit.pas');
end;

procedure TestTD2XFileBase.TearDown;
begin
  FreeAndNil(fFile);

  inherited;
end;

procedure TestTD2XFileBase.TestDescription;
begin
  CheckEqualsString('Testing.Test.AUnit.pas', fFile.Description, 'Description');
end;

procedure TestTD2XFileBase.TestExists;
begin
  CheckTrue(fFile.Exists, 'Exists');
end;

{ TestTD2XFileNil }

procedure TestTD2XFileNil.SetUp;
begin
  inherited;

  fFile := TD2XFileNil.Create('Testing.Test.AUnit.pas');
end;

procedure TestTD2XFileNil.TearDown;
begin
  FreeAndNil(fFile);

  inherited;
end;

procedure TestTD2XFileNil.TestReadFrom;
begin
  CheckReader('', 'Read From', fFile.ReadFrom);
end;

procedure TestTD2XFileNil.TestWriteTo;
var
  lExpected: String;
  lReader : TD2XFileStream;
const
  TEST_FILE_NAME = 'Test\TestTD2XFileNil-OutTest.txt';
begin
  fFile.Free;

  lExpected := 'This is a test text file written at ' + FormatDateTime('dddddd tt', now);

  fFile := TD2XFileNil.Create(TEST_FILE_NAME);
  fFile.WriteTo.Write(lExpected);

  lReader := TD2XFileStream.Create(TEST_FILE_NAME);
  CheckFalse(lReader.Exists, 'Test file doesn''t exist');
  lReader.Free;
end;

initialization

RegisterTests('IO Actual', [TestTD2XFileBase.Suite, TestTD2XFileStream.Suite, TestTD2XFileNil.Suite, TestTD2XDirPath.Suite]);

end.
