unit Test.IO.Actual;

interface

implementation

uses
  D2X,
  D2X.IO,
  D2X.IO.Actual,
  System.Classes,
  System.SysUtils,
  Test.Test,
  TestFramework;

type
  TestTD2XFileStream = class(TStringTestCase)
  private
    fFile: TD2XFileStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestExists;
    procedure TestReadFrom;
    procedure TestWriteTo;

  end;

  TestTD2XDirPath = class(TTestCase)
  private
    fDir: TD2XDirPath;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestExists;

    procedure TestFiles;
    procedure TestDirs;

  end;

  { TestTD2XFileStream }

procedure TestTD2XFileStream.SetUp;
begin
  inherited;

  fFile := TD2XFileStream.Create('Testing.Unit.pas');
end;

procedure TestTD2XFileStream.TearDown;
begin
  FreeAndNil(fFile);

  inherited;
end;

procedure TestTD2XFileStream.TestDescription;
begin
  CheckEqualsString('Testing.Unit.pas', fFile.Description, 'Description');
end;

procedure TestTD2XFileStream.TestExists;
begin
  CheckTrue(fFile.Exists, 'Exists');
end;

procedure TestTD2XFileStream.TestReadFrom;
const
  EXPECTED_FILE = 'unit Test; interface uses System.Classes; {$DEFINE TEST} ' +
    '{$INCLUDE Test.inc} {$D+} implementation uses System.SysUtils; {$IFDEF TEST} ' +
    '{$DEFINE TEST1} {$ELSE} {$DEFINE TEST2} {$ENDIF} {$IFNDEF TEST} {$DEFINE TEST3} ' +
    '{$ENDIF} {$IFOPT D+} {$DEFINE TEST6} {$ENDIF} {$IF Defined(TEST)} {$DEFINE TEST4} ' +
    '{$ELSEIF Defined(TEST1)} {$DEFINE TEST5} {$ENDIF} end.';
begin
  CheckReader(EXPECTED_FILE, 'Read From', fFile.ReadFrom);
end;

procedure TestTD2XFileStream.TestWriteTo;
var
  lExpected: String;
begin
  fFile.Free;

  lExpected := 'This is a test text file written at ' + FormatDateTime('dddddd tt', now);

  fFile := TD2XFileStream.Create('Test\TestOut.txt');
  fFile.WriteTo.Write(lExpected);
  fFile.Free;

  fFile := TD2XFileStream.Create('Test\TestOut.txt');
  CheckReader(lExpected, 'Write To', fFile.ReadFrom);
end;

{ TestTD2XDirPath }

procedure TestTD2XDirPath.SetUp;
begin
  inherited;

  fDir := TD2XDirPath.Create('Config');
end;

procedure TestTD2XDirPath.TearDown;
begin
  FreeAndNil(fDir);

  inherited;
end;

procedure TestTD2XDirPath.TestDescription;
begin
  CheckEqualsString('Config\', fDir.Description);
end;

procedure TestTD2XDirPath.TestExists;
begin
  CheckTrue(fDir.Exists, 'Exists');
end;

procedure TestTD2XDirPath.TestDirs;
begin
  CheckTrue(fDir.FirstDir, 'First Dir');
  CheckEqualsString('Config\Test', fDir.Current, 'First Dir Current');
  CheckFalse(fDir.Next, 'First Dir Next');
  fDir.Close;
end;

procedure TestTD2XDirPath.TestFiles;
begin
  CheckTrue(fDir.FirstFile('*.pas'), 'First File');
  CheckEqualsString('Config\Testing.Dir.pas', fDir.Current, 'First File Current');
  CheckFalse(fDir.Next, 'First File Next');
  fDir.Close;
end;

initialization

RegisterTests('IO', [TestTD2XFileStream.Suite, TestTD2XDirPath.Suite]);

end.
