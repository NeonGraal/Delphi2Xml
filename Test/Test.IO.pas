unit Test.IO;

interface

uses
  D2X,
  D2X.IO,
  System.Classes;

type
  TTestIO = class(TD2XInterfaced, ID2XIO)
  private
    fDesc: string;
    fExists: Boolean;

  public
    constructor Create(pDesc: string; pExists: Boolean);

    function Description: string;
    function Exists: Boolean;
  end;

  TTestFile = class(TTestIO, ID2XFile)
  private
    fSR: TStreamReader;
    fSW: TStreamWriter;
    fSS: TStringStream;

    fInput: string;

  public
    constructor Create(pDesc: string; pExists: Boolean; pInput: string);
    destructor Destroy; override;

    function Written: string;

    function ReadFrom: TStreamReader;
    function WriteTo(pAppend: Boolean = False): TStreamWriter;

  end;

  TTestDir = class(TTestIO, ID2XDir)
  public
    constructor Create(pDesc: string; pExists: Boolean);
    destructor Destroy; override;

    function FirstFile(pWildcard: string): Boolean;
    function FirstDir: Boolean;
    function Next: Boolean;
    procedure Close;
    function Current: string;

  end;

implementation

uses
  System.SysUtils,
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

  fFile := TTestFile.Create('Test', True, 'Test Input');
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
    Write('Testing');
    Flush;
  end;
  CheckEqualsString('Testing', fFile.Written, 'Write To');
end;

{ TTestFile }

constructor TTestFile.Create(pDesc: string; pExists: Boolean; pInput: string);
begin
  inherited Create(pDesc, pExists);

  fInput := pInput;

  fSR := nil;
  fSW := nil;

  fSS := TStringStream.Create;
end;

destructor TTestFile.Destroy;
begin
  FreeAndNil(fSR);
  FreeAndNil(fSW);
  FreeAndNil(fSS);

  inherited;
end;

function TTestFile.ReadFrom: TStreamReader;
begin
  if Assigned(fSR) then
    fSR.Free;

  fSR := TStreamReader.Create(TStringStream.Create(fInput));
  fSR.OwnStream;
  Result := fSR;
end;

function TTestFile.WriteTo(pAppend: Boolean): TStreamWriter;
begin
  if Assigned(fSW) then
    fSW.Free;
  fSW := TStreamWriter.Create(fSS);
  fSS.Clear;
  Result := fSW;
end;

function TTestFile.Written: string;
begin
  if Assigned(fSW) then
    fSW.Flush;
  Result := fSS.DataString;
  fSS.Clear;
end;

{ TTestIO }

constructor TTestIO.Create(pDesc: string; pExists: Boolean);
begin
  fDesc := pDesc;
  fExists := pExists;
end;

function TTestIO.Description: string;
begin
  Result := fDesc;
end;

function TTestIO.Exists: Boolean;
begin
  Result := fExists;
end;

{ TTestDir }

procedure TTestDir.Close;
begin

end;

constructor TTestDir.Create(pDesc: string; pExists: Boolean);
begin
  inherited Create(pDesc, pExists);
end;

function TTestDir.Current: string;
begin
  Result := '';
end;

destructor TTestDir.Destroy;
begin

  inherited;
end;

function TTestDir.FirstDir: Boolean;
begin
  Result := False;
end;

function TTestDir.FirstFile(pWildcard: string): Boolean;
begin
  Result := False;
end;

function TTestDir.Next: Boolean;
begin
  Result := False;
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

  fDir := TTestDir.Create('Test', True);
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
