unit D2X.Stream.Test;

interface

uses
  D2X,
  D2X.Stream,
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
  TestTD2XIO = class(TTestCase)
  private
    fIO: TTestIO;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestExists;

  end;

  TestTD2XFile = class(TTestCase)
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

  TestTD2XDir = class(TTestCase)
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

procedure TestTD2XFile.SetUp;
begin
  inherited;

  fFile := TTestFile.Create('Test', True, 'Test Input');
end;

procedure TestTD2XFile.TearDown;
begin
  FreeAndNil(fFile);

  inherited;
end;

procedure TestTD2XFile.TestDescription;
begin
  CheckEqualsString('Test', fFile.Description, 'Description');
end;

procedure TestTD2XFile.TestExists;
begin
  CheckTrue(fFile.Exists, 'Exists');
end;

procedure TestTD2XFile.TestReadFrom;
begin
  CheckEqualsString('Test Input', fFile.ReadFrom.ReadToEnd, 'Read From');
end;

procedure TestTD2XFile.TestWriteTo;
begin
  with fFile.WriteTo do begin
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

procedure TestTD2XIO.SetUp;
begin
  inherited;

  fIO := TTestIO.Create('Test', True);
end;

procedure TestTD2XIO.TearDown;
begin
  FreeAndNil(fIO);

  inherited;
end;

procedure TestTD2XIO.TestDescription;
begin
  CheckEqualsString('Test', fIO.Description, 'Description');
end;

procedure TestTD2XIO.TestExists;
begin
  CheckTrue(fIO.Exists, 'Exists');
end;

{ TestTD2XDir }

procedure TestTD2XDir.SetUp;
begin
  inherited;

  fDir := TTestDir.Create('Test', True);
end;

procedure TestTD2XDir.TearDown;
begin
  FreeAndNil(fDir);

  inherited;
end;

procedure TestTD2XDir.TestClose;
begin
  fDir.Close;
  Check(True, 'Close');
end;

procedure TestTD2XDir.TestCurrent;
begin
  CheckEqualsString('', fDir.Current, 'Current');
end;

procedure TestTD2XDir.TestDescription;
begin
  CheckEqualsString('Test', fDir.Description, 'Description');
end;

procedure TestTD2XDir.TestExists;
begin
  CheckTrue(fDir.Exists, 'Exists');
end;

procedure TestTD2XDir.TestFirstDir;
begin
  CheckFalse(fDir.FirstDir, 'First Dir');
end;

procedure TestTD2XDir.TestFirstFile;
begin
  CheckFalse(fDir.FirstFile('*'), 'First File');
end;

procedure TestTD2XDir.TestNext;
begin
  CheckFalse(fDir.Next, 'Next');
end;

initialization

RegisterTests('Stream', [TestTD2XIO.Suite, TestTD2XFile.Suite, TestTD2XDir.Suite]);

end.
