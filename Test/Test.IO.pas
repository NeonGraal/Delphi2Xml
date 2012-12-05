unit Test.IO;

interface

uses
  D2X,
  D2X.IO,
  D2X.Param,
  System.Classes,
  System.Generics.Collections,
  System.Types;

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
  private
    fDirs, fFiles: TStringDynArray;
    fCurr: TStack<string>;

  public
    constructor Create(pDesc: string; pExists: Boolean; pDirs, pFiles: array of string);
    destructor Destroy; override;

    function FirstFile(pWildcard: string): Boolean;
    function FirstDir: Boolean;
    function Next: Boolean;
    procedure Close;
    function Current: string;

  end;

  TTestFactory = class(TD2XInterfaced, ID2XIOFactory)
  public
    constructor Create;

    function ConfigFileOrExtn(pFileOrExtn: string): ID2XFile;
    function LogFileOrExtn(pFileOrExtn: string): ID2XFile;
    function BaseFile(pFileOrDir: string): ID2XFile;
    function BaseDir(pFileOrDir: string): ID2XDir;
    function SimpleFile(pFile: string): ID2XFile;

    procedure SetGlobalName(const pName: string);
    procedure SetGlobalValidator(pValidator: TD2XSingleParam<string>.TspValidator);
    procedure RegisterParams(pParams: TD2XParams);
  end;

implementation

uses
  System.RegularExpressions,
  System.StrUtils,
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

constructor TTestDir.Create(pDesc: string; pExists: Boolean; pDirs, pFiles: array of string);
var
  i: Integer;
begin
  inherited Create(pDesc, pExists);

  SetLength(fDirs, Length(pDirs));
  for i := 0 to High(pDirs) do
    fDirs[i] := pDirs[i];
  SetLength(fFiles, Length(pFiles));
  for i := 0 to High(pFiles) do
    fFiles[i] := pFiles[i];
  fCurr := TStack<string>.Create;
end;

function TTestDir.Current: string;
begin
  if fCurr.Count > 0 then
  begin
    if fDesc > '' then
      Result := IncludeTrailingPathDelimiter(fDesc) + fCurr.Peek
    else
      Result := fCurr.Peek;
  end
  else
    Result := '';
end;

destructor TTestDir.Destroy;
begin
  FreeAndNil(fCurr);

  inherited;
end;

function TTestDir.FirstDir: Boolean;
var
  i: Integer;
begin
  Result := Length(fDirs) > 0;
  fCurr.Clear;
  for i := High(fDirs) downto 0 do
    fCurr.Push(fDirs[i]);
end;

function TTestDir.FirstFile(pWildcard: string): Boolean;
var
  i: Integer;
  r: TRegEx;
begin
  Result := Length(fFiles) > 0;
  fCurr.Clear;
  r := TRegEx.Create(ReplaceStr(ReplaceStr(ReplaceStr(pWildcard, '.', '\.'), '?', '.?'),
      '*', '.*'));
  for i := High(fFiles) downto 0 do
    if r.IsMatch(fFiles[i]) then
      fCurr.Push(fFiles[i]);
end;

function TTestDir.Next: Boolean;
begin
  Result := fCurr.Count > 1;
  if Result then
    fCurr.Pop;
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

{ TTestFactor }

function TTestFactory.BaseDir(pFileOrDir: string): ID2XDir;
var
  lDirs, lFiles: TStringDynArray;
begin
  if StartsText('Config\Test', pFileOrDir) then
  begin
    SetLength(lDirs, 0);
    SetLength(lFiles, 1);
    lFiles[0] := 'Testing.TestSubDir.pas';
  end
  else
    if StartsText('Config', pFileOrDir) then
    begin
      SetLength(lDirs, 1);
      lDirs[0] := 'Test';
      SetLength(lFiles, 1);
      lFiles[0] := 'Testing.TestDir.pas';
    end
    else
      if pFileOrDir = '' then
      begin
        SetLength(lDirs, 2);
        lDirs[0] := 'Config';
        lDirs[1] := 'Test';
        SetLength(lFiles, 2);
        lFiles[0] := 'Testing.TestUnit.pas';
        lFiles[1] := 'Testing.TestProgram.dpr';
      end;

  Result := TTestDir.Create(pFileOrDir, True, lDirs, lFiles);
end;

function TTestFactory.BaseFile(pFileOrDir: string): ID2XFile;
var
  lInput: string;
const
  TESTING_UNIT = 'unit Testing.TestUnit; interface uses System.Classes;' +
    '{$DEFINE TEST} {$INCLUDE Test.inc} {$D+}' +
    'implementation uses System.SysUtils;' +
    '{$IFDEF TEST}{$DEFINE TEST1}{$ELSE}{$DEFINE TEST2}{$ENDIF}' +
    '{$IFNDEF TEST}{$DEFINE TEST3}{$ENDIF}' +
    '{$IFOPT D+}{$DEFINE TEST6}{$ENDIF}' +
    '{$IF Defined(TEST)}{$DEFINE TEST4}{$ELSEIF Defined(TEST1)}{$DEFINE TEST5}{$ENDIF}' +
    'end.';
begin
  if pFileOrDir = 'Testing.Test*.pas' then
    Result := TTestFile.Create(pFileOrDir, False, lInput)
  else
  begin
    if pFileOrDir = 'Testing.TestUnit.pas' then
      lInput := TESTING_UNIT
    else
      if pFileOrDir = 'Testing.TestProgram.dpr' then
        lInput := 'program Testing.TestProgram; uses Testing.TestUnit in ''Testing.TestUnit.pas''; begin end.';

    Result := TTestFile.Create(pFileOrDir, True, lInput);
  end;
end;

function TTestFactory.ConfigFileOrExtn(pFileOrExtn: string): ID2XFile;
var
  lInput: string;
begin
  if pFileOrExtn = 'Test.def' then
    lInput := 'Uniform'#13#10'Tango'
  else
    if pFileOrExtn = 'Test.prm' then
      lInput := '-@'#13#10'-@Test.out';

  Result := TTestFile.Create(pFileOrExtn, True, lInput);
end;

constructor TTestFactory.Create;
begin
  inherited;

end;

function TTestFactory.LogFileOrExtn(pFileOrExtn: string): ID2XFile;
var
  lInput: string;
begin
  Result := TTestFile.Create(pFileOrExtn, True, lInput);
end;

procedure TTestFactory.RegisterParams(pParams: TD2XParams);
begin

end;

procedure TTestFactory.SetGlobalName(const pName: string);
begin

end;

procedure TTestFactory.SetGlobalValidator(pValidator: TD2XSingleParam<string>.TspValidator);
begin

end;

function TTestFactory.SimpleFile(pFile: string): ID2XFile;
var
  lInput: string;
begin
  Result := TTestFile.Create(pFile, True, lInput);
end;

initialization

RegisterTests('IO', [TestID2XIO.Suite, TestID2XFile.Suite, TestID2XDir.Suite]);

end.
