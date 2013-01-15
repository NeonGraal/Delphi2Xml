unit Test.IO;

interface

uses
  D2X,
  D2X.IO,
  D2X.Param,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.SysUtils,
  System.Types;

type
  ETestIOException = class(Exception);

  TTestIO = class(TD2XInterfaced, ID2XIO)
  private
    fDesc: string;
    fExists: Boolean;

  public
    constructor Create(pDesc: string; pExists: Boolean);

    function Description: string;
    function Exists: Boolean;
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

  TATestDir = record
    Dirs, Files: TStringDynArray;

    constructor Create(pDirs, pFiles: array of string);
  end;

  TTestFactory = class(TD2XInterfaced, ID2XIOFactory)
  private
    fConfigFiles: TDictionary<string, string>;
    fBaseFiles: TDictionary<string, string>;
    fBaseDirs: TDictionary<string, TATestDir>;
    fOutputFiles: TDictionary<string, string>;

    fGlobalName: string;
    fValidator: TD2XSingleParam<string>.TspValidator;

    procedure InitFiles;

    procedure StoreOutput(pFile, pOutput: string);

  public
    constructor Create;
    destructor Destroy; override;

    function ConfigFileOrExtn(pFileOrExtn: string): ID2XFile;
    function LogFileOrExtn(pFileOrExtn: string): ID2XFile;
    function BaseFile(pFileOrDir: string): ID2XFile;
    function BaseDir(pFileOrDir: string): ID2XDir;
    function SimpleFile(pFile: string): ID2XFile;

    procedure SetGlobalName(const pName: string);
    procedure SetGlobalValidator(pValidator: TD2XSingleParam<string>.TspValidator);
    procedure SetTimestampFlag(pFlag: ID2XFlag);
    procedure RegisterParams(pParams: TD2XParams);
    function GetNow: string;
    function GetDuration(pWatch: TStopwatch): Double;

    function CheckFiles: TArray<string>;
    function CheckOutput(pFile: string): string;

    property GlobalName: string read fGlobalName write SetGlobalName;
  end;

  TTestFile = class(TTestIO, ID2XFile)
  private
    fSR: TStreamReader;
    fSW: TStreamWriter;
    fSS: TStringStream;

    fAppending: Boolean;
    fInput: string;
    fFact: TTestFactory;

  public
    constructor Create(pDesc: string; pExists: Boolean; pInput: string; pFact: TTestFactory);
    destructor Destroy; override;

    function Written: string;

    function ReadFrom: TStreamReader;
    function WriteTo(pAppend: Boolean = False): TStreamWriter;

  end;

implementation

uses
  System.RegularExpressions,
  System.StrUtils,
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

{ TTestFile }

constructor TTestFile.Create(pDesc: string; pExists: Boolean; pInput: string;
  pFact: TTestFactory);
begin
  inherited Create(pDesc, pExists);

  fAppending := False;
  fInput := pInput;
  fFact := pFact;

  fSR := nil;
  fSW := nil;

  fSS := TStringStream.Create;
end;

destructor TTestFile.Destroy;
var
  lData: string;
begin
  lData := fSS.DataString;
  FreeAndNil(fSS);
  FreeAndNil(fSR);

  if Assigned(fSW) then
  begin
    FreeAndNil(fSW);
    inherited;
    if lData > '' then
    begin
      if Assigned(fFact) then
        fFact.StoreOutput(Description, lData)
      else
        if not fAppending then
          raise ETestIOException.Create('Unchecked (' + Description + '): ' + lData);
    end;
  end
  else
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
  fAppending := pAppend;
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
  FreeAndNil(fSW);
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
  lD: TATestDir;
begin
  pFileOrDir := ExcludeTrailingPathDelimiter(pFileOrDir);
  if fBaseDirs.ContainsKey(pFileOrDir) then
    lD := fBaseDirs[pFileOrDir]
  else
    raise ETestIOException.Create('Unknown test Base dir : ' + pFileOrDir);

  Result := TTestDir.Create(pFileOrDir, True, lD.Dirs, lD.Files);
end;

function TTestFactory.BaseFile(pFileOrDir: string): ID2XFile;
var
  lInput: string;
begin
  if fBaseFiles.ContainsKey(pFileOrDir) then
    lInput := fBaseFiles[pFileOrDir]
  else
    raise ETestIOException.Create('Unknown test Base file: ' + pFileOrDir);

  Result := TTestFile.Create(pFileOrDir, lInput > '', lInput, Self);
end;

function TTestFactory.CheckFiles: TArray<string>;
begin
  Result := fOutputFiles.Keys.ToArray;
end;

function TTestFactory.CheckOutput(pFile: string): string;
begin
  if fOutputFiles.TryGetValue(pFile, Result) then
    fOutputFiles.Remove(pFile);
end;

function TTestFactory.ConfigFileOrExtn(pFileOrExtn: string): ID2XFile;
var
  lInput: string;
begin
  if fConfigFiles.ContainsKey(pFileOrExtn) then
    lInput := fConfigFiles[pFileOrExtn]
  else
    raise ETestIOException.Create('Unknown test Config file: ' + pFileOrExtn);

  Result := TTestFile.Create(pFileOrExtn, True, lInput, Self);
end;

constructor TTestFactory.Create;
begin
  inherited;

  fConfigFiles := TDictionary<string, string>.Create;
  fBaseFiles := TDictionary<string, string>.Create;
  fBaseDirs := TDictionary<string, TATestDir>.Create;
  fOutputFiles := TDictionary<string, string>.Create;

  InitFiles;
end;

destructor TTestFactory.Destroy;
var
  lMsg, lSep, lS: string;
begin
  lMsg := '';
  lSep := 'Unchecked: ';
  for lS in fOutputFiles.Keys do
  begin
    lMsg := lMsg + lSep + lS;
    lSep := ', ';
  end;

  FreeAndNil(fOutputFiles);
  FreeAndNil(fBaseDirs);
  FreeAndNil(fBaseFiles);
  FreeAndNil(fConfigFiles);

  inherited;
  if lMsg > '' then
    raise ETestIOException.Create(lMsg);
end;

function TTestFactory.GetDuration(pWatch: TStopwatch): Double;
begin
  Result := 1.234;
end;

function TTestFactory.GetNow: string;
begin
  Result := '2011-Nov-11 11:11:11.111';
end;

procedure TTestFactory.InitFiles;
const
  TESTING_UNIT = 'unit Testing.Test.AUnit; interface uses System.Classes;' +
    '{$DEFINE TEST} {$INCLUDE Test.inc} {$D+}' + 'implementation uses System.SysUtils;' +
    '{$IFDEF TEST}{$DEFINE TEST1}{$ELSE}{$DEFINE TEST2}{$ENDIF}' +
    '{$IFNDEF TEST}{$DEFINE TEST3}{$ENDIF}' + '{$IFOPT D+}{$DEFINE TEST6}{$ENDIF}' +
    '{$IF Defined(TEST)}{$DEFINE TEST4}{$ELSEIF Defined(TEST1)}{$DEFINE TEST5}{$ENDIF}' +
    'end.';
  TESTING_PROGRAM = 'program Testing.Test.AProgram; ' +
    'uses Testing.Test.AUnit in ''Testing.Test.AUnit.pas''; begin end.';
  TESTING_PACKAGE = 'package Testing.Test.APackage; ' +
    'contains Testing.Test.AUnit in ''Testing.Test.AUnit.pas''; end.';
begin
  fConfigFiles.Add('Test.def', 'Uniform'#13#10'Victor'#13#10'Tango');
  fConfigFiles.Add('Test.prm', '-@'#13#10'-@Test.out');
  fConfigFiles.Add('File', '');
  fConfigFiles.Add('.Extn', '');
  fConfigFiles.Add('File.Extn', '');
  fConfigFiles.Add('.skip', '');

  fBaseFiles.Add('Testing.Test*', '');
  fBaseFiles.Add('Testing.Test.AUnit.pas', TESTING_UNIT);
  fBaseFiles.Add('Testing.Test.AProgram.dpr', TESTING_PROGRAM);
  fBaseFiles.Add('Testing.Test.APackage.dpk', TESTING_PACKAGE);
  fBaseFiles.Add('Config\Testing.Test.Dir.pas',
    'unit Testing.Test.Dir; interface implementation end.');
  fBaseFiles.Add('Config\Test\Testing.Test.SubDir.pas',
    'unit Testing.Test.SubDir; interface implementation end.');

  fBaseDirs.Add('', TATestDir.Create(['Config', 'Test'], ['Testing.Test.AUnit.pas',
        'Testing.Test.AProgram.dpr', 'Testing.Test.APackage.dpk']));
  fBaseDirs.Add('Config', TATestDir.Create(['Test'], ['Testing.Test.Dir.pas']));
  fBaseDirs.Add('Config\Test', TATestDir.Create([], ['Testing.Test.SubDir.pas']));
  fBaseDirs.Add('Test', TATestDir.Create([], []));
end;

function TTestFactory.LogFileOrExtn(pFileOrExtn: string): ID2XFile;
var
  lInput: string;
begin
  Result := TTestFile.Create(pFileOrExtn, True, lInput, Self);
end;

procedure TTestFactory.RegisterParams(pParams: TD2XParams);
begin

end;
{$HINTS OFF}

procedure TTestFactory.SetGlobalName(const pName: string);
var
  lRes: Boolean;
begin
  fGlobalName := pName;
  if Assigned(fValidator) then
    lRes := fValidator(pName);
end;
{$HINTS ON}

procedure TTestFactory.SetGlobalValidator(pValidator: TD2XSingleParam<string>.TspValidator);
begin
  fValidator := pValidator;
end;

procedure TTestFactory.SetTimestampFlag(pFlag: ID2XFlag);
begin

end;

function TTestFactory.SimpleFile(pFile: string): ID2XFile;
var
  lInput: string;
begin
  Result := TTestFile.Create(pFile, True, lInput, Self);
end;

procedure TTestFactory.StoreOutput(pFile, pOutput: string);
begin
  fOutputFiles.AddOrSetValue(pFile, pOutput);
end;

{ TATestDir }

constructor TATestDir.Create(pDirs, pFiles: array of string);
var
  i: Integer;
begin
  SetLength(Dirs, Length(pDirs));
  for i := 0 to High(pDirs) do
    Dirs[i] := pDirs[i];
  SetLength(Files, Length(pFiles));
  for i := 0 to High(pFiles) do
    Files[i] := pFiles[i];
end;

initialization

RegisterTests('IO', [TestID2XIO.Suite, TestID2XFile.Suite, TestID2XDir.Suite]);

end.
