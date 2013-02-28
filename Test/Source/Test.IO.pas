unit Test.IO;

interface

uses
  D2X.Global,
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

  TTestDir = class(TTestIO, ID2XIODir)
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
    fTimestampFlag: ID2XFlag;

    procedure InitFiles;

    procedure StoreOutput(pFile, pOutput: string);

  public
    constructor Create;
    destructor Destroy; override;

    function ConfigFileOrExtn(pFileOrExtn: string): ID2XIOFile;
    function LogFileOrExtn(pFileOrExtn: string): ID2XIOFile;
    function BaseFile(pFileOrDir: string): ID2XIOFile;
    function BaseDir(pFileOrDir: string): ID2XIODir;
    function SimpleFile(pFile: string): ID2XIOFile;

    procedure SetGlobalName(const pName: string);
    procedure SetGlobalValidator(pValidator: TD2XSingleParam<string>.TspValidator);
    procedure SetTimestampFlag(pFlag: ID2XFlag);
    procedure RegisterParams(pParams: TD2XParams);
    function GetNow: string;
    function GetDuration(pWatch: TStopwatch): Double;
    function GetInputStream: TStreamReader;

    function CheckFiles: TArray<string>;
    function CheckOutput(pFile: string): string;

    property GlobalName: string read fGlobalName write SetGlobalName;
  end;

  TTestFile = class(TTestIO, ID2XIOFile)
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
  Test.Constants,
  TestFramework;

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

{ TTestFactor }

function TTestFactory.BaseDir(pFileOrDir: string): ID2XIODir;
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

function TTestFactory.BaseFile(pFileOrDir: string): ID2XIOFile;
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

function TTestFactory.ConfigFileOrExtn(pFileOrExtn: string): ID2XIOFile;
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

function TTestFactory.GetInputStream: TStreamReader;
begin
  Result := TStreamReader.Create(TStringStream.Create(''));
  Result.OwnStream;
end;

function TTestFactory.GetNow: string;
begin
  Result := '2011-Nov-11 11:11:11.111';
end;

procedure TTestFactory.InitFiles;
begin
  fConfigFiles.Add('Test.def', 'Uniform'#13#10'Victor'#13#10'Tango');
  fConfigFiles.Add('Test.prm', '-@'#13#10'-@Test.out');
  fConfigFiles.Add('File', '');
  fConfigFiles.Add('.Extn', '');
  fConfigFiles.Add('File.Extn', '');
  fConfigFiles.Add('.skip', '');

  fBaseFiles.Add('File', '');
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

function TTestFactory.LogFileOrExtn(pFileOrExtn: string): ID2XIOFile;
var
  lInput: string;
  lExtn: string;
begin
  if Assigned(fTimestampFlag) and fTimestampFlag.Flag then
  begin
    lExtn := ExtractFileExt(pFileOrExtn);
    Result := TTestFile.Create(ChangeFileExt(pFileOrExtn, '-Timestamp' + lExtn), True,
      lInput, Self);
  end
  else
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
  fTimestampFlag := pFlag;
end;

function TTestFactory.SimpleFile(pFile: string): ID2XIOFile;
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

end.
