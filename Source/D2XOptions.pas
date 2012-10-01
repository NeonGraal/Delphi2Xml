unit D2XOptions;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes;

type
  ED2XOptionsException = class(Exception);

  TD2XParseMode = (pmFull, pmUses);

  TD2XResultPer = (rpFile, rpWildcard, rpSubDir, rpDir, rpParam, rpRun);

  TD2X = class
    class function ToLabel<T>(pVal: T): string;
  end;

  TD2XOptions = class
  private
    fVerbose: boolean;
    fXml: boolean;
    fCountChildren: boolean;
    fCountFileOrExtn: string;
    fXmlDirectory: string;
    fSkipFileOrExtn: string;
    fSkipMethods: boolean;
    fUseBase: boolean;
    fBaseDirectory: string;
    fRecurse: boolean;
    fLogErrors: boolean;
    fLogNotSupported: boolean;
    fTimestampFiles: boolean;
    fDefinesUsed: boolean;
    fUsedFileOrExtn: string;
    fLoadFileOrExtn: string;
    fLoadDefines: boolean;
    fDefines: TStringList;
    fWriteDefines: boolean;
    fDefinesDirectory: string;
    fUseOutput: boolean;
    fOutputDirectory: string;
    fOutputTimestamp: string;
    fUseInput: boolean;
    fInputDirectory: string;
    fParseMode: TD2XParseMode;
    fResultPer: TD2XResultPer;
    fFinalToken: boolean;
    fGlobalName: string;
    fLog: TStreamWriter;

    procedure AddDefine(pDef: string);
    procedure DeleteDefine(pDef: string);
    procedure LoadDefinesFile(pFile: string);

    function SetParseMode(pVal: string): boolean;
    function SetResultPer(pVal: string): boolean;
    function SetGlobalName(pVal: string): boolean;

    procedure Log(pFmt: string; pArgs: array of const);
    procedure LogOptionError(pLabel, pOpt: string);

  public
    property LogErrors: boolean read fLogErrors;
    property LogNotSupported: boolean read fLogNotSupported;
    property TimestampFiles: boolean read fTimestampFiles;
    property Verbose: boolean read fVerbose;
    property Recurse: boolean read fRecurse;
    property UseBase: boolean read fUseBase;
    property BaseDirectory: string read fBaseDirectory;
    // property UseInput: boolean read fUseInput;
    // property InputDirectory: string read fInputDirectory;
    // property UseOutput: boolean read fUseOutput;
    // property OutputDirectory: string read fOutputDirectory;
    property WriteDefines: boolean read fWriteDefines;
    property DefinesDirectory: string read fDefinesDirectory;
    property Xml: boolean read fXml;
    property XmlDirectory: string read fXmlDirectory;
    property DefinesUsed: boolean read fDefinesUsed;
    property UsedFileOrExtn: string read fUsedFileOrExtn;
    property LoadDefines: boolean read fLoadDefines;
    property LoadFileOrExtn: string read fLoadFileOrExtn;
    property Defines: TStringList read fDefines;
    property CountChildren: boolean read fCountChildren;
    property CountFileOrExtn: string read fCountFileOrExtn;
    property SkipMethods: boolean read fSkipMethods;
    property SkipFileOrExtn: string read fSkipFileOrExtn;
    property ParseMode: TD2XParseMode read fParseMode;
    property ResultPer: TD2XResultPer read fResultPer;
    property FinalToken: boolean read fFinalToken;
    property GlobalName: string read fGlobalName;
    property OutputTimestamp: string read fOutputTimestamp;

    constructor Create;
    destructor Destroy; override;

    function InputFileOrExtn(pFileOrExtn: string): string;
    function OutputFileOrExtn(pFileOrExtn: string): string;

    procedure SetLog(pDest: TStream);

    function ParseOption(pOpt: string): boolean;
    function ReportOptions: boolean;
    function ShowOptions: boolean;
  end;

implementation

uses
  System.Rtti,
  System.TypInfo;

type
  TD2XSetterFunc = function(pVal: string): boolean of object;

  { TD2XOptions }

procedure TD2XOptions.AddDefine(pDef: string);
begin
  fLoadDefines := True;
  if fDefines.IndexOf(pDef) < 0 then
    fDefines.Add(pDef);
end;

constructor TD2XOptions.Create;
begin
  inherited;

  fGlobalName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  fLogErrors := True;
  fLogNotSupported := False;
  fTimestampFiles := False;
  fVerbose := False;
  fUseBase := False;
  fBaseDirectory := '';
  fUseInput := True;
  fInputDirectory := 'Config\';
  fUseOutput := True;
  fOutputDirectory := 'Log\';
  fOutputTimestamp := FormatDateTime('-HH-mm', Now);
  fXml := True;
  fXmlDirectory := 'Xml\';
  fWriteDefines := False;
  fDefinesDirectory := 'Defines\';
  fDefinesUsed := True;
  fUsedFileOrExtn := '.used';
  fCountChildren := True;
  fCountFileOrExtn := '.cnt';
  fLoadDefines := True;
  fLoadFileOrExtn := '.def';
  fSkipMethods := True;
  fSkipFileOrExtn := fGlobalName + '.skip';
  fDefines := TStringList.Create;
  fDefines.Sorted := True;
  fParseMode := pmFull;
  fResultPer := rpFile;
  fFinalToken := True;
  fLog := nil;
end;

procedure TD2XOptions.DeleteDefine(pDef: string);
var
  lIdx: Integer;
begin
  lIdx := fDefines.IndexOf(pDef);
  if lIdx >= 0 then
  begin
    fDefines.Delete(lIdx);
    fLoadDefines := True;
  end;
end;

destructor TD2XOptions.Destroy;
begin
  FreeAndNil(fLog);
  FreeAndNil(fDefines);
  inherited;
end;

function TD2XOptions.InputFileOrExtn(pFileOrExtn: string): string;
  function GlobalFileOrExtn(pFileOrExtn: string): string;
  begin
    if StartsText('.', pFileOrExtn) then
      Result := ChangeFileExt(fGlobalName, pFileOrExtn)
    else
      Result := pFileOrExtn;
  end;

begin
  if fUseInput then
    Result := fInputDirectory + GlobalFileOrExtn(pFileOrExtn)
  else
    Result := GlobalFileOrExtn(pFileOrExtn);
end;

procedure TD2XOptions.LoadDefinesFile(pFile: string);
begin
  if pFile = '' then
    fDefines.Clear
  else
    fDefines.LoadFromFile(InputFileOrExtn(pFile));
end;

procedure TD2XOptions.Log(pFmt: string; pArgs: array of const);
begin
  if Assigned(fLog) then
    fLog.WriteLine(pFmt, pArgs)
end;

procedure TD2XOptions.LogOptionError(pLabel, pOpt: string);
begin
  Log('%s option: %s', [pLabel, pOpt]);
end;

function TD2XOptions.OutputFileOrExtn(pFileOrExtn: string): string;
  function GlobalFileOrExtn(pFileOrExtn: string): string;
  var
    lExtn: string;
  begin
    if StartsText('.', pFileOrExtn) then
      if fTimestampFiles then
        Result := ChangeFileExt(fGlobalName, fOutputTimestamp + pFileOrExtn)
      else
        Result := ChangeFileExt(fGlobalName, pFileOrExtn)
    else
      if fTimestampFiles then
      begin
        lExtn := ExtractFileExt(pFileOrExtn);
        Result := ChangeFileExt(pFileOrExtn, fOutputTimestamp + lExtn);
      end
      else
        Result := pFileOrExtn;
  end;

var
  lPath: string;

begin
  if fUseOutput then
    Result := fOutputDirectory + GlobalFileOrExtn(pFileOrExtn)
  else
    Result := GlobalFileOrExtn(pFileOrExtn);

  lPath := ExtractFilePath(ParamStr(0)) + ExtractFilePath(Result);
  ForceDirectories(lPath);
end;

function TD2XOptions.ParseOption(pOpt: string): boolean;
  function ErrorUnlessSet(out pFlag: boolean): boolean;
  begin
    Result := False;
    if (Length(pOpt) = 2) or (pOpt[3] = '+') then
      pFlag := True
    else
      if pOpt[3] = '-' then
        pFlag := False
      else
        Result := True;
  end;
  function ErrorUnlessValue(out pVal: string): boolean;
  begin
    Result := False;
    if (Length(pOpt) > 2) and (pOpt[3] = ':') then
      pVal := Copy(pOpt, 4, 99)
    else
      Result := True;
  end;
  function ErrorUnlessSetter(pFunc: TD2XSetterFunc): boolean;
  begin
    Result := False;
    if (Length(pOpt) > 2) and (pOpt[3] = ':') then
      Result := not pFunc(Copy(pOpt, 4, 99))
    else
      Result := True;
  end;
  function ErrorUnlessSetValue(out pFlag: boolean; out pVal: string): boolean;
  begin
    Result := False;
    if ErrorUnlessSet(pFlag) then
      if pOpt[3] = ':' then
      begin
        pFlag := True;
        pVal := Copy(pOpt, 4, 99)
      end
      else
        Result := True;
  end;
  function ErrorUnlessSetExtension(out pFlag: boolean; out pExtn: string;
    pDflt: string): boolean;
  begin
    Result := False;
    if ErrorUnlessSetValue(pFlag, pExtn) then
      Result := True
    else
      if pExtn = '' then
        pExtn := pDflt
      else
        if pExtn[1] <> '.' then
          pExtn := '.' + pExtn;
  end;
  function ErrorUnlessSetDir(out pFlag: boolean; var pDir: string): boolean;
  var
    lDir: string;
  begin
    Result := False;
    if ErrorUnlessSetValue(pFlag, lDir) then
      Result := True
    else
      if lDir > '' then
        pDir := IncludeTrailingPathDelimiter(lDir);
  end;

var
  lValue: string;
begin
  Result := False;
  if (Length(pOpt) < 2) or not CharInSet(pOpt[1], ['-', '/']) then
    raise ED2XOptionsException.Create('Invalid option: ' + pOpt)
  else
    case pOpt[2] of
      '?':
        Result := False;
      '!':
        begin
          ReportOptions;
          Result := True;
        end;
      'E', 'e':
        if ErrorUnlessSet(fLogErrors) then
          LogOptionError('Invalid Log Error messages', pOpt)
        else
          Result := True;
      'N', 'n':
        if ErrorUnlessSet(fLogNotSupported) then
          LogOptionError('Invalid Log Not Supported messages', pOpt)
        else
          Result := True;
      'T', 't':
        if ErrorUnlessSet(fTimestampFiles) then
          LogOptionError('Invalid Timestamp Files', pOpt)
        else
          Result := True;
      'V', 'v':
        if ErrorUnlessSet(fVerbose) then
          LogOptionError('Invalid Verbose', pOpt)
        else
          Result := True;
      'F', 'f':
        if ErrorUnlessSet(fFinalToken) then
          LogOptionError('Invalid Final token', pOpt)
        else
          Result := True;
      'R', 'r':
        if ErrorUnlessSet(fRecurse) then
          LogOptionError('Invalid Recurse Directories', pOpt)
        else
          Result := True;
      'M', 'm':
        if ErrorUnlessSetter(SetParseMode) then
          LogOptionError('Invalid Parse mode', pOpt)
        else
        begin

          Result := True;
        end;
      'P', 'p':
        if ErrorUnlessSetter(SetResultPer) then
          LogOptionError('Invalid Result per', pOpt)
        else
          Result := True;
      'G', 'g':
        if ErrorUnlessSetter(SetGlobalName) then
          LogOptionError('Invalid Global name', pOpt)
        else
          Result := True;
      'D', 'd':
        if ErrorUnlessValue(lValue) then
          LogOptionError('Invalid Define', pOpt)
        else
        begin
          AddDefine(lValue);
          Result := True;
        end;
      'Z', 'z':
        if ErrorUnlessValue(lValue) then
          LogOptionError('Invalid Undefine', pOpt)
        else
        begin
          DeleteDefine(lValue);
          Result := True;
        end;
      'L', 'l':
        if ErrorUnlessSetValue(fLoadDefines, fLoadFileOrExtn) then
          LogOptionError('Invalid Load Defines', pOpt)
        else
        begin
          LoadDefinesFile(fLoadFileOrExtn);
          Result := True;
        end;
      'W', 'w':
        if ErrorUnlessSetDir(fWriteDefines, fDefinesDirectory) then
          LogOptionError('Invalid Write Defines', pOpt)
        else
          Result := True;
      'B', 'b':
        if ErrorUnlessSetDir(fUseBase, fBaseDirectory) then
          LogOptionError('Invalid Use Base Directory', pOpt)
        else
          Result := True;
      'I', 'i':
        if ErrorUnlessSetDir(fUseInput, fInputDirectory) then
          LogOptionError('Invalid Use Input Directory', pOpt)
        else
          Result := True;
      'O', 'o':
        if ErrorUnlessSetDir(fUseOutput, fOutputDirectory) then
          LogOptionError('Invalid Use Output Directory', pOpt)
        else
          Result := True;
      'X', 'x':
        if ErrorUnlessSetDir(fXml, fXmlDirectory) then
          LogOptionError('Invalid Xml', pOpt)
        else
          Result := True;
      'U', 'u':
        if ErrorUnlessSetExtension(fDefinesUsed, fUsedFileOrExtn, '.used') then
          LogOptionError('Invalid Count Defines Used', pOpt)
        else
          Result := True;
      'C', 'c':
        if ErrorUnlessSetExtension(fCountChildren, fCountFileOrExtn, '.cnt') then
          LogOptionError('Invalid Count Children', pOpt)
        else
          Result := True;
      'S', 's':
        if ErrorUnlessSetValue(fSkipMethods, lValue) then
          LogOptionError('Invalid Load Skipped Methods', pOpt)
        else
        begin
          if ExtractFileExt(lValue) > '.' then
            fSkipFileOrExtn := lValue
          else
            fSkipFileOrExtn := lValue + '.skip';
          Result := True;
        end;
    else
      LogOptionError('Unknown', pOpt);
    end;
end;

function TD2XOptions.ReportOptions: boolean;
  function ShowEnabled(pOpt: boolean; pLabel, pVal: string): string;
  begin
    if pOpt then
      Result := 'Enabled  '
    else
      Result := 'Disabled ';

    if pLabel > '' then
      Result := Result + pLabel + pVal;
  end;

var
  lS: string;
  w: Integer;

  procedure WriteWidth(pStr: string);
  begin
    if Assigned(fLog) then
      fLog.Write(pStr);
    Inc(w, Length(pStr));
  end;

begin
  Result := True;

  Log('Current option settings:', []);
  Log('  Parse Mode              %s', [TD2X.ToLabel(fParseMode)]);
  Log('  Result per              %s', [TD2X.ToLabel(fResultPer)]);
  Log('  Global name             %s', [fGlobalName]);
  Log('  Errors                  %s', [ShowEnabled(fLogErrors, '', '')]);
  Log('  Not Supported           %s', [ShowEnabled(fLogNotSupported, '', '')]);
  Log('  Timestamp Files         %s', [ShowEnabled(fTimestampFiles, '', '')]);
  Log('  Verbose                 %s', [ShowEnabled(fVerbose, '', '')]);
  Log('  Recurse                 %s', [ShowEnabled(fRecurse, '', '')]);
  Log('  Show Final token        %s', [ShowEnabled(fFinalToken, '', '')]);
  Log('  Input base              %s', [ShowEnabled(fUseInput, 'Dir  ', fInputDirectory)]);
  Log('  Output base             %s', [ShowEnabled(fUseOutput, 'Dir  ', fOutputDirectory)]);
  Log('  Directory base          %s', [ShowEnabled(fUseBase, 'Dir  ', fBaseDirectory)]);
  Log('  Xml output              %s', [ShowEnabled(fXml, 'Dir  ', fXmlDirectory)]);
  Log('  Write defines           %s', [ShowEnabled(fWriteDefines, 'Dir  ',
        fDefinesDirectory)]);
  Log('  Count defines used      %s',
    [ShowEnabled(fDefinesUsed, 'File ', OutputFileOrExtn(fUsedFileOrExtn))]);
  Log('  Count min/max children  %s', [ShowEnabled(fCountChildren, 'File ',
        OutputFileOrExtn(fCountFileOrExtn))]);
  Log('  Skip methods in         %s',
    [ShowEnabled(fSkipMethods, 'File ', InputFileOrExtn(fSkipFileOrExtn))]);

  if fLoadDefines then
    if fDefines.Count < 1 then
      Log('Use NO Defines', [])
    else
    begin
      Log('Use these Defines:', []);
      w := 0;
      for lS in fDefines do
      begin
        if w = 0 then
          WriteWidth('    ')
        else
          if (w + Length(lS)) > 78 then
          begin
            Log('', []);
            w := 0;
            WriteWidth('    ');
          end
          else
            WriteWidth(', ');
        WriteWidth(lS);
      end;
      Log('', []);
    end
  else
    Log('Use default Defines', []);
end;

function TD2XOptions.SetGlobalName(pVal: string): boolean;
begin
  fGlobalName := pVal;
  fXmlDirectory := IncludeTrailingPathDelimiter(pVal);
  fDefinesDirectory := IncludeTrailingPathDelimiter(pVal);
  Result := True;
end;

procedure TD2XOptions.SetLog(pDest: TStream);
begin
  fLog := TStreamWriter.Create(pDest);
end;

function TD2XOptions.SetParseMode(pVal: string): boolean;
begin
  Result := True;
  case pVal[1] of
    'U', 'u':
      begin
        fParseMode := pmUses;
        SetGlobalName('Uses');
      end
  else
    fParseMode := pmFull;
  end;
end;

function TD2XOptions.SetResultPer(pVal: string): boolean;
begin
  Result := True;
  case pVal[1] of
    'R', 'r':
      fResultPer := rpRun;
    'P', 'p':
      fResultPer := rpParam;
    'W', 'w':
      fResultPer := rpWildcard;
    'S', 's':
      fResultPer := rpSubDir;
    'D', 'd':
      fResultPer := rpDir;
  else
    fResultPer := rpFile;
  end;
end;

function TD2XOptions.ShowOptions: boolean;
var
  lBase: string;
begin
  Result := True;
  lBase := ChangeFileExt(ExtractFileName(ParamStr(0)), '');

  Log('Usage: %s [ Option | @Params | mFilename | Wildcard ] ... ', [lBase]);
  Log('  Options:        %-15s Description', ['Default']);
  Log('    E[+-]         %-15s Log Error messages', ['-']);
  Log('    F[+-]         %-15s Record Final Token', ['+']);
  Log('    N[+-]         %-15s Log Not Supported messages', ['-']);
  Log('    T[+-]         %-15s Timestamp global output files', ['-']);
  Log('    V[+-]         %-15s Log all Parser methods called', ['-']);
  Log('    R[+-]         %-15s Recurse into subdirectories', ['+']);
  Log('    D:<define>    %-15s Define <define> (also enables "Load Defines")', ['']);
  Log('    G:<global>    %-15s Sets global name', [lBase]);
  Log('    Z:<define>    %-15s Undefine <define> (also enables "Load Defines")', ['']);
  Log('    M:<mode>      %-15s Set Parsing mode (F[ull], U[ses])', ['']);
  Log('    P:<Per>       %-15s Set Result per (F[ile], [S]ubdir, D[ir], W[ildcard], P[aram], R[un])',
    ['']);
  Log('    L[+-]|:<f/e>  %-15s Load Defines from <f/e> (no <f/e> clears all defines)',
    [':.def']);
  Log('    W[+-]|:<dir>  %-15s Generate Final Defines files into current or given <dir>', ['-']
    );
  Log('    B[+-]|:<dir>  %-15s Use <dir> as a base for all file lookups', ['-']);
  Log('    I[+-]|:<dir>  %-15s Use <dir> as a base for all file input', ['-']);
  Log('    O[+-]|:<dir>  %-15s Use <dir> as a base for all file output', ['-']);
  Log('    X[+-]|:<dir>  %-15s Generate XML files into current or given <dir>', ['+']);
  Log('    C[+-]|:<f/e>  %-15s Report Min/Max Children into <f/e>', ['+:.cnt']);
  Log('    U[+-]|:<f/e>  %-15s Report Defines Used into <f/e>', ['+:.used']);
  Log('    S[+-]|:<f/e>  %-15s Load Skipped Methods from <f/e>', ['+:.skip']);
  // Available option letters: AHJKQY
  Log('  Definitions:', []);
  Log('    <f/e> If value begins with "." is appended to global name to give file name', []);
end;

{ TD2X }

class function TD2X.ToLabel<T>(pVal: T): string;
var
  lV: TValue;
begin
  lV := TValue.From<T>(pVal);
  Result := Copy(lV.ToString, 3, 99);
end;

end.
