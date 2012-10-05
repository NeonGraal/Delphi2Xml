unit D2XOptions;

interface

uses
  D2X,
  D2XParam,
  System.SysUtils,
  System.StrUtils,
  System.Classes;

type
  ED2XOptionsException = class(Exception);

  TD2XParseMode = (pmFull, pmUses);

  TD2XResultPer = (rpFile, rpWildcard, rpSubDir, rpDir, rpParam, rpRun);

  TD2XOptions = class
  private
    fVerbose: TD2XBooleanParam;
    fRecurse: TD2XBooleanParam;
    fLogErrors: TD2XBooleanParam;
    fLogNotSupported: TD2XBooleanParam;
    fTimestampFiles: TD2XBooleanParam;
    fFinalToken: TD2XBooleanParam;
    fGlobalName: TD2XStringParam;

    fXml: boolean;
    fXmlDirectory: string;
    fCountChildren: boolean;
    fCountFileOrExtn: string;
    fSkipMethods: boolean;
    fSkipFileOrExtn: string;
    fUseBase: boolean;
    fBaseDirectory: string;
    fDefinesUsed: boolean;
    fUsedFileOrExtn: string;
    fLoadDefines: boolean;
    fLoadFileOrExtn: string;
    fWriteDefines: boolean;
    fDefinesDirectory: string;
    fUseOutput: boolean;
    fOutputDirectory: string;
    fOutputTimestamp: string;
    fUseInput: boolean;
    fInputDirectory: string;
    fParseMode: TD2XParseMode;
    fResultPer: TD2XResultPer;

    fDefines: TStringList;

    fLog: TStreamWriter;

    fParams: TD2XParams;

    procedure AddDefine(pDef: string);
    procedure DeleteDefine(pDef: string);
    procedure LoadDefinesFile(pFile: string);

    function SetParseMode(pVal: string): boolean;
    function SetResultPer(pVal: string): boolean;

    function ConvertGlobalName(pStr: string; out pVal: String): boolean;
    function ValidateGlobalName(pVal: string): boolean;

    procedure Log(pFmt: string; pArgs: array of const);
    procedure LogOptionError(pLabel, pOpt: string);
    function GetVerbose: boolean;
    function GetLogErrors: boolean;
    function GetLogNotSupported: boolean;
    function GetRecurse: boolean;
    function GetTimestampFiles: boolean;
    function GetFinalToken: boolean;
    function GetGlobalName: string;

  public
    property LogErrors: boolean read GetLogErrors;
    property LogNotSupported: boolean read GetLogNotSupported;
    property TimestampFiles: boolean read GetTimestampFiles;
    property Verbose: boolean read GetVerbose;
    property Recurse: boolean read GetRecurse;
    property FinalToken: boolean read GetFinalToken;
    property GlobalName: string read GetGlobalName;

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

function TD2XOptions.ConvertGlobalName(pStr: string; out pVal: String): boolean;
begin
  Result := True;
  if pStr = '' then
    pVal := ChangeFileExt(ExtractFileName(ParamStr(0)), '')
  else
    pVal := pStr;
end;

constructor TD2XOptions.Create;
begin
  inherited;

  fParams := TD2XParams.Create;
  fVerbose := TD2XBooleanParam.CreateBool('V', 'Verbose', 'Log all Parser methods called');
  fParams.Add(fVerbose);
  fLogErrors := TD2XBooleanParam.CreateBool('E', 'Log Errors', 'Log Error messages', True);
  fParams.Add(fLogErrors);
  fLogNotSupported := TD2XBooleanParam.CreateBool('N', 'Log Not Supported',
    'Log Not Supported messages');
  fParams.Add(fLogNotSupported);
  fTimestampFiles := TD2XBooleanParam.CreateBool('T', 'Timestamp',
    'Timestamp global output files');
  fParams.Add(fTimestampFiles);
  fFinalToken := TD2XBooleanParam.CreateBool('F', 'Final Token', 'Record Final Token', True);
  fParams.Add(fFinalToken);
  fRecurse := TD2XBooleanParam.CreateBool('R', 'Recurse', 'Recurse into subdirectories');
  fParams.Add(fRecurse);
  fGlobalName := TD2XStringParam.CreateStr('G', 'Global name', '<str>', 'Sets global name',
    ChangeFileExt(ExtractFileName(ParamStr(0)), ''), ConvertGlobalName, ValidateGlobalName);
  fParams.Add(fGlobalName);

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
  fSkipFileOrExtn := GlobalName + '.skip';
  fDefines := TStringList.Create;
  fDefines.Sorted := True;
  fParseMode := pmFull;
  fResultPer := rpFile;
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
  FreeAndNil(fParams);

  inherited;
end;

function TD2XOptions.GetFinalToken: boolean;
begin
  Result := fFinalToken.Value;
end;

function TD2XOptions.GetGlobalName: string;
begin
  Result := fGlobalName.Value;
end;

function TD2XOptions.GetLogErrors: boolean;
begin
  Result := fLogErrors.Value;
end;

function TD2XOptions.GetLogNotSupported: boolean;
begin
  Result := fLogNotSupported.Value;
end;

function TD2XOptions.GetRecurse: boolean;
begin
  Result := fRecurse.Value;
end;

function TD2XOptions.GetTimestampFiles: boolean;
begin
  Result := fTimestampFiles.Value;
end;

function TD2XOptions.GetVerbose: boolean;
begin
  Result := fVerbose.Value;
end;

function TD2XOptions.InputFileOrExtn(pFileOrExtn: string): string;
  function GlobalFileOrExtn(pFileOrExtn: string): string;
  begin
    if StartsText('.', pFileOrExtn) then
      Result := ChangeFileExt(GlobalName, pFileOrExtn)
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
      if TimestampFiles then
        Result := ChangeFileExt(GlobalName, fOutputTimestamp + pFileOrExtn)
      else
        Result := ChangeFileExt(GlobalName, pFileOrExtn)
    else
      if TimestampFiles then
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
  function ErrorUnlessSetDefault(out pFlag: boolean; out pVal: string; pDflt: string): boolean;
  begin
    Result := False;
    if ErrorUnlessSetValue(pFlag, pVal) then
      Result := True
    else
      if pVal = '' then
        pVal := pDflt;
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
        if not ContainsText(pExtn, '.') then
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
  lPrm: TD2XParam;
begin
  Result := False;
  if (Length(pOpt) < 2) or not CharInSet(pOpt[1], ['-', '/']) then
    raise ED2XOptionsException.Create('Invalid option: ' + pOpt)
  else
  begin
    lPrm := fParams.ForCode(Copy(pOpt, 2, 1));
    if Assigned(lPrm) then
      Result := lPrm.Parse(Copy(pOpt, 2, Length(pOpt)))
    else
      case pOpt[2] of
        '?':
          Result := False;
        '!':
          begin
            ReportOptions;
            Result := True;
          end;
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
  fParams.Log := fLog;
  fParams.ReportAll;
  Log('  Parse Mode              %s', [TD2X.ToLabel(fParseMode)]);
  Log('  Result per              %s', [TD2X.ToLabel(fResultPer)]);
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

function TD2XOptions.ValidateGlobalName(pVal: string): boolean;
begin
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
        fGlobalName.Value := 'Uses';
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
  fParams.Log := fLog;
  fParams.DescribeAll;
  Log('    D:<define>    %-15s Define <define> (also enables "Load Defines")', ['']);
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

end.
