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

  TD2XElapsedMode = (emNone, emQuiet, emTotal, emProcessing);

  TD2XResultPer = (rpFile, rpWildcard, rpSubDir, rpDir, rpParam, rpRun);

  TD2XOptions = class(TD2XLogger)
  private
    fVerbose: TD2XBooleanParam;
    fRecurse: TD2XBooleanParam;
    fLogErrors: TD2XBooleanParam;
    fLogNotSupported: TD2XBooleanParam;
    fTimestampFiles: TD2XBooleanParam;
    fFinalToken: TD2XBooleanParam;
    fGlobalName: TD2XStringParam;
    fParseMode: TD2XSingleParam<TD2XParseMode>;
    fResultPer: TD2XSingleParam<TD2XResultPer>;
    fElapsedMode: TD2XSingleParam<TD2XElapsedMode>;
    fWriteXml: TD2XFlaggedStringParam;
    fWriteDefines: TD2XFlaggedStringParam;
    fUseBase: TD2XFlaggedStringParam;
    fUseOutput: TD2XFlaggedStringParam;
    fUseInput: TD2XFlaggedStringParam;
    fCountChildren: TD2XFlaggedStringParam;
    fSkipMethods: TD2XFlaggedStringParam;
    fDefinesUsed: TD2XFlaggedStringParam;

    fLoadDefines: boolean;
    fOutputTimestamp: string;

    fDefines: TStringList;

    fParams: TD2XParams;

    function ConvertGlobalName(pStr: string; pDflt: string; out pVal: string): boolean;
    function ValidateGlobalName(pVal: string): boolean;

    function ConvertDir(pStr: string; pDflt: string; out pVal: string): boolean;
    function ConvertFile(pStr: string; pDflt: string; out pVal: string): boolean;
    function ConvertExtn(pStr: string; pDflt: string; out pVal: string): boolean;

    function ConvertParsingMode(pStr: string; pDflt: TD2XParseMode;
      out pVal: TD2XParseMode): boolean;
    function FormatParsingMode(pVal: TD2XParseMode): string;

    function ConvertResultPer(pStr: string; pDflt: TD2XResultPer;
      out pVal: TD2XResultPer): boolean;
    function FormatResultPer(pVal: TD2XResultPer): string;

    function ConvertElapsedMode(pStr: string; pDflt: TD2XElapsedMode;
      out pVal: TD2XElapsedMode): boolean;
    function FormatElapsedMode(pVal: TD2XElapsedMode): string;

    function ParseReportOptions(pStr: string): boolean;
    function ParseResetOptions(pStr: string): boolean;
    function ParseShowOptions(pStr: string): boolean;

    function ParseDefines(pStr: string): boolean;
    procedure ResetDefines;

    procedure LogOptionError(pLabel, pOpt: string);
    function GetVerbose: boolean;
    function GetLogErrors: boolean;
    function GetLogNotSupported: boolean;
    function GetRecurse: boolean;
    function GetTimestampFiles: boolean;
    function GetFinalToken: boolean;
    function GetGlobalName: string;
    function GetParseMode: TD2XParseMode;
    function GetResultPer: TD2XResultPer;
    function GetXmlDirectory: string;
    function GetXmlFlag: boolean;
    function GetBaseDirectory: string;
    function GetDefinesDirectory: string;
    function GetUseBase: boolean;
    function GetWriteDefines: boolean;
    function GetCountChildren: boolean;
    function GetCountFileOrExtn: string;
    function GetDefinesUsed: boolean;
    function GetSkipFileOrExtn: string;
    function GetSkipMethods: boolean;
    function GetDefinesUsedFileOrExtn: string;
    function GetElapsedMode: TD2XElapsedMode;

  public
    property LogErrors: boolean read GetLogErrors;
    property LogNotSupported: boolean read GetLogNotSupported;
    property TimestampFiles: boolean read GetTimestampFiles;
    property Verbose: boolean read GetVerbose;
    property Recurse: boolean read GetRecurse;
    property FinalToken: boolean read GetFinalToken;
    property GlobalName: string read GetGlobalName;

    property ParseMode: TD2XParseMode read GetParseMode;
    property ResultPer: TD2XResultPer read GetResultPer;
    property ElapsedMode: TD2XElapsedMode read GetElapsedMode;

    property UseBase: boolean read GetUseBase;
    property BaseDirectory: string read GetBaseDirectory;
    // property UseInput: boolean read fUseInput;
    // property InputDirectory: string read fInputDirectory;
    // property UseOutput: boolean read fUseOutput;
    // property OutputDirectory: string read fOutputDirectory;
    property WriteDefines: boolean read GetWriteDefines;
    property DefinesDirectory: string read GetDefinesDirectory;
    property WriteXml: boolean read GetXmlFlag;
    property XmlDirectory: string read GetXmlDirectory;
    property CountChildren: boolean read GetCountChildren;
    property CountChildrenFoE: string read GetCountFileOrExtn;
    property SkipMethods: boolean read GetSkipMethods;
    property SkipMethodsFoE: string read GetSkipFileOrExtn;
    property DefinesUsed: boolean read GetDefinesUsed;
    property DefinesUsedFoE: string read GetDefinesUsedFileOrExtn;

    property LoadDefines: boolean read fLoadDefines;
    property Defines: TStringList read fDefines;
    property OutputTimestamp: string read fOutputTimestamp;

    constructor Create; override;
    destructor Destroy; override;

    function InputFileOrExtn(pFileOrExtn: string): string;
    function OutputFileOrExtn(pFileOrExtn: string): string;

    function ParseOption(pOpt: string): boolean;
  end;

implementation

uses
  System.Rtti,
  System.TypInfo;

type
  TD2XSetterFunc = function(pVal: string): boolean of object;

  { TD2XOptions }

function TD2XOptions.ConvertDir(pStr: string; pDflt: string; out pVal: string): boolean;
begin
  Result := True;
  if pStr > '' then
    pVal := IncludeTrailingPathDelimiter(pStr)
  else
    pVal := '';
end;

function TD2XOptions.ConvertElapsedMode(pStr: string; pDflt: TD2XElapsedMode;
  out pVal: TD2XElapsedMode): boolean;
begin
  Result := pStr > '';
  if Result then
    case pStr[1] of
      'N', 'n':
        pVal := emNone;
      'Q', 'q':
        pVal := emQuiet;
      'T', 't':
        pVal := emTotal;
      'P', 'p':
        pVal := emProcessing;
      '!':
        pVal := pDflt;
    else
      pVal := emQuiet;
    end;
end;

function TD2XOptions.ConvertExtn(pStr, pDflt: string; out pVal: string): boolean;
begin
  Result := True;
  if pStr = '' then
    pVal := pDflt
  else
    if ExtractFileExt(pStr) > '' then
      pVal := pStr
    else
      pVal := '.' + pStr;
end;

function TD2XOptions.ConvertFile(pStr, pDflt: string; out pVal: string): boolean;
begin
  Result := True;
  if pStr = '' then
    pVal := pDflt
  else
    if ExtractFileExt(pStr) > '' then
      pVal := pStr
    else
      pVal := pStr + pDflt;
end;

function TD2XOptions.ConvertGlobalName(pStr: string; pDflt: string; out pVal: string): boolean;
begin
  Result := True;
  if pStr = '' then
    pVal := ChangeFileExt(ExtractFileName(ParamStr(0)), '')
  else
    pVal := pStr;
end;

function TD2XOptions.ConvertParsingMode(pStr: string; pDflt: TD2XParseMode;
  out pVal: TD2XParseMode): boolean;
begin
  Result := pStr > '';
  if Result then
    case pStr[1] of
      'U', 'u':
        begin
          pVal := pmUses;
          fGlobalName.Value := 'Uses';
        end;
      '!', 'D', 'd':
        pVal := pDflt;
    else
      pVal := pmFull;
    end;
end;

function TD2XOptions.ConvertResultPer(pStr: string; pDflt: TD2XResultPer;
  out pVal: TD2XResultPer): boolean;
begin
  Result := pStr > '';
  if Result then
    case pStr[1] of
      'R', 'r':
        pVal := rpRun;
      'P', 'p':
        pVal := rpParam;
      'W', 'w':
        pVal := rpWildcard;
      'S', 's':
        pVal := rpSubDir;
      'D', 'd':
        pVal := rpDir;
      '!':
        pVal := pDflt;
    else
      pVal := rpFile;
    end;
end;

constructor TD2XOptions.Create;
begin
  inherited;

  fParams := TD2XParams.Create;
  fParams.L.JoinLog(Self);

  fParams.Add(TD2XParam.Create('?', 'Options', '', 'Show valid options', ParseShowOptions));
  fParams.Add(TD2XParam.Create('!', 'Reset', '', 'Reset all options to defaults',
      ParseResetOptions));
  fParams.Add(TD2XParam.Create('@', 'Report', '<file>', 'Report/Output Current options',
      ParseReportOptions));
  fVerbose := TD2XBooleanParam.CreateBool('V', 'Verbose', 'Log all Parser methods called');
  fParams.Add(fVerbose);
  fLogErrors := TD2XBooleanParam.CreateBool('L', 'Log Errors', 'Log Error messages', True);
  fParams.Add(fLogErrors);
  fLogNotSupported := TD2XBooleanParam.CreateBool('N', 'Log Not Supp',
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
  fParseMode := TD2XSingleParam<TD2XParseMode>.CreateParam('M', 'Parse mode', '<mode>',
    'Set Parsing mode (F[ull], U[ses])', pmFull, ConvertParsingMode, FormatParsingMode, nil);
  fParams.Add(fParseMode);
  fResultPer := TD2XSingleParam<TD2XResultPer>.CreateParam('P', 'Results per', '<per>',
    'Set Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un])', rpFile,
    ConvertResultPer, FormatResultPer, nil);
  fParams.Add(fResultPer);
  fElapsedMode := TD2XSingleParam<TD2XElapsedMode>.CreateParam('E', 'Show elapsed', '<mode>',
    'Set Elapsed time display to be (N[one], Q[uiet], T[otal], P[rocessing])', emQuiet,
    ConvertElapsedMode, FormatElapsedMode, nil);
  fParams.Add(fElapsedMode);
  fUseBase := TD2XFlaggedStringParam.CreateFlagStr('B', 'Base dir', '<dir>',
    'Use <dir> as a base for all file lookups', '', False, ConvertDir, nil, nil);
  fParams.Add(fUseBase);
  fUseInput := TD2XFlaggedStringParam.CreateFlagStr('I', 'Input dir', '<dir>',
    'Use <dir> as a base for all file input', 'Config\', True, ConvertDir, nil, nil);
  fParams.Add(fUseInput);
  fUseOutput := TD2XFlaggedStringParam.CreateFlagStr('O', 'Output dir', '<dir>',
    'Use <dir> as a base for all file output', 'Log\', True, ConvertDir, nil, nil);
  fParams.Add(fUseOutput);
  fWriteXml := TD2XFlaggedStringParam.CreateFlagStr('X', 'Generate XML', '<dir>',
    'Generate XML files into current or given <dir>', 'Xml\', True, ConvertDir, nil, nil);
  fParams.Add(fWriteXml);
  fWriteDefines := TD2XFlaggedStringParam.CreateFlagStr('W', 'Write Defines', '<dir>',
    'Generate Final Defines files into current or given <dir>', 'Defines\', False, ConvertDir,
    nil, nil);
  fParams.Add(fWriteDefines);
  fDefinesUsed := TD2XFlaggedStringParam.CreateFlagStr('U', 'Defines Used', '<f/e>',
    'Report Defines Used into <f/e>', '.used', True, ConvertExtn, nil, nil);
  fParams.Add(fDefinesUsed);
  fCountChildren := TD2XFlaggedStringParam.CreateFlagStr('C', 'Count Children', '<f/e>',
    'Report Min/Max Children into <f/e>', '.cnt', True, ConvertExtn, nil, nil);
  fParams.Add(fCountChildren);
  fSkipMethods := TD2XFlaggedStringParam.CreateFlagStr('S', 'Skipped Methods', '<f/e>',
    'Load Skipped Methods from <f/e>', '.skip', True, ConvertFile, nil, nil);
  fParams.Add(fSkipMethods);
  fParams.Add(TD2XResettableParam.CreateReset('D', 'Defines', '[+-!:]<def>',
      'Add(+), Remove(-), Clear(!) or Load(:) Defines', ParseDefines, ResetDefines));

  // Available option letters: AHJKLQYZ

  fOutputTimestamp := FormatDateTime('-HH-mm', Now);

  fLoadDefines := True;
  fDefines := TStringList.Create;
  fDefines.Sorted := True;
end;

destructor TD2XOptions.Destroy;
begin
  FreeAndNil(fDefines);
  FreeAndNil(fParams);

  inherited;
end;

function TD2XOptions.FormatElapsedMode(pVal: TD2XElapsedMode): string;
begin
  Result := TD2X.ToLabel(pVal);
end;

function TD2XOptions.FormatParsingMode(pVal: TD2XParseMode): string;
begin
  Result := TD2X.ToLabel(pVal);
end;

function TD2XOptions.FormatResultPer(pVal: TD2XResultPer): string;
begin
  Result := TD2X.ToLabel(pVal);
end;

function TD2XOptions.GetBaseDirectory: string;
begin
  Result := fUseBase.Value
end;

function TD2XOptions.GetCountChildren: boolean;
begin
  Result := fCountChildren.Flag
end;

function TD2XOptions.GetCountFileOrExtn: string;
begin
  Result := fCountChildren.Value
end;

function TD2XOptions.GetDefinesDirectory: string;
begin
  Result := fWriteDefines.Value
end;

function TD2XOptions.GetDefinesUsed: boolean;
begin
  Result := fDefinesUsed.Flag
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

function TD2XOptions.GetParseMode: TD2XParseMode;
begin
  Result := fParseMode.Value;
end;

function TD2XOptions.GetRecurse: boolean;
begin
  Result := fRecurse.Value;
end;

function TD2XOptions.GetResultPer: TD2XResultPer;
begin
  Result := fResultPer.Value;
end;

function TD2XOptions.GetSkipFileOrExtn: string;
begin
  Result := fSkipMethods.Value
end;

function TD2XOptions.GetSkipMethods: boolean;
begin
  Result := fSkipMethods.Flag
end;

function TD2XOptions.GetTimestampFiles: boolean;
begin
  Result := fTimestampFiles.Value;
end;

function TD2XOptions.GetUseBase: boolean;
begin
  Result := fUseBase.Flag
end;

function TD2XOptions.GetDefinesUsedFileOrExtn: string;
begin
  Result := fDefinesUsed.Value
end;

function TD2XOptions.GetElapsedMode: TD2XElapsedMode;
begin
  Result := fElapsedMode.Value;
end;

function TD2XOptions.GetVerbose: boolean;
begin
  Result := fVerbose.Value;
end;

function TD2XOptions.GetWriteDefines: boolean;
begin
  Result := fWriteDefines.Flag;
end;

function TD2XOptions.GetXmlDirectory: string;
begin
  Result := fWriteXml.Value;
end;

function TD2XOptions.GetXmlFlag: boolean;
begin
  Result := fWriteXml.Flag;
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
  if fUseInput.Flag then
    Result := fUseInput.Value + GlobalFileOrExtn(pFileOrExtn)
  else
    Result := GlobalFileOrExtn(pFileOrExtn);
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
  if fUseOutput.Flag then
    Result := fUseOutput.Value + GlobalFileOrExtn(pFileOrExtn)
  else
    Result := GlobalFileOrExtn(pFileOrExtn);

  lPath := ExtractFilePath(ParamStr(0)) + ExtractFilePath(Result);
  ForceDirectories(lPath);
end;

function TD2XOptions.ParseDefines(pStr: string): boolean;
var
  lStr: string;
  lIdx: Integer;
begin
  Result := False;
  if (pStr = '!') or (pStr = ':') then
  begin
    Result := True;
    fDefines.Clear;
    fLoadDefines := pStr = ':';
  end
  else
    if Length(pStr) > 1 then
    begin
      lStr := Copy(pStr, 2, Length(pStr));
      case pStr[1] of
        '+':
          begin
            Result := True;
            fLoadDefines := True;
            if fDefines.IndexOf(lStr) < 0 then
              fDefines.Add(lStr);
          end;
        '-':
          begin
            Result := True;
            lIdx := fDefines.IndexOf(lStr);
            if lIdx >= 0 then
            begin
              fDefines.Delete(lIdx);
              fLoadDefines := True;
            end;
          end;
        ':':
          begin
            Result := True;
            fLoadDefines := True;
            ConvertFile(Copy(pStr, 2, Length(pStr)), '.def', lStr);
            fDefines.LoadFromFile(InputFileOrExtn(lStr));
          end;
      end;
    end;
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

var
  lPrm: TD2XParam;
begin
  Result := False;
  if (Length(pOpt) < 2) or not CharInSet(pOpt[1], ['-', '/']) then
    raise ED2XOptionsException.Create('Invalid option: ' + pOpt)
  else
  begin
    lPrm := fParams.ForCode(Copy(pOpt, 2, 1));
    if Assigned(lPrm) then
      if lPrm.Parse(Copy(pOpt, 2, Length(pOpt))) then
        Result := True
      else
        LogOptionError('Invalid ' + lPrm.ParamLabel, pOpt)
    else
      LogOptionError('Unknown', pOpt);
  end;
end;

function TD2XOptions.ParseReportOptions(pStr: string): boolean;
var
  lS: string;
  w: Integer;
  lSL: TStringList;
  lFile: string;
  lP: TD2XParam;

  procedure WriteWidth(pStr: string);
  begin
    Log('%s', [pStr], False);
    Inc(w, Length(pStr));
  end;

begin
  if pStr > '' then
  begin
    Result := True;
    ConvertFile(pStr, '.prm', lFile);
    lFile := OutputFileOrExtn(lFile);
    lSL := TStringList.Create;
    try
      for lP in fParams do
        if not lP.IsDefault then
          lSL.Add('-' + lP.ToString);
      if fLoadDefines then
      begin
        lSL.Add('-D:');
        for lS in fDefines do
          lSL.Add('-D+' + lS);
      end;
      if lSL.Count > 0 then
        lSL.SaveToFile(lFile);
    finally
      FreeAndNil(lSL);
    end;
  end
  else
  begin
    Result := True;

    Log('Current option settings:', []);
    fParams.ReportAll;

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
end;

function TD2XOptions.ParseResetOptions(pStr: string): boolean;
begin
  Result := True;
  fParams.ResetAll;
end;

function TD2XOptions.ParseShowOptions(pStr: string): boolean;
var
  lBase: string;
begin
  Result := True;
  lBase := ChangeFileExt(ExtractFileName(ParamStr(0)), '');

  Log('Usage: %s [ Option | @Params | mFilename | Wildcard ] ... ', [lBase]);
  Log('Options:        %-15s Description', ['Default']);
  fParams.DescribeAll;
  Log('  Definitions:', []);
  Log('    <f/e> If value begins with "." is appended to global name to give file name', []);
end;

procedure TD2XOptions.ResetDefines;
begin
  fLoadDefines := True;
  fDefines.Clear;
end;

function TD2XOptions.ValidateGlobalName(pVal: string): boolean;
begin
  if Assigned(fWriteXml) then
    fWriteXml.Value := IncludeTrailingPathDelimiter(pVal);
  if Assigned(fWriteDefines) then
    fWriteDefines.Value := IncludeTrailingPathDelimiter(pVal);
  Result := True;
end;

end.
