unit D2X.Options;

interface

uses
  D2X,
  D2X.Param,
  System.SysUtils,
  System.StrUtils,
  System.Classes;

type
  ED2XOptionsException = class(Exception);

  TD2XParseMode = (pmFull, pmUses);

  TD2XElapsedMode = (emNone, emQuiet, emTotal, emProcessing);

  TD2XResultPer = (rpFile, rpWildcard, rpSubDir, rpDir, rpParam, rpRun);

  TD2XFileOptions = class
  public
    constructor Create(pGlobalValidator: TD2XSingleParam<string>.TspValidator);

    function InputFileOrExtn(pFileOrExtn: string): string;
    function OutputFileOrExtn(pFileOrExtn: string): string;

    procedure RegisterParams(pParams: TD2XParams);

  private
    fUseOutput: TD2XFlaggedStringParam;
    fUseInput: TD2XFlaggedStringParam;
    fGlobalName: TD2XStringParam;
    fTimestampFiles: TD2XBooleanParam;
    fOutputTimestamp: string;

    function ConvertDir(pStr: string; pDflt: string; out pVal: string): boolean;
    function GetGlobalName: string;
    function GetTimestampFiles: Boolean;
    procedure SetGlobalName(const Value: string);

  public
    property OutputTimestamp: string read fOutputTimestamp;
    property GlobalName: string read GetGlobalName write SetGlobalName;
    property TimestampFiles: Boolean read GetTimestampFiles;
  end;

  TD2XOptions = class(TD2XLogger)
  private
    fVerbose: TD2XBooleanParam;
    fRecurse: TD2XBooleanParam;
    fLogErrors: TD2XBooleanParam;
    fLogNotSupported: TD2XBooleanParam;
    fFinalToken: TD2XBooleanParam;
    fParseMode: TD2XSingleParam<TD2XParseMode>;
    fResultPer: TD2XSingleParam<TD2XResultPer>;
    fElapsedMode: TD2XSingleParam<TD2XElapsedMode>;
    fWriteXml: TD2XFlaggedStringParam;
    fWriteDefines: TD2XFlaggedStringParam;
    fUseBase: TD2XFlaggedStringParam;
    fCountChildren: TD2XFlaggedStringParam;
    fSkipMethods: TD2XFlaggedStringParam;
    fDefinesUsed: TD2XFlaggedStringParam;

    fLoadDefines: boolean;

    fDefines: TStringList;

    fParams: TD2XParams;

    fFileOpts: TD2XFileOptions;

    function ConvertDir(pStr: string; pDflt: string; out pVal: string): boolean;
    function ConvertFile(pStr: string; pDflt: string; out pVal: string): boolean;
    function ConvertExtn(pStr: string; pDflt: string; out pVal: string): boolean;

    function ConvertParsingMode(pStr: string; pDflt: TD2XParseMode;
      out pVal: TD2XParseMode): boolean;

    function ConvertResultPer(pStr: string; pDflt: TD2XResultPer;
      out pVal: TD2XResultPer): boolean;

    function ConvertElapsedMode(pStr: string; pDflt: TD2XElapsedMode;
      out pVal: TD2XElapsedMode): boolean;

    function ParseReportOptions(pStr: string): boolean;
    function ParseShowOptions(pStr: string): boolean;

    function ParseDefines(pStr: string): boolean;

    procedure LogOptionError(pLabel, pOpt: string);

    function GetVerbose: boolean;
    function GetLogErrors: boolean;
    function GetLogNotSupported: boolean;
    function GetRecurse: boolean;
    function GetFinalToken: boolean;
    function GetParseMode: TD2XParseMode;
    function GetResultPer: TD2XResultPer;
    function GetXmlDirectory: string;
    function GetWriteXml: boolean;
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
    function GetVerboseFlag: IParamFlag;
    function GetSkipMethodsFlag: IParamFlag;
    function GetCountChildrenFlag: IParamFlag;
    function GetWriteXmlFlag: IParamFlag;

  public
    property LogErrors: boolean read GetLogErrors;
    property LogNotSupported: boolean read GetLogNotSupported;
    property Verbose: boolean read GetVerbose;
    property VerboseFlag: IParamFlag read GetVerboseFlag;
    property Recurse: boolean read GetRecurse;
    property FinalToken: boolean read GetFinalToken;

    property ParseMode: TD2XParseMode read GetParseMode;
    property ResultPer: TD2XResultPer read GetResultPer;
    property ElapsedMode: TD2XElapsedMode read GetElapsedMode;

    property UseBase: boolean read GetUseBase;
    property BaseDirectory: string read GetBaseDirectory;
    property WriteDefines: boolean read GetWriteDefines;
    property DefinesDirectory: string read GetDefinesDirectory;
    property WriteXml: boolean read GetWriteXml;
    property WriteXmlFlag: IParamFlag read GetWriteXmlFlag;
    property XmlDirectory: string read GetXmlDirectory;
    property CountChildren: boolean read GetCountChildren;
    property CountChildrenFlag: IParamFlag read GetCountChildrenFlag;
    property CountChildrenFoE: string read GetCountFileOrExtn;
    property SkipMethods: boolean read GetSkipMethods;
    property SkipMethodsFlag: IParamFlag read GetSkipMethodsFlag;
    property SkipMethodsFoE: string read GetSkipFileOrExtn;
    property DefinesUsed: boolean read GetDefinesUsed;
    property DefinesUsedFoE: string read GetDefinesUsedFileOrExtn;

    property LoadDefines: boolean read fLoadDefines;
    property Defines: TStringList read fDefines;

    property FileOpts: TD2XFileOptions read fFileOpts;

    constructor Create; override;
    destructor Destroy; override;

    function ParseOption(pOpt: string): boolean;
  end;

implementation

uses
  System.Rtti,
  System.TypInfo;

type
  TD2XSetterFunc = reference to function(pVal: string): boolean;

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

function TD2XOptions.ConvertParsingMode(pStr: string; pDflt: TD2XParseMode;
  out pVal: TD2XParseMode): boolean;
begin
  Result := pStr > '';
  if Result then
    case pStr[1] of
      'U', 'u':
        begin
          pVal := pmUses;
          fFileOpts.GlobalName := 'Uses';
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
        function(pStr: string): boolean
    begin
      Result := True;
      if StartsText('!', pStr) then
        fParams.ZeroAll
      else
        fParams.ResetAll;
    end));
  fParams.Add(TD2XParam.Create('@', 'Report', '<file>', 'Report/Output Current options',
    ParseReportOptions));

  fVerbose := TD2XBooleanParam.CreateBool('V', 'Verbose', 'Log all Parser methods called');
  fParams.Add(fVerbose);

  fLogErrors := TD2XBooleanParam.CreateBool('L', 'Log Errors', 'Log Error messages', True);
  fParams.Add(fLogErrors);

  fLogNotSupported := TD2XBooleanParam.CreateBool('N', 'Log Not Supp',
    'Log Not Supported messages');
  fParams.Add(fLogNotSupported);

  fFinalToken := TD2XBooleanParam.CreateBool('F', 'Final Token', 'Record Final Token', True);
  fParams.Add(fFinalToken);

  fRecurse := TD2XBooleanParam.CreateBool('R', 'Recurse', 'Recurse into subdirectories');
  fParams.Add(fRecurse);

  fFileOpts := TD2XFileOptions.Create(
    function(pVal: string): boolean
    begin
      if Assigned(fWriteXml) then
        fWriteXml.Value := IncludeTrailingPathDelimiter(pVal);
      if Assigned(fWriteDefines) then
        fWriteDefines.Value := IncludeTrailingPathDelimiter(pVal);
      Result := True;
    end);
  fFileOpts.RegisterParams(fParams);

  fParseMode := TD2XSingleParam<TD2XParseMode>.CreateParam('M', 'Parse mode', '<mode>',
    'Set Parsing mode (F[ull], U[ses])', pmFull, ConvertParsingMode,
    TD2X.ToLabel<TD2XParseMode>, nil);
  fParams.Add(fParseMode);
  fResultPer := TD2XSingleParam<TD2XResultPer>.CreateParam('P', 'Results per', '<per>',
    'Set Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un])', rpFile,
    ConvertResultPer, TD2X.ToLabel<TD2XResultPer>, nil);
  fParams.Add(fResultPer);
  fElapsedMode := TD2XSingleParam<TD2XElapsedMode>.CreateParam('E', 'Show elapsed', '<mode>',
    'Set Elapsed time display to be (N[one], Q[uiet], T[otal], P[rocessing])', emQuiet,
    ConvertElapsedMode, TD2X.ToLabel<TD2XElapsedMode>, nil);
  fParams.Add(fElapsedMode);

  fUseBase := TD2XFlaggedStringParam.CreateFlagStr('B', 'Base dir', '<dir>',
    'Use <dir> as a base for all file lookups', '', False, ConvertDir, nil, nil);
  fParams.Add(fUseBase);

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
    'Add(+), Remove(-), Clear(!) or Load(:) Defines', ParseDefines,
      procedure
    begin
      fLoadDefines := False;
      fDefines.Clear;
    end,
    procedure
    begin
      fLoadDefines := True;
      fDefines.Clear;
    end));

  // Available option letters: AHJKLQYZ

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

function TD2XOptions.GetBaseDirectory: string;
begin
  Result := fUseBase.Value
end;

function TD2XOptions.GetCountChildren: boolean;
begin
  Result := IParamFlag(fCountChildren).Flag;
end;

function TD2XOptions.GetCountChildrenFlag: IParamFlag;
begin
  Result := fCountChildren;
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
  Result := IParamFlag(fDefinesUsed).Flag
end;

function TD2XOptions.GetFinalToken: boolean;
begin
  Result := fFinalToken.Value;
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
  Result := IParamFlag(fSkipMethods).Flag
end;

function TD2XOptions.GetSkipMethodsFlag: IParamFlag;
begin
  Result := fSkipMethods;
end;

function TD2XOptions.GetUseBase: boolean;
begin
  Result := IParamFlag(fUseBase).Flag
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

function TD2XOptions.GetVerboseFlag: IParamFlag;
begin
  Result := fVerbose;
end;

function TD2XOptions.GetWriteDefines: boolean;
begin
  Result := IParamFlag(fWriteDefines).Flag;
end;

function TD2XOptions.GetXmlDirectory: string;
begin
  Result := fWriteXml.Value;
end;

function TD2XOptions.GetWriteXml: boolean;
begin
  Result := IParamFlag(fWriteXml).Flag;
end;

function TD2XOptions.GetWriteXmlFlag: IParamFlag;
begin
  Result := fWriteXml;
end;

procedure TD2XOptions.LogOptionError(pLabel, pOpt: string);
begin
  Log('%s option: %s', [pLabel, pOpt]);
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
            fDefines.LoadFromFile(fFileOpts.InputFileOrExtn(lStr));
          end;
      end;
    end;
end;

function TD2XOptions.ParseOption(pOpt: string): boolean;
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
    lFile := fFileOpts.OutputFileOrExtn(lFile);
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

{ TD2XFileOptions }

function TD2XFileOptions.ConvertDir(pStr, pDflt: string;
out pVal: string): boolean;
begin
  Result := True;
  if pStr > '' then
    pVal := IncludeTrailingPathDelimiter(pStr)
  else
    pVal := '';
end;

constructor TD2XFileOptions.Create(pGlobalValidator: TD2XSingleParam<string>.TspValidator);
begin
  inherited Create;

  fUseInput := TD2XFlaggedStringParam.CreateFlagStr('I', 'Input dir', '<dir>',
    'Use <dir> as a base for all file input', 'Config\', True, ConvertDir, nil, nil);
  fUseOutput := TD2XFlaggedStringParam.CreateFlagStr('O', 'Output dir', '<dir>',
    'Use <dir> as a base for all file output', 'Log\', True, ConvertDir, nil, nil);
  fGlobalName := TD2XStringParam.CreateStr('G', 'Global name', '<str>', 'Sets global name',
    ChangeFileExt(ExtractFileName(ParamStr(0)), ''),
    function(pStr: string; pDflt: string; out pVal: string): boolean
    begin
      Result := True;
      if pStr = '' then
        pVal := ChangeFileExt(ExtractFileName(ParamStr(0)), '')
      else
        pVal := pStr;
    end, pGlobalValidator);
  fTimestampFiles := TD2XBooleanParam.CreateBool('T', 'Timestamp',
    'Timestamp global output files');

  fOutputTimestamp := FormatDateTime('-HH-mm', Now);
end;

function TD2XFileOptions.GetGlobalName: string;
begin
  Result := fGlobalName.Value;
end;

function TD2XFileOptions.GetTimestampFiles: Boolean;
begin
  Result := fTimestampFiles.Value;
end;

function TD2XFileOptions.InputFileOrExtn(pFileOrExtn: string): string;
  function GlobalFileOrExtn(pFileOrExtn: string): string;
  begin
    if StartsText('.', pFileOrExtn) then
      Result := ChangeFileExt(fGlobalName.Value, pFileOrExtn)
    else
      Result := pFileOrExtn;
  end;

begin
  if IParamFlag(fUseInput).Flag then
    Result := fUseInput.Value + GlobalFileOrExtn(pFileOrExtn)
  else
    Result := GlobalFileOrExtn(pFileOrExtn);
end;

function TD2XFileOptions.OutputFileOrExtn(pFileOrExtn: string): string;
  function GlobalFileOrExtn(pFileOrExtn: string): string;
  var
    lExtn: string;
  begin
    if StartsText('.', pFileOrExtn) then
      if fTimestampFiles.Value then
        Result := ChangeFileExt(fGlobalName.Value, fOutputTimestamp + pFileOrExtn)
      else
        Result := ChangeFileExt(fGlobalName.Value, pFileOrExtn)
    else
      if fTimestampFiles.Value then
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
  if IParamFlag(fUseOutput).Flag then
    Result := fUseOutput.Value + GlobalFileOrExtn(pFileOrExtn)
  else
    Result := GlobalFileOrExtn(pFileOrExtn);

  lPath := ExtractFilePath(ParamStr(0)) + ExtractFilePath(Result);
  ForceDirectories(lPath);
end;

procedure TD2XFileOptions.RegisterParams(pParams: TD2XParams);
begin
  pParams.Add(fTimestampFiles);
  pParams.Add(fGlobalName);
  pParams.Add(fUseInput);
  pParams.Add(fUseOutput);
end;

procedure TD2XFileOptions.SetGlobalName(const Value: string);
begin
  fGlobalName.Value := Value;
end;

end.
