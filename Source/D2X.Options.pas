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
    fWriteXml: TD2XFlaggedStringParam;
    fWriteDefines: TD2XFlaggedStringParam;
    fDefinesUsed: TD2XFlaggedStringParam;

    fLoadDefines: Boolean;

    fDefines: TStringList;

    fFileOpts: TD2XFileOptions;

    procedure OutputDefines(pSL: TStringList);
    procedure ReportDefines;

    function ParseDefines(pStr: string): Boolean;

    function GetXmlDirectory: string;
    function GetWriteXml: Boolean;
    function GetDefinesDirectory: string;
    function GetWriteDefines: Boolean;
    function GetDefinesUsed: Boolean;
    function GetDefinesUsedFileOrExtn: string;
    function GetWriteXmlFlag: IParamFlag;
    function GetWriteDefinesFlag: IParamFlag;

  public
    property WriteDefines: Boolean read GetWriteDefines;
    property WriteDefinesFlag: IParamFlag read GetWriteDefinesFlag;
    property DefinesDirectory: string read GetDefinesDirectory;
    property WriteXml: Boolean read GetWriteXml;
    property WriteXmlFlag: IParamFlag read GetWriteXmlFlag;
    property XmlDirectory: string read GetXmlDirectory;
    property DefinesUsed: Boolean read GetDefinesUsed;
    property DefinesUsedFoE: string read GetDefinesUsedFileOrExtn;

    property LoadDefines: Boolean read fLoadDefines;
    property Defines: TStringList read fDefines;

    property FileOpts: TD2XFileOptions read fFileOpts;

    constructor Create; override;
    destructor Destroy; override;

    procedure RegisterOptionParams(pParams: TD2XParams; pFileOpts: TD2XFileOptions);
    procedure RegisterOtherParams(pParams: TD2XParams);
    procedure RegisterDefineParams(pParams: TD2XParams);
  end;

function ConvertDir(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertExtn(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertFile(pStr, pDflt: string; out pVal: string): Boolean;

implementation

uses
  System.Rtti,
  System.TypInfo;

type
  TD2XSetterFunc = reference to function(pVal: string): Boolean;

function ConvertDir(pStr, pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  if pStr > '' then
    pVal := IncludeTrailingPathDelimiter(pStr)
  else
    pVal := '';
end;

function ConvertExtn(pStr, pDflt: string; out pVal: string): Boolean;
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

function MakeFileName(pStr, pDflt: string): string;
begin
  if pStr = '' then
    Result := pDflt
  else
    if ExtractFileExt(pStr) > '' then
      Result := pStr
    else
      Result := pStr + pDflt;
end;

function ConvertFile(pStr, pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  pVal := MakeFileName(pStr, pDflt);
end;

{ TD2XOptions }

constructor TD2XOptions.Create;
begin
  inherited;

  fFileOpts := TD2XFileOptions.Create(
      function(pVal: string): Boolean
    begin
      if Assigned(fWriteXml) then
        fWriteXml.Value := IncludeTrailingPathDelimiter(pVal);
      if Assigned(fWriteDefines) then
        fWriteDefines.Value := IncludeTrailingPathDelimiter(pVal);
      Result := True;
    end);

  fWriteXml := TD2XFlaggedStringParam.CreateFlagStr('X', 'Generate XML', '<dir>',
    'Generate XML files into current or given <dir>', 'Xml\', True, ConvertDir, nil, nil);
  fWriteDefines := TD2XFlaggedStringParam.CreateFlagStr('W', 'Write Defines', '<dir>',
    'Generate Final Defines files into current or given <dir>', 'Defines\', False, ConvertDir,
    nil, nil);
  fDefinesUsed := TD2XFlaggedStringParam.CreateFlagStr('U', 'Defines Used', '<f/e>',
    'Report Defines Used into <f/e>', '.used', True, ConvertExtn, nil, nil);

  // Available option letters: AHJKLQYZ

  fLoadDefines := True;
  fDefines := TStringList.Create;
  fDefines.Sorted := True;
end;

destructor TD2XOptions.Destroy;
begin
  FreeAndNil(fDefines);

  inherited;
end;

function TD2XOptions.GetDefinesDirectory: string;
begin
  Result := fWriteDefines.Value
end;

function TD2XOptions.GetDefinesUsed: Boolean;
begin
  Result := IParamFlag(fDefinesUsed).Flag
end;

function TD2XOptions.GetDefinesUsedFileOrExtn: string;
begin
  Result := fDefinesUsed.Value
end;

function TD2XOptions.GetWriteDefines: Boolean;
begin
  Result := IParamFlag(fWriteDefines).Flag;
end;

function TD2XOptions.GetWriteDefinesFlag: IParamFlag;
begin
  Result := fWriteDefines;
end;

function TD2XOptions.GetXmlDirectory: string;
begin
  Result := fWriteXml.Value;
end;

function TD2XOptions.GetWriteXml: Boolean;
begin
  Result := IParamFlag(fWriteXml).Flag;
end;

function TD2XOptions.GetWriteXmlFlag: IParamFlag;
begin
  Result := fWriteXml;
end;

procedure TD2XOptions.OutputDefines(pSL: TStringList);
var
  lS: string;
begin
  if fLoadDefines then
  begin
    pSL.Add('-D:');
    for lS in fDefines do
      pSL.Add('-D+' + lS);
  end;
end;

function TD2XOptions.ParseDefines(pStr: string): Boolean;
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
            fDefines.LoadFromFile(fFileOpts.InputFileOrExtn(MakeFileName(Copy(pStr, 2,
                  Length(pStr)), '.def')));
          end;
      end;
    end;
end;

procedure TD2XOptions.RegisterDefineParams(pParams: TD2XParams);
begin
  pParams.Add(TD2XResettableParam.CreateReset('D', 'Defines', '[+-!:]<def>',
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
end;

procedure TD2XOptions.RegisterOptionParams(pParams: TD2XParams; pFileOpts: TD2XFileOptions);
begin
  pParams.Add(TD2XParam.Create('?', 'Options', '', 'Show valid options',
      function(pStr: string): Boolean
    begin
      Result := True;
      pParams.DescribeAll;
    end));
  pParams.Add(TD2XParam.Create('!', 'Reset', '', 'Reset all options to defaults',
    function(pStr: string): Boolean
    begin
      Result := True;
      if StartsText('!', pStr) then
        pParams.ZeroAll
      else
        pParams.ResetAll;
    end));
  pParams.Add(TD2XParam.Create('@', 'Report', '<file>', 'Report/Output Current options',
    function(pStr: string): Boolean
    var
      lSL: TStringList;
      lFile: string;
    begin
      Result := True;
      if pStr > '' then
      begin
        lFile := pFileOpts.OutputFileOrExtn(MakeFileName(pStr, '.prm'));
        lSL := TStringList.Create;
        try
          pParams.OutputAll(lSL);
          OutputDefines(lSL);
          if lSL.Count > 0 then
            lSL.SaveToFile(lFile);
        finally
          FreeAndNil(lSL);
        end;
      end
      else
      begin
        pParams.ReportAll;
        ReportDefines;
      end;
    end));
end;

procedure TD2XOptions.RegisterOtherParams(pParams: TD2XParams);
begin
  pParams.Add(fWriteXml);
  pParams.Add(fWriteDefines);
  pParams.Add(fDefinesUsed);
end;

procedure TD2XOptions.ReportDefines;
var
  lS: string;
  w: Integer;

  procedure WriteWidth(pStr: string);
  begin
    Log('%s', [pStr], False);
    Inc(w, Length(pStr));
  end;

begin
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

{ TD2XFileOptions }

constructor TD2XFileOptions.Create(pGlobalValidator: TD2XSingleParam<string>.TspValidator);
begin
  inherited Create;

  fUseInput := TD2XFlaggedStringParam.CreateFlagStr('I', 'Input dir', '<dir>',
    'Use <dir> as a base for all file input', 'Config\', True, ConvertDir, nil, nil);
  fUseOutput := TD2XFlaggedStringParam.CreateFlagStr('O', 'Output dir', '<dir>',
    'Use <dir> as a base for all file output', 'Log\', True, ConvertDir, nil, nil);
  fGlobalName := TD2XStringParam.CreateStr('G', 'Global name', '<str>', 'Sets global name',
    ChangeFileExt(ExtractFileName(ParamStr(0)), ''),
    function(pStr: string; pDflt: string; out pVal: string): Boolean
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
