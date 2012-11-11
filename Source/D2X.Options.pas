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

    function ForcePath(pFilename: string): String;
  end;

  TD2XOptions = class(TD2XLogger)
  private
    fWriteXml: TD2XFlaggedStringParam;
    fWriteDefines: TD2XFlaggedStringParam;

    fFileOpts: TD2XFileOptions;

    function GetXmlDirectory: string;
    function GetWriteXml: Boolean;
    function GetDefinesDirectory: string;
    function GetWriteDefines: Boolean;
    function GetWriteXmlFlag: IParamFlag;
    function GetWriteDefinesFlag: IParamFlag;

  public
    property WriteDefines: Boolean read GetWriteDefines;
    property WriteDefinesFlag: IParamFlag read GetWriteDefinesFlag;
    property DefinesDirectory: string read GetDefinesDirectory;
    property WriteXml: Boolean read GetWriteXml;
    property WriteXmlFlag: IParamFlag read GetWriteXmlFlag;
    property XmlDirectory: string read GetXmlDirectory;

    property FileOpts: TD2XFileOptions read fFileOpts;

    constructor Create; override;
    destructor Destroy; override;

    procedure RegisterOtherParams(pParams: TD2XParams);
  end;

function ConvertDir(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertExtn(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertFile(pStr, pDflt: string; out pVal: string): Boolean;

function MakeFileName(pStr, pDflt: string): string;

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

  // Available option letters: AHJKLQYZ

end;

destructor TD2XOptions.Destroy;
begin

  inherited;
end;

function TD2XOptions.GetDefinesDirectory: string;
begin
  Result := fWriteDefines.Value
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

procedure TD2XOptions.RegisterOtherParams(pParams: TD2XParams);
begin
  pParams.Add(fWriteXml);
  pParams.Add(fWriteDefines);
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

function TD2XFileOptions.ForcePath(pFilename: string): String;
begin
  Result := pFilename;
  ForceDirectories(ExtractFilePath(ParamStr(0)) + ExtractFilePath(pFilename));
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

begin
  if IParamFlag(fUseOutput).Flag then
    Result := ForcePath(fUseOutput.Value + GlobalFileOrExtn(pFileOrExtn))
  else
    Result := ForcePath(GlobalFileOrExtn(pFileOrExtn));
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
