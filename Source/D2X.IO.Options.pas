unit D2X.IO.Options;

interface

uses
  D2X,
  D2X.IO,
  D2X.Param,
  D2X.Params,
  System.Classes,
  System.SysUtils;

type
  TD2XFileOptions = class(TD2XInterfaced, ID2XIOFactory)
  public
    function ConfigFileOrExtn(pFileOrExtn: string): ID2XFile;
    function LogFileOrExtn(pFileOrExtn: string): ID2XFile;
    function BaseFile(pFileOrDir: string): ID2XFile;
    function BaseDir(pFileOrDir: string): ID2XDir;
    function SimpleFile(pFile: string): ID2XFile;

    procedure SetGlobalName(const pName: string);
    procedure SetGlobalValidator(pValidator: TD2XSingleParam<string>.TspValidator);
    procedure RegisterParams(pParams: TD2XParams);

  private
    fLogBase: TD2XFlaggedStringParam;
    fConfigBase: TD2XFlaggedStringParam;
    fInputBase: TD2XFlaggedStringParam;
    fGlobalName: TD2XStringParam;
    fTimestampFiles: TD2XBooleanParam;

    function GetGlobalName: string;

  protected
    fOutputTimestamp: string;

    function GetTimestampFiles: Boolean;

  public
    property GlobalName: string read GetGlobalName write SetGlobalName;
  end;

function ConvertDir(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertExtn(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertFile(pStr, pDflt: string; out pVal: string): Boolean;

implementation

uses
  D2X.IO.Actual,
  System.StrUtils;

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

function ConvertFile(pStr, pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  pVal := MakeFileName(pStr, pDflt);
end;

{ TD2XFileOptions }

function TD2XFileOptions.GetGlobalName: string;
begin
  Result := fGlobalName.Value;
end;

function TD2XFileOptions.GetTimestampFiles: Boolean;
begin
  Result := fTimestampFiles.Value;
end;

function TD2XFileOptions.BaseDir(pFileOrDir: string): ID2XDir;
begin
  if fInputBase.FlagValue then
    Result := TD2XDirPath.Create(fInputBase.Value + pFileOrDir)
  else
    Result := TD2XDirPath.Create(pFileOrDir);
end;

function TD2XFileOptions.BaseFile(pFileOrDir: string): ID2XFile;
begin
  if fInputBase.FlagValue then
    Result := TD2XFileStream.Create(fInputBase.Value + pFileOrDir)
  else
    Result := TD2XFileStream.Create(pFileOrDir);
end;

function TD2XFileOptions.ConfigFileOrExtn(pFileOrExtn: string): ID2XFile;
  function GlobalFileOrExtn(pFileOrExtn: string): string;
  begin
    if StartsText('.', pFileOrExtn) then
      Result := ChangeFileExt(fGlobalName.Value, pFileOrExtn)
    else
      Result := pFileOrExtn;
  end;

begin
  if ID2XFlag(fConfigBase).Flag then
    Result := TD2XFileStream.Create(fConfigBase.Value + GlobalFileOrExtn(pFileOrExtn))
  else
    Result := TD2XFileStream.Create(GlobalFileOrExtn(pFileOrExtn));
end;

function TD2XFileOptions.LogFileOrExtn(pFileOrExtn: string): ID2XFile;
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
  if ID2XFlag(fLogBase).Flag then
    Result := TD2XFileStream.Create(fLogBase.Value + GlobalFileOrExtn(pFileOrExtn))
  else
    Result := TD2XFileStream.Create(GlobalFileOrExtn(pFileOrExtn));
end;

procedure TD2XFileOptions.RegisterParams(pParams: TD2XParams);
begin
  pParams.Add(fTimestampFiles);
  pParams.Add(fGlobalName);
  pParams.Add(fConfigBase);
  pParams.Add(fLogBase);
  pParams.Add(fInputBase);
end;

procedure TD2XFileOptions.SetGlobalName(const pName: string);
begin
  fGlobalName.Value := pName;
end;

procedure TD2XFileOptions.SetGlobalValidator(pValidator: TD2XSingleParam<string>.TspValidator);
begin
  fConfigBase := TD2XFlaggedStringParam.CreateFlagStr('I', 'Config dir', '<dir>',
    'Use <dir> as a base for all Config files', 'Config\', True, ConvertDir, nil, nil);
  fLogBase := TD2XFlaggedStringParam.CreateFlagStr('O', 'Log dir', '<dir>',
    'Use <dir> as a base for all Log files', 'Log\', True, ConvertDir, nil, nil);
  fInputBase := TD2XFlaggedStringParam.CreateFlagStr('B', 'Base dir', '<dir>',
    'Use <dir> as a base for all Input files', '', False, ConvertDir, nil, nil);
  fGlobalName := TD2XStringParam.CreateStr('G', 'Global name', '<str>', 'Sets global name',
    ChangeFileExt(ExtractFileName(ParamStr(0)), ''),
      function(pStr: string; pDflt: string; out pVal: string): Boolean
    begin
      Result := True;
      if pStr = '' then
        pVal := ChangeFileExt(ExtractFileName(ParamStr(0)), '')
      else
        pVal := pStr;
    end, pValidator);
  fTimestampFiles := TD2XBooleanParam.CreateBool('T', 'Timestamp',
    'Timestamp global output files');

  fOutputTimestamp := FormatDateTime('-HH-mm', Now);
end;

function TD2XFileOptions.SimpleFile(pFile: string): ID2XFile;
begin
  Result := TD2XFileStream.Create(pFile);
end;

end.
