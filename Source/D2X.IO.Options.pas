unit D2X.IO.Options;

interface

uses
  D2X,
  D2X.IO,
  D2X.Param,
  D2X.Params,
  System.Classes,
  System.Diagnostics,
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
    procedure SetTimestampFlag(pFlag: ID2XFlag);
    procedure RegisterParams(pParams: TD2XParams);
    function GetNow: string;
    function GetDuration(pWatch: TStopwatch): Double;
    function GetInputStream: TStreamReader;

  private
    fLogBase: TD2XFlaggedStringParam;
    fConfigBase: TD2XFlaggedStringParam;
    fInputBase: TD2XFlaggedStringParam;
    fGlobalName: TD2XStringParam;
    fTimestampFiles: ID2XFlag;
    fExcludeDirs: TD2XListParam;

    function GetGlobalName: string;

  protected
    fOutputTimestamp: string;

    function GetTimestampFiles: Boolean;

  public
    property GlobalName: string read GetGlobalName write SetGlobalName;
  end;

procedure SplitDirExtn(pStr: string; out pDir, pExtn: string);

function ConvertDir(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertDirExtn(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertExtn(pStr, pDflt: string; out pVal: string): Boolean;
function ConvertFile(pStr, pDflt: string; out pVal: string): Boolean;

implementation

uses
  D2X.IO.Actual,
  System.StrUtils,
  Winapi.Windows;

procedure SplitDirExtn(pStr: string; out pDir, pExtn: string);
var
  lPos: Integer;
begin
  lPos := Pos(',', pStr);
  pExtn := '';
  if lPos > 0 then
  begin
    pDir := ExcludeTrailingPathDelimiter(Copy(pStr, 1, lPos - 1));
    pExtn := Copy(pStr, lPos + 1, Length(pStr));
    if StartsText('.', pExtn) then
      pExtn := Copy(pExtn, 2, Length(pExtn));
  end
  else
    pDir := ExcludeTrailingPathDelimiter(pStr);
end;

function ConvertDir(pStr, pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  if pStr > '' then
    pVal := ExcludeTrailingPathDelimiter(pStr)
  else
    pVal := '';
end;

function ConvertDirExtn(pStr, pDflt: string; out pVal: string): Boolean;
var
  lExtn, lDDir, lDExtn: string;
begin
  Result := True;
  SplitDirExtn(pDflt, lDDir, lDExtn);
  SplitDirExtn(pStr, pVal, lExtn);

  if pVal = '!' then
    pVal := lDDir;
  if lExtn = '!' then
    pVal := pVal + ',' + lDExtn
  else
    if lExtn > '' then
      pVal := pVal + ',' + lExtn
    else
      if lDExtn > '' then
        pVal := pVal + ',' + lDExtn;
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

function TD2XFileOptions.GetDuration(pWatch: TStopwatch): Double;
begin
  Result := pWatch.Elapsed.TotalSeconds;
end;

function TD2XFileOptions.GetGlobalName: string;
begin
  Result := fGlobalName.Value;
end;

function TD2XFileOptions.GetInputStream: TStreamReader;
begin
  Result := TStreamReader.Create(THandleStream.Create(GetStdHandle(STD_INPUT_HANDLE)));
  Result.OwnStream;
end;

function TD2XFileOptions.GetNow: string;
begin
  Result := FormatDateTime('yyyy-mmm-dd HH:nn:ss.zzz', Now);
end;

function TD2XFileOptions.GetTimestampFiles: Boolean;
begin
  Result := fTimestampFiles.Flag;
end;

function TD2XFileOptions.BaseDir(pFileOrDir: string): ID2XDir;
begin
  if fInputBase.FlagValue then
    Result := TD2XDirPath.Create(fInputBase.Value, pFileOrDir, fExcludeDirs.List)
  else
    Result := TD2XDirPath.Create('', pFileOrDir, fExcludeDirs.List);
end;

function TD2XFileOptions.BaseFile(pFileOrDir: string): ID2XFile;
begin
  if fInputBase.FlagValue and (fInputBase.Value > '') then
    Result := TD2XFileStream.Create(IncludeTrailingPathDelimiter(fInputBase.Value) +
        pFileOrDir)
  else
    Result := TD2XFileStream.Create(pFileOrDir);
end;

function TD2XFileOptions.ConfigFileOrExtn(pFileOrExtn: string): ID2XFile;
  function GlobalFileOrExtn(pFileOrExtn: string): string;
  begin
    if StartsText('.', pFileOrExtn) then
      Result := fGlobalName.Value + pFileOrExtn
    else
      Result := pFileOrExtn;
  end;

begin
  if ID2XFlag(fConfigBase).Flag and (fConfigBase.Value > '') then
    Result := TD2XFileStream.Create(IncludeTrailingPathDelimiter(fConfigBase.Value) +
        GlobalFileOrExtn(pFileOrExtn))
  else
    Result := TD2XFileStream.Create(GlobalFileOrExtn(pFileOrExtn));
end;

function TD2XFileOptions.LogFileOrExtn(pFileOrExtn: string): ID2XFile;
  function GlobalFileOrExtn(pFileOrExtn: string): string;
  var
    lExtn: string;
  begin
    if StartsText('.', pFileOrExtn) then
      if fTimestampFiles.Flag then
        Result := fGlobalName.Value + fOutputTimestamp + pFileOrExtn
      else
        Result := fGlobalName.Value + pFileOrExtn
    else
      if fTimestampFiles.Flag then
      begin
        lExtn := ExtractFileExt(pFileOrExtn);
        Result := ChangeFileExt(pFileOrExtn, fOutputTimestamp + lExtn);
      end
      else
        Result := pFileOrExtn;
  end;

begin
  if ID2XFlag(fLogBase).Flag and (fLogBase.Value > '') then
    Result := TD2XFileStream.Create(IncludeTrailingPathDelimiter(fLogBase.Value) +
        GlobalFileOrExtn(pFileOrExtn))
  else
    Result := TD2XFileStream.Create(GlobalFileOrExtn(pFileOrExtn));
end;

procedure TD2XFileOptions.RegisterParams(pParams: TD2XParams);
begin
  pParams.Add(fGlobalName);
  pParams.Add(fConfigBase);
  pParams.Add(fLogBase);
  pParams.Add(fInputBase);
  pParams.Add(fExcludeDirs);
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
    'Use <dir> as a base for all Input files', '.\', False, ConvertDir, nil, nil);
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
  fExcludeDirs := TD2XListParam.CreateList('X', 'Exclude Dirs',
    'Exclude dirs matching these strings', '.xdre', ConfigFileOrExtn);

  fOutputTimestamp := FormatDateTime('-HH-mm', Now);
end;

procedure TD2XFileOptions.SetTimestampFlag(pFlag: ID2XFlag);
begin
  fTimestampFiles := pFlag;
end;

function TD2XFileOptions.SimpleFile(pFile: string): ID2XFile;
begin
  Result := TD2XFileStream.Create(pFile);
end;

end.
