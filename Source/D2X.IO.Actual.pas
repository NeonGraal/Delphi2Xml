unit D2X.IO.Actual;

interface

uses
  D2X.Global,
  D2X.IO,
  System.Classes,
  System.SysUtils,
  System.RegularExpressions;

type
  TD2XFileStream = class(TD2XInterfaced, ID2XFile)
  private
    fSW: TStreamWriter;
    fSR: TStreamReader;

    fFilename: string;
  public
    constructor Create(pFilename: string);
    destructor Destroy; override;

    function Description: string;
    function Exists: Boolean;
    function ReadFrom: TStreamReader;
    function WriteTo(pAppend: Boolean = False): TStreamWriter;
  end;

  TD2XDirPath = class(TD2XInterfaced, ID2XDir)
  private
    fBase, fPath: string;
    fSR: TSearchRec;
    fDirSearch: Boolean;
    fExclude: TStrings;
    fRegexs: array of TRegEx;

    function ExcludeName(pName: string): Boolean;

    function SkipNonDirs: Boolean;
    function SkipNonFiles: Boolean;
  public
    constructor Create(pBase, pPath: string; pExclude: TStrings);
    destructor Destroy; override;

    function Description: string;
    function Exists: Boolean;

    function FirstFile(pWildcard: string): Boolean;
    function FirstDir: Boolean;
    function Next: Boolean;
    procedure Close;
    function Current: string;

  end;

implementation

uses
  System.StrUtils;

{ TD2XFileStream }

constructor TD2XFileStream.Create(pFilename: string);
begin
  fFilename := pFilename;
end;

function TD2XFileStream.Description: string;
begin
  Result := fFilename;
end;

destructor TD2XFileStream.Destroy;
begin
  FreeAndNil(fSR);
  FreeAndNil(fSW);

  inherited;
end;

function TD2XFileStream.Exists: Boolean;
begin
  Result := FileExists(fFilename)
end;

function TD2XFileStream.ReadFrom: TStreamReader;
begin
  fSR := TStreamReader.Create(fFilename, TEncoding.ANSI, True);
  Result := fSR;
end;

function TD2XFileStream.WriteTo(pAppend: Boolean): TStreamWriter;
begin
  if not pAppend then
    ForceDirectories(ExtractFilePath(ParamStr(0)) + ExtractFilePath(fFilename));

  fSW := TStreamWriter.Create(fFilename, pAppend);
  Result := fSW;
end;

{ TD2XDirPath }

procedure TD2XDirPath.Close;
begin
  FindClose(fSR);
end;

constructor TD2XDirPath.Create(pBase, pPath: string; pExclude: TStrings);
begin
  if pBase > '' then
    fBase := IncludeTrailingPathDelimiter(pBase);
  if pPath > '' then
    fPath := IncludeTrailingPathDelimiter(pPath);
  fExclude := pExclude;
end;

function TD2XDirPath.Current: string;
begin
  Result := fPath + fSR.Name;
end;

function TD2XDirPath.Description: string;
begin
  Result := fBase + fPath;
end;

destructor TD2XDirPath.Destroy;
begin

  inherited;
end;

function TD2XDirPath.ExcludeName(pName: string): Boolean;
var
  i: integer;
begin
  Result := False;
  if Assigned(fExclude) then
  begin
    if Length(fRegexs) <> fExclude.Count then
    begin
      SetLength(fRegexs, fExclude.Count);
      for i := 0 to fExclude.Count - 1 do
        fRegexs[i] := TRegEx.Create(fExclude[i], [roIgnoreCase, roExplicitCapture,
            roSingleLine, roIgnorePatternSpace]);
    end;
    for i := 0 to fExclude.Count - 1 do
    begin
      if fRegexs[i].IsMatch(pName) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TD2XDirPath.Exists: Boolean;
begin
  Result := DirectoryExists(fBase + fPath);
end;

{$WARN SYMBOL_PLATFORM OFF}

function TD2XDirPath.FirstDir: Boolean;
begin
  Result := FindFirst(fBase + fPath + '*', faAnyFile - faNormal - faTemporary, fSR) = 0;
  fDirSearch := True;
  if Result then
    Result := SkipNonDirs;
end;
{$WARN SYMBOL_PLATFORM ON}

function TD2XDirPath.FirstFile(pWildcard: string): Boolean;
begin

  Result := FindFirst(fBase + fPath + pWildcard, faAnyFile - faDirectory, fSR) = 0;
  fDirSearch := False;
  if Result then
    Result := SkipNonFiles;
end;

function TD2XDirPath.Next: Boolean;
begin
  Result := FindNext(fSR) = 0;
  if Result then
    if fDirSearch then
      Result := SkipNonDirs
    else
      Result := SkipNonFiles;
end;

function TD2XDirPath.SkipNonDirs: Boolean;
  function ExcludeDir(pDir: string): Boolean;
  begin
    Result := (pDir = '.') or (pDir = '..') or ExcludeName(pDir);
  end;

begin
  Result := True;
  while ((fSR.Attr and faDirectory) = 0) or ExcludeDir(fSR.Name) do
    if FindNext(fSR) <> 0 then
    begin
      Result := False;
      Exit;
    end;
end;

function TD2XDirPath.SkipNonFiles: Boolean;
begin
  Result := True;
  while ExcludeName(fSR.Name) do
    if FindNext(fSR) <> 0 then
    begin
      Result := False;
      Exit;
    end;
end;

end.
