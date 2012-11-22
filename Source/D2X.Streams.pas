unit D2X.Streams;

interface

uses
  D2X,
  D2X.Stream,
  System.Classes,
  System.SysUtils;

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
    fPath: string;
    fSR: TSearchRec;
    fDirSearch: Boolean;

    function SkipNonDirs: Boolean;
  public
    constructor Create(pPath: string);
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
  fSR := TStreamReader.Create(fFilename);
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

constructor TD2XDirPath.Create(pPath: string);
begin
  if pPath > '' then
    fPath := IncludeTrailingPathDelimiter(pPath);
end;

function TD2XDirPath.Current: string;
begin
  Result := fPath + fSR.Name;
end;

function TD2XDirPath.Description: string;
begin
  Result := fPath;
end;

destructor TD2XDirPath.Destroy;
begin

  inherited;
end;

function TD2XDirPath.Exists: Boolean;
begin
  Result := DirectoryExists(fPath);
end;

{$WARN SYMBOL_PLATFORM OFF}
function TD2XDirPath.FirstDir: Boolean;
begin
  Result := FindFirst(fPath + '*', faAnyFile - faNormal - faTemporary, fSR) = 0;
  fDirSearch := True;
  if Result then
    Result := SkipNonDirs;
end;
{$WARN SYMBOL_PLATFORM ON}

function TD2XDirPath.FirstFile(pWildcard: string): Boolean;
begin

  Result := FindFirst(fPath + pWildcard, faAnyFile - faDirectory, fSR) = 0;
  fDirSearch := False;
end;

function TD2XDirPath.Next: Boolean;
begin
  Result := FindNext(fSR) = 0;
  if Result and fDirSearch then
    Result := SkipNonDirs;
end;

function TD2XDirPath.SkipNonDirs: Boolean;
begin
  Result := True;
  while (fSR.Name = '.') or (fSR.Name = '..') or ((fSR.Attr and faDirectory) = 0) do
    if FindNext(fSR) <> 0 then
    begin
      Result := False;
      Exit;
    end;
end;

end.
