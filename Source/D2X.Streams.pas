unit D2X.Streams;

interface

uses
  D2X.Stream,
  System.Classes;

type
  TD2XFileStream = class(TD2XStream)
  private
    fSW: TStreamWriter;
    fSR: TStreamReader;

    fFilename: string;
  public
    constructor Create(pFilename: string);
    destructor Destroy; override;

    function Description: String; override;
    function Exists: Boolean; override;
    function ReadFrom: TStreamReader; override;
    function WriteTo(pAppend: Boolean = False): TStreamWriter; override;
  end;

implementation

uses
  System.SysUtils;

{ TD2XFileStream }

constructor TD2XFileStream.Create(pFilename: string);
begin
  fFilename := pFilename;
end;

function TD2XFileStream.Description: String;
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

end.
