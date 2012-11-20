unit D2X.Stream;

interface

uses
  System.Classes,
  System.SysUtils;

type
  ED2XStreamException = class(Exception);

  TD2XStream = class
  public
    function Description: String; virtual; abstract;
    function Exists: Boolean; virtual; abstract;
    function ReadFrom: TStreamReader; virtual; abstract;
    function WriteTo(pAppend: Boolean = False): TStreamWriter; virtual; abstract;
    function Reader: TStreamReader;
    function Writer: TStreamWriter;
  end;

  TD2XStreamRef = reference to function: TD2XStream;
  TD2XNamedStreamRef = reference to function(pFile: string): TD2XStream;

implementation

{ TD2XStream }

function TD2XStream.Reader: TStreamReader;
begin
  Result := ReadFrom;
end;

function TD2XStream.Writer: TStreamWriter;
begin
  Result := WriteTo;
end;

end.
