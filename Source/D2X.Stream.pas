unit D2X.Stream;

interface

uses
  D2X,
  System.Classes,
  System.SysUtils;

type
  ED2XStreamException = class(Exception);

  ID2XIO = interface
    ['{E99D6005-D73C-4DDE-86FA-409B421EA4B1}']
    function Description: String;
    function Exists: Boolean;
  end;

  ID2XDir = interface(ID2XIO)
    ['{DCF21FAD-CBAD-4224-B87C-6B33F898B116}']
    function FirstFile(pWildcard: String): Boolean;
    function FirstDir: Boolean;
    function Next: Boolean;
    procedure Close;
    function Current: String;
  end;

  ID2XFile = interface(ID2XIO)
    ['{2A2D2153-4F7D-413A-99A0-75A2CDA80B01}']
    function ReadFrom: TStreamReader;
    function WriteTo(pAppend: Boolean = False): TStreamWriter;
  end;

  TD2XFileRef = reference to function: ID2XFile;
  TD2XNamedStreamRef = reference to function(pFile: String): ID2XFile;

  TStreamReaderRef = reference to function: TStreamReader;
  TStreamWriterRef = reference to function: TStreamWriter;

procedure DisposeOf(var pFile: ID2XFile); overload;
procedure DisposeOf(var pDir: ID2XDir); overload;

function FileReaderRef(pFile: ID2XFile): TStreamReaderRef;
function FileWriterRef(pFile: ID2XFile): TStreamWriterRef;

implementation

procedure DisposeOf(var pFile: ID2XFile); overload;
var
  lDS: TD2XInterfaced;
begin
  if Assigned(pFile) then
  begin
    lDS := pFile as TD2XInterfaced;
    pFile :=  nil;
    lDS.Free;
  end;
end;

procedure DisposeOf(var pDir: ID2XDir); overload;
var
  lDS: TD2XInterfaced;
begin
  if Assigned(pDir) then
  begin
    lDS := pDir as TD2XInterfaced;
    pDir :=  nil;
    lDS.Free;
  end;
end;

function FileReaderRef(pFile: ID2XFile): TStreamReaderRef;
begin
  Result := function: TStreamReader
    begin
      Result := pFile.ReadFrom;
    end;
end;

function FileWriterRef(pFile: ID2XFile): TStreamWriterRef;
begin
  Result := function: TStreamWriter
    begin
      Result := pFile.WriteTo;
    end;
end;

end.
