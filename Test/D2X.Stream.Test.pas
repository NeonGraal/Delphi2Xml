unit D2X.Stream.Test;

interface

uses
  D2X.Stream,
  System.Classes;

type
  TTestStream = class(TD2XStream)
  private
    fSR: TStreamReader;
    fSW: TStreamWriter;
    fSS: TStringStream;

    fDesc: string;
    fExists: Boolean;
    fInput: string;

  public
    constructor Create(pDesc: string; pExists: Boolean; pInput: string);
    destructor Destroy; override;

    function Written: string;

    function Description: string; override;
    function Exists: Boolean; override;
    function ReadFrom: TStreamReader; override;
    function WriteTo(pAppend: Boolean = False): TStreamWriter; override;

  end;

implementation

uses
  System.SysUtils,
  TestFramework;

type
  TestTD2XStream = class(TTestCase)
  private
    fStream: TTestStream;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  end;

  { TestTD2XStream }

procedure TestTD2XStream.SetUp;
begin
  inherited;

  fStream := TTestStream.Create('Test', True, 'Test Input');
end;

procedure TestTD2XStream.TearDown;
begin
  FreeAndNil(fStream);

  inherited;
end;

{ TTestStream }

constructor TTestStream.Create(pDesc: string; pExists: Boolean; pInput: string);
begin
  fDesc := pDesc;
  fExists := pExists;
  fInput := pInput;

  fSR := nil;
  fSW := nil;

  fSS := TStringStream.Create;
end;

function TTestStream.Description: string;
begin
  Result := fDesc
end;

destructor TTestStream.Destroy;
begin
  FreeAndNil(fSR);
  FreeAndNil(fSW);
  FreeAndNil(fSS);

  inherited;
end;

function TTestStream.Exists: Boolean;
begin
  Result := fExists;
end;

function TTestStream.ReadFrom: TStreamReader;
begin
  if Assigned(fSR) then
    fSR.Free;

  fSR := TStreamReader.Create(TStringStream.Create(fInput));
  fSR.OwnStream;
  Result := fSR;
end;

function TTestStream.WriteTo(pAppend: Boolean): TStreamWriter;
begin
  if Assigned(fSW) then
    fSW.Free;
  fSW := TStreamWriter.Create(fSS);
  fSS.Clear;
  Result := fSW;
end;

function TTestStream.Written: string;
begin
  if Assigned(fSW) then
    fSW.Flush;
  Result := fSS.DataString;
  fSS.Clear;
end;

initialization

RegisterTests('Stream', [TestTD2XStream.Suite]);

end.
