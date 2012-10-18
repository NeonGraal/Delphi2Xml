unit D2XHandler;

interface

uses
  System.Generics.Collections;

type
  TD2XHandler = class
  public
    procedure Copy(pFrom: TD2XHandler); virtual;

    procedure BeginProcessing; virtual;
    procedure EndProcessing; virtual;

    procedure BeginResults; virtual;
    procedure EndResults(pFile: string); virtual;

    function CheckMethod(pMethod: string): Boolean; virtual;
    procedure DoBeginMethod(pMethod: string); virtual;
    procedure DoEndMethod(pMethod: string); virtual;

    procedure BeginMethod(pMethod: string); virtual;
    procedure EndMethod(pMethod: string); virtual;
  end;

  TD2XHandlers = class(TD2XHandler)
  private
    fHandlers: TObjectList<TD2XHandler>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginProcessing; override;
    procedure EndProcessing; override;

    procedure BeginResults; override;
    procedure EndResults(pFile: string); override;

    function CheckMethod(pMethod: string): Boolean; override;
    procedure DoBeginMethod(pMethod: string); override;
    procedure DoEndMethod(pMethod: string); override;

    function Add(pHandler: TD2XHandler): TD2XHandler;
  end;

implementation

uses
  System.SysUtils;

{ TD2XHandler }

procedure TD2XHandler.DoBeginMethod(pMethod: string);
begin

end;

procedure TD2XHandler.BeginMethod(pMethod: string);
begin
  if CheckMethod(pMethod) then
    DoBeginMethod(pMethod);
end;

procedure TD2XHandler.BeginProcessing;
begin

end;

procedure TD2XHandler.BeginResults;
begin

end;

function TD2XHandler.CheckMethod(pMethod: string): Boolean;
begin
  Result := True;
end;

procedure TD2XHandler.Copy(pFrom: TD2XHandler);
begin

end;

procedure TD2XHandler.DoEndMethod(pMethod: string);
begin

end;

procedure TD2XHandler.EndMethod(pMethod: string);
begin
  if CheckMethod(pMethod) then
    DoEndMethod(pMethod);
end;

procedure TD2XHandler.EndProcessing;
begin

end;

procedure TD2XHandler.EndResults(pFile: string);
begin

end;

{ TD2XHandlers }

function TD2XHandlers.Add(pHandler: TD2XHandler): TD2XHandler;
var
  lH: TD2XHandler;
begin
  Result := nil;
  for lH in fHandlers do
    if lH is pHandler.ClassType then
    begin
      Result := lH;
      lH.Copy(pHandler);
      pHandler.Free;
      Break;
    end;
  if not Assigned(Result) then
  begin
    Result := pHandler;
    fHandlers.Add(pHandler);
  end;
end;

procedure TD2XHandlers.DoBeginMethod(pMethod: string);
var
  lH: TD2XHandler;
begin
  for lH in fHandlers do
    lH.DoBeginMethod(pMethod);
end;

procedure TD2XHandlers.BeginProcessing;
var
  lH: TD2XHandler;
begin
  for lH in fHandlers do
    lH.BeginProcessing;
end;

procedure TD2XHandlers.BeginResults;
var
  lH: TD2XHandler;
begin
  for lH in fHandlers do
    lH.BeginResults;
end;

function TD2XHandlers.CheckMethod(pMethod: string): Boolean;
var
  lH: TD2XHandler;
begin
  Result := True;
  for lH in fHandlers do
    Result := lH.CheckMethod(pMethod) and Result;
end;

constructor TD2XHandlers.Create;
begin
  inherited;

  fHandlers:= TObjectList<TD2XHandler>.Create;
end;

destructor TD2XHandlers.Destroy;
begin
  FreeAndNil(fHandlers);

  inherited;
end;

procedure TD2XHandlers.DoEndMethod(pMethod: string);
var
  lH: TD2XHandler;
begin
  for lH in fHandlers do
    lH.DoEndMethod(pMethod);
end;

procedure TD2XHandlers.EndProcessing;
var
  lH: TD2XHandler;
begin
  for lH in fHandlers do
    lH.EndProcessing;
end;

procedure TD2XHandlers.EndResults(pFile: string);
var
  lH: TD2XHandler;
begin
  for lH in fHandlers do
    lH.EndResults(pFile);
end;

end.
